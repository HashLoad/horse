unit Horse.Core.Router.Radix;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  {$IF DEFINED(FPC)}
    SysUtils,
    Generics.Collections,
    httpdefs,
    Horse.Core.ByteSpan,
  {$ELSE}
    Web.HTTPApp,
    System.Generics.Collections,
  {$ENDIF}
  Horse.Commons, Horse.Callback, Horse.Request, Horse.Response,
  Horse.Core.Router.Contract, Horse.Core.Regex;

type
  { Contexto de fluxo plano de execução de middlewares e rotas }
  TRadixFlow = class
  private
    FIndex: Integer;
    FCallbacks: TList<THorseCallback>;
    FRequest: THorseRequest;
    FResponse: THorseResponse;
    FActive: Boolean;
  public
    constructor Create(ACallbacks: TList<THorseCallback>; AReq: THorseRequest; ARes: THorseResponse);
    destructor Destroy; override;
    procedure Next;
  end;

  { Nó interno da Radix Tree }
  TRadixNode = class
  public
    Part: string;
    IsParam: Boolean;
    ParamName: string;
    IsOptional: Boolean;
    IsRegex: Boolean;
    RegexPattern: string;
    RegexMatcher: THorseRegex;
    FullPath: string;
    Children: TObjectList<TRadixNode>;
    Callbacks: TDictionary<TMethodType, TArray<THorseCallback>>;
    Middlewares: TList<THorseCallback>;
    constructor Create(const APart: string);
    destructor Destroy; override;
    procedure AddRouteCallback(const AHTTPType: TMethodType; const ACallback: THorseCallback; const AIsMiddleware: Boolean);
  end;

  {$IFDEF FPC}
  TStaticRouteItem = record
    Method: TMethodType;
    PathBytes: TBytes;
    Callbacks: TList<THorseCallback>;
  end;
  {$ENDIF}

  { Roteador Radix de alta performance e vetorizado em software (SWAR 64-bit) }
  THorseRadixRouter = class(TInterfacedObject, IHorseRouter)
  private
    FRoot: TRadixNode;
    FGlobalMiddlewares: TList<THorseCallback>;
    {$IFDEF FPC}
    FStaticRoutes: array of TStaticRouteItem;
    FStaticRoutesCount: Integer;
    FStaticRoutesBuilt: Boolean;
    procedure CollectStaticNodes(ANode: TRadixNode; const AParentPath: string);
    procedure BuildStaticRoutesTable;
    {$ENDIF}
    function FindNode(const ASegments: TArray<THorseBufferSlice>; AIndex: Integer; ANode: TRadixNode;
      const AHTTPType: TMethodType; var AMiddlewares: TList<THorseCallback>; var AParams: TDictionary<string, string>): TRadixNode;
    procedure InsertRoute(const APath: string; const AHTTPType: TMethodType; const ACallback: THorseCallback; const AIsMiddleware: Boolean);
  protected
    function _AddRef: Integer; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
    function _Release: Integer; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
  public
    constructor Create;
    destructor Destroy; override;
    {$IFDEF FPC}
    function MatchStaticRoute(const ABuffer: TBytes; const APathSpan: TByteSpan; const AMethod: TMethodType; out ACallbacks: TList<THorseCallback>): Boolean;
    {$ENDIF}
    procedure RegisterRoute(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback);
    procedure RegisterRouteMiddleware(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback);
    procedure RegisterMiddleware(const APath: string; const AMiddleware: THorseCallback); overload;
    procedure RegisterMiddleware(const AMiddleware: THorseCallback); overload;
    function Execute(const ARequest: THorseRequest; const AResponse: THorseResponse): Boolean;
  end;

var
  GActiveRadixRouter: THorseRadixRouter = nil;

implementation

uses
  {$IF DEFINED(FPC)}
  Classes, Diagnostics,
  {$ELSE}
  System.SysUtils, System.Classes, System.Diagnostics,
  {$ENDIF}
  Horse.Exception, Horse.Exception.Interrupted, Horse.Proc, Horse.Utils, Horse, Horse.Core;

{$IFDEF FPC}
function StringToBytes(const AStr: string): TBytes;
var
  I: Integer;
begin
  SetLength(Result, Length(AStr));
  for I := 1 to Length(AStr) do
    Result[I - 1] := Byte(AStr[I]);
end;
{$ENDIF}

procedure RadixMethodNotAllowedFinalizer(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send('Method Not Allowed').Status(THTTPStatus.MethodNotAllowed);
end;

procedure RadixNotFoundFinalizer(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send('Not Found').Status(THTTPStatus.NotFound);
end;



{$IFDEF FPC}
threadvar
  GCurrentExecutor: Pointer;
  GCurrentNext: TNextProc;

type
  TRadixExecutor = class
  private
    FRouter: THorseRadixRouter;
    FRequest: THorseRequest;
    FResponse: THorseResponse;
    FResult: Boolean;
    procedure DoExecuteRoute;
    procedure DoPreValidation(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
  public
    constructor Create(ARouter: THorseRadixRouter; AReq: THorseRequest; ARes: THorseResponse);
    function Run: Boolean;
  end;

procedure RadixExecutorDoExecuteRoute;
begin
  TRadixExecutor(GCurrentExecutor).DoExecuteRoute;
end;

procedure RadixExecutorDoPreParsing;
begin
  THorse.ExecutePreParsing(TRadixExecutor(GCurrentExecutor).FRequest, TRadixExecutor(GCurrentExecutor).FResponse, RadixExecutorDoExecuteRoute);
end;

procedure RadixExecutorDoNext;
begin
  GCurrentNext();
end;

constructor TRadixExecutor.Create(ARouter: THorseRadixRouter; AReq: THorseRequest; ARes: THorseResponse);
begin
  FRouter := ARouter;
  FRequest := AReq;
  FResponse := ARes;
  FResult := False;
end;

function TRadixExecutor.Run: Boolean;
var
  LStopwatch: TStopwatch;
begin
  LStopwatch := TStopwatch.StartNew;
  FResponse.Request := FRequest;
  GCurrentExecutor := Self;
  try
    try
      THorse.ExecuteOnRequest(FRequest, FResponse, RadixExecutorDoPreParsing);
      Result := FResult;
    except
      on E: Exception do
      begin
        Result := False;
        raise;
      end;
    end;
  finally
    LStopwatch.Stop;
    THorseCore.ExecuteOnTelemetry(FRequest, FResponse, LStopwatch.Elapsed.TotalMilliseconds);
    THorse.ExecuteOnResponse(FRequest, FResponse);
  end;
end;

procedure TRadixExecutor.DoExecuteRoute;
var
  LSegments: TArray<THorseBufferSlice>;
  LNode: TRadixNode;
  LMiddlewares: TList<THorseCallback>;
  LParams: TDictionary<string, string>;
  LCallbacksList: TList<THorseCallback>;
  LRouteCallbacks: TArray<THorseCallback>;
  LFlow: TRadixFlow;
  LStartSegmentIndex: Integer;
  LKeys: TArray<string>;
  I: Integer;
  LKey: TMethodType;
  LAllow: string;
  LMethodType: TMethodType;
  LRawWebRequest: TRequest;
begin
  LRawWebRequest := FRequest.RawWebRequest;
  if not Assigned(LRawWebRequest) then
    LMethodType := FRequest.MethodType
  else
    LMethodType := TMethodType.FromString(LRawWebRequest.Method);

  LSegments := FRequest.GetPathSegments;
  
  LStartSegmentIndex := 0;
  if (Length(LSegments) > 0) and LSegments[0].Compare('', True) then
    LStartSegmentIndex := 1;

  LMiddlewares := TList<THorseCallback>.Create;
  LParams := nil;
  try
    LNode := FRouter.FindNode(LSegments, LStartSegmentIndex, FRouter.FRoot, LMethodType, LMiddlewares, LParams);
    
    if LNode <> nil then
    begin
      FRequest.MatchedRoute := LNode.FullPath;
      if LParams <> nil then
      begin
        LKeys := LParams.Keys.ToArray;
        for I := 0 to Length(LKeys) - 1 do
          FRequest.Params.Dictionary.AddOrSetValue(LKeys[I], DecodeParam(LParams.Items[LKeys[I]]));
      end;

      LCallbacksList := TList<THorseCallback>.Create;
      try
        LCallbacksList.AddRange(FRouter.FGlobalMiddlewares);
        
        LCallbacksList.Add(THorseCallback(DoPreValidation));

        LCallbacksList.AddRange(LMiddlewares);
        
        if LNode.Callbacks.TryGetValue(LMethodType, LRouteCallbacks) or LNode.Callbacks.TryGetValue(mtAny, LRouteCallbacks) then
        begin
          LCallbacksList.AddRange(LRouteCallbacks);
        end
        else
        begin
          if LNode.Callbacks.Count > 0 then
          begin
            LAllow := '';
            for LKey in LNode.Callbacks.Keys do
            begin
              if LKey <> TMethodType.mtAny then
              begin
                if LAllow <> '' then
                  LAllow := LAllow + ', ';
                LAllow := LAllow + UpperCase(LKey.ToString);
              end;
            end;
            if LAllow <> '' then
              FResponse.AddHeader('Allow', LAllow);
            LCallbacksList.Add(@RadixMethodNotAllowedFinalizer);
          end
          else
            LCallbacksList.Add(@RadixNotFoundFinalizer);
        end;

        LFlow := TRadixFlow.Create(LCallbacksList, FRequest, FResponse);
        try
          LFlow.Next;
        finally
          LFlow.Free;
        end;
      finally
        LCallbacksList.Free;
      end;
      FResult := True;
    end
    else
    begin
      LCallbacksList := TList<THorseCallback>.Create;
      try
        LCallbacksList.AddRange(FRouter.FGlobalMiddlewares);
        LCallbacksList.Add(@RadixNotFoundFinalizer);

        LFlow := TRadixFlow.Create(LCallbacksList, FRequest, FResponse);
        try
          LFlow.Next;
        finally
          LFlow.Free;
        end;
      finally
        LCallbacksList.Free;
      end;
      FResult := True;
    end;
  finally
    LMiddlewares.Free;
    if LParams <> nil then
      LParams.Free;
  end;
end;

procedure TRadixExecutor.DoPreValidation(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  GCurrentNext := Next;
  THorse.ExecutePreValidation(Req, Res, RadixExecutorDoNext);
end;
{$ENDIF}


{ TRadixFlow }

constructor TRadixFlow.Create(ACallbacks: TList<THorseCallback>; AReq: THorseRequest; ARes: THorseResponse);
begin
  FIndex := 0;
  FCallbacks := TList<THorseCallback>.Create;
  FCallbacks.AddRange(ACallbacks);
  FRequest := AReq;
  FResponse := ARes;
  FActive := True;
end;

destructor TRadixFlow.Destroy;
begin
  FCallbacks.Free;
  inherited;
end;

procedure TRadixFlow.Next;
var
  LIndex: Integer;
begin
  if FResponse.Aborted then
    Exit;
  if (FIndex < FCallbacks.Count) and FActive then
  begin
    LIndex := FIndex;
    Inc(FIndex);
    try
      {$IF DEFINED(FPC)}
      THorseCallbackProc(FCallbacks[LIndex])(FRequest, FResponse, Next);
      {$ELSE}
      FCallbacks[LIndex](FRequest, FResponse, Next);
      {$ENDIF}
    except
      on E: Exception do
      begin
        FActive := False;
        if E is EHorseCallbackInterrupted then
          raise;
        if E is EHorseException then
        begin
          FResponse.Send(EHorseException(E).Error).Status(EHorseException(E).Status);
          Exit;
        end;
        if THorse.HasOnError then
        begin
          THorse.ExecuteOnError(FRequest, FResponse, E);
          Exit;
        end;
        if FResponse.Status < Integer(THTTPStatus.BadRequest) then
          FResponse.Send('Internal Application Error: ' + E.Message).Status(THTTPStatus.InternalServerError);
        Exit;
      end;
    end;
  end;
end;

{ TRadixNode }

constructor TRadixNode.Create(const APart: string);
var
  LPartClean: string;
  LOpenParenthesis: Integer;
  LCloseParenthesis: Integer;
begin
  Part := APart;
  Children := TObjectList<TRadixNode>.Create(True);
  Callbacks := TDictionary<TMethodType, TArray<THorseCallback>>.Create;
  Middlewares := TList<THorseCallback>.Create;

  IsOptional := False;
  IsRegex := False;
  RegexPattern := '';
  RegexMatcher := nil;

  IsParam := APart.StartsWith(':');
  if IsParam then
  begin
    LPartClean := APart.Substring(1);

    if LPartClean.EndsWith('?') then
    begin
      IsOptional := True;
      LPartClean := LPartClean.Substring(0, LPartClean.Length - 1);
    end;

    LOpenParenthesis := LPartClean.IndexOf('(');
    if LOpenParenthesis >= 0 then
    begin
      LCloseParenthesis := LPartClean.IndexOf(')');
      if LCloseParenthesis > LOpenParenthesis then
      begin
        IsRegex := True;
        ParamName := LPartClean.Substring(0, LOpenParenthesis);
        RegexPattern := LPartClean.Substring(LOpenParenthesis + 1, LCloseParenthesis - LOpenParenthesis - 1);
        RegexMatcher := THorseRegex.Create(RegexPattern);
      end;
    end;

    if not IsRegex then
      ParamName := LPartClean;
  end
  else
  begin
    ParamName := '';
  end;
end;

destructor TRadixNode.Destroy;
begin
  Children.Free;
  Callbacks.Free;
  Middlewares.Free;
  if Assigned(RegexMatcher) then
    RegexMatcher.Free;
  inherited;
end;

procedure TRadixNode.AddRouteCallback(const AHTTPType: TMethodType; const ACallback: THorseCallback; const AIsMiddleware: Boolean);
var
  LCurrent: TArray<THorseCallback>;
begin
  if AIsMiddleware then
  begin
    Middlewares.Add(ACallback);
  end
  else
  begin
    if Callbacks.TryGetValue(AHTTPType, LCurrent) then
    begin
      SetLength(LCurrent, Length(LCurrent) + 1);
      LCurrent[Length(LCurrent) - 1] := ACallback;
      Callbacks.AddOrSetValue(AHTTPType, LCurrent);
    end
    else
    begin
      SetLength(LCurrent, 1);
      LCurrent[0] := ACallback;
      Callbacks.Add(AHTTPType, LCurrent);
    end;
  end;
end;

{ THorseRadixRouter }

function THorseRadixRouter._AddRef: Integer; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

function THorseRadixRouter._Release: Integer; {$IFDEF WINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
begin
  Result := -1;
end;

constructor THorseRadixRouter.Create;
begin
  FRoot := TRadixNode.Create('');
  FGlobalMiddlewares := TList<THorseCallback>.Create;
  {$IFDEF FPC}
  FStaticRoutesCount := 0;
  FStaticRoutesBuilt := False;
  SetLength(FStaticRoutes, 0);
  GActiveRadixRouter := Self;
  {$ENDIF}
end;

destructor THorseRadixRouter.Destroy;
{$IFDEF FPC}
var
  I: Integer;
{$ENDIF}
begin
  {$IFDEF FPC}
  if GActiveRadixRouter = Self then
    GActiveRadixRouter := nil;
  {$ENDIF}
  FRoot.Free;
  FGlobalMiddlewares.Free;
  {$IFDEF FPC}
  for I := 0 to FStaticRoutesCount - 1 do
    FStaticRoutes[I].Callbacks.Free;
  SetLength(FStaticRoutes, 0);
  {$ENDIF}
  inherited;
end;

{$IFDEF FPC}
procedure THorseRadixRouter.CollectStaticNodes(ANode: TRadixNode; const AParentPath: string);
var
  LCurrentPath: string;
  I: Integer;
  LChild: TRadixNode;
  LMethod: TMethodType;
  LCallbacks: TList<THorseCallback>;
  LNodeCallbacks: TArray<THorseCallback>;
  LItem: TStaticRouteItem;
begin
  if ANode = nil then Exit;
  
  if (AParentPath = '') or (AParentPath = '/') then
    LCurrentPath := '/' + ANode.Part
  else
    LCurrentPath := AParentPath + '/' + ANode.Part;

  if (ANode.Callbacks.Count > 0) and (not ANode.IsParam) then
  begin
    for LMethod in ANode.Callbacks.Keys do
    begin
      LNodeCallbacks := ANode.Callbacks[LMethod];
      if Length(LNodeCallbacks) > 0 then
      begin
        LCallbacks := TList<THorseCallback>.Create;
        LCallbacks.AddRange(FGlobalMiddlewares);
        LCallbacks.AddRange(ANode.Middlewares);
        LCallbacks.AddRange(LNodeCallbacks);
        
        Inc(FStaticRoutesCount);
        SetLength(FStaticRoutes, FStaticRoutesCount);
        LItem.Method := LMethod;
        LItem.PathBytes := StringToBytes(LCurrentPath);
        LItem.Callbacks := LCallbacks;
        FStaticRoutes[FStaticRoutesCount - 1] := LItem;
      end;
    end;
  end;

  for I := 0 to ANode.Children.Count - 1 do
  begin
    LChild := ANode.Children[I];
    if not LChild.IsParam then
      CollectStaticNodes(LChild, LCurrentPath);
  end;
end;

procedure THorseRadixRouter.BuildStaticRoutesTable;
begin
  FStaticRoutesCount := 0;
  SetLength(FStaticRoutes, 0);
  CollectStaticNodes(FRoot, '');
  FStaticRoutesBuilt := True;
end;

function THorseRadixRouter.MatchStaticRoute(const ABuffer: TBytes; const APathSpan: TByteSpan; const AMethod: TMethodType; out ACallbacks: TList<THorseCallback>): Boolean;
var
  I, J: Integer;
  LItem: TStaticRouteItem;
  LPathLen: Integer;
  LMatch: Boolean;
begin
  ACallbacks := nil;
  if not FStaticRoutesBuilt then
    BuildStaticRoutesTable;

  LPathLen := APathSpan.Length;
  for I := 0 to FStaticRoutesCount - 1 do
  begin
    LItem := FStaticRoutes[I];
    if (LItem.Method = AMethod) and (Length(LItem.PathBytes) = LPathLen) then
    begin
      LMatch := True;
      for J := 0 to LPathLen - 1 do
      begin
        if ABuffer[APathSpan.Offset + J] <> LItem.PathBytes[J] then
        begin
          LMatch := False;
          Break;
        end;
      end;
      
      if LMatch then
      begin
        ACallbacks := LItem.Callbacks;
        Exit(True);
      end;
    end;
  end;
  Result := False;
end;
{$ENDIF}

procedure THorseRadixRouter.InsertRoute(const APath: string; const AHTTPType: TMethodType; const ACallback: THorseCallback; const AIsMiddleware: Boolean);
var
  LSegments: TArray<string>;
  LSeg: string;
  LCurrent: TRadixNode;
  LChild: TRadixNode;
  LFound: Boolean;
  I: Integer;
begin
  LSegments := APath.Trim(['/']).Split(['/']);
  LCurrent := FRoot;

  for I := 0 to Length(LSegments) - 1 do
  begin
    LSeg := LSegments[I];
    if (LSeg = '') and (I > 0) and (I = Length(LSegments) - 1) then
      Continue;

    LFound := False;
    for LChild in LCurrent.Children do
    begin
      if SameText(LChild.Part, LSeg) then
      begin
        LCurrent := LChild;
        LFound := True;
        Break;
      end;
    end;

    if not LFound then
    begin
      LChild := TRadixNode.Create(LSeg);
      LCurrent.Children.Add(LChild);
      LCurrent := LChild;
    end;
  end;

  LCurrent.AddRouteCallback(AHTTPType, ACallback, AIsMiddleware);
  LCurrent.FullPath := '/' + APath.Trim(['/']);
end;

procedure THorseRadixRouter.RegisterRoute(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback);
begin
  InsertRoute(APath, AHTTPType, ACallback, False);
  {$IFDEF FPC}
  FStaticRoutesBuilt := False;
  {$ENDIF}
end;

procedure THorseRadixRouter.RegisterRouteMiddleware(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback);
begin
  InsertRoute(APath, AHTTPType, ACallback, True);
  {$IFDEF FPC}
  FStaticRoutesBuilt := False;
  {$ENDIF}
end;

procedure THorseRadixRouter.RegisterMiddleware(const APath: string; const AMiddleware: THorseCallback);
begin
  InsertRoute(APath, mtAny, AMiddleware, True);
  {$IFDEF FPC}
  FStaticRoutesBuilt := False;
  {$ENDIF}
end;

procedure THorseRadixRouter.RegisterMiddleware(const AMiddleware: THorseCallback);
begin
  FGlobalMiddlewares.Add(AMiddleware);
  {$IFDEF FPC}
  FStaticRoutesBuilt := False;
  {$ENDIF}
end;

function THorseRadixRouter.FindNode(const ASegments: TArray<THorseBufferSlice>; AIndex: Integer; ANode: TRadixNode;
  const AHTTPType: TMethodType; var AMiddlewares: TList<THorseCallback>; var AParams: TDictionary<string, string>): TRadixNode;
var
  LCurrentSlice: THorseBufferSlice;
  LChild: TRadixNode;
  LBestMatch: TRadixNode;
  LTempNode: TRadixNode;
begin
  if ANode = nil then
    Exit(nil);

  if ANode.Middlewares.Count > 0 then
    AMiddlewares.AddRange(ANode.Middlewares);

  if AIndex >= Length(ASegments) then
  begin
    for LChild in ANode.Children do
    begin
      if LChild.IsParam and LChild.IsOptional then
      begin
        if AParams = nil then
          AParams := TDictionary<string, string>.Create;
        AParams.AddOrSetValue(LChild.ParamName, '');

        LTempNode := LChild;
        LBestMatch := FindNode(ASegments, AIndex, LTempNode, AHTTPType, AMiddlewares, AParams);
        if (LBestMatch <> nil) and (LBestMatch.Callbacks.ContainsKey(AHTTPType) or LBestMatch.Callbacks.ContainsKey(mtAny)) then
          Exit(LBestMatch)
        else
          AParams.Remove(LChild.ParamName);
      end;
    end;
    Exit(ANode);
  end;

  LCurrentSlice := ASegments[AIndex];

  // 1. Tenta correspondência exata via SWAR 64-bit
  for LChild in ANode.Children do
  begin
    if (not LChild.IsParam) and (LChild.Part <> '*') and LCurrentSlice.Compare(LChild.Part, True) then
    begin
      LTempNode := LChild;
      LBestMatch := FindNode(ASegments, AIndex + 1, LTempNode, AHTTPType, AMiddlewares, AParams);
      if LBestMatch <> nil then
        Exit(LBestMatch);
    end;
  end;

  // 2. Tenta correspondência de parâmetro (ex: :id)
  for LChild in ANode.Children do
  begin
    if LChild.IsParam then
    begin
      if LChild.IsRegex then
      begin
        if not LChild.RegexMatcher.Match(LCurrentSlice.ToString) then
          Continue;
      end;

      if AParams = nil then
        AParams := TDictionary<string, string>.Create;
      AParams.AddOrSetValue(LChild.ParamName, LCurrentSlice.ToString);
      LTempNode := LChild;
      LBestMatch := FindNode(ASegments, AIndex + 1, LTempNode, AHTTPType, AMiddlewares, AParams);
      if LBestMatch <> nil then
        Exit(LBestMatch)
      else
        AParams.Remove(LChild.ParamName);
    end;
  end;

  // 3. Tenta correspondência de wildcard (*)
  for LChild in ANode.Children do
  begin
    if LChild.Part = '*' then
    begin
      Exit(LChild);
    end;
  end;

  Result := nil;
end;

function THorseRadixRouter.Execute(const ARequest: THorseRequest; const AResponse: THorseResponse): Boolean;
{$IF DEFINED(FPC)}
var
  LExecutor: TRadixExecutor;
begin
  LExecutor := TRadixExecutor.Create(Self, ARequest, AResponse);
  try
    Result := LExecutor.Run;
  finally
    LExecutor.Free;
  end;
end;
{$ELSE}
var
  LResult: Boolean;
  LRoot: TRadixNode;
  LGlobalMiddlewares: TList<THorseCallback>;
  LStopwatch: TStopwatch;
begin
  LStopwatch := TStopwatch.StartNew;
  LResult := False;
  AResponse.Request := ARequest;
  LRoot := FRoot;
  LGlobalMiddlewares := FGlobalMiddlewares;
  try
    try
      THorse.ExecuteOnRequest(ARequest, AResponse,
        procedure
        begin
          THorse.ExecutePreParsing(ARequest, AResponse,
            procedure
            var
              LSegments: TArray<THorseBufferSlice>;
              LNode: TRadixNode;
              LMiddlewares: TList<THorseCallback>;
              LParams: TDictionary<string, string>;
              LCallbacksList: TList<THorseCallback>;
              LRouteCallbacks: TArray<THorseCallback>;
              LFlow: TRadixFlow;
              LStartSegmentIndex: Integer;
              LKeys: TArray<string>;
              I: Integer;
              LKey: TMethodType;
              LAllow: string;
              LMethodType: TMethodType;
              LRawWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF};
            begin
              LRawWebRequest := ARequest.RawWebRequest;
              if not Assigned(LRawWebRequest) then
                LMethodType := ARequest.MethodType
              else
                LMethodType := TMethodType.FromString(LRawWebRequest.Method);

              LSegments := ARequest.GetPathSegments;
              
              LStartSegmentIndex := 0;
              if (Length(LSegments) > 0) and LSegments[0].Compare('', True) then
                LStartSegmentIndex := 1;

              LMiddlewares := TList<THorseCallback>.Create;
              LParams := nil;
              try
                LNode := FindNode(LSegments, LStartSegmentIndex, LRoot, LMethodType, LMiddlewares, LParams);
                
                if LNode <> nil then
                begin
                  ARequest.MatchedRoute := LNode.FullPath;
                  if LParams <> nil then
                  begin
                    LKeys := LParams.Keys.ToArray;
                    for I := 0 to Length(LKeys) - 1 do
                      ARequest.Params.Dictionary.AddOrSetValue(LKeys[I], DecodeParam(LParams.Items[LKeys[I]]));
                  end;

                  LCallbacksList := TList<THorseCallback>.Create;
                  try
                    LCallbacksList.AddRange(LGlobalMiddlewares);
                    
                    // Injeção síncrona do gancho preValidation
                    LCallbacksList.Add(
                      procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
                      begin
                        THorse.ExecutePreValidation(Req, Res, Next);
                      end);

                    LCallbacksList.AddRange(LMiddlewares);
                    
                    if LNode.Callbacks.TryGetValue(LMethodType, LRouteCallbacks) or LNode.Callbacks.TryGetValue(mtAny, LRouteCallbacks) then
                    begin
                      LCallbacksList.AddRange(LRouteCallbacks);
                    end
                    else
                    begin
                      if LNode.Callbacks.Count > 0 then
                      begin
                        LAllow := '';
                        for LKey in LNode.Callbacks.Keys do
                        begin
                          if LKey <> TMethodType.mtAny then
                          begin
                            if LAllow <> '' then
                              LAllow := LAllow + ', ';
                            LAllow := LAllow + UpperCase(LKey.ToString);
                          end;
                        end;
                        if LAllow <> '' then
                          AResponse.AddHeader('Allow', LAllow);
                        {$IF DEFINED(FPC)}LCallbacksList.Add(@RadixMethodNotAllowedFinalizer){$ELSE}LCallbacksList.Add(RadixMethodNotAllowedFinalizer){$ENDIF};
                      end
                      else
                        {$IF DEFINED(FPC)}LCallbacksList.Add(@RadixNotFoundFinalizer){$ELSE}LCallbacksList.Add(RadixNotFoundFinalizer){$ENDIF};
                    end;

                    LFlow := TRadixFlow.Create(LCallbacksList, ARequest, AResponse);
                    try
                      LFlow.Next;
                    finally
                      LFlow.Free;
                    end;
                  finally
                    LCallbacksList.Free;
                  end;
                  LResult := True;
                end
                else
                begin
                  LCallbacksList := TList<THorseCallback>.Create;
                  try
                    LCallbacksList.AddRange(LGlobalMiddlewares);
                    {$IF DEFINED(FPC)}LCallbacksList.Add(@RadixNotFoundFinalizer){$ELSE}LCallbacksList.Add(RadixNotFoundFinalizer){$ENDIF};

                    LFlow := TRadixFlow.Create(LCallbacksList, ARequest, AResponse);
                    try
                      LFlow.Next;
                    finally
                      LFlow.Free;
                    end;
                  finally
                    LCallbacksList.Free;
                  end;
                  LResult := True;
                end;
              finally
                LMiddlewares.Free;
                if LParams <> nil then
                  LParams.Free;
              end;
            end);
        end);
      Result := LResult;
      AResponse.FlushCookiesToWebResponse;
    except
      on E: Exception do
      begin
        if THorse.HasOnError then
        begin
          if E is EHorseCallbackInterrupted then
          begin
            Result := True;
          end
          else
          begin
            THorse.ExecuteOnError(ARequest, AResponse, E);
            Result := True;
          end;
        end
        else
        begin
          {$IF DEFINED(FPC)}
          Writeln('CRITICAL RADIX ERROR: ', E.ClassName, ': ', E.Message); Flush(Output);
          {$ENDIF}
          raise;
        end;
      end;
    end;
  finally
    LStopwatch.Stop;
    THorseCore.ExecuteOnTelemetry(ARequest, AResponse, LStopwatch.Elapsed.TotalMilliseconds);
    THorse.ExecuteOnResponse(ARequest, AResponse);
  end;
end;
{$ENDIF}

end.
