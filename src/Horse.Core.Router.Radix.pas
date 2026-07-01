unit Horse.Core.Router.Radix;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  {$IF DEFINED(FPC)}
    Generics.Collections,
    httpdefs,
  {$ELSE}
    Web.HTTPApp,
    System.Generics.Collections,
  {$ENDIF}
  Horse.Commons, Horse.Callback, Horse.Request, Horse.Response,
  Horse.Core.Router.Contract;

type
  { Contexto de fluxo plano de execução de middlewares e rotas }
  TRadixFlow = class
  private
    FIndex: Integer;
    FCallbacks: TArray<THorseCallback>;
    FRequest: THorseRequest;
    FResponse: THorseResponse;
    FActive: Boolean;
  public
    constructor Create(const ACallbacks: TArray<THorseCallback>; AReq: THorseRequest; ARes: THorseResponse);
    procedure Next;
  end;

  { Nó interno da Radix Tree }
  TRadixNode = class
  public
    Part: string;
    IsParam: Boolean;
    ParamName: string;
    Children: TObjectList<TRadixNode>;
    Callbacks: TDictionary<TMethodType, TArray<THorseCallback>>;
    Middlewares: TList<THorseCallback>;
    constructor Create(const APart: string);
    destructor Destroy; override;
    procedure AddRouteCallback(const AHTTPType: TMethodType; const ACallback: THorseCallback; const AIsMiddleware: Boolean);
  end;

  { Roteador Radix de alta performance e vetorizado em software (SWAR 64-bit) }
  THorseRadixRouter = class(TInterfacedObject, IHorseRouter)
  private
    FRoot: TRadixNode;
    FGlobalMiddlewares: TList<THorseCallback>;
    function FindNode(const ASegments: TArray<THorseBufferSlice>; AIndex: Integer; ANode: TRadixNode;
      const AHTTPType: TMethodType; var AMiddlewares: TList<THorseCallback>; var AParams: TDictionary<string, string>): TRadixNode;
    procedure InsertRoute(const APath: string; const AHTTPType: TMethodType; const ACallback: THorseCallback; const AIsMiddleware: Boolean);
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterRoute(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback);
    procedure RegisterRouteMiddleware(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback);
    procedure RegisterMiddleware(const APath: string; const AMiddleware: THorseCallback); overload;
    procedure RegisterMiddleware(const AMiddleware: THorseCallback); overload;
    function Execute(const ARequest: THorseRequest; const AResponse: THorseResponse): Boolean;
  end;

implementation

uses
  {$IF DEFINED(FPC)}
  SysUtils, Classes,
  {$ELSE}
  System.SysUtils, System.Classes,
  {$ENDIF}
  Horse.Exception, Horse.Exception.Interrupted, Horse.Proc, Horse.Utils;

procedure RadixMethodNotAllowedFinalizer(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send('Method Not Allowed').Status(THTTPStatus.MethodNotAllowed);
end;

procedure RadixNotFoundFinalizer(Req: THorseRequest; Res: THorseResponse; Next: TNextProc);
begin
  Res.Send('Not Found').Status(THTTPStatus.NotFound);
end;



{ TRadixFlow }

constructor TRadixFlow.Create(const ACallbacks: TArray<THorseCallback>; AReq: THorseRequest; ARes: THorseResponse);
begin
  FIndex := 0;
  FCallbacks := ACallbacks;
  FRequest := AReq;
  FResponse := ARes;
  FActive := True;
end;

procedure TRadixFlow.Next;
var
  LIndex: Integer;
begin
  if (FIndex < Length(FCallbacks)) and FActive then
  begin
    LIndex := FIndex;
    Inc(FIndex);
    try
      FCallbacks[LIndex](FRequest, FResponse, Next);
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
        if FResponse.Status < Integer(THTTPStatus.BadRequest) then
          FResponse.Send('Internal Application Error').Status(THTTPStatus.InternalServerError);
        Exit;
      end;
    end;
  end;
end;

{ TRadixNode }

constructor TRadixNode.Create(const APart: string);
begin
  Part := APart;
  IsParam := APart.StartsWith(':');
  if IsParam then
    ParamName := APart.Substring(1)
  else
    ParamName := '';
  Children := TObjectList<TRadixNode>.Create(True);
  Callbacks := TDictionary<TMethodType, TArray<THorseCallback>>.Create;
  Middlewares := TList<THorseCallback>.Create;
end;

destructor TRadixNode.Destroy;
begin
  Children.Free;
  Callbacks.Free;
  Middlewares.Free;
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

function THorseRadixRouter._AddRef: Integer;
begin
  Result := -1;
end;

function THorseRadixRouter._Release: Integer;
begin
  Result := -1;
end;

constructor THorseRadixRouter.Create;
begin
  FRoot := TRadixNode.Create('');
  FGlobalMiddlewares := TList<THorseCallback>.Create;
end;

destructor THorseRadixRouter.Destroy;
begin
  FRoot.Free;
  FGlobalMiddlewares.Free;
  inherited;
end;

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
end;

procedure THorseRadixRouter.RegisterRoute(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback);
begin
  InsertRoute(APath, AHTTPType, ACallback, False);
end;

procedure THorseRadixRouter.RegisterRouteMiddleware(const AHTTPType: TMethodType; const APath: string; const ACallback: THorseCallback);
begin
  InsertRoute(APath, AHTTPType, ACallback, True);
end;

procedure THorseRadixRouter.RegisterMiddleware(const APath: string; const AMiddleware: THorseCallback);
begin
  InsertRoute(APath, mtAny, AMiddleware, True);
end;

procedure THorseRadixRouter.RegisterMiddleware(const AMiddleware: THorseCallback);
begin
  FGlobalMiddlewares.Add(AMiddleware);
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
    Exit(ANode);

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
      AParams.AddOrSetValue(LChild.ParamName, LCurrentSlice.ToString);
      LTempNode := LChild;
      LBestMatch := FindNode(ASegments, AIndex + 1, LTempNode, AHTTPType, AMiddlewares, AParams);
      if LBestMatch <> nil then
        Exit(LBestMatch);
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
var
  LSegments: TArray<THorseBufferSlice>;
  LMethodType: TMethodType;
  LRawWebRequest: {$IF DEFINED(FPC)}TRequest{$ELSE}TWebRequest{$ENDIF};
  LNode: TRadixNode;
  LMiddlewares: TList<THorseCallback>;
  LParams: TDictionary<string, string>;
  LCallbacksList: TList<THorseCallback>;
  LRouteCallbacks: TArray<THorseCallback>;
  LFlow: TRadixFlow;
  LPair: TPair<string, string>;
  LStartSegmentIndex: Integer;
begin
  try
    Result := False;
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
    LParams := TDictionary<string, string>.Create;
    try
      LNode := FindNode(LSegments, LStartSegmentIndex, FRoot, LMethodType, LMiddlewares, LParams);
      
      if LNode <> nil then
      begin
        for LPair in LParams do
          ARequest.Params.Dictionary.AddOrSetValue(LPair.Key, DecodeParam(LPair.Value));

        LCallbacksList := TList<THorseCallback>.Create;
        try
          LCallbacksList.AddRange(FGlobalMiddlewares);
          LCallbacksList.AddRange(LMiddlewares);
          
          if LNode.Callbacks.TryGetValue(LMethodType, LRouteCallbacks) or LNode.Callbacks.TryGetValue(mtAny, LRouteCallbacks) then
          begin
            LCallbacksList.AddRange(LRouteCallbacks);
          end
          else
          begin
            if LNode.Callbacks.Count > 0 then
              LCallbacksList.Add(RadixMethodNotAllowedFinalizer)
            else
              LCallbacksList.Add(RadixNotFoundFinalizer);
          end;

          LFlow := TRadixFlow.Create(LCallbacksList.ToArray, ARequest, AResponse);
          try
            LFlow.Next;
          finally
            LFlow.Free;
          end;
        finally
          LCallbacksList.Free;
        end;
        Result := True;
      end
      else
      begin
        LCallbacksList := TList<THorseCallback>.Create;
        try
          LCallbacksList.AddRange(FGlobalMiddlewares);
          LCallbacksList.Add(RadixNotFoundFinalizer);

          LFlow := TRadixFlow.Create(LCallbacksList.ToArray, ARequest, AResponse);
          try
            LFlow.Next;
          finally
            LFlow.Free;
          end;
        finally
          LCallbacksList.Free;
        end;
        Result := True;
      end;
    finally
      LMiddlewares.Free;
      LParams.Free;
    end;
    
    AResponse.FlushCookiesToWebResponse;
  except
    on E: Exception do
    begin
      {$IF DEFINED(FPC)}
      Writeln('CRITICAL RADIX ERROR: ', E.ClassName, ': ', E.Message); Flush(Output);
      {$ENDIF}
      raise;
    end;
  end;
end;

end.
