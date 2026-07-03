unit Horse.Core.RouterTree.NextCaller;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  Generics.Collections,
  fpHTTP,
  httpprotocol,
{$ELSE}
  Web.HTTPApp,
  System.Generics.Collections,
{$ENDIF}
  Horse.Request,
  Horse.Response,
  Horse.Callback,
  Horse.Commons;

type
  TNextCaller = class
  private
    FIndex: Integer;
    FIndexCallback: Integer;
    FSegments: TArray<THorseBufferSlice>;
    FIndexSegment: Integer;
    FHTTPType: TMethodType;
    FRequest: THorseRequest;
    FResponse: THorseResponse;
    {$IF DEFINED(FPC)}
    FMiddleware: TList<THorseCallback>;
    FCallBack: TObjectDictionary<TMethodType, TList<THorseCallback>>;
    {$ELSE}
    FMiddleware: TArray<THorseCallback>;
    FCallBack: TDictionary<TMethodType, TArray<THorseCallback>>;
    {$ENDIF}
    FCallNextPath: TCallNextPath;
    FIsGroup: Boolean;
    FTag: string;
    FIsParamsKey: Boolean;
    FPart: string;
    FFound: ^Boolean;
  public
    procedure Configure(
      {$IF DEFINED(FPC)}
      const ACallback: TObjectDictionary<TMethodType, TList<THorseCallback>>;
      {$ELSE}
      const ACallback: TDictionary<TMethodType, TArray<THorseCallback>>;
      {$ENDIF}
      const ASegments: TArray<THorseBufferSlice>;
      AIndexSegment: Integer;
      const AHTTPType: TMethodType;
      const ARequest: THorseRequest;
      const AResponse: THorseResponse;
      const AIsGroup: Boolean;
      {$IF DEFINED(FPC)}
      const AMiddleware: TList<THorseCallback>;
      {$ELSE}
      const AMiddleware: TArray<THorseCallback>;
      {$ENDIF}
      const ATag: string;
      const AIsParamsKey: Boolean;
      const ACallNextPath: TCallNextPath;
      const APart: string;
      var AFound: Boolean
    );
    procedure Init;
    procedure Next;
  end;

implementation

uses
{$IF DEFINED(FPC)}
  SysUtils,
{$ELSE}
  System.SysUtils,
{$ENDIF}
  Horse.Utils,
  Horse.Exception,
  Horse.Exception.Interrupted;



procedure TNextCaller.Configure(
  {$IF DEFINED(FPC)}
  const ACallback: TObjectDictionary<TMethodType, TList<THorseCallback>>;
  {$ELSE}
  const ACallback: TDictionary<TMethodType, TArray<THorseCallback>>;
  {$ENDIF}
  const ASegments: TArray<THorseBufferSlice>;
  AIndexSegment: Integer;
  const AHTTPType: TMethodType;
  const ARequest: THorseRequest;
  const AResponse: THorseResponse;
  const AIsGroup: Boolean;
  {$IF DEFINED(FPC)}
  const AMiddleware: TList<THorseCallback>;
  {$ELSE}
  const AMiddleware: TArray<THorseCallback>;
  {$ENDIF}
  const ATag: string;
  const AIsParamsKey: Boolean;
  const ACallNextPath: TCallNextPath;
  const APart: string;
  var AFound: Boolean
);
begin
  FCallBack := ACallback;
  FSegments := ASegments;
  FIndexSegment := AIndexSegment;
  FHTTPType := AHTTPType;
  FRequest := ARequest;
  FResponse := AResponse;
  FIsGroup := AIsGroup;
  FMiddleware := AMiddleware;
  FTag := ATag;
  FIsParamsKey := AIsParamsKey;
  FCallNextPath := ACallNextPath;
  FPart := APart;
  FFound := @AFound;
end;

procedure TNextCaller.Init;
var
  LCurrent: THorseBufferSlice;
  LCurrentStr: string;
begin
  LCurrentStr := '';
  if (not FIsGroup) and (FIndexSegment < Length(FSegments)) then
  begin
    LCurrent := FSegments[FIndexSegment];
    LCurrentStr := LCurrent.ToString;
    Inc(FIndexSegment);
  end;
  FIndex := -1;
  FIndexCallback := -1;
  if FIsParamsKey and (LCurrentStr <> '') then
  begin
      FRequest.Params.Dictionary.AddOrSetValue(FTag, DecodeParam(LCurrentStr));
  end;
end;

procedure TNextCaller.Next;
var
  {$IF DEFINED(FPC)}
  LCallback: TList<THorseCallback>;
  LMiddlewareCount, LCallbackCount: Integer;
  {$ELSE}
  LCallback: TArray<THorseCallback>;
  LMiddlewareCount, LCallbackCount: Integer;
  {$ENDIF}
begin
  {$IF DEFINED(FPC)}
  LMiddlewareCount := FMiddleware.Count;
  {$ELSE}
  LMiddlewareCount := Length(FMiddleware);
  {$ENDIF}

  Inc(FIndex);
  if (LMiddlewareCount > FIndex) then
  begin
    FFound^ := True;
    {$IF DEFINED(FPC)}
    THorseCallbackProc(Self.FMiddleware.Items[FIndex])(FRequest, FResponse, Next);
    LMiddlewareCount := FMiddleware.Count;
    {$ELSE}
    Self.FMiddleware[FIndex](FRequest, FResponse, Next);
    {$ENDIF}
  end
  else if (FIndexSegment = Length(FSegments)) and Assigned(FCallBack) then
  begin
    Inc(FIndexCallback);
    if FCallBack.TryGetValue(FHTTPType, LCallback) then
    begin
      {$IF DEFINED(FPC)}
      LCallbackCount := LCallback.Count;
      {$ELSE}
      LCallbackCount := Length(LCallback);
      {$ENDIF}
      if (LCallbackCount > FIndexCallback) then
      begin
        try
          FFound^ := True;
          {$IF DEFINED(FPC)}
          THorseCallbackProc(LCallback.Items[FIndexCallback])(FRequest, FResponse, Next);
          {$ELSE}
          LCallback[FIndexCallback](FRequest, FResponse, Next);
          {$ENDIF}
        except
          on E: Exception do
          begin
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
        Next;
      end;
    end
    else
    begin
      if FCallBack.Count > 0 then
      begin
        FFound^ := True;
        FResponse.Send('Method Not Allowed').Status(THTTPStatus.MethodNotAllowed);
      end
      else
        FResponse.Send('Not Found').Status(THTTPStatus.NotFound);
    end;
  end
  else
  begin
    FFound^ := FCallNextPath(FSegments, FIndexSegment, FHTTPType, FRequest, FResponse);
    if (not FFound^) and (FPart = '*') and Assigned(FCallBack) and (FCallBack.ContainsKey(FHTTPType) or FCallBack.ContainsKey(TMethodType.mtAny)) then
    begin
      FIndexSegment := Length(FSegments);
      FIndexCallback := -1;
      Next;
    end;
  end;
  if not FFound^ then
    FResponse.Send('Not Found').Status(THTTPStatus.NotFound);
end;

end.
