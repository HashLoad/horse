unit Horse.Core.RouterTree.NextCaller;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils, Generics.Collections, fpHTTP, httpprotocol,
{$ELSE}
  System.NetEncoding, System.SysUtils, Web.HTTPApp, System.Generics.Collections,
{$ENDIF}
  Horse.Commons, Horse.Request, Horse.Response, Horse.Callback, Horse.Core;

type
  TNextCaller = class
  private
    FIndex: Integer;
    FIndexCallback: Integer;
    FPath: TQueue<string>;
    FHTTPType: TMethodType;
    FRequest: THorseRequest;
    FResponse: THorseResponse;
    FMiddleware: TMiddlewares;
    FCallBack: TObjectDictionary<TMethodType, TMiddlewares>;
    FCallNextPath: TCallNextPath;
    FIsGroup: Boolean;
    FTag: string;
    FIsRegex: Boolean;
    FFound: ^Boolean;
  public
    function Init: TNextCaller;
    function SetCallback(const ACallback: TObjectDictionary<TMethodType, TMiddlewares>): TNextCaller;
    function SetPath(const APath: TQueue<string>): TNextCaller;
    function SetHTTPType(const AHTTPType: TMethodType): TNextCaller;
    function SetRequest(const ARequest: THorseRequest): TNextCaller;
    function SetResponse(const AResponse: THorseResponse): TNextCaller;
    function SetIsGroup(const AIsGroup: Boolean): TNextCaller;
    function SetMiddleware(const AMiddleware: TMiddlewares): TNextCaller;
    function SetTag(const ATag: string): TNextCaller;
    function SetIsRegex(const AIsRegex: Boolean): TNextCaller;
    function SetOnCallNextPath(const ACallNextPath: TCallNextPath): TNextCaller;
    function SetFound(var AFound: Boolean): TNextCaller;
    procedure Next;
  end;

implementation

uses Horse.Exception, Horse.Exception.Interrupted;

function TNextCaller.Init: TNextCaller;
var
  LCurrent: string;
begin
  Result := Self;
  if not FIsGroup then
    LCurrent := FPath.Dequeue;
  FIndex := -1;
  FIndexCallback := -1;
  if FIsRegex then
    FRequest.Params.Dictionary.Add(FTag, {$IF DEFINED(FPC)}HTTPDecode(LCurrent){$ELSE}TNetEncoding.URL.Decode(LCurrent){$ENDIF});
end;

procedure TNextCaller.Next;
var
  LCallback: TMiddlewares;
begin
  inc(FIndex);
  if (FMiddleware.Count > FIndex) then
  begin
    FFound^ := True;

    FMiddleware.Values.ToArray[FIndex](FRequest, FResponse, Next, Self.FMiddleware.Keys.ToArray[FIndex]);
    if (FMiddleware.Count > FIndex) then
      Next;
  end
  else
  if (FPath.Count = 0) and Assigned(FCallBack) then
  begin
    inc(FIndexCallback);
    if FCallBack.TryGetValue(FHTTPType, LCallback) then
    begin
      if (LCallback.Count > FIndexCallback) then
      begin
        try
          FFound^ := True;
          LCallback.Values.ToArray[FIndexCallback](FRequest, FResponse, Next, LCallback.Keys.ToArray[FIndexCallback]);
        except
          on E: Exception do
          begin
            if (not(E is EHorseCallbackInterrupted)) and
               (not(E is EHorseException)) and
               (FResponse.Status < Integer(THTTPStatus.BadRequest))
            then
              FResponse.Send('Internal Application Error').Status(THTTPStatus.InternalServerError);
            raise;
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
        FResponse.Send('Not Found').Status(THTTPStatus.NotFound)
    end;
  end
  else
    FFound^ := FCallNextPath(FPath, FHTTPType, FRequest, FResponse);

  if not FFound^ then
    FResponse.Send('Not Found').Status(THTTPStatus.NotFound);
end;

function TNextCaller.SetCallback(const ACallback: TObjectDictionary<TMethodType, TMiddlewares>): TNextCaller;
begin
  FCallBack := ACallback;
  Result := Self;
end;

function TNextCaller.SetFound(var AFound: Boolean): TNextCaller;
begin
  FFound := @AFound;
  Result := Self;
end;

function TNextCaller.SetHTTPType(const AHTTPType: TMethodType): TNextCaller;
begin
  FHTTPType := AHTTPType;
  Result := Self;
end;

function TNextCaller.SetIsGroup(const AIsGroup: Boolean): TNextCaller;
begin
  FIsGroup := AIsGroup;
  Result := Self;
end;

function TNextCaller.SetIsRegex(const AIsRegex: Boolean): TNextCaller;
begin
  FIsRegex := AIsRegex;
  Result := Self;
end;

function TNextCaller.SetMiddleware(const AMiddleware: TMiddlewares): TNextCaller;
begin
  FMiddleware := AMiddleware;
  Result := Self;
end;

function TNextCaller.SetOnCallNextPath(const ACallNextPath: TCallNextPath): TNextCaller;
begin
  FCallNextPath := ACallNextPath;
  Result := Self;
end;

function TNextCaller.SetPath(const APath: TQueue<string>): TNextCaller;
begin
  FPath := APath;
  Result := Self;
end;

function TNextCaller.SetRequest(const ARequest: THorseRequest): TNextCaller;
begin
  FRequest := ARequest;
  Result := Self;
end;

function TNextCaller.SetResponse(const AResponse: THorseResponse): TNextCaller;
begin
  FResponse := AResponse;
  Result := Self;
end;

function TNextCaller.SetTag(const ATag: string): TNextCaller;
begin
  FTag := ATag;
  Result := Self;
end;

end.
