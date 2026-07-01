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
    FSegments: TArray<string>;
    FIndexSegment: Integer;
    FHTTPType: TMethodType;
    FRequest: THorseRequest;
    FResponse: THorseResponse;
    FMiddleware: TList<THorseCallback>;
    FCallBack: TObjectDictionary<TMethodType, TList<THorseCallback>>;
    FCallNextPath: TCallNextPath;
    FIsGroup: Boolean;
    FTag: string;
    FIsParamsKey: Boolean;
    FFound: ^Boolean;
  public
    procedure Configure(
      const ACallback: TObjectDictionary<TMethodType, TList<THorseCallback>>;
      const ASegments: TArray<string>;
      AIndexSegment: Integer;
      const AHTTPType: TMethodType;
      const ARequest: THorseRequest;
      const AResponse: THorseResponse;
      const AIsGroup: Boolean;
      const AMiddleware: TList<THorseCallback>;
      const ATag: string;
      const AIsParamsKey: Boolean;
      const ACallNextPath: TCallNextPath;
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
  System.NetEncoding,
{$ENDIF}
  Horse.Exception,
  Horse.Exception.Interrupted;

procedure TNextCaller.Configure(
  const ACallback: TObjectDictionary<TMethodType, TList<THorseCallback>>;
  const ASegments: TArray<string>;
  AIndexSegment: Integer;
  const AHTTPType: TMethodType;
  const ARequest: THorseRequest;
  const AResponse: THorseResponse;
  const AIsGroup: Boolean;
  const AMiddleware: TList<THorseCallback>;
  const ATag: string;
  const AIsParamsKey: Boolean;
  const ACallNextPath: TCallNextPath;
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
  FFound := @AFound;
end;

procedure TNextCaller.Init;
var
  LCurrent: string;
begin
  LCurrent := '';
  if (not FIsGroup) and (FIndexSegment < Length(FSegments)) then
  begin
    LCurrent := FSegments[FIndexSegment];
    Inc(FIndexSegment);
  end;
  FIndex := -1;
  FIndexCallback := -1;
  if FIsParamsKey and (LCurrent <> '') then
  begin
    if Pos('%', LCurrent) > 0 then
      FRequest.Params.Dictionary.AddOrSetValue(FTag, {$IF DEFINED(FPC)}HTTPDecode(LCurrent){$ELSE}TNetEncoding.URL.Decode(LCurrent){$ENDIF})
    else
      FRequest.Params.Dictionary.AddOrSetValue(FTag, LCurrent);
  end;
end;

procedure TNextCaller.Next;
var
  LCallback: TList<THorseCallback>;
begin
  Inc(FIndex);
  if (FMiddleware.Count > FIndex) then
  begin
    FFound^ := True;
    Self.FMiddleware.Items[FIndex](FRequest, FResponse, Next);
    if (FMiddleware.Count > FIndex) then
      Next;
  end
  else if (FIndexSegment = Length(FSegments)) and Assigned(FCallBack) then
  begin
    Inc(FIndexCallback);
    if FCallBack.TryGetValue(FHTTPType, LCallback) then
    begin
      if (LCallback.Count > FIndexCallback) then
      begin
        try
          FFound^ := True;
          LCallback.Items[FIndexCallback](FRequest, FResponse, Next);
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
        FResponse.Send('Not Found').Status(THTTPStatus.NotFound);
    end;
  end
  else
    FFound^ := FCallNextPath(FSegments, FIndexSegment, FHTTPType, FRequest, FResponse);
  if not FFound^ then
    FResponse.Send('Not Found').Status(THTTPStatus.NotFound);
end;

end.
