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
    FMiddleware: TArray<THorseCallback>;
    FCallBack: TDictionary<TMethodType, TArray<THorseCallback>>;
    FCallNextPath: TCallNextPath;
    FIsGroup: Boolean;
    FTag: string;
    FIsParamsKey: Boolean;
    FFound: ^Boolean;
  public
    procedure Configure(
      const ACallback: TDictionary<TMethodType, TArray<THorseCallback>>;
      const ASegments: TArray<THorseBufferSlice>;
      AIndexSegment: Integer;
      const AHTTPType: TMethodType;
      const ARequest: THorseRequest;
      const AResponse: THorseResponse;
      const AIsGroup: Boolean;
      const AMiddleware: TArray<THorseCallback>;
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
  const ACallback: TDictionary<TMethodType, TArray<THorseCallback>>;
  const ASegments: TArray<THorseBufferSlice>;
  AIndexSegment: Integer;
  const AHTTPType: TMethodType;
  const ARequest: THorseRequest;
  const AResponse: THorseResponse;
  const AIsGroup: Boolean;
  const AMiddleware: TArray<THorseCallback>;
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
    if Pos('%', LCurrentStr) > 0 then
      FRequest.Params.Dictionary.AddOrSetValue(FTag, {$IF DEFINED(FPC)}HTTPDecode(LCurrentStr){$ELSE}TNetEncoding.URL.Decode(LCurrentStr){$ENDIF})
    else
      FRequest.Params.Dictionary.AddOrSetValue(FTag, LCurrentStr);
  end;
end;

procedure TNextCaller.Next;
var
  LCallback: TArray<THorseCallback>;
begin
  Inc(FIndex);
  if (Length(FMiddleware) > FIndex) then
  begin
    FFound^ := True;
    Self.FMiddleware[FIndex](FRequest, FResponse, Next);
    if (Length(FMiddleware) > FIndex) then
      Next;
  end
  else if (FIndexSegment = Length(FSegments)) and Assigned(FCallBack) then
  begin
    Inc(FIndexCallback);
    if FCallBack.TryGetValue(FHTTPType, LCallback) then
    begin
      if (Length(LCallback) > FIndexCallback) then
      begin
        try
          FFound^ := True;
          LCallback[FIndexCallback](FRequest, FResponse, Next);
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
    FFound^ := FCallNextPath(FSegments, FIndexSegment, FHTTPType, FRequest, FResponse);
  if not FFound^ then
    FResponse.Send('Not Found').Status(THTTPStatus.NotFound);
end;

end.
