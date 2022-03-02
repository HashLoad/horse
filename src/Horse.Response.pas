unit Horse.Response;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils, Classes, Generics.Collections, fpHTTP, HTTPDefs,
{$ELSE}
  System.SysUtils, System.Classes, Web.HTTPApp,
  {$IF CompilerVersion > 32.0}
    Web.ReqMulti,
  {$ENDIF}
{$ENDIF}
  Horse.Commons;

type
  THorseResponse = class
  private
    FWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF};
    FContent: TObject;
  public
    function Send(const AContent: string): THorseResponse; overload;
    function Send<T: class>(const AContent: T): THorseResponse; overload;
    function RedirectTo(const ALocation: string): THorseResponse; overload;
    function RedirectTo(const ALocation: string; const AStatus: THTTPStatus): THorseResponse; overload;
    function Status(const AStatus: Integer): THorseResponse; overload;
    function Status(const AStatus: THTTPStatus): THorseResponse; overload;
    function Status: Integer; overload;
    function Content: TObject; overload;
    function Content(const AContent: TObject): THorseResponse; overload;
    function ContentType(const AContentType: string): THorseResponse;
    function RawWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF};
    constructor Create(const AWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF});
    destructor Destroy; override;
  end;

implementation

function THorseResponse.Content(const AContent: TObject): THorseResponse;
begin
  Result := Self;
  FContent := AContent;
end;

function THorseResponse.Content: TObject;
begin
  Result := FContent;
end;

function THorseResponse.ContentType(const AContentType: string): THorseResponse;
begin
  FWebResponse.ContentType := AContentType;
  Result := Self;
end;

constructor THorseResponse.Create(const AWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF});
begin
  FWebResponse := AWebResponse;
  {$IF DEFINED(FPC)}FWebResponse.Code{$ELSE}FWebResponse.StatusCode{$ENDIF} := THTTPStatus.Ok.ToInteger;
end;

destructor THorseResponse.Destroy;
begin
  if Assigned(FContent) then
    FContent.Free;
  inherited;
end;

function THorseResponse.RawWebResponse: {$IF DEFINED(FPC)}TResponse{$ELSE}TWebResponse{$ENDIF};
begin
  Result := FWebResponse;
end;

function THorseResponse.Send(const AContent: string): THorseResponse;
begin
  FWebResponse.Content := AContent;
  Result := Self;
end;

function THorseResponse.Send<T>(const AContent: T): THorseResponse;
begin
  FContent := AContent;
  Result := Self;
end;

function THorseResponse.RedirectTo(const ALocation: string): THorseResponse;
begin
  FWebResponse.SetCustomHeader('Location', ALocation);
  Result := Status(THTTPStatus.SeeOther);
end;

function THorseResponse.RedirectTo(const ALocation: string; const AStatus: THTTPStatus): THorseResponse;
begin
  FWebResponse.SetCustomHeader('Location', ALocation);
  Result := Status(AStatus);
end;

function THorseResponse.Status(const AStatus: THTTPStatus): THorseResponse;
begin
  {$IF DEFINED(FPC)}FWebResponse.Code{$ELSE}FWebResponse.StatusCode{$ENDIF} := AStatus.ToInteger;
  Result := Self;
end;

function THorseResponse.Status: Integer;
begin
  Result := {$IF DEFINED(FPC)}FWebResponse.Code{$ELSE}FWebResponse.StatusCode{$ENDIF};
end;

function THorseResponse.Status(const AStatus: Integer): THorseResponse;
begin
  {$IF DEFINED(FPC)}FWebResponse.Code{$ELSE}FWebResponse.StatusCode{$ENDIF} := AStatus;
  Result := Self;
end;

end.
