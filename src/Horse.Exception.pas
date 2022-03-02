unit Horse.Exception;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  SysUtils,
{$ELSE}
  System.SysUtils,
{$ENDIF}
  Horse.Commons;

type
  EHorseException = class(Exception)
  strict private
    FError: string;
    FUnit: string;
    FTitle: string;
    FCode: Integer;
    FStatus: THTTPStatus;
    FType: TMessageType;
    constructor Create; reintroduce;
  public
    class function New: EHorseException;
    function Error(const AValue: string): EHorseException; overload;
    function Error: string; overload;
    function Title(const AValue: string): EHorseException; overload;
    function Title: string; overload;
    function &Unit(const AValue: string): EHorseException; overload;
    function &Unit: string; overload;
    function Code(const AValue: Integer): EHorseException; overload;
    function Code: Integer; overload;
    function Status(const AValue: THTTPStatus): EHorseException; overload;
    function Status: THTTPStatus; overload;
    function &Type(const AValue: TMessageType): EHorseException; overload;
    function &Type: TMessageType; overload;
  end;

implementation

function EHorseException.Code(const AValue: Integer): EHorseException;
begin
  FCode := AValue;
  Result := Self;
end;

function EHorseException.&Type: TMessageType;
begin
  Result := FType;
end;

function EHorseException.&Type(const AValue: TMessageType): EHorseException;
begin
  FType := AValue;
  Result := Self;
end;

function EHorseException.Code: Integer;
begin
  Result := FCode;
end;

function EHorseException.&Unit: string;
begin
  Result := FUnit;
end;

function EHorseException.&Unit(const AValue: string): EHorseException;
begin
  FUnit := AValue;
  Result := Self;
end;

constructor EHorseException.Create;
begin
  FError := EmptyStr;
  FStatus := THTTPStatus.InternalServerError;
  FCode := 0;
end;

function EHorseException.Error: string;
begin
  Result := FError;
end;

function EHorseException.Error(const AValue: string): EHorseException;
begin
  FError := AValue;
  Result := Self;
end;

class function EHorseException.New: EHorseException;
begin
  Result := EHorseException.Create;
end;

function EHorseException.Status: THTTPStatus;
begin
  Result := FStatus;
end;

function EHorseException.Status(const AValue: THTTPStatus): EHorseException;
begin
  FStatus := AValue;
  Result := Self;
end;

function EHorseException.Title(const AValue: string): EHorseException;
begin
  FTitle := AValue;
  Result := Self;
end;

function EHorseException.Title: string;
begin
  Result := FTitle;
end;

end.
