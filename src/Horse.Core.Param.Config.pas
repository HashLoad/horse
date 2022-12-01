unit Horse.Core.Param.Config;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

type
  THorseCoreParamConfig = class
  private
    class var FInstance: THorseCoreParamConfig;
    FRequiredMessage: string;
    FInvalidFormatMessage: string;
    FDateFormat: string;
    FTimeFormat: string;
    FReturnUTC: Boolean;
    FTrueValue: string;
    FCheckLhsBrackets: Boolean;
    constructor Create;
  public
    function RequiredMessage(const AValue: string): THorseCoreParamConfig; overload;
    function RequiredMessage: string; overload;
    function InvalidFormatMessage(const AValue: string): THorseCoreParamConfig; overload;
    function InvalidFormatMessage: string; overload;
    function DateFormat(const AValue: string): THorseCoreParamConfig; overload;
    function DateFormat: string; overload;
    function TimeFormat(const AValue: string): THorseCoreParamConfig; overload;
    function TimeFormat: string; overload;
    function ReturnUTC(const AValue: Boolean): THorseCoreParamConfig; overload;
    function ReturnUTC: Boolean; overload;
    function TrueValue(const AValue: string): THorseCoreParamConfig; overload;
    function TrueValue: string; overload;
    function CheckLhsBrackets(const AValue: Boolean): THorseCoreParamConfig; overload;
    function CheckLhsBrackets: Boolean; overload;
    class function GetInstance: THorseCoreParamConfig;
    class destructor UnInitialize;
  end;

implementation

uses
{$IF DEFINED(FPC)}
  SysUtils;
{$ELSE}
  System.SysUtils;
{$ENDIF}

constructor THorseCoreParamConfig.Create;
begin
  FReturnUTC := True;
  FDateFormat := 'yyyy-MM-dd';
  FTimeFormat := 'hh:mm:ss';
  FTrueValue := 'true';
  FRequiredMessage := 'The %s param is required.';
  FInvalidFormatMessage := 'The %0:s param ''%1:s'' is not valid a %2:s type.';
  FCheckLhsBrackets := False;
end;

function THorseCoreParamConfig.DateFormat(const AValue: string): THorseCoreParamConfig;
begin
  Result := Self;
  FDateFormat := AValue;
end;

function THorseCoreParamConfig.DateFormat: string;
begin
  Result := FDateFormat;
end;

class function THorseCoreParamConfig.GetInstance: THorseCoreParamConfig;
begin
  if not Assigned(FInstance) then
    FInstance := THorseCoreParamConfig.Create;
  Result := FInstance;
end;

function THorseCoreParamConfig.InvalidFormatMessage: string;
begin
  Result := FInvalidFormatMessage;
end;

function THorseCoreParamConfig.InvalidFormatMessage(const AValue: string): THorseCoreParamConfig;
begin
  Result := Self;
  FInvalidFormatMessage := AValue;
end;

function THorseCoreParamConfig.RequiredMessage(const AValue: string): THorseCoreParamConfig;
begin
  Result := Self;
  FRequiredMessage := AValue;
end;

function THorseCoreParamConfig.RequiredMessage: string;
begin
  Result := FRequiredMessage;
end;

function THorseCoreParamConfig.ReturnUTC(const AValue: Boolean): THorseCoreParamConfig;
begin
  Result := Self;
  FReturnUTC := AValue;
end;

function THorseCoreParamConfig.ReturnUTC: Boolean;
begin
  Result := FReturnUTC;
end;

function THorseCoreParamConfig.TimeFormat: string;
begin
  Result := FTimeFormat;
end;

function THorseCoreParamConfig.TimeFormat(const AValue: string): THorseCoreParamConfig;
begin
  Result := Self;
  FTimeFormat := AValue;
end;

function THorseCoreParamConfig.TrueValue(const AValue: string): THorseCoreParamConfig;
begin
  Result := Self;
  FTrueValue := AValue;
end;

function THorseCoreParamConfig.TrueValue: string;
begin
  Result := FTrueValue;
end;

function THorseCoreParamConfig.CheckLhsBrackets(const AValue: Boolean
  ): THorseCoreParamConfig;
begin
  Result := Self;
  FCheckLhsBrackets := AValue;
end;

function THorseCoreParamConfig.CheckLhsBrackets: Boolean;
begin
  Result := FCheckLhsBrackets;
end;

class destructor THorseCoreParamConfig.UnInitialize;
begin
  if Assigned(FInstance) then
    FreeAndNil(FInstance);
end;

end.
