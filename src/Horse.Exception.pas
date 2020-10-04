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
  EHorseCallbackInterrupted = class(Exception)
    constructor Create; reintroduce;
  end;

  EHorseException = class(Exception)
  private
    FError: string;
    FStatus: THTTPStatus;
    FUnit: string;
    FCode: Integer;
    FTitle: string;
    FType: TMessageType;
  public
    constructor Create; overload;
    constructor Create(const AError: string); overload;
    constructor Create(const AError: string; const ACode: Integer); overload;
    constructor Create(const AError: string; const ACode: Integer; const AType: TMessageType); overload;
    constructor Create(const ATitle: string; const AError: string); overload;
    constructor Create(const ATitle: string; const AError: string; const AUnit: string); overload;
    constructor Create(const ATitle: string; const AError: string; const AUnit: string; const ACode: Integer); overload;
    constructor Create(const ATitle: string; const AError: string; const AUnit: string; const ACode: Integer; const AType: TMessageType); overload;
    constructor Create(const ATitle: string; const AError: string; const ACode: Integer); overload;
    constructor Create(const ATitle: string; const AError: string; const ACode: Integer; const AType: TMessageType); overload;
    constructor Create(const AStatus: THTTPStatus; const AError: string); overload;
    constructor Create(const AStatus: THTTPStatus; const ATitle: string; const AError: string); overload;
    constructor Create(const AStatus: THTTPStatus; const ATitle: string; const AError: string; const AUnit: string); overload;
    constructor Create(const AStatus: THTTPStatus; const ATitle: string; const AError: string; const AUnit: string; const ACode: Integer); overload;
    constructor Create(const AStatus: THTTPStatus; const ATitle: string; const AError: string; const AUnit: string; const ACode: Integer;
      const AType: TMessageType); overload;
    constructor Create(const AStatus: THTTPStatus; const AError: string; const AUnit: string; const ACode: Integer); overload;
    constructor Create(const AStatus: THTTPStatus; const AError: string; const AUnit: string; const ACode: Integer;
      const AType: TMessageType); overload;
    constructor Create(const AStatus: THTTPStatus; const AError: string; const ACode: Integer); overload;
    constructor Create(const AStatus: THTTPStatus; const AError: string; const ACode: Integer; const AType: TMessageType); overload;
    property Status: THTTPStatus read FStatus;
    property Error: string read FError;
    property &Unit: string read FUnit;
    property Code: Integer read FCode;
    property Title: string read FTitle;
    property &Type: TMessageType read FType;
  end;

implementation

{ EHorseException }

constructor EHorseException.Create;
begin
  FError := EmptyStr;
  FStatus := THTTPStatus.BadRequest;
  FCode := 0;
end;

constructor EHorseException.Create(const AError: string);
begin
  FError := AError;
  FStatus := THTTPStatus.BadRequest;
end;

constructor EHorseException.Create(const AStatus: THTTPStatus; const AError: string);
begin
  Create(AError);
  FStatus := AStatus;
end;

constructor EHorseException.Create(const AStatus: THTTPStatus; const ATitle, AError: string);
begin
  Create(AStatus, AError);
  FTitle := ATitle;
end;

constructor EHorseException.Create(const AStatus: THTTPStatus; const ATitle, AError, AUnit: string);
begin
  Create(AStatus, ATitle, AError);
  FUnit := AUnit;
end;

constructor EHorseException.Create(const AStatus: THTTPStatus; const ATitle, AError, AUnit: string; const ACode: Integer);
begin
  Create(AStatus, ATitle, AError, AUnit);
  FCode := ACode;
end;

constructor EHorseException.Create(const AStatus: THTTPStatus; const ATitle, AError, AUnit: string; const ACode: Integer; const AType: TMessageType);
begin
  Create(AStatus, ATitle, AError, AUnit, ACode);
  FType := AType;
end;

constructor EHorseException.Create(const ATitle, AError: string);
begin
  Create(AError);
  FTitle := ATitle;
end;

constructor EHorseException.Create(const ATitle, AError, AUnit: string);
begin
  Create(ATitle, AError);
  FUnit := AUnit;
end;

constructor EHorseException.Create(const ATitle, AError, AUnit: string; const ACode: Integer);
begin
  Create(ATitle, AError, AUnit);
  FCode := ACode;
end;

constructor EHorseException.Create(const ATitle, AError, AUnit: string; const ACode: Integer; const AType: TMessageType);
begin
  Create(ATitle, AError, AUnit, ACode);
  FType := AType;
end;

constructor EHorseException.Create(const AStatus: THTTPStatus; const AError, AUnit: string; const ACode: Integer; const AType: TMessageType);
begin
  Create(AStatus, AError, AUnit, ACode);
  FType := AType;
end;

constructor EHorseException.Create(const AStatus: THTTPStatus; const AError, AUnit: string; const ACode: Integer);
begin
  Create(AStatus, AError, AUnit);
  FCode := ACode;
end;

constructor EHorseException.Create(const AStatus: THTTPStatus; const AError: string; const ACode: Integer; const AType: TMessageType);
begin
  Create(AStatus, AError, ACode);
  FType := AType;
end;

constructor EHorseException.Create(const AError: string; const ACode: Integer; const AType: TMessageType);
begin
  Create(AError, ACode);
  FType := AType;
end;

constructor EHorseException.Create(const AError: string; const ACode: Integer);
begin
  Create(AError);
  FCode := ACode;
end;

constructor EHorseException.Create(const ATitle, AError: string; const ACode: Integer; const AType: TMessageType);
begin
  Create(ATitle, AError, ACode);
  FType := AType;
end;

constructor EHorseException.Create(const ATitle, AError: string; const ACode: Integer);
begin
  Create(ATitle, AError);
  FCode := ACode;
end;

constructor EHorseException.Create(const AStatus: THTTPStatus; const AError: string; const ACode: Integer);
begin
  Create(AStatus, AError);
  FCode := ACode;
end;

{ EHorseCallbackInterrupted }

constructor EHorseCallbackInterrupted.Create;
begin
  inherited Create(EmptyStr);
end;

end.
