unit Horse.Exception;

interface

uses System.SysUtils, Horse.Commons;

type
  EHorseCallbackInterrupted = class(Exception)
    constructor Create; reintroduce;
  end;

  EHorseException = class(Exception)
  private
    FError : string;
    FStatus: THTTPStatus;
    FUnit  : string;
    FCode  : Integer;
    FTitle : string;
    FType  : TMessageType;
  public
    constructor Create; overload;
    constructor Create(const AError: string); overload;
    constructor Create(const ATitle: string; const AError: string; const AUnit: string; const AType: TMessageType); overload;
    constructor Create(const ATitle: string; const AStatus: THTTPStatus; const AError: string; const AUnit: string;
      const AType: TMessageType); overload;
    constructor Create(const ATitle: string; const AError: string; const AUnit: string; const ACode: Integer; const AType: TMessageType); overload;
    constructor Create(const ATitle: string; const AStatus: THTTPStatus; const AError: string; const AUnit: string; const ACode: Integer;
      const AType: TMessageType); overload;
    constructor Create(const AError: string; const AUnit: string; const AType: TMessageType); overload;
    constructor Create(const AStatus: THTTPStatus; const AError: string; const AUnit: string; const AType: TMessageType); overload;
    constructor Create(const AError: string; const AUnit: string; const ACode: Integer; const AType: TMessageType); overload;
    constructor Create(const AStatus: THTTPStatus; const AError: string; const AUnit: string; const ACode: Integer;
      const AType: TMessageType); overload;
    constructor Create(const ATitle: string; const AError: string; const AUnit: string); overload;
    constructor Create(const ATitle: string; const AStatus: THTTPStatus; const AError: string; const AUnit: string); overload;
    constructor Create(const ATitle: string; const AError: string; const AUnit: string; const ACode: Integer); overload;
    constructor Create(const ATitle: string; const AStatus: THTTPStatus; const AError: string; const AUnit: string; const ACode: Integer); overload;
    constructor Create(const AError: string; const AUnit: string); overload;
    constructor Create(const AStatus: THTTPStatus; const AError: string; const AUnit: string); overload;
    constructor Create(const AError: string; const AUnit: string; const ACode: Integer); overload;
    constructor Create(const AStatus: THTTPStatus; const AError: string; const AUnit: string; const ACode: Integer); overload;
    constructor Create(const AStatus: THTTPStatus; const AError: string); overload;
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
  FError  := EmptyStr;
  FStatus := THTTPStatus.BadRequest;
  FCode   := 0;
end;

constructor EHorseException.Create(const AError: string);
begin
  FError  := AError;
  FStatus := THTTPStatus.BadRequest;
end;

constructor EHorseException.Create(const AStatus: THTTPStatus; const AError: string);
begin
  FError  := AError;
  FStatus := AStatus; 
end;

constructor EHorseException.Create(const ATitle: string; const AStatus: THTTPStatus; const AError, AUnit: string; const ACode: Integer;
  const AType: TMessageType);
begin
  FError  := AError;
  FUnit   := AUnit;
  FCode   := ACode;
  FStatus := AStatus;
  FTitle  := ATitle;
  FType   := AType;
end;

constructor EHorseException.Create(const ATitle, AError, AUnit: string; const ACode: Integer; const AType: TMessageType);
begin
  FError  := AError;
  FUnit   := AUnit;
  FCode   := ACode;
  FStatus := THTTPStatus.BadRequest;
  FTitle  := ATitle;
  FType   := AType;
end;

constructor EHorseException.Create(const ATitle: string; const AStatus: THTTPStatus; const AError, AUnit: string; const AType: TMessageType);
begin
  FError  := AError;
  FUnit   := AUnit;
  FStatus := AStatus;
  FTitle  := ATitle;
  FType   := AType;
end;

constructor EHorseException.Create(const ATitle, AError, AUnit: string; const AType: TMessageType);
begin
  FError := AError;
  FUnit  := AUnit;
  FTitle := ATitle;
  FType  := AType;
end;

constructor EHorseException.Create(const AStatus: THTTPStatus; const AError, AUnit: string; const ACode: Integer; const AType: TMessageType);
begin
  FError  := AError;
  FUnit   := AUnit;
  FCode   := ACode;
  FStatus := AStatus;
  FType   := AType;
end;

constructor EHorseException.Create(const AError, AUnit: string; const ACode: Integer; const AType: TMessageType);
begin
  FError := AError;
  FUnit  := AUnit;
  FCode  := ACode;
  FType  := AType;
end;

constructor EHorseException.Create(const AStatus: THTTPStatus; const AError, AUnit: string; const AType: TMessageType);
begin
  FError  := AError;
  FUnit   := AUnit;
  FStatus := AStatus;
  FType   := AType;
end;

constructor EHorseException.Create(const AError, AUnit: string; const AType: TMessageType);
begin
  FError := AError;
  FUnit  := AUnit;
  FType  := AType;
end;

constructor EHorseException.Create(const ATitle, AError, AUnit: string);
begin
  FError  := AError;
  FUnit   := AUnit;
  FStatus := THTTPStatus.BadRequest;
  FTitle  := ATitle;
end;

constructor EHorseException.Create(const ATitle: string; const AStatus: THTTPStatus; const AError, AUnit: string);
begin
  FError  := AError;
  FUnit   := AUnit;
  FStatus := AStatus;
  FTitle  := ATitle;
end;

constructor EHorseException.Create(const ATitle, AError, AUnit: string; const ACode: Integer);
begin
  FError  := AError;
  FUnit   := AUnit;
  FCode   := ACode;
  FStatus := THTTPStatus.BadRequest;
  FTitle  := ATitle;
end;

constructor EHorseException.Create(const ATitle: string; const AStatus: THTTPStatus; const AError, AUnit: string; const ACode: Integer);
begin
  FError  := AError;
  FUnit   := AUnit;
  FCode   := ACode;
  FStatus := AStatus;
  FTitle  := ATitle;
end;

constructor EHorseException.Create(const AStatus: THTTPStatus; const AError, AUnit: string);
begin
  FError  := AError;
  FUnit   := AUnit;
  FStatus := AStatus;
end;

constructor EHorseException.Create(const AError, AUnit: string);
begin
  FError := AError;
  FUnit  := AUnit;
end;

constructor EHorseException.Create(const AError, AUnit: string; const ACode: Integer);
begin
  FError  := AError;
  FUnit   := AUnit;
  FCode   := ACode;
  FStatus := THTTPStatus.BadRequest;
end;

constructor EHorseException.Create(const AStatus: THTTPStatus; const AError, AUnit: string; const ACode: Integer);
begin
  FError  := AError;
  FUnit   := AUnit;
  FCode   := ACode;
  FStatus := AStatus;
end;

{ EHorseCallbackInterrupted }

constructor EHorseCallbackInterrupted.Create;
begin
  inherited Create(EmptyStr);
end;

end.
