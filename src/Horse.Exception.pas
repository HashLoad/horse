unit Horse.Exception;

interface

uses System.SysUtils, Horse.Commons;

type
  EHorseCallbackInterrupted = class(Exception)
    constructor Create; reintroduce;
  end;

  EHorseException = class(Exception)
  private
    FError: string;
    FStatus: THTTPStatus;
  public
    constructor Create; overload;
    constructor Create(const AError: string); overload;
    constructor Create(const AStatus: THTTPStatus; const AError: string); overload;
    property Status: THTTPStatus read FStatus;
    property Error: string read FError;
  end;

implementation

{ EHorseException }

constructor EHorseException.Create;
begin
  FError := EmptyStr;
  FStatus := THTTPStatus.BadRequest;
end;

constructor EHorseException.Create(const AError: string);
begin
  FError := AError;
  FStatus := THTTPStatus.BadRequest;
end;

constructor EHorseException.Create(const AStatus: THTTPStatus; const AError: string);
begin
  FError := AError;
  FStatus := AStatus;
end;

{ EHorseCallbackInterrupted }

constructor EHorseCallbackInterrupted.Create;
begin
  inherited Create(EmptyStr);
end;

end.
