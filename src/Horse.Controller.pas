unit Horse.Controller;

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
  Horse.Request,
  Horse.Response,
  Horse.Proc,
  Horse.Commons;

type
  THorseController = class
  private
    FRequest: THorseRequest;
    FResponse: THorseResponse;
    FNext: TNextProc;
  protected
    property Request: THorseRequest read FRequest;
    property Response: THorseResponse read FResponse;
    property Next: TNextProc read FNext;
  public
    constructor Create(ARequest: THorseRequest; AResponse: THorseResponse; ANext: TNextProc); virtual;
    procedure Execute; virtual;
    procedure Get; virtual;
    procedure Post; virtual;
    procedure Put; virtual;
    procedure Delete; virtual;
    procedure Patch; virtual;
    procedure Head; virtual;
    destructor Destroy; override;
  end;

  THorseControllerClass = class of THorseController;

  THorseController<T: THorseController, constructor> = class
  public
    class procedure Handle(ARequest: THorseRequest; AResponse: THorseResponse; ANext: TNextProc);
  end;

implementation

{ THorseController }

constructor THorseController.Create(ARequest: THorseRequest; AResponse: THorseResponse; ANext: TNextProc);
begin
  FRequest := ARequest;
  FResponse := AResponse;
  FNext := ANext;
end;

destructor THorseController.Destroy;
begin
  inherited;
end;

procedure THorseController.Execute;
begin
  case FRequest.MethodType of
    mtGet: Get;
    mtPost: Post;
    mtPut: Put;
    mtDelete: Delete;
    mtPatch: Patch;
    mtHead: Head;
  end;
end;

procedure THorseController.Get;
begin
end;

procedure THorseController.Post;
begin
end;

procedure THorseController.Put;
begin
end;

procedure THorseController.Delete;
begin
end;

procedure THorseController.Patch;
begin
end;

procedure THorseController.Head;
begin
end;

{ THorseController<T> }

class procedure THorseController<T>.Handle(ARequest: THorseRequest; AResponse: THorseResponse; ANext: TNextProc);
var
  LController: T;
begin
  LController := T.Create(ARequest, AResponse, ANext);
  try
    LController.Execute;
  finally
    LController.Free;
  end;
end;

end.
