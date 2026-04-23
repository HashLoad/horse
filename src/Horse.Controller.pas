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
  {$M+}
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
  {$M-}

  THorseControllerClass = class of THorseController;

  TControllerActionMap = record
    Path: string;
    MethodType: TMethodType;
    MethodName: string;
  end;

  THorseController<T: THorseController, constructor> = class
  private
    class var FRoutes: TArray<TControllerActionMap>;
  public
    class procedure Map(const APath: string; AMethodType: TMethodType; const AMethodName: string);
    class procedure Handle(ARequest: THorseRequest; AResponse: THorseResponse; ANext: TNextProc);
  end;

implementation

uses
  Horse;

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

class procedure THorseController<T>.Map(const APath: string; AMethodType: TMethodType; const AMethodName: string);
var
  LLength: Integer;
begin
  LLength := Length(FRoutes);
  SetLength(FRoutes, LLength + 1);
  FRoutes[LLength].Path := APath;
  FRoutes[LLength].MethodType := AMethodType;
  FRoutes[LLength].MethodName := AMethodName;

  case AMethodType of
    mtGet: THorse.Get(APath, Handle);
    mtPost: THorse.Post(APath, Handle);
    mtPut: THorse.Put(APath, Handle);
    mtDelete: THorse.Delete(APath, Handle);
    mtPatch: THorse.Patch(APath, Handle);
    mtHead: THorse.Head(APath, Handle);
    mtAny: THorse.Any(APath, Handle);
  end;
end;

type
  TControllerAction = procedure of object;

class procedure THorseController<T>.Handle(ARequest: THorseRequest; AResponse: THorseResponse; ANext: TNextProc);
var
  LController: T;
  LRoute: TControllerActionMap;
  LMatched: Boolean;
  LMethod: TMethod;
  LAction: TControllerAction;
  I: Integer;
begin
  LMatched := False;
  LMethod.Code := nil;

  for I := Low(FRoutes) to High(FRoutes) do
  begin
    LRoute := FRoutes[I];
    if (LRoute.MethodType = ARequest.MethodType) or (LRoute.MethodType = mtAny) then
    begin
      if MatchRoute(ARequest.PathInfo, [LRoute.Path]) then
      begin
        LMatched := True;
        Break;
      end;
    end;
  end;

  LController := T.Create(ARequest, AResponse, ANext);
  try
    if LMatched then
    begin
      LMethod.Code := LController.MethodAddress(LRoute.MethodName);
      if LMethod.Code <> nil then
      begin
        LMethod.Data := LController;
        LAction := TControllerAction(LMethod);
        LAction();
        Exit;
      end
      else
        raise Exception.CreateFmt('Method "%s" not found or not published in controller "%s"', [LRoute.MethodName, LController.ClassName]);
    end;

    LController.Execute;
  finally
    LController.Free;
  end;
end;

end.
