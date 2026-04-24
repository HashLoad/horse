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
    ControllerClass: THorseControllerClass;
    Path: string;
    MethodType: TMethodType;
    MethodName: string;
  end;

  THorseControllerRegistry = class
  private
    class var FRoutes: TArray<TControllerActionMap>;
  public
    class procedure RegisterRoute(AClass: THorseControllerClass; const APath: string; AMethodType: TMethodType; const AMethodName: string);
    class function GetRoutes: TArray<TControllerActionMap>;
    class procedure Dispatcher(ARequest: THorseRequest; AResponse: THorseResponse; ANext: TNextProc); {$IF DEFINED(FPC)} static; {$ENDIF}
  end;


implementation

uses
  Horse;

{ THorseControllerRegistry }

class procedure THorseControllerRegistry.RegisterRoute(AClass: THorseControllerClass; const APath: string; AMethodType: TMethodType; const AMethodName: string);
var
  LLength: Integer;
begin
  LLength := Length(FRoutes);
  SetLength(FRoutes, LLength + 1);
  FRoutes[LLength].ControllerClass := AClass;
  FRoutes[LLength].Path := APath;
  FRoutes[LLength].MethodType := AMethodType;
  FRoutes[LLength].MethodName := AMethodName;
end;

class function THorseControllerRegistry.GetRoutes: TArray<TControllerActionMap>;
begin
  Result := FRoutes;
end;

type
  TControllerAction = procedure of object;

class procedure THorseControllerRegistry.Dispatcher(ARequest: THorseRequest; AResponse: THorseResponse; ANext: TNextProc);
var
  LController: THorseController;
  LRoute: TControllerActionMap;
  LMatched: Boolean;
  LMethod: TMethod;
  LAction: TControllerAction;
  I: Integer;
begin
  LMatched := False;
  LMethod.Code := nil;
  LRoute.ControllerClass := nil;

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

  if LMatched and Assigned(LRoute.ControllerClass) then
  begin
    // For standard execution without specific constructor arguments, we instantiate using the class reference.
    // In Delphi/FPC we must use a virtual constructor on the base class.
    LController := LRoute.ControllerClass.Create(ARequest, AResponse, ANext);
    try
      {$IF DEFINED(FPC)}
      LMethod.Code := Pointer(LController.MethodAddress(LRoute.MethodName));
      {$ELSE}
      LMethod.Code := LController.MethodAddress(LRoute.MethodName);
      {$ENDIF}
      if LMethod.Code <> nil then
      begin
        LMethod.Data := Pointer(LController);
        LAction := TControllerAction(LMethod);
        LAction();
        Exit;
      end
      else
        raise Exception.CreateFmt('Method "%s" not found or not published in controller "%s"', [LRoute.MethodName, LController.ClassName]);
      
      LController.Execute;
    finally
      LController.Free;
    end;
  end;
end;

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

procedure THorseController.Get; begin end;
procedure THorseController.Post; begin end;
procedure THorseController.Put; begin end;
procedure THorseController.Delete; begin end;
procedure THorseController.Patch; begin end;
procedure THorseController.Head; begin end;


end.

end.
