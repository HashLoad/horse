unit Horse.Core.Context;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  {$IF DEFINED(FPC)}
    SysUtils,
    Generics.Collections,
  {$ELSE}
    System.SysUtils,
    System.Generics.Collections,
  {$ENDIF}
  Horse.Commons;

type
  THorseServiceFactoryWrapper = class
  private
    FFactory: THorseServiceFactory;
  public
    constructor Create(const AFactory: THorseServiceFactory);
    function CreateInstance: TObject;
  end;

  THorseRequestContext = class
  private
    FInstances: TObjectDictionary<string, TObject>;
    FUnownedInstances: TDictionary<string, TObject>;
    FFactories: TObjectDictionary<string, THorseServiceFactoryWrapper>;
    function GetKey(const AClass: TClass): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(const AClass: TClass; const AInstance: TObject; const AOwns: Boolean = True);
    procedure AddFactory(const AClass: TClass; const AFactory: THorseServiceFactory);

    function Resolve(const AClass: TClass): TObject;
  end;

implementation

{ THorseServiceFactoryWrapper }

constructor THorseServiceFactoryWrapper.Create(const AFactory: THorseServiceFactory);
begin
  inherited Create;
  FFactory := AFactory;
end;

function THorseServiceFactoryWrapper.CreateInstance: TObject;
begin
  Result := FFactory();
end;

{ THorseRequestContext }

constructor THorseRequestContext.Create;
begin
  inherited Create;
  FInstances := TObjectDictionary<string, TObject>.Create([doOwnsValues]);
  FUnownedInstances := TDictionary<string, TObject>.Create;
  FFactories := TObjectDictionary<string, THorseServiceFactoryWrapper>.Create([doOwnsValues]);
end;

destructor THorseRequestContext.Destroy;
begin
  FFactories.Free;
  FUnownedInstances.Free;
  FInstances.Free;
  inherited Destroy;
end;

function THorseRequestContext.GetKey(const AClass: TClass): string;
begin
  if Assigned(AClass) then
    Result := AClass.ClassName
  else
    Result := 'UnknownClass';
end;

procedure THorseRequestContext.Add(const AClass: TClass; const AInstance: TObject; const AOwns: Boolean);
var
  LKey: string;
begin
  LKey := GetKey(AClass);
  
  if FInstances.ContainsKey(LKey) then
    FInstances.Remove(LKey);
  if FUnownedInstances.ContainsKey(LKey) then
    FUnownedInstances.Remove(LKey);

  if AOwns then
    FInstances.Add(LKey, AInstance)
  else
    FUnownedInstances.Add(LKey, AInstance);
end;

procedure THorseRequestContext.AddFactory(const AClass: TClass; const AFactory: THorseServiceFactory);
var
  LKey: string;
  LWrapper: THorseServiceFactoryWrapper;
begin
  LKey := GetKey(AClass);
  if FFactories.TryGetValue(LKey, LWrapper) then
    FFactories.Remove(LKey);
  FFactories.Add(LKey, THorseServiceFactoryWrapper.Create(AFactory));
end;

function THorseRequestContext.Resolve(const AClass: TClass): TObject;
var
  LKey: string;
  LVal: TObject;
  LWrapper: THorseServiceFactoryWrapper;
begin
  LKey := GetKey(AClass);
  
  if FInstances.TryGetValue(LKey, LVal) then
    Exit(LVal);

  if FUnownedInstances.TryGetValue(LKey, LVal) then
    Exit(LVal);

  if FFactories.TryGetValue(LKey, LWrapper) then
  begin
    LVal := LWrapper.CreateInstance;
    FInstances.Add(LKey, LVal);
    FFactories.Remove(LKey);
    Exit(LVal);
  end;

  Result := nil;
end;

end.
