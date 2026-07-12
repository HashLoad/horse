unit Horse.Core.Protobuf.Rtti;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  System.SysUtils, System.Classes, System.TypInfo,
  System.Generics.Collections,
  {$IFNDEF FPC}
  System.Rtti,
  {$ELSE}
  Rtti,
  {$ENDIF}
  Horse.Grpc.Attributes;

type
  THorsePropType = (hptUnknown, hptInt32, hptInt64, hptDouble, hptSingle, hptString, hptBool, hptBytes, hptMessage);

  THorseProtobufProp = record
    Name: string;
    Tag: Integer;
    PropType: THorsePropType;
    RttiProperty: TRttiProperty;
    RttiType: TRttiType;
  end;

  THorseProtobufRtti = class
  private
    class var FContext: TRttiContext;
    class function MapType(AType: TRttiType): THorsePropType; static;
  public
    class constructor Create;
    class function GetProperties(AClass: TClass): TArray<THorseProtobufProp>; static;
    class function GetPropValue(AObject: TObject; const AProp: THorseProtobufProp): TValue; static;
    class procedure SetPropValue(AObject: TObject; const AProp: THorseProtobufProp; const AValue: TValue); static;
  end;

implementation

{ THorseProtobufRtti }

class constructor THorseProtobufRtti.Create;
begin
  FContext := TRttiContext.Create;
end;

class function THorseProtobufRtti.MapType(AType: TRttiType): THorsePropType;
var
  TypeName: string;
begin
  Result := hptUnknown;
  if not Assigned(AType) then Exit;

  case AType.TypeKind of
    tkInteger: Result := hptInt32;
    tkInt64: Result := hptInt64;
    tkFloat:
      begin
        TypeName := AType.Name.ToLower;
        if (TypeName = 'single') or (TypeName = 'singlefloat') then
          Result := hptSingle
        else
          Result := hptDouble;
      end;
    tkEnumeration:
      begin
        if AType.Name.ToLower = 'boolean' then
          Result := hptBool
        else
          Result := hptInt32;
      end;
    tkChar, tkWChar, tkLString, tkWString, tkUString, tkString:
      Result := hptString;
    tkClass:
      Result := hptMessage;
    tkDynArray:
      begin
        if AType.Name.ToLower = 'tbytes' then
          Result := hptBytes;
      end;
  end;
end;

class function THorseProtobufRtti.GetProperties(AClass: TClass): TArray<THorseProtobufProp>;
var
  RttiType: TRttiType;
  Prop: TRttiProperty;
  Attr: TCustomAttribute;
  List: TList<THorseProtobufProp>;
  HProp: THorseProtobufProp;
begin
  List := TList<THorseProtobufProp>.Create;
  try
    RttiType := FContext.GetType(AClass);
    if Assigned(RttiType) then
    begin
      for Prop in RttiType.GetProperties do
      begin
        for Attr in Prop.GetAttributes do
        begin
          if Attr is ProtoMemberAttribute then
          begin
            HProp.Name := Prop.Name;
            HProp.Tag := ProtoMemberAttribute(Attr).Tag;
            HProp.PropType := MapType(Prop.PropertyType);
            HProp.RttiProperty := Prop;
            HProp.RttiType := Prop.PropertyType;
            List.Add(HProp);
            Break;
          end;
        end;
      end;
    end;
    Result := List.ToArray;
  finally
    List.Free;
  end;
end;

class function THorseProtobufRtti.GetPropValue(AObject: TObject; const AProp: THorseProtobufProp): TValue;
begin
  Result := AProp.RttiProperty.GetValue(AObject);
end;

class procedure THorseProtobufRtti.SetPropValue(AObject: TObject; const AProp: THorseProtobufProp; const AValue: TValue);
begin
  if not Assigned(AProp.RttiProperty) then
    raise Exception.CreateFmt('Property "%s" is not initialized in RTTI info.', [AProp.Name]);
  AProp.RttiProperty.SetValue(AObject, AValue);
end;

end.
