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
  PBytes = ^TBytes;
  PObject = ^TObject;
  TSerializeMethod = procedure(AStream: TStream) of object;
  TDeserializeMethod = procedure(AStream: TStream) of object;

  THorsePropType = (hptUnknown, hptInt32, hptInt64, hptDouble, hptSingle, hptString, hptBool, hptBytes, hptMessage);

  THorseProtobufProp = record
    Name: string;
    Tag: Integer;
    PropType: THorsePropType;
    RttiProperty: TRttiProperty;
    RttiType: TRttiType;
    {$IFNDEF FPC}
    FieldOffset: Integer;
    HasField: Boolean;
    {$ENDIF}
  end;

  THorseProtobufRtti = class
  private
    class var FContext: TRttiContext;
    class var FCache: TDictionary<TClass, TArray<THorseProtobufProp>>;
    class var FSerializeMethods: TDictionary<TClass, Pointer>;
    class var FDeserializeMethods: TDictionary<TClass, Pointer>;
    class function MapType(AType: TRttiType): THorsePropType; static;
  public
    class procedure Init;
    class procedure Uninit;
    class function GetProperties(AClass: TClass): TArray<THorseProtobufProp>; static;
    class function GetPropValue(AObject: TObject; const AProp: THorseProtobufProp): TValue; static;
    class procedure SetPropValue(AObject: TObject; const AProp: THorseProtobufProp; const AValue: TValue); static;
    class function CreateInstance(AClass: TClass): TObject; static;
    class function GetSerializeMethod(AClass: TClass): Pointer; static;
    class function GetDeserializeMethod(AClass: TClass): Pointer; static;
  end;

implementation

{ THorseProtobufRtti }

class procedure THorseProtobufRtti.Init;
begin
  FContext := TRttiContext.Create;
  FCache := TDictionary<TClass, TArray<THorseProtobufProp>>.Create;
  FSerializeMethods := TDictionary<TClass, Pointer>.Create;
  FDeserializeMethods := TDictionary<TClass, Pointer>.Create;
end;

class procedure THorseProtobufRtti.Uninit;
begin
  FCache.Free;
  FSerializeMethods.Free;
  FDeserializeMethods.Free;
  FContext.Free;
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
  {$IFNDEF FPC}
  LField: TRttiField;
  {$ENDIF}
begin
  System.TMonitor.Enter(FCache);
  try
    if FCache.TryGetValue(AClass, Result) then
      Exit;
  finally
    System.TMonitor.Exit(FCache);
  end;

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
            {$IFNDEF FPC}
            LField := RttiType.GetField('F' + Prop.Name);
            if not Assigned(LField) then
              LField := RttiType.GetField('F' + Prop.Name.ToLower);
            if not Assigned(LField) then
              LField := RttiType.GetField('F' + Prop.Name.ToUpper);
            if Assigned(LField) then
            begin
              HProp.FieldOffset := LField.Offset;
              HProp.HasField := True;
            end
            else
            begin
              HProp.FieldOffset := 0;
              HProp.HasField := False;
            end;
            {$ENDIF}
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

  System.TMonitor.Enter(FCache);
  try
    if not FCache.ContainsKey(AClass) then
      FCache.Add(AClass, Result)
    else
      Result := FCache[AClass];
  finally
    System.TMonitor.Exit(FCache);
  end;
end;

class function THorseProtobufRtti.GetPropValue(AObject: TObject; const AProp: THorseProtobufProp): TValue;
{$IFNDEF FPC}
var
  Ptr: Pointer;
{$ENDIF}
begin
  {$IFNDEF FPC}
  if AProp.HasField then
  begin
    Ptr := Pointer(PByte(AObject) + AProp.FieldOffset);
    case AProp.PropType of
      hptInt32: Exit(TValue.From<Integer>(PInteger(Ptr)^));
      hptInt64: Exit(TValue.From<Int64>(PInt64(Ptr)^));
      hptDouble: Exit(TValue.From<Double>(PDouble(Ptr)^));
      hptSingle: Exit(TValue.From<Single>(PSingle(Ptr)^));
      hptString: Exit(TValue.From<string>(PString(Ptr)^));
      hptBool: Exit(TValue.From<Boolean>(PBoolean(Ptr)^));
      hptBytes: Exit(TValue.From<TBytes>(PBytes(Ptr)^));
      hptMessage: Exit(TValue.From<TObject>(PObject(Ptr)^));
    end;
  end;
  {$ENDIF}
  Result := AProp.RttiProperty.GetValue(AObject);
end;

class procedure THorseProtobufRtti.SetPropValue(AObject: TObject; const AProp: THorseProtobufProp; const AValue: TValue);
{$IFNDEF FPC}
var
  Ptr: Pointer;
{$ENDIF}
begin
  {$IFNDEF FPC}
  if AProp.HasField then
  begin
    Ptr := Pointer(PByte(AObject) + AProp.FieldOffset);
    case AProp.PropType of
      hptInt32:
        begin
          PInteger(Ptr)^ := AValue.AsInteger;
          Exit;
        end;
      hptInt64:
        begin
          PInt64(Ptr)^ := AValue.AsInt64;
          Exit;
        end;
      hptDouble:
        begin
          PDouble(Ptr)^ := AValue.AsType<Double>;
          Exit;
        end;
      hptSingle:
        begin
          PSingle(Ptr)^ := AValue.AsType<Single>;
          Exit;
        end;
      hptString:
        begin
          PString(Ptr)^ := AValue.AsString;
          Exit;
        end;
      hptBool:
        begin
          PBoolean(Ptr)^ := AValue.AsBoolean;
          Exit;
        end;
      hptBytes:
        begin
          PBytes(Ptr)^ := AValue.AsType<TBytes>;
          Exit;
        end;
      hptMessage:
        begin
          PObject(Ptr)^ := AValue.AsObject;
          Exit;
        end;
    end;
  end;
  {$ENDIF}

  if not Assigned(AProp.RttiProperty) then
    raise Exception.CreateFmt('Property "%s" is not initialized in RTTI info.', [AProp.Name]);
  AProp.RttiProperty.SetValue(AObject, AValue);
end;

class function THorseProtobufRtti.CreateInstance(AClass: TClass): TObject;
var
  RttiType: TRttiType;
  InstanceType: TRttiInstanceType;
begin
  RttiType := FContext.GetType(AClass);
  if Assigned(RttiType) and (RttiType is TRttiInstanceType) then
  begin
    InstanceType := TRttiInstanceType(RttiType);
    Result := InstanceType.MetaclassType.Create;
  end
  else
    Result := AClass.Create;
end;

class function THorseProtobufRtti.GetSerializeMethod(AClass: TClass): Pointer;
var
  RttiType: TRttiType;
  Method: TRttiMethod;
begin
  System.TMonitor.Enter(FSerializeMethods);
  try
    if FSerializeMethods.TryGetValue(AClass, Result) then
      Exit;
  finally
    System.TMonitor.Exit(FSerializeMethods);
  end;

  Result := nil;
  RttiType := FContext.GetType(AClass);
  if Assigned(RttiType) then
  begin
    Method := RttiType.GetMethod('Serialize');
    if Assigned(Method) and (Length(Method.GetParameters) = 1) and (Method.GetParameters[0].ParamType.Name.ToLower = 'tstream') then
      Result := Method.CodeAddress;
  end;

  System.TMonitor.Enter(FSerializeMethods);
  try
    FSerializeMethods.AddOrSetValue(AClass, Result);
  finally
    System.TMonitor.Exit(FSerializeMethods);
  end;
end;

class function THorseProtobufRtti.GetDeserializeMethod(AClass: TClass): Pointer;
var
  RttiType: TRttiType;
  Method: TRttiMethod;
begin
  System.TMonitor.Enter(FDeserializeMethods);
  try
    if FDeserializeMethods.TryGetValue(AClass, Result) then
      Exit;
  finally
    System.TMonitor.Exit(FDeserializeMethods);
  end;

  Result := nil;
  RttiType := FContext.GetType(AClass);
  if Assigned(RttiType) then
  begin
    Method := RttiType.GetMethod('Deserialize');
    if Assigned(Method) and (Length(Method.GetParameters) = 1) and (Method.GetParameters[0].ParamType.Name.ToLower = 'tstream') then
      Result := Method.CodeAddress;
  end;

  System.TMonitor.Enter(FDeserializeMethods);
  try
    FDeserializeMethods.AddOrSetValue(AClass, Result);
  finally
    System.TMonitor.Exit(FDeserializeMethods);
  end;
end;

initialization
  THorseProtobufRtti.Init;

finalization
  THorseProtobufRtti.Uninit;

end.
