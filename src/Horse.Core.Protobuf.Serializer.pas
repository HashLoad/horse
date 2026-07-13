unit Horse.Core.Protobuf.Serializer;

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
  Horse.Grpc.Attributes,
  Horse.Core.Protobuf.Rtti;

type
  TProtobufWriter = class
  private
    FStream: TStream;
  public
    constructor Create(AStream: TStream);
    procedure WriteVarint(Value: UInt64);
    procedure WriteTag(Tag: Integer; WireType: Integer);
    procedure WriteDouble(Tag: Integer; Value: Double);
    procedure WriteSingle(Tag: Integer; Value: Single);
    procedure WriteInt32(Tag: Integer; Value: Integer);
    procedure WriteInt64(Tag: Integer; Value: Int64);
    procedure WriteBool(Tag: Integer; Value: Boolean);
    procedure WriteString(Tag: Integer; const Value: string);
    procedure WriteBytes(Tag: Integer; const Value: TBytes);
    procedure WriteMessage(Tag: Integer; const Value: TBytes);
  end;

  TProtobufReader = class
  private
    FStream: TStream;
    FTag: Integer;
    FWireType: Integer;
  public
    constructor Create(AStream: TStream);
    function ReadField: Boolean;
    function ReadVarint: UInt64;
    function ReadDouble: Double;
    function ReadSingle: Single;
    function ReadInt32: Integer;
    function ReadInt64: Int64;
    function ReadBool: Boolean;
    function ReadString: string;
    function ReadBytes: TBytes;
    procedure SkipField;
    property Tag: Integer read FTag;
    property WireType: Integer read FWireType;
  end;

  THorseProtobufSerializer = class
  private
    class function CreateInstance(AClass: TClass): TObject; static;
  public
    class function Serialize(Obj: TObject): TBytes; static;
    class procedure Deserialize(const Bytes: TBytes; Obj: TObject); static;
  end;

implementation

{ TProtobufWriter }

constructor TProtobufWriter.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

procedure TProtobufWriter.WriteVarint(Value: UInt64);
var
  b: Byte;
begin
  while Value >= $80 do
  begin
    b := Byte((Value and $7F) or $80);
    FStream.Write(b, 1);
    Value := Value shr 7;
  end;
  b := Byte(Value);
  FStream.Write(b, 1);
end;

procedure TProtobufWriter.WriteTag(Tag, WireType: Integer);
begin
  WriteVarint((UInt64(Tag) shl 3) or UInt64(WireType));
end;

procedure TProtobufWriter.WriteDouble(Tag: Integer; Value: Double);
begin
  WriteTag(Tag, 1);
  FStream.Write(Value, SizeOf(Double));
end;

procedure TProtobufWriter.WriteSingle(Tag: Integer; Value: Single);
begin
  WriteTag(Tag, 5);
  FStream.Write(Value, SizeOf(Single));
end;

procedure TProtobufWriter.WriteInt32(Tag: Integer; Value: Integer);
begin
  WriteTag(Tag, 0);
  WriteVarint(UInt64(Value));
end;

procedure TProtobufWriter.WriteInt64(Tag: Integer; Value: Int64);
begin
  WriteTag(Tag, 0);
  WriteVarint(UInt64(Value));
end;

procedure TProtobufWriter.WriteBool(Tag: Integer; Value: Boolean);
begin
  WriteTag(Tag, 0);
  if Value then
    WriteVarint(1)
  else
    WriteVarint(0);
end;

procedure TProtobufWriter.WriteString(Tag: Integer; const Value: string);
var
  Bytes: TBytes;
begin
  Bytes := TEncoding.UTF8.GetBytes(Value);
  WriteMessage(Tag, Bytes);
end;

procedure TProtobufWriter.WriteBytes(Tag: Integer; const Value: TBytes);
begin
  WriteMessage(Tag, Value);
end;

procedure TProtobufWriter.WriteMessage(Tag: Integer; const Value: TBytes);
begin
  WriteTag(Tag, 2);
  WriteVarint(Length(Value));
  if Length(Value) > 0 then
    FStream.Write(Value[0], Length(Value));
end;

{ TProtobufReader }

constructor TProtobufReader.Create(AStream: TStream);
begin
  inherited Create;
  FStream := AStream;
end;

function TProtobufReader.ReadField: Boolean;
var
  Header: UInt64;
begin
  Result := FStream.Position < FStream.Size;
  if not Result then
    Exit;

  Header := ReadVarint;
  FTag := Header shr 3;
  FWireType := Header and 7;
end;

function TProtobufReader.ReadVarint: UInt64;
var
  b: Byte;
  Shift: Integer;
begin
  Result := 0;
  Shift := 0;
  repeat
    if FStream.Read(b, 1) <> 1 then
      Exit;
    Result := Result or (UInt64(b and $7F) shl Shift);
    Inc(Shift, 7);
  until (b and $80) = 0;
end;

function TProtobufReader.ReadDouble: Double;
begin
  Result := 0;
  FStream.Read(Result, SizeOf(Double));
end;

function TProtobufReader.ReadSingle: Single;
begin
  Result := 0;
  FStream.Read(Result, SizeOf(Single));
end;

function TProtobufReader.ReadInt32: Integer;
begin
  Result := Integer(ReadVarint);
end;

function TProtobufReader.ReadInt64: Int64;
begin
  Result := Int64(ReadVarint);
end;

function TProtobufReader.ReadBool: Boolean;
begin
  Result := ReadVarint <> 0;
end;

function TProtobufReader.ReadString: string;
begin
  Result := TEncoding.UTF8.GetString(ReadBytes);
end;

function TProtobufReader.ReadBytes: TBytes;
var
  Len: UInt64;
begin
  Len := ReadVarint;
  SetLength(Result, Len);
  if Len > 0 then
    FStream.Read(Result[0], Len);
end;

procedure TProtobufReader.SkipField;
var
  Len: UInt64;
begin
  case FWireType of
    0: ReadVarint;
    1: FStream.Seek(8, TSeekOrigin.soCurrent);
    2:
      begin
        Len := ReadVarint;
        FStream.Seek(Len, TSeekOrigin.soCurrent);
      end;
    5: FStream.Seek(4, TSeekOrigin.soCurrent);
  else
    raise Exception.CreateFmt('Invalid wire type: %d', [FWireType]);
  end;
end;

{ THorseProtobufSerializer }

class function THorseProtobufSerializer.CreateInstance(AClass: TClass): TObject;
begin
  Result := THorseProtobufRtti.CreateInstance(AClass);
end;

class function THorseProtobufSerializer.Serialize(Obj: TObject): TBytes;
var
  Stream: TBytesStream;
  Writer: TProtobufWriter;
  Props: TArray<THorseProtobufProp>;
  Prop: THorseProtobufProp;
  Val: TValue;
  SubObj: TObject;
  SubBytes: TBytes;
  SerializeAddr: Pointer;
  StaticMethod: TSerializeMethod;
begin
  if not Assigned(Obj) then Exit(nil);

  Stream := TBytesStream.Create(nil);
  try
    SerializeAddr := THorseProtobufRtti.GetSerializeMethod(Obj.ClassType);
    if Assigned(SerializeAddr) then
    begin
      TMethod(StaticMethod).Code := SerializeAddr;
      TMethod(StaticMethod).Data := Obj;
      StaticMethod(Stream);
    end
    else
    begin
      Writer := TProtobufWriter.Create(Stream);
      try
        Props := THorseProtobufRtti.GetProperties(Obj.ClassType);
        for Prop in Props do
        begin
          Val := THorseProtobufRtti.GetPropValue(Obj, Prop);
          if Val.IsEmpty then Continue;

          case Prop.PropType of
            hptInt32: Writer.WriteInt32(Prop.Tag, Val.AsInteger);
            hptInt64: Writer.WriteInt64(Prop.Tag, Val.AsInt64);
            hptDouble: Writer.WriteDouble(Prop.Tag, Val.AsType<Double>);
            hptSingle: Writer.WriteSingle(Prop.Tag, Val.AsType<Single>);
            hptString: Writer.WriteString(Prop.Tag, Val.AsString);
            hptBool: Writer.WriteBool(Prop.Tag, Val.AsBoolean);
            hptBytes: Writer.WriteBytes(Prop.Tag, Val.AsType<TBytes>);
            hptMessage:
              begin
                SubObj := Val.AsObject;
                if Assigned(SubObj) then
                begin
                  SubBytes := Serialize(SubObj);
                  Writer.WriteMessage(Prop.Tag, SubBytes);
                end;
              end;
          end;
        end;
      finally
        Writer.Free;
      end;
    end;
    Result := Stream.Bytes;
    SetLength(Result, Stream.Size);
  finally
    Stream.Free;
  end;
end;

class procedure THorseProtobufSerializer.Deserialize(const Bytes: TBytes; Obj: TObject);
var
  Stream: TBytesStream;
  Reader: TProtobufReader;
  Props: TArray<THorseProtobufProp>;
  Prop: THorseProtobufProp;
  Found: Boolean;
  SubBytes: TBytes;
  SubObj: TObject;
  PropClass: TClass;
  LVal: TValue;
  DeserializeAddr: Pointer;
  StaticMethod: TDeserializeMethod;
begin
  if (Length(Bytes) = 0) or not Assigned(Obj) then Exit;

  Stream := TBytesStream.Create(Bytes);
  try
    DeserializeAddr := THorseProtobufRtti.GetDeserializeMethod(Obj.ClassType);
    if Assigned(DeserializeAddr) then
    begin
      TMethod(StaticMethod).Code := DeserializeAddr;
      TMethod(StaticMethod).Data := Obj;
      StaticMethod(Stream);
    end
    else
    begin
      Props := THorseProtobufRtti.GetProperties(Obj.ClassType);
      Reader := TProtobufReader.Create(Stream);
      try
        while Reader.ReadField do
        begin
          Found := False;
          for Prop in Props do
          begin
            if Prop.Tag = Reader.Tag then
            begin
              Found := True;
              case Prop.PropType of
                hptInt32:
                  begin
                    LVal := TValue.From<Integer>(Reader.ReadInt32);
                    THorseProtobufRtti.SetPropValue(Obj, Prop, LVal);
                  end;
                hptInt64:
                  begin
                    LVal := TValue.From<Int64>(Reader.ReadInt64);
                    THorseProtobufRtti.SetPropValue(Obj, Prop, LVal);
                  end;
                hptDouble:
                  begin
                    LVal := TValue.From<Double>(Reader.ReadDouble);
                    THorseProtobufRtti.SetPropValue(Obj, Prop, LVal);
                  end;
                hptSingle:
                  begin
                    LVal := TValue.From<Single>(Reader.ReadSingle);
                    THorseProtobufRtti.SetPropValue(Obj, Prop, LVal);
                  end;
                hptString:
                  begin
                    LVal := TValue.From<string>(Reader.ReadString);
                    THorseProtobufRtti.SetPropValue(Obj, Prop, LVal);
                  end;
                hptBool:
                  begin
                    LVal := TValue.From<Boolean>(Reader.ReadBool);
                    THorseProtobufRtti.SetPropValue(Obj, Prop, LVal);
                  end;
                hptBytes:
                  begin
                    LVal := TValue.From<TBytes>(Reader.ReadBytes);
                    THorseProtobufRtti.SetPropValue(Obj, Prop, LVal);
                  end;
                hptMessage:
                  begin
                    SubBytes := Reader.ReadBytes;
                    if Length(SubBytes) > 0 then
                    begin
                      SubObj := THorseProtobufRtti.GetPropValue(Obj, Prop).AsObject;
                      if not Assigned(SubObj) then
                      begin
                        PropClass := Prop.RttiType.AsInstance.MetaclassType;
                        SubObj := CreateInstance(PropClass);
                        THorseProtobufRtti.SetPropValue(Obj, Prop, SubObj);
                      end;
                      Deserialize(SubBytes, SubObj);
                    end;
                  end;
              end;
              Break;
            end;
          end;
          if not Found then
            Reader.SkipField;
        end;
      finally
        Reader.Free;
      end;
    end;
  finally
    Stream.Free;
  end;
end;

end.
