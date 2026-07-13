unit Horse.Grpc.Codec;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  System.SysUtils;

type
  THorseGrpcMessageCodec = record
  public
    class function TryDecode(const Buffer: TBytes; var Offset: Integer;
      var Compressed: Boolean; out MsgBytes: TBytes): Boolean; static;
    class function Encode(const MsgBytes: TBytes;
      Compress: Boolean = False): TBytes; static;
  end;

implementation

{ THorseGrpcMessageCodec }

class function THorseGrpcMessageCodec.TryDecode(const Buffer: TBytes;
  var Offset: Integer; var Compressed: Boolean;
  out MsgBytes: TBytes): Boolean;
var
  Available: Integer;
  MsgLen: Cardinal;
begin
  Result := False;
  MsgBytes := nil;
  Available := Length(Buffer) - Offset;
  if Available < 5 then
    Exit;

  Compressed := Buffer[Offset] <> 0;

  MsgLen := (Cardinal(Buffer[Offset + 1]) shl 24) or
            (Cardinal(Buffer[Offset + 2]) shl 16) or
            (Cardinal(Buffer[Offset + 3]) shl 8)  or
             Cardinal(Buffer[Offset + 4]);

  if Available < 5 + Integer(MsgLen) then
    Exit;

  SetLength(MsgBytes, MsgLen);
  if MsgLen > 0 then
    Move(Buffer[Offset + 5], MsgBytes[0], MsgLen);

  Inc(Offset, 5 + MsgLen);
  Result := True;
end;

class function THorseGrpcMessageCodec.Encode(const MsgBytes: TBytes;
  Compress: Boolean): TBytes;
var
  MsgLen: Cardinal;
begin
  MsgLen := Length(MsgBytes);
  SetLength(Result, 5 + MsgLen);

  if Compress then
    Result[0] := 1
  else
    Result[0] := 0;

  Result[1] := Byte(MsgLen shr 24);
  Result[2] := Byte(MsgLen shr 16);
  Result[3] := Byte(MsgLen shr 8);
  Result[4] := Byte(MsgLen);

  if MsgLen > 0 then
    Move(MsgBytes[0], Result[5], MsgLen);
end;

end.
