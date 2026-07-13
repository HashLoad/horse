unit Horse.Core.Http2.Hpack;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}
{$SCOPEDENUMS ON}

interface

uses
  System.SysUtils;

const
  HPACK_STATIC_TABLE_SIZE = 61;
  HPACK_DEFAULT_TABLE_SIZE = 4096;
  HPACK_MAX_DYNAMIC_ENTRIES = 256;

type
  TNameValuePair = record
    Name: string;
    Value: string;
  end;

  TNameValuePairs = array of TNameValuePair;

  THpackDynamicTable = record
  private
    FEntries: array[0..HPACK_MAX_DYNAMIC_ENTRIES - 1] of TNameValuePair;
    FHead: Integer;
    FCount: Integer;
    FCurrentSize: Integer;
    FMaxSize: Integer;
    function EntrySize(const AName, AValue: string): Integer; inline;
    procedure Evict;
  public
    procedure Init(AMaxSize: Integer);
    procedure Add(const AName, AValue: string);
    function Get(AIndex: Integer): TNameValuePair;
    function Count: Integer;
    procedure SetMaxSize(ANewMaxSize: Integer);
    property CurrentSize: Integer read FCurrentSize;
    property MaxSize: Integer read FMaxSize;
  end;

  THpackHuffman = class
  public
    class function Decode(AData: PByte; ALength: Integer): string; static;
    class function Encode(const AValue: string): TBytes; static;
    class function EncodedBitLength(const AValue: string): Integer; static;
  end;

  THpackDecoder = class
  private
    FDynTable: THpackDynamicTable;
    function GetStaticEntry(AIndex: Integer): TNameValuePair;
    function GetTableEntry(AIndex: Integer): TNameValuePair;
    function DecodeString(AData: PByte; AAvail: Integer; out AValue: string;
      out ABytesRead: Integer): Boolean;
    function DecodeInteger(AData: PByte; AAvail: Integer; APrefixBits: Integer;
      out AValue: Cardinal; out ABytesRead: Integer): Boolean;
  public
    constructor Create(AMaxTableSize: Integer = HPACK_DEFAULT_TABLE_SIZE);
    function Decode(AData: PByte; ALength: Integer;
      out AHeaders: TNameValuePairs): Boolean;
    procedure SetMaxTableSize(ASize: Integer);
  end;

  THpackEncoder = class
  private
    FDynTable: THpackDynamicTable;
    FMaxTableSize: Integer;
    function FindInStaticTable(const AName, AValue: string;
      out AIndex: Integer; out AExactMatch: Boolean): Boolean;
    function FindInDynamicTable(const AName, AValue: string;
      out AIndex: Integer; out AExactMatch: Boolean): Boolean;
    procedure EncodeInteger(AValue: Cardinal; APrefixBits: Integer;
      APrefix: Byte; var AOutput: TBytes; var APos: Integer);
    procedure EncodeString(const AValue: string;
      var AOutput: TBytes; var APos: Integer);
    procedure GrowBuffer(var AOutput: TBytes; ARequired: Integer; var APos: Integer);
  public
    constructor Create(AMaxTableSize: Integer = HPACK_DEFAULT_TABLE_SIZE);
    function Encode(const AHeaders: TNameValuePairs): TBytes;
    procedure SetMaxTableSize(ASize: Integer);
  end;

implementation

const
  STATIC_TABLE: array[1..HPACK_STATIC_TABLE_SIZE] of TNameValuePair = (
    (Name: ':authority';            Value: ''),
    (Name: ':method';               Value: 'GET'),
    (Name: ':method';               Value: 'POST'),
    (Name: ':path';                 Value: '/'),
    (Name: ':path';                 Value: '/index.html'),
    (Name: ':scheme';               Value: 'http'),
    (Name: ':scheme';               Value: 'https'),
    (Name: ':status';               Value: '200'),
    (Name: ':status';               Value: '204'),
    (Name: ':status';               Value: '206'),
    (Name: ':status';               Value: '304'),
    (Name: ':status';               Value: '400'),
    (Name: ':status';               Value: '404'),
    (Name: ':status';               Value: '500'),
    (Name: 'accept-charset';        Value: ''),
    (Name: 'accept-encoding';       Value: 'gzip, deflate'),
    (Name: 'accept-language';       Value: ''),
    (Name: 'accept-ranges';         Value: ''),
    (Name: 'accept';                Value: ''),
    (Name: 'access-control-allow-origin'; Value: ''),
    (Name: 'age';                   Value: ''),
    (Name: 'allow';                 Value: ''),
    (Name: 'authorization';         Value: ''),
    (Name: 'cache-control';         Value: ''),
    (Name: 'content-disposition';   Value: ''),
    (Name: 'content-encoding';      Value: ''),
    (Name: 'content-language';      Value: ''),
    (Name: 'content-length';        Value: ''),
    (Name: 'content-location';      Value: ''),
    (Name: 'content-range';         Value: ''),
    (Name: 'content-type';          Value: ''),
    (Name: 'cookie';                Value: ''),
    (Name: 'date';                  Value: ''),
    (Name: 'etag';                  Value: ''),
    (Name: 'expect';                Value: ''),
    (Name: 'expires';               Value: ''),
    (Name: 'from';                  Value: ''),
    (Name: 'host';                  Value: ''),
    (Name: 'if-match';              Value: ''),
    (Name: 'if-modified-since';     Value: ''),
    (Name: 'if-none-match';         Value: ''),
    (Name: 'if-range';              Value: ''),
    (Name: 'if-unmodified-since';   Value: ''),
    (Name: 'last-modified';         Value: ''),
    (Name: 'link';                  Value: ''),
    (Name: 'location';              Value: ''),
    (Name: 'max-forwards';          Value: ''),
    (Name: 'proxy-authenticate';    Value: ''),
    (Name: 'proxy-authorization';   Value: ''),
    (Name: 'range';                 Value: ''),
    (Name: 'referer';               Value: ''),
    (Name: 'refresh';               Value: ''),
    (Name: 'retry-after';           Value: ''),
    (Name: 'server';                Value: ''),
    (Name: 'set-cookie';            Value: ''),
    (Name: 'strict-transport-security'; Value: ''),
    (Name: 'transfer-encoding';     Value: ''),
    (Name: 'user-agent';            Value: ''),
    (Name: 'vary';                  Value: ''),
    (Name: 'via';                   Value: ''),
    (Name: 'www-authenticate';      Value: '')
  );

type
  THuffEntry = record
    Code: Cardinal;
    Bits: Byte;
  end;

const
  HUFF_TABLE: array[0..256] of THuffEntry = (
    (Code: $1ff8;     Bits: 13),  // 0
    (Code: $7fffd8;   Bits: 23),  // 1
    (Code: $fffffe2;  Bits: 28),  // 2
    (Code: $fffffe3;  Bits: 28),  // 3
    (Code: $fffffe4;  Bits: 28),  // 4
    (Code: $fffffe5;  Bits: 28),  // 5
    (Code: $fffffe6;  Bits: 28),  // 6
    (Code: $fffffe7;  Bits: 28),  // 7
    (Code: $fffffe8;  Bits: 28),  // 8
    (Code: $ffffea;   Bits: 24),  // 9
    (Code: $3ffffffc; Bits: 30),  // 10
    (Code: $fffffe9;  Bits: 28),  // 11
    (Code: $fffffea;  Bits: 28),  // 12
    (Code: $3ffffffd; Bits: 30),  // 13
    (Code: $fffffeb;  Bits: 28),  // 14
    (Code: $fffffec;  Bits: 28),  // 15
    (Code: $fffffed;  Bits: 28),  // 16
    (Code: $fffffee;  Bits: 28),  // 17
    (Code: $fffffef;  Bits: 28),  // 18
    (Code: $ffffff0;  Bits: 28),  // 19
    (Code: $ffffff1;  Bits: 28),  // 20
    (Code: $ffffff2;  Bits: 28),  // 21
    (Code: $3ffffffe; Bits: 30),  // 22
    (Code: $ffffff3;  Bits: 28),  // 23
    (Code: $ffffff4;  Bits: 28),  // 24
    (Code: $ffffff5;  Bits: 28),  // 25
    (Code: $ffffff6;  Bits: 28),  // 26
    (Code: $ffffff7;  Bits: 28),  // 27
    (Code: $ffffff8;  Bits: 28),  // 28
    (Code: $ffffff9;  Bits: 28),  // 29
    (Code: $ffffffa;  Bits: 28),  // 30
    (Code: $ffffffb;  Bits: 28),  // 31
    (Code: $14;       Bits:  6),  // 32
    (Code: $3f8;      Bits: 10),  // 33
    (Code: $3f9;      Bits: 10),  // 34
    (Code: $ffa;      Bits: 12),  // 35
    (Code: $1ff9;     Bits: 13),  // 36
    (Code: $15;       Bits:  6),  // 37
    (Code: $f8;       Bits:  8),  // 38
    (Code: $7fa;      Bits: 11),  // 39
    (Code: $3fa;      Bits: 10),  // 40
    (Code: $3fb;      Bits: 10),  // 41
    (Code: $f9;       Bits:  8),  // 42
    (Code: $7fb;      Bits: 11),  // 43
    (Code: $fa;       Bits:  8),  // 44
    (Code: $16;       Bits:  6),  // 45
    (Code: $17;       Bits:  6),  // 46
    (Code: $18;       Bits:  6),  // 47
    (Code: $0;        Bits:  5),  // 48
    (Code: $1;        Bits:  5),  // 49
    (Code: $2;        Bits:  5),  // 50
    (Code: $19;       Bits:  6),  // 51
    (Code: $1a;       Bits:  6),  // 52
    (Code: $1b;       Bits:  6),  // 53
    (Code: $1c;       Bits:  6),  // 54
    (Code: $1d;       Bits:  6),  // 55
    (Code: $1e;       Bits:  6),  // 56
    (Code: $1f;       Bits:  6),  // 57
    (Code: $5c;       Bits:  7),  // 58
    (Code: $fb;       Bits:  8),  // 59
    (Code: $7ffc;     Bits: 15),  // 60
    (Code: $20;       Bits:  6),  // 61
    (Code: $ffb;      Bits: 12),  // 62
    (Code: $3fc;      Bits: 10),  // 63
    (Code: $1ffa;     Bits: 13),  // 64
    (Code: $21;       Bits:  6),  // 65
    (Code: $5d;       Bits:  7),  // 66
    (Code: $5e;       Bits:  7),  // 67
    (Code: $5f;       Bits:  7),  // 68
    (Code: $60;       Bits:  7),  // 69
    (Code: $61;       Bits:  7),  // 70
    (Code: $62;       Bits:  7),  // 71
    (Code: $63;       Bits:  7),  // 72
    (Code: $64;       Bits:  7),  // 73
    (Code: $65;       Bits:  7),  // 74
    (Code: $66;       Bits:  7),  // 75
    (Code: $67;       Bits:  7),  // 76
    (Code: $68;       Bits:  7),  // 77
    (Code: $69;       Bits:  7),  // 78
    (Code: $6a;       Bits:  7),  // 79
    (Code: $6b;       Bits:  7),  // 80
    (Code: $6c;       Bits:  7),  // 81
    (Code: $6d;       Bits:  7),  // 82
    (Code: $6e;       Bits:  7),  // 83
    (Code: $6f;       Bits:  7),  // 84
    (Code: $70;       Bits:  7),  // 85
    (Code: $71;       Bits:  7),  // 86
    (Code: $72;       Bits:  7),  // 87
    (Code: $fc;       Bits:  8),  // 88
    (Code: $73;       Bits:  7),  // 89
    (Code: $fd;       Bits:  8),  // 90
    (Code: $1ffb;     Bits: 13),  // 91
    (Code: $7fff0;    Bits: 19),  // 92
    (Code: $1ffc;     Bits: 13),  // 93
    (Code: $3ffc;     Bits: 14),  // 94
    (Code: $22;       Bits:  6),  // 95
    (Code: $7ffd;     Bits: 15),  // 96
    (Code: $3;        Bits:  5),  // 97
    (Code: $23;       Bits:  6),  // 98
    (Code: $4;        Bits:  5),  // 99
    (Code: $24;       Bits:  6),  // 100
    (Code: $5;        Bits:  5),  // 101
    (Code: $25;       Bits:  6),  // 102
    (Code: $26;       Bits:  6),  // 103
    (Code: $27;       Bits:  6),  // 104
    (Code: $6;        Bits:  5),  // 105
    (Code: $74;       Bits:  7),  // 106
    (Code: $75;       Bits:  7),  // 107
    (Code: $28;       Bits:  6),  // 108
    (Code: $29;       Bits:  6),  // 109
    (Code: $2a;       Bits:  6),  // 110
    (Code: $7;        Bits:  5),  // 111
    (Code: $2b;       Bits:  6),  // 112
    (Code: $76;       Bits:  7),  // 113
    (Code: $2c;       Bits:  6),  // 114
    (Code: $8;        Bits:  5),  // 115
    (Code: $9;        Bits:  5),  // 116
    (Code: $2d;       Bits:  6),  // 117
    (Code: $77;       Bits:  7),  // 118
    (Code: $78;       Bits:  7),  // 119
    (Code: $79;       Bits:  7),  // 120
    (Code: $7a;       Bits:  7),  // 121
    (Code: $7b;       Bits:  7),  // 122
    (Code: $7ffe;     Bits: 15),  // 123
    (Code: $7fc;      Bits: 11),  // 124
    (Code: $3ffd;     Bits: 14),  // 125
    (Code: $1ffd;     Bits: 13),  // 126
    (Code: $ffffffc;  Bits: 28),  // 127
    (Code: $fffe6;    Bits: 20),  // 128
    (Code: $3fffd2;   Bits: 22),  // 129
    (Code: $fffe7;    Bits: 20),  // 130
    (Code: $fffe8;    Bits: 20),  // 131
    (Code: $3fffd3;   Bits: 22),  // 132
    (Code: $3fffd4;   Bits: 22),  // 133
    (Code: $3fffd5;   Bits: 22),  // 134
    (Code: $7fffd9;   Bits: 23),  // 135
    (Code: $3fffd6;   Bits: 22),  // 136
    (Code: $7fffda;   Bits: 23),  // 137
    (Code: $7fffdb;   Bits: 23),  // 138
    (Code: $7fffdc;   Bits: 23),  // 139
    (Code: $7fffdd;   Bits: 23),  // 140
    (Code: $7fffde;   Bits: 23),  // 141
    (Code: $ffffeb;   Bits: 24),  // 142
    (Code: $7fffdf;   Bits: 23),  // 143
    (Code: $ffffec;   Bits: 24),  // 144
    (Code: $ffffed;   Bits: 24),  // 145
    (Code: $3fffd7;   Bits: 22),  // 146
    (Code: $7fffe0;   Bits: 23),  // 147
    (Code: $ffffee;   Bits: 24),  // 148
    (Code: $7fffe1;   Bits: 23),  // 149
    (Code: $7fffe2;   Bits: 23),  // 150
    (Code: $7fffe3;   Bits: 23),  // 151
    (Code: $7fffe4;   Bits: 23),  // 152
    (Code: $1fffdc;   Bits: 21),  // 153
    (Code: $3fffd8;   Bits: 22),  // 154
    (Code: $7fffe5;   Bits: 23),  // 155
    (Code: $3fffd9;   Bits: 22),  // 156
    (Code: $7fffe6;   Bits: 23),  // 157
    (Code: $7fffe7;   Bits: 23),  // 158
    (Code: $ffffef;   Bits: 24),  // 159
    (Code: $3fffda;   Bits: 22),  // 160
    (Code: $1fffdd;   Bits: 21),  // 161
    (Code: $fffe9;    Bits: 20),  // 162
    (Code: $3fffdb;   Bits: 22),  // 163
    (Code: $3fffdc;   Bits: 22),  // 164
    (Code: $7fffe8;   Bits: 23),  // 165
    (Code: $7fffe9;   Bits: 23),  // 166
    (Code: $1fffde;   Bits: 21),  // 167
    (Code: $7fffea;   Bits: 23),  // 168
    (Code: $3fffdd;   Bits: 22),  // 169
    (Code: $3fffde;   Bits: 22),  // 170
    (Code: $fffff0;   Bits: 24),  // 171
    (Code: $1fffdf;   Bits: 21),  // 172
    (Code: $3fffdf;   Bits: 22),  // 173
    (Code: $7fffeb;   Bits: 23),  // 174
    (Code: $7fffec;   Bits: 23),  // 175
    (Code: $1fffe0;   Bits: 21),  // 176
    (Code: $1fffe1;   Bits: 21),  // 177
    (Code: $3fffe0;   Bits: 22),  // 178
    (Code: $1fffe2;   Bits: 21),  // 179
    (Code: $7fffed;   Bits: 23),  // 180
    (Code: $3fffe1;   Bits: 22),  // 181
    (Code: $7fffee;   Bits: 23),  // 182
    (Code: $7fffef;   Bits: 23),  // 183
    (Code: $fffea;    Bits: 20),  // 184
    (Code: $3fffe2;   Bits: 22),  // 185
    (Code: $3fffe3;   Bits: 22),  // 186
    (Code: $3fffe4;   Bits: 22),  // 187
    (Code: $7ffff0;   Bits: 23),  // 188
    (Code: $3fffe5;   Bits: 22),  // 189
    (Code: $3fffe6;   Bits: 22),  // 190
    (Code: $7ffff1;   Bits: 23),  // 191
    (Code: $3ffffe0;  Bits: 26),  // 192
    (Code: $3ffffe1;  Bits: 26),  // 193
    (Code: $fffeb;    Bits: 20),  // 194
    (Code: $7fff1;    Bits: 19),  // 195
    (Code: $3fffe7;   Bits: 22),  // 196
    (Code: $7ffff2;   Bits: 23),  // 197
    (Code: $3fffe8;   Bits: 22),  // 198
    (Code: $1ffffec;  Bits: 25),  // 199
    (Code: $3ffffe2;  Bits: 26),  // 200
    (Code: $3ffffe3;  Bits: 26),  // 201
    (Code: $3ffffe4;  Bits: 26),  // 202
    (Code: $7ffffde;  Bits: 27),  // 203
    (Code: $7ffffdf;  Bits: 27),  // 204
    (Code: $3ffffe5;  Bits: 26),  // 205
    (Code: $fffff1;   Bits: 24),  // 206
    (Code: $1ffffed;  Bits: 25),  // 207
    (Code: $7fff2;    Bits: 19),  // 208
    (Code: $1fffe3;   Bits: 21),  // 209
    (Code: $3ffffe6;  Bits: 26),  // 210
    (Code: $7ffffe0;  Bits: 27),  // 211
    (Code: $7ffffe1;  Bits: 27),  // 212
    (Code: $3ffffe7;  Bits: 26),  // 213
    (Code: $7ffffe2;  Bits: 27),  // 214
    (Code: $fffff2;   Bits: 24),  // 215
    (Code: $1fffe4;   Bits: 21),  // 216
    (Code: $1fffe5;   Bits: 21),  // 217
    (Code: $3ffffe8;  Bits: 26),  // 218
    (Code: $3ffffe9;  Bits: 26),  // 219
    (Code: $ffffffd;  Bits: 28),  // 220
    (Code: $7ffffe3;  Bits: 27),  // 221
    (Code: $7ffffe4;  Bits: 27),  // 222
    (Code: $7ffffe5;  Bits: 27),  // 223
    (Code: $fffec;    Bits: 20),  // 224
    (Code: $fffff3;   Bits: 24),  // 225
    (Code: $fffed;    Bits: 20),  // 226
    (Code: $1fffe6;   Bits: 21),  // 227
    (Code: $3fffe9;   Bits: 22),  // 228
    (Code: $1fffe7;   Bits: 21),  // 229
    (Code: $1fffe8;   Bits: 21),  // 230
    (Code: $7ffff3;   Bits: 23),  // 231
    (Code: $3fffea;   Bits: 22),  // 232
    (Code: $3fffeb;   Bits: 22),  // 233
    (Code: $1ffffee;  Bits: 25),  // 234
    (Code: $1ffffef;  Bits: 25),  // 235
    (Code: $fffff4;   Bits: 24),  // 236
    (Code: $fffff5;   Bits: 24),  // 237
    (Code: $3ffffea;  Bits: 26),  // 238
    (Code: $7ffff4;   Bits: 23),  // 239
    (Code: $3ffffeb;  Bits: 26),  // 240
    (Code: $7ffffe6;  Bits: 27),  // 241
    (Code: $3ffffec;  Bits: 26),  // 242
    (Code: $3ffffed;  Bits: 26),  // 243
    (Code: $7ffffe7;  Bits: 27),  // 244
    (Code: $7ffffe8;  Bits: 27),  // 245
    (Code: $7ffffe9;  Bits: 27),  // 246
    (Code: $7ffffea;  Bits: 27),  // 247
    (Code: $7ffffeb;  Bits: 27),  // 248
    (Code: $ffffffe;  Bits: 28),  // 249
    (Code: $7ffffec;  Bits: 27),  // 250
    (Code: $7ffffed;  Bits: 27),  // 251
    (Code: $7ffffee;  Bits: 27),  // 252
    (Code: $7ffffef;  Bits: 27),  // 253
    (Code: $7fffff0;  Bits: 27),  // 254
    (Code: $3ffffee;  Bits: 26),  // 255
    (Code: $3fffffff; Bits: 30)   // 256 = EOS
  );

{ THpackDynamicTable }

function THpackDynamicTable.EntrySize(const AName, AValue: string): Integer;
begin
  Result := Length(TEncoding.UTF8.GetBytes(AName)) +
            Length(TEncoding.UTF8.GetBytes(AValue)) + 32;
end;

procedure THpackDynamicTable.Init(AMaxSize: Integer);
begin
  FHead := 0;
  FCount := 0;
  FCurrentSize := 0;
  FMaxSize := AMaxSize;
end;

procedure THpackDynamicTable.Evict;
var
  Tail: Integer;
  EntrySizeValue: Integer;
begin
  Tail := (FHead + FCount - 1) mod HPACK_MAX_DYNAMIC_ENTRIES;
  EntrySizeValue := EntrySize(FEntries[Tail].Name, FEntries[Tail].Value);
  Dec(FCurrentSize, EntrySizeValue);
  Dec(FCount);
end;

procedure THpackDynamicTable.Add(const AName, AValue: string);
var
  size: Integer;
begin
  size := EntrySize(AName, AValue);
  while (FCurrentSize + size > FMaxSize) and (FCount > 0) do
    Evict;
  if size > FMaxSize then
    Exit;
  FHead := (FHead - 1 + HPACK_MAX_DYNAMIC_ENTRIES) mod HPACK_MAX_DYNAMIC_ENTRIES;
  FEntries[FHead].Name  := AName;
  FEntries[FHead].Value := AValue;
  Inc(FCount);
  Inc(FCurrentSize, size);
end;

function THpackDynamicTable.Get(AIndex: Integer): TNameValuePair;
begin
  if (AIndex < 1) or (AIndex > FCount) then
    raise EArgumentOutOfRangeException.CreateFmt('HPACK dynamic table index %d out of range [1..%d]', [AIndex, FCount]);
  Result := FEntries[(FHead + AIndex - 1) mod HPACK_MAX_DYNAMIC_ENTRIES];
end;

function THpackDynamicTable.Count: Integer;
begin
  Result := FCount;
end;

procedure THpackDynamicTable.SetMaxSize(ANewMaxSize: Integer);
begin
  FMaxSize := ANewMaxSize;
  while FCurrentSize > FMaxSize do
    Evict;
end;

{ THpackHuffman }

class function THpackHuffman.EncodedBitLength(const AValue: string): Integer;
var
  bytes: TBytes;
  i: Integer;
begin
  Result := 0;
  bytes := TEncoding.UTF8.GetBytes(AValue);
  for i := 0 to High(bytes) do
    Inc(Result, HUFF_TABLE[bytes[i]].Bits);
end;

class function THpackHuffman.Encode(const AValue: string): TBytes;
var
  bytes: TBytes;
  bitLen: Integer;
  byteLen: Integer;
  bits: UInt64;
  bitsAvail: Integer;
  i, b: Integer;
  code: Cardinal;
  codeBits: Byte;
  pos: Integer;
begin
  bytes := TEncoding.UTF8.GetBytes(AValue);
  bitLen := 0;
  for i := 0 to High(bytes) do
    Inc(bitLen, HUFF_TABLE[bytes[i]].Bits);
  byteLen := (bitLen + 7) div 8;
  SetLength(Result, byteLen);
  FillChar(Result[0], byteLen, $FF);

  bits := 0;
  bitsAvail := 0;
  pos := 0;
  for i := 0 to High(bytes) do
  begin
    b := bytes[i];
    code := HUFF_TABLE[b].Code;
    codeBits := HUFF_TABLE[b].Bits;
    bits := (bits shl codeBits) or code;
    Inc(bitsAvail, codeBits);
    while bitsAvail >= 8 do
    begin
      Dec(bitsAvail, 8);
      Result[pos] := Byte(bits shr bitsAvail);
      Inc(pos);
    end;
  end;
  if bitsAvail > 0 then
  begin
    Result[pos] := Byte((bits shl (8 - bitsAvail)) or ($FF shr bitsAvail));
  end;
end;

class function THpackHuffman.Decode(AData: PByte; ALength: Integer): string;
var
  bits: UInt64;
  bitsAvail: Integer;
  i: Integer;
  sym: Integer;
  best: Integer;
  bestBits: Integer;
  resultBytes: TBytes;
  count: Integer;
  code: Cardinal;
  codeBits: Byte;
  match: Boolean;
begin
  Result := '';
  bits := 0;
  bitsAvail := 0;
  SetLength(resultBytes, ALength * 2);
  count := 0;

  for i := 0 to ALength - 1 do
  begin
    bits := (bits shl 8) or AData[i];
    Inc(bitsAvail, 8);

    while bitsAvail > 0 do
    begin
      best := -1;
      bestBits := 0;
      match := False;

      for sym := 0 to 255 do
      begin
        codeBits := HUFF_TABLE[sym].Bits;
        if codeBits > bitsAvail then Continue;
        code := HUFF_TABLE[sym].Code;
        if (bits shr (bitsAvail - codeBits)) and ((Cardinal(1) shl codeBits) - 1) = code then
        begin
          if codeBits > bestBits then
          begin
            best := sym;
            bestBits := codeBits;
            match := True;
          end;
        end;
      end;

      if not match then Break;

      Dec(bitsAvail, bestBits);
      bits := bits and ((UInt64(1) shl bitsAvail) - 1);

      if count >= Length(resultBytes) then
        SetLength(resultBytes, count + 64);
      resultBytes[count] := Byte(best);
      Inc(count);
    end;
  end;

  SetLength(resultBytes, count);
  Result := TEncoding.UTF8.GetString(resultBytes);
end;

{ THpackDecoder }

constructor THpackDecoder.Create(AMaxTableSize: Integer);
begin
  inherited Create;
  FDynTable.Init(AMaxTableSize);
end;

function THpackDecoder.GetStaticEntry(AIndex: Integer): TNameValuePair;
begin
  if (AIndex < 1) or (AIndex > HPACK_STATIC_TABLE_SIZE) then
    raise EArgumentOutOfRangeException.CreateFmt('HPACK static table index %d out of range', [AIndex]);
  Result := STATIC_TABLE[AIndex];
end;

function THpackDecoder.GetTableEntry(AIndex: Integer): TNameValuePair;
begin
  if AIndex <= HPACK_STATIC_TABLE_SIZE then
    Result := GetStaticEntry(AIndex)
  else
    Result := FDynTable.Get(AIndex - HPACK_STATIC_TABLE_SIZE);
end;

function THpackDecoder.DecodeInteger(AData: PByte; AAvail, APrefixBits: Integer;
  out AValue: Cardinal; out ABytesRead: Integer): Boolean;
var
  mask: Byte;
  shift: Integer;
  b: Byte;
begin
  Result := False;
  if AAvail < 1 then Exit;
  mask := Byte((1 shl APrefixBits) - 1);
  AValue := AData[0] and mask;
  ABytesRead := 1;
  if AValue < Cardinal(mask) then
  begin
    Result := True;
    Exit;
  end;
  shift := 0;
  repeat
    if ABytesRead >= AAvail then Exit;
    b := AData[ABytesRead];
    Inc(ABytesRead);
    AValue := AValue + (Cardinal(b and $7F) shl shift);
    Inc(shift, 7);
  until (b and $80) = 0;
  Result := True;
end;

function THpackDecoder.DecodeString(AData: PByte; AAvail: Integer;
  out AValue: string; out ABytesRead: Integer): Boolean;
var
  huffman: Boolean;
  strLen: Cardinal;
  intRead: Integer;
  rawSlice: TBytes;
begin
  Result := False;
  if AAvail < 1 then Exit;
  huffman := (AData[0] and $80) <> 0;
  if not DecodeInteger(AData, AAvail, 7, strLen, intRead) then Exit;
  Inc(AData, intRead);
  Dec(AAvail, intRead);
  if AAvail < Integer(strLen) then Exit;
  if huffman then
    AValue := THpackHuffman.Decode(AData, strLen)
  else
  begin
    SetLength(rawSlice, strLen);
    if strLen > 0 then
      Move(AData^, rawSlice[0], strLen);
    AValue := TEncoding.UTF8.GetString(rawSlice);
  end;
  ABytesRead := intRead + Integer(strLen);
  Result := True;
end;

function THpackDecoder.Decode(AData: PByte; ALength: Integer;
  out AHeaders: TNameValuePairs): Boolean;
var
  pos: Integer;
  avail: Integer;
  b: Byte;
  index: Cardinal;
  intRead: Integer;
  strRead: Integer;
  nameStr, valueStr: string;
  entry: TNameValuePair;
  indexing: Boolean;
  headerCount: Integer;
  newSize: Cardinal;
begin
  Result := False;
  SetLength(AHeaders, 0);
  headerCount := 0;
  pos := 0;
  avail := ALength;

  while avail > 0 do
  begin
    b := AData[pos];

    if (b and $80) <> 0 then
    begin
      if not DecodeInteger(AData + pos, avail, 7, index, intRead) then Exit;
      if index = 0 then Exit;
      entry := GetTableEntry(index);
      SetLength(AHeaders, headerCount + 1);
      AHeaders[headerCount] := entry;
      Inc(headerCount);
      Inc(pos, intRead);
      Dec(avail, intRead);
      Continue;
    end;

    if (b and $C0) = $40 then
    begin
      indexing := True;
      if not DecodeInteger(AData + pos, avail, 6, index, intRead) then Exit;
      Inc(pos, intRead);
      Dec(avail, intRead);
      if index > 0 then
        nameStr := GetTableEntry(index).Name
      else
      begin
        if not DecodeString(AData + pos, avail, nameStr, strRead) then Exit;
        Inc(pos, strRead);
        Dec(avail, strRead);
      end;
      if not DecodeString(AData + pos, avail, valueStr, strRead) then Exit;
      Inc(pos, strRead);
      Dec(avail, strRead);
      if indexing then
        FDynTable.Add(nameStr, valueStr);
      SetLength(AHeaders, headerCount + 1);
      AHeaders[headerCount].Name := nameStr;
      AHeaders[headerCount].Value := valueStr;
      Inc(headerCount);
      Continue;
    end;

    if (b and $E0) = $20 then
    begin
      if not DecodeInteger(AData + pos, avail, 5, newSize, intRead) then Exit;
      FDynTable.SetMaxSize(newSize);
      Inc(pos, intRead);
      Dec(avail, intRead);
      Continue;
    end;

    begin
      if not DecodeInteger(AData + pos, avail, 4, index, intRead) then Exit;
      Inc(pos, intRead);
      Dec(avail, intRead);
      if index > 0 then
        nameStr := GetTableEntry(index).Name
      else
      begin
        if not DecodeString(AData + pos, avail, nameStr, strRead) then Exit;
        Inc(pos, strRead);
        Dec(avail, strRead);
      end;
      if not DecodeString(AData + pos, avail, valueStr, strRead) then Exit;
      Inc(pos, strRead);
      Dec(avail, strRead);
      SetLength(AHeaders, headerCount + 1);
      AHeaders[headerCount].Name := nameStr;
      AHeaders[headerCount].Value := valueStr;
      Inc(headerCount);
    end;
  end;

  Result := True;
end;

procedure THpackDecoder.SetMaxTableSize(ASize: Integer);
begin
  FDynTable.SetMaxSize(ASize);
end;

{ THpackEncoder }

constructor THpackEncoder.Create(AMaxTableSize: Integer);
begin
  inherited Create;
  FMaxTableSize := AMaxTableSize;
  FDynTable.Init(AMaxTableSize);
end;

function THpackEncoder.FindInStaticTable(const AName, AValue: string;
  out AIndex: Integer; out AExactMatch: Boolean): Boolean;
var
  i: Integer;
  lowerName: string;
begin
  Result := False;
  AIndex := 0;
  AExactMatch := False;
  lowerName := LowerCase(AName);
  for i := 1 to HPACK_STATIC_TABLE_SIZE do
  begin
    if STATIC_TABLE[i].Name = lowerName then
    begin
      Result := True;
      if not AExactMatch then
        AIndex := i;
      if (STATIC_TABLE[i].Value = AValue) then
      begin
        AIndex := i;
        AExactMatch := True;
        Exit;
      end;
    end;
  end;
end;

function THpackEncoder.FindInDynamicTable(const AName, AValue: string;
  out AIndex: Integer; out AExactMatch: Boolean): Boolean;
var
  i: Integer;
  entry: TNameValuePair;
  lowerName: string;
begin
  Result := False;
  AIndex := 0;
  AExactMatch := False;
  lowerName := LowerCase(AName);
  for i := 1 to FDynTable.Count do
  begin
    entry := FDynTable.Get(i);
    if entry.Name = lowerName then
    begin
      Result := True;
      if not AExactMatch then
        AIndex := HPACK_STATIC_TABLE_SIZE + i;
      if entry.Value = AValue then
      begin
        AIndex := HPACK_STATIC_TABLE_SIZE + i;
        AExactMatch := True;
        Exit;
      end;
    end;
  end;
end;

procedure THpackEncoder.GrowBuffer(var AOutput: TBytes; ARequired: Integer; var APos: Integer);
begin
  if APos + ARequired > Length(AOutput) then
    SetLength(AOutput, APos + ARequired + 256);
end;

procedure THpackEncoder.EncodeInteger(AValue: Cardinal; APrefixBits: Integer;
  APrefix: Byte; var AOutput: TBytes; var APos: Integer);
var
  mask: Byte;
begin
  mask := Byte((1 shl APrefixBits) - 1);
  GrowBuffer(AOutput, 6, APos);
  if AValue < Cardinal(mask) then
  begin
    AOutput[APos] := APrefix or Byte(AValue);
    Inc(APos);
  end
  else
  begin
    AOutput[APos] := APrefix or mask;
    Inc(APos);
    Dec(AValue, mask);
    while AValue >= 128 do
    begin
      GrowBuffer(AOutput, 2, APos);
      AOutput[APos] := Byte((AValue and $7F) or $80);
      Inc(APos);
      AValue := AValue shr 7;
    end;
    GrowBuffer(AOutput, 1, APos);
    AOutput[APos] := Byte(AValue);
    Inc(APos);
  end;
end;

procedure THpackEncoder.EncodeString(const AValue: string;
  var AOutput: TBytes; var APos: Integer);
var
  utf8: TBytes;
  huffBits: Integer;
  huffBytes: TBytes;
begin
  utf8 := TEncoding.UTF8.GetBytes(AValue);
  huffBits := THpackHuffman.EncodedBitLength(AValue);
  if (huffBits div 8) < Length(utf8) then
  begin
    huffBytes := THpackHuffman.Encode(AValue);
    EncodeInteger(Length(huffBytes), 7, $80, AOutput, APos);
    GrowBuffer(AOutput, Length(huffBytes), APos);
    Move(huffBytes[0], AOutput[APos], Length(huffBytes));
    Inc(APos, Length(huffBytes));
  end
  else
  begin
    EncodeInteger(Length(utf8), 7, $00, AOutput, APos);
    GrowBuffer(AOutput, Length(utf8), APos);
    if Length(utf8) > 0 then
      Move(utf8[0], AOutput[APos], Length(utf8));
    Inc(APos, Length(utf8));
  end;
end;

function THpackEncoder.Encode(const AHeaders: TNameValuePairs): TBytes;
var
  i: Integer;
  pair: TNameValuePair;
  staticIdx, dynIdx: Integer;
  staticExact, dynExact: Boolean;
  hasStatic, hasDyn: Boolean;
  bestIdx: Integer;
  exactMatch: Boolean;
  pos: Integer;
begin
  SetLength(Result, 256);
  pos := 0;

  for i := 0 to High(AHeaders) do
  begin
    pair := AHeaders[i];
    hasStatic := FindInStaticTable(pair.Name, pair.Value, staticIdx, staticExact);
    hasDyn := FindInDynamicTable(pair.Name, pair.Value, dynIdx, dynExact);

    bestIdx := 0;
    exactMatch := False;
    if hasStatic then begin bestIdx := staticIdx; exactMatch := staticExact; end;
    if hasDyn and dynExact then begin bestIdx := dynIdx; exactMatch := True; end
    else if hasDyn and (not exactMatch) then begin bestIdx := dynIdx; end;

    if exactMatch and (bestIdx > 0) then
    begin
      EncodeInteger(bestIdx, 7, $80, Result, pos);
    end
    else if bestIdx > 0 then
    begin
      EncodeInteger(bestIdx, 6, $40, Result, pos);
      EncodeString(pair.Value, Result, pos);
      FDynTable.Add(LowerCase(pair.Name), pair.Value);
    end
    else
    begin
      GrowBuffer(Result, 1, pos);
      Result[pos] := $40;
      Inc(pos);
      EncodeString(pair.Name, Result, pos);
      EncodeString(pair.Value, Result, pos);
      FDynTable.Add(LowerCase(pair.Name), pair.Value);
    end;
  end;

  SetLength(Result, pos);
end;

procedure THpackEncoder.SetMaxTableSize(ASize: Integer);
begin
  FMaxTableSize := ASize;
  FDynTable.SetMaxSize(ASize);
end;

end.
