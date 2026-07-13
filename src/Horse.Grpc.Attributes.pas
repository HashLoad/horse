unit Horse.Grpc.Attributes;

{$IFDEF FPC}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  System.SysUtils;

type
  GrpcMessageAttribute = class(TCustomAttribute)
  end;

  ProtoMemberAttribute = class(TCustomAttribute)
  private
    FTag: Integer;
  public
    constructor Create(ATag: Integer);
    property Tag: Integer read FTag;
  end;

  GrpcServiceAttribute = class(TCustomAttribute)
  private
    FServiceName: string;
  public
    constructor Create(const AServiceName: string);
    property ServiceName: string read FServiceName;
  end;

  GrpcMethodAttribute = class(TCustomAttribute)
  private
    FGrpcMethodName: string;
  public
    constructor Create(const AGrpcMethodName: string);
    property GrpcMethodName: string read FGrpcMethodName;
  end;

implementation

{ ProtoMemberAttribute }

constructor ProtoMemberAttribute.Create(ATag: Integer);
begin
  FTag := ATag;
end;

{ GrpcServiceAttribute }

constructor GrpcServiceAttribute.Create(const AServiceName: string);
begin
  FServiceName := AServiceName;
end;

{ GrpcMethodAttribute }

constructor GrpcMethodAttribute.Create(const AGrpcMethodName: string);
begin
  FGrpcMethodName := AGrpcMethodName;
end;

end.
