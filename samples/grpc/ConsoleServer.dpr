program ConsoleServer;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  users in 'users.pas',
  Horse.Provider.Grpc in '../../src/Horse.Provider.Grpc.pas',
  Horse.Grpc.Attributes in '../../src/Horse.Grpc.Attributes.pas';

type
  {$M+}
  TUserServiceImpl = class(TInterfacedObject, IUserService)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  published
    function GetUser(const ARequest: TUserRequest): TUserResponse;
  end;

function TUserServiceImpl._AddRef: Integer;
begin
  Result := -1;
end;

function TUserServiceImpl._Release: Integer;
begin
  Result := -1;
end;

function TUserServiceImpl.GetUser(const ARequest: TUserRequest): TUserResponse;
begin
  Result := TUserResponse.Create;
  Result.id := ARequest.id;
  Result.name := 'Joao Silva';
  Result.email := 'joao.silva@example.com';
  WriteLn('gRPC GetUser called with ID: ', ARequest.id);
end;

begin
  try
    WriteLn('Registering UserService...');
    Flush(Output);
    THorseGrpcProvider.RegisterService(IUserService, TUserServiceImpl);

    WriteLn('Exporting Proto Schema (Code-First):');
    WriteLn('====================================');
    WriteLn(THorseGrpcProvider.ExportProto);
    WriteLn('====================================');
    Flush(Output);

    WriteLn('Starting gRPC Server (HTTP/2 h2c) on port 9090...');
    Flush(Output);
    THorseGrpcProvider.Start(9090);
    WriteLn('Server started. Press ENTER to stop.');
    Flush(Output);
    ReadLn;
    THorseGrpcProvider.Stop;
    WriteLn('Server stopped.');
    Flush(Output);
  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
      Flush(Output);
    end;
  end;
end.
