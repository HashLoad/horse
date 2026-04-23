unit ControllerTeste;

{$mode delphi}{$H+}

interface

uses Horse, Horse.Controller, SysUtils, fpjson;

type
  {$M+} // Ativa a RTTI para os métodos published no FPC
  TControllerTeste = class(THorseController)
  published
    procedure ListUsers;
    procedure GetUserById;
  end;
  {$M-}

implementation

procedure TControllerTeste.ListUsers;
var
  LJSON: TJSONArray;
begin
  LJSON := TJSONArray.Create;
  LJSON.Add('Lazarus User');
  LJSON.Add('FPC Power');
  Response.Send(LJSON.AsJSON).Status(200);
end;

procedure TControllerTeste.GetUserById;
begin
  Response.Send('FPC ID: ' + Request.Params['id']).Status(200);
end;

end.
