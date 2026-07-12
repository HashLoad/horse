program horse_pb_compiler;

{$APPTYPE CONSOLE}

uses
  System.SysUtils,
  Horse.Protobuf.Compiler.Engine in 'Horse.Protobuf.Compiler.Engine.pas';

begin
  try
    if ParamCount < 2 then
    begin
      WriteLn('Usage: horse-pb-compiler <input.proto> <output.pas>');
      Exit;
    end;
    CompileProto(ParamStr(1), ParamStr(2));
    WriteLn('Protocol buffers compiled successfully.');
  except
    on E: Exception do
    begin
      WriteLn('Error compiling proto file: ' + E.Message);
      ExitCode := 1;
    end;
  end;
end.
