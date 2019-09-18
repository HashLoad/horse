unit Horse.CodeGen.Templates;

interface

resourcestring
  sHorseDPR =
    'program %0:s;' + sLineBreak + sLineBreak +
    '{$APPTYPE CONSOLE}' + sLineBreak + sLineBreak +
    'uses' + sLineBreak +
    '  Horse;' + sLineBreak +  sLineBreak +
    '{$R *.res}' + sLineBreak + sLineBreak +
    'var' + sLineBreak +
    '  App: THorse;' + sLineBreak + sLineBreak +
    'begin' + sLineBreak +
    '  App := THorse.Create(9000);' + sLineBreak + sLineBreak +
    '  App.Get(''/ping'',' + sLineBreak +
    '    procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)' + sLineBreak +
    '    begin' + sLineBreak +
    '      Res.Send(''pong'');' + sLineBreak +
    '    end);' + sLineBreak + sLineBreak +
    '  App.Start;' + sLineBreak +
    'end.' + sLineBreak;

implementation

end.
