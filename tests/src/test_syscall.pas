program test_syscall;
{$mode delphi}
uses BaseUnix, Unix;
var
  LRes: PtrInt;
begin
  LRes := fpSyscall(220, 0);
end.
