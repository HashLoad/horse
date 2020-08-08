unit Horse.MethodType;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

{$IF DEFINED(FPC)}
uses
  Classes, SysUtils;

type

TMethodType = (mtAny, mtGet, mtPut, mtPost, mtHead, mtDelete, mtPatch);
function StringCommandToMethodType(ACommand: string): TMethodType;

{$ENDIF}

implementation

{$IF DEFINED(FPC)}
function StringCommandToMethodType(ACommand: string): TMethodType;
begin
       if ACommand='ANY' then Result := TMethodType.mtAny;
       if ACommand='DELETE'then Result := TMethodType.mtDelete;
       if ACommand='GET' then Result := TMethodType.mtGet;
       if ACommand='HEAD' then Result := TMethodType.mtHead;
       if ACommand='PATCH' then Result := TMethodType.mtPatch;
       if ACommand='POST' then Result := TMethodType.mtPost;
       if ACommand='PUT' then Result := TMethodType.mtPut;

end;
{$ENDIF}

end.

