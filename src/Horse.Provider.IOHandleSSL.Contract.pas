unit Horse.Provider.IOHandleSSL.Contract;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
  IdSSLOpenSSL;

type
  IHorseProviderIOHandleSSL = interface
    ['{E24EB539-B1B9-4E27-88AA-238A3F47BC11}']
    function Active: Boolean; overload;
    function Active(const AValue: Boolean): IHorseProviderIOHandleSSL; overload;
    function CertFile: string; overload;
    function CertFile(const AValue: string): IHorseProviderIOHandleSSL; overload;
    function RootCertFile: string; overload;
    function RootCertFile(const AValue: string): IHorseProviderIOHandleSSL; overload;
    function KeyFile: string; overload;
    function KeyFile(const AValue: string): IHorseProviderIOHandleSSL; overload;
    function Method: TIdSSLVersion; overload;
    function Method(const AValue: TIdSSLVersion): IHorseProviderIOHandleSSL; overload;
    function SSLVersions: TIdSSLVersions; overload;
    function SSLVersions(const AValue: TIdSSLVersions): IHorseProviderIOHandleSSL; overload;
    function DHParamsFile: string; overload;
    function DHParamsFile(const AValue: string): IHorseProviderIOHandleSSL; overload;
    function CipherList: string; overload;
    function CipherList(const AValue: string): IHorseProviderIOHandleSSL; overload;
    function OnGetPassword: TPasswordEvent; overload;
    function OnGetPassword(const AValue: TPasswordEvent): IHorseProviderIOHandleSSL; overload;
  end;

implementation

end.
