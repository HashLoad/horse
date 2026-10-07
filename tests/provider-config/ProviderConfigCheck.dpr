program ProviderConfigCheck;

{$IFDEF FPC}{$MODE DELPHI}{$H+}{$ENDIF}

uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
  SysUtils, Horse, Horse.Provider.Config;

const
  Fields: array[0..9] of string = ('SSLEnabled', 'SSLCertFile', 'SSLKeyFile',
    'SSLKeyPassword', 'SSLCACertFile', 'SSLVerifyPeer', 'SSLCipherList',
    'SSLCipherSuitesTLS13', 'SSLMinVersion', 'SSLMinVersion');
var
  Config: THorseCrossSocketConfig;
  Index, OriginalPort: Integer;
  Rejected: Boolean;
begin
  ValidateNoUnsupportedTls(THorseCrossSocketConfig.Default, 'Default');
  for Index := Low(Fields) to High(Fields) do
  begin
    Config := THorseCrossSocketConfig.Default;
    case Index of
      0: Config.SSLEnabled := True;
      1: Config.SSLCertFile := 'certificate.pem';
      2: Config.SSLKeyFile := 'key.pem';
      3: Config.SSLKeyPassword := 'password';
      4: Config.SSLCACertFile := 'ca.pem';
      5: Config.SSLVerifyPeer := True;
      6: Config.SSLCipherList := 'HIGH';
      7: Config.SSLCipherSuitesTLS13 := 'TLS_AES_256_GCM_SHA384';
      8: Config.SSLMinVersion := htvTLS12;
      9: Config.SSLMinVersion := htvTLS13;
    end;
    OriginalPort := THorse.Port;
    Rejected := False;
    try
      THorse.ListenWithConfig(19132, Config);
    except
      on E: Exception do
      begin
        Rejected := Pos(Fields[Index], E.Message) > 0;
        if not Rejected then
          raise;
      end;
    end;
    if not Rejected then
      raise Exception.Create('Provider silently accepted ' + Fields[Index]);
    if OriginalPort <> THorse.Port then
      raise Exception.Create('Validation changed the listening port');
  end;
  Writeln('PASS: defaults accepted; 10 explicit TLS settings rejected before port mutation.');
end.
