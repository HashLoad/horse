library FpcApacheBaseline;

{$MODE DELPHI}{$H+}{$MODESWITCH CVAR}

// Control fixture: deliberately does not import any Horse unit.
uses
{$IFDEF BASELINE_CMEM}
  cmem,
{$ENDIF}
  cthreads, Classes, httpdefs, fpHTTP, fpWeb, httpd24, fpApache24, custapache24;

type
  TBaselineModule = class(TFPWebModule)
    procedure HandleRequest(ARequest: TRequest; AResponse: TResponse); override;
  end;
  TBaselineHost = class
    procedure GetModule(Sender: TObject; ARequest: TRequest;
      var ModuleClass: TCustomHTTPModuleClass);
  end;

var
  ApacheModuleData: module; public name 'horse_hosted_module';
  Host: TBaselineHost;

exports ApacheModuleData name 'horse_hosted_module';

procedure TBaselineModule.HandleRequest(ARequest: TRequest; AResponse: TResponse);
begin
  AResponse.Content := 'baseline';
end;

procedure TBaselineHost.GetModule(Sender: TObject; ARequest: TRequest;
  var ModuleClass: TCustomHTTPModuleClass);
begin
  ModuleClass := TBaselineModule;
end;

begin
  Host := TBaselineHost.Create;
  Application.ModuleName := 'horse_hosted_module';
  Application.HandlerName := 'horse-hosted-handler';
  Application.SetModuleRecord(ApacheModuleData);
  Application.AllowDefaultModule := True;
  Application.LegacyRouting := True;
  Application.OnGetModule := Host.GetModule;
  Application.Initialize;
end.
