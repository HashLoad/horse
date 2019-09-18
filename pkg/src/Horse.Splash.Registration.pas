unit Horse.Splash.Registration;

interface

uses Winapi.Windows;

var
  bmSplashScreen: HBITMAP;

implementation

uses ToolsAPI, System.SysUtils, Vcl.Dialogs;

resourcestring
  resPackageName = 'Horse';
  resLicense = 'MIT License';

initialization
  bmSplashScreen := LoadBitmap(HInstance, 'HashloadSplash');
  (SplashScreenServices as IOTASplashScreenServices).AddPluginBitmap(resPackageName, bmSplashScreen, False, resLicense);

end.
