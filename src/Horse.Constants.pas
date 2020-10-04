unit Horse.Constants;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

interface

const
  DEFAULT_HOST = '0.0.0.0';
  DEFAULT_PORT = 9000;
  HORSE_ENV = 'HORSE_ENV';
  ENV_D = 'd';
  ENV_DEV = 'dev';
  ENV_DEVELOPMENT = 'development';
  START_RUNNING = 'Server is runing on %s:%d';

implementation

end.
