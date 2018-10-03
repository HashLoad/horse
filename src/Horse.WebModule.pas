unit Horse.WebModule;

interface

uses System.SysUtils, System.IOUtils, System.Classes, Web.HTTPApp, Horse,
  System.RegularExpressions;

type
  THorseResultRegex = record
    Path: string;
    Sucess: Boolean;
    constructor Create(APath: string; ASucess: Boolean);
  end;

  THorseQueueMiddlewares = class
  private
    FMiddlewares: THorseMiddlewares;
    FRequest: THorseRequest;
    FResponse: THorseResponse;
  public
    procedure Next;
    constructor Create(AMiddlewares: THorseMiddlewares; ARequest: THorseRequest;
      AResponse: THorseResponse);
  end;

  THorseWebModule = class(TWebModule)
    procedure HandlerAction(Sender: TObject; Request: TWebRequest;
      Response: TWebResponse; var Handled: Boolean);
  private
    FHorse: THorse;
    function ValidateRegex(APath: string; ARequest: THorseRequest;
      AResponse: THorseResponse): THorseResultRegex;
    function GenerateExpression(APath: string): string;
  public
    property Horse: THorse read FHorse write FHorse;
    constructor Create(AOwner: TComponent); override;
  end;

var
  WebModuleClass: TComponentClass = THorseWebModule;

implementation

{%CLASSGROUP 'System.Classes.TPersistent'}
{$R *.dfm}

constructor THorseWebModule.Create(AOwner: TComponent);
begin
  inherited;
  FHorse := THorse.GetInstance;
end;

function THorseWebModule.GenerateExpression(APath: string): string;
var
  LIdentifier: string;
  LIdentifiers: TArray<string>;
begin
  LIdentifiers := APath.Split(['/']);
  APath := APath.Replace('/', '\/');

  for LIdentifier in LIdentifiers do
  begin
    if LIdentifier.StartsWith(':') then
      APath := APath.Replace(LIdentifier, '(.*)');
  end;

  APath := '^' + APath;
  APath := APath + '$';

  Result := APath;
end;

procedure THorseWebModule.HandlerAction(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var
  LIdentifiers: TArray<string>;
  LIdentifier, LPath: string;
  LMiddleware: THorseMiddleware;
  LMiddlewares, LPreparedMiddlewares: THorseMiddlewares;
  LQueueMiddlewares: THorseQueueMiddlewares;
  LRequest: THorseRequest;
  LResponse: THorseResponse;
  LResultRegex: THorseResultRegex;
begin
  LRequest := THorseRequest.Create(Request);
  LResponse := THorseResponse.Create(Response);

  LResultRegex := ValidateRegex(Request.PathInfo, LRequest, LResponse);
  if LResultRegex.Sucess then
  begin
    LPreparedMiddlewares := THorseMiddlewares.Create;
    try
      LIdentifiers := LResultRegex.Path.Split(['/']);

      for LIdentifier in LIdentifiers do
      begin
        LPath := LPath + LIdentifier;

        if FHorse.Routes.TryGetValue(LPath, LMiddlewares) then
        begin

          for LMiddleware in LMiddlewares do
          begin
            if (LMiddleware.MethodType = mtAny) or
              (LMiddleware.MethodType = Request.MethodType) then
              LPreparedMiddlewares.Enqueue(LMiddleware);
          end;
        end;

        LPath := LPath + '/';
      end;

      LQueueMiddlewares := THorseQueueMiddlewares.Create(LPreparedMiddlewares,
        LRequest, LResponse);
      try
        LQueueMiddlewares.Next;
      finally
        LQueueMiddlewares.Free;
      end;

    finally
      LPreparedMiddlewares.Free;
    end;
  end
  else
  begin
    Response.Content := 'Not Found';
    Response.StatusCode := 404;
  end;
end;

function THorseWebModule.ValidateRegex(APath: string; ARequest: THorseRequest;
  AResponse: THorseResponse): THorseResultRegex;
var
  LMatch: TMatch;
  LRegex: TRegEx;
  LPath: string;
  LIdentifier: string;
  LIdentifiers: TArray<string>;
  LCount: Integer;
begin
  LCount := 1;

  if not APath.StartsWith('/') then
    APath := '/' + APath;

  if APath.EndsWith('/') then
    APath := APath.Remove(High(APath) - 1, 1);

  for LPath in FHorse.Routes.Keys do
  begin
    LRegex.Create(GenerateExpression(LPath));
    LMatch := LRegex.Match(APath);
    Result.Sucess := LMatch.Success;
    if Result.Sucess then
    begin
      Result.Path := LPath;
      LIdentifiers := LPath.Split(['/']);
      for LIdentifier in LIdentifiers do
      begin
        if LIdentifier.StartsWith(':') then
        begin
          THorseHackRequest(ARequest)
            .GetParams.Add(LIdentifier.Replace(':', ''),
            LMatch.Groups.Item[LCount].Value);
          Inc(LCount);
        end;
      end;
      Break;
    end;
  end;
end;

{ THorseResultRegex }

constructor THorseResultRegex.Create(APath: string; ASucess: Boolean);
begin
  Path := APath;
  Sucess := ASucess;
end;

{ THorseQueueMidleware }

constructor THorseQueueMiddlewares.Create(AMiddlewares: THorseMiddlewares;
  ARequest: THorseRequest; AResponse: THorseResponse);
begin
  FMiddlewares := AMiddlewares;
  FRequest := ARequest;
  FResponse := AResponse;
end;

procedure THorseQueueMiddlewares.Next;
begin
  try
    if FMiddlewares.Count > 0 then
      FMiddlewares.Dequeue.Execute(FRequest, FResponse, Next);
  except
    on E: EHorseCallbackInterrupted do
    else
      raise;
  end;
end;

end.
