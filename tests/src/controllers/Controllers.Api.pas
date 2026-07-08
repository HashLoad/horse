unit Controllers.Api;

interface

uses
  Horse;

procedure Registry;
procedure DoGetApi(Req: THorseRequest; Res: THorseResponse; Next: TProc);
procedure DoPostApi(Req: THorseRequest; Res: THorseResponse; Next: TProc);
procedure DoPutApi(Req: THorseRequest; Res: THorseResponse; Next: TProc);
procedure DoDeleteApi(Req: THorseRequest; Res: THorseResponse; Next: TProc);
procedure DoPatchApi(Req: THorseRequest; Res: THorseResponse; Next: TProc);
procedure DoHeadApi(Req: THorseRequest; Res: THorseResponse; Next: TProc);

implementation

uses
  System.JSON,
  Horse.Commons;

procedure MiddlewareCORS(Req: THorseRequest; Res: THorseResponse; Next: TProc);
begin
  Res.AddHeader('Access-Control-Allow-Origin', '*');
  Res.AddHeader('Access-Control-Allow-Headers', 'Content-Type, Authorization');
  Res.AddHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS');
  if Req.Method = 'OPTIONS' then
  begin
    Res.Status(THTTPStatus.NoContent).Send('').Abort;
    Exit;
  end;
  Next;
end;

procedure MiddlewareAuth(Req: THorseRequest; Res: THorseResponse; Next: TProc);
begin
  if Req.Headers['Authorization'] <> 'Bearer MySecretToken' then
  begin
    Res.Status(THTTPStatus.Unauthorized).Send('Unauthorized').Abort;
    Exit;
  end;
  Next;
end;

procedure Registry;
var
  LAuthMiddleware: THorseCallback;
begin
  THorse.Use(MiddlewareCORS);

  LAuthMiddleware := MiddlewareAuth;

  THorse.Route('/Api/Protected')
    .AddCallback(LAuthMiddleware)
    .Get(procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
         begin
           Res.Send('SecretData');
         end);

  THorse.Route('/Api/Cors')
    .Get(procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
         begin
           Res.Send('CorsData');
         end)
    .Post(procedure(Req: THorseRequest; Res: THorseResponse; Next: TProc)
          begin
            Res.Send('CorsData');
          end);

  THorse
    .Group
      .Prefix('/Api')
        .Delete('/Test/:id', DoDeleteApi)
        .Route('/Test')
          .Get(DoGetApi)
          .Post(DoPostApi)
          .Put(DoPutApi)
          .Patch(DoPatchApi)
          .Head(DoHeadApi)
        .&End;
end;

procedure DoGetApi(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LList: TJSONArray;
  LObject01: TJSONObject;
  LObject02: TJSONObject;
  LObject03: TJSONObject;
begin
  LList := TJSONArray.Create;
  try
    LObject01 := TJSONObject.Create;
    LObject01.AddPair(TJSONPair.Create('value', 'test01'));
    LList.AddElement(LObject01);

    LObject02 := TJSONObject.Create;
    LObject02.AddPair(TJSONPair.Create('value', 'test02'));
    LList.AddElement(LObject02);

    LObject03 := TJSONObject.Create;
    LObject03.AddPair(TJSONPair.Create('value', 'test03'));
    LList.AddElement(LObject03);

    Res.Send(LList.ToString);
  finally
    LList.Free;
  end;
end;

procedure DoPostApi(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LValue: string;
  LRequest: TJSONObject;
  LResponse: TJSONObject;
begin
  LValue := '';
  LRequest := TJSONObject.ParseJSONValue(Req.Body) as TJSONObject;
  try
    if (not LRequest.GetValue('value').Null) then
      LValue := LRequest.GetValue('value').value;

    LResponse := TJSONObject.Create;
    try
      LResponse.AddPair(TJSONPair.Create('value', LValue));

      Res.Send(LResponse.ToString).Status(THTTPStatus.Created);
    finally
      LResponse.Free;
    end;
  finally
    LRequest.Free;
  end;
end;

procedure DoPutApi(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LValue: string;
  LRequest: TJSONObject;
  LResponse: TJSONObject;
begin
  LValue := '';
  LRequest := TJSONObject.ParseJSONValue(Req.Body) as TJSONObject;
  try
    if (not LRequest.GetValue('value').Null) then
      LValue := LRequest.GetValue('value').value;

    LResponse := TJSONObject.Create;
    try
      LResponse.AddPair(TJSONPair.Create('value', LValue));

      Res.Send(LResponse.ToString);
    finally
      LResponse.Free;
    end;
  finally
    LRequest.Free;
  end;
end;

procedure DoDeleteApi(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LValue: string;
  LResponse: TJSONObject;
begin
  LValue := Req.Params['id'];

  LResponse := TJSONObject.Create;
  try
    LResponse.AddPair(TJSONPair.Create('value', LValue));

    Res.Send(LResponse.ToString);
  finally
    LResponse.Free;
  end;
end;

procedure DoPatchApi(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LValue: string;
  LRequest: TJSONObject;
  LResponse: TJSONObject;
begin
  LValue := '';
  LRequest := TJSONObject.ParseJSONValue(Req.Body) as TJSONObject;
  try
    if (not LRequest.GetValue('value').Null) then
      LValue := LRequest.GetValue('value').value;

    LResponse := TJSONObject.Create;
    try
      LResponse.AddPair(TJSONPair.Create('value', LValue));

      Res.Send(LResponse.ToString);
    finally
      LResponse.Free;
    end;
  finally
    LRequest.Free;
  end;
end;

procedure DoHeadApi(Req: THorseRequest; Res: THorseResponse; Next: TProc);
begin
  Res.Status(THTTPStatus.NoContent);
end;

end.
