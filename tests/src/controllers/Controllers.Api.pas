unit Controllers.Api;

interface

uses
  Horse, System.JSON, Horse.Commons;

procedure Registry;
procedure DoGetApi(Req: THorseRequest; Res: THorseResponse; Next: TProc);
procedure DoPostApi(Req: THorseRequest; Res: THorseResponse; Next: TProc);
procedure DoPutApi(Req: THorseRequest; Res: THorseResponse; Next: TProc);
procedure DoDeleteApi(Req: THorseRequest; Res: THorseResponse; Next: TProc);

implementation

procedure Registry;
begin
  THorse
    .Group
      .Prefix('/Api')
        .Delete('/Test/:id', DoDeleteApi)
        .Route('/Test')
          .Get(DoGetApi)
          .Post(DoPostApi)
          .Put(DoPutApi)
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

end.
