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
  LLista: TJSONArray;
  LObjeto01: TJSONObject;
  LObjeto02: TJSONObject;
  LObjeto03: TJSONObject;
begin
  LLista := TJSONArray.Create;

  LObjeto01 := TJSONObject.Create;
  LObjeto01.AddPair(TJSONPair.Create('value', 'teste01'));
  LLista.AddElement(LObjeto01);

  LObjeto02 := TJSONObject.Create;
  LObjeto02.AddPair(TJSONPair.Create('value', 'teste02'));
  LLista.AddElement(LObjeto02);

  LObjeto03 := TJSONObject.Create;
  LObjeto03.AddPair(TJSONPair.Create('value', 'teste03'));
  LLista.AddElement(LObjeto03);

  Res.Send(LLista.ToString);
end;

procedure DoPostApi(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LValue: string;
  LRequest: TJSONObject;
  LResponse: TJSONObject;
begin
  LValue := '';
  LRequest := TJSONObject.ParseJSONValue(Req.Body) as TJSONObject;

  if (not LRequest.GetValue('value').Null) then
    LValue := LRequest.GetValue('value').value;

  LResponse := TJSONObject.Create;
  LResponse.AddPair(TJSONPair.Create('value', LValue));

  Res.Send(LResponse.ToString).Status(THTTPStatus.Created);
end;

procedure DoPutApi(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LValue: string;
  LRequest: TJSONObject;
  LResponse: TJSONObject;
begin
  LValue := '';
  LRequest := TJSONObject.ParseJSONValue(Req.Body) as TJSONObject;

  if (not LRequest.GetValue('value').Null) then
    LValue := LRequest.GetValue('value').value;

  LResponse := TJSONObject.Create;
  LResponse.AddPair(TJSONPair.Create('value', LValue));

  Res.Send(LResponse.ToString);
end;

procedure DoDeleteApi(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LValue: string;
  LResponse: TJSONObject;
begin
  LValue := Req.Params['id'];

  LResponse := TJSONObject.Create;
  LResponse.AddPair(TJSONPair.Create('value', LValue));

  Res.Send(LResponse.ToString);
end;

end.
