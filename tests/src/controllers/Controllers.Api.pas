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
  lista: TJSONArray;
  objeto01: TJSONObject;
  objeto02: TJSONObject;
  objeto03: TJSONObject;
begin
  lista := TJSONArray.Create;

  objeto01 := TJSONObject.Create;
  objeto01.AddPair(TJSONPair.Create('value', 'teste01'));
  lista.AddElement(objeto01);

  objeto02 := TJSONObject.Create;
  objeto02.AddPair(TJSONPair.Create('value', 'teste02'));
  lista.AddElement(objeto02);

  objeto03 := TJSONObject.Create;
  objeto02.AddPair(TJSONPair.Create('value', 'teste03'));
  lista.AddElement(objeto03);

  Res.Send(lista.ToString);
end;

procedure DoPostApi(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  value: string;
  request: TJSONObject;
  response: TJSONObject;
begin
  value := '';
  request := TJSONObject.ParseJSONValue(Req.Body) as TJSONObject;

  if (not request.GetValue('value').Null) then
    value := request.GetValue('value').value;

  response := TJSONObject.Create;
  response.AddPair(TJSONPair.Create('value', value));

  Res.Send(response.ToString).Status(THTTPStatus.Created);
end;

procedure DoPutApi(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  value: string;
  request: TJSONObject;
  response: TJSONObject;
begin
  value := '';
  request := TJSONObject.ParseJSONValue(Req.Body) as TJSONObject;

  if (not request.GetValue('value').Null) then
    value := request.GetValue('value').value;

  response := TJSONObject.Create;
  response.AddPair(TJSONPair.Create('value', value));

  Res.Send(response.ToString);
end;

procedure DoDeleteApi(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  value: string;
  response: TJSONObject;
begin
  value := Req.Params['id'];

  response := TJSONObject.Create;
  response.AddPair(TJSONPair.Create('value', value));

  Res.Send(response.ToString);
end;

end.
