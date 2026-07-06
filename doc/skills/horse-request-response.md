---
name: horse-request-response
description: Guide to interacting with THorseRequest (body, query, params, headers) and THorseResponse (Send, Status, ContentType).
---

# Horse Request & Response

## THorseRequest (Reading Request Data)
The `THorseRequest` object contains all details of the incoming HTTP request.

*   **Body**: To read the raw text payload, use `Req.Body`. If the `Jhonson` middleware is active, you can read the parsed JSON directly:
    ```pascal
    var
      LBody: TJSONObject;
    begin
      LBody := Req.Body<TJSONObject>;
    end;
    ```
*   **Params**: For path parameters (defined with `:name`), use `Req.Params.Items['name']` (or `Req.Params['name']`).
*   **Query**: For query parameters (e.g., `?page=1&limit=10`), use `Req.Query.Items['page']` (or `Req.Query['page']`).
*   **Headers**: Read request headers using `Req.Headers.Items['Authorization']`.

---

## Safe Parameter Parsing & Validation (THorseCoreParamField)
Instead of accessing raw string dictionary values (e.g., `Req.Params['id']`) and converting them manually, always prefer using the `.Field()` method to obtain a `THorseCoreParamField`. This provides type-safe conversions and declarative parameter validation:

*   **Type Conversion**: Convert parameters to the target type without manually calling `StrToInt` or `StrToBool`:
    ```pascal
    var
      LId: Integer;
      LActive: Boolean;
      LDate: TDateTime;
    begin
      LId := Req.Params.Field('id').AsInteger;
      LActive := Req.Query.Field('active').AsBoolean;
      LDate := Req.Query.Field('since').AsISO8601DateTime;
    end;
    ```
*   **Required Check**: Automatically halt execution and return a 400 Bad Request with a custom error message if the parameter is missing:
    ```pascal
    var
      LEmail: string;
    begin
      // Automatically raises EHorseException if 'email' query param is empty
      LEmail := Req.Query.Field('email').Required.RequiredMessage('The "email" parameter is required.').AsString;
    end;
    ```
*   **Supported Converters**: `AsInteger`, `AsInt64`, `AsBoolean`, `AsFloat`, `AsCurrency`, `AsDateTime`, `AsISO8601DateTime`, `AsStream` (for multipart uploads), and `AsString`.

---

## Cookies Management
Horse provides a provider-agnostic, typed API to read cookies from requests and write them back in responses (RFC 6265 compliant).

*   **Reading Cookies**: Read incoming cookies safely using the `.Field()` helper:
    ```pascal
    var
      LSessionToken: string;
    begin
      LSessionToken := Req.Cookie.Field('session_token').AsString;
    end;
    ```

*   **Writing Cookies (Set-Cookie)**: Write response cookies using `Res.Cookie` and configure properties fluently:
    ```pascal
    uses Horse.Core.Cookie;

    procedure SetCookieHandler(Req: THorseRequest; Res: THorseResponse; Next: TProc);
    begin
      // Creates, registers, and configures the cookie fluently (XE7 compatible)
      Res.Cookie('session_id', 'xyz789')
        .Path('/')
        .HttpOnly(True)
        .Secure(True)
        .SameSite(TSameSite.ssLax);

      Res.Send('Cookie has been set');
    end;
    ```

---

## THorseResponse (Sending HTTP Responses)
The `THorseResponse` object is used to build and send the HTTP response back to the client.

*   **Send**: Returns text or objects. It supports method chaining:
    ```pascal
    // Sending simple text with Status 200 (OK)
    Res.Send('Success');
    
    // Setting Status 201 (Created) and sending an object (MUST set status BEFORE calling Send)
    Res.Status(THTTPStatus.Created).Send<TJSONObject>(LJson);
    ```
*   **Status**: Set the HTTP status code using integer values or the `THTTPStatus` enum:
    ```pascal
    Res.Status(400); // Bad Request
    Res.Status(THTTPStatus.NoContent);
    ```
*   **ContentType**: Set custom content type headers if you are not returning standard JSON:
    ```pascal
    Res.ContentType('text/html').Send('<h1>HTML Content</h1>');
    ```

---

## Structured Error Handling (EHorseException)
To return error responses with custom HTTP statuses and error details, raise `EHorseException`. The framework captures this exception and formats it as a structured JSON error response:

```pascal
uses Horse.Exception, Horse.Commons;

procedure GetProduct(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LId: Integer;
  LProduct: TProduct;
begin
  LId := Req.Params.Field('id').AsInteger;
  if LId <= 0 then
    raise EHorseException.New
      .Status(THTTPStatus.BadRequest)
      .Error('Invalid product ID');

  LProduct := FindProduct(LId);
  if not Assigned(LProduct) then
    raise EHorseException.New
      .Status(THTTPStatus.NotFound)
      .Error('Product not found')
      .Code(4041)
      .Detail('The requested product does not exist in our catalog.');

  Res.Send(LProduct);
end;
```
When `EHorseException` is raised, it automatically serializes to a clean JSON response containing fields like `error`, `code`, and `detail`.
