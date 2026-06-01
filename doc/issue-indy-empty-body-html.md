# Bug: `Res.Send('')` with Indy returns HTML body instead of empty response

## Summary

When a route handler calls `Res.Send('')` (or `Res.Send(Req.Body)` where the
request body is empty), Horse running under the Indy/Console provider responds
with an unexpected HTML body and silently overrides the `Content-Type` header.

**Expected:** `200 OK`, `Content-Type: text/plain`, empty body (0 bytes)  
**Actual:** `200 OK`, `Content-Type: text/html`, body = `<HTML><BODY><B>200 OK</B></BODY></HTML>`

---

## Minimal reproduction

```pascal
THorse.Post('/echo',
  procedure(Req: THorseRequest; Res: THorseResponse)
  begin
    // Echoes request body — returns empty string when request has no body
    Res.ContentType('text/plain').Send(Req.Body);
  end
);
THorse.Listen(9000);
```

```
curl -s -D - -X POST http://127.0.0.1:9000/echo
```

**Response (actual):**
```
HTTP/1.1 200 OK
Content-Type: text/html
Content-Length: 39

<HTML><BODY><B>200 OK</B></BODY></HTML>
```

**Response (expected):**
```
HTTP/1.1 200 OK
Content-Type: text/plain
Content-Length: 0

```

---

## Root cause

`THorseResponse.Send(const AContent: string)` delegates to
`FWebResponse.Content := AContent` (`Web.HTTPApp.TWebResponse`).  When
`AContent = ''`, Indy's `TIdHTTPResponseInfo.WriteContent` detects that both
`ContentText` and `ContentStream` are empty/nil and executes its HTML fallback:

```pascal
// Inside TIdHTTPResponseInfo.WriteContent (Indy source)
if ContentStream <> nil then
  FConnection.IOHandler.Write(ContentStream, 0, True)
else if Length(ContentText) > 0 then
  FConnection.IOHandler.Write(ContentText, Charset, False)
else
begin
  // Fallback — overwrites whatever ContentType the application set
  ContentType := 'text/html';
  ContentText := '<HTML><BODY><B>' + IntToStr(ResponseCode) + ' ' +
                 ResponseText + '</B></BODY></HTML>';
  FConnection.IOHandler.Write(ContentText, Charset, False);
end;
```

Two problems occur simultaneously:

1. **Unexpected body**: A body with HTML content is sent where 0 bytes were
   intended.
2. **Silent `Content-Type` override**: `ContentType` is overwritten with
   `'text/html'` regardless of what the application had already set (e.g.
   `'application/json'`, `'text/plain'`).

This fallback originates from Indy's history as a browser-facing HTTP server,
where an empty `200 OK` with no body is unusual.  For a REST API, both effects
are always wrong.

---

## Impact on REST API applications

| Concern | Detail |
|---|---|
| **API contract violation** | Clients expecting an empty body (e.g. empty-string JSON field echoes, health-check endpoints) receive 39 bytes of HTML instead. |
| **Content-Type mismatch** | JSON or plain-text clients receive `text/html`. Strict parsers reject the response; lenient parsers silently misinterpret it. |
| **Broken `Content-Length`** | Proxy servers and HTTP/2 multiplexers rely on accurate `Content-Length`. The substituted body changes the byte count from 0 to 39 without the application's knowledge. |
| **Hidden in non-empty paths** | The bug only manifests when the body is exactly empty. Routes that always produce content are unaffected, so the bug can reach production silently. |

---

## Proposed fix — `Horse.Response.pas`

When `AContent = ''` on the Indy path, assign an empty `TMemoryStream` to
`FWebResponse.ContentStream` instead of setting `ContentText := ''`.  Indy
takes the `ContentStream` branch in `WriteContent`, writes 0 bytes, and never
reaches the HTML fallback.  `Content-Type` is preserved exactly as the
application set it.

`TIdHTTPResponseInfo.FreeContentStream` defaults to `True`, so Indy owns and
frees the stream — no memory leak.

```pascal
function THorseResponse.Send(const AContent: string): THorseResponse;
begin
  if not Assigned(FWebResponse) then       // CrossSocket / no-provider path
  begin
    FCSBody := AContent;
    Exit(Self);
  end;

{$IF NOT DEFINED(FPC)}
  // [FIX] When ContentText = '' and ContentStream = nil, Indy's WriteContent
  // substitutes an HTML body and overrides ContentType to 'text/html'.
  // Assigning an empty TMemoryStream forces the stream path (0 bytes written)
  // and preserves the application-set ContentType.
  // FreeContentStream defaults to True — Indy frees the stream.
  if AContent = '' then
  begin
    FWebResponse.ContentStream := TMemoryStream.Create;
    FWebResponse.ContentLength := 0;
    Exit(Self);
  end;
{$ENDIF}

  FWebResponse.Content := AContent;
  Result := Self;
end;
```

### Why not the FPC path?

FPC Horse providers (HTTPApplication, FastCGI, LCL) use `TRequest`/`TResponse`
from `fpHTTP`/`HTTPDefs`, not Indy's `TIdHTTPResponseInfo`.  They do not
exhibit this HTML substitution behaviour, so no change is needed on the FPC
path.

### Why not change `TMemoryStream` lifetime management?

`SendFile` and `Download` already set `FWebResponse.FreeContentStream := False`
when passing caller-owned streams to Indy.  The empty `TMemoryStream` created
here is not caller-owned — it is an implementation detail whose sole purpose is
to route Indy away from the HTML fallback.  Leaving `FreeContentStream` at its
default `True` is correct: Indy owns and frees the stream, no bookkeeping
required on the Horse side.

---

## Affected versions

Any version of Horse that uses the Indy/Console provider
(`Horse.Provider.Console`) and calls `Res.Send('')` or equivalent.  Not
reproducible with the CrossSocket provider (`HORSE_PROVIDER_CROSSSOCKET`)
nor with the mORMot2 provider (`HORSE_PROVIDER_MORMOT`) — both write the
response body directly, bypassing Indy's `TIdHTTPResponseInfo.WriteContent`
substitution path entirely. Switching to either async provider is a valid
workaround as well as a fix for the underlying bug.

---

## Related

- [`TIdHTTPResponseInfo.WriteContent`](https://github.com/IndySockets/Indy) —
  the Indy source method that performs the HTML substitution.
- Delphi `Web.HTTPApp.TWebResponse.ContentStream` — the property used by the
  fix to bypass the fallback.
