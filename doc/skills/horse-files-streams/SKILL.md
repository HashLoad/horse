---
name: horse-files-streams
description: Guide to handling file uploads (multipart), downloads, and stream lifetime management in the Horse framework.
---

# Horse Files & Streams

## 1. File Uploads (Multipart FormData)
When clients upload files via multipart form data, you can retrieve the file as a stream or save it directly to the disk using the `THorseCoreParamField` helper from `Req.ContentFields`:

```pascal
procedure UploadHandler(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LFileField: THorseCoreParamField;
  LStream: TStream;
begin
  // 1. Get the file field
  LFileField := Req.ContentFields.Field('file');
  
  if not Assigned(LFileField) then
    raise EHorseException.New
      .Status(THTTPStatus.BadRequest)
      .Error('No file was uploaded');

  // 2. Save it directly to disk
  LFileField.SaveToFile('C:\uploads\' + LFileField.FieldName);
  
  // 3. Or access the raw TStream
  LStream := LFileField.AsStream;
  // Note: DO NOT free LStream if it is owned by the request lifecycle.
  
  Res.Status(THTTPStatus.OK).Send('File uploaded successfully');
end;
```

---

## 2. File Downloads & Streaming
To send files or custom data streams to the client, use `Res.SendFile` or `Res.Download`.

* **`SendFile`**: Sends the file stream to the client (displays inline in the browser if supported).
* **`Download`**: Forces the browser to download the file with the specified filename.

```pascal
procedure DownloadHandler(Req: THorseRequest; Res: THorseResponse; Next: TProc);
var
  LStream: TStream;
begin
  LStream := TFileStream.Create('C:\data\report.pdf', fmOpenRead or fmShareDenyWrite);
  
  // Sends the stream and instructs the client to download as "report_2026.pdf"
  Res.Status(THTTPStatus.OK).Download(LStream, 'report_2026.pdf');
  
  // CRITICAL MEMORY RULE: DO NOT FREE LStream here.
end;
```

---

## 3. Critical Memory Rule (Stream Ownership)
The `THorseResponse` object **takes ownership** of any stream passed into `SendFile`, `Download`, or `Render`. 

* **Rule**: **NEVER** call `.Free` or `FreeAndNil` on a stream after passing it to `Res.SendFile`, `Res.Download`, or `Res.Render`.
* **Reason**: The framework will automatically destroy the stream object once the HTTP response is written to the socket. Freeing it manually in your handler will cause a Double-Free memory corruption (Access Violation) when the response lifecycle terminates.
