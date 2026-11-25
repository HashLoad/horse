# FPC MIME type regression

`MimeTypeCheck.dpr` checks `THorseMimeTypes.GetExtType` and `GetFileType` on
the `THorseMimeTypesExt` path (FPC, and Delphi up to 10.2).

`GetExtType` receives an extension, with or without the leading dot, or a file
name. It used to run `ExtractFileExt` on the value it received. On FPC,
`ExtractFileExt('.css')` is `''` (a hidden file without extension), and
`ExtractFileExt('css')` is `''` on every compiler. Both came back as
`application/octet-stream`, so a static `.css` served by extension broke in
the browser. `GetFileType` keeps its previous result for file names, hidden
files and names without extension included.

Run from the repository root:

```text
sh tests/fpc-mime/run-mime-test.sh
```

Against the previous `src/Horse.Mime.pas`, five checks fail: `.css`, `.js`,
`css`, `.CSS` and ` .CSS `.
