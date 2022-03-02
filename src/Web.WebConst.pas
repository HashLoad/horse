unit Web.WebConst;

interface

resourcestring
  sDuplicateActionName = 'Duplicate action name';
  sOnlyOneDispatcher = 'Only one WebDispatcher per form/data module';
  sHTTPItemName = 'Name';
  sHTTPItemURI = 'PathInfo';
  sHTTPItemEnabled = 'Enabled';
  sHTTPItemDefault = 'Default';
  sHTTPItemProducer = 'Producer';
  sHTTPItemMethod = 'Method';

  sResNotFound = 'Resource %s not found';

  sTooManyColumns = 'Too many table columns';
  sFieldNameColumn = 'Field Name';
  sFieldTypeColumn = 'Field Type';

  sInternalApplicationError =
    '<html lang="en" style="font-family: sans-serif; background-color: #F5F5F5">' +
    '<head>' +
    '  <meta charset="utf-8">' +
    '  <meta name="viewport" content="width=device-width, initial-scale=1.0">' +
    '  <title>Error 500: Internal Server Error</title>' +
    '</head>' +
    '<body' +
    '  style="display: grid; justify-content: center; height: 100vh; vertical-align: top; margin: 0; margin-top: 20px">' +
    '  <main>' +
    '    <h1 style="font-size: 40px; color: #424242; text-align:center; line-height: 0.25">5🔥🙀</h1>' +
    '    <p style="font-size: 24px; text-align: center; padding-left: 20px; padding-right: 20px;"><span style="color: #424242;">🐎 Internal Server Error 🐎</span><br><br><span style="color: #9e9e9e;">%0:s</span></p>'
    +
    '    <p style="font-size: 18px; text-align: center; padding-left: 20px; padding-right: 20px;"><span style="color: #bdbdbd;">%1:s</span></p>' +
    '  </main>' +
    '</body>' +
    '</html>';

  sWebFileExtensionItemExtensions = 'Extensions';
  sWebFileExtensionItemMimeType = 'Mime Type';
  sDuplicateMimeTypes = 'Duplicate mime types for extension: %s';
  sWebFileDirectoryItemMask = 'Directory Mask';
  sWebFileDirectoryItemAction = 'Action';
  sWebFileExtensionsItemDisplayName = 'Mime type: ''%0:s'', Extensions: ''%1:s''';
  sWebDirectoryInclude = 'Include';
  sWebDirectoryExclude = 'Exclude';
  sWebDirectoryItemDisplayName = 'Action: %0:s, Mask: ''%1:s''';

  {$IF (DEFINED(FPC) OR (CompilerVersion > 27.0))}
    sErrorDecodingURLText = 'Error decoding URL text';
    sInvalidURLEncodedChar = 'Invalid URL encoded char';
    sInvalidHTMLEncodedChar = 'Invalid HTML encoded char';
    sFactoryAlreadyRegistered = 'Factory already registered';
    sAppFactoryAlreadyRegistered = 'App factory already registered';
  {$IFEND}

implementation

end.
