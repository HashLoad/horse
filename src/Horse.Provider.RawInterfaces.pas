unit Horse.Provider.RawInterfaces;

{
  Horse Provider Raw Interfaces
  =============================
  Lightweight interfaces that capture the subset of TWebRequest / TWebResponse
  functionality that Horse middleware actually uses.

  New providers implement these (~10 methods for request, ~1 for response)
  instead of stubbing 30+ abstract methods on TWebRequest / TWebResponse.

  The generic adapter classes in Horse.Provider.RawAdapters wrap an
  IHorseRawRequest / IHorseRawResponse into a real TWebRequest / TWebResponse
  subclass, giving middleware full backward compatibility.

  Dual-compilation: interfaces are identical in Delphi and FPC.

  Compiler-version guard: GetContentLength returns Int64 on Delphi 10.2+
  (where TWebRequest.GetIntegerVariable changed from Integer to Int64)
  and Integer on older Delphi and FPC.
}

{$IF DEFINED(FPC)}
{$MODE DELPHI}{$H+}
{$ENDIF}

interface

uses
{$IF DEFINED(FPC)}
  Classes;
{$ELSE}
  System.Classes;
{$ENDIF}

type
  { IHorseRawRequest — the request surface that middleware actually reads.

    Covers: Method, PathInfo, QueryString, Host, RemoteAddr, ContentType,
    Content (body as string), arbitrary header lookup, and the three
    name=value collections (query, content fields, cookies).

    A provider implements this by wrapping its native request object
    (ICrossHttpRequest, TIdHTTPRequestInfo, etc.) in ~10 one-liner methods. }

  IHorseRawRequest = interface
    ['{A7E3B4C1-5D2F-4A8E-9C6B-1F0D3E2A4B5C}']
    function GetMethod: string;
    function GetProtocolVersion: string;
    function GetURL: string;
    function GetPathInfo: string;
    function GetQueryString: string;
    function GetHost: string;
    function GetRemoteAddr: string;
    function GetServerPort: Integer;
    function GetContentType: string;
    function GetContent: string;
{$IF DEFINED(FPC)}
    function GetContentLength: Integer;
{$ELSEIF CompilerVersion >= 32.0}  // Delphi 10.2 Tokyo+
    function GetContentLength: Int64;
{$ELSE}
    function GetContentLength: Integer;
{$IFEND}
    function GetFieldByName(const AName: string): string;

    { Populate the inherited TStrings collections in TWebRequest.
      Called once by the adapter constructor. }
    procedure PopulateQueryFields(ADest: TStrings);
    procedure PopulateContentFields(ADest: TStrings);
    procedure PopulateCookieFields(ADest: TStrings);

    { Streaming body access — used by ReadClient / ReadString.
      Returns the number of bytes actually read. }
    function ReadBody(var Buffer; Count: Integer): Integer;
  end;

  { IHorseRawResponse — the response surface that middleware actually writes.

    The ONLY method middleware calls on RawWebResponse is SetCustomHeader.
    This interface captures that single operation.

    SetCustomHeader is inherited from TWebResponse and writes to the
    inherited CustomHeaders TStrings — no override needed in the adapter.
    This interface exists so providers that want to intercept header writes
    (e.g. for real-time forwarding) can do so. }

  IHorseRawResponse = interface
    ['{B8F4C5D2-6E3A-4B9F-0D7C-2A1E4F3B5C6D}']
    procedure SetCustomHeader(const AName, AValue: string);
  end;

implementation

end.
