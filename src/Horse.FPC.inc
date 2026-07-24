{$IF FPC_FULLVERSION >= 30301}
  // FPC 3.3.1+ (trunk) rtl-generics declares IEqualityComparer<T> with
  // Delphi-compatible const parameters on EVERY platform (not just Win64).
  // Upstream's guard only checks CPU64+WINDOWS, so on FPC-trunk Linux it
  // picks constref and fails: "No matching implementation for interface
  // method Equals(const...)". (Upstream-PR candidate.)
  {$DEFINE CONST_GENERIC := const}
{$ELSEIF DEFINED(CPU64) AND DEFINED(WINDOWS)}
  {$DEFINE CONST_GENERIC := const}
{$ELSE}
  {$DEFINE CONST_GENERIC := constref}
{$ENDIF}
