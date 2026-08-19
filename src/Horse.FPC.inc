{$IF FPC_FULLVERSION >= 30301}
  // FPC 3.3.1+ (trunk) rtl-generics declares IEqualityComparer<T> with
  // Delphi-compatible const parameters on every platform.
  {$DEFINE CONST_GENERIC := const}
{$ELSE}
  // The official FPC 3.2.x rtl-generics declares IEqualityComparer<T> with
  // constref on Win32, Win64 and Unix targets. Using const breaks the
  // specialized interface implementation (#542).
  {$DEFINE CONST_GENERIC := constref}
{$ENDIF}
