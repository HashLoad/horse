{$IFDEF FPC}
  {$IFDEF HORSE_FPC_FUNCTIONREFERENCES}
    // Delphi evaluates $IF expressions even inside skipped blocks, so FPC_FULLVERSION
    // cannot appear here; FPC's version symbols keep the check to DEFINED().
    {$IF DEFINED(VER1) OR DEFINED(VER2) OR DEFINED(VER3_0) OR DEFINED(VER3_2)}
      {$FATAL HORSE_FPC_FUNCTIONREFERENCES requires FPC 3.3.1 or newer}
    {$ENDIF}
    {$MODESWITCH FUNCTIONREFERENCES}
    {$MODESWITCH ANONYMOUSFUNCTIONS}
  {$ENDIF}
{$ENDIF}
