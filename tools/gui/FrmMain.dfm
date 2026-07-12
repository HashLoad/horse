object FrmMain: TFrmMain
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Compilador gRPC Protobuf para Horse'
  ClientHeight = 450
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 15
  object lblInput: TLabel
    Left = 16
    Top = 16
    Width = 117
    Height = 15
    Caption = 'Diretorio dos arquivos .proto:'
  end
  object lblOutput: TLabel
    Left = 16
    Top = 76
    Width = 115
    Height = 15
    Caption = 'Diretorio de destino (.pas):'
  end
  object edtInput: TEdit
    Left = 16
    Top = 37
    Width = 473
    Height = 23
    TabOrder = 0
  end
  object btnBrowseInput: TButton
    Left = 497
    Top = 36
    Width = 87
    Height = 25
    Caption = 'Procurar...'
    TabOrder = 1
    OnClick = btnBrowseInputClick
  end
  object edtOutput: TEdit
    Left = 16
    Top = 97
    Width = 473
    Height = 23
    TabOrder = 2
  end
  object btnBrowseOutput: TButton
    Left = 497
    Top = 96
    Width = 87
    Height = 25
    Caption = 'Procurar...'
    TabOrder = 3
    OnClick = btnBrowseOutputClick
  end
  object chkRecursive: TCheckBox
    Left = 16
    Top = 136
    Width = 180
    Height = 17
    Caption = 'Incluir subpastas de forma recursiva'
    TabOrder = 4
  end
  object btnCompile: TButton
    Left = 460
    Top = 132
    Width = 124
    Height = 30
    Caption = 'Compilar em Lote'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = btnCompileClick
  end
  object memLog: TMemo
    Left = 16
    Top = 176
    Width = 568
    Height = 258
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 6
  end
  object FolderDialog: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = [fdoPickFolders, fdoPathMustExist]
    Left = 240
    Top = 128
  end
end
