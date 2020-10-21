object FrmVCL: TFrmVCL
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'VCL'
  ClientHeight = 93
  ClientWidth = 248
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object lbStatus: TLabel
    Left = 8
    Top = 7
    Width = 70
    Height = 13
    Caption = 'Status: Offline'
  end
  object lbPorta: TLabel
    Left = 8
    Top = 26
    Width = 24
    Height = 13
    Caption = 'Port:'
  end
  object btnStop: TBitBtn
    Left = 127
    Top = 53
    Width = 113
    Height = 25
    Caption = 'Stop'
    Enabled = False
    TabOrder = 0
    OnClick = btnStopClick
  end
  object btnStart: TBitBtn
    Left = 8
    Top = 53
    Width = 113
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = btnStartClick
  end
end
