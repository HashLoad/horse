object frmVCL: TfrmVCL
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'VCL'
  ClientHeight = 77
  ClientWidth = 129
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
  object btnStartStop: TBitBtn
    Left = 8
    Top = 45
    Width = 113
    Height = 25
    Caption = 'Start'
    TabOrder = 0
    OnClick = btnStartStopClick
  end
end
