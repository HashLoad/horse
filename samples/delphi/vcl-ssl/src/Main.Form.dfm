object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Horse VCL with SSL - Sample'
  ClientHeight = 298
  ClientWidth = 356
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object edtPort: TSpinEdit
    Left = 8
    Top = 20
    Width = 97
    Height = 24
    MaxValue = 9999999
    MinValue = 1000
    TabOrder = 0
    Value = 9000
  end
  object Button1: TButton
    Left = 144
    Top = 242
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 1
    OnClick = Button1Click
  end
  object leKey: TLabeledEdit
    Left = 8
    Top = 76
    Width = 313
    Height = 23
    EditLabel.Width = 58
    EditLabel.Height = 15
    EditLabel.Caption = 'Private Key'
    ReadOnly = True
    TabOrder = 2
    Text = ''
  end
  object leCrt: TLabeledEdit
    Left = 8
    Top = 132
    Width = 313
    Height = 23
    EditLabel.Width = 55
    EditLabel.Height = 15
    EditLabel.Caption = 'Public Key'
    ReadOnly = True
    TabOrder = 3
    Text = ''
  end
  object Button2: TButton
    Left = 320
    Top = 75
    Width = 33
    Height = 25
    Caption = '...'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 320
    Top = 131
    Width = 33
    Height = 25
    Caption = '...'
    TabOrder = 5
    OnClick = Button3Click
  end
  object lePassword: TLabeledEdit
    Left = 9
    Top = 190
    Width = 144
    Height = 23
    EditLabel.Width = 50
    EditLabel.Height = 15
    EditLabel.Caption = 'Password'
    PasswordChar = '*'
    TabOrder = 6
    Text = ''
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 279
    Width = 356
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitTop = 278
    ExplicitWidth = 352
  end
  object OpenDialog1: TOpenDialog
    Left = 264
    Top = 16
  end
end
