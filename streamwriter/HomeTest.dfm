object frmHomeTest: TfrmHomeTest
  Left = 0
  Top = 0
  Caption = 'frmHomeTest'
  ClientHeight = 241
  ClientWidth = 529
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lstEvents: TListBox
    Left = 4
    Top = 4
    Width = 309
    Height = 233
    ItemHeight = 13
    TabOrder = 0
  end
  object Button1: TButton
    Left = 416
    Top = 80
    Width = 109
    Height = 29
    Caption = 'Button1'
    Enabled = False
    TabOrder = 1
    OnClick = Button1Click
  end
  object prgTransfer: TProgressBar
    Left = 332
    Top = 132
    Width = 165
    Height = 17
    TabOrder = 2
  end
  object Button2: TButton
    Left = 416
    Top = 4
    Width = 109
    Height = 29
    Caption = 'LogIn'
    TabOrder = 3
    OnClick = Button2Click
  end
end
