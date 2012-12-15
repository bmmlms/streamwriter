object frmHomeTest: TfrmHomeTest
  Left = 0
  Top = 0
  Caption = 'frmHomeTest'
  ClientHeight = 261
  ClientWidth = 421
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
  object prgTransfer: TProgressBar
    Left = 4
    Top = 240
    Width = 309
    Height = 17
    TabOrder = 1
  end
  object Button1: TButton
    Left = 320
    Top = 4
    Width = 97
    Height = 29
    Caption = 'Button1'
    TabOrder = 2
    OnClick = Button1Click
  end
end
