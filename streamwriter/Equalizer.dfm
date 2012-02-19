object frmEqualizer: TfrmEqualizer
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Equalizer'
  ClientHeight = 145
  ClientWidth = 269
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object pnlEqualizer: TPanel
    Left = 0
    Top = 36
    Width = 269
    Height = 109
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 0
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 8
    Width = 149
    Height = 21
    Caption = 'Equalizer active'
    TabOrder = 1
    OnClick = CheckBox1Click
  end
end
