object frmSettingsAddPostProcessor: TfrmSettingsAddPostProcessor
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Add postprocessor'
  ClientHeight = 120
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 155
    Height = 13
    Caption = 'This postprocessor is used for...'
  end
  object pnlNav: TPanel
    Left = 0
    Top = 76
    Width = 345
    Height = 44
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 0
    ExplicitWidth = 313
    object Bevel2: TBevel
      Left = 4
      Top = 4
      Width = 337
      Height = 5
      Align = alTop
      Shape = bsTopLine
      ExplicitLeft = -7
      ExplicitWidth = 396
    end
    object btnOK: TBitBtn
      Left = 244
      Top = 9
      Width = 97
      Height = 31
      Align = alRight
      Caption = '&OK'
      Default = True
      DoubleBuffered = False
      Layout = blGlyphRight
      ParentDoubleBuffered = False
      TabOrder = 0
      OnClick = btnOKClick
      ExplicitLeft = 212
    end
  end
  object optWAVE: TRadioButton
    Left = 8
    Top = 28
    Width = 333
    Height = 21
    Caption = '...processing when in WAVE-format'
    Checked = True
    TabOrder = 1
    TabStop = True
  end
  object optDestinationFormat: TRadioButton
    Left = 8
    Top = 48
    Width = 333
    Height = 21
    Caption = '...processing after conversion to destination format'
    TabOrder = 2
  end
end
