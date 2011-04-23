object frmStreamData: TfrmStreamData
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Stream data'
  ClientHeight = 345
  ClientWidth = 333
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  OnKeyDown = FormKeyDown
  DesignSize = (
    333
    345)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 112
    Width = 301
    Height = 45
    AutoSize = False
    Caption = 
      'This option means that the stream sends complete titles which ca' +
      'n be detected by streamWriter and that there is no moderation at' +
      ' beginning/end of titles.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 24
    Top = 188
    Width = 301
    Height = 57
    AutoSize = False
    Caption = 
      'This option means that no complete titles are broadcasted, there' +
      ' is moderation in recorded tracks or that the stream sends only ' +
      'silence.'
    WordWrap = True
  end
  object Label3: TLabel
    Left = 8
    Top = 48
    Width = 317
    Height = 37
    AutoSize = False
    Caption = 
      'Information entered here will be sent to the streamWriter server' +
      ' and become available to other users.'
    WordWrap = True
  end
  object btnResetTitlePattern: TPngSpeedButton
    Left = 300
    Top = 268
    Width = 25
    Height = 21
    Hint = 'Reset pattern to default'
    Anchors = [akTop, akRight]
    Flat = True
    ParentShowHint = False
    ShowHint = True
    OnClick = btnResetTitlePatternClick
    PngImage.Data = {
      89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
      610000001974455874536F6674776172650041646F626520496D616765526561
      647971C9653C000002734944415478DA63FCFFFF3F032110B1C4BFE2D78FDFDD
      EB52B6FD45976324C68080591E977EFDF8756A5BDEBE149C06E41F4C91FDF3FB
      4FF0EF9F7FDC7FFDFC6DFCFBE72FD19F3F7E33003532480B4933FCFAF59BE1C6
      9D9B138F369E2EC030206F7FB2E5EFDF7FCAA5B9A4FDF9D90518B8D8B8199818
      19197EFFFBCDF0FBCF1F30FDE3F74F86B317CF31DCB871B3F5F2A41B35700372
      F624C8FEFEF567B2BAB0A6BF18B718C3975F5F809A99197EFFFD0D3500E80A20
      FD0BC83F7BE61CC3CD1BB7BB6FCDBA5B063720754B748124B754BFB2B03250D3
      1F868F3F3E31DC787083E1F6FDDB40E7FF66F809F4828C943403D0850CB76EDC
      9E727BCEFD5C142FC4AD0ED9AE2DA1E3C1C3C9CBF0F5C737865DC7773D02FA7B
      CDC19A13C530451AE92A9780E173F6CEBC0789188118BAD0F70550830010FFFB
      F9FDD777203DED70FDA95A64454A09F2D57FFFFCED7008B08C03B9E817347041
      6C46DF69AE8910C15F702703152E026AA8E8B79DDD8A6C50D822BF66A0BA2C20
      E6045AC604A43F604D07C0289D0F74B2F15497057A30B190F93EBD400D218E06
      8E728CCC4C0CCF5F3D67D87B78FF0E0C0380513A59825332076800C3DD977719
      604E961294625010576060616381C408304AAF5DBB5E886240EEDEC42E710EC9
      5215615586BF7FFF02A3F10F243AA151FA091843CC4C2C0CF71EDC67387EFCE4
      4660F4E7A21890B635A6459C53A25A594499E1EFBF7FE0F8076906D9F8EDFB57
      86D7EF5F333C7DFA8CE1C6B59B1B81D1DA098CD2E3185E00669C09E2BC12F9A0
      A47BE7E11D6868030DFAF9EB3530899F0526F59D40EFAD0546E9639C99C96B92
      D31C60289BED2D3FA2C740006035C0B1DD8A191870A5C08CD341C80000DC1D99
      CCC6DEEDD30000000049454E44AE426082}
  end
  object txtTitlePattern: TLabeledEdit
    Left = 8
    Top = 268
    Width = 289
    Height = 21
    EditLabel.Width = 169
    EditLabel.Height = 13
    EditLabel.Caption = 'Regular expression to detect titles:'
    TabOrder = 0
    OnChange = txtTitlePatternChange
  end
  object optGood: TRadioButton
    Left = 8
    Top = 88
    Width = 233
    Height = 21
    Caption = 'Stream is good for recordings'
    Checked = True
    TabOrder = 1
    TabStop = True
    OnClick = optGoodClick
  end
  object optBad: TRadioButton
    Left = 8
    Top = 164
    Width = 233
    Height = 21
    Caption = 'Stream is bad for recordings'
    TabOrder = 2
    OnClick = optBadClick
  end
  object pnlNav: TPanel
    Left = 0
    Top = 296
    Width = 333
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 3
    ExplicitLeft = -224
    ExplicitTop = 268
    ExplicitWidth = 609
    object Bevel2: TBevel
      Left = 4
      Top = 4
      Width = 325
      Height = 5
      Align = alTop
      Shape = bsTopLine
      ExplicitLeft = -7
      ExplicitWidth = 396
    end
    object btnOK: TBitBtn
      Left = 232
      Top = 9
      Width = 97
      Height = 36
      Align = alRight
      Caption = '&OK'
      Default = True
      DoubleBuffered = True
      Layout = blGlyphRight
      ParentDoubleBuffered = False
      TabOrder = 0
      OnClick = btnOKClick
      ExplicitLeft = 508
    end
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 333
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 4
    ExplicitWidth = 350
    object Shape1: TShape
      Left = 4
      Top = 4
      Width = 325
      Height = 33
      Align = alClient
      Brush.Color = clActiveCaption
      Pen.Color = clActiveBorder
      ExplicitLeft = -276
      ExplicitWidth = 801
      ExplicitHeight = 29
    end
    object lblTop: TLabel
      AlignWithMargins = True
      Left = 10
      Top = 9
      Width = 316
      Height = 25
      Margins.Left = 6
      Margins.Top = 5
      Align = alClient
      AutoSize = False
      Caption = 'Data for'
      Color = clActiveCaption
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      GlowSize = 1
      ParentColor = False
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = 76
      ExplicitTop = 4
      ExplicitWidth = 190
      ExplicitHeight = 30
    end
  end
end
