object frmSetStreamData: TfrmSetStreamData
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Set data'
  ClientHeight = 401
  ClientWidth = 654
  Color = clBtnFace
  Constraints.MinHeight = 440
  Constraints.MinWidth = 670
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    0000010020000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000959595B4818181FF818181FF818181FF818181FF818181FF818181FF8181
    81FF818181FF818181FF818181FF818181FF818181FF959595A8000000000000
    0000818181FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF818181FF000000000000
    0000818181FFFFFFFFFFEDEDEDFFEDEDEDFFEEEEEEFFEFEFEFFFEFEFEFFFF0F0
    F0FFF0F0F0FFE0E0E0FFADADADFFBABABAFFFFFFFFFF818181FF000000000000
    0000818181FFFFFFFFFFEDEDEDFFC6C6C6FFC7C7C7FFC7C7C7FFC8C8C8FFC8C8
    C8FFB5B5B5FF929292FF9E9E9EFFABABABFFFFFFFFFF818181FF000000000000
    0000818181FFFFFFFFFFECECECFFEDEDEDFFEEEEEEFFEEEEEEFFEFEFEFFFD4D4
    D4FF777777FF858585FF909090FF9D9D9DFFFFFFFFFF818181FF000000000000
    0000818181FFFFFFFFFFECECECFFB0B0B0FF585858FF585858FF585858FF5858
    58FF595959FF626262FF6C6C6CFFDCDCDCFFFFFFFFFF818181FF000000000000
    0000818181FFFFFFFFFFECECECFF696969FF696969FF696969FF6A6A6AFF6A6A
    6AFF6A6A6AFF6A6A6AFFD6D6D6FFF0F0F0FFFFFFFFFF818181FF000000000000
    0000818181FFFFFFFFFFEBEBEBFF585858FF585858FFB0B0B0FF585858FF6A6A
    6AFF6A6A6AFFCCD2D2FFEEEEEEFFF0F0F0FFFFFFFFFF818181FF000000000000
    0000818181FFFFFFFFFFEBEBEBFF696969FFD2D2D2FFECECECFFD2D2D2FF6969
    69FF666969FFECECECFFEEEEEEFFF0F0F0FFFFFFFFFF818181FF000000000000
    0000818181FFFFFFFFFFEBEBEBFFC5C5C5FFC6C6C6FFB0B0B0FF585858FF5858
    58FF585858FFC6C6C6FFC7C7C7FFF0F0F0FFFFFFFFFF818181FF000000000000
    0000818181FFFFFFFFFFEAEAEAFFEBEBEBFFEBEBEBFF696969FF696969FF6969
    69FFD2D2D2FFEDEDEDFFEEEEEEFFF0F0F0FFFFFFFFFF818181FF000000000000
    0000818181FFFFFFFFFFEAEAEAFFC4C4C4FFC5C5C5FFC5C5C5FFC6C6C6FFC6C6
    C6FFC6C6C6FFC6C6C6FFC7C7C7FFF0F0F0FFFFFFFFFF818181FF000000000000
    0000818181FFFFFFFFFFECECECFFEAEAEAFFEAEAEAFFEBEBEBFFEBEBEBFFEBEB
    EBFFECECECFFECECECFFEDEDEDFFF0F0F0FFFFFFFFFF818181FF000000000000
    0000818181FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF818181FF000000000000
    0000999999AC818181FF818181FF818181FF818181FF818181FF818181FF8181
    81FF818181FF818181FF818181FF818181FF818181FF8181815600000000FFFF
    0000800100008001000080010000800100008001000080010000800100008001
    000080010000800100008001000080010000800100008001000080030000}
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  DesignSize = (
    654
    401)
  PixelsPerInch = 96
  TextHeight = 13
  object btnResetTitlePattern: TPngSpeedButton
    Left = 625
    Top = 304
    Width = 25
    Height = 21
    Hint = 'Reset regular expression to default'
    Anchors = [akRight, akBottom]
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
    ExplicitLeft = 640
    ExplicitTop = 316
  end
  object Label21: TLabel
    Left = 289
    Top = 160
    Width = 360
    Height = 29
    Anchors = [akTop, akRight]
    AutoSize = False
    Caption = 
      'Use the following regular expressions for this stream (groups: a' +
      ' = artist, t = title, l = album):'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 289
    Top = 4
    Width = 360
    Height = 17
    Anchors = [akTop, akRight]
    AutoSize = False
    Caption = 'Regular expressions set by other users:'
    WordWrap = True
  end
  object pnlNav: TPanel
    Left = 0
    Top = 361
    Width = 654
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 0
    object Bevel2: TBevel
      Left = 4
      Top = 4
      Width = 646
      Height = 5
      Align = alTop
      Shape = bsTopLine
      ExplicitLeft = -7
      ExplicitWidth = 396
    end
    object btnOK: TBitBtn
      Left = 553
      Top = 9
      Width = 97
      Height = 27
      Align = alRight
      Caption = '&OK'
      DoubleBuffered = False
      Layout = blGlyphRight
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 4
      Top = 9
      Width = 97
      Height = 27
      Align = alLeft
      Caption = '&Cancel'
      DoubleBuffered = False
      Layout = blGlyphRight
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
  object lstTitles: TVirtualStringTree
    Left = 4
    Top = 4
    Width = 278
    Height = 353
    Anchors = [akLeft, akTop, akRight, akBottom]
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    Images = modSharedData.imgImages
    TabOrder = 1
    OnGetText = lstTitlesGetText
    OnGetImageIndex = lstTitlesGetImageIndex
    OnMeasureItem = lstTitlesMeasureItem
    OnMeasureTextWidth = lstTitlesMeasureTextWidth
    Columns = <>
  end
  object txtRegEx: TLabeledEdit
    Left = 289
    Top = 304
    Width = 333
    Height = 21
    Anchors = [akRight, akBottom]
    EditLabel.Width = 130
    EditLabel.Height = 13
    EditLabel.Caption = 'Regular expression to add:'
    EditLabel.Layout = tlBottom
    MaxLength = 255
    TabOrder = 2
    OnChange = txtRegExChange
  end
  object btnAddRegEx: TButton
    Left = 461
    Top = 332
    Width = 93
    Height = 27
    Anchors = [akRight, akBottom]
    Caption = '&Add'
    TabOrder = 3
    OnClick = btnAddRegExClick
  end
  object btnRemoveRegEx: TButton
    Left = 557
    Top = 332
    Width = 93
    Height = 27
    Anchors = [akRight, akBottom]
    Caption = '&Remove'
    TabOrder = 4
    OnClick = btnRemoveRegExClick
  end
  object lstRegExps: TListView
    Left = 289
    Top = 188
    Width = 361
    Height = 93
    Align = alCustom
    Anchors = [akTop, akRight, akBottom]
    Columns = <
      item
      end>
    RowSelect = True
    ShowColumnHeaders = False
    SmallImages = modSharedData.imgImages
    TabOrder = 5
    ViewStyle = vsReport
    OnChange = lstRegExpsChange
    OnEdited = lstRegExpsEdited
  end
  object lstOtherRegExps: TListView
    Left = 289
    Top = 20
    Width = 361
    Height = 133
    Align = alCustom
    Anchors = [akTop, akRight]
    Columns = <
      item
      end>
    ReadOnly = True
    RowSelect = True
    ShowColumnHeaders = False
    SmallImages = modSharedData.imgImages
    TabOrder = 6
    ViewStyle = vsReport
  end
end
