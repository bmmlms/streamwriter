object frmStreamData: TfrmStreamData
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Stream data'
  ClientHeight = 406
  ClientWidth = 457
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
    457
    406)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 24
    Top = 108
    Width = 425
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'This option means that the stream sends complete titles which ca' +
      'n be detected by streamWriter and that there is no moderation at' +
      ' beginning/end of titles.'
    WordWrap = True
    OnClick = Label1Click
  end
  object Label2: TLabel
    Left = 24
    Top = 168
    Width = 425
    Height = 33
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'This option means that no complete titles are broadcasted, there' +
      ' is moderation in recorded tracks or that the stream sends only ' +
      'silence.'
    WordWrap = True
    OnClick = Label2Click
  end
  object Label3: TLabel
    Left = 8
    Top = 48
    Width = 441
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Information entered here will be sent to the streamWriter server' +
      ' and become available to other users so the community profits fr' +
      'om information you supply for the selected stream.'
    WordWrap = True
  end
  object btnResetTitlePattern: TPngSpeedButton
    Left = 424
    Top = 224
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
  object lblIgnoreTitles: TLabel
    Left = 8
    Top = 252
    Width = 441
    Height = 29
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'Ignore the following stream title changes (i.e. announcements li' +
      'ke '#39'Next playing: ...'#39') to disable saving:'
    WordWrap = True
  end
  object txtTitlePattern: TLabeledEdit
    Left = 8
    Top = 224
    Width = 409
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 339
    EditLabel.Height = 13
    EditLabel.Caption = 
      'Regular expression to detect artist/title/album from broadcasted' +
      ' titles:'
    TabOrder = 0
    OnChange = txtTitlePatternChange
  end
  object optGood: TRadioButton
    Left = 8
    Top = 84
    Width = 441
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Stream is good for recordings'
    Checked = True
    TabOrder = 1
    TabStop = True
    OnClick = optGoodClick
  end
  object optBad: TRadioButton
    Left = 8
    Top = 144
    Width = 441
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Stream is bad for recordings'
    TabOrder = 2
    OnClick = optBadClick
  end
  object pnlNav: TPanel
    Left = 0
    Top = 357
    Width = 457
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 3
    object Bevel2: TBevel
      Left = 4
      Top = 4
      Width = 449
      Height = 5
      Align = alTop
      Shape = bsTopLine
      ExplicitLeft = -7
      ExplicitWidth = 396
    end
    object btnOK: TBitBtn
      Left = 356
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
    end
    object btnApplyFromStream: TButton
      Left = 4
      Top = 9
      Width = 173
      Height = 36
      Align = alLeft
      Caption = 'Apply &data from stream'
      TabOrder = 1
      OnClick = btnApplyFromStreamClick
    end
  end
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 457
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 4
    object Shape1: TShape
      Left = 4
      Top = 4
      Width = 449
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
      Width = 440
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
  object lstIgnoreTitles: TListView
    Left = 8
    Top = 283
    Width = 341
    Height = 70
    Align = alCustom
    Anchors = [akLeft, akTop, akRight]
    Columns = <
      item
      end>
    RowSelect = True
    ShowColumnHeaders = False
    SmallImages = PngImageList1
    TabOrder = 5
    ViewStyle = vsReport
    OnChange = lstIgnoreTitlesChange
    OnEdited = lstIgnoreTitlesEdited
    OnResize = lstIgnoreTitlesResize
  end
  object btnAddIgnoreTitlePattern: TButton
    Left = 356
    Top = 284
    Width = 93
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Add'
    TabOrder = 6
    OnClick = btnAddIgnoreTitlePatternClick
  end
  object btnRemoveIgnoreTitlePattern: TButton
    Left = 356
    Top = 312
    Width = 93
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = '&Remove'
    Enabled = False
    TabOrder = 7
    OnClick = btnRemoveIgnoreTitlePatternClick
  end
  object PngImageList1: TPngImageList
    PngImages = <
      item
        Background = clWindow
        Name = 'delete'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000037B4944415478DA55930D6C136518C7FF6F8FB56C6DAFEB
          DA6E6CEB0AC887EC833936328DC282130CC4282A8318962598190C060C51188C
          A112ADD86D0685A10D06E302C144CDE2448D7C0D70332AEA18E2DCE868EDC6B6
          E2B8B65BBBEBDA5EDB3BDF9E13C79BFCF2E6EEDEE7FFDCF33EFF874892844847
          2BFE5B9DD53B32E8B695524E29A33094EE69EC15A75A38CC5864A6C0E5EA1DEF
          A468D2F666573E0276C15C68E65B40140493EE61F094D10B5D8805F9E655A75A
          EAEE11089FFF04976A5E6EC928CEDF5EF0E26610FF10C4000731E8950F29B406
          285823609A8F1BAD6DE07EB9667FF4E49197EE0A7C93C55A8DA5450D059B1F87
          70FD32029E71DCFE9343C81F41F2BB3A231573F28D4837EBA15A5A01475B27C6
          AEF434579E78AF8EB4676A7353D46923E5BB6A10EFFF0123BD6318EDF37E9FAC
          977291723F2DA36BD16345D0A7C7E53F4A295C895F0F7F86E878C04CDA4C5AEB
          82F56B1A8CCABFE11FF6E1E6CF9ED3F4CC0BAB3F7E97BB50BB6B055128BA0A6B
          9F834E3981C8E00022BC008D490B6F740E5C5F77BC4D3E37B13DA5D5AB4B18BF
          0B8EAB7E04C702EBD67CD478E6FCD63D7270D1F39BC02AFC080F3A30F0DB1D48
          202878280BA26911BA4F9CEB239F1AD9E883CF9628C5A00FDD973C10130955B2
          85C9E0A55B36422B7108BB69F0351FF889888D96B3B76C552E18568F2BED7F08
          E4A45137B96C458E464144F4FCC8250556CAC1351BA0498C61CAE580B36F027C
          206A5B7BD45A7F66FBFE6869458E327917573B3D026935EA7AF28B0D25A934EF
          5F5C2A02B73C28AE7E06EAC86D845C37E0BCC9233429D8D6BDFF66FD773B5FAF
          D46667752CB648884612E8FDDDE724C70D3AABF93E4343261B476CDE72306A1D
          D453A3E09DFD70B9C3E0F998ED894307EA9319BF7DE5C0170F54ADAD629C3FC1
          37C960C8E53F4A8E19D27355A9AA91251606EA854BE436C9C123318442711B7D
          7C63DAD65B2CE5CB6ACD060953EE01F40DC521848542D9487683BE49CFCEDA3D
          37939105423106DE044B3BE285244A48CFCB41DEF2626468198C779DC5309780
          2F103FFC6463C34E59E0D6B1669CDE77F043BD9A6CCB333048335BA02B7B184A
          5336F5B102C21D7A1F8E5E04FBAF63D42FC2C78BF6A70EEEFBDFCA43F64639F3
          57FB1B9B5218B2DBA896A09B0DA866FD3B304202088401EF148110970EADB7EE
          79F59E611AFCC076773CDB5F6BB2D06D1BA58AB270FAB59BF265D2DE4FBF55E7
          9C39CEFF00233280974F14462B0000000049454E44AE426082}
      end>
    Left = 212
    Top = 116
    Bitmap = {}
  end
end
