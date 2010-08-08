object frmWizard: TfrmWizard
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Setup wizard'
  ClientHeight = 569
  ClientWidth = 547
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnlICEServer: TPanel
    Left = 32
    Top = 12
    Width = 393
    Height = 233
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      393
      233)
    object Label6: TLabel
      Left = 24
      Top = 28
      Width = 365
      Height = 77
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'When enabled, you can listen to streams you are recording by usi' +
        'ng the provided context-menu items or by dragging the stream int' +
        'o your player. This might cause warnings from the firewall, so i' +
        't is disabled by default.'
      WordWrap = True
    end
    object Label1: TLabel
      Left = 24
      Top = 132
      Width = 365
      Height = 77
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'When enabled every stream unknown to streamWriter will be submit' +
        'ted to the stream database. This helps populating the stream dat' +
        'abase so streamWriter'#39's browser will be kept up to date. No pers' +
        'onal information will be sent, only the stream'#39's url.'
      WordWrap = True
    end
    object chkRelay: TCheckBox
      Left = 8
      Top = 4
      Width = 253
      Height = 21
      Caption = 'Enable local ICE-Server to listen to streams'
      TabOrder = 0
    end
    object chkSubmitStreams: TCheckBox
      Left = 8
      Top = 108
      Width = 257
      Height = 21
      Caption = 'Submit stream urls to stream database'
      TabOrder = 1
    end
  end
  object pnlDir: TPanel
    Left = 12
    Top = 288
    Width = 389
    Height = 233
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      389
      233)
    object cmdBrowse: TSpeedButton
      Left = 308
      Top = 56
      Width = 21
      Height = 21
      Hint = 'Browse...'
      Flat = True
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        1800000000000003000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFF5F5F5DDDDDDD1D1D1D1D1D1D1D1D1D1D1D1D1
        D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1D1DDDDDDF5F5F5FFFFFFF5F5F5C5C5C5
        8989897171717171717171717171717171717171717171717171717171717171
        71898989C5C5C5F5F5F5DDDDDD0D73A50D73A50D73A50D73A50D73A50D73A50D
        73A50D73A50D73A50D73A50D73A50D73A5656565898989DDDDDD199AC61C9CC7
        9CFFFF6CD7FF6CD7FF6CD7FF6CD7FF6CD7FF6CD7FF6CD7FF6CD7FF6CD7FF2999
        BF0D73A5717171D1D1D1199AC61A9AC67AE4F09CFFFF7CE3FF7CE3FF7CE3FF7C
        E3FF7CE3FF7CE3FF7CE3FF7CDFFF43B2DE1A7B9D656565B9B9B9199AC626A2CF
        40B8D79CFFFF84EBFF84EBFF84EBFF84EBFF84EBFF84EBFF84EBFF84E7FF43BA
        EF199AC6656565898989199AC643B3E221A0C9A5FFFF94F7FF94F7FF94F7FF94
        F7FF94F7FF94F7FF94F7FF94F7FF53BEE75CBCCE0D73A5717171199AC670D5FD
        199AC689F0F79CFFFF9CFFFF9CFFFF9CFFFF9CFFFF9CFFFF9CFFFF9CFFFF5BC7
        FF96F9FB197B9B717171199AC684D7FF199AC66CBFDAFFFFFFFFFFFFF7FBFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF84E7FFFFFFFF197EA1898989199AC684EBFF
        50C1E2199AC6199AC6199AC6199AC6199AC6199AC6199AC6199AC6199AC6199A
        C6199AC61989B1C5C5C5199AC69CF3FF8CF3FF8CF3FF8CF3FF8CF3FF8CF3FFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFF199AC61A7B9DC5C5C5F5F5F5199AC6FFFFFF
        9CFFFF9CFFFF9CFFFF9CFFFFFFFFFF199AC6199AC6199AC6199AC6199AC6199A
        C6DDDDDDF5F5F5FFFFFFFFFFFF22A2CEFFFFFFFFFFFFFFFFFFFFFFFF199AC6C5
        C5C5F5F5F5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        22A2CE22A2CE22A2CE22A2CEDDDDDDF5F5F5FFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
      Layout = blGlyphRight
      ParentShowHint = False
      ShowHint = True
      OnClick = cmdBrowseClick
    end
    object lblDir: TLabel
      Left = 8
      Top = 4
      Width = 377
      Height = 41
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Please select a folder where songs will be saved.'
      WordWrap = True
      ExplicitWidth = 381
    end
    object Label2: TLabel
      Left = 8
      Top = 128
      Width = 381
      Height = 57
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'You can configure all application'#39's options using the correspond' +
        'ing menu entry when the main window is shown.'
      WordWrap = True
    end
    object txtDir: TLabeledEdit
      Left = 8
      Top = 56
      Width = 293
      Height = 21
      AutoSize = False
      Color = 15790320
      EditLabel.Width = 34
      EditLabel.Height = 13
      EditLabel.Caption = 'Folder:'
      ReadOnly = True
      TabOrder = 0
    end
  end
end
