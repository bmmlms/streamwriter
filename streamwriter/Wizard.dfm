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
  object pnlDir: TPanel
    Left = 8
    Top = 8
    Width = 389
    Height = 233
    BevelOuter = bvNone
    TabOrder = 0
    object cmdBrowse: TSpeedButton
      Left = 308
      Top = 16
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
    object txtDir: TLabeledEdit
      Left = 8
      Top = 16
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
  object pnlBandwidth: TPanel
    Left = 8
    Top = 260
    Width = 389
    Height = 233
    BevelOuter = bvNone
    TabOrder = 1
    object Label2: TLabel
      Left = 80
      Top = 44
      Width = 21
      Height = 13
      Caption = 'KB/s'
    end
    object chkLimit: TCheckBox
      Left = 8
      Top = 0
      Width = 281
      Height = 21
      Caption = 'Limit bandwidth for recordings'
      TabOrder = 0
      OnClick = chkLimitClick
    end
    object txtMaxSpeed: TLabeledEdit
      Left = 24
      Top = 40
      Width = 53
      Height = 21
      EditLabel.Width = 225
      EditLabel.Height = 13
      EditLabel.Caption = 'Maximum bandwidth available to streamWriter:'
      Enabled = False
      MaxLength = 5
      NumbersOnly = True
      TabOrder = 1
    end
  end
end
