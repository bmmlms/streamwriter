object frmSettings: TfrmSettings
  Left = 350
  Top = 283
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Settings'
  ClientHeight = 565
  ClientWidth = 905
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pnlStreams: TPanel
    Left = 304
    Top = 4
    Width = 293
    Height = 489
    TabOrder = 0
    DesignSize = (
      293
      489)
    object Label4: TLabel
      Left = 56
      Top = 248
      Width = 14
      Height = 13
      Caption = 'KB'
    end
    object Label5: TLabel
      Left = 56
      Top = 292
      Width = 14
      Height = 13
      Caption = 'KB'
    end
    object Label8: TLabel
      Left = 267
      Top = 248
      Width = 22
      Height = 13
      Cursor = crHandPoint
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'Help'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = Label8Click
    end
    object Label9: TLabel
      Left = 267
      Top = 296
      Width = 22
      Height = 13
      Cursor = crHandPoint
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'Help'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = Label9Click
    end
    object cmdBrowse: TSpeedButton
      Left = 267
      Top = 172
      Width = 21
      Height = 21
      Hint = 'Browse...'
      Anchors = [akTop, akRight]
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
    object txtShortSongSize: TLabeledEdit
      Left = 4
      Top = 243
      Width = 49
      Height = 21
      EditLabel.Width = 132
      EditLabel.Height = 13
      EditLabel.Caption = 'Ads to skip are smaller than:'
      MaxLength = 4
      NumbersOnly = True
      TabOrder = 0
    end
    object txtSongBuffer: TLabeledEdit
      Left = 4
      Top = 287
      Width = 49
      Height = 21
      EditLabel.Width = 154
      EditLabel.Height = 13
      EditLabel.Caption = 'Buffer for start and end of songs:'
      MaxLength = 3
      NumbersOnly = True
      TabOrder = 1
    end
    object txtDir: TLabeledEdit
      Left = 4
      Top = 172
      Width = 257
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Color = 15790320
      EditLabel.Width = 110
      EditLabel.Height = 13
      EditLabel.Caption = 'Folder for saved songs:'
      ReadOnly = True
      TabOrder = 2
    end
    object GroupBox2: TGroupBox
      Left = 4
      Top = 4
      Width = 285
      Height = 145
      Anchors = [akLeft, akTop, akRight]
      Caption = ' Filenames '
      TabOrder = 3
      DesignSize = (
        285
        145)
      object lblFilePattern: TLabel
        Left = 8
        Top = 108
        Width = 269
        Height = 33
        Anchors = [akLeft, akTop, akRight]
        AutoSize = False
        Caption = '-'
        WordWrap = True
      end
      object txtFilePattern: TLabeledEdit
        Left = 8
        Top = 36
        Width = 269
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        EditLabel.Width = 151
        EditLabel.Height = 13
        EditLabel.Caption = 'Pattern for names of saved files:'
        TabOrder = 0
        OnChange = txtFilePatternChange
      end
      object txtPreview: TLabeledEdit
        Left = 8
        Top = 80
        Width = 269
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        Color = 15790320
        EditLabel.Width = 41
        EditLabel.Height = 13
        EditLabel.Caption = 'Preview:'
        ReadOnly = True
        TabOrder = 1
      end
    end
    object chkSkipShort: TCheckBox
      Left = 4
      Top = 200
      Width = 220
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Skip ads (short songs)'
      TabOrder = 4
      OnClick = chkSkipShortClick
    end
  end
  object pnlMain: TPanel
    Left = 4
    Top = 4
    Width = 293
    Height = 273
    TabOrder = 1
    DesignSize = (
      293
      273)
    object Label7: TLabel
      Left = 56
      Top = 52
      Width = 15
      Height = 13
      Caption = 'GB'
    end
    object Label3: TLabel
      Left = 4
      Top = 84
      Width = 190
      Height = 13
      Caption = 'Default action on doubleclick on stream:'
    end
    object chkTrayClose: TCheckBox
      Left = 4
      Top = 0
      Width = 281
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Show in notification area and minimize on close'
      TabOrder = 0
    end
    object txtMinDiskSpace: TLabeledEdit
      Left = 4
      Top = 48
      Width = 49
      Height = 21
      EditLabel.Width = 205
      EditLabel.Height = 13
      EditLabel.Caption = 'Stop recording when free space gets below'
      MaxLength = 3
      NumbersOnly = True
      TabOrder = 1
    end
    object lstDefaultAction: TComboBox
      Left = 4
      Top = 100
      Width = 213
      Height = 21
      Style = csDropDownList
      TabOrder = 2
    end
  end
  object pnlAdvanced: TPanel
    Left = 604
    Top = 4
    Width = 294
    Height = 273
    TabOrder = 2
    DesignSize = (
      294
      273)
    object Label1: TLabel
      Left = 56
      Top = 64
      Width = 40
      Height = 13
      Caption = 'seconds'
    end
    object Label6: TLabel
      Left = 267
      Top = 120
      Width = 22
      Height = 13
      Cursor = crHandPoint
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'Help'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = Label6Click
    end
    object Label11: TLabel
      Left = 267
      Top = 96
      Width = 22
      Height = 13
      Cursor = crHandPoint
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'Help'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = Label11Click
    end
    object txtMaxRetries: TLabeledEdit
      Left = 4
      Top = 16
      Width = 49
      Height = 21
      EditLabel.Width = 168
      EditLabel.Height = 13
      EditLabel.Caption = 'Max. retries on error (zero is infinite):'
      MaxLength = 3
      NumbersOnly = True
      TabOrder = 0
    end
    object txtRetryDelay: TLabeledEdit
      Left = 4
      Top = 60
      Width = 49
      Height = 21
      EditLabel.Width = 101
      EditLabel.Height = 13
      EditLabel.Caption = 'Time between retries:'
      MaxLength = 3
      NumbersOnly = True
      TabOrder = 1
    end
    object chkSubmitStreams: TCheckBox
      Left = 4
      Top = 116
      Width = 249
      Height = 21
      Caption = 'Submit stream URLs to stream database'
      TabOrder = 2
    end
    object chkRelay: TCheckBox
      Left = 4
      Top = 90
      Width = 249
      Height = 23
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Enable local ICE-Server to listen to streams'
      TabOrder = 3
    end
  end
  object pnlPlugins: TPanel
    Left = 4
    Top = 284
    Width = 294
    Height = 273
    TabOrder = 3
    DesignSize = (
      294
      273)
    object lblHelp: TLabel
      Left = 4
      Top = 184
      Width = 285
      Height = 53
      Anchors = [akLeft, akRight, akBottom]
      AutoSize = False
      Caption = '-'
      WordWrap = True
    end
    object lstPlugins: TListView
      Left = 4
      Top = 1
      Width = 289
      Height = 176
      Align = alCustom
      Anchors = [akLeft, akTop, akRight, akBottom]
      Checkboxes = True
      Columns = <
        item
          Width = 200
        end>
      Groups = <
        item
          GroupID = 0
          State = [lgsNormal]
          HeaderAlign = taLeftJustify
          FooterAlign = taLeftJustify
          TopDescription = 'Pines'
          TitleImage = -1
          ExtendedImage = -1
        end>
      GroupView = True
      ReadOnly = True
      RowSelect = True
      ShowColumnHeaders = False
      TabOrder = 0
      ViewStyle = vsReport
      OnSelectItem = lstPluginsSelectItem
    end
    object cmdConfigure: TBitBtn
      Left = 209
      Top = 248
      Width = 84
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Configure...'
      DoubleBuffered = True
      Enabled = False
      ParentDoubleBuffered = False
      TabOrder = 1
    end
  end
end
