object frmSettings: TfrmSettings
  Left = 350
  Top = 283
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Settings'
  ClientHeight = 702
  ClientWidth = 1229
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlStreams: TPanel
    Left = 304
    Top = 4
    Width = 293
    Height = 369
    TabOrder = 0
    Visible = False
    DesignSize = (
      293
      369)
    object lblDefaultFilter: TLabel
      Left = 4
      Top = 328
      Width = 25
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Filter:'
    end
    object GroupBox2: TGroupBox
      Left = 4
      Top = 4
      Width = 285
      Height = 157
      Anchors = [akLeft, akTop, akRight]
      Caption = ' Filenames '
      TabOrder = 0
      DesignSize = (
        285
        157)
      object lblFilePattern: TLabel
        Left = 8
        Top = 108
        Width = 269
        Height = 41
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
    object chkDeleteStreams: TCheckBox
      Left = 4
      Top = 248
      Width = 265
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'Delete stream-files when recording stops'
      TabOrder = 1
      OnClick = chkDeleteStreamsClick
    end
    object chkAddSavedToIgnore: TCheckBox
      Left = 4
      Top = 272
      Width = 281
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'Automatically add saved titles to ignore list'
      TabOrder = 2
      OnClick = chkAddSavedToIgnoreClick
    end
    object lstDefaultFilter: TComboBox
      Left = 4
      Top = 344
      Width = 213
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akBottom]
      TabOrder = 3
      OnChange = lstDefaultFilterChange
      Items.Strings = (
        'Save every song'
        'Use wishlist'
        'Use ignorelist')
    end
    object chkSeparateTracks: TCheckBox
      Left = 4
      Top = 200
      Width = 281
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'Save separated tracks'
      TabOrder = 4
      OnClick = chkSeparateTracksClick
    end
    object chkSaveStreamsToMemory: TCheckBox
      Left = 4
      Top = 176
      Width = 281
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'Save received data to memory instead of disk'
      TabOrder = 5
      OnClick = chkSaveStreamsToMemoryClick
    end
    object chkOnlySaveFull: TCheckBox
      Left = 20
      Top = 224
      Width = 265
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'Only save whole songs'
      TabOrder = 6
      OnClick = chkOnlySaveFullClick
    end
    object chkOverwriteSmaller: TCheckBox
      Left = 4
      Top = 296
      Width = 281
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'Always overwrite existing files if newer file is larger'
      TabOrder = 7
      OnClick = chkOverwriteSmallerClick
    end
  end
  object pnlMain: TPanel
    Left = 4
    Top = 4
    Width = 293
    Height = 273
    TabOrder = 1
    Visible = False
    DesignSize = (
      293
      273)
    object Label7: TLabel
      Left = 56
      Top = 136
      Width = 15
      Height = 13
      Caption = 'GB'
    end
    object Label3: TLabel
      Left = 4
      Top = 164
      Width = 190
      Height = 13
      Caption = 'Default action on doubleclick on stream:'
    end
    object btnBrowse: TSpeedButton
      Left = 268
      Top = 84
      Width = 21
      Height = 21
      Hint = 'Browse...'
      Anchors = [akTop, akRight]
      Flat = True
      Layout = blGlyphRight
      ParentShowHint = False
      ShowHint = True
      OnClick = btnBrowseClick
    end
    object chkTray: TCheckBox
      Left = 4
      Top = 0
      Width = 281
      Height = 22
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Move to notification area...'
      TabOrder = 0
      OnClick = chkTrayClick
    end
    object txtMinDiskSpace: TLabeledEdit
      Left = 4
      Top = 132
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
      Top = 180
      Width = 213
      Height = 21
      Style = csDropDownList
      TabOrder = 2
      Items.Strings = (
        'Start/stop recording'
        'Listen to stream'
        'Listen to stream (external player)'
        'Listen to recorded file')
    end
    object optClose: TRadioButton
      Left = 20
      Top = 20
      Width = 165
      Height = 21
      Caption = '...on close'
      TabOrder = 3
    end
    object optMinimize: TRadioButton
      Left = 20
      Top = 40
      Width = 165
      Height = 21
      Caption = '...on minimize'
      TabOrder = 4
    end
    object txtDir: TLabeledEdit
      Left = 4
      Top = 84
      Width = 257
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Color = 15790320
      EditLabel.Width = 110
      EditLabel.Height = 13
      EditLabel.Caption = 'Folder for saved songs:'
      ReadOnly = True
      TabOrder = 5
    end
  end
  object pnlAdvanced: TPanel
    Left = 604
    Top = 4
    Width = 294
    Height = 273
    TabOrder = 2
    Visible = False
    object Label1: TLabel
      Left = 56
      Top = 64
      Width = 40
      Height = 13
      Caption = 'seconds'
    end
    object Label11: TLabel
      Left = 4
      Top = 88
      Width = 69
      Height = 13
      Caption = 'Sound device:'
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
      OnChange = txtMaxRetriesChange
    end
    object txtRetryDelay: TLabeledEdit
      Left = 4
      Top = 60
      Width = 49
      Height = 21
      EditLabel.Width = 101
      EditLabel.Height = 13
      EditLabel.Caption = 'Time between retries:'
      MaxLength = 1
      NumbersOnly = True
      TabOrder = 1
      OnChange = txtRetryDelayChange
    end
    object lstSoundDevice: TComboBox
      Left = 4
      Top = 104
      Width = 277
      Height = 21
      Style = csDropDownList
      TabOrder = 2
    end
  end
  object pnlPlugins: TPanel
    Left = 4
    Top = 332
    Width = 294
    Height = 273
    TabOrder = 3
    Visible = False
    DesignSize = (
      294
      273)
    object lblAppParams: TLabel
      Left = 4
      Top = 252
      Width = 281
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      AutoSize = False
      Caption = '-'
      WordWrap = True
    end
    object btnBrowseApp: TSpeedButton
      Left = 268
      Top = 180
      Width = 21
      Height = 21
      Hint = 'Browse...'
      Anchors = [akRight, akBottom]
      Enabled = False
      Flat = True
      Layout = blGlyphRight
      ParentShowHint = False
      ShowHint = True
      OnClick = btnBrowseAppClick
    end
    object btnHelp: TPngSpeedButton
      Left = 265
      Top = 72
      Width = 24
      Height = 25
      Hint = 'Info...'
      Anchors = [akRight, akBottom]
      Layout = blGlyphRight
      ParentShowHint = False
      ShowHint = True
      OnClick = btnHelpClick
      PngOptions = [pngBlendOnDisabled, pngGrayscaleOnDisabled]
    end
    object btnMoveDown: TPngSpeedButton
      Left = 265
      Top = 28
      Width = 24
      Height = 25
      Anchors = [akTop, akRight]
      Layout = blGlyphRight
      OnClick = btnMoveClick
      PngOptions = [pngBlendOnDisabled, pngGrayscaleOnDisabled]
    end
    object btnMoveUp: TPngSpeedButton
      Left = 265
      Top = 0
      Width = 24
      Height = 25
      Anchors = [akTop, akRight]
      Layout = blGlyphRight
      OnClick = btnMoveClick
      PngOptions = [pngBlendOnDisabled, pngGrayscaleOnDisabled]
    end
    object lstPlugins: TListView
      Left = 4
      Top = 1
      Width = 257
      Height = 96
      Align = alCustom
      Anchors = [akLeft, akTop, akRight, akBottom]
      Checkboxes = True
      Columns = <
        item
          Width = 200
        end>
      ReadOnly = True
      RowSelect = True
      ShowColumnHeaders = False
      SmallImages = ImageList1
      TabOrder = 0
      ViewStyle = vsReport
      OnCompare = lstPluginsCompare
      OnResize = lstPluginsResize
      OnSelectItem = lstPluginsSelectItem
    end
    object cmdConfigure: TBitBtn
      Left = 57
      Top = 28
      Width = 84
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Configure...'
      DoubleBuffered = True
      Enabled = False
      ParentDoubleBuffered = False
      TabOrder = 1
      Visible = False
    end
    object btnAddUp: TButton
      Left = 96
      Top = 104
      Width = 93
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Add...'
      TabOrder = 2
      OnClick = btnAddUpClick
    end
    object btnRemove: TButton
      Left = 196
      Top = 104
      Width = 93
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Remove'
      Enabled = False
      TabOrder = 3
      OnClick = btnRemoveClick
    end
    object txtApp: TLabeledEdit
      Left = 4
      Top = 180
      Width = 257
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      AutoSize = False
      Color = 15790320
      EditLabel.Width = 91
      EditLabel.Height = 13
      EditLabel.Caption = 'Path to application:'
      Enabled = False
      ReadOnly = True
      TabOrder = 4
    end
    object txtAppParams: TLabeledEdit
      Left = 4
      Top = 224
      Width = 285
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      EditLabel.Width = 56
      EditLabel.Height = 13
      EditLabel.Caption = 'Parameters:'
      Enabled = False
      TabOrder = 5
      OnChange = txtAppParamsChange
    end
    object chkOnlyIfCut: TCheckBox
      Left = 4
      Top = 136
      Width = 285
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'Use only if file was cut successfully'
      TabOrder = 6
      OnClick = chkOnlyIfCutClick
    end
  end
  object pnlCut: TPanel
    Left = 604
    Top = 284
    Width = 293
    Height = 273
    TabOrder = 4
    Visible = False
    DesignSize = (
      293
      273)
    object Label4: TLabel
      Left = 56
      Top = 48
      Width = 40
      Height = 13
      Caption = 'seconds'
    end
    object Label5: TLabel
      Left = 56
      Top = 220
      Width = 40
      Height = 13
      Caption = 'seconds'
    end
    object Label10: TLabel
      Left = 20
      Top = 96
      Width = 186
      Height = 13
      Caption = 'Silence is defined by volume lower than'
    end
    object Label12: TLabel
      Left = 20
      Top = 140
      Width = 30
      Height = 13
      Caption = 'lasting'
    end
    object Label13: TLabel
      Left = 144
      Top = 140
      Width = 56
      Height = 13
      Caption = 'ms (min. 20)'
    end
    object Label14: TLabel
      Left = 104
      Top = 116
      Width = 33
      Height = 13
      Caption = '(1-100)'
    end
    object lblPanelCut: TLabel
      Left = 232
      Top = 252
      Width = 53
      Height = 13
      Alignment = taCenter
      Caption = 'lblPanelCut'
    end
    object Label6: TLabel
      Left = 20
      Top = 164
      Width = 59
      Height = 13
      Caption = 'in a range of'
    end
    object Label15: TLabel
      Left = 160
      Top = 164
      Width = 40
      Height = 13
      Caption = 'seconds'
    end
    object txtSongBuffer: TLabeledEdit
      Left = 4
      Top = 216
      Width = 49
      Height = 21
      EditLabel.Width = 242
      EditLabel.Height = 13
      EditLabel.Caption = 'If no silence was found, append buffer to start/end:'
      MaxLength = 2
      NumbersOnly = True
      TabOrder = 0
      OnChange = txtSongBufferChange
    end
    object txtShortLengthSeconds: TLabeledEdit
      Left = 4
      Top = 44
      Width = 49
      Height = 21
      EditLabel.Width = 129
      EditLabel.Height = 13
      EditLabel.Caption = 'Ads to skip are shorter than'
      MaxLength = 3
      NumbersOnly = True
      TabOrder = 1
      OnChange = txtShortLengthSecondsChange
    end
    object chkSkipShort: TCheckBox
      Left = 4
      Top = 0
      Width = 225
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Skip ads (short songs)'
      TabOrder = 2
      OnClick = chkSkipShortClick
    end
    object chkSearchSilence: TCheckBox
      Left = 4
      Top = 72
      Width = 225
      Height = 21
      Caption = 'Search for silence before saving tracks'
      TabOrder = 3
      OnClick = chkSearchSilenceClick
    end
    object txtSilenceLevel: TEdit
      Left = 20
      Top = 112
      Width = 81
      Height = 21
      MaxLength = 3
      NumbersOnly = True
      TabOrder = 4
      OnChange = txtSilenceLevelChange
    end
    object txtSilenceLength: TEdit
      Left = 60
      Top = 136
      Width = 81
      Height = 21
      MaxLength = 4
      NumbersOnly = True
      TabOrder = 5
      OnChange = txtSilenceLengthChange
    end
    object txtSilenceBufferSeconds: TEdit
      Left = 88
      Top = 160
      Width = 69
      Height = 21
      MaxLength = 2
      NumbersOnly = True
      TabOrder = 6
      OnChange = txtSilenceBufferSecondsChange
    end
  end
  object pnlHotkeys: TPanel
    Left = 304
    Top = 420
    Width = 293
    Height = 185
    TabOrder = 5
    Visible = False
    DesignSize = (
      293
      185)
    object Label9: TLabel
      Left = 4
      Top = 144
      Width = 37
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Hotkey:'
    end
    object lstHotkeys: TListView
      Left = 4
      Top = 4
      Width = 285
      Height = 129
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
        end
        item
        end>
      RowSelect = True
      ShowColumnHeaders = False
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = lstHotkeysChange
    end
    object txtHotkey: THotKey
      Left = 4
      Top = 160
      Width = 285
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      Enabled = False
      HotKey = 32833
      TabOrder = 1
      OnChange = txtHotkeyChange
    end
  end
  object pnlCommunity: TPanel
    Left = 904
    Top = 4
    Width = 294
    Height = 273
    TabOrder = 6
    Visible = False
    DesignSize = (
      294
      273)
    object Label2: TLabel
      Left = 20
      Top = 140
      Width = 253
      Height = 29
      AutoSize = False
      Caption = 
        'Streams unknown to streamWriter will be submitted to the server ' +
        'so they can appear in the browser.'
      WordWrap = True
    end
    object Label8: TLabel
      Left = 20
      Top = 196
      Width = 253
      Height = 69
      AutoSize = False
      Caption = 
        'Information about Track changes will be sent to the server and b' +
        'roadcasted to other users. This is important for the automatic r' +
        'ecording of songs on the wishlist.'
      WordWrap = True
    end
    object Label16: TLabel
      Left = 20
      Top = 24
      Width = 76
      Height = 13
      Caption = 'Minimum bitrate:'
    end
    object Label17: TLabel
      Left = 20
      Top = 68
      Width = 35
      Height = 13
      Caption = 'Format:'
    end
    object chkAutoTuneIn: TCheckBox
      Left = 4
      Top = 0
      Width = 285
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Tune into stations when a wishlist'#39's song is playing'
      TabOrder = 0
      OnClick = chkAutoTuneInClick
    end
    object chkSubmitStreamInfo: TCheckBox
      Left = 4
      Top = 116
      Width = 285
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Submit information about streams to the server'
      TabOrder = 1
    end
    object chkSubmitStats: TCheckBox
      Left = 4
      Top = 172
      Width = 285
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Submit statistics/track changes to the server'
      TabOrder = 2
    end
    object lstMinBitrate: TComboBox
      Left = 20
      Top = 40
      Width = 129
      Height = 21
      Style = csDropDownList
      TabOrder = 3
      Items.Strings = (
        '32'
        '64'
        '96'
        '128'
        '160'
        '192'
        '224'
        '256'
        '320'
        '384')
    end
    object lstFormat: TComboBox
      Left = 20
      Top = 84
      Width = 129
      Height = 21
      Style = csDropDownList
      TabOrder = 4
      Items.Strings = (
        'MP3/AAC'
        'MP3'
        'AAC')
    end
  end
  object dlgOpen: TOpenDialog
    Filter = 'Executable files (*.exe, *.bat)|*.exe;*.bat'
    Left = 548
    Top = 272
  end
  object ImageList1: TImageList
    Left = 536
    Top = 112
    Bitmap = {
      494C010102000800440110001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B25E2AFFAA5A28FFA657
      27FFA15426FF9B552AF700000000000000008C4B22FD894921FD8B4D28F49057
      34E6000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000B65F2BFFC98459FFC882
      56FFC77F53FFA35526FF0000000000000000924D22FFC57A4CFFC57748FF8646
      1FFF000000000000000000000000000000000000000082828285828282858282
      8285828282858282828582828285828282858282828582828285828282858282
      8285828282858282828582828285000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BE642CFFCA855CFFC067
      30FFC98459FFC06730FFA35526FF9E5325FFB55F2BFFC67B4DFFC57A4CFF8948
      20FF0000000000000000000000000000000000000000383838FF3F3F3FFF3E3E
      3EFF3E3E3EFF4C4747FF544D4DFF585050FF5B5252FF5C5454FF5D5555FF5E55
      55FF5E5555FF5E5555FF514D4DFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C06832FFCB8960FFCA85
      5CFFC37341FFC98459FFC88256FFC88256FFC77F53FFC06832FFC77E52FF8E4A
      21FF0000000000000000000000000000000000000000404040FF4E4E4EFF4A4A
      4AFF474747FF534D4DFF675A5AFF6C5E5EFF706161FF746464FF776666FF7766
      66FF776666FF776666FF5E5555FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C26E3BFFC06832FFC375
      45FFCB8960FFC06832FFC06730FFBF652DFFBB622CFFB7602CFFC77F53FFB35E
      2BFF904E26F98C4A21FF88471FFF0000000000000000424242FF555555FF5151
      51FF4C4C4CFF4C4B4BFF5D5353FF635757FF695B5BFF6E5F5FFF726262FF7665
      65FF776666FF776666FF5E5555FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C16A
      35FFCB8A62FFC16B37FFC06832FFC06832FFC06730FFBB622CFFC26E3BFFC77F
      53FFC77E52FFC77E52FF8C4A21FF0000000000000000454545FF5D5D5DFF5858
      58FF535353FF4F4F4FFF524D4DFF5A5050FF605555FF665959FF6B5D5DFF6F61
      61FF746363FF766666FF5D5454FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C36F
      3CFFCC8C65FFC36F3CFFC16C39FFC16B37FFC06832FFC06730FFC36F3CFFC882
      56FFC77F53FFC77F53FF904C22FF0000000000000000474747FF545454FF6060
      60FF5B5B5BFF4C4C4CFF4D4D4DFF514A4AFF574E4EFF5D5252FF625656FF685B
      5BFF6D5F5FFF716262FF5A5252FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C5784AFFC47645FFC67C
      4FFFCD8E68FFC37341FFC3703EFFC16C39FFC16C39FFC06832FFCA855CFFC067
      30FFA35526FF9C5224FF954F23FF00000000000000004A4A4AFFBFBFBFFF7777
      77FF5F5F5FFFBFBFBFFF8C8C8CFF514F4FFF4D4747FF534B4BFF594F4FFF5F54
      54FF655858FF6A5C5CFF554E4EFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C67C4FFFCF936FFFCD90
      6BFFC77F53FFC47645FFC37341FFC37341FFC37341FFC37140FFCB8960FFAD5B
      29FF00000000000000000000000000000000000000004C4C4CFF5B5B5BFFC5C5
      C5FF888888FF666666FF616161FF5C5C5CFF504E4EFF4A4444FF504848FF564D
      4DFF5C5151FF615656FF504949FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C77F53FFCF9471FFCF93
      6FFFCD906BFFCD906BFFCD8E68FFCD8E68FFCC8B63FFCB8960FFCA855CFFB35E
      2BFF00000000000000000000000000000000000000004D4D4DFFCBCBCBFF8B8B
      8BFF727272FF6E6E6EFF696969FF646464FF5F5F5FFF525151FF484343FF4C46
      46FF524A4AFF584E4EFF4A4444FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C88256FFC77F53FFC67C
      4FFFC67B4DFFC5784AFECD906BFFCD8E68FFC57646F0C47240EDC36E3AEFBD6A
      39EF0000000000000000000000000000000000000000383838FF4D4D4DFF4D4D
      4DFF4C4C4CFF4A4A4AFF494949FF484848FF464646FF444444FF3F3F3FFF3937
      37FF3D3939FF413D3DFF3C3939FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C88054F4CE916CFFCC8C65FFD29570BE00000000000000000000
      00000000000000000000000000000000000000000000BBBBBBFFBABABAFFB8B8
      B8FFB6B6B6FFB3B3B3FFB0B0B0FFAEAEAEFFABABABFFA8A8A8FFA6A6A6FFA3A3
      A3FFA0A0A0FF9E9E9EFF9C9C9CFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C77F53FFCF9471FFCE916CFFC88052EC00000000000000000000
      00000000000000000000000000000000000000000000CFCFCFD4DCDCDCFFD8D9
      D9FFD5D5D5FFD0D1D1FFCCCCCCFFC8C8C8FFC6C6C6FFC6C5C5FFC9C5C5FFCDC6
      C6FFD1C7C7FFD7CBCBFFCDC3C3D4000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000D19772D4C77F53FFC67C4FFED69F7FB400000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF00000000830FFFFF00000000
      830F800100000000800F800100000000800F8001000000008001800100000000
      E001800100000000E0018001000000008001800100000000800F800100000000
      800F800100000000800F800100000000F87F800100000000F87F800100000000
      F87FFFFF00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
  object PngImageList1: TPngImageList
    PngImages = <>
    Left = 644
    Top = 580
  end
end
