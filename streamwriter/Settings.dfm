object frmSettings: TfrmSettings
  Left = 350
  Top = 283
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Settings'
  ClientHeight = 793
  ClientWidth = 1201
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
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pnlStreams: TPanel
    Left = 304
    Top = 4
    Width = 293
    Height = 377
    TabOrder = 0
    Visible = False
    DesignSize = (
      293
      377)
    object lblDefaultFilter: TLabel
      Left = 4
      Top = 336
      Width = 24
      Height = 13
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'Lists:'
    end
    object btnBrowse: TSpeedButton
      Left = 264
      Top = 20
      Width = 25
      Height = 21
      Hint = 'Browse...'
      Anchors = [akTop, akRight]
      Flat = True
      Layout = blGlyphRight
      ParentShowHint = False
      ShowHint = True
      OnClick = btnBrowseClick
    end
    object btnBrowseAuto: TSpeedButton
      Left = 264
      Top = 64
      Width = 25
      Height = 21
      Hint = 'Browse...'
      Anchors = [akTop, akRight]
      Flat = True
      Layout = blGlyphRight
      ParentShowHint = False
      ShowHint = True
      OnClick = btnBrowseClick
    end
    object chkDeleteStreams: TCheckBox
      Left = 4
      Top = 168
      Width = 265
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Delete stream files when recording stops'
      TabOrder = 0
      OnClick = chkDeleteStreamsClick
    end
    object chkAddSavedToIgnore: TCheckBox
      Left = 4
      Top = 192
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Add saved titles to global ignorelist'
      TabOrder = 1
      OnClick = chkAddSavedToIgnoreClick
    end
    object lstDefaultFilter: TComboBox
      Left = 4
      Top = 352
      Width = 213
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 2
      OnChange = lstDefaultFilterChange
      Items.Strings = (
        'Save every song'
        'Use wishlist'
        'Use global ignorelist'
        'Use stream ignorelist'
        'Use both ignorelists'
        'Use all lists')
    end
    object chkSeparateTracks: TCheckBox
      Left = 4
      Top = 120
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Save separated tracks'
      TabOrder = 3
      OnClick = chkSeparateTracksClick
    end
    object chkSaveStreamsToMemory: TCheckBox
      Left = 4
      Top = 96
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Save received data to memory instead of disk'
      TabOrder = 4
      OnClick = chkSaveStreamsToMemoryClick
    end
    object chkOnlySaveFull: TCheckBox
      Left = 20
      Top = 144
      Width = 265
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Only save whole songs'
      TabOrder = 5
      OnClick = chkOnlySaveFullClick
    end
    object chkOverwriteSmaller: TCheckBox
      Left = 4
      Top = 264
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Overwrite existing file if newer file is larger'
      TabOrder = 6
      OnClick = chkOverwriteSmallerClick
    end
    object chkDiscardSmaller: TCheckBox
      Left = 4
      Top = 288
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Discard new file if existing one is lager'
      TabOrder = 7
      OnClick = chkDiscardSmallerClick
    end
    object txtDir: TLabeledEdit
      Left = 4
      Top = 20
      Width = 257
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Color = 15790320
      EditLabel.Width = 110
      EditLabel.Height = 13
      EditLabel.Caption = 'Folder for saved songs:'
      ReadOnly = True
      TabOrder = 8
    end
    object txtDirAuto: TLabeledEdit
      Left = 4
      Top = 64
      Width = 257
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Color = 15790320
      EditLabel.Width = 174
      EditLabel.Height = 13
      EditLabel.Caption = 'Folder for automatically saved songs:'
      ReadOnly = True
      TabOrder = 9
    end
    object chkAddSavedToStreamIgnore: TCheckBox
      Left = 4
      Top = 216
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Add saved titles to stream ignorelist'
      TabOrder = 10
      OnClick = chkAddSavedToStreamIgnoreClick
    end
    object chkRemoveSavedFromWishlist: TCheckBox
      Left = 4
      Top = 240
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Remove saved titles from wishlist'
      TabOrder = 11
      OnClick = chkRemoveSavedFromWishlistClick
    end
  end
  object pnlMain: TPanel
    Left = 4
    Top = 4
    Width = 293
    Height = 341
    TabOrder = 1
    Visible = False
    DesignSize = (
      293
      341)
    object Label7: TLabel
      Left = 56
      Top = 172
      Width = 15
      Height = 13
      Caption = 'GB'
    end
    object Label3: TLabel
      Left = 4
      Top = 252
      Width = 190
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Default action on doubleclick on stream:'
    end
    object Label18: TLabel
      Left = 4
      Top = 300
      Width = 192
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Default action on doubleclick in browser:'
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
      Top = 168
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
      Top = 268
      Width = 213
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akBottom]
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
    object lstDefaultActionBrowser: TComboBox
      Left = 4
      Top = 316
      Width = 213
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akBottom]
      TabOrder = 5
      Items.Strings = (
        'Start recording'
        'Listen to stream'
        'Listen to stream (external player)')
    end
    object chkSnapMain: TCheckBox
      Left = 4
      Top = 72
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Snap mainwindow to screen edges'
      TabOrder = 6
    end
    object chkRememberRecordings: TCheckBox
      Left = 4
      Top = 96
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Remember streams that were recording on exit'
      TabOrder = 7
    end
    object chkDisplayPlayNotifications: TCheckBox
      Left = 4
      Top = 120
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Display notifications on trackchange when playing'
      TabOrder = 8
    end
  end
  object pnlAdvanced: TPanel
    Left = 604
    Top = 4
    Width = 293
    Height = 257
    TabOrder = 2
    Visible = False
    object Label1: TLabel
      Left = 56
      Top = 68
      Width = 40
      Height = 13
      Caption = 'seconds'
    end
    object lblSoundDevice: TLabel
      Left = 4
      Top = 92
      Width = 69
      Height = 13
      Caption = 'Sound device:'
    end
    object txtMaxRetries: TLabeledEdit
      Left = 4
      Top = 20
      Width = 49
      Height = 21
      EditLabel.Width = 210
      EditLabel.Height = 13
      EditLabel.Caption = 'Max. connect retries on error (zero is infinite):'
      MaxLength = 3
      NumbersOnly = True
      TabOrder = 0
      OnChange = txtMaxRetriesChange
    end
    object txtRetryDelay: TLabeledEdit
      Left = 4
      Top = 64
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
      Top = 108
      Width = 277
      Height = 21
      Style = csDropDownList
      TabOrder = 2
    end
  end
  object pnlPlugins: TPanel
    Left = 4
    Top = 388
    Width = 294
    Height = 257
    TabOrder = 3
    Visible = False
    DesignSize = (
      294
      257)
    object lblAppParams: TLabel
      Left = 4
      Top = 236
      Width = 281
      Height = 17
      Anchors = [akLeft, akRight, akBottom]
      AutoSize = False
      Caption = '-'
      WordWrap = True
      ExplicitTop = 252
    end
    object btnBrowseApp: TSpeedButton
      Left = 264
      Top = 164
      Width = 25
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
      Left = 266
      Top = 56
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
      Left = 266
      Top = 28
      Width = 24
      Height = 25
      Anchors = [akTop, akRight]
      Layout = blGlyphRight
      OnClick = btnMoveClick
      PngOptions = [pngBlendOnDisabled, pngGrayscaleOnDisabled]
    end
    object btnMoveUp: TPngSpeedButton
      Left = 266
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
      Height = 80
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
      OnItemChecked = lstPluginsItemChecked
    end
    object btnAddUp: TButton
      Left = 96
      Top = 88
      Width = 93
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Add...'
      TabOrder = 1
      OnClick = btnAddUpClick
    end
    object btnRemove: TButton
      Left = 196
      Top = 88
      Width = 93
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Remove'
      Enabled = False
      TabOrder = 2
      OnClick = btnRemoveClick
    end
    object txtApp: TLabeledEdit
      Left = 4
      Top = 164
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
      TabOrder = 3
    end
    object txtAppParams: TLabeledEdit
      Left = 4
      Top = 208
      Width = 285
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      EditLabel.Width = 56
      EditLabel.Height = 13
      EditLabel.Caption = 'Parameters:'
      Enabled = False
      TabOrder = 4
      OnChange = txtAppParamsChange
    end
    object chkOnlyIfCut: TCheckBox
      Left = 4
      Top = 120
      Width = 285
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'Use only if file was cut successfully'
      TabOrder = 5
      OnClick = chkOnlyIfCutClick
    end
    object btnConfigure: TButton
      Left = 4
      Top = 88
      Width = 93
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = '&Configure...'
      TabOrder = 6
      OnClick = btnConfigureClick
    end
  end
  object pnlCut: TPanel
    Left = 604
    Top = 264
    Width = 293
    Height = 357
    TabOrder = 4
    Visible = False
    DesignSize = (
      293
      357)
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
      Top = 240
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
    object chkAdjustTrackOffset: TCheckBox
      Left = 4
      Top = 248
      Width = 281
      Height = 21
      Caption = 'Adjust offset of detected track changes'
      TabOrder = 7
      OnClick = chkAdjustTrackOffsetClick
    end
    object txtAdjustTrackOffset: TLabeledEdit
      Left = 20
      Top = 272
      Width = 61
      Height = 21
      EditLabel.Width = 56
      EditLabel.Height = 13
      EditLabel.Caption = 'milliseconds'
      Enabled = False
      LabelPosition = lpRight
      MaxLength = 4
      NumbersOnly = True
      TabOrder = 8
      OnChange = txtAdjustTrackOffsetChange
    end
    object optAdjustBackward: TRadioButton
      Left = 20
      Top = 296
      Width = 189
      Height = 21
      Caption = 'Before detected change'
      Checked = True
      Enabled = False
      TabOrder = 9
      TabStop = True
      OnClick = optAdjustClick
    end
    object optAdjustForward: TRadioButton
      Left = 20
      Top = 316
      Width = 189
      Height = 21
      Caption = 'After detected change'
      Enabled = False
      TabOrder = 10
      OnClick = optAdjustClick
    end
  end
  object pnlHotkeys: TPanel
    Left = 304
    Top = 388
    Width = 293
    Height = 141
    TabOrder = 5
    Visible = False
    DesignSize = (
      293
      141)
    object Label9: TLabel
      Left = 4
      Top = 104
      Width = 37
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Hotkey:'
    end
    object lstHotkeys: TListView
      Left = 4
      Top = 0
      Width = 285
      Height = 93
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'Action'
        end
        item
          Caption = 'Hotkey'
        end>
      RowSelect = True
      SmallImages = PngImageList1
      TabOrder = 0
      ViewStyle = vsReport
      OnChange = lstHotkeysChange
      OnResize = lstHotkeysResize
    end
    object txtHotkey: THotKey
      Left = 4
      Top = 120
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
    Width = 293
    Height = 337
    TabOrder = 6
    Visible = False
    DesignSize = (
      293
      337)
    object Label2: TLabel
      Left = 20
      Top = 220
      Width = 268
      Height = 29
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'Streams unknown to streamWriter will be submitted to the server ' +
        'so they can appear in the browser.'
      WordWrap = True
    end
    object Label8: TLabel
      Left = 20
      Top = 276
      Width = 268
      Height = 65
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'Information about Track changes will be sent to the server and b' +
        'roadcasted to other users. This is important for the automatic r' +
        'ecording of songs on the wishlist.'
      WordWrap = True
    end
    object Label16: TLabel
      Left = 20
      Top = 100
      Width = 76
      Height = 13
      Caption = 'Minimum bitrate:'
    end
    object Label17: TLabel
      Left = 20
      Top = 144
      Width = 35
      Height = 13
      Caption = 'Format:'
    end
    object chkAutoTuneIn: TCheckBox
      Left = 4
      Top = 0
      Width = 284
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Tune into stations when a wishlist'#39's song is playing'
      TabOrder = 0
      OnClick = chkAutoTuneInClick
    end
    object chkSubmitStreamInfo: TCheckBox
      Left = 4
      Top = 196
      Width = 284
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Submit information about streams to the server'
      TabOrder = 1
    end
    object chkSubmitStats: TCheckBox
      Left = 4
      Top = 252
      Width = 284
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Submit statistics/track changes to the server'
      TabOrder = 2
    end
    object lstMinBitrate: TComboBox
      Left = 20
      Top = 116
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
      Top = 160
      Width = 129
      Height = 21
      Style = csDropDownList
      TabOrder = 4
      Items.Strings = (
        'MP3/AAC'
        'MP3'
        'AAC')
    end
    object chkAutoTuneInConsiderIgnore: TCheckBox
      Left = 20
      Top = 24
      Width = 268
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Do not tune in if song is on global ignorelist'
      TabOrder = 5
      OnClick = chkAutoTuneInClick
    end
    object chkAutoTuneInAddToIgnore: TCheckBox
      Left = 20
      Top = 48
      Width = 269
      Height = 21
      Caption = 'Add saved titles to global ignorelist'
      TabOrder = 6
    end
    object chkAutoRemoveSavedFromWishlist: TCheckBox
      Left = 20
      Top = 72
      Width = 269
      Height = 21
      Caption = 'Remove saved titles from wishlist'
      TabOrder = 7
    end
  end
  object pnlFilenames: TPanel
    Left = 904
    Top = 348
    Width = 293
    Height = 305
    TabOrder = 7
    Visible = False
    DesignSize = (
      293
      305)
    object lblFilePattern: TLabel
      Left = 4
      Top = 4
      Width = 284
      Height = 49
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = '-'
      WordWrap = True
      ExplicitWidth = 285
    end
    object btnResetFilePattern: TPngSpeedButton
      Left = 264
      Top = 68
      Width = 25
      Height = 21
      Hint = 'Reset pattern to default'
      Anchors = [akTop, akRight]
      Flat = True
      Layout = blGlyphRight
      ParentShowHint = False
      ShowHint = True
      OnClick = btnResetFilePatternClick
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
    object btnResetIncompleteFilePattern: TPngSpeedButton
      Left = 264
      Top = 116
      Width = 25
      Height = 21
      Hint = 'Reset pattern to default'
      Anchors = [akTop, akRight]
      Flat = True
      Layout = blGlyphRight
      ParentShowHint = False
      ShowHint = True
      OnClick = btnResetFilePatternClick
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
    object btnResetAutomaticFilePattern: TPngSpeedButton
      Left = 264
      Top = 164
      Width = 25
      Height = 21
      Hint = 'Reset pattern to default'
      Anchors = [akTop, akRight]
      Flat = True
      Layout = blGlyphRight
      ParentShowHint = False
      ShowHint = True
      OnClick = btnResetFilePatternClick
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
    object btnResetStreamFilePattern: TPngSpeedButton
      Left = 264
      Top = 212
      Width = 25
      Height = 21
      Hint = 'Reset pattern to default'
      Anchors = [akTop, akRight]
      Flat = True
      Layout = blGlyphRight
      ParentShowHint = False
      ShowHint = True
      OnClick = btnResetFilePatternClick
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
    object txtFilePattern: TLabeledEdit
      Left = 4
      Top = 68
      Width = 257
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 241
      EditLabel.Height = 13
      EditLabel.Caption = 'Pattern for filenames of completely recorded tracks:'
      TabOrder = 0
      OnChange = txtFilePatternChange
      OnClick = txtFilePatternClick
    end
    object txtPreview: TLabeledEdit
      Left = 4
      Top = 260
      Width = 256
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = 15790320
      EditLabel.Width = 135
      EditLabel.Height = 13
      EditLabel.Caption = 'Preview for selected pattern:'
      ReadOnly = True
      TabOrder = 1
    end
    object txtIncompleteFilePattern: TLabeledEdit
      Left = 4
      Top = 116
      Width = 257
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 249
      EditLabel.Height = 13
      EditLabel.Caption = 'Pattern for filenames of incompletely recorded tracks:'
      TabOrder = 2
      OnChange = txtFilePatternChange
      OnClick = txtFilePatternClick
    end
    object txtAutomaticFilePattern: TLabeledEdit
      Left = 4
      Top = 164
      Width = 257
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 252
      EditLabel.Height = 13
      EditLabel.Caption = 'Pattern for filenames of automatically recorded tracks:'
      TabOrder = 3
      OnChange = txtFilePatternChange
      OnClick = txtFilePatternClick
    end
    object txtStreamFilePattern: TLabeledEdit
      Left = 4
      Top = 212
      Width = 257
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 295
      EditLabel.Height = 13
      EditLabel.Caption = 
        'Pattern for filenames of stream files (%a, %t and %n are invalid' +
        '):'
      TabOrder = 4
      OnChange = txtStreamFilePatternChange
      OnClick = txtStreamFilePatternClick
    end
  end
  object pnlCommunityBlacklist: TPanel
    Left = 4
    Top = 652
    Width = 293
    Height = 137
    TabOrder = 8
    Visible = False
    DesignSize = (
      293
      137)
    object Label19: TLabel
      Left = 4
      Top = 4
      Width = 256
      Height = 13
      Caption = 'Do not automatically record from the following streams:'
    end
    object pnlBlacklist: TPanel
      Left = 4
      Top = 20
      Width = 285
      Height = 81
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvNone
      TabOrder = 0
    end
    object btnBlacklistRemove: TButton
      Left = 196
      Top = 112
      Width = 93
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Remove'
      Enabled = False
      TabOrder = 1
      OnClick = btnBlacklistRemoveClick
    end
  end
  object pnlStreamsAdvanced: TPanel
    Left = 304
    Top = 536
    Width = 293
    Height = 253
    TabOrder = 9
    Visible = False
    DesignSize = (
      293
      253)
    object btnResetTitlePattern: TPngSpeedButton
      Left = 264
      Top = 32
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
      Left = 4
      Top = 64
      Width = 285
      Height = 29
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'Ignore the following stream title changes (i.e. announcements li' +
        'ke '#39'Next playing: ...'#39') to disable saving:'
      WordWrap = True
    end
    object txtTitlePattern: TLabeledEdit
      Left = 4
      Top = 32
      Width = 257
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 276
      EditLabel.Height = 26
      EditLabel.Caption = 
        'Regular expression to detect artist/title (only change if you kn' +
        'ow what you are doing):'
      EditLabel.Layout = tlBottom
      EditLabel.WordWrap = True
      TabOrder = 0
      OnChange = txtTitlePatternChange
    end
    object lstIgnoreTitles: TListView
      Left = 4
      Top = 95
      Width = 285
      Height = 78
      Align = alCustom
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
        end>
      RowSelect = True
      ShowColumnHeaders = False
      SmallImages = PngImageList1
      TabOrder = 1
      ViewStyle = vsReport
      OnChange = lstIgnoreTitlesChange
      OnEdited = lstIgnoreTitlesEdited
      OnResize = lstIgnoreTitlesResize
    end
    object btnRemoveIgnoreTitlePattern: TButton
      Left = 196
      Top = 228
      Width = 93
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Remove'
      Enabled = False
      TabOrder = 2
      OnClick = btnRemoveIgnoreTitlePatternClick
    end
    object btnAddIgnoreTitlePattern: TButton
      Left = 100
      Top = 228
      Width = 93
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&Add'
      Enabled = False
      TabOrder = 3
      OnClick = btnAddIgnoreTitlePatternClick
    end
    object txtIgnoreTitlePattern: TLabeledEdit
      Left = 4
      Top = 200
      Width = 285
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      EditLabel.Width = 174
      EditLabel.Height = 13
      EditLabel.Caption = 'Pattern to ignore (use '#39'*'#39' as wildcard):'
      TabOrder = 4
      OnChange = txtIgnoreTitlePatternChange
    end
  end
  object pnlBandwidth: TPanel
    Left = 604
    Top = 628
    Width = 293
    Height = 161
    TabOrder = 10
    Visible = False
    DesignSize = (
      293
      161)
    object Label11: TLabel
      Left = 76
      Top = 44
      Width = 24
      Height = 13
      Caption = 'KB/s'
    end
    object txtMaxSpeed: TLabeledEdit
      Left = 20
      Top = 40
      Width = 53
      Height = 21
      EditLabel.Width = 218
      EditLabel.Height = 13
      EditLabel.Caption = 'Maximum bandwidth available to streamWriter:'
      Enabled = False
      MaxLength = 5
      NumbersOnly = True
      TabOrder = 0
    end
    object chkLimit: TCheckBox
      Left = 4
      Top = 0
      Width = 285
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Limit bandwidth for recordings'
      TabOrder = 1
      OnClick = chkLimitClick
    end
  end
  object pnlFilenamesExt: TPanel
    Left = 904
    Top = 660
    Width = 293
    Height = 129
    TabOrder = 11
    DesignSize = (
      293
      129)
    object btnResetRemoveChars: TPngSpeedButton
      Left = 264
      Top = 68
      Width = 25
      Height = 21
      Hint = 'Reset pattern to default'
      Anchors = [akTop, akRight]
      Flat = True
      Layout = blGlyphRight
      ParentShowHint = False
      ShowHint = True
      OnClick = btnResetRemoveCharsClick
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
    object txtRemoveChars: TLabeledEdit
      Left = 4
      Top = 68
      Width = 257
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 228
      EditLabel.Height = 13
      EditLabel.Caption = 'Remove the following characters from filenames:'
      TabOrder = 0
      OnChange = txtRemoveCharsChange
    end
    object txtFilePatternDecimals: TLabeledEdit
      Left = 4
      Top = 20
      Width = 41
      Height = 21
      EditLabel.Width = 212
      EditLabel.Height = 13
      EditLabel.Caption = 'Minimum count of decimals for tracknumbers:'
      MaxLength = 1
      NumbersOnly = True
      TabOrder = 1
      OnChange = txtFilePatternDecimalsChange
    end
    object chkNormalizeVariables: TCheckBox
      Left = 4
      Top = 100
      Width = 284
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Adjust upper-/lowercase of artist/title'
      TabOrder = 2
      OnClick = chkNormalizeVariablesClick
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
      494C010102000800940210001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
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
    PngImages = <
      item
        Background = clWindow
        Name = 'keyboard'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000024E4944415478DAA5933F68DA4114C79F7FA306153A08D1
          45A40E920CA268030E191CA411028238D42D9BA6A9A9ADB66A4208FE6BB57FB4
          367111373B482020687170C810B01992252248292E12885050311A35DABB37B5
          4B11F2E078C7FDEE7DEEFBFBDE3BC66C3683870483026C369B82CC9F90219CB3
          AE47C68F7C3EFF0B0156ABF5592010F04924929579AA6F6E6EAEC2E170F4F8F8
          F81B022C168B636F6FEF68696969AEE3AFAFAF21180C3A4F4E4ED208D8D8D870
          EEEFEF1F76BB5DD0EBF5C0E17080C562E1A0319D4E613299C0783C86F3F37310
          8944707070B05528148E1060369B9F8742A154AFD7039D4E07D56A156AB51AAC
          AEAE8256ABC5C2BBBB3B045380502884DDDDDDED62B1F815012693693B168B7D
          E9F7FBA056AB21914880DBED864C26034EA71301C3E110B85C2E5C5E5EC2E2E2
          2278BDDE17E572398500A3D1E84A269389DBDB5B585E5E86783C0E0A8502DAED
          36B85CAE7F0054994020A0EB3B954A258980B5B5B597E974FA13DDA4542A211A
          8D82CFE783542A051E8F0746A3110C0603E0F178D06834303B1C0EF7E9E9E967
          04180C8657D96CF603FD4FB95C0E54093D656161014DFC1BD06C36717D7373F3
          F5D9D9D9470410E73DB95C2E46A5CA6432A05E50002DA041C114C0E7F3A1D56A
          A19976BBDD4B0C8D2340A3D1BC214DF18E5E15BD32A9548A9BD86C3602EEEFEF
          D1075ACC6432719D34DFDB8B8B8BF70820C6F94AA552A45EAFCFD5482A950AD6
          D7D7FDC4D0280288EC9D4824B245F263AAE07F411510253FFD7EFF21C9090488
          C5E21552F8947C7F34E763FA4D40DF3B9DCE15E3A1CFF90F7E7F33F08269B48E
          0000000049454E44AE426082}
      end
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
      end
      item
        Background = clWindow
        Name = 'list-add'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000017352474200AECE1CE900000006624B474400FF00FF00FFA0BDA793
          000000097048597300000B1300000B1301009A9C180000000774494D4507DA06
          0D091303F56C9333000002DB4944415478DA75536B485361187EBE733677CBDB
          B6BC2C2DCCE10F2B2F1564FE28BAD00FA1C82E988159E81FD30AD3C2148D2C10
          B1D440EC5F38BBAA2111DD142AB454FA91D518CB426D611AA9CD79D9D0EDEC9C
          CEF9066B527DF072F8E07CCFFB3ECFFB3C04F90CE061016F10001E29F3A55006
          EBA1D1C64210BC10782F185606D7CC04E6A7BF81611898492BE016DF712C880F
          80882543CA422954A19168AE3A8AC43561083CDFA75DC8AD6C8363DC0A8B6012
          1B2AC47E1440ECEEE6C51290EC2C4784310D8F6B3350DF6111AC3607E42C8141
          AF46454E2A29BADE8F81EE36583DB701460510990F20F5572578CE4D470E8FD9
          80270D59C8ADE911266C43747C759801EDD57B48417D1F3EF4B48B3464742A2B
          FF1064D3C16A2843221019130FD722878D097A5CCC4D457EDD1BE1EBC8671046
          8E50FD2AB4946D23F75E8CA0F3A5057285067393A35898B6816C3DD684E6CA23
          CB38CF3A3D286CEC137E8C8D5080109D018D45696475E40AFF3F63534EE49499
          40769EEAC0B36B9994F3ABC17128837CE3CDCE4CD10D1086A534620C51502958
          701C8F7571E128CA4C247B2F7481EC28BC8FE70D8728E7914FEFC0CA14F4112B
          5788A5A2608410511FC1DF3D217E359ACEA4938CD2477F004ED4BE1686AD8322
          805C7CC1FC1340F200CFF3CB01769D7E80A757F7E392E9BDD03FF8057265309D
          60C939B38C8256A78742CEC2C37991B4568BF3D949645F793748FAF11B305DCE
          8624906B8983D72BD06F49F35B2A22C30621581B8DBA822D44F2838C65A837EC
          F36E1C28BEE95BA32A2C1A46630216DD1C928D3A9C3DBCFEAF35DEAAD84E3A7B
          6D904AAD944102774C0C8946CA63906AAFFAAF91244D2423DDADDA4D4E36F6C3
          DC1B6024AED30740ADECC1322BD7DCF9289847ED54C0D8951A5CC9DF4C8A9B06
          D0D7255AD9DD1A60E53CE20B132785E91CD4DA18BF268147E29C55D606FB9819
          16BEC5975E1A260A1018E7126874B1D045C5C1CB0BFE353A266D98FB394CEF66
          886974131AE7DF14233FBBCEB8CA430000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'cross-script'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000017352474200AECE1CE900000006624B474400FF00FF00FFA0BDA793
          000000097048597300000B1300000B1301009A9C180000000774494D4507DA0B
          11121B1FDB982573000002504944415478DAAD934B6813511486CFA4790C4393
          34D2D176B048EBA24234D2461ADD145A4445978A2F280842B3D00A62BBF0B151
          44295AB4B8D22C74E3424454100B2E5A77421B0B959AA4514A5DB4D1511347DA
          BC6692EB3F3337524457F5C261EEDC39E73BE7BFFC2330C6682D4BF82F804782
          4002D13EBC5FE5E76FAA44033A36458413E1B69F41E41DC4B613F1F23063F72C
          C04300B0E2470707C3C54281CD4D4D65929393990AD18E1207B888865D6EF71E
          A35CCE227B044763C7516C011ED8807E88E90F4622CDADC1A052CAE7F5F8F8F8
          C7AFAADAE7F7F94ECB8AD2BD904A2591790AB04F660500F41B60DE4499288CD1
          8F60DB1BE9E969AB6F68F0CF27124B1E51A4D4CCCC342689A2F8B3CBD4FE0F80
          A519A387917037D4D1D14E0E87E74322912E140A5BCDA2DA956F580DB86C4BB0
          96834846A7589BA244EA45715D7A71712EE0F5FAB39AA6ADE87AC86C62F0DCEB
          35C0790EA8B363748B2C1F6A12C5A6B8AA26574AA55BE87A60BD2485D47C3E8D
          E2FDD53F01673900DAA2AD5EEF5097246D4E148B4BEF35EDB687E88661771D6D
          743A777F338CE79078C1943252030CD83ED88478725296B7FF348CD2E35CEE35
          E4F4212F27F2FB01E40A1E5D281E464CDCA901A2F604377749D2B16E5154629A
          369BAB544EA8446F9FF2717772F718B6D95C907131C69861019A01D80B235D0A
          043A67753DFB6C79F9FE2BA2A1CC5FAC6B42B6119DFB8209C6189BB60082206C
          C487DE76A23320FF9820BA86EEF3C8CD234C3356F91D9B1670B710B5341035BE
          63EC450DE0E37677F146152E5BE77BC6BD53B72A1CA8FDBEE6BFF11738CD0DF0
          5FE2B5BF0000000049454E44AE426082}
      end>
    Left = 460
    Top = 144
    Bitmap = {}
  end
end
