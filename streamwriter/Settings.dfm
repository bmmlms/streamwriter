object frmSettings: TfrmSettings
  Left = 350
  Top = 283
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Settings'
  ClientHeight = 470
  ClientWidth = 620
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
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlStreams: TPanel
    Left = 304
    Top = 4
    Width = 293
    Height = 325
    TabOrder = 0
    Visible = False
    DesignSize = (
      293
      325)
    object lblDefaultFilter: TLabel
      Left = 4
      Top = 288
      Width = 25
      Height = 13
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'Lists:'
    end
    object chkDeleteStreams: TCheckBox
      Left = 20
      Top = 60
      Width = 265
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Delete stream files when recording stops'
      TabOrder = 0
      OnClick = chkDeleteStreamsClick
    end
    object chkAddSavedToIgnore: TCheckBox
      Left = 4
      Top = 84
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Add saved titles to global ignorelist'
      TabOrder = 1
      OnClick = chkAddSavedToIgnoreClick
    end
    object lstDefaultFilter: TComboBox
      Left = 4
      Top = 304
      Width = 213
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akBottom]
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
      Top = 20
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Save separated tracks'
      TabOrder = 3
      OnClick = chkSeparateTracksClick
    end
    object chkSaveStreamsToDisk: TCheckBox
      Left = 4
      Top = 0
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Save stream files to hard disk drive'
      TabOrder = 4
      OnClick = chkSaveStreamsToDiskClick
    end
    object chkOnlySaveFull: TCheckBox
      Left = 20
      Top = 40
      Width = 265
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Only save whole songs'
      TabOrder = 5
      OnClick = chkOnlySaveFullClick
    end
    object chkOverwriteSmaller: TCheckBox
      Left = 20
      Top = 164
      Width = 265
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Overwrite existing file if newer file is larger'
      TabOrder = 6
      OnClick = chkOverwriteSmallerClick
    end
    object chkDiscardSmaller: TCheckBox
      Left = 20
      Top = 184
      Width = 265
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Discard new file if existing one is lager'
      TabOrder = 7
      OnClick = chkDiscardSmallerClick
    end
    object chkAddSavedToStreamIgnore: TCheckBox
      Left = 4
      Top = 104
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Add saved titles to stream ignorelist'
      TabOrder = 8
      OnClick = chkAddSavedToStreamIgnoreClick
    end
    object chkRemoveSavedFromWishlist: TCheckBox
      Left = 4
      Top = 124
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Remove saved titles from wishlist'
      TabOrder = 9
      OnClick = chkRemoveSavedFromWishlistClick
    end
    object chkDiscardAlways: TCheckBox
      Left = 4
      Top = 144
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Discard new file if it already exists'
      TabOrder = 10
      OnClick = chkDiscardAlwaysClick
    end
  end
  object pnlMain: TPanel
    Left = 4
    Top = 4
    Width = 293
    Height = 385
    TabOrder = 1
    Visible = False
    DesignSize = (
      293
      385)
    object Label7: TLabel
      Left = 56
      Top = 232
      Width = 13
      Height = 13
      Caption = 'GB'
    end
    object Label3: TLabel
      Left = 4
      Top = 304
      Width = 191
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Default action on doubleclick on stream:'
    end
    object Label18: TLabel
      Left = 4
      Top = 348
      Width = 193
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Default action on doubleclick in browser:'
    end
    object chkTray: TCheckBox
      Left = 4
      Top = 20
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Move to notification area...'
      TabOrder = 0
      OnClick = chkTrayClick
    end
    object txtMinDiskSpace: TLabeledEdit
      Left = 4
      Top = 228
      Width = 49
      Height = 21
      EditLabel.Width = 212
      EditLabel.Height = 13
      EditLabel.Caption = 'Stop recording when free space gets below:'
      MaxLength = 3
      NumbersOnly = True
      TabOrder = 1
    end
    object lstDefaultAction: TComboBox
      Left = 4
      Top = 320
      Width = 213
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akBottom]
      TabOrder = 2
      Items.Strings = (
        'Start/stop recording'
        'Listen to stream'
        'Listen to stream (external player)')
    end
    object optClose: TRadioButton
      Left = 20
      Top = 40
      Width = 265
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = '...on close'
      TabOrder = 3
    end
    object optMinimize: TRadioButton
      Left = 20
      Top = 60
      Width = 265
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = '...on minimize'
      TabOrder = 4
    end
    object lstDefaultActionBrowser: TComboBox
      Left = 4
      Top = 364
      Width = 213
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akBottom]
      TabOrder = 5
      Items.Strings = (
        'Start recording'
        'Listen to stream'
        'Listen to stream (external player)'
        'Only add to list')
    end
    object chkSnapMain: TCheckBox
      Left = 4
      Top = 84
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Snap main window to screen edges'
      TabOrder = 6
    end
    object chkRememberRecordings: TCheckBox
      Left = 4
      Top = 104
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Remember streams that were recording on exit'
      TabOrder = 7
    end
    object chkDisplayPlayNotifications: TCheckBox
      Left = 4
      Top = 144
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Display notifications on track change when playing'
      TabOrder = 8
    end
    object chkShowSplashScreen: TCheckBox
      Left = 4
      Top = 164
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Show splash screen on startup'
      TabOrder = 9
    end
    object chkDisplayPlayedSong: TCheckBox
      Left = 4
      Top = 124
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Display played song in titlebar'
      TabOrder = 10
    end
    object chkCoverPanelAlwaysVisible: TCheckBox
      Left = 4
      Top = 184
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Always show cover in "Saved songs"'
      TabOrder = 11
    end
    object chkAutostart: TCheckBox
      Left = 4
      Top = 0
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Start streamWriter on windows logon'
      TabOrder = 12
    end
  end
  object pnlAdvanced: TPanel
    Left = 903
    Top = 271
    Width = 293
    Height = 207
    TabOrder = 2
    Visible = False
    DesignSize = (
      293
      207)
    object Label1: TLabel
      Left = 56
      Top = 64
      Width = 39
      Height = 13
      Caption = 'seconds'
    end
    object lblSoundDevice: TLabel
      Left = 4
      Top = 88
      Width = 68
      Height = 13
      Caption = 'Sound device:'
    end
    object btnBrowseLogFile: TPngSpeedButton
      Left = 264
      Top = 148
      Width = 25
      Height = 21
      Hint = 'Browse...'
      Anchors = [akTop, akRight]
      Flat = True
      Layout = blGlyphRight
      ParentShowHint = False
      ShowHint = True
      OnClick = btnBrowseLogFileClick
    end
    object txtMaxRetries: TLabeledEdit
      Left = 4
      Top = 16
      Width = 49
      Height = 21
      EditLabel.Width = 222
      EditLabel.Height = 13
      EditLabel.Caption = 'Max. connect retries on error (zero is infinite):'
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
      EditLabel.Width = 105
      EditLabel.Height = 13
      EditLabel.Caption = 'Time between retries:'
      MaxLength = 3
      NumbersOnly = True
      TabOrder = 1
      OnChange = txtRetryDelayChange
    end
    object lstSoundDevice: TComboBox
      Left = 4
      Top = 104
      Width = 285
      Height = 21
      Style = csDropDownList
      TabOrder = 2
    end
    object txtLogFile: TLabeledEdit
      Left = 4
      Top = 148
      Width = 257
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      EditLabel.Width = 35
      EditLabel.Height = 13
      EditLabel.Caption = 'Logfile:'
      TabOrder = 3
    end
  end
  object pnlPostProcess: TPanel
    Left = 304
    Top = 336
    Width = 294
    Height = 349
    TabOrder = 3
    Visible = False
    DesignSize = (
      294
      349)
    object lblAppParams: TLabel
      Left = 4
      Top = 304
      Width = 281
      Height = 41
      Anchors = [akLeft, akRight, akBottom]
      AutoSize = False
      Caption = '-'
      WordWrap = True
      ExplicitTop = 264
    end
    object btnBrowseApp: TPngSpeedButton
      Left = 264
      Top = 232
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
      ExplicitTop = 192
    end
    object btnHelpPostProcess: TPngSpeedButton
      Left = 266
      Top = 128
      Width = 24
      Height = 25
      Hint = 'Info...'
      Anchors = [akRight, akBottom]
      Layout = blGlyphRight
      ParentShowHint = False
      ShowHint = True
      OnClick = btnHelpClick
      PngOptions = [pngBlendOnDisabled, pngGrayscaleOnDisabled]
      ExplicitTop = 88
    end
    object btnMoveDown: TPngSpeedButton
      Left = 266
      Top = 80
      Width = 24
      Height = 25
      Hint = 'Move down'
      Anchors = [akTop, akRight]
      Layout = blGlyphRight
      ParentShowHint = False
      ShowHint = True
      OnClick = btnMoveClick
      PngOptions = [pngBlendOnDisabled, pngGrayscaleOnDisabled]
    end
    object btnMoveUp: TPngSpeedButton
      Left = 266
      Top = 52
      Width = 24
      Height = 25
      Hint = 'Move up'
      Anchors = [akTop, akRight]
      Layout = blGlyphRight
      ParentShowHint = False
      ShowHint = True
      OnClick = btnMoveClick
      PngOptions = [pngBlendOnDisabled, pngGrayscaleOnDisabled]
    end
    object lblOutputFormat: TLabel
      Left = 4
      Top = 0
      Width = 172
      Height = 13
      Caption = 'Format to convert recorded files to:'
    end
    object btnConfigureEncoder: TPngSpeedButton
      Left = 266
      Top = 16
      Width = 24
      Height = 21
      Hint = 'Configure encoder...'
      Anchors = [akTop, akRight]
      Layout = blGlyphRight
      ParentShowHint = False
      ShowHint = True
      OnClick = btnConfigureEncoderClick
      PngOptions = [pngBlendOnDisabled, pngGrayscaleOnDisabled]
    end
    object lstPostProcess: TListView
      Left = 4
      Top = 44
      Width = 257
      Height = 109
      Align = alCustom
      Anchors = [akLeft, akTop, akRight, akBottom]
      Checkboxes = True
      Columns = <
        item
          Width = 200
        end>
      Groups = <
        item
          Header = 'Processing when in WAVE-format'
          GroupID = 0
          State = [lgsNormal]
          HeaderAlign = taLeftJustify
          FooterAlign = taLeftJustify
          TitleImage = -1
        end
        item
          Header = 'Processing after conversion to destination format'
          GroupID = 1
          State = [lgsNormal]
          HeaderAlign = taLeftJustify
          FooterAlign = taLeftJustify
          TitleImage = -1
        end>
      GroupView = True
      ReadOnly = True
      RowSelect = True
      ShowColumnHeaders = False
      SmallImages = PngImageList1
      TabOrder = 0
      ViewStyle = vsReport
      OnResize = lstPostProcessResize
      OnSelectItem = lstPostProcessSelectItem
      OnItemChecked = lstPostProcessItemChecked
    end
    object btnAdd: TButton
      Left = 96
      Top = 160
      Width = 93
      Height = 27
      Anchors = [akRight, akBottom]
      Caption = '&Add...'
      TabOrder = 1
      OnClick = btnAddClick
    end
    object btnRemove: TButton
      Left = 196
      Top = 160
      Width = 93
      Height = 27
      Anchors = [akRight, akBottom]
      Caption = '&Remove'
      Enabled = False
      TabOrder = 2
      OnClick = btnRemoveClick
    end
    object txtApp: TLabeledEdit
      Left = 4
      Top = 232
      Width = 257
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      AutoSize = False
      Color = 15790320
      EditLabel.Width = 93
      EditLabel.Height = 13
      EditLabel.Caption = 'Path to application:'
      Enabled = False
      ReadOnly = True
      TabOrder = 3
    end
    object txtAppParams: TLabeledEdit
      Left = 4
      Top = 276
      Width = 285
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      EditLabel.Width = 59
      EditLabel.Height = 13
      EditLabel.Caption = 'Parameters:'
      Enabled = False
      TabOrder = 4
      OnChange = txtAppParamsChange
    end
    object chkOnlyIfCut: TCheckBox
      Left = 4
      Top = 192
      Width = 285
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      Caption = 'Use only if file was cut successfully'
      TabOrder = 5
      OnClick = chkOnlyIfCutClick
    end
    object btnConfigure: TButton
      Left = 4
      Top = 160
      Width = 93
      Height = 27
      Anchors = [akLeft, akBottom]
      Caption = '&Configure...'
      TabOrder = 6
      OnClick = btnConfigureClick
    end
    object lstOutputFormat: TComboBox
      Left = 4
      Top = 16
      Width = 257
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 7
      OnSelect = lstOutputFormatSelect
      Items.Strings = (
        'Do not change format'
        'MP3'
        'AAC'
        'OGG')
    end
  end
  object pnlCut: TPanel
    Left = 604
    Top = 4
    Width = 293
    Height = 361
    TabOrder = 4
    Visible = False
    DesignSize = (
      293
      361)
    object Label4: TLabel
      Left = 56
      Top = 48
      Width = 39
      Height = 13
      Caption = 'seconds'
    end
    object Label5: TLabel
      Left = 56
      Top = 240
      Width = 55
      Height = 13
      Caption = 'milliseconds'
    end
    object Label10: TLabel
      Left = 36
      Top = 148
      Width = 192
      Height = 13
      Caption = 'Silence is defined by volume lower than:'
    end
    object Label12: TLabel
      Left = 36
      Top = 192
      Width = 31
      Height = 13
      Caption = 'lasting'
    end
    object Label13: TLabel
      Left = 160
      Top = 192
      Width = 59
      Height = 13
      Caption = 'ms (min. 20)'
    end
    object Label14: TLabel
      Left = 120
      Top = 168
      Width = 36
      Height = 13
      Caption = '(1-100)'
    end
    object lblPanelCut: TLabel
      Left = 208
      Top = 168
      Width = 53
      Height = 13
      Alignment = taCenter
      Caption = 'lblPanelCut'
    end
    object Label6: TLabel
      Left = 20
      Top = 100
      Width = 61
      Height = 13
      Caption = 'in a range of'
    end
    object Label15: TLabel
      Left = 164
      Top = 100
      Width = 39
      Height = 13
      Caption = 'seconds'
    end
    object txtSongBuffer: TLabeledEdit
      Left = 4
      Top = 236
      Width = 49
      Height = 21
      EditLabel.Width = 252
      EditLabel.Height = 13
      EditLabel.Caption = 'If no silence was found, append buffer to start/end:'
      MaxLength = 5
      NumbersOnly = True
      TabOrder = 0
      OnChange = txtSongBufferChange
    end
    object txtShortLengthSeconds: TLabeledEdit
      Left = 4
      Top = 44
      Width = 49
      Height = 21
      EditLabel.Width = 134
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
      Width = 285
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Skip ads (short songs)'
      TabOrder = 2
      OnClick = chkSkipShortClick
    end
    object chkSearchSilence: TCheckBox
      Left = 4
      Top = 72
      Width = 285
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Search for silence before saving tracks'
      TabOrder = 3
      OnClick = chkSearchSilenceClick
    end
    object txtSilenceLevel: TEdit
      Left = 36
      Top = 164
      Width = 81
      Height = 21
      MaxLength = 3
      NumbersOnly = True
      TabOrder = 4
      OnChange = txtSilenceLevelChange
    end
    object txtSilenceLength: TEdit
      Left = 76
      Top = 188
      Width = 81
      Height = 21
      MaxLength = 4
      NumbersOnly = True
      TabOrder = 5
      OnChange = txtSilenceLengthChange
    end
    object txtSilenceBufferSeconds: TEdit
      Left = 92
      Top = 96
      Width = 69
      Height = 21
      MaxLength = 2
      NumbersOnly = True
      TabOrder = 6
      OnChange = txtSilenceBufferSecondsChange
    end
    object chkAdjustTrackOffset: TCheckBox
      Left = 4
      Top = 268
      Width = 281
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Adjust offset of detected track changes'
      TabOrder = 7
      OnClick = chkAdjustTrackOffsetClick
    end
    object txtAdjustTrackOffset: TLabeledEdit
      Left = 20
      Top = 292
      Width = 61
      Height = 21
      EditLabel.Width = 55
      EditLabel.Height = 13
      EditLabel.Caption = 'milliseconds'
      Enabled = False
      LabelPosition = lpRight
      MaxLength = 5
      NumbersOnly = True
      TabOrder = 8
      OnChange = txtAdjustTrackOffsetChange
    end
    object optAdjustBackward: TRadioButton
      Left = 20
      Top = 316
      Width = 265
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Before detected change'
      Checked = True
      Enabled = False
      TabOrder = 9
      TabStop = True
      OnClick = optAdjustClick
    end
    object optAdjustForward: TRadioButton
      Left = 20
      Top = 336
      Width = 265
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'After detected change'
      Enabled = False
      TabOrder = 10
      OnClick = optAdjustClick
    end
    object chkManualSilenceLevel: TCheckBox
      Left = 20
      Top = 120
      Width = 265
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Use manual silence detection settings:'
      TabOrder = 11
      OnClick = chkManualSilenceLevelClick
    end
  end
  object pnlHotkeys: TPanel
    Left = 904
    Top = 484
    Width = 293
    Height = 77
    TabOrder = 5
    Visible = False
    DesignSize = (
      293
      77)
    object Label9: TLabel
      Left = 4
      Top = 40
      Width = 38
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Hotkey:'
    end
    object lstHotkeys: TListView
      Left = 4
      Top = 0
      Width = 285
      Height = 33
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
      Top = 56
      Width = 285
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      Enabled = False
      HotKey = 32833
      TabOrder = 1
      OnChange = txtHotkeyChange
    end
  end
  object pnlAutoRecord: TPanel
    Left = 904
    Top = 4
    Width = 293
    Height = 261
    TabOrder = 6
    Visible = False
    DesignSize = (
      293
      261)
    object Label16: TLabel
      Left = 20
      Top = 88
      Width = 79
      Height = 13
      Caption = 'Minimum quality:'
    end
    object Label17: TLabel
      Left = 20
      Top = 136
      Width = 38
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
    object lstMinQuality: TComboBox
      Left = 20
      Top = 104
      Width = 129
      Height = 21
      Style = csDropDownList
      TabOrder = 1
      Items.Strings = (
        'High'
        'Medium'
        'Low')
    end
    object lstFormat: TComboBox
      Left = 20
      Top = 152
      Width = 129
      Height = 21
      Style = csDropDownList
      TabOrder = 2
      Items.Strings = (
        'MP3/AAC'
        'MP3'
        'AAC')
    end
    object chkAutoTuneInConsiderIgnore: TCheckBox
      Left = 20
      Top = 20
      Width = 268
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Do not tune in if song is on global ignorelist'
      TabOrder = 3
      OnClick = chkAutoTuneInClick
    end
    object chkAutoTuneInAddToIgnore: TCheckBox
      Left = 20
      Top = 40
      Width = 269
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Add saved titles to global ignorelist'
      TabOrder = 4
    end
    object chkAutoRemoveSavedFromWishlist: TCheckBox
      Left = 20
      Top = 60
      Width = 269
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Remove saved titles from wishlist'
      TabOrder = 5
    end
  end
  object pnlFilenames: TPanel
    Left = 304
    Top = 692
    Width = 293
    Height = 333
    TabOrder = 7
    Visible = False
    DesignSize = (
      293
      333)
    object lblFilePattern: TLabel
      Left = 4
      Top = 280
      Width = 284
      Height = 49
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = '-'
      WordWrap = True
    end
    object btnResetFilePattern: TPngSpeedButton
      Left = 264
      Top = 72
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
      Top = 160
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
      Top = 204
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
    object btnBrowse: TPngSpeedButton
      Left = 264
      Top = 16
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
    object Bevel1: TBevel
      Left = 4
      Top = 48
      Width = 285
      Height = 9
      Anchors = [akLeft, akTop, akRight]
      Shape = bsTopLine
    end
    object txtFilePattern: TLabeledEdit
      Left = 4
      Top = 72
      Width = 257
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 250
      EditLabel.Height = 13
      EditLabel.Caption = 'Pattern for filenames of completely recorded tracks:'
      TabOrder = 0
      OnChange = txtFilePatternChange
      OnEnter = txtFilePatternEnter
    end
    object txtPreview: TLabeledEdit
      Left = 4
      Top = 248
      Width = 256
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Color = 15790320
      EditLabel.Width = 141
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
      EditLabel.Width = 258
      EditLabel.Height = 13
      EditLabel.Caption = 'Pattern for filenames of incompletely recorded tracks:'
      TabOrder = 2
      OnChange = txtFilePatternChange
      OnEnter = txtFilePatternEnter
    end
    object txtAutomaticFilePattern: TLabeledEdit
      Left = 4
      Top = 160
      Width = 257
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 333
      EditLabel.Height = 13
      EditLabel.Caption = 
        'Pattern for filenames of automatically recorded tracks (%n is in' +
        'valid):'
      TabOrder = 3
      OnChange = txtFilePatternChange
      OnEnter = txtFilePatternEnter
    end
    object txtStreamFilePattern: TLabeledEdit
      Left = 4
      Top = 204
      Width = 257
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 331
      EditLabel.Height = 13
      EditLabel.Caption = 
        'Pattern for filenames of stream files (only %s, %d and %i are va' +
        'lid):'
      TabOrder = 4
      OnChange = txtStreamFilePatternChange
      OnClick = txtStreamFilePatternClick
      OnEnter = txtFilePatternEnter
    end
    object txtDir: TLabeledEdit
      Left = 4
      Top = 16
      Width = 257
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      EditLabel.Width = 114
      EditLabel.Height = 13
      EditLabel.Caption = 'Folder for saved songs:'
      TabOrder = 5
    end
  end
  object pnlCommunityBlacklist: TPanel
    Left = 4
    Top = 544
    Width = 293
    Height = 69
    TabOrder = 8
    Visible = False
    DesignSize = (
      293
      69)
    object Label19: TLabel
      Left = 4
      Top = 0
      Width = 266
      Height = 13
      Caption = 'Do not automatically record from the following streams:'
    end
    object pnlBlacklist: TPanel
      Left = 4
      Top = 16
      Width = 285
      Height = 17
      Anchors = [akLeft, akTop, akRight, akBottom]
      BevelOuter = bvNone
      TabOrder = 0
    end
    object btnBlacklistRemove: TButton
      Left = 196
      Top = 42
      Width = 93
      Height = 27
      Anchors = [akRight, akBottom]
      Caption = '&Remove'
      Enabled = False
      TabOrder = 1
      OnClick = btnBlacklistRemoveClick
    end
  end
  object pnlStreamsAdvanced: TPanel
    Left = 604
    Top = 371
    Width = 293
    Height = 369
    TabOrder = 9
    Visible = False
    DesignSize = (
      293
      369)
    object btnResetTitlePattern: TPngSpeedButton
      Left = 264
      Top = 116
      Width = 25
      Height = 21
      Hint = 'Reset regular expression to default'
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
      Top = 184
      Width = 271
      Height = 26
      Anchors = [akLeft, akTop, akRight]
      Caption = 
        'Ignore the following stream title changes (i.e. announcements li' +
        'ke '#39'Next playing: ...'#39') to disable saving:'
      WordWrap = True
    end
    object Label21: TLabel
      Left = 4
      Top = 0
      Width = 255
      Height = 26
      Anchors = [akLeft, akTop, akRight]
      Caption = 
        'Use the following regular expressions for this stream (groups: a' +
        ' = artist, t = title, l = album):'
      WordWrap = True
    end
    object txtRegEx: TLabeledEdit
      Left = 4
      Top = 116
      Width = 257
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 130
      EditLabel.Height = 13
      EditLabel.Caption = 'Regular expression to add:'
      EditLabel.Layout = tlBottom
      TabOrder = 0
      OnChange = txtRegExChange
    end
    object lstIgnoreTitles: TListView
      Left = 4
      Top = 212
      Width = 285
      Height = 85
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
      Top = 342
      Width = 93
      Height = 27
      Anchors = [akRight, akBottom]
      Caption = 'R&emove'
      Enabled = False
      TabOrder = 2
      OnClick = btnRemoveIgnoreTitlePatternClick
    end
    object btnAddIgnoreTitlePattern: TButton
      Left = 100
      Top = 342
      Width = 93
      Height = 27
      Anchors = [akRight, akBottom]
      Caption = 'A&dd'
      Enabled = False
      TabOrder = 3
      OnClick = btnAddIgnoreTitlePatternClick
    end
    object txtIgnoreTitlePattern: TLabeledEdit
      Left = 4
      Top = 318
      Width = 285
      Height = 21
      Anchors = [akLeft, akRight, akBottom]
      EditLabel.Width = 171
      EditLabel.Height = 13
      EditLabel.Caption = 'Pattern to add (use '#39'*'#39' as wildcard):'
      TabOrder = 4
      OnChange = txtIgnoreTitlePatternChange
    end
    object lstRegExes: TListView
      Left = 4
      Top = 28
      Width = 285
      Height = 69
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
      OnChange = lstRegExesChange
      OnEdited = lstRegExesEdited
      OnResize = lstRegExesResize
    end
    object btnAddRegEx: TButton
      Left = 100
      Top = 140
      Width = 93
      Height = 27
      Anchors = [akTop, akRight]
      Caption = '&Add'
      Enabled = False
      TabOrder = 6
      OnClick = btnAddRegExClick
    end
    object btnRemoveRegEx: TButton
      Left = 196
      Top = 140
      Width = 93
      Height = 27
      Anchors = [akTop, akRight]
      Caption = '&Remove'
      Enabled = False
      TabOrder = 7
      OnClick = btnRemoveRegExClick
    end
  end
  object pnlBandwidth: TPanel
    Left = 4
    Top = 620
    Width = 293
    Height = 157
    TabOrder = 10
    Visible = False
    DesignSize = (
      293
      157)
    object Label11: TLabel
      Left = 76
      Top = 44
      Width = 21
      Height = 13
      Caption = 'KB/s'
    end
    object Label22: TLabel
      Left = 4
      Top = 72
      Width = 285
      Height = 81
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      Caption = 
        'Bandwidth is also used for monitoring streams for the community.' +
        ' If your bandwidth/traffic is limited look at the "Community" se' +
        'ction on the left and disable the "Enable monitor mode" checkbox' +
        ' because bandwith used by monitor mode is not considered for the' +
        ' "Limit bandwidth for recordings" setting.'
      WordWrap = True
      ExplicitHeight = 61
    end
    object txtMaxSpeed: TLabeledEdit
      Left = 20
      Top = 40
      Width = 53
      Height = 21
      EditLabel.Width = 205
      EditLabel.Height = 13
      EditLabel.Caption = 'Max. bandwidth available to streamWriter:'
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
    Left = 4
    Top = 408
    Width = 293
    Height = 129
    TabOrder = 11
    DesignSize = (
      293
      129)
    object btnResetRemoveChars: TPngSpeedButton
      Left = 264
      Top = 60
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
      Top = 60
      Width = 257
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 234
      EditLabel.Height = 13
      EditLabel.Caption = 'Remove the following characters from filenames:'
      TabOrder = 0
      OnChange = txtRemoveCharsChange
    end
    object txtFilePatternDecimals: TLabeledEdit
      Left = 4
      Top = 16
      Width = 41
      Height = 21
      EditLabel.Width = 215
      EditLabel.Height = 13
      EditLabel.Caption = 'Minimum count of decimals for tracknumbers:'
      MaxLength = 1
      NumbersOnly = True
      TabOrder = 1
      OnChange = txtFilePatternDecimalsChange
    end
    object chkNormalizeVariables: TCheckBox
      Left = 4
      Top = 88
      Width = 284
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Adjust upper-/lowercase of artist/title/album'
      TabOrder = 2
      OnClick = chkNormalizeVariablesClick
    end
  end
  object pnlAddons: TPanel
    Left = 32
    Top = 368
    Width = 293
    Height = 37
    TabOrder = 12
    Visible = False
    object lstAddons: TListView
      Left = 4
      Top = 0
      Width = 285
      Height = 37
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
      SmallImages = PngImageList1
      TabOrder = 0
      ViewStyle = vsReport
      OnResize = lstAddonsResize
      OnItemChecked = lstAddonsItemChecked
    end
  end
  object pnlCommunity: TPanel
    Left = 904
    Top = 568
    Width = 293
    Height = 297
    TabOrder = 13
    Visible = False
    DesignSize = (
      293
      297)
    object Label2: TLabel
      Left = 20
      Top = 24
      Width = 268
      Height = 33
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'Streams unknown to streamWriter will be submitted to the server ' +
        'so they can appear in the browser.'
      WordWrap = True
      OnClick = Label2Click
    end
    object Label8: TLabel
      Left = 20
      Top = 84
      Width = 268
      Height = 57
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'Information about track changes will be sent to the server and b' +
        'roadcasted to other users. This is important for the automatic r' +
        'ecording of songs on the wishlist.'
      WordWrap = True
      OnClick = Label8Click
    end
    object Label20: TLabel
      Left = 20
      Top = 152
      Width = 268
      Height = 81
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'Server-assigned streams will be monitored for title changes in t' +
        'he background. Do not disable this option if you have a fast int' +
        'ernet connection (DSL or faster) and unlimited traffic (flatrate' +
        '). Monitored streams will not be covered by the bandwidth limit.' +
        ' One monitored stream consumes between 8 and 40KB/s depending on' +
        ' the stream'#39's quality.'
      WordWrap = True
      OnClick = Label20Click
    end
    object chkSubmitStreamInfo: TCheckBox
      Left = 4
      Top = 0
      Width = 284
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Submit information about streams to the server'
      TabOrder = 0
      OnClick = chkSubmitStreamInfoClick
    end
    object chkSubmitStats: TCheckBox
      Left = 4
      Top = 60
      Width = 284
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Submit statistics/track changes to the server'
      TabOrder = 1
      OnClick = chkSubmitStatsClick
    end
    object chkMonitorMode: TCheckBox
      Left = 4
      Top = 128
      Width = 284
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Enable monitor mode'
      TabOrder = 2
      OnClick = chkMonitorModeClick
    end
    object txtMonitorCount: TLabeledEdit
      Left = 20
      Top = 252
      Width = 53
      Height = 21
      EditLabel.Width = 173
      EditLabel.Height = 13
      EditLabel.Caption = 'Max. number of streams to monitor:'
      MaxLength = 3
      NumbersOnly = True
      TabOrder = 3
    end
  end
  object dlgOpen: TOpenDialog
    Filter = 'Executable files (*.exe, *.bat)|*.exe;*.bat'
    Options = [ofHideReadOnly, ofEnableSizing, ofDontAddToRecent]
    Left = 548
    Top = 272
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
      end
      item
        Background = clWindow
        Name = 'lightning'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000021F4944415478DAA5534D6B135114BD9374C69A262D3531
          855AA6944E6C29527463C54A54444403821F584ACD42FC03DDD455775DEAC63F
          E0427051C4228AAE4504ABB14D8B583469880A6AA79369DA66928C6FE6BDE79D
          69062B49D1E285CB7BF371CE9939E73E81730EFF53C276023D7DFA3E673CE9DC
          E38C01305CE9D6DE6D6F4FD9D7AE73F3DD750485B953B4AD67CC872400D47991
          FE6E9B42A5900273236B203854F705089E9242B1C9BDFB8E02D167805B3630CB
          024E2C102397C12C2E41454F53043735FC052D15FFD9D67D4D62661EECD27B04
          6E1188E18B408CEF50D6E680924D45BEB090AB23D0DEC6AF8A2DF2C340340E56
          610618AA3A607FE00880AF1D0C358524DF127262E179431357674F18C1CEB32D
          7EB115099E2118D5090169FF1528AFBE83EADAD2849C58BCD33005F5F5F0905F
          8ACC86BACE03293C71D539AA33D304A9E312E8D9071C5D275E02CC35D7ED8DD8
          F5E50E61E5D57135101D8E4A41B9F6C0761D07B486DB08B09D6BAB960603439B
          876A315B61161DECBB91CF093F5E1E5B47E6E66D19BB6B7BCFC81E333F0D8274
          00444CA6AC2D3A4034B27A2F965CBED97090BCFAF274F0456BE799934D52D855
          2FADA00FC5CC26921F8C2573EA8E93E8D5E7C78778581945035155FF84AA95BB
          CA5876FCAFA35C03A79B43CA61CE70F2F4CC1AAA0E2058851DEA0F02040781FB
          4A8E0FD422B795D1CCAD5D1DA6FCA3810F185304097A116CECFA34E6A6FB877A
          473EBEF917A057BF00F4C177F041057F770000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'application-terminal'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C000001FD4944415478DAA5534D885251143EE7FDF8874F11535C
          B568E3A29D9B5CB972E34250C1551B77B73482180D22701544100311D530FB82
          360E430CD86CC61089065CB675910B8D701C7D2132EF3DED9CA7BEC64D30CD85
          8F7B17F7FB39E7DE83CBE512AEB3B0582CDEA2FD0E41BB2257277CC342A170B7
          5AAD3E8946A3B72DCB04D3B4080618069F4D582C16B00A2903A26243515C301A
          8DBEEFEFEF3EC75C2E77BF52A9BCE552F832C3B22C1BFF3A33042D2E41F4FBFD
          BD482462E7DA08F17E59D4300C98CF2F4092388162A73C3DFD2A309FCFDFA3A8
          EFEAF53AB45A2D180C067674266C309DEA8419C8F22A3E63369BC2C9C96781D9
          6CB64C4E6F82C12094CB6568B7DBD0EBF52E917FD3E50B9BA4AAAEF5AE82AE8F
          E1F8F893C04C26F300115FD76A3568369B301C0EA1542A41B7DB25B10EC5361D
          D78D80A2A824FC0B8E8E1A02D3E9F44359965F699A067EBFDF769D4C26D46DA4
          86FD8DCCA495806A97727EFE130E0F3F0A4CA5528FDC6EF76E2C167362AF9E4F
          A68B9E2D576E9E24B180046767033838F82030994CEEF87CBE97E170D8691EBF
          BBC713DA7267570627E3351E0FA0D1782F3091483C0E04022F08B600BFB3CBE5
          074DBBB145942479EB1B3A02F178FC6928147A4665C0662EBCDE0089781CA224
          E1DA191D81F95C874EE7CB0E52F30435F1E6FF0C12A5FDC192DEF5202957E49B
          3C5078DD71FE03BAFD19B28F3DD1DC0000000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'plugin'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          61000000017352474200AECE1CE900000006624B474400FF00FF00FFA0BDA793
          000000097048597300000B1300000B1301009A9C180000000774494D4507DA07
          080E2524D68B782A0000022B4944415478DAA5934148146114C7FFA3B83BBB6E
          D4BAA69B65EE1A11692B81D0A1ECD2A18B22114197A02E11D6C12228A482A043
          74A84BF720E8D4A540CCF2B258D6B6502DB89BB6ACB59BB290A046923BCECE7C
          DFEBCDB7EE41262AE9C1CC2E33EFFDFEFFF7FF188D88F03FA5FD0E70F2563223
          0475DA82C8B2E9D9C8ED837DFF0CE0E1061E5EDCB955872508D9626971ECCEA1
          C60D39E8BF9EF8D9D6A4D79B9644AE68C4E3F77A8EFC11C0AAC4AA60CB90CCE3
          FF140DEB9A9040BAB0F2C3284BC16E6AC82E23FDE068D0053871F32D39969D61
          DB99E25AFB516B087E61DB8454761E99877D9A0B70EC468222CD3E35FCED7B19
          CB86506EAA450C8B847524276731F5E8B81BD03BF4862C56F17B6AB0ADC1EB04
          C76BB0BA5DB161B27A47AB1F99D9124A4B7390C204F135F3F4BCB62EC4C31727
          686FAB0FD3730603A45AC95ACB25A0D7A22554E7E4A37A53EF93F83C3CB81E70
          E0C24B8A45FCF8F8B5A4069D52B958163607BC0807EB506D4FBD7B8D2F23972B
          809EC157C44923B4A916DB1B754CE6572AA7C1371236536CEC69DB824FF90594
          1672657EE091FCBCF0FC6A05D03D304E5DD17A957471C964258F1A56A7C14EF2
          F326224D5E564DB0EA257788B1B371DA1F0DA890AA19386191B4E00934A37D47
          104E5FE6C304F2A357DC807D6746A96B7748A93A8AE9A919641F9F528DBBFAEF
          5347AC9B8F15984E8DA3F062C80DE83C3D4CD25E55FB92B078656335F7E49CCF
          6968EFBD2B480A76230CBE2D17C6AEB5FCF55BD848FD02C9156CF0B43B4FAC00
          00000049454E44AE426082}
      end
      item
        Background = clWindow
        Name = 'page_edit'
        PngImage.Data = {
          89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
          610000001974455874536F6674776172650041646F626520496D616765526561
          647971C9653C0000032F4944415478DA55936B4814511886DF33DBAEB36EBAA5
          B17431C352D24A4C360DAB2DA552298B0A8220A1CB8FA0EC7EA5E88A586950D1
          8F2C4AA48BAB3FFA114145A268AE97522913ADACB4C84CDDD4D6DCCBB83B3BA7
          3333ABD181731986EFF9DEF39DF7239452AC3E5BB35D1F1AB495233003E0D924
          4060A56CA1124449EA740CD24AF2E7F7315B619617814164C09A8B75B607FBCC
          711C47265140038AF1419533C52FA70FD66607FABE8FBADFB77D0CAB2DCC1A1D
          0764E5D7DB4BF72785B5DBA1E1085132B3A4F0CB99450A8F4F446A8C018D2DCF
          61FA741F9A89F1700DBC04117AB6298075050DEE92BD66FD97210D8BA58A0089
          2D7E4982970104AF1F8B43CAE1FEFC0AFA291618A312E1F8DA808FE577FB5505
          050D1EEB5E33DF2503880C20FF01B48E6788E15B61084DC45067078CE173A00B
          31E143D57D611C509263E6BBFF68027563003F85C8FE69866D8844158CB33330
          DA5B06CF2081BDC38D6137A1DCE860AC02589BCF004C41DF8856B9000D148F38
          AA31CD5F0163F47A083DB7C0E944F89CB330D4F401E77F647B8B4F6D0A525F81
          011E32C02FA756AD3C9BAEAE2798A16D8069C106165C084E2BC23B12057B5D0B
          42D32F21FB8153283F93A2570099794CC101336F7769952BB8BA9E2278C486B9
          962C78FBEF8168BC10862394E021CB6DCC9E39155BAE360B95E70280F4DC7A8F
          F52053E0D2A1FF6D194C6883484C080FABC364938E05CFC44053079C4BAF4334
          4C479C095897D72854E72E51012B2F30C021333FE8D4A1FDE16A6CDE5D848EB2
          A3F8F9AD1E419109F03B290CE93720864629C64C98C661D5E95AA1367FB90A48
          3B53E7B11E35F3076A09E29F24E3C48E5D00E7474BC5637476F7A02AF5110682
          632005DC69CDD022EDB84D787D3D5505584ED6784A8F25F357DAD915CECFC0C2
          59E1488A8D82C7148DDE7947E00B89505E459EAD7611D75200CBC16AE1CDAD55
          2A20E570B5ABF454B23EBF8D279439480A98090147CAEE9477CABA8D63326EAE
          2052D2EE0A77EB9DF41005B0684F65D7F3CBCBA6B87CD4C0BE39F91DC7E41AF5
          13C69B4AAFE5207B4CAFE346E277BEE8692FCE9CAF0092722A72441FDD480849
          64B983FFB5A2EA093AD69EEA264892F48663A578579459F4178E39AC10220115
          C20000000049454E44AE426082}
      end>
    Left = 232
    Top = 160
    Bitmap = {}
  end
  object dlgSave: TSaveDialog
    Filter = 'Text files (*.txt)|*.txt'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing, ofDontAddToRecent]
    Left = 548
    Top = 320
  end
end
