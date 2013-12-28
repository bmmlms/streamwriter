object frmStreamWriterMain: TfrmStreamWriterMain
  Left = 549
  Top = 450
  Caption = 'streamWriter'
  ClientHeight = 332
  ClientWidth = 804
  Color = clBtnFace
  Constraints.MinHeight = 390
  Constraints.MinWidth = 820
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = mnuMain
  OldCreateOrder = False
  SnapBuffer = 12
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object tbClients: TToolBar
    Left = 0
    Top = 0
    Width = 804
    Height = 25
    Images = modSharedData.imgImages
    Indent = 2
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    Transparent = True
    object ToolButton5: TToolButton
      Left = 2
      Top = 0
      Action = actNewCategory
    end
    object ToolButton7: TToolButton
      Left = 25
      Top = 0
      Width = 8
      ImageIndex = 14
      Style = tbsSeparator
    end
    object cmdStart: TToolButton
      Left = 33
      Top = 0
      Action = actStart
    end
    object cmdStop: TToolButton
      Left = 56
      Top = 0
      Action = actStop
    end
    object ToolButton3: TToolButton
      Left = 79
      Top = 0
      Width = 8
      Caption = '-'
      ImageIndex = 7
      Style = tbsSeparator
    end
    object cmdStartPlay: TToolButton
      Left = 87
      Top = 0
      Action = actPlay
    end
    object cmdPause: TToolButton
      Left = 110
      Top = 0
      Action = actPause
    end
    object cmdStopPlay: TToolButton
      Left = 133
      Top = 0
      Action = actStopPlay
    end
    object ToolButton1: TToolButton
      Left = 156
      Top = 0
      Width = 8
      Caption = '-'
      ImageIndex = 7
      Style = tbsSeparator
    end
    object cmdRename: TToolButton
      Left = 164
      Top = 0
      Action = actRename
    end
    object cmdRemove: TToolButton
      Left = 187
      Top = 0
      Action = actRemove
    end
    object ToolButton2: TToolButton
      Left = 210
      Top = 0
      Width = 8
      Caption = '-'
      ImageIndex = 11
      Style = tbsSeparator
    end
    object cmdSetupTimers: TToolButton
      Left = 218
      Top = 0
      Action = actTimers
    end
    object cmdStopAfterSong: TToolButton
      Left = 241
      Top = 0
      Action = actStopAfterSong
      ImageIndex = 52
    end
    object ToolButton9: TToolButton
      Left = 264
      Top = 0
      Width = 8
      Caption = '-'
      ImageIndex = 13
      Style = tbsSeparator
    end
    object cmdOpenWebsite: TToolButton
      Left = 272
      Top = 0
      Action = actOpenWebsite
    end
    object cmdCopyTitle: TToolButton
      Left = 295
      Top = 0
      Action = actCopyTitle
    end
    object ToolButton8: TToolButton
      Left = 318
      Top = 0
      Width = 8
      Caption = 'ToolButton8'
      ImageIndex = 13
      Style = tbsSeparator
    end
    object cmdStreamSettings: TToolButton
      Left = 326
      Top = 0
      Action = actStreamSettings
    end
    object ToolButton6: TToolButton
      Left = 349
      Top = 0
      Action = actResetData
    end
    object ToolButton4: TToolButton
      Left = 372
      Top = 0
      Width = 8
      Caption = '-'
      ImageIndex = 10
      Style = tbsSeparator
      Visible = False
    end
    object cmdShowStreamBrowser: TToolButton
      Left = 380
      Top = 0
      Action = actShowSideBar
      Visible = False
    end
  end
  object addXP: TXPManifest
    Left = 276
    Top = 64
  end
  object mnuMain: TMainMenu
    AutoHotkeys = maManual
    Images = modSharedData.imgImages
    Left = 32
    Top = 32
    object mnuFile: TMenuItem
      Caption = '&File'
      object mnuSettings: TMenuItem
        Action = actSettings
      end
      object Settingsforautomaticrecordings1: TMenuItem
        Caption = 'Settings for &automatic recordings...'
        Hint = 'Settings for automatic recordings...'
        ImageIndex = 90
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object mnuExit: TMenuItem
        Action = actExit
      end
    end
    object Community1: TMenuItem
      Caption = '&Community'
      OnClick = Community1Click
      object Login1: TMenuItem
        Action = actLogOn
      end
      object Logoff1: TMenuItem
        Action = actLogOff
      end
    end
    object mnuStreams: TMenuItem
      Caption = '&Stream'
      Visible = False
      object Newcategory1: TMenuItem
        Action = actNewCategory
      end
      object N13: TMenuItem
        Caption = '-'
      end
      object mnuStartStreaming2: TMenuItem
        Action = actStart
      end
      object mnuStopStreaming2: TMenuItem
        Action = actStop
      end
      object mnuStartPlay2: TMenuItem
        Action = actPlay
      end
      object Pause1: TMenuItem
        Action = actPause
      end
      object mnuStopPlay2: TMenuItem
        Action = actStopPlay
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object Openwebsite1: TMenuItem
        Action = actOpenWebsite
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object mnuRemove2: TMenuItem
        Action = actRemove
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object mnuTuneIn2: TMenuItem
        Caption = '&Listen'
        object mnuListenToStream2: TMenuItem
          Action = actTuneInStream
        end
        object mnuListenToFile2: TMenuItem
          Action = actTuneInFile
        end
      end
      object mnuSavePlaylist2: TMenuItem
        Caption = 'S&ave playlist'
        Hint = 'Save as playlist...'
        object actSavePlaylistStream1: TMenuItem
          Action = actSavePlaylistStream
        end
        object actSavePlaylistFile1: TMenuItem
          Action = actSavePlaylistFile
        end
      end
      object mnuStreamSettings2: TMenuItem
        Action = actStreamSettings
      end
      object mnuReset11: TMenuItem
        Action = actResetData
      end
    end
    object mnuPlayer: TMenuItem
      Caption = '&Player'
      OnClick = mnuPlayerClick
      object mnuPlayPause: TMenuItem
        Action = actPlayerPlayPause
      end
      object mnuStop: TMenuItem
        Action = actPlayerStop
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object mnuIncreaseVolume: TMenuItem
        Action = actPlayerIncreaseVolume
      end
      object mnuPlayerDecreaseVolume: TMenuItem
        Action = actPlayerDecreaseVolume
      end
      object mnuPlayerMuteVolume: TMenuItem
        Action = actPlayerMuteVolume
      end
      object N14: TMenuItem
        Caption = '-'
      end
      object mnuEqualizer: TMenuItem
        Action = actEqualizer
      end
    end
    object View1: TMenuItem
      Caption = '&View'
      Visible = False
      object mnuShowStreamBrowser: TMenuItem
        Action = actShowSideBar
      end
    end
    object mnuUpdate: TMenuItem
      Caption = '&Update'
      object mnuCheckUpdate: TMenuItem
        Caption = 'Search for &update...'
        ImageIndex = 7
        OnClick = mnuCheckUpdateClick
      end
    end
    object mnuHelp: TMenuItem
      Caption = '&Help'
      object mnuHelp2: TMenuItem
        Action = actHelp
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object mnuAbout: TMenuItem
        Action = actAbout
      end
    end
  end
  object ActionList1: TActionList
    Images = modSharedData.imgImages
    Left = 276
    Top = 116
    object actStart: TAction
      Caption = '&Start recording'
      Hint = 'Start recording'
      ImageIndex = 0
    end
    object actStop: TAction
      Caption = 'S&top recording'
      Hint = 'Stop recording'
      ImageIndex = 34
    end
    object actRemove: TAction
      Caption = '&Remove'
      Hint = 'Remove'
      ImageIndex = 21
    end
    object actExit: TAction
      Caption = '&Exit'
      Hint = 'Exit'
      ImageIndex = 6
      OnExecute = actExitExecute
    end
    object actSettings: TAction
      Caption = '&Settings...'
      Hint = 'Settings...'
      ImageIndex = 5
      OnExecute = actSettingsExecute
    end
    object actAbout: TAction
      Caption = '&About...'
      Hint = 'About...'
      ImageIndex = 10
      OnExecute = actAboutExecute
    end
    object actShowSideBar: TAction
      Caption = '&Show sidebar'
      Hint = 'Show sidebar'
      ImageIndex = 12
    end
    object actTuneInStream: TAction
      Caption = '&Stream'
    end
    object actTuneInFile: TAction
      Caption = 'Recorded &file'
    end
    object actSavePlaylistStream: TAction
      Caption = '&Stream'
    end
    object actSavePlaylistFile: TAction
      Caption = 'Recorded &file'
    end
    object actResetData: TAction
      Caption = 'Reset &data'
      Hint = 'Reset data'
      ImageIndex = 13
    end
    object actCutSave: TAction
      Caption = 'Save'
      Hint = 'Save'
      ImageIndex = 14
    end
    object actCutSaveAs: TAction
      Caption = 'Save as...'
      Hint = 'Save as...'
      ImageIndex = 15
    end
    object actHelp: TAction
      Caption = '&Help...'
      Hint = 'Help'
      ImageIndex = 29
      OnExecute = actHelpExecute
    end
    object actPlay: TAction
      Caption = '&Play'
      Hint = 'Play'
      ImageIndex = 33
    end
    object actStopPlay: TAction
      Caption = 'St&op'
      Hint = 'Stop'
      ImageIndex = 1
    end
    object actNewCategory: TAction
      Caption = '&New category...'
      Hint = 'New category...'
      ImageIndex = 11
    end
    object actStreamSettings: TAction
      Caption = 'S&ettings...'
      Hint = 'Settings...'
      ImageIndex = 9
      OnExecute = actStreamSettingsExecute
    end
    object actOpenWebsite: TAction
      Caption = 'Open &website...'
      Hint = 'Open website...'
      ImageIndex = 38
    end
    object actPause: TAction
      Caption = 'Pa&use'
      Hint = 'Pause'
      ImageIndex = 39
    end
    object actLogOn: TAction
      Caption = '&Logon...'
      Hint = 'Logon'
      ImageIndex = 45
      OnExecute = actLogOnExecute
    end
    object actLogOff: TAction
      Caption = 'L&ogoff'
      Hint = 'Logoff'
      ImageIndex = 46
      OnExecute = actLogOffExecute
    end
    object actTimers: TAction
      Caption = 'Setup scheduled record&ings...'
      Hint = 'Setup scheduled recordings...'
      ImageIndex = 50
      OnExecute = actTimersExecute
    end
    object actStopAfterSong: TAction
      AutoCheck = True
      Caption = 'Stop recording after &current title'
      Hint = 'Stop recording after current title'
    end
    object actCopyTitle: TAction
      Caption = '&Copy title to clipboard'
      Hint = 'Copy title to clipboard'
      ImageIndex = 57
    end
    object actAddToSaveList: TAction
      Caption = 'Add to manual &wishlist'
      Hint = 'Add to manual wishlist'
      ImageIndex = 31
    end
    object actAddToGlobalIgnoreList: TAction
      Caption = 'Add to &global ignorelist'
      Hint = 'Add to global ignorelist'
      ImageIndex = 65
    end
    object actAddToStreamIgnoreList: TAction
      Caption = 'Add to &stream ignorelist'
      Hint = 'Add to stream ignorelist'
      ImageIndex = 65
    end
    object actPlayerDecreaseVolume: TAction
      Caption = '&Decrease volume'
      ImageIndex = 72
      OnExecute = actPlayerDecreaseVolumeExecute
    end
    object actPlayerIncreaseVolume: TAction
      Caption = '&Increase volume'
      ImageIndex = 71
      OnExecute = actPlayerIncreaseVolumeExecute
    end
    object actPlayerMuteVolume: TAction
      Caption = '&Mute'
      ImageIndex = 73
      OnExecute = actPlayerMuteVolumeExecute
    end
    object actPlayerPlayPause: TAction
      Caption = '&Pause/play'
      ImageIndex = 39
      OnExecute = actPlayerPlayPauseExecute
    end
    object actPlayerStop: TAction
      Caption = '&Stop'
      ImageIndex = 1
      OnExecute = actPlayerStopExecute
    end
    object actRename: TAction
      Caption = 'Ren&ame'
      Hint = 'Rename'
      ImageIndex = 74
    end
    object actEqualizer: TAction
      Caption = '&Equalizer...'
      Hint = 'Equalizer...'
      ImageIndex = 76
      OnExecute = actEqualizerExecute
    end
  end
  object mnuStreamPopup: TPopupMenu
    AutoHotkeys = maManual
    Images = modSharedData.imgImages
    OnPopup = mnuStreamPopupPopup
    Left = 32
    Top = 88
    object mnuNewCategory1: TMenuItem
      Action = actNewCategory
    end
    object N12: TMenuItem
      Caption = '-'
    end
    object mnuStartStreaming1: TMenuItem
      Action = actStart
    end
    object mnuStopStreaming1: TMenuItem
      Action = actStop
    end
    object mnuStartPlay1: TMenuItem
      Action = actPlay
    end
    object mnuPause1: TMenuItem
      Action = actPause
    end
    object mnuStopPlay1: TMenuItem
      Action = actStopPlay
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object mnuRename1: TMenuItem
      Action = actRename
    end
    object mnuRemove1: TMenuItem
      Action = actRemove
    end
    object N11: TMenuItem
      Caption = '-'
    end
    object mnuSetupTimers1: TMenuItem
      Action = actTimers
    end
    object mnuStopAfterSong1: TMenuItem
      Action = actStopAfterSong
      AutoCheck = True
    end
    object N9: TMenuItem
      Caption = '-'
    end
    object mnuOpenWebsite1: TMenuItem
      Action = actOpenWebsite
    end
    object mnuCurrentTitle1: TMenuItem
      Caption = 'Current tit&le'
      object Copytitletoclipboard1: TMenuItem
        Action = actCopyTitle
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Addtowishlist1: TMenuItem
        Action = actAddToSaveList
      end
      object Addtoglobalignorelist1: TMenuItem
        Action = actAddToGlobalIgnoreList
      end
      object Addtostreamignorelist1: TMenuItem
        Action = actAddToStreamIgnoreList
      end
    end
    object mnuTuneIn1: TMenuItem
      Caption = 'Open'
      object mnuListenToStream1: TMenuItem
        Action = actTuneInStream
      end
      object mnuListenToFile1: TMenuItem
        Action = actTuneInFile
      end
    end
    object mnuSavePlaylist1: TMenuItem
      Caption = 'Save playlist'
      Hint = 'Save as playlist...'
      object Stream1: TMenuItem
        Action = actSavePlaylistStream
      end
      object Stream2: TMenuItem
        Action = actSavePlaylistFile
      end
    end
    object mnuMoveToCategory1: TMenuItem
      Caption = 'Move to &category'
    end
    object N15: TMenuItem
      Caption = '-'
    end
    object mnuStreamSettings1: TMenuItem
      Action = actStreamSettings
    end
    object mnuReset1: TMenuItem
      Action = actResetData
    end
  end
  object tmrSpeed: TTimer
    Enabled = False
    OnTimer = tmrSpeedTimer
    Left = 32
    Top = 144
  end
  object TrayIcon1: TTrayIcon
    Hint = 'streamWriter'
    Icon.Data = {
      0000010001001010000001002000680400001600000028000000100000002000
      000001002000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF800002C3871E110000
      0001FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF008253103168420F85854B11CD643A0ED44028
      08945E3E0D5FAA711C09FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B26D1546A85B17EBCD701BFDDA7F1CFFD77A1BFFC36B
      1AFA874B12E859370A81FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00B96419D5DB862EFFEBA242FFEA9A2EFFEA921EFFEA92
      1EFFCB6C1CFF814911DDC880240EFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFF0001C5691DF0EDB878FFEEC08CFFEDB570FFEB9C37FFEA92
      1EFFDF851DFFA55A17DC80800002FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00CA6B1BBFDB9E68FFF1DBCCFFEEC390FFECA64EFFEA92
      1EFFCB6C1CFFAB671783FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00DB801E2ACC6E20C5D07D3EFBDB9151FFD47D2CFFC764
      1AFFAE5C17E8BF800004FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00DE831A27D1751A74D2761B82C061
      1BFBA65F169EFFFFFF00FFFFFF00FFFFFF00BA751525FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00D0781A7CB65F
      19F3B26F1617FFFFFF00FFFFFF00B36A179683520F6CFFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BC6518CDA15A
      15C000000001FFFFFF00AC65155BB96519F1A1671622FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00DD8C1716C0641AF88F5A
      137C8F60201088531393B15D18F0BC6A18B1FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00D47B1C7AB75E18F8945D
      12809B5715DBBE611AF8C9701A9AFF800002FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C06A1ABBB55E18F8B05B
      18F6BD6119F0D2781B5CFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BD6419E4BC581AFFC268
      1AD7D2831A27FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E68C211FC5651AF7C9701BAAFFAA
      2B06FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E38E1C09E1861E3BFFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      0000F87F0000F01F0000E01F0000E01F0000E01F0000F03F0000FE3F0000FF77
      0000FE770000FEC70000FE0F0000FC3F0000FC7F0000FCFF0000FFFF0000}
    PopupMenu = mnuTray
    OnClick = addTrayClick
    Left = 328
    Top = 64
  end
  object mnuTray: TPopupMenu
    Left = 108
    Top = 88
    object mnuShow: TMenuItem
      Caption = '&Show/Hide'
      Default = True
      OnClick = mnuShowClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Beenden1: TMenuItem
      Action = actExit
    end
  end
  object tmrRecordings: TTimer
    Enabled = False
    Interval = 60000
    OnTimer = tmrRecordingsTimer
    Left = 108
    Top = 144
  end
  object tmrSchedule: TTimer
    Enabled = False
    OnTimer = tmrScheduleTimer
    Left = 172
    Top = 144
  end
  object tmrAutoSave: TTimer
    Enabled = False
    Interval = 600000
    OnTimer = tmrAutoSaveTimer
    Left = 288
    Top = 200
  end
end
