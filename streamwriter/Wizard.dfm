object frmWizard: TfrmWizard
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Setup wizard'
  ClientHeight = 325
  ClientWidth = 430
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
    Width = 413
    Height = 233
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      413
      233)
    object cmdBrowse: TPngSpeedButton
      Left = 388
      Top = 16
      Width = 21
      Height = 21
      Hint = 'Browse...'
      Anchors = [akTop, akRight]
      Flat = True
      Layout = blGlyphRight
      ParentShowHint = False
      ShowHint = True
      OnClick = cmdBrowseClick
      ExplicitLeft = 364
    end
    object txtDir: TLabeledEdit
      Left = 8
      Top = 16
      Width = 373
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Color = 15790320
      EditLabel.Width = 34
      EditLabel.Height = 13
      EditLabel.Caption = 'Folder:'
      ReadOnly = True
      TabOrder = 0
    end
  end
  object pnlMisc: TPanel
    Left = 8
    Top = 248
    Width = 413
    Height = 233
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      413
      233)
    object Label2: TLabel
      Left = 80
      Top = 44
      Width = 21
      Height = 13
      Caption = 'KB/s'
    end
    object Label20: TLabel
      Left = 24
      Top = 100
      Width = 389
      Height = 61
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 
        'Server-assigned streams will be monitored for title changes in t' +
        'he background. Do not disable this option if you have a fast int' +
        'ernet connection (DSL or faster). Monitored streams will not be ' +
        'covered by the bandwidth limit.'
      WordWrap = True
    end
    object chkLimit: TCheckBox
      Left = 8
      Top = 0
      Width = 369
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
      EditLabel.Width = 205
      EditLabel.Height = 13
      EditLabel.Caption = 'Max. bandwidth available to streamWriter:'
      Enabled = False
      MaxLength = 5
      NumbersOnly = True
      TabOrder = 1
    end
    object chkMonitorMode: TCheckBox
      Left = 8
      Top = 76
      Width = 369
      Height = 21
      Caption = 
        'Monitor server-assigned streams for title changes in the backgro' +
        'und'
      TabOrder = 2
      OnClick = chkMonitorModeClick
    end
    object txtMonitorCount: TLabeledEdit
      Left = 24
      Top = 176
      Width = 53
      Height = 21
      EditLabel.Width = 173
      EditLabel.Height = 13
      EditLabel.Caption = 'Max. number of streams to monitor:'
      MaxLength = 2
      NumbersOnly = True
      TabOrder = 3
    end
  end
  object pnlSelectMode: TPanel
    Left = 8
    Top = 8
    Width = 417
    Height = 233
    BevelOuter = bvNone
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 2
    object optModeEasy: TRadioButton
      Left = 8
      Top = 0
      Width = 197
      Height = 21
      Caption = 'Easy mode'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object optModeAdvanced: TRadioButton
      Left = 8
      Top = 24
      Width = 197
      Height = 21
      Caption = 'Advanced mode'
      TabOrder = 1
    end
  end
end
