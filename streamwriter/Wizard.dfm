object frmWizard: TfrmWizard
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Setup wizard'
  ClientHeight = 300
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
    Width = 389
    Height = 233
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      389
      233)
    object cmdBrowse: TPngSpeedButton
      Left = 364
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
    end
    object txtDir: TLabeledEdit
      Left = 8
      Top = 16
      Width = 349
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
