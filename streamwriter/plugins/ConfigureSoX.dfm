object frmConfigureSoX: TfrmConfigureSoX
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Configure SoX'
  ClientHeight = 206
  ClientWidth = 397
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    000001002000000000000000000000000000000000000000000000000000FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00B25E2AFFAA5A28FFA65727FFA15426FF985024F7FFFFFF00FFFFFF008C4A
    21FD894820FD86461FF484451FE6FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00B65F2BFFC98459FFC88256FFC77F53FFA35526FFFFFFFF00FFFFFF00924D
    22FFC57A4CFFC57748FF86461FFFFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00BE642CFFCA855CFFC06730FFC98459FFC06730FFA35526FF9E5325FFB55F
    2BFFC67B4DFFC57A4CFF894820FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00C06832FFCB8960FFCA855CFFC37341FFC98459FFC88256FFC88256FFC77F
    53FFC06832FFC77E52FF8E4A21FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00C26E3BFFC06832FFC37545FFCB8960FFC06832FFC06730FFBF652DFFBB62
    2CFFB7602CFFC77F53FFB35E2BFF8E4A21F98C4A21FF88471FFFFFFFFF00FFFF
    FF00FFFFFF00FFFFFF00C16A35FFCB8A62FFC16B37FFC06832FFC06832FFC067
    30FFBB622CFFC26E3BFFC77F53FFC77E52FFC77E52FF8C4A21FFFFFFFF00FFFF
    FF00FFFFFF00FFFFFF00C36F3CFFCC8C65FFC36F3CFFC16C39FFC16B37FFC068
    32FFC06730FFC36F3CFFC88256FFC77F53FFC77F53FF904C22FFFFFFFF00FFFF
    FF00C5784AFFC47645FFC67C4FFFCD8E68FFC37341FFC3703EFFC16C39FFC16C
    39FFC06832FFCA855CFFC06730FFA35526FF9C5224FF954F23FFFFFFFF00FFFF
    FF00C67C4FFFCF936FFFCD906BFFC77F53FFC47645FFC37341FFC37341FFC373
    41FFC37140FFCB8960FFAD5B29FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00C77F53FFCF9471FFCF936FFFCD906BFFCD906BFFCD8E68FFCD8E68FFCC8B
    63FFCB8960FFCA855CFFB35E2BFFFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00C88256FFC77F53FFC67C4FFFC67B4DFFC5784AFECD906BFFCD8E68FFC26E
    3BF0C06832EDBF652DEFB9612CEFFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C67B4DF4CE916CFFCC8C65FFC371
    40BEFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00C8825612C77F53FFCF9471FFCE916CFFC476
    45ECFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C88256D4C77F53FFC67C4FFEC578
    4AB4FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    0000830F0000830F0000800F0000800F000080010000E0010000E00100008001
    0000800F0000800F0000800F0000F87F0000F87F0000F87F0000FFFF0000}
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 76
    Width = 381
    Height = 5
    Shape = bsTopLine
  end
  object chkFadeoutStart: TCheckBox
    Left = 8
    Top = 8
    Width = 181
    Height = 21
    Caption = 'Add fadein at beginning'
    TabOrder = 0
    OnClick = chkClick
  end
  object chkFadeoutEnd: TCheckBox
    Left = 208
    Top = 8
    Width = 181
    Height = 21
    Caption = 'Add fadeout at end'
    TabOrder = 1
    OnClick = chkClick
  end
  object pnlNav: TPanel
    Left = 0
    Top = 157
    Width = 397
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 2
    object Bevel2: TBevel
      Left = 4
      Top = 4
      Width = 389
      Height = 5
      Align = alTop
      Shape = bsTopLine
      ExplicitLeft = -7
      ExplicitWidth = 396
    end
    object btnOK: TBitBtn
      Left = 296
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
  end
  object txtFadeoutStart: TLabeledEdit
    Left = 24
    Top = 48
    Width = 53
    Height = 21
    EditLabel.Width = 90
    EditLabel.Height = 13
    EditLabel.Caption = 'Length in seconds:'
    Enabled = False
    MaxLength = 1
    NumbersOnly = True
    TabOrder = 3
  end
  object txtFadeoutEnd: TLabeledEdit
    Left = 224
    Top = 48
    Width = 53
    Height = 21
    EditLabel.Width = 90
    EditLabel.Height = 13
    EditLabel.Caption = 'Length in seconds:'
    Enabled = False
    MaxLength = 1
    NumbersOnly = True
    TabOrder = 4
  end
  object chkSilenceStart: TCheckBox
    Left = 8
    Top = 88
    Width = 181
    Height = 21
    Caption = 'Add silence at beginning'
    TabOrder = 5
    OnClick = chkClick
  end
  object chkSilenceEnd: TCheckBox
    Left = 208
    Top = 88
    Width = 181
    Height = 21
    Caption = 'Add silence at end'
    TabOrder = 6
    OnClick = chkClick
  end
  object txtSilenceStart: TLabeledEdit
    Left = 24
    Top = 128
    Width = 53
    Height = 21
    EditLabel.Width = 90
    EditLabel.Height = 13
    EditLabel.Caption = 'Length in seconds:'
    Enabled = False
    MaxLength = 1
    NumbersOnly = True
    TabOrder = 7
  end
  object txtSilenceEnd: TLabeledEdit
    Left = 224
    Top = 128
    Width = 53
    Height = 21
    EditLabel.Width = 90
    EditLabel.Height = 13
    EditLabel.Caption = 'Length in seconds:'
    Enabled = False
    MaxLength = 1
    NumbersOnly = True
    TabOrder = 8
  end
end
