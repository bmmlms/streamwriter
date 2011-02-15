object frmCommunityLogin: TfrmCommunityLogin
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Community login'
  ClientHeight = 300
  ClientWidth = 350
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Icon.Data = {
    0000010001001010000001002000680400001600000028000000100000002000
    000001002000000000000000000000000000000000000000000000000000B170
    4117AB693DFFA75F39F69F57353FFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00BA7B
    4AFFC0895FFFBF8962FFAF6D47FFA25A3741FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C388
    59FFCFA27DFFCDA280FFC08C66FFB07249FFA55D38959F57344E985031399449
    2E1E8F432A04FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CA91
    6671CC9971FFD0A381FFCFA483FFCA9E7BFFBC855DFFAF7149FFA76440FF9E57
    38FF974A30C68F432C09FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00CC946972CE9D76FFD5AC8CFFCB9B76FFCCA07CFFC89B76FFC5956FFFC08F
    69FFAC6D48FF984D3176FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00CE986DB1D3A886FFD6AC8EFFC99871FFC49068FFBF8A5FFFC28F
    67FFBF8B64FF7F6847FB864E3229FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00D6A27C4ED4A37EFFDCB598FFD0A17DFFCC9A74FFCFA483FFC89A
    76FF7D8C64FF6AB87AFF8C5035FB8E412AAF88402709FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00DDAD8839DBAB89FFE1BDA2FFD6AA87FFD9B394FFCE9F7AFF9589
    5FFB6DBC7CFF936F46FFB0754FFFA46241FF8F432BDA8B412804FFFFFF00FFFF
    FF00FFFFFF00E5B4921EE0B18FFFE6C4ABFFE2BFA4FFD8AD8EFFA89471FAAB8A
    5777A47848FCBB845DFFC08F69FFBC8A61FF9C5235FF91442C1EFFFFFF00FFFF
    FF00FFFFFF00E9BD9E04E5B797DAE4BB9FFFE4BB9FFFA0A685FC76C17FFFB08F
    5AFCC7966FFFCB9E7BFFBC8559FFC3926CFFA6633FFF9A503139FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00EABE9F09E6B89AAFBAB292F977C582FFB5B081FFD8B0
    92FFD7AE8FFFC9976FFFC38F66FFC89B76FFB1714AFFA25A374EFFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00DBBB9826C5B386FDE2BEA3FFDFB7
    9AFFD5A886FFD0A17DFFCB9A73FFCEA280FFBF8B62FFAB683C95FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00E5B59576E4BA9DFFE6C4
    ABFFE2BEA4FFDEB99CFFD9B293FFD1A37FFFD1A685FFBB7F51FFAE6A3E3FFFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EABE9F09E6B898C6E3B4
    93FFDFB18EFFDAAB89FFDAAD8CFFDCB598FFD7AF90FFCC9B74FFB77844FAFFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00ECC0A104E7BB
    9C1EE5B59439DFAE8B4ED9A78194D8A782FFD7AC8BFFD3A784FFC18556F6FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00DCAA853ED4A17CFBD09A70F6CA9166489FFF
    00000FFF000003FF0000803F0000C03F0000C01F0000E0070000E0030000E083
    0000E0030000F0030000FC010000FE010000FE000000FFE00000FFF90000}
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 350
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 0
    object Shape1: TShape
      Left = 4
      Top = 4
      Width = 342
      Height = 33
      Align = alClient
      Brush.Color = clActiveCaption
      Pen.Color = clActiveBorder
      ExplicitLeft = -276
      ExplicitWidth = 801
      ExplicitHeight = 29
    end
    object lblTop: TLabel
      AlignWithMargins = True
      Left = 10
      Top = 9
      Width = 333
      Height = 25
      Margins.Left = 6
      Margins.Top = 5
      Align = alClient
      AutoSize = False
      Caption = 'Community login'
      Color = clActiveCaption
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      GlowSize = 1
      ParentColor = False
      ParentFont = False
      Layout = tlCenter
      ExplicitLeft = 76
      ExplicitTop = 4
      ExplicitWidth = 190
      ExplicitHeight = 30
    end
  end
  object pnlNav: TPanel
    Left = 0
    Top = 251
    Width = 350
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 1
    ExplicitTop = 260
    object Bevel2: TBevel
      Left = 4
      Top = 4
      Width = 342
      Height = 5
      Align = alTop
      Shape = bsTopLine
      ExplicitLeft = -7
      ExplicitWidth = 396
    end
    object btnOK: TBitBtn
      Left = 249
      Top = 9
      Width = 97
      Height = 36
      Align = alRight
      Caption = '&OK'
      Default = True
      DoubleBuffered = True
      Layout = blGlyphRight
      ParentDoubleBuffered = False
      TabOrder = 1
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 4
      Top = 9
      Width = 97
      Height = 36
      Align = alLeft
      Caption = '&Cancel'
      DoubleBuffered = True
      Layout = blGlyphRight
      ParentDoubleBuffered = False
      TabOrder = 0
      OnClick = btnCancelClick
    end
  end
  object pnlConnecting: TPanel
    Left = 216
    Top = 108
    Width = 121
    Height = 89
    TabOrder = 2
    DesignSize = (
      121
      89)
    object lblConnecting: TLabel
      Left = 4
      Top = 16
      Width = 113
      Height = 21
      Alignment = taCenter
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = 'Connecting...'
      ExplicitWidth = 205
    end
    object prgConnecting: TProgressBar
      Left = 4
      Top = 44
      Width = 113
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      Style = pbstMarquee
      TabOrder = 0
    end
  end
  object pnlConnect: TPanel
    Left = 0
    Top = 41
    Width = 197
    Height = 204
    TabOrder = 3
    DesignSize = (
      197
      204)
    object lblSignup: TLabel
      Left = 159
      Top = 88
      Width = 33
      Height = 13
      Cursor = crHandPoint
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'Signup'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = lblSignupClick
      ExplicitLeft = 312
    end
    object txtPassword: TLabeledEdit
      Left = 8
      Top = 149
      Width = 60
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 50
      EditLabel.Height = 13
      EditLabel.Caption = 'Password:'
      TabOrder = 0
      ExplicitWidth = 213
    end
    object chkSaveData: TCheckBox
      Left = 8
      Top = 177
      Width = 181
      Height = 21
      Caption = 'Save login data'
      TabOrder = 1
    end
    object txtUsername: TLabeledEdit
      Left = 8
      Top = 108
      Width = 60
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 52
      EditLabel.Height = 13
      EditLabel.Caption = 'Username:'
      TabOrder = 2
      ExplicitWidth = 213
    end
    object txtText: TMemo
      Left = 4
      Top = 8
      Width = 188
      Height = 73
      TabStop = False
      Anchors = [akLeft, akTop, akRight]
      BorderStyle = bsNone
      Color = clBtnFace
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 3
      ExplicitWidth = 341
    end
  end
end
