object frmCommunityLogin: TfrmCommunityLogin
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Community logon'
  ClientHeight = 274
  ClientWidth = 350
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
    FF00FFFFFF00007D21EB037B1EFF00791504FFFFFF00FFFFFF00FFFFFF00FFFF
    FF0069A8DF0D847A7E13B3502098A0401FE3AA4522F5AC4622FAAB4422FAA741
    21F69F3D1FEA0B802BFA43A15FFF027C22CF00791906FFFFFF00FFFFFF006DA2
    D3792579CDFB866161FBBF6035FFFEB961FFFEB962FF239751FF1D9149FF178F
    43FF118B3BFF3A9F5EFF80C196FF46A362FF0B8131E900791907FFFFFF00297D
    D1FE82BAEEFF9F6658FFF5BB84FFFFAC5BFFFEA85AFF299B5BFF90CAA9FF8DC8
    A5FF8AC6A1FF88C59EFF6AB685FF82C297FF48A566FF007D21D700791B09287C
    CEFC78B3EAFFB39E94FFFFB760FFFFB663FFFEB261FF319F63FF94CDADFF6FBA
    8EFF6BB889FF66B685FF61B380FF67B582FF83C298FF3CA05CFF007F25F99E53
    2B508A5444FFFCC8ABFFFFD198FFFEC76DFFFEBF68FF37A36BFF96CEB0FF94CD
    ADFF91CBAAFF90CBA8FF74BC90FF8AC7A1FF46A568FF078735FB01832D01A949
    1301A9460D60C44C1FFFF6E4D6FFFFE4A4FFFFD472FF3DA56FFF3AA36DFF36A1
    67FF329E61FF55AF7CFF91CBAAFF4FAB74FF178F45FB118B3D01FFFFFF00FFFF
    FF00A74A1507A4481369BC481CFFF4E2D4FF4E7BA9FF4D7BA8FF4D7BA8FF4E7B
    A9FFF3D6C3FF3C985DFF5AB381FF289857FFFFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00A54B17056A3C25A2346DA7FF9CCCF8FFAFD4F7FFAFD4F7FFA5CF
    F6FF3474AEFF3A9D68F9319F65FFFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF002A5B92C4A6CAEEFFABCCEAFFA7D0F6FFA8D0F6FFABCC
    EAFFA7CDEEFF2D629ACCFFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF001F5E9BEDD9E8F7FF97C5F1FF8EBBE5FF7FA9D1FF89B5
    DFFFCDDFEEFF2368A7F16AA0D206FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF000C3E87FF7C97B8FF8AB7E4FF719CC8FF15406EFF1944
    72FF22456BFF113B66FA4D78A207FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF000F4B97FF12589FFF0F4A8AFF0F4B87FF114B87FF154C
    85FF124175FF0F335CF1FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF0012336777114E96FE12589BFF125899FF115393FF0F4A
    87FF0E3E71FE132E4B81FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF0014356A7D12488DF4104B90FF0F488AFF1142
    7DF515335B84FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFCF
    0000E007000080030000000100000000000080010000C0030000E0070000E00F
    0000E01F0000E01F0000E01F0000E01F0000F01F0000F83F0000FFFF0000}
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlHeader: TPanel
    Left = 0
    Top = 0
    Width = 350
    Height = 33
    Align = alTop
    BevelOuter = bvNone
    Padding.Left = 2
    Padding.Top = 2
    Padding.Right = 2
    Padding.Bottom = 2
    TabOrder = 0
    object Shape1: TShape
      Left = 2
      Top = 2
      Width = 346
      Height = 29
      Align = alClient
      Brush.Color = clActiveCaption
      Pen.Color = clActiveBorder
      ExplicitLeft = 4
      ExplicitTop = 4
      ExplicitWidth = 342
      ExplicitHeight = 25
    end
    object lblTop: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 340
      Height = 23
      Align = alClient
      AutoSize = False
      Caption = 'Community logon'
      Color = clActiveCaption
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Tahoma'
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
    Top = 234
    Width = 350
    Height = 40
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 1
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
      Height = 27
      Align = alRight
      Caption = '&OK'
      Default = True
      DoubleBuffered = True
      Layout = blGlyphRight
      ParentDoubleBuffered = False
      TabOrder = 0
      OnClick = btnOKClick
    end
    object btnCancel: TBitBtn
      Left = 4
      Top = 9
      Width = 97
      Height = 27
      Align = alLeft
      Caption = '&Cancel'
      DoubleBuffered = True
      Layout = blGlyphRight
      ParentDoubleBuffered = False
      TabOrder = 1
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
      Caption = 'Logging in...'
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
    Left = 4
    Top = 37
    Width = 197
    Height = 192
    TabOrder = 3
    DesignSize = (
      197
      192)
    object lblSignup: TLabel
      Left = 160
      Top = 88
      Width = 32
      Height = 13
      Cursor = crHandPoint
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'Signup'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = lblSignupClick
    end
    object txtPassword: TLabeledEdit
      Left = 8
      Top = 161
      Width = 85
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 50
      EditLabel.Height = 13
      EditLabel.Caption = 'Password:'
      PasswordChar = '*'
      TabOrder = 1
    end
    object txtUsername: TLabeledEdit
      Left = 8
      Top = 120
      Width = 85
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      EditLabel.Width = 52
      EditLabel.Height = 13
      EditLabel.Caption = 'Username:'
      TabOrder = 0
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
      TabOrder = 2
    end
  end
end
