object frmTimers: TfrmTimers
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Setup timers'
  ClientHeight = 413
  ClientWidth = 410
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
    FF00FFFFFF00FFFFFF00FFFFFF00617D9508537591004E7293E14D7497F64D74
    97F64E7293E1527490005E7A9108FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF005D7C950150789AF35A97B1FF85BDCEFFA4D6DEFFA0D5
    DEFF7BB9CBFF4F8CABFF4D7294F459768F04FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF0061819A015588A6FF96C8D6FFCCCABFFFC89F86FFC59276FFC490
    73FFC49A80FFC3C4B9FF7DBDCDFF477798FF5A778F00FFFFFF00FFFFFF00FFFF
    FF006B879D015782A2F6A2CCD8FFCDB7A5FFD8AA89FFEFDFCFFFFAF6F3FFFAF5
    F3FFEFDFD1FFD7A785FFC3AC98FF82BDCEFF4D7194F7627B9101FFFFFF00FFFF
    FF005E85A00278A9C0FFDAD1C6FFD9AA88FFFFFEEBFFFFFAEEFFFFFDF2FFFFFE
    F2FFFFFCEFFFFFFDEBFFD9A37AFFCBC6B9FF5894B1FF54749001FFFFFF00FFFF
    FF005E8EADDEB7D3DDFFD1AA90FFF0DBC1FFFFF7E6FFFAF2E3FFC5C0B6FFF1ED
    DEFFFFFCE8FFFFF5DFFFEFD4AFFFC99B7BFF92C2D0FF507595E2FFFFFF00FFFF
    FF00679AB8F7DDEAECFFCF9F81FFFAF3E2FFFDF6E9FFF2F0E3FF8D8C84FF817F
    77FFD2CFC2FFFFFBEBFFFAECCCFFC7906DFFC1DDE1FF53799DF6FFFFFF00FFFF
    FF006CA1BDF7E4ECEEFFD1A58AFFFAF6E9FFFDF8EEFFFFFFF9FF818079FFB1AE
    A4FF817F77FFC9C6BAFFFAF0D3FFC99572FFC9DFE2FF547B9EF6FFFFFF00FFFF
    FF006DA2BEE7D3E1E6FFDBB9A4FFF1E3D6FFFEFBF5FFF5F5F3FF9F9E95FFF0F0
    E9FFF8F8F2FFBEB9ADFFF0DAC2FFD1A78BFFA9CAD3FF537B9BDFFFFFFF00FFFF
    FF0077A1BCB6B7CFDBFFEDE0D7FFDEBDAAFFFFFFFFFFF0F0EEFFE7E5DAFFFFFF
    F7FFFFFFF7FFFFFEF3FFDCB498FFE1D1C4FF73A4BBFF597C9804FFFFFF00FFFF
    FF0088A3B93C74B3CCFADFE9ECFFE6CFC2FFDFBFAEFFF1E3DCFFF3F1ECFFF4F1
    EBFFF1E2D9FFDCBAA3FFDEC1AFFFB7CFD8FF5883A4F56A869B01FFFFFF00FFFF
    FF00FFFFFF0085A8C00297CCDCFFE1EBEEFFEEE1D9FFDFBEACFFD5AD96FFD4AA
    92FFDCB7A3FFE8DACEFFC6D9DFFF689BB6FF64859F07FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF0086AAC20A78BAD2F9C8D9E3FFDDE7EAFFEEF1F1FFECF0
    F0FFD2E0E4FFA8C4D2FF679CB8F46C8FA90CFFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF008AA8BF077EABC61378B3CDED7BB8D0FA7AB4
    CDFA71A9C4E8719DB9077A99B002FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
    0000FC3F0000F00F0000E0070000C0030000C003000080010000800100008001
    00008001000080030000C0030000E0070000F00F0000FC3F0000FFFF0000}
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlNav: TPanel
    Left = 0
    Top = 364
    Width = 410
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    Padding.Left = 4
    Padding.Top = 4
    Padding.Right = 4
    Padding.Bottom = 4
    TabOrder = 0
    ExplicitLeft = -186
    ExplicitTop = 300
    ExplicitWidth = 609
    object Bevel2: TBevel
      Left = 4
      Top = 4
      Width = 402
      Height = 5
      Align = alTop
      Shape = bsTopLine
      ExplicitLeft = -7
      ExplicitWidth = 396
    end
    object btnOK: TBitBtn
      Left = 309
      Top = 9
      Width = 97
      Height = 36
      Align = alRight
      Caption = '&OK'
      Default = True
      DoubleBuffered = False
      Layout = blGlyphRight
      ParentDoubleBuffered = False
      TabOrder = 0
      OnClick = btnOKClick
      ExplicitLeft = 508
    end
    object btnCancel: TBitBtn
      Left = 4
      Top = 9
      Width = 97
      Height = 36
      Align = alLeft
      Caption = '&Cancel'
      DoubleBuffered = False
      Layout = blGlyphRight
      ParentDoubleBuffered = False
      TabOrder = 1
      OnClick = btnCancelClick
      ExplicitLeft = 508
    end
  end
  object pnlConfig: TPanel
    Left = 0
    Top = 0
    Width = 410
    Height = 233
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object Label1: TLabel
      Left = 4
      Top = 152
      Width = 53
      Height = 13
      Caption = 'Start Time:'
    end
    object Label2: TLabel
      Left = 128
      Top = 152
      Width = 47
      Height = 13
      Caption = 'End Time:'
    end
    object rbRecurring: TRadioButton
      Left = 4
      Top = 8
      Width = 221
      Height = 21
      Caption = 'Recurring schedule'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbRecurringClick
    end
    object rbDate: TRadioButton
      Left = 4
      Top = 92
      Width = 221
      Height = 21
      Caption = 'Specific date'
      TabOrder = 1
      OnClick = rbDateClick
    end
    object lstInterval: TComboBox
      Left = 20
      Top = 32
      Width = 204
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 2
      Text = 'Daily'
      OnChange = lstIntervalChange
      Items.Strings = (
        'Daily'
        'Weekly')
    end
    object lstDay: TComboBox
      Left = 20
      Top = 60
      Width = 204
      Height = 21
      Style = csDropDownList
      TabOrder = 3
      OnChange = lstDayChange
      Items.Strings = (
        'Monday'
        'Tuesday'
        'Thursday'
        'Wednesday'
        'Friday'
        'Saturday'
        'Sunday')
    end
    object dtpDate: TDateTimePicker
      Left = 20
      Top = 116
      Width = 204
      Height = 21
      Date = 40674.868549270830000000
      Time = 40674.868549270830000000
      TabOrder = 4
      OnChange = dtpDateChange
    end
    object btnAdd: TButton
      Left = 4
      Top = 196
      Width = 97
      Height = 29
      Caption = '&Add'
      TabOrder = 5
      OnClick = btnAddClick
    end
    object txtStartHour: TEdit
      Left = 4
      Top = 168
      Width = 37
      Height = 21
      MaxLength = 2
      NumbersOnly = True
      TabOrder = 6
    end
    object txtStartMinute: TEdit
      Left = 60
      Top = 168
      Width = 37
      Height = 21
      MaxLength = 2
      NumbersOnly = True
      TabOrder = 7
    end
    object txtEndHour: TEdit
      Left = 128
      Top = 168
      Width = 37
      Height = 21
      MaxLength = 2
      NumbersOnly = True
      TabOrder = 8
    end
    object txtEndMinute: TEdit
      Left = 184
      Top = 168
      Width = 37
      Height = 21
      MaxLength = 2
      NumbersOnly = True
      TabOrder = 9
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 233
    Width = 410
    Height = 131
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitLeft = 4
    ExplicitTop = 244
    ExplicitWidth = 397
    ExplicitHeight = 117
    object Panel3: TPanel
      Left = 0
      Top = 93
      Width = 410
      Height = 38
      Align = alBottom
      BevelOuter = bvNone
      Padding.Left = 4
      Padding.Top = 4
      Padding.Right = 4
      Padding.Bottom = 4
      TabOrder = 0
      ExplicitLeft = 1
      ExplicitTop = 92
      ExplicitWidth = 408
      object btnRemove: TButton
        Left = 4
        Top = 4
        Width = 96
        Height = 30
        Align = alLeft
        Caption = '&Remove'
        Enabled = False
        TabOrder = 0
        ExplicitLeft = 8
      end
    end
  end
end