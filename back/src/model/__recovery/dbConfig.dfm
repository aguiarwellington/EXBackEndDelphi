object configdm: Tconfigdm
  Height = 480
  Width = 640
  object conn: TFDConnection
    Params.Strings = (
      'Database=appex'
      'User_Name=root'
      'DriverID=MySQL')
    BeforeConnect = connBeforeConnect
    Left = 64
    Top = 80
  end
  object FDPhysMySQLDriverLink: TFDPhysMySQLDriverLink
    VendorLib = 'C:\projeto\EXBackEndDelphi\back\dll\libmysql.dll'
    Left = 248
    Top = 56
  end
end
