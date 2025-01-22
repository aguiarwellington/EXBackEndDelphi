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
    VendorLib = 
      'C:\Users\well well\Desktop\Estudo TI\Projetos\AppEx\backend\src\' +
      'config\libmysql.dll'
    Left = 264
    Top = 88
  end
end
