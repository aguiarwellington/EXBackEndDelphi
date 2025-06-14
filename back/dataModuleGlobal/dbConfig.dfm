object configdm: Tconfigdm
  OnCreate = DataModuleCreate
  Height = 480
  Width = 640
  object conn: TFDConnection
    Params.Strings = (
      'Database=appex'
      'User_Name=root'
      'DriverID=MySQL')
    BeforeConnect = connBeforeConnect
    Left = 64
    Top = 48
  end
  object FDPhysMySQLDriverLink: TFDPhysMySQLDriverLink
    VendorLib = 
      'C:\Users\well well\Desktop\Estudo TI\Projetos\AppEx\backend2\Win' +
      '32\Debug\lib\libmysql.dll'
    Left = 200
    Top = 48
  end
end
