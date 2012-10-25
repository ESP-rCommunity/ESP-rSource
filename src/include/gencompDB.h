C ----------------------------------------------------------------------
C Constant initialisation
C ----------------------------------------------------------------------
      INTEGER Max_Domain_Types,Max_Categories,Max_Items,Max_DB_Items  
      INTEGER Max_Vert,Max_Edge,Max_Dots,Max_Conn_P,Max_Text_Desc
      INTEGER Max_Parameters,Max_Graphic_Data,Max_Datcat
           
      PARAMETER(Max_Domain_Types=5,Max_Categories=20)
      PARAMETER(Max_Items=50)
      PARAMETER(Max_Parameters=250,Max_Datcat=10)
      PARAMETER(Max_Text_Desc=60)
      PARAMETER(Max_Graphic_Data=100)
      PARAMETER(Max_DB_Items=Max_Domain_Types*Max_Categories*Max_Items)
      PARAMETER(Max_Vert=30,Max_Edge=30,Max_Dots=10,Max_Conn_P=10)
C ----------------------------------------------------------------------





