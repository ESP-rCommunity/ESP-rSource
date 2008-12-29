C ********************************************************************************
C ********************************* V1_INPUT *************************************

C Created by: Julia Purdy
C Initial Creation Date: January 7, 2002
C Copyright CETC 2001

C This common declares variables specific to the vertical ground source heat pump
C configuration
C---------------------------------------------------------------------------------
      common/V1_INPUT/Nsoil,Depth(max_layer),Nbore,
     & Utube,SP,Dtop,Dbottom,Dbore,Kgrout,
     & Ks(max_layer),Kw(max_layer),
     & Ds(max_layer),Dw(max_layer)

C---------------------------------------------------------------------------------
C Soil Layer Properties
C---------------------------------------------------------------------------------

C Number of soil layers
      INTEGER  Nsoil

C Depth of each soil layer
      REAL  Depth

C Number of boreholes
      INTEGER  Nbore

C---------------------------------------------------------------------------------
C Vertical U-tube Properties
C---------------------------------------------------------------------------------
C Number of U-bends per borehole
      REAL  Utube

C Distance betwteen pipes in U-tube
C [cm]
      REAL  SP           

C Distance below surface of the top of U-tube
C [m]
      REAL  Dtop         

C Depth below surface of bottom of U-tube
C [m}
      REAL  Dbottom

C Bore hole Diameter 
C [cm]
      REAL  Dbore

C Grout conductivity 
C {W/mC]
      REAL  Kgrout

C---------------------------------------------------------------------------------
C Soil Properties per Layer
C---------------------------------------------------------------------------------
C Soil conductivity in summer and winter
C [W/mK]
      REAL  Ks
C [W/mK]
      REAL  Kw
C
C Soil diffusivity in summer and winter
C [m2/s]
      REAL  Ds
C [m2/s]
      REAL  Dw

C ************************************************************************************
