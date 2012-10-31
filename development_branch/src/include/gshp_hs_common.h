

C ************************************************************************************
C ********************************** HS_INPUT ****************************************
C Created by: Julia Purdy
C Initial Creation Date: January 7, 2002
C Copyright CETC 2001

C This common declares variables specific to the horizontal ground source heat
C pump configuration.
C---------------------------------------------------------------------------------
      common/HS_INPUT/Ks,Kw,Ds,Dw,BD,SD,PD1,PD2

C---------------------------------------------------------------------------------
C Soil properties (conductivity and diffusivity).
C---------------------------------------------------------------------------------
C [W/mK]
      REAL  Ks
C [W/mK]
      REAL  Kw
C [m2/s]
      REAL  Ds
C [m2/s]
      REAL  Dw   

C---------------------------------------------------------------------------------
C Depth of heat exchanger pipe and distance between horizontal pipes
C---------------------------------------------------------------------------------
C burial depth [m]
      REAL  BD
C 2-pipe [m]
      REAL  SD
C 4-pipe [m]
      REAL  PD1
C 4-pipe [m]
      REAL  PD2

C ********************************************************************************


   









 
