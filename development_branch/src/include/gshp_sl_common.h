

C ********************************* SL_INPUT *************************************
C Created by: Julia Purdy
C Initial Creation Date: January 7, 2002
C Copyright CETC 2001

C This common declares variables specific to the horizontal slinky ground source heat
C pump configuration.
C---------------------------------------------------------------------------------
       common/SL_INPUT/Ks,Kw,Ds,Dw,BD,Spiral,NumP

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
C Slinky configuration parameters
C---------------------------------------------------------------------------------
C Depth of heat exchanger pipes
C  [m]
      REAL  BD
C Diameter of slinky spirals
C  [m]
      REAL  Spiral
C Number of pipes per trench
      REAL  NumP









 
