C This file contains the following parameters related to the implementation
C of the AIM-2 infiltration model:


C---------------------------------------------------------------------------------
C Constant initialization.
C---------------------------------------------------------------------------------

C PI is initialized here because FORTRAN lacks an intrinsic function for PI.
        REAL PI        
        PARAMETER (PI = 3.14156)

C Gravitational acceleration (m/s^2).
        REAL gravity
        PARAMETER (gravity = 9.81)

C Interaction coefficient between wind and stack effects. Defined as
C a constant by Walker and Wilson (1993, p4) based on empirical
C observations.
        REAL AIM2_B1 
        PARAMETER ( AIM2_B1 = -1./3.)








