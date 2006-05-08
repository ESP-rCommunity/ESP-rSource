C This file contains the following modules related to ground source heat pumps
C     GCEP       Data concerning the ground coupling

C Created by: Patrice Pinel
C Initial Creation Date: October, 2002
C
C This module declares and initializes all the general input data required for the
C GCEP simulation.
C GCEP stands for: Ground Coupling Ecole Polytechnique
C---------------------------------------------------------------------------------

C--------------------------------------------------------------------------------
C This is the general parameter that defines the maximum number of GCEP systems
C available for simulation (1) 
C--------------------------------------------------------------------------------
      INTEGER max_gcep_systems
      parameter (max_gcep_systems = 1)


C---------------------------------------------------------------------------------
C Modelisation of the borefield (dimensions and mesh)
C---------------------------------------------------------------------------------
C Length of the thermal reservoirs in X and Y
      REAL GCEP_L_res_x
      parameter (GCEP_L_res_x = 500.0)                 ! [m]
      REAL GCEP_L_res_y
      parameter (GCEP_L_res_y = GCEP_L_res_x)               ! [m]

C Number of elements to mesh the reservoirs in X and Y
      INTEGER GCEP_Ndx_res
      parameter ( GCEP_Ndx_res = 20)                ! [elements]

      INTEGER GCEP_Ndy_res
      parameter (GCEP_Ndy_res=GCEP_Ndx_res)           ! [elements]

C Number of elements on each side of the borefields (1/2 times the space between borefields)
      INTEGER GCEP_Ndx_bor_c
      parameter(GCEP_Ndx_bor_c = 5)               ! [elements]

      INTEGER GCEP_Ndy_bor_c
      parameter(GCEP_Ndy_bor_c = GCEP_Ndx_bor_c)       ! [elements]

C Number of elements for borehole+space on each side (1 element forr borehole)
      INTEGER GCEP_Ndx_bor
      parameter (GCEP_Ndx_bor = 2 * GCEP_Ndx_bor_c + 1)     ! [elements]

      INTEGER GCEP_Ndy_bor
      parameter (GCEP_Ndy_bor = 2 * GCEP_Ndy_bor_c + 1)     ! [elements]

C---------------------------------------------------------------------------------
C Time parameters
C---------------------------------------------------------------------------------
C Number of hours in every term of the aggregation scheme (m=month, w=week, d=day, h=number of non-aggregated hours kept)
      INTEGER GCEP_Xm
      parameter (GCEP_Xm = 360)                    ! [hours]

      INTEGER GCEP_Xw
      parameter (  GCEP_Xw = 168    ) ! [hours]

      INTEGER GCEP_Xd
      parameter ( GCEP_Xd = 48 )             ! [hours]

      INTEGER GCEP_Xh
      parameter (GCEP_Xh = 12)               ! [hours]

C Number of ground loads held in the history
      INTEGER GCEP_NHis
      parameter (GCEP_NHis=1+GCEP_Xm+GCEP_Xw+GCEP_Xd+GCEP_Xh) ! [hours]

C Number of hours between numerical calculations of ground temperatures
      REAL GCEP_Inter_Tg
      parameter ( GCEP_Inter_Tg = 336.0 )          ! [hours]







