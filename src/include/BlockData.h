C===================== Block data ======================================
C     Created by: Didier Thevenard
C     Created on: April 2007
C     Copyright:  CETC 2007
C-----------------------------------------------------------------------
C     ABSTRACT:
C     Block data to initialize variables in the STRATIFIED_TANK and in 
C     the NCHE commons
C
C=======================================================================

      BLOCK DATA

C---- Include commons

      INCLUDE "stratified_tank.h"
      INCLUDE "NCHE.h"
      INCLUDE "plant.h"

C---- Initialize variables in the STRATIFIED_TANK common

      INTEGER NAUX
      PARAMETER(NAUX=MAX_NNODES*MAX_STANK)

      DATA N_STANK/0/
     &     IPCOMP_STANK/MAX_STANK*0./
     &     TP_STANK/NAUX*0./
     &     TF_STANK/NAUX*0./
     &     TAVGP_STANK/MAX_STANK*0./
     &     TAVGF_STANK/MAX_STANK*0./
     &     PTIMEF_STANK/-9.9999E+10/

C---- Initialize variables in the NCHE common

      DATA N_NCHE/0/
     &     IPCOMP_NCHE/MAX_NCHE*0/
     &     fHX_eff_CR_a/MAX_NCHE*0./
     &     fHX_eff_CR_b/MAX_NCHE*0./
     &     fHX_flow_P_c/MAX_NCHE*0./
     &     fHX_flow_P_d/MAX_NCHE*0./
     &     fHX_height/MAX_NCHE*0./
     &     fTank_Height/MAX_NCHE*0./
     &     iStrat_tank_CompIndex/MAX_NCHE*0/
     &     iStrat_tank_number/MAX_NCHE*0/

C----  Initialize vars for radiant floor 
      COMMON/RadFloor/RadFloor_active, NRadFloor, IRadFloor, 
     &                InitRadFloor
      LOGICAL RadFloor_active
      INTEGER NRadFloor
      INTEGER IRadFloor(MPCOM), InitRadFloor(MPCOM)
      data RadFloor_active/.false./,NRadFloor/0.0/,IRadFloor/MPCOM*0.0/,
     &      InitRadFloor/MPCOM*0.0/

C---- End of common

      END


