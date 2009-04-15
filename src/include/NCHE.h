C ==================================================================================
C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre
C Natural Resources Canada, Government of Canada
C 2008. Please Contact Ian Beausoleil-Morrison for details
C concerning licensing.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.
C
C======================== NCHE.H ===================================================
C
C This header contains data structures associated with the natural
C convection heat exchanger (NCHE) model.
C
C-----------------------------------------------------------------------------------
C     Parameters
C-----------------------------------------------------------------------------------

C     Maximum number of NCHEs in a simulation
C     Except if there is a good reason to do so, this number should
C     normally be the same as the maximum number of stratified tanks
C     in a simulation (see parameter MAX_STANK in stratified_tank.h)

      INTEGER MAX_NCHE
      PARAMETER (MAX_NCHE=5)

C-----------------------------------------------------------------------------------
C     Data structures
C-----------------------------------------------------------------------------------
    
C.....Model parameters
      COMMON/NCHE/N_NCHE,
     &           IPCOMP_NCHE,
     &           fHX_eff_CR_a, fHX_eff_CR_b,
     &           fHX_flow_P_c, fHX_flow_P_d,
     &           fHX_height,
     &           fTank_Height, 
     &           iStrat_tank_CompIndex,
     &	         iStrat_tank_number

C.....Actual number of natural convection heat exchangers in system
      INTEGER N_NCHE

C.....IPCOMPs of NCHEs
      INTEGER IPCOMP_NCHE(MAX_NCHE)

C.....Modified effectiveness vs. capacitance ratio coefficients
      REAL fHX_eff_CR_a(MAX_NCHE)
      REAL fHX_eff_CR_b(MAX_NCHE)
      
C.....Natural convection flow rate vs. pressure drop coefficients
      REAL fHX_flow_P_c(MAX_NCHE)
      REAL fHX_flow_P_d(MAX_NCHE)

C.....Heat exchanger height (m)
      REAL fHX_height(MAX_NCHE)  

C.....Height of connected storage tank (m)
      REAL fTank_Height(MAX_NCHE)

C.....Component number of attached stratified storage tank
      INTEGER iStrat_tank_CompIndex(MAX_NCHE)

C.....Number of the stratified storage tank component attached to the NCHE component,
      INTEGER iStrat_tank_number(MAX_NCHE)

