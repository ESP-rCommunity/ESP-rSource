C ====================================================================
C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre
C Natural Resources Canada, Government of Canada
C 2007. Please Contact Ian Beausoleil-Morrison for details
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

C===================== COMMON STRATIFIED_TANK ==========================
C     Created by: Didier Thevenard
C     Created on: April 2007
C     Copyright:  CETC 2007
C-----------------------------------------------------------------------
C     ABSTRACT:
C     Common required by the stratified tank model
C
C=======================================================================

C---- PARAMETERS

C     Maximum number of stratified tanks in a simulation

      INTEGER MAX_STANK
      PARAMETER (MAX_STANK=5)

C     Maximum number of nodes in a stratified tank

      INTEGER MAX_NNODES
      PARAMETER (MAX_NNODES=100)

C     Maximum number of cells in radial and axial direction and total in PCM module
      INTEGER MX, MY, MXMY
      PARAMETER (MX=100, MY=100, MXMY=1000)      
      
C     Maximum number of PCM sections in axial direction
      INTEGER MPCMSEC
      PARAMETER (MPCMSEC=100)
      
C     PI is the number pi     
      REAL PI
      PARAMETER (PI=3.1415926535897932385)
     
      
C---- COMMON CONTAINING STRATIFIED TANK DATA

      INTEGER N_STANK                  ! Actual number of stratified tanks
      INTEGER IPCOMP_STANK(MAX_STANK)  ! IPCOMPs of stratified tanks
      REAL TP_STANK(MAX_NNODES,MAX_STANK)    ! Past node temperatures
      REAL TF_STANK(MAX_NNODES,MAX_STANK)    ! Future node temperatures
      REAL TAVGP_STANK(MAX_STANK)      ! Past average tank temperature
      REAL TAVGF_STANK(MAX_STANK)      ! Future average tank temperature
      REAL PTIMEF_STANK                ! Current time of tank calculations
      
C---- COMMON CONTAINIG PCM MODULE DATA

      INTEGER NSIZEPCM(MAX_STANK,2)
      REAL HPCM_MOD(MXMY,MAX_STANK)    ! PCM module enthalpy (for each cell)
      REAL TPCM_MOD(MXMY,MAX_STANK)    ! PCM module temperature (for each cell)
      REAL TM                          ! Phase Change temperature [C]
      PARAMETER (TM=0.0)               ! Scaling temprature [C]
      REAL HPCMSEC(MY,MAX_STANK)       ! PCM section height [m]
      REAL DR(MAX_STANK)               ! Grid dimension in radial direction [m]
      REAL DPCMNODE(MY-1,MAX_STANK)    ! Distance among PCM nodes [m]
      INTEGER NPCMDATA(4,MAX_STANK)    ! nX,nY,PCMbot,nPCMtop
      REAL AXB(MY,MAX_STANK)           ! Border face of external CV, in contact with DHW [m^2]
      REAL AY(MX,MAX_STANK)            ! CV faces in axial direction  (north-south) [m^2]
      REAL AX(MXMY-MY,MAX_STANK)       ! CV faces in radial direction (east-west) [m^2]
      REAL V(MXMY,MAX_STANK)           ! Cells volumes [m^3]
      


      
      COMMON/STRATIFIED_TANK/N_STANK,IPCOMP_STANK,TP_STANK,TF_STANK,
     &  TAVGP_STANK,TAVGF_STANK,PTIMEF_STANK,NSIZEPCM,
     &  HPCM_MOD,TPCM_MOD, NPCMDATA, HPCMSEC, DPCMNODE,
     &  AX,AXB,AY,V,DR


