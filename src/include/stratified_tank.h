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

C---- COMMON CONTAINING STRATIFIED TANK DATA

      INTEGER N_STANK                  ! Actual number of stratified tanks
      INTEGER IPCOMP_STANK(MAX_STANK)  ! IPCOMPs of stratified tanks
      REAL TP_STANK(MAX_NNODES,MAX_STANK)    ! Past node temperatures
      REAL TF_STANK(MAX_NNODES,MAX_STANK)    ! Future node temperatures
      REAL TAVGP_STANK(MAX_STANK)      ! Past average tank temperature
      REAL TAVGF_STANK(MAX_STANK)      ! Future average tank temperature
      REAL PTIMEF_STANK                ! Current time of tank calculations

      COMMON/STRATIFIED_TANK/N_STANK,IPCOMP_STANK,TP_STANK,TF_STANK,
     &  TAVGP_STANK,TAVGF_STANK,PTIMEF_STANK
