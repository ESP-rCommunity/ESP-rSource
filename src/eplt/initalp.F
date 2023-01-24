C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C This file contains the following routines:
C      MZNASP

C ******************** MZNASP ********************

C MZNASP assigns starting values to the plant nodal
C state variables.
      SUBROUTINE MZNASP

#include "building.h"
#include "plant.h"
#include "control.h"
      COMMON/C13PS/NPMCOE,NPNOD,NPMTYP
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)

C Integral and derivative control variables.
      common/ctlactn/rintgrl(MCF,2),deriv(MCF)

C Assign state space variable starting values.
      DO 10 IPNOD=1,NPNOD
      CSVF(IPNOD,1)=20.
      CSVP(IPNOD,1)=20.
      CSVF(IPNOD,2)=0.0
      CSVP(IPNOD,2)=0.0
      CSVF(IPNOD,3)=0.0
      CSVP(IPNOD,3)=0.0
      CSVF(IPNOD,4)=0.0
      CSVP(IPNOD,4)=0.0
   10 CONTINUE

C Initialise integral and derivative control 
C variables.
      do 20 icl=1,mcf
         rintgrl(icl,1)=0.0
         rintgrl(icl,2)=0.0
         deriv(icl)  =0.0
  20  continue
      RETURN
      END

