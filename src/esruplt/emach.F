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

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

********** EMACH *****************
C Subroutine emach.f is used to calculate the power characteristics of an electrical 
C machine associated a plant component. Currently only rudumentary calculations are 
C performed. 

      subroutine EMACH(IPCOMP,IEMODEL,PR,PQ,PA)

# include "plant.h"
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C12PS/NPCDAT(MPCOM,9),IPOFS1(MCOEFG),IPOFS2(MCOEFG,MPVAR)

C Loop through each component to see if calculation is necessary.

      IF(IEMODEL.EQ.1)THEN
        CALL EMACH001(IPCOMP,PR,PQ,PA)
      elseif (iemodel .eq. 2) then
        call emach002(ipcomp,PR,PQ)
      ENDIF
      
      RETURN
      END

****** MACH1 ***********
C Subroutine mach1 calcuates the reactive power component and
C voltage vector of a plant component from power factor information.  

      SUBROUTINE EMACH001(IPCOMP,PR,PQ,PA)

      INCLUDE 'plant.h'

C Common block for load flow simulation
C Plant
      COMMON/ENETPCOMP/PPCMP(MPCOM),QPCMP(MPCOM),
     &VCMP(MPCOM)

      common/elpcp/NPEL,PFP(mpcom),IPFP(mpcom),PWRP(mpcom),
     &BVOLTP(mpcom),IPHP(mpcom)

      LOGICAL CLOSE
      
      COMPLEX VCMP

C Calculate the reactive power component. And save to load flow
C simulation common.

      CALL ECLOSE(PFP(IPCOMP),0.000,0.00001,CLOSE)
      IF(CLOSE)THEN     
        PPCMP(IPCOMP)=PR  
        PA=PR
        QPCMP(IPCOMP)=0.0
      ELSE
        PANG=ACOS(PFP(IPCOMP))
        PPCMP(IPCOMP)=PR     
        QPCMP(IPCOMP)=PWRP(IPCOMP)*TAN(PANG)*(IPFP(IPCOMP))
        PQ=QPCMP(IPCOMP)
        PA=(QPCMP(IPCOMP)**2.+PPCMP(IPCOMP)**2.)**(0.5)
      ENDIF

      RETURN
      END


****** MACH2 **********
C Subroutine mach2 saves the dc and ac power load/generation
C to a common block and sets the other common
C block variables holding the load flow simulation data to  
C zero.

       SUBROUTINE EMACH002(ipcomp,plt_dc_pwr,plt_ac_pwr)
       implicit none
      INCLUDE 'plant.h'

C ESP-r COMMON blocks

       COMMON/ENETPCOMP/PPCMP(MPCOM),QPCMP(MPCOM),VCMP(MPCOM)
       real    ppcmp         !- real power consumption of plant component
       real    qpcmp         !- reactive power consumption of plant component
       complex vcmp          !- voltage of plant component

       COMMON/enetpcomp2/pdcpower(mpcom),pacpower(mpcom)
       real    pdcpower      !- dc power load/generation of plant component
       real    pacpower      !- ac power load/generation of plant component

C Passed variables
       integer ipcomp        !- plant component index
       real    plt_dc_pwr    !- dc power load/generation associated with plant component (W)
       real    plt_ac_pwr    !- ac power load/generation associated with plant component (W)


       pdcpower(ipcomp) = plt_dc_pwr
       pacpower(ipcomp) = plt_ac_pwr

       ppcmp(ipcomp) = 0.
       qpcmp(ipcomp) = 0.


       RETURN
       END
