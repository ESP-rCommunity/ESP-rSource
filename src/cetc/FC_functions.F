C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre 
C Natural Resources Canada, Government of Canada
C 2004. Please Contact Ian Beausoliel-Morrison for details 
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

C ********************************************************************************
C ********************************** H_gas ***************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: August 1, 2001
C Copyright CETC 2001

C Modified by Stephanie Mombourquette on May 6, 2004:
C   - Added IMPLICIT NONE to each subroutine..

C This function calculates the relative enthalpy of a gas using the correlation
C equations provided by Cuthbert (2001) in Table 2. Two forms of correlation
C equations are available: one is applicable for N2, H20, Ar, CH4, C2H6, C3H8,
C and H2; the other is applicable for O2 and CO2. The coefficients of the correlations
C are established in module FC_FLUIDS. The enthalpy is then corrected to a common
C datum (1 atm/25oC).

C INPUTS:
C    igas    an integer indicating the gas under consideration.
C    tempC   the temperature of the gas in degrees Celcius.
C    IUOUT   the output channel for error messages.

C OUTPUTS:
C    H_gas   the relative enthalpy in kJ/kmol.
C --------------------------------------------------------------------------------

      FUNCTION H_gas(tempC,igas,IUOUT)
      IMPLICIT NONE
#include "SOFC.h"

C Declare local variables.
      REAL tempC,tempR,H_gas_old,H_gas
      INTEGER igas,IUOUT

C Convert temperature from degrees Celcius to degrees Rankin.
      tempR = tempC*(9./5.) + 491.69

C Choose the correlation appropriate to the gas under consideration and calculate
C the enthalpy in units of Btu/lbmol. Correct to common datum (1 atm/25oC).
      SELECT CASE (igas)

        CASE (N2,H2O,Ar,CH4,C2H6,C3H8,H2)
          H_gas_old = hcoef(1,igas) + hcoef(2,igas)*tempR  ! Correlation equation 1.
     &                + hcoef(3,igas)*tempR**2. - ho_datum(igas)

        CASE (O2,CO2)
          H_gas_old = hcoef(1,igas) * hcoef(2,igas)**(1./tempR)  ! Correlation equation 2.
     &                * tempR**(hcoef(3,igas)) - ho_datum(igas)
        CASE DEFAULT
          call edisp(IUOUT,' Error in H_gas: gas type not supported.')
          STOP ' H_gas: unresolvable error'

      END SELECT

C Convert enthalpy from Btu/lbmol to kJ/kmol.
      H_gas = H_gas_old * 1.05504 * 2.2046 ! kJ/Btu conversion then lb/kg conversion.

      RETURN
      END


C ********************************************************************************
C ******************************* H3K_HeatGains *******************************
C Created by: Phylroy Lopez
C Initial Creation Date: February 26, 2003
C Copyright 2000: NRCan Buildings Group

C     This routine will add heat energy to the zoned on a volume weighted basis. 
C --------------------------------------------------------------------------------

      SUBROUTINE H3K_HeatGainsFromModels(iZoneNumber, H3KGains)
      IMPLICIT NONE
#include "building.h"
#include "geometry.h"
#include "SOFC.h"

      COMMON/C1/NCOMP,NCON
      INTEGER NCOMP,NCON


C Declarations for local variables.
      INTEGER iZoneNumber ! Zone number
      INTEGER ICOMP !Zone iterator
      REAL fHouseVolume !House Volume
      REAL H3KGains
      fHouseVolume = 0.0 !Set volume to zero.
c Determine house volume.
      DO ICOMP=1,NCOMP
         fHouseVolume = fHouseVolume + VOL(ICOMP)    
      END DO

      H3KGains = 
c Weighted term
     &        (VOL(iZoneNumber)/fHouseVolume)
     &        * fCurrent_TS_q_total_elec
c waste heat from each model component (from FC_ctl_h3khvacloads module)
     &        
      RETURN
      END
