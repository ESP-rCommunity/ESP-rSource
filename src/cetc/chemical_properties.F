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
C
C=======================================================================
C chemical_properties.F
C
C Author: Alex Ferguson
C Date: Dec 21, 2004
C Copyright: Natural Resources Canada 2004
C
C This file contains functions performing low-level calculations
C to characterize various ideal-gasses.
C
C Functions:
C
C   - bflamable         -> report if a compound can be oxidized
C   - fEval_Mixture_MM  -> Determine the molar mass of a mixture
C   - fEval_Compound_MM -> Determine molar mass of a compound
C   - fEval_Compound_Cp -> Determine Cp of a compound
C   - fEval_Compound_Cv -> Determine Cv of a compound
C   - fEval_Mixture_Cp  -> Determine Cp of a mixture
C   - fEval_Mixture_Cv  -> Determine Cv of a mixture 
C   - fEval_Mixture_HV  -> Determine the heating value of a mixture
C   - fEval_Compound_HV -> Determine the heating value of a compound
C   - bFlammable_mixture-> Determine if a mixture can be oxidized.
C
C Subroutines
C
C   - Chemical_properties_msg -> interface for streaming warning/error
C         messages to the buffer.
C
C Refernces:
C  Van Wylen, Sonntag & Borgnakke, "Fundamentals of Classical
C  Thermodynamics", John Wiley & Sons, 1994.
C
C======================================================================


C------------------ fCompound_R ---------------------------------------
C
C This function calculates a compound's ideal gas constant
C
C Inputs
C  - index of compound (checked against named constants in
C    chemical_properties.h)
C
C Outputs:
C  - Compound's Ideal gas constant (J/kmol K)
C
C----------------------------------------------------------------------
      real function fCompound_R(iCompound)
      implicit none
#include "chemical_properties.h"

      integer iCompound

      real fCompound_R_table(iCompound_count)
      save fCompound_R_table

      logical bInitialized
      data bInitialized / .false. /
      
      integer ii

C.....Refernece
      real fEval_Compound_MM      
      
      if ( .not. bInitialized ) then

         do ii = 1, iCompound_count
C----------------------------------------------------------------------
C
C                        universal gas constant 
C         gas constant = -----------------------
C                              Molar mass
C
C----------------------------------------------------------------------
            fCompound_R_table (ii) =
     &               fR_universal / fEval_Compound_MM(ii)

         enddo

         bInitialized = .true.
         
      endif

      fCompound_R = fCompound_R_table ( iCompound )

      return
      end

      
      

C------------------ bFlammable ----------------------------------------
C
C This is a very simple function that reports if a compound can
C be oxidized
C
C inputs:
C  - index of compound (checked against named constants in
C    chemical_properties.h)
C
C Outputs:
C  - logical flag indicating flamability
C
C----------------------------------------------------------------------
      logical function bFlammable ( iCompound )
      implicit none
#include "chemical_properties.h"

      integer iCompound

      bFlammable = bIs_Oxidizable ( iCompound )

      return
      end

C------------------ fEval_Compound_MM ---------------------------------
C
C This function returns the molar mass of a compound, in kg/kmol.
C
C Inputs:
C  - index of compound (checked against named constants in
C    chemical_properties.h)
C
C Outputs:
C  - Compound's molar mass (kg/kmol)
C
C----------------------------------------------------------------------
      real function fEval_Compound_MM( iCompound )
      implicit none
#include "chemical_properties.h"

      integer iCompound

C.....Table of compound molar masses. Populated on first call      
      real fMolar_Mass_Table(iCompound_count)
      save fMolar_Mass_Table

C.....Counters      
      integer ii, jj

C.....Element index (also the element's atomic number)
      integer iElement_index      
      
      logical bMolarMass
      save bMolarMass
      data bMolarMass / .false. /


C.....Check if masses have been initialized
      if ( .not. bMolarMass ) then
C----------------------------------------------------------------------
C     Molar masses (kg/kmol).
C----------------------------------------------------------------------
         do ii = 1, iCompound_count
C...........Zero entry in table 
            fMolar_Mass_Table(ii) = 0.

C...........Loop through active elements
            do jj=1, iActive_El_count

C..............Get indicies of active elements
               iElement_index = iActive_elements(jj)

C..............Increment molar mass table with compound               
               fMolar_Mass_Table(ii) = fMolar_Mass_Table(ii)
     &           + float( iCompound_Composition (ii,iElement_index) )
     &                * fElement_Molar_mass ( iElement_index )

            enddo

         enddo

         bMolarMass = .true.
      
      endif

C.....Return requested molar mass
      fEval_Compound_MM = fMolar_Mass_Table ( iCompound )

      return
      end




C------------------ fEval_Mixture_MM ----------------------------------
C
C This function returns the molar mass of a mixture, in kg/kmol.
C
C Inputs:
C  - Array containing fraction of each compound in the mixture
C    * on a per-mol basis *
C
C Outputs:
C  - Mixture's molar mass (kg/kmol)
C
C
C Assumptions:
C   - composition array topology defined using the compound
C     names from chemical_properties.h
C
C----------------------------------------------------------------------
      real function fEval_Mixture_MM( fComposition )
      implicit none
#include "chemical_properties.h"

      real fComposition ( iCompound_count )
      integer ii
      real fAverage_MM
C.....References
      real fEval_Compound_MM  ! recover the molar mass of the i'th
                              ! compound.

C----------------------------------------------------------------------
C     Evaluate mixture molar mass
C
C     mixture molar mass = sum of [ fraction of constituent i *
C                                       molar mass of constuient i ]
C
C----------------------------------------------------------------------
      fAverage_MM = 0.
      do ii = 1, iCompound_count
         fAverage_MM = fAverage_MM +
     &                 fComposition (ii) * fEval_Compound_MM(ii)
      enddo
      fEval_Mixture_MM  = fAverage_MM

      return
      end

C------------------ fEval_Compound_Cp ---------------------------------
C
C This function returns the constant pressure ideal gas specific
C heat of a compound ( J/kmol K)
C
C Inputs:
C  - Index of compound.
C  - Temperature of compound (K)
C
C Outputs:
C  - Compound's specific heat  ( J/kmol K)
C
C
C Assumptions:
C   - composition array topology defined using the compound
C     names from chemical_properties.h
C
C----------------------------------------------------------------------      
      real function fEval_Compound_Cp ( iCompound, fTemperature )
      implicit none
#include "chemical_properties.h"

      integer iCompound
      real fTemperature

      

      
C.....Check which correlation has been provided.
      if ( iCp_Corr_method ( iCompound ) .eq. iCp_Kyle_Corr ) then

C........Kyle Cp Correlation
C........   Cp = a + bT + cT^2 + dT^3
C........
         fEval_Compound_Cp =
     &      (   fCp_Kyle_Coeff ( iCompound, 1 )
     &        + fCp_Kyle_Coeff ( iCompound, 2 ) * fTemperature
     &        + fCp_Kyle_Coeff ( iCompound, 3 ) * fTemperature ** 2.0
     &        + fCp_Kyle_Coeff ( iCompound, 4 ) * fTemperature ** 3.0 )
     &      * 1000.00
     

      else 
C........No other correlations are presently supported.
         write ( cMessage, '(A,I2,A,I2,A)' )
     &   'Correlation type (',iCp_Corr_method ( iCompound ),') '
     &   // 'for compound ', iCompound, ' is presently unsupported. '
     &   // 'Check definitions in chemical_properties.h'

C........Stream out error & kill simulation
         call Chemical_properties_msg ( cMessage, bError)
      endif

C.....Check ranges on correlation
      if ( fTemperature
     &       .gt. fCp_Kyle_Ranges ( iCompound, iCp_Range_MAX ) .or.
     &     fTemperature
     &       .lt. fCp_Kyle_Ranges ( iCompound, iCp_Range_MIN ) ) then
         
         write ( cMessage, '(A,F7.2,A,F7.2,A,F7.2,A,I2)' )
     &     'Chemical properties.F: Temperature (', fTemperature,
     &     ' K) is outside of valid range (',
     &     fCp_Kyle_Ranges ( iCompound, iCp_Range_MIN ), ' -> ',
     &     fCp_Kyle_Ranges ( iCompound, iCp_Range_MAX ),
     &     ' K) for compound ', iCompound

         call Chemical_properties_msg ( cMessage, bWarning )

      endif
       
      return
      end       

C------------------ fEval_Compound_Cv ---------------------------------
C
C This function returns the constant volume ideal gas specific
C heat of a compound ( J/kmol K)
C
C Inputs:
C  - Index of compound.
C  - Temperature of compound (K)
C
C Outputs:
C  - Compound's specific heat  ( J/kmol K)
C
C
C Assumptions:
C   - composition array topology defined using the compound
C     names from chemical_properties.h
C
C----------------------------------------------------------------------      
      real function fEval_Compound_Cv ( iCompound, fTemperature )
      implicit none
#include "chemical_properties.h"

      integer iCompound
      real fTemperature

C.....References
      real fEval_Compound_Cp   ! function returning the constant pressure 
                               ! specific heat for a compound

C.....Remember Cv = Cp - R_universal
      fEval_Compound_Cv = fEval_Compound_Cp (iCompound, fTemperature)
     &      -  fR_universal

C.....Check that result is not less than zero!
      if ( fEval_Compound_Cv .lt. 0.0 ) then

         write ( cMessage, '(A,f6.2,A,I2,A)' )
     &   'Calcualated constant-volume specific heat (',
     &   fEval_Compound_Cv, ') is less than zero for compound ',
     &   iCompound, '. Check correlation coefficients & bounds '
     &   // ' in chemical_properties.F '

C........Stream out error & kill simulation
         call Chemical_properties_msg ( cMessage, bError)
         
      endif

      return
      end       



C------------------ fEval_Compound_HV ---------------------------------
C
C This function returns the heating value of a chemical
C compound (J/kmol)
C
C Inputs:
C  - Index of compound.
C  - flag indicating if lower or upper heating value is requested.
C
C Outputs:
C  - Compound's lower heating value (J/kmol)
C
C
C Assumptions:
C   - composition array topology defined using the compound
C     names from chemical_properties.h
C
C----------------------------------------------------------------------
      real function fEval_Compound_HV( iCompound, iState )
      implicit none
#include "chemical_properties.h"

      integer iCompound

c.....tables containing LHV, HHV of all compounds
      real fHHV_table ( iCompound_count )
      real fLHV_table ( iCompound_count )
      save fHHV_table, fLHV_table

C.....Flags for lower/upper heating value
      integer iState
      integer iWater

      logical bLower_Heating_Value
      save bLower_Heating_Value

C.....reactant/product ratios
      real fA,fB,fC

C.....Counter
      integer ii

C.....References
      logical bFlammable    ! is compound flamable?
      real fCount_carbon    ! count carbon mols in compound
      real fCount_hydrogen  ! count hydrogen mols in compound
      real fCount_oxygen    ! count oxygen mols in compound
      real fCompound_EF     ! retrieve compound's enthalpy of formation

      data bLower_Heating_Value / .false. /

C.....On first call, populate LHV_table
      if ( .not. bLower_Heating_Value ) then

C........Loop through compounds
         do ii = 1, iCompound_count
C...........If compound cannot be oxidized, report LHV as zero
            if ( bIs_Oxidizable (ii) ) then
C----------------------------------------------------------------------
C              Determine reaction stoicheometry:
C                 CxHyOz + (x + y/4 - z/2)O2 = x(CO2) + y/2(H2O)
C                                 A            B         C
C----------------------------------------------------------------------

C..............Carbon dioxide produced (CO2)
               fB = float(iCompound_Composition (ii, iElement_Carbon  ))
               
C..............water produced in reaction (H2O)
               fC  = float(iCompound_Composition(ii, iElement_Hydrogen))
     &                 / 2.0
               
C..............oxygen required (O2)
               fA =   float(iCompound_Composition(ii,iElement_Carbon  ))
     &              + float(iCompound_Composition(ii,iElement_Hydrogen))
     &                   / 4.
     &              - float(iCompound_Composition(ii,iElement_Oxygen  ))
     &                   / 2.

C----------------------------------------------------------------------
C              Determine enthalpy of combustion @ 25oC, 100kPa
C              LHV = ( product enthalpy of formation -
C                             reactant enthalpy of formation )
C----------------------------------------------------------------------
               fLHV_table (ii) = fB  * fEnthalpy_Formation (iC_Dioxide)
     &                         + fC  * fEnthalpy_Formation (iWater_V  )
     &                         - 1.0 * fEnthalpy_Formation (ii        )

               fHHV_table (ii) = fB  * fEnthalpy_Formation (iC_Dioxide)
     &                         + fC  * fEnthalpy_Formation (iWater_l  )
     &                         - 1.0 * fEnthalpy_Formation (ii        )

            else
               fLHV_table (ii) = 0.0
               fHHV_table (ii) = 0.0
            endif

         enddo
C........Set initialization flag to 'done'
         bLower_Heating_Value = .true.
      endif
C.....Return requested heating value. Note, heating values are
C.....customarly reported as positive, even though they
C.....represent a decrease in enthalpy. Therefore, return
C.....absolute value.
      if ( iState .eq. iLHV ) then
         ! LHV requested
         fEval_Compound_HV  = abs ( fLHV_table (iCompound) )
      else
         ! HHV requested
         fEval_Compound_HV  = abs ( fHHV_table (iCompound) )
      endif

      return
      end

C------------------ fEval_Mixture_Cp ----------------------------------
C
C This function returns the ideal gas constant pressure specific heat
C of a mixture ( J / Kmol K)
C
C Inputs:
C  - Array containing fraction of each compound in the mixture
C    * on a per-mol basis *
C  - Temperature of the mixture
C
C Outputs:
C  - Mixture's ideal gas constant pressure specific heat ( J/kmol K)
C
C
C Assumptions:
C   - composition array topology defined using the compound
C     names from chemical_properties.h
C
C----------------------------------------------------------------------
      real function fEval_Mixture_Cp( fComposition, fTemperature)
      implicit none
#include "chemical_properties.h"

      real fComposition ( iCompound_count )
      integer ii
      real fAverage_Cp

      real fTemperature

C.....References
      real fEval_Compound_Cp  ! recover the constant-pressure specific
                              ! heat of a compound 
         
C----------------------------------------------------------------------
C     Evaluate mixture specific heat capacity
C
C     mixture Cp = sum of [ fraction of constituent i *
C                                       Cp of constituent i ]
C
C----------------------------------------------------------------------
      fAverage_Cp = 0.
      do ii = 1, iCompound_count
         fAverage_Cp = fAverage_Cp
     &        + fComposition(ii) * fEval_Compound_Cp(ii,fTemperature)
      enddo

      fEval_Mixture_Cp  = fAverage_Cp

      return
      end      


C------------------ fEval_Mixture_Cv ----------------------------------
C
C This function returns the ideal gas constant volume specific heat
C of a mixture ( J / Kmol K)
C
C Inputs:
C  - Array containing fraction of each compound in the mixture
C    * on a per-mol basis *
C  - Temperature of the mixture
C
C Outputs:
C  - Mixture's ideal gas constant volume specific heat ( J/kmol K)
C
C
C Assumptions:
C   - composition array topology defined using the compound
C     names from chemical_properties.h
C
C----------------------------------------------------------------------
      real function fEval_Mixture_Cv( fComposition, fTemperature)
      implicit none
#include "chemical_properties.h"

      real fComposition ( iCompound_count )
      integer ii
      real fAverage_Cv

      real fTemperature

C.....References
      real fEval_Compound_Cv  ! recover the constant-volume specific
                              ! heat of a compound 
         
C----------------------------------------------------------------------
C     Evaluate mixture specific heat capacity
C
C     mixture Cp = sum of [ fraction of constituent i *
C                                       Cp of constituent i ]
C
C----------------------------------------------------------------------
      fAverage_Cv = 0.
      do ii = 1, iCompound_count
         fAverage_Cv = fAverage_Cv
     &        + fComposition(ii) * fEval_Compound_Cv(ii,fTemperature)
      enddo

      fEval_Mixture_Cv  = fAverage_Cv

      return
      end      

      
C------------------ fEval_Mixture_HV ----------------------------------
C
C This function returns the heating value of a mixture, in J/kmol.
C
C Inputs:
C  - Array containing fraction of each compound in the mixture
C    * on a per-mol basis *
C  - Flag indicating if lower or higher heating value is requested
C
C Outputs:
C  - Mixture's heating value (J/kmol)
C
C
C Assumptions:
C   - composition array topology defined using the compound
C     names from chemical_properties.h
C
C----------------------------------------------------------------------
      real function fEval_Mixture_HV( fComposition, iState )
      implicit none
#include "chemical_properties.h"

      real fComposition ( iCompound_count )
      integer ii
      real fAverage_HV


      integer iState

C.....References
      real fEval_Compound_HV  ! recover the molar mass of the i'th
                              ! compound. 

C----------------------------------------------------------------------
C     Evaluate mixture heating value
C
C     mixture heating value = sum of [ fraction of constituent i *
C                                       heating value of constuient i ]
C
C----------------------------------------------------------------------
      fAverage_HV = 0.
      do ii = 1, iCompound_count
         fAverage_HV = fAverage_HV
     &        + fComposition(ii) * fEval_Compound_HV(ii,iState)
      enddo

      fEval_Mixture_HV  = fAverage_HV

      return
      end

C------------------ fEval_Mixture_R ----------------------------------
C
C This function returns the ideal gas constant of a mixture, in
C kJ/kg K.
C
C Inputs:
C  - Array containing fraction of each compound in the mixture
C    * on a per-mol basis *
C
C Outputs:
C  - Mixture's ideal gas constant  (J/kg K)
C
C Assumptions:
C   - composition array topology defined using the compound
C     names from chemical_properties.h
C
C----------------------------------------------------------------------
      real function fEval_Mixture_R( fComposition )
      implicit none
#include "chemical_properties.h"

      real fComposition ( iCompound_count )
      integer ii
      real fAverage_R


C.....References
      real fCompound_R ! recover the ideal gas constant of a 
                       ! compound. (kJ/kg K)

C----------------------------------------------------------------------
C     Evaluate Gas constant 
C
C     mixture R = sum of [ fraction of constituent i *
C                                      R value of constuient i ]
C
C----------------------------------------------------------------------
      fAverage_R = 0.
      do ii = 1, iCompound_count
         fAverage_R = fAverage_R
     &        + fComposition(ii) * fCompound_R(ii)
      enddo

      fEval_Mixture_R  = fAverage_R

      return
      end
      
C--------------------- fEval_element_mols_in_mixture ------------------
C
C This function counts the number of mols of an element occuring
C in all compounds in a mixture, and reports the result as a
C dimensionless ratio: # mols elemment / mol mixture
C
C Inputs:
C  - Array containing fraction of each compound in the mixture
C    * on a per-mol basis * (fComposition)
C  - index of element of interest (iElement). Use named constants
C    defined in chemical_properties.h
C
C Outputs:
C  - real value describing ratio of element mols to mol of mixture.
C
C Assumptions:
C   - composition array topology defined using the compound
C     names from chemical_properties.h
C
C----------------------------------------------------------------------
      real function fEval_element_mols_in_mixture ( fComposition,
     &                                              iElement )
      implicit none
#include "chemical_properties.h"

      real fComposition ( iCompound_count )
      integer iElement

      integer ii

      real fMol_count

      fMol_count = 0.0
      do ii = 1, iCompound_count
C----------------------------------------------------------------------
C
C       # of mols = ( # mols of compound / mol mixture )
C                        * ( # mols of element / mol compound )
C
C----------------------------------------------------------------------
        fMol_count = fMol_count + fComposition ( ii )
     &     * float( iCompound_Composition( ii, iElement ) )
      enddo
      
      fEval_element_mols_in_mixture = fMol_count
      
      return
      end



C------------------ bFlammable_Mixture----------------------------------
C
C This function returns 'true' if a mixture can be oxidized
C
C Inputs:
C  - Array containing fraction of each compound in the mixture
C    * on a per-mol basis *
C  - Flag indicating if lower or higher heating value is requested
C
C Outputs:
C  - logical result
C
C Assumptions:
C   - composition array topology defined using the compound
C     names from chemical_properties.h
C
C----------------------------------------------------------------------
      logical function bFlammable_Mixture( fComposition )
      implicit none
#include "chemical_properties.h"

      real fComposition ( iCompound_count )
      integer ii

      real fCalculated_HV
C.....References:
      real fEval_Mixture_HV   ! return the heating value of a mixture.
      
C.....Determine mixture heatng value.
      fCalculated_HV = fEval_Mixture_HV ( fComposition, iLHV)

      if ( ABS (fCalculated_HV) .LT. fTolerance ) then
         bFlammable_Mixture = .false.
      else
         bFlammable_Mixture = .true.
      endif 
     
      return
      end


C------------------ Chemical_properties_msg -----------------------------
C
C This procedure is used to send messages from the 'chemical properties'
C routines to the ESP-r buffer.
C
C Inputs:
C
C   cMessage: message to be reported
C   bFatal: logical indicating if message is a fatal error.
C
C Outputs:
C
C   None.
C
C----------------------------------------------------------------------

      subroutine Chemical_properties_msg(
     &            cMessage,
     &            bFatal )
      implicit none
#include "chemical_properties.h"
C.....ESP-r interface
#include "building.h"
     
      logical bFatal

C----------------------------------------------------------------------
C     ESP-r commons for inputs/outputs
C----------------------------------------------------------------------     
C.....Trace & reporting data
      common/tc/itc,icnt
      common/outin/iuout,iuin
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
C.....Trace/error reporting unit numbers
      integer iTc,iCnt
      integer iUout,iUin
      integer iTcf,iTrace,iZntrc,iTu      

C.....Simulation timestep       
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      integer ihrp              ! not used in current context
      integer ihrf              ! not used in current context
      integer idyp              ! present day #
      integer idyf              ! not used in current context
      integer idwp              ! not used in current context
      integer idwf              ! not used in current context
      integer nsinc             ! time step number
      integer its               ! not used in current context

      write (itu, * )
      write (itu, * )
      write (itu, * )

      
      if ( bFatal ) then
      
         write (itu, * ) 'Chemical_Properties.F: Fatal Error!'

      else

         write (itu, * ) 'Chemical_Properties.F: Warning'

      endif
      
C.....Write out context, if any.
      write (itu, * ) cContext

C.....Write out message.      
      write (itu, * ) cMessage
      
C.....Write timestep, hour, day
      write (itu, '(a,I3,A,I6)') ' Day, ', idyp, ', Timestep: ', nsinc

      write (itu, * )
      write (itu, * )
      write (itu, * )
      
      if ( bFatal ) stop


      return
      end
