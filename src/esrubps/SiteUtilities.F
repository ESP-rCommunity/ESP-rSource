C This file is part of the ESP-r system.
C Copyright CANMET Energy Technology Centre
C Natural Resources Canada, Government of Canada
C 2008. Please Contact Ian Beausoliel-Morrison for details
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

C This file contains routines used to collate fuel used on site.
C
C SiteUtilities:           Supervisory routine
C
C TotalPurchasedEnergy:    Totals fuel consumed in the building
C
C ConvertEnergyToFuel:     Converts purchased energy into an
C                          equivlent amount of fuel use.
C
C H3KReportsFuelUse:       Transports data to H3KReports.
C
C=======================================================================

C=SiteUtilities=========================================================
C
C ABSTRACT:
C
C SiteUtilities invokes the various subroutines needed to characterize
C purchased energy. Support for green-house-gas emissions may be
C added in the future.
C
C=======================================================================
      subroutine SiteUtilities()
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

C.....Attribute uncatagorized energy use in elec, plant domains.
      call AttributePltEnergyUse()

C.....Aggregate energy use from all plant and power only components
      call AggregateMdlCompEnergyUse()

C.....Aggregate energy from HOT3000 base loads
      call AggregateH3KBaseLoadsEnergyUse()

C.....Aggregate fuel costs from HOT3000 base loads
      if( bIncFuelCostCalcs ) then
            call AggregateH3KBaseLoadsFuelCost()
            call AggregatePltFuelCost()            
      end if

C.....Compute site-wide energy consumption
      call TotalEnergyConsumption()

C.....Convert purchased energy to fuel quantity and GHG
      call ConvertEnergyToFuelandGHG()

C.....Compute site-wide fuel cost
      if( bIncFuelCostCalcs ) then
            call TotalFuelCost()
      end if

C.....Report fuel use data
      call H3KReportsFuelUse()

C.....Reset storage arrays for next time step.
      call ZeroEnergyStorageArrays()
     
C.....Reset fuel cost storage array for next time step.
      if( bIncFuelCostCalcs ) then
            call ZeroFuelCostStorageArray()
      end if

C.....Reset HOT3000 Base Loads storage array for next time step.
      call ZeroH3KBaseLoadsEnergyStorageArray()

C.....Reset HOT3000 Base Loads fuel cost storage array for next time step.
      if( bIncFuelCostCalcs ) then
            call ZeroH3KBaseLoadsFuelCostStorageArray()
      end if

      return
      end


C=TotalPurchasedEnergy==================================================
C
C ABSTRACT:
C
C TotalPurchasedEnergy computes the total amount of energy use reported from all
C parts of the simulator.
C
C=======================================================================
      subroutine TotalEnergyConsumption()
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"


C-----------------------------------------------------------------------
C     Loop through all components, end uses; and sum fuel consumption.
C-----------------------------------------------------------------------
      fuel_loop: do iFuel = 1, iNumFuel

C.......Zero total enery use (by fuel) array slot
        fTotalEnergyUse( iFuel ) = 0.0

        use_loop: do iEndUse = 1, iNumUses

C.........Zero end-use energy use (by fuel) array slot
          fEndUseEnergyUse ( iFuel, iEndUse ) = 0.0
          
          component_loop: do iSiteComp = 1, iNumOffsiteUtilComp

            fTotalEnergyUse( iFuel ) =
     &               fTotalEnergyUse( iFuel )
     &             + fSiteEnergyUse( iSiteComp, iFuel, iEndUse )

            fEndUseEnergyUse ( iFuel, iEndUse ) =
     &              fEndUseEnergyUse ( iFuel, iEndUse )
     &            + fSiteEnergyUse( iSiteComp, iFuel, iEndUse )

          enddo component_loop

          
        enddo use_loop
        
        

      enddo fuel_loop

      return
      end

C=ConvertEnergyToFuelandGHG===================================================
C
C ABSTRACT:
C
C ConvertEnergyToFuel converts the total energy use associated with a given
C fuel into a standard unit of measure, based on the fuel's calorific
C value.
C
C=======================================================================
      subroutine ConvertEnergyToFuelandGHG()
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

      

      fuel_loop: do iFuel = 1, iNumFuel

C-----------------------------------------------------------------------
C       Total fuel use = Total energy use * conversion factor
C-----------------------------------------------------------------------

C.......Site-wide fuel consuption (by fuel)
        fTotalFuelUse( iFuel ) = fTotalEnergyUse( iFuel )
     &            * fFuelConversionFactor( iFuel )

        
C.......Site-wide fuel consumption (by fuel, end-use)
        use_loop: do iEndUse = 1, iNumUses
          
          fEndUseFuelUse( iFuel, iEndUse ) =
     &                    fEndUseEnergyUse (iFuel, iEndUse)
     &                  * fFuelConversionFactor( iFuel )

                    
        enddo use_loop

        
      enddo fuel_loop

      return
      end

C=H3KReportsFuelUse=====================================================
C
C ABSTRACT:
C
C H3KReportsFuelUse reports transport data on fuel usage to the H3Kreports facility.
C
C=======================================================================
      subroutine H3KReportsFuelUse
      use h3kmodule
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

      logical bCloseToZero

      integer iRVId     !Report Variable Identifier
      integer iRVIdCost !Report Variable Identifier for Cost

C.....Fuel loop:
      fuel_loop: do iFuel = 1, iNumFuel

C-----------------------------------------------------------------------
C       Consumption by fuel, all end-uses; suppress
C       energy flows < 0.01 W
C-----------------------------------------------------------------------

        call eclose ( fTotalEnergyUse(iFuel), 0.0, 0.01, bCloseToZero)

        if ( .not. bCloseToZero ) then
           SELECT CASE (iFuel)
            CASE (iElectricity)
              iRVId = rvTFuelAllEndEnergyContElec%Identifier
            CASE (iNaturalGas)
              iRVId = rvTFuelAllEndEnergyContNatGas%Identifier
            CASE (iOil)
              iRVId = rvTFuelAllEndEnergyContOil%Identifier
            CASE (iPropane)
              iRVId = rvTFuelAllEndEnergyContPropane%Identifier
            CASE (iMixedWood)
              iRVId = rvTFuelAllEndEnergyContMixWood%Identifier
            CASE (iHardWood)
              iRVId = rvTFuelAllEndEnergyContHardWood%Identifier
            CASE (iSoftWood)
              iRVId = rvTFuelAllEndEnergyContSoftWood%Identifier
            CASE (iWoodPellets)
              iRVId = rvTFuelAllEndEnergyContPellets%Identifier
            CASE DEFAULT
               !Should never execute, but just in case
               iRVId = rvTFuelAllEndEnergyContent%Identifier
          END SELECT


          call AddToReport(
     &         iRVID,
     &         fTotalEnergyUse(iFuel))

        endif

       SELECT CASE (iFuel)
         CASE (iElectricity)
            iRVId = rvTFuelAllEndQtyElectricity%Identifier
            iRVIdCost = rvTFuelCstAllEndElectricity%Identifier
         CASE (iNaturalGas)
            iRVId = rvTFuelAllEndQtyNaturalGas%Identifier
            iRVIdCost = rvTFuelCstAllEndNaturalGas%Identifier
         CASE (iOil)
            iRVId = rvTFuelAllEndQtyOil%Identifier
            iRVIdCost = rvTFuelCstAllEndOil%Identifier
         CASE (iPropane)
            iRVId = rvTFuelAllEndQtyPropane%Identifier
            iRVIdCost = rvTFuelCstAllEndPropane%Identifier
         CASE (iMixedWood)
            iRVId = rvTFuelAllEndQtyMixedWood%Identifier
            iRVIdCost =rvTFuelCstAllEndMixedWood%Identifier
         CASE (iHardWood)
            iRVId = rvTFuelAllEndQtyHardWood%Identifier
            iRVIdCost = rvTFuelCstAllEndHardWood%Identifier
         CASE (iSoftWood)
            iRVId = rvTFuelAllEndQtySoftWood%Identifier
            iRVIdCost = rvTFuelCstAllEndSoftWood%Identifier
         CASE (iWoodPellets)
            iRVId = rvTFuelAllEndQtyWoodPellet%Identifier
            iRVIdCost = rvTFuelCstAllEndWoodPellet%Identifier
         CASE DEFAULT
            !Should never execute, but just in case
            iRVId = rvTFuelAllEndQty%Identifier
            iRVIdCost = rvTFuelCstAllEnd%Identifier
       END SELECT

       call AddToReport(
     &         iRVId,
     &         fTotalFuelUse(iFuel))

C.......Total fuel cost reporting.
        if( bIncFuelCostCalcs ) then
            call AddToReport(
     &         iRVIdCost,
     &         fTotalFuelCost(iFuel))
        end if

C-----------------------------------------------------------------------
C         Consumption by fuel, end-use; suppress
C         energy flows < 0.01 W
C-----------------------------------------------------------------------

C.......end-use loop:
        use_loop: do iEndUse = 1, iNumUses
            call eclose ( fEndUseEnergyUse(iFuel,iEndUse),
     &                0.0, 0.01, bCloseToZero)

                SELECT CASE (iFuel)
                CASE (iElectricity)
                    iRVId = rvTFuelQtyElec%Identifier
                    iRVIdCost = rvTFuelCstElec%Identifier
                CASE (iNaturalGas)
                    iRVId = rvTFuelQtyNatGas%Identifier
                    iRVIdCost = rvTFuelCstNatGas%Identifier
                CASE (iOil)
                    iRVId = rvTFuelQtyOil%Identifier
                    iRVIdCost = rvTFuelCstOil%Identifier
                CASE (iPropane)
                    iRVId = rvTFuelQtyProp%Identifier
                    iRVIdCost = rvTFuelCstProp%Identifier
                CASE (iMixedWood)
                    iRVId = rvTFuelQtyMixWood%Identifier
                    iRVIdCost = rvTFuelCstMixWood%Identifier
                CASE (iHardWood)
                    iRVId = rvTFuelQtyHardWood%Identifier
                    iRVIdCost = rvTFuelCstHardWood%Identifier
                CASE (iSoftWood)
                    iRVId = rvTFuelQtySoftWood%Identifier
                    iRVIdCost = rvTFuelCstSoftWood%Identifier
                CASE (iWoodPellets)
                    iRVId = rvTFuelQtyPellets%Identifier
                    iRVIdCost = rvTFuelCstPellets%Identifier
                CASE DEFAULT
                    !Should never execute, but just in case
                    iRVId = rvTFuelQty%Identifier
                    iRVIdCost = rvTFuelCst%Identifier
                END SELECT
                
            if ( .not. bCloseToZero ) then
                !Removed the dynamic description, if needed it can be re-enable
                call AddToReport(
     &          iRVId,
     &          fEndUseFuelUse(iFuel,iEndUse),
     &          cUseName(iEndUse)(1:iUseNameLen(iEndUse)))

C...............Reporting of total fuel cost by end use.
                if( bIncFuelCostCalcs ) then
                    call AddToReport(
     &               IRVIdCost,
     &               fEndUseFuelCost(iFuel,iEndUse),
     &               cUseName(iEndUse)(1:iUseNameLen(iEndUse)))
                end if

            endif

C...........Reporting of extra billing charges.

C...........This is outside of the if block for suppressing small energy flows
C...........because an energy flow doesn't exist for this end use category.
C...........iExtraBillingCharges is only used to store the minimum monthly
C...........fuel charges.

            if( bIncFuelCostCalcs .and.
     &                iEndUse .eq. iExtraBillingCharges) then
                call AddToReport(
     &               IRVIdCost,
     &               fEndUseFuelCost(iFuel,iEndUse),
     &               cUseName(iEndUse)(1:iUseNameLen(iEndUse)))
            end if

        enddo use_loop

      enddo fuel_loop
      return
      end

C=ZeroEnergyStorageArrays============================================
C
C ABSTRACT:
C
C ZeroEnergyStorageArrays zeros the energy usage storage arrays.
C
C=======================================================================
      subroutine ZeroEnergyStorageArrays()
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"


C.....ESP commons
      ! Plant component count
      common/c9/nPComp,NCI(mpcom),CData(mpcom,mmiscd)
      integer nPComp                     ! # of plant component models
      integer nci                        ! not used
      real CData                         ! not used

      ! DHW
      common/report_loads/fReportDHWLoad(mpcom)
      real fReportDHWLoad                ! DHW load computed by a plant
                                         ! component (W)

      ! Power only components
      common/powc/npowcom,powcomno(mpowcom),powcomid(mpowcom),
     &  powcomphtyp(mpowcom),powconen(mpowcom,mphas),
     &  npowcdat(mpowcom),npowcdats(mpowcom),powcdat(mpowcom,mpwdat)
      integer      npowcom, ! Number of power only component models
     &  powcomno, powcomid, powcomphtyp, powconen, ! not used
     &  npowcdat, npowcdats ! not used
      real         powcdat ! not used

      ! Zone information
      common/c1/ncomp,ncon
      integer ncomp          ! number of zones
      integer ncon           ! not used 

C-----------------------------------------------------------------------
C     Loop through all components, and sum fuel consumption.
C-----------------------------------------------------------------------
      fuel_loop: do iFuel = 1, iNumFuel

        use_loop: do iEndUse = 1, iNumUses

C         Zero Site Components
          SiteComp_loop: do iSiteComp = 1, iNumOffsiteUtilComp

            fSiteEnergyUse( iSiteComp, iFuel, iEndUse ) = 0.

          enddo SiteComp_loop

C         Zero Plant Components
          PltComp_loop: do iPltComp = 1, nPComp

            fPltEnergyUse( iPltComp, iFuel, iEndUse ) = 0.

          enddo PltComp_loop

C         Zero Power only Components
          PowocComp_loop: do iPowocComp = 1, npowcom

            fPowocEnergyUse( iPowocComp, iFuel, iEndUse ) = 0.

          enddo PowocComp_loop

C         Zero Casual gain NG Components
          CasualNG_loop: do iZone = 1, ncomp

            fCasualNGEnergyUse( iZone, iFuel, iEndUse ) = 0.

          enddo CasualNG_loop

        enddo use_loop

      enddo fuel_loop


C     Zero per-plant-component load arrays
      PltComp_loop2: do iPltComp = 1, nPComp

        fReportDHWLoad( iPltComp ) = 0.

      enddo PltComp_loop2
         

      return
      end

C=ZeroH3KBaseLoadsEnergyStorageArray======================================
C
C ABSTRACT:
C
C ZeroH3KBaseLoadsEnergyStorageArray zeros the site energy usage storage
C array specific to HOT3000 Base Loads.
C
C=======================================================================
      subroutine ZeroH3KBaseLoadsEnergyStorageArray()
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

      ! Zone information
      common/c1/ncomp,ncon
      integer ncomp          ! number of zones
      integer ncon           ! not used

C-----------------------------------------------------------------------
C     Loop through all zones, and zero HOT3000 Base Loads energy use
C-----------------------------------------------------------------------

      fuel_loop: do iFuel = 1, iNumFuel

        use_loop: do iEndUse = 1, iNumUses

          zone_loop: do iZone = 1, ncomp

           fH3KBaseLoadsEnergyUse(iZone, iFuel, iEndUse) = 0.0

          enddo zone_loop

        enddo use_loop

      enddo fuel_loop

      return
      end

C=ZeroH3KBaseLoadsFuelCostStorageArray======================================
C
C ABSTRACT:
C
C ZeroH3KBaseLoadsFuelCostStorageArray zeros the site fuel cost storage
C array specific to HOT3000 Base Loads.
C
C=======================================================================
      subroutine ZeroH3KBaseLoadsFuelCostStorageArray()
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

      ! Zone information
      common/c1/ncomp,ncon
      integer ncomp          ! number of zones
      integer ncon           ! not used

C-----------------------------------------------------------------------
C     Loop through all zones, and zero HOT3000 Base Loads energy use
C-----------------------------------------------------------------------

      fuel_loop: do iFuel = 1, iNumFuel

        use_loop: do iEndUse = 1, iNumUses

          zone_loop: do iZone = 1, ncomp

           fH3KBaseLoadsFuelCost(iZone, iFuel, iEndUse) = 0.0

          enddo zone_loop

        enddo use_loop

      enddo fuel_loop

      return
      end

C=ZeroFuelCostStorageArray============================================
C
C ABSTRACT:
C
C ZeroFuelCostStorageArray zeros the site fuel cost storage array.
C
C=======================================================================
      subroutine ZeroFuelCostStorageArray()
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

C-----------------------------------------------------------------------
C     Loop through all components.
C-----------------------------------------------------------------------
      fuel_loop: do iFuel = 1, iNumFuel

        component_loop: do iSiteComp = 1, iNumOffsiteUtilComp

          use_loop: do iEndUse = 1, iNumUses

            fSiteFuelCost( iSiteComp, iFuel, iEndUse ) = 0.0

          enddo use_loop

        enddo component_loop

      enddo fuel_loop

      return
      end

C=StoreSiteUtilityData==================================================
C
C ABSTRACT:
C
C StoreSiteUtilityData takes the data stored in the passed fSUFuelEnergyUse
C array and stuffs it into the protected fSiteEnergyUse array
C
C Input parameters defined via include statements.
C
C   - iSiteComp: integer flag indicating where data originated
C
C   - fSUFuelEnergyUse: 2d array used transport fuel consumption
C     data into Site utilities facility. Array indicies correspond
C     to fuel, end-use.
C
C=======================================================================
      subroutine StoreSiteEnergyUse(iSiteComp, fSUFuelEnergyUse )
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

      logical bInStartup

C-----------------------------------------------------------------------
C     Loop through fuels, end-uses and stuff fuel consumption in
C     protected fSiteEnergyUse array
C-----------------------------------------------------------------------
      fuel_loop: do iFuel = 1, iNumFuel

        use_loop: do iEndUse = 1, iNumUses

          fSiteEnergyUse( iSiteComp, iFuel, iEndUse ) =
     &       fSUFuelEnergyUse ( iFuel, iEndUse )
	 
        enddo use_loop

      enddo fuel_loop

C-----------------------------------------------------------------------      
C     AddToSumFuelUseArray adds fSUFuelEnergyUse to the fSumFuelUse
C     array. StoreSiteFuelCosts fetches the appropriate fuel cost based
C     on the total sum of fuel use so far, and stores the fuel cost
C     in a private array.
C-----------------------------------------------------------------------
      if( bIncFuelCostCalcs.and. (.not. bInStartup() ) ) then
            call AddToSumFuelUseArray(fSUFuelEnergyUse)
            call StoreSiteFuelCosts(iSiteComp, fSUFuelEnergyUse )
      end if

      return
      end

C=StoreSiteFuelCosts====================================================
C
C ABSTRACT
C
C StoreSiteFuelCosts stores fuel costs associated with component energy
C use at each time step for later access in the site utilities facility.
C
C Input parameters defined via include statements.
C
C   - iSiteComp: integer flag indicating where data originated
C
C   - fSUFuelEnergyUse: 2d array used transport fuel consumption
C     data into Site utilities facility. Array indicies correspond
C     to fuel, end-use.
C
C=======================================================================
      subroutine StoreSiteFuelCosts(iSiteComp, fSUFuelEnergyUse )
      implicit none
#include "building.h"
#include "MultiYear_simulations.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

      real fuel_cost
      real fGetFuelCost
      real fConvertEnergyToFuelAmount

C-----------------------------------------------------------------------
C     Loop through fuels, end uses, convert energy use to
C     fuel consumption and stuff into protected fuel costs array
C-----------------------------------------------------------------------
      fuel_loop: do iFuel = 1, iNumFuel

C.......Get the appropriate fuel cost based on the month and rate block.

        fuel_cost = fGetFuelCost(iFuel)


        use_loop: do iEndUse = 1, iNumUses

            fSiteFuelCost( iSiteComp, iFuel, iEndUse ) =
     &            fConvertEnergyToFuelAmount(
     &            fSUFuelEnergyUse( iFuel, iEndUse ),
     &            iFuel ) * fuel_cost	 

        enddo use_loop

      enddo fuel_loop

      return
      end


C=StorePltCompEnergyUse=================================================
C
C ABSTRACT
C
C StorePltCompEnergyUse stores fuel energy use computed by plant
C component models for later access in the site utilities facility.
C
C Input parameters defined via include statements.
C
C   - iPltComp: Plant component index
C
C   - fSUFuelEnergyUse: 2d array used transport fuel consumption
C     data into Site utilities facility. Array indicies correspond
C     to fuel, end-use.
C
C=======================================================================
      subroutine StorePltCompEnergyUse(iPltComp, fSUFuelEnergyUse )
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

      logical bNumsAreClose
      logical bInStartup	  

C-----------------------------------------------------------------------
C     Loop through fuels, end-uses and stuff fuel consumption into
C     protected fSiteEnergyUse array
C-----------------------------------------------------------------------
      fuel_loop: do iFuel = 1, iNumFuel

        use_loop: do iEndUse = 1, iNumUses

          fPltEnergyUse( iPltComp, iFuel, iEndUse ) =
     &       fSUFuelEnergyUse( iFuel, iEndUse )
	 
C...........Mark uncatagorized end-uses for reapportionment
            if ( iEndUse == iUseUncatagorized ) then

              call eclose ( fSUFuelEnergyUse( iFuel, iEndUse ),
     &                      0.0, 0.01, bNumsAreClose)
              if ( bNumsAreClose .and.
     &             fSUFuelEnergyUse( iFuel, iEndUse ) > 0.0 ) then

                bPltUseCatagorized(iPltComp, iFuel) = .false.

              else

                bPltUseCatagorized(iPltComp, iFuel) = .false.

              endif

            endif

        enddo use_loop

      enddo fuel_loop	  
	  
C-----------------------------------------------------------------------
C     AddToSumFuelUseArray adds fSUFuelEnergyUse to the fSumFuelUse
C     array. StorePltFuelCosts fetches the appropriate fuel cost based
C     on the total sum of fuel use so far, and stores the fuel cost
C     in a private array.
C-----------------------------------------------------------------------
      if( bIncFuelCostCalcs.and. (.not. bInStartup() ) ) then
            call AddToSumFuelUseArray(fSUFuelEnergyUse)
            call StorePltFuelCosts(iPltComp, fSUFuelEnergyUse )
      end if      
      
      return
      end

C=StorePowocCompEnergyUse=================================================
C
C ABSTRACT
C
C StorePowocCompEnergyUse stores fuel energy use computed by power-only
C component models for later access in the site utilities facility.
C
C Input parameters defined via include statements.
C
C   - iPowocComp: Power only component index
C
C   - fSUFuelEnergyUse: 2d array used transport fuel consumption
C     data into Site utilities facility. Array indicies correspond
C     to fuel, end-use.
C
C=======================================================================
      subroutine StorePowocCompEnergyUse(iPowocComp,fSUFuelEnergyUse)
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

C-----------------------------------------------------------------------
C     Loop through fuels, end-uses and stuff fuel consumption into
C     protected fSiteEnergyUse array
C-----------------------------------------------------------------------
      fuel_loop: do iFuel = 1, iNumFuel

        use_loop: do iEndUse = 1, iNumUses

          fPowocEnergyUse( iPowocComp, iFuel, iEndUse ) =
     &       fSUFuelEnergyUse( iFuel, iEndUse )

        enddo use_loop

      enddo fuel_loop

      return
      end 

C=StoreH3KBaseLoadsEnergyUse=================================================
C
C ABSTRACT
C
C StoreH3KBaseLoadsEnergyUse stores fuel energy used for HOT3000 base
C loads for later access in the site utilities facility.
C
C Input parameters defined via include statements.
C
C   - iZone: Zone in which energy associated with base laods is consumed
C
C   - fSUFuelEnergyUse: 2d array used to transport fuel consumption
C     data into Site utilities facility. Array indicies correspond
C     to fuel, end-use.
C
C=======================================================================
      subroutine StoreH3KBaseLoadsEnergyUse(iZone, fSUFuelEnergyUse )
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

      ! Zone information
      common/c1/ncomp,ncon
      integer ncomp          ! number of zones
      integer ncon           ! not used

      logical bInStartup

C-----------------------------------------------------------------------
C     Loop through fuels, end uses and stuff HOT3000
C     Base Loads fuel consumption into protected fSiteEnergyUse array
C-----------------------------------------------------------------------

      fuel_loop: do iFuel = 1, iNumFuel

        use_loop: do iEndUse = 1, iNumUses

           fH3KBaseLoadsEnergyUse(iZone, iFuel, iEndUse) =
     &       fSUFuelEnergyUse( iFuel, iEndUse )

        enddo use_loop

      enddo fuel_loop
     
C-----------------------------------------------------------------------      
C     AddToSumFuelUseArray adds fSUFuelEnergyUse to the fSumFuelUse
C     array. StoreH3KBaseLoadsFuelCosts fetches the appropriate fuel cost based
C     on the total sum of fuel use so far, and stores the fuel cost
C     in a private array.
C-----------------------------------------------------------------------
      if( bIncFuelCostCalcs.and. (.not. bInStartup() ) ) then
            call AddToSumFuelUseArray(fSUFuelEnergyUse)
            call StoreH3KBaseLoadsFuelCosts(iZone, fSUFuelEnergyUse )
      end if

      return
      end

C=StoreH3KBaseLoadsFuelCosts============================================
C
C ABSTRACT
C
C StoreH3KBaseLoadsFuelCosts stores fuel costs associated with HOT3000 base
C loads at each time step for later access in the site utilities facility.
C
C Input parameters defined via include statements.
C
C   - iZone: Zone in which energy associated with base laods is consumed
C
C   - fSUFuelEnergyUse: 2d array used to convert fuel consumption
C     data into fuel costs. Array indicies correspond
C     to fuel, end-use.
C
C=======================================================================
      subroutine StoreH3KBaseLoadsFuelCosts(iZone, fSUFuelEnergyUse )
      implicit none
#include "building.h"
#include "MultiYear_simulations.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

      real fuel_cost
      real fGetFuelCost
      real fConvertEnergyToFuelAmount

C-----------------------------------------------------------------------
C     Loop through fuels, end uses, convert HOT3000 Base Loads energy use
C     to fuel consumption and stuff into protected fuel costs array
C-----------------------------------------------------------------------
      fuel_loop: do iFuel = 1, iNumFuel

C.......Get the appropriate fuel cost based on the month,  rate block, or
C.......TOU rate (electricity only).
        fuel_cost = fGetFuelCost(iFuel)

        use_loop: do iEndUse = 1, iNumUses

            fH3KBaseLoadsFuelCost( iZone, iFuel, iEndUse ) =
     &            fConvertEnergyToFuelAmount(
     &            fSUFuelEnergyUse( iFuel, iEndUse ),
     &            iFuel ) * fuel_cost

        enddo use_loop

      enddo fuel_loop

      return
      end

C=StorePltFuelCosts=====================================================
C
C ABSTRACT
C
C StorePltFuelCosts stores fuel costs associated with plant component
C loads at each time step for later access in the site utilities facility.
C
C Input parameters defined via include statements.
C
C   - iPltComp: Plant component index
C
C   - fSUFuelEnergyUse: 2d array used to convert fuel consumption
C     data into fuel costs. Array indicies correspond
C     to fuel, end-use.
C
C=======================================================================
      subroutine StorePltFuelCosts(iPltComp, fSUFuelEnergyUse )
      implicit none
#include "building.h"
#include "MultiYear_simulations.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

      real fuel_cost
      real fGetFuelCost
      real fConvertEnergyToFuelAmount

C-----------------------------------------------------------------------
C     Loop through fuels, end uses, convert HOT3000 Base Loads energy use
C     to fuel consumption and stuff into protected fuel costs array
C-----------------------------------------------------------------------
      fuel_loop: do iFuel = 1, iNumFuel

C.......Get the appropriate fuel cost based on the month,  rate block, or
C.......TOU rate (electricity only).
        fuel_cost = fGetFuelCost(iFuel)

        use_loop: do iEndUse = 1, iNumUses

            fPltFuelCost( iPltComp, iFuel, iEndUse ) =
     &            fConvertEnergyToFuelAmount(
     &            fSUFuelEnergyUse( iFuel, iEndUse ),
     &            iFuel ) * fuel_cost

        enddo use_loop

      enddo fuel_loop

      return
      end	  
	  
C=StoreCasualNGZoneEnergyUse=================================================
C
C ABSTRACT
C
C StoreCasualNGZoneEnergyUse stores fuel energy use computed by casual gain NG
C components for a zone for later access in the site utilities facility.
C
C Input parameters defined via include statements.
C
C   - iZone: Zone in which the NG is consumed
C
C   - fSUFuelEnergyUse: 2d array used transport fuel consumption
C     data into Site utilities facility. Array indicies correspond
C     to fuel, end-use.
C
C=======================================================================
      subroutine StoreCasualNGZoneEnergyUse(iZone,fSUFuelEnergyUse)
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

C-----------------------------------------------------------------------
C     Loop through fuels, end-uses and stuff fuel consumption into
C     protected fSiteEnergyUse array
C-----------------------------------------------------------------------
      fuel_loop: do iFuel = 1, iNumFuel

        use_loop: do iEndUse = 1, iNumUses

          fCasualNGEnergyUse( iZone, iFuel, iEndUse ) =
     &       fSUFuelEnergyUse( iFuel, iEndUse )

        enddo use_loop

      enddo fuel_loop

      return
      end 


C=AggregateMdlCompEnergyUse=============================================
C
C ABSTRACT
C
C AggregateMdlCompEnergyUse aggregates energy consumption computed
C by various plant, power-only, and casual gain components, and appends it to the site-wide
C energy consumption array.
C
C Inputs:
C
C   - None.
C
C=======================================================================
      subroutine AggregateMdlCompEnergyUse()

      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

C.....ESP commons
      ! Plant
      common/c9/nPComp,NCI(mpcom),CData(mpcom,mmiscd)
      integer nPComp                     ! # of plant component models
      integer nci                        ! not used
      real CData                         ! not used

      ! Power only components
      common/powc/npowcom,powcomno(mpowcom),powcomid(mpowcom),
     &  powcomphtyp(mpowcom),powconen(mpowcom,mphas),
     &  npowcdat(mpowcom),npowcdats(mpowcom),powcdat(mpowcom,mpwdat)
      integer      npowcom, ! Number of power only component models
     &  powcomno, powcomid, powcomphtyp, powconen, ! not used
     &  npowcdat, npowcdats ! not used
      real         powcdat ! not used

      ! Zone information
      common/c1/ncomp,ncon
      integer ncomp          ! number of zones
      integer ncon           ! not used 

C-----------------------------------------------------------------------
C     Loop through fuels, end-uses and sum fuel consumption
C-----------------------------------------------------------------------

      fuel_loop: do iFuel = 1, iNumFuel

        use_loop: do iEndUse = 1, iNumUses

C         PLANT
C.........Initialize fuel/end-use combination to zero.
          fSiteEnergyUse( iPltComponents, iFuel, iEndUse ) = 0.

C.........Sum fuel/end-use consumtion for all components
          plt_component_loop: do iPltComp =1 , nPComp

            fSiteEnergyUse( iPltComponents, iFuel, iEndUse ) =
     &           fSiteEnergyUse( iPltComponents, iFuel, iEndUse )
     &         + fPltEnergyUse( iPltComp, iFuel, iEndUse )
	  
          enddo plt_component_loop

c         POWOC
C.........Initialize fuel/end-use combination to zero.
          fSiteEnergyUse( iPowoc, iFuel, iEndUse ) = 0.

C.........Sum fuel/end-use consumtion for all components
          powoc_component_loop: do iPowocComp =1 , npowcom

            fSiteEnergyUse( iPowoc, iFuel, iEndUse ) =
     &           fSiteEnergyUse( iPowoc, iFuel, iEndUse )
     &         + fPowocEnergyUse( iPowocComp, iFuel, iEndUse )

          enddo powoc_component_loop

c         Casual gain NG
C.........Initialize fuel/end-use combination to zero.
          fSiteEnergyUse( iCasual_NG, iFuel, iEndUse ) = 0.

C.........Sum fuel/end-use consumtion for all components
          CasualNG_component_loop: do iZone =1 , ncomp

            fSiteEnergyUse( iCasual_NG, iFuel, iEndUse ) =
     &           fSiteEnergyUse( iCasual_NG, iFuel, iEndUse )
     &         + fCasualNGEnergyUse( iZone, iFuel, iEndUse )

          enddo CasualNG_component_loop

        enddo use_loop

      enddo fuel_loop


      return
      end

C=AggregateH3KBaseLoadsEnergyUse=======================================
C
C ABSTRACT
C
C AggregateH3KBaseLoadsEnergyUse aggregates energy consumption specific
C to HOT3000 Base Loads, and appends it to the site-wide energy
C consumption array.
C
C Inputs:
C
C   - None.
C
C=======================================================================
      subroutine AggregateH3KBaseLoadsEnergyUse()

      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

      ! Zone information
      common/c1/ncomp,ncon
      integer ncomp          ! number of zones
      integer ncon           ! not used

C-----------------------------------------------------------------------
C     Loop through fuels, end-uses, zones and sum fuel consumption
C-----------------------------------------------------------------------

      fuel_loop: do iFuel = 1, iNumFuel

        use_loop: do iEndUse = 1, iNumUses

C.........Initialize fuel/end-use combination to zero.
          fSiteEnergyUse( iH3KBaseLoads, iFuel, iEndUse ) = 0.

C.........Sum base loads fuel/end-use consumtion for all zones
          H3KBaseLoads_loop: do iZone =1 , ncomp

            fSiteEnergyUse( iH3KBaseLoads, iFuel, iEndUse ) =
     &           fSiteEnergyUse( iH3KBaseLoads, iFuel, iEndUse )
     &         + fH3KBaseLoadsEnergyUse(iZone, iFuel, iEndUse)

          enddo H3KBaseLoads_loop

        enddo use_loop

      enddo fuel_loop


      return
      end

C=AggregateH3KBaseLoadsFuelCost=======================================
C
C ABSTRACT
C
C AggregateH3KBaseLoadsFuelCost aggregates fuel costs specific
C to HOT3000 Base Loads, and appends it to the site-wide fuel
C cost array.
C
C Inputs:
C
C   - None.
C
C=======================================================================
      subroutine AggregateH3KBaseLoadsFuelCost()

      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

      ! Zone information
      common/c1/ncomp,ncon
      integer ncomp          ! number of zones
      integer ncon           ! not used

C-----------------------------------------------------------------------
C     Loop through fuels, end-uses, zones and sum fuel consumption
C-----------------------------------------------------------------------

      fuel_loop: do iFuel = 1, iNumFuel

        use_loop: do iEndUse = 1, iNumUses

C.........Initialize fuel/end-use combination to zero.
          fSiteFuelCost ( iH3KBaseLoads, iFuel, iEndUse ) = 0.

C.........Sum base loads fuel/end-use fuel costfor all zones
          H3KBaseLoads_loop: do iZone =1 , ncomp

            fSiteFuelCost( iH3KBaseLoads, iFuel, iEndUse ) =
     &           fSiteFuelCost( iH3KBaseLoads, iFuel, iEndUse )
     &         + fH3KBaseLoadsFuelCost(iZone, iFuel, iEndUse)

          enddo H3KBaseLoads_loop

        enddo use_loop

      enddo fuel_loop


      return
      end

C=AggregatePltFuelCost==================================================
C
C ABSTRACT
C
C AggregatePltFuelCost aggregates fuel costs specific
C to plant components, and appends it to the site-wide fuel
C cost array.
C
C Inputs:
C
C   - None.
C
C=======================================================================
      subroutine AggregatePltFuelCost()

      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

      ! Zone information
      common/c1/ncomp,ncon
      integer ncomp          ! number of zones
      integer ncon           ! not used

      common/c9/nPComp,NCI(mpcom),CData(mpcom,mmiscd)
      integer nPComp                     ! # of plant component models
      integer nci                        ! not used
      real CData                         ! not used

C-----------------------------------------------------------------------
C     Loop through fuels, end-uses and sum fuel consumption
C-----------------------------------------------------------------------

      fuel_loop: do iFuel = 1, iNumFuel

        use_loop: do iEndUse = 1, iNumUses

C.........Initialize fuel/end-use combination to zero.
          fSiteFuelCost ( iPltComponents, iFuel, iEndUse ) = 0.

C.........Sum base loads fuel/end-use fuel costfor all zones
          plt_component_loop: do iPltComp =1 , nPComp

            fSiteFuelCost( iPltComponents, iFuel, iEndUse ) =
     &           fSiteFuelCost( iPltComponents, iFuel, iEndUse )
     &         + fPltFuelCost(iPltComp, iFuel, iEndUse)

          enddo plt_component_loop

        enddo use_loop

      enddo fuel_loop


      return
      end      
      
C=TotalFuelCost=========================================================
C
C ABSTRACT:
C
C TotalFuelCost computes the total fuel cost for each fuel and end use
C in the current time-step.
C
C=======================================================================
      subroutine TotalFuelCost()
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

C-----------------------------------------------------------------------
C     Loop through all components, end uses; and sum fuel cost.
C-----------------------------------------------------------------------
      fuel_loop: do iFuel = 1, iNumFuel

C.......Zero total enery use (by fuel) array slot
        fTotalFuelCost( iFuel ) = 0.0

        use_loop: do iEndUse = 1, iNumUses

C.........Zero end-use energy use (by fuel) array slot
          fEndUseFuelCost ( iFuel, iEndUse ) = 0.0

          component_loop: do iSiteComp = 1, iNumOffsiteUtilComp

            fTotalFuelCost( iFuel ) =
     &               fTotalFuelCost( iFuel )
     &             + fSiteFuelCost( iSiteComp, iFuel, iEndUse )

            fEndUseFuelCost ( iFuel, iEndUse ) =
     &              fEndUseFuelCost ( iFuel, iEndUse )
     &            + fSiteFuelCost( iSiteComp, iFuel, iEndUse ) 
	 
C...........Add minimum monthly fuel charge to end use type iExtraBillingCharges
            call AddMonthlyMinFuelCharge( iEndUse, iFuel )

          enddo component_loop

        enddo use_loop

      enddo fuel_loop

      return
      end      

C=AttributePltEnergyUse==============================================
C
C ABSTRACT:
C
C AttributePltEnergyUse analyses uncatagorized energy consumption reported in
C the plant domains and attempts to attribute them according to
C end-use.
C
C Inputs: None.
C
CC=======================================================================
      subroutine AttributePltEnergyUse()

      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

C.....ESP commons
      common/c9/nPComp,NCI(mpcom),CData(mpcom,mmiscd)
      integer nPComp                     ! # of plant component models
      integer nci                        ! not used
      real CData                         ! not used


      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      integer nPCon                      ! # of connections
      integer iPc1                       ! Index of receiving component
      integer iPn1                       ! Index of receiving node
      integer iPcT                       ! Type of connection.
      integer iPc2                       ! Index of sending component
      integer iPn2                       ! Index of sending node
      real PConDR                        ! Connection diversion ratio
      real pconsd                        ! Supplemental data array.

      common/report_loads/fReportDHWLoad(mpcom)
      real fReportDHWLoad                ! DHW load computed by a plant
                                         ! component (W)

C.....Time
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      integer isd1              ! not used in current context
      integer ism1              ! not used in current context
      integer isd2              ! not used in current context
      integer ism2              ! not used in current context
      integer isds              ! not used in current context
      integer isdf              ! not used in current context
      integer ntstep            ! number of building steps/hour

      common/pctstp/ntstpp
      integer ntstpp            ! number of plant timesteps / building timestep
      integer iNumSubSteps      ! local copy of ntstpp
      common/c12ps/npcdat,ipofs1,ipofs2
      integer npcdat(mpcom,9)      ! miscellaneous plant data
      integer ipofs1(mcoefg)       ! not used in current context
      integer ipofs2(mcoefg,mpvar) ! not used in current context

C.....Plant present and future state variables
      common/pcval/csvf,csvp
      real csvf(mpnode,mpvar)   ! future time-row state variables
      real csvp(mpnode,mpvar)   ! present time-row state variables

C.....Local variables
      integer iConnect                   ! Counter

C.....Storage arrays for upto one week's data of end-use loads at 1 min ts
      integer iStoreSize
      parameter ( iStoreSize = 60*24*7 )
      real fLoadSH( iStoreSize )            ! Space heating load (W)
      real fLoadSC( iStoreSize )            ! Space cooling load (W)
      real fLoadWH( iStoreSize )            ! Water heating load (W)
      real fLoadElUser( iStoreSize )        ! Occupant-driven electric load (W)
      real fLoadElEquip( iStoreSize )       ! HVAC/equipment loads (W)

      data fLoadSH / iStoreSize * 0.0 /
      data fLoadSC / iStoreSize * 0.0 /
      data fLoadWH / iStoreSize * 0.0 /
      data fLoadElUser / iStoreSize * 0.0 /
      data fLoadElEquip / iStoreSize * 0.0 /

      save fLoadSH, fLoadSC, fLoadWH, fLoadElUser, fLoadElEquip

C.....Average loads (W)
      real fLoadSHAvg, fLoadSCAvg, fLoadWHAvg, fLoadElUserAvg,
     &     fLoadElEquipAvg
      save fLoadSHAvg, fLoadSCAvg, fLoadWHAvg, fLoadElUserAvg,
     &     fLoadElEquipAvg

      data fLoadSHAvg, fLoadSCAvg, fLoadWHAvg, fLoadElUserAvg,
     &     fLoadElEquipAvg / 0.0, 0.0, 0.0, 0.0, 0.0 /

      real fLoadTotalAvg

C.....Ratios of end-use loads to total load

      real fLoadRatioSH
      real fLoadRatioWH

C.....Current loads (W)
      real fLoadSHCur, fLoadSCCur, fLoadWHCur, fLoadElUserCur,
     &     fLoadElEquipCur

C.....Number of records in buffer
      integer iBufferLength         ! Length of storage arrays
      integer iBufferCount          ! Current number of records in arrays
      integer iBufferLoc            ! Current location within buffer.

      data iBufferCount /0/

C.....Plant network topology
      integer iCompSupply, iCompReturn ! Indicies of supply & return components
      integer iNodeSupply, iNodeReturn ! Indicies of supply & return nodes
      integer iCompType                ! Plant component type
C.....Plant solution data
      real fTempSupply, fTempReturn    ! Temperature at zone supply, return (oC)
      real fHeatCapacity               ! Heat capacity of air stream (J/oC)
      real fSpaceLoad                  ! Computed space load

C.....Flag for close-to-zero comparison.
      logical bNumsAreClose

C.....References
      real SHtFld                      ! Returns specific heat of a fluid (J/kg oC)
      real CndWat1                     ! Returns heat of condensation of water (J/kg)
      real H3K_Connect_property        ! Returns connection properties

C-----------------------------------------------------------------------
C     Determine length of buffer:
C
C        # of timesteps in a 1 week period =
C             ( # bld ts/hour ) * ( # plt ts / bld ts )
C                               * ( 24 hours )
C                               * ( 7 days )
C-----------------------------------------------------------------------
      ! ntstpp may not be set, ensure that it's at least 1.
      iNumSubSteps = max( ntstpp, 1 )
      iBufferLength = min ( ntstep * iNumSubSteps * 24 * 7, iStoreSize )

C-----------------------------------------------------------------------
C     Zero load parameters (W)
C-----------------------------------------------------------------------
      fLoadSHCur      = 0.0
      fLoadSCCur      = 0.0
      fLoadWHCur      = 0.0
      fLoadElUserCur  = 0.0
      fLoadElEquipCur = 0.0

C-----------------------------------------------------------------------
C     Loop through connections, and establish heating/cooling loads
C     associated with connections to zones.
C-----------------------------------------------------------------------
      do_connections_exist: if ( nPCon > 0 ) then
      connection_loop: do iConnect = 1, nPCon

C.......Check for type-4 connection: coupling between plant
C.......and zone.

        connected_to_zone: if ( iPcT( iConnect ) == 4 ) then

C.........Collect supply & return temperatures for type 4 connection
          iCompReturn = iPc1( iConnect )
          iNodeReturn = npcdat(iCompReturn,9) + ( iPn1( iConnect ) - 1 )

          iCompSupply = iPc2( iConnect )
          iNodeSupply = npcdat(iCompSupply,9) + ( iPn2( iConnect ) - 1 )

          fTempSupply = csvf(iNodeSupply,1)   ! (oc)
          fTempReturn = H3K_Connect_property(iConnect,1) ! (oC)


C.........Collect specific heat capacity
          fHeatCapacity =
     &        csvf(iNodeSupply,2)
     &           * 0.5 * (    SHtFld( 1, fTempSupply )
     &                      + SHtFld( 1, fTempReturn ) )
     &
     &      + min ( csvf(iNodeSupply,3), csvf(iNodeReturn,3) )
     &           * 0.5 * (    SHtFld( 2, fTempSupply )
     &                      + SHtFld( 2, fTempReturn ) )

C.........Compute load
          fSpaceLoad = fHeatCapacity * ( fTempSupply  - fTempReturn )
     &          + ( csvf(iNodeSupply,3) - csvf(iNodeReturn,3) )
     &              * CndWat1( 0.5* ( fTempSupply + fTempReturn ) )

          if ( fSpaceLoad > 0.0 ) then

            fLoadSHCur = fLoadSHCur + fSpaceLoad

          else

            fLoadSHCur = fLoadSHCur + ABS(fSpaceLoad)

          endif

        endif connected_to_zone

      enddo connection_loop
      endif do_connections_exist
C-----------------------------------------------------------------------
C     Loop through plant components, and collect DHW loads from known
C     (this loop might one-day be expanded to collect space loads from
C     radiant systems as well.)
C-----------------------------------------------------------------------
      do_plt_comp_exist: if (nPComp > 0 ) then
      component_loop_1: do iPltComp = 1, nPComp

C.......Collect component type:
        iCompType = nPCdat(iPltComp, 4)

        Plant_type: select case ( iCompType )

C.........Hot water tanks with DHW loads
          case (81,82)

            fLoadWHCur = fLoadWHCur + fReportDHWLoad ( iPltComp )

          case default

C...........Do nothing.

        end select Plant_type

      enddo component_loop_1
      endif do_plt_comp_exist
C-----------------------------------------------------------------------
C     Shift data in load buffers.
C-----------------------------------------------------------------------

C.....If current buffer location is less than length, increment buffer
C.....location,
      buffer_length: if ( iBufferCount < iBufferLength ) then

        iBufferCount = iBufferCount + 1

C.....Otherwise, shift data in buffers, dropping off oldest data
C.....and creating space at the end
      else

        shift_buffers: do iBufferLoc = 1, iBufferCount -1

          fLoadSH(iBufferLoc) = fLoadSH( iBufferLoc + 1 )
          fLoadWH(iBufferLoc) = fLoadWH( iBufferLoc + 1 )

        enddo shift_buffers

      endif buffer_length

C.....Append most recent data to end of buffer.
      fLoadSH( iBufferCount ) = fLoadSHCur
      fLoadWH( iBufferCount ) = fLoadWHCur

C-----------------------------------------------------------------------
C     (Re)Compute average loads over the buffer. Add most recent value,
C     and subtract oldest value in buffer.
C-----------------------------------------------------------------------
      fLoadSHAvg = fLoadSHAvg + fLoadSHCur   / real( iBufferLength )
     &                        - fLoadSH( 1 ) / real( iBufferLength )

      fLoadWHAvg = fLoadWHAvg + fLoadWHCur   / real( iBufferLength )
     &                        - fLoadWH( 1 ) / real( iBufferLength )

C-----------------------------------------------------------------------
C     Compute total thermal load
C-----------------------------------------------------------------------
      fLoadTotalAvg = fLoadSHAvg + fLoadWHAvg

      call eclose ( fLoadTotalAvg, 0.0, 0.01, bNumsAreClose )

      if ( bNumsAreClose ) then

        fLoadRatioSH = 0.5
        fLoadRatioWH = 0.5

      else

        fLoadRatioSH = fLoadSHAvg + fLoadWHAvg

      endif


C-----------------------------------------------------------------------
C     Now loop through plant components, and reassign 'uncatagorized'
C     energy end-use based on computed load ratios
C-----------------------------------------------------------------------

      component_loop_2: do iPltComp = 1, nPComp

        fuel_loop: do iFuel = 1, iNumFuel

          uncatagorized:
     &    if ( .not. bPltUseCatagorized(iPltComp, iFuel ) .and.
     &         .not. bNumsAreClose ) then

            fPltEnergyUse(iPltComp, iFuel, iUseSpaceHeat )
     &        =   fPltEnergyUse( iPltComp, iFuel, iUseUncatagorized )
     &          * fLoadRatioSH

            fPltEnergyUse(iPltComp, iFuel, iUseDHW)
     &        =   fPltEnergyUse( iPltComp, iFuel, iUseUncatagorized )
     &          * fLoadRatioWH

            fPltEnergyUse( iPltComp, iFuel, iUseUncatagorized ) = 0.0

          endif uncatagorized

        enddo fuel_loop

      enddo component_loop_2

      return
      end

C=fConvertEnergyToFuelAmount============================================
C
C ABSTRACT:
C
C fConvertEnergyToFuelAmount converts an energy input into an equivlent
C amount of fuel. It makes the OffsiteUtilities conversion factors
C available elsewhere in the simulator.
C
C Inputs:
C
C  -fEnergy:   Energy used (W)
C  -iFuel:     Integer indicating the type of fuel used.
C
C
C  Outputs:
C
C  -fConvertEnergyToFuelAmount: Equivlent amount of fuel (various units)
C
C=======================================================================
      real function fConvertEnergyToFuelAmount(fEnergy,iFuelIndex)
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

C.....Arguements
      real fEnergy
      integer iFuelIndex

      fConvertEnergyToFuelAmount =   fEnergy
     &                             * fFuelConversionFactor(iFuelIndex)

      return
      end

C=ResetSumFuelUseArray==================================================
C
C ABSTRACT:
C
C ResetSumFuelUseArray resets the fuel use summation array fSumFuelUse
C at the beginning of each month.
C
C=======================================================================
      subroutine ResetSumFuelUseArray()
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      integer ihrp           ! present hour
      integer ihrf           ! not used
      integer idyp           ! present day
      integer idyf           ! not used
      integer idwp           ! not used
      integer idwf           ! not used
      integer nsinc          ! timestep number
      integer its            ! index of timestep in current hour

      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      integer isd1
      integer ism1
      integer isd2
      integer ism2
      integer isds
      integer isdf
      integer ntstep         ! number of simulation time-steps / hour

      integer idyp_adjust, iMonth
      real temp_day
      integer h3k_month

C.....initialize iCurrentMonth
      if ( nsinc .eq. 1) then
            iCurrentMonth = 0
      end if

C-----------------------------------------------------------------------
C     Get current day and month.
C-----------------------------------------------------------------------
C.....Building simulation day (present) ->real number

      if ( ihrp .eq. 24 .and. idyp .eq. 365) then
         idyp_adjust = idyp - 365
      else
         idyp_adjust = idyp
      endif

C.....Calculate present 'real' day -> day number
      temp_day = real(idyp_adjust)
     &         +(real(ihrp)+real(its-1)/real(ntstep))/24.0

      iMonth = h3k_month(temp_day)

C-----------------------------------------------------------------------
C     Loop through all fuels and set fSumFuelUse to 0, if change
C     in month is detected.
C-----------------------------------------------------------------------
      if ( iMonth .ne. iCurrentMonth ) then
            fuel_loop: do iFuel = 1, iNumFuel
                  bApplyMonthlyMinCharge( iFuel ) = .true.
                  fSumFuelUse( iFuel ) = 0.0
            enddo fuel_loop
      end if

C.....save this timestep's month index
      iCurrentMonth = iMonth

      return
      end

C=AddToSumFuelUseArray=============================================
C
C ABSTRACT:
C
C AddToSumFuelUseArray adds the passed energy use to the SumFuelUse
C array.
C
C=======================================================================
      subroutine AddToSumFuelUseArray(fSUFuelEnergyUse)
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      integer isd1
      integer ism1
      integer isd2
      integer ism2
      integer isds
      integer isdf
      integer ntstep         ! number of simulation time-steps / hour

C-----------------------------------------------------------------------
C     Loop through all fuels and uses and add to fSumFuelUse array.
C     fFuelConversionFactor converts from W for the current time
C     step to kWh/s (electricity), m3/s (gas), L/s(oil, propane), Tonne/s (wood)
C     Need to convert to units of fuel consumption for this timestep so that we
C     are summing up the fuel use thus far.
C-----------------------------------------------------------------------

      fuel_loop: do iFuel = 1, iNumFuel

        use_loop: do iEndUse = 1, iNumUses

            fSumFuelUse( iFuel ) =
     &                  fSumFuelUse( iFuel ) +
     &                  fSUFuelEnergyUse( iFuel, iEndUse )
     &                  * fFuelConversionFactor( iFuel )
     &                  * 3600.0 / real(ntstep)

        enddo use_loop

      enddo fuel_loop

      return
      end

C=AddMonthlyMinFuelCharge===============================================
C
C ABSTRACT:
C
C AddMonthlyMinFuelCharge adds the monthly minimum fuel charge to end use
C iExtraBillingCharges and resets the bApplyMonthlyMinCharge flag so that
C this only occurs once a month.
C=======================================================================
      subroutine AddMonthlyMinFuelCharge(iEndUse, iFuel)
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      integer ihrp           ! present hour
      integer ihrf           ! not used
      integer idyp           ! present day
      integer idyf           ! not used
      integer idwp           ! not used
      integer idwf           ! not used
      integer nsinc          ! not used
      integer its            ! index of timestep in current hour

      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      integer isd1
      integer ism1
      integer isd2
      integer ism2
      integer isds
      integer isdf
      integer ntstep         ! number of simulation time-steps / hour

      integer idyp_adjust, iMonth
      real temp_day
      integer h3k_month

C-----------------------------------------------------------------------
C     Get current day and month.
C-----------------------------------------------------------------------
C.....Building simulation day (present) ->real number

      if ( ihrp .eq. 24 .and. idyp .eq. 365) then
         idyp_adjust = idyp - 365
      else
         idyp_adjust = idyp
      endif

C.....Calculate present 'real' day -> day number
      temp_day = real(idyp_adjust)
     &         +(real(ihrp)+real(its-1)/real(ntstep))/24.0

      iMonth = h3k_month(temp_day)

C-----------------------------------------------------------------------
C     Apply the minimum cost and reset bApplyMonthlyMinCharge.
C-----------------------------------------------------------------------
      if (iEndUse .eq. iExtraBillingCharges)then

            if( bApplyMonthlyMinCharge(iFuel) )then

                  fEndUseFuelCost ( iFuel, iEndUse ) =
     &                        fEndUseFuelCost ( iFuel, iEndUse )
     &                      + fFuelMinCharge( iFuel, iMonth )

                  bApplyMonthlyMinCharge( iFuel ) = .false.

            end if

      end if

      return
      end

C=fGetFuelCost==========================================================
C
C ABSTRACT:
C
C fGetFuelCost fetches the appropriate fuel cost from private arrays
C that store BC fuelcost data.
C
C Inputs:
C
C  -iFuel:     Integer indicating the type of fuel used.
C
C
C  Outputs:
C
C  -fGetFuelCost: Fuel cost ($ / various energy units)
C
C=======================================================================
      real function fGetFuelCost(iFuelIndex)
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

C.....Arguements
      integer iFuelIndex
      integer h3k_month

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      integer ihrp           ! present hour
      integer ihrf           ! not used
      integer idyp           ! present day
      integer idyf           ! not used
      integer idwp           ! not used
      integer idwf           ! not used
      integer nsinc          ! not used
      integer its            ! index of timestep in current hour

      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      integer isd1
      integer ism1
      integer isd2
      integer ism2
      integer isds
      integer isdf
      integer ntstep         ! number of simulation time-steps / hour

      integer iMonth, idyp_adjust
      real temp_day

C.....Variables for Access to the Boundary Condition Data
      Real fConvert_current_TS_to_DAY     ! returns day (float)
      Real fGet_BC_data_by_col            ! return the data for the timestep
      Integer iInterp_method              ! 1 = step-wise; 2 = linear
      Character cContext*124              ! string to print on BCD failure

      IF ( iFuelIndex.eq.iElectricity .and. bTOUelecRates ) THEN

C...........Set the BCD interpolation method
            iInterp_method = 1 ! 1 = step-wise; 2 = linear

            write (cContext,*) "Determining TOU fuel cost..."

            fGetFuelCost = fGet_BC_data_by_col(
     &        iTOUcolumn,
     &        fConvert_current_TS_to_DAY(),
     &        iInterp_method,
     &        cContext)

      ELSE

C...........Get current day and month

            if ( ihrp .eq. 24 .and. idyp .eq. 365) then
                  idyp_adjust = idyp - 365
            else
                  idyp_adjust = idyp
            endif

C...........Calculate present 'real' day -> day number
            temp_day = real(idyp_adjust)
     &         +(real(ihrp)+real(its-1)/real(ntstep))/24.0

            iMonth = h3k_month(temp_day)

            if ( fSumFuelUse( iFuelIndex ) .le.
     &           fFuelMinUnits(iFuelIndex, iMonth) )then

                  fGetFuelCost = 0.0

            else if ( fSumFuelUse( iFuelIndex ) .gt.
     &                fFuelMinUnits(iFuelIndex, iMonth) .and.
     &                fSumFuelUse( iFuelIndex ) .le.
     &                fFuelBlockUnits(iFuelIndex, iMonth, 1) )then

                  fGetFuelCost = fFuelBlockCost(iFuelIndex,iMonth,1)

            else if ( fSumFuelUse( iFuelIndex ) .gt.
     &                fFuelBlockUnits(iFuelIndex, iMonth, 1) .and.
     &                fSumFuelUse( iFuelIndex ) .le.
     &                fFuelBlockUnits(iFuelIndex, iMonth, 2) ) then

                  fGetFuelCost = fFuelBlockCost(iFuelIndex,iMonth,2)

            else if ( fSumFuelUse( iFuelIndex ) .gt.
     &                fFuelBlockUnits(iFuelIndex, iMonth, 2) .and.
     &                fSumFuelUse( iFuelIndex ) .le.
     &                fFuelBlockUnits(iFuelIndex, iMonth, 3) ) then

                  fGetFuelCost = fFuelBlockCost(iFuelIndex,iMonth,3)

            else if ( fSumFuelUse( iFuelIndex ) .gt.
     &                fFuelBlockUnits(iFuelIndex, iMonth, 3) ) then

                  fGetFuelCost = fFuelBlockCost(iFuelIndex,iMonth,4)

            else
                  fGetFuelCost = 0.0
            end if

        END IF

      return
      end
