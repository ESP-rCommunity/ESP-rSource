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

C.....Attribute uncatagorized energy use in elec, plant domains.
      call AttributePltPfsEnergyUse()

C.....Aggregate energy use from all plant components
      call AggregatePltCompEnergyUse()

C.....Compute site-wide energy consumption
      call TotalEnergyConsumption()

C.....Convert purchased energy to fuel quantity
      call ConvertEnergyToFuel()

C.....Report fuel use data
      call H3KReportsFuelUse()

C.....Reset storage array for next time step.
      call ZeroFuelEnergyStorageArray()

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
#include "plant.h"
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

          component_loop: do iComponent = 1, iNumOffsiteUtilComp

            fTotalEnergyUse( iFuel ) =
     &               fTotalEnergyUse( iFuel )
     &             + fSiteEnergyUse( iComponent, iFuel, iEndUse )

            fEndUseEnergyUse ( iFuel, iEndUse ) =
     &              fEndUseEnergyUse ( iFuel, iEndUse )
     &            + fSiteEnergyUse( iComponent, iFuel, iEndUse )

          enddo component_loop

        enddo use_loop

      enddo fuel_loop

      return
      end

C=ConvertEnergyToFuel===================================================
C
C ABSTRACT:
C
C ConvertEnergyToFuel converts the total energy use associated with a given
C fuel into a standard unit of measure, based on the fuel's calorific
C value.
C
C=======================================================================
      subroutine ConvertEnergyToFuel()
      implicit none
#include "plant.h"
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
#include "plant.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

      logical bCloseToZero
      integer iRVId; !Report Variable Identifier

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
         CASE (iNaturalGas)
            iRVId = rvTFuelAllEndQtyNaturalGas%Identifier
         CASE (iOil)
            iRVId = rvTFuelAllEndQtyOil%Identifier
         CASE (iPropane)
            iRVId = rvTFuelAllEndQtyPropane%Identifier
         CASE (iMixedWood)
            iRVId = rvTFuelAllEndQtyMixedWood%Identifier
         CASE (iHardWood)
            iRVId = rvTFuelAllEndQtyHardWood%Identifier
         CASE (iSoftWood)
            iRVId = rvTFuelAllEndQtySoftWood%Identifier
         CASE (iWoodPellets)
            iRVId = rvTFuelAllEndQtyWoodPellet%Identifier
         CASE DEFAULT
            !Should never execute, but just in case
            iRVId = rvTFuelAllEndQty%Identifier
       END SELECT


         call AddToReport(
     &         iRVId,
     &         fTotalFuelUse(iFuel))

C-----------------------------------------------------------------------
C         Consumption by fuel, end-use; suppress
C         energy flows < 0.01 W
C-----------------------------------------------------------------------

C.......end-use loop:
        use_loop: do iEndUse = 1, iNumUses
        call eclose ( fEndUseEnergyUse(iFuel,iEndUse),
     &                0.0, 0.01, bCloseToZero)

        if ( .not. bCloseToZero ) then

         SELECT CASE (iFuel)
         CASE (iElectricity)
            iRVId = rvTFuelQtyElec%Identifier
         CASE (iNaturalGas)
            iRVId = rvTFuelQtyNatGas%Identifier
         CASE (iOil)
            iRVId = rvTFuelQtyOil%Identifier
         CASE (iPropane)
            iRVId = rvTFuelQtyProp%Identifier
         CASE (iMixedWood)
            iRVId = rvTFuelQtyMixWood%Identifier
         CASE (iHardWood)
            iRVId = rvTFuelQtyHardWood%Identifier
         CASE (iSoftWood)
            iRVId = rvTFuelQtySoftWood%Identifier
         CASE (iWoodPellets)
            iRVId = rvTFuelQtyPellets%Identifier
         CASE DEFAULT
            !Should never execute, but just in case
            iRVId = rvTFuelQty%Identifier
         END SELECT


        !Claude remove the dynamic description, if needed I can add back
        call AddToReportWild1(
     &         iRVId,
     &         fEndUseFuelUse(iFuel,iEndUse),
     &         cUseName(iEndUse)(1:iUseNameLen(iEndUse)))
        endif
        enddo use_loop

      enddo fuel_loop
      return
      end

C=ZeroFuelEnergyStorageArray============================================
C
C ABSTRACT:
C
C ZeroFuelEnergyStorageArray zeros the site energy usage storage array.
C
C=======================================================================
      subroutine ZeroFuelEnergyStorageArray()
      implicit none
#include "plant.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

C-----------------------------------------------------------------------
C     Loop through all components, and sum fuel consumption.
C-----------------------------------------------------------------------
      fuel_loop: do iFuel = 1, iNumFuel

        component_loop: do iComponent = 1, iNumOffsiteUtilComp

          use_loop: do iEndUse = 1, iNumUses

            fSiteEnergyUse( iComponent, iFuel, iEndUse ) = 0.0

          enddo use_loop

        enddo component_loop

      enddo fuel_loop

      return
      end

C=ZeroPltEnergyStorageArray=============================================
C
C ABSTRACT:
C
C ZeroPltEnergyStorageArray zeros the site energy usage storage array.
C
C=======================================================================
      subroutine ZeroPltEnergyStorageArray()
      implicit none
#include "plant.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

C.....ESP commons
      common/c9/nPComp,NCI(mpcom),CData(mpcom,mmiscd)
      integer nPComp                     ! # of plant component models
      integer nci                        ! not used
      real CData                         ! not used

      common/report_loads/fReportDHWLoad(mpcom)
      real fReportDHWLoad                ! DHW load computed by a plant
                                         ! component (W)

C-----------------------------------------------------------------------
C     Loop through all components, and sum fuel consumption.
C-----------------------------------------------------------------------
      component_loop: do iComponent = 1, nPComp

        fuel_loop: do iFuel = 1, iNumFuel

          use_loop: do iEndUse = 1, iNumUses

            fPltEnergyUse( iComponent, iFuel, iEndUse ) = 0.0

          enddo use_loop

        enddo fuel_loop

C.......Zero per-component load arrays
        fReportDHWLoad( iComponent ) = 0.0

      enddo component_loop

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
C   - iComponent: integer flag indicating where data originated
C
C   - fSUFuelEnergyUse: 2d array used transport fuel consumption
C     data into Site utilities facility. Array indicies correspond
C     to fuel, end-use.
C
C=======================================================================
      subroutine StoreSiteEnergyUse(iComponent, fSUFuelEnergyUse )
      implicit none
#include "plant.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

C-----------------------------------------------------------------------
C     Loop through fuels, end-uses and stuff fuel consumption in
C     protected fSiteEnergyUse array
C-----------------------------------------------------------------------
      fuel_loop: do iFuel = 1, iNumFuel

        use_loop: do iEndUse = 1, iNumUses

          fSiteEnergyUse( iComponent, iFuel, iEndUse ) =
     &       fSUFuelEnergyUse ( iFuel, iEndUse )

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
C   - iComponent: Plant component index
C
C   - fSUFuelEnergyUse: 2d array used transport fuel consumption
C     data into Site utilities facility. Array indicies correspond
C     to fuel, end-use.
C
C=======================================================================
      subroutine StorePltCompEnergyUse(iComponent, fSUFuelEnergyUse )
      implicit none
#include "plant.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

      logical bNumsAreClose

C-----------------------------------------------------------------------
C     Loop through fuels, end-uses and stuff fuel consumption into
C     protected fSiteEnergyUse array
C-----------------------------------------------------------------------
      fuel_loop: do iFuel = 1, iNumFuel

        use_loop: do iEndUse = 1, iNumUses

          fPltEnergyUse( iComponent, iFuel, iEndUse ) =
     &       fSUFuelEnergyUse( iFuel, iEndUse )

C...........Mark uncatagorized end-uses for reapportionment
            if ( iEndUse == iUseUncatagorized ) then

              call eclose ( fSUFuelEnergyUse( iFuel, iEndUse ),
     &                      0.0, 0.01, bNumsAreClose)
              if ( bNumsAreClose .and.
     &             fSUFuelEnergyUse( iFuel, iEndUse ) > 0.0 ) then

                bPltUseCatagorized(iComponent, iFuel) = .false.

              else

                bPltUseCatagorized(iComponent, iFuel) = .false.

              endif

            endif

        enddo use_loop

      enddo fuel_loop

      return
      end

C=AggregatePltCompEnergyUse=============================================
C
C ABSTRACT
C
C AggregatePltCompEnergy aggregates energy consumption computed
C by various plant components, and appends it to the site-wide
C energy consumption array.
C
C Inputs:
C
C   - None.
C
C=======================================================================
      subroutine AggregatePltCompEnergyUse()

      implicit none
#include "plant.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

C.....ESP commons
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
          fSiteEnergyUse( iPltComponents, iFuel, iEndUse ) = 0.0

C.........Sum fuel/end-use consumtion for all components
          plt_component_loop: do iComponent =1 , nPComp

            fSiteEnergyUse( iPltComponents, iFuel, iEndUse ) =
     &           fSiteEnergyUse( iPltComponents, iFuel, iEndUse )
     &         + fPltEnergyUse( iComponent, iFuel, iEndUse )

          enddo plt_component_loop

        enddo use_loop

      enddo fuel_loop


      return
      end

C=AttributePltPfsEnergyUse==============================================
C
C ABSTRACT:
C
C AttributePltPfsEnergyUse analyses uncatagorized energy consumption reported in
C the plant/elec domains and attempts to attribute them according to
C end-use.
C
C Inputs: None.
C
CC=======================================================================
      subroutine AttributePltPfsEnergyUse()

      implicit none
#include "plant.h"
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
      component_loop_1: do iComponent = 1, nPComp

C.......Collect component type:
        iCompType = nPCdat(iComponent, 4)

        Plant_type: select case ( iCompType )

C.........Hot water tanks with DHW loads
          case (81,82)

            fLoadWHCur = fLoadWHCur + fReportDHWLoad ( iComponent )

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

      component_loop_2: do iComponent = 1, nPComp

        fuel_loop: do iFuel = 1, iNumFuel

          uncatagorized:
     &    if ( .not. bPltUseCatagorized(iComponent, iFuel ) .and.
     &         .not. bNumsAreClose ) then

            fPltEnergyUse(iComponent, iFuel, iUseSpaceHeat )
     &        =   fPltEnergyUse( iComponent, iFuel, iUseUncatagorized )
     &          * fLoadRatioSH

            fPltEnergyUse(iComponent, iFuel, iUseDHW)
     &        =   fPltEnergyUse( iComponent, iFuel, iUseUncatagorized )
     &          * fLoadRatioWH

            fPltEnergyUse( iComponent, iFuel, iUseUncatagorized ) = 0.0

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
#include "plant.h"
#include "OffsiteUtilitiesPublic.h"
#include "OffsiteUtilitiesPrivate.h"

C.....Arguements
      real fEnergy
      integer iFuelIndex

      fConvertEnergyToFuelAmount =   fEnergy
     &                             * fFuelConversionFactor(iFuelIndex)

      return
      end

