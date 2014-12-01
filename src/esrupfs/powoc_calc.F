C This file contains the following subroutines related to the calculation
C of the electrical generation and load from power-only components.
C These are used by ESP-r's electric power flow solution domain.

C   POWOC_CALC             Controlling routine.
C   POWOC_nonHVACld        Determines the non-HVAC electrical load for the current
C                          time-step based upon data read from an ASCII file.
C   POWC_CETC_HVAC_ld      Determines the HVAC loads on the electrical network from
C                          HVAC equipment modeled ideally within HOT3000
C   POWOC_OUTSIDE_ld       Determines the electric load on the network from the outside
C                          such as when the electrical network supplies a clustor
C                          of houses. The load is read from an ASCII file.
C   POWOC_TRANSFORMER_loss Determines the transformer losses at a wind farm and
C                          applies them as a load on the electrical network.
C   POWOC_TxDx_loss        Determines the power load on the electrical network due
C                          to transmission and distribution system losses.
C   POWOC_Bldg_PCU         Determines the power load on the electrical network
C                          due to losses from the power conditioning unit that
C                          converts DC power to AC power to meet the buiding
C                          electrical loads.
C   POWOC_Wind_Supp_Elec   Determines the electrical power supply from a wind
C                          farm for the current time step based upon data read
C                          from an ASCII file.
C   POWOC_PCU              Determines the power load to the node sending current to
C                          the PCU and the power source to the node receiving current
C                          from the PCU.
C   POWOC_nonHVACld_BCD    Determines the electrical load demanded by non-HVAC electrical
C                          loads using the boundary condition definition facility,
C
C *******************************************************************************


C *********************************************************************************************
C ********************************** POWOC_CALC ***********************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: March 2, 2004.

C `Power-only' components can be connected to an electrical network within ESP-r's electric
C power flow solution domain.  The power generation produced by or the load caused by
C a power-only component is imposed as a boundary condition upon the power flow solution.
C This subroutine is invoked on a time-step basis each iteration through the power flow
C solution domain to determine the generation and load associated with the power-only
C components.

C INPUTS:
C    IPWC           The index of the power-only component under consideration
C                   (e.g. for a network with 2 power-only components, IPWC will
C                   be either 1 or 2). [from calling statement]
C    POWCOMID(i)    The database ID of the i'th power-only component in the
C                   electrical network. [from common/POWC]

C OUTPUTS:
C    This is a controlling routine. It calls the appropriate routine which
C    subsequently determines the generation or load of the power-only component.

C --------------------------------------------------------------------------------
      SUBROUTINE POWOC_CALC(IPWC)
#include "building.h"
#include "plant.h"
#include "power.h"
      COMMON/OUTIN/IUOUT,IUIN
      integer iuout,iuin

C---------------------------------------------------------------------------------
C Declarations for variables passed in calling statement.
C---------------------------------------------------------------------------------
      INTEGER IPWC

C---------------------------------------------------------------------------------
C Declarations for local variables.
C---------------------------------------------------------------------------------
      INTEGER ITYPE

C---------------------------------------------------------------------------------
C Establish the database ID of the power-only component.  This is the number used
C to uniquely identify the component within the special materials database.
C Refer to the section of the special materials databases marked with `*POWOCOMP'.
C---------------------------------------------------------------------------------
      ITYPE = POWCOMID(IPWC)

C---------------------------------------------------------------------------------
C Calculate the generation or load according to the type of power-only component
C under consideration.
C---------------------------------------------------------------------------------


C-----Non-HVAC electric loads.
      IF (ITYPE.eq.10) THEN
         CALL POWOC_nonHVACld(IPWC)

C-----HVAC electric loads associated with CETC HVAC models
      ELSEIF (ITYPE.eq.11) THEN
         CALL POWOC_CETC_HVAC_ld(IPWC)

C-----Total electric loads (HVAC + non HVAC loads) from the outside
C-----These loads impose an electric load on the network but they do not
C-----contribute any internal gains to the space. These are to be used
C-----when there is a cluster of houses served by the electrical network
C-----inside one building
      ELSEIF (ITYPE.eq.12) THEN
         CALL POWOC_OUTSIDE_ld(IPWC)

C-----Transformer losses
      ELSEIF (ITYPE==13) THEN
         CALL POWOC_TRANSFORMER_loss(IPWC)

C-----Transmission and distribution losses
      ELSEIF (ITYPE==14) THEN
         CALL POWOC_TxDx_loss(IPWC)

C-----Building-side power conditioning unit losses
      ELSEIF (ITYPE==15) THEN
         CALL POWOC_Bldg_PCU(IPWC)

C-----Wind-generated electrical supply
      ELSEIF (ITYPE==16) THEN
         CALL POWOC_Wind_Supp_Elec(IPWC)

C-----Battery
      ELSEIF (ITYPE==17) THEN
         CALL POWOC_CETC_Battery(IPWC)

C-----Non-HVAC electric loads
      elseif ( itype == 18 ) then
         call POWOC_nonHVACld_BCD(IPWC)

C-----Wind-generated electrical supply using profile data.

      ELSEIF (ITYPE==19) THEN
         CALL POWOC_Wind_Prof_Supp_Elec(IPWC)

C-----Power conditioning unit
      elseif ( itype .eq. 20 ) then
         CALL POWOC_PCU(IPWC)

C-----Vanadium Redox Battery (VRB)
      elseif ( itype .eq. 21 ) then
         CALL POWOC_CETC_Battery(IPWC)

C-----Lithium Ion Battery (Li-on)
      elseif ( itype .eq. 22 ) then
         CALL POWOC_CETC_Battery(IPWC)

C-----Electric Vehicle Model (EV)
      elseif ( itype .eq. 23 ) then
         CALL POWOC_EVEHICLE(IPWC)

C-----Boundary Profile from TDF (EV)
      elseif ( itype .eq. 24 ) then
         CALL POWOC_TDF_PROF(IPWC)

C-----Add support for other power-only components here.
c     ELSEIF (ITYPE==??) THEN

C-----This power-only component type is not currently supported.
      ELSE
        WRITE(IUOUT,*) ' No support for this type of power-only comp. ',
     &                 ITYPE
        STOP ' POWOC_CALC: unresolvable error on power-only comp type'
      ENDIF

C---------------------------------------------------------------------------------
      RETURN
      END



C *********************************************************************************************
C ********************************* POWOC_nonHVACld *******************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: March 2, 2004.

C This subroutine determines the electrical load demanded by the non-HVAC power-only
C component.  It is employed to impose non-HVAC electrical loads upon the electric
C power flow modelling domain.  In the first development of fuel-cell cogeneration
C models, the non-HVAC electrical demand was determined within the plant solution
C domain.  This subroutine was added to allow this load to be applied within the
C electric power flow solution domain.  This subroutine is called each time-step
C (perhaps multiple times per time-step as the solver may iterate between the plant
C and electrical domains within a time-step) to establish the non-HVAC load that is
C stored in a binary file.  This binary file is generated by subroutine FC_eloads_convert
C from an ASCII input file (.fcl file).
C
C This routine is now largely a legacy interface for the original POWOC_nonHVACld
C comonent. Most of the functionality has been moved to the fGet_nonHVACld function,
C which can be used by other procedures.

C INPUTS:
C    IPWC           The index of the power-only component under consideration
C                   (e.g. for a network with 2 power-only components, IPWC will
C                   be either 1 or 2).
C    POWCDAT(i,j)   The j'th data item associated with the i'th power-only component
C                   in the electrical network. [from common/POWC]

C OUTPUTS:
C    PPOWOC(i)      The real power demand of the non-HVAC electrical loads (W).
C                   [from common/ENETPOWC]
C    QPOWOC(i)      The reactive power demand of the non-HVAC electrical loads (W).
C                   [from common/ENETPOWC]

C --------------------------------------------------------------------------------
      SUBROUTINE POWOC_nonHVACld(IPWC)
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"


C---------------------------------------------------------------------------------
C Declarations for variables passed in calling statement.
C---------------------------------------------------------------------------------
      INTEGER IPWC

      integer fScale_Factor
      character*72 cFile_Name
      real fGet_nonHVACld           ! Function reference

C.....Get .fcl filename and scaling factor
      write (cFile_Name, '(A)') POWCDATS(IPWC,1)
      fScale_Factor = NINT(POWCDAT(IPWC,1))

C.....Recover loads

      PPOWOC(IPWC) = -1.* fGet_nonHVACld ( cFile_Name ) * fScale_Factor
      QPOWOC(IPWC) = 0.


      RETURN
      END


C *********************************************************************************************
C ********************************* fGet_nonHVACld *******************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: March 2, 2004.

C This function determines the electrical load demanded by the non-HVAC power-only
C component.  This function is called each time-step (perhaps multiple times per
C time-step as the solver may iterate between the plant and electrical domains within
C a time-step) to establish the non-HVAC load that is stored in a binary file.
C This binary file is generated by subroutine FC_eloads_convert from an ASCII input
C file (.fcl file).
C
C The procedures within this function were originally contained wihin the
C POWOC_nonHVACld routine, but have been relocated here for greater flexibility.

C INPUTS:
C    cFile_Name     Name of .fcl file

C    idyp           The day number of the present day. [from common/SIMTIM]
C    btimep         The present time in the building thermal domain. [from common/btime]
C    ptimef         The future time in the plant domain. [from common/ptime]
C    NSINC          The simulation increment counter. [from common/SIMTIM]


C OUTPUTS:
C    fGet_nonHVACld  Occupant-driven electric loads

C --------------------------------------------------------------------------------
      real function fGet_nonHVACld(cFileName)
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"

C External functions.
      integer lnblnk

C Local variables
      character*72  cFileName ! .fcl file name

C Trace & reporting data
      common/tc/itc,icnt
      common/outin/iuout,iuin
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu

      integer itc,icnt
      integer iuout,iuin
      integer itcf,itrace,izntrc,itu

C Common holding simulation type (building, building+plant, or plant-only simulation)
      integer indcfg
      common/c6/indcfg

C---------------------------------------------------------------------------------
C Commons holding simulation time-clock variables.
C---------------------------------------------------------------------------------
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      common/btime/btimep,btimef
      common/ptime/ptimep,ptimef

C---------------------------------------------------------------------------------
C Common holding name of ASCII file containing time-step data on non-HVAC loads.
C---------------------------------------------------------------------------------
      common/Fcell/iFClds,LFClds

C---------------------------------------------------------------------------------
C Common for passing error messages to calling routine (only used when running
C in dll mode).
C---------------------------------------------------------------------------------
      common/dllerr/dllsubr, dllmesg

C---------------------------------------------------------------------------------
C Declarations for common block variables.
C---------------------------------------------------------------------------------

      INTEGER      IHRP, IHRF, IDYP, IDYF, IDWP, IDWF, NSINC, ITS
      REAL         btimep, btimef, ptimep, ptimef
      CHARACTER    LFClds*72
      INTEGER      iFClds
      character    dllsubr*12,dllmesg*124

C---------------------------------------------------------------------------------
C Declarations for local variables.
C---------------------------------------------------------------------------------
      REAL      q_nonHVAC_elec, q_HVAC_elec
      LOGICAL   XST,dll
      CHARACTER OUTS*124
      LOGICAL   FCL_converted

      REAL fFuture_Time                    ! time on future time row.


C--------------------------------------------------------------------------------
C Determine the time corresponding to future time row
C--------------------------------------------------------------------------------
      if ( indcfg .eq. 1 ) then
C........Building+electrical net simulation --- elec. domain is
C........processed at the same frequency as building domain.
C........Building time is 'future time'.

         fFuture_Time = btimef

      elseif ( indcfg .eq. 2 ) then
C........Plant+electrical net simulation --- elec. & plant domains are
C........processed at same frequency. Plant time is 'future time.
         fFuture_Time = ptimef

      elseif ( indcfg .eq. 3 ) then
C........Building+plant+electrical net simulation. --- elec. & plant domains
C........are processed at same frequency. Plant time is 'future time.
         fFuture_Time = ptimef

      else
C........Unsupported simulation type. Shouldn't happen, as indcfg should
C........be error-trapped elsewhere. Give developers a heads-up if
C........other simulation types are created in the future.
         write (itu,'(A,I2,A)')
     &    ' POWOC_nonHVACld(): Specified simulation type (', indcfg,')'
         write (itu,'(A)')
     &    '                    is unsupported. See powoc_calc.F.'
         stop 'POWOC_nonHVACld(): Fatal error!'

      endif


C-------------------------------------------------------------------------------------------
C If this subroutine is being called for the first time then process the user-specified
C electrical loads data. Determine data type and convert from ASCII file to binary-format
C for direct access during simulation.
C-------------------------------------------------------------------------------------------
      IF(iFClds.eq.0) THEN
         LFClds = cFileName
         XST=.FALSE.
         call FINDFIL(LFClds,XST)
         if(XST)then
            iFClds=1
         else
           write(outs,'(3a)') 'FC loads file ',LFClds(1:lnblnk(LFClds)),
     &                        ' was referenced but not found.'
            call isadll(dll)  ! Move this to within loop below.
            if(dll)then
               dllsubr='ersys'
               dllmesg=outs
               return
            else
               call edisp(iuout,outs)
               STOP ' Need elec loads data: unresolvable error.'
            endif
         endif
         CALL FC_eloads_convert
      END IF


C-------------------------------------------------------------------------------------------
C Determine the non-HVAC electrical load for the current time-step.
C-------------------------------------------------------------------------------------------
      CALL FC_eloads_establish(idyp,btimep,fFuture_Time,q_nonHVAC_elec,
     &                             q_HVAC_elec)

      fGet_nonHVACld = q_nonHVAC_elec

      RETURN
      END


C ***********************************POWOC_TDF_PROF********************************************
C Reads real and reactive power flow [rofiles from the temporal database and assigns them  
C to the electrical network model. Positive numbers indicate a load, negative values imply that 
C power is being injected into the network.   
C 
      SUBROUTINE POWOC_TDF_PROF(IPWC)
      implicit none

#include "building.h"
#include "plant.h"
#include "net_flow.h"
#include "power.h"
#include "tdf2.h"


      dimension VAL(MBITS+2)

      INTEGER      IPWC, IFOC, ISD,IER
      REAL         Scale_Factor,VAL, time
      integer ITRC
      REAL         RPOW, QPOW

C Set up data associated with the temporal file read. 
C numerical data from POWCDAT(IPWC,?)
C string data from POWCDATS(IPWC,1)
      time=ENETIMEF
      IFOC=NINT(POWCDAT(IPWC,1)) ! entity number in the tdf database where the information is located
      SCALE_FACTOR=POWCDAT(IPWC,2)
      CALL RCTDFB(ITRC,time,VAL,ISD,IFOC,IER)


C Read data from the temporal file  
      RPOW = VAL(ISD)
      QPOW = VAL(ISD+1)



C Assign real and reactive power flows to the electrical 
      PPOWOC(IPWC) = -1.*RPOW*Scale_Factor
      QPOWOC(IPWC) = -1.*QPOW*Scale_Factor


      RETURN
      END


C *********************************************************************************************
C ********************************* POWOC_CETC_HVAC_ld ****************************************
C     Created by: Alex Ferguson
C     Initial Creation Date: March 9, 2004.
C
C     This subttoutine determines the HVAC-driven electrical loads associated with
C     CETC's idealized plant component models, and imposes these loads on the electrical
C     network.  It specifically ignores hvac models that are implemented in ESP-r's
C     explicit plant network, since these components can be coupled directly to the
C     electrical network (including the TAC and CETC electrical hot water tank models,
C     in which this capability is not implemented as of yet).
C
C     INPUTS:
C     - IPWC           The index of the power-only component under consideration
C                      (e.g. for a network with 2 power-only components, IPWC will
C                      be either 1 or 2).
C     - POWCDAT(i,j)   The j'th data item associated with the i'th power-only component
C                      in the electrical network. [from common/POWC]
C
C     DATA STORED IN FC_ctl_h3khvacloads (W)
C     - FCctl_HRV_draw         -> HRV electrical use
C     - FCctl_AC_compressor    -> AC Air compressor electrical draw
C     - FCctl_AC_circfan       -> AC Circulating fan electrical draw
C     - FCctl_furnace_draft    -> Furnace draft fan electrical draw
C     - FCctl_furnace_circfan  -> Furnace circulating fan electrical draw
C     - q_par_elec             -> SOFC Fuel cell parasitic electrical draw
C     - FCctl_boiler_draft     -> Boiler draft fan electrical draw
C     - FCctl_boiler_circfan   -> Boiler circulating fan electrical draw
C     - FCctl_baseboard_fan    -> Baseboard circulating fan electrical draw
C     - FCctl_ashp_compressor  -> Air source heat pump compressor elec. draw
C     - FCctl_ashp_circfan     -> Air source heat pump circulating fan elec. draw
C     - FCctl_gshp_pump        -> Ground source heat pump pump electrical draw
C
C     OUTPUTS:
C     - PPOWOC(i)      The real power demand of the non-HVAC electrical loads (W).
C                      [from common/ENETPOWC]
C     - QPOWOC(i)      The reactive power demand of the non-HVAC electrical loads (W).
C                      [from common/ENETPOWC]
C
C
C     Notes:
C     1. This routine makes use of the F90 module 'FC_ctl_h3khvacloads',  which
C        stores the power demands of miscellaneous HVAC models. If, or when, CETC
C        decides to migrate back to F77 code, this module will have to be replaced
C        with a set of commonblocks.
C
C     2. Not all of the variables contained within the FC_ctl_h3khvacloads module
C        will be in use during a given simulation.
C
C     3. The fuel cell parasitic electric load data ('q_par_elec') is appended to
C        the total idealized electric loads since the CETC fuel cell model does
C        not deduct this amount from the net electrical generation calculated for
C        the fuel cell and imposed on the electrical network.
C
C --------------------------------------------------------------------------------
      SUBROUTINE POWOC_CETC_HVAC_ld(IPWC)
      implicit none

#include "SOFC.h"
#include "building.h"
#include "plant.h"
#include "power.h"

C---------------------------------------------------------------------------------
C Declarations for variables passed in calling statement.
C---------------------------------------------------------------------------------
      INTEGER IPWC

C---------------------------------------------------------------------------------
C Declarations for local variables.
C---------------------------------------------------------------------------------
      REAL      q_HVAC_elec
      LOGICAL   Debug_me

C      LOGICAL   XST,dll
C      CHARACTER OUTS*124
C      LOGICAL   FCL_converted

C--------------------------------------------------------------------------------
C     Calculate total HVAC electrical loads (W)
C--------------------------------------------------------------------------------
      q_HVAC_elec = FCctl_HRV_draw                                ! HRV
     &            + FCctl_AC_compressor + FCctl_AC_circfan        ! A/C (air-air HP)
     &            + FCctl_furnace_draft + FCctl_furnace_circfan   ! Furnace
     &            + q_par_elec                                    ! FC parasitic losses
     &            + FCctl_boiler_draft + FCctl_boiler_circfan     ! Boiler
     &            + FCctl_baseboard_fan                           ! Baseboards
     &            + FCctl_ashp_compressor + FCctl_ashp_circfan    ! ASHP (heating)
     &            + FCctl_gshp_pump                               ! GSHP

      PPOWOC(IPWC) = -1 * q_HVAC_elec   ! <- note: loads are (-)ive...
      QPOWOC(IPWC) = 0.

C-----Debugging output. Can optionally be commented out.
      Debug_me = .FALSE.
      IF ( Debug_me ) THEN
         write (*,*) '-Subroutine: POWOC_CETC_HVAC_ld start'
         write (*,*) ' Calculating Idealized, hvac-driven loads'
         write (*,*) ' '
         write (*,*) '  HRV fan(s)              ', FCctl_HRV_draw
         write (*,*) ' +AC compressor           ', FCctl_AC_compressor
         write (*,*) ' +AC circulation fan      ', FCctl_AC_circfan
         write (*,*) ' +Furnace draft fan       ', FCctl_furnace_draft
         write (*,*) ' +Furnace circulation fan ', FCctl_furnace_circfan
         write (*,*) ' +Boiler draft fan        ', FCctl_boiler_draft
         write (*,*) ' +Boiler circulation fan  ', FCctl_boiler_circfan
         write (*,*) ' +ASHP compressor         ', FCctl_ashp_compressor
         write (*,*) ' +ASHP cicrulation fan    ', FCctl_ashp_circfan
         write (*,*) ' +GSHP pump               ', FCctl_gshp_pump
         write (*,*) ' +FC Parasitic loss       ', FCctl_gshp_pump
         write (*,*) ' ------------------------------------------------'
         write (*,*) ' +TOTAL HVAC load         ', PPOWOC(IPWC)
         write (*,*) ' '
         write (*,*) '-Subroutine: POWOC_CETC_HVAC_ld finish'
      ENDIF


      RETURN
      END

C *********************************************************************************************
C ********************************* POWOC_OUTSIDE_ld *******************************************
C Created by: Kamel Haddad, based largely on subroutine POWOC_nonHVACld by Ian Beausoleil-Morrison
C Initial Creation Date: March 11th 2004

C This subroutine determines the electrical load from the outside such as when there is a cluster
C of houses served by the same electrical network situated in a specific building. It is
C employed to impose ( HVAC + non HVAC) electrical loads upon the electric power flow
C modelling domain from these outside sources. These loads are accounted for in the
C electrical domain, but they do not contribute any internal heat gains to the building
C because they occur outside of the building.
C In the first development of fuel-cell cogeneration models, the non-HVAC electrical demand was
C determined within the plant solution domain. This subroutine was added to allow this load to
C be applied within the electric power flow solution domain. This subroutine is called each time-step
C (perhaps multiple times per time-step as the solver may iterate between the plant and electrical
C domains within a time-step) to establish the non-HVAC load that is stored in a binary file.
C This binary file is generated by subroutine FC_eloads_convert from an ASCII input file (.fcl file).

C INPUTS:
C    IPWC           The index of the power-only component under consideration
C                   (e.g. for a network with 2 power-only components, IPWC will
C                   be either 1 or 2).
C    POWCDAT(i,j)   The j'th data item associated with the i'th power-only component
C                   in the electrical network. [from common/POWC]
C    idyp           The day number of the present day. [from common/SIMTIM]
C    btimep         The present time in the building thermal domain. [from common/btime]
C    ptimef         The future time in the plant domain. [from common/ptime]
C    NSINC          The simulation increment counter. [from common/SIMTIM]

C OUTPUTS:
C    PPOWOC(i)      The real power demand of the HVAC + non HVAC  electrical loads (W).
C                   [from common/ENETPOWC]
C    QPOWOC(i)      The reactive power demand of the HVAC + non HVAC electrical loads (W).
C                   [from common/ENETPOWC]

C --------------------------------------------------------------------------------
      SUBROUTINE POWOC_OUTSIDE_ld(IPWC)
      implicit none

#include "building.h"
#include "plant.h"
#include "power.h"

      common/outin/iuout,iuin

C---------------------------------------------------------------------------------
C Commons holding simulation time-clock variables.
C---------------------------------------------------------------------------------
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      common/btime/btimep,btimef
      common/ptime/ptimep,ptimef

C---------------------------------------------------------------------------------
C Common holding name of ASCII file containing time-step data on non-HVAC loads.
C---------------------------------------------------------------------------------
      common/Fcell/iFClds,LFClds

C---------------------------------------------------------------------------------
C Common for passing error messages to calling routine (only used when running
C in dll mode).
C---------------------------------------------------------------------------------
      common/dllerr/dllsubr, dllmesg

C---------------------------------------------------------------------------------
C Common created by Neil Saldanha for customized controller with SE and lithium
C ion battery (see his thesis for more info)
C---------------------------------------------------------------------------------
      common/Res_elec_ctl/house_load


C---------------------------------------------------------------------------------
C Declarations for common block variables.
C---------------------------------------------------------------------------------
      REAL         house_load
      INTEGER      IHRP, IHRF, IDYP, IDYF, IDWP, IDWF, NSINC, ITS,
     & IUOUT,IUIN
      REAL         btimep, btimef, ptimep, ptimef
      CHARACTER    LFClds*72
      INTEGER      iFClds
      character    dllsubr*12,dllmesg*124

C---------------------------------------------------------------------------------
C Declarations for variables passed in calling statement.
C---------------------------------------------------------------------------------
      INTEGER IPWC

C---------------------------------------------------------------------------------
C Declarations for local variables.
C---------------------------------------------------------------------------------
      INTEGER   ier
      REAL      q_nonHVAC_elec, q_HVAC_elec
      LOGICAL   XST,dll
      CHARACTER OUTS*124
      LOGICAL   FCL_converted

C-------------------------------------------------------------------------------------------
C If this subroutine is being called for the first time then process the user-specified
C electrical loads data. Determine data type and convert from ASCII file to binary-format
C for direct access during simulation.
C-------------------------------------------------------------------------------------------
      IF(iFClds.eq.0) THEN
         LFClds = POWCDATS(IPWC,1)
         XST=.FALSE.
         call FINDFIL(LFClds,XST)
         if(XST)then
            iFClds=1
         else
           write(outs,'(3a)') 'Electric network loads file ',
     &                         LFClds(1:lnblnk(LFClds)),
     &                        ' was referenced but not found.'
            call isadll(dll)  ! Move this to within loop below.
            if(dll)then
               dllsubr='ersys'
               dllmesg=outs
               ier=2
               return
            else
               call edisp(iuout,outs)
               STOP ' Need elec loads data: unresolvable error.'
            endif
         endif
         CALL FC_eloads_convert
      END IF

C Power only components
      CALL FC_eloads_establish(idyp,btimep,ptimef,q_nonHVAC_elec,
     &                             q_HVAC_elec)

      PPOWOC(IPWC) = -1. * q_nonHVAC_elec * POWCDAT(IPWC,1)
      QPOWOC(IPWC) = 0.

      house_load = PPOWOC(IPWC) ! Store dwelling house load for custom controller (see common block declaration above)


      RETURN
      END


C *********************************************************************************************
C ********************************* POWOC_TRANSFORMER_loss ************************************
C     Created by: Maria Mottillo
C     Initial Creation Date: August 9, 2004.
C
C     This subroutine determines the power load on the electrical network due
C     to transformer losses. A constant loss factor is read from the electrical
C     network input file for the transformer power-only component.
C
C     INPUTS:
C     - IPWC           The index of the power-only component under consideration
C                      (e.g. for a network with 2 power-only components, IPWC will
C                      be either 1 or 2).
C     - POWCDAT(i,j)   The j'th data item associated with the i'th power-only component
C                      in the electrical network. [from common/POWC]
C
C     OUTPUTS:
C     - PPOWOC(i)      The real power demand of the load due to transformer losses (W).
C                      [from common/ENETPOWC]
C     - QPOWOC(i)      The reactive power demand of the load due to transformer losses (W).
C                      [from common/ENETPOWC]
C     - QFarmG         Power delivered to transmission grid after taking into account
C                      transformer losses (W).
C
C --------------------------------------------------------------------------------
      SUBROUTINE POWOC_TRANSFORMER_loss(IPWC)
      implicit none

#include "building.h"
#include "plant.h"
#include "power.h"

C---------------------------------------------------------------------------------
C Common holding power entering/leaving components developed for wind-generated
C hydrogen-based system model. Only QElecT (power from wind farm entering
C transformer) is used in this subroutine.
C---------------------------------------------------------------------------------
       COMMON/WINDHYD/
     & QTotalWindSupply,
     & QFarmG,
     & fWindspeedProfile,
     & fWindPowerProfile,
     & iDataPoints

      REAL      QTotalWindSupply, QFarmG
      REAL      fWindSpeedProfile(100),fWindPowerProfile(100)
      INTEGER   iDataPoints

C---------------------------------------------------------------------------------
C Declarations for variables passed in calling statement.
C---------------------------------------------------------------------------------
      INTEGER IPWC

C---------------------------------------------------------------------------------
C Declarations for local variables.
C---------------------------------------------------------------------------------
      REAL      Transformer_Eff
      REAL      Transformer_Loss

C--------------------------------------------------------------------------------
C Calculate transformer losses (W)
C--------------------------------------------------------------------------------

C transformer power-only component has one additional data item
      Transformer_Eff = POWCDAT(IPWC,1)
C QElecT contains the power produced by the wind turbines (input by user)
      Transformer_Loss = QTotalWindSupply * (1.0 - Transformer_Eff)
C calculate power delivered to the transmission grid, QFarmG
      QFarmG = Transformer_Eff * QTotalWindSupply


      PPOWOC(IPWC) = -1 * Transformer_Loss   ! <- note: loads are (-)ive...
      QPOWOC(IPWC) = 0.

      RETURN
      END

C *********************************************************************************************
C ********************************* POWOC_TxDx_loss ************************************
C     Created by: Maria Mottillo
C     Initial Creation Date: August 9, 2004.
C
C     This subroutine determines the power load on the electrical network due
C     to transmission and distribution system losses. A constant loss factor is
C     read from the electrical  network input file for the transmission and
C     distribution power-only component.
C
C     INPUTS:
C     - IPWC           The index of the power-only component under consideration
C                      (e.g. for a network with 2 power-only components, IPWC will
C                      be either 1 or 2).
C     - POWCDAT(i,j)   The j'th data item associated with the i'th power-only component
C                      in the electrical network. [from common/POWC]
C
C     OUTPUTS:
C     - PPOWOC(i)      The real power demand of the load due to transmission and
C                      distribution losses (W).
C                      [from common/ENETPOWC]
C     - QPOWOC(i)      The reactive power demand of the load due to transmission
C                      and distribution losses (W).
C                      [from common/ENETPOWC]
C     - QElBldg        Power delivered to building site after taking into account
C                      transformer, transmission and distribution losses (W).
C
C --------------------------------------------------------------------------------
      SUBROUTINE POWOC_TxDx_loss(IPWC)
      implicit none

#include "building.h"
#include "plant.h"
#include "power.h"

C---------------------------------------------------------------------------------
C Common holding power entering/leaving components developed for wind-generated
C hydrogen-based system model. Only QFarmG (power delivered to transmission
C grid) is used in this subroutine.
C---------------------------------------------------------------------------------
       COMMON/WINDHYD/
     & QTotalWindSupply,
     & QFarmG,
     & fWindspeedProfile,
     & fWindPowerProfile,
     & iDataPoints

      REAL      QTotalWindSupply, QFarmG
      REAL      fWindSpeedProfile(100),fWindPowerProfile(100)
      INTEGER   iDataPoints

C---------------------------------------------------------------------------------
C Declarations for variables passed in calling statement.
C---------------------------------------------------------------------------------
      INTEGER IPWC

C---------------------------------------------------------------------------------
C Declarations for local variables.
C---------------------------------------------------------------------------------
      REAL      TxDx_Eff
      REAL      TxDx_Loss

C--------------------------------------------------------------------------------
C Calculate transmission and distribution losses (W)
C--------------------------------------------------------------------------------

C transmission and distribution  power-only component has one additional
C data item
      TxDx_Eff = POWCDAT(IPWC,1)
C QFarmG contains the power delivered to the transmission grid
      TxDx_Loss = QFarmG * (1.0 - TxDx_Eff)
C Calculate the power delivered to the building site, QElBldg
C      QElBldg = QFarmG * TxDx_Eff

      PPOWOC(IPWC) = -1 * TxDx_Loss   ! <- note: loads are (-)ive...
      QPOWOC(IPWC) = 0.

      RETURN
      END


C *********************************************************************************************
C ********************************* POWOC_Bldg_PCU *******************************************
C     Created by: Maria Mottillo
C     Initial Creation Date: August 11, 2004.
C
C     This subroutine determines the power load on the electrical network
C     due to losses from the power conditioning unit that converts DC power
C     to AC power to meet the buiding electrical loads.
C
C     INPUTS:
C     - IPWC           The index of the power-only component under consideration
C                      (e.g. for a network with 2 power-only components, IPWC will
C                      be either 1 or 2).
C     - POWCDAT(i,j)   The j'th data item associated with the i'th power-only component
C                      in the electrical network. [from common/POWC]
C
C     OUTPUTS:
C     - PPOWOC(i)      The real power demand of the load due to transmission and
C                      distribution losses (W).
C                      [from common/ENETPOWC]
C     - QPOWOC(i)      The reactive power demand of the load due to transmission
C                      and distribution losses (W).
C                      [from common/ENETPOWC]
C
C --------------------------------------------------------------------------------
      SUBROUTINE POWOC_Bldg_PCU(IPWC)
      implicit none

#include "building.h"
#include "plant.h"
#include "power.h"

C---------------------------------------------------------------------------------
C Common holding the electrical power generated by the electrical generation
C device (plant hybrid component) to which the PCU is connected. Only pwrp(mpcom)
C is used in this subroutine.
C---------------------------------------------------------------------------------
      common/elpcp/npel, pfp, ipfp, pwrp, bvoltp, iphp
      integer npel              ! not used
      real pfp(mpcom)           ! not used
      integer ipfp(mpcom)       ! not used
      real pwrp(mpcom)          ! real power consumption of plant component
      real bvoltp(mpcom)        ! not used
      integer iphp(mpcom)       ! not used

C---------------------------------------------------------------------------------
C Declarations for variables passed in calling statement.
C---------------------------------------------------------------------------------
      INTEGER IPWC

C---------------------------------------------------------------------------------
C Declarations for local variables.
C---------------------------------------------------------------------------------
      INTEGER   iElecGen_ID
      REAL      QElecGen
      REAL      PCU_Eff
      REAL      PCU_Loss

C--------------------------------------------------------------------------------
C Calculate power conditioning unit losses (W)
C--------------------------------------------------------------------------------

C the PCU  power-only component has two additional data items
C The first data item is the efficiency of the PCU
      PCU_Eff = POWCDAT(IPWC,1)
C The second data item is the ID of the plant component (electrical
C generation device) to which the PCU component is connected.
      iElecGen_ID = POWCDAT(IPWC,2)
C Determine DC electrical power output of plant component to which
C the PCU is connected.
      QElecGen = pwrp(iElecGen_ID)

C Determine losses from PCU
      PCU_Loss = QElecGen * (1.0 - PCU_Eff)


      PPOWOC(IPWC) = -1 * PCU_Loss   ! <- note: loads are (-)ive...
      QPOWOC(IPWC) = 0.

      RETURN
      END


C *********************************************************************************************
C ********************************* POWOC_Wind_Supp_Elec **************************************
C Created by: Maria Mottillo
C Initial Creation Date: August 27, 2004.

C This subroutine determines the electrical power supplied by a wind farm.
C It is employed to impose the supply upon the electric
C power flow modelling domain.   This subroutine is called each time-step
C (perhaps multiple times per time-step as the solver may iterate between the plant
C and electrical domains within a time-step) to establish the power supply that is
C stored in a binary file.  This binary file is generated by subroutine H2_wind_convert
C from an ASCII input file (.wnd file).

C This subroutine has been adapted from the subroutine POWOC_nonHVACld created by
C Ian Beausoleil-Morrison.

C INPUTS:
C    IPWC           The index of the power-only component under consideration
C                   (e.g. for a network with 2 power-only components, IPWC will
C                   be either 1 or 2).
C    POWCDAT(i,j)   The j'th data item associated with the i'th power-only component
C                   in the electrical network. [from common/POWC]
C    idyp           The day number of the present day. [from common/SIMTIM]
C    btimep         The present time in the building thermal domain. [from common/btime]
C    ptimef         The future time in the plant domain. [from common/ptime]
C    NSINC          The simulation increment counter. [from common/SIMTIM]


C OUTPUTS:
C    PPOWOC(i)      The real power demand of the non-HVAC electrical loads (W).
C                   [from common/ENETPOWC]
C    QPOWOC(i)      The reactive power demand of the non-HVAC electrical loads (W).
C                   [from common/ENETPOWC]
C    QTotalWindSupply  The power supplied from the wind farm (W).

C --------------------------------------------------------------------------------
      SUBROUTINE POWOC_Wind_Supp_Elec(IPWC)
      implicit none

#include "building.h"
#include "plant.h"
#include "power.h"

      common/OUTIN/IUOUT,IUIN

C---------------------------------------------------------------------------------
C Commons holding simulation time-clock variables.
C---------------------------------------------------------------------------------
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      common/btime/btimep,btimef
      common/ptime/ptimep,ptimef

C---------------------------------------------------------------------------------
C Common holding name of ASCII file containing wind-generated electricity
C time-step data.
C---------------------------------------------------------------------------------
      common/H2wind/LWndSupp,iWndSupp

C---------------------------------------------------------------------------------
C Common holding power entering/leaving components developed for wind-generated
C hydrogen-based system model.
C---------------------------------------------------------------------------------
       COMMON/WINDHYD/
     & QTotalWindSupply,
     & QFarmG,
     & fWindspeedProfile,
     & fWindPowerProfile,
     & iDataPoints

      REAL      QTotalWindSupply, QFarmG
      REAL      fWindSpeedProfile(100),fWindPowerProfile(100)
      INTEGER   iDataPoints

C---------------------------------------------------------------------------------
C Common for passing error messages to calling routine (only used when running
C in dll mode).
C---------------------------------------------------------------------------------
      common/dllerr/dllsubr, dllmesg

C---------------------------------------------------------------------------------
C Declarations for common block variables.
C---------------------------------------------------------------------------------
      INTEGER      IHRP, IHRF, IDYP, IDYF, IDWP, IDWF, NSINC, ITS,
     & IUOUT,IUIN
      REAL         btimep, btimef, ptimep, ptimef
      CHARACTER    LWndSupp*72
      INTEGER      iWndSupp
      character    dllsubr*12,dllmesg*124
      REAL         QElecT

C---------------------------------------------------------------------------------
C Declarations for variables passed in calling statement.
C---------------------------------------------------------------------------------
      INTEGER IPWC

C---------------------------------------------------------------------------------
C Declarations for local variables.
C---------------------------------------------------------------------------------
      INTEGER   ier
      LOGICAL   XST,dll
      CHARACTER OUTS*124


C-------------------------------------------------------------------------------------------
C If this subroutine is being called for the first time then process the user-specified
C electrical supply data. Determine data type and convert from ASCII file to binary-format
C for direct access during simulation.
C-------------------------------------------------------------------------------------------
      IF(iWndSupp==0) THEN
C POWCDATS(IPWC,1) holds the .wnd. file name (additional data string).
         LWndSupp = POWCDATS(IPWC,1)
         XST=.FALSE.
         call FINDFIL(LWndSupp,XST)
         if(XST)then
            iWndSupp=1
         else
           write(outs,'(3a)') 'WND supply file ',LWndSupp
     &       (1:lnblnk(LWndSupp)),' was referenced but not found.'
            call isadll(dll)  ! Move this to within loop below.
            if(dll)then
               dllsubr='ersys'
               dllmesg=outs
               ier=2
               return
            else
               call edisp(iuout,outs)
               STOP ' Need elec supply data: unresolvable error.'
            endif
         endif
         CALL H2_wind_convert
      END IF


C-------------------------------------------------------------------------------------------
C Determine the electrical supply for the current time-step.
C-------------------------------------------------------------------------------------------
      CALL H2_wind_establish(idyp,btimep,ptimef,QElecT)

C  Take into account scalar multiplier applied to wind supply
C  POWCDAT(IPWC,1) holds the scalar multiplier (additional data item).

      QTotalWindSupply = QElecT*POWCDAT(IPWC,1)
      PPOWOC(IPWC) = QTotalWindSupply
      QPOWOC(IPWC) = 0.


      RETURN
      END


C *********************************************************************************************
C ********************************* POWOC_PCU *************************************************
C     Created by: Maria Mottillo
C     Initial Creation Date: March 7, 2005.
C
C     This subroutine treats a power conditioning unit as a power-only component. The subroutine
C     determines the power sink term for the node sending current to the PCU and the power source
C     term for the node receiving current from the PCU. The model is based on the model of a power
C     conditioning unit developed by (Ulleberg, 1998) and implemented in TRNSYS.The power
C     conditioning unit can represent a DC/DC converter, AC/DC rectifier or DC/AC inverter.
C
C
C     INPUTS:
C     - IPWC           The index of the power-only component under consideration
C                      (e.g. for a network with 2 power-only components, IPWC will
C                      be either 1 or 2).
C     - POWCDAT(i,j)   The j'th data item associated with the i'th power-only component
C                      in the electrical network. [from common/POWC]
C
C     OUTPUTS:
C
C     PENODL(i)        Real power load on node 'i'; i is the node sending current to the PCU
C     PENODG(i)        Real power generation on node 'i'; i is the node receiving current from the PCU
C --------------------------------------------------------------------------------
      SUBROUTINE POWOC_PCU(IPWC)
      use h3kmodule
      implicit none
#include "plant.h"
#include "building.h"
#include "power.h"
#include "CETC_definitions.h"

C External functions.
      integer lnblnk

C----------------------------------------------------------------------------------------------------
C ESP-r COMMONs
C----------------------------------------------------------------------------------------------------

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      integer itcf           !- building side increment to end trace output
      integer itrace         !- trace call index for subroutine
      integer izntrc         !- zone trace index
      integer itu            !- trace output unit number

      common/pcu/pcunode(mpowcom,3)
      integer pcunode         !- array holds the nodes to which the pcu is connected
                             !- pcunode(i,1) is the node id of the sending node
                             !- pcunode(i,2) is the node id of the receiving node
                             !- i is the power-only component index


C----------------------------------------------------------------------------------------------------
C Local variables
C----------------------------------------------------------------------------------------------------


C..... Operating mode of power conditioning unit:
C..... 1 = connected to power source (input power known)
C..... 2 = connected to power load (output power known)
C......Variants using simple efficiency calculation:
C..... 3 = connected to power source (input power known)
C..... 4 = connected to power load (output power known)

      integer iOpMode

      integer iConnPwrSource, iConnPwrLoad, iConnPwrSourceSimple,
     &iConnPwrLoadSimple
      parameter( iConnPwrSource = 1,
     &           iConnPwrLoad   = 2,
     &           iConnPwrSourceSimple = 3,
     &           iConnPwrLoadSimple   = 4  )

C.....Nominal power (W)
      real    fNomPower

C.....Idling constant (-)
C.....The idling constant is the ratio of the power loss when
C.....there is no voltage across the PCU and the nominal power
      real    fIdleConst

C.....Setpoint voltage (V)
      real    fUsetp

C.....Voltage input (V)
      real    fUin

C.....Voltage output (V)
      real    fUout

C.....Internal resistance constant (V^2)
C.....The internal resistance constant is the product of the
C.....internal resistance (ohms) and the nominal power (W)
      real    fIntResConst

C.....Power loss from the power conditioning unit (W)
      real    fPwrLoss

C.....Power input to the power conditioning unit (W)
      real    fPwrInput

C.....Power output from the power conditioning unit (W)
      real    fPwrOutput

C.....Efficiency of power conditioning unit (-)
      real    fPCUEff

C.....Specified efficiency for simple calculation (-)
      real    fSpecPCUEfficiency

C.....Auxiliary power required to operate power conditioning unit (W)
C.....This input is not used in the current version of the model
      real    fPwrAux

C.....Variables used for solution of power output from PCU
C.....when power input to PCU is known (i.e. PCU is connected to
C.....power source, iOpMode = 1)
      real    fVarA, fVarB, fVarC, fXSolve

C.....Variable used to compare two real numbers
      logical bPower_Zero

C.....Index of the next power-only component defined in network
      integer next_comp

C.....Counter
      integer i

C.....Boolean to flag error
      logical ok

C.....Database index number for PCU power-only-component
      integer PCU_id
      parameter( PCU_id = 20 )

      INTEGER iNameLength  !Used to capture variable name's length (Reporting)


C----------------------------------------------------------------------------------------------------
C Passed variables
C----------------------------------------------------------------------------------------------------

C.....Power-only component index
      integer IPWC

C----------------------------------------------------------------------------------------------------


C Ensure that PCU power-only component models are the last defined.
C This is necessary to get correct power balance at nodes to which the PCU
C is connected.
      ok = .true.
      next_comp = IPWC + 1
      do 10 i = next_comp, npowcom
         if ( powcomid(i) .ne. PCU_id ) then
            ok = .false.
         endif
 10   continue

      if ( .not. ok ) then
          write(ITU,*) 'POWOC_PCU(powoc_calc.F):'
          write(ITU,*) 'PCUs must be last power-only components '
          write(ITU,*) 'to be defined.'
          stop 'powoc_calc.F: Fatal error!'
      endif


C Inputs from electric network file

C There are 6 data items for the PCU power-only component; See full definitions in the subroutine header.


C.....Operating mode of PCU
      iOpMode = POWCDAT(IPWC,1)

C.....Nominal power of PCU (W)
      fNomPower = POWCDAT(IPWC,2)

C.....PCU Idling constant (-)
      fIdleConst = POWCDAT(IPWC,3)

      if (iOpMode == iConnPwrSource .or.
     &    iOpMode == iConnPwrLoad         ) then


C.......PCU setpoint voltage (V)
        fUsetp = POWCDAT(IPWC,4)

C.......PCU Internal resistance constant (V^2)
        fIntResConst = POWCDAT(IPWC,5)

C.......Auxiliary power required by PCU (W)
C... .. This input is currently not taken into account.
        fPwrAux = POWCDAT(IPWC,6)

C.....Inputs for simple variant using consant efficiency
      elseif ( iOpMode == iConnPwrLoadSimple .or.
     &         iOPMode == iConnPwrSourceSimple      ) then

C.......Nominal efficiency
        fSpecPCUEfficiency = POWCDAT(IPWC,4)

      endif

C.....Obtain voltages from connected nodes (take real part)

C sending node
      fUin = real(enodvlt(pcunode(IPWC,1)))

C receiving node
      fUout = real(enodvlt(pcunode(IPWC,2)))


C.....Power conditioning unit connected to power load (power output is known):

      if ( iOpMode .eq. iConnPwrLoad .or.
     &     iOpMode .eq. iConnPwrLoadSimple ) then

C get net power at receiving  node
          fPwrOutput = ABS(penodl(pcunode(IPWC,2)) +
     &                 penodg(pcunode(IPWC,2)))

          call eclose( fPwrOutput, 0.,0.001,bPower_Zero )

C no power output
          if ( bPower_Zero ) then

             fPwrInput = fIdleConst * fNomPower

          elseif (  iOpMode .eq. iConnPwrLoad  ) then

C Calculate power input based on relationship given by (Ulleberg,1998),
C Section 4.10.2, page 120, equation (4.95)
C Note- there is a (typing) error in equation (4.95) - corrected in
C TRNSYS type 175 (PCU) description and subroutine

             fPwrInput = fNomPower * (( fIdleConst +
     &                 ( 1. + fUsetp/fUout ) * fPwrOutput / fNomPower )
     &                 + ( fIntResConst * ( 1./ (fUout**2) ) *
     &                  ( fPwrOutput / fNomPower )**2 ))

          elseif ( iOpMode .eq. iConnPwrLoadSimple ) then

            fPwrInput = fPwrOutput / fSpecPCUEfficiency

          endif


C Power conditioning unit connected to power source (power input is known):

      elseif ( iOpMode .eq. iConnPwrSource .or.
     &         iOpMode .eq. iConnPwrSourceSimple ) then

C get net power at sending node
          fPwrInput =  penodl(pcunode(IPWC,1)) +
     &                 penodg(pcunode(IPWC,1))


          if ( fPwrInput .lt. 0. ) then
             fPwrInput = 0.
          endif


          call eclose( fPwrInput, 0.,0.001,bPower_Zero )


          if ( bPower_Zero ) then

C no power input, calculate idling losses
             fPwrOutput = -1. * fIdleConst * fNomPower      ! losses are a load, -ve

          elseif ( iOpMode .eq. iConnPwrSource ) then

C Calculate power output based on relationship given by (Ulleberg,1998).
C The 2nd order equation is solved by finding the roots of the quadratic equation
C and only keeping the positive solution.

C  Quadratic equation: y = a * x^2 + b * x + c = 0

C  x  = fPwrOutput/fNomPower

             fVarA = fIntResConst / ( fUout**2 )

             fVarB = 1. + ( fUsetp / fUout )

             fVarC = fIdleConst - ( fPwrInput / fNomPower )

             fXSolve = ( -1. * fVarB + SQRT( fVarB**2 -
     &                  4. * fVarA * fVarC ) ) /
     &                  ( 2. * fVarA )


             fPwrOutput = fXSolve * fNomPower

             if ( fPwrOutput .gt. fPwrInput ) then    ! Ensures that PCU efficiency is not > 100%
                                                      ! and that power losses are not negative!
               fPwrOutput = fPwrInput

             endif

          elseif ( iOpMode .eq. iConnPwrSourceSimple ) then
            fPwrOutput = fSpecPCUEfficiency * fPwrInput

          endif

      else

C........error message goes here


      endif



C Calculate power loss

          fPwrLoss = fPwrInput - fPwrOutput

C Calculate efficiency (only applicable if PCU is operating)

          if ( bPower_Zero ) then

             fPCUEff = 0.            !- to avoid divide by zero error in controller subroutine.

          else

             fPCUEff = fPwrOutput / fPwrInput

          endif

C Place load/generated power on appropriate node:


C power load on sending node (only real power taken into account)
C loads are negative
          penodl(pcunode(IPWC,1)) =  penodl(pcunode(IPWC,1)) -
     &                               fPwrInput


C power source on receiving node (only real power taken into account)
          penodg(pcunode(IPWC,2)) =  penodg(pcunode(IPWC,2)) +
     &                               fPwrOutput


C Save variables that may be required by control sensors.
          iPow_Time_Row_Count(IPWC)    = 3           ! # of additional data.
          fPow_Time_Row_Future(IPWC,1) = fPwrOutput  ! Power output (i.e. generation at receiving node)
          fPow_Time_Row_Future(IPWC,2) = fPCUEff     ! Efficiency.
          fPow_Time_Row_Future(IPWC,3) = fPwrLoss    ! Power losses of PCU


C set power generated/consumed by PCU to zero (taken into account directly in PENODG and PENODL arrays)

          ppowoc(IPWC) = 0.
          qpowoc(IPWC) = 0.


C-------------------------------------------
C XML Output
C-------------------------------------------


C.... Get component name's length
      iNameLength = lnblnk(powcomnam(IPWC))

C.....Format string as 'electrical_net/power_only_components/NAME/misc_data'
      Call AddToReport(rvEPowPCUPowLoss%Identifier,
     &      fPwrLoss,
     &      powcomnam(IPWC)(1:iNameLength))

C.....Only report efficiency if unit is on
      if ( .not. bPower_Zero ) then

      Call AddToReport(rvEPowPCUEff%Identifier,
     &      fPCUEff,
     &      powcomnam(IPWC)(1:iNameLength))

      endif

      Call AddToReport(rvEPowPCUPowIn%Identifier,
     &      fPwrInput,
     &      powcomnam(IPWC)(1:iNameLength))

      Call AddToReport(rvEPowPCUPowOut%Identifier,
     &      fPwrOutput,
     &      powcomnam(IPWC)(1:iNameLength))

          RETURN
          END


C------------------------------------------------------------------------------




C *********************************************************************************************
C ********************************* POWOC_nonHVACld_BCD ****************************************
C Created by: Alex Ferguson
C Initial Creation Date: July 8, 2005

C This subroutine determines the electrical load demanded by the non-HVAC power-only
C component.  It is employed to impose non-HVAC electrical loads upon the electric
C power flow modelling domain. This routine differs from the POWOC_nonHVACld routine
C in that:
C   - it supports reading loads from the boundary condition definition facility,
C     and can thus impose multiple loads on the network,
C   - it can determine the amount of hydrogen used by hydrogen appliances, and
C     deduct an equivlent amount of electricity from the occupant-driven
C     electric load profiles.

C INPUTS:
C    iComponent      Index of comonent in the electrical network.

C OUTPUTS:
C    PPOWOC(i)      The real power demand of the non-HVAC electrical loads (W).
C                   [from common/ENETPOWC]
C    QPOWOC(i)      The reactive power demand of the non-HVAC electrical loads (W).
C                   [from common/ENETPOWC]

C --------------------------------------------------------------------------------
      SUBROUTINE POWOC_nonHVACld_BCD(iComponent)
      use h3kmodule
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "Hydrogen_demand_controller.h"
#include "OffsiteUtilitiesPublic.h"

C---------------------------------------------------------------------------------
C     Passed variables
C---------------------------------------------------------------------------------
      integer iComponent                  ! Component index

C---------------------------------------------------------------------------------
C Trace & reporting data
C---------------------------------------------------------------------------------

      common/outin/iuout,iuin
      integer iuout,iuin

C---------------------------------------------------------------------------------
C Common holding # of plant components
C---------------------------------------------------------------------------------
      common/c9/npcomp,nci,cdata
      integer npcomp,nci(mpcom)
      real cdata(mpcom,mmiscd)

C---------------------------------------------------------------------------------
C Local variables
C---------------------------------------------------------------------------------
      real fDay                           ! real representing day of year
                                          !   (& fraction thereof)
      real fE_Load                        ! Electric loads (W)
      real fE_Load_profile                ! Electric load from original profile (W)
      character*248 cName_buffer          ! Buffer for passing BCD name
      character*72  cFCL_name             ! Buffer for passing .fcl name
      character*124 cContext              ! Contextual buffer
      character*124 cMsg
      integer iInterp_Method              ! Flag indicating which method
                                          !  used for interpolation

      integer iElec_Load_source           ! Flag indicating if load should be
                                          !   obtained from .fcl file or
                                          !   boundary condition facility

      real fScale_factor                ! Load scaling factor
      integer iH2_load_strategy           ! Flag indicating if H2 loads should
                                          !   be ignored (0) or deducted (1)

      integer iCount                      ! counter
      real fH2_app_load_TOTAL             ! Total hydrogen appliance load (W)

      logical bClose                      ! flag for numerical comparisons




C.....Named constants
      integer iIgnore_App_loads           ! Flag for ignoring hydrogen loads
      integer iDeduct_App_loads           ! Flag for deducting hydrogen loads
      parameter ( iIgnore_App_loads = 0,
     &            iDeduct_App_loads = 1 )

      integer iSRC_FClds                  ! Obtain loads from FCL file
      integer iSRC_BCD                    ! Obtain loads from boundary conditions
      parameter ( iSRC_FClds = 1,
     &            iSRC_BCD   = 2 )

      logical bWarned
      save bWarned
      data bWarned /.false./

C---------------------------------------------------------------------------------
C     References
C---------------------------------------------------------------------------------
      real fConvert_current_TS_to_DAY     ! Function returning current day #
      real fGet_BC_data_by_name           ! Function returning BC value
      real fGet_nonHVACld                 ! Function returing loads from .fcl file

C.....Get initial data
      fScale_factor     = powcdat(iComponent,1)
      iInterp_Method    = int ( powcdat(iComponent,2) )
      iH2_load_strategy = int ( powcdat(iComponent,3) )
      iElec_Load_source = int ( powcdat(iComponent,4) )


C.....Write contextual buffer
      write (cContext, '(A,A,I2,A)' ) 'Collecting boundary condition ',
     &  'for occupant-driven electric loads (pfs component ',
     &  iComponent,')'

      if ( iElec_Load_source .eq. iSRC_FClds ) then
C---------------------------------------------------------------------------------
C        Use .fcl file as load source
C---------------------------------------------------------------------------------
C........Save component name as string
         write (cFCL_name, '(A)') powcdats(iComponent,1)
         fE_Load = fGet_nonHVACld ( cFCL_name )

      else
C----------------------------------------------------------------------------------
C        Use boundary condition as load source
C---------------------------------------------------------------------------------
C........Save component name as string
         write (cName_buffer, '(A)') powcdats(iComponent,1)

C........Get day
         fDay = fConvert_current_TS_to_DAY()


C........Get electric loads
         fE_Load =  fGet_BC_data_by_name ( cName_buffer,
     &                                     fDay,
     &                                     iInterp_Method,
     &                                     cContext )

      endif

C....... Multiply electric load by scale factor

         fE_Load = fE_load * fScale_Factor


C Initialise
         fH2_app_load_TOTAL = 0.0


C........Get appliance loads
         if ( iH2_load_strategy .eq. iDeduct_App_loads ) then
            do iCount=1, npcomp
               fH2_app_load_TOTAL = fH2_app_load_TOTAL +
     &                              fH2_plt_app_load (iCount)
            enddo
         endif

         fE_Load_profile = fE_Load                    ! Save value for XML reporting

         fE_Load = fE_Load - fH2_app_load_TOTAL


         if ( iH2_load_strategy .eq. iDeduct_App_loads ) then
C........Is load less than zero?
            call eClose(fE_Load, 0.0, 0.01, bClose)

            if ( fE_load .lt. 0.0 .and. .not. bClose ) then
               fE_load = 0.0
               if ( .not. bWarned ) then
C................Issue warning to user.
                 write (cMsg, '(A,A)') 'Warning: hydrogen appliance ',
     &                 'load exceeds non-hvac electric loads.'
                 call edisp (iuout, cContext)
                 call edisp (iuout, cMsg )

                 bWarned = .true.

               endif
            endif
         endif


      PPOWOC(iComponent) = -1.0 * fE_Load
      QPOWOC(iComponent) = 0.


C-------------------------------------------------------
C XML output
C-------------------------------------------------------
      Call AddToReport(rvEPowLoadProf%Identifier,
     &      fE_Load_profile)

      Call AddToReport(rvEPowH2Load%Identifier,
     &      fH2_app_load_TOTAL)

      Call AddToReport(rvEPowLoadAdj%Identifier,
     &      fE_Load)


      RETURN
      END


