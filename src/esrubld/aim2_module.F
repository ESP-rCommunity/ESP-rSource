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

C This file contains the module AIM2 which contains the data for
C the following routines related to the AIM-2 infiltration model.
C These routines are invoked prior to bps commencing its time-step simulation:

C   AIM2_H2KDefaults	    Initializes generic house data 
C   AIM2_READIN             Reads the data contained in the .aim file.
C   AIM2_SETVARS            Controls calc of time-invariant data for AIM-2 model.
C   AIM2_HOUSE_VOLUME       Calculates house volume.
C   AIM2_FLUEHGT            Sets height of flue.
C   AIM2_LEAKDIST           Sets leakage distribution fractions.
C   AIM2_SETJ               Determines foundation type (necessary to look up leakage coeffs).
C   AIM2_COandN             Calculates fabric leakage coefficient and flow exponent.
C   AIM2_flue_size          Sets effective opening size of furnace flue.
C   AIM2_shelter_wall_flue  Sets wall and flue shelter coefficients.
C   AIM2_wind_correct       Calculates wind speed correction factor.
C   AIM2_report             Report contents of AIM-2 common blocks.

C *******************************************************************************
C References:
C
C    Beausoleil-Morrison I. (2000a), AIM-2 Implementation into ESP-r:
C    Solving for C and n for Blower Door Inputs, NRCan internal report.
C
C    Beausoleil-Morrison I. (2000b), AIM-2 Implementation into ESP-r:
C    Solving for C and n for Blower Door Inputs When User Does Not
C    Input ELA, NRCan internal report.
C
C    Bradley B. (1993), Implementation of the AIM-2 Infiltration
C    Model in HOT2000, Unies Contract Report to NRCan.
C
C    Lew L., "Evaluation of AIM-2", EMR, Apr 23/93.
C
C    Walker I.S. and Wilson D.J. (1990), The Alberta Air Infiltration
C    Model: AIM-2, University of Alberta Dept of Mech Eng, Report 71.
C
C    Wieringa, J. (1986), `Roughness-dependant Geographical Interpolation
C    of surface wind speed averages', Quart. J. R. Met. Soc, 112, 867-889.
C
C *******************************************************************************
C ********************************************************************************
C ************************ MODULE AIM2 *******************************************
C Created by: Francesca Macdonald
C Date: July 3, 2010
C 
C ********************************************************************************
      MODULE AIM2_InputData 
      
      IMPLICIT NONE
      
      SAVE !ensures values do not change between sucessive invocations

C---------------------------------------------------------------------------------
C Constant initialization.
C---------------------------------------------------------------------------------

C PI is initialized here because FORTRAN lacks an intrinsic function for PI.
        REAL, PARAMETER :: PI = 3.14156

C Gravitational acceleration (m/s^2).
        REAL, PARAMETER :: gravity = 9.81

C Interaction coefficient between wind and stack effects. Defined as
C a constant by Walker and Wilson (1993, p4) based on empirical
C observations.
        REAL, PARAMETER :: AIM2_B1 = -1./3.


C---------------------------------------------------------------------------------
C Declare parameters
C---------------------------------------------------------------------------------

c previously defined in common/aim2def/iAIM2,LAIM2

C Flag for whether aim2 AIM-2 infiltration model active.  iAIM2=1 if active
      INTEGER :: iAIM2
C Filename for aim2 file       
      CHARACTER :: LAIM2 *72

C Air tightness type as defined in HOT2000 interface:
C 1=blower door test; 2=quick blower door test;
C 3=loose; 4=average; 5=present; 6=energy tight.
      INTEGER :: airtight_type

C Three options are available for the blower-door results inputs
C Option 1: flow coefficient C0 [m^3/(s Pa^n)]
C           flow exponent n
C Option 2: Air change rate @ 50Pa
C           Pressure difference in Pa
C Option 3: Air change rate @ 50Pa
C           Pressure difference in Pa
C           Equivalent leakage area in cm^2
C           Discharge coefficient Cd used to calculate leakage area
            
      INTEGER :: blower_door_input_option

C Both flow coefficient and flow exponent can be either user inputs or
C derived variables, depending on the blower door input option.
C Since they are already declared as derived variables in
C COMMON/AIM2_CALC_DATA/, no change is made in this respect.

C Air change rate @ 50Pa. Used for blower_door_input_option equal to 2 and 3
      REAL :: ACH_50Pa

C Pressure difference in Pa for ELA_cm.
      REAL :: ELA_deltaP
        
C Flag indicating whether user input ELA: 0=no input; 1=input given.
      INTEGER :: ELA_given

C Equivalent leakage area in cm^2. Only used for blower_door_input_option equal to 3
      REAL :: ELA_cm

C Discharge coefficient. Only used for blower_door_input_option equal to 3
      REAL :: ELA_Cd
              
C Flag indicating whether user has specified leakage fractions for ceiling,
C wall, and floor: 0 indicates user input not given; 1 indicates user input given.
      INTEGER :: userspec_leakage

C Ceiling, wall, and floor leakage fractions.
      REAL :: AIM2_ceil_frac,AIM2_wall_frac,AIM2_floor_frac

C Terrain flag at weather station:
C         1=Open sea,fetch > 5 km
C         2=Mud flats,no vegetation
C         3=Open flat terrain, grass
C         4=Low crops, x/H > 20
C         5=High crops, scattered obstacles
C         6=Parkland, bushes, x/H ~ 10
C         7=Suburban, forest
C         8=City centre
      INTEGER :: AIM2_terrain_weather
      
C Terrain flag at building site. Same options as for AIM2_terrain_weather.
      INTEGER :: AIM2_terrain_building

C Flag indicating local shielding on walls:
C         1=No local shielding
C         2=Light local shielding
C         3=Heavy
C         4=Very heavy
C         5=Complete (by large buildings)
      INTEGER :: AIM2_wall_shielding

C Flag indicating local shielding on flue. Same options as for AIM2_wall_shielding.
      INTEGER :: AIM2_flue_shielding

C Height of anenometor at weather station (m).
      REAL :: AIM2_anemom_height

C Height of building eaves (m).
      REAL :: AIM2_eaves_height

C Diameter of furnace flue (mm).
      REAL :: AIM2_furnace_flue

C Diameter of #1 fireplace flue (mm).
      REAL :: AIM2_fire_flue1

C Diameter of #2 fireplace flue (mm).
      REAL :: AIM2_fire_flue2

C Diameter of primary DHW system flue (mm).
      REAL :: AIM2_DHW_flue1

C Diameter of secondary DHW system flue (mm).
      REAL :: AIM2_DHW_flue2

C Number of zone whose temperature is used as reference for infiltration calculcations.
      INTEGER :: AIM2_ref_zone

C Number of zones that receive infiltration.
      INTEGER :: AIM2_num_zones_infil

C Matrix holding indices of zones that receive infiltration.
C Dynamic memory allocation to set the size of the array, means
C a maximum does not need to be decided at this point
      INTEGER, ALLOCATABLE, DIMENSION(:) :: AIM2_zone_infil

C Zone number of basement (basements treated differently that living space in terms of
C allocation of infiltration air flow).
      INTEGER :: AIM2_basement_zone

C Zone number of Crawl Space (The volume of the crawl space is not included
c in the house volume for infiltration calculations).
      INTEGER :: AIM2_CrawlSpace_zone

C Zone number of attic. Infiltration to attics not calculated with AIM-2 model
C but rather treated with different approach.
      INTEGER :: AIM2_attic_zone

C File version (used in report functions)
      REAL :: fAIM2VersionNumber
	
C Initialization flag	
      LOGICAL :: AIM2_Initialized 

      END MODULE AIM2_InputData

C ********************************************************************************
C ********************************************************************************

      MODULE AIM2_CalcData 

      IMPLICIT NONE
      
      SAVE !ensures values do not change between sucessive invocations
C ********************************************************************************
C ************************* AIM2_CALC_DATA ***************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 19, 2000
C Copyright 2000: NRCan Buildings Group

C This module declares the variables to hold data that are calculated by the
C AIM-2 model. Variables that are calculated prior to the time-step simulation
C are treated here. Most variables that are calculated during the time-step
C simulation are passed through calling statements rather than in this module.
C The only exception is the array which stores the infiltration rate to each
C zone. That is included in this module so that its data can exist following
C the return from AIM2_CONTROL.


C The height of the flue (m).
      REAL :: AIM2_flue_height

C The normalized height of the flue. Refer to eq 20 of Wilson and Walker (1990).
      REAL :: AIM2_Betaf

C The building fabric flow coefficient.
      REAL :: AIM2_CO

C The building flow exponent (dimensionless).
      REAL :: AIM2_n

C Diameter of furnace flue when furnace operating (mm).
      REAL :: furnON_flue_diam

C Diameter of furnace flue when furnace off (mm).
      REAL :: furnOFF_flue_diam

C The wind speed correction factor.
      REAL :: AIM2_Ct

C The shelter coefficients for the walls.
      REAL :: AIM2_Swo

C The shelter coefficients for the flue.
      REAL :: AIM2_Swflue

C House volume in m^3.
      REAL :: AIM2_house_vol

C Matrix holding the infiltration rate apportioned to each zone.
C Dynamic memory allocation to set the size of the array
      REAL, ALLOCATABLE, DIMENSION(:) :: AIM2_infil_to_zones

      END MODULE AIM2_CalcData 

      
C ********************************************************************************
C ********************************************************************************

      MODULE AIM2_H2K_DUMMY 

      IMPLICIT NONE
      
      SAVE !ensures values do not change between sucessive invocations

C     ********************************************************************************
C     ************************* AIM2_H2K_DUMMY ***************************************
C     Created by: Ian Beausoleil-Morrison
C     Initial Creation Date: October 19, 2000
C     Copyright 2000: NRCan Buildings Group
        
C     This module declares and initialzes generic house data that is required by
C     the AIM-2 model. These data are "hard-wired" here for the purposes of testing
C     the AIM-2 model. As work proceeds to integrate the HOT2000 interface and the ESP-r
C     engine these data will be passed by the interface rather than initialized here.

C House type (as defined in H2K list 62):
C   1 = Single detached
C   2 = Double/Semi-detached
C   3 = Duplex
C   4 = Triplex
C   5 = Apartment unit (Individual) 
C   6 = Row house, end unit
C   7 = Mobile home
C   8 = Row house, middle unit
      INTEGER :: H2K_house_type 
        
        
C     Description of number of storeys input by user (as defined in H2K list 63):
C     1 = One storey
C     2 = One and a half
C     3 = Two storeys
C     4 = Two and a half
C     5 = Three storeys
C     6 = Split level
C     7 = Split entry / Raised basement
      INTEGER :: H2K_storeys
        
C     Number of foundations input by user.
      INTEGER ::  H2K_found_num
        
        
C     Foundation type (as defined in H2K iFndatn). The index indicates the
C     foundation number under consideration.
C     1 = N/A
C     2 = Basement
C     3 = Slab-on-grade
C     4 = Closed crawl space
C     5 = Vented crawl space
C     6 = Open crawl space
C     7 = Walkout basement
      INTEGER :: H2K_found_type(6)

        
C     Foundation depth (see H2K rFndatn). The index indicates the
C     foundation number under consideration.
      REAL H2K_found_depth(6) 
        
        
C     Foundation floor area (see H2K rFndatn). The index indicates the
C     foundation number under consideration.
      REAL :: H2K_found_area(6) 
        
        
C     Flags indicating whether "fireplace #1" and "fireplace #2" exist.
      LOGICAL :: H2K_fireplace1  
      LOGICAL :: H2K_fireplace2
        
	
C     Flags indicating presence of primary ("1" suffix) and secondary ("2" suffix)
C     DHW systems.
      LOGICAL :: H2K_DHW1_exist
      LOGICAL :: H2K_DHW2_exist
        
        
C     Flags indicating whether the primary and secondary DHW systems share the
C     flue with the furnace.
      LOGICAL :: H2K_DHW1_shareflue
      LOGICAL :: H2K_DHW2_shareflue 
        
        
C     Flags indicating whether the primary and secondary DHW systems are located
C     indoors (true) or outdoors (false).
      LOGICAL :: H2K_DHW1_inside
      LOGICAL :: H2K_DHW2_inside
       
        
C     Flag indicating whether primary DHW system is a solar collector. Interface
C     does not allow secondary system to be solar.
      LOGICAL :: H2K_DHW1_solar
	

      END MODULE AIM2_H2K_DUMMY 
C ********************************************************************************
C ********************************************************************************

      MODULE AIM2
      
      USE AIM2_InputData
      USE AIM2_CalcData
      USE AIM2_H2K_DUMMY

      IMPLICIT NONE
  
C Add Proceedures
      CONTAINS
C ********************************************************************************
C ************************ Initialize_AIM2_Params *******************************************
C Created by: Francesca Macdonald on May 4, 2010:

C This subroutine initializes the AIM2 parameters, this allows checks to be made on
C which parameters have been defined (for use in menu options).
     
      SUBROUTINE Initialize_AIM2_Params
      
      CALL AIM2_Deallocate

      airtight_type = 0      
      blower_door_input_option = 0
      ACH_50Pa = 0.
      ELA_deltaP = 0.
      ELA_given = 0
      ELA_cm = 0.
      ELA_Cd = 0.
      userspec_leakage = 0
      AIM2_ceil_frac= 0.
      AIM2_wall_frac = 0.
      AIM2_floor_frac = 0.
      AIM2_terrain_weather = 0
      AIM2_terrain_building = 0
      AIM2_wall_shielding = 0
      AIM2_flue_shielding = 0
      AIM2_anemom_height = 10.
      AIM2_eaves_height = 0.
      AIM2_furnace_flue = 0.
      AIM2_fire_flue1 = 0.
      AIM2_fire_flue2 = 0.
      AIM2_DHW_flue1 = 0.
      AIM2_DHW_flue2 = 0.
      AIM2_ref_zone = 0
      AIM2_num_zones_infil = 0
      AIM2_basement_zone = 0
      AIM2_CrawlSpace_zone = 0
      AIM2_attic_zone = 0
      fAIM2VersionNumber = -1.
      AIM2_Initialized = .TRUE.
      
      END SUBROUTINE Initialize_AIM2_Params

C ********************************************************************************
C ************************ InitializeH2KDefaults *******************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: July 31, 2000
C Copyright 2000: NRCan Buildings Group

C Modified by Francesca Macdonald on May 4, 2010:
C   - Seperated this into a subroutine for use with the new interface 

C This subroutine declares and initialzes generic house data that is required by
C the AIM-2 model. These data are "hard-wired" here for the purposes of testing
C the AIM-2 model. As work proceeds to integrate the HOT2000 interface and the ESP-r
C engine these data will be passed by the interface rather than initialized here.
     
      SUBROUTINE AIM2_H2KDefaults
      
      IMPLICIT NONE
   

C House type (as defined in H2K list 62):
C   1 = Single detached
C   2 = Double/Semi-detached
C   3 = Duplex
C   4 = Triplex
C   5 = Apartment unit (Individual)
C   6 = Row house, end unit
C   7 = Mobile home
C   8 = Row house, middle unit
         
      H2K_house_type = 1

C Description of number of storeys input by user (as defined in H2K list 63):
C   1 = One storey
C   2 = One and a half
C   3 = Two storeys
C   4 = Two and a half
C   5 = Three storeys
C   6 = Split level
C   7 = Split entry / Raised basement
      H2K_storeys = 1

C Number of foundations input by user.
      H2K_found_num = 1

C Foundation type (as defined in H2K iFndatn). The index indicates the
C foundation number under consideration.
C   1 = N/A
C   2 = Basement
C   3 = Slab-on-grade
C   4 = Closed crawl space
C   5 = Vented crawl space
C   6 = Open crawl space
C   7 = Walkout basement
      H2K_found_type(1) = 3


C Foundation depth (see H2K rFndatn). The index indicates the
C foundation number under consideration.
      H2K_found_depth(1) = 0.0


C Foundation floor area (see H2K rFndatn). The index indicates the
C foundation number under consideration.
      H2K_found_area(1) = 80.0
         

C Flags indicating whether "fireplace #1" and "fireplace #2" exist.
 
      H2K_fireplace1= .false.
      H2K_fireplace2 = .false.

C Flags indicating presence of primary ("1" suffix) and secondary ("2" suffix)
C DHW systems.

      H2K_DHW1_exist = .true.
      H2K_DHW2_exist = .false.

C Flags indicating whether the primary and secondary DHW systems share the
C flue with the furnace.
        
      H2K_DHW1_shareflue = .true. 
      H2K_DHW2_shareflue = .true.

C Flags indicating whether the primary and secondary DHW systems are located
C indoors (true) or outdoors (false).
       
      H2K_DHW1_inside = .true.
      H2K_DHW2_inside = .true.

C Flag indicating whether primary DHW system is a solar collector. Interface
C does not allow secondary system to be solar.
      H2K_DHW1_solar = .false. 

C **END H2K Defaults. 

      END SUBROUTINE AIM2_H2KDefaults
      
C ********************************************************************************
C ************************ AIM2_READIN *******************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: July 31, 2000
C Copyright 2000: NRCan Buildings Group

C Modified by Stephanie Mombourquette on May 3, 2004:
C   - Added IMPLICIT NONE to each subroutine..

C This subroutine reads the input data required by the AIM-2 infiltration
C model (i.e. the contents of the .aim file). It is called once per
C simulation when the other building-level input data files (e.g. .ctl file)
C are read in. All date read from the .aim file are saved to variables declared
C in MODULE AIM2_INPUT_DATA.

C INPUTS:
C    LAIM2  The name of the AIM-2 data input file.

C OUTPUTS:
C    airtight_type = Air tightness type as defined in HOT2000 interface:
C                    1=blower door test; 2=quick blower door test;
C                    3=loose; 4=average; 5=present; 6=energy tight.
C    ACH_50Pa = Air change rate @ 50Pa. Only used if blower door inputs given.
C    ELA_given = Flag indicating whether user input ELA: 0=no input;
C                1=input given.
C    ELA_cm = Equivalent leakage area in cm^2. Only used if blower door
C             inputs given.
C    ELA_deltaP = Pressure difference in Pa for ELA_cm.
C    userspec_leakage = flag indicating whether user has specified leakage
C                       fractions for ceiling, wall, and floor: 0 indicates
C                       user input not given; 1 indicates user input given.
C    AIM2_ceil_frac = Ceiling leakage fraction.
C    AIM2_wall_frac = Wall leakage fraction.
C    AIM2_floor_frac = Floor leakage fraction.
C    AIM2_terrain_weather = Terrain flag at weather station:
C                             1=Open sea,fetch > 5 km
C                             2=Mud flats,no vegetation
C                             3=Open flat terrain, grass
C                             4=Low crops, x/H > 20
C                             5=High crops, scattered obstacles
C                             6=Parkland, bushes, x/H ~ 10
C                             7=Suburban, forest
C                             8=City centre
C    AIM2_terrain_building = Terrain flag at building site. Same options
C                            as for AIM2_terrain_weather.
C    AIM2_wall_shielding = Flag indicating local shielding on walls:
C                             1=No local shielding
C                             2=Light local shielding
C                             3=Heavy
C                             4=Very heavy
C                             5=Complete (by large buildings)
C    AIM2_flue_shielding = Flag indicating local shielding on flue. Same options
C                          as for AIM2_wall_shielding.
C    AIM2_anemom_height = Height of anenometor at weather station (m).
C    AIM2_eaves_height = Height of building eaves (m).
C    AIM2_furnace_flue = Diameter of furnace flue (mm).
C    AIM2_fire_flue1 = Diameter of #1 fireplace flue (mm).
C    AIM2_fire_flue2 = Diameter of #2 fireplace flue (mm).
C    AIM2_DHW_flue1 = Diameter of primary DHW system flue (mm).
C    AIM2_DHW_flue2 = Diameter of secondary DHW system flue (mm).
C    AIM2_ref_zone = Number of zone whose temp used as reference for infil calcs.
C    AIM2_num_zones_infil = Number of zones that receive infiltration.
C    AIM2_zone_infil(i) = Zone number of i'th zone that receives infiltration.
C    AIM2_basement_zone = Zone number of basement.
C    AIM2_CrawlSpace_zone = Zone number of crawlspace.
C    AIM2_attic_zone = Zone number of attic.
      
      SUBROUTINE AIM2_READIN
      IMPLICIT NONE
#include "building.h"
#include "espriou.h"
C espriou.h provides currentfile.

      integer lnblnk

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

      COMMON/FILEP/IFIL
      INTEGER IFIL

      COMMON/C1/NCOMP,NCON
      INTEGER NCOMP,NCON

      COMMON/WindowControl/
     &     fWindowSetpoint,
     &     fACSetPoint,
     &     bWindowControlFlag,
     &     iWindowControlZone,
     &     bWindowsAreOpen,
     &     fDeltaTInttoExt,
     &     fWindowLowSetpoint,
     &     fWndmMultAIM

      REAL fWindowSetpoint
      REAL fACSetPoint
      INTEGER iWindowControlZone
      LOGICAL bWindowControlFlag
      LOGICAL bWindowsAreOpen
      REAL  fDeltaTInttoExt
      REAL fWindowLowSetpoint
      
      !-LUKAS: Variable added.
      REAL fWndmMultAIM
      
      CHARACTER WORD*128

C Declarations for local variables and variables passed in common blocks.
      CHARACTER outs*124,OUTSTR*124
      INTEGER IUNIT,ISTAT,IER,ND,k,ival
      INTEGER IFLAG
      INTEGER ICOMP
      REAL val
      LOGICAL CLOSE
      
      INTEGER :: allocate_status   !returns 0 for sucessful memory allocation
      
C Initialise H2K Defaults
      CALL AIM2_H2KDefaults()
      

C Open AIM-2 input data file. Note that if IFIL only incremented by one, problems
C occur as this file unit is already in use.
      IUNIT=IFIL+10
      CALL ERPFREE(IUNIT,ISTAT)
      CALL EFOPSEQ(IUNIT,LAIM2,1,IER)
    
      IF(ier.ne.0)goto 1000
      write(currentfile,'(a)') LAIM2(1:lnblnk(LAIM2))

C Get first line.
      CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'AIM-2 Data',IER)
      K=0
C Obtain version number of AIM file.
      CALL EGETW(OUTSTR,K,WORD,'W','Aim File Version',IFLAG)
      if(WORD(1:11).eq.'FileVersion')then
         CALL EGETWR(OUTSTR,K,fAIM2VersionNumber,0.,100.00,'F',
     &        'Version Number',IER)
        
         if(IER.NE.0)then
            call edisp(IUOUT,' Cannot read AIM-2 Version Number.')
            goto 1000
         endif
         CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' AIM-2 data',IER)
      else
         fAIM2VersionNumber = 0. ! the old version
      ENDIF


C Read air tightness type.
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWI(OUTSTR,K,IVAL,1,6,'F',' air tightness type ',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot read air tightness type.')
        goto 1000
      endif
      airtight_type = IVAL

C Read in supplementary data if blower door or quick blower door input given.
      if(airtight_type.le.2)then
      
        CALL ECLOSE(fAIM2VersionNumber, 0.0, 0.001, CLOSE)
        
        if (CLOSE) then ! the old version
          CALL EGETWR(OUTSTR,K,VAL,0.1,30.,'F',' ac/h at 50Pa',IER)
          if(IER.NE.0)then
            call edisp(IUOUT,' Cannot read ac/h at 50Pa.')
            goto 1000
          endif
          ACH_50Pa = VAL
          CALL EGETWR(OUTSTR,K,VAL,4.0,10.,'F',' ELA deltaP',IER)
          if(IER.NE.0)then
            call edisp(IUOUT,' Cannot read ELA press diff.')
            goto 1000
          endif
          ELA_deltaP = VAL
          CALL EGETWI(OUTSTR,K,IVAL,0,1,'F',' ELA given flag ',IER)
          if(IER.NE.0)then
            call edisp(IUOUT,' Cannot read ELA given flag')
            goto 1000
          endif
          ELA_given = IVAL
          ! the case "ELA_given =0" in old version corresponds to
          ! the case "blower_door_input_option=2" in new version

          ! the case "ELA_given =1" in old version corresponds to
          ! the case "blower_door_input_option=3" and "ELA_Cd=0.611"
          ! in new version

          if(ELA_given.eq.0)then  ! ELA not input by user.
            ELA_cm = 0.
            blower_door_input_option = 2
          else  ! Read ELA input by user.
            CALL EGETWR(OUTSTR,K,VAL,0.1,7999.,'F',' ELA',IER)
            if(IER.NE.0)then
              call edisp(IUOUT,' Cannot read ELA.')
              goto 1000
            endif
            ELA_cm = VAL
            blower_door_input_option = 3
            ELA_Cd = 0.611
          endif
          
        else ! the new version
        
          CALL EGETWI(OUTSTR,K,IVAL,1,3,'F',
     &                ' blower door input option ',IER)
          if(IER.NE.0)then
            call edisp(IUOUT,' Cannot read blower door input option.')
            goto 1000
          endif
  
          blower_door_input_option = IVAL
  
          if (blower_door_input_option .eq. 1) then
            CALL EGETWR(OUTSTR,K,VAL,0.,10.,'F',
     &                ' flow coefficient C0 ',IER)
            if(IER.NE.0)then
              call edisp(IUOUT,' Cannot read flow coefficient C0.')
              goto 1000
            endif
            AIM2_CO = VAL
            CALL EGETWR(OUTSTR,K,VAL,0.5,1.,'F',' flow exponent n',IER)
            if(IER.NE.0)then
              call edisp(IUOUT,' Cannot read flow exponent n.')
              goto 1000
            endif
            AIM2_n = VAL
          else  ! blower_door_input_option equal to 2 or 3
            !ACH_50Pa and ELA_deltaP are needed for both option 2 and 3
            CALL EGETWR(OUTSTR,K,VAL,0.1,30.,'F','ac/h at 50Pa',IER)
            if(IER.NE.0)then
              call edisp(IUOUT,' Cannot read ac/h at 50Pa.')
              goto 1000
            endif
            ACH_50Pa = VAL
            CALL EGETWR(OUTSTR,K,VAL,4.0,10.,'F',' ELA deltaP',IER)
            if(IER.NE.0)then
              call edisp(IUOUT,' Cannot read ELA press diff.')
              goto 1000
            endif
            ELA_deltaP = VAL
  
            !ELA and ELA_Cd are needed for option 3 only
            if(blower_door_input_option .eq. 3) then
              CALL EGETWR(OUTSTR,K,VAL,0.1,7999.,'F','ELA',IER)
              if(IER.NE.0)then
                call edisp(IUOUT,' Cannot read ELA.')
                goto 1000
              endif
              ELA_cm = VAL
              CALL EGETWR(OUTSTR,K,VAL,0.,1.,'F',' Cd for ELA ',IER)
              if(IER.NE.0)then
                call edisp(IUOUT,' Cannot read Cd for ELA.')
                goto 1000
              endif
              ELA_Cd = VAL
            endif
          endif ! -> if (blower_door_input_option .eq. 1)
        endif   ! -> if (CLOSE) 
      endif     ! -> if (airtight_type .le. 2)

C Determine whether user has specified leakage distribution.
      CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' AIM-2 data',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWI(OUTSTR,K,IVAL,0,1,'F',' user-spec leakage ',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot read flag for leakage inputs.')
        goto 1000
      endif
      userspec_leakage = IVAL
C If user has specified leakage distribution, then read leakage data.
      IF(userspec_leakage.eq.1)THEN
        CALL EGETWR(OUTSTR,K,VAL,0.,1.,'F',' ceiling leakage',IER)
        if(IER.NE.0)then
          call edisp(IUOUT,' Cannot read ceiling leakage.')
          goto 1000
        endif
        AIM2_ceil_frac = VAL
        CALL EGETWR(OUTSTR,K,VAL,0.,1.,'F',' wall leakage',IER)
        if(IER.NE.0)then
          call edisp(IUOUT,' Cannot read wall leakage.')
          goto 1000
        endif
        AIM2_wall_frac = VAL
        CALL EGETWR(OUTSTR,K,VAL,0.,1.,'F',' floor leakage',IER)
        if(IER.NE.0)then
          call edisp(IUOUT,' Cannot read floor leakage.')
          goto 1000
        endif
        AIM2_floor_frac = VAL
      ENDIF

C Read shielding and terrain data.
      CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' AIM-2 data',IER)
      IF(IER.NE.0)goto 1000
      K=0
C Terrain at weather station.
      CALL EGETWI(OUTSTR,K,IVAL,1,8,'F',' terrain at weather stat',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot read flag for terrain at weather.')
        goto 1000
      endif
      AIM2_terrain_weather = IVAL
C Terrain at building site.
      CALL EGETWI(OUTSTR,K,IVAL,1,8,'F',' terrain at building',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot read flag for terrain at building.')
        goto 1000
      endif
      AIM2_terrain_building = IVAL
C Local shielding on walls.
      CALL EGETWI(OUTSTR,K,IVAL,1,5,'F',' shielding on walls',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot read flag for shielding on walls.')
        goto 1000
      endif
      AIM2_wall_shielding = IVAL
C Local shielding on flue.
      CALL EGETWI(OUTSTR,K,IVAL,1,5,'F',' shielding on flue',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot read flag for shielding on flue.')
        goto 1000
      endif
      AIM2_flue_shielding = IVAL
C Height of anemometor at weather station.
      CALL EGETWR(OUTSTR,K,VAL,0.1,100.,'F',' anemometor',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot read anemometor height.')
        goto 1000
      endif
      AIM2_anemom_height = VAL

C Read building eaves height.
      CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' AIM-2 data',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWR(OUTSTR,K,VAL,1.,10.,'F',' building eaves',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot read building eaves height.')
        goto 1000
      endif
      AIM2_eaves_height = VAL

C Read diameters of furnace, fireplace, and DHW flues.
      CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' AIM-2 data',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWR(OUTSTR,K,VAL,0.,500.,'F',' furnace flue',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot read furnace flue diameter.')
        goto 1000
      endif
      AIM2_furnace_flue = VAL
      CALL EGETWR(OUTSTR,K,VAL,0.,500.,'F',' fireplace 1 flue',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot read fireplace 1 flue diameter.')
        goto 1000
      endif
      AIM2_fire_flue1 = VAL
      CALL EGETWR(OUTSTR,K,VAL,0.,500.,'F',' fireplace 2 flue',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot read fireplace 2 flue diameter.')
        goto 1000
      endif
      AIM2_fire_flue2 = VAL
      CALL EGETWR(OUTSTR,K,VAL,0.,500.,'F',' DHW 1 flue',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot read DHW 1 flue diameter.')
        goto 1000
      endif
      AIM2_DHW_flue1 = VAL
      CALL EGETWR(OUTSTR,K,VAL,0.,500.,'F',' DHW 2 flue',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot read DHW 2 flue diameter.')
        goto 1000
      endif
      AIM2_DHW_flue2 = VAL

C Read number of reference zone. The temperature of this zone is used
C as the reference point for calculating the indoor air density. As such,
C this zone should represent a conditioned space.
      CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' AIM-2 data',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWI(OUTSTR,K,IVAL,1,10,'F',' reference zone',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot read reference zone.')
        goto 1000
      endif
      AIM2_ref_zone = IVAL

C Read number of zones in model that will receive infiltration. This will normally
C be less than the total number of zones in the model, as attic spaces and garages
C will not receive the AIM-2 calculated infiltration. Following this, read indices
C of the zones that do receive infiltration.
      CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' AIM-2 data',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWI(OUTSTR,K,IVAL,1,10,'F',' num zones with infil',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot read num of zones with infil.')
        goto 1000
      endif
      AIM2_num_zones_infil = IVAL
      
 
      IF (AIM2_num_zones_infil.GT.0) THEN
C Ensure that arrays AIM2_zone_infil and AIM2_infil_to_zones are deallocate d.
        CALL AIM2_Deallocate
C Allocate memory for AIM2_num_zones_infil
        ALLOCATE(AIM2_zone_infil(AIM2_num_zones_infil),
     &       STAT=allocate_status)
C Check allocation was sucessful     
        IF (.NOT.ALLOCATED(AIM2_zone_infil)) THEN
          CALL EDISP(IUOUT,' Warning: AIM2_zone_infil not allocated')
	END IF
C Allocate memory for AIM2_num_zones_infil
        ALLOCATE(AIM2_infil_to_zones(NCOMP),
     &       STAT=allocate_status)
C Check allocation was sucessful     
        IF (.NOT.ALLOCATED(AIM2_infil_to_zones)) THEN
          CALL EDISP(IUOUT,'AIM2_infil_to_zones not allocated')
	END IF
	  
        IF (ALLOCATED(AIM2_zone_infil)) THEN
          DO ICOMP=1,AIM2_num_zones_infil
            CALL EGETWI(OUTSTR,K,IVAL,1,10,'F',
     &          ' zone receiving infil',IER)
            if(IER.NE.0)then
              call edisp(IUOUT,' zone receiving infil.')
              goto 1000
            endif
            AIM2_zone_infil(ICOMP) = IVAL
	  END DO
        END IF
      END IF


C Read zone number of basement (basements treated differently that living space
C in terms of allocation of infiltration air flow).
      CALL STRIPC(IUNIT,OUTSTR,99,ND,1,' AIM-2 data',IER)
      IF(IER.NE.0)goto 1000
      K=0
      CALL EGETWI(OUTSTR,K,IVAL,0,10,'F',' basement zone',IER)
      if(IER.NE.0)then
        call edisp(IUOUT,' Cannot read basement zone.')
        goto 1000
      endif
      AIM2_basement_zone = IVAL
C Read zone number of crawlspace. If no data specified, then this was an old
C version of the .aim file so assume no crawlspace (AIM2_CrawlSpace_zone=0).
      If ( ND .lt. 2 ) Then ! No data, default to 'no zone'
         IVAL = 0
      Else ! Read it
         CALL EGETWI(OUTSTR,K,IVAL,0,10,'F',' Crawl Space zone',IER)
         if(IER.NE.0)then
           call edisp(IUOUT,' Cannot read Crawl Space zone.')
           goto 1000
         endif
      Endif
      AIM2_CrawlSpace_zone = IVAL
C Read zone number of the attic. If no data specified, then this was an old
C version of the .aim file so assume no attic (AIM2_attic_zone=0).
      If ( ND .lt. 3 ) Then ! No data, default to 'no zone'
         IVAL = 0
      Else ! Read it
         CALL EGETWI(OUTSTR,K,IVAL,0,10,'F',' attic zone',IER)
         
         if(IER.NE.0)then
           call edisp(IUOUT,' Cannot read attic zone.')
           goto 1000
         endif
      Endif
      AIM2_attic_zone = IVAL
      IF (fAIM2VersionNumber .ge. 2.00 ) THEN

C Read data inputs for window controls :
C    - Window Control flag (0,1) 
C    - Location of temperature control 
C    - Hot (upper?) setpoint (oC)
C    - Cold (lower?) setpoint (oC)
C    - Temperature difference control (oC, min temp difference needed 
C      to open windows?)
C    ( All on the same line !)
         CALL STRIPC(IUNIT,OUTSTR,0,ND,1,' AIM-2 data',IER)
         IF(IER.NE.0)goto 1000
         K=0
         CALL EGETWI(OUTSTR,K,IVAL,0,1,'F',
     &        ' Flag for window control',IER)
         if(IER.NE.0)then
            call edisp(IUOUT,'Cannot read window Control Flag')
            goto 1000
         endif
C Set logical flag for window control.
         IF (IVAL .ge. 1 ) THEN
            bWindowControlFlag = .true.
         ELSE
            bWindowControlFlag = .false.
         ENDIF
C     if the window control model is active get the setpoint and actuating zone.
         IF (bWindowControlFlag ) THEN
 
         CALL EGETWI(OUTSTR,K,IVAL,1,MCOM,'F',
     &           ' Zone actuator for window control',IER)
         if(IER.NE.0)then
            call edisp(IUOUT,
     &           'Cannot read zone actuator for win control')
            goto 1000
         endif
         iWindowControlZone = IVAL

         CALL EGETWR(OUTSTR,K,VAL,0.,500.,'F',
     &        ' Window Control Hot Setpoint',IER)
         if(IER.NE.0)then
            call edisp(IUOUT,' Cannot read window hot setpoint.')
            goto 1000
         endif
         fWindowSetpoint = VAL

         CALL EGETWR(OUTSTR,K,VAL,0.,500.,'F',
     &        ' Window Cold  Setpoint',IER)
         if(IER.NE.0)then
            call edisp(IUOUT,'Cannot read window cold  setpoint.')
            goto 1000
         endif
         fWindowLowSetpoint = VAL

        CALL EGETWR(OUTSTR,K,VAL,0.,100.,'F',
     &        ' Temperature Difference Control',IER)
         if(IER.NE.0)then
            call edisp(IUOUT,
     &           ' Cannot read DeltaT (Text-Tint) for window control')
            goto 1000
         endif
         fDeltaTInttoExt = VAL

      ENDIF
      ENDIF

C Close AIM-2 input data file.
      CALL ERPFREE(IUNIT,ISTAT)
      goto 1001

C Error handling.
 1000 IER=1
      write(outs,7)
    7 format(' AIM-2 input file error.')
      call edisp(iuout,outs)
      stop ' Error in AIM-2 input file.'

 1001 Continue
      RETURN
      END SUBROUTINE AIM2_READIN


C ********************************************************************************
C ************************ AIM2_SETVARS ******************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: August 4, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine controls the calculation and setting of all time-invariant
C data for the AIM-2 infiltration model. It is called once per simulation
C prior to commencement of the time-step simulation.

C INPUTS:
C    None: controlling routine only.

C OUTPUTS:
C    None: controlling routine only.
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_SETVARS

      IMPLICIT NONE
C Calculate the volume of the house.
      CALL AIM2_HOUSE_VOLUME

C Set the leakage distribution fractions.
      CALL AIM2_LEAKDIST

C Set the building's total leakage coefficient and the exponent of the
C flow-versus-pressure relationship.
      CALL AIM2_COandN

C Set the effective diameter of the furnace flue.
      CALL AIM2_flue_size

C Set the shelter coefficient for the walls and the flue.
      CALL AIM2_shelter_wall_flue

C Calculate the wind speed correction factor.
      CALL AIM2_wind_correct

C Set the height of the flue.
      CALL AIM2_FLUEHGT

      RETURN
      END SUBROUTINE AIM2_SETVARS


C ********************************************************************************
C ************************ AIM2_HOUSE_VOLUME *************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 31, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine calculates the "internal" volume of the house. The "internal"
C volume is defined as the volume of the zones that are aerodynamically a part
C of the house's living space.

C INPUTS:
C    NCOMP  The number of zones in the building model.
C    AIM2_num_zones_infil  The number of zones that receive infiltration.
C    AIM2_zone_infil(i)  The zone index of the i'th zone that receives infiltration.
C    AIM2_basement_zone  The zone index of the basement zone.
C    VOL(i)  The volume (m^3) of i'th zone (standard ESP-r variable).

C OUTPUTS:
C    AIM2_house_vol  Total volume of zones that are internal to house (m^3).
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_HOUSE_VOLUME

      IMPLICIT NONE

#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: IUOUT,IUIN


C Declarations for local variables.
      INTEGER i,ICOMP

C Sum the volumes of the zones that are aerodynamically part of the
C house's living space. The zones to include in this summation are defined
C in the AIM-2 input file.
      AIM2_house_vol = 0.
      IF (ALLOCATED(AIM2_zone_infil)) THEN
        DO i=1,AIM2_num_zones_infil
          ICOMP = AIM2_zone_infil(i)                    ! Get zone index number.
c       AIM2_house_vol = AIM2_house_vol + VOL(ICOMP)  ! Add volume of zone.
c The crawl space zone is not part of the living space
          If ( ICOMP .ne. AIM2_CrawlSpace_zone .and.
     &        ICOMP .ne. AIM2_attic_zone) Then
          AIM2_house_vol = AIM2_house_vol + VOL(ICOMP)
          Endif
        END DO
        ELSE
          CALL EDISP(IUOUT,' Warning: AIM2 arrays not allocated')
      END IF

      RETURN
      END SUBROUTINE AIM2_HOUSE_VOLUME


C ********************************************************************************
C ************************** AIM2_FLUEHGT ****************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: August 10, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine sets the height of the flue, a variable required by the
C AIM-2 algorithm. The flue height is simply assumed to be 1.5m above
C the height of the eaves, which is input by the user. It also calculates
C the normalized height of the flue.

C INPUTS:
C    AIM2_eaves_height  The user-input eaves height (m).

C OUTPUTS:
C    AIM2_flue_height  The flue height (m)
C    AIM2_Betaf  The normalized flue height (dimensionless).
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_FLUEHGT


      IMPLICIT NONE


C Set the flue height.
      AIM2_flue_height = AIM2_eaves_height + 1.5

C Calculate the normalized height of the flue. This is eq 20 from
C Wilson and Walker (1990).
      AIM2_Betaf = AIM2_flue_height / AIM2_eaves_height

      RETURN
      END SUBROUTINE AIM2_FLUEHGT


C ********************************************************************************
C ************************ AIM2_LEAKDIST *****************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: August 4, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine sets the ceiling, floor, and wall leakage fractions
C for the AIM-2 infiltration model. It is called once per simulation
C prior to commencement of the time-step simulation. The leakage fractions
C are either input by the user, or they are set according to an algorithm
C that gives the leakage as a function of building type, number of storeys,
C and foundation type. This algorithm is extracted from Lew (1993) and its
C implementation here is based upon the implementation in HOT2000 v8.

C INPUTS:
C    userspec_leakage  Flag indicating whether user has specified leakage
C                      fractions for ceiling, wall, and floor: 0 indicates
C                      user input not given; 1 indicates user input given.
C    H2K_house_type  House type input by user
C    H2K_storeys  Description of number of storeys input by user
C    H2K_found_num  Number of foundations input by user
C    H2K_found_area  Floor area of foundation (m^2).

C OUTPUTS:
C    AIM2_ceil_frac  Ceiling leakage fraction (dimensionless).
C    AIM2_wall_frac  Wall leakage fraction (dimensionless).
C    AIM2_floor_frac  Floor leakage fraction (dimensionless).
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_LEAKDIST

      IMPLICIT NONE


C Type declarations for local variables.
      INTEGER i,j,k,n
      INTEGER jj
      INTEGER j_biggest
      REAL area_temp
      REAL total

C Leakage fractions from Table 3 of Lew (1993).
C `i' is building type index, `j' foundation type index, and `k' a storey index.
      REAL LeakDis(2,4,5,3)
      Data (((( LeakDis(i,j,k,n), n=1,3), k=1,5), j=1,4), i=1,2 ) /
     j .20,.50,.30, .20,.50,.30, .15,.60,.25, .15,.60,.25, .10,.70,.20,
     2 .30,.50,.20, .30,.50,.20, .20,.60,.20, .20,.65,.15, .15,.70,.15,
     3 .30,.50,.20, .30,.50,.20, .20,.60,.20, .15,.70,.15, .10,.80,.10,
     4 .30,.50,.20, .20,.60,.20, .20,.65,.15, .20,.70,.10, .10,.80,.10,
     j .30,.40,.30, .30,.40,.30, .20,.50,.30, .20,.60,.20, .20,.60,.20,
     2 .30,.40,.30, .30,.50,.20, .20,.60,.20, .20,.60,.20, .20,.65,.15,
     3 .30,.50,.20, .30,.55,.15, .25,.60,.15, .20,.70,.10, .20,.70,.10,
     4 .30,.50,.20, .30,.50,.20, .20,.60,.20, .20,.70,.10, .20,.70,.10 /


C Establish the leakage fractions. If the user did not specify the leakage
C distribution, then determine the leakage based on house type, number
C of storeys, and foundation type.

      IF (userspec_leakage.eq.0) THEN  ! User did not specify leakage distribution.

C-------First determine house type and set index `i' for looking up leakage
C-------distribution in `LeakDis' array. Leakage distributions were only
C-------established for single detached and semi-detached houses. Mobile homes
C-------are treated like single detached houses in terms of infiltration. The
C-------leakage distribution of all other other building types (duplex, triplex,
C-------apartment, row house) is treated the same and is not a function of
C-------foundation type or number of storeys.
        i = H2K_house_type              ! H2K_house_type (as defined in H2K list 62).
        if (H2K_house_type.eq.7) i=1    ! Treat mobile homes like single-detached.
        IF ( i.gt.2 ) THEN              ! Duplex, triplex, apt, or row house.
          AIM2_ceil_frac = 0.3          ! Set fractions.
          AIM2_wall_frac = 0.5
          AIM2_floor_frac = 0.2
        ELSE                            ! Single-detached house or semi-detached

C---------Determine predominant foundation type and set index `j' for looking
C---------up leakage distribution in `LeakDis' array. If the user has input
C---------multiple foundations, then use largest to establish leakage distribution.
          IF (H2K_found_num.eq.0) THEN      ! No foundation input by user.
            j = 2                           ! Assume slab-on-grade for purposes of calculating infiltration.
          ELSEIF (H2K_found_num.eq.1) THEN  ! One foundation input by user.
            CALL AIM2_SETJ(1,j)             ! Set `j'.
          ELSE                              ! User has input multiple foundations: find largest.
            area_temp = 0.
            do jj=1,H2K_found_num
C-------------Walkout areas should be treated differently than other foundation types.
C-------------Refer to Ian's email to Brian Bradley on 2000/08/09 entitled "walkouts".
C-------------Revise this once walkouts implemented in BASESIMP.
ccc           if( H2K_found_type(jj).eq.7 ) then  ! Walkout
ccc             H2K_found_area(jj) = ????
ccc           endif
              if( H2K_found_area(jj).gt.area_temp ) then
                j_biggest = jj
                area_temp = H2K_found_area(jj)
              endif
            enddo
            CALL AIM2_SETJ(j_biggest,j)

          ENDIF

C---------Set `k' index for looking up leakage distribution in `LeakDis' array.
C---------This is based on the description of the number of storeys given by the
C---------user. If the house is a split level, then treat it as a one-and-a-half
C---------storey house in terms of the infiltration calculations.
          k = H2K_storeys
          if( k.gt.5) k=2  ! Treat split level as 1.5 storey.

C---------`i', `j', and `k' have now been set, so look up leakage from
C---------`LeakDis' array.
          AIM2_ceil_frac = LeakDis(i,j,k,1)
          AIM2_wall_frac = LeakDis(i,j,k,2)
          AIM2_floor_frac = LeakDis(i,j,k,3)

        ENDIF

      ELSE  ! User specified leakage distribution. Normalize inputs to sum to 1.
        total = AIM2_ceil_frac + AIM2_wall_frac + AIM2_floor_frac
        AIM2_ceil_frac = AIM2_ceil_frac/total
        AIM2_wall_frac = AIM2_wall_frac/total
        AIM2_floor_frac = AIM2_floor_frac/total

      ENDIF

      RETURN
      END SUBROUTINE AIM2_LEAKDIST


C ********************************************************************************
C ************************ AIM2_SETJ *********************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: August 9, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine determines the foundation type and set index `j' for
C looking up the leakage distribution in `LeakDis' array. The index `j'
C indicates the foundation type, but uses a different numbering
C scheme than H2K's generic foundation description.

C INPUTS:
C	ifound  The index number of the foundation under consideration.
C       H2K_found_type  The H2K foundation type flag.
C       H2K_found_depth  The depth of the foundation as input by the user.

C OUTPUTS:
C	j  The foundation type index corresponding to the `LeakDis' array.
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_SETJ(ifound,j)

      IMPLICIT NONE


C Declarations for local variables.
      INTEGER ifound,j


C Examine foundation type and depth of foundation under consideration
C and set `j' index accordingly.
      if ( H2K_found_type(ifound).eq.2 .and.
     &     H2K_found_depth(ifound).ge.1.) j=4  ! Deep basement
      if ( H2K_found_type(ifound).eq.2 .and.
     &     H2K_found_depth(ifound).lt.1.) j=3  ! Shallow basement
      if ( H2K_found_type(ifound).eq.3 ) j=2  ! Slab-on-grade
      if ( H2K_found_type(ifound).ge.4 .and.
     &     H2K_found_type(ifound).le.6 ) j=1  ! Crawl-space
      if ( H2K_found_type(ifound).eq.7 ) j=4  ! Walkout

      RETURN
      END SUBROUTINE AIM2_SETJ


C ********************************************************************************
C ****************************** AIM2_COandN *************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: August 18, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine calculates the building's total fabric leakage coefficient
C and the exponent of the flow-versus-pressure relationship, values required
C by the AIM-2 model. It is called once per simulation, prior to the time-step
C calculations.

C INPUTS:
C    airtight_type  The airtightness type defined by the user.
C    AIM2_house_vol  Volume of house.
C    ACH_50Pa  The ac/h @50 Pa depressurization from blower door test.
C    ELA_given  Flag indicating whether user input ELA.
C    ELA_cm  The ELA from the blower door test.
C    ELA_deltaP  The pressure difference for the given ELA.

C OUTPUTS:
C    AIM2_CO  The building's total fabric leakage coefficient (m^3/s/Pa^1/n).
C    AIM2_n  The exponent of the flow-pressure relationship (dimensionless).
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_COandN

      IMPLICIT NONE

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

C Type declarations for local variables.
      REAL part1, part2, part3, ELA_const1, ELA_const2

C Named constants for case statement. These are the six choices offered
C in the H2K interface.
      INTEGER BLOWER_DOOR
      parameter (BLOWER_DOOR=1)

      INTEGER QUICK_BLOWER_DOOR
      parameter (QUICK_BLOWER_DOOR=2)

      INTEGER LOOSE
      parameter (LOOSE=3)

      INTEGER AVERAGE
      parameter (AVERAGE=4)

      INTEGER PRESENT
      parameter (PRESENT=5)

      INTEGER ENERGY_TIGHT
      parameter (ENERGY_TIGHT=6)



C Set the fabric leakage coefficient and the flow exponent based on the
C air tightness description given by user. If the user has
C CHARACTERized the leakage with a blower door test, the leakage
C coefficient and flow exponent are calculated using the procedures
C described in Beausoleil-Morrison (2000a, 2000b). Two procedures
C are used: one when the user has input the ELA and the other when
C the user has not input the ELA. Alternatively, if the user has
C specified one of H2K's standard air tightnesses (e.g. "loose"),
C the total leakage coefficient is calculated as the product of the
C house volume and the normalized leakage coefficient. The
C normalized leakage coefficients and flow exponents for this
C case are taken from Bradley (1993), Table 3, p11. Note that this
C subroutine calculates the fabric leakage coefficient. This is
C the sum of the floor, ceiling, and wall leakage coefficients.
C It it the building's total leakage coefficient minus the flue
C leakage, and is known as the C0 coefficient. The units of the leakage
C coefficient are m^3/(s Pa^n) where `n' is the flow exponent
C (dimensionless).

      SELECT CASE (airtight_type)

        CASE (BLOWER_DOOR:QUICK_BLOWER_DOOR)

          IF (blower_door_input_option .ne. 1) THEN
        ! ELA_const2 modified from eq 35 of Bradley (p10), considering the
        ! case that different discharge coeffients may be used to calculate ELA
        ! Note that the Constants From eq 32 of Bradley (p9)
        ! and eq 35 of Bradley (p10) are actually the same
        
            ELA_const1 = sqrt(1.204097/2.)*10000.

            if (blower_door_input_option .eq. 2) then
            ! User did not enter ELA: use BeauMo (2000b)
            ! Cd=0.611 is used to calculate ELA
              ELA_const2 = ELA_const1/0.611
              AIM2_n = 0.68
              part1 = ELA_const2 * ACH_50Pa * AIM2_house_vol
     &                * ELA_deltaP**(AIM2_n-0.5)
              part2 = 3600. * 50.**AIM2_n
              ELA_cm = part1 / part2  ! Eq 3 of Beausoleil-Morrison (2000b)
            else ! blower_door_input_option .eq. 3
              ! User specified ELA: use BeauMo (2000a)
              ELA_const2 = ELA_const1/ELA_Cd
              part1 = ACH_50Pa * AIM2_house_vol * ELA_const2
              part2 = ELA_cm * sqrt(ELA_deltaP) * 3600.
              part3 = 50. / ELA_deltaP
              AIM2_n = ( log(part1/part2) ) / log(part3) ! Eq 5 of BeauMo (2000a)
            endif
  
            AIM2_CO = ELA_cm /      ! Eq 6 of Beausoleil-Morrison (2000a)
     &                    (ELA_const2 * ELA_deltaP**(AIM2_n-0.5))
          ENDIF

        CASE (LOOSE)

          AIM2_CO = AIM2_house_vol*16.86105e-5   
          AIM2_n = 0.725

        CASE (AVERAGE)

          AIM2_CO = AIM2_house_vol*8.23811e-5
          AIM2_n = 0.698

        CASE (PRESENT)

          AIM2_CO = AIM2_house_vol*6.10730e-5
          AIM2_n = 0.7125

        CASE (ENERGY_TIGHT)

          AIM2_CO = AIM2_house_vol*2.66534e-5
          AIM2_n = 0.7028

        CASE DEFAULT
          call edisp(IUOUT,' Error in AIM2_COandN: incorrect airtight.')

      END SELECT


      RETURN
      END SUBROUTINE AIM2_COandN


C ********************************************************************************
C ***************************** AIM2_flue_size ***********************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 2, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine sets the effective diameter of the furnace flue for both
C on (i.e. furnace operating) and off cycles. It is called once
C per simulation, prior to performing the time-step calculations.

C INPUTS:
C    ifuel_type(i)  Index indicating fuel used by furnace.
C    isys_type(i)  Indicates type of furnace selected by user.
C    AIM2_furnace_flue  Diameter of furnace flue (mm).

C OUTPUTS:
C    furnON_flue_diam  Diameter of furnace flue when furnace operating (mm).
C    furnOFF_flue_diam  Diameter of furnace flue when furnace off (mm).
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_flue_size

      IMPLICIT NONE
#include "building.h"
#include "hvac_parameters.h"
#include "hvac_common.h"

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

C Type declarations for local variables.
      INTEGER H2K_furnace_fuel,H2K_furnace_type

C Named constants for fuel case statement. These are the five choices offered
C in the H2K interface.
      INTEGER ELEC
      parameter (ELEC=1)

      INTEGER GAS
      parameter (GAS=2)

      INTEGER OIL
      parameter (OIL=3)

      INTEGER PROPANE
      parameter (PROPANE=4)

      INTEGER WOOD
      parameter (WOOD=5)

C The following parameters were altered in July 2001 in order to take the
C furnace type from the .hvac file. Prior to this, these parameters were
C specified in a MODULE that held `dummy' data to enable the development
C and testing of the AIM-2 module in advance of the development of the HVAC models.

C Named constants for the furnace type case statement. These are the choices
C from the H2K interface that are relevant to this subroutine.
C Gas/propane furnace types:
      INTEGER PILOT
      parameter (PILOT=7)

      INTEGER SPARK
      parameter (SPARK=5)

      INTEGER SPARK_VENT
      parameter (SPARK_VENT=6)

      INTEGER INDUCED
      parameter (INDUCED=10)

      INTEGER CONDENSING
      parameter (CONDENSING=8)

C Oil furnace types:
      INTEGER FURNACE
      parameter (FURNACE=14)

      INTEGER VENT_DAMPER
      parameter (VENT_DAMPER=4)

      INTEGER FLAME_RET_HEAD
      parameter (FLAME_RET_HEAD=3)

      INTEGER MIDEFF
      parameter (MIDEFF=11)

      INTEGER CONDENSING_OIL
      parameter (CONDENSING_OIL=8)

      INTEGER DIRECT_VENT_NO_COND
      parameter (DIRECT_VENT_NO_COND=9)


C Map from variables holding data read from .hvac to variables that were
C originally used in this subroutine. `H2K_furnace_fuel' and `H2K_furnace_type'
C were formerly hard-wired in MODULE AIM2_H2K_DUMMY.

C The furnace's fuel type. Note that this assumes a single HVAC system
C is serving the house. If multiple systems are supported in the
C future then this logic will require refinement.
      H2K_furnace_fuel = ifuel_type(1)  ! Use system 1.

C The furnace type. Again, assumes a single HVAC system serving house.
      H2K_furnace_type = isys_type(1)  ! Use system 1.


C Set the opening diameter of the furnace flue for the case when the furnace
C is operating (furnON_flue_diam) and for the case when the furnace is
C off (furnOFF_flue_diam). This is a function of the furnace type and
C its fuel.

      SELECT CASE (H2K_furnace_fuel)

        CASE (ELEC) ! No flue for electric furnaces.
          furnON_flue_diam = 0.
          furnOFF_flue_diam = 0.

        CASE (GAS,PROPANE) ! Treatment depends on type of furnace.

          SELECT CASE (H2K_furnace_type)
            CASE (PILOT,SPARK)
              furnON_flue_diam = AIM2_furnace_flue
              furnOFF_flue_diam = AIM2_furnace_flue ! Flue always fully open.

            CASE (SPARK_VENT)
              furnON_flue_diam = AIM2_furnace_flue
              furnOFF_flue_diam = 0.5 * AIM2_furnace_flue ! Flue constricted.

            CASE (INDUCED,CONDENSING) ! Combustion isolated from house air.
              furnON_flue_diam = 0.
              furnOFF_flue_diam = 0.

            CASE DEFAULT
              call edisp(IUOUT,' Error in AIM2_flue_size.')

          END SELECT

        CASE (OIL)

          SELECT CASE (H2K_furnace_type)
            CASE (FURNACE) ! Old style furnace with no ventilated damper.
              furnON_flue_diam = AIM2_furnace_flue
              furnOFF_flue_diam = AIM2_furnace_flue ! Flue always fully open.

            CASE (VENT_DAMPER,FLAME_RET_HEAD)
              furnON_flue_diam = AIM2_furnace_flue
              furnOFF_flue_diam = 0.74162 * AIM2_furnace_flue ! Flue constricted.

            CASE (MIDEFF,CONDENSING_OIL,DIRECT_VENT_NO_COND)
              furnON_flue_diam = 0. ! Combustion isolated from house.
              furnOFF_flue_diam = 0.

            CASE DEFAULT
              call edisp(IUOUT,' Error in AIM2_flue_size.')

          END SELECT

        CASE (WOOD)
          furnON_flue_diam = AIM2_furnace_flue
          furnOFF_flue_diam = AIM2_furnace_flue ! Flue always fully open.

        CASE DEFAULT
          call edisp(IUOUT,' Warning in AIM2_flue_size.')
          call edisp(IUOUT,'   AIM2_flue_size expects a .hvac '//
     &     'file for flue on/off.')
          call edisp(IUOUT,'   This file was likely not supplied.')
          
          
      END SELECT

      RETURN
      END SUBROUTINE AIM2_flue_size


C ********************************************************************************
C ************************* AIM2_shelter_wall_flue *******************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: August 22, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine sets the wall and flue shelter coefficients, which
C are subsequently used during the time-step simulation to calculate
C the building's shelter coefficient. This subroutine is called once
C per simulation, prior to performing the time-step calculations.

C INPUTS:
C    AIM2_wall_shielding  The shielding description for the walls.
C    AIM2_flue_shielding  The shielding description for the flue.

C OUTPUTS:
C    AIM2_Swo  The shelter coefficients for the walls (dimensionless).
C    AIM2_Swflue  The shelter coefficients for the flue (dimensionless).
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_shelter_wall_flue

      IMPLICIT NONE

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

C Named constants for case statement. These are the five choices offered
C in the H2K interface.
      INTEGER NONE
      parameter (NONE=1)

      INTEGER LIGHT
      parameter (LIGHT=2)

      INTEGER HEAVY
      parameter (HEAVY=3)

      INTEGER VERY_HEAVY
      parameter (VERY_HEAVY=4)

      INTEGER COMPLETE
      parameter (COMPLETE=5)

C Set the shelter coefficient for the walls based on the user's
C description. The descriptions and coefficients are from Table 2
C of Walker and Wilson (1990, p18). The shelter coefficient for the
C walls is named "Swo" in Walker and Wilson.
      SELECT CASE (AIM2_wall_shielding)

        CASE (NONE)
          AIM2_Swo = 1.0

        CASE (LIGHT)
          AIM2_Swo = 0.9

        CASE (HEAVY)
          AIM2_Swo = 0.7

        CASE (VERY_HEAVY)
          AIM2_Swo = 0.5

        CASE (COMPLETE)
          AIM2_Swo = 0.3

        CASE DEFAULT
          call edisp(IUOUT,' Error in AIM2_shelter_wall_flue.')

      END SELECT

C Set the shelter coefficient for the flue. The same descriptions
C and coefficients are used as for the walls.
      SELECT CASE (AIM2_flue_shielding)

        CASE (NONE)
          AIM2_Swflue = 1.0

        CASE (LIGHT)
          AIM2_Swflue = 0.9

        CASE (HEAVY)
          AIM2_Swflue = 0.7

        CASE (VERY_HEAVY)
          AIM2_Swflue = 0.5

        CASE (COMPLETE)
          AIM2_Swflue = 0.3

        CASE DEFAULT
          call edisp(IUOUT,' Error in AIM2_shelter_wall_flue.')

      END SELECT

      RETURN
      END SUBROUTINE AIM2_shelter_wall_flue


C ********************************************************************************
C **************************** AIM2_wind_correct *********************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: August 22, 2000
C Copyright 2000: NRCan Buildings Group

C This subroutine calculates the wind speed correction factor. This
C relates the wind speed at the building to the wind speed at the
C weather station. This subroutine is called once per simulation,
C prior to performing the time-step calculations. The procedure followed
C here is from Bradley (1993), which differs from the approach described
C by Walker and Wilson (1990).

C INPUTS:
C    AIM2_terrain_weather  The terrain flag at the weather station.
C    AIM2_terrain_building  The terrain flag at the building site.
C    AIM2_anemom_height  Height of anenometor at weather station (m).
C    AIM2_eaves_height  Height of building eaves (m).  

C OUTPUTS:
C    AIM2_Ct  The wind speed correction factor.
C --------------------------------------------------------------------------------

      SUBROUTINE AIM2_wind_correct

      IMPLICIT NONE

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

C Type declarations for local variables.
      REAL Zmet, Zsite

C Named constants for case statement. These are the eight choices offered
C in the H2K interface.
      INTEGER OPEN_SEA
      parameter (OPEN_SEA=1)

      INTEGER MUD_FLATS
      parameter (MUD_FLATS=2)

      INTEGER OPEN_FLAT
      parameter (OPEN_FLAT=3)

      INTEGER LOW_CROPS
      parameter (LOW_CROPS=4)

      INTEGER HIGH_CROPS
      parameter (HIGH_CROPS=5)

      INTEGER PARKLAND
      parameter (PARKLAND=6)

      INTEGER SUBURB
      parameter (SUBURB=7)

      INTEGER CITY_CENTRE
      parameter (CITY_CENTRE=8)

C Set the surface roughness length at the weather station. These values are from
C Wieringa (1986) via Table 2 of Bradley (1993, p8). Note that the value for "open
C flat terrain" is given incorrectly in Bradley (1993).
      SELECT CASE (AIM2_terrain_weather)

        CASE (OPEN_SEA)
          Zmet = 0.0002

        CASE (MUD_FLATS)
          Zmet = 0.005

        CASE (OPEN_FLAT)
          Zmet = 0.03  ! Value in Bradley (1993) incorrect. 

        CASE (LOW_CROPS)
          Zmet = 0.1

        CASE (HIGH_CROPS)
          Zmet = 0.25

        CASE (PARKLAND)
          Zmet = 0.5

        CASE (SUBURB)
          Zmet = 1.0

        CASE (CITY_CENTRE)
          Zmet = 2.0  ! Not given in Wieringa (1986), but rather Bradley (1993).

        CASE DEFAULT
          call edisp(IUOUT, ' Error in AIM2_wind_correct:'
     &                     // 'incorrect weather station terrain type.')

      END SELECT

C Set the surface roughness length at the building site. The same descriptions
C and values as used as for the weather site.
      SELECT CASE (AIM2_terrain_building)

        CASE (OPEN_SEA)
          Zsite = 0.0002

        CASE (MUD_FLATS)
          Zsite = 0.005

        CASE (OPEN_FLAT)
          Zsite = 0.03

        CASE (LOW_CROPS)
          Zsite = 0.1

        CASE (HIGH_CROPS)
          Zsite = 0.25

        CASE (PARKLAND)
          Zsite = 0.5

        CASE (SUBURB)
          Zsite = 1.0

        CASE (CITY_CENTRE)
          Zsite = 2.0

        CASE DEFAULT
          call edisp(IUOUT,' Error in AIM2_wind_correct:'
     &                  //  'incorrect on-site terrain type.')

      END SELECT

C Calculate the wind speed correction factor. This is a function of the
C surface roughness lengths at the building site and weather station,
C the height of the eaves, and the height of the anemometor at the
C weather station. This is equation 27 from Bradley (1993, p8).

      AIM2_Ct = ( log(60./Zmet) * log(AIM2_eaves_height/Zsite) ) /
     &          ( log(AIM2_anemom_height/Zmet) * log(60./Zsite) )


      RETURN
      END SUBROUTINE AIM2_wind_correct

C ********************************************************************************
C **************************** AIM2_report **************************************
C Created by: ESRU
C Initial Creation Date: February 28, 2007
C Copyright 2007: ESRU
C AIM2_report decodes the AIM-2 common blocks into a brief report.

      subroutine AIM2_report(itru)

      IMPLICIT NONE
#include "building.h"
#include "geometry.h"

      integer lnblnk



C Declarations for local variables and variables passed in common blocks.
      CHARACTER outs248*248
      character house*32,stories*32,foundation*32    ! string buffers for details 
      character fireplace*96,dhwphrase*48,dhwshare*48
      character dhwinside*48,dhwsolar*32,airtight*112
      character w_terrain*48,b_terrain*48       ! terrain at weather site and bld
      character b_shielding*48,f_shielding*48   ! shielding of building and flue
      character crawl*48,attic*48

      INTEGER ij,itru,llna
      REAL total
      LOGICAL CLOSE

C Decode based on definitions in AIM2_READIN.

C Compose airtightness description based on the combinations of
C parameters and version number of the file.
      CALL ECLOSE(fAIM2VersionNumber, 0.0, 0.001, CLOSE)
      if(airtight_type.eq.1)then
        if (CLOSE) then ! the old v
          write(airtight,'(a,f7.3,a,f7.3,a,f8.1,a,f7.3)') 
     &      'a blower door test with ',
     &      ACH_50Pa,' ac/h @ 50Pa & ',ELA_deltaP,
     &      ' press diff. ELA cm ',ELA_cm,' ELA Cd ',ELA_Cd
        else
          if (blower_door_input_option .eq. 1) then
            write(airtight,'(a,f7.3,a,f7.3,a,f7.3,a,f7.3,a)') 
     &        'a blower door test with flow coef',AIM2_CO,' exp n',
     &      AIM2_n,' & ',ACH_50Pa,' ac/h @ 50Pa & ',ELA_deltaP,
     &      ' press diff'
          elseif(blower_door_input_option .eq. 3) then
            write(airtight,'(a,f8.1,a,f7.3,a,f7.3,a,f7.3,a)') 
     &        'a blower door test with ELA',ELA_cm,' and ELA Cd',
     &        ELA_Cd,' & ',ACH_50Pa,' ac/h @ 50Pa & ',ELA_deltaP,
     &      ' press diff'
          elseif(blower_door_input_option .eq. 2) then
            write(airtight,'(a,f7.3,a,f7.3,a)') 
     &        'a blower door test with ',
     &        ACH_50Pa,' ac/h @ 50Pa & ',ELA_deltaP,' press diff'
          endif
        endif
      elseif(airtight_type.eq.2)then
        if (CLOSE) then ! the old v
          write(airtight,'(a,f7.3,a,f7.3,a,f8.1,a,f7.3)') 
     &      'a quick blower door test with ',
     &      ACH_50Pa,' ac/h @ 50Pa & ',ELA_deltaP,
     &      ' press diff. ELA cm ',ELA_cm,' ELA Cd ',ELA_Cd
        else
          if (blower_door_input_option .eq. 1) then
            write(airtight,'(a,f7.3,a,f7.3,a,f7.3,a,f7.3,a)') 
     &        'a blower door test with flow coef',AIM2_CO,' exp n',
     &      AIM2_n,' & ',ACH_50Pa,' ac/h @ 50Pa & ',ELA_deltaP,
     &      ' press diff'
          elseif(blower_door_input_option .eq. 3) then
            write(airtight,'(a,f8.1,a,f7.3,a,f7.3,a,f7.3,a)') 
     &        'a blower door test with ELA',ELA_cm,' and ELA Cd',
     &        ELA_Cd,' & ',ACH_50Pa,' ac/h @ 50Pa & ',ELA_deltaP,
     &      ' press diff'
          elseif(blower_door_input_option .eq. 2) then
            write(airtight,'(a,f7.3,a,f7.3,a)') 
     &        'a blower door test with ',
     &        ACH_50Pa,' ac/h @ 50Pa & ',ELA_deltaP,' press diff'
          endif
        endif
      elseif(airtight_type.eq.3)then
        airtight='loose airtightness'
      elseif(airtight_type.eq.4)then
        airtight='average airtightness'
      elseif(airtight_type.eq.5)then
        airtight='present airtightness'
      elseif(airtight_type.eq.6)then
        airtight='energy tight build quality'
      else
        airtight='unspecified airtightness'
      endif

      call edisp(itru,'  ')
      outs248 = ' '
      write(outs248,'(3a,f3.1,3a)')
     &  'Alberta Infil Method defined in ',
     &  LAIM2(1:lnblnk(LAIM2)),' version ',fAIM2VersionNumber,
     &  ' based on ',airtight(1:lnblnk(airtight)),'.'
      call edisp248(itru,outs248,90)

C Describe user defined leakage distribution.
      if(userspec_leakage.eq.1)then
        outs248 = ' '
        total=AIM2_ceil_frac+AIM2_wall_frac+AIM2_floor_frac
        if(total.gt.1.0)then
          write(outs248,'(a,f5.2,a,f5.2,a,f5.2,a)') 
     &      'The distribution of leakage has been set as: ceiling',
     &      AIM2_ceil_frac,' walls',AIM2_wall_frac,' floor',
     &      AIM2_floor_frac,' (will be normalised).'
        else
          write(outs248,'(a,f5.2,a,f5.2,a,f5.2,a)') 
     &      'The distribution of leakage has been set as: ceiling',
     &      AIM2_ceil_frac,' walls',AIM2_wall_frac,' floor',
     &      AIM2_floor_frac,'.'
        endif
        call edisp248(itru,outs248,90)
      endif

C Describe terrain at weather station and building site.
      if(AIM2_terrain_weather.eq.1)then
        w_terrain='open sea,fetch > 5 km'
      elseif(AIM2_terrain_weather.eq.2)then
        w_terrain='mud flats,no vegetation'
      elseif(AIM2_terrain_weather.eq.3)then
        w_terrain='open flat terrain, grass'
      elseif(AIM2_terrain_weather.eq.4)then
        w_terrain='low crops, x/H > 20'
      elseif(AIM2_terrain_weather.eq.5)then
        w_terrain='high crops, scattered obstacles'
      elseif(AIM2_terrain_weather.eq.6)then
        w_terrain='parkland, bushes, x/H ~ 10'
      elseif(AIM2_terrain_weather.eq.7)then
        w_terrain='suburban, forest '
      elseif(AIM2_terrain_weather.eq.8)then
        w_terrain='city centre '
      else
        w_terrain='unknown '
      endif
      if(AIM2_terrain_building.eq.1)then
        b_terrain='open sea,fetch > 5 km'
      elseif(AIM2_terrain_building.eq.2)then
        b_terrain='mud flats,no vegetation'
      elseif(AIM2_terrain_building.eq.3)then
        b_terrain='open flat terrain, grass'
      elseif(AIM2_terrain_building.eq.4)then
        b_terrain='low crops, x/H > 20'
      elseif(AIM2_terrain_building.eq.5)then
        b_terrain='high crops, scattered obstacles'
      elseif(AIM2_terrain_building.eq.6)then
        b_terrain='parkland, bushes, x/H ~ 10'
      elseif(AIM2_terrain_building.eq.7)then
        b_terrain='suburban, forest '
      elseif(AIM2_terrain_building.eq.8)then
        b_terrain='city centre '
      else
        b_terrain='unknown '
      endif

C Shielding around the building and flues.
      if(AIM2_wall_shielding.eq.1)then
        b_shielding='no local shielding of building'
      elseif(AIM2_wall_shielding.eq.2)then
        b_shielding='light local shielding of building'
      elseif(AIM2_wall_shielding.eq.3)then
        b_shielding='heavy shielding of building'
      elseif(AIM2_wall_shielding.eq.4)then
        b_shielding='very heavy shielding of building'
      elseif(AIM2_wall_shielding.eq.5)then
        b_shielding='complete shielding of building'
      else
        b_shielding='shielding unspecified'
      endif
      if(AIM2_flue_shielding.eq.1)then
        f_shielding='no local shielding of flues'
      elseif(AIM2_flue_shielding.eq.2)then
        f_shielding='light local shielding of flues'
      elseif(AIM2_flue_shielding.eq.3)then
        f_shielding='heavy shielding of flues'
      elseif(AIM2_flue_shielding.eq.4)then
        f_shielding='very heavy shielding of flues'
      elseif(AIM2_flue_shielding.eq.5)then
        f_shielding='complete shielding of flues'
      else
        f_shielding='shielding unspecified for flues'
      endif

      outs248 = ' '
      write(outs248,'(3a,f6.1,7a)')
     &  'Weather measurements taken ',w_terrain(1:lnblnk(w_terrain)),
     &  ' (wind @',AIM2_anemom_height,
     &  'm) and the building site is ',b_terrain(1:lnblnk(b_terrain)),
     &  ' with ',b_shielding(1:lnblnk(b_shielding)),
     &  ' and ',f_shielding(1:lnblnk(f_shielding)),'.'
      call edisp(itru,' ')
      call edisp248(itru,outs248,90)

C Describe the house type and levels and foundation.
      if(H2K_house_type.eq.1)then
        house='single detached'
      elseif(H2K_house_type.eq.2)then
        house='double/semi-detached'
      elseif(H2K_house_type.eq.3)then
        house='duplex'
      elseif(H2K_house_type.eq.4)then
        house='triplex'
      elseif(H2K_house_type.eq.5)then
        house='apartment unit (Individual)'
      elseif(H2K_house_type.eq.6)then
        house='row house, end unit'
      elseif(H2K_house_type.eq.7)then
        house='mobile home '
      elseif(H2K_house_type.eq.8)then
        house='row house, middle unit'
      else
        house='unknown building type'
      endif

C Set the stories/levels.
      if(H2K_storeys.eq.1)then
        stories='one storey'
      elseif(H2K_storeys.eq.2)then
        stories='one and a half'
      elseif(H2K_storeys.eq.3)then
        stories='two storeys'
      elseif(H2K_storeys.eq.4)then
        stories='two and a half'
      elseif(H2K_storeys.eq.5)then
        stories='three storeys'
      elseif(H2K_storeys.eq.6)then
        stories='split level'
      elseif(H2K_storeys.eq.7)then
        stories='split entry/raised basement'
      else
        stories='unknown levels'
      endif

C Describe foundations.
      if(H2K_found_num.gt.1)then
        do 42 ij=1,H2K_found_num
          if(H2K_found_type(ij).eq.1)then
            foundation='not applicable'
          elseif(H2K_found_type(ij).eq.2)then
            foundation='basement'
          elseif(H2K_found_type(ij).eq.3)then
            foundation='slab-on-grade'
          elseif(H2K_found_type(ij).eq.4)then
            foundation='closed crawl space'
          elseif(H2K_found_type(ij).eq.5)then
            foundation='vented crawl space'
          elseif(H2K_found_type(ij).eq.6)then
            foundation='open crawl space'
          elseif(H2K_found_type(ij).eq.7)then
            foundation='walkout basement'
          else
            foundation='unknown type of'
          endif
  42    continue
      else
        foundation='unspecified'
      endif

C Describe fireplaces.
      if(H2K_fireplace1.and.H2K_fireplace2)then
        write(fireplace,'(a,f6.2,a,f6.2,a,f6.2,a)')
     &    'There are two fireplaces with flue diam ',
     &    AIM2_fire_flue1,'mm and ',AIM2_fire_flue2,
     &    'mm. The furnace flue is ',AIM2_furnace_flue,'mm'
      elseif(H2K_fireplace1.or.H2K_fireplace2)then
        write(fireplace,'(a,f6.2,a,f6.2,a)')
     &   'There is a fireplace with flue diam ',
     &    AIM2_fire_flue1,'mm. The furnace flue is ',
     &    AIM2_furnace_flue,'mm'
      else
        write(fireplace,'(2a,f6.2,a)')
     &    'There is no fireplace in the building and the ',
     &    'furnace flue is ',AIM2_furnace_flue,'mm'
      endif

      outs248 = ' '
      write(outs248,'(9a)')
     &  'The building is a ',stories(1:lnblnk(stories)),' ',
     &  house(1:lnblnk(house)),' with a ',
     &  foundation(1:lnblnk(foundation)),' foundation. ',
     &  fireplace(1:lnblnk(fireplace)),'.'
      call edisp(itru,'  ')
      call edisp248(itru,outs248,90)

C Describe DHW options.
      if(H2K_DHW1_exist.and.H2K_DHW2_exist)then
        dhwphrase='there are two DHW systems'
      elseif(H2K_DHW1_exist.or.H2K_DHW2_exist)then
        dhwphrase='there is one DHW system'
      else
        dhwphrase='there is no DHW'
      endif

C Describe DHW and furnace flues.
      if(H2K_DHW1_shareflue.and.H2K_DHW2_shareflue)then
        if(H2K_DHW1_exist.and.H2K_DHW2_exist)then
          dhwshare='both primary and seconday DHW share furnace flue'
        elseif(H2K_DHW1_exist.or.H2K_DHW2_exist)then
          dhwshare='primary DHW shares furnace flue'
        endif
      elseif(H2K_DHW1_shareflue.and.(.NOT.H2K_DHW2_shareflue))then
        dhwshare='primary DHW shares furnace flue'
      elseif(.NOT.H2K_DHW1_shareflue.and.H2K_DHW2_shareflue)then
        dhwshare='secondary DHW shares furnace flue'
      else
        dhwshare='DHW has separate or no flue'
      endif

C Describe if DHW is inside or outside the house.
      if(H2K_DHW1_inside.and.H2K_DHW2_inside)then
        if(H2K_DHW1_exist.and.H2K_DHW2_exist)then
          dhwinside='both primary and seconday DHW are inside'
        elseif(H2K_DHW1_exist.or.H2K_DHW2_exist)then
          dhwinside='primary DHW is inside'
        endif
      elseif(H2K_DHW1_inside.and.(.NOT.H2K_DHW2_inside))then
        dhwinside='primary DHW is inside'
      elseif(.NOT.H2K_DHW1_inside.and.H2K_DHW2_inside)then
        dhwinside='secondary DHW is inside'
      else
        dhwinside='DHW is outside'
      endif

C Is the DHW tied in with a solar collector.
      if(H2K_DHW1_solar)then
        dhwsolar='A solar collector feeds the DHW.'
      else
        dhwsolar='.'
      endif

      outs248 = ' '
      write(outs248,'(8a)') 'Hot water: ',
     &  dhwphrase(1:lnblnk(dhwphrase)),' where ',
     &  dhwshare(1:lnblnk(dhwshare)),'. Location: ',
     &  dhwinside(1:lnblnk(dhwinside)),' ',
     &  dhwsolar(1:lnblnk(dhwsolar))
      call edisp248(itru,outs248,90)

C Describe crawl space, attic and controls.
      if(AIM2_CrawlSpace_zone.eq.0)then
        crawl='There is no crawl space.'
      else
        llna=lnzname(AIM2_CrawlSpace_zone)
        write(crawl,'(3a)') 
     &    'Zone ',zname(AIM2_CrawlSpace_zone)(1:llna),
     &    ' is the crawl space.'
      endif
      if(AIM2_attic_zone.eq.0)then
        attic='There is no attic space.'
      else
        llna=lnzname(AIM2_attic_zone)
        write(attic,'(3a)') 
     &    'Zone ',zname(AIM2_attic_zone)(1:llna),
     &    ' is the attic space.'
      endif

      outs248 = ' '
      llna=lnzname(AIM2_ref_zone)
      write(outs248,'(3a,i2,4a)') 
     &  'Indoor air temperatures are taken from ',
     &  zname(AIM2_ref_zone)(1:llna),
     &  ' and infiltration is applied to ',AIM2_num_zones_infil,
     &  ' zones. ',crawl(1:lnblnk(crawl)),' ',attic(1:lnblnk(attic))
      call edisp248(itru,outs248,90)

      return
      END subroutine AIM2_report


C ******************** AIM2_Deallocate ****************************
C *********************************************************************
C Created by: Francesca Macdonald
C Date: 23/07/2010
C 
C AIM2_Deallocate deallocates the memory asociated with
C the allocatable arrays in the AIM2 model
C AIM2_infil_to_zones
C AIM2_zone_infil
C *********************************************************************
      SUBROUTINE AIM2_Deallocate
      
      IMPLICIT NONE

C Declare local variables
      INTEGER :: deallocatestatus
C Common blocks
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: IUOUT,IUIN

      IF (ALLOCATED(AIM2_infil_to_zones)) THEN
C Debig AIM2      
C        WRITE(*,*)'AIM2_infil_to_zones',AIM2_infil_to_zones      	
	DEALLOCATE(AIM2_infil_to_zones, STAT=deallocatestatus)
        IF (deallocatestatus.NE.0) THEN
          CALL EDISP(IUOUT,
     &       ' Problem with deallocation of AIM2_infil_to_zones array')
        END IF
      END IF

      IF (ALLOCATED(AIM2_zone_infil)) THEN
C Debig AIM2      
C        WRITE(*,*)'AIM2_zone_infil',AIM2_zone_infil      	
        DEALLOCATE(AIM2_zone_infil, STAT=deallocatestatus)
        IF (deallocatestatus.NE.0) THEN
          CALL EDISP(IUOUT,
     &       ' Problem with deallocation of AIM2_infil_to_zones array')
        END IF
      END IF

      END SUBROUTINE AIM2_Deallocate
       
C ******************** AIM2_ClearAll ****************************
C *********************************************************************
C Created by: Francesca Macdonald
C Date: 15/08/2010
C 
C ClearAim2 resets all the aim2 parameters, used when a new model is loaded
C *********************************************************************
      SUBROUTINE AIM2_ClearAll
 
      IMPLICIT NONE
      
      CALL Initialize_AIM2_Params
      CALL AIM2_H2KDefaults    
      
      END SUBROUTINE AIM2_ClearAll
 
      END MODULE AIM2
