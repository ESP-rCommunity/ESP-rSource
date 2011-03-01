C This file is part of the ESP-r system.
C Copyright Sustainable Building Energy Systems Laboratory, Carleton University.
C Please contact Ian Beausoleil-Morrison for details concerning licensing.

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

C This file contains the following routines related to the interface
C of the AIM-2 infiltration model.

C ************************ MODULE AIM2_INPUTS_InputData*******************************************
C Created by: Francesca Macdonald
C
C Initialise        - contains the exact same data as in AIM2_InputData Module.
C CheckForChanges   - checks if the interface has been used to make any changes
C                     to the AIM2 parameters, so it can promt the user to save
C                     changes if they have been made.
C Deallocate        - deallocates any memory allocated to iAIM2_infil_to_zones
C
C ********************************************************************************
      MODULE AIM2_INPUTS_InputData

      USE AIM2_InputData
      IMPLICIT NONE

      SAVE !ensures values do not change between sucessive invocations

C---------------------------------------------------------------------------------
C Declare parameters - For a detailed description of each parameter see aim2_module
C The naming convention here is to put an i (for input) infront of each aim2_input
C parameter.
C---------------------------------------------------------------------------------
      INTEGER :: iairtight_type
      INTEGER :: iblower_door_input_option
      REAL :: iACH_50Pa
      REAL :: iELA_deltaP
      INTEGER :: iELA_given
      REAL :: iELA_cm
      REAL :: iELA_Cd
      INTEGER :: iuserspec_leakage
      REAL :: iAIM2_ceil_frac,iAIM2_wall_frac,iAIM2_floor_frac
      INTEGER :: iAIM2_terrain_weather
      INTEGER :: iAIM2_terrain_building
      INTEGER :: iAIM2_wall_shielding
      INTEGER :: iAIM2_flue_shielding
      REAL :: iAIM2_anemom_height
      REAL :: iAIM2_eaves_height
      REAL :: iAIM2_furnace_flue
      REAL :: iAIM2_fire_flue1
      REAL :: iAIM2_fire_flue2
      REAL :: iAIM2_DHW_flue1
      REAL :: iAIM2_DHW_flue2
      INTEGER :: iAIM2_ref_zone
      INTEGER :: iAIM2_num_zones_infil
      INTEGER, ALLOCATABLE, DIMENSION(:) :: iAIM2_zone_infil
      INTEGER :: iAIM2_basement_zone
      INTEGER :: iAIM2_CrawlSpace_zone
      INTEGER :: iAIM2_attic_zone

C Add Proceedures
      CONTAINS
C *******************************************************************************
C Initialise() sets all the AIM2_INPUTS_InputData to AIM2_InputData
C *******************************************************************************
      SUBROUTINE Initialise()

      IMPLICIT NONE
C Declare local variables
      INTEGER :: i                          !loop index
      INTEGER :: allocate_status            !returns 0 for sucessful allocation
C Common blocks
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: IUOUT,IUIN

      iairtight_type=airtight_type
      iblower_door_input_option=blower_door_input_option
      iACH_50Pa=ACH_50Pa
      iELA_deltaP=ELA_deltaP
      iELA_given=ELA_given
      iELA_cm=ELA_cm
      iELA_Cd=ELA_Cd
      iuserspec_leakage=userspec_leakage
      iAIM2_ceil_frac=AIM2_ceil_frac
      iAIM2_wall_frac=AIM2_wall_frac
      iAIM2_floor_frac=AIM2_floor_frac
      iAIM2_terrain_weather=AIM2_terrain_weather
      iAIM2_terrain_building=AIM2_terrain_building
      iAIM2_wall_shielding=AIM2_wall_shielding
      iAIM2_flue_shielding=AIM2_flue_shielding
      iAIM2_anemom_height=AIM2_anemom_height
      iAIM2_eaves_height=AIM2_eaves_height
      iAIM2_furnace_flue=AIM2_furnace_flue
      iAIM2_fire_flue1=AIM2_fire_flue1
      iAIM2_fire_flue2=AIM2_fire_flue2
      iAIM2_DHW_flue1=AIM2_DHW_flue1
      iAIM2_DHW_flue2=AIM2_DHW_flue2
      iAIM2_ref_zone=AIM2_ref_zone
      iAIM2_num_zones_infil=AIM2_num_zones_infil
C Allocate memory for iAIM2_zone_infil
      IF (.NOT.ALLOCATED(iAIM2_zone_infil)) THEN
        ALLOCATE(iAIM2_zone_infil(iAIM2_num_zones_infil),
     &       STAT=allocate_status)
C Check allocation was sucessful
        IF (.NOT.ALLOCATED(iAIM2_zone_infil)) THEN
          CALL EDISP(IUOUT,' iAIM2_zone_infil not allocated')
        ELSE
          DO i=1,iAIM2_num_zones_infil
            iAIM2_zone_infil(i)=AIM2_zone_infil(i)
          END DO
        END IF
      END IF
      iAIM2_basement_zone=AIM2_basement_zone
      iAIM2_CrawlSpace_zone=AIM2_CrawlSpace_zone
      iAIM2_attic_zone=AIM2_attic_zone

      END SUBROUTINE Initialise

C *******************************************************************************
C CheckForChanges() compares AIM2_InputData with AIM2_INPUTS_InputData to see
C if the user has changed any parameters.
C *******************************************************************************

      LOGICAL FUNCTION CheckForChanges()

      IMPLICIT NONE

C Function declaration
      LOGICAL :: REQUAL

      CheckForChanges = .FALSE.  !flag for changes

      IF (iairtight_type.NE.airtight_type) CheckForChanges = .TRUE.
      IF (iblower_door_input_option.NE.blower_door_input_option)
     &   CheckForChanges = .TRUE.
      IF (.NOT.REQUAL(iACH_50Pa,ACH_50Pa,.001))CheckForChanges = .TRUE.
      IF (.NOT.REQUAL(iELA_deltaP,ELA_deltaP,.001))
     &   CheckForChanges = .TRUE.
      IF (iELA_given.NE.ELA_given) CheckForChanges= .TRUE.
      IF (.NOT.REQUAL(iELA_cm,ELA_cm,.001)) CheckForChanges = .TRUE.
      IF (.NOT.REQUAL(iELA_Cd,ELA_Cd,.001)) CheckForChanges = .TRUE.
      IF (iuserspec_leakage.NE.userspec_leakage)
     &   CheckForChanges = .TRUE.
      IF (.NOT.REQUAL(iAIM2_ceil_frac,AIM2_ceil_frac,.001))
     &   CheckForChanges = .TRUE.
      IF (.NOT.REQUAL(iAIM2_wall_frac,AIM2_wall_frac,.001))
     &   CheckForChanges = .TRUE.
      IF (.NOT.REQUAL(iAIM2_floor_frac,AIM2_floor_frac,.001))
     &   CheckForChanges = .TRUE.
      IF (iAIM2_terrain_weather.NE.AIM2_terrain_weather)
     &    CheckForChanges = .TRUE.
      IF (iAIM2_terrain_building.NE.AIM2_terrain_building)
     &    CheckForChanges = .TRUE.
      IF (iAIM2_wall_shielding.NE.AIM2_wall_shielding)
     &    CheckForChanges = .TRUE.
      IF (iAIM2_flue_shielding.NE.AIM2_flue_shielding)
     &    CheckForChanges = .TRUE.
      IF (.NOT.REQUAL(iAIM2_anemom_height,AIM2_anemom_height,.001))
     &    CheckForChanges = .TRUE.
      IF (.NOT.REQUAL(iAIM2_eaves_height,AIM2_eaves_height,.001))
     &   CheckForChanges = .TRUE.
      IF (.NOT.REQUAL(iAIM2_furnace_flue,AIM2_furnace_flue,.001))
     &   CheckForChanges = .TRUE.
      IF (.NOT.REQUAL(iAIM2_fire_flue1,AIM2_fire_flue1,.001))
     &   CheckForChanges = .TRUE.
      IF (.NOT.REQUAL(iAIM2_fire_flue2,AIM2_fire_flue2,.001))
     &   CheckForChanges = .TRUE.
      IF (.NOT.REQUAL(iAIM2_DHW_flue1,AIM2_DHW_flue1,.001))
     &   CheckForChanges = .TRUE.
      IF (.NOT.REQUAL(iAIM2_DHW_flue2,AIM2_DHW_flue2,.001))
     &   CheckForChanges = .TRUE.
      IF (iAIM2_ref_zone.NE.AIM2_ref_zone) CheckForChanges = .TRUE.
      IF (iAIM2_num_zones_infil.NE.AIM2_num_zones_infil)
     &   CheckForChanges = .TRUE.
      IF (iAIM2_basement_zone.NE.AIM2_basement_zone)
     &   CheckForChanges = .TRUE.
      IF (iAIM2_CrawlSpace_zone.NE.AIM2_CrawlSpace_zone)
     &   CheckForChanges = .TRUE.
      IF (iAIM2_attic_zone.NE.AIM2_attic_zone) CheckForChanges = .TRUE.

      END FUNCTION CheckForChanges



C *******************************************************************************
C Deallocate() deallocates any memory allocated to iAIM2_infil_to_zones
C *******************************************************************************
      SUBROUTINE Deallocate

C Declare local variables
      INTEGER :: deallocatestatus
C Common blocks
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: IUOUT,IUIN

      IF (ALLOCATED(iAIM2_zone_infil)) THEN
        DEALLOCATE(iAIM2_zone_infil, STAT=deallocatestatus)
C Check allocation was sucessful
        IF (deallocatestatus.NE.0) THEN
          CALL EDISP(IUOUT,
     &       ' Problem with deallocation of AIM2_infil_to_zones array')
        END IF
      END IF
      END SUBROUTINE Deallocate

      END MODULE AIM2_INPUTS_InputData

C ********************************************************************************
C ********************************************************************************
C ********************************************************************************
C Created by: Francesca Macdonald
C This file contains the following subroutines:
C   AIM2_Interface - responsible for enabling the creation & editing
C                    of the .AIM file
C   Toplevel_Menu - function displays top level menu, and returns
C                   the menu selection (avoiding gotos)
C   AIM2_Interface - provides access to the Leakage menus, Shielding
C                    and terrain menus and InfiltrationZonesMenu as
C                    well as enabling the user to specify Building eves
C                    height and Flue diameters
C   ClearLeakageParams - clears previously defined blower door parameters
C   BlowerDoorInputMenu - displays the Blower-door results input menu allowing
C                         the user to iput the required blower door data.
C   LeakageMenu - allows the user to specify whether the leakage description
C                 is from a blower door test (CGSB), a blower door test as
C                 operated oran Air tightness type
C   AirTightnessTypeMenu - displays the options for specifying air tightness
C   LeakageDistribution - allows the user to iput the leakage fractions for
C                         ceiling, wall, and floor
C   ShieldingMenu - displays the shielding menu used to select the local
C                   shielding on the walls and flue
C   TerrainMenu - displays the terrain menu used to select the terrain flag
C                 for the weather station and for the building site.
C                 by zones tocheck for duplicates.
C   InfiltrationZonesMenu - allows the user to specify the zones used in the
C                           infiltration calculations.
C   SAVE_AIM2 - saves the input data required by the AIM-2 infiltration
C               model (i.e. the contents of the .aim file).

C *********************************************************************
C Toplevel_Menu() displays the top level menu and returns the selection
C *********************************************************************
      MODULE AIM2_Inputs

      USE AIM2_InputData
      USE AIM2_CalcData, ONLY:AIM2_infil_to_zones,AIM2_CO,AIM2_n
      USE AIM2, ONLY:Initialize_AIM2_Params,AIM2_H2KDefaults,
     &    AIM2_report,AIM2_Deallocate


C Add Proceedures
      CONTAINS
      INTEGER FUNCTION Toplevel_Menu()

      IMPLICIT NONE

C Declare local variables
      CHARACTER, DIMENSION(10) :: ITEM*34        !Array holding strings for menu items
      INTEGER :: IER                             !errorflag
      INTEGER :: NITEMS                          !number of items

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='AIM2_Inputs'  ! set for subroutine

C Toplevel_Menu needs to be set to -4 for the menu to be displayed in text mode
      Toplevel_Menu=-4

      ITEM(1)='a Leakage description'
      ITEM(2)='b Leakage distribution'
      ITEM(3)='c Shielding and terrain '
      ITEM(4)='d Building eves description'
      ITEM(5)='e Flue diameters'
      ITEM(6)='f Coupling to zone model'
      ITEM(7)=' _________________________ '
      ITEM(8)='! save .AIM file           '
      ITEM(9)='? help           '
      ITEM(10)='- exit this menu'
      NITEMS=10

      helptopic='AIM2_main_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('Empirical Air Infiltration Model',ITEM,NITEMS,
     &            Toplevel_Menu)

      IF (Toplevel_Menu.EQ.NITEMS-1) THEN
C List help text for the top level menu.
        helptopic='AIM2_main_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('AIM2 model menu',nbhelp,'-',0,0,IER)
      END IF
      IF (Toplevel_Menu.EQ.NITEMS) THEN
C Exit this menu
           CALL USRMSG(' ',' ','-')
      END IF

      END FUNCTION Toplevel_Menu


C *********************************************************************
C Created by: Francesca Macdonald
C AIM2_Interface responsible for enabling the creation & editing
C of the .AIM file by providing access to the Leakage menus, Shielding
C and terrain menus and InfiltrationZonesMenu as well as enabling the
C user to specify Building eves height and Flue diameters
C *********************************************************************
      SUBROUTINE AIM2_Interface()

      USE AIM2_INPUTS_InputData

      IMPLICIT NONE
C Declare local variables
      INTEGER :: IER                        !errorflag
      INTEGER :: Selection                  !top level menu choice
      INTEGER :: Selection2                 !lower level menu choice
      REAL :: Temp                          !temporary real value
      CHARACTER ::  DisplayString*248       !data selection display string
      LOGICAL :: OK                         !yes/no flag for saving changes
C Common blocks
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: IUOUT,IUIN

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='AIM2_Interface'  ! set for subroutine

      IF (.NOT.AIM2_Initialized) THEN
C Initialze all input parameters
        CALL Initialize_AIM2_Params()
C Initialze generic house data required by the AIM-2 model
        CALL AIM2_H2KDefaults()
      END IF

C Copy all parameters to temporary variables to enable checking for changes
      CALL Initialise

C Setup help text for the dialogs.
      helptopic='AIM2_attributes_a'
      call gethelptext(helpinsub,helptopic,nbhelp)

      DO
        Selection = Toplevel_Menu()
        SELECT CASE (Selection)

C Leakage description.
        CASE (1)
          CALL  LeakageMenu(Selection2)
          IF (Selection2.LT.3) THEN
            CALL BlowerDoorInputMenu()
          ELSE IF (Selection2.EQ.3) THEN
            CALL AirTightnessTypeMenu()
          ENDIF

          CASE (2)
          CALL LeakageDistribution()

        CASE (3)
C Keep displaying the shielding menu until exit is selected.
          DO
            CALL ShieldingAndTerrainMenu(Selection2)
             IF (Selection2.EQ.1) THEN
              CALL TerrainMenu(AIM2_terrain_weather,
     &        'Terrain at weather station    ')
            ELSE IF (Selection2.EQ.2) THEN
              CALL TerrainMenu(AIM2_terrain_building,
     &        'Terrain at building           ')
            ELSE IF (Selection2.EQ.3) THEN
              CALL ShieldingMenu(AIM2_wall_shielding,
     &        'Shielding on walls            ')
            ELSE IF (Selection2.EQ.4) THEN
              CALL ShieldingMenu(AIM2_flue_shielding,
     &        'Shielding on flue             ')

C Height of anemometer at weather station.
            ELSE IF (Selection2.EQ.5) THEN
              Temp = AIM2_anemom_height
              CALL EASKR(Temp,' ',
     &         'Height of anenometor at weather station (m)?',0.1,'F',
     &         100.0,'F',10.0, 'Amenometer height',IER,nbhelp)
              IF (IER.EQ.0) AIM2_anemom_height = Temp
              WRITE(DisplayString,'(A, F8.2, A)')
     &          'Height of anenometor: ',AIM2_anemom_height, ' m'
              CALL EDISP(IUOUT,DisplayString)
            ELSE IF (Selection2.EQ.7) THEN
              EXIT
            ENDIF
          END DO

        CASE (4)
C Building eves description.
           Temp = AIM2_eaves_height
           CALL EASKR(Temp,' ',
     &        'Height of the building eaves (m)?',1.,'F',10.0,
     &        'F',5.0,'Eaves height',IER,nbhelp)
           IF (IER.EQ.0) AIM2_eaves_height = Temp
           WRITE(DisplayString,'(A, F8.2, A)')'Building eaves height: ',
     &             AIM2_eaves_height, ' m'
           CALL EDISP(IUOUT,DisplayString)

        CASE (5)
C Diameter of furnace flue (mm).
           Temp = AIM2_furnace_flue
           CALL EASKR(Temp,' ','Diameter of furnace flue (mm)?',
     &       0.,'F',500.0,'F',200.0,'Furnace flue diameter',
     &       IER,nbhelp)
           IF (IER.EQ.0) AIM2_furnace_flue = Temp
           WRITE(DisplayString,'(A, F8.2, A)')'Furnace flue diameter: ',
     &             AIM2_furnace_flue, ' mm'
           CALL EDISP(IUOUT,DisplayString)

C Diameter of #1 fireplace flue (mm).
           Temp = AIM2_fire_flue1
           CALL EASKR(Temp,' ','Diameter of #1 fireplace flue (mm)?',
     &       0.,'F',500.0,'F',200.0,'Fireplace #1 flue diameter',
     &       IER,nbhelp)
           IF (IER.EQ.0) AIM2_fire_flue1 = Temp
           WRITE(DisplayString,'(A, F8.2, A)')
     &      'Fireplace #1 flue diameter: ', AIM2_fire_flue1, ' mm'
           CALL EDISP(IUOUT,DisplayString)

C Diameter of #2 fireplace flue (mm).
           Temp = AIM2_fire_flue2
           CALL EASKR(Temp,' ','Diameter of #2 fireplace flue (mm)?',
     &       0.,'F',500.0,'F',200.0,'Fireplace #2 flue diameter',
     &       IER,nbhelp)
           IF (IER.EQ.0) AIM2_fire_flue2 = Temp
           WRITE(DisplayString,'(A, F8.2, A)')
     &      'Fireplace #2 flue diameter: ',AIM2_fire_flue2, ' mm'
           CALL EDISP(IUOUT,DisplayString)

C Diameter of primary DHW system flue (mm).
           Temp = AIM2_DHW_flue1
           CALL EASKR(Temp,' ',
     &       'Diameter of primary DHW system flue (mm)?',0.,'F',
     &       500.0,'F',200.0,'Primary DHW system flue diameter',
     &       IER,nbhelp)
           IF (IER.EQ.0) AIM2_DHW_flue1 = Temp
           WRITE(DisplayString,'(A, F8.2, A)')
     &       'Primary DHW system flue diameter: ',AIM2_DHW_flue1,' mm'
           CALL EDISP(IUOUT,DisplayString)

C Diameter of secondary DHW system flue (mm).
           Temp = AIM2_DHW_flue2
           CALL EASKR(Temp,' ',
     &       'Diameter of Secondary DHW system flue (mm)?',0.,'F',
     &       500.0,'F',200.0,'Secondary DHW system diameter',
     &       IER,nbhelp)
           IF (IER.EQ.0) AIM2_DHW_flue2 = Temp
           WRITE(DisplayString,'(A, F8.2, A)')
     &       'Secondary DHW system flue diameter: ',AIM2_DHW_flue2,
     &       ' mm'
           CALL EDISP(IUOUT,DisplayString)

        CASE (6)
          CALL InfiltrationZonesMenu()
        CASE (8)
C Save AIM2 file
          CALL SAVE_AIM2
C Save all parameters to temporary variables to enable checking for
C any further changes
          CALL Initialise
        CASE (10)
C Check to see if any of the paramets have changed before exiting
          IF (CheckForChanges()) THEN
C Check if the user wants to save the changes made
            CALL ASKOK(' Do you wish to save your changes?',' ',OK,
     &                .TRUE.,nbhelp)
            IF (OK) CALL SAVE_AIM2
          END IF
          EXIT
        END SELECT
      END DO

      END SUBROUTINE AIM2_Interface

C *********************************************************************
C Created by: Francesca Macdonald
C ClearLeakageParams clears previously defined blower door parameters
C *********************************************************************
      SUBROUTINE ClearLeakageParams()

      ACH_50Pa = 0.
      ELA_deltaP = 0.
      ELA_given = 0
      ELA_cm = 0.
      ELA_Cd = 0.
      airtight_type = 0
      blower_door_input_option = 0

      END SUBROUTINE ClearLeakageParams

C *********************************************************************
C Created by: Francesca Macdonald
C LeakageMenu allows the user to specify whether the leakage description
C is from a blower door test (CGSB), a blower door test as operated or
C an Air tightness type

C *********************************************************************
      SUBROUTINE LeakageMenu(Selection)

      IMPLICIT NONE
C Declare calling parameter types
      INTEGER, INTENT(OUT)  :: Selection        !menu selection
C Declare local variables
      CHARACTER, DIMENSION(5) :: ITEM*34        !Array holding strings for menu items
      INTEGER :: IER                            !errorflag
      INTEGER :: NITEMS                         !number of items

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='LeakageMenu'  ! set for subroutine


C Help text for this menu.
      helptopic='AIM_leakage_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

      IF (airtight_type.EQ.1) THEN
        ITEM(1)='a Blower door test (CGSB) ***'
      ELSE 
        ITEM(1)='a Blower door test (CGSB)  '
      ENDIF

      IF (airtight_type.EQ.2) THEN
        ITEM(2)='b Blower door test (operated) ***'
      ELSE
        ITEM(2)='b Blower door test (as operated)'
      ENDIF

      IF (airtight_type.GT.2) THEN
         WRITE(ITEM(3),'(A, I1, A)')
     &          'c Air tightness type (',airtight_type,')'
      ELSE
        ITEM(3)='c Air tightness type         '
      END IF

      ITEM(4)='? Help         '
      ITEM(5)='- Exit         '
      NITEMS=5

C Selection needs to be set to -4 for the menu to be displayed in text mode
      Selection = -4

      CALL EMENU(' Leakage description',ITEM,NITEMS,Selection)
      IF (Selection.EQ.NITEMS-1) THEN! help
C List help text for the Leakage description menu.
        CALL PHELPD('Leakage description',nbhelp,'-',0,0,IER)
      ELSE IF (Selection.EQ.NITEMS) THEN
C Exit this menu
        CALL USRMSG(' ',' ','-')
      END IF

      END SUBROUTINE LeakageMenu

C *********************************************************************
C Created by: Francesca Macdonald
C AirTightnessTypeMenu displays the options for specifying air tightness
C Air tightness type as defined in HOT2000 interface:
C 3=loose; 4=average; 5=present; 6=energy tight.

C *********************************************************************
      SUBROUTINE AirTightnessTypeMenu()

      IMPLICIT NONE
C Declare local variables
      CHARACTER, DIMENSION(6) :: ITEM*34        !Array holding strings for menu items
      CHARACTER :: DisplayString*248            !data selection display string
      INTEGER :: IER                            !errorflag
      INTEGER :: NITEMS                         !number of items
      INTEGER :: INO                            !selection
C Common blocks
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: IUOUT,IUIN

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='AirTightnessTypeMenu'  ! set for subroutine

C Help text for this menu.
      helptopic='AIM_tightness_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

      IF (airtight_type-2.EQ.1) THEN
        ITEM(1)='a Loose          ***'
      ELSE
        ITEM(1)='a Loose             '
      END IF

      IF (airtight_type-2.EQ.2) THEN
        ITEM(2)='b Average        ***'
      ELSE
        ITEM(2)='b Average           '
      END IF

      IF (airtight_type-2.EQ.3) THEN
        ITEM(3)='c Present        ***'
      ELSE
        ITEM(3)='c Present           '
      END IF

      IF (airtight_type-2.EQ.4) THEN
         ITEM(4)='d Energy Tight   ***'
      ELSE
         ITEM(4)='d Energy Tight      '
      END IF

      ITEM(5)='? Help              '
      ITEM(6)='- Exit              '
      NITEMS=6

C INO needs to be set to -4 for the menu to be displayed in text mode
      INO = -4
      CALL EMENU(' Air tightness type',ITEM,NITEMS,INO)
      IF (INO.LE.4) THEN
        IF (INO.NE.airtight_type+2) THEN
          CALL ClearLeakageParams()
        END IF
        airtight_type = INO+2
        WRITE(DisplayString,'(A, A)')'Air tightness type: ',
     &         ITEM(INO)
        CALL EDISP(IUOUT,DisplayString)
      ELSE IF (INO.EQ.NITEMS-1) THEN
C List help text for the Leakage description menu.
        CALL PHELPD('Leakage description',nbhelp,'-',0,0,IER)
      ELSE IF (INO.EQ.NITEMS) THEN
C Exit this menu
        CALL USRMSG(' ',' ','-')
      END IF

      END SUBROUTINE AirTightnessTypeMenu

C *********************************************************************
C Created by: Francesca Macdonald
C BlowerDoorInputMenu displays the Blower-door results input menu allowing
C the user to iput the required blower door data.
C Three options are available for the blower-door results inputs
C Option 1: flow coefficient C0 [m^3/(s Pa^n)]
C           flow exponent n
C Option 2: Air change rate @ 50Pa
C           Pressure difference in Pa
C Option 3: Air change rate @ 50Pa
C           Pressure difference in Pa
C           Equivalent leakage area in cm^2
C           Discharge coefficient Cd used to calculate leakage area

C *********************************************************************
      SUBROUTINE BlowerDoorInputMenu()

      IMPLICIT NONE
C Declare local variables
      CHARACTER, DIMENSION(5) :: ITEM*34        !Array holding strings for menu items
      CHARACTER :: DisplayString*248            !data selection display string
      INTEGER :: IER                            !errorflag
      INTEGER :: NITEMS                         !number of items
      INTEGER :: INO                            !selection
      REAL :: Temp                              !temporary variable
C Common blocks
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: IUOUT,IUIN

C Function definition
      LOGICAL :: REQUAL                         !used to compare real variables

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='BlowerDoorInputMenu'  ! set for subroutine

C Help text for this menu and for the dialogs in the menu.
C << more help text would be useful! >>
      helptopic='AIM_blower_door_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

      IF (blower_door_input_option.EQ.1) THEN
        ITEM(1)='a C & n          ***'
      ELSE
        ITEM(1)='a C & n             '
      ENDIF

      IF (blower_door_input_option.EQ.2) THEN
        ITEM(2)='b ACH & delta P     ***'
      ELSE
        ITEM(2)='b ACH & delta P        '
      ENDIF

      IF (blower_door_input_option.EQ.3) THEN
         ITEM(3)='c ACH, delta P & ELA   ***'
      ELSE
         ITEM(3)='c ACH, delta P & ELA      '
      ENDIF


      DisplayString = 'Blower Door Results'
      CALL EDISP(IUOUT,DisplayString)

      ITEM(4)='? Help              '
      ITEM(5)='- Exit              '
      NITEMS=5

C INO needs to be set to -4 for the menu to be displayed in text mode
      INO = -4
      CALL EMENU(' Blower-door results input',ITEM,NITEMS,INO)
      IF (INO.NE.blower_door_input_option) THEN
        CALL ClearLeakageParams()
      END IF
      blower_door_input_option = INO


      IF (INO.EQ.1) THEN
C Get the flow coefficient and flow exponent
        Temp = AIM2_CO
        IF (REQUAL(Temp,0.,.001)) Temp=0.05
        CALL EASKR(Temp,' ',
     &    ' Building fabric flow coefficient [m^3/(s Pa^n)]? ',
     &    0.,'F',0.1,'W',0.05,'flow coefficient',IER,nbhelp)
        IF (IER.EQ.0) THEN
          AIM2_CO = Temp
          WRITE(DisplayString,'(A, F7.2, A)')
     &      'Building fabric flow coefficient ',AIM2_CO,
     &      ' [m^3/(s Pa^n)?]'
          CALL EDISP(IUOUT,DisplayString)
        ENDIF
        Temp = AIM2_n
        IF (REQUAL(Temp,0.,.001))Temp=0.67
        CALL EASKR(Temp,' ',
     &    ' Building flow exponent (dimensionless)? ',
     &     0.5,'F',1.,'W',0.67,'flow exponent',IER,nbhelp)
        IF (IER.EQ.0) THEN
          AIM2_n = Temp
          WRITE(DisplayString,'(A, F7.2)')
     &      'Building flow exponent (dimensionless) ',AIM2_n
          CALL EDISP(IUOUT,DisplayString)
        ENDIF

      ELSE IF (INO.LT.4) THEN   ! option 2 or 3 (both need air change rate @ 50Pa)
C Get air change rate @ 50Pa
        Temp = ACH_50Pa
        IF (REQUAL(Temp,0.,.001))Temp=15.
        CALL EASKR(Temp,' ',
     &     ' Air change rate @50 Pa? ',
     &     0.1,'F',30.0,'W',15.0,'air change rate',IER,nbhelp)
        IF (IER.EQ.0) THEN
          ACH_50Pa = Temp
          WRITE(DisplayString,'(A, F7.2)')
     &      'Air change rate @50 Pa ',ACH_50Pa
          CALL EDISP(IUOUT,DisplayString)
        ENDIF
C Get Pressure difference in Pa
        Temp = ELA_deltaP
        IF (REQUAL(Temp,0.,.001))Temp=10.
        CALL EASKR(Temp,' ','Pressure difference (Pa)?',
     &         4.0,'W',10.0,'W',10.0,'Pressure difference',
     &         IER,nbhelp)
        IF (IER.EQ.0) THEN
          ELA_deltaP = Temp
          WRITE(DisplayString,'(A, F7.2, A)')
     &       'Pressure difference ',ELA_deltaP,' Pa'
          CALL EDISP(IUOUT,DisplayString)
        ENDIF
        IF (INO.EQ.3) THEN
          Temp = ELA_cm
          IF (REQUAL(Temp,0.,.001))Temp=500.
          CALL EASKR(Temp,' ',
     &      'Equivalent air leakage area in (ELA) cm^2?',0.1,'F',7999.0
     &      ,'W',500.0,'Equivalent air leakage area',IER,nbhelp)
          IF (IER.EQ.0) THEN
            ELA_given = 1 !input given
            ELA_cm = Temp
             WRITE(DisplayString,'(A, F8.2, A)')
     &              'Equivalent air leakage area ',ELA_cm,' cm^2'
            CALL EDISP(IUOUT,DisplayString)
          ELSE
            ELA_given = 0 !no input
          ENDIF
          Temp = ELA_Cd
          IF (REQUAL(Temp,0.,.001))Temp=0.611
          CALL EASKR(Temp,' ',
     &      'Discharge coefficient for ELA?',0.,'F',1.
     &      ,'W',0.611,'Discharge coefficient',IER,nbhelp)
          IF (IER.EQ.0) THEN
            ELA_given = 1 !input given
            ELA_Cd = Temp
             WRITE(DisplayString,'(A, F8.2)')
     &              'Discharge coefficient for ELA ',ELA_Cd
            CALL EDISP(IUOUT,DisplayString)
          ELSE
            ELA_given = 0 !no input
          ENDIF
        ENDIF

      ELSE IF (INO.EQ.NITEMS-1) THEN     !help
C List help text for the Leakage description menu.
        CALL PHELPD('Blower door description',nbhelp,'-',0,0,IER)
      ELSE IF (INO.EQ.NITEMS) THEN
C Exit this menu
        CALL USRMSG(' ',' ','-')
      END IF

      END SUBROUTINE BlowerDoorInputMenu

C *********************************************************************
C Created by: Francesca Macdonald
C LeakageDistribution allows the user to iput the leakage fractions for ceiling, wall, and floor
C userspec_leakage - Flag indicating whether user has specified leakage fractions for ceiling,
C wall, and floor: 0 indicates user input not given; 1 indicates user input given.
C AIM2_ceil_frac,AIM2_wall_frac,AIM2_floor_frac - Ceiling, wall, and floor leakage fractions.

C *********************************************************************
      SUBROUTINE LeakageDistribution()

      IMPLICIT NONE

C Declare local variables
      CHARACTER :: DisplayString*248        !data selection display string
      INTEGER :: IER                        !errorflag
      INTEGER :: INO                        !selection
      REAL :: Temp                          !temporary variable
C Common blocks
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: IUOUT,IUIN

c Function declaration
      LOGICAL :: REQUAL                    !used to compare real variables

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='LeakageDistribution'  ! set for subroutine

C Help text for the leakage distribution
      helptopic='AIM_leak_distr_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EASKAB(
     &    'Specify leakage fractions for ceiling, wall and floor?',
     &    'Options:','No - use defaults','Yes',INO,nbhelp)

      userspec_leakage = INO-1

      IF (userspec_leakage.EQ.1) THEN
         !make sure the fractions add up to 1.0
        DO

C defaults assumed from http://www.inive.org/members_area/medias/pdf/Inive%5CIBPSA%5CUFSC425.pdf
          Temp = AIM2_ceil_frac
          CALL EASKR(Temp,' ',
     &       'What is the leakage fraction for the ceiling?',0.0,'F',
     &         1.0,'F',0.2,'ceiling leakage fraction',IER,nbhelp)
          IF (IER.EQ.0) AIM2_ceil_frac = Temp
          Temp = AIM2_wall_frac
          CALL EASKR(Temp,' ',
     &        'What is the leakage fraction for the wall?  ',0.0,'F',
     &         1.0,'F',0.65,'wall leakage fraction',IER,nbhelp)
          IF (IER.EQ.0) AIM2_wall_frac = Temp
          Temp = AIM2_floor_frac
          CALL EASKR(Temp,' ',
     &        'What is the leakage fraction for the floor? ',0.0,'F',
     &         1.0,'F',0.15,'floor leakage fraction',IER,nbhelp)

C check to ensure fractions add up to 1.
          IF (IER.EQ.0) AIM2_floor_frac = Temp
            Temp = AIM2_ceil_frac+AIM2_wall_frac+AIM2_floor_frac
          IF (REQUAL(Temp,1.,.001))THEN
            WRITE(DisplayString,'(A, F6.3, A, F6.3, A, F6.2)')
     &             ' Ceiling: ',AIM2_ceil_frac,
     &       '     Wall: ',AIM2_wall_frac,
     &       '     Floor: ',AIM2_floor_frac
            CALL EDISP(IUOUT,'Infiltration leakage fractions:')
            CALL EDISP(IUOUT,DisplayString)
            EXIT
          ENDIF
          CALL USRMSG(' ','The fractions must add up to 1.0 ','W')
        END DO
      ENDIF

      END SUBROUTINE LeakageDistribution


C *********************************************************************
C Created by: Francesca Macdonald
C ShieldingAndTerrainMenu enables to user to specify
C Terrain flag at weather station (AIM2_terrain_weather)
C Terrain flag at building site (AIM2_terrain_building)
C Flag indicating local shielding on walls (AIM2_wall_shielding)
C Flag indicating local shielding on flue (AIM2_flue_shielding)
C Height of anenometor at weather station (m).
C *********************************************************************
      SUBROUTINE ShieldingAndTerrainMenu(Selection)

      IMPLICIT NONE
C Declare calling parameters
      INTEGER, INTENT(OUT)  :: Selection
C Declare local variables
      CHARACTER, DIMENSION(7) :: ITEM*34        !Array holding strings for menu items
      CHARACTER :: DisplayString*248            !data selection display string
      INTEGER :: IER                            !errorflag
      INTEGER :: NITEMS                         !number of items
      INTEGER :: INO                            !menu selection
      REAL :: Temp                              !temporary variable
C Common blocks
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: IUOUT,IUIN

C Function declaration
      LOGICAL :: REQUAL                         !used to compare real variables

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='ShieldingAndTerrainMenu'  ! set for subroutine

C Help text for this menu.
      helptopic='AIM_terrain_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

      IF (AIM2_terrain_weather.NE.0) THEN
        WRITE(ITEM(1),'(A, I1, A)')'a Terrain at weather station  (',
     &                  AIM2_terrain_weather,')'
      ELSE
        ITEM(1)='a Terrain at weather station   '
      ENDIF

      IF (AIM2_terrain_building.NE.0) THEN
        WRITE(ITEM(2),'(A, I1, A)')'b Terrain at building         (',
     &                 AIM2_terrain_building,')'
      ELSE
        ITEM(2)='b Terrain at building          '
      ENDIF

      IF (AIM2_wall_shielding.NE.0) THEN
        WRITE(ITEM(3),'(A, I1, A)')'c shielding on walls          (',
     &                AIM2_wall_shielding,')'
      ELSE
        ITEM(3)='c shielding on walls       '
      ENDIF

      IF (AIM2_flue_shielding.NE.0) THEN
        WRITE(ITEM(4),'(A, I1, A)')'d shielding on flue           (',
     &                 AIM2_flue_shielding,')'
      ELSE
        ITEM(4)='d shielding on flue         '
      ENDIF

      IF (.NOT.REQUAL(AIM2_anemom_height,10.,.001)) THEN
        WRITE(ITEM(5),'(A, F6.2, A)')'e anemometer height  (',
     &                   AIM2_anemom_height,' (m))'
      ELSE
        ITEM(5)='e anemometer height     10m '
      ENDIF

      ITEM(6)='? Help              '
      ITEM(7)='- Exit         '
      NITEMS=7


C Because the selection needs to be set to -4 for the menu to be displayed in text mode
C the selection (if one was made) needs to be saved in case the user does not change their
C selection
      Selection = -4

      CALL EMENU(' Shielding and terrain description',ITEM,NITEMS,
     &          Selection)
      IF (Selection.EQ.NITEMS-1) THEN
C List help text for the Shielding and Terrain menu.
        CALL PHELPD('Shielding and terrain description',nbhelp,'-',
     &    0,0,IER)
      ELSE IF (Selection.EQ.NITEMS) THEN
C Exit this menu
        CALL USRMSG(' ',' ','-')
      END IF

      END SUBROUTINE ShieldingAndTerrainMenu


C *********************************************************************
C Created by: Francesca Macdonald
C TerrainMenu displays the terrain menu used to select the terrain flag
C for the weather station and for the building site.
C         1=Open sea,fetch > 5 km
C         2=Mud flats,no vegetation
C         3=Open flat terrain, grass
C         4=Low crops, x/H > 20
C         5=High crops, scattered obstacles
C         6=Parkland, bushes, x/H ~ 10
C         7=Suburban, forest
C         8=City centre
C *********************************************************************
      SUBROUTINE TerrainMenu(TerrainSelection, MenuString)

      IMPLICIT NONE
C Declare calling parameters
      INTEGER, INTENT(INOUT)  :: TerrainSelection            !menu selection
      CHARACTER, INTENT(IN)  :: MenuString*30                !string of menu item
C Declare local variables
      CHARACTER, DIMENSION(10) :: ITEM*34                    !Array holding strings for menu items
      CHARACTER :: DisplayString*248                         !data selection display string
      INTEGER :: IER                                         !errorflag
      INTEGER :: NITEMS                                      !number of items
      INTEGER :: PreviousSelection                           !previous menu selction
C Common blocks
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: IUOUT,IUIN

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='TerrainMenu'  ! set for subroutine

C Help text for this menu.
      helptopic='AIM_terrain_options'
      call gethelptext(helpinsub,helptopic,nbhelp)

      IF (TerrainSelection.EQ.1) THEN
        ITEM(1)='a Open sea,fetch > 5 km     ***'
      ELSE
        ITEM(1)='a Open sea,fetch > 5 km        '
      END IF 
      
      IF (TerrainSelection.EQ.2) THEN
        ITEM(2)='b Mud flats,no vegetation   ***'
      ELSE
        ITEM(2)='b Mud flats,no vegetation      '
      END IF 
      
      IF (TerrainSelection.EQ.3) THEN
        ITEM(3)='c Open flat terrain, grass  ***'
      ELSE
        ITEM(3)='c Open flat terrain, grass     '
      END IF 
      
      IF (TerrainSelection.EQ.4) THEN
        ITEM(4)='d Low crops, x/H > 20       ***'
      ELSE
        ITEM(4)='d Low crops, x/H > 20          '
      END IF 
      
      IF (TerrainSelection.EQ.5) THEN
        ITEM(5)='e High crops, scattered obst ***'
      ELSE
        ITEM(5)='e High crops, scattered obst    '
      END IF 
      
      IF (TerrainSelection.EQ.6) THEN
        ITEM(6)='f Parkland, bushes, x/H ~ 10 ***'
      ELSE
        ITEM(6)='f Parkland, bushes, x/H ~ 10    '
      END IF 
      
      IF (TerrainSelection.EQ.7) THEN
        ITEM(7)='g Suburban, forest           ***'
      ELSE
        ITEM(7)='g Suburban, forest              '
      END IF 
      
      IF (TerrainSelection.EQ.8) THEN
        ITEM(8)='h City centre                ***'
      ELSE
        ITEM(8)='h City centre                   '
      END IF 
     
      ITEM(9)='- Help         '
      ITEM(10)='- Exit         '
      NITEMS=10

C Because the selection needs to be set to -4 for the menu to be displayed in text mode
C the selection (if one was made) needs to be saved in case the user does not change their
C selection
      PreviousSelection = TerrainSelection
      TerrainSelection = -4

      CALL EMENU(MenuString,ITEM,NITEMS,TerrainSelection)

      IF (TerrainSelection.EQ.NITEMS-1) THEN
C List help text for the Shielding and Terrain menu.
        TerrainSelection = PreviousSelection
        CALL PHELPD('Terrain selection',nbhelp,'-',0,0,IER)
      ELSE IF (TerrainSelection.EQ.NITEMS) THEN
C Exit this menu
        TerrainSelection = PreviousSelection
        CALL USRMSG(' ',' ','-')
        RETURN
      ENDIF

      WRITE(DisplayString,'(A, A, A)')MenuString, ' ',
     &   ITEM(TerrainSelection)
      CALL EDISP(IUOUT,DisplayString)

      END SUBROUTINE TerrainMenu

C *********************************************************************
C Created by: Francesca Macdonald
C ShieldingMenu displays the shielding menu used to select the local
C shielding on the walls and flue
C         1=No local shielding
C         2=Light local shielding
C         3=Heavy
C         4=Very heavy
C         5=Complete (by large buildings)
C *********************************************************************
      SUBROUTINE ShieldingMenu(ShieldingSelection, MenuString)

      IMPLICIT NONE
C Declare calling parameters
      INTEGER, INTENT(INOUT)  :: ShieldingSelection          !menu selection
      CHARACTER, INTENT(IN)  :: MenuString*30                !string of menu item
C Declare local variables
      CHARACTER, DIMENSION(7) :: ITEM*34                     !Array holding strings for menu items
      CHARACTER :: DisplayString*248                         !data selection display string
      INTEGER :: IER                                         !errorflag
      INTEGER :: NITEMS                                      !number of items
      INTEGER :: PreviousSelection                           !previous menu selction
C Common blocks
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: IUOUT,IUIN

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='ShieldingMenu'  ! set for subroutine

C Help text for this menu.
      helptopic='AIM_shielding_options'
      call gethelptext(helpinsub,helptopic,nbhelp)

      IF (ShieldingSelection.EQ.1) THEN
        ITEM(1)='a None          ***'
      ELSE
        ITEM(1)='a None             '
      END IF

      IF (ShieldingSelection.EQ.2) THEN
        ITEM(2)='b Light        ***'
      ELSE
        ITEM(2)='b Light           '
      END IF

      IF (ShieldingSelection.EQ.3) THEN
        ITEM(3)='c Heavy       ***'
      ELSE
        ITEM(3)='c Heavy          '
      END IF

      IF (ShieldingSelection.EQ.4) THEN
        ITEM(4)='d Very Heavy            ***'
      ELSE
        ITEM(4)='d Very Heavy               '
      END IF

      IF (ShieldingSelection.EQ.5) THEN
        ITEM(5)='e Complete (large buildings) ***'
      ELSE
        ITEM(5)='e Complete (large buildings)    '
      END IF 
      
      ITEM(6)='- Help         '
      ITEM(7)='- Exit         '
      NITEMS=7

C Because the selection needs to be set to -4 for the menu to be displayed in text mode
C the selection (if one was made) needs to be saved in case the user does not change their
C selection
      PreviousSelection = ShieldingSelection
      ShieldingSelection = -4

      CALL EMENU(MenuString,ITEM,NITEMS,ShieldingSelection)

      IF (ShieldingSelection.EQ.NITEMS-1) THEN
C List help text for the Shielding and Terrain menu.
        ShieldingSelection = PreviousSelection
        CALL PHELPD('Shielding selection',nbhelp,'-',0,0,IER)
       ELSE IF (ShieldingSelection.EQ.NITEMS) THEN
C Exit this menu
        ShieldingSelection = PreviousSelection
        CALL USRMSG(' ',' ','-')
        RETURN
      ENDIF

      WRITE(DisplayString,'(A, A, A)')MenuString, ' ',
     &        ITEM(ShieldingSelection)
      CALL EDISP(IUOUT,DisplayString)

      END SUBROUTINE ShieldingMenu


C *********************************************************************
C Created by: Francesca Macdonald
C InfiltrationZonesMenu allows the user to specify the zones used in the
C infiltration calculations.
C Number of zone whose temperature is used as reference. (AIM2_ref_zone)
C Number of zones that receive infiltration.(AIM2_num_zones_infil)
C Matrix holding indices of zones that receive infiltration.(AIM2_zone_infil)
C Zone number of basement.(AIM2_basement_zone)
C Zone number of Crawl Space (AIM2_CrawlSpace_zone)
C Zone number of attic. (AIM2_attic_zone)
C *********************************************************************
      SUBROUTINE InfiltrationZonesMenu()

      IMPLICIT NONE
#include "building.h"
#include "geometry.h"

C Declare local variables
      INTEGER, DIMENSION(MCOM) :: ivals
      CHARACTER, DIMENSION(8) :: ITEM*34            !Array holding strings for menu items
      CHARACTER :: DisplayString*248                !data selection display string
      CHARACTER :: TempString*128                   !data selection display string
      INTEGER :: IER                                !errorflag
      INTEGER :: INO                                !Selection
      INTEGER :: zoneindex                          !index for do loop
      INTEGER :: Zone                               !current zone number
      INTEGER :: allocate_status                    !returns 0 for sucessful allocation
      INTEGER :: NITEMS                             !number of items
      LOGICAL :: OK                                 !yes/no flag for reverting basement, crawlspace & attic selections
C Common blocks
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: IUOUT,IUIN
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL :: CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK  !CFGOK flag for wether cfg file has been loaded in
      COMMON/C1/NCOMP,NCON
      INTEGER :: NCOMP,NCON                         !ncomp is the number of zones

      INTEGER, DIMENSION(NCOMP) :: InfilZones       !Temporary array to hold zones to recieve infiltration

C Function definition
      INTEGER :: lnblnk

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='InfiltrationZonesMenu'  ! set for subroutine

      InfilZones=0

      ITEM(1)='a Reference zone         '
      ITEM(2)='b Infiltration zones '
      ITEM(3)='c Basement zone '
      ITEM(4)='d Crawl space zone '
      ITEM(5)='e Attic zone '
      ITEM(6)='? Help              '
      ITEM(7)='- Exit              '
      NITEMS = 7

C Help text for this menu.
      helptopic='AIM_zone_for_infil'
      call gethelptext(helpinsub,helptopic,nbhelp)

C INO needs to be set to -4 for the menu to be displayed in text mode
      INO = -4
      DO
        OK = .FALSE.
        CALL EMENU(' Infiltration Zones',ITEM,NITEMS,INO)

        SELECT CASE (INO)
C Number of zone whose temperature is used as reference for infiltration calculations.
        CASE (1)
          Zone = AIM2_ref_zone
C present a list of zones.
          Zone = AIM2_ref_zone
          CALL ASKZONE(Zone,Zone,
     &      'Select the zone to use as the reference zone',
     &      'Reference Zone','-','Please select a zone',IER)
C Check for errors
          IF (IER.EQ.0) THEN
            AIM2_ref_zone = Zone
            WRITE(DisplayString,'(A, I5)')'Reference zone number:',
     &              AIM2_ref_zone
            CALL EDISP(IUOUT,DisplayString)
          END IF

C Array holding indices of zones that receive infiltration.
        CASE (2)
          CALL ASKMULTIZONE(AIM2_num_zones_infil,InfilZones,
     &          ' Select zones to receive infiltration?',
     &          'Infiltration Zones','-',IER)
C Check for errors
          IF (IER.EQ.0) THEN
C Allocate memory for AIM2_zone_infil and AIM2_infil_to_zones matices
            CALL AIM2_Deallocate
            ALLOCATE(AIM2_zone_infil(AIM2_num_zones_infil),
     &          STAT=allocate_status)
C Check allocation was sucessful
            IF (.NOT.ALLOCATED(AIM2_zone_infil)) THEN
              CALL EDISP(IUOUT,' AIM2_zone_infil not allocated')
            END IF

            ALLOCATE(AIM2_infil_to_zones(NCOMP),
     &                 STAT=allocate_status)
C Check allocation was sucessful
            IF (.NOT.ALLOCATED(AIM2_infil_to_zones)) THEN
              CALL EDISP(IUOUT,'AIM2_infil_to_zones not allocated')
            END IF
C Copy array data
            AIM2_zone_infil=InfilZones
C Display the zones selected
            WRITE(DisplayString,'(A, I5)')
     &         'Number of zones receiving infiltration:',
     &          AIM2_num_zones_infil
            CALL EDISP(IUOUT,DisplayString)
C write out the zones selected
            DisplayString = 'Zones receiving infiltration:'
            DO zoneindex=1,AIM2_num_zones_infil
              WRITE(TempString,'(A,1x,I3)')
     &              DisplayString(1:lnblnk(DisplayString)),
     &              AIM2_zone_infil(zoneindex)
              WRITE(DisplayString,'(A)')TempString
            END DO
            CALL EDISP(IUOUT,DisplayString)
          END IF
C Zone number of basement
        CASE (3)
          Zone = AIM2_basement_zone
          IF (Zone.NE.0) THEN
            CALL ASKOK('Do you wish to revert to having no basement?',
     &                ' ',OK,.TRUE.,nbhelp)
            IF (OK) AIM2_basement_zone = 0
          END IF
          IF (.NOT.OK) THEN
            CALL EPICKS(1,IVALS,' ','Basement Zone?',
     &                12,NCOMP,zname,'zone list',IER,nbhelp)
            Zone = IVALS(1)
C check that the basement zone is not the same as the attic and crawlspace
            IF (IER.EQ.0) THEN
              IF (Zone.EQ.AIM2_CrawlSpace_zone) THEN
                CALL USRMSG('',
     &          'Basement & crawlspace zone numbers are the same!','W')
              END IF
              IF (Zone.EQ.AIM2_attic_zone) THEN
                CALL USRMSG('',
     &            'Basement & attic zone numbers are the same!','W')
              END IF
              AIM2_basement_zone = Zone
              WRITE(DisplayString,'(A, I5)')
     &           'Number of basement zone:', AIM2_basement_zone
              CALL EDISP(IUOUT,DisplayString)
            END IF
          END IF

C Zone number of Crawl Space
        CASE (4)
          Zone = AIM2_CrawlSpace_zone
          IF (Zone.NE.0) THEN
           CALL ASKOK('Do you wish to revert to having no crawlspace?',
     &                ' ',OK,.TRUE.,nbhelp)
            IF (OK) AIM2_CrawlSpace_zone = 0
          END IF
          IF (.NOT.OK) THEN
            CALL EPICKS(1,IVALS,' ','Crawl space Zone?',
     &                12,NCOMP,zname,'zone list',IER,nbhelp)
            Zone = IVALS(1)
C check that the basement zone is not the same as the attic and crawlspace
            IF (Zone.EQ.AIM2_basement_zone) THEN
              CALL USRMSG(' ',
     &        'Basement & crawlspace zone numbers are the same!','W')
            END IF
            IF (Zone.EQ.AIM2_attic_zone) THEN
              CALL USRMSG(' ',
     &        'Crawlspace & attic zone numbers are the same!','W')
            END IF
            AIM2_CrawlSpace_zone = Zone
            WRITE(DisplayString,'(A, I5)')
     &        'Number of crawl space zone:', AIM2_CrawlSpace_zone
            CALL EDISP(IUOUT,DisplayString)
          END IF
C Zone number of attic.
        CASE (5)
          Zone = AIM2_attic_zone
          IF (Zone.NE.0) THEN
            CALL ASKOK('Do you wish to revert to having no attic?',
     &                ' ',OK,.TRUE.,nbhelp)
            IF (OK) AIM2_attic_zone = 0
          END IF
          IF (.NOT.OK) THEN
            CALL EPICKS(1,IVALS,' ','Attic Zone?',
     &                12,NCOMP,zname,'zone list',IER,nbhelp)
            Zone = IVALS(1)
C check that the basement zone is not the same as the attic and crawlspace
            IF (Zone.EQ.AIM2_basement_zone) THEN
              CALL USRMSG('',
     &         'Basement & attic zone numbers are the same!','W')
            END IF
            IF (Zone.EQ.AIM2_CrawlSpace_zone) THEN
              CALL USRMSG('',
     &         'Crawlspace & attic zone numbers are the same!','W')
            END IF
            AIM2_attic_zone = Zone
            WRITE(DisplayString,'(A, I5)')
     &         'Number of attic zone:', AIM2_attic_zone
            CALL EDISP(IUOUT,DisplayString)
         END IF
         CASE (6)
C List help text for the Infiltration Zones menu.
           CALL PHELPD('Infiltration Zones',nbhelp,'-',0,0,IER)
         CASE (7)
C Exit this menu
           CALL USRMSG(' ',' ','-')
           EXIT
        END SELECT
      END DO
      END SUBROUTINE InfiltrationZonesMenu

C ********************************************************************************
C ************************ SAVE_AIM2 *******************************************
C Created by: Francesca Macdonald
C SAVE_AIM2 saves the input data required by the AIM-2 infiltration
C model (i.e. the contents of the .aim file).

C INPUTS:
C    LAIM2  The name of the AIM-2 data input file.
C ********************************************************************************

      SUBROUTINE SAVE_AIM2

      IMPLICIT NONE
#include "building.h"
#include "model.h"

C Declare local variables
      CHARACTER :: OutString*248         !display data string
      CHARACTER :: TempString*248        !temporary string
      CHARACTER :: CaseString*128        !temporary string
      CHARACTER :: PadString*128         !temporary formating string
      CHARACTER :: PathString*128        !path to aim2 directory
      INTEGER :: IER                     !errorflag
      INTEGER :: INO                     !Selection
      INTEGER :: zoneindex               !index for do loop
      INTEGER :: Zone                    !current zone number
      INTEGER :: allocate_status         !returns 0 for sucessful allocation
      INTEGER :: ISTAT                   !status flag for file closure
      INTEGER :: AIM2_FUnit              !File unit number
      INTEGER :: i                       !loop counter
      INTEGER :: TestCase                !temporary select case variable
      INTEGER :: padlength               !used to pad out a variable length string
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: IUOUT, IUIN
      COMMON/USER/BROWSE
      LOGICAL BROWSE
      COMMON/rpath/path
      CHARACTER path*72
      INTEGER :: aim2index
      INTEGER :: ipaths
      LOGICAL :: unixok
      CHARACTER :: fs
C Function definition
      INTEGER :: lnblnk

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='SAVE_AIM2'  ! set for subroutine

CPrint out all the selected parameters
      CALL AIM2_report(iuout)

C check that the current model is not one being browsed.
      IF (BROWSE) THEN
        CALL USRMSG('Cannot save the AIM2file while in browse',
     &                'mode: you must own the model!','W')
        RETURN
      END IF


      AIM2_FUnit = 42

C Set iAIM2 flag to ensure AIM2 file path is saved in the cfg file
      iAIM2 = 1

C Check to see if standard directories exist, by checking to see if there is a zones folder
      ipaths = lnblnk(zonepth)
C Other folders exist so create an aim2 folder if it doesn't
      IF (ipaths.GT.3) THEN
	aimpth='../aim2'
        WRITE(OutString,'(3A)')'mkdir -p ',path(1:lnblnk(path)),aimpth
C Create aim2 directory (-p in mkdir supresses any error messages resulting from existing directories)
        CALL system(OutString)
C Set folder separator (fs) to \ or / as required.
        CALL isunix(unixok)
        IF(unixok)THEN
          fs = char(47)
        ELSE
          fs = char(92)
        END IF
        WRITE(TempString,'(3A)')aimpth(1:lnblnk(aimpth)),fs,
     &         'infiltration.aim'
      ELSE
        WRITE(TempString,'(2A)')aimpth(1:lnblnk(aimpth)),
     &         'infiltration.aim'
      END IF
C If LAIM2 isn't set get the default
      IF (LAIM2.EQ.'UNKNOWN'.OR.LEN_TRIM(LAIM2).EQ.0) THEN
        WRITE(LAIM2,'(A)')Tempstring(1:LEN(LAIM2))
      END IF
C Get the filename
      helptopic='AIM_file_name'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKF(LAIM2,' What is the AIM2 file name?',' ',
     &      72,TempString,'AIM2 filename',IER,nbhelp)
C Check for error
      IF (IER.NE.0) THEN
        CALL USRMSG('Problem with filename',' ','W')
        RETURN
      END IF
C If the infiltration file exists use it check about overwritting, otherwise create it.
      CALL EFOPSEQ(AIM2_FUnit,LAIM2,4,IER)


C Check for error
      IF (IER.NE.0) THEN
        CALL USRMSG('Problem with opening aim2 file',' ','W')
        RETURN
      END IF

C If error opening file - return
      IF(IER.LT.0)THEN
        WRITE(TempString,'(A, I2)')'File not opened', IER
        CALL USRMSG(' ',TempString,'W')
        RETURN
      ENDIF

C Write air tightness type.
      WRITE(AIM2_FUnit,'(A)')
     &  '#---Leakage description--------------------------------------'

C Write air tightness type and supplementary blower door data
      IF (airtight_type.GT.2) THEN
        OutString = '# Air tightness type is :'
        TempString = OutString
        SELECT CASE(airtight_type)
          CASE(3)
            WRITE(OutString,'(A,A)')TempString(1:lnblnk(TempString)),
     &            'Loose '
          CASE(4)
            WRITE(OutString,'(A,A)')TempString(1:lnblnk(TempString)),
     &            'Average '
          CASE(5)
            WRITE(OutString,'(A,A)')TempString(1:lnblnk(TempString)),
     &           'Present '
          CASE(6)
            WRITE(OutString,'(A,A)')TempString(1:lnblnk(TempString)),
     &           'Energy tight '
        END SELECT
        WRITE(AIM2_FUnit,'(I3,37x,A)')airtight_type,OutString
      ELSE
        OutString = '# Blower door input; '
        TempString = OutString
        IF (blower_door_input_option.EQ.1) THEN
           WRITE(OutString,'(A,2(A,F7.2))')
     &       TempString(1:lnblnk(TempString)),'flow coefficient (C)=',
     &       AIM2_CO,' [m^3/(s Pa^n)?]; flow exponent (n)=',AIM2_n
          WRITE(AIM2_FUnit,'(2I3,2F7.2,20x,A)') airtight_type,
     &          blower_door_input_option, AIM2_CO, AIM2_n,OutString
        ELSE
          WRITE(OutString,'(A,2(F7.2,A))')
     &       TempString(1:lnblnk(TempString)),ACH_50Pa,
     &       'ac/h @50 Pa; ELA dP=',ELA_deltaP,' Pa;'
          TempString = OutString
          IF (blower_door_input_option.EQ.2) THEN
            WRITE(OutString,'(2A)')
     &          TempString(1:lnblnk(TempString)),'ELA not given'
            WRITE(AIM2_FUnit,'(2I3,2F7.2,20x,A)') airtight_type,
     &          blower_door_input_option, ACH_50Pa, ELA_deltaP,
     &          OutString
          ELSE IF (blower_door_input_option.EQ.3) THEN
            WRITE(OutString,'(2A,F8.2,A, F8.2)')
     &          TempString(1:lnblnk(TempString)),' ELA=',ELA_cm,
     &           ' cm^2; Cd=',ELA_Cd
            WRITE(AIM2_FUnit,'(2I3,4F7.2,6x,A)') airtight_type,
     &          blower_door_input_option, ACH_50Pa,ELA_deltaP, ELA_cm,
     &          ELA_Cd,OutString
          ENDIF
        ENDIF
      ENDIF

C Write leakage distribution (if specified).
      WRITE(AIM2_FUnit,'(A)')
     &  '#---Leakage distribution-------------------------------------'
      IF(userspec_leakage.EQ.1)THEN
        TempString='# Ceiling, wall & floor fractions'
        WRITE(AIM2_FUnit,'(I3,3F7.2,16x,A)')
     &     userspec_leakage,AIM2_ceil_frac,AIM2_wall_frac,
     &     AIM2_floor_frac,TempString
      ELSE IF (userspec_leakage.EQ.0)THEN
        WRITE(AIM2_FUnit,'(I3,37x,A)')userspec_leakage,
     &    '# Use default leakage distribution'
      ENDIF
C Write shielding and terrain data.
      WRITE(AIM2_FUnit,'(A)')
     &  '#---Shielding and terrain data-------------------------------'
      DO i=1,2
        IF (i.EQ.1) THEN
          TestCase = AIM2_terrain_weather
          OutString = '# Weather station terrain: '
        ELSE
          TestCase = AIM2_terrain_building
          TempString = OutString
          WRITE(OutString,'(2A)') TempString(1:lnblnk(TempString)),
     &         ';   Building site terrain: '
        END IF
        SELECT CASE (TestCase)
          CASE (1)
            CaseString='Open sea,fetch > 5 km'
          CASE (2)
            CaseString='Mud flats,no vegetation'
          CASE (3)
            CaseString='Open flat terrain, grass'
          CASE (4)
            CaseString='Low crops, x/H > 20'
          CASE (5)
            CaseString='High crops, scattered obstacles'
          CASE (6)
            CaseString='Parkland, bushes, x/H ~ 10'
          CASE (7)
            CaseString='Suburban, forest'
          CASE (8)
            CaseString='City centre'
          CASE DEFAULT
            CaseString=''
        END SELECT
        TempString = OutString
        WRITE(OutString,'(3A)') TempString(1:lnblnk(TempString)),
     &         CaseString(1:lnblnk(CaseString))
      END DO
      WRITE(AIM2_FUnit,'(4I3,F7.2,21x,A)')
     &    AIM2_terrain_weather,AIM2_terrain_building,
     &    AIM2_wall_shielding,AIM2_flue_shielding,AIM2_anemom_height,
     &    OutString


      DO i=1,2
        IF (i.EQ.1) THEN
          TestCase = AIM2_wall_shielding
          OutString = '# Wall shielding: '
        ELSE
          TestCase = AIM2_flue_shielding
          TempString = OutString
          WRITE(OutString,'(2A)') TempString(1:lnblnk(TempString)),
     &         ';   flue shielding: '
        END IF
        SELECT CASE (TestCase)
          CASE (1)
            CaseString='None'
          CASE (2)
            CaseString='Light'
          CASE (3)
            CaseString='Heavy'
          CASE (4)
            CaseString='Very heavy'
          CASE (5)
            CaseString='Complete'
          CASE DEFAULT
            CaseString=''
        END SELECT
        TempString = OutString
        WRITE(OutString,'(2A)') TempString(1:lnblnk(TempString)),
     &         CaseString(1:lnblnk(CaseString))
      END DO
      WRITE(AIM2_FUnit,'(40x,2A,F6.2,A)')OutString(1:lnblnk(OutString)),
     &    ';   anemometer height=',AIM2_anemom_height,' m'

C Write building eaves height.
      WRITE(AIM2_FUnit,'(A)')
     &  '#---Height of building eaves (m)-----------------------------'
      WRITE(AIM2_FUnit,'(F7.2)') AIM2_eaves_height

C Write diameters of furnace, fireplace, and DHW flues.
      WRITE(AIM2_FUnit,'(A)')
     &  '#---Flue diameters (mm)--------------------------------------'
      TempString = '# Furnace, fire#1, fire#2, dhw#1, dhw#2'
      WRITE(AIM2_FUnit,'(5F7.2,5x,A)')AIM2_furnace_flue,
     &  AIM2_fire_flue1,AIM2_fire_flue2,AIM2_DHW_flue1,AIM2_DHW_flue2,
     &  TempString

C Write the zone coupling data
      WRITE(AIM2_FUnit,'(A)')
     &  '#---Zone indices---------------------------------------------'
C Write the number of the reference zone.
      OutString = '# Zone whose temperature is used to calculate '
      TempString = OutString
      WRITE(OutString,'(2A)') TempString(1:lnblnk(TempString)),
     &      ' the density of indoor air'
      WRITE(AIM2_FUnit,'(I3,37x,A)')AIM2_ref_zone, OutString

C Write the number of zones in model that will receive infiltration, followed by the
C indices of the zones that receive infiltration.
      WRITE(OutString,'(I3)')AIM2_num_zones_infil
      DO zoneindex=1,AIM2_num_zones_infil
        TempString = OutString
        WRITE(OutString,'(A, I3)') TempString(1:lnblnk(TempString)),
     &    AIM2_zone_infil(zoneindex)
      END DO
C Pad the string out to line up formatting
      padlength = 40-LEN_TRIM(OutString)
      PadString = '                                               '
      TempString = OutString
      WRITE(OutString,'(A,A,A)') TempString(1:lnblnk(TempString)),
     &      PadString(1:padlength), 
     &      '# Total number of zones receiving infil; '
      TempString = OutString
      WRITE(OutString,'(2A)') TempString(1:lnblnk(TempString)),
     &      ' indices of zones receiving infil'
      WRITE(AIM2_FUnit,'(A)')OutString
C Write zone number of basement,crawlspace and attic
      OutString = '# Index of basement, crawlspace & attic zones'
      TempString = OutString
      WRITE(OutString,'(2A)')TempString(1:lnblnk(TempString)),
     &      ' (=0 if none)'
      WRITE(AIM2_FUnit,'(3I3,31x,A)') AIM2_basement_zone,
     &      AIM2_CrawlSpace_zone,AIM2_attic_zone,OutString

C Close AIM-2 input data file.
      CALL ERPFREE(AIM2_FUnit,ISTAT)
      RETURN
      END SUBROUTINE SAVE_AIM2

      END MODULE AIM2_Inputs
