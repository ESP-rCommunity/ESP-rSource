C This file contains the following modules related to the implementation
C of the AIM-2 infiltration model:
C   AIM2_INPUT_DATA  Declares variables to hold data defined by user.
C   AIM2_CALC_DATA   Declares variables to hold intermediate and final results.
C   AIM2_H2K_DUMMY   Declares variables to hold data that will eventually be
C                    passed by the H2K interface, but is hard-wired for now.


C ********************************************************************************
C ************************ AIM2_INPUT_DATA ***************************************
C Created by: Ian Beausoleil-Morrison
C Initial Creation Date: October 19, 2000
C Copyright 2000: NRCan Buildings Group
C
C Modified by Ian Beausoleil-Morrison on July 13, 2001:
C   - Some variables removed as part of coding that passes furnace
C     part-load ratio to AIM-2 model. Furnace type and fuel data now
C     comes from .hvac file rather than being set as `dummy' parameter.

C This module declares the variables to hold data that are specified by the
C user via the .aim file. It also initializes some constants.



C---------------------------------------------------------------------------------
C Constant initialization.
C---------------------------------------------------------------------------------




C---------------------------------------------------------------------------------
C Variable declarations.
C---------------------------------------------------------------------------------
      COMMON/AIM2_INPUT_DATA/
     & airtight_type,
     & blower_door_input_option,ACH_50Pa,ELA_given,
     & ELA_cm,ELA_deltaP,ELA_Cd,
     & userspec_leakage,
     & AIM2_ceil_frac,AIM2_wall_frac,AIM2_floor_frac,
     & AIM2_terrain_weather,AIM2_terrain_building,
     & AIM2_wall_shielding,AIM2_flue_shielding,
     & AIM2_anemom_height,AIM2_eaves_height,
     & AIM2_furnace_flue,AIM2_fire_flue1,AIM2_fire_flue2,
     & AIM2_DHW_flue1,AIM2_DHW_flue2,AIM2_ref_zone,
     & AIM2_num_zones_infil,AIM2_zone_infil,AIM2_basement_zone,
     & AIM2_CrawlSpace_zone,AIM2_attic_zone,fAIM2VersionNumber

C Air tightness type as defined in HOT2000 interface:
C 1=blower door test; 2=quick blower door test;
C 3=loose; 4=average; 5=present; 6=energy tight.
        INTEGER  airtight_type

C Three options are available for the blower-door results inputs
C Option 1: flow coefficient C0 [m^3/(s Pa^n)]
C           flow exponent n
C Option 2: Air change rate @ 50Pa
C           Pressure difference in Pa
C Option 3: Air change rate @ 50Pa
C           Pressure difference in Pa
C           Equivalent leakage area in cm^2
C           Discharge coefficient Cd used to calculate leakage area
            
        INTEGER blower_door_input_option

C Both flow coefficient and flow exponent can be either user inputs or
C derived variables, depending on the blower door input option.
C Since they are already declared as derived variables in
C COMMON/AIM2_CALC_DATA/, no change is made in this respect.

C Air change rate @ 50Pa. Used for blower_door_input_option equal to 2 and 3
        REAL  ACH_50Pa

C Pressure difference in Pa for ELA_cm.
        REAL  ELA_deltaP
        
C Flag indicating whether user input ELA: 0=no input; 1=input given.
        INTEGER ELA_given

C Equivalent leakage area in cm^2. Only used for blower_door_input_option equal to 3
        REAL  ELA_cm

C Discharge coefficient. Only used for blower_door_input_option equal to 3
        REAL  ELA_Cd
              
C Flag indicating whether user has specified leakage fractions for ceiling,
C wall, and floor: 0 indicates user input not given; 1 indicates user input given.
        INTEGER  userspec_leakage

C Ceiling, wall, and floor leakage fractions.
        REAL  AIM2_ceil_frac,AIM2_wall_frac,AIM2_floor_frac

C Terrain flag at weather station:
C         1=Open sea,fetch > 5 km
C         2=Mud flats,no vegetation
C         3=Open flat terrain, grass
C         4=Low crops, x/H > 20
C         5=High crops, scattered obstacles
C         6=Parkland, bushes, x/H ~ 10
C         7=Suburban, forest
C         8=City centre
        INTEGER  AIM2_terrain_weather

C Terrain flag at building site. Same options as for AIM2_terrain_weather.
        INTEGER  AIM2_terrain_building

C Flag indicating local shielding on walls:
C         1=No local shielding
C         2=Light local shielding
C         3=Heavy
C         4=Very heavy
C         5=Complete (by large buildings)
        INTEGER  AIM2_wall_shielding

C Flag indicating local shielding on flue. Same options as for AIM2_wall_shielding.
        INTEGER  AIM2_flue_shielding

C Height of anenometor at weather station (m).
        REAL  AIM2_anemom_height

C Height of building eaves (m).
        REAL  AIM2_eaves_height

C Diameter of furnace flue (mm).
        REAL  AIM2_furnace_flue

C Diameter of #1 fireplace flue (mm).
        REAL  AIM2_fire_flue1

C Diameter of #2 fireplace flue (mm).
        REAL  AIM2_fire_flue2

C Diameter of primary DHW system flue (mm).
        REAL  AIM2_DHW_flue1

C Diameter of secondary DHW system flue (mm).
        REAL  AIM2_DHW_flue2

C Number of zone whose temperature is used as reference for infiltration calculcations.
        INTEGER  AIM2_ref_zone

C Number of zones that receive infiltration.
        INTEGER  AIM2_num_zones_infil

C Matrix holding indices of zones that receive infiltration.
        INTEGER AIM2_zone_infil(MCOM)  
C 10 zones should be plenty for H3K.

C Zone number of basement (basements treated differently that living space in terms of
C allocation of infiltration air flow).
        INTEGER  AIM2_basement_zone

C Zone number of Crawl Space (The volume of the crawl space is not included
c in the house volume for infiltration calculations).
        INTEGER  AIM2_CrawlSpace_zone

C Zone number of attic. Infiltration to attics not calculated with AIM-2 model
C but rather treated with different approach.
        INTEGER  AIM2_attic_zone

C File version (used in report functions)
        REAL fAIM2VersionNumber



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

       COMMON/AIM2_CALC_DATA/
     & AIM2_flue_height,
     & AIM2_Betaf,
     & AIM2_CO,
     & AIM2_n,
     & furnON_flue_diam,
     & furnOFF_flue_diam,
     & AIM2_Ct,
     & AIM2_Swo,
     & AIM2_Swflue,
     & AIM2_house_vol,
     & AIM2_infil_to_zones,
     & AIM2_active_zones


C The height of the flue (m).
        REAL  AIM2_flue_height

C The normalized height of the flue. Refer to eq 20 of Wilson and Walker (1990).
        REAL  AIM2_Betaf

C The building fabric flow coefficient.
        REAL  AIM2_CO

C The building flow exponent (dimensionless).
        REAL  AIM2_n

C Diameter of furnace flue when furnace operating (mm).
        REAL  furnON_flue_diam

C Diameter of furnace flue when furnace off (mm).
        REAL  furnOFF_flue_diam

C The wind speed correction factor.
        REAL  AIM2_Ct

C The shelter coefficients for the walls.
        REAL  AIM2_Swo

C The shelter coefficients for the flue.
        REAL  AIM2_Swflue

C House volume in m^3.
        REAL  AIM2_house_vol

C Matrix holding the infiltration rate apportioned to each zone.
        REAL AIM2_infil_to_zones(MCOM)

C Array holding logical indicators at the zone index as to whether it should be modeled with AIM2 or not.
        LOGICAL AIM2_active_zones(MCOM)
      

C     ********************************************************************************
C     ************************* AIM2_H2K_DUMMY ***************************************
C     Created by: Ian Beausoleil-Morrison
C     Initial Creation Date: October 19, 2000
C     Copyright 2000: NRCan Buildings Group
        
C     This module declares and initialzes generic house data that is required by
C     the AIM-2 model. These data are "hard-wired" here for the purposes of testing
C     the AIM-2 model. As work proceeds to integrate the HOT2000 interface and the ESP-r
C     engine these data will be passed by the interface rather than initialized here.
        COMMON/AIM2_H2K_DUMMY/        
     &       H2K_house_type, 
     &       H2K_storeys,
     &       H2K_found_num,
     &       H2K_found_type, 
     &       H2K_found_depth, 
     &       H2K_found_area, 
     &       H2K_fireplace1, 
     &       H2K_fireplace2,
     &       H2K_DHW1_exist, 
     &       H2K_DHW2_exist,
     &       H2K_DHW1_shareflue, 
     &       H2K_DHW2_shareflue,
     &       H2K_DHW1_inside, 
     &       H2K_DHW2_inside, 
     &       H2K_DHW1_solar


C House type (as defined in H2K list 62):
C   1 = Single detached
C   2 = Double/Semi-detached
C   3 = Duplex
C   4 = Triplex
C   5 = Apartment unit (Individual) 
C   6 = Row house, end unit
C   7 = Mobile home
C   8 = Row house, middle unit
        INTEGER H2K_house_type 
        
        
C     Description of number of storeys input by user (as defined in H2K list 63):
C     1 = One storey
C     2 = One and a half
C     3 = Two storeys
C     4 = Two and a half
C     5 = Three storeys
C     6 = Split level
C     7 = Split entry / Raised basement
        INTEGER H2K_storeys
        
C     Number of foundations input by user.
        INTEGER  H2K_found_num
        
        
C     Foundation type (as defined in H2K iFndatn). The index indicates the
C     foundation number under consideration.
C     1 = N/A
C     2 = Basement
C     3 = Slab-on-grade
C     4 = Closed crawl space
C     5 = Vented crawl space
C     6 = Open crawl space
C     7 = Walkout basement
        INTEGER H2K_found_type(6)

        
C     Foundation depth (see H2K rFndatn). The index indicates the
C     foundation number under consideration.
        REAL H2K_found_depth(6) 
        
        
C     Foundation floor area (see H2K rFndatn). The index indicates the
C     foundation number under consideration.
        REAL  H2K_found_area(6) 
        
        
C     Flags indicating whether "fireplace #1" and "fireplace #2" exist.
        LOGICAL H2K_fireplace1  
        LOGICAL H2K_fireplace2
        
        
C     Flags indicating presence of primary ("1" suffix) and secondary ("2" suffix)
C     DHW systems.
        LOGICAL  H2K_DHW1_exist
        LOGICAL  H2K_DHW2_exist
        
        
C     Flags indicating whether the primary and secondary DHW systems share the
C     flue with the furnace.
        LOGICAL H2K_DHW1_shareflue
        LOGICAL  H2K_DHW2_shareflue 
        
        
C     Flags indicating whether the primary and secondary DHW systems are located
C     indoors (true) or outdoors (false).
        LOGICAL  H2K_DHW1_inside
        
        LOGICAL  H2K_DHW2_inside
       
        
C     Flag indicating whether primary DHW system is a solar collector. Interface
C     does not allow secondary system to be solar.
        LOGICAL  H2K_DHW1_solar
