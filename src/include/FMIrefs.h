C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

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

C This file provides descriptions for FMU input and output variables.
C Variables needed to interface with FMUs must be added here.

C This header is automatically imported with FMI.h.

C ESP to FMU (FMU inputs)

C MFMIIREFS - Number of input variables.
C FMIIREFS  - Input variable identifier strings.
C FMIIAorZ  - Integer flag identifying whether input variables can be
C             associated with the environment, building zones, or both:
C             0 = ambient only, 1 = building zone only, 2 = both.
C FMIINSUP  - Number of supplementary data items for each input.
C             Note that values cannot be greater than MFMUSUP defined in
C             FMI.h.

      INTEGER, PARAMETER :: MFMIIREFS=6

C Numeric indices refer to possible values of FMUIVAR (see FMI.h).
C 1: Zone_mean_drybulb_temperature  ! units of degrees C
C 2: Zone_illumination              ! units of lux
C 3: Zone_CO2_concentration         ! units of ppm
C 4: Zone_lights_power              ! units of W
C 5: Ambient_drybulb_temperature    ! units of degrees C
C 6: Ambient_rain_indicator         ! binary; 0 = no rain, 1 = rain

      CHARACTER*30, PARAMETER :: FMIIREFS(MFMIIREFS)=
     &  (/'Zone_mean_drybulb_temperature ',
     &    'Zone_illumination             ',
     &    'Zone_CO2_concentration        ',
     &    'Zone_lights_power             ',
     &    'Ambient_drybulb_temperature   ',
     &    'Ambient_rain_indicator        '/)

      INTEGER, PARAMETER :: FMIIAorZ(MFMIIREFS)=(/1,1,1,1,0,0/)

      INTEGER, PARAMETER :: FMIINSUP(MFMIIREFS)=(/0,0,0,0,0,0/)



C FMU to ESP (FMU outputs)

C MFMIOREFS - Number of output variables.
C FMIOREFS  - Output variable identifier strings.
C FMIOAorZ  - Integer flag identifying whether output variables can be
C             associated with the environment, building zones, or both:
C             0 = ambient only, 1 = building zone only, 2 = both.
C FMIONSUP  - Number of supplementary data items for each output. 
C             Note that values cannot be greater than MFMUSUP defined in
C             FMI.h.

      INTEGER, PARAMETER :: MFMIOREFS=7

C Numeric indices refer to possible values of FMUOVAR (see FMI.h).
C 1. Zone_control_on    ! binary; 0 = off, 1 = on
C 2. Zone_lights_on     ! fraction; 0.0 = off, 0.5 = half, 1.0 = full
C 3. Zone_windows_open  ! fraction; 0.0 = closed, 0.5 = half open, 1.0 = fully open
C 4. Zone_equipment_on  ! fraction; 0.0 = off, 0.5 = half, 1.0 = full
C 5. Zone_thermostat    ! temperature (degrees C)
C 6. Zone_blinds_closed ! binary; 0 = blinds open, 1 = blinds closed
C 7. Zone_occupancy     ! number of occupants

      CHARACTER*30, PARAMETER :: FMIOREFS(MFMIOREFS)=
     &  (/'Zone_control_on               ',
     &    'Zone_lights_on                ',
     &    'Zone_windows_open             ',
     &    'Zone_equipment_on             ',
     &    'Zone_thermostat               ',
     &    'Zone_blinds_closed            ',
     &    'Zone_occupancy                '/)

      INTEGER, PARAMETER :: FMIOAorZ(MFMIOREFS)=(/1,1,1,1,1,1,1/)

      INTEGER, PARAMETER :: FMIONSUP(MFMIOREFS)=(/0,0,1,0,0,0,0/)
