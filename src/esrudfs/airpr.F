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

C Functions to evaluate fluid properties as a function of temperature.
C   AIRDEN - Evaluate air density.
C   AIRBET - Thermal expansion coefficient of air.

C ********************* AIRDEN *********************
C AIRDEN - air density as a function of temperature based on
      FUNCTION AIRDEN(T)

C the ideal gas correlation.
      DATA A1,A2,A3/1.1881,293.15,273.15/
      AIRDEN=A1*A2/(A3+T)
      RETURN
      END

C ********************* AIRBET *********************
C AIRBET - thermal expansion coefficient of the air as a function of 
C the temperature based on the ideal gas correlation.
      FUNCTION AIRBET(T)

      DATA A1/273.15/
      AIRBET=1.0/(T+A1)
      RETURN
      END
