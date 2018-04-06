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

C FMIsim.F contains dummy fortran functions to disable FMI.


C ******************** FMI_INIT
      SUBROUTINE FMI_INIT(IER)
      RETURN
      END

C ******************** FMI_SIMSTART
      SUBROUTINE FMI_SIMSTART(IER)
      RETURN
      END

C ******************** FMI_SIMSTEP
      SUBROUTINE FMI_SIMSTEP(IER)
      RETURN
      END

C ******************** FMI_DONE
      SUBROUTINE FMI_DONE(IER)
      RETURN
      END
