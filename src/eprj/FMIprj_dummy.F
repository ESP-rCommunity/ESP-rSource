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

C FMIprj_dummy.F contains a dummy subroutine blocking FMI functionality
C if prj is not compiled with FMI support.

C ******************** FMI_ED
C This version of FMI_ED just brings up a text box informing the user
C that FMI is not available.

      SUBROUTINE FMI_ED(IER)
#include "help.h"

      helpinsub='FMI_ED'  ! set for subroutine
      helptopic='FMI_disabled'
      CALL gethelptext(helpinsub,helptopic,nbhelp)
      CALL PHELPD('FMI not available',nbhelp,'-',0,0,IER)

      RETURN

      END
