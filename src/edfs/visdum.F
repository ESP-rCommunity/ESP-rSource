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

C This file contains the following dummy routines:
C related to image generation not applicable to bps.

C MAKS2D - dummy - just returns.
C MAKS3D - dummy - just returns.

C ********************* MAKS2D dummy *********************
      SUBROUTINE MAKS2D(ISUR,ilayer,name,IFRAME)
      character name*12
      integer isur,ilayer,iframe
      return
      end

C ********************* MAKS3D dummy *********************
C first view.
      SUBROUTINE MAKS3D(IFRAME)
      integer iframe
      return
      end
