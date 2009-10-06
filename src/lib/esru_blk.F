C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow, Scotland, 2001.

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

C Ezero loads the COMMONS used by libgtkesru.a, libxesru.a and libnoxesru.a.

      subroutine EZERO
#include "building.h"
#include "site.h"

C Ezero loads the COMMONS used by libgtkesru.a, libxesru.a and libnoxesru.a.
#include "epara.h"

C Commons for paged menus, PMENUSV is to pop in a definition for
C later recovery.
      COMMON/PMENUSV/MHEAD1(5),MCTL1(5),MIFUL1(5),MFUL1(5),IST1(5),
     &               ILEN1(5),IPM1(5),MPM1(5),IPFLG1(5),MILEV
      COMMON/PBYDAT/PBYFIL,NSCVP,IPS
      logical PBYFIL

      MHEAD=0
      MCTL=0
      MIFULL=0
      MFULL=0
      IST=0
      ILEN=0
      IPM=1
      MPM=1
      IPFLG=0
      do 42 i=1,5
        MHEAD1(i)=0
        MCTL1(i)=0
        MIFUL1(i)=0
        MFUL1(i)=0
        IST1(i)=0
        ILEN1(i)=0
        IPM1(i)=0
        MPM1(i)=0
        IPFLG1(i)=0
  42  continue

      data grdtmp/
C Profile 1 is the UK monthly mean ground temperatures at 1.2m depth
C as given by Lacy.
     &7.28,6.5,6.56,7.33,9.06,11.39,13.5,14.33,14.11,11.72,
     &10.72,8.67,
     &6.0,8.0,10.0,12.0,13.0,14.0,15.0,14.0,13.0,12.0,
     &10.0,8.0,
     &4.9,4.4,4.1,6.4,9.0,12.9,15.5,15.5,16.4,15.2,9.4,8.7,

C Next two monthly mean daily ground temperature profiles are based
C on earth temperature readings made once daily at 09:00 GMT at
C depths of 0.305m and 1.219m respectively under a short grass surface
C in Paisley (12km west of Glasgow, Altitude 32.3m).
     &3.5,3.61,5.22,8.11,11.39,14.61,16.22,15.78,13.39,10.22,
     &6.72,4.78,
     &6.72,6.11,6.28,7.39,9.28,11.39,13.11,13.72,13.28,11.78,
     &9.72,7.89,
C Add new ground temperature profiles here (space for 5*12 months).
     &0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     &0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     &0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     &0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     &0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0/

      ips=1

C Clear fortran side text buffer.
      call clrtextbuf
      return
      END

