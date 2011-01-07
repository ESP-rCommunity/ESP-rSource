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

      ips=1

C Clear fortran side text buffer.
      call clrtextbuf
      return
      END

C----------------------------------------------------------
C     Initialization of ground-temperature profile names 
C     and data. Should these really be hard coded in the 
C     esru-lib library? A *much* better approach would be
C     to include them in a text-input file. Otherwise the 
C     ground temperature list will always be UK-centric, or
C     grow unmanagably long.  Target redesign 
C     for version 11.11?
C----------------------------------------------------------
      BLOCK DATA InitGrdTmpName 
      implicit none 
      include "building.h"
      include "site.h"
C Names for each ground temperature.
      data grdtmpname/'UK monthly mean via Lacy','unknown',  !<- Should we really have 'unknown' profiles?
     &'unknown','UK Paisley 0.3m',
     &'UK Paisley 1.2m','GBR_Abbotsinch at 0.5m',
     &'GBR_Abbotsinch at 2.0m','GBR_Abbotsinch at 4.0m',
     &'GBR_London.Gatwick 0.5m','GBR_London.Gatwick 2.0m',
     &'GBR_London.Gatwick 4.0m','GBR_Aberdeen at 0.5m',
     &'GBR_Aberdeen at 2.0m','GBR_Aberdeen at 4.0m',
     &'GBR_Birmingham at 0.5m','GBR_Birmingham at 2.0m',
     &'GBR_Birmingham at 4.0m','IRL_Dublin at 0.5m',
     &'IRL_Dublin at 2.0m','IRL_Dublin at 4.0m',
     &'IRL_Kilkenny at 0.5m','IRL_Kilkenny at 2.0m',
     &'IRL_Kilkenny at 4.0m','IRL_Malin at 0.5m',
     &'IRL_Malin at 2.0m','IRL_Malin at 4.0m',
     &'FRA_Dijon at 0.5m','FRA_Dijon at 2.0m',
     &'FRA_Dijon at 4.0m','FRA_Nice at 0.5m',
     &'FRA_Nice at 2.0m','FRA_Nice at 4.0m',
     &'FRA_Paris_Orly at 0.5m','FRA_Paris_Orly at 2.0m',
     &'FRA_Paris_Orly at 4.0m','DEU_Berlin at 0.5m',
     &'DEU_Berlin at 2.0m','DEU_Berlin at 4.0m',
     &'DEU_Koln at 0.5m','DEU_Koln at 2.0m',
     &'DEU_Koln at 4.0m','all zeros','all zeros'/  !<- Why are there two sets of 'all-zeros'?

C The following assumes that grdtmp is sized for 43 sets of 12 months.
      data grdtmp/
C Profile 1 is the UK monthly mean ground temps at 1.2m depth as given by Lacy.
     &7.28,6.5,6.56,7.33,9.06,11.39,13.5,14.33,14.11,11.72,10.72,8.67,
     &6.0,8.0,10.0,12.0,13.0,14.0,15.0,14.0,13.0,12.0,10.0,8.0,
     &4.9,4.4,4.1,6.4,9.0,12.9,15.5,15.5,16.4,15.2,9.4,8.7,
C Next two monthly mean daily ground temperature profiles are based
C on earth temperature readings made once daily at 09:00 GMT at
C depths of 0.305m and 1.219m respectively under a short grass surface
C in Paisley (12km west of Glasgow, Altitude 32.3m).
     &3.5,3.61,5.22,8.11,11.39,14.61,16.22,15.78,13.39,10.22,6.72,4.78,
     &6.72,6.11,6.28,7.39,9.28,11.39,13.11,13.72,13.28,11.78,9.72,7.89,
C GBR_Glasgow-Abbotsinch at 0.5m from EPW file
     &5.68,3.89,3.41,3.83,6.24,8.94,11.47,13.31,13.85,12.98,10.91,8.28,
C GBR_Glasgow-Abbotsinch at 2.0m from EPW file
     &7.28,5.61,4.81,4.78,6.06,7.90,9.89,11.59,12.47,12.30,11.13,9.33,
C GBR_Glasgow-Abbotsinch at 4.0m depth
     &8.33,7.03,6.23,5.99,6.46,7.53,8.87,10.18,11.06,11.27,10.77,9.71,
C GBR_London.Gatwick at 0.5m from EPW file
     &4.16,5.30,7.51,9.61,13.58,15.67,16.24,15.18,12.74,9.69,6.69,4.7,
C GBR_London.Gatwick at 2.0m from EPW file
     &5.73,6.0,7.27,8.68,11.72,13.67,14.64,14.43,13.03,10.93,8.56,6.69,
C GBR_London.Gatwick at 4.0m from EPW file
     &7.35,7.16,7.71,8.51,10.52,12.03,13.01,13.25,12.64,11.4,9.79,8.34,
C GBR_Aberdeen.Dyce at 0.5m from EPW file
     &3.75,4.63,6.32,7.93,10.97,12.57,13.01,12.20,10.33,7.99,5.69,4.17,
C GBR_Aberdeen.Dyce at 2.0m from EPW file
     &4.95,5.17,6.14,7.21,9.55,11.04,11.79,11.62,10.55,8.94,7.12,5.69,
C GBR_Aberdeen.Dyce at 4.0m from EPW file
     &6.20,6.05,6.47,7.08,8.63,9.78,10.53,10.72,10.25,9.30,8.07,6.96,
C GBR_Birmingham at 0.5m from EPW file
     &6.20,4.12,3.56,4.05,6.86,10.0,12.96,15.10,15.73,14.72,12.30,9.24,
C GBR_Birmingham at 2.0m from EPW file
     &8.07,6.12,5.20,5.16,6.64,8.8,11.11,13.09,14.12,13.93,12.56,10.46,
C GBR_Birmingham at 4.0m from EPW file
     &9.29,7.78,6.85,6.57,7.11,8.36,9.92,11.45,12.48,12.72,12.13,10.90,
C IRL_Dublin at 0.5m from EPW file
     &5.34,4.9,5.68,6.89,10.07,12.52,14.17,14.68,13.82,11.92,9.37,7.01,
C IRL_Dublin at 2.0m from EPW file
     &6.95,6.17,6.33,6.97,9.1,11.02,12.56,13.38,13.21,12.13,10.38,8.52,
C IRL_Dublin at 4.0m from EPW file
     &8.28,7.48,7.31,7.55,8.75,10.05,11.23,12.06,12.25,11.79,10.76,9.5,
C IRL_Kilkenny at 0.5m from EPW file
     &4.97,4.51,5.33,6.62,9.99,12.60,14.35,14.89,13.98,11.96,9.25,6.75,
C IRL_Kilkenny at 2.0m from EPW file
     &6.68,5.85,6.02,6.71,8.96,11.0,12.64,13.51,13.33,12.18,10.32,8.35,
C IRL_Kilkenny at 4.0m from EPW file
     &8.10,7.25,7.06,7.32,8.60,9.97,11.23,12.11,12.31,11.82,10.72,9.39,
C IRL_Malin at 0.5m from EPW file
     &7.50,6.20,5.85,6.15,7.9,9.86,11.71,13.04,13.43,12.80,11.29,9.39,
C IRL_Malin at 2.0m from EPW file
     &8.66,7.45,6.87,6.85,7.77,9.11,10.55,11.79,12.43,12.3,11.46,10.15,
C IRL_Malin at 4.0m from EPW file
     &9.42,8.48,7.90,7.72,8.06,8.84,9.81,10.76,11.40,11.56,11.19,10.43,
C FRA_Dijon at 0.5m from EPW file
     &3.23,2.51,3.79,5.8,11.09,15.16,17.91,18.75,17.32,14.16,9.93,6.01,
C FRA_Dijon at 2.0m from EPW file
     &5.91,4.61,4.88,5.95,9.48,12.67,15.23,16.6,16.31,14.51,11.60,8.51,
C FRA_Dijon at 4.0m from EPW file
     &8.12,6.79,6.5,6.91,8.9,11.06,13.03,14.40,14.72,13.94,12.23,10.14,
C FRA_Nice at 0.5m from EPW file
     &11.6,9.25,8.6,9.16,12.33,15.88,19.22,21.64,22.35,21.2,18.47,15.0,
C FRA_Nice at 2.0m from EPW file
     &13.7,11.5,10.46,10.4,12.1,14.5,17.13,19.37,20.53,20.3,18.77,16.4,
C FRA_Nice at 4.0m from EPW file
     &15.1,13.37,12.33,12.0,12.62,14.0,15.8,17.5,18.68,18.95,18.3,16.9,
C FRA_Paris_Orly at 0.5m from EPW file
     &3.98,5.32,7.94,10.41,15.09,17.56,18.24,16.99,14.1,10.5,6.96,4.62,
C FRA_Paris_Orly at 2.0m from EPW file
     &5.83,6.16,7.65,9.31,12.91,15.21,16.35,16.09,14.45,11.96,9.17,6.97,
C FRA_Paris_Orly at 4.0m from EPW file
     &7.74,7.51,8.16,9.11,11.48,13.27,14.42,14.7,13.98,12.52,10.62,8.91,
C DEU_Berlin at 0.5m from EPW file
     &4.94,2.03,1.24,1.93,5.86,10.25,14.39,17.39,18.26,16.86,13.47,9.2,
C DEU_Berlin at 2.0m from EPW file
     &7.56,4.83,3.54,3.49,5.56,8.57,11.8,14.58,16.0,15.75,13.83,10.89,
C DEU_Berlin at 4.0m from EPW file
     &9.26,7.15,5.85,5.45,6.2,7.97,10.14,12.28,13.72,14.06,13.24,11.52,
C DEU_Koln at 0.5m from EPW file
     &2.63,4.00,6.65,9.17,13.93,16.43,17.12,15.85,12.92,9.26,5.66,3.28,
C DEU_Koln at 2.0m from EPW file
     &4.51,4.85,6.36,8.05,11.7,14.04,15.21,14.94,13.27,10.75,7.91,5.67,
C DEU_Koln at 4.0m from EPW file
     &6.46,6.22,6.89,7.84,10.26,12.07,13.24,13.53,12.8,11.31,9.39,7.64,
C Add new ground temperature profiles here (space for 2*12 months).
     &0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
     &0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0/

      END BLOCK DATA InitGrdTmpName 
