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

C This file contains the following routines:
C  UATCHK:   Checks if time based uncertainty active.
C  UAT01:    Edits control information based on time.
C  UAT03:    Edits data: Climate parameters - stores changes
C  UAT03a:   Edits data: Climate parameters - uses changes when a days data is read in.

C ******************** UATFLG ********************
C UATFLG edits control data based on time.

      SUBROUTINE UATFLG(IDAY,IHOUR,ILOC,IVALID)
#include "building.h"
#include "uncertainty.h"

      COMMON/UA2T/NTLOC(MNIL),NTG(MNIL,4)

      IVALID=0
C For uncertainty location ILOC check if in valid period.
C If iday is negative check time only.
      if (IDAY.lt.0) then
        if((IHOUR.gt.NTG(ILOC,2).and.IHOUR.lt.NTG(ILOC,4)))IVALID=1
      elseif (IDAY.gt.NTG(ILOC,1).and.IDAY.lt.NTG(ILOC,3)) then
        IVALID=1
      elseif (IDAY.eq.NTG(ILOC,1).and.IDAY.lt.NTG(ILOC,3)) then
        if (IHOUR.gt.NTG(ILOC,2).or.IHOUR.eq.-1) then
          IVALID=1
        endif
      elseif (IDAY.gt.NTG(ILOC,1).and.IDAY.eq.NTG(ILOC,3)) then
        if (IHOUR.lt.NTG(ILOC,4).or.IHOUR.eq.-1) then
          IVALID=1
        endif
      elseif (IDAY.eq.NTG(ILOC,1).and.IDAY.eq.NTG(ILOC,3)) then
        if ((IHOUR.gt.NTG(ILOC,2).and.IHOUR.lt.NTG(ILOC,4)).or.
     &       IHOUR.eq.-1) then
          IVALID=1
        endif
      endif

C Return to editing routine.

      RETURN
      END

C ******************** UAT01 ********************
C UAT01 flags which control data should be changed during the simulation.

      SUBROUTINE UAT01(IACT,DIR)
#include "building.h"
C Common blocks UA3 UA4 within uncertainty.h
#include "uncertainty.h"

      COMMON/UA32/IMISCD(MNCNG,2)
      COMMON/UATD/UACLIM(6,3),UACTL(7,4),UAMCSA(MNACT)

      CHARACTER ITEM(7)*20

C Data stored in UA3 has the following meaning in this routine:
C IDMAT(?,1): Control function reference number.
C IDMAT(?,2): Control function day type.
C IMISCD(?,1): Control function period.
C IMISCD(?,2): Uncertain parameter.
C IDMAT(?,3): Change type (%; +/-; explicit).
C Define variables for current location and change.
      ICC=IACTD(IACT,1)
      ICL=IACTD(IACT,2)
      ITEM(1) ='Period start time'
      ITEM(2) ='Maximum heating flux'
      ITEM(3) ='Minimum heating flux'
      ITEM(4) ='Maximum cooling flux'
      ITEM(5) ='Minimum cooling flux'
      ITEM(6) ='Heating set point'
      ITEM(7) ='Cooling set point'

      write (36,*) '   '
      write (36,*) 'Entering subroutine UAT01'
      write (36,*) ITEM(IMISCD(ICC,2)),' will be edited.'

      write (36,*) ' IDMAT: ',(IDMAT(ICC,IX),IX=1,3)
      write (36,*) ' IMISCD: ',(IMISCD(ICC,IX),IX=1,2)
      write (36,*) ' DATU: ',(DATU(ICC,IX),IX=1,2)
      UACTL(IMISCD(ICC,2),1)=float(IDMAT(ICC,3))
      if (IDMAT(ICC,3).eq.1) then
        UACTL(IMISCD(ICC,2),2)=((DATU(ICC,1)/100.)*DIR)+1.0
      elseif (IDMAT(ICC,3).eq.2) then
        UACTL(IMISCD(ICC,2),2)=DATU(ICC,1)*DIR
      else
        UACTL(IMISCD(ICC,2),2)=0.
      endif
      UACTL(IMISCD(ICC,2),3)=IACT
      write (36,*) ' UACTL: ',(UACTL(IX,1),IX=1,7)
      write (36,*) ' UACTL: ',(UACTL(IX,2),IX=1,7)
      write (36,*) ' UACTL: ',(UACTL(IX,3),IX=1,7)
      write (36,*) ' UACTL: ',(UACTL(IX,4),IX=1,7)

      RETURN
      END


C ******************** UAT01a ********************
C UAT01a alters control data during simulation.

      SUBROUTINE UAT01a(IDAY,IHOUR)
#include "building.h"
C Common blocks UA3 UA4 within uncertainty.h
#include "uncertainty.h"
#include "control.h"

      COMMON/UA32/IMISCD(MNCNG,2)
      COMMON/UATD/UACLIM(6,3),UACTL(7,4),UAMCSA(MNACT)

      character ITEM(7)*20
      real ACNG

C Data stored in UA03 has the following meaning in this routine:
C IDMAT(?,1): Control function reference number.
C IDMAT(?,2): Control function day type.
C IMISCD(?,1): Control function period.
C IMISCD(?,2): Uncertain parameter.
C IDMAT(?,3): Change type (%; +/-; explicit).
C Define variables for current location (in time) and change.
      ITEM(1) ='Period start time   '
      ITEM(2) ='Maximum heating flux'
      ITEM(3) ='Minimum heating flux'
      ITEM(4) ='Maximum cooling flux'
      ITEM(5) ='Minimum cooling flux'
      ITEM(6) ='Heating set point   '
      ITEM(7) ='Cooling set point   '

      write (36,*) '   '
      write (36,*) 'Entering subroutine UAT01a',IDAY,IHOUR

C Write current data. Consider non-zero values of UACTL(?,1)
C where the ? index is the change topic
      if (nint(UACTL(1,1)).gt.0) then 
        ICC=IACTD(nint(UACTL(1,3)),1)
        ICL=IACTD(nint(UACTL(1,3)),2)
        ICF=IDMAT(ICC,1)
        ICD=IDMAT(ICC,2)
        ICP=IMISCD(ICC,1)
        write (36,*) 'ctl period',tbcps(ICF,ICD,ICP)
      endif
      if (nint(UACTL(2,1)).gt.0) then 
        ICC=IACTD(nint(UACTL(2,3)),1)
        ICL=IACTD(nint(UACTL(2,3)),2)
        ICF=IDMAT(ICC,1)
        ICD=IDMAT(ICC,2)
        ICP=IMISCD(ICC,1)
        write (36,*) 'heat max',bmiscd(ICF,ICD,ICP,2)
      endif
      if (nint(UACTL(3,1)).gt.0) then 
        ICC=IACTD(nint(UACTL(3,3)),1)
        ICL=IACTD(nint(UACTL(3,3)),2)
        ICF=IDMAT(ICC,1)
        ICD=IDMAT(ICC,2)
        ICP=IMISCD(ICC,1)
        write (36,*) 'heat min',bmiscd(ICF,ICD,ICP,3)
      endif
      if (nint(UACTL(4,1)).gt.0) then 
        ICC=IACTD(nint(UACTL(4,3)),1)
        ICL=IACTD(nint(UACTL(4,3)),2)
        ICF=IDMAT(ICC,1)
        ICD=IDMAT(ICC,2)
        ICP=IMISCD(ICC,1)
        write (36,*) 'cool max',bmiscd(ICF,ICD,ICP,4)
      endif
      if (nint(UACTL(5,1)).gt.0) then 
        ICC=IACTD(nint(UACTL(5,3)),1)
        ICL=IACTD(nint(UACTL(5,3)),2)
        ICF=IDMAT(ICC,1)
        ICD=IDMAT(ICC,2)
        ICP=IMISCD(ICC,1)
        write (36,*) 'cool min',bmiscd(ICF,ICD,ICP,5)
      endif
      if (nint(UACTL(6,1)).gt.0) then 
        ICC=IACTD(nint(UACTL(6,3)),1)
        ICL=IACTD(nint(UACTL(6,3)),2)
        ICF=IDMAT(ICC,1)
        ICD=IDMAT(ICC,2)
        ICP=IMISCD(ICC,1)
        write (36,*) 'heat SP',bmiscd(ICF,ICD,ICP,6)
      endif
      if (nint(UACTL(7,1)).gt.0) then 
        ICC=IACTD(nint(UACTL(7,3)),1)
        ICL=IACTD(nint(UACTL(7,3)),2)
        ICF=IDMAT(ICC,1)
        ICD=IDMAT(ICC,2)
        ICP=IMISCD(ICC,1)
        write (36,*) 'cool SP',bmiscd(ICF,ICD,ICP,7)
      endif
      do 10 I=1,7

C Check if control parameter is in valid time period.
        if (nint(UACTL(I,1)).eq.0) goto 10
        ICC=IACTD(nint(UACTL(I,3)),1)
        ICL=IACTD(nint(UACTL(I,3)),2)
        ICF=IDMAT(ICC,1)
        ICD=IDMAT(ICC,2)
        ICP=IMISCD(ICC,1)
        call UATFLG(IDAY,IHOUR,ICL,IVALID)

C If UACTL(?,4) not yet set then time to change value.
        if (IVALID.gt.nint(UACTL(I,4))) then
          UACTL(I,4)=float(IVALID)  ! set 4th slot

C Uncertainty defined and in valid time period.
          if (nint(UACTL(I,1)).eq.1) then ! % change
            PCNG=UACTL(I,2)
            if (I.eq.1) then  ! the period
              tbcps(ICF,ICD,ICP)=tbcps(ICF,ICD,ICP)*PCNG
            else
              bmiscd(ICF,ICD,ICP,I)=bmiscd(ICF,ICD,ICP,I)*PCNG
            endif
          elseif (nint(UACTL(I,1)).eq.2) then  ! abs change
            ACNG=UACTL(I,2)
            if (I.eq.1) then  ! the period
              tbcps(ICF,ICD,ICP)=tbcps(ICF,ICD,ICP)+ACNG
            else
              bmiscd(ICF,ICD,ICP,I)=bmiscd(ICF,ICD,ICP,I)+ACNG
            endif
          endif
        elseif (IVALID.lt.nint(UACTL(I,4))) then

C If IVALID has flipped back to zero then unset the prior change.
          UACTL(I,4)=float(IVALID)

C Uncertainty defined and leaving valid time period.
          if (nint(UACTL(I,1)).eq.1) then 
            PCNG=UACTL(I,2)*(-1.0)
            if (I.eq.1) then  ! the period
              tbcps(ICF,ICD,ICP)=tbcps(ICF,ICD,ICP)*PCNG
            else
              bmiscd(ICF,ICD,ICP,I)=bmiscd(ICF,ICD,ICP,I)*PCNG
            endif
          elseif (nint(UACTL(I,1)).eq.2) then 
            ACNG=UACTL(I,2)*(-1.0)
            if (I.eq.1) then  ! the period
              tbcps(ICF,ICD,ICP)=tbcps(ICF,ICD,ICP)+ACNG
            else
              bmiscd(ICF,ICD,ICP,I)=bmiscd(ICF,ICD,ICP,I)+ACNG
            endif
          endif
        endif
 10   continue

C Write new data.
      if (nint(UACTL(1,1)).gt.0) then 
        ICC=IACTD(nint(UACTL(1,3)),1)
        ICL=IACTD(nint(UACTL(1,3)),2)
        ICF=IDMAT(ICC,1)
        ICD=IDMAT(ICC,2)
        ICP=IMISCD(ICC,1)
        write (36,*) 'ctl period',tbcps(ICF,ICD,ICP)
      endif
      if (nint(UACTL(2,1)).gt.0) then 
        ICC=IACTD(nint(UACTL(2,3)),1)
        ICL=IACTD(nint(UACTL(2,3)),2)
        ICF=IDMAT(ICC,1)
        ICD=IDMAT(ICC,2)
        ICP=IMISCD(ICC,1)
        write (36,*) 'heat max',bmiscd(ICF,ICD,ICP,2)
      endif
      if (nint(UACTL(3,1)).gt.0) then 
        ICC=IACTD(nint(UACTL(3,3)),1)
        ICL=IACTD(nint(UACTL(3,3)),2)
        ICF=IDMAT(ICC,1)
        ICD=IDMAT(ICC,2)
        ICP=IMISCD(ICC,1)
        write (36,*) 'heat min',bmiscd(ICF,ICD,ICP,3)
      endif
      if (nint(UACTL(4,1)).gt.0) then 
        ICC=IACTD(nint(UACTL(4,3)),1)
        ICL=IACTD(nint(UACTL(4,3)),2)
        ICF=IDMAT(ICC,1)
        ICD=IDMAT(ICC,2)
        ICP=IMISCD(ICC,1)
        write (36,*) 'cool max',bmiscd(ICF,ICD,ICP,4)
      endif
      if (nint(UACTL(5,1)).gt.0) then 
        ICC=IACTD(nint(UACTL(5,3)),1)
        ICL=IACTD(nint(UACTL(5,3)),2)
        ICF=IDMAT(ICC,1)
        ICD=IDMAT(ICC,2)
        ICP=IMISCD(ICC,1)
        write (36,*) 'cool min',bmiscd(ICF,ICD,ICP,5)
      endif
      if (nint(UACTL(6,1)).gt.0) then 
        ICC=IACTD(nint(UACTL(6,3)),1)
        ICL=IACTD(nint(UACTL(6,3)),2)
        ICF=IDMAT(ICC,1)
        ICD=IDMAT(ICC,2)
        ICP=IMISCD(ICC,1)
        write (36,*) 'heat SP',bmiscd(ICF,ICD,ICP,6)
      endif
      if (nint(UACTL(7,1)).gt.0) then 
        ICC=IACTD(nint(UACTL(7,3)),1)
        ICL=IACTD(nint(UACTL(7,3)),2)
        ICF=IDMAT(ICC,1)
        ICD=IDMAT(ICC,2)
        ICP=IMISCD(ICC,1)
        write (36,*) 'cool SP',bmiscd(ICF,ICD,ICP,7)
      endif

      RETURN
      END


C ******************** UAT03 ********************
C UAT03 flags which climate data should be adjusted when reading from db.

      SUBROUTINE UAT03(IACT,DIR)
#include "building.h"
C Common blocks UA3 UA4 within uncertainty.h
#include "uncertainty.h"

      COMMON/UATD/UACLIM(6,3),UACTL(7,4),UAMCSA(MNACT)

      CHARACTER ITEM(6)*20

C Data stored in UA03 has the following meaning in this routine:
C IDMAT(?,1): Climate parameter reference number.
C IDMAT(?,3): Change type (%; +/-; explicit).
C Define variables for current location and change.
      ICC=IACTD(IACT,1)
      ICL=IACTD(IACT,2)
      ITEM(1) ='Dry bulb temperature'
      ITEM(2) ='Direct normal solar '
      ITEM(3) ='Diffuse horiz solar '
      ITEM(4) ='Wind speed          '
      ITEM(5) ='Wind direction      '
      ITEM(6) ='Relative humidity   '

      write (36,*) '   '
      write (36,*) 'Entering subroutine UAT03'
      write (36,*) ITEM(IDMAT(ICC,1)),' will be edited.'

      write (36,*) ' IDMAT: ',(IDMAT(ICC,IX),IX=1,3)
      UACLIM(IDMAT(ICC,1),1)=float(IDMAT(ICC,3))
      if (IDMAT(ICC,3).eq.1) then
        UACLIM(IDMAT(ICC,1),2)=((DATU(ICC,1)/100.)*DIR)+1.0
      elseif (IDMAT(ICC,3).eq.2) then
        UACLIM(IDMAT(ICC,1),2)=DATU(ICC,1)*DIR
      else
        UACLIM(IDMAT(ICC,1),2)=0.
      endif
      UACLIM(IDMAT(ICC,1),3)=ICL
      write (36,*) ' UACLIM: ',(UACLIM(IX,1),IX=1,6)
      write (36,*) ' UACLIM: ',(UACLIM(IX,2),IX=1,6)
      write (36,*) ' UACLIM: ',(UACLIM(IX,3),IX=1,6)

      RETURN
      END

C ******************** UAT03a ********************
C UAT03a alters climate data during simulation.

      SUBROUTINE UAT03a(IDAY)
#include "building.h"
#include "uncertainty.h"

      common/clim/idif(mt),itmp(mt),idnr(mt),ivel(mt),idir(mt),
     &            ihum(mt),idiff,itmpf,idnrf,ivelf,idirf,ihumf

      COMMON/UATD/UACLIM(6,3),UACTL(7,4),UAMCSA(MNACT)

C Data stored in UA03 has the following meaning in this routine:
C IDMAT(?,1): Climate parameter reference number.
C IDMAT(?,3): Change type (%; +/-; explicit).
C Define variables for current location (in time) and change.
      write (36,*) '   '
      write (36,*) 'Entering subroutine UAT03a for day',IDAY

C Set time marker to whole day (i.e. -1)
      IHOUR=-1

      if (nint(UACLIM(1,1)).gt.0) then
        write (36,'(a,24i5)') 'ambient',(itmp(IX),IX=1,24)
      endif
      if (nint(UACLIM(2,1)).gt.0) then
        write (36,'(a,24i5)') 'sol dir',(idnr(IX),IX=1,24)
      endif
      if (nint(UACLIM(3,1)).gt.0) then
        write (36,'(a,24i5)') 'sol dif',(idif(IX),IX=1,24)
      endif
      if (nint(UACLIM(4,1)).gt.0) then
        write (36,'(a,24i5)') 'wind vel',(ivel(IX),IX=1,24)
      endif
      if (nint(UACLIM(5,1)).gt.0) then
        write (36,'(a,24i5)') 'wind dir',(idir(IX),IX=1,24)
      endif
      if (nint(UACLIM(6,1)).gt.0) then
        write (36,'(a,24i5)') 'rel hum',(ihum(IX),IX=1,24)
      endif
      do 10 I=1,6

C Check if climate parameter is in valid time period.
        ILOC=nint(UACLIM(I,3))
        if (ILOC.eq.0) goto 10
        call UATFLG(IDAY,IHOUR,ILOC,IVALID)
        if (IVALID.eq.0) goto 10

C Uncertainty defined and in valid time period. Ensure that
C values are within range.
        if (nint(UACLIM(I,1)).eq.1) then 
          PCNG=UACLIM(I,2)
          if (I.eq.1) then
            itmpf=nint(float(itmpf)*PCNG)
          elseif (I.eq.2) then
            idnrf=nint(float(idnrf)*PCNG)
          elseif (I.eq.3) then
            idiff=nint(float(idiff)*PCNG)
          elseif (I.eq.4) then
            ivelf=nint(float(ivelf)*PCNG)
          elseif (I.eq.5) then
            idirf=nint(float(idirf)*PCNG)
            if (idirf.gt.359) idirf=idirf-360
            if (idirf.lt.0) idirf=idirf+360
          elseif (I.eq.6) then
            ihumf=nint(float(ihumf)*PCNG)
            if (ihumf.gt.100) ihumf=100
            if (ihumf.lt.0) ihumf=0
          endif
          do 20 IT=1,mt
            if (I.eq.1) then
              itmp(IT)=nint(float(itmp(IT))*PCNG)
            elseif (I.eq.2) then
              idnr(IT)=nint(float(idnr(IT))*PCNG)
            elseif (I.eq.3) then
              idif(IT)=nint(float(idif(IT))*PCNG)
            elseif (I.eq.4) then
              ivel(IT)=nint(float(ivel(IT))*PCNG)
            elseif (I.eq.5) then
              idir(IT)=nint(float(idir(IT))*PCNG)
              if (idir(IT).gt.359) idir(IT)=idir(IT)-360
              if (idir(IT).lt.0) idir(IT)=idir(IT)+360
            elseif (I.eq.6) then
              ihum(IT)=nint(float(ihum(IT))*PCNG)
              if (ihum(IT).gt.100) ihum(IT)=100
              if (ihum(IT).lt.0) ihum(IT)=0
            endif
 20       continue
        elseif (nint(UACLIM(I,1)).eq.2) then 
          ACNG=UACLIM(I,2)
          if (I.eq.1) then
            itmpf=nint(float(itmpf)+(ACNG*10))
          elseif (I.eq.2) then
            idnrf=nint(float(idnrf)+ACNG)
          elseif (I.eq.3) then
            idiff=nint(float(idiff)+ACNG)
          elseif (I.eq.4) then
            ivelf=nint(float(ivelf)+(ACNG*10))
          elseif (I.eq.5) then
            idirf=nint(float(idirf)+ACNG)
            if (idirf.gt.359) idirf=idirf-360
            if (idirf.lt.0) idirf=idirf+360
          elseif (I.eq.6) then
            ihumf=nint(float(ihumf)+ACNG)
            if (ihumf.gt.100) ihumf=100
            if (ihumf.lt.0) ihumf=0
          endif
          do 30 IT=1,mt
            if (I.eq.1) then
              itmp(IT)=nint(float(itmp(IT))+(ACNG*10))
            elseif (I.eq.2) then
              idnr(IT)=nint(float(idnr(IT))+ACNG)
            elseif (I.eq.3) then
              idif(IT)=nint(float(idif(IT))+ACNG)
            elseif (I.eq.4) then
              ivel(IT)=nint(float(ivel(IT))+(ACNG*10))
            elseif (I.eq.5) then 
              idir(IT)=nint(float(idir(IT))+ACNG)
              if (idir(IT).gt.359) idir(IT)=idir(IT)-360
              if (idir(IT).lt.0) idir(IT)=idir(IT)+360
            elseif (I.eq.6) then
              ihum(IT)=nint(float(ihum(IT))+ACNG)
              if (ihum(IT).gt.100) ihum(IT)=100
              if (ihum(IT).lt.0) ihum(IT)=0
            endif
 30       continue
        endif
 10   continue

C Show the updated values.
      if (nint(UACLIM(1,1)).gt.0) then
        write (36,'(a,24i5)') 'ambient',(itmp(IX),IX=1,24)
      endif
      if (nint(UACLIM(2,1)).gt.0) then
        write (36,'(a,24i5)') 'sol dir',(idnr(IX),IX=1,24)
      endif
      if (nint(UACLIM(3,1)).gt.0) then
        write (36,'(a,24i5)') 'sol dif',(idif(IX),IX=1,24)
      endif
      if (nint(UACLIM(4,1)).gt.0) then
        write (36,'(a,24i5)') 'wind vel',(ivel(IX),IX=1,24)
      endif
      if (nint(UACLIM(5,1)).gt.0) then
        write (36,'(a,24i5)') 'wind dir',(idir(IX),IX=1,24)
      endif
      if (nint(UACLIM(6,1)).gt.0) then
        write (36,'(a,24i5)') 'rel hum',(ihum(IX),IX=1,24)
      endif

      RETURN
      END
