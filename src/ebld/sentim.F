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

C This file contains the following routines:
C  UATCHK:   Checks if time based uncertainty active.
C  UAT01:    Edits control information based on time.
C  UAT01a:   Alters control data during simulation.
C  UAT03:    Edits data: Climate parameters - stores changes
C  UAT03a:   Edits data: Climate parameters - uses changes when a days data is read in.

C ******************** UATFLG ********************
C UATFLG determines if control data is within NTG start day/time and
C NTG finish day/time.
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

      COMMON/UATD/UACLIM(6,4),UACTL(MNACT,7,6),UAMCSA(MNACT)

      character USETNAME*96   ! the synopsis for each MC-run
      character uheader*1000  ! header including name of each uncertainty changed
      character urunvals*1000 ! attribute changed-to value of each uncertainty change for the run.
      COMMON/USETN/USETNAME(MNRS),uheader,urunvals

C Logical flag for uncertainty header has been printed.
      logical uncertheadprinted
      common/uncerthead/uncertheadprinted

      logical headerdone
      logical itemunique  ! should we include in fort.37
      logical isunique    ! for test against past
      integer itemsinheader,nbpastitems,nbdatahits
      character pastphrase*42
      common/uhead/headerdone,itemunique(400),pastphrase(400),
     &  itemsinheader,nbpastitems,nbdatahits

      integer lstur, lfnur
      character strval*12,uhphrase*42,uhroot*42
      character urunvalsd*1000
      character ubasecase*1000,ubasecased*1000,baseval*4

      CHARACTER ITEM(7)*20

C Data stored in UA3 has the following meaning in this routine:
C IDATR(?,1): Control function reference number.
C IDATR(?,2): Control function day type.
C IDATR(?,3): Change type (%; +/-; explicit).
C IDATR(?,4): Control function period.
C IDATR(?,5): Uncertain parameter.
C Define variables for current location and change.
      if(urunvals(1:2).eq.'  ')then
        lstur=1; lfnur=1  ! reset counters for urunvals buffer
      else
        lstur=lnblnk(urunvals)+2; lfnur=lnblnk(urunvals)+2  ! to append to urunvals buffer
      endif
      ICC=IACTD(IACT,1)
      ICL=IACTD(IACT,2)
      itopic= IDATR(ICC,5)  ! which of the 7 topics
      ICF=IDATR(ICC,1)
      ICP=IDATR(ICC,4)

C If initial append to uheader.
      if(uheader(1:2).eq.'  ')then
        lstuh=1; lfnuh=1  ! reset counters for uheader buffer
      else
        lstuh=lnblnk(uheader)+2; lfnuh=lnblnk(uheader)+2  ! to append to uheader buffer
      endif

C And for the Delta values.
      if(urunvals(1:2).eq.'  ')then
        lstur=1; lfnur=1  ! reset counters for urunvals buffer
      else
        lstur=lnblnk(urunvals)+2; lfnur=lnblnk(urunvals)+2  ! to append to urunvals buffer
      endif

      ITEM(1) ='Period start time'
      ITEM(2) ='Maximum heating flux'
      ITEM(3) ='Minimum heating flux'
      ITEM(4) ='Maximum cooling flux'
      ITEM(5) ='Minimum cooling flux'
      ITEM(6) ='Heating set point'
      ITEM(7) ='Cooling set point'

      write (36,*) '   '
      write (36,*) 'Entering subroutine UAT01',' distr',ICC,' loc',ICL
      write (36,*) ITEM(itopic),itopic,' will be edited.'

      write (36,*) ' IDATR: ctl func & day type & change typ',
     &  (IDATR(ICC,IX),IX=1,3)
      write (36,*) ' IDATR: func period & parameter',
     &  (IDATR(ICC,IX),IX=4,5)
      write (36,*) ' DATU: ',(DATU(ICC,IX),IX=1,2)

C UACTL (iact,?,1)=change type (%,+/-); (?,2)=magnitude; (?,3)=associated action;
C UACTL (?,?,4)=time status (0 not active, 1 active)
C UACTL (?,?,5)=counter for the uncertainty action being dealt with
C UACTL (?,?,6)=value of DIR
      UACTL(iact,itopic,1)=float(IDATR(ICC,3))
      UACTL(iact,itopic,5)=float(IACT)  ! which of the uncertainty tasks
      if (IDATR(ICC,3).eq.1) then
        UACTL(iact,itopic,2)=((DATU(ICC,1)/100.)*DIR)+1.0
      elseif (IDATR(ICC,3).eq.2) then
        UACTL(iact,itopic,2)=DATU(ICC,1)*DIR
      else
        UACTL(iact,itopic,2)=0.
      endif
      UACTL(iact,itopic,3)=IACT
      UACTL(iact,itopic,6)=DIR
      write (36,'(a,7f8.3)')' UACTL: chg type',(UACTL(iact,IX,1),IX=1,7)
      write (36,'(a,7f8.3)')' UACTL: value   ',(UACTL(iact,IX,2),IX=1,7)
      write (36,'(a,7f8.3)')' UACTL: ',(UACTL(iact,IX,3),IX=1,7)
      write (36,'(a,7f8.3)')' UACTL: time sta',(UACTL(iact,IX,4),IX=1,7)
      write (36,'(a,7f8.3)')' UACTL: iact    ',(UACTL(iact,IX,5),IX=1,7)
      write (36,'(a,7f8.3)')' UACTL: DIR     ',(UACTL(iact,IX,6),IX=1,7)

C Make specific header string for each change (to match urunvals)
      lncng=lnblnk(LCNG(ICC))
      lnloc=lnblnk(LLOC(ICL))
      if(lncng.gt.18)lncng=18
      uhphrase=' '
      write(uhphrase,'(4a,i2.2,a,i2.2,a)')LCNG(ICC)(1:lncng),
     &  ':',LLOC(ICL)(1:lnloc),':',ICF,':',ICP,','
      luhphrase=lnblnk(uhphrase)
      write(36,'(2a)') 'UAT01 ',uhphrase(1:luhphrase)

C Remember the root name of the phrase so that subsequent duplicates can
C be ignored in the fort.37 file. If the initial item the increment counters
C otherwise search pastphrase(s) for match.
      uhroot=' '
      write(uhroot,'(4a)')LCNG(ICC)(1:lncng),':',
     &                    LLOC(ICL)(1:lnloc),','
      luhroot=lnblnk(uhroot)
      write(36,'(2a)') 'UAT01 ',uhroot(1:luhroot)
      if(itemsinheader.eq.0)then
        nbpastitems=nbpastitems+1
        write(pastphrase(nbpastitems),'(a)') uhroot(1:luhroot)
        itemunique(1)=.TRUE.
        write(36,*) 'UAT01 unique item',nbpastitems,uhroot(1:luhroot)
      elseif(itemsinheader.eq.1)then
        lnpast=lnblnk(pastphrase(1))
        if(uhroot(1:luhroot).eq.pastphrase(1)(1:lnpast))then
          itemunique(itemsinheader+1)=.FALSE.
        else
          itemunique(itemsinheader+1)=.TRUE.
          nbpastitems=nbpastitems+1
          write(pastphrase(nbpastitems),'(a)') uhroot(1:luhroot)
          write(36,*)'UAT01 unique item',nbpastitems,uhroot(1:luhroot)
        endif
      else

C If we ever get a match unset isunique.
        isunique=.true.
        do loop=1,nbpastitems
          lnpast=lnblnk(pastphrase(loop))
          if(uhroot(1:luhroot).eq.
     &       pastphrase(loop)(1:lnpast))then
            isunique=.false.
          endif
        enddo
        if(isunique)then
          itemunique(itemsinheader+1)=.TRUE.
          nbpastitems=nbpastitems+1
          write(pastphrase(nbpastitems),'(a)')
     &      uhroot(1:luhroot)
          write(36,*)'unique item',nbpastitems,
     &      uhroot(1:luhroot)
        endif
      endif

C If unique then include it in the header.
      write(36,*) 'itemunique array ',itemunique
      if(itemunique(itemsinheader+1))then
        lfnuh=lstuh+luhroot
        if(lfnuh.lt.1000)then
          write(uheader(lstuh:lfnuh),'(a)') uhroot(1:luhroot)
          lstuh=lfnuh
          if(.NOT.headerdone)itemsinheader=itemsinheader+1
        endif
      else
        if(.NOT.headerdone)itemsinheader=itemsinheader+1
      endif

C Add to urunvals
      strval=' '        ! clear string buffer for the DIR value
      call relstr(DIR,strval,lna,ier)
      lstrval=lnblnk(strval)
      lfnur=lstur+lstrval
      if(lfnur.lt.1000)then
        write(urunvals(lstur:lfnur),'(a)') strval(1:lstrval)
        lstur=lfnur+2
        write(36,'(a,i3,2a)') 'contents of urunvals @',iact,
     &    ' is ',urunvals(1:lnblnk(urunvals))
      endif

C If this was the last uncertainty i.e. if iact=NIACT header is done.
      if(iact.eq.NIACT)then
        write(36,*) 'set header buffer done UAT01 ',
     &    itemsinheader,iact
        headerdone=.true.
      endif

C Write current header buffer and values buffer. Test if the
C whole header might have been written.  Remove the trailing
C comma from the initial line of fort.37.
      if(headerdone)then
        if(.NOT.uncertheadprinted)then
          lnhead=lnblnk(uheader)
          write(36,'(2a)') 'UAT01 writing: ',uheader(1:lnhead-1)
          write(37,'(a)') uheader(1:lnhead-1)
          uncertheadprinted=.true.
 
C Write out one line of zeros to represent the base case.
          ubasecase='  '; ubasecased='  '; baseval='0.00'
          lbase=4; lstbase=1; lfnbase=1

C Debug.
C          write(6,*) 'itemunique ',itemunique
          do loop=1,itemsinheader
            lfnbase=lstbase+lbase
            if(lfnbase.lt.1000)then
              write(ubasecase(lstbase:lfnbase),'(a)') baseval(1:lbase)
              lstbase=lfnbase+2
            endif
          enddo
          call SDELIM(ubasecase,ubasecased,'C',IW)
          lnbase=lnblnk(ubasecased)
          write(37,'(a)') ubasecased(1:lnbase-1)
        endif

C Add comma separations to the data and write it but do not
C clear urunvals
        if(iact.eq.NIACT)then
          call SDELIM(urunvals,urunvalsd,'C',IW)
          write(36,'(2a)') 'UAT01a ',urunvalsd(1:lnblnk(urunvalsd))
C          write(37,'(a)') urunvalsd(1:lnblnk(urunvalsd))
C          urunvals='  '  ! clear string buffer prior to next run
C          urunvalsd='  '
        endif
      endif
      RETURN
      END


C ******************** UAT01a ********************
C UAT01a alters control data during simulation.
C DIR is embedded within 
      SUBROUTINE UAT01a(IACT,IDAY,IHOUR)

#include "building.h"
#include "uncertainty.h"
#include "control.h"

      COMMON/UATD/UACLIM(6,4),UACTL(MNACT,7,6),UAMCSA(MNACT)

      character ITEM(7)*20
      real ACNG
      
C Data stored in UA03 has the following meaning in this routine:
C IDATR(?,1) (ICF): Control function reference number.
C IDATR(?,2) (ICD): Control function day type.
C IDATR(?,4) (ICP): Control function period.
C IDATR(?,5): Uncertain parameter.
C IDATR(?,3): Change type (%; +/-; explicit).
C Define variables for current location (in time) and change.
      ITEM(1) ='Period start time   '
      ITEM(2) ='Maximum heating flux'
      ITEM(3) ='Minimum heating flux'
      ITEM(4) ='Maximum cooling flux'
      ITEM(5) ='Minimum cooling flux'
      ITEM(6) ='Heating set point   '
      ITEM(7) ='Cooling set point   '

C Check for various iact related to control.
      do 10 I=1,7

C Check if control parameter is in valid time period.
        if (nint(UACTL(iact,I,1)).eq.0) goto 10

        ICC=IACTD(nint(UACTL(iact,I,3)),1)
        ICL=IACTD(nint(UACTL(iact,I,3)),2)
        ICF=IDATR(ICC,1)
        ICD=IDATR(ICC,2)
        ICP=IDATR(ICC,4)
        call UATFLG(IDAY,IHOUR,ICL,IVALID)

C If UACTL(iact,?,4) not yet set then time to change value.
        if (IVALID.gt.nint(UACTL(iact,I,4))) then
          UACTL(iact,I,4)=float(IVALID)  ! set 4th slot
          write (36,*) '   '
          write (36,'(a,7i4,f5.2)') 
     &      'Entering subroutine UAT01a topic day hour dis loc',
     &      I,IDAY,IHOUR,ICC,ICL,ICD,ICP,UACTL(iact,I,3)

          if(I.eq.1)then
            write (36,'(a,f7.2)')ITEM(I)(1:lnblnk(ITEM(I))),
     &        tbcps(ICF,ICD,ICP)
          else
            write (36,'(a,f7.2)')ITEM(I)(1:lnblnk(ITEM(I))),
     &        bmiscd(ICF,ICD,ICP,I)
          endif

C Uncertainty defined and in valid time period.
          if (nint(UACTL(iact,I,1)).eq.1) then ! % change
            PCNG=UACTL(iact,I,2)
            if (I.eq.1) then  ! the period
              tbcps(ICF,ICD,ICP)=tbcps(ICF,ICD,ICP)*PCNG
            else
              bmiscd(ICF,ICD,ICP,I)=bmiscd(ICF,ICD,ICP,I)*PCNG
            endif
          elseif (nint(UACTL(iact,I,1)).eq.2) then  ! abs change
            ACNG=UACTL(iact,I,2)
            if (I.eq.1) then  ! the period
              tbcps(ICF,ICD,ICP)=tbcps(ICF,ICD,ICP)+ACNG
            else
              bmiscd(ICF,ICD,ICP,I)=bmiscd(ICF,ICD,ICP,I)+ACNG
            endif
          endif
          if (nint(UACTL(iact,I,1)).gt.0) then   ! echo the updated value
            if(I.eq.1)then
              write (36,'(a,f7.2)')ITEM(I)(1:lnblnk(ITEM(I))),
     &          tbcps(ICF,ICD,ICP)
            else
              write (36,'(a,f7.2)')ITEM(I)(1:lnblnk(ITEM(I))),
     &          bmiscd(ICF,ICD,ICP,I)
            endif
          endif

        elseif (IVALID.lt.nint(UACTL(iact,I,4))) then

C If IVALID has flipped back to zero then unset the prior change.
          UACTL(iact,I,4)=float(IVALID)
          write (36,*) '   '
          write (36,*) 'Entering subroutine UAT01a',IDAY,IHOUR
          if (nint(UACTL(iact,I,1)).gt.0) then   ! echo the before value
            ICC=IACTD(nint(UACTL(iact,I,3)),1)
            ICL=IACTD(nint(UACTL(iact,I,3)),2)
            ICF=IDATR(ICC,1)
            ICD=IDATR(ICC,2)
            ICP=IDATR(ICC,4)
            if(I.eq.1)then
              write (36,'(a,f7.2)')ITEM(I)(1:lnblnk(ITEM(I))),
     &          tbcps(ICF,ICD,ICP)
            else
              write (36,'(a,f7.2)')ITEM(I)(1:lnblnk(ITEM(I))),
     &          bmiscd(ICF,ICD,ICP,I)
            endif
          endif

C Uncertainty defined and leaving valid time period.
          if (nint(UACTL(iact,I,1)).eq.1) then 
            PCNG=UACTL(iact,I,2)*(-1.0)
            if (I.eq.1) then  ! the period
              tbcps(ICF,ICD,ICP)=tbcps(ICF,ICD,ICP)*PCNG
            else
              bmiscd(ICF,ICD,ICP,I)=bmiscd(ICF,ICD,ICP,I)*PCNG
            endif
          elseif (nint(UACTL(iact,I,1)).eq.2) then 
            ACNG=UACTL(iact,I,2)*(-1.0)
            if (I.eq.1) then  ! the period
              tbcps(ICF,ICD,ICP)=tbcps(ICF,ICD,ICP)+ACNG
            else
              bmiscd(ICF,ICD,ICP,I)=bmiscd(ICF,ICD,ICP,I)+ACNG
            endif
          endif
          if (nint(UACTL(iact,I,1)).gt.0) then   ! echo the updated value
            if(I.eq.1)then
              write (36,'(a,f7.2)')ITEM(I)(1:lnblnk(ITEM(I))),
     &          tbcps(ICF,ICD,ICP)
            else
              write (36,'(a,f7.2)')ITEM(I)(1:lnblnk(ITEM(I))),
     &          bmiscd(ICF,ICD,ICP,I)
            endif
          endif
        endif
 10   continue

      RETURN
      END


C ******************** UAT03 ********************
C UAT03 flags which climate data should be adjusted when reading from db.

      SUBROUTINE UAT03(IACT,DIR)
#include "building.h"
C Common blocks UA3 UA4 within uncertainty.h
#include "uncertainty.h"

      COMMON/UATD/UACLIM(6,4),UACTL(MNACT,7,6),UAMCSA(MNACT)

      CHARACTER ITEM(6)*20

C Data stored in UA03 has the following meaning in this routine:
C IDATR(?,1): Climate parameter reference number.
C IDATR(?,3): Change type (%; +/-; explicit).
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
      write (36,*) ITEM(IDATR(ICC,1)),' will be edited.'

      write (36,*) ' IDATR: ',(IDATR(ICC,IX),IX=1,3)
      UACLIM(IDATR(ICC,1),1)=float(IDATR(ICC,3))
      UACLIM(IDATR(ICC,1),4)=float(IACT)  ! remember which task
      if (IDATR(ICC,3).eq.1) then
        UACLIM(IDATR(ICC,1),2)=((DATU(ICC,1)/100.)*DIR)+1.0
      elseif (IDATR(ICC,3).eq.2) then
        UACLIM(IDATR(ICC,1),2)=DATU(ICC,1)*DIR
      else
        UACLIM(IDATR(ICC,1),2)=0.
      endif
      UACLIM(IDATR(ICC,1),3)=ICL
      write (36,*) ' UACLIM: ',(UACLIM(IX,1),IX=1,6)
      write (36,*) ' UACLIM: ',(UACLIM(IX,2),IX=1,6)
      write (36,*) ' UACLIM: ',(UACLIM(IX,3),IX=1,6)
      write (36,*) ' UACLIM: ',(UACLIM(IX,4),IX=1,6)

C << Potentially could put the fort.37 calls at this point
C << because this is called at the start of the set.

      RETURN
      END

C ******************** UAT03a ********************
C UAT03a alters climate data during simulation.
C Note: this will write information once per day. Logic does not
C yet sort out the writing of intermediate states from the final
C urunvals and also the intermediate state of uheader.

      SUBROUTINE UAT03a(IDAY)
#include "building.h"
#include "uncertainty.h"

      common/clim/idif(mt),itmp(mt),idnr(mt),ivel(mt),idir(mt),
     &            ihum(mt),idiff,itmpf,idnrf,ivelf,idirf,ihumf

      COMMON/UATD/UACLIM(6,4),UACTL(MNACT,7,6),UAMCSA(MNACT)

      character USETNAME*96   ! the synopsis for each MC-run
      character uheader*1000  ! header including name of each uncertainty changed
      character urunvals*1000 ! attribute changed-to value of each uncertainty change for the run.
      COMMON/USETN/USETNAME(MNRS),uheader,urunvals

      logical uncertheadprinted
      common/uncerthead/uncertheadprinted

      logical headerdone
      logical itemunique  ! should we include in fort.37
      integer itemsinheader,nbpastitems,nbdatahits
      character pastphrase*42
      common/uhead/headerdone,itemunique(400),pastphrase(400),
     &  itemsinheader,nbpastitems,nbdatahits

      integer lstur, lfnur
      character strval*10,uhphrase*42,uhroot*42
      character urunvalsd*1000
      character ubasecase*1000,ubasecased*1000,baseval*4

      if(urunvals(1:2).eq.'  ')then
        lstur=1; lfnur=1  ! reset counters for urunvals buffer
      else
        lstur=lnblnk(urunvals)+2; lfnur=lnblnk(urunvals)+2  ! to append to urunvals buffer
      endif

C If initial append to uheader.
      if(uheader(1:2).eq.'  ')then
        lstuh=1; lfnuh=1  ! reset counters for uheader buffer
      else
        lstuh=lnblnk(uheader)+2; lfnuh=lnblnk(uheader)+2  ! to append to uheader buffer
      endif

C Data stored in UA03 has the following meaning in this routine:
C IDATR(?,1): Climate parameter reference number.
C IDATR(?,3): Change type (%; +/-; explicit).
C UACLIM(?,4): which of the uncertainty tasks is being processed
C Define variables for current location (in time) and change.
      write (36,*) '   '
      write (36,*) 'Entering subroutine UAT03a for day',IDAY

C Set time marker to whole day (i.e. -1)
      IHOUR=-1

      if (nint(UACLIM(1,1)).gt.0) then
        write (36,'(a,24i5)') 'ambient',(itmp(IX),IX=1,24)

C Make specific header string for each change (to match urunvals)

C << ??? why 24 separate values for the header? >>

        do ih=1,24
          write(uhphrase,'(a,i2.2,a)') 'amb_db',ih,','
          luhphrase=8
          lfnuh=lstuh+luhphrase
          if(lfnuh.lt.1000)then
            write(uheader(lstuh:lfnuh),'(a)')uhphrase(1:luhphrase)
            lstuh=lfnuh+1
            itemsinheader=itemsinheader+1
          endif
        enddo

C << If we have a mix of uncertainties and this is not the last
C << one then perhaps this should not be set??

        if(headerdone)then
          if(.NOT.uncertheadprinted)then
            write (37,'(2a)') 'uheader ',uheader(1:lnblnk(uheader))
            uncertheadprinted=.true.
          endif
        endif
      endif
      if (nint(UACLIM(2,1)).gt.0) then
        write (36,'(a,24i5)') 'sol dir',(idnr(IX),IX=1,24)
        do ih=1,24
          write(uhphrase,'(a,i2.2,a)') 'sol_dir',ih,','
          luhphrase=9
          lfnuh=lstuh+luhphrase
          if(lfnuh.lt.1000)then
            write(uheader(lstuh:lfnuh),'(a)')uhphrase(1:luhphrase)
            lstuh=lfnuh+1
            itemsinheader=itemsinheader+1
          endif
        enddo
        if(headerdone)then
          if(.NOT.uncertheadprinted)then
            write (37,'(2a)') 'uheader ',uheader(1:lnblnk(uheader))
            uncertheadprinted=.true.
          endif
        endif
      endif
      if (nint(UACLIM(3,1)).gt.0) then
        write (36,'(a,24i5)') 'sol dif',(idif(IX),IX=1,24)
        do ih=1,24
          write(uhphrase,'(a,i2.2,a)') 'sol_dif',ih,','
          luhphrase=9
          lfnuh=lstuh+luhphrase
          if(lfnuh.lt.1000)then
            write(uheader(lstuh:lfnuh),'(a)')uhphrase(1:luhphrase)
            lstuh=lfnuh+1
            itemsinheader=itemsinheader+1
          endif
        enddo
        if(headerdone)then
          if(.NOT.uncertheadprinted)then
            write (37,'(2a)') 'uheader ',uheader(1:lnblnk(uheader))
            uncertheadprinted=.true.
          endif
        endif
      endif
      if (nint(UACLIM(4,1)).gt.0) then
        write (36,'(a,24i5)') 'wind vel',(ivel(IX),IX=1,24)
        do ih=1,24
          write(uhphrase,'(a,i2.2,a)') 'wind_v',ih,','
          luhphrase=8
          lfnuh=lstuh+luhphrase
          if(lfnuh.lt.1000)then
            write(uheader(lstuh:lfnuh),'(a)')uhphrase(1:luhphrase)
            lstuh=lfnuh+1
            itemsinheader=itemsinheader+1
          endif
        enddo
        if(headerdone)then
          if(.NOT.uncertheadprinted)then
            write (37,'(2a)') 'uheader ',uheader(1:lnblnk(uheader))
            uncertheadprinted=.true.
          endif
        endif
      endif
      if (nint(UACLIM(5,1)).gt.0) then
        write (36,'(a,24i5)') 'wind dir',(idir(IX),IX=1,24)
        do ih=1,24
          write(uhphrase,'(a,i2.2,a)') 'wind_d',ih,','
          luhphrase=8
          lfnuh=lstuh+luhphrase
          if(lfnuh.lt.1000)then
            write(uheader(lstuh:lfnuh),'(a)')uhphrase(1:luhphrase)
            lstuh=lfnuh+1
            itemsinheader=itemsinheader+1
          endif
        enddo
        if(headerdone)then
          if(.NOT.uncertheadprinted)then
            write (37,'(2a)') 'uheader ',uheader(1:lnblnk(uheader))
            uncertheadprinted=.true.
          endif
        endif
      endif
      if (nint(UACLIM(6,1)).gt.0) then
        write (36,'(a,24i5)') 'rel hum',(ihum(IX),IX=1,24)
        do ih=1,24
          write(uhphrase,'(a,i2.2,a)') 'rel_hu',ih,','
          luhphrase=8
          lfnuh=lstuh+luhphrase
          if(lfnuh.lt.1000)then
            write(uheader(lstuh:lfnuh),'(a)')uhphrase(1:luhphrase)
            lstuh=lfnuh+1
            itemsinheader=itemsinheader+1
          endif
        enddo
        if(headerdone)then
          if(.NOT.uncertheadprinted)then
            write (37,'(2a)') 'uheader ',uheader(1:lnblnk(uheader))
            uncertheadprinted=.true.
          endif
        endif
      endif

C Test if this is the last uncertainty item, if so header is complete.
      if(.NOT.headerdone)then
        do I=1,6
          if (nint(UACLIM(I,4)).eq.NIACT)then
            headerdone=.true.
C            write(36,*) 'set headerdone in UAT03a'
          endif
        enddo
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
          strval=' '        ! clear string buffer for the value changed
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
              call INTSTR(itmp(IT),strval,ISWD,IFLAG)
            elseif (I.eq.2) then
              idnr(IT)=nint(float(idnr(IT))*PCNG)
              call INTSTR(idnr(IT),strval,ISWD,IFLAG)
             elseif (I.eq.3) then
              idif(IT)=nint(float(idif(IT))*PCNG)
              call INTSTR(idif(IT),strval,ISWD,IFLAG)
            elseif (I.eq.4) then
              ivel(IT)=nint(float(ivel(IT))*PCNG)
              call INTSTR(ivel(IT),strval,ISWD,IFLAG)
            elseif (I.eq.5) then
              idir(IT)=nint(float(idir(IT))*PCNG)
              if (idir(IT).gt.359) idir(IT)=idir(IT)-360
              if (idir(IT).lt.0) idir(IT)=idir(IT)+360
              call INTSTR(idir(IT),strval,ISWD,IFLAG)
            elseif (I.eq.6) then
              ihum(IT)=nint(float(ihum(IT))*PCNG)
              if (ihum(IT).gt.100) ihum(IT)=100
              if (ihum(IT).lt.0) ihum(IT)=0
              call INTSTR(ihum(IT),strval,ISWD,IFLAG)
            endif

C Add to urunvals
            lstrval=lnblnk(strval)
            lfnur=lstur+lstrval
            if(lfnur.lt.1000)then
              write(urunvals(lstur:lfnur),'(a)') strval(1:lstrval)
              lstur=lfnur+2
            endif
 20       continue

C Add comma separations to the data.
          call SDELIM(urunvals,urunvalsd,'C',IW)
          write (37,'(2a)') 'urunvals ',urunvalsd(1:lnblnk(urunvalsd))
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
              call INTSTR(itmp(IT),strval,ISWD,IFLAG)
            elseif (I.eq.2) then
              idnr(IT)=nint(float(idnr(IT))+ACNG)
              call INTSTR(idnr(IT),strval,ISWD,IFLAG)
            elseif (I.eq.3) then
              idif(IT)=nint(float(idif(IT))+ACNG)
              call INTSTR(idif(IT),strval,ISWD,IFLAG)
            elseif (I.eq.4) then
              ivel(IT)=nint(float(ivel(IT))+(ACNG*10))
              call INTSTR(ivel(IT),strval,ISWD,IFLAG)
            elseif (I.eq.5) then 
              idir(IT)=nint(float(idir(IT))+ACNG)
              if (idir(IT).gt.359) idir(IT)=idir(IT)-360
              if (idir(IT).lt.0) idir(IT)=idir(IT)+360
              call INTSTR(idir(IT),strval,ISWD,IFLAG)
            elseif (I.eq.6) then
              ihum(IT)=nint(float(ihum(IT))+ACNG)
              if (ihum(IT).gt.100) ihum(IT)=100
              if (ihum(IT).lt.0) ihum(IT)=0
              call INTSTR(ihum(IT),strval,ISWD,IFLAG)
            endif

C Add to urunvals
            lstrval=lnblnk(strval)
            lfnur=lstur+lstrval
            if(lfnur.lt.1000)then
              write(urunvals(lstur:lfnur),'(a)') strval(1:lstrval)
              lstur=lfnur+2
            endif
 30       continue
          call SDELIM(urunvals,urunvalsd,'C',IW)
          write (37,'(2a)') 'urunvals ',urunvalsd(1:lnblnk(urunvalsd))
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
