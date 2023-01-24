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
C   SENRNS Asks for the preferred SA technique & runs to do
C   SENSA   The main uncertainty analysis controlling rtn.
C   DSA     Changes data for a Differential Sensitivity Analysis.
C   MCSA    Changes data for a Monte-Carlo Sensitivity Analysis.
C   UAE01   Edits data: conduct, density, specific ht, emissivity and 
C                       absorbtivity, dif res(not yet) - with error checking.
C   UAE02   Edits data: thickness and whole MLC's.
C   UAE04   Edits data: Casual gains.
C   UAE05   Edits data: Convection correlations.
C   UAE06   Edits data: Optical properties in zone TMC.
C
C   UAZ01   Edits calculated zone parameters/ assumptions relating to air 
C
C   UAT01   Edits control data.
C   UAT03   Edits climate data.

C ******************** SENRNS ********************
C SENRNS asks for the preferred SA technique and calculates the number of 
C simulations to be run.

      SUBROUTINE SENRNS(IER)
#include "building.h"
#include "uncertainty.h"
#include "help.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL

      COMMON/UA5/IMET,ISIM,NRSIM
      COMMON/UAMET/IUNCMET,nbsets
      COMMON/UAFCT/IADIR(MNFA),NFA

C Uncertainties
      character USETNAME*96   ! the synopsis for each MC-run
      character uheader*1000  ! header including name of each uncertainty changed
      character urunvals*1000 ! attribute changed-to value of each uncertainty
                              ! change for the run.
      COMMON/USETN/USETNAME(MNRS),uheader,urunvals

C Logical flag for uncertainty header has been printed.
      logical uncertheadprinted
      common/uncerthead/uncertheadprinted
      logical headerdone
      logical itemunique  ! should we include in fort.37
      integer itemsinheader,nbpastitems,nbdatahits
      character pastphrase*42
      common/uhead/headerdone,itemunique(400),pastphrase(400),
     &  itemsinheader,nbpastitems,nbdatahits

      character ITEM(4)*18,outs*124

      DIMENSION IVAL(4)

      helpinsub='sensit'  ! set for subroutine
      helptopic='select_sensit_method'
      call gethelptext(helpinsub,helptopic,nbhelp)

      IER=0

C Is analysis method predefined? (passed in via command line).
 10   if (IUNCMET.gt.0) then
        IVAL(1)=IUNCMET
        INPIC=1
        NRSIM=nbsets
      else

C Ask for uncertainty analysis method first.

C << can we offer an option for do-base-case-only ?? >>

        call edisp(iuout,' Available analysis techniques... ')
        ITEM(1) ='Differential'
        ITEM(2) ='Factorial  '
        ITEM(3) ='Monte-Carlo '
        ITEM(4) ='Run base case only'
        NITMS=4

        INPIC=1
        CALL EPICKS(INPIC,IVAL,' ','Which analysis type:',
     &    18,NITMS,ITEM,'Analysis methods',IERR,nbhelp)

C Check for errors - try again if error has occurred.
        if (IERR.ne.0) then
          call edisp(iuout,'Error occurred - try again.')
          call edisp(iuout,'  ')
          goto 10
        endif

C If no type has been selected then return to main menu.
        if (INPIC.eq.0) then
          IER=1
          NRSIM=1
          return
        endif
        if(IVAL(1).eq.4)then  ! only do base case run
          IER=2
          NRSIM=1
          return
        endif
      endif

C An uncertainty analysis technique has been chosen write to file 
C and set number of simulations. Note the first set is always the
C base case.
      IUNIT=IFIL+2
      IREC=1
      IMET=IVAL(1)
      read (IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM,nzrl,izver
      write(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM,nzrl,izver,IMET

C Clear the header text buffer and counter of uncertainty items so that
C it is only written once at the top of the fort.37 file. The following
C block of code deals with up to 400 uncertainty edits.
      uheader='  '
      urunvals='  '
      headerdone=.false.; itemsinheader=0; nbpastitems=0
      uncertheadprinted=.false.; nbdatahits=0
      do loop=1,400
        itemunique(loop)=.FALSE.
        pastphrase(loop)='  '
      enddo
      write(36,*)'Clearing uheader & headerdone & itemsinheader'

 20   if (IMET.eq.1) then 

C Diferential. Set IMET and NSRIM.
        NRSIM=1+(2*NIACT)
        return
      elseif (IMET.eq.2) then 

C Factorial method. First check if there is space for all the runs.
        if (NIACT.gt.MNFA) then
          call edisp(iuout,'Too many uncertainties for this method,')
          call edisp(iuout,'please choose another analysis method.')
          goto 10
        endif
        NRSIM=2**NIACT
        NFA=NIACT
        return
      elseif (IMET.eq.3) then 

C Monte-Carlo. Set IMET and NSRIM unless these have already been
C passed via command line.
        if(IUNCMET.gt.0.and.nbsets.gt.1) then
          NRSIM=nbsets
        else
          NRSIM=10*NIACT  ! 10 times the number of uncertainties
          call EASKI(NRSIM,' ','Required number of simulations?',
     &      70,'W',MNRS,'F',MNRS,'No. of simulations',IER,nbhelp)
        endif

C Seen random number generator.
        call getsecs(istime)
        x= rand(istime)
C For NT.
C          call set_seed@(seed)
C          x=real(random())
C For Absoft.
C          seed=(idyp*24+ihrp)+icomp
C          x=RAND(INT(seed))
        return
      else
        goto 20
      endif
      goto 10

C Error handling
 1000 WRITE(outs,'(A,I5,A)')' MZLIBF: error at record',IREC,'.'
      call edisp(iuout,outs)
      goto 10

      END


C ******************** DSA ********************
C DSA calls the correct routines to edit data in memory for a Diff SA.

      SUBROUTINE DSA(IZONE)
#include "building.h"
C Common blocks UA3 UA4 within uncertainty.h
#include "uncertainty.h"

      COMMON/UA5/IMET,ISIM,NRSIM
      COMMON/UATD/UACLIM(6,4),UACTL(MNACT,7,6),UAMCSA(MNACT)
      COMMON/UATD2/UAMISCTD(3)
      character message*96,phrase*12

C Reset climate change variables to zero - ONLY if first zone.
      if (IZONE.eq.1) then
        do I=1,6
          UACLIM(I,1)=0.0; UACLIM(I,2)=0.0
          UACLIM(I,3)=0.0; UACLIM(I,4)=0.0 
        enddo

C Reset control change variables to zero - ONLY if first zone.
        do loop=1,NIACT
          do loop2=1,7
            UACTL(loop,loop2,1)=0.0; UACTL(loop,loop2,2)=0.0
            UACTL(loop,loop2,3)=0.0; UACTL(loop,loop2,4)=0.0
            UACTL(loop,loop2,5)=0.0; UACTL(loop,loop2,6)=0.0
          enddo
        enddo

C Reset misc vars to zero.
        do I=1,3
          UAMISCTD(I)=0.
        enddo
      endif
      message='  '; lstm=1; lfnm=1
      phrase=' '; lphrase=1

C DIR is the direction of the alteration: 1=up or high, -1= down or low.
      DIR=0.

C Calculate which action is being carried out.
      IACT=ISIM/2
      if ((IACT*2).eq.ISIM) then

C High change
        DIR=1.
      else

C Low change
        DIR=-1.
      endif

      write (36,*) 'IDTYPU(IACTD(IACT,1))',IDTYPU(IACTD(IACT,1)),
     &  IACTD(IACT,1), IACT
      lncng=lnblnk(LCNG(IACTD(IACT,1)))
      lnloc=lnblnk(LLOC(IACTD(IACT,2)))
      if(lncng.gt.21)lncng=21
      write(36,*) LCNG(IACTD(IACT,1))(1:lncng),':',
     &            LLOC(IACTD(IACT,2))(1:lnloc)

C Synopsis of changes.
      write(phrase,'(a,i2.2,F5.2)') 'U',IACT,DIR
      lphrase=lnblnk(phrase)
      lfnm=lstm+lphrase
      if(lfnm.lt.96)then
        write(message(lstm:lfnm),'(2a)') phrase(1:lphrase),' '
        lstm=lfnm+1
      endif

C Depending on change type call editing routine.
      ichgtyp=IDTYPU(IACTD(IACT,1))
      if (ichgtyp.eq.1) then 
        call UAE01(IZONE,IACT,DIR)  ! thermophysical properties
      elseif (ichgtyp.eq.2) then 
        call UAE02(IZONE,IACT,DIR)  ! MLC layer thickness
      elseif (ichgtyp.eq.3) then 
        call UAT03(IACT,DIR)        ! weather data
      elseif (ichgtyp.eq.4) then 
        call UAE04(IZONE,IACT,DIR)  ! casual gains or scheduled air
      elseif (ichgtyp.eq.5) then 
        call UAE05(IZONE,IACT,DIR)  ! hc coefficients
      elseif (ichgtyp.eq.6) then 
        call UAE06(IZONE,IACT,DIR)  ! optical control
      elseif (ichgtyp.eq.101) then 
        call UAZ01(IZONE,IACT,DIR)  ! zone air volume or SNA
      elseif (ichgtyp.eq.1001) then 
        call UAT01(IACT,DIR)  ! control data
      elseif (ichgtyp.eq.1002) then 
         write (36,*) 'WARNING UNDEFINED UNCERTAINTY'

C Debug.
C         write(6,*) 'WARNING UNDEFINED UNCERTAINTY'

C        call UAT02(IACT,DIR)
      elseif (ichgtyp.eq.1004) then 
         write (36,*) 'WARNING UNDEFINED UNCERTAINTY'

C Debug.
C         write(6,*) 'WARNING UNDEFINED UNCERTAINTY'

C        call UAT04(IACT,DIR)
      elseif (ichgtyp.eq.1005) then 
         write (36,*) 'WARNING UNDEFINED UNCERTAINTY'

C Debug.
C         write(6,*) 'WARNING UNDEFINED UNCERTAINTY'

C        call UAT05(IACT,DIR)
      endif
      write (36,*) 'synopsis ',message(1:lnblnk(message))


      RETURN
      END

C ******************** MCSA ********************
C MCSA calls the correct routines to edit data in memory for a 
C Monte-Carlo SA.

      SUBROUTINE MCSA(IZONE)
#include "building.h"
C Common blocks UA3 UA4 within uncertainty.h
#include "uncertainty.h"
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      COMMON/UA5/IMET,ISIM,NRSIM
      COMMON/UATD/UACLIM(6,4),UACTL(MNACT,7,6),UAMCSA(MNACT)
      COMMON/UATD2/UAMISCTD(3)

      character USETNAME*96   ! the synopsis for each MC-run
      character uheader*1000  ! header including name of each uncertainty changed
      character urunvals*1000 ! attribute changed-to value of each uncertainty change for the run.
      COMMON/USETN/USETNAME(MNRS),uheader,urunvals

C Logical flag for uncertainty header has been printed.
      logical uncertheadprinted
      common/uncerthead/uncertheadprinted

      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep

      logical headerdone
      logical itemunique   ! should we include in fort.37
      logical iszonebased  ! at least one of uncertainties is zone based
      logical ismzcoebased ! at least one requires calling mzcoe2
      integer itemsinheader,nbpastitems,nbdatahits
      character pastphrase*42
      common/uhead/headerdone,itemunique(400),pastphrase(400),
     &  itemsinheader,nbpastitems,nbdatahits
      
      common/reclen/nzrl,nprl,nerl
      character RSNAME*40,outs*124,message*96,phrase*12,uhphrase*42
      integer irec,iunit,ichgtyp
      character urunvalsd*1000  ! to comma separate output
      character ubasecase*1000,ubasecased*1000,baseval*4

C Reset climate and control change variables to zero - ONLY if first zone.
      if (IZONE.eq.1) then
        do I=1,6
          UACLIM(I,1)=0.0; UACLIM(I,2)=0.0; UACLIM(I,3)=0.0
          UACLIM(I,4)=0.0
        enddo

C Reset control change variables to zero - ONLY if first zone.
C UACTL (iact,topic,1)=change type (%,+/-); (?,2)=magnitude; (?,3)=associated action;
C UACTL (?,?,4)=time status (0 not active, 1 active)
C UACTL (?,?,5)=counter for the uncertainty action being dealt with
C UACTL (?,?,6)=value of DIR
        do loop=1,NIACT
          do loop2=1,7
            UACTL(loop,loop2,1)=0.0; UACTL(loop,loop2,2)=0.0
            UACTL(loop,loop2,3)=0.0; UACTL(loop,loop2,4)=0.0
            UACTL(loop,loop2,5)=0.0; UACTL(loop,loop2,6)=0.0
          enddo
        enddo

C Reset misc vars to zero.
        do 25 I=1,3
          UAMISCTD(I)=0.
 25     continue

C Set random deviations for all uncertainties.
C Can select from either a normal distribution (DISTTYPE=1) or 
C uniform distribution (DISTTYPE=2).
C RNOR returns a value at random from a normal distribution with
C std of 1.0 and mean of 0.0 (approx. -3 \leq x \leq 3).
C A scale-location transformation of this to a distribution with a mean 
C of 0 and a variance of 0.1 will give numbers (-1 \leq x 1).
C Then s = sqrt(0.1)*rnor() + 0 is what we desire.

C RANDOM_NUMBER is an intrinsic function that returns a single 
C pseudorandom number from the uniform distribution over the 
C range 0 \leq x < 1. This is then mapped to -1 \leq x < 1 to act as
C delta from nominal value from database
        do 20 I=1,NIACT
          ! Hardcode to uniform until assigned via prj
          DISTTYPE(I)=2
          if (DISTTYPE(I).eq.1) then
            UAMCSA(I)=sqrt(0.1)*rnor() + 0
          elseif (DISTTYPE(I).eq.2) then
            CALL RANDOM_NUMBER(UAMCSA(I))
            UAMCSA(I) = -1 + 2*UAMCSA(I)
          endif
 20     continue
      endif

C Change parameters by a random amount (defined above). Clear
C strings for a synopsis of this run (up to a ~dozen actions).
      message='  '; lstm=1; lfnm=1
      phrase=' '; lphrase=1
      uhphrase=' '; luhphrase=1
      if(IZONE.eq.1)then
        urunvals='  '  ! clear string buffer for attribute changes if first zone
        nbdatahits=0
        write (36,*) 'zone zero clear of urunvals'
      endif
      ismzcoebased=.false.
      iszonebased=.false.
      do 10 IACT=1,NIACT

C DIR is the direction of the alteration: +ive =up or high, -ive = down or low.
        DIR=UAMCSA(IACT)

C Remember which uncertainty is being acted upon. This is the
C combined name of the uncertainty:location. Append to the header buffer.
        if(IACTD(IACT,1).eq.0)then
          write (36,*) 'a zero IACTD(IACT,1)',
     &    ' CNG',IACTD(IACT,1),IACT,' Z',IZONE,' LOC',IACTD(IACT,2)
          goto 10
        endif
        write (36,*) 'IDTYPU(IACTD(IACT,1))',IDTYPU(IACTD(IACT,1)),
     &    ' CNG',IACTD(IACT,1),IACT,' Z',IZONE,' LOC',IACTD(IACT,2)
        write (36,*) 'Uncertainty definition ',IACT,' altered by ',DIR
        lncng=lnblnk(LCNG(IACTD(IACT,1)))
        lnloc=lnblnk(LLOC(IACTD(IACT,2)))
        if(lncng.gt.21)lncng=21
        uhphrase=' '
        write(uhphrase,'(4a)')LCNG(IACTD(IACT,1))(1:lncng),':',
     &    LLOC(IACTD(IACT,2))(1:lnloc),','
        luhphrase=lnblnk(uhphrase)
C        write(36,'(a)') uhphrase(1:luhphrase)

C Synopsis of specific changes to identify the run.
        write(phrase,'(a,i2.2,F5.2)') 'U',IACT,DIR
        lphrase=lnblnk(phrase)
        lfnm=lstm+lphrase
        if(lfnm.lt.96)then
          write(message(lstm:lfnm),'(2a)') phrase(1:lphrase),' '
          lstm=lfnm+1
        endif

C Depending on change type call editing routine.
        ichgtyp=IDTYPU(IACTD(IACT,1))
        if (ichgtyp.eq.1) then

C Alter the required thermophysical property by DIR. This change
C is made to the common blocks of thermophyical properties and
C MZPREC will later call mzcoe1 and mzcoe2 to update the matrix 
C to reflect these changes.
          call UAE01(IZONE,IACT,DIR)
          iszonebased=.true.
          ismzcoebased=.true.
        elseif (ichgtyp.eq.2) then

C Alter a layer thickness within a MLC. MZPREC will later call
C mzcoe1 and mzcoe2 to update the matrix to reflect these changes. 
          call UAE02(IZONE,IACT,DIR)  ! layer thickness
          iszonebased=.true.
          ismzcoebased=.true.
        elseif (ichgtyp.eq.3) then 

C Weather applies to the whole model so only call this for the
C first zone ????
          call UAT03(IACT,DIR)        ! weather data ???
        elseif (ichgtyp.eq.4) then 
          call UAE04(IZONE,IACT,DIR)  ! casual gains or scheduled air
          iszonebased=.true.
        elseif (ichgtyp.eq.5) then 
          call UAE05(IZONE,IACT,DIR)  ! hc coefficients
          iszonebased=.true.
        elseif (ichgtyp.eq.6) then 
          call UAE06(IZONE,IACT,DIR)  ! optical control
          iszonebased=.true.
        elseif (ichgtyp.eq.101) then 
          call UAZ01(IZONE,IACT,DIR)  ! zone air volume or SNA
          iszonebased=.true.
        elseif (ichgtyp.eq.1001) then

C Control applies to the whole model so only call this for the
C first zone. 
          if (IZONE.eq.1) call UAT01(IACT,DIR)  ! control data
          iszonebased=.false.
        elseif (ichgtyp.eq.1002) then 
         write (36,*) 'WARNING UNDEFINED UNCERTAINTY'

C Debug.
c          write(6,*) 'WARNING UNDEFINED UNCERTAINTY'
C          call UAT02(IACT,DIR)
        elseif (ichgtyp.eq.1004) then 
         write (36,*) 'WARNING UNDEFINED UNCERTAINTY'

C Debug.
C          write(6,*) 'WARNING UNDEFINED UNCERTAINTY'
C          call UAT04(IACT,DIR)
        elseif (ichgtyp.eq.1005) then 
         write (36,*) 'WARNING UNDEFINED UNCERTAINTY'

C Debug.
C          write(6,*) 'WARNING UNDEFINED UNCERTAINTY'
C          call UAT05(IACT,DIR)
        endif
 10   continue  ! of IACT
      write(36,*) 'synopsis ',message(1:lnblnk(message))

C If we need to rebuild the coeficients do this now.
C      if(ismzcoebased)then
C        call mzcoe1(IZONE)   ! Re-establish building-side matrix equation.
C        IF(NTSTEP.GT.1)THEN  ! Modify computational time-step is < one hour.
C          TSTEP=1./FLOAT(NTSTEP)
C          CALL MZCOE2(TSTEP)
C        ENDIF
C      endif

C After the NIACT loop the header should be filled so if
C zone based write it out if we are at the last zone in the
C model and if control based write if iact=NIACT.
C Both uheader and urnubals will be blank for the case of weather
C data as information is written each day rather than once at the
C start of the run. For control uncertainty the writing to
C fort.37 will be happening in UAT01a.

      if(.NOT.uncertheadprinted)then

C Loop through the list of uncertainties and if zone based and
C also the last zone then write header.
        if(iszonebased)then

C << when weather done add it to if logic >>.
          write(36,*) 'state of uncertheadprinted ',uncertheadprinted

C We have a zone-dependent uncertainty so deal with the
C base values here.
          if(IZONE.eq.NCOMP)then

C Remove the trailing comma from the header line and write the
C header to the fort.37 file. We then need to write a line of
C zeros to represent the base case.
            lnhead=lnblnk(uheader)
            write(36,'(2a)') 'writing: ',uheader(1:lnhead-1)
            write(37,'(a)') uheader(1:lnhead-1)
            uncertheadprinted=.true.

C Write out one line of zeros to represent the base case.
            ubasecase='  '; ubasecased='  '; baseval='0.00'
            lbase=4; lstbase=1; lfnbase=1

C Debug.
            write(6,*) 'itemunique ',itemunique
            do loop=1,itemsinheader
              lfnbase=lstbase+lbase
              if(lfnbase.lt.1000)then
                write(ubasecase(lstbase:lfnbase),'(a)')baseval(1:lbase)
                lstbase=lfnbase+2
              endif
            enddo
            call SDELIM(ubasecase,ubasecased,'C',IW)
            lnbase=lnblnk(ubasecased)
            write(37,'(a)') ubasecased(1:lnbase-1)
          endif
        endif
      endif
      
C If zone dependent are we ready to write urunvalsd?
      if(iszonebased)then
        if(IZONE.eq.NCOMP)then
          urunvalsd='  '
          call SDELIM(urunvals,urunvalsd,'C',IW)
          write(36,'(2a)') 'writing zone',urunvalsd(1:lnblnk(urunvalsd))
          write(37,'(a)') urunvalsd(1:lnblnk(urunvalsd))
          write(36,'(a)') 'clearing zone urunvals'
          urunvals='  '  ! clear string buffer prior to next run
          urunvalsd='  '
        endif
      else

C If control based write urunvals.
C Add comma separations to the data.
        if(IZONE.eq.NCOMP)then
          call SDELIM(urunvals,urunvalsd,'C',IW)
          write(36,'(2a)') 'writing ctl',urunvalsd(1:lnblnk(urunvalsd))
          write(37,'(a)') urunvalsd(1:lnblnk(urunvalsd))
          write(36,'(a)') 'clearing ctl urunvals'
          urunvals='  '  ! clear string buffer prior to next run
          urunvalsd='  '
        endif
      endif

C See what the current position of IREC is and update RSNAME to the
C synopsis of changes recorded above.
      IUNIT=IFIL+2
      IREC=1
      read (IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM,nzrl,izver,IMET
C      WRITE(6,*) 'checking ',NSIM,nzrl,izver,IMET

C Remember the synopsis for this run. The first RSNAME will be 
C the base case so the 2nd simulation set should take the message
C string related to the 1st uncertainty
      if(ISIM.eq.1)then  ! leave base case RSNAME as is.
        write(USETNAME(1),'(a)') 'Base case'
      elseif(ISIM.gt.1)then  ! update RSNAME.
        write(USETNAME(ISIM),'(a)') message(1:lnblnk(message))
        IREC=NSIM+1  ! slightly different from MZLS2
        read(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NST,IRECPL,RSNAME
        WRITE(6,*) 'previous ',NST,IRECPL,' ',RSNAME(1:lnblnk(RSNAME))
        write(RSNAME,'(a)') message(1:40)
        write(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NST,IRECPL,RSNAME
        WRITE(6,*) 'updated ',NST,IRECPL,' ',RSNAME(1:lnblnk(RSNAME))
      endif

      RETURN

 1000 write(outs,'(A,I5,A)')' MCSA: error at record',IREC,'.'
      call edisp(iuout,outs)
      return
      END

C ******************** FCTA ********************
C FCTA calls the correct routines to edit data in memory for a factorial analysis

      SUBROUTINE FCTA(IZONE)
#include "building.h"
C Common blocks UA3 UA4 within uncertainty.h
#include "uncertainty.h"

      COMMON/UA5/IMET,ISIM,NRSIM
      COMMON/UATD/UACLIM(6,4),UACTL(MNACT,7,6),UAMCSA(MNACT)
      COMMON/UAFCT/IADIR(MNFA),NFA
      character message*96,phrase*12

C Reset climate change variables to zero - ONLY if first zone.
      if (IZONE.eq.1) then
        do I=1,6
          UACLIM(I,1)=0.0; UACLIM(I,2)=0.0; UACLIM(I,3)=0.0
          UACLIM(I,4)=0.0
        enddo

C Reset control change variables to zero.
        do loop=1,NIACT
          do loop2=1,7
            UACTL(loop,loop2,1)=0.0; UACTL(loop,loop2,2)=0.0
            UACTL(loop,loop2,3)=0.0; UACTL(loop,loop2,4)=0.0
            UACTL(loop,loop2,5)=0.0; UACTL(loop,loop2,6)=0.0
          enddo
        enddo
      endif

C Set up array of uncertainty states.
      IADIR(1)=IADIR(1)+2
      do 20 I=1,NFA-1
        if (IADIR(I).gt.2) then
          IADIR(I)=-1
          IADIR(I+1)=IADIR(I+1)+2
        endif
 20   continue
        
C Loop through changes.
      write (36,*) 'State: ',(IADIR(I),I=1,NFA)
      message='  '; lstm=1; lfnm=1
      phrase=' '; lphrase=1
      do 30 I=1,NFA
        IACT=I
        DIR=float(IADIR(I))
        write (36,*) 'IDTYPU(IACTD(IACT,1))',IDTYPU(IACTD(IACT,1)),
     &    IACTD(IACT,1), IACT

C Debug...
        write (36,*) 'Uncertainty definition ',IACT,' altered by ',DIR
        lncng=lnblnk(LCNG(IACTD(IACT,1)))
        lnloc=lnblnk(LLOC(IACTD(IACT,2)))
        if(lncng.gt.21)lncng=21
        write(36,*) LCNG(IACTD(IACT,1))(1:lncng),':',
     &              LLOC(IACTD(IACT,2))(1:lnloc)

C Synopsis of changes.
        write(phrase,'(a,i2.2,F5.2)') 'U',IACT,DIR
        lphrase=lnblnk(phrase)
        lfnm=lstm+lphrase
        if(lfnm.lt.96)then
          write(message(lstm:lfnm),'(2a)') phrase(1:lphrase),' '
          lstm=lfnm+1
        endif

C Depending on change type call editing routine.
        ichgtyp=IDTYPU(IACTD(IACT,1))
        if (ichgtyp.eq.1) then 
          call UAE01(IZONE,IACT,DIR)  ! thermophysical properties
        elseif (ichgtyp.eq.2) then 
          call UAE02(IZONE,IACT,DIR)  ! layer thickness
        elseif (ichgtyp.eq.3) then 
          call UAT03(IACT,DIR)        ! weather data
        elseif (ichgtyp.eq.4) then 
          call UAE04(IZONE,IACT,DIR)  ! casual gains or scheduled air
        elseif (ichgtyp.eq.5) then 
          call UAE05(IZONE,IACT,DIR)  ! hc coefficients
        elseif (ichgtyp.eq.6) then 
          call UAE06(IZONE,IACT,DIR)  ! optical control
        elseif (ichgtyp.eq.101) then 
          call UAZ01(IZONE,IACT,DIR)  ! zone air volume or SNA
        elseif (ichgtyp.eq.1001) then 
          call UAT01(IACT,DIR)  ! control data
        endif
 30   continue
      write (36,*) 'synopsis ',message(1:lnblnk(message))

      RETURN
      END


C ******************** UAE01 ********************
C UAE01 edits conductivity, density or specific heat values in memory.
C The values are edited in 'THRMLI' first and then copied into /T1/ and /T2/ 
C to enable pre-simulation calculations. 
C Naming convention:
C IZONE, ISURF: focus zone and surface (the data files for this zone 
C               have just been rescaned). 
C IZ, IS: current zone and surface being checked (these are listed in 
C               the common/ua2/ arrays).
      SUBROUTINE UAE01(IZONE,IACT,DIR)
#include "building.h"
#include "geometry.h"
C Common blocks UA2 UA3 UA4 within uncertainty.h
#include "uncertainty.h"
#include "esprdbfile.h"

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
C      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)
      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)

C Version of construction file. If not specified set at 21 (2.1)
C      integer izconstv,iztmcv
C      common/znconstrv/izconstv(MCOM),iztmcv(MCOM)

      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)

C      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),AE(MCOM,MS)

      character USETNAME*96   ! the synopsis for each MC-run
      character uheader*1000  ! header including name of each uncertainty changed
      character urunvals*1000 ! attribute changed-to value of each uncertainty change for the run.
      COMMON/USETN/USETNAME(MNRS),uheader,urunvals

      logical headerdone
      logical itemunique  ! should we include in fort.37
      logical isunique    ! for test against past
      integer itemsinheader,nbpastitems,nbdatahits
      character pastphrase*42
      common/uhead/headerdone,itemunique(400),pastphrase(400),
     &  itemsinheader,nbpastitems,nbdatahits

      integer lstur, lfnur
      character strval*12,uhphrase*42,uhroot*42
      CHARACTER CSTR*25

      DIMENSION IEL(ME)

C Data stored in UA01 has the following meaning in this routine:
C IDATR(?,1): Materials array index.
C IDATR(?,2): Thermophysical property (con; den; sp ht).
C IDATR(?,3): Change type (%; +/-; explicit).
C Define variables for current location and change.
      if(urunvals(1:2).eq.'  ')then
        lstur=1; lfnur=1  ! reset counters for urunvals buffer
      else
        lstur=lnblnk(urunvals)+2; lfnur=lnblnk(urunvals)+2  ! to append to urunvals buffer
      endif
      ICL=IACTD(IACT,2)
      ICC=IACTD(IACT,1)

C If initial append to uheader.
      if(uheader(1:2).eq.'  ')then
        lstuh=1; lfnuh=1  ! reset counters for uheader buffer
      else
        lstuh=lnblnk(uheader)+2; lfnuh=lnblnk(uheader)+2  ! to append to uheader buffer
      endif

      write (36,*) '   '
      write (36,*) 'Entering subroutine UAE01'
      write (36,*) 'Focus zone: ',IZONE

C Loop through all zones listed in NZNOGU for the defined location ICL.
      do 10 II=1,NZGU(ICL)
       IZ=NZNOGU(ICL,II)
       if (IZ.eq.IZONE) then

C Change data in the required surfaces.
         do 20 JJ=1,NSGU(ICL,II)
           IS=NSNOG(ICL,II,JJ)

C Check to see if material is used in current construction.
C CHKPRIM returns the layers which are made of the required material (if any).
           call CHKPRIM(IZ,IS,IDATR(ICC,1),NEL,IEL)
           if (NEL.gt.0) then
             do 30 IEindex=1,NEL
               IE=IEL(IEindex)

C Trace.
               write(36,*)'Editing element:'
               write(36,*)'Zone: ',IZ,' Surface: ',IS,' Element',IE
               write(36,*)
     &               '   con     den      sht    emis(I,E) abs(I,E)'
               write(36,'(3f9.3,4f5.2)')CON(IS,IE),DEN(IS,IE),
     &           SHT(IS,IE),EMISI(IS), EMISE(IS),ABSI(IS),ABSE(IS)

C Make specific header string for each change (to match urunvals).
               lncng=lnblnk(LCNG(ICC))
               lnloc=lnblnk(LLOC(ICL))
               if(lncng.gt.18)lncng=18
               uhphrase=' '
               write(uhphrase,'(4a,i2.2,a,i3.3,a)')LCNG(ICC)(1:lncng),
     &           ':',LLOC(ICL)(1:lnloc),':',IZ,':',IS,','
               luhphrase=lnblnk(uhphrase)
               write(36,'(a)') uhphrase(1:luhphrase)

C Remember the root name of the phrase so that subsequent duplicates can
C be ignored in the fort.37 file. If the initial item the increment counters
C otherwise search pastphrase(s) for match.
               uhroot=' '
               write(uhroot,'(4a)')LCNG(ICC)(1:lncng),
     &           ':',LLOC(ICL)(1:lnloc),','
               luhroot=lnblnk(uhroot)
C               write(36,'(a)') uhroot(1:luhroot)
               if(itemsinheader.eq.0)then
                 nbpastitems=nbpastitems+1
                 write(pastphrase(nbpastitems),'(a)') uhroot(1:luhroot)
                 itemunique(1)=.TRUE.
                 write(36,*) 'unique item',nbpastitems,uhroot(1:luhroot)
               elseif(itemsinheader.eq.1)then
                 lnpast=lnblnk(pastphrase(1))
                 if(uhroot(1:luhroot).eq.pastphrase(1)(1:lnpast))then
                   itemunique(itemsinheader+1)=.FALSE.
                 else
                   itemunique(itemsinheader+1)=.TRUE.
                   nbpastitems=nbpastitems+1
                  write(pastphrase(nbpastitems),'(a)') uhroot(1:luhroot)
                  write(36,*)'unique item',nbpastitems,uhroot(1:luhroot)
                 endif
               else

C If we ever get a match unset isunique.
                 isunique=.true.
                 do loop=1,nbpastitems
                   lnpast=lnblnk(pastphrase(loop))
                   if(uhroot(1:luhroot).eq.
     &                pastphrase(loop)(1:lnpast))then
                     isunique=.false.
                   endif
                 enddo
                 if(isunique)then
                   itemunique(itemsinheader+1)=.TRUE.
                   nbpastitems=nbpastitems+1
                   write(pastphrase(nbpastitems),'(a)')
     &               uhroot(1:luhroot)
                   write(36,*)'unique item',nbpastitems,
     &               uhroot(1:luhroot)
                 endif
               endif

C If we have not finished the header and the item is unique
C then include it in the header and remember so that we
C can also include a data column.
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
      
               strval=' '        ! clear string buffer for the value changed

C Edit data.
               if (IDATR(ICC,2).eq.1) then
                 call UAEDIT(CON(IS,IE),IDATR(ICC,3),DATU(ICC,1),
     &                       DATU(ICC,2),DIR,-1,0.01,0.0)
               elseif (IDATR(ICC,2).eq.2) then
                 call UAEDIT(DEN(IS,IE),IDATR(ICC,3),DATU(ICC,1),
     &                       DATU(ICC,2),DIR,0,0.01,9000.0)
               elseif (IDATR(ICC,2).eq.3) then
                 call UAEDIT(SHT(IS,IE),IDATR(ICC,3),DATU(ICC,1),
     &                       DATU(ICC,2),DIR,0,0.01,3000.0)
                 call relstr(SHT(IS,IE),strval,lna,ier)
               elseif (IDATR(ICC,2).eq.4) then
                 call UAEDIT(EMISE(IS),IDATR(ICC,3),DATU(ICC,1),
     &                       DATU(ICC,2),DIR,0,0.001,0.999)
                 call UAEDIT(EMISI(IS),IDATR(ICC,3),DATU(ICC,1),
     &                       DATU(ICC,2),DIR,0,0.001,0.999)
               elseif (IDATR(ICC,2).eq.5) then
                 call UAEDIT(ABSE(IS),IDATR(ICC,3),DATU(ICC,1),
     &                       DATU(ICC,2),DIR,0,0.001,0.999)
                 call UAEDIT(ABSE(IS),IDATR(ICC,3),DATU(ICC,1),
     &                       DATU(ICC,2),DIR,0,0.001,0.999)
               endif

               write(36,*) 'to:'
               write(36,'(3f9.3,4f5.2)')CON(IS,IE),DEN(IS,IE),
     &               SHT(IS,IE),EMISI(IS), EMISE(IS),ABSI(IS),ABSE(IS)

C Increment counter of data so as to check against itemunique array.
               nbdatahits=nbdatahits+1
               write(36,*) 'UAE01 setting nbdatahits',nbdatahits

C Add to urunvals if previously marked as unique.
               if(itemunique(nbdatahits))then

C Write DIR to fort.37.
                 call relstr(DIR,strval,lna,ier)
                 lstrval=lnblnk(strval)
                 lfnur=lstur+lstrval
                 if(lfnur.lt.1000)then
                   write(urunvals(lstur:lfnur),'(a)') strval(1:lstrval)
                   lstur=lfnur+2
                   write(36,*) urunvals(1:lnblnk(urunvals))
                   write(6,*) urunvals(1:lnblnk(urunvals))
                 endif
               endif

C Now copy the altered data into the zonal commons.
               if (IDATR(ICC,2).eq.1) then
                 THRMLI(IZ,IS,IE,1)=CON(IS,IE)
               elseif (IDATR(ICC,2).eq.2) then
                 THRMLI(IZ,IS,IE,2)=DEN(IS,IE)
               elseif (IDATR(ICC,2).eq.3) then
                 THRMLI(IZ,IS,IE,3)=SHT(IS,IE)
               endif
 30          continue
           endif
 20      continue

C Call routine to copy surface data into common PREC12.
         call MZMISC(IZONE)
       else

C Check for adjacent surfaces. 
         do 40 ISURF=1,NSUR
           CALL SURADJ(IZONE,ISURF,IEX,T,IZC,ISC,ICN,CSTR)
           if (IZC.eq.IZ) then 

C Check if surface ISC has associated uncertainties, if yes then edit.
             do 60 JJ=1,NSGU(ICL,II)
               if (NSNOG(ICL,II,JJ).eq.ISC) then

C Check to see if material is used in current construction.
C CHKPRIM returns the layers which are made of the required material (if any).
                 call CHKPRIM(IZONE,ISURF,IDATR(ICC,1),NEL,IEL)
                 if (NEL.gt.0) then
                   do 50 IEindex=1,NEL
                     IE=IEL(IEindex)
                     IS=ISURF
                     IZ=IZONE
C Trace.
               write(36,*)'Editing adjacent element:'
               write(36,*)'Zone: ',IZ,' Surface: ',IS,' Element',IE
               write(36,*)
     &               '   con     den      sht    emis(I,E) abs(I,E)'
               write(36,'(3f9.3,4f5.2)')CON(IS,IE),DEN(IS,IE),
     &               SHT(IS,IE),EMISI(IS), EMISE(IS),ABSI(IS),ABSE(IS)

C Edit data.
               if (IDATR(ICC,2).eq.1) then
                 call UAEDIT(CON(IS,IE),IDATR(ICC,3),DATU(ICC,1),
     &             DATU(ICC,2),DIR,-1,0.01,0.0)
               elseif (IDATR(ICC,2).eq.2) then
                 call UAEDIT(DEN(IS,IE),IDATR(ICC,3),DATU(ICC,1),
     &             DATU(ICC,2),DIR,0,0.01,9000.0)
               elseif (IDATR(ICC,2).eq.3) then
                 call UAEDIT(SHT(IS,IE),IDATR(ICC,3),DATU(ICC,1),
     &             DATU(ICC,2),DIR,0,0.01,3000.0)
               elseif (IDATR(ICC,2).eq.4) then
                 call UAEDIT(EMISE(IS),IDATR(ICC,3),DATU(ICC,1),
     &             DATU(ICC,2),DIR,0,0.001,0.999)
                 call UAEDIT(EMISI(IS),IDATR(ICC,3),DATU(ICC,1),
     &             DATU(ICC,2),DIR,0,0.001,0.999)
               elseif (IDATR(ICC,2).eq.5) then
                 call UAEDIT(ABSE(IS),IDATR(ICC,3),DATU(ICC,1),
     &             DATU(ICC,2),DIR,0,0.001,0.999)
                 call UAEDIT(ABSE(IS),IDATR(ICC,3),DATU(ICC,1),
     &             DATU(ICC,2),DIR,0,0.001,0.999)
               endif

               write(36,*) 'to:'
               write(36,'(3f9.3,4f5.2)')CON(IS,IE),DEN(IS,IE),
     &               SHT(IS,IE),EMISI(IS), EMISE(IS),ABSI(IS),ABSE(IS)

C Now copy the altered data into the zonal commons.
               if (IDATR(ICC,2).eq.1) then
                 THRMLI(IZ,IS,IE,1)=CON(IS,IE)
               elseif (IDATR(ICC,2).eq.2) then
                 THRMLI(IZ,IS,IE,2)=DEN(IS,IE)
               elseif (IDATR(ICC,2).eq.3) then
                 THRMLI(IZ,IS,IE,3)=SHT(IS,IE)
               endif
 50                continue
                 endif
               endif
 60          continue
           endif
 40      continue

C Call routine to copy surface data into common PREC12.
         call MZMISC(IZONE)
       endif
 10   continue

C If this was the last uncertainty i.e. if IACT=NIACT
C and we have done all the zones then the header is done.
      if(IACT.eq.NIACT)then
        if(IZONE.eq.NCOMP)then
          write(36,*) 'set header buffer done in UAE01 ',itemsinheader
          headerdone=.true.
        endif
      endif
      RETURN
      END

C ******************** UAE02 ********************
C UAE02 edits layer thicknesses in memory. The editing of whole MLC's 
C will also be added.
C Naming convention:
C IZONE, focus zone (the data files for this zone have just been rescaned). 
C IZ, IS: current zone and surface being checked (these are listed in 
C         the common/ua2/ arrays).

      SUBROUTINE UAE02(IZONE,IACT,DIR)
#include "building.h"
#include "geometry.h"
C Common blocks UA2 UA3 UA4 within uncertainty.h
#include "uncertainty.h"
#include "esprdbfile.h"
#include "material.h"

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)

C Version of construction file. If not specified set at 21 (2.1)
C      integer izconstv,iztmcv
C      common/znconstrv/izconstv(MCOM),iztmcv(MCOM)

      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)

      COMMON/UA31/MATNAM(MNCNG,2)

      character USETNAME*96   ! the synopsis for each MC-run
      character uheader*1000  ! header including name of each uncertainty changed
      character urunvals*1000 ! attribute changed-to value of each uncertainty change for the run.
      COMMON/USETN/USETNAME(MNRS),uheader,urunvals

      logical headerdone
      logical itemunique  ! should we include in fort.37
      logical isunique    ! for test against past
      integer itemsinheader,nbpastitems,nbdatahits
      character pastphrase*42
      common/uhead/headerdone,itemunique(400),pastphrase(400),
     &  itemsinheader,nbpastitems,nbdatahits

C      CHARACTER TITL*72,NAM*72,CSTR*25
      CHARACTER MATNAM*32,CSTR*25

      integer lstur, lfnur
      character strval*12,uhphrase*42,uhroot*42

C Data stored in UA02 has the following meaning in this routine:
C IDATR(?,1): The material in the chosen MLC, if this is 0 then 
C              a new MLC reference will be in MATNAM(?,2).
C IDATR(?,2): Unused.
C IDATR(?,3): Change type (%; +/-; explicit).
C Define variables for current location and change.
      if(urunvals(1:2).eq.'  ')then
        lstur=1; lfnur=1  ! reset counters for urunvals buffer
      else
        lstur=lnblnk(urunvals)+2; lfnur=lnblnk(urunvals)+2  ! to append to urunvals buffer
      endif
      ICL=IACTD(IACT,2)
      ICC=IACTD(IACT,1)

C If initial append to uheader.
      if(uheader(1:2).eq.'  ')then
        lstuh=1; lfnuh=1  ! reset counters for uheader buffer
      else
        lstuh=lnblnk(uheader)+2; lfnuh=lnblnk(uheader)+2  ! to append to uheader buffer
      endif

      write (36,'(a)') '   '
      write (36,'(a)') 'Entering subroutine UAE02'
      write (36,'(a,i3,2a)') 'Focus zone: ',IZONE,' ',zname(IZONE)

C Loop through all zones listed in NZNOGU for the defined location ICL.
      do 10 II=1,NZGU(ICL)
       IZ=NZNOGU(ICL,II)
       if (IZ.eq.IZONE) then

C The current zone (IZ) is equal to the target zone (IZONE). 
C Loop through all the surfaces in the current location ICL for the 
C target zone - search for MLC's with the same name as MATNAM(ICC).
        do 20 JJ=1,NSGU(ICL,II)
         IS=NSNOG(ICL,II,JJ)
         if (SMLCN(IZ,IS)(1:12).eq.MATNAM(ICC,1)(1:12)) then

C This construction has uncertainties defined, but which MLC database 
C entry is it? C Find matching MLC.
          call matchmlcdesc(SMLCN(IZ,is),imlc)
          do 30 IE=1,NE(IS)
C           write (36,*) 'iprmat(imlc,ie): ',IPRMAT(IMLC,IE),imlc,ie
C           write (36,*) 'IDATR(ICC,1): ',IDATR(ICC,1),icc
           if (IPRMAT(IMLC,IE).eq.IDATR(ICC,1)) then

C Make specific header string for each change (to match urunvals)
            lncng=lnblnk(LCNG(ICC))
            lnloc=lnblnk(LLOC(ICL))
            if(lncng.gt.18)lncng=18
            uhphrase=' '
            write(uhphrase,'(4a,i2.2,a,i3.3,a)')LCNG(ICC)(1:lncng),
     &        ':',LLOC(ICL)(1:lnloc),':',IZ,':',IS,','
            luhphrase=lnblnk(uhphrase)
            write(36,'(a)') uhphrase(1:luhphrase)

C Remember the root name of the phrase so that subsequent duplicates can
C be ignored in the fort.37 file. If the initial item the increment counters
C otherwise search pastphrase(s) for match.
            uhroot=' '
            write(uhroot,'(4a)')LCNG(ICC)(1:lncng),
     &        ':',LLOC(ICL)(1:lnloc),','
            luhroot=lnblnk(uhroot)
            write(36,'(a)') uhroot(1:luhroot)
            if(itemsinheader.eq.0)then
              nbpastitems=nbpastitems+1
              write(pastphrase(nbpastitems),'(a)') uhroot(1:luhroot)
              itemunique(1)=.TRUE.
              write(36,*) 'unique item',nbpastitems,uhroot(1:luhroot)
            elseif(itemsinheader.eq.1)then
              lnpast=lnblnk(pastphrase(1))
              if(uhroot(1:luhroot).eq.pastphrase(1)(1:lnpast))then
                itemunique(itemsinheader+1)=.FALSE.
              else
                itemunique(itemsinheader+1)=.TRUE.
                nbpastitems=nbpastitems+1
                write(pastphrase(nbpastitems),'(a)') uhroot(1:luhroot)
                write(36,*) 'unique item',nbpastitems,uhroot(1:luhroot)
              endif
            else

C If we ever get a match unset isunique.
              isunique=.true.
              do loop=1,nbpastitems
                lnpast=lnblnk(pastphrase(loop))
                if(uhroot(1:luhroot).eq.pastphrase(loop)(1:lnpast))then
                  isunique=.false.
                endif
              enddo
              if(isunique)then
                itemunique(itemsinheader+1)=.TRUE.
                nbpastitems=nbpastitems+1
                write(pastphrase(nbpastitems),'(a)') uhroot(1:luhroot)
                write(36,*)'unique item',nbpastitems,uhroot(1:luhroot)
              endif
            endif

C If unique then include it in the header otherwise at least keep track of itemsinheader
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

            strval=' '        ! clear string buffer for the value changed

C Either edit layer thickness or whole construction.
            if (IE.eq.0) then
              write (36,'(a)') 'MLC editing not enabled.'
            else
              write (36,'(a)') 'Editing layer thickness..'
              write (36,'(a,i3,a,i3,a,i2)') 'Zone: ',IZ,
     &         ' Surface: ',IS,' layer',IE
              write (36,'(a,f9.4)') 'THRMLI: ',THRMLI(IZ,IS,IE,4)
             if (IDATR(ICC,3).eq.1) then

C Percentage change. Ensure it does not go below 1mm. 
              write (36,'(a,3f9.4)') 'DATU DIR % factor: ',DATU(ICC,1),
     &          DIR,(((DATU(ICC,1)/100.)*DIR)+1.0)
               THRMLI(IZ,IS,IE,4)=THRMLI(IZ,IS,IE,4)*
     &           (((DATU(ICC,1)/100.)*DIR)+1.0)
               if(THRMLI(IZ,IS,IE,4).lt.0.001)THRMLI(IZ,IS,IE,4)=0.001
             elseif (IDATR(ICC,3).eq.2) then

C Absolute change. Ensure it does not go below 1mm. 
              write (36,'(a,3f9.4)') 'DATU DIR abs factor: ',
     &          DATU(ICC,1),DIR,(DATU(ICC,1)*DIR)
               THRMLI(IZ,IS,IE,4)=THRMLI(IZ,IS,IE,4)+
     &                            (DATU(ICC,1)*DIR)
               if(THRMLI(IZ,IS,IE,4).lt.0.001)THRMLI(IZ,IS,IE,4)=0.001
             elseif (IDATR(ICC,3).eq.3) then

C Explicit change. Ensure it does not go below 1mm.
              write (36,'(a,3f9.4)') 'DATU DIR expl factor: ',
     &          DATU(ICC,1),DIR,(DATU(ICC,1)-THRMLI(IZ,IS,IE,4))*DIR
              if (DIR.gt.0.) then
                THRMLI(IZ,IS,IE,4)=THRMLI(IZ,IS,IE,4)+
     &            (DATU(ICC,1)-THRMLI(IZ,IS,IE,4))*DIR
              else
                THRMLI(IZ,IS,IE,4)=THRMLI(IZ,IS,IE,4)-
     &            (DATU(ICC,2)-THRMLI(IZ,IS,IE,4))*DIR
                if(THRMLI(IZ,IS,IE,4).lt.0.001)THRMLI(IZ,IS,IE,4)=0.001
              endif
             endif
             write (36,'(a)') 'to:'
             write (36,'(a,f9.4)') 'THRMLI: ',THRMLI(IZ,IS,IE,4)

C Increment counter of data so as to check against itemunique array.
             nbdatahits=nbdatahits+1
C             write(36,*) 'UAE02 setting nbdatahits',nbdatahits

C Add to urunvals if previously marked as unique.
             if(itemunique(nbdatahits))then

C Write DIR to fort.37.
               call relstr(DIR,strval,lna,ier)
               lstrval=lnblnk(strval)
               lfnur=lstur+lstrval
               if(lfnur.lt.1000)then
                 write(urunvals(lstur:lfnur),'(a)') strval(1:lstrval)
                 lstur=lfnur+2
                 write(36,*) urunvals(1:lnblnk(urunvals))
                 write(6,*) urunvals(1:lnblnk(urunvals))
               endif
             endif

C Now copy the altered data into the zonal commons.
             THK(IS,IE)=THRMLI(IZ,IS,IE,4)
            endif
           endif
 30       continue
         endif
 20     continue
       else

C Check for adjacent surfaces. 
        do 40 ISURF=1,NSUR
         CALL SURADJ(IZONE,ISURF,IEX,T,IZC,ISC,ICN,CSTR)
         if (IZC.eq.IZ) then 

C Check if the coupled surface ISC is in the surface list NSNOG, if it 
C is then we have to edit the matching surface in this zone ISURF.
          do 60 JJ=1,NSGU(ICL,II)
           IS=NSNOG(ICL,II,JJ)
           if (IS.eq.ISC) then
            if (SMLCN(IZONE,ISURF)(1:12).eq.MATNAM(ICC,1)(1:12))then

C Find matching MLC.
             call matchmlcdesc(SMLCN(IZONE,isurf),imlc)
             do 50 IE=1,NE(ISURF)
              write (36,*) 'iprmat(imlc,ie): ',IPRMAT(IMLC,IE),imlc,ie
              write (36,*) 'IDATR(ICC,1): ',IDATR(ICC,1),icc
              if (IPRMAT(IMLC,IE).eq.IDATR(ICC,1)) then

C Either edit layer thickness ( not yet implemented MLC edit).
C Note the next few lines are not indented wrt the 'if' statement above.
               if (IE.eq.0) then
                write (36,*) 'MLC editing not enabled.'
               else
                write (36,*) 'Editing adjacent element thickness..'
                write (36,*) 'Zone:',IZONE,' Surface:',ISURF,
     &                                                 ' Element:',IE
                write (36,*) 'THRMLI: ',THRMLI(IZONE,ISURF,IE,4)
                if (IDATR(ICC,3).eq.1) then

C Percentage change. Ensure it does not go below 1mm. 
                 THRMLI(IZONE,ISURF,IE,4)=THRMLI(IZONE,ISURF,IE,4)*
     &             (((DATU(ICC,1)/100.)*DIR)+1.0)
                 if(THRMLI(IZONE,ISURF,IE,4).lt.0.001)then
                   THRMLI(IZONE,ISURF,IE,4)=0.001
                 endif
                elseif (IDATR(ICC,3).eq.2) then

C Absolute change. Ensure it does not go below 1mm. 
                 THRMLI(IZONE,ISURF,IE,4)=THRMLI(IZONE,ISURF,IE,4)+
     &             (DATU(ICC,1)*DIR)
                 if(THRMLI(IZONE,ISURF,IE,4).lt.0.001)then
                   THRMLI(IZONE,ISURF,IE,4)=0.001
                 endif
                elseif (IDATR(ICC,3).eq.3) then

C Explicit change. Ensure it does not go below 1mm. 
                 if (DIR.gt.0.) then
                  THRMLI(IZONE,ISURF,IE,4)=THRMLI(IZONE,ISURF,IE,4)+
     &              (DATU(ICC,1)-THRMLI(IZONE,ISURF,IE,4))*DIR
                 else
                  THRMLI(IZONE,ISURF,IE,4)=THRMLI(IZONE,ISURF,IE,4)-
     &              (DATU(ICC,2)-THRMLI(IZONE,ISURF,IE,4))*DIR
                 endif
                 if(THRMLI(IZONE,ISURF,IE,4).lt.0.001)then
                   THRMLI(IZONE,ISURF,IE,4)=0.001
                 endif
                endif
                write (36,*) 'to:'
                write (36,*) 'THRMLI: ',THRMLI(IZONE,ISURF,IE,4)
               endif
              endif
 50          continue
            endif
           endif
 60       continue
         endif
 40     continue
       endif
 10   continue

C If this was the last uncertainty i.e. if IACT=NIACT header
C and we are on the last zone then the header is done.
      if(IACT.eq.NIACT)then
        if(IZONE.eq.NCOMP)then
          write(36,*) 'set header buffer done in UAE02 ',itemsinheader
          headerdone=.true.
        endif
      endif

      RETURN
      END

C ******************** UAE04 ********************
C UAE04 edits casual gains and scheduled air movement in memory.

      SUBROUTINE UAE04(IZONE,IACT,DIR)
#include "building.h"
#include "schedule.h"
C Common blocks UA2 UA3 UA4 within uncertainty.h
#include "uncertainty.h"

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      character USETNAME*96   ! the synopsis for each MC-run
      character uheader*1000  ! header including name of each uncertainty changed
      character urunvals*1000 ! attribute changed-to value of each uncertainty change for the run.
      COMMON/USETN/USETNAME(MNRS),uheader,urunvals

      logical headerdone
      logical itemunique  ! should we include in fort.37
      logical isunique    ! for test against past
      integer itemsinheader,nbpastitems,nbdatahits
      character pastphrase*42
      common/uhead/headerdone,itemunique(400),pastphrase(400),
     &  itemsinheader,nbpastitems,nbdatahits

      integer lstur, lfnur
      character strval*12,uhphrase*42,uhroot*42

C Data stored in UA04 has the following meaning in this routine:
C IDATR(?,1): Casual gain type(1/2/3)/ scheduled air flow(0)
C IDATR(?,2): Cas Gn: sensible(1)/ latent(2)/ radiant(3)/ convective(4)/
C IDATR(?,2):         total Q (same split) (5)/ Period start (6)/
C IDATR(?,2):         N/A (7)/ 
C IDATR(?,2):         same total frac diff split (8)
C IDATR(?,2): Sched Air flow: infiltration(1)/ ventilation(2)/
C IDATR(?,2):         N/A (3)/ vent source temp(4)
C IDATR(?,3): Change type (%; +/-; explicit).
C IDATR(?,4): Day type index << new >>.
C IDATR(?,5): Period index   << new >>.
C Define variables for current location and change.
      if(urunvals(1:2).eq.'  ')then
        lstur=1; lfnur=1  ! reset counters for urunvals buffer
      else
        lstur=lnblnk(urunvals)+2; lfnur=lnblnk(urunvals)+2  ! to append to urunvals buffer
      endif
      ICL=IACTD(IACT,2) ! which variable
      ICC=IACTD(IACT,1) ! cas or flow type
      ICD=IDATR(ICC,4)  ! day type
      ICP=IDATR(ICC,5)  ! period in day

C If initial append to uheader.
      if(uheader(1:2).eq.'  ')then
        lstuh=1; lfnuh=1  ! reset counters for uheader buffer
      else
        lstuh=lnblnk(uheader)+2; lfnuh=lnblnk(uheader)+2  ! to append to uheader buffer
      endif

      write (36,*) '   '
      write (36,*) 'Entering subroutine UAE04'
      write (36,*) 'Focus zone: ',IZONE

C Loop through all zones listed in NZNOGU for the defined location ICL.
      do 10 II=1,NZGU(ICL)
        IZ=NZNOGU(ICL,II)
        if (IZ.eq.IZONE) then

C Edit casual gains.
          if (IDATR(ICC,1).eq.0) then

C Edit scheduled air flows.
C Check that we have the right element(period). << TO DO >>
C Currently changing all defined periods.
            write (36,*) 'Editing scheduled air flow'
            ICF=IDATR(ICC,3)
            DAT1=DATU(ICC,1)
            DAT2=DATU(ICC,2)

C Make specific header string for each change (to match urunvals)
            lncng=lnblnk(LCNG(ICC))
            lnloc=lnblnk(LLOC(ICL))
            if(lncng.gt.18)lncng=18
            uhphrase=' '
            write(uhphrase,'(4a,i2.2,a)')LCNG(ICC)(1:lncng),':',
     &        LLOC(ICL)(1:lnloc),':',IZ,','
            luhphrase=lnblnk(uhphrase)
            write(36,'(a)') uhphrase(1:luhphrase)

C Remember the root name of the phrase so that subsequent duplicates can
C be ignored in the fort.37 file. If the initial item the increment counters
C otherwise search pastphrase(s) for match.
            uhroot=' '
            lncng=lnblnk(LCNG(ICC))
            lnloc=lnblnk(LLOC(ICL))
            write(uhroot,'(4a)')LCNG(ICC)(1:lncng),
     &        ':',LLOC(ICL)(1:lnloc),','
            luhroot=lnblnk(uhroot)
            write(36,'(a)') uhroot(1:luhroot)
            if(itemsinheader.eq.0)then
              nbpastitems=nbpastitems+1
              write(pastphrase(nbpastitems),'(a)') uhroot(1:luhroot)
              itemunique(1)=.TRUE.
              write(36,*) 'unique item',nbpastitems,uhroot(1:luhroot)
            elseif(itemsinheader.eq.1)then
              lnpast=lnblnk(pastphrase(1))
              if(uhroot(1:luhroot).eq.pastphrase(1)(1:lnpast))then
                itemunique(itemsinheader+1)=.FALSE.
              else
                itemunique(itemsinheader+1)=.TRUE.
                nbpastitems=nbpastitems+1
                write(pastphrase(nbpastitems),'(a)') uhroot(1:luhroot)
                write(36,*) 'unique item',nbpastitems,uhroot(1:luhroot)
              endif
            else

C If we ever get a match unset isunique.
              isunique=.true.
              do loop=1,nbpastitems
                lnpast=lnblnk(pastphrase(loop))
                if(uhroot(1:luhroot).eq.pastphrase(loop)(1:lnpast))then
                  isunique=.false.
                endif
              enddo
              if(isunique)then
                itemunique(itemsinheader+1)=.TRUE.
                nbpastitems=nbpastitems+1
                write(pastphrase(nbpastitems),'(a)') uhroot(1:luhroot)
                write(36,*)'unique item',nbpastitems,uhroot(1:luhroot)
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
      
            strval=' '        ! clear string buffer for the value changed

C Percentage change. 
C What data type to change? (see comments at start of routine).
            if (IDATR(ICC,2).eq.1) then
              write (36,*) 'Zone:',IZONE,' infiltration DIR is ',DIR
              T=ACI(ICD,ICP)
              call UAEDIT(ACI(ICD,ICP),ICF,DAT1,DAT2,DIR,-1,
     &          0.0,0.0)
              write(36,*)'Period ',CALENTAG(ICD),ICP,' From:',T,
     &          ' to ',ACI(ICD,ICP)
            elseif (IDATR(ICC,2).eq.2) then
              write (36,*) 'Zone:',IZONE,' ventilation DIR is ',DIR
              T=ACV(ICD,ICP)
              call UAEDIT(ACV(ICD,ICP),ICF,DAT1,DAT2,DIR,-1,
     &          0.0,0.0)
              write(36,*)'Period ',CALENTAG(ICD),ICP,' From:',T,
     &          ' to ',ACV(ICD,ICP)
            elseif (IDATR(ICC,2).eq.3) then
              write (36,*)'Illegal option, cannot change zone index'
            elseif (IDATR(ICC,2).eq.4) then
              write (36,*) 'Zone:',IZONE,' vent source temperature',
     &          ' DIR is ',DIR
              T=TA(ICD,ICP)
              call UAEDIT(TA(ICD,ICP),ICF,DAT1,DAT2,DIR,-1,
     &          0.0,0.0)
              write(36,*)'Period ',CALENTAG(ICD),ICP,' From:',T,
     &          ' to ',TA(ICD,ICP)
            endif

C Increment counter of data so as to check against itemunique array.
            nbdatahits=nbdatahits+1
            write(36,*) 'UAE04 setting nbdatahits',nbdatahits

C Add to urunvals if previously marked as unique.
            if(itemunique(nbdatahits))then

C Write DIR to fort.37.
              call relstr(DIR,strval,lna,ier)
              lstrval=lnblnk(strval)
              lfnur=lstur+lstrval
              if(lfnur.lt.1000)then
                write(urunvals(lstur:lfnur),'(a)') strval(1:lstrval)
                lstur=lfnur+2
              endif
            endif
          else

C Edit casual gains.
C Check that we have the right element(period).
            write (36,*) 'Editing casual gains'
            ICF=IDATR(ICC,3)  ! change type
            ICD=IDATR(ICC,4)  ! day type
            ICP=IDATR(ICC,5)  ! period in day
            DAT1=DATU(ICC,1)
            DAT2=DATU(ICC,2)
            IT=IDATR(ICC,1)   ! casual gain type
            write (36,*) 'CG type:',IT,' Day:',ICD,' period:',ICP

C Make specific header string for each change (to match urunvals)
            lncng=lnblnk(LCNG(ICC))
            lnloc=lnblnk(LLOC(ICL))
            if(lncng.gt.18)lncng=18
            uhphrase=' '
            write(uhphrase,'(4a,i2.2,a)')LCNG(ICC)(1:lncng),':',
     &        LLOC(ICL)(1:lnloc),':',IZ,','
            luhphrase=lnblnk(uhphrase)
            write(36,'(a)') uhphrase(1:luhphrase)

C Remember the root name of the phrase so that subsequent duplicates can
C be ignored in the fort.37 file. If the initial item the increment counters
C otherwise search pastphrase(s) for match.
            uhroot=' '
            write(uhroot,'(4a)')LCNG(ICC)(1:lncng),
     &        ':',LLOC(ICL)(1:lnloc),','
            luhroot=lnblnk(uhroot)
            write(36,'(a)') uhroot(1:luhroot)
            if(itemsinheader.eq.0)then
              nbpastitems=nbpastitems+1
              write(pastphrase(nbpastitems),'(a)') uhroot(1:luhroot)
              itemunique(1)=.TRUE.
              write(36,*) 'unique item',nbpastitems,uhroot(1:luhroot)
            elseif(itemsinheader.eq.1)then
              lnpast=lnblnk(pastphrase(1))
              if(uhroot(1:luhroot).eq.pastphrase(1)(1:lnpast))then
                itemunique(itemsinheader+1)=.FALSE.
              else
                itemunique(itemsinheader+1)=.TRUE.
                nbpastitems=nbpastitems+1
                write(pastphrase(nbpastitems),'(a)') uhroot(1:luhroot)
                write(36,*) 'unique item',nbpastitems,uhroot(1:luhroot)
              endif
            else

C If we ever get a match unset isunique.
              isunique=.true.
              do loop=1,nbpastitems
                lnpast=lnblnk(pastphrase(loop))
                if(uhroot(1:luhroot).eq.pastphrase(loop)(1:lnpast))then
                  isunique=.false.
                endif
              enddo
              if(isunique)then
                itemunique(itemsinheader+1)=.TRUE.
                nbpastitems=nbpastitems+1
                write(pastphrase(nbpastitems),'(a)') uhroot(1:luhroot)
                write(36,*)'unique item',nbpastitems,uhroot(1:luhroot)
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
      
            strval=' '        ! clear string buffer for the value changed

C The next section has the following format:
C  Check casual gain parameter to edit (sensible, latent, convect %...
C  Check if required casual gain type is defined for this period (occ, lights..
C    note weekdays/ sat/ sun checked seperately.
C
            if (IDATR(ICC,2).eq.1) then
              write (36,'(a,i3,a,i2,a)') 'Zone:',IZONE,' CG type:',
     &          IT,' sensible'
              if (IDATR(ICC,1).eq.iabs(ICGT(ICD,ICP))) then
                T=CMGS(ICD,ICP)
                call UAEDIT(CMGS(ICD,ICP),ICF,DAT1,DAT2,DIR,-1,
     &            0.0,0.0)
                call relstr(CMGS(ICD,ICP),strval,lna,ier)
                write(36,*)'Period ',CALENTAG(ICD),ICP,' From:',T,
     &            ' to ',TA(ICD,ICP)
              endif
            elseif (IDATR(ICC,2).eq.2) then
              write (36,'(a,i3,a,i2,a)') 'Zone:',IZONE,' GC type:',
     &          IT,' latent'
              if (IDATR(ICC,1).eq.iabs(ICGT(ICD,ICP))) then
                T1=CMGL(ICD,ICP)
                call UAEDIT(CMGL(ICD,ICP),ICF,DAT1,DAT2,DIR,
     &            -1,0.0,0.0)
                write (36,*)'Period ',CALENTAG(ICD),ICP,
     &            ' From:',T1,' to ',CMGL(ICD,ICP)
                call relstr(CMGL(ICD,ICP),strval,lna,ier)
              endif
            elseif (IDATR(ICC,2).eq.3) then
              write (36,'(a,i3,a,i2,a)') 'Zone:',IZONE,' GC type:',
     &          IT,' rad fraction'
              if (IDATR(ICC,1).eq.iabs(ICGT(ICD,ICP))) then
                T1=RADC(ICD,ICP)
                call UAEDIT(RADC(ICD,ICP),ICF,DAT1,DAT2,DIR,
     &            0,0.0,1.0)
                write (36,*)'Period ',CALENTAG(ICD),ICP,
     &            ' From:',T1,' to ',RADC(ICD,ICP)
                CONC(ICD,ICP)=1.0-RADC(ICD,ICP)  ! adjust CONC
                call relstr(RADC(ICD,ICP),strval,lna,ier)
              endif
            elseif (IDATR(ICC,2).eq.4) then
              write (36,'(a,i3,a,i2,a)')'Zone:',IZONE,' GC type:',IT,
     &          ' conv fraction'
              if (IDATR(ICC,1).eq.iabs(ICGT(ICD,ICP))) then
                T1=CONC(ICD,ICP)
                call UAEDIT(CONC(ICD,ICP),ICF,DAT1,DAT2,DIR,
     &            0,0.0,1.0)
                write (36,*)'Period ',CALENTAG(ICD),ICP,
     &            ' From:',T1,' to ',CONC(ICD,ICP)
                RADC(ICD,ICP)=1.0-CONC(ICD,ICP)  ! adjust RADC
                call relstr(CONC(ICD,ICP),strval,lna,ier)
              endif
            elseif (IDATR(ICC,2).eq.5) then
              write (36,'(a,i3,a,i2,a)') 'Zone:',IZONE,' GC type:',IT,
     &          ' total Q'
              if (IDATR(ICC,1).eq.iabs(ICGT(ICD,ICP))) then
                T1=CMGS(ICD,ICP)
                T2=CMGL(ICD,ICP)
                T3=T1+T2
                call UAEDIT(T3,ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
                CMGS(ICD,ICP)=T3*(CMGS(ICD,ICP)/(T1+T2))
                CMGL(ICD,ICP)=T3*(CMGL(ICD,ICP)/(T1+T2))
                write (36,*)'Period ',CALENTAG(ICD),ICP,
     &            ' Sensible from:',T1,' to ',CMGS(ICD,ICP)
                write (36,*)'Period ',CALENTAG(ICD),ICP,
     &            ' Latent from:',T2,' to ',CMGL(ICD,ICP)
                call relstr(CMGS(ICD,ICP),strval,lna,ier)
              endif
            elseif (IDATR(ICC,2).eq.6) then

C Shift the start time of subsequent periods. If DAT1 is ~1 then
C adjust by 1 hour, if DAT1 is ~2 then attempt to adjust by 2 hours.
              write (36,'(a,i3,a,i2,a)') 'Zone:',IZONE,' CG type:',IT,
     &          ' start'
              if (IDATR(ICC,1).eq.iabs(ICGT(ICD,ICP))) then
                if(ICP.gt.1)then
                  IT1=ICGS(ICD,ICP)
                  if(DAT1.gt.0.0.and.DAT1.le.1.0) IV=1  ! shift 1 hour
                  if(DAT1.gt.1.0.and.DAT1.le.2.0) IV=2  ! shift 2 hours
                  if (DIR.gt.0.) then
                    if(IV.eq.1)then
                      if(ICGS(ICD,ICP)+1.lt.ICGS(ICD,ICP+1))then
                        ICGS(ICD,ICP)=ICGS(ICD,ICP)+1      ! if clear to shift later
                        ICGF(ICD,ICP-1)=ICGF(ICD,ICP-1)+1  ! and shift prior fn
                      endif
                    elseif(IV.eq.2)then
                      if(ICGS(ICD,ICP)+2.lt.ICGS(ICD,ICP+2))then
                        ICGS(ICD,ICP)=ICGS(ICD,ICP)+2      ! if clear to shift later
                        ICGF(ICD,ICP-1)=ICGF(ICD,ICP-1)+2  ! and shift prior fn
                      elseif(ICGS(ICD,ICP)+1.lt.ICGS(ICD,ICP+1))then
                        ICGS(ICD,ICP)=ICGS(ICD,ICP)+1      ! if clear to shift later
                        ICGF(ICD,ICP-1)=ICGF(ICD,ICP-1)+1  ! and shift prior fn
                      endif
                    endif
                  else
                    if(IV.eq.1)then
                      if(ICGS(ICD,ICP)-1.gt.ICGS(ICD,ICP-1))then
                        ICGS(ICD,ICP)=ICGS(ICD,ICP)-1      ! if clear to shift earlier
                        ICGF(ICD,ICP-1)=ICGF(ICD,ICP-1)-1  ! and shift prior fn
                      endif
                    elseif(IV.eq.2)then
                      if(ICGS(ICD,ICP)-2.gt.ICGS(ICD,ICP-2))then
                        ICGS(ICD,ICP)=ICGS(ICD,ICP)-2      ! if clear to shift earlier
                        ICGF(ICD,ICP-1)=ICGF(ICD,ICP-1)-2  ! and shift prior fn
                      elseif(ICGS(ICD,ICP)-1.gt.ICGS(ICD,ICP-1))then
                        ICGS(ICD,ICP)=ICGS(ICD,ICP)-1      ! if clear to shift earlier
                        ICGF(ICD,ICP-1)=ICGF(ICD,ICP-1)-1  ! and shift prior fn
                      endif
                    endif
                  endif
                  write (36,*)'Period ',CALENTAG(ICD),ICP,
     &              ' From:',IT1,' to ',ICGS(ICD,ICP)
                  call relstr(real(ICGS(ICD,ICP)),strval,lna,ier)
                endif
              endif
            elseif (IDATR(ICC,2).eq.8) then
              IT=IDATR(ICC,1)
              write(36,*)'Zone:',IZONE,' type:',IT,' rad/conv split'
              if (IDATR(ICC,1).eq.iabs(ICGT(ICD,ICP))) then
                T1=RADC(ICD,ICP)
                T2=CONC(ICD,ICP)
                T3=T1+T2
                call UAEDIT(CONC(ICD,ICP),ICF,DAT1,DAT2,DIR,
     &            0,0.0,T3)
                RADC(ICD,ICP)=T3-CONC(ICD,ICP)
                write (36,*)'Period ',CALENTAG(ICD),ICP,
     &            ' Rad fraction from:',T1,' to ',RADC(ICD,ICP)
                write (36,*)'Period ',CALENTAG(ICD),ICP,
     &            ' Conv fraction from:',T2,' to ',CONC(ICD,ICP)
                call relstr(RADC(ICD,ICP),strval,lna,ier)
              endif
            else
              write (36,*)'Editing request unknown.'
            endif

C Increment counter of data so as to check against itemunique array.
            nbdatahits=nbdatahits+1
C            write(36,*) 'UAE04 setting nbdatahits',nbdatahits

C Add to urunvals if previously marked as unique.
            if(itemunique(nbdatahits))then

C Write DIR to fort.37.
              call relstr(DIR,strval,lna,ier)
              lstrval=lnblnk(strval)
              lfnur=lstur+lstrval
              if(lfnur.lt.1000)then
                write(urunvals(lstur:lfnur),'(a)') strval(1:lstrval)
                lstur=lfnur+2
              endif
            endif
          endif
        endif
 10   continue

C If this was the last uncertainty i.e. if IACT=NIACT header is done.
      if(IACT.eq.NIACT)then
        if(IZONE.eq.NCOMP)then
          write(36,*) 'set header buffer done in UAE04 ',itemsinheader
          headerdone=.true.
        endif
      endif

      RETURN
      END

C ******************** UAE05 ********************
C UAE05 edits convection coefficients algorithm choice.
C Note: inclusion in USETN commons not yet implemented.

      SUBROUTINE UAE05(IZONE,IACT,DIR)
#include "building.h"
#include "geometry.h"
C Common blocks UA2 UA3 UA4 within uncertainty.h
#include "uncertainty.h"

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      common/CORIND/ICORI(MCOM,MS),ICORE(MCOM,MS)

      character USETNAME*96   ! the synopsis for each MC-run
      character uheader*1000  ! header including name of each uncertainty changed
      character urunvals*1000 ! attribute changed-to value of each uncertainty change for the run.
      COMMON/USETN/USETNAME(MNRS),uheader,urunvals

      integer lstur, lfnur
      character strval*12,uhphrase*42

C Data stored in UA04 has the following meaning in this routine:
C IDATR(?,1): =1 just now - associated data is convection corr to use
C IDATR(?,2): =1 just now - associated data is convection corr to use
C IDATR(?,3): 4 - alternative correlation id number
C Define variables for current location and change.
      if(urunvals(1:2).eq.'  ')then
        lstur=1; lfnur=1  ! reset counters for urunvals buffer
      else
        lstur=lnblnk(urunvals)+2; lfnur=lnblnk(urunvals)+2  ! to append to urunvals buffer
      endif
      strval=' '        ! clear string buffer for the value changed
      ICL=IACTD(IACT,2)
      ICC=IACTD(IACT,1)

      write (36,*) 'Entering subroutine UAE05'
      write (36,*) 'Focus zone: ',IZONE

      write (36,*) ' Existing correlations: '
      write (36,*) '  ',(ICORI(IZONE,JS),JS=1,NSUR)
      write (36,*) 'IZONE:',IZONE,'  NZGU(ICL):',NZGU(ICL)

C Loop through all zones listed in NZNOGU for the defined location ICL.
      do 10 II=1,NZGU(ICL)
        IZ=NZNOGU(ICL,II)
      write (36,*) 'NZNOGU(ICL,II):',NZNOGU(ICL,II),
     &             '  NSGU(ICL,II):',NSGU(ICL,II)
        if (IZ.eq.IZONE) then
          do 20 IJ=1,NSGU(ICL,II)
            IS=NSNOG(ICL,II,IJ)
            write (36,*) 'NSNOG(ICL,II,IJ):',NSNOG(ICL,II,IJ)
            if (DIR.gt.0.) then
              ICORI(IZ,IS)=nint(DATU(ICC,1))
            elseif (DIR.lt.0.) then
              ICORI(IZ,IS)=nint(DATU(ICC,2))
            endif
 20       continue
        endif
 10   continue
      write (36,*) ' New correlations: '
      write (36,*) '  ',(ICORI(IZONE,JS),JS=1,NSUR)
      write (36,*) '   '

      RETURN
      END

C ******************** UAE06 ********************
C UAE06 edits setpoint of blind control.

      SUBROUTINE UAE06(IZONE,IACT,DIR)
#include "building.h"
C #include "geometry.h"
C Common blocks UA2 UA3 UA4 within uncertainty.h
#include "uncertainty.h"

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/TMCB3/NBCTT(MCOM,MTMC),BACTPT(MCOM,MTMC)

      character USETNAME*96   ! the synopsis for each MC-run
      character uheader*1000  ! header including name of each uncertainty changed
      character urunvals*1000 ! attribute changed-to value of each uncertainty change for the run.
      COMMON/USETN/USETNAME(MNRS),uheader,urunvals

      logical headerdone
      logical itemunique  ! should we include in fort.37
      logical isunique    ! for test against past
      integer itemsinheader,nbpastitems,nbdatahits
      character pastphrase*42
      common/uhead/headerdone,itemunique(400),pastphrase(400),
     &  itemsinheader,nbpastitems,nbdatahits


      integer lstur, lfnur
      character strval*12,uhphrase*42,uhroot*42

C Data stored in UA06 has the following meaning in this routine:
C IDATR(?,1): =1 signals setpoint to change
C IDATR(?,2): =0 not yet used
C IDATR(?,3): Change type (%; +/-; explicit).
C Define variables for current location and change.
      if(urunvals(1:2).eq.'  ')then
        lstur=1; lfnur=1  ! reset counters for urunvals buffer
      else
        lstur=lnblnk(urunvals)+2; lfnur=lnblnk(urunvals)+2  ! to append to urunvals buffer
      endif
      strval=' '        ! clear string buffer for the value changed
      ICL=IACTD(IACT,2)
      ICC=IACTD(IACT,1)
      DAT1=DATU(ICC,1)
      DAT2=DATU(ICC,2)
      ICF=IDATR(ICC,3)

C If initial append to uheader.
      if(uheader(1:2).eq.'  ')then
        lstuh=1; lfnuh=1  ! reset counters for uheader buffer
      else
        lstuh=lnblnk(uheader)+2; lfnuh=lnblnk(uheader)+2  ! to append to uheader buffer
      endif

C Loop through all zones listed in NZNOGU for the defined location ICL.
      do 10 II=1,NZGU(ICL)
        IZ=NZNOGU(ICL,II)
        if (IZ.eq.IZONE) then

          write (36,*) 'Entering subroutine UAE06'
          write (36,*) 'Focus zone: ',IZONE,' DIR:',DIR,' ACT:',
     &      IACT,' NTGU:',NTGU(ICL,II,1),NTGU(ICL,II,2)

C Make specific header string for each change (to match urunvals).
          lncng=lnblnk(LCNG(ICC))
          lnloc=lnblnk(LLOC(ICL))
          if(lncng.gt.18)lncng=18
          uhphrase=' '
          write(uhphrase,'(4a,i2.2,a,i3.3,a)')LCNG(ICC)(1:lncng),
     &      ':',LLOC(ICL)(1:lnloc),':',IZ,':',NTGU(ICL,II,1),','
          luhphrase=lnblnk(uhphrase)
          write(36,'(a)') uhphrase(1:luhphrase)

C Remember the root name of the phrase so that subsequent duplicates can
C be ignored in the fort.37 file. If the initial item the increment counters
C otherwise search pastphrase(s) for match.
          uhroot=' '
          write(uhroot,'(4a)')LCNG(ICC)(1:lncng),
     &      ':',LLOC(ICL)(1:lnloc),','
          luhroot=lnblnk(uhroot)
          write(36,'(a)') uhroot(1:luhroot)
          if(itemsinheader.eq.0)then
            nbpastitems=nbpastitems+1
            write(pastphrase(nbpastitems),'(a)') uhroot(1:luhroot)
            itemunique(1)=.TRUE.
            write(36,*) 'unique item',nbpastitems,uhroot(1:luhroot)
          elseif(itemsinheader.eq.1)then
            lnpast=lnblnk(pastphrase(1))
            if(uhroot(1:luhroot).eq.pastphrase(1)(1:lnpast))then
              itemunique(itemsinheader+1)=.FALSE.
            else
              itemunique(itemsinheader+1)=.TRUE.
              nbpastitems=nbpastitems+1
              write(pastphrase(nbpastitems),'(a)') uhroot(1:luhroot)
              write(36,*)'unique item',nbpastitems,uhroot(1:luhroot)
            endif
          else

C If we ever get a match unset isunique.
            isunique=.true.
            do loop=1,nbpastitems
              lnpast=lnblnk(pastphrase(loop))
              if(uhroot(1:luhroot).eq.
     &           pastphrase(loop)(1:lnpast))then
                isunique=.false.
              endif
            enddo
            if(isunique)then
              itemunique(itemsinheader+1)=.TRUE.
              nbpastitems=nbpastitems+1
              write(pastphrase(nbpastitems),'(a)')uhroot(1:luhroot)
              write(36,*)'unique item',nbpastitems,
     &          uhroot(1:luhroot)
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
      
          strval=' '        ! clear string buffer for the value changed

          write (36,'(a,i3,a,2i3,a,f7.1)') 
     &      'NZNOGU(ICL,II):',NZNOGU(ICL,II),
     &      ' NTGU:',NTGU(ICL,II,1),NTGU(ICL,II,2),' BACTPT:',
     &       BACTPT(IZ,NTGU(ICL,II,1))

          T1=BACTPT(IZ,NTGU(ICL,II,1))
          T2=BACTPT(IZ,NTGU(ICL,II,1))
          call UAEDIT(T2,ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
          BACTPT(IZ,NTGU(ICL,II,1))=T2

C Increment counter of data so as to check against itemunique array.
          nbdatahits=nbdatahits+1
          write(36,*) 'UAE06 setting nbdatahits',nbdatahits,T1,'->',T2

C Add to urunvals if previously marked as unique.
          if(itemunique(nbdatahits))then

C Write DIR to fort.37.
            call relstr(DIR,strval,lna,ier)
            lstrval=lnblnk(strval)
            lfnur=lstur+lstrval
            if(lfnur.lt.1000)then
              write(urunvals(lstur:lfnur),'(a)') strval(1:lstrval)
              lstur=lfnur+2
            endif
          endif  ! itemunique
        endif    ! the zone is correct
 10   continue
      write (36,*) '   '

C If this was the last uncertainty i.e. if IACT=NIACT
C and we have done all the zones then the header is done.
      if(IACT.eq.NIACT)then
        if(IZONE.eq.NCOMP)then
          write(36,*) 'set header buffer done in UAE06 ',itemsinheader
          headerdone=.true.
        endif
      endif

      RETURN
      END

C ******************** UAZ01 ********************
C UAZ01 edits zone air volume in memory or surface area.

      SUBROUTINE UAZ01(IZONE,IACT,DIR)
#include "building.h"
#include "geometry.h"
#include "uncertainty.h"
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      common/prec12/ei(mcom,ms),ee(mcom,ms),ai(mcom,ms),ae(mcom,ms)
      COMMON/PREC14/emarea(MCOM)

      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)

      character USETNAME*96   ! the synopsis for each MC-run
      character uheader*1000  ! header including name of each uncertainty changed
      character urunvals*1000 ! attribute changed-to value of each uncertainty change for the run.
      COMMON/USETN/USETNAME(MNRS),uheader,urunvals

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

C Data stored in UAZ01 has the following meaning in this routine:
C IDATR(?,1): 1=air, 2=surface area
C IDATR(?,2): 1=volume, ...
C IDATR(?,3): Change type (%; +/-; explicit).
C Define variables for current location and change.
      if(urunvals(1:2).eq.'  ')then
        lstur=1; lfnur=1  ! reset counters for urunvals buffer
      else
        lstur=lnblnk(urunvals)+2; lfnur=lnblnk(urunvals)+2  ! to append to urunvals buffer
      endif
      ICL=IACTD(IACT,2)
      ICC=IACTD(IACT,1)

C If initial append to uheader.
      if(uheader(1:2).eq.'  ')then
        lstuh=1; lfnuh=1  ! reset counters for uheader buffer
      else
        lstuh=lnblnk(uheader)+2; lfnuh=lnblnk(uheader)+2  ! to append to uheader buffer
      endif

      write (36,*) '   '
      write (36,*) 'Entering subroutine UAZ01'
      write (36,*) 'Focus zone: ',IZONE

C Loop through all zones listed in NZNOGU for the defined location ICL.
      do 10 II=1,NZGU(ICL)
        IZ=NZNOGU(ICL,II)
        if (IZ.eq.IZONE) then

C Edit zone air volume.
          if (IDATR(ICC,1).eq.1) then
            write (36,*) 'Editing zone volume..'
            ICF=IDATR(ICC,3)
            DAT1=DATU(ICC,1)
            DAT2=DATU(ICC,2)

C Make specific header string for each change (to match urunvals)
            lncng=lnblnk(LCNG(ICC))
            lnloc=lnblnk(LLOC(ICL))
            if(lncng.gt.18)lncng=18
            uhphrase=' '
            write(uhphrase,'(4a,i2.2,a,i3.3,a)')LCNG(ICC)(1:lncng),
     &        ':',LLOC(ICL)(1:lnloc),':',IZ,':',IS,','
            luhphrase=lnblnk(uhphrase)
            write(36,'(a)') uhphrase(1:luhphrase)

C Remember the root name of the phrase so that subsequent duplicates can
C be ignored in the fort.37 file. If the initial item the increment counters
C otherwise search pastphrase(s) for match.
            uhroot=' '
            write(uhroot,'(4a)')LCNG(ICC)(1:lncng),
     &        ':',LLOC(ICL)(1:lnloc),','
            luhroot=lnblnk(uhroot)
            write(36,'(a)') uhroot(1:luhroot)
            if(itemsinheader.eq.0)then
              nbpastitems=nbpastitems+1
              write(pastphrase(nbpastitems),'(a)') uhroot(1:luhroot)
              itemunique(1)=.TRUE.
              write(36,*) 'unique item',nbpastitems,uhroot(1:luhroot)
            elseif(itemsinheader.eq.1)then
              lnpast=lnblnk(pastphrase(1))
              if(uhroot(1:luhroot).eq.pastphrase(1)(1:lnpast))then
                itemunique(itemsinheader+1)=.FALSE.
              else
                itemunique(itemsinheader+1)=.TRUE.
                nbpastitems=nbpastitems+1
                write(pastphrase(nbpastitems),'(a)') uhroot(1:luhroot)
                write(36,*) 'unique item',nbpastitems,uhroot(1:luhroot)
              endif
            else

C If we ever get a match unset isunique.
              isunique=.true.
              do loop=1,nbpastitems
                lnpast=lnblnk(pastphrase(loop))
                if(uhroot(1:luhroot).eq.pastphrase(loop)(1:lnpast))then
                  isunique=.false.
                endif
              enddo
              if(isunique)then
                itemunique(itemsinheader+1)=.TRUE.
                nbpastitems=nbpastitems+1
                write(pastphrase(nbpastitems),'(a)') uhroot(1:luhroot)
                write(36,*)'unique item',nbpastitems,uhroot(1:luhroot)
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
      
            strval=' '        ! clear string buffer for the value changed
            if (IDATR(ICC,2).eq.1) then 
              write (36,*) 'Zone: ',IZ,' Volume: ',VOL(IZ)
              call UAEDIT(VOL(IZ),ICF,DAT1,DAT2,DIR,-1,-1.0,0.0)

C Check for out of range values.
              if (VOL(IZ).lt.0.01) then
                VOL(IZ)=1.0
                write (36,*) 'Zone volume less than 0.0, setting to 1.0'
              endif
              write (36,*)'to:                     ',VOL(IZ)
            endif
          elseif (IDATR(ICC,1).eq.2) then
            write (36,*) 'Editing surface area...'
            ICF=IDATR(ICC,3)
            DAT1=DATU(ICC,1)
            DAT2=DATU(ICC,2)
            do JJ=1,NSGU(ICL,II)
              IS=NSNOG(ICL,II,JJ)

C Make specific header string for each change (to match urunvals)
              lncng=lnblnk(LCNG(ICC))
              lnloc=lnblnk(LLOC(ICL))
              if(lncng.gt.18)lncng=18
              uhphrase=' '
              write(uhphrase,'(4a,i2.2,a,i3.3,a)')LCNG(ICC)(1:lncng),
     &          ':',LLOC(ICL)(1:lnloc),':',IZ,':',IS,','
              luhphrase=lnblnk(uhphrase)
              write(36,'(a)') uhphrase(1:luhphrase)

C Remember the root name of the phrase so that subsequent duplicates can
C be ignored in the fort.37 file. If the initial item the increment counters
C otherwise search pastphrase(s) for match.
              uhroot=' '
              write(uhroot,'(4a)')LCNG(ICC)(1:lncng),
     &          ':',LLOC(ICL)(1:lnloc),','
              luhroot=lnblnk(uhroot)
              write(36,'(a)') uhroot(1:luhroot)
              if(itemsinheader.eq.0)then
                nbpastitems=nbpastitems+1
                write(pastphrase(nbpastitems),'(a)') uhroot(1:luhroot)
                itemunique(1)=.TRUE.
                write(36,*) 'unique item',nbpastitems,uhroot(1:luhroot)
              elseif(itemsinheader.eq.1)then
                lnpast=lnblnk(pastphrase(1))
                if(uhroot(1:luhroot).eq.pastphrase(1)(1:lnpast))then
                  itemunique(itemsinheader+1)=.FALSE.
                else
                  itemunique(itemsinheader+1)=.TRUE.
                  nbpastitems=nbpastitems+1
                write(pastphrase(nbpastitems),'(a)') uhroot(1:luhroot)
                write(36,*) 'unique item',nbpastitems,uhroot(1:luhroot)
                endif
              else

C If we ever get a match unset isunique.
                isunique=.true.
                do loop=1,nbpastitems
                  lnpast=lnblnk(pastphrase(loop))
                  if(uhroot(1:luhroot).eq.
     &               pastphrase(loop)(1:lnpast))then
                    isunique=.false.
                  endif
                enddo
                if(isunique)then
                  itemunique(itemsinheader+1)=.TRUE.
                  nbpastitems=nbpastitems+1
                  write(pastphrase(nbpastitems),'(a)') uhroot(1:luhroot)
                  write(36,*)'unique item',nbpastitems,uhroot(1:luhroot)
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
      
              strval=' '        ! clear string buffer for the value changed
              write (36,'(a,f9.4,2i3)') 'From Area: ',SNA(IZ,IS),IZ,IS
              if (IDATR(ICC,2).eq.1) then 
                SNA(IZ,IS)=SNA(IZ,IS)*(((DATU(ICC,1)/100.)*DIR)+1.0)
                if(SNA(IZ,IS).lt.0.01) SNA(IZ,IS)=0.01
              elseif (IDATR(ICC,3).eq.2) then
                SNA(IZ,IS)=SNA(IZ,IS)+(DATU(ICC,1)*DIR)
                if(SNA(IZ,IS).lt.0.01) SNA(IZ,IS)=0.01
              endif
              write (36,'(a,f9.4,2i3)') 'To Area: ',SNA(IZ,IS),IZ,IS
            enddo
            write (36,*) 'sna= ',(SNA(IZ,JS),JS=1,nconst(IZ))

C Update emissivity area weighting to reflect change in SNA
            emarea(IZ)=0.0
            do kk=1,nconst(IZ)
              emarea(IZ)=emarea(IZ)+sna(IZ,kk)*ei(IZ,kk)
            enddo

C Increment counter of data so as to check against itemunique array.
            nbdatahits=nbdatahits+1
C            write(36,*) 'UAZ01 setting nbdatahits',nbdatahits

C Add to urunvals if previously marked as unique.
            if(itemunique(nbdatahits))then

C Write DIR to fort.37.
              call relstr(DIR,strval,lna,ier)
              lstrval=lnblnk(strval)
              lfnur=lstur+lstrval
              if(lfnur.lt.1000)then
                write(urunvals(lstur:lfnur),'(a)') strval(1:lstrval)
                lstur=lfnur+2
              endif
            endif
          endif
        endif
 10   continue

C If this was the last uncertainty i.e. if IACT=NIACT header is done.
      if(IACT.eq.NIACT)then
        if(IZONE.eq.NCOMP)then
          write(36,*) 'set header buffer done in UAZ01 ',itemsinheader
          headerdone=.true.
        endif
      endif

      RETURN
      END

C ******************** CHKPRIM ********************
C CHKPRIM scans the layers of the MLC for a specific surface
C and returns if and where a specific material is used.
      SUBROUTINE CHKPRIM(IZONE,ISURF,IPRIM,NEL,IEL)
#include "building.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"

C Passed parameters:
      integer izone  ! requested zone
      integer isurf  ! surface in the zone
      integer iprim  ! material array index
      integer nel    ! number of matching layers
      integer iel    ! array of the position of the matched material
      DIMENSION IEL(ME)

C Set number of matching layers to zero.
      NEL=0

C Find correct MLC. Check if the material array index matches
C the passed value. C Find matching MLC.
      call matchmlcdesc(SMLCN(IZONE,isurf),i)
      if(i.eq.0) return  ! no match found
      do 20 J=1,LAYERS(I)
        if (IPRMAT(I,J).eq.IPRIM) then
          NEL=NEL+1   ! matches criteria
          IEL(NEL)=J  ! layer it was found in
        endif
 20   continue

      RETURN
      END

C ******************** UAEDIT ********************
C UAEDIT edits the given data item (VAL) depending on change flag (ICFLAG)
C and given direction or magnitude (DIR).
C ICHK: -1 check min val, +1 check max val, 0 check both vals

      SUBROUTINE UAEDIT (VAL,ICFLAG,UPval,DOWNval,DIR,ichk,vmin,vmax)
#include "building.h"

C Debug.
      write(36,'(a)')'UAEDIT VAL ICFLAG UPval DWNval DIR ichk vmn vmx'
      write(36,*) VAL,ICFLAG,UPval,DOWNval,DIR,ichk,vmin,vmax

      if (ICFLAG.eq.1) then

C Percentage change. Logic works as long as UPval is less than or equal
C to 100%.  Over 100% the logic falls apart. 
        VAL=VAL*(((UPval/100.)*DIR)+1.0)
      elseif (ICFLAG.eq.2) then

C Absolute change. 
        VAL=VAL+(UPval*DIR)
      elseif (ICFLAG.eq.3) then

C Explicit change. 
        if (DIR.gt.0.) then
          VAL=VAL+(UPval*DIR)
        else
          VAL=VAL+(DOWNval*DIR)
        endif
      endif
      write(36,*) 'Updated VAL ',VAL
      
C Check limits.
      if (ICHK.le.0) then
        VAL=max(VAL,vmin)
      endif
      if (ICHK.ge.0) then
        VAL=min(VAL,vmax)
      endif
      write(36,*) 'Updated VAL after limit checks ',VAL
      

      RETURN
      END
