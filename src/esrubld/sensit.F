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
C   SENSA   The main uncertainty analysis controlling rtn.
C   DSA     Changes data for a Differential Sensitivity Analysis.
C   MCSA    Changes data for a Monte-Carlo Sensitivity Analysis.
C   UAE01   Edits data: conduct, density, specific ht, emissivity and 
C                       absorbtivity, dif res(not yet) - with error checking.
C   UAE02   Edits data: thickness and whole MLC's.
C   UAE04   Edits data: Casual gains.
C   UAE05   Edits data: Convection correlations.
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
C Common blocks UA4 within uncertainty.h
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      COMMON/UA5/IMET,ISIM,NRSIM
      COMMON/UAMET/IUNCMET
      COMMON/UAFCT/IADIR(MNFA),NFA

      character ITEM(3)*18,outs*124

      DIMENSION IVAL(3)

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='SENRNS'  ! set for subroutine
      helptopic='select_sensit_method'
      call gethelptext(helpinsub,helptopic,nbhelp)

      IER=0

C Is analysis method predefined?
 10   if (IUNCMET.gt.0) then
        IVAL(1)=IUNCMET
        INPIC=1
      else

C Ask for uncertainty analysis method first 
        call edisp(iuout,' Available analysis techniques... ')
        ITEM(1) ='Differential'
        ITEM(2) ='Factorial  '
        ITEM(3) ='Monte-Carlo '
        NITMS=3

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
      endif

C An uncertainty analysis technique has been chosen write to file 
C and set number of simulations.
      IUNIT=IFIL+2
      IREC=1
      IMET=IVAL(1)
      read (IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM,nzrl,izver
      write(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM,nzrl,izver,IMET
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

C Monte-Carlo. Set IMET and NSRIM.
        NRSIM=10*NIACT  ! 10 times the number of uncertainties
        call EASKI(NRSIM,' ','Required number of simulations?',
     &    70,'W',MNRS,'F',MNRS,'No. of simulations',IER,nbhelp)

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
      COMMON/UATD/UACLIM(6,4),UACTL(7,5),UAMCSA(MNACT)
      COMMON/UATD2/UAMISCTD(3)
      character message*96,phrase*12

C Reset climate change variables to zero - ONLY if first zone.
      if (IZONE.eq.1) then
        do 10 I=1,6
          UACLIM(I,1)=0.0; UACLIM(I,2)=0.0
          UACLIM(I,3)=0.0; UACLIM(I,4)=0.0 
 10     continue

C Reset control change variables to zero.
        do 15 I=1,7
          UACTL(I,1)=0.0; UACTL(I,2)=0.0
          UACTL(I,3)=0.0; UACTL(I,4)=0.0; UACTL(I,5)=0.0
 15     continue

C Reset misc vars to zero.
        do 25 I=1,3
          UAMISCTD(I)=0.
 25     continue
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
      if (IDTYPU(IACTD(IACT,1)).eq.1) then 
        call UAE01(IZONE,IACT,DIR)  ! thermophysical properties
      elseif (IDTYPU(IACTD(IACT,1)).eq.2) then 
        call UAE02(IZONE,IACT,DIR)  ! layer thickness
      elseif (IDTYPU(IACTD(IACT,1)).eq.3) then 
        call UAT03(IACT,DIR)        ! weather data
      elseif (IDTYPU(IACTD(IACT,1)).eq.4) then 
        call UAE04(IZONE,IACT,DIR)  ! casual gains or scheduled air
      elseif (IDTYPU(IACTD(IACT,1)).eq.5) then 
        call UAE05(IZONE,IACT,DIR)  ! hc coefficients
      elseif (IDTYPU(IACTD(IACT,1)).eq.101) then 
        call UAZ01(IZONE,IACT,DIR)  ! zone air volume or SNA
      elseif (IDTYPU(IACTD(IACT,1)).eq.1001) then 
        call UAT01(IACT,DIR)  ! control data
      elseif (IDTYPU(IACTD(IACT,1)).eq.1002) then 
         write (36,*) 'WARNING UNDEFINED UNCERTAINTY'

C Debug.
C         write(6,*) 'WARNING UNDEFINED UNCERTAINTY'

C        call UAT02(IACT,DIR)
      elseif (IDTYPU(IACTD(IACT,1)).eq.1004) then 
         write (36,*) 'WARNING UNDEFINED UNCERTAINTY'

C Debug.
C         write(6,*) 'WARNING UNDEFINED UNCERTAINTY'

C        call UAT04(IACT,DIR)
      elseif (IDTYPU(IACTD(IACT,1)).eq.1005) then 
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
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

      COMMON/UA5/IMET,ISIM,NRSIM
      COMMON/UATD/UACLIM(6,4),UACTL(7,5),UAMCSA(MNACT)
      COMMON/UATD2/UAMISCTD(3)

      character USETNAME*96   ! the synopsis for each MC-run
      character uheader*1000  ! header including name of each uncertainty changed
      character urunvals*1000 ! attribute changed-to value of each uncertainty change for the run.
      COMMON/USETN/USETNAME(MNRS),uheader,urunvals

      logical headerdone
      common/uhead/headerdone
      
      common/reclen/nzrl,nprl,nerl
      character RSNAME*40,outs*124,message*96,phrase*12,uhphrase*42
      integer irec,iunit

C Reset climate change variables to zero - ONLY if first zone.
      if (IZONE.eq.1) then
        do 5 I=1,6
          UACLIM(I,1)=0.0; UACLIM(I,2)=0.0; UACLIM(I,3)=0.0
          UACLIM(I,4)=0.0
 5      continue

C Reset control change variables to zero.
C UACTL (?,1)=change type (%,+/-); (?,2)=magnitude; (?,2)=associated action;
C UACTL (?,4)=time status (0 not active, 1 active)
C UACTL (?,5)=counter for the uncertainty action being dealt with
        do 15 I=1,7
          UACTL(I,1)=0.0; UACTL(I,2)=0.0
          UACTL(I,3)=0.0; UACTL(I,4)=0.0; UACTL(I,5)=0.0 
 15     continue

C Reset misc vars to zero.
        do 25 I=1,3
          UAMISCTD(I)=0.
 25     continue

C Set random deviations for all uncertainties.  Rnor returns a value 
C at random from a normal distribution with std deviation of 1.0 and 
C mean of 0.0
        do 20 I=1,NIACT
          UAMCSA(I)=rnor()
 20     continue
      endif

C Change parameters by a random amount (defined above). Clear
C strings for a synopsis of this run (up to a ~dozen actions).
      message='  '; lstm=1; lfnm=1
      phrase=' '; lphrase=1
      uheader=' '   ! clear the header
      uhphrase=' '; luhphrase=1
      urunvals=' '  ! clear string buffer for attribute changes
      headerdone=.false.  ! mark that no headers have been generated
      do 10 IACT=1,NIACT

C DIR is the direction of the alteration: +ive =up or high, -ive = down or low.
        DIR=UAMCSA(IACT)

C Remember which uncertainty is being acted upon. This is the
C combined name of the uncertainty:location. Append to the header buffer.
        write (36,*) 'Uncertainty definition ',IACT,' altered by ',DIR
        lncng=lnblnk(LCNG(IACTD(IACT,1)))
        lnloc=lnblnk(LLOC(IACTD(IACT,2)))
        if(lncng.gt.21)lncng=21
        uhphrase=' '
        write(uhphrase,'(3a)')LCNG(IACTD(IACT,1))(1:lncng),':',
     &    LLOC(IACTD(IACT,2))(1:lnloc)
        luhphrase=lnblnk(uhphrase)
        write(36,'(a)') uhphrase(1:luhphrase)

C Synopsis of specific changes to identify the run.
        write(phrase,'(a,i2.2,F5.2)') 'U',IACT,DIR
        lphrase=lnblnk(phrase)
        lfnm=lstm+lphrase
        if(lfnm.lt.96)then
          write(message(lstm:lfnm),'(2a)') phrase(1:lphrase),' '
          lstm=lfnm+1
        endif

C Depending on change type call editing routine.
        if (IDTYPU(IACTD(IACT,1)).eq.1) then 
          call UAE01(IZONE,IACT,DIR)  ! thermophysical properties
        elseif (IDTYPU(IACTD(IACT,1)).eq.2) then 
          call UAE02(IZONE,IACT,DIR)  ! layer thickness
        elseif (IDTYPU(IACTD(IACT,1)).eq.3) then 
          call UAT03(IACT,DIR)        ! weather data
        elseif (IDTYPU(IACTD(IACT,1)).eq.4) then 
          call UAE04(IZONE,IACT,DIR)  ! casual gains or scheduled air
        elseif (IDTYPU(IACTD(IACT,1)).eq.5) then 
          call UAE05(IZONE,IACT,DIR)  ! hc coefficients
        elseif (IDTYPU(IACTD(IACT,1)).eq.101) then 
          call UAZ01(IZONE,IACT,DIR)  ! zone air volume or SNA
        elseif (IDTYPU(IACTD(IACT,1)).eq.1001) then 
          call UAT01(IACT,DIR)  ! control data
        elseif (IDTYPU(IACTD(IACT,1)).eq.1002) then 
         write (36,*) 'WARNING UNDEFINED UNCERTAINTY'

C Debug.
c         write(6,*) 'WARNING UNDEFINED UNCERTAINTY'

C          call UAT02(IACT,DIR)
        elseif (IDTYPU(IACTD(IACT,1)).eq.1004) then 
         write (36,*) 'WARNING UNDEFINED UNCERTAINTY'

C Debug.
C         write(6,*) 'WARNING UNDEFINED UNCERTAINTY'

C          call UAT04(IACT,DIR)
        elseif (IDTYPU(IACTD(IACT,1)).eq.1005) then 
         write (36,*) 'WARNING UNDEFINED UNCERTAINTY'

C Debug.
C         write(6,*) 'WARNING UNDEFINED UNCERTAINTY'

C          call UAT05(IACT,DIR)
        endif
 10   continue
      write(36,*) 'synopsis ',message(1:lnblnk(message))

C After the NIACT loop the header should be filled so write it out.
C Bother uheader and urnubals will be blank for the case of weather
C data as information is written each day rather than once at the
C start of the run.
C TODO do not need to write the header for the subsequent result sets.
C TODO think of how to do this....
      write(37,*)'**',uheader(1:lnblnk(uheader))
      write(37,*)'**',urunvals(1:lnblnk(urunvals))
C      headerdone=.true.

C See what the current position of IREC is and update RSNAME to the
C synopsis of changes recorded above.
      IUNIT=IFIL+2
      IREC=1
      read (IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM,nzrl,izver,IMET
      WRITE(6,*) 'checking ',NSIM,nzrl,izver,IMET

C Remember the synopsis for this run.
C TODO consider only writing header for NSIM .le. 1 ??
      write(USETNAME(NSIM),'(a)') message(1:lnblnk(message))
      if(NSIM.gt.1)then  ! leave base case RSNAME as is.
        IREC=NSIM+1  ! slightly different from MZLS2
        read(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NST,IRECPL,RSNAME
        WRITE(6,*) 'previous ',NST,IRECPL,RSNAME
        write(RSNAME,'(a)') message(1:40)
        write(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NST,IRECPL,RSNAME
        WRITE(6,*) 'updated ',NST,IRECPL,RSNAME
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
      COMMON/UATD/UACLIM(6,4),UACTL(7,5),UAMCSA(MNACT)
      COMMON/UAFCT/IADIR(MNFA),NFA
      character message*96,phrase*12

C Reset climate change variables to zero - ONLY if first zone.
      if (IZONE.eq.1) then
        do 10 I=1,6
          UACLIM(I,1)=0.0; UACLIM(I,2)=0.0; UACLIM(I,3)=0.0
          UACLIM(I,4)=0.0
 10     continue

C Reset control change variables to zero.
        do 15 I=1,7
          UACTL(I,1)=0.0; UACTL(I,2)=0.0
          UACTL(I,3)=0.0; UACTL(I,4)=0.0; UACTL(I,5)=0.0
 15     continue
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
        if (IDTYPU(IACTD(IACT,1)).eq.1) then 
          call UAE01(IZONE,IACT,DIR)  ! thermophysical properties
        elseif (IDTYPU(IACTD(IACT,1)).eq.2) then 
          call UAE02(IZONE,IACT,DIR)  ! layer thickness
        elseif (IDTYPU(IACTD(IACT,1)).eq.3) then 
          call UAT03(IACT,DIR)        ! weather data
        elseif (IDTYPU(IACTD(IACT,1)).eq.4) then 
          call UAE04(IZONE,IACT,DIR)  ! casual gains or scheduled air
        elseif (IDTYPU(IACTD(IACT,1)).eq.5) then 
          call UAE05(IZONE,IACT,DIR)  ! hc coefficients
        elseif (IDTYPU(IACTD(IACT,1)).eq.101) then 
          call UAZ01(IZONE,IACT,DIR)  ! zone air volume or SNA
        elseif (IDTYPU(IACTD(IACT,1)).eq.1001) then 
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

C      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)
      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)

      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)

C      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),AE(MCOM,MS)

      character USETNAME*96   ! the synopsis for each MC-run
      character uheader*1000  ! header including name of each uncertainty changed
      character urunvals*1000 ! attribute changed-to value of each uncertainty change for the run.
      COMMON/USETN/USETNAME(MNRS),uheader,urunvals

      logical headerdone
      common/uhead/headerdone

      integer lstur, lfnur
      character strval*12,uhphrase*42
      CHARACTER CSTR*25

      DIMENSION IEL(ME)

C Data stored in UA01 has the following meaning in this routine:
C IDMAT(?,1): Materials array index.
C IDMAT(?,2): Thermophysical property (con; den; sp ht).
C IDMAT(?,3): Change type (%; +/-; explicit).
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
           call CHKPRIM(IZ,IS,IDMAT(ICC,1),NEL,IEL)
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

C Make specific header string for each change (to match urunvals)
               lncng=lnblnk(LCNG(ICC))
               lnloc=lnblnk(LLOC(ICL))
               if(lncng.gt.18)lncng=18
               uhphrase=' '
               write(uhphrase,'(4a,i2.2,a,i2.2)')LCNG(ICC)(1:lncng),':',
     &           LLOC(ICL)(1:lnloc),':',IZ,':',IS
               luhphrase=lnblnk(uhphrase)
               write(36,'(a)') uhphrase
      
               lfnuh=lstuh+luhphrase
               if(lfnuh.lt.1000)then
                 write(uheader(lstuh:lfnuh),'(a)')uhphrase(1:luhphrase)
                 lstuh=lfnuh+2
               endif
               strval=' '        ! clear string buffer for the value changed

C Edit data.
               if (IDMAT(ICC,2).eq.1) then
                 call UAEDIT(CON(IS,IE),IDMAT(ICC,3),DATU(ICC,1),
     &                       DATU(ICC,2),DIR,-1,0.01,0.0)
                 call relstr(CON(IS,IE),strval,lna,ier)
               elseif (IDMAT(ICC,2).eq.2) then
                 call UAEDIT(DEN(IS,IE),IDMAT(ICC,3),DATU(ICC,1),
     &                       DATU(ICC,2),DIR,0,0.01,9000.0)
                 call relstr(DEN(IS,IE),strval,lna,ier)
               elseif (IDMAT(ICC,2).eq.3) then
                 call UAEDIT(SHT(IS,IE),IDMAT(ICC,3),DATU(ICC,1),
     &                       DATU(ICC,2),DIR,0,0.01,3000.0)
                 call relstr(SHT(IS,IE),strval,lna,ier)
               elseif (IDMAT(ICC,2).eq.4) then
                 call UAEDIT(EMISE(IS),IDMAT(ICC,3),DATU(ICC,1),
     &                       DATU(ICC,2),DIR,0,0.001,0.999)
                 call UAEDIT(EMISI(IS),IDMAT(ICC,3),DATU(ICC,1),
     &                       DATU(ICC,2),DIR,0,0.001,0.999)
                 call relstr(EMISE(IS),strval,lna,ier)
               elseif (IDMAT(ICC,2).eq.5) then
                 call UAEDIT(ABSE(IS),IDMAT(ICC,3),DATU(ICC,1),
     &                       DATU(ICC,2),DIR,0,0.001,0.999)
                 call UAEDIT(ABSE(IS),IDMAT(ICC,3),DATU(ICC,1),
     &                       DATU(ICC,2),DIR,0,0.001,0.999)
                 call relstr(ABSE(IS),strval,lna,ier)
               endif

               write(36,*) 'to:'
               write(36,'(3f9.3,4f5.2)')CON(IS,IE),DEN(IS,IE),
     &               SHT(IS,IE),EMISI(IS), EMISE(IS),ABSI(IS),ABSE(IS)

C Add to urunvals
               lstrval=lnblnk(strval)
               lfnur=lstur+lstrval
               if(lfnur.lt.1000)then
                 write(urunvals(lstur:lfnur),'(a)') strval(1:lstrval)
                 lstur=lfnur+2
               endif

C Now copy the altered data into the zonal commons.
               if (IDMAT(ICC,2).eq.1) then
                 THRMLI(IZ,IS,IE,1)=CON(IS,IE)
               elseif (IDMAT(ICC,2).eq.2) then
                 THRMLI(IZ,IS,IE,2)=DEN(IS,IE)
               elseif (IDMAT(ICC,2).eq.3) then
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
                 call CHKPRIM(IZONE,ISURF,IDMAT(ICC,1),NEL,IEL)
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
               if (IDMAT(ICC,2).eq.1) then
                 call UAEDIT(CON(IS,IE),IDMAT(ICC,3),DATU(ICC,1),
     &             DATU(ICC,2),DIR,-1,0.01,0.0)
               elseif (IDMAT(ICC,2).eq.2) then
                 call UAEDIT(DEN(IS,IE),IDMAT(ICC,3),DATU(ICC,1),
     &             DATU(ICC,2),DIR,0,0.01,9000.0)
               elseif (IDMAT(ICC,2).eq.3) then
                 call UAEDIT(SHT(IS,IE),IDMAT(ICC,3),DATU(ICC,1),
     &             DATU(ICC,2),DIR,0,0.01,3000.0)
               elseif (IDMAT(ICC,2).eq.4) then
                 call UAEDIT(EMISE(IS),IDMAT(ICC,3),DATU(ICC,1),
     &             DATU(ICC,2),DIR,0,0.001,0.999)
                 call UAEDIT(EMISI(IS),IDMAT(ICC,3),DATU(ICC,1),
     &             DATU(ICC,2),DIR,0,0.001,0.999)
               elseif (IDMAT(ICC,2).eq.5) then
                 call UAEDIT(ABSE(IS),IDMAT(ICC,3),DATU(ICC,1),
     &             DATU(ICC,2),DIR,0,0.001,0.999)
                 call UAEDIT(ABSE(IS),IDMAT(ICC,3),DATU(ICC,1),
     &             DATU(ICC,2),DIR,0,0.001,0.999)
               endif

               write(36,*) 'to:'
               write(36,'(3f9.3,4f5.2)')CON(IS,IE),DEN(IS,IE),
     &               SHT(IS,IE),EMISI(IS), EMISE(IS),ABSI(IS),ABSE(IS)

C Now copy the altered data into the zonal commons.
               if (IDMAT(ICC,2).eq.1) then
                 THRMLI(IZ,IS,IE,1)=CON(IS,IE)
               elseif (IDMAT(ICC,2).eq.2) then
                 THRMLI(IZ,IS,IE,2)=DEN(IS,IE)
               elseif (IDMAT(ICC,2).eq.3) then
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

C If this was the last uncertainty i.e. if IACT=NIACT header is done.
      if(IACT.eq.NIACT)then
        write(36,*) 'set header buffer done in UAE01'
        headerdone=.true.
      endif
      RETURN
      END

C ******************** UAE02 ********************
C UAE02 edits layer thicknesses in memory. The editing of whole MLC's 
C will also be added.
C Naming convention:
C IZONE, ISURF: focus zone and surface (the data files for this zone 
C               have just been rescaned). 
C IZ, IS: current zone and surface being checked (these are listed in 
C               the common/ua2/ arrays).

      SUBROUTINE UAE02(IZONE,IACT,DIR)
#include "building.h"
#include "geometry.h"
C Common blocks UA2 UA3 UA4 within uncertainty.h
#include "uncertainty.h"
#include "esprdbfile.h"
#include "material.h"

      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)

      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)

      COMMON/UA31/MATNAM(MNCNG,2)

      character USETNAME*96   ! the synopsis for each MC-run
      character uheader*1000  ! header including name of each uncertainty changed
      character urunvals*1000 ! attribute changed-to value of each uncertainty change for the run.
      COMMON/USETN/USETNAME(MNRS),uheader,urunvals

      logical headerdone
      common/uhead/headerdone

C      CHARACTER TITL*72,NAM*72,CSTR*25
      CHARACTER MATNAM*32,CSTR*25

      integer lstur, lfnur
      character strval*12,uhphrase*42

C Data stored in UA02 has the following meaning in this routine:
C IDMAT(?,1): The material in the chosen MLC, if this is 0 then 
C              a new MLC reference will be in MATNAM(?,2).
C IDMAT(?,2): Unused.
C IDMAT(?,3): Change type (%; +/-; explicit).
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
         if (SMLCN(IS)(1:12).eq.MATNAM(ICC,1)(1:12)) then

C This construction has uncertainties defined, but which MLC database 
C entry is it? C Find matching MLC.
          call matchmlcdesc(SMLCN(is),imlc)
          do 30 IE=1,NE(IS)
C           write (36,*) 'iprmat(imlc,ie): ',IPRMAT(IMLC,IE),imlc,ie
C           write (36,*) 'IDMAT(ICC,1): ',IDMAT(ICC,1),icc
           if (IPRMAT(IMLC,IE).eq.IDMAT(ICC,1)) then

C Make specific header string for each change (to match urunvals)
            lncng=lnblnk(LCNG(ICC))
            lnloc=lnblnk(LLOC(ICL))
            if(lncng.gt.18)lncng=18
            uhphrase=' '
            write(uhphrase,'(4a,i2.2,a,i2.2)')LCNG(ICC)(1:lncng),':',
     &        LLOC(ICL)(1:lnloc),':',IZ,':',IS
            luhphrase=lnblnk(uhphrase)
            write(36,'(a)') uhphrase
      
            lfnuh=lstuh+luhphrase
            if(lfnuh.lt.1000)then
              write(uheader(lstuh:lfnuh),'(a)') uhphrase(1:luhphrase)
              lstuh=lfnuh+2
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
             if (IDMAT(ICC,3).eq.1) then

C Percentage change. Ensure it does not go below 1mm. 
               THRMLI(IZ,IS,IE,4)=THRMLI(IZ,IS,IE,4)*
     &                            (((DATU(ICC,1)/100.)*DIR)+1.0)
               if(THRMLI(IZ,IS,IE,4).lt.0.001)THRMLI(IZ,IS,IE,4)=0.001
               call relstr(THRMLI(IZ,IS,IE,4),strval,lna,ier)
             elseif (IDMAT(ICC,3).eq.2) then

C Absolute change. Ensure it does not go below 1mm. 
               THRMLI(IZ,IS,IE,4)=THRMLI(IZ,IS,IE,4)+
     &                            (DATU(ICC,1)*DIR)
               if(THRMLI(IZ,IS,IE,4).lt.0.001)THRMLI(IZ,IS,IE,4)=0.001
               call relstr(THRMLI(IZ,IS,IE,4),strval,lna,ier)
             elseif (IDMAT(ICC,3).eq.3) then

C Explicit change. Ensure it does not go below 1mm.
              if (DIR.gt.0.) then
                THRMLI(IZ,IS,IE,4)=THRMLI(IZ,IS,IE,4)+
     &                             (DATU(ICC,1)-THRMLI(IZ,IS,IE,4))*DIR
              else
                THRMLI(IZ,IS,IE,4)=THRMLI(IZ,IS,IE,4)-
     &                             (DATU(ICC,2)-THRMLI(IZ,IS,IE,4))*DIR
                if(THRMLI(IZ,IS,IE,4).lt.0.001)THRMLI(IZ,IS,IE,4)=0.001
              endif
              call relstr(THRMLI(IZ,IS,IE,4),strval,lna,ier)
             endif
             write (36,'(a)') 'to:'
             write (36,'(a,f9.4)') 'THRMLI: ',THRMLI(IZ,IS,IE,4)

C Add to urunvals
             lstrval=lnblnk(strval)
             lfnur=lstur+lstrval
             if(lfnur.lt.1000)then
               write(urunvals(lstur:lfnur),'(a)') strval(1:lstrval)
               lstur=lfnur+2
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
            if (SMLCN(ISURF)(1:12).eq.MATNAM(ICC,1)(1:12))then

C Find matching MLC.
             call matchmlcdesc(SMLCN(isurf),imlc)
             do 50 IE=1,NE(ISURF)
              write (36,*) 'iprmat(imlc,ie): ',IPRMAT(IMLC,IE),imlc,ie
              write (36,*) 'IDMAT(ICC,1): ',IDMAT(ICC,1),icc
              if (IPRMAT(IMLC,IE).eq.IDMAT(ICC,1)) then

C Either edit layer thickness ( not yet implemented MLC edit).
C Note the next few lines are not indented wrt the 'if' statement above.
               if (IE.eq.0) then
                write (36,*) 'MLC editing not enabled.'
               else
                write (36,*) 'Editing adjacent element thickness..'
                write (36,*) 'Zone:',IZONE,' Surface:',ISURF,
     &                                                 ' Element:',IE
                write (36,*) 'THRMLI: ',THRMLI(IZONE,ISURF,IE,4)
                if (IDMAT(ICC,3).eq.1) then

C Percentage change. Ensure it does not go below 1mm. 
                 THRMLI(IZONE,ISURF,IE,4)=THRMLI(IZONE,ISURF,IE,4)*
     &                                   (((DATU(ICC,1)/100.)*DIR)+1.0)
                 if(THRMLI(IZONE,ISURF,IE,4).lt.0.001)then
                   THRMLI(IZONE,ISURF,IE,4)=0.001
                 endif
                elseif (IDMAT(ICC,3).eq.2) then

C Absolute change. Ensure it does not go below 1mm. 
                 THRMLI(IZONE,ISURF,IE,4)=THRMLI(IZONE,ISURF,IE,4)+
     &                                   (DATU(ICC,1)*DIR)
                 if(THRMLI(IZONE,ISURF,IE,4).lt.0.001)then
                   THRMLI(IZONE,ISURF,IE,4)=0.001
                 endif
                elseif (IDMAT(ICC,3).eq.3) then

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

C If this was the last uncertainty i.e. if IACT=NIACT header is done.
      if(IACT.eq.NIACT)then
        write(36,*) 'set header buffer done in UAE02'
        headerdone=.true.
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
      common/uhead/headerdone

      integer lstur, lfnur
      character strval*12,uhphrase*42

C Data stored in UA04 has the following meaning in this routine:
C IDMAT(?,1): Casual gain type(1/2/3)/ scheduled air flow(0)
C IDMAT(?,2): Cas Gn: sensible(1)/ latent(2)/ radiant(3)/ convective(4)/
C IDMAT(?,2):         total Q (same split) (5)/ Period start (6)/
C IDMAT(?,2):         N/A (7)/ 
C IDMAT(?,2):         same total frac diff split (8)
C IDMAT(?,2): Sched Air flow: infiltration(1)/ ventilation(2)/
C IDMAT(?,2):         N/A (3)/ vent source temp(4)
C IDMAT(?,3): Change type (%; +/-; explicit).
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

C Define max number of periods for common P2 (flow) and P3 (Cas gns).
      IMP2=NAC(1)
      IMP3=NCAS(1)
      DO 400 IDTY=1,NBDAYTYPE
        IF(NAC(IDTY).GT.IMP2)IMP2=NAC(IDTY)
        if (NCAS(IDTY).gt.IMP3) IMP3=NCAS(IDTY)
 400  CONTINUE

      write (36,*) '   '
      write (36,*) 'Entering subroutine UAE04'
      write (36,*) 'Focus zone: ',IZONE

C Loop through all zones listed in NZNOGU for the defined location ICL.
      do 10 II=1,NZGU(ICL)
        IZ=NZNOGU(ICL,II)
        if (IZ.eq.IZONE) then

C Edit casual gains.
          if (IDMAT(ICC,1).eq.0) then

C Edit scheduled air flows.
C Check that we have the right element(period). << TO DO >>
C Currently changing all defined periods.
            write (36,*) 'Editing scheduled air flow'
            ICF=IDMAT(ICC,3)
            DAT1=DATU(ICC,1)
            DAT2=DATU(ICC,2)

C Make specific header string for each change (to match urunvals)
            lncng=lnblnk(LCNG(ICC))
            lnloc=lnblnk(LLOC(ICL))
            if(lncng.gt.18)lncng=18
            uhphrase=' '
            write(uhphrase,'(4a,i2.2)')LCNG(ICC)(1:lncng),':',
     &        LLOC(ICL)(1:lnloc),':',IZ
            luhphrase=lnblnk(uhphrase)
            write(36,'(a)') uhphrase
      
            lfnuh=lstuh+luhphrase
            if(lfnuh.lt.1000)then
              write(uheader(lstuh:lfnuh),'(a)') uhphrase(1:luhphrase)
              lstuh=lfnuh+2
            endif
            strval=' '        ! clear string buffer for the value changed

C Percentage change. 
C What data type to change? (see comments at start of routine).
C Chance all instances of this type - will probably add data 
C to specify individual periods later.
            if (IDMAT(ICC,2).eq.1) then
              write (36,*) 'Zone:',IZONE,' infiltration'
              do 100 IX=1,IMP2
                DO 4010 IDTY=1,NBDAYTYPE
                  T=ACI(IDTY,IX)
                  call UAEDIT(ACI(IDTY,IX),ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
                  write(36,*)'Period ',CALENTAG(IDTY),IX,' From:',T,
     &            ' to ',ACI(IDTY,IX)
                call relstr(ACI(IDTY,IX),strval,lna,ier)
 4010           CONTINUE
 100          continue
            elseif (IDMAT(ICC,2).eq.2) then
              write (36,*) 'Zone:',IZONE,' ventilation'
              do 101 IX=1,IMP2
                DO 4011 IDTY=1,NBDAYTYPE
                  T=ACV(IDTY,IX)
                  call UAEDIT(ACV(IDTY,IX),ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
                  write(36,*)'Period ',CALENTAG(IDTY),IX,' From:',T,
     &            ' to ',ACV(IDTY,IX)
                call relstr(ACV(IDTY,IX),strval,lna,ier)
 4011           CONTINUE
 101          continue
            elseif (IDMAT(ICC,2).eq.3) then
              write (36,*)'Illegal option, cannot change zone index'
            elseif (IDMAT(ICC,2).eq.4) then
              write (36,*) 'Zone:',IZONE,' vent source temperature'
              do 104 IX=1,IMP2
                DO 4012 IDTY=1,NBDAYTYPE
                  T=TA(IDTY,IX)
                  call UAEDIT(TA(IDTY,IX),ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
                  write(36,*)'Period ',CALENTAG(IDTY),IX,' From:',T,
     &            ' to ',TA(IDTY,IX)
                call relstr(TA(IDTY,IX),strval,lna,ier)
 4012           CONTINUE
 104          continue
            endif

C Add to urunvals
            lstrval=lnblnk(strval)
            lfnur=lstur+lstrval
            if(lfnur.lt.1000)then
              write(urunvals(lstur:lfnur),'(a)') strval(1:lstrval)
              lstur=lfnur+2
            endif
    
          else

C Edit casual gains.
C Check that we have the right element(period). << TO DO >>
C Currently changing all defined periods.
            write (36,*) 'Editing casual gains'
            ICF=IDMAT(ICC,3)
            DAT1=DATU(ICC,1)
            DAT2=DATU(ICC,2)
            IT=IDMAT(ICC,1)

C Make specific header string for each change (to match urunvals)
            lncng=lnblnk(LCNG(ICC))
            lnloc=lnblnk(LLOC(ICL))
            if(lncng.gt.18)lncng=18
            uhphrase=' '
            write(uhphrase,'(4a,i2.2)')LCNG(ICC)(1:lncng),':',
     &        LLOC(ICL)(1:lnloc),':',IZ
            luhphrase=lnblnk(uhphrase)
            write(36,'(a)') uhphrase
      
            lfnuh=lstuh+luhphrase
            if(lfnuh.lt.1000)then
              write(uheader(lstuh:lfnuh),'(a)') uhphrase(1:luhphrase)
              lstuh=lfnuh+2
            endif
            strval=' '        ! clear string buffer for the value changed

C What data type to change? (see comments at start of routine).
C Chance all instances of this type - will probably add data 
C to specify incividual periods later.
C
C The next section has the following format:
C  Check casual gain parameter to edit (sensible, latent, convect %...
C  Check if required casual gain type is defined for this period (occ, lights..
C    note weekdays/ sat/ sun checked seperately.
C
            if (IDMAT(ICC,2).eq.1) then
              write (36,*) 'Zone:',IZONE,' type:',IT,' sensible'
              do IX=1,IMP3
                do IDTY=1,NBDAYTYPE
                  if (IDMAT(ICC,1).eq.ICGT(IDTY,IX)) then
                    T1=CMGS(IDTY,IX)
                    call UAEDIT(CMGS(IDTY,IX),ICF,DAT1,DAT2,DIR,
     &                -1,0.0,0.0)
                    write (36,*)'Period ',CALENTAG(IDTY),IX,
     &                ' From:',T1,' to ',CMGS(IDTY,IX)
                    call relstr(CMGS(IDTY,IX),strval,lna,ier)
                  endif
                enddo
              enddo
            elseif (IDMAT(ICC,2).eq.2) then
              write (36,*) 'Zone:',IZONE,' type:',IT,' latent'
              do IX=1,IMP3
                do IDTY=1,NBDAYTYPE
                  if (IDMAT(ICC,1).eq.ICGT(IDTY,IX)) then
                    T1=CMGL(IDTY,IX)
                    call UAEDIT(CMGL(IDTY,IX),ICF,DAT1,DAT2,DIR,
     &                -1,0.0,0.0)
                    write (36,*)'Period ',CALENTAG(IDTY),IX,
     &                ' From:',T1,' to ',CMGL(IDTY,IX)
                    call relstr(CMGL(IDTY,IX),strval,lna,ier)
                  endif
                enddo
              enddo
            elseif (IDMAT(ICC,2).eq.3) then
              write (36,*) 'Zone:',IZONE,' type:',IT,' rad fraction'
              do IX=1,IMP3
                do IDTY=1,NBDAYTYPE
                  if (IDMAT(ICC,1).eq.ICGT(IDTY,IX)) then
                    T1=RADC(IDTY,IX)
                    call UAEDIT(RADC(IDTY,IX),ICF,DAT1,DAT2,DIR,
     &                0,0.0,1.0)
                    write (36,*)'Period ',CALENTAG(IDTY),IX,
     &                ' From:',T1,' to ',RADC(IDTY,IX)
                    CONC(IDTY,IX)=1.0-RADC(IDTY,IX)  ! adjust CONC
                    call relstr(RADC(IDTY,IX),strval,lna,ier)
                  endif
                enddo
              enddo
            elseif (IDMAT(ICC,2).eq.4) then
              write (36,*)'Zone:',IZONE,' type:',IT,' conv fraction'
              do IX=1,IMP3
                do IDTY=1,NBDAYTYPE
                  if (IDMAT(ICC,1).eq.ICGT(IDTY,IX)) then
                    T1=CONC(IDTY,IX)
                    call UAEDIT(CONC(IDTY,IX),ICF,DAT1,DAT2,DIR,
     &                0,0.0,1.0)
                    write (36,*)'Period ',CALENTAG(IDTY),IX,
     &                ' From:',T1,' to ',CONC(IDTY,IX)
                    RADC(IDTY,IX)=1.0-CONC(IDTY,IX)  ! adjust RADC
                    call relstr(CONC(IDTY,IX),strval,lna,ier)
                  endif
                enddo
              enddo
            elseif (IDMAT(ICC,2).eq.5) then
              write (36,*) 'Zone:',IZONE,' type:',IT,' total Q'
              do IX=1,IMP3
                do IDTY=1,NBDAYTYPE
                  if (IDMAT(ICC,1).eq.ICGT(IDTY,IX)) then
                    T1=CMGS(IDTY,IX)
                    T2=CMGL(IDTY,IX)
                    T3=T1+T2
                    call UAEDIT(T3,ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
                    CMGS(IDTY,IX)=T3*(CMGS(IDTY,IX)/(T1+T2))
                    CMGL(IDTY,IX)=T3*(CMGL(IDTY,IX)/(T1+T2))
                    write (36,*)'Period ',CALENTAG(IDTY),IX,
     &                ' Sensible from:',T1,' to ',CMGS(IDTY,IX)
                    write (36,*)'Period ',CALENTAG(IDTY),IX,
     &                ' Latent from:',T2,' to ',CMGL(IDTY,IX)
                    call relstr(CMGS(IDTY,IX),strval,lna,ier)
                  endif
                enddo
              enddo
            elseif (IDMAT(ICC,2).eq.6) then

C Shift the start time of subsequent periods.
              write (36,*) 'Zone:',IZONE,' type:',IT,' start'
              do IX=1,IMP3
                do IDTY=1,NBDAYTYPE
                  if (IDMAT(ICC,1).eq.ICGT(IDTY,IX)) then
                    if(IX.gt.1)then  ! no need to do it for initial period
                      IT1=ICGS(IDTY,IX)
                      if (DIR.gt.0.) then
                        if(ICGS(IDTY,IX)+1.lt.ICGS(IDTY,IX+1))then
                          ICGS(IDTY,IX)=ICGS(IDTY,IX)+1      ! if clear to shift later
                          ICGF(IDTY,IX-1)=ICGS(IDTY,IX-1)+1  ! and shift prior fn
                        endif
                      else
                        if(ICGS(IDTY,IX)-1.gt.ICGS(IDTY,IX-1))then
                          ICGS(IDTY,IX)=ICGS(IDTY,IX)-1      ! if clear to shift earlier
                          ICGF(IDTY,IX-1)=ICGS(IDTY,IX-1)-1  ! and shift prior fn
                        endif
                      endif
                      write (36,*)'Period ',CALENTAG(IDTY),IX,
     &                  ' From:',IT1,' to ',ICGS(IDTY,IX)
                      call relstr(real(ICGS(IDTY,IX)),strval,lna,ier)
                    endif
                  endif
                enddo
              enddo
            elseif (IDMAT(ICC,2).eq.8) then
              IT=IDMAT(ICC,1)
              write(36,*)'Zone:',IZONE,' type:',IT,' rad/conv split'
              do IX=1,IMP3
                do IDTY=1,NBDAYTYPE
                  if (IDMAT(ICC,1).eq.ICGT(IDTY,IX)) then
                    T1=RADC(IDTY,IX)
                    T2=CONC(IDTY,IX)
                    T3=T1+T2
                    call UAEDIT(CONC(IDTY,IX),ICF,DAT1,DAT2,DIR,
     &                0,0.0,T3)
                    RADC(IDTY,IX)=T3-CONC(IDTY,IX)
                    write (36,*)'Period ',CALENTAG(IDTY),IX,
     &                ' Rad fraction from:',T1,' to ',RADC(IDTY,IX)
                    write (36,*)'Period ',CALENTAG(IDTY),IX,
     &                ' Conv fraction from:',T2,' to ',CONC(IDTY,IX)
                    call relstr(RADC(IDTY,IX),strval,lna,ier)
                  endif
                enddo
              enddo
            else
              write (36,*)'Editing request unknown.'
            endif

C Add to urunvals
            lstrval=lnblnk(strval)
            lfnur=lstur+lstrval
            if(lfnur.lt.1000)then
              write(urunvals(lstur:lfnur),'(a)') strval(1:lstrval)
              lstur=lfnur+2
            endif
          endif
        endif
 10   continue

C If this was the last uncertainty i.e. if IACT=NIACT header is done.
      if(IACT.eq.NIACT)then
        write(36,*) 'set header buffer done in UAE04'
        headerdone=.true.
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
C IDMAT(?,1): =1 just now - associated data is convection corr to use
C IDMAT(?,2): =1 just now - associated data is convection corr to use
C IDMAT(?,3): 4 - alternative correlation id number
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

C ******************** UAZ01 ********************
C UAZ01 edits zone air volume in memory or surface area.

      SUBROUTINE UAZ01(IZONE,IACT,DIR)
#include "building.h"
#include "geometry.h"
C Common blocks UA2 UA3 UA4 within uncertainty.h
#include "uncertainty.h"
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      COMMON/PREC1/SNA(MCOM,MS)
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
      common/uhead/headerdone

      integer lstur, lfnur
      character strval*12,uhphrase*42

C Data stored in UA03 has the following meaning in this routine:
C IDMAT(?,1): 1=air, 2=surface area
C IDMAT(?,2): 1=volume, ...
C IDMAT(?,3): Change type (%; +/-; explicit).
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
          if (IDMAT(ICC,1).eq.1) then
            write (36,*) 'Editing zone volume..'
            ICF=IDMAT(ICC,3)
            DAT1=DATU(ICC,1)
            DAT2=DATU(ICC,2)

C Make specific header string for each change (to match urunvals)
            lncng=lnblnk(LCNG(ICC))
            lnloc=lnblnk(LLOC(ICL))
            if(lncng.gt.18)lncng=18
            uhphrase=' '
            write(uhphrase,'(4a,i2.2,a,i2.2)')LCNG(ICC)(1:lncng),':',
     &        LLOC(ICL)(1:lnloc),':',IZ,':',IS
            luhphrase=lnblnk(uhphrase)
            write(36,'(a)') uhphrase
      
            lfnuh=lstuh+luhphrase
            if(lfnuh.lt.1000)then
              write(uheader(lstuh:lfnuh),'(a)') uhphrase(1:luhphrase)
              lstuh=lfnuh+2
            endif
            strval=' '        ! clear string buffer for the value changed
            if (IDMAT(ICC,2).eq.1) then 
              write (36,*) 'Zone: ',IZ,' Volume: ',VOL(IZ)
              call UAEDIT(VOL(IZ),ICF,DAT1,DAT2,DIR,-1,-1.0,0.0)

C Check for out of range values.
              if (VOL(IZ).lt.0.01) then
                VOL(IZ)=1.0
                write (36,*) 'Zone volume less than 0.0, setting to 1.0'
              endif
              write (36,*)'to:                     ',VOL(IZ)
              call relstr(VOL(IZ),strval,lna,ier)
            endif
          elseif (IDMAT(ICC,1).eq.2) then
            write (36,*) 'Editing surface area...'
            ICF=IDMAT(ICC,3)
            DAT1=DATU(ICC,1)
            DAT2=DATU(ICC,2)
            do JJ=1,NSGU(ICL,II)
              IS=NSNOG(ICL,II,JJ)

C Make specific header string for each change (to match urunvals)
              lncng=lnblnk(LCNG(ICC))
              lnloc=lnblnk(LLOC(ICL))
              if(lncng.gt.18)lncng=18
              uhphrase=' '
              write(uhphrase,'(4a,i2.2,a,i2.2)')LCNG(ICC)(1:lncng),':',
     &          LLOC(ICL)(1:lnloc),':',IZ,':',IS
              luhphrase=lnblnk(uhphrase)
              write(36,'(a)') uhphrase
      
              lfnuh=lstuh+luhphrase
              if(lfnuh.lt.1000)then
                write(uheader(lstuh:lfnuh),'(a)') uhphrase(1:luhphrase)
                lstuh=lfnuh+2
              endif
              strval=' '        ! clear string buffer for the value changed
              write (36,'(a,f9.4,2i3)') 'From Area: ',SNA(IZ,IS),IZ,IS
              if (IDMAT(ICC,2).eq.1) then 
                SNA(IZ,IS)=SNA(IZ,IS)*(((DATU(ICC,1)/100.)*DIR)+1.0)
                if(SNA(IZ,IS).lt.0.01) SNA(IZ,IS)=0.01
              elseif (IDMAT(ICC,3).eq.2) then
                SNA(IZ,IS)=SNA(IZ,IS)+(DATU(ICC,1)*DIR)
                if(SNA(IZ,IS).lt.0.01) SNA(IZ,IS)=0.01
              endif
              write (36,'(a,f9.4,2i3)') 'To Area: ',SNA(IZ,IS),IZ,IS
              call relstr(SNA(IZ,IS),strval,lna,ier)
            enddo
            write (36,*) 'sna= ',(SNA(IZ,JS),JS=1,nconst(IZ))

C Update emissivity area weighting to reflect change in SNA
            emarea(IZ)=0.0
            do kk=1,nconst(IZ)
              emarea(IZ)=emarea(IZ)+sna(IZ,kk)*ei(IZ,kk)
            enddo

C Add to urunvals
            lstrval=lnblnk(strval)
            lfnur=lstur+lstrval
            if(lfnur.lt.1000)then
              write(urunvals(lstur:lfnur),'(a)') strval(1:lstrval)
              lstur=lfnur+2
            endif
          endif
        endif
 10   continue

C If this was the last uncertainty i.e. if IACT=NIACT header is done.
      if(IACT.eq.NIACT)then
        write(36,*) 'set header buffer done in UAZ01'
        headerdone=.true.
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
      call matchmlcdesc(SMLCN(isurf),i)
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


      if (ICFLAG.eq.1) then

C Percentage change. 
        VAL=VAL*(((UPval/100.)*DIR)+1.0)
      elseif (ICFLAG.eq.2) then

C Absolute change. 
        VAL=VAL+(UPval*DIR)
      elseif (ICFLAG.eq.3) then

C Explicit change. 
        if (DIR.gt.0.) then
          VAL=VAL+((UPval-VAL)*DIR/3.)
        else
          VAL=VAL-((DOWNval-VAL)*DIR/3.)
        endif
      endif
      
C Check limits.
      if (ICHK.le.0) then
        VAL=max(VAL,vmin)
      endif
      if (ICHK.ge.0) then
        VAL=min(VAL,vmax)
      endif
      

      RETURN
      END
