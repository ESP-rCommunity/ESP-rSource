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
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)

      COMMON/UA4/NIACT,IACTD(MNACT,2)
      COMMON/UA5/IMET,ISIM,NRSIM
      COMMON/UAMET/IUNCMET
      COMMON/UAFCT/IADIR(MNFA),NFA

      character ITEM(3)*18,H*72,outs*124

      DIMENSION IVAL(3)

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

        H(1)='Pick the required analysis method.  The only available '
        H(2)='option is Differential sensitivity analysis at present.'
        H(3)='For further details on the method see the final report '
        H(4)='of the PASSYS project. '
        INPIC=1
        CALL EPICKS(INPIC,IVAL,' ',' Which analysis type : ',
     &                        18,NITMS,ITEM,'Analysis methods',IERR,4)

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
        NRSIM=80
        H(1)='A Monte-Carlo analysis usually requires at least 70 '
        H(2)='simulations and usually no more than 100. '
        H(3)='You may have to re-compile so as the result library '
        H(4)='can accomodate all the result sets. '
        call EASKI(NRSIM,' ',' Required number of simulations ? ',
     &        70,'W',MNRS,'F',80,' No. of simulations ',IER,4)

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
#include "uncertainty.h"

      COMMON/UA3/NICNG,IDTYP(MNCNG),IDMAT(MNCNG,3),DAT(MNCNG,2)
      COMMON/UA4/NIACT,IACTD(MNACT,2)
      COMMON/UA5/IMET,ISIM,NRSIM
      COMMON/UATD/UACLIM(6,3),UACTL(7,4),UAMCSA(MNACT)
      COMMON/UATD2/UAMISCTD(3)

C Reset climate change variables to zero - ONLY if first zone.
      if (IZONE.eq.1) then
        do 10 I=1,6
          UACLIM(I,1)=0.
          UACLIM(I,2)=0.
          UACLIM(I,3)=0.
 10     continue

C Reset control change variables to zero.
        do 15 I=1,7
          UACTL(I,1)=0.
          UACTL(I,2)=0.
          UACTL(I,3)=0.
          UACTL(I,4)=0.
 15     continue

C Reset misc vars to zero.
        do 25 I=1,3
          UAMISCTD(I)=0.
 25     continue
      endif

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

      write (36,*) 'IDTYP(IACTD(IACT,1))',IDTYP(IACTD(IACT,1)),
     &                          IACTD(IACT,1), IACT

C Depending on change type call editing routine.
      if (IDTYP(IACTD(IACT,1)).eq.1) then 
        call UAE01(IZONE,IACT,DIR)
      elseif (IDTYP(IACTD(IACT,1)).eq.2) then 
        call UAE02(IZONE,IACT,DIR)
      elseif (IDTYP(IACTD(IACT,1)).eq.3) then 
        call UAT03(IACT,DIR)
      elseif (IDTYP(IACTD(IACT,1)).eq.4) then 
        call UAE04(IZONE,IACT,DIR)
      elseif (IDTYP(IACTD(IACT,1)).eq.5) then 
        call UAE05(IZONE,IACT,DIR)
      elseif (IDTYP(IACTD(IACT,1)).eq.101) then 
        call UAZ01(IZONE,IACT,DIR)
      elseif (IDTYP(IACTD(IACT,1)).eq.1001) then 
        call UAT01(IACT,DIR)
      elseif (IDTYP(IACTD(IACT,1)).eq.1002) then 
         write (36,*) 'WARNING UNDEFINED UNCERTAINTY'
         write (6,*) 'WARNING UNDEFINED UNCERTAINTY'
C        call UAT02(IACT,DIR)
      elseif (IDTYP(IACTD(IACT,1)).eq.1004) then 
         write (36,*) 'WARNING UNDEFINED UNCERTAINTY'
         write (6,*) 'WARNING UNDEFINED UNCERTAINTY'
C        call UAT04(IACT,DIR)
      elseif (IDTYP(IACTD(IACT,1)).eq.1005) then 
         write (36,*) 'WARNING UNDEFINED UNCERTAINTY'
         write (6,*) 'WARNING UNDEFINED UNCERTAINTY'
C        call UAT05(IACT,DIR)
      endif


      RETURN
      END

C ******************** MCSA ********************
C MCSA calls the correct routines to edit data in memory for a 
C Monte-Carlo SA.

      SUBROUTINE MCSA(IZONE)
#include "uncertainty.h"

      COMMON/UA3/NICNG,IDTYP(MNCNG),IDMAT(MNCNG,3),DAT(MNCNG,2)
      COMMON/UA4/NIACT,IACTD(MNACT,2)
      COMMON/UA5/IMET,ISIM,NRSIM
      COMMON/UATD/UACLIM(6,3),UACTL(7,4),UAMCSA(MNACT)
      COMMON/UATD2/UAMISCTD(3)

C Reset climate change variables to zero - ONLY if first zone.
      if (IZONE.eq.1) then
        do 5 I=1,6
          UACLIM(I,1)=0.
          UACLIM(I,2)=0.
          UACLIM(I,3)=0.
 5      continue

C Reset control change variables to zero.
C UACTL (?,1)=change type (%,+/-); (?,2)=magnitude; (?,2)=associated action;
C UACTL (?,4)=time status (0 not active, 1 active)
        do 15 I=1,7
          UACTL(I,1)=0.
          UACTL(I,2)=0.
          UACTL(I,3)=0.
          UACTL(I,4)=0.
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

C Change parameters by a random amount (defined above).
      do 10 IACT=1,NIACT

C DIR is the direction of the alteration: +ive =up or high, -ive = down or low.
        DIR=UAMCSA(IACT)

C Debug...
        write (36,*) 'Uncertainty definition ',IACT,' altered by ',DIR

C Depending on change type call editing routine.
        if (IDTYP(IACTD(IACT,1)).eq.1) then 
          call UAE01(IZONE,IACT,DIR)
        elseif (IDTYP(IACTD(IACT,1)).eq.2) then 
          call UAE02(IZONE,IACT,DIR)
        elseif (IDTYP(IACTD(IACT,1)).eq.3) then 
          call UAT03(IACT,DIR)
        elseif (IDTYP(IACTD(IACT,1)).eq.4) then 
          call UAE04(IZONE,IACT,DIR)
        elseif (IDTYP(IACTD(IACT,1)).eq.5) then 
          call UAE05(IZONE,IACT,DIR)
        elseif (IDTYP(IACTD(IACT,1)).eq.101) then 
          call UAZ01(IZONE,IACT,DIR)
        elseif (IDTYP(IACTD(IACT,1)).eq.1001) then 
          call UAT01(IACT,DIR)
        elseif (IDTYP(IACTD(IACT,1)).eq.1002) then 
         write (36,*) 'WARNING UNDEFINED UNCERTAINTY'
         write (6,*) 'WARNING UNDEFINED UNCERTAINTY'
C          call UAT02(IACT,DIR)
        elseif (IDTYP(IACTD(IACT,1)).eq.1004) then 
         write (36,*) 'WARNING UNDEFINED UNCERTAINTY'
         write (6,*) 'WARNING UNDEFINED UNCERTAINTY'
C          call UAT04(IACT,DIR)
        elseif (IDTYP(IACTD(IACT,1)).eq.1005) then 
         write (36,*) 'WARNING UNDEFINED UNCERTAINTY'
         write (6,*) 'WARNING UNDEFINED UNCERTAINTY'
C          call UAT05(IACT,DIR)
        endif
 10   continue

      RETURN
      END

C ******************** FCTA ********************
C FCTA calls the correct routines to edit data in memory for a factorial analysis

      SUBROUTINE FCTA(IZONE)
#include "building.h"
#include "uncertainty.h"

      COMMON/UA3/NICNG,IDTYP(MNCNG),IDMAT(MNCNG,3),DAT(MNCNG,2)
      COMMON/UA4/NIACT,IACTD(MNACT,2)
      COMMON/UA5/IMET,ISIM,NRSIM
      COMMON/UATD/UACLIM(6,3),UACTL(7,4),UAMCSA(MNACT)
      COMMON/UAFCT/IADIR(MNFA),NFA

C Reset climate change variables to zero - ONLY if first zone.
      if (IZONE.eq.1) then
        do 10 I=1,6
          UACLIM(I,1)=0.
          UACLIM(I,2)=0.
          UACLIM(I,3)=0.
 10     continue

C Reset control change variables to zero.
        do 15 I=1,7
          UACTL(I,1)=0.
          UACTL(I,2)=0.
          UACTL(I,3)=0.
          UACTL(I,4)=0.
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
      do 30 I=1,NFA
        IACT=I
        DIR=float(IADIR(I))
        write (36,*) 'IDTYP(IACTD(IACT,1))',IDTYP(IACTD(IACT,1)),
     &                          IACTD(IACT,1), IACT

C Depending on change type call editing routine.
        if (IDTYP(IACTD(IACT,1)).eq.1) then 
          call UAE01(IZONE,IACT,DIR)
        elseif (IDTYP(IACTD(IACT,1)).eq.2) then 
          call UAE02(IZONE,IACT,DIR)
        elseif (IDTYP(IACTD(IACT,1)).eq.3) then 
          call UAT03(IACT,DIR)
        elseif (IDTYP(IACTD(IACT,1)).eq.4) then 
          call UAE04(IZONE,IACT,DIR)
        elseif (IDTYP(IACTD(IACT,1)).eq.5) then 
          call UAE05(IZONE,IACT,DIR)
        elseif (IDTYP(IACTD(IACT,1)).eq.101) then 
          call UAZ01(IZONE,IACT,DIR)
        elseif (IDTYP(IACTD(IACT,1)).eq.1001) then 
          call UAT01(IACT,DIR)
        endif
 30   continue


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
#include "uncertainty.h"
#include "esprdbfile.h"

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

C      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)
      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)

      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)

C      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),AE(MCOM,MS)

      COMMON/UA2/NILOC,NZG(MNIL),NZNOG(MNIL,MCOM),NSG(MNIL,MCOM),
     &             NSNOG(MNIL,MCOM,MS)
      COMMON/UA3/NICNG,IDTYP(MNCNG),IDMAT(MNCNG,3),DAT(MNCNG,2)
      COMMON/UA4/NIACT,IACTD(MNACT,2)

      CHARACTER CSTR*25

      DIMENSION IEL(ME)

C Data stored in UA01 has the following meaning in this routine:
C IDMAT(?,1): Primitive db reference number.
C IDMAT(?,2): Thermophysical property (con; den; sp ht).
C IDMAT(?,3): Change type (%; +/-; explicit).
C Define variables for current location and change.
      ICL=IACTD(IACT,2)
      ICC=IACTD(IACT,1)

      write (36,*) '   '
      write (36,*) 'Entering subroutine UAE01'
      write (36,*) 'Focus zone: ',IZONE

C Loop through all zones listed in NZNOG for the defined location ICL.
      do 10 II=1,NZG(ICL)
       IZ=NZNOG(ICL,II)
       if (IZ.eq.IZONE) then

C Change data in the required surfaces.
         do 20 JJ=1,NSG(ICL,II)
           IS=NSNOG(ICL,II,JJ)

C Check to see if primitive material is used in current construction.
C CHKPRIM returns the elements which are made of the required material (if any).
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
     &               SHT(IS,IE),EMISI(IS), EMISE(IS),ABSI(IS),ABSE(IS)

C Edit data.
               if (IDMAT(ICC,2).eq.1) then
                 call UAEDIT(CON(IS,IE),IDMAT(ICC,3),DAT(ICC,1),
     &                                     DAT(ICC,2),DIR,-1,0.01,0.0)
               elseif (IDMAT(ICC,2).eq.2) then
                 call UAEDIT(DEN(IS,IE),IDMAT(ICC,3),DAT(ICC,1),
     &                                   DAT(ICC,2),DIR,0,0.01,9000.0)
               elseif (IDMAT(ICC,2).eq.3) then
                 call UAEDIT(SHT(IS,IE),IDMAT(ICC,3),DAT(ICC,1),
     &                                   DAT(ICC,2),DIR,0,0.01,3000.0)
               elseif (IDMAT(ICC,2).eq.4) then
                 call UAEDIT(EMISE(IS),IDMAT(ICC,3),DAT(ICC,1),
     &                                  DAT(ICC,2),DIR,0,0.001,0.999)
                 call UAEDIT(EMISI(IS),IDMAT(ICC,3),DAT(ICC,1),
     &                                  DAT(ICC,2),DIR,0,0.001,0.999)
               elseif (IDMAT(ICC,2).eq.5) then
                 call UAEDIT(ABSE(IS),IDMAT(ICC,3),DAT(ICC,1),
     &                                  DAT(ICC,2),DIR,0,0.001,0.999)
                 call UAEDIT(ABSE(IS),IDMAT(ICC,3),DAT(ICC,1),
     &                                  DAT(ICC,2),DIR,0,0.001,0.999)
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
             do 60 JJ=1,NSG(ICL,II)
               if (NSNOG(ICL,II,JJ).eq.ISC) then

C Check to see if primitive material is used in current construction.
C CHKPRIM returns the elements which are made of the required material (if any).
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
                 call UAEDIT(CON(IS,IE),IDMAT(ICC,3),DAT(ICC,1),
     &                                   DAT(ICC,2),DIR,-1,0.01,0.0)
               elseif (IDMAT(ICC,2).eq.2) then
                 call UAEDIT(DEN(IS,IE),IDMAT(ICC,3),DAT(ICC,1),
     &                                 DAT(ICC,2),DIR,0,0.01,9000.0)
               elseif (IDMAT(ICC,2).eq.3) then
                 call UAEDIT(SHT(IS,IE),IDMAT(ICC,3),DAT(ICC,1),
     &                                 DAT(ICC,2),DIR,0,0.01,3000.0)
               elseif (IDMAT(ICC,2).eq.4) then
                 call UAEDIT(EMISE(IS),IDMAT(ICC,3),DAT(ICC,1),
     &                                DAT(ICC,2),DIR,0,0.001,0.999)
                 call UAEDIT(EMISI(IS),IDMAT(ICC,3),DAT(ICC,1),
     &                                DAT(ICC,2),DIR,0,0.001,0.999)
               elseif (IDMAT(ICC,2).eq.5) then
                 call UAEDIT(ABSE(IS),IDMAT(ICC,3),DAT(ICC,1),
     &                                DAT(ICC,2),DIR,0,0.001,0.999)
                 call UAEDIT(ABSE(IS),IDMAT(ICC,3),DAT(ICC,1),
     &                                DAT(ICC,2),DIR,0,0.001,0.999)
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
#include "uncertainty.h"
#include "esprdbfile.h"

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)

      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)

      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)

      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)

      COMMON/UA2/NILOC,NZG(MNIL),NZNOG(MNIL,MCOM),NSG(MNIL,MCOM),
     &             NSNOG(MNIL,MCOM,MS)
      COMMON/UA3/NICNG,IDTYP(MNCNG),IDMAT(MNCNG,3),DAT(MNCNG,2)
      COMMON/UA31/MATNAM(MNCNG,2)
      COMMON/UA4/NIACT,IACTD(MNACT,2)

C      CHARACTER TITL*72,NAM*72,CSTR*25
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12
      CHARACTER MATNAM*12,CSTR*25,DESC*48

C Data stored in UA02 has the following meaning in this routine:
C IDMAT(?,1): The material in the chosen MLC, if this is 0 then 
C              a new MLC reference will be in MATNAM(?,2).
C IDMAT(?,2): Unused.
C IDMAT(?,3): Change type (%; +/-; explicit).
C Define variables for current location and change.
      ICL=IACTD(IACT,2)
      ICC=IACTD(IACT,1)

      write (36,*) '   '
      write (36,*) 'Entering subroutine UAE02'
      write (36,*) 'Focus zone: ',IZONE

C Loop through all zones listed in NZNOG for the defined location ICL.
      do 10 II=1,NZG(ICL)
       IZ=NZNOG(ICL,II)
       if (IZ.eq.IZONE) then

C The current zone (IZ) is equal to the target zone (IZONE). 
C Loop through all the surfaces in the current location ICL for the 
C target zone - search for MLC's with the same name as MATNAM(ICC).
        do 20 JJ=1,NSG(ICL,II)
         IS=NSNOG(ICL,II,JJ)
         if (SMLCN(IS).eq.MATNAM(ICC,1)(1:12)) then

C This construction has uncertainties defined, but which MLC database 
C entry is it?
          do 25 IX=1,NMLC
           if (DESC(IX)(1:12).eq.SMLCN(IS)) IMLC=IX
 25       continue
          do 30 IE=1,NE(IS)
C           write (36,*) 'ipr(imlc,ie): ',IPR(IMLC,IE),imlc,ie
C           write (36,*) 'IDMAT(ICC,1): ',IDMAT(ICC,1),icc
           if (IPR(IMLC,IE).eq.IDMAT(ICC,1)) then

C Either edit layer thickness or whole construction.
            if (IE.eq.0) then
             write (36,*) 'MLC editing not enabled.'
            else
             write (36,*) 'Editing element thickness..'
             write (36,*) 'Zone: ',IZ,' Surface: ',IS,' Element',IE
             write (36,*) 'THRMLI: ',THRMLI(IZ,IS,IE,4)
             if (IDMAT(ICC,3).eq.1) then

C Percentage change. 
              THRMLI(IZ,IS,IE,4)=THRMLI(IZ,IS,IE,4)*
     &                                 (((DAT(ICC,1)/100.)*DIR)+1.0)
             elseif (IDMAT(ICC,3).eq.2) then

C Absolute change. 
              THRMLI(IZ,IS,IE,4)=THRMLI(IZ,IS,IE,4)+
     &                                 (DAT(ICC,1)*DIR)
             elseif (IDMAT(ICC,3).eq.3) then

C Explicit change. 
              if (DIR.gt.0.) then
               THRMLI(IZ,IS,IE,4)=THRMLI(IZ,IS,IE,4)+
     &                               (DAT(ICC,1)-THRMLI(IZ,IS,IE,4))*DIR
              else
               THRMLI(IZ,IS,IE,4)=THRMLI(IZ,IS,IE,4)-
     &                               (DAT(ICC,2)-THRMLI(IZ,IS,IE,4))*DIR
              endif
             endif
             write (36,*) 'to:'
             write (36,*) 'THRMLI: ',THRMLI(IZ,IS,IE,4)

C Now copy the altered data into the zonal commons.
             THK(IS,IE)=THRMLI(IZ,IS,IE,4)
            endif
           endif
 30       continue
         endif
 20     continue
       else

C Check for adjacent surfaces. 
C        write (36,*) ' number of surfaces ',NSUR
        do 40 ISURF=1,NSUR
         CALL SURADJ(IZONE,ISURF,IEX,T,IZC,ISC,ICN,CSTR)
         if (IZC.eq.IZ) then 

C Check if the coupled surface ISC is in the surface list NSNOG, if it 
C is then we have to edit the matching surface in this zone ISURF.
          do 60 JJ=1,NSG(ICL,II)
           IS=NSNOG(ICL,II,JJ)
           if (IS.eq.ISC) then
            if (SMLCN(ISURF).eq.MATNAM(ICC,1)(1:12)) then
             do 65 IX=1,NMLC
              if (DESC(IX)(1:12).eq.SMLCN(ISURF)) IMLC=IX
 65          continue
             do 50 IE=1,NE(ISURF)
              write (36,*) 'ipr(imlc,ie): ',IPR(IMLC,IE),imlc,ie
              write (36,*) 'IDMAT(ICC,1): ',IDMAT(ICC,1),icc
              if (IPR(IMLC,IE).eq.IDMAT(ICC,1)) then

C Either edit layer thickness or whole construction.
C Note the next few lines are not indented wrt the 'if' statement above.
               if (IE.eq.0) then
                write (36,*) 'MLC editing not enabled.'
               else
                write (36,*) 'Editing adjacent element thickness..'
                write (36,*) 'Zone:',IZONE,' Surface:',ISURF,
     &                                                 ' Element:',IE
                write (36,*) 'THRMLI: ',THRMLI(IZONE,ISURF,IE,4)
                if (IDMAT(ICC,3).eq.1) then

C Percentage change. 
                 THRMLI(IZONE,ISURF,IE,4)=THRMLI(IZONE,ISURF,IE,4)*
     &                                  (((DAT(ICC,1)/100.)*DIR)+1.0)
                elseif (IDMAT(ICC,3).eq.2) then

C Absolute change. 
                 THRMLI(IZONE,ISURF,IE,4)=THRMLI(IZONE,ISURF,IE,4)+
     &                                   (DAT(ICC,1)*DIR)
                elseif (IDMAT(ICC,3).eq.3) then

C Explicit change. 
                 if (DIR.gt.0.) then
                  THRMLI(IZONE,ISURF,IE,4)=THRMLI(IZONE,ISURF,IE,4)+
     &                         (DAT(ICC,1)-THRMLI(IZONE,ISURF,IE,4))*DIR
                 else
                  THRMLI(IZONE,ISURF,IE,4)=THRMLI(IZONE,ISURF,IE,4)-
     &                         (DAT(ICC,2)-THRMLI(IZONE,ISURF,IE,4))*DIR
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

      RETURN
      END

C ******************** UAE04 ********************
C UAE04 edits casual gains in memory.

      SUBROUTINE UAE04(IZONE,IACT,DIR)
#include "building.h"
#include "uncertainty.h"

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      COMMON/C1/NCOMP,NCON

      COMMON/UA2/NILOC,NZG(MNIL),NZNOG(MNIL,MCOM),NSG(MNIL,MCOM),
     &             NSNOG(MNIL,MCOM,MS)
      COMMON/UA3/NICNG,IDTYP(MNCNG),IDMAT(MNCNG,3),DAT(MNCNG,2)
      COMMON/UA4/NIACT,IACTD(MNACT,2)

      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT
      REAL ACI,ACV,TA
      COMMON/P2CTL/ITCTL(MCOM),TLO(MCOM),TUP(MCOM),THI(MCOM),ACIL(MCOM),
     &             ACVL(MCOM),IVL(MCOM),TAL(MCOM),ACIU(MCOM),ACVU(MCOM),
     &             IVU(MCOM),TAU(MCOM),ACIH(MCOM),ACVH(MCOM),
     &             IVH(MCOM),TAH(MCOM)
      COMMON/P3/NCAS1,ICGS1(MC),ICGF1(MC),CMGS1(MC),CMGL1(MC),
     &          RADC1(MC),CONC1(MC),NCAS2,ICGS2(MC),ICGF2(MC),CMGS2(MC),
     &          CMGL2(MC),RADC2(MC),CONC2(MC),NCAS3,ICGS3(MC),ICGF3(MC),
     &          CMGS3(MC),CMGL3(MC),RADC3(MC),CONC3(MC)
      COMMON/P3TYPE/ICGT1(MC),ICGT2(MC),ICGT3(MC)

C Data stored in UA04 has the following meaning in this routine:
C IDMAT(?,1): Casual gain type(1/2/3)/ scheduled air flow(0)
C IDMAT(?,2): Cas Gn: sensible(1)/ latent(2)/ radiant(3)/ convective(4)/
C IDMAT(?,2):         total Q (same split) (5)/ same Q diff split (6)/
C IDMAT(?,2):         total frac (same split) (7)/ 
C IDMAT(?,2):         same total frac diff split (8)
C IDMAT(?,2): Sched Air flow: infiltration(1)/ ventilation(2)/
C IDMAT(?,2):                 vent index(3)/ vent source temp(4)
C IDMAT(?,3): Change type (%; +/-; explicit).
C Define variables for current location and change.
      ICL=IACTD(IACT,2)
      ICC=IACTD(IACT,1)

C Define max number of periods for common P2 (flow).
      IMP2=NAC(1)
      DO 400 IDTY=1,NBDAYTYPE
        IF(NAC(IDTY).GT.IMP2)IMP2=NAC(IDTY)
 400  CONTINUE

C Define max number of periods for common P3 (Cas gns).
      IMP3=NCAS1
      if (NCAS2.gt.IMP3) IMP3=NCAS2
      if (NCAS3.gt.IMP3) IMP3=NCAS3

      write (36,*) '   '
      write (36,*) 'Entering subroutine UAE04'
      write (36,*) 'Focus zone: ',IZONE

C Loop through all zones listed in NZNOG for the defined location ICL.
      do 10 II=1,NZG(ICL)
        IZ=NZNOG(ICL,II)
        if (IZ.eq.IZONE) then

C Edit casual gains.
          if (IDMAT(ICC,1).eq.0) then

C Edit scheduled air flows.
C Check that we have the right element(period). << TO DO >>
C Currently changing all defined periods.
            write (36,*) 'Editing scheduled air flow'
            ICF=IDMAT(ICC,3)
            DAT1=DAT(ICC,1)
            DAT2=DAT(ICC,2)

C Percentage change. 
C What data type to change? (see comments at start of routine).
C Chance all instances of this type - will probably add data 
C to specify incividual periods later.
            if (IDMAT(ICC,2).eq.1) then
              write (36,*) 'Zone:',IZONE,' infiltration'
              do 100 IX=1,IMP2
                DO 4010 IDTY=1,NBDAYTYPE
                  T=ACI(IDTY,IX)
                  call UAEDIT(ACI(IDTY,IX),ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
                  write(36,*)'Period ',CALENTAG(IDTY),IX,' From:',T,
     &            ' to ',ACI(IDTY,IX)
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
 4012           CONTINUE
 104          continue
            endif
          else

C Edit casual gains.
C Check that we have the right element(period). << TO DO >>
C Currently changing all defined periods.
            write (36,*) 'Editing casual gains'
            ICF=IDMAT(ICC,3)
            DAT1=DAT(ICC,1)
            DAT2=DAT(ICC,2)
            IT=IDMAT(ICC,1)

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
              do 1100 IX=1,IMP3
               if (IDMAT(ICC,1).eq.ICGT1(IX)) then
                T1=CMGS1(IX)
                call UAEDIT(CMGS1(IX),ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
                write (36,*)'Period (wkd)',IX,' From:',T1,
     &                                           ' to ',CMGS1(IX)
               endif
               if (IDMAT(ICC,1).eq.ICGT2(IX)) then
                T2=CMGS2(IX)
                call UAEDIT(CMGS2(IX),ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
                write (36,*)'Period (sat)',IX,' From:',T2,
     &                                           ' to ',CMGS2(IX)
               endif
               if (IDMAT(ICC,1).eq.ICGT3(IX)) then
                T3=CMGS3(IX)
                call UAEDIT(CMGS3(IX),ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
                write (36,*)'Period (sun)',IX,' From:',T3,
     &                                           ' to ',CMGS3(IX)
               endif
 1100         continue
            elseif (IDMAT(ICC,2).eq.2) then
              write (36,*) 'Zone:',IZONE,' type:',IT,' latent'
              do 1101 IX=1,IMP3
               if (IDMAT(ICC,1).eq.ICGT1(IX)) then
                T1=CMGL1(IX)
                call UAEDIT(CMGL1(IX),ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
                write (36,*)'Period (wkd)',IX,' From:',T1,
     &                                           ' to ',CMGL1(IX)
               endif
               if (IDMAT(ICC,1).eq.ICGT2(IX)) then
                T2=CMGL2(IX)
                call UAEDIT(CMGL2(IX),ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
                write (36,*)'Period (sat)',IX,' From:',T2,
     &                                           ' to ',CMGL2(IX)
               endif
               if (IDMAT(ICC,1).eq.ICGT3(IX)) then
                T3=CMGL3(IX)
                call UAEDIT(CMGL3(IX),ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
                write (36,*)'Period (sun)',IX,' From:',T3,
     &                                           ' to ',CMGL3(IX)
               endif
 1101         continue
            elseif (IDMAT(ICC,2).eq.3) then
              write (36,*) 'Zone:',IZONE,' type:',IT,' rad fraction'
              do 1103 IX=1,IMP3
               if (IDMAT(ICC,1).eq.ICGT1(IX)) then
                T1=RADC1(IX)
                call UAEDIT(RADC1(IX),ICF,DAT1,DAT2,DIR,0,0.0,1.0)
                write (36,*)'Period (wkd)',IX,' From:',T1,
     &                                           ' to ',RADC1(IX)
               endif
               if (IDMAT(ICC,1).eq.ICGT2(IX)) then
                T2=RADC2(IX)
                call UAEDIT(RADC2(IX),ICF,DAT1,DAT2,DIR,0,0.0,1.0)
                write (36,*)'Period (sat)',IX,' From:',T2,
     &                                           ' to ',RADC2(IX)
               endif
               if (IDMAT(ICC,1).eq.ICGT3(IX)) then
                T3=RADC3(IX)
                call UAEDIT(RADC3(IX),ICF,DAT1,DAT2,DIR,0,0.0,1.0)
                write (36,*)'Period (sun)',IX,' From:',T3,
     &                                           ' to ',RADC3(IX)
               endif
 1103         continue
            elseif (IDMAT(ICC,2).eq.4) then
              write (36,*)'Zone:',IZONE,' type:',IT,' conv fraction'
              do 1104 IX=1,IMP3
               if (IDMAT(ICC,1).eq.ICGT1(IX)) then
                T1=CONC1(IX)
                call UAEDIT(CONC1(IX),ICF,DAT1,DAT2,DIR,0,0.0,1.0)
                write (36,*)'Period (wkd)',IX,' From:',T1,
     &                                           ' to ',CONC1(IX)
               endif
               if (IDMAT(ICC,1).eq.ICGT2(IX)) then
                T2=CONC2(IX)
                call UAEDIT(CONC2(IX),ICF,DAT1,DAT2,DIR,0,0.0,1.0)
                write (36,*)'Period (sun)',IX,' From:',T3,
     &                                           ' to ',CONC3(IX)
               endif
               if (IDMAT(ICC,1).eq.ICGT3(IX)) then
                T3=CONC3(IX)
                call UAEDIT(CONC3(IX),ICF,DAT1,DAT2,DIR,0,0.0,1.0)
                write (36,*)'Period (sat)',IX,' From:',T2,
     &                                           ' to ',CONC2(IX)
               endif
 1104         continue
            elseif (IDMAT(ICC,2).eq.5) then
              write (36,*) 'Zone:',IZONE,' type:',IT,' total Q'
              do 1105 IX=1,IMP3
               if (IDMAT(ICC,1).eq.ICGT1(IX)) then
                T1=CMGS1(IX)
                T2=CMGL1(IX)
                T3=T1+T2
                call UAEDIT(T3,ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
                CMGS1(IX)=T3*(CMGS1(IX)/(T1+T2))
                CMGL1(IX)=T3*(CMGL1(IX)/(T1+T2))
                write (36,*)'Period (wkd)',IX,' Sensible from:',T1,
     &                                           ' to ',CMGS1(IX)
                write (36,*)'Period (wkd)',IX,' Latent from:',T2,
     &                                           ' to ',CMGL1(IX)
               endif
               if (IDMAT(ICC,1).eq.ICGT2(IX)) then
                T1=CMGS2(IX)
                T2=CMGL2(IX)
                T3=T1+T2
                call UAEDIT(T3,ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
                CMGS2(IX)=T3*(CMGS2(IX)/(T1+T2))
                CMGL2(IX)=T3*(CMGL2(IX)/(T1+T2))
                write (36,*)'Period (sat)',IX,' Sensible from:',T1,
     &                                           ' to ',CMGS2(IX)
                write (36,*)'Period (sat)',IX,' Latent from:',T2,
     &                                           ' to ',CMGL2(IX)
               endif
               if (IDMAT(ICC,1).eq.ICGT3(IX)) then
                T1=CMGS3(IX)
                T2=CMGL3(IX)
                T3=T1+T2
                call UAEDIT(T3,ICF,DAT1,DAT2,DIR,-1,0.0,0.0)
                CMGS3(IX)=T3*(CMGS3(IX)/(T1+T2))
                CMGL3(IX)=T3*(CMGL3(IX)/(T1+T2))
                write (36,*)'Period (sun)',IX,' Sensible from:',T1,
     &                                           ' to ',CMGS3(IX)
                write (36,*)'Period (sun)',IX,' Latent from:',T2,
     &                                           ' to ',CMGL3(IX)
               endif
 1105         continue
            elseif (IDMAT(ICC,2).eq.8) then
              IT=IDMAT(ICC,1)
              write(36,*)'Zone:',IZONE,' type:',IT,' rad/conv split'
              do 1108 IX=1,IMP3
               if (IDMAT(ICC,1).eq.ICGT1(IX)) then
                T1=RADC1(IX)
                T2=CONC1(IX)
                T3=T1+T2
                call UAEDIT(CONC1(IX),ICF,DAT1,DAT2,DIR,0,0.0,T3)
                RADC1(IX)=T3-CONC1(IX)
                write (36,*)'Period (wkd)',IX,
     &                     ' Rad fraction from:',T1,' to ',RADC1(IX)
                write (36,*)'Period (wkd)',IX,
     &                    ' Conv fraction from:',T2,' to ',CONC1(IX)
               endif
               if (IDMAT(ICC,1).eq.ICGT2(IX)) then
                T1=RADC2(IX)
                T2=CONC2(IX)
                T3=T1+T2
                call UAEDIT(CONC2(IX),ICF,DAT1,DAT2,DIR,0,0.0,T3)
                RADC2(IX)=T3-CONC2(IX)
                write (36,*)'Period (wkd)',IX,
     &                     ' Rad fraction from:',T1,' to ',RADC2(IX)
                write (36,*)'Period (wkd)',IX,
     &                    ' Conv fraction from:',T2,' to ',CONC2(IX)
               endif
               if (IDMAT(ICC,1).eq.ICGT3(IX)) then
                T1=RADC3(IX)
                T2=CONC3(IX)
                T3=T1+T2
                call UAEDIT(CONC3(IX),ICF,DAT1,DAT2,DIR,0,0.0,T3)
                RADC3(IX)=T3-CONC3(IX)
                write (36,*)'Period (wkd)',IX,
     &                     ' Rad fraction from:',T1,' to ',RADC3(IX)
                write (36,*)'Period (wkd)',IX,
     &                    ' Conv fraction from:',T2,' to ',CONC3(IX)
               endif
 1108         continue
            else
              write (36,*)'Editing request unknown.'
            endif
          endif
        endif
 10   continue

      RETURN
      END

C ******************** UAE05 ********************
C UAE05 edits convection coefficients algorithm choice.

      SUBROUTINE UAE05(IZONE,IACT,DIR)
#include "building.h"
#include "uncertainty.h"

      COMMON/C1/NCOMP,NCON
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

      COMMON/UA2/NILOC,NZG(MNIL),NZNOG(MNIL,MCOM),NSG(MNIL,MCOM),
     &             NSNOG(MNIL,MCOM,MS)
      COMMON/UA3/NICNG,IDTYP(MNCNG),IDMAT(MNCNG,3),DAT(MNCNG,2)
      COMMON/UA4/NIACT,IACTD(MNACT,2)

      common/CORIND/ICORI(MCOM,MS),ICORE(MCOM,MS)

C Data stored in UA04 has the following meaning in this routine:
C IDMAT(?,1): =1 just now - associated data is convection corr to use
C IDMAT(?,2): =1 just now - associated data is convection corr to use
C IDMAT(?,3): 4 - alternative correlation id number
C Define variables for current location and change.
      ICL=IACTD(IACT,2)
      ICC=IACTD(IACT,1)

      write (36,*) 'Entering subroutine UAE05'
      write (36,*) 'Focus zone: ',IZONE

      write (36,*) ' Existing correlations: '
      write (36,*) '  ',(ICORI(IZONE,JS),JS=1,NSUR)
      write (36,*) 'IZONE:',IZONE,'  NZG(ICL):',NZG(ICL)

C Loop through all zones listed in NZNOG for the defined location ICL.
      do 10 II=1,NZG(ICL)
        IZ=NZNOG(ICL,II)
      write (36,*) 'NZNOG(ICL,II):',NZNOG(ICL,II),
     &             '  NSG(ICL,II):',NSG(ICL,II)
        if (IZ.eq.IZONE) then
          do 20 IJ=1,NSG(ICL,II)
            IS=NSNOG(ICL,II,IJ)
      write (36,*) 'NSNOG(ICL,II,IJ):',NSNOG(ICL,II,IJ)
            if (DIR.gt.0.) then
              ICORI(IZ,IS)=nint(DAT(ICC,1))
            elseif (DIR.lt.0.) then
              ICORI(IZ,IS)=nint(DAT(ICC,2))
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
C UAZ01 edits surface properties in memory.

      SUBROUTINE UAZ01(IZONE,IACT,DIR)
#include "building.h"
#include "uncertainty.h"

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/PREC2/VOL(MCOM)

      COMMON/UA2/NILOC,NZG(MNIL),NZNOG(MNIL,MCOM),NSG(MNIL,MCOM),
     &             NSNOG(MNIL,MCOM,MS)
      COMMON/UA3/NICNG,IDTYP(MNCNG),IDMAT(MNCNG,3),DAT(MNCNG,2)
      COMMON/UA4/NIACT,IACTD(MNACT,2)

      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)

      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)


C Data stored in UA03 has the following meaning in this routine:
C IDMAT(?,1): 1=air
C IDMAT(?,2): 1=volume, ...
C IDMAT(?,3): Change type (%; +/-; explicit).
C Define variables for current location and change.
      ICL=IACTD(IACT,2)
      ICC=IACTD(IACT,1)

      write (36,*) '   '
      write (36,*) 'Entering subroutine UAZ01'
      write (36,*) 'Focus zone: ',IZONE

C Loop through all zones listed in NZNOG for the defined location ICL.
      do 10 II=1,NZG(ICL)
        IZ=NZNOG(ICL,II)
        if (IZ.eq.IZONE) then
          write (36,*) 'Editing zone volume..'
          ICF=IDMAT(ICC,3)
          DAT1=DAT(ICC,1)
          DAT2=DAT(ICC,2)
          if (IDMAT(ICC,2).eq.1) then 
            write (36,*) 'Zone: ',IZ,' Volume: ',VOL(IZ)
            call UAEDIT(VOL(IZ),ICF,DAT1,DAT2,DIR,-1,-1.0,0.0)

C Check for out of range values.
            if (VOL(IZ).lt.0.0) then
              VOL(IZ)=1.0
              write (36,*) ' Zone volume less than 0.0, setting to 1.0'
            endif
            write (36,*)'to:                     ',VOL(IZ)
          endif
        endif
 10   continue

      RETURN
      END

C ******************** CHKPRIM ********************
C CHKPRIM returns list of elements referencing required primitive material
C for required surface in zone.
C NEL = number of matching elements
C IEL = index of matching elements

      SUBROUTINE CHKPRIM(IZONE,ISURF,IPRIM,NEL,IEL)
#include "building.h"

      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)
      CHARACTER DESC*48
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12

      DIMENSION IEL(ME)

C Set number of elements to zero.
      NEL=0

C Find correct MLC. 
      do 10 I=1,NMLC
        if (DESC(I)(1:12).eq.SMLCN(ISURF)(1:12)) then
          do 20 J=1,LAYERS(I)
            if (IPR(I,J).eq.IPRIM) then
              NEL=NEL+1
              IEL(NEL)=J
            endif
 20       continue
        endif
 10   continue

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
