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

C context.F contains:
C  EDSITE: edits site/general information for system configuration file
C  Roaming:  manages data associated with rotating a model as a
C            function of time.  .
C  manageimages: allows the user to manage images associated with
C            a model 
C  Setuptemporal: coordinates access to and initial creation of
C                 temporal data to be associated with a model.
C  PECONV: conversion factors for primary energy units.
C  Calenmanage: sets up and manages a calendar for ESP-r.
C  Calenprint:  displays a calendar for year iyear (or a few months)
C  EDDGTP:      sets up ground temperature profiles
C  EDDSHD:      sets up shading and insolation files

C ************* EDSITE 
C EDSITE Edits site/general information for system configuration file.
C ITRU unit number for user output, IER=0 indicates no error.
      SUBROUTINE EDSITE(ITRC,IER)
#include "building.h"
#include "model.h"
#include "site.h"
#include "geometry.h"
#include "tdf2.h"
#include "sbem.h"
#include "ipvdata.h"

      integer lnblnk  ! function definition
      integer igraphiclib  ! external definition
      integer iCountWords

      COMMON/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/PREC8/SLAT,SLON

      common/user/browse

C IPV description via ipvdata.h. If within cfg file then the file
C name is replaced with 'internal'.
      common/IPVF/lipvdatf

C CPCALC description
      common/CPCALC/icpcon,ble,bwi,bhi,blox,bloy,bloz,orient,irt,ra,
     &              sbh,pad,wvpe

C Variables for weekdays, and weekends.
C Assume: Mon=1, Tue=2, Wed=3, Thu=4, Fri=5, Sat=6, Sun=7
      common/wkdtyp/idwe1,idwe2,wkd1,wkd2

C Images.
      character imgfmt*4  ! GIF XBMP TIF JPG
      character imgfoc*4  ! FZON FNET FCTL FDFS
      character limgfil*72  ! file name (extend to 144 char)
      character imgdoc*248  ! text associated with image
      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)

      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes
      common/imagfi/noimg,iton
      COMMON/GTFIL/GTGEOM
      COMMON/GT/GTNAME

C Calendar.
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)

C iSlr_half_hr_flg =0 = hour-centred; 1 = half-hour centred.
      common/CWEC_SOL/iSlr_half_hr_flg
      integer iSlr_half_hr_flg

      logical MODSIT,browse,XST,modish,OK,DOK,unixok
      logical FOUND
      dimension SALT(8),IVALS(8)
      character*10 wkd1, wkd2
      character*10 wkday(7)
      character SEXP*31,SALT*31,hold*72

C ITEMS is menu for site/context items.
      character*35 ITEMS(29)
      character*72 GTGEOM
      character lipvdatf*72,outs*124,ltmp*72
      character act*1,GTNAME*15
      character ipvact*3  ! to denote whether in cfd file or external

      integer idwe1T,idwe2T,iyearT   ! local values for editing.
      integer icreport  ! to signal that calendar has been updated.
      integer IDOL,IDAYNUM,IMTHNUM,IDWKNUM,IDTYY  ! for correcting calendar
      integer nitms,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDSITE'  ! set for subroutine
      
      data wkday/'Monday', 'Tuesday','Wednesday', 'Thursday',
     &           'Friday','Saturday', 'Sunday'/

C Set initial states to false.
      MODSIT=.false.
      modish=.false.
      call tstamp('>','PRJ: model context')

C Check if Unix-based or DOS based.
      call isunix(unixok)

C Initial string for site exposure.
      SALT(1)='urban (normal)         '
      SALT(2)='urban (low density)    '
      SALT(3)='rural                  '
      SALT(4)='urban (equal weighting)'
      SALT(5)='urban (high rise)      '
      SALT(6)='isolated rural         '
      SALT(7)='totally enclosed       '
      SALT(8)='user defined           '
C      WRITE(SALT(8),53)skyview,groundview,buildingview
      SEXP=SALT(siteexposureindex)

C Begin with high level menu.
    3 INO=-4
      IER=0
      WRITE(ITEMS(1),'(A,F6.2)')'a site latitude: ',sitelat
      WRITE(ITEMS(2),'(A,F6.2)')'b longitude difference:',sitelongdif
      if(iSlr_half_hr_flg.eq.0)then
        WRITE(ITEMS(3),'(A)')   'c solar timing: hour centred'
      elseif(iSlr_half_hr_flg.eq.1)then
        WRITE(ITEMS(3),'(A)')   'c solar timing: half-hour centred'
      else
        WRITE(ITEMS(3),'(A)')   'c solar timing: not defined'
      endif

      WRITE(ITEMS(4),'(A,A)')   'c exposure: ',SEXP(1:21)
      IF(groundreflmodel.EQ.1) THEN
        WRITE(ITEMS(5),'(a,f5.3,a)')
     &                   'd grd reflectivity: const. (',groundrefl,')'
      ELSEIF(groundreflmodel.EQ.2) THEN
        ITEMS(5) =       'd grd reflectivity: simple'
      ELSEIF(groundreflmodel.EQ.3) THEN
        ITEMS(5) =       'd grd reflectivity: advanced'
      ENDIF
      WRITE(ITEMS(6),'(4a)')    'e weekends: ',wkd1,'/',wkd2
      WRITE(ITEMS(7),'(a,I4)')  'f year: ',IYEAR
      ITEMS(8) =                'g calendar (holidays)            '
      ITEMS(9) =                'h building and client address    '
      ITEMS(10)=                'i simulationist address          '
      ITEMS(11)=                'j                                '
      ITEMS(12)=                '  ___________________________    '
      if(GTGEOM(1:2).eq.'  '.or.GTGEOM(1:4).eq.'UNKN')then
        ITEMS(13)=              'k ground topology                '
      else
        WRITE(ITEMS(13),'(2A)') 'k ground topology:',GTNAME(1:13)
      endif
      WRITE(ITEMS(14),'(A,i2)') 'l ground temperature profiles:',NGRDP
      ITEMS(15)=                'm 3-D ground representation      '
      ITEMS(16)=                '  ___________________________    '
      ITEMS(17)=                'n primary energy conversions     '
      ITEMS(18)=                'o fan/lift/DHW demands           '
      ITEMS(19)=                'p integrated performance view    '
      ITEMS(20)=                '  ___________________________    '
      ITEMS(21)=                'q pressure coefficients calc.    '
      ITEMS(22)=                'r use measured/temporal data     '
      ITEMS(23)=                '  ___________________________    '
      WRITE(ITEMS(24),'(a,i3)') 's define images: ',noimg
      ITEMS(25)=                't UK NCM specifications          '
c      ITEMS(26)=                'u transient orientation          '
      ITEMS(26)=                '          '
      ITEMS(27)=                ' _____________________________ '
      ITEMS(28)=                '? help                           '
      ITEMS(29)=                '- exit this menu                 '
      nitms=29

C Help text for this menu.
      helptopic='context_overview_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('Model Context',ITEMS,nitms,INO)
      IF(INO.EQ.nitms)THEN

C Update configuration if context changed. If lat/long changes then
C check to see if any existing shading db need to be updated.
        if(MODSIT.and.(.NOT.browse))then
          CALL EMKCFG('-',IER)
        endif
        if(modish.and.(.NOT.browse))then

C If there was a change in location, but no zones defined just return.
          if(ncomp.eq.0)then
            return
          endif
          do 42 iz=1,ncomp
            if(ISI(iz).eq.1)then
              call FINDFIL(LSHAD(iz),XST)
              if(XST)then
                write(outs,'(3a)') 'Shading for ',
     &            zname(iz)(1:lnzname(iz)),' is out of date.'
                call edisp(iuout,outs)
              endif
            endif
  42      continue
          CALL EASKABC(
     &        'Site changes may require shading adjustments',
     &        'Options:',
     &        'recalculate (silent)','recalculate (interactive)',
     &        'cancel',IRT,nbhelp)
          if(IRT.eq.1.or.IRT.eq.2)then
            call edisp(iuout,'Processing zones - may take some time.')
            if(IRT.eq.1)then
              call edisp(iuout,
     &           'Control will be returned to the Project Manager.')
            endif
            do 43 iz=1,ncomp
              if(ISI(iz).eq.0) goto 43
              call FINDFIL(LSHAD(iz),XST)
              if(XST)then

C Get logical name of terminal type, expand model name
C to include the path and create a string to drive ish.
C If user asked for silent recalculation then do each
C one in forground in an xterm with `-act update_silent`
C as the command line. For silent do the runit via
C the current applications command window rather than
C starting up a new xterm (i.e. use '-' rather than 'text')
                if(IRT.eq.1)then
                  call comissionish(iz,'sr ',ier)
                else
                  DOK=.TRUE.
                  write(outs,'(3a)') 'Proceed with shading for ',
     &              zname(iz)(1:lnzname(iz)),'?'
                  CALL ASKOK(outs,' ',OK,DOK,nbhelp)
                  if(OK)then
                    call comissionish(iz,'ir ',ier)
                  endif
                endif
              endif
  43        continue
            call edisp(iuout,'Zones shading db update complete.')
          endif
        endif
        RETURN
      ELSEIF(INO.EQ.nitms-1)THEN

C Produce help text for the menu.
        helptopic='context_overview_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Model context',nbhelp,'-',0,0,IER)

      ELSEIF(INO.EQ.1)THEN

        CALL EASKR(SLAT,' ','Site latitude?',
     &       -89.9,'W',89.9,'W',55.9,'site latitude',IER,nbhelp)
        IF(IER.EQ.0)sitelat=SLAT
        MODSIT=.true.
        modish=.true.

      ELSEIF(INO.EQ.2)THEN

        CALL EASKR(SLON,' ','Longitude difference?',
     &       -15.0,'W',15.0,'W',-4.1,'longitude difference',IER,nbhelp)
        IF(IER.EQ.0)sitelongdif=SLON
        MODSIT=.true.
        modish=.true.

      ELSEIF(INO.EQ.3)THEN

C Solar radiation timing toggle.
        if(iSlr_half_hr_flg.eq.1)then
          iSlr_half_hr_flg=0
        elseif(iSlr_half_hr_flg.eq.0)then
          iSlr_half_hr_flg=1
        endif
        MODSIT=.true.

      ELSEIF(INO.EQ.4)THEN

C        WRITE(SALT(8),53)skyview,groundview,buildingview
C  53    FORMAT('user defined)
        SALT(8)='user defined'

        helptopic='site_exposure_options'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IX=1
        CALL EPICKS(IX,IVALS,' ','Site Exposure',
     &                31,8,SALT,'Site Exposure',IER,nbehlp)
        IF(IX.EQ.0)THEN
          INO=-4
          GOTO 3
        ENDIF

        siteexposureindex=IVALS(1)
        SEXP=SALT(siteexposureindex)
        IF(siteexposureindex.EQ.8)THEN
  290     CALL EASKR(skyview,' ','Sky view factor?',
     &      0.0,'W',0.99,'W',0.33,'sky view factor',IER,nbhelp)
          CALL EASKR(groundview,' ','Ground view factor?',
     &      0.0,'W',0.99,'W',0.33,'ground view factor',IER,nbhelp)
          CALL EASKR(buildingview,' ','Surroundings view factor?',
     &      0.0,'W',0.99,'W',0.33,'surroundings view factor',IER,nbhelp)
          IF(skyview.LT.0.0.or.groundview.LT.0.0.or.
     &       buildingview.LT.0.0)then
            CALL USRMSG(' ','View factor cannot be negative!','W')
            GOTO 290
          ENDIF
          IF(skyview.GT.1.0.or.groundview.GT.1.0.or.
     &       buildingview.GT.1.0)then
            CALL USRMSG(' ','View factor cannot be > 1.0!','W')
            GOTO 290
          ENDIF
          IF((skyview+groundview+buildingview).GT.1.0)THEN
            CALL USRMSG(' ','Total view factor cannot be > 1.0!','W')
            GOTO 290
          ENDIF
          IF(ABS(skyview+groundview+buildingview-1.0).GT.0.001)THEN
            CALL USRMSG(' ','Total view factor must = 1!','W')
            GOTO 290
          ENDIF
        ENDIF
        MODSIT=.true.

      ELSEIF(INO.EQ.5)THEN

C Ground reflectance type
        helptopic='context_ground_refl'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABCD(' ','Ground reflectance method?',
     &    'constant','monthly simple','mnthly advanced','cancel',
     &    IW,nbhelp)

C---- Constant ground reflectance        
        if (IW.EQ.1) then
          groundreflmodel=1
          CALL EASKR(groundrefl,' ',' Ground reflectance?',
     &        0.0,'W',0.99,'W',0.2,'ground reflectance',IER,nbhelp)
          MODSIT=.true.
C---- Simple or advanced models: ask for monthly values 
        elseif (IW.EQ.2 .OR. IW.EQ.3) then
          groundreflmodel=IW
          
   83     HOLD=' '
          WRITE(HOLD,'(1X,6F7.2)')(groundreflmonth(J),J=1,6)
          CALL EASKS(HOLD,' ',
     &      'January thro June ground reflectance (no snow)?',
     &      72,'0.2 0.2 0.2 0.2 0.2 0.2','gr albedo jan-jun',
     &      IER,nbhelp)
          NV = iCountWords(HOLD)
          IF (NV.NE.6) GOTO 83
          K=0
          DO 84 J=1,6
            CALL EGETWR(HOLD,K,GV,0.0,0.99,'W','profile',IER)
            IF (IER.NE.0) GOTO 83
            groundreflmonth(J)=GV
   84     CONTINUE
   85     CONTINUE
          HOLD = ' '
          WRITE(HOLD,'(1X,6F7.2)')(groundreflmonth(J),J=7,12)
          CALL EASKS(HOLD,' ',
     &      'July thro December ground reflectance (no snow)?',
     &       72,'0.2 0.2 0.2 0.2 0.2 0.2','gr albedo jul-dec',
     &      IER,nbhelp)
          NV = iCountWords(HOLD)
          IF (NV.NE.6) GOTO 85
          K=0
          DO 86 J=7,12
            CALL EGETWR(HOLD,K,GV,0.0,0.99,'W','profile',IER)
            IF (IER.NE.0) GOTO 85
            groundreflmonth(J)=GV
   86     CONTINUE
          MODSIT=.true.
C---- Simple model: ask for average albedo of snow, and number of days
C     with snow on the ground
          if (IW.EQ.2) then
            CALL EASKR(snowgroundrefl,' ',
     &        ' Average ground reflectance in the presence of snow?',
     &        0.0,'W',0.99,'W',0.4,'snow reflectance',IER,nbhelp)
            MODSIT=.true.
   87       CONTINUE
            HOLD=' '
            WRITE(HOLD,'(6(1X,I2))')(dayswithsnow(J),J=1,6)
            CALL EASKS(HOLD,' ',
     &        'Number of days in January thro June with snow?',
     &        72,'0 0 0 0 0 0','days with snow jan-jun',IER,nbhelp)
            NV = iCountWords(HOLD)
            IF (NV.NE.6) GOTO 87
            K=0
            DO 88 J=1,6
              CALL EGETWI(HOLD,K,IV,0,31,'W','profile',IER)
              IF (IER.NE.0) GOTO 87
              dayswithsnow(J)=IV
   88       CONTINUE
   89       CONTINUE
            HOLD = ' '
            WRITE(HOLD,'(6(1X,I2))')(dayswithsnow(J),J=7,12)
            CALL EASKS(HOLD,' ',
     &        'Number of days in July thro December with snow?',
     &        72,'0 0 0 0 0 0 ','days with snow jul-dec',IER,nbhelp)
            NV = iCountWords(HOLD)
            IF (NV.NE.6) GOTO 89
            K=0
            DO 90 J=7,12
              CALL EGETWI(HOLD,K,IV,0,31,'W','profile',IER)
              IF (IER.NE.0) GOTO 89
              dayswithsnow(J)=IV
   90       CONTINUE
          endif
C---- Advanced model: ask for albedo of fresh snow, and file containing
C     hourly snow depths
          if (IW.EQ.3) then
            CALL EASKR(snowgroundrefl,' ',
     &        ' Ground reflectance in the presence of fresh snow?',
     &        0.0,'W',0.99,'W',0.5,'snow reflectance',IER,nbhelp)
    
   92       CONTINUE
            CALL EASKS(SNFNAM,'Enter name of snow depth file',
     &        ' ',72,' ','Snow depth file',IER,nbhelp)
            CALL FINDFIL(SNFNAM,FOUND)
            IF(.NOT.FOUND) THEN
              DOK=.FALSE.
              CALL ASKOK('File not found: continue?',' ',
     &          OK,DOK,nbhelp)
              IF (.NOT.OK) GOTO 92
            ENDIF
          endif
        endif

      ELSEIF(INO.EQ.6)THEN

C Weekends def.
        call edisp(iuout,'Please define weekend days.')
        idwe1T=idwe1
        CALL EASKI(idwe1T,'Day index (Mon=1,Tue=2, ...) for',
     &    'the first day of the weekend?',
     &        1,'F',7,'F',6,'1st weekend index',IERI,nbhelp)
        if(ieri.eq.-3) goto 3
        idwe1=idwe1T
        wkd1=wkday(idwe1)

        idwe2T=idwe2
        CALL EASKI(idwe2T,'Day index (Mon=1,Tue=2, ...) for',
     &    'the second day of the weekend?',
     &        1,'F',7,'F',7,'2nd weekend index',IERI,nbhelp)
        if(ieri.eq.-3) goto 3
        idwe2=idwe2T
        wkd2=wkday(idwe2)
        MODSIT=.true.

      ELSEIF(INO.EQ.7)THEN

C Change the year and if calendar has not been set up initialise
C and then list the months of the year.  If there is an existing
C calendar with the default day types and the year has changed
C then re-initialize the calendar.
        helptopic='context_model_year'
        call gethelptext(helpinsub,helptopic,nbhelp)
        iyeart=iyear
        CALL EASKI(iyeart,' ','Simulation year?',
     &       1900,'W',2010,'W',2001,'year',IERI,nbhelp)
        if(ieri.eq.-3) goto 3
        iyear=iyeart
        MODSIT=.true.

C << assumption that 3 day types is a default. Revisit this logic
C << to see if nbdaytype can legally be 1 or 2.

        if(nbdaytype.eq.0)then
          call calenmanage('i',ier)
        elseif(nbdaytype.eq.3)then
          call calenmanage('i',ier)
        elseif(nbdaytype.gt.3)then

C Perform check that calendar day types correspond with simulation year.
C The logic below assumes that the first three day types are still weekday
C saturday sunday. It loops through each Julian day and if the model
C calendar day type is one of the standard 3 it checks that the model weekday
C is the same as a Julian weekday. Any calendar day types beyond 3 are 
C retained. If no calendar was defined in the model then it should do the
C work silently. (Same logic as in esystem.F) 
          ICREPORT=0
          DO 123 IDOL=1,365
            IF(ICALENDER(IDOL).LE.3)THEN
              CALL EDAYR(IDOL,IDAYNUM,IMTHNUM)
              CALL EWEEKD(IDAYNUM,IMTHNUM,IYEAR,IDWKNUM)
              IF(IDWKNUM.LT.6)THEN
                IDTYY=1 ! WEEKDAY
              ELSEIF(IDWKNUM.EQ.6)THEN
                IDTYY=2 ! SATURDAY
              ELSEIF(IDWKNUM.EQ.7)THEN
                IDTYY=3 ! SUNDAY
              ENDIF
              IF(ICALENDER(IDOL).EQ.0)THEN
                ICALENDER(IDOL)=IDTYY  ! update the model calendar
                ICREPORT=2             ! do it silently
              ELSEIF(ICALENDER(IDOL).NE.IDTYY)THEN
                ICALENDER(IDOL)=IDTYY  ! update the model calendar
                ICREPORT=1
              ENDIF
            ENDIF
 123      CONTINUE
          if(ICREPORT.EQ.1)then
            CALL EDISP(IUOUT,'  ')
            CALL EDISP(IUOUT,
     &        'Simulation year and calendar mismatch rectified')
          endif
        endif
        dok=.false.
        call askok(' ','View calendar?',ok,dok,nbhelp)
        if(ok)then
          loop=12
          loopst=1
          call calenprint(iuout,'t',iyear,loopst,loop)
        endif

      ELSEIF(INO.EQ.8)THEN

C Calendar functions. If number of daytypes is zero then
C initialise. Then enter interactive facility to allow user
C to define holidays.
        if(nbdaytype.le.3)then
          act = 'i'
          call calenmanage(act,ier)
        endif
        act = '-'
        call calenmanage(act,ier)

      ELSEIF(INO.EQ.9)THEN

C The address of the model and client (as entered in the ncm file).
        call edsbmbld()

      ELSEIF(INO.EQ.10)THEN

C The address of the simulation team (as entered in the ncm file).
        call edsbmbld()

      ELSEIF(INO.EQ.11)THEN

C Future option.
        continue

      ELSEIF(INO.EQ.13)THEN

C Ground topology.
        call easkabcd(' ','Ground topology defining options:',
     &    'select current','create manually','create via bitmap',
     &    'cancel',iwgt,nbhelp)
        if(iwgt.eq.1)then
          call GTOPOL(itrc,ier)
          MODSIT=.true.
        elseif(iwgt.eq.2)then
          call GTOPOL(itrc,ier)
          MODSIT=.true.
        elseif(iwgt.eq.3)then
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          if(iglib.eq.1)then
            iz=0
            call edisp(iuout,'In the bitmap facility you must toggle')
            call edisp(iuout,'to the ground topology input mode.')
            call clickonbitmap(iz,ier)
            call GTOPOL(itrc,ier)
            MODSIT=.true.
          elseif(iglib.eq.2)then
            call usrmsg('Sorry: the clickonbitmap facility is not yet',
     &        'operational with the current graphics library.','W')
          elseif(iglib.eq.3)then
            call usrmsg('Must be in graphic mode to use the click on',
     &      'bitmap facility to define ground toplogy.','W')
          endif
        endif

      ELSEIF(INO.EQ.14)THEN

C Ground temperature profiles.
C        call edisp(iuout,' ')  
C        write(outs,'(a,i3,a)')' There are currently ',NGRDP,' profiles.'      
C        call edisp(iuout,outs) 
        CALL EASKABC(' ','Ground temperature profile options:',
     &   'list','edit','add',IRT,nbhelp)
        if(IRT.eq.1)then
          if(NGRDP.gt.0)then
            do 22 igrdp=1,NGRDP    
              CALL EDISP(iuout,' Profile for Jan. thro Dec.:')
              WRITE(OUTS,'(12F6.1)')(UGRDTP(J,IGRDP),J=1,12)
              call edisp(iuout,outs)
 22         continue
          endif
          goto 3
        elseif(IRT.eq.2)then
          if(NGRDP.gt.0)then
            CALL EASKI(IFOC,' ','Profile to edit?',
     &        1,'F',NGRDP,'F',1,'edit profile',IERI,nbhelp)
            if(ieri.eq.-3) goto 3
            goto 93
          else
            call usrmsg('No profiles defined.','Choose add!','W')
            goto 3
          endif
        elseif(IRT.eq.3)then
          if(NGRDP.lt.MGRDP)then
            NGRDP=NGRDP+1
            ifoc=NGRDP
            goto 93
          else
            call usrmsg('Additional ground temperature profiles',
     &                  'not permitted in current model!','W')
            goto 3
          endif
        endif
   93   WRITE(HOLD,'(1X,6F7.2)')(UGRDTP(J,IFOC),J=1,6)
        CALL EASKS(HOLD,' ','Temperatures, January- thro June?',
     &      72,' ','gr prof jan-jun',IER,nbhelp)
        NV = iCountWords(HOLD)
        if(NV.ne.6)goto 93
        K=0
        DO 94 J=1,6
          CALL EGETWR(HOLD,K,GV,0.,0.,'-','profile',IER)
          IF(IER.NE.0)GOTO 93
          UGRDTP(J,IFOC)=GV
   94   CONTINUE
   95   hold = ' '
        WRITE(HOLD,'(1X,6F7.2)')(UGRDTP(J,IFOC),J=7,12)
        CALL EASKS(HOLD,' ','Temperatures, July thro December?',
     &      72,' ','gr prof jan-jun',IER,1)
        NV = iCountWords(HOLD)
        if(NV.ne.6)goto 95
        K=0
        DO 96 J=7,12
          CALL EGETWR(HOLD,K,GV,0.,0.,'-','profile',IER)
          IF(IER.NE.0)GOTO 95
          UGRDTP(J,IFOC)=GV
   96   CONTINUE
        MODSIT=.true.

      ELSEIF(INO.EQ.15)THEN

C 3-D Ground representations....
        call usrmsg('Please use program `grd` to define',
     &              '3D ground representations.','-')

      ELSEIF(INO.EQ.17)THEN

C Primary energy conversions.
        CALL EASKABC(' ','Energy demand conversion options:',
     &   'initialise and edit','edit','cancel',IR,nbhelp)
        if(IR.eq.1)then
          call peconv('i')
          call peconv('-')
        elseif(IR.eq.2)then
          call peconv('-')
        endif

      ELSEIF(INO.EQ.18)THEN

C Dispersed fan/pump/lift/DHW demands.
        call prjdmds(itrc,itru,iuo,ier)

      ELSEIF(INO.EQ.19)THEN

C IPV data in the case of a legacy model cfg file (depreciate).
        if(icfgv.lt.4)then
          lr=lnblnk(cfgroot)
          if(lnblnk(lipvdatf).eq.0)then
            write(ltmp,'(2a)') cfgroot(1:lr),'.ipv'
          elseif(lipvdatf(1:7).eq.'UNKNOWN')then
            write(ltmp,'(2a)') cfgroot(1:lr),'.ipv'
          elseif(lipvdatf(1:8).eq.'internal')then

C << really we should just recommend that user updates cfg version >>

C Legacy cfg with internl IPV description. If nipvassmt is still zero then
C nothing has been created yet so initialise and save to the
C configuration file. If there is data call ipvdat and the update the
C configuration file when exiting.
            if(nipvassmt.eq.0)then
              call ipvdat('i')     ! fill with defaults
              CALL EMKCFG('-',IER) ! remember this
              call ipvdat('-')     ! edit data
              CALL EMKCFG('-',IER) ! remember this
            else
              call ipvdat('-')
              CALL EMKCFG('-',IER)
            endif
          else
            ltmp=lipvdatf
          endif
 142      CALL EASKS(ltmp,' ','IPV description file?',
     &               72,'xxx.ipv','IPV file',IER,nbhelp)
          if(ltmp(1:2).ne.'  ')then
            lipvdatf=ltmp
            call FINDFIL(lipvdatf,XST)
            IF(XST)then
	      ipvact='ipv'
              call ripvdat(ifil+1,lipvdatf,ipvact,ier)
              call ipvdat('-')
            else
	      ipvact='ipv'
              call ipvdat('i')
              call mkipvdat(ifil+1,lipvdatf,ipvact)
              call ipvdat('-') 
            endif
          else
            call usrmsg('Please re-enter file name.',' ','W')
            goto 142
          endif
        else
	
C IPV data is held within the configurtion file. Reset the file
C name (since it is not used). If nipvassmt is still zero then
C nothing has been created yet so initialise and save to the
C configuration file. If there is data call ipvdat and the update the
C configuration file when exiting.
          lipvdatf='internal'
          if(nipvassmt.eq.0)then
            call ipvdat('i')     ! fill with defaults
            CALL EMKCFG('-',IER) ! remember this
            call ipvdat('-')     ! edit data
            CALL EMKCFG('-',IER) ! remember this
          else
            call ipvdat('-')
            CALL EMKCFG('-',IER)
          endif
        endif

      ELSEIF(INO.EQ.21)THEN

C Position specific pressure coef.
        call cpcdat

      ELSEIF(INO.EQ.22)THEN

C Temporal definitions.
        call setuptemporal(MODSIT,IER)
C          MODSIT=.true.

      ELSEIF(INO.EQ.24)THEN

C Manage images or browse by listing out current contents of image folder.
        call manageimages(modsit)

      ELSEIF(INO.eq.25)then

C Enter the definitions for UK NCM related data.
        call sbmedit(iuact,ier) ! generate stripped model and ncm file

      ELSEIF(INO.eq.26)then

C Changing orientation as a function of time.
C        CALL ROAMING
        CONTINUE
      ELSE
        GOTO 3
      ENDIF
      GOTO 3

      END

c ****** ROAMING *****
C Roaming manages data associated with rotating a model as a
C function of time. Important variables are defined in roam.h
      SUBROUTINE ROAMING
#include "building.h"
#include "model.h"
#include "roam.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN

      CHARACTER*72 DFLTNAME,OUTS
      CHARACTER TEMPSTR*30
      INTEGER IOK
      LOGICAL OK,DOK

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='ROAMING'  ! set for subroutine

C Initialise variables
      IOK=0
      OK=.FALSE.
      DOK=.TRUE.

C Help message
      helptopic='roaming_choices'
      call gethelptext(helpinsub,helptopic,nbhelp)
      WRITE(DFLTNAME,'(2A)')CFGROOT(1:LNBLNK(CFGROOT)),'.roam'
      IF(IROAM.NE.1)THEN
        LROAM=DFLTNAME
        IROAM=1
        CALL PHELPD('Roaming Introduction',nbhelp,'-',0,0,IER)
        CALL EASKAB(' ','Roaming file actions:',
     &    'create template','cancel and exit',IOK,nbhelp)
        IF(IOK.EQ.1)THEN
          CALL WROAM
          CALL RROAM
        ENDIF
      ELSE
        CALL EASKS2CMD(LROAM,' Roaming file? (see help)',
     &    ' ','dereference','browse',IACT,72,' ','Roaming file',
     &    IER,nbhelp)
        IF(IACT.EQ.1)THEN
          IROAM=0
          CALL EDISP(IUOUT,'Updating configuration file ...')
          CALL EMKCFG('-',IER)
        ELSE
          CALL RROAM
        ENDIF
      ENDIF

C Display file contents
      CALL ASKOK('View location information?',' ',OK,dok,nbhelp)
      IF(OK)THEN
        CALL EDISP(IUOUT,' ')
        WRITE(OUTS,'(A,F3.1)')'ESP-r Roaming file version number ',VERS
        CALL EDISP(IUOUT,OUTS)
        CALL EDISP(IUOUT,
     &  'Location                      Latitude Longitude')
        DO 1000 ILOC=1,NLOC
          WRITE(OUTS,'(A30,1X,F5.1,5X,F5.1)')
     &    LOCRM(ILOC),XLOCLT(ILOC),XLOCLG(ILOC)
          CALL EDISP(IUOUT,OUTS)
 1000   CONTINUE
        CALL ASKOK('View cruise schedule?',' ',OK,dok,nbhelp)
        IF(OK)THEN
          CALL EDISP(IUOUT,
     &    'start hr, dy, mth, orientation, day type, location')
          DO 1100 IPDR=1,NPDR
            IF(LLOCT(IPDR).EQ.0)THEN
              TEMPSTR='cruising'
            ELSE
              TEMPSTR=LOCRM(LLOCT(IPDR))
            ENDIF
            WRITE(OUTS,'(5X,F3.0,1X,I3,2X,I3,7X,I4,5X,I2,7X,A30)')
     &      XLHRS(IPDR),LDYS(IPDR),LMTS(IPDR),LORNT(IPDR),
     &      LDTP(IPDR),TEMPSTR
          CALL EDISP(IUOUT,OUTS)
 1100     CONTINUE
        ENDIF
        CALL EDISP(IUOUT,' ')
      ENDIF

C Update configuration file
      CALL EDISP(IUOUT,'Updating configuration file ...')
      CALL EMKCFG('-',IER)
      RETURN
      END

C ****** manageimages
C manageimages allows the user to manage images associated with a
C model including view and add images (delete not yet working).
      subroutine manageimages(modsit)
#include "building.h"
#include "model.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN
      common/user/browse

C Images.
      character imgfmt*4  ! GIF XBMP TIF JPG
      character imgfoc*4  ! FZON FNET FCTL FDFS
      character limgfil*72  ! file name (extend to 144 char)
      character imgdoc*248  ! text associated with image
      common/imagf/imgfmt(MIMG),imgfoc(MIMG),limgfil(MIMG),imgdoc(MIMG)

      integer noimg  ! number of images
      integer iton   ! zero if images not yet shown, one if yes
      common/imagfi/noimg,iton

      logical MODSIT,browse,unixok

C ITEMS is menu for site/context items, ITEMIMG is menu for editing
C and control of project images.
      character*33 ITEMIMG(20)
      character outs*124
      character limg*72,key*1,fs*1
      character iformat*4,ifocus*4
      character sfile*72,snpfile*72
      character tcname*248   ! for editing image notes
      integer nimgitms,INOI  ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='manageimages'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

  36  WRITE(ITEMIMG(1),'(a,i3,a)')'  current images ... (',noimg,')'
      M=1
      IF(noimg.GT.0)THEN
        M=M+1
        ITEMIMG(M)  ='  type focus file             '
        DO 35 IW=1,noimg
          M=M+1
          CALL EMKEY(M-2,KEY,IER)
          WRITE(ITEMIMG(M),'(a,1x,a,1x,a,1x,a)')KEY,imgfmt(IW),
     &      imgfoc(IW),limgfil(IW)(1:20)   
  35    CONTINUE
        ITEMIMG(M+1)=' _____________________________ '
      else
        ITEMIMG(M+1)=' _____________________________ '
      ENDIF
      ITEMIMG(M+2)='+ add/delete image               '
      ITEMIMG(M+3)='! display current images         '
      ITEMIMG(m+4)='? help                           '
      ITEMIMG(m+5)='- exit this menu                 '
      nimgitms=m+5

C Help text for menu.
      helptopic='model_image_management'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('Project Images',ITEMIMG,nimgitms,INOI)
      if(INOI.EQ.nimgitms)then

C Update configuration if images have changed.
        if(MODSIT.and.(.NOT.browse))then
          CALL EMKCFG('-',IER)
        endif
        return
      elseif(INOI.EQ.nimgitms-1)then
        helptopic='model_image_management'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Model images',nbhelp,'-',0,0,IER)
      elseif(INOI.EQ.nimgitms-2)then

C Display each of the current images by topic.
        call edisp(iuout,' ')
        call edisp(iuout,' General images...')
        if(noimg.gt.0)call imgdisp(1,'****',ier)
        call edisp(iuout,' Zone related images...')
        if(noimg.gt.0)call imgdisp(1,'FZON',ier)
        call edisp(iuout,' Network related images...')
        if(noimg.gt.0)call imgdisp(1,'FNET',ier)
        call edisp(iuout,' Control related images...')
        if(noimg.gt.0)call imgdisp(1,'FCTL',ier)
        call edisp(iuout,' Domain flow related images...')
        if(noimg.gt.0)call imgdisp(1,'FDFS',ier)
        call edisp(iuout,' Performance related images...')
        if(noimg.gt.0)call imgdisp(1,'FPER',ier)
        goto 36

      elseif(INOI.EQ.nimgitms-3)then

C Add images from images folder or other location.
        CALL EASKABCD(' ','Project image options:',
     &    'add from ../images folder','add from another folder',
     &    'delete','cancel',IOK,nbhelp)
        if(iok.eq.1)then
          if(noimg.ge.MIMG)then
            call edisp(iuout,'No more images can be associated with')
            call edisp(iuout,'the current model.')
            continue
          else

C Ask for names of files in the ../images folder.
            sfile=' '
            snpfile=' '
            call edisp(iuout,' ')
            call browsefilelist('?','img','fil',sfile,snpfile,nlist,
     &        iier)

C Try to use information gathered from file scan.
            if(nlist.gt.0)then
              sfile=' '
              snpfile=' '
              call browsefilelist('b','img','fil',sfile,snpfile,nlist,
     &          iier)
              if(snpfile(1:2).ne.'  ')then
                write(limg,'(3a)')imgpth(1:lnblnk(imgpth)),fs,
     &            snpfile(1:lnblnk(snpfile))
              else
                write(limg,'(a)')imgpth(1:lnblnk(imgpth))
              endif
            else
              write(limg,'(a)')imgpth(1:lnblnk(imgpth))
            endif
            iformat='GIF '
            ifocus='****'
            call edimage(limg,iformat,ifocus,iier)
            if(iier.eq.2)then
              continue
            elseif(iier.eq.0)then
              noimg=noimg+1
              limgfil(noimg)=limg
              imgfmt(noimg)=iformat
              imgfoc(noimg)=ifocus

C Edit image documentation if version supports it.
              if(icfgv.gt.3)then
                tcname=imgdoc(noimg)
                CALL EASKS248(tcname,' ','Image notes:',
     &            72,'associated image','image notes',IER,nbhelp)
                if(tcname(1:2).ne.'  ')imgdoc(noimg)=tcname
              endif
              modsit=.true.
            endif
          endif
        elseif(iok.eq.2)then
          if(noimg.ge.MIMG)then
            call edisp(iuout,'No more images can be associated with')
            call edisp(iuout,'the current model.')
            continue
          else
            limg=' '
            call easks(limg,'Image file name',' ',72,'xxx.gif',
     &        'image file name',ier,12)
            iformat='GIF '
            ifocus='****'
            call edimage(limg,iformat,ifocus,iier)
            if(iier.eq.0)then
              noimg=noimg+1
              limgfil(noimg)=limg
              imgfmt(noimg)=iformat
              imgfoc(noimg)=ifocus

C Edit image description if version supports it.
              if(icfgv.gt.3)then
                tcname=imgdoc(noimg)
                CALL EASKS248(tcname,' ','Image notes:',
     &            72,'associated image','image notes',IER,nbhelp)
                if(tcname(1:2).ne.'  ')imgdoc(noimg)=tcname
              endif
              modsit=.true.
              goto 36
            endif
          endif
        elseif(iok.eq.3)then
          DO 33 IW=1,noimg
            WRITE(outs,'(i2,1x,a,1x,a,1x,a)') iw,imgfmt(IW),
     &        imgfoc(IW),limgfil(IW) 
            call edisp(iuout,outs)  
  33      CONTINUE
          call usrmsg('Option to delete an image not yet available.',
     &                ' ','W')
          goto 36
        elseif(iok.eq.4)then
          goto 36
        endif
      elseif(INOI.EQ.1.and.noimg.gt.0)THEN
        goto 36
      elseif(inoi.gt.1.and.inoi.lt.nimgitms-4.and.noimg.gt.0)THEN

C Update the tags associated with the image. Remind the user about
C what they selected first.
        WRITE(outs,'(6a)') ' format ',imgfmt(inoi-2),
     &    ' focus ',imgfoc(inoi-2),' file ',limgfil(inoi-2) 
        call edisp(iuout,outs)  

        limg=limgfil(inoi-2)
        iformat=imgfmt(inoi-2)
        ifocus=imgfoc(inoi-2)
        call edimage(limg,iformat,ifocus,iier)
        if(iier.eq.2)then
          continue
        elseif(iier.eq.0)then
          limgfil(inoi-2)=limg
          imgfmt(inoi-2)=iformat
          imgfoc(inoi-2)=ifocus

C Edit current image description if version allows.
          if(icfgv.gt.3)then
            tcname=imgdoc(inoi-2)
            CALL EASKS248(tcname,' ','Image notes:',
     &        72,'associated image','image notes',IER,nbhelp)
            if(tcname(1:2).ne.'  ')imgdoc(inoi-2)=tcname
          endif
          MODSIT=.true.
        endif
      endif
      goto 36
      end

C ************* setuptemporal
C Setuptemporal coordinates access to and initial creation of
C temporal data to be associated with a model.

      SUBROUTINE setuptemporal(MODSIT,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "tdf2.h"
#include "control.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/user/browse
     
C TDF related.
      COMMON/TDFFLG0/DBTAG(MIT),DBTASK(MIT),DBZN(MIT),DBSN(MIT)

      common/sctl/tcps,ictyp,iclaw,cm(misc)
      common/cctl/icascf(mcom)

      logical MODSIT,browse,OK,dok,unixok
      logical FOUND,XST

      character outs*124,ltmp*72,hold*40
      character fs*1,msgl2*48
      character*72 DLTDF
      character DBTAG*12,DBTASK*12,DBZN*15,DBSN*15
      character OUTSTR*124,HDR*12
      character*34 ITEMS(7)
      integer ier
      integer nitms,irt ! max items and current menu item

C Action to pass to tdfedit. Prior 'update' option moved into
C esrub2e rather than invoking tdf.
       character action*16

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='setuptemporal'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Default name for temporal file. For all cases generate default name.
      if(netpth(1:2).eq.'  '.or.netpth(1:2).eq.'./')then
        WRITE(DLTDF,'(2a)')cfgroot(1:lnblnk(cfgroot)),'_temporal.tdfa'
      else
        WRITE(DLTDF,'(4a)') netpth(1:lnblnk(netpth)),fs,
     &    cfgroot(1:lnblnk(cfgroot)),'_temporal.tdfa'
      endif

C Temporal definitions. Initial scan of configuration file would
C determine if the state of the file:
      if(iabs(itdflg).eq.0)then
C        call edisp(iuout,' ')
C        call edisp(iuout,'No temporal data has been defined.')
C        call edisp(iuout,' ')
      elseif(iabs(itdflg).eq.1)then

C If binary must convert before listing what is available in temporal
C file, what is currently used and other management tasks.
        call edisp(iuout,' ')
        call edisp(iuout,'Temporal data is available for this model')
        call edisp(iuout,'but is held in an old format. Please')
        call edisp(iuout,'update via the edit/list option.')
        call edisp(iuout,' ')
      elseif(iabs(itdflg).eq.2)then

C If binary must convert before listing what is available in temporal
C file, what is currently used and other management tasks.
        call edisp(iuout,' ')
        call edisp(iuout,'Temporal data is available for this model.')
        call edisp(iuout,'but is held in an old format. Please')
        call edisp(iuout,'udate via the edit/list option.')
        call edisp(iuout,' ')
      elseif(iabs(itdflg).eq.3)then
        continue
      endif

C Take into account if the user is browsing or can modify the files
C and present a different interface depending on file type.
 42   helptopic='temporal_setup_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Begin with high level menu.
    3 IER=0
      IRT=-1

      if(itdflg.eq.0)then
        if(browse)then
          call usrmsg(
     &   'No temporal data associated with this model! Because you',
     &   'are in browse mode no changes may be made to the model.','W')
          return
        endif
        WRITE(ITEMS(1),'(A)')'a create temporal entities file'
        WRITE(ITEMS(2),'(A)')'b select temporal file '
        WRITE(ITEMS(3),'(A)')'c not applicable '
        WRITE(ITEMS(4),'(A)')'? help '
        WRITE(ITEMS(5),'(A)')'- exit '
        nitms=5
        CALL EMENU('Temporal file options:',ITEMS,nitms,irt)
        if(irt.EQ.nitms)then
          return
        elseif(irt.EQ.nitms-1)then
          helptopic='temporal_setup_menu'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('temporal help',nbhelp,'-',0,0,IER)
          goto 3
        elseif(irt.EQ.3)then
          goto 3
        elseif(irt.EQ.0)then
          goto 3
        endif
      elseif(iabs(itdflg).eq.1)then
        if(browse)then
          call usrmsg(
     &    'The temporal data is held in an outdated format. You must',
     &    '`own` the model in order to update it.','W')
          return
        endif
        WRITE(ITEMS(1),'(A)')'a view/edit temporal entities'
        WRITE(ITEMS(2),'(A)')'b select temporal file '
        WRITE(ITEMS(3),'(A)')'c dereference temporal file'
        WRITE(ITEMS(4),'(A)')'d list associations'
        WRITE(ITEMS(5),'(A)')'e manage associations'
        WRITE(ITEMS(6),'(A)')'? help '
        WRITE(ITEMS(7),'(A)')'- exit '
        nitms=7
        CALL EMENU('Temporal file options:',ITEMS,nitms,irt)
        if(irt.EQ.nitms)then
          return
        elseif(irt.EQ.nitms-1)then
          CALL PHELPD('temporal help',nbhelp,'-',0,0,IER)
          goto 3
        elseif(irt.EQ.0)then
          goto 3
        endif
      elseif(iabs(itdflg).eq.2)then
        if(browse)then
          WRITE(ITEMS(1),'(A)')'a view temporal entities'
          WRITE(ITEMS(2),'(A)')'b not applicable'
          WRITE(ITEMS(3),'(A)')'c list associations'
          WRITE(ITEMS(4),'(A)')'? help '
          WRITE(ITEMS(5),'(A)')'- exit '
          nitms=5
          CALL EMENU('Temporal file options:',ITEMS,nitms,irt)
          if(irt.EQ.nitms)then
            return
          elseif(irt.EQ.nitms-1)then
            CALL PHELPD('temporal help',nbhelp,'-',0,0,IER)
            goto 3
          elseif(irt.EQ.2)then
            goto 3
          elseif(irt.EQ.0)then
            goto 3
          endif
        else
          WRITE(ITEMS(1),'(A)')'a view/edit temporal entities'
          WRITE(ITEMS(2),'(A)')'b select temporal file '
          WRITE(ITEMS(3),'(A)')'c dereference temporal file'
          WRITE(ITEMS(4),'(A)')'d list associations'
          WRITE(ITEMS(5),'(A)')'e manage associations'
          WRITE(ITEMS(6),'(A)')'? help '
          WRITE(ITEMS(7),'(A)')'- exit '
          nitms=7
          CALL EMENU('Temporal file options:',ITEMS,nitms,irt)
          if(irt.EQ.nitms)then
            return
          elseif(irt.EQ.nitms-1)then
            CALL PHELPD('temporal help',nbhelp,'-',0,0,IER)
            goto 3
          elseif(irt.EQ.0)then
            goto 3
          endif
        endif
      elseif(iabs(itdflg).eq.3)then
        if(browse)then
          WRITE(ITEMS(1),'(A)')'a view temporal entities'
          WRITE(ITEMS(2),'(A)')'b not applicable'
          WRITE(ITEMS(3),'(A)')'c list associations'
          WRITE(ITEMS(4),'(A)')'? help '
          WRITE(ITEMS(5),'(A)')'- exit '
          nitms=5
          CALL EMENU('Temporal file options:',ITEMS,nitms,irt)
          if(irt.EQ.nitms)then
            return
          elseif(irt.EQ.nitms-1)then
            CALL PHELPD('temporal help',nbhelp,'-',0,0,IER)
            goto 3
          elseif(irt.EQ.2)then
            goto 3
          elseif(irt.EQ.0)then
            goto 3
          endif
        else
          WRITE(ITEMS(1),'(A)')'a view/edit temporal entities'
          WRITE(ITEMS(2),'(A)')'b select temporal file '
          WRITE(ITEMS(3),'(A)')'c dereference temporal file'
          WRITE(ITEMS(4),'(A)')'d list associations'
          WRITE(ITEMS(5),'(A)')'e manage associations'
          WRITE(ITEMS(6),'(A)')'? help '
          WRITE(ITEMS(7),'(A)')'- exit '
          nitms=7
          CALL EMENU('Temporal file options:',ITEMS,nitms,irt)
          if(irt.EQ.nitms)then
            return
          elseif(irt.EQ.nitms-1)then
            CALL PHELPD('temporal help',nbhelp,'-',0,0,IER)
            goto 3
          elseif(irt.EQ.0)then
            goto 3
          endif
        endif
      endif
      IF(IRT.LT.1.OR.IRT.GT.7)GOTO 3
  27  continue
      if(IRT.eq.1)then

C If browsing do not bother to confim the file name.
        if(browse)then
          write(outs,'(2a)')' Temporal definitions ',LTDFA
          call edisp(iuout,outs)
        else

C If LTDFA is still UNKNOWN set to default name.
          if(LTDFA(1:4).eq.'UNKN') LTDFA=DLTDF
          ltmp=LTDFA
          CALL EASKS(ltmp,' ','Temporal definitions file?',
     &      72,DLTDF,'temporal definitions file',
     &      IER,nbhelp)
          if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
            LTDFA=ltmp
            MODSIT=.true.
          endif
        endif

C << if creating a new file assist by checking whether there
C << are simulation parameter sets and startup days to guide
C << the user

C << might also be a place to generate the initial lines of
C << asci header with the year of the model etc.

C Go into the tdf editing menu.
        action='-'
        call tdfedit(action,ier)
        goto 42
      elseif(IRT.eq.2)then

C User supplies name of file. Find out its contents (using similar code
C to ersys) and jump back to the start of this subroutine.

C << later add add a file browse function >>
        if(LTDFA(1:4).eq.'UNKN') LTDFA=DLTDF
        ltmp=LTDFA
        CALL EASKS(ltmp,' ','Temporal definitions file?',
     &    72,DLTDF,'temporal definitions file',IER,nbhelp)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          continue
        else
          goto 42
        endif
        LTDFA=ltmp
        MODSIT=.true.
        CALL ERPFREE(IUTDF,ISTAT)
        call FINDFIL(LTDFA,XST)
        if(.NOT.XST)then
          goto 42
        endif

C Try to open as ascii file first. If that fails try as a binary file.
        CALL EFOPSEQ(IUTDF,LTDFA,1,IER)
        if(ier.eq.0)then
          CALL STRIPC(IUTDF,outstr,99,ND,1,'header',IER)
          if(OUTSTR(1:9).eq.'ASCIITDF3')THEN
            ITDFLG=3
          elseif(OUTSTR(1:9).eq.'ASCIITDF2')THEN
            ITDFLG=2
          elseif(OUTSTR(1:12).eq.'TDFdatabase2')THEN
            ITDFLG=1
          else

C Might have reached this point because it was a binary file.
C Scan the first record of this file.
            CALL ERPFREE(IUTDF,ISTAT)
            NWPR=MTABC
            ITWPR=NWPR+1
            ier=0
            call EFOPRAN(iutdf,LTDFA,ITWPR,1,IER)
            IREC=1
            READ(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=103)HDR,NWPR
            if(HDR(1:12).eq.'TDFdatabase2')THEN
              ITDFLG= -2
            elseif(HDR(1:12).eq.'TDFdatabase3')THEN
              ITDFLG= -3
            else
              ITDFLG=0
            endif
            CALL ERPFREE(IUTDF,ISTAT)
            if(iabs(itdflg).gt.1)then
              call usrmsg('Including temporal file in model...',
     &          ' ','-')
              CALL EMKCFG('-',IER)
              call usrmsg('Including temporal file in model...done.',
     &          ' ','-')
            endif
            goto 42
 103        msgl2='in the temporal file'
            CALL USRMSG(' could not read header record 1',msgl2,'W')
            CALL ERPFREE(IUTDF,ISTAT)
            ITDFLG=0
          endif
        else
          CALL ERPFREE(IUTDF,ISTAT)
          NWPR=MTABC
          ITWPR=NWPR+1
          ier=0
          call EFOPRAN(iutdf,LTDFA,ITWPR,1,IER)
          IREC=1
          READ(IUTDF,REC=IREC,IOSTAT=ISTAT,ERR=102)HDR,NWPR
          if(HDR(1:12).eq.'TDFdatabase2')THEN
            ITDFLG= -2
          elseif(HDR(1:12).eq.'TDFdatabase3')THEN
            ITDFLG= -3
          else
            ITDFLG=0
          endif
          CALL ERPFREE(IUTDF,ISTAT)
          if(iabs(itdflg).gt.1)then
            call usrmsg('Including temporal file in model...',
     &        ' ','-')
            CALL EMKCFG('-',IER)
            call usrmsg('Including temporal file in model...done.',
     &        ' ','-')
          endif
          goto 42
 102      msgl2='in the temporal file'
          CALL USRMSG(' could not read header record 1',msgl2,'W')
          CALL ERPFREE(IUTDF,ISTAT)
          ITDFLG=0
        endif
        goto 42
      elseif(IRT.eq.3)then

C Dereference or ignore, depending on the file type.
        if(itdflg.eq.0)then
          continue
        else
          LTDF='UNKNOWN'
          LTDFA='UNKNOWN'
          call clrtabc
          itdflg=0
          NITDF=0
          call usrmsg('Removing temporal references...',' ','-')
          CALL EMKCFG('-',IER)
          call usrmsg('Removing temporal references...done.',' ','-')
          MODSIT=.false.
          return
        endif
      elseif(IRT.eq.4)then

C List associations.
        if(itdflg.eq.0)then
          continue
        else
          if(ITEMSTD.gt.0)then
            call edisp(iuout,' ')
            call edisp(iuout,' Temporal entities currently used...')
            call edisp(iuout,'  ________________________________')
            call edisp(iuout,'  |temporal    |generic |associated')
            call edisp(iuout,
     &           '  |entity name |type    |with   zone &     surface ')
            do 28, l=1,ITEMSTD
              WRITE(outs,29)l,DBTAG(L),DBTASK(L),DBZN(L),DBSN(L)
   29         FORMAT(i2,1X,A,1X,A,5X,A,A)
              call edisp(iuout,outs)
  28        continue
            call edisp(iuout,' ')
          else
            call edisp(iuout,'Currently no temporal items are used.')
          endif
        endif
        goto 42
      elseif(IRT.eq.5)then

C Use/manage entity associations.
        if(itdflg.eq.0)then
          call usrmsg(
     &    'No temporal file yet. Can only create associations with',
     &    'existing entities. ','W')
          goto 42
        endif

        ltmp=LTDFA
        CALL EASKS(ltmp,' Temporal definitions file name?',
     &    ' ',72,DLTDF,'temporal definitions db name',IER,nbhelp)
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then

C Depending on its type read the header of the temporal file.
          LTDFA=ltmp
          MODSIT=.true.
          call supplyandcheck(ltdfa,'P',ier)
          IF(IER.NE.0)then
            call usrmsg('Problem opening ',LTDFA,'W')
            CALL ERPFREE(iutdfa,ISTAT)
            return
          endif
        else
          CALL ERPFREE(iutdfa,ISTAT)
          return
        endif

C Based on header advise user their current options.
        CALL ERPFREE(iutdfa,ISTAT)
            
C Befor management option list current db items and then associations.
 128    call edisp(iuout,' ______Temporal file contents__________')
        call edisp(iuout,' |temporal     |generic |description   ')
        call edisp(iuout,' |entity name  |type    |              ')
        M=0
        if(NITDF.eq.0)then
          call edisp(iuout,' No temporal items to select!')
          return
        endif
        DO 10 L=1,NITDF
          M=M+1
          WRITE(outs,14)M,TAG(L),TTYPE(L),TMENU(L)
   14     FORMAT(i2,1X,A,1X,A,1X,A)
          call edisp(iuout,outs)
   10   CONTINUE
        if(ITEMSTD.gt.0)then
          call edisp(iuout,' ')
          call edisp(iuout,'  _____Temporal entities used_____')
          call edisp(iuout,'  |temporal    |generic |associated')
          call edisp(iuout,
     &           '  |entity name |type    |with   zone &     surface ')
          do 228, l=1,ITEMSTD
            WRITE(outs,29)l,DBTAG(L),DBTASK(L),DBZN(L),DBSN(L)
            call edisp(iuout,outs)
  228     continue
          call edisp(iuout,' ')
        else
          call edisp(iuout,'Currently no temporal items are used.')
        endif

C Manage the list of DBTAG/DBTASK/DBZN/DBSN.
        itro=0
        if(ITEMSTD.eq.0)then
          call easkabc('Temporal association options:',' ',
     &      'n/a','add association','continue',ITRO,nbhelp)
          if(itro.eq.3) goto 42
        else
          call easkabcd('Temporal association options:',' ',
     &      'delete association','add association','edit association',
     &      'continue',ITRO,nbhelp)
          if(itro.eq.4) goto 42
        endif
        if(itro.eq.1)then

C Delete an association and move others up.
          CALL EASKI(it,'Temporal item which you do not want to use',
     &      'in this model?',1,'F',itemstd,'F',1,'selec index',
     &      IERI,nbhelp)
          if(ieri.eq.-3) goto 42

          if(it.eq.0)then
            goto 42
          elseif(it.eq.itemstd)then
            continue
          else
            do 791 idv=it,itemstd-1
              DBTAG(idv)=DBTAG(idv+1)
              DBTASK(idv)=DBTASK(idv+1)
              DBZN(idv)=DBZN(idv+1)
              DBSN(idv)=DBSN(idv+1)
  791       continue
          endif
          ITEMSTD=ITEMSTD-1
          MODSIT=.true.
          call usrmsg('Compacting temporal references...',' ','-')
          CALL EMKCFG('-',IER)
          call usrmsg('Compacting temporal references...done.',' ','-')
          MODSIT=.false.
        elseif(itro.eq.2.or.itro.eq.3)then

C Add association or edit current association.
          if(itro.eq.3)then
            CALL EASKI(ite,'Which temporal association to modify?',
     &        '(index)',1,'F',itemstd,'F',1,'selec tdf assoc',
     &        IERI,nbhelp)
            if(ieri.eq.-3) goto 42
            if(ite.ne.0)then

C Reset itemstd to the item to be edited.
              call edisp(iuout,'The association details are:')
              WRITE(outs,29)ite,DBTAG(ite),DBTASK(ite),DBZN(ite),
     &             DBSN(ite)
              call edisp(iuout,outs)
              CALL EASKI(it,' ','Index of associated temporal item?',
     &              1,'F',NITDF,'F',1,'selec tdf index',IERI,nbhelp)
              if(ieri.eq.-3) goto 42
            endif
          elseif(itro.eq.2)then
            CALL EASKI(it,'Temporal item index to associate with',
     &        'this model?',1,'F',NITDF,'F',1,'selec tdf index',
     &        IERI,nbhelp)
            if(ieri.eq.-3) goto 42
            ITEMSTD=ITEMSTD+1
            ite=ITEMSTD
          endif
          DBTAG(ITE)=TAG(it)
          DBTASK(ITE)=TTYPE(it)

C Depending on the type of the data, ask for associated zone.
          if(TTYPE(it)(1:6).eq.'CASUAL')then
            IC=-1
 250        izdef=0
            call askzone(ic,izdef,' Related zone for casual gains?',
     &      'Related zone','-','Casual gains requires a zone.',ier)
            IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 250
            write(DBZN(ITE),'(a)') zname(IC)(1:12)
            DBSN(ITE)='ALL' 
          elseif(TTYPE(it)(1:6).eq.'ZIVENT')then

C Use DBZN to indicate that tdf ventilation associated with a zone.
C << need to support the no ventilation case? >>
            IC=-1
 251        izdef=0
            call askzone(ic,izdef,' Related zone for ventilation?',
     &        'Related zone','-','Ventilation requires a zone.',ier)
            IF(IC.EQ.-1)GOTO 251
            if(ic.eq.0)then
              write(DBZN(ITE),'(a)') 'none        '
            else
              write(DBZN(ITE),'(a)') zname(IC)(1:12)
            endif
            DBSN(ITE)='ALL'            
          elseif(TTYPE(it)(1:6).eq.'RAIRVL')then

C << Think about this, it might need to know about which surfaces
C << the velocity should be associated with an outside surface?
            IC=-1
 252        izdef=0
            call askzone(ic,izdef,' Related zone for air velocity?',
     &        'Related zone','-','Air velocity requires a zone.',ier)
            IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 252
            write(DBZN(ITE),'(a)') zname(IC)(1:12)
            DBSN(ITE)='ALL'
          elseif(TTYPE(it)(1:6).eq.'CTLSTA')then

C << Think about what this could be used for...if could be a number
C << between 0.0 and 1.0 giving ON fraction.
            IC=-1
 254        izdef=0
            call askzone(ic,izdef,' Related zone for control state?',
     &        'Related zone','-','Set point requires a zone.',ier)
            IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 254
            write(DBZN(ITE),'(a)') zname(IC)(1:12)
            DBSN(ITE)='ALL'
          elseif(TTYPE(it)(1:6).eq.'ACTIVI'.or.
     &           TTYPE(it)(1:6).eq.'HTCLSE')then

C Depending on the type of the data, ask for associated zone.
            IC=-1
 1252       izdef=0
            call askzone(ic,izdef,' Related zone for activity db?',
     &      'Related zone','-','Activity db can be assigned to a zone.',
     &       ier)
            IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 1252
            write(DBZN(ITE),'(a)') zname(IC)(1:12)

C If there is no control file warn the user that one will be added.
            if(ncf.eq.0)then
              call usrmsg(
     &        'There is no zone control file (or it has been.',
     &        'dereferenced). One will be defined automatically.','W')
            endif

C Ask the user whether there has already been a control law defined
C to use the setpoint. If so report on that control loop and offer
C selected information to alter. Contol law 23 is the one used for this
C temporal definition entity
            write(outs,'(a,i2,a)')' There are currently ',ncf,
     &        ' control loops.'
            call edisp(iuout,outs)
            call edisp(iuout,'  ')
            found=.false.
            call edisp(iuout,
     &  'Controls might be suitable for use with temporal data.')
            do 777 ijj=1,ncf
              if(ibclaw(ijj,1,1).eq.23)found=.true.
              call LSTCNTL(iuout,0,ijj)
 777        continue
            if(found)then

C If there are possible control laws and if the user selects one of them
C copy data into cm variables for later editing.
              helptopic='temporal_ctl_linkage'
              call gethelptext(helpinsub,helptopic,nbhelp)
              irtc=0
              CALL EASKABC(' Control linkage options:',' ',
     &          'use existing control','create new control','continue',
     &          IRTC,nbhelp)
              if(irtc.eq.1)then
                CALL EASKI(ispl,'Control loop to use set point data?',
     &            ' ',1,'F',ncf,'F',1,'cntrl loop for set point',
     &            IERI,nbhelp)
                if(ieri.eq.-3) goto 42
                izdef=iban(ispl,1)
                ibsn(ispl,1)=izdef
                ibsn(ispl,2)=0
                ibsn(ispl,3)=0
                ibsn(ispl,4)=0
                cm(2)=bmiscd(ispl,1,1,2)
                cm(3)=bmiscd(ispl,1,1,3)
              elseif(irtc.eq.3)then
                goto 42
              endif
            else

C If no control loop has been defined to associate with this temporal
C setpoint so create another one, fill in as much information as
C possible and link back to the temporal item. Increment ncf.
              call edisp(iuout,'No applicable controls found.')
              irtc=0
              CALL EASKABC(' Control linkage options:',' ',
     &          'n/a','create new control','cancel',IRTC,nbhelp)
              if(irtc.eq.1.or.irtc.eq.3)then
                goto 42
              endif
            endif

C Add another controls
            if(irtc.eq.2)then
              ispl=ncf+1
              ncf=ncf+1
              ibsn(ispl,1)=0
              ibsn(ispl,2)=0
              ibsn(ispl,3)=0
              ibsn(ispl,4)=0
              if(TTYPE(it)(1:6).eq.'ACTIVI')then
                cm(2)=5.0
                cm(3)=6.0
              elseif(TTYPE(it)(1:6).eq.'HTCLSE')then
                cm(2)=2.0
                cm(3)=3.0
              endif         
            endif

            ibsn(ispl,1)=IC
            iban(ispl,1)=IC
            iban(ispl,2)=0
            iban(ispl,3)=0
            nbcdt(ispl)=1
            ibcdv(ispl,1,1)=1
            ibcdv(ispl,1,2)=365
            nbcdp(ispl,1)=1
            tbcps(ispl,1,1)=0.0
            ibctyp(ispl,1,1)=0
            ibclaw(ispl,1,1)=23
            cm(1)=2.0

C Confirm capacities.
  553       WRITE(HOLD,'(2f8.0)')cm(2),cm(3)
            if(TTYPE(it)(1:6).eq.'ACTIVI')then
              CALL EASKS(HOLD,
     &        'Heating and cooling setpoint column number ',
     &        '(respectively):',40,' 5.0 6.0 ',
     &        ' track another column number(s)',IER,nbhelp)
            elseif(TTYPE(it)(1:6).eq.'HTCLSE')then     
              CALL EASKS(HOLD,
     &        'Heating and cooling setpoint column number ',
     &        '(respectively):',40,' 2.0 3.0 ',
     &        ' track another column number(s)',IER,nbhelp)
            endif    
            K=0
            CALL EGETWR(HOLD,K,cm(2),0.,80.,'F','Max columns heat',IER)
            CALL EGETWR(HOLD,K,cm(3),0.,80.,'F','Max columns cool',IER)
            if(ier.ne.0)goto 553

C Update the control period data.
            bmiscd(ispl,1,1,1)=cm(1)
            bmiscd(ispl,1,1,2)=cm(2)
            bmiscd(ispl,1,1,3)=cm(3)                    

C Update the control file and link the control loop to the correct zone.
            icascf(IC)=ispl
            ICTLF=IFIL+1
            call usrmsg('Updating model control for temporal link...',
     &        ' ','-')
            CALL CTLWRT(ICTLF,IER)
            CALL EMKCFG('-',IER)
            call usrmsg(
     &        'Updating model control for temporal link...done.',
     &        ' ','-')
            DBSN(ITE)='ALL'
          elseif(TTYPE(it)(1:6).eq.'SETPTT')then

C If there is no control file warn the user to create one first.
            if(ncf.eq.0)then
              call usrmsg(
     &        'There is no zone control file. Please define one',
     &        'before you create or use a link to temporal data.','W')
              goto 42
            endif

C Present a summary of the steps needed to create a temporal-control link.
            helptopic='temporal_ctl_setpoints'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL PHELPD('temporal-control popup',nbhelp,'-',0,0,IER)

C Ask the user whether there has already been a control law defined
C to use the setpoint. If so report on that control loop and offer
C selected information to alter. Either controls with a sensor
C first item equal to -5 or a control with a type 11 (match sensor)
C control law in the first period.
            write(outs,'(a,i2,a)')' There are currently ',ncf,
     &        ' control loops.'
            call edisp(iuout,outs)
            call edisp(iuout,'  ')
            found=.false.
            call edisp(iuout,
     &  'Controls might be suitable for use with temporal data.')
            do 77 ijj=1,ncf
              if(ibsn(ijj,1).eq.-5)then
                found=.true.
                call LSTCNTL(iuout,0,ijj)
              elseif(ibclaw(ijj,1,1).eq.11)then
                found=.true.
                call LSTCNTL(iuout,0,ijj)
              endif
  77        continue
            if(found)then

C If there are possible control laws and if the user selects one of them
C copy data into cm variables for later editing.
              irtc=0
              CALL EASKABC(' Control linkage options:',' ',
     &          'use existing control','create new control','continue',
     &          IRTC,nbhelp)
              if(irtc.eq.1)then
                CALL EASKI(ispl,'Control loop to use set point data?',
     &            ' ',1,'F',ncf,'F',1,'cntrl loop for set point',
     &            IERI,nbhelp)
                if(ieri.eq.-3) goto 42
                izdef=iban(ispl,1)
                ibsn(ispl,1)=izdef
                ibsn(ispl,2)=0
                ibsn(ispl,3)=0
                ibsn(ispl,4)=0
                cm(2)=bmiscd(ispl,1,1,2)
                cm(3)=bmiscd(ispl,1,1,3)
                cm(4)=bmiscd(ispl,1,1,4)
                cm(5)=bmiscd(ispl,1,1,5)
                cm(6)=bmiscd(ispl,1,1,6)
                cm(7)=bmiscd(ispl,1,1,7)
                cm(8)=bmiscd(ispl,1,1,8)
                cm(9)=bmiscd(ispl,1,1,9)
                cm(10)=bmiscd(ispl,1,1,10)
                cm(11)=bmiscd(ispl,1,1,11)
                cm(12)=bmiscd(ispl,1,1,12)
                cm(13)=bmiscd(ispl,1,1,13)
              elseif(irtc.eq.3)then
                goto 42
              endif
            else

C If no control loop has been defined to associate with this temporal
C setpoint so create another one, fill in as much information as
C possible and link back to the temporal item. Increment ncf.
              call edisp(iuout,'No applicable controls found.')
              irtc=0
              CALL EASKABC(' Control linkage options:',' ',
     &          'n/a','create new control','cancel',IRTC,nbhelp)
              if(irtc.eq.1.or.irtc.eq.3)then
                goto 42
              endif
            endif

C Add another controls
            if(irtc.eq.2)then
              ispl=ncf+1
              ncf=ncf+1
              ibsn(ispl,1)=0
              ibsn(ispl,2)=0
              ibsn(ispl,3)=0
              ibsn(ispl,4)=0
              cm(2)=5000.0
              cm(3)=0.0
              cm(4)=5000.0
              cm(5)=0.0
              izdef=0
            endif
            if(ispl.le.9)then
              write(DBZN(ITE),'(a5,i1)') 'loop_',ispl
            else
              write(DBZN(ITE),'(a5,i2)') 'loop_',ispl
            endif

C Confirm which zone to use with this control. Then instanciate
C all of the usual values for control-temporal links. Note that
C even though cm(8) - cm(13) does not seem to be used set them.
            IC=-1
 255        call askzone(ic,izdef,'Related zone for temporal setpoint?',
     &        'Related zone','-','Set point requires a zone.',ier)
            IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 255
            ibsn(ispl,1)=IC
            iban(ispl,1)=IC
            iban(ispl,2)=0
            iban(ispl,3)=0
            nbcdt(ispl)=1
            ibcdv(ispl,1,1)=1
            ibcdv(ispl,1,2)=365
            nbcdp(ispl,1)=1
            tbcps(ispl,1,1)=0.0
            ibctyp(ispl,1,1)=0
            ibclaw(ispl,1,1)=11
            cm(1)=12.0
            cm(6)=1.0
            cm(7)=3.0
            cm(8)=-5.0
            cm(9)=ITE
            cm(10)=0.0
            cm(11)=0.0
            cm(12)=1.0
            cm(13)=0.0

C Confirm capacities.
  53        WRITE(HOLD,'(4f8.0)')cm(2),cm(3),cm(4),cm(5)
            CALL EASKS(HOLD,
     &      ' Max & Min heat capacity, Max & Min cool capacity for',
     &      ' matching another temp: ',40,' 10000. 0.0 10000. 0.0 ',
     &      ' track another max min heat cool',IER,nbhelp)
            K=0
            CALL EGETWR(HOLD,K,cm(2),0.,999999.,'F','Max Heat',IER)
            CALL EGETWR(HOLD,K,cm(3),0.,999999.,'F','Min Heat',IER)
            CALL EGETWR(HOLD,K,cm(4),0.,999999.,'F','Max Cool',IER)
            CALL EGETWR(HOLD,K,cm(5),0.,999999.,'F','Min Cool',IER)
            if(ier.ne.0)goto 53

C Update the control period data.
            bmiscd(ispl,1,1,1)=cm(1)
            bmiscd(ispl,1,1,2)=cm(2)
            bmiscd(ispl,1,1,3)=cm(3)
            bmiscd(ispl,1,1,4)=cm(4)
            bmiscd(ispl,1,1,5)=cm(5)
            bmiscd(ispl,1,1,6)=cm(6)
            bmiscd(ispl,1,1,7)=cm(7)
            bmiscd(ispl,1,1,8)=cm(8)
            bmiscd(ispl,1,1,9)=cm(9)
            bmiscd(ispl,1,1,10)=cm(10)
            bmiscd(ispl,1,1,11)=cm(11)
            bmiscd(ispl,1,1,12)=cm(12)
            bmiscd(ispl,1,1,13)=cm(13)

C Update the control file and link the control loop to the correct zone.
            icascf(IC)=ispl
            ICTLF=IFIL+1
            call usrmsg('Updating model control for temporal link...',
     &        ' ','-')
            CALL CTLWRT(ICTLF,IER)
            CALL EMKCFG('-',IER)
            call usrmsg(
     &        'Updating model control for temporal link...done.',
     &        ' ','-')
            DBSN(ITE)='ALL'
          elseif(TTYPE(it)(1:8).eq.'BIDIRSET')then

C Depending on the type of data, ask for associated surface 
            IC=-1
 352        izdef=0
            call askzone(ic,izdef,
     &        ' Related zone for bidirectional data set?',
     &        'Related zone','-','Optical sets require a zone.',ier)
            IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 352
            write(DBZN(ITE),'(a)') zname(IC)(1:12)
            call easksur(IC,ISUR,'-',
     &           'Select surface for bidirectional dataset',' ',IER)
            IF(ISUR.EQ.0.OR.ISUR.EQ.-1)GOTO 352
            write(DBSN(ITE),'(a)') sname(IC,ISUR)(1:12)
          elseif(TTYPE(it)(1:7).eq.'VERTSOL')then

C Depending on the type of data, ask for associated surface 
            IC=-1
 353        izdef=0
            call askzone(ic,izdef,
     &        ' Related zone where vertical solar data apply?',
     &        'Related zone','-','zone to assign solar data.',ier)
            IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 353
            write(DBZN(ITE),'(a)') zname(IC)(1:12)
            call easksur(IC,ISUR,'-',
     &           'Select surface to apply vertical solar data',' ',IER)
            IF(ISUR.EQ.0.OR.ISUR.EQ.-1)GOTO 353
            write(DBSN(ITE),'(a)') sname(IC,ISUR)(1:12)
          elseif(TTYPE(it)(1:6).eq.'DBTZNM')then

C Measured db T (probably associated with a zone).
            IC=-1
 354        izdef=0
            call askzone(ic,izdef,' Related zone for measured db T?',
     &      'Related zone','-','Measured db T for which zone?',ier)
            IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 354
            write(DBZN(ITE),'(a)') zname(IC)(1:12)
            DBSN(ITE)='ALL' 
          elseif(TTYPE(it)(1:5).eq.'SURTM')then

C Measured surface T (associated with a zone and up to 6 surfaces).
            IC=-1
 355        izdef=0
            call askzone(ic,izdef,'Related zone for measured surf T?',
     &        'Related zone','-',
     &        'Measured surface T within which zone?',ier)
            IF(IC.EQ.0.OR.IC.EQ.-1)GOTO 355
            write(DBZN(ITE),'(a)') zname(IC)(1:12)
            call easksur(IC,ISUR,'-',
     &        'Select surface associated with measured temperature',
     &        ' ',IER)
            IF(ISUR.EQ.0.OR.ISUR.EQ.-1)GOTO 355
            write(DBSN(ITE),'(a)') sname(IC,ISUR)(1:12) 
          else
            DBZN(ITE)='ALL'
            DBSN(ITE)='ALL'            
          endif
          if(itro.eq.2)then
            dok=.false.
            CALL ASKOK(' ','Use another temporal item?',OK,DOK,nbhelp)
            if(OK)then
              IRT=4
              goto 27
            endif
          endif
          call usrmsg('Updating temporal references...',' ','P')
          CALL EMKCFG('-',IER)
          call usrmsg('Updating temporal references...done.',' ','-')
        endif 
        goto 128
      endif

C Close the tdf file..
      CALL ERPFREE(iutdfa,ISTAT)
      return
      end

C ************* PECONV
C Conversion factors for conversion to primary energy units.
C Conversion from demand units TO primary energy units are:
C  pcnvht = multiplier for plant heating
C  pcnvcl = multiplier for plant cooling
C  pcnvlt = multiplier for lighting casual gain
C  pcnvfn = multiplier for fan/pump casual gain
C  pcnvsp = multiplier for small power casual gain
C  pcnvhw = multiplier for hot water
C  phtco2,phtnox,phtsox = heat related emissions co2/nox/sox
C  pclco2,pclnox,pclsox = cooling related emissions co2/nox/sox
C  pltco2,pltnox,pltsox = lighting related emissions co2/nox/sox
C  pfnco2,pfnnox,pfnsox = fan/pump related emissions co2/nox/sox
C  pspco2,pspnox,pspsox = small power related emissions co2/nox/sox
C  phwco2,phwnox,phwsox = how water related emissions co2/nox/sox
      subroutine peconv(act)

      common/PCONV/ipconv,pcnvht,pcnvcl,pcnvlt,pcnvfn,pcnvsp,pcnvhw
      common/CONVEM/phtco2,phtnox,phtsox,pclco2,pclnox,pclsox,
     &              pltco2,pltnox,pltsox,pfnco2,pfnnox,pfnsox,
     &              pspco2,pspnox,pspsox,phwco2,phwnox,phwsox

      dimension ITEMS(18)
      character items*36,hold*40,act*1
      logical MODSIT
      integer nitms,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='peconv'  ! set for subroutine

      MODSIT=.false.

C If act = 'i' then initialise variables.
      if(act(1:1).eq.'i'.or.act(1:1).eq.'I')then
        helptopic='primary_energy_conv'
        call gethelptext(helpinsub,helptopic,nbhelp)
        ipconv=1
        call easkabc('Typical patterns',' ','UK','Europe','zeros',
     &    IW,nbhelp)
        if(IW.eq.1)then
          pcnvht=1.53; pcnvcl=1.44; pcnvlt=3.6; pcnvfn=3.6
          pcnvsp=3.6; pcnvhw=1.53
          phtco2=190.0; phtnox=0.3; phtsox=0.2; pclco2=612.0
          pclnox=2.06; pclsox=7.57
          pltco2=612.0; pltnox=2.06; pltsox=7.57; pfnco2=612.0
          pfnnox=2.06; pfnsox=7.57
          pspco2=612.0; pspnox=2.06; pspsox=7.57; phwco2=190.0
          phwnox=0.3; phwsox=0.2
        elseif(IW.eq.2)then
          pcnvht=1.53; pcnvcl=1.44; pcnvlt=3.6; pcnvfn=3.6
          pcnvsp=3.6; pcnvhw=1.53
          phtco2=190.0; phtnox=0.3; phtsox=0.2; pclco2=360.0
          pclnox=1.2; pclsox=4.5
          pltco2=360.0; pltnox=1.2; pltsox=4.5; pfnco2=360.0
          pfnnox=1.2; pfnsox=4.5
          pspco2=360.0; pspnox=1.2; pspsox=4.5; phwco2=190.0
          phwnox=0.3; phwsox=0.2
        elseif(IW.eq.3)then
          pcnvht=1.0; pcnvcl=1.0; pcnvlt=1.0; pcnvfn=1.0
          pcnvsp=1.0; pcnvhw=1.0
          phtco2=0.0; phtnox=0.0; phtsox=0.0; pclco2=0.0
          pclnox=0.0; pclsox=0.0
          pltco2=0.0; pltnox=0.0; pltsox=0.0; pfnco2=0.0
          pfnnox=0.0; pfnsox=0.0
          pspco2=0.0; pspnox=0.0; pspsox=0.0; phwco2=0.0
          phwnox=0.0; phwsox=0.0
        endif
        CALL EMKCFG('-',IER)
        return
      endif

    3 INO=-4
      IIER=0

      items(1)                 = '  multipliers demand > primary - '
      WRITE(ITEMS(2),'(A,F6.3)') 'a  heating   :',pcnvht
      WRITE(ITEMS(3),'(A,F6.3)') 'b  cooling   :',pcnvcl
      WRITE(ITEMS(4),'(A,F6.3)') 'c  lighting  :',pcnvlt
      WRITE(ITEMS(5),'(A,F6.3)') 'd  fans&pumps:',pcnvfn
      WRITE(ITEMS(6),'(A,F6.3)') 'e  small powr:',pcnvsp
      WRITE(ITEMS(7),'(A,F6.3)') 'f  hot water :',pcnvhw
      ITEMS(8)=                   '  ______________________________   '
      ITEMS(9)=                   '  emissions (g/kWh) co2  nox  sox  '
      WRITE(ITEMS(10),'(A,3F7.2)')'h heating : ',phtco2,phtnox,phtsox
      WRITE(ITEMS(11),'(A,3F7.2)')'i cooling : ',pclco2,pclnox,pclsox
      WRITE(ITEMS(12),'(A,3F7.2)')'j lighting: ',pltco2,pltnox,pltsox
      WRITE(ITEMS(13),'(A,3F7.2)')'k fan_pump: ',pfnco2,pfnnox,pfnsox
      WRITE(ITEMS(14),'(A,3F7.2)')'l small pr: ',pspco2,pspnox,pspsox
      WRITE(ITEMS(15),'(A,3F7.2)')'m hot water:',phwco2,phwnox,phwsox
      ITEMS(16)=                  '  ______________________________   '
      ITEMS(17)=                  '? help                             '
      ITEMS(18)=                  '- exit this menu                   '
      nitms=18

C Help text for this menu.
  4   helptopic='primary_energy_conv'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('Conversions to primary',ITEMS,nitms,INO)
    
      if(INO.EQ.nitms)then
        if(MODSIT)CALL EMKCFG('-',IER)
        RETURN
      elseif(INO.EQ.nitms-1)then
        CALL PHELPD('prim convers',nbhelp,'-',0,0,IER)
      elseif(ino.eq.2)then
        CALL EASKR(pcnvht,' ','Heating primary energy multiplier:',
     &       1.0,'W',5.0,'W',1.25,'heat prim multip',IER,nbhelp)
        MODSIT=.true.
      elseif(ino.eq.3)then
        CALL EASKR(pcnvcl,' ','Cooling primary energy multiplier:',
     &       1.0,'W',5.0,'W',3.6,'cool prim multip',IER,nbhelp)
        MODSIT=.true.
      elseif(ino.eq.4)then
        CALL EASKR(pcnvlt,' ','Lighting primary energy multiplier:',
     &       1.0,'W',5.0,'W',3.6,'light prim multip',IER,nbhelp)
        MODSIT=.true.
      elseif(ino.eq.5)then
        CALL EASKR(pcnvfn,' ','Fans & pumps primary energy multiplier:',
     &       1.0,'W',5.0,'W',3.6,'fans prim multip',IER,nbhelp)
        MODSIT=.true.
      elseif(ino.eq.6)then
        CALL EASKR(pcnvsp,' ','Small power primary energy multiplier:',
     &       1.0,'W',5.0,'W',3.6,'sml pwr prim multip',IER,nbhelp)
        MODSIT=.true.
      elseif(ino.eq.7)then
        CALL EASKR(pcnvhw,' ','Hot water primary energy multiplier:',
     &       1.0,'W',5.0,'W',3.6,'hot water prim multip',IER,nbhelp)
        MODSIT=.true.
      elseif(ino.eq.10)then
   95   hold = ' '
        WRITE(HOLD,'(3F10.3)')phtco2,phtnox,phtsox
        CALL EASKS(HOLD,'Heating related emissions [gr/kWh primary]:',
     &    'CO2 NOx & SOx ',40,' 190.  0.2  0.1 ','heat emiss',
     &    IIER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,phtco2,0.,999.,'W','ht co2',IIER)
        CALL EGETWR(HOLD,K,phtnox,0.,999.,'W','ht nox',IIER)
        CALL EGETWR(HOLD,K,phtsox,0.,999.,'W','ht sox',IIER)
        if(iier.ne.0)goto 95
        MODSIT=.true.
      elseif(ino.eq.11)then
   96   hold = ' '
        WRITE(HOLD,'(3F10.3)')pclco2,pclnox,pclsox
        CALL EASKS(HOLD,'Cooling related emissions [gr/kWh primary]:',
     &    'CO2 NOx & SOx ',40,' 612.  2.06   7.5 ','cool emiss',
     &    IIER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,pclco2,0.,999.,'W','cl co2',IIER)
        CALL EGETWR(HOLD,K,pclnox,0.,999.,'W','cl nox',IIER)
        CALL EGETWR(HOLD,K,pclsox,0.,999.,'W','cl sox',IIER)
        if(iier.ne.0)goto 96
        MODSIT=.true.
      elseif(ino.eq.12)then
   97   hold = ' '
        WRITE(HOLD,'(3F10.3)')pltco2,pltnox,pltsox
        CALL EASKS(HOLD,'Lighting related emissions [gr/kWh primary]:',
     &   'CO2 NOx & SOx ',40,' 612.  2.06   7.5 ','light emiss',
     &   IIER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,pltco2,0.,999.,'W','lt co2',IIER)
        CALL EGETWR(HOLD,K,pltnox,0.,999.,'W','lt nox',IIER)
        CALL EGETWR(HOLD,K,pltsox,0.,999.,'W','lt sox',IIER)
        if(iier.ne.0)goto 97
        MODSIT=.true.
      elseif(ino.eq.13)then
   98   hold = ' '
        WRITE(HOLD,'(3F10.3)')pfnco2,pfnnox,pfnsox
        CALL EASKS(HOLD,'Fan & pump emissions [gr/kWh primary]:',
     &    'CO2 NOx & SOx ',40,' 612.  2.06   7.5 ','fan emiss',
     &    IIER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,pfnco2,0.,999.,'W','fan co2',IIER)
        CALL EGETWR(HOLD,K,pfnnox,0.,999.,'W','fan nox',IIER)
        CALL EGETWR(HOLD,K,pfnsox,0.,999.,'W','fan sox',IIER)
        if(iier.ne.0)goto 98
        MODSIT=.true.
      elseif(ino.eq.14)then
   99   hold = ' '
        WRITE(HOLD,'(3F10.3)')pspco2,pspnox,pspsox
        CALL EASKS(HOLD,'Small power emissions [gr/kWh primary]:',
     &    'CO2 NOx & SOx ',40,'612.   2.06  7.5 ','smlp emiss',
     &    IIER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,pspco2,0.,999.,'W','sml co2',IIER)
        CALL EGETWR(HOLD,K,pspnox,0.,999.,'W','sml nox',IIER)
        CALL EGETWR(HOLD,K,pspsox,0.,999.,'W','sml sox',IIER)
        if(iier.ne.0)goto 99
        MODSIT=.true.
      elseif(ino.eq.15)then
   94   hold = ' '
        WRITE(HOLD,'(3F10.3)')phwco2,phwnox,phwsox
        CALL EASKS(HOLD,'Hot water emissions [gr/kWh primary]:',
     &    'CO2 NOx & SOx ',40,' 190.  0.2  0.1 ','dhw emiss',
     &    IIER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,phwco2,0.,999.,'W','hw co2',IIER)
        CALL EGETWR(HOLD,K,phwnox,0.,999.,'W','hw nox',IIER)
        CALL EGETWR(HOLD,K,phwsox,0.,999.,'W','hw sox',IIER)
        if(iier.ne.0)goto 94
        MODSIT=.true.
      else
        INO=-4
        GOTO 4
      endif
      INO=-4
      GOTO 3

      end

C ********** calenmanage **************
C Calenmanage sets up and manages a calendar for ESP-r. Currently
C up to 10 day types can be defined and one may be assigned to
C each of the 365 days of the year.
C calename (char*32) is the overall name of this calendar (e.g. `UK standard`)
C calentag (char*12) is a tag for each day type (e.g. `autumn_wkd`)
C calendayname (char*32) menu phrase for each day type (e.g.
C   `autumn weekdays`
C nbdaytype (int) is the number of day types
C nbcaldays (int) is the number of days associated with each type
C icalendar (int*365) for each day, the associated day type
C act (char*1) action to take 'i' initialise, '-' interactive

C Note that there is no specific logic to remind a user to update the
C model cfg file if a change is made.
      subroutine calenmanage(act,ier)
      implicit none
#include "epara.h"
#include "building.h"
#include "control.h"

      integer lnblnk  ! function definition

C Parameters passed
      character act*1 ! action to take
      integer ier     ! error state

      integer iuout,iuin
      common/OUTIN/IUOUT,IUIN
      integer iyear,ibdoy,iedoy,ifday,iftime
      common/set1/iyear,ibdoy,iedoy,ifday,iftime
      common/user/browse
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      character calename*32,calentag*12,calendayname*32
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      integer nbdaytype,nbcaldays,icalender
      COMMON/FILEP/IFIL
      integer ifil
      common/cctlnm/ctldoc,lctlf
      character ctldoc*248,LCTLF*72
      integer MVERT,IVERT,mcvert,icvert ! max items and current menu item
      integer iloop,ib,ld,l,idayloop,loop ! index for looping
      integer ijd,idayn,idayno,idwk,idt,idtt,imthn,ixd   ! for counting days
      integer idno,istjandwk,ISDS,ISDF   ! for counting days
      integer ifoc,ifrq,ihdt,m,mj,ij,mm,loopst,ix,io,isw,ipact,irt ! for position
      integer icfoc,ictlf  ! for control domain and file unit
      integer ieri,istat   ! for error states
      integer ITRC         ! for verbosity

C m1slots and m2slots are for the two possible months to be
C displayed. 42 is 7 days over maximum of 6 periods e.g.
C                   2000
C         Jan                     Feb             
C Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su  
C                 1  2        1  2  3  4  5  6         
C  3  4  5  6  7  8  9     7  8  9 10 11 12 13     
C 10 11 12 13 14 15 16    14 15 16 17 18 19 20    
C 17 18 19 20 21 22 23    21 22 23 24 25 26 27    
C 24 25 26 27 28 29 30    28 29                   
C 31                    
      DIMENSION VERT(35),citem(17),IDVALS(10),clist(365)
      dimension ixdvals(365)
      integer IDVALS,ixdvals

      character vert*33,DS*7,DS1*10,key*1
      character outs*124,head*18,tcalename*32,citem*52,clist*52
      character tcalentag*12,tcalendayn*32
      character dayact*2   ! action to take in managing day types
      logical modify,browse,XST
      logical havectl ! to remember if control file exists
      integer IHDTP   ! for users preference to add or copy
      logical usecalendar  ! signal so easier to detect if other domains
                           ! have a different number of day types

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='calenmanage'  ! set for subroutine
      havectl=.false.          ! assume no control
      usecalendar=.false. 

C If user requested initial calendar.

C << consider if we can default to 4 day types (wk sat sun holiday)

      if(act.eq.'i'.and.(.NOT.browse))then
        if(nbdaytype.le.3)then
          calename='standard weekday Sat & Sun'
          nbdaytype=3
          nbcaldays(1)=0
          calentag(1)='weekdays'
          calendayname(1)='weekdays (all year)'
          nbcaldays(2)=0
          calentag(2)='saturday'
          calendayname(2)='Saturdays (all year)'
          nbcaldays(3)=0
          calentag(3)='sunday'
          calendayname(3)='Sundays (all year)'
          calentag(4)='-'
          calentag(5)='-'
          calentag(6)='-'
          calentag(7)='-'
          calentag(8)='-'
          calentag(9)='-'
          calendayname(4)='-'
          calendayname(5)='-'
          calendayname(6)='-'
          calendayname(7)='-'
          calendayname(8)='-'
          calendayname(9)='-'
          do 42 ijd=1,365

C for julian day ijd find month and day of month and day of week.
            call edayr(ijd,idayn,imthn)
            call eweekd(idayn,imthn,iyear,idwk)
            if(idwk.ge.1.and.idwk.le.5)then
              icalender(ijd)=1
              nbcaldays(1)=nbcaldays(1)+1
            elseif(idwk.eq.6)then
              icalender(ijd)=2
              nbcaldays(2)=nbcaldays(2)+1
            elseif(idwk.eq.7)then
              icalender(ijd)=3
              nbcaldays(3)=nbcaldays(3)+1
            endif
  42      continue
          return
        else
          call usrmsg('day types exist, not initialised.',' ','W')
        endif
      endif

      MHEAD=4
      MCTL=5
      ILEN=365
      IPACT=CREATE
      CALL EKPAGE(IPACT)
      modify=.false.

C Initial menu entry setup.
   92 IER=0
      ILEN=365
      IVERT=-3

C << ?? relax this assumption of 3 day types

      if(nbdaytype.ge.3) usecalendar=.true. 

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
    3 M=MHEAD
      write(VERT(1),'(2a)')     '1 calendar: ',calename(1:20)
      write(VERT(2),'(a,i2,a)') '2 day types (',nbdaytype,')'
      VERT(3)                  =' __________________________'
      VERT(4)                  ='  date         day type    '
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          call stdate(iyear,l,DS,DS1)
          WRITE(VERT(M),'(a1,1x,3a)') KEY,DS1,'  ',
     &      calentag(icalender(L))
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        VERT(M+1)='  ______________________________ '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' --------')
      ENDIF
      VERT(M+2)  ='+ apply day type to many days    '
      VERT(M+3)  ='! list the calendar              '
      VERT(M+4)  ='? help                           '
      VERT(M+5)  ='- exit                           '

C Help text for this menu.
      helptopic='calendar_day_types'
      call gethelptext(helpinsub,helptopic,nbhelp)

      write(head,'(a,i4)') ' Calendar for ',iyear
      CALL EMENU(head,VERT,MVERT,IVERT)
      IF(IVERT.eq.1)THEN

C Edit calendar name
        tcalename=calename
        CALL EASKS(tcalename,' Description of calendar: ',
     &    '  ',32,'weekday sat sun all year','calen descr',
     &    IER,nbhelp)
        if(tcalename(1:2).ne.'  ')then
          if(tcalename(1:lnblnk(tcalename)).ne.
     &       calename(1:lnblnk(calename)))then
            calename=tcalename
            modify=.true.
          endif
        endif
      ELSEIF(IVERT.EQ.2)THEN

C Manage day types.
        CALL EPMENSV
  73    write(citem(1),'(2a)') '  calendar: ',calename(1:20)
        write(citem(2),'(a)')  ' __tag________description______________'
        mj=2
        do 77 ij=1,nbdaytype
          CALL EMKEY(ij,KEY,IER)
          write(citem(ij+mj),'(5a)') KEY,' ',calentag(ij),' ',
     &      calendayname(ij)
  77    continue
        mm=mj+nbdaytype
        write(citem(mm+1),'(a)') ' ____________________________________'
        write(citem(mm+2),'(a)') '+ add/delete                         '
        write(citem(mm+3),'(a)') '? help                               '
        write(citem(mm+4),'(a)') '- exit                               '
        mcvert=mj+nbdaytype+4
        icvert=-1
        CALL EMENU('  Day types',citem,mcvert,icvert)
        if(icvert.eq.mcvert)then
          CALL EPMENRC
          goto 3
        elseif(icvert.eq.mcvert-1)then

C Help.
          CALL PHELPD('calendar day section',nbhelp,'-',0,0,IER)
          goto 73
        elseif(icvert.eq.mcvert-2)then

C Add or delete a day type.

C << can we relax logic to allow there not to be a weekday saturday sunday
C << day type e.g. as in a hospital use scenario?

          if(nbdaytype.ge.3.and.nbdaytype.le.MDTY)then
            CALL EASKABC(' Options:',' ',
     &        'add','delete (not available)','continue',IRT,nbhelp)
            if(irt.eq.1)then

C If there is a control file, scan it and then loop through each of the
C existing control loops to add in what-will-become the new day type.
              ICTLF=IFIL+1
              CALL ERPFREE(ICTLF,ISTAT)
              call FINDFIL(LCTLF,XST)
              if(XST)then
                CALL EZCTLR(ICTLF,ITRC,IUOUT,IER)
                havectl=.true.          ! yes we have control

C Ask user whether to create a minimal control regime or to
C duplicate the last control day type in each loop.
                IDT=nbdaytype     ! set to the not-yet-incremented value
                IHDTP=1
                CALL EASKABC('Pattern for new day type (see help)?',' ',
     &            'One period free floating',
     &            'Use pattern of last day type','cancel',IHDTP,nbhelp)
                if(IHDTP.eq.1)then
                  dayact='SA'       ! assume silent addition
                elseif(IHDTP.eq.2)then
                  dayact='SC'       ! assume silent copy
                elseif(IHDTP.eq.3)then
                  havectl=.false.   ! ignore control
                  goto 87           ! do not bother processing control data
                endif
                if(NCF.gt.0)then  ! zone loops
                  icfoc=0
                  do 80 iloop=1,NCF
                    if(nbcdt(iloop).ne.0.AND.usecalendar)then
                      continue   ! calendar and ideal day types do not match
                    else
                      call ADDCTLD(icfoc,iloop,IDT,dayact)
                    endif
  80              continue
                endif
                if(NCC.gt.0)then  ! flow loops
                  icfoc=2
                  do 81 iloop=1,NCC
                    if(nfcdt(iloop).ne.0.AND.usecalendar)then
                      continue   ! calendar and flow day types do not match
                    else
                      call ADDCTLD(icfoc,iloop,IDT,dayact)
                    endif
  81              continue
                endif
                if(NCL.gt.0)then  ! plant loops
                  icfoc=1
                  do 82 iloop=1,NCL
                    if(npcdt(iloop).ne.0.AND.usecalendar)then
                      continue   ! calendar and plant day types do not match
                    else
                      call ADDCTLD(icfoc,iloop,IDT,dayact)
                    endif
  82              continue
                endif
                if(NGF.gt.0)then  ! global loops
                  icfoc=3
                  do 83 iloop=1,NGF
                    call ADDCTLD(icfoc,iloop,IDT,dayact)
  83              continue
                endif
                if(NOF.gt.0)then  ! optical loops
                  icfoc=5
                  do 84 iloop=1,NOF
                    call ADDCTLD(icfoc,iloop,IDT,dayact)
  84              continue
                endif
                if(nCFCctlloops.gt.0)then ! CFC loops
                  icfoc=6
                  do 86 iloop=1,nCFCctlloops
                    call ADDCTLD(icfoc,iloop,IDT,dayact)
  86              continue
                endif
              endif

C Now increment nbdaytype and get its name and description.
  87          nbdaytype=nbdaytype+1
              nbcaldays(nbdaytype)=0
              calentag(nbdaytype)='new'
              calendayname(nbdaytype)='new day type'

C Edit calendar day tag and name.
              tcalentag=calentag(nbdaytype)
              CALL EASKS(tcalentag,' Calendar day type tag: ',
     &          '  ',12,'holiday','day tag',IER,nbhelp)
              if(tcalentag(1:2).ne.'  ')then
                if(tcalentag(1:lnblnk(tcalentag)).ne.
     &calentag(nbdaytype)(1:lnblnk(calentag(nbdaytype))))then
                  calentag(nbdaytype)=tcalentag
                endif
              endif
              tcalendayn=calendayname(nbdaytype)
              CALL EASKS(tcalendayn,' Calendar day type descripton: ',
     &          '  ',32,'holiday','day descr',IER,nbhelp)
              if(tcalendayn(1:2).ne.'  ')then
                if(tcalendayn(1:lnblnk(tcalendayn)).ne.
     &calendayname(nbdaytype)(1:lnblnk(calendayname(nbdaytype))))then
                  calendayname(nbdaytype)=tcalendayn
                endif
              endif

C Update control data for all zones to include this new day type.
              if(havectl)then
                CALL CTLWRT(ICTLF,IER)
              endif

C Deal with operation files. User will be asked if they want to
C overwrite or save to new file names.
              CALL UPDOPR
              modify=.true.
              goto 73
            elseif(irt.eq.2)then
              goto 73
            elseif(irt.eq.3)then
              goto 73
            endif
          endif
        elseif(icvert.gt.2.and.icvert.lt.mcvert-3)then

C Edit calendar day tag and name.
          ifoc=icvert-2
          tcalentag=calentag(ifoc)
          CALL EASKS(tcalentag,' Calendar day type tag: ',
     &      '  ',12,'holiday','day tag',IER,nbhelp)
          if(tcalentag(1:2).ne.'  ')then
            if(tcalentag(1:lnblnk(tcalentag)).ne.
     &         calentag(ifoc)(1:lnblnk(calentag(ifoc))))then
              calentag(ifoc)=tcalentag
              modify=.true.
            endif
          endif
          tcalendayn=calendayname(ifoc)
          CALL EASKS(tcalendayn,' Calendar day type descripton: ',
     &      '  ',32,'holiday','day descr',IER,nbhelp)
          if(tcalendayn(1:2).ne.'  ')then
            if(tcalendayn(1:lnblnk(tcalendayn)).ne.
     &         calendayname(ifoc)(1:lnblnk(calendayname(ifoc))))then
              calendayname(ifoc)=tcalendayn
              modify=.true.
            endif
          endif
          goto 73
        else
          goto 73
        endif
      ELSEIF(IVERT.EQ.MVERT)THEN
        if(modify.and.(.NOT.browse))then

C Update calendar day types
          DO 21 IB=1,NBDAYTYPE
            NBCALDAYS(IB)=0
 21       CONTINUE
          DO 22 IB=1,365
            NBCALDAYS(ICALENDER(IB))=NBCALDAYS(ICALENDER(IB))+1
 22       CONTINUE

C update the model if not browsing 
          CALL EMKCFG('-',IER)
        endif
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Produce help text for the vertex menu.
        CALL PHELPD('calendar section',nbhelp,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2))THEN

C List current calendar.
C Begin by finding out the day of the week of 1 Jan.
        ijd=1
        call edayr(ijd,idayn,imthn)
        call eweekd(idayn,imthn,iyear,istjandwk)
        idno=1
        isw=0
        call MENUATOL(outs,'Display options:',
     &    'a january - march','b april - june',
     &    'c july - september','d october - december ',
     &    'e all year',' ',' ',' ',' ',' ',' ',' ',isw,idno,nbhelp)

C loop is the number of months to display
C loopst is the month number to start with
        if(isw.eq.1)then
          loop=3
          loopst=1
        elseif(isw.eq.2)then
          loop=3
          loopst=4
        elseif(isw.eq.3)then
          loop=3
          loopst=7
        elseif(isw.eq.4)then
          loop=3
          loopst=10
        elseif(isw.eq.5)then
          loop=12
          loopst=1
        endif

C Print the calendar.
        call calenprint(iuout,'t',iyear,loopst,loop)
        call edisp(iuout,' ')
        call calenprint(iuout,'g',iyear,loopst,loop)
        call edisp(iuout,' ')
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C Apply day type to several days of the year. First ask for the
C day type and then present a list of all of the days so that the
C user can select one or more.
        CALL EPMENSV
        do 67 ij=1,nbdaytype
          write(citem(ij),'(3a)') calentag(ij),' ',calendayname(ij)
  67    continue
        IX=1
        CALL EPICKS(IX,IDVALS,' ',' Available day types:',
     &    52,nbdaytype,citem,'available day types',IER,nbhelp)
       if(ix.ne.0)then
          IHDT=0
          CALL EASKABC('How to add day types (see help)?',' ',
     &    'One by one','Follow pattern','cancel',IHDT,nbhelp)
          IF(IHDT.EQ.1)THEN
            idt=idvals(1)
            do 78 ld=1,365
              call stdate(iyear,ld,DS,DS1)
              WRITE(clist(ld),'(3a)') DS1,'  ',
     &          calentag(icalender(ld))
  78        continue
            CALL EPMENSV
            ixd=365
            CALL EPICKS(ixd,ixdvals,' ',' Days in the year:',
     &        52,365,clist,'available days',IER,nbhelp)
            CALL EPMENRC
            if(ixd.ne.0)then
              do 79 idayloop=1,ixd
                ifoc=ixdvals(idayloop)
                call stdate(iyear,ifoc,DS,DS1)
                icalender(ifoc)=idt
                nbcaldays(idt)=nbcaldays(idt) + 1
                write(outs,*) 'Revised day: ',ifoc,' ',DS1,' ',
     &            icalender(ifoc),' ',calendayname(icalender(ifoc))
                call edisp(iuout,outs)
  79          continue
              modify=.true.
            endif
          ELSEIF(IHDT.EQ.2)THEN
            ISDS=1
            ISDF=365
            IFRQ=1
            call eAskPer('Dates during which to add new day type',
     &      isds,isdf,ifday,ier)

C Regenerate help string that is clobbered by call to easkper
            CALL EASKI(IFRQ,
     &      ' Enter number of days to repeat pattern after ',
     &      ' E.g. 7=weekly, 30=monthly, 1=daily ',
     &      1,'F',30,'F',1,'frequency ',IERI,nbhelp)
            DO 532 IDTT=ISDS,ISDF,IFRQ
              ICALENDER(IDTT)=idvals(1)
 532        CONTINUE
          ENDIF
        endif
        CALL EPMENRC
      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN

C Edit day identified by KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        call stdate(iyear,ifoc,DS,DS1)
        write(outs,*) 'For day: ',ifoc,' ',DS1,' ',
     &    icalender(ifoc),' ',calendayname(icalender(ifoc))
        call edisp(iuout,outs)

C Select from current range of day types. Assign icalendar value
C for the focus day to the selected day type and increment nbcaldays.
        CALL EPMENSV
        do 76 ij=1,nbdaytype
          write(citem(ij),'(3a)') calentag(ij),' ',calendayname(ij)
  76    continue
        IX=1
        CALL EPICKS(IX,IDVALS,' ',' Available day types:',
     &    52,nbdaytype,citem,'avail day types',IER,nbhelp)
        if(ix.ne.0)then
          idt=idvals(1)
          icalender(ifoc)=idt
          nbcaldays(idt)=nbcaldays(idt) + 1
          write(outs,*) 'Revised day: ',ifoc,' ',DS1,' ',
     &      icalender(ifoc),' ',calendayname(icalender(ifoc))
          call edisp(iuout,outs)
          modify=.true.
        endif
        CALL EPMENRC
      ELSE
C Not one of the legal menu choices.
        IVERT=-1
        goto 92
      ENDIF
      IVERT=-2
      goto 3

C Errors.
C   14 if(IOS.eq.2)then
C        CALL USRMSG(' No prmission to write array in ',' ','W')
C      else
C        CALL USRMSG(' Long arrary write error in ',' ','W')
C      endif
C      IER=1
C      return
      end


C ********** calenprint **************
C Calenprint displays a calendar for year iyear beginning
C at loopst for loop months. 
C Currently it prints to text feedback or file only. Future
C option is to display in graphic feedback with day types
C as defined by calenmanage.
      subroutine calenprint(itru,act,iyear,loopst,loop)
#include "building.h"

C Parameters
      integer itru    ! reporting unit
      character act*1 ! action to take 't' text feedback, 'g' graphic feedback
      integer iyear   ! year to use for day of week calculations
      integer loopst  ! month to start list/display
      integer loop    ! number of months to list/display

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)

C m1slots and m2slots are for the two possible months to be
C displayed. 42 is 7 days over maximum of 6 periods e.g.
C                   2000
C         Jan                     Feb             
C Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su  
C                 1  2        1  2  3  4  5  6         
C  3  4  5  6  7  8  9     7  8  9 10 11 12 13     
C 10 11 12 13 14 15 16    14 15 16 17 18 19 20    
C 17 18 19 20 21 22 23    21 22 23 24 25 26 27    
C 24 25 26 27 28 29 30    28 29                   
C 31                    
C         Jan                     Feb             
C Mo Tu We Th Fr Sa Su    Mo Tu We Th Fr Sa Su  
C  x  x  x  x  x  x  x     x  x  x  x  x  x  x
C  x  x  x  x  x  x  x     x  x  x  x  x  x  x
C  x  x  x  x  x  x  x     x  x  x  x  x  x  x
C  x  x  x  x  x  x  x     x  x  x  x  x  x  x
C  x  x  x  x  x  x  x     x  x  x  x  x  x  x
C  x  x  x  x  x  x  x     x  x  x  x  x  x  x
      DIMENSION m1slots(42,2),m2slots(42,2)
#ifdef OSI
      integer iside,isize,ifont     ! passed to viewtext
      integer iicol
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iicol
#endif

C Text for each slot.
      character*3 am1slots(42),am2slots(42)
      character calename*32,calentag*12,calendayname*32,word3*3
      character mohead*44,outs*124,etext*72
      CHARACTER*9 RAMONTH(12)
      DIMENSION ID(12),MDAYS(12)

C ID is the number of days in each month, MDAYS is the julian
C start day of each month.
      DATA ID/31,28,31,30,31,30,31,31,30,31,30,31/
      DATA MDAYS/1,32,60,91,121,152,182,213,244,274,305,335/
      DATA RAMONTH/'January  ','February ','March    ','April    ',
     &             'May      ','June     ','July     ','August   ',
     &             'September','October  ','November ','December '/

C Set common text strings and then if in test mode print months
C side by side.
      mohead = ' Mo Tu We Th Fr Sa Su  Mo Tu We Th Fr Sa Su '
      if(act.eq.'t')then
        write(outs,'(20x,i4)') iyear
        call edisp(itru,outs)

C If number of months to display is a multiple of 2 display side by side.
C imon1 and imon2 are possible months to dispalay (if zero then do not).
C iloopfn is the end month in the do 44
        imon1=0
        imon2=0
        iloopfn=(loopst+loop)-1
        do 44 im=loopst,iloopfn,2
          if(mod(loop,2).eq.0)then

C Set two consecutive months, write their names and the full
C heading and then find the start day of the week for each.
            imon1=im
            imon2=im+1
            write(outs,'(8x,a,17x,a)') ramonth(imon1),ramonth(imon2)
            call edisp(itru,outs)
            write(outs,'(a)') mohead
            call edisp(itru,outs)
            call edayr(mdays(imon1),idayn,imthn)
            call eweekd(idayn,imthn,iyear,im1dwk)
            call edayr(mdays(imon2),idayn,imthn)
            call eweekd(idayn,imthn,iyear,im2dwk)
          else
            if(im.eq.iloopfn)then
              imon1=im
              imon2=0
              write(outs,'(8x,a)') ramonth(imon1)
              call edisp(itru,outs)
              write(outs,'(a)') mohead(1:22)
              call edisp(itru,outs)
              call edayr(mdays(imon1),idayn,imthn)
              call eweekd(idayn,imthn,iyear,im1dwk)
              im2dwk=0
            else
              imon1=im
              imon2=im+1
              write(outs,'(8x,a,17x,a)') ramonth(imon1),ramonth(imon2)
              call edisp(itru,outs)
              write(outs,'(a)') mohead
              call edisp(itru,outs)
              call edayr(mdays(imon1),idayn,imthn)
              call eweekd(idayn,imthn,iyear,im1dwk)
              call edayr(mdays(imon2),idayn,imthn)
              call eweekd(idayn,imthn,iyear,im2dwk)
            endif
          endif

C For one or two current months fill the m1slots and m2slots.
C icntm1 & icntm2 increment as days fill the slots.
C If the slot is before the start of the month fill with 0,
C if the slot is the first day of the month set slot date and day type,
C if a subsequent day of the month the set slot date and day type.
C am1slots(42),am2slots(42)
          icntm1=0
          icntm2=0
          do 46 ims = 1,42
            if(ims.lt.im1dwk)then
              m1slots(ims,1)=0
              m1slots(ims,2)=0
              am1slots(ims)='   '
            elseif(ims.ge.im1dwk)then
              icntm1=icntm1+1
              if(icntm1.le.id(imon1))then
                m1slots(ims,1)=icntm1
                CALL EDAY(icntm1,imon1,icurday)
                m1slots(ims,2)=icalender(icurday)
                write(am1slots(ims),'(i3)') icntm1 
              else
                m1slots(ims,1)=0
                m1slots(ims,2)=0
                am1slots(ims)='   '
              endif
            endif

C Fill second month with real data or blanks.
            if(im2dwk.eq.0)then
              m2slots(ims,1)=0
              m2slots(ims,2)=0
              am2slots(ims)='   '
            else
              if(ims.lt.im2dwk)then
                m2slots(ims,1)=0
                m2slots(ims,2)=0
                am2slots(ims)='   '
              elseif(ims.ge.im2dwk)then
                icntm2=icntm2+1
                if(icntm2.le.id(imon2))then
                  m2slots(ims,1)=icntm2
                  CALL EDAY(icntm2,imon2,icurday)
                  m2slots(ims,2)=icalender(icurday)
                  write(am2slots(ims),'(i3)') icntm2 
                else
                  m2slots(ims,1)=0
                  m2slots(ims,2)=0
                  am2slots(ims)='   '
                endif
              endif
            endif
  46      continue
          write(outs,'(15a)') (am1slots(J),j=1,7),' ',
     &      (am2slots(J),j=1,7)
          call edisp(itru,outs)
          write(outs,'(15a)') (am1slots(J),j=8,14),' ',
     &      (am2slots(J),j=8,14)
          call edisp(itru,outs)
          write(outs,'(15a)') (am1slots(J),j=15,21),' ',
     &      (am2slots(J),j=15,21)
          call edisp(itru,outs)
          write(outs,'(15a)') (am1slots(J),j=22,28),' ',
     &      (am2slots(J),j=22,28)
          call edisp(itru,outs)
          write(outs,'(15a)') (am1slots(J),j=29,35),' ',
     &      (am2slots(J),j=29,35)
          call edisp(itru,outs)
          write(outs,'(15a)') (am1slots(J),j=36,42),' ',
     &      (am2slots(J),j=36,42)
          call edisp(itru,outs)
  44    continue 
      elseif(act.eq.'g')then
        if(MMOD.eq.8)call startbuffer()
        line=3
        write(etext,'(20x,i4)') iyear
        iside=line
        isize=0
        ifont=2
        call viewtext(etext,iside,isize,ifont)

C If number of months to display is a multiple of 2 display side by side.
C imon1 and imon2 are possible months to dispalay (if zero then do not).
C iloopfn is the end month in the do 44
        imon1=0
        imon2=0
        iloopfn=(loopst+loop)-1
        do 144 im=loopst,iloopfn,2
          if(mod(loop,2).eq.0)then

C Set two consecutive months, write their names and the full
C heading and then find the start day of the week for each.
            imon1=im
            imon2=im+1
            line=line+1
            write(etext,'(8x,a,17x,a)') ramonth(imon1),ramonth(imon2)
            iside=line
            isize=0
            ifont=2
            call viewtext(etext,iside,isize,ifont)
            line=line+1
            write(etext,'(a)') mohead
            iside=line
            call viewtext(etext,iside,isize,ifont)
            call edayr(mdays(imon1),idayn,imthn)
            call eweekd(idayn,imthn,iyear,im1dwk)
            call edayr(mdays(imon2),idayn,imthn)
            call eweekd(idayn,imthn,iyear,im2dwk)
          else
            if(im.eq.iloopfn)then
              imon1=im
              imon2=0
              line=line+1
              write(etext,'(8x,a)') ramonth(imon1)
              iside=line
              isize=0
              ifont=2
              call viewtext(etext,iside,isize,ifont)
              line=line+1
              write(etext,'(a)') mohead(1:22)
              iside=line
              call viewtext(etext,iside,isize,ifont)
              call edayr(mdays(imon1),idayn,imthn)
              call eweekd(idayn,imthn,iyear,im1dwk)
              im2dwk=0
            else
              imon1=im
              imon2=im+1
              line=line+1
              write(etext,'(8x,a,17x,a)') ramonth(imon1),ramonth(imon2)
              iside=line
              isize=0
              ifont=2
              call viewtext(etext,iside,isize,ifont)
              line=line+1
              write(etext,'(a)') mohead
              iside=line
              call viewtext(etext,iside,isize,ifont)
              call edayr(mdays(imon1),idayn,imthn)
              call eweekd(idayn,imthn,iyear,im1dwk)
              call edayr(mdays(imon2),idayn,imthn)
              call eweekd(idayn,imthn,iyear,im2dwk)
            endif
            call forceflush()
          endif

C For one or two current months fill the m1slots and m2slots.
C icntm1 & icntm2 increment as days fill the slots.
C If the slot is before the start of the month fill with 0,
C if the slot is the first day of the month set slot date and day type,
C if a subsequent day of the month the set slot date and day type.
C am1slots(42),am2slots(42)
          icntm1=0
          icntm2=0
          do 146 ims = 1,42
            if(ims.lt.im1dwk)then
              m1slots(ims,1)=0
              m1slots(ims,2)=0
              am1slots(ims)='   '
            elseif(ims.ge.im1dwk)then
              icntm1=icntm1+1
              if(icntm1.le.id(imon1))then
                m1slots(ims,1)=icntm1
                CALL EDAY(icntm1,imon1,icurday)
                m1slots(ims,2)=icalender(icurday)
                write(am1slots(ims),'(i3)') icntm1 
              else
                m1slots(ims,1)=0
                m1slots(ims,2)=0
                am1slots(ims)='   '
              endif
            endif

C Fill second month with real data or blanks.
            if(im2dwk.eq.0)then
              m2slots(ims,1)=0
              m2slots(ims,2)=0
              am2slots(ims)='   '
            else
              if(ims.lt.im2dwk)then
                m2slots(ims,1)=0
                m2slots(ims,2)=0
                am2slots(ims)='   '
              elseif(ims.ge.im2dwk)then
                icntm2=icntm2+1
                if(icntm2.le.id(imon2))then
                  m2slots(ims,1)=icntm2
                  CALL EDAY(icntm2,imon2,icurday)
                  m2slots(ims,2)=icalender(icurday)
                  write(am2slots(ims),'(i3)') icntm2 
                else
                  m2slots(ims,1)=0
                  m2slots(ims,2)=0
                  am2slots(ims)='   '
                endif
              endif
            endif
 146      continue

C Repeat this 6 times (there can be this many lines of 7 days)
          do 149 lj=1,6
            if(lj.eq.1)then
              lstart=1
              lfinish=7
            elseif(lj.eq.2)then
              lstart=8
              lfinish=14
            elseif(lj.eq.3)then
              lstart=15
              lfinish=21
            elseif(lj.eq.4)then
              lstart=22
              lfinish=28
            elseif(lj.eq.5)then
              lstart=29
              lfinish=35
            elseif(lj.eq.6)then
              lstart=36
              lfinish=42
            endif

C Plot out the first 7 days.
            isize=2
            line=line+1
            icx=1
            do 147 j = lstart,lfinish
              write(word3(1:3),'(a)') am1slots(J)(1:3)
              idcol=m1slots(j,2)
              iicol=0
              if(idcol.eq.0)call winscl('-',iicol)
              call findviewtext(icx,line,isize,ix,iy)
              if(idcol.eq.0)then
                call textatxy(ix,iy,word3,'-',idcol)
              else
                call textatxy(ix,iy,word3,'z',idcol)
              endif
              icx=icx+3
  147       continue

C Shift over another character and print out the 2nd months line.
            icx=icx+1
            do 148 j = lstart,lfinish
              write(word3(1:3),'(a)') am2slots(J)(1:3)
              idcol=m2slots(j,2)
              iicol=0
              if(idcol.eq.0)call winscl('-',iicol)
              call findviewtext(icx,line,isize,ix,iy)
              if(idcol.eq.0)then
                call textatxy(ix,iy,word3,'-',idcol)
              else
                call textatxy(ix,iy,word3,'z',idcol)
              endif
              icx=icx+3
  148       continue
 149      continue
 144    continue

C Also include list of day types.
        line=line+2
        iicol=0
        call winscl('-',iicol)
        write(etext,*) 'Calendar: ',calename
        iside=line
        isize=0
        ifont=2
        call viewtext(etext,iside,isize,ifont)
        do 151 icd = 1,nbdaytype
          line=line+1
          write(etext,*) calentag(icd),' ',calendayname(icd)
          idcol=icd
          iicol=0
          if(idcol.eq.0)call winscl('-',iicol)
          call findviewtext(3,line,isize,ix,iy)
          if(idcol.eq.0)then
            call textatxy(ix,iy,etext,'-',idcol)
          else
            call textatxy(ix,iy,etext,'z',idcol)
          endif
 151    continue
        iicol=0
        call winscl('-',iicol)
        call forceflush()
      endif
      return

C Errors for debug printing.
C   14 if(IOS.eq.2)then
C        CALL USRMSG(' No prmission to write array in ',' ','W')
C      else
C        CALL USRMSG(' Long array write error in ',' ','W')
C      endif
C      IER=1
C      return
      end

C ********** EDDGTP **************
C EDDGTP Sets up ground temperature profiles
      SUBROUTINE EDDGTP(modsit)
#include "building.h"
#include "site.h"
      integer iCountWords

      common/OUTIN/IUOUT,IUIN

      CHARACTER HOLD*72,OUTS*124
      LOGICAL MODSIT

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDDGTP'  ! set for subroutine

C Ground temperature profiles....
      call edisp(iuout,' ')  
      write(outs,'(a,i3,a)')' There are currently ',NGRDP,' profiles.'      
      call edisp(iuout,outs) 
      helptopic='ground_temp_profile'
      call gethelptext(helpinsub,helptopic,nbhelp)
      if(NGRDP.gt.0)then
        CALL EDISP(IUOUT,' ')
        do 22 igrdp=1,NGRDP    
          WRITE(OUTS,'(A,I5)')'Profile number: ',IGRDP
          CALL EDISP(IUOUT,OUTS)
          CALL EDISP(iuout,'Ground temperature profile Jan-Jun:')
          WRITE(OUTS,'(12F6.1)')(UGRDTP(J,IGRDP),J=1,6)
          call edisp(iuout,outs)
          CALL EDISP(iuout,'Ground temperature profile Jul-Dec:')
          WRITE(OUTS,'(12F6.1)')(UGRDTP(J,IGRDP),J=7,12)
          call edisp(iuout,outs)
          CALL EDISP(IUOUT,' ')
 22     continue
        CALL EASKABC(' ','Options:',
     &             'edit profiles','add profile',' ',IRT,nbhelp)
        if(IRT.eq.1)then
          if(NGRDP.gt.0)then
            ifoc=1
            CALL EASKI(IFOC,' ',' Index of profile to edit?',
     &        1,'F',NGRDP,'F',1,'edit profile',IERI,nbhelp)
            if(ieri.eq.-3) then
              modsit=.false.
              return
            endif
            goto 93
          else
            call usrmsg('No profiles known.','Choose add!','W')
            RETURN
          endif
        elseif(IRT.eq.2)then
          if(NGRDP.lt.MGRDP)then
            NGRDP=NGRDP+1
            ifoc=NGRDP
            goto 93
          else
            call usrmsg('Additional ground temperature profiles',
     &                'not allowed in current model!','W')
            RETURN
          endif
        endif
      ELSE
        CALL EDISP(IUOUT,'No ground temperature profiles defined!')
        if(NGRDP.lt.MGRDP)then
          NGRDP=NGRDP+1
          ifoc=NGRDP
          goto 93
        else
          call usrmsg('Additional ground temperature profiles',
     &                'not allowed in current model!','W')
          RETURN
        endif
      ENDIF

   93 WRITE(HOLD,'(1X,6F7.2)')(UGRDTP(J,IFOC),J=1,6)
      CALL EASKS(HOLD,' Temperatures January-June:',' ',72,' ',
     &    'gr prof jan-jun',IER,nbhelp)
      NV = iCountWords(HOLD)
      CALL EDISP(IUOUT,' Ground temperature profiles of ')
      CALL EDISP(IUOUT,HOLD)
      CALL EDISP(IUOUT,'will be used for January thro June.')
      if(NV.ne.6)goto 93
      K=0
      DO 94 J=1,6
        CALL EGETWR(HOLD,K,GV,0.,0.,'-','profile',IER)
        IF(IER.NE.0)GOTO 93
        UGRDTP(J,IFOC)=GV
   94 CONTINUE
   95 hold = ' '
      WRITE(HOLD,'(1X,6F7.2)')(UGRDTP(J,IFOC),J=7,12)
      CALL EASKS(HOLD,' Temperatures July-December:',' ',72,' ',
     &    'gr prof jul-dec',IER,nbhelp)
      NV = iCountWords(HOLD)
      CALL EDISP(IUOUT,' Ground temperature profiles of ')
      CALL EDISP(IUOUT,HOLD)
      CALL EDISP(IUOUT,'will be used for July thro December.')
      if(NV.ne.6)goto 95
      K=0
      DO 96 J=7,12
        CALL EGETWR(HOLD,K,GV,0.,0.,'-','profile',IER)
        IF(IER.NE.0)GOTO 95
        UGRDTP(J,IFOC)=GV
   96 CONTINUE
      MODSIT=.true.
      RETURN
      END

C ********** EDDSHD **************
C EDDSHD Sets up shading and insolation files

      SUBROUTINE EDDSHD
#include "building.h"
#include "model.h"
#include "geometry.h"

      common/OUTIN/IUOUT,IUIN

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      character outs*248
      logical XST,OK,DOK,unixok

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDDSHD'  ! set for subroutine

      if(ncomp.eq.0)then
        return
      endif

C Check if Unix-based or DOS based.
      call isunix(unixok)

      do 42 iz=1,ncomp
        if(ISI(iz).eq.1)then
          call FINDFIL(LSHAD(iz),XST)
          if(XST)then
            write(outs,'(3a)') 'Shading for ',zname(iz)(1:lnzname(iz)),
     &        ' is out of date.'
            call edisp(iuout,outs)
          endif
        endif
  42  continue
      helptopic='reflect_site_change'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKABC(' Reflect site change in shading options:',' ',
     &  'recalculate (silent)','recalculate (interactive)','continue',
     &  IRT,nbhelp)
      if(IRT.eq.1.or.IRT.eq.2)then
        if(IRT.eq.1)then
        endif
        do 43 iz=1,ncomp
          if(ISI(iz).eq.0) goto 43
          call FINDFIL(LSHAD(iz),XST)

C Get logical name of terminal type, expand model name
C to include the path and create a string to drive ish.
C If user asked for silent recalculation then do each
C one in forground in an xterm with `-act update_silent`
C as the command line.
          if(IRT.eq.1)then
            call comissionish(iz,'sra',ier)
          else
            DOK=.true.
            write(outs,'(3a)') 'Proceed with shading for ',
     &        zname(iz)(1:lnzname(iz)),' ?'
            CALL ASKOK(outs,' ',OK,DOK,nbhelp)
            if(OK)then
              call comissionish(iz,'ira',ier)
            endif
          endif
  43    continue
        call edisp(iuout,
     &       'Update of zones with shading files complete.')
      endif
      return
      end

