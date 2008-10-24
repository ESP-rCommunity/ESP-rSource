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
C  EDSITE: edits site/general information for system configuration file.
C  Setuptemporal: coordinates access to and initial creation of
C                 temporal data to be associated with a model.
C  PECONV: conversion factors for primary energy units.
C  Calenmanage: sets up and manages a calendar for ESP-r.
C  Calenprint:  displays a calendar for year iyear (or a few months)

C ************* EDSITE 
C EDSITE Edits site/general information for system configuration file.
C ITRU unit number for user output, IER=0 indicates no error.
      SUBROUTINE EDSITE(ITRC,IER)
#include "building.h"
#include "tdf2.h"
#include "sbem.h"

      integer lnblnk  ! function definition
      integer igraphiclib  ! external definition

      COMMON/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/C4/XLAT,XLON
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      common/PREC8/SLAT,SLON

      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      COMMON/C5/IXPOS,GREF,ITGREF,GREF12(12),SREF,NSNOW(12),SNFNAM
      CHARACTER*72 SNFNAM
      common/C5R/SKYR,GRDR,BLDR
      common/C14/UGRDTP(12,MGRDP),NGRDP
      common/C21/IFCFG,cfgroot,LCFGF
      common/user/browse

C IPV description.
      common/IPVF/lipvdatf
      common/IPVSEA/nipvassmt,nipvdispjd,ipvastjd(MIPVA),
     &              ipvafnjd(MIPVA),ipvdispjd(10)
      common/CFGV/icfgv

C CPCALC description
      common/CPCALC/icpcon,ble,bwi,bhi,blox,bloy,bloz,orient,irt,ra,
     &              sbh,pad,wvpe

C Variables for weekdays, and weekends.
C Assume: Mon=1, Tue=2, Wed=3, Thu=4, Fri=5, Sat=6, Sun=7
      common/wkdtyp/idwe1,idwe2,wkd1,wkd2

C Images.
      common/IMAGF/imgfmt(10),imgfoc(10),limgfil(10),noimg,iton
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
      character zname*12,zdesc*64,SEXP*31,SALT*31,hold*72,ZN*12

C ITEMS is menu for site/context items.
      character*35 ITEMS(29)
      character*72 GTGEOM
      character lipvdatf*72,H*72,outs*124,ltmp*72
      character cfgroot*24,LCFGF*72
      character imgfmt*4,imgfoc*4,limgfil*72
      character act*1,GTNAME*15
      character D64*64  ! default string for editing
      character ipvact*3  ! to denote whether in cfd file or external

      integer idwe1T,idwe2T,iyearT   ! local values for editing.

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
C      WRITE(SALT(8),53)SKYR,GRDR,BLDR
      SEXP=SALT(IXPOS)

C Begin with high level menu.
    3 INO=-4
      IER=0
      WRITE(ITEMS(1),'(A,F6.2)')'a site latitude: ',XLAT
      WRITE(ITEMS(2),'(A,F6.2)')'b longitude difference:',XLON
      if(iSlr_half_hr_flg.eq.0)then
        WRITE(ITEMS(3),'(A)')   'c solar timing: hour centred'
      elseif(iSlr_half_hr_flg.eq.1)then
        WRITE(ITEMS(3),'(A)')   'c solar timing: half-hour centred'
      else
        WRITE(ITEMS(3),'(A)')   'c solar timing: not defined'
      endif

      WRITE(ITEMS(4),'(A,A)')   'c exposure: ',SEXP(1:21)
      IF(ITGREF.EQ.1) THEN
        WRITE(ITEMS(5),'(a,f5.3,a)')
     &                     'd grd reflectivity: const. (',GREF,')'
      ELSEIF(ITGREF.EQ.2) THEN
        ITEMS(5) =         'd grd reflectivity: simple'
      ELSEIF(ITGREF.EQ.3) THEN
        ITEMS(5) =         'd grd reflectivity: advanced'
      ENDIF
      WRITE(ITEMS(6),'(4a)')    'e weekends: ',wkd1,'/',wkd2
      WRITE(ITEMS(7),'(a,I4)')  'f year: ',IYEAR
      ITEMS(8) =                'g calendar (holidays)            '
      ITEMS(9) =                'h building address               '
      ITEMS(10)=                'i client address                 '
      ITEMS(11)=                'j simulationist address          '
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
      ITEMS(22)=                'r impose  measured data          '
      ITEMS(23)=                '  ___________________________    '
      WRITE(ITEMS(24),'(a,i3)') 's define images: ',noimg
      ITEMS(25)=                't UK NCM specifications          '
      ITEMS(26)=                'u transient orientation          '
      ITEMS(27)=                ' _____________________________ '
      ITEMS(28)=                '? help                           '
      ITEMS(29)=                '- exit this menu                 '
      nitms=29

C Help text for this menu.
      H(1)='This menu allows site and miscellaneous data to'
      H(2)='be defined. In most cases the default entries will'
      H(3)='be acceptable. An explanatyion of the less obvious'
      H(4)='entries follows.'
      H(5)=' '
      H(6)='Primary energy conversions are used to convert'
      H(7)='predicted energy demands (for lights, small power,'
      H(8)='heating etc.) to equivalent primary energy units.'
      H(9)=' '
      H(10)='Dispersed demands are schedules of demands for items'
      H(11)='such as pumps, lifts etc, that are not simulation'
      H(12)='(although they could be). These items may then be'
      H(13)='displayed on an IPV alongside data relating to'
      H(14)='simulated items.'
      H(15)=' '
      H(16)='An integrated performance view (IPV) definition'
      H(17)='specifies the assessments to be included in an IPV,'
      H(18)='what portion(s) of the model to cover, the time'
      H(19)='period to consider and any scaling factors to apply.'
      H(20)=' '
      H(21)='Situation specific wind pressure coefficent sets may'
      H(22)='be calculated using CPCALC for later use in network'
      H(23)='air flow modelling.'
      H(24)=' '
      H(25)='Images are normally in GIF format although other'
      H(26)='formats may be used where an appropriate display tool'
      H(27)='exists.'
      H(28)=' '
      H(29)='Where the time averaging method applied to climate'
      H(30)='data is known this should be set via the toggle.'

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
                write(ZN,'(A)') zname(iz)
                LNZ=max(1,LNBLNK(ZN))
                write(outs,'(3a)') 'Shading for ',ZN(1:lnz),
     &            ' is out of date.'
                call edisp(iuout,outs)
              endif
            endif
  42      continue
          h(1)='Shading should be recalculated when site data'
          h(2)='changes. Each affected zone will be done in turn.'
          CALL EASKABC(
     &        'Site changes may require shading adjustments',
     &        'Options:',
     &        'recalculate (silent)','recalculate (interactive)',
     &        'cancel',IRT,2)
          if(IRT.eq.1.or.IRT.eq.2)then
            call edisp(iuout,'Processing zones - may take some time.')
            if(IRT.eq.1)then
              call edisp(iuout,
     &           'Control will be returned to the Project Manager.')
            endif
            do 43 iz=1,ncomp
              if(ISI(iz).eq.0) goto 43
              call FINDFIL(LSHAD(iz),XST)
              write(ZN,'(A)') zname(iz)
              LNZ=max(1,LNBLNK(ZN))
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
                  h(1)='Each zone is calculated in turn. If you do '
                  h(2)='one zone then you should do all zones.'
                  write(outs,'(3a)') 'Proceed with shading for ',
     &              zn(1:lnz),'?'
                  CALL ASKOK(outs,' ',OK,DOK,2)
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
        CALL PHELPD('Model context',30,'-',0,0,IER)

      ELSEIF(INO.EQ.1)THEN

        H(1)='Site latitude in degrees, where North is +ve, e.g. 55.9'
        H(2)='for Glasgow.'
        CALL EASKR(SLAT,' ','Site latitude?',
     &       -89.9,'W',89.9,'W',55.9,'site latitude',IER,2)
        IF(IER.EQ.0)XLAT=SLAT
        MODSIT=.true.
        modish=.true.

      ELSEIF(INO.EQ.2)THEN

        H(1)='Site longitude difference is the difference in degrees'
        H(2)='from the local reference meridian, East is +ve, e.g.'
        H(3)='-4.1 for Glasgow (relative to the Greenwich Meridian).'
        CALL EASKR(SLON,' ','Longitude difference?',
     &       -15.0,'W',15.0,'W',-4.1,'longitude difference',IER,3)
        IF(IER.EQ.0)XLON=SLON
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

C        WRITE(SALT(8),53)SKYR,GRDR,BLDR
C  53    FORMAT('user defined)
        SALT(8)='user defined'

        H(1)='The site exposure defines the relative view factors'
        H(2)='between a vertical surface and the sky (S), ground (G)'
        H(3)='and surrounding buildings (B). These data are used in'
        H(4)='the calculation of external longwave radiation exchange.'
        H(5)=' '
        H(6)='The following options are available.'
        H(7)=' '
        H(8)='                          View factor to:  '
        H(9)='Context                   S     G     B    '
        H(10)='=========================================  '
        H(11)='urban (normal):          0.36  0.36  0.28  '
        H(12)='urban (low density):     0.41  0.41  0.18  '
        H(13)='rural:                   0.45  0.45  0.10  '
        H(14)='urban (equal weighting): 0.33  0.33  0.34  '
        H(15)='urban (B emphasised):    0.15  0.33  0.52  '
        H(16)='isolated rural:          0.50  0.50  0.00  '
        H(17)='totally enclosed:        0.00  0.00  1.00  '
        H(18)='user defined:             -     -     -    '
        H(19)='=========================================  '
        IX=1
        CALL EPICKS(IX,IVALS,' ','Site Exposure',
     &                31,8,SALT,'Site Exposure',IER,19)
        IF(IX.EQ.0)THEN
          INO=-4
          GOTO 3
        ENDIF

        IXPOS=IVALS(1)
        SEXP=SALT(IXPOS)
        IF(IXPOS.EQ.8)THEN
          H(1)='0 <= Sky view factor <= .99'
  290     CALL EASKR(SKYR,' ','Sky view factor?',
     &         0.0,'W',0.99,'W',0.33,'sky view factor',IER,1)
          H(1)='0 <= Ground view factor <= .99'
          CALL EASKR(GRDR,' ','Ground view factor?',
     &         0.0,'W',0.99,'W',0.33,'ground view factor',IER,1)
          H(1)='0 <= Surroundings view factor <= .99'
          CALL EASKR(BLDR,' ','Surroundings view factor?',
     &         0.0,'W',0.99,'W',0.33,'surroundings view factor',IER,1)
          IF(SKYR.LT.0.0.or.GRDR.LT.0.0.or.BLDR.LT.0.0)then
            CALL USRMSG(' ','View factor cannot be negative!','W')
            GOTO 290
          ENDIF
          IF(SKYR.GT.1.0.or.GRDR.GT.1.0.or.BLDR.GT.1.0)then
            CALL USRMSG(' ','View factor cannot be > 1.0!','W')
            GOTO 290
          ENDIF
          IF((SKYR+GRDR+BLDR).GT.1.0)THEN
            CALL USRMSG(' ','Total view factor cannot be > 1.0!','W')
            GOTO 290
          ENDIF
          IF(ABS(SKYR+GRDR+BLDR-1.0).GT.0.001)THEN
            CALL USRMSG(' ','Total view factor must = 1!','W')
            GOTO 290
          ENDIF
        ENDIF
        MODSIT=.true.

      ELSEIF(INO.EQ.5)THEN

C Ground reflectance type
        h(1)='Ground shortwave reflectance can be determined'
        h(2)='from one of three methods:'
        h(3)='Constant = time invariant value;'
        h(4)='Simple   = 12 monthly values, with user'
        h(5)='              defined snow cover;'
        h(6)='Advanced = 12 monthly values, with hourly'
        h(7)='              snow depth from weather file.'
        CALL EASKABCD(' ','Ground reflectance method?',
     &      'constant','monthly simple','mnthly advanced','cancel',IW,7)
C---- Constant ground reflectance        
        if (IW.EQ.1) then
          ITGREF=1
          H(1)='0 <= ground reflectance <= .99'
          CALL EASKR(GREF,' ',' Ground reflectance?',
     &        0.0,'W',0.99,'W',0.2,'ground reflectance',IER,1)
          MODSIT=.true.
C---- Simple or advanced models: ask for monthly values 
        elseif (IW.EQ.2 .OR. IW.EQ.3) then
          ITGREF=IW
          
          h(1)='Suggested ground reflectance (no snow):'
          h(2)='  bituminous/gravel roof 0.13'
          h(3)='  dry, bare ground       0.20'
          h(4)='  weathered concrete     0.22'
          h(5)='  grass                  0.26'
          h(6)='  sand                   0.40'
          h(7)='  light building surface 0.60'
   83     CONTINUE
          HOLD=' '
          WRITE(HOLD,'(1X,6F7.2)')(GREF12(J),J=1,6)
          CALL EASKS(HOLD,' ',
     &      'January thro June ground reflectance (no snow)?',
     &      72,'0.2 0.2 0.2 0.2 0.2 0.2','gr albedo jan-jun',IER,7)
          CALL CHITMS(HOLD,NV)
          IF (NV.NE.6) GOTO 83
          K=0
          DO 84 J=1,6
            CALL EGETWR(HOLD,K,GV,0.0,0.99,'W','profile',IER)
            IF (IER.NE.0) GOTO 83
            GREF12(J)=GV
   84     CONTINUE
   85     CONTINUE
          HOLD = ' '
          WRITE(HOLD,'(1X,6F7.2)')(GREF12(J),J=7,12)
          CALL EASKS(HOLD,' ',
     &      'July thro December ground reflectance (no snow)?',
     &       72,'0.2 0.2 0.2 0.2 0.2 0.2','gr albedo jul-dec',IER,7)
          CALL CHITMS(HOLD,NV)
          IF (NV.NE.6) GOTO 85
          K=0
          DO 86 J=7,12
            CALL EGETWR(HOLD,K,GV,0.0,0.99,'W','profile',IER)
            IF (IER.NE.0) GOTO 85
            GREF12(J)=GV
   86     CONTINUE
          MODSIT=.true.
C---- Simple model: ask for average albedo of snow, and number of days
C     with snow on the ground
          if (IW.EQ.2) then
            H(1)='Suggested ground reflectance (with snow):'
            H(2)='  city centre    0.20'
            H(3)='  urban          0.40'
            H(4)='  rural          0.50'
            H(5)='  isolated rural 0.70'
            CALL EASKR(SREF,' ',
     &        ' Average ground reflectance in the presence of snow?',
     &        0.0,'W',0.99,'W',0.4,'snow reflectance',IER,5)
            MODSIT=.true.
            H(1)='For each month, enter the number of days with snow'
            H(2)='d=epth typically greater than 5 cm.'
   87       CONTINUE
            HOLD=' '
            WRITE(HOLD,'(6(1X,I2))')(NSNOW(J),J=1,6)
            CALL EASKS(HOLD,' ',
     &        'Number of days in January thro June with snow?',
     &        72,'0 0 0 0 0 0','days with snow jan-jun',IER,2)
            CALL CHITMS(HOLD,NV)
            IF (NV.NE.6) GOTO 87
            K=0
            DO 88 J=1,6
              CALL EGETWI(HOLD,K,IV,0,31,'W','profile',IER)
              IF (IER.NE.0) GOTO 87
              NSNOW(J)=IV
   88       CONTINUE
   89       CONTINUE
            HOLD = ' '
            WRITE(HOLD,'(6(1X,I2))')(NSNOW(J),J=7,12)
            CALL EASKS(HOLD,' ',
     &        'Number of days in July thro December with snow?',
     &        72,'0 0 0 0 0 0 ','days with snow jul-dec',IER,3)
            CALL CHITMS(HOLD,NV)
            IF (NV.NE.6) GOTO 89
            K=0
            DO 90 J=7,12
              CALL EGETWI(HOLD,K,IV,0,31,'W','profile',IER)
              IF (IER.NE.0) GOTO 89
              NSNOW(J)=IV
   90       CONTINUE
          endif
C---- Advanced model: ask for albedo of fresh snow, and file containing
C     hourly snow depths
          if (IW.EQ.3) then
            H(1)='Typical values for ground reflectance with snow:'
            H(2)='  city centre:   0.20'
            H(3)='  urban          0.40'
            H(4)='  rural          0.50'
            H(5)='  isolated rural 0.70'
            CALL EASKR(SREF,' ',
     &        ' Ground reflectance in the presence of fresh snow?',
     &        0.0,'W',0.99,'W',0.5,'snow reflectance',IER,5)
C    
   92       CONTINUE
            H(1)='The snow depth file contains hourly values of snow'
            H(2)='depth. The data should be formatted as a single'
            H(3)='column within an ASCII file, with the snow depth'
            H(4)='given in centimeters for each of the 8760 hours of'
            H(5)='the year.'
            CALL EASKS(SNFNAM,'Enter name of snow depth file',
     &        ' ',72,' ','Snow depth file',IER,5)
            CALL FINDFIL(SNFNAM,FOUND)
            IF(.NOT.FOUND) THEN
              H(1)='Please ensure that the file exists, or selectpress '
              H(2)='`OK` if the file will be created later.'
              DOK=.FALSE.
              CALL ASKOK('File not found: continue?',' ',OK,DOK,2)
              IF (.NOT.OK) GOTO 92
            ENDIF
          endif
        endif

      ELSEIF(INO.EQ.6)THEN

C Weekends def.
        call edisp(iuout,'Please define weekend days.')
        H(1)='Some locations reserve days other than Saturday and'
        H(2)='Sunday as `weekends`. You can define these days or'
        H(3)='set a one day weekend by defining the second day'
        H(4)='equal to the first.'
        idwe1T=idwe1
        CALL EASKI(idwe1T,'Day index (Mon=1,Tue=2, ...) for',
     &    'the first day of the weekend?',
     &        1,'F',7,'F',6,'1st weekend index',IERI,4)
        if(ieri.eq.-3) goto 3
        idwe1=idwe1T
        wkd1=wkday(idwe1)

        idwe2T=idwe2
        CALL EASKI(idwe2T,'Day index (Mon=1,Tue=2, ...) for',
     &    'the second day of the weekend?',
     &        1,'F',7,'F',7,'2nd weekend index',IERI,4)
        if(ieri.eq.-3) goto 3
        idwe2=idwe2T
        wkd2=wkday(idwe2)
        MODSIT=.true.

      ELSEIF(INO.EQ.7)THEN

C Change the year and if calendar has not been set up initialise
C and then list the months of the year.
        H(1)='Control and scheduling actions the year to be defined.'
        H(2)='You may accept the one specified with the climate'
        H(3)='file or provide an alternative.'
        iyeart=iyear
        CALL EASKI(iyeart,' ','Simulation year?',
     &       1900,'W',2010,'W',2001,'year',IERI,3)
        if(ieri.eq.-3) goto 3
        iyear=iyeart
        MODSIT=.true.
        if(nbdaytype.eq.0)then
          call calenmanage('i',ier)
        endif
        dok=.false.
        H(1)='You can use the calandar to check that a day or days'
        H(2)='fall on a required date (e.g. to test a heating system`s'
        H(3)='response under a cold, Monday start).'
        call askok(' ','View calendar?',ok,dok,3)
        if(ok)then
          loop=12
          loopst=1
          call calenprint(iuout,'t',iyear,loopst,loop,ier)
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

C The address of the model (same data fields as entered in the ncm file.
        call edisp(iuout,'Details for code complience...')
        write(outs,'(2a)')'Project name: ',pjname(1:lnblnk(pjname))
        call edisp(iuout,outs)
        write(outs,'(2a)')'Building address: ',baddress
        call edisp(iuout,outs)
        write(outs,'(2a)')'Building city: ',bcity(1:lnblnk(bcity)) 
        call edisp(iuout,outs)
        write(outs,'(2a)')'Building Postcode: ',buildingPC  
        call edisp(iuout,outs)    
        DOK=.FALSE.
        CALL ASKOK('Edit these code complience details?',' ',OK,DOK,2)
        if(OK)then
          H(1)='Input a succinct description of the project name here.'
          write(D64,'(a)') 'notional model not named'
          CALL EASKS(pjname,'Code complience project name?',
     &      '(<64 characters)',64,D64,'project name',IER,1)
          H(1)='Input first line of building address here'
          write(D64,'(a)') 'address not provided'
          CALL EASKS(baddress,'building address?','(<64 characters)',
     &      64,D64,'build address',IER,1)
          H(1)='Input the building city here'
          write(D64,'(a)') 'city not provided'
          CALL EASKS(bcity,'building city?','(<64 characters)',
     &      64,D64,'city building',IER,1)
          H(1)='Input for: building post code'
          write(D64,'(a)') 'postcode not provided'
          CALL EASKS(buildingPC,'building post code ?',
     &      '(<64 characters)',64,D64,'building PC',IER,1)
        endif

      ELSEIF(INO.EQ.10)THEN

C The address of the client (same data fields as entered in the ncm file.
        call edisp(iuout,'Details of client/owner...')
        write(outs,'(2a)')'Building owner name: ',ownername
        call edisp(iuout,outs)   
        write(outs,'(2a)')'Building owner telephone: ',ownertel
        call edisp(iuout,outs)   
        write(outs,'(2a)')'Building owner address: ',owneraddr 
        call edisp(iuout,outs) 
        write(outs,'(2a)')'Building owner city: ',ownercity 
        call edisp(iuout,outs) 
        write(outs,'(2a)')'Building owner Postcode: ',ownerPC
        call edisp(iuout,outs)     
        DOK=.FALSE.
        CALL ASKOK('Edit these owner/client details?',' ',OK,DOK,2)
        if(OK)then
          H(1)='Input a succinct description of the owner name here'
          write(D64,'(a)') 'owner/client not named'
          CALL EASKS(ownername,'Name of owner?','(<64 characters)',
     &      64,D64,'owner name',IER,1)
          H(1)='telephone number of the owner'
          write(D64,'(a)') 'owner/client phone not given'
          CALL EASKS(ownertel,'Telephone of owner?','(<64 characters)',
     &       64,D64,'owner tel',IER,1)
          H(1)='Address of the owner'
          write(D64,'(a)') 'owner/client address not entered'
          CALL EASKS(owneraddr,'Address of owner?','(<64 characters)',
     &      64,D64,'owner address',IER,1)
          H(1)='City (for owner)'
          write(D64,'(a)') 'owner/client city not named'
          CALL EASKS(ownercity,'City of owner?','(<64 characters)',
     &      64,D64,'owner city',IER,1)
          H(1)='Post code (for owner)'
          write(D64,'(a)') 'owner/client postcode not entered'
          CALL EASKS(ownerPC,'Post code for owner?','(<64 characters)',
     &      64,D64,'owner post code',IER,1)
        endif

      ELSEIF(INO.EQ.11)THEN

C Address of simulationist.
        call edisp(iuout,'Details of simulationist...')
        write(outs,'(2a)')'Simulationist name: ',certifname 
        call edisp(iuout,outs)     
        write(outs,'(2a)')'Simulationist telephone: ',certTel 
        call edisp(iuout,outs)     
        write(outs,'(2a)')'Simulationist address: ',certaddress   
        call edisp(iuout,outs)   
        write(outs,'(2a)')'Simulationist city: ',certcity  
        call edisp(iuout,outs)    
        write(outs,'(2a)')'Simulationist postcode: ',certPC  
        call edisp(iuout,outs)          
        DOK=.FALSE.
        CALL ASKOK('Edit the simulationist details?',' ',OK,DOK,2)
        if(OK)then
          H(1)='Person or team preforming the assessment.'
          write(D64,'(a)') 'simulationist not named'
          CALL EASKS(certifname,'Name of simulationist?',
     &      '(<64 characters)',64,D64,'certifier name',IER,1)
          H(1)='Simulationist/certifier telephone number'
          write(D64,'(a)') 'simulationist phone not given'
          CALL EASKS(certTel,'Telephone of simulationist?',
     &      '(<64 characters)',64,D64,'certifier tel',IER,1)
          H(1)='Simulationist/certifier address'
          write(D64,'(a)') 'simulationist address not given'
          CALL EASKS(certaddress,'Address of simulationist?',
     &      '(<64 characters)',64,D64,'certifier address',IER,1)
          H(1)='Simulationist/certifier city'
          write(D64,'(a)') 'simulationist city not given'
          CALL EASKS(certcity,'City of simulationist?',
     &      '(<64 characters)',64,D64,'certifier city',IER,1)
          H(1)='Simulationist/certifier post code'
          write(D64,'(a)') 'simulationist postcode not given'
          CALL EASKS(certPC,'Post code of simulationist?',
     &      '(<64 characters)',64,D64,'certifier Post code',IER,1)
        endif

      ELSEIF(INO.EQ.13)THEN

C Ground topology.
        h(1)='If there is an existing ground toplogy you may browse'
        h(2)='or edit its defining data. To create a new toplogy you'
        h(3)='may input the defining data manually or elect to'
        h(4)='click on points on a bitmap.'
        call easkabcd(' ','Ground topology defining options:',
     &    'select current','create manually','create via bitmap',
     &    'cancel',iwgt,4)
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
        H(1)='Ground temperature profiles define the monthly soil'
        H(2)='temperatures at some arbitrary depth (typically 1.5m)'
        H(3)='for use as aboundary condition within a simulation.'
        H(4)='Up to 12 profiles may be defined, each comprising 12'
        H(5)='monthly temperature values.'
        CALL EASKABC(' ','Ground temperature profile options:',
     &   'list','edit','add',IRT,5)
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
            H(1)='Provide index of the ground temperature profile'
            H(2)='to be edited.'
            CALL EASKI(IFOC,' ','Profile to edit?',
     &        1,'F',NGRDP,'F',1,'edit profile',IERI,2)
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
        H(1)='Temperatures are typically defined at a 1.5m depth.'
        CALL EASKS(HOLD,' ','Temperatures, January- thro June?',
     &      72,' ','gr prof jan-jun',IER,1)
        CALL CHITMS(HOLD,NV)
        if(NV.ne.6)goto 93
        K=0
        DO 94 J=1,6
          CALL EGETWR(HOLD,K,GV,0.,0.,'-','profile',IER)
          IF(IER.NE.0)GOTO 93
          UGRDTP(J,IFOC)=GV
   94   CONTINUE
   95   hold = ' '
        WRITE(HOLD,'(1X,6F7.2)')(UGRDTP(J,IFOC),J=7,12)
        H(1)='Temperatures are typically defined at a 1.5m depth.'
        CALL EASKS(HOLD,' ','Temperatures, July thro December?',
     &      72,' ','gr prof jan-jun',IER,1)
        CALL CHITMS(HOLD,NV)
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
        h(1)='Energy demand may be converted to primary energy'
        h(2)='(e.g. as comsumed at a power station) and also to'
        h(3)='equivalent emission values.'
        CALL EASKABC(' ','Energy demand conversion options:',
     &   'initialise and edit','edit','cancel',IR,3)
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

C IPV data.
        if(icfgv.lt.4)then
          h(1)='This file holds data required to later construct'
          h(2)='an `Integrated Performance View` of simulation'
          h(3)='results.'
          lr=lnblnk(cfgroot)
          if(lnblnk(lipvdatf).eq.0)then
            write(ltmp,'(2a)') cfgroot(1:lr),'.ipv'
          elseif(lipvdatf(1:7).eq.'UNKNOWN')then
            write(ltmp,'(2a)') cfgroot(1:lr),'.ipv'
          else
            ltmp=lipvdatf
          endif
 142      CALL EASKS(ltmp,' ','IPV description file?',
     &               72,'xxx.ipv','IPV file',IER,3)
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
        call setuptemporal(ITRC,MODSIT,IER)
C          MODSIT=.true.

      ELSEIF(INO.EQ.24)THEN

C Manage images or browse by listing out current contents of image folder.
        call manageimages(modsit)

      ELSEIF(INO.eq.25)then

C Enter the definitions for UK NCM related data.
        call sbmedit
      ELSEIF(INO.eq.26)then

C Changing orientation as a function of time.
        CALL ROAMING
      ELSE
        GOTO 3
      ENDIF
      GOTO 3

      END

c ****** ROAMING *****
C This subroutine manages data associated with rotating a model as a
C function of time. Important variables are defined in roam.h
      SUBROUTINE ROAMING
#include "building.h"
#include "roam.h"
      
      integer lnblnk  ! function definition

      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/POPHELP/H(60)

      CHARACTER*72 h,DFLTNAME,LCFGF,OUTS
      CHARACTER CFGROOT*24,TEMPSTR*30
      INTEGER IOK
      LOGICAL OK,DOK

C Initialise variables
      IOK=0
      OK=.FALSE.
      DOK=.TRUE.

C Help message
      H(1) ='This facility is used to change orientation of the model'
      H(2) ='as a function of time. For this purpose a roaming file is'
      H(3) ='required. This file includes information about locations'
      H(4) ='that a model will exist at and the times of validity.'
      H(5) =' '
      H(6) ='When defining the file for the first time in a model the '
      H(7) ='user should generate a template (option to do this follows'
      H(8) ='this message) and use a text editor to add information to '
      H(9) ='the template. If you have previously defined a roaming '
      H(10)='file then you can just view its contents within Project'
      H(11)='Manager. All editing should be done by a third party text'
      H(12)='editor.'
      H(13)=' '
      H(14)=' '
      H(15)=' '
      H(16)=' '
      H(17)=' '
      H(18)=' '
      H(19)=' '
      WRITE(DFLTNAME,'(2A)')CFGROOT(1:LNBLNK(CFGROOT)),'.roam'
      IF(IROAM.NE.1)THEN
        LROAM=DFLTNAME
        IROAM=1
        CALL PHELPD('Roaming Introduction',19,'-',0,0,IER)
        CALL EASKAB(' ','Roaming file actions:',
     &    'create template','cancel and exit',IOK,19)
        IF(IOK.EQ.1)THEN
          CALL WROAM
          CALL RROAM
        ENDIF
      ELSE
        CALL EASKS2CMD(LROAM,' Roaming file? (see help)',
     &  ' ','dereference','browse',IACT,72,' ','Roaming file',IER,19)
        IF(IACT.EQ.1)THEN
          IROAM=0
          CALL EDISP(IUOUT,'Updating configuration file ...')
          CALL EMKCFG('-',IER)
        ELSE
          CALL RROAM
        ENDIF
      ENDIF

C Display file contents
      CALL ASKOK('View location information?',' ',OK,dok,19)
      IF(OK)THEN
        CALL EDISP(IUOUT,' ')
        WRITE(OUTS,'(A,F3.1)')'ESP-r Roaming file version number ',VERS
        CALL EDISP(IUOUT,OUTS)
        CALL EDISP(IUOUT,
     &  'Location                      Latitude Longitude')
        DO 1000 ILOC=1,NLOC
          WRITE(OUTS,'(A30,1X,F5.1,5X,F5.1)')
     &    LOCRM(ILOC),LOCLT(ILOC),LOCLG(ILOC)
          CALL EDISP(IUOUT,OUTS)
 1000   CONTINUE
        CALL ASKOK('View cruise schedule?',' ',OK,dok,19)
        IF(OK)THEN
          CALL EDISP(IUOUT,
     &    'start hr, dy, mth, orientation, day type, location')
          DO 1100 IPDR=1,NPDR
            IF(LLOCT(IPDR).EQ.0)THEN
              TEMPSTR='cruising'
            ELSE
              TEMPSTR=LOCRM(LLOCT(IPDR))
            ENDIF
            WRITE(OUTS,'(5X,I3,1X,I3,2X,I3,7X,I4,5X,I2,7X,A30)')
     &      LHRS(IPDR),LDYS(IPDR),LMTS(IPDR),LORNT(IPDR),
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
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/user/browse
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth

C Images.
      common/IMAGF/imgfmt(10),imgfoc(10),limgfil(10),noimg,iton

      logical MODSIT,browse,XST,unixok

C ITEMS is menu for site/context items, ITEMIMG is menu for editing
C and control of project images.
      character*33 ITEMIMG(20)
      character H*72,outs*124
      character imgfmt*4,imgfoc*4,limgfil*72,limg*72,key*1,fs*1
      character iformat*4,ifocus*4
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24
      character sfile*72,snpfile*72

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
      h(1)='Images may be associated with a model for various'
      h(2)='purposes - for example to record site conditions'
      h(3)='or corresponding design drawings. Up to 10 images'
      h(4)='may be associated with a model.'
      h(5)=' '
      h(6)='The following image types are supported: gif, tiff,'
      h(7)='jpeg and X11 bitmap. To edit an image name, select'
      h(8)='it from the list.'

      CALL EMENU('Project Images',ITEMIMG,nimgitms,INOI)
      if(INOI.EQ.nimgitms)then

C Update configuration if images have changed.
        if(MODSIT.and.(.NOT.browse))then
          CALL EMKCFG('-',IER)
        endif
        return
      elseif(INOI.EQ.nimgitms-1)then
        CALL PHELPD('Model images',8,'-',0,0,IER)
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
        h(1)='Images can be associated with various aspects of a'
        h(2)='model. You can add images to the current list from'
        h(3)='either the model images folder or by entering an'
        h(4)='absolute path and file name.'
        CALL EASKABCD(' ','Project image options:',
     &    'add from standard folder','add from another folder',
     &    'delete','cancel',IOK,4)
        if(iok.eq.1)then
          if(noimg.ge.10)then
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
C                write(6,*) limg
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
              modsit=.true.
            endif
          endif
        elseif(iok.eq.2)then
          if(noimg.ge.10)then
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
C        write(6,*) 'inoi ',inoi
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
          MODSIT=.true.
        endif
      endif
      goto 36
      end

C ************* setuptemporal
C Setuptemporal coordinates access to and initial creation of
C temporal data to be associated with a model.

      SUBROUTINE setuptemporal(ITRC,MODSIT,IER)
#include "building.h"
#include "tdf2.h"
#include "control.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF
      common/user/browse
      common/paths/zonepth,netpth,ctlpth,imgpth,radpth,docpth,tmppth,
     &             dbspth
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
     
C TDF related.
C itdflg=0 none, 1 older file, 2 asci v2, 3 asci v3, -2 bin v2, -3 bin v3
      COMMON/TDFI/IUTDF,ITDFLG,IUTDFA
      COMMON/TDFFT/LTDF,LTDFA
      COMMON/TDFFH/NWPR,NITDF,NTSPH,NEXTCL,NEXTRC,LASTHD,
     &             LSTREC,NDBSTP,NUWPR
      COMMON/TDFFLG/ITEMSTD
      COMMON/TDFFLG0/DBTAG(MIT),DBTASK(MIT),DBZN(MIT),DBSN(MIT)
      common/TDFIH1/TAG(MIT),TTYPE(MIT),TMENU(MIT),TAIDE(MIT)

      common/sctl/tcps,ictyp,iclaw,cm(misc)
      common/cctl/icascf(mcom)

      logical MODSIT,browse,OK,dok,unixok
      logical FOUND,XST

      character H*72,outs*124,ltmp*72,hold*40
      character cfgroot*24,LCFGF*72
      character zonepth*24,netpth*24,ctlpth*24,imgpth*24,radpth*24
      character docpth*24,tmppth*24,dbspth*24,fs*1,msgl2*48
      character*72 LTDF,LTDFA,DLTDF
      character DBTAG*12,DBTASK*12,DBZN*15,DBSN*15
      character TAG*12,TTYPE*8,TMENU*16,TAIDE*36
      character zname*12,zdesc*64,OUTSTR*124,HDR*12
      character*34 ITEMS(7)
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12

C Action to pass to tdfedit. Prior 'update' option moved into
C esrub2e rather than invoking tdf.
C << thought required to transition between functionality of >>
C << the module tdf to the in-built functionality            >>
       character action*16

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
 42   H(1)='Temporal definitions are used to impose special'
      H(2)='boundary condition on a simulation or to modify'
      H(3)='design parameters as a simulation proceeds.'
      H(4)=' '
      H(5)='A temporal file holds one or more entities (e.g. climate'
      H(6)='data, casual gains, set points etc.) over a specified '
      H(7)='number of days and for a specified time step. These'
      H(8)='entities, depending on their type, may be associated'
      H(9)='with zones or controls within the model and will'
      H(10)='replace existing definitions.'
      H(11)=' '
      H(12)='associations - entities in the temporal file can be'
      H(13)='  associated with entities in your model depending on'
      H(14)='  the type of information. For example climate data'
      H(15)='  is associated with the whole model and infiltration'
      H(16)='  is associated with a zone.'
      H(17)=' '
      H(18)='Currently the following item types are offered: '
      H(19)=' ALLCLMT - all climate data (includes next 6 items)'
      H(20)=' BDTEXTT - ambient temperature'
      H(21)=' WINDVLT - wind velocity'
      H(22)=' WINDDRT - wind direction'
      H(23)=' RELHUMT - ambient relative humidity'
      H(24)=' DIFHSLT - diffuse horizonal radiation'
      H(25)=' GLOHSLT - global horizonal radiation'
      H(26)=' DIRSOLT - direct normal solar radiation'
      H(27)=' SETPTTT - control setpoint temperature'
      H(28)=' CASUALT - zone casual gains (combined gains)'
      H(29)=' CASUAL3T -zone casual gains (Occup/Lights/SM Pwr)'
      H(30)=' ACTIVITY -global database: gains,setpoints, etc.'
      H(31)=' BIDIRSET -Bidirectional datasets (e.g. slat angles)'
      H(32)=' VERTSOL -Vertical solar radiation data for a facade'
      H(33)=' HTCLSETP -Specify columns to read H/C setpoints.'
      H(34)='manage associations - you can add and delete associations'
      H(35)='  that have been previously defined. Note this does not'
      H(36)='  alter the contents of the temporal file.'
      H(37)=' '
      H(38)='dereference temporal file - this removes current links'
      H(39)='  between the model and entities in the temporal file.'
      H(40)='  It does not remove the temporal file.'
      H(41)=' '
      H(42)='create file - you supply a file name, if it exists it'
      H(43)='  is scanned otherwise a minimal file is created.'
      H(44)=' '
      H(45)='edit/list - contents of the current file are presented'
      H(46)='  and if you have permission you can alter its contents.'
      H(47)=' '
      H(48)='General instructions for setting up a temporal file:'
      H(49)='a) Decide the period of the data (start and end dates)'
      H(50)='   as well as frequency (1stph to 60 tsph).'
      H(51)='b) If data is to be imported from a file with columns'
      H(52)='   of data take a note of which columns are required.'
      H(53)='c) Plan the names of the temporal entities you will be'
      H(54)='   be creating as well as which zones or controls in '
      H(55)='   the model are going to be associates with the'
      H(56)='   entities in the temporal file.'
      H(57)=' '

C Begin with high level menu.
    3 INO=-4
      IER=0
      IRT=-1

      if(itdflg.eq.0)then
        if(browse)then
          call usrmsg(
     &   'No temporal data associated with this model! Because you',
     &   'are in browse mode no changes may be made to the model.','W')
          return
        endif
        H(58)='At the current time there are no temporal definitions;'
        H(59)='you must create them before you can use them.'
        nbhelp=59
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
        H(58)='An outdated file format is associated with the'
        H(59)='model. Update this by selecting the edit option or '
        H(60)='(dereferences the temporal definitions from your model).'
        nbhelp=60
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
          H(58)='You may view the temporal data associated with the'
          H(59)='model by selecting the view entities option.'
          nbhelp=59
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
          H(58)='An outdated file format is associated with the model.'
          H(59)='Update this by selecting the edit entities or option'
          H(60)='(derefence the temporal derfinition from your model).'
          nbhelp=60
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
          H(58)='You can view the temporal data associated with the'
          H(59)='model via selecting the view entities option.'
          nbhelp=59
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
          H(57)='Temporal definitions are available for editing/use.'
          nbhelp=57
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
     &      IER,5)
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
     &    72,DLTDF,'temporal definitions file',IER,5)
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
   29         FORMAT(i2,1X,A12,1X,A8,5X,A16,A16)
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

        H(1)='Temporal definitions are used to impose special'
        H(2)='boundary condition on a simulation or to modify'
        H(3)='design parameters as the simulation proceeds.'
        ltmp=LTDFA
        CALL EASKS(ltmp,' Temporal definitions file name?',
     &    ' ',72,DLTDF,'temporal definitions db name',IER,3)
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
   14     FORMAT(i2,1X,A12,1X,A8,1X,A16)
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
        h(1)='Existing associations can be removed or new '
        h(2)='associations between temporal entities and '
        h(3)='model zones and/or controls can be added. '
        itro=0
        if(ITEMSTD.eq.0)then
          call easkabc('Temporal association options:',' ',
     &      'n/a','add association','continue',ITRO,3)
          if(itro.eq.3) goto 42
        else
          call easkabcd('Temporal association options:',' ',
     &      'delete association','add association','edit association',
     &      'continue',ITRO,3)
          if(itro.eq.4) goto 42
        endif
        if(itro.eq.1)then

C Delete an association and move others up.
          h(1)='Select the index of the temporal item you do not wish'
          h(2)='use in the current model. This removes the association'
          h(3)='but does not remove the entity from the temporal file.'
          H(4)=' '
          H(5)='A cancel request leaves the model unchanged. '
          CALL EASKI(it,'Temporal item which you do not want to use',
     &      'in this model?',1,'F',itemstd,'F',1,'selec index',IERI,5)
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
            h(1)='Select the index of the association you wish to'
            h(2)='modify in the current model. '
            H(3)=' '
            H(4)='A cancel request leaves the model unchanged. '
            CALL EASKI(ite,'Which temporal association to modify?',
     &        '(index)',1,'F',itemstd,'F',1,'selec tdf assoc',IERI,4)
            if(ieri.eq.-3) goto 42
            if(ite.ne.0)then

C Reset itemstd to the item to be edited.
              call edisp(iuout,'The association details are:')
              WRITE(outs,29)ite,DBTAG(ite),DBTASK(ite),DBZN(ite),
     &             DBSN(ite)
              call edisp(iuout,outs)
              h(1)='Select the index of the temporal item you wish'
              h(2)='to associate. '
              CALL EASKI(it,' ','Associate temporal item?',
     &              1,'F',NITDF,'F',1,'selec tdf index',IERI,24)
              if(ieri.eq.-3) goto 42
            endif
          elseif(itro.eq.2)then
            h(1)='Select the index of the temporal item you wish to'
            h(2)='include in the current model. '
            H(3)=' '
            H(4)='A cancel request leaves the model unchanged. '
            CALL EASKI(it,'Temporal item index to associate with',
     &        'this model?',1,'F',NITDF,'F',1,'selec tdf index',IERI,4)
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
              call LSTCNTL(iuout,0,ijj,IER)
 777        continue
            if(found)then

C If there are possible control laws and if the user selects one of them
C copy data into cm variables for later editing.
              irtc=0
              CALL EASKABC(' Control linkage options:',' ',
     &          'use existing control','create new control','continue',
     &          IRTC,26)
              if(irtc.eq.1)then
                CALL EASKI(ispl,'Control loop to use set point data?',
     &          ' ',1,'F',ncf,'F',1,'cntrl loop for set point',IERI,26)
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
     &          'n/a','create new control','cancel',IRTC,26)
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
            H(1)='This control assumes that heating and cooling'
            H(2)='capacities are adequate for reaching the setpoints'
            H(3)='in the temporal definition file (tdf). It is only'
            H(4)='required to define the columns in the tdf where the'
            H(5)='setpoints should be read from. Defaults are column 4'
            H(6)='for heating setpoint and column 5 for cooling.'
            if(TTYPE(it)(1:6).eq.'ACTIVI')then
              CALL EASKS(HOLD,
     &        'Heating and cooling setpoint column number ',
     &        '(respectively):',40,' 5.0 6.0 ',
     &        ' track another column number(s)',IER,6)
            elseif(TTYPE(it)(1:6).eq.'HTCLSE')then     
              CALL EASKS(HOLD,
     &        'Heating and cooling setpoint column number ',
     &        '(respectively):',40,' 2.0 3.0 ',
     &        ' track another column number(s)',IER,6)
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
            h(1)='A temporal item of type SETPPT can be associated'
            h(2)='with a zone control in order to force the zone'
            h(3)='temperature to match that recorded in the temporal'
            h(4)='file. It makes use a control law 11 to do this. '
            h(5)=' '
            h(6)='If you want to use an existing control it will be '
            h(7)='updated to impliment the necessary links. If you '
            h(8)='want to create a new zone control then all of the '
            h(9)='necessary control period will be generated. '
            h(10)=' '
            h(11)='If you want to use temporal-based setpoints for'
            h(12)='several zones you will need a separate temporal '
            h(13)='item and a separate zone control law for each zone.'
            h(14)=' '
            h(15)='You will be asked about: '
            h(16)=' a) control loop index (existing or new)'
            h(17)=' b) maximum and minimum heating & cooling capacity'
            h(18)='    which should have sufficient capacity to main-'
            h(19)='    tain the setpoint.'
            h(20)=' c) the zone where the set point will be used '
            h(21)=' '
            h(22)='It is not necessary to further modify the zone '
            h(23)='control once it has been created (unless you did '
            h(24)='not guess the correct capacity. '
            h(25)=' '
            h(26)='A cancel request leaves the model unchanged. '
            CALL PHELPD('temporal-control popup',26,'-',0,0,IER)

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
                call LSTCNTL(iuout,0,ijj,IER)
              elseif(ibclaw(ijj,1,1).eq.11)then
                found=.true.
                call LSTCNTL(iuout,0,ijj,IER)
              endif
  77        continue
            if(found)then

C If there are possible control laws and if the user selects one of them
C copy data into cm variables for later editing.
              irtc=0
              CALL EASKABC(' Control linkage options:',' ',
     &          'use existing control','create new control','continue',
     &          IRTC,26)
              if(irtc.eq.1)then
                CALL EASKI(ispl,'Control loop to use set point data?',
     &          ' ',1,'F',ncf,'F',1,'cntrl loop for set point',IERI,26)
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
     &          'n/a','create new control','cancel',IRTC,26)
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
            H(1)='Heat delivery is normally set high so that the '
            H(2)='target temperature is attained. The heating minimum'
            H(3)='is normally 0 but if > 0 then heating will continue'
            H(4)='at the minimum even when the temperature is'
            H(5)='reached. '
            H(6)='In the unlikely event that you want to control to'
            H(7)='another temperature but allow no cooling capacity '
            H(8)='the control may not work properly.'
            CALL EASKS(HOLD,
     &      ' Max & Min heat capacity, Max & Min cool capacity for',
     &      ' matching another temp: ',40,' 10000. 0.0 10000. 0.0 ',
     &      ' track another max min heat cool',IER,8)
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
 352      izdef=0
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
          else
            DBZN(ITE)='ALL'
            DBSN(ITE)='ALL'            
          endif
          if(itro.eq.2)then
            dok=.false.
            h(1)='If you want to continue adding temporal refences '
            h(2)='then answer yes. '
            CALL ASKOK(' ',' Use another temporal item?',OK,DOK,2)
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

      common/pophelp/h(60)
      common/PCONV/ipconv,pcnvht,pcnvcl,pcnvlt,pcnvfn,pcnvsp,pcnvhw
      common/CONVEM/phtco2,phtnox,phtsox,pclco2,pclnox,pclsox,
     &              pltco2,pltnox,pltsox,pfnco2,pfnnox,pfnsox,
     &              pspco2,pspnox,pspsox,phwco2,phwnox,phwsox

      dimension ITEMS(18)
      character items*36,h*72,hold*40,act*1
      logical MODSIT

      MODSIT=.false.

C If act = 'i' then initialise variables.
      if(act(1:1).eq.'i'.or.act(1:1).eq.'I')then
        ipconv=1
        call easkabc('Typical patterns',' ','UK','Europe','zeros',IW,41)
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
  4   h(1) ='Use these emission patterns as an approximation of '
      h(2) ='patterns in several regions. '
      h(3) =' '
      h(4)='Emissions [grams per kWh primary energy]. '
      h(5)='Typical UK       CO2    NOx   SOx   '
      h(6)='Gas              190.   0.3    0.2    '
      h(7)='Oil              260.   2.5    0.1    '
      h(8)='Coal             320.   1.3   2.63  '
      h(9)='Electricity      612.   2.06  7.57   '
      h(10)='Where heating and DHW are gas, and lights/fans/pumps'
      h(11)='are electricity. '
      h(12) =' '
      h(13)='Typical Europe   CO2    NOx   SOx   '
      h(14)='Gas              190.   0.3   0.2    '
      h(15)='Oil              260.   2.5   5.0    '
      h(16)='Coal             320.   1.3   2.63  '
      h(17)='Electricity      360.   1.2   4.5   '
      h(18)=' '
      h(19)='Conversion demand units > primary energy units includes'
      h(20)='both the typical seasonal efficiency of the equipment '
      h(21)='and transport losses. Transmission losses assumed'
      h(22)='are 4% for gas, none for fuel oil, none for solid fuel.'
      h(23)=' '
      h(24)='Heating system efficiencies and conversions are: '
      h(25)='  gas boiler (fan assisted flue)     68%  1.53'
      h(26)='  gas boiler (open/balanced flue)    65%  1.60'
      h(27)='  gas condensing boiler              85%  1.22'
      h(28)='  gas combi boiler                   72%  1.44'
      h(29)='  solid fuel (auto feed)             60%  1.73'
      h(30)='  oil fired (standard)               70%  1.48'
      h(31)='  oil fired (condensing)             85%  1.22'
      h(32)='  gas fired warm air (fan asst flue) 80%  1.30'
      h(33)='  gas fired warm air (open/bal flue) 70%  1.48'
      h(34)=' '
      h(35)='Cooling system efficiencies and conversions are: '
      h(36)='  packaged A/C unit assuming 2.5 CoP      1.44'
      h(37)='  split A/C unit assuming 2.5 CoP         1.44'
      h(38)=' '
      h(39)='Conversion factors: '
      h(40)='  small power & lighting: 3.2-3.6 (typical UK) '
      h(41)='  small power & lighting: ? (typical Europe) '

      CALL EMENU('Conversions to primary',ITEMS,nitms,INO)
    
      if(INO.EQ.nitms)then
        if(MODSIT)CALL EMKCFG('-',IER)
        RETURN
      elseif(INO.EQ.nitms-1)then
        CALL PHELPD('prim convers',41,'-',0,0,IER)
      elseif(ino.eq.2)then
        CALL EASKR(pcnvht,' ','Heating primary energy multiplier:',
     &       1.0,'W',5.0,'W',1.25,'heat prim multip',IER,41)
        MODSIT=.true.
      elseif(ino.eq.3)then
        CALL EASKR(pcnvcl,' ','Cooling primary energy multiplier:',
     &       1.0,'W',5.0,'W',3.6,'cool prim multip',IER,41)
        MODSIT=.true.
      elseif(ino.eq.4)then
        CALL EASKR(pcnvlt,' ','Lighting primary energy multiplier:',
     &       1.0,'W',5.0,'W',3.6,'light prim multip',IER,41)
        MODSIT=.true.
      elseif(ino.eq.5)then
        CALL EASKR(pcnvfn,' ','Fans & pumps primary energy multiplier:',
     &       1.0,'W',5.0,'W',3.6,'fans prim multip',IER,41)
        MODSIT=.true.
      elseif(ino.eq.6)then
        CALL EASKR(pcnvsp,' ','Small power primary energy multiplier:',
     &       1.0,'W',5.0,'W',3.6,'sml pwr prim multip',IER,41)
        MODSIT=.true.
      elseif(ino.eq.7)then
        CALL EASKR(pcnvhw,' ','Hot water primary energy multiplier:',
     &       1.0,'W',5.0,'W',3.6,'hot water prim multip',IER,41)
        MODSIT=.true.
      elseif(ino.eq.10)then
   95   hold = ' '
        WRITE(HOLD,'(3F10.3)')phtco2,phtnox,phtsox
        CALL EASKS(HOLD,'Heating related emissions [gr/kWh primary]:',
     &    'CO2 NOx & SOx ',40,' 190.  0.2  0.1 ','heat emiss',IIER,41)
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
     &    'CO2 NOx & SOx ',40,' 612.  2.06   7.5 ','cool emiss',IIER,41)
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
     &   'CO2 NOx & SOx ',40,' 612.  2.06   7.5 ','light emiss',IIER,41)
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
     &    'CO2 NOx & SOx ',40,' 612.  2.06   7.5 ','fan emiss',IIER,41)
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
     &    'CO2 NOx & SOx ',40,'612.   2.06  7.5 ','smlp emiss',IIER,41)
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
     &    'CO2 NOx & SOx ',40,' 190.  0.2  0.1 ','dhw emiss',IIER,41)
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
      subroutine calenmanage(act,ier)
#include "epara.h"
#include "building.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      common/set1/iyear,ibdoy,iedoy,ifday,iftime
      common/user/browse
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
      DIMENSION VERT(35),citem(17),IDVALS(10),clist(365)
      dimension ixdvals(365)

      character calename*32,calentag*12,calendayname*32
      character h*72,vert*33,DS*7,DS1*10,act*1,key*1
      character outs*124,head*18,tcalename*32,citem*52,clist*52
      character tcalentag*12,tcalendayn*32
      logical modify,browse
      INTEGER IHL

C If user requested initial calendar.
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
      H(1)='This facilitiy sets up and manages a calendar of day'
      H(2)='types which (can eventually) be associated with zone'
      H(3)='casual gains, infiltration schedues, zone controls.'
      H(4)=' '
      H(5)='This facility is work-in-progress and the data saved'
      H(6)='within the configuration file.is not yet used by zone '
      H(7)='operations'
      H(8)=' '
      H(9)='Does not yet hold standard calendars for use by projects'
      H(10)='nor does it access typical patterns for differenct '
      H(11)='countries. '

      write(head,'(a,i4)') ' Calendar for ',iyear
      CALL EMENU(head,VERT,MVERT,IVERT)
      IF(IVERT.eq.1)THEN

C Edit calendar name
        h(1)='Short phrase idenfitying the calendar. '
        h(2)='Useful if you need to maintain several variants.'
        tcalename=calename
        CALL EASKS(tcalename,' Description of calendar: ',
     &    '  ',32,'weekday sat sun all year','calen descr',IER,2)
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
        h(1)='This facility allows you to manage day types. You can'
        h(2)='add a day type. To change the details of a day type'
        h(3)='click on the relevant menu item.'
        icvert=-1
        CALL EMENU('  Day types',citem,mcvert,icvert)
        if(icvert.eq.mcvert)then
          CALL EPMENRC
          goto 3
        elseif(icvert.eq.mcvert-1)then

C Help.
          CALL PHELPD('calendar day section',3,'-',0,0,IER)
          goto 73
        elseif(icvert.eq.mcvert-2)then

C Add or delete a day type.
          if(nbdaytype.ge.3.and.nbdaytype.le.MDTY)then
            write(h(1),'(a,i4,a)')'There can be up to ',MDTY,
     &      ' day types.'
            h(2)='Day types cannot be deleted (yet) but may not'
            h(3)='not be assigned to any day at all. The equivalent'
            h(4)='of deletion is to unassign a day type from all '
            h(5)='actual days '
            CALL EASKABC(' Options:',' ',
     &        'add','delete (not available)','continue',IRT,5)
            if(irt.eq.1)then
              nbdaytype=nbdaytype+1
              nbcaldays(nbdaytype)=0
              calentag(nbdaytype)='new'
              calendayname(nbdaytype)='new day type'

C Edit calendar day tag and name.
              h(1)='The tag is one word (<12 char) e.g. holiday'
              h(2)='Description is phrase for use in selection lists.'
              tcalentag=calentag(nbdaytype)
              CALL EASKS(tcalentag,' Calendar day type tag: ',
     &          '  ',12,'holiday','day tag',IER,2)
              if(tcalentag(1:2).ne.'  ')then
                if(tcalentag(1:lnblnk(tcalentag)).ne.
     &calentag(nbdaytype)(1:lnblnk(calentag(nbdaytype))))then
                  calentag(nbdaytype)=tcalentag
                endif
              endif
              tcalendayn=calendayname(nbdaytype)
              CALL EASKS(tcalendayn,' Calendar day type descripton: ',
     &          '  ',32,'holiday','day descr',IER,2)
              if(tcalendayn(1:2).ne.'  ')then
                if(tcalendayn(1:lnblnk(tcalendayn)).ne.
     &calendayname(nbdaytype)(1:lnblnk(calendayname(nbdaytype))))then
                  calendayname(nbdaytype)=tcalendayn
                endif
              endif

C Update operations data for all zones to include this new day type
              CALL UPDOPR
              modify=.true.
              goto 73
            elseif(irt.eq.2)then
              DO 144 IHL=1,5
                call edisp(iuout,h(ihl))
 144          CONTINUE
              goto 73
            elseif(irt.eq.3)then
              goto 73
            endif
          endif
        elseif(icvert.gt.2.and.icvert.lt.mcvert-3)then

C Edit calendar day tag and name.
          ifoc=icvert-2
          h(1)='The tag is one word (<12 char) e.g. holiday'
          h(2)='Description is phrase for use in selection lists'
          h(3)='e.g. Holidays in Wales'
          tcalentag=calentag(ifoc)
          CALL EASKS(tcalentag,' Calendar day type tag: ',
     &      '  ',12,'holiday','day tag',IER,3)
          if(tcalentag(1:2).ne.'  ')then
            if(tcalentag(1:lnblnk(tcalentag)).ne.
     &         calentag(ifoc)(1:lnblnk(calentag(ifoc))))then
              calentag(ifoc)=tcalentag
              modify=.true.
            endif
          endif
          tcalendayn=calendayname(ifoc)
          CALL EASKS(tcalendayn,' Calendar day type descripton: ',
     &      '  ',32,'holiday','day descr',IER,2)
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

C update the model if not browsing 
          CALL EMKCFG('-',IER)
        endif
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C Produce help text for the vertex menu.
        CALL PHELPD('calendar section',11,'-',0,0,IER)
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
     &    'e all year',' ',' ',' ',' ',' ',' ',' ',isw,idno,3)

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
        call calenprint(iuout,'t',iyear,loopst,loop,ier)
        call edisp(iuout,' ')
        call calenprint(iuout,'g',iyear,loopst,loop,ier)
        call edisp(iuout,' ')
      ELSEIF(IVERT.EQ.(MVERT-3))THEN

C Apply day type to several days of the year. First ask for the
C day type and then present a list of all of the days so that the
C user can select one or more.
        CALL EPMENSV
        h(1)='Select one of the existing day types. This day type'
        h(2)='will be applied to each of the julian days selected.'
        do 67 ij=1,nbdaytype
          write(citem(ij),'(3a)') calentag(ij),' ',calendayname(ij)
  67    continue
        IX=1
        CALL EPICKS(IX,IDVALS,' ',' Available day types:',
     &    52,nbdaytype,citem,'available day types',IER,2)
        if(ix.ne.0)then
          idt=idvals(1)
          do 78 ld=1,365
            call stdate(iyear,ld,DS,DS1)
            WRITE(clist(ld),'(3a)') DS1,'  ',
     &        calentag(icalender(ld))
  78      continue
          CALL EPMENSV
          ixd=365
          CALL EPICKS(ixd,ixdvals,' ',' Days in the year:',
     &      52,365,clist,'available days',IER,2)
          CALL EPMENRC
          if(ixd.ne.0)then
            do 79 idayloop=1,ixd
              ifoc=ixdvals(idayloop)
              call stdate(iyear,ifoc,DS,DS1)
              icalender(ifoc)=idt
              nbcaldays(idt)=nbcaldays(idt) + 1
              write(outs,*) 'Revised day: ',ifoc,' ',DS1,' ',
     &          icalender(ifoc),' ',calendayname(icalender(ifoc))
              call edisp(iuout,outs)
  79        continue
            modify=.true.
          endif
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
        h(1)='Select one of the existing day types. '
        h(2)='Note: this applies only to the current day of the year.'
        do 76 ij=1,nbdaytype
          write(citem(ij),'(3a)') calentag(ij),' ',calendayname(ij)
  76    continue
        IX=1
        CALL EPICKS(IX,IDVALS,' ',' Available day types:',
     &    52,nbdaytype,citem,'avail day types',IER,2)
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
C act (char*1) action to take 't' text feedback, 'g' graphic feedback.
      subroutine calenprint(itru,act,iyear,loopst,loop,ier)
#include "building.h"

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

C Text for each slot.
      character*3 am1slots(42),am2slots(42)
      character calename*32,calentag*12,calendayname*32,word3*3
      character act*1,mohead*44,outs*124,etext*72
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
        CALL viewtext(ETEXT,line,0,2)

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
            CALL viewtext(ETEXT,line,0,2)
            line=line+1
            write(etext,'(a)') mohead
            CALL viewtext(ETEXT,line,0,2)
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
              CALL viewtext(ETEXT,line,0,2)
              line=line+1
              write(etext,'(a)') mohead(1:22)
              CALL viewtext(ETEXT,line,0,2)
              call edayr(mdays(imon1),idayn,imthn)
              call eweekd(idayn,imthn,iyear,im1dwk)
              im2dwk=0
            else
              imon1=im
              imon2=im+1
              line=line+1
              write(etext,'(8x,a,17x,a)') ramonth(imon1),ramonth(imon2)
              CALL viewtext(ETEXT,line,0,2)
              line=line+1
              write(etext,'(a)') mohead
              CALL viewtext(ETEXT,line,0,2)
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
              if(idcol.eq.0)call winscl('-',0)
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
              if(idcol.eq.0)call winscl('-',0)
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
        call winscl('-',0)
        write(etext,*) 'Calendar: ',calename
        CALL viewtext(ETEXT,line,0,2)
        do 151 icd = 1,nbdaytype
          line=line+1
          write(etext,*) calentag(icd),' ',calendayname(icd)
          idcol=icd
          if(idcol.eq.0)call winscl('-',0)
          call findviewtext(3,line,isize,ix,iy)
          if(idcol.eq.0)then
            call textatxy(ix,iy,etext,'-',idcol)
          else
            call textatxy(ix,iy,etext,'z',idcol)
          endif
 151    continue
        call winscl('-',0)
      endif
      call forceflush()
      return

C Errors for debug printing.
C   14 if(IOS.eq.2)then
C        CALL USRMSG(' No prmission to write array in ',' ','W')
C      else
C        CALL USRMSG(' Long arrary write error in ',' ','W')
C      endif
C      IER=1
C      return
      end

C ********** EDDGTP **************
C EDDGTP Sets up ground temperature profiles
      SUBROUTINE EDDGTP(modsit)
#include "building.h"

      common/pophelp/h(60)
      common/OUTIN/IUOUT,IUIN
      common/C14/UGRDTP(12,MGRDP),NGRDP

      CHARACTER H*72,HOLD*72,OUTS*124
      LOGICAL MODSIT

C Ground temperature profiles....
      call edisp(iuout,' ')  
      write(outs,'(a,i3,a)')' There are currently ',NGRDP,' profiles.'      
      call edisp(iuout,outs) 
      H(1)='Ground temperature profiles define the monthly soil'
      H(2)='temperatures at arbitrary depths for use as a boundary'
      H(3)='condition within a simulation.  Up to 12 profiles may'
      H(4)='be defined each comprising 12 monthly temperatures.'
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
     &             'edit profiles','add profile',' ',IRT,4)
        if(IRT.eq.1)then
          if(NGRDP.gt.0)then
            ifoc=1
            H(1)='Provide index of the ground temperature profile.'
            H(2)=' '
            H(3)='A cancel request leaves the model unchanged. '
            CALL EASKI(IFOC,' ',' Index of profile to edit?',
     &        1,'F',NGRDP,'F',1,'edit profile',IERI,3)
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
      H(1)='Temperatures are typically at 1.5m depth.'
      CALL EASKS(HOLD,' Temperatures January-June:',' ',72,' ',
     &    'gr prof jan-jun',IER,1)
      CALL CHITMS(HOLD,NV)
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
      H(1)='Temperatures are typically at 1.5m depth.'
      CALL EASKS(HOLD,' Temperatures July-December:',' ',72,' ',
     &    'gr prof jul-dec',IER,1)
      CALL CHITMS(HOLD,NV)
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
C EDDGTP Sets up shading and insolation files

      SUBROUTINE EDDSHD
#include "building.h"
      
      integer lnblnk  ! function definition

      common/C21/IFCFG,cfgroot,LCFGF
      common/OUTIN/IUOUT,IUIN
      COMMON/precz/zname(MCOM),zdesc(MCOM)

      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/pophelp/h(60)

      character H*72,ZNAME*12,ZN*12,LCFGF*72
      character zdesc*64,outs*248,CFGROOT*24
      logical XST,OK,DOK,unixok

      if(ncomp.eq.0)then
        return
      endif

C Check if Unix-based or DOS based.
      call isunix(unixok)

      do 42 iz=1,ncomp
        if(ISI(iz).eq.1)then
          call FINDFIL(LSHAD(iz),XST)
          if(XST)then
            write(ZN,'(A)') zname(iz)
            LNZ=max(1,LNBLNK(ZN))
            write(outs,'(3a)') 'Shading for ',ZN(1:lnz),
     &        ' is out of date.'
            call edisp(iuout,outs)
          endif
        endif
  42  continue
      h(1)='Shading should be recalculated when site'
      h(2)='changes. Each affected zone will be done in turn.'
      CALL EASKABC(' Reflect site change in shading options:',' ',
     &  'recalculate (silent)','recalculate (interactive)','continue',
     &  IRT,2)
      if(IRT.eq.1.or.IRT.eq.2)then
        if(IRT.eq.1)then
        endif
        do 43 iz=1,ncomp
          if(ISI(iz).eq.0) goto 43
          call FINDFIL(LSHAD(iz),XST)
          write(ZN,'(A)') zname(iz)
          LNZ=max(1,LNBLNK(ZN))

C Get logical name of terminal type, expand model name
C to include the path and create a string to drive ish.
C If user asked for silent recalculation then do each
C one in forground in an xterm with `-act update_silent`
C as the command line.
          if(IRT.eq.1)then
            call comissionish(iz,'sra',ier)
          else
            DOK=.true.
            h(1)='Each zone is calculated in sequence.'
            h(2)='If you do one, you should do all.'
            write(outs,'(3a)') 'Proceed with shading for ',
     &            zn(1:lnz),' ?'
            CALL ASKOK(outs,' ',OK,DOK,2)
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

