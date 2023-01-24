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


C Casual gain control file: creation, editing.
C EDCASCTL - interface for creation and editing of cas gn data.

C ******************** EDCASCTL ********************
C EDCASCTL - edit casual gain control data.

      SUBROUTINE  EDCASCTL(ITRC,ICOMP,IER)
#include "building.h"
#include "model.h"

C geometry.h provides commons G0/G1/G2/G5/c20/precz.
#include "geometry.h"
#include "prj3dv.h"
#include "espriou.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/FILEP/IFIL
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,IDTY
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

C Zone casual gain control data.
      common/CGCIND/cgcdesc(MCOM)
      COMMON/CGCIN2N/NCGTC(MCOM,MDTY),NLITZ(MCOM),IDFST(MCOM,MLCOM),
     & CGX(MCOM,MLCOM,MDF),CGY(MCOM,MLCOM,MDF),CGH(MCOM,MLCOM,MDF),
     & UX(MCOM,MLCOM,MDF),UY(MCOM,MLCOM,MDF),UH(MCOM,MLCOM,MDF),
     & SETPT(MCOM,MLCOM),SYSPER(MCOM,MLCOM),SOFFLL(MCOM,MLCOM),
     & IOFFDT(MCOM,MLCOM),SMLOUT(MCOM,MLCOM),SMEOUT(MCOM,MLCOM)
      COMMON/CGCIN3/ICGCS(MCOM,MDTY),ICGCF(MCOM,MDTY),
     & ICGCFL(MCOM,MLCOM),SPELEC(MCOM,MLCOM),SLOPEM(MCOM,MLCOM)

      COMMON/DAYF/NDF(MCOM,MLCOM),DFDAT(MCOM,MLCOM,MGT,MDF),
     & AZIJL(MCOM,MGT),SUMDF(MCOM,MLCOM,MDF),NUDWIN(MCOM)

C Daylight coefficient common block data definition:
      COMMON/DCOEF/NDCFP,NDCFS(MDCFP),DCOEF(MDCFP,MDCFS,MDCFV),
     & IDCFID(MCOM,MLCOM,MDF)

C TMC common block:
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/TMCB1/IBCMT(MCOM,MTMC)

      DIMENSION ITMC2DF(30),KSTR(MDTY)
      DIMENSION  COG1(3),COG2(3),tip(3),ipoints(6,2)
      LOGICAL XST,lcouple,ldcoef,modcfg,unixok

      character outs*124,DTMP*64
      character KEY*1,hold*40,hold2*72,doitl*248
      character*48 ITEM(30)
      CHARACTER*72 DLCGCIN
      CHARACTER info*18,cgcdesc*64,brw*8,calclabel*20
      character sn*12  ! for source surface name.
      character dradcf*72
      character ltmp*72
      character doit*248
      character msg*48
      character fs*1
C      character longtfile*144,longtfiledos*144
C     character L144*144,lltmp*144
      INTEGER IX,IY,IZ,KSTR
      integer IRT  ! for radio button
      integer NITEMS,INO ! max items and current menu item
      integer lntmp,lnradcf

#ifdef OSI
      integer iix,iiy
#else
      integer*8 iix,iiy
#endif

      helpinsub='edcasctl'  ! set for subroutine

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Assume daylight coeffs do not need to be calculated.
      ldcoef=.false.
      ICMDCF=0  ! set initial value
      LTMP=' '

C Check and see if a casual gain control file exists. Ask
C for confirmation of the name.
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
        WRITE(DLCGCIN,'(A,A4)')zname(ICOMP)(1:lnzname(ICOMP)),'.cgc'
      else
        WRITE(DLCGCIN,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &    zname(ICOMP)(1:lnzname(ICOMP)),'.cgc'
      endif
      LTMP=DLCGCIN
      CALL EASKS(LTMP,'Zone casual gain control file name.',
     &  'Specify:',72,DLCGCIN,'cgc control file',IER,nbhelp)
      if(LTMP(1:2).NE.'  ')then
         LCGCIN(ICOMP)=LTMP
         DLCGCIN=LTMP  ! set the default name to match
         ICGC(ICOMP)=1
      endif
      if(ICGC(ICOMP).NE.0)then
        call FINDFIL(LCGCIN(ICOMP),XST)
        if(XST)then

C Read existing file. If there is an error when opening the cgc file
C then assume that Radiance based daylight coefficients will not have
C been calculated.
          write(currentfile,'(a)')LCGCIN(ICOMP)(1:lnblnk(LCGCIN(ICOMP)))
          call ercgcf(itrc,iuout,LCGCIN(ICOMP),ICOMP,ier)
          call edisp(iuout,'Opened existing casual gain control file.')
          if (ier.eq.-101) then
            call usrmsg('Daylight coefficients not yet calculated!',
     &        '  ','W')
            ldcoef=.true.
          else
            ldcoef=.false.
          endif
        else
          call edisp(iuout,'Could not find casual gain control file:')
          call edisp(iuout,LCGCIN(ICOMP))
        endif
        N=1   ! set N to signal cgc
      endif

C Help text for this menu.
      helptopic='casual_gain_control'
      call gethelptext(helpinsub,helptopic,nbhelp)

      if((ICGC(ICOMP).eq.0).or.(.not.xst))then

C Set up default data based on ....
        call FINDFIL(LCGCIN(ICOMP),XST)
        if(XST)then

C Read existing file or setup initial values.
          ICGC(ICOMP)=1
          call ercgcf(itrc,iuout,LCGCIN(ICOMP),ICOMP,ier)
          call edisp(iuout,'Opened existing casual gain control file.')
        else
          call edisp(iuout,'Setting up default parameters.')
          cgcdesc(icomp)='no description supplied'
          DO 400 IDTY=1,NBDAYTYPE
            NCGTC(ICOMP,IDTY)=2
            ICGCS(ICOMP,IDTY)=8; ICGCF(ICOMP,IDTY)=18
 400      CONTINUE
          ICGCFL(ICOMP,1)=1; IDFST(ICOMP,1)=1
          SETPT(ICOMP,1)=500.0; SOFFLL(ICOMP,1)=1.5
          SMLOUT(ICOMP,1)=0.1; SMEOUT(ICOMP,1)=0.1
          IOFFDT(ICOMP,1)=0
          SYSPER(ICOMP,1)=1.0; SPELEC(ICOMP,1)=15.0
          SLOPEM(ICOMP,1)=-0.025
          NDF(ICOMP,1)=1
          CGX(ICOMP,1,1)=0.0; CGY(ICOMP,1,1)=0.0; CGH(ICOMP,1,1)=0.0
          UX(ICOMP,1,1)=0.0; UY(ICOMP,1,1)=0.0; UH(ICOMP,1,1)=1.0
          IDCFID(ICOMP,1,1)=1
          N=1  ! set N
          NLITZ(ICOMP)=1
          NUDWIN(ICOMP)=1
          do 48 I = 1, NSUR
            DFDAT(ICOMP,1,I,1)=0.0
  48      continue

          LTMP=DLCGCIN
  246     CALL EASKS(LTMP,'Zone casual gain control file name.',
     &      'Specify:',72,DLCGCIN,'cgc control file',IER,nbhelp)
          IF(IER.NE.0)GOTO 246
          IF(LTMP(1:2).NE.'  ')THEN
            LCGCIN(ICOMP)=LTMP
            ICGC(ICOMP)=1
          ELSE
            goto 246
          ENDIF
        endif
      endif

C Rescan geometry file.
      IUF=IFIL+1
      call georead(IUF,LGEOM(ICOMP),ICOMP,0,ITRU,IER)
      MODIFYVIEW=.TRUE.
      MODBND=.TRUE.

C Rescan zone Construction and TMC file 
C (need to test blind/ shuter control flags).
      call FINDFIL(LTHRM(ICOMP),XST)
      if(XST)then
        CALL ECONST(LTHRM(ICOMP),IUF,ICOMP,0,IUOUT,IER)
      else
        write(outs,'(A,I3,A)') 
     &    'WARNING: No zone construction file found in zone ',
     &    ICOMP,' so blind control tracking not possible!'
         call edisp(iuout,outs)
      endif
      if(ITW(ICOMP).eq.1)then
        CALL ERTWIN(0,IUOUT,IUF,LTWIN(ICOMP),ICOMP,IER)
      else
        write(outs,'(A,I3,A)') 
     &    'WARNING: No zone TMC file found for zone ',ICOMP,
     &    ' so blind control tracking not possible!'
        call edisp(iuout,outs)
      endif         
      MODIFYVIEW=.TRUE.
      MODBND=.TRUE.

C Begin composing the display.
  20  continue

C Reset all surface lines to standard width and redraw if revised data.
      if(MODIFYVIEW)then
        CALL INLNST(1)
        nzg=1
        nznog(1)=ICOMP
        izgfoc=ICOMP
        CALL redraw(IER)
      endif
      NL=NLITZ(ICOMP)
      write(ITEM(1),'(A,A)')     '  Zone: ',zname(ICOMP)
      write(ITEM(2),'(A,A36)')   'a Desc: ',cgcdesc(ICOMP)(1:36)
      if(NBDAYTYPE.le.3)then
        write(ITEM(3),'(A,3i2)') 'b Control daytypes :',NCGTC(ICOMP,1),
     &    NCGTC(ICOMP,2),NCGTC(ICOMP,2)
      else
        write(ITEM(3),'(A,4i2,a)') 'b Control daytypes :',
     &    NCGTC(ICOMP,1),NCGTC(ICOMP,2),NCGTC(ICOMP,3),NCGTC(ICOMP,4),
     &    '...'
      endif
      if(NBDAYTYPE.le.3)then
        write(ITEM(4),'(A,6I3)') 'c Control periods  :',ICGCS(ICOMP,1),
     &    ICGCF(ICOMP,1),ICGCS(ICOMP,2),ICGCF(ICOMP,2),
     &    ICGCS(ICOMP,3),ICGCF(ICOMP,3)
      else
        write(ITEM(4),'(A,6I3,a)') 'c Control periods  :',
     &    ICGCS(ICOMP,1),ICGCF(ICOMP,1),ICGCS(ICOMP,2),ICGCF(ICOMP,2),
     &    ICGCS(ICOMP,3),ICGCF(ICOMP,3),'...'
      endif
      write(ITEM(5),'(A,I3)')  '  Lighting zones   : ',NL
      ITEM(6)=            '  _______________________________________'

C If we got to this point and no lighting zone then there should be at least one.
      if (N.le.0) N=1 ! bounds error fix
      write(ITEM(7),'(A,I2)')    'd Lighting zone >> ',N
      info='unknown control'
      if(ICGCFL(ICOMP,N).eq.-1) info='Always On'
      if(ICGCFL(ICOMP,N).eq.0)  info='Always Off'
      if(ICGCFL(ICOMP,N).eq.1)  info='On-Off'
      if(ICGCFL(ICOMP,N).eq.2)  info='Step 100:50:0'
      if(ICGCFL(ICOMP,N).eq.3)  info='Dimming - ideal'
      if(ICGCFL(ICOMP,N).eq.4)  info='On-Off probability'
      if(ICGCFL(ICOMP,N).eq.31) info='Dimming-int. reset'
      if(ICGCFL(ICOMP,N).eq.32) info='Dimming-closd loop'
      write(ITEM(8),'(A,A)') 'e Control type    : ',info(1:lnblnk(info))
      if(IDFST(ICOMP,N).eq.1)then
        write(ITEM(9),'(A)')  'f Calculation type: Analytical DF'
      elseif(IDFST(ICOMP,N).eq.2)then
        write(ITEM(9),'(A)')  'f Calculation type: User defined DF'
      elseif(IDFST(ICOMP,N).eq.3)then
        write(ITEM(9),'(A)')  'f Calculation type: External sensor'
      elseif(IDFST(ICOMP,N).eq.4)then
        write(ITEM(9),'(A)')  'f Calculation type: Time step coupling'
      elseif(IDFST(ICOMP,N).eq.5)then
        write(ITEM(9),'(A)')  'f Calculation type: Daylight coeff.' 
      endif
      write(ITEM(10),'(A,4F6.1,I3)') 'g Control data  :',SETPT(ICOMP,N),
     & SOFFLL(ICOMP,N),SMLOUT(ICOMP,N),SMEOUT(ICOMP,N),IOFFDT(ICOMP,N)
      write(ITEM(11),'(A,F6.2,I3)') 'h Photocell data:',SYSPER(ICOMP,N),
     &                              NDF(ICOMP,N)

      if(IDFST(ICOMP,N).EQ.1.OR.IDFST(ICOMP,N).EQ.2.OR.
     &   IDFST(ICOMP,N).EQ.4)then

C For each sensor for methods 1,2,4.
        write(ITEM(12),'(A)') '  Posn: x  y   z     ux   uy   uz'
        m=12
        do 42 J = 1,NDF(ICOMP,N)
          m=m+1
          CALL EMKEY(m,KEY,IER)
          write(ITEM(m),'(A,6F6.2)') key,CGX(ICOMP,N,J),CGY(ICOMP,N,J),
     &      CGH(ICOMP,N,J),UX(ICOMP,N,J),UY(ICOMP,N,J),UH(ICOMP,N,J)
          COG1(1)=CGX(ICOMP,N,J)
          COG1(2)=CGY(ICOMP,N,J)
          COG1(3)=CGH(ICOMP,N,J)
          CALL VECTRN(COG1,TSMAT,COG2,IER)
          call CLIPPT(COG2(1),COG2(2),COG2(3),iclp1)
          call u2pixel(COG2(1),COG2(2),iix,iiy)
          tip(1)=CGX(ICOMP,N,J)+UX(ICOMP,N,J)
          tip(2)=CGY(ICOMP,N,J)+UY(ICOMP,N,J)
          tip(3)=CGH(ICOMP,N,J)+UH(ICOMP,N,J)
          CALL VECTRN(tip,TSMAT,COG2,IER)
          call CLIPPT(COG2(1),COG2(2),COG2(3),iclp2)
          if (iclp1.eq.0 .and. iclp2.eq.0) then
            CALL ecirc(iix,iiy,2,1)
            call arrow(COG1,tip,0.3,0.1,ipoints,'a',1)
          endif
  42    continue
        call forceflush()
      elseif(IDFST(ICOMP,N).EQ.3)then

C Method 3 (external sensor).
        write(ITEM(12),'(A)') 
     &    '  External photocell posit. & orientation'
        m=13
        CALL EMKEY(m,KEY,IER)
        write(ITEM(m),'(A,2F6.1)')key,CGX(ICOMP,N,1),CGY(ICOMP,N,1)
C Set third coordinate to dummy value:
        CGH(ICOMP,N,1)=0.0       

      elseif(IDFST(ICOMP,N).EQ.5)then

C Daylight coef method (5).
        write(ITEM(12),'(A)') '    Posn: x  y   z    ux   uy    uz   DC'
        m=12
        do 43 J = 1,NDF(ICOMP,N)
          m=m+1
          CALL EMKEY(m,KEY,IER)
          write(ITEM(m),'(A,1x,6F6.2,i2)') key,CGX(ICOMP,N,J),
     &      CGY(ICOMP,N,J),CGH(ICOMP,N,J),UX(ICOMP,N,J),UY(ICOMP,N,J),
     &      UH(ICOMP,N,J),IDCFID(ICOMP,N,J)
          COG1(1)=CGX(ICOMP,N,J)
          COG1(2)=CGY(ICOMP,N,J)
          COG1(3)=CGH(ICOMP,N,J)
          CALL VECTRN(COG1,TSMAT,COG2,IER)
          call u2pixel(COG2(1),COG2(2),iix,iiy)
          call CLIPPT(COG2(1),COG2(2),COG2(3),iclp1)
          tip(1)=CGX(ICOMP,N,J)+UX(ICOMP,N,J)
          tip(2)=CGY(ICOMP,N,J)+UY(ICOMP,N,J)
          tip(3)=CGH(ICOMP,N,J)+UH(ICOMP,N,J)
          CALL VECTRN(tip,TSMAT,COG2,IER)
          call CLIPPT(COG2(1),COG2(2),COG2(3),iclp2)
          if (iclp1.eq.0 .and. iclp2.eq.0) then
            CALL ecirc(iix,iiy,2,1)
            call arrow(COG1,tip,0.3,0.1,ipoints,'a',1)
          endif
  43    continue
        call forceflush()
      endif

C In case of user defined daylight factor. At this point still
C treats CFC and CFC2 with opaque surfaces.
      if(IDFST(ICOMP,N).EQ.2)then
        m=m+1   
        write(ITEM(m),'(A)') '  Source surface & DF(s) per photocell:'
        do 44 ii = 1,NSUR
          if(SOTF(icomp,II)(1:4).NE.'OPAQ'.AND.
     &       SOTF(icomp,II)(1:4).NE.'CFC '.AND.
     &       SOTF(icomp,II)(1:4).NE.'CFC2')THEN
            m=m+1
            CALL EMKEY(m,KEY,IER)
            write(sn,'(a)') sname(icomp,II)
            write(ITEM(m),'(4A,F6.4,a)')key,' ',sn(1:lnblnk(sn)),' ',
     &        DFDAT(ICOMP,N,II,1),' ...'
            ITMC2DF(m)=ii
          endif
  44    continue
      endif

      ITEM(M+1)=          '  ____________________________________  '
      ITEM(M+2)=          '* add/ delete lighting zone             '
      ITEM(M+3)=          '! list casual gain control information  '
      ITEM(M+4)=          '> save casual gain control information  '
      ITEM(M+5)=          '? help                                  '
      ITEM(M+6)=          '- exit menu                             '
      NITEMS=M+6

      INO = -2
   30 CALL EMENU('Casual Gain Control',ITEM,NITEMS,INO)

      IF (INO.EQ.NITEMS)THEN

C Set browsing no for e2r.
        brw = ' -b no '

C Check for control using Radiance.
        helptopic='casual_gain_control'
        call gethelptext(helpinsub,helptopic,nbhelp)
        lcouple=.false.
        DO ILZ=1,NLITZ(ICOMP)
          if (IDFST(ICOMP,N).eq.4) lcouple=.true.
        ENDDO  ! of ILZ

C If coupling or daylight coefficients we need to nominate
C a radiance rcf file and remember it in the cfg file.
        if (lcouple.or.ldcoef)then

C Assume model is multi-folder with a rad folder.
C Set rcf name if unknown and set default name. If path for radiance
C files is ./ then update this to ../rad, create a folder. And
C proceed on the basis that the rcf file will be in the rad folder.
          modcfg=.false.
          if(pwdtocfg(1:1).eq.'!')then
            if(radpth(1:3).eq.'./ '.or.radpth(1:3).eq.'.\\ ')then
              CALL PHELPD('visu-rad-warning',7,'-',0,0,IER)
              write(radpth,'(3a)')'..',fs,'rad'
              write(doit,'(4a)') 'mkdir ',
     &          cfgroot(1:lnblnk(cfgroot)),fs,'rad'
              call usrmsg('Creating folder for radiance model:',doit,
     &          'P')
              call runit(doit,'-')

C And update radpath. The L144 is for future use of EASKXORGTKF
              write(radpth,'(3a)') '..',fs,'rad'
              modcfg=.true.
            endif

            if(lradcf(1:7).eq.'UNKNOWN')then
              write(lradcf,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.rcf'
              modcfg=.true.
            endif
          else

C Prj has not been invoked from within the model cfg folder.
            if(radpth(1:3).eq.'./ '.or.radpth(1:3).eq.'.\\ ')then
              CALL PHELPD('visu-rad-warning',7,'-',0,0,IER)
              write(radpth,'(3a)')'..',fs,'rad'
              write(doit,'(8a)') 'mkdir ',pwdinitial(1:lnpwdi),fs,
     &          pwdtocfg(1:lnblnk(pwdtocfg)),fs,'..',fs,'rad'
              call usrmsg('Creating folder for radiance model:',doit,
     &         'P')
              call runit(doit,'-')

C And update radpath.
              write(radpth,'(3a)') '..',fs,'rad'
              modcfg=.true.
            endif

            if(lradcf(1:7).eq.'UNKNOWN')then
              write(lradcf,'(2a)') cfgroot(1:lnblnk(cfgroot)),'.rcf'
              modcfg=.true.
            endif
          endif

C Check rcf name and contents of file on return from e2r.

 319      ltmp=lradcf
          dradcf='scene.rcf'
          CALL EASKS(ltmp,
     &      ' Radiance scene file to support lighting controls',
     &      ' :',72,dradcf,'Radiance scene file name',IER,nbhelp)
          if(ltmp.eq.' ')goto 319 

C rcf name OK and differs from prior name, update cfg file and start e2r.
          lnradcf=lnblnk(lradcf)
          lntmp=lnblnk(ltmp)
          if(ltmp(1:lntmp).eq.lradcf(1:lnradcf))then
            continue
          else
            modcfg=.true.
            lradcf = ltmp
          endif
          if(modcfg)then
            call tstamp('>','PRJ: save configuration (with rcf file)')
            CALL EMKCFG('-',IER)
          endif
        endif

C Now invoke e2r to do the work.
        if (lcouple) then
          call usrmsg('About to start Radiance in order to generate',
     &      'descriptive files for coupled casual gain control.','W')
          call EASKAB(
     &    ' ','Radiance is being used to assess light levels:',
     &    'Create & edit Radiance model','Cancel',IW,nbhelp)
          if (IW.eq.1) then
            write(doitl,'(3a,i3,a)')'e2r -file ',LCFGF(1:lnblnk(LCFGF)),
     &        ' -purpose Coupling -zone ',ICOMP,' -act Create'
            call edisp (iuout,doitl)
            call runit(doitl,'-')
          endif
        endif
        if (ldcoef) then
 8888     call EASKAB(
     &    ' ','If Radiance is being used for daylight coefficients:',
     &    'Create Radiance model and calculate','Cancel',IW,nbhelp)
          if (IW.eq.1) then
            write(doitl,'(3a,i3,2a)')'e2r -file ',
     &        LCFGF(1:lnblnk(LCFGF)),' -purpose Day_coef -zone ',
     &        ICOMP,' -act Create',brw
          elseif (IW.eq.2) then
            return
          endif
          call edisp (iuout,doitl)
          call runit(doitl,'-')
          goto 8888
        endif

C The model needs to know about this additional file.

 9999   RETURN
      ELSEIF(INO.EQ.NITEMS-1)THEN

C Help.
        helptopic='casual_gain_control'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('cgc editing',nbhelp,'-',0,0,IER)

      ELSEIF(INO.EQ.NITEMS-2)THEN

C Save data.
        LTMP=LCGCIN(ICOMP)
  247   CALL EASKS(LTMP,' Zone casual gain control file ?',
     &    ' ',72,DLCGCIN,'cgc control file',IER,nbhelp)
        IF(IER.NE.0)GOTO 247
        IF(LTMP(1:2).NE.'  ')THEN
          LCGCIN(ICOMP)=LTMP
          ICGC(ICOMP)=1
        ENDIF

C Write data.
        call usrmsg(
     &    ' Updating model for casual gain control...',' ','-')
        call CASCTMK(LCGCIN(ICOMP),ICOMP,'-',IER)
        call tstamp('>','PRJ: save configuration')
        CALL EMKCFG('-',IER)
        call usrmsg(
     &    ' Updating model for casual gain control...done.',' ','-')

      ELSEIF(INO.EQ.NITEMS-3)THEN

C List data. This is similar to code in ercgcf.
C <<  Perhaps this could be joined together? >>
        call edisp(iuout,' ')
        write(outs,'(A,I2)') ' Zone ',ICOMP
        call edisp(iuout,outs)
        DO IDTY=1,NBDAYTYPE
          write(outs,9989)CALENTAG(IDTY),NCGTC(ICOMP,IDTY)
 9989     format(' Casual gain type for control during ',A,' = ',I3)
          call edisp(iuout,outs)
        ENDDO  ! of IDTY
        DO 117 NLZ=1,NLITZ(ICOMP)
          write(outs,'(A,I1)') ' Light zone ',NLZ
          call edisp(iuout,outs)
          if(IDFST(ICOMP,NLZ).eq.1)then
            write(outs,'(A,I2,A)') ' Average of ',NDF(ICOMP,NLZ),
     &        ' analytical daylight factor photocell types.'
            call edisp(iuout,outs)
          elseif(IDFST(ICOMP,NLZ).eq.2)then
            write(outs,'(A,I2,A)') ' Average of ',NDF(ICOMP,NLZ),
     &        ' user specified daylight factor photocell types.'
            call edisp(iuout,outs)
            write(outs,'(A,I2,A)') ' There are ',NUDWIN(ICOMP),
     &        ' source surfaces for daylighting in the zone.'
            call edisp(iuout,outs)
          elseif(IDFST(ICOMP,NLZ).eq.3)then
            call edisp(iuout,' External lighting sensor.')
          elseif(IDFST(ICOMP,NLZ).eq.4)then
            write(outs,'(A,I2,A)') ' Average of ',NDF(ICOMP,NLZ),
     &        ' direct coupling photocell types. '
            call edisp(iuout,outs)
          elseif(IDFST(ICOMP,NLZ).eq.5)then
            write(outs,'(A,I2,A)') ' Average of ',NDF(ICOMP,NLZ),
     &        ' daylight coefficient photocell types. '
            call edisp(iuout,outs)
          endif

          call edisp(iuout,'  ')
          call edisp(iuout,
     &  ' Set   |Switch-off |Minimum dimming|Minimal elec|Switch-off ')
          call edisp(iuout,
     &  ' point |light level|light output   |light output|delay time ')
          write(outs,9987)SETPT(ICOMP,NLZ),SOFFLL(ICOMP,NLZ),
     &      SMLOUT(ICOMP,NLZ), SMEOUT(ICOMP,NLZ),IOFFDT(ICOMP,NLZ)
 9987     format(F7.1,F12.1,F14.2,F14.2,I10)
          call edisp(iuout,outs)

          if(IDFST(ICOMP,NLZ).eq.1)then
            write(calclabel,'(A)')  ' Analytical DF'
          elseif(IDFST(ICOMP,NLZ).eq.2)then
            write(calclabel,'(A)')  ' User defined DF'
          elseif(IDFST(ICOMP,NLZ).eq.3)then
            write(calclabel,'(A)')  ' External sensor'
          elseif(IDFST(ICOMP,NLZ).eq.4)then
            write(calclabel,'(A)')  ' Time step coupling'
          elseif(IDFST(ICOMP,NLZ).eq.5)then
            write(calclabel,'(A)')  ' Daylight coeff.' 
          endif
          call edisp(iuout,'  ')
          call edisp(iuout,' % of |Sensor|Calculation   | ')
          call edisp(iuout,' whole|number|type          | ')
          write(outs,'(F6.2,I7,a)')SYSPER(ICOMP,NLZ),NDF(ICOMP,NLZ),
     &      calclabel
          call edisp(iuout,outs)

          call edisp(iuout,'  ')
          if(IDFST(ICOMP,NLZ).eq.1.or.IDFST(ICOMP,NLZ).eq.2.or.
     &       IDFST(ICOMP,NLZ).eq.4)then
           call edisp(iuout,
     &       ' Sensor|   X  |   Y  |   Z  |  DX |  DY |  DZ |')
          elseif(IDFST(ICOMP,NLZ).eq.5)then
           call edisp(iuout,
     &       ' Sensor|   X  |   Y  |   Z  |  DX |  DY |  DZ |  DC| ')
          elseif(IDFST(ICOMP,NLZ).eq.3)then
            call edisp(iuout,' Sensor  | Sensor |')
            call edisp(iuout,' position| type   |')
          endif

          DO 9986 J=1,NDF(ICOMP,NLZ)
            if(IDFST(ICOMP,NLZ).eq.1.or.IDFST(ICOMP,NLZ).eq.2.or.
     &        IDFST(ICOMP,NLZ).eq.4)then
              write(outs,9914)J,CGX(ICOMP,NLZ,J),CGY(ICOMP,NLZ,J),
     &          CGH(ICOMP,NLZ,J),UX(ICOMP,NLZ,J),UY(ICOMP,NLZ,J),
     &          UH(ICOMP,NLZ,J)
 9914         format(i7,3F7.2,3F6.2)
              call edisp(iuout,outs)
            elseif(IDFST(ICOMP,NLZ).eq.5)then
              write(outs,9915)J,CGX(ICOMP,NLZ,J),CGY(ICOMP,NLZ,J),
     &         CGH(ICOMP,NLZ,J),UX(ICOMP,NLZ,J),UY(ICOMP,NLZ,J),
     &         UH(ICOMP,NLZ,J),IDCFID(ICOMP,NLZ,J)
 9915         format(i7,3F7.2,3F6.2,I4)
              call edisp(iuout,outs)
            elseif(IDFST(ICOMP,NLZ).eq.3)then
              write(outs,9916)CGX(ICOMP,NLZ,1),CGY(ICOMP,NLZ,1)
 9916         format(F8.1,F9.1)
              call edisp(iuout,outs)
            endif
 9986     CONTINUE

          DO J=1,NDF(ICOMP,NLZ)
            if(IDFST(ICOMP,NLZ).EQ.2)then
              call edisp(iuout,' ')
              call edisp(iuout,' Source | Daylight | ')
              call edisp(iuout,' surface| factor(%)| ')
              idd=0
              DO IL=1,NZSUR(icomp)
                if(idd.lt.NUDWIN(ICOMP))then
                  if(DFDAT(ICOMP,NLZ,IL,J).gt.0.0)then
                    idd=idd+1
                    write(outs,9917)IL,DFDAT(ICOMP,NLZ,IL,J)*100.
 9917               format(I6,F10.2)
                    call edisp(iuout,outs)
                  endif
                endif
              ENDDO  ! of IL
            endif
          ENDDO      ! of J

          call edisp(iuout,' ')
          call edisp(iuout,' Start-Finish         | Control type| ')
          info='unknown control'
          if(ICGCFL(ICOMP,NLZ).eq.-1) info='Always On'
          if(ICGCFL(ICOMP,NLZ).eq.0)  info='Always Off'
          if(ICGCFL(ICOMP,NLZ).eq.1)  info='On-Off'
          if(ICGCFL(ICOMP,NLZ).eq.2)  info='Step 100:50:0'
          if(ICGCFL(ICOMP,NLZ).eq.3)  info='Dimming - ideal'
          if(ICGCFL(ICOMP,NLZ).eq.4)  info='On-Off probability'
          if(ICGCFL(ICOMP,NLZ).eq.31) info='Dimming-int. reset'
          if(ICGCFL(ICOMP,NLZ).eq.32) info='Dimming-closd loop'

          write(outs,'(I3,a,2i3,a,2i3,a,i3,2x,A)')
     &      ICGCS(ICOMP,1),'-',ICGCF(ICOMP,1),
     &      ICGCS(ICOMP,2),'-',ICGCF(ICOMP,2),
     &      ICGCS(ICOMP,3),'-',ICGCF(ICOMP,3),info
          call edisp(iuout,outs)

          IF(ICGCFL(ICOMP,NLZ).EQ.31.OR.ICGCFL(ICOMP,NLZ).EQ.32)THEN
            call edisp(iuout,' ')
            call edisp(iuout,' Night | Control| ')
            call edisp(iuout,' Calib.|   Slope| ')
            write(outs,'(f7.1,f8.5)')SPELEC(ICOMP,NLZ),SLOPEM(ICOMP,NLZ)
            call edisp(iuout,outs)
          ENDIF
  117   CONTINUE

      ELSEIF(INO.EQ.NITEMS-4)THEN

C +- Lighting zone.
        CALL EASKAB(' ','Modify nmber of lighting zones:',
     &    'delete one','add one @ end',IW,nbhelp)
        IF(IW.EQ.2.AND.(NLITZ(ICOMP)+1.LE.4))THEN
          NLITZ(ICOMP)= NLITZ(ICOMP)+1
          N=NLITZ(ICOMP)
C Fill data.
          ICGCFL(ICOMP,N)=1; IDFST(ICOMP,N)=1
          SETPT(ICOMP,N)=500.0; SOFFLL(ICOMP,N)=1.5
          SMLOUT(ICOMP,N)=0.1; SMEOUT(ICOMP,N)=0.1
          IOFFDT(ICOMP,N)=0; SYSPER(ICOMP,N)=1.0
          SPELEC(icomp,N)=15.0; SLOPEM(icomp,N)=-0.025
          NDF(ICOMP,N)=1
          CGX(ICOMP,N,1)=0.0; CGY(ICOMP,N,1)=0.0; CGH(ICOMP,N,1)=0.0
          UX(ICOMP,N,1)=0.0; UY(ICOMP,N,1)=0.0; UH(ICOMP,N,1)=1.0
          NUDWIN(ICOMP)=1

C Check for existing daylight coefficients.
          ICMDCF=0
          do IC=1,NCOMP
            do ILZ=1,NLITZ(ICOMP)
              do ISE=1,NDF(ICOMP,ILZ)
                ICMDCF=MAX(ICMDCF,IDCFID(IC,ILZ,ISE))
              enddo  ! of ISE
            enddo    ! of ILZ
          enddo      ! of IC
          do I = 1, NSUR
            DFDAT(ICOMP,N,I,1)=0.0
          enddo  ! of I

          call edisp(iuout,'New lighting zone filled with defaults.')
          MODIFYVIEW=.TRUE.
        ELSEIF(IW.EQ.1)THEN

C Delete lighting zone.  << ... >>
        ENDIF

      ELSEIF (INO.EQ.2)THEN

C Edit general description of the casual gains.
        DTMP=cgcdesc(ICOMP)
 52     CALL EASKS(DTMP,
     &    'Please supply a brief description of the casual gains:',
     &    ' ',64,'Dimming control','cgc description',IER,nbhelp)
        IF(IER.NE.0)GOTO 52
        IF(DTMP(1:2).NE.'  ')THEN
          cgcdesc(ICOMP)=DTMP  
        endif
        goto 20   ! refresh the menu text   

      ELSEIF (INO.EQ.3)THEN

C Control on which calendar day types and then ask about
C associated casual gain type (multiple day tpes get the
C same type).
        CALL EASKAB(' ','Control casual gains for ',
     &    'one specified day type','all day types',IY,nbhelp)
        IF(IY.EQ.1)THEN
          do loop=1,nbdaytype
            NCGTC(ICOMP,IDTY)=0 ! clear other day types
            ICGCS(ICOMP,IDTY)=0; ICGCF(ICOMP,IDTY)=1  ! clear periods
          enddo  ! of loop
          IX=1
          CALL EPICKS(IX,KSTR,' ',
     &      'choose daytype to control casual gains',
     &      12,nbdaytype,calentag,'Choose daytype',IER,nbhelp)
          IDTY=KSTR(1)

C << Should scan the zone operation file and use the labels given
C << in that....
          CALL EASKABC(' ','Control casual gain type ','occupants',
     &       'lighting','equipment',IZ,nbhelp)
          NCGTC(ICOMP,IDTY)=IZ
          WRITE(HOLD,'(2I6)')ICGCS(ICOMP,IDTY),ICGCF(ICOMP,IDTY)
          CALL EASKS(HOLD,' Control is active from hour to hour:',
     &      ' ',40,' 8  18 ',' cast ctl periods',IER,nbhelp)
          K=0
          CALL EGETWI(HOLD,K,ia,0,23,'W','period start',IER)
          CALL EGETWI(HOLD,K,ib,ia+1,24,'W','period end',IER)
          ICGCS(ICOMP,IDTY)=ia; ICGCF(ICOMP,IDTY)=ib
        ELSE
          do loop=1,nbdaytype
            write(msg,'(3a)') 'For daytype ',calentag(loop),
     &        ' associate cgc with '
            CALL EASKABCD(msg,' ','occupants',
     &        'lighting','equipment','no control on this day type',
     &        IZ,nbhelp)
            if(IZ.eq.4)then
              HOLD= '  0  1 '
              NCGTC(ICOMP,loop)=0
            else
              WRITE(HOLD,'(2I6)')ICGCS(ICOMP,loop),ICGCF(ICOMP,loop)
              NCGTC(ICOMP,loop)=IZ
            endif
            CALL EASKS(HOLD,' Control is active from hour to hour:',
     &        ' ',40,' 8  18 ',' cast ctl periods',IER,nbhelp)
            K=0
            CALL EGETWI(HOLD,K,ia,0,23,'W','period start',IER)
            CALL EGETWI(HOLD,K,ib,ia+1,24,'W','period end',IER)
            ICGCS(ICOMP,loop)=ia; ICGCF(ICOMP,loop)=ib
          enddo  ! of loop
        ENDIF

      ELSEIF (INO.EQ.4)THEN

C Edit the control periods for each day type.
        do loop=1,nbdaytype
           write(msg,'(3a)') 'For daytype ',calentag(loop)(1:12),
     &       ' control is active from hour to hour:'
           WRITE(HOLD,'(2I6)')ICGCS(ICOMP,loop),ICGCF(ICOMP,loop)
          CALL EASKS(HOLD,msg,' ',40,' 8  18 ',
     &      ' cast ctl periods',IER,nbhelp)
          K=0
          CALL EGETWI(HOLD,K,ia,0,23,'W','period start',IER)
          CALL EGETWI(HOLD,K,ib,ia+1,24,'W','period end',IER)
          ICGCS(ICOMP,loop)=ia; ICGCF(ICOMP,loop)=ib
        enddo  ! of loop
      ELSEIF (INO.EQ.7)THEN

C Toggle and then reform the menu to match control/calc type.
        N=N+1
        if(N.gt.NL)N=1
        MODIFYVIEW=.TRUE.
        goto 20

      ELSEIF (INO.EQ.8)THEN

C Edit the cgc control type.
        IRT=1
        call EASKATOG(' Control type:',' ',
     &    'Always On','Always Off','On-Off','Step 100:50:0',
     &    'Dimming - ideal','On-Off probability','cancel',
     &    IRT,nbhelp)
        if(irt.eq.1)then
          ICGCFL(ICOMP,N)=-1
        elseif(irt.eq.2)then
          ICGCFL(ICOMP,N)=0
        elseif(irt.eq.3)then
          ICGCFL(ICOMP,N)=1
        elseif(irt.eq.4)then
          ICGCFL(ICOMP,N)=2
        elseif(irt.eq.5)then
          helptopic='cas_ctl_dimming'
          call gethelptext(helpinsub,helptopic,nbhelp)
          IRT=1
          call EASKATOG(' Dimming control type:',' ',
     &        'Ideal','Integral reset','Close loop',
     &        'cancel',' ',' ',' ',IRT,nbhelp)
          if(irt.eq.1)then
            ICGCFL(ICOMP,N)=3
          elseif(irt.eq.2)then
            ICGCFL(ICOMP,N)=31
            SIG=SPELEC(ICOMP,N)
  46        WRITE(HOLD,'(F6.2)')SPELEC(ICOMP,N)
            CALL EASKS(HOLD,' Night-time calibration lux value:',
     &      ' ',40,' 15.0 ',' night-time calib',IER,nbhelp)
            K=0
            CALL EGETWR(HOLD,K,sig,0.,2000.,'W','night calib',IER)
            if(ier.ne.0)goto 46
            SPELEC(ICOMP,N)=SIG; SLOPEM(ICOMP,N)=0.0
          elseif(irt.eq.3)then
            ICGCFL(ICOMP,N)=32
            SIG=SPELEC(ICOMP,N)
            SLP=SLOPEM(ICOMP,N)
  45        WRITE(HOLD,'(F6.2,F8.4)')SPELEC(ICOMP,N),SLOPEM(ICOMP,N)
            CALL EASKS(HOLD,' Night-time calibration lux value & ',
     &        ' Control slope',40,' 15.0  -0.025',' calib & slope',
     &        IER,nbhelp)
            K=0
            CALL EGETWR(HOLD,K,sig,0.,2000.,'W','night calib',IER)
            CALL EGETWR(HOLD,K,slp,-1.,1.,'W','contr slope',IER)
            if(ier.ne.0)goto 45
            SPELEC(ICOMP,N)=SIG; SLOPEM(ICOMP,N)=SLP
          endif
        elseif(irt.eq.6)then
          ICGCFL(ICOMP,N)=4
        endif
        call usrmsg('If you have switched control laws you might also',
     &    'need to re-edit the sensor position.','W')

      ELSEIF (INO.EQ.9)THEN

C Select the calculation type.
        helptopic='cas_ctl_photocell'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('cgs calct type',nbhelp,'photocell',0,0,IER)
        IRT=1
        call EASKATOG(' Calculation type:',' ',
     &    'Analytic DF','User defined DF','Sensor on external face',
     &    'Radiance coupling','DaylightCoeff',
     &    'cancel',' ',IRT,nbhelp)
        if(irt.eq.1)then
          IDFST(ICOMP,N)=1
          ldcoef=.false.
        elseif(irt.eq.2)then
          IDFST(ICOMP,N)=2
          ldcoef=.false.
C Loop through all surfaces and find out number of TMCs: It
C does not (yet) count CFC and CFC2 as transparent.
          ICOUNT=0
          do i = 1, NSUR
            if(SOTF(ICOMP,I)(1:4).NE.'OPAQ'.AND.
     &         SOTF(ICOMP,I)(1:4).NE.'CFC '.AND.
     &         SOTF(ICOMP,I)(1:4).NE.'CFC2')THEN
              ICOUNT=ICOUNT+1              
            endif
          enddo   ! of I
          NUDWIN(ICOMP)=ICOUNT
        elseif(irt.eq.3)then
          IDFST(ICOMP,N)=3
          ldcoef=.false.
        elseif(irt.eq.4)then
          IDFST(ICOMP,N)=4
          ldcoef=.false.
        elseif(irt.eq.5)then
          IDFST(ICOMP,N)=5
          ldcoef=.true.
        endif
        MODIFYVIEW=.TRUE.
        call usrmsg('If you have switched calculation types please',
     &    're-edit the sensor position parameters.','W')
        goto 20

      ELSEIF (INO.EQ.10)THEN

C Edit detailed control data.
        helptopic='ctl_photocell_data'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('cgc control data',nbhelp,'-',0,0,IER)
  24    WRITE(HOLD,'(4F6.2,I4)')SETPT(ICOMP,N),SOFFLL(ICOMP,N),
     &  SMLOUT(ICOMP,N),SMEOUT(ICOMP,N),IOFFDT(ICOMP,N)
        CALL EASKS(HOLD,
     &    'Control data: lux set-point, switch-off, min dim, min elec',
     &    'and switch-off delay: ',40,' 500.0 1.5 0.1 0.1 0 ',
     &    ' control data',IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,aa,1.,2000.,'W','lux set-point',IER)
        CALL EGETWR(HOLD,K,ab,1.,3.,'W','switch-off lit lev',IER)
        CALL EGETWR(HOLD,K,ac,0.,1.,'W','min dimming',IER)
        CALL EGETWR(HOLD,K,ad,0.,1.,'W','min electric',IER)
        CALL EGETWI(HOLD,K,ie,0,30,'W','switch-off delay',IER)
        if(ier.ne.0)goto 24
        SETPT(ICOMP,N)=aa; SOFFLL(ICOMP,N)=ab
        SMLOUT(ICOMP,N)=ac; SMEOUT(ICOMP,N)=ad
        IOFFDT(ICOMP,N)=ie

      ELSEIF (INO.EQ.11)THEN

C Sensor data for lighting zone.
        helptopic='cas_ctl_zone_data'
        call gethelptext(helpinsub,helptopic,nbhelp)
        lastndf=NDF(ICOMP,N)
  35    WRITE(HOLD,'(F6.2,I4)')SYSPER(ICOMP,N),NDF(ICOMP,N)
        CALL EASKS(HOLD,' Casual gain portion & No of photocells',
     &    ' ',40,' 1.0 1',' sensor data',IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,aa,0.,1.,'W','casual gain port',IER)
        CALL EGETWI(HOLD,K,ib,1,9,'W','no. of sensors',IER)
        if(ier.ne.0)goto 35
        SYSPER(ICOMP,N)=aa
        if(ib.ne.lastndf)then
          if(ib.gt.NDF(ICOMP,N))then
            NDF(ICOMP,N)= ib
            CGX(ICOMP,N,ib)=0.0; CGY(ICOMP,N,ib)=0.0
            CGH(ICOMP,N,ib)=0.0
            UX(ICOMP,N,ib)=0.0; UY(ICOMP,N,ib)=0.0
            UH(ICOMP,N,ib)=1.0
          elseif(ib.lt.NDF(ICOMP,N))then
            call usrmsg('Cannot remove sensors at this time...',' ','W')
          endif
        endif
        MODIFYVIEW=.TRUE.
        goto 20

      ELSEIF (INO.GT.12.AND.INO.LT.(NITEMS-5))THEN

C Edit sensor position.
        IF(IDFST(ICOMP,N).EQ.1.OR.IDFST(ICOMP,N).EQ.4)then
          helptopic='cas_ctl_cell_location'
          call gethelptext(helpinsub,helptopic,nbhelp)
          J = INO - 12
  36      WRITE(HOLD,'(6F6.2)')CGX(ICOMP,N,J),CGY(ICOMP,N,J),
     &          CGH(ICOMP,N,J),UX(ICOMP,N,J),UY(ICOMP,N,J),UH(ICOMP,N,J)
          CALL EASKS(HOLD,' Sensor x y z & ux uy uz',
     &         ' ',40,' 0. 0. 0. 0. 0. 1.',' sensor x y z',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,aa,-100.,100.,'W','x coordinate',IER)
          CALL EGETWR(HOLD,K,ab,-100.,100.,'W','y coordinate',IER)
          CALL EGETWR(HOLD,K,ac,-100.,100.,'W','z coordinate',IER)
          CALL EGETWR(HOLD,K,ad,-100.,100.,'W','x dir vector',IER)
          CALL EGETWR(HOLD,K,ae,-100.,100.,'W','y dir vector',IER)
          CALL EGETWR(HOLD,K,af,-100.,100.,'W','z dir vector',IER)
          if(ier.ne.0)goto 36
          CGX(ICOMP,N,J)=aa; CGY(ICOMP,N,J)=ab; CGH(ICOMP,N,J)=ac
          UX(ICOMP,N,J)=ad; UY(ICOMP,N,J)=ae; UH(ICOMP,N,J)=af
          MODIFYVIEW=.TRUE.
          goto 20

        ELSEIF(IDFST(ICOMP,N).EQ.3)THEN

C External sensor associated surface and orientation.
          helptopic='ctl_external_location'
          call gethelptext(helpinsub,helptopic,nbhelp)
  37      WRITE(HOLD,'(2F6.2)')CGX(ICOMP,N,1),CGY(ICOMP,N,1)
          CALL EASKS(HOLD,' Sensor surface number position & ',
     &         'Orientation (0)-horizontal (1)-vertical ',40,' 1. 1.',
     &         ' sensor pos & orien',IER,nbhelp)
          K=0
          EMAX = REAL(NSUR)
          CALL EGETWR(HOLD,K,aa,0.,EMAX,'W','sensor sur no',IER)
          CALL EGETWR(HOLD,K,ab,0.,1.,'W','sensor orient',IER)
          if(ier.ne.0)goto 37
          CGX(ICOMP,N,1)=aa
          CGY(ICOMP,N,1)=ab
C Set third coordinate to dummy value:
          CGH(ICOMP,N,1)=0.0

        ELSEIF(IDFST(ICOMP,N).EQ.5)THEN
        
C Daylight coef method (5).
          helptopic='daylight_coef_method'
          call gethelptext(helpinsub,helptopic,nbhelp)
          J = INO - 12
          ICMDCF=ICMDCF+1
          IDCFID(ICOMP,N,J)=ICMDCF
  38      WRITE(HOLD,'(6F6.2,I2)')CGX(ICOMP,N,J),CGY(ICOMP,N,J),
     &      CGH(ICOMP,N,J),UX(ICOMP,N,J),UY(ICOMP,N,J),UH(ICOMP,N,J),
     &      IDCFID(ICOMP,N,J)
          CALL EASKS(HOLD,' Sensor x y z & ux uy uz',
     &      ' Sensor daycoeff no ',40,' 0. 0. 0. 0. 0. 1. 1',
     &      ' sensor x y z',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,aa,-100.,100.,'W','x coordinate',IER)
          CALL EGETWR(HOLD,K,ab,-100.,100.,'W','y coordinate',IER)
          CALL EGETWR(HOLD,K,ac,-100.,100.,'W','z coordinate',IER)
          CALL EGETWR(HOLD,K,ad,-100.,100.,'W','x dir vector',IER)
          CALL EGETWR(HOLD,K,ae,-100.,100.,'W','y dir vector',IER)
          CALL EGETWR(HOLD,K,af,-100.,100.,'W','z dir vector',IER)
          CALL EGETWI(HOLD,K,ig,1,MDCFP,'W','df number',IER)
          if(ier.ne.0)goto 38
          CGX(ICOMP,N,J)=aa; CGY(ICOMP,N,J)=ab; CGH(ICOMP,N,J)=ac
          UX(ICOMP,N,J)=ad; UY(ICOMP,N,J)=ae; UH(ICOMP,N,J)=af
          IDCFID(ICOMP,N,J)=ig

C Check if blind/ shuter control active for this thermal zone and if yes
C then set daylight coefficient stage number to 2 otherwise set to 1.
C Loop through all TMC in the zone and determine blind and shuter control 
C status.
          NDCFS(ig) = 1  
          DO 51 I=1,NSUR
            IF(ITMCFL(ICOMP,I).gt.0)then
              NTMC=ITMCFL(ICOMP,I)
              IF(IBCMT(ICOMP,NTMC).EQ.1) NDCFS(ig) = 2
            ENDIF
51        CONTINUE
         
C Set all daylight coefficients to -1 for this sensor (i.e. not calculated yet)
          do 996 ns = 1,NDCFS(ig)
            do 995 IP=1,145
              DCOEF(ig,ns,IP)=-1.
 995        continue
 996      continue
          ldcoef=.true.  ! ask when exiting about coefficient generation
          MODIFYVIEW=.TRUE.
          goto 20

        ELSEIF(IDFST(ICOMP,N).EQ.2)THEN

C Photocell orientation.
         IF(INO.GT.12.AND.INO.LT.(13+NDF(ICOMP,N)))THEN
          helptopic='photocell_orientation'
          call gethelptext(helpinsub,helptopic,nbhelp)
          J = INO - 12
  40      WRITE(HOLD,'(6F6.2)')CGX(ICOMP,N,J),CGY(ICOMP,N,J),
     &          CGH(ICOMP,N,J),UX(ICOMP,N,J),UY(ICOMP,N,J),UH(ICOMP,N,J)
          CALL EASKS(HOLD,' Sensor x y z & ux uy uz',
     &      ' ',40,' 0. 0. 0. 0. 0. 1.',' sensor x y z',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,aa,-100.,100.,'W','x coordinate',IER)
          CALL EGETWR(HOLD,K,ab,-100.,100.,'W','y coordinate',IER)
          CALL EGETWR(HOLD,K,ac,-100.,100.,'W','z coordinate',IER)
          CALL EGETWR(HOLD,K,ad,-100.,100.,'W','x dir vector',IER)
          CALL EGETWR(HOLD,K,ae,-100.,100.,'W','y dir vector',IER)
          CALL EGETWR(HOLD,K,af,-100.,100.,'W','z dir vector',IER)
          if(ier.ne.0)goto 40
          CGX(ICOMP,N,J)=aa
          CGY(ICOMP,N,J)=ab
          CGH(ICOMP,N,J)=ac
          UX(ICOMP,N,J)=ad
          UY(ICOMP,N,J)=ae
          UH(ICOMP,N,J)=af
          MODIFYVIEW=.TRUE.
          goto 20

         ELSEIF(INO.GT.(13+NDF(ICOMP,N)).AND.INO.LT.(NITEMS-5))THEN
          helptopic='user_supplied_dfactors'
          call gethelptext(helpinsub,helptopic,nbhelp)
          iww=ITMC2DF(ino)
  39      WRITE(HOLD2,'(9F7.4)')(DFDAT(ICOMP,N,IWW,KK),KK=1,
     &                           NDF(ICOMP,N))
          write(outs,'(a,a,a,i2,a)') 'For surface ',sname(icomp,iww),
     &          ' give DF for each photocell (',NDF(ICOMP,N),' values)'
          CALL EASKS(HOLD2,outs,' ',72,' 1 0.05',' df for each ',
     &      IER,nbhelp)
          K=0
          DO 94 J=1,NDF(ICOMP,N)
            CALL EGETWR(HOLD2,K,v1,0.,1.,'W','df',IER)
            DFDAT(ICOMP,N,IWW,J)=v1
   94     CONTINUE
          if(ier.ne.0)goto 39
C          MODIFYVIEW=.TRUE.
         ENDIF
        ENDIF
      ELSE
        INO=-1
        goto 30
      ENDIF
      goto 20

      END
