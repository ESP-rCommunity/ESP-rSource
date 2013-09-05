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
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,IDTY
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      common/user/browse
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/C21/IFCFG,cfgroot,LCFGF

C Zone casual gain control data.
      common/CGCIND/cgcdesc(MCOM)
      COMMON/CGCIN2N/NCGTC(MCOM,MDTY),NLITZ(MCOM),IDFST(MCOM,MLCOM),
     & CGX(MCOM,MLCOM,MDF),CGY(MCOM,MLCOM,MDF),CGH(MCOM,MLCOM,MDF),
     & UX(MCOM,MLCOM,MDF),UY(MCOM,MLCOM,MDF),UH(MCOM,MLCOM,MDF),
     & SETPT(MCOM,MLCOM),SYSPER(MCOM,MLCOM),SOFFLL(MCOM,MLCOM),
     & IOFFDT(MCOM,MLCOM),SMLOUT(MCOM,MLCOM),SMEOUT(MCOM,MLCOM)
      COMMON/CGCIN3/ICGCS(MCOM),ICGCF(MCOM),
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

      common/RAY3/MODIFY,MODLEN,MODBND

      DIMENSION ITMC2DF(30),KSTR(MDTY)
      DIMENSION  COG1(3),COG2(3),COG3(3),tip(3),ipoints(6,2)
      LOGICAL XST,lcouple,ldcoef
      logical MODIFY,MODLEN,MODBND
      logical browse

      character outs*124,LTMP*72,DTMP*64
      character KEY*1,hold*40,hold2*72,doitl*248,cfgroot*24
      character*48 ITEM(30)
      CHARACTER*72 DLCGCIN,LCFGF
      CHARACTER info*18,cgcdesc*64,brw*8
      INTEGER IX,IY,IZ,KSTR
      integer IRT  ! for radio button
      integer NITEMS,INO ! max items and current menu item

#ifdef OSI
      integer iix,iiy
#else
      integer*8 iix,iiy
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='EDCASCTL'  ! set for subroutine

C Assume daylight coeffs do need calculated.
      ldcoef=.true.

C Check and see if a casual gain control file exists.
      if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
        WRITE(DLCGCIN,'(A,A4)')zname(ICOMP)(1:lnzname(ICOMP)),'.cgc'
      else
        WRITE(DLCGCIN,'(3A,A4)') zonepth(1:lnblnk(zonepth)),'/',
     &    zname(ICOMP)(1:lnzname(ICOMP)),'.cgc'
      endif
      if(ICGC(ICOMP).NE.0)then
        call FINDFIL(LCGCIN(ICOMP),XST)
        if(XST)then

C Read existing file.
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
        LCGCIN(ICOMP)=DLCGCIN
        call FINDFIL(LCGCIN(ICOMP),XST)
        if(XST)then

C Read existing file.
          ICGC(ICOMP)=1
          call ercgcf(itrc,iuout,LCGCIN(ICOMP),ICOMP,ier)
          call edisp(iuout,'Opened existing casual gain control file.')
        else
          cgcdesc(icomp)='no description supplied'
          DO 400 IDTY=1,NBDAYTYPE
            NCGTC(ICOMP,IDTY)=2
 400      CONTINUE
          ICGCS(ICOMP)=8
          ICGCF(ICOMP)=18
          ICGCFL(ICOMP,1)=1
          IDFST(ICOMP,1)=1
          SETPT(ICOMP,1)=500.
          SOFFLL(ICOMP,1)=1.5
          SMLOUT(ICOMP,1)=0.1
          SMEOUT(ICOMP,1)=0.1
          IOFFDT(ICOMP,1)=0
          SYSPER(ICOMP,1)=1.0
          SPELEC(ICOMP,1)=15.0
          SLOPEM(ICOMP,1)=-0.025
          NDF(ICOMP,1)=1
          CGX(ICOMP,1,1)=0.0
          CGY(ICOMP,1,1)=0.0
          CGH(ICOMP,1,1)=0.0
          UX(ICOMP,1,1)=0.0
          UY(ICOMP,1,1)=0.0
          UH(ICOMP,1,1)=1.0
          IDCFID(ICOMP,1,1)=1
          N=1  ! set N
          NLITZ(ICOMP)=1
          NUDWIN(ICOMP)=1
          do 48 I = 1, NSUR
            DFDAT(ICOMP,1,I,1)=0.0
  48      continue

          LTMP=DLCGCIN
  246     CALL EASKS(LTMP,' Zone casual gain control file ?',
     &      ' ',72,DLCGCIN,'cgc control file',IER,nbhelp)
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
      MODIFY=.TRUE.
      MODBND=.TRUE.

C Rescan zone Construction and TMC file 
C (need to test blind/ shuter control flags).
      call FINDFIL(LTHRM(ICOMP),XST)
      if(XST)then
        CALL ECONST(LTHRM(ICOMP),IUF,ICOMP,0,IUOUT,IER)
      else
         write(outs,'(A,I3,A)') ' WARNING: No Constr. file for zone ',
     &                     ICOMP,' blind control tracking not possible!'
         call edisp(iuout,outs)
      endif
      if(ITW(ICOMP).eq.1)then
        CALL ERTWIN(0,IUOUT,IUF,LTWIN(ICOMP),ICOMP,IER)
      else
        write(outs,'(A,I3,A)') ' WARNING: No TMC file for zone ',ICOMP,
     &                        ' blind control tracking not possible!'
        call edisp(iuout,outs)
      endif         
      MODIFY=.TRUE.
      MODBND=.TRUE.

C Begin composing the display.
  20  continue

C Reset all surface lines to standard width and redraw if revised data.
      if(MODIFY)then
        CALL INLNST(1)
        nzg=1
        nznog(1)=ICOMP
        izgfoc=ICOMP
        CALL ADJVIEW(IER)
      endif
      NL=NLITZ(ICOMP)
      write(ITEM(1),'(A,A)')   '  Zone: ',zname(ICOMP)
      write(ITEM(2),'(A,A36)') '  Desc: ',cgcdesc(ICOMP)(1:36)
      write(ITEM(3),'(A)') 'a control for daytypes '
      write(ITEM(4),'(A,2I3)') 'b Control periods  : ',ICGCS(ICOMP),
     &                            ICGCF(ICOMP)
      write(ITEM(5),'(A,I3)')  '  Lighting zones   : ',NL
      ITEM(6)=            '  _______________________________________'

C If we got to this point and no lighting zone then there should be at least one.
      if (N.le.0) N=1 ! bounds error fix
      write(ITEM(7),'(A,I2)')    'c Lighting zone >> ',N
      info='unknown control'
      if(ICGCFL(ICOMP,N).eq.-1) info='Always On'
      if(ICGCFL(ICOMP,N).eq.0)  info='Always Off'
      if(ICGCFL(ICOMP,N).eq.1)  info='On-Off'
      if(ICGCFL(ICOMP,N).eq.2)  info='Step 100:50:0'
      if(ICGCFL(ICOMP,N).eq.3)  info='Dimming - ideal'
      if(ICGCFL(ICOMP,N).eq.4)  info='On-Off probability'
      if(ICGCFL(ICOMP,N).eq.31) info='Dimming-int. reset'
      if(ICGCFL(ICOMP,N).eq.32) info='Dimming-closd loop'
      write(ITEM(8),'(A,A)') 'd Control type    : ',info(1:lnblnk(info))
      if(IDFST(ICOMP,N).eq.1)then
        write(ITEM(9),'(A)')  'e Calculation type: Analytical DF'
      elseif(IDFST(ICOMP,N).eq.2)then
        write(ITEM(9),'(A)')  'e Calculation type: User def. DF'
      elseif(IDFST(ICOMP,N).eq.3)then
        write(ITEM(9),'(A)')  'e Calculation type: External sensor'
      elseif(IDFST(ICOMP,N).eq.4)then
        write(ITEM(9),'(A)')  'e Calculation type: Time step coupling'
      elseif(IDFST(ICOMP,N).eq.5)then
        write(ITEM(9),'(A)')  'e Calculation type: Daylight coeff.' 
      endif
      write(ITEM(10),'(A,4F6.1,I3)') 'f Control data  :',SETPT(ICOMP,N),
     & SOFFLL(ICOMP,N),SMLOUT(ICOMP,N),SMEOUT(ICOMP,N),IOFFDT(ICOMP,N)
      write(ITEM(11),'(A,F6.2,I3)') 'g Photocell data:',SYSPER(ICOMP,N),
     &                              NDF(ICOMP,N)

      if(IDFST(ICOMP,N).EQ.1.OR.IDFST(ICOMP,N).EQ.2.OR.
     &   IDFST(ICOMP,N).EQ.4)then

C For each sensor for methods 1,2,4.
        write(ITEM(12),'(A)') '  Posn: x  y   z   ux   uy   uz'
        m=12
        do 42 J = 1,NDF(ICOMP,N)
          m=m+1
          CALL EMKEY(m,KEY,IER)
          write(ITEM(m),'(A,6F6.2)') key,CGX(ICOMP,N,J),CGY(ICOMP,N,J),
     &      CGH(ICOMP,N,J),UX(ICOMP,N,J),UY(ICOMP,N,J),UH(ICOMP,N,J)
          COG1(1)=CGX(ICOMP,N,J)
          COG1(2)=CGY(ICOMP,N,J)
          COG1(3)=CGH(ICOMP,N,J)
          CALL VECTRN(COG1,TEMAT,COG2,IER)
          CALL VECTRN(COG2,ETSMAT,COG3,IER)
          call u2pixel(COG3(1),COG3(2),iix,iiy)
          CALL ecirc(iix,iiy,2,1)
          tip(1)=CGX(ICOMP,N,J)+UX(ICOMP,N,J)
          tip(2)=CGY(ICOMP,N,J)+UY(ICOMP,N,J)
          tip(3)=CGH(ICOMP,N,J)+UH(ICOMP,N,J)
          call arrow(COG1,tip,0.3,0.1,ipoints,'a',1)
  42    continue
        call forceflush()
      elseif(IDFST(ICOMP,N).EQ.3)then
C Method 3 (external sensor).
        write(ITEM(12),'(A)') '  External photocell posit. & orient:'
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
          CALL VECTRN(COG1,TEMAT,COG2,IER)
          CALL VECTRN(COG2,ETSMAT,COG3,IER)
          call u2pixel(COG3(1),COG3(2),iix,iiy)
          CALL ecirc(iix,iiy,2,1)
          tip(1)=CGX(ICOMP,N,J)+UX(ICOMP,N,J)
          tip(2)=CGY(ICOMP,N,J)+UY(ICOMP,N,J)
          tip(3)=CGH(ICOMP,N,J)+UH(ICOMP,N,J)
          call arrow(COG1,tip,0.3,0.1,ipoints,'a',1)
  43    continue
        call forceflush()
      endif

C In special case...
      if(IDFST(ICOMP,N).EQ.2)then
        m=m+1   
        write(ITEM(m),'(A)') '  TMC number & DF(s) for each photocell:'
        do 44 ii = 1,NSUR
          if(SOTF(II)(1:4).NE.'OPAQ'.AND.SOTF(II)(1:4).NE.'CFC ')THEN
            m=m+1
            CALL EMKEY(m,KEY,IER)
            write(ITEM(m),'(A,I4,2X,F6.4,a)')key,II,DFDAT(ICOMP,N,II,1),
     &            '.......'
            ITMC2DF(m)=ii
          endif
  44    continue
      endif

      ITEM(M+1)=          '  ____________________________________  '
      ITEM(M+2)=          '* add/ delete lighting zone             '
      ITEM(M+3)=          '! list casual gain control information  '
      ITEM(M+4)=          '> save casual gain control information  '
      ITEM(M+5)=          '? help                                  '
      ITEM(M+6)=          '- exit this menu                        '
      NITEMS=M+6

      INO = -2
   30 CALL EMENU('Casual Gain Control',ITEM,NITEMS,INO)

      IF (INO.EQ.NITEMS)THEN

C If browsing pass this information to e2r.
        if(browse)then
          brw = ' -b yes'
        else
          brw = ' -b no '
        endif

C Check for control using Radiance.
        helptopic='casual_gain_control'
        call gethelptext(helpinsub,helptopic,nbhelp)
        lcouple=.false.
        DO 560 ILZ=1,NLITZ(ICOMP)
          if (IDFST(ICOMP,N).eq.4) lcouple=.true.
 560    continue
        if (lcouple) then
          write(doitl,'(3a,i3,a)')'e2r -file ',LCFGF(1:lnblnk(LCFGF)),
     &      ' -purpose Coupling -zone ',ICOMP,' -act Create'
          call edisp (iuout,doitl)
          call runit(doitl,'-')
        elseif (ldcoef) then
 8888     call EASKABC('Radiance model for daylight coefficients:',' ',
     &    'Create and edit model','Calculate coeffs','Continue',
     &    IW,nbhelp)
          if (IW.eq.1) then
            write(doitl,'(3a,i3,2a)')'e2r -file ',
     &        LCFGF(1:lnblnk(LCFGF)),' -purpose Day_coef -zone ',
     &        ICOMP,' -act Create',brw
          elseif (IW.eq.2) then
            write(doitl,'(3a,i3,3a)')'e2r -file ',
     &        LCFGF(1:lnblnk(LCFGF)),' -purpose Day_coef -zone ',
     &        ICOMP,' -act Compute',brw,' &'
          elseif (IW.eq.3) then
            goto 9999
          endif
          call edisp (iuout,doitl)
          call runit(doitl,'-')
          goto 8888
        endif
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
        DO 401 IDTY=1,NBDAYTYPE
          write(outs,9989)CALENTAG(IDTY),NCGTC(ICOMP,IDTY)
 9989     format(' Casual gain type for control during ',1A,' = ',1I3)
          call edisp(itru,outs)
 401    CONTINUE
        DO 117 NLZ=1,NLITZ(ICOMP)
          write(outs,'(A,I1)') ' Light zone ',NLZ
          call edisp(iuout,outs)
          if(IDFST(ICOMP,NLZ).eq.1)then
            write(outs,'(A,I2,A)') ' Average of ',NDF(ICOMP,NLZ),
     &        ' analytical daylight factor photocell types. '
            call edisp(iuout,outs)
          elseif(IDFST(ICOMP,NLZ).eq.2)then
            write(outs,'(A,I2,A)') ' Average of ',NDF(ICOMP,NLZ),
     &        ' user spec. daylight factor photocell types. '
            call edisp(iuout,outs)
            write(outs,'(A,I2,A)') ' There are ',NUDWIN(ICOMP),
     &        ' TMC (transparent) polygons defined. '
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
     &    ' Set   |Switch-off |Min. dim |Min. ele |Switch-off ')
          call edisp(iuout,
     &    ' point |light level|light out|light out|delay time ')
          write(outs,9987)SETPT(ICOMP,NLZ),SOFFLL(ICOMP,NLZ),
     &      SMLOUT(ICOMP,NLZ), SMEOUT(ICOMP,NLZ),IOFFDT(ICOMP,NLZ)
 9987     format(F7.1,F12.1,F10.2,F10.2,I10)
          call edisp(iuout,outs)

          call edisp(iuout,'  ')
          call edisp(iuout,' % of |No. of|Calcul| ')
          call edisp(iuout,' whole|sensor|type  | ')
          write(outs,9988)SYSPER(ICOMP,NLZ),NDF(ICOMP,NLZ),
     &                    IDFST(ICOMP,NLZ)
 9988     format(F6.2,I7,I7)
          call edisp(iuout,outs)

          call edisp(iuout,'  ')
          if(IDFST(ICOMP,NLZ).eq.1.or.IDFST(ICOMP,NLZ).eq.2.or.
     &      IDFST(ICOMP,NLZ).eq.4)then
           call edisp(iuout,
     &       ' Sensor|   X  |   Y  |   Z  |  DX |  DY |  DZ |')
           call edisp(iuout,
     &       ' no.   |      |      |      |     |     |     |')
          elseif(IDFST(ICOMP,NLZ).eq.5)then
           call edisp(iuout,
     &       ' Sensor|   X  |   Y  |   Z  |  DX |  DY |  DZ |  DC| ')
           call edisp(iuout,
     &       ' no.   |      |      |      |     |     |     |    | ')
          elseif(IDFST(ICOMP,NLZ).eq.3)then
            call edisp(iuout,' Sensor | Sensor |')
            call edisp(iuout,' posit. | type   |')
          endif

          DO 9986 J=1,NDF(ICOMP,NLZ)
            if(IDFST(ICOMP,NLZ).eq.1.or.IDFST(ICOMP,NLZ).eq.2.or.
     &        IDFST(ICOMP,NLZ).eq.4)then
              write(outs,9914)J,CGX(ICOMP,NLZ,J),CGY(ICOMP,NLZ,J),
     &        CGH(ICOMP,NLZ,J),UX(ICOMP,NLZ,J),UY(ICOMP,NLZ,J),
     &        UH(ICOMP,NLZ,J)
 9914         format(i7,3F7.2,3F6.2)
              call edisp(iuout,outs)
            elseif(IDFST(ICOMP,NLZ).eq.5)then
              write(outs,9915)J,CGX(ICOMP,NLZ,J),CGY(ICOMP,NLZ,J),
     &        CGH(ICOMP,NLZ,J),UX(ICOMP,NLZ,J),UY(ICOMP,NLZ,J),
     &        UH(ICOMP,NLZ,J),IDCFID(ICOMP,NLZ,J)
 9915         format(i7,3F7.2,3F6.2,I4)
              call edisp(iuout,outs)
            elseif(IDFST(ICOMP,NLZ).eq.3)then
              write(outs,9916)CGX(ICOMP,NLZ,1),CGY(ICOMP,NLZ,1)
 9916         format(F8.1,F9.1)
              call edisp(iuout,outs)
            endif
 9986     CONTINUE

          call edisp(iuout,'  ')
          DO 9985 J=1,NDF(ICOMP,NLZ)
            if(IDFST(ICOMP,NLZ).EQ.2)then
              call edisp(iuout,' ')
              call edisp(iuout,' TMC | Daylight | ')
              call edisp(iuout,' no. | factor(%)| ')
              idd=0
              DO 814 IL=1,NZSUR(icomp)
                if(idd.lt.NUDWIN(ICOMP))then
                  if(DFDAT(ICOMP,NLZ,IL,J).gt.0.0)then
                    idd=idd+1
                    write(outs,9917)IL,DFDAT(ICOMP,NLZ,IL,J)*100.
 9917               format(I5,F10.2)
                    call edisp(iuout,outs)
                  endif
                endif
  814         CONTINUE
            endif
 9985     CONTINUE

          call edisp(iuout,' ')
          call edisp(iuout,' Start| Finish| Control     | ')
          call edisp(iuout,'  Hour|   Hour|    Type     | ')
          info='unknown control'
          if(ICGCFL(ICOMP,NLZ).eq.-1) info='Always On'
          if(ICGCFL(ICOMP,NLZ).eq.0)  info='Always Off'
          if(ICGCFL(ICOMP,NLZ).eq.1)  info='On-Off'
          if(ICGCFL(ICOMP,NLZ).eq.2)  info='Step 100:50:0'
          if(ICGCFL(ICOMP,NLZ).eq.3)  info='Dimming - ideal'
          if(ICGCFL(ICOMP,NLZ).eq.4)  info='On-Off probability'
          if(ICGCFL(ICOMP,NLZ).eq.31) info='Dimming-int. reset'
          if(ICGCFL(ICOMP,NLZ).eq.32) info='Dimming-closd loop'

          write(outs,'(2I7,1x,A)')ICGCS(ICOMP),ICGCF(ICOMP),info
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
          ICGCFL(ICOMP,N)=1
          IDFST(ICOMP,N)=1
          SETPT(ICOMP,N)=500.
          SOFFLL(ICOMP,N)=1.5
          SMLOUT(ICOMP,N)=0.1
          SMEOUT(ICOMP,N)=0.1
          IOFFDT(ICOMP,N)=0
          SYSPER(ICOMP,N)=1.0
          SPELEC(icomp,N)=15.0
          SLOPEM(icomp,N)=-0.025
          NDF(ICOMP,N)=1
          CGX(ICOMP,N,1)=0.0
          CGY(ICOMP,N,1)=0.0
          CGH(ICOMP,N,1)=0.0
          UX(ICOMP,N,1)=0.0
          UY(ICOMP,N,1)=0.0
          UH(ICOMP,N,1)=1.0
          NUDWIN(ICOMP)=1

C Check for existing daylight coefficients.
          ICMDCF=0
          do 145 IC=1,NCOMP
            do 146 ILZ=1,NLITZ(ICOMP)
              do 147 ISE=1,NDF(ICOMP,ILZ)
                ICMDCF=MAX(ICMDCF,IDCFID(IC,ILZ,ISE))
 147          continue
 146        continue
 145      continue
          do 49 I = 1, NSUR
            DFDAT(ICOMP,N,I,1)=0.0
  49      continue

          call edisp(iuout,'New lighting zone filled with defaults.')
          MODIFY=.TRUE.
        ELSEIF(IW.EQ.1)THEN

C Delete lighting zone.  << ... >>
        ENDIF

      ELSEIF (INO.EQ.2)THEN
C General description of the model.
        DTMP=cgcdesc(ICOMP)
 52     CALL EASKS(DTMP,
     &  ' Please supply a brief description of the model:',
     &    ' ',64,'Dimming control','model description',IER,nbhelp)
        IF(IER.NE.0)GOTO 52
        IF(LTMP(1:2).NE.'  ')THEN
          cgcdesc(ICOMP)=DTMP  
        endif    

      ELSEIF (INO.EQ.3)THEN
C Weekday sat sun control
        CALL EASKAB(' ','Control casual gains for ','one day type',
     &     'all day types',IY,nbhelp)
        IF(IY.EQ.1)THEN
          IX=1
          CALL EPICKS(IX,KSTR,' ',
     &    'choose daytype to control casual gains'
     &    ,12,nbdaytype,calentag,'Choose daytype',IER,nbhelp)
          IDTY=KSTR(1)
        ENDIF
        CALL EASKABC(' ','Control casual gain type ','occupants',
     &     'lighting','equipment',IZ,nbhelp)
        IF(IY.EQ.1)THEN
          NCGTC(ICOMP,IDTY)=IZ
        ELSEIF(IY.EQ.2)THEN
          DO 404 IDTY=1,NBDAYTYPE
            NCGTC(ICOMP,IDTY)=IZ
 404      CONTINUE
        ENDIF

      ELSEIF (INO.EQ.4)THEN
C Control periods.
  34    WRITE(HOLD,'(2I6)')ICGCS(ICOMP),ICGCF(ICOMP)
        CALL EASKS(HOLD,' Control is active from hour to hour:',
     &    ' ',40,' 8  18 ',' cast ctl periods',IER,nbhelp)
        K=0
        CALL EGETWI(HOLD,K,ia,0,23,'W','period start',IER)
        CALL EGETWI(HOLD,K,ib,ia+1,24,'W','period end',IER)
        if(ier.ne.0)goto 34
        ICGCS(ICOMP)=ia
        ICGCF(ICOMP)=ib

      ELSEIF (INO.EQ.7)THEN
C Toggle and then reform the menu to match control/calc type.
        N=N+1
        if(N.gt.NL)N=1
        MODIFY=.TRUE.
        goto 20

      ELSEIF (INO.EQ.8)THEN
C Control type.
        IRT=1
        call EASKATOG(' Control type:',' ',
     &    'Alway On','Alway Off','On-Off','Step','Dimming','Probab',
     &    'cancel',IRT,nbhelp)
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
            SPELEC(ICOMP,N)=SIG
            SLOPEM(ICOMP,N)=0.0
          elseif(irt.eq.3)then
            ICGCFL(ICOMP,N)=32
            SIG=SPELEC(ICOMP,N)
            SLP=SLOPEM(ICOMP,N)
  45        WRITE(HOLD,'(F6.2,F8.4)')SPELEC(ICOMP,N),SLOPEM(ICOMP,N)
            CALL EASKS(HOLD,' Night-time calibration lux value &',
     &        ' Control slope',40,' 15.0  -0.025',' calib & slope',
     &        IER,nbhelp)
            K=0
            CALL EGETWR(HOLD,K,sig,0.,2000.,'W','night calib',IER)
            CALL EGETWR(HOLD,K,slp,-1.,1.,'W','contr slope',IER)
        if(ier.ne.0)goto 45
            SPELEC(ICOMP,N)=SIG
            SLOPEM(ICOMP,N)=SLP
          endif
        elseif(irt.eq.6)then
          ICGCFL(ICOMP,N)=4
        endif

      ELSEIF (INO.EQ.9)THEN
C Calculation type.
        helptopic='cas_ctl_photocell'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('cgs calct type',nbhelp,'photocell',0,0,IER)
        IRT=1
        call EASKATOG(' Calculation type:',' ',
     &      'Analyt DF','User DF','Extern','Coupling','DayCoeff',
     &      'cancel',' ',IRT,nbhelp)
        if(irt.eq.1)then
          IDFST(ICOMP,N)=1
          ldcoef=.false.
        elseif(irt.eq.2)then
          IDFST(ICOMP,N)=2
          ldcoef=.false.
C Loop through all surfaces and find out number of TMCs:
          ICOUNT=0
          do 47 i = 1, NSUR
            if(SOTF(I)(1:4).NE.'OPAQ'.AND.SOTF(I)(1:4).NE.'CFC ')THEN
              ICOUNT=ICOUNT+1              
            endif
47        continue 
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
        MODIFY=.TRUE.
        goto 20

      ELSEIF (INO.EQ.10)THEN
C Control data.
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
        SETPT(ICOMP,N)=aa
        SOFFLL(ICOMP,N)=ab
        SMLOUT(ICOMP,N)=ac
        SMEOUT(ICOMP,N)=ad
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
            CGX(ICOMP,N,ib)=0.0
            CGY(ICOMP,N,ib)=0.0
            CGH(ICOMP,N,ib)=0.0
            UX(ICOMP,N,ib)=0.0
            UY(ICOMP,N,ib)=0.0
            UH(ICOMP,N,ib)=1.0
          elseif(ib.lt.NDF(ICOMP,N))then
            call usrmsg('Cannot remove sensors at this time...',' ','W')
          endif
        endif
        MODIFY=.TRUE.
        goto 20

      ELSEIF (INO.GT.12.AND.INO.LT.(NITEMS-5))THEN
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
          CGX(ICOMP,N,J)=aa
          CGY(ICOMP,N,J)=ab
          CGH(ICOMP,N,J)=ac
          UX(ICOMP,N,J)=ad
          UY(ICOMP,N,J)=ae
          UH(ICOMP,N,J)=af
          MODIFY=.TRUE.
          goto 20

        ELSEIF(IDFST(ICOMP,N).EQ.3)THEN
C External sensor method.
          helptopic='ctl_external_location'
          call gethelptext(helpinsub,helptopic,nbhelp)
  37      WRITE(HOLD,'(2F6.2)')CGX(ICOMP,N,1),CGY(ICOMP,N,1)
          CALL EASKS(HOLD,' Sensor surface number position &',
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
          CGX(ICOMP,N,J)=aa
          CGY(ICOMP,N,J)=ab
          CGH(ICOMP,N,J)=ac
          UX(ICOMP,N,J)=ad
          UY(ICOMP,N,J)=ae
          UH(ICOMP,N,J)=af
          IDCFID(ICOMP,N,J)=ig

C Check if blind/ shuter control active for this thermal zone and if yes
C then set daylight coefficient stage number to 2 otherwise set to 1.
C Loop through all TMC in the zone and determine blind and shuter control 
C status.
          NDCFS(ig) = 1  
          DO 51 I=1,NSUR
            IF(ITMCFL(ICOMP,I).gt.0)then
              NTMC=ITMCFL(ICOMP,I)

C Debug.
C              write(6,*) 'TMC No. =',NTMC 
           
              IF(IBCMT(ICOMP,NTMC).EQ.1) NDCFS(ig) = 2

C Debug.
C              write(6,*) 'Suface No. =',I
C              write(6,*) 'Blind flag = ',IBCMT(ICOMP,NTMC)

            ENDIF
51        CONTINUE

C Debug.
C          write(6,*) 'No. of DC stages = ',NDCFS(ig)
         
C Set all daylight coefficients to -1 for this sensor (i.e. not calculated yet)
          do 996 ns = 1,NDCFS(ig)
            do 995 IP=1,145
              DCOEF(ig,ns,IP)=-1.
 995        continue
 996      continue
          MODIFY=.TRUE.
          goto 20

        ELSEIF(IDFST(ICOMP,N).EQ.2)THEN
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
          MODIFY=.TRUE.
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
C          MODIFY=.TRUE.
         ENDIF
        ENDIF
      ELSE
        INO=-1
        goto 30
      ENDIF
      goto 20

      END
