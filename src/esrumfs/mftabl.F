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

C Tabular output for network air flow: 
C   MFTABL formatted tabular output
C   MFRTBL raw tabular output.

C Note: MFTABL and MFRTBL used with ASCII version of network flow results
C       file and is not called for versions after November 2002.
C ***************** MFTABL 
C Tabular output (formatted) of results; local variables:
C     IDIM   - flow units: kg/s (IDIM=1), m^3/hr (IDIM=2), 1/hr (IDIM=3)
C     MNDO   - maximum number of nodes in table; current lay-out based
C              on 2 nodes!
C     NDOFIN - total flow entering nodes in table
C     NDOFUT - total flow leaving nodes in table
C     NDONAM - names of nodes in table
C     NDONUM - numbers of nodes in table
C     NTSPD  - number of simulation time steps per day
C     NTSPH  - number of simulation time steps per hour
C     VOL() - volume used for calculating change rate in case IDIM=3

      SUBROUTINE MFTABL(itrc,IFRES,ier)

#include "net_flow.h"
#include "espriou.h"
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      COMMON/DISPL/IDAYF,IDAYL,IFDAY,IFTIME
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW

      PARAMETER       (MNDO=2)

      CHARACTER*23     ITEM(9)
      CHARACTER*12      NDONAM(MNDO)
      DIMENSION        NDONUM(MNDO)
      DIMENSION        VOL(MNDO)
      DOUBLE PRECISION NDOFIN(MNOD),NDOFUT(MNOD)

      character WORD*20,outs*124,OUTSTR*124
      character PERST1*14,PERST2*44,PERST3*44,NDNAM*12
      integer NITMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created
      helpinsub='MFTABL' ! set for subroutine

      IDIM=1
      VOL(1)=1.
      NNDO=0
      DO 5 INDO=1,MNDO
        NDONUM(INDO)=0
        NDONAM(INDO)=' none '
    5 CONTINUE

C Enable output from previous run; i.e. find IFYD,ILYD, and
C number of simulation time steps per hour and day
      CALL MFDAYS(IFRES,NNOD,NCNN,IFYD,ILYD,IER)
      IF(IER.NE.0)then
        call edisp(iuout,' Problem sorting out simulation period.')
        GOTO 100
      endif

      CALL MFTSPH(IFRES,NNOD,NCNN,NTSPH,IER)
      IF(IER.NE.0)then
        call edisp(iuout,' Problem sorting out simulation timesteps.')
        GOTO 100
      endif
      NTSPD=24*NTSPH

C Set output period (IDAYF up to IDAYL) to IFYD and ILYD if not
C yet set. 
      if(IDAYF.eq.0.or.IDAYL.eq.0)then
        IDAYF=IFYD
        IDAYL=ILYD
      endif

C Set up menu.
   10 INO=-2
      ITEM(1) ='p Analysis period      '
      ITEM(2) ='u Reporting units:     '
      ITEM(3) ='  ___________________  '
      ITEM(4) ='a Select nodes         '
      ITEM(5) ='b Display data         '
      ITEM(6) ='c Deselect nodes       '
      ITEM(7) ='  ___________________  '
      ITEM(8) ='? Help                 '
      ITEM(9) ='- Exit                 '
      NITMS=9

C Help text for menu.
      helptopic='mfs_analysis_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

   12 CALL EMENU('Reporting (formatted)',ITEM,NITMS,INO)

      IF(INO.EQ.1)THEN

C Change output start and stop dates, to allow output
C for less than entire simulation period
C Display format for date is Fri 10 Jan. 
   30   IVBTIM=1
        NTSPH=1
        IVETIM=NTSPH*24
        CALL EPERSTR(IRY,IFYD,IVBTIM,ILYD,IVETIM,NTSPH,
     &           IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)

        WRITE(outs,'(A,A)')' Results available: ',PERST2
        call edisp(iuout,outs)

        CALL EPERSTR(IRY,IDAYF,IVBTIM,IDAYL,IVETIM,NTSPH,
     &           IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
        WRITE(outs,'(A,A)')' Current output   : ',PERST2
        call edisp(iuout,outs)

        CALL EASKPER('Output period:',IDAYF,IDAYL,IFDAY,IER)
        IF(IER.NE.0) GOTO 30
        IF(IDAYF.LT.IFYD.OR.IDAYF.GT.IDAYL.OR.IDAYL.GT.ILYD) GOTO 30
      ELSEIF(INO.EQ.2)THEN

C Change flow results output units
   40   helptopic='mfs_express_flow_results'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABC(' Express flow results as: ',' ',
     &   'kg/s','volume as m^3/hr','volume as ac/h',IDIM,nbhelp)
        IF(IER.NE.0) GOTO 40
      ELSEIF(INO.EQ.4)THEN

C Select nodes.
   20   IF(NNDO.EQ.MNDO) GOTO 10
        CALL ASKRNOD(' current nodes','-',INOD,IER)
        IF(IER.NE.0) GOTO 20
        IF(INOD.EQ.0) GOTO 10
        NNDO=NNDO+1
        NDONAM(NNDO)=NDNAM(INOD)
        NDONUM(NNDO)=INOD

C Node ok, inform user of it`s selection.
        write(outs,'(1X,A,A)') NDNAM(INOD),' added to the display list.'
        call edisp(iuout,outs)
        IF(IDIM.EQ.3) THEN
          helptopic='mfs_zone_volume_evaluated'
          call gethelptext(helpinsub,helptopic,nbhelp)
          VOLUME=SUPNOD(INOD,2)
          write(outs,'(A,A12,A,F8.2,A)')' The current volume of ',
     &                 NDNAM(INOD),' is ',VOLUME,'m^3'
          CALL EASKR(VOLUME,' ',
     &      ' Volume (m^3) in evaluating fluid change rates/hour? ',
     &      0.0,'F',0.,'-',1.,'volume per hour',IER,nbhelp)
          VOL(NNDO)=VOLUME
        ELSE
          VOL(NNDO)=1.0
        END IF
        goto 20
      ELSEIF(INO.EQ.5)THEN
        goto 50
      ELSEIF(INO.EQ.6)THEN

C Reset numbers and names of nodes for output.
        NNDO=0
        DO 61 INDO=1,MNDO
          NDONUM(INDO)=0
          NDONAM(INDO)=' none '
   61   CONTINUE
      ELSEIF(INO.EQ.8)THEN

c Help facilities. 
        helptopic='mfs_analysis_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('pretty tabular ',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.9)THEN
        GOTO 100
      ELSE
        ino=-1
        goto 12
      endif
      goto 10

C Position read-pointer at first record of simulation day IDAYF

C Flush current table through IUOUT or to a file
   50 if(NNDO.EQ.0)then
        call usrmsg(' Nothing selected yet,',
     &              ' returning to start of reporting...','W')
        GOTO 10
      endif
      itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call edisp(iuout,' Output will be directed to file... ')
      endif

      CALL MFSKIP(IFRES,NNOD,NCNN,IFYD,IDAYF,IER)
      if(IER.NE.0)then
        call edisp(iuout,' Problem positioning point in results file.')
        GOTO 100
      endif

C Start day part of main output loop by writing table header
      DO 59 IDAY=IDAYF,IDAYL
      CALL EDAYR(IDAY,ID,IM)
      call edisp(itru,' ')
      call edisp(itru,
     &' Node pressure and fluid flow entering (+ve) or leaving (-ve)')
      write(outs,'(A,I3,A,I2,A,I4,A)')' Day',ID,' of month ',IM,
     &     ' (',IRY,')'
      call edisp(itru,outs)
      write(outs,1051)(NDONAM(INDO),INDO=1,MNDO)
1051  format(' Hour  ',2('|',10X,A12,10X))
      call eddisp(itru,outs)
      IF(IDIM.EQ.1) THEN
         WRITE(outs,10502) ' kg/s ',' kg/s ',' kg/s ',' kg/s '
         call eddisp(itru,outs)
      ELSE IF(IDIM.EQ.2) THEN
         WRITE(outs,10502) 'm^3/hr','m^3/hr','m^3/hr','m^3/hr'
         call eddisp(itru,outs)
      ELSE
         WRITE(outs,10502) ' 1/hr ',' 1/hr ',' 1/hr ',' 1/hr '
         call eddisp(itru,outs)
      END IF
      WRITE(outs,1053)
      call edisp(itru,outs)
10502 FORMAT('       ',2('|',3X,'Pa',5X,2(2X,A6,3X)))
1053  FORMAT(' ======',2('|',32('=')))

C Start hour part of output loop by reading flow rates and pressures
      DO 58 IH=1,NTSPD

C First read hour value of this time step.
      CALL STRIPC(IFRES,OUTSTR,0,ND,1,'time line',IERL)
      K=18
      CALL EGETWR(OUTSTR,K,TIME,0.,0.,'-','time',IERL)

C Start reading node fluid types, temperatures and pressures;
C first skip header
      CALL STRIPC(IFRES,OUTSTR,0,ND,1,'node header',IERL)

C Read results node by node into a string.
C Parse this string and recapture fluid type, temperature, pressure.
      DO 52 INOD=1,NNOD
        CALL STRIPC(IFRES,OUTSTR,0,ND,1,'node data',IERL)
        K=0
        IERL=0

        CALL EGETW(OUTSTR,K,WORD,'-','node',IERL)
        CALL EGETWI(OUTSTR,K,IV1,1,2,'-','fluid',IERL)
        CALL EGETW(OUTSTR,K,WORD,'-','??',IERL)
        CALL EGETWR(OUTSTR,K,RV1,0.,0.,'-','temp',IERL)
        CALL EGETW(OUTSTR,K,WORD,'-','??',IERL)
        CALL EGETWR(OUTSTR,K,RV2,0.,0.,'-','pressure',IERL)
        NDFLD(INOD)=IV1
        TNOD(INOD)=RV1
        PRES(INOD)=RV2
        IF(IERL.NE.0)then
          call edisp(iuout,' Problem reading node data.')
          GOTO 100
        endif
   52 CONTINUE

C Start reading connection mass flow rates; first skip header.
      CALL STRIPC(IFRES,OUTSTR,0,ND,1,'header',IERL)

C Read results connection by connection into a string.
      DO 54 ICNN=1,NCNN
        CALL STRIPC(IFRES,OUTSTR,0,ND,1,'connection data',IERL)
        K=0
        IERL=0

C Parse this string and recapture flow 1 & 2.
        CALL EGETW(OUTSTR,K,WORD,'-','mfr conn w1',IERL)
        CALL EGETW(OUTSTR,K,WORD,'-','mfr conn w2',IERL)
        CALL EGETW(OUTSTR,K,WORD,'-','mfr conn w3',IERL)
        CALL EGETW(OUTSTR,K,WORD,'-','mfr conn w4',IERL)
        CALL EGETW(OUTSTR,K,WORD,'-','mfr conn w5',IERL)
        CALL EGETW(OUTSTR,K,WORD,'-','mfr conn w6',IERL)
        CALL EGETW(OUTSTR,K,WORD,'-','mfr conn w7',IERL)
        CALL EGETWR(OUTSTR,K,RV1,0.,0.,'-','flow 1',IERL)
        CALL EGETWR(OUTSTR,K,RV2,0.,0.,'-','flow 2',IERL)
        FLW1(ICNN)=RV1
        FLW2(ICNN)=RV2
        IF(IERL.NE.0)then
          call edisp(iuout,' Problem reading connection data.')
          GOTO 100
        endif
   54 CONTINUE

C Calculate each node's fluid flow entering/leaving (kg/s or m^3/hr)
      CALL MFSTFL(IDIM,NDOFIN,NDOFUT)

C Write pressure and fluid flow entering/leaving
      WRITE(outs,10505)
     &      TIME,(PRES(NDONUM(INDO)),NDOFIN(NDONUM(INDO))/VOL(INDO),
     &      NDOFUT(NDONUM(INDO))/VOL(INDO),INDO=1,NNDO)
10505 FORMAT(1X,F5.2,1X,2('|',G10.4,1X,G10.4,1X,G10.4))
      call eddisp(itru,outs)
   58 CONTINUE
      CALL EPAGEW
   59 CONTINUE
      GOTO 10

  100 RETURN
      END

C *************** MFRTBL
C Raw tabular output of results; local variables:
C     COLCNT - column contents
C     COLHDS - column headers
C     DATTIM - Julian date; where Jan 1th 1:00 = 1.0
C     ICOLIX - column contents index;
C              ICOLIX(i,j) holds data for column i; if ICOLIX(i,1)
C               =1 then ICOLIX(i,2)=node whose pressure will be written
C               =2 then ICOLIX(i,2)=conn whose flow will be written
C     IDIM   - flow units: kg/s (IDIM=1), m^3/hr (IDIM=2), 1/hr (IDIM=3)
C     MCOLT  - maximum number of columns in table
C     NCOL   - number of columns in table
C     NDONAM - name of node(s) corresponding to column contents
C     NPOUT  - number of pressure output columns
C     VOL() - volume used for calculating change rate in case IDIM=3
C
      SUBROUTINE MFRTBL(itrc,IFRES,ier)

#include "building.h"
#include "net_flow.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure databases)
C ICLIM,LCLIM (for climate file)
#include "espriou.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/DISPL/IDAYF,IDAYL,IFDAY,IFTIME
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW

      PARAMETER (MCOLT=21)
      DIMENSION VOL(MCOLT)

      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      CHARACTER LAPROB*72
      CHARACTER COLHDS(MCOLT)*14
      CHARACTER ITEM(12)*23
      character PERST1*14,PERST2*44,PERST3*44,outs*124,OUTSTR*124
      character louts*248
      character NIS*12,NIE*12,WORD*20,FORMHD*29,FORMCL*29
      CHARACTER NDNAM*12
      DIMENSION  COLCNT(MCOLT),ICOLIX(MCOLT,2)
      logical OK,dok
      integer NITMS,INO ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created
      helpinsub='MFRTBL' ! set for subroutine

      IDIM=1
      VOL(1)=1.
      NCOL=1
      COLHDS(1)='# Julian date '
      FORMHD='(A14,1X,       (1X,A14)     )'
      FORMCL='(3X,F8.4,4X,   (2X,G11.4,2X))'
      WRITE(FORMHD(13:15),1000) MCOLT
      WRITE(FORMCL(13:15),1000) MCOLT-1
 1000 FORMAT(I3)

C Enable output from previous run; i.e. find IFYD,ILYD, and
C number of simulation time steps per hour and day
      CALL MFDAYS(IFRES,NNOD,NCNN,IFYD,ILYD,IER)
      IF(IER.NE.0)then
        call edisp(iuout,' Problem sorting out simulation period.')
        GOTO 100
      endif

      CALL MFTSPH(IFRES,NNOD,NCNN,NTSPH,IER)
      IF(IER.NE.0)then
        call edisp(iuout,' Problem sorting out simulation timesteps.')
        GOTO 100
      endif
      NTSPD=24*NTSPH

C Set output period (IDAYF up to IDAYL) to IFYD and ILYD if not
C yet set. 
      if(IDAYF.eq.0.or.IDAYL.eq.0)then
        IDAYF=IFYD
        IDAYL=ILYD
      endif

C Set up menu.
   10 INO=-2
      ITEM(1) ='p Analysis period      '
      ITEM(2) ='u Reporting units:     '
      ITEM(3) ='  ___________________  '
      ITEM(4) ='a Select nodes         '
      ITEM(5) ='b Select connections   '
      ITEM(6) ='c Clear selections     '
      ITEM(7) ='d Show selection status'
      ITEM(8) ='  ___________________  '
      ITEM(9) ='! List selections      '
      ITEM(10)='  ___________________  '
      ITEM(11)='? Help                 '
      ITEM(12)='- Exit                 '
      NITMS=12

C Help for this menu.
      helptopic='mfs_raw_table_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)
  
   12 CALL EMENU('Reporting (raw)',ITEM,NITMS,INO)

      IF(INO.EQ.1)THEN

C Change output start and stop dates, to allow output
C for less than entire simulation period
C Display format for date is Fri 10 Jan. 
   60   IVBTIM=1
        NTSPH=1
        IVETIM=NTSPH*24
        CALL EPERSTR(IRY,IFYD,IVBTIM,ILYD,IVETIM,NTSPH,
     &           IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)

        WRITE(outs,'(2A)')' Results available: ',PERST2
        call edisp(iuout,outs)

        CALL EPERSTR(IRY,IDAYF,IVBTIM,IDAYL,IVETIM,NTSPH,
     &           IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
        WRITE(outs,'(2A)')' Current output   : ',PERST2
        call edisp(iuout,outs)

        dok=.false.
        helptopic='mfs_table_period_change'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL ASKOK(PERST3,' Different output period? ',OK,dok,nbhelp)
        if(OK)then
          CALL EASKPER('Output period:',IDAYF,IDAYL,IFDAY,IER)
          IF(IER.NE.0) GOTO 60
        endif
      ELSEIF(INO.EQ.2)THEN

C Change flow results output units
   70   helptopic='mfs_table_express_flow'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABC(' Express flow results as: ',' ',
     &   'kg/s','volume as m^3/hr','volume as ac/h',IDIM,nbhelp)
        IF(IER.NE.0) GOTO 70
      ELSEIF(INO.EQ.4)THEN

C Add one or more nodes to the table.
   40   IF(NCOL.EQ.MCOLT) GOTO 10
        CALL ASKRNOD(' current nodes','-',INOD,IER)
        IF(IER.NE.0) GOTO 40
        if(INOD.eq.0) goto 10
        NCOL=NCOL+1
        ICOLIX(NCOL,1)=1
        ICOLIX(NCOL,2)=INOD
        write(COLHDS(NCOL),'(2X,A12)') NDNAM(INOD)

C Node ok, inform user of it`s selection.
        write(outs,'(1X,A,A)') NDNAM(INOD),' added to the display list.'
        call edisp(iuout,outs)

C Verify the node volume if volume flow set.
        if(IDIM.EQ.3)then
          helptopic='mfs_table_current_vol'
          call gethelptext(helpinsub,helptopic,nbhelp)
          VOLUME=SUPNOD(INOD,2)
          write(outs,'(A,A12,A,F8.2,A)')' The current volume of ',
     &                 NDNAM(INOD),' is ',VOLUME,'m^3'
          CALL EASKR(VOLUME,' ',
     &     ' Volume (m^3) in evaluating fluid change rates/hour? ',
     &     0.0,'F',0.,'-',1.,'volume per hour',IER,nbhelp)
          VOL(NCOL)=VOLUME
        else
          VOL(NCOL)=1.0
        endif
        goto 40
      ELSEIF(INO.EQ.5)THEN

C Add one or more connections to the table
   50   IF(NCOL.EQ.MCOLT) GOTO 10
        call ASKRCON('  connections','-',IS,IE,ICNN,IER)
        if(IER.NE.0)goto 50
        if(ICNN.eq.0)goto 10 
        NCOL=NCOL+1
        ICOLIX(NCOL,1)=2
        ICOLIX(NCOL,2)=ICNN
        write(COLHDS(NCOL),'(1X,A6,A1,A6)')
     &NDNAM(IS)(1:6),'-',NDNAM(IE)(1:6)

        write(outs,'(1X,A12,A12,A)') NDNAM(IS),NDNAM(IE),
     &                           ' added to the display list.'
        call edisp(iuout,outs)
        IF(IDIM.EQ.3) THEN
          NIS=NDNAM(IS)
          NIE=NDNAM(IE)
          helptopic='mfs_table_vol_rate'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKAB(' For volume/hr flow use the',
     &    ' volume of which node: ',NIS,NIE,IW,nbhelp)
          if(IW.eq.1)then
            VOL(NCOL)=SUPNOD(IS,2)
          elseif(IW.eq.2)then
            VOL(NCOL)=SUPNOD(IE,2)
          endif
        else
          VOL(NCOL)=1.0
        endif
        GOTO 50
      ELSEIF(INO.EQ.6)THEN
        NCOL=1
      ELSEIF(INO.EQ.7)THEN

C Show status.
        call edisp(iuout,' Current files: ')
        WRITE(outs,'(A,A)') ' Problem : ',LAPROB(1:LNBLNK(LAPROB))
        call edisp(iuout,outs)
        WRITE(outs,'(A,A)') ' Cp-wind : ',LAPRES(1:LNBLNK(LAPRES))
        call edisp(iuout,outs)
        WRITE(louts,'(A,A)') ' Climate : ',LCLIM(1:LNBLNK(LCLIM))
        call edisp248(iuout,louts,100)
        WRITE(outs,'(A,A)') ' Results : ',LAFRES(1:LNBLNK(LAFRES))
        call edisp(iuout,outs)
        call edisp(iuout,' ')
        IF(IDIM.EQ.1) THEN
          WRITE(outs,10302) ' kg/s ',NCOL,MCOLT
        ELSE IF(IDIM.EQ.2) THEN
          WRITE(outs,10302) 'm^3/hr',NCOL,MCOLT
        ELSE
          WRITE(outs,10302) ' 1/hr ',NCOL,MCOLT
        END IF
10302   FORMAT(' Table status: pressure in Pa; flow in ',A6,';',I3,
     &         ' columns defined (max. =',I3,')')
        call edisp(iuout,outs)
        DO 32 ICOL=2,NCOL
          WRITE(outs,'(A,I2,A,A)') ' Column ',ICOL,': ',COLHDS(ICOL)
          call edisp(iuout,outs)
          IF(IDIM.EQ.3) THEN
            WRITE(outs,'(A,F10.2)') ' Ref. volume = ',VOL(ICOL)
            call edisp(iuout,outs)
          endif
   32   CONTINUE
      ELSEIF(INO.EQ.9)THEN
        GOTO 90
      ELSEIF(INO.EQ.11)THEN

C Explain option
        helptopic='mfs_raw_table_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('tabular facilities',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.12)THEN
        GOTO 100
      ELSE
        INO=-1
        GOTO 12
      ENDIF
      GOTO 10


C Flush current table through IUOUT or to a file
   90 itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call edisp(iuout,' Output will be directed to file... ')
      endif

C First general table info and column headers
      call edisp(itru,'# MFRTBL: current files:')
      WRITE(outs,*) '# Problem : ',LAPROB(1:LNBLNK(LAPROB))
      call edisp(itru,outs)
      WRITE(outs,*) '# Cp-wind : ',LAPRES(1:LNBLNK(LAPRES))
      call edisp(itru,outs)
      WRITE(louts,*) '# Climate : ',LCLIM(1:LNBLNK(LCLIM))
      call edisp248(itru,louts,100)
      WRITE(outs,*) '# Results : ',LAFRES(1:LNBLNK(LAFRES))
      call edisp(itru,outs)
      IF(IDIM.EQ.1) THEN
         WRITE(outs,10902) ' kg/s '
      ELSE IF(IDIM.EQ.2) THEN
         WRITE(outs,10902) 'm^3/hr'
      ELSE
         WRITE(outs,10902) ' 1/hr '
      END IF
      call edisp(itru,outs)
10902 FORMAT('# Table: nodes pres in Pa; connection flow rates in ',A6)
      WRITE(outs,FORMHD) (COLHDS(ICOL),ICOL=1,NCOL)
      call edisp(itru,outs)

C Position read-pointer at first record of simulation day IDAYF
      CALL MFSKIP(IFRES,NNOD,NCNN,IFYD,IDAYF,IER)
      if(IER.NE.0)then
        call edisp(iuout,' Problem positioning point in results file.')
        GOTO 100
      endif

C Start output loop
      DO 99 IDAY=IDAYF,IDAYL
      DO 99 IH=1,NTSPD

C First read hour value of this time step.
      CALL STRIPC(IFRES,OUTSTR,0,ND,1,'time line',IERL)
      K=18
      CALL EGETWR(OUTSTR,K,TIME,0.,0.,'-','time',IERL)

C Start reading node fluid types, temperatures and pressures;
C first skip header
      CALL STRIPC(IFRES,OUTSTR,0,ND,1,'node header',IERL)

C Read results node by node into a string.
C Parse this string and recapture fluid type, temperature, pressure.
      DO 92 INOD=1,NNOD
        CALL STRIPC(IFRES,OUTSTR,0,ND,1,'node data',IERL)
        K=0
        IERL=0

        CALL EGETW(OUTSTR,K,WORD,'-','node',IERL)
        CALL EGETWI(OUTSTR,K,IV1,1,2,'-','fluid',IERL)
        CALL EGETW(OUTSTR,K,WORD,'-','??',IERL)
        CALL EGETWR(OUTSTR,K,RV1,0.,0.,'-','temp',IERL)
        CALL EGETW(OUTSTR,K,WORD,'-','??',IERL)
        CALL EGETWR(OUTSTR,K,RV2,0.,0.,'-','pressure',IERL)
        NDFLD(INOD)=IV1
        TNOD(INOD)=RV1
        PRES(INOD)=RV2
        IF(IERL.NE.0)then
          call edisp(iuout,' Problem reading node data.')
          GOTO 100
        endif
   92 CONTINUE

C Start reading connection mass flow rates; first skip header.
      CALL STRIPC(IFRES,OUTSTR,0,ND,1,'header',IERL)

C then read results connection by connection into a string
      DO 94 ICNN=1,NCNN
        CALL STRIPC(IFRES,OUTSTR,0,ND,1,'connection data',IERL)
        K=0
        IERL=0

C Parse this string and recapture flow 1 & 2 via RV1 & RV2
C since they are double precision and egetwr only deals
C with reals.
        CALL EGETW(OUTSTR,K,WORD,'-','mfr conn w1',IERL)
        CALL EGETW(OUTSTR,K,WORD,'-','mfr conn w2',IERL)
        CALL EGETW(OUTSTR,K,WORD,'-','mfr conn w3',IERL)
        CALL EGETW(OUTSTR,K,WORD,'-','mfr conn w4',IERL)
        CALL EGETW(OUTSTR,K,WORD,'-','mfr conn w5',IERL)
        CALL EGETW(OUTSTR,K,WORD,'-','mfr conn w6',IERL)
        CALL EGETW(OUTSTR,K,WORD,'-','mfr conn w7',IERL)
        CALL EGETWR(OUTSTR,K,RV1,0.,0.,'-','flow 1',IERL)
        CALL EGETWR(OUTSTR,K,RV2,0.,0.,'-','flow 2',IERL)
        FLW1(ICNN)=RV1
        FLW2(ICNN)=RV2
        IF(IERL.NE.0)then
          call edisp(iuout,' Problem reading connection data.')
          GOTO 100
        endif
   94 CONTINUE

C Fill table row for current time step; start with Julian date.
C Then derive pressure at each node or flow rate at each connection.
      COLCNT(1)=IDAY+IH/(FLOAT(NTSPD))

      DO 96 ICOL=2,NCOL
      IF(ICOLIX(ICOL,1).EQ.1) THEN

C Node pressure data.
         COLCNT(ICOL)=real(PRES(ICOLIX(ICOL,2)))
      ELSEIF(ICOLIX(ICOL,1).EQ.2) THEN

C Connection flow.
        IF(IDIM.EQ.1) THEN

C Mass flow rate   (kg/s).
          COLCNT(ICOL)=real(FLW1(ICOLIX(ICOL,2))+FLW2(ICOLIX(ICOL,2)))
        ELSE

C Volume flow rate (m^3/hr or 1/hr).
          ICNN=ICOLIX(ICOL,2)
          IDPS=NODPS(ICNN)
          IDNE=NODNE(ICNN)
          IF(FLW1(ICNN).GE.0.) THEN
            FLOW=real(FLW1(ICNN))*3600./DENSIT(NDFLD(IDPS),TNOD(IDPS))
          ELSE
            FLOW=real(FLW1(ICNN))*3600./DENSIT(NDFLD(IDNE),TNOD(IDNE))
          END IF
          IF(FLW2(ICNN).GE.0.) THEN
            FLOW=FLOW + real(FLW2(ICNN))*3600./
     &        DENSIT(NDFLD(IDPS),TNOD(IDPS))
          ELSE
            FLOW=FLOW + real(FLW2(ICNN))*3600./
     &         DENSIT(NDFLD(IDNE),TNOD(IDNE))
          END IF
          COLCNT(ICOL)=FLOW/VOL(ICOL)
        END IF
      END IF
   96 CONTINUE

C Flush this line of the table
      WRITE(outs,FORMCL) (COLCNT(ICOL), ICOL=1,NCOL)
      call edisp(itru,outs)

C End of time step loop
   99 CONTINUE

C Output a 'grtool' demarcation marker to indicate end of current 'set'
      call edisp(itru,'&')
      call edisp(iuout,' Data writing finished.')
      GOTO 10

C Free output file if necessary and return to calling module
  100 RETURN

      END
