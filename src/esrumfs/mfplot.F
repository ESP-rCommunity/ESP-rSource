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

C Following source code used with ASCII version of network
C flow results plotting/graphing. Not called by versons
C after November 2002.

C *********** MFPLOT
C Graphical output of mass flow results; local variables
C     FLIN   - total flow entering node
C     FLUT   - total flow leaving node
C     FLWSIN - total flow entering node in each time step
C     FLWSUT - total flow leaving node in each time step
C     IDIM   - flow: 10^-3.kg/s (IDIM=1), m^3/hr (IDIM=2), 1/hr (IDIM=3)
C     MPTS   - maximum number of time steps in one plot
C     NDONUM - number of node for graphical output
C     NDCNUM - number of coupled node
C     NTSPD  - number of simulation time steps per day
C     NTSPH  - number of simulation time steps per hour
C     VOL() -  volume used for calculating change rate in case IDIM=3

      SUBROUTINE MFPLOT(itrc,itru,IFRES,ier)
#include "building.h"
#include "espriou.h"
C espriou.h provides currentfile.
#include "net_flow.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      integer ifs,itfs,imfs
      COMMON/GFONT/IFS,ITFS,IMFS
      common/pophelp/h(60)
      COMMON/DISPL/IDAYF,IDAYL,IFDAY,IFTIME
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW

      PARAMETER       (MPTS=2400)

C Currently only one node plotted.
      PARAMETER       (MGNDO=1)
      DIMENSION        VOL(MGNDO)

      CHARACTER*24      ITEM(10)
      DOUBLE PRECISION FLIN,FLUT
      DIMENSION        FLWSIN(MPTS),FLWSUT(MPTS)
      CHARACTER LABEL1*10,LABEL2*10
      CHARACTER ETEXT*72,temp*28,WORD*20,OUTSTR*124
      character H*72,outs*124,nterm*1,NDNAM*12
      character PERST1*14,PERST2*44,PERST3*44
      character LAPROB*72
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      logical OK,DOK

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
#ifdef OSX
      integer igwid,igheight  ! for use with axiscale
#else
      integer*8 igwid,igheight  ! for use with axiscale
#endif

C In order to pass strings across to C it is necessary to first
C do an internal write placing an end marker which C can understand.
      nterm=CHAR(0)

      IDIM=1
      NDONUM=0
      NLINES=0
      VOL(1)=1.
      LABEL1='10^-3.kg/s'
      LABEL2='Timesteps '

C Enable output from previous run; i.e. find IFYD,ILYD, and
C number of simulation time steps per hour and day
      CALL MFDAYS(IFRES,NNOD,NCNN,IFYD,ILYD,IER)
      IF(IER.NE.0)then
        call edisp(iuout,' Unable to get simulation period!')
        GOTO 100
      endif
      CALL MFTSPH(IFRES,NNOD,NCNN,NTSPH,IER)
      IF(IER.NE.0)then
        call edisp(iuout,' Unable to get time steps!')
        GOTO 100
      endif
      NTSPD=24*NTSPH

C Set output period (IDAYF up to IDAYL) to IFYD and ILYD if not
C yet set. 
      if(IDAYF.eq.0.or.IDAYL.eq.0)then
        IDAYF=IFYD
        IDAYL=ILYD
      endif

C Set day and month numbers for simulation period.
      CALL EDAYR(IDAYF,ISDF,ISMF)
      CALL EDAYR(IDAYL,ISDL,ISML)
      write(currentfile,'(a)') LAFRES(1:lnblnk(LAFRES))

C Initialize output period day and month numbers.
      IODF=ISDF
      IOMF=ISMF
      IODL=ISDL
      IOML=ISML

C Let the user pick a menu item.
   10 INO=-2
      ITEM(1) ='p Analysis period      '
      ITEM(2) ='u Reporting units:     '
      ITEM(3) ='  ___________________  '
      ITEM(4) ='s Select nodes         '
      ITEM(5) ='d Draw graph           '
      ITEM(6) ='c Clear nodes          '
      ITEM(7) ='  ___________________  '
      ITEM(8) ='? Help                 '
      ITEM(9) ='- Exit to main menu    '

      NITMS=9

C Help for this menu.
   12 H(1)=' This module allows you to define or create a plot of'
      H(2)=' of node coupled fluid mass flow results.'
      H(3)=' The `1 nodes` must be used to define the node for '
      H(4)=' output. `p Period` may be used to restrict the'
      H(5)=' output period, and `u Units` to change expression '
      H(6)=' of flow rate results.'
      H(7)=' The graph is plotted through the `d Draw` command, '
      H(8)=' and may be re-defined after it has been `c Cleared`.'

      CALL EMENU('graph',ITEM,NITMS,INO)

      IF(INO.EQ.1)then

C Define the beginning and ending dates for simulation.
C Display format for date is Fri 10 Jan. 
   60   IVBTIM=1
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

        dok=.false.
        h(1)='Option to specify a different period to display. '
        CALL ASKOK(PERST3,' Different output period?',OK,dok,1)
        if(OK)then
          CALL EASKPER('Output period:',IDAYF,IDAYL,IFDAY,IER)
          IF(IER.NE.0) GOTO 60
        endif
        call usrmsg(' ',' ','-')
      ELSEIF(INO.EQ.2)then

C Get flow results output dimension and set the Y-axis label.
   40   H(1)='Note that volume flow rates use the volumes associated'
        H(2)='with each node. '
        CALL EASKABC(' Express flow results as: ',' ',
     &   'kg/s','volume as m^3/hr','volume as ac/h',IDIM,2)
        IF(IER.NE.0) GOTO 40
        IF(IDIM.EQ.1) LABEL1='10^-3.kg/s'
        IF(IDIM.EQ.2) LABEL1='m^3/hr    '
        IF(IDIM.EQ.3) LABEL1='1/hr'
      ELSEIF(INO.EQ.4)then

C Set number and name of node for output.
        CALL ASKRNOD(' node to plot','-',NDONUM,IER)
        IF(NDONUM.EQ.0)GOTO 10
        NLINES=1
        IF(IDIM.EQ.3) THEN
          H(1)='This should be the same as that of the associated'
          H(2)='zone in the bps simulation. '
          VOLUME=SUPNOD(NDONUM,2)
          write(outs,'(A,A12,A,F8.2,A)')' The current volume of ',
     &                 NDNAM(NDONUM),' is ',VOLUME,'m^3'
          CALL EASKR(VOLUME,' ',
     &     ' Volume (m^3) in evaluating fluid change rates/hour? ',
     &     0.0,'F',0.,'-',1.,'volume per hour',IER,2)
          VOL(NLINES)=VOLUME
        ELSE
          VOL(NLINES)=1.0
        END IF

C Set number and name of coupled node.
   25   CALL EASKAB(' ',' Display flow via:',
     &            'all associated nodes','between one node',IW,0)
        if(IW.eq.1)then
          NDCNUM=0
          GOTO 10
        else
          CALL ASKRNOD(' other node','-',NDCNUM,IER)
          IF(NDCNUM.EQ.0.OR.(NDCNUM.EQ.NDONUM))then
            call usrmsg(' ',' Pick another node!','W')
            GOTO 25
          endif
        endif
      ELSEIF(INO.EQ.5)then
        GOTO 50
      ELSEIF(INO.EQ.6)then
        NDONUM=0
      ELSEIF(INO.EQ.8)then

C Explain option.
        CALL PHELPD('mfs plotting',8,'-',0,0,IER)
      ELSEIF(INO.EQ.9)then
        GOTO 100
      ELSE

C Wrong menu pick.
        INO=-1
        GOTO 12
      ENDIF
      goto 10

C Set up data for graph drawing.
   50 IF(NDONUM.EQ.0)then
        call edisp(iuout,' Must have a node to plot!')
        GOTO 10
      endif

C Refresh the drawing area.
      IF(MMOD.EQ.8)THEN
        LIMTTY=10
        LIMIT =10
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          CALL feedbox(menuchw,2,igfw,igfh)
          CALL opengdisp(menuchw,LIMTTY,2,igdw,igdh)
        endif
        CALL win3d(menuchw,12,8,7,3,igl,igr,igt,igb,igw,igwh)
        igw=igr-igl
        igwh=igb-igt
      ENDIF

      ICOUNT=0
      YMIN=+1.E+10
      YMAX=-1.E+10

C Position read-pointer at first record of simulation day IDAYF.
      CALL MFSKIP(IFRES,NNOD,NCNN,IFYD,IDAYF,IER)
      IF(IER.NE.0)then
        call edisp(iuout,' Problem positioning pointer in file!')
        GOTO 100
      ENDIF

C Start day part of main output loop by reading results.
      DO 59 IDAY=IDAYF,IDAYL
      write(outs,'(A,I3)')' Reading day ',IDAY
      call usrmsg(' ',outs,'-')
      DO 59 IH=1,NTSPD

C First read hour value of this time step.
      CALL STRIPC(IFRES,OUTSTR,0,ND,1,'time line',IERL)

C Start reading node fluid types, temperatures and pressures;
C first skip header
      CALL STRIPC(IFRES,OUTSTR,0,ND,1,'node header',IERL)

C Read results node by node into a string.
      DO 54 INOD=1,NNOD
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
   54 CONTINUE

C Start reading connection mass flow rates; first skip header.
      CALL STRIPC(IFRES,OUTSTR,0,ND,1,'header',IERL)

C Read results connection by connection into a string.
      DO 56 ICNN=1,NCNN
        CALL STRIPC(IFRES,OUTSTR,0,ND,1,'flow connection data',IERL)
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
   56 CONTINUE

C Determine node data.
      ICOUNT=ICOUNT+1

C Maximal MPTS data points. Try to resume operation with the
C data already available.
      if (ICOUNT.ge.MPTS) then
        call edisp(iuout,' To many data points.')
        goto 58
      endif

C Calculate fluid flow entering/leaving
C the node (10^-3.kg/s or m^3/hr).
      CALL MFSNFL(IDIM,NDONUM,NDCNUM,FLIN,FLUT)

C Assign flows; leaving flow is plotted +ve.
      FLWSIN(ICOUNT)= real(FLIN)/VOL(NLINES)
      FLWSUT(ICOUNT)= real(-FLUT)/VOL(NLINES)
      YMAX=AMAX1(YMAX,FLWSIN(ICOUNT),FLWSUT(ICOUNT))
      YMIN=AMIN1(YMIN,FLWSIN(ICOUNT),FLWSUT(ICOUNT))
   59 CONTINUE

   58   IF(NDCNUM.NE.0) THEN
        WRITE(ETEXT,10601)NDNAM(NDONUM),NDNAM(NDCNUM),
     &                    IODF,IOMF,IRY,nterm
      ELSE
        WRITE(ETEXT,10602)NDNAM(NDONUM),
     &                    IODF,IOMF,IRY,nterm
      END IF
10601 FORMAT(2X,'Node ',A12,' with node ',A12,' day ',
     &       I2,' month ',I2,' (',I4,')',A1)
10602 FORMAT(2X,'Node ',A12,' with all connections, day ',
     &       I2,' month ',I2,' (',I4,')',A1)
      CALL viewtext(ETEXT,1,0,1)

      WRITE(ETEXT,10603) IODL,IOML,IRY,nterm
10603 FORMAT(42X,'to day ',I2,' of month ',I2,' (',I4,')',A1)
      CALL viewtext(ETEXT,2,0,1)

C Draw a pairs of labels and line types at top of the graph.
      WRITE(temp,'(A,A12,A,A1)')'Node ',NDNAM(NDONUM),' leaving',nterm
      CALL wstxpt(igl+60,igt-20,temp)
      call eswline(igl+20,igt-25,igl+50,igt-25)
      WRITE(temp,'(A,A12,A,A1)')'Node ',NDNAM(NDONUM),' entering',nterm
      CALL wstxpt(igl+60,igt-40,temp)
      call edwline(igl+20,igt-45,igl+50,igt-45)

C Scale and draw axes.
      XMIN=0.
      XMAX=FLOAT(ICOUNT)/NTSPH+1.
      YMAX=YMAX+0.1
      igwid=igw
      igheight=igwh
      call axiscale(igwid,igheight,XMIN,XMAX,YMIN,YMAX,xsc,ysc,sca,
     &  Xadd,Yadd)
      CALL horaxis(XMIN,XMAX,igl,igr,igb,Xadd,xsc,1,LABEL2)
      CALL vrtaxis(YMIN,YMAX,igl,igb,igt,Yadd,ysc,0,0,LABEL1)
      call linescale(igl,Xadd,xsc,igb,Yadd,ysc)

C Establish step size (in hour) in X-direction.
      STPX=1./FLOAT(NTSPH)

C Plot line for FLWSUT, move to first postion draw it.
      call u2pixel(0.5*STPX,FLWSUT(1),ix,iy)
      call u2pixel((STPX+.5*STPX),FLWSUT(1),ix1,iy1)
      call eswline(ix,iy,ix1,iy1)
      iylast=iy

C Loop through each of the succeeding points in a stair-step pattern.
C Continue from the last flow value to the current and then horizontal
C for the current value to the end of the timestep.
      DO 62 I=1,ICOUNT
        TI=FLOAT(I)*STPX
        call u2pixel((TI-.5*STPX),FLWSUT(I),ix,iy)
        call eswline(ix,iylast,ix,iy)
        call u2pixel((TI+.5*STPX),FLWSUT(I),ix1,iy1)
        call eswline(ix,iy,ix1,iy1)
        iylast=iy
   62 CONTINUE

C Plot double thick line for FLWSIN, move to first postion draw it.
      call u2pixel((0.5*STPX),FLWSIN(1),ix,iy)
      call u2pixel((STPX+.5),FLWSIN(1),ix1,iy)
      call edwline(ix,iy,ix1,iy)
      iylast=iy

C Loop through each of the succeeding points in a stair-step pattern.
C Continue from the last flow value to the current and then horizontal
C for the current value to the end of the timestep.
      DO 64 I=1,ICOUNT
        TI=FLOAT(I)*STPX
        call u2pixel((TI-.5*STPX),FLWSIN(I),ix,iy)
        call edwline(ix,iylast,ix,iy)
        call u2pixel((TI+.5*STPX),FLWSIN(I),ix1,iy)
        call edwline(ix,iy,ix1,iy)
        iylast=iy
   64 CONTINUE

C Return to menu.
      GOTO 10

  100 RETURN
      END

