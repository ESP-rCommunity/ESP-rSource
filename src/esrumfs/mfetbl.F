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

C *************** MFETBL
C Raw tabular output of energy in a flow stream. Results via stderr 
C channel; local variables:
C     DATTIM - Julian date; where Jan 1th 1:00 = 1.0
C     IOCHAN  - output channel: =IUOUT (via stdout) or =21 (to a file)
C     IDIM   - flow units: kg/s (IDIM=1), m^3/hr (IDIM=2), 1/hr (IDIM=3)
C     NDONAM - name of node(s) corresponding to column contents
      SUBROUTINE MFETBL(itrc,IFRES,ier)

      INCLUDE 'building.h'
      INCLUDE 'net_flow.h'
      INCLUDE 'esprdbfile.h'
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure databases)
C ICLIM,LCLIM (for climate file)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      common/pophelp/h(60)
      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/DISPL/IDAYF,IDAYL,IFDAY,IFTIME
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/EXPORTI/ixopen,ixunit,ixpunit

      DOUBLE PRECISION SFLW1,SFLW2

      CHARACTER H*72,LAPROB*72
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      CHARACTER*23     ITEM(10)
      character PERST1*14,PERST2*44,PERST3*44,outs*124,OUTSTR*124
      character WORD*20,NDNAM*12,NR*12,NT*12
      logical OK,DOK,intg

C Integration initially off.
      intg=.false.

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
      ITEM(2) ='  __________________   '
      ITEM(3) ='a Select flow stream   '
      ITEM(4) ='! List performance     '
      ITEM(5) ='c Clear                '
      ITEM(6) ='  ___________________  '
      ITEM(7) ='? Help                 '
      ITEM(8) ='- Exit                 '
      NITMS=8

C Help text for this menu.
      h(1)='The energy transfer associated with flow between two'
      h(2)='nodes (not necessarily adjacent, but part of the same'
      h(3)='flow path) is recovered and displayed. '
      h(4)=' '
      h(5)='This requires: '
      h(6)=' * a reference node (perhaps a boundary node)'
      h(7)=' * a test node (somewhere along a flow path)'
      h(8)=' * a connection wich includes the test node (to'
      h(9)='   find the flow rate)'
     
   12 CALL EMENU('Flow stream energy',ITEM,NITMS,INO)

      IF(INO.EQ.1)THEN

C Change output start and stop dates, to allow output
C for less than entire simulation period
C Display format for date is Fri 10 Jan. 
   60   IVBTIM=1
        INTSPH=1
        IVETIM=INTSPH*24
        CALL EPERSTR(IRY,IFYD,IVBTIM,ILYD,IVETIM,INTSPH,
     &           IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)

        WRITE(outs,'(A,A)')' Results available: ',PERST2
        call edisp(iuout,outs)

        CALL EPERSTR(IRY,IDAYF,IVBTIM,IDAYL,IVETIM,INTSPH,
     &           IFDAY,IFTIME,PERST1,PERST2,PERST3,IER)
        WRITE(outs,'(A,A)')' Current output   : ',PERST2
        call edisp(iuout,outs)

        dok=.false.
        h(1)='Chance to display a different period. '
        CALL ASKOK(PERST3,' Different output period?',OK,dok,1)
        if(OK)then
          CALL EASKPER('Output period:',IDAYF,IDAYL,IFDAY,IER)
          IF(IER.NE.0) GOTO 60
        endif
        if(NTSPH.gt.1)then
          dok=.false.
          h(1)='The simulation was run at more than one timestep per'
          h(2)='hour. You can integrate the data (i.e. to once per'
          h(3)='hour) to remove clutter and see overall patterns. '
          CALL ASKOK(PERST3,' Integrate over each hour?',OK,dok,3)
          if(OK)then
            intg=.true.
            NOUT=NTSPH
            IAV=1
            call edisp(iuout,'Ouput reset to hourly averaged report.')
          endif
        endif
      ELSEIF(INO.EQ.3)THEN

C Define the reference and test node and flow stream.
        CALL PHELPD('flow path',9,'-',0,0,IER)
        call edisp(iuout,' Please select the reference node...')
   40   CALL ASKRNOD(' reference node','-',IRNOD,IER)
        IF(IER.NE.0.or.IRNOD.eq.0) GOTO 40
        NR=NDNAM(IRNOD)

C Node ok, inform user of it`s selection.
        write(outs,'(1X,A,A)') NR,' is the reference node.'
        call edisp(iuout,outs)
        call edisp(iuout,' Please select the test node...')
   41   CALL ASKRNOD(' test node','-',ITNOD,IER)
        IF(IER.NE.0.or.ITNOD.eq.0) GOTO 41
        NT=NDNAM(ITNOD)

C Node ok, inform user of it`s selection.
        write(outs,'(1X,A,A)') NDNAM(ITNOD),' is the test node.'
        call edisp(iuout,outs)
        call edisp(iuout,' Please select the flow path...')

  50    call ASKRCON(' flow path','-',IS,IE,IPCNN,IER)
        if(IER.NE.0)goto 50
        if(IPCNN.eq.0)goto 10 

        write(outs,'(1X,A,A,A)')NDNAM(IS),NDNAM(IE),' is the flow path.'
        call edisp(iuout,outs)
      ELSEIF(INO.EQ.4)THEN

C List output.
        GOTO 90
      ELSEIF(INO.EQ.5)THEN
        IRNOD=0
        ITNOD=0
        ITNOD=0
      ELSEIF(INO.EQ.7)THEN

C Explain option.
        CALL PHELPD('energy in flow',12,'-',0,0,IER)
      ELSEIF(INO.EQ.8)THEN

C Exit.
        GOTO 100
      ELSE
        INO=-1
        GOTO 12
      ENDIF
      GOTO 10

C If output to file alter the edisp unit number.
   90 itru = icout
      if(ixopen.eq.1)then
        itru = ixunit
        call edisp(iuout,' Output will be directed to file... ')
      endif

C First general table info and column headers
      call edisp(itru,'# MFETBL: current files:')
      WRITE(outs,*) '# Problem : ',LAPROB(1:LNBLNK(LAPROB))
      call edisp(itru,outs)
      WRITE(outs,*) '# Cp-wind : ',LAPRES(1:LNBLNK(LAPRES))
      call edisp(itru,outs)
      WRITE(outs,*) '# Climate : ',LCLIM(1:LNBLNK(LCLIM))
      call edisp(itru,outs)
      WRITE(outs,*) '# Results : ',LAFRES(1:LNBLNK(LAFRES))
      call edisp(itru,outs)
      write(outs,'(a,a,1x,a,a)') '# Julian date, Temp @ ',NR,NT,
     &  'Delta T, Flow(kg/s), Watts'
      call edisp(itru,outs)

C Position read-pointer at first record of simulation day IDAYF
      CALL MFSKIP(IFRES,NNOD,NCNN,IFYD,IDAYF,IER)
      if(IER.NE.0)then
        call edisp(iuout,' Problem positioning point in results file.')
        GOTO 100
      endif

C Start output loop
      DO 99 IDAY=IDAYF,IDAYL
        ix=0
        XTRNOD=0.
        XTTNOD=0.
        XDT=0.
        XRATEKG=0.
        XPW=0.
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
          if(inod.eq.IRNOD.or.inod.eq.ITNOD)then
            K=0
            IERL=0
            CALL EGETW(OUTSTR,K,WORD,'-','node',IERL)
            CALL EGETWI(OUTSTR,K,IV1,1,2,'-','fluid',IERL)
            CALL EGETWI(OUTSTR,K,IV2,0,3,'-','type',IERL)
            CALL EGETWR(OUTSTR,K,RV1,0.,0.,'-','temp',IERL)
            CALL EGETWR(OUTSTR,K,RVH,0.,0.,'-','height',IERL)
            CALL EGETWR(OUTSTR,K,RV2,0.,0.,'-','pressure',IERL)
            if(inod.eq.IRNOD)then
             irnfld=IV1
             TRNOD=RV1
            endif
            if(inod.eq.ITNOD)then
             itnfld=IV1
             TTNOD=RV1
            endif
          endif
          if(IERL.NE.0)then
            call edisp(iuout,' Problem reading node data.')
            GOTO 10
          endif
   92   CONTINUE

C Start reading connection mass flow rates; first skip header.
        CALL STRIPC(IFRES,OUTSTR,0,ND,1,'header',IERL)

C then read results connection by connection into a string
        DO 94 ICNN=1,NCNN
          CALL STRIPC(IFRES,OUTSTR,0,ND,1,'connection data',IERL)

C Parse this string and recapture flow 1 & 2 via RV1 & RV2.
          if(ICNN.eq.IPCNN)then
            K=0
            IERL=0
            CALL EGETW(OUTSTR,K,WORD,'-','mfr conn w1',IERL)
            CALL EGETW(OUTSTR,K,WORD,'-','mfr conn w2',IERL)
            CALL EGETW(OUTSTR,K,WORD,'-','mfr conn w3',IERL)
            CALL EGETWI(OUTSTR,K,IV1,0,0,'-','conn type',IERL)
            CALL EGETWI(OUTSTR,K,IV2,1,2,'-','conn fluid',IERL)
            CALL EGETWR(OUTSTR,K,PIJ,0.,0.,'-','conn pij',IERL)
            CALL EGETWR(OUTSTR,K,PST,0.,0.,'-','conn pstk',IERL)
            CALL EGETWR(OUTSTR,K,RV1,0.,0.,'-','flow 1',IERL)
            CALL EGETWR(OUTSTR,K,RV2,0.,0.,'-','flow 2',IERL)
            SFLW1=RV1
            SFLW2=RV2
          endif
          IF(IERL.NE.0)then
            call edisp(iuout,' Problem reading connection data.')
            GOTO 100
          endif
   94   CONTINUE

C Fill table row for current time step; start with Julian date.
        PERIOD=IDAY+IH/(FLOAT(NTSPD))

C Connection flow rate (kg/s).
        RATEKG=real(SFLW1+SFLW2)

C Calculate energy via flow path.
        DT=TTNOD-TRNOD
        if(itnfld.eq.1)then
          PW=DT*real(SFLW1+SFLW2)*1006.
        elseif(itnfld.eq.2)then
          PW=DT*real(SFLW1+SFLW2)*4600.
        endif

C Write data.  Increment ix, if equal to NOUT print.
        if(intg)then
          ix=ix+1
          if(ix.le.NOUT)then
            XTRNOD=XTRNOD+TRNOD
            XTTNOD=XTTNOD+TTNOD
            XDT=XDT+DT
            XRATEKG=XRATEKG+RATEKG
            XPW=XPW+PW
            if(ix.eq.NOUT)then
              XTRNOD=XTRNOD/NOUT
              XTTNOD=XTTNOD/NOUT
              XDT=XDT/NOUT
              XRATEKG=XRATEKG/NOUT
              XPW=XPW/NOUT

C Adjust time reporting to the half hour.
              XPERIOD=PERIOD-0.0208333
              WRITE(outs,'(F10.5,3F8.2,F12.5,F12.1)')XPERIOD,XTRNOD,
     &          XTTNOD,XDT,XRATEKG,XPW
              call eddisp(itru,outs)

C Reset.
              XTRNOD=0.
              XTTNOD=0.
              XDT=0.
              XRATEKG=0.
              XPW=0.
              ix=0
            endif
          endif
        else
          WRITE(outs,'(F10.5,3F8.2,F12.5,F12.1)')PERIOD,TRNOD,TTNOD,DT,
     &      RATEKG,PW
          call eddisp(itru,outs)
        endif

C End of time step loop
   99 CONTINUE

C Output a 'grtool' demarcation marker to indicate end of current 'set'
      call edisp(itru,'&')
      call edisp(iuout,' Data output complete.')
      GOTO 10

  100 RETURN

      END
