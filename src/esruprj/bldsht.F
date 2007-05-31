C Blind control file: creation, editing, reading & writing.
C << not referenced in any module - common blocks can be removed >>
      SUBROUTINE  BLDFMK(ITRC,ICOMP,IER)
      INCLUDE 'building.h'
      common/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      COMMON/GOPT/DG(5),HG(5),UVAL,VTRN,NTL,AB(ME,5),RF(ME),SRF,SAB
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      COMMON/FOPENED/CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK
      COMMON/B2/NBC(MCOM),IBCS(MCOM,MBP),IBCF(MCOM,MBP),BT(MCOM,MBP,5),
     &          BH(MCOM,MBP,5),BU(MCOM,MBP),IBCWIN(MCOM),BV(MCOM,MBP)
      COMMON/B3/NBCT(MCOM,MBP),BACTP(MCOM,MBP)
      COMMON/BLNDF1/BDOPT(MBP)
      common/CFGV/icfgv

      LOGICAL XST,CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK

      dimension ibc(MCOM)
      character zname*12,zdesc*64,outs*124,LTMP*72,UFILE*72,BFILE*72
      character*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      character SOPT*12,DOPT*12,KEY*1,GDESCR*36
      character*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      character BDOPT*12,H*72
      character*42 ITEM(20)

C Check to see that optical properties are available.
      IUF=IFIL+1
      IF(.NOT.OPTKOK)THEN
        SOPT='ALL'
        CALL EROPTDB(0,iuout,SOPT,GDESCR,IER)
        OPTKOK=.TRUE.
      ENDIF

C Check and see if a blind control and utility file exists.
      WRITE(UFILE,'(A,A4)')zname(ICOMP)(1:lnblnk(zname(ICOMP))),'.utl'
      WRITE(BFILE,'(A,A4)')zname(ICOMP)(1:lnblnk(zname(ICOMP))),'.bls'
      if(IBC(ICOMP).NE.0)then
        continue
      else

C Set up default blind control data based on clear glass with internal
C blind.
        DOPT='SC_8985_04ib'
        SOPT=DOPT
        BDOPT(1)=DOPT
        NBC(ICOMP)=1
        IBCS(ICOMP,1)=8
        IBCF(ICOMP,1)=18
        NBCT(ICOMP,1)=0
        BACTP(ICOMP,1)=300.
        CALL EROPTDB(ITRC,iuout,SOPT,GDESCR,IER)
        BU(ICOMP,1)=UVAL
        BV(ICOMP,1)=VTRN
        BT(ICOMP,1,1)=DG(1)
        BT(ICOMP,1,2)=DG(2)
        BT(ICOMP,1,3)=DG(3)
        BT(ICOMP,1,4)=DG(4)
        BT(ICOMP,1,5)=DG(5)
        BH(ICOMP,1,1)=HG(1)
        BH(ICOMP,1,2)=HG(2)
        BH(ICOMP,1,3)=HG(3)
        BH(ICOMP,1,4)=HG(4)
        BH(ICOMP,1,5)=HG(5)
        NBCT(ICOMP,1)=0
        BACTP(ICOMP,1)=300.

        H(1)='The zone blind shutter control file allows default'
        H(2)='windows to change optical and thermophysical pro-'
        H(3)='perties as a function of time, solar intensity, or'
        H(4)='ambient temperature.  It is normally named ??.bls.'
        LTMP=BFILE
  246   CALL EASKS(LTMP,' Zone blind shutter control file ?',
     &    ' ',72,BFILE,'blind control file',IER,4)
        IF(IER.NE.0)GOTO 246
        IF(LTMP.NE.' ')THEN
          IBC(ICOMP)=1
        ELSE
          goto 246
        ENDIF
        if(icfgv.lt.3.and.INDUTL(ICOMP).NE.1)then
          H(1)=' This is required to support blind control data.'
          CALL EASKS(UFILE,' Related zone utility file ? ',' ',
     &       72,'zone.utl','utility',IER,1)
          INDUTL(ICOMP)=1
          LUTIL(ICOMP)=UFILE
        endif

        CALL edisp(iuout,' ')
        call edisp(iuout,'Updating problem for blind control.')
        call tstamp('>','PRJ: update configuration')
        CALL EMKCFG('-',IER)
      endif

C Begin composing the display.
      MHEAD=6
      MCTL=6

   20 write(ITEM(1),'(A,A)')  '  Zone: ',zname(ICOMP)
      if(IBCWIN(ICOMP).eq.0)then
        write(ITEM(2),'(A)') 'a Sensor position: separate control'
      else
        write(ITEM(2),'(A,I2)') 'a Sensor at surface: ',IBCWIN(ICOMP)
      endif
      write(ITEM(3),'(A,I2)') '  Number of control periods: ',NBC(ICOMP)
      ITEM(4)=            '  _______________________________________'
      ITEM(5)=            '  Per|Start|Stop |Optics     |Cntrl|Activ'
      ITEM(6)=            '     |     |     |Name       |index|point'

C Period loop. M is the current menu line index.
      M=MHEAD
      if(NBC(ICOMP).gt.0)then
        DO 10 L=1,NBC(ICOMP)
          M=M+1
          CALL EMKEY(L+1,KEY,IER)
          if(NBCT(ICOMP,L).eq.0)then
            WRITE(ITEM(M),303)KEY,L,IBCS(ICOMP,L),IBCF(ICOMP,L),
     &                      BDOPT(L),BACTP(ICOMP,L)
  303       FORMAT(A1,I4,I5,I5,2X,A12,' rad',F7.1)
          elseif(NBCT(ICOMP,L).eq.1)then
            WRITE(ITEM(M),304)KEY,L,IBCS(ICOMP,L),IBCF(ICOMP,L),
     &                      BDOPT(L),BACTP(ICOMP,L)
  304       FORMAT(A1,I4,I5,I5,2X,A12,' tmp',F7.1)
          endif
   10   CONTINUE
      endif

      ITEM(M+1)=          '  __________________________________    '
      ITEM(M+2)=          '* add/ delete control period            '
      ITEM(M+3)=          '! list blind/ shutter information       '
      ITEM(M+4)=          '> merge data into current problem       '
      ITEM(M+5)=          '? help                                  '
      ITEM(M+6)=          '- exit this menu                        '
      NITEMS=MCTL+M

C Help text for this menu.
  30  H(1)='The zone blind/ shutter control file allows default'
      H(2)='windows to change their optical and thermophysical'
      H(3)='properties as a function of time, solar intensity or'
      H(4)='ambient temperature.  It is normally named ?.bls.'
      H(5)=' '
      H(6)='Blind control takes the form of a 24hr profile which'
      H(7)='repeats throughout a simulation.'
      H(8)=' '
      H(9)='The control index dictates the sensed condition,'
      H(10)='radiation or temperature.  The actuation point,'
      H(11)='if > 0, is the set-point, if = -99 is time control.'
      H(12)=' '
      H(13)='For each profile interval, a set of optical'
      H(14)='properties are referenced as held within the optical'
      H(15)='properties database.  This set includes the angle'
      H(17)='dependent direct, total visable solar transmittance'
      H(18)='and the overall thermal transmittance.'

      INO = -2
      CALL EMENU('Blind Control',ITEM,NITEMS,INO)

      IF (INO.EQ.NITEMS)THEN
	RETURN
      ELSEIF(INO.EQ.NITEMS-1)THEN

C Help.
        CALL PHELPD('blind shutter',18,'-',0,0,IER)
      ELSEIF(INO.EQ.NITEMS-2)THEN

C Save data.
C        H(1)='The zone blind shutter control file allows default'
C        H(2)='windows to change their optical and thermophysical'
C        H(3)='properties as a function of time, solar intensity or'
C        H(4)='ambient temperature.  It is normally named ?.bls.'
C        LTMP=LBLIN(ICOMP)
C   46   CALL EASKS(LTMP,' Zone blind shutter control file?',
C     &    ' ',72,BFILE,'blind control file',IER,4)
C        IF(IER.NE.0)GOTO 46
C        IF(LTMP.NE.' ')THEN
C          LBLIN(ICOMP)=LTMP
C        ELSE
C          goto 46
C        ENDIF

C Write data.
C        call EMKBLDF(iuout,IUF,LBLIN(ICOMP),ICOMP,IER)
      ELSEIF(INO.EQ.NITEMS-3)THEN

C List.
        call edisp(iuout,' ')
        WRITE(outs,'(A,I3)')' Number of control periods: ',NBC(ICOMP)
        call edisp(iuout,outs)
        WRITE(outs,'(A,I3)')' Radiation sensor position: ',IBCWIN(ICOMP)
        call edisp(iuout,outs)
        DO 850 I = 1,NBC(ICOMP)
          call edisp(iuout,' ')
          call edisp(iuout,
     & ' Period|Start|Finish|Direct solar transmittance    |Visible')
          WRITE(outs,5210)  I,IBCS(ICOMP,I),IBCF(ICOMP,I),
     &                     (BT(ICOMP,I,K),K=1,5),BV(ICOMP,I)
 5210     FORMAT(I6,I7,I7,5F6.3,F7.3)
          call edisp(iuout,outs)
          call edisp(iuout,
     & '                    |Total heat gain factor        |U value')
          WRITE(outs,'(20X,5F6.3,F7.3)') (BH(ICOMP,I,K),K=1,5),
     &                                    BU(ICOMP,I)
          call edisp(iuout,outs)
          if(NBCT(ICOMP,I).eq.0)then
            WRITE(outs,'(20X,A)')'Control variable type: radiation'
          elseif(NBCT(ICOMP,I).eq.1)then
            WRITE(outs,'(20X,A)')'Control variable type: temperature'
          endif
          call edisp(iuout,outs)
          WRITE(outs,'(20X,A,F10.3)')'Actuator point',BACTP(ICOMP,I)
          call edisp(iuout,outs)
  850   CONTINUE
      ELSEIF(INO.EQ.NITEMS-4)THEN

C +- Period.
        CALL EASKAB(' ','Modify nmber of periods:',
     &    'delete one','add one @ end',IW,0)
        IF(IW.EQ.2.AND.(NBC(ICOMP)+1.LE.MP))THEN
          NBC(ICOMP) = NBC(ICOMP) + 1
          IN = NBC(ICOMP)
          IBCS(ICOMP,IN)=IBCF(ICOMP,IN-1)
          IBCF(ICOMP,IN)=IBCS(ICOMP,IN)+1
          NBCT(ICOMP,IN)=NBCT(ICOMP,IN-1)
          BACTP(ICOMP,IN)=BACTP(ICOMP,IN-1)
          CALL EDBLCTL(ITRC,ICOMP,IN,IER)
        ELSEIF(IW.EQ.1)THEN

C Delete control period.
          H(1)='Choose one of the existing periods to delete.'
          CALL EASKI(IN,' ',' Control period to delete?',
     &          0,'F',NBC(ICOMP),'F',1,'Which to delete',IER,1)
          IF (IN .EQ. NBC(ICOMP))  GOTO  790
          DO 740 I = IN+1,NBC(ICOMP)
            DO 750 K = 1,5
              BT(ICOMP,I-1,K) = BT(ICOMP,I,K)
              BH(ICOMP,I-1,K) = BH(ICOMP,I,K)
  750       CONTINUE
            BU(ICOMP,I-1) = BU(ICOMP,I)
            BV(ICOMP,I-1) = BV(ICOMP,I)
            NBCT(ICOMP,I-1)=NBCT(ICOMP,I)
            BACTP(ICOMP,I-1)=BACTP(ICOMP,I)
            BDOPT(I-1)=BDOPT(I)
  740     CONTINUE
  790     NBC(ICOMP) = NBC(ICOMP) -1
        ENDIF
      ELSEIF (INO.EQ.2)THEN

C Sensor position.
        H(1)='The sensor postion points to a zone surface, allowing'
        H(2)='blind/ shutter control on the basis of the impinging'
        H(3)='radiation intensity.  If set to 0 then control is on'
        H(4)='the basis of the solar radiation received at the'
        H(5)='surface with which the blind/ shutter is associated.'
        H(6)='If control is on temperature then the sensor location'
        H(7)='is ignored. '
        NSENS=IBCWIN(ICOMP)
        CALL EASKI(NSENS,' ',' Sensor position?',
     &          0,'F',MS,'F',1,'Sensor position',IER,7)
        IBCWIN(ICOMP)=NSENS
      ELSEIF(INO.GT.MHEAD.AND.INO.LT.(NITEMS-MCTL+1))THEN
        IN=INO-MHEAD
        CALL EDBLCTL(ITRC,ICOMP,IN,IER)
      ELSE
        INO=-1
        goto 30
      endif
      goto 20

      END


C ************* EDBLCTL
C Edit blind control period IN.
      SUBROUTINE EDBLCTL(ITRC,ICOMP,IN,IER)

      INCLUDE 'building.h'
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/B2/NBC(MCOM),IBCS(MCOM,MBP),IBCF(MCOM,MBP),BT(MCOM,MBP,5),
     &          BH(MCOM,MBP,5),BU(MCOM,MBP),IBCWIN(MCOM),BV(MCOM,MBP)
      COMMON/B3/NBCT(MCOM,MBP),BACTP(MCOM,MBP)
      COMMON/BLNDF1/BDOPT(MBP)
      COMMON/GOPT/DG(5),HG(5),UVAL,VTRN,NTL,AB(ME,5),RF(ME),SRF,SAB
      COMMON/FOPENED/CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK
      LOGICAL     OK,CFGOK,MLDBOK,CONDBOK,CTLOK,OPTKOK

      character SOPT*12,GDESCR*36,H*72
      character BDOPT*12

C Check to see that optical properties are available.
      IF(.NOT.OPTKOK)THEN
        SOPT='ALL'
        CALL EROPTDB(0,iuout,SOPT,GDESCR,IER)
        OPTKOK=.TRUE.
      ENDIF

      H(1)='Control interval start (0 to 23). '
      IS=IBCS(ICOMP,IN)
      CALL EASKI(IS,' ',' Control interval start?',
     &  0,'F',23,'F',0,'inverval start',IER,1)
      IBCS(ICOMP,IN)=IS

      H(1)='Control interval start (1 to 24). '
      IE=IBCF(ICOMP,IN)
      CALL EASKI(IE,' ',' Control interval end?',
     &  IS,'F',24,'F',IS+1,'interval end',IER,1)
      IBCF(ICOMP,IN)=IE

      CALL EASKOK(' ',' Edit optical properties?',OK)
      if(.NOT.OK)goto 661
      CALL EASKAB(' Change optical properties',' by:',
     &    'edit existing','optics db import',IW,0)
      if(IW.eq.2)then
        H(1)='The optic properties are required for the current'
        H(2)='control period.  Please enter the code for the'
        H(3)='required set as held in the optical properties'
        H(4)='database.'
        CALL PHELPD('optical selection',4,'-',0,0,IER)

   77   CALL EDWINO(SOPT,IER)
        CALL EROPTDB(ITRC,iuout,SOPT,GDESCR,IER)
        IF(IER.EQ.2)THEN
          CALL EASKOK(' ',' Not found.  Try again?',OK)
          IF(OK)GOTO 77
        ENDIF
        BDOPT(IN)=SOPT
        BU(ICOMP,IN)=UVAL
        BV(ICOMP,IN)=VTRN
        BT(ICOMP,IN,1)=DG(1)
        BT(ICOMP,IN,2)=DG(2)
        BT(ICOMP,IN,3)=DG(3)
        BT(ICOMP,IN,4)=DG(4)
        BT(ICOMP,IN,5)=DG(5)
        BH(ICOMP,IN,1)=HG(1)
        BH(ICOMP,IN,2)=HG(2)
        BH(ICOMP,IN,3)=HG(3)
        BH(ICOMP,IN,4)=HG(4)
        BH(ICOMP,IN,5)=HG(5)
      elseif(IW.eq.1)then

        BDOPT(IN)='user_defined'
        V1=BT(ICOMP,IN,1)
        H(1)='Allowable range 0.001 to 0.999.'
        CALL EASKR(V1,' Direct solar transmittance',
     &  ' @ normal incidence:',0.001,'F',0.999,'F',0.819,
     &  'd sol @ 0',IER,1)
        BT(ICOMP,IN,1)=V1

        V1=BT(ICOMP,IN,2)
        CALL EASKR(V1,' Direct solar transmittance',
     &    ' @ 40deg:',0.001,'F',BT(ICOMP,IN,1),'F',0.802,
     &    'd sol @ 40',IER,1)
        BT(ICOMP,IN,2)=V1

        V1=BT(ICOMP,IN,3)
        CALL EASKR(V1,' Direct solar transmittance',
     &    ' @ 55deg: ',0.001,'F',BT(ICOMP,IN,2),'F',0.761,
     &    'd sol @ 55',IER,1)
        BT(ICOMP,IN,3)=V1

        V1=BT(ICOMP,IN,4)
        CALL EASKR(V1,' Direct solar transmittance',
     &    ' @ 70deg: ',0.001,'F',BT(ICOMP,IN,3),'F',0.621,
     &    'd sol @ 70',IER,1)
        BT(ICOMP,IN,4)=V1

        V1=BT(ICOMP,IN,5)
        CALL EASKR(V1,' Direct solar transmittance',
     &    ' @ 80deg: ',0.001,'F',BT(ICOMP,IN,4),'F',0.376,
     &    'd sol @ 80',IER,1)
        BT(ICOMP,IN,5)=V1

  650   CONTINUE
        H(1)='Allowable range 0.001 to 0.999'
        V1=BH(ICOMP,IN,1)
        CALL EASKR(V1,' Total heat gain factor',
     &  ' @ normal incidence: ',0.,'F',0.999,'F',0.86,'ht gn @ 0',IER,1)
        BH(ICOMP,IN,1)=V1

        V1=BH(ICOMP,IN,2)
        CALL EASKR(V1,' Total heat gain factor',
     &    ' @ 40deg: ',0.,'F',BH(ICOMP,IN,1),'F',0.85,
     &    'ht gn @ 40',IER,1)
        BH(ICOMP,IN,2)=V1

        V1=BH(ICOMP,IN,3)
        CALL EASKR(V1,' Total heat gain factor',
     &    ' @ 55deg: ',0.,'F',BH(ICOMP,IN,2),'F',0.80,
     &    'ht gn @ 55',IER,1)
        BH(ICOMP,IN,3)=V1

        V1=BH(ICOMP,IN,4)
        CALL EASKR(V1,' Total heat gain factor',
     &    ' @ 70deg: ',0.,'F',BH(ICOMP,IN,3),'F',0.65,
     &    'ht gn @ 70',IER,1)
        BH(ICOMP,IN,4)=V1

        V1=BH(ICOMP,IN,5)
        CALL EASKR(V1,' Total heat gain factor',
     &    ' @ 80deg: ',0.,'F',BH(ICOMP,IN,4),'F',0.42,
     &    'ht gn @ 80',IER,1)
        BH(ICOMP,IN,5)=V1
        DO 660 K = 1,5
          IF (BT(ICOMP,IN,K) .GT. BH(ICOMP,IN,K))then
            call usrmsg(' Direct transmission cannot be greater',
     &                  ' than total heat gain factor.','W')
            GOTO 650
          endif
  660   CONTINUE

        V1=BU(ICOMP,IN)
        H(1)='The replacement U-value should be negated if control'
        H(2)='is to be extended to internal default windows.'
        CALL EASKR(V1,' ',' Replacement U-value:',
     &    -10.0,'F',10.0,'F',5.7,'U value',IER,2)
        BU(ICOMP,IN)=V1
      endif

C Actuation information.
  661 H(1)='Radiation is in W/m^2.'
      CALL EASKAB(' ','Blind control is based on:',
     &    'external radiation','ambient temperature',IW,1)
      NBCT(ICOMP,IN)=IW-1

C Index is valid.
      H(1)='For radiation - W/m^2, for temperature deg.C,'
      H(2)='if -99 then time time controlled.'
      V1=BACTP(ICOMP,IN)
      CALL EASKR(V1,' ',' Actuation point:',
     &   -99.,'F',999.0,'F',-99.,'Actuation point',IER,2)
      BACTP(ICOMP,IN)=V1

      RETURN
      END
