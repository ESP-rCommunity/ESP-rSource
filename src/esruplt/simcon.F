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
C      MZSIML
C      MZSPER
C      MZSIMH
C      MZTRAC

c ******************** MZSIML ********************

c MZSIML is the main energy simulation controlling routine
c performing the following functions:

c On each entry a routine is called which assigns the user's
c solution library and transfers plant description
c information to library header records.

c On first entry only the simulation period is requested.
c This period is subsequently assumed in force unless
c explicitly changed.

c After each simulation request a routine is called which
c allows definition of the simulation control parameters.

c The 'Save' command MUST be chosen after each simulation
c if the simulation results are to be saved in the solution
c library for subsequent review.

c The 'Delete' command deletes the last simulation result
c set saved in the solutions library.

      SUBROUTINE MZSIML
      INCLUDE 'plant.h'
      INCLUDE 'building.h'

      COMMON/SPAD/MMOD,LIMIT,LIMTTY 
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/HELP/IHELPM

      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/MFTRA/IMFTU

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

      COMMON/AVRAGE/IDAVER
      COMMON/PCEQU/IMPEXP,RATIMP
      COMMON/PMSLV/IPMSLV

      COMMON/FFN/IFLWN,ICFFS(MPCON)
      COMMON/MFSINC/MFBSNC
      common/piter/maxitp,perrel,pertmp,perflx,permfl,itrclp,
     &             icsv(mpnode,mpvar),csvi(mpnode,mpvar)
      integer maxitp            ! not used in current context
      integer itrclp            ! not used in current context
      integer icsv              ! flag marking nodes for iteration
      real perrel               ! not used in current context
      real pertmp               ! not used in current context
      real perflx               ! not used in current context
      real permfl               ! not used in current context
      real csvi                 !'initial' values of state variables
                                !    at start of iteration

      COMMON/CLMFLG/ICLMFL
      common /montrv/mnpc,mcomp(mpcom),mcnod(mpcom),mstate,monitr,
     &               ymin,ymax
      common/tsconp/iptsc,nptscp,ptsdat(24,20)

c The variable 'K' ensures that the 'RSIMUL' command can only
c be chosen after a simulation control function has been defined.
      COMMON/RSIM/K

      COMMON/PBYDAT/PBYFIL,NSCVP,IPS 
      COMMON/MFLITR/MAXITF,FERREL,FERMFL,PMAX,STEFFR,MFTRAC,ITER,IOK

      logical PBYFIL

      CHARACTER*42 ITEM(19)
      CHARACTER  outs*124,h*72

      ITEM(1) ='1: Define results library'
      ITEM(2) ='2: Specify simulation period'
      ITEM(3) ='   ......'
      ITEM(4) ='3: Start the simulation'
      ITEM(5) ='4: Save simulation results'
      ITEM(6) ='5: Delete last saved results'
      ITEM(7) ='   ......'
      ITEM(8) ='6: Monitor component(s) state variable'
      ITEM(9) ='   ......'
      ITEM(10)='0: End'
      ITEM(11)='a: Toggle results averaging'
      ITEM(12)='b: Change plant equations type'
      ITEM(13)='c: Modify fluid flow simulation parameters'
      ITEM(14)='d: Toggle fluid flow simulation time-step'
      ITEM(15)='e: Toggle climate interpolation.'
      ITEM(16)='f: Select another plant matrix solver'
      ITEM(17)='g: Invoke plant time-step controller'      
      DATA K/0/

C Intialise climate interpolation flag. By default, climate interpolation
C is enabled.
      ICLMFL=0

C Intialise default value of state variable monitor switch.
      monitr=0

c Default for plant equation generation is implicit or mixed,
c based on time-step evaluation; if mixed then impl/expl = 0.5
      IMPEXP=3
      RATIMP=.5

C Default mass flow simulation iteration parameters are set in MFLWSU
C Default fluid (air) flow simulation is once per hour (MFBSNC=0)
C This is over-ruled when plant fluid flow simulation is active, in
C which case fluid flow simulation will be performed every time-step
      MFBSNC=IFLWN

C Initialise plant side matrix solver. By default, the Gaussian approach.
      IPMSLV=1

C Initialise plant side iteration parameters
      MAXITP=100
      PERREL=.01
      PERTMP=1.
      PERFLX=10.
      PERMFL=.0005
      itrclp=MAXITP

C Couple building/plant & mass flow calculation trace output channels.
      IMFTU=ITU

C Initialise plant time-step controller flag. By default, no TSC.
      iptsc=0

C Close ascii boundry data file.
        if(pbyfil)then
          iunit=ifil+5
          call erpfree(iunit,istat)
        endif

C Assign user-specified library.
      CALL MZLIBF

c Set period if not already done.
      IF(ISDS.LT.1.OR.ISDS.GT.365.OR.ISDF.LT.1.or.
     &   ISDF.GT.365.OR.ISDF.LT.ISDS)CALL MZSPER

C Display menu of alternatives (J determines if simulation
C has been performed before attempt is made to save results
C in solution file).
      J=0
      NITMS=17
   16 continue
  129 INO=-6
      IF(IHELPM.EQ.0)GOTO 15
      h(1)=' Menu "SIMUL" commands are as follows :'
      h(2)='  1  assigns a user specified simulation results library'
      h(3)='     '
      h(4)='  2  allows user specification of the simulation period.'
      h(5)='     '
      h(6)='   3  invokes the simulation section of program bps.'
      h(7)='     Before simulation commences the user must define'
      h(8)='     the simulation control strategy.'
      h(9)='  4  MUST be chosen after each simulation if'
      h(10)='     the simulation results are to be saved in the '
      h(11)='     solution library for subsequent review.'
      h(12)='  5  deletes the last simulation result set saved'
      h(13)='     in the solution library.'
      h(14)='  a  allows results averaging to be turned ON/OFF'
      h(15)='  b  allows plant component state-space equations'
      h(16)='     to be user specified to implicit(1), mixed(2),'
      h(17)='     time-constant dependent(3, default) or steady'
      h(18)='     state(4).'
      h(19)='  c  if fluid flow simulation is being '
      h(20)='     conducted the user can set the algorithm iteration'
      h(21)='     parameters and trace output flag via this option.'
      h(22)='  d  a toggle which allows the user to perform the  '
      h(23)='     fluid flow simulation once per hour (0; default)'
      h(24)='     or for every building time-step (1).'
      h(25)='  e  this is a toggle which switches on/ off '
      h(26)='     climate interpolation: default ON.'
      h(27)='  f  allows you to select a non default plant matrix'
      h(28)='     equation solution technique, and to set the'
      h(29)='     plant solution process iteration parameters'
      CALL PHELPD('plt simul menu',29,'-',0,0,IER)

   15 CALL EMENU('SIMUL',ITEM,NITMS,INO)

c If menu pick was illegal INO=0.
      IF(INO.EQ.0)GOTO 1
      if(ino.eq.8) then
         if(MMOD.ne.8) then
            call edisp(iuout,' Use this option in graphics mode')
            goto 1
         endif
      endif
      GOTO(2,3,1,4,5,6,1,373,1,12,
     &     121,121,121,121,121,121,121,15,121),INO
    1 INO=-1
      GOTO 15

c Assign user-specified library.
    2 CALL MZLIBF
      J=0
      GOTO 16

c Set simulation period.
    3 CALL MZSPER
      J=0
      GOTO 16

c Set simulation control and invoke simulation routines.
    4 IER=0
      CALL MZCSTR(IER)
      IF(IER.EQ.1)GOTO 16
      CALL MZNUMA
      J=1
      K=1
      call epagew
      GOTO 16

c Save simulation results.
    5 continue
      IF(J.EQ.0)GOTO 17
      CALL MZSAVE(ISIMN1,ISIMN2)
      WRITE(outs,'(A,I3)') ' Plant library simulation no.',ISIMN2
      call edisp(iuout,outs)
      WRITE(outs,18)ISD1,ISM1
   18 FORMAT(' Results saved for the period from day',I3,' of month',
     &I3)
      call edisp(iuout,outs)
      WRITE(outs,19)ISD2,ISM2
   19 format(32X,'to day',I3,' of month',I3)
      call edisp(iuout,outs)
      J=0
   21 call epwait
      GOTO 16

c No simulation performed prior to this attempt to save.
   17 WRITE(outs,'(A)') 'Nothing to save.'
      call edisp(iuout,outs)
      GOTO 21

c Delete last plant simulation result set.
    6 continue
      CALL MZDEL(ISIMLP)
      IF(ISIMLP.LT.0)GOTO 22
      CALL ELINC(1)
      WRITE(outs,68)ISIMLP
   68 FORMAT(' Number of plant result sets remaining = ',I3)
      call edisp(iuout,outs)
      GOTO 21

c Solution file does not contain any simulation results
c in the first place.
   22 CALL ELINC(1)
      WRITE(outs,'(A)') ' No results saved in library !'
      call edisp(iuout,outs)
      GOTO 21

C Picks a, b, c, d, e, and f are password protected.
  121 continue
        H(1)=' This option should be used with care '
        H(2)=' since it can adversely affect the simulation '
        H(3)=' results. Consequently it is password protected. '
        H(4)=' '
        H(5)=' The password is available from ESRU  '
        H(6)='    Email: esru@strath.ac.uk  '
        H(7)='    Tel: +44 41 552 4400 ext 3024  '
        H(8)='    Fax:  +44 41 552 8513  '
        CALL EASKI(IANS,' ',' Password: ',
     &           0,'-',0,'-',0,'Password',IER,8)
        IF(IANS.NE.101)GOTO 16

      IF(INO.EQ.11) GOTO 126
      IF(INO.EQ.12) GOTO 131
      IF(INO.EQ.13) GOTO 302
      IF(INO.EQ.14) GOTO 303
      IF(INO.EQ.15) GOTO 307
      IF(INO.EQ.16) GOTO 308
      IF(INO.EQ.17) GOTO 470

  126   H(1) ='plt generates the values of each state variable '
        H(2) ='at each computational time row. The default action '
        H(3) ='is to then transfer the average value for the time-'
        H(4) ='step to the results database. That is, the average '
        H(5) ='of the present and future time row values at each '
        H(6) ='time-step. This option allows the averaging process '
        H(7) ='to be turned off. Note that res could then be used '
        H(8) ='to perform post simulation averaging if required. '
        CALL EASKI(IDAVER,' ',' Results averaging  0) On,  1) Off ? ',
     &    0,'F',1,'F',0,'Results averaging iq 14',IER,8)
        IF(IER.NE.0)GOTO 126
      GOTO 129

c Plant components state-space equation types.
  131   call edisp(iuout,' ')
        call edisp(iuout,'Plant state-space equation type from ')
        call edisp(iuout,' ')
        call edisp(iuout,'   implicit                          (1)')
        call edisp(iuout,'   mixed                             (2)')
        call edisp(iuout,'   time constant dependent (default) (3)')
        call edisp(iuout,'   steady state                      (4)')
        call edisp(iuout,' ')

        H(1)='To ensure stability, bps changes the plant state-space'
        H(2)='equations as a function of the computation time '
        H(3)='step and component time constant (which is fluid flow '
        H(4)='dependent). If the time step is greater than 63% of'
        H(5)='the time constant then the equation is fully implicit.'
        H(6)='Otherwise it is mixed implicit/explicit.'

        CALL EASKI(IMPEXP,' ',' Plant equation type: ',
     &         1,'F',4,'F',3,'plant equation type iq 15',IER,6)
        IF(IER.NE.0)GOTO 131

        IF(IMPEXP.EQ.2.OR.IMPEXP.EQ.3) THEN
          call edisp(iuout,' ')
          call edisp(iuout,' Implicit portion of ratio (0 <= & <= 1)')
          call edisp(iuout,'   0 = fully explicit  ')
          call edisp(iuout,'  .5 = Crank-Nicholson scheme (default)  ')
          call edisp(iuout,'   1 = fully implicit ')
          call edisp(iuout,' ')
          H(1)='Ratio should be in range of 0.0 to 1.0. '
          CALL EASKR(RATIMP,' ',' Ratio : ',
     &             0.0,'F',1.0,'F',0.5,'implicit/explicit ratio',IER,1)
        ENDIF
      GOTO 129

C Alter mass flow solver iteration parameters, but
C always synchronize corresponding plant solution iteration
C parameters except for MAXITP / MAXITF
  302 IF(IFLWN.EQ.0) THEN
          call edisp(iuout,
     &      ' No fluid flow file active; menu selection invalid.')
      else
          CALL MFMODE(1)
          PERREL=FERREL
          PERMFL=FERMFL

C Use different stack pressure assumptions.?
          CALL MFMODE(2)

C Use a different matrix solver?
          CALL MFMODE(3)

C Mass flow trace.
          CALL MFMODE(4)
      ENDIF
      GOTO 129

C Reset fluid flow calculation & building time-step synchronisation
C This is over-ruled in case of plant fluid simulation
  303 IF(IFLWN.EQ.0) THEN
          call edisp(iuout,
     &     ' No fluid flow file active: Menu selection invalid.')
        ELSE IF(IFLWN.EQ.1) THEN
          call edisp(iuout,
     &     ' Plant fluid flow simulation active; fluid flow')
          call edisp(iuout,
     &     ' simulation & building time-step synchronisation')
          call edisp(iuout,' cannot be reset !')
      ELSE
          CALL EASKAB(' Fluid flow simulation: ',' ',
     &      'once per hour','every building time-step',IW,0)
          MFBSNC=IW-1
      ENDIF
      GOTO 129

C Tggle climate interpolation switch.
  307 CALL EASKAB(' ',' Climate interpolation: ',
     &    'On','Off',IW,0)
      ICLMFL=IW-1
      GOTO 129

C Select a different plant matrix solver.
  308   call edisp(iuout,' ')
        call edisp(iuout,
     &  ' Currently supported plant matrix solver options are:')
        call edisp(iuout,
     &  ' 1 - Compact matrix gaussian elimination with back ')
        call edisp(iuout,'     substitution and no pivoting')
        call edisp(iuout,
     &  ' 2 - LU decomposition with implicit pivoting (Crout).')
        call edisp(iuout,
     &  ' 3 - Gaussian elimination with back substitution')
        call edisp(iuout,'     and no pivoting')
        call edisp(iuout,' ')
        CALL EASKI(IPMSLV,' ',' Plant solver type ?',
     &    1,'F',3,'F',2,'solver type',IER,0)
        IF(IER.NE.0) GOTO 308

  309   call edisp(iuout,
     &  ' Some plant component coefficient generators need to')
        call edisp(iuout,
     &  ' iterate to find an exact solution.  This process is')
        call edisp(iuout,
     &  ' controlled by following parameters:')
        write(outs,10309) MAXITP
10309   format(' number of iterations   [current: ',I4,' (-) ]')
        IVAL=MAXITP
        CALL EASKI(IVAL,outs,' : ',1,'F',0,'-',1,'no iter',IER,0)
        MAXITP=IVAL
        write(outs,10314) itrclp
10314   format(' no. of iterations for control loops',
     &            '   [current: ',I4,' (-) ]')
        IVAL=itrclp
        CALL EASKI(IVAL,outs,' : ',1,'F',0,'-',1,'no iter',IER,0)
        itrclp=IVAL
        write(outs,10310) PERREL
10310   format(' relative error         [current: ',G10.4,' (-) ]')
        VAL=PERREL
        CALL EASKR(VAL,outs,' : ',
     &      0.0001,'F',0.,'-',1.,'rel error',IER,0)
        PERREL=VAL
        write(outs,10311) PERTMP
10311   format(' abs. error temperature [current: ',G10.4,' (K) ]')
        VAL=PERTMP
        CALL EASKR(VAL,outs,' : ',
     &      0.0001,'F',0.,'-',1.,'abs err temp',IER,0)
        PERTMP=VAL
        write(outs,10312) PERFLX
10312   format(' abs. error heat flux   [current: ',G10.4,' (W) ]')
        VAL=PERFLX
        CALL EASKR(VAL,outs,' : ',
     &      0.0001,'F',0.,'-',1.,'abs err flux',IER,0)
        PERFLX=VAL
        write(outs,10313) PERMFL
10313   format(' abs. error mass flow   [current: ',G12.4,' (kg/s) ]')
        VAL=PERMFL
        CALL EASKR(VAL,outs,' : ',
     &      0.0001,'F',0.,'-',1.,'abs err flux',IER,0)
        IF(IER.NE.0) GOTO 309
        PERMFL=VAL

C Perhaps plant solution iteration parameters are altered, so always
C synchronize corresponding mass flow solver iteration parameters
C except for MAXITP / MAXITF
        FERREL=PERREL
        FERMFL=PERMFL
      GOTO 129

C Monitor component state variable.
  373 monitr=1
      call minput
      goto 129

c Invoke plant time-step controller.
  470 iptsc=1
      call ptscin
      goto 129

   12 RETURN
      END

c ******************** MZSPER ********************

c MZSPER allows user definition of the simulation
c period (in terms of start and finish day and month
c numbers) and the computational time step (measured
c as a fraction of and including one hour).

c Common block variables are:

c ISD1 & ISM1  - start day and month numbers.
c ISD2 & ISM2  - finish day and month numbers.
c ISDS & ISDF  - start and finish year day numbers.
c NTSTEP       - number of computational time-steps
c                within each hour.
c TIMSEC       - length of time increment in seconds

      SUBROUTINE MZSPER

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/Pctime/TIMSEC
      COMMON/PCTSTP/NTSTPP

C Once per hour storage of results, averaged over that hour.
      common/rdcgen/irdact

      character h*72

C Request simulation period in user defined format of d-o-y
C or day and month, return julian days and convert to month
C and day.
   14 ifday=1
      isds=9
      isdf=15
    8 call EASKPER('Simulation period:',ISDS,ISDF,IFDAY,IER)
      IF(IER.NE.0) GOTO 14

C Convert to year day numbers (1st Jan = Year day 1 etc.).
      call EDAYR(ISDS,ISD1,ISM1)
      call EDAYR(ISDF,ISD2,ISM2)

C Check start day before finish day.
      IF(ISDS.LE.ISDF)GOTO 16
      CALL edisp(iuout,' Start/Finish dates out of order. ')
      GOTO 8

C Define computational time-step.
   16 NTSTEP=1
      h(1)=' The plant-side equations will be established'
      h(2)=' and solved this many times per hour. A 4/hour'
      h(3)=' scheme is usually sufficient, although smaller'
      h(4)=' time-steps may be required for some control'
      h(5)=' regimes to permit frequent control decision points.'
      call easki(NTSTPP,' ',

C Minimum allowable time-step within specified period is 0.01 second.
     &   ' Plant side: number of time-steps/hour ?',1,'F',
     &   360000,'F',4,'plnt time-steps/h',ier,5)

C Calculate the length of the time increment in seconds.
      TIMSEC=3600.0/(FLOAT(NTSTEP)*FLOAT(NTSTPP))

  50  irdact=1
      H(1) ='plt calculates the value of each state variable '
      H(2) ='at each computational time step. The default action'
      H(3) ='is to transfer each value at each computational'
      H(4) ='time step to the results database(s).'
      H(5) ='This option allows to average results over one'
      H(6) ='hour before transfer to the results database(s),'
      H(7) ='thus decreasing the size of results database(s).'
        CALL EASKI(irdact,' ',
     &    ' Hourly results integration  1) On, 0) Off ? ',
     &    0,'F',1,'F',1,'Hourly integration',IER,7)
      IF(IER.NE.0)GOTO 50

      RETURN
      END

c ******************** MZSIMH ********************

c MZSIMH outputs simulation period and wait time information
c prior to each simulation commencing.

      SUBROUTINE MZSIMH(ICON)
      INCLUDE 'building.h'

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/SHOUT/ICOUT

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/AVRAGE/IDAVER
      COMMON/PCEQU/IMPEXP,RATIMP

      logical ok,dok
      character h*72

      IMESS=0

      IF(IMPEXP.NE.3.OR.RATIMP.NE.0.5) THEN
        IMESS=1
      ENDIF
      IF(IDAVER.NE.0)THEN
        IMESS=1
      ENDIF

      IF(IMESS.EQ.1)THEN
        CALL ELINC(1)
        WRITE(IUOUT,99)
   99   FORMAT(' Special features invoked:')
      ENDIF

c Default for plant equation generation is implicit
c or mixed, based on time-step evaluation.
      IF(IMPEXP.NE.3.OR.RATIMP.NE.0.5) THEN

        CALL ELINC(1)
        WRITE(IUOUT,*) ' Plant equation type:'
        CALL ELINC(2)
        IF(IMPEXP.EQ.1)   WRITE(IUOUT,*) '   1 - implicit'
        IF(IMPEXP.EQ.2)   WRITE(IUOUT,*) '   2 - mixed'
        IF(IMPEXP.EQ.3)   WRITE(IUOUT,*) '   3 - timestep dependent'
        IF(IMPEXP.EQ.4)   WRITE(IUOUT,*) '   4 - steady state'
        IF(RATIMP.NE.0.5) WRITE(IUOUT,*) '   impl/expl = ',RATIMP
      ENDIF

C Default - results are averaged.
      IF(IDAVER.NE.0)THEN
        CALL ELINC(1)
        WRITE(IUOUT,4413)
 4413   FORMAT(' Averaging OFF')
      ENDIF

c Option to terminate.
      dok=.true.
      h(1)='Pause to allow you to abort the simulation. '
      CALL ASKOK(' ',' Continue with simulaton?',OK,dok,1)
      IF(OK)goto 6
      ICON=0
      goto 7

c Commence simulation.
    6 call edisp(icout,' ')
      call edisp(icout,' Simulation has now commenced.')
      ICON=1
      IF(ITC.GT.0.AND.ITU.EQ.IUOUT)continue
    7 RETURN
      END

c ******************** MZTRAC ********************

c This routine allows the user to
c select areas of ESRUplt for trace output.

c Common block variables are:

c ITC     - trace output index consisting of some +ve integer
c           value where:
c                      ITC=0 gives no trace output
c                      ITC>0 gives trace output commencing during
c                            the ITC'th time-increment
c ITCF    - is the ITCF'th time increment for the
c           trace output to end
c ITRACE  - trace call index for appropriate subroutine.
c ICNT    - counts trace output for page numbering purposes

      SUBROUTINE MZTRAC
      INCLUDE 'building.h'

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      CHARACTER*36 ITEM(14)
      character  H*72

      ITEM(1) ='1 Configuration file'
      ITEM(2) ='   ......'
      ITEM(3) ='2 Simulation time increment counter'
      ITEM(4) ='   ......'
      ITEM(5) ='3 Matrix template'
      ITEM(6) ='4 Component static data'
      ITEM(7) ='5 Component adjacency information'
      ITEM(8) ='6 Coefficient  generator data'
      ITEM(9) ='7 Plant matrix solver'
      ITEM(10)='8 Plant matrix set up'
      ITEM(11)='9 Trace all'
      ITEM(12)='  ......'
      ITEM(13)='a CLEAR'
      ITEM(14)='- End selection'

c Initilise trace output facility.
      ITC=0
      ITCF=0
      ITU=IUOUT
      DO 10 I=1,MTRACE
      ITRACE(I)=0
   10 CONTINUE

C Trace output facility is obtained if invoked
C from menu ESRUplt.
      NITEMS=14
      INO=-2
    4 CALL EMENU('TRACE',ITEM,NITEMS,INO)
      IF(INO.LE.0)GOTO 1
      IF(INO.EQ.2.OR.INO.EQ.4.OR.INO.EQ.12)GOTO 1
      IF(INO.EQ.13)GOTO 2
      IF(INO.EQ.14)GOTO 6
      GOTO 3
    1 INO=-1
      GOTO 4

    2 DO 50 I=1,MTRACE
      ITRACE(I)=0
   50 CONTINUE
      GOTO 4

    3 II=INO
      IF(INO.EQ.3)II=33
      IF(INO.GT.4)II=II+29
      ITRACE(II)=1
      GOTO 4

    6 IF(ITRACE(40).EQ.1) THEN
         ITRACE(1) =1
         ITRACE(33)=1
         ITRACE(34)=1
         ITRACE(35)=1
         ITRACE(36)=1
         ITRACE(37)=1
         ITRACE(38)=1
         ITRACE(39)=1
      END IF
      continue
      IVAL=ITC
      H(1)='The trace output commences at the start time increment '
      H(2)='and ceases at the finish. '
      H(3)='Note that the actual times will depend on the '
      H(4)='simulation time step. '
      H(5)=' '
      H(6)='`1` will start the trace immediately, ie within the '
      H(7)='    startup period'
      CALL EASKI(IVAL,' Start trace after how many',
     &  ' (building) time increments ?  ',
     &      1,'F',1,'-',1,'trace start',IER,7)
      ITC=IVAL

      H(1)='The trace output ceases at the finish of the simulation.'
      H(2)='Note that the actual times will depend on the '
      H(3)='simulation time step. '
      H(4)=' '
      H(5)='`24` will finish the trace at the end of the the first'
      H(6)='     startup day if the timestep is one.'
      IVAL=ITC+1
      CALL EASKI(IVAL,' Finish trace after how many',
     &  ' (building) time increments ?  ',
     &      ITC,'F',0,'-',5,'trace finish',IER,6)
      ITCF=IVAL

      H(1)='The trace output can be directed to the '
      H(2)='terminal or placed in a scratch file opened '
      H(3)='on FORTRAN channel 33. '
      CALL EASKAB(' ',' Write trace to: ',
     &    'terminal','scratch file',IW,3)
      if(IW.eq.1)then
        ITU=IUOUT
      else
        ITU=33
        open(itu,file='trace.33',status='unknown')
      endif

      RETURN
      END
