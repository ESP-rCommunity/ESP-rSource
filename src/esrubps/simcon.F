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

C BPS version: This file contains the following routines:

C  SIMCON   Executes the required simulations.
C  MZSIML   Sets the final simulation parameters prior to simulation. 
C  SIMTOG   Set simulation toggles
C  MSTTOG   Set moisture flow toggles.
C  STMTOG   Set building construction structured mesh toggles.
C  MZSPER   Definition of simulation period.
C  MZSIMH   Lists the current simulator parameters (dates, save option etc.)
C  MZTRAC   Define quantities to be traced.
C  MZINFO   Displays values of the currently set simulation parameters.

C ******************** SIMCON
C SIMCON sets the final parameters before executing a simulation.  MZNUMA is 
C called the required number of times, saving results after each simulation. 

      subroutine SIMCON
      
      USE AIM2_InputData, ONLY:iAIM2
      USE AIM2, ONLY: AIM2_SETVARS
      
      IMPLICIT NONE

C      USE H3KSTORE_FC_DATA,ONLY: iCurrent_FCT_OUTPUT_FILE
#include "building.h"
#include "uncertainty.h"
#include "hvac_parameters.h"
#include "h3kstore_parameters.h"
#include "h3kstore_common.h"
#include "SOFC.h"
#include "UserSimulationToggles.h"

      common/filep/ifil
      INTEGER :: ifil
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER :: iuout,iuin
      common/pophelp/h(60)

      COMMON/PBYDAT/PBYFIL,NSCVP,IPS
      INTEGER :: NSCVP,IPS

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      INTEGER :: ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

      COMMON/SOLOFF/ISOLAR
      INTEGER ISOLAR
      
      COMMON/GR1D04/GAM,RGAM
      REAL GAM, RGAM
      
      COMMON/SAVE/ISAVE
      INTEGER :: isave
      COMMON/SAVE1/LSAVE
      INTEGER :: lsave

      COMMON/C6/INDCFG
      INTEGER :: indcfg

C Uncertainty.
      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)
      COMMON/UA5/IMET,ISIM,NRSIM
      INTEGER :: imet,isim,nrsim


c Mechanical ventilation system activator
      Integer iMech
      Character *72 LMech
      Common /mechvdef/ iMech, LMech

C Simulator parameter.
      COMMON/SPFL/spfileok,perok,tstepok,saveok,autook,exitok,startupok
      LOGICAL spfileok,perok,tstepok,saveok,autook,exitok,startupok
      logical PBYFIL,OK,dok

      character LUALF*72,LCNG*15,LLOC*15,outs*124,h*72
      
      INTEGER :: i,ier,irep,isimn1,isimn2,istat,iualf,iunit
      INTEGER :: nstep
      REAL :: tstep

      OK=.FALSE.

C Default values for IMET and NRSIM. 
      IMET=0
      NRSIM=1

C Set simulation control.
      IER=0
      call MZCSTR(IER)
      if (IER.eq.1) then
C If simulator is in silent running mode, set flag to terminate run      
         if ( autook ) exitok = .true.
         return
      endif 

C Calculate the BASESIMP heat loss factors.
      CALL BSFCCTL

C Calculate the time-invariant variables for the AIM-2 infiltration model.
      IF( iAIM2==1 ) THEN
        CALL AIM2_SETVARS
      ENDIF
c
c Initialization for Mechanical ventilation systems
c
      If ( iMech .eq. 1 ) Call MECH_VENT_INIT ( )
c ---

C Are uncertainties defined?
      if (LUALF(1:7).ne.'UNKNOWN') then
        IUALF=IFIL+1
        call readual(IUALF)
        call SENRNS(IER)
        if (IER.eq.1) return
      endif
      

C Reimpose any user-specified simulation toggles as needed 
      if ( bUserGAM ) GAM = fUserGAM        ! User-specified-implicitness.
      if ( bUserSolar ) ISOLAR = iUserSolar ! User-specified-solar-processing 
      print*, ".... ", bUserSolar, iUserSolar
C Run the necessary number of simulations.
      do 100, I=1,NRSIM 
        ISIM=I

C Write info to sensitivity trace file.
        if (NRSIM.gt.1) then
          write (36,*) '**'
          write (36,*) '** Edits for changes in set:',ISIM
          write (36,*) '**'
        endif

C Reset the data if ISIM > 1 and change the required data. 
        if (ISIM.gt.1) then

C Re-scan the model description files silently.
          IREP=1
          write (36,*) '** Edits for changes in set:',ISIM,NTSTEP
          call MZINPT(IREP,IER)
          TSTEP=1.0/float(NTSTEP)
          CALL MZCOE2(TSTEP)
          write (36,*) '** Edits for changes in set:',ISIM,NTSTEP
          call MZCSTR(IER)
          write (36,*) '** Edits for changes in set:',ISIM,NTSTEP
        endif
        
C MZNUMA is effectively a simulation clock.
        CALL MZNUMA

C Close ascii plant-side boundary data file if last simulation 
C otherwise rewind for use in next simulation.
        if(pbyfil)then
          if (ISIM.eq.NRSIM) then 
            iunit=ifil+9
            call erpfree(iunit,istat)
          else
            iunit=ifil+9
            rewind(iunit)
          endif
        endif
        LSAVE=ISAVE

C Confirm save with user - only if not in autoexec mode as
C defined in the simulator perameter file.
c Send Output to file if save level = 5
        IF (ISAVE.eq.5 .and.
     &        ( indcfg == 1 .or. indcfg == 3 ) ) then
          call h3kmzoutp()
        ENDIF
c Close FC timstep output if open.      
        IF (FC_timeStep_output == FC_timeStep_output_yes) then
          close(unit=iCurrent_FCT_OUTPUT_FILE)
        ENDIF

        if(autook) then
          ok=.true.
          exitok=.true.
        elseif (LUALF(1:7).NE.'UNKNOWN') then
          ok=.true.
        else
          dok=.true.
          h(1)='You can only extract performance reports and graphs'
          h(2)='you say YES to this question.  '
          h(3)=' '
          h(4)='If you say NO you will have the computer equivalent'
          h(5)='of a paperweight. '
          CALL ASKOK('Save simulation results?','(see help)',OK,dok,5)
        endif
        IF(.NOT.OK)then
          return
        endif
        IF(LSAVE.EQ.0)then

C Message to recover save option 0 data from 'OUTPUT'.
          call edisp(iuout,' Save option 0 data cannot be saved.')
          call edisp(iuout,' Results recovered via menu command o.')
          return
        endif
C No results dbs for save-level 5
        if (LSAVE .EQ. 5 ) return


        CALL MZSAVE(ISIMN1,ISIMN2)
        if(INDCFG.eq.1)then
          write(outs,'(A,I3)')' Zones library simulation no.',ISIMN1
          call edisp(iuout,outs)
        elseif(INDCFG.eq.2)then
          write(outs,'(A,I3)')' Plant library simulation no.',ISIMN2
          call edisp(iuout,outs)
        elseif(INDCFG.eq.3)then
          write(outs,1870)ISIMN1
 1870     format(' Zones library simulation no.',I3,' and')
          call edisp(iuout,outs)
          write(outs,1871)ISIMN2
 1871     format(' Plant library simulation no.',I3)
          call edisp(iuout,outs)
        endif
        if (ISIM.le.1.or.ISIM.eq.NRSIM) then
          call edisp(iuout,' ')
          write(outs,1872)ISD1,ISM1
 1872     format(' Results saved for the period from day',I3,
     &                                           ' of month',I3)
          call edisp(iuout,outs)
          write(outs,1873)ISD2,ISM2
 1873     format(32X,'to day',I3,' of month',I3)
          call edisp(iuout,outs)
        endif
 100  continue

      return
      end

C ******************** MZSIML ********************

C MZSIML is the main energy simulation controlling routine
C performing the following functions:

C On each entry a routine is called which assigns the user's
C solution library and transfers zone description
C information to library header records.

C On first entry only the simulation period is requested.
C This period is subsequently assumed in force unless
C explicitly changed.

C After each simulation request a routine is called which
C allows definition of the simulation control parameters.

C The '>1SAVE' command MUST be chosen after each simulation
C if the simulation results are to be saved in the solution
C library for subsequent review.

C The '<1DEL' command deletes the last simulation result
C set saved in the solutions library.

C The '0SAVE' command does not write to results library.
C Simulation results retrieved from 'OUTPUT'.
C The '1SAVE', '2SAVE' and '3SAVE' commands cause minimum,
C moderate and all simulation results to be transferred to
C the results library; where:

C     'minimum'  includes zone air temperatures and plant
C                parameters only.
C     'moderate' includes (in addition to the minimum
C                transfer) all other energy flows plus surface
C                temperatures.
C     'all'      includes (in addition to the minimum and
C                moderate transfers) internal fabric
C                temperatures.
C Save option 4 is a special save for surface energy balances.
C It requires the same disk space as save option 3 and uses
C space normally used by the inter surface node temperatures.

C The default is '2SAVE'.

C The 'INFO' command outputs the current simulation period
C and information on the saved result set.

      SUBROUTINE MZSIML
#include "building.h"
#include "model.h"
#include "plant.h"
#include "net_flow.h"
#include "monitor.h"
#include "cfd.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/MFTRA/IMFTU

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

      COMMON/SAVE/ISAVE
      COMMON/C6/INDCFG
      COMMON/BEFORM/IBEQNF
      COMMON/PCEQU/IMPEXP,RATIMP
      COMMON/SOLOFF/ISOLAR
      COMMON/SKYFLG/ISKYFL
      COMMON/PMSLV/IPMSLV

      COMMON/FFN/IFLWN,ICFFS(MPCON)
      COMMON/MFSINC/MFBSNC
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)

C Tolerance for iteration of hydrogen flow matrix 
      common/PITER_H2/fH2_iteration_tolerance
      real fH2_iteration_tolerance

      COMMON/TS/ITSFLG,NTSCPD,ITSSCH(MSCH,5),itsmus
      COMMON/CLMFLG/ICLMFL
      COMMON/AIRWAT/WATERZ(MCOM),WATCAP
      COMMON/VERSOL/ipkzon(mzs),ipksf(mzs),ivsolfil,vsol,VTSOL,vsnam
      COMMON/PWRFLG/IPWRSIM
      common /bmontr/mnzn,mzone(mcom),montrb,yminb(2),ymaxb(2),
     &               ibplot,irecpb(MDPH),mbstv
      common /montrv/mnpc,mcomp(mpcom),mcnod(mpcom),mstate,monitr,
     &               ymin,ymax
      COMMON/SPFL/spfileok,perok,tstepok,saveok,autook,exitok,startupok
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF

      LOGICAL     spfileok,perok,tstepok,saveok,autook,exitok,startupok
      LOGICAL WATERZ,VTSOL,SAVE4,SAVE4OK,dok

      CHARACTER*72 H,vsnam
      CHARACTER*29 ITEM(18)
      character outs*124
      integer NITMS,INO  ! max items and current menu item

C Intialise timestep controller flag. By default, timestep controller
C is disabled.  Also initialise number of TSC operations to zero.
C Initialise pwer simulation flag.
      ITSFLG=0
      NTSCPD=0

C Intialise climate interpolation flag. By default, climate interpolation
C is enabled.
      ICLMFL=0

C Intialise default value of zone(s) temperature monitor switch.
      montrb=0
      ibplot=0

C Intialise default value of plant state variable monitor switch.
      monitr=0

C Default for zone nodal equation generation is implicit or mixed,
C based on time-step evaluation.
      IBEQNF=0

C Default for plant equation generation is implicit or mixed,
C based on time-step evaluation; if mixed then impl/expl = 0.5
      IMPEXP=3
      RATIMP=.5

C Default solar treatment, solar ON, Perez model.
      ISOLAR=0
      iUserSolar = iSolar
      ISKYFL=3

C Default mass flow simulation iteration parameters are set in MFLWSU
C Default fluid (air) flow simulation is once per zone timestep
C (MFBSNC=1). This is over-ruled when plant fluid flow simulation
C is active, in which case fluid flow simulation will be performed
C every plant time-step <<check with essam on this!!!>>.
      MFBSNC=1
      if(IFLWN.eq.1)MFBSNC=IFLWN

C Initialise plant side matrix solver. By default, the Gaussian approach
      IPMSLV=1

C Initialise plant side iteration parameters
      MAXITP=100
      PERREL=.01
      PERTMP=1.
      PERFLX=10.
      PERMFL=.0005
      itrclp=maxitp
C Hydrogen flow iteration tolerance
      fH2_iteration_tolerance = PERMFL * 1.0E-03 


C Initialise all zones to air-filled
      do 114 iwz=1,mcom
        WATERZ(iwz)=.false.
 114  continue

C Initialise reading in of vertical solar data.
      VTSOL=.false.

C Couple zone/plant & mass flow calculation trace output channels.
      IMFTU=ITU

C Assign user-specified library.
      CALL MZLIBF

C Set period if not already done. However if there is a CFD domain
C in the model then we need to ask about when to include it (as this
C information is not yet included in the simulation parameter set). 
      IF ( .not. perok )then
        CALL MZSPER
      ELSE
        call askaboutcfd
      ENDIF
      
C Display menu of alternatives.
      if(.not.saveok)ISAVE=2

C Set up the menu text.
   92 CONTINUE
      INO=-4

      ITEM(1)=   'a Specify results db         '
      ITEM(2)=   'b Specify simulation period  '
      ITEM(3)=   '  ....................       '
      if(ISAVE.eq.0)then
        ITEM(4)= '* Save >> 0 summary table    '
      elseif(ISAVE.eq.1)then
        ITEM(4)= '* Save >> 1 zone temps & inj '
      elseif(ISAVE.eq.2)then
        ITEM(4)= '* Save >> 2 (1+) surf temps  '
      elseif(ISAVE.eq.3)then
        ITEM(4)= '* Save >> 3 (2+) node temps  '
      elseif(ISAVE.eq.4)then
        ITEM(4)= '* Save >> 4 (2+) energy balnc'
c save level 5 
      elseif(ISAVE.eq.5)then
        ITEM(4)= '* Save >> 5 H3K no res file'
      elseif(ISAVE.eq.6)then
        ITEM(4)= '* Save >> 6 summary csv data '
      endif
      ITEM(5)=   '  ....................       '
      if(ISAVE.eq.0)then
        ITEM(6)= '  Monitor (not available)'
      else
        ITEM(6)= 'm Monitor state variables    '
      endif
      ITEM(7)=   't Invoke time-step controller'
      ITEM(8)=   's Invoke simulation          '
      ITEM(9)=   '                             '
      ITEM(10)=  '< Delete last result set     '
      ITEM(11)=  '  ....................       '
      ITEM(12)=  'g Simulation toggles         '
      ITEM(13)=  'i Info on current parameters '
      ITEM(14)=  'o View data from 0 level save'
      ITEM(15)=  '  ....................       '
      ITEM(16)=  '? Help                       '
      ITEM(17)=  '- Exit to main menu          '
      NITMS=17

C Help text for this menu.
  91  H(1) ='The main simulation menu provides the following options:'
      H(2) =' '
      H(3) =' a: assign name to simulation results library.'
      H(4) =' b: specify the simulation period.'
      H(5) =' '
      H(6) =' *: Options which dictate what is saved in the library:'
      H(7) ='     SAVE >>0 summary text report only (see option o)'
      H(8) ='     SAVE >>1 minimum data collection (for small files)'
      H(9) ='     SAVE >>2 moderate data collection'
      H(10)='     SAVE >>3 as save 2 plus all node temperatures'
      H(11)='     SAVE >>4 as save 2 plus surface energy balance'
c Save level 5
      H(12)='     SAVE >>5 Hot3000 output (XML) based on directives'
      H(13)='              in input.xml file in model cfg folder'
      H(14)='     SAVE >>6  no results saved (summary only in '
      H(15)='               database freindly text format)'      
      H(16)=' m: monitor plots zone temperatures, plant flux and'
      H(17)='    plant state variables as the simulation progresses.'
      H(18)=' '
      H(19)=' t: select zone-side time-step control option.'
      H(20)=' s: invoke simulation. '
      H(21)=' <: deletes the last saved simulation result set. '
      H(22)=' g: sets simulation parameter toggles '
      H(23)=' '
      H(24)=' i: outputs the current simulation period and'
      H(25)='    information on the saved result set.'
      H(26)=' o: gives summary results if SAVE O option selected'
      H(27)='    with option to write to a text file after the'
      H(28)='    simulation has completed.'

C Do not display the menu if in autook mode as defined in the 
C simulator parameter file.
C NOTE: if the menu is changed then the values assigned to ino 
C       will have to be updated.
      if (autook) then
        if (exitok) then
          ino=NITMS
        else
          ino=8
        endif
      else
        CALL EMENU('SIMUL',ITEM,NITMS,INO)
      endif

      if(ino.eq.1)then

C Results db name.
        call tstamp('>','SIM: change results library')
        CALL MZLIBF
      elseif(ino.eq.2)then

C Set simulation period.
        call tstamp('>','SIM: change simulation period')
        CALL MZSPER
      elseif(ino.eq.4)then

C Save level.
         call tstamp('>','SIM: change save level')
         ISAVE=ISAVE+1
C Save Level reset to zero if toggle set would be greater than 6.
         if(ISAVE.gt.6)ISAVE=0
      elseif(ino.eq.6)then

C Graphic monitor setup.
        if(indcfg.eq.1) then
          call tstamp('>','SIM: setup graphic monitor')
          montrb=1
          call minptb
        elseif(indcfg.eq.2) then
          call tstamp('>','SIM: setup graphic monitor')
          monitr=1
          call minput
        elseif(indcfg.eq.3) then
          montrb=0
          monitr=0
          h(1)='You may track some performance data as the simulation '
          h(2)='progresses. '
          CALL EASKAB(' Simulation monitoring options: ',
     &    '  ','Zones','Plant',IDIM,2)
          if(idim.eq.1)then
            call tstamp('>','SIM: setup graphic monitor')
            montrb=1
            call minptb
          elseif(idim.eq.2) then
            call tstamp('>','SIM: setup graphic monitor')
            monitr=1
            call minput
          endif
        endif
      elseif(ino.eq.7)then

C Timestep controller.
        call tstamp('>','SIM: setup timestep controller')
        CALL TSCINP
      elseif(ino.eq.8)then

C Simulate.
C If a convection calculation control file has been specified for any of the
C zones, then force a save level 4, as surface convection can only be reproduced
C in res for the simplified convection calculation approaches.
        SAVE4=.false.
        do 42 ICOMP=1,NCOMP
          if(IHC(ICOMP).EQ.1.and.ISAVE.lt.4)SAVE4=.true.
   42   continue
        if (.not.autook) then
          if(SAVE4.and.ISAVE.ne.4)then
            dok=.true.
            h(1)='A model which includes a (non-standard) convection'
            h(2)='regime must be run with save level 4 (full energy '
            h(3)='balance). You should say yes. '
            CALL ASKOK(' Convection regime file detected.',
     &               ' Change save level to 4 ? ',SAVE4OK,dok,3)
           if(SAVE4OK)ISAVE=4
          endif
        endif

C If CFD is active in any of the zones, then force a save level 4, as surface
C convection can only be reproduced in res for the simplified convection
c calculation approaches.
        do 43 ICOMP=1,NCOMP
          if(NCONF.gt.0) ISAVE=4
   43   continue
        call tstamp('>','SIM: enter simulate')
        call SIMCON
        if(isave.eq.6)then
          call MZOUT6  ! Output from 6 save
        elseif(isave.eq.0)then
          call edisp(iuout,'To see save zero output select option o')
        endif
        call tstamp('>','SIM: exit simulate')
      elseif(ino.eq.10)then

c Delete last zones or plant simulation result set.
        IB=0
        IP=0
        IF(INDCFG.EQ.1)IB=1
        IF(INDCFG.EQ.2)IP=1
        if(INDCFG.EQ.3)then
          CALL EASKAB(' Delete last result set for the: ',
     &    '  ','Zones','Plant',IW,0)
          IBP=IW
          if(IBP.eq.1)IB=1
          if(IBP.eq.2)IP=1
        endif

        call tstamp('>','SIM: delete last result set')
        CALL MZDEL(IB,IP,ISIMLB,ISIMLP)
        IF(IP.EQ.1)then
          IF(ISIMLP.LT.0)GOTO 92
          write(outs,68)ISIMLP
   68     format(' Number of plant result sets remaining = ',I3)
          call edisp(iuout,outs)
        endif
        IF(ISIMLB.LT.0)then

c Solution file does not contain any simulation results
c in the first place.
          call edisp(iuout,' No results saved in library !')
          goto 92
        endif
        write(outs,23)ISIMLB
   23   format(' Number of zone result sets remaining = ',I3)
        call edisp(iuout,outs)
      elseif(ino.eq.12)then

C Simulation toggles - ask for password first.
        H(1)=' This option should be used with care '
        H(2)=' since it can adversely affect the simulation '
        H(3)=' results. Consequently it is password protected. '
        H(4)=' '
        H(5)=' The password is available from ESRU  '
        H(6)='    Email: esru@strath.ac.uk  '
        H(7)='    Tele: +44 141 548 3986  '
        H(8)='    Fax:  +44 141 552 8513  '
        CALL EASKI(IANS,' ',' Password: ',
     &           0,'-',0,'-',0,'Password',IER,8)
        IF(IANS.NE.101)GOTO 92
        call tstamp('>','SIM: enter simulation toggles')
        call SIMTOG
        call tstamp('>','SIM: exit simulation toggles')
      elseif(ino.eq.13)then

C Info on current parameters.
        call tstamp('>','SIM: list simulation parameters')
        CALL MZINFO(IUOUT)
      elseif(ino.eq.14)then

C Output from 0 save (with option to write data to file).
        CALL MZOUTP
      elseif(ino.eq.16)then

C Help.
        CALL PHELPD(' sim control',28,'simul_cmds  ',0,0,IER)
      elseif(ino.eq.17)then

C Return.
        return
      else

C Unknown.
        ino= -1
        goto 92
      endif
      goto 92

      END

C ************ SIMTOG
C Set simulation toggles.
      subroutine simtog
#include "building.h"
#include "geometry.h"
#include "plant.h"
#include "net_flow.h"
#include "UserSimulationToggles.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/AVRAGE/IDAVER
      common/hcthry/ihct,icorexhct
      COMMON/PCEQU/IMPEXP,RATIMP
      COMMON/SOLOFF/ISOLAR
      COMMON/SKYFLG/ISKYFL

      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas
      COMMON/FFN/IFLWN,ICFFS(MPCON)
      COMMON/PITER/MAXITP,PERREL,PERTMP,PERFLX,PERMFL,itrclp,
     &             ICSV(MPNODE,MPVAR),CSVI(MPNODE,MPVAR)
C Tolerance for iteration of hydrogen flow matrix 
      common/PITER_H2/fH2_iteration_tolerance
      real fH2_iteration_tolerance
      COMMON/MFSINC/MFBSNC
      COMMON/MFCALC/IPSMOD,MSLVTP
      COMMON/MFLITR/MAXITF,FERREL,FERMFL,PMAX,STEFFR,MFTRAC,ITER,IOK

      COMMON/CLMFLG/ICLMFL
      COMMON/PMSLV/IPMSLV

      common/spmfxst/ispmxist,spflnam 
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

C Common for electrical file 
      COMMON/ENTFILE/ENTFLNAM,IENTXIST
      INTEGER :: ispmxist,ientxist

      COMMON/GR1D04/GAM,RGAM
      COMMON/GRSD100/IndxSt
      COMMON/MOIST01/MSTROK,MSTRZN(MCOM)
      COMMON/AIRWAT/WATERZ(MCOM),WATCAP
      COMMON/VERSOL/ipkzon(mzs),ipksf(mzs),ivsolfil,vsol,VTSOL,vsnam
      common/filep/ifil
      COMMON/Vld30/Tinitial
      REAL :: Tinitial

C Treatment of solar radiation data in weather file
C ESP-r operates on hourly-centered data. That is, the amount of solar
C irradiance at the recorded hour is the instantaneous irradiance at the
C hour. In Canadian Weather for Energy Calculations (CWEC) files, solar
C radiation is integrated over the previous hour. To account for 
C the half hour shift, a flag has been implemented to indicate
C whether solar radiation data in the weather file is hour-centered
C (default) or half-hour centered.
C The flag can be set in the .cfg file or within bps in the 
C 'simulation toggles' menu.

      common/CWEC_SOL/iSlr_half_hr_flg
      integer iSlr_half_hr_flg     !- flag indicating if solar radiation data in weather file
                                   !- is hour-centered (default) or half-hour centered.
                                   !- 0 = hour-centered; 1 = half-hour centered.

      dimension SALT(7),IVALS(5),IVAL(MCOM),HCALT(15),IHCVAL(15)


c This common block is used to define the sky temperature calculation method
      COMMON/EXLONGWV/iExt_longwave_flg
      integer iExt_longwave_flg    ! 0 = Martin & Berdahl (1984);
                                   ! 1 = Clarke ($7.5.4 JC book, 2nd edition, 2001);
                                   ! 2 = Cole (?);
                                   ! 3 = Czeplak & Kasten (1987);               
                                   ! 4 = Swinbank (clear - uses dry bulb T);                  
                                   ! 5 = Swinbank (clear - uses wet bulb T);       
                                                                                                                                                    
      CHARACTER*72 LAPROB,H,vsnam,lfil,entflnam
      CHARACTER*40 ITEMG(34)
      character SALT*51,spflnam*72,outs*124
      character HCALT*42

      logical MSTROK,MSTRZN
      logical WATERZ,VTSOL
      integer NITMS,INO  ! max items and current menu item


C Make up strings for the current set of toggles.
  92  ino= -4
      if(IDAVER.eq.0)then
        ITEMG(1)=   'a Timestep averaging >> ON              '
      elseif(IDAVER.eq.1)then
        ITEMG(1)=   'a Timestep averaging >> OFF             '
      endif
      WRITE(ITEMG(2),'(A,F5.2)')
     &              'b Bldng eqns implicitness degree: ',fUserGAM
      if(IMPEXP.eq.1)then
        ITEMG(3)=   'c Plant equations    >> implicit        '
      elseif(IMPEXP.eq.2)then
        ITEMG(3)=   'c Plant equations    >> mixed impl/expl '
      elseif(IMPEXP.eq.3)then
        ITEMG(3)=   'c Plant equations    >> time-const depnd'
      elseif(IMPEXP.eq.4)then
        ITEMG(3)=   'c Plant equations    >> steady state    '
      endif
      if(iUserSolar.eq.0)then
        ITEMG(4)=   'd Solar calculations >> processing on   '
      elseif(iUserSolar.eq.1)then
        ITEMG(4)=   'd Solar calculations >> processing off  '
      endif
      if(ISKYFL.eq.-1)then
        ITEMG(5)=   'e Sky radiation method >> Isotropic     '
      elseif(ISKYFL.eq.0)then
        ITEMG(5)=   'e Sky radiation method >> Klucher       '
      elseif(ISKYFL.eq.1)then
        ITEMG(5)=   'e Sky radiation method >> Muneer        '
      elseif(ISKYFL.eq.2)then
        ITEMG(5)=   'e Sky radiation method >> Perez 1987    '
      elseif(ISKYFL.eq.3)then
        ITEMG(5)=   'e Sky radiation method >> Perez 1990    '
      endif
      write(ITEMG(6),'(a32,F7.2)')'f Building initial temperature= ',
     &                          Tinitial
      ITEMG(7)=     '  ________________________________      '
      ITEMG(8)=     'g Mass flow parameters                  '
      if(IPSMOD.eq.1)then
        ITEMG(9)=   'h Mass flow stack press >> sending node'
      elseif(IPSMOD.eq.2)then
        ITEMG(9)=   'h Mass flow stack press >> avg of nodes'
      elseif(IPSMOD.eq.0)then
        ITEMG(9)=   'h Mass flow stack press >> NA          '
      endif
      if(MSLVTP.eq.1)then
        ITEMG(10)=  'i Mass flow solution >> Gaussian        '
      elseif(MSLVTP.eq.2)then
        ITEMG(10)=  'i Mass flow solution >> LU decomposition'
      elseif(MSLVTP.eq.0)then
        ITEMG(10)=  'i Mass flow solution >> NA              '
      endif
      if(MFTRAC.eq.-1)then
        ITEMG(11)=  'j Mass flow trace >> silent             '
      elseif(MFTRAC.eq.0)then
        ITEMG(11)=  'j Mass flow trace >> iterations only    '
      elseif(MFTRAC.eq.1)then
        ITEMG(11)=  'j Mass flow trace >> data on worst node '
      elseif(MFTRAC.eq.2)then
        ITEMG(11)=  'j Mass flow trace >> data on all nodes  '
      elseif(MFTRAC.eq.3)then
        ITEMG(11)=  'j Mass flow trace >> all nodes + solver '
      endif
      if(MFBSNC.eq.0)then
        ITEMG(12)=  'k Mass flow timestep >> 1 per hour      '
      elseif(MFBSNC.eq.1)then
        ITEMG(12)=  'k Mass flow timestep >> 1 per zone_step '
      endif
      ITEMG(13)=    '  ________________________________      '
      ITEMG(14)=    'l                                       '
      if(ICLMFL.eq.0) then
        ITEMG(15)=  'm Climate interpolation >> ON           '
      elseif(ICLMFL.eq.1) then
        ITEMG(15)=  'm Climate interpolation >> OFF          '
      endif
      ITEMG(16)=    'n Plant solution method:                '
      ITEMG(17)=    '  ________________________________      '
      if(ispmxist.eq.0) then
        ITEMG(18)=  'o Special materials >> OFF              '
      elseif(ispmxist.gt.0)then
        ITEMG(18)=  'o Special materials >> ON               '
      endif
      IF(IENTXIST.GT.0) then
        ITEMG(19)=  'p Power flow parameters                 '
      ELSE
        ITEMG(19)=  'p Power flow parameters (NA)            '
      ENDIF
      ITEMG(20)=    '  ________________________________      '
      if(MSTROK)then
        ITEMG(21)=  'r Moisture flow solution parameters     '
      else
        ITEMG(21)=  'r Moisture flow solution parameters (NA)'
      endif
      if(IndxSt.GT.0)then
        ITEMG(22)=  'v Structured mesh soln parameters     '
      else
        ITEMG(22)=  'v Structured mesh soln parameters (NA)'
      endif
      ITEMG(23)=    's Convection methods (inside)           '
      ITEMG(24)=    't Convection methods (external)         '
      ITEMG(25)=    '  ________________________________      '
      ITEMG(26)=    'u Water-filled zones                    '
      ITEMG(27)=    '  ________________________________      '
      ITEMG(28)=    'v Vertical solar radiation              '
      ITEMG(29)=    '  ________________________________      '
      if ( iSlr_half_hr_flg .eq. 0 ) then
        ITEMG(30)=  'w Solar radiation data >> hour centered '
      else
        ITEMG(30)=  'w Solar radiation data >> half-hour cent'
      endif
      if(iExt_longwave_flg.eq.0)then
        ITEMG(31)=   'x Ext. longwave rad >> Martin & Berdahl'
      elseif(iExt_longwave_flg.eq.1)then
        ITEMG(31)=   'x Ext. longwave rad >> Clarke          '
      elseif(iExt_longwave_flg.eq.2)then
        ITEMG(31)=   'x Ext. longwave rad >> Cole            '
      elseif(iExt_longwave_flg.eq.3)then
        ITEMG(31)=   'x Ext. longwave rad >> Czeplak & Kasten'
      elseif(iExt_longwave_flg.eq.4)then
        ITEMG(31)=   'x Ext. longwave rad >>  Swinbank-clear'
      elseif(iExt_longwave_flg.eq.5)then
        ITEMG(31)=   'x Ext. longwave rad >>  Swinbank-bldng'
      endif
      ITEMG(32)=    '  ________________________________      '      
      ITEMG(33)=    '? help                                  '
      ITEMG(34)=    '- exit to simul menu                    '
      NITMS=34

C Help text for this menu.
  91  H(1) ='Most simulations do not require special settings. A set'
      H(2) ='of simulation toggles are available to experts to enable'
      H(3) ='fine tuning of calculations or to invoke special '
      H(4) ='facilities or calculation modes. '
      H(5) =' '
      H(6) ='Toggles which are particularly sensitive are password '
      H(7) ='protected.  Others should be used with care. In all'
      H(8) ='cases a note should be made of which settings have been'
      H(9) ='used in case the assessment needs to be repeated. '

      CALL EMENU(' toggles',ITEMG,NITMS,INO)

      if(INO.eq.1)then

C Averaging on/off.
  126   H(1) ='Bps generates the values of each state variable '
        H(2) ='at each computational time row. The default action '
        H(3) ='is to then transfer the average value for the time-'
        H(4) ='step to the results database. That is, the average '
        H(5) ='of the present and future time row values at each '
        H(6) ='time-step. This option allows the averaging process '
        H(7) ='to be turned off. Note that res could then be used '
        H(8) ='to perform post simulation averaging if required. '
        CALL EASKI(IDAVER,' ',' Results averaging  0) On, 1) Off ? ',
     &    0,'F',1,'F',0,'Results averaging iq 14',IER,8)
        IF(IER.NE.0)GOTO 126
        call tstamp('>','SIM: edit timestep averaging')
      elseif(INO.eq.2)then

C Zone side equations implicitness degree.
        
        CALL EASKR(fUserGAM,
     &         ' ',' Zone side equations implicitness degree: ',
     &         0.,'F',1.,'F',0.5,' implicitness degree',IER,0)
        bUserGAM = .true. 
        call tstamp('>','SIM: edit implicitness degree')
      elseif(INO.eq.3)then

C Plant components state-space equation types.
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
        call tstamp('>','SIM: edit plant equation type')

        IF(IMPEXP.EQ.2.OR.IMPEXP.EQ.3) THEN
          call edisp(iuout,' ')
          call edisp(iuout,' Implicit portion of ratio (0 <= & <= 1)')
          call edisp(iuout,'   0 = fully explicit  ')
          call edisp(iuout,'  .5 = Crank-Nicholson scheme (default)  ')
          call edisp(iuout,'   1 = fully implicit ')
          call edisp(iuout,' ')
          H(1)='Ratio should be in range of 0.0 to 1.0. '
          CALL EASKR(RATIMP,' ',' Ratio : ',
     &       0.0,'F',1.0,'F',0.5,'implicit/explicit ratio',IER,1)
        ENDIF
      elseif(INO.eq.4)then

C Solar ON/OFF option.
  191   H(1) ='To assess the effect of solar radiation on a '
        H(2) ='zone a toggle switch on the simulator solar '
        H(3) ='processing algorithms has been installed. If '
        H(4) ='required solar processing can be switched off. '
        CALL EASKI(iUserSolar,' ',
     &    ' Solar processing  0) On,  1) Off ? ',
     &    0,'F',1,'F',0,'Solar processing iq 23',IER,4)
        bUserSolar = .true.
        IF(IER.NE.0)GOTO 191
        call tstamp('>','SIM: edit solar radiation')
      elseif(INO.eq.5)then

C Set required sky model: Klucher (0) or Muneer (1) etc.
  301   H(1)=' See the user manual for details on sky models.'
        CALL EASKI(ISKYFL,' Sky model: Perez 1990 is default',
     &' -1)Isotropic, 0)Klucher, 1)Muneer, 2)Perez 1987, 3)Perez 1990?',
     &    -1,'F',3,'F',3,'Sky model',IER,1)
        IF(IER.NE.0)goto 301
        call tstamp('>','SIM: edit sky model')
      elseif(INO.eq.6)then

C Building initial temperature.
        CALL EASKR(Tinitial,' ','Building initial temperature: ',
     &         0.,'-',1.,'-',15.0,' temperature ',IER,0)
      elseif(INO.eq.8)then

C Alter mass flow solver iteration parameters, but
C always synchronize corresponding plant solution iteration
C parameters except for MAXITP / MAXITF
        IF(IAIRN.EQ.0.AND.IFLWN.EQ.0) THEN
          call edisp(iuout,
     &      ' No fluid flow file active; menu selection invalid.')
        else
          call tstamp('>','SIM: edit network flow solver parameters')
          CALL MFMODE(1)
          PERREL=FERREL
          PERMFL=FERMFL
        endif
      elseif(INO.eq.9)then

C Use different stack pressure assumptions.?
        IF(IAIRN.EQ.0.AND.IFLWN.EQ.0) THEN
          call edisp(iuout,
     &      ' No fluid flow file active; menu selection invalid.')
        else
          call tstamp('>','SIM: edit network stack assumptions')
          CALL MFMODE(2)
        endif
      elseif(INO.eq.10)then

C Use a different matrix solver?
        IF(IAIRN.EQ.0.AND.IFLWN.EQ.0) THEN
          call edisp(iuout,
     &      ' No fluid flow file active; menu selection invalid.')
        else
          call tstamp('>','SIM: edit network matrix solver')
          CALL MFMODE(3)
        endif
      elseif(INO.eq.11)then

C Mass flow trace.
        IF(IAIRN.EQ.0.AND.IFLWN.EQ.0) THEN
          call edisp(iuout,
     &      ' No fluid flow file active; menu selection invalid.')
        else
          CALL MFMODE(4)
        endif
      elseif(INO.eq.12)then

C Reset fluid flow calculation & zone time-step synchronisation
C This is over-ruled in case of plant fluid simulation
        IF(IAIRN.EQ.0.AND.IFLWN.EQ.0) THEN
          call edisp(iuout,
     &     ' No fluid flow file active: Menu selection invalid.')
        ELSE IF(IFLWN.EQ.1) THEN
          call edisp(iuout,
     &     ' Plant fluid flow simulation active; fluid flow')
          call edisp(iuout,
     &     ' simulation & zone time-step synchronisation')
          call edisp(iuout,' cannot be reset !')
        ELSE
          CALL EASKAB(' Fluid flow simulation: ',' ',
     &      'once per hour','every zone time-step',IW,0)
          MFBSNC=IW-1
        ENDIF
      elseif(INO.eq.14)then

      elseif(INO.eq.15)then
        call tstamp('>','SIM: edit climate interpolation')
        CALL EASKAB(' ',' Climate interpolation: ','On','Off',IW,0)
        ICLMFL=IW-1
      elseif(INO.eq.16)then

C Plant solution technique.
  308   call edisp(iuout,' ')
        call edisp(iuout,' Current plant matrix solver options are:')
        call edisp(iuout,' 1: Compact matrix gaussian elimination with')
        call edisp(iuout,'    back substitution and no pivoting.')
        call edisp(iuout,' 2: LU decomposition with implicit pivoting') 
        call edisp(iuout,'    (Crout).')
        call edisp(iuout,' 3: Gaussian elimination with back sub-')
        call edisp(iuout,'    stitution and no pivoting.')
        call edisp(iuout,' ')
        CALL EASKI(IPMSLV,' ',' Plant solver type ?',
     &    1,'F',3,'F',2,'solver type',IER,0)
        IF(IER.NE.0) GOTO 308

  309   call edisp(iuout,' Some plant component coefficient generators')
        call edisp(iuout,' need to iterate to find an exact solution. ')
        call edisp(iuout,' This process is controlled by following ')
        call edisp(iuout,' parameters:')
        CALL EASKI(MAXITP,' Maximum number of iterations',' : ',
     &    1,'F',0,'-',1,'no iter',IER,0)

        CALL EASKI(itrclp,' No. of iterations for control loops',' : ',
     &    1,'F',0,'-',1,'no iter',IER,0)

        CALL EASKR(PERREL,' Relative error (-) ',' : ',
     &    0.0001,'F',0.,'-',1.,'rel err',IER,0)

        CALL EASKR(PERTMP,' Absolute error temperature (K) ',' : ',
     &    0.0001,'F',0.,'-',1.,'abs err temp',IER,0)

        CALL EASKR(PERFLX,' Absolute error heat flux (W) ',' : ',
     &      0.0001,'F',0.,'-',1.,'abs err flux',IER,0)

        CALL EASKR(PERMFL,' Absolute error mass flow (kg/s) ',' : ',
     &      0.0001,'F',0.,'-',1.,'abs err flux',IER,0)

        CALL EASKR(fH2_iteration_tolerance,
     &       ' Absolute error (H2 mass flow) flow (kg/s) ',' : ',
     &       5.0E-6,'F',0.,'-',1.,'abs err flux',IER,0)

        IF(IER.NE.0) GOTO 309

C Perhaps plant solution iteration parameters are altered, so always
C synchronize corresponding mass flow solver iteration parameters
C except for MAXITP / MAXITF
        FERREL=PERREL
        FERMFL=PERMFL
      elseif(INO.eq.18) then

C Special materials.
        call tstamp('>','SIM: edit special materials toggles')
        call EASKAB(' ','Special facade materials : ','Off','On',IW,0)
        ispmxist=IW-1

C Identify special materials file & call routine to read it.
        if(ispmxist.gt.0)then
          call spminit
          h(1)='This file holds timestep information on the '
          h(2)='performance of special materials. '
          call easks(lfil,' Special Materials output ?',' ',
     &      72,'fort.55','special materials output',IER,2)
          call efopseq(55,lfil,3,IER)
        endif

C Set power flow simulation parameters.
      elseif(INO.eq.19)then
        call tstamp('>','SIM: edit power flow parameters')
        IF(IENTXIST.GT.0) THEN
          CALL ENETPARAM
        ELSE
          CALL EDISP(IUOUT,'No electrical network is associated with ')
          CALL EDISP(IUOUT,'model so you cannot specify electrical ')
          CALL EDISP(IUOUT,'solution parameters.                     ')
        ENDIF
      elseif(INO.eq.21.and.MSTROK)then

C Moisture flow solution parameters.
        call msttog
      elseif(INO.eq.22.AND.IndxSt.GT.0)then

C Building structured mesh relaxation factor.
        CALL STMTOG
      elseif(INO.eq.23)then

C Internal surface hc theory selection. Display as a set of text
C alternatives and allow one (only) to be selected.
        H(1)= 'Several methods are available for calculating internal '
        H(2)= 'surface convection coefficients. The Alamdari & Hammond '
        H(3)= '(BSER&T 1983) correlations for buoyancy-driven flow are '
        H(4)= 'used by default.'
        H(5)= ' '
        H(6)= 'Two sets of the Khalifa & Marshall (Int J of H&M Trans '
        H(7)= '1990) correlations (also know as the `Cardiff` '
        H(8)= 'correlations) are available.  One is applicable for '
        H(9)= 'rooms heated by a radiator located underneath a window. '
        H(10)='The other (labelled `NO radiator`) is applicable for '
        H(11)='rooms with floor heating or with radiators which are '
        H(12)='not located underneath windows.  As Khalifa & Marshall '
        H(13)='did not generate correlations for floors or for stably '
        H(14)='stratified convection from ceilings, the Alamdari & '
        H(15)='Hammond correlations are used for these surfaces. '
        H(16)=' '
        H(17)='The other methods use fixed values for different '
        H(18)='heat flow directions: '
        H(19)='Method              Upward   Downward   Horizontal'
        H(20)='The Halcrow low       1.0      0.1         1.0 '
        H(21)='The Halcrow high      6.0      1.2         6.0 '
        H(22)='CIBSE guide           4.3      1.5         3.0 '
        H(23)='CEN method            5.0      0.7         2.5 '
        H(24)=' '
        H(25)='More advanced control over the convection calculations'
        H(26)='is possible through the user of a convection calculation'
        H(27)='file. Refer to the `convection calculations` menu within'
        H(28)='the Project Manager for guidance.'

        SALT(1)='Alamdari and Hammond (default) '
        SALT(2)='Khalifa & Marshall, radiator located under window  '
        SALT(3)='Khalifa & Marshall, NO radiator under window  '
        SALT(4)='Halcrow, time invariant low    '
        SALT(5)='Halcrow, time invariant high   '
        SALT(6)='CIBSE guide, time invariant    '
        SALT(7)='CEN simplified calc method     '
        IX=1
        CALL EPICKS(IX,IVALS,' ','Internal surface convection model:',
     &         51,7,SALT,'hc model',IER,28)
        IF(IX.EQ.0)goto 92
        call tstamp('>','SIM: edit surface convection model')
        ihct=IVALS(1)
      elseif(INO.eq.24)then

C External hc correlations:
C  icorhc = 1 then use the default method,
C  icorhc = 2 then use MoWiTT model (Yazdanian & Klems 1994)
C  icorhc = 3 Aya Hagishima correlation ( ),
C  icorhc = 4 Ya Liu (roof wind speed) ( ),
C  icorhc = 5 Ya Lui (raw wind speed) ( ),
C  icorhc = 6 Loveday (roof wind speed) ( ),
C  icorhc = 7 Loveday (raw wind speed) ( ),
C  icorhc = 8 CIBSE guide ( ),
C  icorhc = 9 ASHRAE Task Group ( ),
C  icorhc = 10 Sturrock ( ),
C  icorhc = 11 Keith Nicol ( ),
C  icorhc = 12 S.E .G.Jayamaha ( ).
        H(1)='You have the option to use various hc correlation'
        H(2)='at the exterior face of surfaces:'
        H(3)=' '
        H(4)=' = 1 then use the default method,'
        H(5)=' = 2 then use MoWiTT model (Yazdanian & Klems 1994)'
        H(6)=' = 3 Aya Hagishima correlation ( ),'
        H(7)=' = 4 Ya Liu (roof wind speed) ( ),'
        H(8)=' = 5 Ya Lui (raw wind speed) ( ),'
        H(8)=' = 6 Ya Lui (weather station wind speed) ( ),'
        H(9)=' = 7 Loveday (roof wind speed) ( ),'
        H(10)=' = 8 Loveday (raw wind speed) ( ),'
        H(11)=' = 9 CIBSE guide ( ),'
        H(12)=' = 10 Loveday (mixed hc) ( ),'
        H(13)=' = 11 British Standard (BS),'
        H(14)=' = 12 ASHRAE Task Group ( ),'
        H(15)=' = 13 Sturrock ( ),'
        H(16)=' = 14 Keith Nicol ( ),'
        H(17)=' = 15 S.E.G. Jayamaha ( ).'
        H(18)=' '
        H(19)='In the above roof wind speed denotes: '
        H(20)=' '
        H(21)='In the above raw wind speed denotes: '
        H(22)=' '
        icorhc=icorexhct
        HCALT(1)='the default ESP-r method'
        HCALT(2)='MoWiTT low rise hc '
        HCALT(3)='Aya Hagishima hc '
        HCALT(4)='Ya Liu (roof wind speed) hc'
        HCALT(5)='Ya Lui (raw wind speed) hc'
        HCALT(6)='Ya Lui (weather station wind speed) hc'
        HCALT(7)='Loveday (roof wind speed) hc'
        HCALT(8)='Loveday (raw wind speed) hc'
        HCALT(9)='CIBSE guide hc'
        HCALT(10)='Loveday (mixed) hc'
        HCALT(11)='British Standard'
        HCALT(12)='ASHRAE Task Group hc'
        HCALT(13)='Sturrock hc'
        HCALT(14)='Keith Nicol hc'
        HCALT(15)='S.E.G. Jayamaha hc'
        IX=1
        CALL EPICKS(IX,IHCVAL,' ','External surface convection model:',
     &         42,15,HCALT,'ext hc model',IER,22)
        IF(IX.EQ.0)goto 92
        call tstamp('>','SIM: edit ext surface convection model')
        icorexhct=IHCVAL(1)
C        CALL EASKABC( 'External hc correlation:',' ','default hc',
C     &   'MoWiTT low rise hc','continue',ichc,2)
C        if(ichc.eq.1)then
C          icorexhct = 1
C        elseif(ichc.eq.2)then
C          icorexhct = 2
C        else
C          continue
C        endif
      elseif(INO.eq.26)then

C Select air (default) or water filled zones.
        H(1)=' Pick one, several or all zones for inclusion'
        INPIC=NCOMP
        CALL EPICKS(INPIC,IVAL,' ',' Which zones to include: ',
     &      12,NCOMP,zname,' zone list',IER,1)
        if(inpic.eq.0)goto 92
        call tstamp('>','SIM: edit water filled zones')
        do 142 ixx=1,inpic
          if(ival(ixx).ne.0)then
            WATERZ(IVAL(ixx))=.true.
          endif
 142    continue
        call edisp(iuout,' ')
        call edisp(iuout,' Enter capacity of fluid in kJ/m3K ')
        call edisp(iuout,' (4190.0 kJ/m3K for water at 20degC)  ')
        call edisp(iuout,' ')
        H(1)='Capacity should be in range of 1.0 to 5000.0. '
        H(2)='Zones not included in list will have capacity of air.'
        CALL EASKR(WATCAP,' ',' Capacity : ',
     &     1.0,'F',5000.0,'F',4190.0,'fluid capacity',IER,2)

      elseif(INO.eq.28)then

C Read in global vertical radiation from file.
        vtsol=.true.
        do 143 iiii=1,mzs
          H(1)=' Pick one zone at a time for inclusion'
          inpic=1
          call epicks(inpic,IVAL,' ',' Which zone : ',
     &        12,ncomp,zname,' zone list',IER,1)
          if(inpic.eq.0)goto 144
          ipkzon(iiii)=IVAL(1)
          call EASKSUR(ipkzon(iiii),ipksf(iiii),'-',
     &      'Select surface to associate with radiation.',' ',IER)
  143   continue
  144   if (inpic.eq.0.and.iiii.eq.1) then
           vtsol=.false.
           goto 92
        endif
        ivsolfil=ifil+31
        call erpfree(ivsolfil,istat)
        call edisp(iuout,' ')
        call edisp(iuout,'Enter filename of vertical solar data. ')
        call edisp(iuout,' ')
        H(1)='Data should be in an ASCII file.'
        H(2)='One column.'
        H(3)='One row per timestep including start-up period.'
        call easks(vsnam,' Vertical Solar file ?',
     &' ',72,' ','vertical solar file',IER,3)
        if(IER.NE.0) then
          write(outs,'(a,a)') 'Could not find file ',vsnam
          call edisp(iuout,outs)      
          vtsol=.false.
          goto 92
        endif
        call efopseq(ivsolfil,vsnam,1,IER)
        if(IER.NE.0) then
          write(outs,'(a,a)') 'Error opening file ',vsnam
          call edisp(iuout,outs)
          vtsol=.false.
          goto 92
        endif
        call tstamp('>','SIM: edit vertical solar file')


      elseif(INO.eq.30)then
C Determine if solar radiation data is to be treated as half-hour
C centered.

        H(1) = 'In CWEC weather files, solar radiation data for a '
        H(2) = 'given hour are the values integrated over the previous '
        H(3) = 'hour. For example, the values that correspond to 9h00 '
        H(4) = 'represent the amount of solar radiation received '
        H(5) = 'between 8h00 and 9h00. By default, ESP-r assumes that '
        H(6) = 'the solar radiation data at a given hour are the '
        H(7) = 'instantaneous values  at the hour (hourly-centered '
        H(8) = 'values).Thus, by default, there is a half-hour shift '
        H(9) = 'in the solar radiation values. This can be corrected '
        H(10)= 'by specifying that the solar radiation data in the '
        H(11)= 'weather file are half-hour centered.'

        call easki(iSlr_half_hr_flg,' Solar data 0) hour-centered, ',
     &    ' 1) half-hour centered ? ',0,'F',1,'F',0,'solar data',IER,11)

      elseif(INO.eq.31)then
C Determine if solar radiation data is to be treated as half-hour
C centered.

        H(1) = 'At the moment there are 5 available calculations '
        H(2) = 'of the sky temperature to be used for the external '
        H(3) = 'longwave radiation calculations. The default used '
        H(4) = 'in ESP-r is Martin & Berdahl. A useful reference is: '
        H(5) = ' Martin & Berdahl, 1984. "Characteristics of infrared '
        H(6) = 'sky radiation in the United States". Solar Energy, '
        H(7) = 'Vol. 33, pp. 321-336.'

        call easki(iExt_longwave_flg,
     &    ' External longwave calculation 0) Martin & Berdahl, ',
     &    ' 1) Clarke, 2) Cole, 3) Czeplak & Kasten, 4) Swinbank-clear, 
     &5) Swinbank-bldng',
     &    0,'F',5,'F',0,'ext lngwave method',IER,7)  

      elseif(INO.eq.(NITMS-1))then

C Help.
        CALL PHELPD(' sim toggles',9,'simul_toggl ',0,0,IER)
      elseif(INO.eq.NITMS)then
        return
      else
        ino = -1
        goto 92
      endif
      ino = -4
      goto 92

      end

C ************ MSTTOG
C Set moisture flow toggles.
      subroutine msttog

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/MOIST11/MITRM,RITRMP,RITRMF
      COMMON/MOIST12/GAMm,RGAMm
      COMMON/MOIST13/RLXln,RRLXln,RLXlnF,RRLXlnF,RLXlnT,RRLXlnT
      COMMON/MOIST14/DTfalse,TRMfls,TRMflsp1

      CHARACTER H*72,ITEMG(11)*40 
      integer NITMS,INO  ! max items and current menu item

  10  INO=-4
      WRITE(ITEMG(1),'(A,F5.2)')
     &              'a Implicitness degree        :',GAMm
      WRITE(ITEMG(2),'(A,F5.2)')
     &              'b Pressure linear relaxation :',RLXln
      WRITE(ITEMG(3),'(A,F5.2)')
     &              'c Fluid linear relaxation    :',RLXlnF
      WRITE(ITEMG(4),'(A,F5.2)')
     &              'd Temp. linear relaxation    :',RLXlnT
      WRITE(ITEMG(5),'(A,F7.1)')
     &              'e False time step relaxation :',DTfalse
      WRITE(ITEMG(6),'(A,I5)')
     &              'f Maximum iterations         :',MITRm
      WRITE(ITEMG(7),'(A,F5.2)')
     &              'g Pressure stopping criterion:',RITRMp
      WRITE(ITEMG(8),'(A,F8.5)')
     &              'h Fluid stopping criterion   :',RITRMf
      ITEMG(9)=     '  ________________________________      '
      ITEMG(10)=     '? help                                  '
      ITEMG(11)=    '- exit to simulation toggles menu       '
      NITMS=11

C Help text for this menu.
      H(1) ='This menu allows defining the moisture solution'
      H(2) ='and convergence parameters.'
      H(3) =' '

      CALL EMENU('Moisture flow solution parameters',ITEMG,NITMS,INO)
      if(INO.eq.1)then

C Moisture flow equations implicitness degree.
        CALL EASKR(GAMm,' ','implicitness degree:',
     &              0.,'F',1.,'F',0.5,'implicitness degree',IER,0)
      elseif(INO.eq.2)then

C Moisture flow equations pressure linear relaxation factor.
        CALL EASKR(RLXln,' ','pressure linear relaxation factor:',
     &               0.,'F',2.,'F',0.1,'relaxation factor',IER,0)
      elseif(INO.eq.3)then

C Moisture flow equations fluid linear relaxation factor.
        CALL EASKR(RLXlnF,' ','fluid linear relaxation factor:',
     &               0.,'F',2.,'F',0.1,'relaxation factor',IER,0)
      elseif(INO.eq.4)then

C Temperature linear relaxation factor.
        CALL EASKR(RLXlnT,' ','temp. linear relaxation factor:',
     &               0.,'F',2.,'F',0.1,'relaxation factor',IER,0)
      elseif(INO.eq.5)then

C Moisture flow equations false time step relaxation factor.
        CALL EASKR(DTfalse,' ','false time step:',
     &               0.,'F',3600.,'-',3600.,'false time step',IER,0)
      elseif(INO.eq.6)then

C Maximum iterations moisture flow equations.
        CALL EASKI(MITRm,' ','Maximum iterations:',
     &                   2,'F',1000,'W',200,'Maximum iterations',IER,0)
      elseif(INO.eq.7)then

C Pressure stopping criterion moisture flow equations.
        CALL EASKR(RITRMp,' ','Pressure stopping criterion:',
     &    0.01,'W',10.,'W',0.1,'Pressure stopping criterion',IER,0)
      elseif(INO.eq.8)then

C Fluid stopping criterion moisture flow equations.
        CALL EASKR(RITRMf,' ','Fluid stopping criterion:',
     &    0.00001,'W',1.0,'W',0.001,'Fluid stopping criterion',IER,0)
      elseif(INO.eq.(NITMS-1))then

C Help.
        CALL PHELPD('moisture toggles',3,'-',0,0,IER)
      elseif(INO.eq.NITMS)then

C Return to simulation toggles menu.
        return
      endif

C Redisplay the moisture flow solution parameters menu.
      goto 10
      end

C ************ STMTOG
C Set building construction structured mesh toggles.
      subroutine STMTOG

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/GRSD120/RLnrSD,RRLnrSD
      COMMON/GRSD121/InSSlv,MSItrn,TSCrSM,QSCrSM
      COMMON/GRSD122/InSvST,ISTemp

      CHARACTER H*72,ITEMG(10)*40 
      integer NITMS,INO  ! max items and current menu item

   10 INO=-4
      IF(InSSlv.EQ.1)THEN
        ITEMG(1)=   'a Structured mesh solver >> direct'
        ITEMG(2)=   'b linear relaxation factor : (N/A)'
        ITEMG(3)=   'c Maximum iterations       : (N/A)'
        ITEMG(4)=   'd Temp. stopping criterion : (N/A)'
        ITEMG(5)=   'e Plant stopping criterion : (N/A)'
      ELSE
        ITEMG(1)=   'a Structured mesh solver >> iterative'
        WRITE(ITEMG(2),'(A,F5.2)')
     &              'b linear relaxation factor :',RLnrSD
        WRITE(ITEMG(3),'(A,I5)')
     &              'c Maximum iterations       :',MSItrn
        WRITE(ITEMG(4),'(A,F8.5)')
     &              'd Temp. stopping criterion :',TSCrSM
        WRITE(ITEMG(5),'(A,F8.5)')
     &              'e Plant stopping criterion :',QSCrSM
      ENDIF
      ITEMG(6)=     '  ________________________________      '
      IF(InSvST.EQ.0)THEN
        ITEMG(7)=     's Save mesh temperatures >> OFF '
      ELSE
        ITEMG(7)=     's Save mesh temperatures >> dump3d'
      ENDIF
      ITEMG(8)=     '  ________________________________      '
      ITEMG(9)=     '? help                                  '
      ITEMG(10)=    '- exit to simulation toggles menu       '
      NITMS=10

C Help text for this menu.
      H(1) ='This menu allows defining the structured mesh'
      H(2) ='solution and convergence parameters.'
      H(3) =' '

      CALL EMENU('structured mesh solution parameters',ITEMG,NITMS,INO)

C Solver index.
      if(INO.eq.1)then
        IF(InSSlv.EQ.1)THEN
          InSSlv=2
        ELSE
          InSSlv=1
        ENDIF
      elseif(INO.eq.2.AND.InSSlv.EQ.2)then

C linear relaxation factor.
        CALL EASKR(RLnrSD,' ','structured mesh relaxation factor:',
     &               0.,'F',2.,'F',1.0,'relaxation factor',IER,0)
      elseif(INO.eq.3.AND.InSSlv.EQ.2)then

C Maximum iterations.
        CALL EASKI(MSItrn,' ','Maximum iterations:',
     &                   2,'F',1000,'W',200,'Maximum iterations',IER,0)
      elseif(INO.eq.4.AND.InSSlv.EQ.2)then

C Temperature stopping criterion.
        CALL EASKR(TSCrSM,' ','Temperature stopping criterion:',
     &    0.000001,'W',1.,'W',0.00001,'stopping criterion',IER,0)
      elseif(INO.eq.5.AND.InSSlv.EQ.2)then

C Plant power stopping criterion.
        CALL EASKR(TSCrSM,' ','Plant power stopping criterion:',
     &    0.00001,'W',10.,'W',0.1,'stopping criterion',IER,0)
      elseif(INO.eq.7)then

C Save structured mesh temperature.
        if(InSvST.EQ.0)then
          InSvST=1
        else
          InSvST=0
        endif
      elseif(INO.eq.(NITMS-1))then

C Help.
        CALL PHELPD(' structured mesh toggles',3,'-',0,0,IER)
      elseif(INO.eq.NITMS)then

C Return to simulation toggles menu.
        return
      endif

C Redisplay the moisture flow solution parameters menu.
      goto 10
      end

C ******************** MZSPER ********************

C MZSPER allows user definition of the simulation
C period (in terms of start and finish day and month
C numbers) and the computational time step (measured
C as a fraction of and including one hour).

C Common block variables are:

C ISD1 & ISM1  - start day and month numbers.
C ISD2 & ISM2  - finish day and month numbers.
C ISDS & ISDF  - start and finish year day numbers.
C NTSTEP       - number of computational time-steps
C                within each hour.
C TIMSEC       - length of time increment in seconds

      SUBROUTINE MZSPER
#include "building.h"
#include "MultiYear_simulations.h"
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/PCTIME/TIMSEC
      COMMON/PCTSTP/NTSTPP
      COMMON/C6/INDCFG
      COMMON/MOIST14/DTfalse,TRMfls,TRMflsp1
      COMMON/PREC7/ITCNST
      
C Active domain common.
      common/ACTDOM/CFDOK

C Once per hour storage of results, averaged over that hour.
      common/rdcgen/irdact

      COMMON/SPFL/spfileok,perok,tstepok,saveok,autook,exitok,startupok
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh

      character outs*124,H*72

      LOGICAL spfileok,perok,tstepok,saveok,autook,exitok,startupok
      logical hri,defok,OK
      logical CFDOK

C Request simulation period in user defined format of d-o-y
C or day and month, return julian days and convert to month
C and day.
  14  if(.NOT.perok) then

C If period has not been defined, assign defaults        
        isds=9
        isdf=15
        if ( bMY_sim_enabled ) then
           iMY_start_year = 2000
           iMY_end_year   = 2000
        endif     
      else

C If period has been defined get start & end days
        CALL EDAY(isd1,ism1,isds)
        CALL EDAY(isd2,ism2,isdf)

C Adjust end date for multi-year simulation
        if ( bMY_sim_enabled ) then           
          isdf = isdf + 365 * ( iMY_end_year - iMY_start_year )         
        endif
        
      endif
    8 if(.not.autook) then
        if ( bMY_sim_enabled ) then
C Multi-year simulation prompt    
          call eAskPerYear('Simulation period:',isds,isdf,
     &                   iMY_start_year,iMY_end_year,ifday,ier)
        else
C Single year simulation prompt
          call eAskPer('Simulation period:',isds,isdf,ifday,ier)
        endif ! <- matches if ( bMY_sim_enabled )...
        IF(IER.NE.0) GOTO 14
      endif ! <- matches if (.not. autook)     
      call edayr(isds,isd1,ism1)
      call edayr(ISDF - 365*(iMY_end_year - iMY_start_year), ISD2,ISM2)

C Check start day before finish day.
      IF(ISDS.LE.ISDF)goto 16
      CALL edisp(iuout,' Start/Finish dates out of order. ')
      if ( .not. autook ) goto 8

C Define startup period.
 16   if ( .not.autook ) then
         
         H(1)='The start-up period is one or more days preceding'
         H(2)='the user defined simulation period. The results for' 
         H(3)='this start-up period are not transferred to the'
         H(4)='results library and so cannot be analysed. '  
         H(5)='  '
         H(6)='Note that during a single year simulation, a start-up'
         H(7)='period prior to January 1 will utilise climatic data'
         H(8)='from the end of the same year.'
         CALL EASKI(ITCNST,' Start-up period duration:',
     &                     ' Number of days ? ',
     &                     0,'F',364,'F',1,'startup period',IER,8)
      endif

C Define computational time-step.
      if (.not.autook) then
        if(.not.tstepok) NTSTEP=1
        IF(INDCFG.EQ.2)goto 31
        h(1)=' The zone-side equations will be established'
        h(2)=' and solved this many times per hour. A 2/hour'
        h(3)=' scheme is usually sufficient, although smaller'
        h(4)=' time-steps may be required for some control'
        h(5)=' regimes to permit frequent control decision points.'
        mtsphr=60
        call easki(ntstep,' ',' Zone side: time-steps/hour ?',
     &    1,'F',mtsphr,'F',1,'time-steps/hour?',ier,5)
      endif

C MZCOE2 called to ensure that coefficient values are
C updated according to the selected time step.
      TSTEP=1./FLOAT(NTSTEP)
      CALL MZCOE2(TSTEP)
      DTfalse=TSTEP*3600.0

   31 IF (INDCFG.ne.1) then

C Define plant time-step. Plant can operate at a different
C frequency to the zones. It assumes the zones constant
C during the plant simulation.
        RMIN=60.0/NTSTEP
        write(outs,22)RMIN
   22   format(' Zone time-step is',F6.2,' minutes')
        call edisp(iuout,outs)
        call edisp(iuout,' ')

C If not in automatic mode and plant timestep is known then
        if (.not.autook) then
          if(.not.tstepok)NTSTPP=4
          h(1)=' The plant-side equations will be established'
          h(2)=' and solved this many times per hour. A 4/hour'
          h(3)=' scheme is usually sufficient, although smaller'
          h(4)=' time-steps may be required for some control'
          h(5)=' regimes to permit frequent control decision points.'

C Minimum allowable time-step within specified period is 0.01 seconds.
          call easki(NTSTPP,' ',
     &      ' Plant side: time-steps/building time-step ?',1,'F',
     &      360000,'F',4,'plnt time-steps/building time-step',ier,5)

C Calculate the length of the time increment in seconds.
          TIMSEC=3600.0/(FLOAT(NTSTEP)*FLOAT(NTSTPP))
        endif
      endif

C CFD simulation period.
      if (CFDOK) then
        call askaboutcfd
      endif

C Set, once per hour storage of results, averaged over that hour.
      if(ntstep.gt.1.or.(indcfg.ne.1.and.ntstep*ntstpp.gt.1)) then
        if (.not.autook) then
          H(1) ='The simulator calculates the value of each state'
          H(2) ='variable at each computational time step. The default'
          H(3) ='action is to transfer each value at each time'
          H(4) ='step to the results files(s).'
          H(5) =' '
          H(6) ='This option allows to average results over one hour'
          H(7) ='before transfer to the results file(s), thus'
          H(8) ='decreasing the size of results files(s). Subsequent'
          H(9) ='invocations of the results analysis module will'
          H(10)='be the same as if the simulation timestep was one'
          H(11)='per hour.'
          defok=.false.
          hri=.false.
          call askok(' Hourly results integration: ',' ',hri,defok,11)
          if(hri)then
            irdact=1
          else
            irdact=0
          endif
        else

C Check against user preference in simulation parameters.
          if(isavgh.eq.1)then
            irdact=1
          else
            irdact=0
          endif
        endif
      else
        irdact=0
      endif

C Debug.
C      write(6,*) 'in mzper irdact & isavgh are ',irdact,isavgh 

      RETURN
      END

C ******************* askaboutcfd ****************
C askaboutcfd confirms simulation periods for CFD assessments.
      subroutine askaboutcfd

#include "building.h"
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      
C Active domain common.
      common/ACTDOM/CFDOK

C CFD commons.
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/CFSEUP/CFTIMS,CFTIMF,ICFDYS,ICFDYF,ICFAUX(MCOM)
      character outs*124,H*72,LCFD*72
      logical OK,dok
      logical CFDOK

C CFD simulation period.
      if (CFDOK) then
        dok=.false.
        h(1)='Including CFD in the startup period as well as during'
        h(2)='the whole simulation will be SLOW! '
        CALL ASKOK(' Do you want activate the CFD model during the',
     &             'whole simulation ?',OK,dok,2)
        IF (OK) RETURN

        dok=.false.
        h(1)='Including CFD in the startup period may cause delay'
        h(2)='with only a limited affect on predictions. '
        CALL ASKOK(' Do you want activate the CFD model during the',
     &             'start-up period ?',OK,dok,2)
        IF (.not.OK) then
          do 20 IC=1,NCOMP
            IFCFD(IC)=-IFCFD(IC)
 20       continue
        endif

C Period has been defined get start & end days, but since isds and isdf
C would have been defined in a separate call to MZPER make sure these
C values have been set.
        CALL EDAY(isd1,ism1,isds)
        CALL EDAY(isd2,ism2,isdf)

C Debug.
C        write(6,*) isd1,ism1,isds,isd2,ism2,isdf

        IFDAY=1
        ICFDYS=ISDS
        ICFDYF=ISDF
  40    call EASKPER('Period when CFD is active:',
     &                                       ICFDYS,ICFDYF,IFDAY,IER)
        if (ICFDYF.lt.ICFDYS) then
          call edisp(iuout,' Start/Finish days out of order.')
          goto 40
        endif

        CFTIMS=0.0
        CFTIMF=24.0
  50    write(outs,'(a)')'For the first day CFD is active specify'
       CALL EASKR(CFTIMS,outs,'starting hour ',
     &             0.0,'F',24.,'F',0.0,'Decimal fraction',IER,1)
        write(outs,'(a)')'For the last day CFD is active specify'
        CALL EASKR(CFTIMF,outs,'finishing hour ',
     &            0.0,'F',24.,'F',24.0,'Decimal fraction',IER,1)
        if ((ICFDYF.eq.ICFDYS).and.(CFTIMF.lt.CFTIMS)) then
          call edisp(iuout,' Start/Finish hours out of order.')
          goto 50
        endif
      
        DO 30 IC=1,NCOMP
          ICFAUX(IC)=IFCFD(IC)
          IFCFD(IC)=0
 30     CONTINUE
      endif
      return
      end


C ******************** MZSIMH ********************
C MZSIMH outputs simulation period and wait time information
C prior to each simulation commencing.

      SUBROUTINE MZSIMH(ICON)
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
#include "control.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/SHOUT/ICOUT
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      
      COMMON/C1/NCOMP,NCON
      integer ncomp,ncon
      COMMON/PREC7/ITCNST

      COMMON/C6/INDCFG

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/PCTIME/TIMSEC
      COMMON/PCTSTP/NTSTPP
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      COMMON/SAVE/ISAVE
      COMMON/LIBREC/IRECPL
      COMMON/LIBRCP/IRCPLP

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)

      COMMON/CCTLNM/CTLDOC,LCTLF
      COMMON/AVRAGE/IDAVER
      COMMON/BEFORM/IBEQNF
      COMMON/PCEQU/IMPEXP,RATIMP
      COMMON/SOLOFF/ISOLAR

      COMMON/WARN/IWARN
      common/C21/IFCFG,cfgroot,LCFGF
      INTEGER :: IFCFG

C TSC data.
      COMMON/TS/ITSFLG,NTSCPD,ITSSCH(MSCH,5),itsmus

      COMMON/VTHP30/ILTHPS,ILTHPZ(MCOM)
      COMMON/VTHP31/INTHPS,INTHPZ(MCOM)
      COMMON/GR1D04/GAM,RGAM
      COMMON/GR1D06/IGR1D
      COMMON/GR3D100/BLDG3D,ZONE3D(MCOM)
      LOGICAL :: BLDG3D,ZONE3D
      COMMON/GRND100/GRND3D
      COMMON/MOIST01/MSTROK,MSTRZN(MCOM)
      COMMON/GRSD100/IndxSt
      common/glbctl/global,glbrwd

C Once per hour storage of results, averaged over that hour.
      common/rdcgen/irdact

C Uncertainty.
      COMMON/UA5/IMET,ISIM,NRSIM
      INTEGER :: imet,isim,nrsim

C Simulator parameters.
      COMMON/SPFL/spfileok,perok,tstepok,saveok,autook,exitok,startupok
      LOGICAL     spfileok,perok,tstepok,saveok,autook,exitok,startupok

      CHARACTER ctldoc*248
      CHARACTER*72 LCFGF,LCTLF
      character PERST1*14,PERST2*44,PERST3*44
      character outs*124,louts*248,cfgroot*24,h*72

      LOGICAL OK,dok,IGR1D,GRND3D,MSTROK,MSTRZN
      LOGICAL ILTHPS,ILTHPZ,INTHPS,INTHPZ,global,glbrwd

      ISAV1=ISAVE+1

      IDAYS=ISDF-ISDS+1

C If multiple simulations then skip reporting (its the same as 
C the first time).
      IF (ISIM.gt.1) goto 6

C Calculate number of words required by solution file.
C The record length of the results file is MS+5 words.
c Added a 5th computed goto for save level 5
      goto (41,8,9,11,11,41),ISAV1
    8 if(irdact.ne.0)then
        NWORD=(IDAYS*irdact*NCOMP*24*1+IRECPL)*(MS+5)
      else
        NWORD=(IDAYS*NTSTEP*NCOMP*24*1+IRECPL)*(MS+5)
      endif
      goto 12
    9 if(irdact.ne.0)then
        NWORD=(IDAYS*irdact*NCOMP*24*3+IRECPL)*(MS+5)
      else
        NWORD=(IDAYS*NTSTEP*NCOMP*24*3+IRECPL)*(MS+5)
      endif
      goto 12
   11 NREC=0
      DO 10 I=1,NCOMP
        NREC=NREC+3
        DO 20 J=1,NCONST(I)
          NREC=NREC+1
   20   CONTINUE
   10 CONTINUE

C << with wider record width is MS+5 still ok for file size prediction ?? >>
      if(irdact.ne.0)then
        NWORD=(IDAYS*irdact*NREC*24+IRECPL)*(MS+5)
      else
        NWORD=(IDAYS*NTSTEP*NREC*24+IRECPL)*(MS+5)
      endif

C There are 4 bytes in a word therefore the number
C of kilobytes required by the solution file is .....
   12 BYTE=FLOAT(NWORD)*4.0/1024.0

C Determine the number of zone days simulated.
   41 NZTS=(NCOMP*(IDAYS+ITCNST))*NTSTEP*24

C Write out appropriate message.
      write(louts,'(a,a)')  ' Climate file         : ',
     &  LCLIM(:lnblnk(LCLIM))
      call edisp248(icout,louts,100)
      write(outs,'(a,a)')   ' Configuration file   : ',
     &  LCFGF(:lnblnk(LCFGF))
      call edisp(icout,outs)
      write(outs,'(a,a)')   ' Configuration name   : ',
     &  modeltitle(:lnblnk(modeltitle))
      call edisp(icout,outs)
      call edisp(icout,' ')
      if(NCF.eq.0.and.NCL.eq.0)then
        call edisp(icout,   ' Control strategy     : free-floating')
        call edisp(icout,' ')
      else
        write(outs,'(a,a)') ' Control file name    : ',
     &    LCTLF(1:lnblnk(LCTLF))
        call edisp(icout,outs)
        call edisp(icout,' ')
      endif

      if(isave.eq.0)then
        call edisp(icout,' Zone save option     : 0 (No db created)')
      elseif(isave.eq.1)then
        call edisp(icout,' Zone save option     : 1 (Minimum)')
      elseif(isave.eq.2)then
        call edisp(icout,' Zone save option     : 2 (Moderate)')
      elseif(isave.eq.3)then
        call edisp(icout,' Zone save option     : 3 (Node temps)')
      elseif(isave.eq.4)then
        call edisp(icout,' Zone save option     : 4 (Energy Balance)')
c Hot 3000: Added save level 5 zone save level info
      elseif(isave.eq.5)then
        call edisp(icout,' Zone save option     : 5 H3K')
      elseif(isave.eq.6)then
        call edisp(icout,' Zone save option     : 6 (No db created)')
      endif
      write(outs,'(a,I5)') ' No. of warnings      : ',IWARN
      call edisp(icout,outs)
      call edisp(icout,' ')

      NPTS=NTSTEP*NTSTPP
      TMIN=60.0/NTSTEP
      TMINP=TIMSEC/60.
      write(outs,'(a,I5,a)')' Simulation period    : ',IDAYS,' day(s)'
      call edisp(icout,outs)

C Derive string for simulation period.
      CALL EPERSTR(IYEAR,ISDS,1,ISDF,24,1,2,0,PERST1,PERST2,PERST3,IER)
      write(outs,'(16X,A44)')PERST2
      call edisp(icout,outs)
      write(outs,'(a,I5,a)')' Start-up period      : ',ITCNST,' day(s)'
      call edisp(icout,outs)

      IF(INDCFG.EQ.2)goto 131
      if(irdact.eq.0)then
        write(outs,'(a,I6,a,F5.2,a)')' Zone time-steps      :',NTSTEP,
     &    ' / hr ( ',TMIN,' min. intervals saved at each timestep)'
      else
        write(outs,'(a,I6,a,F5.2,a)')' Zone time-steps      :',NTSTEP,
     &    ' / hr ( ',TMIN,' min. intervals saved once per hour)'
      endif 
      call edisp(icout,outs)
      write(outs,'(a,I5)') ' Number of zones      : ',NCOMP
      call edisp(icout,outs)
      write(outs,'(a,I6)') ' Zone-time increments :',NZTS
      call edisp(icout,outs)

      IF(INDCFG.EQ.1)goto 133
  131 write(outs,'(a,I5,a,F6.2,a)')' Plant time-steps     : ',NPTS,
     &  ' / hr ( ',TMINP,' min. intervals)'
      call edisp(icout,outs)
  133 IF(INDCFG.EQ.2)goto 135
      IF(ISAVE.GT.0)goto 42
      call edisp(icout,    ' Zone results db size : Not applicable')
      goto 135
   42 IF(BYTE.GT.2000.0)goto 14
      write(outs,'(a,F7.1,a)')' Zone results db size : ',BYTE,' Kbytes'
      if(isave.eq.1.or.isave.eq.2.or.isave.eq.3.or.isave.eq.4)then
        call edisp(icout,outs)
      endif
      goto 135
   14 BYTE=BYTE/1024.0
      write(outs,'(a,F7.1,a)')' Zone results db size : ',BYTE,' Mbytes'
      if(isave.eq.1.or.isave.eq.2.or.isave.eq.3.or.isave.eq.4)then
        call edisp(icout,outs)
      endif

  135 IF(INDCFG.EQ.1)goto 136
      NREC=3
      if(irdact.ne.0)then
        NWORD=(IDAYS*irdact*NREC*24+IRCPLP)*(MS+5)
      else
        NWORD=(IDAYS*NPTS*NREC*24+IRCPLP)*(MS+5)
      endif
      BYTE=FLOAT(NWORD)*4.0/1024.0
      IF(BYTE.GT.2000.0)goto 137
      write(outs,'(a,F7.1,a)')' Plant results db size: ',BYTE,' Kbytes'
      call edisp(icout,outs)
      goto 136
  137 BYTE=BYTE/1024.0
      write(outs,'(a,F7.1,a)')' Plant results db size: ',BYTE,' Mbytes'
      call edisp(icout,outs)

C Time-step controller status.
  136 IF(ITSFLG.EQ.0) THEN
         call edisp(icout, ' Time-step controller :     Not active')
      ELSE
         call edisp(icout, ' Time-step controller :     Active')
      ENDIF

C If appropriate, output special features invoked.
      IMESS=0

      IF(IBEQNF.NE.0)IMESS=1
      IF(IMPEXP.NE.3.OR.RATIMP.NE.0.5)IMESS=1
      IF(ISOLAR.NE.0)IMESS=1
      IF(IDAVER.NE.0)IMESS=1
      IF(ILTHPS)IMESS=1
      IF(INTHPS)IMESS=1
      IF(IGR1D)IMESS=1
      IF(ABS(GAM-0.5).GT.1.E-4)IMESS=1
      IF(BLDG3D)IMESS=1
      IF(GRND3D)IMESS=1
      IF(MSTROK)IMESS=1
      IF(IndxSt.GT.0)IMESS=1

      IF(IMESS.EQ.1)THEN
        call edisp(icout,' Special features invoked:')
        call edisp(icout,' =========================')
      ENDIF
C Zone equations implicitness degree.
      IF(ABS(GAM-0.5).GT.1.E-4)THEN
        WRITE(OUTS,'(A,F5.2)')
     &' Zone equations implicitness degree: ',GAM
        CALL EDISP(IUOUT,OUTS)
      ENDIF

C Linear thermal conductivity.
      IF(ILTHPS)
     &CALL EDISP(IUOUT,' Linear thermal conductivity assigned.')

C Non-linear thermophysical properties assigned.
      IF(INTHPS)
     &CALL EDISP(IUOUT,' Nonlinear thermophysical properties assigned.')

C Adaptive 1D gridding performed.
      IF(IGR1D)CALL EDISP(IUOUT,' Adaptive 1D gridding performed.')
      
C 3D conduction modelling invoked.
      IF(BLDG3D)CALL EDISP(IUOUT,' 3D conduction modelling invoked.')

C 3D ground modelling invoked.
      IF(GRND3D)CALL EDISP(IUOUT,' 3D ground modelling invoked.')

C Moisture transfer invoked.
      IF(MSTROK)CALL EDISP(IUOUT,' Moisture transfer invoked.')

C Structured mesh modelling invoked.
      IF(IndxSt.GT.0)CALL EDISP(IUOUT,
     &                     ' Structured mesh modelling invoked.')

C Default for plant equation generation is implicit
C or mixed, based on time-step evaluation.
      IF(IMPEXP.NE.3.OR.RATIMP.NE.0.5) THEN

        call edisp(icout,' Plant equation type:')
        IF(IMPEXP.EQ.1)call edisp(icout,' 1 - implicit')
        IF(IMPEXP.EQ.2)call edisp(icout,' 2 - mixed')
        IF(IMPEXP.EQ.3)call edisp(icout,' 3 - timestep dependent')
        IF(IMPEXP.EQ.4)call edisp(icout,' 4 - steady state')
        IF(RATIMP.NE.0.5)then
          write(outs,*) ' impl/expl = ',RATIMP
          call edisp(icout,outs)
        endif
      ENDIF

C Default solar treatment, solar ON.
      IF(ISOLAR.NE.0)call edisp(icout,' Solar processing OFF.')

C Default - results are averaged.
      IF(IDAVER.NE.0)call edisp(icout,' Averaging OFF.')

C Option to terminate - ignore if in autoexec mode as defined in 
C the simulator parameter file.
      if (autook.or.(ISIM.gt.1)) then
        ok=.true.
      else
        dok=.true.
        h(1)='Just in case you forgot something.... '
        CALL ASKOK(' ','Continue with simulation?',OK,dok,1)
      endif
      IF(OK)goto 6
      ICON=0
      goto 7

C Commence simulation.
    6 call edisp(icout,' ')
      if (ISIM.gt.1) then
        write(outs,'(a,i4,a,i4,a)')' Simulation ',ISIM,' of ',NRSIM,
     &                                           ' has now commenced.'
        call edisp(icout,outs)
      else
        call edisp(icout,' Simulation has now commenced.')
      endif
      if(global)then
         call edisp(icout,' ')
         call edisp(icout,' Global control strategy in operation:')
         call edisp(icout,' - simulation may take longer than usual.')
      endif
      ICON=1

C If trace is active then write to the file the header 
C information above to assist in identification.
      IF(ITC.gt.0.and.ITU.ne.iuout)call mzinfo(ITU)
    7 RETURN
      END

C ******************** MZTRAC ********************
C This routine allows the user to
C select areas of bps for trace output.

C Common block variables are:

C ITC     - trace output index consisting of some +ve integer
C           value where:
C                      ITC=0 gives no trace output
C                      ITC>0 gives trace output commencing during
C                            the ITC'th time-increment
C ITCF    - is the ITCF'th time increment for the
C           trace output to end
C ITRACE  - trace call index for appropriate subroutine.
C ICNT    - counts trace output for page numbering purposes

      SUBROUTINE MZTRAC
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C6/INDCFG
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)
      COMMON/PREC13/C(MCOM,MS,MN,2),QC(MCOM,MS,MN)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)
      common/C21/IFCFG,cfgroot,LCFGF

C Common for libsv3 trace
      COMMON/enbal/nsurf,insurf(MS)

      dimension IVALS(MCOM)
      CHARACTER*29 ITEM(33)
      character outs*124,h*72,dtrace*72,ltrace*72
      character cfgroot*24,LCFGF*72
      logical OK,dok
      integer NITEMS,INO  ! max items and current menu item

      ITEM(1) ='a electrical calculations    '
      ITEM(2) ='b temporal file items & assoc'
      ITEM(3) ='c view factors & long-wave   '
      ITEM(4) ='d zone shading:insolation    '
      ITEM(5) ='e heat transfer coef & calc  '
      ITEM(6) ='f zone rel hum & condensation'
      ITEM(7) ='g infil & vent coefficients  '
      ITEM(8) ='h zone casual gains          '
      ITEM(9) ='i zone miscellaneous data    '
      ITEM(10)='j zone matrix coef setup&mods'
      ITEM(11)='l solar radiation summary    '
      ITEM(12)='n air flow calculations      '
      ITEM(13)='o zone matrix setup          '
      ITEM(14)='p matrix forward red         '
      ITEM(15)='q matrix surface adjust      '
      ITEM(16)='r matrix forward red compl   '
      ITEM(17)='s zone equation solution     '
      ITEM(18)='t zone & surface flux balance'
      ITEM(19)='u surface adjacent conditions'
      ITEM(20)='v plant matrix template      '
      ITEM(21)='w plant component static     '
      ITEM(22)='x plant component adj inf    '
      ITEM(23)='y plant coef generator       '
      ITEM(24)='z plant matrix solution      '
      ITEM(25)='1 plant matrix setup         '
      ITEM(26)='2 all plant trace items      '
      ITEM(27)='3 control functions          '
      ITEM(28)='4 time-step controller       '
      ITEM(29)='5 network flow (asci results)'
      ITEM(30)='  ___________________________'
      ITEM(31)='/ clear trace selections     '
      ITEM(32)='? help                       '
      ITEM(33)='- exit                       '

C Initilise trace output facility.
      ITC=0
      ITCF=0
      ITU=IUOUT
      DO 10 I=1,MTRACE
        ITRACE(I)=0
   10 CONTINUE
      DO 20 I=1,MCOM
        IZNTRC(I)=0
   20 CONTINUE

      INPIC=NCOMP
      H(1)='The trace facility dumps selected intermediate '
      H(2)='results from user defined subroutines throughout '
      H(3)='a defined part of the simulation. Results can '
      H(4)='be obtained for any number of zones. '
      CALL EPICKS(INPIC,IVALS,' ',' Which zones to include: ',
     &  12,NCOMP,zname,' zone trace list',IER,4)
      IZT=INPIC

      DO 40 I=1,IZT
        IX=IVALS(I)
        IZNTRC(IX)=1
   40 CONTINUE

C Case for plant only.
      if(indcfg.eq.2) goto 9989      

      dok=.true.
      h(1)='If you say yes to this you can select with of the '
      h(2)='trace reports to include.  '
      h(3)='No provides a random collection of reports as the '
      h(4)='model is read in. '
      CALL ASKOK(' ','Select trace topics?',OK,dok,4)
      IF(OK)goto 9989
      DO 9990 I=1,MTRACE
        ITRACE(I)=1
 9990 CONTINUE
      GOTO 9988

 9989 NITEMS=33
      call tstamp('>','SIM: use selective trace')

C Help text for this menu.
   4  H(1)='The trace facility dumps selected intermediate '
      H(2)='results from user defined subroutines throughout '
      H(3)='a defined part of the simulation. Results can '
      H(4)='be obtained for any number of zones. '
      H(5)=' '
      H(6)='Selective trace supports focused views of simulation'
      H(7)='sub-processes. '
      H(8)=' '
      H(9)='Information can be displayed in the text feedback '
      H(10)='portion of the interface or saved to a file. '

      INO=-2
      CALL EMENU(' Selective trace',ITEM,NITEMS,INO)
      IF(INO.LE.0)then
        ino=-1
        goto 4
      elseif(INO.EQ.30)then
        ino=-1
        goto 4
      elseif(INO.eq.NITEMS)then

C Jump to process the selected list.
        goto 9988
      elseif(INO.EQ.NITEMS-1)then

C Help for trace facility.
        CALL PHELPD(' simulation trace',10,'-',0,0,IER)
      endif

C Shift the menu index back to get trace index.
      if(ino.eq.1)then
        II=2
      elseif(ino.eq.2)then
        II=3
      elseif(ino.eq.3)then
        II=4
      elseif(ino.eq.4)then
        II=5
      elseif(ino.eq.5)then
        II=6
      elseif(ino.eq.6)then
        II=10
      elseif(ino.eq.7)then
        II=11
      elseif(ino.eq.8)then
        II=12
      elseif(ino.eq.9)then
        II=14
      elseif(ino.eq.10)then
        II=15
      elseif(ino.eq.11)then
        II=19
      elseif(ino.eq.12)then
        II=25
      elseif(ino.eq.13)then
        II=26
      elseif(ino.eq.14)then
        II=27
      elseif(ino.eq.15)then
        II=28
      elseif(ino.eq.16)then
        II=29
      elseif(ino.eq.17)then
        II=30
      elseif(ino.eq.18)then
        II=31

C Trace output of library save..
        if(IZT.eq.0)then
          call edisp(iuout,'no zones yet for library save trace...')
          goto 4
        endif

C Ask for surfaces to be traced in the surface balances.
        call edisp(iuout,' ')
        call edisp(iuout,'Please note: A trace of the results library')
        call edisp(iuout,'data creation will not be printed during the')
        call edisp(iuout,'startup period. It will also only be done')
        call edisp(iuout,'if the SAVE LEVEL has been set to high. ')
        H(1)='At least one surface is required in each zone. '
        H(2)='Currently, the same surfaces are traced in each zone. '
        CALL EASKI(NSURF,' ',
     &    ' No of surfaces for energy balance trace ? ',
     &    1,'F',0,'-',1,'no of surf for trace',IER,2)
        DO 703 I=1,nsurf
          write(outs,8011)I
 8011     format(' Which surface for energy balance number ',I2,' ?')
          CALL EASKI(IVAL,outs,'  ? ',
     &      1,'F',0,'-',1,'surf for trace',IER,2)
          INSURF(I)=IVAL
  703   CONTINUE
      elseif(ino.eq.19)then
        II=32
      elseif(ino.eq.20)then
        II=34
      elseif(ino.eq.21)then
        II=35
      elseif(ino.eq.22)then
        II=36
      elseif(ino.eq.23)then
        II=37
      elseif(ino.eq.24)then
        II=38
      elseif(ino.eq.25)then
        II=39
      elseif(ino.eq.26)then

C If all plant turn on related traces as well.
        II=40
        ITRACE(34)=1
        ITRACE(35)=1
        ITRACE(36)=1
        ITRACE(37)=1
        ITRACE(38)=1
        ITRACE(39)=1
        ITRACE(40)=1
        goto 4
      elseif(ino.eq.27)then
        II=41
      elseif(ino.eq.28)then
        II=42
      elseif(ino.eq.29)then
        II=7
        ITRACE(7)=1
        goto 4
      elseif(ino.eq.31)then

C Clear all selections.
        DO 50 I=1,MTRACE
          ITRACE(I)=0
   50   CONTINUE
        GOTO 4
      endif

C Set trace index for the topic ON.
      ITRACE(II)=1
      GOTO 4

 9988 IVAL=ITC
      H(1)='The trace output commences at the start time increment '
      H(2)='and ceases at the finish. '
      H(3)='Note that the actual times will depend on the '
      H(4)='simulation time step. '
      H(5)=' '
      H(6)='`1` will start the trace immediately, ie within the '
      H(7)='    startup period'
      CALL EASKI(IVAL,' Start trace after how many',
     &  ' (zone-side) time increments ?  ',
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
     &  ' (zone-side) time increments ?  ',
     &      ITC,'F',0,'-',5,'trace finish',IER,6)
      ITCF=IVAL

      H(1)='The trace output can be directed to the '
      H(2)='terminal or placed in a file. '
      CALL EASKAB(' ',' Write trace to: ',
     &    'terminal','scratch file',IW,2)
      if(IW.eq.1)then
        ITU=IUOUT
      else
        dtrace = 'fort.33'
        ltrace = ' '
        write(ltrace,'(a,a3)') cfgroot(1:lnblnk(cfgroot)),'.33'
        H(1)='The trace output can be directed to the '
        H(2)='terminal or placed in a file. '
  44    CALL EASKS(ltrace,' Trace file name ? ',
     &    ' ',72,dtrace,'scratch file',IER,2)
        ITU=33
        call fpopen(itu,istat,1,3,ltrace)
      endif

      RETURN
      END

C ******************** mzinfo
C Mzinfo displays the values of the currently set simulation
C parameters. If itru=iuout then goes to the display, 
C if itru=33 then works as a header to the trace.

      subroutine mzinfo(itru)
#include "building.h"
#include "model.h"
#include "esprdbfile.h"
#include "control.h"
      
      integer lnblnk  ! function definition

      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      common/save/isave

      integer ncomp,ncon
      common/c1/ncomp,ncon
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES

      common/cctlnm/ctldoc,lctlf
      common/cctl/icascf(mcom)

      character*72 lafres,lctlf,lcfgf,rfile
      character ctldoc*248,outs*124,PFILE*72,MSTRFILE*72,cfgroot*24
      character louts*248

      dimension io(mcom)

      call edisp(itru,' ')
      write(outs,'(A,A)')' Results library     :  ',RFILE
      call edisp(itru,outs)
      write(louts,'(A,A)')' Climate file        :  ',LCLIM
      call edisp248(itru,louts,100)
      write(outs,'(A,A)')' Configuration file  :  ',LCFGF
      call edisp(itru,outs)
      write(outs,'(A,A)')' Configuration descr :  ',modeltitle
      call edisp(itru,outs)
      call edisp(itru,' ')
      IF(NCF.EQ.0.AND.NCL.EQ.0)then
        call edisp(itru,' Control strategy     :     free-floating  ')
      else
        write(outs,'(A,A)')' Control file name   :  ',
     &    LCTLF(1:lnblnk(LCTLF))
        call edisp(itru,outs)
      endif
      call edisp(itru,' ')

      call edisp(itru,' Simulation parameters currently set at: ')

      write(outs,2)isd1,ism1
    2 format(' Period from day',i3,' of month',i3)
      call edisp(itru,outs)
      write(outs,93)isd2,ism2,ntstep
   93 format(10x,'to day',i3,' of month',i3,'   (',
     &i3,' time-step(s)/hr).')
      call edisp(itru,outs)

      if(isave.eq.1)then
        call edisp(itru,' Save option 1 (minimum). ')
      elseif(isave.eq.2)then
        call edisp(itru,' Save option 2 (moderate). ')
      elseif(isave.eq.3)then
        call edisp(itru,' Save option 3 (maximum). ')
      elseif(isave.eq.4)then
        call edisp(itru,' Save option 4 (energy balance). ')
c added Save level 5 information
      elseif(isave.eq.5)then
        call edisp(itru,' Save option 5 (Hot3000). ')
      endif

      call edisp(itru,' Control description: ')
      call edisp248(itru,ctldoc,72)

      write(outs,94)ncf
   94 format(' No.of control functions defined = ',i2)
      call edisp(itru,outs)

      write(outs,95)ncl
   95 format(' No.of control loops active      = ',i2)
      call edisp(itru,outs)

      if(ncf.eq.0)goto 12

      call edisp(itru,' Control  Associated ')
      call edisp(itru,' function zone ')
      do 10 i=1,ncf
        n=0
        ic=0
        do 20 j=1,ncomp
          if(icascf(j).eq.i)goto 14
          goto 20
   14     n=n+1
          ic=ic+1
          io(ic)=j
   20   continue
   10 continue
      if(n.eq.0)then 
        call edisp(itru,'         None')
      else
        write(outs,16)i,(io(j),j=1,n)
   16   format(i6,4x,24(i2,','))
        call edisp(itru,outs)
      endif

   12 return
      end
