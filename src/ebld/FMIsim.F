C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C FMIsim.F contains simulation fortran functions for the FMI
C implementation:
C FMI_INIT     - Initialise FMI and all required FMUs.
C FMI_SIMSTART - Tells all FMU instances that simulation has begun.
C FMI_SIMSTEP  - Runs all instances of all FMUs for one time step.
C FMI_DONE     - Stops simulation and frees memory for all FMUs, and
C                the FMI.

C Errors in this code are always taken as fatal; it stands to reason
C that if the user wants FMI in a simulation they will cancel it anyway
C when the FMI fails.

C ******************** FMI_INIT
C FMI_INIT initialises the FMI and all required FMUs.
      SUBROUTINE FMI_INIT(IER)

#include "building.h"
#include "FMI.h"

      common/OUTIN/IUOUT,IUIN,IEOUT
      common/rpath/path
      character path*72

      common/filep/ifil
      INTEGER ifil

C These variables and arrays contain data for the input and output
C variables that are defined in the modelDescription.xml file of an FMU.
C This assumes that there are no more variable types defined in the FMU
C than are defined in ESP-r.
C     FMUNIVAR   - Number of input variables defined in the FMU.
C     FMUIVARNAM - Names of the input variables.
C     FMUIVARNUM - Reference numbers of the input variables.
C     FMUNOVAR   - Number of output variables defined in the FMU.
C     FMUOVARNAM - Names of the output variables.
C     FMUOVARNUM - Reference numbers of the output variables.
      character*32 FMUIVARNAM(MFMIIREFS),FMUOVARNAM(MFMIOREFS)
      integer FMUIVARNUM(MFMIIREFS),FMUOVARNUM(MFMIOREFS),
     &        FMUNIVAR,FMUNOVAR

      logical XST,unixok,parseRefs,found
      character outs*124,outs2*124,fs*1,OUTSTR*124
C Limit of refname comes from FMU?VNM in FMI.h.
      character refname*32, refcaus*10
      integer refnum

      call isunix(unixok)
      if (unixok) then
        fs=char(47)
      else
        fs=char(92)
      endif

C Bypass subroutine in case there are no viable FMUs.
  1   if (IS_FMU) then

C Initialise FMI.
        call FMIC_INITFMI()
C Loop over FMUs.
        do ifmu=1,FMUNOF

          parseRefs=.true.
          FMUNIVAR=0
          FMUNOVAR=0

C Calculate how many instances needed.
          if (FMUNUMI(ifmu).gt.0) then
            FMUNIS(ifmu)=1
            FMUINS(ifmu,FMUNIS)=FMUIINS(ifmu,1)
            if (FMUNUMI(ifmu).gt.1) then
              do ifmuinp=2,FMUNUMI(ifmu)
                itmp=1
                do ifmuins=1,FMUNIS(ifmu)
                  if (FMUIINS(ifmu,ifmuinp).eq.FMUINS(ifmu,ifmuins))then
                    itmp=0
                    exit
                  endif
                enddo
                if (itmp.eq.1) then
                  FMUNIS(ifmu)=FMUNIS(ifmu)+1
                  FMUINS(ifmu,FMUNIS)=FMUIINS(ifmu,ifmuinp)
                endif
              enddo
            endif
          elseif (FMUNUMO(ifmu).gt.0) then
            FMUNIS(ifmu)=1
            FMUINS(ifmu,FMUNIS)=FMUOINS(ifmu,1)
          else
C Error.
            write(outs,'(a,i1,a)')'FMI_INIT: FMU ',ifmu,
     &        ' does not have any inputs or outputs.'
            call usrmsg(outs,'Please check FMI directives.','F')
          endif
          if (FMUNUMO(ifmu).gt.0) then
            do ifmuout=1,FMUNUMO(ifmu)
              itmp=1
              do ifmuins=1,FMUNIS(ifmu)
                if (FMUOINS(ifmu,ifmuout).eq.FMUINS(ifmu,ifmuins))then
                  itmp=0
                  exit
                endif
              enddo
              if (itmp.eq.1) then
                FMUNIS(ifmu)=FMUNIS(ifmu)+1
                FMUINS(ifmu,FMUNIS)=FMUOINS(ifmu,ifmuout)
              endif
            enddo
          endif

C Instantiate FMUs.
          do ifmuins=1,FMUNIS(ifmu)
C Check if folder already exists.
            call findfil(FMUINS(ifmu,ifmuins),XST)
            if (.not.XST) then
              write(outs,'(2a)')'mkdir ',FMUINS(ifmu,ifmuins)
              call runit(outs,'-')
            endif
            call usrdir(outs2)
            outs=outs2(1:lnblnk(outs2))//fs//
     &     FMUINS(ifmu,ifmuins)(1:lnblnk(FMUINS(ifmu,ifmuins)))
            call FMIC_INITFMU(FMUTO(ifmu),ifmuins,
     &        FMUFIL(ifmu)(1:lnblnk(FMUFIL(ifmu)))//char(0),
     &        outs(1:lnblnk(outs))//char(0),
     &    FMUINS(ifmu,ifmuins)(1:lnblnk(FMUINS(ifmu,ifmuins)))//char(0))

C The first time each FMU is instantiated, read the model description
C XML to get variable references. 
C Assumptions are:
C 1. each element and attribute of the XML is on a separate line,
C 2. contains no in-line comments,
C 3. each attribute is enclosed in quotes, and
C 4. all variables are of type real.
            if (parseRefs) then
              IUNIT=IFIL+1
              call ERPFREE(IUNIT,IER)
              call EFOPSEQ(IUNIT,outs(1:lnblnk(outs))//
     &          '/modelDescription.xml',1,IER)
              if (IER.ne.0) then
C Error.
                write(outs,'(a,i1,a)')
     & 'FMI_INIT: Failed to open modelDescription.xml for FMU ',ifmu,'.'
                call usrmsg(outs,' ','F')
              endif
              found=.false.
              do while (parseRefs)
                call STRIPC(IUNIT,OUTSTR,0,itmp,0,'FMU XML',IER)
                if (IER.ne.0) then
C Error.
                  write(outs,'(a,i1,a)')
     &    'FMI_INIT: Error while parsing modelDescription.xml for FMU ',
     &              ifmu,'.'
                  call usrmsg(outs,' ','F')
                endif
                K=0
                call EGETW(OUTSTR,K,outs,'-',' ',IER)
                if (IER.ne.0) then
                  IER=0
                  continue
                endif
                OUTSTR=outs(1:lnblnk(outs))
                if (IER.ne.0) then
C Error.
                  write(outs,'(a,i1,a)')
     &    'FMI_INIT: Error while parsing modelDescription.xml for FMU ',
     &              ifmu,'.'
                  call usrmsg(outs,' ','F')
                endif
                if (.not.found) then
C Found the name of a variable.
                  if (OUTSTR(1:5).eq.'name=') then
                    refname=OUTSTR(7:lnblnk(OUTSTR)-1)
                    found=.true.
                  elseif (OUTSTR(1:17).eq.'</ModelVariables>') then
                    parseRefs=.false.
                  endif
                else
C Found the reference number for the variable.
                  if (OUTSTR(1:15).eq.'valueReference=') then
                    read(OUTSTR(17:lnblnk(OUTSTR)-1),*) refnum
C Found the causality of the variable.
                  elseif (OUTSTR(1:10).eq.'causality=') then
                    if (OUTSTR(12:16).eq.'input') then
                      FMUNIVAR=FMUNIVAR+1
                      FMUIVARNAM(FMUNIVAR)=refname
                      FMUIVARNUM(FMUNIVAR)=refnum
                    elseif (OUTSTR(12:17).eq.'output') then
                      FMUNOVAR=FMUNOVAR+1
                      FMUOVARNAM(FMUNOVAR)=refname
                      FMUOVARNUM(FMUNOVAR)=refnum
                    else
C Error.
                      write(outs,'(5a,i1,a)')'FMI_INIT: Causality ',
     &        OUTSTR(11:lnblnk(OUTSTR)),' not recognised for variable ',
     &    refname(1:lnblnk(refname)),' in modelDescription.xml of FMU ',
     &                  ifmu,'.'
                      call usrmsg(outs,
     &                  'Please check modelDescription.xml.','F')
                    endif
                    found=.false.
                  endif
                endif                  
              enddo
              call ERPFREE(IUNIT,IER)
            endif           

          enddo ! instances

          

C Now, make a pass through all input and output directives for this FMU and
C populate arrays of variable references based on FMU variable names.
          do iinp=1,FMUNUMI(ifmu)
            found=.false.
            do itmp=1,FMUNIVAR
              if (FMUIVNM(ifmu,iinp).eq.FMUIVARNAM(itmp)) then
                FMUIRFN(ifmu,iinp)=FMUIVARNUM(itmp)
                found=.true.
                exit
              endif
            enddo
            if (.not.found) then
C Error.
              write(outs,'(3a,i1,a)')'FMI_INIT: Input variable name ',
     &          FMUIVNM(ifmu,iinp)(1:lnblnk(FMUIVNM(ifmu,iinp))),
     &          ' not found in modelDescription.xml of FMU ',ifmu,'.'
              call usrmsg(outs,'Please check input directives.','F')
            endif
          enddo

          do iout=1,FMUNUMO(ifmu)
            found=.false.
            do itmp=1,FMUNOVAR
              if (FMUOVNM(ifmu,iout).eq.FMUOVARNAM(itmp)) then
                FMUORFN(ifmu,iout)=FMUOVARNUM(itmp)
                found=.true.
                exit
              endif
            enddo
            if (.not.found) then
C Error.
              write(outs,'(3a,i1,a)')'FMI_INIT: Output variable name ',
     &          FMUOVNM(ifmu,iout)(1:lnblnk(FMUOVNM(ifmu,iout))),
     &          ' not found in modelDescription.xml of FMU ',ifmu,'.'
              call usrmsg(outs,'Please check output directives.','F')
            endif
          enddo            

        enddo ! FMUs
      else
        outs="FMI_INIT: FMI not required."
        call edisp(iuout,outs)
      endif

  999 RETURN

      END

C ******************** FMI_SIMSTART
C FMI_SIMSTART tells all FMU instances that simulation has begun.
      SUBROUTINE FMI_SIMSTART(IER)

#include "building.h"
#include "FMI.h"

      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep

      real tstart,tstop

C Bypass subroutine in case there are no viable FMUs.
  1   if (IS_FMU) then
C Loop over FMUs.
        do ifmu=1,FMUNOF
C Loop over instances.
          do ifmuins=1,FMUNIS(ifmu)
C Calculate start and stop times in seconds, starting at 0,
C and time for each simulation step in seconds
C IMPORTANT: currently main simulation starts at 1:00am on start day.
C Examples: start day = 1,   start sec = 3600
C           stop  day = 1,   stop  sec = 86399
C           start day = 365, start sec = 31453200
C           stop  day = 365, stop  sec = 31535999
            tstart=real(isds) ! day 1
            tstart=(tstart-1.0)*24.0+1.0 ! day -> hour 1
            tstart=(tstart)*60.0 ! hour -> minute
            FMUTSA(ifmu)=(tstart)*60.0 ! minute -> second
            tstop=real(isdf)
            tstop=tstop*24.0
            tstop=tstop*60.0
            FMUTSO(ifmu)=tstop*60.0-1.0
            FMUTSE(ifmu)=1.0*60.0*60.0/real(ntstep)
C Set current time to start time.
            FMUTCU(ifmu)=FMUTSA(ifmu)
            call FMIC_SIMSTART(ifmuins,FMUTSA(ifmu),FMUTSO(ifmu))
          enddo
        enddo
      endif

  999 RETURN
      END

C ******************** FMI_SIMSTEP
C FMI_SIMSTEP runs all instances of all FMUs for one time step.

C IMPORTANT: If new FMI I/O variables are added to FMIrefs.h, this sub
C            must be updated to handle these variables.

      SUBROUTINE FMI_SIMSTEP(IER)

#include "building.h"
#include "model.h"
#include "FMI.h"
#include "net_flow.h"
#include "geometry.h"
#include "tdf2.h"

      common/OUTIN/IUOUT,IUIN,IEOUT

C Arrays for passing FMU input/output information to/from C function.
C This assumes there will be no more than 1 of each input/output type
C as defined in FMIrefs.h, for each instance.
      real inVals(MFMIIREFS)
      integer inRefs(MFMIIREFS)
      integer outRefs(MFMIOREFS)
      real outVals(MFMIOREFS)

C --- ESP-r DATA STRUCTURES FOR INPUTS ---
C For zone temperatures:
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
C For ambient temperature:
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
C For lights power:
      common/btime/btimep,btimef
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its,idynow
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,IDTY
      common/CASGNS/NCGPER(MCOM,MDTY,MGTY),TCGS(MCOM,MDTY,MGTY,MGPER),
     &        CGSENC(MCOM,MDTY,MGTY,MGPER),CGSENR(MCOM,MDTY,MGTY,MGPER),
     &        CGLAT(MCOM,MDTY,MGTY,MGPER),CGCTL(MCOM,2,MGTY)
C For daylight illuminance:
      COMMON/SUNPOS/SAZI,SALT,ISUNUP
      DIMENSION zelum(MLCOM),selum(MLCOM,MDF),alt(8)
C For CO2 concentration:
      COMMON/CONTM8/CCONC(MNOD,MCONTM)
      real CCONC
      common/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      INTEGER :: iairn,icaas
      CHARACTER LAPROB*72
      
C For rain indicator:
      common/trc/itrc
      character*10 wkd1, wkd2
      common/wkdtyp/idwe1,idwe2,wkd1,wkd2

      DIMENSION VAL(MBITS+2)

C Miscellaneous declarations.
      integer N,numI,numO
      logical found
      real vartmp
      character outs*124

C Definition of sun altitude for daylight coefficient method:
      data alt/6.,18.,30.,42.,54.,66.,78.,90./

C Bypass subroutine in case there are no viable FMUs.
  1   if (IS_FMU) then
C Loop over FMUs.
        do ifmu=1,FMUNOF

C Loop over instances.
          do ifmuins=1,FMUNIS(ifmu)
            numI=0
            numO=0

C Assemble arrays of values and references for the FMU inputs.
            do ifmuinp=1,FMUNUMI(ifmu)
C Check if this variable is associated with this instance.
              if (FMUIINS(ifmu,ifmuinp).eq.FMUINS(ifmu,ifmuins)) then

C Extract required data from ESP-r data structures.

C Retrieve zone air temperature.
                if (FMUIVAR(ifmu,ifmuinp).eq.1) then
                  vartmp=TFA(FMUIZON(ifmu,ifmuinp))                  

C Retrieve working plane illuminance from casual gain control daylight
C sensor(s).
                elseif (FMUIVAR(ifmu,ifmuinp).eq.2) then
C Following code largely hijacked from INTLUM in casual.F.
C Set current zone ICOMP.
                  ICOMP=FMUIZON(ifmu,ifmuinp)
C Establish solar data for correct time-row.
                  CALL MZSINT(ICOMP,QDIR,QDIF)
C For conversion from radians to degrees.
                  PI = 4.0 * ATAN(1.0)
                  TODEG=180/PI
                  RAD=PI/180.
C Transform direct normal to horizontal irradiance as only horizontal
C sensor aloved in methods 1 and 2:
                  dir=QDIR*sin(SALT*rad)
C Calculate external horizontal unobstructed diffuse and direct
C illuminance:
                  call LUMEFF(QDIF,QDIR,SALT,IDYP,skyeff,suneff)
                  esun=dir*suneff
                  esky=QDIF*skyeff
C Assume only 1 lighting zone (whole room).
                  N=1

                  CALL CALCILUM(icomp,N,selum,zelum,alt,skyeff,suneff,
     &                          esun,esky,0)

c                  write(6,*)'zelum',zelum(N)
                  vartmp=zelum(N)

C Retrieve CO2 concentration from contaminant network.
                elseif (FMUIVAR(ifmu,ifmuinp).eq.3) then
C CCONC is in kg/kg, multiply by 1,000,000 to get mg/kg and hence ppm.
C Set flow node.
C                  INOD=ICAAS(FMUIZON(ifmu,ifmuinp))
                  INOD=FMUIZON(ifmu,ifmuinp)
C Assume only 1 contaminant, CO2.
                  ICTM=1
c                  write(6,*)'ICOMP, INOD, ICTM = ',
c     &                      FMUIZON(ifmu,ifmuinp),INOD,ICTM
c                  write(6,*)'CCONC = ',CCONC(INOD,ICTM)
                  vartmp=CCONC(INOD,ICTM)*1000000.0

C Retrieve lights power from casual gains.
                elseif (FMUIVAR(ifmu,ifmuinp).eq.4)then
C Assume lights power (W) = lights sensible casual gains (W).
C Outputs from FMU are implemented in casual gain control, so shouldn't
C affect CGSENC and CGSENR commons.
C Following code largely hijacked from MZCASI in casual.F.
C Set current zone ICOMP.
                  ICOMP=FMUIZON(ifmu,ifmuinp)
C Assume lights are on casual gain index 2.
                  IGN=2
C Calculate the current day type IDAY.
                  if(IDWP.EQ.IDWE1)then
                    IDAY=2
                  elseif(IDWP.EQ.IDWE2)then
                    IDAY=3
                  else
                    IDAY=1
                  endif
                  IF(NBDAYTYPE.gt.3)THEN ! non-standard calendar
                    IDAY=ICALENDER(IDYP)
                  endif
C Calculate the current period id IPER.
                  IPER=NCGPER(ICOMP,IDAY,IGN)
                  do I=1,NCGPER(ICOMP,IDAY,IGN)
                    if (BTIMEP.le.TCGS(ICOMP,IDAY,IGN,I+1).and.
     &                  BTIMEP.gt.TCGS(ICOMP,IDAY,IGN,I)) then
                      IPER=I
                    endif
                  enddo
                  vartmp=(CGSENC(ICOMP,IDAY,IGN,IPER)+
     &                            CGSENR(ICOMP,IDAY,IGN,IPER))*
     &                            CGCTL(ICOMP,2,IGN)

C Retrieve ambient temperature from climate data.
                elseif (FMUIVAR(ifmu,ifmuinp).eq.5) then
                  vartmp=TP

C Retrieve rain indicator from temporal data.
                elseif (FMUIVAR(ifmu,ifmuinp).eq.6) then
C If there is rain indicator information available, use this. If not,
C default to 0.
                  if (IRAININD.ne.0) then
                    IFOC=IRAININD
                    CALL RCTDFB(itrc,btimef,VAL,ISD,IFOC,IER)
                    vartmp=VAL(ISD)
                  else
                    vartmp=0.
                  endif

C *** ENTER NEW INPUTS HERE ***

                else
C Error.
                  write(outs,'(a,i2,a,i3,a,i1,2a)')
     &             'FMI_SIMSTEP: Variable index ',FMUIVAR(ifmu,ifmuinp),
     &' not recognised for input ',ifmuinp,' of FMU ',ifmu,', instance '
     &       ,FMUIINS(ifmu,ifmuinp)(1:lnblnk(FMUIINS(ifmu,ifmuinp))),'.'
                  call usrmsg(outs,'Please check input directives.','F')
                endif

C Assign input array values.
                numI=numI+1
                inVals(numI)=vartmp
                inRefs(numI)=FMUIRFN(ifmu,ifmuinp)
              endif
            enddo ! FMU input

C Assemble array of output references needed from the FMU.
            do ifmuout=1,FMUNUMO(ifmu)
C Check if this variable is associated with this instance.
              if (FMUOINS(ifmu,ifmuout).eq.FMUINS(ifmu,ifmuins)) then
                numO=numO+1
                outRefs(numO)=FMUORFN(ifmu,ifmuout)
              endif
            enddo          

C *** FMU INPUT COMMON SHOULD NOW BE POPULATED, RUN FMU ***

C Debug.
c            write(287,*)'time ',IDYP,btimep
c            write(287,*)'inVals ',(inVals(i),i=1,numI)
c            write(287,*)'inRefs ',(inRefs(i),i=1,numI)
c            write(287,*)'outRefs ',(outRefs(i),i=1,numO)

            call FMIC_SIMSTEP(ifmuins,FMUTCU(ifmu),FMUTSE(ifmu),
     &        inVals(1:numI),inRefs(1:numI),numI,
     &        outVals(1:numO),outRefs(1:numO),numO)

C Debug.
c            write(287,*)'outVals ',(outVals(i),i=1,numO)

C Assign control actions based on FMU outputs.
C Loop through outputs from the FMU.
            do iout=1,numO
              iRef=outRefs(iout)
              found=.false.
C Find output variable for this instance with the correct reference.
              do ifmuout=1,FMUNUMO(ifmu)
                if ((FMUOINS(ifmu,ifmuout).eq.FMUINS(ifmu,ifmuins)).and.
     &              (FMUORFN(ifmu,ifmuout).eq.iRef)) then
                  if (FMUOVAR(ifmu,ifmuout).gt.0) then
                    found=.true.
                    FMUCTL(FMUOZON(ifmu,ifmuout),FMUOVAR(ifmu,ifmuout))=
     &                outVals(iout)
                    FMUDOCTL(FMUOZON(ifmu,ifmuout),
     &                FMUOVAR(ifmu,ifmuout))=.true.                  
                    if (FMIONSUP(FMUOVAR(ifmu,ifmuout)).gt.0) then
                      do itmp=1,FMIONSUP(FMUOVAR(ifmu,ifmuout))
                        FMUCTLSUP(FMUOZON(ifmu,ifmuout),
     &                    FMUOVAR(ifmu,ifmuout),itmp)=
     &                    FMUOSUP(ifmu,ifmuout,itmp)
                      enddo
                    endif    
                  else
C Error.
                    write(outs,'(a,i2,a,i3,a,i1,3a)')
     &             'FMI_SIMSTEP: Variable index ',FMUOVAR(ifmu,ifmuinp),
     &            ' not defined for output ',ifmuout,' of FMU ',ifmu,
     &                ', instance ',FMUOINS(ifmu,ifmuout)
     &                (1:lnblnk(FMUOINS(ifmu,ifmuout))),'.'
                    call usrmsg(outs,'Please check output directives.',
     &                'F')
                  endif
                  exit
                endif
              enddo
              if (.not.found) then
C Error. 
                write(outs,'(a,i1,3a,i5,a)')
     & 'FMI_SIMSTEP: Cound not find output directive that matches FMU ',
     &                  ifmu,', instance ',FMUINS(ifmu,ifmuins)(1:lnblnk
     &        (FMUINS(ifmu,ifmuins))),' output with reference ',iRef,'.'
                call usrmsg(outs,'Please check output directives.','F')
              endif
            enddo

          enddo ! FMU instance
        enddo ! FMU

C Advance FMU current time.
        FMUTCU(ifmu)=FMUTCU(ifmu)+FMUTSE(ifmu)

      endif

      RETURN
      END

C ******************** FMI_DONE
C FMI_DONE stops simulation and frees memory for all FMUs, and the FMI.

      SUBROUTINE FMI_DONE(IER)

#include "building.h"
#include "FMI.h"

      if (is_FMU) then
        do ifmu=1,FMUNOF

          call FMIC_SIMSTOP(ifmu)
          call FMIC_FREEFMU(ifmu)

        enddo
        call FMIC_FREEFMI()
      endif

      RETURN
      END
