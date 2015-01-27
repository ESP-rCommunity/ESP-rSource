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

C econtrol.f includes the following subroutines:
C  EZCTLR:  Reads the configuration control strategy file.
C  EZCTLI:  Zeroes all control data in common BCTL, CCTL, and FCTL.
C  EVCNTRL: Decodes & returns string describing control file sensors,
C           actuators, control types.
C  EVCNTRLAW: Decodes & returns string describing control file laws.
C  LSTCNTLD: List one day type of a building/plant/flow/global/optical/
C           complex fenestration control.
C  Ezctlc:  Checks all building control functions, plant loops, and
C           mass flow control functions for validity.
C  tscdta:  Sets TSC6 variables in the case of TSC6 called from a
C           control function (as opposed to set from menu).
C  FZDATA:  reads fuzzy logic data from configuration control file.

C ******************** EZCTLR ********************
C EZCTLR: reads the configuration control strategy file.

c Variables used are:

C ctldoc  - Overall control notes. If equal to 'none'
C           then no control is imposed.

C icascf  - Defines the zone associated with a control function.   Zero
C           means no zone control.

C lctlf   - Holds the configuration control file name if one is active.

C znctldoc | - Hold a descriptive for the zones, plant, mass flow            
C plctldoc |   and power control regimes.
C flctldoc |
C elctldoc |
C opticdoc |
C CFCctldoc|

C ncf   | - Are the number of zone control functions, plant control
C ncl   |   loops, power flow and  mass flow control functions.
C ncc   |
C necl  |
C nol   |
C nCFCctlloops |

C ibsn  | - Defines the sensor node associated with any control function
C ipsn  |   (B), control loop (P),  mass flow control function (F), power
C ifsn  |   control function (E) and Optical control function (O).
C iesn  |
C iosn  |
C           The three locations have the following meaning:

C           i?sn(?,1)> 0; defines a zone where the sensor is located.
C                         If i?sn(?,2)=0 then sensor is that zone's air
C                                        point.
C                         If i?sn(?,2)>0 and i?sn(?,3)=0 then sensor is
C                                        the surface defined by i?sn(?,2).
C                         If i?sn(?,2)>0 and i?sn(?,3)>0 then sensor is
C                                        the construction node defined by
C                                        construction i?sn(?,2), node
C                                        position i?sn(?,3).
C           ibsn(?,1)= 0; control is on associated zone's air point.
C           iosn(?,1)= 0; control is on associated zone's air point.
C           ipsn(?,1)= 0; - not active for plant side.
C           ifsn(?,1)= 0; - not active for flow side.
C           ibsn(?,1)=-1; - not active for building side.
C<<<<<<<<<<<<<<<<<<<<<remember the line below: >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C           iosn(?,1)=-1; - sensing incident solar radiation on a surface.
C           ipsn(?,1)=-1; defines a plant component node given by
C                         component ipsn(?,2), node ipsn(?,3).
C           ifsn(?,1)=-1; defines a plant component node given by
C                         component ifsn(?,2), node ifsn(?,3).
C           i?sn(?,1)=-2; control is on zone i?sn(?,2) convective/radiant
C                         mixed temperature in which the convective
C                         component is given by i?sn(?,3)/100.
C           i?sn(?,1)=-3; control is on outside condition where
C                         if i?sn(?,2)=0 dry bulb temperature is used.
C                         if i?sn(?,2)=1 sol-air temperature is used.
C                         if i?sn(?,2)=2 wind speed is used.
C                         if i?sn(?,2)=3 wind direction is used.
C                         if i?sn(?,2)=4 diffuse hor'l radiation is used.
C                         if i?sn(?,2)=5 direct normal radiation is used.
C                         if i?sn(?,2)=6 ext. rel. humidity is used.
C           ibsn(?,1)=-4; not applicable.
C           ipsn(?,1)=-4; not applicable.
C           ifsn(?,1)=-4; defines a mass flow node or connection given
C                         by ifsn(?,2).
C           ibsn(?,1)=-5; defines an ascii boundry file, the column index 
C                         for data given by ibsn(?,2).
C           ipsn(?,1)=-5; ascii boundry file - not yet active for plant side.
C           ifsn(?,1)=-5; not applicable.
C           ibsn(?,1)=-6; sensed valued is produced by a function generator.
C           ipsn(?,1)=-6; sensed valued is produced by a function generator.
C           ifsn(?,1)=-6; not applicable.
C           ibsn(?,1)=-99; construction substitution.
C                         ibsn(?,2)= upper zone air temperature limit 
C                                    (above which substitution occurs).
C                         ibsn(?,3)= lower zone air temperature limit 
C                                    (below which substitution occurs).
C                              Note: For all building side control function 
C                                    sensors, ibsn(?,4) holds the reference 
C                                    number of any nested control function.
C           ipsn(?,1)=-99; not active.
C           ifsn(?,1)=-99; not active.
C           iesn(?,1)=-9; Control is based on a bus condition
C
C iCFCsensor - Defines the sensor node associated with a complex fenestration
C              control function
C           The three locations have the following meaning:

C           iCFCsensor(?,1)> 0; defines a zone where the sensor is located.
C                         If iCFCsensor(?,2)=0 then sensor is that zone's air
C                                            point.
C                         If iCFCsensor(?,2)>0 and iCFCsensor(?,3)=0 then sensor is
C                                            the surface defined by iCFCsensor(?,2).
C                         If iCFCsensor(?,2)>0 and iCFCsensor(?,3)>0 then sensor is
C                                        the construction node defined by
C                                        construction iCFCsensor(?,2), node
C                                        position iCFCsensor(?,3).
C           iCFCsensor(?,1)= 0; sensor not used (schedule only).
C
C           iCFCsensor(?,1)=-1; - not active for complex fenestration control.
C           iCFCsensor(?,1)=-2; control is on zone iCFCsensor(?,2) convective/radiant
C                         mixed temperature in which the convective
C                         component is given by iCFCsensor(?,3)/100.
C           iCFCsensor(?,1)=-3; control is on outside condition where
C                         if iCFCsensor(?,2)=0 dry bulb temperature is used.
C                         if iCFCsensor(?,2)=1 sol-air temperature is used.
C                         if iCFCsensor(?,2)=2 wind speed is used.
C                         if iCFCsensor(?,2)=3 wind direction is used.
C                         if iCFCsensor(?,2)=4 diffuse hor'l radiation is used.
C                         if iCFCsensor(?,2)=5 direct normal radiation is used.
C           iCFCsensor(?,1)= -4; sensing incident solar radiation on a surface where
C                         iCFCsensor(?,2) refers to the zone and iCFCsensor(?,3) refers
C                         to an external facing surface.

C iban    - Defines the actuator node associated with any zone control
C           function.
C           iban(?,1)>0  is as ibsn(?,1)>0
C           iban(?,2)=-1 is as ibsn(?,1)=-1
C           iban(?,3)=-2 is as ibsn(?,1)=-2
C           For special case of ibsn(?,1)=-99:
C              iban(?,1)=surface pointer for first construction.
C              iban(?,2)=surface pointer for second construction or zero.
C              iban(?,3)=surface pointer for third construction or zero.
C           with other iban combinations not meaningful.

C ipan    - Defines the actuator node associated with any plant
C           control loop.
C           ipan(?,1)=-1  is as ipsn(?,1)=-1
C           with other ipan combinations not meaningful.

C ifan    - Defines the actuator connection or actuator component.
C         - ifan(?,1)=-3; control is on a mass flow connection.
C                         ifan(?,2) > 0; defines connection number 
C                                        to be controlled.
C         - ifan(?,1)=-4; control is on a mass flow component.
C                         ifan(?,2) > 0; defines component number 
C                                        to be controlled.
C                         ifan(?,3) > 0; defines number of connections 
C                                        associated with the control 
C                                        function.
C  << ?? >> with other ifan combinations not meaningful.]
C         - iean(?,1)=-1; control is on a plant component
C         - iean(?,1)=-8; control is on a building component
C         - iean(?,1)=-9; control is on a power only component
C
C iCFCactuator    - Defines the actuator node associated with a complex 
C                   fenestration control function.
C                 iCFCactuator(?,1)=0 actuates shade layer ON/OFF
C                                  =1 actuates slat angle of slat-type shade
C                                  =2 actuates both shade layer ON/OFF status
C                                   and slat angle (for schedule only)
C                 iCFCactuator(?,2) is the zone index
C                 iCFCactuator(?,3) is the CFC type

C nbcdt | - Give the number of control day types for each domain.
C npcdt |   The special case of 0 implies the usual 
C nfcdt |   Weekday/Saturday/Sunday combination.
C necdt |   
C nocdt |   
C nCFCctldaytypes |

C ibcdv | - Give the dates of validity (in year day number format)
C ipcdv |   for each day type.
C ifcdv | 
C iecdv |   
C iocdv |   
C iCFCctldatevalid |

C nbcdp | - Give the number of distinct control periods during each
C npcdp |   day type.   During these periods different control laws 
C nfcdp |   can prevail.
C necdp |
C nocdp |
C nCFCdayctlperiods |

C tbcps | 
C tpcps | - Give the start time for each period.
C tfcps |
C tecps |
C tocps |
C CFCctlperiodstart |

C ibctyp |- Defines the controller type for each building control function
C           for day type, and period:
C           0: senses dry bulb temp; actuates flux.
C           Other controller types not yet active for building side.
C ipctyp |- Defines the controller type for each plant control loop 
C           for day type, and period:
C           0: senses dry bulb temp.         , actuates flux
C           1: senses dry bulb temp.         , actuates mass flow rate
C           2: senses enthalpy               , actuates flux
C           3: senses enthalpy               , actuates mass flow rate
C           4: senses 1st ph. mass flow rate , actuates flux
C           5: senses 1st ph. mass flow rate , actuates mass flow rate
C           6: senses 2nd ph. mass flow rate , actuates flux
C           7: senses 2nd ph. mass flow rate , actuates mass flow rate
C           8: additional plant output       , actuates flux
C           9: additional plant output       , actuates mass flow rate
C          10: relative humidity             , actuates flux
C          11: relative humidity             , actuates mass flow rate
C          12: senses dry bulb temp.         , variable expecting num val.
C          13: senses enthalpy               , variable expecting num val.
C          14: senses 1st ph. mass flow rate , variable expecting num val.
C          15: senses 2nd ph. mass flow rate , variable expecting num val.
C          16: additional plant output       , variable expecting num val.
C          17: relative humidity             , variable expecting num val.
C          18: senses dry bulb temp.         , mass diversion ratio.
C          19: senses enthalpy               , mass diversion ratio.
C          20: senses 1st ph. mass flow rate , mass diversion ratio.
C          21: senses 2nd ph. mass flow rate , mass diversion ratio.
C          22: additional plant output       , mass diversion ratio.
C          23: relative humidity             , mass diversion ratio.
C          Other controller types not yet active for plant side.
C ifctyp |- Defines the controller type for each flow control function 
C           for day type, and period:
C           0: --------not applicable-------
C           1: senses dry bulb temp.        , actuates mass flow rate
C           2: --------not applicable-------
C           3: senses enthalpy              , actuates mass flow rate
C           4: --------not applicable-------
C           5: senses 1th ph. mass flow rate, actuates mass flow rate
C           6: --------not applicable-------
C           7: senses 2nd ph. mass flow rate, actuates mass flow rate
C           8: --------not applicable-------
C           9: additional plt output        , actuates mass flow rate
C          10: --------not applicable-------
C          11: relative humidity            , actuates mass flow rate
C          12 -> 23: ---not applicable------
C          24: temperature difference       , actuates mass flow rate
C          25: absolute temperature diff.   , actuates mass flow rate
C          26: pressure                     , actuates mass flow rate
C          27: pressure difference          , actuates mass flow rate
C          28: absolute pressure difference , actuates mass flow rate
C          29: absolute mass flow rate      , actuates mass flow rate
C          30: wind speed                   , actuates mass flow rate
C          31: wind direction               , actuates mass flow rate
C          32: diffuse horizontal solar rad., actuates mass flow rate
C          33: direct normal solar radiation, actuates mass flow rate
C          34: relative humidity outdoor air, actuates mass flow rate
C iectyp | 1: Senses voltage actuates VAR
C iectype| 2: Senses VA actuates ??
C iCFCctltype |- Defines the controller type for each complex fenestration
C                control function, day type, and period:
C                0: not active for complex fenestration control.
C                1: senses temperature, actuates shade ON/OFF
C                2: senses temperature, actuates slat angle
C                3: senses solar radiation, actuates shade ON/OFF
C                4: senses solar radiation, actuates slat angle
C                5: senses wind speed, actuates shade ON/OFF
C                6: senses wind directions, actuates shade ON/OFF
C                7: no sensor, schedule only

C ibclaw |- Defines the control law which relates the sensor to
C ipclaw |  the actuator.  A number of control laws are active
C ifclaw |  and these are defined in routines BCTL, PCTL and FCTL.
C ioclaw |
C iCFCctllaw | - Complex fenestration control law: 1 - Basic control
C                                                  2 - Schedule

C bmiscd |- Hold data required by the control laws. ?MISCD(?,?,?,1)
C pmiscd |  defines the total number of such items to be read.
C fmiscd |  In the case of thermophysical properties substitution
C emiscd |  items 3-5 are the MLC indices for each of the 3 surfaces.
C omiscd |
C CFCmiscdata |

C iasocc   1st..n connection index for each mass flow connection 
C          associated with a component flow control.  Use with 
C          ifsn(??,1)= -4.
C nfsup    number of flow control supplemental items, associated with
C          a flow control via a component (ifsn(??,1)= -4).  This implies
C          that for flow, a single control type is applicable for each
C          flow control function.

      SUBROUTINE EZCTLR(IUNIT,ITRC,ITRU,IER)

#include "building.h"
#include "geometry.h"
#include "net_flow.h"
#include "control.h"
#include "espriou.h"
C espriou.h provides currentfile.
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      integer ncomp,ncon
      common/c1/ncomp,ncon
      common/cctlnm/ctldoc,lctlf
      integer icascf
      common/cctl/icascf(mcom)
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/fctl4/iasocc(MCF,MCMP),nfsup(MCF)
      common/glbctl/global,glbrwd
      COMMON/PBYDAT/PBYFIL,NSCVP,IPS
      common/bfngr/cfgsd(MCF,6),ibsnv(mcf,3),bfngn
      common/pcfngr/clfgsd(MCF,6),pcfngn,IDAYPD
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      common/senact/realsn(MNONID,3,7),realac(MNONID,5,7),svctlp,
     &       deltap,ctldrp

C Error subroutine and error details for dll mode.
      common/dllerr/dllsubr,dllmesg
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,IDTY
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      character calename*32,calentag*12,calendayname*32
      DIMENSION IVA(MCOM),RVA(misc)
      CHARACTER HDG*5,ctldoc*248,LCTLF*72,OUTSTR*124,SSTR*96
      CHARACTER PERST1*14,PERST2*44,PERST3*44,NODID*12,
     &          WORD*20,CMPID*12,NDPOS*12,NDNEG*12
      character dllsubr*12,dllmesg*124,outs*124,t40*40
      character LOUTSTR*248
      logical close,pbyfil,bfngn,pcfngn,global,glbrwd,dll

      logical first_run_done

C Initialization flag for distributed generation controller.
      common/DgCont/first_run_done

C Initialization flag for adsorption storage unit controller.
      COMMON/ADSCont/bAds_ctl_init
      LOGICAL bADS_ctl_init   
      
      bfngn=.false.     
      pcfngn=.false.
      pbyfil=.false.
      nnidsn=0
      nnidac=0

      IER=0

C Check if running in dll mode.
      call isadll(dll)

C Initialise control file if the name is something other than UNKNOWN.
      if(LCTLF(1:7).ne.'UNKNOWN')then
        CALL EFOPSEQ(IUNIT,LCTLF,1,IER)
        IF(IER.LT.0)THEN
          write(outs,'(3a)') 'Control file ',LCTLF(1:lnblnk(LCTLF)),
     &      ' could not be opened.'
          if(dll)then
            dllsubr='EZCTLR'
            dllmesg=outs
            ier=2
            return
          else
            call edisp(itru,outs)
            return
          endif
        endif
      else
        return
      endif
      write(currentfile,'(a)') LCTLF(1:lnblnk(LCTLF))

c Initialise all control data to zero.
      CALL EZCTLI

C Read lines from file, discarding comments, check the number of items
C in the data. 
      CALL LSTRIPC(IUNIT,LOUTSTR,0,ND,1,'control notes',IER)
      IF(IER.NE.0)goto 99
      ctldoc=LOUTSTR
      IF(ctldoc(1:4).EQ.'NONE'.OR.ctldoc(1:4).EQ.'none')THEN
        CALL EDISP(ITRU,' ')
        CALL EDISP(ITRU,
     &' The current control file has used the descriptor [none].')
        CALL EDISP(ITRU,
     &' This is a protected character string meaning no control.')
        CALL EDISP(ITRU,
     &' Please change this descriptor or specify another control file.')
        GOTO 9
      ENDIF

C Read heading * BUILDING or * PLANT or *MAS *POW  *OPT or *SYSTEM or * CFC.
C After last item of a control function or plant loop is read it will
C loop back here and this may the end of the file so do not warn user
C and do not pass back ier.
  2   CALL STRIPC(IUNIT,OUTSTR,0,ND,0,'control name',IERV)
      if(IERV.eq.2)then
        GOTO 9
      elseif(IERV.eq.0)then
        continue
      else
        goto 99
      endif
      HDG=OUTSTR(1:5)
      if((hdg.ne.'* bui'.and.hdg.ne.'* Bui'.and.hdg.ne.'* BUI').and.
     & (hdg.ne.'* pla'.and.hdg.ne.'* Pla'.and.hdg.ne.'* PLA').and.
     &(hdg.ne.'* fuz'.and.hdg.ne.'* Fuz'.and.hdg.ne.'* FUZ').and.
     & (hdg.ne.'* mas'.and.hdg.ne.'* Mas'.and.hdg.ne.'* MAS').and.
     &(hdg.ne.'* pow'.and.hdg.ne.'* Pow'.and.hdg.ne.'* POW').and.
     &(hdg.ne.'* sys'.and.hdg.ne.'* Sys'.and.hdg.ne.'* SYS').and.
     &(hdg.ne.'* opt'.and.hdg.ne.'* Opt'.and.hdg.ne.'* OPT').and.
     &(hdg.ne.'* cfc'.and.hdg.ne.'* Cfc'.and.hdg.ne.'* CFC'))then

        write(outs,'(3a)') 'Control file ',LCTLF(1:lnblnk(LCTLF)),
     &    ' unexpected end of file sensed.'
        outs= 'End of control file sensed'
        if(dll)then
          dllsubr='EZCTLR'
          dllmesg=outs
          ier=2
          return
        else
          call edisp(itru,outs)
          CALL EDISP(ITRU,OUTSTR)
          CALL ERPFREE(IUNIT,ISTAT)
          ier=1
          return
        endif
      endif

      iborp=1
      if(hdg.eq.'* pla'.or.hdg.eq.'* Pla'.or.hdg.eq.'* PLA')iborp=2
      if(hdg.eq.'* mas'.or.hdg.eq.'* Mas'.or.hdg.eq.'* MAS')iborp=3
      if(hdg.eq.'* sys'.or.hdg.eq.'* Sys'.or.hdg.eq.'* SYS')iborp=4
      if(hdg.eq.'* pow'.or.hdg.eq.'* Pow'.or.hdg.eq.'* POW')iborp=5
      if(hdg.eq.'* opt'.or.hdg.eq.'* Opt'.or.hdg.eq.'* OPT')iborp=6
      if(hdg.eq.'* fuz'.or.hdg.eq.'* Fuz'.or.hdg.eq.'* FUZ')GOTO 7
      if(hdg.eq.'* cfc'.or.hdg.eq.'* Cfc'.or.hdg.eq.'* CFC')iborp=7
      if(iborp.eq.2)goto 11
      if(iborp.eq.3)goto 12
      if(iborp.eq.5)goto 18
      if(iborp.eq.6)goto 19
      if(iborp.eq.7)goto 21
      global=.false.
      if(iborp.eq.4)global=.true.
      if(iborp.eq.4)goto 113

C  Establish zone control regime notes (znctldoc) and number of
C  control functions (NCF).
      CALL LSTRIPC(IUNIT,LOUTSTR,0,ND,1,'zn control notes',IER)
      znctldoc=LOUTSTR

      CALL STRIPC(IUNIT,OUTSTR,1,ND,1,'no cntrl loops',IER)
      KG=0
      CALL EGETWI(OUTSTR,KG,NCF,-1,MCF,'F','no loops',IER)
      IF(IER.NE.0)GOTO 99

C Reporting:
      IF(ITRC.GE.1)THEN
        CALL EDISP(ITRU,' ')
        WRITE(OUTSTR,250)NCF
 250    FORMAT(' Within the current model',I3,' control loops',
     &         ' have been specified. ')
        CALL EDISP(ITRU,OUTSTR)
        CALL EDISP(ITRU,' The overall project control is ')
        CALL EDISP248(ITRU,ctldoc,72)
        CALL EDISP(ITRU,' and the zone control is')
        CALL EDISP248(ITRU,znctldoc,72)
        CALL EDISP(ITRU,' ')
      ENDIF

      if(ncf.le.0)goto 13

c For each control function.
      DO 10 I=1,NCF
        II=I
        ICF=II

C Read header : * CONTROL FUNCTION.
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'* CONTROL ',IER)
        IF(IER.NE.0)GOTO 99
        HDG=OUTSTR(1:5)
        IF(HDG.NE.'* CON'.AND.HDG.NE.'* Con'.AND.HDG.NE.'* con')THEN
          CALL USRMSG(' ',' * CONTROL string not found ','W')
          GOTO 1111
        ENDIF

C Read control functions information and perform checks. If 4 items
C then new format file, otherwise old format so set IBSN(II,4)=0.
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'sensor',IER)
        IF(IER.NE.0)GOTO 99
        IF(ND.GE.3)THEN
          KG=0
          CALL EGETWI(OUTSTR,KG,IVAL,0,0,'-','sensor 1st',IER)
          IBSN(II,1)=IVAL
          CALL EGETWI(OUTSTR,KG,IVAL,0,0,'-','sensor 2nd ',IER)
          IBSN(II,2)=IVAL
          CALL EGETWI(OUTSTR,KG,IVAL,0,0,'-','sensor 3rd ',IER)
          IBSN(II,3)=IVAL
          IF(ND.EQ.4)THEN
            CALL EGETWI(OUTSTR,KG,IVAL,0,0,'-','nested pnter',IER)
            IBSN(II,4)=IVAL
          ELSE
            IBSN(II,4)=0
          ENDIF
          IF(IER.NE.0)GOTO 99

C Range checking.
          if((ibsn(ii,1).lt.-6.or.ibsn(ii,1).gt.mcom)
     &        .and.ibsn(ii,1).ne.-99)goto 1003
          if(abs(ibsn(ii,4)).gt.mcf)goto  1002
          if((ibsn(ii,2).lt.0.or.ibsn(ii,3).lt.0)
     &        .and.ibsn(ii,1).ne.-99)goto 1004
        ENDIF

C Reporting: Get a string describing the current sensor based on common.
        IF(ITRC.GE.1)THEN
          IIS=II
          CALL EVCNTRL(0,IIS,1,1,'S',SSTR)
          if(IBSN(II,4).eq.0)then
            WRITE(OUTSTR,'(a,i2,2a)') ' The sensor for function ',
     &        II,' ',SSTR(1:LNBLNK(SSTR))
          else
            WRITE(OUTSTR,'(a,i2,3a,i2)') ' The sensor for function ',
     &        II,' ',SSTR(1:LNBLNK(SSTR)),
     &        ' with nested loop ',IBSN(II,4)
          endif
          CALL EDISP(ITRU,' ')
          CALL EDISP(ITRU,OUTSTR)
        ENDIF

        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'actuator',IER)
        IF(IER.NE.0)GOTO 99
        IF(ND.GE.3)THEN
          KG=0
          CALL EGETWI(OUTSTR,KG,IV,-3,MCOM,'F','actuator 1st',IER)
          IBAN(II,1)=IV
          CALL EGETWI(OUTSTR,KG,IV,0,MCOM,'F','actuator 2nd',IER)
          IBAN(II,2)=IV
          CALL EGETWI(OUTSTR,KG,IV,0,0,'-','actuator 3rd',IER)
          IBAN(II,3)=IV
          IF(IER.NE.0)GOTO 99

C Range checking.
          if((iban(ii,1).lt.-3.or.iban(ii,1).gt.mcom)
     &        .and.ibsn(ii,1).ne.-99)goto 1005
          if((iban(ii,2).lt.0.or.iban(ii,3).lt.0)
     &        .and.ibsn(ii,1).ne.-99)goto 1005
        ENDIF

C Reporting: Get a string describing the actuator based on common block.
        IF(ITRC.GE.1)THEN
          IIS=II
          CALL EVCNTRL(0,IIS,1,1,'A',SSTR)
          WRITE(OUTSTR,75)II,SSTR(1:LNBLNK(SSTR))
  75      FORMAT(' The actuator for function ',I2,' is ',a)
          CALL EDISP(ITRU,OUTSTR)
        ENDIF

C Day types.
        CALL STRIPC(IUNIT,OUTSTR,1,ND,1,'day types',IER)
        KG=0
        CALL EGETWI(OUTSTR,KG,NBCDT(II),0,MCDT,'F',
     &              'number of control day type(s)',IER)
        IF(IER.NE.0)GOTO 99

C Reporting:
        IF(ITRC.GE.1)THEN
          CALL EDISP(ITRU,' For building control')
          CALL EDISP(ITRU,' the following day types have been defined')
          write(outstr,'(20a)') (CALENTAG(idty),idty=1,NBDAYTYPE)
          call edisp(ITRU,outstr)
        ENDIF

        nn=nbcdt(ii)  ! a zero says use all day types
        if(nn.eq.0)nn=NBDAYTYPE
        do 20 j=1,nn
          jj=j
          CALL STRIPC(IUNIT,OUTSTR,2,ND,1,'validity',IER)
          IF(IER.NE.0)GOTO 99
          KG=0
          CALL EGETWI(OUTSTR,KG,IBCDV(II,JJ,1),1,365,'F',
     &              'control activation start day number',IER)
          CALL EGETWI(OUTSTR,KG,IBCDV(II,JJ,2),1,365,'F',
     &              'control activation end day number',IER)
          IF(IBCDV(II,JJ,2).LT.IBCDV(II,JJ,1))THEN
            CALL USRMSG(' ',' Start & End dates reversed.','W')
            GOTO 2000
          ENDIF

          CALL STRIPC(IUNIT,OUTSTR,1,ND,1,'periods',IER)
          KG=0
          CALL EGETWI(OUTSTR,KG,NBCDP(II,JJ),1,MCDP,'F',
     &              'control periods during day',IER)
          IF(IER.NE.0)GOTO 99

          LL=NBCDP(II,JJ)
          DO 30 K=1,LL
            KK=K
            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'control',IER)
            IF(IER.NE.0)GOTO 99
            KG=0
            CALL EGETWI(OUTSTR,KG,IBCTYP(II,JJ,KK),0,99,'W',
     &              'controller type (1st item)',IER)
            CALL EGETWI(OUTSTR,KG,IBCLAW(II,JJ,KK),0,99,'W',
     &              'control law (2nd item)',IER)
            CALL EGETWR(OUTSTR,KG,TBCPS(II,JJ,KK),0.,24.,'F',
     &              'activation time',IER)
            IF(IER.NE.0)GOTO 99

            IF(KK.EQ.1)THEN
              call eclose(TBCPS(II,JJ,KK),0.0,0.001,close)
              IF(.NOT.close)THEN
                CALL USRMSG(' ',' 1st period must begin @ 0!','W')
                GOTO 2000
              ENDIF
            ELSEIF(TBCPS(II,JJ,KK-1).GE.TBCPS(II,JJ,KK))THEN
              CALL USRMSG(' ',' periods out of order','W')
              GOTO 2000
            ENDIF

            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'data next',IER)
            IF(IER.NE.0)GOTO 99
            KG=0
            CALL EGETWR(OUTSTR,KG,BMISCD(II,JJ,KK,1),0.,
     &       FLOAT(MISC),'F','number of data items for function',IER)
            NITEMS=INT(BMISCD(II,JJ,KK,1))

            IF(NITEMS.GT.0)THEN
              call egetwra(iunit,RVA,nitems,0.,0.,'-','misc items',
     &          ierv)
              do 42 ij=2,nitems+1
                BMISCD(II,JJ,KK,ij)=RVA(ij-1)
  42          continue
              IF(IER.NE.0)GOTO 99
            ENDIF

C Set building-side function generator flag.
            if(ibsn(ii,1).eq.-6)then
               bfngn=.true.
               cfgsd(ii,1)=1.0
            endif
   30     CONTINUE

C Reporting of all periods in this day type (compacted data list):
          IF(ITRC.GE.1)call lstcntld(itru,0,II,JJ)
   20   CONTINUE
   10 CONTINUE

C Now relate control function to associated zone. Since this may be
C the last line of the file do not print end of file warning or pass
C ier back to calling routine.
      IRVA=NCOMP
      CALL EGETWIA(IUNIT,IVA,IRVA,0,0,'-','assoc z list',IER)
      IF(IER.NE.0)GOTO 109
      DO 28 LC=1,NCOMP
        ICASCF(LC)=IVA(LC)

C Range checking (protect from 0 array reference).
        IF(ICASCF(LC).NE.0)THEN
          IF(ABS(ICASCF(LC)).GT.NCF)goto 1081
          if(ibsn(abs(icascf(LC)),1).eq.-99)goto 1082
          if(abs(ibsn(abs(icascf(LC)),4)).gt.ncf)goto 1082
        ENDIF

C Reporting.
        IF(ITRC.GE.1)THEN
           WRITE(OUTS,85)LC,ZNAME(LC),ICASCF(LC)
  85       FORMAT(' zone (',I2,') ',A12,' << control ',I2)
           CALL EDISP(ITRU,OUTS)
        ENDIF
  28  CONTINUE
      goto 15

C Plant control loops.

C Establish plant control regime name and number of
C control loops.
   11 CALL LSTRIPC(IUNIT,LOUTSTR,0,ND,1,'plant ctl notes',IER)
      plctldoc=LOUTSTR

      CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'no control loops',IER)
      KG=0
      CALL EGETWI(OUTSTR,KG,ncl,-1,mcf,'F','no loops',IER)
      IF(IER.NE.0)GOTO 99

C Reporting:
      IF(ITRC.GE.1)THEN
        CALL EDISP(ITRU,' ')
        WRITE(OUTSTR,350)ncl
 350    FORMAT(' Within the current project',I2,' control loops',
     &         ' have been specified. ')
        CALL EDISP(ITRU,OUTSTR)
        call edisp(itru,' plant control is ')
        CALL EDISP248(ITRU,plctldoc,72)
        CALL EDISP(ITRU,' ')
      ENDIF
      if(ncl.le.0)goto 13

C Read control loops information and perform checks
C where possible.
      do 40 i=1,ncl
        ii=i
        icl=ii

C Read header : * control loops.
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'* CONTROL ',IER)
        IF(IER.NE.0)GOTO 99
        HDG=OUTSTR(1:5)
        IF(HDG.NE.'* CON'.AND.HDG.NE.'* Con'.AND.HDG.NE.'* con')THEN
          CALL USRMSG(' ',' * CONTROL string not found ','W')
          GOTO 1111
        ENDIF

C Read control functions information and perform checks. If 4 items
C then new format file, otherwise old format so set IBSN(II,4)=0.
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'sensor',IER)
        IF(IER.NE.0)GOTO 99
        KG=0
        CALL EGETWI(OUTSTR,KG,IVAL,-6,mcom,'F','sensor 1st',IER)
        ipsn(ii,1)=IVAL
        CALL EGETWI(OUTSTR,KG,IVAL,0,0,'-','sensor 2nd ',IER)
        ipsn(ii,2)=IVAL
        CALL EGETWI(OUTSTR,KG,IVAL,0,0,'-','sensor 3rd ',IER)
        ipsn(ii,3)=IVAL
        if(ND.gt.3)then
          CALL EGETWI(OUTSTR,KG,IVAL,0,0,'-','sensor 4th ',IER)
          ipsn(ii,4)=IVAL
        else
          ipsn(ii,4)=0
        endif
        if(ND.gt.4) then
          CALL EGETWI(OUTSTR,KG,IVAL,0,0,'-','sensor 5th ',IER)
          ipsn(ii,5) = IVAL
        else
          ipsn(ii,5) = 0
        endif        
        IF(IER.NE.0)GOTO 99

C Range checking.
        if(ipsn(ii,2).lt.0.or.ipsn(ii,3).lt.0)goto 1052

C Possibly read non-ideal sensor data items.
        iflags=ipsn(ii,4)
        if(iflags.eq.-1)then
          nnidsn=nnidsn+1
          isen=nnidsn
          do 520 ichac=1,3
            ND=5
            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'non-idl sen',IERV)
            IF(IERV.NE.0)GOTO 99
            KG=0
            do 540 idata=1,5
              CALL EGETWR(OUTSTR,KG,VAL,0.,0.,'-',
     &           'non-ideal sensor ',IERV)
              realsn(isen,ichac,idata+2)=VAL
 540        continue
 520      continue
          IF(IERV.NE.0)GOTO 99
        endif

C Reporting: Get a string describing the current sensor based on common.
        IF(ITRC.GE.1)THEN
          IIS=II
          CALL EVCNTRL(1,IIS,1,1,'S',SSTR)
          WRITE(OUTSTR,364)II,SSTR(1:LNBLNK(SSTR))
 364      FORMAT(' The sensor for loop ',I2,' ',a)
          CALL EDISP(ITRU,OUTSTR)
        ENDIF

C Set plant boundry data file flag.
        if(ipsn(ii,1).eq.-5)pbyfil=.true.

        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'actuator',IER)
        IF(IER.NE.0)GOTO 99
        KG=0
        CALL EGETWI(OUTSTR,KG,IV,-1,-1,'F','actuator 1st',IER)
        ipan(II,1)=IV
        CALL EGETWI(OUTSTR,KG,IV,0,0,'-','actuator 2nd',IER)
        ipan(II,2)=IV
        CALL EGETWI(OUTSTR,KG,IV,0,0,'-','actuator 3rd',IER)
        ipan(II,3)=IV
        if(ND.gt.3)then
          CALL EGETWI(OUTSTR,KG,IV,0,0,'-','actuator 4th',IER)
          ipan(II,4)=IV
        else
          ipan(II,4)=0
        endif
        IF(IER.NE.0)GOTO 99

C Range checking.
        if(ipan(ii,2).lt.0.or.ipan(ii,3).lt.0)goto 1053

C Possibly read non-ideal actuator data items.
C First set reference variables allowing matching of sensor and actuator.
        iflaga=ipan(ii,4)
        if(iflaga.eq.-1)then
          do 610 ichrc=1,3
             realsn(isen,ichrc,1)=ipan(ii,2)
             realsn(isen,ichrc,2)=ipan(ii,3)
 610      continue
          nnidac=nnidac+1
          iact=nnidac
          do 620 ichaca=1,5
            realac(iact,ichaca,1)=ipan(ii,2)
            realac(iact,ichaca,2)=ipan(ii,3)
            ND=5
            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'non-idl act',IERV)
            IF(IERV.NE.0)GOTO 99
            KG=0
            do 640 iadata=1,5
              CALL EGETWR(OUTSTR,KG,VAL,0.,0.,'-',
     &           'non-ideal sensor ',IERV)
              realac(iact,ichaca,iadata+2)=VAL
 640        continue
 620      continue
          IF(IERV.NE.0)GOTO 99
        endif

C Reporting: Get a string describing the actuator based on common block.
        IF(ITRC.GE.1)THEN
          IIS=II
          CALL EVCNTRL(1,IIS,1,1,'A',SSTR)
          WRITE(OUTSTR,375)II,SSTR(1:LNBLNK(SSTR))
 375      FORMAT(' The actuator for loop ',I2,' is ',a)
          CALL EDISP(ITRU,OUTSTR)
        ENDIF

C Day types.
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'day types',IER)
        KG=0
        CALL EGETWI(OUTSTR,KG,npcdt(ii),0,mcdt,'F',
     &              'number of plant control day type(s)',IER)
        IF(IER.NE.0)GOTO 99

C Reporting:
        IF(ITRC.GE.1)THEN
          CALL EDISP(ITRU,' For plant control')
          CALL EDISP(ITRU,' the following day types have been defined')
          write(outstr,'(20a)') (CALENTAG(idty),idty=1,NBDAYTYPE)
          call edisp(ITRU,outstr)
        ENDIF

        nn=npcdt(ii)  ! a zero says use all day types
        if(nn.eq.0)nn=nbdaytype
        do 50 j=1,nn
          jj=j
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'validity',IER)
          IF(IER.NE.0)GOTO 99
          KG=0
          CALL EGETWI(OUTSTR,KG,ipcdv(ii,jj,1),1,365,'F',
     &              'control activation start day number',IER)
          CALL EGETWI(OUTSTR,KG,ipcdv(ii,jj,2),1,365,'F',
     &              'control activation end day number',IER)
          IF(ipcdv(ii,jj,2).LT.ipcdv(ii,jj,1))THEN
            CALL USRMSG(' ',' Start & End dates reversed.','W')
            GOTO 2000
          ENDIF

          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'periods',IER)
          KG=0
          CALL EGETWI(OUTSTR,KG,npcdp(ii,jj),1,mcdp,'F',
     &              'plant control periods during day',IER)
          IF(IER.NE.0)GOTO 99
          ll=npcdp(ii,jj)
          do 60 k=1,ll
            kk=k
            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'control',IER)
            IF(IER.NE.0)GOTO 99
            KG=0
            CALL EGETWI(OUTSTR,KG,ipctyp(ii,jj,kk),0,0,'-',
     &              'controller type (1st item)',IER)
            CALL EGETWI(OUTSTR,KG,ipclaw(ii,jj,kk),0,0,'-',
     &              'control law (2nd item)',IER)
C...........Check to see if cogen-system controller has 
C...........been specified, (ipclaw = 10) and if so, 
C...........initialize 'first_run_done' flag, which indicates 
C...........whether or not the controller's initialization
C...........procedures must be called. Note: first_run_done
C...........is a scalar boolian. Therefore, only a single
C...........instance of the cogeneration controller can be 
C...........defined in the plant control file.  Support for 
C...........multiple instances would require this flag
C...........to be re-implemented as a vector, along with 
C...........other modifications to the conroller.
            if ( ipclaw(ii,jj,kk) .eq. 10 ) then
               first_run_done = .FALSE.
            endif


C.......... Check to see of adsorption storage unit controller has
C ..........been specified (ipclaw = 13). If so, initialize 
C ..........bAds_ctl_init flag, which indicates whether or not
C ..........the controller's intialization has been done.
            if ( ipclaw(ii,jj,kk) .eq. 13 ) then
               bADS_ctl_init = .false.
            endif


            CALL EGETWR(OUTSTR,KG,tpcps(ii,jj,kk),0.,24.,'F',
     &              'activation time',IER)
            IF(IER.NE.0)GOTO 99

            IF(KK.EQ.1)THEN
              call eclose(TBCPS(II,JJ,KK),0.0,0.001,close)
              IF(.NOT.close)THEN
                CALL USRMSG(' ',' 1st period must begin @ 0!','W')
                GOTO 2000
              ENDIF
            ELSEIF(tpcps(ii,jj,kk-1).GE.tpcps(ii,jj,kk))THEN
              CALL USRMSG(' ',' periods out of order','W')
              GOTO 2000
            ENDIF

            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'misc data',IER)
            IF(IER.NE.0)GOTO 99
            KG=0
            CALL EGETWR(OUTSTR,KG,pmiscd(ii,jj,kk,1),0.,
     &           FLOAT(MISC),'F','number of data items for loop',IER)
            nitems=int(pmiscd(ii,jj,kk,1))
            IF(NITEMS.GT.0)THEN

C Since this may be the last line of the file do not advise user of
C this and do not pass ier back to calling routine.
              call egetwra(iunit,RVA,nitems,0.,0.,'-',
     &          'plt misc items',ierv)
              do 43 ij=2,nitems+1
                pmiscd(ii,jj,kk,ij)=RVA(ij-1)
  43          continue
              
C              CALL STRIPC(IUNIT,OUTSTR,0,ND,0,'data items',IERV)
C              IF(IERV.NE.0)GOTO 99
C              KG=0
C              DO 229 L2=1,NITEMS
C                CALL EGETWR(OUTSTR,KG,VAL,0.,0.,'-',
C     &              'plant control misc data ',IERV)
C                IF(IERV.NE.0) THEN
C                  CALL EDISP(ITRU,' reading continuation line...')
C                  CALL STRIPC(IUNIT,OUTSTR,0,ND,0,'++ misc',IER)
C                  IF(IER.NE.0)RETURN
C                  KG=0
C                  CALL EGETWR(OUTSTR,KG,VAL,0.,0.,'-',
C     &              'plant control misc data',IERV)
C                ENDIF
C                IF(IERV.NE.0)GOTO 99 
C                pmiscd(ii,jj,kk,l2+1)=VAL
C 229          CONTINUE
              IF(IERV.NE.0)GOTO 99
            ENDIF

C Set plant-side function generator flag.
            if(ipsn(ii,1).eq.-6)then
               pcfngn=.true.
               clfgsd(ii,1)=1.0
            endif
   60     continue

C Reporting (compacted data list):
          IF(ITRC.GE.1)call lstcntld(itru,1,II,JJ)
   50   continue
   40 continue
      goto 15

C mass flow control function.

C Establish mass control regime name and number of
C mass flow control functions.
   12 CALL LSTRIPC(IUNIT,LOUTSTR,0,ND,1,'mass flow ctl name',IER)
      flctldoc=LOUTSTR

      CALL STRIPC(IUNIT,OUTSTR,1,ND,1,'no control function.',IER)
      KG=0
      CALL EGETWI(OUTSTR,KG,ncc,-1,mcF,'F','no function',IER)
      IF(IER.NE.0)GOTO 99

C Reporting:
      IF(ITRC.GE.1)THEN
        CALL EDISP(ITRU,' ')
        WRITE(OUTSTR,360)ncc
 360    FORMAT(' Within the current project',I2,' flow control',
     &         ' functions have been specified. ')
        CALL EDISP(ITRU,OUTSTR)
        CALL EDISP248(ITRU,flctldoc,72)
      endif
      if(ncc.le.0)goto 13

C Read m. flow control functions information and perform checks
C where possible.
      do 70 i=1,ncc
        ii=i
        icc=ii

C Read header : * control mf-functions.
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'* Control mass ',IER)
        IF(IER.NE.0)GOTO 99
        HDG=OUTSTR(1:5)
        IF(HDG.NE.'* CON'.AND.HDG.NE.'* Con'.AND.HDG.NE.'* con')THEN
          CALL USRMSG(' ',' `* Control mass` not found ','W')
          GOTO 1111
        ENDIF

C Read m. flow control functions information and perform checks. 
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'sensor',IER)
        IF(IER.NE.0)GOTO 99
        KG=0
        CALL EGETWI(OUTSTR,KG,IVAL,-4,mcom,'-','sensor 1st',IER)
        ifsn(ii,1)=IVAL
        CALL EGETWI(OUTSTR,KG,IVAL,0,0,'-','sensor 2nd ',IER)
        ifsn(ii,2)=IVAL
        CALL EGETWI(OUTSTR,KG,IVAL,0,0,'-','sensor 3rd ',IER)
        ifsn(ii,3)=IVAL
        CALL EGETWI(OUTSTR,KG,IVAL,0,0,'-','sensor 4th ',IER)
        ifsn(ii,4)=IVAL
        IF(IER.NE.0)GOTO 99

C Range checking.
        if(ifsn(ii,1).lt.-4) goto 1004
        if(ifsn(ii,2).lt.0.or.ifsn(ii,3).lt.0)goto 1062

C Reporting: Get a string describing the current sensor based on common.
        IF(ITRC.GE.1)THEN
          IIS=II
          CALL EVCNTRL(2,IIS,1,1,'S',SSTR)
          WRITE(OUTSTR,'(a,i2,2a)') ' The sensor for function ',
     &      II,' ',SSTR(1:LNBLNK(SSTR))
          CALL EDISP(ITRU,OUTSTR)
        ENDIF

        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'actuator',IER)
        IF(IER.NE.0)GOTO 99
        KG=0
        CALL EGETWI(OUTSTR,KG,IV,-4,-3,'F','actuator 1st',IER)
        ifan(II,1)=IV
        CALL EGETWI(OUTSTR,KG,IV,0,0,'-','actuator 2nd',IER)
        ifan(II,2)=IV
        CALL EGETWI(OUTSTR,KG,IV,0,0,'-','actuator 3rd',IER)
        ifan(II,3)=IV
        IF(IER.NE.0)GOTO 99

C Range checking.
        if((ifan(ii,1).gt.-3).or.(ifan(ii,1).lt.-4)) goto 1005
        if(ifan(ii,2).lt.0)goto 1063

        IF(ifan(ii,1).eq.-4.AND.(ITPCMP(ifan(II,2)).EQ.220.OR.
     &    ITPCMP(ifan(II,2)).EQ.230.OR.ITPCMP(ifan(II,2)).EQ.240.OR.
     &    ITPCMP(ifan(II,2)).EQ.250))THEN
          WRITE(outs,333) ITPCMP(ifan(II,2))
 333      FORMAT(' Component type ',I3,' cannot be actuated.')
          call edisp(itru,'  ')
          call edisp(itru,outs)
          call edisp(itru,' Actuate a connection instead.')
        ENDIF

C Reporting: Get a string describing the actuator based on common block.
        IF(ITRC.GE.1)THEN
          IIS=II
          CALL EVCNTRL(2,IIS,1,1,'A',SSTR)
          WRITE(OUTSTR,75)II,SSTR(1:LNBLNK(SSTR))
          CALL EDISP(ITRU,OUTSTR)
        ENDIF

C Day types.
        CALL STRIPC(IUNIT,OUTSTR,1,ND,1,'day types',IER)
        KG=0
        CALL EGETWI(OUTSTR,KG,nfcdt(ii),0,MCDT,'F',
     &              'number of flow control day type(s)',IER)
        IF(IER.NE.0)GOTO 99

C Reporting:
        IF(ITRC.GE.1)THEN
          call edisp(itru,'  ')
          CALL EDISP(ITRU,' For mass flow network control')
          CALL EDISP(ITRU,' The following day types have been defined')
          write(outstr,'(20a)') (CALENTAG(idty),idty=1,NBDAYTYPE)
          call edisp(ITRU,outstr)
        ENDIF

        nn=nfcdt(ii)  ! a zero says use all day types
        if(nn.eq.0)nn=nbdaytype
        do 71 j=1,nn
          jj=j
          CALL STRIPC(IUNIT,OUTSTR,2,ND,1,'validity',IER)
          IF(IER.NE.0)GOTO 99
          KG=0
          CALL EGETWI(OUTSTR,KG,ifcdv(ii,jj,1),1,365,'F',
     &              'control activation start day number',IER)
          CALL EGETWI(OUTSTR,KG,ifcdv(ii,jj,2),1,365,'F',
     &              'control activation end day number',IER)
          IF(ifcdv(ii,jj,2).LT.ifcdv(ii,jj,1))THEN
            CALL USRMSG(' ',' Start & End dates reversed.','W')
            GOTO 2000
          ENDIF

          CALL STRIPC(IUNIT,OUTSTR,1,ND,1,'periods',IER)
          KG=0
          CALL EGETWI(OUTSTR,KG,nfcdp(ii,jj),1,mcdp,'F',
     &              'mass flow control periods during day',IER)
          IF(IER.NE.0)GOTO 99

C Reporting:
          IF(ITRC.GT.1)THEN

C Get date strings. For this test assume some values.
            IBDOY=IFCDV(II,JJ,1)
            IEDOY=IFCDV(II,JJ,2)
            IVBTIM=1
            NTSPH=1
            IVETIM=NTSPH*24
            CALL EPERSTR(IYEAR,IBDOY,IVBTIM,IEDOY,IVETIM,
     &           NTSPH,2,0,PERST1,PERST2,PERST3,IER)

            CALL EDISP(ITRU,' ')
            WRITE(OUTSTR,'(4A,I2,A)')' Day type ',CALENTAG(JJ),
     &        ' is valid ',PERST3(9:lnblnk(PERST3)),' with ',
     &        NFCDP(II,JJ),' periods.'
            CALL EDISP(ITRU,OUTSTR)
          ENDIF

          ll=nfcdp(ii,jj)
          do 72 k=1,ll
            kk=k
            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'control',IER)
            IF(IER.NE.0)GOTO 99
            KG=0
            CALL EGETWI(OUTSTR,KG,ifctyp(ii,jj,kk),0,0,'-',
     &              'controller type (1st item)',IER)
            CALL EGETWI(OUTSTR,KG,ifclaw(ii,jj,kk),0,0,'-',
     &              'control law (2nd item)',IER)
            CALL EGETWR(OUTSTR,KG,tfcps(ii,jj,kk),0.,24.,'F',
     &              'activation time',IER)
            IF(IER.NE.0)GOTO 99

            IF(KK.EQ.1)THEN
              call eclose(TFCPS(II,JJ,KK),0.0,0.001,close)
              IF(.NOT.close)THEN
                CALL USRMSG(' ',' 1st period must begin @ 0!','W')
                GOTO 2000
              ENDIF
            ELSEIF(tfcps(ii,jj,kk-1).GE.tfcps(ii,jj,kk))THEN
              CALL USRMSG(' ',' periods out of order','W')
              GOTO 2000
            ENDIF

            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'misc data',IER)
            IF(IER.NE.0)GOTO 99
            KG=0
            CALL EGETWR(OUTSTR,KG,fmiscd(ii,jj,kk,1),0.,
     &         FLOAT(MISC),'F','number of data items for function.',IER)
            nitems=int(fmiscd(ii,jj,kk,1))

            IF(NITEMS.GT.0)THEN
              call egetwra(iunit,RVA,nitems,0.,0.,'-',
     &          'flow misc items',ierv)
              do 44 ij=2,nitems+1
                fmiscd(ii,jj,kk,ij)=RVA(ij-1)
  44          continue
              IF(IERV.NE.0)GOTO 99
            ENDIF
   72     continue

C Reporting (compacted data list):
          IF(ITRC.GE.1)call lstcntld(itru,2,II,JJ)
   71   continue

C If control component active read connections.
        if(ifan(ii,1).eq.-4)then
          ICNN=0
          DO 73 ICCN=1,ifan(ii,3)
            CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'connection data',IER)

C The number of supplementary data items is the number of items on
C the line -3. (There are always at least three items on the line.
            nsup=nd-3
            KG=0

C Read Node on +ve side and its index.
            CALL EGETW(OUTSTR,KG,WORD,'W','+ve node',IER)
            NDPOS=WORD(1:12)
            call GETND(NDPOS,iv,1,'+ve node')
            if(iv.eq.0)goto 999

C Read Node on -ve side and its index.
            CALL EGETW(OUTSTR,KG,WORD,'W','-ve node',IER)
            NDNEG=WORD(1:12)
            call GETND(NDNEG,iv,1,'-ve node')
            if(iv.eq.0)goto 999

C Name of linking fluid flow component and its index.
            CALL EGETW(OUTSTR,KG,WORD,'W','Component name',IER)
            CMPID=WORD(1:12)
            call GETCMP(CMPID,iv,1,'Component name')
            if(iv.eq.0)GOTO 999

            call GETCN(NDPOS,NDNEG,CMPID,ICNN,1,'assoc ctl conn')
            if(ICNN.eq.0)goto 999

C Remember connection index for this -4 associated control. 
            iasocc(ii,ICCN)=ICNN
            nfsup(ii)=nsup
            if(nsup.eq.0)then

C Debug.
C              write(6,*) ' supplementary node data zeroed...'

              NDSCNN(ICNN,1)=0
            else
C If supplementary node names for this component read them.

C Debug.
C              write(6,*) ' supplementary node names to be read in...'

              do 91 isup=1,nsup
                CALL EGETW(OUTSTR,KG,WORD,'W','suppl node',IER)
                NODID=WORD(1:12)
                call GETND(NODID,iv,1,'supl node')
                if(iv.eq.0)goto 999
                NDSCNN(ICNN,isup)=iv
  91          continue
            endif
   73     continue
        ENDIF
   70 continue
      goto 15

C Establish power control regime name and number of
C control loops.
   18 CALL LSTRIPC(IUNIT,LOUTSTR,0,ND,1,'power ctl name',IER)
      elctldoc=LOUTSTR

      CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'no power control loops',IER)
      KG=0
      CALL EGETWI(OUTSTR,KG,necl,-1,MCF,'F','no power loops',IER)
      IF(IER.NE.0)GOTO 99

C Reporting:
      IF(ITRC.GE.1)THEN
        CALL EDISP(ITRU,' ')
        WRITE(OUTSTR,550)necl
 550    FORMAT(' Within the current project',I2,' power control loops',
     &         ' have been specified. ')
        CALL EDISP(ITRU,OUTSTR)
        CALL EDISP(ITRU,' power control is')
        CALL EDISP248(ITRU,elctldoc,72)
        CALL EDISP(ITRU,' ')
      ENDIF
      if(necl.le.0)goto 13

C Read control loops information and perform checks
C where possible.
      do 47 i=1,necl
        ii=i
C        iecl=ii

C Read header : * control loops.
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'* CONTROL ',IER)
        IF(IER.NE.0)GOTO 99
        HDG=OUTSTR(1:5)
        IF(HDG.NE.'* CON'.AND.HDG.NE.'* Con'.AND.HDG.NE.'* con')THEN
          CALL USRMSG(' ',' * CONTROL string not found ','W')
          GOTO 1111
        ENDIF

C Read control functions information and perform checks. If 4 items
C then new format file, otherwise old format so set IBSN(II,4)=0.
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'sensor',IER)
        IF(IER.NE.0)GOTO 99
        KG=0
        CALL EGETWI(OUTSTR,KG,IVAL,-9,0,'F','sensor 1st',IER)
        iesn(ii,1)=IVAL
        CALL EGETWI(OUTSTR,KG,IVAL,0,0,'-','sensor 2nd ',IER)
        iesn(ii,2)=IVAL
        CALL EGETWI(OUTSTR,KG,IVAL,0,0,'-','sensor 3rd ',IER)
        iesn(ii,3)=IVAL
        IF(IER.NE.0)GOTO 99

C Range checking.
        if(iesn(ii,2).lt.0)goto 1092

C Reporting: Get a string describing the current sensor based on common.
        IF(ITRC.GE.1)THEN
          IIS=II
          CALL EVCNTRL(4,IIS,1,1,'S',SSTR) 
          WRITE(OUTSTR,564)II,SSTR(1:LNBLNK(SSTR))
 564      FORMAT(' The sensor for loop ',I2,' ',a)
          CALL EDISP(ITRU,OUTSTR)
        ENDIF

        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'actuator',IER)
        IF(IER.NE.0)GOTO 99
        KG=0
        CALL EGETWI(OUTSTR,KG,IV,-9,mcom,'F','actuator 1st',IER)
        iean(II,1)=IV
        CALL EGETWI(OUTSTR,KG,IV,0,0,'-','actuator 2nd',IER)
        iean(II,2)=IV
        CALL EGETWI(OUTSTR,KG,IV,0,0,'-','actuator 3rd',IER)
        iean(II,3)=IV
        IF(IER.NE.0)GOTO 99

C Range checking.
        if(iean(ii,2).lt.0)goto 1093

C Reporting: Get a string describing the actuator based on common block.
        IF(ITRC.GE.1)THEN
          IIS=II
          CALL EVCNTRL(4,IIS,1,1,'A',SSTR)
          WRITE(OUTSTR,575)II,SSTR(1:LNBLNK(SSTR)) 
 575      FORMAT(' The actuator for loop ',I2,' is ',a)
          CALL EDISP(ITRU,OUTSTR)
        ENDIF

C Day types.
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'day types',IER)
        KG=0
        CALL EGETWI(OUTSTR,KG,necdt(ii),0,MCDT,'F',
     &              'number of power control day type(s)',IER)
        IF(IER.NE.0)GOTO 99

C Reporting:
        IF(ITRC.GE.1)THEN
          call edisp(itru,'  ')
          CALL EDISP(ITRU,' For electrical control')
          CALL EDISP(ITRU,' The following day types have been defined')
          write(outstr,'(20a)') (CALENTAG(idty),idty=1,NBDAYTYPE)
          call edisp(ITRU,outstr)
        ENDIF

        nn=necdt(ii)  ! a zero says use all day types
        if(nn.eq.0)nn=nbdaytype
        do 57 j=1,nn
          jj=j
          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'validity',IER)
          IF(IER.NE.0)GOTO 99
          KG=0
          CALL EGETWI(OUTSTR,KG,iecdv(ii,jj,1),1,365,'F',
     &              'control activation start day number',IER)
          CALL EGETWI(OUTSTR,KG,iecdv(ii,jj,2),1,365,'F',
     &              'control activation end day number',IER)
          IF(iecdv(ii,jj,2).LT.iecdv(ii,jj,1))THEN
            CALL USRMSG(' ',' Start & End dates reversed.','W')
            GOTO 2000
          ENDIF

          CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'periods',IER)
          KG=0
          CALL EGETWI(OUTSTR,KG,necdp(ii,jj),1,MCDP,'F',
     &              'power control periods during day',IER)
          IF(IER.NE.0)GOTO 99
          ll=necdp(ii,jj)
          do 67 k=1,ll
            kk=k
            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'control',IER)
            IF(IER.NE.0)GOTO 99
            KG=0
            CALL EGETWI(OUTSTR,KG,iectyp(ii,jj,kk),0,0,'-',
     &              'controller type (1st item)',IER)
            CALL EGETWI(OUTSTR,KG,ieclaw(ii,jj,kk),0,0,'-',
     &              'control law (2nd item)',IER)
            CALL EGETWR(OUTSTR,KG,tecps(ii,jj,kk),0.,24.,'F',
     &              'activation time',IER)
            IF(IER.NE.0)GOTO 99

C Write out string describing the controller type.
            IF(ITRC.GE.1)THEN
              IIS=II
              CALL EVCNTRL(4,IIS,1,1,'T',SSTR)
              CALL EDISP(ITRU,' ')
              WRITE(OUTSTR,595)II,SSTR(1:LNBLNK(SSTR)) 
 595          FORMAT(' Controller type for loop ',I2,' is ',a)
              CALL EDISP(ITRU,OUTSTR)
            ENDIF             

            IF(KK.EQ.1)THEN
              call eclose(TECPS(II,JJ,KK),0.0,0.001,close)
              IF(.NOT.close)THEN
                CALL USRMSG(' ',' 1st period must begin @ 0!','W')
                GOTO 2000
              ENDIF
            ELSEIF(tecps(ii,jj,kk-1).GE.tecps(ii,jj,kk))THEN
              CALL USRMSG(' ',' periods out of order','W')
              GOTO 2000
            ENDIF

            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'misc data',IER)
            IF(IER.NE.0)GOTO 99
            KG=0
            CALL EGETWR(OUTSTR,KG,emiscd(ii,jj,kk,1),0.,
     &           FLOAT(MISC),'F','number of data items for loop',IER)
             nitems=int(emiscd(ii,jj,kk,1))

            IF(NITEMS.GT.0)THEN
              call egetwra(iunit,RVA,nitems,0.,0.,'-',
     &          'power misc items',ierv)
              do 45 ij=2,nitems+1
                emiscd(ii,jj,kk,ij)=RVA(ij-1)
  45          continue
              IF(IERV.NE.0)GOTO 99
            ENDIF
   67     continue

C Reporting (compacted data list):
          IF(ITRC.GE.1)call lstcntld(itru,4,II,JJ)
   57   continue
   47 continue

      goto 15

C Global control function.

C Establish global control regime name and number of
C global control functions.
  113 CALL LSTRIPC(IUNIT,LOUTSTR,0,ND,1,'global ctl name',IER)
      glctldoc=LOUTSTR

      CALL STRIPC(IUNIT,OUTSTR,1,ND,1,'no control function.',IER)
      KG=0
      CALL EGETWI(OUTSTR,KG,ngf,-1,MCF,'F','no function',IER)
      IF(IER.NE.0)GOTO 99


C Reporting:
      IF(ITRC.GE.1)THEN
        CALL EDISP(ITRU,' ')
        WRITE(OUTSTR,460)ngf
 460    FORMAT(' Within the current project',I2,' global control',
     &         ' functions have been specified. ')
        CALL EDISP(ITRU,OUTSTR)
        CALL EDISP(ITRU,' global control is')
        CALL EDISP248(ITRU,glctldoc,72)
        CALL EDISP(ITRU,' ')
      ENDIF
      if(ngf.le.0)goto 13

C Read global control functions information and perform checks
C where possible.
      do 90 i=1,ngf
        ii=i

C Read header : * control gb-functions.
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'* Global ',IER)
        IF(IER.NE.0)GOTO 99
        HDG=OUTSTR(1:5)
        IF(HDG.NE.'* GLO'.AND.HDG.NE.'* Glo'.AND.HDG.NE.'* glo')THEN
          CALL USRMSG(' ',' `* Global` not found ','W')
          GOTO 1111
        ENDIF

C Read global control functions information and perform checks. 
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'sensor',IER)
        IF(IER.NE.0)GOTO 99
        KG=0
        CALL EGETWI(OUTSTR,KG,IVAL,-6,mcom,'-','sensor 1st',IER)
        igsn(ii,1)=IVAL
        CALL EGETWI(OUTSTR,KG,IVAL,0,0,'-','sensor 2nd ',IER)
        igsn(ii,2)=IVAL
        CALL EGETWI(OUTSTR,KG,IVAL,0,0,'-','sensor 3rd ',IER)
        igsn(ii,3)=IVAL
        IF(IER.NE.0)GOTO 99

C Range checking.
        if(igsn(ii,1).lt.-4) goto 1004
        if(igsn(ii,2).lt.0.or.igsn(ii,3).lt.0)goto 1062

C Reporting 

        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'actuator',IER)
        IF(IER.NE.0)GOTO 99
        KG=0
        CALL EGETWI(OUTSTR,KG,IV,-4,MCOM,'F','actuator 1st',IER)
        igan(II,1)=IV
        CALL EGETWI(OUTSTR,KG,IV,0,0,'-','actuator 2nd',IER)
        igan(II,2)=IV
        CALL EGETWI(OUTSTR,KG,IV,0,0,'-','actuator 3rd',IER)
        igan(II,3)=IV
        IF(IER.NE.0)GOTO 99

C Range checking.
c jmq: do later: if((igan(ii,1).gt.-3).or.(igan(ii,1).lt.-4)) goto 1005

C Day types.
        CALL STRIPC(IUNIT,OUTSTR,1,ND,1,'day types',IER)
        KG=0
        CALL EGETWI(OUTSTR,KG,ngcdt(ii),0,mcdt,'F',
     &              'no. of global control day type(s)',IER)
        IF(IER.NE.0)GOTO 99

C Reporting:
        IF(ITRC.GE.1)THEN
          CALL EDISP(ITRU,' For global control')
          CALL EDISP(ITRU,' The following day types have been defined')
          write(outstr,'(20a)') (CALENTAG(idty),idty=1,NBDAYTYPE)
          call edisp(ITRU,outstr)
        ENDIF

        nn=ngcdt(ii)  ! a zero says use all day types
        if(nn.eq.0)nn=nbdaytype
        do 89 j=1,nn
          jj=j
          CALL STRIPC(IUNIT,OUTSTR,2,ND,1,'validity',IER)
          IF(IER.NE.0)GOTO 99
          KG=0
          CALL EGETWI(OUTSTR,KG,igcdv(ii,jj,1),1,365,'F',
     &              'control activation start day number',IER)
          CALL EGETWI(OUTSTR,KG,igcdv(ii,jj,2),1,365,'F',
     &              'control activation end day number',IER)
          IF(igcdv(ii,jj,2).LT.igcdv(ii,jj,1))THEN
            CALL USRMSG(' ',' Start & End dates reversed.','W')
            GOTO 2000
          ENDIF

          CALL STRIPC(IUNIT,OUTSTR,1,ND,1,'periods',IER)
          KG=0
          CALL EGETWI(OUTSTR,KG,ngcdp(ii,jj),1,mcdp,'F',
     &              'global control periods during day',IER)
          IF(IER.NE.0)GOTO 99
          ll=ngcdp(ii,jj)
          do 92 k=1,ll
            kk=k
            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'control',IER)
            IF(IER.NE.0)GOTO 99
            KG=0
            CALL EGETWI(OUTSTR,KG,igctyp(ii,jj,kk),0,0,'-',
     &              'controller type (1st item)',IER)
            CALL EGETWI(OUTSTR,KG,igclaw(ii,jj,kk),0,0,'-',
     &              'control law (2nd item)',IER)
            CALL EGETWR(OUTSTR,KG,tgcps(ii,jj,kk),0.,24.,'F',
     &              'activation time',IER)
            IF(IER.NE.0)GOTO 99

            IF(KK.EQ.1)THEN
              call eclose(TGCPS(II,JJ,KK),0.0,0.001,close)
              IF(.NOT.close)THEN
                CALL USRMSG(' ',' 1st period must begin @ 0!','W')
                GOTO 2000
              ENDIF
            ELSEIF(tgcps(ii,jj,kk-1).GE.tgcps(ii,jj,kk))THEN
              CALL USRMSG(' ',' periods out of order','W')
              GOTO 2000
            ENDIF

            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'misc data',IER)
            IF(IER.NE.0)GOTO 99
            KG=0
            CALL EGETWR(OUTSTR,KG,gmiscd(ii,jj,kk,1),0.,
     &         FLOAT(MISC),'F','number of data items for function.',IER)
             nitems=int(gmiscd(ii,jj,kk,1))

            IF(NITEMS.GT.0)THEN
              call egetwra(iunit,RVA,nitems,0.,0.,'-',
     &          'global misc items',ierv)
              do 46 ij=2,nitems+1
                gmiscd(ii,jj,kk,ij)=RVA(ij-1)
  46          continue
              IF(IERV.NE.0)GOTO 99
            ENDIF
   92     continue
   89   continue
   90 continue
      goto 15

C  Establish optical control regime name (opticdoc) and number of
C  control functions (NOF).
 19   CALL LSTRIPC(IUNIT,LOUTSTR,0,ND,1,'optical control docs',IER)
      OPTICDOC=LOUTSTR

      CALL STRIPC(IUNIT,OUTSTR,1,ND,1,'no cntrl functions',IER)
      KG=0
      CALL EGETWI(OUTSTR,KG,NOF,-1,MCF,'F','no function',IER)
      IF(IER.NE.0)GOTO 99

C Reporting:
      IF(ITRC.GE.1)THEN
        CALL EDISP(ITRU,' ')
        WRITE(OUTSTR,361)nof
 361    FORMAT(' Within the current project',I2,' optical control',
     &         ' functions have been specified. ')
        CALL EDISP(ITRU,OUTSTR)
        CALL EDISP248(ITRU,opticdoc,72)
      endif

      if(nof.le.0)goto 13

c For each control function.
      DO 100 I=1,NOF
        II=I
        IOF=II

C Read header : * OPTICS FUNCTION.
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'* Control optics',IER)
        IF(IER.NE.0)GOTO 99
        HDG=OUTSTR(1:5)
        IF(HDG.NE.'* OPT'.AND.HDG.NE.'* Con'.AND.HDG.NE.'* con')THEN
          CALL USRMSG(' ',' `* Control optics` string not found ','W')
          GOTO 1111
        ENDIF

C Read optical functions information and perform checks.
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'sensor',IER)
        IF(IER.NE.0)GOTO 99
        KG=0
        CALL EGETWI(OUTSTR,KG,IVAL,0,0,'-','sensor 1st',IER)
        IOSN(II,1)=IVAL
        CALL EGETWI(OUTSTR,KG,IVAL,0,0,'-','sensor 2nd ',IER)
        IOSN(II,2)=IVAL
        CALL EGETWI(OUTSTR,KG,IVAL,0,0,'-','sensor 3rd ',IER)
        IOSN(II,3)=IVAL
        IF(IER.NE.0)GOTO 99

C Range checking.
C<<<<<<<<CHECK THE -5 HERE - It may just need to be 0 >>>>>>>>>
        if(iosn(ii,1).lt.-5.or.iosn(ii,1).gt.mcom)goto 1003

C Reporting: Get a string describing the current sensor based on common.
        IF(ITRC.GE.1)THEN
          IIS=II
          CALL EVCNTRL(5,IIS,1,1,'S',SSTR)
          WRITE(OUTSTR,'(a,i2,2a)')' The sensor for function ',II,' ',
     &      SSTR(1:LNBLNK(SSTR))
          CALL EDISP(ITRU,OUTSTR)
        ENDIF

        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'actuator',IER)
        IF(IER.NE.0)GOTO 99
        KG=0
        CALL EGETWI(OUTSTR,KG,IV,-5,MCOM,'F','actuator 1st',IER)
        IOAN(II,1)=IV
        CALL EGETWI(OUTSTR,KG,IV,0,MCOM,'F','actuator 2nd',IER)
        IOAN(II,2)=IV
        CALL EGETWI(OUTSTR,KG,IV,0,0,'-','actuator 3rd',IER)
        IOAN(II,3)=IV
        IF(IER.NE.0)GOTO 99

C Range checking.
C<<<<<<<<<FIX THE RANGE CHECK HERE >>>>>>>>>>>>>
        if(ioan(ii,1).lt.-5.or.ioan(ii,1).gt.MS)goto 1005

C Reporting: Get a string describing the actuator based on common block.
        IF(ITRC.GE.1)THEN
          IIS=II
          CALL EVCNTRL(5,IIS,1,1,'A',SSTR)
          WRITE(OUTSTR,'(a,i2,2a)')' The actuator for function ',II,
     &      ' is ',SSTR(1:LNBLNK(SSTR))
          CALL EDISP(ITRU,OUTSTR)
        ENDIF

C Day types.
        CALL STRIPC(IUNIT,OUTSTR,1,ND,1,'day types',IER)
        KG=0
        CALL EGETWI(OUTSTR,KG,NOCDT(II),0,MCDT,'F',
     &              'number of control day types',IER)
        IF(IER.NE.0)GOTO 99

C Reporting:
        IF(ITRC.GE.1)THEN
          CALL EDISP(ITRU,' For optical control')
          CALL EDISP(ITRU,' The following day types have been defined')
          write(outstr,'(20a)') (CALENTAG(idty),idty=1,NBDAYTYPE)
          call edisp(ITRU,outstr)
        ENDIF

        nn=nocdt(ii)  ! a zero says use all day types
        if(nn.eq.0)nn=nbdaytype
        do 200 j=1,nn ! NUMBER OF DAY TYPES
          jj=j
          CALL STRIPC(IUNIT,OUTSTR,2,ND,1,'validity',IER)
          IF(IER.NE.0)GOTO 99
          KG=0
          CALL EGETWI(OUTSTR,KG,IOCDV(II,JJ,1),1,365,'F',
     &              'control activation start day number',IER)
          CALL EGETWI(OUTSTR,KG,IOCDV(II,JJ,2),1,365,'F',
     &              'control activation end day number',IER)
          IF(IOCDV(II,JJ,2).LT.IOCDV(II,JJ,1))THEN
            CALL USRMSG(' ',' Start & End dates reversed.','W')
            GOTO 2000
          ENDIF

          CALL STRIPC(IUNIT,OUTSTR,1,ND,1,'periods',IER)
          KG=0
          CALL EGETWI(OUTSTR,KG,NOCDP(II,JJ),1,MCDP,'F',
     &              'control periods during day',IER)
          IF(IER.NE.0)GOTO 99

          LL=NOCDP(II,JJ)
          DO 300 K=1,LL ! NUMBER OF PERIODS
            KK=K
            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'control',IER)
            IF(IER.NE.0)GOTO 99
            KG=0

C Controller types are not used for optical controls, yet. They are
C written though as 0 in the controls file to keep consistency with the
C rest of the control domains
            CALL EGETWI(OUTSTR,KG,IOCTYP(II,JJ,KK),0,99,'W',
     &              'controller type (1st item)',IER)
C<<<<<<<Confirm the 0 to 1 range check >>>>>>>>>
            CALL EGETWI(OUTSTR,KG,IOCLAW(II,JJ,KK),0,2,'W',
     &              'control law (2nd item)',IER)
            CALL EGETWR(OUTSTR,KG,TOCPS(II,JJ,KK),0.,24.,'F',
     &              'activation time',IER)
            IF(IER.NE.0)GOTO 99

            IF(KK.EQ.1)THEN
              call eclose(TOCPS(II,JJ,KK),0.0,0.001,close)
              IF(.NOT.close)THEN
                CALL USRMSG(' ',' 1st period must begin @ 0!','W')
                GOTO 2000
              ENDIF
            ELSEIF(TOCPS(II,JJ,KK-1).GE.TOCPS(II,JJ,KK))THEN
              CALL USRMSG(' ',' periods out of order','W')
              GOTO 2000
            ENDIF

            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'data next',IER)
            IF(IER.NE.0)GOTO 99
            KG=0
            CALL EGETWR(OUTSTR,KG,OMISCD(II,JJ,KK,1),0.,
     &       FLOAT(MISC),'F','number of data items for function',IER)
            NITEMS=INT(OMISCD(II,JJ,KK,1))

            IF(NITEMS.GT.0)THEN
              call egetwra(iunit,RVA,nitems,0.,0.,'-','misc items',
     &          ierv)
              do 142 ij=2,nitems+1
                OMISCD(II,JJ,KK,ij)=RVA(ij-1)
 142          continue
              IF(IER.NE.0)GOTO 99
            ENDIF
  300     CONTINUE

C Reporting of all periods in this day type (compacted data list):
          IF(ITRC.GE.1)call lstcntld(itru,5,II,JJ)
  200   CONTINUE
  100 CONTINUE
      goto 15
        
C Complex fenestration control
  
C  Establish complex fenestration control regime notes (CFCctldoc) 
C and number of control functions (nCFCctlloops).
   21 CALL LSTRIPC(IUNIT,LOUTSTR,0,ND,1,
     &      'complex fen. control notes',IER)
      CFCctldoc=LOUTSTR

      CALL STRIPC(IUNIT,OUTSTR,1,ND,1,'no cntrl loops',IER)
      KG=0
      CALL EGETWI(OUTSTR,KG,nCFCctlloops,-1,MCF,'F','no loops',IER)
      IF(IER.NE.0)GOTO 99

C Reporting:
      IF(ITRC.GE.1)THEN
        CALL EDISP(ITRU,' ')
        WRITE(OUTSTR,2501)nCFCctlloops
2501    FORMAT(' Within the current model',I3,' control loops',
     &         ' have been specified. ')
        CALL EDISP(ITRU,OUTSTR)
        CALL EDISP(ITRU,' The overall project control is ')
        CALL EDISP248(ITRU,ctldoc,72)
        CALL EDISP(ITRU,' and the complex fenestration control is')
        CALL EDISP248(ITRU,CFCctldoc,72)
        CALL EDISP(ITRU,' ')
      ENDIF

      if(nCFCctlloops.le.0)goto 13

c For each control function.
      DO 710 I=1,nCFCctlloops
        II=I
        ICF=II

C Read header : * CONTROL FUNCTION.
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'* CONTROL ',IER)
        IF(IER.NE.0)GOTO 99
        HDG=OUTSTR(1:5)
        IF(HDG.NE.'* CON'.AND.HDG.NE.'* Con'.AND.HDG.NE.'* con')THEN
          CALL USRMSG(' ',' * CONTROL string not found ','W')
          GOTO 1111
        ENDIF

C Read control functions information and perform checks. If 4 items
C then new format file, otherwise old format so set IBSN(II,4)=0.
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'sensor',IER)
        IF(IER.NE.0)GOTO 99
        IF(ND.GE.3)THEN
          KG=0
          CALL EGETWI(OUTSTR,KG,IVAL,0,0,'-','sensor 1st',IER)
          iCFCsensor(II,1)=IVAL
          CALL EGETWI(OUTSTR,KG,IVAL,0,0,'-','sensor 2nd ',IER)
          iCFCsensor(II,2)=IVAL
          CALL EGETWI(OUTSTR,KG,IVAL,0,0,'-','sensor 3rd ',IER)
          iCFCsensor(II,3)=IVAL
          IF(ND.EQ.4)THEN
            CALL EGETWI(OUTSTR,KG,IVAL,0,0,'-','sensor pnter',IER)
            iCFCsensor(II,4)=IVAL
          ELSE
            iCFCsensor(II,4)=0
          ENDIF
          IF(IER.NE.0)GOTO 99

C Range checking. TODO: VERIFY THIS FOR CFC sensors
          if((iCFCsensor(ii,1).lt.-6.or.iCFCsensor(ii,1).gt.mcom)
     &        .and.iCFCsensor(ii,1).ne.-99)goto 1003
          if(abs(iCFCsensor(ii,4)).gt.mcf)goto  1002
          if((iCFCsensor(ii,2).lt.0.or.iCFCsensor(ii,3).lt.0)
     &        .and.iCFCsensor(ii,1).ne.-99)goto 1004
        ENDIF

C Reporting: Get a string describing the current sensor based on common.
        IF(ITRC.GE.1)THEN
          IIS=II
          CALL EVCNTRL(6,IIS,1,1,'S',SSTR)
          WRITE(OUTSTR,'(a,i2,2a)') ' The sensor for function ',
     &      II,' ',SSTR(1:LNBLNK(SSTR))
          CALL EDISP(ITRU,' ')
          CALL EDISP(ITRU,OUTSTR)
        ENDIF

        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'actuator',IER)
        IF(IER.NE.0)GOTO 99
        IF(ND.GE.3)THEN
          KG=0
          CALL EGETWI(OUTSTR,KG,IV,-3,MCOM,'F','actuator 1st',IER)
          iCFCactuator(II,1)=IV
          CALL EGETWI(OUTSTR,KG,IV,0,MCOM,'F','actuator 2nd',IER)
          iCFCactuator(II,2)=IV
          CALL EGETWI(OUTSTR,KG,IV,0,0,'-','actuator 3rd',IER)
          iCFCactuator(II,3)=IV
          IF(IER.NE.0)GOTO 99

C Range checking.
          if(iCFCactuator(ii,1).lt.0.or.
     &     iCFCactuator(ii,1).gt.2)goto 1005
          if(iCFCactuator(ii,2).le.0.or.iCFCactuator(ii,3).lt.0.or.
     &     iCFCactuator(ii,2).gt.mcom.or.
     &     iCFCactuator(ii,3).gt.mcfc)goto 1005
        ENDIF

C Reporting: Get a string describing the actuator based on common block.
        IF(ITRC.GE.1)THEN
          IIS=II
          CALL EVCNTRL(6,IIS,1,1,'A',SSTR)
          WRITE(OUTSTR,775)II,SSTR(1:LNBLNK(SSTR))
 775      FORMAT(' The actuator for function ',I2,' is ',a)
          CALL EDISP(ITRU,OUTSTR)
        ENDIF

C Day types.
        CALL STRIPC(IUNIT,OUTSTR,1,ND,1,'day types',IER)
        KG=0
        CALL EGETWI(OUTSTR,KG,nCFCctldaytypes(II),0,MCDT,'F',
     &              'number of control day type(s)',IER)
        IF(IER.NE.0)GOTO 99

C Reporting:
        IF(ITRC.GE.1)THEN
          IF(nCFCctldaytypes(II).EQ.0)THEN
            CALL EDISP(ITRU,
     &      ' The function day types are Weekdays, Saturdays & Sundays')
          ELSE
            WRITE(OUTSTR,778)nCFCctldaytypes(II)
 778        FORMAT(' There have been ',I2,' day type(s) defined.')
            CALL EDISP(ITRU,OUTSTR)
          ENDIF
        ENDIF

        nn=nCFCctldaytypes(ii)
        if(nn.eq.0)nn=3
        do 720 j=1,nn
          jj=j
          CALL STRIPC(IUNIT,OUTSTR,2,ND,1,'validity',IER)
          IF(IER.NE.0)GOTO 99
          KG=0
          CALL EGETWI(OUTSTR,KG,iCFCctldatevalid(II,JJ,1),1,365,'F',
     &              'control activation start day number',IER)
          CALL EGETWI(OUTSTR,KG,iCFCctldatevalid(II,JJ,2),1,365,'F',
     &              'control activation end day number',IER)
          IF(iCFCctldatevalid(II,JJ,2).LT.
     &     iCFCctldatevalid(II,JJ,1))THEN
            CALL USRMSG(' ',' Start & End dates reversed.','W')
            GOTO 2000
          ENDIF

          CALL STRIPC(IUNIT,OUTSTR,1,ND,1,'periods',IER)
          KG=0
          CALL EGETWI(OUTSTR,KG,nCFCdayctlperiods(II,JJ),1,MCDP,'F',
     &              'control periods during day',IER)
          IF(IER.NE.0)GOTO 99

          LL=nCFCdayctlperiods(II,JJ)
          DO 730 K=1,LL
            KK=K
            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'control',IER)
            IF(IER.NE.0)GOTO 99
            KG=0
            CALL EGETWI(OUTSTR,KG,iCFCctltype(II,JJ,KK),0,99,'W',
     &              'controller type (1st item)',IER)
            CALL EGETWI(OUTSTR,KG,iCFCctllaw(II,JJ,KK),0,99,'W',
     &              'control law (2nd item)',IER)
            CALL EGETWR(OUTSTR,KG,CFCctlperiodstart(II,JJ,KK),
     &                   0.,24.,'F','activation time',IER)
            IF(IER.NE.0)GOTO 99

            IF(KK.EQ.1)THEN
              call eclose(CFCctlperiodstart(II,JJ,KK),0.0,0.001,close)
              IF(.NOT.close)THEN
                CALL USRMSG(' ',' 1st period must begin @ 0!','W')
                GOTO 2000
              ENDIF
            ELSEIF(CFCctlperiodstart(II,JJ,KK-1).GE.
     &             CFCctlperiodstart(II,JJ,KK))THEN
              CALL USRMSG(' ',' periods out of order','W')
              GOTO 2000
            ENDIF

            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'data next',IER)
            IF(IER.NE.0)GOTO 99
            KG=0
            CALL EGETWR(OUTSTR,KG,CFCmiscdata(II,JJ,KK,1),0.,
     &       FLOAT(MISC),'F','number of data items for function',IER)
            NITEMS=INT(CFCmiscdata(II,JJ,KK,1))

            IF(NITEMS.GT.0)THEN
              call egetwra(iunit,RVA,nitems,0.,0.,'-','misc items',
     &          ierv)
              do 742 ij=2,nitems+1
                CFCmiscdata(II,JJ,KK,ij)=RVA(ij-1)
 742          continue
              IF(IER.NE.0)GOTO 99
            ENDIF
  730     CONTINUE

C Reporting of all periods in this day type (compacted data list):
          IF(ITRC.GE.1)call lstcntld(itru,6,II,JJ)
  720   CONTINUE
  710 CONTINUE
  
C Check that NCF, NCL, NCC, NOF, NGF and nCFCctlloops are zero. 
C If so there may be more information to be read.
   15 if(ncf.eq.0.or.ncl.eq.0.or.ncc.eq.0.or.ngf.eq.0.or.
     &   nof.eq.0.or.nCFCctlloops.eq.0)goto 2

C At present there is no structure to use a user defined control
C file - therefore set NCF=0, NCL=0, NCC=0, NGF=0, NOF=0.
   13 if(ncf.eq.-1)ncf=0
      if(ncl.eq.-1)ncl=0
      if(ncc.eq.-1)ncc=0
      if(ngf.eq.-1)ngf=0
      if(nof.eq.-1)nof=0
      if(nCFCctlloops.eq.-1)nCFCctlloops=0
      
C Return.
      goto 9

C Error messages.
  999 write(outs,'(a,i3,2a)') 'Error reading connection',icnn,
     &  ' from ',WORD(1:lnblnk(WORD))
      if(dll)then
        dllsubr='EZCTLR'
        dllmesg=outs
        ier=2
        return
      else
        call edisp(iuout,outs)
        CALL EDISP(ITRU,OUTSTR)
        CALL ERPFREE(IUNIT,ISTAT)
        ier=1
        return
      endif

C Make up specific error messages.
 1002 t40='No of control functions out of range in '
      goto 2000
 1003 t40='Control sensor type out of range in '
      goto 2000
 1004 t40='Inappropriate sensor definition in '
      goto 2000
 1005 t40='Control actuator type out of range in '
      goto 2000
 1052 CALL USRMSG(' ',' Plant sensor item 2, 3 or 4 < 0.','W')
      goto 3000
 1053 CALL USRMSG(' ',' Plant actuator item 2, 3 or 4 < 0.','W')
      goto 3000
 1062 CALL USRMSG(' ',' Mass f. control 2nd or 3rd item < 0.','W')
      goto 4000
 1063 CALL USRMSG(' ',' Mass f. actuat. 2nd or 3rd item < 0.','W')
      goto 4000
 1081 CALL USRMSG(' An undefined control was referenced. This may',
     &            ' be a read error, please check carefully.','W')
      goto 1111
 1092 CALL USRMSG(' ',' Power ctl. sens. 2nd or 3rd item< 0.','W')
      goto 3000
 1093 CALL USRMSG(' ',' Power ctl actuat. 2nd or 3rd item< 0.','W')
      goto 3000

C <<<<<<<error messages for optical controls still to be implemented >>>>>>>>

 1082 CALL EDISP(ITRU,' A construction substitution control function ')
      CALL EDISP(ITRU,' cannot be directly allocated to a zone.')
      CALL EDISP(ITRU,' Instead it must be referenced from another ')
      CALL EDISP(ITRU,' control function.  This is a fatal error. ')
      goto 1111

 2000 WRITE(OUTS,'(2a,i3,a,i3,3a)') t40(1:lnblnk(t40)),
     & ' zone control ',ICF,' on day',NBCDT(II),' @ ',outstr(1:40),'...'
      if(dll)then
        dllsubr='EZCTLR'
        dllmesg=outs
        ier=2
        return
      else
        call edisp(iuout,outs)
        ier=1
        return
      endif

 3000 WRITE(OUTS,3001)icl,npcdt(ii)
 3001 FORMAT(' Plant control loop',I3,' day type',I3,': the',
     &       ' offending line was')
      CALL USRMSG(OUTS,OUTSTR,'W')

 4000 WRITE(OUTS,4001)icc,nfcdt(ii)
 4001 FORMAT(' Flow control functions',I3,' day type',I3,': the',
     &       ' offending line was')
      CALL USRMSG(OUTS,OUTSTR,'W')

C "Future" message for optical controls
C 5000 WRITE(OUTS,5001)iof,nocdt(ii)
C 5001 FORMAT(' Optical control functions',I3,' day type',I3,': the',
C     &       ' offending line was')
C      CALL USRMSG(OUTS,OUTSTR,'W')

 1111 CALL USRMSG(' ',' Returning to menu. ','W')
      IER=1

C Close file before exiting.
   9  CALL ERPFREE(IUNIT,ISTAT)

C Re-initialise control file for fuzzy data.
7     CALL EFOPSEQ(IUNIT,LCTLF,1,IER)
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF

C Look for definition of fuzzy data referenced by other
C control laws. If fuzzy data not found set an error condition
C free control file and return. 
      CALL FZDATA(IUNIT,IER)
      IF(IER.eq.1)THEN
        write(outs,'(a)') 'Error reading fuzzy data.'
        if(dll)then
          dllsubr='FZDATA'
          dllmesg=outs
          ier=2
          return
        else
          call edisp(iuout,outs)
          CALL EDISP(ITRU,OUTSTR)
          CALL ERPFREE(IUNIT,ISTAT)
          ier=1
          return
        endif
      ENDIF

C Set TSC6 data items, if applicable.
      call tscdta

C Close file.
      CALL ERPFREE(IUNIT,ISTAT)
      return

C Low level I/O error. Close file before exiting.
  99  continue
      write(outs,'(3a)') 'low level read/parsing error @',
     &  outstr(1:50),'...'
      if(dll)then
        dllsubr='EZCTLR'
        dllmesg=outs
        ier=2
        CALL ERPFREE(IUNIT,ISTAT)
        return
      else
        call edisp(iuout,outs)
        ier=1
        CALL ERPFREE(IUNIT,ISTAT)
        return
      endif

C Issue with the zone - loop links.
 109  continue
      write(outs,'(2a)') 'Warning reading zone - control loop links! ',
     &  'Number of zones & control links differ.'
      if(dll)then
        dllsubr='EZCTLR'
        dllmesg=outs
        ier=2
        CALL ERPFREE(IUNIT,ISTAT)
        return
      else
        call edisp(iuout,outs)
        ier=1
        CALL ERPFREE(IUNIT,ISTAT)
        return
      endif

      RETURN
      END

c ******************** EZCTLI ********************
c EZCTLI: Zeroes all control data in common BCTL, CCTL, FCTL, OCTL, GCTL
C and CFCCTL
      SUBROUTINE EZCTLI
#include "building.h"
#include "net_flow.h"
#include "control.h"

      integer icascf
      common/cctl/icascf(mcom)

c      COMMON/idrhctl1/rhspt(MCOM),irhct(MCOM)
      COMMON/IDRHCTL1/RHSPTU(MCOM),RHSPTL(MCOM),RHHC(MCOM),
     &RHDC(MCOM),IRHCT(MCOM)

      ncf=0
      do 10 i=1,mcom
        icascf(i)=0
   10 continue
      do 20 i=1,mcf
        do 30 j=1,4
          ibsn(i,j)=0
          if(j.eq.4)goto 30
          iban(i,j)=0
   30   continue
        nbcdt(i)=0
        do 40 j=1,MCDT
          nbcdp(i,j)=0
          do 50 k=1,MCDP
            do 60 l=1,misc
              bmiscd(i,j,k,l)=0.0
   60       continue
   50     continue
   40   continue
   20 continue

      ncl=0
      do 120 i=1,mcf
        do 130 j=1,3
          ipsn(i,j)=0
          ipan(i,j)=0
  130   continue
        npcdt(i)=0
        do 140 j=1,mcdt
          npcdp(i,j)=0
          do 150 k=1,mcdp
            do 160 l=1,misc
              pmiscd(i,j,k,l)=0.0
  160       continue
  150     continue
  140   continue
  120 continue

      ncc=0
      do 220 i=1,mcf
        do 230 j=1,4
          ifsn(i,j)=0
          if(j.eq.4)goto 230
          ifan(i,j)=0
  230   continue
        nfcdt(i)=0
        do 240 j=1,MCDT
          nfcdp(i,j)=0
          do 250 k=1,mcdp
            do 260 l=1,misc
              fmiscd(i,j,k,l)=0.0
  260       continue
  250     continue
  240   continue
  220 continue

      ngf=0
      do 320 i=1,MCF
        ngcdt(i)=0
        do 340 j=1,mcdt
          ngcdp(i,j)=0
          do 350 k=1,mcdp
            do 360 l=1,misc
              gmiscd(i,j,k,l)=0.0
  360       continue
  350     continue
  340   continue
  320 continue

      do 330 i=1,mcom
        irhct(i)=0
        RHSPTU(MCOM)=0.0
        RHSPTL(MCOM)=0.0
        RHHC(MCOM)=0.0
        RHDC(MCOM)=0.0
  330 continue

      nof=0
      do 420 i=1,mCF
        nocdt(i)=0
        do 440 j=1,mcdt
          nocdp(i,j)=0
          do 450 k=1,mcdp
            do 460 l=1,misc
              omiscd(i,j,k,l)=0.0
  460       continue
  450     continue
  440   continue
  420 continue

      nCFCctlloops=0
      do 520 i=1,mcf
        do 530 j=1,4
          iCFCsensor(i,j)=0
          if(j.eq.4)goto 530
          iCFCactuator(i,j)=0
  530   continue
        nCFCctldaytypes(i)=0
        do 540 j=1,MCDT
          nCFCdayctlperiods(i,j)=0
          do 550 k=1,MCDP
            do 560 l=1,misc
              CFCmiscdata(i,j,k,l)=0.0
  560       continue
  550     continue
  540   continue
  520 continue
  
  
      RETURN
      END

C ******************** EVCNTRL 
C EVCNTRL: Decodes and returns a string (96 char) describing a config
C control file sensors, actuator, control types, control laws.
      SUBROUTINE EVCNTRL(icfoc,II,JJ,KK,WHICH,SSTR)
#include "building.h"
#include "geometry.h"
#include "plant.h"
#include "net_flow.h"
#include "control.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

C      COMMON/OUTIN/IUOUT,IUIN
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/pcnam/pcname(mpcom)
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)

      CHARACTER SSTR*96,SSTRB*48
      CHARACTER WHICH*1,ZN*12
      CHARACTER LTPCMP*60,CMNAM*12,NDNAM*12,pcname*15,SN*12
      character disp_constr*12

      dimension icsn(5)   ! for passing sensor location to decodesensor
      integer icsn

      SSTR=' '
      SSTRB=' '

      if(icfoc.eq.0)goto 42     ! zone ideal control
      if(icfoc.eq.1)goto 43     ! plant control
      if(icfoc.eq.2)goto 44     ! network flow control
      if(icfoc.eq.3)goto 46     ! global control
      if(icfoc.eq.4)goto 45     ! electrical control
      if(icfoc.eq.5)goto 47     ! optical control
      if(icfoc.eq.6)goto 48     ! complex fenestration control
  42  IF(WHICH.EQ.'S'.OR.WHICH.EQ.'s')THEN

C Ideal Sensor.
        icsn(1)=IBSN(II,1)
        icsn(2)=IBSN(II,2)
        icsn(3)=IBSN(II,3)
        icsn(4)=IBSN(II,4)
        call decodesensor(icfoc,icsn,SSTR,SSTRB,ier)

      ELSEIF(WHICH.EQ.'A'.OR.WHICH.EQ.'a')THEN

C Actuator.
        IF(IBAN(II,1).GT.0)THEN
          write(zn,'(a)') zname(IBAN(II,1))
          if(IBAN(II,2).gt.0)then
            icc=IZSTOCN(IBAN(II,1),IBAN(II,2))
            write(sn,'(a)') SSNAME(icc)
          else
            sn='-'
          endif
          IF(IBAN(II,2).EQ.0)THEN
            WRITE(SSTR,'(3a)')'the air point in ',zn(1:lnblnk(zn)),'.'
          ELSEIF(IBAN(II,2).GT.0.AND.IBAN(II,3).EQ.0)THEN
            WRITE(SSTR,'(5a)')  'at inside face of ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
          ELSEIF(IBAN(II,2).GT.0.AND.IBAN(II,3).GT.0)THEN
            WRITE(SSTR,'(5a)')  'within ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
          ENDIF
        ELSEIF(IBAN(II,1).EQ.0)THEN
          SSTR='air point of the current zone'
        ELSEIF(IBAN(II,1).EQ.-1)THEN
          write(zn,'(a)') zname(IBAN(II,1))
          WRITE(SSTR,'(a,i2,3a)')'plant component ',IBAN(II,2),' in ',
     &      zn(1:lnblnk(zn)),'.'
        ELSEIF(IBAN(II,1).EQ.-2)THEN
          if(IBAN(II,2).gt.0)then
            write(zn,'(a)') zname(IBAN(II,2))
            WRITE(SSTR,'(3a)')'mixed convective/radiant flux in ',
     &        zn(1:lnblnk(zn)),'.'
          else
            WRITE(SSTR,'(2a)')'mixed convective/radiant flux in ',
     &        'the current zone.'
          endif
        ENDIF
        IF(IBSN(II,1).EQ.-99)THEN
          WRITE(SSTR,75)IBAN(II,1),IBAN(II,2),IBAN(II,3)
  75      FORMAT('construction substitution to surfaces: ',3(I2,','))
        ENDIF
      ELSEIF(WHICH.EQ.'T'.OR.WHICH.EQ.'t')THEN

C Control type.
        IF(IBSN(II,1).EQ.-99)then
          SSTR='construction subst.'
        else
          SSTR='db temp   > flux     '
        endif
      ELSEIF(WHICH.EQ.'L'.OR.WHICH.EQ.'l')THEN

C Ideal Control law.
        SSTR='undefined law '
        call usrmsg('should have called EVCNTRLAW',' ','W')
      ENDIF
      return

  43  IF(WHICH.EQ.'S'.OR.WHICH.EQ.'s')THEN

C Plant Sensor.
        SSTR='undefined condition'
        icsn(1)=IPSN(II,1)
        icsn(2)=IPSN(II,2)
        icsn(3)=IPSN(II,3)
        icsn(4)=IPSN(II,4)
        icsn(5)=IPSN(II,5)
        call decodesensor(icfoc,icsn,SSTR,SSTRB,ier)

      ELSEIF(WHICH.EQ.'A'.OR.WHICH.EQ.'a')THEN

C Plant actuator.
        SSTR='undefined actuation'
        IF(IPAN(II,1).EQ.-1)THEN
          WRITE(SSTR,120)IPAN(II,2),
     &pcname(IPAN(II,2))(1:LNBLNK(pcname(IPAN(II,2)))),IPAN(II,3)
 120      FORMAT('plant component ',I3,':',a,' @ node no. ',I2)
        ENDIF
      ELSEIF(WHICH.EQ.'T'.OR.WHICH.EQ.'t')THEN

C Plant control type.
        SSTR='undefined > plant '
        IF(IPCTYP(II,JJ,KK).eq.0)SSTR= 'dry bulb   > flux '
        IF(IPCTYP(II,JJ,KK).eq.1)SSTR= 'dry bulb   > flow '
        IF(IPCTYP(II,JJ,KK).eq.2)SSTR= 'enthalpy   > flux '
        IF(IPCTYP(II,JJ,KK).eq.3)SSTR= 'enthalpy   > flow '
        IF(IPCTYP(II,JJ,KK).eq.4)SSTR= '1st phase  > flux '
        IF(IPCTYP(II,JJ,KK).eq.5)SSTR= '1st phase  > flow '
        IF(IPCTYP(II,JJ,KK).eq.6)SSTR= '2nd phase  > flux '
        IF(IPCTYP(II,JJ,KK).eq.7)SSTR= '2nd phase  > flow '
        IF(IPCTYP(II,JJ,KK).eq.8)SSTR= 'plant      > flux '
        IF(IPCTYP(II,JJ,KK).eq.9)SSTR= 'plant      > flow '
        IF(IPCTYP(II,JJ,KK).eq.10)SSTR='RH         > flux '
        IF(IPCTYP(II,JJ,KK).eq.11)SSTR='RH         > flow '
        IF(IPCTYP(II,JJ,KK).eq.12)SSTR='dry bulb   > varb '
        IF(IPCTYP(II,JJ,KK).eq.13)SSTR='enthalpy   > varb '
        IF(IPCTYP(II,JJ,KK).eq.14)SSTR='1st phase  > varb '
        IF(IPCTYP(II,JJ,KK).eq.15)SSTR='2nd phase  > varb '
        IF(IPCTYP(II,JJ,KK).eq.16)SSTR='plant      > varb '
        IF(IPCTYP(II,JJ,KK).eq.17)SSTR='RH         > varb '
        IF(IPCTYP(II,JJ,KK).eq.18)SSTR='dry bulb > mas.rt '
        IF(IPCTYP(II,JJ,KK).eq.19)SSTR='enthalpy > mas.rt '
        IF(IPCTYP(II,JJ,KK).eq.20)SSTR='1st phas > mas.rt '
        IF(IPCTYP(II,JJ,KK).eq.21)SSTR='2nd phas > mas.rt '
        IF(IPCTYP(II,JJ,KK).eq.22)SSTR='plant    > mas.rt '
        IF(IPCTYP(II,JJ,KK).eq.23)SSTR='RH       > mas.rt '
        IF(IPCTYP(II,JJ,KK).eq.24)SSTR='delt T     > flow '
        IF(IPCTYP(II,JJ,KK).eq.25)SSTR='abs Delt T > flow '
        IF(IPCTYP(II,JJ,KK).eq.26)SSTR='pressure   > flow '
        IF(IPCTYP(II,JJ,KK).eq.27)SSTR='delt prs   > flow '
        IF(IPCTYP(II,JJ,KK).eq.28)SSTR='abs delt P > flow '
        IF(IPCTYP(II,JJ,KK).eq.29)SSTR='abs flow   > flow '
        IF(IPCTYP(II,JJ,KK).eq.30)SSTR='wind spd   > flow '
        IF(IPCTYP(II,JJ,KK).eq.31)SSTR='wind dir   > flow '
        IF(IPCTYP(II,JJ,KK).eq.32)SSTR='dr nrm rad > flow '
        IF(IPCTYP(II,JJ,KK).eq.33)SSTR='df hor rad > flow '
        IF(IPCTYP(II,JJ,KK).eq.34)SSTR='ext RH     > flow '
        IF(IPCTYP(II,JJ,KK).eq.35)SSTR='wind spd   > flux '
        IF(IPCTYP(II,JJ,KK).eq.36)SSTR='wind dir   > flux '
        IF(IPCTYP(II,JJ,KK).eq.37)SSTR='dr nrm rad > flux '
        IF(IPCTYP(II,JJ,KK).eq.38)SSTR='df hor rad > flux '
        IF(IPCTYP(II,JJ,KK).eq.39)SSTR='ext RH     > flux '
      ELSEIF(WHICH.EQ.'L'.OR.WHICH.EQ.'l')THEN

C Plant Control law.
        SSTR='undefined law '
        call usrmsg('should have called EVCNTRLAW',' ','W')
      ENDIF
      return

  44  IF(WHICH.EQ.'S'.OR.WHICH.EQ.'s')THEN

C Flow Sensor.
        SSTR='undefined condition'
        icsn(1)=IFSN(II,1)
        icsn(2)=IFSN(II,2)
        icsn(3)=IFSN(II,3)
        icsn(4)=IFSN(II,4)
        call decodesensor(icfoc,icsn,SSTR,SSTRB,ier)

      ELSEIF(WHICH.EQ.'A'.OR.WHICH.EQ.'a')THEN

C Flow actuator.
        SSTR='undefined actuation'
        IF(IFAN(II,1).EQ.-3)THEN
          L=IFAN(II,2)
          if(ITPCON(L).ne.0)then
            WRITE(SSTR,'(A,i3,1x,5A)')'flow connection: ',
     &        L,NDNAM(NODPS(L))(1:lnblnk(NDNAM(NODPS(L)))),' - ',
     &        NDNAM(NODNE(L))(1:lnblnk(NDNAM(NODNE(L)))),' via ',
     &        CMNAM(ITPCON(L))(1:lnblnk(CMNAM(ITPCON(L))))
          else
            WRITE(SSTR,'(A,i3,1x,4A)')'flow connection: ',
     &        L,NDNAM(NODPS(L))(1:lnblnk(NDNAM(NODPS(L)))),' - ',
     &        NDNAM(NODNE(L))(1:lnblnk(NDNAM(NODNE(L)))),
     &        ' via unknown component.'
          endif
        ELSEIF(IFAN(II,1).EQ.-4)THEN
          L=IFAN(II,2)
          if(L.ne.0)then
            WRITE(SSTR,'(a,i3,1x,a)') 'flow component: ',L,
     &        CMNAM(L)(1:lnblnk(CMNAM(L)))
          endif
        ENDIF
      ELSEIF(WHICH.EQ.'T'.OR.WHICH.EQ.'t')THEN

C Flow control type.
        SSTR='undefined > flow '
        IF(IFCTYP(II,JJ,KK).eq.1)SSTR= 'dry bulb > flow   '
        IF(IFCTYP(II,JJ,KK).eq.3)SSTR= 'enthalpy > flow   '
        IF(IFCTYP(II,JJ,KK).eq.5)SSTR= '1st phase > flow  '
        IF(IFCTYP(II,JJ,KK).eq.7)SSTR= '2nd phase > flow  '
        IF(IFCTYP(II,JJ,KK).eq.9)SSTR= 'plant > flow      '
        IF(IFCTYP(II,JJ,KK).eq.11)SSTR='RH > flow         '
        IF(IFCTYP(II,JJ,KK).eq.24)SSTR='delt T > flow     '
        IF(IFCTYP(II,JJ,KK).eq.25)SSTR='abs Delt T > flow '
        IF(IFCTYP(II,JJ,KK).eq.26)SSTR='pressure > flow   '
        IF(IFCTYP(II,JJ,KK).eq.27)SSTR='delt prs > flow   '
        IF(IFCTYP(II,JJ,KK).eq.28)SSTR='abs delt P > flow '
        IF(IFCTYP(II,JJ,KK).eq.29)SSTR='abs flow > flow   '
        IF(IFCTYP(II,JJ,KK).eq.41)SSTR='contam conc > flow'
        IF(IFSN(II,1).EQ.-3)SSTR='outside ambient > flow  '
      ELSEIF(WHICH.EQ.'L'.OR.WHICH.EQ.'l')THEN

C Control law.
        SSTR='undefined law '
        call usrmsg('should have called EVCNTRLAW',' ','W')
      endif
      RETURN

C Power systems control
C Return sensor type.
  45  IF(WHICH.EQ.'S'.OR.WHICH.EQ.'s')THEN
        WRITE(SSTR,775) IESN(II,2)
  775   FORMAT('is active at bus',I3) 
        IF(IECTYP(II,JJ,KK).EQ.1) THEN
        ENDIF
C Return actuator type.
      ELSEIF(WHICH.EQ.'A'.OR.WHICH.EQ.'a')THEN
        WRITE(SSTR,776) IESN(II,2)
 776    FORMAT('is active at bus ',I3)
      ELSEIF(WHICH.EQ.'T'.OR.WHICH.EQ.'t')THEN
        SSTR='undefined law '
        IF(IECTYP(II,JJ,KK).eq.1)SSTR= 'Voltage > Reactive power '
        IF(IECTYP(II,JJ,KK).eq.10)SSTR= 'ApparentPower > on/off '
      ENDIF
      RETURN

C Global control. Return sensor type << check required if values other
C than -3 allowed for sensor >>.
  46  IF(WHICH.EQ.'S'.OR.WHICH.EQ.'s')THEN
        SSTR='sensors are not used in global controls'
      ELSEIF(WHICH.EQ.'A'.OR.WHICH.EQ.'a')THEN

C Return actuator type. << check to see if any other values allowed >>
        SSTR='actuation not used in global controls'
      ELSEIF(WHICH.EQ.'T'.OR.WHICH.EQ.'t')THEN
        SSTR='undefined law '
        IF(igctyp(II,JJ,KK).eq.1)SSTR= 'Global on-off controller'
        IF(igctyp(II,JJ,KK).eq.2)SSTR= 'Global capacity controller'
        IF(igctyp(II,JJ,KK).eq.3)SSTR= 'Global schedule on controller'
        IF(igctyp(II,JJ,KK).eq.5)SSTR= 'Global multisensor'
      ENDIF
      RETURN

  47  IF(WHICH.EQ.'S'.OR.WHICH.EQ.'s')THEN

C Optical Sensor.
C<<<<<<<<Possibly need only 3 items for sensor>>>>>>>>>>>>????
C<<< Remember also to change it in subroutine ezctlc >>>>>>>>>>>>>
        icsn(1)=IOSN(II,1)
        icsn(2)=IOSN(II,2)
        icsn(3)=IOSN(II,3)
        icsn(4)=IOSN(II,4)
        call decodesensor(icfoc,icsn,SSTR,SSTRB,ier)
      ELSEIF(WHICH.EQ.'A'.OR.WHICH.EQ.'a')THEN

C Actuator.
C <<<Confirm this with bpfcontrl.F>>>>>>>>>>>>>>>>
        SSTR='undefined actuator    '
        IF(IOAN(II,1).eq.-5)THEN
C<<<<Probably IOAN does not need a 3rd item. >>>>
          if(IOAN(II,2).gt.0)then
            icons=IOAN(II,2)
            write(disp_constr,'(a)') mlcname(icons)(1:12)
            WRITE(SSTR,'(3a)')  'actuating the optical properties at ',
     &        'construction ', disp_constr
          else
            WRITE(SSTR,'(2a)')  'Optics actuator has not been assigned',
     &        ' to a construction.' 
          endif
        ENDIF
      ELSEIF(WHICH.EQ.'T'.OR.WHICH.EQ.'t')THEN

C Control type.
        SSTR='bidirectional set  '
      ELSEIF(WHICH.EQ.'L'.OR.WHICH.EQ.'l')THEN

C Ideal Control law.
        SSTR='undefined control        '
        call usrmsg('should have called EVCNTRLAW',' ','W')
      ENDIF
      RETURN

  48  IF(WHICH.EQ.'S'.OR.WHICH.EQ.'s')THEN

C Complex fenestration sensor.
        icsn(1)=iCFCsensor(II,1)
        icsn(2)=iCFCsensor(II,2)
        icsn(3)=iCFCsensor(II,3)
        icsn(4)=iCFCsensor(II,4)
        call decodesensor(icfoc,icsn,SSTR,SSTRB,ier)

      ELSEIF(WHICH.EQ.'A'.OR.WHICH.EQ.'a')THEN

C Actuator.
        IF(iCFCactuator(II,2).GT.0)THEN
          write(zn,'(a)') zname(iCFCactuator(II,2))
          if(iCFCactuator(II,3).gt.0)then
            write(sn,'(i3)')iCFCactuator(II,3)
          else
            sn='-'
          endif
          IF(iCFCactuator(II,1).EQ.0)THEN
            WRITE(SSTR,'(5a)')'Shading ON/OFF in CFC type',
     &       sn(1:lnblnk(sn)),' in ',zn(1:lnblnk(zn)),'.'
          ELSEIF(iCFCactuator(II,1).EQ.1)THEN
            WRITE(SSTR,'(5a)')'Slat angle in CFC type',
     &       sn(1:lnblnk(sn)),' in ',zn(1:lnblnk(zn)),'.'
          ELSEIF(iCFCactuator(II,1).EQ.2)THEN
            WRITE(SSTR,'(5a)')'Shade ON/OFF and slat angle(schedule)',
     &       sn(1:lnblnk(sn)),' in ',zn(1:lnblnk(zn)),'.'     
          ENDIF
        ENDIF
        
      ELSEIF(WHICH.EQ.'T'.OR.WHICH.EQ.'t')THEN

C Complex fenestration control type.
        SSTR='undefined > complex fenestration '
        IF(iCFCctltype(II,JJ,KK).eq.1)
     &         SSTR= 'temp > shd ON/OFF '
        IF(iCFCctltype(II,JJ,KK).eq.2)
     &         SSTR= 'temp > slat angle '
        IF(iCFCctltype(II,JJ,KK).eq.3)
     &         SSTR= 'rad > shd ON/OFF '
        IF(iCFCctltype(II,JJ,KK).eq.4)
     &         SSTR= 'rad > slat angle '
        IF(iCFCctltype(II,JJ,KK).eq.5)
     &         SSTR= 'wnd-spd >shd ON/OFF'
        IF(iCFCctltype(II,JJ,KK).eq.6)
     &         SSTR= 'wnd-dir >slat angle'
        IF(iCFCctltype(II,JJ,KK).eq.7)
     &         SSTR= 'none > sched. '
      ELSEIF(WHICH.EQ.'L'.OR.WHICH.EQ.'l')THEN

C Complex fenestration control law.
        SSTR='undefined law '
        call usrmsg('should have called EVCNTRLAW',' ','W')        
      ENDIF
      return
      
      END

C ******************** EVCNTRLAW 
C EVCNTRLAW: Decodes and returns two strings (96 char & 248 char) describing
C an esp-r control file control laws for zone/plant/flow/electrical.
      SUBROUTINE EVCNTRLAW(icfoc,II,JJ,KK,SSTR,message)
#include "building.h"
#include "net_flow.h"
#include "control.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

C      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW2/NDNAM(0:MNOD)

      CHARACTER SSTR*96,SSTRB*48,SSTR1*96,SSTRB1*48
      CHARACTER NDNAM*12

      dimension cm(misc)  ! for passing miscel control data
      real cm
      dimension icsn(5)   ! for passing sensor location to
      integer icsn
      integer isub1,isub2,isub3 ! for thermophysical substitution
      character sub1*12,sub2*12,sub3*12 

C sub-strings for describing multi-sensor flow controls.
      character msg1*24,msg2*24,msg3*24,msg4*24,msg*72,lmsg*248
      character message*248
      logical oka,okb
      integer I2,I7,I8,I9  ! for plant control decoding

      SSTR=' '
      SSTR1=' '
      SSTRB=' '
      SSTRB1=' '
      message=' '

      if(icfoc.eq.0)then

C Ideal Control law.
        SSTR='undefined control        '
        IF(IBCLAW(II,JJ,KK).EQ.1.or.IBCLAW(II,JJ,KK).EQ.3)then
          IF(IBCLAW(II,JJ,KK).EQ.1)then

C Check if the sensor is thermophysical property substitution.
            if(IBSN(II,1).eq.-99)then
              SSTR='substitution         '
              msg1=' substitution :'
            else
              SSTR='basic control        '
              msg1=' basic control:'
            endif
          ELSEIF(IBCLAW(II,JJ,KK).EQ.3)then
            SSTR='pre-heat/cool        '
            msg1=' pre-heat/cool '
          ENDIF
          cm(1)=bmiscd(II,JJ,KK,1)
          cm(2)=bmiscd(II,JJ,KK,2)
          cm(3)=bmiscd(II,JJ,KK,3)
          cm(4)=bmiscd(II,JJ,KK,4)
          cm(5)=bmiscd(II,JJ,KK,5)
          cm(6)=bmiscd(II,JJ,KK,6)
          cm(7)=bmiscd(II,JJ,KK,7)
          cm(8)=bmiscd(II,JJ,KK,8)
          cm(9)=bmiscd(II,JJ,KK,9)
          cm(10)=bmiscd(II,JJ,KK,10)
          cm(11)=bmiscd(II,JJ,KK,11)
          cm(12)=bmiscd(II,JJ,KK,12)
          if(IBSN(II,1).eq.-99)then
            isub1=nint(cm(3)); isub2=nint(cm(4)); isub3=nint(cm(5))
            sub1='-           '; sub2='-           '
            sub3='-           '
            if(isub1.ne.0) sub1=mlcname(isub1)(1:12)
            if(isub2.ne.0) sub2=mlcname(isub2)(1:12)
            if(isub3.ne.0) sub3=mlcname(isub3)(1:12)
            if(nint(cm(1)).eq.4)then
              write(lmsg,'(8a)')  msg1,' use mlc ',sub1,
     &          ' mlc ',sub2,' mlc ',sub3,' for the surfaces.'
            else
              write(lmsg,'(2a)') 
     &          msg1,' unexpected nb of data items for substitution.'
            endif
          else
            if(nint(cm(1)).eq.6)then

C Older control files might only have 6 items for basic controller.
              write(lmsg,
     &          '(2a,f9.1,a,f9.1,a,f9.1,a,f9.1,a,f7.2,a,f7.2,a)') 
     &          msg1,' max heating capacity ',cm(2),
     &          'W min heating capacity ',cm(3),
     &          'W max cooling capacity ',cm(4),
     &          'W min cooling capacity ',cm(5),
     &          'W. Heating setpoint ',cm(6),'C cooling setpoint ',
     &          cm(7),'C. (missing 7th item)'
            elseif(nint(cm(1)).eq.7)then

C Newer control files have an extra item related to humidity control.
              write(lmsg,
     &          '(2a,f9.1,a,f9.1,a,f9.1,a,f9.1,a,f7.2,a,f7.2,a)') 
     &          msg1(1:lnblnk(msg1)),' max heating capacity ',cm(2),
     &          'W min heating capacity ',cm(3),
     &          'W max cooling capacity ',cm(4),
     &          'W min cooling capacity ',cm(5),
     &          'W. Heating setpoint ',cm(6),'C cooling setpoint ',
     &          cm(7),'C.'
            elseif(nint(cm(1)).eq.11)then
              if(nint(cm(12)).eq.1)then
                write(lmsg,'(2a,f9.1,a,f9.1,a,f9.1,a,f9.1,a,f7.2,
     &            a,f7.2,a,f5.1,a,f5.1,a,f6.2,a,f6.2,a)') 
     &            msg1(1:lnblnk(msg1)),' mx heat cap',cm(2),
     &            'W mn heat cap',cm(3),'W mx cool cap',cm(4),
     &            'W mn cool cap',cm(5),'W. Heat sp',cm(6),
     &            'C cool sp',cm(7),
     &            'C & moist inj RH ctl hi sp',
     &            cm(8),'(%) low sp',cm(9),
     &            '(%) max humid ',cm(10),
     &            '(g/s) max dehumid ',cm(11),'(g/s).'
              elseif(nint(cm(12)).eq.2)then
                write(lmsg,'(2a,f9.1,a,f9.1,a,f9.1,a,f9.1,a,f7.2,
     &            a,f7.2,a,f5.1,a,f5.1,a)') 
     &            msg1(1:lnblnk(msg1)),' max heating cap ',cm(2),
     &            'W min heating cap ',cm(3),
     &            'W max cooling cap ',cm(4),
     &            'W min heaing cap ',cm(5),
     &            'W. Heating sp ',cm(6),
     &            'C cooling sp ',cm(7),
     &            'C and T based RH ctl upper sp ',
     &            cm(8),'(%) lower sp ',cm(9),'(%).'
              elseif(nint(cm(12)).eq.0)then
                write(lmsg,
     &            '(2a,f9.1,a,f9.1,a,f9.1,a,f9.1,a,f7.2,a,f7.2,a)') 
     &            msg1(1:lnblnk(msg1)),' max heating capacity ',cm(2),
     &            'W min heating capacity ',cm(3),
     &            'W max cooling capacity ',cm(4),
     &            'W min cooling capacity ',cm(5),
     &            'W. Heating setpoint ',cm(6),'C cooling setpoint ',
     &            cm(7),'C. with incomplete RH control.'
              endif
            endif
          endif
          call SDELIM(lmsg,message,'S',IW)
C          call edisp248(6,message,80)
        ELSEIF(IBCLAW(II,JJ,KK).EQ.2)then
          SSTR='free floating        '
          message=' free floating (no parameters).'
        ELSEIF(IBCLAW(II,JJ,KK).EQ.4)then
          SSTR='fixed inject/extract '
          cm(2)=bmiscd(II,JJ,KK,2)
          cm(3)=bmiscd(II,JJ,KK,3)
          cm(4)=bmiscd(II,JJ,KK,4)
          cm(5)=bmiscd(II,JJ,KK,5)
          write(lmsg,'(a,f9.1,a,f9.1,a,f9.1,a,f9.1,a)') 
     &      ' fixed injection/extraction: heating injection ',cm(2),
     &      'W cooling extraction ',cm(3),
     &      'W. Heating setpoint ',cm(4),
     &      'C cooling setpoint ',cm(5),'C.'
          call SDELIM(lmsg,message,'S',IW)
C          call edisp248(6,message,80)
        ELSEIF(IBCLAW(II,JJ,KK).EQ.5)then
          SSTR='basic proportional   '
          cm(2)=bmiscd(II,JJ,KK,2)
          cm(3)=bmiscd(II,JJ,KK,3)
          cm(4)=bmiscd(II,JJ,KK,4)
          cm(5)=bmiscd(II,JJ,KK,5)
          cm(6)=bmiscd(II,JJ,KK,6)
          cm(7)=bmiscd(II,JJ,KK,7)
          cm(8)=bmiscd(II,JJ,KK,8)
          cm(9)=bmiscd(II,JJ,KK,9)
          cm(10)=bmiscd(II,JJ,KK,10)
          cm(11)=bmiscd(II,JJ,KK,11)
          cm(12)=bmiscd(II,JJ,KK,12)
          if(nint(cm(2)).eq.1)then
            write(lmsg,'(a,f9.1,a,f9.1,a,f7.2,a,f5.2,a,f9.1,a,f9.1,
     &        a,f7.2,a,f5.2,a)') 
     &      ' proportional: max heating ',cm(3),
     &      'W min heating ',cm(4),'W heating spt ',cm(5),
     &      'C throttling range ',cm(6),'C. Max cooling ',cm(7),
     &      'C min cooling ',cm(8),'W cooling spt ',cm(9),
     &      'C throttling range ',cm(10),'C.' 
          elseif(nint(cm(2)).eq.2)then
            write(lmsg,'(a,f9.1,a,f9.1,a,f7.2,a,f5.2,a,f9.1,a,f9.1,
     &        a,f7.2,a,f5.2,a,f6.1,a)') 
     &      ' proportional+I: max heat ',cm(3),
     &      'W min heat ',cm(4),'W heat spt ',cm(5),
     &      'C throttling range ',cm(6),'C. Max cool ',cm(7),
     &      'W min cool ',cm(8),'W cool spt ',cm(9),
     &      'C throttling range ',cm(10),'C integral action time',
     &      cm(11),'s.' 
          elseif(nint(cm(2)).eq.3)then
            write(lmsg,'(a,f9.1,a,f9.1,a,f7.2,a,f5.2,a,f9.1,a,f9.1,
     &        a,f7.2,a,f5.2,a,f6.1,a)') 
     &      ' proportional+D: max heat ',cm(3),
     &      'W min heat ',cm(4),'W heat spt ',cm(5),
     &      'C throttling range ',cm(6),'C. Max cool ',cm(7),
     &      'W min cool ',cm(8),' W cool spt ',cm(9),
     &      'C throttling range ',cm(10),'C derivative action time',
     &      cm(11),'s.' 
          elseif(nint(cm(2)).eq.4)then
            write(lmsg,'(a,f9.1,a,f9.1,a,f7.2,a,f5.2,a,f9.1,a,f9.1,
     &        a,f7.2,a,f5.2,a,f6.1,a,f6.1,a)') 
     &      ' proportional+I+D: max heat ',cm(3),
     &      'W min heat ',cm(4),'W heat spt ',cm(5),
     &      'C throttling range ',cm(6),'C. Max cool ',cm(7),
     &      'W min cool ',cm(8),'W cool spt ',cm(9),
     &      'C throttling range ',cm(10),'C integral action time',
     &      cm(11),'s derivative action time ',cm(12),'s.' 
          endif
          call SDELIM(lmsg,message,'S',IW)
C          call edisp248(6,message,80)
        ELSEIF(IBCLAW(II,JJ,KK).EQ.6)then
          SSTR='flux zone/plant      '
          cm(1)=bmiscd(II,JJ,KK,1)
          cm(2)=bmiscd(II,JJ,KK,2)
          cm(3)=bmiscd(II,JJ,KK,3)
          cm(4)=bmiscd(II,JJ,KK,4)
          cm(5)=bmiscd(II,JJ,KK,5)
          cm(6)=bmiscd(II,JJ,KK,6)
          cm(7)=bmiscd(II,JJ,KK,7)
          cm(8)=bmiscd(II,JJ,KK,8)
          if(nint(cm(4)).eq.1)then
            if(nint(cm(1)).eq.5)then
              write(lmsg,'(a,i3,a,i3,a,f9.2,a,f9.2,a)') 
     &        ' plant/zone coupling: plant component',nint(cm(2)),
     &        ' plant component node ',nint(cm(3)),
     &        ' coupling type mCp(0s-0a) sequential. Max heating ',
     &        cm(5),'W max cooling ',cm(6),'W.'
            elseif(nint(cm(1)).eq.7)then
              write(lmsg,'(a,i3,a,i3,a,f9.2,a,f9.2,a,i3,a,i3,a)') 
     &        ' plant/zone coupling: source plant component',
     &        nint(cm(2)),' plant component node ',nint(cm(3)),
     &        ' coupling type mCp(0s-0a) sequential. Max heating ',
     &        cm(5),'W max cooling ',cm(6),
     &        'W. Extract plant component ',nint(cm(7)),
     &        ' and extract node ',nint(cm(8)),'.'
            endif
          elseif(nint(cm(4)).eq.2)then
            if(nint(cm(1)).eq.5)then
              write(lmsg,'(a,i3,a,i3,a,f9.2,a,f9.2,a)') 
     &        ' plant/zone coupling: plant component',nint(cm(2)),
     &        ' plant component node ',nint(cm(3)),
     &        ' coupling type hrA(0s-0a) sequential. Max heating ',
     &        cm(5),'W max cooling ',cm(6),'W.'
            elseif(nint(cm(1)).eq.7)then
              write(lmsg,'(a,i3,a,i3,a,f9.2,a,f9.2,a,i3,a,i3,a)') 
     &        ' plant/zone coupling: source plant component',
     &        nint(cm(2)),' plant component node ',nint(cm(3)),
     &        ' coupling type hrA(0s-0a) concurrent. Max heating ',
     &        cm(5),'W max cooling ',cm(6),
     &        'W. Extract plant component ',nint(cm(7)),
     &        ' and extract node ',nint(cm(8)),'.'
            endif
          elseif(nint(cm(4)).eq.5)then
            if(nint(cm(1)).eq.5)then
              write(lmsg,'(a,i3,a,i3,a,f9.2,a,f9.2,a)') 
     &        ' plant/zone coupling: plant component',nint(cm(2)),
     &        ' plant component node ',nint(cm(3)),
     &        ' coupling type mCp(0s-0a) concurrent. Max heating ',
     &        cm(5),'W max cooling ',cm(6),'W.'
            elseif(nint(cm(1)).eq.7)then
              write(lmsg,'(a,i3,a,i3,a,f9.2,a,f9.2,a,i3,a,i3,a)') 
     &        ' plant/zone coupling: source plant component',
     &        nint(cm(2)),' plant component node ',nint(cm(3)),
     &        ' coupling type mCp(0s-0a) concurrent. Max heating ',
     &        cm(5),'W max cooling ',cm(6),
     &        'W. Extract plant component ',nint(cm(7)),
     &        ' and extract node ',nint(cm(8)),'.'
            endif
          endif
          call SDELIM(lmsg,message,'S',IW)
C Debug.
C          call edisp248(6,message,80)
        ELSEIF(IBCLAW(II,JJ,KK).EQ.7)then
          SSTR='multi-stage with hyst'
          cm(2)=bmiscd(II,JJ,KK,2)
          cm(3)=bmiscd(II,JJ,KK,3)
          cm(4)=bmiscd(II,JJ,KK,4)
          cm(5)=bmiscd(II,JJ,KK,5)
          cm(6)=bmiscd(II,JJ,KK,6)
          cm(7)=bmiscd(II,JJ,KK,7)
          cm(8)=bmiscd(II,JJ,KK,8)
          cm(9)=bmiscd(II,JJ,KK,9)
          cm(10)=bmiscd(II,JJ,KK,10)
          cm(11)=bmiscd(II,JJ,KK,11)
          cm(12)=bmiscd(II,JJ,KK,12)
          write(lmsg,'(a,f7.1,a,f9.1,a,f9.1,a,f9.1,a,f7.1,a,f9.1,
     &      a,f9.1,a,f9.1,a,f6.2,a,f5.2,a,f6.2,a,f5.2,a)') 
     &    ' multistage: heat off',cm(2),
     &    'W heat stg 1 ',cm(3),'W heat stg 2 ',cm(4),
     &    'W heat stg 3 ',cm(5),'W cool off',cm(6),
     &    'W cool stg 1 ',cm(7),'W cool stg 2 ',cm(8),
     &    'W cool stg 3 ',cm(9),'W heat sp ',cm(10),
     &    'C heat db ',cm(11),'C cool sp ',cm(12),
     &    'C cool db ',cm(13),'C.' 
          call SDELIM(lmsg,message,'S',IW)
C          call edisp248(6,message,80)
        ELSEIF(IBCLAW(II,JJ,KK).EQ.8)then
          SSTR='variable supply      '
          cm(2)=bmiscd(II,JJ,KK,2)
          cm(3)=bmiscd(II,JJ,KK,3)
          cm(4)=bmiscd(II,JJ,KK,4)
          cm(5)=bmiscd(II,JJ,KK,5)
          cm(6)=bmiscd(II,JJ,KK,6)
          if(nint(cm(7)).eq.0) msg1='C cooling available.'
          if(nint(cm(7)).eq.1) msg1='C no cooling available.'
          write(lmsg,'(a,f7.2,a,f7.2,a,f9.4,a,f6.2,a,f6.2,a)') 
     &    ' variable supply temp: max supply T',cm(2),
     &    'C min supply T',cm(3),'C air flow rate ',cm(4),
     &    '(m^3/s) room heating setpoint ',cm(5),
     &    'C room cooling setpoint',cm(6),msg1 
          call SDELIM(lmsg,message,'S',IW)
C          call edisp248(6,message,80)
        ELSEIF(IBCLAW(II,JJ,KK).EQ.9)then
          SSTR='heat pipe in constr  '
          cm(2)=bmiscd(II,JJ,KK,2)
          cm(3)=bmiscd(II,JJ,KK,3)
          cm(4)=bmiscd(II,JJ,KK,4)
          cm(5)=bmiscd(II,JJ,KK,5)
          cm(6)=bmiscd(II,JJ,KK,6)
          cm(7)=bmiscd(II,JJ,KK,7)
          cm(8)=bmiscd(II,JJ,KK,8)
          if(nint(cm(2)).eq.0) msg1=' heat pipe: no iteration'
          if(nint(cm(2)).eq.1) msg1=' heat pipe: iterating'
          write(lmsg,'(a,i2,a,i2,a,i2,a,f7.2,a,i3,a,f5.2,a)') 
     &    msg1,' surface index',nint(cm(3)),' outer node index',
     &    nint(cm(4)),' inner node index ',nint(cm(5)),
     &    ' critical temperature ',cm(6),'C max iterations',nint(cm(7)),
     &    ' flux difference tolerance ',cm(8),'W.'
          call SDELIM(lmsg,message,'S',IW)
C          call edisp248(6,message,80)
        ELSEIF(IBCLAW(II,JJ,KK).EQ.10)then
          SSTR='separate ON/OFF flux '
          cm(2)=bmiscd(II,JJ,KK,2)
          cm(3)=bmiscd(II,JJ,KK,3)
          cm(4)=bmiscd(II,JJ,KK,4)
          cm(5)=bmiscd(II,JJ,KK,5)
          cm(6)=bmiscd(II,JJ,KK,6)
          cm(7)=bmiscd(II,JJ,KK,7)
          write(lmsg,'(a,f9.2,a,f9.2,a,f6.2,a,f6.2,a,f6.2,a,f6.2,a)') 
     &    ' separate ON/OFF flux: heating capacity ',cm(2),
     &    'W cooling capacity ',cm(3),'W heating on below ',cm(4),
     &    'C heating off above ',cm(5),'C cooling on above ',cm(6),
     &    'C cooling off below ',cm(7),'C.'
          call SDELIM(lmsg,message,'S',IW)
C          call edisp248(6,message,80)
        ELSEIF(IBCLAW(II,JJ,KK).EQ.11)then
          SSTR='match temp - ideal'
          cm(2)=bmiscd(II,JJ,KK,2)
          cm(3)=bmiscd(II,JJ,KK,3)
          cm(4)=bmiscd(II,JJ,KK,4)
          cm(5)=bmiscd(II,JJ,KK,5)

C Depending on one or two aux sensors decode their locations
C into SSTRB and optionally SSTRB1.
          cm(6)=bmiscd(II,JJ,KK,6)
          cm(7)=bmiscd(II,JJ,KK,7)
          if(nint(cm(7)).eq.1)then
            msg1=' greatest value @ '
          elseif(nint(cm(7)).eq.2)then
            msg1=' least value @ '
          elseif(nint(cm(7)).eq.3)then
            msg1=' mean value @ '
          elseif(nint(cm(7)).eq.4)then
            msg1=' weighted value @ '
          endif

          if(nint(cm(6)).eq.1)then
            icsn(1)=nint(bmiscd(II,JJ,KK,8))
            icsn(2)=nint(bmiscd(II,JJ,KK,9))
            icsn(3)=nint(bmiscd(II,JJ,KK,10))
            icsn(4)=nint(bmiscd(II,JJ,KK,11))
            if(nint(cm(7)).eq.4)then
              cm(12)=bmiscd(II,JJ,KK,12)   ! weighting
              cm(13)=bmiscd(II,JJ,KK,13)   ! scale factor 
              cm(14)=bmiscd(II,JJ,KK,14)   ! offset 
            else
              cm(12)=bmiscd(II,JJ,KK,12)   ! scale factor
              cm(13)=bmiscd(II,JJ,KK,13)   ! offset 
            endif
            SSTR=' '
            call decodesensor(0,icsn,SSTR,SSTRB,ier)
          elseif(nint(cm(6)).eq.2)then
            icsn(1)=nint(bmiscd(II,JJ,KK,8))
            icsn(2)=nint(bmiscd(II,JJ,KK,9))
            icsn(3)=nint(bmiscd(II,JJ,KK,10))
            icsn(4)=nint(bmiscd(II,JJ,KK,11))
            SSTR=' '
            call decodesensor(0,icsn,SSTR,SSTRB,ier)
            icsn(1)=nint(bmiscd(II,JJ,KK,12))
            icsn(2)=nint(bmiscd(II,JJ,KK,13))
            icsn(3)=nint(bmiscd(II,JJ,KK,14))
            icsn(4)=nint(bmiscd(II,JJ,KK,15))
            if(nint(cm(7)).eq.4)then
              cm(16)=bmiscd(II,JJ,KK,16)   ! weighting 1st
              cm(17)=bmiscd(II,JJ,KK,17)   ! weighting 2nd
              cm(18)=bmiscd(II,JJ,KK,18)   ! scale factor 
              cm(19)=bmiscd(II,JJ,KK,19)   ! offset 
            else
              cm(16)=bmiscd(II,JJ,KK,16)   ! scale factor
              cm(17)=bmiscd(II,JJ,KK,17)   ! offset 
            endif
            SSTR1=' '
            call decodesensor(0,icsn,SSTR1,SSTRB1,ier)
          endif

C Generate the description from the bits taking into account how
C many aux sensors and whether there is a weighting factor.
          if(nint(cm(6)).eq.1)then
            if(nint(cm(7)).eq.4)then
              write(lmsg,'(4(a,f8.0),a,f3.0,2a,3(a,f5.2))')
     &          ' match temperature (ideal): max heat cp ',cm(2),
     &          'W min heat cp ',cm(3),'W max cool cp ',cm(4),
     &          'W min heat cp ',cm(5),'W Aux sensors ',cm(6),
     &          msg1(1:lnblnk(msg1)),SSTRB(1:lnblnk(SSTRB)),
     &          ' weighting ',cm(12),' scale ',cm(13),
     &          ' offset ',cm(14)
            else
              write(lmsg,'(4(a,f8.0),a,f3.0,2a,2(a,f5.2))')
     &          ' match temperature (ideal): max heat cp ',cm(2),
     &          'W min heat cp ',cm(3),'W max cool cp ',cm(4),
     &          'W min heat cp ',cm(5),'W Aux sensors ',cm(6),
     &          msg1(1:lnblnk(msg1)),SSTRB(1:lnblnk(SSTRB)),
     &          ' scale ',cm(12),' offset ',cm(13)
            endif
            call SDELIM(lmsg,message,'S',IW)
C            call edisp248(6,message,90)
          elseif(nint(cm(6)).eq.2)then
            write(lmsg,'(4(a,f8.0),a,f3.0,4a)')
     &        ' match temperature (ideal): max heat cp ',cm(2),
     &        'W min heat cp ',cm(3),'W max cool cp ',cm(4),
     &        'W min heat cp ',cm(5),'W Aux sensors ',cm(6),
     &        msg1(1:lnblnk(msg1)),SSTRB(1:lnblnk(SSTRB)),
     &        ' & ',SSTRB1(1:lnblnk(SSTRB1))
            call SDELIM(lmsg,message,'S',IW)
C            call edisp248(6,message,90)
          endif
        ELSEIF(IBCLAW(II,JJ,KK).EQ.12)then
          SSTR='match temp - on-off'
          message='match temperature (on/off) report TBD'
        ELSEIF(IBCLAW(II,JJ,KK).EQ.13)then
          SSTR='time-proportional ctl'
          cm(2)=bmiscd(II,JJ,KK,2)
          cm(3)=bmiscd(II,JJ,KK,3)
          cm(4)=bmiscd(II,JJ,KK,4)
          cm(5)=bmiscd(II,JJ,KK,5)
          cm(6)=bmiscd(II,JJ,KK,6)
          cm(7)=bmiscd(II,JJ,KK,7)
          cm(8)=bmiscd(II,JJ,KK,8)
          cm(9)=bmiscd(II,JJ,KK,9)
          cm(10)=bmiscd(II,JJ,KK,10)
          cm(11)=bmiscd(II,JJ,KK,11)
          cm(12)=bmiscd(II,JJ,KK,12)
          cm(13)=bmiscd(II,JJ,KK,13)
          write(lmsg,'(a,f9.2,a,f9.2,a,f6.2,a,f6.2,a,f6.2,a,f6.2,
     &      a,f3.0,a,f3.0,a,f3.0,a,f3.0,a,f3.0,a,f3.0,a)') 
     &    ' time proportioning ON/OFF: heat cap ',cm(2),
     &    'W cool cap ',cm(3),'W heat ON ',cm(4),
     &    'C heat OFF ',cm(5),'C cool ON ',cm(6),
     &    'C cool OFF ',cm(7),'C tot heat ',cm(8),
     &    'mn min heat ON',cm(9),'mn min heat OFF',cm(10),
     &    'mn tot cool mnts',cm(11),' min cool ON',cm(12),
     &    'mn min cool OFF',cm(13),'mn.'
          call SDELIM(lmsg,message,'S',IW)
C          call edisp248(6,message,80)
        ELSEIF(IBCLAW(II,JJ,KK).EQ.14)then
          SSTR='floating control'
          cm(2)=bmiscd(II,JJ,KK,2)
          cm(3)=bmiscd(II,JJ,KK,3)
          cm(4)=bmiscd(II,JJ,KK,4)
          cm(5)=bmiscd(II,JJ,KK,5)
          cm(6)=bmiscd(II,JJ,KK,6)
          cm(7)=bmiscd(II,JJ,KK,7)
          cm(8)=bmiscd(II,JJ,KK,8)
          cm(9)=bmiscd(II,JJ,KK,9)
          cm(10)=bmiscd(II,JJ,KK,10)
          cm(11)=bmiscd(II,JJ,KK,11)
          cm(12)=bmiscd(II,JJ,KK,12)
          cm(13)=bmiscd(II,JJ,KK,13)
          cm(14)=bmiscd(II,JJ,KK,14)
          cm(15)=bmiscd(II,JJ,KK,15)
          write(lmsg,'(a,f7.2,a,f5.2,a,f5.2,a,f5.2,a,f7.2,a,f5.2,
     &      a,f5.2,a,f5.2,a,f9.1,a,f7.1,a,f3.0,a,f9.1,a,f7.1,
     &      a,f3.0,a)') 
     &    ' floating 3-pos: heat sp',cm(2),
     &    'C ht dbd',cm(3),'C ht shut df',cm(4),
     &    'C ht open df',cm(5),
     &    'C cool sp',cm(6),'C cl dbd',cm(7),
     &    'C cl open df',cm(8),'C cl shut df',cm(9),
     &    'C max ht cap',cm(10),'W min ht cap',cm(11),
     &    'W ROC ht',cm(12),'ts max cl cap',cm(13),
     &    'W min cl cap',cm(14),'W ROC cl',cm(15),'ts.'
          call SDELIM(lmsg,message,'S',IW)
C          call edisp248(6,message,80)
        ELSEIF(IBCLAW(II,JJ,KK).EQ.15)then
          SSTR='opt. start logic ctl'
          cm(2)=bmiscd(II,JJ,KK,2)
          cm(3)=bmiscd(II,JJ,KK,3)
          cm(4)=bmiscd(II,JJ,KK,4)
          cm(5)=bmiscd(II,JJ,KK,5)
          cm(6)=bmiscd(II,JJ,KK,6)
          cm(7)=bmiscd(II,JJ,KK,7)
          cm(8)=bmiscd(II,JJ,KK,8)
          cm(9)=bmiscd(II,JJ,KK,9)
          cm(10)=bmiscd(II,JJ,KK,10)
          if(nint(cm(7)).eq.0)then
            msg1=' optimum 4am start:'
            write(lmsg,'(2a,f9.2,a,f6.2,a,f5.2,a,f5.2,a,f5.2,a)') 
     &      msg1,': heating capacity',cm(2),
     &      'W heating setpoint',cm(3),'C temperature diff ',cm(4),
     &      'C desired time of arrival ',cm(5),
     &      'Hr minimum time difference ',cm(6),'Hr.'
          elseif(nint(cm(7)).eq.1)then
            msg1=' optimum user def start'
            write(lmsg,'(2a,f9.2,a,f6.2,a,f5.2,a,f5.2,a,f5.2,a,f5.2,a)') 
     &      msg1,': heating capacity',cm(2),
     &      'W heating setpoint',cm(3),'C temperature diff ',cm(4),
     &      'C desired time of arrival ',cm(5),
     &      'Hr minimum time difference ',cm(6),'Hr starting at ',
     &      cm(8),'Hr.'
          elseif(nint(cm(7)).eq.2)then
            msg1=' optimum Birtles&John Eq'
            write(lmsg,'(2a,f9.2,a,f6.2,a,f5.2,a,f5.2,a,f5.2,a,f5.2,
     &        a,f5.2,a,f5.2,a)') 
     &      msg1,' start: heating capacity',cm(2),
     &      'W heating setpoint',cm(3),'C temperature diff ',cm(4),
     &      'C desired time of arrival ',cm(5),
     &      'Hr minimum time difference ',cm(6),
     &      'Hr B&J Eqn 1st Coef ',cm(8),' B&J 2nd coef ',cm(9),
     &      ' B&J 3rd coef ',cm(10),'.'
          endif
          call SDELIM(lmsg,message,'S',IW)
C          call edisp248(6,message,80)
        ELSEIF(IBCLAW(II,JJ,KK).EQ.16)then
          SSTR='opt. stop logic ctl'
          message='optimal stop logic report TBD'
        ELSEIF(IBCLAW(II,JJ,KK).EQ.17)then
          SSTR='fuzzy logic PI-PD ctl'
          message='fuzzy logic report TBD'
        ELSEIF(IBCLAW(II,JJ,KK).EQ.18)then
          SSTR='null controller.'
          message='null controller has no data'
        ELSEIF(IBCLAW(II,JJ,KK).EQ.19)then
          SSTR='ideal multi-sensor'
          cm(1)=bmiscd(II,JJ,KK,1)
          cm(2)=bmiscd(II,JJ,KK,2)
          cm(3)=bmiscd(II,JJ,KK,3)
          cm(4)=bmiscd(II,JJ,KK,4)
          cm(5)=bmiscd(II,JJ,KK,5)
          cm(6)=bmiscd(II,JJ,KK,6)
          cm(7)=bmiscd(II,JJ,KK,7)
          cm(8)=bmiscd(II,JJ,KK,8)
          write(lmsg,'(a,f8.0,a,f6.0,a,f8.0,a,f6.0,a,f6.1,a,f6.1,a)')     
     &      ' Ideal multi-sensor: max heat cp ',cm(2),
     &      'W min heat cp ',cm(3),'W max cool cp ',cm(4),
     &      'W min heat cp ',cm(5),'W Heat stpt ',cm(6),
     &      'C cool stpt ',cm(7),'C Aux:'
          llongmsg=lnblnk(lmsg)
          lx=llongmsg+1
          nitems=nint(cm(8))
          DO 79 L=1,nitems
            ndx=8+6*(L-1)
            icsn(1)=nint(bmiscd(II,JJ,KK,ndx+1))
            icsn(2)=nint(bmiscd(II,JJ,KK,ndx+2))
            icsn(3)=nint(bmiscd(II,JJ,KK,ndx+3))
            icsn(4)=nint(bmiscd(II,JJ,KK,ndx+4))
            SSTR=' '
            call decodesensor(0,icsn,SSTR,SSTRB,ier)

C Append the sensor msg to the end of lmsg and increment lx position.
            llmsg=lnblnk(SSTRB)+17
            ly=lx+llmsg
            if(ly.lt.247)then
              write(lmsg(lx:ly),'(2a,2F6.1)') SSTRB(1:lnblnk(SSTRB)),
     &          ' h/c',bmiscd(II,JJ,KK,ndx+5),bmiscd(II,JJ,KK,ndx+6)
              lx=ly+1
            endif

C << there will not be sufficient room in the text buffer for 2nd
C << and 3rd aux sensors so these need to be written to subsequent
C << lines of the report.

 79      continue
          call SDELIM(lmsg,message,'S',IW)
C          call edisp248(6,message,80)
        ELSEIF(IBCLAW(II,JJ,KK).EQ.20)then
          SSTR='evaporative source'
          cm(2)=bmiscd(II,JJ,KK,2)
          cm(3)=bmiscd(II,JJ,KK,3)
          cm(4)=bmiscd(II,JJ,KK,4)
          cm(5)=bmiscd(II,JJ,KK,5)
          cm(6)=bmiscd(II,JJ,KK,6)
          cm(7)=bmiscd(II,JJ,KK,7)
          cm(8)=bmiscd(II,JJ,KK,8)
          cm(9)=bmiscd(II,JJ,KK,9)
          cm(10)=bmiscd(II,JJ,KK,10)
          cm(11)=bmiscd(II,JJ,KK,11)
          if(nint(cm(2)).eq.0)then
            lmsg=' wet surface in zone (see actuator) '
          elseif(nint(cm(2)).eq.1)then
            msg1=' swiming pool surface: '
            write(lmsg,'(2a,f9.2,a,f6.3,a,f6.3,a,f5.2,a,f5.2,a,f5.3,
     &        a,f5.2,a,f8.2,a,f7.2,a)') 
     &        msg1,' pool surface area',cm(3),'m^2 pool surf conv htc',
     &        cm(4),' pool surf radiative htc ',cm(5),
     &        ' pool temperature ',cm(6),'C ground temperature ',cm(7),
     &        'C water supply rate ',cm(8),
     &        '(kg/s) water supply temperature ',cm(9),
     &        'C submerged wall area ',cm(10),'m^2 and U value ',
     &        cm(11),'.'
          endif
          call SDELIM(lmsg,message,'S',IW)
C          call edisp248(6,message,80)
        ELSEIF(IBCLAW(II,JJ,KK).EQ.21)then
          SSTR='slave capacity controller'
          cm(2)=bmiscd(II,JJ,KK,2)
          cm(3)=bmiscd(II,JJ,KK,3)
          cm(4)=bmiscd(II,JJ,KK,4)
          write(lmsg,'(a,i2,a,f9.2,a,f9.2,a)') 
     &      ' master/slave control: index of master control loop',
     &       nint(cm(2)),' slave maximum heating capacity ',cm(3),
     &      ' slave maximum cooling capacity ',cm(4),'.'
          call SDELIM(lmsg,message,'S',IW)
C          call edisp248(6,message,80)
        ELSEIF(IBCLAW(II,JJ,KK).EQ.22)then
          SSTR='VAV and CAV air based '
          cm(2)=bmiscd(II,JJ,KK,2)
          cm(3)=bmiscd(II,JJ,KK,3)
          cm(4)=bmiscd(II,JJ,KK,4)
          cm(5)=bmiscd(II,JJ,KK,5)
          cm(6)=bmiscd(II,JJ,KK,6)
          write(lmsg,'(a,f9.2,a,f7.3,a,f7.3,a,f7.4,a,f7.4,a)') 
     &    ' VAV and CAV air based: reheat cap ',cm(2),
     &    'W air supply temp',cm(3),'C room setpoint ',cm(4),
     &    'C max air flow rate ',cm(5),'m^3/s min air flow rate',
     &    cm(6),'m^3/s' 
          call SDELIM(lmsg,message,'S',IW)
C          call edisp248(6,message,80)
        ELSEIF(IBCLAW(II,JJ,KK).EQ.23)then
          SSTR='Sensed temp setpts '
          cm(2)=bmiscd(II,JJ,KK,2)
          cm(3)=bmiscd(II,JJ,KK,3)
          write(lmsg,'(2a,f9.2,a,f7.3)') 
     &    ' Match sensed values (ideal:2 setpoints)',
     &    ' heating setpt col. no. in tdf ',cm(2),
     &    ' cooling setpt col. no. in tdf ',cm(3)
          call SDELIM(lmsg,message,'S',IW)
        ELSEIF(IBCLAW(II,JJ,KK).EQ.24)THEN
          SSTR='Adaptive human comfort model'
          msg1=' '
          cm(1)=bmiscd(II,JJ,KK,1)
          cm(2)=bmiscd(II,JJ,KK,2)
          cm(3)=bmiscd(II,JJ,KK,3)
          cm(4)=bmiscd(II,JJ,KK,4)
          cm(5)=bmiscd(II,JJ,KK,5)
          cm(6)=bmiscd(II,JJ,KK,6)
          cm(7)=bmiscd(II,JJ,KK,7)
          cm(8)=bmiscd(II,JJ,KK,8)
          cm(9)=bmiscd(II,JJ,KK,9)
          write(lmsg,'(2a,f9.1,a,f9.1,a,f9.1,a,f9.1,a,f7.2,a,f7.2,a
     &    ,F7.2,a,F7.2)') 
     &    msg1,' max heating capacity ',cm(2),
     &    'W min heating capacity ',cm(3),'W max cooling capacity ',
     &    cm(4),'W min cooling capacity ',cm(5),
     &    'W. Heating setpoint ',cm(6),'C cooling setpoint ',
     &    cm(7),' Running mean response parameter ',CM(8),
     &    ' Comfort band (default is 2) ',CM(9)
          call SDELIM(lmsg,message,'S',IW)
        ELSEIF(IBSN(II,1).EQ.-99)then
          SSTR='control law not used'
          message='type -99 control law not used'
        ENDIF
        return
      ENDIF

      if(icfoc.eq.1)then

C Plant control law.
        SSTR='undefined control        '
        message='not yet coded for system controls.'
        if(IPCLAW(II,JJ,KK).EQ.0)then
          SSTR='period off        '
          message=' Period switch off control has no data.'
        elseif(IPCLAW(II,JJ,KK).EQ.1)then
          SSTR='PID flux control.  '
          msg1=' '; msg2=' '; msg3=' '
          cm(2)=pmiscd(II,JJ,KK,2)
          cm(3)=pmiscd(II,JJ,KK,3)
          cm(4)=pmiscd(II,JJ,KK,4)
          cm(5)=pmiscd(II,JJ,KK,5)
          cm(6)=pmiscd(II,JJ,KK,6)
          cm(7)=pmiscd(II,JJ,KK,7)
          cm(8)=pmiscd(II,JJ,KK,8)
          cm(9)=pmiscd(II,JJ,KK,9)
          cm(10)=pmiscd(II,JJ,KK,10)
          I2=INT(cm(2))
          if(I2.eq.1.or.I2.eq.-1) msg1='non-recursive postnl'
          if(I2.eq.2.or.I2.eq.-2) msg1='recursive postnl'
          if(I2.eq.3.or.I2.eq.-3) msg1='recursive vel`y rect'
          if(I2.eq.4.or.I2.eq.-4) msg1='recursive vel`y rect'
          if(I2.eq.5.or.I2.eq.-5) msg1='recursive vel`y trapez'
          I7=INT(cm(7))
          I8=INT(cm(8))
          I9=INT(cm(9))
          if(I2.eq.1.or.I2.eq.-1.or.I2.eq.2.or.I2.eq.-2)then
            if(I7.eq.1)then
              write(msg2,'(a,f7.1)') ' Intrg act [s]',cm(8)
              if(I9.eq.1)then
                write(msg3,'(a,f7.1)') ' Deriv act [s]',cm(10)
              endif
            else
              if(I8.eq.1)then
                write(msg2,'(a,f7.1)') ' Deriv act [s]',cm(9)
              endif
            endif
          elseif(I2.eq.3.or.I2.eq.-3)then
            write(msg2,'(a,f7.1)') ' Intrg act [s]',cm(7)
          elseif(I2.eq.4.or.I2.eq.-4.or.I2.eq.5.or.I2.eq.-5)then
            write(msg2,'(a,f7.1)') ' Intrg act [s]',cm(7)
            write(msg3,'(a,f7.1)') ' Deriv act [s]',cm(8)
          endif
          write(lmsg,'(2a,f5.1,a,f9.1,a,f9.1,a,f7.2,a,f7.2,4a)') 
     &    msg1,' mode ',cm(2),' max capacity ',cm(3),
     &    'W min capacity ',cm(4),'W setpoint ',
     &    cm(5),'C Tmp range ',cm(6),'C ',msg2,' ',msg3
          call SDELIM(lmsg,message,'S',IW)
C          call edisp248(6,message,80)

        elseif(IPCLAW(II,JJ,KK).EQ.2)then
          SSTR='PID flow control. '
          msg1=' '; msg2=' '; msg3=' '
          cm(2)=pmiscd(II,JJ,KK,2)
          cm(3)=pmiscd(II,JJ,KK,3)
          cm(4)=pmiscd(II,JJ,KK,4)
          cm(5)=pmiscd(II,JJ,KK,5)
          cm(6)=pmiscd(II,JJ,KK,6)
          cm(7)=pmiscd(II,JJ,KK,7)
          cm(8)=pmiscd(II,JJ,KK,8)
          cm(9)=pmiscd(II,JJ,KK,9)
          cm(10)=pmiscd(II,JJ,KK,10)
          I2=INT(cm(2))
          if(I2.eq.1.or.I2.eq.-1) msg1='non-recursive postnl'
          if(I2.eq.2.or.I2.eq.-2) msg1='recursive postnl'
          if(I2.eq.3.or.I2.eq.-3) msg1='recursive vel rect'
          if(I2.eq.4.or.I2.eq.-4) msg1='recursive vel rect'
          if(I2.eq.5.or.I2.eq.-5) msg1='recursive vel trapez'
          I7=INT(cm(7))
          I8=INT(cm(8))
          I9=INT(cm(9))
          if(I2.eq.1.or.I2.eq.-1.or.I2.eq.2.or.I2.eq.-2)then
            if(I7.eq.1)then
              write(msg2,'(a,f7.1)') ' Intrg act [s]',cm(8)
              if(I9.eq.1)then
                write(msg3,'(a,f7.1)') ' Deriv act [s]',cm(10)
              endif
            else
              if(I8.eq.1)then
                write(msg2,'(a,f7.1)') ' Deriv act [s]',cm(9)
              endif
            endif
          elseif(I2.eq.3.or.I2.eq.-3)then
            write(msg2,'(a,f7.1)') ' Intrg act [s]',cm(7)
          elseif(I2.eq.4.or.I2.eq.-4.or.I2.eq.5.or.I2.eq.-5)then
            write(msg2,'(a,f7.1)') ' Intrg act [s]',cm(7)
            write(msg3,'(a,f7.1)') ' Deriv act [s]',cm(8)
          endif
          write(lmsg,'(2a,f4.1,a,f9.5,a,f9.5,a,f7.2,a,f7.2,4a)') 
     &    msg1,' mode ',cm(2),' max flow ',cm(3),
     &    'kg/s min flow ',cm(4),'kg/s setpoint ',
     &    cm(5),'C throt range ',cm(6),'C ',msg2,' ',msg3
          call SDELIM(lmsg,message,'S',IW)
C          call edisp248(6,message,80)

        elseif(IPCLAW(II,JJ,KK).EQ.3)then
          SSTR='Prop numerical ctrl.'
          msg1=' Prop numerical'
          cm(2)=pmiscd(II,JJ,KK,2)
          cm(3)=pmiscd(II,JJ,KK,3)
          cm(4)=pmiscd(II,JJ,KK,4)
          cm(5)=pmiscd(II,JJ,KK,5)
          cm(6)=pmiscd(II,JJ,KK,6)
          write(lmsg,'(2a,f7.2,a,f7.2,a,f7.2,a,f7.2,a,f7.2,a)') 
     &    msg1,' max output ',cm(2),' min output ',cm(3),
     &    ' upper setpoint ',cm(4),' lower setpoint ',
     &    cm(5),' DS to overcome hystersis ',cm(6),'.'
          call SDELIM(lmsg,message,'S',IW)
C          call edisp248(6,message,80)

        endif
        IF(IPCLAW(II,JJ,KK).EQ.4) SSTR='Optimum start ctl. '
        IF(IPCLAW(II,JJ,KK).EQ.5) SSTR='Prop`l damper ctl.'
        if(IPCLAW(II,JJ,KK).EQ.6)then
          SSTR='Null controller.'
          message=' Null control has no data.'
        endif
        IF(IPCLAW(II,JJ,KK).EQ.7) SSTR='Duty Cycle Ctlr.'
        if(IPCLAW(II,JJ,KK).EQ.8)then
          SSTR='On-Off control.'
          cm(2)=pmiscd(II,JJ,KK,2)
          cm(3)=pmiscd(II,JJ,KK,3)
          cm(4)=pmiscd(II,JJ,KK,4)
          cm(5)=pmiscd(II,JJ,KK,5)
          cm(6)=pmiscd(II,JJ,KK,6)
          cm(7)=pmiscd(II,JJ,KK,7)
          cm(8)=pmiscd(II,JJ,KK,8)
          write(lmsg,'(a,f5.1,a,f7.2,a,f7.2,a,f10.3,a,f10.3,a,f7.1,a,
     &      f7.1,a)') 
     &      ' OnOff std mode ',cm(2),' on setpt ',cm(3),
     &      'C off setpt ',cm(4),'C output @ hi ',cm(5),
     &      ' output @ lo ',cm(6),' sensor lag ',cm(7),
     &      's actuator lag ',cm(8),'s.'
          call SDELIM(lmsg,message,'S',IW)
C          call edisp248(6,message,80)
        endif
        IF(IPCLAW(II,JJ,KK).EQ.9) SSTR='Multi-sensor on-off.'
        IF(IPCLAW(II,JJ,KK).EQ.10) SSTR='Cogen. controller'
        IF(IPCLAW(II,JJ,KK).EQ.14) SSTR='Multi-sensor '
        IF(IPCLAW(II,JJ,KK).EQ.15) SSTR='PI Room Ctrl'
        IF(IPCLAW(II,JJ,KK).EQ.16) SSTR='Amb Temp Cmpnstn ctrl'
        IF(IPCLAW(II,JJ,KK).EQ.17) SSTR='Ideal Optimal start ctrl'
        IF(IPCLAW(II,JJ,KK).EQ.18) SSTR='Ideal Optimal stop ctrl'
        return
      ENDIF

      if(icfoc.eq.2)then

C Flow control law.
        SSTR='undefined law '
        IF(IFCLAW(II,JJ,KK).EQ.0)then
          SSTR='on / off                 '
          msg1=' on/off '
          cm(2)=fmiscd(II,JJ,KK,2)
          write(msg2,'(a,f7.2)') ' setpoint ',cm(2)
          cm(3)=fmiscd(II,JJ,KK,3)
          if(int(cm(3)).eq.-1)msg3=' inverse action'
          if(int(cm(3)).eq.1) msg3=' direct action'
          cm(4)=fmiscd(II,JJ,KK,4)
          write(msg4,'(a,f5.3)') ' ON fraction ',cm(4)

C Gather up all of the message parts into lmsg and then
C use sdelim to remove extra white space.
          write(lmsg,'(5a)') 
     &      msg1(1:lnblnk(msg1)),msg2(1:lnblnk(msg2)),
     &      msg3(1:lnblnk(msg3)),msg4(1:lnblnk(msg4)),'.'
          call SDELIM(lmsg,message,'S',IW)
C          call edisp248(6,message,80)
        elseif(IFCLAW(II,JJ,KK).EQ.1)then
          SSTR='proportional + hysteresis'
          cm(2)=fmiscd(II,JJ,KK,2)
          cm(3)=fmiscd(II,JJ,KK,3)
          cm(4)=fmiscd(II,JJ,KK,4)
          cm(5)=fmiscd(II,JJ,KK,5)
          cm(6)=fmiscd(II,JJ,KK,6)
          write(lmsg,'(a,f7.2,a,f4.1,a,f6.2,a,f5.1,a,f6.2,a)') 
     &      ' proportional + hysteresis: signal lower limit ',cm(2),
     &      ' valve lower limit (%)',cm(3),' signal upper limit ',
     &      cm(4),' valve upper limit (%)',cm(5),' dS ',cm(6),'.'
          call SDELIM(lmsg,message,'S',IW)
C          call edisp248(6,message,80)
        elseif(IFCLAW(II,JJ,KK).EQ.2)then
          SSTR='low/default/mid/hi       '
          cm(2)=fmiscd(II,JJ,KK,2)
          cm(3)=fmiscd(II,JJ,KK,3)
          cm(4)=fmiscd(II,JJ,KK,4)
          cm(5)=fmiscd(II,JJ,KK,5)
          cm(6)=fmiscd(II,JJ,KK,6)
          cm(7)=fmiscd(II,JJ,KK,7)
          write(lmsg,'(a,f7.2,a,f6.2,a,f6.2,a,f5.2,a,f5.2,a,f5.2,a)') 
     &      ' range setpoints: low ',cm(2),' mid ',cm(3),
     &      ' high ',cm(4),' actuation ranges: low (< low sp)',cm(5),
     &      ' mid (>mid sp) ',cm(6),' high (>high sp)',cm(7),'.'
          call SDELIM(lmsg,message,'S',IW)
C          call edisp248(6,message,80)
        elseif(IFCLAW(II,JJ,KK).EQ.3)then

C Set short SSTR and then begin to build up the longer message (lmsg)
C based on the miscel data items.
C << this report does not recognise a single sensor NOT control
C << or a single sensor SLAVE control.
          SSTR='multi sensor on/off      '
          cm(1)=fmiscd(II,JJ,KK,1)
          nitems=nint(cm(1))
          do 77 mloop=2,nitems
            cm(mloop)=fmiscd(II,JJ,KK,mloop)
  77      continue
          NSEN=NINT(cm(3))
          if(nint(cm(2)).eq.0)then
            write(lmsg,'(a,i1,a)') 
     &        ' multi-sensor: normally closed with ',nsen,
     &        ' sensors: For'
          elseif(nint(cm(2)).eq.1)then
            write(lmsg,'(a,i1,a)') 
     &        ' multi-sensor: normally open with ',nsen,
     &        ' sensors: For'
          else
            write(lmsg,'(a,i1,a)') 
     &        ' multi-sensor: unknown position with ',nsen,
     &        ' sensors: For'
          endif
          llongmsg=lnblnk(lmsg)
          lx=llongmsg+1
          DO 358 ISEN=1,NSEN
            ndx=4+8*(ISEN-1)
            call eclose(cm(ndx),-3.0,0.001,oka)
            if(oka)then
              inear=nint(cm(ndx+1))
              msg1=' sensor unknown'
              if(inear.eq.0)msg1=' ambient T'
              if(inear.eq.2)msg1=' wind speed'
              if(inear.eq.3)msg1=' wind direction'
              if(inear.eq.4)msg1=' diffuse solar'
              if(inear.eq.5)msg1=' direct solar'
              if(inear.eq.6)msg1=' senses RH'
            endif
            call eclose(cm(ndx),-4.0,0.001,okb)
            if(okb)then
              inear=nint(cm(ndx+1))
              write(msg1,'(2a)') ' sense node ',
     &          ndnam(inear)(1:lnblnk(ndnam(inear)))
            endif
            write(msg2,'(a,f7.2)') ' setpoint ',cm(ndx+4)
            if(int(cm(ndx+5)).eq.-1)msg3=' inverse action'
            if(int(cm(ndx+5)).eq.1) msg3=' direct action'
            if(int(cm(ndx+7)).eq.1) msg4=' AND'
            if(int(cm(ndx+7)).eq.2) msg4=' OR'
            if(isen.lt.nsen)then
              write(msg,'(a,i1,4a)') ' sensor ',isen,
     &          msg1(1:lnblnk(msg1)),msg2(1:lnblnk(msg2)),
     &          msg3(1:lnblnk(msg3)),msg4(1:lnblnk(msg4))
            else
              write(msg,'(a,i1,4a)') ' sensor ',isen,
     &          msg1(1:lnblnk(msg1)),msg2(1:lnblnk(msg2)),
     &          msg3(1:lnblnk(msg3)),'.'
            endif

C Append the sensor msg to the end of lmsg and increment lx position.
            llmsg=lnblnk(msg)
            ly=lx+llmsg
            if(ly.lt.247)then
              write(lmsg(lx:ly),'(a)') msg(1:llmsg)
              lx=ly+1
            endif
 358      continue

C Tell the user about the multi sensor data.
          call SDELIM(lmsg,message,'S',IW)
          call edisp248(6,message,80)

C Nicol Occupant resopnse model for control
        elseif(IFCLAW(II,JJ,KK).EQ.4)then
          SSTR='Nicol alg version 1.0 '
          cm(2)=fmiscd(II,JJ,KK,2)
          iPGT=ifsn(ii,3)
          write(lmsg,'(2A,F4.2,2A)')SSTR,
     &    'with running mean response parameter of ',cm(2),
     &    ' measuring operative temp of node ',NDNAM(iPGT)
          call SDELIM(lmsg,message,'S',IW)

C PI controller
        elseif(IFCLAW(II,JJ,KK).EQ.5)then
          SSTR='PI controller '
          cm(2)=fmiscd(II,JJ,KK,2)
          cm(3)=fmiscd(II,JJ,KK,3)
          cm(4)=fmiscd(II,JJ,KK,4)
          cm(5)=fmiscd(II,JJ,KK,5)
          cm(6)=fmiscd(II,JJ,KK,6)
          cm(7)=fmiscd(II,JJ,KK,7)
          cm(8)=fmiscd(II,JJ,KK,8)
          write(lmsg,'(2a,f6.2,a,f6.2,a,f6.2,a,f6.2,a,f8.2,a,f8.2,
     &    a,f8.2)')
     &    SSTR(1:lnblnk(SSTR)),' with nominal setpoint=',cm(2),
     &    '; maximum setpoint=',cm(3),'; maximum setpoint=',cm(4),
     &    '; proportional band=',cm(5),'; integral reset=',cm(6),
     &    '; action flag=',cm(7),'; optional multi sensing loop=',cm(8)
          call SDELIM(lmsg,message,'S',IW)
        endif
        return
      endif

      if(icfoc.eq.3)then

C Global control. Return sensor type << check required if values other
C than -3 allowed for sensor >>.
        SSTR='undefined law '
        IF(igctyp(II,JJ,KK).eq.1)SSTR= 'Global on-off controller'
        IF(igctyp(II,JJ,KK).eq.2)SSTR= 'Global capacity controller'
        IF(igctyp(II,JJ,KK).eq.3)SSTR= 'Global schedule on controller'
        lmsg='not yet coded for global controls.'

C Multisensing controller

        IF(igctyp(II,JJ,KK).eq.5)THEN
          SSTR= 'Global multisensor controller'
          DO 90 I=1,MISC
            cm(I)=gmiscd(II,JJ,KK,I)
 90       CONTINUE
          nclp=(nint(cm(1)))/3
          LMSG='Currently sensing'
          DO 5050 ICLP=1,NCLP
            IF(NINT(CM(ICLP*3-1)).EQ.1)THEN
              MSG2=' mass flow loop #'
            ELSEIF(NINT(CM(ICLP*3-1)).EQ.2)THEN
              MSG2=' plant loop #'
            ENDIF
            MSG1=' '
            IF(ICLP.NE.NCLP.OR.ICLP.EQ.1)THEN
              IADR=NINT(CM(ICLP*3+1))
              IF(IADR.EQ.1)THEN
                msg1=' AND'
              ELSEIF(IADR.EQ.2)THEN
                msg1=' OR'
              ELSEIF(IADR.EQ.3)THEN
                msg1=' acting as NOT and'
              ELSEIF(IADR.EQ.4)THEN
                msg1=' acting as SLAVE and'
              ELSEIF(IADR.EQ.5)THEN
                msg1=' acting as MAX and'
              ELSEIF(IADR.EQ.6)THEN
                msg1=' acting as MIN and'
              ENDIF
            ENDIF
            NLPS=NINT(CM(ICLP*3))
            LN1=LNBLNK(MSG1)
            LN2=LNBLNK(MSG2)
            LN=LNBLNK(LMSG)
            WRITE(LMSG,'(3A,i4,2a)')LMSG(1:LN),MSG2(1:LN2),' ',
     &      nlps,' ',MSG1(1:LN1)
 5050     CONTINUE
      IF(NINT(CM(NCLP*3+1)).EQ.1)THEN
        MSG2='Actuates massflow loop #'
      ELSEIF(NINT(CM(NCLP*3+1)).EQ.2)THEN
        MSG2='Actuates plant loop #'
      ENDIF
          IALP=NINT(CM(NCLP*3+2))
          LN=LNBLNK(LMSG)
          LN2=LNBLNK(MSG2)
          WRITE(LMSG,'(4A,I4)')LMSG(1:LN),'. ',MSG2(1:LN2),' ',IALP
        ENDIF
        call SDELIM(lmsg,message,'S',IW)
        return
      ENDIF

      if(icfoc.eq.4)then

C Power systems control law.
        SSTR='undefined law '
        IF(IECTYP(II,JJ,KK).eq.1)SSTR= 'Voltage > Reactive power '
        IF(IECTYP(II,JJ,KK).eq.10)SSTR= 'ApparentPower > on/off '
        lmsg='not yet coded for electrical controls.'
        return
      ENDIF

      if(icfoc.eq.5)then

C Optical control law.
        SSTR='undefined law '     
        if(IOCLAW(II,JJ,KK).eq.1)then

C Here needs to propose which optical property to use - done elsewhere
C This code is not activated yet from bpfcontrol.F
C Only law 2 for bidirectional data will be used at the moment (May 2008) 
          SSTR= 'optical switching '
          msg1=' optical switching:'
        elseif(IOCLAW(II,JJ,KK).eq.2)then
          SSTR= 'bidirectional set '
          msg1='bidirectional set:'
        endif
        cm(1)=omiscd(II,JJ,KK,1)
        cm(2)=omiscd(II,JJ,KK,2)
        cm(3)=omiscd(II,JJ,KK,3)         
        cm(4)=omiscd(II,JJ,KK,4)
        cm(5)=omiscd(II,JJ,KK,5)
        cm(6)=omiscd(II,JJ,KK,6) 
        cm(7)=omiscd(II,JJ,KK,7)
        cm(8)=omiscd(II,JJ,KK,8)
        cm(9)=omiscd(II,JJ,KK,9) 
        cm(10)=omiscd(II,JJ,KK,10)
        cm(11)=omiscd(II,JJ,KK,11)
        cm(12)=omiscd(II,JJ,KK,12) 
        cm(13)=omiscd(II,JJ,KK,13)
        write(lmsg,'(a,f9.2,2a,f9.2,a,f7.3,a)') 
     &  ' Sensed values are:',cm(2),',',
     &  'above this value and below or equal to:',cm(4),
     &  'optical properties number:',cm(3),' will be used.'
        call SDELIM(lmsg,message,'S',IW)
        write(lmsg,'(a,f9.2,2a,f9.2,a,f7.3,a)') 
     &  ' Sensed values are:',cm(4),',',
     &  'above this value and below or equal to:',cm(6),
     &  'optical properties number:',cm(5),' will be used.'
        call SDELIM(lmsg,message,'S',IW)
        write(lmsg,'(a,f9.2,2a,f9.2,a,f7.3,a)') 
     &  ' Sensed values are:',cm(6),',',
     &  'above this value and below or equal to:',cm(8),
     &  'optical properties number:',cm(7),' will be used.'
        call SDELIM(lmsg,message,'S',IW)
        write(lmsg,'(a,f9.2,2a,f9.2,a,f7.3,a)') 
     &  ' Sensed values are:',cm(8),',',
     &  'above this value and below or equal to:',cm(10),
     &  'optical properties number:',cm(9),' will be used.'
        call SDELIM(lmsg,message,'S',IW)
        write(lmsg,'(a,f9.2,2a,f9.2,a,f7.3,a)') 
     &  ' Sensed values are:',cm(10),',',
     &  'above this value and below or equal to:',cm(12),
     &  'optical properties number:',cm(11),' will be used.'
        call SDELIM(lmsg,message,'S',IW)
        write(lmsg,'(a,f9.2,2a,f7.3,a)') 
     &  ' Sensed values are:',cm(12),',',
     &  'above this value optical properties number:',cm(13),
     &  ' will be used.'
        call SDELIM(lmsg,message,'S',IW)
        return
      endif

      if(icfoc.eq.6)then
      
C Ideal complex fenestration control law.
        SSTR='undefined control        '
        IF(iCFCctllaw(II,JJ,KK).EQ.1)then
          SSTR='basic control        '
          msg1=' basic control:'
          cm(1)=CFCmiscdata(II,JJ,KK,1)
          cm(2)=CFCmiscdata(II,JJ,KK,2)
          cm(3)=CFCmiscdata(II,JJ,KK,3)
          cm(4)=CFCmiscdata(II,JJ,KK,4)
          cm(5)=CFCmiscdata(II,JJ,KK,5)

          if(nint(cm(1)).eq.2)then

C Actuator is shading ON/OFF state.
            write(lmsg,'(2a,f9.1,a,f9.1,a,f9.1,a,f9.1)') 
     &        msg1,' [C] shade ON (close) setpoint ',cm(2),
     &        '[C] shade OFF (open) setpoint ',cm(3)
          elseif(nint(cm(1)).eq.4)then

C Actuator is slat angle.
            write(lmsg,'(2a,f9.1,a,f9.1,a,f9.1,a,f9.1)') 
     &        msg1,' [C] shade ON (close) setpoint ',cm(2),
     &        '[C] shade OFF (open) setpoint ',cm(3),
     &        '[deg] close slat angle ',cm(4),
     &        '[deg] open slat angle ',cm(5)
          endif
          call SDELIM(lmsg,message,'S',IW)
        ELSEIF(iCFCctllaw(II,JJ,KK).EQ.2)then
          SSTR='schedule        '
          msg1=' schedule:'
          cm(1)=CFCmiscdata(II,JJ,KK,1)
          cm(2)=CFCmiscdata(II,JJ,KK,2)
          cm(3)=CFCmiscdata(II,JJ,KK,3)

          write(lmsg,'(2a,i3,a,f9.1)') 
     &        msg1,' shade ON (1) or OFF (0) ',nint(cm(2)),
     &        '[deg] slat angle ',cm(3)
          call SDELIM(lmsg,message,'S',IW)

        ENDIF
      endif  
        
      END

C ******** LSTCNTLD
C List one day type of a building/plant/flow/global/optical/
C complex fenestration control. 
      SUBROUTINE LSTCNTLD(itru,icfoc,II,JJ)
#include "building.h"
#include "net_flow.h"
#include "control.h"
      
      integer lnblnk  ! function definition

C Parameters
      integer itru    ! unit to write error
      integer icfoc   ! control domain to work with 
      integer II      ! index of the control
      integer JJ      ! index of day type
      integer lncal   ! length of calendar day name

      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      character calename*32,calentag*12,calendayname*32

      character outs*124,SSTR*96,TMP*96,LTMP*248
      CHARACTER PERST1*14,PERST2*44,PERST3*44
      character outsa*84,outs400*400

C List data for function II and day type JJ.
      if(icfoc.eq.0)then
        IBD=IBCDV(II,JJ,1)
        IED=IBCDV(II,JJ,2)
        LL=NBCDP(II,JJ)
      elseif(icfoc.eq.1)then
        IBD=IPCDV(II,JJ,1)
        IED=IPCDV(II,JJ,2)
        LL=NPCDP(II,JJ)
      elseif(icfoc.eq.2)then
        IBD=IFCDV(II,JJ,1)
        IED=IFCDV(II,JJ,2)
        LL=NFCDP(II,JJ)
      elseif(icfoc.eq.3)then
        IBD=IGCDV(II,JJ,1)
        IED=IGCDV(II,JJ,2)
        LL=NGCDP(II,JJ)
      elseif(icfoc.eq.5)then
        IBD=IOCDV(II,JJ,1)
        IED=IOCDV(II,JJ,2)
        LL=NOCDP(II,JJ)
      elseif(icfoc.eq.6)then
        IBD=iCFCctldatevalid(II,JJ,1)
        IED=iCFCctldatevalid(II,JJ,2)
        LL=nCFCdayctlperiods(II,JJ)
      endif
      NTSPH=1
      CALL EPERSTR(IYEAR,IBD,1,IED,24,
     &       NTSPH,2,0,PERST1,PERST2,PERST3,IIER)

      if(icfoc.eq.0)then
        nnd=nbcdt(ii)
      elseif(icfoc.eq.1)then
        nnd=npcdt(ii)
      elseif(icfoc.eq.2)then
        nnd=nfcdt(ii)
      elseif(icfoc.eq.3)then
        nnd=ngcdt(ii)
      elseif(icfoc.eq.5)then
        nnd=nocdt(ii)
      elseif(icfoc.eq.6)then
        nnd=nCFCctldaytypes(ii)
      endif

C If nnd is zero then omitt days of validity information, otherwise
C include it in the documentation line.
      lncal=lnblnk(CALENTAG(JJ))
      if(nnd.eq.0)then
        WRITE(outs,'(3A,I2,A)') ' Day type ',CALENTAG(JJ)(1:lncal),
     &    ' control has ',LL,' periods.'
      else
        WRITE(outs,'(3A,I2,A)') ' Control is valid ',
     &    PERST3(9:lnblnk(PERST3)),' with ',LL,' periods.'
      endif
      call edisp(itru,outs)

      call edisp(itru,
     &' Per|Start|Sensing  |Actuating  | Control law description')
      DO 30 K=1,LL
        CALL EVCNTRL(icfoc,II,JJ,K,'T',SSTR)
        CALL EVCNTRLAW(icfoc,II,JJ,K,TMP,LTMP)
        if(icfoc.eq.0)then
          WRITE(outsa,83)K,TBCPS(II,JJ,K),SSTR(1:21),TMP(1:20)
          NITEMS=INT(BMISCD(II,JJ,K,1))
        elseif(icfoc.eq.1)then
          WRITE(outsa,83)K,TPCPS(II,JJ,K),SSTR(1:21),TMP(1:20)
          NITEMS=INT(PMISCD(II,JJ,K,1))
        elseif(icfoc.eq.2)then
          WRITE(outsa,83)K,TFCPS(II,JJ,K),SSTR(1:21),TMP(1:20)
          NITEMS=INT(FMISCD(II,JJ,K,1))
        elseif(icfoc.eq.3)then
          WRITE(outsa,83)K,TGCPS(II,JJ,K),SSTR(1:21),TMP(1:20)
          NITEMS=INT(GMISCD(II,JJ,K,1))
        elseif(icfoc.eq.5)then
          WRITE(outsa,83)K,TOCPS(II,JJ,K),SSTR(1:21),TMP(1:21)
          NITEMS=INT(OMISCD(II,JJ,K,1))
        elseif(icfoc.eq.6)then
          WRITE(outsa,83)K,CFCctlperiodstart(II,JJ,K),
     &                   SSTR(1:21),TMP(1:21)
          NITEMS=INT(CFCmiscdata(II,JJ,K,1))
        endif
  83    FORMAT(I4,F6.2,' ',a,' ',a)

         if(NITEMS.GT.0)then

C Write the initial part of the control followed by description.
          write(outs400,'(a,1x,a)',IOSTAT=IOS,ERR=3) outsa(1:32),
     &        ltmp(1:lnblnk(ltmp))

C For zone, plant, flow and global controls list out the verbose 
C decoding of the control law attributes.  Note that edisp248 is
C able to work with strings longer than 248 characters.
          if(icfoc.eq.0.or.icfoc.eq.1.or.icfoc.eq.2.or.icfoc.eq.3)then
            call edisp248(itru,outs400,90)
          endif
        elseif(NITEMS.eq.0)then
          write(outs,'(a)',IOSTAT=IOS,ERR=3) outsa(1:52)
          CALL EDISP(ITRU,outs)
        endif
   30 CONTINUE
      return

 3    if(IOS.eq.2)then
        CALL USRMSG(' ','Permission error generating report.','W')
      else
        CALL USRMSG(' ','Problem generating report.','W')
      endif
      return
      end

C ******************** decodesensor
C Decodesensor takes the 4 indices that define a sensor
C and returns a descriptive string.
C icfoc is the index of the control domain
C icsn(5) is the array of indices
C SSTR is the string (96 char) returned
C SSTRB is brief string (48 char) returned
      subroutine decodesensor(icfoc,icsn,SSTR,SSTRB,ier)

C << adapt report for case of thermophysical property substitution >>

#include "building.h"
#include "geometry.h"
#include "plant.h"
#include "net_flow.h"
      
      integer lnblnk  ! function definition

      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/pcnam/pcname(mpcom)
      COMMON/MFLOW2/NDNAM(0:MNOD)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)

      integer icfoc
      dimension icsn(5)
      character sstr*96,sstrb*48
      CHARACTER ZN*12,NNAM*14,DESC*20
      CHARACTER NDNAM*12,pcname*15,contmnam*12
      CHARACTER sn*12

      sstr=' '
      sstrb=' '
      if(icfoc.eq.0)then

C Ideal Sensor.
        IF(icsn(1).GT.0)THEN
          write(zn,'(a)') zname(icsn(1))
          if(icsn(2).gt.0)then
            icc=IZSTOCN(icsn(1),icsn(2))
            write(sn,'(a)') SSNAME(icc)
          else
            sn='-'
          endif
          IF(icsn(2).EQ.0)THEN
            WRITE(SSTR,'(3a)') 'senses dry bulb temperature in ',
     &        zn(1:lnblnk(zn)),'.'
            WRITE(SSTRB,'(3a)') 'senses dry bulb T in ',
     &        zn(1:lnblnk(zn)),'.'
          ELSEIF(icsn(2).GT.0.AND.icsn(3).EQ.0)THEN
            WRITE(SSTR,'(5a)')  'senses temp at ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
            WRITE(SSTRB,'(5a)')  'senses T at ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
          ELSEIF(icsn(2).GT.0.AND.icsn(3).GT.0)THEN
            WRITE(SSTR,'(5a)')'senses temp within ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
            WRITE(SSTRB,'(5a)')'senses T within ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
          ENDIF
        ELSEIF(icsn(1).EQ.0)THEN
          SSTR='senses the temperature of the current zone.'
          SSTRB='senses db T of the current zone.'
        ELSEIF(icsn(1).EQ.-2)THEN
          if(icsn(2).gt.0)then
            write(zn,'(a)') zname(icsn(2))
            WRITE(SSTR,'(3a)')'senses a mix of db T and MRT in ',
     &        zn(1:lnblnk(zn)),'.'
            WRITE(SSTRB,'(3a)')'senses mix db T & MRT in ',
     &        zn(1:lnblnk(zn)),'.'
          else
            WRITE(SSTR,'(2a)')'senses a mix of db T and MRT in ',
     &        'the current zone.'
            WRITE(SSTRB,'(2a)')'senses mix db T & MRT in ',
     &        'the current zone.'
          endif
        ELSEIF(icsn(1).EQ.-3)THEN
          IF(icsn(2).EQ.0)THEN
            SSTR='senses ambient dry bulb temperature.'
            SSTRB='senses ambient db T.'
          ELSEIF(icsn(2).EQ.1)THEN
            SSTR='senses the sol-air temperature.'
            SSTRB='senses the sol-air T.'
          ELSEIF(icsn(2).EQ.2)THEN
            SSTR='senses wind speed.'
            SSTRB='senses wind speed.'
          ELSEIF(icsn(2).EQ.3)THEN
            SSTR='senses wind direction.'
            SSTRB='senses wind direction.'
          ELSEIF(icsn(2).EQ.4)THEN
            SSTR='senses dif hor rad.'
            SSTRB='senses dif hor rad.'
          ELSEIF(icsn(2).EQ.5)THEN
            SSTR='senses dir nor rad.'
            SSTRB='senses dir nor rad.'
          ELSEIF(icsn(2).EQ.6)THEN
            SSTR='senses ext rel hum.'
            SSTRB='senses ext RH.'
          ENDIF
        ELSEIF(icsn(1).EQ.-5)THEN
          WRITE(SSTR,65)icsn(2)
  65      FORMAT('reads from temporal file item.',I2)
          WRITE(SSTRB,66)icsn(2)
  66      FORMAT('reads temporal item.',I2)
        ELSEIF(icsn(1).EQ.-6)THEN
          SSTR='sensed value from function generator.'
          SSTRB='sensed value from func gen.'
        ELSEIF(icsn(1).EQ.-99)THEN
          SSTR=' construction substitution.'
          SSTRB=' MLC substitution.'
        ELSE
          SSTR='senses UNKNOWN condition.'
          SSTRB='senses UNKNOWN condition.'
        ENDIF
      elseif(icfoc.eq.1)then

C Plant Sensor.
        SSTR='undefined condition'
        IF(icsn(1).GT.0)THEN
          write(zn,'(a)') zname(icsn(1))
          if(icsn(2).gt.0)then
            icc=IZSTOCN(icsn(1),icsn(2))
            write(sn,'(a)') SSNAME(icc)
          else
            sn='-'
          endif
          IF(icsn(2).EQ.0)THEN
            WRITE(SSTR,'(3a)') 'senses dry bulb temperature in ',
     &        zn(1:lnblnk(zn)),'.'
            WRITE(SSTRB,'(3a)') 'senses db T in ',
     &        zn(1:lnblnk(zn)),'.'
          ELSEIF(icsn(2).GT.0.AND.icsn(3).EQ.0)THEN
            WRITE(SSTR,'(5a)')  'senses temp at ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
            WRITE(SSTRB,'(5a)')  'senses T at ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
          ELSEIF(icsn(2).GT.0.AND.icsn(3).GT.0)THEN
            WRITE(SSTR,'(5a)')'senses temp within ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
            WRITE(SSTRB,'(5a)')'senses T within ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
          ENDIF
        ELSEIF((icsn(1).EQ.-1).AND.(icsn(4).LE.0)) THEN
          WRITE(SSTR,113)
     &      icsn(2),pcname(icsn(2))(1:LNBLNK(pcname(icsn(2)))),
     &      icsn(3)
 113      FORMAT('senses var in compt. ',I2,':',a,' @ node no. ',I2)
          WRITE(SSTRB,114)
     &      icsn(2),pcname(icsn(2))(1:LNBLNK(pcname(icsn(2))))
 114      FORMAT('senses var in compt. ',I2,':',a)
        ELSEIF((icsn(1).EQ.-1).AND.(icsn(4).GT.0))THEN
          WRITE(SSTR,1132)
     &icsn(2),pcname(icsn(2))(1:LNBLNK(pcname(icsn(2)))),
     &icsn(3),icsn(4),
     &pcname(icsn(4))(1:LNBLNK(pcname(icsn(4)))),icsn(5)
 1132     FORMAT('sen var diff bet compt. ',I2,':',a,
     &    ' @ node ',I2,'and compt ', I2,':',a,
     &    ' @ node ', I2)     
        ELSEIF(icsn(1).EQ.-2)THEN
          write(zn,'(a)') zname(icsn(2))
          WRITE(SSTR,'(3a)')'senses a mix of db T and MRT in ',
     &      zn(1:lnblnk(zn)),'.'
          WRITE(SSTRB,'(3a)')'senses mix db T & MRT in ',
     &      zn(1:lnblnk(zn)),'.'
        ELSEIF(icsn(1).EQ.-3)THEN
          IF(icsn(2).EQ.0)THEN
            SSTR='senses ambient dry bulb temperature.'
            SSTRB='senses ambient db T.'
          ELSEIF(icsn(2).EQ.1)THEN
            SSTR='senses the sol-air temperature.'
            SSTRB='senses the sol-air T.'
          ELSEIF(icsn(2).EQ.2)THEN
            SSTR='senses wind speed.'
            SSTRB='senses wind speed.'
          ELSEIF(icsn(2).EQ.3)THEN
            SSTR='senses wind direction.'
            SSTRB='senses wind direction.'
          ELSEIF(icsn(2).EQ.4)THEN
            SSTR='senses dif hor rad.'
            SSTRB='senses dif hor rad.'
          ELSEIF(icsn(2).EQ.5)THEN
            SSTR='senses dir nor rad.'
            SSTRB='senses dir nor rad.'
          ELSEIF(icsn(2).EQ.6)THEN
            SSTR='senses ext rel hum.'
            SSTRB='senses ext RH.'
          ENDIF
        ELSEIF(icsn(1).EQ.-4)THEN
          SSTR='senses  mass flow node or connection.'
          SSTRB='senses  mass flow node or connec.'
        ELSEIF(icsn(1).EQ.-5)THEN
          SSTR='reads measurements from a boundry data file.'
          SSTRB='reads from a boundry data file.'
        ELSEIF(icsn(1).EQ.-6)THEN
          SSTR='sensed value from function generator.'
          SSTRB='sensed value from func gen.'
        ENDIF
      elseif(icfoc.eq.2)then

C Flow Sensor.
        SSTR='undefined condition'
        IF(icsn(1).GT.0)THEN
          write(zn,'(a)') zname(icsn(1))
          if(icsn(2).gt.0)then
            icc=IZSTOCN(icsn(1),icsn(2))
            write(sn,'(a)') SSNAME(icc)
          else
            sn='-'
          endif
          IF(icsn(2).EQ.0)THEN
            WRITE(SSTR,'(3a)')'senses dry bulb temperature in ',
     &        zn(1:lnblnk(zn)),'.'
            WRITE(SSTRB,'(3a)') 'senses dry bulb T in ',
     &        zn(1:lnblnk(zn)),'.'
          ELSEIF(icsn(2).GT.0.AND.icsn(3).EQ.0)THEN
            WRITE(SSTR,'(5a)')  'senses temp at ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
            WRITE(SSTRB,'(5a)')  'senses T at ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
          ELSEIF(icsn(2).GT.0.AND.icsn(3).GT.0)THEN
            WRITE(SSTR,'(5a)')'senses temp within ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
            WRITE(SSTRB,'(5a)')'senses T within ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
          ENDIF
        ELSEIF(icsn(1).EQ.-1)THEN
          WRITE(SSTR,163)icsn(2),icsn(1)
 163      FORMAT('senses temp of plant component',I2,' in zone.',I2)
          WRITE(SSTRB,164)icsn(2),icsn(1)
 164      FORMAT('senses T of plant comp',I2,' in zone.',I2)
        ELSEIF(icsn(1).EQ.-2)THEN
          write(zn,'(a)') zname(icsn(2))
          WRITE(SSTR,'(2a)')'senses a mix of db T and MRT in ',
     &      zn(1:lnblnk(zn))
          WRITE(SSTRB,'(3a)')'senses mix db T & MRT in ',
     &      zn(1:lnblnk(zn)),'.'
        ELSEIF(icsn(1).EQ.-3)THEN
          IF(icsn(2).EQ.0)THEN
            SSTR='senses ambient dry bulb temperature.'
            SSTRB='senses ambient db T.'
          ELSEIF(icsn(2).EQ.1)THEN
            SSTR='senses the sol-air temperature.'
            SSTRB='senses the sol-air T.'
          ELSEIF(icsn(2).EQ.2)THEN
            SSTR='senses wind speed.'
            SSTRB='senses wind speed.'
          ELSEIF(icsn(2).EQ.3)THEN
            SSTR='senses wind direction.'
            SSTRB='senses wind direction.'
          ELSEIF(icsn(2).EQ.4)THEN
            SSTR='senses dif hor rad.'
            SSTRB='senses dif hor rad.'
          ELSEIF(icsn(2).EQ.5)THEN
            SSTR='senses dir nor rad.'
            SSTRB='senses dir nor rad.'
          ELSEIF(icsn(2).EQ.6)THEN
            SSTR='senses ext rel hum.'
            SSTRB='senses ext RH.'
          ELSEIF(ICSN(2).EQ.7)THEN
            SSTR='senses temp for adaptive behaviour model'
            SSTRB='senses T_op & T_out for ABM'
          ENDIF
        ELSEIF(icsn(1).EQ.-4)THEN

C Decode which connection or node is sensed. NNAM is the string
C representing the index of the node.
          L=icsn(2)
          NNAM=' '
          CALL INTSTR(L,NNAM,LEN,IER)
          DESC=NDNAM(L)(1:lnblnk(NDNAM(L)))
          IF(icsn(4).EQ.0)THEN
            if(icsn(2).gt.0.and.icsn(3).gt.0)then
              nl1=lnblnk(ndnam(icsn(2)))
              nl2=lnblnk(ndnam(icsn(3)))
              write(SSTR,'(3a,i3,3a,i3,a)')'sensors are nodes ',
     &          ndnam(icsn(2))(1:nl1),' (',icsn(2),') & ',
     &          ndnam(icsn(3))(1:nl2),' (',icsn(3),')'
              write(SSTRB,'(4a)')'sensor nodes ',
     &          ndnam(icsn(2))(1:nl1),' & ',ndnam(icsn(3))(1:nl2)
            elseif(icsn(2).gt.0.and.icsn(3).eq.0)then
              WRITE(SSTR,'(4A)')'senses node (',NNAM(1:LNBLNK(NNAM)),
     &          ') ',DESC(1:lnblnk(desc))
              WRITE(SSTRB,'(4A)')'senses node (',NNAM(1:LNBLNK(NNAM)),
     &          ') ',DESC(1:lnblnk(desc))
            else
              SSTR='sensor node(s) not yet defined'
              SSTRB='sensor node(s) not yet defined'
            endif
          ELSEIF(icsn(4).EQ.1)THEN
            WRITE(SSTR,'(A,i3,4a)') 'senses connection (',
     &        L,') ',NDNAM(NODPS(L))(1:lnblnk(NDNAM(NODPS(L)))),' - ',
     &        NDNAM(NODNE(L))(1:lnblnk(NDNAM(NODNE(L))))
            WRITE(SSTRB,'(4a)') 'senses conn ',
     &        NDNAM(NODPS(L))(1:lnblnk(NDNAM(NODPS(L)))),' - ',
     &        NDNAM(NODNE(L))(1:lnblnk(NDNAM(NODNE(L))))
          ELSEIF(icsn(4).EQ.2)THEN
            WRITE(SSTR,'(6a)')'senses node (',NNAM(1:LNBLNK(NNAM)),') '
     &      ,DESC(1:lnblnk(desc)),', contaminant: ',contmnam(icsn(3))
            WRITE(SSTRB,'(6a)')'senses node (',NNAM(1:LNBLNK(NNAM)),') '
     &      ,DESC(1:lnblnk(desc)),', contm: ',contmnam(icsn(3))
          ENDIF
        ENDIF
      elseif(icfoc.eq.5)then

C Optical Sensor.
        IF(icsn(1).GT.0)THEN
          write(zn,'(a)') zname(icsn(1))
          if(icsn(2).gt.0)then
            icc=IZSTOCN(icsn(1),icsn(2))
            write(sn,'(a)') SSNAME(icc)
          else
            sn='-'
          endif
          IF(icsn(2).EQ.0)THEN
            WRITE(SSTR,'(3a)') 'senses dry bulb temperature in ',
     &        zn(1:lnblnk(zn)),'.'
            WRITE(SSTRB,'(3a)') 'senses dry bulb T in ',
     &        zn(1:lnblnk(zn)),'.'

C<<<need to cancel the sensing of surface temperature and surface node temperature?
          ELSEIF(icsn(2).GT.0.AND.icsn(3).EQ.0)THEN
            WRITE(SSTR,'(5a)')  'senses temp at ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
            WRITE(SSTRB,'(5a)')  'senses T at ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
          ELSEIF(icsn(2).GT.0.AND.icsn(3).GT.0)THEN
            WRITE(SSTR,'(5a)')'senses temp within ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
            WRITE(SSTRB,'(5a)')'senses T within ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
C<<<< Until here
          ENDIF

C<<< Cancel also the next two
        ELSEIF(icsn(1).EQ.0)THEN
          SSTR='senses the temperature of the current zone.'
          SSTRB='senses db T of the current zone.'
        ELSEIF(icsn(1).EQ.-2)THEN
          write(zn,'(a)') zname(icsn(2))
          WRITE(SSTR,'(3a)')'senses a mix of dbT and MRT in ',
     &      zn(1:lnblnk(zn)),'.'
          WRITE(SSTRB,'(3a)')'senses mix db T & MRT in ',
     &      zn(1:lnblnk(zn)),'.'
C<<<< Until here
        ELSEIF(icsn(1).EQ.-3)THEN
          IF(icsn(2).EQ.0)THEN
            SSTR='senses ambient dry bulb temperature.'
            SSTRB='senses ambient db T.'
C<<< Cancel also the next 6?
          ELSEIF(icsn(2).EQ.1)THEN
            SSTR='senses the sol-air temperature.'
            SSTRB='senses the sol-air T.'
          ELSEIF(icsn(2).EQ.2)THEN
            SSTR='senses wind speed.'
            SSTRB='senses wind speed.'
          ELSEIF(icsn(2).EQ.3)THEN
            SSTR='senses wind direction.'
            SSTRB='senses wind direction.'
          ELSEIF(icsn(2).EQ.4)THEN
            SSTR='senses dif hor rad.'
            SSTRB='senses dif hor rad.'
          ELSEIF(icsn(2).EQ.5)THEN
            SSTR='senses dir nor rad.'
            SSTRB='senses dir nor rad.'
          ELSEIF(icsn(2).EQ.6)THEN
            SSTR='senses ext rel hum.'
            SSTRB='senses ext RH.'
C<<<< Until here

          ENDIF
        ELSEIF(icsn(1).EQ.-7)THEN
          SSTR='senses radiation at outside of surface.'
          SSTRB='senses incident rad.'
        ELSEIF(icsn(1).EQ.-8)THEN
          SSTR='senses lux via daylight coef. method.'
          SSTRB='senses lux levels'
        ELSEIF(icsn(1).EQ.-9)THEN
          SSTR='senses lux levels.'
          SSTRB='senses lux levels'
        ELSE
C<<<< Not sure if next line is needed
          SSTR='senses UNKNOWN condition.'
          SSTRB='senses UNKNOWN condition.'
        ENDIF
        
      elseif(icfoc.eq.6)then
C Complex fenestration Sensor.

C Ideal Sensor.
        IF(icsn(1).GT.0)THEN
          write(zn,'(a)') zname(icsn(1))
          if(icsn(2).gt.0)then
            icc=IZSTOCN(icsn(1),icsn(2))
            write(sn,'(a)') SSNAME(icc)
          else
            sn='-'
          endif
          IF(icsn(2).EQ.0)THEN
            WRITE(SSTR,'(3a)') 'senses dry bulb temperature in ',
     &        zn(1:lnblnk(zn)),'.'
            WRITE(SSTRB,'(3a)') 'senses dry bulb T in ',
     &        zn(1:lnblnk(zn)),'.'
          ELSEIF(icsn(2).GT.0.AND.icsn(3).EQ.0)THEN
            WRITE(SSTR,'(5a)')  'senses temp at ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
            WRITE(SSTRB,'(5a)')  'senses T at ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
          ELSEIF(icsn(2).GT.0.AND.icsn(3).GT.0)THEN
            WRITE(SSTR,'(5a)')'senses temp within ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
            WRITE(SSTRB,'(5a)')'senses T within ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
          ENDIF
        ELSEIF(icsn(1).EQ.0)THEN
          SSTR='no sensor - schedule only.'
          SSTRB='no sensor - schedule only'
        ELSEIF(icsn(1).EQ.-2)THEN
          write(zn,'(a)') zname(icsn(2))
          WRITE(SSTR,'(3a)')'senses a mix of db T and MRT in ',
     &      zn(1:lnblnk(zn)),'.'
          WRITE(SSTRB,'(3a)')'senses mix db T & MRT in ',
     &      zn(1:lnblnk(zn)),'.'
        ELSEIF(icsn(1).EQ.-3)THEN
          IF(icsn(2).EQ.0)THEN
            SSTR='senses ambient dry bulb temperature.'
            SSTRB='senses ambient db T.'
          ELSEIF(icsn(2).EQ.1)THEN
            SSTR='senses the sol-air temperature.'
            SSTRB='senses the sol-air T.'
          ELSEIF(icsn(2).EQ.2)THEN
            SSTR='senses wind speed.'
            SSTRB='senses wind speed.'
          ELSEIF(icsn(2).EQ.3)THEN
            SSTR='senses wind direction.'
            SSTRB='senses wind direction.'
          ELSEIF(icsn(2).EQ.4)THEN
            SSTR='senses dif hor rad.'
            SSTRB='senses dif hor rad.'
          ELSEIF(icsn(2).EQ.5)THEN
            SSTR='senses dir nor rad.'
            SSTRB='senses dir nor rad.'
          ELSEIF(icsn(2).EQ.6)THEN
            SSTR='senses ext rel hum.'
            SSTRB='senses ext RH.'
          ENDIF
C        ELSEIF(icsn(1).EQ.-5)THEN
C          WRITE(SSTR,65)icsn(2)
C  65      FORMAT('reads from temporal file item.',I2)
C          WRITE(SSTRB,66)icsn(2)
C  66      FORMAT('reads temporal item.',I2)
C        ELSEIF(icsn(1).EQ.-6)THEN
C          SSTR='sensed value from function generator.'
C          SSTRB='sensed value from func gen.'
C        ELSEIF(icsn(1).EQ.-99)THEN
C          SSTR=' construction substitution.'
C          SSTRB=' MLC substitution.'
        ELSEIF(icsn(1).EQ.-4)THEN
          write(zn,'(a)') zname(icsn(2))
          if(icsn(3).gt.0)then
            icc=IZSTOCN(icsn(2),icsn(3))
            write(sn,'(a)') SSNAME(icc)
          else
            sn='-'
          endif
          WRITE(SSTR,'(5a)')  'senses incident sol rad on ',
     &    sn(1:lnblnk(sn)),' in ',zn(1:lnblnk(zn)),'.'
          WRITE(SSTRB,'(5a)')  'senses sol rad on ',sn(1:lnblnk(sn)),
     &        ' in ',zn(1:lnblnk(zn)),'.'
        ELSE
          SSTR='senses UNKNOWN condition.'
          SSTRB='senses UNKNOWN condition.'
        ENDIF
              
      endif

      return
      end
 
C ******************** ezctlc 
C Ezctlc checks all building control functions and plant
C control loops for legality.

      subroutine ezctlc(ier)
#include "building.h"
#include "geometry.h"
#include "plant.h"
#include "net_flow.h"
#include "control.h"
#include "blc25_open_windows.h"

      common/OUTIN/IUOUT,IUIN
      integer icascf
      common/cctl/icascf(mcom)
      integer ncomp,ncon
      common/c1/ncomp,ncon
      common/c9/npcomp,nci(mpcom),cdata(mpcom,mmiscd)
      common/c12ps/npcdat(mpcom,9),ipofs1(mcoefg),ipofs2(mcoefg,mpvar)
      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp)
      COMMON/MFLOW1/NNOD,NCMP,NCNN

      character outstr*124
      
      integer ii, jj, ll, kk
    
C This code loops throug the building (m=1), plant (m=2) and flow (m=3)
C control loops and tests them for validity. It's a mess of goto statements,
C and is a good candidate for rewriting. 

C Loop through building/plant/flow/optical control loops       
      do 10 m=1,4

      if ( m .eq. 1 ) then 
C Test assign # of parseable controls to # of building control loops 
        nbp=ncf
        
      elseif ( m .eq. 2 ) then 
C Test assign # of parseable controls to # of plant control loops
        nbp=ncl

      elseif ( m.eq. 3 ) then
C Test assign # of parseable controls to # of flow control loops
        nbp=ncc

      elseif ( m.eq. 4 ) then
C Test assign # of parseable controls to # of optical control loops
        nbp=nof
      endif 

C Test if parsable loops exist      
      if ( nbp .gt. 0  ) then

      do 20 i=1,nbp
C 'i' appears to serve no purpose in this code.     
      ii=i
      if(m.eq.2)goto 3
      if(m.eq.3)goto 22
      if(m.eq.4)goto 32

C Check for minimum allowable free cooling delta T
      do ii = 1, NCF
        nn=nbcdt(ii) 
        do jj = 1, nn
          ll = nbcdp(ii,jj)
          do kk = 1, ll
            if ( ibclaw(ii, jj, kk) .eq. 27) then
                if( bmiscd(II,JJ,KK,8) .lt. fMinDelTFreeCool )then
                    goto 1011
                end if
            end if 
          end do
        end do
      end do

C Set up data for building control function ii.
      isn1=ibsn(ii,1)
      isn2=ibsn(ii,2)
      isn3=ibsn(ii,3)
      ian1=iban(ii,1)
      ian2=iban(ii,2)
      ian3=iban(ii,3)

C No checking yet for the case of thermophysical substitution.
      if(isn1.eq.-99)goto 20

C Test for a control function not associated with a building zone.
C Remember to which zone this control function relates.
      icfz=0
      do 30 j=1,ncomp
        if(ii.eq.icascf(j))icfz=j
   30 continue
      if(icfz.eq.0)goto 20
      goto 4

C This line of code appears to serve no purpose, and will
C never be executed.       
    3 if(m.eq.3) goto 22

C Set up data for plant control loop ii.
      isn1=ipsn(ii,1)
      isn2=ipsn(ii,2)
      isn3=ipsn(ii,3)
      isn4=ipsn(ii,4)
      isn5=ipsn(ii,5)
      ian1=ipan(ii,1)
      ian2=ipan(ii,2)
      ian3=ipan(ii,3)
      goto 4

c set up data for mass flow control connections.
  22  isn1=ifsn(ii,1)
      isn2=ifsn(ii,2)
      isn3=ifsn(ii,3)
      ian1=ifan(ii,1)
      ian2=ifan(ii,2)
      ian3=ifan(ii,3)
      goto 4

c set up data for optical control function ii.
  32  isn1=iosn(ii,1)
      isn2=iosn(ii,2)
      isn3=iosn(ii,3)
      isn4=iosn(ii,4)
      ian1=ioan(ii,1)
      ian2=ioan(ii,2)
      ian3=ioan(ii,3)

C isn1 has been tested, therefore use its value to test isn2 & isn3.
    4 is=isn1+6
      if(is.gt.6)is=7
      goto(33,23,7,5,6,71,8)is

C Sensor references a function generator.
      if(is.eq.-1)then
         if(isn2.gt.9)goto 1008
         goto 9
      endif
      
C Senses data from a temporal file, 2nd item will temporal item.
  33  if(isn2.lt.1.or.isn2.gt.24)goto 1022
      goto 9

C Mixed temperature sensor.
    5 if(isn2.lt.0.or.isn2.gt.ncomp)goto 1002 ! if isn=0 sense current zone
      if(isn3.lt.0.or.isn3.gt.100)goto 1003
      goto 9

C Sensor within plant component.
C Check to see if component exists
    6 if(isn2.lt.0.or.isn2.gt.npcomp)goto 1002
C Check to see if sensed node exists    
      if(isn3.lt.1.or.isn3.gt.npcdat(isn2,8))goto 1003
      if(isn4.lt.0.or.isn4.gt.npcomp)goto 1009
      if ( isn4.gt.1) then
        if(isn5.lt.0.or.isn5.gt.npcdat(isn4,8))goto 1010
      endif
      goto 9

C Sensor located externally.
    7 if(isn2.lt.0.or.isn2.gt.7)goto 1002
      goto 9

C Sensor in the zone to which it is assigned.
   71 isn1=ii
      goto 72

C Sensor located within a building zone.
    8 if(isn1.gt.ncomp)goto 1001
   72 if(isn2.lt.0.or.isn2.gt.nzsur(isn1))goto 1002
      if(isn3.lt.0)goto 1003
      if(isn2.eq.0)goto 9

C If this called from simulator NELTS will have been
C instanciated so do a further check if node is within
C the number of layers in this construction.
      if(NELTS(isn1,isn2).gt.0)then
        nn=2*NELTS(isn1,isn2)+1
        if(isn3.gt.nn)goto 1003
      endif
      goto 9

c Sensor located within mass flow connection.
   23 if ((isn2.lt.0).or.((isn2.gt.ncnn).and.(isn2.gt.nnod))) goto 1007

C Check actuator details for legality.
    9 ia=ian1+5
      if(ia.gt.5)ia=6
      goto(25,24,11,12,13,14)ia

C Actuation is on zone node weighting.
C To avoid classes with optical controls additional logic added. The initial code was:
C   11 if(ian2.ne.icfz)goto 1005
   11 if(ian2.ne.icfz.and.((nof.eq.0).or.(ian1.ne.-5)))goto 1005
      if(ian3.lt.0.or.ian3.gt.100)goto 1006
      goto 20

C Actuation is on a plant component.
   12 if(ian2.lt.0.or.ian2.gt.npcomp)goto 1005

C Check to see if the acutuator is correct. Note: the following
C line is erroneous --- the number of control variables associated
C with a plant component is *NOT* constrained by the number of nodes
C in the plant component (despite what prj and other parts of bps
C might suggest).
C      if(ian3.lt.1.or.ian3.gt.npcdat(isn2,8))goto 1003
C
C The number of control variables is constrained by a variable 
C called 'MMISCD', which is assigned in plant.h. Each component
C record within the plant comnponent database defines the number 
C of controls associated the plant component --- this value can
C vary from 1->mmiscd (mmiscd = 5 as of January 2006).  However,
C this data is not presently available in the bps data model
C (the appropriate code would have to be added to MZPMXT). Instead,
C check to see the actuator does not exceed mmiscd:
      if(ian3.lt.1.or.ian3.gt.mmiscd)goto 1003

      goto 20

C Actuation on zone under consideration.
   13 ian1=ii
      goto 131

C Actuation is on a zone node.
   14 if(ian1.ne.icfz)goto 1004
C To avoid classes with optical controls additional logic added. The initial code was:
C  131 if(ian2.lt.0.or.ian2.gt.nzsur(ian1))goto 1005   
  131 if((ian2.lt.0.and.((nof.eq.0).or.(ian1.ne.-5))).or.
     &  (ian2.gt.nzsur(ian1).and.((nof.eq.0).or.(ian1.ne.-5))))then
        goto 1005
      endif  
      if(ian3.lt.0)goto 1006
      if(ian2.eq.0)goto 20
      if(isn2.eq.0)goto 20     ! necessary to avoid array out of bounds error in NELTS(isn1,isn2)
      
C If this called from simulator NELTS will have been
C instanciated so do a further check if node is within
C the number of layers in this construction.
      if(NELTS(isn1,isn2).gt.0)then
        nn=2*NELTS(icfz,ian2)+1
        if(ian3.gt.nn)goto 1006
      endif
      goto 20

c actuation is on a mass flow connection.
   24 if((ian2.lt.0).or.(ian2.gt.ncnn))goto 1005
      goto 20

c actuation is on a mass flow component.
   25 if((ian2.lt.0).or.((ian2.gt.ncnn).and.(ian2.gt.ncmp)))goto 1005

   20 continue
      endif         ! <- matches ' if ( nbp .gt. 0 ) then' ...
   10 continue
      return


C Error messages.
 1001 write(outstr,'(A,I3,A)') ' Sensor node: 1st item (',
     &  isn1,') > number of zones.'
      call edisp(iuout,outstr)
      goto 1000
 1002 write(outstr,'(A,I3,A)') ' Sensor node: 2nd item (',
     &  isn2,') < 0 or > surfaces.'
      call edisp(iuout,outstr)
      goto 1000
 1003 write(outstr,'(A,I3,A)')' Sensor node: 3rd item (',isn3,
     &  ') invalid.'
      call edisp(iuout,outstr)
      goto 1000
 1004 write(outstr,'(A,I3)')' Actuator: 1st item invalid = ',ian1
      call edisp(iuout,outstr)
      goto 1000
 1005 write(outstr,'(A,I3)')' Actuator: 2nd item invalid = ',ian2
      call edisp(iuout,outstr)
      goto 1000
 1006 write(outstr,'(A,I3)')' Actuator: 3rd item invalid = ',ian3
      call edisp(iuout,outstr)
      goto 1000
 1007 call edisp(iuout,
     &' Sensor node: 2nd item < 0 or > mfn connections or nodes. ')
 1008 call edisp(iuout,' Sensor node: 2nd item invalid item: it is > 9')
      goto 1000
 1022 call edisp(iuout,' Sensor node: 2nd item < 1 or > columns/items.')
      goto 1000
 1009 write(outstr,'(A,I3,A)') ' Sensor node: 3rd item (',
     &  isn3,') < 0 or > surfaces.'
      call edisp(iuout,outstr)
      goto 1000
 1010 write(outstr,'(A,I3,A)')' Sensor node: 5th item (',isn5,
     &  ') invalid.'
      call edisp(iuout,outstr)
      goto 1000
 1011 write(outstr,'(A)')
     &' Delta T for free cooling is below the minimum 2 deg C'
      call edisp(iuout,outstr)
      goto 1000

 1000 if(m.eq.2)then
        write(outstr,'(A,I3)')' in plant control loop.',ii
        call edisp(iuout,outstr)
      else if(m.eq.1)then
        write(outstr,'(A,I3)')' in building control function.',ii
        call edisp(iuout,outstr)
      else
        write(outstr,'(A,I3)')' in flow control connection.',ii
        call edisp(iuout,outstr)
      endif
      call edisp(iuout,' ')
      ier=1

      return
      end

C ******************** tscdta
C Tscdta checks if TSC6 is called from a building control law.
C If so, TSC6 period data arrays are set here (as opposed
C to being set from the TSC menu in bps, i.e. in TSCON6 in file
C tscon.F).

      subroutine tscdta
#include "building.h"
#include "control.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TS/ITSFLG,NTSCPD,ITSSCH(MSCH,5),itsmus
      COMMON/TS6/idysv,ihrsv,irwcou,tsclaw,rewnd,nsncv1,nsncv2,pasone
      
      logical tsclaw,rewnd,pasone

      irwlaw=0

      do 500 i=1,ncf
        nn1 = nbcdt(i)
        if(nbcdt(i).eq.0)nn1=3
        do 600 j=1,nn1
          do 700 k=1,nbcdp(i,j)

C FATAL ERROR TEST: COUNT NUMBER OF CONTROL FUNCTIONS 
C SPECIFYING A 'REWIND' LAW - CURRENT MAXIMUM IS ONE 
C SUCH LAW PER CONFIGURATION CONTROL FILE.
            if(ibclaw(i,j,k).eq.15.or.ibclaw(i,j,k).eq.16)then
               irwlaw=irwlaw+1
               if(irwlaw.gt.1)goto 99                 

C SET TSC FLAG.
               itsflg=1

C SET FLAG INDICATING TSC6 SET FROM CONTOL LAW.
               tsclaw=.true.

C SET NUMBER OF TSC PERIODS.
               ntscpd=ntscpd+1
             
C SET TSC TYPE.
               itssch(ntscpd,3)=6

C PERIOD START TIME.
               itssch(ntscpd,1)=int(tbcps(i,j,k))
             
C PERIOD FINISH TIME.
               if(k.eq.nbcdp(i,j))then
C                 itssch(ntscpd,2)=23
                 itssch(ntscpd,2)=24
               else
C                 itssch(ntscpd,2)=int((tbcps(i,j,k+1)-1.))
                 itssch(ntscpd,2)=int(tbcps(i,j,k+1))
               endif
               if(ibclaw(i,j,k).eq.16)then
C                 itssch(ntscpd,2)=int((bmiscd(i,j,k,8)-1.))
                 itssch(ntscpd,2)=int(bmiscd(i,j,k,8))
               endif           

C PERIOD START DAY.
               itssch(ntscpd,4)=ibcdv(i,j,1)
             
C PERIOD FINISH DAY.
               itssch(ntscpd,5)=ibcdv(i,j,2)
  
C ADJUST PERIOD START DAY & HOUR IF PERIOD START = 0.0 HOURS,
C               if(itssch(ntscpd,1).eq.0)then
C                 itssch(ntscpd,1)=24
C                 itssch(ntscpd,4)=ibcdv(i,j,1)-1
C               endif
            endif
 700      continue
 600    continue
 500  continue
      return

   99 call edisp(iuout,' ')
      call edisp(iuout,'TSCDTA: Fatal error - time rewind')
      call edisp(iuout,'control specified more than once in')
      call edisp(iuout,'System Control file!')
      call pauses(3)   ! pause for 3 seconds then abort program.
      call epagend
      stop
      end

C ******************** FZDATA
C fzdat reads fuzzy logic data from configuration control file.
C It returns ier=1 if it cannot find the fuzzy data description
C section in the control file or had a problem reading the data.
      SUBROUTINE FZDATA(IUNIT,IER)

#include "building.h"
#include "control.h"

      common/OUTIN/IUOUT,IUIN
      COMMON/FZCTL1/FZM(MFP,3,5,10),ERRSAV(MFP),QSAV(MFP),
     &ACUT(3,5),FZCTIP(5,5),FZCTOP(5),COA(5)
      COMMON/FZCTL2/NFZF(MFP,3),FZT(MFP,3),NONZRO
      COMMON/FZCTL3/FZLABL,FZLOOK

      DIMENSION RVA(30)

      CHARACTER*8 FZLABL(MFP,3,5)
      CHARACTER*8 FZLOOK(MFP,5,5)
      CHARACTER WORD*20,OUTSTR*124,HDG*5
      
      IFP=0
      NOFP=0
      IFUZ=0
      IFOUND=0

      DO 150 K=1,NCF
        DO 145 KK=1,NBCDT(K)
         DO 140 KKK=1,NBCDP(K,KK)
           IF(IBCLAW(K,KK,KKK).EQ.17)IFUZ=IFUZ+1
  140    CONTINUE
 145    CONTINUE
150   CONTINUE
      
      IF(IFUZ.EQ.0)RETURN
      DO 550 LL=1,3000

C Read header : * FUZZY CONTROL.
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'* CONTROL ',IER)
        IF(IER.NE.0)GOTO 900
        HDG=OUTSTR(1:5)
        IF(HDG.NE.'* FUZ'.AND.HDG.NE.'* Fuz'.AND.HDG.NE.'* fuz')THEN
          IFOUND=0
          GOTO 550
        ENDIF
        IFOUND=1

        CALL STRIPC(IUNIT,OUTSTR,1,ITEMS,1,'No. fz data sets',IER)
        IF(IER.NE.0)GOTO 990
        KG=0
        CALL EGETWI(
     &    OUTSTR,KG,NOFP,1,20,'F','No. of fzy data sets',IER)   

        IF(NOFP.EQ.0)RETURN

        DO 650 I=1,NOFP
          CALL STRIPC(IUNIT,OUTSTR,1,ITEMS,1,'Fz data set',IER)
          IF(IER.NE.0)GOTO 990
          KG=0
          CALL EGETWI(
     &      OUTSTR,KG,IFP,1,20,'F','Fz data set',IER)
               
          DO 870 L=1,3
            CALL STRIPC(IUNIT,OUTSTR,1,ITEMS,1,'No. fz mb',IER)
            IF(IER.NE.0)GOTO 990
            KG=0
            CALL EGETWI(OUTSTR,KG,NFZF(IFP,L),1,5,'F',
     &        'No. fzy mbr',IER)

C Read in fuzzy labels.
            CALL STRIPC(IUNIT,OUTSTR,NFZF(IFP,L),ITEMS,1,
     &        'MF labels',IER)
            KG=0
            DO 872 M=1,NFZF(IFP,L)
              IF(IER.NE.0)GOTO 988
              CALL EGETW(OUTSTR,KG,WORD,'F','-',IER)
              FZLABL(IFP,L,M)=WORD(1:6)
  872       CONTINUE

C Membership function shape data.
            IRVA=NFZF(IFP,L)*6
            CALL EGETWRA(IUNIT,RVA,IRVA,0.,0.,'-','mem data',IER)
            IF(IER.NE.0)GOTO 989
            IV=0
            DO 864 N=1,NFZF(IFP,L)
              DO 868 KV=1,6
                IV=IV+1
                FZM(IFP,L,N,KV)=RVA(IV)
  868         CONTINUE
 864        CONTINUE
870       CONTINUE

          IVAL=1
          DO 777 K=1,NFZF(IFP,IVAL)
            CALL STRIPC(IUNIT,OUTSTR,0,ITEMS,1,'Lookup vals',IER)
            IF(IER.NE.0)GOTO 987
            KG=0
            IVALU=2
            DO 779 KL=1,NFZF(IFP,IVALU)
              CALL EGETW(OUTSTR,KG,WORD,'F','-',IER)
              IF(IER.NE.0)GOTO 987
              FZLOOK(IFP,K,KL)=WORD(1:6)
  779       CONTINUE
 777      CONTINUE

650     CONTINUE
        GOTO 950
550   CONTINUE

900   IF(IFOUND.EQ.0)GOTO 980

C Establish slopes and intercepts for each membership function.
950   CALL FZYFNC
      RETURN

C Error messages.
980   CALL EDISP(IUOUT,' * FUZZY label not found')
      ier=1
      return

987   CALL EDISP(IUOUT,'Error reading in fuzzy look-up table')
      ier=1
      return

988   CALL EDISP(IUOUT,'Error reading in fuzzy labels')
      ier=1
      return

989   CALL EDISP(IUOUT,'Error reading in fuzzy membership data')
      ier=1
      return

990   CALL EDISP(IUOUT,'Error reading in No. of fuzzy membership sets')
      ier=1
      return

      END
