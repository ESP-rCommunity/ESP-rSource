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

C  ***************** SPMMOD
C Subroutine SPMMOD calls the relevant special components subroutine for
C each special material node identified in SPMINIT. At the moment the
C only altered property is absorbed radiation. However, this can be
C changed according to the nature of the material. The change to the
C appropriate property should be done WITHIN the material subroutine.
C
C Current "special components" are as follows: 
C Renewable energy componets
C SPMCMP 1 - silicon solar cell model P=f(Q,T)
C SPMCMP 2 - constant efficiency solar cell model P=f(Q,eff)
C SPMCMP 3 - ducted wind turbine
C SPMCMP 4 - solar thermal collector
C SPMCMP 5 - equivalent one-diode PV model (WATSUN-PV model)

C Active material type components
C SPMCMP 51 - thermochromic glazing material 
C SPMCMP 52 - saturated surface with evaporation
C SPMCMP 53 - phase change material

      subroutine SPMMOD(ICOMP)

#include "building.h"

      common/spmfxst/ispmxist,spflnam 
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      character spflnam*72

C For each node with a special material identify the type which is held 
C in ispmtyp(mspmnod,1). If the affected zone matches the current zone
C then call the relevant subroutine.
      if(ispmxist.gt.0) then
        do 10 I=1,nspmnod
          ISPMNOD=I
          ispmtype=ispmtyp(I,1)
          if((ispmloc(I,1)).EQ.ICOMP) then
            if(ispmtype.eq.1) then
              call spmcmp1(ICOMP,ISPMNOD)
            elseif(ispmtype.eq.2) then
              call spmcmp2(ICOMP,ISPMNOD)
            elseif(ispmtype.eq.3) then
              call spmcmp3(ICOMP,ISPMNOD)
            elseif(ispmtype.eq.4) then
              call spmcmp4(ICOMP,ISPMNOD)
            elseif(ispmtype.eq.51) then
              call spmcmp51(ICOMP,ISPMNOD)
            elseif(ispmtype.eq.52) then
              call spmcmp52(ICOMP,ISPMNOD)
            elseif(ispmtype.eq.53) then
              call spmcmp53(ICOMP,ISPMNOD)
            elseif(ispmtype.eq.5) then
              call spmcmp5(ICOMP,ISPMNOD)
            endif
          endif
  10  continue
      endif
      return

      end
C 
C All special component are components which are an integral 
C part of the building fabric, however they are split into two
C distinct groups: 
C
C Special components 1-50 are renewable energy components for
C the embedded production of heat and/or power for a building.
C
C Special components 51-100 are active type materials such as
C thermochromic, photochromic, electrochromic glazings, phase
C change materials, etc.
C
C  ***************** SPMCMP1  
C Special material 1 is an equation which calculates the electrical
C output from some crystalline silicon solar cells.
C The required data items read from the array spmdat are:
C 1 - Open circuit voltage. (V)
C 2 - Short circuit current. (I)
C 3 - Voltage at Maximum power point.
C 4 - Current at maximum power point.
C 5 - Reference insolation. (W/m2)
C 6 - Reference temperature. (K)
C 7 - Number of series connected cells (not panels) (-)
C 8 - Number of parallel connected branches. (-)
C 9 - Number of panels in surface. (-)
C 10- Empirical value used in calculation of Io.
C 11- Load type (0-maximum power point 1-fixed R 2-fixed V)
C 12- Fixed V or R value
C 13- Shading effects (0-ignore 1-proportional power loss 2-total power loss
C                      3-direct rad. power loss)
C
C   Output data is stored in the common block RESSPM
C   with: DATASPM(*,5) - up to 5 special component results data 
C   items, STRSPM(*,5) - descriptive string for the data item. 

C This status of this component is STABLE, its output has been
C verified against experimental data. 

      subroutine SPMCMP1(ICOMP,ISPMNOD)

#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN

C Special materials
      COMMON/ENETSPM/PSPM(MSPMNOD),QSPM(MSPMNOD),
     &VSPM(MSPMNOD)

      COMMON/RESSPM/NDATSPM,DATASPM(MSPMNOD,3),STRSPM(MSPMNOD,3)
      COMMON/ZONSPMF/ZSPMF1(MCOM),ZSPMF2(MCOM)

      common/spmfxst/ispmxist,spflnam
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      common/spmatlbl/spmlabel(mspmnod)

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/COE32J/QTMCA(MS,MN,2)
      COMMON/COE32/QSOLI(MS,2),QSOLE(MS,2)
      COMMON/PVPNL/TPNL,QPNL,PVPWR

      COMMON/PREC1/SNA(MCOM,MS),ZOA(MCOM)
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),
     &ARP(MCOM,MS),ARF(MCOM,MS)  

      COMMON/SOLEXT/EXRAD(MS),EXDIR(MS),EXDIF(MS),EXSHAD(MS)      

      REAL VOC,ISC,VMP,IMP,QREF,TREF,SCELL,PBRCH,EMPV,VALF,
     &E,K,DF1,DF2,DF,IO,IL,VMP1,VMP2,VMP3,VMP4,
     &VMPMOD,VITER,A1,A2,IMOD,IITER, fMiscLossFactor

      character spmlabel*16, outs*124,spflnam*72,STRSPM*16
      COMPLEX VSPM

C Obtain the relevant panel data from the data base
C Component manufacturers data.

C Open circuit voltage.
      VOC=spmdat(ISPMNOD,1)

C Short circuit current.
      ISC=spmdat(ISPMNOD,2)

C Voltage at maximum power point.
      VMP=spmdat(ISPMNOD,3)

C Current at maximum power point.
      IMP=spmdat(ISPMNOD,4)

C Reference insolation and temp.
      QREF=spmdat(ISPMNOD,5)
      TREF=spmdat(ISPMNOD,6)

C Number of series connected CELL (NOT panels).
      SCELL=spmdat(ISPMNOD,7)

C Number of parallel connected branches.
      PBRCH=spmdat(ISPMNOD,8)

C Number of panels in surface.
      NPNNLS=NINT(spmdat(ISPMNOD,9))

C Empirical value used in calculation of diode current.
      EMPV=spmdat(ISPMNOD,10)
      if(EMPV.lt.1E-10)EMPV=10.

C Load type for the panel (0-maximum power point tracking (default)
C 1 - fixed resistance, 2 - fixed voltage).
      ILODT=NINT(spmdat(ISPMNOD,11))

C Fixed voltage (volts) or resistance value (ohms).
      VALF=spmdat(ISPMNOD,12)

C Shading treatment (0-ignored (default), 1-proportional power loss, 
C 2-total power loss, 3-power output at shaded insolation level
      ISHAD=NINT(spmdat(ISPMNOD,13))

C Miscellaneous loss factor. This factor derates the PV power 
C produced by the panel. Losses may be attributed to uncertainty
C in the manufacturer's rating, ageing, mismatch, soil and dirt, 
C snow, blocking diodes and wiring. 
      fMiscLossFactor = spmdat(ISPMNOD,14) 


C Physical constants: electrical charge and Boltzman constant.
      E=1.60E-19
      K=1.38E-23

C TPNL is the temperature of the PV panel, read from
C the nodal temperature.
      TPNL=TFC(ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2),
     &ispmloc(ISPMNOD,3))+273.0  

C Depending on whether the  panel is shaded and the treatment of the 
C shaded (ISHAD) panel determine the external incident solar radiation. 
C ISHAD=0 the radiation uses the current treatment, output is proportional
C to the % of the panel which is shaded (included for backwards compatibility)
C ISHAD=1 same as ISHAD=0
C ISHAD=2 the external radiation is set to zero for zero power output
C ISHAD=3 the external radiation is set to only the diffuse value, the
C         output of the other cells in the panel falls to that of the
C         shaded cell. 
  

C QPNL is the solar incident on the panel. 
c
       QPNL=EXRAD(ispmloc(ISPMNOD,2)) 

       IF(EXSHAD(ispmloc(ISPMNOD,2)).GT.1E-6)THEN
         IF(ISHAD.EQ.0.OR.ISHAD.EQ.1)THEN
           QPNL=EXRAD(ispmloc(ISPMNOD,2)) 
         ELSEIF(ISHAD.EQ.2)THEN
           QPNL=0.0
         ELSE
           QPNL=EXDIF(ispmloc(ISPMNOD,2)) 
         ENDIF
       ELSE
         QPNL=EXRAD(ispmloc(ISPMNOD,2)) 
       ENDIF

C Calculate the "diode factor".
       DF1=E*(VMP-VOC)/(K*TREF*SCELL)
       DF2=LOG((ISC-IMP)/ISC)
       DF=DF1/DF2

C Calculate the diffusion current.
       IO=(ISC/PBRCH)/(EXP(((E*VOC/SCELL)/(K*DF*TREF)))-1.)*
     &    (2**((TPNL-TREF)/EMPV))

C Calculate the light generated current. 
       IF (QPNL.LT.0.0) QPNL=1E-10
       IL=(QPNL/QREF)*(ISC/PBRCH)

       IF(ILODT.EQ.0)THEN
C Assuming that the maximum power point of the panel is tracked by
C a converter, calculate the maximum output voltage. The solution
C of the equation will require iteration. 
         VITER=0.4
         ITER=1
C Calculate the new value of VMP.
2        VMP1=(K*TPNL*DF/E)
         VMP2=((IL/IO)+1.)
         VMP3=1.+(E*VITER)/(K*TPNL*DF)
         VMP4=LOG(VMP2/VMP3)
         VMPMOD=VMP1*VMP4
         ITER=ITER+1
         IF (ABS(VITER-VMPMOD).GT.0.05.AND.ITER.LT.101) THEN
           VITER=ABS(VMPMOD)
           GOTO 2
         ELSEIF(ITER.GE.101) THEN
          WRITE(outs,*)'SMCMP1 WARNING - '
          call edisp(iuout,outs) 
          WRITE(outs,*)'Iterative solution for panel voltage failed!'
          call edisp(iuout,outs)
         ENDIF

C Calculate the panel output power.
         A1=VMPMOD*IO*(EXP((E*VMPMOD)/(K*TPNL*DF))-1.)
         A2=VMPMOD*IL
         PVPWR=(A1-A2)*SCELL*PBRCH*NPNNLS *
     &         ( 1. - fMiscLossFactor )
         IF(PVPWR.GT.0.0)THEN
           PVPWR=0.0
         ELSE
           PVPWR=-PVPWR
         ENDIF 
         VPV=VMPMOD * SCELL              !- module (vs cell) voltage
       ELSEIF(ILODT.EQ.1)THEN

C Calculate the pv panel output with a fixed resistance load.
C In this case fixV = resistance. The solution of the output
C current will require iteration.
         VALF=VALF/SCELL

         IITER=0.999*IL
         ITER=1

C Calculate the new value of I.
3        A1=(DF*K*TPNL)/(E*VALF)
         A2=(1.+(IL-IITER)/IO)
         IF(IITER.GT.IL)A2=(1.+(IITER-IL)/IO)
         IMOD=A1*LOG(A2)
         ITER=ITER+1
         IF (ABS(IITER-IMOD).GT.0.05.AND.ITER.LT.101) THEN
           IF(ITER.GT.50)THEN
             IITER=IITER+(IMOD-IITER)*0.25
           ELSE
             IITER=(IMOD+IITER)/2.  
           ENDIF 
           GOTO 3
         ELSEIF(ITER.GE.101)THEN     
           WRITE(outs,*)'SMCMP1 WARNING - '
           call edisp(iuout,outs) 
           WRITE(outs,*)'Iterative solution for panel current failed!'
           call edisp(iuout,outs)
         ENDIF  

C Check that the itrations have converged
         A1=IO*(EXP((E*IMOD*VALF)/(K*TPNL*DF))-1.)
         A2=IL

C Check the magnitudes of the diode and light generated currents 
C if Id > IL set the power output to zero.

         PVPWR=(A1-A2)**2*VALF*SCELL*PBRCH*NPNNLS*
     &         ( 1. - fMiscLossFactor )
         IF(A1.GT.A2)THEN
           PVPWR=0.0  
         ELSE
           PVPWR=PVPWR
         ENDIF 
         VPV=(PVPWR*valf)**0.5

       ELSEIF(ILODT.EQ.2)THEN

C Calculate the pv panel output with a fixed voltage type load e.g.
C a battery. In this case fixV=voltage.       
         VALF=VALF/SCELL 
         PVPWR=VALF*(IO*(EXP(E*VALF/(K*TPNL*DF))-1.)-IL)
         A1=IO*(EXP(E*VALF/(K*TPNL*DF))-1.)
         A2=IL
         PVPWR=PVPWR*SCELL*PBRCH*NPNNLS*
     &         ( 1. - fMiscLossFactor )    
         IF(PVPWR.GT.0.0)THEN
           PVPWR=0.0  
         ELSE
           PVPWR=-PVPWR
         ENDIF  
         VPV=VALF*SCELL                   !- module (vs cell) voltage               
       ENDIF



C PV Panel results output. 
       SAR=SNA(ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2)) 
       NDATSPM=2        
       DATASPM(ISPMNOD,1)=PVPWR
       STRSPM(ISPMNOD,1)='Power (W)'
       DATASPM(ISPMNOD,2)=((PVPWR/SAR)/QPNL)*100.
       STRSPM(ISPMNOD,2)='Efficiency (%)'

C Also (as a temporary results recovery measure) pass results output(s) 
C to zone based variable ZSPM1, ZSPM2 for storage in the results library
C the output is limited to ONE special material per zone!
       ZSPMF1(ISPMLOC(ISPMNOD,1))=DATASPM(ISPMNOD,1)
       ZSPMF2(ISPMLOC(ISPMNOD,1))=DATASPM(ISPMNOD,2)

C Save panel power output for load flow simulation. Also save
C the current panel voltage. ** If possible this will later be
C changed to a power converter component which can vary the values
C of P and Q generated by the panel. **
       pspm(ispmnod)=pvpwr
       vspm(ispmnod)=CMPLX(VPV,0.0)
       qspm(ispmnod)=0.0

C Save original nodal absorptions for trace purposes.
       QSOLE1=QSOLE(ispmloc(ISPMNOD,2),2)
       QTMCA1=QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3),2)

C Convert the panel power output W to W/m^2.
         PVPWRSQ= PVPWR/SNA(ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2)) 

C Alter the nodal absorption according to whether node is
C opaque(1) or transparent (0).
       IF(ispmtyp(ISPMNOD,2).gt.0) THEN
         IF(IE(ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2)).EQ.0)THEN
           QSOLE(ispmloc(ISPMNOD,2),2)=QSOLE(ispmloc(ISPMNOD,2),2)
     &     -PVPWRSQ
         ELSE
           QSOLI(ispmloc(ISPMNOD,2),2)=QSOLI(ispmloc(ISPMNOD,2),2)
     &     -PVPWRSQ
         ENDIF
       ELSE

C For a transparent layer modify middle node as well as the
C two boundary node absorptions.
          QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3),2)=
     &QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3),2)-(PVPWRSQ/2.)
          QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3)+1,2)=
     &QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3)+1,2)-(PVPWRSQ/4.)
           QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3)-1,2)=
     &QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3)-1,2)-(PVPWRSQ/4.)     
     
       ENDIF

C Trace output.
      IF(ITC.LE.0.OR.NSINC.LT.ITC.OR.ispmxist.EQ.0)goto 999
      IF(ITRACE(19).EQ.0.OR.NSINC.GT.ITCF.OR.
     &IZNTRC(ICOMP).NE.1)goto 999
       write(outs,'(a)') 'Trace output for Special materials affecting'
       call edisp(itu,outs)
       write(outs,'(a)') 'solar absorption'       
       call edisp(itu,outs)
       write(outs,'(a,a)') 'Subroutine SMCMP1 for ', spmlabel(ISPMNOD)
       call edisp(itu,outs)
       write(outs,'(a,2x,i4,2x,i4,2x,i4)') 'Affected zone, surface and
     &node',ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3)
       call edisp(itu,outs)
       write(outs,'(a,i3,i3,i3)') 'No of panels in surface',NPNNLS
       call edisp(itu,outs)
       IF(ispmtyp(ISPMNOD,2).gt.0) THEN
         write(outs,'(a,1x,f10.2)') 'Original absorption ',QSOLE1
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Modified absorption ',
     &     QSOLE(ispmloc(ISPMNOD,2),2)
       call edisp(itu,outs)
       ELSE
         write(outs,'(a,1x,f10.2)') 'Original absorption ',QTMCA1 
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Modified absorption ',QTMCA
     &(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3),2)
         call edisp(itu,outs)
       ENDIF
       write(outs,'(a,1x,f10.2)') 'Power output ', PVPWR
       call edisp(itu,outs) 
       write(outs,'(a,1x,f10.2)') 'Nodal temperature ',
     &TFC(ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3))     
       call edisp(itu,outs)

999   RETURN

      END

C ***************** SPMCMP2 ***************************************
C Special material 2 very simple solar cell model which calculates
C the electrical power output from some crystalline silicon solar
C cells based on a given efficiency.
C The required data items read from the array spmdat are:
C 1 - Nominal panel efficiency. (V)

C This status of this component is STABLE, its output has
C been verified against simple analytical tests but not
C experimental data. 

      subroutine SPMCMP2(ICOMP,ISPMNOD)

#include "building.h"

C Special materials
      COMMON/ENETSPM/PSPM(MSPMNOD),QSPM(MSPMNOD),
     &VSPM(MSPMNOD)
      COMMON/RESSPM/NDATSPM,DATASPM(MSPMNOD,3),STRSPM(MSPMNOD,3)
      COMMON/ZONSPMF/ZSPMF1(MCOM),ZSPMF2(MCOM)
      common/spmfxst/ispmxist,spflnam
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      common/spmatlbl/spmlabel(mspmnod)

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/COE32J/QTMCA(MS,MN,2)
      COMMON/COE32/QSOLI(MS,2),QSOLE(MS,2)
      COMMON/PVPNL/TPNL,QPNL,PVPWR

      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),
     &ARP(MCOM,MS),ARF(MCOM,MS)  

      COMMON/PREC1/SNA(MCOM,MS),ZOA(MCOM)
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

      COMMON/SOLEXT/EXRAD(MS),EXDIR(MS),EXDIF(MS),EXSHAD(MS)      

      COMPLEX VSPM

      REAL NEFF
      character spmlabel*16, outs*124,spflnam*72,STRSPM*16

C Nominal efficiency
      NEFF=spmdat(ISPMNOD,1)/100.

C QPNL is the solar radiation absorbed by the panel. 
      QPNL=EXRAD(ispmloc(ISPMNOD,2)) 

C Calculate the power generated by the PV (multiply by surface area).
      PVPWR=QPNL*NEFF*SNA(ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2))


C PV Panel results output.   
       NDATSPM=2    
       DATASPM(ISPMNOD,1)=PVPWR
       STRSPM(ISPMNOD,1)='Power (W)'
       DATASPM(ISPMNOD,2)=(PVPWR/QPNL)*100.
       STRSPM(ISPMNOD,2)='Efficiency (%)'

C Also (as a temporary results recovery measure) pass results output(s) 
C to zone based variable ZSPM1, ZSPM2 for storage in the results library
C the output is limited to ONE special material per zone!
       ZSPMF1(ISPMLOC(ISPMNOD,1))=DATASPM(ISPMNOD,1)
       ZSPMF2(ISPMLOC(ISPMNOD,1))=DATASPM(ISPMNOD,2)

C Save panel power output for load flow simulation. If possible
C this will later be changed to a power converter component which
C can vary the values of P and Q generated by the panel. **
       vspm(ispmnod)=CMPLX(220.,0.0)
       pspm(ispmnod)=pvpwr
       qspm(ispmnod)=0.0

C Save original nodal absorptions for trace purposes.
       QSOLE1=QSOLE(ispmloc(ISPMNOD,2),2)
       QTMCA1=QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3),2)

C Convert the panel power output W to W/m^2.
       PVPWRSQ= PVPWR/SNA(ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2)) 

C Alter the nodal absorption according to whether node is
C opaque(1) or transparent (0).
       IF(ispmtyp(ISPMNOD,2).gt.0) THEN
         IF(IE(ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2)).EQ.0)THEN
           QSOLE(ispmloc(ISPMNOD,2),2)=QSOLE(ispmloc(ISPMNOD,2),2)
     &     -PVPWRSQ
         ELSE
           QSOLI(ispmloc(ISPMNOD,2),2)=QSOLI(ispmloc(ISPMNOD,2),2)
     &     -PVPWRSQ
         ENDIF
       ELSE

C For a transparent layer modify middle node as well as the two
C boundary node absorptions.
          QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3),2)=
     &QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3),2)-(PVPWRSQ/2.)
          QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3)+1,2)=
     &QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3)+1,2)-(PVPWRSQ/4.)
           QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3)-1,2)=
     &QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3)-1,2)-(PVPWRSQ/4.)     
     
       ENDIF

C Trace output.
      IF(ITC.LE.0.OR.NSINC.LT.ITC.OR.ispmxist.EQ.0)goto 999
      IF(ITRACE(19).EQ.0.OR.NSINC.GT.ITCF.OR.
     &IZNTRC(ICOMP).NE.1)goto 999
       write(outs,'(a)') 'Trace output for Special materials affecting'
       call edisp(itu,outs)
       write(outs,'(a)') 'solar absorption'       
       call edisp(itu,outs)
       write(outs,'(a,a)') 'Subroutine SMCMP1 for ', spmlabel(ISPMNOD)
       call edisp(itu,outs)
       write(outs,'(a,2x,i4,2x,i4,2x,i4)') 'Affected zone, surface and
     &node',ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3)
       call edisp(itu,outs)
       IF(ispmtyp(ISPMNOD,2).gt.0) THEN
         write(outs,'(a,1x,f10.2)') 'Original absorption ',QSOLE1
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Modified absorption ',QSOLE
       call edisp(itu,outs)
       ELSE
         write(outs,'(a,1x,f10.2)') 'Original absorption ',QTMCA1 
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Modified absorption ',QTMCA
     &(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3),2)
         call edisp(itu,outs)
       ENDIF
       write(outs,'(a,1x,f10.2)') 'Power output ', PVPWR
       call edisp(itu,outs) 
       write(outs,'(a,1x,f10.2)') 'Nodal temperature ',
     &TFC(ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3))     
       call edisp(itu,outs)

999   RETURN

      END

C ***************** SPMCMP3 ***************************************
C Special material 3 is very simple ducted wind turbine model which calculates
C the electrical power output from one or more DWTs associated with two
C surfaces on a building - the positive pressure surface and the negative
C pressure surface.
C The required data items read from the array spmdat are:
C 1       Zone containing outlet (-)
C 2       Surface containing outlet (-)
C 3       Outlet ourface type (-)
C 4       Inlet azimuth angle (deg)
C 5       Outlet azimuth angle (deg)
C 6       Height of the turbine inlet (m)
C 7       Turbine cross sectional area (m^2)
C 8       Duct velocity coefficient (-)
C 9       Cut-in wind speed (m/s)
C 10      Number of turbines in the surface (-)
C 11      Location (1-urban,2-suburban,3-open)
C 12      Reference height (m)
C 13      Wind speed statistics (ON/OFF)
C 14      Wind speed/turbulence profile (ON/OFF)
C 15      Default turbulence intensity (-)

C The model includes a statistical manipulation of the mean wind
C velocity in order to better estimate the power output. 

C The status of this component is PROTOTYPE, use it at your own
C risk. The component coding is likely to change and the output
C of the component has only beed roughly verified. 

      subroutine SPMCMP3(ICOMP,ISPMNOD)

#include "building.h"
#include "plant.h"
#include "net_flow.h"

C Special materials
      COMMON/ENETSPM/PSPM(MSPMNOD),QSPM(MSPMNOD),
     &VSPM(MSPMNOD)
      COMMON/RESSPM/NDATSPM,DATASPM(MSPMNOD,3),STRSPM(MSPMNOD,3)
      COMMON/ZONSPMF/ZSPMF1(MCOM),ZSPMF2(MCOM)
      common/spmfxst/ispmxist,spflnam
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      common/spmatlbl/spmlabel(mspmnod)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/FILEP/IFIL
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/FFN/IFLWN,ICFFS(MPCON)
      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)
      COMMON/AFN/IAIRN,LAPROB,LAPRES,LAFRES,ICAAS(MCOM)

      COMMON/MFLCLM/DRYB,QDIF,QDNR,IRVH,WDIR,WSPD,WRED

      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

      COMPLEX VSPM
      REAL MINU,MAXU,MINV,MAXV,INC


      character spmlabel*16, outs*124,spflnam*72,STRSPM*16
      character*72 LAPROB,LAPRES,LAFRES
      logical close

C Read in the component data.
      INZONE=ispmloc(ISPMNOD,1)
      INSURF=ispmloc(ISPMNOD,2)
      INSURFT=ispmloc(ISPMNOD,3)
      IOZONE=NINT(spmdat(ISPMNOD,1))
      IOSURF=NINT(spmdat(ISPMNOD,2))
      IOSURFT=NINT(spmdat(ISPMNOD,3))
      AZIIN=spmdat(ISPMNOD,4)
      AZIOU=spmdat(ISPMNOD,5)
      HDWTI=spmdat(ISPMNOD,6)
      TAREA=spmdat(ISPMNOD,7)
      DVCOEFF=spmdat(ISPMNOD,8)
      CIWSPD=spmdat(ISPMNOD,9)
      NTURB=NINT(spmdat(ISPMNOD,10))
      ILOC=NINT(spmdat(ISPMNOD,11))
      REFH=spmdat(ISPMNOD,12)
      IWSTAT=NINT(spmdat(ISPMNOD,13))
      IPROF=NINT(spmdat(ISPMNOD,14))
      TURBI=spmdat(ISPMNOD,15)


C Set up pi
      PI=22./7.

C Get the current climate data 
      WIDIR=DF
      WISPD=VF
      DRYBT=TF

      IF(WISPD.LT.1E-3)WISPD=1E-3

C If no mass flow file then open WIND PRESSURE COEFFICIENTS file, 
C read coefficient sets and close.
      IF(IAIRN.EQ.0.AND.IFLWN.EQ.0.AND.NSINC.LT.2)THEN
        IUNIT=IFIL+1
        CALL EFOPSEQ(IUNIT,LAPRES,1,IER)

        IF(IER.GT.0)THEN
          CALL EDISP(IUOUT,
     &'Error opening pres coefficients in SPMCMP03')
          RETURN
        ENDIF
        READ(IUNIT,*,IOSTAT=ISTAT) NPRE
        DO 100 J=1,NPRE
          READ(IUNIT,*,IOSTAT=ISTAT) (FPRE(I,J),I=1,MPOS)
  100   CONTINUE
        CALL ERPFREE(IUNIT,ISTAT)
      ENDIF



C Calculate the wind velocity at the height of the wind turbine adjust the free
C stream velocity to height. This data should be available if a flow network
C is active. I f profiles or speed reduction are not specified then default
C values are used.
      IF(WRED.GT.1E-6.AND.IPROF.LT.1)THEN
        WISPD=WISPD*WRED
      ELSEIF(IPROF.GT.1)THEN

C If profiles are on then fill in the appropriate parameters
C 1 urban, 2 suburban, 3 rural.
        IF(ILOC.EQ.1)THEN

C Urban location
          PWR=0.32
          ROUGH=5.
        ELSEIF(ILOC.EQ.2)THEN

C Suburban location
          PWR=0.23
          ROUGH=0.8
        ELSE

C Rural location
          PWR=0.14
          ROUGH=0.05
        ENDIF

C Set the new wind speed and turbulent intensity from profiles. If height
C is zero set to a small value to avoid a NaN. 
        IF(HDWTI.LT.1E-6)HDWTI=1E-6
        IF(REFH.LT.1E-6)REFH=1E-6
        WISPD=WISPD*(HDWTI/REFH)**PWR
        TURBI=(ROUGH*LOG(30./REFH))/LOG(HDWTI/REFH)
      ENDIF

      

      IF(IWSTAT.GT.0)THEN

C Calculate wind velocity components
        COMPU=WISPD*SIN((WIDIR*PI)/180.)
        COMPV=WISPD*COS((WIDIR*PI)/180.)

C Calculate standard deviation, based on turbulent intensity.
        STDEV=TURBI*WISPD

C Calculate the range of wind component speeds
        MINU=COMPU-3*STDEV
        MAXU=COMPU+3*STDEV
        MINV=COMPV-3*STDEV
        MAXV=COMPV+3*STDEV


C Calculate the increment
        INC=6*STDEV/10.
        ICOUNT=1
        PWRTOT=0.



C Set up starting U values.
        VALU=MINU

C Set up trace probability counters
        TPRU=0
        TPRC=0

C Set up loop to generate power output. 
        DO 66 I=1,10
C Calculate the current U-value probability density
          PDU=(1/(STDEV*(2*PI)**0.5))*
     &EXP(-(((VALU-COMPU)**2.)/(2*STDEV**2.)))

          TPRU=TPRU+PDU*INC
          TPRV=0
C Set the starting value of V
          VALV=MINV
 
          DO 77 J=1,10

C Ditto for the V-value
            PDV=(1/(STDEV*(2*PI)**0.5))*
     &EXP(-(((VALV-COMPV)**2.)/(2*STDEV**2.)))
           TPRV=TPRV+PDV*INC

C Calculate the combined probability.
            PRC=(INC*PDU)*(INC*PDV)

C Calculate the duration of this wind speed and direction
            TSL=3600./float(ntstep)

            TDUR=PRC*TSL

C Re-calculate the new wind speed and wind direction
            WISPDM=(VALU**2.+VALV**2.)**0.5
            IF(ABS(VALV).LT.1E-6)VALV=1E-6
            WIDIRM=ATAN(VALU/VALV)*(180./PI)
            IF(VALV.GE.0)THEN
              IF(VALU.GE.0.)THEN
                WIDIRM=WIDIRM
              ELSE
                WIDIRM=360.+WIDIRM
              ENDIF
            ELSE
              IF(VALU.GE.0.)THEN
                WIDIRM=180.+WIDIRM
              ELSE
                WIDIRM=180.+WIDIRM
              ENDIF
            ENDIF


C Check that the wind is strong enough for the turbine to cut in.

            IF(WISPDM.GE.CIWSPD)THEN
C Check that the component of the wind speed incident on the turbine
C is greater than the cut in wind speed. If not set the wind speed to
C zero for zero power output.
              ANGI=WIDIRM-AZIIN
              IF(ABS(ANGI).GE.180.)ANGI=360.-ABS(ANGI)
              IF(ABS(ANGI).GE.90.)THEN
                WISPDM=0.
              ELSE
                GAMMA=ABS(ANGI)
                VPD=WISPDM*COS((GAMMA*PI)/180.)
                IF(VPD.LT.CIWSPD)THEN
                  WISPDM=0.
                ELSE
                  CONTINUE
                ENDIF
              ENDIF
            ELSE
               WISPDM=0.
            ENDIF
    
C Calculate the differential pressure coefficient
C Set wind direction index 1-16 corresponding to intervals
C with mid-points at 0, 22.5, ... , 337.5 deg, relative to
C surface azimuth (i.e. wind normal to surface = index 1)
               
            IWDR1=NINT((WIDIRM-AZIIN)/22.5)+1
            IF(IWDR1.LT.1 ) IWDR1=IWDR1+16
            IF(IWDR1.GT.16) IWDR1=IWDR1-16
            IWDR2=NINT((WIDIRM-AZIOU)/22.5)+1
            IF(IWDR2.LT.1 ) IWDR2=IWDR2+16
            IF(IWDR2.GT.16) IWDR2=IWDR2-16

C Calculate wind pressure and
C include stack pressure (based on P(height 0)=0 )
            CD1=FPRE(IWDR1,INSURFT)
            CD2=FPRE(IWDR2,IOSURFT)
            DELTA=CD1-CD2

            RO=DENSIT(1,DRYBT)

C Calculate the power output using: 
C P=(Cv/(3.3^(1/2))).rho.A.d^(2/3).V^3 
            PWR=(DVCOEFF/(3*3**(0.5)))*RO*TAREA*
     &(DELTA**(0.667))*(WISPDM**3)*NTURB
            ICOUNT=ICOUNT+1
            VALV=VALV+INC
            PWRTOT=PWRTOT+PWR*TDUR
  77      CONTINUE
          VALU=VALU+INC
  66    CONTINUE

C Write results for probability based power output
        PSPM(ISPMNOD)=PWRTOT/TSL

      ELSE       
        IF(WISPD.GE.CIWSPD)THEN

C Check that the component of the wind speed incident on the turbine
C is greater than the cut in wind speed. If not set WISPD to zero for
C zero power output. 
          ANGI=WIDIR-AZIIN
          IF(ABS(ANGI).GE.180.)ANGI=360.-ABS(ANGI)
          IF(ABS(ANGI).GE.90.)THEN
            WISPD=0.
          ELSE
            GAMMA=ABS(ANGI)
            VPD=WISPD*COS((GAMMA*PI)/180.)
            IF(VPD.LT.CIWSPD)THEN
              WISPD=0.
            ELSE
              CONTINUE
            ENDIF
          ENDIF
        ELSE
           WISPD=0.
        ENDIF
C       

C Calculate the differential pressure coefficient
C Set wind direction index 1-16 corresponding to intervals
C with mid-points at 0, 22.5, ... , 337.5 deg, relative to
C surface azimuth (i.e. wind normal to surface = index 1)
        IWDR1=NINT((WIDIR-AZIIN)/22.5)+1
        IF(IWDR1.LT.1 ) IWDR1=IWDR1+16
        IF(IWDR1.GT.16) IWDR1=IWDR1-16
        IWDR2=NINT((WIDIR-AZIOU)/22.5)+1
        IF(IWDR2.LT.1 ) IWDR2=IWDR2+16
        IF(IWDR2.GT.16) IWDR2=IWDR2-16

C Calculate wind pressure and
C include stack pressure (based on P(height 0)=0 )
        CD1=FPRE(IWDR1,INSURFT)
        CD2=FPRE(IWDR2,IOSURFT)
        DELTA=CD1-CD2

        RO=DENSIT(1,DRYBT)

C Calculate the power output using: 
C P=(Cv/(3.3^(1/2))).rho.A.d^(2/3).V^3 

        PSPM(ISPMNOD)=(DVCOEFF/(3*3**(0.5)))*RO*TAREA*
     &(DELTA**(0.667))*(WISPD**3)*NTURB
      ENDIF

C DWT results output. 
      NDATSPM=1      
      DATASPM(ISPMNOD,1)=PSPM(ISPMNOD)
      STRSPM(ISPMNOD,1)='Power (W)'

C Also (as a temporary results recovery measure) pass results output(s) 
C to zone based variable ZSPM1 for storage in the results library
C the output is limited to ONE special material per zone!
       ZSPMF1(ISPMLOC(ISPMNOD,1))=DATASPM(ISPMNOD,1)

C Trace
      IF(ITC.LE.0.OR.NSINC.LT.ITC.OR.ispmxist.EQ.0)goto 999
      IF(ITRACE(19).EQ.0.OR.NSINC.GT.ITCF.OR.
     &IZNTRC(ICOMP).NE.1)goto 999
       write(outs,'(a)') 'Trace output for ducted wind turbine special'
       call edisp(itu,outs)
       write(outs,'(a)') 'component'       
       call edisp(itu,outs)
       write(outs,'(a,a)') 'Subroutine SMCMP3 for ', spmlabel(ISPMNOD)
       call edisp(itu,outs)
       write(outs,'(a,1x,f10.2)') 'Cp1 ', CD1
       call edisp(itu,outs) 
       write(outs,'(a,1x,f10.2)') 'Cp2 ', CD2
       call edisp(itu,outs) 
       write(outs,'(a,1x,f10.2)') 'Cp1 - Cp2 ', DELTA
       call edisp(itu,outs) 
       write(outs,'(a,1x,f10.2)') 'Power output ', PSPM(ISPMNOD)
       call edisp(itu,outs) 
 999  RETURN
      END

C  ***************** SPMCMP4  
C Special component 4 is a simple model which calculates the heat
C flux recovered from a flat plate solar collector. 
C The required data items read from the array spmdat are:
C 1 - Water flow rate (l/s)
C 2 - Water inlet temperature (oC)
C 3 - No. of tubes (-)
C 4 - Tube diameter (m)
C 5 - Tube length

C The status of this component is PROTOTYPE, use it at your own
C risk. The component coding is likely to change and the output
C of the component has only been roughly verified. 

      subroutine SPMCMP4(ICOMP,ISPMNOD)

#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN

C Special materials
      COMMON/RESSPM/NDATSPM,DATASPM(MSPMNOD,3),STRSPM(MSPMNOD,3)
      COMMON/ZONSPMF/ZSPMF1(MCOM),ZSPMF2(MCOM)
      COMMON/SPVST/SPMVSTR(MSPMNOD,5)

      common/spmfxst/ispmxist,spflnam
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      common/spmatlbl/spmlabel(mspmnod)

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),
     &ARP(MCOM,MS),ARF(MCOM,MS)  

      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/COE32J/QTMCA(MS,MN,2)
      COMMON/COE32/QSOLI(MS,2),QSOLE(MS,2)
      COMMON/PVPNL/TPNL,QPNL,PVPWR

      COMMON/PREC1/SNA(MCOM,MS),ZOA(MCOM)
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      
      REAL MU
      character spmlabel*16, outs*124,spflnam*72,STRSPM*16

      PI=22./7.

C Volume flow rate
      VFR=spmdat(ISPMNOD,1)/1000.

C Inlet temperature 
      TIN=spmdat(ISPMNOD,2)

C Outlet temperature
      TOUT=SPMVSTR(ISPMNOD,1)
      IF(ABS(TOUT).LT.1E-6) TOUT=TIN

C The number of tubes
      NTB=NINT(spmdat(ISPMNOD,3))

C The tube diameter
      TDIA=spmdat(ISPMNOD,4)

C The tube length
      TLEN=spmdat(ISPMNOD,5)

C Internal surface area
      AREA=PI*TDIA*TLEN*NTB


C TCOL is the temperature of the backplate, read from
C the nodal temperature.
      TCOL=TFC(ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2),
     &ispmloc(ISPMNOD,3))
      
C Iteration
      ITER=1

 20   CONTINUE

C Calculate the average fluid temperature 
      dT1=TCOL-TIN
      dT2=TCOL-TOUT
      if(dT1.LE.0..OR.abs(dt1-dt2).lt.1E-6)THEN
C Solar panel results output
        TAVE=TOUT+TIN/2.
      else
        if(dT2.le.0.)THEN
          dT2=1E-6
        endif
        TAVE=(dT1-dT2)/LOG(dT1/dT2)
      endif
      IF(TAVE.LE.0.0)TAVE=0.0
      IF(TAVE.GE.100.) TAVE=100.

C Calculate the fluid density
      DEN=1001.1-0.082*TAVE-0.0036*TAVE**2.

C Calculate the fluid velocity through the tubes
      VEL=VFR/(NTB*PI*(TDIA**2)/4.)


C Calculate the fluid viscosity
      VISC=0.17444E-02 -.15885E-03*TAVE**0.50

C Calculate the fluid condictivity
      COND=((1.12*TAVE)+569.)/1000. 

C Specific heat
      SHT=4244.-(22.65*SQRT(TAVE))+(1.95*TAVE)
                                   
C Calculate Re
      REYNO=DEN*VEL*TDIA/VISC

C Prandtl number
      PRANDT=VISC*SHT/COND

C Calculate the heat transfer coefficient for either laminar
C or fully developed turbulent flow. 
       IF(REYNO.GT.2200.)THEN
         FFR=(0.79*ALOG(REYNO)-1.64)**(-2.)
         FR=FFR/8.
         HIN=(COND/TDIA)*(FR*REYNO*PRANDT)/
     &   (1.07 + 12.7*(FR**0.5)*((PRANDT**0.667)-1.0))  
       ELSE
         HIN=COND*4.4/TDIA

       ENDIF 


C Calculate Tout
c       TOUTi=HIN*AREA*(TCOL-TAVE)/(DEN*VFR*SHT) + TIN
       RT=(DEN*VFR*SHT)/HIN*AREA
       TOUTi=2.*(TCOL-RT*(TOUT-TIN))-TIN

       IF(ABS(TOUTi-TOUT).GT.0.01)THEN
         IF(ITER.LT.101)THEN
           ITER=ITER+1
           TOUT=(TOUTi+TOUT)/2.
           GOTO 20
         ELSE
           CALL EDISP(IUOUT,'ERROR:SPMCMP04 iteration limit exceeded')
         ENDIF
       ENDIF
       
C Store outlet temperature
       SPMVSTR(ISPMNOD,1)=TOUT
       
C Calculate the heat flux 
       QTHERM=VFR*DEN*SHT*(TOUTi-TIN)
       if(QTHERM.lt.1E-6)QTHERM=0.0



C Solar panel results output
       NDATSPM=1
       DATASPM(ISPMNOD,1)=QTHERM
       STRSPM(ISPMNOD,1)='Heat recovery(W)'
       DATASPM(ISPMNOD,2)=Touti
       STRSPM(ISPMNOD,2)='Outlet temp (C)'
C Also (as a temporary results recovery measure) pass results output(s) 
C to zone based variable ZSPM1,ZSPM2 for storage in the results library
C the output is limited to ONE special material per zone!
       ZSPMF1(ISPMLOC(ISPMNOD,1))=DATASPM(ISPMNOD,1)
       ZSPMF2(ISPMLOC(ISPMNOD,1))=DATASPM(ISPMNOD,2)


C Save original nodal absorptions for trace purposes.
       QSOLE1=QSOLE(ispmloc(ISPMNOD,2),2)
       QSOLI1=QSOLI(ispmloc(ISPMNOD,2),2)
       QTMCA1=QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3),2)

C Convert the panel power output W to W/m^2.
         QTHERMQF=QTHERM/SNA(ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2)) 

         QTHERMQP=SPMVSTR(ISPMNOD,2)

C Store the thermal power output
         SPMVSTR(ISPMNOD,2)=QTHERMQF

C Modify thermal output to be a mixture of future and present thermal
C output
         QTHERMQ=0.5*(QTHERMQF+QTHERMQP)
         

C Alter the nodal absorption according to whether node is
C opaque(1) or transparent (0), internal surface or external 
C surface (IE=0).
       IF(ispmtyp(ISPMNOD,2).gt.0) THEN
         IF(IE(ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2)).EQ.0)THEN
           QSOLE(ispmloc(ISPMNOD,2),2)=QSOLE(ispmloc(ISPMNOD,2),2)
     &     -QTHERMQ
         ELSE
           QSOLI(ispmloc(ISPMNOD,2),2)=QSOLI(ispmloc(ISPMNOD,2),2)
     &     -QTHERMQ
         ENDIF
       ELSE

C For a transparent layer modify middle node as well as the
C two boundary node absorptions.
          QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3),2)=
     &QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3),2)-(QTHERMQ/2.)
          QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3)+1,2)=
     &QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3)+1,2)-(QTHERMQ/4.)
           QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3)-1,2)=
     &QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3)-1,2)-(QTHERMQ/4.)     
     
       ENDIF

C Trace output.
c      IF(ITC.LE.0.OR.NSINC.LT.ITC.OR.ispmxist.EQ.0)goto 999
c      IF(ITRACE(19).EQ.0.OR.NSINC.GT.ITCF.OR.
c     &IZNTRC(ICOMP).NE.1)goto 999
       write(outs,'(a)') 'Trace output for Special materials affecting'
       call edisp(itu,outs)
       write(outs,'(a)') 'solar absorption'       
       call edisp(itu,outs)
       write(outs,'(a,a)') 'Subroutine SMCMP4 for ', spmlabel(ISPMNOD)
       call edisp(itu,outs)
       write(outs,'(a,2x,i4,2x,i4,2x,i4)') 'Affected zone, surface and
     &node',ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3)
       call edisp(itu,outs)
       IF(ispmtyp(ISPMNOD,2).gt.0) THEN
         write(outs,'(a,1x,f10.2)') 'Original absorption ',QSOLE1
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Modified absorption ',
     &QSOLE(ispmloc(ISPMNOD,2),2)
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Original absorption ',QSOLI1
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Modified absorption ',
     &QSOLI(ispmloc(ISPMNOD,2),2)
       call edisp(itu,outs)
       ELSE
         write(outs,'(a,1x,f10.2)') 'Original absorption ',QTMCA1 
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Modified absorption ',QTMCA
     &(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3),2)
         call edisp(itu,outs)
       ENDIF
       write(outs,'(a,1x,f10.2)') 'Thermal output ', QTHERM
       call edisp(itu,outs) 
       write(outs,'(a,1x,f10.2)') 'Nodal temperature ',
     &TFC(ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3))     
       call edisp(itu,outs)

999   RETURN

      END


C  ***************** SPMCMP51
C Subroutine SPMCMP51 calculates the change in transmission and 
C absorption due to the changing tint of a thermochromic 
C glass. The model assumes a linear variation of transmission
C in the working temperature range;  Tmin < T > Tmax.
C Outside the working temperature range the material is either
C at maximum tint or clear. Transmission of the material can therefore
C be regarded as a discontinuous function of Tnode.
C
C Data requirements:
C 1. Temperature at minimum transmission Tmint (DegC).
C 2. Temperature at maximum transmission Tmaxt (DegC).
C 3. Minimum transmission as a % of max transmission (%).
C 4. Layer number in TMC.

C It is assumed that while transmission and absorption vary with
C temperature the reflectivity of each element remains constant.

C The status of this component is PROTOTYPE, use it at your own
C risk. The component coding is likely to change and the output
C of the component has only been roughly verified. 

C The status of this component is STABLE, however the output
C of the component has only been roughly verified. 

      subroutine SPMCMP51(ICOMP,ISPMNOD)

#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN

      common/spmfxst/ispmxist,spflnam
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      common/spmatlbl/spmlabel(mspmnod)

      COMMON/RESSPM/NDATSPM,DATASPM(MSPMNOD,3),STRSPM(MSPMNOD,3)
      COMMON/ZONSPMF/ZSPMF1(MCOM),ZSPMF2(MCOM)
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

C TMC and surface data data.
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)

C Stored TMC information.
      common/tmcstor/tran0(mspmnod,5),abs0(mspmnod,MS,5),istore

      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)

      character spmlabel*16,outs*124,spflnam*72,STRSPM*16
      small=1.0E-5

C Temperature at maximum transmission.
      tmaxt=spmdat(ispmnod,1)

C Temperature at minimum transmission.
      tmint=spmdat(ispmnod,2)

C Check that user has not set tmint<=tmaxt.
      if(tmint.lt.tmaxt) goto 999

C Min transmission as a % of transmission at clear state.
      perc=spmdat(ispmnod,3)

C Layer in which thermochromic material is situated.
      ilyr=int(spmdat(ispmnod,4))

C No of layers in the TMC material.
      nlyr=NELTS(icomp,ispmloc(ispmnod,2))

C Put the properties of the node in question into local variables.
      isur=ispmloc(ispmnod,2)
      inod=ispmloc(ispmnod,3)     
      tnod=tfc(icomp,isur,inod)

C Store the clear state optical properties.
      if(istore.eq.0)then

C Store original optical properties for the affected layer(s)?.
C ALL layers after the tinted layer are affected !!

C Save transmission.
      do  5 iang0=1,5 
          tran0(ispmnod,iang0)=TMCT(icomp,ITMCFL(icomp,isur),iang0)
   5  continue

      do  10 layer=1,nlyr
        do  20 iang=1,5

C Save layer absorptions.
          abs0(ispmnod,layer,iang)=TMCA(icomp,ITMCFL(icomp,isur),layer,
     &iang)
  20    continue
  10  continue
        istore=1
      endif

C If T<Tmaxt then the system is unaltered. 
      if(tnod.lt.tmaxt)then
        tint=1.0

C If T>Tmaxt and T<Tmint 
C then alter system transmission and layer absorption.
      elseif(tnod.gt.tmaxt.and.tnod.lt.tmint) then

C Calculate the transmission percentage at the current nodal
C temperature.

C If user has set Tmint=Tmaxt:
        diff = abs(tmaxt-tmint)
        if (diff.le.small) then
          tint=(100.+perc)/200. 
          goto 33
        endif          

        tint=((tnod-tmint)/(tmaxt-tmint))*(100.-perc) + perc
        tint=tint/100.

 33     continue   

C T > Tmint
      elseif(tnod.ge.tmint) then
        tint=perc/100.
      endif

C Glazing results output
       NDATSPM=1
       DATASPM(ISPMNOD,1)=TINT
       STRSPM(ISPMNOD,1)='% Transmission'

C Also (as a temporary results recovery measure) pass results output(s) 
C to zone based variable ZSPM1, for storage in the results library
C the output is limited to ONE special material per zone!
       ZSPMF1(ISPMLOC(ISPMNOD,1))=DATASPM(ISPMNOD,1)


C Alter the values of transmission for the affected layer by
C the TINT value.
C NB that the absorption in all the other layers will have to
C be altered as the incident radiation falling on them will
C be reduced!
      do 30 iang2=1,5
        TMCT(icomp,ITMCFL(icomp,isur),iang2)=
     &  tran0(ispmnod,iang2)*tint

C Calculate the change in transmission and add on to layer absorption
C Check this is OK with Paul!!!!
        TMCA(icomp,ITMCFL(icomp,isur),ilyr,iang2)=
     &  tran0(ispmnod,iang2)*(1.- tint)+ abs0(ispmnod,ilyr,iang2)

  30  continue

C Reduce relative absorption for all layers after the tinted layer.
      if(ilyr.lt.nlyr) then
        do 40 layer2=ilyr+1,nlyr
          do 50 iang3=1,5
             TMCA(icomp,ITMCFL(icomp,isur),layer2,iang3)=
     &  abs0(ispmnod,layer2,iang3)*tint
  50      continue
  40    continue 
      endif        

C Write output to fort file.
       write(55,'(a,F7.4)') 'Nodal temperature ', tnod
       write(55,'(a,F7.4)') 'Max tr temperature ', tmaxt
       write(55,'(a,F7.4)') 'Min tr temperature ', tmint
       write(55,'(a,F7.4)') 'Tint ',tint
       write(55,'(a)') ' '
       write(55,'(a)') 'Original transmission and absorptions. '       
       write(55,'(5(2X,F7.4))') (tran0(ispmnod,itdat), itdat=1,5)
       write(55,'(a)') ' '
       do 60 i=1,nlyr
         write(55,'(5(2X,F7.4))') (abs0(ispmnod,i,itdat2), itdat2=1,5)
  60   continue 
       write(55,'(a)') ' '
       write(55,'(a)') 'Altered transmission and absorptions. '
       write(55,'(5(2X,F7.4))') (TMCT(icomp,ITMCFL(icomp,isur),itdat3)
     & ,itdat3=1,5)
       write(55,'(a)') ' '
       do 70 i2=1,nlyr 
         write(55,'(5(2X,F7.4))')
     &   (TMCA(icomp,ITMCFL(icomp,isur),i2,itdat4),itdat4=1,5) 
  70   continue 
       write(55,'(a)') ' ' 
         
C Trace.
      IF(ITC.LE.0.OR.NSINC.LT.ITC.OR.ispmxist.EQ.0)goto 1000
      IF(ITRACE(19).EQ.0.OR.NSINC.GT.ITCF.OR.
     &IZNTRC(ICOMP).NE.1)goto 1000      
       write(outs,'(a)') 'Trace output for thermochromic material '
       call edisp(itu,outs)
       write(outs,'(a)') 'affecting solar absorption.'       
       call edisp(itu,outs)
       write(outs,'(a,a)') 'Subroutine SMCMP51 for ', spmlabel(ISPMNOD)
       call edisp(itu,outs)
       write(outs,'(a,2x,i4,2x,i4,2x,i4)') 'Affected zone, surface and
     &node',ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3)
       call edisp(itu,outs)
       write(outs,'(a)') ' '
       call edisp(itu,outs)
       write(outs,'(a)') 'Original transmission and absorptions. '
       call edisp(itu,outs)
       write(outs,'(5(2X,F7.4))') (tran0(ispmnod,idat), idat=1,5)
       call edisp(itu,outs)
       write(outs,'(5(2X,F7.4))') (abs0(ispmnod,ilyr,idat), idat2=1,5)
       call edisp(itu,outs)
       write(outs,'(a)') ' ' 
       call edisp(itu,outs)       
       write(outs,'(a)') 'Altered transmission and absorptions. '
       call edisp(itu,outs)
       write(outs,'(5(2X,F7.4))') (TMCT(icomp,ITMCFL(icomp,isur),idat3)
     & ,idat3=1,5)
       call edisp(itu,outs)
       write(outs,'(5(2X,F7.4))')
     & (TMCA(icomp,ITMCFL(icomp,isur),ilyr,idat4),idat4=1,5)
       call edisp(itu,outs)
1000  RETURN

 999  write(outs,'(a)')' Error SPMCMP51 the temp at min transmission'
      call edisp(iuout,outs)
      write(outs,'(a)')' is a lower value than the temp at maximum'
      call edisp(iuout,outs)
      write(outs,'(a)')' transmission ! - this is not allowed.'
      call edisp(iuout,outs) 
      STOP

      end

C  ***************** SPMCMP52
C Subroutine SPMCMP52 the evaporation from a saturated surface.
C The evaporation into the adjacent space and evaporative heat
C loss for the surface are calculated.
C The functions used in the calculation are as follows: 
C
C Evaporation (kg) - from the Lewis relationship: 
C Ev=hc.Asurf.(w surf - w air)/cp (kg/s).
C
C w surf is the saturated moisture content at t surf (kg/kg da)
C w air is the moisture content of the air in the zone (kg/kg da)
C hc is the surface convective heat transfer coefficient (W/m^2K))
C Asurf is the surface area (m^2)
C cp is the specific heat capacity of the air (J/kgK)
C

C The status of this component is PROTOTYPE, use it at your own
C risk. The component coding is likely to change and the output
C of the component has only been roughly verified. 

      SUBROUTINE SPMCMP52(ICOMP,ISPMNOD)
# include "building.h"

      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      common/spmfxst/ispmxist,spflnam
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      common/spmatlbl/spmlabel(mspmnod)   

      COMMON/RESSPM/NDATSPM,DATASPM(MSPMNOD,3),STRSPM(MSPMNOD,3)
      COMMON/ZONSPMF/ZSPMF1(MCOM),ZSPMF2(MCOM)
C Evaporation common block for use in MZVAPC.
      common/evapsur/vapsur(mcom)
      COMMON/PREC1/SNA(MCOM,MS),ZOA(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/CONCOE/HCIP(MCOM,MS),HCIF(MCOM,MS),HCOP(MCOM,MS),
     &              HCOF(MCOM,MS)
      COMMON/FVALG/GFA(MCOM)

C Generic nodal flux common - allows a flux to be added or
C subtracted from a node. (GENFLXP updated in mzls5)
      COMMON/GENFLUX/GENFLXF(MCOM,MS,MN),GENFLXP(MCOM,MS,MN)
      COMMON/ATPRES/PATMOS

      character*16 spmlabel
      character*72 spflnam
      character*124 outs
      character STRSPM*16


C Setup variables
      icomp= ispmloc(ispmnod,1)
      isurf= ispmloc(ispmnod,2)

C Node should be the surface node. 
      inode= ispmloc(ispmnod,3)

C Recover zone-related data from the common blocks.       
      td = tfa(icomp)
      gz=  gfa(icomp)

      zrh= pcrh2(td,gz,PATMOS)
       
      asurf = sna(icomp,isurf)
      tsurf = tfs(icomp,isurf)
      alpha = hcif(icomp,isurf)

C Calculation for vapour released into the space (there may be
C more than one evaporative surface, so ev is added to the current
C value of vapsur(icomp)
       wzone=gfa(icomp)
       cp= sphtc2(td,wzone)*1000
       saturate=100.0
       wsurf=humrt1(tsurf,saturate,PATMOS,0)

       ev= alpha * asurf * (wsurf- wzone)/cp
       vapsur(icomp)=vapsur(icomp)+ev

C Calculate the heat drawn from the surface by the evaporation. 

C Enthalpy of water vapour at (approx) Tsurf
        hvap=cndwat1(tsurf)

C Approx = 2550E+03

C Enthalpy of saturated water at (approx) Tsurf
        hsurf=SHTH2O(tsurf)*1000.

C Calculate the evaporative loss for the surface (W/m^2)  
C qevap - stored in a common block.      
        GENFLXF(icomp,isurf,inode)=-Ev*(hvap-hsurf)
     &                            /sna(icomp,isurf)

C Surface results output
       NDATSPM=2
       DATASPM(ISPMNOD,1)=GENFLXF(icomp,isurf,inode)*sna(icomp,isurf)
       STRSPM(ISPMNOD,1)='Evaporation(W)'
       DATASPM(ISPMNOD,2)=Ev*1000.
       STRSPM(ISPMNOD,2)='Evaporation(g/s)'    

C Also (as a temporary results recovery measure) pass results output(s) 
C to zone based variable ZSPM1, ZSPM2 for storage in the results library
C the output is limited to ONE special material per zone!
       ZSPMF1(ISPMLOC(ISPMNOD,1))=DATASPM(ISPMNOD,1)
       ZSPMF2(ISPMLOC(ISPMNOD,1))=DATASPM(ISPMNOD,2)   

C Trace.
      IF(ITC.LE.0.OR.NSINC.LT.ITC.OR.ispmxist.EQ.0)goto 1000
      IF(ITRACE(19).EQ.0.OR.NSINC.GT.ITCF.OR.
     &IZNTRC(ICOMP).NE.1)goto 1000      
       write(outs,'(a)') 'Trace output for evaporation for a saturated'
       call edisp(itu,outs)
       write(outs,'(a)') 'surface material.'       
       call edisp(itu,outs)
       write(outs,'(a,a)') 'Subroutine SMCMP52 for ', spmlabel(ISPMNOD)
       call edisp(itu,outs)
       write(outs,'(a,2x,i4,2x,i4,2x,i4)') 'Affected zone, surface and
     &node',ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3)
       call edisp(itu,outs)
       write(outs,'(a)') ' '
       call edisp(itu,outs)
       write(outs,'(a)') 'Evaporation from the surface (kg) :'
       call edisp(itu,outs)
       write(outs,'(F7.4)') ev
       call edisp(itu,outs)
       write(outs,'(a)') ' Evaporative loss for the surface (W/m^2) : ' 
       call edisp(itu,outs) 
       write(outs,'(F10.4)') GENFLXF(icomp,isurf,inode)
       call edisp(itu,outs)   
       write(outs,'(a)') ' Evaporative loss for the surface (W) : ' 
       call edisp(itu,outs) 
       write(outs,'(F10.4)') GENFLXF(icomp,isurf,inode)*sna(icomp,isurf)
       call edisp(itu,outs)   
 1000  return
       end

C ************************* SPMCMP 53 **********************************
C        PHASE CHANGE MATERIAL - APPARENT HEAT CAPACITY METHOD
C This subroutine calculates change of phase in every layer of multi-
C layer construction defined as a special material - phase change ma-
C terial layer. The substitiution thermal properties of layer has been
C used to obtain the value of latent heat of phase change stored in
C material during melting and released during solidification.
C Phase transition is included due to introduction of the so-called
C apparent heat capacity which is highly nonlinear function of tempe-
C rature but can be approximated as a constant or linear function in
C phase change temperature range.
C Apparent heat in phase change temperature range is equal to sum of
C specific heat (SHT) and latent heat (LHT). Latent heat is a linear
C function of temperature in phase change temperature range (Ts-Tm),
C y=ax+b. Both of the mamber 'a' and 'b' is defined in spmdat.
C
C The required data items read from the array spmdat are:
C 1 - melting temperature,          degC
C 2 - solidification temperature,   degC
C 3 - conductivity in solid phase,  W/m*K
C 4 - conductivity in liquid phase, W/m*K
C 5 - specific heat,                J/kg*K
C 6 - latent heat member a; y=ax+b, J/kg*K^2
C 7 - latent heat member b; y=ax+b, J/kg*K
C *********************************************************************

C The status of this component is STABLE,  the output
C of the component has only been verified against analytical
C tests, though not experimental data. 

      subroutine SPMCMP53(ICOMP,ISPMNOD)

#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN

C Special materials
      common/spmfxst/ispmxist,spflnam
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      common/spmatlbl/spmlabel(mspmnod)

      COMMON/RESSPM/NDATSPM,DATASPM(MSPMNOD,3),STRSPM(MSPMNOD,3)
      COMMON/ZONSPMF/ZSPMF1(MCOM),ZSPMF2(MCOM)
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)

      COMMON/PREC1/SNA(MCOM,MS),ZOA(MCOM)
      COMMON/PREC13/C(MCOM,MS,MN,2),QC(MCOM,MS,MN)
      COMMON/VTHP15/VCP(MS,MN,2),QCP(MS,MN)
      COMMON/VTHP16/VCF(MS,MN,2),QCF(MS,MN)

C Phase change materials
      COMMON/PCM02/SPCM(MCOM,MS,MN),SPCMTF,SPCMTP
      COMMON/PCM03/PCMFAC(MCOM,MS,MN)

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &             NPGAP(MCOM,MS,MGP)

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      
      REAL TMELT, TSOLI, CONSOL, CONLIQ, SHTSOL, SHTLIQA, SHTLIQB,
     &     APPCON, APPSHT, PCMCON, PCMSHT, FACTP, FACTF, PCMFAC,
     &     GODZ, TFCM, SPCMP, SPCMF, SPCM, SHTLIQ, EFTHK

      INTEGER IELEM, NLOC

      character spmlabel*16,outs*124,spflnam*72,STRSPM*16
      logical close

C Obtain the relevant PCM data from the PCM database:

C Melting and solidification temperature , (degC)
      TMELT=spmdat(ISPMNOD,1)
      TSOLI=spmdat(ISPMNOD,2)

C Conductivity in solid and liquid phase, (W/m*K)
      CONSOL=spmdat(ISPMNOD,3)
      CONLIQ=spmdat(ISPMNOD,4)

C Specific SHT and latent LHT heat, (J/kg*K)
C *******************************************************************
C Two cases can be considered:
C 1 - latent heat is constant in temperature range (Ts-Tm),
C     y=ax+b (a=0, b=SHTLIQB)
C 2 - latent heat is a linear function of temperature,
C     y=ax+b (a=SHTLIQA, b=SHTLIQB)
C *******************************************************************
      SHTSOL=spmdat(ISPMNOD,5)
      SHTLIQA=spmdat(ISPMNOD,6)
      SHTLIQB=spmdat(ISPMNOD,7)

C Setup variables
      icomp= ispmloc(ispmnod,1)
      isurf= ispmloc(ispmnod,2)
      inode= ispmloc(ispmnod,3)
      ielem= nint(real(ispmloc(ispmnod,3))/2.)

C Node defined in special material databases is the middle
C node in the layer described as phase change material.
C So now the calculation is proced for the each node of
C that layer (n-1,n,n+1). NLOC is node location.
      DO 10 IPHNL=1,3

      IF (IPHNL.EQ.1) THEN
         NLOC=inode-1
         EFTHK=THK(isurf,ielem)*0.25
      ELSE IF (IPHNL.EQ.2) THEN
         NLOC=inode
         EFTHK=THK(isurf,ielem)*0.5
      ELSE IF (IPHNL.EQ.3) THEN
         NLOC=inode+1
         EFTHK=THK(isurf,ielem)*0.25
      ENDIF

C TFCM is the temperature of the PCM material node
C read from the nodal temperature TFCM=TFC (degC),
C if the node is surface node     TFCM=TFS (degC).
      IF (IPHNL.EQ.3.AND.IELEM.EQ.NELTS(icomp,isurf)) THEN
         TFCM=TFS(icomp,isurf)
      ELSE
         TFCM=TFC(icomp,isurf,NLOC)
      ENDIF

C *******************************************************************
C Check if phase change occures and calculate the new value of
C conductivity (W/m*K) and apparent heat (J/kg*K)
C as a function of node temperature TFCM according to:
C CON(MCOM,MS,ME) = CONSOL in solid phase  - when T<Tm
C                 = CONSOL+CONLIQ/2        - when Tm<T<Ts
C                 = CONLIQ in liquid phase - when T>Tm
C AHT(MCOM,MS,ME) = SHT + LHT
C SHT=SHTSOL - specific heat
C LHT=SHTLIQ - latent heat; LHT=0                      - when Ts<T<Tm
C                           LHT=SHTLIQA*T + SHTLIQB    - when Ts>T>Tm
C *******************************************************************
      IF (TFCM.LT.TMELT) THEN
          APPCON=CONSOL
          APPSHT=SHTSOL
      ELSEIF (TFCM.GT.TSOLI) THEN
          APPCON=(CONSOL+CONLIQ)/2
          APPSHT=SHTSOL
      ELSE
          APPCON=CONLIQ
          APPSHT=SHTSOL+(SHTLIQA*TFCM+SHTLIQB)
      ENDIF

C Read value of present factor con/sht from common block.
      FACTP=PCMFAC(icomp,isurf,NLOC)

C Calculate new rate of con and sht and future factor con/sht.
      PCMCON=APPCON/CONSOL
      PCMSHT=APPSHT/SHTSOL
      FACTF=PCMCON/PCMSHT

C Save value of future factor con/sht to common block.
      PCMFAC(icomp,isurf,NLOC)=FACTF

C Modify present and future value of 'C' and 'QC' coefficient
      VCP(isurf,NLOC,1)=C(icomp,isurf,NLOC,1)*FACTP
      VCP(isurf,NLOC,2)=C(icomp,isurf,NLOC,2)*FACTP
      QCP(isurf,NLOC)=QC(icomp,isurf,NLOC)*FACTP
      VCF(isurf,NLOC,1)=C(icomp,isurf,NLOC,1)*FACTF
      VCF(isurf,NLOC,2)=C(icomp,isurf,NLOC,2)*FACTF
      QCF(isurf,NLOC)=QC(icomp,isurf,NLOC)*FACTF

C Calculate the value of latent heat (J), stored in part PCM layer
C represented by each node of PCM layer.
C First the present value of heat stored read from common block.
      SPCMP=SPCM(icomp,isurf,NLOC)

C SHTLIQ maximum value of latent heat, (J/kg)
      SHTLIQ=(SHTLIQA*TSOLI+SHTLIQB)

C Now calculate the future value of heat stored.
      IF(TFCM.LT.TMELT) THEN
         SPCMF=0.0
      ELSEIF(TFCM.GT.TSOLI) THEN
         call eclose(SHTLIQA,0.00,0.001,close)
         if(close) THEN
            SPCMF=(SHTLIQ-SHTSOL)*(TSOLI-TMELT)*
     &      DEN(isurf,ielem)*SNA(icomp,isurf)*EFTHK
         else
            SPCMF=0.5*(SHTLIQ-SHTSOL)*(TSOLI-TMELT)*
     &      DEN(isurf,ielem)*SNA(icomp,isurf)*EFTHK
         endif
      ELSE
         call eclose(SHTLIQA,0.00,0.001,close)
         if(close) THEN
            SPCMF=(APPSHT-SHTSOL)*(TFCM-TMELT)*
     &      DEN(isurf,ielem)*SNA(icomp,isurf)*EFTHK
         else
            SPCMF=0.5*(APPSHT-SHTSOL)*(TFCM-TMELT)*
     &      DEN(isurf,ielem)*SNA(icomp,isurf)*EFTHK
         endif
      ENDIF

C Save future value of latent heat stored to common block.
      SPCM(icomp,isurf,NLOC)=SPCMF

10    CONTINUE

C Surface results output
       SPCMTF=SPCM(icomp,isurf,1)+SPCM(icomp,isurf,2)
     &        +SPCM(icomp,isurf,3)
       
       NDATSPM=1
       DATASPM(ISPMNOD,1)=(SPCMTP+SPCMTF)/2.      
       STRSPM(ISPMNOD,1)='Heat stored (J)'
       SPCMTP=SPCMTF
C Also (as a temporary results recovery measure) pass results output(s) 
C to zone based variable ZSPM1 for storage in the results library
C the output is limited to ONE special material per zone!
       ZSPMF1(ISPMLOC(ISPMNOD,1))=DATASPM(ISPMNOD,1)
  


C Trace output.
      IF(ITC.LE.0.OR.NSINC.LT.ITC.OR.ispmxist.EQ.0)goto 999
      IF(ITRACE(19).EQ.0.OR.NSINC.GT.ITCF.OR.
     &IZNTRC(ICOMP).NE.1)goto 999
       write(outs,'(a)') 'Trace output for Special materials affecting'
       call edisp(itu,outs)
       write(outs,'(a)') 'change of phase and energy stored'       
       call edisp(itu,outs)
       write(outs,'(a,a)') 'Subroutine SMCMP53 for ', spmlabel(ISPMNOD)
       call edisp(itu,outs)
       write(outs,'(a,2x,i4,2x,i4,2x,i4)') 
     &  'Affected zone, surface and nodes ',icomp,isurf,inode-1,inode,
     &  inode-1
       call edisp(itu,outs)

      DO 20 IPHNL=1,3
       IF (IPHNL.EQ.1) THEN
         write(outs,'(a,i3)') 'External node',NLOC
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Original temperature ',TFCM
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Future heat stored ',SPCMF
       call edisp(itu,outs)
       ELSE IF (IPHNL.EQ.2) THEN
         write(outs,'(a,i3)') 'Middle node',NLOC
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Original temperature ',TFCM
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Future heat stored ',SPCMF
         call edisp(itu,outs)
       ELSE
         write(outs,'(a,i3)') 'Internal node',NLOC
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Original temperature ',TFCM
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Future heat stored ',SPCMF
         call edisp(itu,outs)
       ENDIF
20    CONTINUE

999   RETURN
      end

C ****************** SPMCMP5  
C Created by: Maria Mottillo
C Original creation date: February 22, 2005

C Special material 5 is an one-diode equivalent PV model.

C The original ESP-r PV model (SPMCMP1 above, described in Kelly, 1998) 
C has been modified to match the WATSUN-PV model following 
C recommendations in (Thevenard, 2004). The WATSUN-PV model
C in described in (Thevenard, 2004).

C This subroutine uses some of the same code and terminology as SPMCMP1
C created by Nick Kelly. The major differences are in the method used to
C calculate the output power. It should be noted that this model, unlike
C the SPMCMP1 model, does not allow for PV modules with a fixed resistance
C load.

C References:
C Kelly, N. (1998), Towards a Design Environment for Building-
C Integrated Energy Systems: The Integration of Electrical
C Power Flow Modelling with Building Simulation, PhD Dissertation,
C University of Strathclyde, Glasgow, UK.
C
C Thevenard, D. (2004) 'Literature Review and Source Code Review
C of ESP-r's Exisiting Photovoltaic (PV) Models - Final Report',
C NRCan Internal Report 23229-049028/001/SQ.

C INPUT DATA
C The required data items read from the array spmdat are:
C 1 - Open circuit voltage at reference conditions for the module (V).
C 2 - Short circuit current at reference conditions for the module (I).
C 3 - Voltage at maximum power point at reference conditions for the module(V).
C 4 - Current at maximum power point at reference conditions for the module (I).
C 5 - Reference insolation. (W/m2)
C 6 - Reference temperature. (K)
C 7 - Empirical coefficient (alpha) of short circuit current (/K)
C 8 - Empirical coefficient (sigma) of open circuit voltage (/K)
C 9 - Empirical coefficient (beta) of open curcuit voltage (-)
C 10 - Number of series connected cells (not modules) (-)
C 11 - Number of parallel connected branches. (-)
C 12 - Number of modules in surface. (-)
C 13- Load type (0-maximum power point 1-fixed voltage)
C 14- Fixed Voltage (if load type = 1)
C 15- Shading effects (0-ignore 1-proportional power loss 2-total power loss
C                      3-direct rad. power loss)

C   Output data is stored in the common block RESSPM
C   with: DATASPM(*,5) - up to 5 special component results data 
C   items, STRSPM(*,5) - descriptive string for the data item. 

      subroutine SPMCMP5(ICOMP,ISPMNOD)
      implicit none

#include "building.h"

C----------------------------------------------------------------------------
C Passed variables
C----------------------------------------------------------------------------
      integer icomp          
      integer ispmnod        !- special material node index

C ESP-r COMMONs
      COMMON/OUTIN/IUOUT,IUIN
      INTEGER iuout          !- write unit number
      INTEGER iuin           !- read unit number

C Special materials
      COMMON/ENETSPM/PSPM(MSPMNOD),QSPM(MSPMNOD),VSPM(MSPMNOD)
      real pspm              !- real power of special material
      real qspm              !- reactive power load/generation of special material
      complex vspm           !- voltage of special material node

      COMMON/RESSPM/NDATSPM,DATASPM(MSPMNOD,3),STRSPM(MSPMNOD,3)
      integer   ndatspm      !- number of data items for special material
      real      dataspm      !- special component results data
      character strspm*16    !- descriptive string for the data item

      COMMON/ZONSPMF/ZSPMF1(MCOM),ZSPMF2(MCOM)
      real zspmf1
      real zspmf2

      common/spmfxst/ispmxist,spflnam
      integer   ispmxist
      character spflnam*72

      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      integer nspmnod,ispmloc,ispmtyp,nnodat
      real    spmdat

      common/spmatlbl/spmlabel(mspmnod)
      character spmlabel*16

      COMMON/TC/ITC,ICNT
      integer itc            !- trace output index
      integer icnt

      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      integer itcf           !- building side increment to end trace output
      integer itrace         !- trace call index for subroutine
      integer izntrc         !- zone trace index
      integer itu            !- trace output unit number

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      integer ihrp           !- hour number of present timestep
      integer ihrf           !- hour number of future timestep
      integer idyp           !- day number of present timestep
      integer idyf           !- day number of future timestep
      integer idwp           !- day of the week of present day
      integer idwf           !- day of the week of future day
      integer nsinc          !- number of building side time increments since start of simulation
      integer its            !- current building time-step within current hour

      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      real    tfc            !- future time-row value of construction node temperature
      real    qfc            !- future time-row value of energy injected at construction node

      COMMON/COE32J/QTMCA(MS,MN,2)
      real    qtmca          !- shortwave absorption at each node of a transparent mlc

      COMMON/COE32/QSOLI(MS,2),QSOLE(MS,2)
      real    qsoli          !- solar energy absorbed by each internal opaque surface
                             !- after adjustments by window shading and taking into account
                             !- directional property of direct beam and multiple diffuse reflections 
      real    qsole          !- solar energy absorbed by each external opaque surface
                             !- after adjustment by surface shading and taking into account
                             !- solar/building geometry

      COMMON/PVPNL/TPNL,QPNL,PVPWR
      real    tpnl,qpnl,pvpwr

      COMMON/PREC1/SNA(MCOM,MS),ZOA(MCOM)
      real    sna            !- surface area (m2)
      real    zoa            !- total surface area of the bounding zone

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      integer isd1           !- start day number 
      integer ism1           !- start month number
      integer isd2           !- finish day number
      integer ism2           !- finish month number
      integer isds           !- start year day number
      integer isdf           !- finish year day number
      integer ntstep         !- number of building timesteps within each hour

      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),
     &ARP(MCOM,MS),ARF(MCOM,MS)  
      integer ie             !- surface defining index
      real    atp            !- present time adjacent temperature
      real    atf            !- future time adjacent temperature
      real    arp            !- present time adjacent incident radiation
      real    arf            !- future time adjacent incident radiation

      COMMON/SOLEXT/EXRAD(MS),EXDIR(MS),EXDIF(MS),EXSHAD(MS)
      real    exrad          !- total solar radiation incident on surface per unit area (includes shading effects)
      real    exdir          !- direct solar radiation incident on surface per unit area (includes shading effects)
      real    exdif          !- diffuse solar radiation incident on surface per unit area (shading effect
                             !- does not apply on diffuse component)
      real    exshad         !- shaded portion expressed as a factor b/w 0 and 1 of surface

C----------------------------------------------------------------------------
C Local variables
C----------------------------------------------------------------------------

      real    fVocRef       !- open circuit voltage at reference conditions for module (V) 
      real    fIscRef       !- short circuit current at reference conditions for module (A)

      real    fVoc          !- open circuit voltage (V) for cell
      real    fIsc          !- short circuit current (A) for cell

      real    fVmpRef       !- voltage at maximum power point at reference conditions for module (V) 
      real    fImpRef       !- current at maximum power point at reference conditions for module (A) 

      real    fVmp          !- voltage at maximum power point (V) for cell
      real    fImp          !- current at maximum power point (A) for cell

      real    fQref         !- reference insolation (W/m2)
      real    fTref         !- reference temperature (K)

      integer iSCell        !- number of cells connected in series (-)
      integer iNPBrch       !- number of branches connected in parallel (-)

      integer iNPnnls       !- number of modules in surface (-)

      real    fAlpha        !- empirical coefficient for calculation of short circuit current (/K)
      real    fSigma        !- empirical coefficient for calculation of open circuit voltage (/K)
      real    fBeta         !- empirical coefficient for calculation of open circuit voltage (/K)

      integer iLodT         !- load type for the panel: 0 - maximum power point tracking (default)
                            !-                          1 - fixed voltage


      integer iMaxPPTrack, iFixedVolt        !- named constants
      parameter( iMaxPPTrack = 0, iFixedVolt  = 1 )

      real    fValf         !- fixed voltage (V) if load type = 1 for the entire module or
                            !- fixed resistance (ohms) if load type = 2

      integer iShad         !- index of shading treatment: 0 - ignored (default)  
                            !-                             1 - proportional power loss
                            !-                             2 - total power loss
                            !-                             3 - power output at shaded insolation level
      integer iIgnore, iProportion, iTotalPLoss, iPwrShd    !- named constants
      parameter( iIgnore = 0, iProportion = 1, iTotalPLoss = 2,
     &           iPwrShd = 3 )

      real    fE            !- physical constant, electrical charge
      parameter( fE = 1.60E-19 )   !- C (J)

      real    fK            !- physical constant, Boltzmann constant
      parameter( fK = 1.38E-23 )   !- J/K

      real    fQPnl         !- solar incident on the module (W/m2)

      real    fTPnl         !- temperature of solar module (K)

      real    fLambda       !- empirical coefficient used to calculate current (/V)

      real    fRatioIoIsc   !- ratio of diode reverse saturation current to short-circuit current (-)
      parameter( fRatioIoIsc =  1.0E-09 )           !- model assumption

      real    fRseries      !- internal series resistance (ohms)

      real    fPVPower      !- power generated by the PV modules (W)

      real    fPVPwrPerArea !- PV generated power per unit area (W/m2)

      real    fPVVoltage    !- voltage of PV array (V)

      real    fPVCurrent    !- current passing through PV array (A)

      real    fIcell        !- cell current (A)
      real    fVcell        !- cell voltage (V)

      real    fSurfArea     !- surface area of PV module
  
      real    fIBound1      !- boundary value required for iterative solution of I (A)
      real    fIBound2      !- boundary value required for iterative solution of I (A)
      real    F, FMID       !- required for iterative solution of I (f(I) = 0)
      real    fDeltaI       !- change in I, variable used in iterative solution (A)
      real    fIMID         !- midpoint value used in iterative solution of I (A)
      real    j             !- counter
      integer maxit         !- maximum number of iterations allowed in iterative solution of I (-)
      parameter( maxit = 100 )
      integer iter          !- iteration counter for iterative solution of I (-)
      logical bfound        !- boolean used in iterative solution of I

      character outs*124    !- used for user message outputs

      logical close         !- boolean used to compare two values

      real    QSOLE1        !- original value of solar absorption at external surface
      real    QTMCA1        !- original shortwave absorption at transparent surface

      real    fMiscLossFactor  !- miscellaneous loss factor used to derate the power
                               !- generated by the PV module (-)

C Obtain the relevant panel data from the data base
C Component manufacturers data.

C Open circuit voltage at reference conditions (for module).
      fVocRef = spmdat(ISPMNOD,1)

C Short circuit current at reference conditions (for module).
      fIscRef = spmdat(ISPMNOD,2)

C Voltage at maximum power point at reference conditions (for module).
      fVmpRef = spmdat(ISPMNOD,3)

C Current at maximum power point at reference conditions (for module).
      fImpRef = spmdat(ISPMNOD,4)

C Reference insolation and temp.
      fQref = spmdat(ISPMNOD,5)
      fTref = spmdat(ISPMNOD,6)

C Empirical coefficient used in calculation of short circuit current.
      fAlpha = spmdat(ISPMNOD,7)

C Empirical coefficients used in calculation of open circuit voltage.
      fSigma = spmdat(ISPMNOD,8)
      fBeta  = spmdat(ISPMNOD,9)

C Number of series connected CELL (NOT modules).
      iSCell = spmdat(ISPMNOD,10)

C Number of parallel connected branches.
      iNPBrch = spmdat(ISPMNOD,11)

C Number of modules in surface.
      iNPnnls = NINT(spmdat(ISPMNOD,12))

C Load type for the panel (0-maximum power point tracking (default) 1 - fixed voltage).
      iLodT = NINT(spmdat(ISPMNOD,13))

C Fixed voltage (volts) for the entire module (used if iLodT = 1).
      fValf = spmdat(ISPMNOD,14)

C Shading treatment (0-ignored (default), 1-proportional power loss, 
C 2-total power loss, 3-power output at shaded insolation level
      iShad = NINT(spmdat(ISPMNOD,15))

C Miscellaneous loss factor. This factor derates the PV power 
C produced by the panel. Losses may be attributed to uncertainty
C in the manufacturer's rating, ageing, mismatch, soil and dirt, 
C snow, blocking diodes and wiring. 
      fMiscLossFactor = spmdat(ISPMNOD,16) 

C TPNL is the temperature of the PV panel, read from
C the nodal temperature.
      fTPnl = TFC(ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2),
     &ispmloc(ISPMNOD,3))+273.0                                   !- K

C Depending on whether the  panel is shaded and the treatment of the 
C shaded panel (specified with iShad) determine the external incident solar radiation. 
C ISHAD=0 (iIgnore) :     the radiation uses the current treatment, output is proportional
C                         to the % of the panel which is shaded (included for backwards compatibility)
C ISHAD=1 (iProportion) : same as iSHAD=0
C ISHAD=2 (iTotalPLoss) : the external radiation is set to zero for zero power output
C ISHAD=3 (iPwrShd) :     the external radiation is set to only the diffuse value, the
C                         output of the other cells in the panel falls to that of the
C                         shaded cell. 

C NOTE: EXRAD variables takes into account shading of direct beam component on surface, therefore
C treatment if iShad = 0 same as if iShad = 1.
  
C fQPnl is the solar incident on the panel. 
       fQPnl = EXRAD(ispmloc(ISPMNOD,2)) 

       IF( EXSHAD(ispmloc(ISPMNOD,2)) .GT. 1E-6 ) THEN          !- if there is external shading
         IF( iShad.EQ.iIgnore .OR. iShad.EQ.iProportion ) THEN
           fQPnl = EXRAD(ispmloc(ISPMNOD,2)) 
         ELSEIF ( iShad .EQ. iTotalPLoss ) THEN
           fQPnl = 0.0
         ELSE
           fQPnl = EXDIF(ispmloc(ISPMNOD,2)) 
         ENDIF
       ELSE                                                     !- no external shading
         fQPnl = EXRAD(ispmloc(ISPMNOD,2)) 
       ENDIF

C Calculate the short circuit current (for cell)

       fIsc = fIscRef / iNPBrch  * ( fQPnl / fQref )  *
     &        ( 1. + fAlpha * ( fTPnl - fTref ) )

C Calculate the open circuit voltage (for cell)
       call eclose( fQPnl, 0.0, 0.001, close )

       if ( .not. close ) then          !- avoid ln(0) error
         fVoc = fVocRef / iScell * ( 1. - fSigma * (fTPnl - fTref)) *
     &          MAX( 0., (1. + fBeta*log(fQPnl/fQref)) )
       else
         fVoc = 0.
       endif

       IF ( iLodT .EQ. iMaxPPTrack ) THEN

C Assuming that the maximum power point of the module is tracked by
C a converter, calculate the maximum output voltage.

C Note: The WATSUN-PV model assumes that the maximum power point voltage has the same
C dependancy on cell temperature and incident irradiance as the open circuit voltage.

          fVmp = fVmpRef / iScell *                          !- this value is for the cell
     &           ( 1. - fSigma * ( fTPnl - fTref ) ) *
     &           MAX( 0., (1. + fBeta*log(fQPnl/fQref)) )

C Calculate the maximum module power
         fPVPower = fImpRef * fVmpRef * fIsc * fVoc /
     &              ( fIscRef * fVocRef ) *
     &              iNPBrch * iSCell * iNPnnls
              
C Calculate the voltage of the module
         fPVVoltage = fVmp * iSCell

       ELSEIF ( iLodT .EQ. iFixedVolt ) THEN

C Calculate the pv panel output with a fixed voltage type load e.g.
C a battery.        

C Calculate the empirical coefficient used in the calculation of the current.
         fLambda = 1. / fVoc * log( 1./ fRatioIoIsc) 

C Calculate current at maximum power point
C Note: The WATSUN-PV model assumes that the maximum power point current has the same
C dependancy on cell temperature and incident irradiance as the short circuit current.
         fImp = fImpRef / iNPBrch * ( fQPnl / fQref ) *               !- this value is per cell
     &          ( 1. + fAlpha * ( fTPnl - fTref ) )

C Calculate voltage at maximum power point.
         fVmp = fVmpRef / iScell *                                    !- this value is per cell
     &          ( 1. - fSigma * ( fTPnl - fTref ) ) *
     &          MAX( 0., (1. + fBeta*log(fQPnl/fQref)) )

C Calculate the internal series resistance.
         fRseries = ( 1. / fLambda * log( 1./fRatioIoIsc * 
     &              ( 1. - fImp / fIsc ) ) - fVmp ) / fImp          

C Determine the cell voltage. fValf = fixed voltage value provided as model input.
         fVcell = fValf / iScell                

C Calculate the cell current. This is done iteratively using the bisection method
C as described in Chapter 9 of (Press et al., 1986). The cell current will be between 0
C and the short-circuit current, therefore these are the bounds for iteration.
         fIBound1 = 0.
         fIBound2 = fIsc

         FMID = fIBound2 - fIsc * ( 1. - fRatioIoIsc * exp( fLambda *
     &                                ( fVcell + fIBound2*fRseries) ) )

         F = fIBound1 - fIsc * ( 1. - fRatioIoIsc * exp( fLambda *
     &                                ( fVcell + fIBound1*fRseries) ) )

         if ( FMID .eq. 0. ) then 
             fIcell = fIBound2
         elseif ( F .eq. 0. ) then        
             fIcell = fIBound1
         endif

         if ( (F*FMID) .gt. 0. ) then
            !- error message here
         endif

         if ( F .lt. 0. ) then
            fIcell = fIBound1
            fDeltaI = fIBound2 - fIBound1
         else
            fIcell = fIBound2
            fDeltaI = fIBound1 - fIBound2
         endif

         bfound = .false.
         iter = 0
         do while ( .not. bfound ) 

C Check that maximum number of iterations is not exceeded.
            if ( iter .gt. maxit ) then

C....................error message goes here

            endif

            fDeltaI = fDeltaI * 0.5
            fIMID = fIcell + fDeltaI
            FMID = fIMID - fIsc * ( 1. - fRatioIoIsc * exp( fLambda *
     &                                 ( fVcell + fIMID*fRseries) ) )

            if ( FMID .le. 0. ) fIcell = fIMID

            if ( (ABS(fDeltaI) .lt. 0.001) .or. (FMID .eq. 0.) )then
                bfound = .true.
            endif
            iter = iter + 1
         enddo

C Current of entire module.
         fPVcurrent = fIcell * iNPBrch

C Set voltage of PV module.
         fPVVoltage = fValf                       

C Calculate power output.
         fPVPower = fPVVoltage * fPVCurrent * iNPnnls *
     &              ( 1. - fMiscLossFactor )

       ENDIF

C PV Panel results output. 
       fSurfArea = SNA(ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2))
       NDATSPM=2        
       DATASPM(ISPMNOD,1)=fPVPower
       STRSPM(ISPMNOD,1)='Power (W)'
       if ( fQPnl .ne.0. ) then                                    !- avoid divide by zero error
          DATASPM(ISPMNOD,2)=((fPVPower/fSurfArea)/fQPnl)*100.
       else
          DATASPM(ISPMNOD,2) = 0.
       endif
       STRSPM(ISPMNOD,2)='Efficiency (%)'

C Also (as a temporary results recovery measure) pass results output(s) 
C to zone based variable ZSPM1, ZSPM2 for storage in the results library
C the output is limited to ONE special material per zone!
       ZSPMF1(ISPMLOC(ISPMNOD,1))=DATASPM(ISPMNOD,1)
       ZSPMF2(ISPMLOC(ISPMNOD,1))=DATASPM(ISPMNOD,2)

C Save panel power output for load flow simulation. Also save
C the current panel voltage. ** If possible this will later be
C changed to a power converter component which can vary the values
C of P and Q generated by the panel. **
       pspm(ispmnod)=fPVPower
       vspm(ispmnod)=CMPLX(fPVVoltage,0.0)
       qspm(ispmnod)=0.0

C Save original nodal absorptions for trace purposes.
       QSOLE1=QSOLE(ispmloc(ISPMNOD,2),2)
       QTMCA1=QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3),2)

C Convert the panel power output W to W/m^2.
       fPVPwrPerArea = fPVPower/fSurfArea 

C Alter the nodal absorption according to whether node is
C opaque(1) or transparent (0).
       IF(ispmtyp(ISPMNOD,2).gt.0) THEN
         IF(IE(ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2)).EQ.0)THEN
           QSOLE(ispmloc(ISPMNOD,2),2)=QSOLE(ispmloc(ISPMNOD,2),2)
     &     -fPVPwrPerArea
         ELSE
           QSOLI(ispmloc(ISPMNOD,2),2)=QSOLI(ispmloc(ISPMNOD,2),2)
     &     -fPVPwrPerArea
         ENDIF
       ELSE

C For a transparent layer modify middle node as well as the
C two boundary node absorptions.
        QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3),2)=
     &    QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3),2)-
     &    (fPVPwrPerArea/2.)
        QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3)+1,2)=
     &    QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3)+1,2)-
     &    (fPVPwrPerArea/4.)
        QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3)-1,2)=
     &    QTMCA(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3)-1,2)-
     &    (fPVPwrPerArea/4.)     
     
       ENDIF

C Trace output.
      IF(ITC.LE.0.OR.NSINC.LT.ITC.OR.ispmxist.EQ.0)goto 999
      IF(ITRACE(19).EQ.0.OR.NSINC.GT.ITCF.OR.
     &IZNTRC(ICOMP).NE.1)goto 999
       write(outs,'(a)') 'Trace output for Special materials affecting'
       call edisp(itu,outs)
       write(outs,'(a)') 'solar absorption'       
       call edisp(itu,outs)
       write(outs,'(a,a)') 'Subroutine SMCMP1 for ', spmlabel(ISPMNOD)
       call edisp(itu,outs)
       write(outs,'(a,2x,i4,2x,i4,2x,i4)') 'Affected zone, surface and 
     &node',ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3)
       call edisp(itu,outs)
       write(outs,'(a,i3,i3,i3)') 'No of modules in surface',iNPnnls
       call edisp(itu,outs)
       IF(ispmtyp(ISPMNOD,2).gt.0) THEN
         write(outs,'(a,1x,f10.2)') 'Original absorption ',QSOLE1
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Modified absorption ',QSOLE
     &(ispmloc(ISPMNOD,2),2)
       call edisp(itu,outs)
       ELSE
         write(outs,'(a,1x,f10.2)') 'Original absorption ',QTMCA1 
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Modified absorption ',QTMCA
     &(ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3),2)
         call edisp(itu,outs)
       ENDIF
       write(outs,'(a,1x,f10.2)') 'Power output ', fPVPower
       call edisp(itu,outs) 
       write(outs,'(a,1x,f10.2)') 'Nodal temperature ',
     &TFC(ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2),ispmloc(ISPMNOD,3))     
       call edisp(itu,outs)

999   RETURN

      END
