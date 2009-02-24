C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software. You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (Version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C ***************** SPMMOD *****************
C This subroutine calls the relevant subroutine for each special
C material (SM) identified in SPMINIT. SMs are components
C that are considered to be an integral part of the building fabric.
C Two types are available: renewable energy components for the
C production of heat and/or power; and active components such as
C phase change materials and thermo-, photo- and electro-chromic
C glazing. The following SMs are available.

C Renewable energy components:
C SPMCMP1  - crystalline silicon solar cell model with power output
C            determined as a function of absorbed solar flux and cell
C            temperature;
C SPMCMP2  - constant efficiency solar cell model with power output
C            determined as a function of absorbed solar flux and a
C            user-specifed efficiency;
C SPMCMP3  - ducted wind turbine model;
C SPMCMP4  - solar thermal collector model;
C SPMCMP5  - a one diode PV model.

C Active material components:
C SPMCMP51 - thermo-chromic glazing;
C SPMCMP52 - saturated surface with evaporation;
C SPMCMP53 - phase change material model 1;
C SPMCMP54 - phase change material model 2;
C SPMCMP55 - phase change material model 3;
C SPMCMP56 - phase change material model 4 (with sub-cooling).
C SPMCMP57 - hydrophob surface with condensation / evaporation;

      subroutine spmmod(icomp)
#include "building.h"

C N.B. All parameters comply with the Fortran implicit naming convention
C except where explicitly redefined.
      common/spmfxst/ispmxist,spflnam 
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &              nnodat(mspmnod),spmdat(mspmnod,mspmdat) 

      character spflnam*72

C For each node with a special material, identify the type as held 
C in ispmtyp(mspmnod,1). If the affected zone matches the current
C zone then call the relevant subroutine.
      if(ispmxist.gt.0)then
         do i=1,nspmnod
            ispmnod=i
            ispmtype=ispmtyp(i,1)
            if((ispmloc(i,1)).eq.icomp)then
               if(ispmtype.eq.1)then
                  call spmcmp1(icomp,ispmnod)
               elseif(ispmtype.eq.2)then
                  call spmcmp2(icomp,ispmnod)
               elseif(ispmtype.eq.3)then
                  call spmcmp3(icomp,ispmnod)
               elseif(ispmtype.eq.4)then
                  call spmcmp4(icomp,ispmnod)
               elseif(ispmtype.eq.5)then
                  call spmcmp5(icomp,ispmnod)
               elseif(ispmtype.eq.51)then
                  call spmcmp51(icomp,ispmnod)
               elseif(ispmtype.eq.52)then
                  call spmcmp52(icomp,ispmnod)
               elseif(ispmtype.eq.53)then
                  call spmcmp53(icomp,ispmnod)
               elseif(ispmtype.eq.54)then
                  call spmcmp54(icomp,ispmnod)
               elseif(ispmtype.eq.55)then
                  call spmcmp55(icomp,ispmnod)
               elseif(ispmtype.eq.56)then
                  call spmcmp56(icomp,ispmnod)
               elseif(ispmtype.eq.57)then
                  call spmcmp57(icomp,ispmnod)
               endif
            endif
         enddo
      endif
      return
      end
 
C ***************** SPMCMP1 *****************
C A crystalline silicon solar cell model (after Nick Kelly) with
C efficiency determined as a function of cell temperature. Defining
C data as read from spmdat:
C    1 - open circuit voltage (volts);
C    2 - short circuit current (amps);
C    3 - voltage at maximum power point;
C    4 - current at maximum power point;
C    5 - reference insolation level (W/m^2);
C    6 - reference temperature (K);
C    7 - number of series connected cells (not panels)(-);
C    8 - number of parallel connected branches (-);
C    9 - number of panels in surface (-);
C   10 - empirical value used in calculation of Io;
C   11 - load type (0-maximum power point,
C                   1-fixed resistance,
C                   2-fixed voltage);
C   12 - fixed voltage (volts) or resistance (ohms) value;
C   13 - shading effects:
C          0 - as 1, retained for backward compatibility (default);
C          1 - proportional power loss based on calculated incident
C              radiation, which includes full shading effects if
C              obstructions are defined;
C          2 - total power loss, solar flux set to zero to give zero
C              power output;
C          3 - direct radiation power loss, the incident flux is set
C              to the diffuse component only (the power output of all
C              cells falls to that of the shaded cells).
C   14 - miscellaneous loss factor, which acts to derates the power 
C        produced by the panel. Such losses may be attributed to
C        uncertainty in the manufacturer's rating, panel ageing,
C        maintenance, snow cover, blocked diodes and wiring etc. 

C Reference:
C Kelly N (1998), 'Towards a Design Environment for Building-Integrated
C Energy Systems: The Integration of Electrical Power Flow Modelling
C with Building Simulation', PhD Thesis, ESRU, University of Strathclyde,
C Glasgow, UK.

      subroutine spmcmp1(icomp,ispmnod)
#include "building.h"

C N.B. All parameters comply with the Fortran implicit naming convention
C except where explicitly redefined.
      common/outin/iuout,iuin
      common/tc/itc,icnt
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/enetspm/pspm(mspmnod),qspm(mspmnod),vspm(mspmnod)
      common/resspm/ndatspm(mspmnod),dataspm(mspmnod,MSPMRES),
     &         strspm(mspmnod,MSPMRES),unitspm(mspmnod,MSPMRES),
     &         txtspm(mspmnod,MSPMRES)
      common/zonspmf/zspmf1(mcom),zspmf2(mcom)
      common/spmfxst/ispmxist,spflnam
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &              nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      common/spmatlbl/spmlabel(mspmnod)
      common/fvalc/tfc(mcom,ms,mn),qfc(mcom)
      common/coe32/qsoli(ms,2),qsole(ms,2)
      common/coe32j/qtmca(ms,mn,2)
      common/pvpnl/tpnl,qpnl,pvpwr
      common/prec1/sna(mcom,ms),zoa(mcom)
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      common/adjc/ie(mcom,ms),atp(mcom,ms),atf(mcom,ms),
     &            arp(mcom,ms),arf(mcom,ms)  
      common/solext/exrad(ms),exdir(ms),exdif(ms),exshad(ms),
     &              exshadf(ms)      

      real isc,imp,k,io,il,imod,iiter
      character spmlabel*16,outs*124,spflnam*72,strspm*16,unitspm*16,
     &          txtspm*72
      complex vspm
      logical bNumsAreClose
      parameter (fSmall=1.0E-05)

C Open circuit voltage.
      voc=spmdat(ispmnod,1)

C Short circuit current.
      isc=spmdat(ispmnod,2)

C Voltage at maximum power point.
      vmp=spmdat(ispmnod,3)

C Current at maximum power point.
      imp=spmdat(ispmnod,4)

C Reference insolation and temperature.
      qref=spmdat(ispmnod,5)
      tref=spmdat(ispmnod,6)

C Number of series connected cells.
      scell=spmdat(ispmnod,7)

C Number of parallel connected branches.
      pbrch=spmdat(ispmnod,8)

C Number of panels in surface.
      npnnls=nint(spmdat(ispmnod,9))

C Empirical value used in calculation of diode current.
      empv=spmdat(ispmnod,10)
      if(empv.lt.1E-10)empv=10.

C Load type.
      ilodt=nint(spmdat(ispmnod,11))

C Fixed voltage or resistance as appropriate.
      valf=spmdat(ispmnod,12)

C Shading treatment.
      ishad=nint(spmdat(ispmnod,13))

C Miscellaneous loss factor, which acts to derates the power 
C produced by the PV panel. Such losses may be attributed to
C uncertainty in the manufacturer's rating, panel ageing,
C maintenance issues, snow covering, blocked diodes and wiring, etc. 
      fMiscLossFactor=spmdat(ispmnod,14) 

C Physical constants: electrical charge and Boltzman constant.
      e=1.60E-19
      k=1.38E-23

C Panel temperature assigned from the associated construction node
C temperature.
      tpnl=tfc(ispmloc(ispmnod,1),ispmloc(ispmnod,2),
     &                                      ispmloc(ispmnod,3))+273.0  

C Determine the total solar flux, qpnl, incident on the panel as a
C function of the requested treatment of shading as defined by ishad. 

C No shading case.
      qpnl=exrad(ispmloc(ispmnod,2))

C Shading case.
      if(exshad(ispmloc(ispmnod,2)).gt.1E-6)then
         if(ishad.eq.0.or.ishad.eq.1)then
            qpnl=exrad(ispmloc(ispmnod,2)) 
         elseif(ishad.eq.2)then
            qpnl=0.0
         elseif(ishad.eq.3)then
            qpnl=exdif(ispmloc(ispmnod,2)) 
         endif
      endif

C Diode factor.
      df1=e*(vmp-voc)/(k*tref*scell)
      df2=log((isc-imp)/isc)
      df=df1/df2

C Diffusion current.
      io=(isc/pbrch)/(exp(((e*voc/scell)/(k*df*tref)))-1.)*
     &                                        (2**((tpnl-tref)/empv))

C Light generated current. 
      if(qpnl.lt.0.0)qpnl=1E-10
      il=(qpnl/qref)*(isc/pbrch)

C Maximum power point tracking, use iteration to determine the maximum
C output voltage, vmpmod.
      if(ilodt.eq.0)then
         viter=0.4
         iter=1
    2    vmp1=(k*tpnl*df/e)
         vmp2=((il/io)+1.)
         vmp3=1.+(e*viter)/(k*tpnl*df)
         vmp4=log(vmp2/vmp3)
         vmpmod=vmp1*vmp4
         iter=iter+1
         if(abs(viter-vmpmod).gt.0.05.and.iter.lt.101)then
            viter=abs(vmpmod)
            goto 2
         elseif(iter.ge.101)then
            call edisp(iuout,' ') 
            write(outs,*)
     &           'SPMCMP1 fatal error: panel voltage solution failed!'
            call edisp(iuout,outs)
            stop
         endif

C Panel power output.
         a1=vmpmod*io*(exp((e*vmpmod)/(k*tpnl*df))-1.)
         a2=vmpmod*il
         pvpwr=(a1-a2)*scell*pbrch*npnnls*(1.-fMiscLossFactor)
         if(pvpwr.gt.0.0)then
            pvpwr=0.0
         else
            pvpwr=-pvpwr
         endif 
         vpv=vmpmod*scell	!- panel voltage

C Fixed resistance load, use iteration to calculate panel current.
      elseif(ilodt.eq.1)then
         valf=valf/scell
         iiter=0.999*il
         iter=1
    3    a1=(df*k*tpnl)/(e*valf)
         a2=(1.+(il-iiter)/io)
         if(iiter.gt.il)a2=(1.+(iiter-il)/io)
         imod=a1*log(a2)
         iter=iter+1
         if(abs(iiter-imod).gt.0.05.and.iter.lt.101)then
            if(iter.gt.50)then
               iiter=iiter+(imod-iiter)*0.25
            else
               iiter=(imod+iiter)/2.  
            endif 
            goto 3
         elseif(iter.ge.101)then     
           call edisp(iuout,' ') 
           write(outs,*)
     &           'SPMCMP1 fatal error: panel current solution failed!'
           call edisp(iuout,outs) 
           stop
         endif  

C Check that iteration has converged.
         a1=io*(exp((e*imod*valf)/(k*tpnl*df))-1.)
         a2=il

C Check the magnitudes of the diode and light generated currents; if
C id > il set the power output to zero.
         pvpwr=(a1-a2)**2*valf*scelL*pbrch*npnnls*(1.- fMiscLossFactor)
         if(a1.gt.a2)then
            pvpwr=0.0  
         else
            pvpwr=pvpwr
         endif 
         vpv=(pvpwr*valf)**0.5

C Fixed voltage type load (e.g. a battery), calculate panel power.       
      elseif(ilodt.eq.2)then
         valf=valf/scell 
         pvpwr=valf*(io*(exp(e*valf/(k*tpnl*df))-1.)-il)
         a1=io*(exp(e*valf/(k*tpnl*df))-1.)
         a2=il
         pvpwr=pvpwr*scell*pbrch*npnnls*(1.- fMiscLossFactor)    
         if(pvpwr.gt.0.0)then
            pvpwr=0.0  
         else
            pvpwr=-pvpwr
         endif  
         vpv=valf*scell		!- panel voltage               
      endif

C Results output. 
      sar=sna(ispmloc(ispmnod,1),ispmloc(ispmnod,2)) 
      ndatspm(ispmnod)=2        
      dataspm(ispmnod,1)=pvpwr
      strspm(ispmnod,1)='Power (W)'
      call eclose(qpnl,0.,fSmall,bNumsAreClose)
      if(bNumsAreClose)then 
         dataspm(ispmnod,2)=0.
      else
         dataspm(ispmnod,2)=((pvpwr/sar)/qpnl)*100.
      endif
      strspm(ispmnod,2)='Efficiency (%)'

C As a temporary measure, use zone-related variables, zspmf1 and zspmf2,
C for results library storage. Output is limited to one special material
C per zone.
      zspmf1(ispmloc(ispmnod,1))=dataspm(ispmnod,1)
      zspmf2(ispmloc(ispmnod,1))=dataspm(ispmnod,2)

C Save panel power output (for load flow simulation) and the current
C panel voltage. (This could later be changed to a power converter
C component that can vary the values of P and Q as generated by the
C panel.)
      pspm(ispmnod)=pvpwr
      vspm(ispmnod)=cmplx(vpv,0.0)
      qspm(ispmnod)=0.0

C Convert panel power output from W to W/m^2.
      pvpwrsq=pvpwr/sna(ispmloc(ispmnod,1),ispmloc(ispmnod,2)) 

C Save original nodal solar absorptions for trace output.
      qsole1=qsole(ispmloc(ispmnod,2),2)
      qtmca1=qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3),2)

C Alter the nodal solar absorption according to whether node is
C transparent (0) or opaque (1).
      if(ispmtyp(ispmnod,2).gt.0)then

C Opaque layer.
         if(ie(ispmloc(ispmnod,1),ispmloc(ispmnod,2)).eq.0)then
            qsole(ispmloc(ispmnod,2),2)=qsole(ispmloc(ispmnod,2),2)
     &                                                       -pvpwrsq
         else
            qsoli(ispmloc(ispmnod,2),2)=qsoli(ispmloc(ispmnod,2),2)
     &                                                       -pvpwrsq
         endif
      else

C Transparent layer, modify the solar flux absorption of the middle
C node and the two boundary nodes.
         qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3),2)=
     &    qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3),2)-(pvpwrsq/2.)
         qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3)+1,2)=
     &    qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3)+1,2)-(pvpwrsq/4.)
         qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3)-1,2)=
     &    qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3)-1,2)-(pvpwrsq/4.)     
      endif

C Trace output.
      if(itc.le.0.or.nsinc.lt.itc.or.ispmxist.eq.0)goto 999
      if(itrace(19).eq.0.or.nsinc.gt.itcf.or.izntrc(icomp).ne.1)goto999
      write(outs,'(a,a)') 'Trace output from SPMCMP1 for ',
     &                                              spmlabel(ispmnod)
      call edisp(itu,outs)
      write(outs,'(a,2x,i4,2x,i4,2x,i4)')
     &        'Affected zone, surface and node: ',ispmloc(ispmnod,1),
     &         ispmloc(ispmnod,2),ispmloc(ispmnod,3)
      call edisp(itu,outs)
      write(outs,'(a,i3,i3,i3)') 'Number of panels in surface: ',npnnls
      call edisp(itu,outs)
      if(ispmtyp(ispmnod,2).gt.0)then
         write(outs,'(a,1x,f10.2)') 'Original flux absorption: ',qsole1
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Modified flux absorption: ',
     &                                    qsole(ispmloc(ispmnod,2),2)
         call edisp(itu,outs)
      else
         write(outs,'(a,1x,f10.2)') 'Original flux absorption: ',qtmca1 
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Modified flux absorption: ',
     &                 qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3),2)
         call edisp(itu,outs)
      endif
      write(outs,'(a,1x,f10.2)') 'Panel power output: ', pvpwr
      call edisp(itu,outs) 
      write(outs,'(a,1x,f10.2)') 'Panel temperature: ',tpnl
      call edisp(itu,outs)
  999 return
      end

C ***************** SPMCMP2 *****************
C A crystalline silicon solar cell model (after Nick Kelly), with
C electrical power output based on a user-specified efficiency.
C Defining data items read from spmdat:
C    1 - nominal panel efficiency (%).

      subroutine spmcmp2(icomp,ispmnod)
#include "building.h"

C N.B. All parameters comply with the Fortran implicit naming convention
C except where explicitly redefined.
      common/tc/itc,icnt
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/enetspm/pspm(mspmnod),qspm(mspmnod),
     &               vspm(mspmnod)
      common/resspm/ndatspm(mspmnod),dataspm(mspmnod,MSPMRES),
     &         strspm(mspmnod,MSPMRES),unitspm(mspmnod,MSPMRES),
     &         txtspm(mspmnod,MSPMRES)
      common/zonspmf/zspmf1(mcom),zspmf2(mcom)
      common/spmfxst/ispmxist,spflnam
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      common/spmatlbl/spmlabel(mspmnod)
      common/fvalc/tfc(mcom,ms,mn),qfc(mcom)
      common/coe32j/qtmca(ms,mn,2)
      common/coe32/qsoli(ms,2),qsole(ms,2)
      common/pvpnl/tpnl,qpnl,pvpwr
      common/adjc/ie(mcom,ms),atp(mcom,ms),atf(mcom,ms),
     &arp(mcom,ms),arf(mcom,ms)  
      common/prec1/sna(mcom,ms),zoa(mcom)
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      common/solext/exrad(ms),exdir(ms),exdif(ms),exshad(ms),
     &                                                    exshadf(ms)

      real neff
      complex vspm
      character spmlabel*16, outs*124,spflnam*72,strspm*16,unitspm*16,
     &          txtspm*72

C Nominal efficiency.
      neff=spmdat(ispmnod,1)/100.

C Solar radiation absorbed by panel. 
      qpnl=exrad(ispmloc(ispmnod,2)) 

C Power generated by panel.
      pvpwr=qpnl*neff*sna(ispmloc(ispmnod,1),ispmloc(ispmnod,2))

C Results output.   
      ndatspm(ispmnod)=2    
      dataspm(ispmnod,1)=pvpwr
      strspm(ispmnod,1)='Power (W)'
      dataspm(ispmnod,2)=(pvpwr/qpnl)*100.
      strspm(ispmnod,2)='Efficiency (%)'

C As a temporary measure, use zone-related variables, zspmf1 and zspmf2,
C for results library storage. Output is limited to one special material
C per zone.
      zspmf1(ispmloc(ispmnod,1))=dataspm(ispmnod,1)
      zspmf2(ispmloc(ispmnod,1))=dataspm(ispmnod,2)

C Save panel power output (for load flow simulation) and the current
C panel voltage. (This could later be changed to a power converter
C component that can vary the values of P and Q as generated by the
C panel.)
      vspm(ispmnod)=cmplx(220.,0.0)
      pspm(ispmnod)=pvpwr
      qspm(ispmnod)=0.0

C Convert the panel power output from W to W/m^2.
       pvpwrsq=pvpwr/sna(ispmloc(ispmnod,1),ispmloc(ispmnod,2)) 

C Save original nodal absorptions for trace output.
      qsole1=qsole(ispmloc(ispmnod,2),2)
      qtmca1=qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3),2)

C Alter the nodal absorption according to whether node is
C transparent (0) or opaque (1).

C Opaque layer.
      if(ispmtyp(ispmnod,2).gt.0)then
         if(ie(ispmloc(ispmnod,1),ispmloc(ispmnod,2)).eq.0)then
            qsole(ispmloc(ispmnod,2),2)=qsole(ispmloc(ispmnod,2),2)
     &                                                       -pvpwrsq
         else
            qsoli(ispmloc(ispmnod,2),2)=qsoli(ispmloc(ispmnod,2),2)
     &                                                       -pvpwrsq
         endif
      else

C Transparent layer: modify the solar flux absorption of the middle
C node and the two boundary nodes.
         qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3),2)=
     &    qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3),2)-(pvpwrsq/2.)
         qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3)+1,2)=
     &    qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3)+1,2)-(pvpwrsq/4.)
         qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3)-1,2)=
     &    qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3)-1,2)-(pvpwrsq/4.)     
      endif

C Trace output.
      if(itc.le.0.or.nsinc.lt.itc.or.ispmxist.eq.0)goto 999
      if(itrace(19).eq.0.or.nsinc.gt.itcf.or.izntrc(icomp).ne.1)goto999
      write(outs,'(a,a)') 'Trace output from SPMCMP2 for ',
     &                                              spmlabel(ispmnod)
      call edisp(itu,outs)
      write(outs,'(a,2x,i4,2x,i4,2x,i4)')
     &        'Affected zone, surface and node: ',ispmloc(ispmnod,1),
     &         ispmloc(ispmnod,2),ispmloc(ispmnod,3)
      call edisp(itu,outs)
c      write(outs,'(a,i3,i3,i3)') 'Number of panels in surface: ',npnnls
c      call edisp(itu,outs)
      if(ispmtyp(ispmnod,2).gt.0)then
         write(outs,'(a,1x,f10.2)') 'Original flux absorption: ',qsole1
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Modified flux absorption: ',
     &                                    qsole(ispmloc(ispmnod,2),2)
         call edisp(itu,outs)
      else
         write(outs,'(a,1x,f10.2)') 'Original flux absorption: ',qtmca1 
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Modified flux absorption: ',
     &                 qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3),2)
         call edisp(itu,outs)
      endif
      write(outs,'(a,1x,f10.2)') 'Panel power output: ', pvpwr
      call edisp(itu,outs) 
      write(outs,'(a,1x,f10.2)') 'Panel temperature: ',tpnl
      call edisp(itu,outs)
  999 return
      end

C ***************** SPMCMP3 *****************
C A model that calculates the electrical power output from a ducted
C wind turbine (after Nick Kelly and Andy Grant). The model includes
C a statistical manipulation of the mean wind velocity in order to
C estimate the power output. Defining data as read from spmdat:
C    1 - zone containing outlet (-)
C    2 - surface containing outlet (-)
C    3 - outlet ourface type (-)
C    4 - inlet azimuth angle (degrees)
C    5 - outlet azimuth angle (degrees)
C    6 - height of turbine inlet (m)
C    7 - turbine cross sectional area (m^2)
C    8 - duct velocity coefficient (-)
C    9 - cut-in wind speed (m/s)
C   10 - number of turbines in the surface (-)
C   11 - location (1-urban, 2-suburban, 3-rural)
C   12 - reference height (m)
C   13 - wind speed statistics (on/off)
C   14 - wind speed/turbulence profile (on/off)
C   15 - default turbulence intensity (-)

      subroutine spmcmp3(icomp,ispmnod)
#include "building.h"
#include "plant.h"
#include "net_flow.h"
#include "esprdbfile.h"

C N.B. All parameters comply with the Fortran implicit naming
C convention except where explicitly redefined.
      common/outin/iuout,iuin
      common/tc/itc,icnt
      common/filep/ifil
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      common/enetspm/pspm(mspmnod),qspm(mspmnod),
     &vspm(mspmnod)
      common/resspm/ndatspm(mspmnod),dataspm(mspmnod,MSPMRES),
     &         strspm(mspmnod,MSPMRES),unitspm(mspmnod,MSPMRES),
     &         txtspm(mspmnod,MSPMRES)
      common/zonspmf/zspmf1(mcom),zspmf2(mcom)
      common/spmfxst/ispmxist,spflnam
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &              nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      common/spmatlbl/spmlabel(mspmnod)
      common/ffn/iflwn,icffs(mpcon)
      common/mflwpr/npre,fpre(mpos,mprd)
      common/afn/iairn,laprob,icaas(mcom)
      common/mflclm/dryb,qdif,qdnr,irvh,wdir,wspd,wred
      common/climi/qfp,qff,tp,tf,qdp,qdf,vp,vf,dp,df,hp,hf

      real minu,maxu,minv,maxv,inc
      complex vspm
      logical unixok

      character spmlabel*16,outs*124,spflnam*72,laprob*72,strspm*16,
     &          unitspm*16,txtspm*72,lworking*144,fs*1

C Set folder separator (fs) to \ or / as required.
      call isunix(unixok)
      if(unixok)then
        fs = char(47)
      else
        fs = char(92)
      endif

C Component data.
      inzone=ispmloc(ispmnod,1)
      insurf=ispmloc(ispmnod,2)
      insurft=ispmloc(ispmnod,3)
      iozone=nint(spmdat(ispmnod,1))
      iosurf=nint(spmdat(ispmnod,2))
      iosurft=nint(spmdat(ispmnod,3))
      aziin=spmdat(ispmnod,4)
      aziou=spmdat(ispmnod,5)
      hdwti=spmdat(ispmnod,6)
      tarea=spmdat(ispmnod,7)
      dvcoeff=spmdat(ispmnod,8)
      ciwspd=spmdat(ispmnod,9)
      nturb=nint(spmdat(ispmnod,10))
      iloc=nint(spmdat(ispmnod,11))
      refh=spmdat(ispmnod,12)
      iwstat=nint(spmdat(ispmnod,13))
      iprof=nint(spmdat(ispmnod,14))
      turbi=spmdat(ispmnod,15)

      pi=4.0*atan(1.0)

C Current weather data.
      widir=df
      drybt=tf
      wispd=vf
      if(wispd.lt.1E-3)wispd=1E-3

C If no mass flow file exists then open wind pressure coefficients 
C database, read coefficient sets and close.
      if(iairn.eq.0.and.iflwn.eq.0.and.nsinc.lt.2)then
         iunit=ifil+1
         if(ipathapres.eq.0.or.ipathapres.eq.1)then
            call efopseq(iunit,lapres,1,ier)
         elseif(ipathapres.eq.2)then
            lndbp=lnblnk(standarddbpath)
            write(lworking,'(3a)') standarddbpath(1:lndbp),fs,
     &      lapres(1:lnblnk(lapres))
            call efopseq(iunit,lworking,1,ier)
         endif
         if(ier.gt.0)then
            call edisp(iuout,
     &         'SPMCMP3 fatal error: when opening pressure coeff. db!')
            stop
         endif
         read(iunit,*,iostat=istat)npre
         do j=1,npre
            read(iunit,*,iostat=istat)(fpre(i,j),i=1,mpos)
         enddo
         call erpfree(iunit,istat)
      endif

C Calculate the wind velocity prevailing at the height of the ducted
C wind turbine. This information will be available if an air flow
C network exists. If profiles or speed reduction are not specified
C then use default values.
      if(wred.gt.1E-6.and.iprof.lt.1)then
         wispd=wispd*wred
      elseif(iprof.gt.1)then

C Urban location.
         if(iloc.eq.1)then
            pwr=0.32
            rough=5.

C Suburban location
         elseif(iloc.eq.2)then
            pwr=0.23
            rough=0.8

C Rural location
         elseif(iloc.eq.3)then
            pwr=0.14
            rough=0.05
         endif

C Set the new wind speed and turbulent intensity from profiles. If
C height is zero, set to a small value to avoid numerical problems. 
         if(hdwti.lt.1E-6)hdwti=1E-6
         if(refh.lt.1E-6)refh=1E-6
         wispd=wispd*(hdwti/refh)**pwr
         turbi=(rough*log(30./refh))/log(hdwti/refh)
      endif

      if(iwstat.gt.0)then

C Wind velocity components.
         compu=wispd*sin((widir*pi)/180.)
         compv=wispd*cos((widir*pi)/180.)

C Standard deviation, based on turbulent intensity.
         stdev=turbi*wispd

C Wind velocity ranges.
         minu=compu-3*stdev
         maxu=compu+3*stdev
         minv=compv-3*stdev
         maxv=compv+3*stdev

C Calculate the increment.
         inc=6*stdev/10.
         icount=1
         pwrtot=0.

C Set starting value of U velocity component.
         valu=minu

C Set up trace probability counters.
         tpru=0
         tprc=0

C Loop to generate power output. 
         do 66 i=1,10

C Current U velocity component probability density.
            pdu=(1/(stdev*(2*pi)**0.5))*
     &                       exp(-(((valu-compu)**2.)/(2*stdev**2.)))
            tpru=tpru+pdu*inc
            tprv=0

C Set starting value of V velocity component.
            valv=minv
 
            do 77 j=1,10

C Current V velocity component probability density.
               pdv=(1/(stdev*(2*pi)**0.5))*
     &                       exp(-(((valv-compv)**2.)/(2*stdev**2.)))
               tprv=tprv+pdv*inc

C Combined probability.
               prc=(inc*pdu)*(inc*pdv)

C Duration of current wind speed and direction.
               tsl=3600./float(ntstep)
               tdur=prc*tsl

C New wind speed and direction.
               wispdm=(valu**2.+valv**2.)**0.5
               if(abs(valv).lt.1E-6)valv=1E-6
               widirm=atan(valu/valv)*(180./pi)
               if(valv.ge.0)then
                  if(valu.ge.0.)then
                     widirm=widirm
                  else
                     widirm=360.+widirm
                  endif
               else
                  if(valu.ge.0.)then
                     widirm=180.+widirm
                  else
                     widirm=180.+widirm
                  endif
               endif

C Check that the wind is strong enough for the turbine to cut in.
               if(wispdm.ge.ciwspd)then

C Check that the component of the wind speed incident on the turbine
C is greater than the cut-in wind speed. If not, set the wind speed
C to zero for zero power output.
                  angi=widirm-aziin
                  if(abs(angi).ge.180.)angi=360.-abs(angi)
                  if(abs(angi).ge.90.)then
                     wispdm=0.
                  else
                     gamma=abs(angi)
                     vpd=wispdm*cos((gamma*pi)/180.)
                     if(vpd.lt.ciwspd)then
                        wispdm=0.
                     else
                        continue
                     endif
                  endif
               else
                  wispdm=0.
               endif
    
C Calculate the differential pressure coefficient and set the wind
C direction index (1-16 corresponds to intervals with mid-points
C at 0, 22.5, ..., 337.5 degrees relative to the surface azimuth,
C i.e. index 1 = wind normal to surface).
               iwdr1=nint((widirm-aziin)/22.5)+1
               if(iwdr1.lt.1)iwdr1=iwdr1+16
               if(iwdr1.gt.16)iwdr1=iwdr1-16
               iwdr2=nint((widirm-aziou)/22.5)+1
               if(iwdr2.lt.1)iwdr2=iwdr2+16
               if(iwdr2.gt.16)iwdr2=iwdr2-16

C Calculate wind pressure and include stack pressure based on
C P = 0 and height = 0.
               cd1=fpre(iwdr1,insurft)
               cd2=fpre(iwdr2,iosurft)
               delta=cd1-cd2

C Power output from P={Cv/[3.3^(1/2)]}.rho.A.d^(2/3).V^3 
               ro=densit(1,drybt)
               pwr=(dvcoeff/(3*3**(0.5)))*ro*tarea*
     &                             (delta**(0.667))*(wispdm**3)*nturb
               icount=icount+1
               valv=valv+inc
               pwrtot=pwrtot+pwr*tdur
   77       continue
            valu=valu+inc
   66    continue

C Write results for probability-based power output.
         pspm(ispmnod)=pwrtot/tsl

      else       
         if(wispd.ge.ciwspd)then

C Check that the component of the wind speed incident on the turbine
C is greater than the cut-in wind speed. If not, set to zero for
C zero power output. 
            angi=widir-aziin
            if(abs(angi).ge.180.)angi=360.-abs(angi)
            if(abs(angi).ge.90.)then
               wispd=0.
            else
               gamma=abs(angi)
               vpd=wispd*cos((gamma*pi)/180.)
               if(vpd.lt.ciwspd)then
                  wispd=0.
               else
                  continue
               endif
            endif
         else
            wispd=0.
         endif      

C Calculate the differential pressure coefficient and set the wind
C direction index (1-16 corresponds to intervals with mid-points
C at 0, 22.5, ..., 337.5 degrees relative to the surface azimuth
C (i.e. index 1 = wind normal to surface)       
         iwdr1=nint((widir-aziin)/22.5)+1
         if(iwdr1.lt.1)iwdr1=iwdr1+16
         if(iwdr1.gt.16)iwdr1=iwdr1-16
         iwdr2=nint((widir-aziou)/22.5)+1
         if(iwdr2.lt.1)iwdr2=iwdr2+16
         if(iwdr2.gt.16)iwdr2=iwdr2-16

C Calculate wind pressure and include stack pressure based on
C P = 0 for height=0.
         cd1=fpre(iwdr1,insurft)
         cd2=fpre(iwdr2,iosurft)
         delta=cd1-cd2

C Power output from P={Cv/[3.3^(1/2)]}.rho.A.d^(2/3).V^3 
         ro=densit(1,drybt)
         pspm(ispmnod)=(dvcoeff/(3*3**(0.5)))*ro*tarea*
     &                              (delta**(0.667))*(wispd**3)*nturb
      endif

C Results output. 
      ndatspm(ispmnod)=1      
      dataspm(ispmnod,1)=pspm(ispmnod)
      strspm(ispmnod,1)='Power (W)'

C As a temporary measure, use zone-related variable, zspmf1, for
C results library storage. Output is limited to one special material
C per zone.
       zspmf1(ispmloc(ispmnod,1))=dataspm(ispmnod,1)

C Trace output.
      if(itc.le.0.or.nsinc.lt.itc.or.ispmxist.eq.0)goto 999
      if(itrace(19).eq.0.or.nsinc.gt.itcf.or.izntrc(icomp).ne.1)goto999
      write(outs,'(a,a)') 'Trace output from SPMCMP3 for ',
     &                                              spmlabel(ispmnod)
      call edisp(itu,outs)
      write(outs,'(a,1x,f10.2)') 'Pressure at inlet: ',cd1
      call edisp(itu,outs) 
      write(outs,'(a,1x,f10.2)') 'Pressure at outlet: ',cd2
      call edisp(itu,outs) 
      write(outs,'(a,1x,f10.2)') 'Pressure difference: ',delta
      call edisp(itu,outs) 
      write(outs,'(a,1x,f10.2)') 'Power output: ',pspm(ispmnod)
      call edisp(itu,outs) 
  999 return
      end

C  ***************** SPMCMP4 *****************
C A model to calculate the heat flux recovered from a flat-plate
C solar collector. Defining data as read from spmdat:
C    1 - water flow rate (l/s);
C    2 - water inlet temperature (degC);
C    3 - number of tubes (-);
C    4 - tube diameter (m);
C    5 - tube length (m).

C The model has been subjected to rudimentary verification.

      subroutine spmcmp4(icomp,ispmnod)
#include "building.h"

C N.B. All parameters comply with the Fortran implicit naming convention
C except where explicitly redefined.
      common/outin/iuout,iuin
      common/tc/itc,icnt
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/resspm/ndatspm(mspmnod),dataspm(mspmnod,MSPMRES),
     &         strspm(mspmnod,MSPMRES),unitspm(mspmnod,MSPMRES),
     &         txtspm(mspmnod,MSPMRES)
      common/zonspmf/zspmf1(mcom),zspmf2(mcom)
      common/spvst/spmvstr(mspmnod,5)
      common/spmfxst/ispmxist,spflnam
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &              nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      common/spmatlbl/spmlabel(mspmnod)
      common/adjc/ie(mcom,ms),atp(mcom,ms),atf(mcom,ms),arp(mcom,ms),
     &            arf(mcom,ms)  
      common/fvalc/tfc(mcom,ms,mn),qfc(mcom)
      common/coe32/qsoli(ms,2),qsole(ms,2)
      common/coe32j/qtmca(ms,mn,2)
      common/pvpnl/tpnl,qpnl,pvpwr
      common/prec1/sna(mcom,ms),zoa(mcom)
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      
      real mu
      character spmlabel*16, outs*124,spflnam*72,strspm*16,unitspm*16,
     &          txtspm*72

      pi=4.0*atan(1.0)

C Volume flow rate.
      vfr=spmdat(ispmnod,1)/1000.

C Inlet temperature.
      tin=spmdat(ispmnod,2)

C Outlet temperature.
      tout=spmvstr(ispmnod,1)
      if(abs(tout).lt.1E-6)tout=tin

C Number of tubes.
      ntb=nint(spmdat(ispmnod,3))

C Tube diameter.
      tdia=spmdat(ispmnod,4)

C Tube length.
      tlen=spmdat(ispmnod,5)

C Internal surface area.
      area=pi*tdia*tlen*ntb

C Temperature of the backplate.
      tcol=tfc(ispmloc(ispmnod,1),ispmloc(ispmnod,2),
     &                                           ispmloc(ispmnod,3))

C Iteration counter.
      iter=1
   20 continue

C Average fluid temperature.
      dt1=tcol-tin
      dt2=tcol-tout
      if(dt1.le.0..or.abs(dt1-dt2).lt.1E-6)then
         tave=tout+tin/2.
      else
         if(dt2.le.0.)then
            dt2=1E-6
         endif
         tave=(dt1-dt2)/log(dt1/dt2)
      endif
      if(tave.le.0.0)tave=0.0
      if(tave.ge.100.)tave=100.

C Fluid density.
      den=1001.1-0.082*tave-0.0036*tave**2.

C Velocity through tubes.
      vel=vfr/(ntb*pi*(tdia**2)/4.)

C Viscosity.
      visc=0.17444E-02-.15885E-03*tave**0.50

C Conductivity.
      cond=((1.12*tave)+569.)/1000. 

C Specific heat capacity.
      sht=4244.-(22.65*sqrt(tave))+(1.95*tave)
 
C Reynold's Number.
      reyno=den*vel*tdia/visc

C Prandtl Number.
      prandt=visc*sht/cond

C Heat transfer coefficient for either laminar or fully developed
C turbulent flow. 
       if(reyno.gt.2200.)then
          ffr=(0.79*alog(reyno)-1.64)**(-2.)
          fr=ffr/8.
          hin=(cond/tdia)*(fr*reyno*prandt)/
     &                    (1.07+12.7*(fr**0.5)*((prandt**0.667)-1.0))  
       else
          hin=cond*4.4/tdia
       endif 

       rt=(den*vfr*sht)/hin*area
       touti=2.*(tcol-rt*(tout-tin))-tin

       if(abs(touti-tout).gt.0.01)then
          if(iter.lt.101)then
             iter=iter+1
             tout=(toutI+tout)/2.
             goto 20
          else
             call edisp(iuout,' ')
             call edisp(iuout,
     &              'SPMCMP4 fatal error: iteration limit exceeded!')
             stop
          endif
       endif
       
C Save the outlet temperature.
       spmvstr(ispmnod,1)=tout
       
C Heat flux. 
       qtherm=vfr*den*sht*(touti-tin)
       if(qtherm.lt.1E-6)qtherm=0.0

C Results output.
       ndatspm(ispmnod)=1
       dataspm(ispmnod,1)=qtherm
       strspm(ispmnod,1)='Heat output (W)'
       dataspm(ispmnod,2)=touti
       strspm(ispmnod,2)='Outlet temp. (C)'

C As a temporary measure, use zone-related variables, zspmf1 and zspmf2,
C for results library storage. Output is limited to one special material
C per zone.
       zspmf1(ispmloc(ispmnod,1))=dataspm(ispmnod,1)
       zspmf2(ispmloc(ispmnod,1))=dataspm(ispmnod,2)

C Save original nodal absorptions for trace output.
       qsole1=qsole(ispmloc(ispmnod,2),2)
       qsoli1=qsoli(ispmloc(ispmnod,2),2)
       qtmca1=qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3),2)

C Convert the panel power output from W to W/m^2.
       qthermqf=qtherm/sna(ispmloc(ispmnod,1),ispmloc(ispmnod,2)) 
       qthermqp=spmvstr(ispmnod,2)

C Save the power output.
       spmvstr(ispmnod,2)=qthermqf

C Modify thermal output to be an average of future and present values.
       qthermq=(qthermqf+qthermqp)*0.5

C Alter the nodal absorption according to whether node is an external
C (ie=1) or internal (ie=0) surface, and opaque (1) or transparent (0). 
       if(ispmtyp(ispmnod,2).gt.0)then
          if(ie(ispmloc(ispmnod,1),ispmloc(ispmnod,2)).eq.0)then
             qsole(ispmloc(ispmnod,2),2)=qsole(ispmloc(ispmnod,2),2)
     &                                                       -qthermq
          else
             qsoli(ispmloc(ispmnod,2),2)=qsoli(ispmloc(ispmnod,2),2)
     &                                                       -qthermq
         endif

C For a transparent layer, modify the solar flux absorption of middle
C node as well as the two boundary nodes.
       else
          qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3),2)=
     &    qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3),2)-(qthermq/2.)
          qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3)+1,2)=
     &    qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3)+1,2)-(qthermq/4.)
          qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3)-1,2)=
     &    qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3)-1,2)-(qthermq/4.)
       endif

C Trace output.
      if(itc.le.0.or.nsinc.lt.itc.or.ispmxist.eq.0)goto 999
      if(itrace(19).eq.0.or.nsinc.gt.itcf.or.izntrc(icomp).ne.1)goto999
      write(outs,'(a,a)') 'Trace output from SPMCMP4 for ',
     &                                              spmlabel(ispmnod)
      call edisp(itu,outs)
      write(outs,'(a,2x,i4,2x,i4,2x,i4)')
     &        'Affected zone, surface and node: ',ispmloc(ispmnod,1),
     &                          ispmloc(ispmnod,2),ispmloc(ispmnod,3)
      call edisp(itu,outs)
      if(ispmtyp(ispmnod,2).gt.0)then
         write(outs,'(a,1x,f10.2)') 'Original flux absorption: ',qsole1
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Modified flux absorption: ',
     &                                    qsole(ispmloc(ispmnod,2),2)
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Original flux absorption: ',qsoli1
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Modified flux absorption: ',
     &                                    qsoli(ispmloc(ispmnod,2),2)
         call edisp(itu,outs)
      else
         write(outs,'(a,1x,f10.2)') 'Original flux absorption: ',qtmca1 
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Modified flux absorption: ',
     &                 qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3),2)
         call edisp(itu,outs)
      endif
      write(outs,'(a,1x,f10.2)') 'Thermal output: ',qtherm
      call edisp(itu,outs) 
      write(outs,'(a,1x,f10.2)') 'Nodal temperature: ',tcol
      call edisp(itu,outs)
  999 return
      end

C ***************** SPMCMP5 *****************
C A one diode PV model (after Maria Mottillo). The SPMCMP1 model
C is modified to match the Watsun-PV model following recommendations
C by Thevenard. The modifications relate to the method used to 
C calculate the output power. Unlike the SPMCMP1 model, this model
C does not cater for PV modules with a fixed resistive load.

C References:
C Thevenard D (2004), 'Literature and Source Code Review of ESP-r's 
C Exisiting Photovoltaic Models', NRCan Internal Report 23229-049028/001/SQ.

C Thevenard D (2005), 'Review and Recommendations for Improving the
C Modelling of Building-Integrated Photovoltaic Systems', Proc.
C Building Simulation 2005, pp1221-1228, Montreal, Canada.

C Thevenard D (2007), 'Continued Validation of ESP-r's Equivalent
C One-Diode PV Models', NRCan Internal Report.

C Defining data as read from array spmdat:
C    1 - open circuit voltage at reference conditions (volts);
C    2 - short circuit current at reference conditions (amps);
C    3 - voltage at maximum power point and reference conditions (volts);
C    4 - current at maximum power point and reference conditions (amps);
C    5 - reference insolation (W/m^2);
C    6 - reference temperature (K);
C    7 - coefficient 'alpha' of short circuit current (/K);
C    8 - coefficient 'gamma' of open circuit voltage (/K);
C    9 - coefficient of logarithm of irradiance for open circuit voltage (-);
C   10 - number of series connected cells (NOT modules) (-);
C   11 - number of parallel connected branches (-);
C   12 - number of modules in surface (-);
C   13 - load type (0 - maximum power point,;
C                   1 - fixed voltage);
C   14 - fixed voltage if load type = 1 (volts);
C   15 - shading effects
C          0 - as 1, retained for backward compatibility (default);
C          1 - proportional power loss based on calculated incident
C              radiation, which includes full shading effects if
C              obstructions are defined;
C          2 - total power loss, solar flux set to zero to give zero
C              power output;
C          3 - direct radiation power loss, the incident flux is set
C              to the diffuse component only (the power output of all
C              cells falls to that of the shaded cells).
C   16 - miscellaneous loss factor used to derates the panel power
C        to account for uncertainty in the manufacturer's rating,
C        ageing, soiling, snow cover,blocking diodes and wiring. 

      subroutine spmcmp5(icomp,ispmnod)
      implicit none

#include "building.h"

      integer icomp		!- zone number       
      integer ispmnod		!- special material node index
      integer lnblnk		!- implicit function returning length of a string

C I/O.
      common/outin/iuout,iuin
      integer iuout		!- write unit number
      integer iuin		!- read unit number

C Special materials.
      common/enetspm/pspm(mspmnod),qspm(mspmnod),vspm(mspmnod)
      real pspm			!- real power of special material
      real qspm			!- reactive power load/generation of special material
      complex vspm		!- voltage of special material node

      common/resspm/ndatspm(mspmnod),dataspm(mspmnod,MSPMRES),
     &         strspm(mspmnod,MSPMRES),unitspm(mspmnod,MSPMRES),
     &         txtspm(mspmnod,MSPMRES)
      integer   ndatspm		!- number of data items for special material
      real      dataspm		!- special component results data
      character strspm*16	!- name string for the data item
      character unitspm*16	!- unit for data item
      character txtspm*72	!- description of data item

      common/zonspmf/zspmf1(mcom),zspmf2(mcom)
      real zspmf1
      real zspmf2

      common/spmfxst/ispmxist,spflnam
      integer   ispmxist
      character spflnam*72

      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &              nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      integer nspmnod,ispmloc,ispmtyp,nnodat
      real    spmdat

      common/spmatlbl/spmlabel(mspmnod)
      character spmlabel*16

      common/tc/itc,icnt
      integer itc		!- trace output index
      integer icnt		!- trace output counter

      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      integer itcf		!- building side increment to end trace output
      integer itrace		!- trace call index for subroutine
      integer izntrc		!- zone trace index
      integer itu		!- trace output unit number

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      integer ihrp		!- hour number of present time step
      integer ihrf		!- hour number of future time step
      integer idyp		!- day number of present time step
      integer idyf		!- day number of future time step
      integer idwp		!- day of the week of present day
      integer idwf		!- day of the week of future day
      integer nsinc		!- number of building side time increments
				!  since start of simulation
      integer its		!- current building time-step within current hour

      common/fvalc/tfc(mcom,ms,mn),qfc(mcom)
      real    tfc		!- construction node temperature at future time-row
      real    qfc		!- energy injected at construction node at future time-row 

      common/coe32j/qtmca(ms,mn,2)
      real    qtmca		!- shortwave absorption at each node of a transparent mlc

      common/coe32/qsoli(ms,2),qsole(ms,2)
      real    qsoli		!- solar energy absorbed by each internal opaque
				!  surface after adjustments by window shading and
				!  taking into account the directional property of
				!  the direct beam and multiple diffuse reflections 
      real    qsole		!- solar energy absorbed by each external opaque
				!  surface after adjustment by surface shading
				!  and taking into account solar/building geometry

      common/pvpnl/tpnl,qpnl,pvpwr
      real    tpnl,qpnl,pvpwr

      common/prec1/sna(mcom,ms),zoa(mcom)
      real    sna		!- surface area
      real    zoa		!- total surface area of the zone

      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      integer isd1		!- start day number 
      integer ism1		!- start month number
      integer isd2		!- finish day number
      integer ism2		!- finish month number
      integer isds		!- start year day number
      integer isdf		!- finish year day number
      integer ntstep		!- number of building time steps within each hour

      common/adjc/ie(mcom,ms),atp(mcom,ms),atf(mcom,ms),
     &arp(mcom,ms),arf(mcom,ms)  
      integer ie		!- surface defining index
      real    atp		!- present time adjacent temperature
      real    atf		!- future time adjacent temperature
      real    arp		!- present time adjacent incident radiation
      real    arf		!- future time adjacent incident radiation

      common/solext/exrad(ms),exdir(ms),exdif(ms),exshad(ms),
     &                                                    exshadf(ms)
      real    exrad		!- total solar radiation incident on surface
				!  per unit area (includes shading effects)
      real    exdir		!- direct solar radiation incident on surface
				!  per unit area (includes shading effects)
      real    exdif		!- diffuse solar radiation incident on surface
				!  per unit area (shading effect does not
				!  apply on diffuse component)
      real    exshad		!- direct shaded portion expressed as a factor
				!  b/w 0 and 1 of surface
      real    exshadf		!- diffuse shaded portion expressed as a factor
				!  b/w 0 and 1 of surface

      common/prectc/itmcfl(mcom,ms),tmct(mcom,mtmc,5),
     &       tmca(mcom,mtmc,me,5),tmcref(mcom,mtmc),tvtr(mcom,mtmc)
      integer itmcfl		!- index indicating whether surface is transparent
      real    tmct		!- direct solar transmittance at 5 representative
				!  angles for TMC system
      real    tmca		!- absorptances for each glazing element of
				!  TMC system at 5 representative angles
      real    tmcref		!- reflectance of TMC system (at 55 degrees) 
      real    tvtr		!- visible transmittance for the TMC (for daylighting)

      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp)
      integer nconst		!- number of surfaces per zone
      integer nelts		!- number of layers within construction
      integer ngaps		!- number of air gaps within construction
      integer npgap		!- location of air gap

      common/pvang/fInc_angle(ms)
      real fInc_angle		!- angle of incidence for surface

C Local variables.
      real    fVocRef		!- open circuit voltage at reference condition 
      real    fIscRef		!- short circuit current at reference condition
      real    fVoc		!- open circuit voltage for cell
      real    fIsc		!- short circuit current for cell
      real    fVmpRef		!- module voltage at maximum power point and reference condition
      real    fImpRef		!- module current at maximum power point and reference condition
      real    fVmp		!- cell voltage at maximum power point
      real    fImp		!- cell current at maximum power point
      real    fQref		!- reference insolation (W/m^2)
      real    fTref		!- reference temperature (K)

      integer iSCell		!- number of cells connected in series
      integer iNPBrch		!- number of branches connected in parallel
      integer iNPnnls		!- number of modules in surface

      real    fAlpha		!- empirical coefficient for short circuit current calculation (/K)
      real    fBeta		!- empirical coefficient for open circuit voltage calculation (-)
      real    fGamma		!- empirical coefficient for open circuit voltage calculation (/K)

      integer iLodT		!- load type for the panel:
				!         0 - maximum power point tracking (default)
				!         1 - fixed voltage

      integer iMaxPPTrack,iFixedVolt        !- named constants
      parameter(iMaxPPTrack=0,iFixedVolt=1)

      real    fValf		!- module fixed voltage if load type = 1
				!  or fixed resistance if load type = 2

      integer iShad		!- shading treatment index (see above)

      integer iIgnore,iProportion,iTotalPLoss,iPwrShd    !- named constants
      parameter(iIgnore=0,iProportion=1,iTotalPLoss=2,iPwrShd=3)

      real    fE		!- physical constant, electrical charge (J)
      parameter(fE=1.60E-19)

      real    fK		!- physical constant, Boltzmann constant (J/K)
      parameter(fK=1.38E-23)

      real    fQPnl		!- solar incident on the module (W/m^2)
      real    fTPnl		!- temperature of solar module (K)
      real    fLambda		!- empirical coefficient used to calculate current (/V)
      real    fRatioIoIsc	!- ratio of diode reverse saturation current
				!  to short-circuit current
      parameter(fRatioIoIsc=1.0E-09)           !- model assumption

      real    fRseries		!- internal series resistance (ohms)
      real    fPVPower		!- power generated by modules (W)
      real    fPVPwrPerArea	!- generated power per unit area (W/m^2)
      real    fPVVoltage	!- array voltage
      real    fPVCurrent	!- array current
      real    fIcell		!- cell current
      real    fVcell		!- cell voltage
      real    fSurfArea		!- module surface area
      real    fIBound1		!- boundary values required for iterative solution
      real    fIBound2		!- of current, I (amps)
      real    F, FMID		!- required for iterative solution of I (f(I) = 0)
      real    fDeltaI		!- change in I, variable used in iterative solution
      real    fIMID		!- mid-point value used in iterative solution of I
      integer i,j		!- counters
      integer maxit		!- maximum iterations allowed in iterative solution of I
      parameter (maxit=100)
      integer iter		!- iteration counter for iterative solution of I
      logical bfound		!- boolean used in iterative solution of I

      character outs*124	!- user message outputs

      logical close		!- boolean used to compare two values

      real    qsole1		!- original value of external surface solar absorption
      real    qtmca1		!- original value of shortwave absorption at transparent surface

      real    fMiscLossFactor	!- miscellaneous loss factor used to derate the power
				!  generated by the module (-)

      integer iTMC_index	!- index of TMC type for surface containing modules

      real    fTrans_0		!- transmittance at zero degrees
      real    fTrans_60		!- transmittance at 60 degrees
      real    fTrans_IncAng	!- transmittance at angle of incidence

      real    fIncidence_angle	!- angle of incidence

      real    fQinc		!- irradiance of module with no account taken of
				!  reflectance of module front cover (W/m^2)

      logical bAngIncidence_found	!- boolean used for interpolation

      real    X1, X2, Y1, Y2	!- variables used for interpolation calculation

      integer iNum_layers	!- number of layers in PV module surface

      real    fTrans_Ads(5)	!- transmission at 5 angles (0,40,55,70,80 degrees)
				!  for PV module surface calculated as the sum of
				!  the transmission for all layers in the surface
				!  at each individual angle
      real fSmall
      parameter (fSmall=1.0E-06)

C Parameters for h3kreports reporting.
      integer iNameLen(mspmnod)
      character*72 cName(mspmnod)
      logical bInitalized(mspmnod)
      data bInitalized / mspmnod * .false. /

C Store name of special material string in array.
      if(.not.bInitalized(ispmnod))then
         cName(ispmnod)=spmlabel(ispmnod)
         iNameLen(ispmnod)=lnblnk(spmlabel(ispmnod))
         bInitalized(ispmnod)=.true.
      endif

C Module open circuit voltage at reference condition.
      fVocRef=spmdat(ispmnod,1)

C Module short circuit current at reference condition.
      fIscRef=spmdat(ispmnod,2)

C Module voltage at maximum power point and reference condition.
      fVmpRef=spmdat(ispmnod,3)

C Module current at maximum power point and reference condition.
      fImpRef=spmdat(ispmnod,4)

C Reference insolation and temperature.
      fQref=spmdat(ispmnod,5)
      fTref=spmdat(ispmnod,6)

C Empirical coefficient used in calculation of short circuit current.
      fAlpha=spmdat(ispmnod,7)

C Empirical coefficients used in calculation of open circuit voltage.
      fGamma=spmdat(ispmnod,8)
      fBeta=spmdat(ispmnod,9)

C Number of series connected cells (NOT modules).
      iSCell=spmdat(ispmnod,10)

C Number of parallel connected branches.
      iNPBrch=spmdat(ispmnod,11)

C Number of modules in surface.
      iNPnnls=nint(spmdat(ispmnod,12))

C Load type for the panel.
      iLodT=nint(spmdat(ispmnod,13))

C Fixed voltage for the entire module (used if iLodT = 1).
      fValf=spmdat(ispmnod,14)

C Shading treatment.
      iShad=nint(spmdat(ispmnod,15))

C Miscellaneous loss factor.
      fMiscLossFactor=spmdat(ispmnod,16) 

C fTPNL is the temperature of the PV panel, assigned from the
C prevailing nodal temperature.
      fTPnl=tfc(ispmloc(ispmnod,1),ispmloc(ispmnod,2),
     &                                      ispmloc(ispmnod,3))+273.0

C Angle of incidence.
       fIncidence_angle=fInc_angle(ispmloc(ispmnod,2))

C Reset variable used for interpolation.
       bAngIncidence_found=.false.

C Obtain transparent surface index related to PV module location.
       iTMC_index=ITMCFL(ispmloc(ISPMNOD,1),ispmloc(ISPMNOD,2))

C Check that surface associated with the PV module is transparent;
C if not, issue an error message.
       if(iTMC_index.eq.0)then
          call edisp(iuout,' ') 
          write(outs,*)'SMCMP5 fatal error: PV surface must be a TMC!'
          call edisp(iuout,outs)
          stop
       endif

C Number of layers in PV module surface.
       iNum_layers=nelts(ispmloc(ispmnod,1),ispmloc(ispmnod,2))

C Initialize array.
       do j=1,5
          fTrans_Ads(j)=0.
       enddo

C Determine transmission-absorptance factors of the PV module
C surface at 5 incidence angles.
       do j=1,5
          do i=1,iNum_layers
             fTrans_Ads(j)=fTrans_Ads(j)+
     &                       tmca(ispmloc(ispmnod,1),iTMC_index,i,j)
          enddo
       enddo

C Get transmission-absorptance of PV module surface at 0 degrees,
C at 60 degres and at angle of incidence.
       fTrans_0=fTrans_Ads(1)

C Linearly interpolate to find absorptance at 60 degrees.
       fTrans_60=fTrans_Ads(3)+
     &             (60.0-55.0)*
     &             (fTrans_Ads(4)-fTrans_Ads(3))/
     &             (70.0-55.0)
 
C Linearly interpolate to find transmittance at angle of incidence.
        if(fIncidence_angle.ge.0.0.and.
     &           fIncidence_angle.lt.40.0)then
           bAngIncidence_found=.true.
           X1=0.0
           X2=40.0
           Y1=fTrans_Ads(1)
           Y2=fTrans_Ads(2)
        elseif(fIncidence_angle.ge.40.0.and.
     &           fIncidence_angle.lt.55.0)then
           bAngIncidence_found=.true.
           X1=40.0
           X2=55.0
           Y1=fTrans_Ads(2)
           Y2=fTrans_Ads(3)
        elseif(fIncidence_angle.ge.55.0.and.
     &           fIncidence_angle.lt.70.0)then
           bAngIncidence_found=.true.
           X1=55.0
           X2=70.0
           Y1=fTrans_Ads(3)
           Y2=fTrans_Ads(4)
        elseif(fIncidence_angle.ge.70.0.and.
     &           fIncidence_angle.lt.80.0)then
           bAngIncidence_found=.true.
           X1=70.0
           X2=80.0
           Y1=fTrans_Ads(4)
           Y2=fTrans_Ads(5)
        endif

        if(bAngIncidence_found)then
           fTrans_IncAng=Y1+(fIncidence_angle-X1)*(Y2-Y1)/(X2-X1)
        else

C Assume equal to incidence angle at 80degC.
           fTrans_IncAng=fTrans_Ads(5)
        endif

C Obtain module irradiance without taking into account reflectance
C of front cover. This variable is used for reporting and debugging
C purposes only.
       fQInc=exrad(ispmloc(ispmnod,2)) 

C Obtain effective irradiance reaching module (unlike the spmcmp1 
C model, this variable takes into account the reflectance of the
C module's front cover).
       fQPnl=(exdir(ispmloc(ispmnod,2))*fTrans_IncAng+
     &           exdif(ispmloc(ispmnod,2))*fTrans_60)/
     &           fTrans_0 

C Impose specified shading treatment.
       if(exshad(ispmloc(ispmnod,2)).gt.1E-6)then

C External shading case.
         if(iShad.eq.iIgnore.or.iShad.eq.iProportion)then
            fQPnl=(exdir(ispmloc(ispmnod,2))*fTrans_IncAng+
     &               exdif(ispmloc(ispmnod,2))*fTrans_60)/
     &               fTrans_0  
         elseif(iShad.eq.iTotalPLoss)then
            fQPnl=0.0
         else
            fQPnl=exdif(ispmloc(ispmnod,2))*fTrans_60/
     &              fTrans_0
         endif
       else

C No external shading case.
         fQPnl=(exdir(ispmloc(ispmnod,2))*fTrans_IncAng+
     &              exdif(ispmloc(ispmnod,2))*fTrans_60)/
     &              fTrans_0 
       endif

C Calculate cell short circuit current.
       fIsc=fIscRef/iNPBrch*(fQPnl/fQref)*(1.+fAlpha*(fTPnl-fTref))

C Calculate cell open circuit voltage.
       call eclose(fQPnl,0.0,0.001,close)
       if(.not.close)then
          fVoc=fVocRef/iScell*(1.-fGamma*(fTPnl-fTref))*
     &           MAX(0.,(1.+fBeta*log(fQPnl/fQref)))
       else
          fVoc=0.
       endif

       if(iLodT.eq.iMaxPPTrack)then

C Assuming that the maximum power point of the module is tracked by
C a converter, calculate the maximum output voltage. Note that
C the Watsun-PV model assumes that the maximum power point voltage
C has the same dependancy on cell temperature and incident irradiance
C as the open circuit voltage.
          fVmp=fVmpRef/float(iScell)*		!- cell value
     &            (1.-fGamma*(fTPnl-fTref))*
     &            max(0.,(1.+fBeta*log((fQPnl+fSmall)/fQref)))

C Calculate the maximum module power.
          fPVPower=fImpRef*fVmpRef*fIsc*fVoc/
     &               (fIscRef*fVocRef)*
     &               iNPBrch*iSCell*iNPnnls
              
C Calculate the module voltage.
          fPVVoltage=fVmp*iSCell

       elseif(iLodT.eq.iFixedVolt)then

C Panel output with a fixed voltage type load, e.g. a battery.        

C Empirical coefficient used in the calculation of current.
          fLambda=1./fVoc*log(1./fRatioIoIsc) 

C Current at maximum power point. Note that the Watsun-PV
C model assumes that the maximum power point current has the same 
C dependancy on cell temperature and incident irradiance as the 
C short circuit current.
          fImp=fImpRef/iNPBrch*(fQPnl/fQref)*		!- cell value
     &              (1.+fAlpha*(fTPnl-fTref))

C Calculate voltage at maximum power point.
          fVmp=fVmpRef/iScell*				!- cell value
     &              (1.-fGamma*(fTPnl-fTref))*
     &              max(0.,(1.+fBeta*log(fQPnl/fQref)))

C Calculate the internal series resistance.
          fRseries=(1./fLambda*log(1./fRatioIoIsc* 
     &              (1.-fImp/fIsc))-fVmp)/fImp          

C Determine the cell voltage. fValf = fixed voltage value provided
C as model input.
          fVcell=fValf/iScell                

C Calculate the cell current. This is done iteratively using the
C bisection method. The cell current will be between 0 and the
C short-circuit current, therefore these are the bounds for iteration.
          fIBound1=0.
          fIBound2=fIsc
          FMID=fIBound2-fIsc*(1.-fRatioIoIsc*exp(fLambda*
     &                              (fVcell+fIBound2*fRseries)))
          F=fIBound1-fIsc*(1.-fRatioIoIsc*exp(fLambda*
     &                              (fVcell+fIBound1*fRseries)))

          if(FMID.eq.0.)then 
             fIcell=fIBound2
          elseif(F.eq.0.)then        
             fIcell=fIBound1
          endif

          if((F*FMID).gt.0.)then
             continue			!- place error message here
          endif
          if(F.lt.0.)then
             fIcell=fIBound1
             fDeltaI=fIBound2-fIBound1
          else
             fIcell=fIBound2
             fDeltaI=fIBound1-fIBound2
          endif

          bfound=.false.
          iter=0
          do while (.not.bfound) 

C Check that the maximum number of iterations is not exceeded.
             if(iter.gt.maxit)then
                call edisp(iuout,' ')
                call edisp(iuout,
     &              'SPMCMP5 fatal error: iteration limit exceeded!')
                stop
             endif

             fDeltaI=fDeltaI*0.5
             fIMID=fIcell+fDeltaI
             FMID=fIMID-fIsc*(1.-fRatioIoIsc*exp(fLambda*
     &                                  (fVcell+fIMID*fRseries)))
             if(FMID.le.0.)fIcell=fIMID
             if((ABS(fDeltaI).lt.0.001).or.(FMID.eq.0.))then
                bfound=.true.
             endif
             iter=iter+1
          enddo

C Current of entire module.
          fPVcurrent=fIcell*iNPBrch

C PV module voltage.
          fPVVoltage=fValf                       

C Power output.
          fPVPower=fPVVoltage*fPVCurrent*iNPnnls*
     &               (1.-fMiscLossFactor)
       endif

C Results output. 
       fSurfArea=sna(ispmloc(ispmnod,1),ispmloc(ispmnod,2))
       ndatspm(ispmnod)=2        
       dataspm(ispmnod,1)=fPVPower
       strspm(ispmnod,1)='Power (W)'
       if(fQPnl.ne.0.)then
          dataspm(ispmnod,2)=((fPVPower/fSurfArea)/fQPnl)*100.
       else
          dataspm(ispmnod,2) = 0.
       endif
       strspm(ispmnod,2)='Efficiency (%)'

C As a temporary measure, use zone-related variables, zspmf1 and zspmf2,
C for results library storage. Output is limited to one special material
C per zone.
       zspmf1(ispmloc(ispmnod,1))=dataspm(ispmnod,1)
       zspmf2(ispmloc(ispmnod,1))=dataspm(ispmnod,2)

C Save panel power output (for load flow simulation) and the current
C panel voltage. (This could later be changed to a power converter
C component that can vary the values of P and Q as generated by the
C panel.)
       pspm(ispmnod)=fPVPower
       vspm(ispmnod)=cmplx(fPVVoltage,0.0)
       qspm(ispmnod)=0.0

C Convert the panel power output from W to W/m^2.
       fPVPwrPerArea = fPVPower/fSurfArea 

C Save original nodal absorptions for trace output.
       qsole1=qsole(ispmloc(ispmnod,2),2)
       qtmca1=qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3),2)

C Alter the nodal absorption according to whether the node is
C opaque (1) or transparent (0).
       if(ispmtyp(ispmnod,2).gt.0)then
         if(ie(ispmloc(ispmnod,1),ispmloc(ispmnod,2)).eq.0)then
           qsole(ispmloc(ispmnod,2),2)=qsole(ispmloc(ispmnod,2),2)
     &                                 -fPVPwrPerArea
         else
           qsoli(ispmloc(ispmnod,2),2)=qsoli(ispmloc(ispmnod,2),2)
     &                                 -fPVPwrPerArea
         endif
       else

C For a transparent layer, modify absorption of the middle node
C and the two boundary nodes.
        qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3),2)=
     &          qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3),2)-
     &          (fPVPwrPerArea/2.)
        qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3)+1,2)=
     &          qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3)+1,2)-
     &          (fPVPwrPerArea/4.)
        qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3)-1,2)=
     &          qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3)-1,2)-
     &          (fPVPwrPerArea/4.)        
       endif

C XML output.
       call add_to_xml_reporting(fQPnl,
     &  'building/spmatl/' // cName(ispmnod)(1:iNameLen(ispmnod)) //
     &     '/misc_data/eff_irradiance/unit_area',
     &  'units',
     &  'W/m^2',
     &  'Effective irradiance of PV module (/m^2)')

       call add_to_xml_reporting(fIncidence_angle,
     &  'building/spmatl/' // cName(ispmnod)(1:iNameLen(ispmnod)) //
     &     '/misc_data/incidence_angle',
     &  'units',
     &  '-',
     &  'Angle of incidence')

       call add_to_xml_reporting(fTrans_IncAng,
     &  'building/spmatl/' // cName(ispmnod)(1:iNameLen(ispmnod)) //
     &     '/misc_data/transmittance_inc_angle',
     &  'units',
     &  '-',
     &  'Transmittance at angle of incidence')

       call add_to_xml_reporting(fTrans_0,
     &  'building/spmatl/' // cName(ispmnod)(1:iNameLen(ispmnod)) //
     &     '/misc_data/transmittance_0',
     &  'units',
     &  '-',
     &  'Transmittance at 0 degrees')

       call add_to_xml_reporting(fTrans_60,
     &  'building/spmatl/' // cName(ispmnod)(1:iNameLen(ispmnod)) //
     &     '/misc_data/transmittance_60',
     &  'units',
     &  '(-)',
     &  'Transmittance at 60 degrees')

       call add_to_xml_reporting(fQInc,
     &  'building/spmatl/' // cName(ispmnod)(1:iNameLen(ispmnod)) //
     &     '/misc_data/total_incident_irradiance/unit_area',
     &  'units',
     &  '(W/m^2)',
     &  'Total irradiance of PV module (/m^2')

       call add_to_xml_reporting(fQInc * fSurfArea,
     &  'building/spmatl/' // cName(ispmnod)(1:iNameLen(ispmnod)) //
     &     '/misc_data/total_incident_irradiance/total',
     &  'units',
     &  '(W)',
     &  'Total irradiance of PV module')

       call add_to_xml_reporting(exdir(ispmloc(ispmnod,2)),
     &  'building/spmatl/' // cName(ispmnod)(1:iNameLen(ispmnod)) //
     &     '/misc_data/direct_incident_irradiance/unit_area',
     &  'units',
     &  '(W/m^2)',
     &  'Direct irradiance of PV module (/m^2)')

       call add_to_xml_reporting(exdif(ispmloc(ispmnod,2)),
     &  'building/spmatl/' // cName(ispmnod)(1:iNameLen(ispmnod)) //
     &     '/misc_data/diffuse_incident_irradiance/unit_area',
     &  'units',
     &  '(W/m^2)',
     &  'Diffuse irradiance of PV module (/m^2)')

       call add_to_xml_reporting(fPVPower,
     &  'building/spmatl/' // cName(ispmnod)(1:iNameLen(ispmnod)) //
     &     '/misc_data/pv_power',
     &  'units',
     &  '(W)',
     &  'Power produced by PV module')

C Trace output.
      if(itc.le.0.or.nsinc.lt.itc.or.ispmxist.eq.0)goto 999
      if(itrace(19).eq.0.or.nsinc.gt.itcf.or.izntrc(icomp).ne.1)goto999
      write(outs,'(a,a)') 'Trace output from SPMCMP5 for ',
     &                                              spmlabel(ispmnod)
      call edisp(itu,outs)
      write(outs,'(a,2x,i4,2x,i4,2x,i4)')
     &    'Affected zone, surface and node: ',ispmloc(ispmnod,1),
     &                          ispmloc(ispmnod,2),ispmloc(ispmnod,3)
      call edisp(itu,outs)
      write(outs,'(a,i3,i3,i3)') 'Number. of modules in surface:',
     &                                                        iNPnnls
      call edisp(itu,outs)
      if(ispmtyp(ispmnod,2).gt.0)then
         write(outs,'(a,1x,f10.2)') 'Original flux absorption: ',qsole1
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Modified flux absorption: ',
     &                                    qsole(ispmloc(ispmnod,2),2)
         call edisp(itu,outs)
      else
         write(outs,'(a,1x,f10.2)') 'Original flux absorption: ',qtmca1 
         call edisp(itu,outs)
         write(outs,'(a,1x,f10.2)') 'Modified flux absorption: ',
     &                 qtmca(ispmloc(ispmnod,2),ispmloc(ispmnod,3),2)
         call edisp(itu,outs)
      endif
      write(outs,'(a,1x,f10.2)') 'Power output: ', fPVPower
      call edisp(itu,outs) 
      write(outs,'(a,1x,f10.2)') 'Panel temperature: ', fTPnl
      call edisp(itu,outs)
  999 return
      end

C ***************** SPMCMP51 *****************
C A model of the change in shortwave flux transmission and absorption
C due to the changing tint of a thermo-chromic glass. The model
C assumes a linear variation of transmission over the working
C temperature range (Tmin-Tmax). Outwith this range, the material
C is either clear (<Tmin) or at maximum tint (>Tmax). While the
C transmission and absorption both vary with temperature, the
C reflectivity remains constant.Transmission is therefore a
C discontinuous function of the temperature of the associated node.
C Note that the transmission charactersitics of all layers after
C the switched layer are affected. Defining data as read from spmdat:
C    1 - temperature at minimum transmission, Tmint (degC);
C    2 - temperature at maximum transmission, Tmaxt (degC);
C    3 - minimum transmission as a % of max transmission;
C    4 - associated layer number in TMC.

      subroutine spmcmp51(icomp,ispmnod)
#include "building.h"

C N.B. All parameters comply with the Fortran implicit naming convention
C except where explicitly redefined.
      common/outin/iuout,iuin
      common/tc/itc,icnt
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/spmfxst/ispmxist,spflnam
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &              nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      common/spmatlbl/spmlabel(mspmnod)
      common/resspm/ndatspm(mspmnod),dataspm(mspmnod,MSPMRES),
     &         strspm(mspmnod,MSPMRES),unitspm(mspmnod,MSPMRES),
     &         txtspm(mspmnod,MSPMRES)
      common/zonspmf/zspmf1(mcom),zspmf2(mcom)
      common/fvalc/tfc(mcom,ms,mn),qfc(mcom)

C TMC and construction data.
      common/prectc/itmcfl(mcom,ms),tmct(mcom,mtmc,5),
     &         tmca(mcom,mtmc,me,5),tmcref(mcom,mtmc),tvtr(mcom,mtmc)
      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp)

C Stored TMC information.
      common/tmcstor/tran0(mspmnod,5),abs0(mspmnod,ms,5),istore

      character spmlabel*16,outs*124,spflnam*72,strspm*16,unitspm*16,
     &          txtspm*72

      small=1.0E-5

C Temperature at maximum transmission.
      tmaxt=spmdat(ispmnod,1)

C Temperature at minimum transmission.
      tmint=spmdat(ispmnod,2)
      if(tmint.lt.tmaxt)then
         call edisp(iuout,' ')
         write(outs,'(a)')'SPMCMP51 fatal error: temperature at'
         call edisp(iuout,outs)
         write(outs,'(a)')'minimum transmission is lower than'
         call edisp(iuout,outs)
         write(outs,'(a)')'at maximum transmission!'
         call edisp(iuout,outs)
         stop
      endif

C Minimum transmission as a % of transmission at clear state.
      perc=spmdat(ispmnod,3)

C Location layer for thermo-chromic material.
      ilyr=int(spmdat(ispmnod,4))

C Number of layers in the TMC.
      nlyr=nelts(icomp,ispmloc(ispmnod,2))

C Node location and temperature.
      isur=ispmloc(ispmnod,2)
      inod=ispmloc(ispmnod,3)     
      tnod=tfc(icomp,isur,inod)

C Store the original clear state optical properties for the affected
C layer(s).
      if(istore.eq.0)then

C Sytem transmission.
         do iang0=1,5 
            tran0(ispmnod,iang0)=tmct(icomp,itmcfl(icomp,isur),iang0)
         enddo

C Layer absorption.
         do layer=1,nlyr
            do iang=1,5
               abs0(ispmnod,layer,iang)=tmca(icomp,itmcfl(icomp,isur),
     &                                                    layer,iang)
            enddo
         enddo
         istore=1
      endif

C System is unaltered. 
      if(tnod.lt.tmaxt)then
        tint=1.0

C Alter system transmission and layer absorption.
      elseif(tnod.gt.tmaxt.and.tnod.lt.tmint) then

C Calculate transmission percentage at current nodal temperature.
         diff=abs(tmaxt-tmint)
         if(diff.le.small)then

C User has set tmint=tmaxt.
            tint=(100.+perc)/200. 
            goto 33
         endif          

         tint=((tnod-tmint)/(tmaxt-tmint))*(100.-perc)+perc
         tint=tint/100.
   33    continue   

C > tmint.
      elseif(tnod.ge.tmint)then
         tint=perc/100.
      endif

C Results output.
      ndatspm(ispmnod)=1
      dataspm(ispmnod,1)=tint
      strspm(ispmnod,1)='Transmission (%)'

C As a temporary measure, use zone-related variable, zspmf1, for
C results library storage. Output is limited to one special material
C per zone.
      zspmf1(ispmloc(ispmnod,1))=dataspm(ispmnod,1)

C Alter the values of transmission for the affected layer as a
C function of the tint value.
      do iang2=1,5
         tmct(icomp,itmcfl(icomp,isur),iang2)=tran0(ispmnod,iang2)*tint

C Adjust the layer absorption as a function of this transmission
C change.
         tmca(icomp,itmcfl(icomp,isur),ilyr,iang2)=
     &       tran0(ispmnod,iang2)*(1.- tint)+abs0(ispmnod,ilyr,iang2)
      enddo

C Reduce relative absorption for all layers after the tinted layer.
      if(ilyr.lt.nlyr)then
         do layer2=ilyr+1,nlyr
            do iang3=1,5
               tmca(icomp,itmcfl(icomp,isur),layer2,iang3)=
     &                                abs0(ispmnod,layer2,iang3)*tint
            enddo
         enddo 
      endif        
       
C Trace output.
      if(itc.le.0.or.nsinc.lt.itc.or.ispmxist.eq.0)goto 999
      if(itrace(19).eq.0.or.nsinc.gt.itcf.or.izntrc(icomp).ne.1)goto999 
      write(outs,'(a,a)') 'Trace output from SPMCMP51 for ',
     &                                              spmlabel(ispmnod)
      call edisp(itu,outs)
      write(outs,'(a,2x,i4,2x,i4,2x,i4)')
     &  'Affected zone, surface and node: ',ispmloc(ispmnod,1),
     &                          ispmloc(ispmnod,2),ispmloc(ispmnod,3)
      call edisp(itu,outs)
      call edisp(itu,' ')
      write(outs,'(a)') 'Original transmissions:'
      call edisp(itu,outs)
      write(outs,'(5(2x,f7.4))') (tran0(ispmnod,idat),idat=1,5)
      call edisp(itu,outs)
      write(outs,'(a)') 'Original absorptions:'
      call edisp(itu,outs)
      write(outs,'(5(2x,f7.4))') (abs0(ispmnod,ilyr,idat),idat=1,5)
      call edisp(itu,outs)
      call edisp(itu,' ')       
      write(outs,'(a)') 'Altered transmissions:'
      call edisp(itu,outs)
      write(outs,'(5(2x,F7.4))') (tmct(icomp,itmcfl(icomp,isur),idat),
     &                                                      idat=1,5)
      call edisp(itu,outs)
      write(outs,'(a)') 'Altered absorptions:'
      call edisp(itu,outs)
      write(outs,'(5(2x,f7.4))') (tmca(icomp,itmcfl(icomp,isur),
     &                                         ilyr,idat),idat=1,5)
      call edisp(itu,outs)
  999 return
      end

C ***************** SPMCMP52 *****************
C A model of saturated surface evaporation and heat loss. The
C evaporation (kg/s) is calculated from the Lewis relationship: 
C        Ev=hc.Asurf.(Wsurf-Wzone)/Cp (kg/s)
C where,
C       hc = convective heat transfer coefficient (W/m^2.degC)
C       Asurf = surface area (m^2)
C       Wsurf = saturated moisture content at Tsurf (kg/kg da)
C       Tsurf = surface temperature (degC)
C       Wzone = moisture content of zone air (kg/kg da)
C       Cp = specific heat capacity of air (J/kgK).

      subroutine spmcmp52(icomp,ispmnod)
#include "building.h"

C N.B. All parameters comply with the Fortran implicit naming convention
C except where explicitly redefined.
      common/tc/itc,icnt
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/spmfxst/ispmxist,spflnam
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &              nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      common/spmatlbl/spmlabel(mspmnod)   
      common/resspm/ndatspm(mspmnod),dataspm(mspmnod,MSPMRES),
     &         strspm(mspmnod,MSPMRES),unitspm(mspmnod,MSPMRES),
     &         txtspm(mspmnod,MSPMRES)
      common/zonspmf/zspmf1(mcom),zspmf2(mcom)
      common/evapsur/vapsur(mcom)
      common/prec1/sna(mcom,ms),zoa(mcom)
      common/fvala/tfa(mcom),qfa(mcom)
      common/fvals/tfs(mcom,ms),qfs(mcom)
      common/concoe/hcip(mcom,ms),hcif(mcom,ms),hcop(mcom,ms),
     &              hcof(mcom,ms)
      common/fvalg/gfa(mcom)
      common/atpres/patmos

C Generic nodal flux common allowing a positive or negative flux
C to be applied to a node; genflxp is updated in mzls5.
      common/genflux/genflxf(mcom,ms,mn),genflxp(mcom,ms,mn)

      character spmlabel*16,spflnam*72,outs*124,strspm*16,unitspm*16,
     &          txtspm*72

C surface location.
      if(icomp.ne.ispmloc(ispmnod,1))then
         write(outs,*)'SPMAT52 fatal error: incorrect zone assignment!'
         call edisp(iuout,outs)
         stop
      endif
      isurf=ispmloc(ispmnod,2)
      inode=ispmloc(ispmnod,3)

      td=tfa(icomp)
      gz=gfa(icomp)
      zrh=pcrh2(td,gz,patmos)
      asurf=sna(icomp,isurf)
      tsurf=tfs(icomp,isurf)
      alpha=hcif(icomp,isurf)


C Because there may be more than one evaporative surface, Ev is added
C to the current value of vapsur(icomp).
      wzone=gfa(icomp)
      cp=sphtc2(td,wzone)*1000
      saturate=100.0
      wsurf=humrt1(tsurf,saturate,PATMOS,0)

      ev=alpha*asurf*(wsurf-wzone)/cp
      vapsur(icomp)=vapsur(icomp)+ev

C Determine the heat drawn from the surface during evaporation. 

C Enthalpy of water vapour at tsurf (approximate value is 2550E+03).
      hvap=cndwat1(tsurf)

C Enthalpy of saturation at tsurf.
      hsurf=shth2O(tsurf)*1000.

C Surface evaporative loss (W/m^2).      
      genflxf(icomp,isurf,inode)=-ev*(hvap-hsurf)/sna(icomp,isurf)

C Results output.
      ndatspm(ispmnod)=2
      dataspm(ispmnod,1)=genflxf(icomp,isurf,inode)*sna(icomp,isurf)
      strspm(ispmnod,1)='Evaporation (W)'
      dataspm(ispmnod,2)=ev*1000.
      strspm(ispmnod,2)='Evaporation(g/s)'    

C As a temporary measure, use zone-related variables, zspmf1 and zspmf2,
C for results library storage. Output is limited to one special material
C per zone.
      zspmf1(ispmloc(ispmnod,1))=dataspm(ispmnod,1)
      zspmf2(ispmloc(ispmnod,1))=dataspm(ispmnod,2)   

C Trace output.
      if(itc.le.0.or.nsinc.lt.itc.or.ispmxist.eq.0)goto 999
      if(itrace(19).eq.0.or.nsinc.gt.itcf.or.izntrc(icomp).ne.1)goto999     
      write(outs,'(a,a)') 'Trace output from SPMCMP52 for ',
     &                                              spmlabel(ispmnod)
      call edisp(itu,outs)
      write(outs,'(a,2x,i4,2x,i4,2x,i4)')
     &        'Affected zone, surface and node: ',ispmloc(ispmnod,1),
     &                          ispmloc(ispmnod,2),ispmloc(ispmnod,3)
      call edisp(itu,outs)
      call edisp(itu,' ')
      write(outs,'(a)') 'Evaporation from surface (kg):'
      call edisp(itu,outs)
      write(outs,'(f7.4)') ev
      call edisp(itu,outs)
      write(outs,'(a)') 'Evaporative heat loss from surface (W/m^2):' 
      call edisp(itu,outs) 
      write(outs,'(F10.4)') genflxf(icomp,isurf,inode)
      call edisp(itu,outs)   
      write(outs,'(a)') 'Evaporative heat loss from surface (W):' 
      call edisp(itu,outs) 
      write(outs,'(F10.4)')genflxf(icomp,isurf,inode)*sna(icomp,isurf)
      call edisp(itu,outs)   
  999 return
      end

C ***************** SPMCMP53 *****************
C A model of phase change material based on the apparent heat capacity
C method. The  thermal properties of a construction layer defined as
C a phase change material (PCM) are adjusted to represent the latent
C heat stored within the material during melting and released during
C solidification. The phase transition is represented by an apparent
C heat capacity (appsht), which is equal to the sum of specific (shtsol)
C and latent (lht) heat, with the latter established as a linear
C function of temperature (tfcm) in the phase change temperature range
C (tsoli-tmelt): lht=shtliqa*tfcm+shtliqb (where shtliqa is given as 0,
C lht will be constant over the phase change temperature range).

C Defining data as read from spmdat:
C    1 - melting temperature, tmelt (degC);
C    2 - solidification temperature, tsoli (degC);
C    3 - conductivity in solid phase, consol (W/m.degC);
C    4 - conductivity in liquid phase, conliq (W/m.degC);
C    5 - specific heat in all phases, shtsol (J/kg.degC);
C    6 - latent heat coefficient, shtliqa (J/kg.degC^2);
C    7 - latent heat coefficient, shtliqb (J/kg.degC).

      subroutine spmcmp53(icomp,ispmnod)
#include "building.h"

C N.B. All parameters comply with the Fortran implicit naming convention
C except where explicitly redefined.
      common/outin/iuout,iuin
      common/tc/itc,icnt
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/fvalc/tfc(mcom,ms,mn),qfc(mcom)
      common/fvals/tfs(mcom,ms),qfs(mcom)
      common/zonspmf/zspmf1(mcom),zspmf2(mcom)

C thrmli(icomp,isurf,ielem,1) = conductivity
C thrmli(icomp,isurf,ielem,2) = density
C thrmli(icomp,isurf,ielem,3) = specific heat
C thrmli(icomp,isurf,ielem,4) = thickness
      common/vthp14/thrmli(mcom,ms,me,7)
      common/prec1/sna(mcom,ms),zoa(mcom)
      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp)
      common/prec13/c(mcom,ms,mn,2),qc(mcom,ms,mn)
      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      common/vthp15/vcp(ms,mn,2),qcp(ms,mn)
      common/vthp16/vcf(ms,mn,2),qcf(ms,mn)
      common/resspm/ndatspm(mspmnod),dataspm(mspmnod,MSPMRES),
     &         strspm(mspmnod,MSPMRES),unitspm(mspmnod,MSPMRES),
     &         txtspm(mspmnod,MSPMRES)

C Special materials.
      common/spmfxst/ispmxist,spflnam
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &              nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      common/spmatlbl/spmlabel(mspmnod)

C Phase change material.
      common/pcm02/spcm(mcom,ms,mn),spcmtf,spcmtp
      common/pcm03/pcmfac(mcom,ms,mn),pcmshts(mcom,ms,mn)

      character spmlabel*16,spflnam*72,outs*124,strspm*16,unitspm*16,
     &          txtspm*72
      logical close

C PCM layer pointers.
      if(icomp.ne.ispmloc(ispmnod,1))then
         write(outs,*)'SPMAT53 fatal error: incorrect zone assignment!'
         call edisp(iuout,outs)
         stop
      endif
      isurf=ispmloc(ispmnod,2)
      inode=ispmloc(ispmnod,3)
      ielem=nint(real(inode)/2.)

C Melting and solidification temperatures (degC).
      tmelt=spmdat(ispmnod,1)
      tsoli=spmdat(ispmnod,2)

C Conductivity in solid and liquid phase (W/m.degC).
      consol=spmdat(ispmnod,3)
      conliq=spmdat(ispmnod,4)

C Specific heat capacity (J/kg.degC).
      shtsol=spmdat(ispmnod,5)

C Latent heat coefficients (J/kg.degC^2 & J/kg.degC respectively).
      shtliqa=spmdat(ispmnod,6)
      shtliqb=spmdat(ispmnod,7)

C Initialisations for xml output.
      Rtot=0.0
      THKsum=0.0
      sumSHTTHK=0.0

C Process the 3 nodes of the PCM layer.
      spcmtf=0.0
      do 10 iphnl=1,3
         if(iphnl.eq.1)then
            nloc=inode-1
            efthk=thrmli(icomp,isurf,ielem,4)*0.25
         elseif(iphnl.eq.2)then
            nloc=inode
            efthk=thrmli(icomp,isurf,ielem,4)*0.5
         elseif(iphnl.eq.3)then
            nloc=inode+1
            efthk=thrmli(icomp,isurf,ielem,4)*0.25
         endif
         THKsum=THKsum+efthk		!- total PCM thickness

C Establish temperature of the PCM material node from tfc if the node
C is intra-construction and tfs if located at the internal surface.
         if(iphnl.eq.3.and.ielem.eq.nelts(icomp,isurf))then
            tfcm=tfs(icomp,isurf)
         else
            tfcm=tfc(icomp,isurf,nloc)
         endif

C If phase change is underway, calculate the new value of
C conductivity (W/m.degC):
C  appcon = consol          for tfcm < tmelt (solid phase);  
C         = conliq          for tfcm > tsoli (liquid phase); 
C         = consol+conliq/2 for tmelt < tfcm < tsoli (mixed phase);
C ... and apparent heat capacity (J/kg.degC):
c  appsht = shtsol + lht
C   where lht = 0                      for tsoli < tfcm < tmelt (solid or liquid phase); 
C             = shtliqa*tfcm + shtliqb for tmelt < tfcm < tsoli (mixed phase). 
         if(tfcm.lt.tmelt)then
            appcon=consol
            appsht=shtsol
         elseif(tfcm.gt.tsoli)then
            appcon=conliq
            appsht=shtsol
         else
            appcon=(consol+conliq)/2.
            appsht=shtsol+(shtliqa*tfcm+shtliqb)
         endif

C Parameters for xml output.
         Rtot=Rtot+efthk/appcon
         sumSHTTHK=sumSHTTHK+appsht

C Establish present value of factp and pcmshtp as last used value
C of factf and pcmsht respectively.
         factp=pcmfac(icomp,isurf,nloc)
         pcmshtp=pcmshts(icomp,isurf,nloc)
         if(nsinc.eq.1)then
            factp=0
         endif

C Calculate new factf; this is used below to remove original values
C of PCM layer conductivity and specific heat as used to establish the
C self- and cross- coupling coefficients of the nodal energy balance
C equations, and substitute the new values as established above. An
C adjusted factf is required where the PCM node separates the PCM layer
C from a non-PCM layer.
         pcmcon=appcon/consol
         pcmsht=appsht/shtsol
         factf=pcmcon/pcmsht

C Modify present and future value of 'c' and 'qc' coefficients (i.e.
C coeficients of nodal energy balance equations.
         x1=1./pcmshtp
         if(nsinc.eq.1)then
            x1=0
         endif
         x2=1./pcmsht
         if(iphnl.eq.1.and.ielem.eq.1)then			!- outermost node
            vcp(isurf,nloc,1)=c(icomp,isurf,nloc,1)*x1
            vcp(isurf,nloc,2)=c(icomp,isurf,nloc,2)*factp
            qcp(isurf,nloc)=qc(icomp,isurf,nloc)*x1
            vcf(isurf,nloc,1)=c(icomp,isurf,nloc,1)*x2
            vcf(isurf,nloc,2)=c(icomp,isurf,nloc,2)*factf
            qcf(isurf,nloc)=qc(icomp,isurf,nloc)*x2
         elseif(iphnl.eq.2)then					!- centre node
            vcp(isurf,nloc,1)=c(icomp,isurf,nloc,1)*factp
            vcp(isurf,nloc,2)=c(icomp,isurf,nloc,2)*factp
            qcp(isurf,nloc)=qc(icomp,isurf,nloc)*x1
            vcf(isurf,nloc,1)=c(icomp,isurf,nloc,1)*factf
            vcf(isurf,nloc,2)=c(icomp,isurf,nloc,2)*factf
            qcf(isurf,nloc)=qc(icomp,isurf,nloc)*x2
         elseif(iphnl.eq.3.and.ielem.eq.nelts(icomp,isurf))then	!- internal surface node
            vcp(isurf,nloc,1)=c(icomp,isurf,nloc,1)*factp
            vcp(isurf,nloc,2)=c(icomp,isurf,nloc,2)*x1
            qcp(isurf,nloc)=qc(icomp,isurf,nloc)*x1
            vcf(isurf,nloc,1)=c(icomp,isurf,nloc,1)*factf
            vcf(isurf,nloc,2)=c(icomp,isurf,nloc,2)*x2
            qcf(isurf,nloc)=qc(icomp,isurf,nloc)*x2
         elseif(iphnl.eq.1.or.iphnl.eq.3)then			!- interface node

C Redefine modification factors to take account of weighted node
C thermal capacity.
            if(iphnl.eq.1)then
               iouter=ielem
               iinner=ielem-1
            else
               iinner=ielem
               iouter=ielem+1
            endif

C Original averaged capacity before adjustment.       
            capouter=thrmli(icomp,isurf,iouter,2)*
     &                               thrmli(icomp,isurf,iouter,3)*
     &                               (thrmli(icomp,isurf,iouter,4)/2.)
            capinner=thrmli(icomp,isurf,iinner,2)*
     &                               thrmli(icomp,isurf,iinner,3)*
     &                               (thrmli(icomp,isurf,iinner,4)/2.)

C Cater for an adjacent air gap.
            ians1=0
            ians2=0
            if(ngaps(icomp,isurf).ne.0)then
               if(iphnl.eq.1)then
                  do igap=1,ngaps(icomp,isurf)
                     if(iinner.eq.npgap(icomp,isurf,igap))then
                        capinner=1.3*1005.5*
     &                               (thrmli(icomp,isurf,iinner,4)/2.)
                        ians1=1
                     endif
                  enddo
               elseif(iphnl.eq.3)then
                  do igap=1,ngaps(icomp,isurf)
                     if(iouter.eq.npgap(icomp,isurf,igap))then
                        capouter=1.3*1005.5*
     &                               (thrmli(icomp,isurf,iouter,4)/2.)
                        ians2=1
                     endif
                  enddo
               endif
            endif
            cap=capouter+capinner

C Average capacity utilising new PCM apparent specific heat.
            capouter=thrmli(icomp,isurf,iouter,2)*
     &                               appsht*
     &                               (thrmli(icomp,isurf,iouter,4)/2.)
            capinner=thrmli(icomp,isurf,iinner,2)*
     &                               thrmli(icomp,isurf,iinner,3)*
     &                               (thrmli(icomp,isurf,iinner,4)/2.)
            if(iphnl.eq.1.and.ians1.eq.1)then
               capinner=1.3*1005.5*(thrmli(icomp,isurf,iinner,4)/2.)
            elseif(iphnl.eq.3.and.ians2.eq.1)then
               capouter=1.3*1005.5*(thrmli(icomp,isurf,iouter,4)/2.)
            endif
            capPCM=capouter+capinner

C capPCM value at previous time step.
            capouter=thrmli(icomp,isurf,iouter,2)*
     &                               pcmshtp*shtsol*
     &                               (thrmli(icomp,isurf,iouter,4)/2.)
            capinner=thrmli(icomp,isurf,iinner,2)*
     &                               thrmli(icomp,isurf,iinner,3)*
     &                               (thrmli(icomp,isurf,iinner,4)/2.)
            if(iphnl.eq.1.and.ians1.eq.1)then
               capinner=1.3*1005.5*(thrmli(icomp,isurf,iinner,4)/2.)
            elseif(iphnl.eq.3.and.ians2.eq.1)then
               capouter=1.3*1005.5*(thrmli(icomp,isurf,iouter,4)/2.)
            endif
            capPCMp=capouter+capinner

            x1=cap/capPCMp
            x2=cap/capPCM
            xfactp=factp*pcmshtp*x1
            xfactf=pcmcon*x2
            if(iphnl.eq.3)then
               vcp(isurf,nloc,1)=c(icomp,isurf,nloc,1)*xfactp
               vcp(isurf,nloc,2)=c(icomp,isurf,nloc,2)*x1
               qcp(isurf,nloc)=qc(icomp,isurf,nloc)*x1
               vcf(isurf,nloc,1)=c(icomp,isurf,nloc,1)*xfactf
               vcf(isurf,nloc,2)=c(icomp,isurf,nloc,2)*x2
               qcf(isurf,nloc)=qc(icomp,isurf,nloc)*x2
            elseif(iphnl.eq.1.and.ians1.eq.1)then
               vcp(isurf,nloc,1)=c(icomp,isurf,nloc,1)*x1
               vcp(isurf,nloc,2)=c(icomp,isurf,nloc,2)*xfactp
               qcp(isurf,nloc)=qc(icomp,isurf,nloc)*x1
               vcf(isurf,nloc,1)=c(icomp,isurf,nloc,1)*x2
               vcf(isurf,nloc,2)=c(icomp,isurf,nloc,2)*xfactf
               qcf(isurf,nloc)=qc(icomp,isurf,nloc)*x2
            endif
         endif

C Save factf and pcmsht for use as factp and pcmshtp at next
C time step.
         pcmfac(icomp,isurf,nloc)=factf
         pcmshts(icomp,isurf,nloc)=pcmsht

C Maximum latent heat (J/kg).
         shtliq=(shtliqa*tsoli+shtliqb)

C Latent heat stored (J) in part of the PCM layer represented by
C node nloc.
         if(tfcm.lt.tmelt)then
            spcmf=0.0
         elseif(tfcm.gt.tsoli)then
            call eclose(shtliqa,0.00,0.001,close)
            if(close)then
               spcmf=(shtliq-shtsol)*(tsoli-tmelt)*
     &             thrmli(icomp,isurf,ielem,2)*sna(icomp,isurf)*efthk
            else
               spcmf=0.5*(shtliq-shtsol)*(tsoli-tmelt)*
     &             thrmli(icomp,isurf,ielem,2)*sna(icomp,isurf)*efthk
            endif
         else
            call eclose(shtliqa,0.00,0.001,close)
            if(close)then
               spcmf=(appsht-shtsol)*(tfcm-tmelt)*
     &             thrmli(icomp,isurf,ielem,2)*sna(icomp,isurf)*efthk
            else
               spcmf=0.5*(appsht-shtsol)*(tfcm-tmelt)*
     &             thrmli(icomp,isurf,ielem,2)*sna(icomp,isurf)*efthk
            endif
         endif

C Save node-related heat stored.
         spcm(icomp,isurf,nloc)=spcmf
   10 continue

C Save total PCM heat stored.
      spcmtf=spcm(icomp,isurf,inode-1)+spcm(icomp,isurf,inode)+
     &                                      spcm(icomp,isurf,inode+1)
       
C Results output.
      ndatspm(ispmnod)=4
      if(nsinc.le.1)then
         strspm(ispmnod,1)='/AppCON'
         unitspm(ispmnod,1)='(W/(m.degC))'
         txtspm(ispmnod,1)='Average conductivity of PCM layer'

         strspm(ispmnod,2)='/SHCtot'
         unitspm(ispmnod,2)='(J/kg)'
         txtspm(ispmnod,2)='Total heat stored in PCM'

         strspm(ispmnod,3)='/SHCarea'
         unitspm(ispmnod,3)='(J/m^2)'
         txtspm(ispmnod,3)='Heat stored per PCM area'

         strspm(ispmnod,4)='/PCMFAC'
         unitspm(ispmnod,4)='(-)'
         txtspm(ispmnod,4)='PCMFAC of middle node'
      endif

      if(Rtot.gt.0)dataspm(ispmnod,1)=THKsum/Rtot
      if(THKsum.gt.0)dataspm(ispmnod,2)=sumSHTTHK
      dataspm(ispmnod,3)=(spcmtp+spcmtf)/2.0
      spcmtp=spcmtf
      dataspm(ispmnod,4)=pcmfac(icomp,isurf,inode)      

C As a temporary measure, use zone-related variable, zspmf1, for
C for results library storage. Output is limited to one special
C material per zone.
      zspmf1(ispmloc(ispmnod,1))=dataspm(ispmnod,1)

C XML output.
      call PCM_to_h3k(icomp,inode,isurf,ispmnod)

C Trace output.
      if(itc.le.0.or.nsinc.lt.itc.or.ispmxist.eq.0)goto 999
      if(itrace(19).eq.0.or.nsinc.gt.itcf.or.izntrc(icomp).ne.1)goto999
      write(outs,'(a,a)') 'Trace output from SPMCMP53 for ',
     &                                              spmlabel(ispmnod)
      call edisp(itu,outs)
      write(outs,'(a,5(2x,i4))')
     &        'Affected zone, surface and node: ',ispmloc(ispmnod,1),
     &                       ispmloc(ispmnod,2),inode-1,inode,inode+1
      call edisp(itu,outs)
      call edisp(itu,' ')
      do iphnl=1,3
         if(iphnl.eq.1)then
            write(outs,'(a,i3)') 'Outermost node (',inode-1,')'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Temperature ',tfcm,' deg.C'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Heat stored (future time row)',
     &                                  spcm(icomp,isurf,inode-1),' J'
            call edisp(itu,outs)
         elseif(iphnl.eq.2)then
            write(outs,'(a,i3)') 'Middle node (',inode,')'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Temperature ',tfcm,' deg.C'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Heat stored (future time row)',
     &                                  spcm(icomp,isurf,inode),' J'
            call edisp(itu,outs)
         else
            write(outs,'(a,i3)') 'Intermost node (',inode+1,')'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Temperature ',tfcm,' deg.C'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Heat stored (future time row)',
     &                                  spcm(icomp,isurf,inode+1),' J'
            call edisp(itu,outs)
         endif
      enddo

  999 return
      end

C ***************** SPMCMP54 *****************
C A model of phase change material taken from the PhD thesis of
C Sabine Hoffmann, University of Weimar, September 2006, pp139-140.
C The functions are of the type 'exponential'. The model parameters
C are defined in file databases/mscomp.db1.

      subroutine spmcmp54(icomp,ispmnod)
#include "building.h"

C N.B. All parameters comply with the Fortran implicit naming convention
C except where explicitly redefined.
      common/outin/iuout,iuin
      common/tc/itc,icnt
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/zonspmf/zspmf1(mcom),zspmf2(mcom)
      common/fvalc/tfc(mcom,ms,mn),qfc(mcom)
      common/fvals/tfs(mcom,ms),qfs(mcom)
      common/vthp14/thrmli(mcom,ms,me,7)
      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp)
      common/prec13/c(mcom,ms,mn,2),qc(mcom,ms,mn)
c      common/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      common/vthp15/vcp(ms,mn,2),qcp(ms,mn)
      common/vthp16/vcf(ms,mn,2),qcf(ms,mn)
      common/resspm/ndatspm(mspmnod),dataspm(mspmnod,MSPMRES),
     &         strspm(mspmnod,MSPMRES),unitspm(mspmnod,MSPMRES),
     &         txtspm(mspmnod,MSPMRES)

C Special materials.
      common/spmfxst/ispmxist,spflnam
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &              nnodat(mspmnod),spmdat(mspmnod,mspmdat)
      common/spmatlbl/spmlabel(mspmnod)

C Phase change material.
      common/pcm02/spcm(mcom,ms,mn),spcmtf,spcmtp
      common/pcm03/pcmfac(mcom,ms,mn),pcmshts(mcom,ms,mn)

      character spflnam*72,spmlabel*16,outs*124,strspm*16,unitspm*16,
     &          txtspm*72

C PCM layer pointers.
      if(icomp.ne.ispmloc(ispmnod,1))then
         write(outs,*)'SPMAT54 fatal error: incorrect zone assignment!'
         call edisp(iuout,outs)
         stop
      endif
      isurf=ispmloc(ispmnod,2)
      inode=ispmloc(ispmnod,3)
      ielem=nint(real(inode)/2.)

C Melting and solidification temperatures (degC).
      tmelt=spmdat(ispmnod,1)
      tsoli=spmdat(ispmnod,2)

C Conductivity in solid and liquid state (W/m.degC).
      consol=spmdat(ispmnod,3)
      conliq=spmdat(ispmnod,4)

C Specific heat capacity in solid and liquid state (J/kg.degC).
      shtsol=spmdat(ispmnod,5)
      sht2=spmdat(ispmnod,6)

C Latent heat of the PCM (J/kg.degC).
      shtl=spmdat(ispmnod,7)
      shtb=0.76224
      shta=-shtb*tsoli+1.00002397*log(shtl)-0.271249489
      c1=exp(shta)

C Maximum value of latent heat, shtliq (J/kg), i.e. integration
C of c(t) equation from tmelt to tsoli.
      shtliq=(c1/shtb)*(exp(shtb*tsoli)-exp(shtb*tmelt))/1000

C Initialisations for xml output.
      Rtot = 0.0
      THKsum = 0.0
      sumSHTTHK = 0.0

C Process the 3 nodes of the PCM layer.
      spcmtf=0.0
      do 10 iphnl=1,3
         if(iphnl.eq.1)then
            nloc=inode-1
            efthk=thrmli(icomp,isurf,ielem,4)*0.25
         elseif(iphnl.eq.2)then
            nloc=inode
            efthk=thrmli(icomp,isurf,ielem,4)*0.5
         elseif(iphnl.eq.3)then
            nloc=inode+1
            efthk=thrmli(icomp,isurf,ielem,4)*0.25
         endif

C Total thickness of PCM.
         THKsum=THKsum+efthk

C Establish temperature of the PCM material node from tfc if the node
C is intra-construction and tfs if located at the internal surface.
         if(iphnl.eq.3.and.ielem.eq.nelts(icomp,isurf))then
            tfcm=tfs(icomp,isurf)
         else
            tfcm=tfc(icomp,isurf,nloc)
         endif

C If phase change is underway, calculate the new value of
C conductivity (W/m.degC):
C  appcon = consol          for tfcm < tmelt (solid phase);  
C         = conliq          for tfcm > tsoli (liquid phase); 
C         = (spcmc/shtliq)*conliq+[(shtliq-spcmc)/shtliq]*consol
C           where spcmc=[(c1/shtb)*exp(shtb*tfcm)-(c1/shtb)*exp(shtb*tmelt)]/1000
C                shtliq=(c1/shtb)*[exp(shtb*tsoli)-exp(shtb*tmelt)]/1000
C                           for tmelt < tfcm < tsoli (mixed phase);
C ... and apparent heat capacity (J/kg.degC):
C  appsht = shtsol          for tfcm < tmelt (solid phase); 
C         = sht2            for tfcm > tsoli (liquid phase); 
C         = c1*exp(shtb*tfcm)+shtsol
C                           for tmelt < tfcm < tsoli (mixed phase).
         if(tfcm.lt.tmelt)then
            appcon=consol
            appsht=shtsol
         elseif(tfcm.gt.tsoli)then
            appcon=conliq
            appsht=sht2
         else
            spcmc=spcm(icomp,isurf,nloc)
            appcon=(spcmc/shtliq)*conliq+((shtliq-spcmc)/shtliq)*consol
            appsht=c1*exp(shtb*tfcm)+shtsol
         endif

C Parameters for xml output.
         Rtot=Rtot+efthk/appcon
         sumSHTTHK=sumSHTTHK+appsht

C Establish present value of factp and pcmshtp as last used value
C of factf and pcmsht respectively.
         factp=pcmfac(icomp,isurf,nloc)
         pcmshtp=pcmshts(icomp,isurf,nloc)
         if(nsinc.eq.1)then
            factp=0
         endif

C Calculate new factf; this is used below to remove original values
C of PCM layer conductivity and specific heat as used to establish the
C self- and cross- coupling coefficients of the nodal energy balance
C equations, and substitute the new values as established above.
         pcmcon=appcon/consol
         pcmsht=appsht/shtsol
         factf=pcmcon/pcmsht

C Modify present and future value of 'c' and 'qc' coefficients (i.e.
C coeficients of nodal energy balance equations.
         x1=1./pcmshtp
         if(nsinc.eq.1)then
            x1=0
         endif
         x2=1./pcmsht
         if(iphnl.eq.1.and.ielem.eq.1)then			!- outermost node
            vcp(isurf,nloc,1)=c(icomp,isurf,nloc,1)*x1
            vcp(isurf,nloc,2)=c(icomp,isurf,nloc,2)*factp
            qcp(isurf,nloc)=qc(icomp,isurf,nloc)*x1
            vcf(isurf,nloc,1)=c(icomp,isurf,nloc,1)*x2
            vcf(isurf,nloc,2)=c(icomp,isurf,nloc,2)*factf
            qcf(isurf,nloc)=qc(icomp,isurf,nloc)*x2
         elseif(iphnl.eq.2)then					!- centre node
            vcp(isurf,nloc,1)=c(icomp,isurf,nloc,1)*factp
            vcp(isurf,nloc,2)=c(icomp,isurf,nloc,2)*factp
            qcp(isurf,nloc)=qc(icomp,isurf,nloc)*x1
            vcf(isurf,nloc,1)=c(icomp,isurf,nloc,1)*factf
            vcf(isurf,nloc,2)=c(icomp,isurf,nloc,2)*factf
            qcf(isurf,nloc)=qc(icomp,isurf,nloc)*x2
         elseif(iphnl.eq.3.and.ielem.eq.nelts(icomp,isurf))then	!- internal surface node
            vcp(isurf,nloc,1)=c(icomp,isurf,nloc,1)*factp
            vcp(isurf,nloc,2)=c(icomp,isurf,nloc,2)*x1
            qcp(isurf,nloc)=qc(icomp,isurf,nloc)*x1
            vcf(isurf,nloc,1)=c(icomp,isurf,nloc,1)*factf
            vcf(isurf,nloc,2)=c(icomp,isurf,nloc,2)*x2
            qcf(isurf,nloc)=qc(icomp,isurf,nloc)*x2
         elseif(iphnl.eq.1.or.iphnl.eq.3)then			!- interface node

C Redefine modification factors to take account of weighted node
C thermal capacity.
            if(iphnl.eq.1)then
               iouter=ielem
               iinner=ielem-1
            else
               iinner=ielem
               iouter=ielem+1
            endif

C Original averaged capacity before adjustment.       
            capouter=thrmli(icomp,isurf,iouter,2)*
     &                               thrmli(icomp,isurf,iouter,3)*
     &                               (thrmli(icomp,isurf,iouter,4)/2.)
            capinner=thrmli(icomp,isurf,ielem-1,2)*
     &                               thrmli(icomp,isurf,iinner,3)*
     &                               (thrmli(icomp,isurf,iinner,4)/2.)

C Cater for an adjacent air gap.
            ians1=0
            ians2=0
            if(ngaps(icomp,isurf).ne.0)then
               if(iphnl.eq.1)then
                  do igap=1,ngaps(icomp,isurf)
                     if(iinner.eq.npgap(icomp,isurf,igap))then
                        capinner=1.3*1005.5*
     &                               (thrmli(icomp,isurf,iinner,4)/2.)
                        ians1=1
                     endif
                  enddo
               elseif(iphnl.eq.3)then
                  do igap=1,ngaps(icomp,isurf)
                     if(iouter.eq.npgap(icomp,isurf,igap))then
                        capouter=1.3*1005.5*
     &                               (thrmli(icomp,isurf,iouter,4)/2.)
                        ians2=1
                     endif
                  enddo
               endif
            endif
            cap=capouter+capinner

C Average capacity utilising new PCM apparent specific heat.
            capouter=thrmli(icomp,isurf,iouter,2)*
     &                               appsht*
     &                               (thrmli(icomp,isurf,iouter,4)/2.)
            capinner=thrmli(icomp,isurf,iinner,2)*
     &                               thrmli(icomp,isurf,iinner,3)*
     &                               (thrmli(icomp,isurf,iinner,4)/2.)
            if(iphnl.eq.1.and.ians1.eq.1)then
               capinner=1.3*1005.5*(thrmli(icomp,isurf,iinner,4)/2.)
            elseif(iphnl.eq.3.and.ians2.eq.1)then
               capouter=1.3*1005.5*(thrmli(icomp,isurf,iouter,4)/2.)
            endif
            capPCM=capouter+capinner

C capPCM value at previous time step.
            capouter=thrmli(icomp,isurf,iouter,2)*
     &                               pcmshtp*shtsol*
     &                               (thrmli(icomp,isurf,iouter,4)/2.)
            capinner=thrmli(icomp,isurf,iinner,2)*
     &                               thrmli(icomp,isurf,iinner,3)*
     &                               (thrmli(icomp,isurf,iinner,4)/2.)
            if(iphnl.eq.1.and.ians1.eq.1)then
               capinner=1.3*1005.5*(thrmli(icomp,isurf,iinner,4)/2.)
            elseif(iphnl.eq.3.and.ians2.eq.1)then
               capouter=1.3*1005.5*(thrmli(icomp,isurf,iouter,4)/2.)
            endif
            capPCMp=capouter+capinner

            x1=cap/capPCMp
            x2=cap/capPCM
            xfactp=factp*pcmshtp*x1
            xfactf=pcmcon*x2
            if(iphnl.eq.3)then
               vcp(isurf,nloc,1)=c(icomp,isurf,nloc,1)*xfactp
               vcp(isurf,nloc,2)=c(icomp,isurf,nloc,2)*x1
               qcp(isurf,nloc)=qc(icomp,isurf,nloc)*x1
               vcf(isurf,nloc,1)=c(icomp,isurf,nloc,1)*xfactf
               vcf(isurf,nloc,2)=c(icomp,isurf,nloc,2)*x2
               qcf(isurf,nloc)=qc(icomp,isurf,nloc)*x2
            elseif(iphnl.eq.1.and.ians1.eq.1)then
               vcp(isurf,nloc,1)=c(icomp,isurf,nloc,1)*x1
               vcp(isurf,nloc,2)=c(icomp,isurf,nloc,2)*xfactp
               qcp(isurf,nloc)=qc(icomp,isurf,nloc)*x1
               vcf(isurf,nloc,1)=c(icomp,isurf,nloc,1)*x2
               vcf(isurf,nloc,2)=c(icomp,isurf,nloc,2)*xfactf
               qcf(isurf,nloc)=qc(icomp,isurf,nloc)*x2
            endif
         endif

C Save factf and pcmsht for use as factp and pcmshtp at next
C time step.
         pcmfac(icomp,isurf,nloc)=factf
         pcmshts(icomp,isurf,nloc)=pcmsht

C Calculate the value of latent heat stored (J) in part of the PCM
C layer represented by node nloc.
         if(tfcm.lt.tmelt)then
            spcmf=0.0
         elseif(tfcm.gt.tsoli)then
            spcmf=shtliq
         else
            spcmf=((c1/shtb)*exp(shtb*tfcm)-
     &                                (c1/shtb)*exp(shtb*tmelt))/1000
         endif

C Save node-related heat stored.
         spcm(icomp,isurf,nloc)=spcmf
10    continue

C Save total PCM heat stored.
      spcmtf=spcm(icomp,isurf,inode-1)+spcm(icomp,isurf,inode)+
     &                                      spcm(icomp,isurf,inode+1)

C Results output.
      ndatspm(ispmnod)=4
      if(nsinc.le.1)then
         strspm(ispmnod,1)='/AppCON'
         unitspm(ispmnod,1)='(W/(m.degC))'
         txtspm(ispmnod,1)='Average conductivity of PCM layer'

         strspm(ispmnod,2)='/SHCtot'
         unitspm(ispmnod,2)='(J/kg)'
         txtspm(ispmnod,2)='Total heat stored in PCM'

         strspm(ispmnod,3)='/SHCarea'
         unitspm(ispmnod,3)='(J/m^2)'
         txtspm(ispmnod,3)='Heat stored per PCM area'

         strspm(ispmnod,4)='/PCMFAC'
         unitspm(ispmnod,4)='(-)'
         txtspm(ispmnod,4)='PCMFAC of middle node'
      endif

      if(Rtot.gt.0)dataspm(ispmnod,1)=THKsum/Rtot      
      if(THKsum.gt.0)dataspm(ispmnod,2)=sumSHTTHK
      dataspm(ispmnod,3)=((spcmtp+spcmtf)/2.0)
      spcmtp=spcmtf
      dataspm(ispmnod,4)=pcmfac(icomp,isurf,inode)      

C As a temporary measure, use zone-related variable, zspmf1, for
C results library storage. Output is limited to one special material
C per zone.
      zspmf1(ispmloc(ispmnod,1))=dataspm(ispmnod,1)

C XML output.
      call PCM_to_h3k(icomp,inode,isurf,ispmnod)

C Trace output.
      if(itc.le.0.or.nsinc.lt.itc.or.ispmxist.EQ.0)goto 999
      if(itrace(19).eq.0.or.nsinc.gt.itcf.or.izntrc(icomp).ne.1)goto999
      write(outs,'(a,a)') 'Trace output from SPMCMP54 for ',
     &                                              spmlabel(ispmnod)
      call edisp(itu,outs)
      write(outs,'(a,5(2x,i4))')
     &        'Affected zone, surface and node: ',ispmloc(ispmnod,1),
     &                       ispmloc(ispmnod,2),inode-1,inode,inode+1
      call edisp(itu,outs)
      call edisp(itu,' ')
      do iphnl=1,3
         if(iphnl.eq.1)then
            write(outs,'(a,i3)') 'Outermost node (',inode-1,')'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Temperature ',tfcm,' deg.C'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Heat stored (future time row)',
     &                                  spcm(icomp,isurf,inode-1),' J'
            call edisp(itu,outs)
         elseif(iphnl.eq.2)then
            write(outs,'(a,i3)') 'Middle node (',inode,')'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Temperature ',tfcm,' deg.C'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Heat stored (future time row)',
     &                                  spcm(icomp,isurf,inode),' J'
            call edisp(itu,outs)
         else
            write(outs,'(a,i3)') 'Intermost node (',inode+1,')'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Temperature ',tfcm,' deg.C'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Heat stored (future time row)',
     &                                  spcm(icomp,isurf,inode+1),' J'
            call edisp(itu,outs)
         endif
      enddo
  999 return
      end

C ***************** SPMCMP55 *****************
C A model of phase change material taken from the PhD thesis of
C Sabine Hoffmann, University of Weimar, September 2006, pp139-140.
C The functions used are of type 'rational'. The model parameters
C are defined in file databases/mscomp.db1

      subroutine spmcmp55(icomp,ispmnod)
#include "building.h"

C N.B. All parameters comply with the Fortran implicit naming convention
C except where explicitly redefined.
      common/outin/iuout,iuin
      common/tc/itc,icnt
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/zonspmf/zspmf1(mcom),zspmf2(mcom)
      common/fvalc/tfc(mcom,ms,mn),qfc(mcom)
      common/fvals/tfs(mcom,ms),qfs(mcom)
      common/vthp14/thrmli(mcom,ms,me,7)
      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp)
      common/prec13/c(mcom,ms,mn,2),qc(mcom,ms,mn)
      common/vthp15/vcp(ms,mn,2),qcp(ms,mn)
      common/vthp16/vcf(ms,mn,2),qcf(ms,mn)
      common/resspm/ndatspm(mspmnod),dataspm(mspmnod,MSPMRES),
     &         strspm(mspmnod,MSPMRES),unitspm(mspmnod,MSPMRES),
     &         txtspm(mspmnod,MSPMRES)

C Special materials.
      common/spmfxst/ispmxist,spflnam
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &              nnodat(mspmnod),spmdat(mspmnod,mspmdat)
      common/spmatlbl/spmlabel(mspmnod)

C Phase change material.
      common/pcm02/spcm(mcom,ms,mn),spcmtf,spcmtp
      common/pcm03/pcmfac(mcom,ms,mn),pcmshts(mcom,ms,mn)

      character spflnam*72,spmlabel*16,outs*124,strspm*16,unitspm*16,
     &          txtspm*72

C PCM layer pointers.
      if(icomp.ne.ispmloc(ispmnod,1))then
         write(outs,*)'SPMAT55 fatal error: incorrect zone assignment!'
         call edisp(iuout,outs)
         stop
      endif
      isurf=ispmloc(ispmnod,2)
      inode=ispmloc(ispmnod,3)
      ielem=nint(real(inode)/2.)

C Melting and solidification temperatures (degC).
      tmelt=spmdat(ispmnod,1)
      tsoli=spmdat(ispmnod,2)

C Conductivity in solid and liquid state (W/m.degC).
      consol=spmdat(ispmnod,3)
      conliq=spmdat(ispmnod,4)

C Specific heat capacity in solid and liquid state (J/kg.degC)
      shtsol=spmdat(ispmnod,5)
      sht2=spmdat(ispmnod,6)

C Coefficients for apparent heat equation: c(T)=(a+cT+eT**2)/(1+bT+dT**2)
      shta=spmdat(ispmnod,7)
      shtb=spmdat(ispmnod,8)
      shtc=spmdat(ispmnod,9)
      shtd=spmdat(ispmnod,10)
      shte=spmdat(ispmnod,11)

C Time invariant coefficients for integration of c(T) equation.
      c1=2*shta*(shtd**2)+(shtb**2)*shte-shtb*shtc*shtd-2*shtd*shte
      c2=2*shtd/sqrt((-(shtb**2)+4*shtd))
      c3=shtb/(sqrt(-shtb**2+4*shtd))
      c4=(shtd**2)*(sqrt(-shtb**2+4*shtd))
      c5=(shtc*shtd-shtb*shte)
      c6=shtb
      c7=shtd
      c8=2*(shtd**2)
      c9=shte/shtd

C Maximum value of latent heat, shtliq (J/kg), i.e. integration
C of c(t) equation from tmelt to tsoli.
      shtliq1=((c1*atan(c2*tsoli+c3))/c4)-((c1*atan(c2*tmelt+c3))/c4)
      shtliq2=((c5*log(1.0E+0+c6*tsoli+c7*(tsoli**2.0)))/c8)-
     &        ((c5*log(1.0E+0+c6*tmelt+c7*(tmelt**2.0)))/c8)
      shtliq3=c9*(tsoli-tmelt)
      shtliq=(shtliq1+shtliq2+shtliq3)/1.0E+03

C Initialisations for xml output.
      Rtot = 0.0
      THKsum = 0.0
      sumSHTTHK = 0.0

C Process the 3 nodes of the PCM layer.
      spcmtf=0.0
      do 10 iphnl=1,3
         if(iphnl.eq.1)then
            nloc=inode-1
            efthk=thrmli(icomp,isurf,ielem,4)*0.25
         elseif(iphnl.eq.2)then
            nloc=inode
            efthk=thrmli(icomp,isurf,ielem,4)*0.5
         elseif(iphnl.eq.3)then
            nloc=inode+1
            efthk=thrmli(icomp,isurf,ielem,4)*0.25
         endif

C Total thickness of PCM.
         THKsum=THKsum+efthk

C Establish temperature of the PCM material node from tfc if the node
C is intra-construction and tfs if located at the internal surface.
         if(iphnl.eq.3.and.ielem.eq.nelts(icomp,isurf))then
            tfcm=tfs(icomp,isurf)
         else
            tfcm=tfc(icomp,isurf,nloc)
         endif

C Current value of conductivity and specific heat:
         if(tfcm.lt.tmelt)then
            appcon=consol
            appsht=shtsol
         elseif(tfcm.gt.tsoli)then
            appcon=conliq
            appsht=sht2
         else
            spcmc=spcm(icomp,isurf,nloc)
            appcon=(spcmc/shtliq)*conliq+((shtliq-spcmc)/shtliq)*consol
            appsht=((shta+shtc*tfcm+shte*(tfcm**2))
     &                      /(1.0E0+shtb*tfcm+shtd*(tfcm**2)))+shtsol
         endif

C Parameters for xml output.
         Rtot=Rtot+efthk/appcon
         sumSHTTHK=sumSHTTHK+appsht

C Establish present value of factp and pcmshtp as last used value
C of factf and pcmsht respectively.
         factp=pcmfac(icomp,isurf,nloc)
         pcmshtp=pcmshts(icomp,isurf,nloc)
         if(nsinc.eq.1)then
            factp=0
         endif

C Calculate new factf; this is used below to remove original values
C of PCM layer conductivity and specific heat as used to establish the
C self- and cross- coupling coefficients of the nodal energy balance
C equations, and substitute the new values as established above. An
C adjusted factf is required where the PCM node separates the PCM layer
C from a non-PCM layer.
         pcmcon=appcon/consol
         pcmsht=appsht/shtsol
         factf=pcmcon/pcmsht

C Modify present and future value of 'c' and 'qc' coefficients (i.e.
C coeficients of nodal energy balance equations.
         x1=1./pcmshtp
         if(nsinc.eq.1)then
            x1=0
         endif
         x2=1./pcmsht
         if(iphnl.eq.1.and.ielem.eq.1)then			!- outermost node
            vcp(isurf,nloc,1)=c(icomp,isurf,nloc,1)*x1
            vcp(isurf,nloc,2)=c(icomp,isurf,nloc,2)*factp
            qcp(isurf,nloc)=qc(icomp,isurf,nloc)*x1
            vcf(isurf,nloc,1)=c(icomp,isurf,nloc,1)*x2
            vcf(isurf,nloc,2)=c(icomp,isurf,nloc,2)*factf
            qcf(isurf,nloc)=qc(icomp,isurf,nloc)*x2
         elseif(iphnl.eq.2)then					!- centre node
            vcp(isurf,nloc,1)=c(icomp,isurf,nloc,1)*factp
            vcp(isurf,nloc,2)=c(icomp,isurf,nloc,2)*factp
            qcp(isurf,nloc)=qc(icomp,isurf,nloc)*x1
            vcf(isurf,nloc,1)=c(icomp,isurf,nloc,1)*factf
            vcf(isurf,nloc,2)=c(icomp,isurf,nloc,2)*factf
            qcf(isurf,nloc)=qc(icomp,isurf,nloc)*x2
         elseif(iphnl.eq.3.and.ielem.eq.nelts(icomp,isurf))then	!- internal surface node
            vcp(isurf,nloc,1)=c(icomp,isurf,nloc,1)*factp
            vcp(isurf,nloc,2)=c(icomp,isurf,nloc,2)*x1
            qcp(isurf,nloc)=qc(icomp,isurf,nloc)*x1
            vcf(isurf,nloc,1)=c(icomp,isurf,nloc,1)*factf
            vcf(isurf,nloc,2)=c(icomp,isurf,nloc,2)*x2
            qcf(isurf,nloc)=qc(icomp,isurf,nloc)*x2
         elseif(iphnl.eq.1.or.iphnl.eq.3)then			!- interface node

C Redefine modification factors to take account of weighted node
C thermal capacity.
            if(iphnl.eq.1)then
               iouter=ielem
               iinner=ielem-1
            else
               iinner=ielem
               iouter=ielem+1
            endif

C Original averaged capacity before adjustment.       
            capouter=thrmli(icomp,isurf,iouter,2)*
     &                               thrmli(icomp,isurf,iouter,3)*
     &                               (thrmli(icomp,isurf,iouter,4)/2.)
            capinner=thrmli(icomp,isurf,ielem-1,2)*
     &                               thrmli(icomp,isurf,iinner,3)*
     &                               (thrmli(icomp,isurf,iinner,4)/2.)

C Cater for an adjacent air gap.
            ians1=0
            ians2=0
            if(ngaps(icomp,isurf).ne.0)then
               if(iphnl.eq.1)then
                  do igap=1,ngaps(icomp,isurf)
                     if(iinner.eq.npgap(icomp,isurf,igap))then
                        capinner=1.3*1005.5*
     &                               (thrmli(icomp,isurf,iinner,4)/2.)
                        ians1=1
                     endif
                  enddo
               elseif(iphnl.eq.3)then
                  do igap=1,ngaps(icomp,isurf)
                     if(iouter.eq.npgap(icomp,isurf,igap))then
                        capouter=1.3*1005.5*
     &                               (thrmli(icomp,isurf,iouter,4)/2.)
                        ians2=1
                     endif
                  enddo
               endif
            endif
            cap=capouter+capinner

C Average capacity utilising new PCM apparent specific heat.
            capouter=thrmli(icomp,isurf,iouter,2)*
     &                               appsht*
     &                               (thrmli(icomp,isurf,iouter,4)/2.)
            capinner=thrmli(icomp,isurf,iinner,2)*
     &                               thrmli(icomp,isurf,iinner,3)*
     &                               (thrmli(icomp,isurf,iinner,4)/2.)
            if(iphnl.eq.1.and.ians1.eq.1)then
               capinner=1.3*1005.5*(thrmli(icomp,isurf,iinner,4)/2.)
            elseif(iphnl.eq.3.and.ians2.eq.1)then
               capouter=1.3*1005.5*(thrmli(icomp,isurf,iouter,4)/2.)
            endif
            capPCM=capouter+capinner

C capPCM value at previous time step.
            capouter=thrmli(icomp,isurf,iouter,2)*
     &                               pcmshtp*shtsol*
     &                               (thrmli(icomp,isurf,iouter,4)/2.)
            capinner=thrmli(icomp,isurf,iinner,2)*
     &                               thrmli(icomp,isurf,iinner,3)*
     &                               (thrmli(icomp,isurf,iinner,4)/2.)
            if(iphnl.eq.1.and.ians1.eq.1)then
               capinner=1.3*1005.5*(thrmli(icomp,isurf,iinner,4)/2.)
            elseif(iphnl.eq.3.and.ians2.eq.1)then
               capouter=1.3*1005.5*(thrmli(icomp,isurf,iouter,4)/2.)
            endif
            capPCMp=capouter+capinner

            x1=cap/capPCMp
            x2=cap/capPCM
            xfactp=factp*pcmshtp*x1
            xfactf=pcmcon*x2
            if(iphnl.eq.3)then
               vcp(isurf,nloc,1)=c(icomp,isurf,nloc,1)*xfactp
               vcp(isurf,nloc,2)=c(icomp,isurf,nloc,2)*x1
               qcp(isurf,nloc)=qc(icomp,isurf,nloc)*x1
               vcf(isurf,nloc,1)=c(icomp,isurf,nloc,1)*xfactf
               vcf(isurf,nloc,2)=c(icomp,isurf,nloc,2)*x2
               qcf(isurf,nloc)=qc(icomp,isurf,nloc)*x2
            elseif(iphnl.eq.1.and.ians1.eq.1)then
               vcp(isurf,nloc,1)=c(icomp,isurf,nloc,1)*x1
               vcp(isurf,nloc,2)=c(icomp,isurf,nloc,2)*xfactp
               qcp(isurf,nloc)=qc(icomp,isurf,nloc)*x1
               vcf(isurf,nloc,1)=c(icomp,isurf,nloc,1)*x2
               vcf(isurf,nloc,2)=c(icomp,isurf,nloc,2)*xfactf
               qcf(isurf,nloc)=qc(icomp,isurf,nloc)*x2
            endif
         endif

C Save factf and pcmsht for use as factp and pcmshtp at next
C time step.
         pcmfac(icomp,isurf,nloc)=factf
         pcmshts(icomp,isurf,nloc)=pcmsht

C Calculate the value of latent heat stored (J) in part of the PCM
C layer represented by node nloc.
         if(tfcm.lt.tmelt)then
            spcmf=0.0
         elseif(tfcm.gt.tsoli)then
            spcmf=shtliq
         else
            spcmf1=(c1*atan(c2*tfcm+c3))/c4-(c1*atan(c2*tmelt+c3))/c4
            spcmf2=(c5*log(1.0E+0+c6*tfcm+c7*(tfcm**2.0)))/c8-
     &                 (c5*log(1.0E+0+c6*tmelt+c7*(tmelt**2.0)))/c8
            spcmf3=c9*tfcm-c9*tmelt
            spcmf4=spcmf1+spcmf2+spcmf3
            spcmf=spcmf4/1.0E+03
         endif

C Save node-related heat stored.
         spcm(icomp,isurf,nloc)=spcmf
10    continue

C Save total PCM heat stored.
      spcmtf=spcm(icomp,isurf,inode-1)+spcm(icomp,isurf,inode)+
     &                                      spcm(icomp,isurf,inode+1)

C Results output.
      ndatspm(ispmnod)=4
      if(nsinc.le.1)then
         strspm(ispmnod,1)='/AppCON'
         unitspm(ispmnod,1)='(W/(m.degC))'
         txtspm(ispmnod,1)='Average conductivity of PCM layer'

         strspm(ispmnod,2)='/SHCtot'
         unitspm(ispmnod,2)='(J/kg)'
         txtspm(ispmnod,2)='Tot. heat stored in PCM'

         strspm(ispmnod,3)='/SHCarea'
         unitspm(ispmnod,3)='(J/m2)'
         txtspm(ispmnod,3)='Heat stored per PCM area'

         strspm(ispmnod,4)='/PCMFAC'
         unitspm(ispmnod,4)='(-)'
         txtspm(ispmnod,4)='PCMFAC of middle node'
      endif

      if(Rtot.gt.0)dataspm(ispmnod,1)=THKsum/Rtot  
      if(THKsum.gt.0)dataspm(ispmnod,2)=sumSHTTHK
      dataspm(ispmnod,3)=(spcmtp+spcmtf)/2.0
      spcmtp=spcmtf
      dataspm(ispmnod,4)=pcmfac(icomp,isurf,inode)      

C As a temporary measure, use zone-related variables, zspmf1, for
C results library storage. Output is limited to one special material
C per zone.
      zspmf1(ispmloc(ispmnod,1))=dataspm(ispmnod,1)

C XML output.
      call PCM_to_h3k(icomp,inode,isurf,ispmnod)

C Trace output.
      if(itc.le.0.or.nsinc.lt.itc.or.ispmxist.eq.0)goto 999
      if(itrace(19).eq.0.or.nsinc.gt.itcf.or.izntrc(icomp).ne.1)goto999
      write(outs,'(a,a)') 'Trace output from SPMCMP55 for ',
     &                                              spmlabel(ispmnod)
      call edisp(itu,outs)
      write(outs,'(a,5(2x,i4))')
     &        'Affected zone, surface and node: ',ispmloc(ispmnod,1),
     &                       ispmloc(ispmnod,2),inode-1,inode,inode+1
      call edisp(itu,outs)
      call edisp(itu,' ')
      do iphnl=1,3
         if(iphnl.eq.1)then
            write(outs,'(a,i3)') 'Outermost node (',inode-1,')'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Temperature ',tfcm,' deg.C'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Heat stored (future time row)',
     &                                  spcm(icomp,isurf,inode-1),' J'
            call edisp(itu,outs)
         elseif(iphnl.eq.2)then
            write(outs,'(a,i3)') 'Middle node (',inode,')'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Temperature ',tfcm,' deg.C'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Heat stored (future time row)',
     &                                  spcm(icomp,isurf,inode),' J'
            call edisp(itu,outs)
         else
            write(outs,'(a,i3)') 'Intermost node (',inode+1,')'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Temperature ',tfcm,' deg.C'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Heat stored (future time row)',
     &                                  spcm(icomp,isurf,inode+1),' J'
            call edisp(itu,outs)
         endif
      enddo
  999 return
      end

C ***************** SPMCMP56 *****************
C Phase change material model: extended approach allowing sub-cooling.
C Created by Achim Geissler, July 30 2008. Parameters are defined in
C file databases/mscomp.db1. Calls:
C    LatentHeat(k,T1,T2) - calculate integral of latent heat between
C                          T1 and T2 for k=1 'standard' range and 
C                          k=0 sub-cooled PCM;
C    PCMStore()          - calculate change in stored latent heat,
C                          apparent conduction and apparent specific
C                          heat capacity.

      subroutine spmcmp56(icomp,ispmnod)
#include "building.h"

C N.B. All parameters comply with the Fortran implicit naming convention
C except where explicitly redefined.
      common/outin/iuout,iuin
      common/tc/itc,icnt
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
      common/pvalc/tpc(mcom,ms,mn),qpc(mcom)
      common/pvals/tps(mcom,ms),qps(mcom)
      common/fvalc/tfc(mcom,ms,mn),qfc(mcom)
      common/fvals/tfs(mcom,ms),qfs(mcom)
      common/prec9/nconst(mcom),nelts(mcom,ms),ngaps(mcom,ms),
     &             npgap(mcom,ms,mgp)
      common/prec13/c(mcom,ms,mn,2),qc(mcom,ms,mn)
      common/vthp14/thrmli(mcom,ms,me,7)
      common/vthp15/vcp(ms,mn,2),qcp(ms,mn)
      common/vthp16/vcf(ms,mn,2),qcf(ms,mn)
      common/zonspmf/zspmf1(mcom),zspmf2(mcom)
      common/resspm/ndatspm(mspmnod),dataspm(mspmnod,MSPMRES),
     &         strspm(mspmnod,MSPMRES),unitspm(mspmnod,MSPMRES),
     &         txtspm(mspmnod,MSPMRES)

C Special materials.
      common/spmfxst/ispmxist,spflnam
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &              nnodat(mspmnod),spmdat(mspmnod,mspmdat)
      common/spmatlbl/spmlabel(mspmnod)

C Phase change materials.
      common/pcm02/spcm(mcom,ms,mn),spcmtf,spcmtp
      common/pcm03/pcmfac(mcom,ms,mn),pcmshts(mcom,ms,mn)
      common/pcm05/appconp(mcom,ms,mn),appshcp(mcom,ms,mn),
     &                                                 pk(mcom,ms,mn)
      integer pk	!- Present value of switch 'k' (-).

      common/pcmparm/tsoli,tmelt,sub,Lges,consol,conliq,shcsol,shcliq,
     &               a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2,LHSimple
      real Lges		!- Total latent heat capacity of PCM (J/kg).
      logical LHSimple	!- .true. is previous simple model, .false. is
			!  extended model.
C Local variables.
      real LayFrac(3)
      integer presk
      integer db(3)	!- Debug variable, stores 'case' of PCM heating/cooling.

C Data.
      data LayFrac /0.25, 0.5, 0.25/	!- PCM split into 3 layers.

      character spflnam*72,spmlabel*16,outs*124,strspm*16,unitspm*16,
     &          txtspm*72

C PCM location.
      if(icomp.ne.ispmloc(ispmnod,1))then
         write(outs,*)'SPMAT56 fatal error: incorrect zone assignment!'
         call edisp(iuout,outs)
         stop
      endif
      isurf=ispmloc(ispmnod,2)
      inode=ispmloc(ispmnod,3)
      ielem=nint(real(inode)/2.)

C Melting and solidification temperatures (degC), and sub-cooling
C temperature difference (degC).
      tmelt=spmdat(ispmnod,1)
      tsoli=spmdat(ispmnod,2)
      sub=spmdat(ispmnod,3)

C Conductivity in solid and liquid phase (W/m.K).
      consol=spmdat(ispmnod,4)
      conliq=spmdat(ispmnod,5)

C Specific heat capacity in solid and liquid phase (J/kg.K)
      shcsol=spmdat(ispmnod,6)
      shcliq=spmdat(ispmnod,7)

C Total latent heat capacity of PCM (J/kg).
      Lges=spmdat(ispmnod,8)

      if(spmdat(ispmnod,9).eq.0)then
         LHSimple=.true.
      else 
         LHSimple=.false.
      endif

C Coefficients for non-subcooling (a1-f1) and sub-cooling (a2-f2) integral
C of latent heat - lh(t1,t2)=exp(a)/b*(exp(b*T1)-exp(b*t2)) - and latent
C heat function from c(T)=(a+cT+eT**2)/(1+bT+dT**2) extended to
C (t**f)*c(t) (see thesis by Sabine Hoffmann).
      a1=spmdat(ispmnod,10)
      b1=spmdat(ispmnod,11)
      c1=spmdat(ispmnod,12)
      d1=spmdat(ispmnod,13)
      e1=spmdat(ispmnod,14)
      f1=spmdat(ispmnod,15)
      a2=spmdat(ispmnod,16)
      b2=spmdat(ispmnod,17)
      c2=spmdat(ispmnod,18)
      d2=spmdat(ispmnod,19)
      e2=spmdat(ispmnod,20)
      f2=spmdat(ispmnod,21)

C Initialisations.
      if(nsinc.le.1)then
         do iphnl=1,3
            nloc=inode-2+iphnl
            pk(icomp,isurf,nloc)=1
            spcm(icomp,isurf,nloc)=0.0
            appconp(icomp,isurf,nloc)=consol
            appshcp(icomp,isurf,nloc)=shcsol
         enddo
      endif

C Initialisations for xml output.
      Rtot = 0.0
      aveAppSHC= 0.0
      sumStoredLH = 0.0

C Process the 3 nodes of the PCM layer.
      do 10 iphnl=1,3
         nloc=inode-2+iphnl
         efthk=thrmli(icomp,isurf,ielem,4)*LayFrac(iphnl)

C Establish temperature of the PCM material node from tfc if the node
C is intra-construction and tfs if located at the internal surface.
         if(iphnl.eq.3.and.ielem.eq.nelts(icomp,isurf))then
            tfcm=tfs(icomp,isurf)
            tpcm=tps(icomp,isurf)
         else
            tfcm=tfc(icomp,isurf,nloc)
            tpcm=tpc(icomp,isurf,nloc)
         endif

C Retrieve stored latent heat, apparent conductivity and apparent
C specific heat from previous time step.
         storedLH=spcm(icomp,isurf,nloc)
         appcon=appconp(icomp,isurf,nloc)
         appshc=appshcp(icomp,isurf,nloc)
         presk=pk(icomp,isurf,nloc)

C Calculated change in stored latent heat, apparent conduction and
C apparent specific heat capacity based on present and future 
C temperatures and state, 'k', of PCM.
         call PCMStore(tpcm,tfcm,presk,storedLH,appcon,appshc,
     &                                                     db(iphnl))

C Parameters for xml output.
         Rtot=Rtot+efthk/appcon
         aveAppSHC=aveAppSHC+appshc*LayFrac(iphnl)
         sumStoredLH=sumStoredLH+storedLH*LayFrac(iphnl)

C Establish present value of factp and pcmshtp as last used value
C of factf and pcmsht respectively.
         factp=pcmfac(icomp,isurf,nloc)
         pcmshtp=pcmshts(icomp,isurf,nloc)
         if(nsinc.eq.1)then
            factp=0
         endif

C Calculate new factf; this is used below to remove original values
C of PCM layer conductivity and specific heat as used to establish the
C self- and cross- coupling coefficients of the nodal energy balance
C equations, and substitute the new values as established above. An
C adjusted factf is required where the PCM node separates the PCM layer
C from a non-PCM layer.
         pcmcon=appcon/consol
         pcmsht=appshc/shcsol
         factf=pcmcon/pcmsht

C Modify present and future value of 'c' and 'qc' coefficients (i.e.
C coeficients of nodal energy balance equations.
         x1=1./pcmshtp
         if(nsinc.eq.1)then
            x1=0
         endif
         x2=1./pcmsht
         if(iphnl.eq.1.and.ielem.eq.1)then			!- outermost node
            vcp(isurf,nloc,1)=c(icomp,isurf,nloc,1)*x1
            vcp(isurf,nloc,2)=c(icomp,isurf,nloc,2)*factp
            qcp(isurf,nloc)=qc(icomp,isurf,nloc)*x1
            vcf(isurf,nloc,1)=c(icomp,isurf,nloc,1)*x2
            vcf(isurf,nloc,2)=c(icomp,isurf,nloc,2)*factf
            qcf(isurf,nloc)=qc(icomp,isurf,nloc)*x2
         elseif(iphnl.eq.2)then					!- centre node
            vcp(isurf,nloc,1)=c(icomp,isurf,nloc,1)*factp
            vcp(isurf,nloc,2)=c(icomp,isurf,nloc,2)*factp
            qcp(isurf,nloc)=qc(icomp,isurf,nloc)*x1
            vcf(isurf,nloc,1)=c(icomp,isurf,nloc,1)*factf
            vcf(isurf,nloc,2)=c(icomp,isurf,nloc,2)*factf
            qcf(isurf,nloc)=qc(icomp,isurf,nloc)*x2
         elseif(iphnl.eq.3.and.ielem.eq.nelts(icomp,isurf))then	!- internal surface node
            vcp(isurf,nloc,1)=c(icomp,isurf,nloc,1)*factp
            vcp(isurf,nloc,2)=c(icomp,isurf,nloc,2)*x1
            qcp(isurf,nloc)=qc(icomp,isurf,nloc)*x1
            vcf(isurf,nloc,1)=c(icomp,isurf,nloc,1)*factf
            vcf(isurf,nloc,2)=c(icomp,isurf,nloc,2)*x2
            qcf(isurf,nloc)=qc(icomp,isurf,nloc)*x2
         elseif(iphnl.eq.1.or.iphnl.eq.3)then			!- interface node

C Redefine modification factors to take account of weighted node
C thermal capacity.
            if(iphnl.eq.1)then
               iouter=ielem
               iinner=ielem-1
            else
               iinner=ielem
               iouter=ielem+1
            endif

C Original averaged capacity before adjustment.       
            capouter=thrmli(icomp,isurf,iouter,2)*
     &                               thrmli(icomp,isurf,iouter,3)*
     &                               (thrmli(icomp,isurf,iouter,4)/2.)
            capinner=thrmli(icomp,isurf,ielem-1,2)*
     &                               thrmli(icomp,isurf,iinner,3)*
     &                               (thrmli(icomp,isurf,iinner,4)/2.)

C Cater for an adjacent air gap.
            ians1=0
            ians2=0
            if(ngaps(icomp,isurf).ne.0)then
               if(iphnl.eq.1)then
                  do igap=1,ngaps(icomp,isurf)
                     if(iinner.eq.npgap(icomp,isurf,igap))then
                        capinner=1.3*1005.5*
     &                               (thrmli(icomp,isurf,iinner,4)/2.)
                        ians1=1
                     endif
                  enddo
               elseif(iphnl.eq.3)then
                  do igap=1,ngaps(icomp,isurf)
                     if(iouter.eq.npgap(icomp,isurf,igap))then
                        capouter=1.3*1005.5*
     &                               (thrmli(icomp,isurf,iouter,4)/2.)
                        ians2=1
                     endif
                  enddo
               endif
            endif
            cap=capouter+capinner

C Average capacity utilising new PCM apparent specific heat.
            capouter=thrmli(icomp,isurf,iouter,2)*
     &                               appshc*
     &                               (thrmli(icomp,isurf,iouter,4)/2.)
            capinner=thrmli(icomp,isurf,iinner,2)*
     &                               thrmli(icomp,isurf,iinner,3)*
     &                               (thrmli(icomp,isurf,iinner,4)/2.)
            if(iphnl.eq.1.and.ians1.eq.1)then
               capinner=1.3*1005.5*(thrmli(icomp,isurf,iinner,4)/2.)
            elseif(iphnl.eq.3.and.ians2.eq.1)then
               capouter=1.3*1005.5*(thrmli(icomp,isurf,iouter,4)/2.)
            endif
            capPCM=capouter+capinner

C capPCM value at previous time step.
            capouter=thrmli(icomp,isurf,iouter,2)*
     &                               pcmshtp*shcsol*
     &                               (thrmli(icomp,isurf,iouter,4)/2.)
            capinner=thrmli(icomp,isurf,iinner,2)*
     &                               thrmli(icomp,isurf,iinner,3)*
     &                               (thrmli(icomp,isurf,iinner,4)/2.)
            if(iphnl.eq.1.and.ians1.eq.1)then
               capinner=1.3*1005.5*(thrmli(icomp,isurf,iinner,4)/2.)
            elseif(iphnl.eq.3.and.ians2.eq.1)then
               capouter=1.3*1005.5*(thrmli(icomp,isurf,iouter,4)/2.)
            endif
            capPCMp=capouter+capinner

            x1=cap/capPCMp
            x2=cap/capPCM
            xfactp=factp*pcmshtp*x1
            xfactf=pcmcon*x2
            if(iphnl.eq.3)then
               vcp(isurf,nloc,1)=c(icomp,isurf,nloc,1)*xfactp
               vcp(isurf,nloc,2)=c(icomp,isurf,nloc,2)*x1
               qcp(isurf,nloc)=qc(icomp,isurf,nloc)*x1
               vcf(isurf,nloc,1)=c(icomp,isurf,nloc,1)*xfactf
               vcf(isurf,nloc,2)=c(icomp,isurf,nloc,2)*x2
               qcf(isurf,nloc)=qc(icomp,isurf,nloc)*x2
            elseif(iphnl.eq.1.and.ians1.eq.1)then
               vcp(isurf,nloc,1)=c(icomp,isurf,nloc,1)*x1
               vcp(isurf,nloc,2)=c(icomp,isurf,nloc,2)*xfactp
               qcp(isurf,nloc)=qc(icomp,isurf,nloc)*x1
               vcf(isurf,nloc,1)=c(icomp,isurf,nloc,1)*x2
               vcf(isurf,nloc,2)=c(icomp,isurf,nloc,2)*xfactf
               qcf(isurf,nloc)=qc(icomp,isurf,nloc)*x2
            endif
         endif

C Save factf and pcmsht for use as factp and pcmshtp at next
C time step.
         pcmfac(icomp,isurf,nloc)=factf
         pcmshts(icomp,isurf,nloc)=pcmsht

C Save node-related heat stored.
         spcm(icomp,isurf,nloc)=storedLH

C Save future time row values as present values for next time step.
         appconp(icomp,isurf,nloc)=appcon
         appshcp(icomp,isurf,nloc)=appshc
         pk(icomp,isurf,nloc)=presk
10    continue

C Results output.
      ndatspm(ispmnod)=8
      if(nsinc.le.1)then
         strspm(ispmnod,1)='/AppCON'
         unitspm(ispmnod,1)='(W/(m.degC))'
         txtspm(ispmnod,1)='Apparent conductivity of PCM layer'

         strspm(ispmnod,6)='/APPSHC'
         unitspm(ispmnod,6)='(J/(kg.degC))'
         txtspm(ispmnod,6)='Apparent spec. heat capacity of PCM layer'

         strspm(ispmnod,2)='/StoredLH'
         unitspm(ispmnod,2)='(J/kg)'
         txtspm(ispmnod,2)='Latent heat stored in PCM'

         strspm(ispmnod,3)='/SHCarea'
         unitspm(ispmnod,3)='(J/m2)'
         txtspm(ispmnod,3)='Latent heat stored per PCM area'

         strspm(ispmnod,4)='/PCMFAC'
         unitspm(ispmnod,4)='(-)'
         txtspm(ispmnod,4)='PCMFAC of middle node'

         strspm(ispmnod,5)='/kkk'
         unitspm(ispmnod,5)='(-)'
         txtspm(ispmnod,5)='k for each subnode - outside to inside'

         strspm(ispmnod,7)='/ccc'
         unitspm(ispmnod,7)='(-)'
         txtspm(ispmnod,7)=
     &                 'case for each subnode - outside to inside'

         strspm(ispmnod,8)='/DT_F-P'
         unitspm(ispmnod,8)='(K)'
         txtspm(ispmnod,8)=
     &                 'Temperature difference (T_Future - T_Present)'
      endif

      if(Rtot.gt.0)dataspm(ispmnod,1)=thrmli(icomp,isurf,ielem,4)/Rtot
      dataspm(ispmnod,2)=sumStoredLH 
      dataspm(ispmnod,3)=sumStoredLH*thrmli(icomp,isurf,ielem,2)*
     &                                    thrmli(icomp,isurf,ielem,4)
      dataspm(ispmnod,4)=pcmfac(icomp,isurf,inode)
      dataspm(ispmnod,5)=100*pk(icomp,isurf,inode-1)+
     &                                      10*pk(icomp,isurf,inode)+
     &                                      pk(icomp,isurf,inode+1)
      dataspm(ispmnod,6)=aveAPPSHC
      dataspm(ispmnod,7)=100*db(1)+10*db(2)+db(3)
      dataspm(ispmnod,8)=(tfcm-tpcm)

C As a temporary measure, use zone-related variables, zspmf1, for
C results library storage. Output is limited to one special material
C per zone.
      zspmf1(ispmloc(ispmnod,1))=dataspm(ispmnod,1)

C XML output.
      call PCM_to_h3k(icomp,inode,isurf,ispmnod)

C Trace output.
      if(itc.le.0.or.nsinc.lt.itc.or.ispmxist.eq.0)goto 999
      if(itrace(19).eq.0.or.nsinc.gt.itcf.or.izntrc(icomp).ne.1)goto999
      write(outs,'(a,a)') 'Trace output from spmcmp56 for ',
     &                                              spmlabel(ispmnod)
      call edisp(itu,outs)
      write(outs,'(a,5(2x,i4))')
     &        'Affected zone, surface and node: ',ispmloc(ispmnod,1),
     &                       ispmloc(ispmnod,2),inode-1,inode,inode+1
      call edisp(itu,outs)
      call edisp(itu,' ')
      do iphnl=1,3
         if(iphnl.eq.1)then
            write(outs,'(a,i3)') 'Outermost node (',inode-1,')'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Temperature ',tfcm,' deg.C'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Heat stored (future time row)',
     &                                  spcm(icomp,isurf,inode-1),' J'
            call edisp(itu,outs)
         elseif(iphnl.eq.2)then
            write(outs,'(a,i3)') 'Middle node (',inode,')'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Temperature ',tfcm,' deg.C'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Heat stored (future time row)',
     &                                  spcm(icomp,isurf,inode),' J'
            call edisp(itu,outs)
         else
            write(outs,'(a,i3)') 'Intermost node (',inode+1,')'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Temperature ',tfcm,' deg.C'
            call edisp(itu,outs)
            write(outs,'(a,1x,f10.2)') 'Heat stored (future time row)',
     &                                  spcm(icomp,isurf,inode+1),' J'
            call edisp(itu,outs)
         endif
      enddo
  999 return
      end

C ***************** LatentHeat *****************
C Created by Achim Geissler, July 11 2008.
C This function models PCM materials, which show significant sub-cooling
C properties, i.e. materials that, once melted, must be sub-cooled by
C a few degrees below the conversion temperature at which they are
C completely liquified. The function is called by SPMCPM56 to calculate
C the integral of the stored/released latent heat between start temperature,
C T1, and end temperature, T2:
C          k = normal (1) or sub-cooled (0) range;
C          LHSimple = simple (.true.) or extended function (.false.).

C The returned value is LH, the stored/released latent heat (J/kg); 
C thereby, an energy balance is formulated from the viewpoint of the
C surrounding zone:
C            > 0 when T1 > T2, i.e. heat released;
C            < 0 when T1 < T2, i.e. heat stored.

C The base equations are taken from the PhD thesis of Sabine Hoffmann, 
C University of Weimar, September 2006 ('Advanced' function extended
C by factor T^f).

      real function LatentHeat(k,T1,T2)
      implicit none

      real T1,T2
      integer k

C Phase change material parameters.
      common/pcmparm/tsoli,tmelt,sub,Lges,consol,conliq,shcsol,shcliq,
     &                   a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2,LHSimple
      real tsoli		!- solidification temperature (degC)
      real tmelt		!- melting temperature (degC)
      real sub			!- sub-cooling temperature difference (degC)
      real Lges			!- total latent heat capacity of PCM (J/kg)
      real consol,conliq	!- conductivity of solid and liquid phase (W/(m.K))
      real shcsol,shcliq	!- specific heat of solid and liquid phase (J/(kg.K)
      real a1,b1,c1,d1,e1,f1	!- equation parameters for non-subcooling (a1-f1) 
      real a2,b2,c2,d2,e2,f2	!  and subcooling (a2-f2) states
      logical LHSimple		!- .true. is simple model, .false. is extended model
 
C Local variables.
      real aa,bb,ff,LH_T1,LH_T2,v_func

C Which function?
      if(k.eq.0)then 
         if(LHSimple)then	!- simple function
            aa=a2
            bb=b2
         else			!- extended function                 
            ff=f2		!- aa to ee are set in function v_func()
         endif    
      else			!- function defaults to 'normal' mode
         if(LHSimple)then	!- simple function
            aa=a1
            bb=b1
         else			!- extended function
            ff=f1		!- aa to ee are set in function v_func()	
         endif
      endif

      if(LHSimple)then		!- simple function
         LatentHeat=exp(aa)/bb*(exp(bb*T1)-exp(bb*T2))
      else    

C Calculate functions to build LatentHeat.
         LH_T1=T1**ff*v_func(k,T1)-ff*T1**(ff-1)*v_func(k,T1)  ! integral approximation
         LH_T2=T2**ff*v_func(k,T2)-ff*T2**(ff-1)*v_func(k,T2)
         LatentHeat=(LH_T1-LH_T2)*1000.0   !- change upper and lower boundary 
                                           !  of array to comply with sign
                                           !  definition for LatentHeat and
                                           !  adapt unit from J/g to J/kg
      endif

C More than |Lges| is not possible.
      LatentHeat=sign(1.0,LatentHeat)*min(Lges,abs(LatentHeat))

      return
      end

C ***************** v_func *****************
C Functions for extended latent heat integral.

      real function v_func(k,T)
      implicit none
      
      integer k
      real T
      
C Phase change material parameters.
      common/pcmparm/tsoli,tmelt,sub,Lges,consol,conliq,shcsol,shcliq,
     &                    a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2,LHSimple
      real tsoli		!- solidification temperature (degC)
      real tmelt		!- melting temperature (degC)
      real sub			!- sub-cooling temperature difference (degC)
      real Lges			!- total latent heat capacity of PCM (J/kg)
      real consol,conliq	!- conductivity of solid and liquid phase (W/m.K)
      real shcsol,shcliq	!- specific heat of solid and liquid phase (J/kg.K)
      real a1,b1,c1,d1,e1,f1	!- equation parameters for non-subcooling (a1-f1)
      real a2,b2,c2,d2,e2,f2	!  and subcooling (a2-f2) states
      logical LHSimple		!- .true. is simple model, .false. is extended model

C Local variables.
      REAL aa,bb,cc,dd,ee,CC1,CC2,C3,C4,C5,C6,C7,C8,C9

C Coefficients for extended function.
      if(k.eq.0)then		!- subcooling
         aa=a2
         bb=b2
         cc=c2
         dd=d2
         ee=e2
      else			!- non-subcooling
         aa=a1
         bb=b1
         cc=c1
         dd=d1
         ee=e1
      endif
      
C Calculate coefficients.
      CC1=2.0*aa*(dd**2.0)+(bb**2.0)*ee-bb*cc*dd-2.0*dd*ee
      CC2=2.0*dd/SQRT((-(bb**2.0)+4.0*dd))
      C3=bb/(sqrt(-bb**2.0+4.0*dd))
      C4=(dd**2.0)*(sqrt(-bb**2.0+4.0*dd))
      C5=(cc*dd-bb*ee)
      C6=bb
      C7=dd
      C8=2.0*(dd**2.0)
      C9=ee/dd

      v_func=((CC1*atan(CC2*T+C3))/C4)+
     &                     (C5*log(1.0E0+C6*T+C7*T**2)/C8)+C9*T

      return
      end

C ***************** PCMSTORE *****************
C This routine keeps track of the latent heat stored/released by a
C PCM. The routine is called once each simulation time step and for
C each PCM node.

      subroutine pcmstore(TnS,TnS1,k,storedLH,AppCon,AppSHC,Debug)
      implicit none

C Parameters.
      real TnS
      real TnS1
      integer k		!- sub-cooling switch, value of preceding time
			!  step passed (typically initialised to 1)
      real storedLH	!- stored latent heat, value of preceeding time
			!  step passed (typically initialised to 0)
      real AppCon	!- apparent thermal conductivity
      real AppSHC	!- apparent specific heat capacity
      integer Debug	!- output of 'case' as defined below

C Phase change material parameters.
      common/pcmparm/tsoli,tmelt,sub,Lges,consol,conliq,shcsol,shcliq,
     &                    a1,b1,c1,d1,e1,f1,a2,b2,c2,d2,e2,f2,LHSimple
      real tsoli		!- solidification temperature (degC)
      real tmelt		!- melting temperature (degC)
      real sub			!- sub-cooling temperature difference (degC)
      real Lges			!- total latent heat capacity of PCM (J/kg)
      real consol,conliq	!- conductivity of solid and liquid phase (W/m.K)
      real shcsol,shcliq	!- specific heat of solid and liquid phase (J/(kg.K)
      real a1,b1,c1,d1,e1,f1	!- equation parameters for non-subcooling (a1-f1)
      real a2,b2,c2,d2,e2,f2	!  and subcooling (a2-f2) states
      logical LHSimple		!- .true. is simple model, .false. is extended model

C Local variables.
      real tsoli2		!- sub-cooling solidification temperature (degC)
      real tmelt2		!- sub-cooling melting temperature (degC)
      real storeThisTS		!- latent heat stored in PCM (J/kg)
      real LatentHeat

C Set up sub-cooling temperatures.
      tsoli2=tsoli-sub
      tmelt2=tmelt-sub

C *****************
C Calculate the heat stored according to the temperature change
C between present (TnS) and future (TnS1) temperatures. Calculate also
C the apparent heat capacity and thermal conductivity based on the
C percentage of liquidification of the PCM.

C The following cases are checked/differentiated:
C      heating - cases 1, 2, 3, 4, 5, 6 and 7;
C      cooling - cases 1, 2, 3, 5, 6, 7 and 8.

C          
C         ^ C (J/(kg.K)
C         |                       _____
C         |                      /     \
C         |                     /       \
C         |                    /    7    \
C         |                   /           \
C         |                  /             \ 6   5   tsoli   (5)
C         |      (1)      8 /      tmelt    \---<-->---/----<-->- C_liq
C   C_sol |-----<-->-------/----------\   tsoli2      / 4   
C         |             tmelt2   1   2 \             /
C         |                             \           /
C         |                              \         /  
C         |                               \   3   /
C         |                                \     /
C         |                                 \___/
C         |-------------------------------------------------> T (degC)      
C *****************

      if(TnS.lt.TnS1)then

C PCM temperature increase.
         if(k.eq.1)then

C PCM behaves according to non-subcooling properties.
            if(TnS1.le.tmelt)then

C PCM is solid (case 1), StoredLH = 0.
               AppCon=consol
               AppSHC=shcsol
               Debug=1
            elseif(TnS.le.tmelt.and.TnS1.gt.tmelt)then

C PCM begins to melt (case 2).
               storeThisTS=LatentHeat(1,tmelt,TnS1)
               storedLH=0.0E0-storeThisTS		!- storedLH must be >0
               AppCon=(1.0E0-storedLH/Lges)*consol+
     &                                          storedLH/Lges*conliq
               AppSHC=shcsol+storeThisTS/(tmelt-TnS1)	!- to be verified
               Debug=2
            elseif(TnS.gt.tmelt.and.TnS1.le.tsoli)then

C PCM is partially liquid, latent heat is stored in the normal
C range (k=1, case 3).
               storeThisTS=LatentHeat(1,TnS,TnS1)
               storedLH=storedLH-storeThisTS
               AppCon=(1.0E0-storedLH/Lges)*consol+
     &                                          storedLH/Lges*conliq
               AppSHC=shcliq+storeThisTS/(TnS-TnS1)	!- to be verified
               Debug=3
            elseif(TnS.gt.tmelt.and.TnS1.gt.tsoli)then

C Rest of solid PCM is completely melted and also heated as
C liquid (case 4).
               storeThisTS=LatentHeat(1,TnS,tsoli)
               storedLH=storedLH-storeThisTS
          if(storedLH.gt.Lges)storedLH=Lges
               AppCon=(1.0E0-storedLH/Lges)*consol+
     &                                          storedLH/Lges*conliq
               AppSHC=shcliq+storeThisTS/(TnS-tsoli)	!- to be verified

C Set new k.
               k=0
               Debug=4
            endif		!- temperature bracketing for k = 1
         else			!- k=0

C PCM behaves according to sub-cooling properties.
            if(TnS.gt.tsoli2)then

C PCM is completely liquified and heated further (case 5).
               storedLH=Lges
               AppCon=conliq
               AppSHC=shcliq
               Debug=5
            elseif(TnS1.gt.tsoli2)then

C PCM is sub-cooled partial liquid and is completely
C (re-)melted (case 6).
               storeThisTS=LatentHeat(0,TnS,tsoli2)
               storedLH=storedLH-storeThisTS
          if(storedLH.gt.Lges)storedLH=Lges
               AppCon=(1.0E0-storedLH/Lges)*consol+storedLH/Lges*conliq
               AppSHC=shcliq+storeThisTS/(TnS-tsoli2)	!- to be verified
               Debug=6
            elseif(TnS1.le.tsoli2)then

C PCM is sub-cooled partial liquid and not completely
C melted (k=0, case 7).
               storeThisTS=LatentHeat(0,TnS,TnS1)
               storedLH=storedLH-storeThisTS
               AppCon=(1.0E0-storedLH/Lges)*consol+storedLH/Lges*conliq
               AppSHC=shcliq+storeThisTS/(TnS-TnS1)	!- to be verified
               Debug=7
            endif		!- temperature bracketing for k=0
         endif
      elseif(TnS.gt.TnS1)then

C PCM temperature decrease.
         if(k.eq.1)then

C PCM behaves according to non-subcooling properties.
            if(TnS.le.tmelt)then

C PCM is solid (case 1), StoredLH = 0.
               AppCon=consol
               AppSHC=shcsol
               Debug=1
            elseif(TnS.gt.tmelt.and.TnS1.le.tmelt)then

C PCM non-subcooled solidification is completed (case 2).              
               AppCon=(1.0E0-storedLH/Lges)*consol+storedLH/Lges*conliq
               AppSHC=SHCLIQ+storedLH/(TnS-tmelt)
               storedLH=0.0E0		!- now all PCM is solid
               Debug=2
            elseif(TnS.le.tsoli.and.TnS1.ge.tmelt) then

C PCM is partially liquid and is now cooled (non-subcooling, case 3).
               storeThisTS=LatentHeat(1,TnS,TnS1)
               storedLH=storedLH-storeThisTS
               if(storedLH.lt.0.0E+0)storedLH=0.0E+0	!- storedLH >= 0
               AppCon=(1.0E0-storedLH/Lges)*consol+storedLH/Lges*conliq
               AppSHC=shcliq+storeThisTS/(TnS-TnS1) ! to be verified
               Debug=3
            endif		!- temperature bracketing for k=1
         else			!- k=0

C PCM behaves according to subcooling properties.
            if(TnS1.ge.tsoli2)then

C PCM is completely melted and does not reach sub-cooled
C solidification temperature (case 5).
               storedLH=Lges	!- crude
               AppCon=conliq
               AppSHC=shcliq
               Debug=5
             elseif(TnS.gt.tsoli2.and.TnS1.lt.tsoli2)then

C PCM begins to solidify in sub-cooled mode (k=0, case 6).
               storeThisTS=LatentHeat(0,tsoli2,TnS1)
               storedLH=Lges-storeThisTS
               AppCon=(1.0E0-storedLH/Lges)*consol+storedLH/Lges*conliq
               AppSHC=shcsol+storeThisTS/(tsoli2-TnS1)	!- to be verified
               Debug=6
            elseif(TnS.le.tsoli2.and.TnS1.gt.tmelt2)then

C PCM is subcooled partial liquid and not completely
C solidified (k=0, case 7).
               storeThisTS=LatentHeat(0,TnS,TnS1)
               storedLH=storedLH-storeThisTS 
               if(storedLH.lt.0.0E+0)storedLH=0.0E+0	!- storedLH >= 0 
               AppCon=(1.0E0-storedLH/Lges)*consol+storedLH/Lges*conliq
               AppSHC=shcsol+storeThisTS/(TnS-TnS1)	!- to be verified
               Debug=7
             elseif(TnS.gt.tmelt2.and.TnS1.le.tmelt2)then

C PCM completely solidifies (k=0, case 8).
               storeThisTS=LatentHeat(0,TnS,tmelt2)
               storedLH=0.0E0
               AppCon=consol
               AppSHC=shcsol+storeThisTS/(TnS-tmelt2)	!- to be verified

C Set new k.
               k=1
               Debug=8
            endif		!- temperature bracketing for k=0
         endif
      else
				!- what happens here (TnS = TnS1)?
         if(AppCon.le.0.0E+00)AppCon=consol
         if(AppSHC.le.0.0E+00)AppSHC=shcsol
         Debug=9
      endif

      return
      end


C ***************** SPMCMP57 *****************
C (c) A. Geissler, Yuanda-Europe, January 2009 - GPL
C A model of surface condensation / evaporation and corresponding 
C heat gain / loss to / from the surface. 
C Mass transport to / from the surface (kg/s) is calculated ...
C
C where,
C       hc = convective heat transfer coefficient (W/(m^2 K))
C       Asurf = surface area (m^2)
C       Tsurf = surface temperature (degC)

      subroutine spmcmp57(icomp,ispmnod)
#include "building.h"

C N.B. All parameters comply with the Fortran implicit naming convention
C except where explicitly redefined.
      common/tc/itc,icnt
      common/trace/itcf,itrace(mtrace),izntrc(mcom),itu
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its
C.....Climate data (present and future time rows)
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

      common/spmfxst/ispmxist,spflnam
      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &              nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      common/spmatlbl/spmlabel(mspmnod)   
      common/resspm/ndatspm(mspmnod),dataspm(mspmnod,MSPMRES),
     &         strspm(mspmnod,MSPMRES),unitspm(mspmnod,MSPMRES),
     &         txtspm(mspmnod,MSPMRES)
      common/zonspmf/zspmf1(mcom),zspmf2(mcom)
      common/evapsur/vapsur(mcom)
      common/prec1/sna(mcom,ms),zoa(mcom)
      common/vthp14/thrmli(mcom,ms,me,7)
      common/fvala/tfa(mcom),qfa(mcom)
      common/fvals/tfs(mcom,ms),qfs(mcom)
C TFC(MCOM,MS,MN),     ! - future construction temperature (oC)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      REAL TFC, QFC
      common/concoe/hcip(mcom,ms),hcif(mcom,ms),hcop(mcom,ms),
     &              hcof(mcom,ms)
      common/fvalg/gfa(mcom)
      COMMON/VAPC2/ZMDAP(MCOM),ZMDAF(MCOM)
      common/atpres/patmos

c Common for the number of time steps per hour
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      
C Generic nodal flux common allowing a positive or negative flux
C to be applied to a node; genflxp is updated in mzls5.
      common/genflux/genflxf(mcom,ms,mn),genflxp(mcom,ms,mn)

C Common for the storage of surface condensation amounts. 
C     1... external surface of ms
C     2... internal surface of ms
      common/surfcond/mh2o(mcom,ms,2),dmh2op(mcom,2),dmh2of(mcom,2)
      real mh2o,dmh2op,dmh2of

      character spmlabel*16,spflnam*72,outs*124,strspm*16,unitspm*16,
     &          txtspm*72

C local variables
      real time_step,mflux_max,beta,dT,Qavail,hvap
      real mflux(2),icefac(2),zrh(2),tzone(2),tsurf(2),wzone(2)
      real alpha(2),pD(2),psurf_sat(2),tz_sat(2)
      real asurf_min(2),asurf_max(2)
      real den_h2o,hystCond,hystEvap

      logical ice(2)

C surface location.
      if(icomp.ne.ispmloc(ispmnod,1))then
         write(outs,*)'SPMAT57 fatal error: incorrect zone assignment!'
         call edisp(iuout,outs)
         stop
      endif
      isurf=ispmloc(ispmnod,2)
      inode=ispmloc(ispmnod,3)
      ielem=nint(real(inode)/2.)

C initialize
      if(nsinc.le.1)then
        do i=1,2
          mh2o(icomp,isurf,i)=0.0
          dmh2op(icomp,i)=0.0
          dmh2of(icomp,i)=0.0
        enddo
      endif

      time_step=3600./float(ntstep)
      den_h2o=1010.0 !  kg/m^3

C set required local variables 
      do i=1,2
        ice(i)=.false.
        icefac(i)=1.0
        mflux(i)=0.0
      enddo

      hystCond=0.05
      hystEvap=0.10
      beta=0.0
      Qavail=0.0

      asurf=sna(icomp,isurf)

C Data for external surface
      tsurf(1)=tfc(icomp,isurf,1)  ! external surface temperature
        if (tsurf(1) .lt. 0.0) ice(1)=.true.
      alpha(1)=hcof(icomp,isurf)       ! W/(m2 K)
      tzone(1)=TF                      ! ambient air temperature
      wzone(1)=HUMRT1(TF,HF,PATMOS,0)  ! ambient air moisture content, kg/kg_dry-air
      zrh(1)=HF                        ! ambient air relative humidity, %

C Data for internal surface
      tsurf(2)=tfs(icomp,isurf)
        if (tsurf(2) .lt. 0.0) ice(2)=.true.
      alpha(2)=hcif(icomp,isurf)  ! W/(m2 K)
      tzone(2)=tfa(icomp)
      wzone(2)=gfa(icomp)              !   zone moisture content, kg_h2o/kg_dry-air
      zrh(2)=pcrh2(tzone(2),wzone(2),patmos) ! zone relative humidity, %
      zrh(2)=min(100.0,zrh(2))            ! more than 100% is not possible, however pcrh2 returns values in excess of this!!

C 
      do i=1,2
        pD(i)=zrh(i)*SATVP(tzone(i))/100.0 ! vapour pressure at rh, tzone; mbar
        psurf_sat(i)=SATVP(tsurf(i))       ! saturation vapour pressure at tsurf; mbar
        tz_sat(i)=dewpt(humr(tzone(i),zrh(i),PATMOS),PATMOS)
      enddo

C Store old future value as current present value
      do i=1,2 
        dmh2op(icomp,i)=dmh2of(icomp,i)
      enddo

C set "usable" thickness of surface material and surface properties (use for both internal
C and external ...
      efthk=min(0.001,thrmli(icomp,isurf,ielem,4))
      den=thrmli(icomp,isurf,ielem,2)
      sht=thrmli(icomp,isurf,ielem,3)

C loop for external and internal surfaces
      do i=1,2
        if (ice(i)) icefac(i)=1.13 ! approximate increase in hvap
        if ( .not. ((tsurf(i) .lt. (tz_sat(i)-hystCond))
     &              .and. (psurf_sat(i) .lt. pD(i)))
     &       .and.
     &       .not. ( ((tsurf(i) .gt. (tz_sat(i)+hystEvap)) 
     &               .and. (mh2o(icomp,isurf,i) .gt. 0.0)) 
     &               .and. (psurf_sat(i) .gt. pD(i)) ) ) cycle
C ... if we reach here, something actually happens ...
C                   Calculate available capacity; hereby assume that we can not condensate more than
C                   will lead to a dT increase of surface temperature due to condensation heat gain
        dT=tz_sat(i)-tsurf(i)
        Qavail=sht*den*efthk*dT/time_step ! J/(s m^2)
        mflux_max=Qavail*asurf*time_step/cndwat1(tsurf(i)) ! kg
C                 - calc mass flux to / from surface, Zuercher, page 57
        beta=3.5E06*alpha(i)/(461.5*(273.15+(tsurf(i)+tzone(i))*0.5)) ! mg/(m2 h Pa)
        mflux(i)=asurf*beta*(pD(i)-psurf_sat(i))*100 ! mg/h
        mflux(i)=mflux(i)*time_step/3600. ! mg
        mflux(i)=mflux(i)*1.0E-06 ! kg
C                  Check against max. possible flux due to available heat capacity of surface
        mflux(i)=sign(min(abs(mflux_max),abs(mflux(i))),dT)
        dmh2of(icomp,i)=-mflux(i) ! change in zone is negative of flux to / from surface
C                   and update total amount of condensate
        mh2o(icomp,isurf,i)=mh2o(icomp,isurf,i)+mflux(i)
        mh2o(icomp,isurf,i)=max(0.0,mh2o(icomp,isurf,i))
C                 - change heat balance of surface (genflxf()?)
C                   Enthalpy of phase change of water at tsurf (J/kg)
        hvap=icefac(i)*cndwat1(tsurf(i))
C                   Surface condensation gain (J/(s m^2)).      
        genflxf(icomp,isurf,inode)=mflux(i)*hvap/(asurf*time_step)
      enddo

C Calculate percentage of surface area for which water amount theoretically is enough to be visible
C Note: This approximation does not distinguish between water and ice ...
      do i=1,2
        asurf_min(i)=100.0*mh2o(icomp,isurf,i)/(asurf*den_h2o*1.9E-07)
        asurf_max(i)=100.0*mh2o(icomp,isurf,i)/(asurf*den_h2o*9.5E-08)
      enddo

C Because there may be more than one evaporative surface
C per zone, Ev is added to the current value of vapsur(icomp).
C      vapsur(icomp)=vapsur(icomp)+ev


C Results output.
      ndatspm(ispmnod)=9

      dataspm(ispmnod,1)=genflxf(icomp,isurf,inode)
      strspm(ispmnod,1)='/QPhCh'
      unitspm(ispmnod,1)='(W/m^2)'
      txtspm(ispmnod,1)='Heat flux of phase change of water'

      dataspm(ispmnod,2)=mflux(2)*1000./time_step
      strspm(ispmnod,2)='/MFluxInt'
      unitspm(ispmnod,2)='(g/s)'
      txtspm(ispmnod,2)='Internal surface mass flux'    

      dataspm(ispmnod,3)=mh2o(icomp,isurf,2)
      strspm(ispmnod,3)='/totH2OInt'
      unitspm(ispmnod,3)='(kg)'
      txtspm(ispmnod,3)='total mass on internal surface'

      dataspm(ispmnod,4)=mflux(1)*1000./time_step
      strspm(ispmnod,4)='/MFluxExt'
      unitspm(ispmnod,4)='(g/s)'
      txtspm(ispmnod,4)='External surface mass flux'
      
      dataspm(ispmnod,5)=mh2o(icomp,isurf,1)
      strspm(ispmnod,5)='/totH2OExt'
      unitspm(ispmnod,5)='(kg)'
      txtspm(ispmnod,5)='total mass on external surface'

      dataspm(ispmnod,6)=asurf_min(2)
      strspm(ispmnod,6)='/AminInt'
      unitspm(ispmnod,6)='(%)'
      txtspm(ispmnod,6)='Area frac. for visibility, internal, lower'

      dataspm(ispmnod,7)=asurf_max(2)
      strspm(ispmnod,7)='/AmaxInt'
      unitspm(ispmnod,7)='(%)'
      txtspm(ispmnod,7)='Area frac. for visibility, internal, upper'

      dataspm(ispmnod,8)=asurf_min(1)
      strspm(ispmnod,8)='/AminExt'
      unitspm(ispmnod,8)='(%)'
      txtspm(ispmnod,8)='Area frac. for visibility, external, lower'

      dataspm(ispmnod,9)=asurf_max(1)
      strspm(ispmnod,9)='/AmaxExt'
      unitspm(ispmnod,9)='(%)'
      txtspm(ispmnod,9)='Area frac. for visibility, external, upper'

C      write(*,"(14(a,E11.4))")
C     & "ts_i=",tsurf(2)," tz_i=",tzone(2)," tz_sat_i=",tz_sat(2),
C     & " zrh_i=",zrh(2)," pD_i=",pD(2)," ps_sat_i=",psurf_sat(2),
C     & " QPhCh=",genflxf(icomp,isurf,inode),
C     & " Qavail=",Qavail,
C     & " MFlux_i=",mflux(2)," M_tot_i=",mh2o(icomp,isurf,2),
C     & " beta=",beta," wz_i=",wzone(2),
C     & " a_min_i=",asurf_min(2)," a_max_i=",asurf_max(2)

C As a temporary measure, use zone-related variables, zspmf1 and zspmf2,
C for results library storage. Output is limited to one special material
C per zone.
      zspmf1(ispmloc(ispmnod,1))=dataspm(ispmnod,1)
      zspmf2(ispmloc(ispmnod,1))=dataspm(ispmnod,2)   

C XML output.
      call SurfCond_to_h3k(icomp,inode,isurf,ispmnod)

C Trace output.
      if(itc.le. 0 .or. nsinc .lt. itc .or. ispmxist .eq. 0) goto 999
      if(itrace(19).eq. 0 .or.
     &            nsinc .gt. itcf .or. izntrc(icomp) .ne. 1) goto 999
      write(outs,'(a,a)') 'Trace output from SPMCMP57 for ',
     &                                              spmlabel(ispmnod)
      call edisp(itu,outs)
      write(outs,'(a,2x,i4,2x,i4,2x,i4)')
     &        'Affected zone, surface and node: ',ispmloc(ispmnod,1),
     &                          ispmloc(ispmnod,2),ispmloc(ispmnod,3)
      call edisp(itu,outs)
      call edisp(itu,' ')
      write(outs,'(a)') 'Flux to / from internal surface (g/s):'
      call edisp(itu,outs)
      write(outs,'(f7.4)') mflux(2)
      call edisp(itu,outs)
      write(outs,'(a)') 'Heat flux due flux to / from surface (W/m^2):' 
      call edisp(itu,outs) 
      write(outs,'(F10.4)') genflxf(icomp,isurf,inode)
      call edisp(itu,outs)   
      write(outs,'(a)') 'Total mass on internal surface (kg):' 
      call edisp(itu,outs) 
      write(outs,'(F10.6)')mh2o(icomp,isurf,2)
      call edisp(itu,outs)   
  999 return
      end
      
C ========================================================
C Utility routines
C=========================================================
C ***************** PCM_to_h3k *****************
C This routine transfers PCM domain data to the H3K reporting
C facilities. Created by Achim Geissler, June 17 2008.

      subroutine PCM_to_h3k(iZone,iNode,iSurf,ispmnod)
      implicit none

#include "building.h"

C Parameters.
      integer iZone,iNode,iSurf,ispmnod

C Commons.
      common/spmatlbl/spmlabel(mspmnod)
      character spmlabel*16
      common/resspm/ndatspm(mspmnod),dataspm(mspmnod,MSPMRES),
     &         strspm(mspmnod,MSPMRES),unitspm(mspmnod,MSPMRES),
     &         txtspm(mspmnod,MSPMRES)
      integer ndatspm		!- number of results items for each special material
      real dataspm		!- results data for special material
      character strspm*16
      character unitspm*16	!- unit for data item
      character txtspm*72	!- description of data item

C Local variables.
      character*2 cZone_Chars, cSurf_Chars, cNode_Chars
      integer ndat,idat

C References.
      integer lnblnk

C Pad index to 'XX'.
      if(iZone.gt.9)then
         write(cZone_Chars,'(i2)')iZone
      else
         write(cZone_Chars,'(a,i1)') '0',iZone
      endif
      if(isurf.gt.9)then
         write(cSurf_Chars,'(i2)')isurf
      else
         write(cSurf_Chars,'(a,i1)') '0',isurf
      endif
      if(inode.gt.9)then
         write(cNode_Chars,'(i2)')inode
      else
         write(cNode_Chars,'(a,i1)') '0',inode
      endif

      ndat=min0(MSPMRES,ndatspm(ispmnod)) !- maximum of MSPMRES entries possible

C Output available PCM layer data.
      do idat=1,ndat
         call add_to_xml_reporting(
     &     dataspm(ispmnod,idat),
     &     'bui/spm/'//spmlabel(ispmnod)(1:lnblnk(spmlabel(ispmnod)))//
     &     '/z' // cZone_Chars // '/s' // cSurf_Chars //
     &     '/nd' // cNode_Chars // 
     &     strspm(ispmnod,idat)(1:lnblnk(strspm(ispmnod,idat))),
     &     'units',
     &     unitspm(ispmnod,idat)(1:lnblnk(unitspm(ispmnod,idat))),
     &     txtspm(ispmnod,idat)(1:lnblnk(txtspm(ispmnod,idat))))
      enddo

      return
      end
      
C ***************** SurfCond_to_h3k *****************
C This routine transfers PCM domain data to the H3K reporting
C facilities. Created by Achim Geissler, Jan 16 2009.

      subroutine SurfCond_to_h3k(iZone,iNode,iSurf,ispmnod)
      implicit none

#include "building.h"

C Parameters.
      integer iZone,iNode,iSurf,ispmnod

C Commons.
      common/spmatlbl/spmlabel(mspmnod)
      character spmlabel*16
      common/resspm/ndatspm(mspmnod),dataspm(mspmnod,MSPMRES),
     &         strspm(mspmnod,MSPMRES),unitspm(mspmnod,MSPMRES),
     &         txtspm(mspmnod,MSPMRES)
      integer ndatspm		!- number of results items for each special material
      real dataspm		!- results data for special material
      character strspm*16
      character unitspm*16	!- unit for data item
      character txtspm*72	!- description of data item

C Local variables.
      character*2 cZone_Chars, cSurf_Chars, cNode_Chars
      integer ndat,idat

C References.
      integer lnblnk

C Pad index to 'XX'.
      if(iZone.gt.9)then
         write(cZone_Chars,'(i2)')iZone
      else
         write(cZone_Chars,'(a,i1)') '0',iZone
      endif
      if(isurf.gt.9)then
         write(cSurf_Chars,'(i2)')isurf
      else
         write(cSurf_Chars,'(a,i1)') '0',isurf
      endif
      if(inode.gt.9)then
         write(cNode_Chars,'(i2)')inode
      else
         write(cNode_Chars,'(a,i1)') '0',inode
      endif

      ndat=min0(MSPMRES,ndatspm(ispmnod)) !- maximum of MSPMRES entries possible

C Output available surface condensation data.
      do idat=1,ndat
         call add_to_xml_reporting(
     &     dataspm(ispmnod,idat),
     &     'bui/spm/'//spmlabel(ispmnod)(1:lnblnk(spmlabel(ispmnod)))//
     &     '/z' // cZone_Chars // '/s' // cSurf_Chars //
     &     '/nd' // cNode_Chars // 
     &     strspm(ispmnod,idat)(1:lnblnk(strspm(ispmnod,idat))),
     &     'units',
     &     unitspm(ispmnod,idat)(1:lnblnk(unitspm(ispmnod,idat))),
     &     txtspm(ispmnod,idat)(1:lnblnk(txtspm(ispmnod,idat))))
      enddo

      return
      end
