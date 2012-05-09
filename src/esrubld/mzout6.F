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
C      MZOUT6

c ******************** MZOUT6 ********************

C MZOUT6 gives summary output for save option 6 only.
C ztu,ztl are upper/lower zone temperature.
C zph,zpc are zone heating/cooling capacities.
C tzph,tzpc are zone heating/cooling use over time.

C ---------------------- IMPORTANT -------------------------------
C Any changes made to key words in this subroutine should also be 
C made in subroutine RSL6TF in esruprj/mksbem.F
C ----------------------------------------------------------------

      SUBROUTINE MZOUT6
#include "building.h"
#include "geometry.h"
#include "sbem.h"      
      
      integer lnblnk  ! function definition
      
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL   
      COMMON/SAVE0/TVMEM(MCOM,4,2),ZTU(MCOM),ZTL(MCOM),ZPH(MCOM),
     &   ZPC(MCOM),TZPH(MCOM),TZPC(MCOM),TZPHM(MCOM,12),TZPCM(MCOM,12)
      COMMON/SAVE1/LSAVE
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/C21/IFCFG,cfgroot,LCFGF

      character LTMP*72
      character cfgroot*24,tab*1
      character*72 lcfgf
      CHARACTER DS*7,DS1*10,DE*7,DE1*10
      CHARACTER min*420

C For DHW and ncm calculations:
      integer iactvalue !temporary integer for holding activity numbers (UK NCM) to
                        !use in DHW calculations
      dimension MJ_DHW_ann_energy(MCOM),kWh_DHW_ann_energy(MCOM)
      dimension z_DHWperm2(MCOM)
      real MJ_DHW_ann_energy,kWh_DHW_ann_energy,Tot_MJ_DHW_ann_energy
      real Tot_kWh_DHW_ann_energy,z_DHWperm2,TotDHWperm2
C DHW end

C Common to calculate ncm lighting energy calculation for results output
      common/ncmlightEnergy/LGHT_ncm_Energy(MCOM),
     &      fmonthLGHT_ncm_Energy(12,MCOM)      
      real LGHT_ncm_Energy,fmonthLGHT_ncm_Energy
      real ncmBuildLight_kWh !total UK ncm energy cnsumption for lighting in kWh
      real ncm_m2_BuildLight_kWh !total UK ncm energy cnsumption for lighting in kWh/m2
      logical closea
      dimension zlightperm2(MCOM)
      real zlightperm2
      real fReportMonthlyLighting !Local variable to report the monthly lighting loads in kWh
      dimension fReportMonthlyLighting(12,MCOM)
      
C Auxiliary energy:
      common/ncmAuxilEnergy/zonAuxil(12,MCOM),m2zonAuxil(12,MCOM),
     &      fmonthAuxil_ncm_Energy(12,MCOM)
      real zonAuxil,m2zonAuxil,fmonthAuxil_ncm_Energy
      real fReportMonthlyAuxil
      dimension fReportMonthlyAuxil(12,MCOM) !Local variable to report the monthly Auxiliary loads in kWh
      
C Common to count for the number of timesteps that systems are in operatrion
C Used to calculate an NCM Auxiliary energy (post-processed in save level 6)
C It excludes start-up timesteps
      common/ncmOperHrSystem/iOperTimestep(MCOM),
     &       iMonthOperTimestep(12,MCOM)     
      integer iOperTimestep,iMonthOperTimestep   

C Common to count the number of occupied hours above 27oC (oper. temperature)
C Used for UK NCM purposes
C NOTE: this is timestep overheating
      common/ncmHrOverheat/ihroverheats(MCOM),ihroccup(MCOM)
      integer ihroverheats,ihroccup
      dimension prcntAbove27_Overheat(MCOM)
      real prcntAbove27_Overheat
      dimension hroverheats(MCOM),hroccup(MCOM)   
      real hroverheats,hroccup

C Variables related to Auxiliary energy for UK NCM calculations:
      real AuxResNcm_kWh,m2_AuxResNcm_kWh !total auxiliary for the building
      dimension m2kWhZAuxil(MCOM)
      real m2kWhZAuxil !auxiliary energy per zone in kWh/m2
      real ZoneHL_kWh,ZoneCL_kWh !heat and cooling load per zone in kWh
      real fmonthlyMJDHW !dhw per month in MJ
      dimension fmonthlyMJDHW(12,MCOM)
      real fmonthlykWhDHW !dhw per month in kWh
      dimension fmonthlykWhDHW(12,MCOM)      

      DIMENSION Total_zone_H(MCOM),Total_zone_C(MCOM)
      integer icfgNameLen
      save icfgNameLen

      logical bInitialized
      data bInitialized /.false./
      real  total_floor_area  ! to record total floor area
      real fZoneHoursOperation
      dimension fZoneHoursOperation(MCOM)
      real fZoneMonthlyHoursOperation
      dimension fZoneMonthlyHoursOperation(12,MCOM)   
         
C Check if save option 6 has been selected.
      tab=','
      IF(LSAVE.NE.6)goto 2

C Set  automatic file name and is saved in the models folder:
      IWU=IFIL+1
      write(LTMP,'(2a)')cfgroot(1:lnblnk(cfgroot)),'.txt'
      CALL EFOPSEQ(IWU,LTMP,3,IER)
      IF(IER.LT.0) return

      DO 221 I=1,NCOMP

C Avoid cases of dividing by 0
        call eclose(ZBASEA(I),0.0,0.0001,closea)
        if(closea)then
          write(min,'(a,I3)') 'Base area seems to be 0 for zone: ',I
          call edisp(IWU,min)               
          write(min,'(a)') 'Check the model and try again'
          call edisp(IWU,min)               
          return       
        else
          continue
        endif

C Convert TO kWh and divide by floor area
        DO 222 IMNTH=1,12
          TZPHM(I,IMNTH)=TZPHM(I,IMNTH)/((1000.0*NTSTEP)*ZBASEA(I))
          TZPCM(I,IMNTH)=TZPCM(I,IMNTH)/((1000.0*NTSTEP)*ZBASEA(I))       
  222   CONTINUE
  221 CONTINUE       
        
c Save option is six.
      SHTG=0.0
      SCLG=0.0
      VA=-1000.0
      VB=1000.0
      VC=-1000.0
      VD=1000.0

C Generate view period string.
      CALL EDAY(ISD1,ISM1,IODS)
      CALL STDATE(IYEAR,IODS,DS,DS1)
      CALL EDAY(ISD2,ISM2,IODF)
      CALL STDATE(IYEAR,IODF,DE,DE1)

C Most of the items in this/next loop are not used (i.e. written out) 
C at the moment but may be a good idea to use them in the future.
      DO 20 I=1,NCOMP
        CALL EDAYR(INT(TVMEM(I,1,2)),ID1,IM1)
        CALL EDAYR(INT(TVMEM(I,2,2)),ID2,IM2)
        CALL EDAYR(INT(TVMEM(I,3,2)),ID3,IM3)
        CALL EDAYR(INT(TVMEM(I,4,2)),ID4,IM4)
        CALL STDATE(IYEAR,INT(TVMEM(I,3,2)),DS,DS1)
        CALL STDATE(IYEAR,INT(TVMEM(I,4,2)),DE,DE1)
        PH=ZPH(I)/1000.0
        PC=ZPC(I)/1000.0
        TPH=TZPH(I)/((1000.00*NTSTEP)*ZBASEA(I))
        TPC=TZPC(I)/((1000.0*NTSTEP)*ZBASEA(I))

C Find diversifide totals for heating and cooling and occurance.
        SHTG=SHTG+TPH
        SCLG=SCLG+TPC
        IF(ZTU(I).LE.VA.AND.I.NE.1)goto 6
        VA=ZTU(I)
    6   IF(ZTL(I).GE.VB.AND.I.NE.1)goto 7
        VB=ZTL(I)
    7   IF(PH.LE.VC.AND.I.NE.1)goto 8
        VC=PH
    8   IF(PC.GE.VD.AND.I.NE.1)goto 20
        VD=PC
   20 CONTINUE

      write(min,'(7a)')'Case_ID',tab,'Zone_ID',tab,
     &'key',tab,'Value'

      call edisp(IWU,min)
      do 42 i = ISM1,ISM2
        HM=0.
        CM=0.
        do 43 j = 1,NCOMP
          HM=HM+TZPHM(j,i)
          CM=CM+TZPCM(j,i)
  43    continue
  42  continue

      if ( .not. bInitialized ) then
        total_floor_area=0.0  ! to record total floor area
        do I=1, NCOMP
          total_floor_area=total_floor_area+zbasea(I)
        enddo
        icfgNameLen = lnblnk(LCFGF)
        bInitialized = .true.
      endif

      if(ISBEM.EQ.2)then

C DHW calculations for UK NCM data
C NCM method assumes that DT (temper. difference) is always 50oC for
C Domestic hot water. See SBEM Technical manual for additional info.
C Takes from tdf the requirements in l/d/m2.
        Tot_MJ_DHW_ann_energy=0.
        Tot_kWh_DHW_ann_energy=0.
        TotDHWperm2=0.
        ncmBuildLight_kWh=0.
        ncm_m2_BuildLight_kWh=0.
        AuxResNcm_kWh=0.
        m2_AuxResNcm_kWh=0.
        

C Work out DHW demand 
C<< Uncomment when DHW calculations are completely put in
c        CALL NCMDHW

        do 741 I=1,NCOMP

C Avoid cases of dividing by 0
          call eclose(ZBASEA(I),0.0,0.0001,closea)
          if(closea)then
            write(min,'(a,I3)') 'Base area seems to be 0 for zone: ',I
            call edisp(IWU,min)               
            write(min,'(a)') 'Check the model and try again'
            call edisp(IWU,min)               
            return       
          else
            continue
          endif
          iactvalue=theactivityindex(I)

C DHW in MJ:
          MJ_DHW_ann_energy(I)=
     &        dhw_litres(iactvalue)*ZBASEA(I)*(4.18/1000.)*50.
          Tot_MJ_DHW_ann_energy=
     &        MJ_DHW_ann_energy(I)+Tot_MJ_DHW_ann_energy

C DHW in kWh:
          kWh_DHW_ann_energy(I)=MJ_DHW_ann_energy(I)/3.6
          Tot_kWh_DHW_ann_energy=
     &        kWh_DHW_ann_energy(I)+Tot_kWh_DHW_ann_energy
          z_DHWperm2(I)=kWh_DHW_ann_energy(I)/ZBASEA(I)

C Now write monthly values for DHW. SPlit it evenly every month
C Also include in this loop the lighting energy consumption per zone
          do 1242 imnth=1,12
            if(imnth.eq.1.or.imnth.eq.3.or.imnth.eq.5.or.imnth.eq.7.or.
     &         imnth.eq.8.or.imnth.eq.10.or.imnth.eq.12)then
              fmonthlyMJDHW(imnth,I)=(MJ_DHW_ann_energy(I)/365.)*31.
              fmonthlykWhDHW(imnth,I)=(kWh_DHW_ann_energy(I)/365.)*31.
              if(imnth.lt.10)then
                write(min,'(5a,i1,2a,f12.3)') 
     &            LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &            tab,'z_DHW_Month_',imnth,'_MJ',tab,
     &            fmonthlyMJDHW(imnth,I)
                call edisp(IWU,min)               
                write(min,'(5a,i1,2a,f12.3)') !kWh
     &            LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &            tab,'z_DHW_Month_',imnth,'_kWh',tab,
     &            fmonthlykWhDHW(imnth,I)
                call edisp(IWU,min)                
              else
                write(min,'(5a,i2,2a,f12.3)') 
     &            LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &            tab,'z_DHW_Month_',imnth,'_MJ',tab,
     &            fmonthlyMJDHW(imnth,I)
                call edisp(IWU,min) 
                write(min,'(5a,i2,2a,f12.3)') 
     &            LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &            tab,'z_DHW_Month_',imnth,'_kWh',tab,
     &            fmonthlykWhDHW(imnth,I)
                call edisp(IWU,min)                 
              endif
            elseif(imnth.eq.2)then
              fmonthlyMJDHW(imnth,I)=(MJ_DHW_ann_energy(I)/365.)*28.  
              fmonthlykWhDHW(imnth,I)=(kWh_DHW_ann_energy(I)/365.)*28.
              write(min,'(5a,i1,2a,f12.3)') 
     &          LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &          tab,'z_DHW_Month_',imnth,'_MJ',tab,
     &          fmonthlyMJDHW(imnth,I)
              call edisp(IWU,min) 
              write(min,'(5a,i1,2a,f12.3)') 
     &          LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &          tab,'z_DHW_Month_',imnth,'_kWh',tab,
     &          fmonthlykWhDHW(imnth,I)
              call edisp(IWU,min) 

C 30 days months
            else    
              fmonthlyMJDHW(imnth,I)=(MJ_DHW_ann_energy(I)/365.)*30.   
              fmonthlykWhDHW(imnth,I)=(kWh_DHW_ann_energy(I)/365.)*30.              
              if(imnth.lt.10)then       
                write(min,'(5a,i1,2a,f12.3)') 
     &            LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &            tab,'z_DHW_Month_',imnth,'_MJ',tab,
     &            fmonthlyMJDHW(imnth,I)
                call edisp(IWU,min) 
                write(min,'(5a,i1,2a,f12.3)') 
     &            LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &            tab,'z_DHW_Month_',imnth,'_kWh',tab,
     &            fmonthlykWhDHW(imnth,I)
                call edisp(IWU,min)                               
              else
                write(min,'(5a,i2,2a,f12.3)') 
     &            LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &            tab,'z_DHW_Month_',imnth,'_MJ',tab,
     &            fmonthlyMJDHW(imnth,I)
                call edisp(IWU,min) 
                write(min,'(5a,i2,2a,f12.3)') 
     &            LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &            tab,'z_DHW_Month_',imnth,'_kWh',tab,
     &            fmonthlykWhDHW(imnth,I)
                call edisp(IWU,min)                 
              endif              
            endif

C Zone monthly lighting loads in kWh         
            fReportMonthlyLighting(imnth,I)=
     &        fmonthLGHT_ncm_Energy(imnth,I)/(1000.0*NTSTEP)
            if(imnth.lt.10)then    
              write(min,'(5a,i1,2a,f12.1)') 
     &          LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &          tab,'z_Lights_Month_',imnth,'_kWh',tab,
     &          fReportMonthlyLighting(imnth,I)
              call edisp(IWU,min)    
            else
              write(min,'(5a,i2,2a,f12.1)') 
     &          LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &          tab,'z_Lights_Month_',imnth,'_kWh',tab,
     &          fReportMonthlyLighting(imnth,I)
              call edisp(IWU,min)                
            endif       
 1242     continue
 
C Write out the values per zone for DHW
          write(min,'(6a,f12.3)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &      tab,'z_DHW_MJ',tab,MJ_DHW_ann_energy(I)
          call edisp(IWU,min)  
          write(min,'(6a,f12.3)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &      tab,'z_DHW_kWh',tab,kWh_DHW_ann_energy(I)
          call edisp(IWU,min) 

          write(min,'(6a,f12.3)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &      tab,'z_DHWkWhperm2',tab,z_DHWperm2(I)
          call edisp(IWU,min)           

C Lighting energy consumption for NCM calculation method. Assume
C manual control on/off for the moment.
C Dividing by 1000 to convert to kWh
          ncmBuildLight_kWh=ncmBuildLight_kWh+
     &               (LGHT_ncm_Energy(I)/(1000.0*NTSTEP))

C Write out the values per zone for lighting
C zlightperm2 is the required energy needed for lighting per m2
          zlightperm2(I)=LGHT_ncm_Energy(I)/(ZBASEA(I)*1000.0*NTSTEP)
          write(min,'(6a,f12.3)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &      tab,'z_ReqLight_kWhm2',tab,zlightperm2(I)
          call edisp(IWU,min)

C Get the number of hours of operation for each zone to use them
C in the estimation of the auxiliary energy
          fZoneHoursOperation(I)=real(iOperTimestep(I)/NTSTEP)  
          do 44 im=1,12
            fZoneMonthlyHoursOperation(im,I)=
     &        real(iMonthOperTimestep(im,I)/NTSTEP)
 44       continue                  
          if(inoti.eq.1.or.inoti.eq.2.or.inoti.eq.3.or.
     &    (inoti.eq.4.and.iDsmTestingFlag.gt.0))then

C Get hours of operation from SBEM.db1 data
            fZoneHoursOperation(I)=fMonthEstSysHrs(iactvalue,13)

C UK NCM Notional or reference building:          
C Auxiliary energy consumption for NCM calculation method.
C Dividing by 1000 to convert to kWh
C Calculate the total for the building
C Write out the values per zone for auxiliary energy
C m2zonAuxil is the required auxiliary energy per m2
C Converted to kWh/m^2 here as: m2kWhZAuxil
            do 4 im=1,12
              m2kWhZAuxil(I)=m2zonAuxil(im,I)/1000.0+m2kWhZAuxil(I)
              AuxResNcm_kWh=AuxResNcm_kWh+(zonAuxil(im,I)/1000.0)
              fReportMonthlyAuxil(im,I)=
     &          fmonthAuxil_ncm_Energy(im,I)/(1000.0)
              if(im.lt.10)then
                write(min,'(5a,i1,2a,f12.3)') 
     &            LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &            tab,'z_Aux_Month_',im,'_kWh',tab,
     &            fReportMonthlyAuxil(im,I)
                call edisp(IWU,min) 
              else
                write(min,'(5a,i2,2a,f12.3)') 
     &            LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &            tab,'z_Aux_Month_',im,'_kWh',tab,
     &            fReportMonthlyAuxil(im,I)
                call edisp(IWU,min)                
              endif
 4          continue       
          else

C Use a method described in an NCM HVAC document. This is summarised as:
C (control adjustment: CtlAdj is assumed 1 for the moment)
C Aux. Energy in kWh/m2 = (hrs of Operation/3255)*tAEnergy*CtlAdj*
C (tASFPa*SFP+tASFPb)*((tADLa*(duct_tDLd+AHU_tDLd))+tADLb)

C NOTE: if IHLZ(I) is zero e.g. the zone is not linked to
C a system assume zero for m2kWhZAuxil.
            if(IHLZ(I).eq.0)then
              m2kWhZAuxil(I)= 0.0
            else
              m2kWhZAuxil(I)=(fZoneHoursOperation(I)/3255.)*
     &          tAEnergy(IHLZ(I))*(tASFPa(IHLZ(I))*SFPHS(IHLZ(I))+
     &          tASFPb(IHLZ(I)))*((tADLa(IHLZ(I))*(duct_tDLd(IHLZ(I))+
     &          AHU_tDLd(IHLZ(I))))+tADLb(IHLZ(I)))
C             m2_AuxResNcm_kWh=m2_AuxResNcm_kWh+m2kWhZAuxil(I)
            endif       
            AuxResNcm_kWh=AuxResNcm_kWh+(m2kWhZAuxil(I)*ZBASEA(I))

C Now doing the same as above for every month. The calculation is based on
C BRE/SBEM docs that seem out of date. It is therefore an approximation.
C The value below will be reported in kWh, so there will be a *ZBASEA(I)
C plus a division of 3255 (reference operation hours) with 12 (approximation)
C Another division with 12 makes coverts it to monthly values
            do 444 im=1,12
              if(IHLZ(I).eq.0)then
                fReportMonthlyAuxil(im,I)= 0.0
              else
                fReportMonthlyAuxil(im,I)=
     &           (((fZoneMonthlyHoursOperation(im,I)/(3255./12.))*
     &           tAEnergy(IHLZ(I))*(tASFPa(IHLZ(I))*SFPHS(IHLZ(I))+
     &           tASFPb(IHLZ(I)))*((tADLa(IHLZ(I))*(duct_tDLd(IHLZ(I))+
     &           AHU_tDLd(IHLZ(I))))+tADLb(IHLZ(I))))*ZBASEA(I))/12
              endif

C Write Monthly Auxiliary here to avoid looping again over months
              if(im.lt.10)then
                write(min,'(5a,i1,2a,f12.3)') 
     &            LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &            tab,'z_Aux_Month_',im,'_kWh',tab,
     &            fReportMonthlyAuxil(im,I)
                call edisp(IWU,min) 
              else
                write(min,'(5a,i2,2a,f12.3)') 
     &            LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &            tab,'z_Aux_Month_',im,'_kWh',tab,
     &            fReportMonthlyAuxil(im,I)
                call edisp(IWU,min)                
              endif
 444        continue
          endif

C Write out the hours of operation
          write(min,'(6a,f10.3)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &      tab,'z_hrs_operation',tab,fZoneHoursOperation(I)
          call edisp(IWU,min)

C Auxiliary energy (kWh/m2)
          write(min,'(6a,f10.3)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &      tab,'z_Auxiliary_kWhm2',tab,m2kWhZAuxil(I)
          call edisp(IWU,min)
          
C Report UK NCM overheating based on the modelling guide v4d March 2008
C Find the percentage of occupied hours exceeding 27oC operative temparature
C This will be used for example for reporting in BRUKL.        
          if(ihroverheats(I).gt.0)then
            hroverheats(I)=real(ihroverheats(I))
            hroccup(I)=real(ihroccup(I))
            prcntAbove27_Overheat(I)=((hroverheats(I)/hroccup(I))*100.)
          else
            prcntAbove27_Overheat(I)=0.0
          endif
          write(min,'(6a,f7.3)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &      tab,'Overh_PercentOcc_Above27',tab,prcntAbove27_Overheat(I)
          call edisp(IWU,min)                
 741    continue        

C Domestic hot water total
        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,'Total_DHW',
     &    tab,'t_DHW_kWh',tab,Tot_kWh_DHW_ann_energy
        call edisp(IWU,min)
        TotDHWperm2=Tot_kWh_DHW_ann_energy/total_floor_area
        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,'Total_DHW_per_m^2',
     &    tab,'t_DHW_kWhperm2',tab,TotDHWperm2
        call edisp(IWU,min)  

C Lighting total
        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,'Total_lighting',
     &    tab,'t_light_kWh',tab,ncmBuildLight_kWh
        call edisp(IWU,min)
	ncm_m2_BuildLight_kWh=ncmBuildLight_kWh/total_floor_area  
        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,'Total_lighting_per_m^2',
     &    tab,'t_light_kWhperm2',tab,ncm_m2_BuildLight_kWh
        call edisp(IWU,min)

C Auxiliary energy total
        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,'Total_Auxiliary_Energy',
     &    tab,'t_aux_kWh',tab,AuxResNcm_kWh
        call edisp(IWU,min)  
        m2_AuxResNcm_kWh=AuxResNcm_kWh/total_floor_area
        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,'Total_Auxiliary_per_m^2',
     &    tab,'t_auxiliary_kWhperm2',tab,m2_AuxResNcm_kWh
        call edisp(IWU,min)
      endif            

      DO 220 I=1,NCOMP
        PH=ZPH(I)/1000.0
        PC=ZPC(I)/1000.0
        TPH=TZPH(I)/((1000.0*NTSTEP)*ZBASEA(I))
        TPC=TZPC(I)/((1000.0*NTSTEP)*ZBASEA(I))

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MH1',tab,TZPHM(I,1)
        call edisp(IWU,min)

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MC1',tab,TZPCM(I,1)
        call edisp(IWU,min)

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MH2',tab,TZPHM(I,2)
        call edisp(IWU,min)  

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MC2',tab,TZPCM(I,2)
        call edisp(IWU,min)

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MH3',tab,TZPHM(I,3)
        call edisp(IWU,min)  

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MC3',tab,TZPCM(I,3)
        call edisp(IWU,min)

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MH4',tab,TZPHM(I,4)
        call edisp(IWU,min)  

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MC4',tab,TZPCM(I,4)
        call edisp(IWU,min)

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MH5',tab,TZPHM(I,5)
        call edisp(IWU,min)  

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MC5',tab,TZPCM(I,5)
        call edisp(IWU,min)

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MH6',tab,TZPHM(I,6)
        call edisp(IWU,min)  

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MC6',tab,TZPCM(I,6)
        call edisp(IWU,min)

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MH7',tab,TZPHM(I,7)
        call edisp(IWU,min)  

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MC7',tab,TZPCM(I,7)
        call edisp(IWU,min)

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MH8',tab,TZPHM(I,8)
        call edisp(IWU,min)  

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MC8',tab,TZPCM(I,8)
        call edisp(IWU,min)

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MH9',tab,TZPHM(I,9)
        call edisp(IWU,min)  

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MC9',tab,TZPCM(I,9)
        call edisp(IWU,min)

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MH10',tab,TZPHM(I,10)
        call edisp(IWU,min)  

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MC10',tab,TZPCM(I,10)
        call edisp(IWU,min)

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MH11',tab,TZPHM(I,11)
        call edisp(IWU,min)  

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MC11',tab,TZPCM(I,11)
        call edisp(IWU,min)

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MH12',tab,TZPHM(I,12)
        call edisp(IWU,min)  

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'MC12',tab,TZPCM(I,12)

        call edisp(IWU,min)      
        Total_zone_H(I)=0.0
        Total_zone_C(I)=0.0
        do 420 m = ISM1,ISM2       
          Total_zone_H(I)=Total_zone_H(I)+TZPHM(I,m)
          Total_zone_C(I)=Total_zone_C(I)+TZPCM(I,m)
  420   continue
  
        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'ZAH',tab,Total_zone_H(I)

        ZoneHL_kWh=Total_zone_H(I)*ZBASEA(I) 
        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'integrZAHforFloorArea',tab,ZoneHL_kWh
     
        call edisp(IWU,min)                     

        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'ZAC',tab,Total_zone_C(I)
     
        ZoneCL_kWh=Total_zone_C(I)*ZBASEA(I)      
        write(min,'(6a,f12.3)') 
     &    LCFGF(1:icfgNameLen),tab,zname(I)(1:lnzname(I)),
     &    tab,'integrZACforFloorArea',tab,ZoneCL_kWh
     
        call edisp(IWU,min)  
  
  220 continue

C Close output file after writing.
      CALL ERPFREE(IWU,ISTAT)
      return

c Output for save options 1,2 and 3.
    2 CALL edisp(iuout,' Results analysis is done via module res.')
      RETURN
      END

c ******************** NCMDHW ********************
C NCMDHW computes DHW demands while accounting for DHW energy losses and
C contributions from renewable energy resources
      SUBROUTINE NCMDHW
      IMPLICIT NONE
#include "building.h"
#include "geometry.h"
#include "sbem.h"      

      COMMON/OUTIN/IUOUT,IUIN
      INTEGER IUOUT,IUIN

      INTEGER NCOMP,NCON
      COMMON/C1/NCOMP,NCON

      INTEGER IDHWSYS,I,iactvalue,IREN,ICOMP,IM,IHROP
      CHARACTER min*420
      LOGICAL CLOSEA

      dimension MJ_DHW_ann_energy(MCOM),kWh_DHW_ann_energy(MCOM)
      dimension DHWDEMAND(MNS),DISTRIBLOSS(MNS)
      DIMENSION SOLARC(MNS),DAYMNT(12),SECPUMPE(MNS)
      real MJ_DHW_ann_energy,kWh_DHW_ann_energy,Tot_MJ_DHW_ann_energy
      real Tot_kWh_DHW_ann_energy
      REAL DHWDEMAND,DISTRIBLOSS,TZONEA,SOLARC,DAILYDHWDEMAND,DAYMNT
      REAL STORAGEVOL,STORAGEVOL5,STORAGELOSS,XLOOPLEN,CIRCLOSS
      REAL PUMPPOWER,SECPUMPE   ! secpumpe is not yet used
      REAL SECLOSS
      real  total_floor_area  ! to record total floor area

      DATA DAYMNT(1),DAYMNT(2),DAYMNT(3),DAYMNT(4),DAYMNT(5),DAYMNT(6),
     &DAYMNT(7),DAYMNT(8),DAYMNT(9),DAYMNT(10),DAYMNT(11),DAYMNT(12)
     &/31,28,31,30,31,30,31,31,30,31,30,31/

      IHROP=4380 !Please provide total hours of operation of DHW system

C Fill DHW demand variables
      total_floor_area=0.0  ! to record total floor area
      Tot_MJ_DHW_ann_energy=0.0
      Tot_kWh_DHW_ann_energy=0.0
      do 741 I=1,NCOMP

C Avoid cases of dividing by 0
	call eclose(ZBASEA(I),0.0,0.0001,closea)
	if(closea)then
	  write(min,'(a,I3)') 
     &    'NCMDHW: Base area seems to be 0 for zone: ',I
          call edisp(iuout,min)
          call edisp(iuout,'Check the model and try again')
	  return       
	else
          total_floor_area=total_floor_area+zbasea(I)
	endif
	iactvalue=theactivityindex(I)

C DHW in MJ:
        MJ_DHW_ann_energy(I)=
     &      dhw_litres(iactvalue)*ZBASEA(I)*(4.18/1000.)*50.
        Tot_MJ_DHW_ann_energy=
     &      MJ_DHW_ann_energy(I)+Tot_MJ_DHW_ann_energy

C DHW in kWh:
        kWh_DHW_ann_energy(I)=MJ_DHW_ann_energy(I)/3.6
        Tot_kWh_DHW_ann_energy=
     &      kWh_DHW_ann_energy(I)+Tot_kWh_DHW_ann_energy
 741  continue
 
      DO 100 IDHWSYS=1,NDHWSYS

C Initialise
        SOLARC(IDHWSYS)=0.0       ! Solar themal collector contribution
        DHWDEMAND(IDHWSYS)=0.0    ! DHW demand on this system
        DISTRIBLOSS(IDHWSYS)=0.0  ! distribution losses (deadleg)
        TZONEA=0.0       ! total serviced area

C Get contribution from associated solar thermal collectors
C << this calculation is not complete >>
        DO 101 IREN=1,NREN
          IF(NRENTYPE(IREN).EQ.1)THEN ! If this renewable sys is solar thermal
            IF(int(RENDATA(IREN,1)).EQ.IDHWSYS)THEN ! If this sys is linked to this DHW system
C<<            SOLARC(IDHWSYS)=SOLARC(IDHWSYS)+     ! Contribution from this solar thermal collector
            ENDIF
          ENDIF
 101    CONTINUE

C Get demand for all associated zones
        DO 102 ICOMP=1,NCOMP
          IF(IDHWLZ(ICOMP).EQ.IDHWSYS)THEN
            DHWDEMAND(IDHWSYS)=
     &      DHWDEMAND(IDHWSYS)+MJ_DHW_ann_energy(ICOMP)

C Integrate distribution loss (deadleg loss)
            IF(DEADLEG(ICOMP).GT.0.0)DISTRIBLOSS(IDHWSYS)=
     &      DISTRIBLOSS(IDHWSYS)+0.17*MJ_DHW_ann_energy(ICOMP)

C Get total area served by this DHW system
            TZONEA=TZONEA+ZBASEA(ICOMP)
          ENDIF
 102    CONTINUE          

C Subtract solar thermal contributions (upto half of total demand)
        IF(SOLARC(IDHWSYS).GT.0.5*DHWDEMAND(IDHWSYS))
     &  SOLARC(IDHWSYS)=0.5*DHWDEMAND(IDHWSYS)
        DHWDEMAND(IDHWSYS)=DHWDEMAND(IDHWSYS)/SOLARC(IDHWSYS)

C Subtract CHP contributions
C << Still to do

C Add distribution loss (deadleg)
        DHWDEMAND(IDHWSYS)=DHWDEMAND(IDHWSYS)+DISTRIBLOSS(IDHWSYS)

C Compute storage loss
        IF(IDHWSS(IDHWSYS).GE.1)THEN
          STORAGELOSS=0.0
          DO 103 IM=1,12
            dailyDHWdemand=(DHWDEMAND(IDHWSYS)/12.)/DAYMNT(IM)
            Call eclose(DHWSPD(IDHWSYS,1),0.0,0.1,closea)
            IF(CLOSEA)THEN
              STORAGEVOL=dailyDHWdemand*36.
            ELSE
              STORAGEVOL=DHWSPD(IDHWSYS,1)
            ENDIF
            STORAGEVOL5=5.*36.
            CALL ECLOSE(DHWSPD(IDHWSYS,2),0.0,0.1,closea)
            IF(CLOSEA)THEN
              STORAGELOSS=0.1*(STORAGEVOL5**0.333)*(STORAGEVOL**0.667)*
     &        365.*3.6/12.+STORAGELOSS
            ELSE
              STORAGELOSS=DHWSPD(IDHWSYS,2)+STORAGELOSS
            ENDIF
 103      CONTINUE
        ENDIF

C Compute circulation loop loss and auxiliary pump energy
        IF(IDHWSS(IDHWSYS).EQ.2)THEN
          SECLOSS=0.0
          CALL ECLOSE(DHWSPD(IDHWSYS,3),0.0,0.1,closea)
          IF(CLOSEA)THEN
            CIRCLOSS=15.
          ELSE
            CIRCLOSS=DHWSPD(IDHWSYS,3)
          ENDIF
          CALL ECLOSE(DHWSPD(IDHWSYS,5),0.0,0.1,closea)
          IF(CLOSEA)THEN
            XLOOPLEN=4.*SQRT(TZONEA)
          ELSE
            XLOOPLEN=DHWSPD(IDHWSYS,5)
          ENDIF
          CALL ECLOSE(DHWSPD(IDHWSYS,4),0.0,0.1,closea)
          IF(CLOSEA)THEN
            PUMPPOWER=(0.25*XLOOPLEN+42.)/500.
          ELSE
            PUMPPOWER=DHWSPD(IDHWSYS,4)
          ENDIF
          SECLOSS=CIRCLOSS*XLOOPLEN*REAL(IHROP)*365.*3.6/1000.
          SECPUMPE(IDHWSYS)=PUMPPOWER*REAL(IHROP) ! Add to aux energy
        ENDIF

C Add storage and circulation losses
        DHWDEMAND(IDHWSYS)=DHWDEMAND(IDHWSYS)+STORAGELOSS+SECLOSS
 100  CONTINUE

      RETURN
      END


