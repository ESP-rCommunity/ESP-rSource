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
C      MZOUTP

c ******************** MZOUTP ********************

C MZOUTP gives summary output for save option 0 only.
C ztu,ztl are upper/lower zone temperature.
C zph,zpc are zone heating/cooling capacities.
C tzph,tzpc are zone heating/cooling use over time.
      SUBROUTINE MZOUTP
#include "building.h"
#include "esprdbfile.h"
#include "sbem.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/SHOUT/ICOUT
      COMMON/FILEP/IFIL
      COMMON/SAVE0/TVMEM(MCOM,4,2),ZTU(MCOM),ZTL(MCOM),ZPH(MCOM),
     &   ZPC(MCOM),TZPH(MCOM),TZPC(MCOM),TZPHM(MCOM,12),TZPCM(MCOM,12)
      COMMON/SAVE1/LSAVE
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/precz/zname(MCOM),zdesc(MCOM)

      COMMON/C1/NCOMP,NCON
      common/c2/lsnam,nccode(mcom),lproj(mcom),lgeom(mcom),
     &          lshad(mcom),lthrm(mcom),indutl(mcom),lutil(mcom)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      common/PREC17/ZBASEA(MCOM),IBASES(MCOM,12),IUZBASEA(MCOM),
     &  IZBASELIST(MCOM)
      character LTMP*72,h*72,zname*12,zdesc*64,outs*124
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      character cfgroot*24,tab*1
      character*72 lsnam,lproj,lgeom
      character*72 lshad,lthrm,lutil,lcfgf
      DIMENSION MTHNAM(12)
      CHARACTER DS*7,DS1*10,DE*7,DE1*10,MTHNAM*3
      logical OK,dok
      integer iactvalue !temporary integer for holding activity numbers (UK NCM) to
                        !use in DHW calculations
      DIMENSION MJ_DHW_ann_energy(MCOM),kWh_DHW_ann_energy(MCOM)
      real MJ_DHW_ann_energy,kWh_DHW_ann_energy,Tot_MJ_DHW_ann_energy
      real Tot_kWh_DHW_ann_energy,z_DHWperm2,TotDHWperm2

      DATA MTHNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/

C Test save option selected.
C [for NRCan]      tab=','
      tab=' '
      IF(LSAVE.NE.0)goto 2
      IF(ICOUT.EQ.99) THEN
         IWU=IUOUT
      ELSE
         IWU=ICOUT
      END IF

c Save option is zero.
   16 SHTG=0.0
      SCLG=0.0
      VA=-1000.0
      VB=1000.0
      VC=-1000.0
      VD=1000.0

      call edisp(IWU,' ')
      call edisp(IWU,'Performance assessment report')
      write(outs,'(3a)')'Results library',tab,RFILE
      call edisp(IWU,outs)
      write(outs,'(3a)')'Climate file',tab,LCLIM
      call edisp(IWU,outs)
      write(outs,'(3a)')'Configuration file',tab,LCFGF
      call edisp(IWU,outs)
      write(outs,'(3a)')'Configuration descr',tab,LSNAM
      call edisp(IWU,outs)

C Generate view period string.
      CALL EDAY(ISD1,ISM1,IODS)
      CALL STDATE(IYEAR,IODS,DS,DS1)
      CALL EDAY(ISD2,ISM2,IODF)
      CALL STDATE(IYEAR,IODF,DE,DE1)
      write(outs,'(7a,i4)') 'Period',tab,DS1,' to ',DE1,' Year',tab,
     &  IYEAR
      call edisp(IWU,outs)
      call edisp(IWU,' ')

C Report on max and min air temperatures and times of occurance for
C each zone.
      call edisp(IWU,
     & ' Zone         max air T  (occurance)  min air T  (occurance)')
      DO 10 I=1,NCOMP
        T1=TVMEM(I,1,1)
        T2=TVMEM(I,2,1)
        CALL STDATE(IYEAR,INT(TVMEM(I,1,2)),DS,DS1)
        CALL STDATE(IYEAR,INT(TVMEM(I,2,2)),DE,DE1)
        WRITE(outs,'(2a,f7.2,3a,f5.2,a,f7.2,3a,f5.2)')zname(I),tab,
     &    ZTU(I),tab,DS1,'@',T1,tab,ZTL(I),tab,DE1,'@',T2
        call edisp(IWU,outs)
   10 CONTINUE

C Report on heating/cooling capacity and demands for each zone.
      call edisp(IWU,' ')
      write(outs,'(2a)')
     & ' Zone           max heat (occurance)      max cool (occurance)',
     & '        heating     cooling'
      call edisp(IWU,outs)
      write(outs,'(2a)')
     & '                 (kW)                       (kW)              ',
     & '        (kWhr)      (kWhr)'
      call edisp(IWU,outs)

      DO 20 I=1,NCOMP
        T1=TVMEM(I,1,1)
        T2=TVMEM(I,2,1)
        T3=TVMEM(I,3,1)
        T4=TVMEM(I,4,1)
        CALL EDAYR(INT(TVMEM(I,1,2)),ID1,IM1)
        CALL EDAYR(INT(TVMEM(I,2,2)),ID2,IM2)
        CALL EDAYR(INT(TVMEM(I,3,2)),ID3,IM3)
        CALL EDAYR(INT(TVMEM(I,4,2)),ID4,IM4)
        CALL STDATE(IYEAR,INT(TVMEM(I,3,2)),DS,DS1)
        CALL STDATE(IYEAR,INT(TVMEM(I,4,2)),DE,DE1)
        PH=ZPH(I)/1000.0
        PC=ZPC(I)/1000.0
        TPH=TZPH(I)/(1000.0*NTSTEP)
        TPC=TZPC(I)/(1000.0*NTSTEP)
        WRITE(outs,'(2a,f9.2,3a,f5.2,a,f9.2,3a,f5.2,a,f10.1,a,f10.1)')
     &    zname(I),tab,PH,tab,DS1,'@',T3,tab,PC,tab,DE1,'@',T4,tab,
     &    TPH,tab,TPC
        call edisp(IWU,outs)

C Find diversifide totals for heating and cooling and occurance.
        SHTG=SHTG+TPH
        SCLG=SCLG+TPC
        IF(ZTU(I).LE.VA.AND.I.NE.1)goto 6
        IA=I
        VA=ZTU(I)
        TZA=T1
        LD1=ID1
        LM1=IM1
    6   IF(ZTL(I).GE.VB.AND.I.NE.1)goto 7
        IB=I
        VB=ZTL(I)
        TZB=T2
        LD2=ID2
        LM2=IM2
    7   IF(PH.LE.VC.AND.I.NE.1)goto 8
        IC=I
        VC=PH
        TZC=T3
        LD3=ID3
        LM3=IM3
    8   IF(PC.GE.VD.AND.I.NE.1)goto 20
        ID=I
        VD=PC
        TZD=T4
        LD4=ID4
        LM4=IM4
   20 CONTINUE

c Write save option 0 data.
      call edisp(IWU,' ')
      call edisp(IWU,'All zones:')
      CALL EDAY(LD1,LM1,IODS)
      CALL STDATE(IYEAR,IODS,DS,DS1)
      write(outs,'(2a,f6.1,6a,f5.2)')' Max_Temp',tab,VA,tab,' in ',
     & zname(IA)(1:lnblnk(zname(IA))),' on ',DS1,'@',TZA
      call edisp(IWU,outs)

      CALL EDAY(LD2,LM2,IODS)
      CALL STDATE(IYEAR,IODS,DS,DS1)
      write(outs,'(2a,f6.1,6a,f5.2)')' Min_Temp',tab,VB,tab,' in ',
     & zname(IB)(1:lnblnk(zname(IB))),' on ',DS1,'@',TZB
      call edisp(IWU,outs)

      CALL EDAY(LD3,LM3,IODS)
      CALL STDATE(IYEAR,IODS,DS,DS1)
      write(outs,'(2a,f6.1,6a,f5.2)')' Max_Heat',tab,VC,tab,' in ',
     & zname(IC)(1:lnblnk(zname(IC))),' on ',DS1,'@',TZC
      call edisp(IWU,outs)

      CALL EDAY(LD4,LM4,IODS)
      CALL STDATE(IYEAR,IODS,DS,DS1)
      write(outs,'(2a,f6.1,6a,f5.2)')' Max_Cool',tab,VD,tab,' in ',
     & zname(ID)(1:lnblnk(zname(ID))),' on ',DS1,'@',TZD
      call edisp(IWU,outs)

      call edisp(IWU,' ')
      write(outs,'(2a,f10.1,2a,f10.1,a)')'Total heating requirements',
     &  tab,SHTG,' (kWhr)',tab,SHTG*3.6,' (MJ)'
      call edisp(IWU,outs)
      write(outs,'(2a,f10.2,2a,f10.1,a)')'Total cooling requirements',
     &  tab,SCLG,' (kWhr)',tab,SCLG*3.6,' (MJ)'
      call edisp(IWU,outs)

C Monthly heating and cooling demands.
      call edisp(IWU,' ')
      call edisp(IWU,'Monthly metrics:')
      call edisp(IWU,'Month   Heating    Cooling    Heating   Cooling')
      call edisp(IWU,'Month    (kWhr)    (kwhr)      (MJ)      (MJ) ')
      do 42 i = ISM1,ISM2
        HM=0.
        CM=0.
        do 43 j = 1,NCOMP
          HM=HM+(TZPHM(j,i)/(1000.0*NTSTEP))
          CM=CM+(TZPCM(j,i)/(1000.0*NTSTEP))
  43    continue
        write(outs,'(2a,f10.1,a,f10.1,a,f10.1,a,f10.1)') MTHNAM(i),tab,
     &    HM,tab,CM,tab,HM*3.6,tab,CM*3.6
        call edisp(IWU,outs)
  42  continue

C DHW calculations for UK NCM data
      if(ISBEM.EQ.1)then

C NCM method assumes that DT (temper. difference) is always 50oC for
C Domestic hot water. See SBEM Technical manual for additional info.
C Takes from tdf the requirements in l/d/m2.
        call edisp(IWU,' ')
        write(outs,'(a)') 'Annual energy   '
        call edisp(IWU,outs)
        write(outs,'(9a)') 'requirements - DHW: ',tab,'Zone       ',tab,
     &    '   (MJ)  ',tab,'  (kWh) ',tab,'  (kWh/m2) '  
        call edisp(IWU,outs)
        Tot_MJ_DHW_ann_energy=0.
        Tot_kWh_DHW_ann_energy=0.
        TotDHWperm2=0.
        do 741 I=1,NCOMP
          iactvalue=theactivityindex(I)

C In MJ:
          MJ_DHW_ann_energy(I)=
     &        365*dhw_litres(iactvalue)*ZBASEA(I)*(4.18/1000)*50
          Tot_MJ_DHW_ann_energy=
     &        MJ_DHW_ann_energy(I)+Tot_MJ_DHW_ann_energy

C In kWh:
          kWh_DHW_ann_energy(I)=MJ_DHW_ann_energy(I)/3.6
          Tot_kWh_DHW_ann_energy=
     &        kWh_DHW_ann_energy(I)+Tot_kWh_DHW_ann_energy
          z_DHWperm2=kWh_DHW_ann_energy(I)/ZBASEA(I)   
          write(outs,'(3a,f8.1,a,f8.1,2a,f5.1)') '                    ',
     &     zname(i),tab,MJ_DHW_ann_energy(I),tab,kWh_DHW_ann_energy(I),
     &     tab,'   ',z_DHWperm2
          TotDHWperm2=z_DHWperm2+TotDHWperm2
          call edisp(IWU,outs)     
 741    continue
        write(outs,'(3a,f10.1,a,f10.1,2a,f5.1)') 
     &       '             Total:   ',tab,'        ',
     &    Tot_MJ_DHW_ann_energy,tab,Tot_kWh_DHW_ann_energy,
     &    tab,'  ',TotDHWperm2
        call edisp(IWU,outs)
      endif
      
C Close output file after writing.
      IF(IWU.NE.ICOUT)goto 17

      dok=.true.
      h(1)='The terse performance report is only written if you'
      h(2)='answer yes. '
      CALL ASKOK(' ','Write save option 0 data to file?',OK,dok,2)
      IF(.NOT.OK)goto  11
   14 IWU=IFIL+1
      H(1)='Supply a file name for this report. '
      CALL EASKS(LTMP,' Save option 0 data file name ?',
     &   ' ',72,'report','save0 file name',IER,1)
      CALL EFOPSEQ(IWU,LTMP,2,IER)
      IF(IER.LT.0)GOTO 14
      goto 16

   17 CALL ERPFREE(IWU,ISTAT)
      goto 11

c Output for save options 1,2 and 3.
    2 CALL edisp(iuout,' Results analysis is done via module res.')
   11 RETURN
      END
