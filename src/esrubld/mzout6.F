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
      SUBROUTINE MZOUT6
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SHOUT/ICOUT
      COMMON/FILEP/IFIL   
      COMMON/SAVE0/TVMEM(MCOM,4,2),ZTU(MCOM),ZTL(MCOM),ZPH(MCOM),
     &   ZPC(MCOM),TZPH(MCOM),TZPC(MCOM),TZPHM(MCOM,12),TZPCM(MCOM,12)
      COMMON/SAVE1/LSAVE
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/precz/zname(MCOM),zdesc(MCOM)

      COMMON/C1/NCOMP,NCON
      common/C21/IFCFG,cfgroot,LCFGF
      common/PREC17/ZBASEA(MCOM),IBASES(MCOM,12),IUZBASEA(MCOM),
     &  IZBASELIST(MCOM)

      character LTMP*72,zname*12,zdesc*64
      character cfgroot*24,tab*1
      character*72 lcfgf
      CHARACTER DS*7,DS1*10,DE*7,DE1*10
      CHARACTER min*420

      DIMENSION Total_zone_H(MCOM),Total_zone_C(MCOM)
      integer iZoneNameLen(MCOM)
      save iZoneNameLen
      integer icfgNameLen
      save icfgNameLen

      logical bInitialized
      data bInitialized /.false./
      
C Test save option selected.
C [for NRCan]      tab=','
        tab=','
      IF(LSAVE.NE.6)goto 2
      IF(ICOUT.EQ.99) THEN
         IWU=IUOUT
      ELSE
         IWU=ICOUT
      ENDIF
      DO 221 I=1,NCOMP

C << CONVERT TO kWh and divide by floor area
        TZPHM(I,1)=TZPHM(I,1)/((1000.0*NTSTEP)*ZBASEA(I))
        TZPCM(I,1)=TZPCM(I,1)/((1000.0*NTSTEP)*ZBASEA(I))       
        TZPHM(I,2)=TZPHM(I,2)/((1000.0*NTSTEP)*ZBASEA(I))     
        TZPCM(I,2)=TZPCM(I,2)/((1000.0*NTSTEP)*ZBASEA(I))         
        TZPHM(I,3)=TZPHM(I,3)/((1000.0*NTSTEP)*ZBASEA(I))       
        TZPCM(I,3)=TZPCM(I,3)/((1000.0*NTSTEP)*ZBASEA(I))     
        TZPHM(I,4)=TZPHM(I,4)/((1000.0*NTSTEP)*ZBASEA(I))      
        TZPCM(I,4)=TZPCM(I,4)/((1000.0*NTSTEP)*ZBASEA(I))           
        TZPHM(I,5)=TZPHM(I,5)/((1000.0*NTSTEP)*ZBASEA(I))
        TZPCM(I,5)=TZPCM(I,5)/((1000.0*NTSTEP)*ZBASEA(I))         
        TZPHM(I,6)=TZPHM(I,6)/((1000.0*NTSTEP)*ZBASEA(I))       
        TZPCM(I,6)=TZPCM(I,6)/((1000.0*NTSTEP)*ZBASEA(I))
        TZPHM(I,7)=TZPHM(I,7)/((1000.0*NTSTEP)*ZBASEA(I))        
        TZPCM(I,7)=TZPCM(I,7)/((1000.0*NTSTEP)*ZBASEA(I))   
        TZPHM(I,8)=TZPHM(I,8)/((1000.0*NTSTEP)*ZBASEA(I))    
        TZPCM(I,8)=TZPCM(I,8)/((1000.0*NTSTEP)*ZBASEA(I))          
        TZPHM(I,9)=TZPHM(I,9)/((1000.0*NTSTEP)*ZBASEA(I))     
        TZPCM(I,9)=TZPCM(I,9)/((1000.0*NTSTEP)*ZBASEA(I))
        TZPHM(I,10)=TZPHM(I,10)/((1000.0*NTSTEP)*ZBASEA(I))
        TZPCM(I,10)=TZPCM(I,10)/((1000.0*NTSTEP)*ZBASEA(I))       
        TZPHM(I,11)=TZPHM(I,11)/((1000.0*NTSTEP)*ZBASEA(I))        
        TZPCM(I,11)=TZPCM(I,11)/((1000.0*NTSTEP)*ZBASEA(I))         
        TZPHM(I,12)=TZPHM(I,12)/((1000.0*NTSTEP)*ZBASEA(I))        
        TZPCM(I,12)=TZPCM(I,12)/((1000.0*NTSTEP)*ZBASEA(I))
  221 CONTINUE       
        
c Save option is six.
   16 SHTG=0.0
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
        do I=1, NCOMP
          iZoneNameLen(I) = lnblnk(zname(I))
        enddo 
        icfgNameLen = lnblnk(LCFGF)
        bInitialized = .true.
      endif
      
      DO 220 I=1,NCOMP
        PH=ZPH(I)/1000.0
        PC=ZPC(I)/1000.0
        TPH=TZPH(I)/((1000.0*NTSTEP)*ZBASEA(I))
        TPC=TZPC(I)/((1000.0*NTSTEP)*ZBASEA(I))

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MH1',tab,TZPHM(I,1)
        call edisp(IWU,min)

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MC1',tab,TZPCM(I,1)
        call edisp(IWU,min)


        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MH2',tab,TZPHM(I,2)
        call edisp(IWU,min)  

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MC2',tab,TZPCM(I,2)
        call edisp(IWU,min)

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MH3',tab,TZPHM(I,3)
        call edisp(IWU,min)  

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MC3',tab,TZPCM(I,3)
        call edisp(IWU,min)

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MH4',tab,TZPHM(I,4)
        call edisp(IWU,min)  

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MC4',tab,TZPCM(I,4)
        call edisp(IWU,min)

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MH5',tab,TZPHM(I,5)
        call edisp(IWU,min)  

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MC5',tab,TZPCM(I,5)
        call edisp(IWU,min)

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MH6',tab,TZPHM(I,6)
        call edisp(IWU,min)  

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MC6',tab,TZPCM(I,6)
        call edisp(IWU,min)

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MH7',tab,TZPHM(I,7)
        call edisp(IWU,min)  

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MC7',tab,TZPCM(I,7)
        call edisp(IWU,min)

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MH8',tab,TZPHM(I,8)
        call edisp(IWU,min)  

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MC8',tab,TZPCM(I,8)
        call edisp(IWU,min)

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MH9',tab,TZPHM(I,9)
        call edisp(IWU,min)  

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MC9',tab,TZPCM(I,9)
        call edisp(IWU,min)

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MH10',tab,TZPHM(I,10)
        call edisp(IWU,min)  

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MC10',tab,TZPCM(I,10)
        call edisp(IWU,min)

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MH11',tab,TZPHM(I,11)
        call edisp(IWU,min)  

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MC11',tab,TZPCM(I,11)
        call edisp(IWU,min)

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'MH12',tab,TZPHM(I,12)
        call edisp(IWU,min)  

        write(min,'(6a,f10.1)') 
     &     LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &     tab,'MC12',tab,TZPCM(I,12)

        call edisp(IWU,min)      
        Total_zone_H(I)=0.0
        Total_zone_C(I)=0.0
        do 420 m = ISM1,ISM2       
          Total_zone_H(I)=Total_zone_H(I)+TZPHM(I,m)
          Total_zone_C(I)=Total_zone_C(I)+TZPCM(I,m)
  420   continue
  
        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'ZAH',tab,Total_zone_H(I)

        call edisp(IWU,min)                     

        write(min,'(6a,f10.1)') 
     &      LCFGF(1:icfgNameLen),tab,zname(I)(1:iZoneNameLen(I)),
     &      tab,'ZAC',tab,Total_zone_C(I)

        call edisp(IWU,min)  
  
  220 continue

C Close output file after writing.
      IF(IWU.NE.ICOUT)goto 17

   14 IWU=IFIL+1

C This added to get automatic names and is saved in the models folder:
      write(LTMP,'(2a)')cfgroot(1:lnblnk(cfgroot)),'.txt'
      CALL EFOPSEQ(IWU,LTMP,3,IER)
      IF(IER.LT.0)GOTO 14
      goto 16

   17 CALL ERPFREE(IWU,ISTAT)
      goto 11

c Output for save options 1,2 and 3.
    2 CALL edisp(iuout,' Results analysis is done via module res.')
   11 RETURN
      END
