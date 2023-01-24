C ESP-r is free software. You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.

C This file contains the following routines:

C ZCASC:  returns one day's timestep values of all convective casual gains via VAL3.
C ZCASR:  returns one day's timestep values of all (surface) rad casual gains via VAL3.
C CASRT:  returns one day's timstep values of all zone rad casual gains 
C ZCASCF: returns one day's unaveraged values of convective casual gains via VAL3. 

C ************ ZCASC
C Reconstructs one day's values of each of the uncontrolled and controlled
C convective casual gains for a zone. To save space VAL3(15,I) to VAL3(22,I)
C are used for temporary storage.
C    VAL3(15,?) holds uncontrolled casual slot 1.
C    VAL3(16,?) holds uncontrolled casual slot 2.
C    VAL3(17,?) holds uncontrolled casual slot 3.
C    VAL3(18,?) holds controlled casual slot 4.
C    VAL3(19,?) holds controlled casual type 1.
C    VAL3(20,?) holds controlled casual type 2.
C    VAL3(21,?) holds controlled casual type 3.
C    VAL3(22,?) holds controlled casual type 4.

      SUBROUTINE ZCASC(IZONE,IDAY)
#include "building.h"
#include "net_flow.h"
#include "tdf2.h"

      common/recver/izver,ipver,iever
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      logical found
      real QCASR,QCASC,QCASL  ! total radiant/convective/latent
      real FRAC               ! controlled fraction.
      real perocupc,perocupr,perocupl    ! average occupant to write out
      real perlightc,perlightr,perlightl ! average lighting to write out
      real perequipc,perequipr,perequipl ! average equipment to write out
      real otherc,otherr,otherl ! average other (future expansion) to write out
      integer theonectld        ! if non-zero the casual gain type that is controlled.

C Set chosen result set.
      ISET=ISIM

      IEND=24*NTS

C For izver >= 4 the read the casual gain data directly.
      if(izver.ge.4)then
        found = .false.
        if(ICASUAL(izone).ne.0)found = .true.  ! if single casual item
        if(ICASUAL3(izone).ne.0)found = .true. ! if 3 separate casual item
        if(IACTIV(izone).ne.0)found = .true.   ! if UK activity item
        if(found)then
          DO 141, I=1,IEND
            call getallcas(IDAY,IZONE,ISET,I,QCASR,QCASC,QCASL,FRAC,
     &      perocupc,perlightc,perequipc,perotherc,perocupr,perlightr,
     &      perequipr,perotherr,perocupl,perlightl,perequipl,perotherl,
     &      theonectld)

C For gains from casual there is no option for control and theonectld
C should be zero.
            VAL3(15,I)=perocupc
            VAL3(16,I)=perlightc
            VAL3(17,I)=perequipc
            VAL3(18,I)=perotherc
            VAL3(19,I)=0.0
            VAL3(20,I)=0.0
            VAL3(21,I)=0.0
            VAL3(22,I)=0.0
  141     continue
          return
        else

C Assume that the original description was from a zone operation file.
          DO 142, I=1,IEND
            call getallcas(IDAY,IZONE,ISET,I,QCASR,QCASC,QCASL,FRAC,
     &      perocupc,perlightc,perequipc,perotherc,perocupr,perlightr,
     &      perequipr,perotherr,perocupl,perlightl,perequipl,perotherl,
     &      theonectld)

C Depending on whether there is a controlled casual gain fill the
C 18/19/20 slots with what was recovered from getallcas because
C the explicit data already takes into account that control has
C been applied.
            if(theonectld.eq.0)then
              VAL3(15,I)=perocupc
              VAL3(16,I)=perlightc
              VAL3(17,I)=perequipc
              VAL3(18,I)=perotherc
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
              VAL3(21,I)=0.0
              VAL3(22,I)=0.0
            elseif(theonectld.eq.1)then
              VAL3(15,I)=0.0
              VAL3(16,I)=perlightc
              VAL3(17,I)=perequipc
              VAL3(18,I)=perotherc
              VAL3(19,I)=perocupc
              VAL3(20,I)=0.0
              VAL3(21,I)=0.0
              VAL3(22,I)=0.0
            elseif(theonectld.eq.2)then
              VAL3(15,I)=perocupc
              VAL3(16,I)=0.0
              VAL3(17,I)=perequipc
              VAL3(18,I)=perotherc
              VAL3(19,I)=0.0
              VAL3(20,I)=perlightc
              VAL3(21,I)=0.0
              VAL3(22,I)=0.0
            elseif(theonectld.eq.3)then
              VAL3(15,I)=perocupc
              VAL3(16,I)=perlightc
              VAL3(17,I)=0.0
              VAL3(18,I)=perotherc
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
              VAL3(21,I)=perequipc
              VAL3(22,I)=0.0
             elseif(theonectld.eq.4)then
              VAL3(15,I)=perocupc
              VAL3(16,I)=perlightc
              VAL3(17,I)=perequipc
              VAL3(18,I)=0.0
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
              VAL3(21,I)=0.0
              VAL3(22,I)=perotherc
            endif
  142     continue
          return
        endif
      endif

      END

C ******************** ZCASR ********************

C Reconstructs one day's values of each of the uncontrolled and controlled
C radiant casual gains for a surface in a zone. To save space VAL3(15,I) to VAL3(22,I)
C are used for temporary storage. To account for area and emissivity weighting,
C compute `emarea` based on data previously read from the zone construction file.
C    VAL3(15,?) holds uncontrolled casual slot 1.
C    VAL3(16,?) holds uncontrolled casual slot 2.
C    VAL3(17,?) holds uncontrolled casual slot 3.
C    VAL3(18,?) holds uncontrolled casual slot 4.
C    VAL3(19,?) holds controlled casual type 1.
C    VAL3(20,?) holds controlled casual type 2.
C    VAL3(21,?) holds controlled casual type 3.
C    VAL3(22,?) holds controlled casual type 4.

      SUBROUTINE ZCASR(IZONE,ISFN,IDAY)
#include "building.h"
#include "geometry.h"
#include "net_flow.h"
#include "tdf2.h"

      common/recver/izver,ipver,iever
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)

      logical found
      real QCASR,QCASC,QCASL     ! total radiant/convective/latent
      real FRAC                  ! controlled fraction.
      real perocupc,perocupr,perocupl    ! average occupant to write out
      real perlightc,perlightr,perlightl ! average lighting to write out
      real perequipc,perequipr,perequipl ! average equipment to write out
      real perotherc,perotherr,perotherl ! average other (future expansion) to write out
      integer theonectld         ! if non-zero the casual gain type that is controlled.

      IEND=24*NTS

C Set chosen result set.
      ISET=ISIM

C Radiant casual gains are area and emissivity weighted.
      emarea=0.0
      do 80 kk=1,NZSUR(izone)
        emarea=emarea+sna(izone,kk)*EMISI(kk)
  80  continue

C For izver >= 4 the read the data directly and multiply the radiant
C value by SCALE. Similar pattern of code used elsewhere for izver >=4.
      if(izver.ge.4)then
        found = .false.
        if(ICASUAL(izone).ne.0)found = .true.  ! if single casual item
        if(ICASUAL3(izone).ne.0)found = .true. ! if 3 separate casual item
        if(IACTIV(izone).ne.0)found = .true.   ! if UK activity item
        SCALE=sna(IZONE,ISFN)*EMISI(ISFN)/emarea
        if(found)then
          DO 141, I=1,IEND
            call getallcas(IDAY,IZONE,ISET,I,QCASR,QCASC,QCASL,FRAC,
     &       perocupc,perlightc,perequipc,perotherc,perocupr,perlightr,
     &       perequipr,perotherr,perocupl,perlightl,perequipl,perotherl,
     &       theonectld)
            VAL3(15,I)=perocupr*SCALE
            VAL3(16,I)=perlightr*SCALE
            VAL3(17,I)=perequipr*SCALE
            VAL3(18,I)=perotherr*SCALE
            VAL3(19,I)=0.0
            VAL3(20,I)=0.0
            VAL3(21,I)=0.0
            VAL3(22,I)=0.0
  141     continue
          return   ! got it so return to calling routine
        else

C Assume that the original description was from a zone operation file.
          DO 142, I=1,IEND
            call getallcas(IDAY,IZONE,ISET,I,QCASR,QCASC,QCASL,FRAC,
     &      perocupc,perlightc,perequipc,perotherc,perocupr,perlightr,
     &      perequipr,perotherr,perocupl,perlightl,perequipl,perotherl,
     &      theonectld)

C Depending on whether there is a controlled casual gain, fill the
C 15-19 slots with what was recovered from getallcas because
C the explicit data already takes into account that control has
C been applied.
            if(theonectld.eq.0)then
              VAL3(15,I)=perocupr*SCALE
              VAL3(16,I)=perlightr*SCALE
              VAL3(17,I)=perequipr*SCALE
              VAL3(18,I)=perotherr*SCALE
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
              VAL3(21,I)=0.0
              VAL3(22,I)=0.0
            elseif(theonectld.eq.1)then
              VAL3(15,I)=0.0
              VAL3(16,I)=perlightr*SCALE
              VAL3(17,I)=perequipr*SCALE
              VAL3(18,I)=perotherr*SCALE
              VAL3(19,I)=perocupr*SCALE
              VAL3(20,I)=0.0
              VAL3(21,I)=0.0
              VAL3(22,I)=0.0
            elseif(theonectld.eq.2)then
              VAL3(15,I)=perocupr*SCALE
              VAL3(16,I)=0.0
              VAL3(17,I)=perequipr*SCALE
              VAL3(18,I)=perotherr*SCALE
              VAL3(19,I)=0.0
              VAL3(20,I)=perlightr*SCALE
              VAL3(21,I)=0.0
              VAL3(22,I)=0.0
            elseif(theonectld.eq.3)then
              VAL3(15,I)=perocupr*SCALE
              VAL3(16,I)=perlightr*SCALE
              VAL3(17,I)=0.0
              VAL3(18,I)=perotherr*SCALE
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
              VAL3(21,I)=perequipr*SCALE
              VAL3(22,I)=0.0
            elseif(theonectld.eq.4)then
              VAL3(15,I)=perocupr*SCALE
              VAL3(16,I)=perlightr*SCALE
              VAL3(17,I)=perequipr*SCALE
              VAL3(18,I)=0.0
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
              VAL3(21,I)=0.0
              VAL3(22,I)=perotherr*SCALE
            endif
  142     continue
          return   ! got it so return to calling routine
        endif
      endif

      END


C ******************** CASRT ********************

C Reconstructs one day's values of each type of 
C uncontrolled and controlled radiant casual gains for a zone.
C To save space VAL3(15,I) to VAL3(22,I) are used for 
C temporary storage.
C    VAL3(15,?) holds uncontrolled casual slot 1.
C    VAL3(16,?) holds uncontrolled casual slot 2.
C    VAL3(17,?) holds uncontrolled casual slot 3.
C    VAL3(18,?) holds controlled casual slot 4.
C    VAL3(19,?) holds controlled casual type 1.
C    VAL3(20,?) holds controlled casual type 2.
C    VAL3(21,?) holds controlled casual type 3.
C    VAL3(22,?) holds controlled casual type 4.

      SUBROUTINE CASRT(IZONE,IDAY)
#include "building.h"
#include "net_flow.h"
#include "tdf2.h"

      common/recver/izver,ipver,iever
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      logical found
      real QCASR,QCASC,QCASL    ! total radiant/convective/latent
      real FRAC                 ! controlled fraction.
      real perocupc,perocupr,perocupl ! average occupant to write out
      real perlightc,perlightr,perlightl ! average lighting to write out
      real perequipc,perequipr,perequipl ! average equipment to write out
      real perotherc,perotherr,perotherl ! average other (future expansion) to write out
      integer theonectld        ! if non-zero the casual gain type that is controlled.

      IEND=24*NTS

C Set chosen result set.
      ISET=ISIM

C If izver >= 4 the read the data directly.
      if(izver.ge.4)then
        found = .false.
        if(ICASUAL(izone).ne.0)found = .true.  ! if single casual item
        if(ICASUAL3(izone).ne.0)found = .true. ! if 3 separate casual item
        if(IACTIV(izone).ne.0)found = .true.   ! if UK activity item
        if(found)then
          DO 141, I=1,IEND
            call getallcas(IDAY,IZ,ISET,JTS,QCASR,QCASC,QCASL,FRAC,
     &       perocupc,perlightc,perequipc,perotherc,perocupr,perlightr,
     &       perequipr,perotherr,perocupl,perlightl,perequipl,perotherl,
     &       theonectld)

C For gains from casual there is no option for control and the onectld
C should be zero.
            VAL3(15,I)=perocupr
            VAL3(16,I)=perlightr
            VAL3(17,I)=perequipr
            VAL3(18,I)=perotherr
            VAL3(19,I)=0.0
            VAL3(20,I)=0.0
            VAL3(21,I)=0.0
            VAL3(22,I)=0.0
  141     continue
          return  ! recovered what we need so return
        else

C Assume that the original description was from a zone operation file.
          DO 142, I=1,IEND
            call getallcas(IDAY,IZ,ISET,JTS,QCASR,QCASC,QCASL,FRAC,
     &       perocupc,perlightc,perequipc,perotherc,perocupr,perlightr,
     &       perequipr,perotherr,perocupl,perlightl,perequipl,perotherl,
     &       theonectld)

C Depending on whether there is a controlled casual gain fill the
C 19/20/21/22 slots with what was recovered from getallcas because
C the explicit data already takes into account that control has
C been applied.
            if(theonectld.eq.0)then
              VAL3(15,I)=perocupr
              VAL3(16,I)=perlightr
              VAL3(17,I)=perequipr
              VAL3(18,I)=perotherr
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
              VAL3(21,I)=0.0
              VAL3(22,I)=0.0
            elseif(theonectld.eq.1)then
              VAL3(15,I)=0.0
              VAL3(16,I)=perlightr
              VAL3(17,I)=perequipr
              VAL3(18,I)=perotherr
              VAL3(19,I)=perocupr
              VAL3(20,I)=0.0
              VAL3(21,I)=0.0
              VAL3(22,I)=0.0
            elseif(theonectld.eq.2)then
              VAL3(15,I)=perocupr
              VAL3(16,I)=0.0
              VAL3(17,I)=perequipr
              VAL3(18,I)=perotherr
              VAL3(19,I)=0.0
              VAL3(20,I)=perlightr
              VAL3(21,I)=0.0
              VAL3(22,I)=0.0
            elseif(theonectld.eq.3)then
              VAL3(15,I)=perocupr
              VAL3(16,I)=perlightr
              VAL3(17,I)=0.0
              VAL3(18,I)=perotherr
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
              VAL3(21,I)=perequipr
              VAL3(22,I)=0.0
            elseif(theonectld.eq.4)then
              VAL3(15,I)=perocupr
              VAL3(16,I)=perlightr
              VAL3(17,I)=perequipr
              VAL3(18,I)=0.0
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
              VAL3(21,I)=0.0
              VAL3(22,I)=perotherr
            endif
  142     continue
          return  ! recovered what we need so return
        endif
      endif

      END

C ************ ZCASCF
C Reconstructs one day's unaveraged values of each of 
C the uncontrolled and controlled convective casual gains for a zone.
C To save space VAL3(15,I) to VAL3(22,I) are used for temporary storage.
C The method does not shift/average data from the operations file.
C The parameter IDAY is the day of the week (1-7).
C The parameter IJDAY is the julian day of the year (for the case
C of checking occupancy on a specific day). If IJDAY is zero then
C do not bother.
C    VAL3(15,?) holds uncontrolled casual slot 1.
C    VAL3(16,?) holds uncontrolled casual slot 2.
C    VAL3(17,?) holds uncontrolled casual slot 3.
C    VAL3(18,?) holds uncontrolled casual slot 4.
C    VAL3(19,?) holds controlled casual type 1.
C    VAL3(20,?) holds controlled casual type 2.
C    VAL3(21,?) holds controlled casual type 3.
C    VAL3(22,?) holds controlled casual type 4.

      SUBROUTINE ZCASCF(IZONE,IDAY,IJDAY)
#include "building.h"
#include "net_flow.h"
#include "tdf2.h"

      common/recver/izver,ipver,iever
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

      logical found
      real QCASR,QCASC,QCASL  ! total radiant/convective/latent
      real FRAC ! controlled fraction.
      real perocupc,perocupr,perocupl ! average occupant to write out
      real perlightc,perlightr,perlightl ! average lighting to write out
      real perequipc,perequipr,perequipl ! average equipment to write out
      real perotherc,perotherr,perotherl ! average other (future expansion) to write out
      integer theonectld  ! if non-zero the casual gain type that is controlled.

C Set chosen result set.
      ISET=ISIM

      IEND=24*NTS

C If izver >= 4 the read the casual gain data directly.
      if(izver.ge.4)then
        found = .false.
        if(ICASUAL(izone).ne.0)found = .true.   ! if single casual item
        if(ICASUAL3(izone).ne.0.and.IJDAY.ne.0)found = .true.  ! if separate casual item
        if(IACTIV(izone).ne.0.and.IJDAY.ne.0)found = .true.    ! if UK activity item
        if(IACTIV(izone).ne.0)found = .true.
        if(found)then
          DO 141, I=1,IEND
            call getallcas(IDAY,IZONE,ISET,I,QCASR,QCASC,QCASL,FRAC,
     &      perocupc,perlightc,perequipc,perotherc,perocupr,perlightr,
     &      perequipr,perotherr,perocupl,perlightl,perequipl,perotherl,
     &      theonectld)

C For gains from casual there is no option for control and theonectld
C should be zero.
            VAL3(15,I)=perocupc
            VAL3(16,I)=perlightc
            VAL3(17,I)=perequipc
            VAL3(18,I)=perotherc
            VAL3(19,I)=0.0
            VAL3(20,I)=0.0
            VAL3(21,I)=0.0
            VAL3(22,I)=0.0
  141     continue
          return   ! got what we wanted so return
        else

C Assume that the original description was from a zone operation file.
          DO 142, I=1,IEND
            call getallcas(IDAY,IZONE,ISET,I,QCASR,QCASC,QCASL,FRAC,
     &      perocupc,perlightc,perequipc,perotherc,perocupr,perlightr,
     &      perequipr,perotherr,perocupl,perlightl,perequipl,perotherl,
     &      theonectld)

C Depending on whether there is a controlled casual gain fill the
C 18/19/20 slots with what was recovered from getallcas because
C the explicit data already takes into account that control has
C been applied.
            if(theonectld.eq.0)then
              VAL3(15,I)=perocupc
              VAL3(16,I)=perlightc
              VAL3(17,I)=perequipc
              VAL3(18,I)=perotherc
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
              VAL3(21,I)=0.0
              VAL3(22,I)=0.0
            elseif(theonectld.eq.1)then
              VAL3(15,I)=0.0
              VAL3(16,I)=perlightc
              VAL3(17,I)=perequipc
              VAL3(18,I)=perotherc
              VAL3(19,I)=perocupc
              VAL3(20,I)=0.0
              VAL3(21,I)=0.0
              VAL3(22,I)=0.0
            elseif(theonectld.eq.2)then
              VAL3(15,I)=perocupc
              VAL3(16,I)=0.0
              VAL3(17,I)=perequipc
              VAL3(18,I)=perotherc
              VAL3(19,I)=0.0
              VAL3(20,I)=perlightc
              VAL3(21,I)=0.0
              VAL3(22,I)=0.0
             elseif(theonectld.eq.3)then
              VAL3(15,I)=perocupc
              VAL3(16,I)=perlightc
              VAL3(17,I)=0.0
              VAL3(18,I)=perotherc
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
              VAL3(21,I)=perequipc
              VAL3(22,I)=0.0
             elseif(theonectld.eq.4)then
              VAL3(15,I)=perocupc
              VAL3(16,I)=perlightc
              VAL3(17,I)=perequipc
              VAL3(18,I)=0.0
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
              VAL3(21,I)=0.0
              VAL3(22,I)=perotherc
            endif
  142     continue
          return   ! got what we wanted so return
        endif
      endif

      END

