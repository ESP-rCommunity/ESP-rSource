C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-2013.

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

C This file castype.f contains the following routines:

C CASTYP: determines controlled convective casual gains at one timestep.
C ZCASC:  returns one day's timestep values of all convective casual gains via VAL3.
C ZCASR:  returns one day's timestep values of all (surface) rad casual gains via VAL3.
C CASRT:  returns one day's timstep values of all zone rad casual gains 
C ZCASCF: returns one day's unaveraged values of convective casual gains via VAL3. 

C ******************** ZCASC ********************

C ZCASC reconstructs one day's values of each of 
C the uncontrolled and controlled convective casual gains for a zone.
C To save space VAL3(15,I) to VAL3(21,I) are used for temporary storage.
C Casual gain data is read directly from the data fields.
C The parameter IDAY is 
C VAL3(15,?) holds uncontrolled convective casual type 1.
C VAL3(16,?) holds uncontrolled convective casual type 2.
C VAL3(17,?) holds uncontrolled convective casual type 3.
C VAL3(18,?) holds controlled convective casual type 1.
C VAL3(19,?) holds controlled convective casual type 2.
C VAL3(20,?) holds controlled convective casual type 3.

      SUBROUTINE ZCASC(IZONE,IDAY)
#include "building.h"
C #include "geometry.h"
#include "net_flow.h"
#include "tdf2.h"

C      COMMON/OUTIN/IUOUT,IUIN
      common/recver/izver,ipver,iever
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

C      character outs*124
      logical found
      real QCASR,QCASC,QCASL  ! total radiant/convective/latent
      real FRAC ! controlled fraction.
      real perocupc,perocupr,perocupl ! average occupant to write out
      real perlightc,perlightr,perlightl ! average lighting to write out
      real perequipc,perequipr,perequipl ! average equipment to write out
      real otherc,otherr,otherl ! average other (future expansion) to write out
      integer theonectld  ! if non-zero the casual gain type that is controlled.

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
     &      perocupc,perlightc,perequipc,otherc,perocupr,perlightr,
     &      perequipr,otherr,perocupl,perlightl,perequipl,otherl,
     &      theonectld)

C For gains from casual there is no option for control and theonectld
C should be zero.
            VAL3(15,I)=perocupc
            VAL3(16,I)=perlightc
            VAL3(17,I)=perequipc
            VAL3(18,I)=0.0
            VAL3(19,I)=0.0
            VAL3(20,I)=0.0
  141     continue
          return
        else

C Assume that the original description was from a zone operation file.
          DO 142, I=1,IEND
            call getallcas(IDAY,IZONE,ISET,I,QCASR,QCASC,QCASL,FRAC,
     &      perocupc,perlightc,perequipc,otherc,perocupr,perlightr,
     &      perequipr,otherr,perocupl,perlightl,perequipl,otherl,
     &      theonectld)

C Depending on whether there is a controlled casual gain fill the
C 18/19/20 slots with what was recovered from getallcas because
C the explicit data already takes into account that control has
C been applied.
            if(theonectld.eq.0)then
              VAL3(15,I)=perocupc
              VAL3(16,I)=perlightc
              VAL3(17,I)=perequipc
              VAL3(18,I)=0.0
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
            elseif(theonectld.eq.1)then
              VAL3(15,I)=0.0
              VAL3(16,I)=perlightc
              VAL3(17,I)=perequipc
              VAL3(18,I)=perocupc
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
            elseif(theonectld.eq.2)then
              VAL3(15,I)=perocupc
              VAL3(16,I)=0.0
              VAL3(17,I)=perequipc
              VAL3(18,I)=0.0
              VAL3(19,I)=perlightc
              VAL3(20,I)=0.0
            elseif(theonectld.eq.3)then
              VAL3(15,I)=perocupc
              VAL3(16,I)=perlightc
              VAL3(17,I)=0.0
              VAL3(18,I)=0.0
              VAL3(19,I)=0.0
              VAL3(20,I)=perequipc
            endif
  142     continue
          return
        endif
      endif

      END

C ******************** ZCASR ********************

C ZCASR reconstructs one day's values of each of the 
C uncontrolled and controlled radiant casual gains for a 
C surface in a zone.
C To save space VAL3(15,I) to VAL3(21,I) are used for 
C temporary storage. The method used shifts information from the
C operations file into the equivalent of bps timestep averaging.
C To account for area and emissivity weighting compute `emarea`
C based on data previously read in from the construction file.

C VAL3(15,?) holds uncontrolled radiant casual type 1.
C VAL3(16,?) holds uncontrolled radiant casual type 2.
C VAL3(17,?) holds uncontrolled radiant casual type 3.
C VAL3(18,?) holds controlled radiant casual type 1.
C VAL3(19,?) holds controlled radiant casual type 2.
C VAL3(20,?) holds controlled radiant casual type 3.

      SUBROUTINE ZCASR(IZONE,ISFN,IDAY)
#include "building.h"
#include "geometry.h"
#include "net_flow.h"
#include "tdf2.h"

C      COMMON/OUTIN/IUOUT,IUIN
      common/recver/izver,ipver,iever
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/RECV5/sna(MCOM,MS)
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)
      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)

C      character outs*124
      logical found
      real QCASR,QCASC,QCASL  ! total radiant/convective/latent
      real FRAC ! controlled fraction.
      real perocupc,perocupr,perocupl ! average occupant to write out
      real perlightc,perlightr,perlightl ! average lighting to write out
      real perequipc,perequipr,perequipl ! average equipment to write out
      real otherc,otherr,otherl ! average other (future expansion) to write out
      integer theonectld  ! if non-zero the casual gain type that is controlled.

      IEND=24*NTS

C Set chosen result set.
      ISET=ISIM

C Radiant casual gains are area AND emissivity weighted.
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
     &       perocupc,perlightc,perequipc,otherc,perocupr,perlightr,
     &       perequipr,otherr,perocupl,perlightl,perequipl,otherl,
     &       theonectld)
            VAL3(15,I)=perocupr*SCALE
            VAL3(16,I)=perlightr*SCALE
            VAL3(17,I)=perequipr*SCALE
            VAL3(18,I)=0.0
            VAL3(19,I)=0.0
            VAL3(20,I)=0.0
  141     continue
          return   ! got it so return to calling routine
        else

C Assume that the original description was from a zone operation file.
          DO 142, I=1,IEND
            call getallcas(IDAY,IZONE,ISET,I,QCASR,QCASC,QCASL,FRAC,
     &      perocupc,perlightc,perequipc,otherc,perocupr,perlightr,
     &      perequipr,otherr,perocupl,perlightl,perequipl,otherl,
     &      theonectld)

C Depending on whether there is a controlled casual gain fill the
C 18/19/20 slots with what was recovered from getallcas because
C the explicit data already takes into account that control has
C been applied.
            if(theonectld.eq.0)then
              VAL3(15,I)=perocupr*SCALE
              VAL3(16,I)=perlightr*SCALE
              VAL3(17,I)=perequipr*SCALE
              VAL3(18,I)=0.0
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
            elseif(theonectld.eq.1)then
              VAL3(15,I)=0.0
              VAL3(16,I)=perlightr*SCALE
              VAL3(17,I)=perequipr*SCALE
              VAL3(18,I)=perocupr*SCALE
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
            elseif(theonectld.eq.2)then
              VAL3(15,I)=perocupr*SCALE
              VAL3(16,I)=0.0
              VAL3(17,I)=perequipr*SCALE
              VAL3(18,I)=0.0
              VAL3(19,I)=perlightr*SCALE
              VAL3(20,I)=0.0
            elseif(theonectld.eq.3)then
              VAL3(15,I)=perocupr*SCALE
              VAL3(16,I)=perlightr*SCALE
              VAL3(17,I)=0.0
              VAL3(18,I)=0.0
              VAL3(19,I)=0.0
              VAL3(20,I)=perequipr*SCALE
            endif
  142     continue
          return   ! got it so return to calling routine
        endif
      endif

      END


C ******************** CASRT ********************

C CASRT reconstructs one day's values of each type of 
C uncontrolled and controlled radiant casual gains for a zone.
C To save space VAL3(15,I) to VAL3(21,I) are used for 
C temporary storage. The method used shifts information from the
C operations file into the equivalent of bps timestep averaging.

C VAL3(15,?) holds uncontrolled casual type 1.
C VAL3(16,?) holds uncontrolled casual type 2.
C VAL3(17,?) holds uncontrolled casual type 3.
C VAL3(18,?) holds controlled casual type 1.
C VAL3(19,?) holds controlled casual type 2.
C VAL3(20,?) holds controlled casual type 3.

      SUBROUTINE CASRT(IZONE,IDAY)
#include "building.h"
C #include "geometry.h"
#include "net_flow.h"
#include "tdf2.h"

      common/recver/izver,ipver,iever
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

C      character outs*124
      logical found
      real QCASR,QCASC,QCASL  ! total radiant/convective/latent
      real FRAC ! controlled fraction.
      real perocupc,perocupr,perocupl ! average occupant to write out
      real perlightc,perlightr,perlightl ! average lighting to write out
      real perequipc,perequipr,perequipl ! average equipment to write out
      real otherc,otherr,otherl ! average other (future expansion) to write out
      integer theonectld  ! if non-zero the casual gain type that is controlled.

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
     &       perocupc,perlightc,perequipc,otherc,perocupr,perlightr,
     &       perequipr,otherr,perocupl,perlightl,perequipl,otherl,
     &       theonectld)

C For gains from casual there is no option for control and theonectld
C should be zero.
            VAL3(15,I)=perocupr
            VAL3(16,I)=perlightr
            VAL3(17,I)=perequipr
            VAL3(18,I)=0.0
            VAL3(19,I)=0.0
            VAL3(20,I)=0.0
  141     continue
          return  ! recovered what we need so return
        else

C Assume that the original description was from a zone operation file.
          DO 142, I=1,IEND
            call getallcas(IDAY,IZ,ISET,JTS,QCASR,QCASC,QCASL,FRAC,
     &       perocupc,perlightc,perequipc,otherc,perocupr,perlightr,
     &       perequipr,otherr,perocupl,perlightl,perequipl,otherl,
     &       theonectld)

C Depending on whether there is a controlled casual gain fill the
C 18/19/20 slots with what was recovered from getallcas because
C the explicit data already takes into account that control has
C been applied.
            if(theonectld.eq.0)then
              VAL3(15,I)=perocupr
              VAL3(16,I)=perlightr
              VAL3(17,I)=perequipr
              VAL3(18,I)=0.0
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
            elseif(theonectld.eq.1)then
              VAL3(15,I)=0.0
              VAL3(16,I)=perlightr
              VAL3(17,I)=perequipr
              VAL3(18,I)=perocupr
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
            elseif(theonectld.eq.2)then
              VAL3(15,I)=perocupr
              VAL3(16,I)=0.0
              VAL3(17,I)=perequipr
              VAL3(18,I)=0.0
              VAL3(19,I)=perlightr
              VAL3(20,I)=0.0
            elseif(theonectld.eq.3)then
              VAL3(15,I)=perocupr
              VAL3(16,I)=perlightr
              VAL3(17,I)=0.0
              VAL3(18,I)=0.0
              VAL3(19,I)=0.0
              VAL3(20,I)=perequipr
            endif
  142     continue
          return  ! recovered what we need so return
        endif
      endif

      END


C ******************** CASTYP ********************

C CASTYP is a generic routine to work out the
C uncontrolled casual gain types 1, 2 & 3 at each time step.
C Given the information in common block, PC3 and P3TYPE returns:
C UCCT1 uncontrolled casual convective type 1,
C UCCT2 uncontrolled casual convective type 2,
C UCCT3 uncontrolled casual convective type 3,
C UCCRT1 uncontrolled casual radiant type 1,
C UCCRT2 uncontrolled casual radiant type 2,
C UCCRT3 uncontrolled casual radiant type 3,
C ICC, controlled casual type
C IF izver >=4 then use direct reads via getallcas and test
C the values of theonectld. If theonectld non-zero set that
C casual gain to zero (because the controlled has been dealt
C with separately.
      SUBROUTINE CASTYP(IZ,IDAY,ITSTEP,UCCT1,UCCT2,UCCT3,
     &                  UCCRT1,UCCRT2,UCCRT3,ICC)
#include "building.h"
C #include "model.h"
C #include "geometry.h"
C #include "schedule.h"

      common/recver/izver,ipver,iever
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME

      real QCASR,QCASC,QCASL  ! total radiant/convective/latent
      real FRAC ! controlled fraction.
      real perocupc,perocupr,perocupl ! average occupant to write out
      real perlightc,perlightr,perlightl ! average lighting to write out
      real perequipc,perequipr,perequipl ! average equipment to write out
      real otherc,otherr,otherl ! average other (future expansion) to write out
      integer theonectld  ! if non-zero the casual gain type that is controlled.

C Assume that IYEAR is available via RESDEF.
C Determine whether IDAY is a weekday, Saturday or Sunday.
C Determine the month and day of month via EDAYR.
      CALL EDAYR(IDAY,IDAYN,IMTHN)
      CALL EWEEKD(IDAYN,IMTHN,IYEAR,IDWK)

      ICC=0
      UCCT1=0.
      UCCRT1=0.
      UCCT2=0.
      UCCRT2=0.
      UCCT3=0.
      UCCRT3=0.

C Set chosen result set.
      ISET=ISIM

C If izver >= 4 the read the data directly.
      if(izver.ge.4)then

C Assume that the original description was from a zone operation file.
        call getallcas(IDAY,IZ,ISET,ITSTEP,QCASR,QCASC,QCASL,FRAC,
     &    perocupc,perlightc,perequipc,otherc,perocupr,perlightr,
     &    perequipr,otherr,perocupl,perlightl,perequipl,otherl,
     &    theonectld)

C Depending on whether there is a controlled casual gain fill the
C 18/19/20 slots with what was recovered from getallcas because
C the explicit data already takes into account that control has
C been applied.
        if(theonectld.eq.0)then
          UCCT1=perocupc
          UCCT2=perlightc
          UCCT3=perequipc
          UCCRT1=perocupr
          UCCRT2=perlightr
          UCCRT3=perequipr
        elseif(theonectld.eq.1)then
          UCCT1=0.0
          UCCT2=perlightc
          UCCT3=perequipc
          UCCRT1=0.0
          UCCRT2=perlightr
          UCCRT3=perequipr
        elseif(theonectld.eq.2)then
          UCCT1=perocupc
          UCCT2=0.0
          UCCT3=perequipc
          UCCRT1=perocupr
          UCCRT2=0.0
          UCCRT3=perequipr
        elseif(theonectld.eq.3)then
          UCCT1=perocupc
          UCCT2=perlightc
          UCCT3=0.0
          UCCRT1=perocupr
          UCCRT2=perlightr
          UCCRT3=0.0
        endif
        return   ! got what we wanted so return
      endif

      END


C ******************** ZCASCF ********************
C ZCASCF reconstructs one day's unaveraged values of each of 
C the uncontrolled and controlled convective casual gains for a zone.
C To save space VAL3(15,I) to VAL3(21,I) are used for temporary storage.
C The method does not shift/average data from the operations file.
C The parameter IDAY is the day of the week (1-7).
C The parameter IJDAY is the julian day of the year (for the case
C of checking occupancy on a specific day). If IJDAY is zero then
C do not bother.

C VAL3(15,?) holds uncontrolled casual type 1.
C VAL3(16,?) holds uncontrolled casual type 2.
C VAL3(17,?) holds uncontrolled casual type 3.
C VAL3(18,?) holds controlled casual type 1.
C VAL3(19,?) holds controlled casual type 2.
C VAL3(20,?) holds controlled casual type 3.

      SUBROUTINE ZCASCF(IZONE,IDAY,IJDAY)
#include "building.h"
C #include "geometry.h"
#include "net_flow.h"
#include "tdf2.h"

C      COMMON/OUTIN/IUOUT,IUIN
      common/recver/izver,ipver,iever
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET1/VAL1(MZS,MTS),VAL2(MZS,MTS),VAL3(MZRL,MTS)

C      character outs*124
      logical found
      real QCASR,QCASC,QCASL  ! total radiant/convective/latent
      real FRAC ! controlled fraction.
      real perocupc,perocupr,perocupl ! average occupant to write out
      real perlightc,perlightr,perlightl ! average lighting to write out
      real perequipc,perequipr,perequipl ! average equipment to write out
      real otherc,otherr,otherl ! average other (future expansion) to write out
      integer theonectld  ! if non-zero the casual gain type that is controlled.

C Set chosen result set.
      ISET=ISIM

      IEND=24*NTS

C If izver >= 4 the read the casual gain data directly.
      if(izver.ge.4)then
        found = .false.
        if(ICASUAL(izone).ne.0)found = .true.  ! if single casual item
        if(ICASUAL3(izone).ne.0.and.IJDAY.ne.0)found = .true.  ! if separate casual item
        if(IACTIV(izone).ne.0.and.IJDAY.ne.0)found = .true.   ! if UK activity item
        if(IACTIV(izone).ne.0)found = .true.
        if(found)then
          DO 141, I=1,IEND
            call getallcas(IDAY,IZONE,ISET,I,QCASR,QCASC,QCASL,FRAC,
     &      perocupc,perlightc,perequipc,otherc,perocupr,perlightr,
     &      perequipr,otherr,perocupl,perlightl,perequipl,otherl,
     &      theonectld)

C For gains from casual there is no option for control and theonectld
C should be zero.
            VAL3(15,I)=perocupc
            VAL3(16,I)=perlightc
            VAL3(17,I)=perequipc
            VAL3(18,I)=0.0
            VAL3(19,I)=0.0
            VAL3(20,I)=0.0
  141     continue
          return   ! got what we wanted so return
        else

C Assume that the original description was from a zone operation file.
          DO 142, I=1,IEND
            call getallcas(IDAY,IZONE,ISET,I,QCASR,QCASC,QCASL,FRAC,
     &      perocupc,perlightc,perequipc,otherc,perocupr,perlightr,
     &      perequipr,otherr,perocupl,perlightl,perequipl,otherl,
     &      theonectld)

C Depending on whether there is a controlled casual gain fill the
C 18/19/20 slots with what was recovered from getallcas because
C the explicit data already takes into account that control has
C been applied.
            if(theonectld.eq.0)then
              VAL3(15,I)=perocupc
              VAL3(16,I)=perlightc
              VAL3(17,I)=perequipc
              VAL3(18,I)=0.0
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
            elseif(theonectld.eq.1)then
              VAL3(15,I)=0.0
              VAL3(16,I)=perlightc
              VAL3(17,I)=perequipc
              VAL3(18,I)=perocupc
              VAL3(19,I)=0.0
              VAL3(20,I)=0.0
            elseif(theonectld.eq.2)then
              VAL3(15,I)=perocupc
              VAL3(16,I)=0.0
              VAL3(17,I)=perequipc
              VAL3(18,I)=0.0
              VAL3(19,I)=perlightc
              VAL3(20,I)=0.0
            elseif(theonectld.eq.3)then
              VAL3(15,I)=perocupc
              VAL3(16,I)=perlightc
              VAL3(17,I)=0.0
              VAL3(18,I)=0.0
              VAL3(19,I)=0.0
              VAL3(20,I)=perequipc
            endif
  142     continue
          return   ! got what we wanted so return
        endif
      endif

      END

