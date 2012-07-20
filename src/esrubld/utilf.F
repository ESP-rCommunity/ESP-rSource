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
C    MZHCFV:  reads the convection-calculation control file.
C    MZHCFV2: reads the convection-regime file (version 2)
C    MZSHIN:  checks validity of a zone shading file
C    MZVFIN:  reads geometric view factor between surface pairs from file

C ******************** MZHCFV ********************

C MZHCFV reads the convection regime file.

      SUBROUTINE MZHCFV(ICOMP)
#include "building.h"
#include "geometry.h"
#include "model.h"

      COMMON/ER/IER
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/TC/ITC,ICNT

      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)

      COMMON/HCFIX2/NHCFP(MCOM),HCFPST(MCOM,MBP),HCFPFT(MCOM,MBP),
     &              HCFVI(MCOM,MS,MBP),HCFVE(MCOM,MS,MBP)

      common/HCTL/iCTLTP(MCOM,MBP),IcIuse(MCOM,MS,MBP),
     &            IcEuse(MCOM,MS,MBP),icnvKY(MCOM,MS,MBP),
     &            CVdat(MS*MBP,8)
      COMMON/HCCEN2/CENLOC(MCOM,MBP,14)
      COMMON/VERTC/IAORZ,CWIDTH,CHEIGHT

      character outs*124,OUTSTR*124
      logical OK,dok

      DIMENSION HIV(MS),HEV(MS)

C NHCFP(ICOMP) is the number of control time intervals.
C Variables used for `type 1' fixed coefficients control:
C    HCFVI(i,j,k): interior HTC for zone `i', surface `j', time interval `k'.
C    HCFVE(i,j,k): exterior HTC for zone `i', surface `j', time interval `k'.
C Variables used for `type 2' fixed correlations control:
C    IcIuse(i,j,k): correlation-equation index for interior of zone `i',
C                   surface `j', time interval `k'.
C    IcEuse(i,j,k): correlation-equation index for exterior of zone `i',
C                   surface `j', time interval `k'.
C Variables used for `type 3' adaptive control:
C    icnvKY(i,j,k): index for zone `i', surface `j', time interval `k'.
C    II: counter used for the icnvKY() index array.
C    CVdat(II,jj): data items (`jj') for index `II':
C       jj=1:   control law for interior surface
C       jj=2-4: data items associated with control law for interior
C       jj=5:   control law for exterior surface
C       jj=6-8: data items associated with control law for exterior
C Variables used for `type 4' CEN coefficients control:
C   CENLOC(i,k,jj) hc values for zone 'i', time interval 'k':
C       jj=1 index of a floor surface
C       jj=2 index of a ceiling surface
C       jj=3-5 floor upwd_inside, downwd_inside, external
C       jj=6-8 ceiling upwd_inside, downwd_inside, external
C       jj=9-11 sloped upwd_inside, downwd_inside, external
C       jj=12-14 wall inside, partition, external

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MZHCFV'  ! set for subroutine

C Initialize data.
      NHCFP(ICOMP)=0
      II=0
      IUNIT=IFIL+1

C Check for old format.
      CALL EFOPSEQ(IUNIT,LHCCO(ICOMP),1,IER)
      IF(ier.eq.0)THEN
C-------Does the 4th line contain a control `type' or HC data?
        READ(IUNIT,'(//)',IOSTAT=ISTAT,ERR=1001)
        CALL STRIPC(IUNIT,OUTSTR,99,num,1,'htc line 4',IER)
        if(num.ne.1)then
          call edisp(iuout,
     &      ' HTC file in old format: file must be updated.')
          CALL ERPFREE(IUNIT,ISTAT)
          stop ' HTC file in old format: file must be updated.'
        endif
      ENDIF
      CALL ERPFREE(IUNIT,ISTAT)

C Open convection file.
      CALL EFOPSEQ(IUNIT,LHCCO(ICOMP),1,IER)
      IF(ier.ne.0)goto 1000

C Read the number of control intervals.
      READ(IUNIT,*,IOSTAT=ISTAT,ERR=1001,END=1002)N
      IF(N.LT.1.OR.N.GT.MBP)goto 1003
      NHCFP(ICOMP)=N

C Read the number of mlc's (sometimes referred to as `surfaces', but
C there are actually two surfaces--interior and exterior--for each mlc).
      READ(IUNIT,*,IOSTAT=ISTAT,ERR=1001,END=1002)NS
      IF(NSUR.NE.NS)goto 1007

C Iterate through each time interval.
      DO 10 I=1,N

C Read start and end times of interval.
        READ(IUNIT,*,IOSTAT=ISTAT,ERR=1001,END=1002)ST,FT
        IF(ST.LT.0.OR.ST.GT.24.)goto 1004
        IF(FT.LT.0.OR.FT.GT.24.)goto 1005
        IF(FT.LT.ST)goto 1006
        HCFPST(ICOMP,I)=ST
        HCFPFT(ICOMP,I)=FT

C Read control `type' for interval.  This controls how the remaining
C data is read in.
        READ(IUNIT,*,IOSTAT=ISTAT,ERR=1001,END=1002) Itype
        if(Itype.lt.1 .or. Itype.gt.4) goto 1010
        iCTLTP(ICOMP,I) = Itype

        if(Itype.eq.1) then

C User specifies coefficients for each surface.
C Read in convection coefficients for interior and exterior surfaces.
          READ(IUNIT,*,IOSTAT=ISTAT,ERR=1001,END=1002)(HIV(J),J=1,NS)
          READ(IUNIT,*,IOSTAT=ISTAT,ERR=1001,END=1002)(HEV(J),J=1,NS)

C Assign convection coefficients to arrays.  Ensure there will be
C heat transfer at the exterior surface.
          DO 20 J=1,NS
            IF(ABS(HEV(J)).LT.0.00001.AND.EMISE(J).GT.0.0)goto 1009
            HCFVI(ICOMP,J,I)=HIV(J)
            HCFVE(ICOMP,J,I)=HEV(J)
   20     CONTINUE
        elseif(Itype.eq.2) then

C User specifies the correlation to use for each surface.
C Read in the correlation indices.
          READ(IUNIT,*,IOSTAT=ISTAT,ERR=1001,END=1002)
     &         (IcIuse(ICOMP,J,I),J=1,NS)
          READ(IUNIT,*,IOSTAT=ISTAT,ERR=1001,END=1002)
     &         (IcEuse(ICOMP,J,I),J=1,NS)

        elseif(Itype.eq.3) then

C User specifies `adaptive' control.
C Read in control laws and associated data. One line of data for each mlc.
          do 30 J=1,NS
            II = II + 1
            icnvKY(ICOMP,J,I) = II
            READ(IUNIT,*,IOSTAT=ISTAT,ERR=1001,END=1003)
     &           (CVdat(II,jj),jj=1,8)
   30     continue

C Read IAORZ,CWIDTH and CHEIGHT of ventilated channels. Close
C the file if this line was not found.
          READ(IUNIT,*,IOSTAT=ISTAT,ERR=1002,END=2003)
     &      IAORZ,CWIDTH,CHEIGHT

        elseif(Itype.eq.4) then

C CEN mode sets values for specific conditions.
          CALL STRIPC(IUNIT,OUTSTR,99,num,1,'htc cen base top',IER)
          if(OUTSTR(1:8).eq.'*basetop')then
            K=8
            CALL EGETWR(OUTSTR,K,V1,1.,100.,'W','htc cen base',IER)
            CALL EGETWR(OUTSTR,K,V2,1.,100.,'W','htc cen top',IER)
            CENLOC(icomp,i,1)=V1
            CENLOC(icomp,i,2)=V2
          else
            goto 1011
          endif
          CALL STRIPC(IUNIT,OUTSTR,99,num,1,'htc cen floor',IER)
          if(OUTSTR(1:6).eq.'*floor')then
            K=6
            CALL EGETWR(OUTSTR,K,V1,0.,30.,'F','htc cen upward',IER)
            CALL EGETWR(OUTSTR,K,V2,0.,30.,'F','htc cen down',IER)
            CALL EGETWR(OUTSTR,K,V3,0.,30.,'F','htc cen ext',IER)
            CENLOC(icomp,i,3)=V1
            CENLOC(icomp,i,4)=V2
            CENLOC(icomp,i,5)=V3
          else
            goto 1011
          endif
          CALL STRIPC(IUNIT,OUTSTR,99,num,1,'htc cen ceiling',IER)
          if(OUTSTR(1:8).eq.'*ceiling')then
            K=8
            CALL EGETWR(OUTSTR,K,V1,0.,30.,'F','htc cen upward',IER)
            CALL EGETWR(OUTSTR,K,V2,0.,30.,'F','htc cen down',IER)
            CALL EGETWR(OUTSTR,K,V3,0.,30.,'F','htc cen ext',IER)
            CENLOC(icomp,i,6)=V1
            CENLOC(icomp,i,7)=V2
            CENLOC(icomp,i,8)=V3
          else
            goto 1012
          endif
          CALL STRIPC(IUNIT,OUTSTR,99,num,1,'htc cen slope',IER)
          if(OUTSTR(1:6).eq.'*slope')then
            K=6
            CALL EGETWR(OUTSTR,K,V1,0.,30.,'F','htc cen upward',IER)
            CALL EGETWR(OUTSTR,K,V2,0.,30.,'F','htc cen down',IER)
            CALL EGETWR(OUTSTR,K,V3,0.,30.,'F','htc cen ext',IER)
            CENLOC(icomp,i,9)=V1
            CENLOC(icomp,i,10)=V2
            CENLOC(icomp,i,11)=V3
          else
            goto 1013
          endif
          CALL STRIPC(IUNIT,OUTSTR,99,num,1,'htc cen wall',IER)
          if(OUTSTR(1:5).eq.'*wall')then
            K=5
            CALL EGETWR(OUTSTR,K,V1,0.,30.,'F','htc cen inside',IER)
            CALL EGETWR(OUTSTR,K,V2,0.,30.,'F','htc cen ptn',IER)
            CALL EGETWR(OUTSTR,K,V3,0.,30.,'F','htc cen ext',IER)
            CENLOC(icomp,i,12)=V1
            CENLOC(icomp,i,13)=V2
            CENLOC(icomp,i,14)=V3
          else
            goto 1014
          endif
        else

C Invalid control type specified.
          goto 1010
        endif
   10 CONTINUE

C Free file.
 2003 CALL ERPFREE(IUNIT,ISTAT)

C Trace output ?
    8 IF(ITC.EQ.1)goto 5
      IF(ITRACE(6).EQ.1.AND.IZNTRC(ICOMP).EQ.1)goto  5
      goto 6
 1000 IER=1
      write(outs,7)ICOMP
    7 format(' Zone',I3,' convective coefficient file error.')
      call edisp(iuout,outs)
      CALL edisp(iuout,' run being terminated.')
      goto 8
 1001 CALL edisp(iuout,' Convective coef file read error.')
      goto 1000
 1002 CALL edisp(iuout,' End of convective coef file sensed.')
      goto 1000
 1003 CALL edisp(iuout,' Number of conv coef periods out of range.')
      goto 1000
 1004 CALL edisp(iuout,' Conv coef control start out of range.')
      goto 1000
 1005 CALL edisp(iuout,' Conv coef control finish out of range.')
      goto 1000
 1006 CALL edisp(iuout,' Conv coef cntrl cannot extend over midnight.')
      goto 1000
 1007 call edisp(iuout,' Number of surfaces do not match.')
      goto 1000
 1009 call edisp(iuout,' A zero surface convection coefficient')
      call edisp(iuout,' corresponding to a finite surface emissivity')
      call edisp(iuout,' defines a physically unrealistic model')
      call edisp(iuout,' which might lead to numerical instability.')
      call epwait
      call epagend
      STOP
 1010 call edisp(iuout,' Invalid HTC control type specified')
      goto 1000
 1011 call edisp(iuout,' Expecting *floor in htc file.')
      goto 1000
 1012 call edisp(iuout,' Expecting *ceil in htc file.')
      goto 1000
 1013 call edisp(iuout,' Expecting *slope in htc file.')
      goto 1000
 1014 call edisp(iuout,' Expecting *wall in htc file.')
      goto 1000

C Trace output follows.
    5 CONTINUE 
      dok=.false.
      helptopic='hc_coef_trace_dialog'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL ASKOK(' ',' Convective coefficient file output?',
     &  OK,dok,nbhelp)
      IF(.NOT.OK)goto 6
      write(outs,'(A,I4)')' Subroutine MZHCFV      Trace output',ICNT
      call edisp(itu,outs)
      write(outs,'(23x,A,I7)')'Zone',ICOMP
      call edisp(itu,outs)
      call edisp(itu,' ')
      ICNT=ICNT+1
      call edisp(itu,' Control Start Finish')
      N=NHCFP(ICOMP)
      DO 9994 I=1,N
        write(outs,'(I6,2I7)')I,HCFPST(ICOMP,I),HCFPFT(ICOMP,I)
        call edisp(itu,outs)
        DO 9992 J=1,NS
          write(outs,'(2F15.3)')HCFVI(ICOMP,J,I),HCFVE(ICOMP,J,I)
          call edisp(itu,outs)
 9992   CONTINUE
 9994 CONTINUE
    6 IF(IER.EQ.5)goto 9
      RETURN

C Fatal error trap.
    9 call epwait
      call epagend
      STOP

      END

C ******************** MZHCFV2 ********************
C MZHCFV2 reads the convection-regime file (version 2)

      SUBROUTINE MZHCFV2(ICOMP)
#include "building.h"
#include "geometry.h"
#include "model.h"
#include "espriou.h"
      
      integer lnblnk  ! function definition

      COMMON/ER/IER
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/TC/ITC,ICNT

      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)

      COMMON/HCFIX2/NHCFP(MCOM),HCFPST(MCOM,MBP),HCFPFT(MCOM,MBP),
     &              HCFVI(MCOM,MS,MBP),HCFVE(MCOM,MS,MBP)

      common/HCTL/iCTLTP(MCOM,MBP),IcIuse(MCOM,MS,MBP),
     &            IcEuse(MCOM,MS,MBP),icnvKY(MCOM,MS,MBP),
     &            CVdat(MS*MBP,8)
      common/HCFPHI/hcfpdescr(MBP)
      COMMON/VERTC/IAORZ,CWIDTH,CHEIGHT
      COMMON/HCCEN2/CENLOC(MCOM,MBP,14)

      character outs*124,OUTSTR*124,word*24
      character hcfpdescr*72
      logical OK,dok,newer

      dimension HCIA(MS),HCEA(MS),IHCIA(MS),IHCEA(MS)

C NHCFP(ICOMP) is the number of control time intervals.
C Variables used for `type 1' control:
C    HCFVI(i,j,k): interior HTC for zone `i', surface `j', time interval `k'.
C    HCFVE(i,j,k): exterior HTC for zone `i', surface `j', time interval `k'.
C Variables used for `type 2' control:
C    IcIuse(i,j,k): correlation-equation index for interior of zone `i',
C                   surface `j', time interval `k'.
C    IcEuse(i,j,k): correlation-equation index for exterior of zone `i',
C                   surface `j', time interval `k'.
C Variables used for `type 3' control:
C    icnvKY(i,j,k): index for zone `i', surface `j', time interval `k'.
C    II: counter used for the icnvKY() index array.
C    CVdat(II,jj): data items (`jj') for index `II':
C       jj=1:   control law for interior surface
C       jj=2-4: data items associated with control law for interior
C       jj=5:   control law for exterior surface
C       jj=6-8: data items associated with control law for exterior
C Variables used for `type 4' control:
C   CENLOC(i,k,jj) hc values for zone 'i', time interval 'k':
C       jj=1 index of a floor surface
C       jj=2 index of a ceiling surface
C       jj=3-5 floor upwd_inside, downwd_inside, external
C       jj=6-8 ceiling upwd_inside, downwd_inside, external
C       jj=9-11 sloped upwd_inside, downwd_inside, external
C       jj=12-14 wall inside, partition, external

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MZHCFV2'  ! set for subroutine

C Initialize data.
      NHCFP(ICOMP)=0
      II=0
      IUNIT=IFIL+1

C Check for format.
      CALL EFOPSEQ(IUNIT,LHCCO(ICOMP),1,IER)
      IF(ier.eq.0)THEN
        write(currentfile,'(a)') LHCCO(ICOMP)(1:lnblnk(LHCCO(ICOMP)))

C Does the 1st and 2nd line have one item and the 3rd line have two items?
C Or does the file start with '*HC_CTL'?
        CALL STRIPC(IUNIT,OUTSTR,99,num1,1,'htc line 1',IER)
        CALL STRIPC(IUNIT,OUTSTR,99,num2,1,'htc line 2',IER)
        CALL STRIPC(IUNIT,OUTSTR,99,num3,1,'htc line 3',IER)
        if(num1.eq.1.and.num2.eq.1.and.num3.eq.2)then
          rewind(iunit,err=392)
          newer=.false.
        else

C Rewind file check first line and if *HC_Ctl read 2nd line as datestamp.
          rewind(iunit,err=392)
          CALL STRIPC(iunit,OUTSTR,99,num1,1,'htc header',IER)
          if(OUTSTR(1:7).eq.'*HC_CTL'.or.
     &       OUTSTR(1:7).eq.'*hc_ctl')then
            newer=.true.
            CALL STRIPC(iunit,OUTSTR,99,num1,1,'htc date',IER)
          else

C Not a recognised file << what error code to return? >>
            CALL ERPFREE(IUNIT,ISTAT)
            stop ' HTC file in old format: file must be updated.'
          endif
        endif
      endif

C Read the number of control intervals.
      CALL STRIPC(iunit,OUTSTR,1,num1,1,'htc periods',IER)
      K=0
      CALL EGETWI(OUTSTR,K,N,1,MBP,'F','htc periods',IER)
      if(ier.ne.0) goto 390
      NHCFP(ICOMP)=N

C Read the number of surfaces
      CALL STRIPC(iunit,OUTSTR,1,num1,1,'htc surfaces',IER)
      K=0
      CALL EGETWI(OUTSTR,K,NS,1,NSUR,'W','htc surfaces',IER)
      IF(NSUR.NE.NS)goto 1007

C Iterate through each time interval.
      DO 10 I=1,N

C Read start and end times of interval.
        CALL STRIPC(iunit,OUTSTR,2,num1,1,'htc start - end',IER)
        K=0
        CALL EGETWR(OUTSTR,K,ST,0.,24.,'F','htc start',IER)
        CALL EGETWR(OUTSTR,K,FT,ST,24.,'F','htc end',IER)
        if(ier.ne.0) goto 390
        IF(FT.LT.ST)goto 1006
        HCFPST(ICOMP,I)=ST
        HCFPFT(ICOMP,I)=FT

C Read control `type' for interval.  This controls how the remaining
C data is read in.
        CALL STRIPC(iunit,OUTSTR,1,num1,1,'htc ctl type',IER)
        K=0
        CALL EGETWI(OUTSTR,K,Itype,1,4,'F','htc ctl type',IER)
        if(ier.ne.0) goto 390
        iCTLTP(ICOMP,I) = Itype

C If newer format then scan for hcfpdescr.
        if(newer)then
          CALL STRIPC(iunit,OUTSTR,99,num1,1,'htc ctl doc',IER)
          K=0
          CALL EGETW(OUTSTR,K,WORD,'W','*doc',IFLAG)
          call egetrm(outstr,K,hcfpdescr(i),'W','hc doc',IER)
        else
          hcfpdescr(i)='Imported legacy file with no regime docs.'
        endif

        if(Itype.eq.1) then

C User has specified surface convection coefficients for inside and/or
C surface outermost zone surfaces. Note that a value of -1 for any
C surface indicates that no user prescription is active and that the
C defualt value should be used (hence -1 is included in the following
C range check).
          CALL EGETWRA(iunit,HCIA,NS,-1.,999.,'W','ins face hc',IER)
          CALL EGETWRA(iunit,HCEA,NS,-1.,999.,'W','other face hc',IER)
          DO 114 KV=1,NS
            IF(ABS(HCEA(KV)).LT.0.00001.AND.EMISE(KV).GT.0.0)goto 1009
            HCFVI(ICOMP,KV,I)=HCIA(KV)
            HCFVE(ICOMP,KV,I)=HCEA(KV)
  114     CONTINUE
        elseif(Itype.eq.2) then

C User specifies the correlation to use for each surface.
C Read in the correlation indices. Note that
C the `use default value` indicator is -1 to allow that in range
C checking.
          CALL EGETWIA(iunit,IHCIA,NS,-1,99,'W',
     &      'inside face hc cor',IER)
          CALL EGETWIA(iunit,IHCEA,NS,-1,99,'W',
     &     'other face hc cor',IER)
          DO 115 KV=1,NS
            IcIuse(ICOMP,KV,I)=IHCIA(KV)
            IcEuse(ICOMP,KV,I)=IHCEA(KV)
  115     CONTINUE
        elseif(Itype.eq.3) then

C User specifies `adaptive' control.
C Read in control laws and associated data. One line of data for each mlc.
          do 30 J=1,NS
            II = II + 1
            icnvKY(ICOMP,J,I) = II
            READ(IUNIT,*,IOSTAT=ISTAT,ERR=1001,END=1003)
     &           (CVdat(II,jj),jj=1,8)
   30     continue

C Read IAORZ,CWIDTH and CHEIGHT of ventilated channels. Close
C the file if this line was not found.
          READ(IUNIT,*,IOSTAT=ISTAT,ERR=1002,END=2003)
     &      IAORZ,CWIDTH,CHEIGHT
        elseif(Itype.eq.4) then

C CEN mode sets values for specific conditions.
          CALL STRIPC(IUNIT,OUTSTR,99,num,1,'htc cen base top',IER)
          if(OUTSTR(1:8).eq.'*basetop')then
            K=8
            CALL EGETWR(OUTSTR,K,V1,1.,100.,'W','htc cen base',IER)
            CALL EGETWR(OUTSTR,K,V2,1.,100.,'W','htc cen top',IER)
            CENLOC(icomp,i,1)=V1
            CENLOC(icomp,i,2)=V2
          else
            goto 1011
          endif
          CALL STRIPC(IUNIT,OUTSTR,99,num,1,'htc cen floor',IER)
          if(OUTSTR(1:6).eq.'*floor')then
            K=6
            CALL EGETWR(OUTSTR,K,V1,0.,20.,'F','htc cen upward',IER)
            CALL EGETWR(OUTSTR,K,V2,0.,20.,'F','htc cen down',IER)
            CALL EGETWR(OUTSTR,K,V3,0.,20.,'F','htc cen ext',IER)
            CENLOC(icomp,i,3)=V1
            CENLOC(icomp,i,4)=V2
            CENLOC(icomp,i,5)=V3
          else
            goto 1011
          endif
          CALL STRIPC(IUNIT,OUTSTR,99,num,1,'htc cen ceiling',IER)
          if(OUTSTR(1:8).eq.'*ceiling')then
            K=8
            CALL EGETWR(OUTSTR,K,V1,0.,20.,'F','htc cen upward',IER)
            CALL EGETWR(OUTSTR,K,V2,0.,20.,'F','htc cen down',IER)
            CALL EGETWR(OUTSTR,K,V3,0.,20.,'F','htc cen ext',IER)
            CENLOC(icomp,i,6)=V1
            CENLOC(icomp,i,7)=V2
            CENLOC(icomp,i,8)=V3
          else
            goto 1012
          endif
          CALL STRIPC(IUNIT,OUTSTR,99,num,1,'htc cen slope',IER)
          if(OUTSTR(1:6).eq.'*slope')then
            K=6
            CALL EGETWR(OUTSTR,K,V1,0.,20.,'F','htc cen upward',IER)
            CALL EGETWR(OUTSTR,K,V2,0.,20.,'F','htc cen down',IER)
            CALL EGETWR(OUTSTR,K,V3,0.,20.,'F','htc cen ext',IER)
            CENLOC(icomp,i,9)=V1
            CENLOC(icomp,i,10)=V2
            CENLOC(icomp,i,11)=V3
          else
            goto 1013
          endif
          CALL STRIPC(IUNIT,OUTSTR,99,num,1,'htc cen wall',IER)
          if(OUTSTR(1:5).eq.'*wall')then
            K=5
            CALL EGETWR(OUTSTR,K,V1,0.,20.,'F','htc cen inside',IER)
            CALL EGETWR(OUTSTR,K,V2,0.,20.,'F','htc cen ptn',IER)
            CALL EGETWR(OUTSTR,K,V3,0.,20.,'F','htc cen ext',IER)
            CENLOC(icomp,i,12)=V1
            CENLOC(icomp,i,13)=V2
            CENLOC(icomp,i,14)=V3
          else
            goto 1014
          endif
        else

C Invalid control type specified.
          goto 1010
        endif
   10 CONTINUE

C Free file.
 2003 CALL ERPFREE(IUNIT,ISTAT)

C Trace output ?
    8 IF(ITC.EQ.1)goto 5
      IF(ITRACE(6).EQ.1.AND.IZNTRC(ICOMP).EQ.1)goto  5
      goto 6
 1000 IER=1
      write(outs,7)ICOMP
    7 format(' Zone',I3,' convective regime file error.')
      call edisp(iuout,outs)
      CALL edisp(iuout,' run being terminated.')
      goto 8
 1001 CALL edisp(iuout,' Convective regime file read error.')
      goto 1000
 1002 CALL edisp(iuout,' End of convective regime file sensed.')
      goto 1000
 1003 CALL edisp(iuout,' Number of conv coef periods out of range.')
      goto 1000
 1006 CALL edisp(iuout,' Conv coef cntrl cannot extend over midnight.')
      goto 1000
 1007 call edisp(iuout,' Number of surfaces do not match.')
      goto 1000
 1009 call edisp(iuout,' A zero surface convection coefficient')
      call edisp(iuout,' corresponding to a finite surface emissivity')
      call edisp(iuout,' defines a physically unrealistic model')
      call edisp(iuout,' which might lead to numerical instability.')
      call epwait
      call epagend
      STOP
 1010 call edisp(iuout,' Invalid HTC control type specified')
      goto 1000
 1011 call edisp(iuout,' Expecting *floor in htc file.')
      goto 1000
 1012 call edisp(iuout,' Expecting *ceil in htc file.')
      goto 1000
 1013 call edisp(iuout,' Expecting *slope in htc file.')
      goto 1000
 1014 call edisp(iuout,' Expecting *wall in htc file.')
      goto 1000

C Error messages.
  390 write(outs,77)OUTSTR
   77 format(' Outstr: ',A,' HC File read error. ')
      call edisp(iuout,outs)
      CALL ERPFREE(IUF,ISTAT)
      call epwait
      call epagend
      STOP
  392 call edisp(iuout,' Unable to rewind HC File.')
      CALL ERPFREE(IUF,ISTAT)
      call epwait
      call epagend
      STOP

C Trace output follows.
    5 CONTINUE
      dok=.true.
      helptopic='hc_coef_trace_dialog'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL ASKOK(' ',' Convective regime trace output?',
     &  OK,dok,nbhelp)
      IF(.NOT.OK)goto 6
      write(outs,'(A,I4,a,i2)')' Subroutine MZHCFV2  Trace output',
     &  ICNT,' for zone ',ICOMP
      call edisp(itu,outs)
      call edisp(itu,' ')
      ICNT=ICNT+1
      call edisp(itu,' Control Start Finish')
      N=NHCFP(ICOMP)
      DO 9994 I=1,N
        write(outs,'(1x,a)') hcfpdescr(i)
        call edisp(itu,outs)
        if(Itype.eq.1) then
          write(outs,'(I6,2I7)')I,HCFPST(ICOMP,I),HCFPFT(ICOMP,I)
          call edisp(itu,outs)
          DO 9992 J=1,NS
            write(outs,'(2F15.3)')HCFVI(ICOMP,J,I),HCFVE(ICOMP,J,I)
            call edisp(itu,outs)
 9992     CONTINUE
        elseif(Itype.eq.2) then
          write(outs,'(I6,2I7)')I,HCFPST(ICOMP,I),HCFPFT(ICOMP,I)
          call edisp(itu,outs)
          DO 9982 J=1,NS
            write(outs,'(2I10)')IcIuse(ICOMP,J,I),IcEuse(ICOMP,J,I)
            call edisp(itu,outs)
 9982     CONTINUE
        elseif(Itype.eq.3) then
          write(outs,'(I6,2I7)')I,HCFPST(ICOMP,I),HCFPFT(ICOMP,I)
          call edisp(itu,outs)
          DO 9972 J=1,NS
            write(outs,'(i5)') icnvKY(ICOMP,J,I)
            call edisp(itu,outs)
 9972     CONTINUE
          call edisp(itu,'Per surface data for type 3')
          DO 9962 J=1,II
            write(outs,'(I3,8F8.3)') II,(CVdat(II,jj),jj=1,8)
            call edisp(itu,outs)
 9962     CONTINUE
        endif
 9994 CONTINUE
    6 IF(IER.EQ.5)goto 9
      RETURN

C Fatal error trap.
    9 call epwait
      call epagend
      STOP

      END

c ******************** MZSHIN ********************

C MZSHIN assigns the zone associated shading file for use
C later within the simulation time-stepping.

C Format of a zone shading & insolation db:
C Minimum width is 24 words, normal width is nzsur(icomp) words.
C Record 1: first 12 words holds ishd(), the analysis type for
C           each day-month (0 no data, 1 shading only, 2 insolation
C           only, 3 shading and insolation), followed by isadd(),
C           the record where the shading and/or insolation data for
C           each day-month commences.
C Record 2: first word is irecx (the last record written to + 1, i.e.
C           the next record available for new data), second is nsurs
C           (the number of surfaces in the zone at the time the db
C           was created), third is msurs (the maximum number of
C           insolated surfaces defined when the db was created). The
C           second and third items are used as consistency checks.
C Record 3: reserved for future use.
C Record 4: itmc(), if 1, indicating that a surface is a TMC, i.e.
C           and insolation source.
C Record 5: beginning of shading and/or insolation data for first
C           month considered.
C Shading records:
C   24 hour values of direct, pso(), and diffuse, psof(), shading
C   factors are written in record pairs occupying 2*nsur records.
C Insolation records:
C   These start at record isadd(mon)+2*nsur. The first record of
C   the block holds the ioffs array, which gives the record offset
C   to the start of the data for each considered insolation source.
C   These data correspond to each of the possible misur insolation
C   sources and comprise 24 hour values of the insolated surface
C   index, insst(), and the insolation factor, pinwst(), written
C   in record pairs. Thus, igc() for a 6 surface zone might, for
C   example, be 0,1,x,0,y,0 (where x and y have specific integer
C   values) indicating that surfaces 2, 3 and 5 are insolation
C   sources, that the data for surface 3 commences at the next
C   record (i.e. with an offset of 1), the data for surface 3
C   commences with an offset of x=2*misur and the data for surface 5
C   commences with an offset of y=2*(2*misur).

      SUBROUTINE MZSHIN(ICOMP)
#include "building.h"
#include "model.h"
#include "geometry.h"

      COMMON/FILEP/IFIL
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/OUTIN/IUOUT,IUIN

      common/rsolmax/maxrec(mcom)

      CHARACTER LS*72
      character outs*124

      DIMENSION ISUM1(12),ISUM2(12),PS(24),
     &          IGC(MS),ISADD(12),ISHD(12)
      logical OK,dok
      integer ier

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MZSHIN'  ! set for subroutine

c Initialise shading file (unit number = IFIL+1).
      JOUT=ITC
      IUNIT=IFIL+1
      LS=LSHAD(ICOMP)
      maxrec(icomp)=24
      if(NZSUR(icomp).gt.24)maxrec(icomp)=NZSUR(icomp)
      ier=0
      call EFOPRAN(IUNIT,LS,maxrec(icomp),1,IER)
      IF(ier.ne.0)goto 1000

C Check file has been written to. If a problem the try again
C at maxrec=24 for older files. Remember maxrec for use in solar.F.
      IREC=2
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1001)INEXT,NS
      if(INEXT.LE.3.OR.NS.NE.NSUR)then
        CALL ERPFREE(IUNIT,ISTAT)
        maxrec(icomp)=24
        ier=0
        call EFOPRAN(IUNIT,LS,maxrec(icomp),1,IER)
        IF(ier.ne.0)goto 1000
        IREC=2
        READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1001)INEXT,NS
        if(INEXT.LE.3.OR.NS.NE.NSUR)then
          CALL ERPFREE(IUNIT,ISTAT)
          maxrec(icomp)=MS
          ier=0
          call EFOPRAN(IUNIT,LS,maxrec(icomp),1,IER)
          IF(ier.ne.0)goto 1000
          IREC=2
          READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1001)INEXT,NS
          if(INEXT.LE.3.OR.NS.NE.NSUR)goto 1003
        endif
      endif
          
C Trace output. MZSHIN is only called during input to no need
C to put a simulation time limit on this trace option.
    3 IF(ITRACE(5).EQ.1.AND.IZNTRC(ICOMP).EQ.1) goto 1

C If this point reached there was no request for trace so
C close the file.
      CALL ERPFREE(IUNIT,ISTAT)
      IF(IER.EQ.1)then
        call epwait
        call epagend
        STOP
      endif
      RETURN
 

C Errors that also attempt to write trace output prior to
C quitting.
 1000 IER=1
      write(outs,'(2A)') zname(ICOMP),' shading file error in '
      call edisp(iuout,outs)
      call edisp(iuout,LS)
      CALL edisp(iuout,' Run being terminated.')
      goto 3  ! jump back to trace

 1001 CALL edisp(iuout,' Read error in shading file.')
      goto 1000  ! write further error information

 1003 write(outs,'(a,i2,a,i2,a,i2)') 'Model nsur =',NSUR,' db ns =',
     &  ns,' inext=',INEXT
      call edisp(iuout,' Mismatch between model and shading file.')
      call edisp(iuout,outs)
      goto 1000  ! write further error information

C Trace output follows. The binary file is scanned and the whole
C years data for each of the selected zones is printed out. This
C only works if the user first selects the shading & insolation
C trace and then re-scanns in the model cfg file.

C Code to read the file is out of date and differes from the
C logic within solar.F so diffuse shading and insolation data
C is not presented. Direct shading is presented only.
    1 CONTINUE 
      dok=.true.
      write(outs,'(A,I4)')' Subroutine MZSHIN     Trace output',ICNT
      call edisp(itu,outs)
      write(outs,'(23X,A,I7)')'Zone',ICOMP
      call edisp(itu,outs)
      ICNT=ICNT+1
      IREC=1
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1005)(ISHD(I),I=1,12),
     & (ISADD(I),I=1,12)

C For each month generate a report.
      DO 99961 I=1,12
      IF(ISHD(I).LT.0.OR.ISHD(I).GT.3)goto 1006
      IF(ISHD(I).EQ.0.OR.ISADD(I).EQ.0)goto 99961
      ISS=ISHD(I)
      goto (9994,9995,9994),ISS

c Insolation case only.
 9995 call edisp(itu,' Insolation only case.')
      goto 99931

c Shading only or shading/insolation case.
 9994 IF(ISS.EQ.3)goto 9992
      call edisp(itu,' Shading case only.')
      goto 9990
 9992 call edisp(itu,' Shading/insolation case.')
 9990 call edisp(itu,' ')
      call edisp(itu,' Month Surface 24-hour shading time-series')
      call edisp(itu,' ')
      IREC=ISADD(I)
      DO 9986 J=1,NSUR
        READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1008)(PS(K),K=1,24)
        IREC=IREC+2   ! there is a 2nd record in solar.F
        IF(J.GT.1)goto 9985
        write(outs,9984)I,J,(PS(K),K=1,24)
 9984   format(I4,I7,F7.1,',',22(F3.1,','),F3.1)
        call edisp(itu,outs)
        goto 9986
 9985   write(outs,9983)J,(PS(K),K=1,24)
 9983   format(I11,F7.1,',',22(F3.1,','),F3.1)
        call edisp(itu,outs)
 9986 CONTINUE
      J=ICNT-1
      write(outs,9982)J,ICOMP
 9982 FORMAT(' Trace output',I4,' Zone',I4,' Continued')
      call edisp(itu,outs)
      call edisp(itu,' ')
      call edisp(itu,' Month Window 24-hour shading time-series')
      IREC=ISADD(I)
      IREC=IREC+NSUR
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1008)(IGC(J),J=1,NSUR)
      IRM=IREC

c IRSM summates the number of windows handled in total.
      IRSM=0
      ISUM1(I)=IRSM
      IF(ISS.EQ.1)goto 2

c Insolation case.
99931 write(outs,9975)J,ICOMP
 9975 format(' Trace output',I4,' Zone',I4,' Continued')
      call edisp(itu,outs)
      call edisp(itu,' Month Window Hour Receiving Receiving')
      call edisp(itu,'                   Surface 1 Surface 2')
      call edisp(itu,'                     Number    Number')

c Output IRS.
      IRM=ISADD(I)+NSUR+ISUM1(I)+1
      IF(ISHD(I).EQ.2)IRM=ISADD(I)
      IRSM=0
      IREC=IRM
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1008)(IGC(J),J=1,NSUR)
      DO 9972 J=1,NSUR
        IF(IGC(J).EQ.0)goto 9972
        ING=0
        IS=IGC(J)
        IRSM=IRSM+ING*3
        IREC=IRM+IS
 9972 CONTINUE
      ISUM2(I)=IRSM

c Output PI.
      call edisp(itu,'                   Pro.Split Pro.Split')
      IRM=ISADD(I)+NSUR+ISUM1(I)+ISUM2(I)+1
      IF(ISHD(I).EQ.2)IRM=ISADD(I)+ISUM2(I)
      DO 9963 J=1,NSUR
        IF(IGC(J).EQ.0)goto 9963
        ING=0
        IS=IGC(J)
        IREC=IRM+IS
 9963 CONTINUE
99961 CONTINUE
      goto 2

 1005 CALL edisp(iuout,' Shading db read during trace.')
      goto 1008
 1006 CALL edisp(iuout,' Shading index for month not correct type.')
      goto 1008
 1008 JOUT=0
      goto 1000

    2 CALL ERPFREE(IUNIT,ISTAT)
      IF(IER.EQ.1)then
        call epwait
        call epagend
        STOP
      endif
      RETURN

      END


c ******************** MZVFIN ********************

c MZVFIN reads the black body geometric view factor between
c each internal surface pair from a file prepared via
c program ESRU: mrt.

c Common block variables are:

c CFB  - the black body view factor (sometimes known as the
c        configuration or angle factor) between each internal
c        surface polygon (defining opaque and transparent surfaces).

c IVF - determines whether this information is available
c        or not.

      SUBROUTINE MZVFIN(ICOMP)
#include "building.h"
#include "geometry.h"
#include "model.h"

      COMMON/ER/IER
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/FILEP/IFIL

      COMMON/V2/CFB(MCOM,MST,MST)
      COMMON/MRTF/FFL(MCUB,6,MSM),VEWF(MS,MS)

      character outs*124

C Read viewfactor & MRT sensor file.
      IUNIT=IFIL+1
      itrc=0
      CALL ERMRT(ITRC,iuout,IUNIT,LVIEW(ICOMP),ICOMP,IER)

c Transfer VEWF array to CFB.
      DO 30 I=1,NSUR
        do 35 J=1,NSUR
          CFB(ICOMP,I,J)=VEWF(I,J)
          if(CFB(ICOMP,I,J).LT.0..OR.CFB(ICOMP,I,J).GT.1.)then
            write(outs,'(A,2I3,A)') ' View between surfs ',I,J,
     &                              ' out of range.'
            call edisp(iuout,outs)
            goto  1000
          endif
   35   continue
   30 CONTINUE

c Free file.
    8 CALL ERPFREE(IUNIT,ISTAT)
      IF(IER.EQ.1)then
        call epwait
        call epagend
        STOP
      endif
      RETURN

 1000 IER=1
      write(outs,'(A,I3,A)')' zone',ICOMP,' view factor file error.'
      call edisp(iuout,outs)
      goto 8

      END

