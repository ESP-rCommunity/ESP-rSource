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
C ercgcf  - read casual gain control file.
C CASCTMK - write casual gain control file.

C ******************** ercgcf ********************
C ercgcf  - read casual gain control file.

C Common block variables are:
C NLITZ    - Number of lighting zones per thermal zone

C NCGTC    - Type number of casual gains to be controlled during
C            different day types

C IDFST    - illuminance calculation type (1: ESP-r calculated daylight
C            factors, 2: User specified daylight factors, 3: external
C            sensor, 4: Coupling with 3rd party lighting simulation)

C NCGCP    - number of distinct casual gain control periods during a
C            typical day (maximum=MBP)
C ICGCS    - start hour of each period (0-24)
C ICGCF    - finish hour of each period
C ICGCFL   - control type (1 on/off; 2 step down; 3 proportional; 4 Hunts)
C            5 topup)
c CGX, CGY
c  & CGH   - x and y position of sensed point relative to zone origin 
C            and working plane height, in some cases they are
C            overloaded to hold other applicable information (IDFST=3).
C NUDWIN   - Number of user defined glazing for use with user defined
C            daylight factors.
C SETPT    - Reference light level (set point) in Lux
C SYSPER   - % of controled casual gain associated with light zone
C SOFFLL   - Switch-off light level, portion of SETPT (-)
C IOFFDT   - Switch-off delay time, number of time steps (-)
C SMLOUT   - Minimum dimming light output, 0. - 1. (-)
C SMEOUT   - Minimum dimming electric wattage, portion of total wattage (-)
C SPELEC   - Photo-cell sensor signal (lux) under artificial lighting,
C            determoned by night-time calibration
C SLOPEM   - Linear control slope (-), determined by day-time calibration

C Data for use with Daylight coefficient method:
C NDCFP    - Number of Daylight coefficient points i.e. sensors (max 5)
C NDCFS    - Number of Daylight coefficient stages (max 2)
C DCOEF    - Value of given Daylight coefficient
C IDCFID   - Daylight coefficient sensor ID

      SUBROUTINE ercgcf(itrc,itru,LCGCIN,ICOMP,ier)
#include "building.h"
#include "geometry.h"
#include "espriou.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,IDTY
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32

C Zone casual gain control data.
      common/CGCIND/cgcdesc(MCOM)
      COMMON/CGCIN2N/NCGTC(MCOM,MDTY),NLITZ(MCOM),IDFST(MCOM,MLCOM),
     & CGX(MCOM,MLCOM,MDF),CGY(MCOM,MLCOM,MDF),CGH(MCOM,MLCOM,MDF),
     & UX(MCOM,MLCOM,MDF),UY(MCOM,MLCOM,MDF),UH(MCOM,MLCOM,MDF),
     & SETPT(MCOM,MLCOM),SYSPER(MCOM,MLCOM),SOFFLL(MCOM,MLCOM),
     & IOFFDT(MCOM,MLCOM),SMLOUT(MCOM,MLCOM),SMEOUT(MCOM,MLCOM)
      COMMON/CGCIN3/ICGCS(MCOM),ICGCF(MCOM),
     & ICGCFL(MCOM,MLCOM),SPELEC(MCOM,MLCOM),SLOPEM(MCOM,MLCOM)

      COMMON/DAYF/NDF(MCOM,MLCOM),DFDAT(MCOM,MLCOM,MGT,MDF),
     & AZIJL(MCOM,MGT),SUMDF(MCOM,MLCOM,MDF),NUDWIN(MCOM)

C Daylight coefficient common block data definition:
      COMMON/DCOEF/NDCFP,NDCFS(MDCFP),DCOEF(MDCFP,MDCFS,MDCFV),
     & IDCFID(MCOM,MLCOM,MDF)

      CHARACTER LCGCIN*72
      character outs*124,OUTSTR*124,cgcdesc*64,info*18

      dimension DV(MDCFV)

      ier=0

      IUNIT=IFIL+1
      CALL EFOPSEQ(IUNIT,LCGCIN,1,IER)
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF
      write(currentfile,'(a)') LCGCIN(1:lnblnk(LCGCIN))

      cgcdesc(icomp)='no description'

C Clear daylight factor array.
      do 2 ii=1,MLCOM
        do 3 jj=1,MGT
          do 4 kk=1,MDF
            DFDAT(icomp,ii,jj,kk)=0.0
    4     continue
    3   continue
    2 continue

c Read type of casual gain to be controlled during each day type.
      CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'cgc line 1',IER)
      if(IER.NE.0)return
      if(OUTSTR(1:16).eq.'*Casual_gain_ctl')then
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'cgc line 2',IER)
        if(IER.NE.0)return
        if(OUTSTR(1:5).eq.'*desc')then
          k=6
          CALL EGETRM(OUTSTR,K,cgcdesc(icomp),'W','descr',IER)
          CALL STRIPC(IUNIT,OUTSTR,3,ND,1,'cgc line 3',IER)
        endif
      endif
      K=0
      DO 400 IDTY=1,NBDAYTYPE
        CALL EGETWI(OUTSTR,K,N1,0,MCT,'F',' gain type',IER)
        NCGTC(ICOMP,IDTY)=N1
 400  CONTINUE

C Read control period start and finish hours
      CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'cgc file line 2',IER)
      K=0
      CALL EGETWI(OUTSTR,K,IS,0,24,'F','cgc contr start',IER)
      CALL EGETWI(OUTSTR,K,IF,IS,24,'F','cgc contr finish',IER)
      ICGCS(ICOMP)=IS
      ICGCF(ICOMP)=IF

C Read number of lighting zones in this thermal zone NLITZ(MLCOM).
C Maximum 4 lighting zones allowed (MLCOM=4):
      CALL STRIPC(IUNIT,OUTSTR,1,ND,1,'cgc line 2',IER)
      K=0
      CALL EGETWI(OUTSTR,K,NL,1,MLCOM,'F','No. light zones',IER)
      NLITZ(ICOMP)=NL

C Loop through all lighting zones and read info.
      SUMSYS=0.
      DO 111 N=1,NLITZ(ICOMP)

C Read lux setpoint, switch-off light level, minimum dimming light output,
C switch-off delay time:
      CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'setpt, %, ndf, idfst',IER)
      K=0
      CALL EGETWR(OUTSTR,K,sp,0.,0.,'-','zone sen setpt',IER)
      CALL EGETWR(OUTSTR,K,soff,1.,3.,'F',
     &              'switch-off light level',IER)
      CALL EGETWR(OUTSTR,K,sml,0.,1.,'F',
     &              'min. dim. light output',IER)
      if(ND.eq.5)then
        CALL EGETWR(OUTSTR,K,sme,0.,1.,'F',
     &              'minimum wattage',IER)   
      else
        sme=sml
      endif   
      CALL EGETWI(OUTSTR,K,ndt,0,6,'F',
     &              'switch-off delay time',IER)

      SETPT(ICOMP,N)=sp
      SOFFLL(ICOMP,N)=soff
      SMLOUT(ICOMP,N)=sml
      SMEOUT(ICOMP,N)=sme
      IOFFDT(ICOMP,N)=ndt

C Read per centage of casual gain, number of sensors (MAX 9),
C and calculation type:
      CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'setpt, %, ndf, idfst',IER)
      K=0
      CALL EGETWR(OUTSTR,K,sy,0.,1.,'F','zone cg %',IER)
      CALL EGETWI(OUTSTR,K,nls,1,MDF,'F',
     &              'no. of light sensors',IER)
      CALL EGETWI(OUTSTR,K,idc,1,5,'F','sensor type',IER)
      SYSPER(ICOMP,N)=sy
      NDF(ICOMP,N)=nls
      IDFST(ICOMP,N)=idc
      SUMSYS=SUMSYS+SYSPER(ICOMP,N)
      IF(SUMSYS.GT.1.0)then
        call usrmsg('Individual lighting systems % are > 1.',' ','W')
        goto 1000
      endif

C Read sensor location data x, y, z coordinates,
C Read Daylight coefficient sensor identification data:
      DO 200 J=1,NDF(ICOMP,N)
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'composite sensor',IER)
        K=0
        CALL EGETWR(OUTSTR,K,cx,0.,0.,'-','comp sens x',IER)
        CALL EGETWR(OUTSTR,K,cy,0.,0.,'-','comp sens y',IER)
        CALL EGETWR(OUTSTR,K,cz,0.,0.,'-','comp sens ht',IER)
        CGX(ICOMP,N,J)=cx
        CGY(ICOMP,N,J)=cy
        CGH(ICOMP,N,J)=cz
        if(IDFST(ICOMP,N).ne.3)then
          CALL EGETWR(OUTSTR,K,cux,0.,0.,'-','comp sens ux',IER)
          CALL EGETWR(OUTSTR,K,cuy,0.,0.,'-','comp sens uy',IER)
          CALL EGETWR(OUTSTR,K,cuz,0.,0.,'-','comp sens uh',IER)
          UX(ICOMP,N,J)=cux
          UY(ICOMP,N,J)=cuy
          UH(ICOMP,N,J)=cuz
          if(IDFST(ICOMP,N).EQ.5)then
            CALL EGETWI(OUTSTR,K,ID,1,MDCFP,'F','Daylit cf. ID',
     &        IER)
            IDCFID(ICOMP,N,J)=ID
          endif
        endif
200   CONTINUE

C If user specified df data, then read individual window related df:
      IF(IDFST(ICOMP,N).EQ.2)THEN
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'User windows def',IER)
        K=0
        CALL EGETWI(OUTSTR,K,NUDWIN(ICOMP),1,MS,'W','user df',IER)
        if(NUDWIN(ICOMP).eq.0)then
          call usrmsg('Must have at least 1 surface to associate',
     &      'with daylightfactor.','W')
          goto 1000
        endif
        DO 112 IL=1,NUDWIN(ICOMP)
          DO 114 L=1,NDF(ICOMP,N)
            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'user df',IER)
            K=0
            CALL EGETWI(OUTSTR,K,IW,1,MS,'w','surf indx',IER)
            CALL EGETWR(OUTSTR,K,df,0.,0.,'-','daylight fac',IER)
            DFDAT(ICOMP,N,IW,L)=df
  114     CONTINUE
  112   CONTINUE
      ENDIF

C Read control details. Control flag is:
C -1 for ON,  0 for OFF,  1 for ON/OFF
C  2 for step down, 3 for linear proportional
C 31 for integral reset dimming control
C 32 for closed loop proportional dimming control
C  4 Hunt probability function

      CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'cgc contr law line ',IER)
      K=0
      CALL EGETWI(OUTSTR,K,IFL,-1,32,'F','cgc contr law',IER)
      ICGCFL(ICOMP,N)=IFL

C Read additional input data for control laws 31 and 32:
C SPELEC - Photo-cell lux signal under artificial lighting
C          determined by night-time calibration
C SLOPEM - linear control slope used by control 32

      IF(ICGCFL(ICOMP,N).EQ.31.OR.ICGCFL(ICOMP,N).EQ.32)THEN
        CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'supl ctl line',IER)
        K=0
        CALL EGETWR(OUTSTR,K,SIG,0.,0.,'-','night calib',IER)
        CALL EGETWR(OUTSTR,K,SLP,0.,0.,'-','control slope',IER)
        SPELEC(ICOMP,N)=SIG
        SLOPEM(ICOMP,N)=SLP
      ENDIF
       
  111 CONTINUE

C Check that all of the gain has been controlled.
      IF(SUMSYS.LT.1.0)then
        call usrmsg(' Sum of controled casual gains fraction  < 1.',
     &             ' ','W')
        goto 1000
      endif
      
C Loop through all lighting zones and read daylight
C coefficient values from casual gain file:
      DO 206 N=1,NLITZ(ICOMP)
        IF(IDFST(ICOMP,N).EQ.5)THEN
          ierr=0
          do 207 ISE=1,NDF(ICOMP,N)
            call STRIPC(IUNIT,OUTSTR,0,ND,1,'DCF set',IER)
            K=0
            CALL EGETWI(OUTSTR,K,ISN,1,5,'F','DCF set No.',IER)
            CALL EGETWI(OUTSTR,K,INS,1,2,'F','No. of stages',IER)
            IDCFID(ICOMP,N,ISE) = ISN
            NDCFS(ISN) = INS
            do 219 i = 1,INS
              call STRIPC(IUNIT,OUTSTR,0,ND,1,'DCF stage no.',IER)
              K=0
              CALL EGETWI(OUTSTR,K,IST,1,2,'F','DCF stage no',IER)
              call EGETWRA(IUNIT,DV,MDCFV,-1.0,1.0,'F','DCF vals',IER)
              
C Should may be write the number of coefficients into the file.
              do 208 JX=1,MDCFV
                DCOEF(ISN,IST,JX)=DV(JX)
                if (DV(JX).lt.-0.99) then
                  ierr=-101
                elseif (DV(JX).lt.0.) then
                  WRITE(outs,'(A,I3,A,f9.6)')
     &               'Problem detected with daylight coefficient ',
     &                JX,'. Value=',DV(JX)
                  CALL USRMSG(outs,'  ','W')
                endif
208           continue
219         continue
207       continue
          if (ierr.eq.-101) then
            CALL USRMSG('Daylight coeffs have not been calculated yet',
     &                   '  ','W')
          endif
        ENDIF
206   CONTINUE
      
C Free file.
      CALL ERPFREE(IUNIT,ISTAT)

c Trace output ?
    8 IF(itrc.gt.0)goto 5
      goto 6

 1000 IER=1
      write(outs,'(A,I3,A)')' Zone',ICOMP,' casual gain control error'
      call edisp(iuout,outs)
      goto 8
      
c Trace output follows.
    5 call edisp(itru,' ')
      write(outs,'(A,I2)') ' Zone ',ICOMP
      call edisp(itru,outs)

      DO 401 IDTY=1,NBDAYTYPE
        write(outs,9989)CALENTAG(IDTY),NCGTC(ICOMP,IDTY)
 9989     format(' Casual gain type for control during ',1A,' = ',1I3)
        call edisp(itru,outs)
 401  CONTINUE
        DO 117 N=1,NLITZ(ICOMP)
          write(outs,'(A,I1)') ' Light zone ',N
          call edisp(itru,outs)
          if(IDFST(ICOMP,N).eq.1)then
            write(outs,'(A,I2,A)') ' Average of ',NDF(ICOMP,N),
     &        ' analytical daylight factor photocell types. '
            call edisp(itru,outs)
          elseif(IDFST(ICOMP,N).eq.2)then
            write(outs,'(A,I2,A)') ' Average of ',NDF(ICOMP,N),
     &        ' user spec. daylight factor photocell types. '
            call edisp(itru,outs)
            write(outs,'(A,I2,A)') ' There are ',NUDWIN(ICOMP),
     &        ' TMC (transparent) polygons defined. '
            call edisp(itru,outs)
          elseif(IDFST(ICOMP,N).eq.3)then
            call edisp(itru,' External lighting sensor.')
          elseif(IDFST(ICOMP,N).eq.4)then
            write(outs,'(A,I2,A)') ' Average of ',NDF(ICOMP,N),
     &        ' direct coupling photocell types. '
            call edisp(itru,outs)
          elseif(IDFST(ICOMP,N).eq.5)then
            write(outs,'(A,I2,A)') ' Average of ',NDF(ICOMP,N),
     &        ' daylight coefficient photocell types. '
            call edisp(itru,outs)
          endif

          call edisp(itru,'  ')
          call edisp(itru,
     &    ' Set   |Switch-off |Min. dim |Min. ele |Switch-off ')
          call edisp(itru,
     &    ' point |light level|light out|light out|delay time ')
          write(outs,9987)SETPT(ICOMP,N),SOFFLL(ICOMP,N),
     &      SMLOUT(ICOMP,N), SMEOUT(ICOMP,N),IOFFDT(ICOMP,N)
 9987     format(F7.1,F12.1,F10.2,F10.2,I10)
          call edisp(itru,outs)

          call edisp(itru,'  ')
          call edisp(itru,' % of |No. of|Calcul| ')
          call edisp(itru,' whole|sensor|type  | ')
          write(outs,9988)SYSPER(ICOMP,N),NDF(ICOMP,N),IDFST(ICOMP,N)
 9988     format(F6.2,I7,I7)
          call edisp(itru,outs)

          call edisp(itru,'  ')
          if(IDFST(ICOMP,N).eq.1.or.IDFST(ICOMP,N).eq.2.or.
     &      IDFST(ICOMP,N).eq.4)then
           call edisp(itru,
     &       ' Sensor|   X  |   Y  |   Z  |  DX |  DY |  DZ |')
           call edisp(itru,
     &       ' no.   |      |      |      |     |     |     |')
          elseif(IDFST(ICOMP,N).eq.5)then
           call edisp(itru,
     &       ' Sensor|   X  |   Y  |   Z  |  DX |  DY |  DZ |  DC| ')
           call edisp(itru,
     &       ' no.   |      |      |      |     |     |     |    | ')
          elseif(IDFST(ICOMP,N).eq.3)then
            call edisp(itru,' Sensor | Sensor |')
            call edisp(itru,' posit. | type   |')
          endif

          DO 9986 J=1,NDF(ICOMP,N)
            if(IDFST(ICOMP,N).eq.1.or.IDFST(ICOMP,N).eq.2.or.
     &        IDFST(ICOMP,N).eq.4)then
              write(outs,9914)J,CGX(ICOMP,N,J),CGY(ICOMP,N,J),
     &        CGH(ICOMP,N,J),UX(ICOMP,N,J),UY(ICOMP,N,J),UH(ICOMP,N,J)
 9914         format(i7,3F7.2,3F6.2)
              call edisp(itru,outs)
            elseif(IDFST(ICOMP,N).eq.5)then
              write(outs,9915)J,CGX(ICOMP,N,J),CGY(ICOMP,N,J),
     &        CGH(ICOMP,N,J),UX(ICOMP,N,J),UY(ICOMP,N,J),UH(ICOMP,N,J),
     &        IDCFID(ICOMP,N,J)
 9915         format(i7,3F7.2,3F6.2,I4)
              call edisp(itru,outs)
            elseif(IDFST(ICOMP,N).eq.3)then
              write(outs,9916)CGX(ICOMP,N,1),CGY(ICOMP,N,1)
 9916         format(F8.1,F9.1)
              call edisp(itru,outs)
            endif
 9986     CONTINUE

          call edisp(itru,'  ')
          DO 9985 J=1,NDF(ICOMP,N)
            if(IDFST(ICOMP,N).EQ.2)then
              call edisp(itru,' ')
              call edisp(itru,' TMC | Daylight | ')
              call edisp(itru,' no. | factor(%)| ')
              idd=0
              DO 814 IL=1,NZSUR(icomp)
                if(idd.lt.NUDWIN(ICOMP))then
                  if(DFDAT(ICOMP,N,IL,J).gt.0.0)then
                    idd=idd+1
                    write(outs,9917)IL,DFDAT(ICOMP,N,IL,J)*100.
 9917               format(I5,F10.2)
                    call edisp(itru,outs)
                  endif
                endif
  814         CONTINUE
            endif
 9985     CONTINUE

          call edisp(itru,' ')
          call edisp(itru,' Start| Finish| Control    | ')
          call edisp(itru,'  Hour|   Hour|    Type    | ')
          info='unknown control'
          if(ICGCFL(ICOMP,N).eq.-1) info='Always On'
          if(ICGCFL(ICOMP,N).eq.0)  info='Always Off'
          if(ICGCFL(ICOMP,N).eq.1)  info='On-Off'
          if(ICGCFL(ICOMP,N).eq.2)  info='Step 100:50:0'
          if(ICGCFL(ICOMP,N).eq.3)  info='Dimming - ideal'
          if(ICGCFL(ICOMP,N).eq.4)  info='On-Off probability'
          if(ICGCFL(ICOMP,N).eq.31) info='Dimming-int. reset'
          if(ICGCFL(ICOMP,N).eq.32) info='Dimming-closd loop'
          write(outs,'(2I7,1x,A)')ICGCS(ICOMP),ICGCF(ICOMP),info
          call edisp(itru,outs)

          IF(ICGCFL(ICOMP,N).EQ.31.OR.ICGCFL(ICOMP,N).EQ.32)THEN
            call edisp(itru,' ')
            call edisp(itru,' Night | Control| ')
            call edisp(itru,' Calib.|   Slope| ')
            write(outs,'(f7.1,f8.5)')SPELEC(ICOMP,N),SLOPEM(ICOMP,N)
            call edisp(itru,outs)
          ENDIF
  117   CONTINUE
      
    6 RETURN

      END

C ******************** CASCTMK ********************
C CASCTMK - write casual gain control file.
C ACT - 's' do not ask the user for confirmation of file write.

      SUBROUTINE CASCTMK(LCGCIN,ICOMP,ACT,IER)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/FILEP/IFIL
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER,IDTY

C Zone casual gain control data.
      common/CGCIND/cgcdesc(MCOM)
      COMMON/CGCIN2N/NCGTC(MCOM,MDTY),NLITZ(MCOM),IDFST(MCOM,MLCOM),
     & CGX(MCOM,MLCOM,MDF),CGY(MCOM,MLCOM,MDF),CGH(MCOM,MLCOM,MDF),
     & UX(MCOM,MLCOM,MDF),UY(MCOM,MLCOM,MDF),UH(MCOM,MLCOM,MDF),
     & SETPT(MCOM,MLCOM),SYSPER(MCOM,MLCOM),SOFFLL(MCOM,MLCOM),
     & IOFFDT(MCOM,MLCOM),SMLOUT(MCOM,MLCOM),SMEOUT(MCOM,MLCOM)
      COMMON/CGCIN3/ICGCS(MCOM),ICGCF(MCOM),
     & ICGCFL(MCOM,MLCOM),SPELEC(MCOM,MLCOM),SLOPEM(MCOM,MLCOM)

      COMMON/DAYF/NDF(MCOM,MLCOM),DFDAT(MCOM,MLCOM,MGT,MDF),
     & AZIJL(MCOM,MGT),SUMDF(MCOM,MLCOM,MDF),NUDWIN(MCOM)

C Daylight coefficient common block data definition:
      COMMON/DCOEF/NDCFP,NDCFS(MDCFP),DCOEF(MDCFP,MDCFS,MDCFV),
     & IDCFID(MCOM,MLCOM,MDF)

      character cgcdesc*64,outs*128
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      CHARACTER tmp*16,LCGCIN*72,ACT*1
      DIMENSION NCGTCX(MDTY)
      INTEGER ILEN,NCGTCX,LENGTHX,ITRUNCX

      IER=0
      if (ACT(1:1).eq.'s') then
        IACT=3
      else
        IACT=4
      endif

C Initialise file.
      IUNIT=IFIL+1
      CALL EFOPSEQ(IUNIT,LCGCIN,IACT,IER)
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF

C Write data to file.
      WRITE(IUNIT,'(A)',IOSTAT=ios,ERR=1)'*Casual_gain_ctl'
      WRITE(IUNIT,'(A,A)',IOSTAT=ios,ERR=1) '*desc ',
     &  cgcdesc(icomp)(1:lnblnk(cgcdesc(icomp)))
      DO 400 IDTY=1,NBDAYTYPE
        NCGTCX(IDTY)=NCGTC(ICOMP,IDTY)
 400  CONTINUE
      OUTS=' '
      ILEN=1
      CALL AILIST(ILEN,NBDAYTYPE,NCGTCX,MDTY,' ',OUTS,lengthX,itruncX)
      ILEN=LNBLNK(OUTS)
      WRITE(IUNIT,'(2A)',IOSTAT=ios,ERR=1) OUTS(1:ILEN),
     &  ' # Gain to ctl for different day types'
      WRITE(IUNIT,'(2I3,A)',IOSTAT=ios,ERR=1)ICGCS(ICOMP),ICGCF(ICOMP),
     &  '  # Control period start & end hours'
      WRITE(IUNIT,'(I3,A)',IOSTAT=ios,ERR=1)NLITZ(ICOMP),
     &  '  # No. of lighting zone(s)'
      DO 111 N=1,NLITZ(ICOMP)
        WRITE(IUNIT,'(f6.1,3f5.1,i3,A)',IOSTAT=ios,ERR=1)SETPT(ICOMP,N),
     &   SOFFLL(ICOMP,N),SMLOUT(ICOMP,N),SMEOUT(ICOMP,N),
     &   IOFFDT(ICOMP,N),' # Setpt, switch off lvl, min dim/el, delay'
        WRITE(IUNIT,'(f5.2,2i3,A)',IOSTAT=ios,ERR=1) SYSPER(ICOMP,N),
     &   NDF(ICOMP,N),IDFST(ICOMP,N),
     &   ' # % casual, no of sensors, calc type' 
        IF(IDFST(ICOMP,N).EQ.5)THEN
          DO 201 J=1,NDF(ICOMP,N)
            WRITE(IUNIT,'(6F7.3,I3,A)',IOSTAT=ios,ERR=1) CGX(ICOMP,N,J),
     &        CGY(ICOMP,N,J),CGH(ICOMP,N,J),UX(ICOMP,N,J),
     &        UY(ICOMP,N,J),UH(ICOMP,N,J),IDCFID(ICOMP,N,J),
     &        ' # Sensor x y z, dir vect & DC link'
201       CONTINUE
        ELSEIF(IDFST(ICOMP,N).EQ.3)THEN
            WRITE(IUNIT,'(2F7.3,A)',IOSTAT=ios,ERR=1) CGX(ICOMP,N,1),
     &        CGY(ICOMP,N,1),' # Sensor location & type'
        ELSE
          DO 200 J=1,NDF(ICOMP,N)
            WRITE(IUNIT,'(6F7.3,A)',IOSTAT=ios,ERR=1) CGX(ICOMP,N,J),
     &        CGY(ICOMP,N,J),CGH(ICOMP,N,J),UX(ICOMP,N,J),
     &        UY(ICOMP,N,J),UH(ICOMP,N,J),' # Sensor x y z & dir vect'
200       CONTINUE
        ENDIF
        IF(IDFST(ICOMP,N).EQ.2)THEN
          WRITE(IUNIT,'(I3,A)',IOSTAT=ios,ERR=1) NUDWIN(ICOMP),
     &     ' # No of TMC(s) in zone ' 
          DO 112 IL=1,NZSUR(ICOMP)
            icc=IZSTOCN(icomp,il)
            IF(SSOTF(icc)(1:4).NE.'OPAQ'
     &            .AND.SSOTF(icc)(1:4).NE.'CFC ')THEN
              DO 114 L=1,NDF(ICOMP,N) 
                WRITE(IUNIT,'(I3,F7.4,A)',IOSTAT=ios,ERR=1)IL,
     &               DFDAT(ICOMP,N,IL,L),' # TMC number & coresp DF '
  114         CONTINUE            
            ENDIF
  112     CONTINUE
        ENDIF
        if(ICGCFL(ICOMP,N).eq.-1)tmp='Alway ON '
        if(ICGCFL(ICOMP,N).eq.0) tmp='Alway OFF '
        if(ICGCFL(ICOMP,N).eq.1) tmp='On-Off '
        if(ICGCFL(ICOMP,N).eq.2) tmp='Step '
        if(ICGCFL(ICOMP,N).eq.3) tmp='Dimming ideal '
        if(ICGCFL(ICOMP,N).eq.4) tmp='On-Off probab '
        if(ICGCFL(ICOMP,N).eq.31)tmp='Integral reset '
        if(ICGCFL(ICOMP,N).eq.32)tmp='Closed loop '
        WRITE(IUNIT,'(I3,a,a)',IOSTAT=ios,ERR=1) ICGCFL(ICOMP,N),
     &     ' # Control law: ',tmp
        IF(ICGCFL(ICOMP,N).EQ.31.OR.ICGCFL(ICOMP,N).EQ.32)THEN
          WRITE(IUNIT,'(2F7.3,a)',IOSTAT=ios,ERR=1) SPELEC(ICOMP,N),
     &     SLOPEM(ICOMP,N),' # Night calib & control slope'
        ENDIF
  111 CONTINUE
              
C If daylight coefficient method write data.
        do 323 N=1,NLITZ(ICOMP)
          if (IDFST(ICOMP,N).EQ.5) then
            do 333 ISEN=1,NDF(ICOMP,N)
              JDC=IDCFID(ICOMP,N,ISEN)
			if (JDC.le.0) JDC=1 ! bounds error fix (?) ag@23.03.2010
              JST=NDCFS(JDC)
              write(IUNIT,'(2i3,a)') JDC,JST, 
     &               ' # Daylight coefficient No. & No. of stages '
              do 313 I = 1,JST
                write(IUNIT,'(i3,a)') I,' # Stage No. '
                do 343 ICN=1,145,5
                  write(IUNIT,*)(DCOEF(JDC,I,K),K=ICN,(ICN+4))
 343            continue
 313          continue
 333        continue
          endif
 323    continue

   12 CALL ERPFREE(IUNIT,ios)
      RETURN

    1 call edisp(iuout,'  Write error in CASCTMK.')
      IER=1
      goto 12
      end
