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

C  Opendb: Open materials constructions, multi-layer constructions and
C          optical db.
C  Zoneslist : Display Zones list and wait for INIPIC item selected
C  ZNLIST :    Takes the current selected zones and builds a descriptive
C              string to be used in headers.
C  ZoneDisp :  Displays zone based information
C  CompoList : Display Composites list and wait for INIPIC item selected
C  DispTRev :  Display the reverberation time
C  PLOTTrev : Plot the reverberation time curve


C ********** opendb (copy of code from eddb.F)
C Open materials, constructions, multi-layer constructions and
C optical properties databases.
      subroutine opendb(ier)
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN
      common/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

C Material data arrays.
      real matver
      integer matcats,matcatitems,matdbitems
      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
     &  matdbdate,matdbdoc,matcatname(MGCL),matcatdoc(MGCL)

      logical    XST,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,close
      DIMENSION ICLN(30)

      character SOPT*12,outs*124,GDESCR*36

      IER=0

C Open currently named materials database.  Note that
C by a minor rewrite of ICLN in rec 601 we can get by with changing
C this to 24 words wide.
      CALL ERPFREE(IFMAT,ISTAT)
      call EFOPRAN(IFMAT,LFMAT,40,0,IER)
      IF(ier.ne.0)THEN
        write(outs,'(a,a,a)') ' No materials database ',
     &     LFMAT(1:lnblnk(LFMAT)),' found!'
        call edisp(iuout,outs)
        MATDBOK=.FALSE.
      else

C Also check to see if the file has correct info in 
        IREC=601
        READ(IFMAT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NCLASS,
     &    (ICLN(I),I=1,30)
        if(NCLASS.LE.0)then
          goto 1000   ! no classifications so try alternatives
        else
          MATDBOK=.TRUE.
          matver = 0.0
          goto 139  !  a binary file found so carry on.
        endif
 1000   CALL ERPFREE(IFMAT,ISTAT)  ! if a read error look at alternatives.
        call rascimat(IFMAT,LFMAT,IER)  ! try current ascii format
        if(ier.eq.-2)then
          ier=0
          call rlegacymat(IFMAT,LFMAT,ier)  ! try older ascii format
          if(ier.ne.0)then
            call usrmsg('No readable materials database was not found',
     &                  'or file was corrupt','W')
            MATDBOK=.FALSE.
          else
            call eclose(matver,1.1,0.001,close)
            if(close)then
              call edisp(iuout,' Filled material data arrays...')
              MATDBOK=.TRUE.
            else
              call usrmsg(
     &          'No readable material database was found. ',
     &          'check other warnings for advise.','W')
              MATDBOK=.FALSE.
            endif
          endif
        elseif(ier.eq.0)then
          call eclose(matver,1.1,0.001,close)
          if(close)then
            call edisp(iuout,' Filled material data arrays...')
            MATDBOK=.TRUE.
          endif
        endif
      endif
        
C Read multilayer db information into common.
 139  CALL ERPFREE(IFMUL,ISTAT)
      call FINDFIL(LFMUL,XST)
      if(XST)then
        CALL ERMLDB(0,IUOUT,IER)
        IF(IER.NE.0)then
          write(outs,'(a,a,a)') ' Problem with constr. composites db ',
     &     LFMAT(1:lnblnk(LFMAT)),' !'
          call edisp(iuout,outs)
          MLDBOK=.FALSE.
        else
          call usrmsg(' Opened constr. composites db.',' ','p')
          MLDBOK=.TRUE.
        endif
      else
        write(outs,'(a,a,a)') ' No constr. composites db ',
     &     LFMAT(1:lnblnk(LFMAT)),' found!'
        call edisp(iuout,outs)
      endif
C Open optical database and read into common.
      SOPT='ALL'
      CALL EROPTDB(0,iuout,SOPT,GDESCR,IER)
      if(ier.ne.0)then
        call usrmsg(
     &  ' Optical properties db not found or there was a problem',
     &  ' reading it!','W')
        OPTKOK=.FALSE.
      else
        call usrmsg(' Opened optical properties db!',' ','p')
        OPTKOK=.TRUE.
      endif
      return
      end


C********************* Zoneslist (copy of MOZDFN from resdef.F) ****
C Display Zones list and wait for INIPIC item selected

      SUBROUTINE Zoneslist
#include "building.h"
#include "geometry.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/pophelp/h(60)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/surfsel/isursel(MCON),NbSurSel(MCOM)
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      COMMON/ERRHAND/IDerr

      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      DIMENSION IVALZ(MCOM)
      CHARACTER H*72
      
      do 5 i=1,MCOM
        NbSurSel(i) = 0
  5   continue

      if (.not.cfgok) goto 666
C Clear nznog array and isursel array (remembers which surfaces have
C been selected).
      do 6 II=1,MCOM
        nznog(II)=0
  6   continue
      NZG=0
      do 8 II=1,MCON
        isursel(II)=0
  8   continue
      if(NCOMP.EQ.1)then
        call edisp(iuout,' Single zone model, this zone selected. ')
        call edisp(iuout,'  ')
        nznog(1)=1
      else

C Otherwise select the zones from a list of zone names.
        call edisp(iuout,' Zone selection... ')
        call edisp(iuout,'  ')

 7      H(1)='Pick one, several or All zones that must be included in'
        H(2)='the reverberation time calculation. The calculation will'
        H(3)='evaluated the performance over all the selected zones '
        H(4)='as it was corresponding to a single space. This allow to'
        H(5)='calculate the reverberation time of a space that is '
        H(6)='represented by several thermal zones (ex. atrium) '
        INPIC = NCOMP 
        CALL EPICKS(INPIC,IVALZ,' ',' Which zones to include: ',
     &    12,NCOMP,ZNAME,'Zone(s) list',IDerr,6)

C Check for errors (if => exit sub).
        if (IDerr.ne.0) goto 666
C If no zones have been selected then return to main menu.
        if (INPIC.eq.0) then
          izgfoc = 0           
          call edisp(iuout,' Returning to main menu.... ')
          GOTO 666
        endif
        NZG=INPIC
        izgfoc = Ncomp
        do 40 I=1,INPIC
          NZNOG(I)=IVALZ(I)
 40     continue

C Two zones cannot have the same number.
        DO 32 I=1,NZG
          DO 30 J=1,NZG
            IF(I.EQ.J)goto 30
            J1=NZNOG(I)
            J2=NZNOG(J)
            IF(J1.EQ.J2)then
              call edisp(iuout,'Two zones cannot have the same number!')
              goto 7
            endif
 30       continue
 32     continue
      endif

 666  RETURN
      END


C ******************** ZNLIST (edited from esrures/utils.F) *******
C ZNLIST takes the current selected zones and builds a descriptive
C string to be used in headers. 
C zdescr : string containing the names of the slected zone(s)
C length : Length of zdescr
      SUBROUTINE ZNLIST(zdescr,length,ierr)
#include "building.h"
#include "geometry.h"

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      CHARACTER zdescr*80,outs*244,outsd*80
      logical unixok

      length=0
      do 42 i=1,nzg
        lna=lnzname(nznog(i))
        length=length+lna+1
  42  continue
      if(length.lt.72)then
        WRITE(outs,5)(zname(nznog(I)),I=1,nzg)
    5   FORMAT(20(a12,' '))
        call sdelim(outs,outsd,'S',IW)
        zdescr=outsd
        return
      else
        if(nzg.eq.1)then
         WRITE(ZDESCR,'(A,I2,A,A)')' (',nznog(1),') ',zname(nznog(1))
        elseif(nzg.gt.1.and.nzg.LE.20)THEN
         WRITE(ZDESCR,6)(nznog(I),I=1,nzg)
    6    FORMAT(20(I2,','))
        ELSEIF(nzg.GT.20.AND.nzg.LE.24)THEN
         WRITE(ZDESCR,7)(nznog(I),I=1,9),(nznog(J),J=10,nzg)
    7    FORMAT(9(I1,','),15(I2,','))
        ELSEIF(nzg.GT.24)THEN
         WRITE(ZDESCR,8)(nznog(I),I=1,9),(nznog(J),J=10,24)
    8    FORMAT(9(I1,','),15(I2,','))
        ENDIF
        return
      endif

C If we got to this point must have been an error
      call isunix(unixok)
      if(unixok)then
        write(6,*) 'ZLIST: error writing zone names: ',outs
      endif
      ierr=1
      return
      END


C ************* ZoneDisp
C ZoneDisp - displays zone based information and allows....
      SUBROUTINE ZoneDisp(icomp)
#include "building.h"
#include "model.h"

C geometry.h provides commons G0/G2/G4/prec2/precz/prec17.
#include "geometry.h"

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/gzonpik/izgfoc,nzg,nznog(mcom)

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)

      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/SDATA/SNA(MS),GSA(MS),GWA(MS),PAZI(MS),PELV(MS)

      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      CHARACTER outs*124

      LOGICAL MODIFY,MODLEN,MODBND,zbzero
      logical close,context
      logical newgeo  ! to use for testing if new/old geometry file.

C Initialisation
      zbzero=.FALSE.
      newgeo=.false.  ! assume older format geometry.

C Read geometry file for chosen zone.
      ITMP=IFIL+1
      call eclose(gversion(icomp),1.1,0.01,newgeo)
      if(newgeo)then
        call georead(ITMP,LGEOM(ICOMP),ICOMP,0,iuout,IER)
      else
        call egomin(ITMP,LGEOM(ICOMP),ICOMP,0,0,iuout,IER)
      endif
      MODIFY=.TRUE.
      MODBND=.TRUE.
      nzg=1
      nznog(1)=ICOMP
      izgfoc=ICOMP
      CALL ADJVIEW(IER)

C Recover zone data (read only, no display).
      CALL ZINFO(SNA,ZOA,PAZI,PELV,ZVOL)
      Vol(icomp)= ZVOL
      CALL ECLOSE(ZBASEA(ICOMP),0.0,0.001,zbzero)
      FLA=0.
      IFLA=0
      OPQ=0.
      TRN=0.
      DO 41 I=1,NSUR
        icon=IZSTOCN(icomp,i)
        if(SSOTF(icon)(1:4).NE.'OPAQ'.AND.
     &            SSOTF(icon)(1:4).NE.'CFC ')then
          TRN=TRN+SNA(I)
        else
          OPQ=OPQ+SNA(I)
        endif
   41 CONTINUE

C Display high level data.
      write(outs,'(A,A)')' Composition of ',ZNAME(ICOMP)
      call edisp(iuout,outs)
      WRITE(outs,'(a,f8.2,a)')'  Volume :          ',VOL(ICOMP),'m^3'
      call edisp(iuout,outs)
      WRITE(outs,'(a,f8.2,a)')'  Base/floor area : ',ZBASEA(ICOMP),'m^2'
      call edisp(iuout,outs)
      WRITE(outs,'(a,f8.2,a)')'  Opaque constr. (OPQ):  ',OPQ,'m^2'
      call edisp(iuout,outs)
      WRITE(outs,'(a,f8.2,a)')'  Trans. constr. (TRN): ',TRN,'m^2'
      call edisp(iuout,outs)

C IF * BROWSE
      context=.false.
      CALL SURINFO(ICOMP,iuout,context)

      END


C********************* CompoList **************************
C Display Composites list and wait for INIPIC item selected

C IComfoc      (Integer) Composite selected
C NComPik          (Integer) Number of selected constructions (INPIC)
C LstComp(NMLC) (Integer) List of the selected constructions number
C WidthCo (Real) : Width [m] of the constructions, for LCA calculation.
C HeightCo (Real) : Height [m] of the constructions, for LCA calculation.


      SUBROUTINE CompoList
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/pophelp/h(60)

      character DESC*48
      integer NMLC,IPR,LAYERS
      real DTHK,DRAIR
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      COMMON/ERRHAND/IDerr
      COMMON/CompPik/IComfoc,NComPik,LstComp(MMLC)

      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK

      DIMENSION IVALZ(MCOM)

      CHARACTER H*72

C Read database if not already done.
      if (.not.MLDBOK) then 
        call ERMLDB(0,iuout,IER)
        IF(IER.NE.0)THEN
          CALL USRMSG(
     &    ' Error in reading construction.',
     &    ' Unable to create zone construction file!','W')
          RETURN
        ELSE
          MLDBOK=.TRUE.
        ENDIF
      ENDIF
      DO 6 II=1,NComPik
        LstComp(II)=0
 6    continue
      NComPik=0
C Otherwise select the zones from a list of zone names.
      call edisp(iuout,' Composite(s) selection... ')
      call edisp(iuout,'  ')
 7    H(1)='Pick one, several or All zones for inclusion in a'
      H(2)='xx from the list provided. '
      H(3)='You may choose them in any order. '
      INPIC=NMLC
      CALL EPICKS(INPIC,IVALZ,' ',' Which constructions to include:',
     &  20,NMLC,Desc,' Composite(s) list',IDerr,3)

C Check for errors (if => exit sub).
      if (IDerr.ne.0) RETURN

C If no construction have been selected then return to main menu.
      NComPik=INPIC
      if (NComPik .eq. 0) then
        IComfoc = 0           
        call edisp(iuout,' Returning to main menu.... ')
        RETURN
      ELSEIF (NComPik .eq. NMLC) THEN
        IComfoc = 0                       
        CALL EPAGE
        call edisp(iuout,' "*All items in list" selected.')
        RETURN
      ENDIF

      IComfoc = NComPik
      do 40 I=1,NComPik
        LstComp(I)=IVALZ(I)
 40   continue

C Two construction cannot have the same number.
      DO 32 I=1,NComPik
        DO 30 J=1,NComPik
          IF(I.EQ.J)goto 30
          J1=LstComp(I)
          J2=LstComp(J)
          IF(J1.EQ.J2)then
            call edisp(iuout,
     &            ' Two constructions cannot have the same number!')
            goto 7
          endif
 30     continue
 32   continue


      RETURN
      END


C******************** DispTRev **************************
C Display the reverberation time according to the selected mode (Mode),
C frequency band (IOctave). In numerical mode, ISE displays or not the 
C equivalent surfaces
C
C Mode : 1 = Numeric (3 lines) ;2 = Numeric (1 lines) 
C IOctave : 1 = 1 octave; 2 = 1/3 octave
C ISE :    Flag for detailed display
C          1 = Display only rev. time
C          2 = Display equivalente surfaces
C          3 = Display Absorption coefficients + equivalente surfaces
C
C Method : Set of reverberation time to display
C          1 = Sabine ; 2 = Eyring; 3 = Millington, 4 = all
C IChanel : Sending result to that chanel: 
C          1 = textual feedback or 2 = File

      SUBROUTINE DispTRev(Mode,IOctave,ISE,Method,IAirAb, IChanel)

#include "building.h"
#include "model.h"
#include "geometry.h"
#include "acoustic.h"

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/SDATA/SNA(MS),GSA(MS),GWA(MS),PAZI(MS),PELV(MS)

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)

      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MetNam/ACOName,CalName(4),LabFreq(MNbAbs)
      COMMON/AbsoCoef/SysAbsCo(MMLC,MNbAbs),PubAbsCo(MaxPub,MNbAbs),
     &                FurAbsCo(MaxPub,MNbAbs),AirAbs(MNbAbs)
      COMMON/AbsCHAR/PubType(MaxPub),FurType(MaxFur),FurCat(MaxFur)
      COMMON/PubFurZone/NbPub(MaxPub),XNbFur(MaxFur),SNAeff(MS),
     &                  IndexPub(MaxPub),IndexFur(MaxFur)
      COMMON/CPubFurZone/CovSurf(MaxFur)
      COMMON/AirPro/RTair,RHR
      COMMON/Trev/Divis(MNbAbs),SurfEquM(MS,MNbAbs), SEquM(MS,MNbAbs), 
     &            SurfEquP(MaxPub,MNbAbs), SurfEquF(MaxFur,MNbAbs),
     &            SurfEquT(MNbAbs), SurfTot, TReverb(MNbAbs), Tmean

      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      CHARACTER*36 RevLabA,RevLabB
      CHARACTER*63 FreqLabA,FreqLabB
      CHARACTER extraouts*189, outs*156
      CHARACTER CalName*12,ACOName*72
      CHARACTER*4 LabFreq
      CHARACTER PubType*12, FurType*12, FurCat*4, CovSurf*12
      logical newgeo  ! to use for testing if new/old geometry file.

      newgeo=.false.  ! assume older format geometry.
      ITMP=IFIL+1
      RevLabA = ' '
      RevLabB = ' '
      FreqLabA = ' '
      FreqLabB = ' '
      LabFreq(1) = ' 100'
      LabFreq(2) = ' 125'
      LabFreq(3) = ' 160'
      LabFreq(4) = ' 200'
      LabFreq(5) = ' 250'
      LabFreq(6) = ' 315'
      LabFreq(7) = ' 400'
      LabFreq(8) = ' 500'
      LabFreq(9) = ' 630'
      LabFreq(10) = ' 800'
      LabFreq(11) = '1000'
      LabFreq(12) = '1250'
      LabFreq(13) = '1600'
      LabFreq(14) = '2000'
      LabFreq(15) = '2500'
      LabFreq(16) = '3150'
      LabFreq(17) = '4000'
      
      if (Mode.le.2) then
        ICompact = Mode
      else
        continue
      endif
      IF (Method .LT. 4) THEN
        IStart = Method
        IStop = Method 
      ELSEIF (Method .EQ. 4) THEN
        IStart = 1
        IStop = 3
      ELSE
        write(outs,'(a,I1,a)')'The calculation method : ', Method,
     &      ' does not exist. Please check.'
        call edisp(Ichanel,outs)
      ENDIF
      DO 600 M = IStart, IStop
C First reset the diviseur (sum of absorption) when starting a new calculation method
C and should not reset when calculating the absorption of each zone ...
        DO 10 I=1,MNbAbs
          Divis(I)= 0.0
 10     Continue
        call edisp(Ichanel,
     &  '-------------------------------------------------------------')
        WRITE(outs,'(2a)') ' Calculation method: ', CalName(M)
        call edisp(Ichanel,outs)
        IF (IAirAb .EQ. 1) THEN
          WRITE(outs,'(a)') ' Air absroption: included '
          WRITE(outs,'(a,F6.2)') ' Air Temperature [C]: ', RTair
          call edisp(Ichanel,outs)
          WRITE(outs,'(a,F6.2)') ' Air Humidity [%]: ', RHR
          call edisp(Ichanel,outs)
        ELSE
          WRITE(outs,'(a)') ' Air absoption: NOT included '
        call edisp(Ichanel,outs)
        ENDIF

C Display left part of first label line (Revlab)
        IF (ICompact.EQ.1) THEN
          if (ISE.eq.1) then
            IF (IOctave.EQ.2) THEN
              FreqLabA= 'Frequency [Hz]'
              write(outs,'(50x,a)')FreqLabA
              call edisp(Ichanel,outs)
              write(FreqLabB,'(5(a1,1x,a,2x))')('|',LabFreq(I),I=3,15,3)
              write(extraouts,'(38x,a)')FreqLabB
              call edisp(Ichanel,extraouts)
              write(FreqLabB,'(6(a1,1x,a,2x))')('|',LabFreq(I),I=2,17,3)
              write(extraouts,'(38x,a)')FreqLabB
              call edisp(Ichanel,extraouts)
              write(FreqLabB,'(6(a1,1x,a,2x))')('|',LabFreq(I),I=1,16,3)
              write(extraouts,'(38x,a)')FreqLabB
              call edisp(Ichanel,extraouts)
            ENDIF
          elseif (ISE.eq.2) then
            IF (IOctave.EQ.2) THEN
              FreqLabA= 'Frequency [Hz]'
              write(outs,'(50x,a)')FreqLabA
              call edisp(Ichanel,outs)
              write(FreqLabB,'(5(a1,1x,a,2x))')('|',LabFreq(I),I=3,15,3)
              write(extraouts,'(36x,2x,a)')FreqLabB
              call edisp(Ichanel,extraouts)
              RevLabA = '   Surface    |  Area | Construction'
              write(FreqLabB,'(6(a1,1x,a,2x))')('|',LabFreq(I),I=2,17,3)
              write(extraouts,'(a36,2x,a)')RevLabA,FreqLabB
              call edisp(Ichanel,extraouts)
              RevLabB = '     name     |  [m2] |     name    '
              write(FreqLabB,'(6(a1,1x,a,2x))')('|',LabFreq(I),I=1,16,3)
              write(extraouts,'(a36,2x,a)')RevLabB,FreqLabB
              call edisp(Ichanel,extraouts)
            ENDIF
          elseif (ISE.eq.3) then
            IF (IOctave.EQ.2) THEN
              FreqLabA= 'Frequency [Hz]'
              write(outs,'(50x,a)')FreqLabA
              call edisp(Ichanel,outs)
              write(FreqLabB,'(5(a1,1x,a,2x))')('|',LabFreq(I),I=3,15,3)
              write(extraouts,'(36x,2x,a)')FreqLabB
              call edisp(Ichanel,extraouts)
              RevLabA = '   Surface    |  Area | Construction'
              write(FreqLabB,'(6(a1,1x,a,2x))')('|',LabFreq(I),I=2,17,3)
              write(extraouts,'(a36,2x,a)')RevLabA,FreqLabB
              call edisp(Ichanel,extraouts)
              RevLabB = '     name     |  [m2] |     name    '
              write(FreqLabB,'(6(a1,1x,a,2x))')('|',LabFreq(I),I=1,16,3)
              write(extraouts,'(a36,2x,a)')RevLabB,FreqLabB
              call edisp(Ichanel,extraouts)
            ENDIF
          endif
        ELSEIF (ICompact.EQ.2) THEN
          if (ISE.eq.1) then
            IF (IOctave.EQ.1) THEN
              FreqLabA= 'Frequency [Hz]'
              write(FreqLabB,'(6(a1,1x,a,2x))')('|',LabFreq(I),I=2,17,3)
              write(outs,'(50x,a)')FreqLabA
              write(extraouts,'(38x,a)')FreqLabB
            ELSEIF (IOctave.EQ.2) THEN
              FreqLabA= 'Frequency [Hz]'
              write(outs,'(41x,a)') FreqLabA
             write(extraouts,'(22x,17(a1,a,1x))')
     &                        ('|',LabFreq(I),I=1,17)
            ENDIF
          elseif (ISE.eq.2) then
            IF (IOctave.EQ.1) THEN
              RevLabA = '   Surface    |  Area | Construction'
              FreqLabA= '|            Frequency [Hz] '
              RevLabB = '     name     |  [m2] |     name'
              write(FreqLabB,'(6(a1,1x,a,2x))')('|',LabFreq(I),I=2,17,3)
              write(outs,'(a36,2x,a27)')RevLabA,FreqLabA
              write(extraouts,'(a33,5x,a)')RevLabB,FreqLabB
            ELSEIF (IOctave.EQ.2) THEN
              RevLabA = '   Surface    |Area/Nb|'
              FreqLabA= 'Frequency [Hz]'
              RevLabB = '    name      | [m2/-] '
              write(outs,'(a22,30x,a37)')RevLabA,FreqLabA
              write(extraouts,'(a22,17(a1,a,1x))') RevLabB,
     &                                        ('|',LabFreq(I),I=1,17)
            ENDIF
          elseif (ISE.eq.3) then
            IF (IOctave.EQ.1) THEN
              RevLabA = '   Surface            | Construction'
              FreqLabA= '|            Frequency [Hz] '
              RevLabB = '     name             |     name'
              write(FreqLabB,'(6(a1,1x,a,2x))')('|',LabFreq(I),I=2,17,3)
              write(outs,'(a36,2x,a27)')RevLabA,FreqLabA
              write(extraouts,'(a33,5x,a)')RevLabB,FreqLabB              
            ELSEIF (IOctave.EQ.2) THEN
              RevLabA = '   Surface             |'
              FreqLabA= 'Frequency [Hz]'
              RevLabB = '    name     '
              write(outs,'(a25,30x,a37)')RevLabA,FreqLabA
              write(extraouts,'(a16,7x,17(a1,a,1x))') RevLabB,
     &                                        ('|',LabFreq(I),I=1,17)
            ENDIF
          endif        
        ELSEIF (ICompact.EQ.3) THEN
          call edisp(Ichanel,'Not yet implemented')
          Continue
        ENDIF
        if (ICompact.EQ.2) then 
          call edisp(Ichanel,outs)
          call edisp(Ichanel,extraouts)
        endif



C Display absorption coefficients information of MATERIAL 
C for each surface in the zone according to the frequency band
          IF (ISE .EQ. 3) THEN
            call edisp(Ichanel,' Absorption coeff.')
            DO 399 J =1,NZG 
              if (NZG.GT.1) then
                 WRITE(outs,'(a,a)') 'zone: ', ZNAME(nZnog(J))
                 call edisp(Ichanel,outs)
              endif
              call eclose(gversion(nZnog(J)),1.1,0.01,newgeo)
              if(newgeo)then
                call georead(ITMP,LGEOM(nZnog(J)),nZnog(J),0,iuout,
     &           IER)
              else
                call egomin(ITMP,LGEOM(nZnog(J)),nZnog(J),0,0,iuout,
     &           IER)
              endif
              CALL ZINFO(SNA,ZOA,PAZI,PELV,ZVOL)
            DO 400 I = 1,NSUR
              icon=IZSTOCN(nZnog(J),I)
              In = MLCIndex(SSMLCN(icon))
              IF (SNA(I) .GT. 0.) THEN
                if (IOctave .EQ. 1) then
                  WRITE(outs,311) SSNAME(icon), SSMLCN(icon),
     &                           (SysAbsCo(In,IY),IY=2,17,3)
 311              FORMAT(1X,A12,3x, 5x, 3x, A12, 6(4x, F4.2))
                  call edisp(Ichanel,outs)
                elseif (IOctave .EQ. 2) then
                  IF (ICompact.EQ.1) THEN
                    WRITE(outs,321) SSNAME(icon),SNA(I), 
     &                 SSMLCN(icon),(SysAbsCo(In,IY),IY=3,15,3)
 321                FORMAT(1X,A12,3x,F5.1,3x,a12, 6(4x, F4.2))
                    call edisp(Ichanel,outs)
                    WRITE(outs,322) (SysAbsCo(In,IY),IY=2,17,3)
 322                FORMAT(36X,6(4x, F4.2))
                    call edisp(Ichanel,outs)
                    WRITE(outs,323) (SysAbsCo(In,IY),IY=1,16,3)
 323                FORMAT(36x,6(4x, F4.2))
                    call edisp(Ichanel,outs)
                    call edisp(Ichanel,' ')
                  ELSE
C Display material name rather surface name
                    WRITE(outs,324) SSMLCN(icon),
     &               (SysAbsCo(In,IY),IY=1,17)
 324                FORMAT(1X,A12,2x,7x, 17(2x, F4.2))
                    call edisp(Ichanel,outs)
                  ENDIF
                endif
              ENDIF
 400        CONTINUE
 399        CONTINUE 
            call edisp(Ichanel,' ')

C Display absorption coefficients information of PUBLIC
C for each surface in the zone according to the frequency band
            DO 499 k =1,NZG 
              call scnzoneaco(ZNAME(nZnog(k)),0,IER)
              DO 500 It = 1,MaxPub
                IF (NbPub(It) .GT. 0) THEN
                  IF ((It .EQ.1).AND.(k.LE.1)) THEN
                    call edisp(Ichanel,'Public')
                  ENDIF
                  if (NZG.GT.1) then
                    WRITE(outs,'(a,a)') 'zone: ', ZNAME(nZnog(k))
                    call edisp(Ichanel,outs)
                  endif
                  I = IndexPub(It)
                  if (IOctave .EQ. 1) then
                    WRITE(outs,411) PubType(I),'  ',
     &                           (PubAbsCo(I,J),J=2,17,3)
 411                FORMAT(1X,A12,3x, 5x, 3x, A12, 6(4x, F4.2))
                    call edisp(Ichanel,outs)
                  elseif (IOctave .EQ. 2) then
                    IF (ICompact.EQ.1) THEN
                      WRITE(outs,421)  PubType(I),NbPub(I), 
     &                              (PubAbsCo(I,J),J=3,15,3)
 421                  FORMAT(1X,A12,3x,I5,3x,12x, 6(4x, F4.2))
                      call edisp(Ichanel,outs)
                      WRITE(outs,422) (PubAbsCo(I,J),J=2,17,3)
 422                  FORMAT(36X,6(4x, F4.2))
                      call edisp(Ichanel,outs)
                      WRITE(outs,423) (PubAbsCo(I,J),J=1,16,3)
 423                  FORMAT(36x,6(4x, F4.2))
                      call edisp(Ichanel,outs)
                      call edisp(Ichanel,' ')
                    ELSE
C Display public name rather surface name
                      WRITE(outs,424) PubType(I),
     &                                (PubAbsCo(I,J),J=1,17)
 424                  FORMAT(1X,A12,2x,7x, 17(2x, F4.2))
                      call edisp(Ichanel,outs)
                    ENDIF
                  endif
                ELSE
                  GOTO 499
                ENDIF
 500          CONTINUE
 499        CONTINUE


            call edisp(Ichanel,' ')

C Display absorption coefficients information of FURNITURE
C for each surface in the zone according to the frequency band
            DO 699 k =1,NZG 
              call scnzoneaco(ZNAME(nZnog(k)),0,IER)
              DO 700 It = 1,MaxFur
                IF (XNbFur(It) .GT. 0) THEN
                  IF ((It .EQ.1).AND.(k.Eq.1)) THEN
                    call edisp(Ichanel,'Furniture')
                  ENDIF
                  if ((NZG.GT.1).AND.(It.EQ.1)) then
                    WRITE(outs,'(a,a)') 'zone: ', ZNAME(nZnog(k))
                    call edisp(Ichanel,outs)
                  endif
                  I = IndexFur(It)
                  if (IOctave .EQ. 1) then
                    IF (CovSurf(I).EQ.'none') THEN
                      WRITE(outs,511) FurType(IndexFur(I)),' ',
     &                           (FurAbsCo(IndexFur(I),J),J=2,17,3)
 511                  FORMAT(1X,A12,3x, 5x, 3x, A12, 6(4x, F4.2))
                    ELSE
                      WRITE(outs,512) CovSurf(I),' has ',
     &                              FurType(IndexFur(I)), 
     &                             (FurAbsCo(IndexFur(I),J),J=2,17,3)
 512                  FORMAT(1X,A12,3x, A5, 1x, A12,2x, 6(2x, F6.2))
                    ENDIF
                    call edisp(Ichanel,outs)
                  elseif (IOctave .EQ. 2) then
                    IF (ICompact.EQ.1) THEN
                      IF (FurCat(IndexFur(I)).EQ.'Surf') THEN
                        WRITE(outs,521)  FurType(IndexFur(I)),XNbFur(I), 
     &                              (FurAbsCo(IndexFur(I),J),J=3,15,3)
 521                    FORMAT(1X,A12,3x,F5.1,3x,12x, 6(4x, F4.2))
                      ELSE
                        WRITE(outs,526)  FurType(IndexFur(I)), 
     &                 INT(XNbFur(I)),(FurAbsCo(IndexFur(I),J),J=3,15,3)
 526                    FORMAT(1X,A12,3x,I5,3x,12x, 6(4x, F4.2))
                      ENDIF
                      call edisp(Ichanel,outs)
                      WRITE(outs,522) (FurAbsCo(IndexFur(I),J),J=2,17,3)
 522                  FORMAT(36X,6(4x, F4.2))
                      call edisp(Ichanel,outs)
                      WRITE(outs,523) (FurAbsCo(IndexFur(I),J),J=1,16,3)
 523                  FORMAT(36x,6(4x, F4.2))
                      call edisp(Ichanel,outs)
                      call edisp(Ichanel,' ')
                    ELSE
C Display public name rather surface name
                      WRITE(outs,524) FurType(IndexFur(I)),
     &                 (FurAbsCo(IndexFur(I),J),J=1,17)
 524                  FORMAT(1X,A12,2x,7x, 17(2x, F4.2))
                      call edisp(Ichanel,outs)
                    ENDIF
                  endif
                ELSE
                  GOTO 699
                ENDIF
 700          CONTINUE
 699        CONTINUE


            call edisp(Ichanel,' ')
          ENDIF

C Display EQUIVALENT area information 
C for each surface in the zone according to the frequency band 
          IF (ISE .GE. 2) THEN
            call edisp(Ichanel,' ')
            IF (ISE.EQ.3) THEN
              write(outs,'(a25,30x,a37)')RevLabA,FreqLabA
              write(extraouts,'(23x,17(a1,a,1x))') 
     &                                        ('|',LabFreq(I),I=1,17)
             call edisp(Ichanel,outs)
              call edisp(Ichanel,extraouts)
            ENDIF
            call edisp(Ichanel,' Equivalent area [m2]')
            if ((IOctave .EQ. 2).AND.(ICompact.EQ.1)) then
              FreqLabA= 'Frequency [Hz]'
              write(outs,'(50x,a)')FreqLabA
              call edisp(Ichanel,outs)
              write(FreqLabB,'(5(a1,1x,a,2x))')('|',LabFreq(J),J=3,15,3)
              write(extraouts,'(36x,2x,a)')FreqLabB
              call edisp(Ichanel,extraouts)
              write(FreqLabB,'(6(a1,1x,a,2x))')('|',LabFreq(J),J=2,17,3)
              write(extraouts,'(38x,a)')FreqLabB
              call edisp(Ichanel,extraouts)
              write(FreqLabB,'(6(a1,1x,a,2x))')('|',LabFreq(J),J=1,16,3)
              write(extraouts,'(38x,a)')FreqLabB
              call edisp(Ichanel,extraouts)
            endif

C Display equivalent surface for MATERIAL
            DO 199 k =1,NZG 
              if (NZG.GT.1) then
                WRITE(outs,'(a,a)') 'zone: ', ZNAME(nZnog(k))
                call edisp(Ichanel,outs)
                call edisp(Ichanel,'-----')
              endif
              call eclose(gversion(nZnog(k)),1.1,0.01,newgeo)
              if(newgeo)then
                call georead(ITMP,LGEOM(nZnog(k)),nZnog(k),0,iuout,IER)
              else
                call egomin(ITMP,LGEOM(nZnog(k)),nZnog(k),0,0,iuout,
     &           IER)
              endif
              CALL ZINFO(SNA,ZOA,PAZI,PELV,ZVOL)
              call SurfEquiCalc(nznog(k), M, IAirAb)
            DO 200 I = 1,NSUR
              icon=IZSTOCN(nZnog(k),I)
              ISys = MLCIndex(SSMLCN(icon))
              IF (SNA(I) .GT. 0.) THEN
                if (IOctave .EQ. 1) then
                  WRITE(outs,111)SSNAME(icon),SNA(I),SSMLCN(icon),
     &                           (SurfEquM(I,IY),IY=2,17,3)
 111              FORMAT(1X,A12,1x, F7.1, 1x, A12, 2x, 6(2x, F6.1))
                  call edisp(Ichanel,outs)
                  IF (ABS(SNA(I)-SNAeff(I)).GT.0.001) THEN
                    DO 997 J = 1,MaxFur
                      if (CovSurf(J).EQ.SSNAME(icon)) then
                        WRITE(outs,116) '*',-XNbFur(J),FurType(J),
     &                    (-XNbFur(J)* SysAbsCo(ISys,IY),IY=2,17,3)
 116                    FORMAT(13x,A1,1x,F7.1,1x,A12,2x, 6(2x,F6.1))
                        call edisp(Ichanel,outs)
                      endif
 997                CONTINUE
                  ENDIF
                elseif (IOctave .EQ. 2) then
                  IF (ICompact.EQ.1) THEN
                    WRITE(outs,206)SSNAME(icon),SNA(I),
     &                SSMLCN(icon),(SurfEquM(I,IY),IY=3,15,3)
 206                FORMAT(1X,A12,3x, F7.1, 3x, A12, 6(2x, F6.1))
                    call edisp(Ichanel,outs)
                    WRITE(outs,208) (SurfEquM(I,IY),IY=2,17,3)
 208                FORMAT(37X,6(2x, F6.1))
                    call edisp(Ichanel,outs)
                    WRITE(outs,209) (SurfEquM(I,IY),IY=1,16,3)
 209                FORMAT(37x,6(2x, F6.1))
                    call edisp(Ichanel,outs)
                    call edisp(Ichanel,' ')
                    if (ABS(SNA(I)-SNAeff(I)).GT.0.001) then
C If furniture infront of surface
                      DO 999 J = 1,MaxFur
                        if (CovSurf(J).EQ.SSNAME(icon)) then
                          WRITE(outs,216)'*',-XNbFur(J),FurType(J),
     &                      (-XNbFur(J)* SysAbsCo(ISys,IY),IY=3,15,3)
 216                      FORMAT(1X,A12,3x, F6.1, 3x, A12, 
     &                           6(2x, F6.1))
                          call edisp(Ichanel,outs)
                          WRITE(outs,218)(-XNbFur(J)* 
     &                              SysAbsCo(ISys,IY),IY=2,17,3)
 218                      FORMAT(37X,6(2x, F6.1))
                          call edisp(Ichanel,outs)
                          WRITE(outs,219)(-XNbFur(J)* 
     &                              SysAbsCo(ISys,IY),IY=1,16,3)
 219                      FORMAT(37x,6(2x, F6.1))
                          call edisp(Ichanel,outs)
                          call edisp(Ichanel,' ')
                        endif
 999                  CONTINUE
                    endif
                  ELSE
                    WRITE(extraouts,214) SSNAME(icon),SNA(I),
     &                         (SurfEquM(I,IY),IY=1,17)
 214                FORMAT(1X,A12,1x,F7.1,1x,17(F6.1))
                    call edisp(Ichanel,extraouts)

                    if (ABS(SNA(I)-SNAeff(I)).GT.0.001) then
C If furniture infront of surface
                      DO 998 J = 1,MaxFur
                       if (CovSurf(J).NE.SSNAME(icon)) then
                          WRITE(extraouts,117) '*',FurType(IndexFur(J)),
     &                    -XNbFur(J),
     &                    (-XNbFur(J)* SysAbsCo(Isys,IY),IY=1,17)
 117                      FORMAT(1x,A1,A12,1x,F6.1,1x, 17(F6.1))
                          call edisp(Ichanel,extraouts)
                        endif
 998                  CONTINUE
                    endif
                  ENDIF
                endif
              ENDIF
 200        CONTINUE

C Display equivalent surface for PUBLIC
            DO 300 I = 1,MaxPub
              IF (NbPub(I) .GT. 0) THEN
                if (I .EQ. 1) then
                  call edisp(iuout,' Occupants in Zone:')
                endif
                if (IOctave .EQ. 1) then
                  WRITE(outs,221) PubType(I), NbPub(I), 
     &                           (SurfEquP(I,J),J=2,17,3)
 221              FORMAT(1X,A12,3x, I5, 3x, 12x,1x, 6(2x, F6.1))
                  call edisp(Ichanel,outs)
                elseif (IOctave .EQ. 2) then
                  IF (ICompact.EQ.1) THEN
                    WRITE(outs,301) PubType(I), NbPub(I),
     &                              (SurfEquP(I,J),J=3,15,3)
 301                FORMAT(1X,A12,3x, I5, 3x, 12x, 1x, 6(2x, F6.1))
                    call edisp(Ichanel,outs)
                    WRITE(outs,312) (SurfEquP(I,J),J=2,17,3)
 312                FORMAT(37X,6(2x, F6.1))
                    call edisp(Ichanel,outs)
                    WRITE(outs,313) (SurfEquP(I,J),J=1,16,3)
 313                FORMAT(37x,6(2x, F6.1))
                    call edisp(Ichanel,outs)
                    call edisp(Ichanel,' ')
                  ELSE
                    WRITE(extraouts,314)  PubType(I), NbPub(I),
     &                           (SurfEquP(I,J),J=1,17)
 314                FORMAT(1X,A12,1x,I4, 4x,17(F6.1))
                    call edisp(Ichanel,extraouts)
                  ENDIF
                endif
              ENDIF
 300        CONTINUE

C Display equivalent surface for FURNITURE 
C ONLY if not in front of surface (This lastest already 
C display in the material list)
            DO 430 I = 1,MaxFur
              IF (XNbFur(I) .GT. 0.) THEN
                if (I .EQ. 1) then
                  call edisp(iuout,' Furniture in Zone:')
                endif
                if (IOctave .EQ. 1) then
                  IF (FurCat(IndexFur(I)).EQ.'Surf') THEN
                    WRITE(outs,331) FurType(IndexFur(I)), XNbFur(I),
     &                    '(',CovSurf(I) ,')', (SurfEquF(I,J),J=2,17,3)
 331                FORMAT(1X,A12,3x, F5.1,2x,A1,A12,A1,6(2x, F6.1))
                  ELSE
                    WRITE(outs,332) FurType(IndexFur(I)),INT(XNbFur(I)),
     &                             (SurfEquF(I,J),J=2,17,3)
 332                FORMAT(1X,A12,3x, I5, 3x, 12x, 1x, 6(2x, F6.1))
                  ENDIF
                  call edisp(Ichanel,outs)
                elseif (IOctave .EQ. 2) then
                  IF (ICompact.EQ.1) THEN
                    IF (FurCat(IndexFur(I)).EQ.'Surf') THEN
                      WRITE(outs,401) FurType(IndexFur(I)), XNbFur(I),
     &                             (SurfEquF(I,J),J=2,17,3)
 401                  FORMAT(1X,A12,3x, F5.1, 3x, 12x, 1x, 6(2x, F6.1))
                    ELSE
                      WRITE(outs,402) FurType(IndexFur(I)),
     &                         INT(XNbFur(I)),(SurfEquF(I,J),J=2,17,3)
 402                  FORMAT(1X,A12,3x, I5, 3x, 12x, 1x, 6(2x, F6.1))
                    ENDIF
                    call edisp(Ichanel,outs)
                    WRITE(outs,412) (SurfEquF(I,J),J=2,17,3)
 412                FORMAT(37X,6(2x, F6.1))
                    call edisp(Ichanel,outs)
                    WRITE(outs,413) (SurfEquF(I,J),J=1,16,3)
 413                FORMAT(37x,6(2x, F6.1))
                    call edisp(Ichanel,outs)
                    call edisp(Ichanel,' ')
                  ELSE
                    IF (FurCat(IndexFur(I)).EQ.'Surf') THEN
                     WRITE(extraouts,414)FurType(IndexFur(I)),XNbFur(I),
     &                             (SurfEquF(I,J),J=1,17)
 414                  FORMAT(1X,A12,2x, F6.1, 1x, 17F6.1)
                    ELSE
                      WRITE(extraouts,415) FurType(IndexFur(I)),
     &                         INT(XNbFur(I)),(SurfEquF(I,J),J=1,17)
 415                  FORMAT(1X,A12,1x,I4, 4x,17(F6.1))
                    ENDIF
                    call edisp(Ichanel,extraouts)
                  ENDIF
                endif
              ENDIF
 430        CONTINUE

C Display Total Equivalent surface according to frenquency band
            CALL edisp(Ichanel,' ')
            IF (IOctave .EQ. 1) THEN
              WRITE(outs,122) 'Total equivalent area [m2]',
     &                         (SurfEquT(IY),IY=2,17,3)
 122          FORMAT(10x,A, 1x, 6(2x, F6.1))
              call edisp(Ichanel,outs)
            ELSEIF ((IOctave .EQ. 2).AND.(ICompact.EQ.1)) THEN
              FreqLabA= 'Frequency [Hz]'
              write(outs,'(50x,a)')FreqLabA
              call edisp(Ichanel,outs)
              write(FreqLabB,'(5(a1,1x,a,2x))')('|',LabFreq(I),I=3,15,3)
              write(extraouts,'(36x,2x,a)')FreqLabB
              call edisp(Ichanel,extraouts)
              write(FreqLabB,'(6(a1,1x,a,2x))')('|',LabFreq(I),I=2,17,3)
              write(extraouts,'(38x,a)')FreqLabB
              call edisp(Ichanel,extraouts)
              write(FreqLabB,'(6(a1,1x,a,2x))')('|',LabFreq(I),I=1,16,3)
              write(extraouts,'(38x,a)')FreqLabB
              call edisp(Ichanel,extraouts)
              IF (ICompact.EQ.1) THEN
                WRITE(outs,132) 'Total equivalent area [m2]',
     &                           (SurfEquT(IY),IY=3,15,3)
 132            FORMAT(10x,A, 1x, 6(2x, F6.1))
                call edisp(Ichanel,outs)
                WRITE(outs,142) (SurfEquT(IY),IY=2,17,3)
 142            FORMAT(37x, 6(2x, F6.1))
                call edisp(Ichanel,outs)
                WRITE(outs,152) (SurfEquT(IY),IY=1,16,3)
 152            FORMAT(37x, 6(2x, F6.1))
                call edisp(Ichanel,outs)
              ELSE
                WRITE(extraouts,153) 'Total',
     &                               (SurfEquT(IY),IY=1,17)
 153            FORMAT(1x,A14, 17(1x, F5.1))
                call edisp(Ichanel,extraouts)
              ENDIF
            ELSEIF ((IOctave .EQ. 2).AND.(ICompact.EQ.2)) THEN
              WRITE(extraouts,162) 'Total eq. area in zone',
     &                         (SurfEquT(IY),IY=1,17)
 162          FORMAT(A,17F6.1)
              call edisp(Ichanel,extraouts)
            ENDIF

C Display equivalent surface for Air absorption
            IF (ISE .GE. 2) THEN
              IF (IAirAb .eq. 1) THEN
                call edisp(Ichanel,' ')
                if (ICompact.EQ.1) then
                  WRITE(outs,192) 'Air absorption in zone',
     &                           (AirAbs(IY),IY=3,15,3)
 192              FORMAT(14x,A, 1x, 6(2x, F6.1))
                  call edisp(Ichanel,outs)
                  WRITE(outs,172) (AirAbs(IY),IY=2,17,3)
 172              FORMAT(37x, 6(2x, F6.1))
                  call edisp(Ichanel,outs)
                  WRITE(outs,182) (AirAbs(IY),IY=1,16,3)
 182              FORMAT(37x, 6(2x, F6.1))
                  call edisp(Ichanel,outs)
                else
                  IF (IOctave .EQ. 1) THEN
                    WRITE(extraouts,118) 'Air absorption in zone',
     &                           (AirAbs(IY),IY=2,17,3)
 118                FORMAT(10x,A,7x, 17(F6.1,2x))
                  ELSE
                    WRITE(extraouts,119) 'Air absorption in zone',
     &                           (AirAbs(IY),IY=1,17)
 119                FORMAT(A,17(F6.1))
                  ENDIF
                  call edisp(Ichanel,extraouts)
                endif
              ELSE
                WRITE(extraouts,'(a)') 'Air absorption not included'
                call edisp(Ichanel,extraouts)
              ENDIF
              call edisp(Ichanel,' ')
            ENDIF

 199        CONTINUE
          ELSE

C Should any way compute the equivalent area
            DO 691 k =1,NZG 
              call eclose(gversion(nZnog(k)),1.1,0.01,newgeo)
              if(newgeo)then
                call georead(ITMP,LGEOM(nZnog(k)),nZnog(k),0,iuout,IER)
              else
                call egomin(ITMP,LGEOM(nZnog(k)),nZnog(k),0,0,iuout,
     &           IER)
              endif
              CALL ZINFO(SNA,ZOA,PAZI,PELV,ZVOL)
              call SurfEquiCalc(nznog(k), M, IAirAb)
 691        CONTINUE
          ENDIF

C Display Reverberation time according to frenquency band
C Display loops trough the calculation method
C Then for the selected method(s) display the corresponding results
          call TrevCalc
          IF (IOctave .EQ. 1) THEN
            WRITE(outs,173) 'REVERBERATION TIME [s]',
     &             (TReverb(IY),IY=2,17,3), TMean,' (Mean value)'
 173        FORMAT(14x,a, 1x,7(4x, F4.2),a)
            call edisp(Ichanel,outs)
          ELSEIF (IOctave .EQ. 2) THEN
            if (ICompact.EQ.1) then
              WRITE(outs,123) 'REVERBERATION TIME [s]',
     &                    (TReverb(IY),IY=3,15,3)
 123          FORMAT(15x,a, 7(4x, F4.2))
              call edisp(Ichanel,outs)
              WRITE(outs,133) (TReverb(IY),IY=2,17,3)
 133          FORMAT(37x,7(4x, F4.2))
              call edisp(Ichanel,outs)
              WRITE(outs,143)(TReverb(IY),IY=1,16,3),TMean,
     &                       ' (Mean value)'
 143          FORMAT(37x,7(4x, F4.2),a)
              call edisp(Ichanel,outs)
              call edisp(Ichanel,' ')
            else
              WRITE(extraouts,140)'REVERBERATION TIME [s]',
     &                             (TReverb(IY),IY=1,17)
 140          FORMAT(1x,A,17(1x, F4.2,1x))
              call edisp(Ichanel,extraouts)
              call edisp(Ichanel,' ')
              WRITE(outs,154)'Mean reverberation time ',TMean,' [s]'
 154          FORMAT(1x,a,F4.2,a)
              call edisp(Ichanel,outs)
            endif
            CALL edisp(Ichanel,' ')
          ENDIF
          CALL PLOTTrev(method,M)
C Next Reverberation time calculation
 600    CONTINUE

      RETURN
      END


C ********************* PLOTTrev *********************
C PLOTTrev - This routine traces the results of the reverberation
C calculation. If all the reverberation time calculation have
C been selected, then add each curve on the same graph.
C MALL       - Index for the calculation metod used (4=all)
C MethodCalc - Index for the current calculation metod (1: Sabine..)
C Isymbol    - Symbol used to draw a curve (see etplot_ in wwcut.c for listing)
C RESMX      - Maximum reverberation time
C RESMN      - Minimum reverberation time

      SUBROUTINE PLOTTrev(MAll, MethodCalc)

#include "building.h"
#include "acoustic.h"

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/Zones/TxtZone

      COMMON/MetNam/ACOName,CalName(4),LabFreq(MNbAbs)

      COMMON/Trev/Divis(MNbAbs),SurfEquM(MS,MNbAbs), SEquM(MS,MNbAbs), 
     &            SurfEquP(MaxPub,MNbAbs), SurfEquF(MaxFur,MNbAbs),
     &            SurfEquT(MNbAbs), SurfTot, TReverb(MNbAbs), Tmean

      CHARACTER title*200
      CHARACTER CalName*12,ACOName*72, LabFreq*4, TxtZone*72
#ifdef OSX
      integer igwid,igheight  ! for use with axiscale
      integer iside,isize,ifont     ! passed to viewtext
      integer iupdown,isym    ! passed to etplot
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iupdown,isym    ! passed to etplot
#endif

C Initialisation
      YMAX=1.0
      YMIN=0.0
      RESMX = 0.
      RESMN = 10.

C Setup the graph dimension according to the max and min RT
      IF ((MAll.EQ.4).AND.(MethodCalc.GT.1)) THEN
      ELSE
        Call win3d(menuchw,15,15,5,5,igl,igr,igt,igb,igw,igwh)
        igw=igr-igl
        igwh=igb-igt
        do 20 I=1,MNbAbs
          if (TReverb(I).gt.RESMX) then
            RESMX=TReverb(I)
          endif
          if (TReverb(I).lt.RESMN) then
            RESMN=TReverb(I)
          endif
 20     continue


C Calculate plotting box locations.
        plxmin=0.
        plxmax=4000.
        YMAX=RESMX+0.5
        YMIN=0.
        igwid=igw
        igheight=igwh
        call axiscale(igwid,igheight,plxmin,plxmax,Ymin,Ymax,                 
     &                            xsc,ysc,sca,xadd,yadd)
        CALL linescale(igl,xadd,xsc,igb,yadd,ysc)

C Scale and draw monitoring plot.
        Call horaxis(plxmin,plxmax,igl,igr,igb,xadd,XSC,0,'Frequ.[Hz]')
        call vrtaxis(YMIN,YMAX,igl,igb,igt,Yadd,ysc,0,0,'RT  s')

C Print title. IPS is used to keep track of number of title lines.
        IPS=1
        write(title,'(20x,2a)') 'Reverberation time for space: ',TxtZone
        iside=IPS
        isize=0
        ifont=0
        CALL viewtext(title,iside,isize,ifont)
      ENDIF

C Write the label concerning the calculation mode used
      IF (MAll.LT.4) THEN
        IPS=IPS+1
        write(title,'(1x,2a)') 'Method: ',CalName(MethodCalc)(1:11)
        iside=IPS
        isize=0
        ifont=0
        CALL viewtext(title,iside,isize,ifont)
      ENDIF
      IF ((MAll.EQ.4).AND.(MethodCalc.EQ.1)) THEN
        IPS=IPS+1
        write(title,'(1x,a)') 'Method: All'
        iside=IPS
        isize=0
        ifont=0
        CALL viewtext(title,iside,isize,ifont)
      ENDIF

C Define for each calculation method which symbol to use on the curve
      if (MethodCalc.EQ.1) THEN
        Isymbol = 1
      elseif (MethodCalc.EQ.2) THEN
        Isymbol = 11
      elseif (MethodCalc.EQ.3) THEN
        Isymbol = 6
      endif


C Plot reverberation time curve
      iupdown=0
      isym=0
      CALL etplot(125.,TReverb(1),iupdown,isym)
      do 30 I=1,MNbABS
          FREQ=INT(100* EXP((I-1)/3.*LOG(2.)))
          iupdown=1
          isym=Isymbol
          CALL etplot(FREQ,TReverb(I),iupdown,isym)
 30   continue

C Label curves
      IF ((MAll.EQ.4).AND.(MethodCalc.EQ.1)) THEN
        IPS=IPS+1
        write(title,'(40x,3(a,8x))')'Sabine (x)','Eyring (+)',
     &                              'Millington (.)'
        iside=IPS
        isize=0
        ifont=0
        CALL viewtext(title,iside,isize,ifont)
      ENDIF

      RETURN
      END


