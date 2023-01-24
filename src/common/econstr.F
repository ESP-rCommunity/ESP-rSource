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

C Note: the routines make use of parameters from /usr/esru/include.
C Econstr.F provides the following facilities:
C  ECONST:  Reads zone construction file.  
C  EMKCON:  Creates zone construction file based on blocks
C           T1 T2 T3 T3ADD T4.
C  CONINF:  Provides a description of the constructions in a zone based
C           on common blocks.
C  ERTWIN:  reads all transparent wall properties and optical
C           control details from an annotated ascii file.
C  ERBIWIN: reads bi-directional optical data from measurements
C           file.


C ******************** ECONST 
C ECONST reads zone thermophysical data from a user-constructed data
C file as ASCII strings and with or without comments.  LCONS is the
C name of the file, ICOMP is the zone number, ITRC trace level, 
C ITRU output unit, IER error reading file.  Geometry data is taken
C from the current contents of common blocks G0 G2 G4 G6.

C Common block variables are:
C NE      - no of homogeneous layers in each construction. 
C NAIRG   - number of air gaps in each construction.
C IPAIRG  - position of each air gap counting from 'outside'.

C RAIRG   - overall resistance (radiation + convection) of each air gap.
C CON     - conductivity of each layer (air gaps set to zero).
C DEN     - density as above.
C SHT     - specific heat as above.
C THK     - thickness (m) of each layer.
C GVTR    - visible transmission for each window.
C EMISI   - emissivity of internal surfaces.
C EMISE   - emissivity of external surfaces.
C ABSI    - solar absorptivity of internal surfaces.
C ABSE    - solar absorptivity of external surfaces.

      SUBROUTINE ECONST(LCONS,IUNIT,ICOMP,ITRC,ITRU,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "espriou.h"
#include "esprdbfile.h"
#include "material.h"

      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)
      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)

      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/PRECT2/TMCT2(MCOM,MTMC,5),TMCA2(MCOM,MTMC,ME,5),
     &              TVTR2(MCOM,MTMC)

      COMMON/PRECT3/NTMC,NGLAZ(MTMC)
      common/PRECT4/TOPTIC(MCOM,MTMC)

      COMMON/TMCB1/IBCMT(MCOM,MTMC)
      COMMON/TMCB2/NBCTMC(MCOM,MTMC),IBCST(MCOM,MTMC),
     &             IBCFT(MCOM,MTMC),IBCSUR(MCOM,MTMC)
      COMMON/TMCB3/NBCTT(MCOM,MTMC),BACTPT(MCOM,MTMC)

      common/BSCTL/ITPREP(MCOM,MTMC),ICZN,IMDB(3),IGLZ(3)

C Version of construction file. If not specified set at 21 (2.1)
      integer izconstv,iztmcv
      common/znconstrv/izconstv(MCOM),iztmcv(MCOM)

      COMMON/VTHP02/IVKON(MCOM,MS,ME)
      COMMON/VTHP30/ILTHPS,ILTHPZ(MCOM)

      DIMENSION RVC(MS)

      CHARACTER OUTSTR*124,LCONS*72,outs248*248,WORD*20
      CHARACTER LUA*72,TOPTIC*12
      character outs*124,outs2*124
      character dstmp*24
      LOGICAL CLOSE,ILTHPS,ILTHPZ
      logical TFOUND,readtmc
      integer lsn  ! length of currentfile

C Thermal property checking values follow.
      DATA CONCH/250./,DENCH/8900./,SHTCH/4200./,THKCH/0.5/

      IER=0

C Open existing ASCII construction data file as unit IUNIT.
      CALL EFOPSEQ(IUNIT,LCONS,1,IER)
      IF(IER.LT.0)THEN
        write(outs,'(3a)') 'Constructions file ',LCONS(1:lnblnk(LCONS)),
     &    ' could not be opened.'
        call edisp(ITRU,outs)
        IER=1
        RETURN
      endif
      write(currentfile,'(a)') LCONS(1:lnblnk(LCONS))

C Attempt to read the first line and Isee if it is '*Constructions 2.1'
      CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'*Constructions',IER)
      if(OUTSTR(1:18).eq.'*Constructions 2.1')then
        izconstv(ICOMP)=21
C Read the *date stamp.
        CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'*date',IER)
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','*date',IER)
        if(WORD(1:5).eq.'*Date'.or.WORD(1:5).eq.'*date')then
          CALL EGETRM(OUTSTR,K,dstmp,'W','date stamp',IER)
          if(IER.ne.0) IER=0  ! missing date stamp not fatal
        endif
      else

C Rewind the file and read legacy format.
        REWIND(IUNIT,ERR=1001)
      endif

C Check if any of the surfaces are transparent.
      TFOUND=.FALSE.
      do IS=1,NZSUR(icomp)
        IF(SOTF(icomp,is)(1:4).NE.'OPAQ'.AND.
     &     SOTF(icomp,is)(1:3).NE.'CFC')TFOUND=.TRUE.
        IF(ITMCFL(ICOMP,IS).GT.0)TFOUND=.TRUE.
      enddo

C Read the number of layers and number of air gaps for each surface
C defined in the geometry file. If the constructions file is
C out of date the call to STRIPC can give a fatal error so check
C if the number of items found on a line is 2 and if not gracefully
C exit. For newer file format also read in the surface properties
C and the tmc index.
      if(izconstv(ICOMP).eq.21)then
        do I=1,NZSUR(icomp)
          CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'layers & gaps',IER)
          IF(IER.NE.0)goto 1001
          if(ND.LT.5)goto 1001
          K=0
          CALL EGETWI(OUTSTR,K,NE(I),1,ME,'F','layers',IER)
          CALL EGETWI(OUTSTR,K,NAIRG(I),0,MGP,'F','air gap',IER)
          CALL EGETWR(OUTSTR,K,EMISI(I),0.,1.,'W','emissivity in',IER)
          CALL EGETWR(OUTSTR,K,EMISE(I),0.,1.,'W','emissivity out',IER)
          CALL EGETWR(OUTSTR,K,ABSI(I),0.,1.,'W','absorptivity in',IER)
          CALL EGETWR(OUTSTR,K,ABSE(I),0.,1.,'W','absorptivity out',IER)
          CALL EGETWI(OUTSTR,K,itm,0,MTMC,'F','tmc index',IER)
          ITMCFL(icomp,i)=itm
          IF(itm.GT.0)TFOUND=.TRUE.
        enddo
        if(TFOUND)then     ! Signal subsequent to write embedded tmc.
          iztmcv(icomp)=21
          itw(icomp)=2
        endif
      else
        do I=1,NZSUR(icomp)
          CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'layers & gaps',IER)
          IF(IER.NE.0)goto 1001
          if(ND.NE.2)goto 1001
          K=0
          CALL EGETWI(OUTSTR,K,NE(I),1,ME,'F','layers',IER)
          CALL EGETWI(OUTSTR,K,NAIRG(I),0,MGP,'F','air gap',IER)
          IF(IER.NE.0)goto 1001
        enddo
      endif

C For each surface with air gaps read in the air gap position & U value.
      DO 10 I=1,NZSUR(icomp)
        IGPS=NAIRG(I)
        IF(IGPS.GT.0)THEN
          igl=IGPS*2
          CALL STRIPC(IUNIT,OUTSTR,igl,ND,1,'gap data',IER)
          IF(IER.NE.0)goto 1001
          K=0
          DO 165 J=1,IGPS

C Move across gap position and U value one item at a time.
            CALL EGETWI(OUTSTR,K,IPA,1,NE(I),'F','air gap position',IER)
            IPAIRG(I,J)=IPA
            CALL EGETWR(OUTSTR,K,RAI,-10.,10.,'W','air gap U value',IER)
            RAIRG(I,J)=RAI
            call eclose(RAIRG(I,J),0.0,0.0001,close)
            if(close)then
              write(outs,'(a,i3,a,i3,a,i3,a)')
     &          ' Air gap resistance too small in zone',icomp,
     &          ' surface',i,' gap ',j,' please increase!'
              call usrmsg(' ',outs,'W')
              IER=1
              CALL ERPFREE(IUNIT,ios)
              RETURN
            endif
  165     CONTINUE
        ENDIF
   10 CONTINUE

c Read layer thermophysical properties from 'outside' to inside.
      ILTHPZ(ICOMP)=.FALSE.
      DO 30 I=1,NZSUR(icomp)
        IELT=NE(I)
        DO 40 J=1,IELT
          CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'properties',IER)
          IF(IER.NE.0)RETURN
          IF(ND.EQ.4.OR.ND.EQ.8)THEN
            K=0

C If reading an air gap allow 0.0 properties.
            if(J.eq.IPAIRG(I,1).or.J.eq.IPAIRG(I,2).or.
     &         J.eq.IPAIRG(I,3).or.J.eq.IPAIRG(I,4).or.
     &         J.EQ.IPAIRG(I,5).or.J.eq.IPAIRG(I,6).or.
     &         J.EQ.IPAIRG(I,7).OR.J.EQ.IPAIRG(I,8))then
              CALL EGETWR(OUTSTR,K,CON(I,J),0.,CONCH,'W','conductivity',
     &          IER)
              CALL EGETWR(OUTSTR,K,DEN(I,J),0.0,DENCH,'W','density',IER)
              CALL EGETWR(OUTSTR,K,SHT(I,J),0.0,SHTCH,'W','spec ht',IER)
            else

C Flag up small or negative values.
              CALL EGETWR(OUTSTR,K,CON(I,J),0.,CONCH,'W','conductivity',
     &          IER)
              if(CON(I,J).lt.20.0)then
                CALL EGETWR(OUTSTR,K,DEN(I,J),0.1,DENCH,'W','density',
     &             IER)
              else
                CALL EGETWR(OUTSTR,K,DEN(I,J),0.1,DENCH*2.,'-',
     &              'metal density',IER)
              endif
              CALL EGETWR(OUTSTR,K,SHT(I,J),0.1,SHTCH,'W','spec ht',IER)
            endif
            THRMLI(ICOMP,I,J,1)=CON(I,J)
            THRMLI(ICOMP,I,J,2)=DEN(I,J)
            THRMLI(ICOMP,I,J,3)=SHT(I,J)
            CALL EGETWR(OUTSTR,K,THK(I,J),0.,THKCH,'W','thick (m)',IER)
            THRMLI(ICOMP,I,J,4)=THK(I,J)
            IF(ND.EQ.8)THEN

C Read the thermal conductivity linear dependence type.
              CALL EGETWI(OUTSTR,K,IVKON(ICOMP,I,J),0,3,
     &                   'W','dependence type',IER)
              IF(IVKON(ICOMP,I,J).NE.0)ILTHPZ(ICOMP)=.TRUE.

C Read the referance temperature for the thermal properties 
C given above.
              CALL EGETWR(OUTSTR,K,THRMLI(ICOMP,I,J,5),-20.,40.,
     &                    'W','ref_temp',IER)

C Read the linear temeprature dependency factor for thermal conductivity.
              CALL EGETWR(OUTSTR,K,THRMLI(ICOMP,I,J,6),-1.,1.,
     &                     'W','temp_factor',IER)

C Read the linear moisture content dependency factor for thermal
C conductivity.
              CALL EGETWR(OUTSTR,K,THRMLI(ICOMP,I,J,7),-1.,1.,
     &                     'W','H2O_factor',IER)
            ELSE
              IVKON(ICOMP,I,J)=0
              THRMLI(ICOMP,I,J,5)=0.0
              THRMLI(ICOMP,I,J,6)=0.0
              THRMLI(ICOMP,I,J,7)=0.0
            ENDIF
          ELSE
            write(outs248,'(4a,i2,2a)') 'In the file ',
     &        currentfile(1:lnblnk(currentfile)),
     &        ' when reading layer thermophysical properties expected',
     &        ' to find 4 or 8 items but got ',ND,' items in line: ',
     &        outstr(1:72)
            CALL EDISP248(iuout,outs248,92)
            outs=' incorrect number of thermophysical data'
            call usrmsg(' ',outs,'W')
            IER=1
            CALL ERPFREE(IUNIT,ios)
            RETURN
          endif
   40   CONTINUE
   30 CONTINUE
      IF(ILTHPZ(ICOMP))ILTHPS=.TRUE.

      if(izconstv(ICOMP).eq.21)then
        continue

c Read surface emissivity and solar absorptivity data.  This may take
C more than one line. Begin with inside face ir emissivity.
      else
        IF(NZSUR(icomp).GT.0) THEN
          CALL EGETWRA(IUNIT,RVC,NZSUR(icomp),0.0001,0.999,'W',
     &      'inside face emis',IER)
          DO KV=1,NZSUR(icomp)
            EMISI(KV)=RVC(KV)
          ENDDO
          IF(IER.NE.0) goto  1001

C Outside side face ir emissivity.
          CALL EGETWRA(IUNIT,RVC,NZSUR(icomp),0.0001,0.999,'W',
     &      'outside face emis',IER)
          DO KV=1,NZSUR(icomp)
            EMISE(KV)=RVC(KV)
          ENDDO
          IF(IER.NE.0) GOTO 1001

C Inside face solar absorption.
          CALL EGETWRA(IUNIT,RVC,NZSUR(icomp),0.0001,0.999,'W',
     &      'inside face abs',IER)
          DO KV=1,NZSUR(icomp)
            ABSI(KV)=RVC(KV)
          ENDDO
          IF(IER.NE.0) GOTO 1001

C Outside face solar absorption.
          CALL EGETWRA(IUNIT,RVC,NZSUR(icomp),0.0001,0.999,'W',
     &      'outside face abs',IER)
          DO KV=1,NZSUR(icomp)
            ABSE(KV)=RVC(KV)
          ENDDO
        ENDIF
        IF(IER.NE.0) GOTO 1001
      endif

C If there tmcs in the zone attempt to read optical data using
C code derived from ERTWIN.
      readtmc=.false.
      if(TFOUND.and.izconstv(ICOMP).eq.21)then
        CALL STRIPC(IUNIT,OUTSTR,99,ND,0,'*tmc',IER)
        if(IER.eq.2) then
          IER=0
          CALL ERPFREE(IUNIT,ISTAT)
          IF(ITRC.GE.1)CALL CONINF(ICOMP,0,ITRU)
          RETURN
        endif
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','*tmc',IER)
        if(WORD(1:4).eq.'*tmc')then
          CALL EGETW(OUTSTR,K,WORD,'W','YES',IER)
          if(WORD(1:3).eq.'YES')then
            readtmc=.true.
          endif
        else
          continue
        endif
        if(readtmc)then
          NTMC=0
          DO I=1,NZSUR(icomp)
            icn1=izstocn(icomp,i)
            if(icn1.ne.0)then
              IF(ITMCFL(ICOMP,I).NE.0.AND.
     &          (SOTF(icomp,i)(1:4).EQ.'OPAQ'))THEN
                WRITE(OUTS,'(5a)')'The TMC file ',LUA(1:lnblnk(LUA)),
     &          ' not sure if ',SNAME(icomp,i),' is transparent.'
                call edisp(itru,outs)
                call edisp(itru,' Check your zone files.')
              ENDIF
            else
              WRITE(OUTS,'(a,i3,a,i2,a)')
     &         'While rebulding optics found surface ',i,' in zone ',
     &         icomp,' does not exist in master connections list.'
              call edisp(itru,outs)
            endif
            IF(ITMCFL(ICOMP,I).GT.NTMC)NTMC=ITMCFL(ICOMP,I)
          enddo
          IF(NTMC.EQ.0.OR.NTMC.GT.MTMC)THEN
            CALL EDISP(ITRU,' No. of transparent types out of range.')
            GOTO 1001
          ENDIF

          DO 20 I=1,NTMC

c Establish the number of glazing layers and check that each item in
C the list points to an existing set of properties.
            CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'glazing layers',IER)
            IF(IER.NE.0)GOTO 1001
            K=0
            CALL EGETWI(OUTSTR,K,NTGLAZ,1,ME,'W','glazing elem',IER)
            if(ND.eq.2)then
              CALL EGETW(OUTSTR,K,WORD,'W','matching optics',IFLAG)
              TOPTIC(ICOMP,I)=WORD(1:12)
            endif
            IERR=0
            do J=1,NZSUR(icomp)
              lnslmlcn=lnblnk(SMLCN(icomp,j))
              IF(ITMCFL(ICOMP,J).EQ.I.AND.NTGLAZ.NE.NE(J))IERR=1
              IF(IERR.EQ.1)THEN
                write(outs,'(6a,1x,a)')
     &          ' Mismatched TMC & mlc layers: ',
     &          SNAME(icomp,j)(1:lnblnk(SNAME(icomp,j))),' in ',
     &          zname(ICOMP)(1:lnzname(ICOMP)),' composed of ',
     &          SMLCN(icomp,j)(1:lnslmlcn),TOPTIC(ICOMP,I)
                call edisp(iuout,outs)
                goto 1001
              ENDIF
            enddo
            NGLAZ(I)=NTGLAZ

C Read transmissions (T) for the transparent multi-layer construction.
            CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'transmission',IER)
            IF(IER.NE.0)GOTO 1001
            K=0
            CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 0',IER)
            TMCT(ICOMP,I,1)=VAL
            CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 40',IER)
            TMCT(ICOMP,I,2)=VAL
            CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 55',IER)
            TMCT(ICOMP,I,3)=VAL
            CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 70',IER)
            TMCT(ICOMP,I,4)=VAL
            CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 80',IER)
            TMCT(ICOMP,I,5)=VAL
            IF(ND.EQ.6)THEN
              CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','vis transmis',IER)
              TVTR(ICOMP,I)=VAL
            ELSE
              TVTR(ICOMP,I)=0.85
            ENDIF

c Read absorptions (A) for each element.
            do J=1,NTGLAZ
              IRVC=5
              CALL EGETWRA(IUNIT,RVC,IRVC,0.,0.999,'W','absorption',IER)
              do J5=1,5
                TMCA(ICOMP,I,J,J5)=RVC(J5)
              enddo
            enddo

C Calculate reflectance.
            SUM=TMCT(ICOMP,I,3)
            do K=1,NTGLAZ
              SUM=SUM+TMCA(ICOMP,I,K,3)
            enddo
            IF(SUM.LT.0..OR.SUM.GT.1.)then
              write(outs,'(3a,i2,a,f6.3)')' In ',
     &          zname(ICOMP)(1:lnzname(ICOMP)),' tmc ',
     &          i,': the optical reflectance is ',SUM
              CALL EDISP(ITRU,outs)
              CALL EDISP(ITRU,' Reading of zone optics terminated.')
              goto 1001
            endif
            TMCREF(ICOMP,I)=1.-SUM

C Read in optical control information for this TMC type. The logic
C differs for current and legacy tmc controls.
            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'optical control',IER)
            IF(IER.NE.0)GOTO 1001
            K=0
            CALL EGETWI(OUTSTR,K,IBCMT(ICOMP,I),-99,1,'W','control',IER)
            if(IBCMT(ICOMP,I).EQ.0)then
              GOTO 20
            elseif(IBCMT(ICOMP,I).lt.0)then
              IF(ITRC.GT.0)CALL EDISP(ITRU,
     &          ' Points to an optical control loop.')
              NBCTMC(ICOMP,I)=1; IBCSUR(ICOMP,I)=0
              IBCST(ICOMP,I)=0; IBCFT(ICOMP,I)=24
              NBCTT(ICOMP,I)=0; BACTPT(ICOMP,I)=0.0

C Read replacement transmissions (T) for the transparent multi-layer construction
C and replacement visible transmittance.
              CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'repl transmission',IER)
              IF(IER.NE.0)GOTO 1001
              K=0
              CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 0',IER)
              TMCT2(ICOMP,I,1)=VAL
              CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 40',IER)
              TMCT2(ICOMP,I,2)=VAL
              CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 55',IER)
              TMCT2(ICOMP,I,3)=VAL
              CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 70',IER)
              TMCT2(ICOMP,I,4)=VAL
              CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 80',IER)
              TMCT2(ICOMP,I,5)=VAL
              IF(ND.EQ.6)THEN
                CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','vis trans',IER)
                TVTR2(ICOMP,I)=VAL
              ELSE
                TVTR2(ICOMP,I)=0.85
              ENDIF


C Read replacement absorptance for each substrate.
              do J=1,NTGLAZ
                CALL EGETWRA(IUNIT,RVC,5,0.,0.999,'W','alt abs',IER)
                do J5=1,5
                  TMCA2(ICOMP,I,J,J5)=RVC(J5)
                enddo
              enddo

C Read dummy index for thermophysical property replacement.
              CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'alt thermo prop',IER)
              IF(IER.NE.0)RETURN
              K=0
              CALL EGETWI(OUTSTR,K,IMLC,0,1,'-','alt pr',IER)
              ITPREP(ICOMP,I)=IMLC
              GOTO 20    ! jump past in-built control logic
            endif

C Legacy tmc control logic.
            CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'TMC control',IER)
            IF(IER.NE.0)GOTO 1001
            K=0
            CALL EGETWI(OUTSTR,K,N1,0,1,'W','control per',IER)
            NBCTMC(ICOMP,I)=N1
            CALL EGETWI(OUTSTR,K,N2,0,MS,'W','control sen',IER)
            IBCSUR(ICOMP,I)=N2

C For each period.
            DO 21 KK=1,N1
              CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'period range',IER)
              IF(IER.NE.0)GOTO 1001
              K=0
              CALL EGETWI(OUTSTR,K,IS,0,24,'W','start',IER)
              CALL EGETWI(OUTSTR,K,IF,IS,24,'W','end',IER)
              IBCST(ICOMP,I)=IS
              IBCFT(ICOMP,I)=IF

              CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'sensor data',IER)
              IF(IER.NE.0)GOTO 1001
              K=0
              CALL EGETWI(OUTSTR,K,NCT,-99,4,'W','sensor type',IER)
              NBCTT(ICOMP,I)=NCT
              CALL EGETWR(OUTSTR,K,ACTP,0.,0.,'-','actuation pt',IER)
              BACTPT(ICOMP,I)=ACTP

C Read replacement transmissions (T) for the transparent multi-layer construction
C and replacement visible transmittance.
              CALL STRIPC(IUNIT,OUTSTR,99,ND,1,'repl transmission',IER)
              IF(IER.NE.0)GOTO 1001
              K=0
              CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 0',IER)
              TMCT2(ICOMP,I,1)=VAL
              CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 40',IER)
              TMCT2(ICOMP,I,2)=VAL
              CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 55',IER)
              TMCT2(ICOMP,I,3)=VAL
              CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 70',IER)
              TMCT2(ICOMP,I,4)=VAL
              CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 80',IER)
              TMCT2(ICOMP,I,5)=VAL
              IF(ND.EQ.6)THEN
                CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','vis transm',IER)
                TVTR2(ICOMP,I)=VAL
              ELSE
                TVTR2(ICOMP,I)=0.85
              ENDIF

C Read replacement absorptance for each substrate.
              do J=1,NTGLAZ
                CALL EGETWRA(IUNIT,RVC,5,0.,0.999,'W','alt abs',IER)
                do J5=1,5
                  TMCA2(ICOMP,I,J,J5)=RVC(J5)
                enddo
              enddo

C Now read in index for thermophysical property replacement
C In this version, index will be zero; if not give error
              CALL STRIPC(IUNIT,OUTSTR,0,ND,1,'alt thermo prop',IER)
              IF(IER.NE.0)RETURN
              K=0
              CALL EGETWI(OUTSTR,K,IMLC,0,1,'-','alt pr',IER)
              ITPREP(ICOMP,I)=IMLC
   21       CONTINUE
   20     CONTINUE
        endif
        if(izconstv(ICOMP).eq.21)then
          itw(icomp)=2             ! signal embedded tmc file
          LTWIN(icomp)='UNKNOWN'   ! clear tmc file name
        endif
      endif

c Close thermal properties file.
      CALL ERPFREE(IUNIT,ISTAT)

c Trace output ?
      IF(ITRC.GE.1)CALL CONINF(ICOMP,0,ITRU)
      RETURN

 1001 write(outs,'(3a)') 'ECONST: Read/conversion error in...',
     &  OUTSTR(1:50),'...'
      lsn=MIN0(lnblnk(currentfile),110)
      write(outs2,'(2a)') 'in: ',currentfile(1:lsn)
      call edisp(itru,outs)
      call edisp(itru,outs2)
      IER=1
      CALL ERPFREE(IUNIT,ISTAT)
      RETURN

      END

C ************* EMKCON 
C Generic routine to write a construction file based on information cur-
C rently held in common blocks T1 T2 T4.
C CONFIL is the name of the file to be written to (any existing file
C by this name is overwritten).
      SUBROUTINE EMKCON(CONFIL,IFILT,ICOMP,QUIET,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

C Parameters
      character CONFIL*72  ! zone constructions file
      integer IFILT        ! file unit for zone constructions file
      integer ICOMP        ! index of the zone
      logical QUIET        ! true acts silently
      integer IER          ! IER=0 OK, IER=1 problem

      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/VTHP02/IVKON(MCOM,MS,ME)

      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)
      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)

      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/PRECT2/TMCT2(MCOM,MTMC,5),TMCA2(MCOM,MTMC,ME,5),
     &              TVTR2(MCOM,MTMC)

      COMMON/PRECT3/NTMC,NGLAZ(MTMC)
      common/PRECT4/TOPTIC(MCOM,MTMC)

      COMMON/TMCB1/IBCMT(MCOM,MTMC)
      COMMON/TMCB2/NBCTMC(MCOM,MTMC),IBCST(MCOM,MTMC),
     &             IBCFT(MCOM,MTMC),IBCSUR(MCOM,MTMC)
      COMMON/TMCB3/NBCTT(MCOM,MTMC),BACTPT(MCOM,MTMC)

C Version of construction file. If not specified set at 21 (2.1)
      integer izconstv,iztmcv
      common/znconstrv/izconstv(MCOM),iztmcv(MCOM)

      character outs*124,louts*248,lkouts*1000
      character dstmp*24
      character tokens*156,comment*76,aligned_str*156
      logical TFOUND

      CHARACTER TOPTIC*12

      IER=0

C Open any existing file by this name (CONFIL), 
C (ask user for confirmation to over-write) or create a new file.
C If in quiet mode then file should exist.
      IF(QUIET) THEN
        CALL EFOPSEQ(IFILT,CONFIL,1,IER)
      ELSE
        CALL EFOPSEQ(IFILT,CONFIL,4,IER)
      ENDIF
      IF(IER.LT.0)THEN
        IER=1
        RETURN
      ENDIF

C Write out the construction file data.
C Begin with header and version and date.
      if(izconstv(ICOMP).eq.21)then
        write(comment,'(2a)') 'thermophysical properties of ',
     &    zname(ICOMP)(1:lnzname(ICOMP))
        write(tokens,'(a)') '*Constructions 2.1'
        call align_comment(48,tokens,comment,aligned_str)
        write(IFILT,'(a)') aligned_str(1:lnblnk(aligned_str))
        call dstamp(dstmp)
        write(comment,'(a)') 'latest file modification'
        write(tokens,'(2a)') '*date ',dstmp
        call align_comment(48,tokens,comment,aligned_str)
        write(IFILT,'(a)') aligned_str(1:lnblnk(aligned_str))
      else
        WRITE(IFILT,'(2a)',IOSTAT=IOS,ERR=146) 
     &  '# thermophysical properties of ',zname(ICOMP)(1:lnzname(ICOMP))
      endif
     
      if(izconstv(ICOMP).eq.21)then
        WRITE(IFILT,'(2a)',IOSTAT=IOS,ERR=146) 
     &  '# layers|air | emissivity |absorptivity| tmc | ',
     &  'surface|surface     |construction '
        WRITE(IFILT,'(2a)',IOSTAT=IOS,ERR=146) 
     &  '#       |gaps|inside other|inside other|index| ',
     &  'index  |name        |name '
      else 
        WRITE(IFILT,'(2a)',IOSTAT=IOS,ERR=146) 
     &  '# no. of |no. of  |              surface|surface     ',
     &  '|construction '
        WRITE(IFILT,'(2a)',IOSTAT=IOS,ERR=146) 
     &  '# layers |air gaps|              index  |name        ',
     &  '|name '
      endif
      DO 1180 I=1,NZSUR(icomp)
        lnslmlcn=lnblnk(SMLCN(icomp,i))
        lns=lnblnk(SNAME(icomp,i))
        if(izconstv(ICOMP).eq.21)then
          write(comment,'(i3,4a)') i,'  ',SNAME(icomp,i),' ',
     &      SMLCN(icomp,i)(1:lnslmlcn)
          write(tokens,'(2i6,4F7.2,i4)') NE(I),NAIRG(I),
     &      EMISI(I),EMISE(I),ABSI(I),ABSE(I),ITMCFL(icomp,i)
          call align_comment(48,tokens,comment,aligned_str)
          write(IFILT,'(a)') aligned_str(1:lnblnk(aligned_str))
        else
          WRITE(IFILT,5632,IOSTAT=IOS,ERR=146)NE(I),NAIRG(I),I,
     &      SNAME(icomp,i),SMLCN(icomp,i)(1:lnslmlcn)
5632      FORMAT(2I6,'                        # ',I3,'  ',A,' ',A)
        endif

C Trap in case of mlc not being found.
        if(NE(I).eq.0)then
          write(outs,'(A,A)') ' FAILURE: writing out ',SNAME(icomp,i)
          call usrmsg(outs,' Please check!....','W')
          ier=1
          goto 99
        endif
1180  CONTINUE

C Write air gap details. 
      WRITE(IFILT,'(a)',IOSTAT=IOS,ERR=146) 
     & '# air gap layer positions and restances'
      icount=0
      DO 1190 I = 1,NZSUR(icomp)
        IF (NAIRG(I).GT.0)THEN
          icount=icount+1
          write(tokens,'(10(I3,F7.3))') 
     &      (IPAIRG(I,J),RAIRG(I,J),J=1,NAIRG(I))
          lns=lnblnk(SNAME(icomp,i))
          if(icount.eq.1)then
            write(comment,'(2a)') 'for ',SNAME(icomp,i)(1:lns)
          else
            write(comment,'(2a)') 'and for ',SNAME(icomp,i)(1:lns)
          endif
          call align_comment(48,tokens,comment,aligned_str)
          write(IFILT,'(a)') aligned_str(1:lnblnk(aligned_str))
        ENDIF
1190  CONTINUE
                                                                        
C Write layer thermophysical properties.
      WRITE(IFILT,'(a)',IOSTAT=IOS,ERR=146) 
     & '# layer thermophysical properties'
      WRITE(IFILT,'(2A)',IOSTAT=IOS,ERR=146)
     & '# conduc-  | density |specific | thick- |dpnd|',
     & 'ref.| temp. |moisture|surface|layer'
      WRITE(IFILT,'(2A)',IOSTAT=IOS,ERR=146)
     & '# tivity   |         |heat     |ness(m) |type|',
     & 'temp|factor | factor |       |  '
      DO 1220 I = 1,NZSUR(icomp)
        imatc=0
        call matchmlcdesc(SMLCN(icomp,i),imatc)
        DO 1210 J = 1,NE(I)
          matarrayindex=IPRMAT(imatc,J)   ! which array index
          if(matarrayindex.ge.0)then

C And if matarrayindex is zero then establish NAM.
            if(matarrayindex.eq.0)then
              if(J.EQ.1)THEN
                write(comment,'(i3,i4,a)') I,J,' AIR'
              else
                write(comment,'(i7,a)') J,' AIR'
              endif
            else
              if(J.EQ.1)THEN
                write(comment,'(i3,i4,2a)') I,J,' ',
     &            matname(matarrayindex)(1:32)
              else
                write(comment,'(i7,2a)') J,' ',
     &            matname(matarrayindex)(1:32)
              endif
            endif
          endif
          WRITE(tokens,5662,IOSTAT=IOS,ERR=146)CON(I,J),DEN(I,J),
     &      SHT(I,J),THK(I,J),IVKON(ICOMP,I,J),
     &      (THRMLI(ICOMP,I,J,K),K=5,7)
5662      FORMAT(1X,F10.4,F10.1,F10.1,F8.4,3X,I1,F7.2,F8.5,F8.5)
          call align_comment(48,tokens,comment,aligned_str)
          write(IFILT,'(a)') aligned_str(1:lnblnk(aligned_str))
1210    CONTINUE
1220  CONTINUE

C Signal whether there is optical data.
      if(izconstv(ICOMP).eq.21)then
        TFOUND=.FALSE.
        do IS=1,NZSUR(icomp)
          IF(SOTF(icomp,is)(1:4).NE.'OPAQ'.AND.
     &       SOTF(icomp,is)(1:3).NE.'CFC')TFOUND=.TRUE.
          IF(ITMCFL(ICOMP,IS).GT.0)TFOUND=.TRUE.
        enddo
        if(TFOUND)then
          write(tokens,'(a)')  '*tmc YES'
          write(comment,'(a)') 'optical properties:'
          call align_comment(48,tokens,comment,aligned_str)
          write(IFILT,'(a)') aligned_str(1:lnblnk(aligned_str))
          itw(icomp)=2             ! signal embedded tmc file
          LTWIN(icomp)='UNKNOWN'   ! clear tmc file name

C Follows logic in edcon.F for generating a tmc file.
          DO 100 I=1,NTMC
            write(tokens,'(I4,2A)') NGLAZ(I),'  ',TOPTIC(ICOMP,I) 
            write(comment,'(a,i2)') 'layers in tmc type',I
            call align_comment(48,tokens,comment,aligned_str)
            write(IFILT,'(a)') aligned_str(1:lnblnk(aligned_str))

            write(tokens,'(6F7.3)') (TMCT(ICOMP,I,J),J=1,5),
     &        TVTR(ICOMP,I)
            write(comment,'(a)') 'transmission @ 5 angles & visible tr.'
            call align_comment(48,tokens,comment,aligned_str)
            write(IFILT,'(a)') aligned_str(1:lnblnk(aligned_str))

            DO J=1,NGLAZ(I)
              write(tokens,'(5F7.3)') (TMCA(ICOMP,I,J,K),K=1,5)
              write(comment,'(a)')'for each layer absorption @ 5 angles'
              if(j.eq.1)then
                call align_comment(48,tokens,comment,aligned_str)
                write(IFILT,'(a)') aligned_str(1:lnblnk(aligned_str))
              else
                write(IFILT,'(a)') tokens(1:lnblnk(tokens))
              endif
            enddo
            write(tokens,'(i4)') IBCMT(ICOMP,I)
            if(IBCMT(ICOMP,I).eq.1)then
              write(comment,'(a)') 'optical control flag'
            elseif(IBCMT(ICOMP,I).eq.0)then
              write(comment,'(a)') 'no alternative optics'
            elseif(IBCMT(ICOMP,I).lt.0)then
              write(comment,'(a)') 'links to an optical control loop'
            endif
            call align_comment(48,tokens,comment,aligned_str)
            write(IFILT,'(a)') aligned_str(1:lnblnk(aligned_str))
            IF(IBCMT(ICOMP,I).EQ.0)GOTO 100
            if(IBCMT(ICOMP,I).LT.0)then  ! only alternative optical set
              write(tokens,'(6F7.3)') 
     &          (TMCT2(ICOMP,I,M),M=1,5),TVTR2(ICOMP,I)
              write(comment,'(a)') 
     &      'alt solar & vis trans followed by absorp for each layer'
              call align_comment(48,tokens,comment,aligned_str)
              write(IFILT,'(a)') aligned_str(1:lnblnk(aligned_str))
              DO J=1,NGLAZ(I)
                WRITE(IFILT,'(5F8.3)',IOSTAT=ISTAT,ERR=98)
     &                  (TMCA2(ICOMP,I,J,M),M=1,5)
              ENDDO
              ITPREP=0
              WRITE(IFILT,'(I4)',IOSTAT=ISTAT,ERR=98)ITPREP
              goto 100   ! jump for next tmc
            endif

            write(tokens,'(2i4)') NBCTMC(ICOMP,I),IBCSUR(ICOMP,I)
            write(comment,'(a)')'number of control periods & sensor loc'
            call align_comment(48,tokens,comment,aligned_str)
            write(IFILT,'(a)') aligned_str(1:lnblnk(aligned_str))

            WRITE(IFILT,'(A)',IOSTAT=ISTAT,ERR=98)
     &         '# Replacement properties for each control period'
            DO 121 K=1,NBCTMC(ICOMP,I)
              write(tokens,'(2i4)') IBCST(ICOMP,I),IBCFT(ICOMP,I)
              write(comment,'(a)') 'period start and end'
              call align_comment(48,tokens,comment,aligned_str)
              write(IFILT,'(a)') aligned_str(1:lnblnk(aligned_str))

              if(NBCTT(ICOMP,I).eq.0)then
                write(tokens,'(I4,F6.1)') NBCTT(ICOMP,I),
     &            BACTPT(ICOMP,I)
                write(comment,'(a)') 
     &          'sensing total radiation @ actuation point'
                call align_comment(48,tokens,comment,aligned_str)
                write(IFILT,'(a)') aligned_str(1:lnblnk(aligned_str))
              elseif(NBCTT(ICOMP,I).eq.1)then
                write(tokens,'(I4,F6.1)') NBCTT(ICOMP,I),
     &            BACTPT(ICOMP,I)
                write(comment,'(a)') 
     &            'sensing ambient temperature @ actuation point'
                call align_comment(48,tokens,comment,aligned_str)
                write(IFILT,'(a)') aligned_str(1:lnblnk(aligned_str))
              elseif(NBCTT(ICOMP,I).eq.2)then
                write(tokens,'(I4,F6.1)') NBCTT(ICOMP,I),
     &            BACTPT(ICOMP,I)
                write(comment,'(a)') 
     &            'sensing zone temperature @ actuation point'
                call align_comment(48,tokens,comment,aligned_str)
                write(IFILT,'(a)') aligned_str(1:lnblnk(aligned_str))
              elseif(NBCTT(ICOMP,I).eq.3)then
                write(tokens,'(I4,F6.1)') NBCTT(ICOMP,I),
     &            BACTPT(ICOMP,I)
                write(comment,'(a)') 
     &            'sensing daylight coeff. @ actuation point'
                call align_comment(48,tokens,comment,aligned_str)
                write(IFILT,'(a)') aligned_str(1:lnblnk(aligned_str))
              elseif(NBCTT(ICOMP,I).eq.4)then
                write(tokens,'(I4,F6.1)') NBCTT(ICOMP,I),
     &            BACTPT(ICOMP,I)
                write(comment,'(a)') 
     &            'sensing lightswitch @ actuation point'
                call align_comment(48,tokens,comment,aligned_str)
                write(IFILT,'(a)') aligned_str(1:lnblnk(aligned_str))
              elseif(NBCTT(ICOMP,I).eq.-99)then
                write(tokens,'(I4,F6.1)') NBCTT(ICOMP,I),
     &            BACTPT(ICOMP,I)
                write(comment,'(a)') 
     &            'sensing time @ actuation point'
                call align_comment(48,tokens,comment,aligned_str)
                write(IFILT,'(a)') aligned_str(1:lnblnk(aligned_str))
              endif

              write(tokens,'(6F7.3)') 
     &          (TMCT2(ICOMP,I,M),M=1,5),TVTR2(ICOMP,I)
              write(comment,'(a)') 
     &      'alt solar & vis trans followed by absorp for each layer'
              call align_comment(48,tokens,comment,aligned_str)
              write(IFILT,'(a)') aligned_str(1:lnblnk(aligned_str))
              DO J=1,NGLAZ(I)
                WRITE(IFILT,'(5F8.3)',IOSTAT=ISTAT,ERR=98)
     &                  (TMCA2(ICOMP,I,J,M),M=1,5)
              ENDDO
              ITPREP=0
              WRITE(IFILT,'(I4)',IOSTAT=ISTAT,ERR=98)ITPREP
  121       CONTINUE
  100     CONTINUE

        else
          write(tokens,'(a)')  '*tmc NONE'
          write(comment,'(a)') 'all surfaces are opaque'
          call align_comment(48,tokens,comment,aligned_str)
          write(IFILT,'(a)') aligned_str(1:lnblnk(aligned_str))
        endif

C Write out surface properties as one or more lines of packed
C strings. Code should be good for any number of surfaces. Use
C arlist2 because not so many digits of accuracy are needed.
      else
        WRITE(IFILT,'(a)',IOSTAT=IOS,ERR=146)
     &    '# for each surface: inside face emissivity'
        itrunc=1
        ipos=1
        do while (itrunc.ne.0)
          call arlist3(ipos,nzsur(icomp),EMISI,MS,'C',lkouts,loutln,
     &      itrunc)
          write(ifilt,'(1x,a)',IOSTAT=ios,ERR=146) lkouts(1:loutln)
          ipos=itrunc+1
        end do

        WRITE(IFILT,'(a)',IOSTAT=IOS,ERR=146)
     &    '# for each surface: outside face emissivity'
        itrunc=1
        ipos=1
        do while (itrunc.ne.0)
         call arlist3(ipos,nzsur(icomp),EMISE,MS,'C',lkouts,loutln,
     &     itrunc)
         write(ifilt,'(1x,a)',IOSTAT=ios,ERR=146) lkouts(1:loutln)
         ipos=itrunc+1
        end do

        WRITE(IFILT,'(a)',IOSTAT=IOS,ERR=146)
     &    '# for each surface: inside face solar absorptivity'
        itrunc=1
        ipos=1
        do while (itrunc.ne.0)
          call arlist3(ipos,nzsur(icomp),ABSI,MS,'C',lkouts,loutln,
     &      itrunc)
          write(ifilt,'(1x,a)',IOSTAT=ios,ERR=146) lkouts(1:loutln)
          ipos=itrunc+1
        end do

        WRITE(IFILT,'(a)',IOSTAT=IOS,ERR=146)
     &    '# for each surface: outside face solar absorptivity'
        itrunc=1
        ipos=1
        do while (itrunc.ne.0)
          call arlist3(ipos,nzsur(icomp),ABSE,MS,'C',lkouts,loutln,
     &      itrunc)
          write(ifilt,'(1x,a)',IOSTAT=ios,ERR=146) lkouts(1:loutln)
          ipos=itrunc+1
        end do
      endif

   99 CALL ERPFREE(IFILT,ISTAT)
      RETURN

C Error messages.
  146 if(IOS.eq.2)then
        CALL USRMSG(' No permission to write ',CONFIL,'W')
      else
        CALL USRMSG(' File write error in ',CONFIL,'W')
      endif
      IER=1
      GOTO 99

C If in quiet mode: the file should exist => error if we are here.
   98 IF (IER.EQ.-301) THEN
        CALL USRMSG(' No permission to write ',CONFIL,'W')
      ELSE
        CALL USRMSG(' File write error in ',CONFIL,'W')
      ENDIF
      IER=1
      goto 99

      END


C ****************** CONINF 
C CONINF provides a description of the constructions in a zone based
C on common blocks. If isur=0 then all, otherwise for one surface. 
      SUBROUTINE CONINF(ICOMP,ISUR,ITRU)
      use CFC_Module, ONLY : cfcver, ITMCFCDB, cfcname, cfcitmindex
#include "building.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      COMMON/T2/CON(MS,ME),DEN(MS,ME),SHT(MS,ME),THK(MS,ME)

      COMMON/T4/EMISI(MS),EMISE(MS),ABSI(MS),ABSE(MS)
      
      integer matarrayindex ! the indes within matdatarray
      logical closemat1,closemat2

      integer cfcarrayindex ! the indes within cfcdataarray
      logical closecfc1,closecfc2

      dimension RT(MS),OPT(MS),PNAM(MS,ME),iprtext(MS,ME),imatch(MS)
      CHARACTER OUTS*124,sn*12,SOPT*12,OPT*12,PNAM*72,NAM*72
      character iprtext*4,GDESCR*36

C Use material arrays if available. This also assumes that constructions will
C have been filled after the model cfg file was scanned.
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)
      if(closemat1.or.closemat2)then
        continue
      else
        call usrmsg('CONINF: The materials arrays are incomplete so',
     &    'this report is not available.','W')
        return
      endif

C Use CFC layer arrays if available. This also assumes that constructions will
C have been filled after the model cfg file was scanned.
      call eclose(cfcver,1.1,0.01,closecfc1)
      call eclose(cfcver,1.2,0.01,closecfc2)
      if(closecfc1.or.closecfc2)then
        continue
      else
        call usrmsg('CONINF: The CFC layer arrays are incomplete so',
     &    'this report is not available.','W')
        return
      endif

C Heading if all surfaces.
      if(isur.eq.0)then
        CALL EDISP(ITRU,' ')
        WRITE(OUTS,9996)zname(ICOMP)(1:lnzname(ICOMP)),ICOMP
 9996   FORMAT(' Zone construction details for ',A,' (',I2,')')
        CALL EDISP(ITRU,OUTS)
      endif

C Reporting.
      CALL EDISP(ITRU,' ')
      write(outs,'(2a)') 'Surface|Mat|Thick  |Conduc-|',
     &                   'Density |Specif|IR  |Solr| Description'
      CALL EDISP(ITRU,outs)
      write(outs,'(2a)') 'layer  |db | (mm)  |tivity |',
     &                   '        |heat  |emis|abs |'
      CALL EDISP(ITRU,outs)
      DO 9994 I=1,NZSUR(icomp)

C Jump if not all requied and not the focus surface.
        if(isur.ne.0.and.isur.ne.i)goto 9994
        sn=SNAME(icomp,i)

C Find matching MLC index in ii and assign to imatch array.
        imatch(i)=0
        RT(I)=0.
        call matchmlcdesc(SMLCN(icomp,i),ii)
        imatch(i)=ii          
        if(imatch(i).eq.0) then
          call edisp(iuout,'Warning: no matching MLC defined!')
          OPT(i)='UNKNOWN'
          NELT=NE(I)
          DO 155, IL=1,NELT
            PNAM(i,IL)='user defined'
            iprtext(i,IL)='  - '
  155     CONTINUE

C Get the air gap contribution to U value for reporting.
          IF(NAIRG(I).GT.0)THEN
            DO 9990 J=1,NAIRG(I)
              RT(I)=RT(I)+RAIRG(I,J)
 9990       CONTINUE
          ENDIF
        else

C Set local optical name from material.h commons.
          WRITE(OPT(i),'(A)') mlcoptical(imatch(i))(1:12)
          NELT=NE(I)

C For each layer recover the name of material. For non-airgap layers
C add resistance for layer (using data from constructions file)
          DO 15, IL=1,NELT
C...........If CFC2, get details from CFClayers db
            IF(SOTF(icomp,i)(1:4).EQ.'CFC2') THEN
              cfcarrayindex=ITMCFCDB(imatch(i),IL)
              if(cfcarrayindex.gt.0)then
                write(NAM,'(a)') cfcname(cfcarrayindex)(1:32)
                write(iprtext(i,IL),'(i4)') cfcitmindex(cfcarrayindex)
                PNAM(i,IL)=NAM
              endif
            ELSE
              matarrayindex=IPRMAT(imatch(i),IL)   ! which array index
              if(matarrayindex.ge.0)then

C And if matarrayindex is zero then establish NAM.
                if(matarrayindex.eq.0)then
                  NAM='AIR'
                else
                  write(NAM,'(a)') matname(matarrayindex)(1:32)
                endif
              endif
              IF(IPR(imatch(i),IL).EQ.0)THEN

C Find matching air gap layer and resistance.
                IF(NAIRG(I).GT.0)THEN
                  DO 90 J=1,NAIRG(I)
                    if(IPAIRG(I,J).eq.IL)then
                      WRITE(PNAM(i,IL),'(a,f6.3,a)')'air gap (R=',
     &                  RAIRG(I,J),')'
                      RT(I)=RT(I)+RAIRG(I,J)
                    endif
 90               CONTINUE
                ENDIF
              ELSE
                PNAM(i,IL)=NAM
                RT(I)=RT(I)+THK(I,IL)/CON(I,IL)
              ENDIF
              write(iprtext(i,IL),'(i4)') IPR(imatch(i),IL)
            ENDIF
  15      CONTINUE
        endif
 9994 CONTINUE

      DO 9986 I=1,NZSUR(icomp)

C Jump if not all requied and not the focus surface.
        if(isur.ne.0.and.isur.ne.i)goto 9986
        sn=SNAME(icomp,i)
        lsml=lnblnk(SMLCN(icomp,i))
        if(OPT(i)(1:4).eq.'OPAQ')then
          write(outs,'(4a)') sn(1:lnblnk(sn)),' is composed of ',
     &      SMLCN(icomp,i)(1:lsml),' and is opaque:'
        elseif(OPT(i)(1:11).eq.'USE GSLedit'.or.
     &         OPT(i)(1:3).eq.'N/A')then
          write(outs,'(4a)') sn(1:lnblnk(sn)),' is composed of ',
     &      SMLCN(icomp,i)(1:lsml),' and is complex fenestration const.'
        elseif(OPT(i)(1:7).eq.'UNKNOWN')then
          write(outs,'(4a)') sn(1:lnblnk(sn)),' is composed of ',
     &     SMLCN(icomp,i)(1:lsml),' and has unknown optical properties:'
        else
          write(outs,'(5a)') sn(1:lnblnk(sn)),' is composed of ',
     &      SMLCN(icomp,i)(1:lsml),' & optics ',OPT(i)
        endif
        CALL EDISP(ITRU,OUTS)
        NELT=NE(I)
        DO 9985 J=1,NELT
          IF(J.EQ.1.AND.J.EQ.NELT)THEN
            WRITE(OUTS,9980) J,iprtext(i,J),THK(I,J)*1000.,CON(I,J),
     &        DEN(I,J),SHT(I,J),EMISI(I),ABSI(I),PNAM(i,J)(1:32)
 9980       FORMAT(I6,1x,a,F7.1,F9.3,2F8.1,2F5.2,2x,a32)
            CALL EDISP(ITRU,OUTS)
          ELSEIF(J.EQ.1)THEN
            WRITE(OUTS,9981)J,iprtext(i,J),THK(I,J)*1000.,CON(I,J),
     &        DEN(I,J),SHT(I,J),EMISE(I),ABSE(I),PNAM(i,J)(1:32)
 9981       FORMAT(I6,1x,a,F7.1,F9.3,2F8.1,2F5.2,2x,a32)
            CALL EDISP(ITRU,OUTS)
          ELSEIF(J.EQ.NELT)THEN
            WRITE(OUTS,9983)  J,iprtext(i,J),THK(I,J)*1000.,CON(I,J),
     &        DEN(I,J),SHT(I,J),EMISI(I),ABSI(I),PNAM(i,J)(1:32)
 9983       FORMAT(I6,1x,a,F7.1,F9.3,2F8.1,2F5.2,2x,a32)
            CALL EDISP(ITRU,OUTS)
          ELSE
            WRITE(OUTS,9982)J,iprtext(i,J),THK(I,J)*1000.,CON(I,J),
     &        DEN(I,J),SHT(I,J),PNAM(i,J)(1:32)
 9982       FORMAT(I6,1x,a,F7.1,F9.3,2F8.1,12x,a32)
            CALL EDISP(ITRU,OUTS)
          ENDIF
 9985   CONTINUE

C Historic ESP-r assumptions of outside hc of 0.055 and horizontal
C flow internal hc of 0.123.
C       RT(I)=RT(I)+0.055+0.123
C       UVALUE=1.0/RT(I)

C ISO 6946 hc assumptions are 0.04 external, 0.13 inside horizontal,
C 0.10 inside upward flow and 0.17 inside downward flow.
        RTH=RT(I)+0.04+0.13
        UVALUEH=1.0/RTH
        RTU=RT(I)+0.04+0.10
        UVALUEU=1.0/RTU
        RTD=RT(I)+0.04+0.17
        UVALUED=1.0/RTD
        RTI=RT(I)+0.13+0.13
        UVALUEI=1.0/RTI
        lnsmlcn=lnblnk(SMLCN(icomp,i))
        IF(SOTF(icomp,i)(1:4).NE.'CFC2') THEN
          write(OUTS,'(3A,3F7.3,a,F7.3)')
     &      ' ISO 6946 U values (hor/up/dn heat flow) for ',
     &      SMLCN(icomp,i)(1:lnsmlcn),' is',
     &      UVALUEH,UVALUEU,UVALUED,' (partn)',UVALUEI
          call edisp(itru,OUTS)
        ENDIF

C Also list optical properties for this construction (if it is
C transparent and the optical property is likely to be known).
        if(OPT(i)(1:4).eq.'OPAQ'.or.OPT(i)(1:11).eq.'USE GSLedit'
     &     .or.OPT(i)(1:3).eq.'N/A')then
          continue
        elseif(OPT(i)(1:7).eq.'UNKNOWN')then
          continue
        else
          SOPT=OPT(i)
          CALL EROPTDB(1,itru,SOPT,GDESCR,IER)
        endif

        call edisp(itru,
     &    '______________________________________________________')
 9986 CONTINUE

      RETURN
      END

c ******************** ERTWIN 
c ERTWIN reads all transparent surface properties and optical
c control details from an annotated ascii file.

C Common block variables are:

c ITMCFL  - index identifying whether an opaque surface is to be
c         treated as a transparent construction.
c IBCMT   - index specifying whether movable shutter operation
c         is imposed:
c              IBCMT=0 ; no, therefore standard TMC properties
c                        hold at all times.
c              IBCMT=1 ; yes, therefore properties are replaced
c                        during specified periods.
c              IBCMT<0 ; points to optical control in the model
c                        control file in which case only the alt
c                        optical properties are held in the tmc file.
c TMCT    - direct solar transmittance at 5 representative
c         incidence angles for TMC system (standard values).
C TVTR    - visible transmitttance for the tmc (for daylighting).
C TMCA    - absorptances for each glazing element at 5 representative
C         incidence angles for TMC system (standard values).
C NBCTMC  - number of distinct tmc control periods during a
c         typical day (maximum=3).
c IBCST   - start hour of period (0-24).
c IBCFT   - finish hour of period.
c TMCT2   - direct solar transmittance at 5 representative incidence
c         angles for TMC system during each period (replacement values).
C TVTR2   - visible transmitttance for the tmc (replacement value).
c TMCA2   - absorptances for each glazing element at 5 representative
c         incidence angles for TMC system during each period
c         (replacement values).
c NBCTT   - index identifying the sensed blind control
c         variable type:
c               NBCTT=0 ; total radiation sensed,
c               NBCTT=1 ; ambient temperature sensed,
c               NBCTT=2 ; internal air temperature sensed,
c               NBCTT=3 ; daylight coeff. sensor lux maintainance
C                         by linear optical property variation,
C               NBCTT=4 ; SHOCC control (Lightswitch2002 manual
C                         control is currently not available)
C               NBCTT=-99; time based control (force alternative optics).
c BACTPT  - actuation point of blind. Value of sensed variable
c         must exceed this value for blind to operate. Setting
c         NBCTT is -99 the optical control is forced to
c         operate at all times within the required period.
c IBCSUR  - for a radiation sensor, specifies external surface
c         on which sensor is placed.
c         IBCSUR=0 implies sensor on each external surface.
c ITPREP  - index pointing to replacement thermophysical properties
c         (not used in the current implementation),

      SUBROUTINE ERTWIN(ITRC,ITRU,IUA,LUA,ICOMP,IER)
#include "building.h"
#include "geometry.h"
#include "espriou.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)

      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)
      COMMON/PRECT2/TMCT2(MCOM,MTMC,5),TMCA2(MCOM,MTMC,ME,5),
     &              TVTR2(MCOM,MTMC)
      COMMON/PRECT3/NTMC,NGLAZ(MTMC)
      common/PRECT4/TOPTIC(MCOM,MTMC)

      COMMON/TMCB1/IBCMT(MCOM,MTMC)
      COMMON/TMCB2/NBCTMC(MCOM,MTMC),IBCST(MCOM,MTMC),
     &             IBCFT(MCOM,MTMC),IBCSUR(MCOM,MTMC)
      COMMON/TMCB3/NBCTT(MCOM,MTMC),BACTPT(MCOM,MTMC)

      common/BSCTL/ITPREP(MCOM,MTMC),ICZN,IMDB(3),IGLZ(3)
      
      DIMENSION IVA(MS),RVC(MS)

      CHARACTER OUTSTR*124,OUTS*124,LUA*72,TOPTIC*12,WORD*20
      character msg*28
      LOGICAL MODGEO

      MODGEO=.FALSE.

      CALL EFOPSEQ(IUA,LUA,1,IER)
      IF(IER.NE.0)THEN
        IER=1
        goto 1000
      ENDIF
      write(currentfile,'(a)') LUA(1:lnblnk(LUA))

c Read zone transparent multi-layer construction properties from file.
C Read lines from file, discarding comments.
      CALL STRIPC(IUA,OUTSTR,1,ND,1,'number of zone surfaces',IER)
      IF(IER.NE.0)GOTO 1001
      K=0
      CALL EGETWI(OUTSTR,K,NS,4,MS,'W','no zone tmc surf',IER)

C Read pointer to type of TMC for each surface, strip comments etc. If
C ITMCFL != 0 and surface attribute is UNKNOWN or OPAQUE then confirm
C if the surface attribute should be updated.

      IRVA=NZSUR(icomp)
      CALL EGETWIA(IUA,IVA,IRVA,0,MTMC,'W','tmc list',IER)
      NTMC=0
      do I=1,NZSUR(icomp)
        icn1=izstocn(icomp,i)
        if(icn1.ne.0)then
          ITMCFL(ICOMP,I)=IVA(I)
          IF(ITMCFL(ICOMP,I).NE.0.AND.
     &      (SOTF(icomp,i)(1:4).EQ.'OPAQ'))THEN
            WRITE(OUTS,'(5a)')'The TMC file ',LUA(1:lnblnk(LUA)),
     &        ' not sure if ',SNAME(icomp,i),' is transparent.'
            call edisp(itru,outs)
            call edisp(itru,' Check your zone files.')
          ENDIF
        else
          WRITE(OUTS,'(a,i3,a,i2,a)')
     &      'While rebulding optics found surface ',i,' in zone ',
     &      icomp,' does not exist in master connections list.'
          call edisp(itru,outs)
        endif
        IF(ITMCFL(ICOMP,I).GT.NTMC)NTMC=ITMCFL(ICOMP,I)
      enddo
      IF(NTMC.EQ.0.OR.NTMC.GT.MTMC)THEN
        CALL EDISP(ITRU,' No. of transparent types out of range.')
        GOTO 1002
      ENDIF

C Reporting.
      IF(ITRC.GT.0)THEN
        CALL EDISP(ITRU,' ')
        WRITE(OUTS,9996)zname(ICOMP)(1:lnzname(ICOMP))
 9996   FORMAT(' Transparent construction file details for ',A)
        CALL EDISP(ITRU,OUTS)
        CALL EDISP(ITRU,' ')
        CALL EDISP(ITRU,' Surface      Construction OPAQ/   Optical  ')
        CALL EDISP(ITRU,' Name         Description  TRANS   Reference')
        DO 31, ISR=1,NZSUR(icomp)
          lnslmlcn=lnblnk(SMLCN(icomp,isr))
          WRITE(OUTS,'(1X,A,2X,A,2X,A,I4)')SNAME(icomp,isr),
     &      SMLCN(icomp,isr)(1:lnslmlcn),SOTF(icomp,isr)(1:6),
     &      ITMCFL(ICOMP,ISR)
          CALL EDISP(ITRU,OUTS)
   31   CONTINUE
      ENDIF

      DO 20 I=1,NTMC

c Establish the number of glazing layers and check that each item in
C the list points to an existing set of properties.
        CALL STRIPC(IUA,OUTSTR,99,ND,1,'glazing layers',IER)
        IF(IER.NE.0)GOTO 1001
        K=0
        CALL EGETWI(OUTSTR,K,NTGLAZ,1,ME,'W','glazing elem',IER)
        if(ND.eq.2)then
          CALL EGETW(OUTSTR,K,WORD,'W','matching optics',IFLAG)
          TOPTIC(ICOMP,I)=WORD(1:12)
        endif
        IERR=0
        do J=1,NS
          lnslmlcn=lnblnk(SMLCN(icomp,j))
          IF(ITMCFL(ICOMP,J).EQ.I.AND.NTGLAZ.NE.NE(J))IERR=1
          IF(IERR.EQ.1)THEN
            write(outs,'(6a,1x,a)')
     &      ' Mismatched TMC & mlc layers: ',
     &      SNAME(icomp,j)(1:lnblnk(SNAME(icomp,j))),' in ',
     &      zname(ICOMP)(1:lnzname(ICOMP)),' composed of ',
     &      SMLCN(icomp,j)(1:lnslmlcn),TOPTIC(ICOMP,I)
            call edisp(iuout,outs)
            goto 1002
          ENDIF
        enddo
        NGLAZ(I)=NTGLAZ

C Read transmissions (T) for the transparent multi-layer construction.
        CALL STRIPC(IUA,OUTSTR,99,ND,1,'transmission',IER)
        IF(IER.NE.0)GOTO 1001
        K=0
        CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 0',IER)
        TMCT(ICOMP,I,1)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 40',IER)
        TMCT(ICOMP,I,2)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 55',IER)
        TMCT(ICOMP,I,3)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 70',IER)
        TMCT(ICOMP,I,4)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 80',IER)
        TMCT(ICOMP,I,5)=VAL
        IF(ND.EQ.6)THEN
          CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','vis transmis',IER)
          TVTR(ICOMP,I)=VAL
        ELSE
          TVTR(ICOMP,I)=0.85
        ENDIF

c Read absorptions (A) for each element.
        do J=1,NTGLAZ
          IRVC=5
          CALL EGETWRA(IUA,RVC,IRVC,0.,0.999,'W','absorption',IER)
          do J5=1,5
            TMCA(ICOMP,I,J,J5)=RVC(J5)
          enddo
        enddo

C Calculate reflectance.
        SUM=TMCT(ICOMP,I,3)
        do K=1,NTGLAZ
          SUM=SUM+TMCA(ICOMP,I,K,3)
        enddo
        IF(SUM.LT.0..OR.SUM.GT.1.)then
          write(outs,'(3a,i2,a,f6.3)')' In ',
     &      zname(ICOMP)(1:lnzname(ICOMP)),' tmc ',
     &      i,': the optical reflectance is ',SUM
          CALL EDISP(ITRU,outs)
          CALL EDISP(ITRU,' Reading of zone optics terminated.')
          goto 1002
        endif
        TMCREF(ICOMP,I)=1.-SUM

C Reporting
        IF(ITRC.GT.0)THEN
          CALL EDISP(ITRU,' ')
          WRITE(OUTS,'(A,I2,A,F6.3)')
     &    ' For TMC type ',I,' with visible trn:',TVTR(ICOMP,I)
          CALL EDISP(ITRU,OUTS)
          CALL EDISP(ITRU,' Direct transmission @ 5 angles  ')
          WRITE(OUTS,'(2X,5F7.3)')(TMCT(ICOMP,I,J5),J5=1,5)
          CALL EDISP(ITRU,OUTS)
          CALL EDISP(ITRU,' For each layer absorption @ 5 angles ')
          do IL=1,NTGLAZ
            WRITE(OUTS,'(2X,5F7.3)')(TMCA(ICOMP,I,IL,J5),J5=1,5)
            CALL EDISP(ITRU,OUTS)
          enddo
        ENDIF

C Read in optical control information for this TMC type
        CALL STRIPC(IUA,OUTSTR,0,ND,1,'optical control',IER)
        IF(IER.NE.0)GOTO 1001
        K=0
        CALL EGETWI(OUTSTR,K,IBCMT(ICOMP,I),-10,1,'W','control',IER)
        if(IBCMT(ICOMP,I).EQ.0)then

C Reporting.
          IF(ITRC.GT.0)CALL EDISP(ITRU,' There are no controls active.')
          GOTO 20
        elseif(IBCMT(ICOMP,I).lt.0)then
          IF(ITRC.GT.0)CALL EDISP(ITRU,
     &      ' Points to an optical control loop.')
          NBCTMC(ICOMP,I)=1; IBCSUR(ICOMP,I)=0
          IBCST(ICOMP,I)=0; IBCFT(ICOMP,I)=24
          NBCTT(ICOMP,I)=0; BACTPT(ICOMP,I)=0.0

C Read replacement transmissions (T) for the transparent multi-layer construction
C and replacement visible transmittance.
          CALL STRIPC(IUA,OUTSTR,99,ND,1,'repl transmission',IER)
          IF(IER.NE.0)GOTO 1001
          K=0
          CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 0',IER)
          TMCT2(ICOMP,I,1)=VAL
          CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 40',IER)
          TMCT2(ICOMP,I,2)=VAL
          CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 55',IER)
          TMCT2(ICOMP,I,3)=VAL
          CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 70',IER)
          TMCT2(ICOMP,I,4)=VAL
          CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 80',IER)
          TMCT2(ICOMP,I,5)=VAL
          IF(ND.EQ.6)THEN
            CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','vis transmis',IER)
            TVTR2(ICOMP,I)=VAL
          ELSE
            TVTR2(ICOMP,I)=0.85
          ENDIF


C Read replacement absorptance for each substrate.
          do J=1,NTGLAZ
            CALL EGETWRA(IUA,RVC,5,0.,0.999,'W','alt abs',IER)
            do J5=1,5
              TMCA2(ICOMP,I,J,J5)=RVC(J5)
            enddo
          enddo

C Read dummy index for thermophysical property replacement.
          CALL STRIPC(IUA,OUTSTR,0,ND,1,'alt thermo prop',IER)
          IF(IER.NE.0)RETURN
          K=0
          CALL EGETWI(OUTSTR,K,IMLC,0,1,'-','alt pr',IER)
          ITPREP(ICOMP,I)=IMLC
          GOTO 20    ! jump past in-built control logic
        endif

        CALL STRIPC(IUA,OUTSTR,0,ND,1,'TMC control',IER)
        IF(IER.NE.0)GOTO 1001
        K=0
        CALL EGETWI(OUTSTR,K,N1,0,1,'W','control per',IER)
        NBCTMC(ICOMP,I)=N1
        CALL EGETWI(OUTSTR,K,N2,0,MS,'W','control sen',IER)
        IBCSUR(ICOMP,I)=N2

C Reporting.
        IF(ITRC.GT.0)THEN
          CALL EDISP(ITRU,' ')
          lto=lnblnk(TOPTIC(ICOMP,I))
          IF(N2.EQ.0)THEN
            write(outs,'(4a,i2,a)')' Surfaces with optic ',
     &        TOPTIC(ICOMP,I)(1:lto),' individually sensed ',
     &       'over ',NBCTMC(ICOMP,I),' control periods.'
          ELSE
            write(outs,'(3a,i2,a,i2,a)')' Surfaces with optic ',
     &        TOPTIC(ICOMP,I)(1:lto),' sense surface ',
     &        IBCSUR(ICOMP,I),' over ',NBCTMC(ICOMP,I),
     &        ' control periods.'
          ENDIF
          CALL EDISP(ITRU,OUTS)
        ENDIF

C For each period.
        DO 21 KK=1,N1
          CALL STRIPC(IUA,OUTSTR,0,ND,1,'period range',IER)
          IF(IER.NE.0)GOTO 1001
          K=0
          CALL EGETWI(OUTSTR,K,IS,0,24,'W','start',IER)
          CALL EGETWI(OUTSTR,K,IF,IS,24,'W','end',IER)
          IBCST(ICOMP,I)=IS
          IBCFT(ICOMP,I)=IF

          CALL STRIPC(IUA,OUTSTR,0,ND,1,'sensor data',IER)
          IF(IER.NE.0)GOTO 1001
          K=0
          CALL EGETWI(OUTSTR,K,NCT,-99,4,'W','sensor type',IER)
          NBCTT(ICOMP,I)=NCT
          CALL EGETWR(OUTSTR,K,ACTP,0.,0.,'-','actuation pt',IER)
          BACTPT(ICOMP,I)=ACTP

C Reporting.
          IF(ITRC.GT.0)THEN
            if(NBCTT(ICOMP,I).eq.0)then
              msg=' sensing total radiation'
            elseif(NBCTT(ICOMP,I).eq.1)then
              msg=' sensing ambient temperature'
            elseif(NBCTT(ICOMP,I).eq.2)then
              msg=' sensing zone temperature'
            elseif(NBCTT(ICOMP,I).eq.3)then
              msg=' sensing daylight coeff.'
            elseif(NBCTT(ICOMP,I).eq.4)then
              msg=' Lightswitch2002.'
            elseif(NBCTT(ICOMP,I).eq.-99)then
              msg=' sensing time'
            endif
            WRITE(OUTS,'(A,I2,A,I2,A,I2,2A,F7.2)') ' Period ',KK,
     &        ': from ',IBCST(ICOMP,I),' to ',IBCFT(ICOMP,I),
     &        msg(1:lnblnk(msg)),' set @ ',BACTPT(ICOMP,I)
            CALL EDISP(ITRU,OUTS)
          ENDIF

C Read replacement transmissions (T) for the transparent multi-layer construction
C and replacement visible transmittance.
          CALL STRIPC(IUA,OUTSTR,99,ND,1,'transmission',IER)
          IF(IER.NE.0)GOTO 1001
          K=0
          CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 0',IER)
          TMCT2(ICOMP,I,1)=VAL
          CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 40',IER)
          TMCT2(ICOMP,I,2)=VAL
          CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 55',IER)
          TMCT2(ICOMP,I,3)=VAL
          CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 70',IER)
          TMCT2(ICOMP,I,4)=VAL
          CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','dir t @ 80',IER)
          TMCT2(ICOMP,I,5)=VAL
          IF(ND.EQ.6)THEN
            CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','vis transmis',IER)
            TVTR2(ICOMP,I)=VAL
          ELSE
            TVTR2(ICOMP,I)=0.85
          ENDIF


C Read replacement absorptance for each substrate.
          do J=1,NTGLAZ
            CALL EGETWRA(IUA,RVC,5,0.,0.999,'W','alt abs',IER)
            do J5=1,5
              TMCA2(ICOMP,I,J,J5)=RVC(J5)
            enddo
          enddo

C Reporting.
          IF(ITRC.GT.0)THEN
            CALL EDISP(ITRU,' Alt direct trans @ 5 angles & vis tran  ')
            WRITE(OUTS,'(2X,6F6.2)')(TMCT2(ICOMP,I,J5),J5=1,5),
     &                              TVTR2(ICOMP,I)
            CALL EDISP(ITRU,OUTS)
            CALL EDISP(ITRU,' For each layer alt absorp @ 5 angles ')
            do IL=1,NTGLAZ
              WRITE(OUTS,'(2X,5F7.3)')(TMCA2(ICOMP,I,IL,J5),J5=1,5)
              CALL EDISP(ITRU,OUTS)
            enddo
          ENDIF

C Now read in index for thermophysical property replacement
C In this version, index will be zero; if not give error
          CALL STRIPC(IUA,OUTSTR,0,ND,1,'alt thermo prop',IER)
          IF(IER.NE.0)RETURN
          K=0
          CALL EGETWI(OUTSTR,K,IMLC,0,1,'-','alt pr',IER)
          ITPREP(ICOMP,I)=IMLC
   21   CONTINUE
   20 CONTINUE

C If surface attributes have been changed, warn user.
      IF(MODGEO)THEN
        CALL USRMSG(' Remember to update the model description',
     &  ' as one or more surface attributes have changed!','W')
      ENDIF

c Free file.
 1000 CALL ERPFREE(IUA,ISTAT)
      RETURN

C Error messages.
 1001 CALL USRMSG(' Problem reading data in:',OUTSTR,'W')
      goto 1000
 1002 write(outs,'(a,a)') ' Please check data in: ',LUA(1:lnblnk(LUA))
      CALL USRMSG(outs,' and try the model again!','W')
      goto 1000

      END

C ******************************* THERMS ******************************
C THERMS saves a non-linear thermophysical properties configuration
C file.
C *********************************************************************
      SUBROUTINE THERMS(FLTHRM,IIN,ITRC,IER)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/VTHP11/NTHF,IBTHAL(MTHF,3)
      COMMON/VTHP12/NTHEQ(MTHF),BTHDT(MTHF,MTHEQ,MDATA)

      CHARACTER*72 FLTHRM

      IER=0

C Open existing ASCII construction data file as unit IIN.
      CALL  EFOPSEQ(IIN,FLTHRM,4,IER)
      IF(IER.LT.0)RETURN
      WRITE(IIN,'(I2,5X,A)')NTHF,'# Total number of thermal functions'
      DO 10 I=1,NTHF
        WRITE(IIN,'(1X,I2,10X,A)')I,'# thermal function No.'
        WRITE(IIN,91)(IBTHAL(I,K),K=1,3),'# actuator location'
   91   FORMAT(3(1X,I2),4X,A)
        WRITE(IIN,'(1X,I2,10X,A)')NTHEQ(I),'# Total thermal equations'
        WRITE(IIN,'(2A)')'# No T  MIN(Temp.)MAX      A          B     ',
     &        '   b       C        c       D        d        E        e'
        DO 20 J=1,NTHEQ(I)
          WRITE(IIN,92)J,INT(BTHDT(I,J,1)),(BTHDT(I,J,K),K=2,12)
   20   CONTINUE
   10 CONTINUE
   92 FORMAT(2X,I1,2X,I1,2(1X,F7.2),1X,F10.4,4(1X,F9.6,1X,F6.2))
      IF(ITRC.NE.0)THEN
       CALL EDISP(IUOUT,'Thermal configuration file successfuly saved!')
      ENDIF
      CALL ERPFREE(IIN,ISTAT)
      RETURN
      END

C ****************************** THERMR *******************************
C THERMR reads a non-linear thermophysical properties configuration
C file.
C *********************************************************************
      SUBROUTINE THERMR(FLTHRM,IIN,ITRC,IER)
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/VTHP11/NTHF,IBTHAL(MTHF,3)
      COMMON/VTHP12/NTHEQ(MTHF),BTHDT(MTHF,MTHEQ,MDATA)
      COMMON/VTHP31/INTHPS,INTHPZ(MCOM)
      LOGICAL INTHPS,INTHPZ

      CHARACTER FLTHRM*72,OUTSTR*124

      IER=0

C Open existing ASCII construction data file as unit IIN.
      CALL EFOPSEQ(IIN,FLTHRM,1,IER)
      IF(IER.LT.0)GOTO 777
      DO 10 I=1,NCOMP
        INTHPZ(I)=.FALSE.
   10 CONTINUE
      CALL STRIPC(IIN,OUTSTR,1,ND,1,' No. of functions ',IER)
      IF(IER.NE.0)GOTO 777
      K=0
      CALL EGETWI(OUTSTR,K,NTHF,0,MTHF,'F',' functions ',IER)
      IF(IER.NE.0)GOTO 777
      IF(NTHF.GT.0)INTHPS=.TRUE.
      DO 20 I=1,NTHF
        CALL STRIPC(IIN,OUTSTR,1,ND,1,' function No.',IER)
        IF(IER.NE.0)GOTO 777
        K=0
        CALL EGETWI(OUTSTR,K,ITHF,0,MTHF,'W',' function No.',IER)
        IF(IER.NE.0)GOTO 777
        CALL STRIPC(IIN,OUTSTR,3,ND,1,'actuator location',IER)
        IF(IER.NE.0)GOTO 777
        K=0
        CALL EGETWI(OUTSTR,K,IBTHAL(ITHF,1),0,NCOMP,'W','IZ',IER)
        IF(IER.NE.0)GOTO 777
        IF(IBTHAL(ITHF,1).EQ.0)THEN
          DO 30 J=1,NCOMP
            INTHPZ(J)=.TRUE.
   30     CONTINUE
        ELSE
          INTHPZ(IBTHAL(ITHF,1))=.TRUE.
        ENDIF
        CALL EGETWI(OUTSTR,K,IBTHAL(ITHF,2),0,MS,'W','IS',IER)
        CALL EGETWI(OUTSTR,K,IBTHAL(ITHF,3),0,ME,'W','IL',IER)
        CALL STRIPC(IIN,OUTSTR,1,ND,1,'No. of equations',IER)
        IF(IER.NE.0)GOTO 777
        K=0
        CALL EGETWI(OUTSTR,K,NTHEQ(I),0,MTHEQ,'F',' eqns ',IER)
        IF(IER.NE.0)GOTO 777
        DO 40 J=1,NTHEQ(I)
          CALL STRIPC(IIN,OUTSTR,13,ND,1,'coefficients',IER)
          IF(IER.NE.0)GOTO 777
          K=0
          CALL EGETWI(OUTSTR,K,IDUM,1,MTHEQ,'F',' eqn No. ',IER)
          IF(IER.NE.0)GOTO 777
          CALL EGETWI(OUTSTR,K,ITYPE,1,3,'F',' property type',IER)
          IF(IER.NE.0)GOTO 777
          BTHDT(I,J,1)=FLOAT(ITYPE)
          CALL EGETWR(OUTSTR,K,BTHDT(I,J,2),-100.,100.,'-',
     &              ' min temp. ',IER)
          CALL EGETWR(OUTSTR,K,BTHDT(I,J,3),BTHDT(I,J,2),
     &                  10000.,'F', ' max temp. ',IER)
          CALL EGETWR(OUTSTR,K,BTHDT(I,J,4),-100.,100.,'-',
     &                  ' coefficient A ',IER)
          CALL EGETWR(OUTSTR,K,BTHDT(I,J,5),-100.,100.,'-',
     &                  ' coefficient B ',IER)
          CALL EGETWR(OUTSTR,K,BTHDT(I,J,6),-100.,100.,'-',
     &                  ' coefficient b ',IER)
          CALL EGETWR(OUTSTR,K,BTHDT(I,J,7),-100.,100.,'-',
     &                  ' coefficient C ',IER)
          CALL EGETWR(OUTSTR,K,BTHDT(I,J,8),-100.,100.,'-',
     &                  ' coefficient c ',IER)
          CALL EGETWR(OUTSTR,K,BTHDT(I,J,9),-100.,100.,'-',
     &                  ' coefficient D ',IER)
          CALL EGETWR(OUTSTR,K,BTHDT(I,J,10),-100.,100.,'-',
     &                  ' coefficient d ',IER)
          CALL EGETWR(OUTSTR,K,BTHDT(I,J,11),-100.,100.,'-',
     &                  ' coefficient E ',IER)
          CALL EGETWR(OUTSTR,K,BTHDT(I,J,12),-100.,100.,'-',
     &                  ' coefficient e ',IER)
   40   CONTINUE
   20 CONTINUE
      IF(ITRC.NE.0)THEN
        CALL EDISP(IUOUT,'Thermal configuration file successfuly read!')
      ENDIF
      CALL ERPFREE(IIN,ISTAT)
      RETURN
  777 CALL ERPFREE(IIN,ISTAT)
      RETURN
      END

C ******** ERBIWIN
C  ERBIWIN: reads bi-directional optical data from measurements
C           file.
      subroutine erbiwin(itru,iua,lua,ier)
#include "building.h"
#include "espriou.h"
      PARAMETER (MSTMC=20,MSGAL=40,MANH=37,MANV=37)
      
      integer lnblnk  ! function definition

C Parameters
      integer itru  ! channel for feedback
      integer iua   ! file unit
      character LUA*72  ! optical data file name
      integer ier   ! error state to return zero is ok

      COMMON/BIDIR/IFLAGBI,INTVALBI,NSTMCFL(MCON)
      COMMON/BIDIRFL/bidirfile,bidirname(MSTMC)

C NGNTL number of layers in each TMC type.
C NGANGS number of angles in each TMC type. Set to 37 at present.
C TMTSOD(MSTMC,MSGAL,MANH,MANV) - outdoor side direct solar trans (direct to direct)
C TMTSOB(MSTMC,MSGAL,MANH,MANV) - outdoor side direct solar trans (direct to diff)
C TMABSO(MSTMC,MSGAL,ME,MANH,MANV) - outdoor side solar absorb for each alternative
C   and each layer and each angle.
C THTSOB(MSTMC,MSGAL) - outdoor side direct solar trans (diff to diff)
C THRSOB(MSTMC,MSGAL) - outdoor side solar refl (diff to diff)
C TMABSDIF(MSTMC,MSGAL,ME) - outdoor side diffuse solar absorptance for each layer
c TMGVALUE(MSTMC,MSGAL,MANH,MANV) - total solar energy transmittance (g value)
C TMREFLECT(MSTMC,MSGAL,MANH,MANV) - Reflectance
C TUVALUE(MSTMC,MSGAL) - U value based on experimental Re and Ri
C TREXTERNAL(MSTMC,MSGAL) - experimental external thermal resistance (from
C                          measurements)
C TRINTERNAL(MSTMC,MSGAL) - experimental internal thermal resistance (from
C                          measurements)
C TMVISUAL(MSTMC,MSGAL) - Visual transmittance for each set in case it is available.
C                         Will use (in solar.F) default value 0.85 in case it 
C                         is not included in the dataset. This is only used for 
C                         daylight controls.
C DF_TX_SKY is the diffuse transmittance for sky radiation
C DF_G_SKY is the diffuse g-value for sky radiation
C DF_RF_SKY is the diffuse reflectance for sky radiation
C Similarly for DF_TX_GND, DF_G_GND and DF_RF_GND for ground reflected radiation
C a_dfSKYinner and a_dfGNDinner are the inner layer absorptances for 
C   sky and ground diffuse radiation
C a_dfSKYouter and a_dfGNDouter are the inner layer absorptances for 
C   sky and ground diffuse radiation
      COMMON/OPTDAT/NSGALFL(MSTMC),NGNTL(MSTMC),
     &  NGANGS(MSTMC),TMTSOD(MSTMC,MSGAL,MANH,MANV),
     &  TMTSOB(MSTMC,MSGAL,MANH,MANV),TMABSO(MSTMC,MSGAL,ME,MANH,MANV),
     &  THTSOB(MSTMC,MSGAL),TMABSDIF(MSTMC,MSGAL,ME),
     &  TMGVALUE(MSTMC,MSGAL,MANH,MANV),
     &  TMREFLECT(MSTMC,MSGAL,MANH,MANV),TUVALUE(MSTMC,MSGAL),
     &  TREXTERNAL(MSTMC,MSGAL),TRINTERNAL(MSTMC,MSGAL),
     &  TMVISUAL(MSTMC,MSGAL)
      COMMON/BITYPE/IBIDATATYPE       
      integer IBIDATATYPE !thisis kept in a common block
                          !It specifies what type of data to read
                          !1: the existing transmittance and at different layers absorptance 
                          !2: the transmittance, g value and reflectance 
                          !3: the transmittance and g value only
      COMMON/OPTDAT2/DF_TX_SKY(MSTMC,MSGAL),DF_G_SKY(MSTMC,MSGAL),
     &  DF_RF_SKY(MSTMC,MSGAL),DF_TX_GND(MSTMC,MSGAL), 
     &  DF_G_GND(MSTMC,MSGAL),DF_RF_GND(MSTMC,MSGAL),
     &  a_dfSKYinner(MSTMC,MSGAL),a_dfGNDinner(MSTMC,MSGAL),
     &  a_dfSKYouter(MSTMC,MSGAL),a_dfGNDouter(MSTMC,MSGAL) 
      character OUTSTR*124,OUTS*124,WORD*20
      character bidirfile*72,bidirname*12
      logical bCloseToZero

C New variables are defined below
      integer ISETTYPE !this specifies what the set of data is about
                       !For example 1 can be for 0 degrees slat angle
                       !No rules have been defined for this.

C explicit definition of some variables used in the egetwr 
      real Uvalue_bi,Ri,Re,visual_transm 

      CALL EFOPSEQ(IUA,LUA,1,IER)
      IF(IER.NE.0)THEN
        IER=1
        goto 1000
      ENDIF
      write(currentfile,'(a)') LUA(1:lnblnk(LUA))

C Initiate all parameters needed for simulation to 0
      do 111 imstmc=1,MSTMC
        do 222 imsgal=1,MSGAL
          THTSOB(imstmc,imsgal)=0.
          TUVALUE(imstmc,imsgal)=0.
          TREXTERNAL(imstmc,imsgal)=0.
          TRINTERNAL(imstmc,imsgal)=0.
          TMVISUAL(imstmc,imsgal)=0.
          DF_TX_SKY(imstmc,imsgal)=0.
          DF_G_SKY(imstmc,imsgal)=0.
          DF_RF_SKY(imstmc,imsgal)=0.
          DF_TX_GND(imstmc,imsgal)=0.
          DF_G_GND(imstmc,imsgal)=0.
          DF_RF_GND(imstmc,imsgal)=0.
          a_dfSKYinner(imstmc,imsgal)=0.
          a_dfGNDinner(imstmc,imsgal)=0.
          a_dfSKYouter(imstmc,imsgal)=0.
          a_dfGNDouter(imstmc,imsgal)=0.
          do 333 ime=1,ME
            TMABSDIF(imstmc,imsgal,ime)=0.
            do 444 imanh=1,MANH
              do 555 imanv=1,MANV
                TMABSO(imstmc,imsgal,ime,imanh,imanv)=0.
                TMTSOD(imstmc,imsgal,imanh,imanv)=0.
                TMTSOB(imstmc,imsgal,imanh,imanv)=0.
                TMGVALUE(imstmc,imsgal,imanh,imanv)=0.
                TMREFLECT(imstmc,imsgal,imanh,imanv)=0.
  555         continue 
  444       continue                
  333     continue                 
  222   continue       
  111 continue
  
C Read header.
      CALL STRIPC(IUA,OUTSTR,99,ND,1,'bi-optic header',IER)
      if(outstr(1:14).eq.'*BIDIRECTIONAL')then
        ibitype=0
      else
        write(outs,'(2a)') 'The file ',LUA(1:lnblnk(LUA))
        call edisp(itru,outs)
        call edisp(itru,
     &    'does not contain bi-directional optical data.')
        ier=1
        return
      endif

  42  CALL STRIPC(IUA,OUTSTR,99,ND,1,'bi-optic tags',IER)
      if(ier.ne.0)then
        CALL ERPFREE(IUA,ISTAT)
        call edisp(itru,'Closing bi-directional file')
        ier=0
        return
      endif
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','bi-optic tags',IFLAG)
      if(WORD(1:6).eq.'*types')then
        CALL EGETWI(OUTSTR,K,IFLAGBI,1,MSTMC,'W','bi-type',IER)
        goto 42
      elseif(WORD(1:5).eq.'*item')then
        CALL EGETW(OUTSTR,K,WORD,'W','bi-type name',IFLAG)
        ibitype=ibitype+1
        ibset=0
        bidirname(ibitype)=WORD(1:12)
        goto 42
      elseif(WORD(1:9).eq.'*end_item')then
        goto 42

C 3 options: Te_g_rho, Te_g_only and existing: Te_abs_n_diffuse
      elseif(WORD(1:9).eq.'*datatype')then
        CALL EGETW(OUTSTR,K,WORD,'W','bi-type format',IFLAG)
        if(WORD(1:16).eq.'Te_abs_n_diffuse')then

C Existing format - keep reading as previously for existing bidirectional files        
          IBIDATATYPE=1
          goto 42
        elseif(WORD(1:8).eq.'Te_g_rho')then
          IBIDATATYPE=2

C expect less columns and different type of data
          goto 42
        elseif(WORD(1:9).eq.'Te_g_only')then
          IBIDATATYPE=3

C expect less columns and different type of data
          goto 42
        else
          IBIDATATYPE=1

C Existing format - keep reading as previously for existing bidirectional files        
          goto 42        
        endif
      elseif(WORD(1:7).eq.'*layers')then
        CALL EGETWI(OUTSTR,K,IV,1,8,'W','bi-layers',IER)
        NGNTL(ibitype)=iv
        goto 42
      elseif(WORD(1:5).eq.'*sets')then
        CALL EGETWI(OUTSTR,K,IV,1,20,'W','bi-sets',IER)
        NSGALFL(ibitype)=iv
        goto 42
      elseif(WORD(1:10).eq.'*start_set')then
        ibset=ibset+1
        goto 42
      elseif(WORD(1:9).eq.'*set_type')then
        CALL EGETWI(OUTSTR,K,IV,1,100,'W','set types',IER)
        ISETTYPE=IV   ! not used yet
        goto 42
      elseif(WORD(1:8).eq.'*end_set')then
        goto 42
      elseif(WORD(1:9).eq.'*end_file')then
        goto 999
      elseif(WORD(1:7).eq.'*Uvalue')then
        CALL EGETWR(OUTSTR,K,Uvalue_bi,0.,1.,'-','u value ',IER)
        TUVALUE(ibitype,ibset)=Uvalue_bi
        goto 42
      elseif(WORD(1:10).eq.'*Rexternal')then
        CALL EGETWR(OUTSTR,K,Re,0.,1.,'-','Re experimental ',IER)
        TREXTERNAL(ibitype,ibset)=Re
        goto 42
      elseif(WORD(1:10).eq.'*Rinternal')then
        CALL EGETWR(OUTSTR,K,Ri,0.,1.,'-','Ri experimental ',IER)
        TRINTERNAL(ibitype,ibset)=Ri
        goto 42
      elseif(WORD(1:7).eq.'*tauvis')then
        CALL EGETWR(OUTSTR,K,visual_transm,0.,1.,'-','visual transm ',
     &              IER)
        TMVISUAL(ibitype,ibset)=visual_transm
        goto 42
      elseif(WORD(1:12).eq.'*diffuse_abs')then

C Get diffuse abs for each layer.
        do 43 ij=1,NGNTL(ibitype)
          CALL EGETWR(OUTSTR,K,da,0.,1.,'-','diffuse abs ',IER)
          TMABSDIF(ibitype,ibset,ij)=da
43      continue
        goto 42
      elseif(WORD(1:12).eq.'*diffuse_trn')then
        CALL EGETWR(OUTSTR,K,TH,0.,1.,'-','diffuse trn ',IER)
        THTSOB(ibitype,ibset)=TH
        goto 42
      elseif(WORD(1:12).eq.'*direct_angs')then

C Get direct angles horiz and vert. For hemisphere at 5 degrees
C there should be 1369 data lines.
        CALL EGETWI(OUTSTR,K,IV1,1,37,'W','bi-angles',IER)
        CALL EGETWI(OUTSTR,K,IV2,1,37,'W','bi-angles',IER)
        NGANGS(ibitype)=iv1

C << idataloop is not yet used >>
C        if(iv1.eq.37)idataloop=1369
        goto 42
      elseif(WORD(1:12).eq.'*data')then
  44    CALL STRIPC(IUA,OUTSTR,99,ND,1,'bi-optic data',IER)
        if(ier.ne.0)then
          CALL ERPFREE(IUA,ISTAT)
          call edisp(itru,'Closing bi-directional file after *data.')
          ier=0
          return
        endif
        K=0
        if(OUTSTR(1:8).eq.'*end_set')then
          goto 42
        elseif(OUTSTR(1:9).eq.'*end_item')then
          goto 42
        else
          CALL EGETWI(OUTSTR,K,IA1,-90,90,'W','bi-horiz ang',IER)
          ihangindex= (IA1/5)+19
          CALL EGETWI(OUTSTR,K,IA2,-90,90,'W','bi-vert ang',IER)
          ivangindex= (IA2/5)+19
          if(IBIDATATYPE.eq.1)then
            CALL EGETWR(OUTSTR,K,T1,0.,1.,'-','total trn',IER)
            TMTSOD(ibitype,ibset,ihangindex,ivangindex)=T1
            do 143 ij=1,NGNTL(ibitype)
              CALL EGETWR(OUTSTR,K,da,0.,1.,'-','direct abs',IER)
              TMABSO(ibitype,ibset,ij,ihangindex,ivangindex)=da
 143        continue
            CALL EGETWR(OUTSTR,K,T2,0.,1.,'-','dirdif trn',IER)
            TMTSOB(ibitype,ibset,ihangindex,ivangindex)=T2

C Debug.
C          write(6,*) ibitype,ibset,T1,T2

            goto 44
          elseif(IBIDATATYPE.eq.2)then
            CALL EGETWR(OUTSTR,K,T1,0.,1.,'-','total trn',IER)
            TMTSOD(ibitype,ibset,ihangindex,ivangindex)=T1
            CALL EGETWR(OUTSTR,K,g,0.,1.,'-','g value',IER)
            TMGVALUE(ibitype,ibset,ihangindex,ivangindex)=g
            CALL EGETWR(OUTSTR,K,rho,0.,1.,'-','reflectance',IER)
            TMREFLECT(ibitype,ibset,ihangindex,ivangindex)=rho
            goto 44
          elseif(IBIDATATYPE.eq.3)then
            CALL EGETWR(OUTSTR,K,T1,0.,1.,'-','total trn',IER)
            TMTSOD(ibitype,ibset,ihangindex,ivangindex)=T1
            CALL EGETWR(OUTSTR,K,g,0.,1.,'-','g value',IER)
            TMGVALUE(ibitype,ibset,ihangindex,ivangindex)=g
            goto 44
          endif
        endif
      endif
     
C Now calculate diffuse properties for birectional data types 2 and 3.
C The calculation is done here so the assumption is that all surfaces are vertical
c and also that the diffuse sky is isotropic.
C Separate diffuse properties are calculated for ground and sky components. 
C Could be improved later with a separate calculation for each surface, depending on its tilt.

C Do for each bidirectional type (MSTMC).
C Do for each dataset (MSGAL).
 999  IF(IBIDATATYPE.eq.2.or.IBIDATATYPE.eq.3) then
        R=ATAN(1.0)/45.0
        DO 200 ibidir=1,ibitype 
        DO 201 ibset=1,NSGALFL(ibitype)

C Firstly, focus on sky diffuse properties, for which solar altitude >=0.
C Use Trapezoidal Rule twice, for double integral in formula
C given by Kuhn T E, Energy and Buildings 38 (2006) pp648-660.
C Note that the integrals for sky and ground are separated in the implementation below.
          SUM1=0.0
          SUM2=0.0
          SUM3=0.0
          SUM4=0.0
          SUM5=0.0
          SUM6=0.0

C First of all for ivangindex=19 (i.e. solar altitude = 0deg) 
          SUM1=SUM1+0.5*TMTSOD(ibidir,ibset,1,19)*cos(-90.0*R)
          SUM2=SUM2+0.5*TMGVALUE(ibidir,ibset,1,19)*cos(-90.0*R)
          IF (IBIDATATYPE.eq.2)then
            SUM3=SUM3+0.5*TMREFLECT(ibidir,ibset,1,19)*cos(-90.0*R)
          ENDIF

          DO 202 ihangindex=2,36
            COSMULT1=cos(5.0*real(ihangindex-19)*R)
            SUM1=SUM1+TMTSOD(ibidir,ibset,ihangindex,19)*COSMULT1
            SUM2=SUM2+TMGVALUE(ibidir,ibset,ihangindex,19)*COSMULT1
            IF (IBIDATATYPE.eq.2)then
              SUM3=SUM3+TMREFLECT(ibidir,ibset,ihangindex,19)*COSMULT1
            ENDIF
 202      CONTINUE
          SUM1=SUM1+0.5*TMTSOD(ibidir,ibset,37,19)*cos(90.0*R)
          SUM2=SUM2+0.5*TMGVALUE(ibidir,ibset,37,19)*cos(90.0*R)
          IF (IBIDATATYPE.eq.2)then
            SUM3=SUM3+0.5*TMREFLECT(ibidir,ibset,37,19)*cos(90.0*R)
          ENDIF
          SUM4=SUM4+SUM1*0.5*(cos(0.0*R))**2
          SUM5=SUM5+SUM2*0.5*(cos(0.0*R))**2
          SUM6=SUM6+SUM3*0.5*(cos(0.0*R))**2
          SUM1=0.0
          SUM2=0.0
          SUM3=0.0

C Now for ivangindex = 20 to 36 (i.e. solar altitude = +5deg to +85deg)         
          DO 203 ivangindex=20,36
            SUM1=SUM1+0.5*TMTSOD(ibidir,ibset,1,ivangindex)*cos(-90.0*R)
            SUM2=SUM2+0.5*TMGVALUE(ibidir,ibset,1,ivangindex)
     &                  *cos(-90.0*R)
            IF (IBIDATATYPE.eq.2)then
              SUM3=SUM3+0.5*TMREFLECT(ibidir,ibset,1,ivangindex)
     &                  *cos(-90.0*R)
            ENDIF
            DO 204 ihangindex=2,36
              COSMULT1=cos(5.0*real(ihangindex-19)*R)
              SUM1=SUM1+TMTSOD(ibidir,ibset,ihangindex,ivangindex)
     &                  *COSMULT1
              SUM2=SUM2+TMGVALUE(ibidir,ibset,ihangindex,ivangindex)
     &                  *COSMULT1
              IF (IBIDATATYPE.eq.2)then
                SUM3=SUM3+TMREFLECT(ibidir,ibset,ihangindex,ivangindex)
     &                    *COSMULT1
              ENDIF
204        CONTINUE
            SUM1=SUM1+0.5*TMTSOD(ibidir,ibset,37,ivangindex)*cos(90.0*R)
            SUM2=SUM2+0.5*TMGVALUE(ibidir,ibset,37,ivangindex)
     &                  *cos(90.0*R)
            IF (IBIDATATYPE.eq.2)then
              SUM3=SUM3+0.5*TMREFLECT(ibidir,ibset,37,ivangindex)
     &                  *cos(90.0*R)
            ENDIF
            SUM4=SUM4+SUM1*(cos(5.0*real(ivangindex-19)*R))**2
            SUM5=SUM5+SUM2*(cos(5.0*real(ivangindex-19)*R))**2
            SUM6=SUM6+SUM3*(cos(5.0*real(ivangindex-19)*R))**2
            SUM1=0.0
            SUM2=0.0
            SUM3=0.0
 203      CONTINUE

C Finally for ivangindex = 37 (i.e. solar altitude = +90deg)
          SUM1=SUM1+0.5*TMTSOD(ibidir,ibset,1,37)*cos(-90.0*R)
          SUM2=SUM2+0.5*TMGVALUE(ibidir,ibset,1,37)
     &                *cos(-90.0*R)
          IF (IBIDATATYPE.eq.2)then
            SUM3=SUM3+0.5*TMREFLECT(ibidir,ibset,1,37)
     &                *cos(-90.0*R)
          ENDIF
         DO 205 ihangindex=2,36
            COSMULT1=cos(5.0*real(ihangindex-19)*R)
            SUM1=SUM1+TMTSOD(ibidir,ibset,ihangindex,37)
     &               *COSMULT1
            SUM2=SUM2+TMGVALUE(ibidir,ibset,ihangindex,37)
     &               *COSMULT1
            IF (IBIDATATYPE.eq.2)then
              SUM3=SUM3+TMREFLECT(ibidir,ibset,ihangindex,37)
     &                 *COSMULT1
            ENDIF
205      CONTINUE
          SUM1=SUM1+0.5*TMTSOD(ibidir,ibset,37,37)*cos(90.0*R)
          SUM2=SUM2+0.5*TMGVALUE(ibidir,ibset,37,37)*cos(90.0*R)
          IF (IBIDATATYPE.eq.2)then
            SUM3=SUM3+0.5*TMREFLECT(ibidir,ibset,37,37)*cos(90.0*R)
          ENDIF
          SUM4=SUM4+SUM1*0.5*(cos(90.0*R))**2
          SUM5=SUM5+SUM2*0.5*(cos(90.0*R))**2
          SUM6=SUM6+SUM3*0.5*(cos(90.0*R))**2
    
C Multiply by constant terms to get diffuse properties.  
          DF_TX_SKY(ibidir,ibset)=SUM4*0.5/atan(1.0)*(5.0*R)**2
          DF_G_SKY(ibidir,ibset)=SUM5*0.5/atan(1.0)*(5.0*R)**2
          DF_RF_SKY(ibidir,ibset)=SUM6*0.5/atan(1.0)*(5.0*R)**2

C Now repeat for ground reflected properties.
          SUM1=0.0
          SUM2=0.0
          SUM3=0.0
          SUM4=0.0
          SUM5=0.0
          SUM6=0.0

C First of all for ivangindex=1 (i.e. solar altitude = -90deg) 
          SUM1=SUM1+0.5*TMTSOD(ibidir,ibset,1,1)*cos(-90.0*R)
          SUM2=SUM2+0.5*TMGVALUE(ibidir,ibset,1,1)*cos(-90.0*R)
          IF (IBIDATATYPE.eq.2)then
            SUM3=SUM3+0.5*TMREFLECT(ibidir,ibset,1,1)*cos(-90.0*R)
          ENDIF
          DO 206 ihangindex=2,36
            COSMULT1=cos(5.0*real(ihangindex-19)*R)
            SUM1=SUM1+TMTSOD(ibidir,ibset,ihangindex,1)*COSMULT1
            SUM2=SUM2+TMGVALUE(ibidir,ibset,ihangindex,1)*COSMULT1
            IF (IBIDATATYPE.eq.2)then
              SUM3=SUM3+TMREFLECT(ibidir,ibset,ihangindex,1)*COSMULT1
            ENDIF
 206      CONTINUE
          SUM1=SUM1+0.5*TMTSOD(ibidir,ibset,37,1)*cos(90.0*R)
          SUM2=SUM2+0.5*TMGVALUE(ibidir,ibset,37,1)*cos(90.0*R)
          IF (IBIDATATYPE.eq.2)then
            SUM3=SUM3+0.5*TMREFLECT(ibidir,ibset,37,9)*cos(90.0*R)
          ENDIF
          SUM4=SUM4+SUM1*0.5*(cos(-90.0*R))**2
          SUM5=SUM5+SUM2*0.5*(cos(-90.0*R))**2
          SUM6=SUM6+SUM3*0.5*(cos(-90.0*R))**2
          SUM1=0.0
          SUM2=0.0
          SUM3=0.0

C Now for ivangindex = 2 to 18 (i.e. solar altitude = -85deg to -5deg)         
          DO 207 ivangindex=2,18
            SUM1=SUM1+0.5*TMTSOD(ibidir,ibset,1,ivangindex)*cos(-90.0*R)
            SUM2=SUM2+0.5*TMGVALUE(ibidir,ibset,1,ivangindex)
     &                  *cos(-90.0*R)
            IF (IBIDATATYPE.eq.2)then
              SUM3=SUM3+0.5*TMREFLECT(ibidir,ibset,1,ivangindex)
     &                  *cos(-90.0*R)
            ENDIF
            DO 208 ihangindex=2,36
              COSMULT1=cos(5.0*real(ihangindex-19)*R)
              SUM1=SUM1+TMTSOD(ibidir,ibset,ihangindex,ivangindex)
     &                  *COSMULT1
              SUM2=SUM2+TMGVALUE(ibidir,ibset,ihangindex,ivangindex)
     &                  *COSMULT1
              IF (IBIDATATYPE.eq.2)then
                SUM3=SUM3+TMREFLECT(ibidir,ibset,ihangindex,ivangindex)
     &                    *COSMULT1
              ENDIF
 208        CONTINUE
            SUM1=SUM1+0.5*TMTSOD(ibidir,ibset,37,ivangindex)*cos(90.0*R)
            SUM2=SUM2+0.5*TMGVALUE(ibidir,ibset,37,ivangindex)
     &                  *cos(90.0*R)
            IF (IBIDATATYPE.eq.2)then
              SUM3=SUM3+0.5*TMREFLECT(ibidir,ibset,37,ivangindex)
     &                  *cos(90.0*R)
            ENDIF
            SUM4=SUM4+SUM1*(cos(5.0*real(ivangindex-19)*R))**2
            SUM5=SUM5+SUM2*(cos(5.0*real(ivangindex-19)*R))**2
            SUM6=SUM6+SUM3*(cos(5.0*real(ivangindex-19)*R))**2
            SUM1=0.0
            SUM2=0.0
            SUM3=0.0
 207      CONTINUE

C Finally for ivangindex = 19 (i.e. solar altitude = 0deg)
          SUM1=SUM1+0.5*TMTSOD(ibidir,ibset,1,19)*cos(-90.0*R)
          SUM2=SUM2+0.5*TMGVALUE(ibidir,ibset,1,19)
     &                *cos(-90.0*R)
          IF (IBIDATATYPE.eq.2)then
            SUM3=SUM3+0.5*TMREFLECT(ibidir,ibset,1,19)
     &                *cos(-90.0*R)
          ENDIF
          DO 209 ihangindex=2,36
            COSMULT1=cos(5.0*real(ihangindex-19)*R)
            SUM1=SUM1+TMTSOD(ibidir,ibset,ihangindex,19)
     &               *COSMULT1
            SUM2=SUM2+TMGVALUE(ibidir,ibset,ihangindex,19)
     &               *COSMULT1
            IF (IBIDATATYPE.eq.2)then
              SUM3=SUM3+TMREFLECT(ibidir,ibset,ihangindex,19)
     &                 *COSMULT1
            ENDIF
 209      CONTINUE
          SUM1=SUM1+0.5*TMTSOD(ibidir,ibset,37,19)*cos(90.0*R)
          SUM2=SUM2+0.5*TMGVALUE(ibidir,ibset,37,19)*cos(90.0*R)
          IF (IBIDATATYPE.eq.2)then
            SUM3=SUM3+0.5*TMREFLECT(ibidir,ibset,37,19)*cos(90.0*R)
          ENDIF
          SUM4=SUM4+SUM1*0.5*(cos(0.0*R))**2
          SUM5=SUM5+SUM2*0.5*(cos(0.0*R))**2
          SUM6=SUM6+SUM3*0.5*(cos(0.0*R))**2
    
C Multiply by constant terms to get diffuse properties.  
          DF_TX_GND(ibidir,ibset)=SUM4*0.5/atan(1.0)*(5.0*R)**2
          DF_G_GND(ibidir,ibset)=SUM5*0.5/atan(1.0)*(5.0*R)**2
          DF_RF_GND(ibidir,ibset)=SUM6*0.5/atan(1.0)*(5.0*R)**2

C Calculate the layer absorptances for bidirectional datatypes 2 and 3
C for both the sky and ground reflected components of diffuse radiation.
C Get effective total absorptance from the interpolation of the solar
C transmittance according to a method defined by Fraunhofer.
C Do it only in cases of bidirectional datatypes 2 and 3 (g,te,rho
C or g,te only). These calculations are repeated in solar.F for the direct
C radiation at each timestep (because the transmissivity etc changes 
C according to solar azimuth and solar altitude).
        IF(IBIDATATYPE.EQ.2)THEN   
          EFF_SKY_TOT_ABS=1.0-DF_TX_SKY(ibidir,ibset)
     &                       -DF_RF_SKY(ibidir,ibset)
          EFF_GND_TOT_ABS=1.0-DF_TX_GND(ibidir,ibset)
     &                       -DF_RF_GND(ibidir,ibset)
        ELSEIF(IBIDATATYPE.EQ.3)THEN
          EFF_SKY_TOT_ABS=0.5*(1.0-DF_TX_SKY(ibidir,ibset))
          EFF_GND_TOT_ABS=0.5*(1.0-DF_TX_GND(ibidir,ibset))
        ENDIF  

C Solar flux for bidirectional data (experimental conditions)
C will be g value - transmittance
        qi_SKY_bidir=DF_G_SKY(ibidir,ibset)-DF_TX_SKY(ibidir,ibset)
        qi_GND_bidir=DF_G_GND(ibidir,ibset)-DF_TX_GND(ibidir,ibset)

C Calculate the thermal resistance of the glazing based on the U values
C Re and Ri values defined in the bidirectional data file
C Trap also the zeros in case of corrupted data (will result to
C a non-realistic Rs_glazing)
        call eclose(TUVALUE(IBIDIR,ibset),0.0,0.001,bCloseToZero)
        if(bCloseToZero)then
          Rs_glazing=0.0-TREXTERNAL(ibidir,ibset)-
     &       TRINTERNAL(ibidir,ibset)
        else
          Rs_glazing=(1.0/TUVALUE(ibidir,ibset))-
     &       TREXTERNAL(ibidir,ibset)-TRINTERNAL(ibidir,ibset)
        endif

C Calculate the effective absorptance of the inner layer (based on
C the experimental bidirectional data)
C abs_in=(qi*(Re+Rs+Ri)-abs_tot*Re)/Rs
C Trap also the zeros in case of corrupted data
        call eclose(Rs_glazing,0.0,0.001,bCloseToZero)
        if(bCloseToZero)then
          a_dfSKYinner(ibidir,ibset)=0.0
          a_dfGNDinner(ibidir,ibset)=0.0
        else
          a_dfSKYinner(ibidir,ibset)=(qi_SKY_bidir*
     &      (TREXTERNAL(ibidir,ibset)+
     &      Rs_glazing+TRINTERNAL(ibidir,ibset))-
     &      EFF_SKY_TOT_ABS*TREXTERNAL(ibidir,ibset))/Rs_glazing
          a_dfGNDinner(ibidir,ibset)=(qi_GND_bidir*
     &      (TREXTERNAL(ibidir,ibset)+
     &      Rs_glazing+TRINTERNAL(ibidir,ibset))-
     &      EFF_GND_TOT_ABS*TREXTERNAL(ibidir,ibset))/Rs_glazing
         endif

C Calculate the effective absorptance of the outer layer (based on
C the experimental bidirectional data)
C abs_out=abs_tot*(Re+Rs)-qi(Re+Rs+Ri)/Rs
C Trap also the zeros in case of corrupted data
        call eclose(Rs_glazing,0.0,0.001,bCloseToZero)
        if(bCloseToZero)then
          a_dfSKYouter(ibidir,ibset)=0.0
          a_dfGNDouter(ibidir,ibset)=0.0
        else
          a_dfSKYouter(ibidir,ibset)=
     &      (EFF_SKY_TOT_ABS*(TREXTERNAL(ibidir,ibset)+
     &      Rs_glazing)-qi_SKY_bidir*
     &      (TREXTERNAL(ibidir,ibset)+Rs_glazing+
     &      TRINTERNAL(ibidir,ibset)))/Rs_glazing
          a_dfGNDouter(ibidir,ibset)=
     &      (EFF_GND_TOT_ABS*(TREXTERNAL(ibidir,ibset)+
     &      Rs_glazing)-qi_GND_bidir*
     &      (TREXTERNAL(ibidir,ibset)+Rs_glazing+
     &      TRINTERNAL(ibidir,ibset)))/Rs_glazing
        endif

  201   CONTINUE
  200   CONTINUE
      ENDIF

c Free file.
 1000 CALL ERPFREE(IUA,ISTAT)
      RETURN
  
      end

