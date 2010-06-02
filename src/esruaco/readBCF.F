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


C************ MatIndex*************
C Using an Material name (XMat), returns its material block index in the 
C BCF file. Scan the list of Mat name (CompoNam) and compare to XMat.

C XMat (Char) : Material name to find
C CompoNam(MMLC) (Char) : List of MLC name in BCF.
C MatIndex (Integer) : Material index in the BCF file corresponding to XMat

      FUNCTION MatIndex(XMat)

#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/EIAhigh/NbComp,NbMat,IMatID(MMLC,ME),ImatDbID(0:MMAT-1),
     &               LCIATag
      COMMON/DesMLC/MatNamed(MMAT),MatDescd(MMAT),MatCatd(MMAT),
     &              CompoNam(MMLC), LayerDes(MMLC,ME)

      CHARACTER*12 XMat
      CHARACTER*72 outs
      CHARACTER*72 LayerDes, MatDescd, MatCatd
      CHARACTER*12 MatNamed, CompoNam
 
      MatIndex = 0
      DO 10 IC=1,NbMat
        IF (MatNamed(IC) .EQ. XMat) THEN
          MatIndex = IC
          GOTO 363
        ENDIF
 10   CONTINUE
363   IF (MatIndex .EQ. 0) THEN
        WRITE(outs,'(a,a)') XMat,' not found in BCF file.'
        CALL edisp(iuout,outs)
        GOTO 666
      ENDIF
666   RETURN
      END





c ******************** STRIPCB
C STRIPCB strips comments from a ASCII file string and returns the data.
C It assumes that if a string begins with a '#' then the whole line is 
C a comment an the next line is read.  If a ' #' is discovered within
C a line the rest of the line is removed. 
C Is equivalent to STRIPC but do not take into account a blank as a 
C separator.
C IER=0 if ok. MSG is a text string used in error messages. If
C IR=0 then acts silently, otherwise notes when EOF found.
C IEXP is the number of expected items in the line: 
C   IEXP = 0 means don't care or already know no. items - don't check
C   IEXP >0  means a specific number of items expected (error if not)
C   IEXP = 99 check number of items and return in ITEMS
 
      SUBROUTINE STRIPCB(INPCH,OUTSTR,IEXP,ITEMS,IR,MSG,IER)
      CHARACTER*124 tmp,STRING,OUTSTR,MSG1
      CHARACTER*(*) MSG
      logical unixok
      
      integer lnblnk  ! function definition
      integer iCountWords

C Read a line of the file, strip off any trailing blanks, if the first
C character is a # then read the next line from the file.
      IER=0
    8 READ(INPCH,10,ERR=101,END=102)STRING
   10 FORMAT(A124)
      tmp=STRING(1:LNBLNK(STRING))

C Take the string and check for a #, discarding any text which follows.
      iloc = INDEX(tmp,'#')
      if(iloc.eq.1)then
        goto 8
      elseif(iloc.eq.0)then
        OUTSTR=tmp
      elseif(iloc.gt.1)then
        OUTSTR=tmp(1:ILOC-1)
      endif

C Find out the number of separate words/string groupings.
      if(IEXP.eq.99)then
        ITEMS = iCountWords(OUTSTR)
      elseif(IEXP.eq.0)then
        ITEMS=0
      elseif(IEXP.gt.0)then
        ITEMS = iCountWords(OUTSTR)
        if(IEXP.ne.ITEMS)then
          WRITE(tmp,'(A,I3,A,A,A)')' Looking for ',IEXP,' items (',
     &       MSG(1:lnblnk(MSG)),') in...'
          CALL USRMSG(tmp,OUTSTR,'F')
         endif
      endif
      
    4 RETURN

  101 IER=1
      IF(IR.EQ.1)THEN
        WRITE(MSG1,'(A,A)',ERR=1)' Error reading: ',
     &  OUTSTR(1:lnblnk(OUTSTR))
        CALL USRMSG(MSG1,MSG,'W')
      ENDIF
      goto 4

  102 IER=2
      IF(IR.EQ.1)THEN
        WRITE(tmp,'(A,A,A)',ERR=1)' End of ',
     &    MSG(1:lnblnk(MSG)),' sensed.'
        CALL USRMSG(' ',tmp,'-')
      ENDIF
      goto 4

C Trap for I/O error.
 1    call isunix(unixok)
      if(.NOT.unixok) return  ! if DOS return because of lack of I/O channel
      write(6,*) 'STRIPCB: error writing warning re: ',OUTSTR
      return

      END


C ******************** SCNBCF ******************************
C  SCNBCF scans the building constructions file (BCF) and recorde all 
C Ecobalance values from all th constructions 

C CONSTANTS
C----------
C MIMP (INTEGER) = Number of impacts
 
C AbrevIMP(MIMP)  INTEGER: String contaning the following impacts 
C                        abreviation
C    1. NRE : Non renewable energy
C    2. GWP : Global Warming Potential
C    3. PS : Photosmog potential
C    4. AP : Acidification potential
 
C MNbTrans (INTEGER) = Maximum of different tranport category for a destination
C For inst. A Window can be transport by train then by 28t and finally 
C by 16t truck
C MNbM (INTEGER)  = Maximum of maintenance type for a construction during
C                      its lifetime
C DistMin REAL  = Minimum allowed transportation distance [km]
C DistMax REAL = Maximum allowed transportaton distance [km]
C MinImpTr(MIMP) INTEGER = Minimum impact for considered transport 
C                         type [Impact/kg*/km]
C MaxImpTr(MIMP) INTEGER = Maximum impact for considered transport
C                         type [Impact/kg*/km]
C MinImp(MIMP) INTEGER = Minimum impact for considered process [Impact/kg*]
C MaxImp(MIMP) INTEGER = Maximum impact for considered process [Impact/kg*]

C Variables list
C---------------

C BCFfile = file name of BCF
C IUC = Entrance file unit number  (IBCF)
C IER Error code
C IUIN = User input channel
C IUOUT = the message channel
C   1 = should exist, message & error -301 if not. 

C IW       (Integer) = 1 If read BCF silently (=2 if not)

C Variables lists for CONSTRUCTION manipulation 
C -----------------------------------
C IBCF     (Integer)  Unit channel of the BCFfile file
C BCFVer   (String)   Version of the BCFfile 
C TstNcom  Test if NMLC (nb of constructions in of the system) = TstNcom
C           which is read in the IBC
C BCFOK    (Logical)  TRUE if BCFFile exits
C RmStr    (String)   Remaining string in string handling (EGETRM)
C MMLC     (Integer)  Nb max of construction in a system (building.h)
C ME       (Integer)  Nb max of layers in a construction (building.h)
C IRdComp  (Integer) The current readed Construction
C IRdLay   (Integer) The current readed Layer
C NLayers  (Integer) Nb of layer readed from the BCF file.
C NbTr     (Integer) Number of current transport category

C Variables for LCA calculation
C ------------------------------

C  BuiLife REAL = Building life time [year]

C   Construction transport (*Construction_Transport)
C   ------------------- 
C For semi-finished construction such as prefabricated or window: Transport 
C impact of the assembled construction from assembly factory to 
C building site.
C For construction erected on the building site: No construction transport.
C CoTrPt(MMLC,MNbTrans) CHAR = Construction transport category
C CoTrDist(MMLC,MNbTrans) INT  = Construction transport distance [km]
C CoTrImp(MMLC,MNbTrans,MIMP) REAL = Impact for transport category [Imp/kg*/km]
C CoTrBrk(MMLC,MNbTrans)   REAL = Lost & break rate during construction  
C                               transport (0-1)[-]

C Surface construction assembly (*Construction_Assembly_Surface)
C ------------------
C Surface construction assembly LCA
C For semi-finished surface construction such as prefabricated or window: Impact 
C of the surface construction assembly in factory.
C For construction erected on the building site: None (Included in "Construction
C assembly on building" Impact) .
C ComAsPa(MMLC,MNbAss)  CHAR = P if related to perimeter part of the construction
C                       S if related to surfacic part of the construction
C ComAsPt(MMLC,MNbAss)  CHAR = Surface construction assembly pointer
C ComAsImp(MMLC,MNbAss,MIMP) REAL = NRE for surface construction assembly [Impact/kg*]
C
C Perimeter construction assembly (*Construction_Assembly_Perimeter)
C ------------------
C Perimeter construction assembly LCA
C For semi-finished perimeter construction such as prefabricated or window: Impact 
C of the perimeter construction assembly in factory (frame, spacer).
C For construction erected on the building site: None (Included in "Construction
C assembly on building" Impact) .
C AsPerPt(MMLC)  CHAR = Construction assembly pointer
C AsPerImp(MMLC,MIMP) REAL = Impact for perimeter construction assembly [Impact/kg*]

C Construction assembly on building
C ------------------------------
C Construction assembly LCA required to "add" the construction element to the
C building.
C AsProjPt(MMLC,MNbAss)  CHAR = Assembly on building pointer
C AsProImp(MMLC,MNbAss,MIMP) REAL =  for assembly on building [Impact/kg*]

C Construction maintenance
C ---------------------
C Construction maintenance during its life.
C MaintPt(MMLC,MNbM)      CHAR = Construction maintenance pointer
C MaintTyp(MMLC,MNbM)     CHAR = Construction maintenance type
C                                   - S: for construction surface
C                                   - P: for construction perimeter (e.g. frame)
C MaintPer(MMLC,MNbM)      REAL = Construction maintenance periode [year]
C RMainImp(MMLC,MNbM,MIMP) REAL = Impact for construction maintenance [Impact/m2*]

C Layer-Block Header
C ------------------
C General information on the layer
C LayerNam(MMLC,ME)  CHAR = Layer long name
C LayerCat(MMLC,ME)  CHAR = Layer category (T:thermal, B: bindings, ...)
C QuaLayer(MMLC,ME)  REAL = Layer surface rate [-]
C          If surfacic layer (f.i. pane (QuaLayer = S)): 1.0
C          If periferic layer (f.i. spacer(QuaLayer = P)): 1.0
C          If partial-surfacic layer (f.i. bindings(QuaLayer = B)): 
C                      x = percentage of current current construction recover
C FixLayer(MMLC,ME)  REAL = Layer fix dimension [m]
C    For instance, a frame or a spacer has a constant dim (height)

C Fabrication-Block 
C ------------------
C Fabrication Impact of the material at the end of the fabrication process
C FabPt(MMAT)  CHAR = Pointer to the corresponding material 
C                         fabrication LCA
C FabImp(MMAT,MIMP)  REAL = Impact required for material fabric. [Impact/kg*]
C RMatLife(MMAT)  REAL  = Layer material lifetime [year]
C AssLoss(MMAT)  REAL = Lost & break rate during material  
C                         fabrication (0-1)[-]

C Layer transport
C ----------------
C For semi-finished construction such as prefabricated or window: Transport 
C impact of the material from fabrication factory to assembly factory. 
C For construction erected on the building site:  Transport impact of the
C material from fabrication factory to building site.
C TrPt(MMAT,MNbTrans)   CHAR = Construction transport category
C TrDist(MMAT,MNbTrans) REAL  = Construction transport distance [km]
C TrBrk(MMAT,MNbTrans)  REAL = Lost & break rate during material  
C                              transport (0-1)[-]
C TrImp(MMAT,MNbTrans,MIMP)  REAL = Impact for transport [Impact/kg*/km]

C Recycling
C ---------
C Recycling rate
C RecyPt(MMAT)  CHAR = Pointer to the corresponding recycling impacts
C RecyRate(MMAT)  REAL = Recycling rate of the material (0-1)[-]
C RecyImp(MMAT,MIMP)  REAL = Impact required for material recycling [Impact/kg*]
C
C Transport of recycled material
C -----------------------------
C Impacts due to the recycled material transport .
C The journey correspond from the building site to the recycling site.
C ReTrPt(MMAT,MNbTrans)   CHAR = Recycling transport category
C ReTrDist(MMAT,MNbTrans) INT  = Recycling transport distance [km]
C ReTrImp(MMAT,MNbTrans,MIMP)  REAL = Impact for transport [Impact/kg*/km]

C Incineration process
C --------------------
C Incineration impact of the material after deconstruction
C BurnPt(MMAT)  CHAR = Pointer to the corresponding incineration impacts
C BurnRate(MMAT) REAL = Burning rate of the material (0-1)[-]
C BurnImp(MMAT,MIMP)  REAL = Impact required for material burning [Impact/kg*]

C Transport to the incineration site
C ----------------------------------
C Impacts due to the material transport after building deconstruction.
C The journey correspond from the building site to the material 
C incineration site.
C BuTrPt(MMAT,MNbTrans)   CHAR = Construction transport category
C BuTrDist(MMAT,MNbTrans) INT  = Construction transport distance [km]
C BuTrImp(MMAT,MNbTrans,MIMP)  REAL = Impact for transport [Impact/kg*/km]

C Dump process
C --------------------
C Dump impact of the material after deconstruction
C DumpPt(MMAT)  CHAR = Pointer to the corresponding dump impact
C DumpRate(MMAT) REAL = Dump rate of the material (0-1)[-]
C DumpImp(MMAT,MIMP)  REAL = Impact required to dump material [Impact/kg*]
C
C Transport to the dump site
C ----------------------------------
C Impacts due to the material transport after building deconstruction.
C The journey correspond from the building site to the material 
C dump site.
C DuTrPt(MMAT,MNbTrans)   CHAR = Construction transport category
C DuTrDist(MMAT,MNbTrans) INT  = Construction transport distance to dump site [km]
C DuTrImp(MMAT,MNbTrans,MIMP)  REAL = Impact for transport to 
C                                      dump site [Impact/kg*/km]
C 
C ic Inetger: dummy variable use to skip "None" in list (transports, etc.)


      SUBROUTINE SCNBCF(IUC,BCFfile,IER)
#include "building.h"
#include "LCA.h"
#include "acoustic.h"
#include "esprdbfile.h"
#include "material.h"
      
      integer lnblnk  ! function definition


      COMMON/OUTIN/IUOUT,IUIN
      COMMON/C21/IFCFG,cfgroot,LCFGF

      COMMON/EVGENS/BCFName,System,BCFVer
      COMMON/EVGENL/BCFOK

      COMMON/EIAScnPt/FabPt(MMAT),TrPt(MMAT,MNbTrans),
     &                RecyPt(MMAT),ReTrPt(MMAT,MNbTrans),
     &                BurnPt(MMAT),BuTrPt(MMAT,MNbTrans),
     &                DumpPt(MMAT),DuTrPt(MMAT,MNbTrans),
     &                ComAsPt(MMLC,MNbAss),CoTrPt(MMLC,MNbTrans),
     &                MaintTyp(MMLC,MNbM),MaintPt(MMLC,MNbM),
     &                ComAsPa(MMLC,MNbAss),AsProjPt(MMLC,MNbAss),
     &                AsProjPa(MMLC,MNbAss),LayerCat(MMLC,ME),
     &                MainPart(MMLC,MNbM),LayerNam(MMLC,ME),
     &                LayerTyp(MMLC,ME)

      COMMON/EIAImp/FabImp(MMAT,MIMP),TrImp(MMAT,MNbTrans,MIMP),
     &          RecyImp(MMAT,MIMP),ReTrImp(MMAT,MNbTrans,MIMP),
     &          BurnImp(MMAT,MIMP),BuTrImp(MMAT,MNbTrans,MIMP),
     &          DumpImp(MMAT,MIMP),DuTrImp(MMAT,MNbTrans,MIMP),
     &          ComAsImp(MMLC,MNbAss,MIMP),CoTrImp(MMLC,MNbTrans,MIMP),
     &          AsProImp(MMLC,MNbAss,MIMP), RMainImp(MMLC,MNbM,MIMP)

      COMMON/EIADist/TrDist(MMAT,MNbTrans), CoTrDist(MMLC,MNbTrans),
     &                 ReTrDist(MMAT,MNbTrans), BuTrDist(MMAT,MNbTrans),
     &                 DuTrDist(MMAT,MNbTrans)

      COMMON/EIAMisc/NbEIALay(MMLC),ProjLife, RMatLife(MMAT),
     &               PerMaint(MMLC,MNbM),RecyRate(MMAT),BurnRate(MMAT),
     &               DumpRate(MMAT),AssLoss(MMAT),TrBrk(MMAT,MNbTrans),
     &               AsProBrk(MMAT,MNbTrans),CoTrBrk(MMLC,MNbAss),
     &               QuaLayer(MMLC,ME),FixLayer(MMLC,ME),DenLay(MMAT),
     &               SpecData(MMAT,2), NRi(MMLC,ME)

      COMMON/EIAhigh/NbComp,NbMat,IMatID(MMLC,ME),ImatDbID(0:MMAT-1),
     &               LCIATag

      COMMON/DesMLC/MatNamed(MMAT),MatDescd(MMAT),MatCatd(MMAT),
     &              CompoNam(MMLC), LayerDes(MMLC,ME)

      COMMON/AbsoCoef/SysAbsCo(MMLC,MNbAbs),PubAbsCo(MaxPub,MNbAbs),
     &                FurAbsCo(MaxPub,MNbAbs),AirAbs(MNbAbs)

      COMMON/SysTre/SyTrevPt(MMLC),SyTrevDe(MMLC),AbsUnit(MMLC)
      COMMON/ABREVImp/AbrevIMP(MIMP)

      LOGICAL BCFOK, ACOOK

      CHARACTER BCFfile*72,WORD*30,OUTSTR*124,RmStr*124,outs*174
      CHARACTER LCFGF*72,cfgroot*24,CompDes*74,EOFT*4
      CHARACTER BCFName*72, System*72,BCFVer*72
      character Tmpstr*24
      CHARACTER SyTrevPt*12, SyTrevDe*40, AbsUnit*5, AbsFreq(MNbAbs)*9
      CHARACTER*12 FabPt,TrPt,RecyPt,ReTrPt,BurnPt,BuTrPt,DumpPt,DuTrPt
      CHARACTER*12 ComAsPt,CoTrPt,MaintPt,AsProjPt
      CHARACTER*1 ComAsPa,MaintTyp,LayerCat,MainPart,AsProjPa,LayerTyp
      CHARACTER*72 LayerDes, MatDescd, MatCatd
      CHARACTER*12 MatNamed, CompoNam
      CHARACTER*12 LayerNam
      CHARACTER*1 LayerThm(MMLC,MMAT)
C Temporary values
      INTEGER TmpInt
      REAL TmpReal

      REAL DistMax
      PARAMETER (DistMax = 10000.)
      REAL DistMin
      PARAMETER (DistMin = 0.)

C Declare constant arrays
      REAL MaxImpTr(MIMP),MinImpTr(MIMP),MaxImp(MIMP),MinImp(MIMP)
      CHARACTER*4 AbrevImp

C Initialiastion
      DATA MaxImpTr /100.,100.,100.,100./
      DATA MinImpTr /0.,0.,0.,0./
      DATA MaxImp /10000.,10000.,10000.,10000./
      DATA MinImp /0.,0.,0.,0./

      do 144 I=1,ME
        NbEIALay(I)=0
 144  continue

      do 146 J=1,ME
        do 145 I=1,MMLC
          IMatID(I,J)=0
 145    continue
 146  continue

      BCFOK=.FALSE.
      ACOOK=.FALSE.

      IER=0

C Specify the reading mode.
      CALL EASKAB(' When reading in the BCF file, do you',
     &  ' want it:','done silently','with reporting of contents',IW,0)
      CALL EFOPSEQ(IUC,BCFfile,1,IER)
      IF (IER.LT.0) then
        WRITE (outs,'(a,a)')' Error while opening BCF file : ',
     &                             BCFfile(1:lnblnk(BCFfile))
        CALL EDISP(Iuout,Outs)
        IER=1
        RETURN
      else
        BCFOK = .TRUE.
      endif
      
      IF (BCFOK) then

        OUTSTR =' '
C Material, Layer and Construction counting
        IRdMat = 0
        IRdLay = 1
        IRdComp = 0
        NbMat = 1
        NbComp = 1
        NLayers = 0
C Define impacts abreviated names
        AbrevIMP(1)='GWP'
        AbrevIMP(2)='AP'
        AbrevIMP(3)='POPC'
        AbrevIMP(4)='NRE'

C Define absorption frequencies
        AbsFreq(1)='100 [Hz]'
        AbsFreq(2)='125 [Hz]'
        AbsFreq(3)='160 [Hz]'
        AbsFreq(4)='200 [Hz]'
        AbsFreq(5)='250 [Hz]'
        AbsFreq(6)='315 [Hz]'
        AbsFreq(7)='400 [Hz]'
        AbsFreq(8)='500 [Hz]'
        AbsFreq(9)='630 [Hz]'
        AbsFreq(10)='800 [Hz]'
        AbsFreq(11)='1000 [Hz]'
        AbsFreq(12)='1250 [Hz]'
        AbsFreq(13)='1600 [Hz]'
        AbsFreq(14)='2000 [Hz]'
        AbsFreq(15)='2500 [Hz]'
        AbsFreq(16)='3150 [Hz]'
        AbsFreq(17)='4000 [Hz]'

C Initialise the layer related data, for each mlc and each layer in.
        DO 4 I=1,MMLC
          DO 3 J=1,ME
            LayerNam(I,J)='none'
            LayerCat(I,J)=' '
            QuaLayer(I,J)=0.0
            FixLayer(I,J)=0.0
 3        continue 
 4      continue

C Initialise the construction transport related data
        DO 12 I=1,MMLC
          DO 13 K=1,MNbTrans
            CoTrPt(I,K)='none'
            CoTrDist(I,K)=0
            CoTrBrk(I,K)=0.0
            DO 14 L=1,MIMP
              CoTrImp(I,J,L)=0.0
 14         continue
 13       continue

C Initialise the construction maintenance + Assembly on building related data
          DO 15 K=1,MNbM
            MaintPt(I,K)='none'
            AsProjPt(I,K)='none'
            MaintTyp(I,K)='N'
            PerMaint(I,K)=0.0
            DO 16 L=1,MIMP
              RMainImp(I,K,L)=0.0
 16         continue
 15       continue
 12     continue 

C Initialise the material related data and hygrothermal properties

        DO 17 J=1,MMAT
          MatNamed(J)='UNKNOWN'
          MatDescd(J)='UNKNOWN'
          MatCatd(J)='UNKNOWN'
C          RLayeDen(J)=0.0
          DenLay(J)=0.0
          RMatLife(J)=0.0
          FabPt(J)='none'
          AssLoss(J)=0.0
          MatNamed(J)='UNKNOWN'
          DO 177 M=1,2
            SpecData(J,M)=0.0
177       CONTINUE
          RecyPt(J)='none'
          RecyRate(J)=0
          BurnPt(J)='none'
          BurnRate(J)=0.0
          DumpPt(J)='none'
          DumpRate(J)=0.0
          DO 18 K=1, MNbTrans           
            TrPt(J,K)='none'
            TrDist(J,K)=0.0
            TrBrk(J,K)=0.0
            ReTrPt(J,K)='none'
            ReTrDist(J,K)=0.0
            BuTrPt(J,K)='none'
            BuTrDist(J,K)=0.0
            DuTrPt(J,K)='none'
            DuTrDist(J,K)=0.0
            DO 19 L=1,MIMP
              TrImp(J,K,L)=0.0
              ReTrImp(J,K,L)=0.0
              BuTrImp(J,K,L)=0.0
              DuTrImp(J,K,L)=0.0
 19         continue
 18       continue
 17     continue

C Initialise the material impacts related data
        DO 20 J=1,MMAT
          DO 21 L=1,MIMP
            FabImp(J,L)=0.0
            BurnImp(J,L)=0.0
            DumpImp(J,L)=0.0
 21       continue
 20     continue


C Initialise the material impacts related data
        DO 25 I=1,MMLC
          DO 26 K=1,MNbAss
            ComAsPt(I,K)='none'
 26       continue
          DO 27 K=1,MNbTrans
            CoTrPt(J,K)='none'
            CoTrDist(J,K)=0.0
 27       continue
 25     continue

C Initialise the reverberation time related data
        DO 22 I=1,MMLC
            SyTrevPt(I)='none'
            DO 24 L=1,MNbAbs
              SysAbsCo(I,L)=0.0
 24         continue
 22     continue
C        CALL EDISP(Iuout,' Variables initialisation completed.')
C End initialisation
      endif

      CALL USRMSG(' ',' ','-')

C Start reading the BCF file
C      CALL EDISP(Iuout,' Start reading data.')
      CALL STRIPC(IUC,OUTSTR,99,ND,1,'BCF',IER)
      if(outstr(1:22).ne.'*Project_constructions')then
        call usrmsg(' ',
     &    'Opening line not bcf file. Shoud be:*Project_constructions'
     &     ,'W')
        goto 666
      endif

C Read BCF header : version, date, system scope, lifetime information.
  42  CALL STRIPC(IUC,OUTSTR,99,ND,1,'BCF',IER)
      IF (IW.EQ.2) call edisp(iuout,OUTSTR) 
      IF (OUTSTR(1:8).eq.'*Version') then
        IF ((IER .GE. 1) .OR. (ND.NE.2)) THEN
C          CALL ErrArg(0,0,'Version',1,ND-1,'BCF_header',OutStr)
          GOTO 666         
        ENDIF
        K=8
        CALL EGETW(OUTSTR,K,BCFVer,'W','version number',IER)
        IF (IER .GE. 1) goto 666
        goto 42 
      elseif (OUTSTR(1:7).eq.'*System') then
        IF ((IER .GE. 1) .OR. (ND.NE.2)) THEN
C          CALL ErrArg(0,0,'System name',1,ND-1,OUTSTR(1:7),OutStr)
          GOTO 666         
        ENDIF
        K=7
        CALL EGETW(OUTSTR,K,System,'W','loaded system',IER)
        IF (IER .GE. 1) goto 666 
C if bcf file does not correspond to loaded project
        IF (System(1:lnblnk(System)) .NE.
     &                                LCFGF(1:lnblnk(LCFGF))) then
          call edisp(iuout,' ')
          write(outs,'(a,a)')'BCF file does not correspond to ',
     &       LCFGF(:lnblnk(LCFGF))
          call edisp(iuout,outs) 
          call edisp(iuout,' ')
          IER = 1
          RETURN           
        else     
          goto 42 
        endif
      elseif (OUTSTR(1:5).eq.'*Date') then
C << Do something with balance of date line. >>
        goto 42 
      elseif (OUTSTR(1:6).eq.'*Scope') then
C << Do something with balance of scope line. >>
        goto 42 
      elseif (OUTSTR(1:9).eq.'*Lifetime') then
        IF ((IER .GE. 1) .OR. (ND.NE.2)) THEN
C         CALL ErrArg(0,0,'Lifetime period ',1,ND-1,OUTSTR(1:9),OutStr)
          GOTO 666         
        ENDIF
        K=10
        CALL EGETWR(OUTSTR,K,ProjLife,1.0,1000.0,'W',
     &                                    'Building life time',IER)
        IF (IER .GE. 1) goto 666 
        goto 42 
      elseif (OUTSTR(1:7).eq.'*Nb_mat') then
C Read number of materials and constructions.
        IF ((IER .GE. 1) .OR. (ND.NE.2)) THEN
C          CALL ErrArg(0,0,'Number of material(s) ',1,ND-1,OUTSTR(1:7),
C     &                OutStr)
          GOTO 666         
        ENDIF
        K=8
        CALL EGETWI(OUTSTR,K,NBMat,1,MMAT,'W','Nb materials',IER)
        IF (IER .GE. 1) goto 666 
        goto 42 
      elseif (OUTSTR(1:8).eq.'*Nb_comp') then
        IF ((IER .GE. 1) .OR. (ND.NE.2)) THEN
C          CALL ErrArg(0,0,'Number of construction(s) ',1,ND-1,
C     &                OUTSTR(1:8),OutStr)
          GOTO 666         
        ENDIF
        K=9
        CALL EGETWI(OUTSTR,K,NbComp,1,MMLC,'W',
     &              'Nb of constructions',IER)
        IF (IER .GE. 1) goto 666
C        call edisp(iuout,'Read Material(s) block') 
        goto 2          
C End BCF header
      endif

C Main data reading loop trough material block
C      IF (IRdmat.EQ.1) THEN
C        call edisp(iuout,'Read Material(s) block')
C      ENDIF

  2   CALL STRIPC(IUC,OUTSTR,99,ND,1,
     &                 'materials data loop',IER)
      IF (IW.EQ.2) call edisp(iuout,OUTSTR) 
      IF(IER.NE.0) goto 1
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','tag',IFLAG)

      if (outstr(1:9).eq.'*Material') then
        IRdMat = IRdMat + 1
        IF (IRdMat .EQ. NbMat + 1) goto 44
        K=9
        CALL EGETP(OUTSTR,K,MatCatd(IRdMat),'W',
     &                                'material category',IER)
        IF (IER .GE. 1) goto 666 
        CALL EGETW(OUTSTR,K,MatNamed(IRdMat),'W','material name',
     &                                             IER)
        IF (IER .GE. 1) goto 666 
        CALL EGETP(OUTSTR,K,MatDescd(IRdMat),'W',
     &                                'material description',IER)
        IF (IER .GE. 1) goto 666 
        goto 2

C Test if end of material information and jump to constructions code.
      ELSEIF (WORD(1:13) .EQ. '*End_Material') then
        goto 44
      elseif (WORD(1:8).eq.'*Domains') then
        goto 2
      elseif (WORD(1:8).eq.'*Thermal') then
        IF ((IER .GE. 1) .OR. (ND .NE. 12)) THEN
C          CALL ErrArg(0,IRdMat,MatNamed(IRdMat),1,ND-1,WORD(1:8),OutStr)
          GOTO 666         
        ENDIF
C Thermophysical data. <<Jon >> real variable names
        if(ND.gt.9)then
          K=8
          CALL EGETW(OutStr,K,Tmpstr,'W','Mat. name',IER)
          IF (IER.GE.1) goto 666 
          CALL EGETWR(OUTSTR,K,TmpReal,0.0,200.0,
     &                'W','layer conductibility',IER)
          IF (IER .GE. 1) goto 666 
          CALL EGETWR(OUTSTR,K,DenLay(IRdMat),0.0,10000.,
     &                'W','layer density',IER)
          IF (IER .GE. 1) goto 666 
          CALL EGETWR(OUTSTR,K,TmpReal,0.0,10000.0,
     &                'W','layer Cp',IER)
          IF (IER .GE. 1) goto 666 
          CALL EGETWR(OUTSTR,K,TmpReal,0.0,1.0,
     &                'W','emissivity',IER)
          IF (IER .GE. 1) goto 666 
          CALL EGETWR(OUTSTR,K,TmpReal,0.0,1.0,
     &                'W','layer solar absoption',IER)
          IF (IER .GE. 1) GOTO 666 
          CALL EGETWR(OUTSTR,K,TmpReal,0.0,20000.0,
     &                'W','diffuse resistance',IER)
          IF (IER .GE. 1) GOTO 666 
          CALL EGETWR(OUTSTR,K,TmpReal,0.0,1.0,
     &                'W','roughness',IER)
          IF (IER .GE. 1) GOTO 666 
          CALL EGETWR(OUTSTR,K,TmpReal,0.0,1.0,
     &                'W','specularity',IER)
          IF (IER .GE. 1) GOTO 666 
          CALL EGETWR(OUTSTR,K,TmpReal,0.0,1.0,
     &                'W','data1',IER)
          IF (IER .GE. 1) GOTO 666 
          CALL EGETWR(OUTSTR,K,TmpReal,0.0,1.0,
     &                'W','data2',IER)
          IF (IER .GE. 1) GOTO 666 
          goto 2
        endif
      elseif (OUTSTR(1:9).eq.'*Acou_Mat') then
C << Not necessary >>
        CALL STRIPC(IUC,OUTSTR,99,ND,1,'BCF:Acou_MAT',IER)
        goto 2 

      elseif (WORD(1:12).eq.'*Mat_effects') then
C Material Fabrication tag
        K=12
        NbTr=0
        CALL EGETW(OutStr,K,FabPt(IRdMat),'W','Mat pointer',IER)
        IF (IER.GE.1) goto 666 
        IF (FabPt(IRdMat)(1:4).NE.'none') THEN 
          IF ((IER .GE. 1) .OR. (ND.NE.7)) THEN
C          CALL ErrArg(1,IRdMat,MatNamed(IRdMat),MIMP+7,ND,WORD(1:12),
C     &                OutStr)
           GOTO 666         
          ENDIF
          CALL EGETWI(OUTSTR,K,NbTr,0,MNbTrans,'W',
     &         'Nb of transportation to construction site',IER)
          IF (IER .GE. 1) goto 666 
          CALL EGETWR(OutStr,K,Rtmp,0.,1000.,'W','lifetime',IER)
          RMatLife(IRdMat)=Rtmp
          IF (IER.GE.1) goto 666 
          CALL EGETWR(OutStr,K,Rtmp,0.0,1.0,'W',
     &                  'mat loss during assembly stage',IER)
          AssLoss(IRdMat)=Rtmp
          IF (IER.GE.1) goto 666 
          CALL EGETWR(OutStr,K,Rtmp,0.0,1000.0,'W',
     &                  'mat special data 1',IER)
          SpecData(IRdMat,1)=Rtmp
          IF (IER.GE.1) goto 666 
          CALL EGETWR(OutStr,K,Rtmp,0.0,1000.0,'W',
     &                  'mat special data 2',IER)
          SpecData(IRdMat,2)=Rtmp
          IF (IER.GE.1) goto 666 
          CALL STRIPC(IUC,OUTSTR,99,ND,1,'BCF',IER)
          IF ((IER .GE. 1) .OR. (ND.NE.MIMP)) THEN
            CALL EDISP(Iuout,' Error in material impacts.')
            goto 666
          ELSE
            K=0
            DO 421 L=1,MIMP
              CALL EGETWR(OutStr,K,Rtmp,MinImp(L),MaxImp(L),'W'
     &                      ,outs,IER)
              FabImp(IRdMat,L)=Rtmp
              IF (IER.GE.1) goto 666 
421         continue
          ENDIF
        ELSE
        ENDIF

C read transport(s) to construction site
        IF (NbTr.GT.0) THEN           
          Ic = 0
          DO 422 IT=1,NbTr
            CALL STRIPC(IUC,OUTSTR,99,ND,1,'BCF',IER)
            IF (OutStr(1:19).NE.'*Tran_assembly_site') THEN 
              CALL EDISP(Iuout,'Error at *Tran_assembly_site tag.')
              goto 666
            ENDIF
            if ((IER .GE. 1) .OR. (ND.NE.4+MIMP)) then
              CALL EDISP(Iuout,
     &           'Error in *Tran_assembly_site impacts nb.')
              goto 666
            else
              K = 19
              IF (OutStr(1:4).NE.'none') THEN 
                Ic = Ic+1
                CALL EGETW(OutStr,K,TrPt(IRdMat,Ic),'W',
     &               'Transport category to assembly site',IER)
                IF (IER.GE.1) goto 666 
                CALL EGETWR(OutStr,K,TrDist(IRdMat,Ic),DistMin,
     &           DistMax,'W','transport distance to assembly site',IER)
                IF (IER.GE.1) goto 666 
                CALL EGETWR(OutStr,K,TrBrk(IRdMat,Ic),0.,1.,'W',
     &          'Break & lost rate during transp to assembly site',IER)
                IF (IER.GE.1) goto 666 
                DO 522 L=1,MIMP
                  WRITE(outs,'(a,a)')AbrevIMP(L),
     &                    'impacts of transport to assembly site'
                  CALL EGETWR(OutStr,K,TrImp(IRdMat,Ic,L),
     &                         MinImpTr(L), MaxImpTr(L),'W',outs,IER)
                  IF (IER.GE.1) goto 666 
522             continue
              ENDIF
            endif
422       continue
        ENDIF
        goto 2

      elseif (WORD(1:10).eq.'*Dec_Recyc') then
C Recycling stage
        K=10
        NbTr = 0
        CALL EGETW(OutStr,K,RecyPt(IRdMat),'W','Recy pointer',IER)
        IF (IER.GE.1) goto 666 
        IF (RecyPt(IRdMat)(1:4).NE.'none') THEN
          IF ((IER .GE. 1) .OR. (ND.NE.4+MIMP)) THEN
C           CALL ErrArg(1,IRdMat,MatNamed(IRdMat),4+MIMP,ND,WORD(1:14),
C     &                OutStr)
            GOTO 666         
          ENDIF
          CALL EGETWI(OUTSTR,K,NbTr,0,MNbTrans,'W',
     &         'Nb of transportation to recycling site',IER)
          IF (IER .GE. 1) goto 666 
          CALL EGETWR(OutStr,K,RecyRate(IRdMat),0.,1.,'W',
     &               'Recycling rate',IER)
          IF (IER.GE.1) goto 666 
          DO 423 L=1,MIMP
            WRITE(outs,'(a,a)')AbrevIMP(L),' for material fabrication'
            CALL EGETWR(OutStr,K, RecyImp(IRdMat,L),
     &                         MinImpTr(L), MaxImpTr(L),'W',outs,IER)
            IF (IER.GE.1) goto 666
423       continue
        ENDIF

C Read transport to recycling site
        IF (NbTr.GT.0) THEN 
          Ic = 0
          DO 424 IT=1,NbTr
            CALL STRIPC(IUC,OutStr,99,ND,1,'BCF',IER)
            IF (OutStr(1:11).NE.'*Tran_Recyc') THEN 
              CALL EDISP(Iuout,'Error at *Tran_recyc tag.')
              goto 666
            ENDIF
            if ((IER .GE. 1) .OR. (ND.NE.3+MIMP)) then
              CALL EDISP(Iuout,'Error in *Tran_Recyc impacts nb.')
              goto 666
            else
              K=11
              IF (OutStr(1:4).NE.'none') THEN 
                Ic = Ic+1
                CALL EGETW(OutStr,K,ReTrPt(IRdMat,Ic),'W',
     &               'pointer transport to recycling site',IER)
                IF (IER.GE.1) goto 666
                CALL EGETWR(OutStr,K,ReTrDist(IRdMat,Ic),DistMin,
     &           DistMax,'W','transport distance to recyc. site',IER)
                IF (IER.GE.1) goto 666 
                DO 524 L=1,MIMP
                  WRITE(outs,'(a,a)')AbrevIMP(L),' for recycling stage'
                  CALL EGETWR(OutStr,K,ReTrImp(IRdMat,Ic,L),
     &                         MinImpTr(L), MaxImpTr(L),'W',outs,IER)
                  IF (IER.GE.1) goto 666 
524             continue
              ENDIF
            endif
424       continue
        ENDIF
        goto 2

      elseif (WORD(1:10).eq.'*Dec_Incin') then
C Incineration process.
        K=10  
        NbTr = 0
        CALL EGETW(OutStr,K,BurnPt(IRdMat),'W',
     &                    'incineration pointer',IER)
        IF (IER.GE.1) goto 666 
        IF (BurnPt(IRdMat)(1:4).NE.'none') THEN
          IF ((IER .GE. 1) .OR. (ND .NE. 4+MIMP)) THEN
C          CALL ErrArg(1,IRdMat,MatNamed(IRdMat),4+MIMP,ND,WORD(1:11),
C     &                 OutStr)
          GOTO 666         
          ENDIF
          CALL EGETWI(OUTSTR,K,NbTr,0,MNbTrans,'W',
     &         'Nb of transportation to incineration site',IER)
          IF (IER .GE. 1) goto 666 
          CALL EGETWR(OutStr,K,BurnRate(IRdMat),0.,1.,'W',
     &                              'Incineration rate',IER)
          IF (IER.GE.1) goto 666 
          DO 425 L=1,MIMP
           WRITE(outs,'(a,a)')AbrevIMP(L),' for material fabrication'
            CALL EGETWR(OutStr,K,BurnImp(IRdMat,L),
     &                         MinImpTr(L), MaxImpTr(L),'W',outs,IER)
            IF (IER.GE.1) goto 666
425       CONTINUE
        ENDIF

C Read transport to incineration site
        IF (NbTr.GT.0) THEN 
          Ic = 0
          DO 426 IT=1,NbTr
            CALL STRIPC(IUC,OutStr,99,ND,1,'incin. transport',IER)
            IF (OutStr(1:11).NE.'*Tran_Incin') THEN 
              CALL EDISP(Iuout,'Error at *Tran_Incin tag.')
              goto 666
            ENDIF
            if ((IER .GE. 1) .OR. (ND.NE.3+MIMP)) then
              CALL EDISP(Iuout,'Error in *Tran_Incin impacts nb.')
              goto 666
            else
              K=11
              IF (OutStr(1:4).NE.'none') THEN 
                Ic = Ic+1
                CALL EGETW(OutStr,K,BuTrPt(IRdMat,Ic),'W',
     &               'transport category to incineration site',IER)
                IF (IER.GE.1) goto 666 
                CALL EGETWR(OutStr,K,BuTrDist(IRdMat,Ic),DistMin,
     &           DistMax,'W','transport distance to incineration site',
     &            IER)
                IF (IER.GE.1) goto 666 
                DO 526 L=1,MIMP
                  WRITE(outs,'(a,a)')AbrevIMP(L),' for recycling stage'
                  CALL EGETWR(OutStr,K,BuTrImp(IRdMat,Ic,L),
     &                         MinImpTr(L), MaxImpTr(L),'W',outs,IER)
                  IF (IER.GE.1) goto 666 
526             continue
              ENDIF
            endif
426       continue
        ENDIF
        goto 2

      elseif (WORD(1:9).eq.'*Dec_Dump') then
C Dump process
        K=9 
        NbTr = 0
        CALL EGETW(OutStr,K,DumpPt(IRdMat),'W','dump pointer',IER)
        IF (IER.GE.1) goto 666 
        IF (DumpPt(IRdMat)(1:4).NE.'none') THEN 
          IF ((IER .GE. 1) .OR. (ND.NE.4+MIMP)) THEN
C Check consistancy
C          CALL ErrArg(1,IRdMat,MatNamed(IRdMat),MIMP+2,ND,WORD(1:11),
C     &                OutStr)
            GOTO 666         
          ENDIF
          CALL EGETWI(OutStr,K,NbTr,0,MNbTrans,'W',
     &       'Nb of transportation categories to dump site',IER)
          IF (IER.GE.1) goto 666 
          CALL EGETWR(OutStr,K,DumpRate(IRdMat),0.,1.,'W',
     &                    'dump rate',IER)
          IF (IER.GE.1) goto 666 
          DO 427 L=1,MIMP
            WRITE(outs,'(a,a)')AbrevIMP(L),' for material fabrication'
            CALL EGETWR(OutStr,K,DumpImp(IRdMat,L),
     &                         MinImp(L), MaxImp(L),'W',outs,IER)
            IF (IER.GE.1) goto 666
427       CONTINUE
        ENDIF

C Check consitency for disposal rate
       TotRate = RecyRate(IRdMat)+BurnRate(IRdMat)+DumpRate(IRdMat)
       IF (NINT(TotRate) .NE. 1) THEN 
         WRITE(outs,'(a,a)')'For material : ', MatNamed(IRdMat)
         CALL edisp(iuout,outs)
         CALL EDISP(iuout,' The sum of dispoal rates is not = 1 !')
         WRITE(outs,'(a,F6.2)')' Recycling rate    = ', RecyRate(IRdMat)
         CALL edisp(iuout,outs)
         WRITE(outs,'(a,F6.2)')' Incineration rate = ', BurnRate(IRdMat)
         CALL edisp(iuout,outs)
         WRITE(outs,'(a,F6.2)')' Dump rate         = ', DumpRate(IRdMat)
         CALL edisp(iuout,outs)
         CALL EDISP(iuout,'                  --------')
         WRITE(outs,'(a,F6.2)')' Total             = ', TotRate
         CALL edisp(iuout,outs)
         goto 666 
       ENDIF

C Read transport to dump site
        IF (NbTr.GT.0) THEN 
          Ic = 0
          DO 428 IT=1,NbTr
            CALL STRIPC(IUC,OutStr,99,ND,1,'BCF',IER)

            IF (OutStr(1:10).NE.'*Tran_Dump') THEN 
              CALL EDISP(Iuout,'Error at *Tran_Dump tag.')
              goto 666
            ENDIF
            if ((IER .GE. 1) .OR. (ND.NE.3+MIMP)) then
              CALL EDISP(Iuout,'Error in *Tran_Dump impacts nb.')
              goto 666
            else
              K=10
              IF (OutStr(1:4).NE.'none') THEN 
                Ic = Ic+1
                CALL EGETW(OutStr,K,DuTrPt(IRdMat,Ic),'W',
     &               'transport category to dump site',IER)
                IF (IER.GE.1) goto 666 
                CALL EGETWR(OutStr,K,DuTrDist(IRdMat,Ic),DistMin,
     &            DistMax,'W','transport distance to dump site',IER)
                IF (IER.GE.1) goto 666 
                DO 528 L=1,MIMP
                  WRITE(outs,'(a,a)')AbrevIMP(L),' for trans.dump stage'
                  CALL EGETWR(OutStr,K,DuTrImp(IRdMat,Ic,L),
     &                         MinImpTr(L), MaxImpTr(L),'W',outs,IER)
                  IF (IER.GE.1) goto 666 
528             continue
              ENDIF
            endif
428       continue
        ENDIF
        goto 2
      endif

C CONSTRUCTIONS INFORMATION
C ----------------------
  44  continue
      CALL STRIPCB(IUC,OUTSTR,99,ND,1,'constructions data loop',
     &                                                             IER)
      IF (IW.EQ.2) call edisp(iuout,OUTSTR) 
      IF(IER.NE.0) goto 1
      K=0
      CALL EGETW(OUTSTR,K,WORD,'W','tag',IFLAG)

C Test if end of BCF file
      IF (WORD(1:8) .EQ. '*End_BCF') then
          GOTO 777         
C Test if end of construction information
      ELSEIF (WORD(1:17) .EQ. '*End_Construction') then
        IF ((IER .GE. 1) .OR. (ND.NE.1)) THEN
C         CALL ErrArg(2,IRdComp,CompoNam(IRdComp),1,ND,WORD(1:14),OutStr)
C          GOTO 666         
        ENDIF
        GOTO 777
      ENDIF
      
      if (WORD(1:11).eq.'*Gen_constr') then
        IRdComp = IRdComp + 1
        if (IRdComp .EQ. NbComp + 1) then
C Read is finished
          goto 777
        else
C Go to read another construction.
C          goto 44
        endif

        IF ((IER .GE. 1) .OR. (ND.NE.5)) THEN
C         CALL ErrArg(2,IRdComp,CompoNam(IRdComp),1,ND-1,WORD(1:23),
C     &                 OutStr)
          GOTO 666 
        ENDIF        
        CALL EGETRM(OUTSTR,K,RmStr,'W','General info.',IER)     
        IF (IER .GE. 1) goto 666 
        K=0
C << JON>> define variable names
        CALL EGETWI(RmStr,K,TmpInt,1,MMLC,'W','comp. ID',IER)
        IF (IER .GE. 1) goto 666 
        IF (TmpInt .NE. IRdComp) then
          call edisp(iuout,' ')
          write(outs,'(a,a)')'Construction numbering error in ',
     &          BCFfile(:lnblnk(BCFfile))
          call edisp(iuout,outs) 
          write(outs,'(a,I4)')'At construction no.: ',IRdComp
          call edisp(iuout,outs) 
          write(outs,'(a,I4)')'Layer no.: ',IRdLay
          call edisp(iuout,outs) 
          call edisp(iuout,' ')         
          goto 666
        endif

        CALL EGETW(RmStr,K,CompoNam(IRdComp),'W',
     &             'construction name',IER)
        
        CALL EGETP(RmStr,K,CompDes,'W','construction descr.',IER)
        CALL EGETW(RmStr,K,EOFT,'W','construction type',IER)
        goto 44

      elseif (WORD(1:6).eq.'*Solar') then
        goto 44
      elseif (WORD(1:7).eq.'*Optics') then
        CALL EGETW(RmStr,K,Tmpstr,'W','construction type',IER)
        goto 44
      elseif (WORD(1:14).eq.'*Colour_inside') then
        CALL STRIPCB(IUC,OUTSTR,99,ND,1,'Colour inside',IER)
        goto 44
      elseif (WORD(1:13).eq.'*Colour_other') then
        CALL STRIPCB(IUC,OUTSTR,99,ND,1,'Colour other',IER)
        goto 44
      elseif (WORD(1:11).eq.'*Acou_other') then
        CALL STRIPCB(IUC,OutStr,99,ND,1,'BCF',IER)
        goto 44
      elseif (WORD(1:12).eq.'*Acou_inside') then
        IF ((IER .GE. 1) .OR. (ND.NE.4)) THEN
C          CALL ErrArg(2,IRdComp,CompoNam(IRdComp),1,ND,WORD(1:12),
C     &                OutStr)
          GOTO 666         
        ENDIF
        K=13
        CALL EGETW(OutStr,K,SyTrevPt(IRdComp),
     &                   'W','Absorption coefficients pointer',IER)
        IF (IER.GE.1) goto 666 
        IF (SyTrevPt(IRdComp)(1:4).NE.'none') THEN 
          CALL EGETP(OutStr,K,SyTrevDe(IRdComp),  
     &                        'W',' Acoustic system description',IER)
          IF (IER.GE.1) goto 666
          CALL EGETW(OutStr,K,AbsUnit(IRdComp),  
     &                        'W','Absorb coeff unit',IER)
          IF (IER.GE.1) goto 666
          CALL STRIPCB(IUC,OutStr,99,ND,1,'BCF',IER)
          IF ((IER .GE. 1) .OR. (ND .NE. MNbAbs)) THEN
C            CALL ErrArg(1,IRdComp,CompoNam(IRdComp),MNbAbs,ND,
C     &                  'Absorption coefficients',OutStr)
            GOTO 666         
          ENDIF
          K=0
          DO 441 L=1,MNbAbs
            WRITE(outs,'(a,a)')AbsFreq(L),' for material'
            CALL EGETWR(OutStr,K,SysAbsCo(IRdComp,L), 0., 
     &                        2.,'W',outs,IER)
            IF (IER.GE.1) goto 666
441       continue
        ENDIF
        goto 44

C Construction assembly block
      elseif (WORD(1:16).eq.'*Constr_assembly') then
        IF ((IER .GE. 1) .OR. (ND .NE. 2)) THEN
C          CALL ErrArg(2,IRdComp,CompoNam(IRdComp),1,ND-1,WORD(1:14),
C     &                OutStr)
          GOTO 666         
        ENDIF
C Read information
       if (Outstr(18:21).eq.'none') goto 44
        CALL EGETWI(OutStr,K,NbAss,0,MNbAss,'W',
     &       'Nb of processes for construction assembly',IER)
        IF (IER.GE.1) goto 666  
        IF (NbAss.GT.0) THEN 
          Ic = 0
          do 429 IT=1,NbAss
            CALL STRIPC(IUC,OutStr,99,ND,1,'BCF',IER)
            IF ((IER .GE. 1) .OR. (ND .NE. 7)) THEN
C              CALL ErrArg(2,IRdComp,CompoNam(IRdComp),7,ND,
C     &                    'Assembly',OutStr)
              GOTO 666         
            ENDIF
            K=0
            IF (OutStr(3:7).NE.'none') THEN 
              Ic = Ic+1
              CALL EGETW(OutStr,K,ComAsPa(IRdComp,IT),'W',
     &               'part of MLC ',IER)
              IF (IER.GE.1) goto 666 
              CALL EGETW(OutStr,K,ComAsPt(IRdComp,IT),'W',
     &             'assembly pointer',IER)
              IF (IER.GE.1) goto 666 
              CALL EGETWR(OutStr,K,TmpReal,0., 1.,'W',outs,IER)
              IF (IER.GE.1) goto 666 
              do 529 L=1,MIMP
                WRITE(outs,'(a,a)')AbrevIMP(L),' for compos. assembly'
                CALL EGETWR(OutStr,K,ComAsImp(IRdComp,Ic,L),
     &                         MinImp(L), MaxImp(L),'W',outs,IER)
                IF (IER.GE.1) goto 666 
529           continue
            ENDIF
429       continue
        ENDIF
        goto 44



C Prefabricated block
C      elseif (WORD(1:15).eq.'*Prefabrication') then
C
C        IF ((IER .GE. 1) .OR. (ND .NE. 2)) THEN
CC          CALL ErrArg(2,IRdComp,CompoNam(IRdComp),1,ND-1,WORD(1:15),
CC     &                OutStr)
C          GOTO 666         
C        ENDIF
CC Read information
C        CALL EGETW(OutStr,K,ResPref,'W','Prefabrication Y/N',IER)
C        IF (IER.GE.1) goto 666
C        if (ResPref.eq.'Yes') then
C73        CALL STRIPCB(IUC,iuout,OUTSTR,99,ND,1,'prefa.block',IER)
C          IF (IW.EQ.2) call edisp(iuout,OUTSTR) 
C          IF(IER.NE.0) goto 1
C          K=0
C          CALL EGETW(OUTSTR,K,WORD,'W','tag',IFLAG)
      elseif (WORD(1:12) .EQ. '*Tran_Constr') then
        IF ((IER .GE. 1) .OR. (ND.NE.2)) THEN
C         CALL ErrArg(2,IRdComp,CompoNam(IRdComp),1,ND-1,WORD(1:23),
C     &                 OutStr)
          GOTO 666         
        ENDIF
        IF (Outstr(14:17).eq.'none') GOTO 44         
        CALL EGETWI(OutStr,K,NbTr,0,MNbTrans,'W',
     &       'Nb of transportation categories for construction',IER)
        IF (IER.GE.1) goto 666 
        IF (NbTr.GT.0) THEN 

C Read the Transport information 
          Ic=0
          DO 430 IT=1,NbTr
            CALL STRIPC(IUC,OutStr,99,ND,1,'BCF',IER)
            IF ((IER .GE. 1) .OR. (ND .NE.(MIMP+3))) THEN
C                  CALL ErrArg(3,IRdComp,CompoNam(IRdComp),MIMP+3,ND,
C     &                   'Construction transport',OutStr)
                  GOTO 666         
            ENDIF
            K=0
            IF (OutStr(1:4).NE.'none') THEN
              Ic = Ic+1
              CALL EGETW(OutStr,K,CoTrPt(IRdComp,Ic),
     &              'W','construction transport category ',IER)
              IF (IER.GE.1) goto 666 
              CALL EGETWR(OutStr,K,CoTrDist(IRdComp,Ic),DistMin,
     &          DistMax,'W','transport distance for construction',IER)
              IF (IER.GE.1) goto 666 
              CALL EGETWR(OutStr,K,CoTrBrk(IRdComp,Ic),0.,100.,
     &         'W','breake & loss during construction transport',IER)
              IF (IER.GE.1) goto 666 
              do 530 L=1,MIMP
                WRITE(outs,'(a,a)')AbrevIMP(L),
     &            'impacts of construction transport to project site'
                CALL EGETWR(OutStr,K,CoTrImp(IRdComp,Ic,L),
     &                         MinImpTr(L), MaxImpTr(L),'W',outs,IER)
                IF (IER.GE.1) goto 666
530           continue
            ENDIF
430       CONTINUE
        ENDIF
        GOTO 44

      elseif (WORD(1:17).eq.'*Assembly_at_site') then
        IF (Outstr(19:22).eq.'none') GOTO 44 
        IF ((IER .GE. 1) .OR. (ND.NE.2)) THEN
C          CALL ErrArg(2,IRdComp,CompoNam(IRdComp),1,ND-1,
C     &                     WORD(1:21),OutStr)
C          GOTO 666         
        ENDIF
        CALL EGETWI(OutStr,K,NbAss,0,MNbAss,'W',
     &           'Nb of processes for assembly on building ',IER)
        IF (IER.GE.1) goto 666 
        IF (NbAss.GT.0) THEN 
          Ic = 0
          DO 431 IT=1,NbAss
            CALL STRIPC(IUC,OutStr,99,ND,1,'BCF',IER)
            IF ((IER .GE. 1) .OR. (ND .NE. (MIMP+3))) THEN
C              CALL ErrArg(2,IRdComp,CompoNam(IRdComp),MIMP+3,ND,
C     &                        'Assembly on projetc process',OutStr)
               GOTO 666         
            ENDIF
            K=0
            IF (OutStr(1:4).NE.'none') THEN 
              Ic = Ic+1
              CALL EGETW(OutStr,K,AsProjPt(IRdComp,Ic),
     &                'W','assembly on project pointer',IER)
              IF (IER.GE.1) goto 666 
              CALL EGETW(OutStr,K,AsProjPa(IRdComp,Ic),
     &                'W','dump pointer',IER)
              IF (IER.GE.1) goto 666 
              CALL EGETWR(OUTSTR,K,AsProBrk(IRdComp,Ic),
     &                 0.0,1.0,'W',
     &         'Prefab. construction break during assembly on project',
     &                  IER)
              IF (IER .GE. 1) goto 666 
              do 531 L=1,MIMP
                WRITE(outs,'(a,a)')AbrevIMP(L),
     &                  'impacts of assembly on project'
                CALL EGETWR(OutStr,K,AsProImp(IRdComp,Ic,L),
     &                         MinImp(L),MaxImp(L),'W',outs,IER)
                IF (IER.GE.1) goto 666
531           continue
            ENDIF
431       CONTINUE
        ENDIF
C          endif
C End of prefabrication information block
C        endif
        goto 44

C Construction Maintenance
      elseif (WORD(1:13).eq.'*Constr_maint') then

         IF (Outstr(15:18).eq.'none') GOTO 44 
         IF ((IER .GE. 1) .OR. (ND.NE.2)) THEN
C          CALL ErrArg(2,IRdComp,CompoNam(IRdComp),1,ND-1,WORD(1:12),
C     &                OutStr)
          GOTO 666         
        ENDIF
        CALL EGETWI(OutStr,K,NbMain,0,MNbM,'W',
     &       'Nb of construction maintenance',IER)
        IF (IER.GE.1) goto 666 
        IF (NbMain.GT.0) THEN 
          Ic = 0
          DO 432 IT=1,NbMain
           CALL STRIPC(IUC,OutStr,99,ND,1,'Maintenance type',IER)
            IF ((IER .GE. 1) .OR. (ND .NE. (MIMP+3))) THEN
C              CALL ErrArg(2,IRdComp,CompoNam(IRdComp),MIMP+3,ND,
C     &                    'Maintenance process',OutStr)
              GOTO 666         
            ENDIF
            K=0
            IF (OutStr(1:4).NE.'none') THEN 
              Ic = Ic+1
              CALL EGETW(OutStr,K,MaintTyp(IRdComp,IT),
     &              'W','maintenance part',IER)
              IF (IER.GE.1) goto 666 
              CALL EGETW(OutStr,K,MaintPt(IRdComp,Ic),
     &                'W','Construction maintenance ptr',IER)
              IF (IER .GE. 1) goto 666 
              CALL EGETWR(OutStr,K,PerMaint(IRdComp,Ic),0.,
     &                100.,'W','Construction maintenance period',IER)
              IF (IER .GE. 1) goto 666 
              DO 433 L=1,MIMP
                WRITE(outs,'(a,a)')AbrevIMP(L),'impacts for maintening'
                CALL EGETWR(OutStr,K,RMainImp(IRdComp,Ic,L),
     &                  MinImp(L),MaxImp(L),'W',outs,IER)
                IF (IER.GE.1) goto 666
433           continue
            ENDIF
432       continue
        ENDIF
        goto 44

      elseif (WORD(1:7).eq.'*Layers') then
        IF ((IER .GE. 1) .OR. (ND.NE.3)) THEN
C          CALL ErrArg(2,IRdComp,CompoNam(IRdComp),0,ND-1,WORD(1:6),
C     &               OutStr)
          GOTO 666
        ELSE
          K=8
          CALL EGETWI(OutStr,K,NTLayers,1,ME,'W','total thermo',IER)
          CALL EGETWI(OutStr,K,NLayers,1,ME,'W','total eia lay',IER)
          NbEIALay(IRdComp)= NLayers
        ENDIF
C Layer header
        do 66 IRdLay=1,NbEIALay(IRdComp)
          IF (IW.EQ.2) call edisp(iuout,OUTSTR) 
          CALL STRIPCB(IUC,OutStr,99,ND,1,'Layer block',IER)
          IF ((IER .GE. 1) .OR. (ND .NE. 8)) THEN
C            CALL ErrArg(2,IRdComp,CompoNam(IRdComp),10,ND,
C     &                  'Layer header',OutStr)
            GOTO 666         
          ENDIF
          K=0
          CALL EGETW(OutStr,K,LayerThm(IRdComp,IRdLay),
     &        'W','Layer type  t: thermal or n: non-thermal',IER)
          IF (IER .GE. 1) goto 666 
          CALL EGETW(OutStr,K,LayerTyp(IRdComp,IRdLay),
     &        'W','Layer type (S or G for thermal) else GVNPFS',IER)
          IF (IER .GE. 1) goto 666 
C Group LayerTyp by Surface category (S) or by Perimeter category (P)
          if (LayerThm(IRdComp,IRdLay) .EQ. 't') then
            LayerCat(IRdComp,IRdLay) = 'S'
          elseif (LayerThm(IRdComp,IRdLay) .EQ. 'n') then
            IF (LayerTyp(IRdComp,IRdLay) .EQ. 't') THEN
              LayerCat(IRdComp,IRdLay) = 'S'
            ELSEIF (LayerTyp(IRdComp,IRdLay) .EQ. 's') THEN
              LayerCat(IRdComp,IRdLay) = 'S'
            ELSEIF (LayerTyp(IRdComp,IRdLay) .EQ. 'g') THEN
              LayerCat(IRdComp,IRdLay) = 'S'
            ELSEIF (LayerTyp(IRdComp,IRdLay) .EQ. 'v') THEN
              LayerCat(IRdComp,IRdLay) = 'S'
            ELSEIF (LayerTyp(IRdComp,IRdLay) .EQ. 'n') THEN
              LayerCat(IRdComp,IRdLay) = 'S'
            ELSEIF (LayerTyp(IRdComp,IRdLay) .EQ. 'p') THEN
              LayerCat(IRdComp,IRdLay) = 'S'
            ELSEIF (LayerTyp(IRdComp,IRdLay) .EQ. 'f') THEN
              LayerCat(IRdComp,IRdLay) = 'P'
            ELSEIF (LayerTyp(IRdComp,IRdLay) .EQ. 'c') THEN
              LayerCat(IRdComp,IRdLay) = 'P'
            ELSE
              WRITE(outs,'(a,a)')'For construction: ',CompoNam(IRdComp)
              call edisp(iuout,outs)
              WRITE(outs,'(a,a)')'Layer number:     ',IRdLay
              call edisp(iuout,outs)
              WRITE(outs,'(3a)')'Layertype ',LayerTyp(IRdComp,IRdLay),
     &                          ' is unknown'
              call edisp(iuout,outs)
              CALL EDISP(iuout,' Check the BCF file.')
              GOTO 666         
            ENDIF
C Layer header
          endif         
          CALL EGETP(OutStr,K,Tmpstr,'W','Layer category ',IER)
          IF (IER .GE. 1) goto 666 
          CALL EGETP(OutStr,K,LayerNam(IRdComp,IRdLay),
     &                'W','Layer pointer',IER)
          IMatID(IRdComp,IRdLay)=MatIndex(LayerNam(IRdComp,IRdLay))
          IF (IER .GE. 1) goto 666 
          CALL EGETWI(OutStr,K,ITmp,0,1000,
     &                'W','Material db reference',IER)

          IF (IER .GE. 1) goto 666 
          CALL EGETWI(OutStr,K,ITmp,0,1000,'W',
     &                    'not relevant',IER)
          IF (IER .GE. 1) goto 666 

          CALL EGETWR(OutStr,K,DTHK(IRdComp,IRdLay),0.,
     &                10.,'W','Layer thickness',IER)
          IF (IER .GE. 1) goto 666 
          CALL EGETP(OutStr,K,LayerDes(IRdComp,IRdLay),
     &                'W','Layer description',IER)
          IF (IER .GE. 1) goto 666 

  66    continue
        goto 44
      ELSE
       call edisp(iuout,outstr)
       call edisp(iuout,'unrecognised tag:')
        call edisp(iuout,outstr)
        goto 666
      endif

777   WRITE (outs,'(3a)')' BCF file: ', BCFfile(1:lnblnk(BCFfile)),
     &                          ' successfully read.'
      call EDISP(iuout,outs)
      IF(ACOOK) THEN
        call EDISP(iuout,'Please select zone(s) to include.')
      ENDIF
C Close cfg file before exiting.
 99   CALL ERPFREE(IUC,ISTAT)
      RETURN

 666  WRITE(outs,'(a,a)')' Unable to read the file : ',BCFfile
      CALL edisp(iuout,outs)
      CALL EDISP(iuout,' Please check your BCF file.')
      BCFOK = .FALSE. 
      BCFfile= 'UNKNOWN'
      IER=1
      goto 99

 1    WRITE(outs,'(a,a)')' Unable to read the file : ',BCFfile
      CALL EDISP(iuout,outs)
      CALL EDISP(iuout,' Please check your building constructions file')
      IER=1
      goto 99

      END


C *************************ErrArg**********************************
C Return error message to Textual feedback, when nb of arguments 
C not valid in a line in a material block of the BCF file 
C and return its location in the file.
C
C In the BCF header :  Itype = 0
C For material data :  Itype = 1 
C For construction data : Itype = 2 
C For transport data : Itype = 3
 
      SUBROUTINE ErrArg(Itype,ID,Label,MShould,MIs,Block,OutStr)

      COMMON/OUTIN/IUOUT,IUIN
      CHARACTER Label*24,OutStr*124,outs*174, Block*30, SBlock*30

      SBlock = ' '
      SBlock = Block
C Check consistancy
      IF (Itype .EQ. 0) THEN
        WRITE(outs,'(a,a)')' Error in BCF header block: ',Label
      ELSEIF (Itype .EQ. 1) THEN
        WRITE(outs,'(a,I3,a,a)')' For material: ',ID,' ',Label
      ELSEIF (Itype .EQ. 2) THEN
        WRITE(outs,'(a,I3,a,a)')' For construction: ',ID,' ',Label
      ENDIF
      Call edisp(iuout, outs)
      IF (Itype .NE. 0) THEN
        WRITE(outs,'(a,a)')' Error in block : ', SBlock
        Call edisp(iuout, outs)
      ENDIF
      IF (Itype .EQ. 3) THEN
        WRITE(outs,'(a)')' in transport data'
        Call edisp(iuout, outs)
      ENDIF
      WRITE(outs,'(a,a)')' Nb of arguments is incorrect in : ', OutStr
      Call edisp(iuout, outs)
      WRITE(outs,'(a,I3,a,I3,a)')' Should be ',MShould, '   but are  ',
     &      MIs,'   arguments'
      Call edisp(iuout, outs)
      RETURN
      END


