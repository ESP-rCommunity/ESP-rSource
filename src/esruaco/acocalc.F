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

C acocalc.F holds routines for reverberation time calculation:
C  MLCIndex : return the construction index in the BCF file using 
C             the construction name of a surface. 

C  factorM  : Calcul the air absorption coefficient according to 
C             the air humidity, temperature, and sound frequency.

C  SurfEquiCalc:  Calculate the equivalent surface area of zone (IZone)
C                 using the method (Method) and return the values for
C                 each frequency.

C TRevCalc : Calculate the reverberation time of zone(s) using the
C            selected method.


C************ MLCIndex*************
C Using an MLC name (XMLC) of a surface, returns its MLC index in the BCF file.
C Scan the list of MLC name (CompoNam) and compare to XMLC.

C XMLC (Char) : MLC name to find
C CompoNam(MMLC) (Char) : List of MLC name in BCF.
C MLCIndex (Integer) : MLC index in the BCF file corresponding to XMLC

      FUNCTION MLCIndex(XMLC)

#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/EIAhigh/NbComp,NbMat,IMatID(MMLC,ME),ImatDbID(0:MMAT-1),
     &               LCIATag
      COMMON/DesMLC/MatName(MMAT),MatDesc(MMAT),MatCat(MMAT),
     &              CompoNam(MMLC), LayerDes(MMLC,ME)
      CHARACTER*12 XMLC
      CHARACTER*72 outs
      CHARACTER*72 LayerDes, MatDesc, MatCat
      CHARACTER*12 MatName, CompoNam
 
      MLCIndex = 0
      DO 10 IC=1,NbComp
        IF (CompoNam(IC) .EQ. XMLC) THEN
          MLCIndex = IC
          GOTO 363
        ENDIF
 10   CONTINUE
363   IF (MLCIndex .EQ. 0) THEN
        WRITE(outs,'(a,a)') XMLC,' not found in BCF file.'
        CALL edisp(iuout,outs)
        GOTO 666
      ENDIF

666   RETURN
      END


C************ factorM*************
C Calcul the air absorption coefficient according to 
C the air humidity, temperature, and sound frequency
C
C RH   : Relative humidity [%]
C Freq : sound frequency [Hz]
C To : Triple point istherm temperature [K]
C Tr : Reference air temperature [K]
C Tair : Air temperature [K]
C XN : Fractional volume of Nitrogen [%]
C XO : Fractional volume of Oxygen[%]
C VN : Vibrational temperature of Nitrogen [K]
C VO : Vibrational temperature of Oxygen [K]
C c :  Sound Speed [m/s]
C FrN: relaxation frequency of Nitrogen [Hz]
C FrO: relaxation frequency of Oxygen [Hz]

      FUNCTION FactorM(RH,TairC,IFreq)

      COMMON/OUTIN/IUOUT,IUIN
      CHARACTER*72 outs

      IF (IFreq .EQ. 0) THEN
        WRITE(outs,'(I4,a)') IFreq,
     &                     ' [Hz] is not an acceptable frequency.'
        CALL edisp(iuout,outs)
        GOTO 666
      ELSE
C List of constants
        To1 = 273.15
        To = 293.15
        Tair = TairC + 273.15
C Molar concentration h
        p = -6.8346*(To1/Tair)**1.261 + 4.6151
        h = RH * 10**p
C Relaxation frequencies
        FrN =(Tair/To)**(-1./2.)*
     &               (9.+280.*h*exp(-4.170*(((Tair/To)**(-1./3.))-1.)))
        FrO = 24. + 4.04*10000.*h*(0.02+h)/(0.391+h)
 
        factorM = 3.68E-11*IFreq**2*sqrt(Tair/To) + (Tair/To)**(-5./2.)*
     &     (0.1068*exp(-3352.0/Tair)*2*IFreq**2/(FrN + (IFreq**2/FrN))+
     &      0.0128*exp(-2239.1/Tair)*2*IFreq**2/(FrO + (IFreq**2/FrO)))
      ENDIF

666   RETURN
      END



C*********** SurfEquiCalc ***********
C Calculate the equivalent surface area of zone (IZone) using the method (Method)
C and return the values for each frequency
C
C IZone  : Zone number for which the calculation has to be performed
C Method  : Method number used for the reverberation time calculation
C                    1: Sabine, 2: Eyring, 3: Millington, 4: all methods
C SurfEquiM : Equivalent surface for surface material [m2]
C SurfEquiP : Equivalent surface of Public [m2]
C SurfEquiF : Equivalent surface of Furniture [m2]
C SurfEquT  : Total Equivalent surface for current zone [m2]
C SurfTot   : Total surface area of surfaces in the current zone [m2]
C NbPubTot  : Number of public type
C XNbFur    : Nb of item or surface area of Furniture
C SNAeff    : Effectif surface of a zone surface when behind a furniture
C CovSurf   : Name of a surface behind a furniture (Extracted from ACA file)


      SUBROUTINE SurfEquiCalc(IZone,Method, IairAbso)

#include "building.h"
#include "geometry.h"
#include "acoustic.h"

      COMMON/SDATA/SNA(MS),GSA(MS),GWA(MS),PAZI(MS),PELV(MS)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/AbsoCoef/SysAbsCo(MMLC,MNbAbs),PubAbsCo(MaxPub,MNbAbs),
     &                FurAbsCo(MaxPub,MNbAbs),AirAbs(MNbAbs)

      COMMON/Trev/Divis(MNbAbs),SurfEquM(MS,MNbAbs), SEquM(MS,MNbAbs), 
     &            SurfEquP(MaxPub,MNbAbs), SurfEquF(MaxFur,MNbAbs),
     &            SurfEquT(MNbAbs), SurfTot, TReverb(MNbAbs), Tmean

      COMMON/AirPro/RTair,RHR
      COMMON/AbsCHAR/PubType(MaxPub),FurType(MaxFur),FurCat(MaxFur)
      COMMON/PubFurZone/NbPub(MaxPub),XNbFur(MaxFur),SNAeff(MS),
     &                  IndexPub(MaxPub),IndexFur(MaxFur)
      COMMON/CPubFurZone/CovSurf(MaxFur)
      COMMON/MetNam/ACOName,CalName(4),LabFreq(MNbAbs)
      CHARACTER CalName*12
      CHARACTER outs*124
      CHARACTER PubType*12, FurType*12, FurCat*4, CovSurf*12
      CHARACTER LabFreq*4,ACOName*72


C Initialisation
      SurfTot = 0.0
      DO 10 I=1,MNbAbs
        SNAeff(I)= 0.0
        do 11 J=1,MS
          SurfEquM(J,I) = 0.0
          SurfEquP(J,I) = 0.0
          SurfEquF(J,I) = 0.0
 11     CONTINUE
 10   Continue

C Read the ACO file to determine if they are some public and furniture
      call SCNZONEACO(zname(Izone),0,IER)

C Find if a surface is hiden by a furniture.
C If so the effecif surface area (SNAeff) corespond to 
C Surface area (SNA) - Furniture area (XnbFur)
      DO 20 I=1,MS
        SNAeff(I) = SNA(I)
 20   Continue

      DO 50 IFur = 1,MaxFur
        IF (CovSurf(IFur).NE.'none') THEN          
          DO 51 ISurf = 1, NSUR
            icon=IZSTOCN(izone,isurf)
            IF (CovSurf(IFur).EQ.SSNAME(icon)) THEN
              SNAeff(ISurf) =  SNAeff(ISurf) - XNbFur(IFur)
              GOTO 50
            ENDIF
C Next Surface
 51       CONTINUE
        ENDIF
C Next Furniture
 50   CONTINUE

C For each surface in the zone, find the MLC number in BCF file according 
C to its surface name and calcule the equivalente area of the MATERIAL
      DO 100 ICurSurf = 1,NSUR
        icon=IZSTOCN(izone,ICurSurf)
        IF ((SNAeff(ICurSurf) .GT. 0.) .OR. (ICurSurf .EQ. MS)) THEN

          if (SNAeff(ICurSurf) .GT. 0.) then
            SurfTot = SurfTot + SNAeff(ICurSurf)
            ICurComp = MLCIndex(SSMLCN(icon))
          endif

C For each frenquency, compute the equivalente surface FOR MATERIAL and 
C total equivalente surface at frequency ICurFreq ...
          DO 200 ICurFreq=1,MNbAbs

C ...depending on the following calculation method:

            IF (Method .EQ. 1) THEN
C Sabine
              SurfEquM(ICurSurf,ICurFreq) = 
     &                  SNAeff(ICurSurf) * SysAbsCo(ICurComp,ICurFreq)
              SurfEquT(ICurFreq) = SurfEquT(ICurFreq) + 
     &                             SurfEquM(ICurSurf,ICurFreq)

            ELSEIF (Method .EQ. 2) THEN
C Eyring
              SurfEquM(ICurSurf,ICurFreq) = 
     &                   SNAeff(ICurSurf) * SysAbsCo(ICurComp,ICurFreq)
              SurfEquT(ICurFreq) = SurfEquT(ICurFreq) + 
     &                             SurfEquM(ICurSurf,ICurFreq)

            ELSEIF (Method .EQ. 3) THEN
C Millington
              SurfEquM(ICurSurf,ICurFreq) = -1. * SNAeff(ICurSurf) *
     &                           LOG(1. - SysAbsCo(ICurComp,ICurFreq))
              SurfEquT(ICurFreq) = SurfEquT(ICurFreq) + 
     &                             SurfEquM(ICurSurf,ICurFreq)
            ENDIF
C Next frequency
  200     CONTINUE
        ENDIF
C Next Surface
  100 CONTINUE


C Calcule the equivalente area of the PUBLIC
      DO 110 ICurPub = 1,MaxPub
        IF (NbPub(ICurPub) .GT. 0.)  THEN

C For each frenquency
          DO 210 ICurFreq=1,MNbAbs
            IF (Method .EQ. 1) THEN
C Sabine
              SurfEquP(ICurPub,ICurFreq) = NbPub(ICurPub) *
     &                             PubAbsCo(IndexPub(ICurPub),ICurFreq)
              SurfEquT(ICurFreq) = SurfEquT(ICurFreq) + 
     &                             SurfEquP(ICurPub,ICurFreq)

            ELSEIF (Method .EQ. 2) THEN
C Eyring
              SurfEquP(ICurPub,ICurFreq) = NbPub(ICurPub) *
     &                      PubAbsCo(IndexPub(ICurPub),ICurFreq)
C              SurfEquP(ICurPub,ICurFreq) = NbPub(ICurPub) * 
C     &               (1.- EXP(- PubAbsCo(IndexPub(ICurPub),ICurFreq)))
              SurfEquT(ICurFreq) = SurfEquT(ICurFreq) + 
     &                             SurfEquP(ICurPub,ICurFreq)

            ELSEIF (Method .EQ. 3) THEN
C Millington
              SurfEquP(ICurPub,ICurFreq) = -1. * NbPub(ICurPub) *
     &                  LOG(1. - PubAbsCo(IndexPub(ICurPub),ICurFreq))
              SurfEquT(ICurFreq) = SurfEquT(ICurFreq) + 
     &                           SurfEquP(ICurPub,ICurFreq)
            ENDIF
C Next frequency
  210     CONTINUE
        ENDIF
C Next Occupant
  110 CONTINUE


C Calcule the equivalente area of the FURNITURE
      DO 120 ICurFur = 1,MaxFur

C If furniture infront of a zone surface add it to the total area 
        IF (XNbFur(ICurFur) .GT. 0.)  THEN
          IF (Method .EQ. 2) THEN
            IF (FurCat(IndexFur(ICurFur)).EQ.'Surf') THEN
              SurfTot = SurfTot +  XNbFur(ICurFur)
            ENDIF
          ENDIF
          DO 220 ICurFreq=1,MNbAbs

            IF (Method .EQ. 1) THEN
C Sabine
              SurfEquF(ICurFur,ICurFreq) = XNbFur(ICurFur) *
     &                      FurAbsCo(IndexFur(ICurFur),ICurFreq)
              SurfEquT(ICurFreq) = SurfEquT(ICurFreq) + 
     &                             SurfEquF(ICurFur,ICurFreq)

            ELSEIF (Method .EQ. 2) THEN
C Eyring
              SurfEquF(ICurFur,ICurFreq) = XNbFur(ICurFur) *
     &                      FurAbsCo(IndexFur(ICurFur),ICurFreq)
              SurfEquT(ICurFreq) = SurfEquT(ICurFreq) + 
     &                             SurfEquF(ICurFur,ICurFreq)

            ELSEIF (Method .EQ. 3) THEN
C Millington
              SurfEquF(ICurFur,ICurFreq) = -1. * XNbFur(ICurFur) *
     &                  LOG(1. - FurAbsCo(IndexFur(ICurFur),ICurFreq))
              SurfEquT(ICurFreq) = SurfEquT(ICurFreq) + 
     &                           SurfEquF(ICurFur,ICurFreq)
            ENDIF

C Next frequency
  220     CONTINUE
        ENDIF
C Next Furniture
  120 CONTINUE

C Calcul air absorption
      DO 300 ICurFreq=1,MNbAbs
        if (IairAbso.eq.1) then
CS          Ifreq = INT(100* EXP((ICurFreq-1)/3.*LOG(2.)))
          Ifreq = INT(1000.*(10.**(1./10.))**(ICurFreq-11))
          AirAbs(ICurFreq) = 4.*Vol(IZone)*FactorM(RHR,RTair,Ifreq)
        else
          AirAbs(ICurFreq) = 0.
        endif
  300 CONTINUE


C Calcul divisor for the reverberation time formula (Total absorption)
      DO 500 ICurFreq = 1,MNbAbs
        IF (Method .LT. 4) THEN
          if (Method .EQ. 2) then
C Sum equivalent surface if Eyring calculation method
            Divis(ICurFreq) =  Divis(ICurFreq) +
     &                 (-SurfTot * LOG(1.- SurfEquT(ICurFreq)/SurfTot))
          else
            Divis(ICurFreq) = Divis(ICurFreq) + SurfEquT(ICurFreq)
          endif
          Divis(ICurFreq) = Divis(ICurFreq) + AirAbs(ICurFreq)
        ELSE
          write(outs,'(a,a,a)') 'The method ',CalName(Method),  
     &        ' does not exist. Please check.'
          call edisp(iuout,outs)
        ENDIF
  500 CONTINUE

      RETURN
      END


C*********** TRevCalc ***********
C Calculate the reverberation time of zone (IZone)
C and return the values (for each frequency) in the array RevTime
C
C TReverb : Reverberation time for each frequency (MNbAbs).
C Tmean   : Mean reverberation time over all frequencies (1/3 octave)

      SUBROUTINE TRevCalc

#include "building.h"
#include "geometry.h"
#include "acoustic.h"

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/AirPro/RTair,RHR
      COMMON/Trev/Divis(MNbAbs),SurfEquM(MS,MNbAbs), SEquM(MS,MNbAbs), 
     &            SurfEquP(MaxPub,MNbAbs), SurfEquF(MaxFur,MNbAbs),
     &            SurfEquT(MNbAbs), SurfTot, TReverb(MNbAbs), Tmean

C Initialisation
      Tmean = 0.0
      TotVol = 0.0
      DO 10 I=1,MNbAbs
        TReverb(I) = 0.0
 10   Continue

C Speed sound
        c = 343.23*sqrt((RTair + 273.15)/293.15)


      DO 600 k = 1,NZG
C For each selected zones
        TotVol = TotVol + Vol(nznog(k))
  600 CONTINUE

C Reverberation time calculation according to selected method
      DO 501 ICurFreq = 1,MNbAbs
C        TReverb(ICurFreq) = 0.161 * TotVol / Divis(ICurFreq)
        TReverb(ICurFreq) = (55.3/c) * SQRT((RTair+273.15)/293.15) 
     &                      * TotVol / Divis(ICurFreq)
        TMean = TMean + TReverb(ICurFreq)/MNbAbs
  501 CONTINUE

      RETURN
      END


