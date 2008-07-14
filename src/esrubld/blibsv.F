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

C      MZLS1
C      MZLS2
C      MZLS3
C      MZLS4
C      MZLS5
C      SURBAL

c ******************** MZLS1 ********************

c MZLS1 SAVES THE CONTENTS OF THE SYSTEM
c CONFIGURATION FILE AND THE NAMES OF THE
c PROJECT, GEOMETRY, SHADING AND THERMAL PROPERTY
c FILES RELATING TO EACH ZONE.
c SELECTED MISCELLANEOUS DATA SUCH AS SURFACE AREAS ETC.
c AND THE CURRENTLY ASSIGNED CLIMATE FILE NAME ARE ALSO SAVED.

      SUBROUTINE  MZLS1
#include "building.h"
#include "esprdbfile.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      COMMON/HCFIX2/NHCFP(MCOM),HCFPST(MCOM,MBP),HCFPFT(MCOM,MBP),
     &HCFVI(MCOM,MS,MBP),HCFVE(MCOM,MS,MBP)
      COMMON/LIBREC/IRECPL
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/PREC1/SNA(MCOM,MS),ZOA(MCOM)

      COMMON/PREC3/PAZI(MCOM,MS),PELV(MCOM,MS)
      COMMON/PREC3I/CHARDM(MCOM,MS)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),
     &                NGAPS(MCOM,MS),NPGAP(MCOM,MS,MGP)
      COMMON/PREC10/NWINS(MCOM,MS)
      common/PREC17/ZBASEA(MCOM),IBASES(MCOM,12),IUZBASEA(MCOM),
     &  IZBASELIST(MCOM)
      common/PREC18/ZTRANA(MCOM)

      COMMON/HCTHRY/IHCT,ICOREXHCT

      dimension izdum(MZS)
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LCFGF,LUTIL
      CHARACTER ZNAME*12,zdesc*64,ZNPAD*15,doit*248,outs248*248
      CHARACTER*6 NAME
      character outs*124,tfile*72,cfgroot*24,longtfile*144
      logical concat

C << following block of code is for testing >>
C Create tables for SQL database.
C      QT=CHAR(39)
C      do 10 I=1,NCOMP
C        write (doit,'(9a)')'echo ',QT,
C     &     'CREATE TABLE ',zname(I),' (time DATETIME,',
C     &     ' ait_T REAL, plant REAL, inf REAL, vent REAL, tmcon REAL,',
C     &     ' outopq REAL, outtrn REAL, QW1 REAL, QW2 REAL)',
C     &     QT,'|mysql -u iain -pxx fred'
C        write (6,*) doit
C        call runit(doit,'-')
C 10   continue

C Clear dummy zones variable (in place of old NDOORS array).
      do 42 jj=1,MCOM
        izdum(jj)=0
  42  continue
      IUNIT = IFIL + 2
      IREC = MNRS+2

      NCP=NCOMP
      IF(NCOMP.GT.18)NCP=18
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NCOMP,NCON
      IREC = IREC + 1
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)LSNAM
      IREC = IREC + 1

      NCP=NCOMP
      IF(NCOMP.GT.20)NCP=20
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)  (NCCODE(I),I=1,NCP)
      IREC = IREC + 1
      IF(NCOMP.LE.20)goto  4
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(NCCODE(I),
     &I=21,NCOMP)
      IREC = IREC + 1
    4 WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)LCLIM
      IREC=IREC+1
      DO 20 I = 1,NCOMP
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)LPROJ(I)
      IREC = IREC + 1
   20 CONTINUE

      DO 40 I = 1,NCOMP
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)LGEOM(I)
      IREC = IREC + 1
   40 CONTINUE

      DO 60 I = 1,NCOMP
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)LSHAD(I)
      IREC = IREC + 1
   60 CONTINUE

      DO 80 I = 1,NCOMP
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)LTHRM(I)
      IREC = IREC + 1
   80 CONTINUE

      X=FLOAT(NCON)/20.
      N=INT(X)+1
      DO 100 I = 1,N
      J1 = I*20-19
      J2 = I*20
      IF (I .EQ. N)  J2 = NCON
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)  (IC1(J),J=J1,J2)
      IREC = IREC + 1
  100 CONTINUE

      DO 110 I = 1,N
      J1 = I*20-19
      J2 = I*20
      IF (I .EQ. N)  J2 = NCON
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)  (IE1(J),J=J1,J2)
      IREC = IREC + 1
  110 CONTINUE

      DO 120 I = 1,N
      J1 = I*20-19
      J2 = I*20
      IF (I .EQ. N)  J2 = NCON
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)  (ICT(J),J=J1,J2)
      IREC = IREC + 1
  120 CONTINUE

      DO 130 I = 1,N
      J1 = I*20-19
      J2 = I*20
      IF (I .EQ. N)  J2 = NCON
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)  (IC2(J),J=J1,J2)
      IREC = IREC + 1
  130 CONTINUE

      DO 140 I = 1,N
      J1 = I*20-19
      J2 = I*20
      IF (I .EQ. N)  J2 = NCON
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)  (IE2(J),J=J1,J2)
      IREC = IREC + 1
  140 CONTINUE

C Write out 15 char string for zone name (so reading logic the same).
      DO 150 I = 1,NCOMP
        write(znpad,'(a,a)')zname(I),'   '
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)znpad
      IREC = IREC + 1
  150 CONTINUE

      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)  (NCONST(I),I=1,NCP)
      IREC = IREC + 1
      IF(NCOMP.LE.20)goto  5
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(NCONST(I),I=21,NCOMP)
      IREC = IREC + 1
    5 DO 160 I = 1,NCOMP
      NN = NCONST(I)
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000) (NELTS(I,J),J=1,NN)
      IREC = IREC + 1
  160 CONTINUE

      DO 170 I = 1,NCOMP
      NN = NCONST(I)
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000) (NGAPS(I,J),J=1,NN)
      IREC = IREC + 1
  170 CONTINUE

      DO 180 I = 1,NCOMP
      NN = NCONST(I)
      DO 190 J = 1,NN,5

C DUMP FOR ALL THREE POSSIBLE AIR GAPS
        if(J.LE.(NN-5))then
          WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &     (NPGAP(I,J,K),K=1,MGP),(NPGAP(I,J+1,K),K=1,MGP),
     &     (NPGAP(I,J+2,K),K=1,MGP),(NPGAP(I,J+3,K),K=1,MGP),
     &     (NPGAP(I,J+4,K),K=1,MGP)
        elseif(J.eq.(NN-4))then
          WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &     (NPGAP(I,J,K),K=1,MGP),(NPGAP(I,J+1,K),K=1,MGP),
     &     (NPGAP(I,J+2,K),K=1,MGP),(NPGAP(I,J+3,K),K=1,MGP)
        elseif(J.eq.(NN-3))then
          WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &     (NPGAP(I,J,K),K=1,MGP),(NPGAP(I,J+1,K),K=1,MGP),
     &     (NPGAP(I,J+2,K),K=1,MGP)
        elseif(J.eq.(NN-2))then
          WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &     (NPGAP(I,J,K),K=1,MGP),(NPGAP(I,J+1,K),K=1,MGP)
        elseif(J.eq.(NN-1))then
          WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &     (NPGAP(I,J,K),K=1,MGP)
        endif
        IREC = IREC + 1
  190 CONTINUE
  180 CONTINUE

C Save floor area of each zone.
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000) (ZBASEA(I),I=1,NCOMP)
      IREC = IREC + 1
C      write (6,*) 'FLOOR A:',IREC,(ZBASEA(I),I=1,NCOMP)

C Save area of transparent surfaces connected to external type BC.
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000) (ZTRANA(I),I=1,NCOMP)
      IREC = IREC + 1
C      write (6,*) 'TRAN A:',IREC,(ZTRANA(I),I=1,NCOMP)

C Save surface area of each surface in each zone.
      DO 230 I=1,NCOMP
      NN=NCONST(I)
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(SNA(I,J),J=1,NN)
      IREC=IREC+1
  230 CONTINUE
      DO 240 I=1,NCOMP
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)ZOA(I)
      IREC=IREC+1
  240 CONTINUE
      DO 250 I=1,NCOMP
      NN=NCONST(I)
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(CHARDM(I,J),J=1,NN)
      IREC=IREC+1
  250 CONTINUE
      DO 260 I=1,NCOMP
      NN=NCONST(I)
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(PAZI(I,J),J=1,NN)
      IREC=IREC+1
  260 CONTINUE
      DO 270 I=1,NCOMP
      NN=NCONST(I)
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(PELV(I,J),J=1,NN)
      IREC=IREC+1
  270 CONTINUE
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(NHCFP(J),J=1,NCP)
      IREC=IREC+1
      IF(NCOMP.LE.20)goto 6
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(NHCFP(J),J=21,NCOMP)
      IREC=IREC+1
    6 DO 280 I=1,NCOMP
      IF(IHC(I).EQ.0)goto 280
      NP=NHCFP(I)
      NN=NCONST(I)
      DO 290 K=1,NP
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)HCFPST(I,K),
     &HCFPFT(I,K),(HCFVI(I,J,K),J=1,NN)
      IREC=IREC+1
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(HCFVE(I,J,K),J=1,NN)
      IREC=IREC+1
  290 CONTINUE
  280 CONTINUE
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)IHCT,ICOREXHCT
      IREC=IREC+1
      NAME='CONFIG'
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NAME
      IREC=IREC+1

C If the path is other then ./ add in the path to the configuration
C file name.
      call addpath(LCFGF,longtfile,concat)
      if(concat)then
        call edisp(iuout,' ')
        call edisp(iuout,'Simulation run with configuration file: ')
        call edisp(iuout,longtfile)
        call edisp(iuout,' ')
      endif

C << NOTE: this needs 36 words wide to be the minimal record width >>
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)longtfile
      IREC=IREC+1
C     \
C      \
C       \
C        \
C         \
C          \
C          /
C         /
C        /
C       /
C      /
C     /
C NB The following NCOMP records are used to store
C external and internal window and door areas per zone.

      IRECPL = IREC+NCOMP
      IREC = 2
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)  IRECPL
      RETURN

 1000 write(outs,'(A,I5,A)')  ' MZLS1: error at record',IREC,'.'
      call edisp(iuout,outs)

      RETURN
      END

c ******************** MZLS2 ********************

c MZLS2 writes the simulation control information to the
c solutions library.   This information is stored in a
c header block associated with the current simulation
c save set.

      SUBROUTINE MZLS2
#include "building.h"
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/pophelp/h(60)

      COMMON/C1/NCOMP,NCON
      common/C21/IFCFG,cfgroot,LCFGF

      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      common/prec7/itcnst

      COMMON/CCTLNM/CTLDOC,LCTLF
      COMMON/CCTL/ICASCF(MCOM)

      COMMON/SAVE/ISAVE
      COMMON/AVRAGE/IDAVER

      COMMON/LIBREC/IRECPL

C Once per hour storage of results, averaged over that hour.
      common/rdcgen/irdact

      INTEGER irdcb
      REAL sTMA,sTMCON,sQM,sTMS,sQSI,sQSE,sQV1,sQV2,sQW1,sQW2
      REAL sZONPP,sZONQP,sQSA,sQCASR,sQCASC,sZRH,sFRAC
      REAL sTMC,sZLL,sSPM1,sSPM2,sqswrdi,sqlwrdi,sqconvi
      REAL sqcondi,sqstori,sqstore,sqconde,sqconve,sqlwrde
      REAL sqlwrbd,sqlwrsk,sqswrde,soutopq,souttrn,sopqin,strnin
      REAL sqair,sqbridge
      common/rdcbld/irdcb(MCOM),sTMA(MCOM),sTMCON(MCOM),sQM(MCOM),
     &            sTMS(MCOM,MS),sQSI(MCOM,MS),sQSE(MCOM,MS),
     &            sQV1(MCOM),sQV2(MCOM),sQW1(MCOM),sQW2(MCOM),
     &            sZONPP(MCOM),sZONQP(MCOM),sQSA(MCOM),sQCASR(MCOM),
     &            sQCASC(MCOM),sZRH(MCOM),sFRAC(MCOM),
     &            sTMC(MCOM,MS,MN),sZLL(MCOM),sSPM1(MCOM),sSPM2(MCOM),
     &            sqswrdi(MCOM,MS),sqlwrdi(MCOM,MS),sqconvi(MCOM,MS),
     &            sqcondi(MCOM,MS),sqstori(MCOM,MS),sqstore(MCOM,MS),
     &            sqconde(MCOM,MS),sqconve(MCOM,MS),sqlwrde(MCOM,MS),
     &            sqlwrbd(MCOM,MS),sqlwrsk(MCOM,MS),sqswrde(MCOM,MS),
     &            soutopq(MCOM),souttrn(MCOM),sopqin(MCOM),
     &            strnin(MCOM),sqair(MCOM),sqbridge(MCOM)

C Once per hour storage of separate convective, radiant and latent W
C for each casual gain type after controls have been applied.
      real sctlperocupc,sctlperocupr,sctlperocupl,sctlperlightc
      real sctlperlightr,sctlperlightl,sctlperequipc,sctlperequipr
      real sctlperequipl,sctlperothc,sctlperothr,sctlperothl
      common/rdccas/sctlperocupc(MCOM),sctlperocupr(MCOM),
     &            sctlperocupl(MCOM),sctlperlightc(MCOM),
     &            sctlperlightr(MCOM),sctlperlightl(MCOM),
     &            sctlperequipc(MCOM),sctlperequipr(MCOM),
     &            sctlperequipl(MCOM),sctlperothc(MCOM),
     &            sctlperothr(MCOM),sctlperothl(MCOM)

C Uncertainty.
      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)
      COMMON/UA4/NIACT,IACTD(MNACT,2)
      COMMON/UA5/IMET,ISIM,NRSIM
      COMMON/UAFCT/IADIR(MNFA),NFA

C Simulator parameters.
      COMMON/SPFL/spfileok,perok,tstepok,saveok,autook,exitok,startupok
      LOGICAL     spfileok,perok,tstepok,saveok,autook,exitok,startupok

      character LUALF*72,LCNG*15,LLOC*15,H*72
      character LCFGF*72,cfgroot*24

C scname is the first 12 characters of the overall control name.
      CHARACTER CTLDOC*248,LCTLF*72,RSNAME*40,outs*124,scname*12

C Treatment of solar radiation data in weather file
C ESP-r operates on hourly-centered data. That is, the amount of solar
C irradiance at the recorded hour is the instantaneous irradiance at the
C hour. In Canadian Weather for Energy Calculations (CWEC) files, solar
C radiation is integrated over the previous hour. To account for 
C the half hour shift, a flag has been implemented to indicate
C whether solar radiation data in the weather file is hour-centered
C (default) or half-hour centered.
C The flag can be set in the .cfg file or within bps in the 
C 'simulation toggles' menu.
      common/CWEC_SOL/iSlr_half_hr_flg
      integer iSlr_half_hr_flg     !- flag indicating if solar radiation data in weather file
                                   !- is hour-centered (default) or half-hour centered.
                                   !- 0 = hour-centered; 1 = half-hour centered.


C Initialise once per hour storage facility.  (Future work could
C involve storage of results at higher frequency.
      do 101 ICOMP=1,NCOMP
        irdcb(ICOMP)=1
        soutopq(ICOMP)=0.
        souttrn(ICOMP)=0.
        sopqin(ICOMP)=0.
        strnin(ICOMP)=0.
        sTMA(ICOMP)=0.
        sTMCON(ICOMP)=0.
        sQM(ICOMP)=0.
        sQV1(ICOMP)=0.
        sQV2(ICOMP)=0.
        sQW1(ICOMP)=0.
        sQW2(ICOMP)=0.
        sZONPP(ICOMP)=0.
        sZONQP(ICOMP)=0.
        sQSA(ICOMP)=0.
        sQCASR(ICOMP)=0.
        sQCASC(ICOMP)=0.
        sZRH(ICOMP)=0.
        sFRAC(ICOMP)=0.
        sZLL(ICOMP)=0. 
        sSPM1(ICOMP)=0.
        sSPM2(ICOMP)=0.
        sqair(ICOMP)=0.
        sqbridge(ICOMP)=0.
        sctlperocupc(ICOMP)=0.
        sctlperocupr(ICOMP)=0.
        sctlperocupl(ICOMP)=0.
        sctlperlightc(ICOMP)=0.
        sctlperlightr(ICOMP)=0.
        sctlperlightl(ICOMP)=0.
        sctlperequipc(ICOMP)=0.
        sctlperequipr(ICOMP)=0.
        sctlperequipl(ICOMP)=0.
        sctlperothc(ICOMP)=0.
        sctlperothr(ICOMP)=0.
        sctlperothl(ICOMP)=0.

        do 102 ISF=1,MS
          sTMS(icomp,isf)=0.0
          sTMC(icomp,isf,1)=0.0
          sQSI(icomp,isf)=0.0
          sQSE(icomp,isf)=0.0
          sqswrdi(icomp,isf)=0.0
          sqlwrdi(icomp,isf)=0.0
          sqconvi(icomp,isf)=0.0
          sqcondi(icomp,isf)=0.0
          sqstori(icomp,isf)=0.0
          sqstore(icomp,isf)=0.0
          sqconde(icomp,isf)=0.0
          sqconve(icomp,isf)=0.0
          sqlwrde(icomp,isf)=0.0
          sqlwrbd(icomp,isf)=0.0
          sqlwrsk(icomp,isf)=0.0
          sqswrde(icomp,isf)=0.0
  102   continue
  101 continue

c Read number of simulation result sets currently stored
c in solution file.
      IUNIT=IFIL+2
      IREC=1
    4 READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM

c Is this solution library full ?
      IF(NSIM.EQ.MNRS)goto 1
      goto 2

c Solutions library full: discard result-sets or
c open new library.
    1 call edisp(iuout,'Results library full: please specify another.')
      CALL MZLIBF
      goto 4

c Determine start address of the result-set about to
c be entered.
    2 IREC=NSIM+2
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NST
      IREC=NST

c Now write the simulation control information to the
c library.
      scname=' '
      write(scname,'(a)')CTLDOC(1:12)
      if(irdact.gt.0) then
        WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)ISD1,ISM1,ISD2,ISM2,
     &    ISDS,ISDF,irdact,ISAVE,SCNAME,IDAVER,itcnst, iSlr_half_hr_flg
      else
        WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)ISD1,ISM1,ISD2,ISM2,
     &    ISDS,ISDF,NTSTEP,ISAVE,SCNAME,IDAVER,itcnst, iSlr_half_hr_flg
      endif
      IREC=IREC+1
      IF(SCNAME(1:5).EQ.'NONE ')goto 5
      NCP=NCOMP
      IF(NCOMP.GT.20)NCP=20
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(ICASCF(I),I=1,NCP)
      IREC=IREC+1
      IF(NCOMP.LE.20)goto 8
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(ICASCF(I),I=21,NCOMP)
      IREC=IREC+1
    8 WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)LCTLF
      IREC=IREC+1

c Transfer current record position to library and
c hold in memory for subsequent simulation result entry to library.
    5 IRECPL=IREC

C Ask for result-set information phrase.
C Ignore if in autoexec mode - used the default.
C Generate automatically if an uncertainty analysis.
      write(RSNAME,'(2a)') 'Results for ',cfgroot(1:lnblnk(cfgroot))
      if (NRSIM.gt.1) then
        if (ISIM.eq.1) then
          RSNAME='Base case'
        else
          if (IMET.eq.1) then
            IACT=ISIM/2
            if ((IACT*2).eq.ISIM) then
              write(RSNAME,'(a,a)')LCNG(IACTD(IACT,1)),' - High change'
            else
              write(RSNAME,'(a,a)')LCNG(IACTD(IACT,1)),' - Low change'
            endif
          elseif (IMET.eq.2) then
            write (RSNAME,'(a,10i3)') 'FC:',(IADIR(IX),IX=1,NFA)
          elseif (IMET.eq.3) then
            write (RSNAME,'(a,i4)') 'MC run:',NSIM
          endif
        endif
      endif
      IASK=0
      if (autook) IASK=1
      if (NRSIM.gt.1) IASK=1
      if (IASK.eq.0) then
   50   H(1)='A phrase describing what this result-set represents '
        H(2)='is required. The default phrase includes the root '
        H(3)='name of the model. '
        H(4)='  '
        H(5)='These phrases are generated automatically during an '
        H(6)='uncertainty analysis. '
        CALL EASKS(RSNAME,' Result-set description?',
     &    ' ',40,'standard simulation','Result-set brief descr.',IER,6)
        IF(RSNAME(1:2).EQ.'  ')GOTO 50
      endif
      IREC=NSIM+2
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NST,IRECPL,RSNAME
    7 RETURN

 1000 write(outs,'(A,I5,A)')' MZLS2: error at record',IREC,'.'
      call edisp(iuout,outs)
      goto 7

      END

c ******************** MZLS3
c MZLS3 is called NCOMP times per time-step and
C prior to computation beginning for the next zone)
c to transfer the mean results of the current zone
c computation to the results library. The transferred data
c is dependent on the save option chosen from the simulation
c menu.

C ITSMUS relates to the time-step control facility. It
C is 1 when time-row corresponds to user specified
C time-stepping scheme; 0 when it does not.
C ndbph is number of data blocks/hr for monitor function.

      SUBROUTINE MZLS3(ICOMP,IDAY,ITSMUS,ndbph)
#include "building.h"
C#include "cfd.h"
#include "monitor.h"

      COMMON/TC/ITC,ICNT

      COMMON/FILEP/IFIL
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/BTIME/BTIMEP,BTIMEF

      COMMON/LIBREC/IRECPL

      COMMON/SAVE/ISAVE

      COMMON/PREC1/SNA(MCOM,MS),ZOA(MCOM)
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      common/recver/izver,ipver,iever

      COMMON/PVALC/TPC(MCOM,MS,MN),QPC(MCOM)
      COMMON/PVALS/TPS(MCOM,MS),QPS(MCOM)
      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)

      COMMON/SETUQ/QPLTP(MCOM),QPLTF(MCOM),CONV(MCOM)

      COMMON/COE32/QSOLI(MS,2),QSOLE(MS,2)
      COMMON/COE34/QCASRT(MCOM),QCASCT
      COMMON/COE39/CVIP,CVVP,CVIF,CVVF,QVNP,XX3,CVVPM,CVVFM

C Commons for summary of solar (for results library)
      common/solsum/q1adjz(2),q1outs(2),q2adjz(2),q2lost(2),q2tmc(2),
     &  q2wall(2),q2rem(2)

      COMMON/CCTL/ICASCF(MCOM)
      COMMON/BCTL/NCF,IBSN(MCF,4),IBAN(MCF,3),NBCDT(MCF),
     &IBCDV(MCF,MBCDT,2),NBCDP(MCF,MBCDT),TBCPS(MCF,MBCDT,MBCDP),
     &IBCTYP(MCF,MBCDT,MBCDP),IBCLAW(MCF,MBCDT,MBCDP),
     &BMISCD(MCF,MBCDT,MBCDP,MISC)
      COMMON/PREC2/VOL(MCOM)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)

      COMMON/RHCTLDAT/ZLLDF(MCOM),ZLLDP(MCOM)

      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF

      COMMON/SUS/ISSNY

      COMMON/SAVE0/TVMEM(MCOM,4,2),ZTU(MCOM),ZTL(MCOM),ZPH(MCOM),
     &   ZPC(MCOM),TZPH(MCOM),TZPC(MCOM),TZPHM(MCOM,12),TZPCM(MCOM,12)
      COMMON/LS3/LS3CNT
      COMMON/AVRAGE/IDAVER
      COMMON/ITERST/ITER
      COMMON/LS3A/TMIXP

      common/CASGNS/NCGPER(MCOM,MDTY,MGTY),TCGS(MCOM,MDTY,MGTY,MGPER),
     &        CGSENC(MCOM,MDTY,MGTY,MGPER),CGSENR(MCOM,MDTY,MGTY,MGPER),
     &        CGLAT(MCOM,MDTY,MGTY,MGPER),CGCTL(MCOM,2,MGTY)
      COMMON/LIGHTS/ICGCTL(MCOM)

      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),ARP(MCOM,MS),
     &ARF(MCOM,MS)

C Commons in support of surface energy balance trace.
      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/COE31/HRP(MS,MS),ZHRP(MS),HRF(MS,MS),ZHRF(MS)
      common/concoe/hcip(mcom,ms),hcif(mcom,ms),hcop(mcom,ms),
     &              hcof(mcom,ms)

C nsurf is the number of surfaces to be traced (should be zone based)
C insurf is the index of each surface to be traced
      common/enbal/nsurf,insurf(ms)

      common /bmontr/mnzn,mzone(mcom),montrb,yminb(2),ymaxb(2),
     &               ibplot,irecpb(MDPH),mbstv

C BEMS interaction time for results dump. 
      COMMON/BEMSTIM/bemstime,ibems,bresfil

      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)

      COMMON/ENETZONE/PEZON(MCOM,MGTY),QEZON(MCOM,MGTY)
      COMMON/ENETZONP/ZONPPP(MCOM),ZONQPP(MCOM)
      COMMON/ENETZONF/ZONPP(MCOM),ZONQP(MCOM)
      COMMON/ZONSPMF/ZSPMF1(MCOM),ZSPMF2(MCOM)
      COMMON/ZONSPMP/ZSPMP1(MCOM),ZSPMP2(MCOM)

C Surface energy balance.
      common/SRFBAL/qswrdi(MCOM,MS,2),qlwrdi(MCOM,MS,2),
     &           qconvi(MCOM,MS,2),qcondi(MCOM,MS,2),qstori(MCOM,MS,2),
     &           qcri(MCOM,MS,2),qpltri(MCOM,MS,2),qstore(MCOM,MS,2),
     &           qconde(MCOM,MS,2),qconve(MCOM,MS,2),qlwrde(MCOM,MS,2),
     &           qlwrbd(MCOM,MS,2),qlwrsk(MCOM,MS,2),qswrde(MCOM,MS,2)
     
C Once per hour storage of results, averaged over that hour.
      common/rdcgen/irdact

      INTEGER irdcb
      REAL sTMA,sTMCON,sQM,sTMS,sQSI,sQSE,sQV1,sQV2,sQW1,sQW2
      REAL sZONPP,sZONQP,sQSA,sQCASR,sQCASC,sZRH,sFRAC
      REAL sTMC,sZLL,sSPM1,sSPM2,sqswrdi,sqlwrdi,sqconvi
      REAL sqcondi,sqstori,sqstore,sqconde,sqconve,sqlwrde
      REAL sqlwrbd,sqlwrsk,sqswrde,soutopq,souttrn,sopqin,strnin
      REAL sqair,sqbridge
      common/rdcbld/irdcb(MCOM),sTMA(MCOM),sTMCON(MCOM),sQM(MCOM),
     &            sTMS(MCOM,MS),sQSI(MCOM,MS),sQSE(MCOM,MS),
     &            sQV1(MCOM),sQV2(MCOM),sQW1(MCOM),sQW2(MCOM),
     &            sZONPP(MCOM),sZONQP(MCOM),sQSA(MCOM),sQCASR(MCOM),
     &            sQCASC(MCOM),sZRH(MCOM),sFRAC(MCOM),
     &            sTMC(MCOM,MS,MN),sZLL(MCOM),sSPM1(MCOM),sSPM2(MCOM),
     &            sqswrdi(MCOM,MS),sqlwrdi(MCOM,MS),sqconvi(MCOM,MS),
     &            sqcondi(MCOM,MS),sqstori(MCOM,MS),sqstore(MCOM,MS),
     &            sqconde(MCOM,MS),sqconve(MCOM,MS),sqlwrde(MCOM,MS),
     &            sqlwrbd(MCOM,MS),sqlwrsk(MCOM,MS),sqswrde(MCOM,MS),
     &            soutopq(MCOM),souttrn(MCOM),sopqin(MCOM),
     &            strnin(MCOM),sqair(MCOM),sqbridge(MCOM)

C Once per hour storage of separate convective, radiant and latent W
C for each casual gain type after controls have been applied.
      real sctlperocupc,sctlperocupr,sctlperocupl,sctlperlightc
      real sctlperlightr,sctlperlightl,sctlperequipc,sctlperequipr
      real sctlperequipl,sctlperothc,sctlperothr,sctlperothl
      common/rdccas/sctlperocupc(MCOM),sctlperocupr(MCOM),
     &            sctlperocupl(MCOM),sctlperlightc(MCOM),
     &            sctlperlightr(MCOM),sctlperlightl(MCOM),
     &            sctlperequipc(MCOM),sctlperequipr(MCOM),
     &            sctlperequipl(MCOM),sctlperothc(MCOM),
     &            sctlperothr(MCOM),sctlperothl(MCOM)

C Variables for weekdays, and weekends.
C Assume: Mon=1, Tue=2, Wed=3, Thu=4, Fri=5, Sat=6, Sun=7
      common/wkdtyp/idwe1,idwe2,wkd1,wkd2
      character*10 wkd1, wkd2

C Thermal bridge common block is described in esruprj/edgeo.F
      integer nbrdg, ibrdg
      real psi,lngth,losspercent,totheatloss,thbrpercent
      common/THRBRDG/nbrdg(MCOM),psi(MCOM,16),lngth(MCOM,16),
     &               ibrdg(MCOM,16),losspercent(MCOM),totheatloss(MCOM),
     &               thbrpercent

C This declaration is required to support the bugfix noted below
      logical bInStartup        ! <- function for determining if simulation
                                !    is in startup

      REAL PEZON,QEZON

C Common for occupant/lighting/equipment sensible convective, sensible
C radiant and latent W for future and present in each zone.
      real ctlperocupc,ctlperocupr,ctlperocupl ! occupant after control applied
      real ctlperlightc,ctlperlightr,ctlperlightl ! lighting after control applied
      real ctlperequipc,ctlperequipr,ctlperequipl ! equipment after control applied
      common/applyctl/ctlperocupc(MCOM,2),ctlperocupr(MCOM,2),
     &  ctlperocupl(MCOM,2),ctlperlightc(MCOM,2),ctlperlightr(MCOM,2),
     &  ctlperlightl(MCOM,2),ctlperequipc(MCOM,2),ctlperequipr(MCOM,2),
     &  ctlperequipl(MCOM,2) 

C Temporal db commons.
      COMMON/TDFFLG2/IALLCLM,ICASUAL(MCOM),IZIVENT(MCOM),IRAIRVL(MCOM),
     &       ISETPTT(MCOM),ICTLSTA(MCOM),ISKYLUX,IDBTEXT,IWINDVL,
     &       IWINDDR,IRELHUM,IDIFHSL,IDIRSOL,IGLOHSL,IOBJVEL,IOBJDIR,
     &       ISKYTMP,IGLOVRT,ICASUAL3(MCOM),IACTIV(MCOM),
     &       IBIDIRS(MCOM,MS)

C Local variables for casual gains.
      integer icurrentday  ! julian day
      integer iweekdayweekend ! indicates weekday saturday sunday
      integer iperocup,iperlight,iperequip ! which period of day
      real perocupc,perocupr,perocupl  ! convective/radiant/latent for occupants
      real perlightc,perlightr,perlightl  ! convective/radiant/latent for lights
      real perequipc,perequipr,perequipl  ! convective/radiant/latent for small power
      real avgctlperocupc,avgctlperocupr,avgctlperocupl ! average occupant to write out
      real avgctlperlightc,avgctlperlightr,avgctlperlightl ! average lighting to write out
      real avgctlperequipc,avgctlperequipr,avgctlperequipl ! average equipment to write out
      real avgctlotherc,avgctlotherr,avgctlotherl ! average other (future expansion) to write out
      integer theonectld  ! if non-zero the casual gain type that is controlled.
      PARAMETER       (SMALL=1.0E-15)

      character outs*144,zname*12,zdesc*64
      CHARACTER ZSDES*28,ZSDESC*20,ZSDESS*16,bresfil*72
      logical close

      DIMENSION TMS(MS),QSI(MS),QSE(MS),TMC(MN)
      data ipass/0/,icntb/1/

      IF(NSINC.EQ.1)LS3CNT=0

C Compute mean zone air temperature across time-step.
      if (IDAVER.eq.1) then
        TMA=TFA(ICOMP)
      else
        TMA=(TPA(ICOMP)+TFA(ICOMP))*0.5
      endif

C Call routine MZVAPC to establish the vapour content. 
      CALL MZVAPC(ICOMP,ZRH,CNDS)

C If the bems flag is active then write out the current zone temperature to the bems
C output file (fort.60). Currently only write out the data for optimum start
C stop. This will later be replaced by a dedicated call to res. 
      if(ibems.gt.0) then
C        WRITE(*,'(A,I2)') 'Dumpimg BEMS data ..... Zone ',ICOMP
        WRITE(59,*) '#Zone  Day  Time   Temperature Plant'
        CALL ECLOSE(BTIMEF,1.000,0.001,CLOSE)
        IF(CLOSE)THEN
          WRITE(59,'(I6,1X,F8.4,1X,F8.4,1X,F8.4,2F8.1)') 
     &      ICOMP,FLOAT(IDYF),BTIMEF,TFA(ICOMP),QFA(ICOMP),tfs(ICOMP,1)
        ELSE
          WRITE(59,'(I6,1X,F8.4,1X,F8.4,1X,F8.4,2F8.1)') 
     &      ICOMP,FLOAT(IDYP),BTIMEF,TFA(ICOMP),QFA(ICOMP),tfs(ICOMP,1)
        ENDIF
      endif

C Jump if time-step controller active and current time-row
C does not coincide with user specified time-stepping scheme.
      IF (ITSMUS.EQ.0) return

C Calculate the distribution of casual gains during the startup
C period so that at the first timestep that is saved the present
C values of common block applyctl will be known (otherwise the
C first averaged saved value will be half).

C Derive the separate sensible convective and radiante and latent
C for each type of casual gain in preparation for writing this
C out to the results file. First figure out whether this is a
C weekday saturday or sunday.
      if (IHRF.eq.1) then 
        icurrentday=IDWF
      else
        icurrentday=IDWP
      endif
      if (btimef.gt.24.) then
        icurrentday=IDWF
      endif
      if(icurrentday.EQ.IDWE1)then
        iweekdayweekend=2
      elseif(icurrentday.EQ.IDWE2)then
        iweekdayweekend=3
      else
        iweekdayweekend=1
      endif

C Use data from one of three sources.
      if(ICASUAL(icomp).ne.0)then

C If ICASUAL() set then casual gains lumped into one casual
C gain type and perocupc perocupr perocupl need to be set from
C ctlperocupc() previously filled in casual.F
        perocupc=ctlperocupc(icomp,2)
        perocupr=ctlperocupr(icomp,2)
        perocupl=ctlperocupl(icomp,2)
        perlightc=ctlperlightc(icomp,2)
        perlightr=ctlperlightr(icomp,2)
        perlightl=ctlperlightl(icomp,2)
        perequipc=ctlperequipc(icomp,2)
        perequipr=ctlperequipr(icomp,2)
        perequipl=ctlperequipl(icomp,2)
      elseif(ICASUAL3(icomp).ne.0)then

C If ICASUAL3() set then 3 separate casual gains are set via
C data previously filled in casual.F
        perocupc=ctlperocupc(icomp,2)
        perocupr=ctlperocupr(icomp,2)
        perocupl=ctlperocupl(icomp,2)
        perlightc=ctlperlightc(icomp,2)
        perlightr=ctlperlightr(icomp,2)
        perlightl=ctlperlightl(icomp,2)
        perequipc=ctlperequipc(icomp,2)
        perequipr=ctlperequipr(icomp,2)
        perequipl=ctlperequipl(icomp,2)
      elseif(IACTIV(icomp).ne.0)then
        perocupc=ctlperocupc(icomp,2)
        perocupr=ctlperocupr(icomp,2)
        perocupl=ctlperocupl(icomp,2)
        perlightc=ctlperlightc(icomp,2)
        perlightr=ctlperlightr(icomp,2)
        perlightl=ctlperlightl(icomp,2)
        perequipc=ctlperequipc(icomp,2)
        perequipr=ctlperequipr(icomp,2)
        perequipl=ctlperequipl(icomp,2)
      else

C Data in operations file.
C Calculate the current period id for occupants.
        iperocup=NCGPER(ICOMP,iweekdayweekend,1)
        do 301 I=1,NCGPER(ICOMP,iweekdayweekend,1)
          if(BTIMEF.le.TCGS(ICOMP,iweekdayweekend,1,I+1).and.
     &       BTIMEF.gt.TCGS(ICOMP,iweekdayweekend,1,I)) then
            iperocup=I
          endif
 301    continue
        if(iperocup.gt.0)then
          perocupc=CGSENC(icomp,iweekdayweekend,1,iperocup)
          perocupr=CGSENR(icomp,iweekdayweekend,1,iperocup)
          perocupl=CGLAT(icomp,iweekdayweekend,1,iperocup)
          ctlperocupc(icomp,2)=perocupc
          ctlperocupr(icomp,2)=perocupr
          ctlperocupl(icomp,2)=perocupl
        else
          perocupc=0.0
          perocupr=0.0
          perocupl=0.0
          ctlperocupc(icomp,2)=perocupc
          ctlperocupr(icomp,2)=perocupr
          ctlperocupl(icomp,2)=perocupl
        endif

C Calculate the current period id for lights. If non-zero then
C the current time is within a defined period. Set perlightc/r/l
C based on the CGSENC/R/L value from precomp and the there is
C casual gain control for type 2 then setup ctlperlightc/l/r/l
C for after the FRAC has been applied.
        iperlight=NCGPER(ICOMP,iweekdayweekend,2)
        do 302 I=1,NCGPER(ICOMP,iweekdayweekend,2)
          if(BTIMEF.le.TCGS(ICOMP,iweekdayweekend,2,I+1).and.
     &       BTIMEF.gt.TCGS(ICOMP,iweekdayweekend,2,I)) then
            iperlight=I
          endif
 302    continue
        if(iperlight.gt.0)then
          perlightc=CGSENC(icomp,iweekdayweekend,2,iperlight)
          perlightr=CGSENR(icomp,iweekdayweekend,2,iperlight)
          perlightl=CGLAT(icomp,iweekdayweekend,2,iperlight)
          if(ICGCTL(ICOMP).eq.2)then
            ctlperlightc(icomp,2)=perlightc*CGCTL(ICOMP,2,2)
            ctlperlightr(icomp,2)=perlightr*CGCTL(ICOMP,2,2)
            ctlperlightl(icomp,2)=perlightl*CGCTL(ICOMP,2,2)
          else
            ctlperlightc(icomp,2)=perlightc
            ctlperlightr(icomp,2)=perlightr
            ctlperlightl(icomp,2)=perlightl
          endif
        else
          perlightc=0.0
          perlightr=0.0
          perlightl=0.0
          ctlperlightc(icomp,2)=perlightc
          ctlperlightr(icomp,2)=perlightr
          ctlperlightl(icomp,2)=perlightl
        endif

C Calculate the current period id for equipment. Use similar logic
C to the lighting code block above.
        iperequip=NCGPER(ICOMP,iweekdayweekend,3)
        do 303 I=1,NCGPER(ICOMP,iweekdayweekend,3)
          if(BTIMEF.le.TCGS(ICOMP,iweekdayweekend,3,I+1).and.
     &       BTIMEF.gt.TCGS(ICOMP,iweekdayweekend,3,I)) then
            iperequip=I
          endif
 303    continue
        if(iperequip.gt.0)then
          perequipc=CGSENC(icomp,iweekdayweekend,3,iperequip)
          perequipr=CGSENR(icomp,iweekdayweekend,3,iperequip)
          perequipl=CGLAT(icomp,iweekdayweekend,3,iperequip)
          if(ICGCTL(ICOMP).eq.3)then
            ctlperequipc(icomp,2)=perequipc*CGCTL(ICOMP,2,3)
            ctlperequipr(icomp,2)=perequipr*CGCTL(ICOMP,2,3)
            ctlperequipl(icomp,2)=perequipl*CGCTL(ICOMP,2,3)
          else
            ctlperequipc(icomp,2)=perequipc
            ctlperequipr(icomp,2)=perequipr
            ctlperequipl(icomp,2)=perequipl
          endif
        else
          perequipc=0.0
          perequipr=0.0
          perequipl=0.0
          ctlperequipc(icomp,2)=perequipc
          ctlperequipr(icomp,2)=perequipr
          ctlperequipl(icomp,2)=perequipl
        endif
      endif

C If it is the first timestep of the simulation the present value
C will be unknown so follow the pattern established in casual.F
      if(NSINC.eq.1)then
        ctlperocupc(icomp,1)=ctlperocupc(icomp,2)
        ctlperocupr(icomp,1)=ctlperocupr(icomp,2)
        ctlperocupl(icomp,1)=ctlperocupl(icomp,2)
        ctlperlightc(icomp,1)=ctlperlightc(icomp,2)
        ctlperlightr(icomp,1)=ctlperlightr(icomp,2)
        ctlperlightl(icomp,1)=ctlperlightl(icomp,2)
        ctlperequipc(icomp,1)=ctlperequipc(icomp,2)
        ctlperequipr(icomp,1)=ctlperequipr(icomp,2)
        ctlperequipl(icomp,1)=ctlperequipl(icomp,2)
      endif

C Calculate surface balance data for use later (need to do this now so 
C that present value will be set for first saved record).
      call SURBAL(ICOMP)

C ESRU definition of startup/simulation/post-simulation period.
C 
C 1. Simulation will begin on the first hour (01:00h)
C    of the first day of the start-up period.
C 2. Results storage will begin at midnight of the 
C    first day of the simulation period (00:00h).
C 3. Results storage will end just before midnight 
C    (23:59h) on the last day of the simulation period 
C 4. The simulation will continue for an additional hour, 
C    but no results will be written out. 
C
C However ESRU's implementation (below) is buggy. Usually, the 
C results output starts at midnight of the first day of the 
C simulation period (00:00h) and ends just before midnight (23:59h) 
C on the last day of the simulation period. Yet, if the simulation 
C starts on Jan 1st and has a start-up period duration of at least
C 1 day, results output will actually begin at 01:00h on Jan 1st 
C and continue until 00:00h on the last day of the simulation 
C period. 
C
C Results will be stored for a period that is one hour shorter
C than the actual simulation period. When these results are
C viewed/exported using the res tool, temporal events 
C will appear to occur one hour earlier than expected, and 
C res will report that all simulation data remains at a 
C constant value for the last hour of the simulation.
C The differneces between these implenentations are shown
C below.
C
C
C      Implementation: Start day > Jan 1st
C
C  ^
C  |
C  |
C  |
C  |                          
C  |                                    
C  |                                          o
C  |  o                                   o
C  |   o                             o
C  |    o                       o
C  |     o                 o 
C  |       o          o   
C  |         o  o   
C  |
C  |
C  +-//----|------|------|------|------|------|-->
C         19h    20h    21h    22h    23h    24h
C
C
C
C      Implementation: Start day = Jan 1st
C
C  ^
C  |  Period at end of simulation   ----+
C  |  for which no data is available     \   
C  |                                      \
C  |                                    |<-+->|
C  |                                    
C  |                                    ooooooo
C  |                                o   
C  |                           o
C  |                      o
C  |                 o 
C  |  o         o   
C  |    o  o   
C  |         
C  |
C  +-//----|------|------|------|------|------|-->
C         19h    20h    21h    22h    23h    24h
C
C CETC's implementation:
C
      if (bInStartup())then
C       write(6,*) 'bInstartup is true'
C       write(6,*) IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,BTIMEP,
C     &    BTIMEF,ISSNY,IDAY,ISDS,ISDF

C While in startup check if ISSNY is 1 and if IDAY is 1 reset ISSNY.
          if(issny.eq.1.and.iday.eq.1) issny=0
       return
      else
C       write(6,*) 'bInstartup is false'
C       write(6,*) IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS,BTIMEP,
C     &    BTIMEF,ISSNY,IDAY,ISDS,ISDF
      endif

C Previous implementation:
C
C Jump if still in start-up period.
C      IF(ISSNY.EQ.0) goto 88
C      IF(IDAY.EQ.1) goto 881
C      IF(IDAY.EQ.365.AND.IHRP.EQ.24.AND.ITS.EQ.NTSTEP.AND.
C     &   ISDS.EQ.1) goto  991
C      return
C 881  ISSNY=0
C 88   IF(IDAY.EQ.ISDS-1.AND.IHRP.EQ.24) goto  991
C      IF(IDAY.LT.ISDS) return
C      IF(IDAY.EQ.ISDF.AND.IHRP.EQ.24) then
C        ipass=0
C        return
C      endif     

  991 IUNIT=IFIL+2
      IREC=IRECPL
      irecpb(icntb)=irecpl
      icntb=icntb+1
      if(icntb.gt.ndbph) icntb=1
      
C Once per hour results storage. Determine whether storage at this
C time step is needed; ie only during last time step within
C current hour.
      if(irdact.gt.0) then
        if(its.eq.ntstep) then
          irdnow=1
        else
          irdnow=0
        endif
      endif

C If zone temperature monitor flag is active then
C go and set up x & y axes for subsequent plotting.
C ipass ensures that this is done only once.
      if(montrb.eq.1.and.ipass.eq.0) then
        if(issny.eq.1) then
          call zngrph(1,isdf,0,1)
        else
          call zngrph(iday+1,isdf,0,1)
        endif
        call usrmsg(' ',' Monitoring now in progress...','-')
        ipass=1
        ibplot=1
      endif

C Determine control function for current zone.
      IC=ICASCF(ICOMP)

c Determine mean control node temperature (same as mean air
c temperature if control point node - CPN - is located at air
c point).

c If no control imposed assume CPN is air point.
      IF(NCF.EQ.0.OR.IC.EQ.0)goto 2
      IF(IBSN(IC,1).EQ.0)INODE=1
      IF(IBSN(IC,2).EQ.0.AND.IBSN(IC,3).EQ.0.AND.
     &IBSN(IC,1).GT.0)INODE=1
      IF(IBSN(IC,2).GT.0.AND.IBSN(IC,3).EQ.0)INODE=2
      IF(IBSN(IC,2).GT.0.AND.IBSN(IC,3).GT.0)INODE=3
      IF(IBSN(IC,1).EQ.-2)INODE=4
      IF(IBSN(IC,1).EQ.-3)INODE=5
      IF(IBSN(IC,1).EQ.99)INODE=6
      IF(IBSN(IC,1).EQ.-1)INODE=7
      goto (2,3,4,5,51,61,71),INODE

c Control point is air node.
    2 TMCON=TMA
      goto 1

c Control node is situated at a surface.
    3 ISURF=IBSN(IC,2)
      if (IDAVER.EQ.1) then
        TMCON=TFS(ICOMP,ISURF)
      else
        TMCON=(TPS(ICOMP,ISURF)+TFS(ICOMP,ISURF))*0.5
      endif
      goto 1

c Control node is within a construction.
    4 IPOS=IBSN(IC,3)
      ISURF=IBSN(IC,2)
      if (IDAVER.EQ.1) then
        TMCON=TFC(ICOMP,ISURF,IPOS)
      else
        TMCON=(TPC(ICOMP,ISURF,IPOS)+TFC(ICOMP,ISURF,IPOS))*0.5
      endif
      goto 1

c Control node is at the zone weighted temperature point.
    5 ITER=ITER-1
      CALL MZMIXT(ICOMP,TMRT,TMIX)
      if (IDAVER.EQ.1) then
        TMCON=TMIX
      else
        TMCON=(TMIXP+TMIX)*0.5
      endif
      TMIXP=TMIX
      goto 1

C Control point is external air temperature.
   51 TMCON=(TP+TF)*0.5
      goto 1

c Control point is the air point in another zone.
   61 IZ=-IBSN(IC,1)
      TMCON=(TFA(IZ)+TPA(IZ))*0.5
      goto 1

c Control point is a plant component node (nb to be implemented).
   71 TMCON=TMA

c Compute mean energy injection/extraction at plant
c interaction node (PIN). Also remember convective plant
C injection (qma) for trace).
    1 QM=0.
      qma=0.
      IF(NCF.EQ.0.OR.IC.EQ.0)goto 6
      INODE=1
      IF(IBAN(IC,3).GT.0)INODE=3
      IF(IBAN(IC,2).GT.0.AND.IBAN(IC,3).EQ.0)INODE=2
      IF(IBAN(IC,1).EQ.-1)INODE=4
      IF(IBAN(IC,1).EQ.-2)INODE=5
      goto (7,8,9,91,92),INODE

c PIN is air point.
    7 if (IDAVER.EQ.1) then
        QM=QFA(ICOMP)
      else
        QM=(QPA(ICOMP)+QFA(ICOMP))*0.5
      endif
      qma=QM
      goto 6

c PIN is at surface.
    8 if (IDAVER.EQ.1) then
        QM=QFS(ICOMP)
      else
        QM=(QPS(ICOMP)+QFS(ICOMP))*0.5
      endif
      qma=0.
      goto 6

c PIN is within construction.
    9 if (IDAVER.EQ.1) then
        QM=QFC(ICOMP)
      else
        QM=(QPC(ICOMP)+QFC(ICOMP))*0.5
      endif
      qma=0.
      goto 6

c PIN is at a plant component node (NB not yet implemented).
   91 goto 7

c PIN is a mix of radiant/convective flux (eg radiator).
   92 if (IDAVER.EQ.1) then
        QM=QPLTF(ICOMP)
      else
        QM=(QPLTP(ICOMP)+QPLTF(ICOMP))*0.5
      endif
      qma=QM*CONV(ICOMP)

 6    CONTINUE

C Compute the zone-side latent load
      if (IDAVER.eq.1) then
        ZLL=ZLLDF(ICOMP)
      else
        ZLL=(ZLLDP(ICOMP)+ZLLDF(ICOMP))*.5
      endif
      
C Store the currently computed value.
      ZLLDP(ICOMP)=ZLLDF(ICOMP)

C Compute the special component/material output for the 
C zone. 
      if (IDAVER.eq.1) then
        SPM1=ZSPMF1(ICOMP)
        SPM2=ZSPMF2(ICOMP)
      else
        SPM1=(ZSPMF1(ICOMP)+ZSPMP1(ICOMP))*0.5
        SPM2=(ZSPMF2(ICOMP)+ZSPMP2(ICOMP))*0.5
      endif   

C Store the currently computed value.
      ZSPMP1(ICOMP)=ZSPMF1(ICOMP)
      ZSPMP2(ICOMP)=ZSPMF2(ICOMP)    
 
C Write out solar zone info (to see if similar to that of solar.F)
C      write(6,*) 'solsum blibsv',BTIMEF,ICOMP,q1adjz(2),q1outs(2),
C     &   q2adjz(2),q2lost(2),q2tmc(2),q2wall(2),q2rem(2)

C Temporary statement for dumping structured mesh temperatures.
C      call TEMPSD(icomp)
      if (ISAVE.eq.0) then

C Save appropriate data for ISAVE=0. Initialise during first pass
C of each zone. Find current month (IS0M) for the simulation day.
        CALL EDAYR(IDAY,IS0D,IS0M)
        IF (ICOMP.EQ.1) LS3CNT=LS3CNT+1
        IF (LS3CNT.eq.1)then
          DO 50 I=1,4
            DO 60 J=1,2
              TVMEM(ICOMP,I,J)=0
   60       CONTINUE
   50     CONTINUE
          ZTU(ICOMP)=-1000.0
          ZTL(ICOMP)=1000.0
          ZPH(ICOMP)=0.0
          ZPC(ICOMP)=0.0
          TZPH(ICOMP)=0.0
          TZPC(ICOMP)=0.0
          do 15 im=1,12
            TZPHM(ICOMP,im)=0.0
            TZPCM(ICOMP,im)=0.0
   15     continue
        endif
        if (IDAVER.EQ.1) then
          TIME=BTIMEF
        else
          TIME=(BTIMEP+BTIMEF)*0.5
        endif
        NDAY=IDYP
        IF(TIME.LE.BTIMEF) goto 551
          TIME=TIME-12.0
          NDAY=IDYF
  551   IF (TMA.LE.ZTU(ICOMP).AND.LS3CNT.NE.1) goto 17
          ZTU(ICOMP)=TMA
          TVMEM(ICOMP,1,1)=TIME
          TVMEM(ICOMP,1,2)=FLOAT(NDAY)
   17   IF (TMA.GE.ZTL(ICOMP).AND.LS3CNT.NE.1) goto 18
          ZTL(ICOMP)=TMA
          TVMEM(ICOMP,2,1)=TIME
          TVMEM(ICOMP,2,2)=FLOAT(NDAY)
   18   call eclose(QM,0.00,0.001,close)
        IF (close.AND.LS3CNT.NE.1) return
        IF (close.AND.LS3CNT.EQ.1) goto 21
        IF (QM.LT.0.0.AND.LS3CNT.NE.1) goto 19

C Increment zone heating demand (total and monthly).
        TZPH(ICOMP)=TZPH(ICOMP)+QM
        TZPHM(ICOMP,IS0M)=TZPHM(ICOMP,IS0M)+QM
        IF(QM.LE.ZPH(ICOMP).AND.LS3CNT.NE.1) return
        ZPH(ICOMP)=QM
   21   TVMEM(ICOMP,3,1)=TIME
        TVMEM(ICOMP,3,2)=FLOAT(NDAY)
        IF(LS3CNT.EQ.1)goto 22
        return

C Increment zone cooling demand (total and monthly).
   19   TZPC(ICOMP)=TZPC(ICOMP)+QM
        TZPCM(ICOMP,IS0M)=TZPCM(ICOMP,IS0M)+QM
        IF(QM.GE.ZPC(ICOMP).AND.LS3CNT.NE.1) return
        ZPC(ICOMP)=QM
   22   TVMEM(ICOMP,4,1)=TIME
        TVMEM(ICOMP,4,2)=FLOAT(NDAY)
      elseif (ISAVE.eq.1) then

c Write to results library if ISAVE = 1.
        if(irdact.eq.0) then
          WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)TMA,TMCON,QM
          IREC=IREC+1
        else
          sTMA(ICOMP)=sTMA(ICOMP)+TMA
          sTMCON(ICOMP)=sTMCON(ICOMP)+TMCON
          sQM(ICOMP)=sQM(ICOMP)+QM
          if(irdnow.eq.1) then
            irdc=irdcb(ICOMP)
            WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &              sTMA(ICOMP)/irdc,sTMCON(ICOMP)/irdc,sQM(ICOMP)/irdc
            IREC=IREC+1
            sTMA(ICOMP)=0.0
            sTMCON(ICOMP)=0.0
            sQM(ICOMP)=0.0
          endif
        endif
      elseif (ISAVE.ge.2) then

C Need to compute additional averages.
C Calculate surface based averages first: Internal T; External solar 
C   abs; Internal solar abs.
        NSUR=NCONST(ICOMP)
        do 10 I=1,NSUR
          if (IDAVER.eq.1) then
            TMS(I)=TFS(ICOMP,I)
            QSE(I)=QSOLE(I,2)
            QSI(I)=QSOLI(I,2)
          else
            TMS(I)=(TPS(ICOMP,I)+TFS(ICOMP,I))*0.5
            QSI(I)=(QSOLI(I,1)+QSOLI(I,2))*0.5
            QSE(I)=(QSOLE(I,1)+QSOLE(I,2))*0.5
          endif
 10     continue

C Zone based averages: Infiltration; Ventilation [remember  
C   ventilation conductance (CVV?M) is adjusted to outside 
C   temperatures.]; 
        if (IDAVER.eq.1) then
          QV1=CVIF*(TF-TFA(ICOMP))
          X=TF
          Y=TP
          IF(ABS(X).LT.SMALL)X=1.
          IF(ABS(Y).LT.SMALL)Y=1.
          QV2=CVVFM*X-CVVF*TFA(ICOMP)

C Solar distribution values: QW1 slot used for solar entering from outside,
C QW2 slot used for solar from adjacent zones and QSA is solar absorbed.
          QW1=q1outs(2)
          QW2=q1adjz(2)
          QSA=q2tmc(2)+q2wall(2)+q2rem(2)

C Thermal bridges.
          qbridge=totheatloss(icomp)*(TF-TFA(icomp))
        else
          QV1=(CVIP*(TP-TPA(ICOMP))+CVIF*(TF-TFA(ICOMP)))*0.5
          X=TF
          Y=TP
          IF(ABS(X).LT.SMALL)X=1.
          IF(ABS(Y).LT.SMALL)Y=1.
          QV2=(CVVPM*Y-CVVP*TPA(ICOMP)+CVVFM*X-CVVF*TFA(ICOMP))*0.5

C Solar trace rather than default window stuff.
          QW1=(q1outs(1)+q1outs(2))*0.5
          QW2=(q1adjz(1)+q1adjz(2))*0.5
          QSA=((q2tmc(1)+q2tmc(2))*0.5)+((q2wall(1)+q2wall(2))*0.5)+
     &        ((q2rem(1)+q2rem(2))*0.5)

C Thermal bridges.
          qbridge=totheatloss(icomp)*
     &            ((TF-TFA(icomp))+(TP-TPA(icomp)))*0.5
        endif

C Total Real and reactive power consumptions for each zone. 
        ZONPP(ICOMP)=0.0
        ZONQP(ICOMP)=0.0
        EZONPP=0.0
        EZONQP=0.0
C For each casual gain type.
        DO 77 K=1,MGTY
          ZONPP(ICOMP)=ZONPP(ICOMP)+PEZON(ICOMP,K)
          ZONQP(ICOMP)=ZONQP(ICOMP)+QEZON(ICOMP,K)
  77    CONTINUE
        IF(IDAVER.EQ.1)THEN
          EZONPP=ZONPP(ICOMP)
          EZONQP=ZONQP(ICOMP)
          ZONPPP(ICOMP)=ZONPP(ICOMP)
          ZONQPP(ICOMP)=ZONQP(ICOMP)
        ELSE
          EZONPP=(ZONPP(ICOMP)+ZONPPP(ICOMP))*0.5
          EZONQP=(ZONQP(ICOMP)+ZONQPP(ICOMP))*0.5
          ZONPPP(ICOMP)=ZONPP(ICOMP)
          ZONQPP(ICOMP)=ZONQP(ICOMP)
        ENDIF

C Casual gains and average FRACtion controlled.
        QCASR=QCASRT(icomp)*(ZOA(ICOMP))/2.0
        QCASC=QCASCT/2.0
        if(ICGCTL(ICOMP).gt.0)then
          FRAC=(CGCTL(ICOMP,1,ICGCTL(ICOMP))+
     &          CGCTL(ICOMP,2,ICGCTL(ICOMP)))/2.0
        else
          FRAC=1.0
        endif

C Comput averages for each of the separate casual gains (after ctl applied).
        if (IDAVER.EQ.1) then
          avgctlperocupc=ctlperocupc(icomp,2)
          avgctlperocupr=ctlperocupr(icomp,2)
          avgctlperocupl=ctlperocupl(icomp,2)
          avgctlperlightc=ctlperlightc(icomp,2)
          avgctlperlightr=ctlperlightr(icomp,2)
          avgctlperlightl=ctlperlightl(icomp,2)
          avgctlperequipc=ctlperequipc(icomp,2)
          avgctlperequipr=ctlperequipr(icomp,2)
          avgctlperequipl=ctlperequipl(icomp,2)
          avgctlotherc=0.0
          avgctlotherr=0.0
          avgctlotherl=0.0
        else
          avgctlperocupc=(ctlperocupc(icomp,1)+
     &                    ctlperocupc(icomp,2))/2.0
          avgctlperocupr=(ctlperocupr(icomp,1)+
     &                    ctlperocupr(icomp,2))/2.0
          avgctlperocupl=(ctlperocupl(icomp,1)+
     &                     ctlperocupl(icomp,2))/2.0
          avgctlperlightc=(ctlperlightc(icomp,1)+
     &                     ctlperlightc(icomp,2))/2.0
          avgctlperlightr=(ctlperlightr(icomp,1)+
     &                     ctlperlightr(icomp,2))/2.0
          avgctlperlightl=(ctlperlightl(icomp,1)+
     &                     ctlperlightl(icomp,2))/2.0
          avgctlperequipc=(ctlperequipc(icomp,1)+
     &                     ctlperequipc(icomp,2))/2.0
          avgctlperequipr=(ctlperequipr(icomp,1)+
     &                     ctlperequipr(icomp,2))/2.0
          avgctlperequipl=(ctlperequipl(icomp,1)+
     &                     ctlperequipl(icomp,2))/2.0
          avgctlotherc=0.0
          avgctlotherr=0.0
          avgctlotherl=0.0
        endif

C Debug
C        write(6,'(A,i2,9F9.2)') 
C     &   ' Casual pres ocup/light/equip convec ctl applied',
C     &   icomp,ctlperocupc(icomp,1),ctlperocupc(icomp,2),
C     &   ctlperlightc(icomp,1),ctlperlightc(icomp,2),
C     &   ctlperequipc(icomp,1),ctlperequipc(icomp,2),
C     &   avgctlperocupc,avgctlperlightc,avgctlperequipc

C Save future to current for these casual gains. Note: the
C future value is still needed for the trace so delay zeroing
C the future value until after the trace logic (i.e. after 700).
        ctlperocupc(icomp,1)=ctlperocupc(icomp,2)
        ctlperocupr(icomp,1)=ctlperocupr(icomp,2)
        ctlperocupl(icomp,1)=ctlperocupl(icomp,2)
        ctlperlightc(icomp,1)=ctlperlightc(icomp,2)
        ctlperlightr(icomp,1)=ctlperlightr(icomp,2)
        ctlperlightl(icomp,1)=ctlperlightl(icomp,2)
        ctlperequipc(icomp,1)=ctlperequipc(icomp,2)
        ctlperequipr(icomp,1)=ctlperequipr(icomp,2)
        ctlperequipl(icomp,1)=ctlperequipl(icomp,2)

C Get the ratio between the W for occup/lighting and small power
C to use when writing out surface energy balance data.
        avgradtot=avgctlperequipr+avgctlperlightr+avgctlperocupr+
     &            avgctlotherr
        if(avgradtot.lt.0.001)then
          fracrlight=1.0
          fracrocup=1.0
          fracrequip=1.0
          fracroth=1.0
        else
          fracrlight=avgctlperlightr/avgradtot
          fracrocup=avgctlperocupr/avgradtot
          fracrequip=avgctlperequipr/avgradtot
          fracroth=avgctlotherr/avgradtot
        endif 

C Loop through all of the surfaces and get the convective flux at
C the inside and outside face to sum up for the air balance reporting.
C Note: to express in terms of the air node negate the value.
        outopq=0.
        outtrn=0.
        opqin=0.
        trnin=0.
        DO 310 ISFN=1,NSUR
          QC=(qconvi(ICOMP,ISFN,1)+qconvi(ICOMP,ISFN,2))*0.5
          if (IE(ICOMP,ISFN).EQ.0) then          
            if (ITMCFL(ICOMP,ISFN).EQ.0) then

C Exterior BC opaque.
              outopq=outopq-QC
            else

C Exterior BC transparent.
              outtrn=outtrn-QC
            endif
          else
            if (ITMCFL(ICOMP,ISFN).EQ.0) then
          
C Non-exterior BC opaque.
              opqin=opqin-QC
            else
          
C Non-exterior BC transparent.
              trnin=trnin-QC
            endif
          endif
  310   continue
C Debug...
C        write(outs,'(A,2F9.2,A,2F9.2)')'Opaque conv: ext int',outopq,
C     &    opqin,' Transp conv: ext int',outtrn,trnin
C        call edisp(itu,outs)

C Prior to izver 4 the zone air node energy balance information
C did not include storage of the air mass itself. The following
C qair could be written to the results file to improve the reporting
C of energy balances.
         qair=real(ntstep)*0.33*VOL(ICOMP)*(TPA(ICOMP)-TFA(ICOMP))

C As of izver 4 explicit casual gain information is written into
C the results library. The first record has appended to it the sensible
C convective W for occup/light/equip/other as well as qair.
C The second record has the sensible radiant W for occup/light/equip/other
C as well as thermal bridge loss.
C The third record has the latent W for occup/light/equip/other (there
C is one un-used slot).

C Now write to results library if ISAVE = 2, 3, 4.
        if(irdact.eq.0) then
          WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)TMA,QM,
     &        (TMS(I),I=1,NSUR),QV1,QV2,TMCON,outopq,outtrn,
     &        avgctlperocupc,avgctlperlightc,avgctlperequipc,
     &        avgctlotherc,qair
          IREC=IREC+1

          WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)QW1,QW2,EZONPP,
     &        EZONQP,QSA,(QSI(I),I=1,NSUR),opqin,trnin,
     &        avgctlperocupr,avgctlperlightr,avgctlperequipr,
     &        avgctlotherr,qbridge
          IREC=IREC+1

C Special materials/components output (TEMPORARILY) written to this slot. Due 
C to limited space only ONE special output per zone is allowed, with a 
C maximum of two variables stored. Separate latent casuals also appended.
C To help with display of controlled casual gains the value of ICGCTL(icomp)
C is written as well.
          theonectld=ICGCTL(icomp) 
          WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(QSE(I),I=1,NSUR),
     &      QCASR,QCASC,ZRH,ZLL,FRAC,SPM1,SPM2,
     &      avgctlperocupl,avgctlperlightl,avgctlperequipl,
     &      avgctlotherl,theonectld
          IREC=IREC+1


C << the following code is for testing >>
C SQL results write.
C          QT=CHAR(39)
C          call DATETIME(IAVE,DT)
C          write (sqls,'(9(g10.4,1x))') TMA,QM,QV1,QV2,TMCON,outopq,
C     &             outtrn,QW1,QW2
C          write (sqls,'(9(g10.4,1x))') 
C          call sdelim(sqls,sqlsd,'C',IW)
C          write (doit,'(6a,i2.2,3a,f7.3,3a)') 'echo ',QT,
C     & 'replace INTO indoor_climate (ref, ic_code, datetime, ',
C     & 'ic_item, value) VALUES ("',cfgroot(1:lnblnk(cfgroot)),'_',
C     & ICOMP,'","ESP",',DT,',"DB",',TMA,')',QT,
C     & '|mysql -u manager -pmanager -h130.159.102.16 merit'
C          write (6,*) doit
C          call runit(doit,'-')
C          write (doit,'(6a,i2.2,3a,f7.3,3a)') 'echo ',QT,
C     & 'replace INTO indoor_climate (ref, ic_code, datetime, ',
C     & 'ic_item, value) VALUES ("',cfgroot(1:lnblnk(cfgroot)),'_',
C     & ICOMP,'","ESP",',DT,',"RH",',ZRH,')',QT,
C     & '|mysql -u manager -pmanager -h130.159.102.16 merit'
C          write (6,*) doit
C          call runit(doit,'-')
C          write (doit,'(6a,i2.2,3a,2(f8.1,a),2a)') 'echo ',QT,
C     & 'replace INTO energy (ref, meter_id, fuel_type, r_type,',
C     & 'to_datetime, dial_1, tot) VALUES ("',cfgroot(1:lnblnk(cfgroot)),
C     & '_',ICOMP,'","HF","HF","ESP",',DT,',',QM,',',QM,')',QT,
C     & '|mysql -u manager -pmanager -h130.159.102.16 merit'
C          write (6,*) doit
C          call runit(doit,'-')

C If the bems flag is active then write out the current zone temperature to the bems
C output file (fort.60). Currently only write out the data for optimum start
C stop. This will later be replaced by a dedicated call to res. 
          if(ibems.gt.0) then
C            WRITE(*,'(A,I2)') 'Dumpimg BEMS data ..... Zone ',ICOMP
            WRITE(60,*) '#Zone  Day  Time   Temperature '
            CALL ECLOSE(BTIMEF,1.000,0.001,CLOSE)
            IF(CLOSE)THEN
              WRITE(60,'(I6,1X,F8.4,1X,F8.4,1X,F8.4)') 
     &                    ICOMP,FLOAT(IDYF),BTIMEF,TFA(ICOMP)
            ELSE
              WRITE(60,'(I6,1X,F8.4,1X,F8.4,1X,F8.4)') 
     &                    ICOMP,FLOAT(IDYP),BTIMEF,TFA(ICOMP)
            ENDIF
          endif
        else

C Add to the saved common the data that would have been written
C out to the file.
          soutopq(ICOMP)=soutopq(ICOMP)+outopq
          souttrn(ICOMP)=souttrn(ICOMP)+outtrn
          sopqin(ICOMP)=sopqin(ICOMP)+opqin
          strnin(ICOMP)=strnin(ICOMP)+trnin
          sTMA(ICOMP)=sTMA(ICOMP)+TMA
          sQM(ICOMP)=sQM(ICOMP)+QM
          do 101 I=1,NSUR
            sTMS(ICOMP,I)=sTMS(ICOMP,I)+TMS(I)
            sQSI(ICOMP,I)=sQSI(ICOMP,I)+QSI(I)
            sQSE(ICOMP,I)=sQSE(ICOMP,I)+QSE(I)
 101      continue
          sQV1(ICOMP)=sQV1(ICOMP)+QV1
          sQV2(ICOMP)=sQV2(ICOMP)+QV2
          sTMCON(ICOMP)=sTMCON(ICOMP)+TMCON
          sQW1(ICOMP)=sQW1(ICOMP)+QW1
          sQW2(ICOMP)=sQW2(ICOMP)+QW2
          sZONPP(ICOMP)=sZONPP(ICOMP)+ZONPP(ICOMP)
          sZONQP(ICOMP)=sZONQP(ICOMP)+ZONQP(ICOMP)
          sQSA(ICOMP)=sQSA(ICOMP)+QSA
          sQCASR(ICOMP)=sQCASR(ICOMP)+QCASR
          sQCASC(ICOMP)=sQCASC(ICOMP)+QCASC
          sZRH(ICOMP)=sZRH(ICOMP)+ZRH
          sZLL(ICOMP)=sZLL(ICOMP)+ZLL
          sFRAC(ICOMP)=sFRAC(ICOMP)+FRAC
          sSPM1(ICOMP)=sSPM1(ICOMP)+SPM1
          sSPM2(ICOMP)=sSPM2(ICOMP)+SPM2
          sqair(ICOMP)=sqair(ICOMP)+qair
          sqbridge(ICOMP)=sqbridge(ICOMP)+qbridge

C Save back the current written out values of separate casual gains.
          sctlperocupc(ICOMP)=sctlperocupc(ICOMP)+avgctlperocupc
          sctlperocupr(ICOMP)=sctlperocupr(ICOMP)+avgctlperocupr
          sctlperocupl(ICOMP)=sctlperocupl(ICOMP)+avgctlperocupl
          sctlperlightc(ICOMP)=sctlperlightc(ICOMP)+avgctlperlightc
          sctlperlightr(ICOMP)=sctlperlightr(ICOMP)+avgctlperlightr
          sctlperlightl(ICOMP)=sctlperlightl(ICOMP)+avgctlperlightl
          sctlperequipc(ICOMP)=sctlperequipc(ICOMP)+avgctlperequipc
          sctlperequipr(ICOMP)=sctlperequipr(ICOMP)+avgctlperequipr
          sctlperequipl(ICOMP)=sctlperequipl(ICOMP)+avgctlperequipl
          sctlperothc(ICOMP)=sctlperothc(ICOMP)+avgctlotherc
          sctlperothr(ICOMP)=sctlperothr(ICOMP)+avgctlotherr
          sctlperothl(ICOMP)=sctlperothl(ICOMP)+avgctlotherl

C Debug
C          write(6,'(A,2i2,8F9.2)') 
C     &    ' Casual acum ocup/light/equip convec',icomp,irdnow,
C     &    sctlperocupc(icomp),sctlperlightc(icomp),ctlperequipc(icomp),
C     &    sctlperothc(icomp),avgctlperocupc,avgctlperlightc,
C     &    avgctlperequipc,avgctlotherc

          if(irdnow.eq.1) then

C Debug
C            write(6,'(A,2i2,4F9.2)') 
C     &        ' Casual written ocup/light/equip/other convec',
C     &        icomp,irdnow,sctlperocupc(icomp)/irdc,
C     &        sctlperlightc(icomp)/irdc,
C     &        ctlperequipc(icomp)/irdc,
C     &        sctlperothc(icomp)/irdc

            irdc=irdcb(ICOMP)
            WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &        sTMA(ICOMP)/irdc,sQM(ICOMP)/irdc,
     &        (sTMS(ICOMP,I)/irdc,I=1,NSUR),sQV1(ICOMP)/irdc,
     &        sQV2(ICOMP)/irdc,sTMCON(ICOMP)/irdc,soutopq(ICOMP)/irdc,
     &        souttrn(ICOMP)/irdc,sctlperocupc(ICOMP)/irdc,
     &        sctlperlightc(ICOMP)/irdc,sctlperequipc(ICOMP)/irdc,
     &        sctlperothc(ICOMP)/irdc,sqair(ICOMP)/irdc

            IREC=IREC+1
            WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &        sQW1(ICOMP)/irdc,sQW2(ICOMP)/irdc,
     &        sZONPP(ICOMP)/irdc,sZONQP(ICOMP)/irdc,
     &        sQSA(ICOMP)/irdc,(sQSI(ICOMP,I)/irdc,I=1,NSUR),
     &        sopqin(ICOMP)/irdc,strnin(ICOMP)/irdc,
     &        sctlperocupr(ICOMP)/irdc,sctlperlightr(ICOMP)/irdc,
     &        sctlperequipr(ICOMP)/irdc,sctlperothr(ICOMP)/irdc,
     &        sqbridge(ICOMP)/irdc
            IREC=IREC+1

            WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &        (sQSE(ICOMP,I)/irdc,I=1,NSUR),
     &        sQCASR(ICOMP)/irdc,sQCASC(ICOMP)/irdc,
     &        sZRH(ICOMP)/irdc,sZLL(ICOMP)/irdc,sFRAC(ICOMP)/irdc,
     &        sSPM1(ICOMP)/irdc,sSPM2(ICOMP)/irdc,
     &        sctlperocupl(ICOMP)/irdc,sctlperlightl(ICOMP)/irdc,
     &        sctlperequipl(ICOMP)/irdc,sctlperothl(ICOMP)/irdc,
     &        theonectld
            IREC=IREC+1

C Reset the saved values after having written the data.
            soutopq(ICOMP)=0.
            souttrn(ICOMP)=0.
            sopqin(ICOMP)=0.
            strnin(ICOMP)=0.
            sTMA(ICOMP)=0.
            sQM(ICOMP)=0.
            do 102 I=1,NSUR
              sTMS(ICOMP,I)=0.
              sQSI(ICOMP,I)=0.
              sQSE(ICOMP,I)=0.
 102        continue
            sQV1(ICOMP)=0.
            sQV2(ICOMP)=0.
            sTMCON(ICOMP)=0.
            sQW1(ICOMP)=0.
            sQW2(ICOMP)=0.
            sZONPP(ICOMP)=0.
            sZONQP(ICOMP)=0.
            sQSA(ICOMP)=0.
            sQCASR(ICOMP)=0.
            sQCASC(ICOMP)=0.
            sZRH(ICOMP)=0.
            sZLL(ICOMP)=0.
            sFRAC(ICOMP)=0.
            sSPM1(ICOMP)=0.
            sSPM2(ICOMP)=0.
            sqair(ICOMP)=0.0
            sqbridge(ICOMP)=0.0
            sctlperocupc(ICOMP)=0.
            sctlperocupr(ICOMP)=0.
            sctlperocupl(ICOMP)=0.
            sctlperlightc(ICOMP)=0.
            sctlperlightr(ICOMP)=0.
            sctlperlightl(ICOMP)=0.
            sctlperequipc(ICOMP)=0.
            sctlperequipr(ICOMP)=0.
            sctlperequipl(ICOMP)=0.
            sctlperothc(ICOMP)=0.
            sctlperothr(ICOMP)=0.
            sctlperothl(ICOMP)=0.
          endif
        endif

C If higher save level than 2 then calculate additional information.
C ISAVE=3 - intra-surface temperatures.
        if (ISAVE.eq.3) then
          DO 30 I=1,NSUR
            NN=NNDC(ICOMP,I)-1
            DO 40 J=1,NN
              if (IDAVER.EQ.1) then
                TMC(J)=TFC(ICOMP,I,J)
              else
                TMC(J)=(TPC(ICOMP,I,J)+TFC(ICOMP,I,J))*0.5
              endif
   40       CONTINUE
            if(irdact.eq.0) then
              WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)(TMC(J),J=1,NN)
              IREC=IREC+1
            else
              do 401 J=1,NN
                sTMC(ICOMP,I,J)=sTMC(ICOMP,I,J)+TMC(J)
  401         continue
              if(irdnow.eq.1) then
                WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &             (sTMC(ICOMP,I,J)/irdcb(ICOMP),J=1,NN)
                IREC=IREC+1
                do 402 J=1,NN
                  sTMC(ICOMP,I,J)=0.
  402           continue
              endif
            endif
   30     CONTINUE

C ISAVE=4 - surface fluxes.
        elseif (ISAVE.eq.4) then
          IZFLG=0
          DO 300 ISF=1,NSUR

C Outside surface node temperature and average surface energy transfers.
            if (IDAVER.EQ.1) then
              TMC(1)=TFC(ICOMP,ISF,1)
              avqswrdi=qswrdi(ICOMP,ISF,2)
              avqlwrdi=qlwrdi(ICOMP,ISF,2)
              avqconvi=qconvi(ICOMP,ISF,2)
              avqcondi=qcondi(ICOMP,ISF,2)
              avqstori=qstori(ICOMP,ISF,2)
              avqcri=  qcri(ICOMP,ISF,2)
              avqpltri=qpltri(ICOMP,ISF,2)
              avqstore=qstore(ICOMP,ISF,2)
              avqconde=qconde(ICOMP,ISF,2)
              avqconve=qconve(ICOMP,ISF,2)
              avqlwrde=qlwrde(ICOMP,ISF,2)
              avqlwrbd=qlwrbd(ICOMP,ISF,2)
              avqlwrsk=qlwrsk(ICOMP,ISF,2)
              avqswrde=qswrde(ICOMP,ISF,2)
            else
              TMC(1)=(TPC(ICOMP,ISF,1)+TFC(ICOMP,ISF,1))*0.5
              avqswrdi=(qswrdi(ICOMP,ISF,1)+qswrdi(ICOMP,ISF,2))*0.5
              avqlwrdi=(qlwrdi(ICOMP,ISF,1)+qlwrdi(ICOMP,ISF,2))*0.5
              avqconvi=(qconvi(ICOMP,ISF,1)+qconvi(ICOMP,ISF,2))*0.5
              avqcondi=(qcondi(ICOMP,ISF,1)+qcondi(ICOMP,ISF,2))*0.5
              avqstori= qstori(ICOMP,ISF,1)-qstori(ICOMP,ISF,2)
              avqcri=  (qcri(ICOMP,ISF,1)  +qcri(ICOMP,ISF,2))*0.5
              avqpltri=(qpltri(ICOMP,ISF,1)+qpltri(ICOMP,ISF,2))*0.5
              avqstore= qstore(ICOMP,ISF,1)-qstore(ICOMP,ISF,2)
              avqconde=(qconde(ICOMP,ISF,1)+qconde(ICOMP,ISF,2))*0.5
              avqconve=(qconve(ICOMP,ISF,1)+qconve(ICOMP,ISF,2))*0.5
              avqlwrde=(qlwrde(ICOMP,ISF,1)+qlwrde(ICOMP,ISF,2))*0.5
              avqlwrbd=(qlwrbd(ICOMP,ISF,1)+qlwrbd(ICOMP,ISF,2))*0.5
              avqlwrsk=(qlwrsk(ICOMP,ISF,1)+qlwrsk(ICOMP,ISF,2))*0.5
              avqswrde=(qswrde(ICOMP,ISF,1)+qswrde(ICOMP,ISF,2))*0.5
            endif

C Write out all of the information for ISAVE=4.
C << Note: in some cases it should be possible to write info for two
C << surfaces into one record. 
            if(irdact.eq.0) then
              if(izver.lt.4)then
                WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)TMC(1),
     &              avqswrdi,avqlwrdi,avqconvi,avqcondi,avqstori,
     &              avqstore,avqconde,avqconve,avqlwrde,avqlwrbd,
     &              avqlwrsk,avqswrde
              else

C For izver >=4 also include the explicit radiant casual gains
C for occupants, lights, small power and other so no calculations
C are needed to reconstitute the radiant casual gains.
                WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)TMC(1),
     &              avqswrdi,avqlwrdi,avqconvi,avqcondi,avqstori,
     &              avqstore,avqconde,avqconve,avqlwrde,avqlwrbd,
     &              avqlwrsk,avqswrde,avqcri*fracrocup,
     &              avqcri*fracrlight,avqcri*fracrequip,
     &              avqcri*fracroth
              endif
              IREC=IREC+1
            else
              sTMC(ICOMP,ISF,1)=sTMC(ICOMP,ISF,1)+TMC(1)
              sqswrdi(ICOMP,ISF)=sqswrdi(ICOMP,ISF)+avqswrdi
              sqlwrdi(ICOMP,ISF)=sqlwrdi(ICOMP,ISF)+avqlwrdi
              sqconvi(ICOMP,ISF)=sqconvi(ICOMP,ISF)+avqconvi
              sqcondi(ICOMP,ISF)=sqcondi(ICOMP,ISF)+avqcondi
              sqstori(ICOMP,ISF)=sqstori(ICOMP,ISF)+avqstori
              sqstore(ICOMP,ISF)=sqstore(ICOMP,ISF)+avqstore
              sqconde(ICOMP,ISF)=sqconde(ICOMP,ISF)+avqconde
              sqconve(ICOMP,ISF)=sqconve(ICOMP,ISF)+avqconve
              sqlwrde(ICOMP,ISF)=sqlwrde(ICOMP,ISF)+avqlwrde
              sqlwrbd(ICOMP,ISF)=sqlwrbd(ICOMP,ISF)+avqlwrbd
              sqlwrsk(ICOMP,ISF)=sqlwrsk(ICOMP,ISF)+avqlwrsk
              sqswrde(ICOMP,ISF)=sqswrde(ICOMP,ISF)+avqswrde
              if(irdnow.eq.1) then
                ird=irdcb(ICOMP)
                WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &          sTMC(ICOMP,ISF,1)/ird,sqswrdi(ICOMP,ISF)/ird,
     &          sqlwrdi(ICOMP,ISF)/ird,sqconvi(ICOMP,ISF)/ird,
     &          sqcondi(ICOMP,ISF)/ird,sqstori(ICOMP,ISF)/ird,
     &          sqstore(ICOMP,ISF)/ird,sqconde(ICOMP,ISF)/ird,
     &          sqconve(ICOMP,ISF)/ird,sqlwrde(ICOMP,ISF)/ird,
     &          sqlwrbd(ICOMP,ISF)/ird,sqlwrsk(ICOMP,ISF)/ird,
     &          sqswrde(ICOMP,ISF)/ird
                IREC=IREC+1
                sTMC(ICOMP,ISF,1)=0.
                sqswrdi(ICOMP,ISF)=0.
                sqlwrdi(ICOMP,ISF)=0.
                sqconvi(ICOMP,ISF)=0.
                sqcondi(ICOMP,ISF)=0.
                sqstori(ICOMP,ISF)=0.
                sqstore(ICOMP,ISF)=0.
                sqconde(ICOMP,ISF)=0.
                sqconve(ICOMP,ISF)=0.
                sqlwrde(ICOMP,ISF)=0.
                sqlwrbd(ICOMP,ISF)=0.
                sqlwrsk(ICOMP,ISF)=0.
                sqswrde(ICOMP,ISF)=0.
              endif
            endif

C Trace output.
            IF(ITC.LE.0.OR.NSINC.LT.ITC)goto 700
            IF(ITRACE(31).NE.1.OR.NSINC.GT.ITCF.OR.
     &                         IZNTRC(ICOMP).NE.1)goto 700

C Should surface ISF be traced?
            ipro=0
            if(nsurf.gt.0)then
              do 701 j=1,nsurf
                jj=insurf(j)
                if(jj.eq.ISF)ipro=1
  701         continue
            endif

C Only print the following zone data once.
            IF(IZFLG.EQ.0)THEN
              write(outs,'(A,I4,A,A)')' Subroutine MZLS3 Trace output',
     &          ICNT,' Zone ',zname(ICOMP)
              call edisp(itu,outs)
              CALL DAYCLK(IDYP,BTIMEF,ITU)
              ICNT=ICNT+1

              write(outs,8002)QPA(ICOMP),QFA(ICOMP),QPS(ICOMP),
     &                        QFS(ICOMP),QPC(ICOMP),QFC(ICOMP),QM
 8002         format(' QPA=',F7.1,' QFA=',F7.1,' QPS=',F7.1,' QFS=',
     &                     F7.1,' QPC=',F7.1,' QFC=',F7.1,' QM=',F7.1)
              call edisp(itu,outs)

C Need to account for the storage of the air mass itself.
              qair=real(ntstep)*0.33*VOL(ICOMP)*(TPA(ICOMP)-TFA(ICOMP))

C Get sum at the air node. d1 to d6 are the present and future
C portions of each of the saved variables. qma from QM calc above.
C              tot=QV1+QV2+QW1+QW2+QSA+QCASC+outopq+opqin+
C     &                                           outtrn+trnin+qma+qair
C QW1 and QW2 and QSA are used for solar trace only.
              tot=QV1+QV2+QCASC+outopq+opqin+outtrn+trnin+qma+qair
              d1=CVIP*(TP-TPA(ICOMP))
              d2=CVIF*(TF-TFA(ICOMP))
              d3=CVVPM*Y-CVVP*TPA(ICOMP)
              d4=CVVFM*X-CVVF*TFA(ICOMP)
              d5=totheatloss(icomp)*(TP-TPA(icomp))
              d6=totheatloss(icomp)*(TF-TFA(icomp))
              call edisp(itu,'  ')
              call edisp(itu,
     &            ' Energy at the air node:   saved    pres    futr')
              write(outs,'(A,3F9.2)')' Infiltration air load  ',
     &                                                      QV1,d1,d2
              call edisp(itu,outs)
              write(outs,'(A,3F9.2)')' Ventilation air load   ',
     &                                                      QV2,d3,d4
              call edisp(itu,outs)
              write(outs,'(A,3F9.2)')' Thermal bridge air load   ',
     &                                                  qbridge,d5,d6
              call edisp(itu,outs)

C Solar distribution info.
              write(outs,'(A,3F9.2)')
     &          ' Solar from outside & adj zones and solar absorbed:',
     &          QW1,QW2,QSA
              call edisp(itu,outs)

              if(ICGCTL(ICOMP).gt.0)then
                write(outs,'(A,3F6.3,A,i2)')' Control frac p/f/avg   ',
     &            CGCTL(ICOMP,1,ICGCTL(ICOMP)),
     &            CGCTL(ICOMP,2,ICGCTL(ICOMP)),FRAC,
     &            ' for type ',ICGCTL(ICOMP)
              else
                write(outs,'(A,F6.3,A,i2)')' Control fraction avg   ',
     &            FRAC,' no casual gain control.'
              endif
              call edisp(itu,outs)

              write(outs,'(A,F9.2,A,3F9.2,3i3)') 
     &          ' Total casual convec',QCASC,
     &          ' future ocup/light/equip ',
     &          perocupc,perlightc,perequipc,iperocup,
     &          iperlight,iperequip
              call edisp(itu,outs)
              if(ICGCTL(ICOMP).gt.0)then
                write(outs,'(A,3F9.2)') 
     &          ' Casual future  ocup/light/equip convec ctl applied',
     &          ctlperocupc(icomp,2),ctlperlightc(icomp,2),
     &          ctlperequipc(icomp,2)
                call edisp(itu,outs)
                write(outs,'(A,3F9.2)') 
     &          ' Casual average ocup/light/equip convec ctl applied',
     &          avgctlperocupc,avgctlperlightc,avgctlperequipc
                call edisp(itu,outs)
              else
                write(outs,'(A,3F9.2)') 
     &          ' Casual average ocup/light/equip convec ',
     &          avgctlperocupc,avgctlperlightc,avgctlperequipc
                call edisp(itu,outs)
              endif

              write(outs,'(A,F9.2,A,3F9.2)') ' Total casual radiant',
     &          QCASR,' future ocup/light/equip ',perocupr,perlightr,
     &          perequipr
              call edisp(itu,outs)
              if(ICGCTL(ICOMP).gt.0)then
                write(outs,'(A,3F9.2)') 
     &          ' Casual future  ocup/light/equip radiant ctl applied',
     &          ctlperocupr(icomp,2),ctlperlightr(icomp,2),
     &          ctlperequipr(icomp,2)
                call edisp(itu,outs)
                write(outs,'(A,3F9.2)') 
     &          ' Casual average ocup/light/equip radiant ctl applied',
     &          avgctlperocupr,avgctlperlightr,avgctlperequipr
                call edisp(itu,outs)
              else
                write(outs,'(A,3F9.2)') 
     &          ' Casual average ocup/light/equip radiant ',
     &          avgctlperocupr,avgctlperlightr,avgctlperequipr
                call edisp(itu,outs)
              endif

              write(outs,'(A,3F9.2)')
     &          ' Casual future ocup/light/equip latent',
     &          perocupl,perlightl,perequipl
              call edisp(itu,outs)
              if(ICGCTL(ICOMP).gt.0)then
                write(outs,'(A,3F9.2)') 
     &          ' Casual future  ocup/light/equip latent ctl applied',
     &          ctlperocupl(icomp,2),ctlperlightl(icomp,2),
     &          ctlperequipl(icomp,2)
                call edisp(itu,outs)
                write(outs,'(A,3F9.2)') 
     &          ' Casual average ocup/light/equip latent ctl applied',
     &          avgctlperocupl,avgctlperlightl,avgctlperequipl
                call edisp(itu,outs)
              else
                write(outs,'(A,3F9.2)') 
     &          ' Casual average ocup/light/equip latent ',
     &          avgctlperocupl,avgctlperlightl,avgctlperequipl
                call edisp(itu,outs)
              endif
              call edisp(itu,'  ')

              write(outs,'(A,2F9.2)')' Opaque MLC convec: ext int ',
     &          outopq,opqin
              call edisp(itu,outs)
              write(outs,'(A,2F9.2)')' Transp MLC convec: ext int ',
     &          outtrn,trnin
              call edisp(itu,outs)
              write(outs,'(A,F9.2)') ' Convec portion of plant',qma
              call edisp(itu,outs)
              write(outs,'(A,F9.2)') ' Storage @ air point    ',qair
              call edisp(itu,outs)
              write(outs,'(A,F9.2)') ' Totals (error term)    ',tot
              call edisp(itu,outs)
              call edisp(itu,'  ')
              write(outs,'(A,3F7.3)')' zone air temp. TPA, TFA, TMA: ',
     &                                      TPA(ICOMP),TFA(ICOMP),TMA
              call edisp(itu,outs)
              write(outs,'(A,2F6.2)')' outside air tp, tf:   ',tp,tf
              call edisp(itu,outs)
              write(outs,'(A,F6.2)') ' Zone RH = ',ZRH
              call edisp(itu,outs)
              call edisp(itu,' Surface temperatures')

C List first 22 surfaces.
              nss=min0(22,nsur)
              write(outs,'(A,22I6)')' Surf no. >',(I2,I2=1,NSS)
              call edisp(itu,outs)
              write(outs,'(A,22F6.2)')
     &                   ' int pres: ',(TPS(ICOMP,I2),I2=1,NSS)
              call edisp(itu,outs)
              write(outs,'(A,22F6.2)')
     &                   ' int futr: ',(TFS(ICOMP,I2),I2=1,NSS)
              call edisp(itu,outs)
              write(outs,'(A,22F6.2)')' int save: ',(TMS(I2),I2=1,NSS)
              call edisp(itu,outs)
              write(outs,'(A,22F6.2)')
     &                 ' ext pres: ',(TPC(ICOMP,I2,1),I2=1,NSS)
              call edisp(itu,outs)
              write(outs,'(A,22F6.2)')
     &                 ' ext futr: ',(TFC(ICOMP,I2,1),I2=1,NSS)
              call edisp(itu,outs)
              IZFLG=1
            ENDIF

C Surface specific trace.
            if(ipro.eq.0) goto 700
            lel=nelts(icomp,ISF)
            nnx=nndc(icomp,ISF)-1
            call zsid(icomp,isf,zsdes,zsdesc,zsdess)
            write(outs,8010)zsdes(1:lnblnk(zsdes)),sna(icomp,ISF)
 8010       format(' Internal flux balance for MLC surface: ',a,
     &                                      ' with area=',F6.3,' m^2')
            call edisp(itu,'  ')
            call edisp(itu,outs)
            write(outs,80101)THRMLI(ICOMP,ISF,lel,2),
     &                THRMLI(ICOMP,ISF,lel,1),THRMLI(ICOMP,ISF,lel,4),
     &                THRMLI(ICOMP,ISF,lel,3),
     &                tpc(icomp,ISF,nnx),tfc(icomp,ISF,nnx)
80101       format(' den=',F8.2,' con=',F8.3,' thk=',F6.3,
     &                  ' sht=',F8.2,' tpc(n)=',F6.2,' tfc(n)=',F6.2)
            call edisp(itu,outs)
            call edisp(itu,'  ')
            write(outs,8013)avqconvi,hcip(icomp,ISF),hcif(icomp,ISF)
 8013       format(' convective   ',F10.3,' W composed of:',
     &                                         '   hcip,hcif:',2F8.3)
            call edisp(itu,outs)
            write(outs,'(A,F10.3,A)')
     &                      ' longwave     ',avqlwrdi,' W composed of:'
            call edisp(itu,outs)

C List first 22 surfaces.
            nss=min0(22,nsur)
            write(outs,'(A,22F6.3)') '  hrp : ',(hrp(j1,ISF),j1=1,NSS)
            call edisp(itu,outs)
            write(outs,'(A,22F6.3)') '  hrf : ',(hrf(j1,ISF),j1=1,NSS)
            call edisp(itu,outs)

C Compute error term.
            XERR=avqcondi+avqcri+avqpltri+avqswrdi+avqstori+avqlwrdi+
     &           avqconvi
            write(outs,'(A,F10.3,A)')' conductive   ',avqcondi,' W'
            call edisp(itu,outs)
            write(outs,'(A,F10.3,A,3F10.3)')' casual lw    ',avqcri,
     &        ' W & ocup/light/equipW ',avqcri*fracrocup,
     &        avqcri*fracrlight,avqcri*fracrequip
            call edisp(itu,outs)
            write(outs,'(A,F10.3,A)')' radiant plant',avqpltri,' W'
            call edisp(itu,outs)
            write(outs,'(A,F10.3,A)')' shortwave    ',avqswrdi,' W'
            call edisp(itu,outs)
            write(outs,'(A,F10.3,A)')' heat storage ',avqstori,' W'
            call edisp(itu,outs)
            write(outs,'(A,F10.5,A)')' error term   ',XERR,' W'
            call edisp(itu,outs)
            call edisp(itu,'  ')

C Exterior side of surface, print more information.
            IF(IE(icomp,ISF).EQ.0)THEN
              write(outs,8021)zsdes
 8021         format(' External flux balance for MLC surface: ',a)
              call edisp(itu,outs)
              write(outs,80211)THRMLI(ICOMP,ISF,1,2),
     &         THRMLI(ICOMP,ISF,1,1),THRMLI(ICOMP,ISF,1,4),
     &         THRMLI(ICOMP,ISF,1,3)
80211         format(' with den=',F8.2,' con=',F8.2,' thk=',F6.3,
     &                                                  ' sht=',F8.2)
              call edisp(itu,outs)
              write(outs,80212)tpc(icomp,ISF,1),tfc(icomp,ISF,1),
     &                              tpc(icomp,ISF,2),tfc(icomp,ISF,2)
80212         format(' tpc(1)=',F6.2,' tfc(1)=',F6.2,
     &                               ' tpc(2)=',F6.2,' tfc(2)=',F6.2)
              call edisp(itu,outs)
              call edisp(itu,'  ')

              write(outs,8024)avqconve,hcop(icomp,ISF),hcof(icomp,ISF)
 8024         format(' convective      ',F10.3,' W composed of:',
     &                                         ' hcop, hcof:',2F8.3)
              call edisp(itu,outs)

C Compute error term.
              XERR=avqconde+avqswrde+avqstore+avqlwrde+avqlwrbd+
     &             avqlwrsk+avqconve
              write(outs,'(A,F10.3,A)')' conductive      ',avqconde,' W'
              call edisp(itu,outs)
              write(outs,'(A,F10.3,A)')' longwave > grnd ',avqlwrde,' W'
              call edisp(itu,outs)
              write(outs,'(A,F10.3,A)')' longwave > bldgs',avqlwrbd,' W'
              call edisp(itu,outs)
              write(outs,'(A,F10.3,A)')' longwave > sky  ',avqlwrsk,' W'
              call edisp(itu,outs)
              write(outs,'(A,F10.3,A)')' shortwave       ',avqswrde,' W'
              call edisp(itu,outs)
              write(outs,'(A,F10.3,A)')' heat stored     ',avqstore,' W'
              call edisp(itu,outs)
              write(outs,'(A,F10.5,A)')' error term      ',XERR,' W'
              call edisp(itu,outs)
              call edisp(itu,'  ')
            ENDIF
  700     continue

C The arrays ctlperocupc(?,2) etc. had not been cleared because they
C were used in the trace facility.  To match the other variable types
C the future values are reset to zero at this point.
          ctlperocupc(icomp,2)=0.0
          ctlperocupr(icomp,2)=0.0
          ctlperocupl(icomp,2)=0.0
          ctlperlightc(icomp,2)=0.0
          ctlperlightr(icomp,2)=0.0
          ctlperlightl(icomp,2)=0.0
          ctlperequipc(icomp,2)=0.0
          ctlperequipr(icomp,2)=0.0
          ctlperequipl(icomp,2)=0.0

  300     CONTINUE
        endif
      else
        write(outs,'(A,I3,A)')' Save option',ISAVE,' illegal '
        call edisp(iuout,outs)
        call epwait
        CALL ERPFREE(IUNIT,ISTAT)
        CALL EPAGEND
        STOP
      ENDIF

C Update storage counter if 'once per hour write' active.
      if(irdact.gt.0) then
        if(irdnow.eq.1) then
          irdcb(ICOMP)=1
        else
          irdcb(ICOMP)=irdcb(ICOMP)+1
        endif
      endif

C Remember current record number.
      IRECPL=IREC
      return

C Write error.
 1000 write(outs,'(A,I5,A)')' MZLS3: error at record',IREC,'.'
      call edisp(iuout,outs)
      return

      END

c ******************** MZLS4 ********************

c MZLS4 saves the start address of the next result-set
c in the appropriate record (2-31) in the building results library.

      SUBROUTINE MZLS4
#include "building.h"

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN

      COMMON/LIBREC/IRECPL

      character outs*124

      IUNIT=IFIL+2
      IREC=1

c Read number of result sets currently stored in results
c library.
      READ(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)NSIM
      IF((NSIM+1).EQ.MNRS)goto 1

c Compute record start address of next available 'start
c address' record.
      IREC=NSIM+3

c Save start address of next result-set.
      WRITE(IUNIT,REC=IREC,IOSTAT=ISTAT,ERR=1000)IRECPL
    3 RETURN
    1 call edisp(iuout,' Library now full. ')
      goto 3
 1000 write(outs,'(A,I5,A)')' MZLS4: error at record',IREC,'.'
      call edisp(iuout,outs)
      goto 3
      END

c ******************** MZLS5 ********************
      SUBROUTINE MZLS5(ICOMP)
#include "building.h"

      COMMON/PVALC/TPC(MCOM,MS,MN),QPC(MCOM)
      COMMON/PVALS/TPS(MCOM,MS),QPS(MCOM)
      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/SETUQ/QPLTP(MCOM),QPLTF(MCOM),CONV(MCOM)
      COMMON/CFDCNV/QCFDF(MCOM,MS),QCFDP(MCOM,MS)

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)

      COMMON/GENFLUX/GENFLXF(MCOM,MS,MN),GENFLXP(MCOM,MS,MN)

C Now, for the next time-step, set the present temperature
C and plant injection values equal to the future values.
      QPLTP(ICOMP)=QPLTF(ICOMP)
      QPA(ICOMP)= QFA(ICOMP)
      QPS(ICOMP)=QFS(ICOMP)
      QPC(ICOMP)=QFC(ICOMP)
      TPA(ICOMP)=TFA(ICOMP)
      NSUR=NCONST(ICOMP)
      DO 10 I=1,NSUR
        TPS(ICOMP,I)=TFS(ICOMP,I)

C Set the present CFD surface convection heat fluxes
C to their present values.
        QCFDP(ICOMP,I)=QCFDF(ICOMP,I)

        NN=NNDC(ICOMP,I)-1
      DO 10 J=1,NN
        TPC(ICOMP,I,J)=TFC(ICOMP,I,J)
        GENFLXP(ICOMP,I,J) = GENFLXF(ICOMP,I,J)
   10 CONTINUE
      RETURN
      END

C ******************** SURBAL ********************
C SURBAL calculates surface energy balances for future time row.
C Present time row data is copied from future before new calculation, 
C so that variable thermophysical properties are accounted for (e.g. 
C in blind shutter control, or bcl99).  If variable thermophysical 
C properties are defined as a function of e.g. temperature (i.e. IVTHPZ
C is true) then a separate calculation is made as the conductivity etc 
C is held in a separate set of commons.
C Possibly all calculations should use the IVTHPZ in the future?

      SUBROUTINE SURBAL(ICOMP)

#include "building.h"
C#include "cfd.h"

      COMMON/PREC1/SNA(MCOM,MS),ZOA(MCOM)
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP

      COMMON/PVALC/TPC(MCOM,MS,MN),QPC(MCOM)
      COMMON/PVALS/TPS(MCOM,MS),QPS(MCOM)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/SETUQ/QPLTP(MCOM),QPLTF(MCOM),CONV(MCOM)

      COMMON/COE32/QSOLI(MS,2),QSOLE(MS,2)
      COMMON/COE34/QCASRT(MCOM),QCASCT

      COMMON/CCTL/ICASCF(MCOM)
      COMMON/BCTL/NCF,IBSN(MCF,4),IBAN(MCF,3),NBCDT(MCF),
     &IBCDV(MCF,MBCDT,2),NBCDP(MCF,MBCDT),TBCPS(MCF,MBCDT,MBCDP),
     &IBCTYP(MCF,MBCDT,MBCDP),IBCLAW(MCF,MBCDT,MBCDP),
     &BMISCD(MCF,MBCDT,MBCDP,MISC)

      COMMON/PREC9/NCONST(MCOM),NELTS(MCOM,MS),NGAPS(MCOM,MS),
     &NPGAP(MCOM,MS,MGP)
      COMMON/PREC12/EI(MCOM,MS),EE(MCOM,MS),AI(MCOM,MS),AE(MCOM,MS)
      COMMON/PREC14/emarea(MCOM)
      COMMON/PRECTC/ITMCFL(MCOM,MS),TMCT(MCOM,MTMC,5),
     &       TMCA(MCOM,MTMC,ME,5),TMCREF(MCOM,MTMC),TVTR(MCOM,MTMC)

      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      COMMON/ADJC/IE(MCOM,MS),ATP(MCOM,MS),ATF(MCOM,MS),ARP(MCOM,MS),
     &ARF(MCOM,MS)

      COMMON/VTHP14/THRMLI(MCOM,MS,ME,7)
      COMMON/VTHP21/THCONp(MS,MN),THDNSp(MS,MHCV),THCAPp(MS,MHCV)
      COMMON/VTHP22/THCONf(MS,MN),THDNSf(MS,MHCV),THCAPf(MS,MHCV)
      COMMON/VTHP32/IVTHPS,IVTHPZ(MCOM)

      COMMON/COE32J/QTMCA(MS,MN,2)

C Commons in support of surface energy balance trace feature.
      COMMON/COE33S/QELWPB(MS),QELWFB(MS),QELWPK(MS),QELWFK(MS),
     &QELWPG(MS),QELWFG(MS)
      COMMON/COE31/HRP(MS,MS),ZHRP(MS),HRF(MS,MS),ZHRF(MS)
      common/concoe/hcip(mcom,ms),hcif(mcom,ms),hcop(mcom,ms),
     &              hcof(mcom,ms)

      COMMON/GR1D01/NNDS,NNDZ(MCOM),NNDC(MCOM,MS),NNDL(MCOM,MS,ME)

      COMMON/CFDCNV/QCFDF(MCOM,MS),QCFDP(MCOM,MS)
      
C Energy balance.
      common/SRFBAL/qswrdi(MCOM,MS,2),qlwrdi(MCOM,MS,2),
     &           qconvi(MCOM,MS,2),qcondi(MCOM,MS,2),qstori(MCOM,MS,2),
     &           qcri(MCOM,MS,2),qpltri(MCOM,MS,2),qstore(MCOM,MS,2),
     &           qconde(MCOM,MS,2),qconve(MCOM,MS,2),qlwrde(MCOM,MS,2),
     &           qlwrbd(MCOM,MS,2),qlwrsk(MCOM,MS,2),qswrde(MCOM,MS,2)

      COMMON/BTIME/BTIMEP,BTIMEF

      LOGICAL IVTHPS,IVTHPZ

C Do for all surfaces.
      do 10 ISFN=1,NCONST(ICOMP)
      
C Surface data.
        lel=nelts(ICOMP,ISFN)
        nnx=nndc(ICOMP,ISFN)-1
        NSUR=NCONST(ICOMP)

C Calculate surface balance data to pass to results library.
C Copy future to present and then calculate new future value.

C --------- INSIDE FACE -----------
        DELXI=THRMLI(ICOMP,ISFN,lel,4)/FLOAT(NNDL(ICOMP,ISFN,lel))
        
C -- Conduction.
        if(IVTHPZ(icomp))then
          qcondi(ICOMP,ISFN,1)=THCONp(ISFN,nnx)*sna(icomp,ISFN)*
     &        (tpc(icomp,ISFN,nnx)-tps(icomp,ISFN))/DELXI
          qcondi(ICOMP,ISFN,2)=THCONf(ISFN,nnx)*sna(icomp,ISFN)*
     &        (tfc(icomp,ISFN,nnx)-tfs(icomp,ISFN))/DELXI
        else
          qcondi(ICOMP,ISFN,1)=qcondi(ICOMP,ISFN,2)
          qcondi(ICOMP,ISFN,2)=THRMLI(ICOMP,ISFN,lel,1)*sna(ICOMP,ISFN)*
     &                       (tfc(ICOMP,ISFN,NNX)-tfs(ICOMP,ISFN))/DELXI
        endif

C -- Convection.
        qconvi(ICOMP,ISFN,1)=qconvi(ICOMP,ISFN,2)

C Calculate QCFD? for use by CFD conflation type 2.
        QCFDP(ICOMP,ISFN)=QCFDF(ICOMP,ISFN)
        QCFDF(ICOMP,ISFN)=hcif(ICOMP,ISFN)*(tfa(ICOMP)-tfs(ICOMP,ISFN))
        qconvi(ICOMP,ISFN,2)=QCFDF(ICOMP,ISFN)*sna(ICOMP,ISFN)

C -- Longwave radiation.
        qlwrdi(ICOMP,ISFN,1)=qlwrdi(ICOMP,ISFN,2)
        qlwrdi(ICOMP,ISFN,2)=0.
        do 702 J=1,NSUR
          if (J.ne.ISFN) then
            qlwrdi(ICOMP,ISFN,2)=qlwrdi(ICOMP,ISFN,2)+
     &                        hrf(J,ISFN)*(tfs(ICOMP,J)-tfs(ICOMP,ISFN))
          endif
  702   continue
        qlwrdi(ICOMP,ISFN,2)=qlwrdi(ICOMP,ISFN,2)*sna(ICOMP,ISFN)

C -- Radiant casual gains are area AND emissivity weighted.
        qcri(ICOMP,ISFN,1)=qcri(ICOMP,ISFN,2)
        qcri(ICOMP,ISFN,2)=QCASRT(ICOMP)*0.5*zoa(ICOMP)*sna(ICOMP,ISFN)*
     &                                      ei(ICOMP,ISFN)/emarea(ICOMP)

C -- Plant radiant energy.
        qpltri(ICOMP,ISFN,1)=qpltri(ICOMP,ISFN,2)
        IC=ICASCF(ICOMP)
        IF(IC.NE.0)THEN
          IF(IBAN(IC,1).EQ.ICOMP.AND.IBAN(IC,2).EQ.ISFN.AND.
     &                               IBAN(IC,3).EQ.0)then
            qpltri(ICOMP,ISFN,2)=QFS(ICOMP)*sna(ICOMP,ISFN)
          ELSEIF(IBAN(IC,1).EQ.-2.AND.IBAN(IC,2).EQ.ICOMP)then
            qpltri(ICOMP,ISFN,2)=QPLTF(ICOMP)*(1.-CONV(ICOMP))*
     &                    sna(ICOMP,ISFN)*(ei(ICOMP,ISFN)/emarea(ICOMP))
          ELSE
            qpltri(ICOMP,ISFN,2)=0.0
          ENDIF
        ELSE
          qpltri(ICOMP,ISFN,2)=0.0
        ENDIF

C -- Shortwave. Test if TMC (include solar abs).
        qswrdi(ICOMP,ISFN,1)=qswrdi(ICOMP,ISFN,2)
        IF(ITMCFL(icomp,ISFN).EQ.0)THEN
          qswrdi(ICOMP,ISFN,2)=qsoli(ISFN,2)*sna(ICOMP,ISFN)
        ELSE
          NN=NNDC(ICOMP,ISFN)
          qswrdi(ICOMP,ISFN,2)=QTMCA(ISFN,NN,2)*sna(ICOMP,ISFN)
        ENDIF

C -- Energy stored.
        qstori(ICOMP,ISFN,1)=qstori(ICOMP,ISFN,2)
        qstori(ICOMP,ISFN,2)=THRMLI(ICOMP,ISFN,lel,2)*
     &                       THRMLI(ICOMP,ISFN,lel,3)*sna(ICOMP,ISFN)*
     &                     (DELXI/2.)*tfs(ICOMP,ISFN)*real(ntstep)/3600.
        
C --------- OTHER FACE -----------
C Calculate only if facing exterior type boundaty condition.
        IF(IE(icomp,ISFN).EQ.0)THEN
          DELXE=THRMLI(ICOMP,ISFN,1,4)/FLOAT(NNDL(ICOMP,ISFN,1))
          
C -- Conduction.
          if(IVTHPZ(icomp))then
            qconde(ICOMP,ISFN,1)=THCONp(ISFN,1)*sna(ICOMP,ISFN)*
     &                       (tpc(icomp,ISFN,2)-tpc(ICOMP,ISFN,1))/DELXE
            qconde(ICOMP,ISFN,2)=THCONf(ISFN,1)*sna(ICOMP,ISFN)*
     &                       (tfc(ICOMP,ISFN,2)-tfc(ICOMP,ISFN,1))/DELXE
          else
            qconde(ICOMP,ISFN,1)=qconde(ICOMP,ISFN,2)
            qconde(ICOMP,ISFN,2)=THRMLI(ICOMP,ISFN,1,1)*sna(ICOMP,ISFN)*
     &                       (tfc(ICOMP,ISFN,2)-tfc(ICOMP,ISFN,1))/DELXE
          endif

C -- Convection.
          qconve(ICOMP,ISFN,1)=qconve(ICOMP,ISFN,2)
          qconve(ICOMP,ISFN,2)=hcof(ICOMP,ISFN)*sna(ICOMP,ISFN)*
     &                                            (tf-tfc(ICOMP,ISFN,1))

C -- Longwave radiation (ground, buildings, sky).
          qlwrde(ICOMP,ISFN,1)=qlwrde(ICOMP,ISFN,2)
          qlwrde(ICOMP,ISFN,2)=QELWFG(ISFN)*sna(ICOMP,ISFN)
          qlwrbd(ICOMP,ISFN,1)=qlwrbd(ICOMP,ISFN,2)
          qlwrbd(ICOMP,ISFN,2)=QELWFB(ISFN)*sna(ICOMP,ISFN)
          qlwrsk(ICOMP,ISFN,1)=qlwrsk(ICOMP,ISFN,2)
          qlwrsk(ICOMP,ISFN,2)=QELWFK(ISFN)*sna(ICOMP,ISFN)

C -- Shortwave radiation. Test if TMC.
          qswrde(ICOMP,ISFN,1)=qswrde(ICOMP,ISFN,2)
          if (ITMCFL(icomp,ISFN).EQ.0) then
            qswrde(ICOMP,ISFN,2)=qsole(ISFN,2)*sna(ICOMP,ISFN)
          else
            qswrde(ICOMP,ISFN,2)=QTMCA(ISFN,1,2)*sna(ICOMP,ISFN)
          endif

C -- Energy stored.
          qstore(ICOMP,ISFN,1)=qstore(ICOMP,ISFN,2)
          qstore(ICOMP,ISFN,2)=THRMLI(ICOMP,ISFN,1,2)*
     &                         THRMLI(ICOMP,ISFN,1,3)*sna(ICOMP,ISFN)*
     &                   (DELXE/2.)*tfc(ICOMP,ISFN,1)*real(ntstep)/3600.
        ELSE

C Inside surface so pass "0" for each of the variables.
          qconde(ICOMP,ISFN,1)=0.
          qconde(ICOMP,ISFN,2)=0.
          qconve(ICOMP,ISFN,1)=0.
          qconve(ICOMP,ISFN,2)=0.
          qlwrde(ICOMP,ISFN,1)=0.
          qlwrde(ICOMP,ISFN,2)=0.
          qswrde(ICOMP,ISFN,1)=0.
          qswrde(ICOMP,ISFN,2)=0.
          qstore(ICOMP,ISFN,1)=0.
          qstore(ICOMP,ISFN,2)=0.
          qlwrbd(ICOMP,ISFN,1)=0.
          qlwrbd(ICOMP,ISFN,2)=0.
          qlwrsk(ICOMP,ISFN,1)=0.
          qlwrsk(ICOMP,ISFN,2)=0.
        ENDIF
 10   continue
 
      RETURN
      END
