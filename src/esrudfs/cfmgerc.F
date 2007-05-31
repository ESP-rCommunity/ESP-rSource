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

C Contains:
C  CFMNGE - Invoke CFD routine and print output files. (version for cfd module)


C ********************* CFMNGE *********************
C CFMNGE - Invoke CFD routine and print output files. (version for cfd module)
      SUBROUTINE CFMNGE(ICOMP,IND)
      INCLUDE 'cfd.h'

      common/outin/iuout,iuin
      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      COMMON/ICFCHN/ICFMON,ICFTMP,ICFLIB
      common/utlcom/photon,matlab,textout,flwvis,xslice,yslice,zslice
      common/slicom/islice,jslice,kslice

      COMMON/SIMT/TSTEP,NSINC

      COMMON/ICFNOD/ICFD,ICP
      common/cfdini/INITFL(MNZ)
      common/param1/MAXITR(MNZ),IMONT(MNZ),JMONT(MNZ),KMONT(MNZ),
     1             IPPHI(MNZ),SRMAX(MNZ)
      common/param2/TITLE(MNZ),CFTRFL(MNZ),LPHI(MNZ)
      COMMON/Sbdary/NSB(MNZ),ISBi(MNSBZ,MNZ),ISBf(MNSBZ,MNZ),
     &              JSBi(MNSBZ,MNZ),JSBf(MNSBZ,MNZ),
     &              KSBi(MNSBZ,MNZ),KSBf(MNSBZ,MNZ),
     &              ISUFLC(MNSBZ,MNZ),IWSB(MNSBZ,MNZ),SSB(MNSBZ,MNZ),
     &              SSBHC(MNSBZ,MNZ),
     &              ITCtype(MNSBZ,MNZ),icTREF(MNSBZ,MNZ)

C Flow visualization common blocks.
      COMMON/VISFIL/LVEC,LBIT

      DIMENSION HC(MNSBZ),SFNAM(MNSBZ)

      CHARACTER NBCHAR*10,SFNAM*12
      CHARACTER*72 TITLE,outfil,TITL,WORD,CFTRFL,LPHI
      CHARACTER*72 LVEC,LBIT,vecfil

      LOGICAL INITFL,CONVER
      logical photon,matlab,textout,flwvis,xslice,yslice,zslice

      ICFD=IND
      ICP=ICOMP
      IMFACT=0
      IBLACT=0

      IF(NSINC.EQ.1)then

C Initialisation at the first time-step.
C Initialize some constant values.
        CALL INICNT
        IF(INITFL(ICFD))then

C Re-initialise field values of variables if required.
          ifl=ICFTMP
          ier=0
          call INTSTR(ICP,nbchar,ISWD,IER)
          outfil='tmp_DFS'
          CALL FPOPEN(IFL,IER,5,1,outfil)
          REWIND(ifl)
          CALL REAOLD(ifl)
        ELSE

C Initialise field values of variables.
          CALL INIT
        ENDIF
      ELSE

C Read present values of the variables and some grid parameters.
        ifl=ICFTMP
        ier=0
        call INTSTR(ICP,nbchar,ISWD,IER)
        outfil='tmp_DFS'
        ier=0
        CALL FPOPEN(IFL,IER,5,1,outfil)
        REWIND(ifl)
        CALL REAOLD(ifl)
      ENDIF
      IPRPHI=IPPHI(ICFD)

C Calculate geometrical quantities.
      CALL GRID(IER)

C Set up boundary conditions which are independent of the airflow.
      CALL INDBND
      CALL INOUT

C Invoke CFD simulation.
      ifl=ICFMON
      IF(NSINC.EQ.1)call efopseq(ifl,CFTRFL(ICFD),3,ier)
      CALL CFCALC(NSINC,TSTEP,IMFACT,IBLACT,CONVER)

C Convection coefficients.
      NTSURF=0
      DO 120 L=1,NSB(ICFD)
        IS=ISUFLC(L,ICFD)
        CALL INTSTR(IS,nbchar,ISWD,IER)
        loc=ABS(IWSB(L,ICFD))
        IF(loc.EQ.1)THEN
          write(SFNAM(IS),'(a,a)')'Wwall_',nbchar(1:ISWD)
        ELSEIF(loc.EQ.2)THEN
          write(SFNAM(IS),'(a,a)')'Ewall_',nbchar(1:ISWD)
        ELSEIF(loc.EQ.3)THEN
          write(SFNAM(IS),'(a,a)')'Swall_',nbchar(1:ISWD)
        ELSEIF(loc.EQ.4)THEN
          write(SFNAM(IS),'(a,a)')'Nwall_',nbchar(1:ISWD)
        ELSEIF(loc.EQ.5)THEN
          write(SFNAM(IS),'(a,a)')'Lwall_',nbchar(1:ISWD)
        ELSEIF(loc.EQ.6)THEN
          write(SFNAM(IS),'(a,a)')'Hwall_',nbchar(1:ISWD)
        ENDIF
        NTSURF=NTSURF+1
 120  CONTINUE

C Determine solid boundary surface areas.
      CALL CFAREA(NTSURF)

C Determine convection coefficients. 
      TAIR=20.0
      CALL CFCONV(TAIR,HC,SFNAM,NTSURF)

C Write future field values of variables to be used in the next time-step.
      ifl=icftmp
      call INTSTR(ICP,nbchar,ISWD,IER)
      outfil='tmp_DFS'
      CALL FPOPEN(IFL,IER,6,3,outfil)
      REWIND(ifl)
      CALL PRNNEW(ifl)
    
C Write results files (flow visualization + matlab, or photon, or text).
      ISPER=NSINC
      IF((INT(ISPER/IPRPHI)*IPRPHI).NE.ISPER)GOTO 99
      ifl=ICFLIB
      call INTSTR(NSINC,nbchar,ISWD,IER)
      K=0
      CALL EGETW(LPHI(ICFD),K,WORD,'W',' ',IER)
      call efopseq(ifl,WORD,3,ier)
      WORD=TITLE(ICFD)
      write(TITL,'(a,a,a)')WORD(1:25),' ITIMST = ',nbchar(1:ISWD)

      if (photon) CALL PRNPHI(TITL,ifl)
      if (matlab) CALL PRNMAT(ifl)
      if (textout) CALL PRNTXT(ifl)
      CALL ERPFREE(IFL,ISTAT)
      vecfil=LVEC
      if (flwvis) CALL GENVEC(vecfil)

   99 RETURN
      END
