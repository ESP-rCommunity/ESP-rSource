C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001-.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 orlater).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C This file contains the following subroutines.
C CHECK - Checks if CFD convergence has been reached.


C ********************* CHECK *********************
C Checks if CFD convergence has been reached.

      SUBROUTINE CHECK(MAXIT,CONVER)
#include "building.h"
#include "cfd.h"
#include "help.h"

      integer MAXIT
      logical CONVER

      common/outin/iuout,iuin,ieout
C      COMMON/FDTRFC/FLSDTU(MNZ),FLSDTV(MNZ),FLSDTW(MNZ),FLSDTP(MNZ),
C     &              FLSDTT(MNZ),FLSDTK(MNZ),FLSDTE(MNZ)
      COMMON/LINRFC/URFCU(MNZ),URFCV(MNZ),URFCW(MNZ),URFCP(MNZ),
     &              URFCT(MNZ),URFCK(MNZ),URFCE(MNZ),URFCVS(MNZ),
     &              URFCC(MNZ,MCTM)
      common/EQTION/CALLU(MNZ),CALLV(MNZ),CALLW(MNZ),CALLT(MNZ),
     &             CALLC(MNZ),KEMDL(MNZ),BUOY(MNZ),BOUSSI(MNZ),
     &             ZEROT(MNZ),ZandKE(MNZ),MITzero(MNZ)
      COMMON/PCOR/RESORM,NSWPP,URFP,FSDTP,IPREF,JPREF,KPREF
      COMMON/UVEL/RESORU,NSWPU,URFU,FSDTU,DXEPU(ntcelx),
     1            DXPWU(ntcelx),SEWU(ntcelx)
      COMMON/VVEL/RESORV,NSWPV,URFV,FSDTV,DYNPV(ntcely),
     1            DYPSV(ntcely),SNSV(ntcely)
      COMMON/WVEL/RESORW,NSWPW,URFW,FSDTW,DZHPW(ntcelz),
     1            DZPLW(ntcelz),SHLW(ntcelz)
      COMMON/FLUPRf/URFVIS,VISCOS,PRANDT,SH,
     1            DENf(ntcelx,ntcely,ntcelz),VIS(ntcelx,ntcely,ntcelz),
     2            BETA(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/TEN/RESORK,NSWPK,URFK,FSDTK
      COMMON/TDIS/RESORE,NSWPD,URFE,FSDTE
C      common/param1/MAXITR(MNZ),IMONT(MNZ),JMONT(MNZ),KMONT(MNZ),
C     1             IPPHI(MNZ),SRMAX(MNZ)
      COMMON/ICFNOD/ICFD,ICP
C      COMMON/LINRFC1/URFCD(MNZ)
      COMMON/URFDEN/URFDEN
C      COMMON/NSSWP/NSSWPU(MNZ),NSSWPV(MNZ),NSSWPW(MNZ),NSSWPP(MNZ),
C     &             NSSWPT(MNZ),NSSWPK(MNZ),NSSWPE(MNZ)
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/ACCrec/IACC(MNZ)
C      common/ADAPITER/ITERADD
      
      CHARACTER word*124,outs*124,LCFD*72

      LOGICAL OK,CALLU,CALLV,CALLW,CALLT,KEMDL,BUOY,CALLC
      LOGICAL ZEROT,ZandKE,MITzero,BOUSSI

      helpinsub='chkconvg'  ! set for subroutine

C Solution not converged.  If in adaptive conflation mode increase number 
C of iterations by 50% (unless this is less than 10% of original number of 
C iterations) << this behaviour disabled >>.  Write current residuals to ACC file.
      IF( abs(IFCFD(ICP)).eq.4 .or. abs(IFCFD(ICP)).eq.5)THEN
        write(IACC(icfd),*)
        WRITE(IACC(icfd),'(a,I6,a)')
     &    'Convergence has not been achieved after ',
     &    MAXIT,' iterations.'
        write(IACC(icfd),*)' Current residual values:'
        WRITE(IACC(icfd),'(a,7E10.3)') '  Mass = ',RESORM
        IF(CALLU(ICFD))THEN
          WRITE(IACC(icfd),'(a,7E10.3)') '  X momentum = ',RESORU
        ENDIF
        IF(CALLV(ICFD))THEN
          WRITE(IACC(icfd),'(a,7E10.3)') '  Y momentum = ',RESORV
        ENDIF
        IF(CALLW(ICFD))THEN
          WRITE(IACC(icfd),'(a,7E10.3)') '  Z momentum = ',RESORW
        ENDIF
        IF(CALLT(ICFD))THEN
          WRITE(IACC(icfd),'(a,7E10.3)') '  Energy = ',RESORT
        ENDIF
        IF(KEMDL(ICFD))THEN
          WRITE(IACC(icfd),'(a,7E10.3)') '  Turbulent energy = ',
     &                                                     RESORK
          WRITE(IACC(icfd),'(a,7E10.3)') '  Energy dissipation = ',
     &                                                       RESORE
        endif

C << Following code commented out to conform to user set max. iterations >>
c        if (MAXITR(ICFD).eq.MAXIT) then
c          ITERADD=MAXIT/2
c          MAXIT=MAXIT+ITERADD
c          write (IACC(icfd),*) 'New maximum number of iterations ',
c     &                                                        MAXIT
c        elseif ((MAXITR(ICFD)/10).lt.(ITERADD/2)) then
c          ITERADD=ITERADD/2
c          MAXIT=MAXIT+ITERADD
c          write (IACC(icfd),*) 'New maximum number of iterations ',
c     &                                                        MAXIT
c        else
          write (IACC(icfd),*) ' ** Accepting unconverged solution **'
          CONVER=.TRUE.
c        endif
        return
      endif
      
      CALL EDISP(IUOUT,' ')
      WRITE(word,'(a,I6,a)')'Convergence has not been achieved after ',
     &                       MAXIT,' iterations.'
      CALL EDISP(IUOUT,word)
      CALL EDISP(IUOUT,' Current residual values:')
      WRITE(word,'(a,7E10.3)') '  Mass = ',RESORM
      CALL EDISP(IUOUT,word)
      IF(CALLU(ICFD))THEN
        WRITE(word,'(a,7E10.3)') '  X momentum = ',RESORU
        CALL EDISP(IUOUT,word)
      ENDIF
      IF(CALLV(ICFD))THEN
        WRITE(word,'(a,7E10.3)') '  Y momentum = ',RESORV
        CALL EDISP(IUOUT,word)
      ENDIF
      IF(CALLW(ICFD))THEN
        WRITE(word,'(a,7E10.3)') '  Z momentum = ',RESORW
        CALL EDISP(IUOUT,word)
      ENDIF
      IF(CALLT(ICFD))THEN
        WRITE(word,'(a,7E10.3)') '  Energy = ',RESORT
        CALL EDISP(IUOUT,word)
      ENDIF
      IF(KEMDL(ICFD))THEN
        WRITE(word,'(a,7E10.3)') '  Turbulent energy = ',RESORK
        CALL EDISP(IUOUT,word)
        WRITE(word,'(a,7E10.3)') '  Energy dissipation = ',RESORE
        CALL EDISP(IUOUT,word)
      ENDIF

      helptopic='dfs_convergence_accept'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKOK(' ','Accept as a converged solution?',
     &            OK,nbhelp)
      IF(OK)THEN
        CONVER=.TRUE.
        return
      ENDIF

      CALL EASKOK(' ',
     &           'Increase maximum number of iterations?',OK,nbhelp)
      IF(OK)THEN
        IVAL=MAXIT
        CALL EASKI(IVAL,' ','New maximum number of iterations (-) ?',
     &    MAXIT,'F',10000,'F',MAXIT,'Max number of iterations',
     &    IER,nbhelp)
        MAXIT=IVAL
        helptopic='under_relaxation_factor'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK(' ','Change under-relaxation factors?',OK,nbhelp)
        IF(OK)THEN
          CALL EDISP(IUOUT,' ')
          CALL EDISP(IUOUT,' Current linear under-relaxation factors:')

          WRITE(word,'(a,f6.4)') ' Pressure = ',URFP
          CALL EDISP(IUOUT,word)
          IF(CALLU(ICFD))THEN
            WRITE(word,'(a,f6.4)') ' X momentum = ',URFU
            CALL EDISP(IUOUT,word)
          ENDIF
          IF(CALLV(ICFD))THEN
            WRITE(word,'(a,f6.4)') ' Y momentum = ',URFV
            CALL EDISP(IUOUT,word)
          ENDIF
          IF(CALLW(ICFD))THEN
            WRITE(word,'(a,f6.4)') ' Z momentum = ',URFW
            CALL EDISP(IUOUT,word)
          ENDIF
          IF(CALLT(ICFD))THEN
            WRITE(word,'(a,f6.4)') ' Energy = ',URFT
            CALL EDISP(IUOUT,word)
          ENDIF
          IF(KEMDL(ICFD))THEN
            WRITE(word,'(a,f6.4)') ' Turb. energy = ',URFK
            CALL EDISP(IUOUT,word)
            WRITE(word,'(a,f6.4)') ' Energy dissip. = ',URFE
            CALL EDISP(IUOUT,word)
            WRITE(word,'(a,f6.4)') ' Viscosity = ',URFVIS
            CALL EDISP(IUOUT,word)
          ENDIF
          IF(CALLT(ICFD)) THEN
            WRITE(word,'(a,f6.4)') ' Density = ',URFDEN
            CALL EDISP(IUOUT,word)
          ENDIF

          VAL=URFP
          CALL EASKR(VAL,' ','Pressure under-relaxation factor (-)?',
     &      0.0001,'F',1.0,'F',1.0,'linear under-relaxation',IER,nbhelp)
          URFP=VAL
C          URFCP(ICFD)=VAL
          IF(CALLU(ICFD))THEN
            VAL=URFU
            CALL EASKR(VAL,' ',
     &        'U (X momentum) under-relaxation factor (-) ?',
     &        0.0001,'F',1.0,'F',1.0,'linear under-relaxation',
     &        IER,nbhelp)
            URFU=VAL
C            URFCU(ICFD)=VAL
          ENDIF
          IF(CALLV(ICFD))THEN
            VAL=URFV
            CALL EASKR(VAL,' ',
     &        'V (Y momentum) under-relaxation factor (-)?',
     &        0.0001,'F',1.0,'F',1.0,'linear under-relaxation',
     &        IER,nbhelp)
            URFV=VAL
C            URFCV(ICFD)=VAL
          ENDIF
          IF(CALLW(ICFD))THEN
            VAL=URFW
            CALL EASKR(VAL,' ',
     &        'W (Z momentum) under-relaxation factor (-) ?',
     &        0.0001,'F',1.0,'F',1.0,'linear under-relaxation',
     &        IER,nbhelp)
            URFW=VAL
C            URFCW(ICFD)=VAL
          ENDIF
          IF(CALLT(ICFD))THEN
            VAL=URFT
            CALL EASKR(VAL,' ',
     &        'T (Energy) under-relaxation factor (-) ?',
     &        0.0001,'F',1.0,'F',1.0,'linear under-relaxation',
     &        IER,nbhelp)
            URFT=VAL
C            URFCT(ICFD)=VAL
          ENDIF
          IF(KEMDL(ICFD))THEN
            VAL=URFK
            CALL EASKR(VAL,' ',
     &        'K (Turb. energy) under-relaxation factor (-) ?',
     &        0.0001,'F',1.0,'F',1.0,'linear under-relaxation',
     &        IER,nbhelp)
            URFK=VAL
C            URFCK(ICFD)=VAL
            VAL=URFE
            CALL EASKR(VAL,' ',
     &        'Epsilon (Energy dissip.) under-relaxation factor (-)?',
     &        0.0001,'F',1.0,'F',1.0,'linear under-relaxation',
     &        IER,nbhelp)
            URFE=VAL
C            URFCE(ICFD)=VAL
            VAL=URFVIS
          CALL EASKR(VAL,' ','Viscosity under-relaxation factor (-)?',
     &       0.0001,'F',1.0,'F',1.0,'linear under-relaxation',
     &       IER,nbhelp)
            URFVIS=VAL
            URFCVS(ICFD)=VAL
          ENDIF
          IF(CALLT(ICFD))THEN
            VAL=URFDEN
            CALL EASKR(VAL,' ','Density under-relaxation factor (-)?',
     &        0.0001,'F',1.0,'F',1.0,'linear under-relaxation',
     &        IER,nbhelp)
            URFDEN=VAL
C            URFCD(ICFD)=VAL
          ENDIF
        ENDIF

        CALL EASKOK(' ','Change time-step relaxation factors?',
     &           OK,nbhelp)
        IF(OK)THEN
          CALL EDISP(IUOUT,' ')
          write (outs,'(a,a)') 'Current fictitious time-step ',
     &                         'under-relaxation factors:'
          CALL EDISP(IUOUT,outs)
          IF(CALLU(ICFD))THEN
            WRITE(word,'(a,E8.2)') ' FSDTU = ',FSDTU
            CALL EDISP(IUOUT,word)
          ENDIF
          IF(CALLV(ICFD))THEN
            WRITE(word,'(a,E8.2)') ' FSDTV = ',FSDTV
            CALL EDISP(IUOUT,word)
          ENDIF
          IF(CALLW(ICFD))THEN
            WRITE(word,'(a,E8.2)') ' FSDTW = ',FSDTW
            CALL EDISP(IUOUT,word)
          ENDIF
          IF(CALLT(ICFD))THEN
            WRITE(word,'(a,E8.2)') ' FSDTT = ',FSDTT
            CALL EDISP(IUOUT,word)
          ENDIF
          IF(KEMDL(ICFD))THEN
            WRITE(word,'(a,E8.2)') ' FSDTK = ',FSDTK
            CALL EDISP(IUOUT,word)
            WRITE(word,'(a,E8.2)') ' FSDTE = ',FSDTE
            CALL EDISP(IUOUT,word)
          ENDIF
          IF(CALLU(ICFD))THEN
            VAL=FSDTU
            CALL EASKR(VAL,' ','U under-relaxation factor (-)?',
     &        1.0E-20,'F',1.0E20,'F',1.0E20, 
     &        ' fictitions time-step under-relaxation',IER,nbhelp)
            FSDTU=VAL
C            FLSDTU(ICFD)=VAL
          ENDIF
          IF(CALLV(ICFD))THEN
            VAL=FSDTV
            CALL EASKR(VAL,' ','V under-relaxation factor (-)?',
     &        1.0E-20,'F',1.0E20,'F',1.0E20,
     &        ' fictitions time-step under-relaxation',IER,nbhelp)
            FSDTV=VAL
C            FLSDTV(ICFD)=VAL
          ENDIF
          IF(CALLW(ICFD))THEN
            VAL=FSDTW
            CALL EASKR(VAL,' ','W under-relaxation factor (-)?',
     &        1.0E-20,'F',1.0E20,'F',1.0E20,
     &        ' fictitions time-step under-relaxation',IER,nbhelp)
            FSDTW=VAL
C            FLSDTW(ICFD)=VAL
          ENDIF
          IF(CALLT(ICFD))THEN
            VAL=FSDTT
            CALL EASKR(VAL,' ','T under-relaxation factor (-)?',
     &        1.0E-20,'F',1.0E20,'F',1.0E20,
     &        'fictitions time-step under-relaxation',IER,nbhelp)
            FSDTT=VAL
C            FLSDTT(ICFD)=VAL
          ENDIF
          IF(KEMDL(ICFD))THEN
            VAL=FSDTK
            CALL EASKR(VAL,' ','K under-relaxation factor (-)?',
     &        1.0E-20,'F',1.0E20,'F',1.0E20,
     &        'fictitions time-step under-relaxation',IER,nbhelp)
            FSDTK=VAL
C            FLSDTK(ICFD)=VAL
            VAL=FSDTE
            CALL EASKR(VAL,' ','Epsilon under-relaxation factor (-)?',
     &        1.0E-20,'F',1.0E20,'F',1.0E20,
     &        'fictitions time-step under-relaxation',IER,nbhelp)
            FSDTE=VAL
C            FLSDTE(ICFD)=VAL
          ENDIF
        ENDIF
      ENDIF

      CALL EASKOK(' ','Change the number of sweeps?',OK,nbhelp)
      IF(OK)THEN
        CALL EDISP(IUOUT,' ')
        CALL EDISP(IUOUT,'Current number of sweeps:')
          WRITE(word,'(a,i2)') ' NSWPP = ',NSWPP
          CALL EDISP(IUOUT,word)
        IF(CALLU(ICFD))THEN
          WRITE(word,'(a,i2)') ' NSWPU = ',NSWPU
          CALL EDISP(IUOUT,word)
        ENDIF
        IF(CALLV(ICFD))THEN
          WRITE(word,'(a,i2)') ' NSWPV = ',NSWPV
          CALL EDISP(IUOUT,word)
        ENDIF
        IF(CALLW(ICFD))THEN
          WRITE(word,'(a,i2)') ' NSWPW = ',NSWPW
          CALL EDISP(IUOUT,word)
        ENDIF
        IF(CALLT(ICFD))THEN
          WRITE(word,'(a,i2)') ' NSWPT = ',NSWPT
          CALL EDISP(IUOUT,word)
        ENDIF
        IF(KEMDL(ICFD))THEN
          WRITE(word,'(a,i2)') ' NSWPK = ',NSWPK
          CALL EDISP(IUOUT,word)
          WRITE(word,'(a,i2)') ' NSWPE = ',NSWPD
          CALL EDISP(IUOUT,word)
        ENDIF

        IVAL=NSWPP
        CALL EASKI(IVAL,' ','P number of sweeps (-) ?',
     &    1,'F',10,'F',10,' Number of sweeps',IER,nbhelp)
        NSWPP=IVAL
C        NSSWPP(ICFD)=IVAL

        IF(CALLU(ICFD))THEN
          IVAL=NSWPU
          CALL EASKI(IVAL,' ','U number of sweeps (-) ?',
     &      1,'F',10,'F',10,' Number of sweeps',IER,nbhelp)
          NSWPU=IVAL
C          NSSWPU(ICFD)=IVAL
        ENDIF
        IF(CALLV(ICFD))THEN
          IVAL=NSWPV
          CALL EASKI(IVAL,' ','V number of sweeps (-) ?',
     &      1,'F',10,'F',10,' Number of sweeps',IER,nbhelp)
          NSWPV=IVAL
C          NSSWPV(ICFD)=IVAL
        ENDIF
        IF(CALLW(ICFD))THEN
          IVAL=NSWPW
          CALL EASKI(IVAL,' ','W number of sweeps (-) ?',
     &      1,'F',10,'F',10,' Number of sweeps',IER,nbhelp)
          NSWPW=IVAL
C          NSSWPW(ICFD)=IVAL
        ENDIF
        IF(CALLT(ICFD))THEN
          IVAL=NSWPT
          CALL EASKI(IVAL,' ','T number of sweeps (-) ?',
     &      1,'F',10,'F',10,' Number of sweeps',IER,nbhelp)
          NSWPT=IVAL
C          NSSWPT(ICFD)=IVAL
        ENDIF
        IF(KEMDL(ICFD))THEN
          IVAL=NSWPK
          CALL EASKI(IVAL,' ','K number of sweeps (-) ?',
     &      1,'F',10,'F',10,' Number of sweeps',IER,nbhelp)
          NSWPK=IVAL
C          NSSWPK(ICFD)=IVAL
          IVAL=NSWPD
          CALL EASKI(IVAL,' ','Epsilon number of sweeps (-)?',
     &      1,'F',10,'F',10,' Number of sweeps',IER,nbhelp)
          NSWPD=IVAL
C          NSSWPE(ICFD)=IVAL
        ENDIF
      ENDIF 
      CONTINUE

      RETURN
      END
