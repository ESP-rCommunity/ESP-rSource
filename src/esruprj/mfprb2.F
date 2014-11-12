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

C ################### MFPRB2 
C This file contains the following subroutines
C     MF010I
C     MF011I   Air inlet device self-adjustable, (polinomial)
C     MF012I   Power law flow component with max flow or shut-off setp.
C     MF015I
C     MF017I
C     MF020I
C     MF025I
C     MF030I
C     MF035I
C     MF040I
C     MF050I
C     MF110I
C     MF120I
C     MF130I
C     MF210I
C     MF211I  Cowl Roof Outlet
C     MF220I
C     MF230I
C     MF240I
C     MF250I
C     MF310I
C     MF410I
C     MF420I
C     MF460I
C     MF500I

C ****************** MF010I 
C Fluid flow component supplementary data input for
C power law volume flow resistance component (m = rho . a . dP^b [m^3/s])
      SUBROUTINE MF010I(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 
      CHARACTER LTPCMP*60,CMNAM*12,prompt*48,hold*32,outs*124

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF010I'  ! set for subroutine

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=3
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(outs,'(A)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_010'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',
     &    IW,nbhelp)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Flow coefficients.
      write(hold,'(2f12.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3)
  43  CALL EASKS(hold,'Coef `a` and exponent `b` in Q=a.dP^b(m^3/s) ?',
     &  '(help for synopsis)',32,' 1. 1. ','comp 10',IER,nbhelp)
      K=0
      CALL EGETWR(hold,K,VAL2,0.,99999.,'W','coef a',IER)
      CALL EGETWR(hold,K,VAL3,0.,99999.,'W','exp b',IER)
      IF(IER.NE.0) GOTO 43
      SUPCMP(ICMP,2)=VAL2
      SUPCMP(ICMP,3)=VAL3

      RETURN
      END

C ****************** MF011I 
C Fluid flow component supplementary data input for
C Self regulating vent for 15 or 30 m3/h at 20 Pa)
      SUBROUTINE MF011I(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 
      CHARACTER LTPCMP*60,CMNAM*12,prompt*48,hold*32,outs*124

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF011I'  ! set for subroutine

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=3
      ISDCNN(ICMP)=0

C Show short description of flow component.
      WRITE(outs,'(1X,A60)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_011'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL PHELPD('comp 11',nbhelp,'-',0,0,IER)

C Input fluid type.
      SUPCMP(ICMP,1)=1.0

C Flow coefficients.
      write(hold,'(2f12.1)')SUPCMP(ICMP,2),SUPCMP(ICMP,3)
  43  CALL EASKS(hold,
     &  'Rated flow (15 or 30) m3/h at 20 Pa and number of devices?',
     &  ' ',32,' 15. 1. ','comp 11',IER,nbhelp)
      K=0
      CALL EGETWR(hold,K,VAL2,0.,99999.,'W','Rated capacity',IER)
      CALL EGETWR(hold,K,VAL3,0.,99999.,'W','Nb of devices',IER)
      IF(IER.NE.0) GOTO 43
      SUPCMP(ICMP,2)=VAL2
      SUPCMP(ICMP,3)=VAL3

      RETURN
      END

C ****************** MF012I 
C Fluid flow component supplementary data input for
C power law volume flow resistance component (m = rho . a . dP^b [m^3/s])
C with max flow or shut-off behaviour.
C
C A.Geissler, Sept. 2014
C
      SUBROUTINE MF012I(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 
      CHARACTER LTPCMP*60,CMNAM*12,prompt*48,hold*48,outs*124

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF012I'  ! set for subroutine

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=5
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(outs,'(A)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_012'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',
     &    IW,nbhelp)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Flow coefficients, max flow and type switch.
      write(hold,'(4f11.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3),
     &  SUPCMP(ICMP,4),SUPCMP(ICMP,5)
  43  CALL EASKS(hold,
     & 'Coef `a` and exp. `b` in Q=a.dP^b(m^3/s),',
     & '`max flow`/`dp` (m^3/s)/(Pa),',
     & 'switch ? (help for synopsis)',
     & 48,' 0.001  0.67  0.01  0. ','comp 12',
     & IER,nbhelp)
      K=0
      CALL EGETWR(hold,K,VAL2,0.,99999.,'W','coef a',IER)
      CALL EGETWR(hold,K,VAL3,0.,99999.,'W','exp b',IER)
      CALL EGETWR(hold,K,VAL4,0.,99999.,'W','max flow/dp',IER)
      CALL EGETWR(hold,K,VAL5,0.,1.1,'W','switch',IER)
      IF(IER.NE.0) GOTO 43
      SUPCMP(ICMP,2)=VAL2
      SUPCMP(ICMP,3)=VAL3
      SUPCMP(ICMP,4)=VAL4
      SUPCMP(ICMP,5)=VAL5

      RETURN
      END

C ***************** MF015I 
C Fluid flow component supplementary data input for
C power law mass flow resistance component (m = a . dP^b [kg/s])
      SUBROUTINE MF015I(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 

      CHARACTER LTPCMP*60,CMNAM*12,prompt*48,hold*32,outs*124

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF015I'  ! set for subroutine

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=3
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(outs,'(A)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_015'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',
     &    IW,nbhelp)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Flow coefficients.
      write(hold,'(2f12.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3)
  43  CALL EASKS(hold,'Coef `a` and exponent `b` in m=a.dP^b [kg/s]?',
     &  '(help for synopsis)',32,' 1. 1. ','comp 15',IER,nbhelp)
      K=0
      CALL EGETWR(hold,K,VAL2,0.,99999.,'W','coef a',IER)
      CALL EGETWR(hold,K,VAL3,0.,99999.,'W','exp b',IER)
      IF(IER.NE.0) GOTO 43
      SUPCMP(ICMP,2)=VAL2
      SUPCMP(ICMP,3)=VAL3

      RETURN
      END

C ***************** MF017I 
C Fluid flow component supplementary data input for
C power law mass flow resistance component (m = a.rho^1/2.dP^b [kg/s])
      SUBROUTINE MF017I(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 

      CHARACTER LTPCMP*60,CMNAM*12,prompt*48,hold*32,outs*124

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF017I'  ! set for subroutine

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=3
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(outs,'(A)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_017'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',
     &    IW,nbhelp)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Flow coefficients.
      write(hold,'(2f12.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3)
  43  CALL EASKS(hold,
     &  'Coef `a` and exponent `b` in m=a.rho^1/2.dP^b (kg/s) ?',
     &  '(help for synopsis)',32,' 1. 1. ','comp 17',IER,nbhelp)
      K=0
      CALL EGETWR(hold,K,VAL2,0.,99999.,'W','coef a',IER)
      CALL EGETWR(hold,K,VAL3,0.,99999.,'W','exp b',IER)
      IF(IER.NE.0) GOTO 43
      SUPCMP(ICMP,2)=VAL2
      SUPCMP(ICMP,3)=VAL3

      RETURN
      END

C ***************** MF020I 
C Fluid flow component supplementary data input or check for
C quadratic law volume flow resistance component (dP = a.m/ro+b.(m/ro)^2)
      SUBROUTINE MF020I(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 

      CHARACTER LTPCMP*60,CMNAM*12,prompt*48,hold*32,outs*124

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF020I'  ! set for subroutine

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=3
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(outs,'(A)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_020'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',
     &    IW,nbhelp)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Flow coefficients.
      write(hold,'(2f12.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3)
  43  CALL EASKS(hold,
     &  'Coef `a` and exponent `b` in dP=a.Q+b.Q^2 (Pa/m^3/s) ?',
     &  '(help for synopsis)',32,' 1. 1. ','comp 20',IER,nbhelp)
      K=0
      CALL EGETWR(hold,K,VAL2,0.,99999.,'W','coef a',IER)
      CALL EGETWR(hold,K,VAL3,0.,99999.,'W','exp b',IER)
      IF(IER.NE.0) GOTO 43
      SUPCMP(ICMP,2)=VAL2
      SUPCMP(ICMP,3)=VAL3

      RETURN
      END

C ***************** MF025I 
C Fluid flow component supplementary data input or check for
C quadratic law mass flow resistance component (dP = a.m + b.m^2)
      SUBROUTINE MF025I(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 

      CHARACTER LTPCMP*60,CMNAM*12,prompt*48,hold*32,outs*124

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF025I'  ! set for subroutine

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=3
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(outs,'(A)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_025'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',
     &    IW,nbhelp)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Flow coefficients.
      write(hold,'(2f12.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3)
  43  CALL EASKS(hold,
     &  'Coef `a` and exponent `b` in dP = a.M + b.M^2 (Pa/(kg/s)^2)?',
     &  '(help for synopsis)',32,' 1. 1. ','comp 25',IER,nbhelp)
      K=0
      CALL EGETWR(hold,K,VAL2,0.,99999.,'W','coef a',IER)
      CALL EGETWR(hold,K,VAL3,0.,99999.,'W','exp b',IER)
      IF(IER.NE.0) GOTO 43
      SUPCMP(ICMP,2)=VAL2
      SUPCMP(ICMP,3)=VAL3

      RETURN
      END

C ******************* MF030I 
C Fluid flow component supplementary data input or check for
C constant volume flow rate component (m = rho . a)
      SUBROUTINE MF030I(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 

      CHARACTER LTPCMP*60,CMNAM*12,H*72,prompt*72,hold*32,outs*124
      logical ok,dok

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF030I'  ! set for subroutine

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=2
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(outs,'(A)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_030'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Input fluid type.
  42  if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',
     &    IW,nbhelp)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Offer shortcut definition.
      write(prompt,'(3a)') 'Derive flow through ',CMNAM(ICMP),
     &  ' (m^3/s) via:'
      CALL EASKABCD(prompt,' ','direct edit','l/s/person',
     &  'm^3/hr','ach (use volume of node)',IW,nbhelp)
      if(iw.eq.1)then

C Flow m^3/s (zero flow allowed).
        VAL=SUPCMP(ICMP,2)
        write(prompt,'(3a)') 'Flow through ',CMNAM(ICMP),' (m^3/s) ?'
   20   CALL EASKR(VAL,prompt,'(help for synopsis)',0.,'W',0.,'-',1.,
     &    'flow in comp',IER,nbhelp)
        IF(IER.NE.0) GOTO 20
        SUPCMP(ICMP,2)=VAL
        return
      elseif(iw.eq.2)then

C Derive from litres/second/person and nb of people.
        write(prompt,'(3a,F8.5,a)') ' The current flow of ',CMNAM(ICMP),
     &  ' (m^3/s) is ',SUPCMP(ICMP,2),'.'
        call edisp(iuout,prompt)
        hold = '  6.0    1.0 '
  43    CALL EASKS(hold,' litres/sec/person and number of people?',
     &    '(help for synopsis)',32,' 6.0 1 ','l/s/p',IER,nbhelp)
        K=0
        CALL EGETWR(hold,K,VAL2,0.1,99.,'W','l/s/p',IER)
        CALL EGETWR(hold,K,VAL3,0.1,999.,'W','number people',IER)
        IF(IER.NE.0) GOTO 43

        VAL= (val2/1000.0) * val3
        write(prompt,'(3a,F8.5,a)') ' The flow through ',CMNAM(ICMP),
     &  ' (m^3/s) is now ',VAL,'.'
        dok=.true.
        call askok(prompt,' Is this ok? ',OK,dok,nbhelp)
        if (.NOT.OK) goto 42
        SUPCMP(ICMP,2)=VAL
        return
      elseif(iw.eq.3)then

C Convert any existing value to m3/hr
        write(prompt,'(3a,F8.5,a)') ' The current flow of ',CMNAM(ICMP),
     &  ' (m^3/s) is ',SUPCMP(ICMP,2),'.'
        call edisp(iuout,prompt)
        VAL2=SUPCMP(ICMP,2)*3600.0
        write(prompt,'(3a)') 'Flow through ',CMNAM(ICMP),' (m^3/hr) ?'
   21   CALL EASKR(VAL2,prompt,'(help for synopsis)',0.,'W',0.,'-',10.,
     &    'flow m3/hr in comp',IER,nbhelp)
        IF(IER.NE.0) GOTO 21
        if(val2.gt.0.0) VAL = (val2/3600.0)
        write(prompt,'(3a,F8.5,a)') ' The flow through ',CMNAM(ICMP),
     &  ' (m^3/s) is now ',VAL,'.'
        dok=.true.
        call askok(prompt,' Is this ok? ',OK,dok,nbhelp)
        if (.NOT.OK) goto 42
        SUPCMP(ICMP,2)=VAL
        return
      elseif(iw.eq.4)then

C Based on ach against the volume of a particular node.
        write(prompt,'(3a,F8.5,a)') ' The current flow of ',CMNAM(ICMP),
     &  ' (m^3/s) is ',SUPCMP(ICMP,2),'.'
        call edisp(iuout,prompt)
        call ASKRNOD(' volume from which node','-',IV,IER)
        write(prompt,*)'Associated node volume is ',SUPNOD(IV,2)
        call edisp(iuout,prompt)
        val2=1.0
        write(prompt,'(3a)') 'Air changes for ',CMNAM(ICMP),' (ach) ?'
   22   CALL EASKR(VAL2,prompt,'(help for synopsis)',0.,'W',0.,'-',
     &    1.,'flow ach in comp',IER,nbhelp)
        IF(IER.NE.0) GOTO 22
        VAL = (SUPNOD(IV,2) * VAL2)/3600.0
        write(prompt,'(3a,F8.5,a)') ' The flow through ',CMNAM(ICMP),
     &  ' (m^3/s) is now ',VAL,'.'
        dok=.true.
        call askok(prompt,' Is this ok? ',OK,dok,nbhelp)
        if (.NOT.OK) goto 42
        SUPCMP(ICMP,2)=VAL
        return
      endif

      END

C **************** MF035I 
C Fluid flow component supplementary data input or check for
C constant mass flow rate component (m = a)
      SUBROUTINE MF035I(ICMP,IER)
#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 

      CHARACTER LTPCMP*60,CMNAM*12,prompt*48,outs*124

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF035I'  ! set for subroutine

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=2
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(outs,'(A)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_035'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',
     &    IW,nbhelp)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Flow m^3/s (zero flow allowed).
      VAL=SUPCMP(ICMP,2)
      write(prompt,'(3a)') 'Flow through ',CMNAM(ICMP),'(kg/s) ?'
   20 CALL EASKR(VAL,prompt,'(help for synopsis)',0.,'W',0.,'-',1.,
     &  'flow in comp',IER,nbhelp)
      IF(IER.NE.0) GOTO 20
      SUPCMP(ICMP,2)=VAL

      RETURN
      END

C ***************** MF040I 
C Fluid flow component supplementary data input or check for
C common orifice flow component (m = rho Cd A (2/rho dP)**.5)
      SUBROUTINE MF040I(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 

      CHARACTER LTPCMP*60,CMNAM*12,hold*32,prompt*48,outs*124

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF040I'  ! set for subroutine

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=3
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(outs,'(A)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_040'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',
     &    IW,nbhelp)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Opening area and discharge factor.
      write(hold,'(2f12.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3)
  43  CALL EASKS(hold,' Opening area (m^2) and discharge factor (-)?',
     &  '(help for synopsis)',32,' 1. 0.63 ','comp 40',IER,nbhelp)
      K=0
      CALL EGETWR(hold,K,VAL2,0.,99999.,'W','area',IER)
      CALL EGETWR(hold,K,VAL3,0.,1.,'W','discharge fac',IER)
      IF(IER.NE.0) GOTO 43
      SUPCMP(ICMP,2)=VAL2
      SUPCMP(ICMP,3)=VAL3

      RETURN
      END

C ***************** MF050I 
C Fluid flow component supplementary data input or check for
C laminar pipe volume flow rate component (m = rho (dP/(8 mu L)) pi R**4)
      SUBROUTINE MF050I(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 

      CHARACTER LTPCMP*60,CMNAM*12,hold*32,prompt*48,outs*124

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF050I'  ! set for subroutine

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=3
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(outs,'(A)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_050'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',
     &    IW,nbhelp)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Length of flow path (m) and radius of opening (m).
      write(hold,'(2f12.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3)
  43  CALL EASKS(hold,' Length of flow path (m) & opening radius (m)?',
     &  '(help for synopsis)',32,' 1. 0.025 ','comp 50',IER,nbhelp)
      K=0
      CALL EGETWR(hold,K,VAL2,0.,99999.,'W','length',IER)
      CALL EGETWR(hold,K,VAL3,0.01,99.,'W','radius',IER)
      IF(IER.NE.0) GOTO 43
      SUPCMP(ICMP,2)=VAL2
      SUPCMP(ICMP,3)=VAL3

      RETURN
      END

C ***************** MF110I 
C Fluid flow component supplementary data input or check for
C Specific air flow opening.
      SUBROUTINE MF110I(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)

      CHARACTER LTPCMP*60,CMNAM*12,prompt*48,outs*124

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF110I'  ! set for subroutine

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=2
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(outs,'(A)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_110'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Set fluid type.
      SUPCMP(ICMP,1)=1.

C Opening area (m^2).
      VAL=SUPCMP(ICMP,2)
      write(prompt,'(3a)')' Opening area of ',CMNAM(ICMP),' (m^2) ?'
   20 CALL EASKR(VAL,'(help for synopsis)',prompt,0.,'F',0.,'-',1.,
     &  'opening area',IER,nbhelp)
      IF(IER.NE.0) GOTO 20
      SUPCMP(ICMP,2)=VAL

      RETURN
      END

C ******************* MF120I 
C Fluid flow component supplementary data input or check for
C specific air flow crack component.
      SUBROUTINE MF120I(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)

      CHARACTER LTPCMP*60,CMNAM*12,hold*32,outs*124

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF120I'  ! set for subroutine

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=3
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(outs,'(A)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_120'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Set fluid type.
      SUPCMP(ICMP,1)=1.

C Crack width (mm) and length (m).
      write(hold,'(2f12.5)')SUPCMP(ICMP,2)*1000.0,SUPCMP(ICMP,3)
  43  CALL EASKS(hold,' Crack width (mm) and length (m) ?',
     &  '(help for synopsis)',32,' 5. 1.0 ','comp 120',IER,nbhelp)
      K=0
      CALL EGETWR(hold,K,VAL2,0.01,100.,'W','width',IER)
      CALL EGETWR(hold,K,VAL3,0.01,999.,'W','length',IER)
      IF(IER.NE.0) GOTO 43
      SUPCMP(ICMP,2)=VAL2/1000.
      SUPCMP(ICMP,3)=VAL3

      RETURN
      END

C ********************* MF130I 
C Fluid flow component supplementary data input or check for
C specific air flow door.
      SUBROUTINE MF130I(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)

      CHARACTER LTPCMP*60,CMNAM*12,hold*48,outs*124

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF130I'  ! set for subroutine

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=5
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(outs,'(A)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_130'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Set fluid type.
      SUPCMP(ICMP,1)=1.

C Door width, height, reference ht (m) & discharge factor.
      write(hold,'(4f11.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3),
     &  SUPCMP(ICMP,4),SUPCMP(ICMP,5)
  43  CALL EASKS(hold,
     &'Door width, height, adj node ht above base, & discharge factor?',
     &'(help for synopsis)',48,' 0.8  2.1  1.5 0.61 ','comp 130',
     &IER,nbhelp)
      K=0
      CALL EGETWR(hold,K,VAL2,0.01,20.,'W','width',IER)
      CALL EGETWR(hold,K,VAL3,0.01,99.,'W','height',IER)
      CALL EGETWR(hold,K,VAL4,0.01,999.,'W','adj node ht from base',IER)
      CALL EGETWR(hold,K,VAL5,0.01,1.,'W','disch fac',IER)
      IF(IER.NE.0) GOTO 43
      SUPCMP(ICMP,2)=VAL2
      SUPCMP(ICMP,3)=VAL3
      SUPCMP(ICMP,4)=VAL4
      SUPCMP(ICMP,5)=VAL5

      RETURN
      END

C ******************** MF210I 
C Fluid flow component supplementary data input or check for
C general flow conduit (ie. duct or pipe).
C     SUPCMP(ICMP,1) - fluid type (1=air, 2=water)
C     SUPCMP(ICMP,2) - conduit hydraulic diameter (m)
C     SUPCMP(ICMP,3) - cross-sectional area (m^2)
C     SUPCMP(ICMP,4) - conduit length (m)
C     SUPCMP(ICMP,5) - absolute wall roughness (m)
C     SUPCMP(ICMP,6) - sum of local dynamic loss factors (-)

      SUBROUTINE MF210I(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 

      CHARACTER LTPCMP*60,CMNAM*12,hold*48,prompt*48,outs*124
      logical close

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF210I'  ! set for subroutine

C Set number of supplementary data items.
      PI=4.*ATAN(1.)
      IER=0
      ISDCMP(ICMP)=6
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(outs,'(A)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_210'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',
     &    IW,nbhelp)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Hydraulic diameter, cross-sectional area, length, roughness
      write(hold,'(4f11.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3),
     &  SUPCMP(ICMP,4),SUPCMP(ICMP,5)*1000.
  43  CALL EASKS(hold,
     & 'Hydraulic diam, x-sectional area, length (m), roughness (mm)?',
     &  '(help for synopsis)',48,' 1.0  1.0  1.5 0.61 ','comp 130',
     &  IER,nbhelp)
      K=0
      CALL EGETWR(hold,K,VAL2,0.001,99.,'W','hydr diam',IER)
      CALL EGETWR(hold,K,VAL3,0.0,99.,'W','x-sect area',IER)
      CALL EGETWR(hold,K,VAL4,0.01,999.,'W','length',IER)
      CALL EGETWR(hold,K,VAL5,0.1,999.,'W','roughness',IER)
      IF(IER.NE.0) GOTO 43
      SUPCMP(ICMP,2)=VAL2
      SUPCMP(ICMP,3)=VAL3

C Area must be >= area of circular cross-section based on hydraulic
C diameter (.99 is used to avoid problems due to round off errors).
      CALL ECLOSE(SUPCMP(ICMP,3),0.0,0.001,close)
      IF(close) SUPCMP(ICMP,3)=PI*SUPCMP(ICMP,2)**2/4.
      if(SUPCMP(ICMP,3).LE.0..OR.
     &   SUPCMP(ICMP,3).LT.(.99*PI*SUPCMP(ICMP,2)**2/4.))then
        call edisp(iuout,' area must be >= hydraulic diam area')
        GOTO 43
      endif
      SUPCMP(ICMP,4)=VAL4
      SUPCMP(ICMP,5)=VAL5/1000.
      if(SUPCMP(ICMP,5).LT.0.0.OR.
     &  SUPCMP(ICMP,5).GE.SUPCMP(ICMP,2)/2.)then
        call edisp(iuout,' Roughness factor out of range.')
        GOTO 43
      endif

C Sum of local dynamic loss factors (-).
      VAL=SUPCMP(ICMP,6)
   60 CALL EASKR(VAL,' Sum of local dynamic loss factors',
     &  ' (Evaluated with average velocity in *this* conduit !!!) ?',
     &  0.,'F',0.,'-',1.,'dynamic loss fac',IER,nbhelp)
      IF(IER.NE.0) GOTO 60
      SUPCMP(ICMP,6)=VAL

      RETURN
      END

C ******************** MF211I
C Cowl roof ventilator using a method after Armando Pinto based on
C experimental results of:
C Gonzalez M.A. On the aerodynamics of natural ventilators. London
C Pergamon Press, 1984, Building and environment. Vol 19 No 3.

C The paper mentions 7 different models of cowels tested and table
C one gives several of the coefficients.
C Model 1 coef B varies between 0.72 and 1.28 depending on direction
C         coef n varies between 0.51 and 0.26 depending on direction
C Model 2 coef B varies between 0.37 and 0.80 depending on direction
C         coef n varies between 0.31 and 0.40 depending on direction
C Model 3 coef B 0.25  coef n 0.16
C Model 4 coef B 0.12  coef n 0.17
C Model 5 coef B 0.37  coef n 0.10
C Model 6 coef B 0.21  coef n 0.19
C Model 7 coef B 0.28  coef n 0.45

C Fluid mass flow calculation routine for flow component type:
C Cowl Roof Outlet
C     FLUID =   SUPCMP(ICMP,1) - fluid type (1=air)
C     DIAH =    SUPCMP(ICMP,2) - Cowl hydraulic diameter (m)
C     K =       SUPCMP(ICMP,3) - local dynamic loss factors (-)
C     B =       SUPCMP(ICMP,4) - coefficient of cowl
C     n =       SUPCMP(ICMP,5) - coefficient of cowl
C     WINDEFF = SUPCMP(ICMP,6) - FLAG =1 CONSIDER WIND EFFECT, =0 DONT

      SUBROUTINE MF211I(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 

      CHARACTER LTPCMP*60,CMNAM*12
      character outs*124,hold*48,prompt*48
      logical close

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF211I'  ! set for subroutine

C Set number of supplementary data items.
      PI=4.*ATAN(1.)
      IER=0
      ISDCMP(ICMP)=6
      ISDCNN(ICMP)=0

C Show short description of flow component.
      WRITE(outs,'(A)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_211'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL PHELPD('comp 211',nbhelp,'-',0,0,IER)

C Set fluid type.
      SUPCMP(ICMP,1)=1.0
     
C DATA OF COWL
      write(hold,'(4f11.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3),
     &  SUPCMP(ICMP,4),SUPCMP(ICMP,5)
  44  CALL EASKS(hold,
     & 'Hydrolic Diameter (m), K head loss, coef B, coef n (see help)?',
     & ' ',48,' 1.0  1.0  1.5 0.61 ','comp 211',IER,nbhelp)
      K=0
      CALL EGETWR(hold,K,VAL2,0.001,99.,'W','Hydro diameter',IER)
      CALL EGETWR(hold,K,VAL3,0.001,99.,'W','K head loss',IER)
      CALL EGETWR(hold,K,VAL4,0.0001,999.,'W','cowel coef B ',IER)
      CALL EGETWR(hold,K,VAL5,0.0001,1.,'W','cowel coef n ',IER)
      IF(IER.NE.0) GOTO 44
      
      SUPCMP(ICMP,2)=VAL2
      SUPCMP(ICMP,3)=VAL3
      SUPCMP(ICMP,4)=VAL4
      SUPCMP(ICMP,5)=VAL5

C Wind effect 1 include  0 don't include           
      VAL=SUPCMP(ICMP,6)
      write(prompt,'(3a)') 'Wind effect ? (1-yes/0-no)'
   20 CALL EASKR(VAL,' ',prompt,0.,'W',0.,'-',1.,'flow in comp',
     &  IER,nbhelp)
      IF(IER.NE.0) GOTO 20
      SUPCMP(ICMP,6)=VAL
           
      RETURN
      END

C ********************* MF220I 
C Fluid flow component supplementary data input or check for
C flow conduit ending in converging 3-leg junction
C     SUPCMP(ICMP,1) - fluid type (1=air, 2=water) ---------\
C     SUPCMP(ICMP,2) - conduit hydraulic diameter (m)        \
C     SUPCMP(ICMP,3) - cross-sectional area (m^2)             |= type
C     SUPCMP(ICMP,4) - conduit length (m)                     |  210
C     SUPCMP(ICMP,5) - absolute wall roughness (m)           /
C     SUPCMP(ICMP,6) - sum of local dynamic loss factors (-)/
C     SUPCMP(ICMP,7) - cross-sectional area of common junction section
C     SUPCMP(ICMP,8) - Ccp coefficient a0 (-)
C      '  '   '   '     '    '  '  '    '  '
C     SUPCMP(ICMP,13) - Ccp coefficient a5 (-)

      SUBROUTINE MF220I(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      CHARACTER LTPCMP*60,CMNAM*12
      character outs*124
      logical OK,dok,close

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF220I'  ! set for subroutine

C Synopsis of flow component.
      WRITE(outs,'(1X,A60)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_220'
      call gethelptext(helpinsub,helptopic,nbhelp)
      dok=.true.
      CALL ASKOK(outs,'Synopsis of this component type?',OK,dok,nbhelp)

      if(OK) CALL PHELPD('comp 220',nbhelp,'-',0,0,IER)

C Use MF210 to input the general data.
      ISDCMP(ICMP)=6
      ISDCNN(ICMP)=0
      CALL MF210I(ICMP,IER)
      IF(IER.NE.0)then
        call MFERR(ICMP,' Problem with basic type 210 data!',IER)
        return
      endif

C Reset no of supplemental data for this specific case.
      ISDCMP(ICMP)=13
      ISDCNN(ICMP)=1
      call edisp(iuout,' ')
      call edisp(iuout,' Now we proceed with the specifics of a:')
      WRITE(outs,'(1X,A)') LTPCMP(ICMP)
      call edisp(iuout,outs)

C Cross-sectional area of common section of junction.
      VAL=SUPCMP(ICMP,7)
   20 CALL EASKR(VAL,
     &  ' Cross-sectional area of junction common (m^2) ',
     &  ' (`0` indicates area equals area of conduit) ? ',
     &  -0.0001,'F',0.,'-',0.,'x section area',IER,nbhelp)
      IF(IER.NE.0) GOTO 20
      CALL ECLOSE(VAL,0.0,0.001,close)
      IF(.NOT.close) THEN
        SUPCMP(ICMP,7)=VAL
      ELSE
        SUPCMP(ICMP,7)=SUPCMP(ICMP,3)
      END IF

C Coefficients of Ccp relation.
   30 DO 32 I=0,5
        write(outs,'(A,I1)') ' Coef a',I
        VAL=SUPCMP(ICMP,I+8)
        CALL EASKR(VAL,outs,' ? ',0.,'-',0.,'-',0.,'Cf a0',IER,nbhelp)
        SUPCMP(ICMP,I+8)=VAL
   32 CONTINUE
      if(IER.NE.0)goto 30

      RETURN
      END

C ***************** MF230I 
C Fluid flow component supplementary data input or check for
C flow conduit starting in diverging 3-leg junction
C     SUPCMP(ICMP,1) - fluid type (1=air, 2=water) ---------\
C     SUPCMP(ICMP,2) - conduit hydraulic diameter (m)        \
C     SUPCMP(ICMP,3) - cross-sectional area (m^2)             |= type
C     SUPCMP(ICMP,4) - conduit length (m)                     |  210
C     SUPCMP(ICMP,5) - absolute wall roughness (m)           /
C     SUPCMP(ICMP,6) - sum of local dynamic loss factors (-)/
C     SUPCMP(ICMP,7) - cross-sectional area of common junction section
C     SUPCMP(ICMP,8) - Ccp coefficient a0 (-)
C      '  '   '   '     '    '  '  '    '  '
C     SUPCMP(ICMP,13) - Ccp coefficient a5 (-)

      SUBROUTINE MF230I(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      CHARACTER LTPCMP*60,CMNAM*12
      character outs*124
      logical OK,dok,close

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF230I'  ! set for subroutine

C Show short description of flow component.
      IER=0
      dok=.true.
      WRITE(outs,'(a)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_230'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL ASKOK(outs,'Synopsis of this component type?',OK,dok,nbhelp)

      if(OK) CALL PHELPD('comp 230',nbhelp,'-',0,0,IER)

C Use MF210 to input the general data.
      ISDCMP(ICMP)=6
      ISDCNN(ICMP)=0
      CALL MF210I(ICMP,IER)
      IF(IER.NE.0)then
        call MFERR(ICMP,' Problem with basic type 210 data!',IER)
        return
      endif

C Reset no of supplemental data for this specific case.
      ISDCMP(ICMP)=13
      ISDCNN(ICMP)=1
      call edisp(iuout,' Now we proceed with the specifics of a:')
      WRITE(outs,'(1X,A)') LTPCMP(ICMP)
      call edisp(iuout,outs)

C Cross-sectional area of common section of junction.
      VAL=SUPCMP(ICMP,7)
   20 CALL EASKR(VAL,
     &  ' Cross-sectional area of junction common (m^2) ',
     &  ' (`0` indicates area equals area of conduit) ? ',
     &  -0.0001,'F',0.,'-',0.,'x section area',IER,nbhelp)
      IF(IER.NE.0) GOTO 20
      CALL ECLOSE(VAL,0.0,0.001,close)
      IF(.NOT.close) THEN
        SUPCMP(ICMP,7)=VAL
      ELSE
        SUPCMP(ICMP,7)=SUPCMP(ICMP,3)
      END IF

C Coefficients of Ccp relation.
   30 DO 32 I=0,5
        write(outs,'(A,I1)') ' Coef a',I
        VAL=SUPCMP(ICMP,I+8)
        CALL EASKR(VAL,outs,' ? ',
     &    0.,'-',0.,'-',0.,'Coef a0',IER,nbhelp)
        SUPCMP(ICMP,I+8)=VAL
   32 CONTINUE
      if(IER.NE.0)goto 30

      RETURN
      END

C ********************* MF240I 
C Fluid flow component supplementary data input or check for
C flow conduit ending in converging 4-leg junction
C     SUPCMP(ICMP,1) - fluid type (1=air, 2=water) ---------\
C     SUPCMP(ICMP,2) - conduit hydraulic diameter (m)        \
C     SUPCMP(ICMP,3) - cross-sectional area (m^2)             |= type
C     SUPCMP(ICMP,4) - conduit length (m)                     |  210
C     SUPCMP(ICMP,5) - absolute wall roughness (m)           /
C     SUPCMP(ICMP,6) - sum of local dynamic loss factors (-)/
C     SUPCMP(ICMP,7) - cross-sectional area of common junction section
C     SUPCMP(ICMP,8) - Ccp coefficient a0 (-)
C      '  '   '   '     '    '  '  '    '  '
C     SUPCMP(ICMP,17) - Ccp coefficient a9 (-)

      SUBROUTINE MF240I(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      CHARACTER LTPCMP*60,CMNAM*12
      character outs*124
      logical OK,dok,close

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF240I'  ! set for subroutine

C Set/check number of supplementary data items.
      IER=0

C Show short description of flow component.
      dok=.true.
      WRITE(outs,'(a)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_240'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL ASKOK(outs,'Synopsis of this component type?',OK,dok,nbhelp)

      if(OK) CALL PHELPD('comp 240',nbhelp,'-',0,0,IER)

C Use MF210 to input the general data.
      ISDCMP(ICMP)=6
      ISDCNN(ICMP)=0
      CALL MF210I(ICMP,IER)
      IF(IER.NE.0)then
        call MFERR(ICMP,' Problem with basic type 210 data!',IER)
        return
      endif

C Reset no of supplemental data for this specific case.
      ISDCMP(ICMP)=17
      ISDCNN(ICMP)=2
      call edisp(iuout,' Now we proceed with the specifics of a:')
      WRITE(outs,'(1X,A)') LTPCMP(ICMP)
      call edisp(iuout,outs)

C Cross-sectional area of common section of junction.
      VAL=SUPCMP(ICMP,7)
   20 CALL EASKR(VAL,
     &  ' Cross-sectional area of junction common (m^2) ',
     &  ' (`0` indicates area equals area of conduit) ? ',
     &    -0.0001,'F',0.,'-',0.,'x section area',IER,nbhelp)
      IF(IER.NE.0) GOTO 20
      CALL ECLOSE(VAL,0.0,0.001,close)
      IF(.NOT.close) THEN
        SUPCMP(ICMP,7)=VAL
      ELSE
        SUPCMP(ICMP,7)=SUPCMP(ICMP,3)
      END IF

C Coefficients of Ccp relation.
   30 DO 32 I=0,9
        write(outs,'(A,I1)') ' Coef a',I
        VAL=SUPCMP(ICMP,I+8)
        CALL EASKR(VAL,outs,' ? ',
     &    0.,'-',0.,'-',0.,'Coef a0',IER,nbhelp)
        SUPCMP(ICMP,I+8)=VAL
   32 CONTINUE
      if(IER.NE.0)goto 30

      RETURN
      END


C ***************** MF250I 
C Fluid flow component supplementary data input or check for
C flow conduit starting in diverging 4-leg junction
C     SUPCMP(ICMP,1) - fluid type (1=air, 2=water) ---------\
C     SUPCMP(ICMP,2) - conduit hydraulic diameter (m)        \
C     SUPCMP(ICMP,3) - cross-sectional area (m^2)             |= type
C     SUPCMP(ICMP,4) - conduit length (m)                     |  210
C     SUPCMP(ICMP,5) - absolute wall roughness (m)           /
C     SUPCMP(ICMP,6) - sum of local dynamic loss factors (-)/
C     SUPCMP(ICMP,7) - cross-sectional area of common junction section
C     SUPCMP(ICMP,8) - Ccp coefficient a0 (-)
C      '  '   '   '     '    '  '  '    '  '
C     SUPCMP(ICMP,17) - Ccp coefficient a9 (-)

      SUBROUTINE MF250I(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      CHARACTER LTPCMP*60,CMNAM*12
      character outs*124
      logical OK,dok,close

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF250I'  ! set for subroutine

      IER=0

C Show short description of flow component.
      dok=.true.
      WRITE(outs,'(a)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_250'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL ASKOK(outs,'Synopsis of this component type?',OK,dok,nbhelp)

      if(OK) CALL PHELPD('comp 250',nbhelp,'-',0,0,IER)

C Use MF210 to input the general data.
      ISDCMP(ICMP)=6
      ISDCNN(ICMP)=0
      CALL MF210I(ICMP,IER)
      IF(IER.NE.0)then
        call MFERR(ICMP,' Problem with basic type 210 data!',IER)
        return
      endif

C Reset no of supplemental data for this specific case.
      ISDCMP(ICMP)=17
      ISDCNN(ICMP)=2
      call edisp(iuout,' Now we proceed with the specifics of a:')
      WRITE(outs,'(1X,A)') LTPCMP(ICMP)
      call edisp(iuout,outs)

C Cross-sectional area of common section of junction.
      VAL=SUPCMP(ICMP,7)
   20 CALL EASKR(VAL,
     &  ' Cross-sectional area of junction common (m^2) ',
     &  ' (`0` indicates area equals area of conduit) ? ',
     &  -0.0001,'F',0.,'-',0.,'x section area',IER,nbhelp)
      IF(IER.NE.0) GOTO 20
      CALL ECLOSE(VAL,0.0,0.001,close)
      IF(.NOT.close) THEN
        SUPCMP(ICMP,7)=VAL
      ELSE
        SUPCMP(ICMP,7)=SUPCMP(ICMP,3)
      END IF

C Coefficients of Ccp relation.
   30 DO 32 I=0,9
        write(outs,'(A,I1)') ' Coef a',I
        VAL=SUPCMP(ICMP,I+8)
        CALL EASKR(VAL,outs,' ? ',
     &    0.,'-',0.,'-',0.,'Coef a0',IER,nbhelp)
        SUPCMP(ICMP,I+8)=VAL
   32 CONTINUE
      if(IER.NE.0)goto 30

      RETURN
      END


C ********************* MF310I
C Fluid flow component supplementary data input or check for
C general flow inducer (dP = SUM (i=0 to 3) ai.(m/ro)^i; which is
C                       only valid for q,min <= m/ro <= q,max )

C << consider what would need to change to implement a cancel
C << option within this interface and the degree to which the
C << data instanciation needs to change.

      SUBROUTINE MF310I(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 

C Maxp is max number of points on performance curve and ideg is one
C more than degree of curve (maxp>=ideg)
      parameter (maxp=20)
      parameter (ideg=4)

      CHARACTER LTPCMP*60,CMNAM*12
      character outs*124,hold*48,prompt*48
      logical OK,dok,closea,closeb,closec,SINGUL
      doubleprecision xval(maxp),yval(maxp),RIHAS(IDEG)

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF310I'  ! set for subroutine

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=7
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(outs,'(A)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_310'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',
     &    IW,nbhelp)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Volume flow rate lower limit for dP(m) relation.
      VAL=SUPCMP(ICMP,2)
   20 CALL EASKR(VAL,
     &  ' Vol. flow rate lower limit for dP(m) relation (m^3/s) ?',
     &  '(help for synopsis)',0.001,'F',0.,'-',1.,'vol lower limit',
     &  IER,nbhelp)
      IF(IER.NE.0) GOTO 20
      SUPCMP(ICMP,2)=VAL

C Volume flow rate upper limit for dP(m) relation.
      VAL=SUPCMP(ICMP,3)
   30 CALL EASKR(VAL,' ',
     &  ' Vol. flow rate upper limit for dP(m) relation (m^3/s) ?',
     &   0.001,'F',0.,'-',1.,'vol upper limit',IER,nbhelp)
      IF(IER.NE.0) GOTO 30
      SUPCMP(ICMP,3)=VAL

C Coefficients or performance curve 
      WRITE(prompt,'(a)') 'Is input coefficients or performance curve?'
      CALL EASKAB(' ',prompt,'coefficients','perf curve',IIW,nbhelp)
      IF(IIW.EQ.1) THEN

C Flow coefficient a0.
        call edisp(iuout,
     &    ' Coefficients in dP=a0+a1.q+a2.q^2+a3.q^3 (Pa) ')
        call edisp(iuout,' (Remember that this is a flow INDUCER where')
        call edisp(iuout,' dP in the dP(q) relationship stands for ')
        call edisp(iuout,' pressure rise as opposed to pressure drop)')

C  dP=a0+a1.q+a2.q^2+a3.q^3 (Pa).
        write(hold,'(4f11.2)')SUPCMP(ICMP,4),SUPCMP(ICMP,5),
     &    SUPCMP(ICMP,6),SUPCMP(ICMP,7)
  43    CALL EASKS(hold,
     &    ' Coef a0 a1 a2 a3 in dP=a0+a1.q+a2.q^2+a3.q^3 (Pa) ?',
     &    ' ',48,' 0.1  0.1  0.1  0.1 ','comp 310 coeff',IER,nbhelp)
        K=0
        CALL EGETWR(hold,K,VAL2,0.0,0.,'-','a0',IER)
        CALL EGETWR(hold,K,VAL3,0.0,0.,'-','a1',IER)
        CALL EGETWR(hold,K,VAL4,0.0,0.,'-','a2',IER)
        CALL EGETWR(hold,K,VAL5,0.0,0.,'-','a3',IER)
        IF(IER.NE.0) GOTO 43
        SUPCMP(ICMP,4)=VAL2
        SUPCMP(ICMP,5)=VAL3
        SUPCMP(ICMP,6)=VAL4
        SUPCMP(ICMP,7)=VAL5

C Check if not all coefficients equal to zero.
        CALL ECLOSE(SUPCMP(ICMP,5),0.0,0.001,closea)
        CALL ECLOSE(SUPCMP(ICMP,6),0.0,0.001,closeb)
        CALL ECLOSE(SUPCMP(ICMP,7),0.0,0.001,closec)
        IF(closea.and.closeb.and.closec) GOTO 43

C Points to be read off performance curve.

C << to be done: implement a cancel option for easki >>
      ELSE
 71     CALL PHELPD('curve fit',nbhelp,'-',0,0,IER)
 88     CALL EASKI(IVAL,
     &    'How many points to be read off performance curve?',
     &    '(note: CANCEL option not implemented)',
     &    4,'F',maxp,'F',4,'points read off',IER,nbhelp)
        DO 53 IIVAL=1,IVAL
          WRITE(HOLD,'(2F11.5)')XVAL(IIVAL),YVAL(IIVAL)
 63       CALL EASKS(HOLD,
     &      ' Enter Volume flow rate (m^3/s) and pressure (Pa)',' ',24,
     &      ' 0.1 0.1 ','comp 310 perf curv',IER,nbhelp)
          K=0
          CALL EGETWR(hold,K,VALX,0.0,0.,'-','pressure value',IER)
          CALL EGETWR(hold,K,VALY,0.0,0.,'-','vol flow rate value',IER)
          IF(IER.NE.0) GOTO 63
          XVAL(IIVAL)=VALX
          YVAL(IIVAL)=VALY
 53     CONTINUE

C Display coordinates chosen and ask for confirmation
        CALL EDISP(IUOUT,' Coordinates chosen are (m^3/s,Pa):')
        DO 477 K=1,IVAL
          WRITE(hold,'(1X,A,f11.2,A,f11.2,A)') 
     &      '(',XVAL(K),',',YVAL(K),')'
          CALL EDISP(IUOUT,hold)
 477    CONTINUE
        dok=.true.
        CALL ASKOK(' ',' Accept values?  ',OK,dok,nbhelp)
        IF(.NOT.OK) GOTO 88

C Do least squares curve fit
        CALL LESQRA(IVAL,XVAL,YVAL,SINGUL,RIHAS)

C Check for singular matrix
        IF(SINGUL)THEN
          CALL EDISP(IUOUT,' Points do not define a function')
          GOTO 88
        ENDIF

C Display coefficients and ask for confirmation
        WRITE(outs,'(F11.2,A,F11.2,A,F11.2,A,F11.2,A)')
     &    RIHAS(1),' + ',RIHAS(2),'.q + ',RIHAS(3),
     &    '.q^2 + ',RIHAS(4),'.q^3'
        CALL EDISP(IUOUT,' The following function will be used 
     &    for dpressure:')
        CALL EDISP(IUOUT,outs)
        dok=.true.
        CALL ASKOK(' ',' Accept this?  ',OK,dok,nbhelp)
        IF(.NOT.OK) GOTO 71

C Fill common blocks
        SUPCMP(ICMP,4)=REAL(RIHAS(1))
        SUPCMP(ICMP,5)=REAL(RIHAS(2))
        SUPCMP(ICMP,6)=REAL(RIHAS(3))
        SUPCMP(ICMP,7)=REAL(RIHAS(4))
      ENDIF

      RETURN
      END

C ******************* MF410I 
C Fluid flow component supplementary data input or check for
C general flow corrector (ie. valve or damper)
      SUBROUTINE MF410I(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 
      CHARACTER LTPCMP*60,CMNAM*12,prompt*48,outs*124

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF410I'  ! set for subroutine

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=7
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(outs,'(A)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_410'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',
     &    IW,nbhelp)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Standard density rho 0 (kg/m^3).
      VAL=SUPCMP(ICMP,2)
   20 CALL EASKR(VAL,
     &  ' Density of fluid used for `measuring` k,vs (kg/m^3)',
     &  ' (for water this is usually 1000 kg/m^3) ? ',
     &  0.,'F',0.,'-',1000.,'fluid density',IER,nbhelp)
      IF(IER.NE.0) GOTO 20
      SUPCMP(ICMP,2)=VAL

C Standard pressure difference dP0 (Pa).
      VAL=SUPCMP(ICMP,3)
   30 CALL EASKR(VAL,
     &  ' Pressure difference used for `measuring` k,vs (Pa)',
     &  ' (for water this is usually 100000 Pa) ? ',
     &  0.,'F',0.,'-',100000.,'pres diff',IER,nbhelp)
      IF(IER.NE.0) GOTO 30
      SUPCMP(ICMP,3)=VAL

C Characteristic index (1 = linear, 2 = logartihmic)
      CALL EASKAB(' ',' Characteristic index: ',
     &            'linear','logartihmic',IW,nbhelp)
      SUPCMP(ICMP,4)=REAL(IW)

C Volume flow when fully open k,vs (m^3/s)
      VAL=SUPCMP(ICMP,5)
   50 CALL EASKR(VAL,' ',
     &  '  Volume flow when fully open: k,vs (m^3/s) ? ',
     &  0.,'F',0.,'-',1.,'vol flow open',IER,nbhelp)
      IF(IER.NE.0) GOTO 50
      SUPCMP(ICMP,5)=VAL

C Theoretical flow when closed, k,v0 (%).
      VAL=SUPCMP(ICMP,6)
   60 CALL EASKR(VAL,' ',
     &  ' Theoretical flow when closed(ie H/H100=0%): k,v0 (%) ? ',
     &   0.,'F',100.,'F',0.,'vol flow close',IER,nbhelp)
      IF(IER.NE.0) GOTO 60
      SUPCMP(ICMP,6)=VAL

C Lower validity limit for characteristic, k,vr (%).
      VAL=SUPCMP(ICMP,7)
   70 CALL EASKR(VAL,' ',
     &  ' Lower validity limit for characteristic: k,vr (%) ? ',
     &  0.,'F',100.,'F',0.,'lower val limit',IER,nbhelp)
      IF(IER.NE.0) GOTO 70
      SUPCMP(ICMP,7)=VAL

      RETURN
      END

C ****************** MF420I 
C Fluid flow component supplementary data input or check for
C flow corrector (ie. valve or damper) with polynomial flow resistance.
C See synopsis for SUPCMP data.
      SUBROUTINE MF420I(ICMP,IER)

#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 
      CHARACTER LTPCMP*60,CMNAM*12,hold*48,prompt*48,outs*124

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF420I'  ! set for subroutine

C Set/check number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=6
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(outs,'(A)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_420'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',
     &    IW,nbhelp)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C area of cross-section containing flow corrector
      VAL=SUPCMP(ICMP,2)
   20 CALL EASKR(VAL,
     &  ' Area of cross-section containing flow corrector (m^2) ?',
     &  '(help for synopsis)',0.0001,'F',0.,'-',1.,'x section area ',
     &  IER,nbhelp)
      IF(IER.NE.0) GOTO 20
      SUPCMP(ICMP,2)=VAL

C Coefficients of C = fie(H/H100) relation.
      write(hold,'(4f11.5)')SUPCMP(ICMP,3),SUPCMP(ICMP,4),
     &  SUPCMP(ICMP,5),SUPCMP(ICMP,6)
  43  CALL EASKS(hold,
     &'Coef a0 to a3 in a0+a1.(H/H100)+a2.(H/H100)^2+a3.(H/H100)^3 ?',
     &  ' ',48,' 0.1  0.1  0.1  0.1 ','comp 420',IER,nbhelp)
      K=0
      CALL EGETWR(hold,K,VAL2,0.0,0.,'-','a0',IER)
      CALL EGETWR(hold,K,VAL3,0.0,0.,'-','a1',IER)
      CALL EGETWR(hold,K,VAL4,0.0,0.,'-','a2',IER)
      CALL EGETWR(hold,K,VAL5,0.0,0.,'-','a3',IER)
      IF(IER.NE.0) GOTO 43
      SUPCMP(ICMP,3)=VAL2
      SUPCMP(ICMP,4)=VAL3
      SUPCMP(ICMP,5)=VAL4
      SUPCMP(ICMP,6)=VAL5

      RETURN
      END

C ******************* MF460I 
C Fluid flow component supplementary data input or check for
C fixed flow rates controller. See synopsis for SUPCMP data.
      SUBROUTINE MF460I(ICMP,IER)
#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 
      CHARACTER LTPCMP*60,CMNAM*12,hold*32,prompt*48,outs*124

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF460I'  ! set for subroutine

C Set number of supplementary data items
      IER=0
      ISDCMP(ICMP)=3
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(outs,'(A)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_460'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',
     &    IW,nbhelp)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Flow rate when S <  Ssp (m^3/s).
      write(hold,'(2f11.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3)
  43  CALL EASKS(hold,' Flows when S < Ssp and S > Ssp (m3/s) ?',
     &  '(help for synopsis)',32,' 0.1 0.2 ','comp 460',IER,nbhelp)
      K=0
      CALL EGETWR(hold,K,VAL2,0.,0.,'-','flow <ssp',IER)
      CALL EGETWR(hold,K,VAL3,0.,0.,'-','flow >ssp',IER)
      IF(IER.NE.0) GOTO 43
      SUPCMP(ICMP,2)=VAL2
      SUPCMP(ICMP,3)=VAL3

      RETURN
      END

C ******************* MF500I 
C Fluid flow component supplementary data input or check for
C fixed flow rates controller. See synopsis for SUPCMP data.
      SUBROUTINE MF500I(ICMP,IER)
#include "net_flow.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 
      CHARACTER LTPCMP*60,CMNAM*12,prompt*48,outs*124
      PARAMETER (MCCN=3)

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MF500I'  ! set for subroutine

C Set number of supplementary data items
      IER=0
      ISDCMP(ICMP)=6
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(outs,'(A)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      helptopic='flow_cmp_500'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',
     &    IW,nbhelp)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Ask component and control action, currently three components are
C supported. To increase this number modify MCCN
      DO 500 II=1,MCCN
        call ASKRCMP('select component','-',ICOMPN,IER)

C First reset help message the first 5 array elements of which are 
C overwritten by ASKRCMP
        SUPCMP(ICMP,2*II)=ICOMPN
        IF(II.NE.MCCN)THEN
          CALL EASKR(VAL,'Enter control action for this component',
     &    ' ',0.,'W',0.,'-',1.,'flow in comp',IER,nbhelp)
          SUPCMP(ICMP,2*II+1)=VAL
        ENDIF
 500  CONTINUE
      RETURN
      END
