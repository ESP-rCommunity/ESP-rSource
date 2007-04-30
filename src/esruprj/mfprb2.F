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

C ****************** MF010I 
C Fluid flow component supplementary data input for
C power law volume flow resistance component (m = rho . a . dP^b [m^3/s])
      SUBROUTINE MF010I(ICMP,IER)

#include "net_flow.h"
      common/pophelp/h(60)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 
      CHARACTER LTPCMP*60,CMNAM*12,prompt*48,H*72,hold*32

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=3
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2)='Type 10 is a power law volume flow resistance.'
      H(3)='Power law relationships are correlations rather than'
      H(4)='physical law.  It is a convenient characterization of'
      H(5)='openings for building air infiltration calculations'
      H(6)='and much data is available in this form.'
      H(7)='See ESP-r manual section 3.6.'
      H(8)=' '
      H(9)='  (m = rho . a . dP^b [m^3/s]) '
      H(10)=' '
      H(11)='Supplemental data: '
      H(12)='  (1) fluid type (1=air, 2=water), '
      H(13)='  (2) flow coef a, '
      H(14)='  (3) flow exponent b.'

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',IW,14)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Flow coefficients.
      write(hold,'(2f12.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3)
  43  CALL EASKS(hold,'Coef `a` and exponent `b` in Q=a.dP^b(m^3/s) ?',
     &  '(help for synopsis)',32,' 1. 1. ','comp 10',IER,14)
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
      character outs*124

#include "net_flow.h"
      common/pophelp/h(60)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 
      CHARACTER LTPCMP*60,CMNAM*12,prompt*48,H*72,hold*32

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=3
      ISDCNN(ICMP)=0

C Show short description of flow component.
      WRITE(outs,'(1X,A60)') LTPCMP(ICMP)
      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2)='Type 11 approximates self regulating devices which have'
      H(3)='a rated flow rate of 15m3/h at 20 Pa. or 30m3/h.'
      H(4)=' '
      H(5)='These devices are designed to maintain flow close to these'
      H(6)='rates over a range of pressure differences. The component'
      H(7)='uses a polynomial to predict flows at other pressures. '
      H(8)=' '
      H(9)='  (Q = Nb devides*(a6*DP**6+a5*DP**5++...a1*DP) '
      H(10)=' '
      H(11)='Supplemental data: '
      H(12)= ' '
      H(13)='  (2) air flow at 20 Pa, 15 or 30 m3/h'
      H(14)='  (3) number of devices'      
      H(15)=' '
      H(16)='Based on work by Armando Pinto. See mfmach code MF011C'
      H(17)='for details of the method.'
      CALL PHELPD('comp 11',17,'-',0,0,IER)

C Input fluid type.
      SUPCMP(ICMP,1)=1.0

C Flow coefficients.
      write(hold,'(2f12.1)')SUPCMP(ICMP,2),SUPCMP(ICMP,3)
  43  CALL EASKS(hold,
     &  'Rated flow (15 or 30) m3/h at 20 Pa and number of devices?',
     &  ' ',32,' 15. 1. ','comp 11',IER,17)
      K=0
      CALL EGETWR(hold,K,VAL2,0.,99999.,'W','Rated capacity',IER)
      CALL EGETWR(hold,K,VAL3,0.,99999.,'W','Nb of devices',IER)
      IF(IER.NE.0) GOTO 43
      SUPCMP(ICMP,2)=VAL2
      SUPCMP(ICMP,3)=VAL3

      RETURN
      END

C ***************** MF015I 
C Fluid flow component supplementary data input for
C power law mass flow resistance component (m = a . dP^b [kg/s])
      SUBROUTINE MF015I(ICMP,IER)

#include "net_flow.h"
      common/pophelp/h(60)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 

      CHARACTER LTPCMP*60,CMNAM*12,prompt*48,H*72,hold*32

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=3
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2)='Type 15 is a power law mass flow resistance component.'
      H(3)='Power law relationships are correlations rather than'
      H(4)='physical law. See discussion of type 10 as well as' 
      H(5)='ESP-r manual section 3.6.'
      H(6)=' '
      H(7)='(m = a . dP^b [kg/s]) '
      H(8)=' '
      H(9)='Supplemental data: '
      H(10)='  (1) fluid type (1=air, 2=water),'
      H(11)='  (2) flow coef a,'
      H(12)='  (3) flow exponent b.'

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',IW,12)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Flow coefficients.
      write(hold,'(2f12.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3)
  43  CALL EASKS(hold,'Coef `a` and exponent `b` in m=a.dP^b [kg/s]?',
     &  '(help for synopsis)',32,' 1. 1. ','comp 15',IER,12)
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
      common/pophelp/h(60)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 

      CHARACTER LTPCMP*60,CMNAM*12,prompt*48,H*72,hold*32

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=3
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2)='Type 17 is a power law mass flow component.'
      H(3)='Power law relationships are correlations rather than'
      H(4)='physical law. See discussion of type 10 as well as'
      H(5)='ESP-r manual section 3.6.'
      H(6)='  '
      H(7)='(m = a.rho^1/2.dP^b [kg/s]) '
      H(8)='  '
      H(9)='Supplemental data: '
      H(10)='  (1) fluid type (1=air, 2=water),'
      H(11)='  (2) flow coef a, '
      H(12)='  (3) flow exponent b.'
      H(13)='  '

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',IW,13)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Flow coefficients.
      write(hold,'(2f12.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3)
  43  CALL EASKS(hold,
     &  'Coef `a` and exponent `b` in m=a.rho^1/2.dP^b (kg/s) ?',
     &  '(help for synopsis)',32,' 1. 1. ','comp 17',IER,13)
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
      common/pophelp/h(60)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 

      CHARACTER LTPCMP*60,CMNAM*12,prompt*48,H*72,hold*32

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=3
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2)='Type 20 is a quadratic law volume flow component.'
      H(3)='Quadratic law relationships are included because some'
      H(4)='sources regard this is a superior form for building'
      H(5)='fabric infiltration as it allows consideration of both'
      H(6)='the laminar and turbulent flow. Type 20 is more com-'
      H(7)='monly encountered in the literature than type 25.'
      H(8)=' '
      H(9)='(dP = a.m/ro+b.(m/ro)^2) '
      H(10)=' '
      H(11)='Supplemental data: '
      H(12)='  (1) fluid type (1=air, 2=water),'
      H(13)='  (2) flow coef a (Pa.s/m^3),'
      H(14)='  (3) flow exponent b (Pa.(s/m^3)^2).'
      H(15)=' '
      H(16)='See ESP-r manual section 3.6.'

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',IW,16)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Flow coefficients.
      write(hold,'(2f12.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3)
  43  CALL EASKS(hold,
     &  'Coef `a` and exponent `b` in dP=a.Q+b.Q^2 (Pa/m^3/s) ?',
     &  '(help for synopsis)',32,' 1. 1. ','comp 20',IER,16)
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
      common/pophelp/h(60)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 

      CHARACTER LTPCMP*60,CMNAM*12,prompt*48,H*72,hold*32

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=3
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2)='Type 25 is a quadratic law mass flow component which'
      H(3)='is similar to type 20.'
      H(4)=' '
      H(5)='dP = a.M + b.M^2 (Pa/(kg/s)^2)  '
      H(6)=' '
      H(7)='Supplemental data: '
      H(8)=' (1) fluid type (1=air, 2=water),'
      H(9)=' (2) flow coef a (Pa.s/kg),'
      H(10)=' (3) flow exponent b (Pa.(s/kg)^2).'
      H(11)=' '
      H(12)='See ESP-r manual section 3.6.'

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',IW,12)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Flow coefficients.
      write(hold,'(2f12.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3)
  43  CALL EASKS(hold,
     &  'Coef `a` and exponent `b` in dP = a.M + b.M^2 (Pa/(kg/s)^2)?',
     &  '(help for synopsis)',32,' 1. 1. ','comp 25',IER,12)
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
      common/pophelp/h(60)
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 

      CHARACTER LTPCMP*60,CMNAM*12,H*72,prompt*72,hold*32
      logical ok,dok

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=2
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2)='Type 30 is a constant flow rate component which is'
      H(3)='included as a convenient constant flow inducer'
      H(4)='which can be combined with other components.'
      H(5)=' (m = rho . a)'
      H(6)=' '
      H(7)='Supplemental data: '
      H(8)='  (1) fluid type (1=air, 2=water), '
      H(9)='  (2) flow rate in m^3/s.'
      H(10)=' '
      H(11)='The flow rate can be derived from litres/s/person, '
      H(12)='or m^3/hr or air changes per hour based on the volume '
      H(13)='associated with a particular node. '
      H(14)=' '
      H(15)='The flow rate can be modified by the use of a range'
      H(16)='based controller (see control definition facility) or'
      H(17)='by application of an on-off controller. '
      H(18)='For other details see ESP-r manual section 3.6.'

C Input fluid type.
  42  if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',IW,18)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Offer shortcut definition.
      write(prompt,'(3a)') 'Derive flow through ',CMNAM(ICMP),
     &  ' (m^3/s) via:'
      CALL EASKABCD(prompt,' ','direct edit','l/s/person',
     &  'm^3/hr','ach (use volume of node)',IW,18)
      if(iw.eq.1)then

C Flow m^3/s (zero flow allowed).
        VAL=SUPCMP(ICMP,2)
        write(prompt,'(3a)') 'Flow through ',CMNAM(ICMP),' (m^3/s) ?'
   20   CALL EASKR(VAL,prompt,'(help for synopsis)',0.,'W',0.,'-',1.,
     &    'flow in comp',IER,18)
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
     &    '(help for synopsis)',32,' 6.0 1 ','l/s/p',IER,18)
        K=0
        CALL EGETWR(hold,K,VAL2,0.1,99.,'W','l/s/p',IER)
        CALL EGETWR(hold,K,VAL3,0.1,999.,'W','number people',IER)
        IF(IER.NE.0) GOTO 43

        VAL= (val2/1000.0) * val3
        write(prompt,'(3a,F8.5,a)') ' The flow through ',CMNAM(ICMP),
     &  ' (m^3/s) is now ',VAL,'.'
        dok=.true.
        call askok(prompt,' Is this ok? ',OK,dok,18)
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
     &    'flow m3/hr in comp',IER,18)
        IF(IER.NE.0) GOTO 21
        if(val2.gt.0.0) VAL = (val2/3600.0)
        write(prompt,'(3a,F8.5,a)') ' The flow through ',CMNAM(ICMP),
     &  ' (m^3/s) is now ',VAL,'.'
        dok=.true.
        call askok(prompt,' Is this ok? ',OK,dok,18)
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
     &    1.,'flow ach in comp',IER,18)
        IF(IER.NE.0) GOTO 22
        VAL = (SUPNOD(IV,2) * VAL2)/3600.0
        write(prompt,'(3a,F8.5,a)') ' The flow through ',CMNAM(ICMP),
     &  ' (m^3/s) is now ',VAL,'.'
        dok=.true.
        call askok(prompt,' Is this ok? ',OK,dok,18)
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
      common/pophelp/h(60)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 

      CHARACTER LTPCMP*60,CMNAM*12,prompt*48,H*72

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=2
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2)='Type 35 is a constant flow rate component which is'
      H(3)='included as a convenient constant flow inducer which'
      H(4)='can be combined with other components.'
      H(5)='  (m = a)'
      H(6)=' '
      H(7)='Supplemental data: '
      H(8)='  (1) fluid type (1=air, 2=water), '
      H(9)='  (2) flow rate in kg/s.'
      H(10)=' '
      H(11)='The flow rate can be modified by the use of a range'
      H(12)='based controller (see control definition facility) or'
      H(13)='by application of an on-off controller. '
      H(14)='For other details see ESP-r manual section 3.6.'

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',IW,14)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Flow m^3/s (zero flow allowed).
      VAL=SUPCMP(ICMP,2)
      write(prompt,'(3a)') 'Flow through ',CMNAM(ICMP),'(kg/s) ?'
   20 CALL EASKR(VAL,prompt,'(help for synopsis)',0.,'W',0.,'-',1.,
     &  'flow in comp',IER,14)
      IF(IER.NE.0) GOTO 20
      SUPCMP(ICMP,2)=VAL

      RETURN
      END

C ***************** MF040I 
C Fluid flow component supplementary data input or check for
C common orifice flow component (m = rho Cd A (2/rho dP)**.5)
      SUBROUTINE MF040I(ICMP,IER)

#include "net_flow.h"
      common/pophelp/h(60)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 

      CHARACTER LTPCMP*60,CMNAM*12,H*72,hold*32,prompt*48

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=3
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2)='Type 40 is a common orifice flow component which is a'
      H(3)='basic expression for turbulent flow through relatively'
      H(4)='large openings such as a vent in the case of buildings'
      H(5)='or a pipe restriction in a hydronic network.'
      H(6)=' '
      H(7)='Supplemental data: '
      H(8)=' (1) fluid type (1=air, 2=water), '
      H(9)=' (2) opening area (m^2), '
      H(10)=' (3) discharge factor (-).'
      H(11)=' '
      H(12)='The opening area can be modified by the use of a range'
      H(13)='based controller (see control definition facility) or'
      H(14)='by application of an on-off controller. '
      H(15)='For other details see ESP-r manual section 3.6.'

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',IW,15)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Opening area and discharge factor.
      write(hold,'(2f12.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3)
  43  CALL EASKS(hold,' Opening area (m^2) and discharge factor (-)?',
     &  '(help for synopsis)',32,' 1. 0.63 ','comp 40',IER,15)
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
      common/pophelp/h(60)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 

      CHARACTER LTPCMP*60,CMNAM*12,H*72,hold*32,prompt*48

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=3
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2)='Type 50 is a laminar pipe flow component which is a'
      H(3)='basic expression for laminar flow through relatively'
      H(4)='long flow paths.  A warning is given if the '
      H(5)='Reynolds-number exceeds 2320 during simulation.'
      H(6)='  '
      H(7)=' (m = rho (dP/(8 mu L)) pi R**4) '
      H(8)='  '
      H(9)='Supplemental data: '
      H(10)=' (1) fluid type (1=air, 2=water), , '
      H(11)=' (2) path length (m),'
      H(12)=' (3) opening radius (m).'
      H(13)=' '
      H(14)='See ESP-r manual section 3.6.'

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',IW,14)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Length of flow path (m) and radius of opening (m).
      write(hold,'(2f12.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3)
  43  CALL EASKS(hold,' Length of flow path (m) & opening radius (m)?',
     &  '(help for synopsis)',32,' 1. 0.025 ','comp 50',IER,14)
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
      common/pophelp/h(60)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)

      CHARACTER LTPCMP*60,CMNAM*12,H*72,prompt*48

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=2
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2)='Type 110 is an air flow opening (type 40) with the'
      H(3)='fluid type set to air and a discharge factor of 0.65.'
      H(4)=' '
      H(5)='Supplemental data: opening area(m)'
      H(6)=' (1) fluid type (air only)'
      H(7)=' (2) opening area (m).'
      H(8)=' '
      H(9)='The opening area can be modified by the use of a range'
      H(10)='based controller (see control definition facility) or'
      H(11)='by application of an on-off controller. '
      H(12)='For other details see ESP-r manual section 3.6.'

C Set fluid type.
      SUPCMP(ICMP,1)=1.

C Opening area (m^2).
      VAL=SUPCMP(ICMP,2)
      write(prompt,'(3a)')' Opening area of ',CMNAM(ICMP),' (m^2) ?'
   20 CALL EASKR(VAL,'(help for synopsis)',prompt,0.,'F',0.,'-',1.,
     &  'opening area',IER,12)
      IF(IER.NE.0) GOTO 20
      SUPCMP(ICMP,2)=VAL

      RETURN
      END

C ******************* MF120I 
C Fluid flow component supplementary data input or check for
C specific air flow crack component.
      SUBROUTINE MF120I(ICMP,IER)

#include "net_flow.h"
      common/pophelp/h(60)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)

      CHARACTER LTPCMP*60,CMNAM*12,H*72,hold*32

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=3
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2)='Type 120 is an air flow crack component.'
      H(3)=' '
      H(4)='Supplemental data:'
      H(5)=' (1) fluid type (air only)' 
      H(6)=' (2) crack width (mm),'
      H(7)=' (3) crack length(m)'
      H(8)=' '
      H(9)='See ESP-r manual section 3.6.'

C Set fluid type.
      SUPCMP(ICMP,1)=1.

C Crack width (mm) and length (m).
      write(hold,'(2f12.5)')SUPCMP(ICMP,2)*1000.0,SUPCMP(ICMP,3)
  43  CALL EASKS(hold,' Crack width (mm) and length (m) ?',
     &  '(help for synopsis)',32,' 5. 1.0 ','comp 120',IER,9)
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
      common/pophelp/h(60)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)

      CHARACTER LTPCMP*60,CMNAM*12,H*72,hold*48

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=5
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2)='Type 130 is an air flow door component which is '
      H(3)='applicable to vertical opening where bi-directional'
      H(4)='flow may exist. Based on an expression by Cockroft.'
      H(5)=' '
      H(6) ='Supplemental data:'
      H(7) =' (1) fluid type (air only)'
      H(8) =' (2) door width (m)'
      H(9) =' (3) door height (m), '
      H(10)=' (4) vertical distance (m) between base of door and'
      H(11)='     adjacent air nodes (nodes at each side should be '
      H(12)='     at the sameheight). If this component type is used'
      H(13)='     in several locations with different vertical heights'
      H(14)='     you should create multiple door components.'
      H(15)=' (5) discharge factor (-) typically 0.6 but can be smaller'
      H(16)='     to represent a door that is partially open.'
      H(17)=' '
      H(18)='See ESP-r manual section 3.6.'

C Set fluid type.
      SUPCMP(ICMP,1)=1.

C Door width, height, reference ht (m) & discharge factor.
      write(hold,'(4f11.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3),
     &  SUPCMP(ICMP,4),SUPCMP(ICMP,5)
  43  CALL EASKS(hold,
     &'Door width, height, adj node ht above base, & discharge factor?',
     &'(help for synopsis)',48,' 0.8  2.1  1.5 0.61 ','comp 130',IER,18)
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
      common/pophelp/h(60)
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 

      CHARACTER LTPCMP*60,CMNAM*12,H*72,hold*48,prompt*48
      logical close

C Set number of supplementary data items.
      PI=4.*ATAN(1.)
      IER=0
      ISDCMP(ICMP)=6
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2)='Type 210 is a general flow conduit component which'
      H(3)='takes into account frictional and dynamic losses'
      H(4)='within a duct or pipe assuming a uniform x-section,'
      H(5)='no pressure gain from a pump or fan and steady-state'
      H(6)='conditions.  This type is widely supported in the'
      H(7)='literature and can be combined with component types'
      H(8)='220, 230, 240 & 250 to form complex networks.'
      H(9)=' '
      H(9)='Supplemental data:'
      H(10)=' fluid type (1=air or 2=water),'
      H(11)=' conduit hydraulic diam (m) which is:'
      H(12)='   (4*x-section area)/perimeter of conduit'
      H(13)=' x-section area (m^2)'
      H(14)=' conduit length (m), absolute wall roughness(mm),'
      H(15)=' sum of local dynamic loss factors (suggest looking'
      H(16)='   this up in a handbook)'
      H(17)='See ESP-r manual section 3.6.'

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',IW,17)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Hydraulic diameter, cross-sectional area, length, roughness
      write(hold,'(4f11.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3),
     &  SUPCMP(ICMP,4),SUPCMP(ICMP,5)*1000.
  43  CALL EASKS(hold,
     & 'Hydraulic diam, x-sectional area, length (m), roughness (mm)?',
     &  '(help for synopsis)',48,' 1.0  1.0  1.5 0.61 ','comp 130',
     &  IER,17)
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
     &  0.,'F',0.,'-',1.,'dynamic loss fac',IER,17)
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
      common/pophelp/h(60)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 

      CHARACTER LTPCMP*60,CMNAM*12
      character H*72,outs*124,hold*48,prompt*48
      logical close

C Set number of supplementary data items.
      PI=4.*ATAN(1.)
      IER=0
      ISDCMP(ICMP)=6
      ISDCNN(ICMP)=0

C Show short description of flow component.
      WRITE(outs,'(1X,A60)') LTPCMP(ICMP)
      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2)= 'Type 211 represents roof top ventilation cowl which is'
      H(3)= 'based on experimental data and which takes into account'
      H(4)= 'head losses and wind effects. '
      H(5)= ' '
      H(6)= 'The method is based on the work of Armando Pinto and '
      H(7)= 'implements experiments reported in: '
      H(8)= ' Gonzalez M.A. On the aerodynamics of natural ventilators'
      H(9)= ' London. Pergamon Press, 1984, Building and environment.'
      H(10)=' Vol 19 No 3.'
      H(11)=' '
      H(12)='Please read the paper if you want to understand the types'
      H(13)='of cowls that this component can represent. '
      H(14)='The paper mentions 7 different cowels models and Table 1'
      H(15)='tgives several of the coefficients:'
      H(16)=' Model 1 coef B between 0.72 and 1.28 by wind direction'
      H(17)='         coef n between 0.51 and 0.26 by wind direction'
      H(18)=' Model 2 coef B between 0.37 and 0.80 by wind direction'
      H(19)='         coef n between 0.31 and 0.40 bt wind direction'
      H(20)=' Model 3 coef B 0.25  coef n 0.16'
      H(21)=' Model 4 coef B 0.12  coef n 0.17'
      H(22)=' Model 5 coef B 0.37  coef n 0.10'
      H(23)=' Model 6 coef B 0.21  coef n 0.19'
      H(24)=' Model 7 coef B 0.28  coef n 0.45'
      H(25)=' '
      H(26)=' Supplemental data: '
      H(27)='  - fluid type (1=air)'
      H(27)='  - Cowl hydraulic diameter (m)'
      H(27)='  - local dynamic loss factors (-)'
      H(27)='  - coefficient of cowl B (see above)'
      H(27)='  - coefficient of cowl n (see above)'
      H(28)='   Wind effect '
      H(29)='     0 no wind effect '
      H(30)='     1 include wind effect '
      CALL PHELPD('comp 211',30,'-',0,0,IER)

C Set fluid type.
      SUPCMP(ICMP,1)=1.0
     
C DATA OF COWL
      write(hold,'(4f11.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3),
     &  SUPCMP(ICMP,4),SUPCMP(ICMP,5)
  44  CALL EASKS(hold,
     & 'Hydrolic Diameter (m), K head loss, coef B, coef n (see help)?',
     & ' ',48,' 1.0  1.0  1.5 0.61 ','comp 211',IER,30)
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
   20 CALL EASKR(VAL,' ',prompt,0.,'W',0.,'-',1.,'flow in comp',IER,14)
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
      common/pophelp/h(60)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      CHARACTER LTPCMP*60,CMNAM*12
      character H*72,outs*124
      logical OK,dok,close

C Synopsis of flow component.
      WRITE(outs,'(1X,A60)') LTPCMP(ICMP)
      call edisp(iuout,outs)
      dok=.true.
      H(1)='Details of this flow component are available if you'
      H(2)='answer yes. '
      CALL ASKOK(outs,'Synopsis of this component type?',OK,dok,2)

      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2)='Type 220 is a variant of a general flow conduit which'
      H(3)='ends in a converging 3-leg junction.  It represents'
      H(4)='only half of a junction and the branch must be rep-'
      H(5)='resented by a similar component.  It is well supported'
      H(6)='in the literature.  Supplemental data:'
      H(7)=' '
      H(8) =' fluid type, conduit hydraulic diam, x-section area'
      H(9) =' conduit length, wall roughness, sum of local dynamic'
      H(10)=' loss factors, x-sectional area of common junction '
      H(11)=' section, Ccp coefficients a0 - a5 in:'
      H(12)=' '
      H(13)='    Ccp = a0+a1.X+a2.X.X+a3.Y+a4.Y.Y+a5.X.Y  '
      H(14)='where X is the ratio between the volume flow rate in'
      H(15)='this conduit and the common section, respectively and'
      H(16)='Y is the ratio between the volume flow rate in the '
      H(17)='other entrance of the junction and the common. '
      H(18)=' '
      H(19)='See ESP-r manual section 3.6.'
      if(OK) CALL PHELPD('comp 220',19,'-',0,0,IER)

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
     &  -0.0001,'F',0.,'-',0.,'x section area',IER,19)
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
        CALL EASKR(VAL,outs,' ? ',0.,'-',0.,'-',0.,'Cf a0',IER,19)
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
      common/pophelp/h(60)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      CHARACTER LTPCMP*60,CMNAM*12
      character H*72,outs*124
      logical OK,dok,close

C Show short description of flow component.
      IER=0
      dok=.true.
      WRITE(outs,'(1X,A60)') LTPCMP(ICMP)
      H(1)='Details of this flow component are available if you'
      H(2)='answer yes. '
      CALL ASKOK(outs,'Synopsis of this component type?',OK,dok,2)

      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2)='Type 230 is a variant of a general flow conduit which'
      H(3)='ends in a diverging 3-leg junction. '
      H(4)=' '
      H(5)='Supplemental data:'
      H(6)=' fluid type (1=air or 2=water),'
      H(7)=' conduit hydraulic diam (m) which is:'
      H(8)='   (4*x-section area)/perimeter of conduit'
      H(9)=' x-section area (m^2)'
      H(10)=' conduit length (m), absolute wall roughness(mm), '
      H(11)=' sum of local dynamic loss factors, '
      H(12)=' x-sectional area of common junction section,'
      H(13)=' '
      H(14)=' Ccp coefficients a0 - a5 in:'
      H(15)='    Ccp = a0+a1.X+a2.X.X+a3.Y+a4.Y.Y+a5.X.Y  '
      H(16)='where X is the ratio between the volume flow rate in'
      H(17)='this conduit and the common section, respectively and'
      H(18)='Y is the ratio between the volume flow rate in the '
      H(19)='other entrance of the junction and the common. '
      H(20)=' '
      H(21)='See discussion of type 210 & 220 as well as'
      H(22)='the ESP-r manual section 3.6.'
      if(OK) CALL PHELPD('comp 230',22,'-',0,0,IER)

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
     &  -0.0001,'F',0.,'-',0.,'x section area',IER,22)
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
     &    0.,'-',0.,'-',0.,'Coef a0',IER,22)
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
      common/pophelp/h(60)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      CHARACTER LTPCMP*60,CMNAM*12
      character H*72,outs*124
      logical OK,dok,close

C Set/check number of supplementary data items.
      IER=0

C Show short description of flow component.
      dok=.true.
      WRITE(outs,'(1X,A60)') LTPCMP(ICMP)
      H(1)='Details of this flow component are available if you'
      H(2)='answer yes. '
      CALL ASKOK(outs,'Synopsis of this component type?',OK,dok,2)

      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2)='Type 240 is a variant of a general flow conduit which'
      H(3)='ends in a converging 4-leg junction. '
      H(4)=' '
      H(5)='Supplemental data:'
      H(6)=' fluid type (1=air or 2=water),'
      H(7)=' conduit hydraulic diam (m) which is:'
      H(8)='   (4*x-section area)/perimeter of conduit'
      H(9)=' x-section area (m^2)'
      H(10)=' conduit length (m), absolute wall roughness(mm),'
      H(11)=' sum of local dynamic loss factors (suggest looking'
      H(12)='   this up in a handbook)'
      H(13)=' x-sectional area of common junction section'
      H(14)=' '
      H(15)=' Ccp coefficients a0 - a9 in:'
      H(16)='    Ccp = a0+a1.X+a2.X.X+a3.Y+a4.Y.Y+a5.X.Y  '
      H(17)='            +a6.Z+a7.Z.Z+a8.X.Z+a9.Y.Z'
      H(18)='where X is the ratio between the volume flow rate in'
      H(19)='this conduit and the common section, respectively and'
      H(20)='Y & Z are the ratios between the volume flow rate in '
      H(21)='the other entrances of the junction and the common. '
      H(22)=' '
      H(23)='See discussion of type 210 & 220 as well as Hensen'
      H(24)='the ESP-r manual section 3.6.'
      if(OK) CALL PHELPD('comp 240',24,'-',0,0,IER)

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
     &    -0.0001,'F',0.,'-',0.,'x section area',IER,24)
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
     &    0.,'-',0.,'-',0.,'Coef a0',IER,24)
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
      common/pophelp/h(60)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      CHARACTER LTPCMP*60,CMNAM*12
      character H*72,outs*124
      logical OK,dok,close

      IER=0

C Show short description of flow component.
      dok=.true.
      WRITE(outs,'(1X,A60)') LTPCMP(ICMP)
      H(1)='Details of this flow component are available if you'
      H(2)='answer yes. '
      CALL ASKOK(outs,'Synopsis of this component type?',OK,dok,2)

      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2)='Type 250 is a variant of a general flow conduit which'
      H(3)='starts with a diverging 4-leg junction. '
      H(4)=' '
      H(5)='Supplemental data:'
      H(7)=' fluid type (1=air or 2=water),'
      H(8)=' conduit hydraulic diam (m) which is:'
      H(9)='   (4*x-section area)/perimeter of conduit'
      H(10)=' x-section area (m^2)'
      H(11)=' conduit length (m), absolute wall roughness(mm),'
      H(12)=' sum of local dynamic loss factors (suggest looking'
      H(13)='   this up in a handbook)'
      H(14)=' x-sectional area of common junction section,'
      H(16)=' '
      H(17)='  Ccp coefficients a0 - a9 in:'
      H(18)='    Ccp = a0+a1.X+a2.X.X+a3.Y+a4.Y.Y+a5.X.Y  '
      H(19)='            +a6.Z+a7.Z.Z+a8.X.Z+a9.Y.Z'
      H(20)='where X is the ratio between the volume flow rate in'
      H(21)='this conduit and the common section, respectively and'
      H(22)='Y & Z are the ratios between the volume flow rate in '
      H(23)='the other entrances of the junction and the common. '
      H(24)=' '
      H(25)='See discussion of type 210 & 220 as well as'
      H(26)='the ESP-r manual section 3.6.'
      if(OK) CALL PHELPD('comp 250',26,'-',0,0,IER)

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
     &  -0.0001,'F',0.,'-',0.,'x section area',IER,26)
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
     &    0.,'-',0.,'-',0.,'Coef a0',IER,26)
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
      common/pophelp/h(60)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 

C Maxp is max number of points on performance curve and ideg is one
C more than degree of curve (maxp>=ideg)
      parameter (maxp=20)
      parameter (ideg=4)

      CHARACTER LTPCMP*60,CMNAM*12
      character H*72,outs*124,hold*48,prompt*48
      logical OK,dok,closea,closeb,closec,SINGUL
      doubleprecision xval(maxp),yval(maxp),RIHAS(IDEG)

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=7
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2) ='Type 310 is a general flow inducer which approximates '
      H(3) ='a pump or fan by a performance curve (cubic polynomial)'
      H(4) ='which relates the total pressure rise to the volume '
      H(5) ='flow rate for a given fan or pump speed and fluid'
      H(6) ='density.'
      H(7) =' '
      H(8) =' (dP = SUM (i=0 to 3) ai.(m/ro)^i; which is '
      H(9) ='               only valid for q,min <= m/ro <= q,max )'
      H(10) ='Supplemental data:'
      H(11)=' (1) fluid type (1=air, 2=water) '
      H(12)=' (2,3) volume flow rate lower and upper limits (m^3/s),'
      H(13)='Either:'
      H(14)=' (4-7) flow coef a0 (Pa),     a1 (Pa/(m^3/s)),'
      H(15)='           a2 (Pa.(m^3/s)^2), a3 (Pa.(m^3/s)^3)'
      H(16)='Or:'
      H(17)=' four or more coordinates read off a performance curve'
      H(18)=' in which case flow coef are not required'
      H(19)=' '
      H(20)='See ESP-r manual section 3.6.'

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',IW,20)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Volume flow rate lower limit for dP(m) relation.
      VAL=SUPCMP(ICMP,2)
   20 CALL EASKR(VAL,
     &  ' Vol. flow rate lower limit for dP(m) relation (m^3/s) ?',
     &  '(help for synopsis)',0.001,'F',0.,'-',1.,'vol lower limit',
     &  IER,20)
      IF(IER.NE.0) GOTO 20
      SUPCMP(ICMP,2)=VAL

C Volume flow rate upper limit for dP(m) relation.
      VAL=SUPCMP(ICMP,3)
   30 CALL EASKR(VAL,' ',
     &  ' Vol. flow rate upper limit for dP(m) relation (m^3/s) ?',
     &   0.001,'F',0.,'-',1.,'vol upper limit',IER,20)
      IF(IER.NE.0) GOTO 30
      SUPCMP(ICMP,3)=VAL

C Coefficients or performance curve 
      WRITE(prompt,'(a)') 'Is input coefficients or performance curve?'
      CALL EASKAB(' ',prompt,'coefficients','perf curve',IIW,20)
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
     &    ' ',48,' 0.1  0.1  0.1  0.1 ','comp 310 coeff',IER,20)
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
 71     H(1)=' Coordinates entered should be over the '
        H(2)=' entire range of the performance curve. '
        H(3)=' (Not just the preferred operating range)'
        H(4)=' Ideally values for near max and near min'
        H(5)=' flow should also be used.'
        H(6)=' '
        H(7)='Cancel option not yet implemented. '
        CALL PHELPD('curve fit',5,'-',0,0,IER)
 88     CALL EASKI(IVAL,
     &    'How many points to be read off performance curve?',
     &    '(note: CANCEL option not implemented)',
     &    4,'F',maxp,'F',4,'points read off',IER,7)
        DO 53 IIVAL=1,IVAL
          WRITE(HOLD,'(2F11.5)')XVAL(IIVAL),YVAL(IIVAL)
 63       CALL EASKS(HOLD,
     &      ' Enter Volume flow rate (m^3/s) and pressure (Pa)',' ',24,
     &      ' 0.1 0.1 ','comp 310 perf curv',IER,5)
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
        h(1)='Chance to correct the data if incorrect. '
        CALL ASKOK(' ',' Accept values?  ',OK,dok,1)
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
        h(1)='Chance to adjust the values you edited.'
        CALL ASKOK(' ',' Accept this?  ',OK,dok,1)
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
      common/pophelp/h(60)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 
      CHARACTER LTPCMP*60,CMNAM*12,prompt*48,H*72

C Set number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=7
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2) ='Type 410 is a general flow corrector or variable '
      H(3) ='flow resistance such as a valve or damper. Actuation'
      H(4) ='of this component requires a configuration control '
      H(5) ='file otherwise it is assumed the valve (or damper) is'
      H(6) ='open.'
      H(7) =' '
      H(8) ='Supplemental data: '
      H(9) =' (1) fluid type (1=air, 2=water),   choice of'
      H(10)=' (2) fluid density rho 0 (kg/m^3),'
      H(11)=' (3) presure drop dP0 (Pa),'
      H(12)=' (4) linear (1) or logarithmic (2)position dependency,'
      H(13)=' (5) volume flow when fully open  k,vs (m^3/s), '
      H(14)=' (6) theoretical flow when closed, k,v0 (%)'
      H(15)=' (7) lower validity limit k,vr (%).'
      H(16) =' '
      H(17) ='There is a lengthy discussion of this in the '
      H(18) ='ESP-r manual section 3.6.'

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',IW,18)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Standard density rho 0 (kg/m^3).
      VAL=SUPCMP(ICMP,2)
   20 CALL EASKR(VAL,
     &  ' Density of fluid used for `measuring` k,vs (kg/m^3)',
     &  ' (for water this is usually 1000 kg/m^3) ? ',
     &  0.,'F',0.,'-',1000.,'fluid density',IER,15)
      IF(IER.NE.0) GOTO 20
      SUPCMP(ICMP,2)=VAL

C Standard pressure difference dP0 (Pa).
      VAL=SUPCMP(ICMP,3)
   30 CALL EASKR(VAL,
     &  ' Pressure difference used for `measuring` k,vs (Pa)',
     &  ' (for water this is usually 100000 Pa) ? ',
     &  0.,'F',0.,'-',100000.,'pres diff',IER,18)
      IF(IER.NE.0) GOTO 30
      SUPCMP(ICMP,3)=VAL

C Characteristic index (1 = linear, 2 = logartihmic)
      CALL EASKAB(' ',' Characteristic index: ',
     &            'linear','logartihmic',IW,18)
      SUPCMP(ICMP,4)=REAL(IW)

C Volume flow when fully open k,vs (m^3/s)
      VAL=SUPCMP(ICMP,5)
   50 CALL EASKR(VAL,' ',
     &  '  Volume flow when fully open: k,vs (m^3/s) ? ',
     &  0.,'F',0.,'-',1.,'vol flow open',IER,18)
      IF(IER.NE.0) GOTO 50
      SUPCMP(ICMP,5)=VAL

C Theoretical flow when closed, k,v0 (%).
      VAL=SUPCMP(ICMP,6)
   60 CALL EASKR(VAL,' ',
     &  ' Theoretical flow when closed(ie H/H100=0%): k,v0 (%) ? ',
     &   0.,'F',100.,'F',0.,'vol flow close',IER,18)
      IF(IER.NE.0) GOTO 60
      SUPCMP(ICMP,6)=VAL

C Lower validity limit for characteristic, k,vr (%).
      VAL=SUPCMP(ICMP,7)
   70 CALL EASKR(VAL,' ',
     &  ' Lower validity limit for characteristic: k,vr (%) ? ',
     &  0.,'F',100.,'F',0.,'lower val limit',IER,18)
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
      common/pophelp/h(60)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 
      CHARACTER LTPCMP*60,CMNAM*12,H*72,hold*48,prompt*48

C Set/check number of supplementary data items.
      IER=0
      ISDCMP(ICMP)=6
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2) ='Type 420 is a flow corrector or variable flow resis-'
      H(3) ='tance such as a valve or damper which is represented'
      H(4) ='as a polynomial. This approach is as if it were a'
      H(5) ='conduit with local dynamic losses dependent on the '
      H(6) ='correctors relative position. For actuation in'
      H(7) ='this component the configuration control file must be'
      H(8) ='used together otherwise the valve (or damper) is kept'
      H(9) ='open during the whole simulation.'
      H(10) =' '
      H(11) ='Supplemental data: '
      H(12) =' (1) fluid type (air|water), , polynomial '
      H(13) =' (2) cross-sectional area (m^2)'
      H(14) =' (3-6) polynomial coefficients a0 - a3 (-) in:'
      H(15) =' '
      H(16)='    C = a0+a1.(H/H100)+a2.(H/H100)^2+a3.(H/H100)^3'
      H(17)=' where H/H100 is the relative valve/damper position'
      H(18)=' (ie. valve stem displacement of damper blade angle),'

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',IW,18)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C area of cross-section containing flow corrector
      VAL=SUPCMP(ICMP,2)
   20 CALL EASKR(VAL,
     &  ' Area of cross-section containing flow corrector (m^2) ?',
     &  '(help for synopsis)',0.0001,'F',0.,'-',1.,'x section area ',
     &  IER,18)
      IF(IER.NE.0) GOTO 20
      SUPCMP(ICMP,2)=VAL

C Coefficients of C = fie(H/H100) relation.
      write(hold,'(4f11.5)')SUPCMP(ICMP,3),SUPCMP(ICMP,4),
     &  SUPCMP(ICMP,5),SUPCMP(ICMP,6)
  43  CALL EASKS(hold,
     &'Coef a0 to a3 in a0+a1.(H/H100)+a2.(H/H100)^2+a3.(H/H100)^3 ?',
     &  ' ',48,' 0.1  0.1  0.1  0.1 ','comp 420',IER,18)
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
      common/pophelp/h(60)
      COMMON/MFLOW8/CMNAM(MCMP),LTPCMP(MCMP)
      COMMON/MFLOW9/ITPCMP(MCMP),ISDCMP(MCMP),ISDCNN(MCMP),
     &              SUPCMP(MCMP,MCMS)
      COMMON/MFLOWIT/fndegc,imix 
      CHARACTER LTPCMP*60,CMNAM*12,H*72,hold*32,prompt*48

C Set number of supplementary data items
      IER=0
      ISDCMP(ICMP)=3
      ISDCNN(ICMP)=0

C Synopsis of flow component.
      WRITE(H(1),'(A)') LTPCMP(ICMP)
      H(2) ='Type 460 is a fixed flow rate device, which unlike'
      H(3) ='types 30 or 35  responds to a wide range of signals'
      H(4) ='which are defined by the user.  A typical use of this'
      H(6) ='component is an an early design stage substitute for'
      H(5) ='a fan or pump in order to characterise flow rates'
      H(6) ='before specifying a specific device. For actuation in'
      H(7) ='this component the configuration control file must be'
      H(8) ='used together otherwise the flow rate will be equal'
      H(9) ='to the value above setpoint.'
      H(10) =' '
      H(11) ='Supplemental data: '
      H(12) =' (1) fluid type (air|water),'
      H(13) =' (2) flow (m^3/s) below setpoint, '  
      H(14) =' (3) flow (m^3/s) above setpoint.'  
      H(15)=' '
      H(16)='See ESP-r manual section 3.6.'

C Input fluid type.
      if(imix.eq.1)then
        SUPCMP(ICMP,1)=1.0
      elseif(imix.eq.2)then
        SUPCMP(ICMP,1)=2.0
      else
        write(prompt,'(3a)') 'Fluid through ',CMNAM(ICMP),':'
        CALL EASKAB(prompt,'(help for synopsis)','air','water',IW,16)
        SUPCMP(ICMP,1)=REAL(IW)
      endif

C Flow rate when S <  Ssp (m^3/s).
      write(hold,'(2f11.5)')SUPCMP(ICMP,2),SUPCMP(ICMP,3)
  43  CALL EASKS(hold,' Flows when S < Ssp and S > Ssp (m3/s) ?',
     &  '(help for synopsis)',32,' 0.1 0.2 ','comp 460',IER,16)
      K=0
      CALL EGETWR(hold,K,VAL2,0.,0.,'-','flow <ssp',IER)
      CALL EGETWR(hold,K,VAL3,0.,0.,'-','flow >ssp',IER)
      IF(IER.NE.0) GOTO 43
      SUPCMP(ICMP,2)=VAL2
      SUPCMP(ICMP,3)=VAL3

      RETURN
      END
