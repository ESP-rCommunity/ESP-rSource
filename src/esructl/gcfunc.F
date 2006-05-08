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
C     MZGCTL: global control executive.

      SUBROUTINE MZGCTL

#include "building.h"
#include "control.h"


      COMMON/OUTIN/IUOUT,IUIN
      COMMON/GCLSOL/igf,IDTYPG,IPERG

      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its

      common/btime/btimep,btimef

      common/gctl/ngf,igsn(mgl,4),igan(mgl,3),ngcdt(mgl),
     &       igcdv(mgl,mgcdt,2),ngcdp(mgl,mgcdt),tgcps(mgl,mgcdt,mgcdp),
     &       igctyp(mgl,mgcdt,mgcdp),igclaw(mgl,mgcdt,mgcdp),
     &       gmiscd(mgl,mgcdt,mgcdp,misc)

      character outs*124

C Return if no global control function active
      IF(ngf.EQ.0) GOTO 999

C Determine year day number of present day
      iday=idyp
      if(ihrp.eq.24)iday=idyf

      DO 100 IGL=1,ngf
      igf=IGL
C Set up day type and period pointers
      NDAYT=NGCDT(igf)

C If NDAYT=0 set data to weekday, saturday and sunday format
      IF(NDAYT.EQ.0) THEN
         IDTYPG=1
         IF(IDWP.EQ.6) IDTYPG=2
         IF(IDWP.EQ.7) IDTYPG=3
      ELSE
         DO 10 IDTYPG=1,NDAYT
         IDS=IGCDV(igf,IDTYPG,1)
         IDF=IGCDV(igf,IDTYPG,2)
         IF(IDAY.GE.IDS.AND.IDAY.LE.IDF) GOTO 20
   10    CONTINUE
         WRITE(outs,*) ' MZGCTL: global control '
         call edisp(iuout,outs)
         WRITE(outs,*) '         no valid day type for year-day ',IDAY
         call edisp(iuout,outs)
         call edisp(iuout,
     &          ' MZGCTL: cannot locate appropriate day type')
         CALL EPAGEND
         STOP
      END IF

C Check number of periods in each day and the start and finish times
   20 NDAYPG=NGCDP(igf,IDTYPG)
      IF(NDAYPG.EQ.0) STOP ' MZGCTL: no day-periods defined'
      DO 22 IDAYG=1,NDAYPG
      IPERG=IDAYG
      TPS=TGCPS(igf,IDTYPG,IDAYG)
      IF(IDAYG.LT.NDAYPG) THEN
         TPF=TGCPS(igf,IDTYPG,IDAYG+1)
      ELSE
         TPF=24.
      END IF

      IF(btimef.GT.TPS.AND.btimef.LE.TPF) GOTO 30
   22 CONTINUE
      call edisp(iuout,' MZGCTL: cannot locate appropriate day-period')
      CALL EPAGEND
      STOP

C Valid period established; now invoke appropriate control law routine
C Global control law 1: Global ON/OFF Controller.
   30 IF(IGCLAW(igf,IDTYPG,IPERG).EQ.1)THEN
         CALL GCL01

C Global control law 2: Global Capacity Management Controller.
      ELSE IF(IGCLAW(igf,IDTYPG,IPERG).EQ.2)THEN
         CALL GCL02

C Global control law 3: Global Schedule Controller.
      ELSE IF(IGCLAW(igf,IDTYPG,IPERG).EQ.3)THEN
         CALL GCL03

C Global control law 4: Global Free-float Controller.
      ELSE IF(IGCLAW(igf,IDTYPG,IPERG).EQ.4)THEN
         CALL GCL04
   
C New global controllers inserted here!
      ELSE
        WRITE(outs,*) 'MZGCTL: invalid global control law ',
     &    IGCLAW(igf,IDTYPG,IPERG),' has been referenced ',
     &    igf,IDTYPG,IPERG
        call edisp(iuout,outs)
        CALL EPAGEND
        STOP
      END IF

 100  CONTINUE

 999  RETURN
      END

C ******************** GCFVAR

C GCFVAR determines the sensed condition associated
C with the sensor defined for the global control function.

      SUBROUTINE GCFVAR(GVAR)
#include "control.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)

      COMMON/GCLSOL/igf,IDTYPG,IPERG
      common/gctl/ngf,igsn(mgl,4),igan(mgl,3),ngcdt(mgl),
     &       igcdv(mgl,mgcdt,2),ngcdp(mgl,mgcdt),tgcps(mgl,mgcdt,mgcdp),
     &       igctyp(mgl,mgcdt,mgcdp),igclaw(mgl,mgcdt,mgcdp),
     &       gmiscd(mgl,mgcdt,mgcdp,misc)

      common/bfngr/cfgsd(MCF,6),ibsnv(mcf,3),bfngn
      
      logical bfngn,svcfgr
      
      svcfgr=.false.

C If function genrator referenced and required function
C is `sensed variable function`, then re-set IGSN(igf,?)
      IF(IGSN(igf,1).EQ.-6.AND.IGSN(igf,2).EQ.9)THEN
         IBSNV(ICF,1)=IGSN(igf,1)
         IBSNV(ICF,2)=IGSN(igf,2)
         IBSNV(ICF,3)=IGSN(igf,3)
         IGSN(igf,1)=int(cfgsd(icf,1))
         IGSN(igf,2)=int(cfgsd(icf,2))
         IGSN(igf,3)=int(cfgsd(icf,3))
         SVCFGR=.TRUE.
      ENDIF

      IF(IGSN(igf,1).EQ.0.OR.IGSN(igf,1).EQ.IICOMP)THEN
         GVAR=TNP

      ELSEIF(IGSN(igf,1).GT.0.AND.IGSN(igf,1).NE.IICOMP)THEN
         IZ=IGSN(igf,1)
         IS=IGSN(igf,2)
         IN=IGSN(igf,3)
         IF(IN.EQ.0.AND.IS.EQ.0)GVAR=TFA(IZ)
         IF(IN.EQ.0.AND.IS.GT.0)GVAR=TFS(IZ,IS)
         IF(IN.GT.0.AND.IS.GT.0)GVAR=TFC(IZ,IS,IN)

C Sensor measures plant node state-space variable.
      ELSE IF(IGSN(igf,1).EQ.-1) THEN

C Which is obviously not allowed in case of bld.
         call edisp(iuout,
     &   ' GCFVAR: simulator does not support sensing plt node state.')
         call epwait
         call epagend
         STOP
      ELSEIF(IGSN(igf,1).EQ.-2)THEN
         IZ=IGSN(igf,2)
         CALL MZMIXT(IZ,TMRT,GVAR)

      ELSEIF(IGSN(igf,1).EQ.-3)THEN
C Match flow controller type.
      IGTYPE=IGCTYP(IGF,IDTYPG,IPERG)

C Controller type 0: sensor measures temperature.
        IF(IGTYPE.EQ.0)THEN
           IF(IGSN(igf,2).EQ.0)THEN
              GVAR=TF
           ELSEIF(IGSN(igf,2).EQ.1)THEN
              GVAR=SOLAIR(TF,QFF,QDF)
           ELSE
              GOTO 9998
           ENDIF

C Controller type 35: sensor measures wind speed.
         ELSE IF(IGTYPE.EQ.35)THEN     
           IF(IGSN(igf,2).EQ.2)THEN
              GVAR=VF
           ELSE
              GOTO 9998
           ENDIF
             
C Controller type 36: sensor measures wind direction.
         ELSE IF(IGTYPE.EQ.36)THEN     
           IF(IGSN(igf,2).EQ.3)THEN
              GVAR=DF
           ELSE
              GOTO 9998
           ENDIF

C Controller type 37: sensor measures diffuse horizontal solar rad.
         ELSE IF(IGTYPE.EQ.37)THEN
           IF(IGSN(igf,2).EQ.4)THEN
              GVAR=QFF
           ELSE
              GOTO 9998
           ENDIF

C Controller type 38: sensor measures direct normal solar rad.
         ELSE IF(IGTYPE.EQ.38)THEN
           IF(IGSN(igf,2).EQ.5)THEN
              GVAR=QDF
           ELSE
              GOTO 9998
           ENDIF

C Controller type 39: sensor measures relative humidity outdoor air.
         ELSE IF(IGTYPE.EQ.39)THEN
           IF(IGSN(igf,2).EQ.6)THEN
              GVAR=HF
           ELSE
              GOTO 9998
           ENDIF

C Invalid controller type.
         ELSE
           CALL ELINC(2)
           WRITE(IUOUT,*) ' GCFVAR: function ',ICF,' type ',IGTYPE
           STOP ' GCFVAR: invalid controller type for type -3 sensor'
         ENDIF

C Sensing mass flow component variable.          
      ELSEIF(IGSN(igf,1).EQ.-4)THEN
C Which is currently not active in the case of bld.
         call edisp(iuout,
     &  'GCFVAR: simulator does not support sensing m_flow variable.')
         call epwait
         call epagend
         STOP

      ELSEIF(IGSN(igf,1).EQ.-5)THEN
C Which is currently not supported.
         call edisp(iuout,
     &  'GCFVAR: global control does not support -5 sensor .')
         call epwait
         call epagend
         STOP
      ELSEIF(IGSN(igf,1).EQ.-6)THEN
         IFNC=IGSN(igf,2)
         CALL CFFNGR(IFNC,GVAR)
      ENDIF

      IF(SVCFGR)THEN
         IFUNC=9
         CALL CFFNGR(IFUNC,GVAR)
         IGSN(igf,1)=IBSNV(ICF,1)
         IGSN(igf,2)=IBSNV(ICF,2)
         IGSN(igf,3)=IBSNV(ICF,3)
      ENDIF

      RETURN

9998  CALL ELINC(2)
      WRITE(IUOUT,*) ' GCFVAR: function ',igf,' type ',IGTYPE
      STOP ' GCFVAR: incorrect match of sensor and controller type.'

      END

C ******************** GFMVAR

      SUBROUTINE GFMVAR
#include "control.h"
#include "building.h"

      COMMON/PSTSOL/ICF,IDTYP,IPER,BB1,BB2,BB3,IICOMP,TNP,QFUT,TFUT
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      COMMON/FVALS/TFS(MCOM,MS),QFS(MCOM)
      COMMON/FVALC/TFC(MCOM,MS,MN),QFC(MCOM)
      COMMON/GCMVAR/NGSEN(MGL),IGSMOD(MGL),IGMSN(MGL,MSEN,4),
     &GTAXSN(MCF,MSEN)

      COMMON/GCLSOL/igf,IDTYPG,IPERG

      DO 28 J=1,NGSEN(igf)

        IF(IGMSN(igf,J,1).EQ.0.OR.IGMSN(igf,J,1).EQ.IICOMP)THEN
           GTAXSN(igf,J)=TNP
        ELSEIF(IGMSN(igf,J,1).GT.0.AND.IGMSN(igf,J,1).NE.IICOMP)THEN
           IZ=IGMSN(igf,J,1)
           IS=IGMSN(igf,J,2)
           IN=IGMSN(igf,J,3)
           IF(IN.EQ.0.AND.IS.EQ.0)GTAXSN(igf,J)=TFA(IZ)
           IF(IN.EQ.0.AND.IS.GT.0)GTAXSN(igf,J)=TFS(IZ,IS)
           IF(IN.GT.0.AND.IS.GT.0)GTAXSN(igf,J)=TFC(IZ,IS,IN)
C Sensor measures plant node state-space variable.
        ELSE IF(IGMSN(igf,J,1).EQ.-1) THEN

C Which is obviously not allowed in case of bld.
          call edisp(iuout,
     &   'GCMVAR: simulator does not support sensing plant node state.')
          call epwait
          call epagend
          STOP

        ELSEIF(IGMSN(igf,J,1).EQ.-2)THEN
           IZ=IGMSN(igf,J,2)
           CALL MZMIXT(IZ,TMRT,TCTL)
           GTAXSN(igf,J)=TCTL

        ELSEIF(IGMSN(igf,J,1).EQ.-3)THEN
           IF(IGMSN(igf,J,2).EQ.0)THEN
              GTAXSN(igf,J)=TF
           ELSEIF(IGMSN(igf,J,2).EQ.1)THEN
              GTAXSN(igf,J)=SOLAIR(TF,QFF,QDF)   
           ELSEIF(IGMSN(igf,J,2).EQ.2)THEN
              GTAXSN(igf,J)=VF 
           ELSEIF(IGMSN(igf,J,2).EQ.3)THEN
              GTAXSN(igf,J)=DF
           ELSEIF(IGMSN(igf,J,2).EQ.4)THEN
              GTAXSN(igf,J)=QFF
           ELSEIF(IGMSN(igf,J,2).EQ.5)THEN
              GTAXSN(igf,J)=QDF
           ELSEIF(IGMSN(igf,J,2).EQ.6)THEN
              GTAXSN(igf,J)=HF
           ENDIF

C Sensing mass flow component variable.          
        ELSEIF(IGMSN(igf,J,1).EQ.-4)THEN

C Which is obviously not allowed in case of bld.
          call edisp(iuout,
     & 'GCMVAR: simulator does not support sensing mass flow variable.')
          call epwait
          call epagend
          STOP
        ELSEIF(IGMSN(igf,J,1).EQ.-5)THEN
C Which is not supported..
          call edisp(iuout,
     &    'GCFVAR: global control does not support -5 sensor.')
          call epwait
          call epagend
          STOP
        ELSEIF(IGMSN(igf,J,1).EQ.-6)THEN
          call edisp(iuout,
     &     'GCMVAR: version does not support -6 type AUXILIARY sensor.')
          call epwait
          call epagend
          STOP
        ENDIF
28    CONTINUE

      RETURN
      END

C *******  GCL01 Global On-Off controller.

      SUBROUTINE GCL01
#include "control.h"
#include "building.h"

      common/pstsol/icf,idtyp,iper,bb1,bb2,bb3,iicomp,tnp,qfut,tfut

      COMMON/GCLSOL/igf,IDTYPG,IPERG

      common/bctl/ncf,ibsn(mcf,4),iban(mcf,3),nbcdt(mcf),
     &       ibcdv(mcf,mbcdt,2),nbcdp(mcf,mbcdt),tbcps(mcf,mbcdt,mbcdp),
     &       ibctyp(mcf,mbcdt,mbcdp),ibclaw(mcf,mbcdt,mbcdp),
     &       bmiscd(mcf,mbcdt,mbcdp,misc)

      common/gctl/ngf,igsn(mgl,4),igan(mgl,3),ngcdt(mgl),
     &       igcdv(mgl,mgcdt,2),ngcdp(mgl,mgcdt),tgcps(mgl,mgcdt,mgcdp),
     &       igctyp(mgl,mgcdt,mgcdp),igclaw(mgl,mgcdt,mgcdp),
     &       gmiscd(mgl,mgcdt,mgcdp,misc)

      common/globct/qfutgb(mcf),tfutgb(mcf),tnpgb(mcf)

      common/glbctl/global,glbrwd
      common/sysctl/ipassr

      logical global,glbrwd

      imode=int(gmiscd(igf,IDTYPG,IPERG,2))
      if(imode.eq.1.or.imode.eq.-1)minon=int(gmiscd(igf,IDTYPG,IPERG,3))
      if(imode.eq.2.or.imode.eq.-2)setpt=int(gmiscd(igf,IDTYPG,IPERG,3))
      
      SMALL=1.0E-15

      if(ipassr.eq.1)then
         if(icf.eq.ncf)then
            ion=0
            do 103 i=1,ncf
               if(abs(qfutgb(i)).gt.small)ion=ion+1
 103        continue
         glbrwd=.true.
         endif
      endif

      if(ipassr.eq.2)then
         if(imode.eq.1.or.imode.eq.-1)then
            if((imode.eq.1.and.ion.ge.minon).or.
     &         (imode.eq.-1.and.ion.le.minon))then
                qfut=qfutgb(icf)
                tfut=tfutgb(icf)
            else
                qfut=0.
                tfut=tnpgb(icf)
            endif
         
         elseif(imode.eq.2.or.imode.eq.-2)then
            call gcfvar(gvar)
            if((imode.eq.2.and.gvar.lt.setpt).or.
     &         (imode.eq.-2.and.gvar.gt.setpt))then
                qfut=qfutgb(icf)
                tfut=tfutgb(icf)
            else
                qfut=0.
                tfut=tnpgb(icf)
            endif
         endif
         glbrwd=.false.
      endif

      RETURN
      END

C *******  GCL02 Global Capacity Management controller.

      SUBROUTINE GCL02
#include "control.h"
#include "building.h"

      COMMON/GCLSOL/igf,IDTYPG,IPERG

      common/pstsol/icf,idtyp,iper,bb1,bb2,bb3,iicomp,tnp,qfut,tfut
      common/bctl/ncf,ibsn(mcf,4),iban(mcf,3),nbcdt(mcf),
     &       ibcdv(mcf,mbcdt,2),nbcdp(mcf,mbcdt),tbcps(mcf,mbcdt,mbcdp),
     &       ibctyp(mcf,mbcdt,mbcdp),ibclaw(mcf,mbcdt,mbcdp),
     &       bmiscd(mcf,mbcdt,mbcdp,misc)

      common/gctl/ngf,igsn(mgl,4),igan(mgl,3),ngcdt(mgl),
     &       igcdv(mgl,mgcdt,2),ngcdp(mgl,mgcdt),tgcps(mgl,mgcdt,mgcdp),
     &       igctyp(mgl,mgcdt,mgcdp),igclaw(mgl,mgcdt,mgcdp),
     &       gmiscd(mgl,mgcdt,mgcdp,misc)

      common/globct/qfutgb(mcf),tfutgb(mcf),tnpgb(mcf)

      common/glbctl/global,glbrwd
      common/sysctl/ipassr

      logical global,glbrwd

      qhgcap=gmiscd(igf,IDTYPG,IPERG,2)
      qcgcap=-gmiscd(igf,IDTYPG,IPERG,3)
      nshed=int(gmiscd(igf,IDTYPG,IPERG,4))

      qtot2=0.

      if(ipassr.eq.1)then
       if(icf.eq.ncf)then
         do 203 im=1,ncf
            in=im
            qtot2=qtot2+qfutgb(in)
 203     continue

         if(qtot2.gt.qhgcap.or.qtot2.lt.qcgcap)then
           qlop=qtot2
           n=nshed
           do 205 ii=1,n
              i=ii
              j=int(gmiscd(igf,IDTYPG,IPERG,4+i))              
              qlop=qlop-qfutgb(j)
              qfutgb(j)=0.
              tfutgb(j)=tnpgb(j)
              if(qlop.le.qhgcap.and.qlop.ge.qcgcap)goto 207
 205       continue
 207       continue
         endif
         glbrwd=.true.
       endif
      endif

      if(ipassr.eq.2)then
         qfut=qfutgb(icf)
         tfut=tfutgb(icf)
         glbrwd=.false.
      endif

      RETURN
      END

C *******  GCL03 Global "Sequence On" Controller.

      SUBROUTINE GCL03
#include "control.h"
#include "building.h"

      common/pstsol/icf,idtyp,iper,bb1,bb2,bb3,iicomp,tnp,qfut,tfut

      common/bctl/ncf,ibsn(mcf,4),iban(mcf,3),nbcdt(mcf),
     &       ibcdv(mcf,mbcdt,2),nbcdp(mcf,mbcdt),tbcps(mcf,mbcdt,mbcdp),
     &       ibctyp(mcf,mbcdt,mbcdp),ibclaw(mcf,mbcdt,mbcdp),
     &       bmiscd(mcf,mbcdt,mbcdp,misc)
      common/gctl/ngf,igsn(mgl,4),igan(mgl,3),ngcdt(mgl),
     &       igcdv(mgl,mgcdt,2),ngcdp(mgl,mgcdt),tgcps(mgl,mgcdt,mgcdp),
     &       igctyp(mgl,mgcdt,mgcdp),igclaw(mgl,mgcdt,mgcdp),
     &       gmiscd(mgl,mgcdt,mgcdp,misc)

      COMMON/GCLSOL/igf,IDTYPG,IPERG
      common/globct/qfutgb(mcf),tfutgb(mcf),tnpgb(mcf)

      common/glbctl/global,glbrwd
      common/sysctl/ipassr

      logical global,glbrwd

C nsloop = number of scheduled loops

      nsloop=int(gmiscd(igf,IDTYPG,IPERG,2))
      SMALL=gmiscd(igf,IDTYPG,IPERG,3)

      if(ipassr.eq.1)then
       if(icf.eq.ncf)then
         n=nsloop-1
         do 305 ii=1,n
            i=ii
            j=int(gmiscd(igf,IDTYPG,IPERG,3+i))
            if(abs(qfutgb(j)).lt.small)then
               do 303 kk=4+i,nsloop+3
                  k=kk
                  m=int(gmiscd(igf,IDTYPG,IPERG,k))
                  qfutgb(m)=0.
                  tfutgb(m)=tnpgb(m)
 303           continue
            endif
 305     continue
         glbrwd=.true.
       endif
      endif

      if(ipassr.eq.2)then
         qfut=qfutgb(icf)
         tfut=tfutgb(icf)
         glbrwd=.false.
      endif

      RETURN
      END

C *******  GCL04 Global "Free-float" Controller.

      SUBROUTINE GCL04
#include "control.h"
#include "building.h"

      common/pstsol/icf,idtyp,iper,bb1,bb2,bb3,iicomp,tnp,qfut,tfut

      common/gctl/ngf,igsn(mgl,4),igan(mgl,3),ngcdt(mgl),
     &       igcdv(mgl,mgcdt,2),ngcdp(mgl,mgcdt),tgcps(mgl,mgcdt,mgcdp),
     &       igctyp(mgl,mgcdt,mgcdp),igclaw(mgl,mgcdt,mgcdp),
     &       gmiscd(mgl,mgcdt,mgcdp,misc)

      COMMON/GCLSOL/igf,IDTYPG,IPERG
      common/globct/qfutgb(mcf),tfutgb(mcf),tnpgb(mcf)

      common/glbctl/global,glbrwd
      common/sysctl/ipassr

      logical global,glbrwd
      
      glbrwd=.false.

      RETURN
      END
