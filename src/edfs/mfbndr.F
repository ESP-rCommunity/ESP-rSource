C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

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
C MFBNDR - Principal conflation subroutine for mfs and CFD
C This subroutine is invoked with argument 1,2,3 or 0
C Invoked with 1 it initialises dfs massflows to dry mfs calculated values
C Invoked with 2 it normalises all mass_in and mass_out
C and imposes all airflows as velocity type boundary conditions.
C Invoked with 3 it does a pressure correction to set dfs and mfs
C pressure datum equal. Imposes dfs predicted pressures onto the
C conflated mfs and solves it. It returns with the information if 
C If the previous run values and the present values of massflow rate 
C are within a tolerance.
C NOTE: Dry run mfs is the standard version of mfs as run by bps without
C CFD. Conflated mfs is the new network built up by removing connections
C to the node representative of CFD domain and including nodes
C representative of CFD opening type boundary conditions.
C Invoked with 0 this subroutine deactivates conflated mfs and activates
C dry mfs. 
C For the purposes of mfs-dfs conflation relevant
C airflow nodes are divided into three types: 
C dom_nod = node representative of CFD domain
C opn_nod = node representative of CFD key volume where air is inlet/outlet
C ext_nod = node external to CFD (not necessarily a boundary type node),
C           which is connected to dom_nod in the original flow network
C Principal variables used for mfs-cfd conflation are
C ICFDNOD = mfs node number representative of dom_nod
C ICFDCNN = mfs connection numbers representative of dom_nod connections
C MFNEWN  = mfs node numbers of opn_nod
C NCFDOPNO= CFD key volume number which is to be represented by opn_nod
C           these are held in same order as MFNEWN
C NFNDN   = total number of opn_nod (original mfs nodes are 1 to NNOD-
C           NFNDN and opn_nodes are NNOD-NFNDN+1 to NNOD
C NFNPDN  = as above, per domain
C ********************* MFBNDR *********************
      SUBROUTINE MFBNDR(INTL)
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "cfd.h"

      COMMON/ICFNOD/ICFD,ICP
      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF
      common/outin/iuout,iuin
      COMMON/PVALA/TPA(MCOM),QPA(MCOM)
      COMMON/FVALA/TFA(MCOM),QFA(MCOM)
      common/SSOinit/areaSSO
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      common/KEYVDAT/IVTYPE(MNVLS,MNZ),VOLTemp(MNVLS,MNZ),
     &          VOLHeat(MNVLS,MNZ),IVConfl(MNVLS,MNZ),VOLHum(MNVLS,MNZ),
     &          VOLCO2(MNVLS,MNZ),VOLVel(MNVLS,MNZ),VOLDir(MNVLS,MNZ,2),
     &          VOLArea(MNVLS,MNZ),VOLPres(MNVLS,MNZ),
     &          VOLPol(MCTM,MNVLS,MNZ)

      COMMON/MFLW11/PSTACK(MCNN),HDP(MCNN),HDV(MCNN),HFL(MCNN)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)

      COMMON/CFDMFS1/ICFDNOD(MNZ),ICFDCNN(MNVLS,MNZ)
      COMMON/CFDMFS2/MFNEWN(MNVLS,MNZ),NFNDN,NCFDOPNO(MNVLS,MNZ),
     &               NFNPDN(MNZ)
      COMMON/CFDMFS4/VOLVELOLD(MNVLS,MNZ),VOLTEMPOLD(MNVLS,MNZ),
     1 VOLHUMOLD(MNVLS,MNZ),VOLDIROLD(MNVLS,MNZ,2),IVTYPEOLD(MNVLS,MNZ)
      COMMON/MFCFD/NCNRMV,ICNRMV(MCFND,MNZ),NCNON,ICNON(2*MCFND,MNZ)
      COMMON/ACT/ACTCNN(MCNN)
      COMMON/MFCALC/IPSMOD,MSLVTP

      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW,PSTACK,HDP,HDV,HFL,
     *                 FLWNET
      LOGICAL ACTCNN,FOUND,FIRST,areaSSO
      CHARACTER LAPROB*72
      integer TMPNODN

      INITL=INTL

C Set up small openings, but only if this is the first time
C the CFD domain has been invoked during this simulation.
      IF(.NOT.areaSSO) THEN
        CALL DEFINESSO
      ENDIF

      IF(INITL.EQ.0)GOTO 100
      IF(INITL.EQ.1)GOTO 101
      IF(INITL.EQ.2)GOTO 102
      IF(INITL.EQ.3)GOTO 103
      CALL EDISP(IUOUT,'MFBNDR: Error configuring mfs-cfd conflation')
      RETURN

C Initialise the massflows to/from CFD domain to mfs dry run values      
 101  DO 1976 ICN=1,NCNN
        IF(NODPS(ICN).EQ.ICFDNOD(ICFD).AND.NODNE(ICN).LE.(NNOD-NFNDN))
     &  THEN
          JNOD=NODNE(ICN)
          DO 1977 I=1,NCNN
            DO 1978 N=1,NFNDN
              IF(NODPS(I).EQ.MFNEWN(N,ICFD).AND.NODNE(I).EQ.JNOD)THEN
                FLW1(I)=FLW1(ICN)
                FLW2(I)=FLW2(ICN)
              ENDIF
 1978       CONTINUE
 1977     CONTINUE
        ELSEIF(NODNE(ICN).EQ.ICFDNOD(ICFD).AND.NODPS(ICN)
     &         .LE.(NNOD-NFNDN))THEN
          JNOD=NODPS(ICN)
          DO 1979 I=1,NCNN
            DO 1980 N=1,NFNDN
              IF(NODNE(I).EQ.MFNEWN(N,ICFD).AND.NODPS(I).EQ.JNOD)THEN
                FLW1(I)=FLW1(ICN)
                FLW2(I)=FLW2(ICN)
              ENDIF
 1980       CONTINUE
 1979     CONTINUE
        ENDIF
 1976 CONTINUE
      GOTO 9999

C Calculate connections stack pressure based on previous
C flow direction.
 102  DO 5011 ICNN=1,NCNN
        IDPS=NODPS(ICNN)
        IDNE=NODNE(ICNN)
        IF(IPSMOD.EQ.1) THEN
          IF(FLW1(ICNN).GE.0.0) THEN
            PSTACK(ICNN)=9.81*(RHON(IDPS)*(HNOD(IDPS,3)-HNOD(IDNE,3))
     &                         +HGTNE(ICNN)*(RHON(IDNE)-RHON(IDPS)))
          ELSE
            PSTACK(ICNN)=9.81*(RHON(IDNE)*(HNOD(IDPS,3)-HNOD(IDNE,3))
     &                         +HGTPS(ICNN)*(RHON(IDNE)-RHON(IDPS)))
          ENDIF
        ELSE IF(IPSMOD.EQ.2) THEN
          PSTACK(ICNN)=9.81*0.5*
     &      ((RHON(IDPS)+RHON(IDNE))*(HNOD(IDPS,3)-HNOD(IDNE,3))
     &      +(HGTNE(ICNN)+HGTPS(ICNN))*(RHON(IDNE)-RHON(IDPS)))
        ELSE
          STOP ' MFLWCA: unresolvable error !'
        END IF
 5011 CONTINUE

C Impose massflows to the CFD domain as velocity type boundary condition
      IOPNO=1
c      FLOWIN=0.0
      DO 570 IV=1,NVOL(ICFD)
        IF(IV.EQ.NCFDOPNO(IOPNO,ICFD))THEN

C Get massflow node (opn_nod) associated with this CFD opening          
          INOD=MFNEWN(IOPNO,ICFD)

C Get connection of this node with the node external to CFD domain (JNOD)
C INDEX=-1 means flow is going out of CFD domain and vice versa
          DO 571 ICNN=1,NCNN
            FLWNET=FLW1(ICNN)+FLW2(ICNN)
            IF(NODPS(ICNN).EQ.INOD.AND.NODNE(ICNN).LE.(NNOD-NFNDN)
     &         .AND.NODNE(ICNN).NE.ICFDNOD(ICFD))THEN
              JNOD=NODNE(ICNN)
              IF(FLWNET.GT.0.0)THEN
                INDEX=-1
              ELSEIF(FLWNET.LE.0.0)THEN
                INDEX=1
              ENDIF
              GOTO 1010
            ELSEIF(NODNE(ICNN).EQ.INOD.AND.NODPS(ICNN).LE.(NNOD-NFNDN)
     &             .AND.NODPS(ICNN).NE.ICFDNOD(ICFD))THEN
              JNOD=NODPS(ICNN)
              ICN=ICNN
              IF(FLWNET.GT.0.0)THEN
                INDEX=1
              ELSEIF(FLWNET.LE.0.0)THEN
                INDEX=-1
              ENDIF
              GOTO 1010
            ELSE
              GOTO 571
            ENDIF

C Before changing boundary condition first hold original values
 1010       IVTYPEOLD(IV,ICFD)  = IVTYPE(IV,ICFD)
            VOLVELOLD(IV,ICFD)  = VOLVel(IV,ICFD)
            VOLTEMPOLD(IV,ICFD) = VOLTemp(IV,ICFD)
            VOLHUMOLD(IV,ICFD)  = VOLHum(IV,ICFD)
            VOLDIROLD(IV,ICFD,1)= VOLDir(IV,ICFD,1)
            VOLDIROLD(IV,ICFD,2)= VOLDir(IV,ICFD,2)
  
C Set this boundary condition to velocity depending on flow in/out
C Areas of all opening type boundary conditions have already been set
C in DEFINESSO so there is no need to set VOLArea
            IVTYPE(IV,ICFD)=11
            FLOW=REAL(FLWNET)
            IF(INDEX.EQ.1)THEN
              VOLVel(IV,ICFD)=ABS(FLOW)/VOLArea(IV,ICFD)
c              FLOWIN=FLOWIN+VOLVel(IV,ICFD)
            ELSEIF(INDEX.EQ.-1)THEN
              VOLVel(IV,ICFD)=-ABS(FLOW)/VOLArea(IV,ICFD)
            ENDIF

C Get zone db temp of zone from which air is entering
            FOUND=.FALSE.
            DO 572 IZN=1,MCOM
              IF(ICAAS(IZN).EQ.JNOD)THEN
                TMPRTR=0.5*(TPA(IZN)+TFA(IZN))
                FOUND=.TRUE. 
              ENDIF
 572        CONTINUE

C If zone not found air is entering from ambient
C << Cannot assume this, what about internal known nodes? >>
            IF(.NOT.FOUND)THEN
C Find sending node.
              if (FLOW.gt.0.) then ! net flow NODPS -> NODNE
                TMPNODN=NODPS(ICNN)
              else                 ! net flow NODNE -> NODPS
                TMPNODN=NODNE(ICNN)
              endif
              IF((NDTYP(TMPNODN).EQ.2.AND.NINT(SUPNOD(TMPNODN,2)).EQ.1)
     &          .OR.NDTYP(TMPNODN).EQ.3) then ! external temp
                call gettemp(TMPRTR)
              ELSEIF(ITND(TMPNODN).GT.0) then ! temp from another node
                TMPRTR=TNOD(ITND(TMPNODN))
              ELSE                            ! constant temp
                TMPRTR=TNOD(TMPNODN)
              ENDIF
            ENDIF
            VOLTemp(IV,ICFD)=TMPRTR

C<< To include direction components and humidity
            VOLDir(IV,ICFD,1)=0.0
            VOLDir(IV,ICFD,2)=0.0
            VOLHum(IV,ICFD)=0.0
 571      CONTINUE
          IOPNO=IOPNO+1
        ENDIF
 570  CONTINUE

C Normalise all flowrates so that equal mass flows into and out of CFD
C domain. This is done by totalling all airflow to/from CFD domain 
c (which should total to zero) and distributing any residual over all
c openings.
c      FIRST=.FALSE.
      IX=0
      FLOWCORR=0.0
      DO 580 IV=1,NVOL(ICFD)
        IF(IVTYPE(IV,ICFD).EQ.11)THEN
c          IF(.NOT.FIRST)THEN
c            IFST=IV
c            FIRST=.TRUE.
c          ENDIF
c          FLOWCORR=FLOWCORR+VOLVel(IV,ICFD)*VOLArea(IV,ICFD)
          IX=IX+1
          FLOWCORR=FLOWCORR+VOLVel(IV,ICFD)*VolArea(IV,ICFD)
        ENDIF
 580  CONTINUE
c      VOLVel(IFST,ICFD)=VOLVel(IFST,ICFD)-FLOWCORR/VOLArea(IFST,ICFD)
      if (IX.gt.0) FLOWCORR=FLOWCORR/real(IX)
      DO 581 IV=1,NVOL(ICFD)
        IF(IVTYPE(IV,ICFD).EQ.11)THEN
          VOLVel(IV,ICFD)=VOLVel(IV,ICFD)-FLOWCORR/VOLArea(IV,ICFD)
        ENDIF
 581  CONTINUE

C Update common block variables that are originally updated in NEW2OLD
      CALL NEW2OLD
      GOTO 9999

C Call subroutine to compare CFD and mfs predicted pressures & flowrates
C It returnes with IPK=0 if pressures and flowrates are close or with
C IPK=3 if they are not
 103  IPK=3
      CALL MFSCORR(IPK)
      INTL=IPK
      GOTO 9999

C CFD-mfs conflation has been attempted and hopefully achieved, reset
C both domains to their original settings.
C Activate all connections.
 100  DO 200 ICN=1,MCNN
        ACTCNN(ICN)=.TRUE.
 200  CONTINUE

C Deactivate CFD massflow connections
      DO 201 IC=1,NCONF
        DO 202 N=1,NCNON
          ACTCNN(ICNON(N,IC))=.FALSE.
  202   CONTINUE
  201 CONTINUE

      CALL MFSOLV

C Restore original CFD boundary conditions
      DO 577 IV=1,NVOL(ICFD)
        IF(IVTYPEOLD(IV,ICFD).NE.0)THEN
         IVTYPE(IV,ICFD)  = IVTYPEOLD(IV,ICFD) 
         VOLVel(IV,ICFD)  = VOLVELOLD(IV,ICFD)  
         VOLTemp(IV,ICFD) = VOLTEMPOLD(IV,ICFD)
         VOLHum(IV,ICFD)  = VOLHUMOLD(IV,ICFD) 
         VOLDir(IV,ICFD,1)= VOLDIROLD(IV,ICFD,1) 
         VOLDir(IV,ICFD,2)= VOLDIROLD(IV,ICFD,2)
        ENDIF
 577  CONTINUE
      CALL NEW2OLD

 9999 RETURN
      END

C ********************* MFSCORR *********************
C This subroutine gets pressure as computed by CFD for the different 
C CFD openings. It calculates a pressure correction term to correct CFD
C calculated pressures to mfs pressures. The conflated mfs is then run 
C and if massflows and pressures from this run are within a tolerance of
C the previous run values ICORROK=0 else ICORROK=3 and more mutual 
C cfd-mfs iterations are done.
      SUBROUTINE MFSCORR(ICORROK)
#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "cfd.h"
      COMMON/ICFNOD/ICFD,ICP
      common/ndcfd/ncfdnd,icfdnd(MNZ),NCONF
      common/outin/iuout,iuin
     
      COMMON/NDMAP/NOPEN(MNZ),MFNODE(MCFND,MNZ),IOPENi(MCFND,MNZ),
     &             IOPENf(MCFND,MNZ),JOPENi(MCFND,MNZ),
     &             JOPENf(MCFND,MNZ),KOPENi(MCFND,MNZ),
     &             KOPENf(MCFND,MNZ),FIXM(MCFND,MNZ),
     &             FIXT(MCFND,MNZ),FIXC(MCFND,MNZ),
     &             FIXK(MCFND,MNZ),FIXE(MCFND,MNZ),
     &             IWOPEN(MCFND,MNZ),ICFDCN(MCFND,MNZ),
     &             ICNACT(MCFND,MNZ),IVOLN(MCFND,MNZ)
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      common/KEYVOLS/NVOL(MNZ),IVOLF(MNVLS,MNZ),IVCELLS(MNVLS,MNZ,2),
     &               JVCELLS(MNVLS,MNZ,2),KVCELLS(MNVLS,MNZ,2)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      common/blksso/NBLK(MNZ),INBLK(MNVLS,MNZ),NSSO(MNZ),
     &          INSSO(MNVLS,MNZ)

      COMMON/MFLW11/PSTACK(MCNN),HDP(MCNN),HDV(MCNN),HFL(MCNN)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)

      COMMON/MFCFD/NCNRMV,ICNRMV(MCFND,MNZ),NCNON,ICNON(2*MCFND,MNZ)
      COMMON/ACT/ACTCNN(MCNN)
      COMMON/CFDMFS2/MFNEWN(MNVLS,MNZ),NFNDN,NCFDOPNO(MNVLS,MNZ),
     &               NFNPDN(MNZ)
      COMMON/CFDMFS1/ICFDNOD(MNZ),ICFDCNN(MNVLS,MNZ)
      COMMON/MFCALC/IPSMOD,MSLVTP

C Commons for trace.
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      
      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW,FLW1OLD(MCNN)
      DOUBLE PRECISION FLW2OLD(MCNN),PSTACK,HDP,HDV,HFL
      LOGICAL ACTCNN,OK,LINVOL
      CHARACTER*12 VOLNAME
      REAL PRSROLD(MNOD)

C Adjust pressure tol from 0.001 to 0.1.
      PARAMETER(TOLM=0.001)
      PARAMETER(TOLP=0.1)

C Check if CFD-MFS trace output is requested.
C ITRC: If 0, no trace. If >0, unit number.
      ITRC=0
      if (ITRACE(44).gt.0) then
C Check if we are within trace period.
        if (NSINC.ge.ITC.and.NSINC.le.ITCF) then
          ITRC=ITU
        endif
      endif

C Remember previous flow network values to compare against new run
      DO 1983 ICNN=1,NCNN
        FLW1OLD(ICNN)=FLW1(ICNN)
        FLW2OLD(ICNN)=FLW2(ICNN)
 1983 CONTINUE
      DO 1984 INOD=1,NNOD
        PRSROLD(INOD)=REAL(PRES(INOD))
 1984 CONTINUE

C Get average pressure in the CFD domain
      PRESSURE=0.0
      NCIO=0
      DO 100 I=2,NIM1
        DO 1001 J=2,NJM1
          DO 1002 K=2,NKM1
C Find total pressure
            PRESSURE=PRESSURE+P(I,J,K)
            NCIO=NCIO+1

C Check for blockages
            IF(NBLK(ICFD).GT.0) THEN 
              DO 2006 IVO=1,NBLK(ICFD)
                IV=INBLK(IVO,ICFD) 
                OK=LINVOL(I,J,K,
     &                  IVCELLS(IV,ICFD,1),IVCELLS(IV,ICFD,2),
     &                  JVCELLS(IV,ICFD,1),JVCELLS(IV,ICFD,2),
     &                  KVCELLS(IV,ICFD,1),KVCELLS(IV,ICFD,2))
                IF(OK) THEN 

C Cell is within the blockage 
                  PRESSURE=PRESSURE-P(I,J,K)
                  NCIO=NCIO-1
                ENDIF
 2006         CONTINUE
            ENDIF
 1002     CONTINUE
 1001   CONTINUE
 100  CONTINUE

C Find average pressure
      IF(NCIO.GT.0)THEN
        PRESSURE=PRESSURE/REAL(NCIO)
      ELSE
        CALL EDISP(IUOUT,'Error MFSCORR: 0 or -ve CFD cells defined')
      ENDIF

C Get pressure correction factor 
      PCORR=REAL(PRES(ICFDNOD(ICFD)))-PRESSURE

C Correct dom_nod pressure using pressure correction. Following line of 
C code is redundant but included to maintain logic of approach
      PRES(ICFDNOD(ICFD))=PRESSURE+PCORR

C KNOD is the ext_nod connected to this opn_nod and JNOD is dom_nod
C INOD is the opn_nod 
C ICNEXT is the connection from KNOD to INOD
C ICNDOM is the connection from JNOD to INOD (INOD is always +ve node)
C For each opn_nod correct the pressure
      DO 1981 INOD=NNOD-NFNDN+1,NNOD

C Get key volume representative of this opn_node
        DO 1982 IVL=1,MNVLS
          IF(MFNEWN(IVL,ICFD).EQ.INOD)M=IVL
 1982   CONTINUE

C Get CFD predicted pressure at cells representative of this key volume
        PRESSURE=0.0
        NCIO=0
        DO 9051 I=IOPENi(M,ICFD),IOPENf(M,ICFD)
          DO 9052 J=JOPENi(M,ICFD),JOPENf(M,ICFD)
            DO 9053 K=KOPENi(M,ICFD),KOPENf(M,ICFD)
              PRESSURE=PRESSURE+P(I,J,K)
              NCIO=NCIO+1
 9053       CONTINUE
 9052     CONTINUE
 9051   CONTINUE
        IF(NCIO.NE.0)PRESSURE=PRESSURE/NCIO
        PRES(INOD)=PRESSURE+PCORR
 1981 CONTINUE

C Calculate connections stack pressure based on previous
C flow direction.
      DO 50 ICNN=1,NCNN
        IDPS=NODPS(ICNN)
        IDNE=NODNE(ICNN)
        IF(IPSMOD.EQ.1) THEN
          IF(FLW1(ICNN).GE.0.0) THEN
            PSTACK(ICNN)=9.81*(RHON(IDPS)*(HNOD(IDPS,3)-HNOD(IDNE,3))
     &                         +HGTNE(ICNN)*(RHON(IDNE)-RHON(IDPS)))
          ELSE
            PSTACK(ICNN)=9.81*(RHON(IDNE)*(HNOD(IDPS,3)-HNOD(IDNE,3))
     &                         +HGTPS(ICNN)*(RHON(IDNE)-RHON(IDPS)))
          ENDIF
        ELSE IF(IPSMOD.EQ.2) THEN
          PSTACK(ICNN)=9.81*0.5*
     &      ((RHON(IDPS)+RHON(IDNE))*(HNOD(IDPS,3)-HNOD(IDNE,3))
     &      +(HGTNE(ICNN)+HGTPS(ICNN))*(RHON(IDNE)-RHON(IDPS)))
        ELSE
          STOP ' MFSCORR: unresolvable error !'
        END IF
   50 CONTINUE

C Activate conflated network
      DO 1 ICN=1,MCNN
        ACTCNN(ICN)=.TRUE.
   1  CONTINUE
      DO 21 IC=1,NCONF
        DO 2001 N=1,NCNRMV
          ACTCNN(ICNRMV(N,IC))=.FALSE.
 2001   CONTINUE
 21   CONTINUE

C Call massflow solver
      CALL MFSOLV

C Deactivate conflated network
      DO 111 ICN=1,MCNN
        ACTCNN(ICN)=.TRUE.
 111  CONTINUE
      DO 201 IC=1,NCONF
        DO 202 N=1,NCNON
          ACTCNN(ICNON(N,IC))=.FALSE.
  202   CONTINUE
  201 CONTINUE

C Now check against previous flow rates and pressures and return with 
C information if within tolerance or not
      JCHK=0
C Trace output.
      if (ITRC.gt.0) then
        CALL MFOUTP(ITRC)
        write(ITRC,'(a)')' '
        write(ITRC,'(a,f6.3)')' Mass flow tolerance = ',TOLM
      endif
      DO 1985 I=1,NCNN
        rtmp=ABS(REAL(FLW1(I)-FLW1OLD(I)))
C Trace output.
        if (ITRC.gt.0) then
          write(ITRC,'(a,i2.2,a,f6.3)')' Flow connection ',I,
     &      ' mass flow rate, current less previous = ',rtmp
          if (rtmp.gt.TOLM) then
            JCHK=1
            write(ITRC,'(a)')'* NOT WITHIN TOLERANCE'
          else
            write(ITRC,'(a)')'* WITHIN TOLERANCE'
          endif
        else
          if(rtmp.gt.TOLM)JCHK=1
        endif
 1985 CONTINUE
C Trace output.
      if (ITRC.gt.0) then
        write(ITRC,'(a)')' '
        write(ITRC,'(a,f6.3)')' Pressure tolerance = ',TOLP
      endif
      DO 1986 INOD=1,NNOD
        rtmp=ABS(REAL(PRES(INOD)-PRSROLD(INOD)))
C Trace output.
        if (ITRC.gt.0) then
          write(ITRC,'(a,i2.2,a,f6.3)')' Flow node ',INOD,
     &      ' pressure, current less previous = ',rtmp
          if (rtmp.gt.TOLP) then
            JCHK=1
            write(ITRC,'(a)')'* NOT WITHIN TOLERANCE'
          else
            write(ITRC,'(a)')'* WITHIN TOLERANCE'
          endif
        else
          if(rtmp.gt.TOLP)JCHK=1
        endif
 1986 CONTINUE      
C Trace output.
      if (ITRC.gt.0) then
        write(ITRC,'(a)')' '
        if (JCHK.eq.0) then
          ICORROK=0
          write(ITRC,'(a)')'** CONVERGED'
        else
          write(ITRC,'(a)')'** NOT CONVERGED'
        endif
      else
        IF(JCHK.EQ.0)ICORROK=0
      endif

      RETURN
      END

      subroutine gettemp(tmp)
      COMMON/CLIMI/QFP,QFF,TP,TF,QDP,QDF,VP,VF,DP,DF,HP,HF
      TMP=0.5*(TF+TP)
      return
      end
