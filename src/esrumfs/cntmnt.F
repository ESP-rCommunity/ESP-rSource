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

C This file contains the following subroutines
C MZCONT main solution engine that calculates contaminant concentrations
C PRERES writes out contaminant concentration vector to 2D array
C MSOLVD same as JSOLVD in mfmach.F
C CTSASM calculates source and sink strengths

C ******************** MZCONT ********************
C Calculates contaminant concentrations 
C for each air point node representing a zone
C Global variables are defined in ctread.F 
C Various local variables used in contaminant modelling:
C ZMCPAM   = Contaminant mass vector ((Q))
C CNCAJAM  = contaminant concentration vector ((X))
C AFLOWM   = Airmass flow matrix with reaction rate coefficients ((K))

      SUBROUTINE MZCONT

#include "building.h"
#include "net_flow.h"

      COMMON/pers/isd1,ism1,isd2,ism2,isds,isdf,ntstep
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/MFLOW3/NDFLD(MNOD),NDTYP(MNOD),HNOD(MNOD),SUPNOD(MNOD,MNDS)
      COMMON/MFLW10/NODPS(MCNN),HGTPS(MCNN),NODNE(MCNN),HGTNE(MCNN),
     &              ITPCON(MCNN),NDSCNN(MCNN,MCNS)
      COMMON/MFLRES/FLW1(MCNN),FLW2(MCNN),PRES(MNOD),
     &              RESID(MNOD),SAFLW(MNOD)
      common/simtim/ihrp,ihrf,idyp,idyf,idwp,idwf,nsinc,its

      COMMON/CONTM/CNTMFIL,CNTMDESC,NTSTEPC
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      COMMON/CONTM1/AFLOWM(MPRODI,MPRODI)
      COMMON/CONTM2/CNCAJAM(MPRODI),ZMCPAM(MPRODI)
      COMMON/CONTM3/CNCAA(MCONTM,MT),FILEFA(MCONTM,MCNN)
     &,FORCAB(MCONTM,MCONTM)
      COMMON/CONTM4/GERATAM(MPRODI),RECCOAM(MPRODI)
      COMMON/CONTM6/CNCNI(MCONTM,MNOD)
      COMMON/CONTM7/PRECON(MPRODI)

      CHARACTER CNTMFIL*72
      CHARACTER CONTMNAM*12,CNTMDESC*124
      DOUBLE PRECISION FLW1,FLW2,PRES,RESID,SAFLW
      REAL DEN,AFLOWM,CNCAJAM,ZMCPAM
      REAL CNCAA,FILEFA,FORCAB
      REAL GERATAM,RECCOAM             
      REAL AA(MPRODI,MPRODI),QDOT(MPRODI),BB(MPRODI),V(MPRODI)
      REAL AAA(MPRODI,MPRODI),XXX(MPRODI),BBB(MPRODI)
      REAL AAAA(MPRODI,MPRODI)
      REAL NODVOL(MNOD)
      LOGICAL NOTFOUND,PRECON,CLOSER

C Setup kounter so that this subroutine is run NTSTEPC number of times
C per building side timestep NTSTEP
      KOUNTER=NTSTEPC/NTSTEP
      IF(KOUNTER.LE.1)KOUNTER=1
      DO 999 IKOUNTER=1,KOUNTER

C Define density and weightage factor Gamma 
C A factor of 0.0 corresponds to standard explicit solution, 0.5 to
C Crank-Nicolson method and 1.0 to standard implicit solution
      DEN=1.2
      GAMMA=0.5
      NPRODI=NCONTM*NNOD
      IF(NSINC.EQ.1) NOEQ=NPRODI
      TSTEP=REAL(NTSTEPC)

C Get node volume      
      IF(NSINC.EQ.1) THEN
        DO 5551 INOD=1,NNOD
          DO 5552 ICMP=1,MCOM
            IF(NDTYP(INOD).LT.2)NODVOL(INOD)=SUPNOD(INOD,2)
 5552     CONTINUE
 5551   CONTINUE
      ENDIF

C Initialise QDOT, V, internal and external concentration vectors and 
C PRECON, 
      DO 7053 INOD=1,NNOD
        DO 7054 ICONTM=1,NCONTM
          IROWNO=NCONTM*(INOD-1)+ICONTM
          QDOT(IROWNO)=0.0
          V(IROWNO)=0.0
          IF(IKOUNTER.EQ.1)THEN
            IF(NDTYP(INOD).LT.2)THEN
              IF(NSINC.EQ.1)THEN
                IF((CNCNI(ICONTM,INOD)+1.0).LE.0.001)THEN
                  CNCAJAM(IROWNO)=CNCAA(ICONTM,IHRP)
                ELSE
                  CNCAJAM(IROWNO)=CNCNI(ICONTM,INOD)
                ENDIF
              ENDIF
            ELSE
              CNCAJAM(IROWNO)=CNCAA(ICONTM,IHRP)
            ENDIF
            CALL PRERES
          ENDIF
          IF(NSINC.EQ.1) THEN
            PRECON(IROWNO)=.TRUE.
            IF(NDTYP(INOD).GE.2)THEN
              PRECON(IROWNO)=.FALSE.
              NOEQ=NOEQ-1
            ELSE
              NOTFOUND=.TRUE.
              ICNN=1
              DO WHILE(ICNN.LE.NCNN.AND.NOTFOUND)
                IF(NODPS(ICNN).EQ.INOD.OR.NODNE(ICNN).EQ.INOD) THEN
                  NOTFOUND=.FALSE.
                ELSE
                  ICNN=ICNN+1
                ENDIF
              END DO
              IF(NOTFOUND) THEN
                NOEQ=NOEQ-1
                PRECON(IROWNO)=.FALSE.
              ENDIF
            ENDIF
          ENDIF
          GERATAM(IROWNO)=0.0
          RECCOAM(IROWNO)=0.0
 7054  CONTINUE
 7053 CONTINUE

C Update generation rates
C << Update filter efficiencies
      CALL CTSASM

      DO 7703 INOD=1,NNOD
C Jump for external nodes
      IF(NDTYP(INOD).GE.2) GOTO 7703

C Get all mass flows out from INOD
C First flush FLOWOUT
      FLOWOUT=0.0
      DO 7710 ICNN=1,NCNN
        IF((NODNE(ICNN).EQ.INOD.AND.FLW1(ICNN).LT.0.0)
     &  .OR.(NODPS(ICNN).EQ.INOD.AND.FLW1(ICNN).GT.0.0)) THEN
          FLOWOUT=FLOWOUT+ABS(REAL(FLW1(ICNN))*(1.0-FILEFA(ICONTM,ICNN))
     &    )
        ENDIF
        CALL ECLOSE(REAL(FLW2(ICNN)),0.0,0.001,CLOSER)
        IF(.NOT.CLOSER) THEN
          IF((NODPS(ICNN).EQ.INOD.AND.FLW2(ICNN).GT.0.0)
     &    .OR.(NODNE(ICNN).EQ.INOD.AND.FLW2(ICNN).LT.0.0)) THEN
            FLOWOUT=FLOWOUT+ABS(REAL(FLW2(ICNN))*(1.0-FILEFA(ICONTM,
     &      ICNN)))         
          ENDIF
        ENDIF
 7710 CONTINUE     
  
C Fill in vector ZMCPAM (Q)
C IROWNO is row number and ICOLNO is columns number
      DO 7704 ICONTM=1,NCONTM
        IROWNO=NCONTM*(INOD-1)+ICONTM
        ZMCPAM(IROWNO)=NODVOL(INOD)*DEN*CNCAJAM(IROWNO)

C Fill in airflow matrix AFLOWM
C First fill in principal diagonal with total flow out and recovery
C coefficient  
        DO 7705 ICOLNO=1,NPRODI
          IF(IROWNO.EQ.ICOLNO)THEN
            AFLOWM(ICOLNO,IROWNO)=-(RECCOAM(IROWNO)+FLOWOUT)

C Now fill in first order chemical reaction rate constants
          ELSEIF(ICOLNO.LE.(NCONTM*INOD).AND.ICOLNO.GT.(NCONTM*(INOD-1)
     &    ))THEN
            AFLOWM(ICOLNO,IROWNO)=FORCAB(ICONTM,ICOLNO-NCONTM*(INOD-1))
     &                            *NODVOL(INOD)*DEN

C Now fill in flow in from connected nodes
          ELSE
            DO 7706 JNOD=1,NNOD
              IF(ICOLNO.EQ.((JNOD-1)*NCONTM+ICONTM)) THEN

C Get mass flow from JNOD to INOD            
C First flush FLOWIN
                FLOWIN=0.0
                DO 7711 ICNN=1,NCNN
                 IF((NODNE(ICNN).EQ.INOD.AND.FLW1(ICNN).GT.0.0.AND.
     &               NODPS(ICNN).EQ.JNOD).OR.(NODPS(ICNN).EQ.INOD.AND.
     &            FLW1(ICNN).LT.0.0.AND.NODNE(ICNN).EQ.JNOD))THEN
                    IF(NDTYP(JNOD).LT.2)THEN
                      FLOWIN=FLOWIN+ABS(REAL(FLW1(ICNN)))*
     &                (1.0-FILEFA(ICONTM,ICNN))
                    ELSE
                      V(IROWNO)=V(IROWNO)+ABS(REAL(FLW1(ICNN)))
     &                *CNCAA(ICONTM,IHRP)*(1.0-FILEFA(ICONTM,ICNN))
                    ENDIF   
                  ENDIF
                  CALL ECLOSE(REAL(FLW2(ICNN)),0.0,0.001,CLOSER)
                  IF(.NOT.CLOSER) THEN
                    IF((NODNE(ICNN).EQ.INOD.AND.FLW2(ICNN).GT.0.0
     &              .AND.NODPS(ICNN).EQ.JNOD).OR.(NODPS(ICNN).EQ.
     &              INOD.AND.FLW2(ICNN).LT.0.0.AND.NODNE(ICNN)
     &              .EQ.JNOD))THEN
                      IF(NDTYP(JNOD).LT.2)THEN
                        FLOWIN=FLOWIN+ABS(REAL(FLW2(ICNN))*
     &                  (1.0-FILEFA(ICONTM,ICNN)))
                      ELSE
                        V(IROWNO)=V(IROWNO)+ABS(REAL(FLW2(ICNN)))
     &                  *CNCAA(ICONTM,IHRP)*(1.0-FILEFA(ICONTM,ICNN))
                      ENDIF   
                    ENDIF
                  ENDIF 
                  AFLOWM(ICOLNO,IROWNO)=FLOWIN
 7711           CONTINUE
              ENDIF
 7706       CONTINUE 
          ENDIF

C Define vector on LHS ((BB))
C B=KX+G The KX part is done here and the +G part is done after the 
C 7705 DO loop
          QDOTR=0.0
          QDOTR=AFLOWM(ICOLNO,IROWNO)*CNCAJAM(ICOLNO)
          QDOT(IROWNO)=QDOT(IROWNO)+QDOTR

C Define matrix on RHS ((AA))
         IF(IROWNO.EQ.ICOLNO) THEN
           AA(ICOLNO,IROWNO)=NODVOL(INOD)*DEN-GAMMA*3600.0/TSTEP*
     &       AFLOWM(ICOLNO,IROWNO)
         ELSE
           AA(ICOLNO,IROWNO)=-GAMMA*3600.0/TSTEP*AFLOWM(ICOLNO,IROWNO)
         ENDIF
 7705   CONTINUE
        QDOT(IROWNO)=QDOT(IROWNO)+GERATAM(IROWNO)
        BB(IROWNO)=ZMCPAM(IROWNO)+3600.0/TSTEP*
     &             ((1.0-GAMMA)*QDOT(IROWNO)+V(IROWNO))
     &             +GAMMA*3600.0/TSTEP*(GERATAM(IROWNO))
 7704  CONTINUE
 7703 CONTINUE

C Precondition AA,BB and CNCAJAM to exclude external and unconnected
C internal nodes
      IROW=1
      DO 7901 IROWNO=1,NPRODI
        IF(PRECON(IROWNO))THEN
          BBB(IROW)=BB(IROWNO)
          XXX(IROW)=CNCAJAM(IROWNO)
          ICOL=1
          DO 7902 ICOLNO=1,NPRODI
            IF(PRECON(ICOLNO)) THEN
              AAA(IROW,ICOL)=AA(IROWNO,ICOLNO)
              ICOL=ICOL+1
            ENDIF
 7902     CONTINUE
          IROW=IROW+1
        ENDIF
 7901 CONTINUE

C Transpose Matrix AAA
      DO 4010 I=1,NOEQ
        DO 4011 J=1,NOEQ
          AAAA(I,J)=AAA(J,I)
 4011   CONTINUE
 4010 CONTINUE

C Call solver for Ax=B 
      CALL MSOLVD(AAAA,NOEQ,MPRODI,XXX,BBB)

C Store computed concentrations back in CNCAJAM, setting -ve 
C concentrations (if any) back to zero
      IROW=1
      IF(XXX(IROW).LT.0.0) XXX(IROW)=0.0
      DO 7903 IROWNO=1,NPRODI
        IF(PRECON(IROWNO)) THEN
          CNCAJAM(IROWNO)=XXX(IROW)
          IROW=IROW+1
        ENDIF  
 7903 CONTINUE

C Write contaminant concentrations to array dimensioned by node and
C contaminant
      CALL PRERES
 999  CONTINUE
      RETURN
      END

C ******************* PRERES
C Subroutine Preres writes out contaminant concentration vector CNCAJAM
C to CCONC(NNOD,NCONTM)

      SUBROUTINE PRERES

#include "net_flow.h"

      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      COMMON/CONTM2/CNCAJAM(MPRODI),ZMCPAM(MPRODI)
      COMMON/CONTM8/CCONC(MNOD,MCONTM)

      CHARACTER CONTMNAM*12
      REAL CNCAJAM,ZMCPAM,CCONC

      DO 701 INOD=1,NNOD
        DO 702 ICONTM=1,NCONTM
          IROWNO=NCONTM*(INOD-1)+ICONTM
          CCONC(INOD,ICONTM)=CNCAJAM(IROWNO)
 702    CONTINUE
 701  CONTINUE
      RETURN
      END

C ******************* MSOLVD 
C MSOLVD is an exact copy of JSOLVD in esrumfs
C MSOLVD solves the matrix equation {A} {X} = {B} based
C on Gaussian elimination with backsubstitution and no pivoting.
C An efficient matrix technique is used to forward reduce the
C N*N matrix A (physical size NP*NP) halfway, to a matrix whose
C components on the diagonal and above remain nontrivial.
C The solution vector X (size N) is then generated through
C backsubstitution of the known right hand side vector B (size N)

      SUBROUTINE MSOLVD(A,N,NP,X,B)

      REAL A(NP,NP),X(N),B(N)
      REAL AB,AC,AD

C Forward reduce matrix A.
      DO 10 I=2,N
   10 A(1,I)=A(1,I)/A(1,1)
      DO 50 K=2,N
      K1=K-1
      DO 20 IK=K,N
      DO 20 J=1,K1
      AB=A(IK,J)
      AC=A(J,K)
      AD=AB*AC
   20 A(IK,K)=A(IK,K)-AD
      J1=K+1
      DO 30 J=J1,N
      DO 40 MJ=1,K1
      AB=A(K,MJ)
      AC=A(MJ,J)
      AD=AB*AC
   40 A(K,J)=A(K,J)-AD
   30 A(K,J)=A(K,J)/A(K,K)
   50 CONTINUE

C Conduct backward substitution to establish solution vextor X.
      B(1)=B(1)/A(1,1)
      DO 70 I=2,N
      K1=I-1
      DO 60 J=1,K1
      AB=B(J)
      AC=A(I,J)
      AD=AB*AC
   60 B(I)=B(I)-AD
   70 B(I)=B(I)/A(I,I)
      X(N)=B(N)
      N1=N-1
      DO 90 JJ=1,N1
      I=N-JJ
      IK=I+1
      DO 80 J=IK,N
      AB=A(I,J)
      AC=B(J)
      AD=AB*AC
   80 B(I)=B(I)-AD
   90 X(I)=B(I)

      RETURN
      END

C ******************** CTSASM ********************
C Contaminant source and sink models: this subroutine calculates the 
C mass injections/removals from INOD depending upon the sources/sinks 
C specified for that INOD
C All global variables are defined in ctread.F, the following local
C variables are used:
C IPRODI = index specifier in concentration and generation rate vectors.
C          Where NCONTM rows are allocated for each INOD
C ISSN = Source/sink number
C ISTYP  = Source/sink type
C RCMGL  = Rate of contaminant mass gained/lost from each source/sink

      SUBROUTINE CTSASM
#include "net_flow.h"
#include "building.h"
#include "FMI.h"

C Simulation time commons
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

C Operations file commons
      common/CASGNS/NCGPER(MCOM,MDTY,MGTY),TCGS(MCOM,MDTY,MGTY,MGPER),
     &        CGSENC(MCOM,MDTY,MGTY,MGPER),CGSENR(MCOM,MDTY,MGTY,MGPER),
     &        CGLAT(MCOM,MDTY,MGTY,MGPER),CGCTL(MCOM,2,MGTY)

C Calendar commons
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

C Mass flow and Contaminant commons
      COMMON/MFLOW1/NNOD,NCMP,NCNN
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/MFLOW4/ITND(MNOD),TNOD(MNOD)

      COMMON/CONTM0/NCONTM,NOCNTM,CONTMNAM(MCONTM)
      COMMON/CONTM2/CNCAJAM(MPRODI),ZMCPAM(MPRODI)
      COMMON/CONTM4/GERATAM(MPRODI),RECCOAM(MPRODI)
      COMMON/CONTM5/SPMSUP(MSPMNO,MCSD),SSLINK2(MSPMNO,MNOD),
     &SPMTYP(MSPMNO),SSNAME(MSPMNO),NSPMNO,SSLINK1(MSPMNO,MCONTM)
      COMMON/CONTM7/PRECON(MPRODI)
      COMMON/CONTM9/SSSTR(MSPMNO,MCONTM)

      REAL CNCAJAM,ZMCPAM,GERATAM,RECCOAM,SPMSUP,RCMGL
      INTEGER SPMTYP,SSLINK1,SSLINK2
      CHARACTER SSNAME*12,CONTMNAM*12
      LOGICAL ISLNKD1,ISLNKD2,PRECON
      CHARACTER LAPROB*72

C Initialise source / sink mass 
      RCMGL=0.0

      DO 1 INOD=1,NNOD
        DO 2 ICONTM=1,NCONTM
          IPRODI=NCONTM*(INOD-1)+ICONTM
          IF(PRECON(IPRODI))THEN
            ISLNKD1=.TRUE.
            ISLNKD2=.TRUE.

C Get source/sink linked with this contaminant at this node
            ISPMNO1=1
            ISPMNO2=1
            DO WHILE(ISLNKD1)
             IF(SSLINK1(ISPMNO1,ICONTM).EQ.0)GOTO 123
             IF(SSLINK1(ISPMNO1,ICONTM).NE.0)THEN
              DO WHILE(ISLNKD2)
                IF(SSLINK2(ISPMNO2,INOD).EQ.0)GOTO 124
                DO 100 ISPMNO=1,MSPMNO  
                  IF(SSLINK2(ISPMNO2,INOD).EQ.SSLINK1(ISPMNO,ICONTM))
     &            THEN

C Get source/sink type and number
                    ISSN=SSLINK2(ISPMNO2,INOD)
                    ISTYP=SPMTYP(ISSN)

C Calculate amount of contaminant generated 
                    IF(ISTYP.EQ.1) THEN
                      RCMGL=SPMSUP(ISSN,1)
                    ELSEIF(ISTYP.EQ.2) THEN
                      IF(CNCAJAM(IPRODI).LT.SPMSUP(ISSN,2))THEN
                        RCMGL=SPMSUP(ISSN,1)*(1.0-CNCAJAM(IPRODI)
     &                  /SPMSUP(ISSN,2))
                      ELSE
                        RCMGL=0.0
                      ENDIF
                    ELSEIF(ISTYP.EQ.3) THEN
                      IDAYSTART = INT(SPMSUP(ISSN,3))
                      IMNTSTART = INT(SPMSUP(ISSN,4))
                      IHRSTART  = INT(SPMSUP(ISSN,5))
                      CALL EDAY(IDAYSTART,IMNTSTART,IDYNSTR)
                      IHRSTR=IDYNSTR*24+IHRSTART
                      IHRSIM=IDYP*24+IHRP
                      IF(IHRSIM.LT.IHRSTR)THEN
                        RCMGL=0.0
                      ELSE
                        RCMGL=SPMSUP(ISSN,1)*EXP(-(IHRSIM-IHRSTR)
     &                  /SPMSUP(ISSN,2))
                      ENDIF
                    ELSEIF(ISTYP.EQ.4) THEN
                      ZMADSRBNT=SPMSUP(ISSN,4)*SPMSUP(ISSN,3)
                      RCMGL=-SPMSUP(ISSN,1)*SPMSUP(ISSN,2)*SPMSUP(ISSN,3
     &                )*(CNCAJAM(IPRODI)-SPMSUP(ISSN,6)/SPMSUP(ISSN,5))
                      SPMSUP(ISSN,6)=-RCMGL/ZMADSRBNT+SPMSUP(ISSN,6)
                    ELSEIF(ISTYP.EQ.5) THEN
                      IDAYSTART = INT(SPMSUP(ISSN,2))
                      IMNTSTART = INT(SPMSUP(ISSN,3))
                      IHRSTART  = INT(SPMSUP(ISSN,4))
                      IDAYSTOP  = INT(SPMSUP(ISSN,5))
                      IMNTSTOP  = INT(SPMSUP(ISSN,6))
                      IHRSTOP   = INT(SPMSUP(ISSN,7))
                      CALL EDAY(IDAYSTART,IMNTSTART,IDYNSTR)
                      CALL EDAY(IDAYSTOP,IMNTSTOP,IDYNSTP)
                      IHRSTR=IDYNSTR*24+IHRSTART
                      IHRSTP=IDYNSTP*24+IHRSTOP
                      IHRSIM=IDYP*24+IHRP
                      IF(IHRSIM.LT.IHRSTP.AND.IHRSIM.GE.IHRSTR)THEN
                        RCMGL=SPMSUP(ISSN,1)
                      ENDIF
                    ELSEIF(ISTYP.EQ.6.AND.SPMSUP(ISSN,1).GT.0.0) THEN
                      IDAYSTART = INT(SPMSUP(ISSN,2))
                      IMNTSTART = INT(SPMSUP(ISSN,3))
                      IHRSTART  = INT(SPMSUP(ISSN,4))
                      IDAYSTOP  = INT(SPMSUP(ISSN,5))
                      IMNTSTOP  = INT(SPMSUP(ISSN,6))
                      IHRSTOP   = INT(SPMSUP(ISSN,7))
                      CALL EDAY(IDAYSTART,IMNTSTART,IDYNSTR)
                      CALL EDAY(IDAYSTOP,IMNTSTOP,IDYNSTP)
                      IHRSTR=IDYNSTR*24+IHRSTART
                      IHRSTP=IDYNSTP*24+IHRSTOP
                      IHRSIM=IDYP*24+IHRP

C Density of CO2 is cubically interpolated from 'known' densities
C at -23, 0, 15, 27 and 78.4 Celsius
                      TMPT=273.16+TNOD(INOD)
                      CO2DEN=-0.96893+0.047248*TMPT-0.00020068*
     &                  TMPT**2+2.4604e-07*TMPT**3
                      IF(IHRSIM.LT.IHRSTP.AND.IHRSIM.GE.IHRSTR)THEN
                        IF((ABS(SPMSUP(ISSN,1))-1.).LT.0.1) THEN
                          RCMGL=4.*CO2DEN*1E-6*SPMSUP(ISSN,8)
                        ELSEIF((ABS(SPMSUP(ISSN,1))-2.).LT.0.1) THEN
                          RCMGL=9.*CO2DEN*1E-6*SPMSUP(ISSN,8)
                        ELSEIF((ABS(SPMSUP(ISSN,1))-3.).LT.0.1) THEN
                          RCMGL=16.*CO2DEN*1E-6*SPMSUP(ISSN,8)
                        ELSEIF((ABS(SPMSUP(ISSN,1))-4.).LT.0.1) THEN
                          RCMGL=23.*CO2DEN*1E-6*SPMSUP(ISSN,8)
                        ELSEIF((ABS(SPMSUP(ISSN,1))-5.).LT.0.1) THEN
                          RCMGL=29.*CO2DEN*1E-6*SPMSUP(ISSN,8)
                        ELSE
                          RCMGL=SPMSUP(ISSN,9)*CO2DEN*1E-6*
     &                    SPMSUP(ISSN,8)/25.
                        ENDIF
                      ENDIF
                    ELSEIF(ISTYP.EQ.6.AND.SPMSUP(ISSN,1).LT.0.0) THEN

C Special case of CO2 mass taken from operations file                      
C First get day type
                      IDYTP=icalender(IDYP)

C Get zone represented by this airflow node
                      DO 576 IZN=1,MCOM
                        IF(ICAAS(IZN).EQ.INOD)ICOMP=IZN
 576                  CONTINUE

C Assume casual gain type 1 is occupancy
                      IGTYP=1
                      POW=0.0
                      DO 577 IGNO=1,NCGPER(ICOMP,IDYTP,IGTYP)
                        IF(IHRP.GE.TCGS(ICOMP,IDYTP,IGTYP,IGNO).AND.
     &                     IHRP.LE.TCGS(ICOMP,IDYTP,IGTYP,IGNO+1))THEN
C <FMI>
C If FMI occupancy control is active, need to multiply gains by control
C value.
                          if (FMUDOCTL(ICOMP,7)) then
                            POW=(CGSENC(ICOMP,IDYTP,IGTYP,IGNO) +
     &                           CGSENR(ICOMP,IDYTP,IGTYP,IGNO) +
     &                           CGLAT(ICOMP,IDYTP,IGTYP,IGNO)) *
     &                           FMUCTL(ICOMP,7)
                          else
C </FMI>
                            POW=CGSENC(ICOMP,IDYTP,IGTYP,IGNO) +
     &                          CGSENR(ICOMP,IDYTP,IGTYP,IGNO) +
     &                          CGLAT(ICOMP,IDYTP,IGTYP,IGNO) 
                          endif
                        ENDIF
 577                  CONTINUE

C Calculate volume of CO2 given off at 4E-5 litres/sec from 1 Watt
C Density of CO2 is cubically interpolated from 'known' densities
C at -23, 0, 15, 27 and 78.4 Celsius
                      TMPT=273.16+TNOD(INOD)
                      CO2DEN=-0.96893+0.047248*TMPT-0.00020068*
     &                  TMPT**2+2.4604e-07*TMPT**3
                      RCMGL=POW*4.0E-5*CO2DEN/1000
                    ENDIF
                    GERATAM(IPRODI)=GERATAM(IPRODI)+RCMGL
                    SSSTR(ISPMNO,ICONTM)=RCMGL
                  ENDIF
 100            CONTINUE

C Increment counter and go on to next source/sink
                  ISPMNO2=ISPMNO2+1
                  GOTO 125
 124              ISLNKD2=.FALSE.
 125            END DO
                ISPMNO1=ISPMNO1+1
                GOTO 126
              ENDIF
 123          ISLNKD1=.FALSE.
 126        END DO
          ENDIF
 2      CONTINUE
 1    CONTINUE
      RETURN
      END
