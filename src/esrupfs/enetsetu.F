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

C ************************************************************************************
C At the start of each simulation the routines in this file are used to set up 
C the electrical network prior to its solution. 
C The routines are as follows:
C ENETSETU - the main controlling routine for the setting up of the electrical
C            network. 
C ENETSINI - this routine initialises all the electrical network solution 
C            variables.
C ENETLIBH - this routine writes the header iformation to the electrical 
C          - results library.
C ENETCBAZ - calculates the impedance values of the linkages between the nodes in the
C            electrical network. These impedance values are NOT PER-UNIT.
C ENETZ2PU - calculates the primitive per-unit impedances between the nodes in the
C            electrical network. This inmpedance is the per-unit form of the 
C            connecting component's impedance matrix. Depending on the connection
C            phase the connection per-unit impedance will be a matrix of 1,4 or 9 
C            elements. 
C ENETCPUY - calculates the primitive per-unit admittances between the nodes in the
C            electrical network. The admittance is usually the inverse of the complex 
C            impedance of the linkage between the nodes. However, when conductors are
C            mutually coupled (i.e. overhead lines), the calculation of the admittance
C            is more convoluted. All admittance values as calculated in PER-UNIT.
C ENETADMY - calculates the self-admittance and branch admittances for each node            C            in the electrical network. 
C ENETPARAM- this routine allows the user to set up the electrical power flow solution
C            parameters.
C     
C ******************************* ENETSETU ********************************************
C This subroutine controls the setting up of the electrical network prior to 
C the commencement of an integrated simulation.
      SUBROUTINE ENETSETU

C Initialise the network and read in the electrical network file
      CALL ENETINIT
      CALL ENETREAD('S')

C Initialise all the network variables. 
      CALL ENETSINI

C Set up the network impedance matrix and convert to per-unit.
      CALL ENETCBAZ
      CALL ENETZ2PU

C Convert to a primitive per-unit admittance matrix
      CALL ENETCPUY

C Calculate the driving-port and transfer admittances for each node. 
      CALL ENETADMY 

      RETURN 
      END

C  
C ******************************* ENETSINI ********************************************  
C This routine initialises all the electrical network and solution
C parameters prior to the start of an electrical network simulation. 
C 
      SUBROUTINE ENETSINI
#include "power.h"

C The PER-UNIT electrical network state variables.
      COMMON/ENETVPU/VOLTPUP(MENOD),VOLTPUF(MENOD)

C The electrical simulation parameters. 
      COMMON/ENETSIMP/ENETMAXI,ENETSLVT,ENETVCVG,ENETACVG,ENETVLIM,
     &ENODCVGF(MENOD),CVGCHK

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP

C The electrical network topology
      COMMON/ECONN/NECON,ECONO(MECON),CONPHTYP(MECON),
     &CONPH(MECON,MPHAS),CCNO(MECON),SENOD(MECON,MPHAS),
     &EENOD(MECON,MPHAS)

C The connecting components
      COMMON/CONCOM/NCONECOM,CONECOMNO(MCONECOM),CONECOMID(MCONECOM),
     &CONECOMPHTYP(MCONECOM),NCONECOMDAT(MCONECOM),
     &CONECOMDAT(MCONECOM,MCCDAT)

C The calculated component impedance (SI)
      COMMON/ECONZVAL/NCONZ(MCONECOM),CONZ(MCONECOM,MZARRY)

C The calculated component impedance (per-unit)
      COMMON/ECONZYPU/NCONZPU(MECON),CONZPU(MECON,MZARRY),
     &CONYPU(MECON,MYARRY)

C The electrical network jacobian solution matrix
      COMMON/ENETMTRX/JACOBEL(MJROW,MJCOL),EMATJZ(MJROW),
     &EDELTA(MJROW)

C The array of driving port and transfer admittances
      COMMON/YARRY/NYELM,YADM(MENOD,MENOD)

C More network-related info - phase angles.
      COMMON/ENETMSC/PHASEANG(MPHAS)

C Initialization status of power-only components
      COMMON/ENetPowComp/bComp_initialized
      logical bComp_Initialized(MPOWCOM)

            
      INTEGER NCONZPU,NCONECOM,CONECOMNO,CONECOMID,CONECOMPHTYP,
     &NCONECOMDAT,NECON,ECONO,CONPHTYP,CONPH,CCNO,SENOD,EENOD,NCONZ,
     &NENOD,ENODNO,ENODPHTYP,ENODPH,ENODTYP,ENETMAXI,ENETSLVT,NYELM,
     &ENODCVGF

      REAL RADPHA,PHASEANG,CONECOMDAT,ENODBASEV,ENODBASEP,ENETACVG,
     &ENETVLIM,ENETVCVG

      DOUBLE PRECISION JACOBEL,EMATJZ,EDELTA

      COMPLEX CONZPU,CONYPU,YADM,VOLTPUF,VOLTPUP,CONZ

      LOGICAL CVGCHK

C Solver parameters  
      ENETMAXI=100
      ENETSLVT=2
      ENETACVG=10.
      ENETVCVG=0.1
      ENETVLIM=5.0
         
C Nodal Variables
      DO 10 INOD=1,NENOD

C Voltages.
        PI=3.142857
        RADPHA=PHASEANG(ENODPH(INOD))*(PI/180.)
        VOLTPUP(INOD)=CMPLX(COS(RADPHA),SIN(RADPHA))
        VOLTPUF(INOD)=VOLTPUP(INOD)
  10  CONTINUE

C Admittance and impedance values.      
      DO 20 ICCN=1,NCONECOM

C Component impedances 
        NCONZ(ICCN)=0
        DO 30 IZL=1,MZARRY 
          CONZ(ICCN,IZL)=CMPLX(0.0,0.0)
  30    CONTINUE
  20  CONTINUE

C Network per-unit impedances and admittances
      DO 40 ICON=1,NECON
        NCONZPU(ICON)=0
        DO 50 IZL=1,MZARRY
          CONZPU(ICON,IZL)=CMPLX(0.0,0.0)
          CONYPU(ICON,IZL)=CMPLX(0.0,0.0)
  50    CONTINUE
  40  CONTINUE
      NYELM=0
      DO 60 INOD=1,NENOD
        DO 70 JNOD=1,NENOD
          YADM(INOD,JNOD)=CMPLX(0.0,0.0)
  70    CONTINUE
  60  CONTINUE

C Matrix parameters
      DO 80 IJR=1,MJROW
        EMATJZ(IJR)=0.0
        EDELTA(IJR)=0.0
        DO 90 IJC=1,MJCOL
          JACOBEL(IJR,IJC)=0.0
  90  CONTINUE
  80  CONTINUE

C Set power-only component initialization flags to false.
C Power-only component data requiring initialization will
C set these data to true after initialization is complete.
      DO iComponent=1, MPOWCOM
        bComp_Initialized(iComponent) = .false.
      ENDDO

  
      RETURN 
      END
 
C ******************************* ENETLIBH ********************************************
C This routine opens the library and writes the basic electrical network data to the 
C electrical results library.
C
C Variables introduced in this routine are:
C IELFIL   - the electrical results library file number
C IELIBHL  - the library header length im records
C IELIBW   - the library width
C IELSTREC - the last written record
C NETS     - the number of electrical time steps
C NPREC    - the number of records used to store power-only component data
C NHREC    - the number of records used to store hybrid component data

C **************************************************************************************
      SUBROUTINE ENETLIBH 

#include "power.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL

C Common for electrical file 
      COMMON/ENTFILE/ENTFLNAM,IENTXIST

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP

C The connected hybrid components
      COMMON/HYBE/NHYBCOM,HYBCOMNO(MHYCOM),HYCOMTYP(MHYCOM),
     &HYCOMPHTYP(MHYCOM),HYCONEN(MHYCOM,MPHAS),HYLOC(MHYCOM,MPHAS),
     &NHYBDAT(MHYCOM),HYBDAT(MHYCOM,MHYDAT)

C The connected power only components
      COMMON/POWC/NPOWCOM,POWCOMNO(MPOWCOM),POWCOMID(MPOWCOM),
     &POWCOMPHTYP(MPOWCOM),POWCONEN(MPOWCOM,MPHAS),
     &NPOWCDAT(MPOWCOM),NPOWCDATS(MPOWCOM),POWCDAT(MPOWCOM,MPWDAT)


C The electrical network topology
      COMMON/ECONN/NECON,ECONO(MECON),CONPHTYP(MECON),
     &CONPH(MECON,MPHAS),CCNO(MECON),SENOD(MECON,MPHAS),
     &EENOD(MECON,MPHAS)

C Electrical library details - the number of records per timestep and the number 
C of records used to store hybrid and power only data. 
      COMMON/ENELIBRC/IELFIL,IELIBHL,IELIBW,IELSTREC,NETS,NTSREC,NHREC,
     &NPREC

C Type casting (all variables and arrays cast explicitly)
      INTEGER NENOD,ENODNO,ENODPHTYP,ENODPH,ENODTYP,NHYBCOM,HYBCOMNO,
     &HYCOMTYP,HYCOMPHTYP,HYCONEN,HYLOC,NHYBDAT,NPOWCOM,POWCOMNO,
     &POWCOMID,POWCOMPHTYP,POWCONEN,NPOWCDAT,NPOWCDATS,CONPH,NECON,
     &ECONO,CONPHTYP,CCNO,SENOD,EENOD,NTSREC,NHREC,NPREC

      INTEGER IENTXIST,IUOUT,IUIN,IFIL,IELFIL,IELIBHL,IELIBW,
     &IELSTREC
     
      REAL ENODBASEV,ENODBASEP,HYBDAT,POWCDAT

C Characters
      CHARACTER*72 ENTFLNAM

C Get the results library file number and record width from the include file. 
      IELFIL=IFIL+MADFIL
      IELIBW=MELIBW
      IELIBHL=MELIBH
C Calculate the number of records required for power-only component data.
      IF(NHYBCOM.GT.0) THEN
        IF(MOD(2*NHYBCOM,IELIBW).NE.0)THEN
          NHREC=INT(2*NHYBCOM/IELIBW)+1
        ELSE
          NHREC=INT(2*NHYBCOM/IELIBW)
        ENDIF
      ELSE
          NHREC=0
      ENDIF

C Calculate the number of records required for hybrid component data.
      IF(NPOWCOM.GT.0) THEN
        IF(MOD(2*NPOWCOM,IELIBW).NE.0)THEN
          NPREC=INT(2*NPOWCOM/IELIBW)+1
        ELSE
          NPREC=INT(2*NPOWCOM/IELIBW)
        ENDIF
      ELSE
          NPREC=0
      ENDIF

C Calculate the number of records per timestep (the final records hold the 
C network data). 
      NTSREC=NENOD+NECON+NHREC+NPREC+2

C Write out the results header 
      IREC=1
      WRITE(IELFIL,REC=IREC,IOSTAT=ISTAT,ERR=999) ENTFLNAM
      IREC=IREC+1
      WRITE(IELFIL,REC=IREC,IOSTAT=ISTAT,ERR=999) NENOD,NECON,
     &NHYBCOM,NPOWCOM,NHREC,NPREC,NTSREC
      IELSTREC=IREC

      RETURN

  999 WRITE(IUOUT,*)' ENETLIBH ERROR: problem writing header.'
      CALL EPWAIT
      CALL EPAGEND
      STOP 

      END
C
C ******************************* ENETCBAZ ********************************************
C This routine calculates the impedance parameters for each connecting element in the
C network. The impedance values are held in a 1-dimensional array. Array structure
C examples for various components are given below.
C Single phase/d.c. conductor:
C Zp Iab = Va - Vb    Zp held in Array [1]
C 
C Two phase conductor (with or without mutual couplings):
C | Zp1  Zm  | |Ia1b1| = Va1-Vb1
C | Zm   Zp2 | |Ia2b2| = Va2-Vb2 Zp1 Zm Zm Zp2 held in Array [1 2 3 4]
C
C Three phase conductor (with or without multual couplings):
C | Zp1  Zm    Zm| |Ia1b1| = Va1-Vb1
C | Zm   Zp2   Zm| |Ia2b2| = Va2-Vb2 
C | Zm   Zm   Zp3| |Ia3b3| = Va3-Vb3 
C Zp1 Zm Zm Zm Zp2 Zm Zm Zm Zp3  held in Array [1 2 3 4 5 6 7 8 9]
C
C Three phase coupled transformer (with or without multual couplings):
C | Zp1  Zm    Zm| |Ia1| = Va1
C | Zm   Zp2   Zm| |Ia2| = Va2 
C | Zm   Zm   Zp3| |Ia3| = Va3 
C Zp1 Zm Zm Zm Zp2 Zm Zm Zm Zp3  held in Array [1 2 3 4 5 6 7 8 9]
C
C Three phase coupled transformer (with or without mutual couplings):
C | Zp1  Zm    Zm| |Ib1| = Vb1
C | Zm   Zp2   Zm| |Ib2| = Vb2 
C | Zm   Zm   Zp3| |Ib3| = Vb3 
C Zp1 Zm Zm Zm Zp2 Zm Zm Zm Zp3  held in Array [10 11 12 13 14 15 16 17 18]
C
C Currently supported component calcluations are
C Cables and lines
C 1 - d.c. line/cable
C 2 - 1-phase a.c. line/cable
C 3 - 2-phase a.c. line/cable (w/wo mutual coupling between phases)
C 4 - 3-phase a.c. line/cable (w/wo mutual coupling between phases)
C
C Transformers
C 10 - 1-phase single winding transformer *-*
C 11 - 2-phase (single winidings) transformer (common core) *-*<<to be added>>
C 12 - 3-phase (single windings)  transformer (common core) *-*
C 13 - 1-phase single winding transformer *-/\ with phase shift <<to be added>> 
C
C Controllable switches<<to be added >>
C 20 - Controllable switch in d.c. connection
C 21 - Controllable switch in 1-phase a.c. connection 
C 22 - Controllable switch in 2-phase a.c. connection
C 23 - Controllable switch in 3-phase a.c. connection
C
C AC-DC Inverters/Rectifiers <<to be added>>
C 30 - Single phase inverter. 
C 
C Variables introdiced in this subroutine are:
C NCONZ(MCONECOM)       - the number of impedance values for each component.
C CONZ(MCONECOM,MZARRY) - array containing the connecting component impedance
C                         values. 
C ******************************* ENETCBAZ  ******************************************* 
C 
      SUBROUTINE ENETCBAZ

#include "power.h"
#include "building.h" 

C Trace
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

C The connecting components
      COMMON/CONCOM/NCONECOM,CONECOMNO(MCONECOM),CONECOMID(MCONECOM),
     &CONECOMPHTYP(MCONECOM),NCONECOMDAT(MCONECOM),
     &CONECOMDAT(MCONECOM,MCCDAT)

C The calculated component impedance
      COMMON/ECONZVAL/NCONZ(MCONECOM),CONZ(MCONECOM,MZARRY)

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP


C Type casting (all variables and arrays cast explicitly)
      INTEGER NCONECOM,CONECOMNO,CONECOMID,CONECOMPHTYP,
     &NCONECOMDAT,NCONZ,IDLC,INC, NENOD

      REAL CONECOMDAT,R,X,M,RN,XN,NM,R1,X1,R2,X2,RN1,XN1,RN2,XN2,L
      INTEGER ENODNO, ENODPHTYP, ENODPH, ENODTYP
      REAL  ENODBASEV, ENODBASEP

      COMPLEX CONZ
      CHARACTER*124 OUTS

C Trace 
        IF(ITRACE(2).GT.0) THEN
          WRITE(ITU,*)' '
          WRITE(ITU,*)'Subroutine ENETCBAZ'
        ENDIF
C Based on the connecting component model type calculate the basic impedance 
C impedance characteristics. Note: suppress 'no connections' warning if 
C network consists of only one node. 
      IF(NCONECOM.EQ.0 .AND. NENOD .GT. 1 ) THEN
        WRITE(OUTS,'(A)') 
     &'Subroutine: ENETCBAZ, warning no connecting components detected!'
        CALL EDISP(IUOUT,OUTS)
        RETURN
      ENDIF
      DO 10 ICCN=1,NCONECOM
        IF(CONECOMID(ICCN).EQ.1) THEN

C Calculation for the d.c. conductor

C Firstly check for the correct number of data items
          IF(NCONECOMDAT(ICCN).NE.2) GOTO 1001

C Zero the local variables 
          R=0.
          L=0.
C Get the series resistance characteristics for the d.c. line
          L=CONECOMDAT(ICCN,2)   
          R=CONECOMDAT(ICCN,1)*L   

C Write this into the complex common element.
          NCONZ(ICCN)=1
          CONZ(ICCN,1)=CMPLX(R,0.0)    

        ELSEIF(CONECOMID(ICCN).EQ.2) THEN

C Calculation for the 1-phase a.c. conductor

C Firstly check for the correct number of data items
          IF(NCONECOMDAT(ICCN).NE.6) GOTO 1001

C Zero the local variables 
          R=0.
          X=0.
          RN=0.
          XN=0.
          NM=0.
          L=0.
C Get the series resistance, reactance characteristics for the a.c. line
C also the multual coupling between the phase conductor and the neutral
          L=CONECOMDAT(ICCN,6)
          R=CONECOMDAT(ICCN,1)*L  
          X=CONECOMDAT(ICCN,2)*L
          RN=CONECOMDAT(ICCN,3)*L  
          XN=CONECOMDAT(ICCN,4)*L
          NM=CONECOMDAT(ICCN,5)*L

C Calculate the complex impedance term referenced to the neutral.
C Given by Zph+Zn-2Zm,ph-n (1x1 array):   
          NCONZ(ICCN)=1  
          CONZ(ICCN,1)=CMPLX(R,X)+CMPLX(RN,XN)-2*CMPLX(0.0,NM)

        ELSEIF(CONECOMID(ICCN).EQ.3) THEN

C Calculation for the 2-phase a.c. conductor

          IF(NCONECOMDAT(ICCN).NE.7) GOTO 1001
C Zero the local variables 
          R=0.
          X=0.
          RN=0.
          XN=0.
          NM=0.
          L=0.
C Get the series resistance and reactance characteristics for the a.c. line

C Series resistance and reactance and mutual couplings for the phase lines 
C (assumed identical in this model.
          L=CONECOMDAT(ICCN,7)
          R=CONECOMDAT(ICCN,1)*L 
          X=CONECOMDAT(ICCN,2)*L
          M=CONECOMDAT(ICCN,3)*L
C Series resistance and reactance for the neutral
          RN=CONECOMDAT(ICCN,4)*L 
          XN=CONECOMDAT(ICCN,5)*L
          NM=CONECOMDAT(ICCN,6)*L

C Calculate the complex impedance terms referenced to the neutral.
C Given by Zph+Zn-2Zm,ph-n and Zm+Zn-2Zm (2x2 array):   
          NCONZ(ICCN)=4    
          CONZ(ICCN,1)=CMPLX(R,X)+CMPLX(RN,XN)-2*CMPLX(0.0,NM)
          CONZ(ICCN,2)=CMPLX(0.0,M)+CMPLX(RN,XN)-2*CMPLX(0.0,NM)
          CONZ(ICCN,3)=CMPLX(0.0,M)+CMPLX(RN,XN)-2*CMPLX(0.0,NM)
          CONZ(ICCN,4)=CMPLX(R,X)+CMPLX(RN,XN)-2*CMPLX(0.0,NM)


        ELSEIF(CONECOMID(ICCN).EQ.4) THEN

C Calculation for the 3-phase a.c. conductor

          IF(NCONECOMDAT(ICCN).NE.7) GOTO 1001
C Zero the local variables 
          R=0.
          X=0.
          RN=0.
          XN=0.
          NM=0.
          L=0.
C Series resistance and reactance and mutual couplings for the phase lines 
C (assumed identical in this model).
          L=CONECOMDAT(ICCN,7)
          R=CONECOMDAT(ICCN,1)*L
          X=CONECOMDAT(ICCN,2)*L
          M=CONECOMDAT(ICCN,3)*L

C Series resistance and reactance for the neutral and the mutual coupling
C between the neutral and the phase conductors.
          RN=CONECOMDAT(ICCN,4)*L  
          XN=CONECOMDAT(ICCN,5)*L
          NM=CONECOMDAT(ICCN,6)*L

C Calculate the complex impedance terms referenced to the neutral.
C Given by Zph+Zn-2Zm,ph-n and Zm+Zn-2Zm (3x3 array):   
          NCONZ(ICCN)=9  
          CONZ(ICCN,1)=CMPLX(R,X)+CMPLX(RN,XN)-2*CMPLX(0.0,NM)
          CONZ(ICCN,2)=CMPLX(0.0,M)+CMPLX(RN,XN)-2*CMPLX(0.0,NM)
          CONZ(ICCN,3)=CMPLX(0.0,M)+CMPLX(RN,XN)-2*CMPLX(0.0,NM)
          CONZ(ICCN,4)=CMPLX(0.0,M)+CMPLX(RN,XN)-2*CMPLX(0.0,NM)
          CONZ(ICCN,5)=CMPLX(R,X)+CMPLX(RN,XN)-2*CMPLX(0.0,NM)
          CONZ(ICCN,6)=CMPLX(0.0,M)+CMPLX(RN,XN)-2*CMPLX(0.0,NM)
          CONZ(ICCN,7)=CMPLX(0.0,M)+CMPLX(RN,XN)-2*CMPLX(0.0,NM)
          CONZ(ICCN,8)=CMPLX(0.0,M)+CMPLX(RN,XN)-2*CMPLX(0.0,NM)
          CONZ(ICCN,9)=CMPLX(R,X)+CMPLX(RN,XN)-2*CMPLX(0.0,NM)

        ELSEIF(CONECOMID(ICCN).EQ.10) THEN

C Calculation for the Single-phase transformer
          IF(NCONECOMDAT(ICCN).NE.8) GOTO 1001
        
C Zero the local variables
          R1=0.0 
          X1=0.0
          R2=0.0 
          X2=0.0
          RN1=0.0 
          XN1=0.0
          RN2=0.0 
          XN2=0.0
        
C Series resistance and reactance and mutual couplings for the high voltage
C side (assumed identical in this model). Note the first data item is the
C transformer turns ratio.
          R1=CONECOMDAT(ICCN,2)  
          X1=CONECOMDAT(ICCN,3)

C Series resistance and reactance and mutual couplings for the low voltage
C side (assumed identical in this model). 
          R2=CONECOMDAT(ICCN,4)  
          X2=CONECOMDAT(ICCN,5)

C Series resistance and reactance for the neutral.
          RN1=CONECOMDAT(ICCN,6)  
          XN1=CONECOMDAT(ICCN,7)

C Series resistance and reactance for the neutral.
          RN2=CONECOMDAT(ICCN,8)  
          XN2=CONECOMDAT(ICCN,9)

C Calculate the high and low-voltage side impedance values.
C Given by Z1+Zn1,ph-n and Z2+Zn2 2x(1x1 arrays).
          NCONZ(ICCN)=2
          CONZ(ICCN,1)=CMPLX(R1,X1)+CMPLX(RN1,XN1)
          CONZ(ICCN,2)=CMPLX(R2,X2)+CMPLX(RN2,XN2)      

      ELSEIF(CONECOMID(ICCN).EQ.12) THEN

C Calculation for the 3-single-phase transformers in 3-phase 
C *-* connection.
          IF(NCONECOMDAT(ICCN).NE.8) GOTO 1001
        
C Zero the local variables
          R1=0.0 
          X1=0.0
          R2=0.0 
          X2=0.0
          RN1=0.0 
          XN1=0.0
          RN2=0.0 
          XN2=0.0
        
C Series resistance and reactance and mutual couplings for the high voltage
C side (phases assumed identical in this model). Note the first data item is the
C transformer turns ratio.
          R1=CONECOMDAT(ICCN,2)  
          X1=CONECOMDAT(ICCN,3)

C Series resistance and reactance and mutual couplings for the low voltage
C side (phases are assumed identical in this model).
          R2=CONECOMDAT(ICCN,4)  
          X2=CONECOMDAT(ICCN,5)

C Series resistance and reactance for the neutral.
          RN1=CONECOMDAT(ICCN,6)  
          XN1=CONECOMDAT(ICCN,7)

C Series resistance and reactance for the neutral.
          RN2=CONECOMDAT(ICCN,8)  
          XN2=CONECOMDAT(ICCN,9)

C Calculate the high -voltage side impedance values.
C Given by Z1+Zn1,ph-n and Zn1 1x(3x3 array).
        
          CONZ(ICCN,1)=CMPLX(R1,X1)+CMPLX(RN1,XN1)
          CONZ(ICCN,2)=CMPLX(RN1,XN1)
          CONZ(ICCN,3)=CMPLX(RN1,XN1)
          CONZ(ICCN,4)=CMPLX(RN1,XN1)
          CONZ(ICCN,5)=CMPLX(R1,X1)+CMPLX(RN1,XN1) 
          CONZ(ICCN,6)=CMPLX(RN1,XN1) 
          CONZ(ICCN,7)=CMPLX(RN1,XN1) 
          CONZ(ICCN,8)=CMPLX(RN1,XN1) 
          CONZ(ICCN,9)=CMPLX(R1,X1)+CMPLX(RN1,XN1)

C Calculate the low-voltage side impedance values.
C Given by Z2+Zn2,ph-n and Zn2 1x(3x3 array).     
          CONZ(ICCN,10)=CMPLX(R2,X2)+CMPLX(RN2,XN2)
          CONZ(ICCN,11)=CMPLX(RN2,XN2)
          CONZ(ICCN,12)=CMPLX(RN2,XN2)
          CONZ(ICCN,13)=CMPLX(RN2,XN2)
          CONZ(ICCN,14)=CMPLX(R2,X2)+CMPLX(RN2,XN2) 
          CONZ(ICCN,15)=CMPLX(RN2,XN2) 
          CONZ(ICCN,16)=CMPLX(RN2,XN2) 
          CONZ(ICCN,17)=CMPLX(RN2,XN2) 
          CONZ(ICCN,18)=CMPLX(R2,X2)+CMPLX(RN2,XN2)

        ELSE
          WRITE(OUTS,'(A,I3)') 
     &  'Subroutine: ENETCBAZ, unsupported connector type, component',
     &  CONECOMNO(ICCN)
          CALL EDISP(IUOUT,OUTS)
          RETURN              

        ENDIF 

C Trace
        IF(ITRACE(2).GT.0) THEN

C Write out the calculated impedance values
          WRITE(ITU,*)' '

          WRITE(ITU,*)' Calculated impedance values for component',ICCN
          WRITE(ITU,*)' Component type',CONECOMID(ICCN)
          WRITE(ITU,*)' No. of calculated impedance values',NCONZ(ICCN)
          INC=2
          IF(NCONZ(ICCN).EQ.1) INC=0
          IF(NCONZ(ICCN).EQ.4) INC=1
          IF(NCONZ(ICCN).EQ.9) INC=2
          IDLC=1
22        WRITE(ITU,1116)
     &    (REAL(CONZ(ICCN,I)),AIMAG(CONZ(ICCN,I)),I=IDLC,IDLC+INC)
1116      FORMAT(1X,6(2X,G12.5))
          IDLC=IDLC+INC+1
          IF((IDLC+INC).GT.NCONZ(ICCN)) 
     &    INC=NCONZ(ICCN)-IDLC
          IF(IDLC.LE.NCONZ(ICCN).AND.IDLC.LT.MZARRY) GOTO 22 
          ENDIF
 10   CONTINUE  
      RETURN 

C Error message
 1001 WRITE(OUTS,*) 
     &'ERROR: ENETCBAZ incorrect no. of data items specified for conn '
     &,ICCN
      CALL EDISP(itu,outs)
      CALL EPWAIT
      call epagend
      STOP

      END

C ******************************* ENETZ2PU  *******************************************
C This routine calculates the per-unit impedance for each connection in the 
C network, based on the impedance characteristics of the connecting component
C and the impedance base values of the connected nodes. 
C The per-unit conversion is as follows: per-unit impedance = impedance/node 
C base impedance. The node being the start node of the connection. The nodal 
C impedance base value is given by:
C   nodal impedance base = voltage base^2 /network power base. 
C In the case of a transformer, the impedances on the high and low voltage side of 
C the transformer are divided by different base values of the terminating high and low
C voltage nodes:
C   LV pu impedance = LV impedance/LV node base impedance;
C   HV pu impedance = HV impedance/HV node base impedance.
C
C Variables introduced in this routine:
C ENODZBAS - the nodal impedance base value. 
C CONZPU - the connection per-unit impedance values a 1, 4 or 9-element
C          matrix:
C                    component SI impedance  connection per-unit 
C                                               impedance       
C          1-phase ax -- Zax,bx -- bx --> [z(a,b)] (z = Z pu)
C
C          2-phase a1 -Za1,b1 Za1,b2- --> [z(a1,b1) z(a1,b2) ]
C                  a2 -Za2,b1 Za2,b2- --> [z(a2,b1) z(a2,b2) ]
C
C          3-phase a1 -Za1,b1 Za1,b2 Za1,b3- b1 --> [z(a1,b1) z(a1,b2) z(a1,b3)]
C                  a2 -Za2,b1 Za2,b2 Za2,b3- b2 --> [z(a2,b1) z(a2,b2) z(a2,b3)]
C                  a3 -Za2,b1 Za2,b2 Za2,b3- b3 --> [z(a3,b1) z(a3,b2) z(a3,b3)]
C
C For a transformer (e.g. 3-phase):
C                    component SI impedance
C          3-phase a1 -Zha1,b1 Zha1,b2 Zha1,b3 | Zla1,b1 Zla1,b2 Zla1,b3 - b1 
C                  a2 -Zha2,b1 Zha2,b2 Zha2,b3 | Zla2,b1 Zla2,b2 Zla2,b3 - b2 
C                  a3 -Zha2,b1 Zha2,b2 Zha2,b3 | Zla2,b1 Zla2,b2 Zla2,b3 - b3 
C                    connection per-unit impedance
C               --> [zh(a1,b1)+zl(a1,b1) zh(a1,b2)+zl(a1,b2) zh(a1,b3)+zl(a1,b3)]
C               --> [zh(a2,b1)+zl(a2,b1) zh(a2,b2)+zl(a2,b2) zh(a2,b3)+zl(a2,b3)]
C               --> [zh(a3,b1)+zl(a3,b1) zh(a3,b2)+zl(a3,b2) zh(a3,b3)+zl(a3,b3)]
C ***********************************************************************************

      SUBROUTINE ENETZ2PU

#include "power.h"
#include "building.h"

C Trace
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

      COMMON/OUTIN/IUOUT,IUIN

C The electrical network
      COMMON/ENET/ENTYPE

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP

C The connecting components
      COMMON/CONCOM/NCONECOM,CONECOMNO(MCONECOM),CONECOMID(MCONECOM),
     &CONECOMPHTYP(MCONECOM),NCONECOMDAT(MCONECOM),
     &CONECOMDAT(MCONECOM,MCCDAT)

C The calculated component impedances
      COMMON/ECONZVAL/NCONZ(MCONECOM),CONZ(MCONECOM,MZARRY)

C The per unit connection impedances and admittances
      COMMON/ECONZYPU/NCONZPU(MECON),CONZPU(MECON,MZARRY),
     &CONYPU(MECON,MYARRY)

C The electrical network topology
      COMMON/ECONN/NECON,ECONO(MECON),CONPHTYP(MECON),
     &CONPH(MECON,MPHAS),CCNO(MECON),SENOD(MECON,MPHAS),
     &EENOD(MECON,MPHAS)

C Derived System and Nodal Values
      COMMON/ENETSMC/LNPWRBAS,ENODZBAS(MENOD),ENODIBAS(MENOD)

      REAL CONECOMDAT,ENODBASEV,ENODBASEP,LNPWRBAS,ENODIBAS

      INTEGER NENOD,ENODNO,ENODPHTYP,ENODPH,ENODTYP,NCONECOM,CONECOMNO,
     &CONECOMID,CONECOMPHTYP,NCONECOMDAT,NCONZ,NCONZPU,NECON,ECONO,
     &CONPHTYP,CONPH,CCNO,SENOD,EENOD,MAXPH,ENTYPE

      COMPLEX CONZ,CONZPU,CONYPU

      LOGICAL CLOSE

      CHARACTER*124 OUTS

C Firstly calculate the appropriate base power value (line-neutral in 
C an n-phase system)
      IF(ENTYPE.EQ.1.OR.ENTYPE.EQ.2.OR.ENTYPE.EQ.4) THEN
        MAXPH=1
      ELSEIF(ENTYPE.EQ.3) THEN
C Check for the maximum phase no.
       MAXPH=2
        DO 3 INOD=1,NENOD
          IF(ENODPHTYP(INOD).EQ.4) MAXPH=3
  3     CONTINUE

      ELSEIF(ENTYPE.EQ.5) THEN
C Check for the maximum phase no.
        MAXPH=1
        DO 5 INOD=1,NENOD
          IF(ENODPHTYP(INOD).EQ.3.AND.MAXPH.LE.2) MAXPH=2
          IF(ENODPHTYP(INOD).EQ.4) MAXPH=3
  5     CONTINUE

      ENDIF

      LNPWRBAS=ENODBASEP/FLOAT(MAXPH)

C Calculate all the nodal impedance and current base values. 
      DO 10 INOD=1,NENOD
        IF(ENODBASEP.GT.0.0) THEN
          ENODZBAS(INOD)=ENODBASEV(INOD)**2./LNPWRBAS
          ENODIBAS(INOD)=ENODBASEV(INOD)/ENODZBAS(INOD)
        ELSE
          CALL EDISP(IUOUT,'Base power value not set - using 1000.0 as')
          CALL EDISP(IUOUT,'the default system power base. ')
          ENODBASEP=1000.0
          LNPWRBAS=ENODBASEP/FLOAT(MAXPH)
          ENODZBAS(INOD)=ENODBASEV(INOD)**2./LNPWRBAS
        ENDIF
  10  CONTINUE

C Loop through each connection and calculate its per-unit impedance
      DO 20 ICON=1,NECON

C Determine the connecting component and its ID
        ICCN=CCNO(ICON)
        ICID=CONECOMID(ICCN)

C Get the number of impedance array elements
        NZEL=NCONZ(ICCN)

C Begin to calculate the connection Z per unit value. 
        IF(ICID.LT.10) THEN

C Check the number of impedance elements is correct for this connection. 
C 1-phase, d.c. or balanced.
          IF(CONPHTYP(ICON).EQ.1.OR.CONPHTYP(ICON).EQ.2
     &  .OR.CONPHTYP(ICON).EQ.2) THEN
            IF(NZEL.NE.1)THEN
              CALL EDISP(IUOUT,'Error: ENETZ2PU there is a mismatch ') 
              CALL EDISP(IUOUT,'between the connecting component phase') 
              CALL EDISP(IUOUT,'type and the connection phase type ') 
              WRITE(OUTS,'(A,I3)') 'in connection ',ICON
              CALL EDISP(IUOUT,OUTS)
              CALL EPWAIT
              call epagend
              STOP
            ENDIF

C 2-phase
          ELSEIF(CONPHTYP(ICON).EQ.3) THEN
            IF(NZEL.NE.4)THEN
              CALL EDISP(IUOUT,'Error: ENETZ2PU there is a mismatch ') 
              CALL EDISP(IUOUT,'between the connecting component phase') 
              CALL EDISP(IUOUT,'type and the connection phase type ') 
              WRITE(OUTS,'(A,I3)') 'in connection ',ICON
              CALL EDISP(IUOUT,OUTS)
              CALL EPWAIT
              call epagend
              STOP
            ENDIF

C 3-phase
          ELSEIF(CONPHTYP(ICON).EQ.4) THEN
            IF(NZEL.NE.9)THEN
              CALL EDISP(IUOUT,'Error: ENETZ2PU there is a mismatch ') 
              CALL EDISP(IUOUT,'between the connecting component phase') 
              CALL EDISP(IUOUT,'type and the connection phase type ') 
              WRITE(OUTS,'(A,I3)') 'in connection ',ICON
              CALL EDISP(IUOUT,OUTS)
              CALL EPWAIT
              call epagend
              STOP
            ENDIF
          ENDIF

C Set the number of connection impedance elements. 
          NCONZPU(ICON)=NZEL
          IC=1
          DO 30 IPHAS=1,MPHAS
            IF(SENOD(ICON,IPHAS).NE.0)THEN

C Check that the base value on each side of the connection is the same
C if so divide by the start node base value.
              CALL ECLOSE(ENODBASEV(SENOD(ICON,IPHAS)),
     &ENODBASEV(EENOD(ICON,IPHAS)),0.01,CLOSE) 
              IF(.NOT.CLOSE) THEN
                CALL EDISP(IUOUT,'Error: ENETZ2PU there is a mismatch ') 
                CALL EDISP(IUOUT,'between base voltages of the start') 
                WRITE(OUTS,'(A,I3)')' and end nodes in connection.',
     &          ICON
                CALL EDISP(IUOUT,OUTS)
                CALL EPWAIT
                call epagend
                STOP            
              ELSE
                DO 40 IPHAS2=1,MPHAS
                  IF(SENOD(ICON,IPHAS2).NE.0) THEN
                    CONZPU(ICON,IC)=CONZ(ICCN,IC)/
     &              ENODZBAS(SENOD(ICON,IPHAS2))
                    IC=IC+1
                  ENDIF
  40            CONTINUE                                          
              ENDIF  
            ENDIF  
  30      CONTINUE
        ELSEIF(ICID.GE.10.AND.ICID.LE.20) THEN

C Check the number of impedance elements is correct for this connection. 
C 1-phase, d.c. or balanced.
          IF(CONPHTYP(ICON).EQ.1.OR.CONPHTYP(ICON).EQ.2
     &  .OR.CONPHTYP(ICON).EQ.2) THEN
            IF(NZEL.NE.2)THEN
              CALL EDISP(IUOUT,'Error: ENETZ2PU there is a mismatch ') 
              CALL EDISP(IUOUT,'between the connecting component phase') 
              CALL EDISP(IUOUT,'type and the connection phase type. ') 
              WRITE(OUTS,'(A,I3)') 'in connection ',ICON
              CALL EDISP(IUOUT,OUTS)
              CALL EPWAIT
              call epagend
              STOP
            ENDIF
C 2-phase
          ELSEIF(CONPHTYP(ICON).EQ.3) THEN
            IF(NZEL.NE.8)THEN
              CALL EDISP(IUOUT,'Error: ENETZ2PU there is a mismatch ') 
              CALL EDISP(IUOUT,'between the connecting component phase') 
              CALL EDISP(IUOUT,'type and the connection phase type. ') 
              WRITE(OUTS,'(A,I3)') 'in connection ',ICON
              CALL EDISP(IUOUT,OUTS)
              CALL EPWAIT
              call epagend
              STOP
            ENDIF
C 3-phase
          ELSEIF(CONPHTYP(ICON).EQ.4) THEN
            IF(NZEL.NE.18)THEN
              CALL EDISP(IUOUT,'Error: ENETZ2PU there is a mismatch ') 
              CALL EDISP(IUOUT,'between the connecting component phase') 
              CALL EDISP(IUOUT,'type and the connection phase type. ') 
              WRITE(OUTS,'(A,I3)') 'in connection ',ICON
              CALL EDISP(IUOUT,OUTS)
              CALL EPWAIT
              call epagend
              STOP
            ENDIF
          ENDIF

C Set the number of connection impedance elements. 
          NCONZPU(ICON)=NZEL/2
          IH=1
          IL=9
          DO 50 IPHAS=1,MPHAS
            IF(SENOD(ICON,IPHAS).NE.0)THEN     
      
C Check that the base value on each side of the connection is the same
C if so divide by the start node base value.
              IF(ENODBASEV(SENOD(ICON,IPHAS)).LT.
     &ENODBASEV(EENOD(ICON,IPHAS))) THEN
                CALL EDISP(IUOUT,'Error: ENETZ2PU the HV base value ') 
                CALL EDISP(IUOUT,'is less than the LV base value ') 
                WRITE(OUTS,'(A,I3)')'in connection',ICON
                CALL EDISP(IUOUT,OUTS) 
                CALL EPWAIT
                call epagend
                STOP            
              ELSE
                DO 60 IPHAS2=1,MPHAS
                  IF(SENOD(ICON,IPHAS2).NE.0)THEN
                    ENODZH=ENODZBAS(SENOD(ICON,IPHAS2))
                    ENODZL=ENODZBAS(EENOD(ICON,IPHAS2))
                    CONZPU(ICON,IH)=(CONZ(ICCN,IH)/ENODZH)+
     &              (CONZ(ICCN,IL)/ENODZL)   
                    IH=IH+1
                    IL=IL+1
                  ENDIF
  60            CONTINUE                                          
              ENDIF    
            ENDIF
  50      CONTINUE

C Calculation for simple controlled component (switch)
        ELSEIF(ICID.GE.20.AND.ICID.LE.30) THEN

C Check the number of impedance elements is correct for this connection. 
C 1-phase, d.c. or balanced.
          IF(CONPHTYP(ICON).EQ.1.OR.CONPHTYP(ICON).EQ.2
     &  .OR.CONPHTYP(ICON).EQ.2) THEN
            IF(NZEL.NE.1)THEN
              CALL EDISP(IUOUT,'Error: ENETZ2PU there is a mismatch ') 
              CALL EDISP(IUOUT,'between the connecting component phase') 
              CALL EDISP(IUOUT,'type and the connection phase type ') 
              WRITE(OUTS,'(A,I3)') 'in connection ',ICON
              CALL EDISP(IUOUT,OUTS)
              CALL EPWAIT
              call epagend
              STOP
            ENDIF
C 2-phase
          ELSEIF(CONPHTYP(ICON).EQ.3) THEN
            IF(NZEL.NE.4)THEN
              CALL EDISP(IUOUT,'Error: ENETZ2PU there is a mismatch ') 
              CALL EDISP(IUOUT,'between the connecting component phase') 
              CALL EDISP(IUOUT,'type and the connection phase type ') 
              WRITE(OUTS,'(A,I3)') 'in connection ',ICON
              CALL EDISP(IUOUT,OUTS)
              CALL EPWAIT
              call epagend
              STOP
            ENDIF
C 3-phase
          ELSEIF(CONPHTYP(ICON).EQ.4) THEN
            IF(NZEL.NE.9)THEN
              CALL EDISP(IUOUT,'Error: ENETZ2PU there is a mismatch ') 
              CALL EDISP(IUOUT,'between the connecting component phase') 
              CALL EDISP(IUOUT,'type and the connection phase type ') 
              WRITE(OUTS,'(A,I3)') 'in connection ',ICON
              CALL EDISP(IUOUT,OUTS)
              CALL EPWAIT
              call epagend
              STOP
            ENDIF
          ENDIF

C Set the number of connection impedance elements. 
          NCONZPU(ICON)=NZEL
          IC=1
          DO 90 IPHAS=1,MPHAS
            IF(SENOD(ICON,IPHAS).NE.0)THEN
C Check that the base value on each side of the connection is the same
C if so divide by the start node base value.
              CALL ECLOSE(ENODBASEV(SENOD(ICON,IPHAS)),
     &ENODBASEV(EENOD(ICON,IPHAS)),0.01,CLOSE) 
              IF(CLOSE) THEN
                CALL EDISP(IUOUT,'Error: ENETZ2PU there is a mismatch ') 
                CALL EDISP(IUOUT,'between the base voltages of the ') 
                WRITE(OUTS,'(A,I3)')'start and end nodes in connection',
     &          ICON
                CALL EDISP(IUOUT,OUTS)
                call epwait
                call epagend
                STOP            
              ELSE
                DO 100 IPHAS2=1,MPHAS
                  IF(SENOD(ICON,IPHAS2).NE.0)THEN
                    CONZPU(ICON,IC)=CONZ(ICCN,IC)/
     &              ENODZBAS(SENOD(ICON,IPHAS2))
                    IC=IC+1
                  ENDIF
  100           CONTINUE                                          
              ENDIF   
            ENDIF 
  90      CONTINUE

          
C Calculation for converting component (ac/dc dc/ac etc.)
        ELSEIF(ICID.GE.30.AND.ICID.LE.40) THEN
C << no components of this type supported yet >>
          CONTINUE
        ENDIF


  20  CONTINUE

C Trace 
      IF(ITRACE(2).GT.0) THEN
      WRITE(ITU,*) ' '
      WRITE(ITU,*) 'Trace subroutine ENETZ2PU'
      DO 110 ICN=1,NECON
        WRITE(ITU,*) 'Connection ',ICN
        WRITE(ITU,*) '             Zpu real    | Zpu imag  '   
        DO 120 IDAT=1,NCONZPU(ICN)
          WRITE(ITU,*) 
     &'Mtrx element:', IDAT,' ', REAL(CONZPU(ICN,IDAT)),
     &AIMAG(CONZPU(ICN,IDAT))          
 120    CONTINUE
 110  CONTINUE
      ENDIF

      RETURN
      END
      
C ******************************* ENETCPUY  *******************************************
C This routine calculates the PU admittance characteristics of each connection in the 
C network. The per-unit admittance of the connection is the inverse of its 
C impedance matrix. 

      SUBROUTINE ENETCPUY

#include "power.h"
#include "building.h"

C Trace
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

C The electrical network topology
      COMMON/ECONN/NECON,ECONO(MECON),CONPHTYP(MECON),
     &CONPH(MECON,MPHAS),CCNO(MECON),SENOD(MECON,MPHAS),
     &EENOD(MECON,MPHAS)

C The per unit connection impedances and admittances
      COMMON/ECONZYPU/NCONZPU(MECON),CONZPU(MECON,MZARRY),
     &CONYPU(MECON,MYARRY)

C Type casting (all variables and arrays cast explicitly)
      INTEGER NCONZPU,NECON,ECONO,CONPHTYP,CONPH,CCNO,INC,SENOD,
     &EENOD,I,J,K,IDLC

C Dimension temporary 3x3 and 2x2 arrays to pass into the inversion routine
      DIMENSION TPUY3(MPHAS,MPHAS),TPUYI3(MPHAS,MPHAS),
     &TPROD3(MPHAS,MPHAS),TPUY2(MPHAS-1,MPHAS-1),
     &TPUYI2(MPHAS-1,MPHAS-1),TPROD2(MPHAS-1,MPHAS-1)

      COMPLEX CONZPU,CONYPU,TPUY3,TPUYI3,TPROD3,TPUY2,TPUYI2,TPROD2

C Loop through each component and invert the impedance value arrays. 
C Arrays are either 1x1 [1], 2x2 [4] or 3x3 [9]. 

      DO 10 ICON=1,NECON

        IF(NCONZPU(ICON).EQ.1) THEN

C Simple inversion of a single impedance value.
           CONYPU(ICON,1)=CMPLX(1.0,0.0)/CONZPU(ICON,1)
        ELSEIF(NCONZPU(ICON).EQ.4.OR.NCONZPU(ICON).EQ.9) THEN

C Inversion of 2x2 and 3x3 impedance elements. 
           IF(NCONZPU(ICON).EQ.4) THEN
             NE=2
           ELSEIF(NCONZPU(ICON).EQ.9) THEN
             NE=3
           ENDIF
           J=1
           K=1 
           DO 20 I=1,NCONZPU(ICON) 
            
C Assign the values of the existing per unit impedance values to the
C temporary NxN array. 
              IF(NE.EQ.2) THEN
                TPUY2(J,K)=CONZPU(ICON,I)
              ELSE IF(NE.EQ.3) THEN
                TPUY3(J,K)=CONZPU(ICON,I)
              ENDIF
              K=K+1
              IF(K.GT.NE) THEN
                K=1
                J=J+1
              ENDIF
  20       CONTINUE

C Invert the TPUY array to get the actual admittance values.
           IF(NE.EQ.2) THEN
             CALL CINVRLU(TPUY2,TPUYI2,NE)
           ELSEIF(NE.EQ.3) THEN
             CALL CINVRLU(TPUY3,TPUYI3,NE)
           ENDIF

C Trace ...
           DO 31 IROW=1,NE
             DO 32 JCOL=1,NE
               DO 33 IELM=1,NE
                 IF(NE.EQ.2) THEN
                   TPROD2(IROW,JCOL)=TPROD2(IROW,JCOL)+
     &TPUY2(IROW,IELM)*TPUYI2(IELM,JCOL)
                 ELSEIF(NE.EQ.3) THEN
                   TPROD3(IROW,JCOL)=TPROD3(IROW,JCOL)+
     &TPUY3(IROW,IELM)*TPUYI3(IELM,JCOL)
                 ENDIF
  33           CONTINUE
  32         CONTINUE
  31       CONTINUE

C Assign the elements of TPUYI to the per unit admittance array. 
           K=1
           J=1
           DO 30 I=1,NE
             DO 40 J=1,NE
               IF(NE.EQ.2)THEN
                 CONYPU(ICON,K)=TPUYI2(I,J)
               ELSEIF(NE.EQ.3)THEN
                 CONYPU(ICON,K)=TPUYI3(I,J)
               ENDIF
               K=K+1
  40         CONTINUE
  30       CONTINUE

C Clear Temporary arrays.
           DO 44 IROW=1,NE
             DO 45 JCOL=1,NE
               DO 46 IELM=1,NE
                 IF(NE.EQ.2) THEN
                   TPUY2(IROW,JCOL)=CMPLX(0.0,0.0)
                   TPUYI2(IROW,JCOL)=CMPLX(0.0,0.0)
                   TPROD2(IROW,JCOL)=CMPLX(0.0,0.0)
                 ELSEIF(NE.EQ.3)THEN
                   TPUY3(IROW,JCOL)=CMPLX(0.0,0.0)
                   TPUYI3(IROW,JCOL)=CMPLX(0.0,0.0)
                   TPROD3(IROW,JCOL)=CMPLX(0.0,0.0)
                 ENDIF
  46           CONTINUE
  45         CONTINUE
  44       CONTINUE
             
        ENDIF
C Trace 
        IF(ITRACE(2).GT.0) THEN

C Set the inc value for write out.
          IF(NCONZPU(ICON).EQ.1) INC=0
          IF(NCONZPU(ICON).EQ.4) INC=1
          IF(NCONZPU(ICON).EQ.9) INC=2
          WRITE(ITU,*) ' '
          WRITE(ITU,*) 'Subroutine ENETCPUY' 
          WRITE(ITU,*) 
     &'Inverting impedance values of connecting components'
          WRITE(ITU,*) ' '
          WRITE(ITU,*)'Connection: ',ICON
          WRITE(ITU,*)'Connecting component impedance characteristics..'
          WRITE(ITU,*)'R per unit |  X per unit        '
          IDLC=1
21        WRITE(ITU,1111)
     &    (REAL(CONZPU(ICON,I)),AIMAG(CONZPU(ICON,I)),I=IDLC,IDLC+INC)
          IDLC=IDLC+INC+1
          IF((IDLC+INC).GT.NCONZPU(ICON)) 
     &    INC=NCONZPU(ICON)-IDLC
          IF(IDLC.LE.NCONZPU(ICON).AND.IDLC.LT.MZARRY) GOTO 21

C Reset inc value.
          IF(NCONZPU(ICON).EQ.1) INC=0
          IF(NCONZPU(ICON).EQ.4) INC=1
          IF(NCONZPU(ICON).EQ.9) INC=2

          WRITE(ITU,*) 'Calculated admittances ....   '
          WRITE(ITU,*)'Y = a + jb (per-unit)'        
          IDLC=1
22        WRITE(ITU,1111)
     &  (REAL(CONYPU(ICON,I)),AIMAG(CONYPU(ICON,I)),I=IDLC,IDLC+INC)
1111      FORMAT(1X,3(F10.3,2X,F10.3))
          IDLC=IDLC+INC+1
          IF((IDLC+INC).GT.NCONZPU(ICON)) 
     &    INC=NCONZPU(ICON)-IDLC
          IF(IDLC.LE.NCONZPU(ICON).AND.IDLC.LT.MYARRY) GOTO 22
        ENDIF  

            

  10  CONTINUE
      IF(ITRACE(2).GT.0) THEN
        WRITE(ITU,*)' '
        WRITE(ITU,*)'Leaving ENETCPUY' 
        WRITE(ITU,*)' ' 
      ENDIF

      RETURN
      END
C ******************************* ENETADMY *******************************************
C This subroutine calculates the driving port admittances and transfer admitances for
C each node in the network, these are constructed from the values caculated for each
C connection in ENETCPUY. The calculated admittances are as follows:
C Yii = sum Yij (sum of admittances connected to the node)
C Yij = -Yij negative of the admittance connected node
C
C If the node is part of a multi-phase set of nodes and  multi-phase components
C connect to the node then extra admittance elements need to be added:
C Yix =  sum Ymk where Ymk is the sum of all the mutual admittance values 
C        to phase k of connected multi-phase components, x is the node of the
C        multi-phase set associated with phase k
C Yif =  -Ym where Ym is the mutual admittance between between the phase assoc. with 
C        node i and phase k, f is the node at the other end of the conductor associated 
C        with the same phase.
C        .
C        .
C Yiy =  sum Ymn where Ymn is the sum of all the mutual admittance values 
C        to phase n of connected multi-phase components, y is the node of the
C        multi-phase set associated with phase n. 
C Yih =  -Ym where Ym is the mutual admittance between the phase assoc. with node i 
C         and phase k, f is the node at the other end of the conductor associated 
C         with the same phase.
C         (note k....n <> i)
C
C ************************************************************************************
C Variables introduved in this routine are as follows:
C NYELM - the number of elements in the admittance array
C         (equal to the NENOD*NENOD).
C YADM(I,J) - an enntry in the admittance matrix

      SUBROUTINE ENETADMY

#include "power.h"
#include "building.h"

C Trace
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

C The electrical nodes
      COMMON/ENOD/NENOD,ENODNO(MENOD),ENODPHTYP(MENOD),
     &ENODPH(MENOD),ENODTYP(MENOD),ENODBASEV(MENOD),
     &ENODBASEP

C The electrical network topology
      COMMON/ECONN/NECON,ECONO(MECON),CONPHTYP(MECON),
     &CONPH(MECON,MPHAS),CCNO(MECON),SENOD(MECON,MPHAS),
     &EENOD(MECON,MPHAS)

C The per unit connection impedance and admittances
      COMMON/ECONZYPU/NCONZPU(MECON),CONZPU(MECON,MZARRY),
     &CONYPU(MECON,MYARRY)

C The array of driving port and transfer admittances
      COMMON/YARRY/NYELM,YADM(MENOD,MENOD)

C Variable casting
      INTEGER NENOD,ENODNO,ENODPHTYP,ENODPH,ENODTYP, NECON,ECONO,
     &CONPHTYP,CONPH,CCNO,SENOD,EENOD,NCONZPU,NYELM,ICON,IC,IMEL,
     &JMEL,IPH1,IPH2

      REAL ENODBASEV,ENODBASEP

      COMPLEX CONZPU,CONYPU,YADM

      LOGICAL CLOSER,CLOSEI

C Set the number of admittance array elements 
      NYELM=NENOD*NENOD

C Loop through each connection.
      DO 10 ICON=1,NECON

C Determine the connecting component.
C Set the pointer to the connecting component admittance array value
        IC=1

C Loop through each phase
        DO 20 IPH1=1,MPHAS
          IF(CONPH(ICON,IPH1).NE.0)THEN
            IST=SENOD(ICON,IPH1)
            IEN=EENOD(ICON,IPH1)
            DO  30 IPH2=1,MPHAS
              IF(CONPH(ICON,IPH2).NE.0)THEN
                IF(IPH1.EQ.IPH2)THEN

C Add the admittance of the component to the driving port admittances of the
C start and end nodes of this phase of the component
                  IF(IST.NE.0.AND.IEN.NE.0)THEN
C Driving port admittance for start and end nodes
                    YADM(IST,IST)=YADM(IST,IST)+CONYPU(ICON,IC)
                    YADM(IEN,IEN)=YADM(IEN,IEN)+CONYPU(ICON,IC)

C Branch admittance
                    YADM(IST,IEN)=-CONYPU(ICON,IC)
                    YADM(IEN,IST)=-CONYPU(ICON,IC)

C Increment the admittance array pointer.
                    IC=IC+1
                  ENDIF
                ELSEIF(IPH1.NE.IPH2)THEN

C Now deal with the mutual couplings between conductors. 
                  ISM=SENOD(ICON,IPH2)
                  IEM=EENOD(ICON,IPH2)
                  IF(ISM.NE.0.AND.IEM.NE.0)THEN
                    YADM(IST,ISM)=YADM(IST,ISM)+CONYPU(ICON,IC)
                    YADM(IEN,IEM)=YADM(IEN,IEM)+CONYPU(ICON,IC)
                    YADM(IST,IEM)=-CONYPU(ICON,IC)
                    YADM(IEN,ISM)=-CONYPU(ICON,IC)

C Again increment the admittance array pointer.
                    IC=IC+1
                  ENDIF
                ENDIF
              ENDIF
30          CONTINUE
          ENDIF
20      CONTINUE
10    CONTINUE

C Trace 
      IF(ITRACE(2).GT.0) THEN
        WRITE(ITU,*)' '
        WRITE(ITU,*)'Subroutine ENETADMY '
        WRITE(ITU,*)'The calculated admittance/conductance matrix: '
        DO 40 IMEL=1,NENOD
          DO 50 JMEL=1,NENOD
            WRITE(ITU,1111) 'Coeff: ',IMEL,JMEL,REAL(YADM(IMEL,JMEL)),
     &AIMAG(YADM(IMEL,JMEL))
1111        FORMAT(A,I3,1X,I3,1X,E12.5,1X,E12.5)
            CLOSER=.TRUE.
            CLOSEI=.TRUE.
            IF(IMEL.NE.JMEL) THEN
              A=REAL(YADM(IMEL,JMEL))
              B=REAL(YADM(JMEL,IMEL))
              C=AIMAG(YADM(IMEL,JMEL))
              D=AIMAG(YADM(JMEL,IMEL))
              CALL ECLOSE(A,B,0.5,CLOSER)
              CALL ECLOSE(C,D,0.5,CLOSEI)
              IF(.NOT.CLOSER.OR.(.NOT.CLOSEI)) THEN
                WRITE(ITU,*)'WARNING - matrix possibly not '
                WRITE(ITU,*)'symetrical about leading diagonal.'
                WRITE(ITU,*)'at coeffs ',IMEL,JMEL,' and ',JMEL,IMEL
                WRITE(ITU,*) IMEL,JMEL,': ',YADM(IMEL,JMEL)
                WRITE(ITU,*) JMEL,IMEL,': ',YADM(IMEL,JMEL)              
              ENDIF
            ENDIF
50        CONTINUE
40      CONTINUE

C Check the matrix symetry about the leading diagonal.


        WRITE(ITU,*)' '
        WRITE(ITU,*)'Leaving Subroutine ENETADMY '
        WRITE(ITU,*)' '
      ENDIF

      RETURN
      END
C ******************************* ENETPARAM ********************************************
C This subroutine allows the electrical network simulation parameters to be edited.
C The parameters are as follows:
C ENETMAXI - the maximum number of iterations per simulation time step. 
C ENETVCVG - the voltage convergence criteria for the iterative solution.
C ENETACVG - the apparent power convergence criteria for the iterative solution.
C ENETVLIM - the volyage change limit for each iteration. 
C ENODCVGF - the convergence flag for each node. 
C ENETSLVT - the solver type used for the network 1-Newton-Raphson 2-Gauss Siedel. 
C **************************************************************************************

       SUBROUTINE ENETPARAM

#include "power.h"

      COMMON/ENETSIMP/ENETMAXI,ENETSLVT,ENETVCVG,ENETACVG,ENETVLIM,
     &ENODCVGF(MENOD),CVGCHK

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/POPHELP/H(60)

      INTEGER ENETMAXI,ENETSLVT,ENODCVGF,DEFMAXI
       
      REAL ENETVCVG,ENETACVG,ENETVLIM,DEFVCVG,DEFACVG

      LOGICAL CVGCHK
      CHARACTER*40 ITEM(10)
      CHARACTER*72 H

C Build up a menu allowing the user to edit the simulation parameters
  10  WRITE(ITEM(1),'(A)')'  Select parameter to change:'
      IF(ENETSLVT.EQ.2)THEN      
        WRITE(ITEM(2),'(A,A)')'a solver >>', ' Newton-Raphson no pivot'
      ELSEIF(ENETSLVT.EQ.3)THEN      
        WRITE(ITEM(2),'(A,A)')'a solver >>', ' Newton-Raphson + pivot'
      ELSE
        WRITE(ITEM(2),'(A,A)')'a solver >>', ' Gauss Siedel'  
      ENDIF
      WRITE(ITEM(3),'(A)')' '
      WRITE(ITEM(4),'(A,I3)')  'b max. no. of iterations:   ',ENETMAXI
      WRITE(ITEM(5),'(A,F8.3)')'c voltage converge (V):     ',ENETVCVG
      WRITE(ITEM(6),'(A,F8.3)')'d voltage change limit (V): ',ENETVLIM
      WRITE(ITEM(7),'(A,F8.3)')'e power flow converge (VA): ',ENETACVG
      WRITE(ITEM(8),'(A)')' ----------------------    '
      WRITE(ITEM(9),'(A)')' ? Help                    '
      WRITE(ITEM(10),'(A)')' - Exit                   '
      INO=-1
      NITMS=10

C Help text for this menu.
      H(1)='The parameters displayed in this menu allow the user to'
      H(2)='customise the solution of the electrical network by '
      H(3)='altering the convergence criteria and the solver    '
      H(4)='employed. If in doubt the user is advised to use the '
      H(5)='parameters.'

C Build up the menu
      CALL EMENU(' Electrical Parameters',ITEM,NITMS,INO)
      IF(INO.EQ.2) THEN

C Alter the solver type
        H(1)='You can select the solver for use in the power flow '
        H(2)='simulation, however it is recommended that you use the '
        H(3)='faster Newton-Raphson solver. The Gauss-Siedel solver is '
        H(4)='mainly used for testing purposes.'
        CALL EASKABCD('Select the solver you want to use.',
     &'The Newton-Raphson (w. pivoting) solver is recommended.',
     &'Newton-Raph.(w. pivoting)','Newton-Raphson','Gauss-Siedel',
     &'Continue',IW,4)
        IF(IW.EQ.1) THEN
          ENETSLVT=3
        ELSEIF(IW.EQ.2) THEN
          ENETSLVT=2
        ELSEIF(IW.EQ.3) THEN
          ENETSLVT=1
        ELSE
          GOTO 10
        ENDIF
      ELSEIF(INO.EQ.4)THEN

C Set the maximum number of iterations. 
        IF(ENETSLVT.EQ.1) DEFMAXI=500
        IF(ENETSLVT.EQ.2.OR.ENETSLVT.EQ.3) DEFMAXI=100
        H(1)='Please enter the maxumum number if iterations. If the'
        H(2)='number of iterations exceeds this value then the     '
        H(3)='network solution will fail and a warning will be     '
        H(4)='displayed.                                           '
        CALL EASKI(ENETMAXI,'Enter the maximum number of iterations',
     &'(if in doubt select the default)',1,'F',500,'W',
     &DEFMAXI,'maxiter',IER,4)
      ELSEIF(INO.EQ.5)THEN

C Set criteria for convergence in terms of voltage (SI V). 
        DEFVCVG=0.1
        H(1)='Please enter the voltage change between iterations   '
        H(2)='at which the solution can be considered to have      '
        H(3)='converged.                                           '
        CALL EASKR(ENETVCVG,'Enter the voltage convergence criteria',
     &'(if in doubt select the default)',0.,'F',0.,'-',DEFVCVG,
     &'vcvg',IER,3)          
      ELSEIF(INO.EQ.6)THEN

C Set the maximum voltage change between iterations (SI V). 
        DEFVLIM=5.0
        H(1)='Please enter the maximum voltage change allowable   '
        H(2)='between iterations.                                 '
        CALL EASKR(ENETVLIM,'Enter the voltage convergence criteria',
     &'(if in doubt select the default)',0.,'F',0.,'-',DEFVLIM,
     &'vlim',IER,2)
      ELSEIF(INO.EQ.7)THEN

C Set criteria for convergence in terms of apparent power at each node (SI VA).
        DEFACVG=10.
        H(1)='Please enter the apparent power flow change between  '
        H(2)='iterations at which the solution can be considered to '
        H(3)='have converged.                                       '
        CALL EASKR(ENETACVG,'Enter the voltage convergence criteria',
     &'(if in doubt select the default)',0.,'F',0.,'-',DEFACVG,
     &'acvg',IER,3)
      ELSEIF(INO.EQ.9)THEN
        CALL PHELPD('netwk sol param',5,'-',0,0,IER)
      ELSEIF(INO.EQ.10)THEN
        RETURN    
      ELSE
        GOTO 10
      ENDIF
      GOTO 10

      END

