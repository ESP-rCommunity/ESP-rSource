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

C At each simulation time step the routines in this file transfer the electrical power
C information calculated elsewhere in the ESP-r model to the electrical network.
C The routines are as follows:
C ENETMODL - calculates the output of any electrical "power only" models coupled to
C            the electrical network <<to be added>>.
C ENETASGN - this routine assugns the boundary conditions from building; plant; power
C            only components to the appropriate node in the electrical network. 
C ENETB2PU - this routine converts the boundary power flows to per-unit. 
C 
C ******************************* ENETASGN *******************************************
C 
C This routine assigns the real and reactive electrical power drawn or injected into 
C nodes in an electrical network. Power drawn from the network is assigned a negative 
C value while power injected into the network is assigned a positive value. 
C Reactive power associated with and indictive load (jQL) is assigned a positive 
C value, while power associated with a capacitive load is assigned a negative value
C -jQC. 
C The various possible loads and sources are therfore as follows:
C
C                          LOAD          SOURCE
C Resistive                -P            P
C
C Reactive                 -jQ           jQ
C
C Capacitive               -(-jQ)        -jQ
C
C Mixed
C (resistive+reactive)     -(P+jQ)       P+jQ

C (resistive+capacitive)   -(P-jQ)       P-jQ
C
C The positive or negative values are assigned by the individual hybrid components.
C Components which can draw from/feed into the network are:
C
C 1. Lighting and power loads in building zones. 
C 2. Power consuming plant components  (Fans, pumps, heaters, compressors, etc.).
C 3. Power generating plant components (CHP).
C 4. Photvoltaic special materials.
C 5. Power only components (wind turbines, remote power sources, etc.)
C 6. <<further additions>> ......
C
C ******************************* ENETASGN *******************************************
C 
C This routine assigns the real and reactive electrical power drawn or injected into 
C nodes in an electrical network. Power drawn from the network is assigned a negative 
C value while power injected into the network is assigned a positive value. 
C Reactive power associated with and indictive load (jQL) is assigned a positive 
C value, while power associated with a capacitive load is assigned a negative value
C -jQC. 
C The various possible loads and sources are therfore as follows:
C
C                        LOAD       SOURCE
C Resistive              -P         P
C
C Reactive               -jQ        jQ
C
C Capacitive             -(-jQ)     -jQ
C
C Mixed
C (resistive+reactive)   -(P+jQ)     P+jQ

C (resistive+capacitive) -(P-jQ)     P-jQ
C
C The positive or negative values are assigned by the individual hybrid components.
C Components which can draw from/feed into the network are:
C
C 1. Lighting and power loads in building zones. 
C 2. Power consuming plant components  (Fans, pumps, heaters, compressors, etc.).
C 3. Power generating plant components (CHP).
C 4. Photvoltaic special materials.
C 5. Power only components (wind turbines, remote power sources, etc.)
C 6. <<further additions>> ......
C
C Variables introduced in this routine:
C
C PEZON  - a zone casual gain type's real power consumption
C QEZON  - a zone casual gain type's reactive power consumption
C PPCMP  - a plant component real power demand/output
C QPCMP  - a plant component reactive power demand/output
C PSPM   - a special material (embedded renewable) real power output
C QSPM   - a special material (embedded renewable) reactive power output
C PPOWOC - a power only component real power demand/output
C QPOWOC - a power only component reactive power demand/output
C PENOD  - the net nodal real power flow (load+generation)
C QENOD  - the net nodal reactive power flow (load+generation)
C PENODL - the nodal real power load
C QENODL - the nodal reactive power load
C PENODG - the nodal real power generation
C QENODG - the nodal reactive power generation
C
C
C************************************** ENETASGN ***************************************

      SUBROUTINE ENETASGN

C Includes
#include "building.h"
#include "plant.h"
#include "power.h"

C Trace
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS

C Relevant network commons

C Type casting
      REAL DIVIS
      
      CHARACTER*124 OUTS

C New timestep so zero the existing nodal boundary power flows. 
      DO 10 IENOD=1,NENOD
        PENOD(IENOD)=0.
        QENOD(IENOD)=0.
        PENODL(IENOD)=0.
        QENODL(IENOD)=0.
        PENODG(IENOD)=0.
        QENODG(IENOD)=0.
        PENODT(IENOD)=0.
        QENODT(IENOD)=0.
  10  CONTINUE

C Loop through each hybrid component in the electrical network and assign the
C power flows to the connected node.
      IF(NHYBCOM.GT.0) THEN
        DO 30 IHYB=1,NHYBCOM

C Assign power flows from hybrid connected components (either
C zones - type 1 or special material - type 2 or plant type 3
          IF(HYCOMTYP(IHYB).EQ.1) THEN       
            IF(HYCOMPHTYP(IHYB).EQ.3) THEN
              DIVIS=2.
            ELSEIF(HYCOMPHTYP(IHYB).EQ.4) THEN
              DIVIS=3.
            ELSE
              DIVIS=1.
            ENDIF
          
C Assign the building side power flows to the network
            DO 35 IPHAS=1,MPHAS

C Assign zone-side power consumptions to the network. Power
C consumptions are connected by type. 
              IF(HYCONEN(IHYB,IPHAS).GT.0) THEN
                IF(PEZON(HYLOC(IHYB,1),HYLOC(IHYB,2)).GT.0.)THEN
                  PENODG(HYCONEN(IHYB,IPHAS))=              
     &PENODG(HYCONEN(IHYB,IPHAS))+
     &PEZON(HYLOC(IHYB,1),HYLOC(IHYB,2))/DIVIS
                  QENODG(HYCONEN(IHYB,IPHAS))=
     &QENODG(HYCONEN(IHYB,IPHAS))+
     &QEZON(HYLOC(IHYB,1),HYLOC(IHYB,2))/DIVIS  
                ELSE
                  PENODL(HYCONEN(IHYB,IPHAS))=              
     &PENODL(HYCONEN(IHYB,IPHAS))+
     &PEZON(HYLOC(IHYB,1),HYLOC(IHYB,2))/DIVIS
                  QENODL(HYCONEN(IHYB,IPHAS))=
     &QENODL(HYCONEN(IHYB,IPHAS))+
     &QEZON(HYLOC(IHYB,1),HYLOC(IHYB,2))/DIVIS 
                ENDIF                
              ENDIF
  35        CONTINUE    

C Apply the special materials (PV) power flows to the network        
          ELSEIF(HYCOMTYP(IHYB).EQ.2) THEN
            IF(HYCOMPHTYP(IHYB).EQ.3) THEN
              DIVIS=2.
            ELSEIF(HYCOMPHTYP(IHYB).EQ.4) THEN
              DIVIS=3.
            ELSE
              DIVIS=1.
            ENDIF
            DO 37 IPHAS=1,MPHAS
              IF(HYCONEN(IHYB,IPHAS).GT.0) THEN
                IF(PSPM(HYLOC(IHYB,1)).GT.0.)THEN
                  PENODG(HYCONEN(IHYB,IPHAS))=
     &PENODG(HYCONEN(IHYB,IPHAS))+PSPM(HYLOC(IHYB,1))/DIVIS
                  QENODG(HYCONEN(IHYB,IPHAS))=
     &QENODG(HYCONEN(IHYB,IPHAS))+QSPM(HYLOC(IHYB,1))/DIVIS
                ELSE
                 PENODL(HYCONEN(IHYB,IPHAS))=
     &PENODL(HYCONEN(IHYB,IPHAS))+PSPM(HYLOC(IHYB,1))/DIVIS
                  QENODL(HYCONEN(IHYB,IPHAS))=
     &QENODL(HYCONEN(IHYB,IPHAS))+QSPM(HYLOC(IHYB,1))/DIVIS
                ENDIF
              ENDIF
  37        CONTINUE  

C Apply the plant power flows to the network. 
          ELSEIF(HYCOMTYP(IHYB).EQ.3) THEN
            IF(HYCOMPHTYP(IHYB).EQ.3) THEN
              DIVIS=2.
            ELSEIF(HYCOMPHTYP(IHYB).EQ.4) THEN
              DIVIS=3.
            ELSE
              DIVIS=1.
            ENDIF

C Add the plant power flows to the nodal power flows. 
            DO 39 IPHAS=1,MPHAS
              IF(HYCONEN(IHYB,IPHAS).GT.0) THEN
                IF(PPCMP(HYLOC(IHYB,1)).GT.0.)THEN
                  PENODG(HYCONEN(IHYB,IPHAS))=
     &PENODG(HYCONEN(IHYB,IPHAS))+PPCMP(HYLOC(IHYB,1))/DIVIS
                  QENODG(HYCONEN(IHYB,IPHAS))=
     &QENODG(HYCONEN(IHYB,IPHAS))+QPCMP(HYLOC(IHYB,1))/DIVIS
                ELSE
                  PENODL(HYCONEN(IHYB,IPHAS))=
     &PENODL(HYCONEN(IHYB,IPHAS))+PPCMP(HYLOC(IHYB,1))/DIVIS
                  QENODL(HYCONEN(IHYB,IPHAS))=
     &QENODL(HYCONEN(IHYB,IPHAS))+QPCMP(HYLOC(IHYB,1))/DIVIS
                ENDIF
              ENDIF
  39        CONTINUE 

C Consider plant components that are connected to a DC and an AC node.
            if ( iplt_conn_nodes(ihyb).eq.2 ) then                !- plant component is connected to 2 nodes (DC/AC)
              if ( iplt_dcnode_id(ihyb) .gt. 0 .or.
     &             iplt_acnode_id(ihyb) .gt. 0 ) then          !- node indices are valid
                if ( pdcpower(hyloc(ihyb,1)) .gt. 0. ) then
                     penodg(iplt_dcnode_id(ihyb)) =
     &penodg(iplt_dcnode_id(ihyb)) + pdcpower(hyloc(ihyb,1))      !- only real power taken into account
                else
                  penodl(iplt_dcnode_id(ihyb)) = 
     &penodl(iplt_dcnode_id(ihyb)) + pdcpower(hyloc(ihyb,1))
                endif
                if ( pacpower(hyloc(ihyb,1)) .gt. 0. ) then
                     penodg(iplt_acnode_id(ihyb)) = 
     &penodg(iplt_acnode_id(ihyb)) + pacpower(hyloc(ihyb,1))
                else
                  penodl(iplt_acnode_id(ihyb)) = 
     &penodl(iplt_acnode_id(ihyb)) + pacpower(hyloc(ihyb,1))
                endif
              endif
            endif

          ELSE

C Write out warning as a non-supported type has been defined.
            WRITE(OUTS,'(2(A,I3))') 
     &'ENETASGN: A non-supported hybrid component type ',
     &HYCOMTYP(IHYB),' has been defined for component ',IHYB
            CALL EDISP(IUOUT,OUTS)
          ENDIF 
  30    CONTINUE  
      ENDIF

C Assign the flows from the power-only components.
      IF(NPOWCOM.GT.0) THEN
        DO 40 IPWC=1,NPOWCOM
          IF(POWCOMPHTYP(IPWC).EQ.3) THEN
            DIVIS=2.
          ELSEIF(POWCOMPHTYP(IPWC).EQ.4) THEN
            DIVIS=3.
          ELSE
            DIVIS=1.
          ENDIF

C Add the power-only power flows to the nodal power flows. 

C Determine the load or generation from the power-only component.
          CALL POWOC_CALC(IPWC)

          DO 43 IPHAS=1,MPHAS
            IF(POWCONEN(IPWC,IPHAS).GT.0) THEN
              IF(PPOWOC(IPWC).GT.0.)THEN
                PENODG(POWCONEN(IPWC,IPHAS))=
     &PENODG(POWCONEN(IPWC,IPHAS))+PPOWOC(IPWC)/DIVIS
                QENODG(POWCONEN(IPWC,IPHAS))=
     &QENODG(POWCONEN(IPWC,IPHAS))+QPOWOC(IPWC)/DIVIS
              ELSE
                PENODL(POWCONEN(IPWC,IPHAS))=
     &PENODL(POWCONEN(IPWC,IPHAS))+PPOWOC(IPWC)/DIVIS
                QENODL(POWCONEN(IPWC,IPHAS))=
     &QENODL(POWCONEN(IPWC,IPHAS))+QPOWOC(IPWC)/DIVIS
              ENDIF
            ENDIF
  43      CONTINUE
  40    CONTINUE
      ENDIF

C Sum the load and generation power flows to get the net boundary power flows. 
      DO 50 IENOD=1,NENOD
        PENOD(IENOD)=PENODL(IENOD)+PENODG(IENOD)
        QENOD(IENOD)=QENODL(IENOD)+QENODG(IENOD)
  50  CONTINUE

C Trace - electrical calcs currently assigned to trace no. 2
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &ITRACE(2).GT.0) THEN
        WRITE(ITU,*)' '
        WRITE(ITU,*)' Subroutine ENETASGN:                        '

C Write out the details 
        WRITE(ITU,*)' Real and reactive power flows at each node  '
        WRITE(ITU,*)' in the electrical network.                  '
        DO 700 INOD=1,NENOD
          WRITE(ITU,*)'Node: ',INOD
          WRITE(ITU,*)'         Real Power        Reactive Power  '
          WRITE(ITU,*)'          (kW)                  (kVAR)     '
          WRITE(ITU,'(10X,F9.5,12X,F9.5)') 
     &PENOD(INOD)/1000.,QENOD(INOD)/1000.
        WRITE(ITU,*)' '        
        
 700    CONTINUE


C Write out the details of ... 
        WRITE(ITU,*)' Power flows in connected hybrid components      '
        IF(NHYBCOM.EQ.0) THEN
          WRITE(ITU,*) 'NONE'
        ELSE 
          DO 707 IHYB=1,NHYBCOM    
            WRITE(ITU,*)'Component: ',IHYB
            WRITE(ITU,*)'Type: ',HYCOMTYP(IHYB)
            WRITE(ITU,*)'         Real Power        Reactive Power  '
            WRITE(ITU,*)'          (kW)                  (kVAR)     '
            IF(HYCOMTYP(IHYB).EQ.1) THEN
              WRITE(ITU,'(10X,F7.3,12X,F7.3)') 
     &PEZON(HYLOC(IHYB,1),HYLOC(IHYB,2))/1000.,
     &QEZON(HYLOC(IHYB,1),HYLOC(IHYB,2))/1000.
            ELSEIF(HYCOMTYP(IHYB).EQ.2) THEN
              WRITE(ITU,'(F7.3,1X,1X,F7.3,1X)') 
     &PPCMP(HYLOC(IHYB,1))/1000.,QPCMP(HYLOC(IHYB,1))/1000.
            ELSEIF(HYCOMTYP(IHYB).EQ.3) THEN
              WRITE(ITU,'(F7.3,1X,1X,F7.3,1X)') 
     &PSPM(HYLOC(IHYB,1))/1000.,QSPM(HYLOC(IHYB,1))/1000.
            ENDIF           
            WRITE(ITU,*)' '        
 707      CONTINUE 
        ENDIF 
      
C Write ou the details of the connectec power only components.
        WRITE(ITU,*)' Power flows in connected power-only components   '
        IF(NPOWCOM.EQ.0) THEN
          WRITE(ITU,*) 'NONE'
        ELSE
          DO 709 IPWC=1,NPOWCOM 
            WRITE(ITU,*)'Component: ',IPWC
            WRITE(ITU,*)'Component i.d.: ',POWCOMID(IPWC)
            WRITE(ITU,*)'         Real Power        Reactive Power  '
            WRITE(ITU,*)'          (kW)                  (kVAR)     '
            WRITE(ITU,'(10X,F7.3,1X,1X,F7.3,1X)') 
     &PPOWOC(IPWC)/1000.,QPOWOC(IPWC)/1000.      
 709      CONTINUE           
        ENDIF
        WRITE(ITU,*)' '
        WRITE(ITU,*)'Leaving ENETASGN' 
        WRITE(ITU,*)' '
      ENDIF
      
      RETURN
      END

C ******************************* ENETB2PU *******************************************
      SUBROUTINE ENETB2PU

C Includes
#include "building.h"
#include "plant.h"
#include "power.h"


C Trace
      COMMON/TC/ITC,ICNT
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU
      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      
C Loop through each node in the network and convert the calculated boundary
C power flows to per-unit. 
      DO 10 INOD=1,NENOD
        PENODPU(INOD)=PENOD(INOD)/LNPWRBAS
        QENODPU(INOD)=QENOD(INOD)/LNPWRBAS
        PENODLPU(INOD)=PENODL(INOD)/LNPWRBAS
        QENODLPU(INOD)=QENODL(INOD)/LNPWRBAS
        PENODGPU(INOD)=PENODG(INOD)/LNPWRBAS
        QENODGPU(INOD)=QENODG(INOD)/LNPWRBAS
        PENODTPU(INOD)=PENODT(INOD)/LNPWRBAS
        QENODTPU(INOD)=QENODT(INOD)/LNPWRBAS
 10   CONTINUE


C Trace 
      IF(ITC.GT.0.AND.NSINC.GE.ITC.AND.NSINC.LE.ITCF.AND.
     &ITRACE(2).GT.0) THEN
        WRITE(ITU,*) 'Trace subroutine ENETB2PU'
        DO 30 INOD=1,NENOD
          WRITE(ITU,*) 'Per-unit nodal boundary power flows ',
     &'Node: ', INOD
          WRITE(ITU,*) 'Connected loads '
          WRITE(ITU,*) '   Real(pu)     |      Im(pu)'
          WRITE(ITU,*) 
          WRITE(ITU,'(I3,2(F9.4,10X))') INOD,PENODLPU(INOD),
     &QENODLPU(INOD)  
          WRITE(ITU,*) ' '
          WRITE(ITU,*) 'Connected generation '
          WRITE(ITU,*) '   Real(pu)     |      Im(pu)'
          WRITE(ITU,'(I3,2(F9.4,10X))') INOD,PENODGPU(INOD),
     &QENODGPU(INOD)
          WRITE(ITU,*) 'Net nodal power flow '
          WRITE(ITU,*) ' ------------------------------'     
          WRITE(ITU,'(I3,2(F9.4,10X))') INOD,PENODPU(INOD),QENODPU(INOD)      
  30    CONTINUE
      ENDIF

      RETURN
      END
