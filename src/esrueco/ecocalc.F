C****************VoluMass************************************
C
C This function return the multiplicative facteur to apply to the trmasport
C Impacts Factror (IF) depending on the desnity of the transported (new) material
C
C If density > 300 kg/m3 : mass is predominant
C else the volume is predominant. Thus, the transport of a light material 
C required several journey, compare to a heavy. To account for this repetition
C of transport, the IF of the transported are multiplied by the number of required
C journey. Assuming, that mineral wool (60 kg/m3) generate 4 times more impacts
C 
C Ro : Material density


      FUNCTION VoluMass(Ro)
      IF (Ro.LT.300.) THEN
        Vx = REAL(INT(Ro/1440. + 273.5/Ro)+1.)        
      ELSE
        Vx = 1.0
      ENDIF
        VoluMass = Min(Vx,10.)
      RETURN
      END


C ******************** PRECALC ******************************
C  PRECALC a do precalculation for all elementary stages, according
C to the values scan in the bcf file.

C LCA calculation Step 1 : Mass impacts
C ----------------------
C LayersMS((MMLC)   REAL =  Specific mass of (partial-)surfacic layer
C    If surfacic layer (f.i. pane (QuaLayer = S)): [kg*/m2 of composite]
C    If partial-surfacic layer (f.i. bindings(QuaLayer = B)): [kg/m2 of composite] 
C CompSMS((MMLC)   REAL = Specific mass of (partial-)surfacic layer of composite 
C                       = Somme(LayerSMS)
C LayerSMP((MMLC)   REAL = Specific mass of peripherical layer
C    If periferic layer (f.i. spacer(QuaLayer = P)): [kg*/mLinear]
C    mLinear: linear meter of  permimeter [m]
C    Ex.: Spacer (or frame) mass is define by its thickness and glass permimeter,
C         the laste dimension is generaly defined
C CompSMP((MMLC)   REAL = Specific mass of permiter layer of composite 
C                       = Somme(LayerSMP)
C PMassIMP(MMLC,MIMP)   REAL = Impact of peripherical layer mass, including :
C                            - Fabrication
C                            - Transport
C                            - Breakage
C SMassIMP(MMLC,MIMP)   REAL = Impact of surfacif layer mass, including :
C                            - Fabrication
C                            - Transport
C                            - Breakage

C STEP 2: Transport to composite construction site 
C ------------------------------------------------
C Transport from material facory to composite site construction
C TrPImp(MMLC,ME,MIMP)   REAL = Impact due to the transport of periferic
C                           layer [Impact/mlinear*]
C TrSImp(MMLC,ME,MIMP)   REAL = Impact due to transport of surfacic 
C                           layer [Impact/m2*]

C STEP 3: Composite assembly
C --------------------------
C Composite assembly impacts
C Composite specifi mass (surfacic or perimeter) * Imapacts
C ComAsImp(MMLC,MIMP) REAL = Impact for composite assembly [Impact/m2*]
C PerAsImp(MMLC,MIMP) REAL = Impact for composite perimeter
C                           assembly [Impact/mlinear*]

C STEP 4: Transport from composite construction site to biulding site 
C -------------------------------------------------------------------
C Impact due to the transport of the whole composite (prefabricated or
C semi-finish) from the the composite assembly factory 
C (f.i. window factory) to the building site
C TrPerImp(MMLC,MIMP) REAL = Impact for the perimeter layer[Impact/mlinear*]
C TrComImp(MMLC,MIMP) REAL = Impact for the surfacic layer[Impact/m2*]

C STEP 5: Assembly on the building
C --------------------------------
C Impact due to the assembly of the composite on the building
C ComBuImp(MMLC,MIMP) REAL = Impact for the perimeter layer[Impact/mlinear*]
C PerBuImp(MMLC,MIMP) REAL = Impact for the surfacic layer[Impact/m2*]

C Step 6: Construction impact
C --------------------------------
C Impacts due to the construction of the building (from material 
C extration to the construction of the building)
C It is the sum of impacts due to :
C   - Mass (step 1)
C   - Transport of basic material (step 2 and 4)
C   - Composite assembly (step 3)
C   - Assembly on the building (step 5)

C ComCoImp(MMLC,MIMP) REAL = Impact for the perimeter layers 
C                           construction [Impact/mlinear*]
C PerCoImp(MMLC,MIMP) REAL = Impact for the surfacic layers 
C                           construction [Impact/m2*]

C Step 7: Composite maintenance
C -----------------------------
C Impacts due to the composite maintenance over the building life time
C ComSMain(MMLC,MIMP) REAL = Impacts of the surface composite maintenance
C ComPMain(MMLC,MIMP) REAL = Impacts of the surface composite maintenance

C STEP 8: Layer replacement impacts
C ---------------------------------
C Impacts due to the layer replacement over the building life time
C Currently using the time weighting method
C SurReImp(MMLC,ME,MIMP) REAL = Impacts due to surfacic layers 
C                           replacement [Impacts/m2*]
C PerReImp(MMLC,ME,MIMP) REAL = Impacts due to surfacic layers 
C                           replacement [Impacts/mlinear*]

C STEP 9: Composite impacts over building life time
C -------------------------------------------------
C Impacts due to the composite over the building life time
C Sum of maintenance (step 7) and replacement (step 8)
C SurLfImp(MMLC,MIMP) INTEGER = Impacts of surfacic layers [Impacts/m2*]
C PerLfImp(MMLC,MIMP) INTEGER = Impacts of periferic layers [Impacts/mlinear*]

C STEP 10: Dump impacts
C -------------------------------------
C Mass of waste transported to the dump site
C DumPWast(MMLC,ME) REAL = Mass of the dumped periferical layer [kg/mlinear*]
C DumSWast(MMLC,ME) REAL = Mass of the dumped surfacic layer [kg/m2*]

C Impacts due to the transport of the dumped material
C DumPTr(MMLC,ME,MIMP) REAL = Impacts of the periferical layer trans.[kg/mlinear*]
C DumSTr(MMLC,ME,MIMP) REAL = Impacts of the surfacic layer trans.[kg/mlinear*]

C Impacts due to the dump process
C DumPPr(MMLC,ME,MIMP) REAL = Impacts of the periferical layer dumped [kg/mlinear*]
C DumSPr(MMLC,ME,MIMP) REAL = Impacts of the surfacic layer dumped [kg/mlinear*]

C STEP 11: Incineration impacts
C -------------------------------------
C Mass of waste transported to the incineration site
C BurPWast(MMLC,ME) REAL = Mass of the incineratied periferical layer [kg/mlinear*]
C BurSWast(MMLC,ME) REAL = Mass of the incinerated surfacic layer [kg/m2*]

C Impacts due to the transport of the incinerated material
C BurPTr(MMLC,ME,MIMP) REAL = Impacts of the periferical layer trans.[kg/mlinear*]
C BurSTr(MMLC,ME,MIMP) REAL = Impacts of the surfacic layer trans.[kg/mlinear*]

C Impacts due to the incineration process
C BurPPr(MMLC,ME,MIMP) REAL = Impacts of the periferical layer incinerated[kg/mlinear*]
C BurSPr(MMLC,ME,MIMP) REAL = Impacts of the surfacic layer incinerated[kg/mlinear*]

C STEP 12: Recycling waste
C -------------------------------------
C Mass of waste transported to the incineration site
C RecPWast(MMLC,ME) REAL = Mass of the recycled periferical layer [kg/mlinear*]
C RecSWast(MMLC,ME) REAL = Mass of the recycled surfacic layer [kg/m2*]
C
C Impacts due to the transport of the recycled material
C RecPTr(MMLC,ME,MIMP) REAL = Impacts of the periferical layer trans.[kg/mlinear*]
C RecSTr(MMLC,ME,MIMP) REAL = Impacts of the surfacic layer trans.[kg/mlinear*]
C NM (Integer): dummy variable : Material ID related to Ist Composite and the Jth layer

      SUBROUTINE PRECALC
      INCLUDE 'building.h'
      INCLUDE 'LCA.h'

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)

      COMMON/EIAScnPt/FabPt(MMAT),TrPt(MMAT,MNbTrans),
     &                RecyPt(MMAT),ReTrPt(MMAT,MNbTrans),
     &                BurnPt(MMAT),BuTrPt(MMAT,MNbTrans),
     &                DumpPt(MMAT),DuTrPt(MMAT,MNbTrans),
     &                ComAsPt(MMLC,MNbAss),CoTrPt(MMLC,MNbTrans),
     &                MaintTyp(MMLC,MNbM),MaintPt(MMLC,MNbM),
     &                ComAsPa(MMLC,MNbAss),AsProjPt(MMLC,MNbAss),
     &                AsProjPa(MMLC,MNbAss),LayerCat(MMLC,ME),
     &                MainPart(MMLC,MNbM),LayerNam(MMLC,ME),
     &                LayerTyp(MMLC,ME)


      COMMON/EIAImp/FabImp(MMAT,MIMP),TrImp(MMAT,MNbTrans,MIMP),
     &          RecyImp(MMAT,MIMP),ReTrImp(MMAT,MNbTrans,MIMP),
     &          BurnImp(MMAT,MIMP),BuTrImp(MMAT,MNbTrans,MIMP),
     &          DumpImp(MMAT,MIMP),DuTrImp(MMAT,MNbTrans,MIMP),
     &          ComAsImp(MMLC,MNbAss,MIMP),CoTrImp(MMLC,MNbTrans,MIMP),
     &          AsProImp(MMLC,MNbAss,MIMP), RMainImp(MMLC,MNbM,MIMP)

      COMMON/EIADist/TrDist(MMAT,MNbTrans), CoTrDist(MMLC,MNbTrans),
     &                 ReTrDist(MMAT,MNbTrans), BuTrDist(MMAT,MNbTrans),
     &                 DuTrDist(MMAT,MNbTrans)


      COMMON/EIAMisc/NbEIALay(MMLC),ProjLife, RMatLife(MMAT),
     &               PerMaint(MMLC,MNbM),RecyRate(MMAT),BurnRate(MMAT),
     &               DumpRate(MMAT),AssLoss(MMAT),TrBrk(MMAT,MNbTrans),
     &               AsProBrk(MMAT,MNbTrans),CoTrBrk(MMLC,MNbAss),
     &               QuaLayer(MMLC,ME),FixLayer(MMLC,ME),DenLay(MMAT),
     &               SpecData(MMAT,2), NRi(MMLC,ME)


      COMMON/SPECMASS/SMPLayer(MMLC,ME),SMSLayer(MMLC,ME),IWhich,
     &                SupFab(MMLC,ME),CompSMP(MMLC),CompSMS(MMLC),
     &                SupTr(MMLC,ME),SupTrMat(MMLC,ME),WeigtMat

      COMMON/ECOCalcB/RecPWast(MMLC,ME), RecSWast(MMLC,ME),
     &               RecPTr(MMLC,ME,MNbTrans,MIMP),
     &               RecSTr(MMLC,ME,MNbTrans,MIMP),
     &               RecPPr(MMLC,ME,MIMP),RecSPr(MMLC,ME,MIMP), 
     &               DumPWast(MMLC,ME),DumSWast(MMLC,ME),
     &               DumPTr(MMLC,ME,MNbTrans,MIMP),
     &               DumSTr(MMLC,ME,MNbTrans,MIMP),
     &               DumPPr(MMLC,ME,MIMP),DumSPr(MMLC,ME,MIMP), 
     &               BurPWast(MMLC,ME),BurSWast(MMLC,ME),
     &               BurPTr(MMLC,ME,MNbTrans,MIMP),
     &               BurSTr(MMLC,ME,MNbTrans,MIMP),
     &               BurPPr(MMLC,ME,MIMP),BurSPr(MMLC,ME,MIMP)


      COMMON/ECOCalcA/SMassImp(MMLC,ME,MIMP),PMassImp(MMLC,ME,MIMP),
     &               TrSImp(MMLC,ME,MNbTrans,MIMP),
     &               TrPImp(MMLC,ME,MNbTrans,MIMP),
     &               ComAsIm(MMLC,MNbAss,MIMP), 
     &               PerAsIm(MMLC,MNbAss,MIMP),
     &               TrComImp(MMLC,ME,MNbTrans,MIMP),
     &               TrPerImp(MMLC,ME,MNbTrans,MIMP),
     &               ComBuImp(MMLC,MNbAss,MIMP),
     &               PerBuImp(MMLC,MNbAss,MIMP),
     &               ComCoImp(MMLC,MIMP),PerCoImp(MMLC,MIMP),
     &               ComSMain(MMLC,MNbAss,MIMP),
     &               ComPMain(MMLC,MNbAss,MIMP),
     &               SurReMas(MMLC,ME),PerReMas(MMLC,ME),
     &               SurReImp(MMLC,ME,MIMP),PerReImp(MMLC,ME,MIMP),
     &               SurLfImp(MMLC,MIMP),PerLfImp(MMLC,MIMP)

      COMMON/EIAhigh/NbComp,NbMat,IMatID(MMLC,ME),ImatDbID(0:MMAT-1),
     &               LCIATag
      COMMON/ZFunction/ZFun(NbSteps,MMLC,ME)
      COMMON/LCAUnit/UnitEner,UnitMass,UnitImp
      COMMON/LCAFlag/ILoss,ICalType,Irep

      CHARACTER DESC*48
      CHARACTER*12 FabPt,TrPt,RecyPt,ReTrPt,BurnPt,BuTrPt,DumpPt,DuTrPt
      CHARACTER*12 ComAsPt,CoTrPt,MaintPt,AsProjPt
      CHARACTER*1 ComAsPa,MaintTyp,LayerCat,MainPart,AsProjPa,LayerTyp
      CHARACTER*12 LayerNam
      CHARACTER UnitEner*5, UnitMass*4, UnitImp*8
      Dimension ConstrImp(MMLC,ME,MIMP)

       DO 1 I=1,MMLC
         CompSMP(I) = 0.
         CompSMS(I) = 0.
         DO 2 J=1,ME
           NRi(I,J) = 0
           SMSLayer(I,J) = 0.
           SMPLayer(I,J) = 0.
           DumPWast(I,J) = 0.
           DumSWast(I,J) = 0.
           BurPWast(I,J) = 0.
           BurSWast(I,J) = 0.
           RecPWast(I,J) = 0.
           RecSWast(I,J) = 0.
           DO 3 L=1,MIMP
             PMassImp(I,J,L) = 0.
             SMassImp(I,J,L) = 0.
             PerReImp(I,J,L) = 0.
             SurReImp(I,J,L) = 0.
             DumPPr(I,J,L) = 0.
             DumSPr(I,J,L) = 0.
             BurPPr(I,J,L) = 0.
             BurSPr(I,J,L) = 0.
             CONSTRIMP(I,J,L) = 0.
             DO 33 K=1,MNbTrans
               TrPImp(I,J,K,L) = 0.
               TrSImp(I,J,K,L) = 0.
               DumPTr(I,J,K,L) = 0.
               DumSTr(I,J,K,L) = 0.
               BurPTr(I,J,K,L) = 0.
               BurSTr(I,J,K,L) = 0.
               RecPTr(I,J,K,L) = 0.
               RecSTr(I,J,K,L) = 0.
               TrPerImp(I,J,K,L) = 0.
               TrComImp(I,J,K,L) = 0.
 33        CONTINUE
 3        CONTINUE
 2      CONTINUE
 1    CONTINUE 


       DO 4 I=1,MMLC
         DO 5 J=1,MNbAss
           DO 6 L=1,MIMP
             ComAsIm(I,J,L) = 0.
             ComBuImp(I,J,L) = 0.
             PerBuImp(I,J,L) = 0.
 6        CONTINUE
 5      CONTINUE
 4    CONTINUE 


       DO 10 I=1,MMLC
         DO 11 K=1,MNbM
           DO 12 L=1,MIMP
             ComSMain(I,K,L) = 0.
 12        CONTINUE
 11      CONTINUE
 10    CONTINUE 


C Intermediary values
C Layer specific mass [kg/m2] or [kg/mlinear]
       DO 40 I=1,NbComp
         DO 41 J=1,NbEIALay(I)
           NM = IMatID(I,J)
           IF (LayerCat(I,J) .EQ. 'P') then
C Perimetric calculation
             SMPLayer(I,J) = DTHK(I,J) * SpecData(NM,1)
             CompSMP(I) = CompSMP(I) + SMPLayer(I,J)
           ELSE
C Surfacic calculation           
             SMSLayer(I,J) = DenLay(NM) * DTHK(I,J) 
             CompSMS(I) =  CompSMS(I) + SMSLayer(I,J)
           endif
 41      continue
 40    continue 

C STEP 1: Manufacture Impacts
       DO 42 I=1,NbComp
         DO 43 J=1,NbEIALay(I)
           NM=IMatID(I,J)
           if (ILoss .EQ. 0) then
             BLcon = 0.
             BLbuil = 0.
C Include break & loss in calculation
             do 441 K=1,MNbTrans
               BLcon = BLcon + TrBrk(NM,K)
               BLbuil = BLbuil + CoTrBrk(I,K)
441          continue
             SupFab(I,J) = ((AssLoss(NM)+1.)*(1.+ BLcon) + BLbuil)
           else
             SupFab(I,J) = 1.
           endif                
           DO 44 L=1,MIMP
             IF (LayerCat(I,J) .EQ. 'P') then
               PMassImp(I,J,L) = SMPLayer(I,J)*SupFab(I,J)*FabImp(NM,L)
             ELSE
               SMassImp(I,J,L) = SMSLayer(I,J)*SupFab(I,J)*FabImp(NM,L)
             ENDIF
 44        CONTINUE
 43      continue
 42    continue 

C STEP 2: Transport to composite construction site 
       DO 50 I=1,NbComp
         DO 51 J=1,NbEIALay(I)
           NM=IMatID(I,J)
           IF (DenLay(NM).LT.300.) THEN
C             ZF = Min(VoluMass(DenLay(NM)),7)
             ZF = VoluMass(DenLay(NM))
           ELSE
             ZF = 1.0
           ENDIF
           ZFun(2,I,J)=ZF
           if (ILoss .EQ. 0) then
C Include break & loss in calculation
             AL = AssLoss(NM)
           else
             AL = 0.
           endif
           DO 52 K=1,MNbTrans
             IF (ILoss .EQ. 0) THEN
C Include break & loss in calculation
               SupTrMat(I,J) = 1.+ TrBrk(NM,K)
             ELSE
               SupTrMat(I,J) = 1.
             ENDIF
             DO 53 L=1,MIMP
               IF (LayerCat(I,J) .EQ. 'P') then
                 TrPImp(I,J,K,L) =  TrPImp(I,J,K,L) + 
     &                      (SMPLayer(I,J)*(1.+AL)*SupTrMat(I,J))/1000.
     &                         * ZF *TrDist(NM,K)*TrImp(NM,K,L)
               ELSE
                   TrSImp(I,J,K,L) =  TrSImp(I,J,K,L) + 
     &                      (SMSLayer(I,J)*(1.+AL)*SupTrMat(I,J))/1000.
     &                         *  ZF *TrDist(NM,K)*TrImp(NM,K,L)
               ENDIF
 53          continue
 52        continue
 51      continue 
 50    continue 

C STEP 3: Composite assembly
       DO 60 I=1,NbComp
         DO 61 J=1,NbEIALay(I)
           NM=IMatID(I,J)
           DO 62 K=1,MNbAss
             DO 63 L=1,MIMP
               if (LayerCat(I,J) .EQ. 'P') then
C Include break & loss in calculation
                PerAsIm(I,K,L) = CompSMP(I) * ComAsImp(I,K,L)
               else
                 ComAsIm(I,K,L) = CompSMS(I) * ComAsImp(I,K,L)
               endif
 63          continue 
 62        continue 
 61      continue 
 60    continue 

C STEP 4: Transport from composite construction site to biulding site 
       DO 69 I=1,NbComp
         do 70 J=1,NbEIALay(I)
           DO 71 L=1,MIMP
             DO 72 K=1,MNbTrans
               IF (ILoss .EQ. 0) THEN
C Include break & loss in calculation
                 SupTr(I,K) = 1. + CoTrBrk(I,K)
               ELSE
                 SupTr(I,K) = 1.
               ENDIF
                 TrPerImp(I,J,K,L) = TrPerImp(I,J,K,L) +
     &                       SMPLayer(I,J)/1000. * CoTrImp(I,K,L) 
     &                       * CoTrDist(I,K) * SupTr(I,K)
                 TrComImp(I,J,K,L) = TrComImp(I,J,K,L) + 
     &                       SMSLayer(I,J)/1000. * CoTrImp(I,K,L) 
     &                       * CoTrDist(I,K) * SupTr(I,K)
 72          continue
 71        CONTINUE
 70      continue
 69    CONTINUE 

C STEP 5: Composite assembly on building
       DO 74 I=1,NbComp
         DO 75 K=1,MNbAss
           DO 76 L=1,MIMP
             ComBuImp(I,K,L) = CompSMS(I) * AsProImp(I,K,L)
             PerBuImp(I,K,L) = CompSMP(I) * AsProImp(I,K,L)
 76        continue 
 75      continue 
 74    continue 


C STEP 6: Composite maintenance Impacts
       DO 85 I=1,NbComp
         DO 86 K=1,MNbM
           IF (MaintPt(I,K) .NE. 'none') then           
             DO 87 L=1,MIMP
               IF (MaintTyp(I,K) .EQ. 'S') then
                 ComSMain(I,K,L) = ComSMain(I,K,L)+ RMainImp(I,K,L) *
     &                           ProjLife/PerMaint(I,K)
               else
                 ComPMain(I,K,L) = ComPMain(I,K,L)+ RMainImp(I,K,L) *
     &                           ProjLife/PerMaint(I,K)
               endif
 87          continue
           endif
 86      continue
 85    continue 

C STEP 7: Layer replacement impacts
       DO 21 I=1,NbComp
         DO 22 J=1,NbEIALay(I)
           DO 23 L=1,MIMP
             ConstrImp(I,J,L)=PMassImp(I,J,L)+SMassImp(I,J,L)
             DO 24 K=1,MNbTrans
               ConstrImp(I,J,L) = ConstrImp(I,J,L) + 
     &                            TrPImp(I,J,K,L) + TrPerImp(I,J,K,L)
               ConstrImp(I,J,L) = ConstrImp(I,J,L)+ 
     &                            TrSImp(I,J,K,L) + TrComImp(I,J,K,L)
 24          CONTINUE
 23        CONTINUE

           DO 25 L=1,MIMP
             DO 26 K=1,MNbAss
               ConstrImp(I,J,L) = ConstrImp(I,J,L)
     &                  + (PerAsIm(I,K,L) + ComBuImp(I,K,L))/NbEIALay(I)
               ConstrImp(I,J,L) = ConstrImp(I,J,L)
     &                  + (ComAsIm(I,K,L) + PerBuImp(I,K,L))/NbEIALay(I)
 26          CONTINUE
 25        CONTINUE

 22      CONTINUE
 21    CONTINUE


       DO 90 I=1,NbComp
         RR = 0.
         Iair=0
         DO 901 J=1,NbEIALay(I)
           NM=IMatID(I,J)
C Check if layer is air if so, it will not be accounted in the 
C composite mean service life NRj
           if (LayerNam(I,J).eq.'Air'.or.(LayerNam(I,J).eq.'air')) then
             Iair=Iair+1
           endif
           RR = RR + RMatLife(NM)
  901    CONTINUE
         NRj = INT(RR/(NbEIALay(I)-Iair))
         DO 91 J=1,NbEIALay(I)
           NM=IMatID(I,J)
           K = 1
           RRi = ProjLife/RMatLife(NM)
           IF ((RRi-INT(RRi)).LT.0.5) THEN
             NRi(I,J) = INT(RRi)-1
           ELSE
             NRi(I,J) = INT(RRi)
           ENDIF
           IF (NRi(I,J).LT.0) NRi(I,J)=0
C Fraction of mass that has to replace lost and broken material
C           PerReMas(I,J)= REAL(NRi(I,J)) * SMPLayer(I,J) * SupFab(I,J)
C           SurReMas(I,J)= REAL(NRi(I,J)) * SMSLayer(I,J) * SupFab(I,J)
C            PerReMas(I,J)= REAL(NRi(I,J)) * SMPLayer(I,J) * SupFab(I,J)
C            SurReMas(I,J)= REAL(NRi(I,J)) * SMSLayer(I,J) * SupFab(I,J)
           DO 92 L=1,MIMP
             if (LayerCat(I,J) .EQ. 'P') then
               PerReImp(I,J,L)= REAL(NRi(I,J)) * ConstrImp(I,J,L)
C               PerReImp(I,J,L)= PerReMas(I,J) * FabImp(NM,L)
             else
               SurReImp(I,J,L)= REAL(NRi(I,J)) * ConstrImp(I,J,L)
C               SurReImp(I,J,L)= SurReMas(I,J) * FabImp(NM,L)
             endif
 92        continue
 91      continue
 90    continue 


C STEP 7 + 8: Waste transport to dump site and dump impacts
       DO 105 I=1,NbComp
         DO 106 J=1,NbEIALay(I)
           NM=IMatID(I,J)
           IF (DumpPt(NM)(1:4) .NE. 'none') then 
             if (LayerCat(I,J) .EQ. 'P') then
               DumPWast(I,J)=DumpRate(NM)*(SMPLayer(I,J)*SupFab(I,J)
     &                       *(REAL(NRi(I,J)+1)))
             else
               DumSWast(I,J)=DumpRate(NM)*(SMSLayer(I,J)*SupFab(I,J)
     &                       *(REAL(NRi(I,J)+1)))
             endif
             DO 107 L=1,MIMP
               DumPPr(I,J,L) = DumPWast(I,J) * DumpImp(NM,L)
               DumSPr(I,J,L) = DumSWast(I,J) * DumpImp(NM,L)
               DO 108 K=1,MNbTrans
                 IF (LayerCat(I,J) .EQ. 'P') then
                   DumPTr(I,J,K,L)= DumPTr(I,J,K,L)+DumPWast(I,J)/1000.
     &                              *DuTrImp(NM,K,L) * DuTrDist(NM,K)
                 else
                   DumSTr(I,J,K,L)= DumSTr(I,J,K,L)+DumSWast(I,J)/1000.
     &                            * DuTrImp(NM,K,L) * DuTrDist(NM,K)
                 endif
108            continue
107          continue
           endif
106      continue 
105    continue 

C STEP 9 + 10: Waste transport to incineration site and incineration impacts
       DO 110 I=1,NbComp
         DO 111 J=1,NbEIALay(I)
           NM=IMatID(I,J)
           IF (BurnPt(NM)(1:4) .NE. 'none') then 
             if (LayerCat(I,J) .EQ. 'P') then
               BurPWast(I,J)= BurnRate(NM)*(SMPLayer(I,J)*SupFab(I,J)
     &                       *(REAL(NRi(I,J)+1)))
             else
               BurSWast(I,J)= BurnRate(NM)*(SMSLayer(I,J)*SupFab(I,J)
     &                       *(REAL(NRi(I,J)+1)))
             endif
             DO 112 L=1,MIMP
               BurPPr(I,J,L) = BurPWast(I,J) * BurnImp(NM,L)
               BurSPr(I,J,L) = BurSWast(I,J) * BurnImp(NM,L)
               DO 113 K=1,MNbTrans
                 IF (LayerCat(I,J) .EQ. 'P') then
                   BurPTr(I,J,K,L)=BurPTr(I,J,K,L)+BurPWast(I,J)/1000.
     &                             *BuTrImp(NM,K,L) * BuTrDist(NM,K)
                 else
                   BurSTr(I,J,K,L)=BurSTr(I,J,K,L)+BurSWast(I,J)/1000.
     &                             *BuTrImp(NM,K,L)*BuTrDist(NM,K)
                 endif
113            continue
112          continue
           endif
111      continue 
110    continue 

C STEP 11 + 12: Waste transport to recycling site and recycling waste impacts
       DO 115 I=1,NbComp
         DO 116 J=1,NbEIALay(I)
           NM=IMatID(I,J)
           IF (RecyPt(NM)(1:4) .NE. 'none') then 
             if (LayerCat(I,J) .EQ. 'P') then
               RecPWast(I,J)=RecyRate(NM)*(SMPLayer(I,J)*SupFab(I,J)
     &                       *(REAL(NRi(I,J)+1)))
             else
               RecSWast(I,J)=RecyRate(NM)*(SMSLayer(I,J)*SupFab(I,J)
     &                       *(REAL(NRi(I,J)+1)))
             endif
             DO 117 L=1,MIMP
               RecPPr(I,J,L) = RecPWast(I,J) * RecyImp(NM,L)
               RecSPr(I,J,L) = RecSWast(I,J) * RecyImp(NM,L)
               DO 118 K=1,MNbTrans
                 IF (LayerCat(I,J) .EQ. 'P') then
                   RecPTr(I,J,K,L)= RecPTr(I,J,K,L)+RecPWast(I,J)/1000.
     &                             *ReTrImp(NM,K,L) * ReTrDist(NM,K)
                 else
                   RecSTr(I,J,K,L)= RecSTr(I,J,K,L)+RecSWast(I,J)/1000.
     &                              *ReTrImp(NM,K,L) * ReTrDist(NM,K)
                 endif
118            continue
117          continue
           endif
116      continue 
115    continue 

      RETURN
      END
 
