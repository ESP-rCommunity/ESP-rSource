C ESRUPDF.H of ESRUpdf
C
C ESRUpdf Plant network definition program, PARAMETERs & COMMONs.
C
C     Library record length.
C     Remeber to update this if changed in PLANTDB.H.
      PARAMETER (IRECLN = 20)

C     Max header records in database.
C     Remeber to update this if changed in PLANTDB.H.
      PARAMETER (MHEADR = 25)

C     The following parameters are same as in plant.h.
      PARAMETER (MAXPC = 460)
      PARAMETER (MPCOM = 50)
      PARAMETER (MMISCD= 5)
C     This parameter is same as in building.h.
      PARAMETER (MCOM = 25)

C     Maximum number of control variables.
C     Remember to update this if changed in PLANT.H.
      PARAMETER (MCONVR = 3)

C     Maximum number of miscellaneous data items.
C     Remember to update this if changed in PLANTDB.H.
      PARAMETER (MADATA= 60)

C     Maximum number of connections.
C     Remember to update this if changed in PLANT.H.
      PARAMETER (MPCON= 75)

C     Maximum number of nodes in a component.
C     Remember to update this if changed in PLANTDB.H.
      PARAMETER (MNODEC= 20)

C     Common blocks
      COMMON /dbdat/ NPCDB, ITMLOC(MAXPC,2)

C Plant component database filename.
      COMMON/C7/LPCDB

C Plant network description.
      COMMON/C8/LPNAM

C The first two variables in C13PS are redundunt.
C Only NPMTYP is used.
      COMMON/C13PS/NPMCOE,NPNOD,NPMTYP

C
      COMMON/FFN/IFLWN,ICFFS(MPCON)

C
      COMMON/AFN/IAIRN,LAPROB,LAPRES,LAFRES,ICAAS(MCOM)

C Components variables.
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      common /pcdat/ nnodes(mpcom), isv(mpcom,mnodec), 
     &               ndcon(mpcom,mnodec)

C These variables hold data related to new component
C parameters.
      COMMON/PCPAR/NPI(MPCOM), TADATA(MPCOM,MADATA)

C Connections variables.
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)

C Containments variables
      COMMON/C11/NCONT,IPCC(MPCOM),INDCP(MPCOM),CNTDAT(MPCOM,3)

C Strings describing miscellaneous data, control variable data
C and plant component.
      common /datdsc/ mscdsc(madata), cvrdsc(mconvr)
      common /pcddsc/ pcdesc(maxpc), npref(mpcom)

C The following arrays are used to hold mass flow
C network information.
      common/mfng/nodnam(mpcom), fcmp(mpcom)

      common/trnsys/ittype(mpcom), loctrs(mpcom), idbct(mpcom), ntypes

      character*68 mscdsc, cvrdsc
      character*80 pcdesc
      character*72 lpcdb, lpnam,LAPROB,LAPRES,LAFRES
      character*6  nodnam, fcmp
