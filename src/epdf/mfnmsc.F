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

C ************************* genmfn **************************
C This subroutine generates a massflow network for the
C plant problem.
C NOTE:
C    mode = 1  : Lists the mass flow network
C    mode = 2  : Writes mass flow network description to a file.

      subroutine genmfn(mode)

#include "building.h"
#include "net_flow.h"
#include "net_flow_data.h"
#include "plant.h"

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FFN/IFLWN,ICFFS(MPCON)
      character LAPROB*72
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/MFLCLM/DRYB,QDIF,QDNR,IRVH,WDIR,WSPD,WRED
      COMMON/MFLCTL/IRY,IRM,IRD,IRH,FLWTIM,IHOUR,IYD,IFYD,ILYD,IPROG
      COMMON/MFLITR/MAXITF,FERREL,FERMFL,PMAX,STEFFR,MFTRAC,ITER,IOK

      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      common /pcdat/ nnodes(mpcom), isv(mpcom,mnodec), 
     &               ndcon(mpcom,mnodec)
      COMMON/PCPAR/NPI(MPCOM), TADATA(MPCOM,MADATA)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)

      common/tmpmfdt/tmpmfdat(mpcon,17),imfed(mpcon,17)
      common/mfnchl/mfunit
      common/mfcpar/itypmf(mpcon), isdcmf(mpcon), icnnmf(mpcon),
     &              supcmf(mpcon,17), ltpcmf(mpcon), isdimf(mpcon)

C To signal to MFWRIT that globals are available from graphic network.
C Currently set true if graphic network file, false otherwise.
      logical haveglobal
      COMMON/MFLOW11/haveglobal

      dimension rcvnod(mpcon), sndnod(mpcon), indxcn(mpcom)
      character  rcvstr*12, sndstr*12, str*12
      character  rcvnod*12, sndnod*12, ltpcmf*60

C Initialise mass flow netwrk variables.
      nnod=0; ncmp=0; ncnn=0; wred=1.0
      do 1 in=1, mnod
        ndnam(in)=' '
    1 continue
      do 2 icon=1, npcon
         sndnod(icon)=' '
    3    rcvnod(icon)=' '
    2 continue
      do 4 ipc=1, npcomp
        indxcn(ipc)=0
    4 continue

C Initialise variables related to node names
      rcvstr='CompXXNodeXX'
      sndstr='CompXXNodeXX'
      k=ichar('0')

C Assign names to each connected node.
      do 10 icon=1, npcon

C Establish name of recieving node as CXXNXX
C where CXX component number and NXX node number.
        ic1=ipc1(icon)
        in1=ipn1(icon)
        il=ic1/10
        ir=ic1-il*10
        rcvstr(5:5)=char(k+il)
        rcvstr(6:6)=char(k+ir)
        il=in1/10
        ir=in1-il*10
        rcvstr(11:11)=char(k+il)
        rcvstr(12:12)=char(k+ir)

C Establish name of sending node as CXXNXX
C where CXX component number and NXX node number.
        ic2=ipc2(icon)
        in2=ipn2(icon)
        il=ic2/10
        ir=ic2-il*10
        sndstr(5:5)=char(k+il)
        sndstr(6:6)=char(k+ir)
        il=in2/10
        ir=in2-il*10
        sndstr(11:11)=char(k+il)
        sndstr(12:12)=char(k+ir)

C Add name to recieving node array
        rcvnod(icon)=rcvstr

C Add name to sending node array
        sndnod(icon)=sndstr
   10 continue

C Now find number of nodes and fill 'ndnam' array.
C Also find flow network node type (ie. boundary node)
      nnod=0
      do 20 icon=1, npcon
        rcvstr=rcvnod(icon)
        sndstr=sndnod(icon)
        ict=ipct(icon)
        ifond1=0
        ifond2=0
        if(nnod.gt.0) then
          do 25 inod=1, nnod
            if(ndnam(inod)(1:12).eq.rcvstr(1:12)) ifond1=1
            if(ndnam(inod)(1:12).eq.sndstr(1:12)) ifond2=1
  25      continue
        endif
        if(ifond1.eq.0) then
          nnod=nnod+1
          ndnam(nnod)=rcvstr
          ndfld(nnod)=1
          ndtyp(nnod)=0
          hnod(nnod,1)=0.0; hnod(nnod,2)=0.0; hnod(nnod,3)=1.0
          tnod (nnod)=20.0
          supnod(nnod,1)=0.0
          supnod(nnod,2)=0.0
        endif
        if(ifond2.eq.0) then
          nnod=nnod+1
          ndnam(nnod)=sndstr
          ndfld(nnod)=1

C Special case for a boundary node.
          if(ict.eq.4.and.int(pconsd(icon,1)).eq.0) then
            ndtyp(nnod)=3
            supnod(nnod,1)=18.0
            supnod(nnod,2)=180.0
            hnod(nnod,1)=0.0; hnod(nnod,2)=0.0; hnod(nnod,3)=1.0
            tnod (nnod)=0.0
          else
            ndtyp(nnod)=0
            supnod(nnod,1)=0.0
            supnod(nnod,2)=0.0
            hnod(nnod,1)=0.0; hnod(nnod,2)=0.0; hnod(nnod,3)=1.0
            tnod (nnod)=20.0
          endif
        endif
   20 continue

C For each component node in the plant inter-component connectivity
C list, establish mass flow network arrays 'nodps' and 'nodne'
      do 55 icon=1, npcon
        rcvstr=rcvnod(icon)
        sndstr=sndnod(icon)
        do 44 inod=1, nnod
          if(ndnam(inod)(1:12).eq.rcvstr(1:12)) nodne(icon)=inod
          if(ndnam(inod)(1:12).eq.sndstr(1:12)) nodps(icon)=inod
  44    continue

C Fill plant flow mapping array
        icffs(icon)=icon
  55  continue

C Initialise variables related to flow component name and
C number of flow components
      str='CompXXConnXX'
      k=ichar('0')
      ncmp=0

C Now find total number of mass flow components in network
C and name of each flow component as CompXXConnXX
C                                    |-||-|
C                                     |  |
C             plant component no. ----|  |----connection no.
      do 30 icon=1, npcon
        ic1=ipc1(icon)
        il=ic1/10
        ir=ic1-il*10
        str(5:5)=char(k+il)
        str(6:6)=char(k+ir)
        il=icon/10
        ir=icon-il*10
        str(11:11)=char(k+il)
        str(12:12)=char(k+ir)
        ncmp=ncmp+1
        cmnam(ncmp)=str
        itpcon(icon)=ncmp
   30 continue

C For each connection, fill mass flow component array
C with data from plant component data base.
      do 100 icon=1, npcon
        ipc=ipc1(icon)
        indxcn(ipc)=indxcn(ipc)+1
        nconns=0
        do 110 inod=1, nnodes(ipc)
  110     if(ndcon(ipc,inod).gt.0) nconns=nconns+ndcon(ipc,inod)
          if(nconns.gt.0) then
            call getmfc(ipc,nconns)
            ifc=itpcon(icon)
            itpcmp(ifc)=itypmf(indxcn(ipc))
            isdcmp(ifc)=isdcmf(indxcn(ipc))
            isdcnn(ifc)=icnnmf(indxcn(ipc))
            ltpcmp(ifc)=ltpcmf(indxcn(ipc))
            do 120 isup=1, isdcmp(ifc)
              supcmp(ifc,isup)=supcmf(indxcn(ipc),isup)
C If data has been edited set to the new value
              if(imfed(indxcn(ipc),isup).eq.1) supcmp(ifc,isup)=
     &           tmpmfdat(indxcn(ipc),isup)
  120       continue

C If control index not set then only initial parameters are required.
            if(isdimf(indxcn(ipc)).lt.isdcmp(ifc)) then
              if(supcmp(ifc,isdimf(indxcn(ipc))+1).eq.0.0) 
     &           isdcmp(ifc)=isdimf(indxcn(ipc))
            endif
         endif
  100 continue

      ncnn=npcon
      if(mode.eq.1) then
        call mflist(iuout,'f')
      else

C Use IAIRN check so network flow file data is not written
C to a graphic network flow file.
        if(IAIRN.eq.1)then
          haveglobal=.FALSE.
        elseif(IAIRN.eq.2.or.IAIRN.eq.3)then
          haveglobal=.TRUE.
        endif
        if(IAIRN.eq.1)then
          CALL MFWRIT(mfunit)
        elseif(IAIRN.eq.3)then
          CALL MF3DWRIT(mfunit)
        endif
      endif
      return
      end


C *********************** askmfn ************************
C Ask user for related mass flow network files.
      subroutine askmfn(mfunit)
#include "building.h"
#include "plant.h"
#include "esprdbfile.h"
#include "help.h"

      COMMON/FFN/IFLWN,ICFFS(MPCON)
      COMMON/AFN/IAIRN,LAPROB,ICAAS(MCOM)
      COMMON/RESLIB/RFILE,PFILE,MSTRFILE,LAFRES
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      LOGICAL OK
      character LAPROB*72
      character rfile*72,PFILE*72,MSTRFILE*72,LAFRES*72
      character lltmp*144,ltmp*72

      h(1)='This file is assumed to exist. '
      ltmp=laprob
  220 CALL EASKS(ltmp,' Mass flow network file name? ',' ',
     &      72,' ',' file name',IER,1)
      if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
        laprob=ltmp
      else
        goto 220
      endif
      ok=.true.
      CALL FPOPEN (mfunit,ISTAT,4,3,laprob)
      IF (ISTAT .EQ. 0) THEN
         h(1)='The flow file will be overwritten unless you '
         h(2)='say no and nominate a different file name. '
         CALL EASKOK('Network file exists!',
     &               'Overwrite?',OK,2)
      endif
      IF(.not.ok) goto 220
      h(1)='This file is assumed to exist. '
      lltmp=lapres
      CALL EASKS(lltmp,' Wind pressure coefficients db ? ',' ',
     &        96,' ',' file name',IER,1)
      if(lltmp(1:2).ne.'  '.and.lltmp(1:4).ne.'UNKN')then
        lapres=lltmp
      endif
      
      h(1)='This file is assumed to exist. '
      ltmp=lafres
      CALL EASKS(ltmp,' Mass flow simulation results file ? ',
     &        ' ',72,' ',' file name',IER,1)
      if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
        lafres=ltmp
      endif
      iflwn=1
      return
      end
