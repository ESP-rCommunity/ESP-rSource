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

C Common source in building/plant/ flow/ global/ optical control.

C ******** ADDCNTL
C Add or delete a building/plant/flow/optical control.
      SUBROUTINE ADDCNTL(icfoc,ACT,IER)
#include "building.h"
#include "net_flow.h"
#include "control.h"

      common/pophelp/h(60)
      COMMON/OUTIN/IUOUT,IUIN
      common/fctl4/iasocc(MCF,mcmp),nfsup(MCF)

      character ACT*1,outs*124,H*72
      dimension rper(30),rperf(30)

      if(ACT.eq.'D')then

C To delete a control loop ask for its index. Offer users a
C cancel option.
        ID=1
        if(icfoc.eq.0)limit=ncf
        if(icfoc.eq.1)limit=ncl
        if(icfoc.eq.2)limit=ncc
        if(icfoc.eq.3)limit=ngf
        if(icfoc.eq.5)limit=nof
        h(1)='Controls are referenced by their index. '
        h(2)='If you select cancel then you will be returned to '
        h(3)='control menu. '
        CALL EASKI(ID,' ',' Which control index to delete? ',
     &    1,'F',limit,'F',1,'delete control',IERI,3)
        if(ieri.eq.-3)then
          return
        endif
        IF(ID.LE.0.OR.ID.GT.LIMIT)THEN
          WRITE(outs,*)'Index id=',ID,' not in range... Returning'
          CALL EDISP(IUOUT,outs)
          RETURN
        ENDIF

C If one control and asked to remove then just decrement ncf.
        if(icfoc.eq.0.and.ncf.eq.1)then
          ncf=ncf-1
        elseif(icfoc.eq.0.and.ncf.ge.2)then

C Shift later controls up in list, in the case of nbcdt()=0 then
C assume weekday/sat/sun day types.
          DO 792 IDV=ID,ncf-1
            ibsn(IDV,1)=ibsn(IDV+1,1)
            ibsn(IDV,2)=ibsn(IDV+1,2)
            ibsn(IDV,3)=ibsn(IDV+1,3)
            iban(IDV,1)=iban(IDV+1,1)
            iban(IDV,2)=iban(IDV+1,2)
            iban(IDV,3)=iban(IDV+1,3)
            if(nbcdt(IDV+1).eq.0)then
              licc=3
            else
              licc=nbcdt(IDV+1)
            endif
            nbcdt(IDV)=nbcdt(IDV+1)
            do 793 IDT=1,licc
              ibcdv(IDV,IDT,1)=ibcdv(IDV+1,IDT,1)
              ibcdv(IDV,IDT,2)=ibcdv(IDV+1,IDT,2)
              nbcdp(IDV,IDT)=nbcdp(IDV+1,IDT)
              do 794 IDP=1,nbcdp(IDV,IDT)
                tbcps(IDV,IDT,IDP)=tbcps(IDV+1,IDT,IDP)
                ibctyp(IDV,IDT,IDP)=ibctyp(IDV+1,IDT,IDP)
                ibclaw(IDV,IDT,IDP)=ibclaw(IDV+1,IDT,IDP)
                bmiscd(IDV,IDT,IDP,1)=bmiscd(IDV+1,IDT,IDP,1)
                imis=INT(bmiscd(IDV,IDT,IDP,1))
                do 795 IPM=2,imis+1
                  bmiscd(IDV,IDT,IDP,IPM)=bmiscd(IDV+1,IDT,IDP,IPM)
  795           continue
  794         continue
  793       continue
  792     CONTINUE
          ncf=ncf-1
        elseif(icfoc.eq.1.and.ncl.eq.1)then

C Plant (reset to no loops).
          ncl=ncl-1
        elseif(icfoc.eq.1.and.ncl.ge.2)then

C Plant.
          DO 892 IDV=ID,ncl-1
            ipsn(IDV,1)=ipsn(IDV+1,1)
            ipsn(IDV,2)=ipsn(IDV+1,2)
            ipsn(IDV,3)=ipsn(IDV+1,3)
            ipan(IDV,1)=ipan(IDV+1,1)
            ipan(IDV,2)=ipan(IDV+1,2)
            ipan(IDV,3)=ipan(IDV+1,3)
            if(npcdt(IDV+1).eq.0)then
              licc=3
            else
              licc=npcdt(IDV+1)
            endif
            npcdt(IDV)=npcdt(IDV+1)
            do 893 IDT=1,licc
              ipcdv(IDV,IDT,1)=ipcdv(IDV+1,IDT,1)
              ipcdv(IDV,IDT,2)=ipcdv(IDV+1,IDT,2)
              npcdp(IDV,IDT)=npcdp(IDV+1,IDT)
              do 894 IDP=1,npcdp(IDV,IDT)
                tpcps(IDV,IDT,IDP)=tpcps(IDV+1,IDT,IDP)
                ipctyp(IDV,IDT,IDP)=ipctyp(IDV+1,IDT,IDP)
                ipclaw(IDV,IDT,IDP)=ipclaw(IDV+1,IDT,IDP)
                pmiscd(IDV,IDT,IDP,1)=pmiscd(IDV+1,IDT,IDP,1)
                imis=INT(pmiscd(IDV,IDT,IDP,1))
                do 895 IPM=2,imis+1
                  pmiscd(IDV,IDT,IDP,IPM)=pmiscd(IDV+1,IDT,IDP,IPM)
  895           continue
  894         continue
  893       continue
  892     CONTINUE
          ncl=ncl-1
        elseif(icfoc.eq.2.and.ncc.eq.1)then

C Flow (reset to no flow loops).
          ncc=ncc-1
        elseif(icfoc.eq.2.and.ncc.ge.2)then

C Flow.
          DO 882 IDV=ID,ncc-1
            ifsn(IDV,1)=ifsn(IDV+1,1)
            ifsn(IDV,2)=ifsn(IDV+1,2)
            ifsn(IDV,3)=ifsn(IDV+1,3)
            ifan(IDV,1)=ifan(IDV+1,1)
            ifan(IDV,2)=ifan(IDV+1,2)
            ifan(IDV,3)=ifan(IDV+1,3)
            if(nfcdt(IDV+1).eq.0)then
              licc=3
            else
              licc=nfcdt(IDV+1)
            endif
            nfcdt(IDV)=nfcdt(IDV+1)
            i3=ifan(IDV,3)
            if(i3.gt.0)then
              do 887 iac=1,i3
                iasocc(IDV,i3)=iasocc(IDV+1,i3)
                nfsup(IDV)=nfsup(IDV+1)
  887         continue
            endif
            do 883 IDT=1,licc
              ifcdv(IDV,IDT,1)=ifcdv(IDV+1,IDT,1)
              ifcdv(IDV,IDT,2)=ifcdv(IDV+1,IDT,2)
              nfcdp(IDV,IDT)=nfcdp(IDV+1,IDT)
              do 884 IDP=1,nfcdp(IDV,IDT)
                tfcps(IDV,IDT,IDP)=tfcps(IDV+1,IDT,IDP)
                ifctyp(IDV,IDT,IDP)=ifctyp(IDV+1,IDT,IDP)
                ifclaw(IDV,IDT,IDP)=ifclaw(IDV+1,IDT,IDP)
                fmiscd(IDV,IDT,IDP,1)=fmiscd(IDV+1,IDT,IDP,1)
                imis=INT(fmiscd(IDV,IDT,IDP,1))
                do 885 IPM=2,imis+1
                  fmiscd(IDV,IDT,IDP,IPM)=fmiscd(IDV+1,IDT,IDP,IPM)
  885           continue
  884         continue
  883       continue
  882     CONTINUE
          ncc=ncc-1
        elseif(icfoc.eq.3.and.ngf.eq.1)then

C Global (reset to zero global loops).
          ngf=ngf-1
        elseif(icfoc.eq.3.and.ngf.ge.2)then

C Global.
          DO 982 IDV=ID,ngf-1
            igsn(IDV,1)=igsn(IDV+1,1)
            igsn(IDV,2)=igsn(IDV+1,2)
            igsn(IDV,3)=igsn(IDV+1,3)
            igan(IDV,1)=igan(IDV+1,1)
            igan(IDV,2)=igan(IDV+1,2)
            igan(IDV,3)=igan(IDV+1,3)
            if(ngcdt(IDV+1).eq.0)then
              licc=3
            else
              licc=ngcdt(IDV+1)
            endif
            ngcdt(IDV)=ngcdt(IDV+1)

            do 983 IDT=1,licc
              igcdv(IDV,IDT,1)=igcdv(IDV+1,IDT,1)
              igcdv(IDV,IDT,2)=igcdv(IDV+1,IDT,2)
              ngcdp(IDV,IDT)=ngcdp(IDV+1,IDT)
              do 984 IDP=1,ngcdp(IDV,IDT)
                tgcps(IDV,IDT,IDP)=tgcps(IDV+1,IDT,IDP)
                igctyp(IDV,IDT,IDP)=igctyp(IDV+1,IDT,IDP)
                igclaw(IDV,IDT,IDP)=igclaw(IDV+1,IDT,IDP)
                gmiscd(IDV,IDT,IDP,1)=gmiscd(IDV+1,IDT,IDP,1)
                imis=INT(gmiscd(IDV,IDT,IDP,1))
                do 985 IPM=2,imis+1
                  gmiscd(IDV,IDT,IDP,IPM)=gmiscd(IDV+1,IDT,IDP,IPM)
  985           continue
  984         continue
  983       continue
  982     CONTINUE
          ngf=ngf-1
        elseif(icfoc.eq.5.and.nof.eq.1)then

C Optics (reset to no optics loops).
          nof=nof-1
        elseif(icfoc.eq.5.and.nof.ge.2)then

C Optical.
          DO 1 IDV=ID,nof-1
            iosn(IDV,1)=iosn(IDV+1,1)
            iosn(IDV,2)=iosn(IDV+1,2)
            iosn(IDV,3)=iosn(IDV+1,3)
            ioan(IDV,1)=ioan(IDV+1,1)
            ioan(IDV,2)=ioan(IDV+1,2)
            ioan(IDV,3)=ioan(IDV+1,3)
            if(nocdt(IDV+1).eq.0)then
              licc=3
            else
              licc=nocdt(IDV+1)
            endif
            nocdt(IDV)=nocdt(IDV+1)
            do 2 IDT=1,licc
              iocdv(IDV,IDT,1)=iocdv(IDV+1,IDT,1)
              iocdv(IDV,IDT,2)=iocdv(IDV+1,IDT,2)
              nocdp(IDV,IDT)=nocdp(IDV+1,IDT)
              do 3 IDP=1,nocdp(IDV,IDT)
                tocps(IDV,IDT,IDP)=tocps(IDV+1,IDT,IDP)
                ioctyp(IDV,IDT,IDP)=ioctyp(IDV+1,IDT,IDP)
                ioclaw(IDV,IDT,IDP)=ioclaw(IDV+1,IDT,IDP)
                omiscd(IDV,IDT,IDP,1)=omiscd(IDV+1,IDT,IDP,1)
                imis=INT(omiscd(IDV,IDT,IDP,1))
                do 4 IPM=2,imis+1
                  omiscd(IDV,IDT,IDP,IPM)=omiscd(IDV+1,IDT,IDP,IPM)
  4             continue
  3           continue
  2         continue
  1       CONTINUE
          nof=nof-1
        endif
      elseif(ACT.eq.'A')then
        h(1)='Control day types allows different control patterns '
        h(2)='on different days.  '
        h(3)='If you cancel when specifying the day types the control'
        h(4)='loop will not be added (and please confirm the details '
        h(5)='in case an error crept in. '
        if(icfoc.eq.0.and.ncf+1.gt.mcf)then
          call usrmsg('Cannot add more zone controls.',' ','W')
          return
        elseif(icfoc.eq.1.and.ncl+1.gt.mcf)then
          call usrmsg('Cannot add more plant controls.',' ','W')
          return
        elseif(icfoc.eq.2.and.ncc+1.gt.mcF)then
          call usrmsg('Cannot add more flow controls.',' ','W')
          return
        elseif(icfoc.eq.3.and.ngf+1.gt.mcf)then
          call usrmsg('Cannot add more global controls.',' ','W')
          return
        elseif(icfoc.eq.5.and.ngf+1.gt.mcf)then
          call usrmsg('Cannot add more optical controls.',' ','W')
          return
        endif
        if(icfoc.eq.0)then
          ncf=ncf+1
          ibsn(ncf,1)=0
          ibsn(ncf,2)=0
          ibsn(ncf,3)=0
          iban(ncf,1)=0
          iban(ncf,2)=0
          iban(ncf,3)=0

C Initial setup of day types. Offer the user a cancel option and
C if that is selected decrement ncf and return.
          IV=NBCDT(ncf)
          write(outs,'(a,i2,a)')
     &      ' Number of zone control day types (currently',IV,') :'
          CALL EASKABC(outs,' ',
     &      'weekday/Saturday/Sunday','one','other',IV,5)
          if(iv.eq.1)then
            NBCDT(ncf)=0
          elseif(iv.eq.2)then
            NBCDT(ncf)=1
          elseif(iv.eq.3)then
            CALL EASKI(IV,'No of user defined control day types',' ',
     &       0,'F',MCF,'-',1,'no control day types',IERI,5)
            if(ieri.eq.-3)then
              ncf=ncf-1
              return
            else
              NBCDT(ncf)=IV
            endif
          endif
          NN=NBCDT(ncf)
          IF(NN.EQ.0)NN=3
          h(1)='The default is one period in the day and a free-float'
          h(2)='control. You can specify that there will be several'
          h(3)='periods in the day and then supply details later.'
          h(4)='You can add or delete or modify periods later.'
          h(5)='If you cancel when specifying periods in the day type'
          h(6)='the control loop will be removed. '
          do 87 ik=1,NN
            IP=1
            write(outs,'(a,i2)') 'How many periods in day type ',ik
            CALL EASKI(IP,outs,'(default is 1 period free floating)',
     &      1,'F',MCDP,'F',1,'ctl periods',IERI,6)
            if(ieri.eq.-3)then
              ncf=ncf-1
              return
            else
              ibcdv(ncf,ik,1)=1
              ibcdv(ncf,ik,2)=365
              nbcdp(ncf,ik)=IP
            endif

C Initialise and edit array of start times.
            call initperr(IP,rper,rperf,'i')
C            write(6,*) rper
            do 784 IDP=1,IP
              if(IDP.eq.1)then
                tbcps(ncf,ik,IDP)=0.
              else
                if(rper(IDP).gt.0.0)then 
                  tbcps(ncf,ik,IDP)=rper(IDP)
                else
                  tbcps(ncf,ik,IDP)=float(IDP)
                endif
C              if(IDP.eq.1)tbcps(ncf,ik,IDP)=0.
C              if(IDP.gt.1)tbcps(ncf,ik,IDP)=float(IDP)
              endif
              ibctyp(ncf,ik,IDP)=0
              ibclaw(ncf,ik,IDP)=2
              bmiscd(ncf,ik,IDP,1)=0.
  784       continue
  87      continue
        elseif(icfoc.eq.1)then
          ncl=ncl+1
          ipsn(ncl,1)=0
          ipsn(ncl,2)=0
          ipsn(ncl,3)=0
          ipan(ncl,1)=0
          ipan(ncl,2)=0
          ipan(ncl,3)=0

C Initial setup of day types.
          h(1)='Control day types allows different control patterns '
          h(2)='on different days.  '
          h(3)='If you cancel when specifying the day types the control'
          h(4)='loop will not be added (and please confirm the details'
          h(5)='in case an error crept in. '
          IV=NPCDT(ncl)
          write(outs,'(a,i2,a)')
     &     ' Number of plant control day types (currently',IV,') :'
          CALL EASKABC(outs,' ',
     &      'weekday/Saturday/Sunday','one','other',IV,5)
          if(iv.eq.1)then
            NPCDT(ncl)=0
          elseif(iv.eq.2)then
            NPCDT(ncl)=1
          elseif(iv.eq.3)then
            CALL EASKI(IV,'No of user defined control day types',' ',
     &       0,'F',MCF,'-',1,'no control day types',IERI,5)
            if(ieri.eq.-3)then
              ncl=ncl-1
              return
            else
              NPCDT(ncl)=IV
            endif
          endif
          NN=NPCDT(ncl)
          IF(NN.EQ.0)NN=3
          h(1)='The default is one period in the day and a free-float'
          h(2)='control. You can specify that there will be several'
          h(3)='periods in the day and then supply details later.'
          h(4)='You can add or delete or modify periods later.'
          h(5)='If you cancel when specifying periods in the day type'
          h(6)='the control loop will be removed. '
          do 187 ik=1,NN
            IP=1
            write(outs,'(a,i2)') 'How many periods in day type ',ik
            CALL EASKI(IP,outs,'(default is 1 period free floating)',
     &      1,'F',mcdp,'F',1,'ctl periods',IERI,6)
            if(ieri.eq.-3)then
              ncl=ncl-1
              return
            else
              ipcdv(ncl,ik,1)=1
              ipcdv(ncl,ik,2)=365
              npcdp(ncl,ik)=IP
            endif

C Initialise and edit array of start times.
            call initperr(IP,rper,rperf,'i')
C            write(6,*) rper
            do 287 IDP=1,IP
              if(IDP.eq.1)then
                tpcps(ncl,ik,IDP)=0.
              else
                if(rper(IDP).gt.0.0)then 
                  tpcps(ncl,ik,IDP)=rper(IDP)
                else
                  tpcps(ncl,ik,IDP)=float(IDP)
                endif
              endif
C              if(IDP.eq.1)tpcps(ncl,ik,IDP)=0.
C              if(IDP.gt.1)tpcps(ncl,ik,IDP)=float(IDP)
              ipctyp(ncl,ik,IDP)=0
              ipclaw(ncl,ik,IDP)=2
              pmiscd(ncl,ik,IDP,1)=0.
 287        continue
 187      continue
        elseif(icfoc.eq.2)then

C Add a flow function.
          ncc=ncc+1
          ifsn(ncc,1)=-4
          ifsn(ncc,2)=0
          ifsn(ncc,3)=0
          ifsn(ncc,4)=0
          ifan(ncc,1)=-4
          ifan(ncc,2)=0
          ifan(ncc,3)=0
          iasocc(ncc,1)=0
          nfsup(ncc)=0

C Initial setup of day types.
          h(1)='Control day types allows different control patterns '
          h(2)='on different days.  '
          h(3)='If you cancel when specifying the day types the control'
          h(4)='loop will not be added (and please confirm the details '
          h(5)='in case an error crept in. '
          IV=nfcdt(ncc)
          write(outs,'(a,i2,a)')
     &      ' No. vent/hydronic control day types (currently',IV,') :'
          CALL EASKABC(outs,' ',
     &      'weekday/Saturday/Sunday','one','other',IV,5)
          if(iv.eq.1)then
            NFCDT(ncc)=0
          elseif(iv.eq.2)then
            NFCDT(ncc)=1
          elseif(iv.eq.3)then
            CALL EASKI(IV,'No of user defined control day types',' ',
     &       0,'F',MCF,'-',1,'no control day types',IERI,5)
            if(ieri.eq.-3)then
              ncc=ncc-1
              return
            else
              NFCDT(ncc)=IV
            endif
          endif
          NN=nfcdt(ncc)
          IF(NN.EQ.0)NN=3
          do 587 ik=1,NN
            IP=1
            write(outs,'(a,i2)') 'How many periods in day type ',ik
            CALL EASKI(IP,outs,'(default is 1 period free floating)',
     &      1,'F',mcdp,'F',1,'ctl periods',IERI,5)
            if(ieri.eq.-3)then
              ncc=ncc-1
              return
            else
              ifcdv(ncc,ik,1)=1
              ifcdv(ncc,ik,2)=365
              nfcdp(ncc,ik)=IP
            endif

C Initialise and edit array of start times.
            call initperr(IP,rper,rperf,'i')
C            write(6,*) rper
            do 586 IDP=1,IP
              if(IDP.eq.1)then
                tfcps(ncc,ik,IDP)=0.
              else
                if(rper(IDP).gt.0.0)then 
                  tfcps(ncc,ik,IDP)=rper(IDP)
                else
                  tfcps(ncc,ik,IDP)=float(IDP)
                endif
C              if(IDP.eq.1)tfcps(ncc,ik,IDP)=0.
C              if(IDP.gt.1)tfcps(ncc,ik,IDP)=float(IDP)
              endif
              ifctyp(ncc,ik,IDP)=1
              ifclaw(ncc,ik,IDP)=0
              fmiscd(ncc,ik,IDP,1)=2.
              fmiscd(ncc,ik,IDP,2)=0.
              fmiscd(ncc,ik,IDP,3)=1.
 586        continue
 587      continue
        elseif(icfoc.eq.3)then
          ngf=ngf+1
          igsn(ngf,1)=0
          igsn(ngf,2)=0
          igsn(ngf,3)=0
          igan(ngf,1)=0
          igan(ngf,2)=0
          igan(ngf,3)=0

C Initial setup of day types.
          h(1)='Control day types allows different control patterns '
          h(2)='on different days.  '
          h(3)='If you cancel when specifying the day types the control'
          h(4)='loop will not be added (and please confirm the details '
          h(5)='in case an error crept in). '
          IV=NGCDT(ngf)
          CALL EASKI(IV,'No of control day types',
     &      ' (Nb. 0 implies typical Weekday/Saturday/Sunday ). ',
     &      0,'F',mcf,'-',50,'no control day types',IERI,5)
          if(ieri.eq.-3)then
            ngf=ngf-1
            return
          else
            NGCDT(ngf)=IV
          endif
          NN=NGCDT(ngf)
          IF(NN.EQ.0)NN=3
          do 687 ik=1,NN
            igcdv(ngf,ik,1)=1
            igcdv(ngf,ik,2)=365
            ngcdp(ngf,ik)=1
            tgcps(ngf,ik,1)=0.
            igctyp(ngf,ik,1)=0
            igclaw(ngf,ik,1)=2
            gmiscd(ngf,ik,1,1)=0.
 687      continue
        elseif(icfoc.eq.5)then
          nof=nof+1
          iosn(nof,1)=0
          iosn(nof,2)=0
          iosn(nof,3)=0
          ioan(nof,1)=0
          ioan(nof,2)=0
          ioan(nof,3)=0

C Initial setup of day types. Offer the user a cancel option and
C if that is selected decrement ncf and return.
          IV=NOCDT(nof)
          write(outs,'(a,i2,a)')
     &      ' Number of optical control day types (currently',IV,') :'
          CALL EASKABC(outs,' ',
     &      'weekday/Saturday/Sunday','one','other',IV,5)
          if(iv.eq.1)then
            NOCDT(nof)=0
          elseif(iv.eq.2)then
            NOCDT(nof)=1
          elseif(iv.eq.3)then
            CALL EASKI(IV,'No of user defined control day types',' ',
     &       0,'F',MCF,'-',1,'no control day types',IERI,5)
            if(ieri.eq.-3)then
              nof=nof-1
              return
            else
              NOCDT(nof)=IV
            endif
          endif
          NN=NOCDT(nof)
          IF(NN.EQ.0)NN=3
          h(1)='The default is one period in the day and a free-float'
          h(2)='control. You can specify that there will be several'
          h(3)='periods in the day and then supply details later.'
          h(4)='You can add or delete or modify periods later.'
          h(5)='If you cancel when specifying periods in the day type'
          h(6)='the control loop will be removed. '
          do 5 ik=1,NN
            IP=1
            write(outs,'(a,i2)') 'How many periods in day type ',ik
            CALL EASKI(IP,outs,'(default is 1 period free floating)',
     &      1,'F',mcdp,'F',1,'ctl periods',IERI,6)
            if(ieri.eq.-3)then
              nof=nof-1
              return
            else
              iocdv(nof,ik,1)=1
              iocdv(nof,ik,2)=365
              nocdp(nof,ik)=IP
            endif

C Initialise and edit array of start times.
            call initperr(IP,rper,rperf,'i')
C            write(6,*) rper
            do 6 IDP=1,IP
              if(IDP.eq.1)then
                tocps(ncf,ik,IDP)=0.
              else
                if(rper(IDP).gt.0.0)then 
                  tocps(nof,ik,IDP)=rper(IDP)
                else
                  tocps(nof,ik,IDP)=float(IDP)
                endif
C              if(IDP.eq.1)tbcps(ncf,ik,IDP)=0.
C              if(IDP.gt.1)tbcps(ncf,ik,IDP)=float(IDP)
              endif
              ioctyp(nof,ik,IDP)=0
              ioclaw(nof,ik,IDP)=2
              omiscd(nof,ik,IDP,1)=0.
  6         continue
  5       continue
        endif
      elseif(ACT.eq.'C')then

C Copy an existing control loop.
        if(icfoc.eq.0.and.ncf+1.gt.mcf)then
          call usrmsg('Cannot add more zone controls.',' ','W')
          return
        elseif(icfoc.eq.1.and.ncl+1.gt.mcF)then
          call usrmsg('Cannot add more plant controls.',' ','W')
          return
        elseif(icfoc.eq.2.and.ncc+1.gt.mcF)then
          call usrmsg('Cannot add more vent/hydronic controls.',' ','W')
          return
        elseif(icfoc.eq.3.and.ngf+1.gt.mcf)then
          call usrmsg('Cannot add more global controls.',' ','W')
          return
        elseif(icfoc.eq.5.and.ngf+1.gt.mcf)then
          call usrmsg('Cannot add more optical controls.',' ','W')
          return
        endif
        ICC=1
        if(icfoc.eq.0)limit=ncf
        if(icfoc.eq.1)limit=ncl
        if(icfoc.eq.2)limit=ncc
        if(icfoc.eq.3)limit=ngf
        if(icfoc.eq.5)limit=nof
        h(1)='Control functions are referenced by their index. '
        h(2)='A cancel returns you to the main control menu. '
        CALL EASKI(ICC,' ',' Which control function index to copy? ',
     &    1,'F',limit,'F',1,'copy control',IERI,2)
        if(ieri.eq.-3)then
          return
        endif
        if(icfoc.eq.0)then
          ncf=ncf+1
          ibsn(ncf,1)=ibsn(ICC,1)
          ibsn(ncf,2)=ibsn(ICC,2)
          ibsn(ncf,3)=ibsn(ICC,3)
          iban(ncf,1)=iban(ICC,1)
          iban(ncf,2)=iban(ICC,2)
          iban(ncf,3)=iban(ICC,3)
          if(nbcdt(ICC).eq.0)then
            licc=3
          else
            licc=nbcdt(ICC)
          endif
          nbcdt(ncf)=nbcdt(ICC)
          do 780 IDT=1,licc
            ibcdv(ncf,IDT,1)=ibcdv(ICC,IDT,1)
            ibcdv(ncf,IDT,2)=ibcdv(ICC,IDT,2)
            nbcdp(ncf,IDT)=nbcdp(ICC,IDT)
            do 781 IDP=1,nbcdp(ICC,IDT)
              tbcps(ncf,IDT,IDP)=tbcps(ICC,IDT,IDP)
              ibctyp(ncf,IDT,IDP)=ibctyp(ICC,IDT,IDP)
              ibclaw(ncf,IDT,IDP)=ibclaw(ICC,IDT,IDP)
              bmiscd(ncf,IDT,IDP,1)=bmiscd(ICC,IDT,IDP,1)
              imis=INT(bmiscd(ICC,IDT,IDP,1))
              do 782 IPM=2,imis+1
                bmiscd(ncf,IDT,IDP,IPM)=bmiscd(ICC,IDT,IDP,IPM)
  782         continue
  781       continue
  780     continue
        elseif(icfoc.eq.1)then
          ncl=ncl+1
          ipsn(ncl,1)=ipsn(ICC,1)
          ipsn(ncl,2)=ipsn(ICC,2)
          ipsn(ncl,3)=ipsn(ICC,3)
          ipan(ncl,1)=ipan(ICC,1)
          ipan(ncl,2)=ipan(ICC,2)
          ipan(ncl,3)=ipan(ICC,3)
          if(npcdt(ICC).eq.0)then
            licc=3
          else
            licc=npcdt(ICC)
          endif
          npcdt(ncl)=npcdt(ICC)
          do 773 IDT=1,licc
            ipcdv(ncl,IDT,1)=ipcdv(ICC,IDT,1)
            ipcdv(ncl,IDT,2)=ipcdv(ICC,IDT,2)
            npcdp(ncl,IDT)=npcdp(ICC,IDT)
            do 774 IDP=1,nbcdp(ICC,IDT)
              tpcps(ncl,IDT,IDP)=tpcps(ICC,IDT,IDP)
              ipctyp(ncl,IDT,IDP)=ipctyp(ICC,IDT,IDP)
              ipclaw(ncl,IDT,IDP)=ipclaw(ICC,IDT,IDP)
              pmiscd(ncl,IDT,IDP,1)=pmiscd(ICC,IDT,IDP,1)
              imis=INT(pmiscd(ICC,IDT,IDP,1))
              do 775 IPM=2,imis+1
                pmiscd(ncl,IDT,IDP,IPM)=pmiscd(ICC,IDT,IDP,IPM)
  775         continue
  774       continue
  773     continue
        elseif(icfoc.eq.2)then
          ncc=ncc+1
          ifsn(ncc,1)=ifsn(ICC,1)
          ifsn(ncc,2)=ifsn(ICC,2)
          ifsn(ncc,3)=ifsn(ICC,3)
          ifan(ncc,1)=ifan(ICC,1)
          ifan(ncc,2)=ifan(ICC,2)
          ifan(ncc,3)=ifan(ICC,3)
          if(nfcdt(ICC).eq.0)then
            licc=3
          else
            licc=nfcdt(ICC)
          endif
          nfcdt(ncc)=nfcdt(ICC)
          do 763 IDT=1,licc
            ifcdv(ncc,IDT,1)=ifcdv(ICC,IDT,1)
            ifcdv(ncc,IDT,2)=ifcdv(ICC,IDT,2)
            nfcdp(ncc,IDT)=nfcdp(ICC,IDT)
            do 764 IDP=1,nfcdp(ICC,IDT)
              tfcps(ncc,IDT,IDP)=tfcps(ICC,IDT,IDP)
              ifctyp(ncc,IDT,IDP)=ifctyp(ICC,IDT,IDP)
              ifclaw(ncc,IDT,IDP)=ifclaw(ICC,IDT,IDP)
              fmiscd(ncc,IDT,IDP,1)=fmiscd(ICC,IDT,IDP,1)
              imis=INT(fmiscd(ICC,IDT,IDP,1))
              do 765 IPM=2,imis+1
                fmiscd(ncc,IDT,IDP,IPM)=fmiscd(ICC,IDT,IDP,IPM)
  765         continue
  764       continue
  763     continue
        elseif(icfoc.eq.3)then
          ngf=ngf+1
          igsn(ngf,1)=igsn(ICC,1)
          igsn(ngf,2)=igsn(ICC,2)
          igsn(ngf,3)=igsn(ICC,3)
          igan(ngf,1)=igan(ICC,1)
          igan(ngf,2)=igan(ICC,2)
          igan(ngf,3)=igan(ICC,3)
          if(ngcdt(ICC).eq.0)then
            licc=3
          else
            licc=ngcdt(ICC)
          endif
          ngcdt(ngf)=ngcdt(ICC)
          do 753 IDT=1,licc
            igcdv(ngf,IDT,1)=igcdv(ICC,IDT,1)
            igcdv(ngf,IDT,2)=igcdv(ICC,IDT,2)
            ngcdp(ngf,IDT)=ngcdp(ICC,IDT)
            do 754 IDP=1,ngcdp(ICC,IDT)
              tgcps(ngf,IDT,IDP)=tgcps(ICC,IDT,IDP)
              igctyp(ngf,IDT,IDP)=igctyp(ICC,IDT,IDP)
              igclaw(ngf,IDT,IDP)=igclaw(ICC,IDT,IDP)
              gmiscd(ngf,IDT,IDP,1)=gmiscd(ICC,IDT,IDP,1)
              imis=INT(gmiscd(ICC,IDT,IDP,1))
              do 755 IPM=2,imis+1
                gmiscd(ngf,IDT,IDP,IPM)=gmiscd(ICC,IDT,IDP,IPM)
  755         continue
  754       continue
  753     continue
        elseif(icfoc.eq.5)then
          nof=nof+1
          iosn(nof,1)=iosn(ICC,1)
          iosn(nof,2)=iosn(ICC,2)
          iosn(nof,3)=iosn(ICC,3)
          ioan(nof,1)=ioan(ICC,1)
          ioan(nof,2)=ioan(ICC,2)
          ioan(nof,3)=ioan(ICC,3)
          if(nocdt(ICC).eq.0)then
            licc=3
          else
            licc=nocdt(ICC)
          endif
          nocdt(nof)=nocdt(ICC)
          do 7 IDT=1,licc
            iocdv(nof,IDT,1)=iocdv(ICC,IDT,1)
            iocdv(nof,IDT,2)=iocdv(ICC,IDT,2)
            nocdp(nof,IDT)=nocdp(ICC,IDT)
            do 8 IDP=1,nocdp(ICC,IDT)
              tocps(nof,IDT,IDP)=tocps(ICC,IDT,IDP)
              ioctyp(nof,IDT,IDP)=ioctyp(ICC,IDT,IDP)
              ioclaw(nof,IDT,IDP)=ioclaw(ICC,IDT,IDP)
              omiscd(nof,IDT,IDP,1)=omiscd(ICC,IDT,IDP,1)
              imis=INT(omiscd(ICC,IDT,IDP,1))
              do 9 IPM=2,imis+1
                omiscd(nof,IDT,IDP,IPM)=omiscd(ICC,IDT,IDP,IPM)
  9           continue
  8         continue
  7       continue
        endif
      endif
      return
      end

C ******** ADDCTLD
C Add or delete a building/plant/flow/global/optical control day type.
      SUBROUTINE ADDCTLD(icfoc,II,IER)
#include "building.h"
#include "net_flow.h"
#include "control.h"

      common/pophelp/h(60)
      character h*72

  77  if(icfoc.eq.0)then
        nn=nbcdt(ii)
      elseif(icfoc.eq.1)then
        nn=npcdt(ii)
      elseif(icfoc.eq.2)then
        nn=nfcdt(ii)
      elseif(icfoc.eq.3)then
        nn=ngcdt(ii)
      elseif(icfoc.eq.5)then
        nn=nocdt(ii)
      endif

      h(1)='One of the actions which can be taken on a control.'
      h(2)='If you select continue then you will be returned to'
      h(3)='the main control menu without any changes. '
      CALL EASKABCD('Options: ',' ','delete day type',
     &  'add day type','copy existing day type','continue',IW,3)
      IF(IW.EQ.1)THEN
        ID=1
        h(1)='Expecting an index. If you select cancel then you'
        h(2)='will be returned to the main control menu. '
        CALL EASKI(ID,' ',' Which day type (index)? ',
     &          1,'F',MB,'F',1,'delete day type',IERI,2)
        if(ieri.eq.-3) return
        if(icfoc.eq.0.and.NN.ge.2)then
          do 793 IDT=ID,NN-1
            ibcdv(II,IDT,1)=ibcdv(II,IDT+1,1)
            ibcdv(II,IDT,2)=ibcdv(II,IDT+1,2)
            nbcdp(II,IDT)=nbcdp(II,IDT+1)
            do 794 IDP=1,nbcdp(II,IDT)
              tbcps(II,IDT,IDP)=tbcps(II,IDT+1,IDP)
              ibctyp(II,IDT,IDP)=ibctyp(II,IDT+1,IDP)
              ibclaw(II,IDT,IDP)=ibclaw(II,IDT+1,IDP)
              bmiscd(II,IDT,IDP,1)=bmiscd(II,IDT+1,IDP,1)
              imis=INT(bmiscd(II,IDT,IDP,1))
              do 795 IPM=2,imis+1
                bmiscd(II,IDT,IDP,IPM)=bmiscd(II,IDT+1,IDP,IPM)
  795         continue
  794       continue
  793     continue
          NN=NN-1
          nbcdt(II)=NN
        elseif(icfoc.eq.1.and.NN.ge.2)then
          do 693 IDT=ID,NN-1
            ipcdv(II,IDT,1)=ipcdv(II,IDT+1,1)
            ipcdv(II,IDT,2)=ipcdv(II,IDT+1,2)
            npcdp(II,IDT)=npcdp(II,IDT+1)
            do 694 IDP=1,npcdp(II,IDT)
              tpcps(II,IDT,IDP)=tpcps(II,IDT+1,IDP)
              ipctyp(II,IDT,IDP)=ipctyp(II,IDT+1,IDP)
              ipclaw(II,IDT,IDP)=ipclaw(II,IDT+1,IDP)
              pmiscd(II,IDT,IDP,1)=pmiscd(II,IDT+1,IDP,1)
              imis=INT(pmiscd(II,IDT,IDP,1))
              do 695 IPM=2,imis+1
                pmiscd(II,IDT,IDP,IPM)=pmiscd(II,IDT+1,IDP,IPM)
  695         continue
  694       continue
  693     continue
          NN=NN-1
          npcdt(II)=NN
        elseif(icfoc.eq.2.and.NN.ge.2)then
          do 593 IDT=ID,NN-1
            ifcdv(II,IDT,1)=ifcdv(II,IDT+1,1)
            ifcdv(II,IDT,2)=ifcdv(II,IDT+1,2)
            nfcdp(II,IDT)=nfcdp(II,IDT+1)
            do 594 IDP=1,nfcdp(II,IDT)
              tfcps(II,IDT,IDP)=tfcps(II,IDT+1,IDP)
              ifctyp(II,IDT,IDP)=ifctyp(II,IDT+1,IDP)
              ifclaw(II,IDT,IDP)=ifclaw(II,IDT+1,IDP)
              fmiscd(II,IDT,IDP,1)=fmiscd(II,IDT+1,IDP,1)
              imis=INT(fmiscd(II,IDT,IDP,1))
              do 595 IPM=2,imis+1
                fmiscd(II,IDT,IDP,IPM)=fmiscd(II,IDT+1,IDP,IPM)
  595         continue
  594       continue
  593     continue
          NN=NN-1
          nfcdt(II)=NN
        elseif(icfoc.eq.3.and.NN.ge.2)then
          do 893 IDT=ID,NN-1
            igcdv(II,IDT,1)=igcdv(II,IDT+1,1)
            igcdv(II,IDT,2)=igcdv(II,IDT+1,2)
            ngcdp(II,IDT)=ngcdp(II,IDT+1)
            do 894 IDP=1,ngcdp(II,IDT)
              tgcps(II,IDT,IDP)=tgcps(II,IDT+1,IDP)
              igctyp(II,IDT,IDP)=igctyp(II,IDT+1,IDP)
              igclaw(II,IDT,IDP)=igclaw(II,IDT+1,IDP)
              gmiscd(II,IDT,IDP,1)=gmiscd(II,IDT+1,IDP,1)
              imis=INT(gmiscd(II,IDT,IDP,1))
              do 895 IPM=2,imis+1
                gmiscd(II,IDT,IDP,IPM)=gmiscd(II,IDT+1,IDP,IPM)
  895         continue
  894       continue
  893     continue
          NN=NN-1
          ngcdt(II)=NN
        elseif(icfoc.eq.5.and.NN.ge.2)then
          do 1 IDT=ID,NN-1
            iocdv(II,IDT,1)=iocdv(II,IDT+1,1)
            iocdv(II,IDT,2)=iocdv(II,IDT+1,2)
            nocdp(II,IDT)=nocdp(II,IDT+1)
            do 2 IDP=1,nocdp(II,IDT)
              tocps(II,IDT,IDP)=tocps(II,IDT+1,IDP)
              ioctyp(II,IDT,IDP)=ioctyp(II,IDT+1,IDP)
              ioclaw(II,IDT,IDP)=ioclaw(II,IDT+1,IDP)
              omiscd(II,IDT,IDP,1)=omiscd(II,IDT+1,IDP,1)
              imis=INT(omiscd(II,IDT,IDP,1))
              do 3 IPM=2,imis+1
                omiscd(II,IDT,IDP,IPM)=omiscd(II,IDT+1,IDP,IPM)
  3           continue
  2         continue
  1       continue
          NN=NN-1
          nocdt(II)=NN
        endif
      ELSEIF(IW.EQ.2)THEN

C Create a new control day type.
        h(1)='The default is one period in the day and a free-float'
        h(2)='control. You can specify that there will be several'
        h(3)='periods in the day and then supply details later.'
        h(4)='You can add or delete or modify periods later.'
        h(5)=' '
        h(6)='If you select cancel when specifying the number of '
        h(7)='periods in a day the options list will be redisplayed.'
        if(icfoc.eq.0.and.NN+1.LE.MCDT)then
          IP=1
          CALL EASKI(IP,' How many periods in this day type? ',
     &      '(default is 1 period free floating)',
     &      1,'F',MCDP,'F',1,'ctl periods',IERI,7)
          if(ieri.eq.-3)then
            goto 77
          else
            NN=NN+1
            nbcdt(II)=NN
            nb=nbcdt(II)
            ibcdv(II,nb,1)=1
            ibcdv(II,nb,2)=365
            nbcdp(II,nb)=IP
          endif
          do 784 IDP=1,IP
            if(IDP.eq.1)tbcps(II,nb,IDP)=0.
            if(IDP.gt.1)tbcps(II,nb,IDP)=float(IDP)
            ibctyp(II,nb,IDP)=0
            ibclaw(II,nb,IDP)=2
            bmiscd(II,nb,IDP,1)=0.
  784     continue
        elseif(icfoc.eq.1.and.NN+1.LE.mcdt)then
          IP=1
          CALL EASKI(IP,' How many periods in this day type? ',
     &      '(default is 1 period free floating)',
     &      1,'F',mcdp,'F',1,'ctl periods',IERI,7)
          if(ieri.eq.-3)then
            goto 77
          else
            NN=NN+1
            npcdt(II)=NN
            np=npcdt(II)
            ipcdv(II,np,1)=1
            ipcdv(II,np,2)=365
            npcdp(II,np)=IP
          endif
          do 684 IDP=1,IP
            if(IDP.eq.1)tpcps(II,np,IDP)=0.
            if(IDP.gt.1)tpcps(II,np,IDP)=float(IDP)
            ipctyp(II,np,IDP)=0
            ipclaw(II,np,IDP)=2
            pmiscd(II,np,IDP,1)=0.
  684     continue
        elseif(icfoc.eq.2.and.NN+1.LE.mcdt)then
          IP=1
          CALL EASKI(IP,' How many periods in this day type? ',
     &      '(default is 1 period free floating)',
     &      1,'F',mcdp,'F',1,'ctl periods',IERI,7)
          if(ieri.eq.-3)then
            goto 77
          else
            NN=NN+1
            nfcdt(II)=NN
            nf=nfcdt(II)
            ifcdv(II,nf,1)=1
            ifcdv(II,nf,2)=365
            nfcdp(II,nf)=IP
          endif
          do 584 IDP=1,IP
            if(IDP.eq.1)tfcps(II,nf,IDP)=0.
            if(IDP.gt.1)tfcps(II,nf,IDP)=float(IDP)
            ifctyp(II,nf,IDP)=1
            ifclaw(II,nf,IDP)=0
            fmiscd(II,nf,IDP,1)=2.
            fmiscd(II,nf,IDP,2)=0.
            fmiscd(II,nf,IDP,3)=1.
  584     continue
        elseif(icfoc.eq.3.and.NN+1.LE.mcdt)then
          NN=NN+1
          ngcdt(II)=NN
          ng=ngcdt(II)
          igcdv(II,ng,1)=1
          igcdv(II,ng,2)=365
          ngcdp(II,ng)=1
          tgcps(II,ng,1)=0.
          igctyp(II,ng,1)=1
          igclaw(II,ng,1)=0
          gmiscd(II,ng,1,1)=2.
          gmiscd(II,ng,1,2)=0.
          gmiscd(II,ng,1,3)=1.
        elseif(icfoc.eq.5.and.NN+1.LE.mcdt)then
          IP=1
          CALL EASKI(IP,' How many periods in this day type? ',
     &      '(default is 1 period free floating)',
     &      1,'F',mcdp,'F',1,'ctl periods',IERI,7)
          if(ieri.eq.-3)then
            goto 77
          else
            NN=NN+1
            nocdt(II)=NN
            no=nocdt(II)
            iocdv(II,no,1)=1
            iocdv(II,no,2)=365
            nocdp(II,no)=IP
          endif
          do 4 IDP=1,IP
            if(IDP.eq.1)tocps(II,no,IDP)=0.
            if(IDP.gt.1)tocps(II,no,IDP)=float(IDP)
            ioctyp(II,no,IDP)=0
            ioclaw(II,no,IDP)=0
            omiscd(II,no,IDP,1)=0.
  4       continue
        endif
      elseif(IW.EQ.3)then

C Copy and existing control loop day type.
        IDC=1
        h(1)='Expecting an index. If you select cancel then no'
        h(2)='action will be taken. '
        CALL EASKI(IDC,' ',' Which day type to copy ? ',
     &          1,'F',MB,'F',1,'copy day type',IERI,1)
        if(ieri.eq.-3)then
          goto 77
        endif
        if(icfoc.eq.0.and.NN+1.LE.MCDT)then
          NN=NN+1
          nbcdt(II)=NN
          nb=nbcdt(II)
          ibcdv(II,nb,1)=ibcdv(II,IDC,1)
          ibcdv(II,nb,2)=ibcdv(II,IDC,2)
          nbcdp(II,nb)=nbcdp(II,IDC)
          do 494 IDP=1,nbcdp(II,IDC)
            tbcps(II,nb,IDP)=tbcps(II,IDC,IDP)
            ibctyp(II,nb,IDP)=ibctyp(II,IDC,IDP)
            ibclaw(II,nb,IDP)=ibclaw(II,IDC,IDP)
            bmiscd(II,nb,IDP,1)=bmiscd(II,IDC,IDP,1)
            imis=INT(bmiscd(II,IDC,IDP,1))
            do 495 IPM=2,imis+1
              bmiscd(II,nb,IDP,IPM)=bmiscd(II,IDC,IDP,IPM)
  495       continue
  494     continue
        elseif(icfoc.eq.1.and.NN+1.LE.mcdt)then
          NN=NN+1
          npcdt(II)=NN
          np=npcdt(II)
          ipcdv(II,np,1)=ipcdv(II,IDC,1)
          ipcdv(II,np,2)=ipcdv(II,IDC,2)
          npcdp(II,np)=npcdp(II,IDC)
          do 394 IDP=1,npcdp(II,IDC)
            tpcps(II,np,IDP)=tpcps(II,IDC,IDP)
            ipctyp(II,np,IDP)=ipctyp(II,IDC,IDP)
            ipclaw(II,np,IDP)=ipclaw(II,IDC,IDP)
            pmiscd(II,np,IDP,1)=pmiscd(II,IDC,IDP,1)
            imis=INT(pmiscd(II,IDC,IDP,1))
            do 395 IPM=2,imis+1
              pmiscd(II,np,IDP,IPM)=pmiscd(II,IDC,IDP,IPM)
  395       continue
  394     continue
        elseif(icfoc.eq.2.and.NN+1.LE.mcdt)then
          NN=NN+1
          nfcdt(II)=NN
          nf=nfcdt(II)
          ifcdv(II,nf,1)=ifcdv(II,IDC,1)
          ifcdv(II,nf,2)=ifcdv(II,IDC,2)
          nfcdp(II,nf)=nfcdp(II,IDC)
          do 392 IDP=1,nfcdp(II,IDC)
            tfcps(II,nf,IDP)=tfcps(II,IDC,IDP)
            ifctyp(II,nf,IDP)=ifctyp(II,IDC,IDP)
            ifclaw(II,nf,IDP)=ifclaw(II,IDC,IDP)
            fmiscd(II,nf,IDP,1)=fmiscd(II,IDC,IDP,1)
            imis=INT(fmiscd(II,IDC,IDP,1))
            do 393 IPM=2,imis+1
              fmiscd(II,nf,IDP,IPM)=fmiscd(II,IDC,IDP,IPM)
  393       continue
  392     continue
        elseif(icfoc.eq.3.and.NN+1.LE.mcdt)then
          NN=NN+1
          ngcdt(II)=NN
          ng=ngcdt(II)
          igcdv(II,ng,1)=igcdv(II,IDC,1)
          igcdv(II,ng,2)=igcdv(II,IDC,2)
          ngcdp(II,ng)=ngcdp(II,IDC)
          do 390 IDP=1,ngcdp(II,IDC)
            tgcps(II,ng,IDP)=tgcps(II,IDC,IDP)
            igctyp(II,ng,IDP)=igctyp(II,IDC,IDP)
            igclaw(II,ng,IDP)=igclaw(II,IDC,IDP)
            gmiscd(II,ng,IDP,1)=gmiscd(II,IDC,IDP,1)
            imis=INT(gmiscd(II,IDC,IDP,1))
            do 391 IPM=2,imis+1
              gmiscd(II,ng,IDP,IPM)=gmiscd(II,IDC,IDP,IPM)
  391       continue
  390     continue
        elseif(icfoc.eq.5.and.NN+1.LE.mcdt)then
          NN=NN+1
          nocdt(II)=NN
          no=nocdt(II)
          iocdv(II,no,1)=iocdv(II,IDC,1)
          iocdv(II,no,2)=iocdv(II,IDC,2)
          nocdp(II,no)=nocdp(II,IDC)
          do 5 IDP=1,nocdp(II,IDC)
            tocps(II,no,IDP)=tocps(II,IDC,IDP)
            ioctyp(II,no,IDP)=ioctyp(II,IDC,IDP)
            ioclaw(II,no,IDP)=ioclaw(II,IDC,IDP)
            omiscd(II,no,IDP,1)=omiscd(II,IDC,IDP,1)
            imis=INT(omiscd(II,IDC,IDP,1))
            do 6 IPM=2,imis+1
              omiscd(II,no,IDP,IPM)=omiscd(II,IDC,IDP,IPM)
  6         continue
  5       continue
         endif
      elseif(IW.EQ.4)then
        return
      endif
      return
      end

C ******** ADDCTLDP
C Add or delete a building/plant/flow/global/optical control period.
C II is the loop, ID is the day type, act is "D" delete, "A"
C for add.
      SUBROUTINE ADDCTLDP(icfoc,II,ID,act,IER)
#include "building.h"
#include "net_flow.h"
#include "control.h"

      common/pophelp/h(60)
      character h*72,act*1

      if(icfoc.eq.0)limit=nbcdp(II,ID)
      if(icfoc.eq.1)limit=npcdp(II,ID)
      if(icfoc.eq.2)limit=nfcdp(II,ID)
      if(icfoc.eq.3)limit=ngcdp(II,ID)
      if(icfoc.eq.5)limit=nocdp(II,ID)
      if(act(1:1).eq.'D'.or.act(1:1).eq.'d')then
        IP=1
        h(1)='Expecting an index of the period. If you select'
        h(2)='cancel then no changes will be made to the control. '
        CALL EASKI(IP,' ',' Which period to delete ? ',
     &          1,'F',limit,'F',1,'delete period',IERI,2)
        if(ieri.eq.-3) return
        if(icfoc.eq.0)then
          do 794 IDP=IP,limit-1
            tbcps(II,ID,IDP)=tbcps(II,ID,IDP+1)
            ibctyp(II,ID,IDP)=ibctyp(II,ID,IDP+1)
            ibclaw(II,ID,IDP)=ibclaw(II,ID,IDP+1)
            bmiscd(II,ID,IDP,1)=bmiscd(II,ID,IDP+1,1)
            imis=INT(bmiscd(II,ID,IDP+1,1))
            do 795 IPM=2,imis+1
              bmiscd(II,ID,IDP,IPM)=bmiscd(II,ID,IDP+1,IPM)
  795       continue
  794     continue
          nbcdp(II,ID)=nbcdp(II,ID)-1
        elseif(icfoc.eq.1)then
          do 694 IDP=IP,limit-1
            tpcps(II,ID,IDP)=tpcps(II,ID,IDP+1)
            ipctyp(II,ID,IDP)=ipctyp(II,ID,IDP+1)
            ipclaw(II,ID,IDP)=ipclaw(II,ID,IDP+1)
            pmiscd(II,ID,IDP,1)=pmiscd(II,ID,IDP+1,1)
            imis=INT(pmiscd(II,ID,IDP+1,1))
            do 695 IPM=2,imis+1
              pmiscd(II,ID,IDP,IPM)=pmiscd(II,ID,IDP+1,IPM)
  695       continue
  694     continue
          npcdp(II,ID)=npcdp(II,ID)-1
        elseif(icfoc.eq.2)then
          do 594 IDP=IP,limit-1
            tfcps(II,ID,IDP)=tfcps(II,ID,IDP+1)
            ifctyp(II,ID,IDP)=ifctyp(II,ID,IDP+1)
            ifclaw(II,ID,IDP)=ifclaw(II,ID,IDP+1)
            fmiscd(II,ID,IDP,1)=fmiscd(II,ID,IDP+1,1)
            imis=INT(fmiscd(II,ID,IDP+1,1))
            do 595 IPM=2,imis+1
              fmiscd(II,ID,IDP,IPM)=fmiscd(II,ID,IDP+1,IPM)
  595       continue
  594     continue
          nfcdp(II,ID)=nfcdp(II,ID)-1
        elseif(icfoc.eq.3)then
          do 894 IDP=IP,limit-1
            tgcps(II,ID,IDP)=tgcps(II,ID,IDP+1)
            igctyp(II,ID,IDP)=igctyp(II,ID,IDP+1)
            igclaw(II,ID,IDP)=igclaw(II,ID,IDP+1)
            gmiscd(II,ID,IDP,1)=gmiscd(II,ID,IDP+1,1)
            imis=INT(gmiscd(II,ID,IDP+1,1))
            do 895 IPM=2,imis+1
              gmiscd(II,ID,IDP,IPM)=gmiscd(II,ID,IDP+1,IPM)
  895       continue
  894     continue
          ngcdp(II,ID)=ngcdp(II,ID)-1
        elseif(icfoc.eq.5)then
          do 1 IDP=IP,limit-1
            tocps(II,ID,IDP)=tocps(II,ID,IDP+1)
            ioctyp(II,ID,IDP)=ioctyp(II,ID,IDP+1)
            ioclaw(II,ID,IDP)=ioclaw(II,ID,IDP+1)
            omiscd(II,ID,IDP,1)=omiscd(II,ID,IDP+1,1)
            imis=INT(omiscd(II,ID,IDP+1,1))
            do 2 IPM=2,imis+1
              omiscd(II,ID,IDP,IPM)=omiscd(II,ID,IDP+1,IPM)
  2         continue
  1       continue
          nocdp(II,ID)=nocdp(II,ID)-1
        endif
      elseif(act(1:1).eq.'A'.or.act(1:1).eq.'a')then

C Add a control period (make period start 1 hour after the previous one.
        h(1)='The default is one period in the day and a free-float'
        h(2)='control. Periods are added to the end of the list.'
        if(icfoc.eq.0.and.limit+1.LE.MCDP)then
          nbcdp(II,ID)=nbcdp(II,ID)+1
          IDP=nbcdp(II,ID)
          tbcps(II,ID,IDP)=0.
          if(IDP.gt.1)tbcps(II,ID,IDP)=tbcps(II,ID,IDP-1)+1.0
          ibctyp(II,ID,IDP)=0
          ibclaw(II,ID,IDP)=2
          bmiscd(II,ID,IDP,1)=0.
        elseif(icfoc.eq.1.and.limit+1.LE.mcdp)then
          npcdp(II,ID)=npcdp(II,ID)+1
          IDP=npcdp(II,ID)
          if(IDP.gt.1)tpcps(II,ID,IDP)=tpcps(II,ID,IDP-1)+1.0
          ipctyp(II,ID,IDP)=0
          ipclaw(II,ID,IDP)=2
          pmiscd(II,ID,IDP,1)=0.
        elseif(icfoc.eq.2.and.limit+1.LE.mcdp)then
          nfcdp(II,ID)=nfcdp(II,ID)+1
          IDP=nfcdp(II,ID)
          if(IDP.gt.1)tfcps(II,ID,IDP)=tfcps(II,ID,IDP-1)+1.0
          ifctyp(II,ID,IDP)=1
          ifclaw(II,ID,IDP)=0
          fmiscd(II,ID,IDP,1)=2.
          fmiscd(II,ID,IDP,2)=0.
          fmiscd(II,ID,IDP,3)=1.
        elseif(icfoc.eq.3.and.limit+1.LE.mcdp)then
          ngcdp(II,ID)=ngcdp(II,ID)+1
          IDP=ngcdp(II,ID)
          if(IDP.gt.1)tgcps(II,ID,IDP)=tgcps(II,ID,IDP-1)+1.0
          igctyp(II,ID,IDP)=1
          igclaw(II,ID,IDP)=0
          gmiscd(II,ID,IDP,1)=2.
          gmiscd(II,ID,IDP,2)=0.
          gmiscd(II,ID,IDP,3)=1.
        elseif(icfoc.eq.5.and.limit+1.LE.mcdp)then
          nocdp(II,ID)=nocdp(II,ID)+1
          IDP=nocdp(II,ID)
          tocps(II,ID,IDP)=0.
          if(IDP.gt.1)tocps(II,ID,IDP)=tocps(II,ID,IDP-1)+1.0
          ioctyp(II,ID,IDP)=0
          ioclaw(II,ID,IDP)=2
          omiscd(II,ID,IDP,1)=0.
        endif
      endif
      return
      end

C ******** LSTCNTL
C List a building/plant/flow/global/optical control.  If II = 0 then
C list out the zone/loop associations. 
      SUBROUTINE LSTCNTL(itru,icfoc,II,IER)
#include "building.h"
#include "geometry.h"
#include "net_flow.h"
#include "control.h"
      
      integer lnblnk  ! function definition

C Parameters
      integer itru  ! unit for feedback
      integer icfoc ! control domain to work with
      integer II    ! zero list all otherwise list specific

      integer ncomp,ncon
      common/c1/ncomp,ncon
      common/cctl/icascf(mcom)

      character outs*124,SSTR*96

C List linkages with zones if II = 0.
      if(II.eq.0)then
        if(icfoc.eq.0)then
          call edisp(itru,' ')
          call edisp(itru,' Zone to contol loop linkages:')
          DO 28 LC=1,NCOMP
            WRITE(outs,'(a,i2,a,a,a,i2)')' zone (',LC,') ',zname(LC),
     &        ' << control ',ICASCF(LC)
            call edisp(itru,outs)
  28      CONTINUE
        elseif(icfoc.eq.1)then
        elseif(icfoc.eq.2)then
        elseif(icfoc.eq.3)then
        elseif(icfoc.eq.4)then
        elseif(icfoc.eq.5)then
        endif
        return
      endif

C List function details.
      CALL EVCNTRL(icfoc,II,1,1,'S',SSTR)
      call edisp(itru,' ')
      LN=max(1,LNBLNK(SSTR))
      WRITE(outs,65)II,SSTR(1:LN)
  65  FORMAT(' The sensor for function ',I2,' ',a)
      call edisp(itru,outs)

      CALL EVCNTRL(icfoc,II,1,1,'A',SSTR)
      LN=max(1,LNBLNK(SSTR))
      WRITE(outs,75)II,SSTR(1:LN)
  75  FORMAT(' The actuator for function ',I2,' is ',a)
      call edisp(itru,outs)
      if(icfoc.eq.0)then
        nn=nbcdt(ii)
      elseif(icfoc.eq.1)then
        nn=npcdt(ii)
      elseif(icfoc.eq.2)then
        nn=nfcdt(ii)
      elseif(icfoc.eq.3)then
        nn=ngcdt(ii)
      elseif(icfoc.eq.4)then
        nn=necdt(ii)
      elseif(icfoc.eq.5)then
        nn=nocdt(ii)
      endif
      IF(nn.EQ.0)THEN
        call edisp(itru,
     &     ' The function day types are Weekdays, Saturdays & Sundays')
      ELSE
        WRITE(outs,78)nn
  78    FORMAT(' There have been ',I2,' day types defined.')
        call edisp(itru,outs)
      ENDIF
      if(nn.eq.0)nn=3
      do 20 j=1,nn
        JJ=J
        call lstcntld(itru,icfoc,II,JJ,iier)
   20 CONTINUE
      return
      end

C ****** stfctl
C Copies from control commons to working array.
      subroutine stfctl(icfoc,i,j,k)
#include "building.h"
#include "net_flow.h"
#include "control.h"

      common/sctl/tcps,ictyp,iclaw,cm(misc)

      if(icfoc.eq.0)then
        iclaw=ibclaw(I,J,K)
        ictyp=ibctyp(I,J,K)
        tcps=tbcps(I,J,K)
        iv1=int(BMISCD(I,J,K,1))
        cm(1)=BMISCD(I,J,K,1)
        do 42 m=2,iv1+1
          cm(m)=BMISCD(I,J,K,m)
  42    continue
      elseif(icfoc.eq.1)then
        iclaw=ipclaw(I,J,K)
        ictyp=ipctyp(I,J,K)
        tcps=tpcps(I,J,K)
        cm(1)=PMISCD(I,J,K,1)
        iv1=int(PMISCD(I,J,K,1))
        do 43 m=2,iv1+1
          cm(m)=PMISCD(I,J,K,m)
  43    continue
      elseif(icfoc.eq.2)then
        iclaw=ifclaw(I,J,K)
        ictyp=ifctyp(I,J,K)
        tcps=tfcps(I,J,K)
        cm(1)=FMISCD(I,J,K,1)
        iv1=int(FMISCD(I,J,K,1))
        do 44 m=2,iv1+1
          cm(m)=FMISCD(I,J,K,m)
  44    continue
      elseif(icfoc.eq.3)then
        iclaw=igclaw(I,J,K)
        ictyp=igctyp(I,J,K)
        tcps=tgcps(I,J,K)
        cm(1)=GMISCD(I,J,K,1)
        iv1=int(GMISCD(I,J,K,1))
        do 46 m=2,iv1+1
          cm(m)=GMISCD(I,J,K,m)
  46    continue
      elseif(icfoc.eq.4)then
        iclaw=ieclaw(I,J,K)
        ictyp=iectyp(I,J,K)
        tcps=tecps(I,J,K)
        cm(1)=EMISCD(I,J,K,1)
        iv1=int(EMISCD(I,J,K,1))
        do 45 m=2,iv1+1
          cm(m)=EMISCD(I,J,K,m)
  45    continue
      elseif(icfoc.eq.5)then
        iclaw=ioclaw(I,J,K)
        ictyp=ioctyp(I,J,K)
        tcps=tocps(I,J,K)
        iv1=int(OMISCD(I,J,K,1))
        cm(1)=OMISCD(I,J,K,1)
        do 47 m=2,iv1+1
          cm(m)=OMISCD(I,J,K,m)
  47    continue
      endif
      return
      end

C ****** extrctl
C Copies from working array to control commons.
      subroutine extrctl(icfoc,i,j,k)
#include "building.h"
#include "net_flow.h"
#include "control.h"

      common/sctl/tcps,ictyp,iclaw,cm(misc)

C Make sure cm(1) is passed properly from bpfcontrl.F when a new i?claw
C is introduced
      iv1=int(cm(1))
      if(icfoc.eq.0)then
        ibclaw(I,J,K)=iclaw
        ibctyp(I,J,K)=ictyp
        tbcps(I,J,K)=tcps
        BMISCD(I,J,K,1)=cm(1)
        do 42 m=2,iv1+1
          BMISCD(I,J,K,m)=cm(m)
  42    continue
      elseif(icfoc.eq.1)then
        ipclaw(I,J,K)=iclaw
        ipctyp(I,J,K)=ictyp
        tpcps(I,J,K)=tcps
        PMISCD(I,J,K,1)=cm(1)
        do 43 m=2,iv1+1
          PMISCD(I,J,K,m)=cm(m)
  43    continue
      elseif(icfoc.eq.2)then
        ifclaw(I,J,K)=iclaw
        ifctyp(I,J,K)=ictyp
        tfcps(I,J,K)=tcps
        FMISCD(I,J,K,1)=cm(1)
        do 44 m=2,iv1+1
          FMISCD(I,J,K,m)=cm(m)
  44    continue
      elseif(icfoc.eq.3)then
        igclaw(I,J,K)=iclaw
        igctyp(I,J,K)=ictyp
        tgcps(I,J,K)=tcps
        GMISCD(I,J,K,1)=cm(1)
        do 46 m=2,iv1+1
          GMISCD(I,J,K,m)=cm(m)
  46    continue
      elseif(icfoc.eq.4)then
        ieclaw(I,J,K)=iclaw
        iectyp(I,J,K)=ictyp
        tecps(I,J,K)=tcps
        EMISCD(I,J,K,1)=cm(1)
        do 45 m=2,iv1+1
          EMISCD(I,J,K,m)=cm(m)
  45    continue
      elseif(icfoc.eq.5)then
        ioclaw(I,J,K)=iclaw
        ioctyp(I,J,K)=ictyp
        tocps(I,J,K)=tcps
        OMISCD(I,J,K,1)=cm(1)
        do 47 m=2,iv1+1
          OMISCD(I,J,K,m)=cm(m)
  47    continue
      endif
      return
      end

C ******* initperi
C initperi supports setting up initial (integer) periods for
C schedules. It is passed nper and returns an array iper(30)
C with the start times of each period and iperf(3)) with finish
C times for each period. If act='e' the allow
C editing of current array values. If act='i' initialise and 
C edit.
      subroutine initperi(nper,iper,iperf,act)
      common/pophelp/h(60)
      character act*1,cstr*80,h*72
      dimension iper(30),iperf(30)

      if(nper.le.24)incr=1
      if(nper.lt.12)incr=2
      if(nper.lt.8)incr=3
      if(nper.lt.4)incr=6
      if(act.eq.'i')then
        DO 100 I = 1,nper
          if(I.eq.1)then
            iper(I)=0
          else
            iper(I)=iperf(I-1)
          endif
          iperf(I)=iper(I)+incr
          IF(iperf(I).gt.24) iperf(I)=24
100     CONTINUE
      endif

C Make up an editing string for the start of each period.
      CSTR=' '
      K=1
      DO 60 J=1,nper
        K1=K+2
        WRITE(CSTR(K:K1),'(i2)')iper(J)
        K=K1+1
   60 CONTINUE
      h(1)='If you edit in actual start times the subsequent '
      h(2)='editing tasks will be easier. '
      CALL EASKS(CSTR,' Start hour of each period',
     &  ' ',80,' 0  4   6   8','profile starts',IER,2)

C Extract the start timings and then sort out the finishes.
      K=0
      DO 61 J=1,nper
        CALL EGETWI(cstr,K,IV,0,24,'W','period start',IER)
        iper(J)=IV
   61 CONTINUE
      DO 62 J=1,nper
        if(j.ne.nper)then
          iperf(J)=iper(J+1)
        else
          iperf(J)=24
        endif
   62 CONTINUE

      return
      end


C ******* initperr
C initperi supports setting up initial (real) periods for
C schedules. It is passed nper and returns an array rper(30)
C with the start times of each period and rperf(30) with finish
C times for each period. If act='e' the allow
C editing of current array values. If act='i' initialise and 
C edit.
      subroutine initperr(nper,rper,rperf,act)
      common/pophelp/h(60)
      character act*1,cstr*248,h*72
      dimension rper(30),rperf(30)
      real incr

      incr=1.0   ! initial guess of increment in case nper > 24
      if(nper.le.24)incr=1.
      if(nper.lt.12)incr=2.
      if(nper.lt.8)incr=3.
      if(nper.lt.4)incr=6.
      if(act.eq.'i')then
        DO 100 I = 1,nper
          if(I.eq.1)then
            rper(I)=0.0
          else
            rper(I)=rperf(I-1)
          endif
          rperf(I)=rper(I)+incr
          IF(rperf(I).gt.24.0) rperf(I)=24.0
100     CONTINUE
      endif

C Make up an editing string for the start of each period.
      CSTR=' '
      K=1
      DO 60 J=1,nper
        K1=K+6
        WRITE(CSTR(K:K1),'(f6.3)')rper(J)
        K=K1+1
   60 CONTINUE
      h(1)='If you edit in actual start times the subsequent '
      h(2)='editing tasks will be easier. Currently the facility'
      h(3)='does not support more than 24 periods. '
      CALL EASKS248(CSTR,' Start time of each period',
     &  ' ',72,' 0.0  4.0  6.0  8.0 ','time starts',IER,2)

C Extract the start timings and then sort out the finishes.
      K=0
      DO 61 J=1,nper
        CALL EGETWR(cstr,K,R,0.0,24.0,'W','event start',IER)
        rper(J)=R
   61 CONTINUE
      DO 62 J=1,nper
        if(j.ne.nper)then
          rperf(J)=rper(J+1)
        else
          rperf(J)=24.0
        endif
   62 CONTINUE

      return
      end
