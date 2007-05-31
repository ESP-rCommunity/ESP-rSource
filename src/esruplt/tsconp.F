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

c This file contains the following subroutines:
c   ptscin
c   tsctyp
c   ptsc1
c   ptsc2

c ******************** ptscin ********************

c This subroutine reads number of time-step control
c periods and any data associated with a time-step 
c controller type.

c Common block 'tsconp' consists of the following variables:

c iptsc   : Plant time-step controller flag (0=off, 1=on).
c nptscp  : Total number of time-step control periods.
c ptsdat  : Array holding information related to each
c           time-step controller type. The miscellaneous data
c           held in this array are as follows (? denotes period):
c ptsdat(?,1): TSC type:
c                 0: No time-step control.
c                 1: Time-step control based on component time
c                    constant.
c                 2: Time-step control based on absolute difference
c                    of the state variable(s) of any number of 
c                    components.
c ptsdat(?,2): Period start hour.
c ptsdat(?,3): User option selected.
c           If 1 & TSC type is 1 then
c             ptsdat(?,4): Component number. 
c           
c        {  If 1 & TSC is 2 then
c             ptsdat(?,4): Temperature absolute difference.
c           If 2 & TSC is 2 then
c             ptsdat(?,4): 1st phase mass flow rate abs difference.
c           If 3 & TSC is 2 then
c             ptsdat(?,4): 2nd phase mass flow rate abs difference.
c           and 
c             ptsdat(?,5): Number of components considered.
c             If ptsdat(?,5) < 'npcomp' then
c               ptsdat(?,6): 1st component number.
c               ptsdat(?,7): 2nd component number...etc.  }
c           
c        {  If 4 & TSC is 2 then
c             ptsdat(?,4): Temperature absolute difference.
c             ptsdat(?,5): 1st phase mass flow rate abs difference.
c             ptsdat(?,6): 2nd phase mass flow rate abs difference.
c           and
c             ptsdat(?,7): Number of components considered.
c             If ptsdat(?,7) < 'npcomp' then
c               ptsdat(?,8): 1st component number.
c               ptsdat(?,9): 2nd component number...etc.  }

      subroutine ptscin
      INCLUDE 'plant.h'

      common/outin/iuout,iuin
      common/pophelp/h(60)
      common/c9/npcomp,nci(mpcom),cdata(mpcom,mmiscd)
      common/tsconp/iptsc,nptscp,ptsdat(24,20)

      character outs*124,h*72
      logical ok,close0,close1,close2,close3,close31,close32,close33
      logical close34,dok
      
      mptspd=24
      mispts=20

c Initialise all array elements.
      do 2 i=1,mptspd
         do 2 j=1,mispts
            ptsdat(i,j)=0.0
    2 continue

c Get number of control periods.
    5 call easki(nptscp,' Number of plant time-step control periods?',
     &  ' ',1,'F',mptspd,'F',1,'plt t-s ctl periods',IER,7)
      if(ier.ne.0) then
         goto 5
      endif
      
c For each period get TSC type and start hour and any
c associated data.
      do 10 i=1,nptscp
         int1=int(ptsdat(i,1))
   15    write(outs,'(a,i3)') 
     &     'Enter plant time-step controller type for period ',i
         call easki(int1,outs,' ',0,'F',3,'F',1,'ptsdat(i,1)',IER,0)
         if(ier.ne.0) then
           goto 15
         endif
         ptsdat(i,1)=float(int1)
         write(outs,'(a,i3)') 
     &     'Enter plant time-step start hour for period ',i
         int1=int(ptsdat(i,2))
         call easki(int2,outs,' ',0,'W',24,'W',1,'ptsdat(i,2)',IER,0)
         if(ier.ne.0) then
            goto 15
         endif
         ptsdat(i,2)=float(int2)

c Check start hour value for this period. If less or equal
c than previous period then print error message.
         if(i.gt.1.and.ptsdat(i,2).le.ptsdat(i-1,2)) then
           write(outs,1007)
 1007      format('ERROR: Start hour must be greater than ',
     &            'start hour of previous period.')
           call edisp(iuout,outs)
           goto 15
         endif
         
c Do type 0 (i.e no time-step control).
         call eclose(ptsdat(i,1),0.00,0.0001,close0)
         call eclose(ptsdat(i,1),1.00,0.0001,close1)
         call eclose(ptsdat(i,1),2.00,0.0001,close2)
         if(close0) then
           continue

c Do type 1 (i.e time-step control if dt is less than component(s) 
c time constant.
         else if(close1) then
           call edisp(iuout,
     &  'Plant time-step control based on component(s) time constant.')
           int3=int(ptsdat(i,3))
   20      call easki(int3,'Options: 1) specify a plant component, ',
     &       '2) component with smallest t-c, 3) largest t-c ',
     &       0,'F',3,'F',1,'ptsdat(i,3)',IER,0)
           if(ier.ne.0) then
             goto 20
           endif
           ptsdat(i,3)=float(int3)

c If first option selected then get component number.
           call eclose(ptsdat(i,3),1.00,0.0001,close3)
           if(close3) then
             int4=int(ptsdat(i,4))
   80        call easki(int4,' Component number? ',' ',
     &          1,'F',npcomp,'F',1,'ptsdat(i,4)',IER,0)
             if(ier.ne.0) then
               goto 80
             endif
             ptsdat(i,4)=float(int4)
           endif
            
c Do type 2 (i.e time-step control if absolute difference of
c state variable(s) less than user specified value).
         else if(close2) then
           call edisp(iuout,'Plant time-step control based on absolute')
           call edisp(iuout,'difference of state variable(s). ')
           call edisp(iuout,'Please select a state variable:  ')
           call edisp(iuout,'  1) Temperature (C). ')
           call edisp(iuout,'  2) First phase mass flow rate (kg/s). ')
           call edisp(iuout,'  3) Second phase mass flow rate (kg/s).')
           call edisp(iuout,'  4) All.')
   30      call easki(int5,' Which option? ',' ',
     &        1,'F',4,'F',1,'ptsdat(i,3)',IER,0)
           if(ier.ne.0) then
             goto 30
           endif
           ptsdat(i,3)=float(int5)

c Case for temperature or all.
           call eclose(ptsdat(i,3),1.00,0.0001,close31)
           call eclose(ptsdat(i,3),4.00,0.0001,close34)
           if(close31.or.close34) then
  40         CALL EASKR(r1,' Temperature absolute difference? ',' ',
     &         0.0,'F',999.,'F',1.0,'abs temp diff',IER,0)
             if(ier.ne.0) then
               goto 40
             endif
             ptsdat(i,4)=r1
           endif

c Case for 1st phase mass flow rate or all.
           call eclose(ptsdat(i,3),2.00,0.0001,close32)
           call eclose(ptsdat(i,3),4.00,0.0001,close34)
           if(close32.or.close34) then
  50         CALL EASKR(r2,' 1st phase flow rate absolute difference? ',
     &         ' ',0.0,'F',999.,'F',1.0,'1st phase flow diff',IER,0)
             if(ier.ne.0) then
               goto 50
             endif
             if(close34) then 
               ptsdat(i,5)=r2
             else
               ptsdat(i,4)=r2
             endif
           endif

c Case for 2nd phase mass flow rate or all.
           call eclose(ptsdat(i,3),3.00,0.0001,close33)
           call eclose(ptsdat(i,3),4.00,0.0001,close34)
           if(close33.or.close34) then
  60         CALL EASKR(r3,' 2nd phase flow rate absolute difference? ',
     &         ' ',0.0,'F',999.,'F',1.0,'1st phase flow diff',IER,0)
             if(ier.ne.0) then
                goto 60
             endif
             if(close34) then 
               ptsdat(i,6)=r3
             else
               ptsdat(i,4)=r3
             endif
          endif
               
c Now find which component(s) state variables to be tested.
          dok=.false.
          h(1)='Timestep control can be based on test of conditions at'
          h(2)='each plant component node. If you say no you will be '
          h(3)='asked to specify which nodes. '
          CALL ASKOK('Do you wish to test state variable(s) of each',
     &        ' component? ',OK,dok,3)
          if(.NOT.ok)goto 74

C Case for all components in network considered.
          call eclose(ptsdat(i,3),4.00,0.0001,close34)
          if(close34) then 
            ptsdat(i,7)=npcomp
          else
            ptsdat(i,5)=npcomp
          endif
          goto 10

c If not all components considered then get number of 
c components and a list of component numbers.
   74     call easki(m,' Number of components to consider? ',' ',
     &        1,'F',npcomp,'F',1,'ptsdat(i,5)',IER,0)
          if(ier.ne.0) then
            goto 74
          endif
          ptsdat(i,5)=float(m)

c Get list of component numbers.
   90     continue
          do 95 ii=1,m
            call easki(n,' Index of component? ',' ',
     &        1,'F',npcomp,'F',1,'ptsdat(i,5+ii)',IER,0)
            if(ier.ne.0) then
              goto 90
            endif
            ptsdat(i,5+ii)=float(n)
   95     continue
        endif
   10 continue

      return            
      end

c ******************** tsctyp ********************

c This subroutine finds which TSC type is active at the present time.
      subroutine tsctyp(ihrp,itctyp,nptspd)

      common/tsconp/iptsc,nptscp,ptsdat(24,20)

c Initialise itctyp.
      itctyp=0

c See which period is active if more than one is specified.
      if(nptspd.lt.nptscp.and.ihrp.gt.ptsdat(nptspd+1,2))
     & nptspd=nptspd+1

c Get plant TSC type for current period.      
      if(ihrp.gt.ptsdat(nptspd,2)) itctyp=INT(ptsdat(nptspd,1))
    
      return
      end

c ******************** ptsc1 ********************

c This subroutine handles type 1 plant time-step controller.
c Time-step reduction will take place until the time-step 
c value is less than a component time constant. The component  
c selected is one of the following:
c
c  1) Component is specified by the user.
c  2) Component time constant is the smallest.
c  3) Component time constant is the largest.

      subroutine ptsc1(itstpp,ratiop,nptspd)
      INCLUDE 'plant.h'

      common/tsconp/iptsc,nptscp,ptsdat(24,20)
      COMMON/PERS/ISD1,ISM1,ISD2,ISM2,ISDS,ISDF,NTSTEP
      COMMON/Pctime/TIMSEC
      common/pctstp/ntstpp
      common/pctc/tc(mpcom)
      common/c9/npcomp,nci(mpcom),cdata(mpcom,mmiscd)
      
      integer ratiop
      logical close31,close32,close33

c If component specified then get its number.
      call eclose(ptsdat(nptspd,3),1.00,0.0001,close31)
      call eclose(ptsdat(nptspd,3),2.00,0.0001,close32)
      call eclose(ptsdat(nptspd,3),3.00,0.0001,close33)
      if(close31) then
         icomp=int(ptsdat(nptspd,4))

c If smallest time constant selcted then find component
c with smallest TC value.
      else if(close32) then
         val=tc(1)
         icomp=1
         do 10 i=1,npcomp
            if(tc(i).lt.val) then
               val=tc(i)
               icomp=i
            endif
   10    continue

c If largest time constant selcted then find component
c with largest TC value.
      else if(close33) then
         val=tc(1)
         icomp=1
         do 20 i=1,npcomp
            if(tc(i).gt.val) then
               val=tc(i)
               icomp=i
            endif
   20    continue
      endif

c Now reduce time-step until it is smaller than time constant 
c of desired component.
      if(tc(icomp).gt.0.0.and.timsec.gt.tc(icomp)) then
   30    ntstpp=ntstpp*2
         timsec=3600./(float(ntstep)*float(ntstpp))
         if(timsec.gt.tc(icomp)) goto 30

c Calculate ratio of new ntstpp over old itstpp.
         ratiop=ntstpp/itstpp
      endif

      return
      end

c ******************** ptsc2 ********************

c This subroutine handles type 2 plant time-step controller.
c Time-step reduction takes place until the difference in
c the average value of the state variable(s) at this hour and
c the previous hour is less than or equals the user specified
c absolute difference value. Remember that the time-step value
c at this hour is smller than the previuos hour.
c
      subroutine ptsc2(itstpp,ratiop,iterp)
      INCLUDE 'plant.h'

      common/tsconp/iptsc,nptscp,ptsdat(24,20)
      common/pctstp/ntstpp
      COMMON/Pctime/TIMSEC
      COMMON/PCVAL/CSVF(MPNODE,MPVAR),CSVP(MPNODE,MPVAR)
      COMMON/PCVAR/PCTF(MPCON),PCRF(MPCON),PUAF(MPNODE),PCQF(MPNODE),
     &             PCNTMF(MPCOM),
     &             PCTP(MPCON),PCRP(MPCON),PUAP(MPNODE),PCQP(MPNODE),
     &             PCNTMP(MPCOM)
      COMMON/PCDAT/PCDATF(MPCOM,MPCDAT),PCDATP(MPCOM,MPCDAT)
      COMMON/PTIME/PTIMEP,PTIMEF
      COMMON/CLIMIP/QFPP,QFFP,TPP,TFP,QDPP,QDFP,VPP,VFP,DPP,DFP,HPP,HFP
      COMMON/CLMPHG/HEXTPP,HEXTFP,GEXTPP,GEXTFP,TWBPP,TWBFP
      common/c9/npcomp,nci(mpcom),cdata(mpcom,mmiscd)
      COMMON/C13PS/NPMCOE,NPNOD,NPMTYP
      COMMON/tempvp/tvp1(40),tvp2(5,MPNODE),tvp3(2,MPNODE,MPVAR),
     &              tvp4(mpcom,mpcdat)

      COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
      COMMON/LIBRCP/IRCPLP

      integer ratiop

      data ipass/0/

      if(ipass.eq.0) then
C Save begining of hour values of variuos quantities.
         tvp1(1) =NSINC
         tvp1(2) =QFFP
         tvp1(3) =TFP
         tvp1(4) =QDFP
         tvp1(5) =VFP
         tvp1(6) =DFP
         tvp1(7) =HFP
         tvp1(8) =TWBFP
         tvp1(9) =GEXTFP
         tvp1(10)=HEXTFP
         tvp1(11)=QFPP
         tvp1(12)=TPP
         tvp1(13)=QDPP
         tvp1(14)=VPP
         tvp1(15)=DPP
         tvp1(16)=HPP
         tvp1(17)=TWBPP
         tvp1(18)=GEXTPP
         tvp1(19)=HEXTPP
         tvp1(20)=ptimef
         tvp1(21)=IRCPLP

c Save plant node related present time values.
         DO 10 i=1,NPNOD
           tvp2(1,i)=puaf(i)
           tvp2(2,i)=pcqf(i)
           tvp2(3,i)=pctf(i)
           tvp2(4,i)=pcrf(i)
           tvp2(5,i)=pcntmf(i)

c Save present and future state variable's present time values.
           DO 11 J=1,MPVAR
             tvp3(1,I,J)=CSVP(I,J)
             tvp3(2,I,J)=CSVF(I,J)
   11      CONTINUE
   10    CONTINUE

c Also save specific component variables.
         do 20 i=1,npcomp
            do 20 j=1, mpcdat
               tvp4(i,j)=pcdatp(i,j)
   20    continue
      endif

      ratiop=1
      iterp=0

      return
      end
