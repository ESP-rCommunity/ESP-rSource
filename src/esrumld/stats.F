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

C Subroutines in this file
C Mycostat - Gathers info on growth conditions
C Mycotabl - Growth conditions look up routine (TBA)
C ********************* MYCOSTAT ******************************
C Subroutine MYCOSTAT analyses the simulation data against the 
C mould growth data held in the moulds database. The subroutine
C can be called in three modes.
C 1 - Initialise
C 2 - Get data
C 3 - Final reporting
C *************************************************************

      subroutine mycostat(imod,tdb,rh)

      INCLUDE 'mould.h'

      COMMON/OUTIN/IUOUT,IUIN

      common/mglook/GROT(MLOOK,MOULD),GRORH(MLOOK,MOULD)
      common/mglook2/GROW(MLOOK,MOULD)

C Commons
      common/mstats/imstat,ifloat(mould),ifloatp(mould),imdur1(mould)
     &,imdur0(mould),i1max(mould),i0max(mould),imup(mould),
     &imdn(mould)

      common/mstats2/maxrh(mould),minrh(mould),maxt(mould),mint(mould)
     &,ioutlk(mould),nog(mould),ipor(mould),imdt(mould),igod(mould),
     &ivgd(mould),nent(mould),ient(mould)

      common/moupk/ispp(mould),a(4,mould),mldnam(mould)
      common/meqn/a0(MOULD),a1(MOULD),a2(MOULD),a3(MOULD)
      character mldnam*48,outs*124,grow*1
      real maxrh,minrh,maxt,mint

C Initialisation of parameters.
      if(imod.eq.1) then
        do 5 imld=1,mould
           ifloat(imld)=0
           ifloatp(imld)=0 
           imdur1(imld)=0
           imdur0(imld)=0 
           i1max(imld)=0
           i0max(imld)=0
           imup(imld)=0
           imdn(imld)=0
           ioutlk(imld)=0
           nog(imld)=0
           ipor(imld)=0
           imdt(imld)=0
           igod(imld)=0
           ivgd(imld)=0          
  5    continue

      elseif(imod.eq.2) then 
C Loop through each active mould.
        do 10 imld=1,mould

C Check whether the current condition in above or below the min. growth 
C curve.
           if(ispp(imld).gt.0) then
             rhg=(a(4,imld)*TDB**3)+(a(3,imld)*TDB**2)+
     &(a(2,imld)*TDB)+a(1,imld) 
             if(rh.ge.rhg) then
               ifloat(imld)=1
             else
               ifloat(imld)=0
             endif 
             if(ifloat(imld).ne.ifloatp(imld)) then
               imdur1(imld)=0
               imdur0(imld)=0
             endif
             if(ifloat(imld).gt.0) then
               imup(imld)=imup(imld)+1
               imdur1(imld)= imdur1(imld)+1
               if(imdur1(imld).gt.i1max(imld)) then
                  i1max(imld)=imdur1(imld)
               endif
             else
               imdn(imld)=imdn(imld)+1
               imdur0(imld)= imdur0(imld)+1
               if(imdur0(imld).gt.i0max(imld)) then
                  i0max(imld)=imdur0(imld)
               endif
             endif
C Store the ifloat value for the mould
             ifloatp(imld)=ifloat(imld)

C Check the current growth condition against the lookup table.
        if(tdb.gt.maxt(imld).or.tdb.lt.mint(imld).or.rh.lt.
     &0.0.or.rh.gt.100.) then
        ioutlk(imld)=ioutlk(imld)+1
        elseif(rh.le.minrh(imld)) then
          nog(imld)=nog(imld)+1
        else

C find column 
        delt=grot(2,imld)-grot(1,imld)
        delrh=grorh(1+nent(imld),imld)-grorh(1,imld)

        do 80 ipos=1,ient(imld)
           tdiff=tdb-grot(ipos,imld)
           rdiff=rh-grorh(ipos,imld)
           if(tdiff.gt.0.and.rdiff.gt.0) then
            if(tdiff.lt.delt.and.rdiff.lt.delrh) then
               iposf=ipos
               goto 77
            elseif(tdiff.lt.delt.and.rh.gt.maxrh(imld)) then
               iposf=ient(imld)-nent(imld)+ipos
               goto 77
            endif
           endif
   80     continue
           
   77     continue
          if(grow(iposf,imld).eq.'N') then
             nog(imld)=nog(imld)+1
          elseif(grow(iposf,imld).eq.'P') then
             ipor(imld)=ipor(imld)+1
          elseif(grow(iposf,imld).eq.'M') then
             imdt(imld)=imdt(imld)+1                  
          elseif(grow(iposf,imld).eq.'G') then
             igod(imld)=igod(imld)+1  
          elseif(grow(iposf,imld).eq.'V') then
             ivgd(imld)=ivgd(imld)+1  
          endif
        
        endif

        endif
  10    continue


      elseif(imod.eq.3) then 
        do 20 imld=1,mould               
           if(ispp(imld).gt.0) then
             call edisp(iuout,' ')            
             write(outs,*) 'Analysis for ',mldnam(imld)
             call edisp(iuout,outs)
             itot=imup(imld)+imdn(imld)
             tot=itot
             up=imup(imld)
             dn=imdn(imld)
             x=up/tot
             y=dn/tot
             write(outs,*) 'Conditions above curve for ',
     &x*100.,' % of the simulation.'
             call edisp(iuout,outs)
             write(outs,*) 'Conditions below curve for ',
     &y*100.,' % of the simulation'
             write(outs,*) 'Maximum time above growth curve is ',
     &i1max(imld),' timesteps.'
             call edisp(iuout,outs)
             write(outs,*) 'Maximum time below growth curve is ',
     &i0max(imld),' timesteps.'
             call edisp(iuout,outs)
             call edisp(iuout,' ')
             if(nog(imld).gt.0) then
               write(outs,*) 'NO GROWTH will occur during ',
     &nog(imld),' timesteps.'
             call edisp(iuout,outs)
             endif
             if(ipor(imld).gt.0) then
               write(outs,*) 'Growth conditions are POOR  for ',
     &ipor(imld),' timesteps.'
             call edisp(iuout,outs)
             endif
             if(imdt(imld).gt.0) then
               write(outs,*) 'Growth conditions are MODERATE  for '
     &,imdt(imld),' timesteps.'
             call edisp(iuout,outs)
             endif
             if(igod(imld).gt.0) then
               write(outs,*) 'Growth conditions are GOOD for '
     &,igod(imld),' timesteps.'
             call edisp(iuout,outs)
             endif
             if(ivgd(imld).gt.0) then
               write(outs,*) 'Growth onditions are VERY GOOD for '
     &,ivgd(imld),' timesteps.'
             call edisp(iuout,outs)
             endif
             if(ioutlk(imld).gt.0) then
C               write(outs,*) 'Conditions OUTWITH table bounds for '
C     &,ioutlk(imld),' timesteps.'
C             call edisp(iuout,outs)
             endif
           endif
  20    continue

C Reset parameters
        do 95 imld=1,mould
           ifloat(imld)=0
           ifloatp(imld)=0 
           imdur1(imld)=0
           imdur0(imld)=0 
           i1max(imld)=0
           i0max(imld)=0
           imup(imld)=0
           imdn(imld)=0
           ioutlk(imld)=0
           nog(imld)=0
           ipor(imld)=0
           imdt(imld)=0
           igod(imld)=0
           ivgd(imld)=0          
 95    continue
      endif



      return
      end
   

               


