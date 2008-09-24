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

C PSYCHART draws a window with a simplified version of
C the standard  psychrometric chart based on the current
C user focus defined by act. act = 'f' for a flow network,
C act = 'z' for zone air status, act = 'p' for plant
C component status, act = 'u' for user supplied points.
      SUBROUTINE PSYCHART(act)

#include "plant.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/PPERS/IPSD1,IPSM1,IPSD2,IPSM2,IPSDS,IPSDF,NPTS,ipsav
      COMMON/PERO/IOD1,IOM1,IOH1,IOD2,IOM2,IOH2,IODS,IODF,NOUT,IAV
      COMMON/SET1/IYEAR,IBDOY,IEDOY,IFDAY,IFTIME
      common/prav/IDAVER

      common/grphlim/xmin,xmax,ymin,ymax
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PLTRC/IPLOTREC(MPCOM,MNODEC),IPLABEL  
      common/pcnam/pcname(mpcom)
      common/pcdat/ CSV1M(mnodec),CSV2M(mnodec),CSV3M(mnodec),
     &              NNODE(mpcom),nrecpc(mpcom)
      common/ntype/isv(mpcom,mnodec)
      common/psychro/PSY(MPCOM,mnodec,2)
      COMMON/PSY3/PCNAM1(MPCOM)
      COMMON/PSY2/IPSYTS,ICPLOT
      dimension  mcomp(mpcom), ipcndx(mpcom,2)

      dimension UTD(20),URH(20)     
      CHARACTER*23 ITEM(12)
      CHARACTER H*72,HOLD*24,act*1,pcname*15,pcnam1*15,head*124
      CHARACTER DESCRH*5,outs*124,DESCR*7,DESCR1*10
      CHARACTER ETEXT*124,PDESCR*60,SDESCR*44
      logical ok,dok,ask
#ifdef OSX
      integer iside,isize,ifont     ! passed to viewtext
#else
      integer*8 iside,isize,ifont     ! passed to viewtext
#endif

      SMALL=1E-20
      PATMOS=1013.25

C Depending on domain, set inclusion of point labels.
      if(act.eq.'f')then
        ICPLOT=1
      elseif(act.eq.'z')then
        ICPLOT=0
      elseif(act.eq.'u')then
        ICPLOT=0
      elseif(act.eq.'p')then
        ICPLOT=1
      endif

C Clear local array.
      IUP=0
      do 96 i=1,20
        UTD(i)=10.0
        URH(i)=50.0
  96  continue

C Scale the axes of the chart (initial values)
      Xmin=0.0
      Xmax=30.0
      Ymin=0.0
      Ymax=16.0
      enthmax=(enthp2(Xmax,Ymax/1000.))
      enthminx= (enthp2(Xmin,Ymax/1000.))    
      enthminy= (enthp2(Xmin,Ymin/1000.)) 
      enthmaxy= (enthp2(Xmin,Ymax/1000.)) 
 10   call psychartbackground(Xmin,Xmax,Ymin,Ymax,enthmax,
     &  enthminx,enthminy,enthmaxy)

C Setup menu text.
C << the menu to allow the timestep to be incremented or decremented
C << and then if those are called it just resets the timestep and
C << forces a re-draw.
      ITEM(1)     ='1 display period     '
      if(act.eq.'f')then
        ITEM(2)   ='2 select flow nodes '
        if(icplot.eq.0)then
          ITEM(3) ='3 node labels >> off'
        else
          ITEM(3) ='3 node labels >> on'
        endif
      elseif(act.eq.'z')then
        ITEM(2)   ='2 select zones      '
        if(icplot.eq.0)then
          ITEM(3) ='3 zone labels >> off'
        else
          ITEM(3) ='3 zone labels >> on'
        endif
      elseif(act.eq.'u')then
        ITEM(2) ='2 user defined points'
        if(icplot.eq.0)then
          ITEM(3) ='3 user labels >> off'
        else
          ITEM(3) ='3 user labels >> on'
        endif
      elseif(act.eq.'p')then
        ITEM(2)   ='2 select components '
        if(icplot.eq.0)then
          ITEM(3) ='3 comp. labels >> off'
        else
          ITEM(3) ='3 comp. labels >> on'
        endif
      endif
      ITEM(4) ='4 change Scales        '
      ITEM(5) ='  ____________________ '
      ITEM(6) ='5 display one timestep '
      ITEM(7) ='6 display sequence     '
      ITEM(8) ='7 clear display        '
      ITEM(9) ='  ____________________ '
      ITEM(10)='                       '
      ITEM(11)='h help                 '
      ITEM(12)='- exit                 '

C Display menu.
 2    NITMS=12
      INO=-2

C Instanciate h() strings for the menu.
      H(1)='This psychrometric plotting facility allows'
      H(2)='the psycrometric state of an AC plant component'
      H(3)='or network to be plotted on a psychrometric   '
      H(4)='chart. The user can select which plant nodes'
      H(5)='should be plotted on the chart. Any component'
      H(6)='linkages will be detected and drawn by the '
      H(7)='program. The user must select the timestep from '
      H(8)='which the results are taken.'
      H(9)='     - Click to return to program-         '

 5    CALL EMENU('Psychrometric chart',ITEM,NITMS,INO)
      IF(INO.EQ.1)THEN

C Display period.
        call mooper
      elseif(ino.eq.2)then

C Point selection depening on domain.
        if(act.eq.'f')then

C flow nodes << to be done >>
          call edisp(iuout,'Still working on this...')
          goto 2
        elseif(act.eq.'z')then

C zone nodes << to be done >>
          call edisp(iuout,'Still working on this...')
          goto 2
        elseif(act.eq.'u')then
          if(IUP.ne.0)then
            h(1)='System components performance can be plotted on the'
            h(2)='chart or the user can supply temperature and humidity'
            h(3)='data to be plotted. '
            dok=.true.
            call askok('Re-plot previous points?',' ',ok,dok,3)
            if(ok)then
              ask=.false.
            else
              ask=.true.
            endif
          else
            ask=.true.
          endif
          if(ask)then
            h(1)='You can supply up to 20 sets of DbT and RH. '
            CALL EASKI(IUP,'How many points to plot?',' ',
     &             1,'F',20,'W',2,'points to plot',IER,1)
            do 196 i=1,IUP
              write(hold,'(2F7.3)') UTD(i),URH(i)
              CALL EASKS(HOLD,' ',' dbT and RH : ',
     &          24,' 10.  50.0 ','dbT and RH',IER,6)
              K=0
              CALL EGETWR(HOLD,K,UTD(i),-10.0,90.0,'F','dbT',IER)
              CALL EGETWR(HOLD,K,URH(i),0.0,100.0,'F','RH',IER)
  196       continue
          endif
        elseif(act.eq.'p')then

C Select plant components that support moist air.
          k=0
          do 30 i=1,npcomp
            do 25 j=1, nnode(i) 
              if(isv(i,j).eq.21) then
                k=k+1
                pcnam1(k)=pcname(i)
                ipcndx(k,1)=i
                ipcndx(k,2)=j            
              endif
  25        continue
  30      continue

C Display these components on a menu.
          if(k.eq.0) then
            call usrmsg('No components found ! ',' ','W')
            goto 2
          endif
          head='Components'
          call askpsycmp(head,is,k,IER)
          if(is.eq.0) goto 2

C If 'all items selection is picked'.
          IF(IS.EQ.99) THEN 
            IMDRW1=1
            IMDRW2=k
          ELSE
            IMDRW1=is
            IMDRW2=is
          ENDIF
          call edisp(iuout,'You can now display your selection.')
C          CALL PSYREAD
        endif
      elseif(ino.eq.3)then

C Toggle labels.
        icplot=icplot+1
        if(icplot.eq.2)icplot=0
      elseif(ino.eq.4)then

C Change scale.
        call easkr(val,'Max temp C','for chart ',0.0,'F',60.,'-',
     &  60.0,'max temp',IER,0)
        Xmax=val
        call easkr(val,'Max moisture content g/kg','for chart '
     &  ,0.0,'F',30.,'-',30.0,'max mcnt',IER,0)
        Ymax=val

C Calculate the maximum enthalpy for the chart
        enthmax=(enthp2(Xmax,Ymax/1000.))
        enthminx= (enthp2(Xmin,Ymax/1000.))    
        enthminy= (enthp2(Xmin,Ymin/1000.)) 
        enthmaxy= (enthp2(Xmin,Ymax/1000.)) 
        call psychartbackground(Xmin,Xmax,Ymin,Ymax,enthmax,
     &    enthminx,enthminy,enthmaxy)
        GOTO 2
      elseif(ino.eq.5)then
        GOTO 2
      elseif(ino.eq.6)then

C One timestep, initial value is at the start of the display period.
C Make up a string describing this time.
        if(IMDRW2.eq.0)then
          call edisp(iuout,'Nothing to plot yet...')
          goto 2
        endif
        ENTERVAL=1.0/FLOAT(NPTS)/24.0
        if(idaver.eq.0) then
           ctime=float(ioh1-1)/24.0+ENTERVAL/2.+float(iods)
        else
           ctime=float(ioh1-1)/24.0+enterval+float(iods)
        endif
        CALL STDATE(IYEAR,iods,DESCR,DESCR1)
        itstep=((ioh1-1)+(iods-IPSDS)*24)*npts
        if(itstep.eq.0)itstep=1
        itstepstart=itstep
        call DASYSTIME(itstep,TIME)
        MINH=INT(time)
        MINU=INT((time-AINT(time))*60.+0.5)
        if(MINH.lt.10)then
          IF(MINU.LT.10)THEN
            WRITE(DESCRH,'(a1,i1,a2,i1)')'0',INT(TIME),'h0',MINU
          ELSE
            WRITE(DESCRH,'(a1,i1,a1,i2)')'0',INT(TIME),'h',MINU
          ENDIF
        else
          IF(MINU.LT.10)THEN
            WRITE(DESCRH,'(i2,a2,i1)')INT(TIME),'h0',MINU
          ELSE
            WRITE(DESCRH,'(i2,a1,i2)')INT(TIME),'h',MINU
          ENDIF
        endif
        write(outs,'(a,i4,4a)') 'The current timestep (',itstep,') is ',
     &    descrh,' on ',DESCR1
        call edisp(iuout,outs)
        h(1)='Choose the simulation timestep to use.'
        CALL EASKI(itstep,'Which timestep number? ',' ',
     &    1,'-',10000,'-',1,'Timestep Number',IER,1)
        IPSYTS=ITSTEP
  77    idift=itstep-itstepstart
        ctime=ctime+(enterval*idift)*float(nout)
        iday=INT(ctime)
        CALL STDATE(IYEAR,IDAY,DESCR,DESCR1)
        call DASYSTIME(itstep,TIME)
        MINH=INT(time)
        MINU=INT((time-AINT(time))*60.+0.5)
        if(MINH.lt.10)then
          IF(MINU.LT.10)THEN
            WRITE(DESCRH,'(a1,i1,a2,i1)')'0',INT(TIME),'h0',MINU
          ELSE
            WRITE(DESCRH,'(a1,i1,a1,i2)')'0',INT(TIME),'h',MINU
          ENDIF
        else
          IF(MINU.LT.10)THEN
            WRITE(DESCRH,'(i2,a2,i1)')INT(TIME),'h0',MINU
          ELSE
            WRITE(DESCRH,'(i2,a1,i2)')INT(TIME),'h',MINU
          ENDIF
        endif
        write(outs,'(a,i4,4a)') 'The timestep (',itstep,') is ',
     &    descrh,' on ',DESCR1
        call edisp(iuout,outs)

C Refresh the graph, add timestep title and then plot the data.
        call psychartbackground(Xmin,Xmax,Ymin,Ymax,enthmax,
     &  enthminx,enthminy,enthmaxy)
        iside=1
        isize=0
        ifont=1
        call viewtext(outs,iside,isize,ifont)
        if(act.eq.'u')then
          if(iup.gt.1)then
            do 197 i=1,iup
              X=UTD(i)
              GS=1000.*(HUMRT1(UTD(i),URH(i),PATMOS,IOPT))
              IPNZO= 100 + i
              CALL PSYPLOT(X,GS,ipnzo,0)
 197        continue
          endif
        elseif(act.eq.'f')then
          call edisp(iuout,'Still working on this...')
        elseif(act.eq.'z')then
          call edisp(iuout,'Still working on this...')
        elseif(act.eq.'p')then

C Draw selected components for the current timestep.
          DO 99 IMDRW=IMDRW1,IMDRW2
            INDX=IPCNDX(IMDRW,1)
            NSND=IPCNDX(IMDRW,2)

C indx is the plant component number in the network.
            mcomp(1)=indx

C Obtain the data for this selected component
            CALL fnddat(itstep,mcomp(1),ier)
            if(ier.ne.0)then
              write(6,*) 'An error in fnddat ',itstep,mcomp(1),ier
              goto 99
            endif

C Save the components psycrometric cordinates in an array.
C Ignore nodes with an incompatible ISV number.
            IF(isv(MCOMP(1),NSND).eq.21) then  
               PSY(MCOMP(1),NSND,1)=CSV1M(NSND)
               PSY(MCOMP(1),NSND,2)=CSV3M(NSND)/CSV2M(NSND)*1000.
               X=PSY(MCOMP(1),NSND,1)
               Y=PSY(MCOMP(1),NSND,2)

               write(outs,*) 'plotting for component ',pcname(indx),
     &           ' node ',nsnd
               call usrmsg(outs,' ','-')
              
               CALL PSYPLOT(x,y,MCOMP(1),NSND)
             ELSEIF(isv(MCOMP(1),NSND).ne.21) THEN
               PSY(MCOMP(1),NSND,1)=-99.
               PSY(MCOMP(1),NSND,2)=-99.
             ELSEIF(CSV2M(NSND).lt.SMALL)  THEN
               call usrmsg('There is no flow at this timestep !',' ',
     &           '-')
             ELSE
              call usrmsg('PSYREAD: error getting component data!',
     &          ' ','-')
             ENDIF
  99      CONTINUE
        endif

C Ask whether user wants to step forwards, backwards or return.
        h(1) ='Step forwards or backwards or continue.'
        call EASKABC(' ','Options:',
     &    'previous ts','next ts','continue',IW,1)
        if(iw.eq.1)then
          if(itstep.gt.2)then
            itstep=itstep-1
            IPSYTS=ITSTEP
            goto 77
          endif
        elseif(iw.eq.2)then
          itstep=itstep+1
          IPSYTS=ITSTEP
          goto 77
        elseif(iw.eq.3)then
          continue
        endif
      elseif(ino.eq.7)then

C Several timesteps.
C TSTART and TFINISH - start and finish timesteps from 00h00 on the 
C first day of output.
        if(IMDRW2.eq.0)then
          call edisp(iuout,'Nothing to plot yet...')
          goto 2
        endif
        CALL HDDATE(PDESCR)
        CALL HDSTEP(SDESCR)
        WRITE(ETEXT,'(3A)')PDESCR(1:lnblnk(PDESCR)),' ',
     &    SDESCR(11:lnblnk(SDESCR))
        ENTERVAL=1.0/FLOAT(NPTS)/24.0
        if(idaver.eq.0) then
          ctime=float(ioh1-1)/24.0+ENTERVAL/2.+float(iods)
        else
          ctime=float(ioh1-1)/24.0+enterval+float(iods)
        endif
        istart=((ioh1-1)+(iods-IPSDS)*24)*npts
        if(istart.eq.0)istart=1
        iend  =((ioh2)+(iodf-IPSDS)*24)*npts-1
        TSTART=float(istart)
        TFINSH=float(iend)
        write(6,*) 'start and finish for ',TSTART,TFINSH,ctime
        do 200 ii=istart, iend, nout
          ctime=ctime+(enterval*float(nout))
          iday=INT(ctime)
          if(iday.gt.0)then
            CALL STDATE(IYEAR,IDAY,DESCR,DESCR1)
          else
            CALL STDATE(IYEAR,iods,DESCR,DESCR1)
          endif
          call DASYSTIME(ii,TIME)
          MINH=INT(time)
          MINU=INT((time-AINT(time))*60.+0.5)
          if(MINH.lt.10)then
            IF(MINU.LT.10)THEN
              WRITE(DESCRH,'(a1,i1,a2,i1)')'0',INT(TIME),'h0',MINU
            ELSE
              WRITE(DESCRH,'(a1,i1,a1,i2)')'0',INT(TIME),'h',MINU
            ENDIF
          else
            IF(MINU.LT.10)THEN
              WRITE(DESCRH,'(i2,a2,i1)')INT(TIME),'h0',MINU
            ELSE
              WRITE(DESCRH,'(i2,a1,i2)')INT(TIME),'h',MINU
            ENDIF
          endif
          write(6,*) ' *200 ii ctime iday time ',ii,ctime,iday,time
          write(outs,'(a,i4,4a)') 'The timestep (',ii,') is ',
     &      descrh,' on ',DESCR1

C Refresh the graph, add timestep title and then plot the data.
          call psychartbackground(Xmin,Xmax,Ymin,Ymax,enthmax,
     &      enthminx,enthminy,enthmaxy)
          iside=1
          isize=0
          ifont=1
          call viewtext(etext,iside,isize,ifont)
          iside=2
          call viewtext(outs,iside,isize,ifont)

C Draw selected components for the current timestep.
          DO 98 IMDRW=IMDRW1,IMDRW2
            INDX=IPCNDX(IMDRW,1)
            NSND=IPCNDX(IMDRW,2)

C indx is the plant component number in the network.
            mcomp(1)=indx

C Obtain the data for this selected component
            CALL fnddat(ii,mcomp(1),ier)
            if(ier.ne.0)then
              write(6,*) 'An error in fnddat ',ii,mcomp(1),ier
              goto 98
            endif

C Save the components psycrometric cordinates in an array.
C Ignore nodes with an incompatible ISV number.
            IF(isv(MCOMP(1),NSND).eq.21) then  
               PSY(MCOMP(1),NSND,1)=CSV1M(NSND)
               PSY(MCOMP(1),NSND,2)=CSV3M(NSND)/CSV2M(NSND)*1000.
               X=PSY(MCOMP(1),NSND,1)
               Y=PSY(MCOMP(1),NSND,2)
               CALL PSYPLOT(x,y,MCOMP(1),NSND)

C One for a fraction of a second. 
               call forceflush()
               call pausems(50)
             ELSEIF(isv(MCOMP(1),NSND).ne.21) THEN
               PSY(MCOMP(1),NSND,1)=-99.
               PSY(MCOMP(1),NSND,2)=-99.
             ELSEIF(CSV2M(NSND).lt.SMALL)  THEN
               call usrmsg('There is no flow at this timestep !',' ',
     &           '-')
             ELSE
              call usrmsg('PSYREAD: error getting component data!',
     &          ' ','-')
             ENDIF
  98      CONTINUE

C One for a fraction of a second.
          call forceflush()
          call pausems(300)
  200   continue
      elseif(ino.eq.8)then

C Reset plot records, clear the graphic display and jump back
C to label 10 and redraw the background lines.
        CALL startbuffer
        DO 998 I=1,MPCOM
          DO 999 J=1,MNODEC
             IPLOTREC(I,J)=0
 999      CONTINUE
 998    CONTINUE
        GOTO 10       
      elseif(ino.eq.11)then
        CALL PHELPD('Psychrometric chart',9,'-',0,0,IER)
      ELSEIF(INO.EQ.12)THEN

C When exiting, clear and then reset the plot records and re-establish
C the initial graphic context.
        CALL startbuffer
        DO 101 I=1,MPCOM
          DO 102 J=1,MNODEC
             IPLOTREC(I,J)=0
 102      CONTINUE
 101    CONTINUE
        RETURN        
      ELSE
        GOTO 5
      ENDIF
      GOTO 2
      
      END

************** PSYPLOT ********************
C Subroutine PSYPLOT plots a point onto the psychrometric
C chart, but first does some bounds checking.
C IPCOMP: zero is ext dbT, > zero is component index, < zero is zone.
      SUBROUTINE PSYPLOT(X,Y,IPCOMP,NOD)

#include "plant.h"
C#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/C10/NPCON,IPC1(MPCON),IPN1(MPCON),IPCT(MPCON),
     &           IPC2(MPCON),IPN2(MPCON),PCONDR(MPCON),PCONSD(MPCON,2)
      common/psychro/PSY(MPCOM,mnodec,2)
      COMMON/PLTRC/IPLOTREC(MPCOM,MNODEC),IPLABEL
      common/grphlim/xmin,xmax,ymin,ymax
      common/pcdat/ CSV1M(mnodec),CSV2M(mnodec),CSV3M(mnodec),
     &              NNODE(mpcom),nrecpc(mpcom)
      COMMON/PSY2/IPSYTS,ICPLOT
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

      CHARACTER PUTS*15,outs*124
      logical colok

      colok=.false.
      if(nzonec.ge.24)colok=.true.

      IF(X.GT.Xmax.OR.X.LT.Xmin) THEN
        if(x.gt.Xmax)x=Xmax
        if(x.lt.Xmin)x=Xmin
        call usrmsg('X-value reset within bounds!',' ','-')
      ELSEIF(Y.GT.Ymax.OR.Y.LT.Ymin) THEN
        if(y.gt.ymax)y=ymax
        if(y.lt.ymin)y=ymin
        call usrmsg('Y-value reset within bounds!',' ','-')
      ELSE
        CALL ETPLOT(X,Y,0,0)
        IF(ICPLOT.EQ.1) THEN
          IF(IPCOMP.GT.100) THEN
            WRITE(PUTS,*) IPCOMP-100
            call u2pixel(x,y,ixs,iys)
            if(colok)then
              icm=IPCOMP-100
              if(ICM.gt.nzonec) ICM=(IPCOMP-100)-nzonec
              call winscl('z',ICM)
            endif
            call wstxpt(ixs+4,iys-4,puts)
            if(colok)call winscl('-',0)
            call forceflush()
          ELSEIF(IPCOMP.GT.0.AND.IPCOMP.LE.100) THEN 

C Keep track of components plotted
            IPLOTREC(IPCOMP,NOD)=1 
            WRITE(PUTS,*) IPCOMP
            call u2pixel(x,y,ixs,iys)
            if(colok)then
              icm=IPCOMP
              if(ICM.gt.nzonec) ICM=IPCOMP-nzonec
              call winscl('z',ICM)
            endif
            call wstxpt(ixs+4,iys-4,puts)
            if(colok)call winscl('-',0)
            call forceflush()
          ELSEIF(IPCOMP.EQ.0) THEN
            WRITE(PUTS,*) 'Ext db'
            call u2pixel(x,y,ixs,iys)
            icm=0
            call wstxpt(ixs+4,iys-4,puts)
          ELSEIF(IPCOMP.LT.0) THEN
            WRITE(PUTS,*) 'Zone',-IPCOMP
            call u2pixel(x,y,ixs,iys)
            icm=0
            call wstxpt(ixs+4,iys-4,puts)
          ENDIF
        ENDIF 
        call u2pixel(x,y,ix,iy)
        if(colok.and.icm.ne.0)call winscl('z',ICM)
        call ecirc(ix,iy,3,1)
        if(colok)call winscl('-',0)
        IF(IPCOMP.GT.0.AND.IPCOMP.LE.100) IPLOTREC(IPCOMP,NOD)=1
        call forceflush()
      ENDIF

 
C Check to see if the component is connected to any others 
C on the graph. If so draw a line between them.
      DO 20 I=1,NPCON
        IF(IPC1(I).EQ.IPCOMP)THEN
          DO 30 J=1,NPCOMP
            IF(IPC2(I).EQ.J) THEN
              DO 40 K=1,NNODE(J)
                IF(IPLOTREC(J,K).EQ.1.AND.IPCT(I).NE.4) 
     &             CALL ETPLOT(PSY(J,K,1),PSY(J,K,2),-350,0)
  40          CONTINUE
            ENDIF            
  30      CONTINUE
        ENDIF           
  20  CONTINUE

      DO 50 I=1,NPCON
        IF(IPC2(I).EQ.IPCOMP)THEN
          DO 60 J=1,NPCOMP
            IF(IPC1(I).EQ.J) THEN
              DO 70 K=1,NNODE(J)
                IF(IPLOTREC(J,K).EQ.1.AND.IPCT(I).NE.4) 
     &             CALL ETPLOT(PSY(J,K,1),PSY(J,K,2),-350,0)
  70          CONTINUE
            ENDIF            
  60      CONTINUE
        ENDIF           
  50  CONTINUE
          
      RETURN
      END 

C ********************** ASKPSYCMP
C ASKPCMP presents a list of plant components returning the index IS.
C If MOD = 'M' then include option to add an item then the appropriate
C interaction is begun.
      SUBROUTINE ASKPSYCMP(head,IS,NEL,IER)

#include "epara.h"
#include "plant.h"
#include "building.h"
      COMMON/C9/NPCOMP,NCI(MPCOM),CDATA(MPCOM,MMISCD)
      COMMON/PPERS/IPSD1,IPSM1,IPSD2,IPSM2,IPSDS,IPSDF,NPTS,ipsav
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/GET2/XDUM(MTS),XDUM1(MTS),GVAL(MTS)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
      COMMON/PSY3/PCNAM1(MPCOM)
      COMMON/PSY2/IPSYTS,ICPLOT
      COMMON/ZONPIK/NZ,NZNO(MCOM)
      COMMON/DEFLT/IDEFLT
      DIMENSION SVERT(50),TMS(MS),TMSO(MS)
      CHARACTER PCNAM1*15
      character*124 head
      CHARACTER H*72,SVERT*23,KEY*1
      LOGICAL SELECT

      PATMOS=1013.25

C Initialise node menu variables based on window size. 
C ISVERT is the menu position, MSVERT the current number 
C of menu lines based on selected list.
  3   IER=0
      SELECT=.FALSE.
      MHEAD=0
      MCTL=6
      ILEN=NEL

C Initial menu entry setup.
      IPACT=CREATE
      CALL EKPAGE(IPACT)
   92 IER=0
      ISVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          WRITE(SVERT(M),'(A1,1X,A15)')KEY,PCNAM1(L)
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MSVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        SVERT(M+1)='  ________________ '
      ELSE
        WRITE(SVERT(M+1),'(A,I2,A,I2)')'0 Page: ',IPM,' :',MPM 
      ENDIF

      SVERT(M+2)  ='* all items        '
      SVERT(M+3)  ='@ add Ambient+Zone '

      SVERT(M+4)  ='                   '
      SVERT(M+5)  ='? help             '
      SVERT(M+6)  ='- exit             '

C Instanciate h() strings for the menu.
      H(1)='The list shows the (currently) available components '
      H(2)='defined in the plant network which can be plotted on '
      H(3)='the chart. Each selected component is drawn as a  '
      H(4)='diamond on the chart, linkages between components are'
      H(5)='drawn as a thick black line. Note that ambient and   '
      H(6)='zone conditions will only be displayed if a building'
      H(7)='results library is available for the simulation.'
      H(8)='If a component is selected and no mark appears on the'
      H(9)='chart then either the timestep selected is outwith  '
      H(10)='the simulation period or the component has the same '
      H(11)='psychrometric state as one already displayed.'
      H(12)='  '

C Display the menu.
      CALL EMENU(head,SVERT,MSVERT,ISVERT)
      IF(ISVERT.LE.MHEAD)THEN
        ISVERT=-1
        goto 3
      ELSEIF(ISVERT.EQ.MSVERT)THEN

C If no selection has been made before exit then return with 0.
        IF(.NOT.SELECT)IS=0
        RETURN
      ELSEIF(ISVERT.EQ.(MSVERT-1))THEN
        CALL PHELPD('node connection selection',12,'-',0,0,IER)
        GOTO 3
      ELSEIF(ISVERT.EQ.(MSVERT-2))THEN

C Script directed input if not in graphic mode.
        if(MMOD.EQ.8)goto 3
        IV=1
  96    CALL EASKI(IV,' ',' Index of component ? ',
     &         1,'F',NPCOMP,'F',1,'script comp',IER,0)
        if(IER.NE.0)goto 96
        IS=IV
        RETURN

C Add ambient and zone conditions to the plot
      ELSEIF(ISVERT.EQ.(MSVERT-3))THEN
C Obtain the building timestep number for the current plant timestep
      
        A=IPSYTS
        B=NPTS
        DAY=FLOAT(IPSDS)+((A/B)/24.)

C << next line probably should be IDY = int(DAY) >>
C        IDY=AINT(DAY)
        IDY=INT(DAY)
        JDY=INT(DAY)-IPSDS
        IBPSYTS=(IPSYTS-(JDY*24*NPTS))/(NPTS/NTS) 
        IDEFLT=0
        CALL MOZDFN
        DO 600 IZC=1,NZ

C Set chosen result set.
          ISET=ISIM
          CALL GZTMS(IDY,NZNO(IZC),ISET,IBPSYTS,TMS,TMSO,TMA,TMCON,
     &      QW1,QW2,QSA,RH)
          X=TMA
          Z=RH
          Y=1000.*HUMRT1(X,Z,PATMOS,IOPT)
          IPNZO=-NZNO(IZC)
          CALL PSYPLOT(X,Y,IPNZO,0)
600     CONTINUE

C Get extenal climate parameters.
        CALL MOCLIM(IDY,2)
        X=GVAL(IBPSYTS)
        CALL MOCLIM(IDY,28)
        Z=GVAL(IBPSYTS)
        Y=1000.*HUMRT1(X,Z,PATMOS,IOPT)
        CALL PSYPLOT(X,Y,0,0)
      ELSEIF(ISVERT.EQ.(MSVERT-4))THEN
               IS=99
               RETURN     
      ELSEIF(ISVERT.EQ.(MSVERT-5))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(ISVERT.GT.MHEAD.AND.ISVERT.LT.(MSVERT-MCTL+1))THEN

C Decode from the potential long list to the comp no via KEYIND.
        CALL KEYIND(MSVERT,ISVERT,IFOC,IO)
        SELECT=.TRUE.
        IS=IFOC
        RETURN
      ELSE
        ISVERT=-1
        goto 92
      ENDIF
      ISVERT=-2
      goto 3

      END

C ***************psychartbackground****************
C psychartbackground draws a background psychrometric chart given the
C following parameters:
C Xmin db T minimum
C Xmax db T maximum
C Ymin moisture g/kg minimum
C Ymax moisture g/kg maximum
C enthmax enthalpy kJ/kg x axis maximum
C enthminx enthalpy x axis minimum
C enthminy enthalpy y axis minimum
C enthmaxy enthalpy y axis maximum
      subroutine psychartbackground(Xmin,Xmax,Ymin,Ymax,enthmax,
     &  enthminx,enthminy,enthmaxy)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      logical close,colok,cscaleok
      CHARACTER puts*12

      integer iglib   ! if 1 then X11, if 2 then GTK, if 3 then text only.
#ifdef OSX
      integer igwid,igheight  ! for use with axiscale
      integer ipos,isize      ! for use with etlabel
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 ipos,isize      ! for use with etlabel
#endif

      PATMOS=1013.25

C Check if can draw in colour and with a range of colours.
C Colours used are nb 15 = grey40, nb 17 =grey60.
      colok=.false.
      if(nzonec.ge.24)colok=.true.
      cscaleok=.false.
      if(ncset.gt.24)cscaleok=.true.
      
      IF(MMOD.EQ.8)THEN

C Store original text screen size
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1)then
          LIMTTP=LIMTTY
          LIMITP=LIMIT
          LIMTTY=8
          LIMIT =8
          call feedbox(menuchw,2,igfw,igfh)      
          call opengdisp(menuchw,LIMTTY,2,igdw,igdh)
        endif
        call win3d(menuchw,15,15,5,5,igl,igr,igt,igb,igw,igwh)
        igw=igr-igl
        igwh=igb-igt
        igwid=igw
        igheight=igwh
      ENDIF

10    continue
      call axiscale(igwid,igheight,Xmin,Xmax,Ymin,Ymax,xsc,ysc,sca,
     &  xadd,yadd)
      CALL startbuffer

C Draw the axes of the chart.
      CALL horaxis(Xmin,Xmax,igl,igr,igb,xadd,XSC,0,'db Temp C')
      CALL vrtaxis(0.0,0.01,igl,igb,igt,yadd,YSC,0,0,'Enthalpy kJ/kg')
      CALL vrtaxis(Ymin,Ymax,igr,igb,igt,yadd,YSC,0,1,'Moisture g/kg')

C Store the scaling parameters used in drawing the axes
      CALL linescale(igl,xadd,XSC,igb,yadd,YSC)
C Draw the dry-bulb temperature lines
      CALL etplot(0.,0.,0,0)

C Set the intervals for the chart.
      IF(xmax.gt.40.0) then
        TDINT=5.0
      ELSE
        TDINT=2.0
      ENDIF
      
      IF(ymax.gt.40.0) then
        GSINT=5.0
      ELSE
        GSINT=2.0
      ENDIF

      TD=0.
  20  CALL etplot(TD,ymax,1,0)
      TD=TD+TDINT
      CALL etplot(TD,0.,0,0)
      IF(TD.LE.xmax) goto 20

C Draw the moisture content lines
      CALL etplot(0.,0.,0,0)
      GS=0.
  30  CALL etplot(xmax,GS,1,0)
      GS=GS+GSINT
C      if(colok)call winscl('z',15)
      CALL etplot(0.,GS,0,0)
      IF(GS.LE.ymax) goto 30 
      if(colok)call winscl('-',0)

C Draw on the RH curves.
      DO 40 IRH=100,10,-10
        RH=FLOAT(IRH)
        IPFLAG=0
        DO 50 ITD=0,int(xmax),1
          TD=FLOAT(ITD)
          GS=1000.*(HUMRT1(TD,RH,PATMOS,IOPT))
          IF(ITD.EQ.0)THEN
            CALL etplot(TD,GS,0,0)
          ELSE
            IF(GS.GT.0..AND.GS.LT.ymax.AND.IPFLAG.EQ.0) THEN
              if(colok)call winscl('z',17)
              CALL etplot(TD,GS,-102,0)
            ENDIF

C Case where TD<60 and GS>30
            IF(GS.GT.ymax.AND.IPFLAG.EQ.0)THEN
  60          TD=TD-0.05
              GS=1000.*(HUMRT1(TD,RH,PATMOS,IOPT))
              IF(GS.GT.ymax) GOTO 60
              if(colok)call winscl('z',17)
              CALL etplot(TD,GS,-102,0)
              IPFLAG=1 
            ENDIF
            call forceflush()
          ENDIF
C Debug...
C          write(6,*) TD,xmax,xmax/2.
          call eclose(TD,real(int(xmax/2.)),0.001,close)
C          IF(TD.EQ.int(xmax/2.)) THEN
          IF(close) THEN
            write(puts,*) INT(RH),'%'
            if(GS.LE.ymax)then
              if(colok)call winscl('z',17)
              ipos=0
              isize=0
              CALL etlabel(puts,TD,GS,ipos,isize)
            endif
          ENDIF
          if(colok)call winscl('-',0)
          call forceflush()
  50    CONTINUE
  40  CONTINUE
                
C Draw on the lines of constant enthalpy.  
      DO 70 IENTH=0,int(enthmax),10
        ENTH=FLOAT(IENTH)
        IPFLAG=0
        DO 80 IGS=0,int(ymax),1
          GS=FLOAT(IGS)
          TD=TDB(ENTH,(GS/1000.))
          IF(IGS.EQ.0.AND.TD.LT.xmax)THEN
            CALL etplot(TD,GS,0,0)
          ELSEIF(IGS.EQ.0.AND.TD.GT.xmax)THEN
  110       GS=GS+0.05
            TD=TDB(ENTH,(GS/1000.))
            IF(TD.GT.xmax) GOTO 110
            if(colok)call winscl('z',24)
            CALL etplot(TD,GS,0,0)
          ELSE
            IF(TD.GT.0..AND.TD.LT.xmax.AND.IPFLAG.EQ.0) THEN
              if(colok)call winscl('z',24)
              CALL etplot(TD,GS,-102,0)
            ENDIF 

C Case where TD<60 and GS>30
            IF(TD.GT.xmax.AND.IPFLAG.EQ.0)THEN
  90          GS=GS+0.05
              TD=TDB(ENTH,(GS/1000.))
              IF(TD.GT.xmax) GOTO 90
              if(colok)call winscl('z',24)
              CALL etplot(TD,GS,1,0) 
            ENDIF
            IF(TD.LT.0..AND.IPFLAG.EQ.0) THEN
 100          GS=GS-0.05
              TD=TDB(ENTH,(GS/1000.))
              IF(TD.LT.0) GOTO 100
              if(colok)call winscl('z',24)
              CALL etplot(TD,GS,-102,0)
              IPFLAG=1
            ENDIF                                       
          ENDIF
          if(colok)call winscl('-',0)
          call forceflush()
  80     CONTINUE
  70   CONTINUE

C Draw on the enthalpy scales
      if(colok)call winscl('-',0)
      DO 200 IENTH=0,int(ENTHMAXY),10
        ENTH=FLOAT(IENTH)
        IF (ENTH.GT.ENTHMINY.AND.ENTH.LE.ENTHMAXY) THEN
          TD=Xmin
          GS=((ENTH-TD)/(1.82*TD+2500.))*1000.
          write(puts,*) IENTH
          ipos=0
          isize=0
          CALL etlabel(puts,Xmin-Xmax/XSC,GS,ipos,isize)
          call forceflush()
        ENDIF
  200 CONTINUE

      DO 210 IENTH=0,int(ENTHMAX),10
        ENTH=FLOAT(IENTH)
        IF (ENTH.GT.ENTHMINX.AND.ENTH.LE.ENTHMAX) THEN
          TD=TDB(ENTH,(YMAX)/1000.)
          write(puts,*) IENTH
          ipos=0
          isize=0
          CALL etlabel(puts,TD,YMAX+YMAX/YSC,ipos,isize)
          call forceflush()
        ENDIF
  210 CONTINUE

      return
      end
