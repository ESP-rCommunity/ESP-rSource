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

C This file contains the following routines.

C myocrecv - recovers information from database. 

C mycograph - plots the conditions during the simulation on a T/RH graph.
C Also plotted are the minimum growth curves for moulds selected by the
C user. 

C curvedraw - draws the mimimum growth conditions curves on the T RH graph

C Local variables in this module are:
C     IUOUT  - write unit (standard output).
C     IUIN   - read  unit (standard input).
C     ICOUT  - write unit for (non-fatal) warning messages.
C     MODEL  - determines the device type attached to the current job.

C     iucfg  - unit number for plant configuration file.
C     iupdb  - plant component database unit.

C ********************** MYCORECV ************************           
C Subroutine mycorecv recovers data from the mould database in the form
C of 1-mould names
C    2-equations 
C    3-look up table data
C    4-mould information

      subroutine mycorecv(irecov,imld)

#include "mould.h"
#include "esprdbfile.h"
      
      integer lnblnk  ! function definition

      common/outin/iuout,iuin
      common/moupk/ispp(mould),a(4,mould),mldnam(mould)

      common/mstats/imstat,ifloat(mould),ifloatp(mould),imdur1(mould)
     &,imdur0(mould),i1max(mould),i0max(mould),imup(mould),
     &imdn(mould)
 
      common/mstats2/maxrh(mould),minrh(mould),maxt(mould),mint(mould)
     &,ioutlk(mould),nog(mould),ipor(mould),imdt(mould),igod(mould),
     &ivgd(mould),nent(mould),ient(mould)

      common/mglook/GROT(MLOOK,MOULD),GRORH(MLOOK,MOULD)
      common/mglook2/GROW(MLOOK,MOULD)

C The mycotoxin database.
C << this should become part of model cfg file >>
      common/mdbase/mdbnam
      common/iopnmld/iopmld

      COMMON/FILEP/IFIL
      COMMON/DEFLT3/DFCFD,DECMPDBFL,DICONDBFL

      dimension item(mould+5)
      real maxrh,minrh,maxt,mint
      character outs*124, head*32, item*42, mdbnam*144, outstr*124
      character mldnam*48,grow*1,str*1,KEY*1
      character*72 DFCFD,DECMPDBFL,DICONDBFL
      integer nitem,ino  ! max items and current menu item

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created

      helpinsub='mycorecv'  ! set for subroutine

C Open the mould database.
      if(iopmld.eq.0) then
        lnmdb=lnblnk(mdbnam)
        if(mdbnam(1:4).eq.'UNKN'.or.mdbnam(1:2).eq.'  ')then
          write(mdbnam,'(a)') dmdbnam(1:lnblnk(dmdbnam))
        endif
        helptopic='mould_database_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easks(mdbnam,'Mould isopleth filename ?',' ',72,dmdbnam,
     &    'mould isopleths',IER,nbhelp)
      endif
      imfil=ifil+1
      call efopseq(imfil,mdbnam,1,IER)
      if(IER.lt.0) then
        call edisp(iuout,'Error - could not find the moulds database.')
        return
      else
        iopmld=1
      endif

C Initilaise picked items
      if(irecov.eq.1) then
        icount=0
C later recover names from the database......
        do 5 im=1,mould
          ispp(im)=0
   5    continue  

   6    call stripc(imfil,outstr,0,ITEMS,0,'mdb read',IER)
        if(IER.eq.2)  goto 7
        if(outstr(1:6).eq.'*MOULD') then 
          icount=icount+1   
          CALL EMKEY(icount,KEY,IER)
          call stripc(imfil,outstr,0,ITEMS,0,'mdb read',IER)
          write(item(icount),'(a1,1x,a)') KEY,outstr(1:40)
          mldnam(icount)=outstr(1:48)
          goto 6
        else
          goto 6 
        endif
  7     continue

C Display menu of mould names ....
  2     head='Mould types:'        
        item(icount+1)='-----------------------'
        item(icount+2)='1 Mould Information    '
        item(icount+3)='                       '
        item(icount+4)='? Help '
        item(icount+5)='- Exit'
        nitem=icount+5
        ino=-1

C Help text for this menu.
        helptopic='mould_species_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)

        call emenu(head,item,nitem,ino)
        if(ino.ge.1.and.ino.lt.nitem-4) then
          write(outs,'(a24,a)') item(ino),' chosen recovering data ...' 
          call usrmsg(outs,' ','-') 
          ispp(ino)=1
          goto 2
        elseif(ino.eq.nitem) then
          return
        elseif(ino.eq.nitem-1) then
          CALL PHELPD('Mould species selection',nbhelp,'-',0,0,IER)
          goto 2
        elseif(ino.eq.nitem-3) then
          do 66 imno=1,mould
          if(ispp(imno).gt.0) then

C Recursive call not allowed in NT and Linux.
C           call mycorecv(4,imno)
          endif
  66      continue
          goto 2
        endif
      elseif(irecov.eq.2) then
        icount=0
        call usrmsg('Getting data for plot ..',' ','-')
  11    call stripc(imfil,outstr,0,ITEMS,0,'mdb read',IER)
        if(IER.eq.2) then
          call edisp(iuout,'Error getting plot data !')
        else
          if(outstr(1:6).eq.'*MOULD') then
            icount=icount+1      
          endif
          if (icount.ne.imld) goto 11
 12       call stripc(imfil,outstr,99,ITEMS,0,'mdb read',IER)
          if(ITEMS.NE.4)  goto 12
          k=0
          do 14 ieqn=1,4
            call egetwr(outstr,k,val,0.0,1000.,'-','mdb eqn',IER)
            a(ieqn,imld)=val
 14       continue
        endif

C Get data for lookup table if needed
        if(imstat.gt.0) then
          call stripc(imfil,outstr,99,ITEMS,0,'lookup read',IER)
          call stripc(imfil,outstr,99,ITEMS,0,'lookup read',IER)
          k=0
          call egetwi(outstr,k,ival,1,100,'F','nent read',IER)
          ient(imld)=ival
          call egetwi(outstr,k,ival,1,100,'F','nelin read',IER)
          nent(imld)=ival
          if(nent(imld).eq.0) then
             call edisp(iuout,'Error reading lookup table!')
             return
          endif
          nline=ient(imld)/nent(imld)

C Recover lookup triplet data
          ilook=1
          do 51 iline=1,nline
            call stripc(imfil,outstr,99,ITEMS,0,'tripl read',IER)
            k=0
            do 55 itrip=1,nent(imld) 
              call egetwr(outstr,k,val,0.0,100.,'-','RHlook',IER)
              grorh(ilook,imld)=val
              call egetwr(outstr,k,val,0.0,100.,'-','T look',IER)
              grot(ilook,imld)=val
              call egetw(outstr,k,str,'W','grow look',IER)
              grow(ilook,imld)=str(1:1)
              ilook=ilook+1
              if(ilook.gt.MLOOK) goto 52
  55        continue
  51      continue

C Set lookup limits.
  52      minrh(imld)=grorh(1,imld)
          mint(imld)=grot(1,imld)
          if(ient(imld).le.MLOOK)then
            maxrh(imld)=grorh(ient(imld),imld)
            maxt(imld)=grot(ient(imld),imld)
          else
            maxrh(imld)=99.0
            maxt(imld) =35.0
          endif
        endif
      elseif(irecov.eq.4) then
C Get the database information for the mould species.
        icount=0
  21    call stripc(imfil,outstr,0,ITEMS,0,'mdb read',IER)
        if(IER.eq.2)  then
          call edisp(iuout,' Error getting plot data! ')
          return
        endif
        if(outstr(1:6).eq.'*MOULD') then
          icount=icount+1      
        endif
        if (icount.ne.imld) goto 21
 22     call stripc(imfil,outstr,99,ITEMS,0,'mdb read',IER)
        if(outstr(1:5).ne.'*TEXT') goto 22
 
        call edisp(iuout,' ')
 23     read(imfil,'(a)') outstr
        if(outstr(1:8).ne.'*ENDTEXT') then
          call edisp(iuout,outstr(1:LNBLNK(outstr)))
          goto 23
        endif
      endif

C Close the mould database.
      close(imfil)

      end

C ****************** mycograph *****************************

      subroutine mycograph

#include "mould.h"

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      common/moupk/ispp(mould),a(4,mould),mldnam(mould)
      common/ppoint/npoint,ixt(24),iyr(24),tpt(24),rpt(24)
      common/grphlim/xmin,xmax,ymin,ymax
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      common/mstats/imstat,ifloat(mould),ifloatp(mould),imdur1(mould)
     &,imdur0(mould),i1max(mould),i0max(mould),imup(mould),
     &imdn(mould)

      CHARACTER*29 ITEM(6)
      CHARACTER mldnam*48
      logical greyok
      integer NITMS,INO ! max items and current menu item

#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer ipos,isize      ! for use with etlabel
      integer iupdown,isym    ! passed to etplot
      integer iicol,iix,iiy
      integer iigl,iigr,iigt,iigb,iigw,iigwh
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 ipos,isize      ! for use with etlabel
      integer*8 iupdown,isym    ! passed to etplot
      integer*8 iicol,iix,iiy
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
#endif

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found (will be non-zero
                         ! because default message will be created

      helpinsub='mycograph'  ! set for subroutine

C Check in graphics mode.
      IF(MMOD.ne.8) return

C Cast values for linescale.
      iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh

C If libraries and monitor support greyscale then reset forground
C to 50% grey when drawing dots.
      greyok=.false.
      if(nifgrey.gt.4)then
        greyok=.true.
      endif

C Scale the axes of the chart (initial values)
      Xmin=5.0
      Xmax=35.0
      Ymin=60.0
      Ymax=100.0
      npoint=0

10    igwid=igw
      igheight=igwh
      call axiscale(igwid,igheight,Xmin,Xmax,Ymin,Ymax,xsc,ysc,sca,
     &   xadd,yadd)
      CALL startbuffer()

C Draw the axes of the chart.
      call dintervalf(Xmin,Xmax,DY,NDEC,0)
      CALL horaxisdd(Xmin,Xmax,iigl,iigr,iigb,xadd,XSC,0,
     &  DY,NDEC,'Db Temp C')
      call dintervalf(Ymin,Ymax,DY,NDEC,0)
      CALL vrtaxisdd(Ymin,Ymax,iigl,iigb,iigt,yadd,YSC,0,
     &  DY,NDEC,0,'RH %')

C Store the scaling parameters used in drawing the axes
      CALL linescale(iigl,xadd,XSC,iigb,yadd,YSC)

C Label graph
      ipos=0
      isize=2
      call etlabel('Mould Growth Conditions Plot',
     &  (xmax+xmin)/2,ymax+9.,ipos,isize)

C Draw the dry-bulb temperature lines
      iupdown=0
      isym=0
      CALL etplot(xmin,ymin,iupdown,isym)

C Set the intervals for the chart.
      IF(xmax.gt.40.) then
        TDINT=10.0
      ELSE
        TDINT=5.0
      ENDIF
      
      IF(ymax.gt.40.) then
        RHINT=5.0
      ELSE
        RHINT=2.0
      ENDIF

C If horizontal line is not the bottom or top draw as grey.
      TD=xmin
  20  if(greyok.and.(TD.gt.xmin.and.TD.lt.xmax))then
        iicol=nifgrey-3
        call winscl('i',iicol)
      endif
      iupdown=1
      isym=0
      CALL etplot(TD,ymax,iupdown,isym)
      if(greyok)then
        iicol=0
        call winscl('-',iicol)
      endif
      TD=TD+TDINT
      iupdown=0
      CALL etplot(TD,ymin,iupdown,isym)
      IF(TD.LE.xmax) goto 20
      if(greyok)then
        iicol=0
        call winscl('-',iicol)
      endif

C Draw the relative humidity lines
      iupdown=0
      CALL etplot(xmin,ymin,iupdown,isym)
      RH=ymin
  30  if(greyok.and.(RH.gt.ymin.and.RH.lt.ymax))then
        iicol=nifgrey-3
        call winscl('i',iicol)
      endif
      iupdown=1
      CALL etplot(xmax,RH,iupdown,isym)
      if(greyok)then
        iicol=0
        call winscl('-',iicol)
      endif
      RH=RH+RHINT
      iupdown=0
      CALL etplot(xmin,RH,iupdown,isym)
      IF(RH.LE.ymax) goto 30 
      if(greyok)then
        iicol=0
        call winscl('-',iicol)
      endif

C Draw on mould growth curves curves.
      do 90 imld=1,mould
        if(ispp(imld).gt.0) then
          call curvedraw(imld)
        endif
   90 continue
                
C Draw on the selected points.
      do 99 ipt=1,npoint
        iix=ixt(ipt); iiy=iyr(ipt)
C        call ecirc(ixt(ipt),iyr(ipt),3,1) 
        call ecirc(iix,iiy,3,1) 
  99  continue
      call forceflush()

C Setup menu text.
      ITEM(1) ='1 Plot points on chart '
      ITEM(2) ='2 Change Scales        '
      ITEM(3) ='3 Re-draw Chart        '
      ITEM(4) ='  ____________________ '
      ITEM(5) ='? Help                 '
      ITEM(6) ='- Exit                 '

C Display menu.
 2    NITMS=6
      INO=-2
 5    continue

C Help text for this menu.
      helptopic='mould_prediction_plots'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU('Results',ITEM,NITMS,INO)
      IF(INO.EQ.1)THEN
        helptopic='mould_results_file_read'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easkab('Read from results file or user input'
     &,' ','Result file','User',IW,nbhelp)
       if(IW.eq.1) then
         call vread
         if(imstat.gt.0) then
           call mycostat(3,0.,0.)
         endif
       elseif(IW.eq.2) then
         if(npoint+1.gt.24) then
           call edisp(iuout,'Max no. of points plotted ') 
           goto 2
         endif
         helptopic='temp_rh_at_point'
         call gethelptext(helpinsub,helptopic,nbhelp)
         call easkr(tval,'Temp C','for point ? ',xmin,'F',xmax,'-',
     &     20.0,'max temp',IER,nbhelp)
         call easkr(rval,'RH %','for point ? ',ymin,'F',ymax,'-',
     &     80.0,'max temp',IER,nbhelp)
         call u2pixel(tval,rval,iix,iiy)
         npoint=npoint+1
         tpt(npoint)=tval
         rpt(npoint)=rval
         ixt(npoint)=iix
         iyr(npoint)=iiy
         call ecirc(iix,iiy,3,1)
       endif
      ELSEIF(INO.EQ.2)THEN
        val = Xmax
        helptopic='temp_max_for_graph'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call easkr(val,'Max temp C','for chart ',0.0,'F',60.,'-',
     &  35.0,'max temp',IER,nbhelp)
        Xmax=val
        val = Xmin
        call easkr(val,'Min temp C','for chart ',0.0,'F',60.,'-',
     &  5.0,'max temp',IER,nbhelp)
        Xmin=val
        helptopic='rh_max_for_graph'
        call gethelptext(helpinsub,helptopic,nbhelp)
        val = Ymax
        call easkr(val,'Max RH %','for chart ',0.0,'F',
     &  100.,'-',100.0,'max temp',IER,0)
        Ymax=val
        val = Ymin
        call easkr(val,'Min RH %','for chart ',0.0,'F',
     &  70.,'-',30.0,'max temp',IER,0)
        Ymin=val
        GOTO 2
      ELSEIF(INO.EQ.3)THEN
        CALL startbuffer()
        GOTO 10       
      ELSEIF(INO.EQ.5)THEN
        helptopic='mould_prediction_plots'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('Mould growth chart',nbhelp,'-',0,0,IER)
      ELSEIF(INO.EQ.6)THEN
        CALL startbuffer()

C Reset plot records.
        RETURN        
      ELSE
        GOTO 5
      ENDIF
      GOTO 2
      
      END

C ********************
C subroutine curvedraw 
      subroutine curvedraw(imld)
#include "mould.h"

      common/outin/iuout,iuin
      common/grphlim/xmin,xmax,ymin,ymax
      common/moupk/ispp(mould),a(4,mould),mldnam(mould)
      common/mdbase/mdbnam
      common/FILEP/IFIL

      character outs*124, mdbnam*144 ,mldnam*48
#ifdef OSI
      integer ipos,isize      ! for use with etlabel
      integer iupdown,isym    ! passed to etplot
#else
      integer*8 ipos,isize      ! for use with etlabel
      integer*8 iupdown,isym    ! passed to etplot
#endif

      iget=imld
      call mycorecv(2,iget)

      write(outs,*) 'Eqn is: RH=',a(4,imld),'T^3+',a(3,imld),'T^2+',
     &a(2,imld),'T+',a(1,imld),' '
      call usrmsg(outs,' ','-')

C Plot the curve on the chart
      istfl=0
      ilab=0
      do 10 ITDB=int(xmin),int(xmax)
         TDB=FLOAT(ITDB)
         RH=(a(4,imld)*TDB**3)+(a(3,imld)*TDB**2)+(a(2,imld)*TDB)
     &+a(1,imld)
         if(ITDB.eq.int(xmin).or.istfl.eq.0) then
           if(RH.le.100.) then
             iupdown=0
             isym=0
             call etplot(TDB,RH,iupdown,isym)
             istfl=1
           endif
         else
            if(RH.le.ymax.and.RH.ge.ymin.and.istfl.gt.0) then
               iupdown=-102
               isym=0
               call etplot(TDB,RH,iupdown,isym)
             if(ITDB.gt.xmax/2.and.ilab.eq.0) then  
               ipos=0
               isize=0
               call etlabel(mldnam(iget)(1:20),xmax/2,RH,ipos,isize)
               ilab=1
             endif
            endif
         endif   
   10 continue
      call forceflush()  
      return
      end
