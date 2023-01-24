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

C ESRU: e2r viewpoint facilities.
C  VCONE: View cone for plan/elevation.
C  VEWFORM: View control and definition.

C ******* View cone for plan/elevation.
      SUBROUTINE VCONE(direc)
#include "building.h"
#include "e2r_common.h"
#include "prj3dv.h"
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      character direc*1
#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iupdown,isym    ! passed to etplot
      integer iigl,iigr,iigt,iigb,iigw,iigwh
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iupdown,isym    ! passed to etplot
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
#endif

C Cast values for linscale.
      iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh

C Set the length of the viewcone according to the image.
      PI = 4.0 * ATAN(1.0)
      RAD = PI/180.
      XD=(XMX-XMN)**2 + (YMX-YMN)**2
      DD=SQRT(XD)
      VCL=DD*0.20

C Plot a heavy line along the view cone then
C two light lines at edge of view cone.
      if(direc.eq.'P')then
        igwid=igw
        igheight=igwh
        call axiscale(igwid,igheight,XMN,XMX,YMN,YMX,xsc,ysc,sca,
     &     Xadd,Yadd)
        call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
        iupdown=0
        isym=14
        call etplot(rvpx,rvpy,iupdown,isym)
        isym=0
        call etplot(rvpx,rvpy,iupdown,isym)
        wxe=VCL*vdx; wye=VCL*vdy
        iupdown=-302; isym=0
        call etplot(rvpx+wxe,rvpy+wye,iupdown,isym)
        astrt=azimuth+(angh/2.)
        RYAZI = astrt*RAD
        xastrt = rvpx+(VCL*SIN(RYAZI))
        yastrt = rvpy+(VCL*COS(RYAZI))
        iupdown=0; isym=0
        call etplot(rvpx,rvpy,iupdown,isym)
        iupdown=1
        call etplot(xastrt,yastrt,iupdown,isym)
        aend=astrt-angh
        RYAZI = aend*RAD
        xaend = rvpx+(VCL*SIN(RYAZI))
        yaend = rvpy+(VCL*COS(RYAZI))
        iupdown=0; isym=0
        call etplot(rvpx,rvpy,iupdown,isym)
        iupdown=1
        call etplot(xaend,yaend,iupdown,isym)
      elseif(direc.eq.'S')then

C If south elevation, omitt the view-cone.
        igwid=igw
        igheight=igwh
        call axiscale(igwid,igheight,XMN,XMX,ZMN,ZMX,xsc,ysc,sca,
     &     Xadd,Yadd)
        call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
        iupdown=0; isym=14
        call etplot(rvpx,rvpz,iupdown,isym)
        isym=0
        call etplot(rvpx,rvpz,iupdown,isym)
        wxe=VCL*vdx
        wze=VCL*vdz
        iupdown=-302; isym=0
        call etplot(rvpx+wxe,rvpz+wze,iupdown,isym)
      elseif(direc.eq.'E')then
        igwid=igw
        igheight=igwh
        call axiscale(igwid,igheight,YMN,YMX,ZMN,ZMX,xsc,ysc,sca,
     &     Xadd,Yadd)
        call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
        iupdown=0; isym=14
        call etplot(rvpy,rvpz,iupdown,isym)
        isym=0
        call etplot(rvpy,rvpz,iupdown,isym)
        wye=VCL*vdy
        wze=VCL*vdz
        iupdown=-302; isym=0
        call etplot(rvpy+wye,rvpz+wze,iupdown,isym)
      endif
      call forceflush()
      return
      end


C ******* Default views formation.
      SUBROUTINE VEWFORM(IER)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"
#include "e2r_common.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      COMMON/FILEP/IFIL
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY5OB/OXMN,OYMN,OZMN,OXMX,OYMX,OZMX
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      COMMON/ZONPIK/NZ,NZNO(MCOM)
      integer IZLST(MCON),ISLST(MCON)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      LOGICAL XST,OK
      logical addview   ! mark we are working a new view
      logical do_redraw
      logical short     ! for shorter format
      logical okfore,okaft ! for near zero cuts

      DIMENSION ITEM(21),IVALS(MCOM),VALT(20),IVALV(20)

      character item*30,hold*28,VALT*28,OUTSTR*148,WORD*20
      character outs*124
      character doit*146,ltmp*72,lview*124
      character L144*144,lltmp*144,dtmp96*96
      character trvewsh*10,mrvewsh*30
      character rifwave*72
      integer NITEMS,INO  ! max items and current menu item
      integer iglib
      integer ISTRW

#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iupdown,isym    ! passed to etplot
      integer iigl,iigr,iigt,iigb,iigw,iigwh
      integer iix,iiy,iixx,iiyy,iik
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iupdown,isym    ! passed to etplot
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
      integer*8 iix,iiy,iixx,iiyy,iik
#endif

      helpinsub='e2rvp'  ! set for subroutine

C Cast values for linescale.
      iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh

C Addview = true then save editing as a new view, if false
C then it is an update to an existing view.
      ITA2 = IFIL+7
      vewdone=.false.
      do_redraw =.true.
      addview=.false.
      trvewsh='new'
      ier=0
      ITPPSW=3

C First thing is to ask what zones to include
C in the plan view.
C If outside view then initially select all zones...
      helptopic='view_form_creation'
      call gethelptext(helpinsub,helptopic,nbhelp)
      if(intext.eq.0)then
        nzg=NCOMP
        izgfoc=0
        DO 44 I=1,nzg
          nznog(I)=I
  44    CONTINUE
        call edisp(iuout,'Including all zones in outside view.')
      else
        call edisp(iuout,'For an inside view... ')
        INPIC=NCOMP
        CALL EPICKS(INPIC,IVALS,' ','Which zones to include:',
     &    12,NCOMP,zname,' zone list',IER,nbhelp)
        nzg=INPIC
        NZ=INPIC
        izgfoc=0
        if(nzg.eq.1)izgfoc=IVALS(1)
        DO 45 I=1,nzg
          nznog(I)=IVALS(I)
          NZNO(I)=IVALS(I)
  45    CONTINUE
      endif
      MODBND=.TRUE.; MODLEN=.TRUE.; MODIFYVIEW=.TRUE.
      CALL BNDOBJ(1,IER)

C Outside view. Scale bounding box to be a bit less than
C extremes of the ground disk. Save off zone bounds before
C adding in the ground disk.
      OXMN=XMN; OYMN=YMN; OZMN=ZMN
      OXMX=XMX; OYMX=YMX; OZMX=ZMX
      if(intext.eq.0)then
        XD=(XMX-XMN)**2 + (YMX-YMN)**2
        DD=SQRT(XD)*1.5
        cx=XMN+((XMX-XMN)/2.)
        cy=YMN+((YMX-YMN)/2.)
        tXMN=cx-DD; tXMX=cx+DD; 
        tYMN=cy-DD; tYMX=cy+DD
        tZMN=tZMN; tZMX=DD
      else   ! For plan view just do the extents.
        tXMN=XMN; tXMX=XMX
        tYMN=YMN; tYMX=YMX
        tZMN=ZMN; tZMX=ZMX
      endif
      MODBND=.FALSE.; MODLEN=.FALSE.

C Pull up a list of views and ask user for which one. Include an additional
C option for creation of a new view or importation of a rvu generated file.
C << take account of length of rvewsh to allow more of vewcmds to display >>
  142 do 333 iv=1,indxvew
        write(valt(iv),'(3a)') rvewsh(iv)(1:lnblnk(rvewsh(iv))),':',
     &                         vewcmds(iv)(1:16)
  333 continue
      valt(indxvew+1) = 'Create a new view.'
      valt(indxvew+2) = 'Import a rvu view file'
      valt(indxvew+3) = 'Create views for MRT sensors'
      IX=1
  334 CALL EPICKS(IX,IVALV,' Select a view: ',' ',
     &  28,indxvew+3,VALT,'View options',IER,nbhelp)
      if(IX.eq.0)then
        CALL EASKAB(' One of the options must be selected:',' ',
     &    'select an option','exit view point facility',IW,nbhelp)
        if(iw.eq.1)goto 334
        if(iw.eq.2)return
      endif
      ipckvew=IVALV(1)  ! remember the current view
      if(ipckvew.eq.indxvew+2)then
        addview = .true.

C Ask for name vp file.
C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
        dtmp96='default.vew'
        L144='../rad/'
        iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
        if(iglib.eq.1.or.iglib.eq.3)then
          ISTRW=96
        elseif(iglib.eq.2)then
          ISTRW=144
        else
          ISTRW=96
        endif
        CALL EASKF(L144,' rvu view file?',' ',ISTRW,dtmp96,
     &    'view file name',IER,nbhelp)

        IF(L144(1:2).EQ.'  '.or.L144(1:4).eq.'UNKN')GOTO 142
        call st2file(L144,lltmp)
        call usrmsg('File selected is',l144,'-')
        XST=.false.
        INQUIRE (FILE=lltmp,EXIST=XST)
        if (XST)then

C Check for file (add working dir path)
C          write (lview,'(2a)') runpath(1:lnblnk(runpath)),
C     &     lltmp(1:lnblnk(lltmp))

C debug
C          write(6,*) 'l144 ',l144(1:lnblnk(l144))
C          write(6,*) 'lltmp ',lltmp(1:lnblnk(lltmp))
          CALL ERPFREE(ITA2,ISTAT)

C Read a radiance view point file and make sure that the
C view point is within the bounds of the image.
          call RRVEW(ITA2,lltmp,IER)

C At this point common block radv has been filled but
C not yet written to vewcmds array.
          call UV2AZ(vdx,vdy,vdz,azimuth,elevtn)
          write(outs,'(a,3f7.3,a,2f7.2)') 'Converting vector ',
     &      vdx,vdy,vdz,' to ',azimuth,elevtn
          call edisp(iuout,outs)
          tXMN=AMIN1(XMN,rvpx); tYMN=AMIN1(YMN,rvpy)
          tZMN=AMIN1(ZMN,rvpz)
          tXMX=AMAX1(XMX,rvpx); tYMX=AMAX1(YMX,rvpy)
          tZMX=AMAX1(ZMX,rvpz)
          EYEM(1)=rvpx; EYEM(2)=rvpy; EYEM(3)=rvpz
          write(outs,'(a,3f8.3)') 'Setting eyepoint to ',rvpx,rvpy,rvpz
          call edisp(iuout,outs)

          if(intext.eq.0)then

C External view.
            ITPPSW=3
            ANG=AMAX1(angh,angv)
            write(outs,'(a,3f8.3)') 'Recovered view angles ',
     &         angh,angv,ANG
            call edisp(iuout,outs)
            HANG=ANG/2.0; ITBND=0
            call edisp(iuout,' ')
            call edisp(iuout,'Switching to static viewing angles.')
            MODIFYVIEW=.TRUE.
            MODLEN=.TRUE.
          else

C Internal view.
            ITPPSW=0
            call edisp(iuout,' ')
            call edisp(iuout,'Use plan or elevation views.')
          endif
        else
          call edisp(iuout,'View file not found, returning.')
          return
        endif

        CALL EASKS(trvewsh,'Name for view?',' ',10,'vew1',
     &    'short view tag',IER,nbhelp)
        call edisp(iuout,
     &    'Please check the view point...if ok save the view.')

      elseif(ipckvew.eq.indxvew+1)then

C User requested a new view. Take the current wireframe information
C as a start.
        addview = .true.

C External view.
        if(intext.eq.0)then

C Take eye point from 3D view commons.
          rvpx=EYEM(1); rvpy=EYEM(2); rvpz=EYEM(3)

C Calculate azimuth and elevation from eye point -> view point line, and
C convert to unit vector.
          call ln2az(rvpx,rvpy,rvpz,VIEWM(1),VIEWM(2),VIEWM(3),
     &      azimuth,elevtn)
          call AZ2UV(azimuth,elevtn,vdx,vdy,vdz)

C Set radiance view angles.
          angh=ANG; angv=ANG

C Default to fisheye view for glare analysis, perspective for everything
C else.
          ITPPSW=3
          if (SCENEPURP(ISCENE)(1:5).eq.'Glare') then
            vewtyp='h' ! fisheye
            anght=180.0
            angvt=180.0
          else
            vewtyp='v' ! perspective
            anght=angh
            angvt=angv
          endif 

C Internal view.
        else
          ITPPSW=0
          rvpx=tXMN+0.1  ! slightly inside room
          rvpy=tYMN+0.1
          rvpz=VIEWM(3) ! at room centre
        endif

        CALL EASKS(trvewsh,'Name for view?',' ',10,'vew1',
     &    'short view tag',IER,nbhelp)
        call edisp(iuout,
     &    'Please adjust the new view point and save it.')

      elseif(ipckvew.eq.indxvew+3)then

C This option only recommended for internal views.
        if (intext.eq.0) then
          helptopic='ext_MRTview'
          call gethelptext(helpinsub,helptopic,nbhelp)
          write(outs,'(a)')
     &      'This option not recommended for external views!'
          call EASKOK(outs,'Continue?',ok,nbhelp)
          if (.not.ok) goto 10
        endif

C Create a viewpoint for each selected MRT sensor, pointing in the direction of the sensor rotation.
C First, select MRT sensors.
        NSEL=999
        CALL PIKMRTS(IZLST,ISLST,NSEL,-1,IER)
        if (ier.ne.0) then
          call edisp(iuout,'Error selecting MRT sensors.')
          goto 334
        endif

C Now, create a viewpoint for each one.
C For Glare analysis, use fisheye views.
        do IMRT=1,NSEL

C Use coordinates at the centre of the top face of the MRT sensor.
          PI = 4.0 * ATAN(1.0)
          R=PI/180.
          SA=SIN(CANG(IMRT)*R)
          CA=COS(CANG(IMRT)*R)
          DX=(DXC(IMRT)/2.)*CA-(DYC(IMRT)/2.)*SA
          DY=(DXC(IMRT)/2.)*SA+(DYC(IMRT)/2.)*CA

C Set coordinates.
          rvpx=XOC(IMRT)+DX
          rvpy=YOC(IMRT)+DY
          rvpz=ZOC(IMRT)+DZC(IMRT)

          write(outs,*)'MRV rvpx,rvpy,rvpz',rvpx,rvpy,rvpz
          call edisp(iuout,outs)

C Set view direction.
          CALL AZ2UV(CANG(IMRT),0.0,vdx,vdy,vdz)
C For some reason we need to reverse the X axis.
C ESP-r's rotation is anticlockwise - maybe radiance is clockwise?
C Would explain this.
          vdx=-vdx

C Set -vu (?).
          vux=0.0; vuy=0.0; vuz=1.0

C Set view type and angles.
          angh=90.0; angv=60.0
          if (SCENEPURP(ISCENE)(1:5).eq.'Glare') then
            vewtyp='h' ! fisheye
            anght=180.0
            angvt=180.0
          else
            vewtyp='v' ! perspective
            anght=angh
            angvt=angv
          endif          

          indxvew = indxvew +1
          write(trvewsh,'(a,i2.2)')'v',IMRT
          rvewsh(indxvew) = trvewsh  ! remember new name
          if(short)then
            if(okfore.and.okaft)then
              write(doit,'(3a,3F7.1,a,3F7.3,a,3F5.2,2(a,F5.1),a)') 
     &        '-vt',vewtyp,' -vp',rvpx,rvpy,rvpz,' -vd',vdx,vdy,vdz,
     &        ' -vu ',vux,vuy,vuz,' -vh ',anght,' -vv ',angvt,
     &        ' -vs 0 -vl 0 -vo 0 -va 0'
            else
              write(doit,'(3a,3F7.1,a,3F7.3,a,3F5.2,4(a,F5.1))') 
     &        '-vt',vewtyp,' -vp',rvpx,rvpy,rvpz,' -vd',vdx,vdy,vdz,
     &        ' -vu ',vux,vuy,vuz,' -vh ',anght,' -vv ',angvt,
     &        ' -vs 0 -vl 0 -vo ',cutfor,' -va ',cutaft
            endif
          else
            if(okfore.and.okaft)then
              write(doit,'(3a,3F8.2,a,3F7.3,a,3F5.2,2(a,F5.1),a)') 
     &        '-vt',vewtyp,' -vp',rvpx,rvpy,rvpz,' -vd',vdx,vdy,vdz,
     &        ' -vu ',vux,vuy,vuz,' -vh ',anght,' -vv ',angvt,
     &        ' -vs 0 -vl 0 -vo 0 -va 0'
            else
              write(doit,'(3a,3F8.2,a,3F7.3,a,3F5.2,4(a,F5.1))') 
     &        '-vt',vewtyp,' -vp',rvpx,rvpy,rvpz,' -vd',vdx,vdy,vdz,
     &        ' -vu ',vux,vuy,vuz,' -vh ',anght,' -vv ',angvt,
     &        ' -vs 0 -vl 0 -vo ',cutfor,' -va ',cutaft
            endif
          endif
          WRITE(vewcmds(indxvew),'(a)') doit  ! new cmd

        enddo    
        goto 142 

      else

C An existing view. Perhaps a referenced file.
        write(OUTSTR,'(a)')vewcmds(ipckvew)(1:lnblnk(vewcmds(ipckvew)))
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','view command',IFLAG)
        if(word(1:3).eq.'-vf')then
          CALL EGETRM(OUTSTR,K,ltmp,'W','view file',IER)
          call edisp(iuout,' Recovered view file name.')
          if(ltmp.ne.' ')then

C Check for file (add working dir path)
            write (lview,'(a,a)') runpath(1:lnblnk(runpath)),
     &       ltmp(1:lnblnk(ltmp))
            CALL ERPFREE(ITA2,ISTAT)
            call FINDFIL(lview,XST)
            IF(XST)THEN
              trvewsh = rvewsh(ipckvew)
              addview = .false.

C Read a radiance view point file and make sure that the
C view point is within the bounds of the image.
              call RRVEW(ITA2,lview,IER)
              call UV2AZ(vdx,vdy,vdz,azimuth,elevtn)
              tXMN=AMIN1(XMN,rvpx); tYMN=AMIN1(YMN,rvpy)
              tZMN=AMIN1(ZMN,rvpz)
              tXMX=AMAX1(XMX,rvpx); tYMX=AMAX1(YMX,rvpy)
              tZMX=AMAX1(ZMX,rvpz)
              EYEM(1)=rvpx; EYEM(2)=rvpy; EYEM(3)=rvpz
              if(intext.eq.0)then

C External view.
                ITPPSW=3; ANG=angh; ITBND=0
                call edisp(iuout,' ')
                call edisp(iuout,'Switching to static viewing angles.')
                MODIFYVIEW=.TRUE.; MODLEN=.TRUE.
              else

C Internal view.
                ITPPSW=0
                call edisp(iuout,' ')
                call edisp(iuout,'Use plan or elevation views.')
              endif
            else
              call edisp(iuout,'View file not found, returning.')
              return
            endif
          else
            call edisp(iuout,'Could not parse the command:')
            call edisp(iuout,vewcmds(ipckvew))
            return
          endif

        elseif(word(1:3).eq.'-vt')then

C An in-line view command within the rif file.
          vewtyp(1:1)=word(4:4)
  42      CALL EGETW(OUTSTR,K,WORD,'-','command',IFLAG)

C Debug.
C          write(6,*) outstr
          IF(IFLAG.ne.0)goto 43
          if(word(1:3).eq.'-vp')then
            CALL EGETWR(OUTSTR,K,rvpx,0.,0.,'-','X view pt',IER)
            CALL EGETWR(OUTSTR,K,rvpy,0.,0.,'-','Y view pt',IER)
            CALL EGETWR(OUTSTR,K,rvpz,0.,0.,'-','Z view pt',IER)
            IF(IER.ne.0)goto 43
          elseif(word(1:3).eq.'-vd')then
            CALL EGETWR(OUTSTR,K,vdx,0.,0.,'-','X view vec',IER)
            CALL EGETWR(OUTSTR,K,vdy,0.,0.,'-','Y view vec',IER)
            CALL EGETWR(OUTSTR,K,vdz,0.,0.,'-','Z view vec',IER)
            IF(IER.ne.0)goto 43
          elseif(word(1:3).eq.'-vu')then
            CALL EGETWR(OUTSTR,K,vux,0.,0.,'-','vu x',IER)
            CALL EGETWR(OUTSTR,K,vuy,0.,0.,'-','vu y',IER)
            CALL EGETWR(OUTSTR,K,vuz,0.,0.,'-','vu z',IER)
            IF(IER.ne.0)goto 43
          elseif(word(1:3).eq.'-vh')then
            CALL EGETWR(OUTSTR,K,angh,0.,0.,'-','horiz ang',IER)
            IF(IER.ne.0)goto 43
          elseif(word(1:3).eq.'-vv')then
            CALL EGETWR(OUTSTR,K,angv,0.,0.,'-','vert ang',IER)
            IF(IER.ne.0)goto 43
          elseif(word(1:3).eq.'-vs')then
            CALL EGETWR(OUTSTR,K,vs,0.,0.,'-','skip vl',IER)
            IF(IER.ne.0)goto 43
          elseif(word(1:3).eq.'-vl')then
            CALL EGETWR(OUTSTR,K,vl,0.,0.,'-','skip vl',IER)
            IF(IER.ne.0)goto 43
          else
            goto 43
          endif
          goto 42

C Got all of the viewing data.
  43      continue
          call UV2AZ(vdx,vdy,vdz,azimuth,elevtn)  ! re-establish
          tXMN=AMIN1(XMN,rvpx); tYMN=AMIN1(YMN,rvpy)
          tZMN=AMIN1(ZMN,rvpz)
          tXMX=AMAX1(XMX,rvpx); tYMX=AMAX1(YMX,rvpy)
          tZMX=AMAX1(ZMX,rvpz)
          EYEM(1)=rvpx; EYEM(2)=rvpy; EYEM(3)=rvpz
          if(intext.eq.0)then

C External view.
            ITPPSW=3; ANG=angh; ITBND=0
            call edisp(iuout,' ')
            call edisp(iuout,'Switching to static viewing angles.')
            MODIFYVIEW=.TRUE.; MODLEN=.TRUE.
          else
            ITPPSW=0
            call edisp(iuout,' ')
            call edisp(iuout,'Use plan or elevation views.')
          endif
        endif
      endif

10    INO = -4

C Reconstitute the viewing eye angle.
      call ang3vtx(OXMN,OYMN,OZMN,EYEM(1),EYEM(2),EYEM(3),OXMX,
     &             OYMX,OZMX,eang)

      ITEM(1) =                   'a view list                   '
      ITEM(2) =                   'b zones to include            '
      if(ITPPSW.eq.0)then
        ITEM(3) =                 'c display >> plan view        '
      elseif(ITPPSW.eq.1)then 
        ITEM(3) =                 'c display >> south elevation  '
      elseif(ITPPSW.eq.2)then
        ITEM(3) =                 'c display >> east elevation   '
      elseif(ITPPSW.eq.3)then
        ITEM(3) =                 'c display >> perspective      '
      endif
      ITEM(4) =                   '  __________________________  '
      write(ITEM(5),'(A,3F6.1)')  'e eye @ :',rvpx,rvpy,rvpz
      write(ITEM(6),'(A,2F6.1)')  'f direc azi elv:',azimuth,elevtn
      write(ITEM(7),'(A,3F6.2)')  '  (vector):',vdx,vdy,vdz
      write(ITEM(8),'(A,3F6.2)')  '  (-vu)   :',vux,vuy,vuz
      if(ITPPSW.eq.3)then
        write(ITEM(9),'(A,F5.1,a,F5.1,a)')
     &                            'h view angle: ',angh,' (',eang,')'
      else
        write(ITEM(9),'(A,F5.1)') 'h hor view angle: ',angh
      endif
      write(ITEM(10),'(A,F5.1)')  'i vert view angle: ',angv
      write(ITEM(11),'(A,2F6.1)') 'j clip front aft:',cutfor,cutaft
      if (vewtyp(1:1).eq.'h') then
        write(ITEM(12),'(A)')     'k view type >> fisheye'        
      elseif (vewtyp(1:1).eq.'v') then      
        write(ITEM(12),'(A)')     'k view type >> perspective'
      else ! default to perspective if not set.
        vewtyp='v'
        write(ITEM(12),'(A)')     'k view type >> perspective'
      endif
      ITEM(13) =                  '  ___site___________________  '
      if(ITPPSW.eq.0)then

C Plan view.
        write(ITEM(14),'(A,2F6.1)') 'l east min~max : ',tXMN,tXMX
        write(ITEM(15),'(A,2F6.1)') 'm north min~max: ',tYMN,tYMX
      elseif(ITPPSW.eq.1)then

C South elevation view.
        write(ITEM(14),'(A,2F6.1)') 'l south min~max: ',tXMN,tXMX
        write(ITEM(15),'(A,2F6.1)') 'm up    min~max: ',tZMN,tZMX
      elseif(ITPPSW.eq.2)then

C East elevation view.
        write(ITEM(14),'(A,2F6.1)') 'l east min~max : ',tYMN,tYMX
        write(ITEM(15),'(A,2F6.1)') 'm up    min~max: ',tZMN,tZMX
      elseif(ITPPSW.eq.3)then

C Perspective view.
        write(ITEM(14),'(A,2F6.1)') 'l east min~max : ',tXMN,tXMX
        write(ITEM(15),'(A,2F6.1)') 'm north min~max: ',tYMN,tYMX
      endif
      ITEM(16) =                  '  __________________________  '
      ITEM(17) =                  '> save view info              '
      if (MMOD.eq.8) then
        ITEM(18) =                '! update from model display   '
      else
        ITEM(18) =                '  not available               '
      endif
      ITEM(19) =                  '  __________________________  '
      ITEM(20) =                  '? help                        '
      ITEM(21) =                  '- exit                        '
      NITEMS = 21

C Check if graphics are active.
      if (MMOD.ne.8) then
        do_redraw=.false.
      endif

      if(do_redraw)then

C Draw the problem in plan/elevation with viewcone.
        if(ITPPSW.eq.0)then
          call PLELEV('P')
          call VCONE('P')
        elseif(ITPPSW.eq.1)then
          call PLELEV('S')
          call VCONE('S')
        elseif(ITPPSW.eq.2)then
          call PLELEV('E')
          call VCONE('E')
        elseif(ITPPSW.eq.3)then
          MODIFYVIEW=.TRUE.
          CALL redraw(IER)
          MODLEN=.FALSE.
          MODIFYVIEW=.FALSE.
        endif
        do_redraw =.false.
      endif

C Help text for main menu.
      helptopic='view_definition_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Adapt title to reflect the current view name.
      if(addview)then
        write(mrvewsh,'(2a)') 'Viewpoint: ',trvewsh
      else
        write(mrvewsh,'(2a)') 'Viewpoint: ',rvewsh(ipckvew)
      endif
      CALL EMENU(mrvewsh,ITEM,NITEMS,INO)
      if(INO.EQ.1)then

C Present list of views.
        vewdone=.false.
        do_redraw =.true.
        MODLEN=.true.
        MODIFYVIEW=.TRUE.
        goto 142

      elseif(INO.EQ.2)then

        INPIC=NCOMP
        CALL EPICKS(INPIC,IVALS,
     &    'More than one zone may be seen in the view',
     &    'Which to include: ',12,NCOMP,zname,' zone list',IER,nbhelp)
        call edisp(iuout,' Checking bounds....')
        nzg=INPIC
        izgfoc=0
        DO 46 I=1,nzg
          nznog(I)=IVALS(I)
  46    CONTINUE
        MODLEN=.TRUE.
        MODBND=.TRUE.
        CALL BNDOBJ(1,IER)
        do_redraw =.true.
        if(intext.eq.0)then
          continue
        else   ! For plan view revise the extents.
          tXMN=XMN; tXMX=XMX
          tYMN=YMN; tYMX=YMX
          tZMN=ZMN; tZMX=ZMX
        endif

      elseif(INO.EQ.3)then
        CALL EASKABCD(' View choices: ',' ','plan',
     &    'south elevation','east elevation','perspective?',IW,nbhelp)
        if(IW-1.ne.ITPPSW)do_redraw =.true.
        ITPPSW=IW-1

      elseif(INO.EQ.5)THEN

C Eye Point...(assume begins at centre of zone).  Logic for
C Picking on plan is faulty - comment out and just support
C a direct edit.
        if(ITPPSW.eq.0)then
          IEE=1
C          CALL EASKABC(' Specify via: ',' ','editing',
C     &     'pick on plan','ignore ?',IEE,nbhelp)
C          if(IEE.eq.3)then
C            goto 10

C Select point from plan or current perspective.
C          elseif(IEE.eq.2)then
C            call usrmsg(' ',' Select point via cursor...','-')
C            igwid=igw
C            igheight=igwh
C            call axiscale(igwid,igheight,tXMN,tXMX,tYMN,tYMX,xsc,ysc,
C     &        sca,Xadd,Yadd)
C            call linescale(iigl,Xadd,sca,iigb,Yadd,sca)

C            CALL trackview(iik,iix,iiy)
C            ix=int(iix); iy=int(iiy)

C Debug.
C            write(6,*) 'axiscale ',igwid,igheight,tXMN,tXMX,tYMN,tYMX,
C     &        xsc,ysc,sca,Xadd,Yadd
C            write(6,*) 'trackview iix,iiy,ix,iy,igb,igl,ITPPSW',
C     &        iix,iiy,ix,iy,igb,igl,ITPPSW
C            write(6,*) 'diffs ',(iix-igl),(igb-iiy)

C Transform back from mouse position to actual coordinate.
C Note: the coordinate is an offset from the lower left corner
C of the zone plan display and for zones which do not start at
C the site origin we need to add in offsets.
C            if(ITPPSW.eq.0)then
C              ex=((FLOAT(iix-igl)/sca)-Xadd)+tXMN
C              ey=((FLOAT(igb-iiy)/sca)-Yadd)+tYMN
C            elseif(ITPPSW.eq.1)then
C              ex=((FLOAT(iix-igl)/sca)-Xadd)+tXMN
C              ey=((FLOAT(igb-iiy)/sca)-Yadd)+tZMN
C            elseif(ITPPSW.eq.2)then
C              ex=((FLOAT(iix-igl)/sca)-Xadd)+tYMN
C              ey=((FLOAT(igb-iiy)/sca)-Yadd)+tZMN
C            elseif(ITPPSW.eq.3)then
C              ex=((FLOAT(iix-igl)/sca)-Xadd)+tXMN
C              ey=((FLOAT(igb-iiy)/sca)-Yadd)+tYMN
C            endif
C            iupdown=0; isym=6
C            call etplot(ex,ey,iupdown,isym)
C            call forceflush()
C            WRITE(outs,'(a,f7.3,a,f7.3)')'The point is at x=',ex,
C     &                                   ' & y=',ey
C            CALL EASKOK(outs,'View OK?',OK,nbhelp)
C            IF(OK)then
C              rvpx=ex; rvpy=ey
C              vewdone=.false.; do_redraw =.true.
C              MODLEN=.true.; MODIFYVIEW=.TRUE.
C              rvpz=VIEWM(3)
C            endif
C          endif
        elseif(ITPPSW.eq.3)then
          CALL EASKABC(' Specify via: ',' ','editing',
     &     'accept current eyepoint','ignore ?',IEE,nbhelp)
          if(IEE.eq.3)then
            goto 10
          elseif(IEE.eq.2)then
            write(outs,'(a,3F7.1)') 'Perspective eyepoint is ',
     &        EYEM(1),EYEM(2),EYEM(3)
            CALL EASKOK(outs,'OK?',OK,nbhelp)
            if(OK)then
              rvpx=EYEM(1); rvpy=EYEM(2); rvpz=EYEM(3)
              call ln2az(EYEM(1),EYEM(2),EYEM(3),VIEWM(1),VIEWM(2),
     &          VIEWM(3),az,el)
              call AZ2UV(az,el,vdx,vdy,vdz)
              call ang3vtx(OXMN,OYMN,OZMN,EYEM(1),EYEM(2),EYEM(3),OXMX,
     &             OYMX,OZMX,eang)
              azimuth=az; elevtn=el
              vewdone=.false.; do_redraw =.true.
              MODLEN=.true.; MODIFYVIEW=.TRUE.
            endif
          else
            MODLEN=.true.; MODIFYVIEW=.TRUE.
          endif
        else
          IEE=1
        endif
          
        if(IEE.eq.1)then
          helptopic='eyepoint_editing'
          call gethelptext(helpinsub,helptopic,nbhelp)
  55      WRITE(HOLD,'(3f9.3)')rvpx,rvpy,rvpz
          CALL EASKS(HOLD,' Eye point  X  Y  Z (in metres): ',
     &      '  ',28,' ','eye point coord',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,rvpx,-999.,999.,'W','X cord',IER)
          CALL EGETWR(HOLD,K,rvpy,-999.,999.,'W','Y cord',IER)
          CALL EGETWR(HOLD,K,rvpz,0.,999.,'W','eye Z cord',IER)
          if(ier.ne.0)goto 55
          vewdone=.false.; do_redraw =.true.
          iupdown=0; isym=14
          call etplot(rvpx,rvpy,iupdown,isym)
          call forceflush()
          if(ITPPSW.eq.3)then
            helptopic='reflect_rad_view_in_per'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKOK(' ','Reflect this in perspective view?',
     &        OK,nbhelp)
            if(OK)then
              EYEM(1)=rvpx; EYEM(2)=rvpy; EYEM(3)=rvpz
              call ln2az(EYEM(1),EYEM(2),EYEM(3),VIEWM(1),VIEWM(2),
     &          VIEWM(3),az,el)
              azimuth=az; elevtn=el
              vewdone=.false.; do_redraw =.true.
              MODLEN=.true.; MODIFYVIEW=.TRUE.
              call usrmsg('Altering the eyepoint may require using the',
     &        'wireframe control to set the focus point.','W')
            endif
          endif
        endif

      elseif(INO.EQ.6)then

C Directional vector. Input as azimuth and elevation and then
C convert into normal or take the direction from the perspective.
        if(ITPPSW.eq.3)then
          helptopic='edit_or_accept_current'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKABC(' Specify via: ',' ','editing',
     &      'accept current direction','ignore ?',IEE,nbhelp)
          if(IEE.eq.3)then
            goto 10
          elseif(IEE.eq.2)then

C Update the viewing azimuth and elevation.
            call ln2az(EYEM(1),EYEM(2),EYEM(3),VIEWM(1),VIEWM(2),
     &        VIEWM(3),az,el)
            write(outs,'(a,F7.1,a,F7.1)') 'The perspective azimuth is ',
     &        az,' and elevation is ',el
            CALL EASKOK(outs,'View OK?',OK,nbhelp)
            if(OK)then
              azimuth=az; elevtn=el
              call  AZ2UV(azimuth,elevtn,vdx,vdy,vdz)
              call ang3vtx(OXMN,OYMN,OZMN,EYEM(1),EYEM(2),EYEM(3),OXMX,
     &             OYMX,OZMX,eang)
              vewdone=.false.; do_redraw =.true.
              MODLEN=.TRUE.; MODIFYVIEW=.TRUE.
            endif
          else
            MODLEN=.TRUE.; MODIFYVIEW=.TRUE.
          endif
        else
          IEE=1
        endif
        
        if(IEE.eq.1)then
          call edisp(iuout,' ')
          helptopic='view_azimuth_edit'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKR(azimuth,' ',' View azimuth (deg) ? ',
     &      -359.,'W',359.,'W',90.,'view azimuth',IER,nbhelp)
          CALL EASKR(elevtn,' ',' View elevation (deg) ? ',
     &      -89.,'W',89.,'W',0.,'view elevation',IER,nbhelp)
          call AZ2UV(azimuth,elevtn,vdx,vdy,vdz)
          call ang3vtx(OXMN,OYMN,OZMN,EYEM(1),EYEM(2),EYEM(3),OXMX,
     &             OYMX,OZMX,eang)
          vewdone=.false.; do_redraw =.true.
        endif

      elseif(INO.EQ.9.or.INO.EQ.10)then

C View angle horizontal and vertical.
        helptopic='view_angle_edit'
        call gethelptext(helpinsub,helptopic,nbhelp)
        write(outs,'(a,f6.1,a,f6.1)') 'View angle (hor)=',angh,
     &    ' deg. View angle @ bounds=',eang
        CALL EASKR(angh,outs,'View angle (hor deg) ? ',
     &    0.01,'W',180.,'W',60.,'hor view angle',IER,nbhelp)
        write(outs,'(a,f6.1,a)') 'View angle (vert)=',angv,
     &    ' deg.'
        CALL EASKR(angv,outs,' View angle (vert deg) ? ',
     &    0.01,'W',180.,'W',60.,'view angle',IER,nbhelp)
        if(ITPPSW.eq.3)then
          helptopic='reflect_rad_view_in_per'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKOK(' ','Reflect this in perspective view?',
     &      OK,nbhelp)
          if(OK)then
            ITBND=0
            ANG=angh; HANG=ANG/2.0
            MODLEN=.TRUE.; MODIFYVIEW=.TRUE.
          endif
        endif
        vewdone=.false.; do_redraw =.true.

      elseif(INO.EQ.11)then

C Clipping fore and aft.
        CALL EASKR(cutfor,' ','Clipping distance fore? ',
     &    0.0,'W',100.,'W',0.0,'clip dist for',IER,nbhelp)
        CALL EASKR(cutaft,' ','Clipping distance aft? ',
     &    0.0,'W',200.,'W',0.0,'clip dist aft',IER,nbhelp)
        vewdone=.false.

C toggle perspective or fisheye view.
      elseif(INO.EQ.12)then
        if (vewtyp(1:1).eq.'v') then
          vewtyp='h'
        elseif (vewtyp(1:1).eq.'h') then
          vewtyp='v'
        endif

      elseif(INO.EQ.14)then

C horiz axis...
        helptopic='e2r_east_axis'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(ITPPSW.eq.0)then
  41      WRITE(HOLD,'(1x,2f8.3)')tXMN,tXMX
          CALL EASKS(HOLD,' East axis minimum and maximum: ',
     &      '  ',28,' ','east axis min-max',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,tXMN,-300.,300.,'W','X min',IER)
          CALL EGETWR(HOLD,K,tXMX,tXMN,300.,'W','X max',IER)
          if(ier.ne.0)goto 41
          igwid=igw; igheight=igwh
          call axiscale(igwid,igheight,tXMN,tXMX,tYMN,tYMX,xsc,ysc,sca,
     &       Xadd,Yadd)
          call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
        elseif(ITPPSW.eq.1)then
 141      WRITE(HOLD,'(1x,2f8.3)')tXMN,tXMX
          CALL EASKS(HOLD,' South axis minimum and maximum: ',
     &      '  ',28,' ','south axis min-max',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,tXMN,-300.,300.,'W','X min',IER)
          CALL EGETWR(HOLD,K,tXMX,tXMN,300.,'W','X max',IER)
          if(ier.ne.0)goto 141
          igwid=igw; igheight=igwh
          call axiscale(igwid,igheight,tXMN,tXMX,tZMN,tZMX,xsc,ysc,sca,
     &      Xadd,Yadd)
          call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
        elseif(ITPPSW.eq.2)then
 241      WRITE(HOLD,'(1x,2f8.3)')tYMN,tYMX
          CALL EASKS(HOLD,' East axis minimum and maximum: ',
     &      '  ',28,' ','east axis min-max',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,tYMN,-300.,300.,'W','Y min',IER)
          CALL EGETWR(HOLD,K,tYMX,tYMN,300.,'W','Y max',IER)
          if(ier.ne.0)goto 241
          igwid=igw; igheight=igwh
          call axiscale(igwid,igheight,tYMN,tYMX,tZMN,tZMX,xsc,ysc,sca,
     &      Xadd,Yadd)
          call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
        elseif(ITPPSW.eq.3)then
        endif
        vewdone=.false.; do_redraw =.true.

      elseif(INO.EQ.15)then

C vertical axis...
        helptopic='e2r_north_axis'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(ITPPSW.eq.0)then
 144      WRITE(HOLD,'(1x,2f8.3)')tYMN,tYMX
          CALL EASKS(HOLD,' North axis minimum and maximum: ',
     &      '  ',28,' ','north axis min-max',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,tYMN,-300.,300.,'W','Y min',IER)
          CALL EGETWR(HOLD,K,tYMX,tYMN,300.,'W','Y max',IER)
          if(ier.ne.0)goto 144
          igwid=igw; igheight=igwh
          call axiscale(igwid,igheight,tXMN,tXMX,tYMN,tYMX,xsc,ysc,sca,
     &      Xadd,Yadd)
          call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
        elseif(ITPPSW.eq.1)then
 145      WRITE(HOLD,'(1x,2f8.3)')tZMN,tZMX
          CALL EASKS(HOLD,' Up axis minimum and maximum: ',
     &      '  ',28,' ','up axis min-max',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,tZMN,-300.,300.,'W','Y min',IER)
          CALL EGETWR(HOLD,K,tZMX,tZMN,300.,'W','Y max',IER)
          if(ier.ne.0)goto 145
          igwid=igw; igheight=igwh
          call axiscale(igwid,igheight,tXMN,tXMX,tZMN,tZMX,xsc,ysc,sca,
     &      Xadd,Yadd)
          call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
        elseif(ITPPSW.eq.2)then
 245      WRITE(HOLD,'(1x,2f8.3)')tZMN,tZMX
          CALL EASKS(HOLD,' Up axis minimum and maximum: ',
     &      '  ',28,' ','up axis min-max',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,tZMN,-300.,300.,'W','Y min',IER)
          CALL EGETWR(HOLD,K,tZMX,tZMN,300.,'W','Y max',IER)
          if(ier.ne.0)goto 245
          igwid=igw; igheight=igwh
          call axiscale(igwid,igheight,tYMN,tYMX,tZMN,tZMX,xsc,ysc,sca,
     &      Xadd,Yadd)
          call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
        elseif(ITPPSW.eq.3)then
        endif
        vewdone=.false.; do_redraw =.true.

      elseif(INO.EQ.17)then

C If an existing view then just update, if a new view then
C extend the list and the view command. If viewed point is
C near origin can use shorter format.
        if(rvpx.lt.99.0.and.rvpy.lt.99.0.and.rvpz.lt.99.0)then
          short=.true.
        else
          short=.false.
        endif
        call eclose(cutfor,0.0,0.1,okfore)
        call eclose(cutaft,0.0,0.1,okaft)

C Set up view bounds (for fisheye view).
        if (vewtyp(1:1).eq.'h') then
          anght=180.0
          angvt=180.0
        else
          anght=angh
          angvt=angv
        endif

        if(addview)then
          indxvew = indxvew +1
          addview=.false.  ! turn off the toggle
          rvewsh(indxvew) = trvewsh  ! remember new name
          if(short)then
            if(okfore.and.okaft)then
              write(doit,'(3a,3F7.1,a,3F7.3,a,3F5.2,2(a,F5.1),a)') 
     &        '-vt',vewtyp,' -vp',rvpx,rvpy,rvpz,' -vd',vdx,vdy,vdz,
     &        ' -vu ',vux,vuy,vuz,' -vh ',anght,' -vv ',angvt,
     &        ' -vs 0 -vl 0 -vo 0 -va 0'
            else
              write(doit,'(3a,3F7.1,a,3F7.3,a,3F5.2,4(a,F5.1))') 
     &        '-vt',vewtyp,' -vp',rvpx,rvpy,rvpz,' -vd',vdx,vdy,vdz,
     &        ' -vu ',vux,vuy,vuz,' -vh ',anght,' -vv ',angvt,
     &        ' -vs 0 -vl 0 -vo ',cutfor,' -va ',cutaft
            endif
          else
            if(okfore.and.okaft)then
              write(doit,'(3a,3F8.2,a,3F7.3,a,3F5.2,2(a,F5.1),a)') 
     &        '-vt',vewtyp,' -vp',rvpx,rvpy,rvpz,' -vd',vdx,vdy,vdz,
     &        ' -vu ',vux,vuy,vuz,' -vh ',anght,' -vv ',angvt,
     &        ' -vs 0 -vl 0 -vo 0 -va 0'
            else
              write(doit,'(3a,3F8.2,a,3F7.3,a,3F5.2,4(a,F5.1))') 
     &        '-vt',vewtyp,' -vp',rvpx,rvpy,rvpz,' -vd',vdx,vdy,vdz,
     &        ' -vu ',vux,vuy,vuz,' -vh ',anght,' -vv ',angvt,
     &        ' -vs 0 -vl 0 -vo ',cutfor,' -va ',cutaft
            endif
          endif
          WRITE(vewcmds(indxvew),'(a)') doit  ! new cmd
        else
          if(short)then
            if(okfore.and.okaft)then
              write(doit,'(3a,3F7.1,a,3F7.3,a,3F5.2,2(a,F5.1),a)') 
     &        '-vt',vewtyp,' -vp',rvpx,rvpy,rvpz,' -vd',vdx,vdy,vdz,
     &        ' -vu ',vux,vuy,vuz,' -vh ',anght,' -vv ',angvt,
     &        ' -vs 0 -vl 0 -vo 0 -va 0'
            else
              write(doit,'(3a,3F7.1,a,3F7.3,a,3F5.2,4(a,F5.1))') 
     &        '-vt',vewtyp,' -vp',rvpx,rvpy,rvpz,' -vd',vdx,vdy,vdz,
     &        ' -vu ',vux,vuy,vuz,' -vh ',anght,' -vv ',angvt,
     &        ' -vs 0 -vl 0 -vo ',cutfor,' -va ',cutaft
            endif
          else
            if(okfore.and.okaft)then
              write(doit,'(3a,3F8.2,a,3F7.3,a,3F5.2,2(a,F5.1),a)') 
     &        '-vt',vewtyp,' -vp',rvpx,rvpy,rvpz,' -vd',vdx,vdy,vdz,
     &        ' -vu ',vux,vuy,vuz,' -vh ',anght,' -vv ',angvt,
     &        ' -vs 0 -vl 0 -vo 0 -va 0'
            else
              write(doit,'(3a,3F8.2,a,3F7.3,a,3F5.2,4(a,F5.1))') 
     &        '-vt',vewtyp,' -vp',rvpx,rvpy,rvpz,' -vd',vdx,vdy,vdz,
     &        ' -vu ',vux,vuy,vuz,' -vh ',anght,' -vv ',angvt,
     &        ' -vs 0 -vl 0 -vo ',cutfor,' -va ',cutaft
            endif
          endif
          WRITE(vewcmds(ipckvew),'(a)') doit  ! current cmd
        endif

C Write both standard and wavefront versions of rif file.
        IFC=3
        call mkrif(ITA2,RIFNAME(ISCENE),IFC,0,'-',IER)
        rifwave='obj.rif'
        call mkrif(ITA2,rifwave,IFC,0,'w',IER)
        if (NBSRIF(ISCENE).eq.1) then
          call mkrif(ita2,LBSRIF(ISCENE),IFC,1,'-',ier)
        endif
        vewdone=.true.

C Update view point from model display.
      elseif(INO.eq.18)then
        if (MMOD.ne.8) goto 10
        rvpx=EYEM(1); rvpy=EYEM(2); rvpz=EYEM(3)
        angh=ANG; angv=ANG
        call ln2az(rvpx,rvpy,rvpz,VIEWM(1),VIEWM(2),VIEWM(3),
     &    azimuth,elevtn)
        call AZ2UV(azimuth,elevtn,vdx,vdy,vdz)

      elseif(INO.EQ.(NITEMS-1))then

c Explains main menu.
        helptopic='view_definition_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('view menu',nbhelp,'-',0,0,IER)

C Exit.
      elseif(INO.EQ.NITEMS)then
        if(vewdone)then
          return
        else
          helptopic='e2r_remind_to_save'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKOK('View information not saved!',
     &      'OK to exit?',OK,nbhelp)
          IF(OK)return
        endif
      endif
      goto 10

      end
