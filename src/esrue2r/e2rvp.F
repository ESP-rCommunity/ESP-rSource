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

C ESRU: e2r viewpoint facilities.
C  VCONE: View cone for plan/elevation.
C  VEWFORM: View control and definition.

C ******* View cone for plan/elevation.
      SUBROUTINE VCONE(direc)
#include "building.h"
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      common/radv/rvpx,rvpy,rvpz,vdx,vdy,vdz,angh,angv,azim,elev
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      character direc*1
#ifdef OSX
      integer igwid,igheight  ! for use with axiscale
      integer iupdown,isym    ! passed to etplot
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iupdown,isym    ! passed to etplot
#endif

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
        call linescale(igl,Xadd,sca,igb,Yadd,sca)
        iupdown=0
        isym=14
        call etplot(rvpx,rvpy,iupdown,isym)
        isym=0
        call etplot(rvpx,rvpy,iupdown,isym)
        wxe=VCL*vdx
        wye=VCL*vdy
        iupdown=-302
        isym=0
        call etplot(rvpx+wxe,rvpy+wye,iupdown,isym)
        astrt=azim+(angh/2.)
        RYAZI = astrt*RAD
        xastrt = rvpx+(VCL*SIN(RYAZI))
        yastrt = rvpy+(VCL*COS(RYAZI))
        iupdown=0
        isym=0
        call etplot(rvpx,rvpy,iupdown,isym)
        iupdown=1
        call etplot(xastrt,yastrt,iupdown,isym)
        aend=astrt-angh
        RYAZI = aend*RAD
        xaend = rvpx+(VCL*SIN(RYAZI))
        yaend = rvpy+(VCL*COS(RYAZI))
        iupdown=0
        isym=0
        call etplot(rvpx,rvpy,iupdown,isym)
        iupdown=1
        call etplot(xaend,yaend,iupdown,isym)
      elseif(direc.eq.'S')then

C If south elevation, omitt the view-cone.
        igwid=igw
        igheight=igwh
        call axiscale(igwid,igheight,XMN,XMX,ZMN,ZMX,xsc,ysc,sca,
     &     Xadd,Yadd)
        call linescale(igl,Xadd,sca,igb,Yadd,sca)
        iupdown=0
        isym=14
        call etplot(rvpx,rvpz,iupdown,isym)
        isym=0
        call etplot(rvpx,rvpz,iupdown,isym)
        wxe=VCL*vdx
        wze=VCL*vdz
        iupdown=-302
        isym=0
        call etplot(rvpx+wxe,rvpz+wze,iupdown,isym)
      elseif(direc.eq.'E')then
        igwid=igw
        igheight=igwh
        call axiscale(igwid,igheight,YMN,YMX,ZMN,ZMX,xsc,ysc,sca,
     &     Xadd,Yadd)
        call linescale(igl,Xadd,sca,igb,Yadd,sca)
        iupdown=0
        isym=14
        call etplot(rvpy,rvpz,iupdown,isym)
        isym=0
        call etplot(rvpy,rvpz,iupdown,isym)
        wye=VCL*vdy
        wze=VCL*vdz
        iupdown=-302
        isym=0
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
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/pophelp/h(60)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/RAY5OB/OXMN,OYMN,OZMN,OXMX,OYMX,OZMX
      common/raddn/skydone,outdone,indone,misdone,vewdone,glzdone
      common/radv/rvpx,rvpy,rvpz,vdx,vdy,vdz,angh,angv,azim,elev
      common/e2rv/vewcmds(20),rvewsh(20),indxvew,ipckvew
      common/rad2/mono,intext,ifocz
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      common/raddata/SCENE(MCOM+1),RIFNAME(MCOM+1),SCENERT(MCOM+1),
     &               RIFDESC(MCOM+1),RIFPURP(MCOM+1)
      common/raddata2/NRIF,IRIFFOC
      common/raddata3/NBSRIF(MCOM+1),LBSRIF(MCOM+1)

      LOGICAL MODIFY,MODLEN,MODBND,XST,OK,DOK,addview
      logical skydone,outdone,indone,misdone,vewdone,glzdone,redraw

      DIMENSION ITEM(19),IVALS(MCOM),VALT(20),IVALV(20)

      character SCENE*28,RIFNAME*72,SCENERT*28,RIFDESC*72,RIFPURP*12

      character item*30,h*72,hold*28,VALT*28,OUTSTR*124,WORD*20
      character outs*124,LBSRIF*72
      character doit*124,ltmp*72,trvewsh*10
      character vewcmds*124,rvewsh*10
#ifdef OSX
      integer igwid,igheight  ! for use with axiscale
      integer iupdown,isym    ! passed to etplot
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iupdown,isym    ! passed to etplot
#endif

C Addview = true then save editing as a new view, if false
C then it is an update to an existing view.
      IUF=IFIL+1
      ITA2 = IFIL+7
      vewdone=.false.
      redraw =.true.
      addview=.false.
      trvewsh='new'
      ier=0
      ITPPSW=3

C First thing is to ask what zones to include
C in the plan view.
C If outside view then initially select all zones...
      if(intext.eq.0)then
        nzg=NCOMP
        izgfoc=0
        DO 44 I=1,nzg
          nznog(I)=I
  44    CONTINUE
        call edisp(iuout,'Including all zones in outside view.')
      else
        call edisp(iuout,'For an inside view... ')
        H(1)=' Pick one, several or all zones for inclusion'
        INPIC=NCOMP
        CALL EPICKS(INPIC,IVALS,' ','Which zones to include:',
     &      12,NCOMP,zname,' zone list',IER,1)
        nzg=INPIC
        izgfoc=0
        if(nzg.eq.1)izgfoc=IVALS(1)
        DO 45 I=1,nzg
          nznog(I)=IVALS(I)
  45    CONTINUE
      endif
      MODBND=.TRUE.
      MODLEN=.TRUE.
      MODIFY=.TRUE.
      CALL BNDOBJ(1,IER)

C Outside view. Scale bounding box to be a bit less than
C extremes of the ground disk. Save off zone bounds before
C adding in the ground disk.
      OXMN=XMN
      OYMN=YMN
      OZMN=ZMN
      OXMX=XMX
      OYMX=YMX
      OZMX=ZMX
      if(intext.eq.0)then
        XD=(XMX-XMN)**2 + (YMX-YMN)**2
        DD=SQRT(XD)*1.5
        cx=XMN+((XMX-XMN)/2.)
        cy=YMN+((YMX-YMN)/2.)
        XMN=cx-DD
        XMX=cx+DD
        YMN=cy-DD
        YMX=cy+DD
        ZMN=ZMN
        ZMX=DD
      endif
      MODBND=.FALSE.
      MODLEN=.FALSE.

C Pull up a list of views and ask user for which one.
  142 H(1)='Please select one view in the case of an interactive'
      H(2)='session.  If no view selected the first one will be'
      H(3)='created. '
      do 333 iv=1,indxvew
        write(valt(iv),'(3a)') rvewsh(iv)(1:lnblnk(rvewsh(iv))),':',
     &                         vewcmds(iv)(1:16)
  333 continue
      valt(indxvew+1) = 'Create a new view.'
      IX=1
  334 CALL EPICKS(IX,IVALV,' Select a view: ',' ',
     &         28,indxvew+1,VALT,'View options',IER,3)
      if(IX.eq.0)then
        H(1)='You must either select one of the known views'
        H(2)='or create a new view point. '
        CALL EASKAB(' One of the options must be selected:',' ',
     &     'select an option','exit facility',IW,2)
        if(iw.eq.1)goto 334
        if(iw.eq.2)return
      endif
      ipckvew=IVALV(1)
      if(ipckvew.eq.indxvew+1)then

C Ask for name of new viewpoint file and set eye point at centre.
        addview = .true.
        if(intext.eq.0)then
          rvpx=XMN
          rvpy=YMN
          rvpz=ZMX
        else
          rvpx=XMN+0.1
          rvpy=YMN+0.1
          rvpz=VIEWM(3)
        endif
        call ln2az(rvpx,rvpy,rvpz,VIEWM(1),VIEWM(2),VIEWM(3),azim,elev)
        H(1)='This is a tag used for reference.'
        CALL EASKS(trvewsh,'Name for view?',' ',10,'vew1',
     &                                         'short view tag',IER,1)
        call edisp(iuout,'Please define the new view point now.')

C If the default view.
      elseif(ipckvew.eq.1)then
        if(intext.eq.0)then
          rvpx=-100.0
          rvpy=-100.0
          rvpz=100.0
          call ang3vtx(OXMN,OYMN,OZMN,EYEM(1),EYEM(2),EYEM(3),OXMX,
     &             OYMX,OZMX,eang)
          angv=eang*1.25
          angh=eang*1.25
        else
          rvpx=XMN+0.1
          rvpy=YMN+0.1
          rvpz=VIEWM(3)
          angv=90.0
          angh=90.0
        endif
        call ln2az(rvpx,rvpy,rvpz,VIEWM(1),VIEWM(2),VIEWM(3),azim,elev)
        call AZ2UV(azim,elev,vdx,vdy,vdz)
      else

C Perhaps a referenced file.
        write(OUTSTR,'(a)')vewcmds(ipckvew)(1:lnblnk(vewcmds(ipckvew)))
        K=0
        CALL EGETW(OUTSTR,K,WORD,'W','view command',IFLAG)
        if(word(1:3).eq.'-vf')then
          CALL EGETRM(OUTSTR,K,ltmp,'W','view file',IER)
          call edisp(iuout,' Recovered view file name.')
          if(ltmp.ne.' ')then
            CALL ERPFREE(ITA2,ISTAT)
            call FINDFIL(ltmp,XST)
            IF(XST)THEN
              trvewsh = rvewsh(ipckvew)
              addview = .false.

C Read a radiance view point file and make sure that the
C view point is within the bounds of the image.
              call RRVEW(ITA2,ltmp,IER)
              call UV2AZ(vdx,vdy,vdz,azim,elev)
              XMN=AMIN1(XMN,rvpx)
              YMN=AMIN1(YMN,rvpy)
              ZMN=AMIN1(ZMN,rvpz)
              XMX=AMAX1(XMX,rvpx)
              YMX=AMAX1(YMX,rvpy)
              ZMX=AMAX1(ZMX,rvpz)
              EYEM(1)=rvpx
              EYEM(2)=rvpy
              EYEM(3)=rvpz
              if(intext.eq.0)then
                ITPPSW=3
                call ang3vtx(OXMN,OYMN,OZMN,EYEM(1),EYEM(2),EYEM(3),
     &             OXMX,OYMX,OZMX,eang)
                angv=eang*1.25
                angh=eang*1.25
                ANG=angh
                HANG=ANG/2.0
                ITBND=0
                call edisp(iuout,' ')
                call edisp(iuout,'Switching to static viewing angles.')
                MODIFY=.TRUE.
                MODLEN=.TRUE.
              else
                ITPPSW=0
                call edisp(iuout,' ')
                call edisp(iuout,'Use plan or elevation views.')
                angv=90.0
                angh=90.0
              endif
            else
              call edisp(iuout,'File not found, returning.')
              return
            endif
          else
            call edisp(iuout,'Could not parse the command:')
            call edisp(iuout,vewcmds(ipckvew))
            return
          endif
        endif
      endif

C Reflect eyepoint in the perspective view.
      EYEM(1)=rvpx
      EYEM(2)=rvpy
      EYEM(3)=rvpz
      call AZ2UV(azim,elev,vdx,vdy,vdz)
      if(intext.eq.0)then
        ITPPSW=3
        call ang3vtx(OXMN,OYMN,OZMN,EYEM(1),EYEM(2),EYEM(3),OXMX,
     &             OYMX,OZMX,eang)
        angv=eang*1.25
        angh=eang*1.25
        ANG=angh
        HANG=ANG/2.0
        ITBND=0
        call edisp(iuout,' ')
        call edisp(iuout,'Switching to static viewing angles... ')
        MODIFY=.TRUE.
        MODLEN=.TRUE.
      else
        ITPPSW=0
        call edisp(iuout,' ')
        call edisp(iuout,'Use plan or elevation views for interiors...')
        angv=90.0
        angh=90.0
      endif
10    INO = -4

C Reconstitute the viewing vector and eye angle.
      call AZ2UV(azim,elev,vdx,vdy,vdz)
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
      write(ITEM(6),'(A,2F6.1)')  'f direc azi elv:',azim,elev
      write(ITEM(7),'(A,3F6.2)')  '  (vector):',vdx,vdy,vdz
      if(ITPPSW.eq.3)then
        write(ITEM(8),'(A,F5.1,a,F5.1,a)')
     &                            'h view angle: ',angh,' (',eang,')'
      else
        write(ITEM(8),'(A,F5.1)') 'h view angle: ',angh
      endif
      ITEM(9) =                   '  __________________________  '
      if(ITPPSW.eq.0)then
        write(ITEM(10),'(A,2F6.1)') 'l east min~max : ',XMN,XMX
        write(ITEM(11),'(A,2F6.1)') 'm north min~max: ',YMN,YMX
      elseif(ITPPSW.eq.1)then
        write(ITEM(10),'(A,2F6.1)') 'l south min~max: ',XMN,XMX
        write(ITEM(11),'(A,2F6.1)') 'm up    min~max: ',ZMN,ZMX
      elseif(ITPPSW.eq.2)then
        write(ITEM(10),'(A,2F6.1)') 'l east min~max : ',YMN,YMX
        write(ITEM(11),'(A,2F6.1)') 'm up    min~max: ',ZMN,ZMX
      elseif(ITPPSW.eq.3)then
        write(ITEM(10),'(A,2F6.1)') 'l east min~max : ',XMN,XMX
        write(ITEM(11),'(A,2F6.1)') 'm north min~max: ',YMN,YMX
      endif
      ITEM(12) =                  '  __________________________  '
      ITEM(13) =                  '> save view info              '
      ITEM(14) =                  '  __________________________  '
      ITEM(15) =                  '? help                        '
      ITEM(16) =                  '- exit                        '
      NITEMS = 16
      if(redraw)then

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
          MODIFY=.TRUE.
          CALL ADJVIEW(IER)
          MODLEN=.FALSE.
          MODIFY=.FALSE.
        endif
        redraw =.false.
      endif

C Help text for main menu.
      H(1)='This menu selects the viewpoint and direction, from'
      H(2)='where the Radiance image will be viewed.'
      H(3)=' '
      H(4)='For external views the perspective feedback may be '
      H(5)='preferable to set views.  You may need to interact  '
      H(6)='with the `wireframe control` to fine-tune the view '
      H(7)='as Radiance uses an eyepoint & view vector approach'
      H(8)='while e2r is built on an eyepoint & viewed point '
      H(9)='approach. '
      H(10)=' '
      H(11)='For internal views the perspecitve feedback is not '
      H(12)='appropriate--use the plan & elevation views. '
      H(13)=' '
      H(14)='The viewpoint can be changed in three ways:'
      H(15)='* Eye Point: The (x,y,z) position of the eye point are'
      H(16)='  entered directly into the computer.'
      H(17)='* Pick EP on plan view: (easier!) select the eye point'
      H(18)='  by placing the cursor on the desired point and press'
      H(19)='  the left-hand button.'
      H(20)='* Accept perspective viewing eyepoint (easiest)  '
      H(21)='  '
      H(22)='The View Direction can be changed by: '
      H(23)='* Dir az elv: The direction and elevation are entered'
      H(24)='  directly into the computer. North is 0 deg and East'
      H(25)='  is 90 deg. The elevation is positive for up and '
      H(26)='  negative for down.'
      H(27)='* Accept perspective viewing direction.'
      H(28)=' '
      H(29)='* View angle by editing, or accepting perspective.'
      H(30)='  NOTE that the angle in () is that required to see '
      H(31)='  the whole ground disk.'

      CALL EMENU(' Viewpoint definition',ITEM,NITEMS,INO)
      if(INO.EQ.1)then

C Present list of views.
        vewdone=.false.
        redraw =.true.
        MODLEN=.true.
        MODIFY=.TRUE.
        goto 142
      elseif(INO.EQ.2)then
        H(1)=' Pick one, several or all zones for inclusion'
        INPIC=NCOMP
        CALL EPICKS(INPIC,IVALS,
     &    ' More than one zone may be seen in the view',
     &    ' Which to include: ',12,NCOMP,zname,' zone list',IER,1)
        call edisp(iuout,' Checking bounds....')
        nzg=INPIC
        izgfoc=0
        DO 46 I=1,nzg
          nznog(I)=IVALS(I)
  46    CONTINUE
        MODLEN=.TRUE.
        MODBND=.TRUE.
        CALL BNDOBJ(1,IER)
        redraw =.true.
      elseif(INO.EQ.3)then
        H(1)='In plan view you can change the viewing parameters,'
        H(2)='the elevations are primarily for checking. '
        CALL EASKABCD(' View choices: ',' ','plan',
     &    'south elevation ','east elevation','perspective ?',IW,2)
        if(IW-1.ne.ITPPSW)redraw =.true.
        ITPPSW=IW-1
      elseif(INO.EQ.5)THEN

C Eye Point...(assume begins at centre of zone).
        if(ITPPSW.eq.0)then
          H(1)='In plan view you can only edit or pick on the plan.'
          CALL EASKABC(' Specify via: ',' ','editing',
     &     'pick on plan','ignore ?',IEE,1)
          if(IEE.eq.3)then
            goto 10

C Select point from plan or current perspective.
          elseif(IEE.eq.2)then
            call usrmsg(' ',' Select point via cursor...','-')
            igwid=igw
            igheight=igwh
            call axiscale(igwid,igheight,XMN,XMX,YMN,YMX,xsc,ysc,sca,
     &        Xadd,Yadd)
            call linescale(igl,Xadd,sca,igb,Yadd,sca)

C << implemented in GTK? >>
            CALL trackview(ii,ix,iy)

C Transform back from mouse position to actual coordinate.
            ex=((FLOAT(ix-igl)/sca)-Xadd)
            ey=((FLOAT(igb-iy)/sca)-Yadd)
            iupdown=0
            isym=6
            call etplot(ex,ey,iupdown,isym)
            call forceflush()
            WRITE(outs,'(a,f7.3,a,f7.3)')' The point is at x=',ex,
     &                                   ' & y=',ey
            dok=.true.
            h(1)='Pause in case you need to revise the viewpoint. '
            CALL ASKOK(outs,' Is this view ok?',OK,dok,1)
            IF(OK)then
              rvpx=ex
              rvpy=ey
              vewdone=.false.
              redraw =.true.
              MODLEN=.true.
              MODIFY=.TRUE.
              rvpz=VIEWM(3)
            endif
          endif
        elseif(ITPPSW.eq.3)then
          CALL EASKABC(' Specify via: ',' ','editing',
     &     'accept current eyepoint','ignore ?',IEE,1)
          if(IEE.eq.3)then
            goto 10
          elseif(IEE.eq.2)then
            write(outs,'(a,3F7.1)') 'The perspective eyepoint is ',
     &        EYEM(1),EYEM(2),EYEM(3)
            dok=.true.
            h(1)='Pause to allow you to adjust the eyepoint. '
            CALL ASKOK(outs,' Is this ok ?',OK,dok,1)
            if(OK)then
              rvpx=EYEM(1)
              rvpy=EYEM(2)
              rvpz=EYEM(3)
              call ln2az(EYEM(1),EYEM(2),EYEM(3),VIEWM(1),VIEWM(2),
     &          VIEWM(3),az,el)
              azim=az
              elev=el
              vewdone=.false.
              redraw =.true.
              MODLEN=.true.
              MODIFY=.TRUE.
            endif
          else
            MODLEN=.true.
            MODIFY=.TRUE.
          endif
        else
          IEE=1
        endif
          
        if(IEE.eq.1)then
          H(1)='The eyepoint is in metres in the coordinate system '
          H(2)='of the site. '
  55      WRITE(HOLD,'(3f9.3)')rvpx,rvpy,rvpz
          CALL EASKS(HOLD,' Eye point  X  Y  Z (in metres): ',
     &      '  ',28,' ','eye point coord',IER,2)
          K=0
          CALL EGETWR(HOLD,K,rvpx,-999.,999.,'W','X cord',IER)
          CALL EGETWR(HOLD,K,rvpy,-999.,999.,'W','Y cord',IER)
          CALL EGETWR(HOLD,K,rvpz,0.,999.,'W','eye Z cord',IER)
          if(ier.ne.0)goto 55
          vewdone=.false.
          redraw =.true.
          iupdown=0
          isym=14
          call etplot(rvpx,rvpy,iupdown,isym)
          call forceflush()
          if(ITPPSW.eq.3)then
            dok=.false.
            h(1)='The wireframe view can be updated to approximate'
            h(2)='the radiance view (does not work well for internal'
            h(3)='views). '
            CALL ASKOK(' ','Reflect this in perspective view?',OK,dok,3)
            if(OK)then
              EYEM(1)=rvpx
              EYEM(2)=rvpy
              EYEM(3)=rvpz
              call ln2az(EYEM(1),EYEM(2),EYEM(3),VIEWM(1),VIEWM(2),
     &          VIEWM(3),az,el)
              azim=az
              elev=el
              vewdone=.false.
              redraw =.true.
              MODLEN=.true.
              MODIFY=.TRUE.
              call usrmsg('Altering the eyepoint may require using the',
     &        'wireframe control to set the viewed point. ','W')
            endif
          endif
        endif
      elseif(INO.EQ.6)then

C Directional vector. Input as azimuth and elevation and then
C convert into normal or take the direction from the perspective.
        if(ITPPSW.eq.3)then
          CALL EASKABC(' Specify via: ',' ','editing',
     &     'accept current direction','ignore ?',IEE,1)
          if(IEE.eq.3)then
            goto 10
          elseif(IEE.eq.2)then
            call ln2az(EYEM(1),EYEM(2),EYEM(3),VIEWM(1),VIEWM(2),
     &        VIEWM(3),az,el)
            write(outs,'(a,F7.1,a,F7.1)') 'The perspective azimuth is ',
     &        az,' and elevation is ',el
            dok=.true.
            h(1)='Pause to allow you to adjust the view. '
            CALL ASKOK(outs,' Is this view ok?',OK,dok,1)
            if(OK)then
              azim=az
              elev=el
              call  AZ2UV(azim,elev,vdx,vdy,vdz)
              vewdone=.false.
              redraw =.true.
              MODLEN=.TRUE.
              MODIFY=.TRUE.
            endif
          else
            MODLEN=.TRUE.
            MODIFY=.TRUE.
          endif
        else
          IEE=1
        endif
        
        if(IEE.eq.1)then
          call edisp(iuout,' ')
          H(1)='The view azimuth 0= looking north, 90 =looking east.'
          CALL EASKR(azim,' ',' View azimuth (deg) ? ',
     &      -359.,'W',359.,'W',90.,'view azimuth',IER,1)
          CALL EASKR(elev,' ',' View elevation (deg) ? ',
     &      -89.,'W',89.,'W',0.,'view elevation',IER,1)
          call AZ2UV(azim,elev,vdx,vdy,vdz)
          vewdone=.false.
          redraw =.true.
        endif
      elseif(INO.EQ.8)then

C View angle...
        H(1)='The view angle is normally 60 deg.  The range'
        H(2)='is 20. < x < 179.'
        write(outs,'(a,f6.1,a,f6.1)') ' View angle=',angh,
     &    ' deg. View angle @ bounds=',eang
        CALL EASKR(angh,outs,' View angle (deg) ? ',
     &    0.01,'W',180.,'W',60.,'view angle',IER,2)
        angv=angh
        if(ITPPSW.eq.3)then
          dok=.false.
          h(1)='The wireframe view can be updated to approximate'
          h(2)='the radiance view (does not work well for internal'
          h(3)='views). '
          CALL ASKOK(' ','Reflect this in perspective view?',OK,dok,3)
          if(OK)then
            ITBND=0
            ANG=angh
            HANG=ANG/2.0
            MODLEN=.TRUE.
            MODIFY=.TRUE.
          endif
        endif
        vewdone=.false.
        redraw =.true.
      elseif(INO.EQ.10)then

C horiz axis...
        H(1)='Adjust axis min and max to allow more distant view.'
        if(ITPPSW.eq.0)then
  41      WRITE(HOLD,'(1x,2f8.3)')XMN,XMX
          CALL EASKS(HOLD,' East axis minimum and maximum: ',
     &      '  ',28,' ','east axis min-max',IER,1)
          K=0
          CALL EGETWR(HOLD,K,XMN,-300.,300.,'W','X min',IER)
          CALL EGETWR(HOLD,K,XMX,XMN,300.,'W','X max',IER)
          if(ier.ne.0)goto 41
          igwid=igw
          igheight=igwh
          call axiscale(igwid,igheight,XMN,XMX,YMN,YMX,xsc,ysc,sca,
     &       Xadd,Yadd)
          call linescale(igl,Xadd,sca,igb,Yadd,sca)
        elseif(ITPPSW.eq.1)then
 141      WRITE(HOLD,'(1x,2f8.3)')XMN,XMX
          CALL EASKS(HOLD,' South axis minimum and maximum: ',
     &      '  ',28,' ','south axis min-max',IER,1)
          K=0
          CALL EGETWR(HOLD,K,XMN,-300.,300.,'W','X min',IER)
          CALL EGETWR(HOLD,K,XMX,XMN,300.,'W','X max',IER)
          if(ier.ne.0)goto 141
          igwid=igw
          igheight=igwh
          call axiscale(igwid,igheight,XMN,XMX,ZMN,ZMX,xsc,ysc,sca,
     &      Xadd,Yadd)
          call linescale(igl,Xadd,sca,igb,Yadd,sca)
        elseif(ITPPSW.eq.2)then
 241      WRITE(HOLD,'(1x,2f8.3)')YMN,YMX
          CALL EASKS(HOLD,' East axis minimum and maximum: ',
     &      '  ',28,' ','east axis min-max',IER,1)
          K=0
          CALL EGETWR(HOLD,K,YMN,-300.,300.,'W','Y min',IER)
          CALL EGETWR(HOLD,K,YMX,YMN,300.,'W','Y max',IER)
          if(ier.ne.0)goto 241
          igwid=igw
          igheight=igwh
          call axiscale(igwid,igheight,YMN,YMX,ZMN,ZMX,xsc,ysc,sca,
     &      Xadd,Yadd)
          call linescale(igl,Xadd,sca,igb,Yadd,sca)
        elseif(ITPPSW.eq.3)then
        endif
        vewdone=.false.
        redraw =.true.
      elseif(INO.EQ.11)then

C vertical axis...
        H(1)='Adjust axis min and max to allow more distant view.'
        if(ITPPSW.eq.0)then
 144      WRITE(HOLD,'(1x,2f8.3)')YMN,YMX
          CALL EASKS(HOLD,' North axis minimum and maximum: ',
     &      '  ',28,' ','north axis min-max',IER,1)
          K=0
          CALL EGETWR(HOLD,K,YMN,-300.,300.,'W','Y min',IER)
          CALL EGETWR(HOLD,K,YMX,YMN,300.,'W','Y max',IER)
          if(ier.ne.0)goto 144
          igwid=igw
          igheight=igwh
          call axiscale(igwid,igheight,XMN,XMX,YMN,YMX,xsc,ysc,sca,
     &      Xadd,Yadd)
          call linescale(igl,Xadd,sca,igb,Yadd,sca)
        elseif(ITPPSW.eq.1)then
 145      WRITE(HOLD,'(1x,2f8.3)')ZMN,ZMX
          CALL EASKS(HOLD,' Up axis minimum and maximum: ',
     &      '  ',28,' ','up axis min-max',IER,1)
          K=0
          CALL EGETWR(HOLD,K,ZMN,-300.,300.,'W','Y min',IER)
          CALL EGETWR(HOLD,K,ZMX,ZMN,300.,'W','Y max',IER)
          if(ier.ne.0)goto 145
          igwid=igw
          igheight=igwh
          call axiscale(igwid,igheight,XMN,XMX,ZMN,ZMX,xsc,ysc,sca,
     &      Xadd,Yadd)
          call linescale(igl,Xadd,sca,igb,Yadd,sca)
        elseif(ITPPSW.eq.2)then
 245      WRITE(HOLD,'(1x,2f8.3)')ZMN,ZMX
          CALL EASKS(HOLD,' Up axis minimum and maximum: ',
     &      '  ',28,' ','up axis min-max',IER,1)
          K=0
          CALL EGETWR(HOLD,K,ZMN,-300.,300.,'W','Y min',IER)
          CALL EGETWR(HOLD,K,ZMX,ZMN,300.,'W','Y max',IER)
          if(ier.ne.0)goto 245
          igwid=igw
          igheight=igwh
          call axiscale(igwid,igheight,YMN,YMX,ZMN,ZMX,xsc,ysc,sca,
     &      Xadd,Yadd)
          call linescale(igl,Xadd,sca,igb,Yadd,sca)
        elseif(ITPPSW.eq.3)then
        endif
        vewdone=.false.
        redraw =.true.
      elseif(INO.EQ.13)then

C If an existing view then just update, if a new view then
C extend the list.
        if(addview)then
          indxvew = indxvew +1
        endif
        rvewsh(indxvew) = trvewsh
        write(doit,'(a,3F9.3,a,3F8.4,a,F6.1,a,F6.1,a)') 
     &     '-vtv -vp ',rvpx,rvpy,rvpz,' -vd ',vdx,vdy,vdz,
     &     ' -vu 0 0 1 -vh ',angh,' -vv ',angv,' -vs 0 -vl 0 '
        WRITE(vewcmds(indxvew),'(a)') doit
        IFC=3
        call MKRIF(ITA2,RIFNAME(IRIFFOC),IFC,0,IER)
        if (NBSRIF(IRIFFOC).eq.1) then
          call mkrif(ita2,LBSRIF(IRIFFOC),IFC,1,ier)
        endif
        vewdone=.true.
      elseif(INO.EQ.(NITEMS-1))then

c Explains main menu.
        CALL PHELPD('view menu',31,'-',0,0,IER)
      elseif(INO.EQ.NITEMS)then
        if(vewdone)then
          return
        else
          dok=.false.
          h(1)='Recent changes in the view parameters might be lost'
          h(2)='if you exit without saving the data. '
          CALL ASKOK('View information has not been saved',
     &      'Is it ok to exit?',OK,dok,2)
          IF(OK)return
        endif
      endif
      goto 10

      end
