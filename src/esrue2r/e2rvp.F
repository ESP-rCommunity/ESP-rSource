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
#include "e2r_common.h"
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
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
        wxe=VCL*vdx
        wye=VCL*vdy
        iupdown=-302
        isym=0
        call etplot(rvpx+wxe,rvpy+wye,iupdown,isym)
        astrt=azimuth+(angh/2.)
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
        call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
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
        call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
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
#include "e2r_common.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/RAY5OB/OXMN,OYMN,OZMN,OXMX,OYMX,OZMX
      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh

      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW

      LOGICAL MODIFY,MODLEN,MODBND
      COMMON/RAY3/MODIFY,MODLEN,MODBND

      LOGICAL XST,OK,DOK
      logical addview   ! mark we are working a new view
      logical redraw

      DIMENSION ITEM(19),IVALS(MCOM),VALT(20),IVALV(20)

      character item*30,hold*28,VALT*28,OUTSTR*124,WORD*20
      character outs*124
      character doit*124,ltmp*72,lview*124
      character trvewsh*10,mrvewsh*30
      integer NITEMS,INO  ! max items and current menu item

#ifdef OSI
      integer igwid,igheight  ! for use with axiscale
      integer iupdown,isym    ! passed to etplot
      integer iigl,iigr,iigt,iigb,iigw,iigwh
#else
      integer*8 igwid,igheight  ! for use with axiscale
      integer*8 iupdown,isym    ! passed to etplot
      integer*8 iigl,iigr,iigt,iigb,iigw,iigwh
#endif

      integer irif ! loop variable for more than one rif per scene
C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='VEWFORM'  ! set for subroutine

C Cast values for linescale.
      iigl=igl;iigr=igr;iigt=igt;iigb=igb;iigw=igw;iigwh=igwh

C Addview = true then save editing as a new view, if false
C then it is an update to an existing view.
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
  142 do 333 iv=1,indxvew
        write(valt(iv),'(3a)') rvewsh(iv)(1:lnblnk(rvewsh(iv))),':',
     &                         vewcmds(iv)(1:16)
  333 continue
      valt(indxvew+1) = 'Create a new view.'
      IX=1
  334 CALL EPICKS(IX,IVALV,' Select a view: ',' ',
     &  28,indxvew+1,VALT,'View options',IER,nbhelp)
      if(IX.eq.0)then
        CALL EASKAB(' One of the options must be selected:',' ',
     &    'select an option','exit view point facility',IW,nbhelp)
        if(iw.eq.1)goto 334
        if(iw.eq.2)return
      endif
      ipckvew=IVALV(1)  ! remember the current view
      if(ipckvew.eq.indxvew+1)then

C Ask for name of new viewpoint or vp file and set eye point at centre.
        addview = .true.
        if(intext.eq.0)then
          rvpx=XMN
          rvpy=YMN
          rvpz=ZMX
        else
          rvpx=XMN+0.1  ! slightly inside room
          rvpy=YMN+0.1
          rvpz=VIEWM(3) ! at room centre
        endif

C Given view point and viewed point return and update common values
C of the azimuth and elevation.
        call ln2az(rvpx,rvpy,rvpz,VIEWM(1),VIEWM(2),VIEWM(3),
     &    azimuth,elevtn)
        CALL EASKS(trvewsh,'Name for view?',' ',10,'vew1',
     &    'short view tag',IER,nbhelp)
        call edisp(iuout,'Please define the new view point now.')

      elseif(ipckvew.eq.1)then

C If the default view for exterior (intext zero) or interior view.
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

C Given view point and viewed point return and update common values
C of the azimuth and elevation.
        call ln2az(rvpx,rvpy,rvpz,VIEWM(1),VIEWM(2),VIEWM(3),
     &    azimuth,elevtn)
        call AZ2UV(azimuth,elevtn,vdx,vdy,vdz)

      else

C An exiting view. Perhaps a referenced file.
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

C External view.
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

        elseif(word(1:4).eq.'-vtv')then

C An in-line view command within the rif file.
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
            CALL EGETWR(OUTSTR,K,vu,0.,0.,'-','skip vu',IER)
            CALL EGETWR(OUTSTR,K,vu,0.,0.,'-','skip vu',IER)
            CALL EGETWR(OUTSTR,K,vu,0.,0.,'-','skip vu',IER)
            IF(IFLAG.ne.0)goto 43
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

C External view.
            ITPPSW=3
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
          endif
        endif
      endif

C Reflect eyepoint in the perspective view.
      if(intext.eq.0)then
        ITPPSW=3
        ANG=angh*1.1
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
      if(ITPPSW.eq.3)then
        write(ITEM(8),'(A,F5.1,a,F5.1,a)')
     &                            'h view angle: ',angh,' (',eang,')'
      else
        write(ITEM(8),'(A,F5.1)') 'h hor view angle: ',angh
      endif
      write(ITEM(9),'(A,F5.1)')   'i vert view angle: ',angv
      ITEM(10) =                   '  __________________________  '
      if(ITPPSW.eq.0)then

C Plan view.
        write(ITEM(11),'(A,2F6.1)') 'l east min~max : ',XMN,XMX
        write(ITEM(12),'(A,2F6.1)') 'm north min~max: ',YMN,YMX
      elseif(ITPPSW.eq.1)then

C South elevation view.
        write(ITEM(11),'(A,2F6.1)') 'l south min~max: ',XMN,XMX
        write(ITEM(12),'(A,2F6.1)') 'm up    min~max: ',ZMN,ZMX
      elseif(ITPPSW.eq.2)then

C East elevation view.
        write(ITEM(11),'(A,2F6.1)') 'l east min~max : ',YMN,YMX
        write(ITEM(12),'(A,2F6.1)') 'm up    min~max: ',ZMN,ZMX
      elseif(ITPPSW.eq.3)then

C Perspective view.
        write(ITEM(11),'(A,2F6.1)') 'l east min~max : ',XMN,XMX
        write(ITEM(12),'(A,2F6.1)') 'm north min~max: ',YMN,YMX
      endif
      ITEM(13) =                  '  __________________________  '
      ITEM(14) =                  '> save view info              '
      ITEM(15) =                  '  __________________________  '
      ITEM(16) =                  '? help                        '
      ITEM(17) =                  '- exit                        '
      NITEMS = 17
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
        redraw =.true.
        MODLEN=.true.
        MODIFY=.TRUE.
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
        redraw =.true.
      elseif(INO.EQ.3)then
        CALL EASKABCD(' View choices: ',' ','plan',
     &    'south elevation','east elevation','perspective?',IW,nbhelp)
        if(IW-1.ne.ITPPSW)redraw =.true.
        ITPPSW=IW-1

      elseif(INO.EQ.5)THEN

C Eye Point...(assume begins at centre of zone).
        if(ITPPSW.eq.0)then
          CALL EASKABC(' Specify via: ',' ','editing',
     &     'pick on plan','ignore ?',IEE,nbhelp)
          if(IEE.eq.3)then
            goto 10

C Select point from plan or current perspective.
          elseif(IEE.eq.2)then
            call usrmsg(' ',' Select point via cursor...','-')
            igwid=igw
            igheight=igwh
            call axiscale(igwid,igheight,XMN,XMX,YMN,YMX,xsc,ysc,sca,
     &        Xadd,Yadd)
            call linescale(iigl,Xadd,sca,iigb,Yadd,sca)

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
            CALL ASKOK(outs,'Is this view ok?',OK,dok,nbhelp)
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
     &     'accept current eyepoint','ignore ?',IEE,nbhelp)
          if(IEE.eq.3)then
            goto 10
          elseif(IEE.eq.2)then
            write(outs,'(a,3F7.1)') 'The perspective eyepoint is ',
     &        EYEM(1),EYEM(2),EYEM(3)
            dok=.true.
            CALL ASKOK(outs,'Is this ok ?',OK,dok,nbhelp)
            if(OK)then
              rvpx=EYEM(1)
              rvpy=EYEM(2)
              rvpz=EYEM(3)
              call ln2az(EYEM(1),EYEM(2),EYEM(3),VIEWM(1),VIEWM(2),
     &          VIEWM(3),az,el)
              call AZ2UV(az,el,vdx,vdy,vdz)
              call ang3vtx(OXMN,OYMN,OZMN,EYEM(1),EYEM(2),EYEM(3),OXMX,
     &             OYMX,OZMX,eang)
              azimuth=az
              elevtn=el
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
          vewdone=.false.
          redraw =.true.
          iupdown=0
          isym=14
          call etplot(rvpx,rvpy,iupdown,isym)
          call forceflush()
          if(ITPPSW.eq.3)then
            dok=.false.
            helptopic='reflect_rad_view_in_per'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL ASKOK(' ','Reflect this in perspective view?',
     &        OK,dok,nbhelp)
            if(OK)then
              EYEM(1)=rvpx
              EYEM(2)=rvpy
              EYEM(3)=rvpz
              call ln2az(EYEM(1),EYEM(2),EYEM(3),VIEWM(1),VIEWM(2),
     &          VIEWM(3),az,el)
              azimuth=az
              elevtn=el
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
            dok=.true.
            CALL ASKOK(outs,' Is this view ok?',OK,dok,nbhelp)
            if(OK)then
              azimuth=az
              elevtn=el
              call  AZ2UV(azimuth,elevtn,vdx,vdy,vdz)
              call ang3vtx(OXMN,OYMN,OZMN,EYEM(1),EYEM(2),EYEM(3),OXMX,
     &             OYMX,OZMX,eang)
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
          helptopic='view_azimuth_edit'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKR(azimuth,' ',' View azimuth (deg) ? ',
     &      -359.,'W',359.,'W',90.,'view azimuth',IER,nbhelp)
          CALL EASKR(elevtn,' ',' View elevation (deg) ? ',
     &      -89.,'W',89.,'W',0.,'view elevation',IER,nbhelp)
          call AZ2UV(azimuth,elevtn,vdx,vdy,vdz)
          call ang3vtx(OXMN,OYMN,OZMN,EYEM(1),EYEM(2),EYEM(3),OXMX,
     &             OYMX,OZMX,eang)
          vewdone=.false.
          redraw =.true.
        endif

      elseif(INO.EQ.8.or.INO.EQ.9)then

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
          dok=.false.
          helptopic='reflect_rad_view_in_per'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL ASKOK(' ','Reflect this in perspective view?',
     &      OK,dok,nbhelp)
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

      elseif(INO.EQ.11)then

C horiz axis...
        helptopic='e2r_east_axis'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(ITPPSW.eq.0)then
  41      WRITE(HOLD,'(1x,2f8.3)')XMN,XMX
          CALL EASKS(HOLD,' East axis minimum and maximum: ',
     &      '  ',28,' ','east axis min-max',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,XMN,-300.,300.,'W','X min',IER)
          CALL EGETWR(HOLD,K,XMX,XMN,300.,'W','X max',IER)
          if(ier.ne.0)goto 41
          igwid=igw
          igheight=igwh
          call axiscale(igwid,igheight,XMN,XMX,YMN,YMX,xsc,ysc,sca,
     &       Xadd,Yadd)
          call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
        elseif(ITPPSW.eq.1)then
 141      WRITE(HOLD,'(1x,2f8.3)')XMN,XMX
          CALL EASKS(HOLD,' South axis minimum and maximum: ',
     &      '  ',28,' ','south axis min-max',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,XMN,-300.,300.,'W','X min',IER)
          CALL EGETWR(HOLD,K,XMX,XMN,300.,'W','X max',IER)
          if(ier.ne.0)goto 141
          igwid=igw
          igheight=igwh
          call axiscale(igwid,igheight,XMN,XMX,ZMN,ZMX,xsc,ysc,sca,
     &      Xadd,Yadd)
          call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
        elseif(ITPPSW.eq.2)then
 241      WRITE(HOLD,'(1x,2f8.3)')YMN,YMX
          CALL EASKS(HOLD,' East axis minimum and maximum: ',
     &      '  ',28,' ','east axis min-max',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,YMN,-300.,300.,'W','Y min',IER)
          CALL EGETWR(HOLD,K,YMX,YMN,300.,'W','Y max',IER)
          if(ier.ne.0)goto 241
          igwid=igw
          igheight=igwh
          call axiscale(igwid,igheight,YMN,YMX,ZMN,ZMX,xsc,ysc,sca,
     &      Xadd,Yadd)
          call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
        elseif(ITPPSW.eq.3)then
        endif
        vewdone=.false.
        redraw =.true.

      elseif(INO.EQ.12)then

C vertical axis...
        helptopic='e2r_north_axis'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(ITPPSW.eq.0)then
 144      WRITE(HOLD,'(1x,2f8.3)')YMN,YMX
          CALL EASKS(HOLD,' North axis minimum and maximum: ',
     &      '  ',28,' ','north axis min-max',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,YMN,-300.,300.,'W','Y min',IER)
          CALL EGETWR(HOLD,K,YMX,YMN,300.,'W','Y max',IER)
          if(ier.ne.0)goto 144
          igwid=igw
          igheight=igwh
          call axiscale(igwid,igheight,XMN,XMX,YMN,YMX,xsc,ysc,sca,
     &      Xadd,Yadd)
          call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
        elseif(ITPPSW.eq.1)then
 145      WRITE(HOLD,'(1x,2f8.3)')ZMN,ZMX
          CALL EASKS(HOLD,' Up axis minimum and maximum: ',
     &      '  ',28,' ','up axis min-max',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,ZMN,-300.,300.,'W','Y min',IER)
          CALL EGETWR(HOLD,K,ZMX,ZMN,300.,'W','Y max',IER)
          if(ier.ne.0)goto 145
          igwid=igw
          igheight=igwh
          call axiscale(igwid,igheight,XMN,XMX,ZMN,ZMX,xsc,ysc,sca,
     &      Xadd,Yadd)
          call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
        elseif(ITPPSW.eq.2)then
 245      WRITE(HOLD,'(1x,2f8.3)')ZMN,ZMX
          CALL EASKS(HOLD,' Up axis minimum and maximum: ',
     &      '  ',28,' ','up axis min-max',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,ZMN,-300.,300.,'W','Y min',IER)
          CALL EGETWR(HOLD,K,ZMX,ZMN,300.,'W','Y max',IER)
          if(ier.ne.0)goto 245
          igwid=igw
          igheight=igwh
          call axiscale(igwid,igheight,YMN,YMX,ZMN,ZMX,xsc,ysc,sca,
     &      Xadd,Yadd)
          call linescale(iigl,Xadd,sca,iigb,Yadd,sca)
        elseif(ITPPSW.eq.3)then
        endif
        vewdone=.false.
        redraw =.true.

      elseif(INO.EQ.14)then

C If an existing view then just update, if a new view then
C extend the list and the view command.
        if(addview)then
          indxvew = indxvew +1
          addview=.false.  ! turn off the toggle
          rvewsh(indxvew) = trvewsh  ! remember new name
        endif
        write(doit,'(a,3F9.3,a,3F8.4,a,F6.1,a,F6.1,a)') 
     &     '-vtv -vp ',rvpx,rvpy,rvpz,' -vd ',vdx,vdy,vdz,
     &     ' -vu 0 0 1 -vh ',angh,' -vv ',angv,' -vs 0 -vl 0 '
        if(addview)then
          WRITE(vewcmds(indxvew),'(a)') doit  ! new cmd
        else
          WRITE(vewcmds(ipckvew),'(a)') doit  ! current cmd
        endif
        IFC=3
        do 66 irif=1,NBSRIF(ifocz,ISCENE)
          call MKRIF(ITA2,rifname(ifocz,iscene,irif),IFC,0,IER)
c        if (NBSRIF(ISCENE).eq.1) then
c          call mkrif(ita2,LBSRIF(ISCENE),IFC,1,ier)
c        endif
 66     continue
        vewdone=.true.

      elseif(INO.EQ.(NITEMS-1))then

c Explains main menu.
        helptopic='view_definition_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('view menu',nbhelp,'-',0,0,IER)
      elseif(INO.EQ.NITEMS)then
        if(vewdone)then
          return
        else
          dok=.false.
          helptopic='e2r_remind_to_save'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL ASKOK('View information has not been saved',
     &      'Is it ok to exit?',OK,dok,nbhelp)
          IF(OK)return
        endif
      endif
      goto 10

      end
