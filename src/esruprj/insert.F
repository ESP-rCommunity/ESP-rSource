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

C insert.F contains:
C  ADDSUR : add, delete, copy a surface within a zone.
C  ADDVERT: add or delete a vertex within a zone.
C  CKVERT : checks vertex lists & returns if zone is fully bounded.
C  INSREC : add rect surface within or at base of surface.

C ************* ADDSUR 
C ADDSUR: Add / delete / copy a surface within a zone.
C Passed character ACTION to signal deletion 'D', addition 'A', copy 'C'.
C It is easy to add a surface at the end of the list. 
C Method checks for minimally linked vertices.  MODE
C passed `V` is verbose and asks for confirmation, `A`
C is automatic and proceeds with minimal interaction
C as in deletion of an entire zone.
C ISCOPT signals how to treat the copy of a surface as follows:
C   iscopt = 0 offer editing box and option to click on mouse for vertices
C   iscopt = 1 use the mouse click directly to specify vertices
C   iscopt = 2 offer copy of local (in the same zone) surfaces 
      SUBROUTINE ADDSUR(ITRC,ICOMP,IWHICH,ACTION,MODE,ISCOPT,IER)
#include "building.h"
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/precz/zname(MCOM),zdesc(MCOM)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G2/NGL(MS)
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/G7/SSNA(MCON),SSPAZI(MCON),SSPELV(MCON),SSPERIM(MCON),
     &          SSUREQN(MCON,4),SSURCOG(MCON,3),SSURVN(MCON,3)
      common/G8/nbedgdup(MCON),iedgdup(MCON,MV),nbedgshr(MCON),
     &          iedgshr(MCON,MV),imatshr(MCON,MV)
      COMMON/C1/NCOMP,NCON
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/PREC17/ZBASEA(MCOM),IBASES(MCOM,6),IUZBASEA(MCOM)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/DRWMAT/TEMAT(4,4), TSMAT(4,4),
     &              ETMMAT(4,4), ETSMAT(4,4), TBOX(3,2)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      LOGICAL MODIFY,MODLEN,MODBND,closex,closey,closez
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL     OK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      logical same,clkok,found,DOK,DDOK,nameok,matchver

      dimension X1(MV),Y1(MV),Z1(MV),jvn1(MV),XT(MV),YT(MV),ZT(MV)
      DIMENSION COG1(3),COG2(3),COG3(3),JJVN(MV),ispk(MS)

      CHARACTER SNAME*12,SMLCN*12,SVFC*4,SOTF*4,SOTHER*15
      CHARACTER SNAME1*12,SMLCN1*12,SVFC1*4,SOTF1*4,SNAMED*12
      character SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12
      character SSPARENT*12
      CHARACTER H*72,ACTION*1,MODE*1,outs*124,SN*12,SN2*12
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      character ZSDES*28,zname*12,zdesc*64,T14*14,D14*14
      character HOLD*72,hold64*64,holds*36,t248*248
    
      IER=0
      DDOK=.false.
      vdis=0.0
      valx=0.0
      valy=0.0
      valz=0.0
      ang=0.0
      IOK=0
      IRV=1

      IF(ACTION.EQ.'D')THEN

C Unlink any other reference pointing to zone icomp and surface iwhich.
C While looping remember the connection associated with zone:surf.
        do 35 IXU = 1,NCON
          if(IC1(IXU).eq.ICOMP.AND.IE1(IXU).eq.IWHICH)ICON=IXU
          if(ICT(IXU).EQ.3)then
            if(IC2(IXU).eq.ICOMP.and.IE2(IXU).EQ.IWHICH)THEN
              ICT(IXU)=0
              IC2(IXU)=0
              IE2(IXU)=0
              SSOTHER(IXU)='EXTERIOR'
            endif
          endif
   35   continue

C Any other connection which references a surface in focus zone
C and which is about to be moved down should be adjusted.
        do 36 IXU = 1,NCON
          if(ICT(IXU).EQ.3)then
            if(IC2(IXU).eq.ICOMP.and.IE2(IXU).gt.IWHICH)THEN
              IE2(IXU)=IE2(IXU)-1
            endif
          endif
   36   continue

C Compact the list:  If within the zone where the surface was removed, 
C update IE1 for surfaces > IWHICH otherwise just compact IC1 & IE1.
        DO 235 IX=ICON,NCON-1
          IF(IC1(IX+1).EQ.ICOMP)THEN
            IF(IE1(IX+1).GT.IWHICH)IE1(IX)=IE1(IX+1)-1
          ELSEIF(IC1(IX+1).gt.ICOMP)THEN
            IC1(IX)=IC1(IX+1)
            IE1(IX)=IE1(IX+1)
          ENDIF
          ICT(IX)=ICT(IX+1)
          IC2(IX)=IC2(IX+1)
          IE2(IX)=IE2(IX+1)
          SSNAME(ix)=SSNAME(IX+1)
          SSOTF(IX)=SSOTF(IX+1)
          SSMLCN(IX)=SSMLCN(IX+1)
          SSVFC(IX)=SSVFC(IX+1)
          SSOTHER(IX)=SSOTHER(IX+1)
          SSNA(IX)=SSNA(IX+1)
          SSPAZI(IX)=SSPAZI(IX+1)
          SSPELV(IX)=SSPELV(IX+1)
          SSPERIM(IX)=SSPERIM(IX+1)
          SSUREQN(IX,1)=SSUREQN(IX+1,1)
          SSUREQN(IX,2)=SSUREQN(IX+1,2)
          SSUREQN(IX,3)=SSUREQN(IX+1,3)
          SSUREQN(IX,4)=SSUREQN(IX+1,4)
          SSURCOG(IX,1)=SSURCOG(IX+1,1)
          SSURCOG(IX,2)=SSURCOG(IX+1,2)
          SSURCOG(IX,3)=SSURCOG(IX+1,3)
          SSURVN(IX,1)=SSURVN(IX+1,1)
          SSURVN(IX,2)=SSURVN(IX+1,2)
          SSURVN(IX,3)=SSURVN(IX+1,3)
          nbedgdup(IX)=nbedgdup(IX+1)
          nbedgshr(IX)=nbedgshr(IX+1)
          do 42 ijj=1,MV
            iedgdup(IX,ijj)=iedgdup(IX+1,ijj)
            iedgshr(IX,ijj)=iedgshr(IX+1,ijj)
            imatshr(IX,ijj)=imatshr(IX+1,ijj)
  42      continue
  235   CONTINUE
        NCON=NCON-1

C Re-establish link between zone/surfaces and connections.
        do 335 iccc = 1, NCON
          IZSTOCN(IC1(iccc),IE1(iccc))=iccc
 335    continue

C  Since this routine may be used to completly remove a zone, trap
C if small number of surfaces.
        if(NSUR.eq.1)goto 65

C Check if one of the ibases list points to iwhich, if so shift
C later ibases items down one.
        is=iwhich
        if(is.eq.ibases(icomp,1))then
          ibases(icomp,1)=ibases(icomp,2)
          ibases(icomp,2)=ibases(icomp,3)
          ibases(icomp,3)=ibases(icomp,4)
          ibases(icomp,4)=ibases(icomp,5)
          ibases(icomp,5)=ibases(icomp,6)
        elseif(is.eq.ibases(icomp,2))then
          ibases(icomp,2)=ibases(icomp,3)
          ibases(icomp,3)=ibases(icomp,4)
          ibases(icomp,4)=ibases(icomp,5)
          ibases(icomp,5)=ibases(icomp,6)
        elseif(is.eq.ibases(icomp,3))then
          ibases(icomp,3)=ibases(icomp,4)
          ibases(icomp,4)=ibases(icomp,5)
          ibases(icomp,5)=ibases(icomp,6)
        elseif(is.eq.ibases(icomp,4))then
          ibases(icomp,4)=ibases(icomp,5)
          ibases(icomp,5)=ibases(icomp,6)
        elseif(is.eq.ibases(icomp,5))then
          ibases(icomp,5)=ibases(icomp,6)
        elseif(is.eq.ibases(icomp,6))then
          ibases(icomp,6)=0
        endif

C Loop through each of the surfaces greater than IWHICH and see if
C they are included in ibases list. If so decrement ibases value.
        DO 68 IS=IWHICH,NSUR-1
          if(is.eq.ibases(icomp,1)) ibases(icomp,1)=ibases(icomp,1)-1
          if(is.eq.ibases(icomp,2)) ibases(icomp,2)=ibases(icomp,2)-1
          if(is.eq.ibases(icomp,3)) ibases(icomp,3)=ibases(icomp,3)-1
          if(is.eq.ibases(icomp,4)) ibases(icomp,4)=ibases(icomp,4)-1
          if(is.eq.ibases(icomp,5)) ibases(icomp,5)=ibases(icomp,5)-1
          if(is.eq.ibases(icomp,6)) ibases(icomp,6)=ibases(icomp,6)-1
  68    continue

C Loop through each of the surfaces greater than IWHICH and copy their
C contents into the surface below. Use subsequent file write/read
C to clear up window commons.  Since this routine may be used to
C completly remove a zone, trap if small number of surfaces.
        if(IWHICH.eq.NSUR)then
          SNAME(ICOMP,IWHICH)=' '
          goto 65
        endif
        DO 66 IS=IWHICH,NSUR-1
          SNAME(ICOMP,IS)=SNAME(ICOMP,IS+1)
          SOTF(IS)=SOTF(IS+1)
          SMLCN(IS)=SMLCN(IS+1)
          SVFC(IS)=SVFC(IS+1)
          SOTHER(IS)=SOTHER(IS+1)
          NVER(IS)=NVER(IS+1)
          DO 67 IV=1,NVER(IS)
            JVN(IS,IV)=JVN(IS+1,IV)
   67     CONTINUE
          NGL(IS)=NGL(IS+1)
   66   CONTINUE
   65   NSUR=NSUR-1
        NZSUR(ICOMP)=NSUR
        NZTV(ICOMP)=NTV

C If in automatic mode 
        if(MODE.eq.'A')then
          MODIFY=.TRUE.
          return
        endif
      ELSEIF(ACTION.EQ.'C')THEN

C Copy an existing surface, remember how many can be copied
C from another zone. Initil transforms:
        IF(NSUR+1.GT.MS)THEN
          CALL USRMSG(' No more surfaces are allowed. ',' ','W')
          RETURN
        ENDIF
        iposbcp=MS-NSUR
        call tstamp('>','PRJ: copy surface')
        if(ISCOPT.eq.1)then  

C Find out which other zone to copy from. Save the current state of the
C menu so that can return to where we were. Also connected surface.
 99       CALL EPMENSV
          CALL EASKGEOF('Select source zone to copy from:',
     &      CFGOK,IZ,'-',IER)
          CALL EPMENRC
          if(iz.eq.icomp)goto 99
          if(iz.eq.0)return
          same=.false.
        elseif(ISCOPT.eq.2)then
          same=.true.
        elseif(ISCOPT.eq.3)then
          return
        endif

C Reset all surface lines in donor zone.
        if(.NOT.same)then
          call edisp(iuout,' updating current zone before copy...')
          CALL EMKGEO(IFIL+1,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          MODIFY=.TRUE.
          MODBND=.TRUE.
          CALL INLNST(1)
          itsnm=0
          nzg=1
          nznog(1)=IZ
          izgfoc=ICOMP
          CALL ADJVIEW(0,IER)
          MODIFY=.TRUE.
          MODBND=.TRUE.
        endif

C Use pickssur to find up to ilimit surfaces to copy, returning
C the selections via array ispk.
        CALL EDISP(iuout,'Please specify surface(s) to copy...')
        IS=1
        inpick=0
        if(same)then
          CALL EPMENSV
          ilimit=iposbcp
          call pickssur(icomp,inpick,'c',ispk,0,ilimit,ier)
          CALL EPMENRC
          if(inpick.eq.0)then
            return
          endif
        else
          CALL EPMENSV
          ilimit=iposbcp
          call pickssur(iz,inpick,'c',ispk,0,ilimit,ier)
          CALL EPMENRC
          if(inpick.eq.0)then
            return
          endif
        endif

C Read the other zone and put selected surface data into temp storage.
        if(.NOT.same)then
          CALL EGOMIN(IFIL+1,LGEOM(IZ),IZ,0,0,iuout,IER)
        endif
        npick=1
  191   continue
        is=ispk(npick)
        if(same)then
          write(outs,'(2a)') ' Copying attributes of ',SNAME(ICOMP,IS)
        else
          write(outs,'(2a)') ' Copying attributes of ',SNAME(IZ,IS)
        endif
        call usrmsg(outs,' ','-')
        NVER1=NVER(IS)
        DO 142 IV=1,NVER1
          X1(iv)=X(JVN(IS,IV))
          Y1(iv)=Y(JVN(IS,IV))
          Z1(iv)=Z(JVN(IS,IV))
  142   continue

C Generate a new surface name and default, then check to see if
C if is a duplicate. 
C << note: does not check name against the most recently copied
C << surface.
  141   if(same)then
          write(SNAMED,'(a,a)') 'x',SNAME(ICOMP,IS)(1:11)
          write(SNAME1,'(a)') SNAME(ICOMP,IS)(1:12)
        else
          write(SNAMED,'(a,a)') 'x',SNAME(IZ,IS)(1:11)
          write(SNAME1,'(a)') SNAME(IZ,IS)(1:12)
        endif
        call snamdup(SNAME1,icomp,0,nameok)
        if(.NOT.nameok)then

C Copy to a slightly longer buffer for editing and ensure that
C the final name uses the allowed character set.
          write(T14,'(2a)') SNAME1(1:12),'  '
          write(D14,'(2a)') SNAMED(1:12),'  '
          CALL EASKS(T14,'Revised surface name','(duplicate found)',
     &    14,D14,'surface name',IER,2)
          if(T14(1:2).NE.'  ')then
            write(SNAME1,'(a)') T14(1:12)
          else
            goto 141
          endif
        endif
        SOTF1=SOTF(IS)
        SMLCN1=SMLCN(IS)
        SVFC1=SVFC(IS)

C Now re-read focus zone and copy the data into normal common
C blocks. Because the surface to be added is not yet in the
C connections list find ICON future position via `one connection 
C past the current last surface in the zone`.
        if(.NOT.same)then
          CALL EGOMIN(IFIL+1,LGEOM(ICOMP),ICOMP,0,0,iuout,IER)
        endif

C Up the surface count.
        ICON=IZSTOCN(icomp,nsur)+1
        NSUR=NSUR+1
        NZSUR(ICOMP)=NSUR
        NZTV(ICOMP)=NTV
        NGL(NSUR)=0
        NVER(NSUR)=NVER1
        SNAME(ICOMP,NSUR)=SNAME1
        SOTF(NSUR)=SOTF1
        SMLCN(NSUR)=SMLCN1
        SVFC(NSUR)=SVFC1
        SOTHER(NSUR)='UNKNOWN'

C Generate combined name for dialog.
        ZSDES=' '
        WRITE(ZSDES,'(a,a1,a)') SNAME1(1:lnblnk(SNAME1)),':',
     &    ZNAME(icomp)(1:lnblnk(ZNAME(icomp)))

        H(1)='Actions: '
        H(2)=' Shift surface along (+=outwards) surface normal.'
        H(3)=' Transform vertices by X,Y,Z metres.'
        H(4)=' Rotate surface about one of its vertices.'
        H(5)=' Invert ordering (to swop front & back faces).'
        H(6)=' Do several operations to the surface.'
        H(7)=' Continue with no action.'
        if(.NOT.DDOK)then
          CALL EASKATOG(' Actions to take on the new surface :',' ',
     &      'shift along normal','transform xyz','rotate','invert',
     &      'combination','continue ?',' ',IRT,7)
        endif
        if(IRT.eq.5)then
          if(.NOT.DDOK)then

C Create string buffer for variables with space at the end.
            H(1)='Distance along the normal (+=out) in metres.'
            H(2)='Transform vertices by X,Y,Z metres.'
            H(3)='Rotate surface about one of its vertices (index).'
            H(4)='Rotation angle (degrees) '
            H(5)=' '
            H(6)='If you leave transform values and/or rotation as'
            H(7)='zero then that transform will have no effect. '
            hold64='                                                  '
            write(hold64,'(f9.4,2x,3f9.3,i4,f9.3,a)') vdis,valx,valy,
     &        valz,irv,ang,'   '
 552        CALL EASKS(hold64,
     &'Distance along normal, X Y & Z offsets, vertex, rotation angle:',
     &' ',64,' 0.0  0.0  0.0 0. 1  0.0  ','combined transforms',IER,7)
            if(ier.ne.0)goto 552
            K=0
            CALL EGETWR(HOLD64,K,Vdis,-9.0,9.0,'W','normal distance',
     &        IER)
            CALL EGETWR(HOLD64,K,VALX,-50.0,50.0,'W','X off',IER)
            CALL EGETWR(HOLD64,K,VALY,-50.0,50.0,'W','Y off',IER)
            CALL EGETWR(HOLD64,K,VALZ,-50.0,50.0,'W','Z off',IER)
            CALL EGETWI(HOLD64,K,irv,0,nver1,'W','vertex index',IER)
            CALL EGETWR(HOLD64,K,ang,-359.0,359.0,'W','rotation ang',
     &        IER)
          endif

C Do each of the transforms in order.
          CALL TRANSUR(ITRC,iuout,NVER1,X1,Y1,Z1,vdis,XT,YT,ZT,ZSDES)
          do 551 nt1=1,NVER1
            X1(nt1)=XT(nt1)
            Y1(nt1)=YT(nt1)
            Z1(nt1)=ZT(nt1)
 551      continue

          DO 553 I=1,NVER1
            X1(I)=X1(I)+VALX
            Y1(I)=Y1(I)+VALY
            Z1(I)=Z1(I)+VALZ
 553      continue

          if(ANG.LT.-.01.OR.ANG.GT..01)then
            PI = 4.0 * ATAN(1.0)
            A=-ANG*PI/180.
            CA=COS(A)
            SA=SIN(A)
            XX1=X1(IRV)
            YY1=Y1(IRV)
            DO 500 I=1,NVER1
              XXX=X1(I)-XX1
              YYY=Y1(I)-YY1
              XR=XXX*CA+YYY*SA
              YR=YYY*CA-XXX*SA
              X1(I)=XR+XX1
              Y1(I)=YR+YY1
  500       continue
          endif
        endif

C User selected only one transform to make. Process it.
        if(IRT.eq.1)then

C Shift along normal and copy transformed points back to X1,Y1,Z1 array.
          H(1)='Distance along the normal (+=out) in metres.'
          if(.NOT.DDOK)then
            vdis=0.0
            CALL EASKR(vdis,' Distance along the normal (metres): ',
     &        ' : ',-9.999,'F',9.999,'W',0.0,'d along normal',IER,1)
          endif
          CALL TRANSUR(ITRC,iuout,NVER1,X1,Y1,Z1,vdis,XT,YT,ZT,ZSDES)
          do 151 nt1=1,NVER1
            X1(nt1)=XT(nt1)
            Y1(nt1)=YT(nt1)
            Z1(nt1)=ZT(nt1)
 151      continue
        elseif(IRT.eq.2)then

C Transform all surface vertices.
          if(.NOT.DDOK)then
            HOLDS= '  0.0   0.0   0.0    '
 152        CALL EASKS(HOLDS,' X Y & Z offsets: ',' ',
     &        36,'  0.0   0.0   0.0   ','offsets',IER,9)
            K=0
            CALL EGETWR(HOLDS,K,VALX,-50.0,50.0,'W','X off',IER)
            CALL EGETWR(HOLDS,K,VALY,-50.0,50.0,'W','Y off',IER)
            CALL EGETWR(HOLDS,K,VALZ,-50.0,50.0,'W','Z off',IER)
            if(ier.ne.0)goto 152
          endif
          DO 153 I=1,NVER1
            X1(I)=X1(I)+VALX
            Y1(I)=Y1(I)+VALY
            Z1(I)=Z1(I)+VALZ
 153      continue
        elseif(IRT.eq.3)then

C Rotate around a point in the surface.
          H(1)='The rotation is applied around one vertex of'
          H(2)='the surface. The index is to the 1st, 2nd, 3rd,'
          H(3)='vertex in the surface. '
          H(4)=' '
          H(5)='The rotation is applied around at and positive'
          H(6)='is anticlockwise. '
          H(7)=' '
          H(8)='A cancel leaves skips the rotation step. '
          if(.NOT.DDOK)then
            CALL EASKI(IRV,'Rotate about which vertex index: ',' ',
     &        1,'F',NVER1,'F',1,'rotation anchor',IERI,8)
            if(ieri.eq.-3)then
              goto 166
            endif
            ANG=0.
            CALL EASKR(ANG,' ',' Rotation (deg + = anticlockwise) ? ',
     &       -359.0,'W',359.0,'W',0.0,'rotation',IER,8)
          endif
          if(ANG.LT.-.01.OR.ANG.GT..01)then
            PI = 4.0 * ATAN(1.0)
            A=-ANG*PI/180.
            CA=COS(A)
            SA=SIN(A)
            XX1=X1(IRV)
            YY1=Y1(IRV)
            DO 10 I=1,NVER1
              XXX=X1(I)-XX1
              YYY=Y1(I)-YY1
              XR=XXX*CA+YYY*SA
              YR=YYY*CA-XXX*SA
              X1(I)=XR+XX1
              Y1(I)=YR+YY1
   10       continue
          endif
        endif

C After transforms, append new surface vertices to zone list if
C they are unique, otherwise link the new surface to an existing vetex.
  166   continue
        do 143 ix = 1,NVER1
          if(NTV+1.le.MTV)then
            found=.false.
            do 144 ixx = 1,NTV
              CALL ECLOSE(X1(ix),X(ixx),0.004,closex)
              CALL ECLOSE(Y1(ix),Y(ixx),0.004,closey)
              CALL ECLOSE(Z1(ix),Z(ixx),0.004,closez)
              if(closex.and.closey.and.closez)then
                found=.true.
                match=ixx
                goto 145
              endif
  144       continue

C If an existing vertex is close then reference it otherwise
C add a new vertex to the end of the zone list.
  145       if(found)then
              JVN(NSUR,ix)=match
            else
              NTV=NTV+1
              NZTV(icomp)=NTV
              JVN(NSUR,ix)=NTV
              X(NTV)=X1(ix)
              Y(NTV)=Y1(ix)
              Z(NTV)=Z1(ix)
              XMN=AMIN1(XMN,X(NTV))
              YMN=AMIN1(YMN,Y(NTV))
              ZMN=AMIN1(ZMN,Z(NTV))
              XMX=AMAX1(XMX,X(NTV))
              YMX=AMAX1(YMX,Y(NTV))
              ZMX=AMAX1(ZMX,Z(NTV))

C For a unique vertex also test to see if it is close (+-0.002m) to
C one of the existing lines. nsur-1 because the new surface does
C not need to be checked. Also check the last edge in the surface.
              iwhich3=NTV
              do 246 ivj=1,NSUR-1
                ivjlimit=NVER(ivj)
                do 247 ivjj=1,ivjlimit
                  if(ivjj.eq.ivjlimit)then
                    iwhich1=JVN(ivj,ivjj)
                    iwhich2=JVN(ivj,1)
                  else
                    iwhich1=JVN(ivj,ivjj)
                    iwhich2=JVN(ivj,ivjj+1)
                  endif

C Report length of line. Use method of Ward/Radiance in fvect.c
                  call pointtoline(iwhich3,iwhich1,iwhich2,offset,
     &              matchver)
                  if(.NOT.matchver) goto 247
                  if(offset.lt.0.004)then
                    write(outs,'(a,i2,a,3f8.3,a,f6.4,a,i3,a,i3,2a)')
     &               'New vertex ',iwhich3,' @',
     &                X(NTV),Y(NTV),Z(NTV),' is close (',offset,
     &                ') to edge ',IWHICH1,' &',IWHICH2,' of surface ',
     &                SNAME(ICOMP,ivj)
                    call edisp(iuout,outs)

C If current surface (ivj) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JVN indices up one until at the current edge (ivjj) and then
C inserting the new vertex index.
                    if(NVER(ivj)+1.le.MV)then
                      if(offset.le.0.004)then
                        ok=.true.
                      else
                        dok=.true.
                        h(1)='Vertex is close to an existing edge. It'
                        h(2)='probably should also be included in the'
                        h(3)='list of the adjacent surface (so that'
                        h(4)='the edge topology is consistent). '
                        call askok(
     &                  'Insert this vertex into the adjacent surface?',
     &                  '(see help)',ok,dok,4)
                      endif
                    else
                      ok=.false.
                    endif
                    if(ok)then
                      NVER(ivj)=NVER(ivj)+1
                      IXV=NVER(ivj)+1
  148                 continue
                      IXV=IXV-1
                      JVN(ivj,IXV)=JVN(ivj,IXV-1)
                      IF(IXV.GT.ivjj+1)GOTO 148
                      JVN(ivj,ivjj+1)=iwhich3

C Debug...
C                      write(6,'(a,30i3)') 'now jvn is ',
C     &                  (JVN(ivj,ii),ii=1,NVER(ivj))

C Surface (ivj) vertex list has been updated. Go on to next surface.
                      goto 246 
                    endif
                  endif
  247           continue
  246         continue
            endif
          endif
  143   continue

C Reverse the ordering.
        if(IRT.eq.4.or.IRT.eq.5.or.IRT.eq.6)then
          H(1)='You can swop the front:back faces of a surface. This'
          H(2)='is necessary for partitions copied from another '
          H(3)='zone or where you are creating back-to-back surfaces'
          H(4)='to represent internal mass. '
          write(outs,'(3a)') ' surface ',SNAME1(1:lnblnk(SNAME1)),
     &                       ' (yes for most partitions) ?'
          if(.NOT.DDOK)then
            if(IRT.eq.4)then
              IOK=1
            else
              CALL EASKAB(' Reverse the edge ordering of',
     &                  outs,'yes','no',IOK,4)
            endif
          endif
          if(IOK.eq.1)then
            do 146, iyy = 1,NVER(NSUR)
              jvn1(iyy)=JVN(NSUR,iyy)
  146       CONTINUE
            JVN(NSUR,1)=jvn1(2)
            JVN(NSUR,2)=jvn1(1)
            do 147, iyy = 3,NVER(NSUR)
              izz=NVER(NSUR)+3-iyy
              JVN(NSUR,iyy)=jvn1(izz)
  147       CONTINUE
          endif
        endif

C Update the connection list. Move all others up and then insert with
C default values.
        IF(NCON.LT.MCON)THEN
          NCON=NCON+1
          IX=NCON+1

C Update common blocks G6 & G7
          call adjcbg6g7('+',ix,icon)

          IC1(ICON)=ICOMP
          IE1(ICON)=NSUR
          ICT(ICON)=0
          IC2(ICON)=0
          IE2(ICON)=0
          IZSTOCN(icomp,nsur)=icon
          SSNAME(ICON)=SNAME(ICOMP,NSUR)
          SSOTF(ICON)=SOTF(NSUR)
          SSMLCN(ICON)=SMLCN(NSUR)
          SSVFC(ICON)=SVFC(NSUR)
          SSOTHER(ICON)=SOTHER(NSUR)

C Now update the G7 common block to reflect additional surface.
          call zgupdate(1,icomp,ier)
        ENDIF

C If more than one surface to copy save the updated zone before
C copying the next surface.
        if(npick.lt.inpick)then
          npick=npick+1
          if(.NOT.same)then
            call edisp(iuout,' updating current zone...')
            CALL EMKGEO(IFIL+1,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            call edisp(iuout,' getting donor zone info...')
            CALL EGOMIN(IFIL+1,LGEOM(IZ),IZ,0,0,iuout,IER)
            write(outs,'(3a)') ' Repeat transforms for ',
     &        SNAME(IZ,ispk(npick)),' ?'
          else
            call edisp(iuout,' updating current zone...')
            CALL EMKGEO(IFIL+1,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            write(outs,'(3a)') ' Repeat transforms for ',
     &       SNAME(ICOMP,ispk(npick)),' ?'
          endif
          if(inpick.gt.1)then
            dok=.true.
            h(1)='Often the transforms that are required for one'
            h(2)='surface should also be applied to the other'
            h(3)='surfaces that you copied.  '
            h(4)=' '
            h(5)='Double check the zone after you have finished'
            h(6)='copying the surfaces to ensure they are located'
            h(7)='correctly and face the correct direction. '
            CALL ASKOK(outs,'(see help)',DDOK,dok,7)
          endif
          goto 191
        endif
        MODBND=.TRUE.
        MODIFY=.TRUE.
      ELSEIF(ACTION.EQ.'A')THEN

C Create a new surface, set as many defaults as possible. Because
C the surface to be added is not yet in the connections list find 
C ICON future position via `one connection past the current last
C surface in the zone`.
        IF(NSUR+1.GT.MS)THEN
          CALL USRMSG(' No more surfaces are allowed. ',' ','W')
          RETURN
        ENDIF
        call tstamp('>','PRJ: add surface')
        ICON=IZSTOCN(icomp,nsur)+1
        NSUR=NSUR+1
        NZSUR(ICOMP)=NSUR
        NZTV(ICOMP)=NTV
        NGL(NSUR)=0

C Allow user to type in the list of vertices as a string.  The
C number of items is the number of vertices and then parse out
C the individual vertex numbers from the list.
   41   HOLD='                                                     '
        H(1)='The number of vertices must be less than 24. '
        H(2)='A surface is composed of a list of vertices. Type'
        H(3)='this list in order (anticlockwise viewed from the'
        H(4)='outside).  Note: the last line of the polygon is'
        H(5)='from the last vertex to the start vertex. '
        h(6)=' '
        h(7)='Or click mouse on existing vertices, ending with an '
        h(8)='` e `. A match is assumed if within 5 pixels.'
        h(9)='You will be given an oportunity to confirm the list. '
        if(MMOD.ne.8)then
          HOLD='                                                  '
          CALL EASKS(HOLD,
     &     ' Give vertices (order anticlockwise viewed from outside)',
     &     '  ',72,' ','associated vertices',IER,5)
          CALL CHITMS(HOLD,NV)
          K=0
          DO 94 J=1,NV
            CALL EGETWI(HOLD,K,JV,1,NTV,'W','vertex list',IER)
            JVN(NSUR,J)=JV
   94     CONTINUE
          NVER(NSUR)=NV
        else

C If user did not explicitly specify that the mouse should be used
C present editing of vertices with the option for mouse.
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          if(iglib.eq.1)then
            if(ISCOPT.eq.0)then  
              hold64='                                                '
              write(t248,'(4a)') hold64,hold64,hold64,'               '
              WRITE(t248,'(24I4)')(JJVN(J),J=1,ijvn)
              CALL EASKS248(t248,' Associated vertices (type indices)?',
     &        ' ',72,' 1  2  3  4  ','associated vertices',IER,9)
              CALL CHITMS(t248,NV)
              K=0
              DO 943 J=1,NV
                CALL EGETWI(t248,K,JV,1,NTV,'W','vertex list',IER)
                JVN(NSUR,J)=JV
  943         CONTINUE
              NVER(NSUR)=NV
            elseif(ISCOPT.eq.1)then

C User has specified clicking from mouse so just do that.
              clkok=.true.
            endif
            if(clkok)then
              ijvn=0
              call edisp(iuout,
     &          'Select points via cursor...type `e` to finish.')
  46          CALL trackview(ii,ixx,iyy)
              if(ii.eq.69.or.ii.eq.101)goto 47
              found=.false.
              do 45 i=1,NTV
                COG1(1)=X(I)
                COG1(2)=Y(I)
                COG1(3)=Z(I)
                CALL VECTRN(COG1,TEMAT,COG2,IER)
                CALL VECTRN(COG2,ETSMAT,COG3,IER)
                call u2pixel(COG3(1),COG3(2),ix,iy)
                ixd=ix-ixx
                iyd=iy-iyy
                if(abs(ixd).lt.5.and.abs(iyd).lt.5)then
                  if(found)then
                    call edisp(iuout,'Close points...try again.')
                    goto 46
                  endif
                  WRITE(outs,'(a,i5,a,i5,a,i3)')' The point @ x=',ixx,
     &                                ' & y=',iyy,' matches vertex ',i
                  call edisp(iuout,outs)
                  found=.true.
                  ijvn=ijvn+1
                  jjvn(ijvn)=i
                  goto 46
                endif
  45          continue
              if(.NOT.found)goto 46
  47          if(ijvn.lt.3)then
                call edisp(iuout,'Not enough points to make a surface.')
                 goto 41
              endif
              hold64='                                                '
              write(t248,'(4a)') hold64,hold64,hold64,'               '
              WRITE(t248,'(24I4)')(JJVN(J),J=1,ijvn)
              CALL EASKS248(t248,' Associated vertices (type indices)?',
     &        ' ',72,' 1  2  3  4  ','associated vertices',IER,9)
              CALL CHITMS(t248,NV)
              K=0
              DO 945 J=1,NV
                CALL EGETWI(t248,K,JV,1,NTV,'W','vertex list',IER)
                JVN(NSUR,J)=JV
  945         CONTINUE
              NVER(NSUR)=NV
            endif
          elseif(iglib.eq.2)then
         
C For the GTK version there is no mouse click so offer a long blank editing
C string. Fill the string first with spaces (MSYS should like this approach
            hold64='                                                  '
            write(t248,'(4a)') hold64,hold64,hold64,'                 '
            CALL EASKS248(t248,'Associated vertices (type indices)?',
     &        ' ',72,' 1  2  3  4  ','associated vertices',IER,8)
            CALL CHITMS(t248,NV)
            K=0
            DO 944 J=1,NV
              CALL EGETWI(t248,K,JV,1,NTV,'W','vertex list',IER)
              JVN(NSUR,J)=JV
  944       CONTINUE
            NVER(NSUR)=NV
          endif
        endif

C Fill default attributes.
        CALL FILSUR(iuout,ICOMP,NSUR)
        T14=' '
        write(T14,'(2a)') SNAME(ICOMP,NSUR),'  '
 52     CALL EASKS(T14,'Surface name','(unique word <=12char):',
     &      14,'new_surf','surface name',IER,0)
        write(SN,'(a)') T14(1:12)

        call st2name(SN,SN2)
        call snamdup(SN2,icomp,NSUR,nameok)
        if(nameok)then
          SNAME(ICOMP,NSUR)=SN2
        else
          call edisp(iuout,'Surface name is a duplicate of an existing')
          call edisp(iuout,'surface. Please supply a different name.')
          goto 52
        endif

C Update the connection list. Move all others up and then insert with
C default values.
        IF(NCON.LT.MCON)THEN
          NCON=NCON+1
          IX=NCON+1

C Update common blocks G6 & G7
          call adjcbg6g7('+',ix,icon)

          IC1(ICON)=ICOMP
          IE1(ICON)=NSUR
          ICT(ICON)=0
          IC2(ICON)=0
          IE2(ICON)=0
          IZSTOCN(icomp,nsur)=icon
          SSNAME(ICON)=SNAME(ICOMP,NSUR)
          SSOTF(ICON)=SOTF(NSUR)
          SSMLCN(ICON)=SMLCN(NSUR)
          SSVFC(ICON)=SVFC(NSUR)
          SSOTHER(ICON)=SOTHER(NSUR)

C Now update the G7 common block to reflect new surface.
          call zgupdate(2,icomp,ier)
        ENDIF
        MODIFY=.TRUE.
      endif

      RETURN
      END

C ************* ADDVERT 
C ADDVERT: Add,  delete, copy a vertex within a zone.
C Passed character ACTION to signal deletion or addition. NUM will
C be either the vertex to delete or the number of vertices to
C added at the end of the list.
      SUBROUTINE ADDVERT(icomp,NUM,ACTION,IER)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      LOGICAL MODIFY,MODLEN,MODBND,check,found,closex,closey,closez
      logical ok,dok,matchver

      DIMENSION ISASSO(MS),X1(MV),Y1(MV),Z1(MV)
      CHARACTER H*72,OUTSTR*124,ACTION*1,HOLD*32,outs*124
      CHARACTER SOTHER*15,SMLCN*12,SVFC*4,SOTF*4,SNAME*12

      check = .false.
      if(NUM.eq.0)then
        call usrmsg('Sorry, there is no vertex 0 in the list...',
     &    ' ','W')
        return
      endif
      if(ACTION.eq.'D'.or.ACTION.eq.'d')then

C Loop through the surfaces and see if this vertex is mentioned.
        IWHICH=NUM
        IF(NSUR.GT.0)THEN
          IHIT=0
          DO 8792 IS=1,NSUR
            DO 8794 IV=1,NVER(IS)
              IF(IWHICH.EQ.JVN(IS,IV))THEN
                IHIT=IHIT+1
                ISASSO(IHIT)=IS
              ENDIF
 8794       CONTINUE
 8792     CONTINUE
          IF(IHIT.GT.0)THEN
            WRITE(OUTSTR,9993)IWHICH,X(IWHICH),Y(IWHICH),Z(IWHICH)
 9993       FORMAT(' Vertex ',I2,' located at ',3F10.3)
            CALL EDISP(iuout,OUTSTR)
            CALL EDISP(iuout,' is associated with surfaces: ')
            WRITE(OUTSTR,9983)(ISASSO(IH),IH=1,IHIT)
 9983       FORMAT(' ',10(I2,', '))
            CALL EDISP(iuout,OUTSTR)
          ELSE

C Vertex is free of connections so loop through the vertices in each
C surface and any JNV larger than IWHICH should be decremented by one
            DO 792 IS=1,NSUR
              DO 794 IV=1,NVER(IS)
                IF(JVN(IS,IV).GT.IWHICH)THEN
                  JVN(IS,IV)=JVN(IS,IV)-1
                ENDIF
  794         CONTINUE
  792       CONTINUE

C Now loop through the vertices and copy the contents of vertices >
C IWHICH into the next lower one.
            DO 791 IVV=IWHICH,NTV-1
              X(IVV)=X(IVV+1)
              Y(IVV)=Y(IVV+1)
              Z(IVV)=Z(IVV+1)
  791       CONTINUE

C Finally decrement NTV.
            NTV=NTV-1
            NZTV(icomp)=NTV
          ENDIF
        ENDIF
        MODIFY=.TRUE.

      elseif(ACTION.eq.'A'.or.ACTION.eq.'a')then

C Increment NTV and present vertex coords (use previous
C in case of repitition), then parse. Check to see if the
C new vertex is unique (i.e. not within 1mm of an existing
C vertex.
        IADD=NUM
        DO 18 I=1,IADD
          if(I.eq.1)then
            HOLD = ' 0.000  0.000  0.000    '
          else
            WRITE(HOLD,'(3f9.3,a)')X1(I-1),Y1(I-1),Z1(I-1),'  '
          endif
          H(1)='The vertex coordinates should be in metres. '
          H(2)='It is not advisable for Z to be negative.'
          CALL EASKS(HOLD,' Vertex  X  Y  Z (in metres): ',
     &     '  ',32,' 0. 0. 0. ','vertex coord',IER,2)
          K=0
          CALL EGETWR(HOLD,K,X1(I),-999.9,999.9,'W','Xcoord',IER)
          CALL EGETWR(HOLD,K,Y1(I),-999.9,999.9,'W','Ycoord',IER)
          CALL EGETWR(HOLD,K,Z1(I),-9.9,999.9,'W','Zcoord',IER)
  18    CONTINUE

C Append new vertices to zone list if they are unique,
C otherwise continue.
        do 143 ix = 1,IADD
          if(NTV+1.le.MTV)then
            found=.false.
            do 144 ixx = 1,NTV
              CALL ECLOSE(X1(ix),X(ixx),0.004,closex)
              CALL ECLOSE(Y1(ix),Y(ixx),0.004,closey)
              CALL ECLOSE(Z1(ix),Z(ixx),0.004,closez)
              if(closex.and.closey.and.closez)then
                found=.true.
                goto 145
              endif
  144       continue

C If an existing vertex is close then skip to next, otherwise
C add a new vertex to the end of the zone list.
  145       if(found)then
              continue
            else
              NTV=NTV+1
              NZTV(icomp)=NTV
              X(NTV)=X1(ix)
              Y(NTV)=Y1(ix)
              Z(NTV)=Z1(ix)

C For a unique vertex also test to see if it is close (+-0.003m) to
C one of the existing lines. Also check the last edge in the surface.
              iwhich3=NTV
              do 246 ivj=1,NSUR
                ivjlimit=NVER(ivj)
                do 247 ivjj=1,ivjlimit
                  if(ivjj.eq.ivjlimit)then
                    iwhich1=JVN(ivj,ivjj)
                    iwhich2=JVN(ivj,1)
                  else
                    iwhich1=JVN(ivj,ivjj)
                    iwhich2=JVN(ivj,ivjj+1)
                  endif

C Report length of line. Use method of Ward/Radiance in fvect.c
                  call pointtoline(iwhich3,iwhich1,iwhich2,offset,
     &              matchver)
                  if(.NOT.matchver) goto 247
                  if(offset.lt.0.004)then
                    write(outs,'(a,i2,a,3f8.3,a,f6.4,a,i3,a,i3,2a)')
     &               'New vertex ',iwhich3,' @',
     &                X(NTV),Y(NTV),Z(NTV),' is close (',offset,
     &                ') to edge ',IWHICH1,' &',IWHICH2,' of ',
     &                sname(icomp,ivj)
                    call edisp(iuout,outs)

C If current surface (ivj) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JVN indices up one until at the current edge (ivjj) and then
C inserting the new vertex index.
                    if(NVER(ivj)+1.le.MV)then
                      if(offset.le.0.004)then
                        ok=.true.
                      else
                        dok=.true.
                        h(1)='Vertex is close to an existing edge. It'
                        h(2)='probably should also be included in the'
                        h(3)='list of the adjacent surface (so that'
                        h(4)='the edge topology is consistent). '
                        call askok(
     &                  'Insert this vertex into the adjacent surface?',
     &                  '(see help)',ok,dok,4)
                      endif
                    else
                      ok=.false.
                    endif
                    if(ok)then
                      NVER(ivj)=NVER(ivj)+1
                      IXV=NVER(ivj)+1
  148                 continue
                      IXV=IXV-1
                      JVN(ivj,IXV)=JVN(ivj,IXV-1)
                      IF(IXV.GT.ivjj+1)GOTO 148
                      JVN(ivj,ivjj+1)=iwhich3

C Debug...
C                      write(6,'(a,30i3)') 'now jvn is ',
C     &                  (JVN(ivj,ii),ii=1,NVER(ivj))

C Surface (ivj) vertex list has been updated. Go on to next surface.
                      goto 246 
                    endif
                  endif
  247           continue
  246         continue
            endif
          endif
  143   continue
        MODIFY=.TRUE.
        check = .true.
      elseif(ACTION.eq.'C'.or.ACTION.eq.'c')then

C Copy vertex index and then increment NTV and present vertex coords 
C then parse into new vertex.
        call tstamp('>','PRJ: copy vertex')
        WRITE(HOLD,'(3f9.3,a)')X(NUM),Y(NUM),Z(NUM),'  '
        H(1)='The vertex coordinates should be in metres. '
        H(2)='Exact duplicate vertices should be avoided.'
        H(3)='It is not advisable for Z to be negative.'
        write(outs,'(a,i2,a,i2,a)')'Copied Vertex (',NUM,
     &    ' old; current index ',NTV+1,')'
        CALL EASKS(HOLD,outs,' X  Y  Z (in metres): ',
     &    32,' 0. 0. 0. ','vertex coord',IER,3)
        K=0
        CALL EGETWR(HOLD,K,X1(1),-999.9,999.9,'W','X coord',IER)
        CALL EGETWR(HOLD,K,Y1(1),-999.9,999.9,'W','Y coord',IER)
        CALL EGETWR(HOLD,K,Z1(1),-9.9,999.9,'W','Z coord',IER)

C Append new vertice to zone list if it is unique (current assumption
C is that for most models, less than 4mm difference would be considered
C a repeat of an existing vertex, otherwise continue.
        if(NTV+1.le.MTV)then
          found=.false.
          do 244 ixx = 1,NTV
            CALL ECLOSE(X1(1),X(ixx),0.004,closex)
            CALL ECLOSE(Y1(1),Y(ixx),0.004,closey)
            CALL ECLOSE(Z1(1),Z(ixx),0.004,closez)
            if(closex.and.closey.and.closez)then
              found=.true.
              goto 245
            endif
  244     continue

C If an existing vertex is close then skip to next, otherwise
C add a new vertex to the end of the zone list.
  245     if(found)then
            continue
          else
            NTV=NTV+1
            NZTV(icomp)=NTV
            X(NTV)=X1(1)
            Y(NTV)=Y1(1)
            Z(NTV)=Z1(1)

C For a unique vertex also test to see if it is close (+-0.003m) to
C one of the existing lines. Also check the last edge in the surface.
            iwhich3=NTV
            do 346 ivj=1,NSUR
              ivjlimit=NVER(ivj)
              do 347 ivjj=1,ivjlimit
                if(ivjj.eq.ivjlimit)then
                  iwhich1=JVN(ivj,ivjj)
                  iwhich2=JVN(ivj,1)
                else
                  iwhich1=JVN(ivj,ivjj)
                  iwhich2=JVN(ivj,ivjj+1)
                endif

C Report length of line. Use method of Ward/Radiance in fvect.c
                call pointtoline(iwhich3,iwhich1,iwhich2,offset,
     &            matchver)
                if(.NOT.matchver) goto 347
                if(offset.lt.0.004)then
                  write(outs,'(a,i2,a,3f8.3,a,f6.4,a,i3,a,i3,2a)')
     &             'Copied vertex ',iwhich3,' @',
     &              X(NTV),Y(NTV),Z(NTV),' is close (',offset,
     &              ') to edge ',IWHICH1,' &',IWHICH2,' of ',
     &                sname(icomp,ivj)
                  call edisp(iuout,outs)

C If current surface (ivj) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JVN indices up one until at the current edge (ivjj) and then
C inserting the new vertex index.
                  if(NVER(ivj)+1.le.MV)then
                    if(offset.le.0.004)then
                      ok=.true.
                    else
                      dok=.true.
                      h(1)='Vertex is close to an existing edge. It'
                      h(2)='probably should also be included in the'
                      h(3)='list of the adjacent surface (so that'
                      h(4)='the edge topology is consistent). '
                      call askok(
     &                'Insert this vertex into the adjacent surface?',
     &                '(see help)',ok,dok,4)
                    endif
                  else
                    ok=.false.
                  endif
                  if(ok)then
                    NVER(ivj)=NVER(ivj)+1
                    IXV=NVER(ivj)+1
  248               continue
                    IXV=IXV-1
                    JVN(ivj,IXV)=JVN(ivj,IXV-1)
                    IF(IXV.GT.ivjj+1)GOTO 248
                    JVN(ivj,ivjj+1)=iwhich3

C Surface (ivj) vertex list has been updated. Go on to next surface.
                    goto 346 
                  endif
                endif
  347         continue
  346       continue
          endif
        endif
        MODIFY=.TRUE.
        check = .true.
      endif
      if(check)then

C Check to see if new vertex is beyond current bounds of zone.
        MODBND=.FALSE.
        MODLEN=.FALSE.
        IF(X(NTV).GT.XMX.OR.X(NTV).LT.XMN) MODBND=.TRUE.
        IF(Y(NTV).GT.YMX.OR.Y(NTV).LT.YMN) MODBND=.TRUE.
        IF(Z(NTV).GT.ZMX.OR.Z(NTV).LT.ZMN) MODBND=.TRUE.
        IF(MODBND)MODLEN=.TRUE.
      endif

C If change in vertex detected then update common G7
      if(MODIFY)then
        call zgupdate(0,icomp,ier)
      endif
      RETURN
      END

C ************* CKVERT 
C CKVERT: Checks vertex lists and returns whether zone is fully 
C bounded (bound=.true.) and which surfaces/vertex links are unmatched.
C Number of unbounded edges in iub, number inverted surfaces inve.
C IVB = 0 is silent, IVB=1 feedback on errors.

      SUBROUTINE CKVERT(ivb,ICOMP,bounded,iub,inve,IER)
#include "building.h"
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/DRWMAT/TEMAT(4,4), TSMAT(4,4),
     &              ETMMAT(4,4), ETSMAT(4,4), TBOX(3,2)

      DIMENSION COG1(3),COG2(3),COG3(3)
      DIMENSION itv1(MS*MV),ITV2(MS*MV),irelsur(MS*MV)
      CHARACTER outs*124
      CHARACTER SOTHER*15,SMLCN*12,SVFC*4,SOTF*4,SNAME*12
      logical bounded,hilight

C Take a line pair and then look and see if it is referenced in
C reverse order just once. itv1 & itv2 are the vertices, its1 is the
C reference surface. 
      if(ivb.ne.0)call tstamp('>','PRJ: checking vertex match')
      ipair=0
      inve=0
      iub=0
      DO 40 IS=1,NSUR
        DO 42 IV=1,NVER(IS)
          ipair=ipair+1
          itv1(ipair)=JVN(IS,IV)

C Also check the last vertex back to the first.
          if(IV.eq.NVER(IS))then
            itv2(ipair)=JVN(IS,1)
          else
            itv2(ipair)=JVN(IS,IV+1)
          endif
          irelsur(ipair)=IS
  42    continue
  40  continue

C Count the number of times the edge mentioned (in both directions).
      bounded=.true.
      hilight=.false.
      do 144 ip=1,ipair
        matchab = 0
        matchba = 0
        do 146 ipck=1,ipair

C First see if reverse match has been found.
          if(itv1(ip).eq.itv2(ipck).and.itv2(ip).eq.itv1(ipck))then
            matchba = matchba +1
          elseif(itv1(ip).eq.itv1(ipck).and.itv2(ip).eq.itv2(ipck))then
            matchab = matchab +1
          endif
 146    continue
        if(matchab.ne.matchba)then

C The imbalance between matchab and matchba is the number of problem
C edges.  If ivimb=2 then probably two edges in same direction, if
C ivimb=1 then we have an unmatched edge.
          bounded=.false.
          ivimb=(abs((matchab+10)-(matchba+10)))
          if(ivimb.eq.2)then
            write(outs,'(a,i2,a,i2,a,a)')
     &        'The edge between vertices ',itv1(ip),' &',
     &        itv2(ip),' of ',SNAME(icomp,irelsur(ip))
            if(ivb.ne.0)then
              call edisp(iuout,outs)
              call edisp(iuout,'may be adjacent to a reversed surface.')
              hilight=.true.
            endif
            inve=inve+1
          elseif(ivimb.eq.1)then
            write(outs,'(a,i2,a,i2,3a)')
     &        'The edge between vertices ',itv1(ip),' &',itv2(ip),
     &        ' of ',SNAME(icomp,irelsur(ip)),' may be unbounded.'
            if(ivb.ne.0)then
              call edisp(iuout,outs)
              hilight=.true.
            endif
            iub=iub+1
          endif
          if(hilight)then
C Transform ends of edge.
            ILT= -305
            COG1(1)=X(itv1(ip))
            COG1(2)=Y(itv1(ip))
            COG1(3)=Z(itv1(ip))
            CALL VECTRN(COG1,TEMAT,COG2,IER)
            CALL VECTRN(COG2,ETSMAT,COG3,IER)
            call etplot(COG3(1),COG3(2),0,0)
            COG1(1)=X(itv2(ip))
            COG1(2)=Y(itv2(ip))
            COG1(3)=Z(itv2(ip))
            CALL VECTRN(COG1,TEMAT,COG2,IER)
            CALL VECTRN(COG2,ETSMAT,COG3,IER)
            call etplot(COG3(1),COG3(2),ILT,0)
            hilight=.false.
          endif
        endif
 144  continue

      if(.not.bounded)then
        if(ivb.ne.0)then
          call edisp(iuout,'The enclosure is improperly bounded and')
          write(outs,'(a,i3,a)')'there are ',inve+iub,
     &      ' problem edges (hilighted in wireframe).'
          call edisp(iuout,outs)
        endif
      elseif(bounded)then
        if(ivb.ne.0)call edisp(iuout,' Enclosure is fully bounded.')
      endif

      RETURN
      END

C ************* INSREC
C Add a rectanguular surface within or at bottom of existing surface.
C ITRU = unit number for user output, IER=0 OK, IER=1 problem.
C act is 'ii' insert interactive, 'sw' insert window silently, or
C act = 'sd' insert door silently or 'sp' insert window percentage silently.

C << Implementation of a cancel option will require the common block
C << instanciation code blocks to be moved so that the data is only
C << written at the end of the process. This has yet to be done.

      SUBROUTINE INSREC(ITRC,ITRU,ICOMP,IS,act,XO,ZO,XW,ZH,IER)
#include "building.h"
      PARAMETER  (MPL = 200 )
      COMMON/DRWMAT/TEMAT(4,4), TSMAT(4,4),
     &              ETMMAT(4,4), ETSMAT(4,4), TBOX(3,2)

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      COMMON/C1/NCOMP,NCON
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G2/NGL(MS)
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY6/LINSTY(MCON)
      LOGICAL MODIFY,MODLEN,MODBND,OK,dok,matchver

      DIMENSION  XX(MV),YY(MV),ZZ(MV),XA(4),YA(4),ZA(4)
      DIMENSION  AX(MPL),AY(MPL),AZ(MPL),IANXT(MPL)
      DIMENSION  BX(MPL),BY(MPL),BZ(MPL),IBNXT(MPL)
      DIMENSION  SBBOX(3,2)

      CHARACTER SOTHER*15,SNAME*12,SMLCN*12,SVFC*4,SOTF*4
      CHARACTER outs*124,ETEXT*82,hold*36,H*72,DESCRC*25,SN*12,DESC*48
      CHARACTER SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12
      character SSPARENT*12,act*2,T14*14
      character*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL

C Depending on act.
      if(act(1:2).eq.'ii')then
        continue
      elseif(act(1:2).eq.'sw')then
        IRT=1
        IF((NSUR+1.GT.MS).OR.(NTV+4.GT.MTV).OR.(NVER(IS)+6.GT.MV).OR.
     &     (NCON+1.GT.MCON))THEN
          CALL USRMSG(' A new surface could not be added as it',
     &      ' will make the model/zone/surface too complex!','W')
          RETURN
        ENDIF
      elseif(act(1:2).eq.'sd')then
        IRT=2
        IF((NSUR+1.GT.MS).OR.(NTV+4.GT.MTV).OR.(NVER(IS)+6.GT.MV).OR.
     &     (NCON+1.GT.MCON))THEN
          CALL USRMSG(' A new surface could not be added as it',
     &      ' will make the model/zone/surface too complex!','W')
          RETURN
        ENDIF
      elseif(act(1:2).eq.'sp')then
        IRT=3
        IF((NSUR+1.GT.MS).OR.(NTV+4.GT.MTV).OR.(NVER(IS)+6.GT.MV).OR.
     &     (NCON+1.GT.MCON))THEN
          CALL USRMSG(' A new surface could not be added as it',
     &      ' will make the model/zone/surface too complex!','W')
          RETURN
        ENDIF
        PERC = XO

C Determine surface length and height.
        CALL SURLEHI(IS,XYMAX,ZMAX)
        Write(outs,'(a,f4.1,a,f4.1,a)') 'Surface lenght = ',XYMAX,
     &      ' m, surface height = ',ZMAX,' m.'
        call edisp(itru,outs)

C Determine opening width, height, X offset and z offset.
        XW=(XYMAX*SQRT(PERC))/10
        ZH=(ZMAX*SQRT(PERC))/10
        XO=((XYMAX-XW)/2)
        ZO=((ZMAX-ZH)/2)
      endif
      if(act(1:2).ne.'ii')then

C Set all surfaces to standard line width and surface being 
C edited to a thick line if currently in graphics mode.
        ITVNO=0
        ITSNM=0
        MODIFY=.TRUE.
        if(MMOD.EQ.8)then
          CALL INLNST(1)
          CALL SURADJ(ICOMP,IS,IE,TMP,IZC,ISC,IC,DESCRC)
          LINSTY(IC)=2
          nzg=1
          nznog(1)=ICOMP
          izgfoc=ICOMP
          CALL ADJVIEW(ITRC,IER)
        endif
        goto 72
      endif

C Insert within surface or at bottom edge (as in a door).
C First detect the orientation of the surface, if vertical
C then offer option of insertion as percentage.
      if(SVFC(IS)(1:4).eq.'VERT')then
        h(1)='If insertion is within the surface you will be asked for'
        h(2)='an X Y offset from the lower left corner (when viewed '
        h(3)='from the outside) and a width and height. '
        h(4)=' '
        h(5)='If insertion is at the base of the surface you will be '
        h(6)='asked for an X offset from the left edge of the surface '
        h(7)='as well as a width and a height. '
        h(8)=' '
        H(9)='The percentage of facade supports the insertion of'
        H(10)='an opening (window) into a surface as a percentage of'
        H(11)='the facade area.'
        H(12)='Openings will be inserted at the centre of the'
        H(13)='surface.'
        H(14)='NOTE: The function can only be applied for rectangular'
        H(15)='and vertical surfaces.'
        CALL EASKABCD(' Insert rectangular surface:','(see help)',
     &    'within surface','at base','percentage of facade ','continue',
     &    IRT,15)
        if(IRT.eq.4)return
      else
        h(1)='If insertion is within the surface you will be asked for'
        h(2)='an X Y offset from the lower left corner (when viewed '
        h(3)='from the outside) and a width and height. '
        h(4)=' '
        h(5)='If insertion is at the base of the surface you will be '
        h(6)='asked for an X offset from the left edge of the surface '
        h(7)='as well as a width and a height. '
        CALL EASKABC(' Insert rectangular surface:',' ',
     &   'within surface','at base','continue',IRT,7)
        if(IRT.eq.3)return
      endif
      if(IRT.eq.1)then

C In order to insert within a surface, check first that
C there is room for an additional surface and 4 vertices (one for extra
C possible link to the existing surface) as well as if the number of
C vertices in this surface +6 are < MV. 
        IF((NSUR+1.GT.MS).OR.(NTV+4.GT.MTV).OR.(NVER(IS)+6.GT.MV).OR.
     &     (NCON+1.GT.MCON))THEN
          CALL USRMSG(' A new surface could not be added as it',
     &      ' will make the model/zone/surface too complex!','W')
          RETURN
        ENDIF
        XO=1.
        ZO=0.5
        XW=1.
        ZH=1.
      elseif(IRT.eq.2)then
        IF((NSUR+1.GT.MS).OR.(NTV+4.GT.MTV).OR.(NVER(IS)+6.GT.MV).OR.
     &     (NCON+1.GT.MCON))THEN
          CALL USRMSG(' A new surface could not be added as it',
     &      ' will make the model/zone/surface too complex!','W')
          RETURN
        ENDIF
        XO=0.1
        ZO=0.
        XW=0.8
        ZH=2.1
      elseif(IRT.eq.3)then
        IF((NSUR+1.GT.MS).OR.(NTV+4.GT.MTV).OR.(NVER(IS)+6.GT.MV).OR.
     &     (NCON+1.GT.MCON))THEN
          CALL USRMSG(' A new surface could not be added as it',
     &      ' will make the model/zone/surface too complex!','W')
          RETURN
        ENDIF
        PERC = 15.0
      endif

 30   CONTINUE

C Set all surfaces to standard line width and surface being 
C edited to a thick line if currently in graphics mode.
      ITVNO=0
      ITSNM=0
      MODIFY=.TRUE.
      if(MMOD.EQ.8)then
        CALL INLNST(1)
        CALL SURADJ(ICOMP,IS,IE,TMP,IZC,ISC,IC,DESCRC)
        LINSTY(IC)=2
        nzg=1
        nznog(1)=ICOMP
        izgfoc=ICOMP
        CALL ADJVIEW(ITRC,IER)
      endif

  43  if(IRT.eq.1)then
        H(1)='The X offset is the distance from the lower left '
        H(2)='corner of the existing surface (looking from the  '
        H(3)='outside) to the left side of the new surface. '
        H(4)=' '
        H(5)='The Z offset is the distance from the lower left '
        H(6)='corner of the existing surface (looking from the '
        H(7)='outside) to the bottom of the new surface. '
        H(8)=' '
        H(9)='The opening width and height are in metres. '
        write(hold,'(4f8.3,a)') XO,ZO,XW,ZH,'  '
        CALL EASKS(hold,'Insert: X offset, Z offset, Width, Height:',
     &    ' ',36,' 0.1 0.1 1.0 1.0','Xoff Zoff width height',IER,9)
        K=0
        CALL EGETWR(HOLD,K,XO,0.0,99.,'W','X offset',IER)
        CALL EGETWR(HOLD,K,ZO,0.0,99.,'W','Z offset',IER)
        CALL EGETWR(HOLD,K,XW,0.0,99.,'W','width',IER)
        CALL EGETWR(HOLD,K,ZH,0.0,99.,'W','height',IER)
      elseif (IRT.eq.2)then
        H(1)='The X offset is the distance from the lower left '
        H(2)='corner of the existing surface (looking from the  '
        H(3)='outside) to the left side of the new surface. '
        H(4)=' '
        H(5)='The opening width and height are in metres. '
        write(hold,'(3f8.3,a)') XO,XW,ZH,'  '
        CALL EASKS(hold,'Insert: X offset, Width, Height (m):',
     &    ' ',36,' 0.1 0.8 2.1','Xoff Zoff width height',IER,5)
        K=0
        CALL EGETWR(HOLD,K,XO,0.0,99.,'W','X offset',IER)
        CALL EGETWR(HOLD,K,XW,0.0,99.,'W','width',IER)
        CALL EGETWR(HOLD,K,ZH,0.0,99.,'W','height',IER)
      elseif (IRT.eq.3)then

C Ask for percentage.
        hold=' '
        H(1)='Define the surface area of the opening as a percentage'
        H(2)='value of the existing surface (e.g. 40%).'
        H(3)='The new surface will be located in the centre of the'
        H(4)='existing surface.'
        CALL EASKR(PERC,'Surface percentage of existing surface.',
     &    ' ',0.,'F',99.,'F',15.0,'Facade percentage',IER,4)

C Determine surface length and height.
        CALL SURLEHI(IS,XYMAX,ZMAX)
        Write(outs,'(a,f4.1,a,f4.1,a)') 'Surface lenght = ',XYMAX,
     &      ' m, surface height = ',ZMAX,' m.'
        call edisp(itru,outs)

C Determine opening width, height, X offset and z offset.
        XW=(XYMAX*SQRT(PERC))/10
        ZH=(ZMAX*SQRT(PERC))/10
        XO=((XYMAX-XW)/2)
        ZO=((ZMAX-ZH)/2)
      endif
      if(ier.ne.0)goto 43

C Transform existing surface into into site coordinates in the
C surface of the plane. Make up XX,YY,ZZ to pass across to the
C transform routine.
  72  N = NVER(IS)
      DO 150 J = 1,N
        XX(J) = X(JVN(IS,J))
        YY(J) = Y(JVN(IS,J))
        ZZ(J) = Z(JVN(IS,J))
  150 CONTINUE
      CALL ETRANSW(ITRC,ITRU,N,XX,YY,ZZ,0.00,XO,ZO,XW,ZH,XA,YA,ZA)

C If in graphics mode figure out where to draw the new rectangle.
      if(MMOD.EQ.8)then
        IAPNT = 1
        DO 350 J = 1,4
          AX(J) = XA(J)
          AY(J) = YA(J)
          AZ(J) = ZA(J)
          IANXT(J) = J + 1
  350   CONTINUE
        IANXT(4) = IAPNT
        CALL MATPOL(4,IAPNT,AX,AY,AZ,IANXT,TEMAT,
     &              SBBOX,NBP,IBPNT,BX,BY,BZ,IBNXT,IERR)
        CALL CLIPFL(NBP,BX,BY,BZ,ISTAT)
        IF (ISTAT .EQ. 1)goto 32
        IF (ISTAT .EQ.-1)CALL CUTPOL(NB,NBP,IBPNT,BX,BY,BZ,IBNXT,ISTAT)

C Draw the (perhaps clipped) rectangle.
        DO 302 JB = 1,NB
          IP = IABS(IBPNT)
          CALL ORTTRN(BX(IP),BY(IP),BZ(IP),ETSMAT,AAX,AAY,AAZ,IERR)
          call etplot(AAX,AAY,0,0)

C Draw succeeding points until last vertex.
          IP1 = IBNXT(IP)
 451      CONTINUE
          CALL  ORTTRN(BX(IP1),BY(IP1),BZ(IP1),ETSMAT,AAX,AAY,AAZ,IERR)
          call etplot(AAX,AAY,1,0)

          IP1 = IBNXT(IP1)
          IF(IP1 .NE. IP) GOTO 451

C Complete rec.
          CALL  ORTTRN(BX(IP),BY(IP),BZ(IP),ETSMAT,AAX,AAY,AAZ,IERR)
          call etplot(AAX,AAY,1,0)
  302   CONTINUE
   32   CONTINUE

C Confirm opening.
        dok=.true.
        h(1)='If the location appears to be correct in the preview'
        h(2)='you can accept it by saying yes. If you are in doubt'
        h(3)='then say no for a chance to modify it. '
        CALL ASKOK('Position OK?','(see help)',OK,dok,3)
        IF(.NOT.OK)GOTO 30
      endif

C Now update the appropriate common data for the new surface. The
C following code is similar to that in addsur in edzone.f. Because
C the surface to be added is not yet in the connections list find
C its future position via `one connection past the current last
C surface in the zone`. ICONO is the connection for the parent surface.
      ICONT=IZSTOCN(icomp,nsur)+1
      ICONO=IZSTOCN(icomp,is)
      NZSUR(ICOMP)=NZSUR(ICOMP)+1
      NSUR=NSUR+1
      NVER(NSUR)=4
      DO 151 J = 1,4
        X(NTV+J)=XA(J)
        Y(NTV+J)=YA(J)
        Z(NTV+J)=ZA(J)
        JVN(NSUR,J)=NTV+J
  151 CONTINUE
      T14=' '
      SN=' '
      h(1)='Well named surfaces (e.g. call a door a door) '
      h(2)='can help you understand a model better! '
      CALL EASKS(T14,' ',' Inserted surface name (12 char max):',
     &  14,'new_door','surface name',IER,2)
      write(SN,'(a)') T14(1:12)
      call st2name(SN,SNAME(ICOMP,NSUR))

      SOTF(NSUR)=SOTF(IS)
      SVFC(NSUR)=SVFC(IS)
      SOTHER(NSUR)=SOTHER(IS)
      CALL EPMENSV
      CALL EPKMLC(ISEL,'Select one of the constructions for the',
     &    'inserted surface. ',IER)
      CALL EPMENRC
      IF(ISEL.GT.0)then
        WRITE(SMLCN(NSUR),'(A12)')DESC(ISEL)(1:12)
        IF(DESC(ISEL)(15:18).EQ.'TRAN')SOTF(NSUR)='TRAN'
        IF(DESC(ISEL)(15:18).EQ.'OPAQ')SOTF(NSUR)='OPAQ'
      endif
      NGL(NSUR)=0

C The original surface should now be revised as much as possible
C before user intervention is called for. Assume that the vertices
C associated with the new surface will be added to the end of the
C current list. NVV is the previous number of vertices, update 
C JVN to list these in clockwise order.
C If new surface is all inside existing surface connect back 
C to the a vertex of the surface.
      if(IRT.eq.1.or.IRT.eq.3)then

        IHIT=0
        DO 792 IVCK1=1,NVER(IS)
          DO 794 IVCK2=1,NVER(IS)
            if(ivck1.ne.ivck2)then
              IF(JVN(IS,IVCK1).EQ.JVN(IS,IVCK2))IHIT=IHIT+1
            endif
 794      CONTINUE
 792    CONTINUE

C If there already is a hole in the surface (via looking
C at duplicated vertices) then ask preference on how to link
C in the new surface.
        if(IHIT.gt.0)then
          H(1)='The topology of the current surface indicates that '
          H(2)='there is probably an existing `hole` in the surface'
          H(3)='(typically used for glazing). '
          H(4)=' '
          H(5)='Polygons in esp-r are defined as a continuous line,'
          H(6)='so the addition of a second `hole` requires that the'
          H(7)='bounding surface be revised to flow around the new'
          H(8)='surface.  '
          H(9)=' '
          H(10)='You will be asked which corner of the new surface'
          H(11)='and the vertex in the bounding surface to link to.'
          H(12)='If the first `hole` linked to the lower left corner'
          H(13)='(as indicated by a diagonal line) the new surface '
          H(14)='might link to the lower right corner of the '
          H(15)='bounding surface.  This convention will hopefully '
          H(16)='limit the chance of the edges crossing. '
          H(17)=' '
          H(18)='The bounding surface must be wrapped around new one'
          H(19)='so you will be asked for additional information.'
          H(20)=' '
          H(21)='If you cancel when asked about connecting to a'
          H(22)='vertex in the bounding surface, the first point'
          H(23)='of the bounding surface will be assumed. Please'
          H(24)='resolve this issue at a later point!'
          CALL PHELPD('tmc insert warning',24,'-',0,0,IER)

          CALL EASKAB('There is already a `hole` in the surface - ',
     &      'corner of insert to connect to: ','lower left',
     &      'lower right',ICH,24)

          WRITE(outs,'(24i3)')(JVN(IS,J),J=1,NVER(IS))
          call edisp(iuout,'The bounding surface vertices are...')
          call edisp(iuout,outs)
 8795     CALL EASKI(IVH,
     &      ' Which vertex in the bounding surface to tie to:',
     &      ' ( usually at lower right corner of bounding surface) ? ',
     &      1,'F',NTV,'F',1,'bound vert',IERI,24)
          if(ieri.eq.-3)then

C In response to a cancel set ipoint to the first vertex position
C of the bounding surface (position in the list rather than the
C index of the vertex).
            ipoint=1
          else

C See which JVN matches IVH. ipoint is then the position in the edge
C list of the bounding surface.
            ipoint=0
            DO 8796 IVCK=1,NVER(IS)
              IF(JVN(IS,IVCK).EQ.IVH)ipoint=IVCK
 8796       CONTINUE
            if(ipoint.eq.0)then
              call edisp(iuout,'Vertex not part of bounding surface...')
              goto  8795
            endif
          endif

C Shift links above ipoint to allow room.
          call edisp(iuout,' shifting vertex links...')
          NNV=NVER(IS)
          NVER(IS)=NVER(IS)+6
          NTV=NTV+4
          NZTV(icomp)=NTV

C NNX is downward loop counter to shift jvn.

C << for some surface topologies the ipoint logic and the
C << shifting logic clash - requires careful debuging
          NNX=NVER(IS)+1
  154     continue
          NNX=NNX-1
          JVN(IS,NNX)=JVN(IS,NNX-6)
          if(NNX-6.GT.ipoint+1)goto 154
          if(ICH.eq.1)then

C From lower left.
            JVN(IS,ipoint+1)=NTV-3
            JVN(IS,ipoint+2)=NTV
            JVN(IS,ipoint+3)=NTV-1
            JVN(IS,ipoint+4)=NTV-2
            JVN(IS,ipoint+5)=NTV-3
            JVN(IS,ipoint+6)=JVN(IS,ipoint)
          elseif(ICH.eq.2)then

C From lower right.
            JVN(IS,ipoint+1)=NTV-2
            JVN(IS,ipoint+2)=NTV-3
            JVN(IS,ipoint+3)=NTV
            JVN(IS,ipoint+4)=NTV-1
            JVN(IS,ipoint+5)=NTV-2
            JVN(IS,ipoint+6)=JVN(IS,ipoint)
          endif
        else

C No other hole so link to first vertex of surface.
          NNV=NVER(IS)
          NVER(IS)=NVER(IS)+6
          NTV=NTV+4
          NZTV(icomp)=NTV
          JVN(IS,NNV+1)=JVN(IS,1)
          JVN(IS,NNV+2)=NTV-3
          JVN(IS,NNV+3)=NTV
          JVN(IS,NNV+4)=NTV-1
          JVN(IS,NNV+5)=NTV-2
          JVN(IS,NNV+6)=NTV-3
        endif

C Since this rectangle is a new surface, add it to the connection list assuming
C it has the same boundary conditions as the parent surface.
        NCON=NCON+1
        IX=NCON+1

C Update common blocks G6 & G7 (insert data into connections based list).
        call adjcbg6g7('+',ix,icont)

        IC1(ICONT)=ICOMP
        IE1(ICONT)=NSUR
        ICT(ICONT)=ICT(ICONO)
        IC2(ICONT)=IC2(ICONO)
        IE2(ICONT)=IE2(ICONO)
        IZSTOCN(icomp,nsur)=icont
        SSNAME(ICONT)=SNAME(ICOMP,NSUR)
        SSOTF(ICONT)=SOTF(NSUR)
        SSMLCN(ICONT)=SMLCN(NSUR)
        SSVFC(ICONT)=SVFC(NSUR)
        SSOTHER(ICONT)=SOTHER(NSUR)

C Now update the G7 common block to reflect additional surface.
        call zgupdate(1,icomp,ier)

C Update the image.
        MODIFY=.TRUE.
        CALL INLNST(1)
        CALL SURADJ(ICOMP,IS,IE,TMP,IZC,ISC,IC,DESCRC)
        LINSTY(IC)=2
        nzg=1
        nznog(1)=ICOMP
        izgfoc=ICOMP
        CALL ADJVIEW(ITRC,IER)
        call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &    iicfgz,iicfgn,iicfgc,iicfgdfn)
        if(MMOD.EQ.8)then
          call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &      iicfgz,iicfgn,iicfgc,iicfgdfn)
          WRITE(ETEXT,'(2A)')'Project: ',LSNAM(1:lnblnk(LSNAM))
          CALL viewtext(ETEXT,1,1,1)
        endif
        call edisp(iuout,' ')
      elseif(IRT.eq.2)then
        NNV=NVER(IS)
        NVER(IS)=NVER(IS)+4
        NTV=NTV+4
        NZTV(icomp)=NTV

C Shift most of the existing vertices to make room for door.
C NNX is downward loop counter. The logic assumes that the
C lower left corner of the door will become the 2nd vertex
C of the surface.

C check and see if vertex ntv-3 is along the 1st edge.
        iwhich3=NTV-3
        iwhich1=1
        iwhich2=2
        call pointtoline(iwhich3,iwhich1,iwhich2,offset,matchver)
        if(matchver)then
          islip=6
        else

C check and see if vertex ntv-3 is along the 2nd edge.
          iwhich1=2
          iwhich2=3
          call pointtoline(iwhich3,iwhich1,iwhich2,offset,matchver)
          if(matchver)then
            islip=7
          else

C check and see if vertex ntv-3 is along the 3rd edge.
            iwhich1=3
            iwhich2=4
            call pointtoline(iwhich3,iwhich1,iwhich2,offset,matchver)
            if(matchver)then
              islip=8
            else

C check and see if vertex ntv-3 is along the 4th edge.
              iwhich1=4
              iwhich2=5
              call pointtoline(iwhich3,iwhich1,iwhich2,offset,matchver)
              if(matchver)then
                islip=9
              else

C check and see if vertex ntv-3 is along the 5th edge (typical if one
C door already added to the surface.
                iwhich1=5
                iwhich2=6
                call pointtoline(iwhich3,iwhich1,iwhich2,offset,
     &            matchver)
                if(matchver)then
                  islip=10
                else

C Some other surface edge list so cannot do this automatically.
                  call usrmsg('You will have to manually edit the',
     &              'surface to `wrap` it around the door','W') 
                  islip=6
                endif
              endif
            endif
          endif
        endif
        NNX=NVER(IS)+1
  153   continue
        NNX=NNX-1
        JVN(IS,NNX)=JVN(IS,NNX-4)
        if(NNX.GT.islip)goto 153
        JVN(IS,islip-4)=NTV-3
        JVN(IS,islip-3)=NTV
        JVN(IS,islip-2)=NTV-1
        JVN(IS,islip-1)=NTV-2

C Since this door is a new surface, add it to the connection list assuming
C it has the same boundary conditions as the parent surface.
        NCON=NCON+1
        IX=NCON+1

C Update common blocks G6 & G7 (insert data into connections based list).
        call adjcbg6g7('+',ix,icont)

        IC1(ICONT)=ICOMP
        IE1(ICONT)=NSUR
        ICT(ICONT)=ICT(ICONO)
        IC2(ICONT)=IC2(ICONO)
        IE2(ICONT)=IE2(ICONO)
        IZSTOCN(icomp,nsur)=icont
        SSNAME(ICONT)=SNAME(ICOMP,NSUR)
        SSOTF(ICONT)=SOTF(NSUR)
        SSMLCN(ICONT)=SMLCN(NSUR)
        SSVFC(ICONT)=SVFC(NSUR)
        SSOTHER(ICONT)=SOTHER(NSUR)

C Now update the G7 common block to reflect additional surface.
        call zgupdate(1,icomp,ier)

C Update the image.
        MODIFY=.TRUE.
        CALL INLNST(1)
        CALL SURADJ(ICOMP,IS,IE,TMP,IZC,ISC,IC,DESCRC)
        LINSTY(IC)=2
        nzg=1
        nznog(1)=ICOMP
        izgfoc=ICOMP
        CALL ADJVIEW(ITRC,IER)
        call cfgtogg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &    iicfgz,iicfgn,iicfgc,iicfgdfn)
        if(MMOD.EQ.8)then
          call opencfg(icfg_type,icfgz,icfgn,icfgc,icfgdfn,
     &      iicfgz,iicfgn,iicfgc,iicfgdfn)
          WRITE(ETEXT,'(2A)')'Project: ',LSNAM(1:lnblnk(LSNAM))
          CALL viewtext(ETEXT,1,1,1)
        endif
        call edisp(iuout,' ')

C This checks to see what edges are close to the new verticies
C ntv-2 and ntv-3 and the offering to update that surfaces vertex list.
        iadd=2
        do 143 ix = 1,IADD
          if(ix.eq.1)then
            iwhich3=NTV-3
          elseif(ix.eq.2)then
            iwhich3=NTV-2
          endif
          do 246 ivj=1,NSUR

C Don't bother checking the surface that the door has been inserted
C into or the door itself.
            if(ivj.eq.is.or.ivj.eq.nsur) goto 246
            ivjlimit=NVER(ivj)
            do 247 ivjj=1,ivjlimit
              if(ivjj.eq.ivjlimit)then
                iwhich1=JVN(ivj,ivjj)
                iwhich2=JVN(ivj,1)
              else
                iwhich1=JVN(ivj,ivjj)
                iwhich2=JVN(ivj,ivjj+1)
              endif

C Report length of line. Use method of Ward/Radiance in fvect.c
              call pointtoline(iwhich3,iwhich1,iwhich2,offset,
     &          matchver)
              if(.NOT.matchver) goto 247
              if(offset.lt.0.004)then
                write(outs,'(a,i2,a,3f8.3,a,f6.4,a,i3,a,i3,2a)')
     &            'Door vertex ',iwhich3,' @',
     &            X(NTV),Y(NTV),Z(NTV),' is close (',offset,
     &            ') to edge ',IWHICH1,' &',IWHICH2,' of ',
     &            sname(icomp,ivj)
                call edisp(iuout,outs)

C If current surface (ivj) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JVN indices up one until at the current edge (ivjj) and then
C inserting the new vertex index.
                if(NVER(ivj)+1.le.MV)then
                  if(offset.le.0.004)then
                    ok=.true.
                  else
                    dok=.true.
                    h(1)='The vertex is close to another edge. To'
                    h(2)='maintain the edge topology you probably want'
                    h(3)='to update the edges of the adjacent surface.'
                    call askok(
     &                'Insert this vertex into the adjacent surface?',
     &                '(see help)',ok,dok,3)
                  endif
                else
                  ok=.false.
                endif
                if(ok)then
                  NVER(ivj)=NVER(ivj)+1
                  IXV=NVER(ivj)+1
  148             continue
                  IXV=IXV-1
                  JVN(ivj,IXV)=JVN(ivj,IXV-1)
                  IF(IXV.GT.ivjj+1)GOTO 148
                  JVN(ivj,ivjj+1)=iwhich3

C Debug...
C                  write(6,'(a,30i3)') 'now jvn is ',
C     &              (JVN(ivj,ii),ii=1,NVER(ivj))

C Surface (ivj) vertex list has been updated. Go on to next surface.
                  goto 246 
                endif
              endif
  247       continue
  246     continue
  143   continue
      endif

C Update common blocks G6 & G7 to account for any vertices added
C to adjacent surfaces.
      call zgupdate(1,icomp,ier)

C Now warn the user to carefully look at the new surface which has been
C created as well as the additional info in the old surface.  The 
C assumptions for where to add vertices may require the user to do
C further modifications.
      H(1)='The modifications to the geometry of the zone, surface'
      H(2)='attributes and topology which have been made need to'
      H(3)='be checked closely.  For example the original surface'
      H(4)='topology must have a link between the outer boundaries'
      H(5)='and the `hole` so that it is a proper polygon. '
      CALL PHELPD('surface details',5,'-',0,0,IER)
      MODIFY=.TRUE.

      RETURN
      END

C*******************************************************************
C SURLEHI Determines the overall lenght (in the x/y plane) and hight
C (in z-direction) of a surface and passes the vaules back as
C XYMAX and ZMAX.
      SUBROUTINE SURLEHI(IS,XYMAX,ZMAX)
#include "building.h"

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

C Set maximum values to zero.
      XYMAX = 0
      ZMAX = 0

C Loop through all the vertices of the surface and define vertex
C pair to be compared.
      N = NVER(IS)
      Do 151 JA = 1,N
        Do 152 JB = 1,N
        X1 = X(JVN(IS,JA))
        Y1 = Y(JVN(IS,JA))
        Z1 = Z(JVN(IS,JA))
        X2 = X(JVN(IS,JB))
        Y2 = Y(JVN(IS,JB))
        Z2 = Z(JVN(IS,JB))

C Determine the hight difference between the vertices. If the hight
C difference is greater than the previous values then define as new
C maximum.
        ZVAL = ABS(Z1 - Z2)
        If (ZVAL.gt.ZMAX) ZMAX = ZVAL

C Determine the distance between the vertices on the x/y plane. If the
C distance is greater than the previous values then define as new
C maximum.
        XYVAL= crowxyz(X1,Y1,0.,X2,Y2,0.)
        if (XYVAL.gt.XYMAX) XYMAX = XYVAL
  152   Continue
  151 Continue
      END

C ********* adjcbg6g7
C Adjcbg6g7 adjusts common blocks G6 and G7.
C Passed act:
C act = `+` does the SS(ix)=SS(ix-1) bit and ix is one more
C           than the current number of connections and icon
C           is the point of insertion (so as to know when to
C           stop shifting).
      subroutine adjcbg6g7(act,ix,icon)
#include "building.h"

C nbedgdup(MCON) number of duplicate edges in surface edge list.
C iedgdup(MCON,MV) for each edge, the connection and edge duplicated.
C nbedgshr(MCON) number of surfaces (same orient) which share an edge:
C   if only one then it must be parent, if two then it might be the
C   case of a door so determine which is primary.
C iedgshr(MCON,MV) for each edge, the connection of surface (similarly
C   oriented) sharing an edge. Used to detect parent/child. Zero denotes
C   this does not apply.
C imatshr(MCON,MV) for each edge, the connection of surface (similarly
C   oriented) which has the same material. Zero denotes this does not
C   apply.used to enhance wire frame drawings of discritized zones (e.g.
C   where a surface such as a floor has been subdivided)
      common/G8/nbedgdup(MCON),iedgdup(MCON,MV),nbedgshr(MCON),
     &          iedgshr(MCON,MV),imatshr(MCON,MV)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/G7/SSNA(MCON),SSPAZI(MCON),SSPELV(MCON),SSPERIM(MCON),
     &          SSUREQN(MCON,4),SSURCOG(MCON,3),SSURVN(MCON,3)
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      character SSMLCN*12,SSVFC*4,SSOTF*4,SSOTHER*15,SSNAME*12
      character SSPARENT*12,act*1

      if(act.eq.'+')then
C Add a connection, this code shifts G6 G7 G8 data upwards.
C Typically the mark 234 has been used with this code.
  234   CONTINUE
        IX=IX-1
        IC1(IX)=IC1(IX-1)
        IE1(IX)=IE1(IX-1)
        ICT(IX)=ICT(IX-1)
        IC2(IX)=IC2(IX-1)
        IE2(IX)=IE2(IX-1)
        SSNAME(IX)=SSNAME(IX-1)
        SSOTF(IX)=SSOTF(IX-1)
        SSMLCN(IX)=SSMLCN(IX-1)
        SSVFC(IX)=SSVFC(IX-1)
        SSOTHER(IX)=SSOTHER(IX-1)
        SSNA(IX)=SSNA(IX-1)
        SSPAZI(IX)=SSPAZI(IX-1)
        SSPELV(IX)=SSPELV(IX-1)
        SSPERIM(IX)=SSPERIM(IX-1)
        SSUREQN(IX,1)=SSUREQN(IX-1,1)
        SSUREQN(IX,2)=SSUREQN(IX-1,2)
        SSUREQN(IX,3)=SSUREQN(IX-1,3)
        SSUREQN(IX,4)=SSUREQN(IX-1,4)
        SSURCOG(IX,1)=SSURCOG(IX-1,1)
        SSURCOG(IX,2)=SSURCOG(IX-1,2)
        SSURCOG(IX,3)=SSURCOG(IX-1,3)
        SSURVN(IX,1)=SSURVN(IX-1,1)
        SSURVN(IX,2)=SSURVN(IX-1,2)
        SSURVN(IX,3)=SSURVN(IX-1,3)
        nbedgdup(IX)=nbedgdup(IX-1)
        nbedgshr(IX)=nbedgshr(IX-1)
        do 42 ijj=1,MV
          iedgdup(IX,ijj)=iedgdup(IX-1,ijj)
          iedgshr(IX,ijj)=iedgshr(IX-1,ijj)
          imatshr(IX,ijj)=imatshr(IX-1,ijj)
  42    continue
        IF(IX.GT.ICON+1)GOTO 234
        return
      else
        return
      endif
      end
