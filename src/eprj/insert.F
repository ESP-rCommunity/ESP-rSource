C This file is part of the ESP-r system.
C Copyright Energy Systems Research Unit, University of
C Strathclyde, Glasgow Scotland, 2001.

C ESP-r is free software.  You can redistribute it and/or
C modify it under the terms of the GNU General Public
C License as published by the Free Software Foundation 
C (version 2 or later).

C ESP-r is distributed in the hope that it will be useful
C but WITHOUT ANY WARRANTY; without even the implied
C warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
C PURPOSE. See the GNU General Public License for more
C details.


C insert.F contains:
C  ADDSUR : add, delete, copy a surface within a zone.
C  ADDMASS: add a horizontal or vertical rectangular mass in zone.
C  ADDVERT: add or delete a vertex within a zone.
C  CKVERT : checks vertex lists & returns if zone is fully bounded.
C  INSREC : add rect surface within or at base of surface.
C  Adjcbg6g7: adjusts common blocks G6 and G7 to account for new surface.
C addedsurf: copies G1 & G5 common block values to G6
C mergedoorinparent: does what is says, works with current common block variables.
C mergechildinparent: does what is says, works with current common block variables.

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

C << what about including common zndata in this manipulation? >>

      SUBROUTINE ADDSUR(ITRC,ICOMP,IWHICH,ACTION,MODE,ISCOPT,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "sbem.h"
#include "prj3dv.h"
#include "esprdbfile.h"
#include "material.h"
#include "help.h"
      
      integer lnblnk  ! function definition
      integer iCountWords

C Passed parameters:
      integer itrc,icomp,iwhich,iscopt,ier
      character ACTION*1,MODE*1

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      LOGICAL closex,closey
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK
      LOGICAL     OK,CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK,CFCDBOK

C Tollerance for surface matching.
      real ANGCC  ! angle between surfaces tolerance
      real CACC   ! tolerance between vertices
      real DACC   ! tolerance along line
      real COGCC  ! tolerance between surface COG
      real SNACC  ! tolerance between surface areas
      integer IACC ! number of matching corners outside dist tolerance
      common/matching/ANGCC,CACC,DACC,COGCC,SNACC,IACC

      logical same,clkok,found,DDOK,nameok,matchver
      logical newgeo  ! to use for testing if new/old geometry file.
      logical hasanotherchild ! true is surface is parent
      logical greyok  ! for drawing red dot
      logical modgeo  ! in case SVFC is updated
      logical showother,updoth,done
      integer isurloop,iverloop,ichildloop ! for use in loops
      integer iccc ! for connection numbers
      integer lnmyp,lnssn  ! lengths of strings
      integer myedges ! number of edges in the child surface
      integer parentedges  ! number of edges in the parent surface
      integer foundone  ! index of a shared vertex
      integer foundonepos ! position of shared vertex in parent jvn array
      integer iotherzone ! the other zone needing to be updated
      integer ipkany,izc ! for use with pickssur
      integer ijvn       ! to count selected points

      dimension X1(MV),Y1(MV),Z1(MV),jvn1(MV),XT(MV),YT(MV),ZT(MV)
      dimension jvn2(MV)  ! dummy array for sorti call
      DIMENSION COG1(3),COG2(3),JJVN(MV),ispk(MS)

      CHARACTER SNAME1*12,SMLCN1*32,SVFC1*4,SOTF1*32,SNAMED*12
      character SUSE11*12,SUSE12*12
      CHARACTER outs*124,SN*12,SN2*12
      character ZSDES*28,T14*14,D14*14
      character TOSMLCN*32       ! to remember other side mlc name
      character TOOPT*32         ! to remember other side optics
      character TOUSE1*12,TOUSE2*12  ! to remember other side use
      character TSOTH1*24,TSOTH2*24,TSOTH3*24
      character HOLD*96,hold64*64,holds*36,t248*496
C      character guesstype*24  ! for context of surface addition
      character myparentis*12 ! to remember the parent of a surface.
      character message*48    ! for vertex dialog
      integer IRT  ! for radio button
      integer icmpall  ! to signal request to compare all names
      integer ltrace   ! local trace
      integer ISTRW

C Trackview should require iixx,iiyy.
#ifdef OSI
      integer iix,iiy,ixd,iyd,iicol,iixx,iiyy,iik
#else
      integer*8 iix,iiy,ixd,iyd,iicol,iixx,iiyy,iik
#endif

      helpinsub='insert'  ! set for subroutine
      greyok=.false.
      if(nifgrey.gt.4)then
        greyok=.true.
      endif

      IER=0
      
C Clear local variables.
      DDOK=.false.
      newgeo=.false.  ! assume older format geometry.
      vdis=0.0; valx=0.0; valy=0.0; valz=0.0
      angr=0.0
      same=.true.
      clkok=.false.
      done=.false.
      IOK=0; IRV=1; IZC=0
      TOSMLCN=' '; TOOPT=' '; TOUSE1=' '; TOUSE2=' '
      TSOTH1=' '; TSOTH2=' '; TSOTH3=' '

      IF(ACTION.EQ.'D')THEN

C Unlink any other reference pointing to zone icomp and surface iwhich.
C Save the current state of the zone where the surface is being deleted.
        CALL ESCZONE(ICOMP)

C While looping remember the connection associated with zone:surf.
        do 35 IXU = 1,NCON
          if(IC1(IXU).eq.ICOMP.AND.IE1(IXU).eq.IWHICH)ICON=IXU
          if(ICT(IXU).EQ.3)then
            if(IC2(IXU).eq.ICOMP.and.IE2(IXU).EQ.IWHICH)THEN
              iotherzone=IC1(IXU)
              write(outs,'(5a)') 
     &          'Freeing `other side` information of ',
     & sname(iotherzone,IE1(ixu))(1:lnblnk(sname(iotherzone,IE1(ixu)))),
     &          ' ',zname(iotherzone)(1:lnzname(iotherzone)),'...'
              call edisp(iuout,outs)

C The sother() array has changed so the other zone should
C be rewritten. Scan it and update sother and then write.

C << goal is to have all of the information within zone geometry
C << files held in memory so files can be updated without having
C << to re-read. several steps required before this is possible.

              call eclose(gversion(iotherzone),1.1,0.01,newgeo)
              call georead(IFIL+2,LGEOM(iotherzone),iotherzone,0,
     &            iuout,IER)

C Adjust the data structures.
              ICT(IXU)=-1; IC2(IXU)=0; IE2(IXU)=0
              SOTHER(IC1(IXU),IE1(IXU),1)='UNKNOWN'
              SOTHER(IC1(IXU),IE1(IXU),2)='0'
              SOTHER(IC1(IXU),IE1(IXU),3)='0'

C Logic to decide whether to upgrade other zone geometry format.
              if(.NOT.newgeo)then
                gversion(iotherzone) =1.1
                newgeo = .true.
              endif
              call geowrite(IFIL+2,LGEOM(iotherzone),iotherzone,
     &          iuout,3,IER)
            endif
          endif
   35   continue

C Any other connection which references a surface in focus zone
C and which is about to be moved down should be adjusted.
        do 36 IXU = 1,NCON
          if(ICT(IXU).EQ.3)then
            if(IC2(IXU).eq.ICOMP.and.IE2(IXU).gt.IWHICH)THEN

C The sother() array has changed so the other zone should
C be rewritten. Scan it and update ssother and then write.
              iotherzone=IC1(IXU)
              write(outs,'(3a)') 
     &          'Freeing `other side` information in ',
     &          zname(iotherzone)(1:lnzname(iotherzone)),
     &          ' and compacting master list...'
              call edisp(iuout,outs)
              call eclose(gversion(iotherzone),1.1,0.01,newgeo)
              call georead(IFIL+2,LGEOM(iotherzone),iotherzone,0,
     &            iuout,IER)

C Update IE2 and sother third tag in the other zone.
              IE2(IXU)=IE2(IXU)-1
              write(sother(IC1(IXU),IE1(IXU),1),'(a)') 'ANOTHER'
              write(sother(IC1(IXU),IE1(IXU),2),'(i2.2)') IC2(ixu)
              write(sother(IC1(IXU),IE1(IXU),3),'(i3.3)') IE2(ixu)

C Upgrade other zone geometry format if older.
              if(.NOT.newgeo)then
                gversion(iotherzone) =1.1
                newgeo = .true.
              endif
              call geowrite(IFIL+2,LGEOM(iotherzone),iotherzone,
     &          iuout,3,IER)
            endif
          endif
   36   continue

C Read current zone back in from temporary store.
        CALL ERCZONE(ICOMP)

C Is the surface to be deleted a child of another surface. If it
C is remember the name of the parent surface and also remember the
C indices of the vertices used by the child so that they can be
C removed from the parent surface.
        if(SPARENT(icomp,iwhich)(1:2).eq.'- ')then
          myparentis='- '
        else
          myparentis=SPARENT(icomp,iwhich)
          myedges=NVER(iwhich)
          do 746 iyy = 1,MV
            if(iyy.le.myedges)then
              jvn1(iyy)=JVN(iwhich,iyy)
            else
              jvn1(iyy)=0
            endif
  746     continue
        endif

C Compact the list:  If within the zone where the surface was removed, 
C update IE1 for surfaces > IWHICH otherwise just compact IC1 & IE1.
        DO 235 IX=ICON,NCON-1
          IF(IC1(IX+1).EQ.ICOMP)THEN
            IF(IE1(IX+1).GT.IWHICH)IE1(IX)=IE1(IX+1)-1
          ELSEIF(IC1(IX+1).gt.ICOMP)THEN
            IC1(IX)=IC1(IX+1)
            IE1(IX)=IE1(IX+1)
          ENDIF
          ICT(IX)=ICT(IX+1); IC2(IX)=IC2(IX+1); IE2(IX)=IE2(IX+1)
          nbedgdup(IX)=nbedgdup(IX+1)
          nbedgshr(IX)=nbedgshr(IX+1)
          do 42 ijj=1,MV
            iedgdup(IX,ijj)=iedgdup(IX+1,ijj)
            iedgshr(IX,ijj)=iedgshr(IX+1,ijj)
            imatshr(IX,ijj)=imatshr(IX+1,ijj)
  42      continue

  235   CONTINUE
        NCON=NCON-1  ! decrement number of connections in the model

C Re-establish link between zone/surfaces and connections.
        do 335 iccc = 1, NCON
          IZSTOCN(IC1(iccc),IE1(iccc))=iccc
 335    continue

C  Since this routine may be used to completly remove a zone, trap
C if small number of surfaces.
        if(NSUR.eq.1)goto 65

C Check if one of the ibases list points to iwhich, if so shift
C later ibases items down one.

C Remove index iwhich from ibases list. Loop to check if any point
C in the array is the same as iwhich and then pack list from that
C point and jump to 338.
        is=iwhich
        do 336 ijj=1,9
          if(ibases(icomp,ijj).eq.is)then
            do 337 ijk=ijj,9
              ibases(icomp,ijk)=ibases(icomp,ijk+1)
  337       continue
            goto 338
          endif
  336   continue
  338   continue

C Loop through each of the surfaces greater than IWHICH and see if
C they are included in ibases list. If so decrement ibases value.
        DO 68 IS=IWHICH,NSUR-1
          if(is.eq.ibases(icomp,1)) ibases(icomp,1)=ibases(icomp,1)-1
          if(is.eq.ibases(icomp,2)) ibases(icomp,2)=ibases(icomp,2)-1
          if(is.eq.ibases(icomp,3)) ibases(icomp,3)=ibases(icomp,3)-1
          if(is.eq.ibases(icomp,4)) ibases(icomp,4)=ibases(icomp,4)-1
          if(is.eq.ibases(icomp,5)) ibases(icomp,5)=ibases(icomp,5)-1
          if(is.eq.ibases(icomp,6)) ibases(icomp,6)=ibases(icomp,6)-1
          if(is.eq.ibases(icomp,7)) ibases(icomp,7)=ibases(icomp,7)-1
          if(is.eq.ibases(icomp,8)) ibases(icomp,8)=ibases(icomp,8)-1
          if(is.eq.ibases(icomp,9)) ibases(icomp,9)=ibases(icomp,9)-1
          if(is.eq.ibases(icomp,10))ibases(icomp,10)=ibases(icomp,10)-1
          if(is.eq.ibases(icomp,11))ibases(icomp,11)=ibases(icomp,11)-1
          if(is.eq.ibases(icomp,12))ibases(icomp,12)=ibases(icomp,12)-1
          if(is.eq.ibases(icomp,13))ibases(icomp,13)=ibases(icomp,13)-1
          if(is.eq.ibases(icomp,14))ibases(icomp,14)=ibases(icomp,14)-1
          if(is.eq.ibases(icomp,15))ibases(icomp,15)=ibases(icomp,15)-1
          if(is.eq.ibases(icomp,16))ibases(icomp,16)=ibases(icomp,16)-1
          if(is.eq.ibases(icomp,17))ibases(icomp,17)=ibases(icomp,17)-1
          if(is.eq.ibases(icomp,18))ibases(icomp,18)=ibases(icomp,18)-1
          if(is.eq.ibases(icomp,19))ibases(icomp,19)=ibases(icomp,19)-1
  68    continue

C Loop through each of the surfaces greater than IWHICH and copy their
C G5 contents into the surface below. The G6 commons were done already.
        if(IWHICH.eq.NSUR)then
          SNAME(ICOMP,IWHICH)=' '; SOTF(ICOMP,IWHICH)=' '
          SMLCN(ICOMP,IWHICH)=' '; SVFC(ICOMP,IWHICH)=' '
          SOTHER(ICOMP,IS,1)=' ';  SOTHER(ICOMP,IS,2)=' '
          SOTHER(ICOMP,IS,3)=' '
          goto 65
        endif
        DO 66 IS=IWHICH,NSUR-1
          SNAME(ICOMP,IS)=SNAME(ICOMP,IS+1)
          LNSNAME(ICOMP,IS)=LNSNAME(ICOMP,IS+1)
          SOTF(ICOMP,IS)=SOTF(ICOMP,IS+1)
          LNSOTF(ICOMP,IS)=LNSOTF(ICOMP,IS+1)
          SMLCN(ICOMP,IS)=SMLCN(ICOMP,IS+1)
          smlcindex(ICOMP,IS)=smlcindex(ICOMP,IS+1)
          SVFC(ICOMP,IS)=SVFC(ICOMP,IS+1)
          SOTHER(ICOMP,IS,1)=SOTHER(ICOMP,IS+1,1)
          SOTHER(ICOMP,IS,2)=SOTHER(ICOMP,IS+1,2)
          SOTHER(ICOMP,IS,3)=SOTHER(ICOMP,IS+1,3)
          SPARENT(ICOMP,IS)=SPARENT(ICOMP,IS+1)
          SUSE(ICOMP,IS,1)=SUSE(ICOMP,IS+1,1)
          SUSE(ICOMP,IS,2)=SUSE(ICOMP,IS+1,2)
          NVER(IS)=NVER(IS+1)
          isznver(ICOMP,IS)=NVER(IS)
          DO IV=1,NVER(IS)
            JVN(IS,IV)=JVN(IS+1,IV)
            iszjvn(ICOMP,IS,IV)=JVN(IS,IV)
          ENDDO

          SUREQN(ICOMP,IS,1)=SUREQN(ICOMP,IS+1,1)
          SUREQN(ICOMP,IS,2)=SUREQN(ICOMP,IS+1,2)
          SUREQN(ICOMP,IS,3)=SUREQN(ICOMP,IS+1,3)
          SUREQN(ICOMP,IS,4)=SUREQN(ICOMP,IS+1,4)
   66   CONTINUE
   65   NSUR=NSUR-1
        NZSUR(ICOMP)=NSUR
        NZTV(ICOMP)=NTV

C If the deleted surface was a child then it should also be possible
C to remove the vertices of the child from the parent surface. Loop
C through surfaces in the zone to find one matching the name myparentis.
C Then loop through each of the vertices of that matching surface and
C for any vertex that was used by the child offer to delete that vertex
C reference in the parent. Use addvert to do the deletion.
        if(myparentis(1:2).eq.'- ')then
          continue
        else
          lnmyp=lnblnk(myparentis)
          do 747 isurloop=1,NZSUR(ICOMP)
            parentedges=NVER(isurloop)
            lnssn=lnblnk(sname(icomp,isurloop))
            if(myparentis(1:lnmyp).eq.
     &         sname(icomp,isurloop)(1:lnssn))then
  760         foundone=0   ! where the logic beings
              foundonepos=0
              do 748 iverloop=1,parentedges
                do 749 ichildloop=1,myedges
                  if(jvn1(ichildloop).eq.jvn(isurloop,iverloop))then
                    foundone=jvn1(ichildloop)
                    foundonepos=iverloop
                  endif
  749           continue

C Shared vertex located, remove this vertex from the list of the parent
C surface and then decrement nver for the parent surface.
                if(foundone.eq.0.or.foundonepos.eq.0)then
                  continue
                else
                  do 750 IV=foundonepos,parentedges-1
                    JVN(isurloop,IV)=JVN(isurloop,IV+1)
                    iszjvn(icomp,isurloop,IV)=JVN(isurloop,IV)
  750             continue
                  JVN(isurloop,parentedges)=0     ! zero last position
                  iszjvn(icomp,isurloop,parentedges)=0
                  NVER(isurloop)=NVER(isurloop)-1 ! decrement list length
                  isznver(icomp,isurloop)=NVER(isurloop)
                  parentedges=NVER(isurloop)      ! so next pass works.
                  foundone=0     ! reset
                  foundonepos=0
                  goto 760  ! start the process again.
                endif
  748         continue

C There may still be duplicate vertices if the parent surface
C wrapped around the child. Check if it has another child and
C if so do a search for duplicate vertices.
              hasanotherchild=.false.
              do 751 ichildloop=1,NZSUR(ICOMP)
                if(sparent(icomp,ichildloop)(1:12).eq.
     &             sname(icomp,isurloop)(1:12))then
                  hasanotherchild=.true.
                endif
  751         continue
              if(.NOT.hasanotherchild)then

C The classic case for removing a window is that this leaves
C a parent surface with the first and last vertices identical
C and if this is true the last vertex can be deleted from the
C list. If the surface does not have another child then do this
C test and if it does have another child then best not to modify
C the jvn list of the parent surface.
                if(JVN(isurloop,1).eq.JVN(isurloop,NVER(isurloop)))then
                  JVN(isurloop,NVER(isurloop))=0
                  iszjvn(icomp,isurloop,NVER(isurloop))=0
                  NVER(isurloop)=NVER(isurloop)-1
                  isznver(icomp,isurloop)=NVER(isurloop)
                endif

C The other case that might be encountered is two consecutive
C identical entries in jvn in which case the 2nd can be deleted.
                do 753 iver=1,nver(isurloop)-1
                  IIVER=JVN(isurloop,iver)    ! the vertex to compare against
                  JJVER=JVN(isurloop,iver+1)  ! the next position
                  if(JJVER.eq.IIVER)then
                    do 754 IV=JJVER,parentedges-1
                      JVN(isurloop,IV)=JVN(isurloop,IV+1)
                      iszjvn(icomp,isurloop,IV)=JVN(isurloop,IV)
  754               continue
                    JVN(isurloop,parentedges)=0
                    iszjvn(icomp,isurloop,parentedges)=0
                    NVER(isurloop)=NVER(isurloop)-1
                    isznver(icomp,isurloop)=NVER(isurloop)
                  endif 
  753           continue
              endif   ! of hasanotherchild
            endif     ! of myparentis
  747     continue  ! end of isurloop zone loop.

C Now that the matching vertices have been removed from the parent
C surface jvn list the child vertices can be deleted. Delete them
C in decending order (use call to sorti for this). There is one way
C that this process can fail - if an adjacent surface is found with
C one of these matching vertices and addvert is called then the
C indicies of the verticies in jvn1 will not reflect what is available
C in the zone. Tested for deleting a door and the logic works because
C of the normal ordering of the vertices.
          KFLAG = -1
          call SORTI(jvn1,jvn2,MV,KFLAG)
          do 752 ichildloop=1,myedges
            ijj=jvn1(ichildloop)
            CALL ADDVERT(icomp,ijj,'D',IER)
            if(ier.eq.0)then
              continue
            elseif(ier.gt.0)then
              write(outs,'(a,i3,3a)') 'The vertex ',ijj,
     &          ' is also referenced by surface ',sname(icomp,ier),'!'
              helptopic='remove_vert_as_well'
              call gethelptext(helpinsub,helptopic,nbhelp)

C Be on the safe side and do not remove verticies for now, except in UK
C NCM model generation where removal of surfaces and vertices is
C expected to be done without user interaction
c              call easkok(outs,
c     &          'Remove it from there as well?',ok,nbhelp)
              ok=.false.
              IF(INOTI.EQ.1)OK=.TRUE.
              if(ok)then

C Similar logic for identifying the position in the other surface
C of the shared vertex, then decrement nver() for the other surface.
                iwhich=ier
                foundone=0   ! where the logic beings
                foundonepos=0
                do 755 iverloop=1,nver(iwhich)
                  if(ijj.eq.jvn(iwhich,iverloop))then
                    foundone=ijj
                    foundonepos=iverloop
                  endif
  755           continue
                if(foundone.eq.0.or.foundonepos.eq.0)then
                  continue
                else
                  do 756 IV=foundonepos,nver(iwhich)-1
                    JVN(iwhich,IV)=JVN(iwhich,IV+1)
                    iszjvn(icomp,ier,IV)=JVN(iwhich,IV)
  756             continue
                  JVN(iwhich,nver(iwhich))=0    ! zero last position
                  iszjvn(icomp,ier,nver(iwhich))=0
                  NVER(iwhich)=NVER(iwhich)-1   ! decrement list length
                  isznver(icomp,iwhich)=NVER(iwhich)
                  foundone=0     ! reset
                  foundonepos=0

C Try a second time to delete this vertex from the zone. This works
C because the next child vertex to be deleted has a smaller index.
                  CALL ADDVERT(icomp,ijj,'D',IER)
                endif
              endif
            endif
  752     continue
        endif

C If in automatic mode 
        if(MODE.eq.'A')then
          MODIFYVIEW=.TRUE.
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
     &      CFGOK,IZC,'-',34,IER)

          CALL EPMENRC
          if(izc.eq.icomp)goto 99
          if(izc.eq.0)return
          same=.false.
        elseif(ISCOPT.eq.2)then
          same=.true.
        elseif(ISCOPT.eq.3)then
          return
        endif

C Reset all surface lines in donor zone.
        if(.NOT.same)then
          call edisp(iuout,' updating current zone before copy...')
          call eclose(gversion(icomp),1.1,0.01,newgeo)
          if(.NOT.newgeo)then
            gversion(icomp) =1.1
            newgeo = .true.
          endif
          call geowrite(IFIL+1,LGEOM(ICOMP),ICOMP,iuout,3,IER)
          MODIFYVIEW=.TRUE.; MODBND=.TRUE.
          CALL INLNST(1)
          itsnm=0
          nzg=1
          nznog(1)=IZC   ! alter focus to the other zone
          izgfoc=ICOMP
          CALL redraw(IER)
          MODIFYVIEW=.TRUE.; MODBND=.TRUE.
        endif

C Use pickssur to find up to ilimit surfaces to copy, returning
C the selections via array ispk.
        CALL EDISP(iuout,'Please specify surface(s) to copy...')
        IS=1
        inpick=0
        if(same)then
          CALL EPMENSV
          ilimit=iposbcp
          ipkany=0
          call pickssur(icomp,inpick,'c',ispk,ipkany,ilimit,ier)
          CALL EPMENRC
          if(inpick.eq.0)then
            return
          endif
        else
          CALL EPMENSV
          ilimit=iposbcp
          ipkany=0
          call pickssur(izc,inpick,'c',ispk,ipkany,ilimit,ier)
          CALL EPMENRC
          if(inpick.eq.0)then
            return
          endif
        endif

C Consider what needs to be remembered to allow a connection association
C between the source surface and what that surface beomes after it is
C copied. Both zones will need to be updated at a point when the connection
C list is stable.

C Read the other zone and put selected surface data into temp storage.
        if(.NOT.same)then
          call georead(IFIL+1,LGEOM(IZC),IZC,0,iuout,IER)
        endif
        npick=1

  191   continue          ! Jump back point for processing subsequent surfaces.
        is=ispk(npick)    ! Index of surface to copy.
        iso=ispk(npick)    ! Index of surface we copied.
        if(same)then
          write(outs,'(2a)') ' Copying attributes of ',SNAME(ICOMP,IS)
        else
          write(outs,'(2a)') ' Copying attributes of ',SNAME(IZC,IS)
        endif
        call usrmsg(outs,' ','-')
        NVER1=NVER(IS)
        DO 142 IV=1,NVER1
          X1(iv)=X(JVN(IS,IV)); Y1(iv)=Y(JVN(IS,IV))
          Z1(iv)=Z(JVN(IS,IV))
  142   continue

C Generate a new surface name and default, then check to see if
C if is a duplicate. By definition, if we are copying a surface
C from the same zone we must adapt the name.
  141   if(same)then
          write(SNAMED,'(a,a)') 'x',SNAME(ICOMP,IS)(1:11)
          write(SNAME1,'(a,a)') 'x',SNAME(ICOMP,IS)(1:11)
        else
          write(SNAMED,'(a,a)') 'x',SNAME(IZC,IS)(1:11)
          write(SNAME1,'(a)') SNAME(IZC,IS)(1:12)
        endif
        icmpall=0 ! force compare of all surfaces
        call snamdup(SNAME1,icomp,icmpall,nameok)
        if(.NOT.nameok)then

C Copy to a slightly longer buffer for editing and ensure that
C the final name uses the allowed character set.
          helptopic='duplicate_surf_name'
          call gethelptext(helpinsub,helptopic,nbhelp)
          write(T14,'(2a)') SNAME1(1:12),'  '
          write(D14,'(2a)') SNAMED(1:12),'  '
          CALL EASKS(T14,'Revised surface name','(duplicate found)',
     &    14,D14,'surface name',IER,nbhelp)
          if(T14(1:2).NE.'  ')then
            write(SNAME1,'(a)') T14(1:12)
          else
            goto 141
          endif
        endif
        if(same)then              ! copy to temporary array from current zone
          SOTF1=SOTF(ICOMP,IS); SMLCN1=SMLCN(ICOMP,IS)
          SVFC1=SVFC(ICOMP,IS) 
          SUSE11=SUSE(ICOMP,IS,1); SUSE12=SUSE(ICOMP,IS,2)
        else                      ! copy to temporary array from other zone
          SOTF1=SOTF(IZC,IS); SMLCN1=SMLCN(IZC,IS)
          SVFC1=SVFC(IZC,IS)
          SUSE11=SUSE(IZC,IS,1); SUSE12=SUSE(IZC,IS,2)
        endif

C Ensure no NULL character strings.
        n=iachar(SMLCN1(1:1))
        if(n.eq.0) SMLCN1='UNKNOWN'
        n=iachar(SUSE11(1:1))
        if(n.eq.0) SUSE11='-'
        n=iachar(SUSE12(1:1))
        if(n.eq.0) SUSE12='-'
        n=iachar(SOTF1(1:1))
        if(n.eq.0) SOTF1='OPAQUE'

C Now re-read focus zone and copy the data into normal common
C blocks. Because the surface to be added is not yet in the
C connections list find ICON future position via `one connection 
C past the current last surface in the zone`.
        if(.NOT.same)then
          call georead(IFIL+1,LGEOM(ICOMP),ICOMP,0,iuout,IER)
        endif

C Up the surface count.
        ICON=IZSTOCN(icomp,nsur)+1
        NSUR=NSUR+1
        NZSUR(ICOMP)=NSUR
        NZTV(ICOMP)=NTV
        NVER(NSUR)=NVER1
        isznver(ICOMP,NSUR)=NVER1
        SNAME(ICOMP,NSUR)=SNAME1
        SOTF(ICOMP,NSUR)=SOTF1
        SMLCN(ICOMP,NSUR)=SMLCN1
        SVFC(ICOMP,NSUR)=SVFC1
        SOTHER(ICOMP,NSUR,1)='UNKNOWN'
        SOTHER(ICOMP,NSUR,2)='-'
        SOTHER(ICOMP,NSUR,3)='-'
        SUSE(ICOMP,NSUR,1)=SUSE11
        SUSE(ICOMP,NSUR,2)=SUSE12
        SPARENT(ICOMP,NSUR)='-'

C Generate combined name for dialog.
        ZSDES=' '
        WRITE(ZSDES,'(a,a1,a)') SNAME1(1:lnblnk(SNAME1)),':',
     &    ZNAME(icomp)(1:lnzname(icomp))

        helptopic='copied_surf_actions'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(.NOT.DDOK)then
          IRT=1
          CALL EASKATOG(' ','Actions to take on the new surface:',
     &      'shift along normal','transform xyz','rotate','invert',
     &      'combination','continue ?',' ',IRT,nbhelp)
        endif
        if(IRT.eq.5)then
          if(.NOT.DDOK)then

C Create string buffer for variables with space at the end.
            hold64='                                                  '
            write(hold64,'(f9.4,2x,3f9.3,i4,f9.3,a)') vdis,valx,valy,
     &        valz,irv,angr,'   '
 552        CALL EASKS(hold64,
     &'Distance along normal, X Y & Z offsets, vertex, rotation angle:',
     &  ' ',64,' 0.0  0.0  0.0 0. 1  0.0  ','combined transforms',IER,
     &  nbhelp)
            if(ier.ne.0)goto 552
            K=0
            CALL EGETWR(HOLD64,K,Vdis,-9.0,9.0,'W','normal distance',
     &        IER)
            CALL EGETWR(HOLD64,K,VALX,-50.0,50.0,'W','X off',IER)
            CALL EGETWR(HOLD64,K,VALY,-50.0,50.0,'W','Y off',IER)
            CALL EGETWR(HOLD64,K,VALZ,-50.0,50.0,'W','Z off',IER)
            CALL EGETWI(HOLD64,K,irv,0,nver1,'W','vertex index',IER)
            CALL EGETWR(HOLD64,K,angr,-359.0,359.0,'W','rotation ang',
     &        IER)
          endif

C Do each of the transforms in order.
          CALL TRANSUR(ITRC,iuout,NVER1,X1,Y1,Z1,vdis,XT,YT,ZT,ZSDES)
          do 551 nt1=1,NVER1
            X1(nt1)=XT(nt1); Y1(nt1)=YT(nt1); Z1(nt1)=ZT(nt1)
 551      continue

          DO 553 I=1,NVER1
            X1(I)=X1(I)+VALX; Y1(I)=Y1(I)+VALY; Z1(I)=Z1(I)+VALZ
 553      continue

          if(ANGR.LT.-.01.OR.ANGR.GT..01)then
            PI = 4.0 * ATAN(1.0); A=-ANGR*PI/180.
            CA=COS(A); SA=SIN(A)
            XX1=X1(IRV); YY1=Y1(IRV)
            DO 500 I=1,NVER1
              XXX=X1(I)-XX1; YYY=Y1(I)-YY1
              XR=XXX*CA+YYY*SA; YR=YYY*CA-XXX*SA
              X1(I)=XR+XX1; Y1(I)=YR+YY1
  500       continue
          endif
        endif

C User selected only one transform to make. Process it.
        if(IRT.eq.1)then

C Shift along normal and copy transformed points back to X1,Y1,Z1 array.
          if(.NOT.DDOK)then
            vdis=0.0
            CALL EASKR(vdis,' Distance along the normal (metres): ',
     &        ' : ',-9.999,'F',9.999,'W',0.0,'d along normal',IER,
     &        nbhelp)
          endif
          CALL TRANSUR(ITRC,iuout,NVER1,X1,Y1,Z1,vdis,XT,YT,ZT,ZSDES)
          do 151 nt1=1,NVER1
            X1(nt1)=XT(nt1); Y1(nt1)=YT(nt1); Z1(nt1)=ZT(nt1)
 151      continue
        elseif(IRT.eq.2)then

C Transform all surface vertices.
          if(.NOT.DDOK)then
            HOLDS= '  0.0   0.0   0.0    '
 152        CALL EASKS(HOLDS,' X Y & Z offsets: ',' ',
     &        36,'  0.0   0.0   0.0   ','offsets',IER,nbhelp)
            K=0
            CALL EGETWR(HOLDS,K,VALX,-50.0,50.0,'W','X off',IER)
            CALL EGETWR(HOLDS,K,VALY,-50.0,50.0,'W','Y off',IER)
            CALL EGETWR(HOLDS,K,VALZ,-50.0,50.0,'W','Z off',IER)
            if(ier.ne.0)goto 152
          endif
          DO 153 I=1,NVER1
            X1(I)=X1(I)+VALX; Y1(I)=Y1(I)+VALY; Z1(I)=Z1(I)+VALZ
 153      continue
        elseif(IRT.eq.3)then

C Rotate around a point in the surface.
          if(.NOT.DDOK)then
            CALL EASKI(IRV,'Rotate about which vertex index: ',' ',
     &        1,'F',NVER1,'F',1,'rotation anchor',IERI,nbhelp)
            if(ieri.eq.-3)then
              goto 166
            endif
            ANGR=0.
            CALL EASKR(ANGR,' ',' Rotation (deg + = anticlockwise) ?',
     &       -359.0,'W',359.0,'W',0.0,'rotation',IER,nbhelp)
          endif
          if(ANGR.LT.-.01.OR.ANGR.GT..01)then
            PI = 4.0 * ATAN(1.0); A=-ANGR*PI/180.
            CA=COS(A); SA=SIN(A)
            XX1=X1(IRV); YY1=Y1(IRV)
            DO 10 I=1,NVER1
              XXX=X1(I)-XX1; YYY=Y1(I)-YY1
              XR=XXX*CA+YYY*SA; YR=YYY*CA-XXX*SA
              X1(I)=XR+XX1; Y1(I)=YR+YY1
   10       continue
          endif
        endif

C After transforms, append new surface vertices to zone list if
C they are unique, use CACC for auto otherwise check with user 
C if <10mm, otherwise link the new surface to an existing vetex.
  166   continue
        do 143 ix = 1,NVER1
          if(NTV+1.le.MTV)then
            found=.false.
            do 144 ixx = 1,NTV
              call eclose3(X1(ix),Y1(ix),Z1(ix),X(ixx),Y(ixx),Z(ixx),
     &          CACC,closex)   ! if within CACC assume the same
              call eclose3(X1(ix),Y1(ix),Z1(ix),X(ixx),Y(ixx),Z(ixx),
     &          CACC*2,closey)
              dist=crowxyz(X1(ix),Y1(ix),Z1(ix),X(ixx),Y(ixx),Z(ixx))
              if(closex)then
                found=.true.  ! close enough to auto match
                match=ixx
                goto 145
              endif
              if(closey)then

C Feedback to the user about using existing vertex.
                write(outs,'(a,i3,a,3f8.3,a,f7.4,a,i3,a,3f8.3)')
     &            'New vertex ',ix,' @',X1(ix),Y1(ix),Z1(ix),
     &            ' is close (',dist,') to existing vertex',iix,' @',
     &            X(ixx),Y(ixx),Z(ixx)
                call edisp(iuout,outs)
                call easkok(' ',
     &            'Use existing vertex instead of the new one?',
     &            found,nbhelp)
                if(found)then
                  match=ixx
                  goto 145
                endif
              endif
  144       continue

C If an existing vertex is close then reference it otherwise
C add a new vertex to the end of the zone list.
  145       if(found)then
              JVN(NSUR,ix)=match
              iszjvn(icomp,nsur,ix)=match
            else
              NTV=NTV+1
              NZTV(icomp)=NTV
              JVN(NSUR,ix)=NTV
              iszjvn(icomp,nsur,ix)=NTV
              X(NTV)=X1(ix); Y(NTV)=Y1(ix); Z(NTV)=Z1(ix)
              szcoords(ICOMP,ntv,1)=X1(ix)
              szcoords(ICOMP,ntv,2)=Y1(ix)
              szcoords(ICOMP,ntv,3)=Z1(ix)
              XMN=AMIN1(XMN,X(NTV)); YMN=AMIN1(YMN,Y(NTV))
              ZMN=AMIN1(ZMN,Z(NTV)); XMX=AMAX1(XMX,X(NTV))
              YMX=AMAX1(YMX,Y(NTV)); ZMX=AMAX1(ZMX,Z(NTV))

C For a unique vertex also test to see if it is close (+-CACC) to
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

C If iwhich1 & iwhich2 are the same skip this zero length line.
                  if(iwhich1.eq.iwhich2) goto 247

C If either iwhich1 & iwhich2 are zero skip this zero length line.
                  if(iwhich1.eq.0.or.iwhich2.eq.0) goto 247

C Double check that the line from iwhich1,iwhich2 is > 0.1m. If so
C it can be considered for inserting new vertex into.
                  xitem=X(iwhich1); yitem=Y(iwhich1); zitem=Z(iwhich1)
                  xpitem=X(iwhich2);ypitem=Y(iwhich2);zpitem=Z(iwhich2)
                  vdis= crowxyz(xitem,yitem,zitem,xpitem,ypitem,zpitem)
                  if(vdis.lt.0.1)goto 247

C Report length of line. Use method of Ward/Radiance in fvect.c
                  call pointtoline(iwhich3,iwhich1,iwhich2,offset,
     &              matchver)
                  if(.NOT.matchver) goto 247
                  if(offset.le.CACC)then
                    write(outs,'(a,i3,a,3f8.3,a,f6.4,a,i3,a,i3,2a)')
     &                'New vertex ',iwhich3,' @',
     &                X(NTV),Y(NTV),Z(NTV),' is close (',offset,
     &                ') to edge ',IWHICH1,' & ',IWHICH2,
     &                ' of surface ',SNAME(ICOMP,ivj)
                    call edisp(iuout,outs)

C If current surface (ivj) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JVN indices up one until at the current edge (ivjj) and then
C inserting the new vertex index.
                    if(NVER(ivj)+1.le.MV)then
                      if(offset.le.CACC)then
                        ok=.true.
                      else
                        helptopic='vertex_close_edge'
                        call gethelptext(helpinsub,helptopic,nbhelp)
                        call easkok(' ',
     &                  'Insert this vertex into the adjacent surface?',
     &                  ok,nbhelp)
                      endif
                    else
                      ok=.false.
                    endif
                    if(ok)then
                      NVER(ivj)=NVER(ivj)+1
                      isznver(icomp,ivj)=NVER(ivj)
                      IXV=NVER(ivj)+1
  148                 continue
                      IXV=IXV-1
                      JVN(ivj,IXV)=JVN(ivj,IXV-1)
                      iszjvn(icomp,ivj,IXV)=jvn(ivj,IXV)
                      IF(IXV.GT.ivjj+1)GOTO 148
                      JVN(ivj,ivjj+1)=iwhich3
                      iszjvn(icomp,ivj,ivjj+1)=iwhich3

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
          helptopic='copied_surf_actions'
          call gethelptext(helpinsub,helptopic,nbhelp)
          write(outs,'(3a)') ' surface ',SNAME1(1:lnblnk(SNAME1)),
     &                       ' (yes for most partitions) ?'
          if(.NOT.DDOK)then
            if(IRT.eq.4)then
              IOK=1
            else
              CALL EASKAB(' Reverse the edge ordering of',
     &                  outs,'yes','no',IOK,nbhelp)
            endif
          endif
          if(IOK.eq.1)then
            do 146, iyy = 1,NVER(NSUR)
              jvn1(iyy)=JVN(NSUR,iyy)
  146       CONTINUE
            JVN(NSUR,1)=jvn1(2)
            iszjvn(icomp,NSUR,1)=jvn1(2)
            JVN(NSUR,2)=jvn1(1)
            iszjvn(icomp,NSUR,2)=jvn1(1)
            do 147, iyy = 3,NVER(NSUR)
              izz=NVER(NSUR)+3-iyy
              JVN(NSUR,iyy)=jvn1(izz)
              iszjvn(icomp,NSUR,iyy)=jvn1(izz)
  147       CONTINUE

C If construction is symmetric then do nothing but if it
C references a non-symetric MLC then use an inverted MLC.
            call matchmlcdesc(SMLCN(icomp,NSUR),imlcindex)
            if(imlcindex.ne.0)then
              ii=imlcindex
              lnssmlc=lnblnk(SMLCN(ICOMP,NSUR))
              if(mlcsymetric(ii)(1:9).EQ.'SYMMETRIC')then
                if(SMLCN(ICOMP,NSUR)(1:lnssmlc).eq.
     &             mlcname(ii)(1:lnmlcname(ii)))then
                  SMLCN(ICOMP,NSUR)=mlcname(ii)
                elseif(SMLCN(ICOMP,NSUR)(1:4).eq.'UNKN')then
                  SMLCN(ICOMP,NSUR)=mlcname(ii)
                else
                  SMLCN(ICOMP,NSUR)=mlcname(ii)
                endif
                iissmlci=ii
              elseif(mlcsymetric(ii)(1:12).EQ.'NONSYMMETRIC')then

C If the current construction is non-symmetric then it should not be
C used for a partition (if db does not have a linked MLC, inform the user).
                SMLCN(ICOMP,NSUR)=mlcsymetric(ii)
                iissmlci=matsymindex(ii)  ! use returned value
              else
                lnssmlc=lnblnk(SMLCN(ICOMP,NSUR))
                if(SMLCN(ICOMP,NSUR)(1:4).eq.'UNKN')then
                  SMLCN(ICOMP,NSUR)=mlcsymetric(ii)
                elseif(SMLCN(ICOMP,NSUR)(1:lnssmlc).eq.
     &                 mlcsymetric(ii)(1:lnssmlc))then
                  SMLCN(ICOMP,NSUR)=mlcsymetric(ii)
                else
                  SMLCN(ICOMP,NSUR)=mlcsymetric(ii)
                endif
                iissmlci=matsymindex(ii)       ! use returned value
              endif
              if(iissmlci.ne.0)then
                smlcindex(ICOMP,NSUR)=iissmlci ! update
              endif
            endif
          endif
        endif

C Update the connection list. Move all others up and then insert with
C default values (allow moderate trace (3rd parameter in addedsurf).
        call addedsurf(icomp,icon,1,ier)

        call updatesvfc(icomp,modgeo)  ! update SVFC orientation string.

C If more than one surface to copy save the updated zone before
C copying the next surface.
        if(npick.le.inpick)then
          if(npick.lt.inpick)then
            npick=npick+1
            done=.false.
          else
            done=.true.
          endif
          if(.NOT.same)then
            call edisp(iuout,' updating current zone...')
            call eclose(gversion(ICOMP),1.1,0.01,newgeo)
            if(.NOT.newgeo)then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            call geowrite(IFIL+1,LGEOM(ICOMP),ICOMP,iuout,3,IER)

C Ask user if they want to make a thermophysical connection with the
C new surface and the source in the other zone. ICOMP & NSUR & ICON
C are associate with the new focus surface and IZC,ISO in the source zone.
            if(.NOT.same)then
              lnsn1=lnblnk(sname(icomp,nsur))
              lnsn2=lnblnk(sname(izc,iso))
              write(outs,'(9a)') 
     &          'Make thermophysical connection between ',
     &          sname(icomp,nsur)(1:lnsn1),':',zname(icomp),' & ',
     &          sname(izc,iso)(1:lnsn2),':',zname(izc),'?'
              updoth=.false.

C Bookkeepping is off. 
              CALL EASKOK(' ',outs,updoth,nbhelp)
              if(updoth)then

C Update the new surface to be a partition.
                ICT(ICON)=3; IC2(ICON)=IZC; IE2(ICON)=ISO
                SOTHER(icomp,nsur,1)='ANOTHER'
                write(SOTHER(icomp,nsur,2),'(i3.3)') IZC
                write(SOTHER(icomp,nsur,3),'(i3.3)') ISO
                write(SN,'(a12)') SNAME(ICOMP,nsur)
                call geowrite(IFIL+1,LGEOM(ICOMP),ICOMP,iuout,3,IER)

C Reflect changes in the source zone.
                ICOTH=IZSTOCN(IZC,ISO)
                ICT(ICOTH)=3; IC2(ICOTH)=ICOMP; IE2(ICOTH)=NZSUR(ICOMP)
                SOTHER(izc,iso,1)='ANOTHER'
                write(SOTHER(izc,iso,2),'(i3.3)') ICOMP
                write(SOTHER(izc,iso,3),'(i3.3)') NSUR
                TSOTH1='ANOTHER'
                write(TSOTH2,'(i3.3)') ICOMP
                write(TSOTH3,'(i3.3)') NSUR

                call reconcileattributes(icomp,nsur,SN,izc,iso,
     &            showother,updoth,TOSMLCN,TOOPT,TOUSE1,TOUSE2,
     &            IISSMLCI)

C Update the other zone surface attributes.
                write(SN,'(a12)') SNAME(izc,iso)
                lnicoth=lnblnk(SMLCN(izc,iso))
                write(outs,'(6a)')
     &            'Updating `other side` composition of ',
     &            SN(1:lnblnk(SN)),' from ',
     &            SMLCN(izc,iso)(1:lnicoth),' > ',TOSMLCN
                call edisp(iuout,outs)
                CALL ESCZONE(ICOMP)
                call eclose(gversion(izc),1.1,0.01,newgeo)
                if(newgeo)then
                  call georead(IFIL+2,LGEOM(izc),izc,0,iuout,IER)
                else
                  call egomin(IFIL+2,LGEOM(izc),izc,0,0,iuout,IER)
                endif

C Assign new MLC name to the icoth surface as well as SOTF with the
C optical properties and MLC index from material.h commons.
                if(iissmlci.ne.0)then
                  SMLCN(izc,iso)=TOSMLCN
                  smlcindex(izc,iso)=iissmlci  ! update
                endif
                if(TOOPT(1:2).ne.' ')then
                  write(SOTF(izc,iso),'(a)') TOOPT
                endif
                SUSE(izc,iso,1)=TOUSE1
                SUSE(izc,iso,2)=TOUSE2
                SOTHER(izc,iso,1)='ANOTHER'
                write(SOTHER(izc,iso,2),'(i3.3)') ICOMP
                write(SOTHER(izc,iso,3),'(i3.3)') NZSUR(ICOMP)
                SOTHER(izc,iso,1)=TSOTH1
                SOTHER(izc,iso,2)=TSOTH2
                SOTHER(izc,iso,3)=TSOTH3
                ICOTH=IZSTOCN(IZC,ISO)
                ICT(ICOTH)=3; IC2(ICOTH)=ICOMP; IE2(ICOTH)=NZSUR(ICOMP)

                call eclose(gversion(izc),1.1,0.01,newgeo)
                if(.NOT.newgeo)then
                  gversion(izc) =1.1
                  newgeo = .true.
                endif
                call geowrite(IFIL+2,LGEOM(izc),izc,iuout,3,IER)
                IF(IER.NE.0)CALL USRMSG(
     &            'Problem updating other surface attribute... ',
     &            '(could not write other geometry file).','W')

                call usrmsg(' Updating other side...done.',' ','-')
                CALL ERCZONE(ICOMP)
                CALL EMKCFG('-',IER)  ! update connections file.
                updoth=.false.
              endif
            endif

C If there are more surfaces to process rescan the source zone
C before looping back to process the next surface.
            if(.NOT.done)then
              call usrmsg('Getting donor zone info...',' ','-')
              call eclose(gversion(IZC),1.1,0.01,newgeo)
              call georead(IFIL+1,LGEOM(IZC),IZC,0,iuout,IER)
              write(outs,'(3a)') ' Repeat transforms for ',
     &          SNAME(IZC,ispk(npick)),' ?'
            endif
          else
            call usrmsg(' Updating current zone...',' ','-')
            call eclose(gversion(icomp),1.1,0.01,newgeo)
            if(.NOT.newgeo)then
              gversion(icomp) =1.1
              newgeo = .true.
            endif
            call geowrite(IFIL+1,LGEOM(ICOMP),ICOMP,iuout,3,IER)
            write(outs,'(3a)') 'Repeat transforms for ',
     &       SNAME(ICOMP,ispk(npick)),'?'
          endif

C Set DDOK to control invocation of transforms in the subsequent surface.
          if(inpick.gt.1)then
            CALL EASKOK(' ',outs,DDOK,nbhelp)
          endif
          if(done)then
            continue    ! All of the surfaces have been processed.
          else
            goto 191    ! Jump back and process next surface.
          endif
        endif
        MODBND=.TRUE.
        MODIFYVIEW=.TRUE.
        
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

C Allow user to type in the list of vertices as a string.  The
C number of items is the number of vertices and then parse out
C the individual vertex numbers from the list.
   41   HOLD='                                                     '
        helptopic='create_new_surface'
        call gethelptext(helpinsub,helptopic,nbhelp)
        if(MMOD.ne.8)then
          HOLD='                                                  '
          CALL EASKS(HOLD,
     &     ' Give vertices (order anticlockwise viewed from outside)',
     &     '  ',96,' ','associated vertices',IER,nbhelp)
          NV = iCountWords(HOLD)
          K=0
          DO 94 J=1,NV
            CALL EGETWI(HOLD,K,JV,1,NTV,'W','vertex list',IER)
            JVN(NSUR,J)=JV
            iszjvn(ICOMP,NSUR,J)=JV
   94     CONTINUE
          NVER(NSUR)=NV
          isznver(ICOMP,NSUR)=NV
        else

C If user did not explicitly specify that the mouse should be used
C present editing of vertices with the option for mouse.
          iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
          if(iglib.eq.1)then
            if(ISCOPT.eq.0)then  
              hold64='                                                '
              write(t248,'(3a)') hold64,hold64,hold64
              write(message,'(a)')'Vertices associated with new surface'
              ISTRW=72
              CALL EASKS248(t248,message,' ',
     &          ISTRW,' 1  2  3  4  ','associated vertices',IER,nbhelp)
              NV = iCountWords(t248)
              K=0
              DO 943 J=1,NV
                CALL EGETWI(t248,K,JV,1,NTV,'W','vertex list',IER)
                JVN(NSUR,J)=JV
                iszjvn(ICOMP,NSUR,J)=JV
  943         CONTINUE
              NVER(NSUR)=NV
              isznver(ICOMP,NSUR)=NV
            elseif(ISCOPT.eq.1)then

C User has specified clicking from mouse so just do that.
              clkok=.true.
            endif
            if(clkok)then
              ijvn=0
              call edisp(iuout,
     &          'Select vertices with the mouse.')
              call edisp(iuout,
     &          'Press e to finish, or v to reposition the view.')
  46          CALL trackview(iik,iixx,iiyy)
              if(iik.eq.69.or.iik.eq.101)goto 47 ! pressed e or numpad 5
              if (iik.eq.118) then ! pressed v
                call tmpmenu
                goto 46
              endif
              found=.false.
              do 45 i=1,NTV
                COG1(1)=X(I); COG1(2)=Y(I); COG1(3)=Z(I)
                CALL VECTRN(COG1,TSMAT,COG2,IER)
                call u2pixel(COG2(1),COG2(2),iix,iiy)
                ixd=iix-iixx
                iyd=iiy-iiyy
C Debug
C               write(6,'(10i5)') i,iik,iixx,iiyy,ixx,iyy,iix,iiy,ixd,iyd
                if(abs(ixd).lt.5.and.abs(iyd).lt.5)then
                  if(found)then
                    call edisp(iuout,'Close points, try again.')
                    goto 46
                  endif
                  WRITE(outs,'(a,i5,a,i5,a,i3)')' The point @ x=',iixx,
     &              ' & y=',iiyy,' matches vertex ',i
                  call edisp(iuout,outs)
                  found=.true.
                  ijvn=ijvn+1
                  jjvn(ijvn)=i
C                  write(6,*) ijvn,i,jjvn(ijvn)

C Highight the matching vertex.
                  iicol=0
                  if(greyok)call winscl('z',iicol)
                  call esymbol(iix,iiy,24,1)
                  iicol=0
                  if(greyok)call winscl('-',iicol)
                  call forceflush()
                  goto 46   ! wait for another click or `e`
                endif
  45          continue
              if(.NOT.found)goto 46
  47          if(ijvn.lt.3)then
                call edisp(iuout,'Not enough points to make a surface.')
                goto 41
              endif

C Clear the string buffer, write into it the list of vertices that were
C selected by the user so that errors and duplicates can be corrected.
C If the user was clicking with a mouse then they might have duplicated
C some vertices and thus be beyond the normal limit of edges. If MV is
C updated reflect this in the write(t248 statement).
              hold64='                                                '
              write(t248,'(3a)') hold64,hold64,hold64
              WRITE(t248,'(124I4)')(JJVN(J),J=1,ijvn)
              write(message,'(a)')'Vertices associated with new surface'
              ISTRW=72
              CALL EASKS248(t248,message,' ',
     &          ISTRW,' 1  2  3  4  ','associated vertices',IER,nbhelp)
              NV = iCountWords(t248)
              K=0
              DO 945 J=1,NV
                CALL EGETWI(t248,K,JV,1,NTV,'W','vertex list',IER)
                JVN(NSUR,J)=JV
                iszjvn(ICOMP,NSUR,J)=JV
  945         CONTINUE
              NVER(NSUR)=NV
              isznver(ICOMP,NSUR)=NV
            endif
          elseif(iglib.eq.2)then
         
C For the GTK version there is no mouse click so offer a blank editing
C string. Fill the string first with spaces and a single character (GTK
C does not seem to like to start with a fully blank string).
            t248='  1                                               '
            write(message,'(a)') 'Vertices associated with new surface'
            ISTRW=72
            CALL EASKS248(t248,message,' ',
     &        ISTRW,' 1  2  3  4  ','associated vertices',IER,nbhelp)
            NV = iCountWords(t248)
            K=0
            DO 944 J=1,NV
              CALL EGETWI(t248,K,JV,1,NTV,'W','vertex list',IER)
              JVN(NSUR,J)=JV
              iszjvn(ICOMP,NSUR,J)=JV
  944       CONTINUE
            NVER(NSUR)=NV
            isznver(ICOMP,NSUR)=NV
          endif
        endif

C Fill default attributes. Edit with a 14 char wide string buffer
C and then copy to 12 char string (SN) and then use st2name to
C get rid of blanks and non-printable characters and then use
C snamdup to ensure that it is a unique name (in this zone).
        CALL FILSUR(ICOMP,NSUR)
        helptopic='new_surface_name'
        call gethelptext(helpinsub,helptopic,nbhelp)
        T14=' '
        write(T14,'(2a)') SNAME(ICOMP,NSUR),'  '
 52     CALL EASKS(T14,'Surface name','(unique word <=12char):',
     &      14,'new_surf','surface name',IER,nbhelp)
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

C Set context for the use of the surface SUSE.
C << subroutine to use guesstype needs to be added as in INSREC >>
C        guesstype='any_type'

C Update the connection list. Move all others up and then insert with
C default values. (allow verbose trace (3rd parameter in addedsurf).
        ltrace=1
        call addedsurf(icomp,icon,ltrace,ier)
        call updatesvfc(icomp,modgeo)  ! update SVFC orientation string.
        MODIFYVIEW=.TRUE.
      endif

      RETURN
      END

C ************* ADDMASS 
C ADDMASS: Add a horizontal or vertical rectangular mass in zone.
C Parameter passed:
C ACT (2 char) to signal vertical mass 'VM', vertical single 'VS'
C   horizontal mass 'HM' or horizontal single 'HS'.
C MODE (1 char) passed `s` is silent (no graphic feedback) or 'g' graphic
C   feedback.
C VALOX,VALOY,VALOZ is the XYZ of the origin of the surface.
C AANG is the azimuth in degrees.
C DDX, DDZ length and width of rectangle.
C sn (12 char) surface name
C constr (32 char) the name of the construction for then new surface(s).
C opt (12 char) the name of the optical set for the new surface(s).
C IER is zero then no issue during creation, IER is -1 then user asked
C   for new position, IER is -3 user aborted action.
      SUBROUTINE ADDMASS(ICOMP,ACT,MODE,VALOX,VALOY,VALOZ,
     &   AANG,DDX,DDZ,SN,constr,OPT,IER)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

      character constr*32,act*2,mode*1,OPT*12
      character outs*124,SN*12

      DIMENSION  XXW(4),YYW(4),ZZW(4)  ! for the initial surface
      DIMENSION  XXM(4),YYM(4),ZZM(4)  ! for matching surface
      DIMENSION  JVNM(4)               ! for matching surface
      DIMENSION  AX(MPL),AY(MPL),AZ(MPL),IANXT(MPL)
      DIMENSION  BX(MPL),BY(MPL),BZ(MPL),IBNXT(MPL)
      DIMENSION  SBBOX(3,2)
      logical newgeo  ! to use for testing if new/old geometry file.
      logical modgeo  ! true if SVFC updated

#ifdef OSI
      integer iupdown,isym    ! passed to etplot
#else
      integer*8 iupdown,isym    ! passed to etplot
#endif

      helpinsub='insert'  ! set for subroutine

C Set first coord at origin (lower left).
      XXW(1) = VALOX; YYW(1) = VALOY; ZZW(1) = VALOZ

C Set PI and RAD.
      PI = 4.0 * ATAN(1.0)
      RAD = PI/180.

      if(act(1:1).eq.'V'.or.act(1:1).eq.'v')then

C Set second coord along DDX metres at AANG-90.
        RYAZI = (AANG-90.0)*RAD
        RSALT = 0.00
        z3 = DDX*SIN(RSALT)
        XYDIS = DDX*COS(RSALT)
        IF (ABS(XYDIS) .LT. 1E-6)THEN
          x3 = 0.
          y3 = 0.
        ELSE
          x3 = XYDIS*SIN(RYAZI)
          y3 = XYDIS*COS(RYAZI)
        ENDIF
        XXW(2)=x3+XXW(1); YYW(2)=y3+YYW(1); ZZW(2)=z3+ZZW(1)

C Third point is as second with Z adjusted. Fourth point is as first with
C Z adjusted.
        XXW(3)=XXW(2); YYW(3)=YYW(2); ZZW(3)=ZZW(2)+DDZ
        XXW(4)=XXW(1); YYW(4)=YYW(1); ZZW(4)=ZZW(1)+DDZ

      elseif(act(1:1).eq.'H'.or.act(1:1).eq.'h')then
        XXW(2) = VALOX+DDX; YYW(2) = VALOY; ZZW(2) = VALOZ
        XXW(3) = VALOX+DDX; YYW(3) = VALOY+DDZ; ZZW(3) = VALOZ
        XXW(4) = VALOX; YYW(4) = VALOY+DDZ; ZZW(4) = VALOZ

C Now rotate around the first coordinate.
        if(AANG.LT.-.01.OR.AANG.GT..01)then
          A=-AANG*PI/180.
          CA=COS(A); SA=SIN(A)
          XX1=XXW(1); YY1=YYW(1)
          DO 200 I=1,4
            XXX=XXW(I)-XX1
            YYY=YYW(I)-YY1
            XR=XXX*CA+YYY*SA
            YR=YYY*CA-XXX*SA
            XXW(I)=XR+XX1
            YYW(I)=YR+YY1
  200     continue
        endif

      endif

C Copy vertices in inverse order for matching surface.
      XXM(1) = XXW(2); YYM(1) = YYW(2); ZZM(1) = ZZW(2)
      XXM(2) = XXW(1); YYM(2) = YYW(1); ZZM(2) = ZZW(1)
      XXM(3) = XXW(4); YYM(3) = YYW(4); ZZM(3) = ZZW(4)
      XXM(4) = XXW(3); YYM(4) = YYW(3); ZZM(4) = ZZW(3)

C I mode is 'g' then draw the new surface.
      if(mode(1:1).eq.'G'.or.mode(1:1).eq.'g')then

C Display the new vertical (irt=6) or horizontal (irt=7) rectangle.
        if(MMOD.eq.8)then
          IAPNT = 1
          DO 350 J = 1,4
            AX(J) = XXW(J); AY(J) = YYW(J); AZ(J) = ZZW(J)
            IANXT(J) = J + 1
  350     CONTINUE
          IANXT(4) = IAPNT
          CALL MATPOL(4,IAPNT,AX,AY,AZ,IANXT,TSMAT,
     &              SBBOX,NBP,IBPNT,BX,BY,BZ,IBNXT,IERR)
          CALL CLIPFL(NBP,BX,BY,BZ,ISTAT)
          IF(ISTAT .EQ. 1)goto 32
          if(ISTAT .EQ.-1)then
            CALL CUTPOL(NB,NBP,IBPNT,BX,BY,BZ,IBNXT,ISTAT)
          else
            NB=1
          endif

C Draw the (perhaps clipped) rectangle.
          DO 302 JB = 1,NB
            IP = IABS(IBPNT)
            iupdown=0
            isym=0
            call etplot(BX(IP),BY(IP),iupdown,isym)

C Draw succeeding points until last vertex.
            IP1 = IBNXT(IP)
 451        CONTINUE
            iupdown=1
            isym=0
            call etplot(BX(IP1),BY(IP1),iupdown,isym)

            IP1 = IBNXT(IP1)
            IF(IP1 .NE. IP) GOTO 451

C Complete rec.
            iupdown=1
            isym=0
            call etplot(BX(IP),BY(IP),iupdown,isym)
  302     CONTINUE
        endif   ! graphic mode test

   32   CONTINUE
        CALL EDISP(iuout,
     &    ' New coords: (lower left, lower right, up right, up left')
        WRITE(OUTS,'(a,4F8.3)') ' X coords:',(XXW(I),I=1,4)
        CALL EDISP(iuout,OUTS)
        WRITE(OUTS,'(a,4F8.3)') ' Y coords:',(YYW(I),I=1,4)
        CALL EDISP(iuout,OUTS)
        WRITE(OUTS,'(a,4F8.3)') ' Z coords:',(ZZW(I),I=1,4)
        CALL EDISP(iuout,OUTS)

C Confirm opening.
        helptopic='addmass_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKABC('Options for this rectangle:',' ',
     &     'accept position','revise position','abort',INVT,nbhelp)
        if(INVT.eq.2)then
          ITVNO=0
          ITSNM=0
          MODIFYVIEW=.TRUE.
          CALL INLNST(1)
          nzg=1
          nznog(1)=ICOMP
          izgfoc=ICOMP
          CALL redraw(IER)
          ier=-1
          return
        elseif(INVT.eq.3)then
          ier=-3
          return
        endif

      else
        continue
      endif

C Now update the appropriate common data for the new surface. The
C following code is similar to that in addsur in edzone.f.
C ICONT is the connection for the surface. Because the surface 
C to be added is not yet in the connections list find ICONT
C its future position via `one connection past the current last 
      ICONT=IZSTOCN(icomp,nsur)+1
      NZSUR(ICOMP)=NZSUR(ICOMP)+1
      NSUR=NSUR+1
      IZSTOCN(icomp,nsur)=icont  ! so connection will be known
      NVER(NSUR)=4
      isznver(ICOMP,NSUR)=4
      DO 251 J = 1,4
        X(NTV+J)=XXW(J); Y(NTV+J)=YYW(J); Z(NTV+J)=ZZW(J)
        JVN(NSUR,J)=NTV+J
        iszjvn(ICOMP,NSUR,J)=NTV+J
        szcoords(ICOMP,ntv+J,1)=XXW(J)
        szcoords(ICOMP,ntv+J,2)=YYW(J)
        szcoords(ICOMP,ntv+J,3)=ZZW(J)
  251 CONTINUE

C Remember the reverse order of vertices for matching surface.
      JVNM(1)= JVN(NSUR,2)
      JVNM(2)= JVN(NSUR,1)
      JVNM(3)= JVN(NSUR,4)
      JVNM(4)= JVN(NSUR,3)
      NTV=NTV+4
      NZTV(icomp)=NTV

      call st2name(SN,SNAME(ICOMP,NSUR))  ! copy SN into SNAME

      if(act(1:1).eq.'V'.or.act(1:1).eq.'v')then
        SVFC(ICOMP,NSUR)='VERT'
      elseif(act(1:1).eq.'H'.or.act(1:1).eq.'h')then
        SVFC(ICOMP,NSUR)='CEIL'
      endif

      SOTHER(ICOMP,NSUR,1)='UNKNOWN'
      SOTHER(ICOMP,NSUR,2)='0'
      SOTHER(ICOMP,NSUR,3)='0'
      SPARENT(ICOMP,NSUR)='-'

      WRITE(SMLCN(ICOMP,NSUR),'(A)')constr
      write(SOTF(ICOMP,NSUR),'(a)') OPT(1:12)

C If version 1.1 of geometry specify the use of the surface.
      call eclose(gversion(icomp),1.1,0.01,newgeo)
      if(newgeo)then
        SUSE(ICOMP,nsur,1)='PARTN'
        SUSE(ICOMP,nsur,2)='- '
      else
        SUSE(ICOMP,NSUR,1)='-'
        SUSE(ICOMP,NSUR,2)='-'
      endif

C Just before calling addedsurf if this initial surface is going to be
C matched then set sother to point to the next surface that will be created.
      if(act(2:2).eq.'M'.or.act(2:2).eq.'m')then
        SOTHER(ICOMP,NSUR,1)='ANOTHER'
        write(SOTHER(ICOMP,NSUR,2),'(i2.2)')INT(icomp)
        write(SOTHER(ICOMP,NSUR,3),'(i3.3)')INT(NSUR+1)
      endif

C Add surface to the connection list (icont) assuming an unknown boundary. 
      call addedsurf(icomp,icont,1,ier)
      call updatesvfc(icomp,modgeo)  ! update SVFC orientation string.

C If user requested mass be added then generate coordinates and
C attributes for the matching surface.
      if(act(2:2).eq.'M'.or.act(2:2).eq.'m')then

C Update the data for the matching surface.
        icontm=IZSTOCN(icomp,nsur)+1
        NZSUR(ICOMP)=NZSUR(ICOMP)+1
        NSUR=NSUR+1
        IZSTOCN(icomp,nsur)=icontm  ! so connection will be known
        NVER(NSUR)=4
        isznver(ICOMP,NSUR)=4
        DO 252 J = 1,4
          X(NTV+J)=XXM(J); Y(NTV+J)=YYM(J); Z(NTV+J)=ZZM(J)
          JVN(NSUR,J)=JVNM(J)  ! user the reversed order list.
          iszjvn(ICOMP,NSUR,J)=JVNM(J) 
          szcoords(ICOMP,ntv+J,1)=XXM(J)
          szcoords(ICOMP,ntv+J,2)=YYM(J)
          szcoords(ICOMP,ntv+J,3)=ZZM(J)
  252   CONTINUE

C If SN is less than 12 characters add an _
        lnsn=lnblnk(SN)
        if(lnsn.lt.12)then
          write(SN,'(2a)') SN(1:lnsn),'_'
        else
          write(SN(12:12),'(a)') '_'
        endif
        call st2name(SN,SNAME(ICOMP,NSUR))  ! copy SN into SNAME

        if(act(1:1).eq.'V'.or.act(1:1).eq.'v')then
          SVFC(ICOMP,NSUR)='VERT'
        elseif(act(1:1).eq.'H'.or.act(1:1).eq.'h')then
          SVFC(ICOMP,NSUR)='CEIL'
        endif

        SOTHER(ICOMP,NSUR,1)='ANOTHER'
        write(SOTHER(ICOMP,NSUR,2),'(i2.2)')INT(icomp)
        write(SOTHER(ICOMP,NSUR,3),'(i3.3)')INT(NSUR-1)
        SPARENT(ICOMP,NSUR)='-'

        WRITE(SMLCN(ICOMP,NSUR),'(A)')constr
        write(SOTF(ICOMP,NSUR),'(a)') OPT(1:12)

C If version 1.1 of geometry specify the use of the surface.
        call eclose(gversion(icomp),1.1,0.01,newgeo)
        if(newgeo)then
          SUSE(ICOMP,nsur,1)='PARTN'
          SUSE(ICOMP,nsur,2)='- '
        else
          SUSE(ICOMP,NSUR,1)='-'
          SUSE(ICOMP,NSUR,2)='-'
        endif

C Add surface to the connection list (icont) assuming an unknown boundary. 
        call addedsurf(icomp,icontm,1,ier)
        call updatesvfc(icomp,modgeo)  ! update SVFC orientation string.

      endif

      return
      end

C ************* ADDVERT 
C ADDVERT: Add,  delete (interactive or silent), copy a vertex within
C a zone. Passed character ACTION to signal deletion or addition.

C ier (integer) is zero if nothing wrong. If action is to delete
C   and there is at least one surface that uses the vertex set
C   ier to be equal to that surface index so that calling code
C   can take appropriate actions. If there is more than one surface
C   using this index negate the index of the first surface and
C   pass back in ier.
      SUBROUTINE ADDVERT(icomp,NUM,ACTION,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "prj3dv.h"
#include "help.h"

C Parameters passed.
      integer icomp  ! the focus zone
      integer num    ! the vertex to delete or the number of vertices
                     ! to be added at the end of the list
      character action*1 ! A a for add, D d for delete, C c for copy
                         ! Q for quiet delete
      integer ier    ! zero is ok non-zero see note above

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

C Tollerance for surface matching.
      real ANGCC  ! angle between surfaces tolerance
      real CACC   ! tolerance between vertices
      real DACC   ! tolerance along line
      real COGCC  ! tolerance between surface COG
      real SNACC  ! tolerance between surface areas
      integer IACC ! number of matching corners outside dist tolerance
      common/matching/ANGCC,CACC,DACC,COGCC,SNACC,IACC

      LOGICAL check,found,closex,closey
      logical ok

      DIMENSION ISASSO(MS),X1(MV),Y1(MV),Z1(MV)
      CHARACTER OUTSTR*124,HOLD*36,outs*124
      integer iier  ! local variable for use in can't delete
      logical changedgeo  ! to signal need to write geom file.
      logical newgeo  ! version of geom file

      helpinsub='insert'  ! set for subroutine

      check = .false.
      changedgeo = .false.
      iier = 0

      if(NUM.eq.0)then
        call usrmsg('Sorry, there is no vertex 0 in the list...',
     &    ' ','W')
        return
      endif
      if(ACTION.eq.'D'.or.ACTION.eq.'d'.or.
     &   ACTION.eq.'Q'.or.ACTION.eq.'q')then

C Loop through the surfaces and see if this vertex is mentioned. If
C it is then it should not be delted so set iier
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
            if(ACTION.eq.'D'.or.ACTION.eq.'d')then
              WRITE(OUTSTR,9993)IWHICH,X(IWHICH),Y(IWHICH),Z(IWHICH)
 9993         FORMAT(' Vertex ',I3,' located at ',3F10.3)
              CALL EDISP(iuout,OUTSTR)
              CALL EDISP(iuout,' is associated with surfaces: ')
              WRITE(OUTSTR,9983)(ISASSO(IH),IH=1,IHIT)
 9983         FORMAT(' ',10(I2,', '))
              CALL EDISP(iuout,OUTSTR)
            endif
            if(ihit.eq.1)then
              iier=ISASSO(1)
            else
              iier=ISASSO(1)*(-1)
            endif
            MODIFYVIEW=.TRUE.  ! needed to preserve zone bounds
          ELSE

C Vertex is free of connections so loop through the vertices in each
C surface and any JNV larger than IWHICH should be decremented by one.
C Update as iszjvn well.
            DO 792 IS=1,NSUR
              DO 794 IV=1,NVER(IS)
                IF(JVN(IS,IV).GT.IWHICH)THEN
                  JVN(IS,IV)=JVN(IS,IV)-1
                  iszjvn(icomp,is,IV)=JVN(IS,IV)
                ENDIF
  794         CONTINUE
  792       CONTINUE

C Now loop through the vertices and copy the contents of vertices >
C IWHICH into the next lower one and also update szcoords.
            DO 791 IVV=IWHICH,NTV-1
              X(IVV)=X(IVV+1); Y(IVV)=Y(IVV+1); Z(IVV)=Z(IVV+1)
              szcoords(ICOMP,ivv,1)=szcoords(ICOMP,ivv+1,1)
              szcoords(ICOMP,ivv,2)=szcoords(ICOMP,ivv+1,2)
              szcoords(ICOMP,ivv,3)=szcoords(ICOMP,ivv+1,3)
  791       CONTINUE

C Finally decrement NZTV and NTV.
            NZTV(icomp)=NZTV(icomp)-1
            NTV=NZTV(icomp)
            changedgeo = .true.
            MODIFYVIEW=.TRUE.  ! signal a change
            check = .true.
          ENDIF
        ENDIF

      elseif(ACTION.eq.'A'.or.ACTION.eq.'a')then

C Increment NTV and present vertex coords (use previous
C in case of repitition), then parse. Check to see if the
C new vertex is unique (i.e. not within 4mm of an existing
C vertex.
        helptopic='vert_add_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        IADD=NUM
        DO 18 I=1,IADD
          if(I.eq.1)then
            HOLD = ' 0.000  0.000  0.000    '
          else
            WRITE(HOLD,'(3f11.5,a)')X1(I-1),Y1(I-1),Z1(I-1),'  '
          endif
          CALL EASKS(HOLD,' ','Vertex X Y Z (in metres):',
     &      36,' 0. 0. 0. ','vertex coord',IER,nbhelp)
          K=0
          CALL EGETWR(HOLD,K,X1(I),-999.9,999.9,'W','Xcoord',IER)
          CALL EGETWR(HOLD,K,Y1(I),-999.9,999.9,'W','Ycoord',IER)
          CALL EGETWR(HOLD,K,Z1(I),-9.9,999.9,'W','Zcoord',IER)
  18    CONTINUE

C Append new vertices to zone list if they are unique (within 4mm
C or if 10mm check with user, otherwise continue.
        do 143 ix = 1,IADD
          if(NTV+1.le.MTV)then
            found=.false.
            do 144 ixx = 1,NTV
              call eclose3(X1(ix),Y1(ix),Z1(ix),X(ixx),Y(ixx),Z(ixx),
     &          CACC,closex)
              call eclose3(X1(ix),Y1(ix),Z1(ix),X(ixx),Y(ixx),Z(ixx),
     &          CACC*2,closey)
              dist=crowxyz(X1(ix),Y1(ix),Z1(ix),X(ixx),Y(ixx),Z(ixx))
              if(closex)then
                call usrmsg(
     &            'Co-ordinates ~identical to an existing vertex',
     &            'so ignored.','P')
                found=.true.
                goto 145
              endif
              if(closey)then

C Feedback to the user about using existing vertex.
                write(outs,'(a,i3,a,3f8.3,a,i3,a,3f8.3)')
     &            'New vertex ',ix,' @',X1(ix),Y1(ix),Z1(ix),
     &            ' is close (',dist,') to existing vertex',ixx,' @',
     &            X(ixx),Y(ixx),Z(ixx)
                call edisp(iuout,outs)
                call easkok(' ',
     &            'Use existing vertex instead of new one?',
     &            found,nbhelp)
                if(found)then
C                  match=ixx  !  match is not yet used
                  goto 145
                endif
              endif
  144       continue

C If an existing vertex is close then skip to next, otherwise
C add a new vertex to the end of the zone list and update szcoords.
  145       if(found)then
              continue
            else
              changedgeo = .true.

C Merge the copied point into the zone and existing edges.
              QX=X1(ix); QY=Y1(ix); QZ=Z1(ix)
              CALL MERGEVERTINSURF(ICOMP,QX,QY,QZ,IER)
            endif
          endif
  143   continue
        MODIFYVIEW=.TRUE.
        check = .true.
      elseif(ACTION.eq.'C'.or.ACTION.eq.'c')then

C Copy vertex index and then increment NTV and present vertex coords 
C then parse into new vertex.
        helptopic='vert_copy_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call tstamp('>','PRJ: copy vertex')
        WRITE(HOLD,'(3f11.5,a)')X(NUM),Y(NUM),Z(NUM),'  '
        write(outs,'(a,i3,a,i3,a)')'Copied Vertex (',NUM,
     &    ' old; current index ',NTV+1,')'
        CALL EASKS(HOLD,outs,' X  Y  Z (in metres): ',
     &    36,' 0. 0. 0. ','vertex coord',IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,X1(1),-999.9,999.9,'W','X coord',IER)
        CALL EGETWR(HOLD,K,Y1(1),-999.9,999.9,'W','Y coord',IER)
        CALL EGETWR(HOLD,K,Z1(1),-9.9,999.9,'W','Z coord',IER)

C Append new vertice to zone list if it is unique (current assumption
C is that for most models, less than 4mm difference would be considered
C a repeat of an existing vertex, if 5-10mm then check with the
C user otherwise continue.
        if(NTV+1.le.MTV)then
          found=.false.
          do 244 ixx = 1,NTV
            call eclose3(X1(1),Y1(1),Z1(1),X(ixx),Y(ixx),Z(ixx),
     &        CACC,closex)
            call eclose3(X1(1),Y1(1),Z1(1),X(ixx),Y(ixx),Z(ixx),
     &        CACC*2,closey)
            dist=crowxyz(X1(1),Y1(1),Z1(1),X(ixx),Y(ixx),Z(ixx))
            if(closex)then
              found=.true.
              goto 245
            endif
            if(closey)then
              write(outs,'(a,3f8.3,a,f8.3,a,i3,a,3f8.3)')
     &          'Copied vertex @',X1(1),Y1(1),Z1(1),
     &          ' is close (',dist,') to existing vertex',ixx,' @',
     &          X(ixx),Y(ixx),Z(ixx)
              call edisp(iuout,outs)
              call easkok('Copied point close to an existing vertex!',
     &          'Skip copy?',found,nbhelp)
              goto 245
            endif
  244     continue

C If an existing vertex is close then skip to next, otherwise
C add a new vertex to the end of the zone list and update szcoords.
  245     if(found)then
            call edisp(iuout,'Duplicate of existing vertex. Ignoring.')
            continue
          else

C Merge the copied point into the zone and existing edges.
            QX=X1(1); QY=Y1(1); QZ=Z1(1)
            write(outs,'(a,3F8.3)') 'Adding vertex @ ',QX,QY,QZ
            call edisp(iuout,outs)
            CALL MERGEVERTINSURF(ICOMP,QX,QY,QZ,IER)
          endif
        endif
        MODIFYVIEW=.TRUE.
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

C If we modified the geometry then write it out because subsequent
C calls to redraw will rescan this geometry file and undo the
C changes.
      if(changedgeo)then
        call eclose(gversion(icomp),1.1,0.01,newgeo)
        if(.NOT.newgeo)then
           gversion(icomp) =1.1
           newgeo = .true.
        endif
        call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,iuout,3,IER)
        call zgupdate(0,icomp,ier)
      endif

C If change in vertex detected then update common G7
      if(MODIFYVIEW)then
        call zgupdate(0,icomp,ier)
      endif

C Assign ier as set within the delete function.
      ier=iier
      RETURN
      END

C ************* CKVERT 
C CKVERT: Checks vertex lists and returns whether zone is fully 
C bounded (bound=.true.) and which surfaces/vertex links are unmatched.
C Number of unbounded edges in iub, number inverted surfaces inve.
C IVB = 0 is silent, IVB=1 feedback on errors. act '-' report only,
C act 'r' attempt repair.

C << could this be done with G6 and with ??

      SUBROUTINE CKVERT(ivb,ICOMP,bounded,iub,inve,act,IER)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "prj3dv.h"
      integer ivb   ! verbosity
      integer icomp ! the current zone
      logical bounded ! true if fully bounded
      integer iub     ! number of unbounded edges
      integer inve    ! number of inverted edges
      character act*1 ! - report r repair
      integer ier     ! error state

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      common/appcols/mdispl,nifgrey,ncset,ngset,nzonec

C Tollerance for surface matching.
      real ANGCC  ! angle between surfaces tolerance
      real CACC   ! tolerance between vertices
      real DACC   ! tolerance along line
      real COGCC  ! tolerance between surface COG
      real SNACC  ! tolerance between surface areas
      integer IACC ! number of matching corners outside dist tolerance
      common/matching/ANGCC,CACC,DACC,COGCC,SNACC,IACC

      DIMENSION COG1(3),COG2(3)
      DIMENSION itv1(MS*MV) ! 1st of every edge in every surface
      DIMENSION itv2(MS*MV) ! 2nd of every edge in every surface
      DIMENSION irelsur(MS*MV) ! which surface assoc with every edge
      DIMENSION invesur(MS),jvn1(MV)
      CHARACTER outs*124,louts*496,msg*96
      logical hilight
      logical greyok,matchver,notin,OK,adjusted
      logical newgeo  ! version of geom file
      integer icursur,ipair
      integer iii

#ifdef OSI
      integer iupdown,isym,iix,iiy,iicol    ! passed to etplot
#else
      integer*8 iupdown,isym,iix,iiy,iicol    ! passed to etplot
#endif

      greyok=.false.      ! see if colour available
      if(nifgrey.gt.4)then
        greyok=.true.
      endif

C New or older geometry file?
      call eclose(gversion(icomp),1.1,0.01,newgeo)
      adjusted=.false.  ! set true later if change requires file update

C Jump back point in case the edge lists have been updated
C (the itv1 & itv2 need to be re-formed.
  42  continue
  
C Take a line pair and then look and see if it is referenced in
C reverse order just once. itv1 & itv2 are the vertices at the
C start and end of each edge, its1 is the reference surface. 
      if(ivb.ne.0)call tstamp('>','PRJ: checking vertex match')
      ipair=0; inve=0; iub=0
      DO IS=1,NSUR
        invesur(IS)=0
        if(NVER(IS).eq.0)then
          continue   ! what about itv2 value for this?
        else
          DO IV=1,NVER(IS)  ! need to trap for NVER zero
            ipair=ipair+1
            itv1(ipair)=JVN(IS,IV)

C Also check the last vertex back to the first.
            if(IV.eq.NVER(IS))then
              itv2(ipair)=JVN(IS,1)
            else
              itv2(ipair)=JVN(IS,IV+1)
            endif
            irelsur(ipair)=IS
          ENDDO  ! of NVER
        endif
      ENDDO      ! of NSUR

C Count the number of times the edge mentioned (in both directions).
      bounded=.true.
      hilight=.false.
      do 144 ip=1,ipair
        matchab = 0; matchba = 0
        do ipck=1,ipair

C First see if reverse match has been found.
          if(itv1(ip).eq.itv2(ipck).and.itv2(ip).eq.itv1(ipck))then
            matchba = matchba +1
          elseif(itv1(ip).eq.itv1(ipck).and.itv2(ip).eq.itv2(ipck))then
            matchab = matchab +1
          endif
        enddo
        if(matchab.ne.matchba)then

C The imbalance between matchab and matchba is the number of problem
C edges.  If ivimb=2 then probably two edges in same direction, if
C ivimb=1 then we have an unmatched edge.
          bounded=.false.
          ivimb=(abs((matchab+10)-(matchba+10)))
          if(ivimb.eq.2)then
            write(outs,'(a,i3,a,i3,3a)')
     &        'The edge between vertices ',itv1(ip),' & ',
     &        itv2(ip),' of ',SNAME(icomp,irelsur(ip)),
     &        ' may be adjacent to a reversed surface.'
            if(ivb.ne.0)then
              call edisp(iuout,outs)
              if(MMOD.lt.8)then
                continue
              else
                hilight=.true.
              endif
            endif

C Incrememt hits for zone as well as for this surface.
            inve=inve+1
            invesur(irelsur(ip))=invesur(irelsur(ip))+1
    
          elseif(ivimb.eq.1)then
            icursur=irelsur(ip)  ! remember the current surface
            write(outs,'(a,i3,a,i3,3a)')
     &        'The edge between vertices ',itv1(ip),' & ',itv2(ip),
     &        ' of ',SNAME(icomp,icursur),' may be unbounded.'
            if(ivb.ne.0)then
              call edisp(iuout,outs)
              if(MMOD.lt.8)then
                continue
              else
                hilight=.true.
              endif
            endif
            iub=iub+1

            if(act(1:1).eq.'-')return   ! report only

C This is a great place to look for vertices that are not
C part of this surface's edges (notin true) that could be
C added. If we have a corrupt JVN list (a zero entry) ignore.
            do iwhich1=1,NTV
              notin=.true.
              if(NVER(icursur).eq.0)then
                notin=.false.
              else
                do iwhich2=1,NVER(icursur)
                  if(iwhich1.eq.JVN(icursur,iwhich2)) notin=.false.
                  if(JVN(icursur,iwhich2).eq.0) notin=.false.
                enddo
              endif

C Also check that the line has some length.
              xitem=X(itv1(ip)); yitem=Y(itv1(ip)); zitem=Z(itv1(ip))
              xpitem=X(itv2(ip));ypitem=Y(itv2(ip));zpitem=Z(itv2(ip))
              vdis= crowxyz(xitem,yitem,zitem,xpitem,ypitem,zpitem)
              if(vdis.lt.0.1) notin=.false.
              if(notin)then
                call pointtoline(iwhich1,itv1(ip),itv2(ip),offset,
     &            matchver)
                if(itv1(ip).eq.itv2(ip))then
                  matchver=.false.  ! cannot deal with this
                endif
                if(matchver)then
                  if(offset.lt.CACC)then
                    write(outs,'(a,i3,a,3f9.4,a,f6.4,a,i3,a,i3,2a)')
     &                'Vertex ',iwhich1,' @',X(iwhich1),Y(iwhich1),
     &                Z(iwhich1),' is close (',offset,') to edge ',
     &                itv1(ip),' & ',itv2(ip),' of surface ',
     &                SNAME(icomp,icursur)
                    if(ivb.ne.0) call edisp(iuout,outs)

C Loop through the current surface and find the position in the
C list for itv1(ip) so insertion can come just after this point.
                    itsposition=0
                    do iwhich2=1,NVER(icursur)
                      if(itv1(ip).eq.JVN(icursur,iwhich2))then
                        itsposition=iwhich2
                      endif
                    enddo
                    ivtoadd=iwhich1  ! remember it
                    write(louts,'(a,124i4)') 'cur list: ',
     &                (JVN(icursur,ii),ii=1,NVER(icursur))
                    if(ivb.ne.0) call edisp248(iuout,louts,100)

C If current surface (icursur) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JVN indices up one until at the current edge and then
C inserting the new vertex index.
                    if(NVER(icursur)+1.le.MV)then
                      if(offset.lt.0.004)then
                        ok=.true.  ! If within 4mm just do it
                        write(outs,'(a,f6.4,3a)') 'Inserting a (',
     &                    offset,'m) to edge vertex within ',
     &                    SNAME(icomp,icursur),'.'
                        if(ivb.ne.0) call edisp(iuout,outs)
                      else
                        call edisp248(iuout,louts,100)
                        write(msg,'(a,f6.4,3a)') 'Insert this (',offset,
     &                    'm) to edge vertex in ',
     &                    SNAME(icomp,icursur),'?'
                        call easkok(' ',msg,ok,nbhelp)
                      endif
                    else
                      ok=.false.
                    endif
                    if(ok)then
                      adjusted=.true.
                      NVER(icursur)=NVER(icursur)+1
                      isznver(icomp,icursur)=NVER(icursur)

C If its location in the list is actually the last in the list?
                      if(itsposition+1.eq.NVER(icursur))then
                        JVN(icursur,itsposition+1)=ivtoadd
                        iszjvn(icomp,icursur,itsposition+1)=ivtoadd
                      else
                        iii=NVER(icursur)+1
  348                   continue
                        iii=iii-1
                        JVN(icursur,iii)=JVN(icursur,iii-1)
                        iszjvn(icomp,icursur,iii)=JVN(icursur,iii)
                        IF(iii.GT.itsposition+1)GOTO 348
                        JVN(icursur,itsposition+1)=ivtoadd
                        iszjvn(icomp,icursur,itsposition+1)=ivtoadd
                      endif
C Debug.
                      write(louts,'(a,124i4)') 'adj list: ',
     &                  (JVN(icursur,ii),ii=1,NVER(icursur))
                      if(ivb.ne.0) call edisp248(iuout,louts,100)
                      iub=iub-1   ! decrement counter of unbounded
                      goto 42     ! jump back and reform itv1 & itv2
                    endif
                  endif
                endif
              endif
            enddo
          endif

          if(hilight)then
            if(itv1(ip).gt.0.and.itv2(ip).gt.0)then
    
C Transform ends of edge, highlight will not be true
C if we are not running in graphic mode.
              ILT= -305
              COG1(1)=X(itv1(ip)); COG1(2)=Y(itv1(ip))
              COG1(3)=Z(itv1(ip))
              CALL VECTRN(COG1,TSMAT,COG2,IER)
              call CLIPPT(COG2(1),COG2(2),COG2(3),icp)
              if (iclp.eq.0) then
                call u2pixel(COG2(1),COG2(2),iix,iiy)
                iicol=0
                if(greyok)call winscl('z',iicol)
                call esymbol(iix,iiy,24,1)
                call VERTLBL(iix,iiy,COG2(3),itv1(ip),ier)
                iicol=0
                if(greyok)call winscl('-',iicol)
                call forceflush()
              endif

              iupdown=0
              isym=0
              call etplot(COG2(1),COG2(2),iupdown,isym)
              COG1(1)=X(itv2(ip)); COG1(2)=Y(itv2(ip))
              COG1(3)=Z(itv2(ip))
              CALL VECTRN(COG1,TSMAT,COG2,IER)
              iupdown=ILT
              call etplot(COG2(1),COG2(2),iupdown,isym)
              call CLIPPT(COG2(1),COG2(2),COG2(3),iclp)
              if (iclp.eq.0) then
                call u2pixel(COG2(1),COG2(2),iix,iiy)
                iicol=0
                if(greyok)call winscl('z',iicol)
                call esymbol(iix,iiy,24,1)
                call VERTLBL(iix,iiy,COG2(3),itv2(ip),ier)
                iicol=0
                if(greyok)call winscl('-',iicol)
                call forceflush()
              endif
              hilight=.false.
            endif
          endif
        endif
 144  continue
      if(ivb.ne.0)then

C If all edges of a surface are likely to be inverted offer
C the user the option to invert.
C Debug.
C        write(6,*) 'reversed hits per surface ',invesur
C        write(6,*) 'edges per surface ',NVER
        DO IS=1,NSUR
          if(NVER(IS).gt.0.and.(invesur(IS).eq.NVER(IS)))then
            write(louts,'(a,124i4)') 'current list: ',
     &        (JVN(is,ii),ii=1,NVER(is))
            call edisp248(iuout,louts,100)
            write(outs,'(3a)')
     &        'The edges of surface ',SNAME(icomp,is),
     &        ' likely need to be inverted!'
            call easkok(outs,'Invert?',ok,nbhelp)
            if(ok)then
              adjusted=.true.  ! ensure option to save is given
              do 146, iyy = 1,NVER(is)
                jvn1(iyy)=JVN(is,iyy)
  146         CONTINUE
              JVN(is,1)=jvn1(2)
              iszjvn(icomp,is,1)=jvn1(2)
              JVN(is,2)=jvn1(1)
              iszjvn(icomp,is,2)=jvn1(1)
              do 147, iyy = 3,NVER(is)
                izz=NVER(is)+3-iyy
                JVN(is,iyy)=jvn1(izz)
                iszjvn(icomp,is,iyy)=jvn1(izz)
  147         CONTINUE
              write(louts,'(a,124i4)') 'inverted list is ',
     &          (JVN(is,ii),ii=1,NVER(is))
              call edisp248(iuout,louts,100)
            endif
          endif
        ENDDO
      endif

C If any of the edge lists were altered give option to update.
      if(adjusted)then
        if(ivb.eq.0)then
          ok=.true.
        else
          call easkok(' ','Update geometry file?',ok,nbhelp)
        endif

C Update the geometry file.
        if(ok)then
          if(newgeo)then
            call geowrite(IFIL+2,LGEOM(ICOMP),ICOMP,
     &        iuout,3,IER)
          else
            call emkgeo(IFIL+2,LGEOM(ICOMP),ICOMP,3,IER)
          endif
        endif
      endif
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
C Add a rectangular surface within or at bottom of existing surface.
C ITRU = unit number for user output, IER=0 OK, IER=1 problem.
C act is 'ii' insert interactive, 'sw' insert window silently, or
C act = 'sd' insert door silently or 'sp' insert window percentage silently.
C Logic is intended to work with vertical surfaces as well as sloped
C surfaces.  There are some assumptions that the parent surface is
C rectangular (this needs further investigation).

C For the silent modes the initial name and construction and use
C (rsname,rsotf,rsmlcn,rsuse1,rsuse2,rsparent) are passed as parameters. 

C << Implementation of a cancel option will require the common block
C << instantiation code blocks to be moved so that the data is only
C << written at the end of the process. This has yet to be done.

      SUBROUTINE INSREC(ITRC,ITRU,ICOMP,IS,act,XO1,ZO1,XW,ZH,
     &  rsname,rsotf,rsmlcn,rsuse1,rsuse2,rsparent,guesstype,IER)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"
#include "esprdbfile.h"
#include "material.h"
#include "help.h"

C Parameters
      integer itrc,itru      ! tracel level and reporting unit
      integer icomp          ! zone number
      integer is             ! surface number (parent)
      character act*2        ! user supplied action
      real XO1,ZO1,XW,ZH     ! X and Z offset width and height of opening
      character rsname*12,rsotf*32,rsmlcn*32,rsuse1*12,rsuse2*12
      character rsparent*12
      character guesstype*24 ! pass back context of surface
      integer IER            ! zero is ok

      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)

C Assumption about inserted window surface percentage and assumed
C door width and height.
      real defwininsert,defdoorwidth,defdoorheight
      common/ginsert/defwininsert,defdoorwidth,defdoorheight

      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

C Tollerance for surface matching.
      real ANGCC  ! angle between surfaces tolerance
      real CACC   ! tolerance between vertices
      real DACC   ! tolerance along line
      real COGCC  ! tolerance between surface COG
      real SNACC  ! tolerance between surface areas
      integer IACC ! number of matching corners outside dist tolerance
      common/matching/ANGCC,CACC,DACC,COGCC,SNACC,IACC

      LOGICAL OK,matchver,modmlc
      logical newgeo  ! to use for testing if new/old geometry file.

      DIMENSION  XX(MV),YY(MV),ZZ(MV)
      DIMENSION  XA(4),YA(4),ZA(4)
      DIMENSION  XFA(MV),YFA(MV),ZFA(MV)
      DIMENSION  AX(MPL),AY(MPL),AZ(MPL),IANXT(MPL)
      DIMENSION  BX(MPL),BY(MPL),BZ(MPL),IBNXT(MPL)
      DIMENSION  SBBOX(3,2)

      CHARACTER outs*124,louts*248,hold*36,DESCRC*25,SN*12
      CHARACTER T14*14
      integer iwedge ! number of extra frame vertices
      integer llpos,lrpos,ulpos,urpos     ! closest to BB corners for parent

#ifdef OSI
      integer iupdown,isym    ! passed to etplot
#else
      integer*8 iupdown,isym    ! passed to etplot
#endif

      helpinsub='insert'  ! set for subroutine

C Check that we can deal with additional complexity. E.G that
C there is room for an additional surface and 4 vertices (one for extra
C possible link to the existing surface) as well as if the number of
C vertices in this surface +6 are < MV. 
      IF((NSUR+1.GT.MS).OR.(NTV+4.GT.MTV).OR.(NVER(IS)+6.GT.MV).OR.
     &   (NCON+1.GT.MCON))THEN
        CALL USRMSG(' A new surface could not be added as it',
     &    ' will make the model/zone/surface too complex!','W')
        ier=1
        RETURN
      ENDIF

C Depending on act.
      newgeo=.false.  ! assume older format geometry.
      if(act(1:2).eq.'ii')then
        continue    ! insert interactive
      elseif(act(1:2).eq.'sw')then

C Insert window silently, code assumes G1 common block is fresh.
        IRT=1
        guesstype='window_grill_frame'
        goto 72

      elseif(act(1:2).eq.'sd')then

C Insert door silently.
        IRT=2
        guesstype='door_grill_frame'
        goto 72

      elseif(act(1:2).eq.'sp')then

C Insert percentage silently.
        IRT=3
        PERC = XO1

C Determine opening width, height, X offset and z offset.
C        CALL ZSURLEHI(ICOMP,IS,XYMAX,ZMAX,llpos,lrpos,ulpos,urpos,
C     &    DZLLFF)
        CALL SURLEHI(IS,XYMAX,ZMAX,llpos,lrpos,ulpos,urpos)
        XW=(XYMAX*SQRT(PERC))/10
        ZH=(ZMAX*SQRT(PERC))/10
        XO1=((XYMAX-XW)/2)
        ZO1=((ZMAX-ZH)/2)
        guesstype='window_grill_frame'
        goto 72
      endif

      if(act(1:2).ne.'ii')then

C If in a silent mode, set all surfaces to standard line width and
C surface being edited to a thick line if currently in graphics mode.
        ITVNO=0
        ITSNM=0
        MODIFYVIEW=.TRUE.
        if(MMOD.EQ.8)then
          CALL INLNST(1)
          CALL SURADJ(ICOMP,IS,IE,TMP,IZC,ISC,IC,DESCRC)
          LINSTY(IC)=2
          nzg=1
          nznog(1)=ICOMP
          izgfoc=ICOMP
          CALL redraw(IER)
        endif
        goto 72
      endif

C Ask user whether new surface should be inserted within surface (IRT=1)
C or at bottom edge (as in a door) (IRT=2) as percentage (IRT=3) or a
C fixed width frame (IRT=4).
      helptopic='add_rect_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKATOG('  ','Insert surface:',
     &  'within surface','at base (door)','% of surface area',
     &  'within frame','continue ?',' ',' ',IRT,nbhelp)
      if(IRT.eq.5)return

      if(IRT.eq.1)then

C Set initial offset values for rectangle within surface.
        XO1=1.0; ZO1=0.5; XW=1.0; ZH=1.0
        guesstype='window_grill_frame'
        IPS=IS  ! remember

      elseif(IRT.eq.2)then

C Set initial offset values for door at base of surface.
        XO1=0.1; ZO1=0.0
        XW=defdoorwidth; ZH=defdoorheight
        guesstype='door_grill_frame'
        IPS=IS  ! remember

      elseif(IRT.eq.3)then

C Set initial offset values for rectangle centred in surface.
        PERC = defwininsert
        guesstype='window_grill_frame'
        IPS=IS  ! remember

      elseif(IRT.eq.4)then

C Set initial frame width
        FRW = 0.080
        guesstype='window_grill_frame'
        IPS=IS  ! remember
      endif

 30   CONTINUE

C Set all surfaces to standard line width and surface being 
C edited to a thick line if currently in graphics mode.
      ITVNO=0
      ITSNM=0
      MODIFYVIEW=.TRUE.
      if(MMOD.EQ.8)then
        CALL INLNST(1)
        CALL SURADJ(ICOMP,IS,IE,TMP,IZC,ISC,IC,DESCRC)
        LINSTY(IC)=2
        nzg=1
        nznog(1)=ICOMP
        izgfoc=ICOMP
        CALL redraw(IER)
      endif

C Report parent surface length and height (bounding box).
C      CALL ZSURLEHI(ICOMP,IS,XYMAX,ZMAX,llpos,lrpos,ulpos,urpos,
C    &   DZLLFF)
      CALL SURLEHI(IS,XYMAX,ZMAX,llpos,lrpos,ulpos,urpos)
      Write(outs,'(a,f6.3,a,f6.3,a)') 'Parent surface width = ',
     &  XYMAX,' m, surface height = ',ZMAX,' m (bounding box).'
      call edisp(itru,outs)

  43  if(IRT.eq.1)then

C Warn if offsets or size are small.
        helptopic='add_rect_offsets'
        call gethelptext(helpinsub,helptopic,nbhelp)
        write(hold,'(4f8.3,a)') XO1,ZO1,XW,ZH,'  '
        CALL EASKS(hold,
     &    'Insert: X offset, Z offset, Width, Height: (see help)',
     &    ' ',36,' 0.2 0.8 1.0 1.0','Xoff Zoff width height',IER,
     &    nbhelp)
        K=0
        CALL EGETWR(HOLD,K,XO1,0.025,99.0,'F','X offset',IER)
        CALL EGETWR(HOLD,K,ZO1,0.025,99.0,'F','Z offset',IER)
        CALL EGETWR(HOLD,K,XW,0.02,99.0,'F','width',IER)
        CALL EGETWR(HOLD,K,ZH,0.02,99.0,'F','height',IER)

C Test if the opening is going to be bigger than the bounding box.
        if((XW+XO1).ge.XYMAX)then
          call edisp(iuout,
     &      'Width of new opening is bigger than bounding box!')
          goto 43
        elseif((ZH+ZO1).ge.ZMAX)then
          call edisp(iuout,
     &      'Height of new opening is bigger than bounding box!')
          goto 43
        endif

      elseif (IRT.eq.2)then

C Warn if offsets or size are small.
        helptopic='small_offset_warning'
        call gethelptext(helpinsub,helptopic,nbhelp)
        write(hold,'(3f8.3,a)') XO1,XW,ZH,'  '
        CALL EASKS(hold,
     &    'Insert: X offset, Width, Height (m): (see help)',
     &    ' ',36,' 0.1 0.8 2.1','Xoff Zoff width height',IER,nbhelp)
        K=0
        CALL EGETWR(HOLD,K,XO1,0.025,99.,'F','X offset',IER)
        CALL EGETWR(HOLD,K,XW,0.02,99.0,'F','width',IER)
        CALL EGETWR(HOLD,K,ZH,0.02,99.0,'F','height',IER)

C Test if the opening is going to be bigger than the bounding box.
        if((XW+XO1).ge.XYMAX)then
          call edisp(iuout,
     &      'Width of new opening is bigger than bounding box!')
          goto 43
        elseif(ZH.ge.ZMAX)then
          call edisp(iuout,
     &      'Height of new opening is bigger than bounding box!')
          goto 43
        endif
        if((XO1).lt.0.05)then
          call edisp(iuout,
     &      'Offset is to close to parent left vertical edge!')
          goto 43
        endif

      elseif (IRT.eq.3)then

C Ask for percentage between 5 and 95%.
        hold=' '
        helptopic='opening_percent_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKR(PERC,'Percentage of parent surface.',
     &    ' ',5.,'F',95.,'F',15.0,'parent percentage',IER,nbhelp)

C Determine opening width, height, X offset and z offset.
C NOTE: this logic assumes a rectangular parent surface
C and the area will not be correct for other surface shapes.
        XW=(XYMAX*SQRT(PERC))/10
        ZH=(ZMAX*SQRT(PERC))/10
        XO1=((XYMAX-XW)/2)
        ZO1=((ZMAX-ZH)/2)
        guesstype='window_grill_frame'

      elseif (IRT.eq.4)then

C Ask for frame width in m. Logic for frame width could check the
C size and minimum dimensions < not yet done >. 
        hold=' '
        helptopic='opening_percent_overview'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKR(FRW,'Frame width (m).','  ',
     &    0.001,'F',0.3,'W',40.0,'frame width',IER,nbhelp)
      endif

C If error state non-zero then jump back.
      if(ier.ne.0)goto 43

C Process the information gathered.
  72  continue

C Transform existing surface into into site coordinates in the
C surface of the plane. Make up XX,YY,ZZ to pass across to the
C transform routine. The returned coordinates are in XA YA ZA or
C in XFA YFA ZFA.
      N = NVER(IS)
      DO 150 J = 1,N
        XX(J) = X(JVN(IS,J))
        YY(J) = Y(JVN(IS,J))
        ZZ(J) = Z(JVN(IS,J))
  150 CONTINUE
      loop=4   ! assume rectangular insert
      if(IRT.eq.1.or.IRT.eq.2.or.IRT.eq.3)then
        CALL ETRANSW(ITRC,ITRU,N,XX,YY,ZZ,0.00,XO1,ZO1,XW,ZH,XA,YA,ZA)
      else

C In the case of inserting a frame pass in the number of parent surface
C edges (N) and coords in XX YY ZZ and frame width FRW and get back array
C XFA YFA ZFA as well as the number of edges.
        CALL ETRANFRAME(ITRC,N,XX,YY,ZZ,FRW,XFA,YFA,ZFA,iwedge,IER)
        loop=N  ! reset to match polygon edges
        loop=iwedge  ! reset to match polygon edges
C << trap non-zero ier value >>
      endif

C If in silent mode skip past the graphic feedback.
      if(act(1:2).eq.'sw'.or.act(1:2).eq.'sd'.or.act(1:2).eq.'sp')then
        goto 152
      endif

C If in graphics mode figure out where to draw the new surface.
      if(MMOD.EQ.8)then
        IAPNT = 1
        DO 350 J = 1,loop
          if(IRT.eq.1.or.IRT.eq.2.or.IRT.eq.3)then
            AX(J) = XA(J); AY(J) = YA(J); AZ(J) = ZA(J)  ! use rect array
            IANXT(J) = J + 1
          else
            AX(J) = XFA(J); AY(J) = YFA(J); AZ(J) = ZFA(J)  ! use poly array
            IANXT(J) = J + 1
          endif
  350   CONTINUE
        IANXT(loop) = IAPNT
        CALL MATPOL(loop,IAPNT,AX,AY,AZ,IANXT,TSMAT,
     &    SBBOX,NBP,IBPNT,BX,BY,BZ,IBNXT,IERR)
        CALL CLIPFL(NBP,BX,BY,BZ,ISTAT)
        IF (ISTAT .EQ. 1) then
          goto 32
        ELSEIF (ISTAT .EQ.-1)then
          CALL CUTPOL(NB,NBP,IBPNT,BX,BY,BZ,IBNXT,ISTAT)
        ELSE
          NB=1
        ENDIF

C Draw the (perhaps clipped) rectangle.
        DO 302 JB = 1,NB
          IP = IABS(IBPNT)
          iupdown=0; isym=0
          call etplot(BX(IP),BY(IP),iupdown,isym)

C Draw succeeding points until last vertex.
          IP1 = IBNXT(IP)
 451      CONTINUE
          iupdown=1; isym=0
          call etplot(BX(IP1),BY(IP1),iupdown,isym)

          IP1 = IBNXT(IP1)
          IF(IP1 .NE. IP) GOTO 451

C Complete rec.
          iupdown=1; isym=0
          call etplot(BX(IP),BY(IP),iupdown,isym)
  302   CONTINUE    ! loop for NB
   32   CONTINUE

C Confirm opening.
        call forceflush()  ! ensure opening is drawn
        helptopic='confirm_location'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKOK(
     &   'Wireframe shows the position based on your input.',
     &   'Accept?',OK,nbhelp)
        IF(.NOT.OK)GOTO 30
      endif

C Now update the appropriate common data for the new surface. The
C following code is similar to that in addsur in edzone.f. Because
C the surface to be added is not yet in the connections list find
C its future position via `one connection past the current last
C surface in the zone`. ICONO is the connection for the parent surface.
 152  ICONT=IZSTOCN(icomp,nsur)+1
      ICONO=IZSTOCN(icomp,is)
      IZSTOCN(icomp,nsur+1)=icont   ! so that new connection is known
      NZSUR(ICOMP)=NZSUR(ICOMP)+1
      NSUR=NSUR+1
      NVER(NSUR)=loop    ! new surface has loop edges
      isznver(icomp,nsur)=loop
      DO 151 J = 1,loop  ! loop for the corners of new surface
        if(IRT.eq.1.or.IRT.eq.2.or.IRT.eq.3)then
          X(NTV+J)=XA(J); Y(NTV+J)=YA(J); Z(NTV+J)=ZA(J)
          JVN(NSUR,J)=NTV+J
          iszjvn(icomp,nsur,j)=NTV+J
          szcoords(ICOMP,ntv+J,1)=XA(J)
          szcoords(ICOMP,ntv+J,2)=YA(J)
          szcoords(ICOMP,ntv+J,3)=ZA(J)
        else
          X(NTV+J)=XFA(J); Y(NTV+J)=YFA(J); Z(NTV+J)=ZFA(J)
          JVN(NSUR,J)=NTV+J
          iszjvn(icomp,nsur,j)=NTV+J
          szcoords(ICOMP,ntv+J,1)=XFA(J)
          szcoords(ICOMP,ntv+J,2)=YFA(J)
          szcoords(ICOMP,ntv+J,3)=ZFA(J)
        endif
  151 CONTINUE
C Debug
C      write(6,'(a,2i3,124i4)') '151 jvn ',
C     &  nsur,NVER(nsur),(jvn(nsur,j),J=1,NVER(nsur))
      if(act(1:2).eq.'sw'.or.act(1:2).eq.'sd'.or.act(1:2).eq.'sp')then
        write(SN,'(a)') rsname(1:12)
        call st2name(SN,SNAME(ICOMP,NSUR))
      else
        T14=' '; SN=' '
        helptopic='surface_name_logic'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKS(T14,' ',' Inserted surface name (12 char max):',
     &    14,'new_door','surface name',IER,nbhelp)
        write(SN,'(a)') T14(1:12)
        call st2name(SN,SNAME(ICOMP,NSUR))
      endif

C Assign parent name if in silent name, otherwise use placeholder.
      if(act(1:2).eq.'sw'.or.act(1:2).eq.'sd'.or.act(1:2).eq.'sp')then
        write(sparent(ICOMP,nsur),'(a)') rsparent
      else
        SPARENT(ICOMP,NSUR)='-'
      endif

C New surface to inherit the general properties of the parent. If
C parent is already marked as ANOTHER then set to UNKNOWN
C (otherwise there will be two surfaces point to the same other surface).
      SOTF(ICOMP,NSUR)=SOTF(ICOMP,IS); SVFC(ICOMP,NSUR)=SVFC(ICOMP,IS)
      if(SOTHER(ICOMP,IS,1)(1:7).eq.'ANOTHER')then
        SOTHER(ICOMP,NSUR,1)='UNKNOWN'
        SOTHER(ICOMP,NSUR,2)='00'
        SOTHER(ICOMP,NSUR,3)='000'
      else
        SOTHER(ICOMP,NSUR,1)=SOTHER(ICOMP,IS,1)
        SOTHER(ICOMP,NSUR,2)=SOTHER(ICOMP,IS,2)
        SOTHER(ICOMP,NSUR,3)=SOTHER(ICOMP,IS,3)
      endif
      SUSE(ICOMP,NSUR,1)='-'; SUSE(ICOMP,NSUR,2)='-'

C If in silent mode assign construction otherwise select. 
      if(act(1:2).eq.'sw'.or.act(1:2).eq.'sd'.or.act(1:2).eq.'sp')then
        WRITE(SMLCN(ICOMP,NSUR),'(A)') rsmlcn(1:12)
        WRITE(SOTF(ICOMP,NSUR),'(A)')  rsotf(1:12)  ! only 1st 12 char
      else
        CALL EPMENSV
        if(mlcver.eq.0)then
          CALL EPKMLC(ISEL,'Select one of the constructions for the',
     &    'inserted surface. ',IER)
        else
          call edisp(iuout,
     &      'Select a construction for inserted surface')
          CALL EDMLDB2(modmlc,'-',ISEL,IER)
        endif
        CALL EPMENRC
        IF(ISEL.GT.0)then
          WRITE(SMLCN(ICOMP,NSUR),'(A)') mlcname(ISEL)
          IF(mlctype(ISEL)(1:4).NE.'OPAQ')SOTF(ICOMP,NSUR)='TRAN'
          IF(mlctype(ISEL)(1:4).EQ.'OPAQ')SOTF(ICOMP,NSUR)='OPAQUE'
          IF(mlctype(ISEL)(1:4).EQ.'CFC ')SOTF(ICOMP,NSUR)='CFC '
          IF(mlctype(ISEL)(1:4).EQ.'CFC2')SOTF(ICOMP,NSUR)='CFC2'
        else
          WRITE(SMLCN(ICOMP,NSUR),'(A)') 'UNKNOWN'
          SOTF(ICOMP,NSUR)='OPAQUE'
        endif
      endif

C If version 1.1 of geometry specify the use of the surface.
      call eclose(gversion(icomp),1.1,0.01,newgeo)
      if(newgeo)then
        if(act(1:2).eq.'sw'.or.act(1:2).eq.'sd'.or.
     &     act(1:2).eq.'sp')then
          WRITE(SUSE(ICOMP,NSUR,1),'(A)')  rsuse1(1:8)
          WRITE(SUSE(ICOMP,NSUR,2),'(A)')  rsuse2(1:8)
          isur=nsur
        else
          isur=nsur
          WRITE(SUSE(ICOMP,NSUR,1),'(A)') '- '
          WRITE(SUSE(ICOMP,NSUR,2),'(A)') '- '
        endif
      endif

C Process holes within a surface types.
      if(IRT.eq.1.or.IRT.eq.3.or.IRT.eq.4)then
        ICS=isur
        call mergechildinparent(1,icomp,IPS,ICS,IRT,ier)

C Feedback.
        write(louts,'(a,124i4)') 'parent surf after ',
     &    (jvn(IPS,j),J=1,NVER(IPS))
        call edisp248(iuout,louts,100)

C Since this rectangle is a new surface, add it to the connection list
C assuming it has the same boundary conditions as the parent surface
C (but for a partition set to unknown).
        NTV=NTV+loop    ! update number of vertices in zone
        NZTV(icomp)=NTV
        NCON=NCON+1
        IX=NCON+1

C Update common blocks G6 & G7 (insert data into connections based list).
        call adjcbg6g7('+',ix,icont)

        IC1(ICONT)=ICOMP
        IE1(ICONT)=NSUR
        if(ICT(ICONO).eq.3)then
          ICT(ICONT)=-1  ! if parent a partition set child to unknown
          IC2(ICONT)=0
          IE2(ICONT)=0
        else
          ICT(ICONT)=ICT(ICONO)
          IC2(ICONT)=IC2(ICONO)
          IE2(ICONT)=IE2(ICONO)
        endif
        IZSTOCN(icomp,nsur)=icont

C Discover matching MLC index.
        call matchmlcdesc(SMLCN(icomp,NSUR),imlcindex)
        smlcindex(icomp,nsur)=imlcindex
        isznver(icomp,NSUR)=NVER(NSUR)  ! might not be necessary
        do K=1,isznver(icomp,NSUR)
          iszjvn(icomp,NSUR,K)=JVN(NSUR,K)
        enddo

C Now update the G7 common block to reflect additional surface.
        call zgupdate(1,icomp,ier)

C Signal to update the image if not in one of the silent modes.
        if(act(1:2).eq.'sw'.or.act(1:2).eq.'sd'.or.
     &     act(1:2).eq.'sp')then
          continue
        else
          MODIFYVIEW=.TRUE.
        endif
        call edisp(iuout,' ')

      elseif(IRT.eq.2)then

C A door topology might also apply to surfaces which share
C one or more edges with the parent. Return icwhich1 & icwhich2
        ICS=isur
        call mergedoorinparent(1,icomp,IPS,ICS,icwhich1,icwhich2,ier)

C Since this rectangle is a new surface, add it to the connection list
C assuming it has the same boundary conditions as the parent surface
C (but for a partition set to unknown).
        NTV=NTV+loop    ! update number of vertices in zone
        NZTV(icomp)=NTV
        NCON=NCON+1
        IX=NCON+1

C Update common blocks G6 & G7 (insert data into connections based list).
        call adjcbg6g7('+',ix,icont)

        IC1(ICONT)=ICOMP
        IE1(ICONT)=NSUR
        if(ICT(ICONO).eq.3)then
          ICT(ICONT)=-1  ! if parent a partition set child to unknown
          IC2(ICONT)=0
          IE2(ICONT)=0
        else
          ICT(ICONT)=ICT(ICONO)
          IC2(ICONT)=IC2(ICONO)
          IE2(ICONT)=IE2(ICONO)
        endif
        IZSTOCN(icomp,nsur)=icont

C Discover matching MLC index.
        call matchmlcdesc(SMLCN(ICOMP,NSUR),imlcindex)
        smlcindex(icomp,nsur)=imlcindex
        isznver(icomp,NSUR)=NVER(NSUR)  ! might not be necessary
        do K=1,isznver(icomp,NSUR)
          iszjvn(icomp,NSUR,K)=JVN(NSUR,K)
        enddo

C Now update the G7 common block to reflect additional surface.
        call zgupdate(1,icomp,ier)

C Signal to update the image if not in one of the silent modes.
        if(act(1:2).eq.'sw'.or.act(1:2).eq.'sd'.or.
     &     act(1:2).eq.'sp')then
          continue
        else
          MODIFYVIEW=.TRUE.
        endif
        call edisp(iuout,' ')

C Note the new vertices icwhich1 & icwhich2 might need to be
C inserted into other surfaces in the zone. Ignore the parent
C and the door when looking.
        iadd=2
        do 143 ix = 1,IADD
          if(ix.eq.1)then
            iwhich3=icwhich1
          elseif(ix.eq.2)then
            iwhich3=icwhich2
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

C If iwhich1 & iwhich2 are the same skip this zero length line.
              if(iwhich1.eq.iwhich2) goto 247

C If either iwhich1 & iwhich2 are zero skip this zero length line.
              if(iwhich1.eq.0.or.iwhich2.eq.0) goto 247

C Report length of line. Use method of Ward/Radiance in fvect.c
              call pointtoline(iwhich3,iwhich1,iwhich2,offset,
     &          matchver)
              if(.NOT.matchver) goto 247
              if(offset.lt.CACC)then
                write(outs,'(a,i3,a,3f8.3,a,f6.4,a,i3,a,i3,2a)')
     &            'Door vertex ',iwhich3,' @',
     &            X(NTV),Y(NTV),Z(NTV),' is close (',offset,
     &            ') to edge ',IWHICH1,' & ',IWHICH2,' of ',
     &            sname(icomp,ivj)
                call edisp(iuout,outs)

C If current surface (ivj) can take another vertex expand the
C list. Logic works by looping down (from one more than the current
C number of vertices associated with this surface) shifting
C JVN indices up one until at the current edge (ivjj) and then
C inserting the new vertex index.
                if(NVER(ivj)+1.le.MV)then
                  if(offset.le.CACC)then
                    ok=.true.
                  else
                    helptopic='vertex_close_to_edge'
                    call gethelptext(helpinsub,helptopic,nbhelp)
                    call easkok(' ',
     &                'Insert vertex into adjacent surface?',
     &                ok,nbhelp)
                  endif
                else
                  ok=.false.
                endif
                if(ok)then
                  NVER(ivj)=NVER(ivj)+1
                  isznver(icomp,ivj)=NVER(ivj)
                  IXV=NVER(ivj)+1
  148             continue
                  IXV=IXV-1
                  JVN(ivj,IXV)=JVN(ivj,IXV-1)
                  iszjvn(icomp,ivj,IXV)=JVN(ivj,IXV)
                  IF(IXV.GT.ivjj+1)GOTO 148
                  JVN(ivj,ivjj+1)=iwhich3
                  iszjvn(icomp,ivj,ivjj+1)=iwhich3

C Debug.
C                  write(6,'(a,124i4)') 'now jvn is ',
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
      helptopic='mods_need_checking'
      call gethelptext(helpinsub,helptopic,nbhelp)
      if(act(1:2).eq.'sw'.or.act(1:2).eq.'sd'.or.act(1:2).eq.'sp')then
        continue
      else
        CALL PHELPD('surface details',nbhelp,'-',0,0,IER)
      endif
      MODIFYVIEW=.TRUE.

      RETURN
      END


C ************* insvertinsurf
C Computer vertex within a surface via offsets from lower left. Useful
C for sloped and/or complex surfaces.
C ITRU = unit number for user output, IER=0 OK, IER=1 problem.
C New point returned in x3,y3,z3.
      SUBROUTINE insvertinsurf(ITRC,ICOMP,IS,XO1,ZO1,
     &  x3,y3,z3,IER)
#include "building.h"
#include "geometry.h"
C #include "prj3dv.h"
#include "esprdbfile.h"
#include "material.h"

C      integer lnblnk  ! function definition

C Parameters
      integer itrc,itru  ! tracel level and reporting unit
      integer icomp      ! zone number
      integer is         ! surface number
      real XO1,ZO1,offset  ! X and Z offset
      real x3,y3,z3      ! new point
      integer IER        ! zero is ok

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      real TMAT,RMAT,VP,EP,EQN
      DIMENSION  XX(MV),YY(MV),ZZ(MV)
      DIMENSION  TMAT(4,4),RMAT(4,4)
      DIMENSION  VP(3),EP(3),EQN(4)
      CHARACTER OUTSTR*124

C Transform existing surface into into site coordinates in the
C surface of the plane. Make up XX,YY,ZZ to pass across to the
C transform routine.
      N = NVER(IS)
      DO 150 J = 1,N
        XX(J) = X(JVN(IS,J))
        YY(J) = Y(JVN(IS,J))
        ZZ(J) = Z(JVN(IS,J))
  150 CONTINUE

C Logic from ETRANSW
C Find transformation matrices that normalise face.
      call PLEQN(XX,YY,ZZ,N,VP,EQN,IER)
      IF(IER.LT.0)CALL EDISP(iuout,'insvertinsurf plane equation error')

      DO 250 J = 1,3
        EP(J) = VP(J) + EQN(J)
  250 CONTINUE
      IF(ITRC.GT.1)THEN
        CALL EDISP(ITRU,' Plane equation data: ')
        WRITE(OUTSTR,'(a,3F8.3)')' Center of grav:',(VP(I),I=1,3)
        CALL EDISP(iuout,OUTSTR)
        WRITE(OUTSTR,'(a,4F8.3)')' Equation:',(EQN(I),I=1,4)
        CALL EDISP(iuout,OUTSTR)
        WRITE(OUTSTR,'(a,3F8.3)')' Eye Point:',(EP(I),I=1,3)
        CALL EDISP(iuout,OUTSTR)
      ENDIF

C Call eyemat with 1m offset.
      offset=1.00
      CALL EYEMAT(EP,VP,offset,TMAT,RMAT)

C Transform all points in surface and find lower left corner.
      XMIN=100.0; YMIN=100.0
      DO 300 I=1,N
        CALL ORTTRN(XX(I),YY(I),ZZ(I),TMAT,X1,Y1,ZZZ,IERR)
        ier=ierr
        IF(X1.LT.XMIN)XMIN=X1
        IF(Y1.LT.YMIN)YMIN=Y1
  300 CONTINUE

C Apply reverse transformation.
C      write(6,*) 'icomp is xmin ymin xxv yyv zzz',
C     &  icomp,is,xmin,ymin,xxv,yyv,zzz
      XXV=XMIN+XO1
      YYV=YMIN+ZO1
      CALL ORTTRN(XXV,YYV,ZZZ,RMAT,X3,Y3,Z3,IERR)
      WRITE(OUTSTR,'(a,3F9.5)') 'New XYZ coords:',X3,Y3,Z3
      CALL EDISP(iuout,OUTSTR)

      RETURN
      END   ! of insvertinsurf

C ********* adjcbg6g7
C Adjusts common blocks C3, G6 and G7.
C Passed act:
C act = `+` does the SS(ix)=SS(ix-1) bit and ix is one more
C           than the current number of connections and icon
C           is the point of insertion (so as to know when to
C           stop shifting).
      subroutine adjcbg6g7(act,ix,icon)
#include "building.h"
#include "geometry.h"

C << potential place to track edges which might fall into specific
C << thermal bridge categories. ? extend G8 ?
      
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      CHARACTER act*1

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

C Is this needed?
        SUREQN(IC1(IX),IE1(IX),1)=SUREQN(IC1(IX-1),IE1(IX-1),1)
        SUREQN(IC1(IX),IE1(IX),2)=SUREQN(IC1(IX-1),IE1(IX-1),2)
        SUREQN(IC1(IX),IE1(IX),3)=SUREQN(IC1(IX-1),IE1(IX-1),3)
        SUREQN(IC1(IX),IE1(IX),4)=SUREQN(IC1(IX-1),IE1(IX-1),4)
        SURVN(IC1(IX),IE1(IX),1)=SURVN(IC1(IX-1),IE1(IX-1),1)
        SURVN(IC1(IX),IE1(IX),2)=SURVN(IC1(IX-1),IE1(IX-1),2)
        SURVN(IC1(IX),IE1(IX),3)=SURVN(IC1(IX-1),IE1(IX-1),3)
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

C ****************** addedsurf
C addedsurf takes the current G1 & G5 common block values and copies them
C to the G6 common block. It inserts default surface information into the 
C connections-based data structures.
      subroutine addedsurf(icomp,icon,itrc,ier)
#include "building.h"
#include "geometry.h"

C Parameters
      integer icomp ! is the index of the zone
      integer icon  ! is the index of the new connection (from parent code)
      integer itrc  ! verbosity
      integer ier   ! is non-zero if the surface could not be added
      
      COMMON/OUTIN/IUOUT,IUIN,IEOUT
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      integer IZSTOCN
      COMMON/C24/IZSTOCN(MCOM,MS)
      integer IX,ival1,ival2
      character outs*124

C Update the connection list. Move all others up and then insert with
C default values.
      IF(NCON.LT.MCON)THEN
        NCON=NCON+1
        IX=NCON+1

C Update common blocks C3, G6 & G7.
        call adjcbg6g7('+',ix,icon)

        IC1(ICON)=ICOMP
        IE1(ICON)=NSUR
        ICT(ICON)= -1  ! initial assumption of unknown
        IC2(ICON)=0
        IE2(ICON)=0
        IZSTOCN(icomp,nsur)=icon

C Re-establish link between zone/surfaces and connections.
        do iccc = 1, NCON
          IZSTOCN(IC1(iccc),IE1(iccc))=iccc
        enddo

C Discover matching MLC index.
        call matchmlcdesc(SMLCN(ICOMP,NSUR),imlcindex)
        smlcindex(ICOMP,NSUR)= imlcindex
        isznver(icomp,NSUR)=NVER(NSUR)  ! might not be necessary
        do K=1,isznver(icomp,NSUR)
          iszjvn(icomp,NSUR,K)=JVN(NSUR,K)
        enddo

C If sother(ICOMP,NSUR,1) is 'ANOTHER' or one of the other known phrases
C then the values of ICT and IC2 and IE2 should be updated so that
C the connections file is correct.
        if(SOTHER(ICOMP,NSUR,1)(1:7).eq.'ANOTHER')then
          read(SOTHER(ICOMP,NSUR,2),'(i2.2)') ival1
          read(SOTHER(ICOMP,NSUR,3),'(i3.3)') ival2
          ICT(ICON)=3; IC2(ICON)=ival1; IE2(ICON)=ival2
        elseif(SOTHER(ICOMP,NSUR,1)(1:9).eq.'ADIABATIC')then
          ICT(ICON)=5
        elseif(SOTHER(ICOMP,NSUR,1)(1:8).eq.'CONSTANT')then
          read(SOTHER(ICOMP,NSUR,2),'(i2.2)') ival1
          read(SOTHER(ICOMP,NSUR,3),'(i3.3)') ival2
          ICT(ICON)=2; IC2(ICON)=ival1; IE2(ICON)=ival2
        elseif(SOTHER(ICOMP,NSUR,1)(1:6).eq.'GROUND')then
          read(SOTHER(ICOMP,NSUR,2),'(i2.2)') ival1
          read(SOTHER(ICOMP,NSUR,3),'(i3.3)') ival2
          ICT(ICON)=4; IC2(ICON)=ival1; IE2(ICON)=ival2
        elseif(SOTHER(ICOMP,NSUR,1)(1:7).eq.'SIMILAR')then
          read(SOTHER(ICOMP,NSUR,2),'(i2.2)') ival1
          read(SOTHER(ICOMP,NSUR,3),'(i3.3)') ival2
          ICT(ICON)=1; IC2(ICON)=ival1; IE2(ICON)=ival2
        endif

C Now update the G7 common block to reflect additional surface.
        call zgupdate(itrc,icomp,ier)
      else
        write(outs,'(a,i4,a,i4,a,i2,a,i3)') 
     &    'addedsurf: skip adding connect ',
     &    NCON,' > ',MCON,' for zone',icomp,' connection',icon
        call edisp(iuout,outs)
        ier=1  ! pass back error state
      endif
      return
      end

C ******************
C mergedoorinparent works with current common block variables.
C Called from INSREC as well as clickonbitmap.
      subroutine mergedoorinparent(itrc,IZ,IPS,ICS,icwhich1,icwhich2,
     &  ier)
#include "building.h"
#include "geometry.h"
      integer itrc ! 0 silent 1 chatter
      integer IZ   ! current zone
      integer IPS  ! parent surface index
      integer ICS  ! child surface index
      integer ier  ! if error condition
      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      integer icwhich1,icwhich2,icvwhich1,icvwhich2
      integer ipwhich1a,ipwhich2a,ipwhich1b,ipwhich2b
      integer ipvwhich1a,ipvwhich2a,ipvwhich1b,ipvwhich2b
      integer ivc,ivll
      logical matchver
      character louts*496
      
C A door topology might also apply to surfaces which share
C one or more edges with the parent. First step is to loop
C through all of the child vertices and see if they are
C on one of the parent edges.
C Feedback.
      if(IPS.eq.0.or.ICS.eq.0)then
        call edisp(iuout,'Index of door or parent is zero.')
        ier=1
        return
      endif
      if(NVER(ICS).gt.0)then
        write(louts,'(a,2i3,124i4)') 'door surf jvn ',
     &    ICS,NVER(ICS),(jvn(ICS,j),J=1,NVER(ICS))
        if(itrc.gt.0)call edisp248(iuout,louts,100)
      else
        call edisp(iuout,'No door vertices found.')
        ier=1
        return
      endif
      if(NVER(IPS).gt.0)then
        write(louts,'(a,2i3,124i4)') 'parent surf jvn ',
     &    IPS,NVER(IPS),(jvn(IPS,j),J=1,NVER(IPS))
        if(itrc.gt.0)call edisp248(iuout,louts,100)
      else
        call edisp(iuout,'No parent vertices found.')
        ier=1
        return
      endif
      icwhich1=0; icwhich2=0   ! vertex index
      icvwhich1=0; icvwhich2=0 ! vertex position
      ipwhich1a=0; ipwhich2a=0   ! parent edge vertex index
      ipwhich1b=0; ipwhich2b=0   ! parent edge vertex index
      ipvwhich1a=0; ipvwhich2a=0   ! parent vertex position
      ipvwhich1b=0; ipvwhich2b=0   ! parent vertex position

      do ivc=1,NVER(ICS)  ! for each child vertex
        item=jvn(ICS,ivc)   ! vert of the child
        do iyy = 1,NVER(IPS)-1   ! for each parent vertex
          iptem1=jvn(IPS,iyy); iptem2=jvn(IPS,iyy+1)
          call pointtoline(item,iptem1,iptem2,offset,matchver)
          if(offset.lt.0.002.and.matchver)then  ! if within 2mm
            if(icwhich1.eq.0)then
              icwhich1=item  ! remember first match
              icvwhich1=ivc  ! and its position
              ipwhich1a=iptem1
              ipwhich1b=iptem2
              ipvwhich1a=iyy
              ipvwhich1b=iyy+1
            else
              icwhich2=item  ! remember 2nd match
              icvwhich2=ivc  ! and its position
              ipwhich2a=iptem1
              ipwhich2b=iptem2
              ipvwhich2a=iyy
              ipvwhich2b=iyy+1
            endif
          endif
        enddo
      enddo

C In the parent surface we loop backwards until we are just
C after ipvwhich1a we then insert door vertex icwhich1 
C then loop backwards (3 times) within the door vertex list.
C Debug.
C      write(6,*) 'door verts @ edge ',
C     &  icwhich1,icvwhich1,icwhich2,icvwhich2,ipwhich1a,ipvwhich1a,
C     &  ipwhich1b,ipvwhich1b,ipwhich2a,ipvwhich2a,ipwhich2b,ipvwhich2b
      ivll=ipvwhich1a  ! remember it
C Debug.
C      write(6,'(a,i3,124i4)') 'ivll parent surf jvn ',
C     &  ivll,(jvn(IPS,j),J=1,NVER(IPS))

C Start at end of list and work backwards. Shift existing
C members of the list and then at the insertion point (ivll)
C place the child vert and shift the insertion point to 
C the right. NC begins one past NVER(ICS) because we first
C need to insert icwhich1. Use icstart logic for this.
      icstart=0
      NC=NVER(ICS)+1
      do iyy = NC,2,-1  ! loop for 3 edges of door plus icwhich1
        NVER(IPS)=NVER(IPS)+1  ! increment parent
        isznver(IZ,IPS)=NVER(IPS)
        iii=NVER(IPS)+1
  448   continue
        iii=iii-1
        jvn(IPS,iii)=jvn(IPS,iii-1)  ! copy lower existing up
        iszjvn(iz,ips,iii)=jvn(IPS,iii)
        IF(iii.GT.ivll+1)GOTO 448
        if(icstart.eq.0)then
          jvn(IPS,iii)=icwhich1 ! assign icwhich1
          iszjvn(iz,IPS,iii)=icwhich1
          ivll=ivll+1  ! shift insertion point to the right
          icstart=1
        elseif(icstart.eq.1)then
          jvn(IPS,iii)=jvn(ICS,iyy) ! assign child vertex
          iszjvn(iz,IPS,iii)=jvn(IPS,iii)
          ivll=ivll+1  ! shift insertion point to the right
        endif
      enddo

C Feedback.
      write(louts,'(a,i3,124i4)') 'parent surf after ',iyy,
     &  (jvn(IPS,j),J=1,NVER(IPS))
      if(itrc.gt.0)call edisp248(iuout,louts,100)
      return
      end

C ******************
C mergechildinparent works with current common block variables.
C Called from INSREC as well as clickonbitmap. Assumes that
C common G1 is fresh.
      subroutine mergechildinparent(itrc,IZ,IPS,ICS,IRT,ier)
#include "building.h"
#include "geometry.h"
      integer itrc ! 0 silent 1 chatter
      integer IZ   ! current zone
      integer IPS  ! parent surface
      integer ICS  ! child surface
      integer IRT  ! 1=simple child 3= % 4= frame 5=
      integer ier  ! if error condition

      COMMON/OUTIN/IUOUT,IUIN,IEOUT

      integer llpos,lrpos,ulpos,urpos     ! closest to BB corners for parent
      integer llparpos,lrparpos,ulparpos,urparpos
      integer lldpos,lrdpos,uldpos,urdpos
      integer ivll
      real shortestll,shortestlr,shortestul,shortestur
      real overall
      character louts*496

      integer jvn1,jvn2
      DIMENSION  jvn1(MV),jvn2(MV)

C Logic for merging vertices into the parent surface
C based on what is closest.
C Debug.
      isur=ICS
      write(louts,'(a,i3,124i4)') 'parent surf jvn ',IPS,
     &  (jvn(IPS,j),J=1,NVER(IPS))
      if(itrc.gt.0)call edisp248(iuout,louts,100)
      write(louts,'(a,2i3,124i4)') 'child surf jvn ',
     &  isur,NVER(isur),(jvn(isur,j),J=1,NVER(isur))
      if(itrc.gt.0)call edisp248(iuout,louts,100)
      CALL SURLEHI(IPS,XYMAX,ZMAX,llpos,lrpos,ulpos,urpos)
C      write(6,*) 'par llpos lrpos ulpos urpos',llpos,lrpos,ulpos,urpos
      CALL SURLEHI(ICS,XYMAX,ZMAX,lldpos,lrdpos,uldpos,urdpos)
C      write(6,*) 'win lldpos lrdpos uldpos urdpos',
C     &  lldpos,lrdpos,uldpos,urdpos

C For the case of a frame or % go with LL for both the parent and the child.
      if(IRT.eq.3.or.IRT.eq.4)then
        ipwhich=llpos
        icwhich=lldpos
        item=jvn(ICS,lldpos)   ! LL of the child
        xitem=X(item); yitem=Y(item); zitem=Z(item)
        shortestll=100.0
        iptem=jvn(IPS,llpos)
        xpitem=X(iptem); ypitem=Y(iptem); zpitem=Z(iptem)
        vdis= crowxyz(xitem,yitem,zitem,xpitem,ypitem,zpitem)
        if(vdis.lt.shortestll) shortestll=vdis
C        write(6,*) 
C     &    'frame parent jvn position closest to child LL LR UR UL',
C     &    llpos,lrpos,urpos,ulpos,' dist ',
C     &    shortestll
C        write(6,*) 
C     &    'frame shortest dist ',shortestll,' parent pos ',ipwhich,
C     &    ' parent vert is ',jvn(IPS,ipwhich),' @',
C     &    X(jvn(IPS,ipwhich)),Y(jvn(IPS,ipwhich)),Z(jvn(IPS,ipwhich))
        icvert=jvn(ICS,icwhich)
C        write(6,*) 'frame child vertex pos is ',icwhich,
C     &    ' child vert is ',icvert,' @',
C     &    X(icvert),Y(icvert),Z(icvert)
      else
      
C For each of the child vertex closest to LL LR UL UR find the
C distance to parent vertices and report on the shortest.
        ipwhich=0; icwhich=0; overall=100.0
        item=jvn(ICS,lldpos)   ! LL of the child
        xitem=X(item); yitem=Y(item); zitem=Z(item)
        shortestll=100.0; llparpos=0
        do iyy = 1,NVER(IPS)   ! apply iopenjvn to jvn
          iptem=jvn(IPS,iyy)
          xpitem=X(iptem); ypitem=Y(iptem); zpitem=Z(iptem)
          vdis= crowxyz(xitem,yitem,zitem,xpitem,ypitem,zpitem)
          if(vdis.lt.shortestll)then
            shortestll=vdis; llparpos=iyy
          endif
        enddo
        if(shortestll.lt.overall)then
          overall=shortestll; ipwhich=llparpos; icwhich=lldpos
        endif
        item=jvn(ICS,lrdpos)   ! LR of the child
        xitem=X(item); yitem=Y(item); zitem=Z(item)
        shortestlr=100.0; lrparpos=0
        do iyy = 1,NVER(IPS)   ! apply iopenjvn to jvn
          iptem=jvn(IPS,iyy)
          xpitem=X(iptem); ypitem=Y(iptem); zpitem=Z(iptem)
          vdis= crowxyz(xitem,yitem,zitem,xpitem,ypitem,zpitem)
          if(vdis.lt.shortestlr)then
            shortestlr=vdis; lrparpos=iyy
          endif
        enddo
        if(shortestlr.lt.overall)then
          overall=shortestlr; ipwhich=lrparpos; icwhich=lrdpos
        endif
        item=jvn(ICS,urdpos)   ! UR of the child
        xitem=X(item); yitem=Y(item); zitem=Z(item)
        shortestur=100.0; urparpos=0
        do iyy = 1,NVER(IPS)   ! apply iopenjvn to jvn
          iptem=jvn(IPS,iyy)
          xpitem=X(iptem); ypitem=Y(iptem); zpitem=Z(iptem)
          vdis= crowxyz(xitem,yitem,zitem,xpitem,ypitem,zpitem)
          if(vdis.lt.shortestur)then
            shortestur=vdis; urparpos=iyy
          endif
        enddo
        if(shortestur.lt.overall)then
          overall=shortestur; ipwhich=urparpos; icwhich=urdpos
        endif
        item=jvn(ICS,uldpos)   ! UR of the child
        xitem=X(item); yitem=Y(item); zitem=Z(item)
        shortestul=100.0; ulparpos=0
        do iyy = 1,NVER(IPS)   ! apply iopenjvn to jvn
          iptem=jvn(IPS,iyy)
          xpitem=X(iptem); ypitem=Y(iptem); zpitem=Z(iptem)
          vdis= crowxyz(xitem,yitem,zitem,xpitem,ypitem,zpitem)
          if(vdis.lt.shortestul)then
            shortestul=vdis; ulparpos=iyy
          endif
        enddo
        if(shortestul.lt.overall)then
          overall=shortestul; ipwhich=ulparpos; icwhich=uldpos
        endif
C        write(6,*) 'parent jvn position closest to child LL LR UR UL',
C     &    llparpos,lrparpos,urparpos,ulparpos,' dist ',
C     &    shortestll,shortestlr,shortestur,shortestul
C        write(6,*) 'shortest dist ',overall,' parent pos ',ipwhich,
C     &    ' parent vert is ',jvn(IPS,ipwhich),' @',
C     &    X(jvn(IPS,ipwhich)),Y(jvn(IPS,ipwhich)),Z(jvn(IPS,ipwhich))
        icvert=jvn(ICS,icwhich)
C        write(6,*) 'child vertex pos is ',icwhich,
C     &    ' child vert is ',icvert,' @',
C     &    X(icvert),Y(icvert),Z(icvert)
      endif

C For the child use cyclejvn to get an ordering that begins at
C the childs closest vertex.
      do iyy = 1,NVER(ICS)   ! make arrays to pass
        jvn1(iyy)=jvn(ICS,iyy)
        jvn2(iyy)=0
      enddo
      call cyclejvn(jvn1,NVER(ICS),icvert,jvn2)
C Debug.
C      write(6,*) 'jvn1 ',(jvn1(j),j=1,NVER(ICS))
C      write(6,*) 'jvn2 ',(jvn2(j),j=1,NVER(ICS))

      if(IRT.eq.1.or.IRT.eq.3.or.IRT.eq.4)then

C The process for a surface-within is to:
C locate position ipwhich in the parent and then insert
C icvert and then looping down each of the vertices in
C the child followed by jvn(IPS,ipwhich).
        ivll=ipwhich  ! remember it
C Debug.
C        write(6,'(a,i3,124i4)') 'ivll parent surf jvn ',
C     &    ivll,(jvn(IPS,j),J=1,NVER(IPS))

C Expand the list by looping down (from one more than the current
C number of vertices associated with parent surface) shifting
C JVN indices up one until at the lower left corner and then
C inserting the new vertes for the door.

C If ipwhich location in the list is actually the last in the list?
        n=NVER(IPS)
        if(ivll.eq.n)then

C Add icvert to parent.
          icstart=0
          nc=NVER(ICS)
          NVER(IPS)=NVER(IPS)+1  ! increment parent
          isznver(IZ,IPS)=NVER(IPS)
          n=NVER(IPS)        
          jvn(IPS,n)=icvert ! assign
          iszjvn(IZ,IPS,n)=icvert
          do iyy = NC,1,-1     ! for each of the child vertices
            NVER(IPS)=NVER(IPS)+1  ! increment parent
            isznver(IZ,IPS)=NVER(IPS)
            n=NVER(IPS)        
            jvn(IPS,n)=jvn2(iyy) ! assign
            iszjvn(IZ,IPS,n)=jvn2(iyy)
          enddo

C Add jvn(IPS,ipwhich) so as to rejoin the parent.
          NVER(IPS)=NVER(IPS)+1  ! increment parent
          isznver(IZ,IPS)=NVER(IPS)
          n=NVER(IPS)        
          jvn(IPS,n)=jvn(IPS,ipwhich) ! assign
          iszjvn(IZ,IPS,n)=jvn(IPS,n)

C Feedback.
          write(louts,'(a,124i4)') 'parent surf after ',
     &      (jvn(IPS,j),J=1,NVER(IPS))
          if(itrc.gt.0)call edisp248(iuout,louts,100)
        else

C Start at end of list and work backwards. Shift existing
C members of the list and then at the insertion point (ivll)
C place the child vert and shift the insertion point to 
C the right. NC begins one past NVER(ICS) because we first
C need to insert icvert. Use icstart logic for this. When
C we reach iyy=0 then insert jvn(IPS,ipwhich).
          icstart=0
          NC=NVER(ICS)+1
          do iyy = NC,0,-1  ! add iopenjvn to parent
            NVER(IPS)=NVER(IPS)+1  ! increment parent
            isznver(IZ,IPS)=NVER(IPS)
            iii=NVER(IPS)+1
  348       continue
            iii=iii-1
            jvn(IPS,iii)=jvn(IPS,iii-1)  ! copy lower existing up
            iszjvn(IZ,IPS,iii)=jvn(IPS,iii)
            IF(iii.GT.ivll+1)GOTO 348
            if(icstart.eq.0)then
              jvn(IPS,iii)=icvert ! assign icvert
              iszjvn(IZ,IPS,iii)=icvert
              ivll=ivll+1  ! shift insertion point to the right
              icstart=1
            elseif(icstart.eq.1)then
              if(iyy.gt.0)then
                jvn(IPS,iii)=jvn2(iyy) ! assign
                iszjvn(IZ,IPS,iii)=jvn2(iyy)
                ivll=ivll+1  ! shift insertion point to the right
              else
                jvn(IPS,iii)=jvn(IPS,ipwhich) ! re-connect
                iszjvn(IZ,IPS,iii)=jvn(IPS,iii)
                ivll=ivll+1  ! shift insertion point to the right
              endif
            endif
C Debug.
C            write(6,'(a,124i4)') 'parent surf adj ',
C     &        (jvn(IPS,j),J=1,NVER(IPS))
          enddo

C Feedback.
          write(louts,'(a,124i4)') 'parent surf after ',
     &      (jvn(IPS,j),J=1,NVER(IPS))
          if(itrc.gt.0)call edisp248(iuout,louts,100)
        endif
      endif
      return
      end

