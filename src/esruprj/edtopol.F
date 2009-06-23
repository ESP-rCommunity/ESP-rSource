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

C The file edtopol.f is a collection of support facilities for 
C creating and editing the model topology ie. connection lists:
C  EDCONN: Edit/check/clear connection attributes in common block C3
C          and includes logic that use to be found in module cfg.
C  CONFIG: processes the vertex data and presents close matches
C          to confirm before updating cfg and cnn files.
C  ESACON: import surface connection attributes into common
C          block C3 while taking into account confirmation preferences.
C  anchor:   anchor point items (a list of connections & concept).
C  anchlist: manage list of anchors
C  easkconn: Select one or more connections from a list.
C  PROCGDST process GDST command sets (defined in common blocks
C           idsta and anchora.
C  Serchrpl handles search and replace of composition attributes.

C ************* EDCONN 
C Edit/clear/check connection attributes in common blocks via a paging menu.
C ITRU = unit number for user output, IER=0 OK, IER=1 problem.
      SUBROUTINE EDCONN(IER)
#include "building.h"

C geometry.h provides commons G0/G2/c20.
#include "geometry.h"
#include "espriou.h"
C espriou.h provides currentfile.
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      common/pophelp/h(60)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      common/C3F/LCNN
      common/C21/IFCFG,cfgroot,LCFGF
      common/CFGV/icfgv
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

C ITDSP labels toggle: all surf + obs = 0, all surf = 1, partn = 2,
C       similar = 4, surfs + obs+ ground = 5, ground only = 6
C ITBND bounds toggle: static = 0, optimum = 1, zone focus = 2
C ITEPT is
C ITZNM zone name toggle: display = 0, hidden = 1
C ITSNM surface name toggle: display = 0, hidden = 1
C ITORG origin toggle: display = 0, hidden = 1
C ITSNR surf normal toggle: display = 0, hidden = 1.
C ITOBS obstruction toggle: not yet enabled.
C ITHLS highlight toggle: normal 0, constr 1, trans/opaq/cfc 2, part atrib 3
C ITGRD grid toggle: display = 0, hidden = 1
C ITVNO vertex toggle: display = 0, hidden = 1
C ITPPSW current view - perspective/plan/south/west
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND

C Pause length during sequentail scan longpause (true=1 sec)
      common/scanpause/longpause

      LOGICAL MODIFY,MODLEN,MODBND,OTHEROK
      logical M0SKP,M1SKP,M2SKP,M3SKP,M4SKP,M5SKP,M6SKP,M7SKP,M8SKP
      logical M1OK,M2OK,M4OK,M5OK,M6OK,M7OK
      LOGICAL prob,OK,dok,unixok
      logical longpause
      logical newgeo  ! to use for testing if new/old geometry file.

      DIMENSION ITEMS1(23)
      CHARACTER CXSTR*78,H*72,outs*124,ITEMS1*34,CFGFN*72
      CHARACTER LCFGF*72
      character cfgroot*24
      character DCNN*72,LCNN*72
      character bl*2

C Check if Unix-based or DOS based.
      call isunix(unixok)
      longpause=.true.     ! initially use longer pause
      newgeo=.false.  ! assume older format geometry.

C General image option flags for use with cadjview.
      ITDSP=1
      ITBND=1
      ITEPT=0
      ITZNM=0
      ITSNM=0
      ITVNO=1
      ITORG=1
      ITSNR=1
      ITGRD=1
      GRDIS=0.0
      ITRC=0

C On first entry, see if the list is consistent with geometry. If
C prob is .true. then there is at least some inconsistency. The value
C of iprob is the number of inconsistencies. If more than 10
C inconsistencies then something should be done. There are several
C possible vectors for disagreements between what is held in the
C zone geometry files and the connection information held in
C the model connections file:
C a) prj crashed while managing the files resulting in incomplete
C    files (either *.geo or *.cnn)
C b) a 3rd party agent altered the contents of the model files
C    and introduced an error
C c) a glitch in the source code allowed an inconsistency into
C    the model.

C Typically it is assumed that the geometry file is correct and
C the connections file has been corrupted. Thus the `clear` function
C relies on re-scanning the geometry files, updating the values in
C the nzsur() array and then re-setting the common block C3 data.

C Because legacy zone geometry files only held portions of the connetion
C information equivalent to IC1 IE1 and ICT user intervention is needed
C to fully re-establish the C3 common block. The version 1.1 geometry
C file holds sufficient information to re-establish the C3 common
C block with the help of topology checking logic.

C Assume that all surfaces marked as identical, constant, ground and
C adiabatic  and back-to-back require no confirmation (if no
C geometric match is found).
C Assume all existing partitions are re-checked and 
C failed searches brought to users attention.
      M0SKP=.FALSE.
      M1SKP=.TRUE.
      M2SKP=.TRUE.
      M3SKP=.FALSE.
      M4SKP=.TRUE.
      M5SKP=.TRUE.
      M6SKP=.TRUE.
      M7SKP=.TRUE.
      M8SKP=.FALSE.  !  back-to-back in zone assumed to be partitions
      OTHEROK=.TRUE.
      MODIFY=.FALSE.

C CACC is the tolerance between two vertices, IACC is the number
C of corners beyond tolerance which are allowed,
C DACC is the line degree tolerance for setting of corners.
      CACC=0.010
      IACC=0
      DACC=5.0
      DCNN=' '
      bl='  '

C The inital task is to see if there are easily spotted issues with
C the connections list.
      call usrmsg(
     &  'Checking zone surfaces against master list...',' ','-')
      call ckcurmatch(prob,iprob,ier)
      if(prob)then
        call tstamp('>','PRJ: found topology incinsistent')
        if(IPROB.gt.10)then
          write(outs,*) IPROB,' inconsistencies found!'
          call edisp(iuout,outs)
          call usrmsg(
     &    'Topology list dangerously inconsistent. Suggest you',
     &    'clear the current topology!','W')
        else
          call usrmsg(
     &    'Topology list inconsistent. Either clear current topology',
     &    'and/or use the check topology option to resolve this!','W')
        endif
      else
        call usrmsg(
     &    'Checking zone surfaces against master list...done.',' ','-')
      endif

C Refresh the global data arrays used during the checking.
      NZONES=NCOMP
      ISFSUM=0
      CALL ZDATA (0,IER,NZONES,ISFSUM)

C Set for redraw if image on first entry (if refresh set to after
C each edit). Set flag for updating geometry file (for changed
C surface attribute.
    3 INO=-4
      write(ITEMS1(1),'(a,i4,a)')' Connections (',ncon,')'
      ITEMS1(2)=   '  ______________________      '
      ITEMS1(3)=   'b set vertex `close` tolerance'
      ITEMS1(4)=   '  confirm if (already):       '
      if(M1SKP)then
        ITEMS1(5)= 'c   marked identical : No     '
      else
        ITEMS1(5)= 'c   marked identical : Yes    '
      endif
      if(M2SKP)then
        ITEMS1(6)= 'd   marked constant  : No     '
      else
        ITEMS1(6)= 'd   marked constant  : Yes    '
      endif
      if(M3SKP)then
        ITEMS1(7)= 'e   marked partition : No     '
      else
        ITEMS1(7)= 'e   marked partition : Yes    '
      endif
      if(M4SKP)then
        ITEMS1(8)= 'f   marked ground    : No     '
      else
        ITEMS1(8)= 'f   marked ground    : Yes    '
      endif
      if(M5SKP)then
        ITEMS1(9)= 'g   marked adiabatic : No     '
      else
        ITEMS1(9)= 'g   marked adiabatic : Yes    '
      endif
      if(M6SKP)then
        ITEMS1(10)='h   marked BASESIMP  : No     '
      else
        ITEMS1(10)='h   marked BASESIMP  : Yes    '
      endif
      if(M7SKP)then
        ITEMS1(11)='h   marked IDENT_CEN  : No    '
      else
        ITEMS1(11)='h   marked IDENT_CEN  : Yes   '
      endif
      if(M0SKP)then
        ITEMS1(12)='i   if no match found: No     '
      else
        ITEMS1(12)='i   if no match found: Yes    '
      endif
      if(M8SKP)then
        ITEMS1(13)='j   if back-to-back found: No '
      else
        ITEMS1(13)='j   if back-to-back found: Yes'
      endif
      ITEMS1(14)=  '  _______________________     '

      ITEMS1(15)=  'p edit individual connections'

C If contiguity has problems then hilight the selection.
      if(prob)then
        if(IPROB.gt.10)then
          ITEMS1(16)='q CLEAR ALL contiguity  '
        else
          ITEMS1(16)='q CLEAR SOME/ALL contiguity  '
        endif
      else
          ITEMS1(16)='q clear some/all contiguity  '
      endif
      ITEMS1(17)=    'r check via vertex contiguity'
      ITEMS1(18)=  '  _______________________     '

      IF(ITRC.EQ.0)THEN
        ITEMS1(19)='s reporting >> silent         '
      ELSEIF(ITRC.EQ.1)THEN  
        ITEMS1(19)='s reporting >> summary        ' 
      ELSEIF(ITRC.EQ.2)THEN
        ITEMS1(19)='s reporting >> verbose        '
      ENDIF
      ITEMS1(20)=  '! list connections            '
      if(longpause)then
        ITEMS1(21)='* pause during check 1 sec    '
      else
        ITEMS1(21)='* pause during check 0.5 sec  '
      endif
      ITEMS1(22)=  '? help                        '
      ITEMS1(23)=  '- exit                        '
      MITEM=23

C Help text for this menu.
      H(1)='The model contiguity ie. the boundary conditions'
      H(2)='at surfaces are set in this menu.  Before a simulation'
      H(3)='can be performed it is necessary that it be complete.'
      H(4) =' '
      H(5) ='There are several means to define the topology: '
      H(6) =' a) attribute surfaces via the zone geometry facility'
      H(7) ='    as they are created,'
      H(8) ='    '
      H(9) =' b) manually edit them via the `cfg` facility, '
      H(10)='    '
      H(11)=' c) search for matching vertices within the model'
      H(12)='    to fill in all partitions and then complete this'
      H(13)='    list by editing or importing surface attributes.'
      H(14)=' '
      H(15)='If internal mass in zone or explicit blinds have been'
      H(16)='defined as back-to-back surfaces then they will be'
      H(17)='treated as partitions unless you force confirmation.'
      H(18)=' '
      H(19)='The automated facility is considerably faster than'
      H(20)='manual methods and best used when all the zones have'
      H(21)='been created. '

C If user has defined problem and perhaps resized the display then
C redraw the problem image.
      if(MODIFY)then
        MODBND=.TRUE.
        MODLEN=.TRUE.
        NZONES=NCOMP
        nzg=NZONES
        DO 44 I=1,nzg
          nznog(I)=I
  44    CONTINUE

C (Re)Set all surfaces to standard line width.
        izgfoc=0
        if(MMOD.eq.8)then
          CALL INLNST(1)
          CALL ADJVIEW(IER)
        endif
      endif

C Now display the menu (short heading if in text mode).
      if(mmod.eq.8)then
        CALL EMENU('  Surface Connections & Boundary',ITEMS1,MITEM,INO)
      else
        CALL EMENU('Topology',ITEMS1,MITEM,INO)
      endif
      IF(INO.EQ.MITEM)THEN
        RETURN
      ELSEIF(INO.EQ.MITEM-1)THEN

C List help.
        CALL PHELPD('connection section',21,'surf_topo   ',0,0,IER)
      ELSEIF(INO.EQ.MITEM-2)THEN

C Swop the pause length.
        if(longpause)then
          longpause=.false.
        else
          longpause=.true.
        endif
      ELSEIF(INO.EQ.MITEM-3)THEN

C List connections.
        call tstamp('>','CFG: list topology')
        WRITE(outs,'(A,I3)')' Number of connections = ',NCON
        CALL EDISP(iuout,' ')
        CALL EDISP(iuout,outs)
        CALL CONXINF(1,0,CXSTR)
        CALL EDISP(iuout,CXSTR)
        DO 21 ICON=1,NCON
          CALL CONXINF(1,ICON,CXSTR)
          CALL EDISP(iuout,CXSTR)
   21   CONTINUE
      ELSEIF(INO.EQ.3)THEN

C Set tolerances for vertex comparison.
        call tstamp('>','CFG: enter tolerances')
        H(1)='The tolerance is expressed in metres and applies to'
        H(2)='the X, Y & Z of each vertex.'
        CALL EASKR(CACC,' ',' Tolerance (metres) ? ',
     &             0.001,'W',0.2,'W',0.001,'dist tolerance',IER,2)

        H(1)='The tolerance is expressed in degrees and allows the'
        H(2)='user to control how much of a change in direction'
        H(3)='along a line constitutes a surface corner.  If set '
        H(4)='too small, it will pick up minor vertices along a '
        H(5)='side as corners. '
        CALL EASKR(DACC,' ',' Corner tolerance (degrees) ?',
     &             0.1,'W',5.0,'W',1.0,'degree tolerance',IER,5)

        H(1)='Within cfg the shape of a surface is determined by'
        H(2)='the number of corners within the surface polygon.'
        H(3)='Vertices which lie along a line are not counted unless'
        H(4)='the angle of the line changes by several degrees.'
        H(5)='If the difference is 0 then both surfaces must have '
        H(6)='the same number of corners and they must be within '
        H(7)='the distance tolerance specified.  Increasing this'
        H(8)='number will allow for one or more of the corners to'
        H(9)='be out of tolerance and still be presented to the'
        H(10)='user for confirmation. '
        CALL EASKI(IACC,' Number of corners which are allowed outside',
     &    ' of the distance tolerance ? ',
     &     0,'F',2,'W',1,'corner tolerance',IER,10)

      ELSEIF(INO.EQ.5)THEN

C Flip logic for identical.
        call tstamp('>','CFG: toggle identical')
        if(M1SKP)then
          M1SKP=.false.
        else
          M1SKP=.true.
        endif
      ELSEIF(INO.EQ.6)THEN

C Flip logic for constant.
        call tstamp('>','CFG: toggle constant')
        if(M2SKP)then
          M2SKP=.false.
        else
          M2SKP=.true.
        endif
      ELSEIF(INO.EQ.7)THEN

C Flip logic for partitions.
        call tstamp('>','CFG: toggle partitions')
        if(M3SKP)then
          M3SKP=.false.
        else
          dok=.false.
          h(1)='Typically, you would want to include existing '
          h(2)='partitions in the list of surfaces to check.'
          h(3)='You are allowed to exclude them even though it '
          h(4)='is a dangerous option for expert users. '
          call askok('Are you sure you want to skip marked partitions?',
     &               '(see help)',OK,dok,4)
          if(OK)M3SKP=.true.
        endif
      ELSEIF(INO.EQ.8)THEN

C Flip logic for ground.
        call tstamp('>','CFG: toggle ground')
        if(M4SKP)then
          M4SKP=.false.
        else
          
          M4SKP=.true.
        endif
      ELSEIF(INO.EQ.9)THEN

C Flip logic for adiabatic.
        call tstamp('>','CFG: toggle adiabatic')
        if(M5SKP)then
          M5SKP=.false.
        else
          M5SKP=.true.
        endif
      ELSEIF(INO.EQ.10)THEN

C Flip logic for case of BASESIMP or no match found.
        call tstamp('>','CFG: toggle no match found')
        if(M6SKP)then
          M6SKP=.false.
        else
          M6SKP=.true.
        endif
      ELSEIF(INO.EQ.11)THEN

C Flip logic for case of CEN 13791 partition or no match found.
        call tstamp('>','CFG: toggle no match found')
        if(M7SKP)then
          M7SKP=.false.
        else
          M7SKP=.true.
        endif
      ELSEIF(INO.EQ.12)THEN

C Flip logic for case of UNKNOWN or no match found.
        call tstamp('>','CFG: toggle no match found')
        if(M0SKP)then
          M0SKP=.false.
        else
          H(1)='This option will result in any surface which does'
          H(2)='not conform to the geometric matching criteria to be'
          H(3)='assumed to have an EXTERNAL boundary condition.'
          H(4)='If you want to choose the boundary condition then'
          H(5)='you should toggle `no match found` to YES.'
          CALL PHELPD('cfg import atrib',5,'-',0,0,IER)
          M0SKP=.true.
        endif
      ELSEIF(INO.EQ.13)THEN
      
C Flip logic for back-to-back within zone.
        call tstamp('>','CFG: toggle back-to-back')
        if(M8SKP)then
          M8SKP=.false.
        else
          dok=.false.
          h(1)='Typically, back-to-back surfaces in a room represent'
          h(2)='mass and/or blinds and if these surfaces are connected'
          h(3)='to each other heat will flow. If you want the option'
          h(4)='to reset these to ADIABETIC say YES. '
          call askok('Do you want to confirm back-to-back surfaces',
     &               'within the room (see help)',OK,dok,4)
          if(OK)M3SKP=.true.
        endif

      ELSEIF(INO.EQ.15)THEN

C Edit single connection.
        call edaconn(ier)
        MODIFY=.TRUE.
      ELSEIF(INO.EQ.16)THEN

C Clear contiguity.
        h(1)='In restructuring a model it is sometimes useful to '
        h(2)='redefine the topology.'
        h(3)=' '
        h(4)='The `clear subset` asks for each type of connection '
        h(5)='(partitions/identical etc.) to clear. '
        h(6)=' '
        h(7)='The `clear all` will rebuild the whole master list '
        h(8)='and this should be done if the list has been corrupted.'
        CALL EASKABC(' Topology Options:',' ','clear all topology',
     &   'clear subset of topology.','continue',IRT,9)
        if(IRT.eq.1)then
          dok=.false.
          call askok('About to clear ALL contiguity.','Is this ok',
     &      OK,dok,8)
          if(.NOT.OK)goto 3

C Clear the connections list by scanning each zone geometry
C file in turn, re-establish nzsur. Once there is a new list
C of connections re-establish the IZSTOCN array.
          call tstamp('>','CFG: clearing contiguity')
          call edisp(iuout,'Clearing contiguity... ')
          ICC=0
          do 87 IZ=1,NCOMP
            call eclose(gversion(IZ),1.1,0.01,newgeo)
            if(newgeo)then
              call georead(IFIL+1,LGEOM(IZ),IZ,0,itru,IER)
            else
              call egomin(IFIL+1,LGEOM(IZ),IZ,0,0,itru,IER)
            endif
            nzsur(iz)=nsur
            do 88 IS=1,NSUR
              ICC=ICC+1
              IC1(ICC)=IZ
              IE1(ICC)=IS
              ICT(ICC)=0
              IC2(ICC)=0
              IE2(ICC)=0
   88       continue
   87     continue
          NCON=ICC
          do 145 icc = 1, NCON
            IZSTOCN(IC1(icc),IE1(icc))=icc
 145      continue
          write(outs,'(a,i4)')'No. of connections now = ',NCON
          call edisp(iuout,outs)

C Re-establish arrays of zone data now that there are possibly
C different numbers of connections.
          NZONES=NCOMP
          CALL ZDATA (0,IER,NZONES,ISFSUM)

C Check if user wants to re-establish connection data from surface
C attributes.
          H(1)='After clearing contiguity you can re-establish some of'
          H(2)='the connections via surface attributes (adiabetic,'
          H(3)='similar, constant and ground connections). If these.'
          H(4)='attributes are YES in the list of boundary type'
          H(5)='toggles (choices c-h) then you will be asked for '
          H(6)='confirmation. '
          H(7)=' '
          H(8)='Answering yes to this question can save you lots of '
          H(9)='time. '
          dok=.true.
          call askok('Recover surface attribute data?',
     &      '(adiabetic,similar, constant etc. see help)',OK,dok,9)
          if(OK)then
            call tstamp('>','CFG: import surface attributes')
            call ESACON(ITRC,M1SKP,M2SKP,M4SKP,M5SKP,M6SKP,M7SKP,IER)
            MODIFY=.TRUE.
          endif
        elseif(IRT.eq.2)then
          call tstamp('>','CFG: clearing partial contiguity')
          h(1)='The `clear subset` asks for each type of connection '
          h(2)='(partitions/identical etc.) to clear. '
          h(3)=' '
          h(4)='Say yes to SIMILAR and/or CONSTANT and/or GROUND'
          h(5)='and/or ADIABETIC if you want to clear connections'
          h(6)='of surfaces matching these boundary types. '
          dok=.false.
          call askok(' ','Clear items marked SIMILAR?',M1OK,dok,6)
          call askok(' ','Clear items marked CONSTANT?',M2OK,dok,6)
          call askok(' ','Clear items marked GROUND?',M4OK,dok,6)
          call askok(' ','Clear items marked ADIABETIC?',M5OK,dok,6)
          call askok(' ','Clear items marked BASESIMP?',M6OK,dok,6)
          call askok(' ','Clear items marked IDENT_CEN?',M7OK,dok,6)
          call usrmsg('Clearing partial contiguity...',bl,'-')
          ICC=0

C << show the surface attributes also be cleared?? >>

          do 187 IZ=1,NCOMP
            call eclose(gversion(IZ),1.1,0.01,newgeo)
            if(newgeo)then
              call georead(IFIL+1,LGEOM(IZ),IZ,0,itru,IER)
            else
              call egomin(IFIL+1,LGEOM(IZ),IZ,0,0,itru,IER)
            endif
            nzsur(iz)=nsur
            do 188 IS=1,NSUR
              if(ICT(ICC).eq.1.and.M1OK)then
                ICC=ICC+1
                IC1(ICC)=IZ
                IE1(ICC)=IS
                ICT(ICC)=0
                IC2(ICC)=0
                IE2(ICC)=0
              elseif(ICT(ICC).eq.2.and.M2OK)then
                ICC=ICC+1
                IC1(ICC)=IZ
                IE1(ICC)=IS
                ICT(ICC)=0
                IC2(ICC)=0
                IE2(ICC)=0
              elseif(ICT(ICC).eq.4.and.M4OK)then
                ICC=ICC+1
                IC1(ICC)=IZ
                IE1(ICC)=IS
                ICT(ICC)=0
                IC2(ICC)=0
                IE2(ICC)=0
              elseif(ICT(ICC).eq.5.and.M5OK)then
                ICC=ICC+1
                IC1(ICC)=IZ
                IE1(ICC)=IS
                ICT(ICC)=0
                IC2(ICC)=0
                IE2(ICC)=0
              elseif(ICT(ICC).eq.6.and.M6OK)then
                ICC=ICC+1
                IC1(ICC)=IZ
                IE1(ICC)=IS
                ICT(ICC)=0
                IC2(ICC)=0
                IE2(ICC)=0
              elseif(ICT(ICC).eq.7.and.M7OK)then
                ICC=ICC+1
                IC1(ICC)=IZ
                IE1(ICC)=IS
                ICT(ICC)=0
                IC2(ICC)=0
                IE2(ICC)=0
              else
                ICC=ICC+1
                IC1(ICC)=IZ
                IE1(ICC)=IS
                ICT(ICC)=0
                IC2(ICC)=0
                IE2(ICC)=0
              endif
  188       continue
  187     continue
          NCON=ICC
          write(outs,'(a,i4)')'No. of connections now = ',NCON
          call edisp(iuout,outs)

C Re-build zone:surface to connection hash array.
          do 45 icc = 1, NCON
            IZSTOCN(IC1(icc),IE1(icc))=icc
 45       continue

C Check if user wants to re-establish connection data from surface
C attributes.
          H(1)='After clearing contiguity you can re-establish some of'
          H(2)='the connections via surface attributes (adiabetic,'
          H(3)='similar, constant and ground connections). If these.'
          H(4)='attributes are YES in the list of boundary type'
          H(5)='toggles (choices c-h) then you will be asked for '
          H(6)='confirmation. '
          H(7)='This facility might save lots of time. '
          call askok('Recover surface attribute data?',
     &      '(adiabetic,similar, constant etc. see help)',OK,dok,7)
          if(OK)then
            call tstamp('>','CFG: import surface attributes')
            call ESACON(ITRC,M1SKP,M2SKP,M4SKP,M5SKP,M6SKP,M7SKP,IER)
            MODIFY=.TRUE.
          endif
        elseif(IRT.eq.3)then
          goto 3
        endif

C Save current common block information to file, jump back and
C update the display.  If icfgv = 3 or 4 then only need to update
C the connections file.  If it changes also update the configuration file.
        if(icfgv.eq.3.or.icfgv.eq.4)then
  286     H(1)='The surface topology of the problem is held'
          H(2)='in a connections file.'
          write(DCNN,'(a,a)')cfgroot(1:lnblnk(cfgroot)),'.cnn'
          if(LCNN(1:1).eq.' ')LCNN=DCNN

          CALL EASKS(LCNN,' Updated surface connections file ?',' ',
     &      72,DCNN,'system connx file name',IER,2)
          IF(LCNN.EQ.' ')GOTO 286
          CALL EMKCFG('-',IER)
        elseif(icfgv.lt.3)then

C Older configuration file format, ask for its name.
          H(1)='The current problem description will be placed'
          H(2)='into this file. It is suggested that you define'
          H(3)='a new file (ie. project.cfg > project_a.cfg).'
          CFGFN=LCFGF
          CALL EASKS(CFGFN,
     &     'Updated configuration file name ? (best if a new file) ',
     &     ' ',72,'xxx.cfg','revised config file',IER,3)
          if(CFGFN.ne.' ')LCFGF=CFGFN
          CALL EMKCFG('-',IER)
        endif
      ELSEIF(INO.EQ.17)THEN

C Check vertex matches and rebuild configuration file. If icfgv = 3 or 4 then
C only need to update the connections file. If it changes also update
C the configuration file.
        ITSNM=0
        if(icfgv.eq.3.or.icfgv.eq.4)then
  285     H(1)='The surface topology of the problem is held'
          H(2)='in a connections file.'
          write(DCNN,'(a,a)')cfgroot(1:lnblnk(cfgroot)),'.cnn'
          if(LCNN(1:1).eq.' ')then
            LCNN=DCNN
          endif
          CALL EASKS(LCNN,' Updated surface connections file ?',' ',
     &      72,DCNN,'system connx file name',IER,2)
          IF(LCNN.EQ.' ')GOTO 285
        elseif(icfgv.lt.3)then

C Older configuration file format, ask for its name.
          H(1)='The current problem description will be placed'
          H(2)='into this file. It is suggested that you define'
          H(3)='a new file (ie. project.cfg > project_a.cfg).'
          CFGFN=LCFGF
          CALL EASKS(CFGFN,
     &     'Updated configuration file name ? (best if a new file) ',
     &     ' ',72,'xxx.cfg','revised config file',IER,3)
          if(CFGFN.ne.' ')LCFGF=CFGFN
        endif

C Look for connections.
        call tstamp('>','CFG: do graphic match of surfaces')
        CALL CONFIG(NZONES,CACC,IACC,DACC,M0SKP,M1SKP,M2SKP,M3SKP,
     &    M4SKP,M5SKP,M6SKP,M7SKP,M8SKP,OTHEROK)

C Re-build zone:surface to connection hash array.
        do 43 icc = 1, NCON
          IZSTOCN(IC1(icc),IE1(icc))=icc
 43     continue

C Save data to configuration and connections file.
        CALL EMKCFG('-',IER)
        MODIFY=.TRUE.
      ELSEIF(INO.EQ.19)THEN

C Toggle trace level.
        ITRC=ITRC+1
        IF(ITRC.GT.2)ITRC=0
        INO=-4
      ELSE
        INO=-1
        GOTO 3
      ENDIF
      GOTO 3

      END 

C ************** CONFIG
C CONFIG processes the vertex data and presents close matches for
C the user to confirm before writing to temporary configuration file.
      SUBROUTINE CONFIG(NZONES,CACC,IACC,DACC,M0SKP,M1SKP,M2SKP,M3SKP,
     &  M4SKP,M5SKP,M6SKP,M7SKP,M8SKP,OTHEROK)
#include "building.h"
#include "geometry.h"
#include "prj3dv.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/FILEP/IFIL
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY5/ZCOG(MCOM,3),XMN,YMN,ZMN,XMX,YMX,ZMX
      COMMON/RAY6/LINSTY(MCON)

      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      common/C3F/LCNN
      COMMON/C14/UGRDTP(12,MGRDP),NGRDP
      common/ground/grdtmp(12,mgrdp)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

C G5 required for updating zone geometry file (until such time as geometry
C write can use G6 common blocks).
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)

C Pause length during sequentail scan longpause (true=1 sec)
      common/scanpause/longpause

      DIMENSION COORD1(MV,3), COORD2(MV,3)
      dimension ICVALS(MCOM),TOTHER(MS,3),ipmz(3),ipms(3)

      LOGICAL MODIFY,MODLEN,MODBND,OK,DOK,DOIT,OTHEROK,UPDGEO
      logical M0SKP,M1SKP,M2SKP,M3SKP,M4SKP,M5SKP,M6SKP,M7SKP,M8SKP
      logical closea,closee,close,longpause
      logical newgeo  ! to use for testing if new/old geometry file.
      character outs*124,CXSTR*78,SN*12
      character SO*24,TOTHER*24
      character DESCRC*25
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12
      CHARACTER H*72
      character LCNN*72,DCNN*72,cfgroot*24,LCFGF*72,bl*2
      integer izselfcheck  ! the surface being tested
      logical selfcheck  ! set true if currently looking within zone

C CACC is the tolerance allowed between two vertices.
C TOTHER is array of temporary new topology attributes for current zone.

C ICNT holds the number of matching vertices between two surfaces.
      bl='  '
      ITSNM=0
      ICNT=0
      IC=0
      newgeo=.false.  ! assume older format geometry.

C Set for redraw if image on first entry (if refresh set to after
C each edit).
      MODIFY=.TRUE.
      MODLEN=.TRUE.
      MODBND=.TRUE.

C Request which zones to include.
      H(1)=' Pick one, several or all zones for inclusion'
      ICPIC=NZONES
      CALL EPICKS(ICPIC,ICVALS,' ',' Which zones to include: ',
     &  12,NZONES,zname,' zone list',IER,1)

C Loop for each zone, only doing comparison for selected zones.
      DO 100 I=1,NZONES
        IZ=I
        DOIT=.FALSE.
        UPDGEO=.FALSE.
        do 215 ix=1,icpic
          if(IZ.eq.ICVALS(ix))DOIT=.TRUE.
  215   continue
        if(DOIT)then

C Get confirmation for continuing, if user wishes to skip a
C selected zone then updae the IC count to reflect the surfaces
C skiped.
          if(IZ.ne.1)then
            write(outs,'(3a)')' Continue with ',zname(IZ),' ?'
            dok=.true.
            h(1)='Normally you would check all of the zones. And '
            h(2)='if you need to exit this process (ran out of '
            h(3)='coffee) this is a place you can do this. NOTE '
            h(4)='that this can leave the model in an inconsistent'
            h(5)='state. '
            CALL ASKOK(outs,'(see help)',OK,dok,5)
            if(.NOT.OK)then
              IC=IC+nzsur(IZ)
              goto 100
            endif
          endif
          WRITE(outs,'(A,I2,A,A)') 'Processing (',IZ,') ',zname(IZ)
          call edisp(iuout,outs)
          call edisp(iuout,' corners|connx| description ')
          call edisp(iuout,' in surf|index| of existing topology')
        endif

C Display the reference zone (if type 3 then display both).
C Remember topology attributes for reference zone.
        IUO=IFIL+1
        call eclose(gversion(IZ),1.1,0.01,newgeo)
        if(newgeo)then
          call georead(IUO,LGEOM(IZ),IZ,0,iuout,IER)
        else
          call egomin(IUO,LGEOM(IZ),IZ,0,0,iuout,IER)
        endif
        do 322 IJ=1,NSUR
          ICN=IZSTOCN(iz,ij)
          TOTHER(IJ,1)=SSOTHER(ICN,1)
          TOTHER(IJ,2)=SSOTHER(ICN,2)
          TOTHER(IJ,3)=SSOTHER(ICN,3)
  322   continue
        MODIFY=.TRUE.
        MODBND=.TRUE.

C Loop over each surface and increment the counter IC.
        DO 110 J=1,nzsur(IZ)
          JS=J
          IC=IC+1
          izselfcheck=JS  ! remember the surface being tested

C If zone is not to be included in the analysis, jump.
          if(.NOT.DOIT)goto 110

C Reset all surface lines to standard width and then the specific 
C surface(s) under consideration to wide lines. If other side
C is not a type 3 then don`t highlight.
          if(MMOD.eq.8)then
            CALL INLNST(1)
            LINSTY(IC)=2
          endif
          nzg=1
          nznog(1)=IZ
          izgfoc = IZ
          if(MMOD.eq.8)CALL CADJVIEW(IER)
          MODIFY=.TRUE.

C Find number of corners and their coordinates for this surface.
          CALL SFCRNR(DACC,IZ, JS, NAPIC1, COORD1)
          CALL CONXINF(1,IC,CXSTR)
          write(outs,'(I6,3X,A)') NAPIC1,CXSTR
          call edisp(iuout,outs)

C If user said skip confirmation of marked partitions, check and update the
C surface attribute if a dash and jump around.
          if(M3SKP.and.(ICT(IC).eq.3))then
            call edisp(iuout,'known connection skipped.')
            if(MMOD.eq.8)then
              if(longpause)then
                call pauses(1)
              else
                call pausems(400)
              endif
            endif

C If tother(JS,2) is a dash then update with IC2 IE2.
            if(TOTHER(JS,2)(1:1).eq.'-')then
              write(TOTHER(JS,1),'(a)') 'ANOTHER'
              write(tother(JS,2),'(i2.2)') IC2(ic)
              write(tother(JS,3),'(i2.2)') IE2(ic)
              UPDGEO=.TRUE.
            endif
            goto 110
          endif

C Reset number of geometric matches. If more than one then ask.
          ipmatch=0

C Loop over zones including this zone so that back-to-back surfaces
C are also detected.
          DO 200 II=1, NZONES
            if (II.EQ.IZ)then
	      selfcheck=.true.  ! currently looking within zone
            else
              selfcheck=.false.
            endif

C Find if there is a surface connection. Skip past self if in same zone.
            DO 210 JJ=1,nzsur(II)
              if(selfcheck.and.JJ.eq.izselfcheck)then
	        goto 210
              endif
	      
C Find number of corners and their coordinates for the other surface.
              CALL SFCRNR(DACC,II, JJ, NAPIC2, COORD2)

C If the number of corners of the two surface are equal then
C check if their coordinates are within tolerance.
              ICNT=0
              IF (NAPIC1.EQ.NAPIC2) THEN
                DO 300 K=1, NAPIC1
                  DO 350 KK=1, NAPIC1
                    XDIFF=ABS(COORD2(KK,1)-COORD1(K,1))
                    YDIFF=ABS(COORD2(KK,2)-COORD1(K,2))
                    ZDIFF=ABS(COORD2(KK,3)-COORD1(K,3))
                    IF(XDIFF.LE.CACC.AND.YDIFF.LE.CACC.AND.
     &                 ZDIFF.LE.CACC) THEN
                      ICNT=ICNT+1
                      GOTO 300
                    ENDIF
  350             CONTINUE
  300           CONTINUE
              ENDIF

C If the number of matching corners is within the IACC limit, then
C present choice to the user.
              IF ((NAPIC1-ICNT).le.IACC) THEN
                ipmatch=ipmatch+1
                if(ipmatch.le.3)then
                  ipmz(ipmatch)=II
                  ipms(ipmatch)=JJ
                endif
              ENDIF

C Reset ICNT before commencing a new loop.
              ICNT=0
  210       CONTINUE
  200     CONTINUE

C Depending on how many matches were found, proceed.  If there were
C no matches then check for existing IDENTICAL or CONSTANT or GROUND
C or ADIABATIC or BASESIMP or IDENT_CEN and set these if the user
C asked to skip onfirmation.
          if(ipmatch.eq.0)then

C If user said skip confirmation of marked IDENTICAL, check and update the
C surface attribute if it does not match and jump around.
            WRITE(SO,'(A)') TOTHER(JS,1)
            if(M1SKP.and.(ICT(IC).eq.1))then
              call edisp(iuout,'known connection skipped.')
              if(MMOD.eq.8)then
                if(longpause)then
                  call pauses(1)
                else
                  call pausems(400)
                endif
              endif
              if(SO(1:7).ne.'SIMILAR')then
                TOTHER(JS,1)='SIMILAR'
                write(tother(JS,2),'(i2.2)') IC2(ic)
                write(tother(JS,3),'(i2.2)') IE2(ic)
                UPDGEO=.TRUE.
              endif
              goto 110
            endif

C If user said skip confirmation of marked CONSTANT, check and update the
C surface attribute if it does not match and jump around.
            if(M2SKP.and.(ICT(IC).eq.2))then
              call edisp(iuout,'known connection skipped.')
              if(MMOD.eq.8)then
                if(longpause)then
                  call pauses(1)
                else
                  call pausems(400)
                endif
              endif
              if(SO(1:8).ne.'CONSTANT')then
                TOTHER(JS,1)='CONSTANT'
                write(tother(JS,2),'(i2.2)') IC2(ic)
                write(tother(JS,3),'(i2.2)') IE2(ic)
                UPDGEO=.TRUE.
              endif
              goto 110
            endif

C If user said skip confirmation of marked GROUND, check and update the
C surface attribute if it does not match and jump around.
            if(M4SKP.and.(ICT(IC).eq.4))then
              call edisp(iuout,'known connection skipped.')
              if(MMOD.eq.8)then
                if(longpause)then
                  call pauses(1)
                else
                  call pausems(400)
                endif
              endif
              if(SO(1:6).ne.'GROUND')then
                TOTHER(JS,1)='GROUND'
                write(tother(JS,2),'(i2.2)') IC2(ic)
                write(tother(JS,3),'(i2.2)') IE2(ic)
                UPDGEO=.TRUE.
              endif
              goto 110
            endif

C If user said skip confirmation of marked ADIABATIC, check and update the
C surface attribute if it does not match and jump around.
            if(M5SKP.and.(ICT(IC).eq.5))then
              call edisp(iuout,'known connection skipped.')
              if(MMOD.eq.8)then
                if(longpause)then
                  call pauses(1)
                else
                  call pausems(400)
                endif
              endif
              if(SO(1:9).ne.'ADIABATIC')then
                TOTHER(JS,1)='ADIABATIC'
                write(tother(JS,2),'(i2.2)') IC2(ic)
                write(tother(JS,3),'(i2.2)') IE2(ic)
                UPDGEO=.TRUE.
              endif
              goto 110
            endif

C If user said skip confirmation of marked BASESIMP, check and update the
C surface attribute if it does not match and jump around. 
            if(M6SKP.and.(ICT(IC).eq.6))then
              call edisp(iuout,'known connection skipped.')
              if(MMOD.eq.8)then
                if(longpause)then
                  call pauses(1)
                else
                  call pausems(400)
                endif
              endif
              if(SO(1:8).ne.'BASESIMP')then
                TOTHER(JS,1)='BASESIMP'
                write(tother(JS,2),'(i2.2)') IC2(ic)
                write(tother(JS,3),'(i2.2)') IE2(ic)
                UPDGEO=.TRUE.
              endif
              goto 110
            endif

C If user said skip confirmation of marked IDENT_CEN, check and update the
C surface attribute if it does not match and jump around. 
            if(M7SKP.and.(ICT(IC).eq.7))then
              call edisp(iuout,'known connection skipped.')
              if(MMOD.eq.8)then
                if(longpause)then
                  call pauses(1)
                else
                  call pausems(400)
                endif
              endif
              if(SO(1:9).ne.'IDENT_CEN')then
                TOTHER(JS,1)='IDENT_CEN'
                write(tother(JS,2),'(i2.2)') IC2(ic)
                write(tother(JS,3),'(i2.2)') IE2(ic)
                UPDGEO=.TRUE.
              endif
              goto 110
            endif

C Ask user if some other type of connection. If M0SKP is true then
C set connection to exterior and proceed.
            if(OTHEROK)then
              if(M0SKP)then
                IC1(IC)=IZ
                IE1(IC)=JS
                ICT(IC)=0
                IE2(IC)=0
                IC2(IC)=0
                if(SO(1:8).ne.'EXTERIOR')then
                  TOTHER(JS,1)='EXTERIOR'
                  write(tother(JS,2),'(i2.2)') IC2(ic)
                  write(tother(JS,3),'(i2.2)') IE2(ic)
                  UPDGEO=.TRUE.
                endif
                call edisp(iuout,'no match found setting to EXTERIOR.')
                if(MMOD.eq.8)then
                  if(longpause)then
                    call pauses(1)
                  else
                    call pausems(400)
                  endif
                endif
              else

C Setup contextural help for boundary conditions.
        H(1) ='The boundary conditions at the other face of this sur-'
        H(2) ='face is specified via the selection of one of these'
        H(3) ='choices.  The default is OUTSIDE.'
        H(4) =' '
        H(5) =' Exterior - the other face of the surface is exposed to'
        H(6) ='  ambient air temperature, wind and solar radiation.'
        H(4) =' '
        H(5) =' Dynamic/similar - the other face of the surface is'
        H(6) ='  exposed to the same conditions as the inside face'
        H(7) ='  (except that when direct sunlight is falling on the'
        H(8) ='  inside face it cannot be falling on the other face'
        H(9) ='  only the diffuse component is used on the other face.'
        H(10)='  where both sides get the same radiation.'
        H(11)='  A dynamic/similar connection can include an offset'
        H(12)='  temperature (C) and radiation (W/m^2).'
        H(13)=' '
        H(14)=' Static - requires that a constant temperature and'
        H(15)='  radiation be supplied. You might use this to represent'
        H(16)='  a cold storage room.'
        H(17)=' '
        H(18)=' Surface in another zone - the other face of the surface'
        H(19)='  gets its boundary conditions from the last known state'
        H(20)='  of the other surface. There is a thermophysical con-'
        H(21)='  nection established between the two zones.'
        H(22)=' '
        H(23)=' Ground (standard profile) - the other face of the'
        H(24)='  surface is in direct contact with one of the standard'
        H(25)='  monthly temperature profiles embedded in ESP-r.'
        H(26)=' '
        H(27)=' Ground (user defined profile) - the other face of the'
        H(28)='  surface is in direct contact with a monthly profile'
        H(29)='  previously created by the user (in the model context)'
        H(30)=' '
        H(31)=' Ground (3D conduction) - the other face connects to a'
        H(32)='  3D ground representation. This is not often used.'
        H(33)=' '
        H(34)=' Adiabatic boundary - typically used for back-to-back'
        H(35)='  surfaces in a zone representing thermal mass. There'
        H(36)='  is no heat flux passed beyond the other-face. '
        H(37)=' '
        H(38)=' BASESIMP is used to represent one of several basement'
        H(39)='  configurations and precalculated patterns of heat'
        H(40)='  transfer. For this to be useful there must be a'
        H(41)='  basesimp file created for the zone giving a number'
        H(42)='  of dimensions and computational parameters.'
        H(43)=' '
        H(44)=' CEN 13791 partition which is like the SIMILAR'
        H(45)='  but for CEN 13791 test. The offset temperauture'
        H(46)='  and radiation (W/m^2) in SIMILAR is not available'
        H(47)='  for the CEN 13791 partition type. For details see'
        H(48)='  section 4.4.2 CEN 13791 standard. '
        H(49)=' '
        H(50)='There is a module of ESP-r which uses polygon adjacency'
        H(51)='to automatically link partitions together. '
        H(52)=' '
                if(ICT(IC).eq.3)then
                  write(outs,'(a,a)') 
     &              'Partition, but no geometric match found. Is ',
     &              CXSTR(1:lnblnk(CXSTR))
                elseif(ICT(IC).ne.3)then
                  write(outs,'(a,a)')'No geometric match found. Is ',
     &              CXSTR(1:lnblnk(CXSTR))
                  idno=ICT(IC)
                endif
                iart=0
                call MENUATOL(outs,' Connection options ',
     &  'a exterior                  ','b dynamic (similar) boundary',
     &  'c static boundary conditions','d ground (standard profile) ',
     &  'e ground (user def profile) ','f ground (3D conduction)    ',
     &  'g adiabatic (no heat flux)  ','h BASESIMP foundation       ',
     &  'i CEN 13791 partition       ',
     &  'j UNKNOWN (at this time)    ','k accept current            ',
     &   ' ',iart,idno,52)
                if(IART.eq.0)then
                  continue
                elseif(IART.eq.1)then
                  IC1(IC)=IZ
                  IE1(IC)=JS
                  ICT(IC)=0
                  IE2(IC)=0
                  IC2(IC)=0
                  if(SO(1:8).ne.'EXTERIOR')then
                    TOTHER(JS,1)='EXTERIOR'
                    write(tother(JS,2),'(i2.2)') IC2(ic)
                    write(tother(JS,3),'(i2.2)') IE2(ic)
                    UPDGEO=.TRUE.
                  endif
                elseif(IART.eq.2)then
                  IC1(IC)=IZ
                  IE1(IC)=JS
                  ICT(IC)=1
                  IE2(IC)=0
                  IC2(IC)=0
                  if(SO(1:7).ne.'SIMILAR')then
                    TOTHER(JS,1)='SIMILAR'
                    write(tother(JS,2),'(i2.2)') IC2(ic)
                    write(tother(JS,3),'(i2.2)') IE2(ic)
                    UPDGEO=.TRUE.
                  endif
                elseif(IART.eq.3)then
                  H(1)='A type 2 connection requires that a constant'
                  H(2)='temperature and radiation (Watts) be entered.'
                  CALL EASKR(VAL,' ',' Constant temperature ? ',
     &              -99.,'F',99.,'F',20.,'adjacent temperature',IER,2)
                  CALL EASKR(VAL2,' ',' Constant rad (W/m^2) ? ',
     &              -999.,'F',999.,'F',0.,'adjacent rad',IER,2)
                  IC1(IC)=IZ
                  IE1(IC)=JS
                  ICT(IC)=2
                  IE2(IC)=INT(VAL2)
                  IC2(IC)=INT(VAL)
                  if(SO(1:8).ne.'CONSTANT')then
                    TOTHER(JS,1)='CONSTANT'
                    write(tother(JS,2),'(i2.2)') IC2(ic)
                    write(tother(JS,3),'(i2.2)') IE2(ic)
                    UPDGEO=.TRUE.
                  endif
                elseif(IART.eq.4)then

C List out standard profiles, assuming that if June is 0.0, then the
C profile has not been defined.
                  CALL EDISP(iuout,'Standard ground profiles Jan-Dec:')
                  CALL EDISP(iuout,
     &              'id  Jan, Feb, Mar, Apr, May, Jun, Jul...')
                  do 21 igrdp=1,mgrdp
                    CALL ECLOSE(GRDTMP(6,igrdp),0.0,0.001,close)
                    if(.NOT.close)then
                      WRITE(OUTS,'(I2,12F5.1)')igrdp,
     &                  (GRDTMP(JG,IGRDP),JG=1,12)
                      call edisp(iuout,outs)
                    endif
21                continue
                  H(1)='A ground connection allows either:'
                  H(2)='> 0 a connection to one of several standard'
                  H(3)='    profiles. Supply its index.'
                  H(4)='= 0 a connection to a user defined profile. '
                  H(5)='    supply 0 plus user profile index. '
                  H(6)='-3  a connection to a 3D ground model.'
                  iic2=ic2(IC)
  777             CALL EASKI(IIC2,' Monthly (standard) profile index?',
     &              ' ',1,'F',12,'F',1,'ground profile',IER,7)
                  IC1(IC)=IZ
                  IE1(IC)=JS
                  ICT(IC)=4
                  IF(IIC2.GT.0)THEN
                    IC2(IC)=iic2
                    IE2(IC)=0
                    if(SO(1:6).ne.'GROUND')then
                      TOTHER(JS,1)='GROUND'
                      write(tother(JS,2),'(i2.2)') IC2(ic)
                      write(tother(JS,3),'(i2.2)') IE2(ic)
                      UPDGEO=.TRUE.
                    endif
                  ELSE
                    CALL USRMSG(' ',' Unacceptable ground profile...',
     &  'W')
                    GOTO 777
                  ENDIF
                elseif(IART.eq.5)then

C List out user defined profiles, assuming that if June is 0.0, then the
C profile has not been defined.
                  if(NGRDP.gt.0)then
                    CALL EDISP(iuout,' ')
                    do 22 igrdp=1,NGRDP    
                      CALL EDISP(iuout,
     &                  'User defined ground profile Jan-Dec:')
                      WRITE(OUTS,'(12F6.1)')(UGRDTP(JG,IGRDP),JG=1,12)
                      call edisp(iuout,outs)
 22                 continue
                    H(1)='One of the user defined profile numbers is'
                    H(2)='required at this point.'
                    CALL EASKI(IIE2,
     &                'User defined ground profile index?',
     &                ' ',0,'F',9,'F',1,'user defined profile',IER,2)
                    IC1(IC)=IZ
                    IE1(IC)=JS
                    ICT(IC)=4
                    IC2(IC)=0
                    IE2(IC)=iie2
                    if(SO(1:6).ne.'GROUND')then
                      TOTHER(JS,1)='GROUND'
                      write(tother(JS,2),'(i2.2)') IC2(ic)
                      write(tother(JS,3),'(i2.2)') IE2(ic)
                      UPDGEO=.TRUE.
                    endif
                  else
                    call usrmsg(
     &               'No user defined ground profiles have been found.',
     &               'Go to `problem context` to define these.','W')
                  endif
                elseif(iart.eq.6)then 

C Link to a 3D conduction model..
                  IC1(IC)=IZ
                  IE1(IC)=JS
                  ICT(IC)=4
                  IC2(IC)=-3
                  IE2(IC)=0
                  if(SO(1:6).ne.'GROUND')then
                    TOTHER(IE1(JS),1)='GROUND'
                    write(tother(JS,2),'(i2.2)') IC2(ic)
                    write(tother(JS,3),'(i2.2)') IE2(ic)
                    UPDGEO=.TRUE.
                  endif
                elseif(IART.eq.7)then
                  IC1(IC)=IZ
                  IE1(IC)=JS
                  ICT(IC)=5
                  IE2(IC)=0
                  IC2(IC)=0
                  if(SO(1:9).ne.'ADIABATIC')then
                    TOTHER(JS,1)='ADIABATIC'
                    write(tother(JS,2),'(i2.2)') IC2(ic)
                    write(tother(JS,3),'(i2.2)') IE2(ic)
                    UPDGEO=.TRUE.
                  endif
                elseif(iart.eq.8)then 

C BASESIMP start.
C Get BASESIMP configuration via call to bsimtype. Save state of current
C menu and recover after the call.
                  IC1(IC)=IZ
                  IE1(IC)=JS
                  ICT(IC)=6
                  H(1)='Specify the BASESIMP configuration index.'
                  H(2)='Refer to src/esp-r/esrubld/bscoeff.F for the '
                  H(3)='correspondence between iconfig and standard '
                  H(4)='BASESIMPdescriptors (e.g. BCIN_1).'
                  IBS=IC2(IC)
                  call bsimtype(ibs)
                  IC2(IC)=IBS

C Get `surface weighting factor', the percentage of the BASESIMP heat loss to
C attribute to the surface under consideration.
                  Ifrac=IE2(IC)
          H(1)='BASESIMP is a `whole-foundation model`. Therefore, when'
          H(2)='a foundation is represented by numerous ESP-r surfaces'
          H(3)='(e.g. four walls + floor to represent a basement), you'
          H(4)='must specify the percentage of the whole-foundation'
          H(5)='heat loss to attribute to each surface. Normally the'
          H(6)='sum of surfaces representing a founcation is 100%.'
                  CALL EASKI(Ifrac,
     &              ' Percent of BASESIMP loss to this surface?'
     &             ,' ',0,'F',100,'F',100,'BASESIMP heat loss percent',
     &              IER,6)
                  IE2(IC)=Ifrac
                  if(SO(1:8).ne.'BASESIMP')then
                    TOTHER(IE1(JS),1)='BASESIMP'
                    write(tother(JS,2),'(i2.2)') IC2(ic)
                    write(tother(JS,3),'(i2.2)') IE2(ic)
                    UPDGEO=.TRUE.
                  endif
C BASESIMP end.
                elseif(IART.eq.9)then
                  IC1(IC)=IZ
                  IE1(IC)=JS
                  ICT(IC)=7
                  IE2(IC)=0
                  IC2(IC)=0
                  if(SO(1:7).ne.'IDENT_CEN')then
                    TOTHER(JS,1)='IDENT_CEN'
                    write(tother(JS,2),'(i2.2)') IC2(ic)
                    write(tother(JS,3),'(i2.2)') IE2(ic)
                    UPDGEO=.TRUE.
                  endif

C CEN 13791 partition end.
                elseif(IART.eq.10)then
                  IC1(IC)=IZ
                  IE1(IC)=JS
                  ICT(IC)=-1
                  IE2(IC)=0
                  IC2(IC)=0
                  if(SO(1:7).ne.'UNKNOWN')then
                    TOTHER(JS,1)='UNKNOWN'
                    write(tother(JS,2),'(i2.2)') IC2(ic)
                    write(tother(JS,3),'(i2.2)') IE2(ic)
                    UPDGEO=.TRUE.
                  endif
                endif
              endif
            endif
          elseif(ipmatch.eq.1)then

C Only one possible matching surface, inform user and then proceed. If
C selfcheck is false then the match is as a partition to a surface in
C another zone. If selfcheck is true then we have a back-to-back
C match and we need to test the value of M8SKP.
            II=ipmz(1)
            JJ=ipms(1)

C Re-evaluate the state of selfcheck based on the surface that was
C found to match.
            if (II.EQ.IZ)then
	      selfcheck=.true.  ! currently looking within zone
            else
              selfcheck=.false.
            endif

            MODBND=.TRUE.
            if(MMOD.eq.8)then
              CALL INLNST(1)
              LINSTY(IC)=2
            endif
            nzg=2
            nznog(1)=IZ
            nznog(2)=II
            CALL SURADJ(II,JJ,IE,TMP,IZC,ISC,ICO,DESCRC)
            if(MMOD.eq.8)LINSTY(ICO)=2
            izgfoc = IZ
            if(MMOD.eq.8)CALL CADJVIEW(IER)
            MODIFY=.TRUE.
            MODBND=.TRUE.
            IC1(IC)=IZ
            IE1(IC)=JS
            ICT(IC)=3
            IC2(IC)=II
            IE2(IC)=JJ
            call usrmsg(' ',bl,'-')
            if(MMOD.eq.8)then
              if(longpause)then
                call pauses(1)
              else
                call pausems(400)
              endif
            endif

C If an actual partition then no need for confirmation, just
C update the attributes.
            if(.NOT.selfcheck)then
              write(TOTHER(JS,1),'(a)') 'ANOTHER'
              write(tother(JS,2),'(i2.2)') IC2(ic)
              write(tother(JS,3),'(i2.2)') IE2(ic)
              UPDGEO=.TRUE.
              ICN=IZSTOCN(II,JJ)
              SN=SSNAME(icn)
              WRITE(outs,'(4a)')' accepting match to surface ',
     &          SN(1:lnblnk(SN)),' in ',zname(II)(1:lnzname(II))
              call edisp(iuout,outs)
              goto 110  ! no need to check further
            else
	    
C If we are in the same room then we have a back-to-back surface.
C If M8SKP was set to false then go ahead and
C make it a partition without bothering to ask the user.
              if(M8SKP)then
                write(TOTHER(JS,1),'(a)') 'ANOTHER'
                write(tother(JS,2),'(i2.2)') IC2(ic)
                write(tother(JS,3),'(i2.2)') IE2(ic)
                UPDGEO=.TRUE.
                ICN=IZSTOCN(II,JJ)
                SN=SSNAME(icn)
                WRITE(outs,'(3a)')' accepting match to surface ',
     &            SN(1:lnblnk(SN)),' in current zone.'
                call edisp(iuout,outs)
                goto 110  ! no need to check further
              else
                h(1)='You asked to specify connection if a match was'
                h(2)='found to another surface in the same room. '
                CALL EASKABC('Matched surface within room:',' ',
     &            'set to back-to-back','set to ADIABETIC',
     &            'set to UNKNOWN',IRTA,2)
                if(IRTA.eq.1)then
                  write(TOTHER(JS,1),'(a)') 'ANOTHER'
                  write(tother(JS,2),'(i2.2)') IC2(ic)
                  write(tother(JS,3),'(i2.2)') IE2(ic)
                  UPDGEO=.TRUE.
                  ICN=IZSTOCN(II,JJ)
                  SN=SSNAME(icn)
                  WRITE(outs,'(3a)')' accepting match to surface ',
     &              SN(1:lnblnk(SN)),' in current zone.'
                  call edisp(iuout,outs)
                elseif(IRTA.eq.2)then
                  IC1(IC)=IZ
                  IE1(IC)=JS
                  ICT(IC)=5
                  IE2(IC)=0
                  IC2(IC)=0
                  TOTHER(JS,1)='ADIABATIC'
                  write(tother(JS,2),'(i2.2)') IC2(ic)
                  write(tother(JS,3),'(i2.2)') IE2(ic)
                  UPDGEO=.TRUE.
                elseif(IRTA.eq.3)then
                  IC1(IC)=IZ
                  IE1(IC)=JS
                  ICT(IC)=-1
                  IE2(IC)=0
                  IC2(IC)=0
                  TOTHER(JS,1)='UNKNOWN'
                  write(tother(JS,2),'(i2.2)') IC2(ic)
                  write(tother(JS,3),'(i2.2)') IE2(ic)
                  UPDGEO=.TRUE.
                endif
                goto 110  ! no need to check further
              endif
            endif
          elseif(ipmatch.gt.1)then

C If there is more than ONE possible matching geometric connection
C then confirm with the user. First check if one of the matches
C is of a reasonable orientation.
            iipmatch=MIN0(ipmatch,3)
            write(outs,'(a,i2,a,a)') 'There are ',iipmatch,
     &        ' possible matches with ',CXSTR(1:lnblnk(CXSTR))
            call edisp(iuout,outs)
            do 841 ipml=1,iipmatch
              IIx=ipmz(ipml)
              JJx=ipms(ipml)
              CALL SURADJ(IIx,JJx,IE,TMP,IZC,ISC,ICO,DESCRC)
              SELV=SSPELV(ic)+SSPELV(ico)
              DAZI=SSPAZI(ic)-SSPAZI(ico)
              DAZI=ABS(DAZI)
              call eclose(DAZI,180.0,3.0,closea)
              call eclose(SELV,0.0,3.0,closee)
              ICN=IZSTOCN(IIx,JJx)
              SN=SSNAME(ICN)
              if(closea.and.closee)then
                WRITE(outs,'(4a)')
     &            ' Probable (orientation close) match to surface ',
     &            SN(1:lnblnk(SN)),' in ',zname(IIx)(1:lnzname(IIx))
              else
                WRITE(outs,'(4a)')
     &            ' Possible (orientation not close) match to surface ',
     &            SN(1:lnblnk(SN)),' in ',zname(IIx)(1:lnzname(IIx))
              endif
              call edisp(iuout,outs)
 841        continue
 840        continue

C With two surfaces close together it is difficult to see the connection.
C Zoom in on the surface in question - find its bounding box and centre
C the view on it and calculate the angle of view to that box.
            if(MMOD.eq.8)then
              call surbox(iz,js,1.,cx,cy,cz,sxmx,sxmn,symx,symn,szmx,
     &          szmn)
              call saveview
              call ang3vtx(sxmn,symn,sznb,EYEM(1),EYEM(2),EYEM(3),sxmx,
     &          symx,szmx,angn)
              ANG=angn
              HANG=ANG/2.0
            endif
            do 843 ipml=1,iipmatch
              ITBND=0
              VIEWM(1)=cx
              VIEWM(2)=cy
              VIEWM(3)=cz
              XMN=sxmn
              YMN=symn
              ZMN=szmn
              XMX=sxmx
              YMX=symx
              ZMX=szmx
              MODIFY=.TRUE.
              MODLEN=.TRUE.
              if(MMOD.eq.8)CALL INLNST(1)
              if(MMOD.eq.8)LINSTY(IC)=2
              nzg=2
              II=ipmz(ipml)
              JJ=ipms(ipml)
              nznog(1)=IZ
              nznog(2)=II
              if(MMOD.eq.8)LINSTY(ICO)=2
              izgfoc = IZ
              if(MMOD.eq.8)CALL CADJVIEW(IER)
              ICN=IZSTOCN(II,JJ)
              SN=SSNAME(ICN)
              WRITE(outs,'(4a)')' Probable match to surface ',
     &            SN(1:lnblnk(SN)),' in ',zname(II)(1:lnzname(II))
              dok=.true.
              h(1)='If there is more than ONE possible matching '
              h(2)='geometric connection you are asked to confirm'
              h(3)='which one. Exercise care - the interface does'
              h(4)='not tell you how many possible matches it will'
              h(5)='be showing you. '
              CALL ASKOK(outs,' Accept this? (see help)',OK,dok,5)
              IF(OK)then
                IC1(IC)=IZ
                IE1(IC)=JS
                ICT(IC)=3
                IC2(IC)=II
                IE2(IC)=JJ

C Update surface attributes and then restore the view points.
                write(TOTHER(JS,1),'(a)') 'ANOTHER'
                write(tother(JS,2),'(i2.2)') IC2(ic)
                write(tother(JS,3),'(i2.2)') IE2(ic)
                UPDGEO=.TRUE.
                if(MMOD.eq.8)call recoview
                ITBND=1
                MODLEN=.TRUE.
                MODIFY=.TRUE.
                MODBND=.TRUE.

C Move to the next surface.
                ICNT=0
                goto 110
              endif
 843        continue

C If reached this point then no surface selected. 
            if(MMOD.eq.8)call recoview
            ITBND=1
            MODBND=.TRUE.
            MODIFY=.TRUE.
            MODLEN=.TRUE.
            h(1)='If you continue, the current connection type '
            h(2)='will be retained.  You can edit this connection '
            h(3)='after all of the surfaces have been checked.'
            CALL EASKABCD(' None of the suggestions taken:',' ',
     &        'loop again','set to UNKNOWN','set to EXTERIOR',
     &        'continue',IRTA,3)
            if(irta.eq.1)then
              goto 840
            elseif(irta.eq.2)then
              IC1(IC)=IZ
              IE1(IC)=JS
              ICT(IC)=-1   ! unknown connection is a -1
              IE2(IC)=0
              IC2(IC)=0
              if(SO(1:7).ne.'UNKNOWN')then
                TOTHER(JS,1)='UNKNOWN'
                write(tother(JS,2),'(i2.2)') IC2(ic)
                write(tother(JS,3),'(i2.2)') IE2(ic)
                UPDGEO=.TRUE.
              endif
            elseif(irta.eq.3)then
              IC1(IC)=IZ
              IE1(IC)=JS
              ICT(IC)=0
              IE2(IC)=0
              IC2(IC)=0
              if(SO(1:7).ne.'EXTERIOR')then
                TOTHER(JS,1)='EXTERIOR'
                write(tother(JS,2),'(i2.2)') IC2(ic)
                write(tother(JS,3),'(i2.2)') IE2(ic)
                UPDGEO=.TRUE.
              endif
            elseif(irta.eq.4)then
              ICNT=0
              goto 110
            endif

C Reset line style.
            if(MMOD.eq.8)CALL INLNST(1)
          ENDIF
  110   CONTINUE

C Update geometry file if required.  Do this by re-reading the
C geometry file, then update SOTHER and SSOTHER and write out the
C geometry file with the updated boundary tag. The ssother will be
C held in memory until the connections file is written.

C << what about using CALL ESCZONE(ICOMP) and then CALL ERCZONE(ICOMP)? >>

        if(UPDGEO)then
          call edisp(iuout,' Updating surface attributes...')
          call eclose(gversion(IZ),1.1,0.01,newgeo)
          if(newgeo)then
            call georead(IFIL+2,LGEOM(IZ),IZ,0,iuout,IER)
          else
            call egomin(IFIL+2,LGEOM(IZ),IZ,0,0,iuout,IER)
          endif
          do 323 IJ=1,NSUR
            SOTHER(IJ,1)=TOTHER(IJ,1)
            SOTHER(IJ,2)=TOTHER(IJ,2)
            SOTHER(IJ,2)=TOTHER(IJ,3)
            ICN=IZSTOCN(iz,ij)
            SSOTHER(ICN,1)=TOTHER(IJ,1)
            SSOTHER(ICN,2)=TOTHER(IJ,2)
            SSOTHER(ICN,3)=TOTHER(IJ,3)
  323     continue
  
C Debug
C          write(6,*) (TOTHER(ij,1),ij=1,10)
          call eclose(gversion(IZ),1.1,0.01,newgeo)
          if(igupgrade.eq.2.and.(.NOT.newgeo))then
            gversion(iz) =1.1
            newgeo = .true.
          endif
          if(newgeo)then
            call geowrite(IFIL+2,LGEOM(IZ),IZ,iuout,3,IER)
          else
            call emkgeo(IFIL+2,LGEOM(IZ),IZ,3,IER)
          endif
          IF(IER.NE.0)THEN
            CALL USRMSG(bl,' Problem updating geometry file.','W')
          ENDIF
        endif
  100 CONTINUE

C Ask user if updated information should be saved to different file.
 289  h(1)='At this point the zone geometry files will have updated'
      h(2)='surface attributes. And the master connections list is '
      h(3)='still held in memory and needs to be written out to file'
      h(4)='or the model will be inconsistent.'
      h(5)=' '
      h(6)='This dialog allows you to save the master connections list'
      h(7)='to a different file name (in case you do not want to'
      h(8)='overwrite the existing file. '
      write(DCNN,'(a,a)')cfgroot(1:lnblnk(cfgroot)),'.cnn'
      if(LCNN(1:1).eq.' ')LCNN=DCNN
      CALL EASKS(LCNN,' Surface connections file name?',' ',
     &  72,DCNN,'system connx file name',IER,2)
      IF(LCNN.EQ.' ')GOTO 289
      call EMKCFG('-',IER)

      RETURN
      END

C ************* ESACON 
C ESACON import surface connection attributes into the problem topology
C common block C3 while taking into account confirmation preferences.
C ITRU = unit number for user output, IER=0 OK, IER=1 problem.
      SUBROUTINE ESACON(ITRC,M1SKP,M2SKP,M4SKP,M5SKP,M6SKP,M7SKP,IER)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C14/UGRDTP(12,MGRDP),NGRDP
      COMMON/C24/IZSTOCN(MCOM,MS)
      common/ground/grdtmp(12,mgrdp)

      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY6/LINSTY(MCON)
      common/scanpause/longpause

      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      LOGICAL OK,DOK,MODIFY,MODLEN,MODBND,close,longpause
      logical M1SKP,M2SKP,M4SKP,M5SKP,M6SKP,M7SKP
      logical newgeo  ! to use for testing if new/old geometry file.

      CHARACTER CXSTR*78,outs*124
      CHARACTER OUTSTR*124,SO*12,TMP*54,H*72
      CHARACTER bl*2
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character SSPARENT*12

C General image option flags.
      newgeo=.false.  ! assume older format geometry.
      bl='  '
      ITDSP=1
      ITBND=1
      ITEPT=0
      ITZNM=0
      ITSNM=0
      ITVNO=1
      ITORG=1
      ITSNR=1
      ITGRD=1
      GRDIS=0.0

C Display connection header.
      TMP=' '
      CALL EDISP(iuout,' ')
      CALL CONXINF(1,0,CXSTR)
      CALL EDISP(iuout,CXSTR)

C Loop through each connection in turn. Read in the geometry
C attributes for each set of connections in turn.
C Map between the connection and surface. Only read in geometry
C and display zone when connection jumps to another zone.
      ICL=0
      DO 123 ICC=1,NCON
        CALL CONXINF(1,ICC,CXSTR)
        CALL EDISP(iuout,CXSTR)
        IF(ICL.NE.IC1(ICC))THEN
          call eclose(gversion(IC1(ICC)),1.1,0.01,newgeo)
          if(newgeo)then
            call georead(IFIL+1,LGEOM(IC1(ICC)),IC1(ICC),1,ITRU,IER)
          else
            call egomin(IFIL+1,LGEOM(IC1(ICC)),IC1(ICC),1,0,ITRU,IER)
          endif
          MODIFY=.TRUE.
          MODBND=.TRUE.

C Set all surfaces to standard line width.
          if(MMOD.eq.8)CALL INLNST(1)
          nzg=1
          nznog(1)=IC1(ICC)
          izgfoc=IC1(ICC)
          if(MMOD.eq.8)CALL CADJVIEW(IER)
          IF(IER.NE.0)THEN
            RETURN
          ELSE
            ICL=IC1(ICC)
            ISS=0
          ENDIF
        ENDIF
        ISS=ISS+1

C If surface attribute is something other than UNKNOWN then see about 
C upgrading the current connection. Display the connection and the
C surface attribute and ask user if attribute should be used. First
C check for easy matches.
        ICN=IZSTOCN(IC1(ICC),iss)
        WRITE(SO,'(A)')SSOTHER(ICN,1)(1:12)
        WRITE(TMP,'(A,I3,A,A15)')'Surface ',ISS,' attribute: ',SO
        WRITE(OUTSTR,'(A,A)')'Topology: ',CXSTR(1:lnblnk(CXSTR))
        IF(SO(1:8).EQ.'EXTERIOR'.AND.ICT(ICC).EQ.0)THEN
          CALL EDISP(iuout,'   topology & attribute EXTERIOR matches.')
          goto  123
        ELSEIF(SO(1:7).EQ.'SIMILAR'.AND.ICT(ICC).EQ.1)THEN
          CALL EDISP(iuout,'   topology & attribute SIMILAR matches.')
          GOTO 123
        ELSEIF(SO(1:8).EQ.'CONSTANT'.AND.ICT(ICC).EQ.2)THEN
          CALL EDISP(iuout,'   topology & attribute CONSTANT matches.')
          GOTO 123
        ELSEIF(SO(1:6).EQ.'GROUND'.AND.ICT(ICC).EQ.4)THEN
          CALL EDISP(iuout,'   topology & attribute GROUND match.')
          GOTO 123
        ELSEIF(SO(1:9).EQ.'ADIABATIC'.AND.ICT(ICC).EQ.5)THEN
          CALL EDISP(iuout,'   topology & attribute ADIABATIC match.')
          GOTO 123
        ELSEIF(SO(1:8).EQ.'BASESIMP'.AND.ICT(ICC).EQ.6)THEN
          CALL EDISP(iuout,'   topology & attribute BASESIMP match.')
          GOTO 123
        ELSEIF(SO(1:9).EQ.'IDENT_CEN'.AND.ICT(ICC).EQ.7)THEN
          CALL EDISP(iuout,'   topology & attribute IDENT_CEN match.')
          GOTO 123
        ELSEIF(SO(1:7).EQ.'UNKNOWN'.OR.SO(1:2).EQ.'  ')THEN
          CALL EDISP(iuout,' surface attribute was UNKNOWN: skipping.')
          GOTO 123
        ELSEIF(ICT(ICC).EQ.3.AND.(SO.EQ.zname(IC2(ICC))))then
          CALL EDISP(iuout,'   connected zone names match.')
          GOTO 123
        ELSEIF(SO(1:7).EQ.'SIMILAR')THEN
          MODIFY=.TRUE.
          if(MMOD.eq.8)CALL INLNST(1)
          LINSTY(ICC)=2
          nzg=1
          nznog(1)=IC1(ICC)
          izgfoc=IC1(ICC)
          if(MMOD.eq.8)CALL CADJVIEW(IER)
          if(.NOT.M1SKP)then
            call edisp(iuout,OUTSTR)
            call edisp(iuout,TMP)
            dok=.true.
            h(1)='This pause allows you to correct errors. '
            CALL ASKOK(' ','Use this surface attribute?',OK,dok,1)
            IF(.NOT.OK)GOTO 123
          endif
          CALL EDISP(iuout,'   topology << SIMILAR.')
          if(MMOD.eq.8)then
            if(longpause)then
              call pauses(1)
            else
              call pausems(400)
            endif
          endif
          ICT(ICC)=1
          IC2(ICC)=0
          IE2(ICC)=0
          GOTO 123
        ELSEIF(SO(1:8).EQ.'CONSTANT')THEN
          MODIFY=.TRUE.
          if(MMOD.eq.8)CALL INLNST(1)
          LINSTY(ICC)=2
          nzg=1
          nznog(1)=IC1(ICC)
          izgfoc=IC1(ICC)
          if(MMOD.eq.8)CALL CADJVIEW(IER)
          if(.NOT.M2SKP)then
            call edisp(iuout,OUTSTR)
            call edisp(iuout,TMP)
            dok=.true.
            h(1)='This pause allows you to correct errors. '
            CALL ASKOK(' ','Use this surface attribute?',OK,dok,1)
            IF(.NOT.OK)GOTO 123
          endif
          CALL EDISP(iuout,'   topology << CONSTANT.')
          ICT(ICC)=2
          H(1)='A type 2 connection requires that a constant '
          H(2)='temperature and radiation (Watts) be entered.'
          VAL=IC2(ICC)
          CALL EASKR(VAL,' ',' Constant temperature ? ',
     &        -99.,'F',99.,'F',20.,'adjacent temperature',IER,2)
          VAL2=IE2(ICC)
          CALL EASKR(VAL2,' ',' Constant rad (W/m^2) ? ',
     &        -999.,'F',999.,'F',0.,'adjacent rad',IER,2)
          IC2(ICC)=INT(VAL)
          IE2(ICC)=INT(VAL2)
          GOTO 123
        ELSEIF(SO(1:6).EQ.'GROUND')THEN
          MODIFY=.TRUE.
          if(MMOD.eq.8)CALL INLNST(1)
          LINSTY(ICC)=2
          nzg=1
          nznog(1)=IC1(ICC)
          izgfoc=IC1(ICC)
          if(MMOD.eq.8)CALL CADJVIEW(IER)
          if(.NOT.M4SKP)then
            call edisp(iuout,OUTSTR)
            call edisp(iuout,TMP)
            dok=.true.
            h(1)='This pause allows you to correct errors. '
            CALL ASKOK(' ','Use this surface attribute?',OK,dok,1)
            IF(.NOT.OK)GOTO 123
          endif
          CALL EDISP(iuout,'   topology << GROUND.')
          ICT(ICC)=4
          CALL EDISP(iuout,'Standard ground profiles Jan-Dec:')
          CALL EDISP(iuout,'id  Jan, Feb, Mar, Apr, May, Jun, Jul...')
          do 21 igrdp=1,mgrdp
            CALL ECLOSE(GRDTMP(6,igrdp),0.0,0.001,close)
            if(.NOT.close)then
              WRITE(OUTS,'(I2,12F5.1)')igrdp,(GRDTMP(J,IGRDP),J=1,12)
              call edisp(iuout,outs)
            endif
21        continue

C List out user defined profiles, assuming that if June is 0.0, then the
C profile has not been defined.
          if(NGRDP.gt.0)then
            CALL EDISP(iuout,' ')
            do 22 igrdp=1,NGRDP    
              CALL EDISP(iuout,'User defined ground profile Jan-Dec:')
              WRITE(OUTS,'(12F6.1)')(UGRDTP(J,IGRDP),J=1,12)
              call edisp(iuout,outs)
 22         continue
          endif
          H(1)='A ground connection allows either:'
          H(2)='> 0 - a connection to one of several standard profiles'
          H(3)='  in which case the profile number is required.'
          H(4)='= 0 - a connection to a user defined profile:'
          H(5)='    a 0 followed by a profile number is required.'
          H(6)='-3  a connection to a 3D ground model.'
          IVAL=1
          CALL EASKI(IVAL,' Monthly profile (0 = user defined) ? ',
     &      ' ',-3,'F',12,'F',1,'ground profile',IER,5)
          IC2(ICC)=IVAL
          IF(IC2(ICC).GT.0)THEN
            IE2(ICC)=0
          ELSEIF(IC2(ICC).eq.0)THEN
            H(1)='One of the user defined profiles numbers is'
            H(2)='required at this point.'
            IVAL=IE2(ICC)
            CALL EASKI(IVAL,' User defined profile: ',
     &        ' ',0,'F',9,'F',1,'user defined profile',IER,2)
            IE2(ICC)=IVAL
          ELSEIF(IC2(ICC).eq.-3)THEN
            IE2(ICC)=0
          ENDIF
          GOTO 123
        ELSEIF(SO(1:9).EQ.'ADIABATIC')THEN
          MODIFY=.TRUE.
          if(MMOD.eq.8)CALL INLNST(1)
          LINSTY(ICC)=2
          nzg=1
          nznog(1)=IC1(ICC)
          izgfoc=IC1(ICC)
          if(MMOD.eq.8)CALL CADJVIEW(IER)
          if(.NOT.M5SKP)then
            call edisp(iuout,OUTSTR)
            call edisp(iuout,TMP)
            dok=.true.
            h(1)='This pause allows you to correct errors. '
            CALL ASKOK(' ','Use this surface attribute?',OK,dok,1)
            IF(.NOT.OK)GOTO 123
          endif
          CALL EDISP(iuout,'   topology << ADIABATIC.')
          if(MMOD.eq.8)then
            if(longpause)then
              call pauses(1)
            else
              call pausems(400)
            endif
          endif
          ICT(ICC)=5
          IC2(ICC)=0
          IE2(ICC)=0
          GOTO 123
        ELSEIF(SO(1:8).EQ.'BASESIMP')THEN

C BASESIMP start.
C Get BASESIMP configuration via call to bsimtype. Save state of current
C menu and recover after the call.
          MODIFY=.TRUE.
          if(MMOD.eq.8)CALL INLNST(1)
          LINSTY(ICC)=2
          nzg=1
          nznog(1)=IC1(ICC)
          izgfoc=IC1(ICC)
          if(MMOD.eq.8)CALL CADJVIEW(IER)
          if(.NOT.M6SKP)then
            call edisp(iuout,OUTSTR)
            call edisp(iuout,TMP)
            dok=.true.
            h(1)='This pause allows you to correct errors. '
            CALL ASKOK(' ','Use this surface attribute?',OK,dok,1)
            IF(.NOT.OK)GOTO 123
          endif
          CALL EDISP(iuout,'   topology << BASESIMP.')
          if(MMOD.eq.8)then
            if(longpause)then
              call pauses(1)
            else
              call pausems(400)
            endif
          endif
          ICT(ICC)=6

          H(1)='Specify the BASESIMP configuration index.'
          H(2)='Refer to src/esp-r/esrubld/bscoeff.F for the '
          H(3)='correspondence between iconfig and standard '
          H(4)='BASESIMPdescriptors (e.g. BCIN_1).'
C          CALL EPMENSV
          IBS=IC2(ICC)
          call bsimtype(ibs)
C           CALL EPMENRC
          IC2(ICC)=IBS

C Get `surface weighting factor', the percentage of the BASESIMP heat loss to
C attribute to the surface under consideration.
          Ifrac=IE2(ICC)
          H(1)='BASESIMP is a `whole-foundation model`. Therefore, when'
          H(2)='a foundation is represented by numerous ESP-r surfaces'
          H(3)='(e.g. four walls + floor to represent a basement), you'
          H(4)='must specify the percentage of the whole-foundation'
          H(5)='heat loss to attribute to each surface. Normally the'
          H(6)='sum of surfaces representing a founcation is 100%.'
          CALL EASKI(Ifrac,
     &      ' Percent of BASESIMP loss to this surface?'
     &      ,' ',0,'F',100,'F',100,'BASESIMP heat loss percent',IER,6)
          IE2(ICC)=Ifrac
C BASESIMP end.
          GOTO 123
        ELSEIF(SO(1:9).EQ.'IDENT_CEN')THEN
          MODIFY=.TRUE.
          if(MMOD.eq.8)CALL INLNST(1)
          LINSTY(ICC)=2
          nzg=1
          nznog(1)=IC1(ICC)
          izgfoc=IC1(ICC)
          if(MMOD.eq.8)CALL CADJVIEW(IER)
          if(.NOT.M7SKP)then
            call edisp(iuout,OUTSTR)
            call edisp(iuout,TMP)
            dok=.true.
            h(1)='This pause allows you to correct errors. '
            CALL ASKOK(' ','Use this surface attribute?',OK,dok,1)
            IF(.NOT.OK)GOTO 123
          endif
          CALL EDISP(iuout,'   topology << IDENT_CEN.')
          if(MMOD.eq.8)then
            if(longpause)then
              call pauses(1)
            else
              call pausems(400)
            endif
          endif
          ICT(ICC)=7
          IC2(ICC)=0
          IE2(ICC)=0
          GOTO 123
        ELSEIF(SO(1:8).EQ.'EXTERIOR')THEN
          MODIFY=.TRUE.
          if(MMOD.eq.8)CALL INLNST(1)
          LINSTY(ICC)=2
          nzg=1
          nznog(1)=IC1(ICC)
          izgfoc=IC1(ICC)
          if(MMOD.eq.8)CALL CADJVIEW(IER)
          call edisp(iuout,OUTSTR)
          call edisp(iuout,TMP)
          dok=.true.
          h(1)='This pause allows you to correct errors. '
          CALL ASKOK(' ','Use this surface attribute?',OK,dok,1)
          IF(.NOT.OK)GOTO 123
          CALL EDISP(iuout,'   topology << EXTERIOR.')
          if(MMOD.eq.8)then
            if(longpause)then
              call pauses(1)
            else
              call pausems(400)
            endif
          endif
          ICT(ICC)=0
          IC2(ICC)=0
          IE2(ICC)=0
        ELSE

C Not an easy match or mandated match so tell user.
          MODIFY=.TRUE.
          if(MMOD.eq.8)CALL INLNST(1)
          LINSTY(ICC)=2
          nzg=1
          nznog(1)=IC1(ICC)
          izgfoc=IC1(ICC)
          if(MMOD.eq.8)CALL CADJVIEW(IER)
          WRITE(OUTSTR,'(A,A)')'Topology: ',CXSTR(1:lnblnk(CXSTR))
          call edisp(iuout,' ')
          call edisp(iuout,OUTSTR)
          call edisp(iuout,TMP)
          CALL usrmsg(bl,'Attribute cannot be imported.','-')
          if(MMOD.eq.8)then
            if(longpause)then
              call pauses(1)
            else
              call pausems(400)
            endif
          endif
          GOTO 123
        ENDIF
  123 CONTINUE

C Ask user if updated info should be saved to file.
      dok=.true.
      h(1)='Assuming the last task was successful you need to '
      h(2)='save the topology to file to preserve any changes '
      h(3)='made to the model.  '
      CALL ASKOK(' ','Save updated topology?',OK,dok,3)
      if(OK)then
        call EMKCFG('-',IER)
      endif

      RETURN
      END

C **************** anchor 
C anchor defines and instanciates anchor points (a list of connections
C associated with a concept).
      subroutine anchor(ifoc,ier)
#include "building.h"
#include "epara.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      COMMON/RAY6/LINSTY(MCON)

C Anchor commons:
C NALOC = nb of anchors,
C ALOC = substitution attribute used with the the anchor (usually
C   passed from a command to a function which manipulates the anchor.),
C ALOCLBL = user's name for the anchor,
C ALOCTYP = type (SURF|CTRL|LOAD) for the anchor,
C IALOC = nb of links (surfaces, etc.) associated with the anchor,
C lstanchr = links for each anchor
      common/anchora/NALOC,ALOC(20),ALOCLBL(20),ALOCTYP(20)
      common/anchorb/IALOC(20),lstanchr(20,60)

      DIMENSION VERT(35),flag(mcon),iconnpk(60)
      CHARACTER VERT*54,KEY*1,H*72,outs*124
      character CXITM*43,flag*9
      character ALOC*12,ALOCLBL*12,ALOCTYP*4,AL*12
      logical found,ok,dok,foundone,okd,MODIFY,MODLEN,MODBND

C Initialise connection menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
  31  MHEAD=7
      MCTL=4
      ILEN=NCON
      IPACT=CREATE
      CALL EKPAGE(IPACT)

      do 2 i = 1,ncon
        flag(i)= '      '
  2   continue

C Force redraw of zones, highlighting anchor list.
      MODIFY=.TRUE.
      nzg=NCOMP
      if(nzg.gt.0)then
        DO 44 I=1,nzg
          nznog(I)=I
  44    CONTINUE
        CALL INLNST(1)
        do 45 jj = 1,IALOC(ifoc)
          kk=lstanchr(ifoc,jj)
          if(kk.ne.0)LINSTY(kk)=2
  45    continue
        CALL ADJVIEW(IER)
      endif

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Help text.
   3  h(1)='Anchor points allow a user to line a number of items '
      h(2)='(such as surfaces) to a concept so that global actions'
      h(3)='(such as attribution or performance enquiries) can be '
      h(4)='undertaken. An achor point consists of name (12 char),'
      h(5)='a list of item indices and a type indicator. '
      h(6)=' '
      h(7)='Anchor types set the context within which esp-r can'
      h(8)='manipulate items associated with an anchor. Details: '
      h(9)=' a general surface - supports actions such as changing'
      h(10)='   attributes or graphing temperatures of a '
      h(11)='   collection of surfaces).'
      h(12)=' '
      h(13)='Tagging surfaces as possible openings or leakage paths'
      h(14)='in a flow network:'
      h(15)=' b closed door - surface acts as or includes a closed'
      h(16)='   door (or crack) between two flow nodes.'
      h(17)=' c open(able) door - surface acts as or includes an '
      h(18)='   open door (perhaps controlled) between flow nodes.'
      h(19)=' d closed window - surface acts as/includes a closed'
      h(20)='   window (or crack) between two flow nodes.'
      h(21)=' e open(able) window - surface acts as/includes a'
      h(22)='   window (perhaps controlled) between flow nodes.'
      h(23)=' f closed grill - surface acts as/includes a closed'
      h(24)='   grill (or crack) between two flow nodes.'
      h(25)=' g open(able) grill - surface includes an grill or'
      h(26)='   vent (perhaps controlled) between two flow nodes.'
      h(27)=' h duct or pipe - surface inclues an duct or pipe'
      h(28)='   between two flow nodes.'
      h(29)=' i fan or pump - surface near a fan or pump between'
      h(30)='   two flow nodes.'

C Set menu header text. Update flag string with * depending on which
C anchor set(s) is/are associated with a connection.
      write(VERT(1),'(a)') '  anchor data: '
      write(VERT(2),'(a)') '  ____________________________________'
      write(VERT(3),'(a)') '  index  label    type       nb. links'
      write(VERT(4),'(a,i2,5a,i2,a)') '1 ',ifoc,' ',ALOCLBL(ifoc),' ',
     &  ALOCTYP(ifoc),' (',IALOC(ifoc),' links)'
      VERT(5)='  ___________________________________________   '
      VERT(6)=' conn|  connection      |    connection     |123456789'
      VERT(7)=' no. | inside face      | other side data   |anchor'
      do 5 i=1,NALOC
        if(IALOC(i).gt.0)then
          do 4 j = 1,IALOC(i)
            k=lstanchr(i,j)
            if(k.ne.0.and.i.eq.1)write(flag(k)(1:1),'(a)') '*'
            if(k.ne.0.and.i.eq.2)write(flag(k)(2:2),'(a)') '*'
            if(k.ne.0.and.i.eq.3)write(flag(k)(3:3),'(a)') '*'
            if(k.ne.0.and.i.eq.4)write(flag(k)(4:4),'(a)') '*'
            if(k.ne.0.and.i.eq.5)write(flag(k)(5:5),'(a)') '*'
            if(k.ne.0.and.i.eq.6)write(flag(k)(6:6),'(a)') '*'
            if(k.ne.0.and.i.eq.7)write(flag(k)(7:7),'(a)') '*'
            if(k.ne.0.and.i.eq.8)write(flag(k)(8:8),'(a)') '*'
            if(k.ne.0.and.i.ge.9)write(flag(k)(9:9),'(a)') '*'
   4      continue
        endif
   5  continue

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          call CONXMENU(L,CXITM)
          VERT(M)=' '
          WRITE(VERT(M),'(A1,1x,A42,1a,a)')KEY,CXITM(1:42),'|',flag(L)
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  ________________________________________ '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page ------- Part: ',I2,' of ',I2,' -----')
      ENDIF
      VERT(M+2)  ='* modify list                         '
      VERT(M+3)  ='? help                                '
      VERT(M+4)  ='- exit this menu                      '

C Now display the menu.
      CALL EMENU('  Anchor points',VERT,MVERT,IVERT)

      if(ivert.eq.mvert)then

C Save and return. 
        if(NALOC.gt.0)CALL EMKCFG('-',IER)
        return
      elseif(ivert.eq.mvert-1)then
        CALL PHELPD('anchor points',30,'-',0,0,IER)
      elseif(ivert.eq.mvert-2)then
        if(IALOC(ifoc).eq.0)then
          inpic = 60
        else
          inpic = 60 - IALOC(ifoc)
        endif
        CALL EPMENSV
        call easkconn(inpic,iconnpk,60,
     &    'Select from the list of connections the',
     &    'surfaces associated with this anchor.',ALOCLBL(ifoc),
     &    ifoc,ier,30)
        CALL EPMENRC
        if(inpic.gt.0)then
          do 242 ij=1,inpic

C For each item from iconnpk that does not match an existing
C anchor update the list.
            found=.false.
            if(IALOC(ifoc).eq.0)then
              IALOC(ifoc)=IALOC(ifoc)+1
              lstanchr(ifoc,IALOC(ifoc))=iconnpk(ij)
            else
              do 243 ik=1,IALOC(ifoc)
                if(lstanchr(ifoc,ik).eq.iconnpk(ij))found=.true.
  243         continue
              if(.NOT.found)then
                IALOC(ifoc)=IALOC(ifoc)+1
                lstanchr(ifoc,IALOC(ifoc))=iconnpk(ij)
              endif
            endif
  242     continue
          goto 31
        else
          call edisp(iuout,'No surfaces associated with anchor.')
        endif
        goto 31
      elseif(ivert.eq.mvert-3)then

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      elseif(ivert.eq.4)then

C Force redraw of zones, highlighting anchor list.
        MODIFY=.TRUE.
        nzg=NCOMP
        if(nzg.gt.0)then
          DO 144 I=1,nzg
            nznog(I)=I
 144      CONTINUE
          CALL INLNST(1)
          do 145 jj = 1,IALOC(ifoc)
            kk=lstanchr(ifoc,jj)
            if(kk.ne.0)LINSTY(kk)=2
 145      continue
          CALL ADJVIEW(IER)
        endif
        dok=.false.
        h(1)='A chance to abort incorrect selection. '
        call askok('Edit anchor name or type?',' ',ok,dok,1)
        if(ok)then
          ino=0
          if(ALOCTYP(ifoc)(1:4).eq.'SURF')ino=1
          if(ALOCTYP(ifoc)(1:4).eq.'CLDR')ino=2
          if(ALOCTYP(ifoc)(1:4).eq.'OPDR')ino=3
          if(ALOCTYP(ifoc)(1:4).eq.'CLWN')ino=4
          if(ALOCTYP(ifoc)(1:4).eq.'OPWN')ino=5
          if(ALOCTYP(ifoc)(1:4).eq.'CLGR')ino=6
          if(ALOCTYP(ifoc)(1:4).eq.'OPGR')ino=7
          if(ALOCTYP(ifoc)(1:4).eq.'DUCT')ino=8
          if(ALOCTYP(ifoc)(1:4).eq.'FANP')ino=9
          ilno=ino
          idno=1
          call MENUATOL('Select a type for the anchor:',
     &    'Anchor type (options)','a surface (general)','b closed door',
     &    'c open(able) door','d closed window','e open(able) window',
     &    'f closed grill','g openable grill','h duct or pipe',
     &    'i fan or pump',' ',' ',' ',ino,idno,30)
          if(ino.eq.0)then
            goto 31
          elseif(ino.eq.1)then
            AL=ALOCLBL(ifoc)
            if(AL(1:4).eq.'unde')AL='  '
            CALL EASKS(AL,' Label for anchor: ',
     &        ' ',12,'none','anchor label',IER,30)
            ALOCLBL(ifoc)=AL
            ALOCTYP(ifoc)='SURF'
          elseif(ino.eq.2)then
            if(ALOCLBL(ifoc)(1:4).eq.'unde')AL='closed_door'
            CALL EASKS(AL,'Label:',' ',12,'none','closed dr',IER,30)
            ALOCLBL(ifoc)=AL
            ALOCTYP(ifoc)='CLDR'
          elseif(ino.eq.3)then
            if(ALOCLBL(ifoc)(1:4).eq.'unde')AL='open_door'
            CALL EASKS(AL,'Label:',' ',12,'none','open dr ',IER,30)
            ALOCLBL(ifoc)=AL
            ALOCTYP(ifoc)='OPDR'
          elseif(ino.eq.4)then
            if(ALOCLBL(ifoc)(1:4).eq.'unde')AL='closed_windw'
            CALL EASKS(AL,'Label:',' ',12,'none','closd win',IER,30)
            ALOCLBL(ifoc)=AL
            ALOCTYP(ifoc)='CLWN'
          elseif(ino.eq.5)then
            if(ALOCLBL(ifoc)(1:4).eq.'unde')AL='open_window'
            CALL EASKS(AL,'Label:',' ',12,'none','open win',IER,30)
            ALOCLBL(ifoc)=AL
            ALOCTYP(ifoc)='OPWN'
          elseif(ino.eq.6)then
            if(ALOCLBL(ifoc)(1:4).eq.'unde')AL='closed_grill'
            CALL EASKS(AL,'Label:',' ',12,'none','closd grill',IER,30)
            ALOCLBL(ifoc)=AL
            ALOCTYP(ifoc)='CLGR'
          elseif(ino.eq.7)then
            if(ALOCLBL(ifoc)(1:4).eq.'unde')AL='open_grill'
            CALL EASKS(AL,'Label:',' ',12,'none','open grill',IER,30)
            ALOCLBL(ifoc)=AL
            ALOCTYP(ifoc)='OPGR'
          elseif(ino.eq.8)then
            if(ALOCLBL(ifoc)(1:4).eq.'unde')AL='duct'
            CALL EASKS(AL,'Label:',' ',12,'none','ductorpipe',IER,30)
            ALOCLBL(ifoc)=AL
            ALOCTYP(ifoc)='DUCT'
          elseif(ino.eq.9)then
            if(ALOCLBL(ifoc)(1:4).eq.'unde')AL='fanorpump'
            CALL EASKS(AL,'Label:',' ',12,'none','fanorpump',IER,30)
            ALOCLBL(ifoc)=AL
            ALOCTYP(ifoc)='FANP'
          endif
          CALL EMKCFG('-',IER)
        endif
      elseif(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))then

C Enquire about connection identified by KEYIND.
        CALL KEYIND(MVERT,IVERT,IFOCC,IO)

C Display information about the anchors associated with connection.
        call CONXMENU(ifocc,CXITM)
        write(outs,'(a,a)') CXITM(1:lnblnk(CXITM)),
     &    ' is associated with the following anchors...'
        call edisp(iuout,outs)
        outs=' '
        foundone=.false.
        do 6 i=1,NALOC
          if(IALOC(i).gt.0)then
            do 7 j = 1,IALOC(i)
              k=lstanchr(i,j)
              if(k.eq.ifocc.and.i.gt.0)then
                foundone=.true.
                write(outs(1:12),'(a,a)') ALOCLBL(i)
                call edisp(iuout,outs)
              endif
   7        continue
          endif
   6    continue
        if(foundone)then
          dok=.false.
          h(1)='A chance to abort de-reference of link. '
          call askok('De-reference link(s) to this surface?',' ',
     &      ok,dok,1)
          if(ok)then

C Loop through all of the anchor lists and check if any reference this
C connection. If use requests deletion, shift list (lstanchr) and
C decrement IALOC.
            do 16 i=1,NALOC
              ikk=IALOC(i)
              if(ikk.gt.0)then
                do 17 j = 1,ikk
                  k=lstanchr(i,j)
                  if(k.eq.ifocc)then
                   write(outs,'(3a)')'Dereference ',ALOCLBL(i),'?'
                    dok=.true.
                    h(1)='A chance not to dereference this anchor. '
                    call askok(outs,' ',okd,dok,1)
                    if(okd)then
                      do 18 ik=j,ikk-1
                        lstanchr(i,j)=lstanchr(i,j+1)
  18                  continue
                      IALOC(i)=IALOC(i)-1
                    endif
                  endif
  17            continue
              endif
  16        continue
            goto 31
          endif
        endif
C << what additional functionality?? >>
      else
        ivert=-1
        goto 92
      endif
      ivert=-4
      goto 3

      end

C **************** anchlist 
C anchlist allows selection and list management of anchor points (a list of connections
C associated with a concept).
      subroutine anchlist(iasel,ier)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

      common/anchora/NALOC,ALOC(20),ALOCLBL(20),ALOCTYP(20)
      common/anchorb/IALOC(20),lstanchr(20,60)

      DIMENSION AVERT(27)
      CHARACTER AVERT*36,KEY*1,H*72
      character ALOC*12,ALOCLBL*12,ALOCTYP*4,AL*12

      iasel=0

C Initialise connection menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      if(NALOC.eq.0)then
        call usrmsg('Currently there are no anchors defined. ',
     &    'Begin by selecting the + option. ','W')
      endif
  31  MHEAD=3
      MCTL=4
C Debug
C      write(6,*) 'naloc mhead mctl ',naloc,mhead,mctl

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Help text.
   3  h(1)='Anchor points allow a user to line a number of items '
      h(2)='(such as surfaces) to a concept so that global actions'
      h(3)='(such as attribution or performance enquiries) can be '
      h(4)='undertaken. An achor point consists of name (12 char),'
      h(5)='a list of item indices and a type indicator. '
      h(6)=' '
      h(7)='Anchor types set the context within which esp-r can'
      h(8)='manipulate items associated with an anchor. Details: '
      h(9)=' a general surface - supports actions such as changing'
      h(10)='   attributes or graphing temperatures of a '
      h(11)='   collection of surfaces).'
      h(12)=' '
      h(13)='Tagging surfaces as possible openings or leakage paths'
      h(14)='in a flow network:'
      h(15)=' b closed door - surface acts as or includes a closed'
      h(16)='   door (or crack) between two flow nodes.'
      h(17)=' c open(able) door - surface acts as or includes an '
      h(18)='   open door (perhaps controlled) between flow nodes.'
      h(19)=' d closed window - surface acts as/includes a closed'
      h(20)='   window (or crack) between two flow nodes.'
      h(21)=' e open(able) window - surface acts as/includes a'
      h(22)='   window (perhaps controlled) between flow nodes.'
      h(23)=' f closed grill - surface acts as/includes a closed'
      h(24)='   grill (or crack) between two flow nodes.'
      h(25)=' g open(able) grill - surface includes an grill or'
      h(26)='   vent (perhaps controlled) between two flow nodes.'
      h(27)=' h duct or pipe - surface inclues an duct or pipe'
      h(28)='   between two flow nodes.'
      h(29)=' i fan or pump - surface near a fan or pump between'
      h(30)='   two flow nodes.'

C Set menu header text.
      write(AVERT(1),'(a,i2)') '1 anchors: ',NALOC
      write(AVERT(2),'(a)')    '  ________________________________ '
      write(AVERT(3),'(a)')    '  index  label     type  links     '
      M=MHEAD
      do 5 L=1,NALOC
        M=M+1
        CALL EMKEY(L,KEY,IER)
        AVERT(M)=' '
        write(AVERT(M),'(2a,i2,5a,i2,a)') KEY,' ',L,'   ',ALOCLBL(L),
     &  ' ',ALOCTYP(L),' (',IALOC(L),' links)'
   5  continue

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      AVERT(M+1)  ='  ________________________________ '
      AVERT(M+2)  ='+ add/delete an anchor             '
      AVERT(M+3)  ='? help                             '
      AVERT(M+4)  ='- exit this menu                   '

C Now display the menu.
      if(mmod.eq.8)then
        CALL EMENU('  Current anchor points',AVERT,MVERT,IVERT)
      else
        CALL EMENU('  Anchor points',AVERT,MVERT,IVERT)
      endif

      if(ivert.eq.mvert)then

C Save and return. 
        if(NALOC.gt.0)CALL EMKCFG('-',IER)
        return
      elseif(ivert.eq.mvert-1)then
        CALL PHELPD('anchor points',30,'-',0,0,IER)
      elseif(ivert.eq.mvert-2)then
        CALL EASKABC('Anchor point options:',' ',
     &    'add an anchor','delete an anchor','contine',II,6)
        if(II.eq.1)then
          NALOC=NALOC+1
          ino=0
          ilno=0
          idno=3
          call MENUATOL('Select a type for the anchor:',
     &    'Anchor type (options)','a surface (general)','b closed door',
     &    'c open(able) door','d closed window','e open(able) window',
     &    'f closed grill','g openable grill','h duct or pipe',
     &    'i fan or pump',' ',' ',' ',ino,idno,30)
          if(ino.eq.0)then
            goto 31
          elseif(ino.eq.1)then
            AL=ALOCLBL(NALOC)
            CALL EASKS(AL,' Label for anchor: ',
     &        ' ',12,'none','anchor label',IER,30)
            ALOCLBL(NALOC)=AL
            ALOCTYP(NALOC)='SURF'
          elseif(ino.eq.2)then
            if(ALOCLBL(NALOC)(1:4).eq.'unde')AL='closed_door'
            CALL EASKS(AL,'Label:',' ',12,'none','closed dr',IER,30)
            ALOCLBL(NALOC)=AL
            ALOCTYP(NALOC)='CLDR'
          elseif(ino.eq.3)then
            if(ALOCLBL(NALOC)(1:4).eq.'unde')AL='open_door'
            CALL EASKS(AL,'Label:',' ',12,'none','open dr ',IER,30)
            ALOCLBL(NALOC)=AL
            ALOCTYP(NALOC)='OPDR'
          elseif(ino.eq.4)then
            if(ALOCLBL(NALOC)(1:4).eq.'unde')AL='closed_windw'
            CALL EASKS(AL,'Label:',' ',12,'none','closd win',IER,30)
            ALOCLBL(NALOC)=AL
            ALOCTYP(NALOC)='CLWN'
          elseif(ino.eq.5)then
            if(ALOCLBL(NALOC)(1:4).eq.'unde')AL='open_window'
            CALL EASKS(AL,'Label:',' ',12,'none','open win',IER,30)
            ALOCLBL(NALOC)=AL
            ALOCTYP(NALOC)='OPWN'
          elseif(ino.eq.6)then
            if(ALOCLBL(NALOC)(1:4).eq.'unde')AL='closed_grill'
            CALL EASKS(AL,'Label:',' ',12,'none','closd grill',IER,30)
            ALOCLBL(NALOC)=AL
            ALOCTYP(NALOC)='CLGR'
          elseif(ino.eq.7)then
            if(ALOCLBL(NALOC)(1:4).eq.'unde')AL='open_grill'
            CALL EASKS(AL,'Label:',' ',12,'none','open grill',IER,30)
            ALOCLBL(NALOC)=AL
            ALOCTYP(NALOC)='OPGR'
          elseif(ino.eq.8)then
            if(ALOCLBL(NALOC)(1:4).eq.'unde')AL='duct'
            CALL EASKS(AL,'Label:',' ',12,'none','ductorpipe',IER,30)
            ALOCLBL(NALOC)=AL
            ALOCTYP(NALOC)='DUCT'
          elseif(ino.eq.9)then
            if(ALOCLBL(NALOC)(1:4).eq.'unde')AL='fanorpump'
            CALL EASKS(AL,'Label:',' ',12,'none','fanorpump',IER,30)
            ALOCLBL(NALOC)=AL
            ALOCTYP(NALOC)='FANP'
          endif
          call usrmsg('You may now associate surfaces with this',
     &      'anchor... Begin by selecting from the list. ','W')
          iasel=NALOC
          if(NALOC.gt.0)CALL EMKCFG('-',IER)
          call anchor(iasel,ier)
          goto 31
        elseif(II.eq.2)then

C Deletion function. << to be done >>

        elseif(II.eq.3)then
          continue
        endif
        goto 31
      elseif(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))then

C Pass one back for selection and manipulation.
        iasel=ivert-MHEAD
        call anchor(iasel,ier)
        goto 31
      else
        ivert=-1
        goto 92
      endif
      ivert=-4
      goto 3

      end

C **************** easkconn 
C Select one or more connections from a list.
      subroutine easkconn(inpic,iconnpk,lconnpk,promp1,promp2,ermsg,
     &  ltype,ier,nhelp)
#include "building.h"
#include "epara.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      common/anchorb/IALOC(20),lstanchr(20,60)

      DIMENSION iconnpk(lconnpk),VERT(35),flag(mcon)
      CHARACTER*(*) PROMP1,PROMP2,ERMSG
      CHARACTER VERT*54,KEY*1,H*72,CXITM*43,flag*1,SEL*50
      logical found
    
C At this point bring up a menu with the string alternatives, assuming
C there is more than one alternative to pick from.
      IF(lconnpk.LT.1)THEN
        CALL USRMSG(PROMP1,' Nunber of items to small.','W')
        IER=1
        RETURN
      ENDIF
      CALL USRMSG(PROMP1,PROMP2,'-')

C Initialise connection menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      MHEAD=2
      MCTL=3
      ILEN=NCON
      IPACT=CREATE
      CALL EKPAGE(IPACT)
      IALLOW=INPIC

C Clear iconnpk and INPIC and, if editing an existing anchor, point
C to those items already associated.
      IER=0
      INPIC=0
      DO 40 I=1,lconnpk
        iconnpk(I)=0
   40 CONTINUE
      if(IALOC(ltype).gt.0)then
        do 42 ia=1,IALOC(ltype)
          if(lstanchr(ltype,ia).ne.0)then
            iconnpk(ia)=lstanchr(ltype,ia)
          endif
  42    continue
      endif

C If previous list being edited, marke these.
      do 2 i = 1,ncon
        flag(i)= ' '
        if(IALOC(ltype).gt.0)then
          do 41 ia=1,IALOC(ltype)
            if(lstanchr(ltype,ia).eq.i)then
              flag(i)= '*'
            endif
  41      continue
        endif
  2   continue

C Initial menu entry setup.
   92 IER=0
      IVERT=-3

C Set menu header text. Update flag string with * depending on which
C items have been selected. Generate text for title.
    3 lne=LNBLNK(ERMSG)
      IF(lne.GT.50)WRITE(SEL,'(1X,A)')ERMSG(1:50)
      IF(lne.LE.50)WRITE(SEL,'(1X,A)')ERMSG(1:lne)
      VERT(1)=  ' no. | inside face     | other side data   |anchor'
      VERT(2)=  ' conn|  connection     |    connection     |select'

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 10 L=1,ILEN
        IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
          M=M+1
          CALL EMKEY(L,KEY,IER)
          call CONXMENU(L,CXITM)
          VERT(M)=' '
          WRITE(VERT(M),'(A1,1x,A43,1x,a1)')KEY,CXITM,flag(L)
        ENDIF
   10 CONTINUE

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)='  ________________________________________ '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page ------- Part: ',I2,' of ',I2,' -----')
      ENDIF
      VERT(M+2)  ='? help                                '
      VERT(M+3)  ='- exit this menu                      '

C Help text for the menu.
      IF(NHELP.GT.0)then
        WRITE(H(NHELP+1),65)
      ELSEIF(NHELP.EQ.0)then
        WRITE(H(1),'(A,A)')' No help available for ',ERMSG
        WRITE(H(2),65)
      ENDIF

C Now display the menu.
      CALL EMENU(sel,VERT,MVERT,IVERT)

      if(IVERT.LE.MHEAD)then

C Within the header so skip request.
        IVERT=-1
        goto 3
      elseif(ivert.eq.mvert)then
        return
      elseif(ivert.eq.mvert-1)then

C Produce help text for the menu.
        IF(NHELP.GT.0)then
          CALL PHELPD('connection pick string',NHELP+1,'-',0,0,IER)
        ELSEIF(NHELP.EQ.0)then
          CALL PHELPD('connection pick string',2,'-',0,0,IER)
        ENDIF
  65    format(' Type key to select, repeat as',
     &         ' required. -Exit to return list.')
      elseif(ivert.eq.mvert-2)then

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      elseif(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))then

C Mark identified by KEYIND. Increment ialoc counter and then
C instanciate iconnpk with current connection number. If item
C already selected, treat second selection as a toggle OFF.
        CALL KEYIND(MVERT,IVERT,IFOC,IO)
        FOUND=.FALSE.
        IF(INPIC.GT.0)THEN
          DO 44 J=1,INPIC
            if(iconnpk(J).EQ.IFOC)then
              FOUND=.TRUE.
              ljfoc=iconnpk(j)
              lj=j
            endif
  44      CONTINUE
          if(.NOT.FOUND)then
            INPIC=INPIC+1
            iconnpk(INPIC)=IFOC
            flag(ifoc)='*'
          else
            INPIC=INPIC-1
            iconnpk(lj)=0
            flag(ljfoc)=' '
          endif
        ELSEIF(INPIC.EQ.0)THEN
          INPIC=1
          iconnpk(INPIC)=IFOC
          flag(ifoc)='*'
        ENDIF
        IF(INPIC.EQ.IALLOW)THEN

C Have picked as many as are allowed.
          RETURN
        ELSE
          IVERT=-2
          goto 3
        ENDIF
      else
        ivert=-1
        goto 92
      endif
      ivert=-4
      goto 3

      end

C ***** PROCGDST
C PROCGDST process GDST command sets (defined in common blocks
C idsta and anchora.
C << Note: this procedure is somewhat out of date. Its function
C << should be reviewed for current simulation demands
      subroutine procgdst(itrc,runmode,keepres,donegdst,ier)
#include "building.h"
#include "geometry.h"
#include "esprdbfile.h"
#include "espriou.h"  
C espriou.h for climatelist file name.
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/appw/iappw,iappx,iappy
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)

      character DESC*48
      integer NMLC,IPR,LAYERS
      real DTHK,DRAIR
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)

C IDST tool commands.
      common/idsta/CMDCFG,CMDCLM,CMDCLMN,CMDNOTE,CMDRES,CMDLBL(10),
     &             CMDLNAM(10)
      common/idstb/nidst
      common/anchora/NALOC,ALOC(20),ALOCLBL(20),ALOCTYP(20)
      common/anchorb/IALOC(20),lstanchr(20,60)
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres

      character CMDCFG*72,CMDCLM*72,CMDCLMN*72,CMDNOTE*64,CMDRES*72
      character CMDLNAM*80,CMDLBL*12
      character LCFGF*72,cfgroot*24,h*72
      character CXITM*43,outs*124,LTMP*72
      character clmdb*144
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      CHARACTER SSPARENT*12
      character doit*248,tmode*8,tfile*72,aut*8,runmode*12,keepres*4
      character sblres*72,sflres*72,splres*72,smstres*72,sipvres*72
      character selres*72,alias*32,donegdst*72
      character longtfile*144,longtfiledos*144
      character ALOC*12,ALOCLBL*12,ALOCTYP*4
      logical updatecon,QUIET,concat,unixok
      logical newgeo  ! to use for testing if new/old geometry file.

C Set flag for zone construction file update.
      updatecon=.false.
      newgeo=.false.  ! assume older format geometry.

C Check if Unix-based or DOS based.
      call isunix(unixok)

C If IDST climate is different update the configuration. Check
C against the climate list to see related db for climate name.
      iuf=IFIL+1
      write(alias,'(a)')CMDCLMN(1:lnblnk(CMDCLMN))
      call matchcdb(iuf,alias,clmdb,ier)
      if(clmdb(1:lnblnk(clmdb)).ne.LCLIM(1:lnblnk(LCLIM)))then
        if(clmdb(1:4).ne.'UNKN')then
          LCLIM=clmdb

C Debug
C          write(6,*)'using new climate db ',clmdb
          call tstamp('>','PRJ: IDST update climate in configuration')
          call tstamp('>',LCFGF)
          CALL EMKCFG('-',IER)
        endif
      endif

C Loop through anchor types and deal with known types.
      call edisp(iuout,' no. |zone|surf|type|   other side data')
      call edisp(iuout,' conn|reference|conn|      connection  ')
      do 32 m=1,nidst

C Now see if can match the IDST ext_window parameter. ALOC(m).
        imlcrepl=0
        do 33 k=1,NMLC
          if(ALOC(m)(1:12).eq.DESC(k)(1:12))then
            imlcrepl=k
          endif
  33    continue
        if(CMDLBL(m)(1:10).eq.'ext_window')then

C If replacement mlc found then loop through anchor points and update
C surface attributes by reading in the zone geometry, changing the
C variable ssmlcn and ssotr and then writing out the geometry file
C again. Also set updatecon to true so that construction files are
C updated as well.
          if(imlcrepl.ne.0)then
            do 34 i=1,nidst
              if(ALOCLBL(i)(1:10).eq.'ext_window')then
                call edisp(iuout,' for type ext_window...')
                do 243 j=1,IALOC(i)
                  ifoc=lstanchr(i,j)
                  call CONXMENU(ifoc,CXITM)
                  call usrmsg(' Processing...',CXITM,'-')
                  iz=ic1(ifoc)
                  is=ie1(ifoc)
                  icc=izstocn(iz,is)
                  LTMP=LGEOM(IZ)
                  call eclose(gversion(IZ),1.1,0.01,newgeo)
                  if(newgeo)then
                    call georead(IUF,LTMP,IZ,1,iuout,IER)
                  else
                    call egomin(IUF,LTMP,IZ,1,0,iuout,IER)
                  endif
                  WRITE(SSMLCN(icc),'(A12)')DESC(imlcrepl)(1:12)
                 if(DESC(imlcrepl)(15:18).NE.'OPAQ')SSOTF(icc)='TRAN'
                 if(DESC(imlcrepl)(15:18).EQ.'OPAQ')SSOTF(icc)='OPAQUE'
                 if(DESC(imlcrepl)(15:18).EQ.'CFC ')SSOTF(icc)='CFC '
                  updatecon=.true.
                  call eclose(gversion(IZ),1.1,0.01,newgeo)
                  if(igupgrade.eq.2.and.(.NOT.newgeo))then
                    gversion(iz) =1.1
                    newgeo = .true.
                  endif
                  if(newgeo)then
                    call geowrite(IUF,LTMP,IZ,iuout,3,IER)
                  else
                    call emkgeo(IUF,LTMP,IZ,3,IER)
                  endif
 243            continue
              endif
  34        continue
          endif
        elseif(CMDLBL(m)(1:10).eq.'int_window')then

C If replacement mlc found then loop through anchor points and update
C surface attributes. Similar logic to the 'ext_window' case above.
          if(imlcrepl.ne.0)then
            do 44 i=1,nidst
              if(ALOCLBL(i)(1:10).eq.'int_window')then
                call edisp(iuout,' of type int_window...')
                do 244 j=1,IALOC(i)
                  ifoc=lstanchr(i,j)
                  call CONXMENU(ifoc,CXITM)
                  call usrmsg(' Processing...',CXITM,'-')
                  iz=ic1(ifoc)
                  is=ie1(ifoc)
                  icc=izstocn(iz,is)
                  LTMP=LGEOM(IZ)
                  call eclose(gversion(IZ),1.1,0.01,newgeo)
                  if(newgeo)then
                    call georead(IUF,LTMP,IZ,1,iuout,IER)
                  else
                    CALL EGOMIN(IUF,LTMP,IZ,1,0,iuout,IER)
                  endif
                  WRITE(SSMLCN(icc),'(A12)')DESC(imlcrepl)(1:12)
                 if(DESC(imlcrepl)(15:18).NE.'OPAQ')SSOTF(icc)='TRAN'
                 if(DESC(imlcrepl)(15:18).EQ.'OPAQ')SSOTF(icc)='OPAQUE'
                 if(DESC(imlcrepl)(15:18).EQ.'CFC ')SSOTF(icc)='CFC '
                  updatecon=.true.
                  call eclose(gversion(IZ),1.1,0.01,newgeo)
                  if(igupgrade.eq.2.and.(.NOT.newgeo))then
                    gversion(iz) =1.1
                    newgeo = .true.
                  endif
                  if(newgeo)then
                    call geowrite(IUF,LTMP,IZ,iuout,3,IER)
                  else
                    call emkgeo(IUF,LTMP,IZ,3,IER)
                  endif
 244            continue
              endif
  44        continue
          endif
        elseif(CMDLBL(m)(1:10).eq.'atr_window')then

C If replacement mlc found then loop through anchor points and update
C surface attributes. Similar logic to the 'ext_window' case above.
          if(imlcrepl.ne.0)then
            do 45 i=1,nidst
              if(ALOCLBL(i)(1:10).eq.'atr_window')then
                call edisp(iuout,' of type atr_window...')
                do 245 j=1,IALOC(i)
                  ifoc=lstanchr(i,j)
                  call CONXMENU(ifoc,CXITM)
                  call usrmsg(' Processing...',CXITM,'-')
                  iz=ic1(ifoc)
                  is=ie1(ifoc)
                  icc=izstocn(iz,is)
                  LTMP=LGEOM(IZ)
                  call eclose(gversion(IZ),1.1,0.01,newgeo)
                  if(newgeo)then
                    call georead(IUF,LTMP,IZ,1,iuout,IER)
                  else
                    call egomin(IUF,LTMP,IZ,1,0,iuout,IER)
                  endif
                  WRITE(SSMLCN(icc),'(A12)')DESC(imlcrepl)(1:12)
                 if(DESC(imlcrepl)(15:18).NE.'OPAQ')SSOTF(icc)='TRAN'
                 if(DESC(imlcrepl)(15:18).EQ.'OPAQ')SSOTF(icc)='OPAQUE'
                 if(DESC(imlcrepl)(15:18).EQ.'CFC ')SSOTF(icc)='CFC '
                  updatecon=.true.
                  call eclose(gversion(IZ),1.1,0.01,newgeo)
                  if(igupgrade.eq.2.and.(.NOT.newgeo))then
                    gversion(iz) =1.1
                    newgeo = .true.
                  endif
                  if(newgeo)then
                    call geowrite(IUF,LTMP,IZ,iuout,3,IER)
                  else
                    call emkgeo(IUF,LTMP,IZ,3,IER)
                  endif
 245            continue
              endif
  45        continue
          endif
        endif
  32  continue

C If attributes changed then zone construction files should be
C re-generated via call to edcon.
      if(updatecon)then
        call tstamp('>','PRJ: IDST update zone constructions')
        QUIET=.TRUE.
        DO 2491, IZ=1,NCOMP
          CALL EDCON(0,iuout,IZ,QUIET,IER)
 2491   CONTINUE
        QUIET=.FALSE.
      endif

C Run the appropriate simulations.

C << this is similar to code within prj.F subroutine simula >>
C << consider combining these code blocks  >>

      h(1)='Interactive mode uses simulation parameters as '
      h(2)='defaults. The silent running mode uses the'
      h(3)='parameters to run in automatic mode. '
      if(runmode(1:11).eq.'interactive')then
        CALL EASKAB('Simulation interaction options: (see help)',
     &    ' ','run interactively','silent running mode',isauto,3)
      else
        isauto=2
      endif
      call tstamp('>','PRJ: beginning IPV simulations.')
      doit = ' '
      call tchild(ICPMOD)
      call termode(ICPMOD,tmode)
      if(isauto.eq.1)aut=' default'
      if(isauto.eq.2)then
        aut=' silent '
        tmode='text'
      endif
      if(unixok)then
        call addpath(LCFGF,longtfile,concat)
      else

C If running on a non-unix machine see if there are spaces in the name
C and change any / to \.
        call addpath(LCFGF,longtfile,concat)
        call cmdfiledos(longtfile,longtfiledos,ier)
        longtfile=' '
        longtfile=longtfiledos
      endif

C If prj initial size is a % of default pass this on to child with
C an offset from prj start position.
      if(iappw.gt.0.and.iappw.le.200)then
        write(doit,'(3a,3i4,5a)') 'bps -mode ',tmode,
     &    ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &    longtfile(:lnblnk(longtfile)),' -p win',aut
      else
        write(doit,'(8a)') 'bps -mode ',tmode,' -s 0 0 0 -file ',
     &   longtfile(1:lnblnk(longtfile)),' -p win',aut
      endif
      call usrmsg('starting winter assessment via',doit,'-')
      call runit(doit,tmode)
      call edisp(iuout,' ')
      call edisp(iuout,'Winter season run complete.')
      write(outs,'(a,a)')'IPV intermediate results file should be:',
     &  sipvres(1:lnblnk(sipvres))
      call edisp(iuout,outs)

      if(iappw.gt.0.and.iappw.le.200)then
        write(doit,'(3a,3i4,4a)') 'res -mode ',tmode,
     &    ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &    sblres(1)(1:lnblnk(sblres(1))),' -act ipv_win ',aut
      else
        write(doit,'(6a)') 'res -mode ',tmode,
     &   ' -s 0 0 0 -file ',sblres(1)(1:lnblnk(sblres(1))),
     &   ' -act ipv_win ',aut
      endif
      call usrmsg('starting winter recovery via',doit,'-')
      call runit(doit,tmode)
      call edisp(iuout,'Winter season results complete.')

      if(iappw.gt.0.and.iappw.le.200)then
        write(doit,'(3a,3i4,5a)') 'bps -mode ',tmode,
     &    ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &    longtfile(1:lnblnk(longtfile)),' -p trn',aut
      else
        write(doit,'(8a)') 'bps -mode ',tmode,' -s 0 0 0 -file ',
     &    longtfile(1:lnblnk(longtfile)),' -p trn',aut
      endif
      call usrmsg('starting transition assessment via',doit,'-')
      call runit(doit,tmode)
      call edisp(iuout,'Transition season run complete.')

      write(outs,'(a,a)')'IPV intermediate results file should be:',
     &  sipvres(1:lnblnk(sipvres))
      call edisp(iuout,outs)
      if(iappw.gt.0.and.iappw.le.200)then
        write(doit,'(3a,3i4,4a)') 'res -mode ',tmode,
     &    ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &    sblres(2)(1:lnblnk(sblres(2))),' -act ipv_trn ',aut
      else
        write(doit,'(6a)') 'res -mode ',tmode,
     &   ' -s 0 0 0 -file ',sblres(2)(1:lnblnk(sblres(2))),
     &   ' -act ipv_trn ',aut
      endif
      call usrmsg('starting transition recovery via',doit,'-')
      call runit(doit,tmode)
      call edisp(iuout,'Transition season results complete.')

      if(iappw.gt.0.and.iappw.le.200)then
        write(doit,'(3a,3i4,5a)') 'bps -mode ',tmode,
     &    ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &    longtfile(1:lnblnk(longtfile)),' -p sum',aut
      else
        write(doit,'(8a)') 'bps -mode ',tmode,' -s 0 0 0 -file ',
     &   longtfile(1:lnblnk(longtfile)),' -p sum',aut
      endif
      call usrmsg('starting summer assessment via',doit,'-')
      call runit(doit,tmode)
      call edisp(iuout,'Summer season run complete.')

      write(outs,'(a,a)')'IPV intermediate results file should be:',
     &  sipvres(1:lnblnk(sipvres))
      call edisp(iuout,outs)
      if(iappw.gt.0.and.iappw.le.200)then
        write(doit,'(3a,3i4,4a)') 'res -mode ',tmode,
     &    ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &    sblres(3)(1:lnblnk(sblres(3))),' -act ipv_sum ',aut
      else
        write(doit,'(6a)') 'res -mode ',tmode,
     &   ' -s 0 0 0 -file ',sblres(3)(1:lnblnk(sblres(3))),
     &   ' -act ipv_sum ',aut
      endif
      call edisp(iuout,doit)
      call runit(doit,tmode)
      call edisp(iuout,'Summer season results complete.')

      call edisp(iuout,'Annual IPV recovery starting...')
      if(iappw.gt.0.and.iappw.le.200)then
        write(doit,'(3a,3i4,4a)') 'res -mode ',tmode,
     &    ' -s ',iappw,iappx+15,iappy+50,' -file ',
     &    sblres(3)(1:lnblnk(sblres(3))),' -act ipv_ann ',aut
      else
        write(doit,'(6a)') 'res -mode ',tmode,
     &   ' -s 0 0 0 -file ',sblres(3)(1:lnblnk(sblres(3))),
     &   ' -act ipv_ann ',aut
      endif
      call edisp(iuout,doit)
      call runit(doit,tmode)
      call edisp(iuout,'Summer season results complete.')

C Remove the esp-r results files if requested.
C << only do this for the actual number of sets >>
      if(keepres(1:2).eq.'no')then
        idf=ifil+1
        nzrl = MZRL
        CALL ERPFREE(idf,ISTAT)
        call EFOPRAN(idf,sblres(1),nzrl,1,IER)
        CALL EFDELET(idf,ISTAT)
        CALL ERPFREE(idf,ISTAT)
        call EFOPRAN(idf,sblres(2),nzrl,1,IER)
        CALL EFDELET(idf,ISTAT)
        CALL ERPFREE(idf,ISTAT)
        call EFOPRAN(idf,sblres(3),nzrl,1,IER)
        CALL EFDELET(idf,ISTAT)
        call edisp(iuout,'ESP-r results libraries cleaned up.')
      endif

      call edisp(iuout,'GDST COMMANDS FINISHED.')
      return

 3    CALL ERPFREE(idf,ISTAT)
      return

      end

C ******** serchrpl
C Serchrpl handles search and replace of composition attributes.
      subroutine serchrpl(act,itrc,ier)
#include "building.h"
#include "geometry.h"
      
      integer lnblnk  ! function definition

      COMMON/FILEP/IFIL
      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/RAY3/MODIFY,MODLEN,MODBND
      
      integer ncomp,ncon
      common/C1/NCOMP,NCON
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      COMMON/C24/IZSTOCN(MCOM,MS)

      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON,3),SSPARENT(MCON),SSUSE(MCON,2)

      character DESC*48
      integer NMLC,IPR,LAYERS
      real DTHK,DRAIR
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)
      common/anchora/NALOC,ALOC(20),ALOCLBL(20),ALOCTYP(20)
      common/anchorb/IALOC(20),lstanchr(20,60)

      logical OK,DOK,MODIFY,MODLEN,MODBND,QUIET,apply
      logical newgeo  ! to use for testing if new/old geometry file.

      dimension IVALS(MCOM)
C     dimension SNA(MS),PAZI(MS),PELV(MS)

      character LTMP*72
      character H*72
      CHARACTER SSMLCN*32,SSVFC*4,SSOTF*32,SSOTHER*24,SSNAME*12,SSUSE*8
      character ZN*12,outs*124,SN*12,SSPARENT*12
      character DESCRC*25,act*1
      character ALOC*12,ALOCLBL*12,ALOCTYP*4
      character AL1*12,AL2*12,AL3*12,AL4*12,AL5*12,AL6*12,AL7*12
      character AL8*12,AL9*12,CXITM*43

      iuf=IFIL+1
      newgeo=.false.  ! assume older format geometry.
      if(act.eq.'c')then
        H(1)='Often it is convenient to search a model for one  '
        H(2)='constructional attribute and replace this by'
        H(3)='another. You can do this by searching for occurances'
        H(4)='of a particular construction or by selecting an anchor'
        H(5)='point (with an associated list of surfaces) to apply'
        H(6)='the new attribute to.'
        H(7)=' '
        H(8)='In the first method, choose the construction to search'
        H(9)='for and then its replacement (you will be asked to '
        H(10)='confirm each replacement'
        H(11)=' '
        H(12)='In the 2nd method, choose an anchor point and then the'
        H(13)='substutute construction. '
        H(14)=' '
        H(15)='NOTE: After using the search & replace facility '
        H(16)='zone construction files should be rebuilt for the'
        H(17)='new attributes to take effect. '
        CALL PHELPD('global attributions',17,'-',0,0,IER)
        if(NALOC.gt.0)then
          CALL EASKABC(' Surfaces to update:',' ',
     &      'which match a construction','linked with an anchor',
     &      'cancel',ins9,17)
        else
          ins9=1
        endif
        if(ins9.eq.1)then

C Search and replace by finding matching constructions.
          CALL EPKMLC(ISEL,'Select a construction to SEARCH for',
     &      'or -Exit',IER)
          if(ISEL.eq.0)return
          CALL EPKMLC(ISELR,'Select a construction to REPLACE it',
     &      'with or -Exit',IER)
          if(ISELR.eq.0)return

          H(1)=' Pick one, several or all zones to update.'
          INPIC=NCOMP
          CALL EPICKS(INPIC,IVALS,' ',' Search zones:',
     &      12,NCOMP,zname,' zone list',IER,1)
          IF(INPIC.EQ.0)return
          call tstamp('>','PRJ: global search & replace')
          DO 96 IZ=1,INPIC
            IF(IVALS(IZ).GT.0)THEN
              write(zn,'(A)') zname(IVALS(IZ))
              write(outs,'(3a)') ' Updating: ',zn(1:lnblnk(zn)),
     &           ' attributes...'
              CALL USRMSG(' ',outs,'-')
              LTMP=LGEOM(IVALS(IZ))
              call eclose(gversion(IVALS(IZ)),1.1,0.01,newgeo)
              if(newgeo)then
                call georead(IUF,LTMP,IVALS(IZ),1,iuout,IER)
              else
                call egomin(IUF,LTMP,IVALS(IZ),1,0,iuout,IER)
              endif

              WRITE(outs,93)
              call edisp(iuout,outs)
              WRITE(outs,94)
              call edisp(iuout,outs)
              do 91 i=1,nsur
                icc=izstocn(ivals(iz),i)
                if(SSMLCN(icc)(1:12).eq.DESC(ISEL)(1:12))then
                  write(sn,'(a)') SSNAME(icc)
                  WRITE(outs,'(a7,I3,1X,F7.2,F5.0,F5.0,1X,A12,1X,A4,
     &              1X,A4,1X,A12,1X,A13)')'Match: ',I,SSNA(icc),
     &              SSPAZI(icc),SSPELV(icc),SN,SSOTF(icc)(1:4),
     &              SSVFC(icc),SSMLCN(icc)(1:12),SSOTHER(icc,1)(1:13)
                  call edisp(iuout,outs)
                  write(outs,'(5a)') ' Apply construction to ',
     &              zn(1:lnblnk(zn)),':',sn(1:lnblnk(sn)),'?'
                  dok=.true.
                  h(1)='Just in case you want to be selective...'
                  CALL ASKOK(' ',outs,OK,dok,1)
                  IF(OK)then
                    ioc=IZSTOCN(IVALS(IZ),i)
                    WRITE(SSMLCN(ioc),'(A12)')DESC(ISELR)(1:12)

C << If newgeometry then can write out the optical property instead
C << of TRAN for SSOTF.
                    if(DESC(ISELR)(15:18).NE.'OPAQ')SSOTF(ioc)='TRAN'
                    if(DESC(ISELR)(15:18).EQ.'OPAQ')SSOTF(ioc)='OPAQUE'
                    if(DESC(ISELR)(15:18).EQ.'CFC ')SSOTF(ioc)='CFC '
                  endif
                endif
 91           continue
              call eclose(gversion(IVALS(IZ)),1.1,0.01,newgeo)
              if(igupgrade.eq.2.and.(.NOT.newgeo))then
                gversion(IVALS(IZ)) =1.1
                newgeo = .true.
              endif
              if(newgeo)then
                call geowrite(IUF,LTMP,IVALS(IZ),iuout,3,IER)
              else
                call emkgeo(IUF,LTMP,IVALS(IZ),3,IER)
              endif
              QUIET=.TRUE.
              write(outs,'(3a)') ' Updating: ',zn(1:lnblnk(zn)),
     &           ' constructions...'
              CALL USRMSG(' ',outs,'P')
              call tstamp('>','PRJ: auto update zone construction')
              CALL EDCON(0,ITRU,IVALS(IZ),QUIET,IER)
              QUIET=.FALSE.
              write(outs,'(3a)') ' Updating: ',zn(1:lnblnk(zn)),
     &        '... done.'
              CALL USRMSG(' ',outs,'-')
            endif
  96      continue
          MODIFY=.TRUE.
          MODBND=.TRUE.
        elseif(ins9.eq.2)then

C Anchor point substitution.
          AL1=ALOCLBL(1)
          AL2=ALOCLBL(2)
          AL3=ALOCLBL(3)
          AL4=ALOCLBL(4)
          AL5=ALOCLBL(5)
          h(1)='Indicate which anchor to use. '
          CALL easkatog(' Which anchor:',' ',AL1,AL2,AL3,AL4,AL5,
     &      'other anchors','continue',ltype,1)
          if(ltype.eq.7)then
            call usrmsg('No anchor selected.',' ','W')
            return
          elseif(ltype.eq.6)then
            AL6=ALOCLBL(6)
            AL7=ALOCLBL(7)
            AL8=ALOCLBL(8)
            AL9=ALOCLBL(9)
            CALL easkatog(' Which anchor:',' ',AL6,AL7,AL8,AL9,
     &        'continue',' ',' ',ltype,1)
            if(ltype.gt.4)then
              call usrmsg('No anchor selected.',' ','W')
              return
            else
              ltype=ltype+5
            endif
          endif
          CALL EPKMLC(ISELR,'Select a construction to use with this',
     &      'surface list or -Exit',IER)
          if(ISELR.eq.0)return
          dok=.true.
          h(1)='A chance to abort if incorrect construction selected.'
          CALL ASKOK('Apply this construction?',' ',OK,dok,1)
          IF(.NOT.OK)return
          do 2243 j=1,IALOC(ltype)
            ifoc=lstanchr(ltype,j)
            call CONXMENU(ifoc,CXITM)
            call usrmsg(' Processing...',CXITM,'-')
            iz=ic1(ifoc)
            is=ie1(ifoc)
            icc=izstocn(iz,is)
            LTMP=LGEOM(IZ)
            call eclose(gversion(IZ),1.1,0.01,newgeo)
            if(newgeo)then
              call georead(IUF,LTMP,IZ,1,iuout,IER)
            else
              call egomin(IUF,LTMP,IZ,1,0,iuout,IER)
            endif
            WRITE(SSMLCN(icc),'(A12)')DESC(ISELR)(1:12)

C << If newgeometry then can write out the optical property instead
C << of TRAN for SSOTF.
            IF(DESC(ISELR)(15:18).NE.'OPAQ')SSOTF(icc)='TRAN'
            IF(DESC(ISELR)(15:18).EQ.'OPAQ')SSOTF(icc)='OPAQUE'
            IF(DESC(ISELR)(15:18).EQ.'CFC ')SSOTF(icc)='CFC '
            call eclose(gversion(IZ),1.1,0.01,newgeo)
            if(igupgrade.eq.2.and.(.NOT.newgeo))then
              gversion(iz) =1.1
              newgeo = .true.
            endif
            if(newgeo)then
              call geowrite(IUF,LTMP,IZ,iuout,3,IER)
            else
              call emkgeo(IUF,LTMP,IZ,3,IER)
            endif
 2243     continue
          QUIET=.FALSE.
          dok=.true.
          h(1)='A search & replace only changes the surface attribute'
          h(2)='it does not update the raw thermophysical data held in'
          h(3)='the zone construction files. It is recommended that '
          h(4)='you also update the zone files. '
          CALL ASKOK(
     &      'All construction & related files should be updated...',
     &      'Do this now (recommended - see help) ?',QUIET,dok,4)
          if(.NOT.QUIET)return
          call tstamp('>','PRJ: auto update zone construction')
          DO 2492,IZ=1,NCOMP
            CALL EDCON(0,ITRU,IZ,QUIET,IER)
 2492     CONTINUE
          QUIET=.FALSE.
        endif
      elseif(act.eq.'a')then
        H(1)='Often it is convenient to supply surface construct-'
        H(2)='ional attribution by globally applying a material to'
        H(3)='a subset of surfaces and then attend to exceptions. '
        H(4)=' '
        H(5)='The following dialogue will allow you to define a '
        H(6)='subset of surfaces via filters for position,  '
        H(7)='orientation, opacity, etc. '
        H(8)=' '
        H(9)='NOTE: After using the global attribution facility '
        H(10)='you must then rebuild the zone construction files'
        H(11)='for the new attributes to take effect. '
        CALL PHELPD('global attributions',11,'-',0,0,IER)

        CALL EPKMLC(ISEL,'Select construction for matching surfaces',
     &    'or -Exit',IER)
        if(ISEL.eq.0)return

        H(1)='You may restrict updating to only opaque surfaces,'
        H(2)='transparent surfaces or apply to both. '
        CALL EASKABCD(' First filter--surface opacity:',' ',
     &    'opaque','transparent','both','cancel',ins1,2)
        if(ins1.eq.4)return

        H(1)='You may restrict updating to only surfaces which '
        H(2)='face the outside, partitions (internal) etc. '
        call EASKATOG(' Second filter--surface contiguity:',' ',
     &      'external','similar','constant','partitions','ground',
     &      'adiabatic','cancel',ins2,2)
        if(ins2.eq.7)then
          return
        endif

        H(1)='You may restrict updating to only surfaces which '
        H(2)='have a particular orientation. '
        call EASKATOG(' Third filter--surface orientation:',' ',
     &      'vertical','sloped','horizontal up','horizontal down',
     &      'any horizontal','any orien.','cancel',ins3,2)
        if(ins3.eq.7)then
          return
        endif

        H(1)='You may restrict update to avoid already attributed'
        H(2)='surfaces. '
        CALL EASKABCD(' Surfaces to update:',' ',
     &    'those already attributed','those marked UNKNOWN','both',
     &    'cancel',ins4,2)
        if(ins4.eq.4)then
          return
        endif

        H(1)=' Pick one, several or all zones to update.'
        INPIC=NCOMP
        CALL EPICKS(INPIC,IVALS,' ',' Which zones to update: ',
     &      12,NCOMP,zname,' zone list',IER,1)
        IF(INPIC.EQ.0)return
        call tstamp('>','PRJ: global construction attribution')
        DO 95 IZ=1,INPIC
          IF(IVALS(IZ).GT.0)THEN
            write(zn,'(A)') zname(IVALS(IZ))
            write(outs,'(3a)') ' Updating: ',zn(1:lnblnk(zn)),
     &         '...'
            CALL USRMSG(' ',outs,'-')
            LTMP=LGEOM(IVALS(IZ))
            call eclose(gversion(IVALS(IZ)),1.1,0.01,newgeo)
            if(newgeo)then
              call georead(IUF,LTMP,IVALS(IZ),1,iuout,IER)
            else
              call egomin(IUF,LTMP,IVALS(IZ),1,0,iuout,IER)
            endif

            WRITE(outs,93)
   93     FORMAT('           Sur|  Area  |Azim|Elev| surface    |',
     &        'geometry| multilayer  |environment')
            call edisp(iuout,outs)
            WRITE(outs,94)
   94     FORMAT('              |  m^2   |deg |deg | name       |',
     &        'type|loc| constr name |other side ')
            call edisp(iuout,outs)
            do 11 i=1,nsur
              icc=izstocn(ivals(iz),i)
              apply=.true.
              if(ins1.eq.1.and.SSOTF(icc)(1:4).ne.'OPAQ'.and.
     &            SSOTF(icc)(1:4).ne.'CFC ')apply=.false.
              if(ins1.eq.2.and.SSOTF(icc)(1:4).eq.'OPAQ')apply=.false.

C Determine the match in the system topology. 
              CALL SURADJ(IVALS(IZ),I,IE,TMP,IZC,ISC,IC,DESCRC)
              if(ins2.eq.1.and.IE.ne.0)then
                apply=.false.
              elseif(ins2.eq.2.and.IE.ne.1)then
                apply=.false.
              elseif(ins2.eq.3.and.IE.ne.2)then
                apply=.false.
              elseif(ins2.eq.4.and.IE.ne.3)then
                apply=.false.
              elseif(ins2.eq.5.and.IE.ne.4)then
                apply=.false.
              elseif(ins2.eq.6.and.IE.ne.5)then
                apply=.false.
              endif

C Orientation check.
C 'vertical','sloped','horizontal up','horizontal down',
C 'any horizontal','any orien.','cancel'.
              if(ins3.eq.1.and.SSVFC(icc)(1:4).ne.'VERT')then
                apply=.false.
              elseif(ins3.eq.2.and.SSVFC(icc)(1:4).ne.'SLOP')then
                apply=.false.
              elseif(ins3.eq.3.and.SSVFC(icc)(1:4).ne.'CEIL')then
                apply=.false.
              elseif(ins3.eq.4.and.SSVFC(icc)(1:4).ne.'FLOR')then
                apply=.false.
              elseif((ins3.eq.5.and.SSVFC(icc)(1:4).eq.'FLOR').or.
     &               (ins3.eq.5.and.SSVFC(icc)(1:4).eq.'CEIL'))then
                continue
              elseif(ins3.eq.6)then
                continue
              endif

              if(ins4.eq.2.and.SSMLCN(icc)(1:4).ne.'UNKN')apply=.false.

              if(apply)then
                write(sn,'(a)') SSNAME(icc)
                WRITE(outs,'(a7,I3,1X,F7.2,F5.0,F5.0,1X,A12,1X,A4,
     &            1X,A4,1X,A12,1X,A13)')'Match: ',I,SSNA(icc),
     &            SSPAZI(icc),SSPELV(icc),SN,SSOTF(icc)(1:4),
     &            SSVFC(icc),SSMLCN(icc)(1:12),SSOTHER(icc,1)(1:13)
                call edisp(iuout,outs)
                write(outs,'(5a)') ' Apply construction to ',
     &            zn(1:lnblnk(zn)),':',sn(1:lnblnk(sn)),'?'
                dok=.true.
                h(1)='The criter you set were matched. And you can'
                h(2)='still opt not to apply it in all cases. '
                CALL ASKOK(' ',outs,OK,dok,2)
                IF(OK)then
                  ioc=IZSTOCN(IVALS(IZ),i)
                  WRITE(SSMLCN(ioc),'(A12)')DESC(ISEL)(1:12)
                  IF(DESC(ISEL)(15:18).NE.'OPAQ')SSOTF(ioc)='TRAN'
                  IF(DESC(ISEL)(15:18).EQ.'OPAQ')SSOTF(ioc)='OPAQ'
                  IF(DESC(ISEL)(15:18).EQ.'CFC ')SSOTF(ioc)='CFC '
                endif
              endif
 11         continue
            call eclose(gversion(IVALS(IZ)),1.1,0.01,newgeo)
            if(igupgrade.eq.2.and.(.NOT.newgeo))then
              gversion(IVALS(IZ)) =1.1
              newgeo = .true.
            endif
            if(newgeo)then
              call geowrite(IUF,LTMP,IVALS(IZ),ITRU,3,IER)
            else
              call emkgeo(IUF,LTMP,IVALS(IZ),3,IER)
            endif
            QUIET=.TRUE.
            write(outs,'(3a)') ' Updating: ',zn(1:lnblnk(zn)),
     &         'constructions...'
            CALL USRMSG(' ',outs,'P')
            call tstamp('>','PRJ: auto update zone construction')
            CALL EDCON(0,ITRU,IVALS(IZ),QUIET,IER)
            QUIET=.FALSE.
            write(outs,'(3a)') ' Updating: ',zn(1:lnblnk(zn)),
     &        '... done.'
            CALL USRMSG(' ',outs,'-')
          endif
  95    continue
        MODIFY=.TRUE.
        MODBND=.TRUE.
      else
        call usrmsg('Unknown action...returning. ',' ','P')
      endif
      return

      end
