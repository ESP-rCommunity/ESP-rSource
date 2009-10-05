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

C The subroutines contained within this file are concerned with the 
C embedded generation or advanced materials (i.e. phase change) side 
C of the building model.
 
C ************* EDSPLIST 
C Edit special components for the current project. The routine will
C Selectively list embedded renewable components or Advanced materials
C according to the calling mode: 
C LM - list advanced materials
C LG - list renewable components
C EM - edit/add advanced materials
C EG - edit/add renewables
C IER=0 OK, IER=1 problem.

      SUBROUTINE EDSPLIST(MODE,IVAL,IER)
#include "epara.h"
#include "building.h"

      COMMON/OUTIN/IUOUT,IUIN
      common/filep/ifil

      common/pophelp/h(60)

      common/spmfxst/ispmxist,spflnam 
      common/spmopn/ispopen 

      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      common/spmatlbl/spmlabel(mspmnod)

      DIMENSION VERT(35), ITKEY(MSPMNOD)
      CHARACTER VERT*36,KEY*1,H*72,spmlabel*16
      character*72 spflnam

      CHARACTER deflnam*72,lfil*72
      CHARACTER*2 MODE,MOD
      LOGICAL ISPOPEN, XST,NEW,OK,OKK,DOK
      OK=.FALSE.
      OKK=.FALSE.
      IFOC=0
      IVAL=0
      ispmfil=ifil+1
      NEW=.TRUE.

      if(.not.ispopen)then
        dok=.false.
        h(1)='If you know of the name of an existing file click on'
        h(2)='ok. If you want to create one say no. '
        CALL ASKOK('Does a special components file exist?',' ',OK,dok,2)
        IF(OK)THEN
          CONTINUE
        ELSE
          dok=.false.
          h(1)='If you have the data needed to create this portion of'
          h(2)='the model say ok. '
          CALL ASKOK('Would you like to create a new special',
     &'components file?',OKK,dok,2)
          IF(OKK)THEN
            CONTINUE
          ELSE
            RETURN
          ENDIF
        ENDIF
C Open or create a special materials/components file.
        deflnam='data.spm'
        lfil=spflnam
        h(1)='This file contains information about special components'
        h(2)='integrated into the building fabric. Examples include:'
        h(3)='facade integrated photovoltaics, ducted wind turbines,'
        h(4)='phase change materials, thermochromic glazing, etc.'
        
        call easks(lfil,' ',' Special components filename?',72,
     &    deflnam,' SPM filename',IER,4)
        if(lfil(1:2).ne.'  ')spflnam=lfil
        XST=.false.
        call FINDFIL(spflnam,XST)
        if(XST)then
          call erpfree(ispmfil,istat)
          call spminit
          ispmxist=1
          ispopen=.true.
        else
          ispopen=.false.
        endif
      endif

C Initialise special component menu size variables based on window size. 
C IVERT is the menu position, MVERT the current number of menu lines.
      ILEN=nspmnod
      IPACT=CREATE
      CALL EKPAGE(IPACT)
  92  MHEAD=2
      MCTL=5
      ILEN=nspmnod

C Initial menu entry setup.
      IER=0
      ILEN=nspmnod
      IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
    3 M=MHEAD

C Clear temporary storage for special material node numbers.
      DO 5 I=1,MSPMNOD
        ITKEY(I)=0
  5   CONTINUE

      NSPMTYP1=0
C Display active material components.
      IF(MODE.EQ.'EM'.OR.MODE.EQ.'LM')THEN
        DO 10 L=1,ILEN
          IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
            IF(ISPMTYP(L,1).GT.50)THEN
c              WRITE(*,*) 'type ', ISPMTYP(L,1) 
              M=M+1
              NSPMTYP1=NSPMTYP1+1
              ITKEY(NSPMTYP1)=L
c              WRITE(*,*) NSPMTYP1,ITKEY(NSPMTYP1)
              CALL EMKEY(L,KEY,IER)
              WRITE(VERT(M),14)KEY,L,spmlabel(L),ispmloc(L,1),
     &        ispmloc(L,2),ispmloc(L,3)
   14         FORMAT(A1,I3,1X,A16,3I4)
            ENDIF
          ENDIF
   10   CONTINUE
      ENDIF
       
      NSPMTYP2=0

C Display embedded renewable components.
      IF(MODE.EQ.'EG'.OR.MODE.EQ.'LG')THEN
        DO 9 L=1,ILEN
          IF(L.GE.IST.AND.(L.LE.(IST+MIFULL)))THEN
            IF(ISPMTYP(L,1).LE.50)THEN  
c              WRITE(*,*) 'type ', ISPMTYP(L,1)         
              M=M+1
              NSPMTYP2=NSPMTYP2+1
              ITKEY(NSPMTYP2)=L
c              WRITE(*,*) NSPMTYP2,ITKEY(NSPMTYP2)
              CALL EMKEY(L,KEY,IER)
              WRITE(VERT(M),13)KEY,L,spmlabel(L),ispmloc(L,1),
     &          ispmloc(L,2),ispmloc(L,3)
   13         FORMAT(A1,I3,1X,A16,3I4)
            ENDIF
          ENDIF
   9   CONTINUE
      ENDIF

      IF(MODE.EQ.'EG'.OR.MODE.EQ.'LG')THEN
        WRITE(VERT(1),'(A,I2,A)')
     &' No. of embedded renewables .(',NSPMTYP2,')'
      ELSE
        WRITE(VERT(1),'(A,I2,A)')
     &' No. of advanced materials ..(',NSPMTYP1,')'
      ENDIF
      VERT(2)=' Item|description  |zone|surf|node'

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN  
        VERT(M+1)='  ________________________________  '
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT   ('0 page: ',I2,' of ',I2,' ---------')
      ENDIF
      IF(MODE.NE.'LM'.AND.MODE.NE.'LG') THEN
        VERT(M+2)  ='+ add/delete item                   '
        VERT(M+3)  ='> update file                       '
        VERT(M+4)  ='? help                              '
        VERT(M+5)  ='- exit                              '
      ELSE
        VERT(M+2)  ='                                    '
        VERT(M+3)  ='                                    '
        VERT(M+4)  ='? help                              '
        VERT(M+5)  ='- exit                              '
      ENDIF

C Help text for this menu.
      H(1)='This menu controls the definition of special'
      H(2)='components in the model. Special component properties '
      H(3)='supports features such as integrated photovoltaics or'
      H(4)='and phase change materials. '
      H(5)=' '
      H(6)='Each entry is given a name and includes positional '
      H(7)='information to link it to a surface/layer within'
      H(8)='the building model.  NOTE that changes to zones such '
      H(9)='as adding/deleting surfaces or changing composition '
      H(10)='is not automatically updated. '

C Now display the menu.
      IF(MODE.EQ.'LM'.OR.MODE.EQ.'EM')THEN
        CALL EMENU(' Advanced materials',VERT,MVERT,IVERT) 
      ELSE     
        CALL EMENU(' Embedded renewables',VERT,MVERT,IVERT)
      ENDIF

      IF(IVERT.LE.MHEAD) THEN

C Within the header so skip request.
        IVERT=-1
        GOTO 3
      ELSEIF(IVERT.EQ.MVERT)THEN
        RETURN
      ELSEIF(IVERT.EQ.(MVERT-1))THEN

C List help text for the vertex menu.
        CALL PHELPD('special component edit',10,'-',0,0,IER)
      ELSEIF(IVERT.EQ.(MVERT-2).AND.MODE(1:1).NE.'L')THEN
        ispmxist = 1
        call spmwrit
        ispmxist = 1
      ELSEIF(IVERT.EQ.(MVERT-3).AND.MODE(1:1).NE.'L')THEN

C Alter list by deleting/ editing.
        write(H(1),'(A,I2,A)')' Up to ',mspmnod-nspmnod,
     &                             ' items may be added. '
        H(2)=' '
        CALL EASKABCD('Options:',' ','delete component',
     &    'add component','edit component','continue ?',IRT,2)
        if(IRT.eq.1)then
          
C Delete the chosen special component.
          CALL EMENU(' Delete which component',VERT,MVERT,ID)
          CALL KEYIND(MVERT,ID,IFOC,IO)
          ISPD=ITKEY(IFOC)
c          write(*,*) 'selected ',IFOC, 'deleting ',ISPD
          IF(ISPD.LT.NSPMNOD) THEN
            DO 20 ISPDEL=ISPD,NSPMNOD-1
              ISPMTYP(ISPDEL,1)=ISPMTYP(ISPDEL+1,1)
              ISPMTYP(ISPDEL,2)=ISPMTYP(ISPDEL+2,2)
              NNODAT(ISPDEL)=NNODAT(ISPDEL+1)
              SPMLABEL(ISPDEL)=SPMLABEL(ISPDEL+1)
              DO 25 JSPDEL=1,3
               ISPMLOC(ISPDEL,JSPDEL)=ISPMLOC(ISPDEL+1,JSPDEL)
  25          CONTINUE
              DO 27 KSPEDEL=1,NNODAT(ISPDEL)
                SPMDAT(ISPDEL,KSPEDEL)=SPMDAT(ISPDEL+1,KSPEDEL)
  27          CONTINUE
  20        CONTINUE
          ENDIF
          NSPMNOD=NSPMNOD-1
          ILEN=nspmnod
          IPACT=CREATE
          CALL EKPAGE(IPACT)

C Redisplay the menu
          GOTO 92

        elseif(IRT.eq.2)then
          MOD=MODE
          NEW=.TRUE.
          ISPA=NSPMNOD+1
          CALL EDSPM(ISPA,MOD,ITRU)
          GOTO 92
        elseif(IRT.eq.3)then
          MOD=MODE
          NEW=.FALSE.
          CALL EMENU(' Edit which component',VERT,MVERT,ID)
          CALL KEYIND(MVERT,ID,IFOC,IO)
          ISPE=ITKEY(IFOC)
          CALL EDSPM(ISPE,MOD,ITRU) 
          GOTO 92       
        elseif( IRT.eq.4)then
          goto 92        
        endif

      ELSEIF(IVERT.EQ.(MVERT-4))THEN

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
        ENDIF
      ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1))THEN
          IF(MODE.EQ.'EM'.OR.MODE.EQ.'EG')THEN
            MOD=MODE
            NEW=.FALSE.
            dok=.true.
            h(1)='Last chance to avoid editing the component. '
            CALL ASKOK('Edit this component?',' ',OK,dok,1)
            IF(OK)THEN
              CALL KEYIND(MVERT,IVERT,IFOC,IO)
              ISPE=ITKEY(IFOC)
              CALL EDSPM(ISPE,MOD,ITRU)  
            ELSE  
              GOTO 3
            ENDIF
          ELSE
C Return the value of the selected component
            CALL KEYIND(MVERT,IVERT,IFOC,IO)
            IVAL=ITKEY(IFOC)
            RETURN
          ENDIF
      IVERT=-2
      goto 92

      ENDIF

C Redisplay the menu 
      GOTO 92

      END 

C ********* EDSPM 
C Subroutine EDSPM allows the user to enter details about special
C components associated with a particular surface and node.
C Information entered includes:
C Special material type
C Location (construction layer)
C Number of miscellaneous data items
C Data items.

      subroutine EDSPM(ISPM,MODE,ITRU)
#include "net_flow.h"
#include "building.h"
#include "geometry.h"
#include "power.h"
#include "epara.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LAPRES,IAPRES (for window pressure databases)
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
      
      integer lnblnk  ! function definition

      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/FILEP/IFIL

      common/spmatl/nspmnod,ispmloc(mspmnod,3),ispmtyp(mspmnod,2),
     &nnodat(mspmnod),spmdat(mspmnod,mspmdat) 
      common/spmatlbl/spmlabel(mspmnod)
      COMMON/MFLWPR/NPRE,FPRE(MPOS,MPRD)

      character DESC*48
      integer NMLC,IPR,LAYERS
      real DTHK,DRAIR
      COMMON/MLC/NMLC,DESC(MMLC),DTHK(MMLC,ME),IPR(MMLC,ME),
     &           LAYERS(MMLC),DRAIR(MMLC,ME,3)

C << rewrite logic using G6 common block >>

      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS,3),
     &          SUSE(MS,2),SPARENT(MS)
      integer nccode,indutl
      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72
      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/RAY2/ITDSP,ITBND,ITEPT,ITZNM,ITSNM,ITVNO,ITORG,ITSNR,
     &            ITOBS,ITHLS,ITHLZ,ITGRD,GRDIS,ITPPSW
      COMMON/RAY3/MODIFY,MODLEN,MODBND
      common/MFLDOC/DEPRE(MPRD) 
      COMMON/RAY6/LINSTY(MCON)
      COMMON/FOPENED/CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)

C Special materials file flag
      common/spmfxst/ispmxist,spflnam
      common/entfile/entflnam, ientxist

C Arrays of material properties.
      real matver
      integer matcats,matcatitems,matdbitems
      character matdbdate*24,matdbdoc*248,matcatname*32,matcatdoc*248
      common/matheader/matver,matcats,matdbitems,matcatitems(MGCL),
     &  matdbdate,matdbdoc,matcatname(MGCL),matcatdoc(MGCL)

      real matdbcon,matdbden,matdbsht,matdboute,matdbine
      real matdbouta,matdbina,matdbdrv,matdbthick
      common/matdatarray/matlegindex(MGIT),matcatindex(MGIT),
     &  matdbcon(MGIT),matdbden(MGIT),matdbsht(MGIT),matdboute(MGIT),
     &  matdbine(MGIT),matdbouta(MGIT),matdbina(MGIT),matdbdrv(MGIT),
     &  matdbthick(MGIT)

      integer mathash
      common/mathasharray/mathash(0:600)

      character matname*32,matdoc*248,matopaq*1
      common/matnamearray/matname(MGIT),matdoc(MGIT),matopaq(MGIT)

      integer legmat  ! local variable for legacy index.
      integer matarrayindex ! the indes within matdatarray
      logical closemat

C << rewrite logic to use G7 commons instead of these local variables. >>

      DIMENSION SNA(MS),PAZI(MS),PELV(MS)
      DIMENSION PNAM(ME),item(ME+10),spitem(40),spitem2(40),
     &dadesc(40), SPMISC(MPWDAT),SPVAR(MPWDAT),SPSTR(90),SSTR(90)
C Note: spdesc is passed to subroutine MCDBSCAN in leu of sstr;
C therefore spdesc must have the same dimensions as dummy arguement
C sstr in spdesc.
      dimension spdesc(90)

      logical ok,dok, SPREPT
      LOGICAL CFGOK,MLDBOK,MATDBOK,CTLOK,OPTKOK
      LOGICAL MODIFY,MODLEN,MODBND
      logical newgeo  ! to use for testing if new/old geometry file.
      integer header,footer
      character outs*124,spmlabel*16,item*32,spitem*32,entflnam*72
      CHARACTER NAM*72,PNAM*20,spdefnam*16,labl*16,H*72
      CHARACTER TITL*72,DADESC*72,spitem2*32
      CHARACTER SOTHER*24,SNAME*12,SMLCN*32,SVFC*4,SOTF*32,SUSE*8
      CHARACTER SPARENT*12
      CHARACTER spflnam*72,spdesc*72
      CHARACTER SPSTR*72,SSTR*72, MODE*2, T72*72, t144*144
      CHARACTER PCITM(35)*42,DEPRE*40,KEY*1,DESCRC*25

C Set the special components reporting variable
      SPREPT=.FALSE.
      newgeo=.false.  ! assume older format geometry.

C Set the number of special maperials
      ispmn=ispm

C Check whether a file is in existence.
      if(ispmxist.EQ.0) ispmxist=1

C Ask the user to select from the available categories of special components
C scanned from the database if none found go direct to questions.
      SSTR(1)=' ' 
  8   CALL MCDBSCAN(1,1,SSTR,SPVAR,SPMISC,NIT,IER)
      IF(IER.GT.0) GOTO 99

C Show only the categories which are appropriate - either materials
C or embedded renewables (categories 1-50).
      IF(MODE.EQ.'LM'.OR.MODE.EQ.'EM')THEN
        NIT=NIT-4
        IMS=5
        IME=NIT+4
      ELSE
        NIT=4
        IMS=1
        IME=4
      ENDIF
      J=1
      DO 19 I=IMS,IME
        SPSTR(J)=SSTR(I)
        WRITE(SPITEM(J),'(I2,1X,A)') J,SPSTR(J)(1:28)
        J=J+1
  19  CONTINUE
      spitem(NIT+1)=' * User defined          '
      spitem(NIT+2)='-------------------------'
      spitem(NIT+3)=' ? help'
      spitem(NIT+4)=' - exit this menu'
      nspitm=NIT+4
      imout=-1

C Help text for this menu.
  9   h(1)='The menu options relate to different categories'
      h(2)='of special components which may be selected for'
      h(3)='integration into the building. Select a category'

      call emenu('Available Components',spitem,nspitm,imout)
      if(IMOUT.GT.0.AND.IMOUT.LE.NIT) THEN 
        
C Store the number of the selected special materials category 
      IF(MODE.EQ.'LM'.OR.MODE.EQ.'EM')THEN
        NIT=IMOUT+4
      ELSE
        NIT=IMOUT
      ENDIF     
      elseif(imout.eq.nspitm-1) then
        call phelpd('Special component',3,'-',0,0,IER)
        goto 9
      elseif(imout.eq.nspitm-3) then
        goto 99
      elseif(imout.eq.nspitm) then
        return
      else
        imout=-1
        goto 9
      endif

C Scan the database and display a menu of the types which fall into the category
C Variable NIT is overloaded and returns the number of menu items. 
 11   CALL MCDBSCAN(1,2,SSTR,SPVAR,SPMISC,NIT,IER)
      DO 12 J=1,NIT
        WRITE(SPITEM2(J),'(I2,1X,A)')J,SSTR(J)(1:28)
 12   CONTINUE
      IF(IER.GT.0) GOTO 8
 14   IF(NIT.GT.0) THEN
        SPITEM2(NIT+1)='------------------------'
        IF(SPREPT) THEN
          SPITEM2(NIT+2)='>> component info ON  '
        ELSE
          SPITEM2(NIT+2)='>> component info OFF '
        ENDIF
        SPITEM2(NIT+3)='? help'
        SPITEM2(NIT+4)='- exit this menu '
        NSPITEM2=NIT+4
 13     IMOUT2=-1      
        H(1)='Select a special component from the list. Each material  '
        H(2)='shown is associated with a set of pre-defined parameters'
        H(3)=',these can be altered if required. '
        H(4)='  '
        H(5)='  '
        H(6)='Switching the "component info" toggle to ON lets the'
        H(7)='user view any more information associated with the   '
        H(8)='special component; this appears in the text window. '
        CALL EMENU('Components in category',SPITEM2,NSPITEM2,IMOUT2)
        
        IF(IMOUT2.EQ.NSPITEM2) THEN
          RETURN
        ELSEIF(IMOUT2.EQ.NSPITEM2-1) THEN
          CALL PHELPD('Special material',8,'-',0,0,IER)
          IMOUT=-1
          IMOUT2=-1
          GOTO 13
        ELSEIF(IMOUT2.EQ.NSPITEM2-2) THEN
          IF(SPREPT) THEN
            SPREPT=.FALSE.
          ELSE
            SPREPT=.TRUE.
          ENDIF
          GOTO 14
        ELSEIF(IMOUT2.GE.1.AND.IMOUT2.LE.NIT) THEN

C Store the name of the selected string and get the data.
          SPDESC(1)=SPITEM2(IMOUT2)(4:LNBLNK(SPITEM2(IMOUT2)))
          SPDEFNAM=SPDESC(1)(1:LNBLNK(SPDESC(1)))
          SSTR(1)=SPDESC(1)

C Recover the special components data and descriptive strings. Firstly
C Blank the descriptive strings
          CALL MCDBSCAN(1,3,SSTR,SPVAR,SPMISC,NIT,IER)
          IF(IER.GT.0) GOTO 11
          ISPMTYP(ISPMN,1)=INT(SPMISC(2))
          NNODAT(ISPMN)=INT(SPMISC(3))
          DO 25 ISPDT=1,NNODAT(ISPMN) 
            SPDESC(ISPDT)=SSTR(ISPDT)
            SPMDAT(ISPMN,ISPDT)=SPVAR(ISPDT)
            DADESC(ISPDT)=SPDESC(ISPDT)
  25      CONTINUE
          IF(SPREPT) THEN
            SPDESC(1)=SPDEFNAM
            CALL MCDBSCAN(1,4,SPDESC(1),SPVAR,SPMISC,NIT,IER)
            IF(IER.GT.0) GOTO 11 
          ENDIF
        ELSE
          IMOUT2=-1
          GOTO 11
        ENDIF

C Set up the menu
      ELSE
        CALL EDISP(IUOUT,'No components found in this category')
        GOTO 8
      ENDIF

C Label lthe material according to type
  99  labl=spdefnam
      call easks(labl,' ',' Label for material (no spaces)?',16,
     &  spdefnam,'  spm label ', IER,0)
      spmlabel(ispmn)=labl

C Get data items for special component.
      idfspdat=1
      h(1)='Enter the required data for the special component. '
      h(2)='Either accept the default values or enter your own'
      h(3)='data.                                             '
      call phelpd('Special mat data',3,'-',0,0,IER)
      nspdat=nnodat(ispmn)
      if(ier.eq.0) then
        datsp=0.0   

C Enter the data items depending on the type of component the ducted wind
C turbine is a special case
        if(ispmtyp(ispmn,1).eq.3)then

C Skip the 1st 5 data items - these are required to hold positional data 
C for the outlet.
          do 29 idat=6,nspdat
            if(dadesc(idat)(1:4).eq.'    ') then
              write(outs,'(a,i2)') ' Enter data item: ',idat
            else
              write(outs,'(a)') dadesc(idat)
            endif
            datsp=spmdat(ispmn,idat)
            call easkr(datsp,outs,' ',1.,'-',99.,'-',1.0,'SPM data',
     &           IER,16)
            spmdat(ispmn,idat)=datsp
  29      continue
C       if(ispmtyp(ispmn,1).eq.?)then
C         <<other special cases here>>
        else     
          do 30 idat=1,nspdat
            if(dadesc(idat)(1:4).eq.'    ') then
              write(outs,'(a,i2)') ' Enter data item: ',idat
            else
              write(outs,'(a)') dadesc(idat)
            endif
            datsp=spmdat(ispmn,idat)
            call easkr(datsp,outs,' ',1.,'-',99.,'-',1.0,'SPM data',
     &           IER,16)
            spmdat(ispmn,idat)=datsp
  30      continue
        endif

C blank the temporary data description array
        DO 23 J=1,40
           DADESC(J)=' '
  23    CONTINUE
      elseif(ier.ne.0) then
        call edisp(iuout,' An error has occured reading the data.')
        call edisp(iuout,' Please re-enter.')
        goto 8
      endif

C Check the special component type and set how much positional data 
C is required i.e. zone|surface or zone|surface|node
C << to do >>
C Ask for zone and surface...<< why are next two lines commented out? >>
c      IZ=ispmloc(IVAL,1)
c      ISO=ispmloc(IVAL,2)
      IF(ISPMTYP(ISPMN,1).EQ.3)THEN
        WRITE(OUTS,'(A)') 
     &  'Select zone to in which locate the DWT inlet. '
      ELSE
        WRITE(OUTS,'(A)') 
     &  'Select zone to in which locate the special component. '
      ENDIF
      IZ=0
      CALL ASKZONE(IZ,0,OUTS,'Select zone','-','spm locate',IER)
      call usrmsg(' ',' ','-')
      if(IZ.eq.0) return
      call eclose(gversion(IZ),1.1,0.01,newgeo)
      if(newgeo)then
        call georead(IFIL+1,LGEOM(IZ),IZ,1,0,iuout,IER)
      else
        call egomin(IFIL+1,LGEOM(IZ),IZ,1,0,iuout,IER)
      endif

C << rewrite logic to use G7 common rather than call zinfo >>

      CALL ZINFO(iuout,SNA,ZOA,PAZI,PELV,ZVOL)

C General image option flags.
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
      ITPPSW=0

      MODIFY=.TRUE.
      MODBND=.TRUE.
      CALL INLNST(1)
      ITVNO=0
      nzg=1
      nznog(1)=IZ
      izgfoc=IZ
      CALL ADJVIEW(IER)
      MODIFY=.TRUE.
      MODBND=.TRUE.

      ISO=1
      CALL EPMENSV
      IF(ISPMTYP(ISPMN,1).EQ.3)THEN
        CALL EASKSUR(IZ,ISO,'-','Select surface for DWT','inlet.',IER)
      ELSE
        CALL EASKSUR(IZ,ISO,'-','Select surface for special',
     &    'component. ',IER)
      ENDIF
      CALL EPMENRC
      if(SOTF(ISO)(1:4).eq.'OPAQ'.or.SOTF(ISO)(1:4).eq.'CFC ')then
        ISPMF=1
      else
        ISPMF=0
      endif              

C Edit item identified by KEYIND. Recover the related zone.
c      CALL KEYIND(MVERT,IVERT,IFOC,IO)
c      IVAL=IFOC

      call eclose(gversion(IZ),1.1,0.01,newgeo)
      if(newgeo)then
        call georead(IFIL+1,LGEOM(IZ),IZ,1,ITRC,iuout,IER)
      else
        call egomin(IFIL+1,LGEOM(IZ),IZ,1,ITRC,iuout,IER)
      endif
      CALL SURADJ(IZ,ISO,IE,TMP,IZC,ISC,IC,DESCRC)

      MODIFY=.TRUE.
      MODBND=.TRUE.
      CALL INLNST(1)
      LINSTY(IC)=2
      ITVNO=0
      nzg=1
      nznog(1)=IZ
      izgfoc=IZ
      CALL ADJVIEW(IER)
      MODIFY=.TRUE.
      MODBND=.TRUE.
      if(SOTF(ISO)(1:4).eq.'OPAQ'.or.SOTF(ISO)(1:4).eq.'CFC ')then
        ISPMF=1
      else
        ISPMF=0
      endif

      ispmtyp(ispmn,2)=ISPMF

C Store the surface and zone information.
      ispmloc(ispmn,1)=iz
      ispmloc(ispmn,2)=iso

      IF(ISPMTYP(ISPMN,1).EQ.3)THEN

C Determine the surface type
C Create a menu showing the available database items. 
C Allow user to select one. Begin by reading current database.
C Begin by reading current database (signal by passing a
C blank string to erprcdb).
        t144='  '
        CALL ERPRCDB(t144,0,3,IER)
        if (ier.ne.0) THEN
          CALL EDISP(IUOUT,
     &      'Error opening pressure coefficients database.')
          RETURN
        endif

C Setup multi-page menu. 
        MHEAD=2
        MCTL=4
        ILEN=NPRE
        IPACT=CREATE
        CALL EKPAGE(IPACT)

C Initial menu entry setup.
 71     IER=0
        ILEN=NPRE
        IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
        PCITM(1)=' W:H  |  Exposure  |  Pitch (roof)'
        PCITM(2)='  (W:H = width to height ratio)   '
        M=MHEAD
        DO 202 IM=1,NPRE
          IF(IM.GE.IST.AND.(IM.LE.(IST+MIFULL)))THEN
            M=M+1
            CALL EMKEY(M,KEY,IER)   
            WRITE(PCITM(M),'(A1,1X,A)')KEY,DEPRE(IM)
          ENDIF
  202   CONTINUE

C Number of actual items displayed.
        NITMS=M+MCTL

C If a long list include page facility text.      
        IF(IPFLG.EQ.0)THEN
          PCITM(M+1)='  ________________________  '
        ELSE
          WRITE(PCITM(M+1),16)IPM,MPM 
 16       FORMAT   ('0 ---Page: ',I2,' of ',I2,' ---')
        ENDIF
        PCITM(M+2)=' '
        PCITM(M+3)='? help                   '
        PCITM(M+4)='- exit this menu         '
        IVERT=-1

C Help text for this menu.
        H(1)='To enable the calculation of wind-induced surface'
        H(2)='pressures (as required by the ducted wind turbine'
        H(3)='model), a database of angle dependent pressure'
        H(4)='coefficients is provided.'
        H(5)='The default ESP-r set contains wind pressure coeffi-'
        H(6)='cients for surfaces in typical positions and within '
        H(7)='several different exposure categories.'
        H(8)='They can be used (with care) for low-rise buildings.'
        H(9)='See the ESP manual for additional info.'
        h(10)=' '
        h(11)='It is also possible to calculate pressure coeffi-' 
        h(12)='cients using CpCalc (see database maintenance).'
        h(13)=' '
        h(14)='Select the pressure coefficient from this list, ' 
        h(15)='which is most appropriate for the location of the '
        h(16)='ducted wind turbine.'
        h(17)=' '
        h(18)='Notes about pressure coefficients: '
        H(19)='The first 27 data sets in the standard pressure coef'
        H(20)='database are from the Air Infiltration & Ventilation'
        H(21)='Centre Handbook called <Air Infiltration Calculation'
        H(22)='Techniques - An Applications Guide>. The 27 data '
        H(23)='sets are only applicable for buildings up to 3 '
        H(24)='storeys andcome with the usual warnings about usage'
        H(25)='of the data. '
        H(26)=' '
        H(27)='Each data set is described with the following terms:'
        H(28)=' a) facade or roof at different roof pitch angles'
        H(29)=' b) the ratio of the building length to width and'
        H(30)='    for a non square building there will be a long'
        H(31)='    and a short wall to select'
        H(32)=' c) site shielding as follows:'
        H(33)='   exposed - open countryside with no obstructions'
        H(34)='   simi-exposed - rural with some obstructions'
        H(35)='   sheltered - urban, surrounded on all sides by'
        H(36)='     obstructions of similar size.'

C Now display the menu.
        call USRMSG('Select the pressure coefficient set which',
     &    'best matches this surface. (see help) ','-') 
        CALL EMENU('Pressure Coefficient Sets:',PCITM,NITMS,IVERT)
        IF(IVERT.EQ.NITMS)THEN
          CALL USRMSG(
     &      'You must select one pressure coefficient set!',' ','W')
          GOTO 71
        ELSEIF(IVERT.EQ.NITMS-1)THEN
          CALL PHELPD('databases overview',36,'-',0,0,IER)
          GOTO 71
        ELSEIF(IVERT.EQ.NITMS-3)THEN

C If there are enough items allow paging control via EKPAGE.
          IF(IPFLG.EQ.1)THEN
            IPACT=EDIT
            CALL EKPAGE(IPACT)
          ENDIF
          GOTO 71
        ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(NITMS-MCTL+1))THEN

C Let user select a pc set.
          CALL KEYIND(NITMS,IVERT,IFOC,IO)
C            SUPNOD(IFNOD,1)=IFOC
           ISPMLOC(ispmn,3)=IFOC
        ELSE
          IVERT=-1
          GOTO 71
        ENDIF

C Ask whether choice is OK.
        dok=.true.
        h(1)='A chance to correct selection error.'
        write(t72,'(a,a)')'Use ',DEPRE(IFOC)
        CALL ASKOK(t72,'as the pressure coefficient set?',OK,dok,1)
        IF(.NOT.OK)GOTO 71

C Input angle external node.
        H(1)='This angle is used to derive the boundary pressure, '
        H(2)='which is angular dependent, from the pressure coef db'
        H(3)='and the current wind speed and direction.'

C Ask the user to select the external surface associated with the node.
C and extract the azimuth info. 
        IF(IZ.EQ.0) THEN
          VAL=SPMDAT(ISPMN,4)
        ELSE
          VAL=PAZI(ISO)
        ENDIF
        CALL EASKR(VAL,' ',
     &     'Surface azimuth angle (degrees clockwise from north): ',
     &      0.0,'F',359.0,'F',1.0,'surf azimuth angle',IER,3)
        SPMDAT(ISPMN,4)=VAL
      ENDIF

C For ducted wind turbine - select the position of the DWT outlet      
      if(ispmtyp(ispmn,1).eq.3)then
        WRITE(OUTS,'(A)') 
     &  'Select zone to in which locate the DWT outlet. '
        CALL ASKZONE(IZ,0,OUTS,'Select zone','-','spm locate',IER)
        call usrmsg(' ',' ','-')
        if(IZ.eq.0) return
        call eclose(gversion(IZ),1.1,0.01,newgeo)
        if(newgeo)then
          call georead(IFIL+1,LGEOM(IZ),IZ,1,ITRC,iuout,IER)
        else
          call egomin(IFIL+1,LGEOM(IZ),IZ,1,ITRC,iuout,IER)
        endif
        CALL ZINFO(iuout,SNA,ZOA,PAZI,PELV,ZVOL)
C General image option flags.
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
        ITPPSW=0

        MODIFY=.TRUE.
        MODBND=.TRUE.
        CALL INLNST(1)
        ITVNO=0
        nzg=1
        nznog(1)=IZ
        izgfoc=IZ
        CALL ADJVIEW(IER)
        MODIFY=.TRUE.
        MODBND=.TRUE.

        SPMDAT(ISPMN,1)=float(IZ)
  
        ISO=1
        CALL EPMENSV
        CALL EASKSUR(IZ,ISO,'-','Select surface for DWT','outlet.',IER)
        CALL EPMENRC

C Recover the related zone.
        call eclose(gversion(IZ),1.1,0.01,newgeo)
        if(newgeo)then
          call georead(IFIL+1,LGEOM(IZ),IZ,1,ITRC,iuout,IER)
        else
          call egomin(IFIL+1,LGEOM(IZ),IZ,1,ITRC,iuout,IER)
        endif
        CALL SURADJ(IZ,ISO,IE,TMP,IZC,ISC,IC,DESCRC)

        MODIFY=.TRUE.
        MODBND=.TRUE.
        CALL INLNST(1)
        LINSTY(IC)=2
        ITVNO=0
        nzg=1
        nznog(1)=IZ
        izgfoc=IZ
        CALL ADJVIEW(IER)
        MODIFY=.TRUE.
        MODBND=.TRUE.
       
        SPMDAT(ISPMN,2)=ISO

C Create a menu showing the available database items. 
C Allow user to select one. Begin by reading current database.
C Begin by reading current database
C (signal by passing a blank string to erprcdb).
        t144='  '
        CALL ERPRCDB(t144,0,3,IER)
        if (ier.ne.0) THEN
          CALL EDISP(IUOUT,'Error opening pressure coefficients file.')
          RETURN
        endif

C Setup multi-page menu. 
        MHEAD=2
        MCTL=4
        ILEN=NPRE
        IPACT=CREATE
        CALL EKPAGE(IPACT)

C Initial menu entry setup.
 70     IER=0
        ILEN=NPRE
        IVERT=-3

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
        PCITM(1)=' W:H  |  Exposure  |  Pitch (roof)'
        PCITM(2)='  (W:H = width to height ratio)   '
        M=MHEAD
        DO 201 IM=1,NPRE
          IF(IM.GE.IST.AND.(IM.LE.(IST+MIFULL)))THEN
            M=M+1
            CALL EMKEY(M,KEY,IER)   
            WRITE(PCITM(M),'(A1,1X,A)')KEY,DEPRE(IM)
          ENDIF
  201   CONTINUE

C Number of actual items displayed.
        NITMS=M+MCTL

C If a long list include page facility text.      
        IF(IPFLG.EQ.0)THEN
          PCITM(M+1)='  ________________________  '
        ELSE
          WRITE(PCITM(M+1),15)IPM,MPM 
 15     FORMAT   ('0 ---Page: ',I2,' of ',I2,' ---')
        ENDIF
        PCITM(M+2)=' '
        PCITM(M+3)='? help                   '
        PCITM(M+4)='- exit this menu         '
        IVERT=-1

C Help text for this menu.
        H(1)='To enable the calculation of wind-induced surface'
        H(2)='pressures (as required by the ducted wind turbine'
        H(3)='model), a database of angle dependent pressure'
        H(4)='coefficients is provided.'
        H(5)='The default ESP-r set contains wind pressure coeffi-'
        H(6)='cients for surfaces in typical positions and within '
        H(7)='several different exposure categories.'
        H(8)='They can be used (with care) for low-rise buildings.'
        H(9)='See the ESP manual for additional info.'
        h(10)=' '
        h(11)='It is also possible to calculate pressure coeffi-' 
        h(12)='cients using CpCalc (see database maintenance).'
        h(13)=' '
        h(14)='Select the pressure coefficient from this list, ' 
        h(15)='which is most appropriate for the location of the '
        h(16)='ducted wind turbine.'

C Now display the menu.
        call USRMSG('Select the pressure coefficient set which',
     &    'best matches this surface. (see help) ','-') 
        CALL EMENU('Pressure Coefficient Sets:',PCITM,NITMS,IVERT)
        IF(IVERT.EQ.NITMS)THEN
          CALL USRMSG(
     &     'You must select one pressure coefficient set!',' ','W')
          GOTO 70
        ELSEIF(IVERT.EQ.NITMS-1)THEN
          CALL PHELPD('databases overview',16,'-',0,0,IER)
          GOTO 70
        ELSEIF(IVERT.EQ.NITMS-3)THEN

C If there are enough items allow paging control via EKPAGE.
          IF(IPFLG.EQ.1)THEN
            IPACT=EDIT
            CALL EKPAGE(IPACT)
          ENDIF
          GOTO 70
        ELSEIF(IVERT.GT.MHEAD.AND.IVERT.LT.(NITMS-MCTL+1))THEN

C Let user select a pc set.
          CALL KEYIND(NITMS,IVERT,IFOC,IO)
C            SUPNOD(IFNOD,1)=IFOC
           SPMDAT(ispmn,3)=IFOC
        ELSE
          IVERT=-1
          GOTO 70
        ENDIF

C Ask whether choice is OK.
        dok=.true.
        h(1)='A chance to correct selection made in error. '
        write(t72,'(a,a)')'Use ',DEPRE(IFOC)
        CALL ASKOK(t72,'as the pressure coefficient set?',OK,dok,1)
        IF(.NOT.OK)GOTO 70   

C Ask the user to select the external surface associated with the node.
C and extract the azimuth info. 
        IF(IZ.EQ.0)THEN
          VAL=SPMDAT(ISPMN,4)
        ELSE
          VAL=PAZI(ISO)
        ENDIF
        CALL EASKR(VAL,' ',
     &    'Surface azimuth angle (degrees clockwise from north): ',
     &     0.0,'F',359.0,'F',1.0,'surf azimuth angle',IER,3)
        SPMDAT(ISPMN,5)=VAL

C Set the nodal location to 0 for the DWT - this information is not needed.
c        ispmloc(ispmn,3)=0 
      else

C Check composite name of surface with composite name in MLC common.
        imatch=0
        do 5 i=1,nmlc
          if(SMLCN(iso)(1:12).eq.DESC(i)(1:12)) then
            imatch=i          
          endif
  5     continue
        if(imatch.eq.0) then
          call edisp(iuout,'Error: probably no MLC defined!')
          return
        endif

C Write menu displaying a list of layer names
        header=4
        footer=3
        write(item(1),'(a,a)') 'a surface name: ',SNAME(iz,iso)
        write(item(2),'(a,i2)')'b number of layers: ',LAYERS(imatch)
        write(item(3),'(a,a)') 'c MLC name: ',DESC(imatch)(1:12)
        item(4)='  ----------------------------'
        call eclose(matver,1.1,0.01,closemat)
        do 10 j=1,LAYERS(imatch)

C If an air layer include the R values in the display.
          if(closemat)then
            legmat=IPR(imatch,j)   ! which legacy index
            if(legmat.ge.0)then
              matarrayindex=mathash(legmat)  ! find matching array index
              DBCON=matdbcon(matarrayindex)
              DBDEN=matdbden(matarrayindex)
              DBSHT=matdbsht(matarrayindex)
              E=matdbine(matarrayindex)
              A=matdbina(matarrayindex)
              DRV=matdbdrv(matarrayindex)
              write(TITL,'(a)') 'placeholder'
              write(NAM,'(a)') matname(matarrayindex)(1:32)
            endif
          else
            CALL ERPCDB(IFMAT,IPR(imatch,j),ITRU,IER,DBCON,DBDEN,
     &        DBSHT,E,A,DRV,TITL,NAM)
          endif
          IF(IER.EQ.1) THEN
             call edisp(iuout,' Found an error reading PCDB!')
          ENDIF
          IF(IPR(imatch,j).EQ.0)THEN
            PNAM(j)='Air gap'
          ELSE
            PNAM(j)=NAM(1:20)
          ENDIF 
          write(item(header+j),'(a,i2,1x,a12)')' Layer: ',j,PNAM(j)
          ndisp=header+j
  10    continue
        item(ndisp+footer-2)='  ----------------------------' 
        item(ndisp+footer-1)='? help  '
        item(ndisp+footer)  ='- exit  '
        ino=-1
        nitms=ndisp+footer
        call edisp(iuout,'Select a layer to locate special component.')

C Help text for this menu.
   7    h(1)='Choose a layer at which to situate a special'
        h(2)='materials node.                                     '

        call emenu('Special Components',item,nitms,ino)
        if(ino.le.header.and.ino.gt.ndisp) ino=-1
        if(ino.gt.header.and.ino.le.ndisp) then
          if(pnam(ino-header)(1:7).eq.'Air gap') then 
            call edisp(iuout,' Cannot put a solid node here!') 
            goto 7
          endif

C Ask for the nodal location. Firstly calculate the default
          nodloc=2*(ino-header)
          h(1)='Select a node within the layer in which to position' 
          h(2)='the special component. The value given is the node in'
          h(3)='the centre of the layer.'
          h(4)='  '
          h(5)='If a PV component is chosen and the ALL the '
          h(6)='construction layers are opaque then the node should'
          h(7)='be placed at the outside surface (node no. 1).'
          H(8)=' '
          H(9)='A cancel request leaves the model unchanged.'
          nodlocd=nodloc
          call easki(nodloc,' Position component at ',
     &      ' which node (for PV material see help) ? ',
     &      nodloc-1,'W',nodloc+1,'W',nodlocd,'spm node',IERI,9)
          if(ieri.eq.-3)then
            goto 7
          endif

C Check if the node has an existing special component.
          if(nspmnod.eq.0)then
            ispmloc(ispmn,3)=nodloc
          else
            do 20  k=1,nspmnod
              if(iso.eq.ispmloc(k,2).and.iz.eq.ispmloc(k,1).and.nodloc
     &              .eq.ispmloc(k,3).and.k.ne.ispmn) then
                dok=.false.
                h(1)='This is a location that can accept another node.'
                call askok(' An existing special component exists at',
     &            ' this node. Do you want add another?',OK,dok,1)
                if(.not.ok) then
                  ino=-4
                  goto 7
                else 
                  ispmloc(ispmn,3)=nodloc
                endif
              else
                ispmloc(ispmn,3)=nodloc
              endif
              if(nspmnod+1.gt.mspmnod) then 
                call edisp(iuout,' Maximum number of nodes exceeded!')
                ino=-5
                goto 7
              endif
  20        continue
          endif
        elseif(ino.eq.ndisp+footer-1) then
          call phelpd('Special material',2,'-',0,0,IER)
          ino=-1
          goto 7
        elseif (ino.eq.ndisp+footer) then
          return
        elseif(ino.lt.0) then
          goto 7
        else
          goto 7
        endif
      endif

C Report back to user
      call edisp(iuout,' ')
      if(ispmn.gt.nspmnod) then 
        nspmnod=nspmnod+1
        write(outs,'(a,a)')'Added component: ',spmlabel(ispmn) 
        call edisp(iuout,outs)
      else
        write(outs,'(a,a)')'Edited component: ',spmlabel(ispmn) 
        call edisp(iuout,outs)
      endif

      if(ispmtyp(ispmn,1).NE.3)THEN
        write(outs,'(a,i2)')' Placed at node: ',ispmloc(ispmn,3)
        call edisp(iuout,outs)
      endif

      write(outs,'(a,i2)')' No. of special component nodes: ',nspmnod
      call edisp(iuout,outs)        
      return
      end
