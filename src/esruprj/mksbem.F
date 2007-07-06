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

C You should have received a copy of the GNU General Public
C License along with ESP-r. If not, write to the Free
C Software Foundation, Inc., 59 Temple Place, Suite 330,
C Boston, MA 02111-1307 USA.

C This file includes:
C  EMKSBeM creates a model variant based on the SBEM methodology. 
C  ENLARGE performs geometric enlargement (contraction) of a surface.


C ********************* EMKSBeM
C EMKSBeM creates a model variant based on the SBEM methodology. Details 
C of this can be found in relevant EU and UK building regulations. 
C This subroutine copies the cfg, cnn and mandatory zones files and
C makes a new model representative of the notional building. 
C Construction and Materials databases are also copied.
C Changes to the model are then made, geometry and construction files
C are changed and so are the constructions and materials databases. 
C All networks (plant, airflow, contaminants etc) are removed from the
C model.
      SUBROUTINE EMKSBM
#include "building.h"

      common/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)

C Common block for UK calculation method.
      integer isbem  ! a value of 1 signals that isbem data exists
      integer ibtyp  ! this matches the index value in NCM guide table 4
      integer ibusertyp ! this matches the list of buildings in isbem.
      real rooffr  ! roof glazing fraction required
      real wallfr  ! wall glazing fraction required
      COMMON/SBEM/isbem,ibtyp,ibusertyp,rooffr,wallfr
      COMMON/C1/NCOMP,NCON

C << G1 is probably not the common block to use for this >>
      COMMON/G1/X(MTV),Y(MTV),Z(MTV),NSUR,JVN(MS,MV),NVER(MS),NTV

      common/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
      COMMON/C3/IC1(MCON),IE1(MCON),ICT(MCON),IC2(MCON),IE2(MCON)
      common/C3F/LCNN
      COMMON/C20/NZSUR(MCOM),NZTV(MCOM)
      common/C21/IFCFG,cfgroot,LCFGF
      COMMON/C24/IZSTOCN(MCOM,MS)
      COMMON/INDICS/IVF(MCOM),ISI(MCOM),IHC(MCOM),
     &              ITW(MCOM),ICGC(MCOM),IOBS(MCOM)
      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      COMMON/G5/SNAME(MCOM,MS),SOTF(MS),SMLCN(MS),SVFC(MS),SOTHER(MS)
      COMMON/G6/SSNAME(MCON),SSOTF(MCON),SSMLCN(MCON),SSVFC(MCON),
     &          SSOTHER(MCON),SSPARENT(MCON)
      COMMON/G7/SSNA(MCON),SSPAZI(MCON),SSPELV(MCON),SSPERIM(MCON),
     &          SSUREQN(MCON,4),SSURCOG(MCON,3),SSURVN(MCON,3)
      common/FILEP/IFIL
      common/spfldat/nsset,isset,isstup,isbnstep,ispnstep,issave,isavgh
      common/spflres/sblres(MSPS),sflres(MSPS),splres(MSPS),
     &  smstres(MSPS),selres(MSPS),sipvres

      CHARACTER SNAME*12,SMLCN*12,SVFC*4,SOTF*4,SOTHER*15
      CHARACTER ACT*1
      CHARACTER APE*4,EXT*4,SSOTF*4,SSVFC*4
      CHARACTER SSNAME*12,SSMLCN*12,SSPARENT*12
      CHARACTER SSOTHER*15
      CHARACTER CFGROOT*24
      CHARACTER LCFGF*72,LCNN*72,NNAME*72,H*72
      CHARACTER*72 LSNAM,LPROJ,LGEOM,LSHAD,LTHRM,LUTIL
      CHARACTER*72 LVIEW,LHCCO,LTWIN,LCGCIN,ZOBS
      character*72 sblres,sflres,splres,smstres,sipvres,selres
      CHARACTER OUTS*124
      character doit*248

C ivalsv keeps track of new viewfactor files which need analysys
C ivalsg keeps track of whether a geometry file has alread been copied.
      dimension ivalsv(MCOM),ivalsg(MCOM)

      DIMENSION IACT(MCON),COE(3),POINT(3),TRANS(3)
      logical unixok,docnn

C Setup Notional Model Flag
      ISBEM=1
      APE='_not'

C << Assumptions code below can be removed as soon as the common
C << block variables isbem,ibtyp,ibusertyp,rooffr,wallfr are
C << held in the configuration file.

      H(1) = '****************** IMPORTANT *****************'
      H(2) = ' '
      H(3) = 'Make sure that the original model follows the following '
      H(4) = 'assumptions before generating the notional model '
      H(5) = '1. All external glazings are contained within other'
      H(6) = '   surfaces. '
      H(7) = '2. Except for doors no opaque surfaces are contained'
      H(8) = '   within other surfaces. e.g. it is not allowed to '
      H(9) = '   contain a surface meant to be an opaque wall within '
      H(10)= '   another wall.'
      H(11)= ' '
      H(12)= ' '
      CALL PHELPD('SBEM Assumptions',12,'-',0,0,IER)

C Find out what type of building it is and set up flag
      H(1) = 'Windows, doors and rooflight areas are modified in the '
      H(2) = 'notional building depending upon building type. There'
      H(3) = 'are three types of building: '
      H(4) = '1. Residential (where people temporarily or permanently'
      H(5) = '   reside)'
      H(6) = '2. Assembly (places of assembly, offices and shops)'
      H(7) = '3. Industrial (industrial or storage buildings)'
      CALL EASKABCD('What type of building is it? (see help)',' ',
     &  'residential','assembly',
     &  'industrial','cancel',IBTYP,7)
      IF(IBTYP.EQ.4)THEN
        RETURN

C Establish glazing fractions based on type of building
      ELSEIF(IBTYP.EQ.1)THEN
        ROOFFR=0.2
        WALLFR=0.3
      ELSEIF(IBTYP.EQ.2)THEN
        ROOFFR=0.2
        WALLFR=0.4
      ELSEIF(IBTYP.EQ.3)THEN
        ROOFFR=0.2
        WALLFR=0.15
      ENDIF
    
C The inital task is to make copies of the relevant zone files
C which is functionally equivalent to the code blocks in subroutine
C verman for the case of geometry and attribution (ILM=2).
      call isunix(unixok)

C Assume that a new connections file needs to be created.
      docnn=.true.

C Change name of configuration file to filename_not.cfg
      EXT='.cfg'
      CALL FNCNGR(LCFGF,APE,EXT,NNAME)
      LCFGF=NNAME
  
C Loop through each zone in the model...
      DO 550 ICOMP=1,NCOMP
        IUF=IFIL+2
        EXT='.geo'
        CALL EGOMIN(IUF,LGEOM(ICOMP),ICOMP,1,0,iuout,IER)
        CALL FNCNGR(LGEOM(ICOMP),APE,EXT,NNAME)
        LGEOM(ICOMP)=NNAME
        CALL EMKGEO(IUF,LGEOM(ICOMP),ICOMP,ITRU,3,IER)

C Establish parents of surfaces in this zone (fill common blocks G6 G7)
        ACT='C'
        CALL SURREL(ACT,ICOMP,IER)

C Remember this geometry file has been done so that a later selection
C of constructions does not re-do it.
        ivalsg(icomp)=icomp

C If there is an existing shading file copy it to new name
C so that subsequent changes are applied to the new file.
        if(ISI(icomp).eq.1)then
          EXT='.shd'
          CALL FNCNGR(LSHAD(ICOMP),APE,EXT,NNAME)
          if(unixok)then
            WRITE(doit,'(4A)')'cp ',
     &        LSHAD(ICOMP)(1:LNBLNK(LSHAD(ICOMP))),' ',
     &        NNAME(1:LNBLNK(NNAME))
          else
            WRITE(doit,'(4A)')'copy /y ',
     &        LSHAD(ICOMP)(1:LNBLNK(LSHAD(ICOMP))),' ',
     &        NNAME(1:LNBLNK(NNAME))
          endif
          CALL USRMSG('copying shading file via:',doit,'-')
          CALL RUNIT(doit,'-')
          LSHAD(ICOMP)=NNAME
        endif

C If there is an existing viewfactor file copy it to new name
C so that subsequent changes are applied to the new file.
        if(IVF(icomp).eq.1)then
          EXT='.vwf'
          CALL FNCNGR(LVIEW(ICOMP),APE,EXT,NNAME)
          if(unixok)then
            WRITE(doit,'(4A)')'cp ',
     &        LVIEW(ICOMP)(1:LNBLNK(LVIEW(ICOMP))),' ',
     &        NNAME(1:LNBLNK(NNAME))
          else
            WRITE(doit,'(4A)')'copy /y ',
     &        LVIEW(ICOMP)(1:LNBLNK(LVIEW(ICOMP))),' ',
     &        NNAME(1:LNBLNK(NNAME))
          endif
          CALL USRMSG('copying viewfactor file via:',doit,'-')
          CALL RUNIT(doit,'-')
          LVIEW(ICOMP)=NNAME

C ? after copy it is probably not necessary to re-run the viewfactor analysis.
C          ivalsv(ic)=ivals(IC)
        endif

C If there is a heat transfer regime copy the exiting file
C so that subsequent changes are applied to the new file.
        if(IHC(icomp).eq.1)then
          EXT='.htc'
          CALL FNCNGR(LHCCO(ICOMP),APE,EXT,NNAME)
          if(unixok)then
            WRITE(doit,'(4A)')'cp ',
     &        LHCCO(ICOMP)(1:LNBLNK(LHCCO(ICOMP))),' ',
     &        NNAME(1:LNBLNK(NNAME))
          else
            WRITE(doit,'(4A)')'copy /y ',
     &        LHCCO(ICOMP)(1:LNBLNK(LHCCO(ICOMP))),' ',
     &        NNAME(1:LNBLNK(NNAME))
          endif
          CALL USRMSG('copying hc regime file via:',doit,'-')
          CALL RUNIT(doit,'-')
          LHCCO(ICOMP)=NNAME
        endif

C If there is a casual gain control file copy the exiting file
C so that subsequent changes are applied to the new file.
        if(ICGC(icomp).eq.1)then
          EXT='.cgc'
          CALL FNCNGR(LHCCO(ICOMP),APE,EXT,NNAME)
          if(unixok)then
            WRITE(doit,'(4A)')'cp ',
     &        LCGCIN(ICOMP)(1:LNBLNK(LCGCIN(ICOMP))),' ',
     &        NNAME(1:LNBLNK(NNAME))
          else
            WRITE(doit,'(4A)')'copy /y ',
     &        LCGCIN(ICOMP)(1:LNBLNK(LCGCIN(ICOMP))),' ',
     &        NNAME(1:LNBLNK(NNAME))
          endif
          CALL USRMSG('copying casual gain ctl file via:',doit,'-')
          CALL RUNIT(doit,'-')
          LCGCIN(ICOMP)=NNAME
        endif

C If there is an obstruction file copy the exiting file
C so that subsequent changes are applied to the new file.
        if(IOBS(icomp).eq.1)then
          EXT='.obs'
          CALL FNCNGR(ZOBS(ICOMP),APE,EXT,NNAME)
          if(unixok)then
            WRITE(doit,'(4A)')'cp ',
     &        ZOBS(ICOMP)(1:LNBLNK(ZOBS(ICOMP))),' ',
     &        NNAME(1:LNBLNK(NNAME))
          else
            WRITE(doit,'(4A)')'copy /y ',
     &        ZOBS(ICOMP)(1:LNBLNK(ZOBS(ICOMP))),' ',
     &        NNAME(1:LNBLNK(NNAME))
          endif
          CALL USRMSG('copying obstruction file via:',doit,'-')
          CALL RUNIT(doit,'-')
          ZOBS(ICOMP)=NNAME
        endif
 550  CONTINUE
      call edisp(iuout,
     &   'New zone files available for applying nominal rules.')

C Remind the user that other files have been copied.
      ishdq=-1
      ivfwq=-1
      do 43 iz=1,ncomp
        if(ISI(iz).eq.1)ishdq=1
        if(IVF(iz).eq.1)ivfwq=1
  43  continue
      if(ishdq.eq.1)then
        call usrmsg(
     &    'shading files found which will be dependant on future',
     &    'geometry changes. Shading files copied.','W')
        ishd=1
      endif
      if(ivfwq.eq.1)then
        call usrmsg(
     &    'viewfactor files found which will be dependant on future',
     &    'geometry changes. Viewfactor files copied.','W')
      endif

C At this point prepare to update the configuration and connections
C file for the nominal mode. 
C But first change names of results libraries zones, flow, plant,
C moisture, electrical and IPV
      DO 123 ISPS=1,NSSET
        EXT='.res'
        CALL FNCNGR(SBLRES(ISPS),APE,EXT,NNAME)
        if(NNAME(1:4).ne.'not_')SBLRES(ISPS)=NNAME
        EXT='.mfr'
        CALL FNCNGR(SFLRES(ISPS),APE,EXT,NNAME)
        if(NNAME(1:4).ne.'not_')SFLRES(ISPS)=NNAME
        EXT='.plr'
        CALL FNCNGR(SPLRES(ISPS),APE,EXT,NNAME)
        if(NNAME(1:4).ne.'not_')SPLRES(ISPS)=NNAME
        EXT='.msr'
        CALL FNCNGR(SMSTRES(ISPS),APE,EXT,NNAME)
        if(NNAME(1:4).ne.'not_')SMSTRES(ISPS)=NNAME
        EXT='.res'
        CALL FNCNGR(SELRES(ISPS),APE,EXT,NNAME)
        if(NNAME(1:4).ne.'not_')SELRES(ISPS)=NNAME
 123  CONTINUE
      EXT='.rep'
      CALL FNCNGR(SIPVRES,APE,EXT,NNAME)
      SIPVRES=NNAME
      LRCR=2

C Check length of cfgroot string.
      IROOTLEN=LNBLNK(CFGROOT)
      IF(IROOTLEN.GT.(24-3))THEN
        IROOTLEN=21
      ENDIF
      write(cfgroot,'(2a)')cfgroot(1:irootlen),ape

C If a variant connections file also required set this up.
      if(docnn)then
        EXT='.cnn'
        CALL FNCNGR(LCNN,APE,EXT,NNAME)
        LCNN=NNAME
      endif
      CALL EMKCFG('-',IER)
      WRITE (OUTS,'(2A)')'Writing new configuration file ',LCFGF
      CALL EDISP(IUOUT,OUTS)


C << the following logic does not work so is skipped for the current time >>

C Work out areas of all child surfaces (AREA) and transparent
C surfaces (TAREA) and orientation of child and parent (IACT)

C Loop through each of the connections in the model.

C << go a global replace of isur with icc because we are working
C << with a loop connection by connection.
C      DO 551 ISUR=1,NCON

C Need to figure out if we happen to have just the correct amount
C of glazing in a particular exernal wall.
C << >>
C        NCHILD=0
C        NTCHILD=0
C        AREA=0.
C        TAREA=0.
C        IF(SSOTHER(ISUR)(1:8).EQ.'EXTERIOR'.AND.
C     &     SSOTF(ISUR).NE.'TRAN')THEN
C          DO 552 JSUR=1,NCON
C            IF(SSNAME(ISUR).EQ.SSPARENT(JSUR))THEN
C              NCHILD=NCHILD+1
C              AREA=SSNA(JSUR)+AREA
C              IF(SSOTF(JSUR).EQ.'TRAN')THEN
C                NTCHILD=NTCHILD+1
C                TAREA=SSNA(JSUR)+TAREA
C              ENDIF
C            ENDIF
C 552      CONTINUE

C IACT is action flag that means:
C 0 = Exterior surface has requisite amount of glazing, do nothing
C 1 = Exterior surface has no child surfaces and needs requisite amount
C     of glazing inserted
C 2 = Exterior surface has one or more child surfaces (all transparent)
C     but more glazed area needs to be added
C 3 = Exterior surface has one or more child surfaces (all opaque) and
C     needs requisite amount of glazing
C 4 = Exterior surface has mix of opaque and transparent children and
C     needs requisite amount of glazing
C          IACT(ISUR)=0
C          IF(NCHILD.EQ.0)THEN
C            IACT(ISUR)=1
C          ELSEIF(NCHILD.EQ.NTCHILD)THEN
C            IACT(ISUR)=2
C          ELSEIF(NTCHILD.EQ.0)THEN
C            IACT(ISUR)=3
C          ELSE
C            IACT(ISUR)=4
C          ENDIF

C          ICOMP=IC1(ISUR)  ! the zone associated with this connection.

C Find the number of surfaces in this zone.
C          NSUR=NZSUR(icomp)

C And we might need to re-scan geometry file.

C          CALL ESCZONE(ICOMP)   ! ?? why the G1 common blocks are not fresh.

C Modify geometry files to have exterior walls/roofs with prescribed 
C amount of glazing and doors. If the area of glazing is less than 
C prescribed make balance up with windows and rooflights.
C          KOUNTER=IACT(ISUR)
C          IF(KOUNTER.EQ.0)THEN
C            CONTINUE
C          ELSEIF(KOUNTER.EQ.1)THEN

C Enlarge/Diminish the glazed surface with vertex weighted centre of 
c gravity of the parent surface as centre of enlargement.
C            DO 560 I=1,3
C              COE(I)=SSURCOG(ISUR,I)
C 560        CONTINUE
C            IF(SSVFC(ISUR).EQ.'VERT')THEN
C              FACTOR=SQRT(WALLFR)
C            ELSEIF(SSVFC(ISUR).EQ.'SLOP'.OR.SSVFC(ISUR).EQ.'CEIL')THEN
C              FACTOR=SQRT(ROOFFR)
C            ENDIF

C << NVER and JVN are not the correct variable to use unless the zone
C << geometry has been refreshed. Probably need to use the 
C << same logic as in esrucfg.
C            DO 561 NV=1,NVER(ISUR)
C              POINT(1)=X(JVN(ISUR,NV))
C              POINT(2)=Y(JVN(ISUR,NV))
C              POINT(3)=Z(JVN(ISUR,NV))
C              CALL ENLARGE(POINT,TRANS,COE,FACTOR)

C Add new vertex to vertex list
C              X(JVN(NSUR+1,NV))=TRANS(1)
C              Y(JVN(NSUR+1,NV))=TRANS(2)
C              Z(JVN(NSUR+1,NV))=TRANS(3)
C 561        CONTINUE

C Make new surface based on these vertices
C << the array size for nver is not the same as for SSVFC
C            NSUR=NSUR+1
C            NVER(NSUR)=NVER(ISUR)
C            SSVFC(NSUR)=SSVFC(ISUR)
C            SSOTF(NSUR)='TRAN'
C            SSOTHER(NSUR)='EXTERIOR'

C << probably want I2.2 as the format statement but what
C << happens with more than 99 surfaces? >>
C            WRITE(SSNAME(NSUR),'(I2,A)')ISUR,'_notional'
C            IZSTOCN(ICOMP,NSUR)=NSUR
C            SNAME(ICOMP,NSUR)=SSNAME(NSUR)
C          ELSEIF(KOUNTER.EQ.2)THEN
C          ELSEIF(KOUNTER.EQ.3)THEN
C          ELSEIF(KOUNTER.EQ.4)THEN
C          ENDIF
C        ENDIF
C 551  CONTINUE

C After having made the changes, save the nominal model configuration file.
C      CALL EMKCFG('-',IER)
C      WRITE (OUTS,'(2A)')'Writing new configuration file ',LCFGF
C      CALL EDISP(IUOUT,OUTS)

C << now what is in memory and in the files is the nominal model
C << do we leave it at that or return to the original model?

      RETURN
      END

************************* ENLARGE *****************************
C Subroutine ENLARGE performs geometric enlargement (contraction) of a
C surface and for a given vertex (POINT) it returns a transformed
C vertex (TRANS) based on a scale factor (factor) and a centre of
C enlargement (coe). TRANS will be somewhere along the line between
C COE and POINT. It is expected to be called once for each vertex
C of the surface who's size is to be adjusted.

C Factor can be a fraction (in which case it is a contraction of a
C geometric surface or may be greater than one in which case it is an
C enlargement.
      SUBROUTINE ENLARGE(POINT,TRANS,COE,FACTOR)
      DIMENSION POINT(3),COE(3),TRANS(3)
      DO 1 I=1,3
        TRANS(I)=(ABS(1.-FACTOR))*COE(I)+FACTOR*POINT(I)
 1    CONTINUE
      RETURN
      END

