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

C mocfd.F contains the following:
C MOCFD - Entry point for CFD menus.
C RCFDLIBH - read CFD library header.
C MCFDM - define the parameters for the visualization of results.
C CFDPER - sets the CFD output period.
C RDCFDAT - reads a timesteps worth of data into standard arrays.
C OPCFD - checks if the CFD library is open, and prompts user if needed.
C GETIFRM - returns IFRAME for getting CFD results using RDCFDAT.
C MCFDV - define CFD variables for text/graph reporting.

C ******************** MOCFD ********************
C MOCFD is the entry point for CFD menus.
C It opens the CFD results library and chooses a domain.
C If ACT is "M" will go to visualisation menu.
C If ACT is "V" will go to variable reporting menu.

      SUBROUTINE MOCFD(ACT)

      character*1 ACT

      COMMON/OUTIN/IUOUT,IUIN

      logical XST

C Ensure CFD results library is open.
      call OPCFD('c',XST,IER)
      if (.not.XST.or.IER.ne.0) return

C Read header data and set common block data.
      call RCFDLIBH(0,IER)
      if (IER.ne.0) return

C Display menu options.
      if (ACT.eq.'M') then
        call MCFDM
      elseif (ACT.eq.'V') then

C Currently, we hijack mass flow code to report CFD metrics.
        call MFOUTP('c')
      else
        call EDISP(iuout,'MOCFD: invalid ACT')
      endif

      return
      end

C ******************** RCFDLIBH ********************
C RCFDLIBH - read CFD library header.
C If IZONE>0, will select the domain coupled to the zone instead of
C asking which domain.

      SUBROUTINE RCFDLIBH(IZONE,IER)
#include "building.h"
#include "cfd.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      
      integer ncomp,ncon
      common/c1/ncomp,ncon
      COMMON/ICFNOD/ICFD,ICP

      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)
      COMMON/cfdfil/LCFD(MCOM),IFCFD(MCOM)
      COMMON/cfdsmper/ICDYS,ICDYF,CFTS,CFTF
      COMMON/cfdhsh/NCFDSZ,NRCFDOM(MNZ)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      common/GRDVRTS/iorg(MNZ),ixend(MNZ),iyend(MNZ),izend(MNZ),
     &  izende(MNZ)
      
C NCDOM - number of CFD domains
C ICFDZ - thermal zone associated with each CFD domain
      COMMON/cfddoms/NCDOM,ICFDZ(MNZ)

      LOGICAL CALPOL,CALLMA
      CHARACTER LCFD*72,POLNAM*12

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='RCFDLIBH'  ! set for cfiles

C Set unit number.
      IER=0
      iunit=IFIL+14

C Read record 1. If this is a version 1 or 2 results file, this will
C include the equations saved, the number of which varies depending on 
C record 4 (yuck!). So, we have to read this record with enough space to
C accomodate the maximum possible amount of things in here, and then 
C proceed based on the file version number (item 2).

      IREC=1
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) II,ICFDSV,
     &  (IEQSV(I),I=1,5+MCTM)

C Debug.
c      write(6,*)II,ICFDSV,(IEQSV(I),I=1,5+MCTM)

      if (ICFDSV.eq.1.or.ICFDSV.eq.2) then
        goto 102
      elseif (ICFDSV.eq.3) then
        goto 103
      endif

C *** VERSIONS 1 & 2 ***

C Read records 2 and 3 (active CFD domains/ simulation period).
  102 IREC=2
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) (IFCFD(I),I=1,NCOMP)

C Debug.
C      write(6,*) (IFCFD(I),I=1,NCOMP)

      IREC=3
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) ICDYS,ICDYF,CFTS,CFTF

C Debug.
C      write(6,*) IREC,ICDYS,ICDYF,CFTS,CFTF

      call CFDPER(1)

C Read number of Contaminants and their names (assume zone 1)

      ICFD=1
      IREC=4
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) NCTM(ICFD)

C Read which variables are saved
      IREC=1
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)II,II,
     &    (IEQSV(I),I=1,5+NCTM(ICFD))
      IREC=4

      DO 33 ICTM=1,NCTM(ICFD)
        IREC=IREC+1
        read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) POLNAM(ICTM,ICFD)
 33   CONTINUE
      
C Work out which thermal zone the CFD zone is conflated to.
C If more than one then ask which one to use, unless IZONE>0 tells us
C which one to use.
      ICFD=0
      NCDOM=0
      NCFDSZ=0
      do 10 IC=1,NCOMP
        if (IFCFD(IC).ne.0) then
          NCDOM=NCDOM+1
          ICFDZ(NCDOM)=IC
          if (IC.eq.IZONE) ICFD=NCDOM

C Read domain size.
          IREC=5+NCTM(ICFD)+(4*(IC-1))
          read (iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) 
     &                                           IDRSZE,NIM2,NJM2,NKM2

C Debug.
C          write(6,*) IREC,IDRSZE,NIM2,NJM2,NKM2

C NRCFDOM is the number of records for each CFD domain.
          NRCFDOM(NCDOM)=IDRSZE

C NCFDSZ is the number of records for each timestep (all CFD domains).
          NCFDSZ=NCFDSZ+IDRSZE
        endif
 10   continue

C Set up contaminants in correct CFD domain
      DO 1234 ICTM=1,NCTM(ICFD)
        POLNAM(ICTM,ICFDZ(NCDOM))=POLNAM(ICTM,ICFD)
 1234 CONTINUE
      if (NCDOM.eq.0) then
        call edisp(iuout,'No CFD zones detected')
      elseif (NCDOM.eq.1) then
        ICFD=1
      elseif (IZONE.le.0) then
 99     IER=0
        helptopic='res_cfd_which_res_set'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKI(I,' There is more than 1 CFD domain.',
     &    ' Output results for which domain? ',
     &    1,'F',NCDOM,'F',1,' CFD domain',IER,nbhelp)
        if (IER.ne.0) goto 99
        ICFD=I
      endif
      ICP=ICFDZ(ICFD)

C Read domain size of selected domain.
      IREC=5+(4*(ICP-1))+NCTM(ICFD)
      read (iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) IDRSZE,NIM2,NJM2,NKM2

C Debug.
C      write(6,*) IREC,IDRSZE,NIM2,NJM2,NKM2

C Set grid variables
      NI=NIM2+2
      NJ=NJM2+2
      NK=NKM2+2
      NIM1=NIM2+1
      NJM1=NJM2+1
      NKM1=NKM2+1

C Read domain geometry.
      IREC=IREC+1
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) (XU(I),I=1,NI)

C Debug.
C      write(6,*) IREC,(XU(I),I=1,NI)

      IREC=IREC+1
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) (YV(I),I=1,NJ)

C Debug.
C      write(6,*) IREC,(YV(I),I=1,NJ)

      IREC=IREC+1
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) (ZW(I),I=1,NK)

C Debug.
C      write(6,*) IREC,(ZW(I),I=1,NK)

      goto 998

C *** VERSION 3 ***

  103 IREC=2
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) (IFCFD(I),I=1,NCOMP)

C Debug.
c      write(6,*)(IFCFD(I),I=1,NCOMP)

      IREC=3
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) ICDYS,ICDYF,CFTS,CFTF

C Debug.
c      write(6,*)ICDYS,ICDYF,CFTS,CFTF

      call CFDPER(1)

C Work out CFD domain to zone couplings from zone to CFD domain 
C couplings.
C While we're here, populate variables for the hash table by reading
C domain size for each domain.
      NCDOM=0
      ICFD=0
      NCFDSZ=0
      do IC=1,NCOMP
        if (IFCFD(IC).ne.0) then
          NCDOM=NCDOM+1
          ICFD=IFCFD(IC)
          ICFDZ(ICFD)=IC

          IREC=3+(8*(ICFD-1))+4
          read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &      IDRSZE,NIM2,NJM2,NKM2

C Debug.
c          write(6,*)IREC,':',IDRSZE,NIM2,NJM2,NKM2

C NRCFDOM is the number of records for each CFD domain.
          NRCFDOM(ICFD)=IDRSZE

C NCFDSZ is the number of records for each timestep (all CFD domains).
          NCFDSZ=NCFDSZ+IDRSZE

        endif
      enddo

C If IZONE has not told us which CFD domain to use, and if there is more
C than one, ask user which one to use.
      if (NCDOM.eq.0) then
        call EDISP(iuout,'No CFD zones detected')
      elseif (NCDOM.eq.1) then
        ICFD=1
      elseif (IZONE.gt.0) then
        ICFD=IFCFD(IZONE)
      else
 98     IER=0
        helptopic='res_cfd_which_res_set'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL EASKI(I,' There is more than 1 CFD domain.',
     &    ' Output results for which domain? ',
     &    1,'F',NCDOM,'F',1,' CFD domain',IER,nbhelp)
        if (IER.ne.0) goto 98
        ICFD=I
      endif
      ICP=ICFDZ(ICFD)

      IREC=3+(8*(ICFD-1))

C Read number of contaminants.
      IREC=IREC+1
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)NCTM(ICFD)

C Read contaminant names.
      IREC=IREC+1
      if (NCTM(ICFD).gt.0) then
        read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &    (POLNAM(I,ICFD),I=1,NCTM(ICFD))
      endif

      IREC=IREC+1
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &  (IEQSV(I),I=1,5+NCTM(ICFD))

C Read domain size.
      IREC=IREC+1
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) IDRSZE,NIM2,NJM2,NKM2

C Set grid variables
      NI=NIM2+2
      NJ=NJM2+2
      NK=NKM2+2
      NIM1=NIM2+1
      NJM1=NJM2+1
      NKM1=NKM2+1

C Write grid sizing information.
      IREC=IREC+1
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) (XU(I),I=1,NI)

      IREC=IREC+1
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) (YV(I),I=1,NJ)

      IREC=IREC+1
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000) (ZW(I),I=1,NK)

C Write grid origin vertices.
      IREC=IREC+1
      read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)IORG(ICFD),ixend(ICFD),
     &  iyend(ICFD),izend(ICFD)

C Debug.
c      write(6,*)IREC,IORG(ICFD),ixend(ICFD),iyend(ICFD),izend(ICFD)

      goto 998

C Set geometric variables.
  998 call GRIDGEO

  999 return

 1000 IER=-1
      call edisp(iuout,'CFD lib header fault. Cannot read file')

C Debug.
C      write(6,*) 'IREC ',IREC

c      call erpfree(iunit,istat)
      return
      end

C ********************* MCFDM *********************
C MCFDM - define the parameters for the visualization of results.

      subroutine MCFDM
#include "building.h"
#include "cfd.h"
#include "geometry.h"
#include "prj3dv.h"
      
      integer lnblnk  ! function definition
      integer iCountWords

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2

      COMMON/cfdsmper/ICDYS,ICDYF,CFTS,CFTF
      COMMON/cfdotper/ICDYOS,ICDYOF,CFTOS,CFTOF
      common/flvimg/imgtyp,IMOPTS
      common/flvnam/LBITrt(MCOM,6)
      common/flvdat/Nview(MCOM),ISURvw(MCOM,6),ILAYvw(MCOM,6),
     &              IRESvw(MCOM,6),VLSCvw(MCOM,6),VTSCvw(MCOM,6),
     &              HLSCvw(MCOM,6),HTSCvw(MCOM,6),IFRQvw(MCOM,6)
      common/flvcol/ISOPT(6),IBGOPT(6),SCmin,SCmax
      COMMON/cfddoms/NCDOM,ICFDZ(MNZ)
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)

      common/MRTF/FFL(MCUB,6,MSM),VEWF(MS,MS)
      real XC,YC,ZC
      integer IX,IY,IZ
      integer IZNSEL(MCON),ISFSEL(MCON)
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      COMMON/ICFNOD/ICFD,ICP

      CHARACTER KEY*1,ITEM(35)*26,outs*124
      character LBITrt*12,t12*12,t24*24
      integer IWM,IWE  ! for radio buttons
      integer NITMS,INO ! max items and current menu item
      logical ok,dok

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MCFDM'  ! set for subroutine

C Set vector colour option to velocity and temperature only.
      IMOPTS=0

C Create a menu showing the available database items.  Allow user to
C select one and then list details of this item, allowing editing.
C Setup for multi-page menu.

C Note: if you do not alter the output period it will produce images
C or image files for each of the timesteps defined in the CFD output
C file.
      ICOMP=ICP
 3    MHEAD=3
      MCTL=6
      ILEN=Nview(ICOMP)
      INO=-3

C imgtyp determines the format of the generated image:
C  imgtyp = 0 : 2d slice drawn on screen.
C  imgtyp = 1 : 2d slice in Xbitmap format to file.
C  imgtyp = 2 : 2d slice in Xpixmap format to file.
C  imgtyp = 3 : 3d image drawn on screen.
C  imgtyp = 4 : TECplot data file.
C  imgtyp = 5 : ParaView data file.
      ITEM(1)='a Output period'
      if (imgtyp.eq.0) then
        ITEM(2)='b Image format >>screen2d'
      elseif (imgtyp.eq.1) then
        ITEM(2)='b Image format >> Xbitmap'
      elseif (imgtyp.eq.2) then
        ITEM(2)='b Image format >> XPM    '
      elseif (imgtyp.eq.3) then
        ITEM(2)='b Image format >>screen3d'
      elseif (imgtyp.eq.4) then
        ITEM(2)='b Output format >>TECplot'
      elseif (imgtyp.eq.5) then
        ITEM(2)='b Output format >>ParaView'
      endif
      ITEM(3)=' ----------------------- '

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu. 
      M=MHEAD
      DO 20 IM=1,ILEN
        M=M+1
        CALL EMKEY(M,KEY,IER)
        WRITE(ITEM(M),22) KEY,LBITrt(ICOMP,IM)
  22    FORMAT(A1,1X,A12)
  20  CONTINUE

C Number of actual items displayed.
      NITMS=M+MCTL
      ITEM(M+1)=' ----------------------- '
      ITEM(M+2)='* add/ delete view '
      ITEM(M+3)='! list current '
      ITEM(M+4)='> create image(s)   '
      ITEM(M+5)='? help     '
      ITEM(M+6)='- exit this menu  '
      INO=-4

C Instanciate help strings for this menu.
      helptopic='res_cfd_2d_slice'
      call gethelptext(helpinsub,helptopic,nbhelp)
 
  80  CALL EMENU('Flow visualization',ITEM,NITMS,INO)
      if (INO.EQ.0)THEN

C Try again.
        INO=-1
        GOTO 80
      elseif (INO.EQ.(NITMS))THEN
        return
      elseif (INO.EQ.(NITMS-1))THEN
        helptopic='res_cfd_2d_slice'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD('flow vis facility',nbhelp,'-',0,0,IER)
      elseif (INO.EQ.(NITMS-2)) THEN

C Generate images.
C Loop through each timestep. 
C Calculate number of time steps for output, set output frame and
C redefine name of image. As for temperature range.
        if(imgtyp.eq.0.or.imgtyp.eq.3)then
          t24='30.0 0.0'
 204      helptopic='contaminant_scale_req'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call EASKS(t24,'  ',
     &      'max and min Temp:',24,'50.0 0.0','scale',IER,nbhelp)
          K=0
          call EGETWR(t24,K,SCmax,0.,0.,'-','maximum',IER)
          call EGETWR(t24,K,SCmin,0.,0.,'-','minimum',IER)
          if (SCmax.lt.SCmin) goto 204
        endif
        if (imgtyp.eq.3) CALL INIT_CFDTRANS(IER)
        if (IER.ne.0) goto 52
        NCT=int((float(ICDYOF-ICDYOS)*24.0+CFTOF-CFTOS)*float(NTS))+1
        ICTS=int((float(ICDYOS-ICDYS)*24.0+CFTOS-CFTS)*float(NTS))+1
        ICTF=ICTS+NCT-1
        if (NCT.gt.0) then
          do 48 IM=1,Nview(ICOMP)
            ISOPT(IM)=-1
 48       continue
          do 50 IFRAME=ICTS,ICTF
            call RDCFDAT(IFRAME)
            if (imgtyp.eq.4) then
              call tecplotend(IFRAME)
            elseif (imgtyp.eq.5) then
              call paraviewend(IFRAME)
            else
              call VISMAK(ICOMP,IFRAME)

C If drawing on screen allow a pause between each timestep. If user
C says no then exit the loop.
              if(imgtyp.eq.0.or.imgtyp.eq.3)then
                dok=.true.
                call ASKOK('View next timestep?',' ',ok,dok,nbhelp)
                iF(.NOT.ok) goto 51
              endif
            endif
 50       continue
 51       continue
        endif
 52     continue

      elseif (INO.EQ.(NITMS-3)) THEN

C List current.
        if (Nview(ICOMP).eq.0) then
          write (outs,'(a)') 'No view definitions to list yet !'
          call edisp(iuout,outs)
        else
          write (outs,'(a)') 'Current view definitions:'
          call edisp(iuout,outs)
          write (outs,'(a,a)') 'Image root |View  |Layer| Res  |',
     &            '     Arrow scaling factors           | Freq'
          call edisp(iuout,outs)
          write (outs,'(a,a)') ' name      |direct|     |      |',
     &            'Shaft len|Shaft thk|Head len|Head thk|'
          call edisp(iuout,outs)
          outs='  '
          do 100 I=1,Nview(ICOMP)
            write (outs(1:13),'(a)') LBITrt(ICOMP,I)

C Viewing direction.
            if (ISURvw(ICOMP,I).eq.1) then
              write (outs(15:21),'(a)') 'North '
            elseif (ISURvw(ICOMP,I).eq.2) then
              write (outs(15:21),'(a)') 'East  '
            elseif (ISURvw(ICOMP,I).eq.3) then
              write (outs(15:21),'(a)') 'South '
            elseif (ISURvw(ICOMP,I).eq.4) then
              write (outs(15:21),'(a)') 'West  '
            elseif (ISURvw(ICOMP,I).eq.5) then
              write (outs(15:21),'(a)') 'Top   '
            elseif (ISURvw(ICOMP,I).eq.6) then
              write (outs(15:21),'(a)') 'Bottom'
            endif

C Layer.
            write (outs(22:25),'(i3)') ILAYvw(ICOMP,I)

C Image quality.
            if (IRESvw(ICOMP,I).eq.1) then
              write (outs(26:32),'(a)') 'Low   '
            elseif (IRESvw(ICOMP,I).eq.2) then
              write (outs(26:32),'(a)') 'Medium'
            elseif (IRESvw(ICOMP,I).eq.3) then
              write (outs(26:32),'(a)') 'High  '
            endif

C Arrow style.
            write(outs(33:71),'(2(f6.1,4x),2(f5.1,4x))')VLSCvw(ICOMP,I),
     &              VTSCvw(ICOMP,I),HLSCvw(ICOMP,I),HTSCvw(ICOMP,I)

C Image frequency (dynamic flow viualisation only).
            write (outs(71:75),'(i4)') IFRQvw(ICOMP,I)
            call edisp(iuout,outs)
 100      continue
        endif
      elseif (INO.EQ.(NITMS-4)) THEN

C Add/ delete image definition.
        helptopic='res_cfd_new_view'
        call gethelptext(helpinsub,helptopic,nbhelp)
        call EASKATOG(' ','View definitions:','add one','delete one',
     &    'MRT sensor','room COG','continue',' ',' ',
     &    IW,nbhelp)
        if (IW.eq.1) then

C Add new image definition.
          Nview(ICOMP)=Nview(ICOMP)+1
          NV=Nview(ICOMP)
 101      helptopic='res_cfd_view_root_name'
          call gethelptext(helpinsub,helptopic,nbhelp)
          t12=' new_view'
          CALL EASKS(t12,' ',' Name for image ? ',
     &      12,'flowvis','image name',IER,nbhelp)
          if (t12(1:2).eq.'  ') goto 101
          LBITrt(ICOMP,NV)=t12
          helptopic='res_cfd_view_direc'
          call gethelptext(helpinsub,helptopic,nbhelp)
          IWM=1
          CALL EASKATOG('View direction:',' ','South','East',
     &      'North','West','Top','Bottom','  ',IWM,nbhelp)
          ISURvw(ICOMP,NV)=IWM
 102      IER=0

C Display grid, to aid user in picking a plane.
          call INIT_CFDTRANS(IER)
          call VGRID3D('zn ','xyz',ICP)

          IF(IWM.EQ.1)THEN

C View from south.
            CALL EASKI(ilayer,'  ',' Which j layer ',
     &        1,'F',NJM2,'F',1,' Slice number',IER,nbhelp)
            IF(IER.NE.0) GOTO 102
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   X-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Z-axis displayed vertically.')
          ELSEIF(IWM.EQ.2)THEN

C View from east.
            CALL EASKI(ilayer,'  ',' Which i layer ',
     &        1,'F',NIM2,'F',1,' Slice number',IER,nbhelp)
            IF(IER.NE.0) GOTO 102
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   Y-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Z-axis displayed vertically.')
          ELSEIF(IWM.EQ.3)THEN

C View from north.
            CALL EASKI(ilayer,'  ',' Which j layer ',
     &        1,'F',NJM2,'F',1,' Slice number',IER,nbhelp)
            IF(IER.NE.0) GOTO 102
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   X-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Z-axis displayed vertically.')
          ELSEIF(IWM.EQ.4)THEN

C View from west.
            CALL EASKI(ilayer,'  ',' Which i layer ',
     &        1,'F',NIM2,'F',1,' Slice number',IER,nbhelp)
            IF(IER.NE.0) GOTO 102
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   Y-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Z-axis displayed vertically.')
          ELSEIF(IWM.EQ.5)THEN

C View from top.
            CALL EASKI(ilayer,'  ',' Which k layer ',
     &        1,'F',NKM2,'F',1,' Slice number',IER,nbhelp)
            IF(IER.NE.0) GOTO 102
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   X-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Y-axis displayed vertically.')
          ELSEIF(IWM.EQ.6)THEN

C View from bottom.
            CALL EASKI(ilayer,'  ',' Which k layer ',
     &        1,'F',NKM2,'F',1,' Slice number',IER,nbhelp)
            IF(IER.NE.0) GOTO 102
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   X-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Y-axis displayed vertically.')
          endif
          ILAYvw(ICOMP,NV) = ilayer + 1

C Image resolution.
          helptopic='res_cfd_view_resolution'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call EASKABC(' ','Image resolution?','Low','Medium','High',
     &      IWR,nbhelp)
          IRESvw(ICOMP,NV)=IWR

C Set defaults for customisation and frequency variables.
          VLSCvw(ICOMP,NV)=1.0
          VTSCvw(ICOMP,NV)=1.0
          HLSCvw(ICOMP,NV)=1.0
          HTSCvw(ICOMP,NV)=1.0
          IFRQvw(ICOMP,NV)=4
          write (outs,'(2a)')' Defaults set for arrow scaling factors ',
     &                'and generation frequency. Edit via image pick. '
          call edisp(iuout,outs)

        elseif (IW.eq.2) then

C Remove an image definition.
          CALL EASKI(NV,'  ',' Which view to remove? ',1,'F',
     &      Nview(ICOMP),'F',Nview(ICOMP),' View number',IER,nbhelp)
          if (IV.lt.Nview(ICOMP)) then
            do IV=NV,(Nview(ICOMP)-1)
              LBITrt(ICOMP,IV)=LBITrt(ICOMP,IV+1)
              ISURvw(ICOMP,IV)=ISURvw(ICOMP,IV+1)
              ILAYvw(ICOMP,IV)=ILAYvw(ICOMP,IV+1)
              IRESvw(ICOMP,IV)=IRESvw(ICOMP,IV+1)
            enddo
          endif
          LBITrt(ICOMP,Nview(ICOMP))='UNKNOWN'
          ISURvw(ICOMP,Nview(ICOMP))=0
          ILAYvw(ICOMP,Nview(ICOMP))=0
          IRESvw(ICOMP,Nview(ICOMP))=0
          Nview(ICOMP)=Nview(ICOMP)-1

        elseif (IW.eq.3) then

C Add XY, XZ and YZ planes intersecting at an MRT sensor.

C If old CFD results version, read CFD file to get axis vertices to
C initialise CFD - building domain transformation matrices.
          NSEL=1
          CALL PIKMRTS(IZNSEL,ISFSEL,NSEL,ICOMP,IER)
          IMRT=ISFSEL(1)
          if (ICFDSV.eq.1.or.ICFDSV.eq.2) then
            CALL RCFDVRTS(ICOMP,IER)
          endif
          CALL INIT_CFDTRANS(IER)
          if (IER.ne.0) then
            call edisp(iuout,' Problem reading CFD axis vertices')
          else
C Find CFD point closest to MRT sensor.              
            CALL GRIDGEO
            PI = 4.0 * ATAN(1.0)
            R=PI/180.
            SA=SIN(CANG(IMRT)*R)
            CA=COS(CANG(IMRT)*R)
            DX=(DXC(IMRT)/2.)*CA-(DYC(IMRT)/2.)*SA
            DY=(DXC(IMRT)/2.)*SA+(DYC(IMRT)/2.)*CA
            CALL CFDTRANS(2,XOC(IMRT)+DX,YOC(IMRT)+DY,
     &                      ZOC(IMRT)+DZC(IMRT)/2,XC,YC,ZC,IER)
            CALL FDCFDPT(XC,YC,ZC,IIX,IIY,IIZ,IER)
            if (IER.ne.0) then
              call edisp(iuout,' Problem finding CFD point')
            endif
          endif
         
          Nview(ICOMP)=Nview(ICOMP)+1
          NV=Nview(ICOMP)
          LBITrt(ICOMP,NV)='XZ'            
          ISURvw(ICOMP,NV)=1
          ILAYvw(ICOMP,NV)=IIY
          IRESvw(ICOMP,NV)=2
          VLSCvw(ICOMP,NV)=1.0
          VTSCvw(ICOMP,NV)=1.0
          HLSCvw(ICOMP,NV)=1.0
          HTSCvw(ICOMP,NV)=1.0
          IFRQvw(ICOMP,NV)=4
            
          Nview(ICOMP)=Nview(ICOMP)+1
          NV=Nview(ICOMP)
          LBITrt(ICOMP,NV)='YZ'            
          ISURvw(ICOMP,NV)=2
          ILAYvw(ICOMP,NV)=IIX
          IRESvw(ICOMP,NV)=2
          VLSCvw(ICOMP,NV)=1.0
          VTSCvw(ICOMP,NV)=1.0
          HLSCvw(ICOMP,NV)=1.0
          HTSCvw(ICOMP,NV)=1.0
          IFRQvw(ICOMP,NV)=4
            
          Nview(ICOMP)=Nview(ICOMP)+1
          NV=Nview(ICOMP)
          LBITrt(ICOMP,NV)='XY'            
          ISURvw(ICOMP,NV)=6
          ILAYvw(ICOMP,NV)=IIZ
          IRESvw(ICOMP,NV)=2
          VLSCvw(ICOMP,NV)=1.0
          VTSCvw(ICOMP,NV)=1.0
          HLSCvw(ICOMP,NV)=1.0
          HTSCvw(ICOMP,NV)=1.0
          IFRQvw(ICOMP,NV)=4

          write (outs,'(2a)')' Defaults set for arrow scaling factors ',
     &                'and generation frequency. Edit via image pick. '
          call edisp(iuout,outs)

          goto 3

        elseif (IW.eq.4) then

C Add XY, XZ and YZ planes intersecting at the room centre of gravity.

          nzg=1
          nznog(1)=ICOMP
          ITBND=1
          CALL BNDOBJ(0,IER)
          if (IER.ne.0) then
            call edisp(iuout,' Problem calculating zone COG')
            goto 3
          endif

C If old CFD results version, read CFD file to get axis vertices to
C initialise CFD - building domain transformation matrices.
          if (ICFDSV.eq.1.or.ICFDSV.eq.2) then
            CALL RCFDVRTS(ICOMP,IER)
          endif
          CALL INIT_CFDTRANS(IER)
          if (IER.ne.0) then
            call edisp(iuout,' Problem reading CFD axis vertices')
            goto 3
          else
C Find CFD plane closest to selected Z height.
            CALL GRIDGEO
            CALL CFDTRANS(2,ZCOG(ICOMP,1),ZCOG(ICOMP,2),ZCOG(ICOMP,3),
     &        XC,YC,ZC,IER)
            CALL FDCFDPT(XC,YC,ZC,IIX,IIY,IIZ,IER)
            if (IER.ne.0) then
              call edisp(iuout,' Problem finding CFD point')
              goto 3
            endif
          endif

          Nview(ICOMP)=Nview(ICOMP)+1
          NV=Nview(ICOMP)
          LBITrt(ICOMP,NV)='XZ'            
          ISURvw(ICOMP,NV)=1
          ILAYvw(ICOMP,NV)=IIY
          IRESvw(ICOMP,NV)=2
          VLSCvw(ICOMP,NV)=1.0
          VTSCvw(ICOMP,NV)=1.0
          HLSCvw(ICOMP,NV)=1.0
          HTSCvw(ICOMP,NV)=1.0
          IFRQvw(ICOMP,NV)=4

          Nview(ICOMP)=Nview(ICOMP)+1
          NV=Nview(ICOMP)
          LBITrt(ICOMP,NV)='YZ'            
          ISURvw(ICOMP,NV)=2
          ILAYvw(ICOMP,NV)=IIX
          IRESvw(ICOMP,NV)=2
          VLSCvw(ICOMP,NV)=1.0
          VTSCvw(ICOMP,NV)=1.0
          HLSCvw(ICOMP,NV)=1.0
          HTSCvw(ICOMP,NV)=1.0
          IFRQvw(ICOMP,NV)=4
            
          Nview(ICOMP)=Nview(ICOMP)+1
          NV=Nview(ICOMP)
          LBITrt(ICOMP,NV)='XY'            
          ISURvw(ICOMP,NV)=6
          ILAYvw(ICOMP,NV)=IIZ
          IRESvw(ICOMP,NV)=2
          VLSCvw(ICOMP,NV)=1.0
          VTSCvw(ICOMP,NV)=1.0
          HLSCvw(ICOMP,NV)=1.0
          HTSCvw(ICOMP,NV)=1.0
          IFRQvw(ICOMP,NV)=4

          write (outs,'(2a)')' Defaults set for arrow scaling factors ',
     &                'and generation frequency. Edit via image pick. '
          call edisp(iuout,outs)

          goto 3

        endif

C Output period.
      ELSEIF(INO.eq.1) then
        CALL CFDPER(3)

C Image format.
      ELSEIF(INO.eq.2) then
        imgtyp=imgtyp+1
        if (imgtyp.gt.5) imgtyp=0

C Existing image selected.
      ELSEIF(INO.GT.MHEAD.AND.INO.LT.(NITMS-MCTL+1))THEN

C Decode from the potentially long list to the zone number via KEYIND.
C If delete or copy scene previously selected then new selection should 
C be deleted or copied, respectively.
        NV=INO-MHEAD
 200    write (outs,'(a,a,a)') 'For image definition ',
     &        LBITrt(ICOMP,NV)(1:lnblnk(LBITrt(ICOMP,NV))),' edit:'
        IWE=1
        helptopic='res_cfd_view_edit'  ! combined messages
        call gethelptext(helpinsub,helptopic,nbhelp)
        call EASKATOG(outs,' ','Name','View dir & layer','Resolution',
     &    'Customise arrows','Continue','  ','  ',IWE,nbhelp)
        if (IWE.eq.1) then

C Image root name.
 201      CALL EASKS(t12,' ',' Name for image ? ',
     &      12,'flowvis','image name',IER,nbhelp)
          if (t12(1:2).eq.'  ') goto 201
          LBITrt(ICOMP,NV)=t12
        elseif (IWE.eq.2) then

C View direction and layer.
          IWM=1
          CALL EASKATOG('View direction:',' ','South','East',
     &      'North','West','Top','Bottom','  ',IWM,nbhelp)
          ISURvw(ICOMP,NV)=IWM
 202      IER=0

C Display grid, to aid user in picking a plane.
          call INIT_CFDTRANS(IER)
          call VGRID3D('zn ','xyz',ICP)

          IF(IWM.EQ.1)THEN

C View from north.
            CALL EASKI(ilayer,'  ',' Which j layer ',
     &        1,'F',NJM2,'F',1,' Slice number',IER,nbhelp)
            IF(IER.NE.0) GOTO 202
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   X-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Z-axis displayed vertically.')
          ELSEIF(IWM.EQ.2)THEN

C View from east.
            CALL EASKI(ilayer,'  ',' Which i layer ',
     &        1,'F',NIM2,'F',1,' Slice number',IER,nbhelp)
            IF(IER.NE.0) GOTO 202
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   Y-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Z-axis displayed vertically.')
          ELSEIF(IWM.EQ.3)THEN

C View from south.
            CALL EASKI(ilayer,'  ',' Which j layer ',
     &        1,'F',NJM2,'F',1,' Slice number',IER,nbhelp)
            IF(IER.NE.0) GOTO 202
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   X-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Z-axis displayed vertically.')
          ELSEIF(IWM.EQ.4)THEN

C View from west.
            CALL EASKI(ilayer,'  ',' Which i layer ',
     &        1,'F',NIM2,'F',1,' Slice number',IER,nbhelp)
            IF(IER.NE.0) GOTO 202
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   Y-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Z-axis displayed vertically.')
          ELSEIF(IWM.EQ.5)THEN

C View from top.
            CALL EASKI(ilayer,'  ',' Which k layer ',
     &        1,'F',NKM2,'F',1,' Slice number',IER,nbhelp)
            IF(IER.NE.0) GOTO 202
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   X-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Y-axis displayed vertically.')
          ELSEIF(IWM.EQ.6)THEN

C View from bottom.
            CALL EASKI(ilayer,'  ',' Which k layer ',
     &        1,'F',NKM2,'F',1,' Slice number',IER,nbhelp)
            IF(IER.NE.0) GOTO 202
            CALL EDISP(IUOUT,' Axis orientation in image:')
            CALL EDISP(IUOUT,'   X-axis displayed horizontally;')
            CALL EDISP(IUOUT,'   Y-axis displayed vertically.')
          endif
          ILAYvw(ICOMP,NV) = ilayer + 1

C Image resolution.
        elseif (IWE.eq.3) then
          call EASKABC(' ','Image resolution?','Low','Medium','High',
     &      IWR,nbhelp)
          IRESvw(ICOMP,NV)=IWR
        elseif (IWE.eq.4) then

C Customise arrows.
 203      write (t24,'(6f6.1)') VLSCvw(ICOMP,NV),VTSCvw(ICOMP,NV),
     &                          HLSCvw(ICOMP,NV),HTSCvw(ICOMP,NV)
          helptopic='res_cfd_view_vector_edit'
          call gethelptext(helpinsub,helptopic,nbhelp)
          call EASKS(t24,
     &      'Scale factors: Shaft len; Shaft thK; Head len; Head thk',
     &      '  ',24,' 1.0 1.0 1.0 1.0','scale factrs',IER,nbhep)
          ISC = iCountWords(t24)
          if (ISC.ne.4) goto 203
          K=0
          call EGETWR(t24,K,SIL,0.,0.,'-','shaft length',IER)
          VLSCvw(ICOMP,NV)=SIL
          call EGETWR(t24,K,SIL,0.,0.,'-','shaft thickness',IER)
          VTSCvw(ICOMP,NV)=SIL
          call EGETWR(t24,K,SIL,0.,0.,'-','head length',IER)
          HLSCvw(ICOMP,NV)=SIL
          call EGETWR(t24,K,SIL,0.,0.,'-','head thickness',IER)
          HTSCvw(ICOMP,NV)=SIL
        elseif (IWE.eq.5) then
          goto 3
        elseif (IWE.eq.6) then
          goto 3
        endif
        goto 200
      endif

      goto 3
      end

C ******************** CFDPER ********************
C CFDPER sets the CFD output period. 
C IOPT=1 selects whole period
C IOPT=2 user selects one time step
C IOPT=3 user selects period

      SUBROUTINE CFDPER(IOPT)

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/cfdsmper/ICDYS,ICDYF,CFTS,CFTF
      COMMON/cfdotper/ICDYOS,ICDYOF,CFTOS,CFTOF

      CHARACTER MTHNAM(12)*3,outs*124,PDESCR*60

      DATA MTHNAM/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/

      if (IOPT.eq.1) then

C In default mode then set output period to whole period.
        ICDYOS=ICDYS
        ICDYOF=ICDYF
        CFTOS=CFTS
        CFTOF=CFTF
      elseif (IOPT.eq.2) then
        write (outs,'(2a)') 'Please pick a single timestep for flow ',
     &          'visualisation.'
        call edisp(iuout,outs)
        CALL EDAYR(ICDYS,IDs,IMs)
        CALL EDAYR(ICDYF,IDf,IMf)
        write(PDESCR,'(2(i3,a4,a,f5.2,a))')IDs,MTHNAM(IMs),', hr ',CFTS,
     &              ' to',IDf,MTHNAM(IMf),', hr ',CFTF,'.'
        write (outs,'(2a)')
     &     'Current result set contains CFD data for the period:',PDESCR
        call edisp(iuout,outs)
        call edisp(iuout,'Set a time within this period to plot.')

C Set default to first timestep.
        IMO=IMs
        IDO=IDs
        IDAY=ICDYS
        TIME=CFTS

C Select a single timestep to plot output for.
 10     IFDAY=1
        call ASKRTIM(IFDAY,IMO,IDO,IDAY,TIME,IT,IER)

C Check that CFD active at this time.
        if (IDAY.lt.ICDYS.or.IDAY.gt.ICDYF) then
          call edisp(iuout,'Time is outside simulated period.')
          goto 10
        elseif (IDAY.eq.ICDYS.and.IDAY.eq.ICDYF.and.
     &          TIME.lt.CFTS.and.TIME.gt.CFTF) then
          call edisp(iuout,'Time outside simulated period. Try again.')
          goto 10
        elseif (IDAY.eq.ICDYS.and.TIME.lt.CFTS) then
          call edisp(iuout,'Time is before start time. Try again.')
          goto 10
        elseif (IDAY.eq.ICDYF.and.TIME.gt.CFTF) then
          call edisp(iuout,'Time is after finish time. Try again.')
          goto 10
        endif
        ICDYOS=IDAY
        ICDYOF=IDAY
        CFTOS=TIME
        CFTOF=TIME
      else
        ICDYOS=ICDYS
        ICDYOF=ICDYF
        CFTOS=CFTS
        CFTOF=CFTF
        CALL EDAYR(ICDYS,IDs,IMs)
        CALL EDAYR(ICDYF,IDf,IMf)
        write(PDESCR,'(2(i3,a4,a,f5.2,a))')IDs,MTHNAM(IMs),', hr ',CFTS,
     &              ' to',IDf,MTHNAM(IMf),', hr ',CFTF,'.'
        write (outs,'(2a)')
     &     'Current result set contains CFD data for the period:',PDESCR
        call edisp(iuout,outs)
 20     IFDAY=1
        call ASKRTIM(IFDAY,IMs,IDs,ICDYOS,CFTOS,IT,IER)
        call ASKRTIM(IFDAY,IMf,IDf,ICDYOF,CFTOF,IT,IER)

C Check that CFD active at this time.
        if (ICDYOS.lt.ICDYS.or.ICDYOF.gt.ICDYF) then
          call edisp(iuout,'Time is outside simulated period.')
          goto 20
        elseif (ICDYOS.eq.ICDYS.and.CFTOS.lt.CFTS) then
          call edisp(iuout,'Time is before start.')
          goto 20
        elseif (ICDYOF.eq.ICDYF.and.CFTOF.gt.CFTF) then
          call edisp(iuout,'Time is after finish.')
          goto 20
        elseif (ICDYOS.eq.ICDYS.and.ICDYOF.eq.ICDYF.and.
     &          CFTOS.lt.CFTS.and.CFTOF.ge.CFTF) then
          call edisp(iuout,'Time outside simulated period.')
          goto 20
        endif
      endif

      RETURN
      END

C ******************** RDCFDAT ********************
C RDCFDAT reads a timesteps worth of data into standard arrays.
C   ICFTS - CFD timestep to read (use this value if +ive otherwise
C      use first timestep of output period)

      SUBROUTINE RDCFDAT(ICFTS)
#include "building.h"
#include "cfd.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/cfdsmper/ICDYS,ICDYF,CFTS,CFTF
      COMMON/cfdotper/ICDYOS,ICDYOF,CFTOS,CFTOF

      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      COMMON/GEOM/XP(ntcelx),YP(ntcely),ZP(ntcelz),
     1            DXEP(ntcelx),DXPW(ntcelx),DYNP(ntcely),DYPS(ntcely),
     2            DZHP(ntcelz),DZPL(ntcelz),
     3            SEW(ntcelx),SNS(ntcely),SHL(ntcelz),
     4            XU(ntcelx),YV(ntcely),ZW(ntcelz)

      common/vecXYZ/vecXbeg(ntcelx,ntcely,ntcelz),
     1              vecXend(ntcelx,ntcely,ntcelz),
     2              vecYbeg(ntcelx,ntcely,ntcelz),
     3              vecYend(ntcelx,ntcely,ntcelz),
     4              vecZbeg(ntcelx,ntcely,ntcelz),
     5              vecZend(ntcelx,ntcely,ntcelz)
      COMMON/VARf/Uf(ntcelx,ntcely,ntcelz),Vf(ntcelx,ntcely,ntcelz),
     1            Wf(ntcelx,ntcely,ntcelz),
     2            P(ntcelx,ntcely,ntcelz),PP(ntcelx,ntcely,ntcelz),
     3            TEf(ntcelx,ntcely,ntcelz),EDf(ntcelx,ntcely,ntcelz)
      COMMON/TEMPf/Tf(ntcelx,ntcely,ntcelz),GAMH(ntcelx,ntcely,ntcelz),
     1             RESORT,NSWPT,URFT,FSDTT,PRANDL,PFUN
      COMMON/LOCAGE/AGEf(ntcelx,ntcely,ntcelz)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      COMMON/CFDPOL/POLCONCp(MCTM,ntcelx,ntcely,ntcelz),
     1              POLCONCf(MCTM,ntcelx,ntcely,ntcelz)

      COMMON/cfdhsh/NCFDSZ,NRCFDOM(MNZ)
      COMMON/cfddoms/NCDOM,ICFDZ(MNZ)
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      COMMON/ICFNOD/ICFD,ICP

      CHARACTER  POLNAM*12
      LOGICAL CALPOL,CALLMA

      logical ok

C Set unit number.
      iunit=IFIL+14

C Calculate number of time steps into CFD results lib.
      if (ICFTS.gt.0) then
        ITS=ICFTS
      else
        ITS=int((float(ICDYOS-ICDYS)*24.0+CFTOS-CFTS)*float(NTS))+1
      endif

C Read data from res file.
      if (ICFDSV.eq.1.or.ICFDSV.eq.2) then
        ISTREC=5+((MCOM-1)*4)+ NCFDSZ*(ITS-1)+NCTM(ICFD)
      elseif (ICFDSV.eq.3) then
        ISTREC=4+(NCDOM*8)+ NCFDSZ*(ITS-1)
      endif
      if (ICFD.gt.1) then
        do 10 I=1,ICFD-1
          ISTREC=ISTREC+NRCFDOM(I)
 10     continue
      endif

C Debug.    
c      write(6,*) 'ISTREC ',ISTREC,NCFDSZ,ITS,NRCFDOM(1),NRCFDOM(2)
c      write(6,*)'ieqsv ',(ieqsv(i),i=1,5+NCTM(ICFD))

      IREC=ISTREC
      do 100 K=2,NK
        do 101 J=2,NJ

C Read data from library. Adjust trace to file unit 6 if ntcelz>48.
          if (ICFDSV.eq.1) then
            IREC=IREC+1
            read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Uf(I,J,K),I=2,NI)

C Debug.
C            write(395,'(i5,48f9.5)') IREC,(Uf(I,J,K),I=2,NI)

            IREC=IREC+1
            read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Vf(I,J,K),I=2,NI)

C Debug.
C            write(395,'(i5,48f9.5)') IREC,(Vf(I,J,K),I=2,NI)

            IREC=IREC+1
            read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Wf(I,J,K),I=2,NI)

C Debug.
C            write(395,'(i5,48f9.5)') IREC,(Wf(I,J,K),I=2,NI)

            IREC=IREC+1
            read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)(Tf(I,J,K),I=2,NI)

C Debug.
C            write(395,'(i5,48f9.5)') IREC,(Tf(I,J,K),I=2,NI)

          else
          
C Results library version 2 and 3.  Each metric is individually flagged
C if saved.
            if (IEQSV(1).eq.1) then
              IREC=IREC+1
              read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                             (Uf(I,J,K),I=2,NI)

C Debug.
c              write(395,*) IREC,' Uf ',(Uf(I,J,K),I=2,NI)

            endif
            if (IEQSV(2).eq.1) then
              IREC=IREC+1
              read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                             (Vf(I,J,K),I=2,NI)

C Debug.
c              write(395,*) IREC,' Vf ',(Vf(I,J,K),I=2,NI)

            endif
            if (IEQSV(3).eq.1) then
              IREC=IREC+1
              read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                             (Wf(I,J,K),I=2,NI)

C Debug.
c              write(395,*) IREC,' Wf ',(Wf(I,J,K),I=2,NI)

            endif
            if (IEQSV(4).eq.1) then
              IREC=IREC+1
              read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                             (Tf(I,J,K),I=2,NI)

C Debug.
c              write(395,*) IREC,' Tf ',(Tf(I,J,K),I=2,NI)

            endif
            if (IEQSV(5).eq.1) then
              IREC=IREC+1
              read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                           (AGEf(I,J,K),I=2,NI)

C Debug.
c              write(395,*) IREC,' Age ',(AGEf(I,J,K),I=2,NI)

            endif
            DO 121 ICTM=1,NCTM(ICFD)
            if (IEQSV(5+NCTM(ICFD)).eq.1) then
              IREC=IREC+1
              read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)
     &                                     (POLCONCf(ICTM,I,J,K),I=2,NI)

C Debug.
c                  write(395,*)IREC,' pol ',(POLCONCf(ICTM,I,J,K),I=2,NI)

            endif
 121        CONTINUE
          endif
 101    continue
 100  continue

C Create vector info.
      do 200 K=2,NKM1
        do 201 J=2,NJM1
          do 202 I=2,NIM1
            hlfUatP=0.25*(Uf(I,J,K)+Uf(I+1,J,K))
            hlfVatP=0.25*(Vf(I,J,K)+Vf(I,J+1,K))
            hlfWatP=0.25*(Wf(I,J,K)+Wf(I,J,K+1))
            VECXbeg(I,J,K) = XP(I) - hlfUatP
            VECXend(I,J,K) = XP(I) + hlfUatP
            VECYbeg(I,J,K) = YP(J) - hlfVatP
            VECYend(I,J,K) = YP(J) + hlfVatP
            VECZbeg(I,J,K) = ZP(K) - hlfWatP
            VECZend(I,J,K) = ZP(K) + hlfWatP
 202      continue
 201    continue
 200  continue

      RETURN
 1000 call edisp(iuout,'CFD read data fault. Cannot read file.')

C Debug.
C      write(6,*) 'IREC ',IREC

      call erpfree(iunit,istat)
      return
      END

C ******************** OPCFD ********************
C OPCFD opens the CFD results library
C act='c' - check to see if the library is already open, and if not,
C           prompt the user for a file.
C act='o' - close a file if one is already open, and prompt the user for
C           a file.
      SUBROUTINE OPCFD(ACT,is_open,IER)
#include "building.h"
#include "cfd.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      common/rpath/path
      common/CFDFL/LCFDFL
      character LCFDFL*72

      character path*72,LCMDFL*144,ltmpc*144,LTMP*72,lpath*72
      integer igraphiclib  ! external definition
      character outs*124,ACT*1
      logical is_open,XST

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='OPCFD'  ! set for cfiles

      helptopic='res_cfd_lib_name'
      call gethelptext(helpinsub,helptopic,nbhelp)
      LTMP='UNKNOWN'
      ltmpc='UNKNOWN'
      llt=lnblnk(ltmpc)

      is_open=.false.

      if (ACT.eq.'c') then

C Check if CFD results library has been opened.
        INQUIRE(IFIL+14,OPENED=XST)
        if (XST) then
          goto 926
        else
C If no, ask for a file.
          goto 11
        endif
      elseif (ACT.eq.'o') then

C Ask for a file either way
        goto 11
      else
        call EDISP(iuout,' OPCFD: invalid ACT')
        return
      endif

C Ask for CFD results library.
C The X11 version will be returning only the name of the
C file, while the GTK version will be returning the
C name with the full path.
   11 iglib = igraphiclib()  ! find out if X11 or GTK or text support only.
      if(iglib.eq.1.or.iglib.eq.3)then
        if(llt.lt.96)then
          CALL EASKF(ltmpc,'CFD library name?',' ',96,LTMP,
     &    'CFDresfl name',IER,nbhelp)
        elseif(llt.ge.96.and.llt.lt.124)then
          CALL EASKF(ltmpc,'CFD library name?',' ',124,LTMP,
     &    'CFDresfl name',IER,nbhelp)
        elseif(llt.ge.124.and.llt.le.144)then
          CALL EASKF(ltmpc,'CFD library name?',' ',144,LTMP,
     &    'CFDresfl name',IER,nbhelp)
        endif
      elseif(iglib.eq.2)then
        CALL EASKF(ltmpc,'CFD library name?',' ',144,LTMP,
     &  'CFDresfl name',IER,nbhelp)
      else
        CALL EASKF(ltmpc,'CFD library name?',' ',96,LTMP,
     &  'CFDresfl name',IER,nbhelp)
      endif
      if(ier.eq.-3) goto 925  ! cancel detected, no CFD.

      if(ltmpc(1:2).ne.'  '.and.ltmpc(1:4).ne.'UNKN')then
        LCMDFL=ltmpc
      else
        goto 924
      endif
      IER=0

C Find the path and local file name.

C Debug.
C      write(6,*) 'OPCFD: current path is ',path

      iunit=IFIL+14
      lpath=path
      call fdroot(LCMDFL,path,LTMP)
      LCFDFL=LTMP
c      write(6,*)'lpath,LCMDFL,path,LTMP',lpath,LCMDFL,path,LTMP
      CALL ERPFREE(iunit,ISTAT)
      call FINDFIL(LTMP,XST)
      
      IF(XST)THEN

C Check that its a CFD results library.
        call EFOPRAN(iunit,LTMP,MCEL1D,1,IER)
        if(ier.eq.0) then
          irec=1
          read(iunit,rec=irec,iostat=istat) 
     &      IRLEN,ICFDSV,(IEQSV(J),J=1,5+MCTM)

C Debug.
C          write(6,*) IRLEN,ICFDSV,(IEQSV(J),J=1,5+MCTM)

          if (IRLEN.ne.MCEL1D) then
            path=lpath
            goto 924
          endif
        endif
      ELSE
        call usrmsg(' Could not find: ',LTMP,'W')
        path=lpath
        goto 924
      ENDIF
      path=lpath

 926  if (IER.ne.0) goto 924
      is_open=.true.
 925  RETURN

 924  write(outs,'(a)')' OPCFD: problem opening CFD library'
      call edisp(iuout,outs)
      IER=1
      goto 925

      END

C ******************** GETIFRM ********************
C GETIFRM returns IFRAME for getting CFD results using RDCFDAT, based on
C day and time step indices IDAY and ITS. Returns IER=1 if CFD results
C are not available for this timestep.

      SUBROUTINE GETIFRM(IDAY,ITS,IFRAME,IER)

      COMMON/cfdsmper/ICDYS,ICDYF,CFTS,CFTF
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE

      IER=0

C Check if there are CFD results available for this time step.
c      write(6,*)'IDAY,ICDYS,ICDYF',IDAY,ICDYS,ICDYF
      if (IDAY.eq.ICDYS.and.IDAY.eq.ICDYF) then              
c        write(6,*)'ITS,int(CFTS*float(NTS)),int(CFTF*float(NTS))',
c     &             ITS,int(CFTS*float(NTS)),int(CFTF*float(NTS))
        if (ITS.ge.int(CFTS*float(NTS)).and.
     &      ITS.le.int(CFTF*float(NTS))) then
          IFRAME=ITS-int(CFTS*float(NTS))+1
        else
          goto 666
        endif
      elseif (IDAY.eq.ICDYS) then
c        write(6,*)'ITS,int(CFTS*float(NTS))',
c     &             ITS,int(CFTS*float(NTS))
        if (ITS.ge.int(CFTS*float(NTS))) then
          IFRAME=ITS-int(CFTS*float(NTS))+1
        else
          goto 666
        endif
      elseif (IDAY.gt.ICDYS.and.IDAY.lt.ICDYF) then
        IFRAME=ITS+int((float(IDAY-ICDYS)*24.0-CFTS)*float(NTS))
      elseif (IDAY.eq.ICDYF) then            
c        write(6,*)'ITS,int(CFTF*float(NTS))',
c     &             ITS,int(CFTF*float(NTS))
        if (ITS.le.int(CFTF*float(NTS))) then
          IFRAME=ITS+int((float(IDAY-ICDYS)*24.0-CFTS)*float(NTS))
        else
          goto 666
        endif
      else
        goto 666
      endif
c      write(6,*)'IFRAME',IFRAME

 999  RETURN

 666  IER=1
      GOTO 999
      END

C ******************** MCFDV ********************
C MCFDV allows definition of CFD variables to be reported via the
C graphical or timestep data facilities of res. Currently we hijack the
C mass flow reporting data structures to do this.

C Much of this code is adapted from MFMENU in mfget.F.

C CFD metrics (for mass flow metrics, see mfget.F):
C IMFGET = 15 U velocity component
C IMFGET = 16 V velocity component
C IMFGET = 17 W velocity component
C IMFGET = 18 Velocity magnitude
C IMFGET = 19 Temperature
C IMFGET = 20 Mean age of air
C IMFGET = 21 Contaminant concentration

C IMFGETNO contains all the information needed for specifying which data to recover
C IMFGETNO(*,1): The IMFGET number - metric identifier
C IMFGETNO(*,2): For contaminant concentration, selected contaminant.
C                Also used to select line colour.
C IMFGETNO(*,3): Domain number
C IMFGETNO(*,4): LISTID for IMFLIST
C IMFGETNO(*,5): Line type
C IMFGETNO(*,6): Symbol index
C IMFGETNO(*,7): Associated axis 1=temperature, 
C                                2=pressure,
C                                3=mass flow rate
C                                4=misc, 
C                                5=concentration g/kg,
C                                6=velocity m/s
C IMFGETNO(*,8): 0 = standard units 1 alternative units e.g. l/s rather than m^3/s

C IMFLIST - selected cell to report:
C   IMFLIST(LISTID,1) = X cell
C   IMFLIST(LISTID,2) = Y cell
C   IMFLIST(LISTID,3) = Z cell

      SUBROUTINE MCFDV(idone)

#include "building.h"
#include "cfd.h"
#include "net_flow.h"
#include "schedule.h"
#include "geometry.h"
#include "prj3dv.h"

      common/getmenu/menutype,igetind(65),igetflux(65)
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      LOGICAL CALPOL,CALLMA
      CHARACTER POLNAM*12
      COMMON/ICFNOD/ICFD,ICP
      COMMON/MFPICK/NMFGET,IMFGETNO(MFRS,8)
      COMMON/MFPK2/NMFLST,IMFLIST(MFCNLST,MFCNEL+1)
      character SMFLABEL*32,GMFLABEL*20,GLMFLABEL*32
      COMMON/MFLABEL/SMFLABEL(MFRS),GMFLABEL(MFRS),GLMFLABEL(MFRS)
      integer  LNSMFLABEL,LNGMFLABEL,LNGLMFLABEL ! length of label strings
      COMMON/LNMFLABEL/LNSMFLABEL(MFRS),LNGMFLABEL(MFRS),
     &                 LNGLMFLABEL(MFRS)
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      COMMON/IGETFLG/IOCUPF,ialstused,IROC
      common/cmftpar/WVEL,ACTL,CLO,iocut,iocuset
      COMMON/GRTOOL/IHFLAG,IDHFLG,ILFLAG
      common/exporttg/xfile,tg,delim
      character xfile*144,tg*1,delim*1
      COMMON/SPAD/MMOD,LIMIT,LIMTTY
      COMMON/OUTIN/IUOUT,IUIN
      COMMON/GRAF1/YMAX(6),YMIN(6),YAXSET(6),ADDLIN,horaxisdiv
      integer YAXSET,ADDLIN,horaxisdiv
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      common/grextras/graftitle,grlbl(10),ngrlbl,lblpx(10),lblpy(10)
      character graftitle*64,grlbl*24      
      COMMON/gzonpik/izgfoc,nzg,nznog(mcom)
      common/recov01/pifltog,lpifl
      character pifltog*4,lpifl*96

      CHARACTER ITEM(35)*23, MTITLE*22, values*20

      logical XST,SELCEL,SELCTM,dok,OKL
      character SUFFIX*6
      CHARACTER outs*124,hold*24
      character ll1*12,ll2*12,ll3*12,t64*64,t24*24
      character zn*12
      integer IZNSEL(MCON),ISFSEL(MCON)
#ifdef OSI
      integer iid1,iid2,iixx,iiyy,iik
#else
      integer*8 iid1,iid2,iixx,iiyy,iik
#endif

C For help messages
      character helpinsub*24   ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='MCFDV'  ! set for cfiles

C We assume that the CFD library has been opened, and a domain selected.
      XST=.true.

C Do we need to select a CFD cell for this metric?
    1 SELCEL=.false.

C Do we need to select a contaminant for this metric?
      SELCTM=.false.

C MNGET is the maximum number of metrics allowed to be chosen from the menu.
      if (MENUTYPE.eq.1) then
        return
      elseif (MENUTYPE.eq.2) then
        MTITLE='Frequency histogram'
        MNGET=1
      elseif (MENUTYPE.eq.3) then
        MTITLE='Time series plot'
        MNGET=MFRS-1
      elseif (MENUTYPE.eq.4) then
        MTITLE='Performance metrics'
        MNGET=13
      elseif (MENUTYPE.eq.5) then
        MTITLE='Summary statistics'
        MNGET=1
      elseif (MENUTYPE.eq.6) then
        MTITLE='Sensitivity'
        MNGET=1
      elseif (MENUTYPE.eq.7) then
        MTITLE='Hrs above query point'
        MNGET=1
      elseif (MENUTYPE.eq.8) then
        MTITLE='Hrs below query point'
        MNGET=1
      else
        MNGET=1
        MTITLE='  '
      endif

C Set up menu items.
      ITEM(1) ='2 result set          '
      ITEM(2) ='3 display period      '
      if(MENUTYPE.eq.1)then
       ITEM(3)='4 output style        '
      elseif(MENUTYPE.eq.2)then
       ITEM(3)='4 output >> histogram '
      elseif(MENUTYPE.eq.3)then
       ITEM(3)='4 output >> graph     '
      elseif(MENUTYPE.eq.4)then
       ITEM(3)='4 output >> tabular   '
      elseif(MENUTYPE.eq.5)then
       ITEM(3)='4 output >> statistics'
      elseif(MENUTYPE.eq.6)then
       ITEM(3)='4 output: sensitivity '
      elseif(MENUTYPE.eq.7)then
       ITEM(3)='4 output >> hours over'
      elseif(MENUTYPE.eq.8)then
       ITEM(3)='4 output >> hrs below '
      endif
      ITEM(4) ='  ------------------- '
      ITEM(5) ='a ambient temperature '
      ITEM(6) ='b wind speed          '
      ITEM(7) ='c wind direction      '
      ITEM(8) ='d ambient RH          '
      ITEM(9) ='e X velocity component'
      ITEM(10)='f Y velocity component'
      ITEM(11)='g Z velocity component'
      ITEM(12)='h velocity magnitude  '
      ITEM(13)='i temperature         '
      if (IEQSV(5).gt.0) then
        ITEM(14)='j mean age of air     '
      else
        ITEM(14)='  MAA not saved       '
      endif
      if (NCTM(ICFD).gt.0) then
        ITEM(15)='k contaminant conc.   '
      else
        ITEM(15)='  contam. not saved   '
      endif
      ITEM(16)='  ------------------- '
      if (NMFGET.eq.0) then
        ITEM(17)='                      '
      else
        ITEM(17)='q edit selections     '
      endif
      if(ixopen.eq.1)then
        ITEM(18)='> display to >> file  '
      elseif(ixopen.eq.0)then
        ITEM(18)='> display to >> screen'
      endif
      if(IROC.eq.0)then
        ITEM(19)='& data: as values     '
      elseif(IROC.eq.1)then
        ITEM(19)='& data: rate of change'
      endif
      if (MENUTYPE.eq.3) then

C If a time series plot clear selections otherwise show state of occupancy filter.
        ITEM(20)='/ clear all selections'
      else
        if(iocut.eq.0)then
          ITEM(20)='+ filter >> none      '
        elseif(iocut.eq.-1)then
          ITEM(20)='+ filter >> time based'
        else
          ITEM(20)='+ filter >> occup only'
        endif
      endif
      if (MENUTYPE.eq.3) then

C If plot indicate scale, otherwise toggle time format.
        ITEM(21)='= set axis scale'
      else
        if(IHFLAG.eq.0)then
          ITEM(21)='* time >> 10h30   '
        elseif(IHFLAG.eq.1)then
          ITEM(21)='* time >> 0.4375  '
        endif
      endif
      if (MENUTYPE.eq.3) then

C If plot show add profile, otherwise toggle delimiter state.
        ITEM(22)='+ add another profile'
      else
        if(delim.eq.'-')then
          ITEM(22)='^ delim >> normal  '
        elseif(delim.eq.'T')then
          ITEM(22)='^ delim >> TAB     '
        elseif(delim.eq.'C')then
          ITEM(22)='^ delim >> comma   '
        elseif(delim.eq.'S')then
          ITEM(22)='^ delim >> space   '
        elseif(delim.eq.'X')then
          ITEM(22)='^ delim >> tagged  '
        endif
      endif
      if(MENUTYPE.eq.3) then

C If plot give graph command and labels command, otherwise list command.
        ITEM(23)  ='@ labels           '
        ITEM(24)  ='! draw graph       '
        nl=2
      elseif(MENUTYPE.eq.4)then
        if(ILFLAG.eq.0)then
          ITEM(23)='@ labels>>short    '
        elseif(ILFLAG.eq.1)then
          ITEM(23)='@ labels>>long     '
        elseif(ILFLAG.eq.2)then
          ITEM(23)='@ labels>>long no #'
        endif
        ITEM(24)  ='! list data        '
        nl=2
      else
        nl=0
      endif
      ITEM(23+nl) ='? help             '
      ITEM(24+nl) ='- exit             '

      NITMS=24+nl
      if(MMOD.eq.8)then
        INO=-1
      else
        INO=-2
      endif

C Instanciate help strings for this menu.
      helptopic='res_CFD_var_menu'
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL EMENU(MTITLE,ITEM,NITMS,INO)

      IF(INO.EQ.1)then
    2   call OPCFD('o',XST,IER)
        if (IER.ne.0) goto 2
        if (.not.XST) then
          INO=-1
          goto 1
        endif
        call RCFDLIBH(0,IER)
        if (IER.ne.0) then
          XST=.false.
          IER=0
          goto 2
        endif
        INO=-1
      elseif(INO.EQ.2)then
        CALL CFDPER(3)
        INO=-1
      elseif(INO.EQ.3)then
        CALL MOSTYLE
        INO=-1

      elseif(INO.EQ.5)then

C Ambient temperature:IMFGET = 9
C Note: overload IMFGETNO(NMFGET,2) so that climate values can
C each have a different line colour.
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=9
        IMFGETNO(NMFGET,2)=9
        IMFGETNO(NMFGET,5)=-302
        IMFGETNO(NMFGET,6)=1
        IMFGETNO(NMFGET,7)=1
        IMFGETNO(NMFGET,8)=0
        GMFLABEL(NMFGET)='Ambient db Tmp'; LNGMFLABEL(NMFGET)=14
        GLMFLABEL(NMFGET)='Ambient dry bulb Temp'
        LNGLMFLABEL(NMFGET)=21
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Ambient db temperature (degC)'
          LNSMFLABEL(NMFGET)=29
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Ambient db temp change (degC/hr)'
          LNSMFLABEL(NMFGET)=32
        endif

C If timestep listing go back to see if any other data should be included.
        if(MENUTYPE.eq.4) goto 1

      elseif(INO.EQ.6)then

C Wind speed: IMFGET = 10
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=10
        IMFGETNO(NMFGET,2)=10
        IMFGETNO(NMFGET,5)=-203
        IMFGETNO(NMFGET,6)=6
        IMFGETNO(NMFGET,7)=6
        IMFGETNO(NMFGET,8)=0
        GMFLABEL(NMFGET)='Wind speed'; LNGMFLABEL(NMFGET)=10
        GLMFLABEL(NMFGET)='Wind speed'; LNGLMFLABEL(NMFGET)=10
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Wind speed (m/sec)'
          LNSMFLABEL(NMFGET)=18
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Wind speed change (m/sec/hr)'
          LNSMFLABEL(NMFGET)=28
        endif

C If timestep listing go back to see if any other data should be included.
        if(MENUTYPE.eq.4) goto 1

      elseif(INO.EQ.7)then

C Wind direction: IMFGET = 11
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=11
        IMFGETNO(NMFGET,2)=11
        IMFGETNO(NMFGET,5)=-203
        IMFGETNO(NMFGET,6)=4
        IMFGETNO(NMFGET,7)=4
        IMFGETNO(NMFGET,8)=0
        GMFLABEL(NMFGET)='Wind dir'; LNGMFLABEL(NMFGET)=8
        GLMFLABEL(NMFGET)='Wind direction'; LNGLMFLABEL(NMFGET)=14
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Wind direction (deg)'
          LNSMFLABEL(NMFGET)=20
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Wind direction change (deg/hr)'
          LNSMFLABEL(NMFGET)=30
        endif

C If timestep listing go back to see if any other data should be included.
        if(MENUTYPE.eq.4) goto 1

      elseif(INO.EQ.8)then

C Ambient RH: IMFGET = 12
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=12
        IMFGETNO(NMFGET,2)=12
        IMFGETNO(NMFGET,5)=-202
        IMFGETNO(NMFGET,6)=7
        IMFGETNO(NMFGET,7)=4
        IMFGETNO(NMFGET,8)=0
        GMFLABEL(NMFGET)='Ambient RH'; LNGMFLABEL(NMFGET)=10
        GLMFLABEL(NMFGET)='Ambient relative humidity'
        LNGLMFLABEL(NMFGET)=25
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Ambient RH (%)'
          LNSMFLABEL(NMFGET)=14
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Ambient RH change (%/hr)'
          LNSMFLABEL(NMFGET)=24
        endif

C If timestep listing go back to see if any other data should be included.
        if(MENUTYPE.eq.4) goto 1

      elseif(INO.EQ.9)then

C U velocity component: IMFGET = 15
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=15
        IMFGETNO(NMFGET,2)=5
        IMFGETNO(NMFGET,3)=ICFD
        IMFGETNO(NMFGET,5)=1
        IMFGETNO(NMFGET,6)=1
        IMFGETNO(NMFGET,7)=6
        IMFGETNO(NMFGET,8)=0
        SUFFIX=' X vel m/s'
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='X velocity (m/s)'
          LNSMFLABEL(NMFGET)=16
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='X velocity change (m/s/hr)'
          LNSMFLABEL(NMFGET)=26
        endif
        SELCEL=.true.

      elseif(INO.EQ.10)then

C V velocity component: IMFGET = 16
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=16
        IMFGETNO(NMFGET,2)=6
        IMFGETNO(NMFGET,3)=ICFD
        IMFGETNO(NMFGET,5)=1
        IMFGETNO(NMFGET,6)=1
        IMFGETNO(NMFGET,7)=6
        IMFGETNO(NMFGET,8)=0
        SUFFIX=' Y vel m/s'
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Y velocity (m/s)'
          LNSMFLABEL(NMFGET)=16
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Y velocity change (m/s/hr)'
          LNSMFLABEL(NMFGET)=26
        endif
        SELCEL=.true.

      elseif(INO.EQ.11)then

C U velocity component: IMFGET = 17
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=17
        IMFGETNO(NMFGET,2)=7
        IMFGETNO(NMFGET,3)=ICFD
        IMFGETNO(NMFGET,5)=1
        IMFGETNO(NMFGET,6)=1
        IMFGETNO(NMFGET,7)=6
        IMFGETNO(NMFGET,8)=0
        SUFFIX=' Z vel m/s'
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Z velocity (m/s)'
          LNSMFLABEL(NMFGET)=16
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Z velocity change (m/s/hr)'
          LNSMFLABEL(NMFGET)=26
        endif
        SELCEL=.true.

      elseif(INO.EQ.12)then

C Velocity magnitude: IMFGET = 18
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=18
        IMFGETNO(NMFGET,2)=8
        IMFGETNO(NMFGET,3)=ICFD
        IMFGETNO(NMFGET,5)=1
        IMFGETNO(NMFGET,6)=1
        IMFGETNO(NMFGET,7)=6
        IMFGETNO(NMFGET,8)=0
        SUFFIX=' vel m m/s'
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Velocity (m/s)'
          LNSMFLABEL(NMFGET)=14
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Velocity change (m/s/hr)'
          LNSMFLABEL(NMFGET)=24
        endif
        SELCEL=.true.

      elseif(INO.EQ.13)then

C Temperature: IMFGET = 19
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=19
        IMFGETNO(NMFGET,2)=13
        IMFGETNO(NMFGET,3)=ICFD
        IMFGETNO(NMFGET,5)=1
        IMFGETNO(NMFGET,6)=1
        IMFGETNO(NMFGET,7)=1
        IMFGETNO(NMFGET,8)=0
        SUFFIX=' temp degC'
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Temperature (degC)'
          LNSMFLABEL(NMFGET)=14
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Temperature change (degC/hr)'
          LNSMFLABEL(NMFGET)=28
        endif
        SELCEL=.true.

      elseif(INO.EQ.14)then

        if (IEQSV(5).eq.0) goto 1

C Mean age of air: IMFGET = 20
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=20        
        IMFGETNO(NMFGET,2)=14
        IMFGETNO(NMFGET,3)=ICFD
        IMFGETNO(NMFGET,5)=1
        IMFGETNO(NMFGET,6)=1
        IMFGETNO(NMFGET,7)=4
        IMFGETNO(NMFGET,8)=0
        SUFFIX=' MAA secs'
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Mean age of air (s)'
          LNSMFLABEL(NMFGET)=19
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Mean age of air change (s)'
          LNSMFLABEL(NMFGET)=26
        endif
        SELCEL=.true.

      elseif(INO.EQ.15)then

        if (NCTM(ICFD).eq.0) goto 1

C Contaminant concentration: IMFGET = 21
        if((NMFGET+1).GT.MFRS)then
          call usrmsg('Number of items exceeded.',' ','P')
          goto 1
        endif
        NMFGET=NMFGET+1
        IMFGETNO(NMFGET,1)=21
        IMFGETNO(NMFGET,3)=ICFD
        IMFGETNO(NMFGET,5)=1
        IMFGETNO(NMFGET,6)=1
        IMFGETNO(NMFGET,7)=5
        IMFGETNO(NMFGET,8)=0
        if(IROC.eq.0)then
          SMFLABEL(NMFGET)='Contaminant conc (g/kg)'
          LNSMFLABEL(NMFGET)=23
        elseif(IROC.eq.1)then
          SMFLABEL(NMFGET)='Contam conc change (g/kg)'
          LNSMFLABEL(NMFGET)=25
        endif
        SELCEL=.true.
        SELCTM=.true.

      elseif(INO.EQ.17)then

C Edit selections.
        if(NMFGET.gt.0)then
          call EDMFGOGET
          goto 1
        endif

      elseif(INO.EQ.18)then

C Output redirect to graphics, tabular listing or a text report.
        if(MENUTYPE.eq.3)then
          call ctlexp(xfile,ixopen,ixunit,ixpunit,'G','Graphics',IER)
        elseif(MENUTYPE.eq.4)then
          call ctlexp(xfile,ixopen,ixunit,ixpunit,'X','Tabular',IER)
        else
          call ctlexp(xfile,ixopen,ixunit,ixpunit,'T','Text',IER)
        endif

C If user request cancel reset ixopen to zero.
        if(ier.eq.-3) ixopen=0
        goto 1

      elseif(INO.EQ.19)then

C Data presentation format (actual or rate of change).
        call usrmsg('Switch from raw data to rate of change or data',
     &    'does not (yet) apply to CFD data ... ignored.','W')

C << rate of change needs to be implemented >>
C        if(IROC.eq.0)then
C          write(outs,'(a)')'(currently variable)'
C        else
C          write(outs,'(a)')'(currently rate of change)'
C        endif
C        helptopic='res_mass_flow_menu'
C        call gethelptext(helpinsub,helptopic,nbhelp)
C        CALL EASKABC('Data display options:',outs,
C     &    'variable (default)','rate of change of variable','contine',
C     &    II,nbhelp)
C        if(II.ne.3.and.(IROC.ne.(II-1)))then
C          IROC=II-1
C          call usrmsg ('Clearing current selections....','  ','W')
C          call MOFLOWSU
C        endif

      elseif(INO.EQ.20)then

C Clear all selections (time series plot).
        if(MENUTYPE.eq.3) then
          call MOFLOWSU
        else

C Occupancy filter (all types bar time series plot).
C Occupancy filter for flow results requires the user to specify>>
C a thermal zone.
          if(iocupf.eq.0)then

C Casual gain type.  Ask user which constitutes occupancy.
C Labels for loads, use first zones labels as examples. If time
C based set iocut=-1, if always occup set iocut=0.

C << revise for additional casual gain types >>
            write(ll1,'(a)') lodlabel(1,1)
            write(ll2,'(a)') lodlabel(1,2)
            write(ll3,'(a)') lodlabel(1,3)
            call edisp(iuout,' ')
            helptopic='res_casual_gain_type'
            call gethelptext(helpinsub,helptopic,nbhelp)
 937        IOT=1
            CALL EASKATOG('Which casual gain represents occupancy:',
     &         '  ','Always occupied',ll1,ll2,ll3,'time',' ',' ',IOT,
     &         nbhelp)
            if (IOT.gt.1.and.IOT.lt.5) then
              iocupf=1      
              iocut=IOT-1
            elseif(IOT.eq.5)then
              iocupf=1
              iocut=-1
              write(HOLD,'(a)') '  0  24  '
              CALL EASKS(HOLD,' ',' Weekday occupancy period: ',
     &         24,' 0  24 ','wkd occup period',IER,nbhelp)
              K=0
              CALL EGETWI(HOLD,K,iwkdst,0,24,'W','iwkdst',IER)
              CALL EGETWI(HOLD,K,iwkdfn,iwkdst,24,'W','iwkdst',IER)

              write(HOLD,'(a)') '  0  24  '
              CALL EASKS(HOLD,' ',
     &         ' First weekend day occupancy period: ',24,' 0  24 ',
     &         'sat occup period',IER,nbhelp)
              K=0
              CALL EGETWI(HOLD,K,isatst,0,24,'W','isatst',IER)
              CALL EGETWI(HOLD,K,isatfn,isatst,24,'W','isatst',IER)

              write(HOLD,'(a)') '  0  24  '
              CALL EASKS(HOLD,' ',
     &         ' Second weekend day occupancy period: ',24,' 0  24 ',
     &         'sun occup period',IER,nbhelp)
              K=0
              CALL EGETWI(HOLD,K,isunst,0,24,'W','isunst',IER)
              CALL EGETWI(HOLD,K,isunfn,isunst,24,'W','isunst',IER)
            elseif(IOT.eq.1)then
              iocpzone=0
              iocupf=0
              iocut=0
            endif
          else
            iocupf=0
            iocut=0
            iocpzone=0
          endif

C Get occupancy pattern from the zone associated with the CFD domain.
          if(iocut.gt.0) iocpzone=ICP
          goto 1
        endif

      elseif(INO.EQ.21)then
        if(MENUTYPE.eq.3) then

C Set axis limits for the current plot.
          helptopic='res_flow_axis_range'
          call gethelptext(helpinsub,helptopic,nbhelp)
          do 555 Iaxis=1,6
            if (YAXSET(Iaxis).gt.0) then
 501          write (values,'(8(f9.2,1x))') YMIN(Iaxis),YMAX(Iaxis)
              if (Iaxis.eq.1) then
                call EASKS(values,'  ','Min &max for temperature axis?',
     &            20,' -10 30 ','temp axis',IER,nbhelp)
              elseif (Iaxis.eq.2) then
                call EASKS(values,'  ','Min & max for pressure axis?',
     &            20,' -100 100 ','pres axis',IER,nbhelp)
              elseif (Iaxis.eq.3) then
                call EASKS(values,'  ','Min & max for flow rate axis?',
     &            20,' 0 1000 ','flow axis',IER,nbhelp)
              elseif (Iaxis.eq.4) then
                call EASKS(values,'  ','Min & max for misc axis?',
     &            20,' 0 100 ','misc axis',IER,nbhelp)
              elseif (Iaxis.eq.5) then
                call EASKS(values,'  ','Min&max for concntration axis?',
     &            20,' 0 0.25 ','conc axis',IER,nbhelp)
              elseif (Iaxis.eq.6) then
                call EASKS(values,'  ','Min & max for velocity axis?',
     &            20,' 0 0.5 ','vel axis',IER,nbhelp)
              endif
              K=0
              CALL EGETWR(values,K,YN,0.,0.,'-','Y min',IER)
              CALL EGETWR(values,K,YM,0.,0.,'-','Y max',IER)
              if (YN.gt.YM) then
                call usrmsg('Maximum value is less than minimum value.',
     &                    ' Please respecify. ','W')
                goto 501
              endif
              YMAX(Iaxis)=YM
              YMIN(Iaxis)=YN
              YAXSET(Iaxis)=2
            endif
 555      continue

C Ask user preference for horizontal axis.
          CALL EASKABCD('Horizontal axis divisions:',' ','default',
     &      'hours','days','weeks',horaxisdiv,nbhelp)

C Redraw graph (if applicable).
          if (NMFGET.gt.0) then
            if (YAXSET(1).eq.2.or.YAXSET(2).eq.2.or.YAXSET(3).eq.2.or.
     &          YAXSET(4).eq.2.or.YAXSET(5).eq.2) return
          endif
        else

C Toggle time format and day separators for all other display formats.
          IHFLAG=IHFLAG+1
          if(IHFLAG.GT.1)IHFLAG=0
          if(IHFLAG.eq.0)then
            call edisp(iuout,' ')
            call edisp(iuout,'standard display time = 10h30 set ')
          elseif(IHFLAG.eq.1)then
            call edisp(iuout,' ')
            call edisp(iuout,'3rd party graphing time=day fraction set')
          endif
          helptopic='res_flow_hash_separator'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL EASKAB('Include mark between days when ',
     &      'displaying or writing data :','no','yes',IDH,nbhelp)
          IDHFLG=IDH-1
          goto 1
        endif

      elseif(INO.EQ.22)then
        if(MENUTYPE.eq.3) then

C Add another selection - time series plot only.
          INO=-4
          ADDLIN=1
        else

C Toggle delimeter.
          helptopic='res_flow_delimeter'
          call gethelptext(helpinsub,helptopic,nbhelp)
          IWM=1
          CALL EASKATOG('Delimeter to use between columns of data:',' ',
     &      'normal spaces','single space','tab','comma','tagged',
     &      'continue',' ',IWM,nbhelp)
          if(iwm.eq.1)then
            delim = '-'
          elseif(iwm.eq.2)then
            delim = 'S'
          elseif(iwm.eq.3)then
            delim = 'T'
          elseif(iwm.eq.4)then
            delim = 'C'
          elseif(iwm.eq.5)then
            delim = 'X'
          endif
          goto 1
        endif

      elseif(INO.eq.23)then
        if(MENUTYPE.eq.3) then

C Control labels if graphic plot.
C Add in a title for this graph 64 char string graftitle.
 290      helptopic='res_mass_graph_title'
          call gethelptext(helpinsub,helptopic,nbhelp)
          t64=graftitle
          CALL EASKS(t64,' Title for graph ? ',
     &     ' ',64,'  ','graph title',IER,nbhelp)
          if(t64(1:2).ne.'  '.and.t64(1:4).ne.'UNKN')then
            graftitle=t64
          else
            goto 290
          endif

C Add or clear labels within a graph.
          if(ngrlbl.eq.0)then
            helptopic='res_mass_graph_title'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKAB('Labels within graph options:',' ',
     &        'edit/add labels','contine',igopt,nbhelp)
            if(igopt.eq.1)then
              igrl=0
              grlbl(1)=' '; grlbl(2)=' '; grlbl(3)=' '; grlbl(4)=' '
              grlbl(5)=' '; grlbl(6)=' '; grlbl(7)=' '; grlbl(8)=' '
              grlbl(9)=' '; grlbl(10)=' '
            elseif(igopt.eq.2)then
              goto 1
            endif
          elseif(ngrlbl.gt.0)then
            helptopic='res_mass_graph_title'
            call gethelptext(helpinsub,helptopic,nbhelp)
            CALL EASKABC('Labels within graph options:',' ',
     &        'clear existing labels','edit/add labels','contine',
     &        igopt,nbhelp)
            if(igopt.eq.1)then
              ngrlbl=0
              igrl=0
              grlbl(1)=' '; grlbl(2)=' '; grlbl(3)=' '; grlbl(4)=' '
              grlbl(5)=' '; grlbl(6)=' '; grlbl(7)=' '; grlbl(8)=' '
              grlbl(9)=' '; grlbl(10)=' '
              goto 1
            elseif(igopt.eq.2)then
              igrl=0   ! reset which one we start editing at
            elseif(igopt.eq.3)then
              goto 1
            endif
          endif
 291      continue

C If there are no labels or some label start at first, if more than 9 exit.
          if(ngrlbl.eq.0)then
            ngrlbl=1
            igrl=1
          elseif(ngrlbl.gt.9)then
            call usrmsg('Array of labels is full.',' ','W')
            goto 1
          else
            igrl=igrl+1
          endif
          helptopic='mass_graph_edit_label'
          call gethelptext(helpinsub,helptopic,nbhelp)
          t24=grlbl(igrl)
 292      CALL EASKS(t24,
     &     'Label within graph (AFTER editing select a point on graph',
     &     'for the label position).',24,'x','graph label',IER,nbhelp)
          if(t24(1:2).ne.'  '.and.t24(1:4).ne.'UNKN')then
            grlbl(igrl)=t24
            if(igrl.gt.ngrlbl)ngrlbl=igrl
          else
            goto 292
          endif

C Find out point where text should be drawn.
          CALL trackview(iik,iixx,iiyy)
          lblpx(igrl)=int(iixx)
          lblpy(igrl)=int(iiyy)
          iid1=int(iixx); iid2=int(iiyy);
          t24=grlbl(igrl)
          call wstxpt(iid1,iid2,t24)
          dok=.true.
          CALL ASKOK('Include another user supplied label?',' ',
     &      OKL,dok,nbhelp)
          if(OKL) goto 291

        elseif(MENUTYPE.eq.4) then

C Toggle tabular listing label format.
          ILFLAG=ILFLAG+1
          if(ILFLAG.GT.2)ILFLAG=0
          if(ILFLAG.eq.0)then
            call edisp(iuout,' ')
            call edisp(iuout,'column labels on multi-lines ')
          elseif(ILFLAG.eq.1)then
            call edisp(iuout,' ')
            call edisp(iuout,'one line column labels (spreadsheets)')
          elseif(ILFLAG.eq.2)then
            call edisp(iuout,' ')
            call edisp(iuout,
     &      'column labels on one line & no # header lines in files')
          endif
          goto 1   ! jump back to the menu
        else
          call edisp(iuout,'selected item 26?? ')
        endif

      elseif(INO.EQ.24.and.(MENUTYPE.eq.3.or.MENUTYPE.eq.4))then

C List out data to text feedback or to file or to graphics.

C Debug.
C        do 999 I=1,NMFGET
C          write(6,*) (IMFGETNO(I,K),K=1,8)
C          write(6,*) SMFLABEL(I)
C          write(6,*) GMFLABEL(I)
C          write(6,*) GLMFLABEL(I)
C          write(6,*) '  '
C 999    continue

        if (NMFGET.lt.1) then
          call usrmsg('No data to list/ plot!',
     &                'Please choose a metric.','W')
          INO=-3
        else

C Write GOGET info to file (so that current data recovery can be
C replicated).
          if(pifltog(1:3).eq.'ON ')call WRITEPIF(MENUTYPE)
          return
        endif

C At this point we just fall past the end of the INO equals statements.

      elseif(INO.EQ.(NITMS-1))then
        helptopic='res_CFD_var_menu'
        call gethelptext(helpinsub,helptopic,nbhelp)
        CALL PHELPD(MTITLE,nbhelp,'-',0,0,IER)
        goto 1

      elseif(INO.EQ.NITMS)then
        call usrmsg('  ','  ','-')
        idone=1
        return
      else

C Did not understand menu item or it was a non-action menu item.
        goto 1
      endif

C Most of the above logic falls through to this point. If an item
C has been selected proceed otherwise loop back to '1'.
      if (INO.lt.0.or.NMFGET.lt.1) goto 1

C Select a CFD cell if needed.
      if (SELCEL) then

C Ask user how they want to specify a cell - MRT sensor, room COG, or
C pick one manually.
  346   iopt=1
        call EASKABC(' ','location: ','pick cell','MRT sensor',
     &    'room COG',iopt,nbhelp)

        NMFLST=NMFLST+1
        IMFGETNO(NMFGET,4)=NMFLST

        if (iopt.eq.1) then

C If in graphic mode, display grid to aid user in picking a cell.
          if (MMOD.eq.8) then
            call INIT_CFDTRANS(IER)
            call VGRID3D('zn ','xyz',ICP)
          endif

          ilayer=IMFLIST(NMFLST,1)
          if (ilayer.lt.1.or.ilayer.gt.NIM2) ilayer=1
          CALL EASKI(ilayer,'  ',' Which i layer ',
     &        1,'F',NIM2,'F',1,' Slice number',IER,nbhelp)
          IMFLIST(NMFLST,1)=ilayer

          jlayer=IMFLIST(NMFLST,2)
          if (jlayer.lt.1.or.jlayer.gt.NJM2) jlayer=1
          CALL EASKI(jlayer,'  ',' Which j layer ',
     &        1,'F',NJM2,'F',1,' Slice number',IER,nbhelp)
          IMFLIST(NMFLST,2)=jlayer

          klayer=IMFLIST(NMFLST,3)
          if (klayer.lt.1.or.klayer.gt.NKM2) klayer=1
          CALL EASKI(klayer,'  ',' Which k layer ',
     &        1,'F',NKM2,'F',1,' Slice number',IER,nbhelp)
          IMFLIST(NMFLST,3)=klayer

        elseif (iopt.eq.2) then

          ilayer=1
          jlayer=1
          klayer=1

C Find CFD grid cell closest to an MRT sensor.
          NSEL=1
          CALL PIKMRTS(IZNSEL,ISFSEL,NSEL,ICP,IER)
          IMRT=ISFSEL(1)
          CALL INIT_CFDTRANS(IER)
          if (IER.eq.0) then
            CALL GRIDGEO
            PI = 4.0 * ATAN(1.0)
            R=PI/180.
            SA=SIN(CANG(IMRT)*R)
            CA=COS(CANG(IMRT)*R)
            DX=(DXC(IMRT)/2.)*CA-(DYC(IMRT)/2.)*SA
            DY=(DXC(IMRT)/2.)*SA+(DYC(IMRT)/2.)*CA
            CALL CFDTRANS(2,XOC(IMRT)+DX,YOC(IMRT)+DY,
     &                      ZOC(IMRT)+DZC(IMRT)/2,XC,YC,ZC,IER)
            CALL FDCFDPT(XC,YC,ZC,ilayer,jlayer,klayer,IER)
            ilayer=ilayer-1
            jlayer=jlayer-1
            klayer=klayer-1
            if (IER.ne.0) then
              call edisp(iuout,' Problem finding CFD point')
            else
            endif
          endif
          IMFLIST(NMFLST,1)=ilayer
          IMFLIST(NMFLST,2)=jlayer
          IMFLIST(NMFLST,3)=klayer

        elseif (iopt.eq.3) then

          ilayer=1
          jlayer=1
          klayer=1

C Find CFD grid cell closest to room COG.
          nzg=1
          nznog(1)=ICP
          ITBND=1
          CALL BNDOBJ(0,IER)
          if (IER.ne.0) then
            call edisp(iuout,' Problem calculating zone COG')
          endif
          CALL INIT_CFDTRANS(IER)
          if (IER.eq.0) then
C Find CFD plane closest to selected Z height.
            CALL GRIDGEO
            CALL CFDTRANS(2,ZCOG(ICP,1),ZCOG(ICP,2),ZCOG(ICP,3),
     &        XC,YC,ZC,IER)
            CALL FDCFDPT(XC,YC,ZC,ilayer,jlayer,klayer,IER)
            ilayer=ilayer-1
            jlayer=jlayer-1
            klayer=klayer-1
            if (IER.ne.0) then
              call edisp(iuout,' Problem finding CFD point')
            endif
          endif
          IMFLIST(NMFLST,1)=ilayer
          IMFLIST(NMFLST,2)=jlayer
          IMFLIST(NMFLST,3)=klayer

        endif

C If in graphic mode, highlight the cell and ask user to confirm this is
C the cell they want.
        if (MMOD.eq.8) then

C If MRT sensor or COG, refresh display to avoid showing previous cell
C highlights. Also decrement ?layer, because outputs from FDCFDPT do
C consider additional 
          if (iopt.eq.2.or.iopt.eq.3) then
            call INIT_CFDTRANS(IER)
            call VGRID3D('zn ','xyz',ICP)
          endif

          call V1CEL3D(ilayer,jlayer,klayer)
          dok=.true.
          CALL ASKOK('Is this the cell you want?',' ',OKL,dok,nbhelp)
          if (.not.OKL) goto 346
        endif

C Assemble label.
        if (.not.SELCTM) then
          write(GMFLABEL(NMFGET),'(I2.2,a,I2.2,a,I2.2,a)')
     &      IMFLIST(NMFLST,1),'-',IMFLIST(NMFLST,2),'-',
     &      IMFLIST(NMFLST,3),SUFFIX
          LNGMFLABEL(NMFGET)=lnblnk(GMFLABEL(NMFGET))
          GLMFLABEL=GMFLABEL
          LNGLMFLABEL(NMFGET)=LNGMFLABEL(NMFGET)
        endif

      endif

C Select contaminant if needed.
      if (SELCTM) then
        CALL ASKCFDCTM(ICTM)
        IMFGETNO(NMFGET,2)=ICTM

        if (SELCEL) then
          write(GMFLABEL(NMFGET),'(I2.2,a,I2.2,a,I2.2,1x,a)')
     &      IMFLIST(NMFLST,1),'-',IMFLIST(NMFLST,2),'-',
     &      IMFLIST(NMFLST,3),POLNAM(ICTM,ICFD)(1:11)
          LNGMFLABEL(NMFGET)=lnblnk(GMFLABEL(NMFGET))
          GLMFLABEL=GMFLABEL
          LNGLMFLABEL(NMFGET)=LNGMFLABEL(NMFGET)
        else
          write(GMFLABEL(NMFGET),'(I2.2,a,I2.2,a,I2.2,1x,a)')
     &      POLNAM(ICTM,ICFD)(1:12)
          LNGMFLABEL(NMFGET)=lnblnk(GMFLABEL(NMFGET))
          GLMFLABEL=GMFLABEL
          LNGLMFLABEL(NMFGET)=LNGMFLABEL(NMFGET)          
        endif

      endif

C If timestep listing go back to see if any other data should be included.
      if(MENUTYPE.eq.4) goto 1

      RETURN
      END

C ******************** GOCFDGET ********************
C GOCFDGET retrieves data from CFD results library according to common
C IMFGETNO. If ICFTS +ve then get results at this time step, if not use
C first time step in library.
C IDAY, IHR and ITS are only used for retrieving climate metrics.

C Only valid for CFD results library versions 3+.

C Assumes that CFD results library is open.

C CFD metrics (for mass flow metrics, see mfget.F):
C IMFGET = 15 U velocity component
C IMFGET = 16 V velocity component
C IMFGET = 17 W velocity component
C IMFGET = 18 Velocity magnitude
C IMFGET = 19 Temperature
C IMFGET = 20 Mean age of air
C IMFGET = 21 Contaminant concentration

C IMFGETNO contains all the information needed for specifying which data to recover
C IMFGETNO(*,1): The IMFGET number - metric identifier
C IMFGETNO(*,2): For contaminant concentration, selected contaminant
C IMFGETNO(*,3): Domain number
C IMFGETNO(*,4): LISTID for IMFLIST
C IMFGETNO(*,5): Line type
C IMFGETNO(*,6): Symbol index
C IMFGETNO(*,7): Associated axis 1=temperature, 
C                                2=pressure,
C                                3=mass flow rate
C                                4=misc, 
C                                5=concentration g/kg,
C                                6=velocity m/s
C IMFGETNO(*,8): 0 = standard units 1 alternative units e.g. l/s rather than m^3/s

C IMFLIST - selected cell to report:
C   IMFLIST(LISTID,1) = X cell
C   IMFLIST(LISTID,2) = Y cell
C   IMFLIST(LISTID,3) = Z cell

      SUBROUTINE GOCFDGET(ICFTS,IDAY,IHR,ITS,IER)

#include "building.h"
#include "cfd.h"
#include "net_flow.h"

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/FILEP/IFIL
      COMMON/cfdsmper/ICDYS,ICDYF,CFTS,CFTF
      COMMON/cfdotper/ICDYOS,ICDYOF,CFTOS,CFTOF
      COMMON/cfdhsh/NCFDSZ,NRCFDOM(MNZ)
      COMMON/cfddoms/NCDOM,ICFDZ(MNZ)
      common/CFDSV/IRECPC,ICFDSV,IEQSV(5+MCTM)
      COMMON/ICFNOD/ICFD,ICP
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      COMMON/MFPICK/NMFGET,IMFGETNO(MFRS,8)
      COMMON/MFGET/FLOWVALS(MFRS),MFRECVR(MCNN+2+MCONTM)
      COMMON/MFPK2/NMFLST,IMFLIST(MFCNLST,MFCNEL+1)
      COMMON/ALL/NI,NJ,NK,NIM1,NJM1,NKM1,NIM2,NJM2,NKM2
      common/EQTION3/CALLMA(MNZ),CALPOL(MCTM,MNZ),POLNAM(MCTM,MNZ),
     &               NCTM(MNZ),JHUMINDX(MNZ),URFC(MCTM)
      character POLNAM*12

      real vals,Usqrd,Vsqrd,Wsqrd,val
      DIMENSION vals(NIM1)

      iunit=ifil+14

C Check library version.
      if (ICFDSV.lt.3) then
        call EDISP(iuout,'GOCFDGET: legacy CFD results library.')
        IER=1
        RETURN
      endif

C Calculate number of time steps into CFD results lib.
      if (ICFTS.gt.0) then
        IHTS=ICFTS
      else
        IHTS=int((float(ICDYOS-ICDYS)*24.0+CFTOS-CFTS)*float(NTS))+1
      endif

C Read data from res file.
      if (ICFDSV.eq.1.or.ICFDSV.eq.2) then
        ISTREC=5+((MCOM-1)*4)+ NCFDSZ*(IHTS-1)+NCTM(ICFD)
      elseif (ICFDSV.eq.3) then
        ISTREC=4+(NCDOM*8)+ NCFDSZ*(IHTS-1)
      endif
      if (ICFD.gt.1) then
        do 10 I=1,ICFD-1
          ISTREC=ISTREC+NRCFDOM(I)
 10     continue
      endif

C TODO: Assume that we only have results from 1 domain selected.
C Otherwise, need to add domain dimension to IEQSV.

      NEQ=0
      do I=1,5+NCTM(ICFD)
        if (IEQSV(I).eq.1) NEQ=NEQ+1
      enddo

      do IGET=1,NMFGET

        ixcell=IMFLIST(IMFGETNO(IGET,4),1)
        iycell=IMFLIST(IMFGETNO(IGET,4),2)
        izcell=IMFLIST(IMFGETNO(IGET,4),3)

C Calculate record for correct x row.
        IREC=ISTREC+((izcell-1)*NJM1+(iycell-1))*NEQ

C U velocity component.
C Velocity grid is offset from scalar grid, so interpolate data.
        if (IMFGETNO(IGET,1).eq.15) then
          if (IEQSV(1).lt.1) then
            call EDISP(iuout,'U velocity not available in results.')
            continue
          endif
          IREC=IREC+1
          read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)(vals(I),I=1,NIM1)
          FLOWVALS(IGET)=0.5*(vals(ixcell)+vals(ixcell+1))

C V velocity component.
C Velocity grid is offset from scalar grid, so interpolate data.
        elseif (IMFGETNO(IGET,1).eq.16) then
          if (IEQSV(2).lt.1) then
            call EDISP(iuout,'V velocity not available in results.')
            continue
          endif
          IREC=IREC+2
          read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)(vals(I),I=1,NIM1)
          rtmp=vals(ixcell)
          IREC=IREC+NEQ
          read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)(vals(I),I=1,NIM1)
          FLOWVALS(IGET)=0.5*(rtmp+vals(ixcell))

C W velocity component.
C Velocity grid is offset from scalar grid, so interpolate data.
        elseif (IMFGETNO(IGET,1).eq.17) then
          if (IEQSV(3).lt.1) then
            call EDISP(iuout,'W velocity not available in results.')
            continue
          endif
          IREC=IREC+3
          read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)(vals(I),I=1,NIM1)
          rtmp=vals(ixcell)
          IREC=IREC+NEQ*NJM1
          read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)(vals(I),I=1,NIM1)
          FLOWVALS(IGET)=0.5*(rtmp+vals(ixcell))

C Velocity magnitude.
C Velocity grid is offset from scalar grid, so interpolate data.
        elseif (IMFGETNO(IGET,1).eq.18) then
          if (IEQSV(1).lt.1.and.IEQSV(2).lt.1.and.IEQSV(3).lt.1) then
            call EDISP(iuout,'Velocity not available in results.')
            continue
          endif
          IRECtmp=IREC
          IREC=IRECtmp+1
          read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)(vals(I),I=1,NIM1)
          Usqrd=(0.5*(vals(ixcell)+vals(ixcell+1)))**2

          IREC=IRECtmp+2
          read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)(vals(I),I=1,NIM1)
          rtmp=vals(ixcell)
          IREC=IREC+NEQ
          read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)(vals(I),I=1,NIM1)
          Vsqrd=(0.5*(rtmp+vals(ixcell)))**2

          IREC=IRECtmp+3
          read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)(vals(I),I=1,NIM1)
          rtmp=vals(ixcell)
          IREC=IREC+NEQ*NJM1
          read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)(vals(I),I=1,NIM1)
          Wsqrd=(0.5*(rtmp+vals(ixcell)))**2

          FLOWVALS(IGET)=SQRT(Usqrd+Vsqrd+Wsqrd)

C Temperature.
        elseif (IMFGETNO(IGET,1).eq.19) then
          if (IEQSV(4).lt.1) then
            call EDISP(iuout,'Temperature not available in results.')
            continue
          endif
          IREC=IREC+4
          read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)(vals(I),I=1,NIM1)
          FLOWVALS(IGET)=vals(ixcell)

C Mean age of air.
        elseif (IMFGETNO(IGET,1).eq.20) then
          if (IEQSV(5).lt.1) then
            call EDISP
     &        (iuout,'Mean age of air not available in results.')
            continue
          endif
          IREC=IREC+5
          read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)(vals(I),I=1,NIM1)
          FLOWVALS(IGET)=vals(ixcell)

C Contaminant.
        elseif (IMFGETNO(IGET,1).eq.21) then
          ICTM=IMFGETNO(IGET,2)
          if (IEQSV(5+ICTM).lt.1) then
            call EDISP
     &        (iuout,'Selected contaminant not available in results.')
            continue
          endif
          IREC=IREC+5+ICTM
          read(iunit,REC=IREC,IOSTAT=ISTAT,ERR=1000)(vals(I),I=1,NIM1)
          FLOWVALS(IGET)=vals(ixcell)*1000.0 ! convert kg/kg to g/kg

C Copied from GOMFGET.
        elseif (IMFGETNO(IGET,1).eq.9) then

C Ambient temperature.
          IPAR=1
          call MFCLIM(IDAY,IHR,ITS,IPAR,VAL)
          FLOWVALS(IGET)=VAL

        elseif (IMFGETNO(IGET,1).eq.10) then

C Ambient wind speed.
          IPAR=2
          call MFCLIM(IDAY,IHR,ITS,IPAR,VAL)
          FLOWVALS(IGET)=VAL

        elseif (IMFGETNO(IGET,1).eq.11) then

C Ambient wind direction.
          IPAR=3
          call MFCLIM(IDAY,IHR,ITS,IPAR,VAL)
          FLOWVALS(IGET)=VAL

        elseif (IMFGETNO(IGET,1).eq.12) then

C Ambient RH.
          IPAR=4
          call MFCLIM(IDAY,IHR,ITS,IPAR,VAL)
          FLOWVALS(IGET)=VAL


        endif
      enddo

      RETURN

 1000 call edisp(iuout,'CFD read data fault. Cannot read file.')
      call erpfree(iunit,istat)
      return

      END




