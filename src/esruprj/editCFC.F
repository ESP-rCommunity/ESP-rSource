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

C This file contains the following routines:

C  makeCFCfile          Creates a *.cfc file from imported GSLedit data.

C  importGSLedit        Imports data from *.GSL files and stores in CFC
C                       import commons

C  read_in_cfc_file     Reads an annotated ASCII *.cfc file and stores
C                       CFC data in commons

C  editCFC_shading_schedule
C                       Facility for editing CFC shading schedule and 
C                       control. Launched from zone operations menu. 

C  askCFCtype           Presents list of CFC types in zone and asks user
C                       to select a CFC type for editing shading schedule.

C  plot_CFC_schedules   Plots CFC shading schedule

C  editCFC_period_data  Edit CFC shading schedule period data

C  addCFC_period_data   Add CFC shading schedule period data in correct
C                       location

C  chkCFC_period_data   Check CFC shading period data for overlapping periods

C  cpdelCFC_period_data Copy/Delete CFC shading period data


C ********************************************************************
C                        --makeCFCfile--
C
C Sets up importing of GSLedit data from *.GSL files and creates
C an annotated ASCII *.cfc file for each zone.
C
C For details on assembling *.GSL files and *.cfc file data 
C structure, refer to Appendix F in:
C Lomanowski, B.A. (2008) 'Implementation of Window Shading Models
C into Dynamic Whole-Building Simulation', MASc Thesis, University
C of Waterloo.
C
C ********************************************************************
      subroutine makeCFCfile(icomp,ier)
#include "building.h"
#include "model.h"
#include "geometry.h"
#include "CFC_common.h"
      COMMON/OUTIN/IUOUT,IUIN
      integer iuout,iuin
      COMMON/FILEP/IFIL
      integer ifil

      COMMON/C24/IZSTOCN(MCOM,MS) !array which holds the connection index
      integer izstocn

      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      integer ne,nairg,ipairg
      real rairg

      integer IER,IS,icomp,icn1,ncfccount,igap,ie
      integer nmaxtypes,ntypes,icfctp,isame,iss,ncfctypes,i,icount
      integer ij,it,icompare1,icompare2,icn2,ibk,iwa,nmax,itrunc,ipos
      integer IFU,ival,itype,istat,loutlen,ln_outs,ln_zonepth
      integer lnblnk    !function definition
      CHARACTER outs*124
      CHARACTER con_name*32,ltmp*72,louts*248

      DIMENSION con_name(ms)        !local storage of CFC const. name 
      dimension ival(MS)

      LOGICAL XST,vb_xst

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='makeCFCfile'  ! set for subroutine

      vb_xst=.false.
      IER=0
      IFU=IFIL+2

C Ask user to import GSLedit files or continue
      write(outs,'(a,a)') 'Import GSLedit files to ',
     &                      lcfcin(icomp)
      helptopic='CFC_creation_overview'
      call gethelptext(helpinsub,helptopic,nbhelp)
      CALL EASKAB(outs,'Options:',
     &     'Import GSLedit files','continue',IWA,nbhelp)

C Import GSLedit files selected
      IF(IWA.EQ.1)THEN

C 1. scan G6 common and check that CFCs are VERT and EXTERIOR
      ncfccount=0
      DO 10 IS=1,NZSUR(icomp)
        icn1=izstocn(icomp,is)
        IF (SSOTF(icn1)(1:4).eq.'CFC ') then
c           if(SSVFC(icn1)(1:4).eq.'VERT'.and.
           if(
     &          SSOTHER(icn1,1)(1:5).eq.'EXTER')then
				ncfccount=ncfccount+1   !count CFCs in zone
				con_name(is)=SSMLCN(icn1)

C 2.  scan constructions layers and check that there are air gaps
C between each glazing/shading layer
                igap=0
                do 15 ie=2,ne(is),2
                  igap=igap+1
                  if (ie.ne.IPAIRG(IS,igap)) then
                    CALL USRMSG(
     &             ' CFCs must have alternating solid and gas',
     &             ' gap layers. Check constructions and try again.',
     &              'W')
                    IER=1
                    return
				  endif
 15             continue

            else
                  CALL USRMSG(
     &            ' CFC must be (VERT) and EXTERIOR',
     &            ' please update geometry and try again.','W')
                  IER=1
                  return
            end if
        ELSE
            con_name(is)=' '
        END IF
 10   CONTINUE

C Count how many default CFC types exist (ie. how many different
C CFC constructions exist in the zone)
C nmaxtypes holds default max # of CFC types
      IF(ncfccount.ge.1)then
          nmaxtypes=0
          ntypes=0
          DO 20 IS=1,NZSUR(icomp)
             if(is.eq.1.and.con_name(is).ne.' ') ntypes=1
             if(is.ne.NZSUR(icomp))then

               if(con_name(is).ne.con_name(is+1)
     &               .and.con_name(is+1).ne.' ')then
                 isame=0
                 do 30 iss=is,1,-1
                        if(con_name(is+1).eq.con_name(iss))then
                          isame=isame+1
                        end if
 30              continue
                 if(isame.eq.0)ntypes=ntypes+1

               endif

             if(ntypes.gt.nmaxtypes)then
                nmaxtypes=ntypes
             endif

             endif
 20       CONTINUE
      END IF
      ncfctypes=nmaxtypes

C Ask user to provide alternate number of CFC types
C (cannot exceed # of CFCs in zone).
 11   CALL EASKI(ncfctypes,' ','Number of CFC types?',
     &    nmaxtypes,'F',ncfccount,'F',nmaxtypes,
     &    'no of CFC types',IER,nbhelp)
      if(IER.EQ.-3)GOTO 99
      im_ncfc(icomp)=ncfctypes

C go through CFC constructions and ask user to assign type for each one,
      ncfccount=0
      DO 40 IS=1,NZSUR(icomp)
        icn1=izstocn(icomp,is)
        itype=0
        IF(SSOTF(icn1)(1:4).eq.'CFC ')then

          if(ncfccount.lt.nmaxtypes)ncfccount=ncfccount+1

          write(outs,'(A,A,A,A)')'Surface: ',SSNAME(icn1),
     &          'Constr: ',SSMLCN(icn1)
          CALL EASKI(itype,'CFC type for: ',outs,1,'F',
     &      ncfctypes,'F',ncfccount,'CFC type',IER,nbhelp)
          if(IER.EQ.-3)GOTO 99
          im_cfcfl(icomp,is)=itype
        ELSE
          im_cfcfl(icomp,is)=0
        ENDIF
 40   CONTINUE

C check that all CFC types have been assigned 
      DO 50 i=1,ncfctypes
      icount=0

       do 60 is=1,NZSUR(icomp)
          if(im_cfcfl(icomp,is).eq.i)icount=icount+1
 60    continue

       if(icount.gt.0)then
          continue
       else
          write(outs,'(A,I4)')'Missing CFC type: ',i
          CALL USRMSG(outs,'Please try again. ','W')
          goto 11
       endif

 50   CONTINUE

C check mismatch between CFC type and construction (two different CFC 
C constructions cannot have the same CFC type)
      if(ncfccount.ge.1)then
      DO 70 is=1, NZSUR(icomp)
        icn1=izstocn(icomp,is)
        icompare1=im_cfcfl(icomp,is)
        if(icompare1.gt.0)then
         do 80 iss=1,NZSUR(icomp)
         icn2=izstocn(icomp,iss)
         icompare2=im_cfcfl(icomp,iss)
          if(icompare2.gt.0)then
            if(icompare2.eq.icompare1)then
              if(SSMLCN(icn1).ne.SSMLCN(icn2))then
                 CALL USRMSG(
     &           ' Two different CFC compositions cannot',
     &           ' have same CFC type, please try again. ','W')
                 goto 11
              end if
            endif
          endif
 80      continue
        endif
 70   CONTINUE
      end if

C ------------------------------------------------------------------
C if at this point then checks passed, ready to assemble *.GSL files
C ------------------------------------------------------------------

C GSLfiles is array that holds DEFAULT names for *.GSL files for
C each CFC type.
      nmax=0
      do 90 is=1,NZSUR(icomp)

      if(im_cfcfl(icomp,is).gt.0)then
        icfctp=im_cfcfl(icomp,is)

C If this cfc type has already been processed, skip to next surface.
        icount=0
        do 91 ibk=is,1, -1
          if(im_cfcfl(icomp,ibk).eq. im_cfcfl(icomp,is))then
            icount=icount+1
          endif 
  91    continue
        if(icount.gt.1)goto 90
  12      write(outs,'(a,i1)')'cfc',icfctp
        if(zonepth(1:2).eq.'  '.or.zonepth(1:2).eq.'./')then
          ln_outs=lnblnk(outs)
          write(GSLfiles(icomp,icfctp),'(2a)')
     &            outs(1:ln_outs),'.GSL'
        else
          ln_zonepth=lnblnk(zonepth)
          ln_outs=lnblnk(outs)
          write(GSLfiles(icomp,icfctp),'(4a)')
     &     zonepth(1:ln_zonepth),'/',outs(1:ln_outs),'.GSL'
        endif

C get location and GSLedit file name for each CFC type
        ltmp=GSLfiles(icomp,icfctp)
        write(outs,'(a,i4)')
     &      'GSLedit file name and path for CFC type: ',icfctp
        CALL EASKS(ltmp,outs,' ',72,ltmp,'GSLedit file name',
     &    IER,nbhelp)
        IF(IER.EQ.-3)GOTO 99
        if(ltmp(1:2).ne.'  '.and.ltmp(1:4).ne.'UNKN')then
          GSLfiles(icomp,icfctp)=ltmp
        endif

C attempt to find GSL file, if successful import GSLdata,
C if not, go back and prompt user for file name, or cancel
        call FINDFIL(GSLfiles(icomp,icfctp),XST)
        if(XST)then
C import GSLedit data and store in cfc commons 
          call importGSLedit(IFU,GSLfiles(icomp,icfctp),
     &            icomp,icfctp,is,IER)
          if(IER.gt.0)goto 98
          write(outs,'(a,a,a)')'Import from GSLedit file: ',
     &    GSLfiles(icomp,icfctp)(1:lnblnk(GSLfiles(icomp,icfctp))),
     &    ' OK'
          CALL EDISP(IUOUT,' ')
          CALL EDISP(IUOUT,outs)
        else
          CALL EASKAB('File not found! ','Options:',
     &       'try again','cancel',IWA,nbhelp)
          if(IWA.eq.1)goto 12
          if(IWA.eq.2)GOTO 99
        endif
      endif

      if(icfctp.gt.nmax)nmax=icfctp

 90   continue

C -------------------------------------------------------------------------
C If at this point, all necessary *.GSL files exist and import from GSLedit
C is successful for all CFC types.
C -------------------------------------------------------------------------

C Open *.cfc file
      if(lcfcin(icomp)(1:4).eq.'UNKN'.or.
     &   lcfcin(icomp)(1:2).eq.'  ')then
         goto 98
      else
         CALL EFOPSEQ(IFU,lcfcin(icomp),4,IER)
         call edisp(iuout,' Creating a new CFC file...')
         call edisp(iuout,lcfcin(icomp))
      endif
      IF(IER.NE.0)THEN
        GOTO 98
      ENDIF

C Write out complex fenestration construction file data.

      WRITE(IFU,100,IOSTAT=ISTAT,ERR=98)
     &  zname(ICOMP)(1:lnblnk(zname(ICOMP))),
     &  lcfcin(ICOMP)(1:lnblnk(lcfcin(ICOMP)))
 100  FORMAT('# complex fenestration construction properties of ',
     &      a,' defined in ',a)

C Build one or two packed strings and write out. Should be good
C for up to NZSUR() of ~62.
      WRITE(IFU,'(I4,A)',IOSTAT=ISTAT,ERR=98)NZSUR(icomp),
     &  '   # surfaces'
      do 110 ij=1,MS
        ival(ij)=im_cfcfl(icomp,ij)
 110  continue
      WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)'# CFC index for each surface'
      itrunc=1
      ipos=1
      do while (itrunc.ne.0)
        call ailist(ipos,nzsur(icomp),ival,MS,'C',louts,loutlen,itrunc)
        write(ifu,'(a)',ERR=98) louts(1:loutlen)
        ipos=itrunc+1
      end do

C write out solar, visual, and longwave properties for each cfc type
C assuming these have been imported from GSL file and stored in cfc
C commons. 
      do 120 it=1,im_ncfc(icomp)
        write(outs,'(i4,a,i2)')im_ncfc_el(icomp,it),
     &      '   # layers in cfc type: ',it
        WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))
        write(outs,'(a,a)')
     &      '# For each layer: normal solar optical properties - ',
     &      'R_fr, R_bk, Tran.'
        WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))

        do 130 ie=1,im_ncfc_el(icomp,it)

              if(im_cfcltp(icomp,it,ie).eq.2)then
                write(outs,'(3F8.3,a)')rim_SolRf(icomp,it,ie),
     &            rim_SolRb(icomp,it,ie),rim_SolT(icomp,it,ie),
     &            '   # slat-type blind '
                WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &                        outs(1:lnblnk(outs))

              elseif(im_cfcltp(icomp,it,ie).eq.1)then
                write(outs,'(3F8.3,a)')rim_SolRf(icomp,it,ie),
     &            rim_SolRb(icomp,it,ie),rim_SolT(icomp,it,ie),
     &            '   # glazing '
                WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &                        outs(1:lnblnk(outs))

              elseif(im_cfcltp(icomp,it,ie).eq.0)then
                write(outs,'(3F8.3,a)')rim_SolRf(icomp,it,ie),
     &            rim_SolRb(icomp,it,ie),rim_SolT(icomp,it,ie),
     &            '   # gas gap '
                WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &                        outs(1:lnblnk(outs))

              else
                GOTO 98
              endif

 130    continue

        write(outs,'(a,a)')
     &      '# For each layer: normal visible optical properties - ',
     &      'R_fr, R_bk, Tran. EXPERIMENTAL, USE WITH CAUTION'
        WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))

        do 140 ie=1,im_ncfc_el(icomp,it)

              if(im_cfcltp(icomp,it,ie).eq.2)then
                write(outs,'(3F8.3,a)')rim_VisRf(icomp,it,ie),
     &            rim_VisRb(icomp,it,ie),rim_VisT(icomp,it,ie),
     &            '   # slat-type blind '
                WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &                        outs(1:lnblnk(outs))

              elseif(im_cfcltp(icomp,it,ie).eq.1)then
                write(outs,'(3F8.3,a)')rim_VisRf(icomp,it,ie),
     &            rim_VisRb(icomp,it,ie),rim_VisT(icomp,it,ie),
     &            '   # glazing '
                WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &                        outs(1:lnblnk(outs))

              elseif(im_cfcltp(icomp,it,ie).eq.0)then
                write(outs,'(3F8.3,a)')rim_VisRf(icomp,it,ie),
     &            rim_VisRb(icomp,it,ie),rim_VisT(icomp,it,ie),
     &            '   # gas gap '
                WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &                        outs(1:lnblnk(outs))

              else
                GOTO 98
              endif

 140    continue

        write(outs,'(a,a)')
     &      '# For each layer: normal longwave radiative properties - ',
     &      'EmisF, EmisB, Tran.'
        WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))

        do 150 ie=1,im_ncfc_el(icomp,it)

              if(im_cfcltp(icomp,it,ie).eq.2)then
                write(outs,'(3F8.3,a)')rim_lwEf(icomp,it,ie),
     &            rim_lwEb(icomp,it,ie),rim_lwT(icomp,it,ie),
     &            '   # slat-type blind '
                WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &                        outs(1:lnblnk(outs))

              elseif(im_cfcltp(icomp,it,ie).eq.1)then
                write(outs,'(3F8.3,a)')rim_lwEf(icomp,it,ie),
     &            rim_lwEb(icomp,it,ie),rim_lwT(icomp,it,ie),
     &            '   # glazing '
                WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &                        outs(1:lnblnk(outs))

              elseif(im_cfcltp(icomp,it,ie).eq.0)then
                write(outs,'(3F8.3,a)')rim_lwEf(icomp,it,ie),
     &            rim_lwEb(icomp,it,ie),rim_lwT(icomp,it,ie),
     &            '   # gas gap '
                WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &                        outs(1:lnblnk(outs))

              else
                GOTO 98
              endif

 150    continue


 120  continue

C write out layer type index
      DO 240 it=1,im_ncfc(icomp)
        vb_xst=.false.
        do 250 ij=1,im_ncfc_el(icomp,it)
          ival(ij)=im_cfcltp(icomp,it,ij)
          if(ival(ij).eq.2)vb_xst=.true.
 250    continue
        write(outs,'(a,i2)')
     &      '# layer type index for cfc type: ',it
        WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))
        itrunc=1
        ipos=1

        do while (itrunc.ne.0)
          call ailist(ipos,im_ncfc_el(icomp,it),
     &            ival,ME,'C',louts,loutlen,itrunc)
          write(ifu,'(a)',ERR=98) louts(1:loutlen)
          ipos=itrunc+1
        end do
      if(im_ncfc_el(icomp,it).eq.1)goto 241


C WRITE OUT GAS PROPERTIES HERE
        write(outs,'(a,i2)')
     &      '# Gas mixture properties for cfc type: ',it
        WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))
        do 260 ie=1,im_ncfc_el(icomp,it)
          if(ival(ie).eq.0)then
            write(outs,'(a,i4)')'# gas layer',ie
            WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))

            write(outs,'(e10.3,a)')rim_mlr_mass(icomp,it,ie),
     &            '        # molecular mass of gas mixture (g/gmole)'
            WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))

            write(outs,'(e10.3,1x,e10.3,a)')rim_cond_A(icomp,it,ie),
     &            rim_cond_B(icomp,it,ie),
     &            '        # a and b coeffs.- gas conductivity (W/m.K)'
            WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))

            write(outs,'(e10.3,1x,e10.3,a)')rim_visc_A(icomp,it,ie),
     &            rim_visc_B(icomp,it,ie),
     &            '        # a and b coeffs.- gas viscosity (N.s/m2)'
            WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))

            write(outs,'(e10.3,1x,e10.3,a)')rim_spht_A(icomp,it,ie),
     &            rim_spht_B(icomp,it,ie),
     &            '        # a and b coeffs.- specific heat (J/kg.K)'
            WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))
           endif
  260   continue

C WRITE OUT SLAT-TYPE BLIND PROPERTIES HERE
  241   if(vb_xst)then
         write(outs,'(a,i2)')
     &      '# slat-type blind attributes for cfc type: ',it
         WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))

         write(outs,'(a,a,a)')
     &      '# slat: width(mm); spacing(mm); angle(deg);',
     &      ' orientation(HORZ/VERT); crown (mm);',
     &      ' w/r ratio; slat thickness (mm)'
         WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))

         write(outs,'(3F8.3,a,a,a,3F8.3)')rim_vb_w(icomp,it),
     &         rim_vb_s(icomp,it),rim_vb_phi(icomp,it),
     &         '  ',cim_VorH(icomp,it),' ',rim_vb_crown(icomp,it),
     &         rim_vb_wr(icomp,it),rim_vb_t(icomp,it)
         WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))
        endif

 240  CONTINUE

      CALL ERPFREE(IFU,ISTAT)
 1111 RETURN

C Error if we are here
   98 CALL EDISP(IUOUT,' ')
      CALL EDISP(IUOUT,' CFC file write error ')
      IER=1
      write(outs,'(a,a)')' Removing incomplete cfc file: ',
     &                   lcfcin(ICOMP)
      call EDISP(IUOUT,outs)
      CALL EFDELET(IFU,ISTAT)
      goto 1111

c User cancelled if we are here
   99 CALL EDISP(IUOUT,' ')
      CALL EDISP(IUOUT,' Import of GSL files cancelled by user')
      IER=1
      write(outs,'(a,a)')' Removing incomplete cfc file: ',
     &                   lcfcin(ICOMP)
      call EDISP(IUOUT,outs)
      CALL ERPFREE(IFU,ISTAT)
      CALL EFOPSEQ(IFU,lcfcin(icomp),1,IER)
      CALL EFDELET(IFU,ISTAT)
      GOTO 1111

C User choice 'continue'
      ELSEIF(IWA.eq.2)then
        return
      ENDIF 

      return
      end


C ********************************************************************
C                        --importGSLedit--
C
C Reads data from an annotated ASCII *.GSL file and stores data in 
C CFC import commons. Performs various checks to ensure that GSLedit
C data matches CFC composition. 
C
C For details on assembling *.GSL files in GSLedit refer to 
C Appendix F in:
C Lomanowski, B.A. (2008) 'Implementation of Window Shading Models
C into Dynamic Whole-Building Simulation', MASc Thesis, University
C of Waterloo.
C
C GSLfile containts the path to *.GSL file
C
C
C TO DO: add solar optical and longwave property summation checks.
C
C ********************************************************************
      subroutine importGSLedit(IUF,GSLfile,icomp,icfctp,isur,IER)
#include "building.h"
#include "CFC_common.h"
      COMMON/OUTIN/IUOUT,IUIN
      integer iuout,iuin
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      integer ne,nairg,ipairg
      real rairg

      integer IUF,icomp,icfctp,isur,IER,nelts_nogas,K,neimp,ivb_count
      integer j,nelts,i,ie,ND,IWA,litems,lgasgaps,ltypes,istat
      real rphi,rprop,rvb_w

      integer ln_outs
      integer lnblnk    !function definition
      
      CHARACTER OUTSTR*124,OUTS*124,GSLfile*72,ltype*3
      CHARACTER LOUTSTR*248

      logical vb_xst

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='importGSLedit'  ! set for subroutine

      vb_xst=.false.

      if(GSLfile(1:4).eq.'UNKN'.or.GSLfile(1:2).eq.'  ')then
         goto 98
      else
         CALL EFOPSEQ(IUF,GSLfile,1,IER) !Open *.GSL ASCII file
         call edisp(iuout,' ')
         call edisp(iuout,' Importing data from GSLedit file:')
         call edisp(iuout,GSLfile)
      endif
      IF(IER.NE.0)THEN
        GOTO 98
      ENDIF

C establish no. of non-gas gap layers in cfc type 
      nelts_nogas=ne(isur)-nairg(isur)

C read no. of layers from GSLedit file
      CALL STRIPC(IUF,OUTSTR,1,ND,1,'no, layers in GSLedit file',IER)
C                            ^  ^ ^         ^
C                            \  \  \        \---- error msg
C                            \  \  \------------- IR=0 then acts silently, otherwise notes when EOF found.
C                            \  \---------------- if IEXP=99, store no. of items in ND
C                            \------------------- no. of expected items IEXP
      IF(IER.NE.0)GOTO 1001
      K=0
      CALL EGETWI(OUTSTR,K,neimp,1,ME,'F',
     &      ' no. layers in GSLedit file',IER)

C check no. of non-gas gap layers mismatch between cfc and GSLedit file
      if(neimp.ne.nelts_nogas)then
        call edisp(iuout,' GSLedit import error: ')
        call edisp(iuout,' mismatch in no. of layers ')
        IER=1
        goto 1111
      endif
 

C read layer types from GSLfile and assign to cfc common:
C     GLZ= glazing, corresponds to '1' in array icfcltype
C     VBD= venetian (slat-type) blind, corresponds to '2' in array icfcltype
C gas gaps are not represented as layers in GSLedit so between
C each layer assign a '0' in array icfltype for a gas gap.
      CALL STRIPC(IUF,OUTSTR,nelts_nogas,ND,1,
     &            'layer types in GSLedit file',IER)
      IF(IER.NE.0)GOTO 1001
      K=0

      nelts=ne(isur)
      im_ncfc_el(icomp,icfctp)=nelts
      ivb_count=0
      do 10 j=1,nelts,2
        CALL EGETW(OUTSTR,K,ltype,'F','layer type',IER)
        if(ltype.ne.'GLZ'.and.ltype.ne.'VBD')then
            call edisp(iuout,' GSLedit import error: ')
            call edisp(iuout,' allowed layer types: GLZ, VBD ')
            IER=1
            goto 1111
        elseif(ltype.eq.'VBD')then
            im_cfcltp(icomp,icfctp,j)=2
            ivb_count=ivb_count+1
            vb_xst=.true.
        else
            im_cfcltp(icomp,icfctp,j)=1
        end if

        if(j.lt.nelts)im_cfcltp(icomp,icfctp,j+1)=0
   10 continue
      if(ivb_count.gt.1)then
            call edisp(iuout,' GSLedit import error: ')
            call edisp(iuout,' Maximum of 1 VBD layers allowed ')
            IER=1
            goto 1111
      endif

C Ask user to specify slat orientation (HORZ, VERT) and initial slat
C angle. 
      helptopic='slat_angle_convention'
      call gethelptext(helpinsub,helptopic,nbhelp)
      if(vb_xst)then
          write(outs,'(a,i2)')
     &      'Specify blind slat orientation for cfc type: ',icfctp
          ln_outs=lnblnk(outs)
          CALL EASKAB(outs(1:ln_outs),' ',
     &            'Vertical','Horizontal',IWA,nbhelp)
          if(IWA.eq.1)cim_VorH(icomp,icfctp)='VERT'
          if(IWA.eq.2)cim_VorH(icomp,icfctp)='HORZ'

          write(outs,'(a,i2)')
     &      'Specify intial blind slat angle for cfc type: ',icfctp
          rphi=0.0
          ln_outs=lnblnk(outs)
          CALL EASKR(rphi,' ',outs(1:ln_outs),-89.999,'F',
     &               89.999,'F',0.0,'slat angle',IER,nbhelp)
          rim_vb_phi(icomp,icfctp)=rphi
      endif

C Read Solar optical, visual and longwave optical properties for
C each layer. If venetian/slat-type blind, also read w/r ratio (Widht/radius
C curvature ratio), slat thickness and slat apparent thickness
      nelts=ne(isur)
      do 20 i=1,nelts,2
         CALL LSTRIPC(IUF,LOUTSTR,99,litems,1,
     &            'optical properties in GSLedit file',IER)
         IF(IER.NE.0)GOTO 1001
         K=23

         if(im_cfcltp(icomp,icfctp,i).eq.1)then

            CALL EGETWR(LOUTSTR,K,rprop,.0,0.999,'F',
     &            'solar transmission',IER)
            rim_SolT(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,0.999,'F',
     &            'solar reflectance - front',IER)
            rim_SolRf(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,0.999,'F',
     &            'solar reflectance - back',IER)
            rim_SolRb(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,0.999,'F',
     &            'visual transmission',IER)
            rim_VisT(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,0.999,'F',
     &            'visual reflectance - front ',IER)
            rim_VisRf(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,0.999,'F',
     &            'visual reflectance - back',IER)
            rim_VisRb(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,0.999,'F',
     &            'long wave transmission',IER)
            rim_lwT(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,0.999,'F',
     &            'long wave emissivity - front',IER)
            rim_lwEf(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,0.999,'F',
     &            'long wave emissivity - back',IER)
            rim_lwEb(icomp,icfctp,i)=rprop

         elseif(im_cfcltp(icomp,icfctp,i).eq.2)then

            CALL EGETWR(LOUTSTR,K,rprop,.0,0.999,'F',
     &            'solar slat transmission',IER)
            rim_SolT(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,0.999,'F',
     &            'solar slat reflectance - front(top)',IER)
            rim_SolRf(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,0.999,'F',
     &            'solar slat reflectance - back(top)',IER)
            rim_SolRb(icomp,icfctp,i)=rprop

C Assign default visual=solar properties until visual properies added 
C to GSLedit
c            rim_VisT(icomp,icfctp,i) = rim_SolT(icomp,icfctp,i)
            rim_VisRf(icomp,icfctp,i)= rim_SolRf(icomp,icfctp,i)
            rim_VisRb(icomp,icfctp,i)= rim_SolRb(icomp,icfctp,i)

            CALL EGETWR(LOUTSTR,K,rprop,.001,0.30,'F',
     &            'slat width (m)',IER)
            rim_vb_w(icomp,icfctp)=rprop*1000.  ![convert to mm]
            CALL EGETWR(LOUTSTR,K,rprop,.001,0.30,'F',
     &            'slat spacing (m)',IER)
            rim_vb_s(icomp,icfctp)=rprop*1000.  ![convert to mm]
            rvb_w=rprop
            CALL EGETWR(LOUTSTR,K,rprop,0.0,rvb_w/2.0,'F',
     &            'slat crown (m)',IER)
            rim_vb_crown(icomp,icfctp)=rprop*1000.    ![convert to mm]
            CALL EGETWR(LOUTSTR,K,rprop,0.0,1.999,'F',
     &            'slat w/r ratio',IER)
            rim_vb_wr(icomp,icfctp)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,0.999,'F',
     &            'long wave transmission',IER)
            rim_lwT(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,0.999,'F',
     &            'long wave emissivity - front',IER)
            rim_lwEf(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,0.999,'F',
     &            'long wave emissivity - back',IER)
            rim_lwEb(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,0.3,'F',
     &            'slat thickness (m)',IER)
            rim_vb_t(icomp,icfctp)=rprop*1000.  ![convert to mm]

         else
            goto 1001
         end if

C assign optical properties for air gap
        if(i.lt.nelts)then
         rim_SolT(icomp,icfctp,i+1) =     0.0001
         rim_SolRf(icomp,icfctp,i+1)=     0.0001
         rim_SolRb(icomp,icfctp,i+1)=     0.0001
         rim_VisT(icomp,icfctp,i+1) =     0.0001
         rim_VisRf(icomp,icfctp,i+1)=     0.0001
         rim_VisRb(icomp,icfctp,i+1)=     0.0001
         rim_lwT(icomp,icfctp,i+1)  =     0.0001
         rim_lwEf(icomp,icfctp,i+1) =     0.0001
         rim_lwEb(icomp,icfctp,i+1) =     0.0001
        endif

 20   continue

C--------------------------------------------------------------------
C If here, import commons for optical properties and slat blind 
C attributes are filled.
C -------------------------------------------------------------------

      if(im_ncfc_el(icomp,icfctp).eq.1)then  !CFC must have more than one layer
            IER=-1
            goto 98
      end if

C Read cavity gas data

C count number of gas gaps and check that this equals NAIRG
      CALL STRIPC(IUF,OUTSTR,99,lgasgaps,1,'no. of gas gaps',IER)
      IF(IER.NE.0)GOTO 1001
      if(lgasgaps.ne.nairg(isur))then
            call edisp(iuout,' GSLedit import error: ')
            call edisp(iuout,' mismatch in no. of gas gaps ')
            IER=1
            goto 1111
      endif

C Loop through CFC layers and assign gas properties to CFC commons
      do 30 ie=1,im_ncfc_el(icomp,icfctp)
        if(im_cfcltp(icomp,icfctp,ie).eq.0)then

C skip mixture composition and gap thickness data
          CALL STRIPC(IUF,OUTSTR,99,ltypes,1,'skip this line',IER)
C import molecular mass of gas mixture (g/gmole)
          CALL STRIPC(IUF,OUTSTR,1,ND,1,
     &            'molecular mass (g/gmole)',IER)
          K=0
          CALL EGETWR(OUTSTR,K,rprop,.000,500.0,'W',
     &            'molecular mass (g/gmole)',IER)
          rim_mlr_mass(icomp,icfctp,ie)=rprop
C import a anb b coefficients for conductivity (W/m.K)
          CALL STRIPC(IUF,OUTSTR,2,ND,1,
     &            'a and b coeffs.- gas conductivity (W/m.K)',IER)
          K=0
          CALL EGETWR(OUTSTR,K,rprop,0.0,1.0,'W',
     &            'a coefficient - gas conductivity (W/m.K)',IER)
          rim_cond_A(icomp,icfctp,ie)=rprop
          CALL EGETWR(OUTSTR,K,rprop,0.0,1.0,'W',
     &            'b coefficient - gas conductivity (W/m.K)',IER)
          rim_cond_B(icomp,icfctp,ie)=rprop
C import a anb b coefficients for viscosity (N.s/m2)
          CALL STRIPC(IUF,OUTSTR,2,ND,1,
     &            'a and b coeffs.- gas viscosity (N.s/m2)',IER)
          K=0
          CALL EGETWR(OUTSTR,K,rprop,0.0,1.0,'W',
     &            'a coefficient - gas viscosity (N.s/m2)',IER)
          rim_visc_A(icomp,icfctp,ie)=rprop
          CALL EGETWR(OUTSTR,K,rprop,0.0,1.0,'W',
     &            'b coefficient - gas viscosity (N.s/m2)',IER)
          rim_visc_B(icomp,icfctp,ie)=rprop
C import a anb b coefficients for specific heat (J/kg.K)
          CALL STRIPC(IUF,OUTSTR,2,ND,1,
     &            'a and b coeffs.- specific heat (J/kg.K)',IER)
          K=0
          CALL EGETWR(OUTSTR,K,rprop,0.0,20000.0,'W',
     &            'a coefficient - specific heat (J/kg.K)',IER)
          rim_spht_A(icomp,icfctp,ie)=rprop
          CALL EGETWR(OUTSTR,K,rprop,-10.0,1000.0,'W',
     &            'b coefficient - specific heat (J/kg.K)',IER)
          rim_spht_B(icomp,icfctp,ie)=rprop
        endif

  30  continue

 1111 CALL ERPFREE(IUF,ISTAT)
      RETURN

c GSLedit import Error messages.
 1001 CALL USRMSG(' Problem reading data in:',OUTSTR,'W')
      goto 1111

C If in quiet mode: the file should exist => error if we are here.
   98 IF(IER.EQ.-1)THEN
        call edisp(iuout,'CFCs must contain more than one layer.')
      ELSE
        CALL EDISP(IUOUT,' GSLedit import error ')
      ENDIF
      IER=1
      goto 1111

      end


C ********************************************************************
C                       --read_in_cfc_file--
C
C Reads an annotated ASCII *.cfc file, strips comments and stores data
C in CFC common blocks.
C
C (Based on subroutine ERTWIN that reads *.tmc input file)
C
C TO DO: add solar optical and longwave property summation checks.
C
C ********************************************************************
      subroutine read_in_cfc_file(itrc,itru,iua,lua,icomp,ier)
#include "building.h"
#include "geometry.h"
#include "espriou.h"
#include "CFC_common.h"
      COMMON/OUTIN/IUOUT,IUIN
      integer iuout,iuin
      COMMON/C24/IZSTOCN(MCOM,MS)
      integer izstocn
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      integer ne,nairg,ipairg
      real rairg
	  
      integer iva,itrc,itru,iua,icomp,ier,NS,IRVA,i,j,k,ierr
      integer nshade_layers,ie,ND,ISR,icn,ntmpcfc_el,iflag,istat
      integer lnblnk    !function definition
      
      DIMENSION IVA(MS)
    
      CHARACTER OUTSTR*124,OUTS*124,LUA*72,WORD*20

      real val
      logical vb_xst
      val=0.
      vb_xst=.false.

      CALL EFOPSEQ(IUA,LUA,1,IER)
      IF(IER.NE.0)THEN
       IER=1
       goto 1000
      ENDIF

c Read zone complex fenestration construction properties from file.
C Read lines from file, discarding comments.
      CALL STRIPC(IUA,OUTSTR,1,ND,1,'no zone surfaces',IER)
      IF(IER.NE.0)GOTO 1001
      K=0
      CALL EGETWI(OUTSTR,K,NS,4,MS,'W','no zone cfc surf',IER)

C Read pointer to type of CFC for each surface, strip comments etc. If
C ICFCFL != 0 and surface attribute is not 'CFC ' then confirm
C if the surface attribute should be updated.
      IRVA=NS
      CALL EGETWIA(IUA,IVA,IRVA,0,MCFC,'W','cfc list',IER)
      ncfc(icomp)=0
      DO 10 I=1,NS
        icfcfl(icomp,i)=iva(i)
       IF(icfcfl(icomp,i).NE.0.AND.
     &     (SSOTF(IZSTOCN(ICOMP,I)).NE.'CFC '))THEN
         WRITE(OUTS,'(5a)')' The CFC file ',LUA(1:lnblnk(LUA)),
     &      ' not sure if ',SSNAME(IZSTOCN(ICOMP,I)),
     &     ' is a complex fenestration constr.'
         call edisp(itru,outs)
         call edisp(itru,' Check your zone files.')
       ENDIF
        IF(icfcfl(icomp,i).GT.ncfc(icomp))ncfc(icomp)=icfcfl(icomp,i)
   10 CONTINUE
      IF(ncfc(icomp).EQ.0.OR.ncfc(icomp).GT.mcfc)THEN
        CALL EDISP(ITRU,' No. of CFC types out of range.')
        GOTO 1002
      ENDIF

C Reporting.
      IF(ITRC.GT.0)THEN
        CALL EDISP(ITRU,' ')
        WRITE(OUTS,9996)zname(ICOMP)(1:lnblnk(zname(ICOMP)))
 9996   FORMAT(' Complex fenestration construction file details for ',
     &           A)
        CALL EDISP(ITRU,OUTS)
        CALL EDISP(ITRU,' ')
        CALL EDISP(ITRU,' Surface      Construction   OPAQ/  CFC type ')
        CALL EDISP(ITRU,' Name         Description  TRAN/CFC Reference')
        DO 31, ISR=1,NS
          icn=izstocn(icomp,isr)
          WRITE(OUTS,'(1X,A12,2X,A12,2X,A4,I5)')SSNAME(icn),
     &      SSMLCN(icn)(1:12),SSOTF(icn)(1:4),icfcfl(icomp,isr)
          CALL EDISP(ITRU,OUTS)
   31   CONTINUE
      ENDIF

      DO 20 I=1,ncfc(icomp)

c Establish the number of layers in CFC type I, check that no. of 
C layers matches mlc database.
        CALL STRIPC(IUA,OUTSTR,99,ND,1,'CFC layers',IER)
        IF(IER.NE.0)GOTO 1001
        K=0
        CALL EGETWI(OUTSTR,K,ntmpcfc_el,1,ME,'W','CFC layers',IER)
        IERR=0
        DO 21 J=1,NS
          IF(icfcfl(icomp,j).EQ.I.AND.ntmpcfc_el.NE.NE(J))IERR=1
          IF(IERR.EQ.1)THEN
            write(outs,'(6a)')
     &      ' Mismatched CFC & mlc layers: ',
     & SSNAME(IZSTOCN(ICOMP,J))(1:lnblnk(SSNAME(IZSTOCN(ICOMP,J)))),
     &   ' in ',zname(ICOMP)(1:lnblnk(zname(ICOMP))),' composed of ',
     &      SSMLCN(IZSTOCN(ICOMP,J))
            call edisp(iuout,outs)
            goto 1002
          ENDIF
   21   CONTINUE
        ncfc_el(icomp,i)=ntmpcfc_el

C Read normal solar optical properties for complex fenestration construction.
      do 40 j=1,ncfc_el(icomp,i)
        CALL STRIPC(IUA,OUTSTR,99,ND,1,'normal solar-optical',IER)
        IF(IER.NE.0)GOTO 1001
        K=0
        CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','front solar ref.',
     &              IER)
        solRF(ICOMP,i,j)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','back solar ref.',
     &              IER)
        solRB(ICOMP,i,j)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','front solar tran.',
     &              IER)
        solT(ICOMP,i,j)=VAL
   40 continue

C Read visible solar properties for complex fenestration construction.
      do 50 j=1,ncfc_el(icomp,i)
        CALL STRIPC(IUA,OUTSTR,99,ND,1,'visible solar',IER)
        IF(IER.NE.0)GOTO 1001
        K=0
        CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','front visible ref.',
     &              IER)
        visRF(ICOMP,i,j)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','back visible ref.',
     &              IER)
        visRB(ICOMP,i,j)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','front visible tran.',
     &              IER)
        visT(ICOMP,i,j)=VAL
   50 continue

C Read long-wave properties for complex fenestration construction.
      do 70 j=1,ncfc_el(icomp,i)
        CALL STRIPC(IUA,OUTSTR,99,ND,1,'long-wave',IER)
        IF(IER.NE.0)GOTO 1001
        K=0
        CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','front lw emiss.',
     &              IER)
        rlwEF(ICOMP,i,j)=VAL
        rlwEF_sv(icomp,i,j)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','back lw emiss. ',
     &              IER)
        rlwEB(ICOMP,i,j)=VAL
        rlwEB_sv(icomp,i,j)=VAL
        CALL EGETWR(OUTSTR,K,VAL,0.,0.999,'W','lw trans.',
     &              IER)
        rlwT(ICOMP,i,j)=VAL
        rlwT_sv(icomp,i,j)=VAL
   70 continue

C Reporting
       IF(ITRC.GT.0)THEN
         CALL EDISP(ITRU,' ')
         WRITE(OUTS,'(A,I2)')
     &    ' For CFC type ',I
         CALL EDISP(ITRU,OUTS)
         CALL EDISP(ITRU,
     &      ' Normal solar-optical prop. front/back/tran. ')
         do 32 j=1,ncfc_el(icomp,i)
         WRITE(OUTS,'(2X,3F7.3)')solRF(icomp,i,j),solRB(icomp,i,j),
     &                           solT(icomp,i,j)
  32     continue
         CALL EDISP(ITRU,OUTS)

         CALL EDISP(ITRU,
     &      ' Normal visual-optical prop. front/back/tran. ')
         do 33 j=1,ncfc_el(icomp,i)
         WRITE(OUTS,'(2X,3F7.3)')visRF(icomp,i,j),visRB(icomp,i,j),
     &                           visT(icomp,i,j)
  33     continue
         CALL EDISP(ITRU,OUTS)

         CALL EDISP(ITRU,
     &      ' Normal longwave-optical prop. front/back/tran. ')
         do 34 j=1,ncfc_el(icomp,i)
         WRITE(OUTS,'(2X,3F7.3)')rlwEF(icomp,i,j),rlwEB(icomp,i,j),
     &                           rlwT(icomp,i,j)
  34     continue
         CALL EDISP(ITRU,OUTS)

       ENDIF

   20 CONTINUE

C Read element type for each layer of complex fenestration construction
C icfcltp - 0=gas gap, 1=opaque/glass, 2=slat-type blind
      do 80 i=1,ncfc(icomp)

      nshade_layers=0
      IRVA=ncfc_el(icomp,i)
      CALL EGETWIA(IUA,IVA,IRVA,0,2,'W','cfc layer type index',IER)

C check that only one slat-type blind exists and highest value in 
C icfcltp = 2 (currently only slat-type blinds and glazings allowed)
      do 90 j=1,ncfc_el(icomp,i)
             if(IVA(j).eq.2)then
               nshade_layers=nshade_layers+1
             elseif(iva(j).gt.2)then
               call edisp(itru,' ')
               call edisp(itru,
     &         ' CFC layer type index cannot exceed 2.')
               GOTO 1002
             end if
             icfcltp(icomp,i,j)=IVA(j)
   90 continue

      if(nshade_layers.eq.1)then
        vb_xst=.true.
      elseif(nshade_layers.gt.1)then
        call edisp(itru,' ')
        call edisp(itru,
     &      ' Detected more than one slat-type blind layer.')
        GOTO 1002
      else
        vb_xst=.false.
      end if

C Reporting
      IF(ITRC.GT.0)THEN
        CALL EDISP(ITRU,' ')
        WRITE(OUTS,'(A,I2,A)')
     &   ' For CFC type ',I,' ,layer type index:'
        CALL EDISP(ITRU,OUTS)
        CALL EDISP(ITRU,' ')
        CALL EDISP(ITRU,' Layer      type index')
        DO 91, j=1,ncfc_el(icomp,i)
          WRITE(OUTS,'(1X,i5,2X,i4)')i,icfcltp(icomp,i,j)
          CALL EDISP(ITRU,OUTS)
   91   CONTINUE
      ENDIF

      if(ncfc_el(icomp,i).eq.1)goto 81

C Read gas gap properties
      do 100 ie=2,ncfc_el(icomp,i),2

          CALL STRIPC(IUA,OUTSTR,99,ND,1,
     &            'molecular mass (g/gmole)',IER)
          K=0
          CALL EGETWR(OUTSTR,K,VAL,.000,500.0,'W',
     &            'molecular mass (g/gmole)',IER)
          rmlr_mass(icomp,i,ie)=VAL
          CALL STRIPC(IUA,OUTSTR,99,ND,1,
     &            'a and b coeffs.- gas conductivity (W/m.K)',IER)
          K=0
          CALL EGETWR(OUTSTR,K,VAL,0.0,1.0,'W',
     &            'a coefficient - gas conductivity (W/m.K)',IER)
          cond_A(icomp,i,ie)=VAL
          CALL EGETWR(OUTSTR,K,VAL,0.0,1.0,'W',
     &            'b coefficient - gas conductivity (W/m.K)',IER)
          cond_B(icomp,i,ie)=VAL

          CALL STRIPC(IUA,OUTSTR,99,ND,1,
     &            'a and b coeffs.- gas viscosity (N.s/m2)',IER)
          K=0
          CALL EGETWR(OUTSTR,K,VAL,0.0,1.0,'W',
     &            'a coefficient - gas viscosity (N.s/m2)',IER)
          visc_A(icomp,i,ie)=VAL
          CALL EGETWR(OUTSTR,K,VAL,0.0,1.0,'W',
     &            'b coefficient - gas viscosity (N.s/m2)',IER)
          visc_B(icomp,i,ie)=VAL

          CALL STRIPC(IUA,OUTSTR,99,ND,1,
     &            'a and b coeffs.- specific heat (J/kg.K)',IER)
          K=0
          CALL EGETWR(OUTSTR,K,VAL,0.0,20000.0,'W',
     &            'a coefficient - specific heat (J/kg.K)',IER)
          spht_A(icomp,i,ie)=VAL
          CALL EGETWR(OUTSTR,K,VAL,-10.0,1000.0,'W',
     &            'b coefficient - specific heat (J/kg.K)',IER)
          spht_B(icomp,i,ie)=VAL

C Reporting
      IF(ITRC.GT.0)THEN
        CALL EDISP(ITRU,' ')
        WRITE(OUTS,'(A,I2,A,I2,A)')
     &   ' For CFC type ',I,' ,layer ',ie,' , gas mix. properties:'
        CALL EDISP(ITRU,OUTS)
        CALL EDISP(ITRU,' ')
        write(OUTS,'(e10.3,a)')rmlr_mass(icomp,i,ie),
     &        '    - molecular mass of gas mixture (g/gmole)'
        CALL EDISP(ITRU,OUTS)
        write(OUTS,'(2e10.3,a)')cond_A(icomp,i,ie),
     &        cond_B(icomp,i,ie),
     &        '    - a and b coeffs.- gas conductivity (W/m.K)'
        CALL EDISP(ITRU,OUTS)

        write(OUTS,'(2e10.3,a)')visc_A(icomp,i,ie),
     &        visc_B(icomp,i,ie),
     &        '    - a and b coeffs.- gas viscosity (N.s/m2)'
        CALL EDISP(ITRU,OUTS)

        write(OUTS,'(2e10.3,a)')spht_A(icomp,i,ie),
     &        spht_B(icomp,i,ie),
     &        '    - a and b coeffs.- specific heat (J/kg.K)'
        CALL EDISP(ITRU,OUTS)
        CALL EDISP(ITRU,' ')
      ENDIF

 100  continue


C If slat-type blind exists then read slat attributes:
C Read venetian/slat-type blind descriptors (for type 2 cfc layer):
C    vb_w      = slat width [mm]
C    vb_s      = slat spacing [mm]
C    vb_phi    = slat angle [deg]
C    vb_crown  = slat crown [mm]
C    vb_wr     = slat w/r ratio
C    vb_t      = thickness [mm]
C    vb_VorH   = slat orientation ("HORZ" OR "VERT")
  81  if(vb_xst)then

      CALL STRIPC(IUA,OUTSTR,99,ND,1,'slat-type blind attributes',IER)
      IF(IER.NE.0)GOTO 1001
      K=0
      CALL EGETWR(OUTSTR,K,VAL,1.0,300.,'W','slat width(mm)',
     &              IER)
      vb_w(ICOMP,i)=VAL
      CALL EGETWR(OUTSTR,K,VAL,1.0,300.,'W','slat spacing(mm)',
     &              IER)
      vb_s(ICOMP,i)=VAL
      CALL EGETWR(OUTSTR,K,VAL,-89.999,89.999,'W','slat angle(deg)',
     &              IER)
      vb_phi(icomp,i)=VAL
      CALL EGETW(OUTSTR,K,WORD,'W','slat orientation',IFLAG)
      vb_VorH(icomp,i)=WORD(1:4)
      CALL EGETWR(OUTSTR,K,VAL,0.0,vb_w(icomp,i)/2.,'W',
     &      'slat crown(mm)',IER)
      vb_crown(icomp,i)=VAL
      CALL EGETWR(OUTSTR,K,VAL,0.0,1.999,'W',
     &      'slat w/r ratio',IER)
      vb_wr(icomp,i)=VAL
      CALL EGETWR(OUTSTR,K,VAL,0.0,300.,'W',
     &      'slat thickness (mm)',IER)
      vb_t(icomp,i)=VAL

C Reporting
      IF(ITRC.GT.0)THEN
       CALL EDISP(ITRU,' ')
       WRITE(OUTS,'(A,I2)')
     &  ' For CFC type ',I
       CALL EDISP(ITRU,OUTS)
       CALL EDISP(ITRU,' Slat-type blind attributes:')
       CALL EDISP(ITRU,
     & ' width(mm),spacing(mm),angle(deg),orient,crown(mm),w/r rat.')
       WRITE(OUTS,'(1X,3F7.3,1x,a,1x,2F7.3)')vb_w(ICOMP,i),
     &      vb_s(ICOMP,i),vb_phi(icomp,i),vb_VorH(icomp,i),
     &      vb_crown(icomp,i),vb_wr(icomp,i)
       CALL EDISP(ITRU,OUTS)
      ENDIF

      end if

   80 continue

c Free file.
 1000 CALL ERPFREE(IUA,ISTAT)
      return

C Error messages.
 1001 CALL USRMSG(' Problem reading data in:',OUTSTR,'W')
      IER=1
      goto 1000
 1002 write(outs,'(a,a)') ' Please check data in: ',LUA(1:lnblnk(LUA))
      CALL USRMSG(outs,' and try the model again!','W')
      IER=1
      goto 1000
      end

C ********************************************************************
C                    --editCFC_shading_schedule--
C
C UNDER CONSTRUCTION
C Based on subroutine EDAIR2
C
C ********************************************************************
      subroutine editCFC_shading_schedule(icomp,icfctype,ier)
# include "building.h"
# include "geometry.h"
# include "epara.h"
# include "CFC_common.h"   

c      COMMON/precz/zname(MCOM),zdesc(MCOM)
c      character zname*12, zdesc*64
c      COMMON/PMENU/MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG
c      integer MHEAD,MCTL,MIFULL,MFULL,IST,ILEN,IPM,MPM,IPFLG

      COMMON/SPAD/MMOD,LIMIT,LIMTTY

      COMMON/OUTIN/IUOUT,IUIN

      common/pophelp/h(60)
      character H*72

      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      
      character title*32, vert(35)*33, KEY*1
      integer mvert     !number of actual items displayed
      integer ivert     !menu selection
      !integer iCFC_ctl(icomp,icfctype) !CFC control flag
      integer istartcount, iendcount, L
      logical XST,closezero,closeone,closetwo
      integer KSTR(MDTY)
      
C Initial menu entry setup.
      idtyp=1
  5   MHEAD=3
      MCTL=5
      ILEN=n_ctl_periods(1,icfctype,idtyp)*3
      IPACT=1!CREATE
      CALL EKPAGE(IPACT)
      IER=0
      IVERT=-3

C Check period data and plot CFC schedules
      call chkCFC_period_data(icomp,icfctype)
      if (MMOD.eq.8) call plot_CFC_schedules(ICOMP,icfctype,idtyp)

      
C Set up top of menu.
  44  if (i_CFC_control(icomp,icfctype).eq.0) then 
        write(VERT(1),'(A)') '1 CFC shading control (none)'
      else
        write(VERT(1),'(A)') '1 control:  CFC control ON'
      endif
      vert(2) =              '  -----------------------------'
      write(vert(3),'(3a,i2,a)') '> CFC schedule >> ',
     & calentag(idtyp)(1:10),'(',n_ctl_periods(icomp,icfctype,idtyp),')'

C Loop through the items until the page to be displayed. M is the 
C current menu line index. Build up text strings for the menu.     
      M=MHEAD

C Day type loop
      istartcount=1
      iendcount=0
      do 10 L=1,n_ctl_periods(icomp,icfctype,idtyp)

C Ensure that all three lines can be written. If on subsequent
C pages then check loosely in order to catch an item that 
C would have otherwise spanned the page divide.
        iendcount=istartcount+2
        if(istartcount.GE.IST.AND.(iendcount.LE.(IST+MIFULL)))then
          continue
        elseif(istartcount+2.GE.IST.AND.
     &        (iendcount.LE.(IST+MIFULL)))then
          continue
        else
          istartcount=iendcount+1
          goto 10
        endif
        M=M+1
        call EMKEY(M-MHEAD,KEY,IER)
        write (VERT(M),'(2a,i3,a,i3)') KEY,' period:',
     &    i_ctl_start(icomp,icfctype,idtyp,L),'-',
     &    i_ctl_end(icomp,icfctype,idtyp,L)
        M=M+1
        call EMKEY(M-MHEAD,KEY,IER)
        write (VERT(M),'(2a,i3)') KEY,'  Shade ON/OFF:',
     &            i_shdONorOFF(icomp,icfctype,idtyp,L)   
        M=M+1
        call EMKEY(M-MHEAD,KEY,IER)
        write (VERT(M),'(2a,f6.2)') KEY,'  Slat angle:',
     &            slat_angle(icomp,icfctype,idtyp,L)   
 
        istartcount=iendcount+1
 10   continue

C Number of actual items displayed.
      MVERT=M+MCTL

C If a long list include page facility text.      
      IF(IPFLG.EQ.0)THEN
        VERT(M+1)=           '  -----------------------------'
      ELSE
        WRITE(VERT(M+1),15)IPM,MPM 
   15   FORMAT             ('0 Page --- Part: ',I2,' of ',I2,' ---')
      ENDIF
      VERT(M+2) = '+ add/delete/copy/import period'
      VERT(M+3) = '! list current information     '
      VERT(M+4) = '? help                         '
      VERT(M+5) = '- exit this menu               '

C Now display the menu.      
      write(title,'(a,i2,2a)')'CFC type',icfctype,
     & ' schedule in ',zname(icomp)(1:lnblnk(zname(icomp)))
      CALL EMENU(title,vert,mvert,IVERT)

C Help text for this menu.
      H(1)='CFC shading schedule help'
      
      if (IVERT.EQ.MVERT) then
        return
      elseif (IVERT.EQ.(MVERT-1)) then
        CALL PHELPD('CFC shading schedule',1,'-',0,0,IER)
      elseif (IVERT.EQ.(MVERT-2)) then
C List current
      ! nothing here yet
      elseif (IVERT.EQ.(MVERT-3)) then

C Add delete copy period or import from another zone

        H(1)='The options: '
        H(2)='The add/delete/copy/import option are as follows.'
        H(3)=' add - adds a period to one or all day types.' 
        H(4)=' '
        H(5)=' delete - deletes one or more periods (within a list'
        H(6)='   of all periods in all days).'
        H(7)=' '
        H(8)=' copy - copies periods for different days e.g. Weekdays'
        H(9)='   -> Sundays or Saturdays -> Sundays'
        CALL EASKATOG(' ','Options (see help):',
     &    'add period','delete period','copy period',
     &    'import from other zone','import pattern','cancel',' ',IW,9)

C Add period
        if (IW.EQ.1) then 
          CALL EASKAB(' ','Add period to ','one day type',
     &     'all day types',IY,35)
          IF(IY.EQ.1)THEN
            IX=1
            CALL EPICKS(IX,KSTR,' ','choose daytype to add period in'
     &      ,12,nbdaytype,calentag,'Choose daytype',IER,35)
            call edisp(iuout,'choose daytype to add period in')
            IDT=KSTR(1)
            idtyp=idt
            IF(n_ctl_periods(icomp,icfctype,IDT).EQ.MA)THEN
              CALL USRMSG('Sorry, too many periods in day type ',
     &        calentag(idt),'W')
            ELSE
              IPER=0
              CALL editCFC_period_data(ICOMP,icfctype,IDT,IPER,-1)
            ENDIF
          ELSEIF(IY.EQ.2)THEN
            IDT=-1
            DO 400 IDTY=1,NBDAYTYPE
            IF(n_ctl_periods(icomp,icfctype,IDTY).EQ.MA)THEN
              CALL USRMSG('Sorry, too many periods in day type ',
     &        calentag(idt),'W')
              CALL EDISP(IUOUT,
     &        'Add periods by individually choosing each day type')
              IDT=0
            ENDIF
 400        CONTINUE
            IF(IDT.NE.0)THEN
              IDT=-1
              IPER=0
              call editCFC_period_data(ICOMP,icfctype,IDT,IPER,-1)
            ENDIF
          ENDIF
        elseif (IW.EQ.2) then 
          call cpdelCFC_period_data(icomp,icfctype,1)    ! delete one or more periods
        elseif (IW.EQ.3) then
          call cpdelCFC_period_data(icomp,icfctype,2)    ! copy a period
        elseif (IW.EQ.4.or.IW.eq.5) then

            
        elseif(iw.eq.5)then

        endif
        
      elseif (IVERT.EQ.(MVERT-4)) then

C If there are enough items allow paging control via EKPAGE.
        IF(IPFLG.EQ.1)THEN
          IPACT=EDIT
          CALL EKPAGE(IPACT)
          ILEN=n_ctl_periods(1,icfctype,idtyp)*3
          goto 44
        ENDIF
      elseif (IVERT.EQ.1) then

C CFC Control.
C        call EDAFCTL(ICOMP,IER)
      elseif (IVERT.EQ.3) then

C Increment the daytype focus.
        idtyp = idtyp + 1
        if(idtyp.gt.nbdaytype) idtyp = 1
        goto 5
      elseif (IVERT.GT.MHEAD.AND.IVERT.LT.(MVERT-MCTL+1)) then

C Edit block identified by KEYIND.
        CALL KEYIND(MVERT,IVERT,I,IO)

C Depending on currently defined gains work out day type of selected
C item. Also calculate period index in day type. Adjust for 2nd page
        if(ipm.eq.2.and.I.gt.1) I=I-1  ! adjust for 2nd page
        if(ipm.eq.3.and.I.gt.1) I=I-2  ! adjust for 3rd page
        if(ipm.eq.4.and.I.gt.1) I=I-3  ! adjust for 4th page

C Items are in groups of 3. If i/3 has no remainder I is 3,6,9,etc.
C so the period is just the nearest integer to I/3.0. If there is
C a remainder then I is 1,2 or 4,5 and it is one more than the
C nearest integer.
        trymod=amod(real(I),3.0)
        call eclose(trymod,0.0,0.001,closezero)
        call eclose(trymod,1.0,0.001,closeone)
        call eclose(trymod,2.0,0.001,closetwo)
        if(closezero.or.closetwo)then
          try=real(I)/3.0
          iper=nint(try)
        endif
        if(closeone)then
          try=real(I)/3.0
          iper=nint(try)+1
        endif
        IDT=idtyp
        if(closeone)then
          call editCFC_period_data(ICOMP,icfctype,IDT,IPER,1)
        endif
        if(closetwo)then
          call editCFC_period_data(ICOMP,icfctype,IDT,IPER,2)
        endif
        if(closezero)then
          call editCFC_period_data(ICOMP,icfctype,IDT,IPER,0)
        endif
      endif
      
C Redisplay menu.
      IVERT=-4
      GOTO 5
      
      END

C ********************************************************************
C                          --askCFCtype--
C
C Presents a list of CFC types to select from and returns icfctype
C UNDER CONSTRUCTION
C Based on subroutine ASKZONE
C
C     - check if *.cfc file exists for this zone
C     - determine number of CFC types
C
C ********************************************************************
      
C ASKZONE presents  a list of zones to select from.
C It is passed a prompt, menu title, default
C index and indicates current index (if zero then says no prior
C selection). It is assumed that the user will make one selection only.
C MOD display modifier - `c` note constructions defined, `o` note
C operations defined, `s` note shading defined, `z` note zone attrib
C complete, `v` note view factors, `g` casual gain ctl, `d` domain flow,
C `h` heat transfer (comvective) methods, `-` zone names only.
      subroutine askCFCtype(icomp,icfctype,IER)
#include "building.h"
#include "geometry.h"
#include "model.h"
#include "epara.h"
#include "CFC_common.h"

      COMMON/C24/IZSTOCN(MCOM,MS) !array which holds the connection index
      integer izstocn

      COMMON/FILEP/IFIL
c      COMMON/precz/zname(MCOM),zdesc(MCOM)
      
      COMMON/C1/NCOMP,NCON
      integer ncomp,ncon

c      COMMON/C2/LSNAM,NCCODE(MCOM),LPROJ(MCOM),LGEOM(MCOM),
c     &          LSHAD(MCOM),LTHRM(MCOM),INDUTL(MCOM),LUTIL(MCOM)
c      integer nccode,indutl
c      character LSNAM*72,LPROJ*72,LGEOM*72,LSHAD*72,LTHRM*72,LUTIL*72

c      common/UDESC/LVIEW(MCOM),LHCCO(MCOM),
c     &             LTWIN(MCOM),LCGCIN(MCOM),ZOBS(MCOM)
      common/cfdfil/LCFD(MCOM),IFCFD(MCOM)

C Version of operations file. ip3ver=0 standard, =1 sorted with header
C =2 with more than 3 day types allowed and sorted
      common/p3ver/ip3ver(MCOM)

      LOGICAL SELECT,XST
      CHARACTER title*72
      
      DIMENSION VERT(35)
      CHARACTER VERT*27,KEY*1
      
      character OUTSTR*124
      integer IUA, NS, K, M, L, i, j, icn, II, IO
      integer MVERT,IVERT ! max items and current menu item

      DIMENSION IVA(MS)

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      helpinsub='askCFCtype'  ! set for subroutine
      
      IUA=IFIL+1
      ncfctypes=0
      nlast=0
      
      XST=.false.

      SELECT=.FALSE.
   5  MHEAD=0
      MCTL=3

C Initial menu entry setup.
c   92 IER=0
      IER=0
      IVERT=-3

C find *.cfc file for this zone and read it in      
      call FINDFIL(lcfcin(icomp),XST)
      
      IF(XST.and.icfc(icomp).eq.1)then

C Read CFC type index from zone *.cfc file
        CALL EFOPSEQ(IUA,lcfcin(icomp),1,IER)
        IF(IER.NE.0)THEN
         IER=1
         goto 1000
        ENDIF

C read in number of surfaces in zone
        CALL STRIPC(IUA,OUTSTR,1,ND,1,'no zone surfaces',IER)
        IF(IER.NE.0)GOTO 1001
        K=0
        CALL EGETWI(OUTSTR,K,NS,4,MS,'W','no zone cfc surf',IER)

C Read pointer to type of CFC for each surface, strip comments etc.
C Determine number of CFC types in zone
        IRVA=NS
        CALL EGETWIA(IUA,IVA,IRVA,0,MCFC,'W','cfc list',IER)
        ncfc(icomp)=0
        DO 10 I=1,NS
          icfcfl(icomp,i)=iva(i)
          IF(icfcfl(icomp,i).GT.ncfc(icomp))
     &    ncfc(icomp)=icfcfl(icomp,i)
   10   CONTINUE

        ILEN=ncfc(icomp)
        IPACT=1!CREATE
        CALL EKPAGE(IPACT)
        
        M=MHEAD
        do 20 L=1,ILEN
            M=M+1
            CALL EMKEY(L,KEY,IER)
            WRITE(VERT(M),'(a1, 1x, a10, i2)')KEY, 'CFC type ', L
   20   continue

C write out corresponding surfaces to each CFC type in zone
        VERT(M+1)=  '  ______________________'
        VERT(M+2)=  'Corresponding surface(s)'
        VERT(M+3)=' '
        M=M+3

        do 30 LL=1,ILEN
        M=M+1
          WRITE(VERT(M),'(a10, i2)')
     &         '  CFC type', LL

          do 40 j=1,NS
            if(icfcfl(icomp,j).eq.LL)then
              icn=izstocn(icomp,j)
              M=M+1
              WRITE(VERT(M), '(a6,a12)')'',SSNAME(icn)
            endif
   40     continue

   30   continue
   

C If a long list include page facility text and info on portion seen.      
        IF(IPFLG.EQ.0)THEN
          VERT(M+1)=  '  ______________________'
        ELSE
          WRITE(VERT(M+1),15)IPM,MPM 
   15     FORMAT ('0 page part: ',I1,' -- of:',I1)
        ENDIF

        VERT(M+2)  ='? help                  '
        VERT(M+3)  ='- exit this menu        '

C Number of actual items displayed
        MVERT=M+MCTL
        
C Help message for this menu.
        helptopic='CFC_type_select'
        call gethelptext(helpinsub,helptopic,nbhelp)

C Display the menu.
        write(title,'(2a)')'Select CFC type in ',
     &  zname(icomp)(1:lnblnk(zname(icomp)))
        CALL EMENU(title,VERT,MVERT,IVERT)        
        
C Get user selection
        if (IVERT.EQ.MVERT) then
          return
        elseif (IVERT.EQ.(MVERT-1)) then
          helptopic='CFC_type_select'
          call gethelptext(helpinsub,helptopic,nbhelp)
          CALL PHELPD('Choose CFC type',nbhelp,'-',0,0,IER)
        elseif (IVERT.GT.MHEAD.AND.IVERT.LT.(ILEN+1)) then
          icfctype=IVERT
          goto 1000
        else
          !do nothing
        end if
      
C Redisplay menu.
      IVERT=-4
      GOTO 5      

      ELSE
      
        CALL PHELPD('CFC file not found',nbhelp,'-',0,0,IER)      
        
      ENDIF
 
c Free file.
 1000 CALL ERPFREE(IUA,ISTAT)
      return

C Error messages.
 1001 CALL USRMSG(' Problem reading data in:',OUTSTR,'W')
      IER=1
      goto 1000
C 1002 write(outs,'(a,a)') ' Please check data in: ',LUA(1:lnblnk(LUA))
C      CALL USRMSG(outs,' and try the model again!','W')
C      IER=1
C      goto 1000 
      
      END

C ********************************************************************
C                          --plot_CFC_schedules--
C
C Plots CFC shading schedule
C UNDER CONSTRUCTION
C Based on subroutine PLTAIR
C
C ********************************************************************
      subroutine plot_CFC_schedules(ICOMP,icfctype,idtyp)
#include "building.h"
#include "geometry.h"
#include "CFC_common.h"

      integer menuchw,igl,igr,igt,igb,igw,igwh
      COMMON/VIEWPX/menuchw,igl,igr,igt,igb,igw,igwh
c      COMMON/PREC2/VOL(MCOM)
c      COMMON/precz/zname(MCOM),zdesc(MCOM)

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT,IDTY,IDAY,JDAY,KDAY
      REAL ACI,ACV,TA

      character outs*124
      dimension TACI(24),TACV(24)
#ifdef OSX
      integer ipltwd,igheight  ! for use with axiscale
      integer iside,isize,ifont     ! passed to viewtext
      integer iupdown,isym    ! passed to etplot
#else
      integer*8 ipltwd,igheight  ! for use with axiscale
      integer*8 iside,isize,ifont     ! passed to viewtext
      integer*8 iupdown,isym    ! passed to etplot
#endif

      IDTY=IDTYP
      IDAY=IDTY-1
      JDAY=IDTY
      KDAY=IDTY+1
      IF(IDAY.LE.0)IDAY=NBDAYTYPE
      IF(KDAY.GT.NBDAYTYPE)KDAY=1

C Scan each days gains and find maximum flow.
      YMAX=1.
      MXPERS=max(n_ctl_periods(icomp,icfctype,IDAY),
     &           n_ctl_periods(icomp,icfctype,JDAY),
     &           n_ctl_periods(icomp,icfctype,KDAY))
      do 10 I=1,MXPERS
       YMAX=amax1(YMAX,slat_angle(icomp,icfctype,IDAY,I),
     &  slat_angle(icomp,icfctype,JDAY,I),
     &  slat_angle(icomp,icfctype,KDAY,I))
 10   continue

C Clear and setup plot area.
      CALL startbuffer()
      CALL win3d(menuchw,10,10,8,3,igl,igr,igt,igb,igw,igwh)
      igw=igr-igl
      igwh=igb-igt
      igheight=igwh

C Calculate plotting box locations.
      ipltwd=int(igw/3)-6
      iglsat=igl + int(ipltwd) + 9
      iglsun=igl+2*(int(ipltwd)+9)
      plxmin=0.
      plxmax=24.
      YMAX=90.
      YMIN=-YMAX
      YMAXF=YMAX

C Scale and draw right hand ON/OFF axis first.
      CALL axiscale(ipltwd,igheight,plxmin,plxmax,YMIN,YMAXF,
     &                                         xsc,ysc,sca,xadd,yadd)
      call vrtaxis(0.,1.,igr,igb,igt,0.,20.,0,1,'Shade ON/OFF')

C Scale and draw for first day type requested
      CALL horaxis(plxmin,plxmax,igl,igr,igb,xadd,xsc,0,'Time (hrs)')
      call vrtaxis(YMIN,YMAX,igl,igb,igt,yadd,ysc,0,0,
     &             'Slat Angle (deg)')
      call linescale(igl,xadd,xsc,igb,yadd,ysc)
      iupdown=0
      isym=0
      CALL etplot(plxmax,YMIN,iupdown,isym)
      iupdown=1
      CALL etplot(plxmax,YMAX,iupdown,isym)
      CALL etplot(plxmin,YMAX,iupdown,isym)

C Draw 0 axis line
      CALL etplot(plxmin,0.,-1,isym)      
      CALL etplot(plxmax,0.,-1,isym)
      
C Scale and draw second day type requested
      CALL horaxis(plxmin,plxmax,iglsat,igr,igb,xadd,XSC,0,' ')
      call linescale(iglsat,xadd,xsc,igb,yadd,ysc)
      iupdown=0
      isym=0
      CALL etplot(plxmax,YMIN,iupdown,isym)
      iupdown=1
      CALL etplot(plxmax,YMAX,iupdown,isym)
      CALL etplot(plxmin,YMAX,iupdown,isym)
      CALL etplot(plxmin,YMIN,iupdown,isym)

C Draw 0 axis line
      CALL etplot(plxmin,0.,-1,isym)      
      CALL etplot(plxmax,0.,-1,isym)

C Scale and draw third day type requested
      CALL horaxis(plxmin,plxmax,iglsun,igr,igb,xadd,XSC,0,' ')
      call linescale(iglsun,xadd,xsc,igb,yadd,ysc)
      iupdown=0
      isym=0
      CALL etplot(plxmin,YMIN,iupdown,isym)
      iupdown=1
      CALL etplot(plxmin,YMAX,iupdown,isym)
      CALL etplot(plxmax,YMAX,iupdown,isym)

C Draw 0 axis line
      iupdown=0
      CALL etplot(plxmin,0.,0,isym)      
      iupdown=1
      CALL etplot(plxmax,0.,-1,isym)

C Draw right hand axis on top of scaled ON/OFF axis to fill in gap
      iupdown=0
      CALL etplot(plxmax,YMIN,iupdown,isym)      
      iupdown=1
      CALL etplot(plxmax,YMAX,iupdown,isym)     
      
C Display slat angle and shading ON/OFF curves.
C first day type requested

      do 50 IHR=1,24
        TACI(IHR)=0.
        TACV(IHR)=0.
 50   continue
      do 60 IP=1,n_ctl_periods(icomp,icfctype,IDAY)
        do 65 IHR=i_ctl_start(icomp,icfctype,IDAY,IP),
     &            i_ctl_end(icomp,icfctype,IDAY,IP)-1
          TACI(IHR+1)=real(i_shdONorOFF(icomp,icfctype,IDAY,IP))
          TACV(IHR+1)=slat_angle(icomp,icfctype,IDAY,IP)
 65     continue
 60   continue
      do 70 IHR=1,24
        XST=real(IHR-1)
        XFN=real(IHR)
        call linescale(igl,xadd,xsc,igb,0.,20.)
        iupdown=0
        isym=0
        CALL etplot(XST,TACI(IHR),iupdown,isym)
        iupdown=-4
        CALL etplot(XFN,TACI(IHR),iupdown,isym)
        if (IHR.ne.24)then
          CALL etplot(XFN,TACI(IHR+1),iupdown,isym)
        endif
        call linescale(igl,xadd,xsc,igb,yadd,ysc)
        iupdown=0
        isym=0
        CALL etplot(XST,TACV(IHR),iupdown,isym)
        iupdown=1
        CALL etplot(XFN,TACV(IHR),iupdown,isym)
        if (IHR.ne.24)then
          CALL etplot(XFN,TACV(IHR+1),iupdown,isym)
        endif
 70   continue

C second day type requested
      do 150 IHR=1,24
        TACI(IHR)=0.
        TACV(IHR)=0.
 150  continue
      do 160 IP=1,n_ctl_periods(icomp,icfctype,JDAY)
       do 165 IHR=i_ctl_start(icomp,icfctype,JDAY,IP),
     &            i_ctl_end(icomp,icfctype,JDAY,IP)-1
          TACI(IHR+1)=real(i_shdONorOFF(icomp,icfctype,JDAY,IP))
          TACV(IHR+1)=slat_angle(icomp,icfctype,JDAY,IP)
 165    continue
 160  continue
      do 170 IHR=1,24
        XST=real(IHR-1)
        XFN=real(IHR)
        call linescale(iglsat,xadd,xsc,igb,0.,20.)
        iupdown=0
        isym=0
        CALL etplot(XST,TACI(IHR),iupdown,isym)
        iupdown=-4
        CALL etplot(XFN,TACI(IHR),iupdown,isym)
        if (IHR.ne.24)then
          CALL etplot(XFN,TACI(IHR+1),iupdown,isym)
        endif
        call linescale(iglsat,xadd,xsc,igb,yadd,ysc)
        iupdown=0
        isym=0
        CALL etplot(XST,TACV(IHR),iupdown,isym)
        iupdown=1
        CALL etplot(XFN,TACV(IHR),iupdown,isym)
        if (IHR.ne.24)then
          CALL etplot(XFN,TACV(IHR+1),iupdown,isym)
        endif
 170  continue
      call forceflush()

C third day type requested
      do 250 IHR=1,24
        TACI(IHR)=0.
        TACV(IHR)=0.
 250  continue
      do 260 IP=1,n_ctl_periods(icomp,icfctype,KDAY)
       do 265 IHR=i_ctl_start(icomp,icfctype,KDAY,IP),
     &            i_ctl_end(icomp,icfctype,KDAY,IP)-1
          TACI(IHR+1)=real(i_shdONorOFF(icomp,icfctype,KDAY,IP))
          TACV(IHR+1)=slat_angle(icomp,icfctype,KDAY,IP)
 265    continue
 260  continue
      do 270 IHR=1,24
        XST=real(IHR-1)
        XFN=real(IHR)
        call linescale(iglsun,xadd,xsc,igb,0.,20.)
        iupdown=0
        isym=0
        CALL etplot(XST,TACI(IHR),iupdown,isym)
        iupdown=-4
        CALL etplot(XFN,TACI(IHR),iupdown,isym)
        if (IHR.ne.24)then
          CALL etplot(XFN,TACI(IHR+1),iupdown,isym)
        endif
        call linescale(iglsun,xadd,xsc,igb,yadd,ysc)
        iupdown=0
        isym=0
        CALL etplot(XST,TACV(IHR),iupdown,isym)
        iupdown=1
        CALL etplot(XFN,TACV(IHR),iupdown,isym)
        if (IHR.ne.24)then
          CALL etplot(XFN,TACV(IHR+1),iupdown,isym)
        endif
 270  continue

C Title and labels.
      write (outs,'(a,i2,2a)') 'Shading schedule for CFC type ',
     &                    icfctype,' in ',ZNAME(ICOMP)
      iside=1
      isize=1
      ifont=1
      call viewtext(outs,iside,isize,ifont)

      call wstxpt(igl+20,igt-65,'Shading ON/OFF')
      call wstxpt(igr-100,igt-65,'Slat Angle')
      call pixel2u(igl+120,igt-69,gx,gy)
      iupdown=0
      isym=0
      CALL etplot(gx,gy,iupdown,isym)
      call pixel2u(igl+180,igt-69,gx,gy)
      iupdown=-4
      CALL etplot(gx,gy,iupdown,isym)
      call pixel2u(igr-120,igt-69,gx,gy)
      iupdown=0
      CALL etplot(gx,gy,iupdown,isym)
      call pixel2u(igr-180,igt-69,gx,gy)
      iupdown=1
      CALL etplot(gx,gy,iupdown,isym)

      ioff=(int(ipltwd)/3)
      call wstxpt(igl+ioff,igt-10,CALENTAG(IDAY))
      call wstxpt(iglsat+ioff,igt-10,CALENTAG(JDAY))
      call wstxpt(iglsun+ioff,igt-10,CALENTAG(KDAY))
      call forceflush()

      return
      end

C ********************************************************************
C                        --editCFC_period_data--
C
C Edit CFC shading schedule period data
C UNDER CONSTRUCTION
C Based on subroutine EDAIRFL
C
C  ICOMP zone number
C  IDT day type
C  IPER period (=0 then new)
C  itype is what attributes to edit (=-1 all, =1 period,
C     =2 shade ON/OFF, =0 slat angle.
C ********************************************************************
      subroutine editCFC_period_data(ICOMP,icfctype,IDT,IPER,itype)
#include "building.h"
#include "geometry.h"
#include "CFC_common.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      common/pophelp/h(60)
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

c      COMMON/precz/zname(MCOM),zdesc(MCOM)

c      COMMON/PREC2/VOL(MCOM)
      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT
      REAL ACI,ACV,TA
     
      character OUTS*124,HOLD*16,h*72

C Working variables
      real slatX         !shade ON/OFF, slat angle
      integer IctlSX,IctlFX,ishdX   !period start, period end
      real Xslat
      
C << This subroutine will need further code changes to implement
C << user cancel actions.

C If new period then set defaults otherwise get data from common.
      if (IPER.eq.0) then
        IctlSX=0
        IctlFX=24
        ishdX=0
        slatX=0.
      ELSEIF(IDT.GT.0)THEN
        IctlSX=i_ctl_start(icomp,icfctype,IDT,IPER)
        IctlFX=i_ctl_end(icomp,icfctype,IDT,IPER)
        ishdX=i_shdONorOFF(icomp,icfctype,IDT,IPER)
        slatX=slat_angle(icomp,icfctype,IDT,IPER)
      endif

C Ask for start and finish times.
      if(itype.eq.-1.or.itype.eq.1)then
 10     write (OUTS,'(A)') 'Start and finish hours for period?'
        write (HOLD,'(2i5,a)') IctlSX,IctlFX,'  '
        h(1)='Period during which CFC shading schedule applies.'
        CALL EASKS(HOLD,OUTS,' ',16,' 0 24 ','CFC st-fn hr',IER,1)
        K=0
        CALL EGETWI(HOLD,K,IVS,0,24,'F','period start',IER)
        CALL EGETWI(HOLD,K,IVF,IVS+1,24,'F','period end',IER)
        if (IER.ne.0) goto 10
        IctlSX=IVS
        IctlFX=IVF
      endif

C Ask for shading ON/OFF and/or slat angle.
      if(itype.eq.-1.or.itype.eq.2.or.itype.eq.0)then
        if(itype.eq.0)goto 11
        h(1)='Specify whether CFC shading for current period is ON'
        h(2)='or OFF (ie. is shading layer deployed or retracted).'
        IGU=1
        CALL EASKAB(' ','CFC shading layer','ON',
     &     'OFF',IGU,2)
        if (IGU.eq.1) then
          ishdX=1
        elseif(IGU.eq.2)then
          ishdX=0
        endif

  11    if(itype.eq.-1.or.itype.eq.0)then
  20        Xslat=slatX
            h(1)='Supply the slat angle in degrees from horizontal.'
            call EASKR(Xslat,' ','Slat angle?',
     &        -89.9,'F',89.9,'F',0.0,'slat angle (deg)',IERI,1)
            if(IERI.eq.0)then
              slatX=Xslat
            elseif(ieri.eq.-3)then
              call edisp(iuout,'Cancel not implemented.')
            else
              goto 20
            endif          
        endif

      endif

C If day type -1 then add data to all day types.
      if (IDT.eq.-1) then
        ILOOP=NBDAYTYPE
      else
        ILOOP=1
      endif

C If new period then work out where to insert data.
      do 400 ILDT=1,ILOOP

C If looping through day types then set day type (IDT) to current 
C value of loop counter (ILDT).  Otherwise IDT will have the value 
C that was passed to the subroutine.
        if (ILOOP.NE.1) IDT=ILDT
        call addCFC_period_data
     &   (icomp,icfctype,IPER,IDT,IctlSX,IctlFX,ishdX,slatX)
 400  continue

      return
      end

C ********************************************************************
C                        --addCFC_period_data--
C
C Add CFC shading schedule period data in correct location (time based).
C Other routines assume that periods have been added in the order 
C resulting from the sorting done here.
C UNDER CONSTRUCTION
C Based on subroutine ADDAFP
C
C IPER=0 if its a new period
C IDT=day type wkday=1, sat=2, sun=3, other types if specified
C IctlS,IctlF period start and finish time
C shdON, slat_angle - shading ON/OFF and shade layer slat angle
C IPT,TA ventilation source zone, if IPT=0 then TA is source temperature
C ********************************************************************
      subroutine addCFC_period_data
     &(icomp,icfctype,IPER,IDT,IctlSX,IctlFX,ishdONX,slat_angleX)
#include "building.h"
#include "CFC_common.h"

      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT
      REAL ACI,ACV,TA

      integer ILPER, I, INC
      integer icomp,icfctype,IPER,IDT,IctlSX,IctlFX,ishdONX
      real slat_angleX
      
      if (IPER.eq.0) then
        ILPER=1
        DO 401 I=1,n_ctl_periods(icomp,icfctype,IDT)
          if (iabs(i_ctl_start(icomp,icfctype,IDT,I)).le.
     &    iabs(IctlSX)) then
            ILPER=I
            if (i_ctl_start(icomp,icfctype,IDT,I).lt.IctlSX) then
              ILPER=I+1
            elseif(i_ctl_start(icomp,icfctype,IDT,I).eq.IctlSX.and.
     &      i_ctl_end(icomp,icfctype,IDT,I).gt.IctlFX)then
              ILPER=I
            endif
          endif
 401    CONTINUE
        n_ctl_periods(icomp,icfctype,IDT)=
     &  n_ctl_periods(icomp,icfctype,IDT)+1
        INC=n_ctl_periods(icomp,icfctype,IDT)
      else
        ILPER=IPER
        INC=IPER
      endif

C Now update common blocks.
      do 300 IX=INC,ILPER,-1
        if (IX.eq.ILPER) then
          i_ctl_start(icomp,icfctype,IDT,IX)=IctlSX
          i_ctl_end(icomp,icfctype,IDT,IX)=IctlFX
          i_shdONorOFF(icomp,icfctype,IDT,IX)=ishdONX
          slat_angle(icomp,icfctype,IDT,IX)=slat_angleX
        else
          i_ctl_start(icomp,icfctype,IDT,IX)=
     &    i_ctl_start(icomp,icfctype,IDT,IX-1) 
          i_ctl_end(icomp,icfctype,IDT,IX)=
     &    i_ctl_end(icomp,icfctype,IDT,IX-1) 
          i_shdONorOFF(icomp,icfctype,IDT,IX)=
     &    i_shdONorOFF(icomp,icfctype,IDT,IX-1)
          slat_angle(icomp,icfctype,IDT,IX)=
     &    slat_angle(icomp,icfctype,IDT,IX-1) 
        endif
 300  continue

      return
      end      

C ********************************************************************
C                        --chkCFC_period_data--
C
C Check CFC shading period data for overlapping periods.
C Based on subroutine CHKAIRFL
C
C ********************************************************************
      subroutine chkCFC_period_data(icomp,icfctype)
#include "building.h"
#include "CFC_common.h"

      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER
      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT
      REAL ACI,ACV,TA
      
      logical RECHECK

      integer Ifrom, Ito, Inew, I, J
      
 5    RECHECK=.false.
      Ifrom=0
      Ito=0
      Inew=0

C Check all daytypes.
      DO 400 IDTY=1,NBDAYTYPE

C I is the earlier of the two periods being checked.
C J is the later of the two periods.
C If periods I and J overlap (start time for J within period I) then
C copy data for all periods up one slot (if room) and insert new period.
C Therefore the data in period J becomes J+1 and the new period J is the 
C overlapping period of the two initial periods.
      do 10 I=1,n_ctl_periods(icomp,icfctype,IDTY)
        if (I+1.le.n_ctl_periods(icomp,icfctype,IDTY)) then
        do 15 J=I+1,n_ctl_periods(icomp,icfctype,IDTY)
          if (i_ctl_start(icomp,icfctype,IDTY,J).ge.
     &    i_ctl_start(icomp,icfctype,IDTY,I).and.
     &    i_ctl_start(icomp,icfctype,IDTY,J).lt.
     &    i_ctl_end(icomp,icfctype,IDTY,I)) then

C Start time of period J is between start and finish times of period I.
C Create a new period by splitting period J, only if start or finish time 
C of overlapping periods in not equal.
C     |-------------------------|
C     |                         |
C     ||.or.|-----------| ..or.||.or.| 
C     ||    |           |      ||    |
C  -----------------------------

            if (i_ctl_start(icomp,icfctype,IDTY,J).eq.
     &      i_ctl_start(icomp,icfctype,IDTY,I).and.
     &      i_ctl_end(icomp,icfctype,IDTY,J).gt.
     &      i_ctl_end(icomp,icfctype,IDTY,I)) then
C
C     |----I------------|
C     |                 |
C     ||---J------- ----|--| J
C     ||                |  |
C  -----------------------------
              Ifrom=J
              Ito=I
              Inew=I
              i_ctl_start(icomp,icfctype,IDTY,J)=
     &        i_ctl_end(icomp,icfctype,IDTY,I)
            elseif (i_ctl_start(icomp,icfctype,IDTY,J).eq.
     &      i_ctl_start(icomp,icfctype,IDTY,I).and.
     &      i_ctl_end(icomp,icfctype,IDTY,J).lt.
     &      i_ctl_end(icomp,icfctype,IDTY,I)) then

C
C     |----I------------|
C     |                 |
C     ||---J-------|    |
C     ||           |    |  
C  -----------------------------
              Ifrom=J
              Ito=I
              Inew=I

C By starting period I at finish of J periods will be out of order,
C so swap period data from I to J and J to I.
              i_ctl_start(icomp,icfctype,IDTY,I)=
     &        i_ctl_end(icomp,icfctype,IDTY,J)
              i_ctl_startTMP=i_ctl_start(icomp,icfctype,IDTY,I)
              i_ctl_endTMP=i_ctl_end(icomp,icfctype,IDTY,I)
              i_shdONorOFF_TMP=i_shdONorOFF(icomp,icfctype,IDTY,I)
              slat_angleTMP=slat_angle(icomp,icfctype,IDTY,I)
              i_ctl_start(icomp,icfctype,IDTY,I)=
     &        i_ctl_start(icomp,icfctype,IDTY,J)
              i_ctl_end(icomp,icfctype,IDTY,I)=
     &        i_ctl_end(icomp,icfctype,IDTY,J)
              i_shdONorOFF(icomp,icfctype,IDTY,I)=
     &        i_shdONorOFF(icomp,icfctype,IDTY,J)
              slat_angle(icomp,icfctype,IDTY,I)=
     &        slat_angle(icomp,icfctype,IDTY,J)
              i_ctl_start(icomp,icfctype,IDTY,J)=i_ctl_startTMP
              i_ctl_end(icomp,icfctype,IDTY,J)=i_ctl_endTMP
              i_shdONorOFF(icomp,icfctype,IDTY,J)=i_shdONorOFF_TMP
              slat_angle(icomp,icfctype,IDTY,J)=slat_angleTMP
 
            elseif (i_ctl_end(icomp,icfctype,IDTY,J).eq.
     &      i_ctl_end(icomp,icfctype,IDTY,I).and.
     &      i_ctl_start(icomp,icfctype,IDTY,J).ne.
     &      i_ctl_start(icomp,icfctype,IDTY,I)) then

C     |---I------------|
C     |                |
C     |   |---J-------||
C     |.  |           ||
C  -----------------------------
C Finishes are the same and starts are different.
              Ifrom=I
              Ito=J
              Inew=J
              i_ctl_end(icomp,icfctype,IDTY,I)=
     &        i_ctl_start(icomp,icfctype,IDTY,J)

            elseif (i_ctl_start(icomp,icfctype,IDTY,J).gt.
     &      i_ctl_start(icomp,icfctype,IDTY,I).and.
     &      i_ctl_end(icomp,icfctype,IDTY,J).lt.
     &      i_ctl_end(icomp,icfctype,IDTY,I)) then

C     |---I------------|
C     |                |
C     |...|---J-----|  |
C     |.  |         |  |
C  -----------------------------
C Later period starts after earlier period and later period finishes before
C the earlier period.
              if (n_ctl_periods(icomp,icfctype,IDTY).lt.MA) then
                Ifrom=I
                Ito=J
                Inew=J
                do 16 K=n_ctl_periods(icomp,icfctype,IDTY),J,-1
                  i_ctl_start(icomp,icfctype,IDTY,K+1)=
     &            i_ctl_start(icomp,icfctype,IDTY,K)
                  i_ctl_end(icomp,icfctype,IDTY,K+1)=
     &            i_ctl_end(icomp,icfctype,IDTY,K)
                  i_shdONorOFF(icomp,icfctype,IDTY,K+1)=
     &            i_shdONorOFF(icomp,icfctype,IDTY,K)
                  slat_angle(icomp,icfctype,IDTY,K+1)=
     &            slat_angle(icomp,icfctype,IDTY,K)
 16             continue
                n_ctl_periods(icomp,icfctype,IDTY)=
     &          n_ctl_periods(icomp,icfctype,IDTY)+1
                i_ctl_end(icomp,icfctype,IDTY,J+1)=
     &          i_ctl_end(icomp,icfctype,IDTY,I)
                i_ctl_end(icomp,icfctype,IDTY,I)=
     &          i_ctl_start(icomp,icfctype,IDTY,J)
                i_ctl_start(icomp,icfctype,IDTY,J+1)=
     &          i_ctl_end(icomp,icfctype,IDTY,J)
                i_shdONorOFF(icomp,icfctype,IDTY,J+1)=
     &          i_shdONorOFF(icomp,icfctype,IDTY,I)
                slat_angle(icomp,icfctype,IDTY,J+1)=
     &          slat_angle(icomp,icfctype,IDTY,I)

              else
                Ifrom=0
                call USRMSG('CFC shading periods overlap and maximum',
     &                      'number of periods defined. ','W')
              endif

            elseif (i_ctl_start(icomp,icfctype,IDTY,J).gt.
     &      i_ctl_start(icomp,icfctype,IDTY,I).and.
     &      i_ctl_end(icomp,icfctype,IDTY,J).gt.
     &      i_ctl_end(icomp,icfctype,IDTY,I)) then

C     |---I------------|
C     |                |
C     |...|---J--------|--|
C     |.  |            |  |
C  -----------------------------
C If later period starts after earlier period and latter period finishes
C after the earlier period.
              if (n_ctl_periods(icomp,icfctype,IDTY).lt.MA) then
                Ifrom=J+1
                Ito=I
                Inew=J
                do 17 K=n_ctl_periods(icomp,icfctype,IDTY),J,-1
                  i_ctl_start(icomp,icfctype,IDTY,K+1)=
     &            i_ctl_start(icomp,icfctype,IDTY,K)
                  i_ctl_end(icomp,icfctype,IDTY,K+1)=
     &            i_ctl_end(icomp,icfctype,IDTY,K)
                  i_shdONorOFF(icomp,icfctype,IDTY,K+1)=
     &            i_shdONorOFF(icomp,icfctype,IDTY,K)
                  slat_angle(icomp,icfctype,IDTY,K+1)=
     &            slat_angle(icomp,icfctype,IDTY,K)
 17             continue
                n_ctl_periods(icomp,icfctype,IDTY)=
     &            n_ctl_periods(icomp,icfctype,IDTY)+1

                i_ctl_end(icomp,icfctype,IDTY,J)=
     &          i_ctl_end(icomp,icfctype,IDTY,I)
                i_ctl_end(icomp,icfctype,IDTY,I)=
     &          i_ctl_start(icomp,icfctype,IDTY,J)
                i_ctl_start(icomp,icfctype,IDTY,J+1)=
     &          i_ctl_end(icomp,icfctype,IDTY,J)

              else
                Ifrom=0
                call USRMSG('Air flow periods overlap and maximum',
     &                      'number of periods defined. ','W')
              endif
            else
              call USRMSG(
     & 'New CFC shading period defined for same period as existing',
     & 'period. Combining data, please check/ edit existing.','W')
              Ifrom=0

              i_shdONorOFF(icomp,icfctype,IDTY,I)=1
              slat_angle(icomp,icfctype,IDTY,I)=0.

              do 18 K=I,n_ctl_periods(icomp,icfctype,IDTY)
                if ((K+1).gt.MA) then
                  i_ctl_start(icomp,icfctype,IDTY,K)=0
                  i_ctl_end(icomp,icfctype,IDTY,K)=0
                  i_shdONorOFF(icomp,icfctype,IDTY,K)=0
                  slat_angle(icomp,icfctype,IDTY,K)=0.
                else
                  i_ctl_start(icomp,icfctype,IDTY,K)=
     &            i_ctl_start(icomp,icfctype,IDTY,K+1)
                  i_ctl_end(icomp,icfctype,IDTY,K)=
     &            i_ctl_end(icomp,icfctype,IDTY,K+1)
                  i_shdONorOFF(icomp,icfctype,IDTY,K)=
     &            i_shdONorOFF(icomp,icfctype,IDTY,K+1)
                  slat_angle(icomp,icfctype,IDTY,K)=
     &            slat_angle(icomp,icfctype,IDTY,K+1)
                endif
 18           continue
              n_ctl_periods(icomp,icfctype,IDTY)=
     &          n_ctl_periods(icomp,icfctype,IDTY)-1
            endif

C Try and set data in new period.
            if (Ifrom.gt.0) then
              i_shdONorOFF(icomp,icfctype,IDTY,Inew)=1
              slat_angle(icomp,icfctype,IDTY,Inew)=0.

C Jump out of loop and start checking again.
              RECHECK=.true.
              goto 10
            endif
          endif
 15     continue
        endif
 10   continue
 400  CONTINUE

C If changes have been made then recheck all definitions.
      if (RECHECK) goto 5
      return
      end

C ********************************************************************
C                        --cpdelCFC_period_data--
C
C Copy/Delete CFC shading period data
C ICD - copy (=2) delete (=1) flag
C Based on subroutine CDAIRFL
C
C ********************************************************************
      subroutine cpdelCFC_period_data(icomp,icfctype,ICD)
#include "building.h"
#include "geometry.h"
#include "CFC_common.h"

      common/pophelp/h(60)

c      COMMON/precz/zname(MCOM),zdesc(MCOM)
      common/calena/calename,calentag(MDTY),calendayname(MDTY)
      CHARACTER CALENAME*32,CALENTAG*12,CALENDAYNAME*32
      common/caleni/nbdaytype,nbcaldays(MDTY),icalender(365)
      INTEGER NBDAYTYPE,NBCALDAYS,ICALENDER

      COMMON/P2N/NAC(MDTY),IACS(MDTY,MA),IACF(MDTY,MA),ACI(MDTY,MA),
     &ACV(MDTY,MA),IPT(MDTY,MA),TA(MDTY,MA)
      INTEGER NAC,IACS,IACF,IPT
      REAL ACI,ACV,TA
      
      character h*72,VERT(3*MA)*35,OUTS*124,OUT*4
      INTEGER IGOON,ILET,IDT,IDTY,IAFP,IX,KSTR(MDTY),ilen,ilm,IJ,L,M

      integer IctlSX,IctlFX,ishdONX
      real slat_angleX
      
      dimension IVERT(3*MA),ITEMP(3*MA)

      ILEN=0

C Set up menu of current definitions.
      DO 399 IDTY=1,NBDAYTYPE
        ILEN=ILEN+n_ctl_periods(icomp,icfctype,IDTY)
 399  CONTINUE

C Loop through the items until the page to be displayed.
C Build up text strings for the menu. 
      M=0
      DO 400 IDTY=1,NBDAYTYPE
        DO 10 L=1,n_ctl_periods(icomp,icfctype,IDTY)
          M=M+1
          write(out,'(A)')calentag(idty)(1:4)
          
          write(VERT(M),323)OUT,i_ctl_start(icomp,icfctype,IDTY,L),
     &    i_ctl_end(icomp,icfctype,IDTY,L),
     &    i_shdONorOFF(icomp,icfctype,IDTY,L),
     &    slat_angle(icomp,icfctype,IDTY,L)
     
  323     FORMAT(A,' ',3i4,1x,f6.2)
 10     continue
 400  CONTINUE

C To delete period(s) redisplay the menu and allow user to indicate
C which one, then collapse the period list.
      if (ICD.eq.1) then
        H(1)='One or more of the periods may be deleted.'
        call EPICKS(ILEN,IVERT,' ','CFC shading period(s) to delete:',
     &         35,M,VERT,'CFC shading period to delete',IER,1)

C Delete items in IVERT array. Sort IVERT first.
        if (ILEN.gt.0) then
          KFLAG=-1
          call SORTI(IVERT,ITEMP,NBDAYTYPE*MA,KFLAG)
          do 100 IJ=1,ILEN
            IAFP=IVERT(IJ)
            IGOON=1
            ILET=0
            ILM=0
            DO 401 IDT=1,NBDAYTYPE
              IF(IGOON.EQ.1)THEN
                ILET=ILET+n_ctl_periods(icomp,icfctype,IDT)
                IF(IAFP.GT.ILET)THEN
                  IGOON=1
                  ILM=ILM+n_ctl_periods(icomp,icfctype,IDT)
                ELSE
                  IGOON=0
                ENDIF
                IDTY=IDT
              ENDIF
 401        CONTINUE
            DO 402 IX=IAFP-ILM,n_ctl_periods(icomp,icfctype,IDTY)
              if (IX.ne.MA) then

                i_ctl_start(icomp,icfctype,IDTY,IX)=
     &          i_ctl_start(icomp,icfctype,IDTY,IX+1) 
                i_ctl_end(icomp,icfctype,IDTY,IX)=
     &          i_ctl_end(icomp,icfctype,IDTY,IX+1) 
                i_shdONorOFF(icomp,icfctype,IDTY,IX)=
     &          i_shdONorOFF(icomp,icfctype,IDTY,IX+1)
                slat_angle(icomp,icfctype,IDTY,IX)=
     &          slat_angle(icomp,icfctype,IDTY,IX+1) 
     
              endif
 402        CONTINUE
            n_ctl_periods(icomp,icfctype,IDTY)=
     &      n_ctl_periods(icomp,icfctype,IDTY)-1
            M=M-1
 100      continue
        endif

      else

C Copy periods.
        h(1)='All the CFC shading periods associated with a'
        h(2)='specified day type can be copied to another day type.'
        IX=1
        CALL EDISP(IUOUT,'choose daytype to copy periods from')
        CALL EPICKS(IX,KSTR,' ','choose daytype to copy periods from',
     &  12,nbdaytype,calentag,'Choose daytype',IER,35)
        IDTY=KSTR(1)
        CALL EDISP(IUOUT,'choose daytype to copy periods to')
        CALL EPICKS(IX,KSTR,' ','choose daytype to copy periods to',
     &  12,nbdaytype,calentag,'Choose daytype',IER,35)
        IDT=KSTR(1)
     	do 210 IX=1,n_ctl_periods(icomp,icfctype,IDTY)
     	  if (n_ctl_periods(icomp,icfctype,IDT).lt.MA) then
     	    IPER=0

          IctlSX=i_ctl_start(icomp,icfctype,IDTY,IX)
          IctlFX=i_ctl_end(icomp,icfctype,IDTY,IX)
          ishdONX=i_shdONorOFF(icomp,icfctype,IDTY,IX)
          slat_angleX=slat_angle(icomp,icfctype,IDTY,IX)

          call addCFC_period_data(icomp,icfctype,IPER,IDT,IctlSX,
     &                            IctlFX,ishdONX,slat_angleX)

     	    call chkCFC_period_data(icomp,icfctype)
          
          ELSE
            WRITE(OUTS,'(2a)')'Sorry, too many periods in day type ',
     &      calentag(idt)
            call usrmsg(' ',outs,'W')
     	  endif
 210 	continue
      endif
      return
      end
