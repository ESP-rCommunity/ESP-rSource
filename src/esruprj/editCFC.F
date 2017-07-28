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

C  askCFCtype           Presents list of CFC types in zone and asks user
C                       to select a CFC type for editing shading schedule.

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
#include "esprdbfile.h"
#include "material.h"
#include "CFC_common.h"
      COMMON/OUTIN/IUOUT,IUIN
      integer iuout,iuin
      COMMON/FILEP/IFIL
      integer ifil
      common/pophelp/h(60)
      character h*72
      COMMON/C24/IZSTOCN(MCOM,MS) !array which holds the connection index
      integer izstocn

      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      integer ne,nairg,ipairg
      real rairg

      integer IER,IS,icomp,icn1,ncfccount,igap,ie, imlc
      integer nmaxtypes,ntypes,icfctp,isame,iss,ncfctypes,i,icount
      integer ij,it,icompare1,icompare2,icn2,ibk,iwa,nmax,itrunc,ipos
      integer IFU,ival,itype,istat,loutlen,ln_outs,ln_zonepth
      integer lnblnk    !function definition
      integer cfcarrayindex ! for type CFC2
      CHARACTER outs*124
      CHARACTER con_name*32,ltmp*72,louts*248

      DIMENSION con_name(ms)        !local storage of CFC const. name 
      dimension ival(MS)

      LOGICAL XST, vb_xst, drp_xst, rld_xst, bug_xst

      LOGICAL legacy_CFC_xst, CFC2_xst
      integer iShdLayers
      
      vb_xst = .false.
      drp_xst = .false.
      rld_xst = .false.
      bug_xst = .false.

      legacy_CFC_xst = .false.
      CFC2_xst = .false.
  
      IER=0
      IFU=IFIL+2

C Ask user to update CFC file or continue
      write(outs,'(a,a)') 'CFC model file is ',
     &                      lcfcin(icomp)
      h(1)=' For instructions on importing *.GSL files'
      h(2)=' see "Appendix F: CFC User Reference" from'
      h(3)=' http://uwspace.uwaterloo.ca'
      h(4)=' /bitstream/10012/4164/1/Lomanowski_Bartosz.pdf'
      h(5)=''
      h(6)=' If GSL files have already been imported, select'
      h(7)=' continue.'
      h(8)=' To download the Glazing Shading Layer Editor go to'
      h(9)=' the ESRU website downloads section.'
      CALL EASKAB(outs,'Options:',
     &     'Update CFC file','continue',IWA,9)

C Update CFC file selected
      IF(IWA.EQ.1)THEN

C 1. scan G6 common and check that CFCs are VERT and EXTERIOR
      ncfccount=0
      DO 10 IS=1,NZSUR(icomp)
        icn1=izstocn(icomp,is)
        imlc = ssmlcindex(icn1)
        IF(SSOTF(icn1)(1:3).eq.'CFC')then
            if(SSVFC(icn1)(1:4).eq.'VERT'.and.
     &            SSOTHER(icn1,1)(1:5).eq.'EXTER')then
                  ncfccount=ncfccount+1   !count CFCs in zone
                  con_name(is)=SSMLCN(icn1)

C 2.  scan constructions layers and check that there are air gaps
C between each glazing/shading layer
                  igap=0
                  do 15 ie=2,ne(is),2
                   check_CFC_or_CFC2:
     &             IF(SSOTF(icn1)(1:4).EQ.'CFC ') THEN    
                     igap=igap+1
                     if(ie.ne.IPAIRG(IS,igap))then
                       CALL USRMSG(
     &             ' CFCs must have alternating solid and gas',
     &             ' gap layers. Check constructions and try again.',
     &             'W') 
                       IER=1
                       return
                     endif
                     legacy_CFC_xst = .true.
                   ELSEIF(SSOTF(icn1)(1:4).EQ.'CFC2')THEN
                     cfcarrayindex = ITMCFCDB(imlc,ie)
                     if(CFCshdtp(cfcarrayindex).ne.iGasGap
     &              .or.
     &           (CFCshdtp(cfcarrayindex).eq.iGasGap.and.ie.eq.ne(is))
     &              .or.
     &           (CFCshdtp(cfcarrayindex).eq.iGasGap.and.ie.eq.1)
     &                 )then

                       CALL USRMSG(
     &             ' CFCs must have alternating solid and gas',
     &             ' gap layers. Check constructions and try again.',
     &             'W') 
                       IER=1
                       return
                     endif
                     CFC2_xst = .true.
                   ENDIF check_CFC_or_CFC2
 15               continue

C 3.  scan constructions layers and check that there isn't more
C than 1 shade layer (for CFC2 type only).
                  iShdLayers = 0
                  do 16 ie=1,ne(is),1
                   IF(SSOTF(icn1)(1:4).EQ.'CFC2')THEN
                     cfcarrayindex = ITMCFCDB(imlc,ie)
                     if(CFCshdtp(cfcarrayindex).gt.iGlazing)then
                       iShdLayers = iShdLayers+1
                     endif

                     if(iShdLayers.gt.1)then
                       CALL USRMSG(
     &             ' Currently only one shade layer is allowed',
     &             ' Check constructions and try again.',
     &             'W') 
                       IER=1
                       return
                     endif
                   ENDIF
 16               continue

            else
                  CALL USRMSG(
     &            ' CFC must be VERT and EXTERIOR',
     &            ' please update geometry and try again.','W')
                  IER=1
                  return
            end if
        ELSE
            con_name(is)=' '
        END IF
 10   CONTINUE

C Found both legacy CFC (used with GSLedit import) and new CFC2
C (used with native CFClayers db) types. To avoid complications,
C use one or the other, but not both. Warn user and return.
      IF(legacy_CFC_xst.and.CFC2_xst)THEN
        CALL USRMSG(
     &      ' Found both CFC (legacy) and CFC2 (new) types',
     &      ' please redefine model to use one or the other.','W')
        IER=1
        return
      ENDIF

C count how many default CFC types exist (ie. how many different
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

C ask user to provide alternate number of CFC types
C (cannot exceed # of CFCs in zone)
 11   H(1)=' The minimum number of CFC types required is given as'
      H(2)=' default. If, for instance, two surfaces use the same'
      H(3)=' CFC definition but different slat angle settings'
      H(4)=' then two different CFC types can be defined here.'
      CALL EASKI(ncfctypes,' ','Number of CFC types?',
     &    nmaxtypes,'F',ncfccount,'F',nmaxtypes,
     &    'no of CFC types',IER,4)
      if(IER.EQ.-3)GOTO 99
      im_ncfc(icomp)=ncfctypes

C go through CFC constructions and ask user to assign type for each one,
      ncfccount=0
      DO 40 IS=1,NZSUR(icomp)
        icn1=izstocn(icomp,is)
        itype=0
        IF(SSOTF(icn1)(1:3).eq.'CFC')then

          if(ncfccount.lt.nmaxtypes)ncfccount=ncfccount+1

          H(1)= 'Assign CFC type to surface. '
          H(2)=''
          H(3)=' For an example of usage see'
          H(4)=' "Appendix F: CFC User Reference" from'
          H(5)=' http://uwspace.uwaterloo.ca'
          H(6)=' /bitstream/10012/4164/1/Lomanowski_Bartosz.pdf'

          write(outs,'(4A)')'Surface: ',SSNAME(icn1),
     &      'Constr: ',SSMLCN(icn1)
          CALL EASKI(itype,'CFC type for: ',outs,1,'F',
     &    ncfctypes,'F',ncfccount,'CFC type',IER,1)
          if(IER.EQ.-3)GOTO 99
          im_cfcfl(icomp,is)=itype
        ELSE
          im_cfcfl(icomp,is)=0
        ENDIF
 40   CONTINUE

C check that all CFC types have been assigned 
      DO 50 i=1, ncfctypes
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
C If we are at this point then checks passed, ready to assemble CFC file.
C Ask user to either import CFC data from GSL edit, or use the CFClayers
C database
C ------------------------------------------------------------------

C.....Import GSLedit files if legacy CFCs exist
      import_CFC_data: IF(legacy_CFC_xst.and.(.not.CFC2_xst))then

C GSLfiles is array that holds DEFAULT names for *.GSL files for
C each CFC type
      nmax=0
      do 90 is=1,NZSUR(icomp)

      if(im_cfcfl(icomp,is).gt.0)then
        icfctp=im_cfcfl(icomp,is)
C if this cfc type has already been processed, skip to next surface
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

        H(1)=' Provide the path and file name of the *.GSL file  '
        H(2)=' that corresponds to this CFC type.'
        H(3)=''
        H(4)=' For an example of usage see'
        H(5)=' "Appendix F: CFC User Reference" from'
        H(6)=' http://uwspace.uwaterloo.ca'
        H(7)=' /bitstream/10012/4164/1/Lomanowski_Bartosz.pdf'

        ltmp=GSLfiles(icomp,icfctp)
        write(outs,'(a,i4)')
     &      'GSLedit file name and path for CFC type: ',icfctp
        CALL EASKS(ltmp,outs,' ',72,ltmp,'GSLedit file name',IER,7)
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
          h(1)=' Generate *.GSL file using GSLedit and copy file'
          h(2)=' to desired location within the Model folders.'
          h(3)=' Make sure path is correct.'
          h(4)=''
          h(5)=' For an example of usage see'
          h(6)=' "Appendix F: CFC User Reference" from'
          h(7)=' http://uwspace.uwaterloo.ca'
          h(8)=' /bitstream/10012/4164/1/Lomanowski_Bartosz.pdf'



          CALL EASKAB('File not found! ','Options:',
     &       'try again','cancel',IWA,8)
          if(IWA.eq.1)goto 12
          if(IWA.eq.2)GOTO 99
        endif
      endif

      if(icfctp.gt.nmax)nmax=icfctp

 90   continue

C If CFC2 types found then use native CFClayers db
      ELSEIF((.not.legacy_CFC_xst).and.CFC2_xst)then
C.......Get CFC layer data from CFClayers db commons

C.......Loop through all surfaces and pick out CFC types
C.......For each CFC type, get construction details and 
C.......import CFC layer properties from the CFClayers db. 
        
        do icfctp = 1, im_ncfc(icomp)
          is = 1
          do while (icfctp .ne. im_cfcfl(icomp,is) 
     &              .and. is .le. NZSUR(icomp))
            is = is + 1
          end do
C.........Now get construction layer details for surface 'is'
          icn1 = izstocn(icomp,is)   
          imlc = ssmlcindex(icn1)
          im_ncfc_el(icomp,icfctp) = ne(is)
          DO IL = 1, ne(is)
            cfcarrayindex = ITMCFCDB(imlc,IL)
C...........The CFC is composed of 's' shade type layers,
C...........including IGDB glazing layers, and 'f' fill gas layers.
C...........At this point we've already checked for presence of 
C...........alternating air gaps. 

C...........Get shade and glazing layer properties
            if (cfcarrayindex.gt.0) then
              im_cfcltp(icomp,icfctp,IL) = CFCshdtp(cfcarrayindex)

              if (im_cfcltp(icomp,icfctp,IL).eq.iGlazing) then

                if (CFC_layer_flipped(imlc,IL)) then
C                 Switch front / back properties
                  rim_SolRf(icomp,icfctp,IL)=
     &                    CFCsolreflin(cfcarrayindex)
                  rim_SolRb(icomp,icfctp,IL)=
     &                    CFCsolreflout(cfcarrayindex)
                  rim_lwEf(icomp,icfctp,IL)=
     &                  CFCemissin(cfcarrayindex)
                  rim_lwEb(icomp,icfctp,IL)=
     &                  CFCemissout(cfcarrayindex)

                  rim_VisRf(icomp,icfctp,IL)=
     &                  CFCvisreflin(cfcarrayindex)
                  rim_VisRb(icomp,icfctp,IL)=
     &                  CFCvisreflout(cfcarrayindex)

                else
C                 Copy values as is from IGDB db
                  rim_SolRf(icomp,icfctp,IL)=
     &                    CFCsolreflout(cfcarrayindex)
                  rim_SolRb(icomp,icfctp,IL)=
     &                    CFCsolreflin(cfcarrayindex)
                  rim_lwEf(icomp,icfctp,IL)=
     &                  CFCemissout(cfcarrayindex)
                  rim_lwEb(icomp,icfctp,IL)=
     &                  CFCemissin(cfcarrayindex)

                  rim_VisRf(icomp,icfctp,IL)=
     &                  CFCvisreflout(cfcarrayindex)
                  rim_VisRb(icomp,icfctp,IL)=
     &                  CFCvisreflin(cfcarrayindex)

                endif
C               Idependant of flip:
                rim_SolT(icomp,icfctp,IL)=
     &                CFCsoltrandir(cfcarrayindex)
                rim_SolTf_tot(icomp,icfctp,IL)=
     &                CFCsoltrandir(cfcarrayindex)
                rim_SolTb_tot(icomp,icfctp,IL)=
     &                CFCsoltrandir(cfcarrayindex)
                rim_SolTf_bd(icomp,icfctp,IL)=0.0
                rim_SolTb_bd(icomp,icfctp,IL)=0.0
                rim_lwT(icomp,icfctp,IL)=
     &                CFClwtran(cfcarrayindex)
                cim_IGDB_ID(icomp,icfctp,IL)=
     &                CFC_IGDB_ID(cfcarrayindex)

                rim_VisT(icomp,icfctp,IL)=
     &                CFCvistrandir(cfcarrayindex)


              elseif (im_cfcltp(icomp,icfctp,IL).eq.iVenBlind) then

                rim_SolRf(icomp,icfctp,IL)=
     &                  CFCsolreflout(cfcarrayindex)
                rim_SolRb(icomp,icfctp,IL)=
     &                  CFCsolreflin(cfcarrayindex)
                rim_SolT(icomp,icfctp,IL)=
     &                  CFCslattran(cfcarrayindex)
                rim_lwEf(icomp,icfctp,IL)=
     &                  CFCemissout(cfcarrayindex)
                rim_lwEb(icomp,icfctp,IL)=
     &                  CFCemissin(cfcarrayindex)
                rim_lwT(icomp,icfctp,IL)=
     &                  CFClwtran(cfcarrayindex)
                rim_vb_w(icomp,icfctp)=
     &                  CFCslatwidth(cfcarrayindex)
                rim_vb_s(icomp,icfctp)=
     &                  CFCslatspacing(cfcarrayindex)
                rim_vb_phi(icomp,icfctp)=
     &                  CFCslatangle(cfcarrayindex)
                cim_VorH(icomp,icfctp)=
     &                  CFCslatorient(cfcarrayindex)
                rim_vb_crown(icomp,icfctp)=
     &                  CFCslatcrown(cfcarrayindex)
                rim_vb_wr(icomp,icfctp)=
     &                  CFCslatwr(cfcarrayindex)
                rim_vb_t(icomp,icfctp)=
     &                  CFCslatthk(cfcarrayindex)

                rim_VisRf(icomp,icfctp,IL)=
     &                  CFCvisreflout(cfcarrayindex)
                rim_VisRb(icomp,icfctp,IL)=
     &                  CFCvisreflin(cfcarrayindex)

              elseif(im_cfcltp(icomp,icfctp,IL).eq.iPleatedDrape)then

                rim_SolRf(icomp,icfctp,IL)=
     &                  CFCsolreflout(cfcarrayindex)
                rim_SolRb(icomp,icfctp,IL)=
     &                  CFCsolreflin(cfcarrayindex)
                rim_SolT(icomp,icfctp,IL)=
     &                  CFCsoltrandir(cfcarrayindex)
                rim_SolTf_tot(icomp,icfctp,IL)= 
     &                  CFCsoltrantotout(cfcarrayindex)
                rim_SolTb_tot(icomp,icfctp,IL)=       
     &                  CFCsoltrantotin(cfcarrayindex)
                rim_SolTf_bd(icomp,icfctp,IL)= 
     &                   CFCsoltrantotout(cfcarrayindex) - 
     &                   CFCsoltrandir(cfcarrayindex)
                rim_SolTb_bd(icomp,icfctp,IL)=     
     &                   CFCsoltrantotin(cfcarrayindex) - 
     &                   CFCsoltrandir(cfcarrayindex)

                rim_VisRf(icomp,icfctp,IL)=
     &                  CFCvisreflout(cfcarrayindex)
                rim_VisRb(icomp,icfctp,IL)=
     &                  CFCvisreflin(cfcarrayindex)
                rim_VisT(icomp,icfctp,IL)=
     &                  CFCvistrandir(cfcarrayindex)

C...............Longwave layer inputs are typical values for 0 openness.
C...............Effective lw properties are then calculated in cfc_thermal_processing()
C...............taking into account the openness (beam-beam tran at 0 incidence angle).

C...............Typical apparent LW emittance of drape fabric at 0 openness.
                rim_lwT(icomp,icfctp,IL)=.05
C...............Typical apparent LW transmittance of drape fabric at 0 openness. 
                rim_lwEf(icomp,icfctp,IL)= .87
                rim_lwEb(icomp,icfctp,IL)= .87
                
                rim_drp_w(icomp,icfctp)=   
     &                  CFCdrpwidth(cfcarrayindex)
                rim_drp_s(icomp,icfctp)=
     &                  CFCdrpspacing(cfcarrayindex)

              elseif(im_cfcltp(icomp,icfctp,IL).eq.iRollerBlind)then

                rim_SolRf(icomp,icfctp,IL)=
     &                  CFCsolreflout(cfcarrayindex)
                rim_SolRb(icomp,icfctp,IL)=
     &                  CFCsolreflin(cfcarrayindex)
                rim_SolT(icomp,icfctp,IL)=
     &                  CFCsoltrandir(cfcarrayindex)
                rim_SolTf_tot(icomp,icfctp,IL)= 
     &                  CFCsoltrantotout(cfcarrayindex)
                rim_SolTb_tot(icomp,icfctp,IL)=       
     &                  CFCsoltrantotin(cfcarrayindex)
                rim_SolTf_bd(icomp,icfctp,IL)= 
     &                   CFCsoltrantotout(cfcarrayindex) - 
     &                   CFCsoltrandir(cfcarrayindex)
                rim_SolTb_bd(icomp,icfctp,IL)=     
     &                   CFCsoltrantotin(cfcarrayindex) - 
     &                   CFCsoltrandir(cfcarrayindex)

                rim_VisRf(icomp,icfctp,IL)=
     &                  CFCvisreflout(cfcarrayindex)
                rim_VisRb(icomp,icfctp,IL)=
     &                  CFCvisreflin(cfcarrayindex)
                rim_VisT(icomp,icfctp,IL)=
     &                  CFCvistrandir(cfcarrayindex)

C...............Longwave layer inputs are typical values for 0 openness.
C...............Effective lw properties are then calculated in cfc_thermal_processing()
C...............taking into account the openness (beam-beam tran at 0 incidence angle).

C...............Typical apparent LW emittance of roller blind at 0 openness.
                rim_lwT(icomp,icfctp,IL)=.05
C...............Typical apparent LW transmittance of roller blind at 0 openness. 
                rim_lwEf(icomp,icfctp,IL)= .91
                rim_lwEb(icomp,icfctp,IL)= .91

              elseif(im_cfcltp(icomp,icfctp,IL).eq.iInsectScreen)then

                rim_SolRf(icomp,icfctp,IL)=
     &                  CFCsolreflout(cfcarrayindex)
                rim_SolRb(icomp,icfctp,IL)=
     &                  CFCsolreflin(cfcarrayindex)
                rim_SolT(icomp,icfctp,IL)=
     &                  CFCsoltrandir(cfcarrayindex)
                rim_SolTf_tot(icomp,icfctp,IL)= 
     &                  CFCsoltrantotout(cfcarrayindex)
                rim_SolTb_tot(icomp,icfctp,IL)=       
     &                  CFCsoltrantotin(cfcarrayindex)
                rim_SolTf_bd(icomp,icfctp,IL)= 
     &                   CFCsoltrantotout(cfcarrayindex) - 
     &                   CFCsoltrandir(cfcarrayindex)
                rim_SolTb_bd(icomp,icfctp,IL)=     
     &                   CFCsoltrantotin(cfcarrayindex) - 
     &                   CFCsoltrandir(cfcarrayindex)
                rim_bug_Emis(icomp,icfctp)=
     &                  CFCwireemiss(cfcarrayindex)

                rim_VisRf(icomp,icfctp,IL)=
     &                  CFCvisreflout(cfcarrayindex)
                rim_VisRb(icomp,icfctp,IL)=
     &                  CFCvisreflin(cfcarrayindex)
                rim_VisT(icomp,icfctp,IL)=
     &                  CFCvistrandir(cfcarrayindex)

C...............Longwave layer inputs are typical values for 0 openness.
C...............Effective lw properties are then calculated in cfc_thermal_processing()
C...............taking into account the openness (beam-beam tran at 0 incidence angle).

C...............Typical apparent LW emittance of insect screen at 0 openness. 
                rim_lwEf(icomp,icfctp,IL) = rim_bug_Emis(icomp,icfctp)
                rim_lwEb(icomp,icfctp,IL) = rim_bug_Emis(icomp,icfctp)
              
C...............Typical apparent LW transmittance of insect screen at 0 openness.
                if(rim_bug_Emis(icomp,icfctp).gt.0.5)then
                  ! assume non-metallic insect screen
                  rim_lwT(icomp,icfctp,IL) = .02               
                else
                  ! assume metallic insect screen
                  rim_lwT(icomp,icfctp,IL) = .19               
                endif
                
                rim_bug_d(icomp,icfctp)=
     &                  CFCwirediam(cfcarrayindex)
                rim_bug_s(icomp,icfctp)=
     &                  CFCwirespace(cfcarrayindex)

C.............Get fill gas gap properties
              elseif(im_cfcltp(icomp,icfctp,IL).eq.iGasGap)then

                im_cfcltp(icomp,icfctp,IL) = iGasGap
                imlr_mass_frac(icomp,icfctp,IL,1) = 
     &                  CFCfillAir(cfcarrayindex)
                imlr_mass_frac(icomp,icfctp,IL,2) = 
     &                  CFCfillAr(cfcarrayindex)
                imlr_mass_frac(icomp,icfctp,IL,3) = 
     &                  CFCfillKr(cfcarrayindex)
                imlr_mass_frac(icomp,icfctp,IL,4) = 
     &                  CFCfillXe(cfcarrayindex)
                imlr_mass_frac(icomp,icfctp,IL,5) = 
     &                  CFCfillSF6(cfcarrayindex)

                call calc_CFC_fill_gas_mix_coeff(icomp, icfctp, IL)
              endif
            else
              ! error, cfcarrayindex not valid
              CALL USRMSG(
     &      ' CFClayers db index not found',
     &      ' please check CFC details and try again.','W')
              IER=1
              return
            endif

          END DO
        end do 

      ENDIF import_CFC_data

C -------------------------------------------------------------------------
C if at this point, the import commons have been filled successfully and
C we can now proceed to write the *.cfc file.
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
      do 120 it=1, im_ncfc(icomp)
        write(outs,'(i4,a,i2)')im_ncfc_el(icomp,it),
     &      '   # layers in cfc type: ',it
        WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))
        write(outs,'(a,a)')
     &      '# For each layer: normal solar optical properties - ',
     &      'R_fr, R_bk, Tran., (T_f_tot, T_b_tot, T_f_bd, T_b_bd)'
        WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))

        do 130 ie=1,im_ncfc_el(icomp,it)

            if(im_cfcltp(icomp,it,ie).eq.iInsectScreen)then
            
            write(outs,'(7F8.3,a)')rim_SolRf(icomp,it,ie),
     &      rim_SolRb(icomp,it,ie),rim_SolT(icomp,it,ie),
     &      rim_SolTf_tot(icomp,it,ie), rim_SolTb_tot(icomp,it,ie),
     &      rim_SolTf_bd(icomp,it,ie), rim_SolTb_bd(icomp,it,ie),
     &      '   # insect screen '
            WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &      outs(1:lnblnk(outs))
 
            elseif(im_cfcltp(icomp,it,ie).eq.iRollerBlind)then
            
            write(outs,'(7F8.3,a)')rim_SolRf(icomp,it,ie),
     &      rim_SolRb(icomp,it,ie),rim_SolT(icomp,it,ie),
     &      rim_SolTf_tot(icomp,it,ie), rim_SolTb_tot(icomp,it,ie),
     &      rim_SolTf_bd(icomp,it,ie), rim_SolTb_bd(icomp,it,ie),
     &      '   # roller blind '
            WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &      outs(1:lnblnk(outs))

            elseif(im_cfcltp(icomp,it,ie).eq.iPleatedDrape)then
            
            write(outs,'(7F8.3,a)')rim_SolRf(icomp,it,ie),
     &      rim_SolRb(icomp,it,ie),rim_SolT(icomp,it,ie),
     &      rim_SolTf_tot(icomp,it,ie), rim_SolTb_tot(icomp,it,ie),
     &      rim_SolTf_bd(icomp,it,ie), rim_SolTb_bd(icomp,it,ie),
     &      '   # pleated drape '
            WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &      outs(1:lnblnk(outs))
     
        elseif(im_cfcltp(icomp,it,ie).eq.iVenBlind)then
            
            write(outs,'(7F8.3,a)')rim_SolRf(icomp,it,ie),
     &      rim_SolRb(icomp,it,ie),rim_SolT(icomp,it,ie),0.,0.,0.,0.,
     &      '   # slat-type blind '
            WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &      outs(1:lnblnk(outs))

        elseif(im_cfcltp(icomp,it,ie).eq.iGlazing)then

            if(CFC2_xst .and. cim_IGDB_ID(icomp,it,ie) .ne. '')then
              write(outs,'(7F8.3,a,a)')rim_SolRf(icomp,it,ie),
     &        rim_SolRb(icomp,it,ie),rim_SolT(icomp,it,ie),0.,0.,0.,0.,
     &        '   # glazing, IGDB ID: ', cim_IGDB_ID(icomp,it,ie)
              WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &        outs(1:lnblnk(outs))
            else
              write(outs,'(7F8.3,a)')rim_SolRf(icomp,it,ie),
     &        rim_SolRb(icomp,it,ie),rim_SolT(icomp,it,ie),0.,0.,0.,0.,
     &        '   # glazing '
              WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &        outs(1:lnblnk(outs))
            endif

        elseif(im_cfcltp(icomp,it,ie).eq.iGasGap)then        
        
            write(outs,'(7F8.3,a)')rim_SolRf(icomp,it,ie),
     &      rim_SolRb(icomp,it,ie),rim_SolT(icomp,it,ie),0.,0.,0.,0.,
     &      '   # gas gap '
            WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &      outs(1:lnblnk(outs))

        else
        
          GOTO 98
        
        endif

 130    continue

        write(outs,'(a,a)')
     &      '# For each layer: normal visible optical properties - ',
     &      'R_fr, R_bk, Tran. EXPERIMENTAL'
        WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))

        do 140 ie=1,im_ncfc_el(icomp,it)

          if(im_cfcltp(icomp,it,ie).eq.iInsectScreen)then
            write(outs,'(3F8.3,a)')rim_VisRf(icomp,it,ie),
     &        rim_VisRb(icomp,it,ie),rim_VisT(icomp,it,ie),
     &        '   # insect screen '
              WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &                        outs(1:lnblnk(outs))
 
          elseif(im_cfcltp(icomp,it,ie).eq.iRollerBlind)then
             write(outs,'(3F8.3,a)')rim_VisRf(icomp,it,ie),
     &         rim_VisRb(icomp,it,ie),rim_VisT(icomp,it,ie),
     &         '   # roller blind '
             WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &                       outs(1:lnblnk(outs))

          elseif(im_cfcltp(icomp,it,ie).eq.iPleatedDrape)then
             write(outs,'(3F8.3,a)')rim_VisRf(icomp,it,ie),
     &         rim_VisRb(icomp,it,ie),rim_VisT(icomp,it,ie),
     &         '   # pleated drape '
             WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &                       outs(1:lnblnk(outs))
     
          elseif(im_cfcltp(icomp,it,ie).eq.iVenBlind)then
             write(outs,'(3F8.3,a)')rim_VisRf(icomp,it,ie),
     &         rim_VisRb(icomp,it,ie),rim_VisT(icomp,it,ie),
     &         '   # slat-type blind '
             WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98) outs(1:lnblnk(outs))

          elseif(im_cfcltp(icomp,it,ie).eq.iGlazing)then
             write(outs,'(3F8.3,a)')rim_VisRf(icomp,it,ie),
     &         rim_VisRb(icomp,it,ie),rim_VisT(icomp,it,ie),
     &         '   # glazing '
             WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98) outs(1:lnblnk(outs))

          elseif(im_cfcltp(icomp,it,ie).eq.iGasGap)then
             write(outs,'(3F8.3,a)')rim_VisRf(icomp,it,ie),
     &         rim_VisRb(icomp,it,ie),rim_VisT(icomp,it,ie),
     &         '   # gas gap '
             WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98) outs(1:lnblnk(outs))

          else
             GOTO 98
          endif

 140    continue

        write(outs,'(a,a)')
     &      '# For each layer: normal longwave radiative properties - ',
     &      'EmisF, EmisB, Tran.'
        WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))

        do 150 ie=1,im_ncfc_el(icomp,it)

              if(im_cfcltp(icomp,it,ie).eq.iInsectScreen)then

                write(outs,'(3F8.3,a)')rim_lwEf(icomp,it,ie),
     &            rim_lwEb(icomp,it,ie),rim_lwT(icomp,it,ie),
     &            '   # insect screen '
                WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &                        outs(1:lnblnk(outs))
 
              elseif(im_cfcltp(icomp,it,ie).eq.iRollerBlind)then
                write(outs,'(3F8.3,a)')rim_lwEf(icomp,it,ie),
     &            rim_lwEb(icomp,it,ie),rim_lwT(icomp,it,ie),
     &            '   # roller drapes '
                WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &                        outs(1:lnblnk(outs))

              elseif(im_cfcltp(icomp,it,ie).eq.iPleatedDrape)then
                write(outs,'(3F8.3,a)')rim_lwEf(icomp,it,ie),
     &            rim_lwEb(icomp,it,ie),rim_lwT(icomp,it,ie),
     &            '   # drapes '
                WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &                        outs(1:lnblnk(outs))
     
              elseif(im_cfcltp(icomp,it,ie).eq.iVenBlind)then
                write(outs,'(3F8.3,a)')rim_lwEf(icomp,it,ie),
     &            rim_lwEb(icomp,it,ie),rim_lwT(icomp,it,ie),
     &            '   # slat-type blind '
                WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &                        outs(1:lnblnk(outs))

              elseif(im_cfcltp(icomp,it,ie).eq.iGlazing)then
                write(outs,'(3F8.3,a)')rim_lwEf(icomp,it,ie),
     &            rim_lwEb(icomp,it,ie),rim_lwT(icomp,it,ie),
     &            '   # glazing '
                WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)
     &                        outs(1:lnblnk(outs))

              elseif(im_cfcltp(icomp,it,ie).eq.iGasGap)then
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
  
            vb_xst=.false.  ! slat-type blind
            drp_xst=.false. ! pleated drape
            rld_xst=.false. ! roller blind 
            bug_xst=.false. ! insect screen

C determine which shading device is being used, set shading_device_xst = .true.
        do 250 ij=1,im_ncfc_el(icomp,it)
     
           ival(ij)=im_cfcltp(icomp,it,ij)
          
           if(ival(ij).eq.iVenBlind) then 
            vb_xst=.true.    
     
           elseif(ival(ij).eq.iPleatedDrape) then 
            drp_xst=.true.
     
           elseif(ival(ij).eq.iRollerBlind) then 
            rld_xst=.true.
 
           elseif(ival(ij).eq.iInsectScreen) then
            bug_xst=.true.
 
           endif
     
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
  241 if(vb_xst)then
  
        do 270 ij=1,im_ncfc_el(icomp,it)
        
           if(im_cfcltp(icomp,it,ij).eq.2) then
           
             write(outs,'(a,i2)')
     &        '# slat-type blind attributes for cfc type: ',it
     
             WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))

             write(outs,'(a,a,a)')
     &        '# slat: width(mm); spacing(mm); angle(deg);',
     &        ' orientation(HORZ/VERT); crown (mm);',
     &        ' w/r ratio; slat thickness (mm)'
             WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))

             write(outs,'(3F8.3,a,a,a,3F8.3)')rim_vb_w(icomp,it),
     &         rim_vb_s(icomp,it),rim_vb_phi(icomp,it),
     &         '  ',cim_VorH(icomp,it),' ',rim_vb_crown(icomp,it),
     &         rim_vb_wr(icomp,it),rim_vb_t(icomp,it)
     
             WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))
               
           endif
           
  270   continue
  
      endif

C Pleated drape properties
      if(drp_xst)then
  
        do 280 ij=1,im_ncfc_el(icomp,it)
           if(im_cfcltp(icomp,it,ij).eq.3) then
           
               write(outs,'(a,i2)')
     &           '# pleated drape attributes for cfc type: ',it
     
               WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))

               write(outs,'(a,a,a)')
     &           '# pleated drape: width(mm); spacing(mm);'
               WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))

               write(outs,'(3F8.3,3F8.3)')rim_drp_w(icomp,it),
     &           rim_drp_s(icomp,it)
     
               WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))
               
           endif
 
  280   continue
  
      endif

C Insect screen properties
      if(bug_xst)then
  
        do 290 ij=1,im_ncfc_el(icomp,it)
        
          if(im_cfcltp(icomp,it,ij).eq.5) then
            
            write(outs,'(a,i2)')
     &        '# insect screen attributes for cfc type: ',it
     
            WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))

            write(outs,'(a,a,a)')
     &        '# insect screen: width(mm); spacing(mm); Wire Emissivity'
     
            WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))

            write(outs,'(3F8.3,3F8.3,3F8.3)')rim_bug_d(icomp,it),
     &         rim_bug_s(icomp,it), rim_bug_Emis(icomp,it)
     
            WRITE(IFU,'(A)',IOSTAT=ISTAT,ERR=98)outs(1:lnblnk(outs))
              
          endif
 
  290   continue
  
      endif

C TODO: check is additional attributes are needed for roller blinds

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
      common/pophelp/h(60)
      character h*72
      COMMON/T1/NE(MS),NAIRG(MS),IPAIRG(MS,MGP),RAIRG(MS,MGP)
      integer ne,nairg,ipairg
      real rairg

      integer IUF,icomp,icfctp,isur,IER,nelts_nogas,K,neimp
      integer ivb_count,idrp_count
      integer j,nelts,i,ie,ND,IWA,litems,lgasgaps,ltypes,istat
      real rphi,rprop,rvb_w,rdrp_w,rdrp_s

      integer ln_outs
      integer lnblnk    !function definition
      
      CHARACTER OUTSTR*124,OUTS*124,GSLfile*72,ltype*3
      CHARACTER LOUTSTR*248

      logical vb_xst,drp_xst,rld_xst,bug_xst

      vb_xst = .false. ! slat-type blind
      drp_xst = .false. ! pleated drape
      rld_xst = .false. ! roller blind
      bug_xst = .false. ! insect screen
      
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
C     DRP= drape blind, corresponds to '3' in array icfcltype
C     RLD= roller blind, corresponds to '4' in array icfcltype
C     BUG= insect screen, corresponds to '5' in array icfcltype
C gas gaps are not represented as layers in GSLedit so between
C each layer assign a '0' in array icfltype for a gas gap.
      CALL STRIPC(IUF,OUTSTR,nelts_nogas,ND,1,
     &            'layer types in GSLedit file',IER)
      IF(IER.NE.0)GOTO 1001
      K=0

      nelts = ne(isur)
      im_ncfc_el(icomp,icfctp) = nelts
      ivb_count = 0
      idrp_count = 0
      irld_count = 0
      ibug_count = 0
  
        do 10 j=1,nelts,2
      
            CALL EGETW(OUTSTR,K,ltype,'F','layer type',IER)
        
            if(ltype.ne.'GLZ'.and.ltype.ne.'VBD'
     &          .and.ltype.ne.'DRP'.and.
     &          ltype.ne.'RLD'.and.ltype.ne.'BUG')then
            
              call edisp(iuout,' GSLedit import error: ')
              call edisp(iuout,
     &          ' allowed layer types: GLZ, VBD, DRP, RLD, BUG')

              IER=1
              goto 1111
        
            elseif(ltype.eq.'VBD') then
        
                  im_cfcltp(icomp,icfctp,j) = iVenBlind
                  ivb_count = ivb_count+1
                  vb_xst = .true.
    
            elseif(ltype.eq.'DRP') then
    
                  im_cfcltp(icomp,icfctp,j) = iPleatedDrape
                  idrp_count = idrp_count + 1
                  drp_xst = .true.
        
            elseif(ltype.eq.'RLD') then

                  im_cfcltp(icomp,icfctp,j) = iRollerBlind
                  irld_count = irld_count + 1
                  rld_xst = .true.

            elseif(ltype.eq.'BUG') then

                  im_cfcltp(icomp,icfctp,j) = iInsectScreen
                  ibug_count = irld_count + 1
                  bug_xst = .true.

            elseif(ltype.eq.'GLZ') then
        
                  im_cfcltp(icomp,icfctp,j)=iGlazing
        
            end if
 
        if(j.lt.nelts) im_cfcltp(icomp,icfctp,j+1) = iGasGap

   10 continue
   
C      if((ivb_count+idrp_count).gt.1)then
C            call edisp(iuout,' GSLedit import error: ')
C            call edisp(iuout,' Maximum of 1 layer of blinds is allowed ')
C            IER=1
C            goto 1111
C      endif

C Ask user to specify slat orientation (HORZ, VERT) and initial slat
C angle for venetian blinds, or calculate fullness ratio for draperies. 
      if(vb_xst)then
          h(1)=' Vertical or Horizontal Slats: '
          h(2)=' '
          h(3)=' For slat angle sign convention, see Figure F.12 in:'
          h(4)=' "Appendix F: CFC User Reference" from'
          h(5)=' http://uwspace.uwaterloo.ca'
          h(6)=' /bitstream/10012/4164/1/Lomanowski_Bartosz.pdf'
          
          write(outs,'(a,i2)')
     &      'Specify blind slat orientation for cfc type: ',icfctp
          ln_outs=lnblnk(outs)
          CALL EASKAB(outs(1:ln_outs),' ',
     &            'Vertical','Horizontal',IWA,6)
          if(IWA.eq.1)cim_VorH(icomp,icfctp)='VERT'
          if(IWA.eq.2)cim_VorH(icomp,icfctp)='HORZ'

          h(1)=' Initial Slat angle:'
          h(2)=' This is a constant if there is not slat control'
          h(3)=''
          h(4)=' For slat angle sign convention, see Figure F.12 in:'
          h(5)=' "Appendix F: CFC User Reference" from'
          h(6)=' http://uwspace.uwaterloo.ca'
          h(7)=' /bitstream/10012/4164/1/Lomanowski_Bartosz.pdf'
          
          write(outs,'(a,i2)')
     &      'Specify intial blind slat angle for cfc type: ',icfctp
          rphi=0.0
          ln_outs=lnblnk(outs)
          CALL EASKR(rphi,' ',outs(1:ln_outs),-89.999,'F',
     &               89.999,'F',0.0,'slat angle',IER,7)
          rim_vb_phi(icomp,icfctp)=rphi
      endif

      if(drp_xst) then !ask for drape descriptors from user
    
        rdrp_w=0.0
        rdrp_s=0.0
    
          CALL EASKR(rdrp_w,'Specify drape width (mm): ',' ',0.0000,'F',
     & 500.00,'F',0.0,'drape width',IER,7)
          rim_drp_w(icomp,icfctp)=rdrp_w   
        
          CALL EASKR(rdrp_s, 'Specify pleat spacing (mm): ',' ',0.0000,
     &      'F',500.00,'F',0.0,'drape spacing',IER,7)     
          rim_drp_s(icomp,icfctp)=rdrp_s
    
!          rim_drp_Fr(icomp,icfctp) = 1 + rim_drp_w(icomp,icfctp)/
!     &    rim_drp_s(icomp,icfctp)
      endif

C Read Solar optical, visual and longwave optical properties for
C each layer. If venetian/slat-type blind, also read w/r ratio (Width/radius
C curvature ratio), slat thickness and slat apparent thickness. 
      nelts=ne(isur)
  
      do 20 i=1,nelts,2
      
        CALL LSTRIPC(IUF,LOUTSTR,99,litems,1,
     &        'optical properties in GSLedit file',IER)
 
         IF(IER.NE.0)GOTO 1001
     
         K=23

         if(im_cfcltp(icomp,icfctp,i).eq.iGlazing)then
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'solar transmission',IER)
            rim_SolT(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'solar reflectance - front',IER)
            rim_SolRf(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'solar reflectance - back',IER)
            rim_SolRb(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'visual transmission',IER)
            rim_VisT(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'visual reflectance - front ',IER)
            rim_VisRf(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'visual reflectance - back',IER)
            rim_VisRb(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'long wave transmission',IER)
            rim_lwT(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'long wave emissivity - front',IER)
            rim_lwEf(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'long wave emissivity - back',IER)
            rim_lwEb(icomp,icfctp,i)=rprop

         elseif(im_cfcltp(icomp,icfctp,i).eq.iVenBlind)then
 
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'solar slat transmission',IER)
            rim_SolT(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'solar slat reflectance - front(top)',IER)
            rim_SolRf(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'solar slat reflectance - back(top)',IER)
            rim_SolRb(icomp,icfctp,i)=rprop

C Assign default visual properties until visual properies added 
C to GSLedit
            rim_VisT(icomp,icfctp,i) =     0.600
            rim_VisRf(icomp,icfctp,i)=     0.070
            rim_VisRb(icomp,icfctp,i)=     0.070

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
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'long wave transmission',IER)
            rim_lwT(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'long wave emissivity - front',IER)
            rim_lwEf(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'long wave emissivity - back',IER)
            rim_lwEb(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,0.3,'F',
     &            'slat thickness (m)',IER)
            rim_vb_t(icomp,icfctp)=rprop*1000.  ![convert to mm]

         elseif(im_cfcltp(icomp,icfctp,i).eq.iPleatedDrape)then
         
C read pleated drape optical properties from .GSL file
         
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'solar drape transmission (BB)',IER)
            rim_SolT(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'solar drape transmission (BT) - front',IER)
            rim_SolTf_tot(icomp,icfctp,i)=rprop
            rim_SolTf_bd(icomp,icfctp,i)=rim_SolTf_tot(icomp,icfctp,i)
     &        -rim_SolT(icomp,icfctp,i)
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'solar drape transmission (BT) - back',IER)
            rim_SolTb_tot(icomp,icfctp,i)=rprop
            rim_SolTb_bd(icomp,icfctp,i)=rim_SolTb_tot(icomp,icfctp,i)
     &        -rim_SolT(icomp,icfctp,i)     
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'solar drape reflectance (BD) - front',IER)
            rim_SolRf(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'solar drape reflectance (BD) - back',IER)
            rim_SolRb(icomp,icfctp,i)=rprop

C currently no visible properties for draperies, assume similar to
C total solar properties

            rim_VisT(icomp,icfctp,i) = rim_SolT(icomp,icfctp,i)
            rim_VisRf(icomp,icfctp,i)= rim_SolRf(icomp,icfctp,i)
            rim_VisRb(icomp,icfctp,i)= rim_SolRb(icomp,icfctp,i)
 
C...........Longwave layer inputs are typical values for 0 openness.
C...........Effective lw properties are then calculated in cfc_thermal_processing()
C...........taking into account the openness (beam-beam tran at 0 incidence angle).

C...........Typical apparent LW emittance of drape fabric at 0 openness.
            rim_lwT(icomp,icfctp,i)=.05
C...........Typical apparent LW transmittance of drape fabric at 0 openness. 
            rim_lwEf(icomp,icfctp,i)= .87
            rim_lwEb(icomp,icfctp,i)= .87
         
         elseif(im_cfcltp(icomp,icfctp,i).eq.iRollerBlind)then

C read roller drape optical properties from .GSL file
         
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'solar drape transmission (BB)',IER)
            rim_SolT(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'solar drape transmission (BT) - front',IER)
            rim_SolTf_tot(icomp,icfctp,i)=rprop
            rim_SolTf_bd(icomp,icfctp,i)=rim_SolTf_tot(icomp,icfctp,i)
     &        -rim_SolT(icomp,icfctp,i)
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'solar drape transmission (BT) - back',IER)
            rim_SolTb_tot(icomp,icfctp,i)=rprop
            rim_SolTb_bd(icomp,icfctp,i)=rim_SolTb_tot(icomp,icfctp,i)
     &        -rim_SolT(icomp,icfctp,i)     
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'solar drape reflectance (BD) - front',IER)
            rim_SolRf(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'solar drape reflectance (BD) - back',IER)
            rim_SolRb(icomp,icfctp,i)=rprop

C currently no visible properties for roller blinds, assume similar to
C total solar properties

            rim_VisT(icomp,icfctp,i) = rim_SolT(icomp,icfctp,i)
            rim_VisRf(icomp,icfctp,i)= rim_SolRf(icomp,icfctp,i)
            rim_VisRb(icomp,icfctp,i)= rim_SolRb(icomp,icfctp,i)
C 
C...........Longwave layer inputs are typical values for 0 openness.
C...........Effective lw properties are then calculated in cfc_thermal_processing()
C...........taking into account the openness (beam-beam tran at 0 incidence angle).

C...........Typical apparent LW emittance of roller blind at 0 openness.
            rim_lwT(icomp,icfctp,i)=.05
C...........Typical apparent LW transmittance of roller blind at 0 openness. 
            rim_lwEf(icomp,icfctp,i)= .91
            rim_lwEb(icomp,icfctp,i)= .91

         elseif(im_cfcltp(icomp,icfctp,i).eq.iInsectScreen)then
 
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'solar drape transmission (BB)',IER)
            rim_SolT(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'solar drape transmission (BT) - front',IER)
            rim_SolTf_tot(icomp,icfctp,i)=rprop
            rim_SolTf_bd(icomp,icfctp,i)=rim_SolTf_tot(icomp,icfctp,i)
     &        -rim_SolT(icomp,icfctp,i)
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'solar drape transmission (BT) - back',IER)
            rim_SolTb_tot(icomp,icfctp,i)=rprop
            rim_SolTb_bd(icomp,icfctp,i)=rim_SolTb_tot(icomp,icfctp,i)
     &        -rim_SolT(icomp,icfctp,i)     
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'solar drape reflectance (BD) - front',IER)
            rim_SolRf(icomp,icfctp,i)=rprop
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'solar drape reflectance (BD) - back',IER)
            rim_SolRb(icomp,icfctp,i)=rprop
            
            CALL EGETWR(LOUTSTR,K,rprop,.001,0.30,'F',
     &            'wire diameter (m)',IER)
            rim_bug_d(icomp,icfctp)=rprop*1000.  ![convert to mm]
            
            CALL EGETWR(LOUTSTR,K,rprop,.001,0.30,'F',
     &            'wire pitch (m)',IER)
            rim_bug_s(icomp,icfctp)=rprop*1000.  ![convert to mm]
            
            CALL EGETWR(LOUTSTR,K,rprop,.0,1.0,'F',
     &            'wire emissivity',IER)
            rim_bug_Emis(icomp,icfctp)=rprop


C currently no visible properties for insect screens, assume similar to
C total solar properties

            rim_VisT(icomp,icfctp,i) = rim_SolT(icomp,icfctp,i)
            rim_VisRf(icomp,icfctp,i)= rim_SolRf(icomp,icfctp,i)
            rim_VisRb(icomp,icfctp,i)= rim_SolRb(icomp,icfctp,i) 

            
C...........Longwave layer inputs are typical values for 0 openness.
C...........Effective lw properties are then calculated in cfc_thermal_processing()
C...........taking into account the openness (beam-beam tran at 0 incidence angle).

C...........Typical apparent LW emittance of insect screen at 0 openness. 
            rim_lwEf(icomp,icfctp,i) = rim_bug_Emis(icomp,icfctp)
            rim_lwEb(icomp,icfctp,i) = rim_bug_Emis(icomp,icfctp)
              
C...........Typical apparent LW transmittance of insect screen at 0 openness.
            if(rim_bug_Emis(icomp,icfctp).gt.0.5)then
              ! assume non-metallic insect screen
              rim_lwT(icomp,icfctp,i) = .02               
            else
              ! assume metallic insect screen
              rim_lwT(icomp,icfctp,i) = .19               
            endif

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
C attributes have been filled.
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
        if(im_cfcltp(icomp,icfctp,ie).eq.iGasGap)then
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
        H(1)=' CFCs must contain more than one layer.'
        CALL PHELPD('file error',2,'-',0,0,IER)
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
      integer nshade_layers !, vbshade_layers, drpshade_layers
      integer ie,ND,ISR,icn,ntmpcfc_el,iflag,istat
      integer lnblnk    !function definition
      
      DIMENSION IVA(MS)
    
      CHARACTER OUTSTR*124,OUTS*124,LUA*72,WORD*20

      real val
      logical vb_xst, drp_xst, rld_xst, bug_xst
      val=0.
      vb_xst = .false. ! slat-type shade
      drp_xst = .false. ! pleated drape
      rld_xst = .false. ! roller blind
      bug_xst = .false. ! insect screen
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
C ICFCFL != 0 and surface attribute is not 'CFC' then confirm
C if the surface attribute should be updated.
      IRVA=NS
      CALL EGETWIA(IUA,IVA,IRVA,0,MCFC,'W','cfc list',IER)
      ncfc(icomp)=0
      DO 10 I=1,NS
       icfcfl(icomp,i)=iva(i)
       icn=izstocn(icomp,i)        
       IF(icfcfl(icomp,i).NE.0.AND.(SSOTF(icn)(1:3).NE.'CFC'))THEN
         WRITE(OUTS,'(5a)')' The CFC file ',LUA(1:lnblnk(LUA)),
     &      ' not sure if ',SSNAME(icn),
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
          lnsmlcn=lnblnk(SSMLCN(ICN))
          WRITE(OUTS,'(1X,A,2X,A,2X,A,I5)')SSNAME(icn),
     &      SSMLCN(icn)(1:lnsmlcn),SSOTF(icn)(1:4),icfcfl(icomp,isr)
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
     &      SSNAME(J)(1:lnblnk(SSNAME(J))),' in ',
     &      zname(ICOMP)(1:lnblnk(zname(ICOMP))),' composed of ',
     &      SSMLCN(J)
            call edisp(iuout,outs)
            goto 1002
          ENDIF
   21   CONTINUE
        ncfc_el(icomp,i)=ntmpcfc_el
! 
C Read normal solar optical properties for complex fenestration construction.
      do 40 j=1,ncfc_el(icomp,i)
        CALL STRIPC(IUA,OUTSTR,99,ND,1,'normal solar-optical',IER)
        IF(IER.NE.0)GOTO 1001
        K=0
        CALL EGETWR(OUTSTR,K,VAL,0.,1.0,'W','front solar ref.',
     &              IER)
        solRF(ICOMP,i,j)=VAL

        CALL EGETWR(OUTSTR,K,VAL,0.,1.0,'W','back solar ref.',
     &              IER)
        solRB(ICOMP,i,j)=VAL

        CALL EGETWR(OUTSTR,K,VAL,0.,1.0,'W','front solar tran.',
     &              IER)
        solT(ICOMP,i,j)=VAL
        
C.......Check number of items. If only 3 then this is a legacy file.
C.......If 7 then this is a new file which includes the complete set 
C.......of optical properties required for pleated drapes, roller blinds
C.......and insect screens.        
        if(ND.eq.3)then
          SolTf_tot(ICOMP,i,j)=0.0
          SolTb_tot(ICOMP,i,j)=0.0
          SolTf_bd(ICOMP,i,j)=0.0
          SolTb_bd(ICOMP,i,j)=0.0
        else if(ND.eq.7)then
          CALL EGETWR(OUTSTR,K,VAL,0.,1.0,'W','total front tran.',
     &              IER)
          SolTf_tot(ICOMP,i,j)=VAL
          CALL EGETWR(OUTSTR,K,VAL,0.,1.0,'W','total back tran.',
     &              IER)
          SolTb_tot(ICOMP,i,j)=VAL
          CALL EGETWR(OUTSTR,K,VAL,0.,1.0,'W','bd front tran.',
     &              IER)
          SolTf_bd(ICOMP,i,j)=VAL
          CALL EGETWR(OUTSTR,K,VAL,0.,1.0,'W','bd back tran.',
     &              IER)
          SolTb_bd(ICOMP,i,j)=VAL
        endif

   40 continue

C Read visible solar properties for complex fenestration construction.
      do 50 j=1,ncfc_el(icomp,i)
        CALL STRIPC(IUA,OUTSTR,99,ND,1,'visible solar',IER)
        IF(IER.NE.0)GOTO 1001
        K=0
        CALL EGETWR(OUTSTR,K,VAL,0.,1.0,'W','front visible ref.',
     &              IER)
        visRF(ICOMP,i,j)=VAL

        CALL EGETWR(OUTSTR,K,VAL,0.,1.0,'W','back visible ref.',
     &              IER)
        visRB(ICOMP,i,j)=VAL

        CALL EGETWR(OUTSTR,K,VAL,0.,1.0,'W','front visible tran.',
     &              IER)
        visT(ICOMP,i,j)=VAL

   50 continue

C Read long-wave properties for complex fenestration construction.
      do 70 j=1,ncfc_el(icomp,i)
        CALL STRIPC(IUA,OUTSTR,99,ND,1,'long-wave',IER)
        IF(IER.NE.0)GOTO 1001
        K=0
        CALL EGETWR(OUTSTR,K,VAL,0.,1.0,'W','front lw emiss.',
     &              IER)
     
        rlwEF(ICOMP,i,j)=VAL
        rlwEF_sv(ICOMP,i,j)=VAL
        
        CALL EGETWR(OUTSTR,K,VAL,0.,1.0,'W','back lw emiss. ',
     &              IER)
     
        rlwEB(ICOMP,i,j)=VAL
        rlwEB_sv(ICOMP,i,j)=VAL
        
        CALL EGETWR(OUTSTR,K,VAL,0.,1.0,'W','lw trans.',
     &              IER)
     
        rlwT(ICOMP,i,j)=VAL
        rlwT_sv(ICOMP,i,j)=VAL
        
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
C icfcltp - 0=gas gap, 1=opaque/glass, 2=slat-type blind, 3=drapery, 4=roller drape
C 5 = insect screen
      do 80 i=1,ncfc(icomp)

        nshade_layers=0
  
        vb_xst = .false.
        drp_xst = .false.
        rld_xst = .false.
        bug_xst = .false.
  
        IRVA=ncfc_el(icomp,i)
      
        CALL EGETWIA(IUA,IVA,IRVA,0,5,'W','cfc layer type index',IER)

C check that the highest value of icfcltp <= 5 (either slat-type
C blind, insect screen, drape blind, roller blind or glazing) 

        do 90 j=1,ncfc_el(icomp,i)
             if(IVA(j).GE.2 .AND. IVA(j).LE.5)then

                 nshade_layers=nshade_layers+1
    
                 if(IVA(j).eq.iVenBlind)then

                     vb_xst = .true.

                 elseif(IVA(j).eq.iPleatedDrape)then

                     drp_xst = .true.

                 elseif(IVA(j).eq.iRollerBlind)then

                     rld_xst = .true.

                 elseif(IVA(j).eq.iInsectScreen)then

                     bug_xst = .true.

                 endif
    
             elseif(IVA(j).gt.5)then
               
                 call edisp(itru,' ')
                 call edisp(itru,
     &             ' CFC layer type index cannot exceed 5.')
                 GOTO 1002
             
              end if
             
              icfcltp(icomp,i,j)=IVA(j)
 
   90 continue

      if(nshade_layers.gt.1)then
  
      do 110 j=1,ncfc_el(icomp,i)-2

         if(IVA(j).gt.1.AND.IVA(j+2).gt.1)then

           call edisp(itru,' ')
           call edisp(itru,
     &       ' Detected two adjacent blind layers.')
           GOTO 1002

         end if

  110 continue 

      elseif(nshade_layers.eq.0)then
      
            vb_xst = .false.
            drp_xst = .false.
            rld_xst = .false.
            bug_xst = .false.
      
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

C If drape blind exists then read drape attributes:
C Read roller blind descriptors (for type 4 cfc layer):
C    drp_w      = drape width [mm]
C    drp_s      = drape spacing [mm]

      if(drp_xst)then
  
      CALL STRIPC(IUA,OUTSTR,99,ND,1,'drape attributes',IER)
      
      IF(IER.NE.0)GOTO 1001
      
      K=0
      
      CALL EGETWR(OUTSTR,K,VAL,1.0,300.,'W','pleat width(mm)',
     &              IER)
      drp_w(ICOMP,i)=VAL
  
      CALL EGETWR(OUTSTR,K,VAL,1.0,300.,'W','pleat spacing(mm)',
     &              IER)
      drp_s(ICOMP,i)=VAL
  
      IF(ITRC.GT.0)THEN
      
            CALL EDISP(ITRU,' Drape attributes:')
            CALL EDISP(ITRU,
     & ' width(mm),spacing(mm) ')
             WRITE(OUTS,'(1X,3F7.3,2F7.3)')drp_w(ICOMP,i),
     &      drp_s(ICOMP,i)
             
             CALL EDISP(ITRU,OUTS)
      
      ENDIF

      end if

C If insect screen exists then read drape attributes:
C Read insect screen descriptors (for type 5 cfc layer):
C    bug_d      = wire diameter [mm]
C    bug_s      = wire pitch [mm]
C    bug_Emis   = wire emissivity

      if(bug_xst)then
  
      CALL STRIPC(IUA,OUTSTR,99,ND,1,'drape attributes',IER)
      
      IF(IER.NE.0)GOTO 1001
      
      K=0
      
      CALL EGETWR(OUTSTR,K,VAL,1.0,300.,'W','wire diameter(mm)',
     &              IER)
      bug_d(ICOMP,i)=VAL
  
      CALL EGETWR(OUTSTR,K,VAL,1.0,300.,'W','wire pitch(mm)',
     &              IER)
      bug_s(ICOMP,i)=VAL

      CALL EGETWR(OUTSTR,K,VAL,0.0,1.0,'W','wire emissivity',
     &              IER)
      bug_Emis(ICOMP,i)=VAL
  
      IF(ITRC.GT.0)THEN 

            CALL EDISP(ITRU,' Insect screen attributes:')
            CALL EDISP(ITRU,
     & ' diameter(mm),pitch(mm),emissivity ')
            WRITE(OUTS,'(1X,3F7.3,2F7.3)')bug_d(ICOMP,i),
     &      bug_s(ICOMP,i),bug_Emis(ICOMP,i)
             
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
#include "epara.h"
#include "CFC_common.h"
#include "geometry.h"

      COMMON/C24/IZSTOCN(MCOM,MS) !array which holds the connection index
      integer izstocn

      COMMON/FILEP/IFIL
      common/pophelp/h(60)

      LOGICAL XST
      CHARACTER title*72
      
      DIMENSION VERT(35)
      CHARACTER VERT*27,KEY*1
      character h*72
      
      character OUTSTR*124
      integer IUA, NS, K, M, L, i, j, icn
      integer MVERT,IVERT ! max items and current menu item

      DIMENSION IVA(MS)
      
      IUA=IFIL+1
      
      XST=.false.

   5  MHEAD=0
      MCTL=3

C Initial menu entry setup.
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
        H(1)='Select CFC type to apply shading schedule/control.'

C Display the menu.
        write(title,'(2a)')'Select CFC type in ',
     &  zname(icomp)(1:lnblnk(zname(icomp)))
        CALL EMENU(title,VERT,MVERT,IVERT)        
        
C Get user selection
        if (IVERT.EQ.MVERT) then
          return
        elseif (IVERT.EQ.(MVERT-1)) then
          CALL PHELPD('Choose CFC type',1,'-',0,0,IER)
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
      
        H(1)='*.cfc file not found'
        H(2)=''
        H(3)='CFC input data must first be imported before'
        H(4)='editing the shading schedule can commence.'
       
        CALL PHELPD('CFC file not found',4,'-',0,0,IER)      
        
      ENDIF
 
c Free file.
 1000 CALL ERPFREE(IUA,ISTAT)
      return

C Error messages.
 1001 CALL USRMSG(' Problem reading data in:',OUTSTR,'W')
      IER=1
      goto 1000
      
      END

C ********************************************************************
C                   --calc_CFC_fill_gas_mix_coeff--
C
C Calculates fill gas properties for a given mixture by % mole fraction.
C
C INPUT: 
C        imlr_mass_frac(mcom,mcfc,me,5): 
C                Percent mole fractions for Air, Ar, Kr, Xe and SF6 for
C                a given CFC fill gas layer. 
C OUTPUT: 
C         cond_A(mcom,mcfc,me), cond_B(mcom,mcfc,me): 
C                Conductivity linear fit coefficients: A+B*Tgap = k [W/m.K]
C         visc_A(mcom,mcfc,me), visc_B(mcom,mcfc,me): 
C                Viscosity linear fit coefficients: A+B*Tgap = mu [kg/m.s]
C         spht_A(mcom,mcfc,me), spht_B(mcom,mcfc,me): 
C                Specifc heat linear fit coefficients: A+B*Tgap = Cp [J/kg.K]
C         rmlr_mass(mcom,mcfc,me):
C                Molar mass (g/gmole) of mixture
C
C Reference: VISION4 Glazing System Thermal Analysis: Reference Manual,
C            Advanced Glazing System Laboratory, University of Waterloo, 
C            May 1995
C            Contact E-mail: jlwright@uwaterloo.ca
C
C ********************************************************************
      subroutine calc_CFC_fill_gas_mix_coeff(icomp, icfctp, IL)
      IMPLICIT NONE
#include "building.h"
#include "CFC_common.h"

      INTEGER NGASP1 ! # of constituent gas types
      Parameter ( NGASP1 = 5 ) ! Air, Ar, Kr, Xe and SF6

      REAL, DIMENSION(NGASP1) :: mlr_mass_i
      REAL, DIMENSION(NGASP1) :: cond_Ai, cond_Bi
      REAL, DIMENSION(NGASP1) :: visc_Ai, visc_Bi
      REAL, DIMENSION(NGASP1) :: spht_Ai, spht_Bi
      INTEGER, DIMENSION(NGASP1) :: lc_imlr_mass_frac

      INTEGER i, j, k, icomp, icfctp, IL
      REAL fsum, fsum_a, fsum_b
      REAL T1, T2 ! two temperatures used to calculate linear temp. dependence
      REAL viscT1, viscT2
      REAL get_mixture_viscosity
      REAL condT1, condT2
      REAL get_mixture_conductivity

      T1 = 273.15+20. ![K]
      T2 = 273.15-20. ![K]

      do i = 1, NGASP1
        lc_imlr_mass_frac(i) = imlr_mass_frac(icomp, icfctp, IL,i)
      end do

C.....Molar mass (g/gmole) of Air, Ar, Kr, Xe and SF6
      mlr_mass_i = (/ 28.97, 39.948, 83.8, 131.3, 146.1 /)

C.....Conductivity A coefficients of Air, Ar, Kr, Xe and SF6
      cond_Ai = (/ 0.0953286*2.414e-02, 
     &             0.151607*1.634e-02, 
     &             0.855136*1.e-03,
     &             4.62e-04, 
     &             0.013 /)
C.....Conductivity B coefficients of Air, Ar, Kr, Xe and SF6
      cond_Bi = (/ 0.0033086*2.414e-02, 
     &             0.0031036*1.634e-02, 
     &             0.0286275*1.e-03,
     &             1.72e-05,
     &             0.0 /)

C.....Viscosity A coefficients of Air, Ar, Kr, Xe and SF6
      visc_Ai = (/ 0.0035165, 
     &             0.003618,
     &             0.00234,
     &             7.652e-04,
     &             7.214e-04 /)
C.....Viscosity B coefficients of Air, Ar, Kr, Xe and SF6
      visc_Bi = (/ 0.0000498, 
     &             0.0000644, 
     &             0.0000783, 
     &             7.43333e-05, 
     &             4.928e-05 /)

C.....Specific heat A coefficients of Air, Ar, Kr, Xe and SF6
      spht_Ai = (/ 3.4898964*0.287041*mlr_mass_i(1), 
     &             2.525325*0.208152*mlr_mass_i(2), 
     &             0.2497*mlr_mass_i(3), 
     &             0.158*mlr_mass_i(4), 
     &             0.4186*mlr_mass_i(5) /)
C.....Specific heat B coefficients of Air, Ar, Kr, Xe and SF6
      spht_Bi = (/ 0.0000511*0.287041*mlr_mass_i(1), 0., 0., 0., 0. /)

C.....Calculate molar mass of mixture (g/gmole)
      fsum = 0.0
      do i = 1, NGASP1
        fsum = fsum + lc_imlr_mass_frac(i)*mlr_mass_i(i)
      end do
      rim_mlr_mass(icomp, icfctp, IL) = fsum / 100.

C.....Calculate A and B specific heat coefficients
C.....of the mixture [J/kg.K]
      fsum_a = 0.
      fsum_b = 0.
      do i = 1, NGASP1
        fsum_a = fsum_a + 
     &             imlr_mass_frac(icomp, icfctp, IL,i) * spht_Ai(i)
        fsum_b = fsum_b + 
     &             imlr_mass_frac(icomp, icfctp, IL,i) * spht_Bi(i) 
      end do
      rim_spht_A(icomp,icfctp,IL) = 1000.*fsum_a /
     &                        (100.*rim_mlr_mass(icomp, icfctp, IL))
      rim_spht_B(icomp,icfctp,IL) = 1000.*fsum_b /
     &                        (100.*rim_mlr_mass(icomp, icfctp, IL))

C.....Get viscosity at T1 and T2
      viscT1 = get_mixture_viscosity(T1, visc_Ai, visc_Bi,
     &                       mlr_mass_i, lc_imlr_mass_frac, NGASP1)
      viscT2 = get_mixture_viscosity(T2, visc_Ai, visc_Bi,
     &                       mlr_mass_i, lc_imlr_mass_frac, NGASP1)
C.....Get y intercept (a) and slope (b) based on the two temperatre points
      rim_visc_B(icomp,icfctp,IL) = (viscT2-viscT1)/(T2-T1)
      rim_visc_A(icomp,icfctp,IL) = 
     &          viscT1-rim_visc_B(icomp,icfctp,IL)*T1

C.....Get conductivity at T1 and T2
      condT1 = get_mixture_conductivity(T1, cond_Ai, cond_Bi,
     &         visc_Ai, visc_Bi, mlr_mass_i, lc_imlr_mass_frac, NGASP1)
      condT2 = get_mixture_conductivity(T2, cond_Ai, cond_Bi,
     &         visc_Ai, visc_Bi, mlr_mass_i, lc_imlr_mass_frac, NGASP1)
C.....Get y intercept (a) and slope (b) based on the two temperatre points
      rim_cond_B(icomp,icfctp,IL) = (condT2-condT1)/(T2-T1)
      rim_cond_A(icomp,icfctp,IL) = 
     &          condT1-rim_cond_B(icomp,icfctp,IL)*T1

      end

C ********************************************************************
C                       --get_mixture_viscosity--
C
C Calculates gas mixture viscosity for a given temperature.
C
C Reference: VISION4 Glazing System Thermal Analysis: Reference Manual,
C            Advanced Glazing System Laboratory, University of Waterloo, 
C            May 1995
C            Contact E-mail: jlwright@uwaterloo.ca
C
C ********************************************************************
      FUNCTION get_mixture_viscosity(Tm, visc_Ai, visc_Bi,
     &                       mlr_mass_i, lc_imlr_mass_frac, NGASP1)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: NGASP1 ! # of constituent gas types
      INTEGER, INTENT(IN) :: lc_imlr_mass_frac(NGASP1)
      REAL, INTENT(IN) :: Tm
      REAL, INTENT(IN) :: mlr_mass_i(NGASP1)
      REAL, INTENT(IN) :: visc_Ai(NGASP1), visc_Bi(NGASP1)

      REAL get_mixture_viscosity
      INTEGER i, j, icomp, icfctp, IL
      REAL dv, dv1, dv2, denom, phi, fsum

      fsum = 0.
      do i = 1, NGASP1
        denom = 0.
        if(lc_imlr_mass_frac(i).gt.0.)then
          do j = 1, NGASP1
            dv1=(visc_Ai(i)+visc_Bi(i)*Tm)/(visc_Ai(j)+visc_Bi(j)*Tm)
            dv1=SQRT(dv1)
            dv2=mlr_mass_i(j)/mlr_mass_i(i)
            dv2=SQRT(dv2)    
            dv2=SQRT(dv2)
            dv =(1.+dv1*dv2)*(1.+dv1*dv2)
            dv1=mlr_mass_i(i)/mlr_mass_i(j)
            dv1=SQRT((1.+dv1))
            phi=dv/(dv1*2.*SQRT((2.)))
            denom = denom + phi*(lc_imlr_mass_frac(j))/
     &              (lc_imlr_mass_frac(i))
          end do      
          if(denom.gt.0.00001)then
            fsum = fsum + (visc_Ai(i) + visc_Bi(i)*Tm) / denom
          end if
        end if
      end do

      get_mixture_viscosity = fsum / 1000. ! [kg/m.s]

      END FUNCTION get_mixture_viscosity

C ********************************************************************
C                       --get_mixture_conductivity--
C
C Calculates gas mixture conductivity for a given temperature.
C
C Reference: VISION4 Glazing System Thermal Analysis: Reference Manual,
C            Advanced Glazing System Laboratory, University of Waterloo, 
C            May 1995
C            Contact E-mail: jlwright@uwaterloo.ca
C
C ********************************************************************
      FUNCTION get_mixture_conductivity(Tm, cond_Ai, cond_Bi,
     &      visc_Ai, visc_Bi, mlr_mass_i, lc_imlr_mass_frac, NGASP1)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: NGASP1 ! # of constituent gas types
      INTEGER, INTENT(IN) :: lc_imlr_mass_frac(NGASP1)
      REAL, INTENT(IN) :: Tm
      REAL, INTENT(IN) :: mlr_mass_i(NGASP1)
      REAL, INTENT(IN) :: cond_Ai(NGASP1), cond_Bi(NGASP1)
      REAL, INTENT(IN) :: visc_Ai(NGASP1), visc_Bi(NGASP1)

      REAL R_UGC ! Universal gas constant
      Parameter ( R_UGC = 8.3144621 / 1000. ) ! J/gmol.K

      REAL get_mixture_conductivity
      INTEGER i, j, icomp, icfctp, IL
      REAL dv, dv1, dv2, denom, psi, phi, fsum, kpi,kpj, kdpi

      fsum = 0.
      do i = 1, NGASP1
        denom = 0.
        if(lc_imlr_mass_frac(i).gt.0.)then
          kpi = (15./4.)*(R_UGC/mlr_mass_i(i))*
     &        (visc_Ai(i)+visc_Bi(i)*Tm)
          do j = 1, NGASP1
            kpj = (15./4.)*(R_UGC/mlr_mass_i(j))*
     &          (visc_Ai(j)+visc_Bi(j)*Tm)
            dv1 = SQRT(kpi/kpj)
            dv2=mlr_mass_i(i)/mlr_mass_i(j)
            dv2=SQRT(dv2)
            dv2=SQRT(dv2)
            dv =(1.+dv1*dv2)*(1.+dv1*dv2)
            dv1=mlr_mass_i(i)/mlr_mass_i(j)
            dv1=SQRT((1.+dv1))
            phi=dv/(dv1*2.*SQRT((2.)))
            denom = denom + phi * (lc_imlr_mass_frac(j))/
     &              (lc_imlr_mass_frac(i))
          end do      
          kdpi = (cond_Ai(i) + cond_Bi(i)*Tm) - kpi
          if(denom.gt.0.00001)then       
            fsum = fsum + kdpi / denom 
          end if
        end if
      end do

      do i = 1, NGASP1
        denom = 0.
        if(lc_imlr_mass_frac(i).gt.0.)then
          kpi = (15./4.)*(R_UGC/mlr_mass_i(i))*
     &        (visc_Ai(i)+visc_Bi(i)*Tm)
          do j = 1, NGASP1
            kpj = (15./4.)*(R_UGC/mlr_mass_i(j))*
     &          (visc_Ai(j)+visc_Bi(j)*Tm)
            dv1 = SQRT(kpi/kpj)
            dv2=mlr_mass_i(i)/mlr_mass_i(j)
            dv2=SQRT(dv2)
            dv2=SQRT(dv2)
            dv =(1.+dv1*dv2)*(1.+dv1*dv2)
            dv1=mlr_mass_i(i)/mlr_mass_i(j)
            dv1=SQRT((1.+dv1))
            psi=dv/(dv1*2.*SQRT(2.))
            dv1=mlr_mass_i(i)-mlr_mass_i(j)
            dv2=mlr_mass_i(i)-0.142*mlr_mass_i(j)
            dv=(mlr_mass_i(i)+mlr_mass_i(j))*
     &         (mlr_mass_i(i)+mlr_mass_i(j))
            dv=1.0 + 2.41*dv1*dv2/dv
            psi = psi * dv
            denom = denom + psi*(lc_imlr_mass_frac(j))/
     &              (lc_imlr_mass_frac(i))
          end do      
          if(denom.gt.0.00001)then
            fsum = fsum + kpi / denom
          end if
        end if
      end do

      get_mixture_conductivity = fsum ! [W/m.K]

      END FUNCTION get_mixture_conductivity
