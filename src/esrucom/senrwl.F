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

C Controling routines for parametric studies:
C READUAL:    Reads the information contained in the UA library.
C WRITUAL:    Writes the information contained in the UA commons to 
C             the UA library.
C listual     List contents of uncertainties file.

C ***************** READUAL *********************
C READUAL reads the data fron an existing UA library.

      subroutine READUAL(IUALF)
#include "building.h"
#include "uncertainty.h"
#include "espriou.h"
C espriou.h provides currentfile.
#include "control.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)
      COMMON/UA2/NILOC,NZG(MNIL),NZNOG(MNIL,MCOM),NSG(MNIL,MCOM),
     &             NSNOG(MNIL,MCOM,MS)
      COMMON/UA2T/NTLOC(MNIL),NTG(MNIL,4)
      COMMON/UA3/NICNG,IDTYP(MNCNG),IDMAT(MNCNG,3),DAT(MNCNG,2)
      COMMON/UA31/MATNAM(MNCNG,2)
      COMMON/UA32/IMISCD(MNCNG,2)
      COMMON/UA4/NIACT,IACTD(MNACT,2)
      
      character LUALF*72,LCNG*15,LLOC*15,MATNAM*12
      CHARACTER OUTSTR*124, WORD*20,outs*124


      IER=0
      call EFOPSEQ(IUALF,LUALF,1,IER)
      if (IER.lt.0) then
        return
      endif
      write(currentfile,'(a)') LUALF(1:lnblnk(LUALF))

      call STRIPC(IUALF,OUTSTR,0,ND,0,'line 1',IER)
      if (IER.NE.0) return
      K=0
      call EGETW(OUTSTR,K,WORD,'W','File type check',IFLAG)
      if (WORD(1:7).ne.'*Uncert') then
        call EDISP (IUOUT,' Not an uncertainty analysis library.')
        call ERPFREE(IUALF,ISTAT)
        return
      endif

      CALL EDISP(IUOUT,'Activating uncertainties....')

C Read the number of changes, locations and actions
      call STRIPC(IUALF,OUTSTR,3,ND,0,'line 2',IER)
      if (IER.NE.0) return
      K=0
      call EGETWI(OUTSTR,K,NICNG,0,MNCNG,'F','Changes',IER)
      call EGETWI(OUTSTR,K,NILOC,0,MNIL,'F','Locations',IER)
      call EGETWI(OUTSTR,K,NIACT,0,MNACT,'F','Actions',IER)

C Read the changes.
      do 10 II=1,NICNG
        IER=0
        call STRIPC(IUALF,OUTSTR,0,ND,0,'change definition',IER)
        if (IER.NE.0) return
        K=0
        call EGETW(OUTSTR,K,WORD,'W','change type check',IFLAG)
        if (WORD(1:8).ne.'*cng_def') then
          call EDISP (IUOUT,' Not a change definition.')
          call ERPFREE(IUALF,ISTAT)
          return
        endif
        call STRIPC(IUALF,OUTSTR,0,ND,0,'change name',IER)
        if (IER.NE.0) return
        K=0
        call EGETW(OUTSTR,K,LCNG(II),'W','change name',IFLAG)
        call STRIPC(IUALF,OUTSTR,1,ND,0,'change type id.',IER)
        if (IER.NE.0) return

C Provide feedback about what we're reading in.
        write (outs,'(2a)') 'Reading change definition:',LCNG(II)
        call EDISP (IUOUT,outs)

        K=0
        call EGETWI(OUTSTR,K,IDTYP(II),1,1001,'W',
     &                                              'change type',IER)
        if (IDTYP(II).eq.1) then

C Thermophysical property.
          call STRIPC(IUALF,OUTSTR,2,ND,0,'db ref & prop.',IER)
          if (IER.NE.0) return
          K=0
          call EGETWI(OUTSTR,K,IDMAT(II,1),1,500,'W',
     &                                                   'db ref.',IER)
          call EGETWI(OUTSTR,K,IDMAT(II,2),1,6,'W',
     &                                             'property ref.',IER)
          call STRIPC(IUALF,OUTSTR,3,ND,0,'data.',IER)
          if (IER.NE.0) return
          K=0
          call EGETWI(OUTSTR,K,IDMAT(II,3),1,3,'W',
     &                                          'change type ref.',IER)
          call EGETWR(OUTSTR,K,DAT(II,1),0.,100.,'W',
     &                                               'change mag.',IER)
          call EGETWR(OUTSTR,K,DAT(II,2),0.,100.,'W',
     &                                               'change mag.',IER)
        elseif (IDTYP(II).eq.2) then

C MLC name or layer thickness.
          call STRIPC(IUALF,OUTSTR,2,ND,0,'Layer and MLC name.',IER)
          if (IER.NE.0) return
          K=0
          call EGETWI(OUTSTR,K,IDMAT(II,1),1,500,'W',
     &                                                   'Layer.',IER)
          if (IDMAT(II,1).eq.0) then

C 2 MLC names.
            call EGETW(OUTSTR,K,MATNAM(II,1),'W','MLC name 1',IFLAG)
            call EGETW(OUTSTR,K,MATNAM(II,2),'W','MLC name 2',IFLAG)
          else

C 1 MLC name and data for thickness change.
            call EGETW(OUTSTR,K,MATNAM(II,1),'W','MLC name 1',IFLAG)
            call STRIPC(IUALF,OUTSTR,3,ND,0,'data.',IER)
            if (IER.NE.0) return
            K=0
            call EGETWI(OUTSTR,K,IDMAT(II,3),1,3,'W',
     &                                          'change type ref.',IER)
            call EGETWR(OUTSTR,K,DAT(II,1),0.,100.,'W',
     &                                               'change mag.',IER)
            call EGETWR(OUTSTR,K,DAT(II,2),0.,100.,'W',
     &                                               'change mag.',IER)
          endif
        elseif (IDTYP(II).eq.3) then

C Climate parameters.
          call STRIPC(IUALF,OUTSTR,1,ND,0,'Clm par.',IER)
          if (IER.NE.0) return
          K=0
          call EGETWI(OUTSTR,K,IDMAT(II,1),1,6,'W','Clm var.',IER)
          call STRIPC(IUALF,OUTSTR,3,ND,0,'data.',IER)
          if (IER.NE.0) return
          K=0
          call EGETWI(OUTSTR,K,IDMAT(II,3),1,2,'W',
     &                                          'change type ref.',IER)
          call EGETWR(OUTSTR,K,DAT(II,1),0.,10.,'W',
     &                                               'change mag.',IER)
          call EGETWR(OUTSTR,K,DAT(II,2),0.,10.,'W',
     &                                               'change mag.',IER)
        elseif (IDTYP(II).eq.4) then

C Scheduled operations.
          call STRIPC(IUALF,OUTSTR,2,ND,0,'Oper data.',IER)
          if (IER.NE.0) return
          K=0
          call EGETWI(OUTSTR,K,IDMAT(II,1),0,4,'W','Op type.',IER)
          call EGETWI(OUTSTR,K,IDMAT(II,2),1,8,'W','Property.',IER)

C Read associated data.
          call STRIPC(IUALF,OUTSTR,3,ND,0,'data.',IER)
          if (IER.NE.0) return
          K=0
          call EGETWI(OUTSTR,K,IDMAT(II,3),1,3,'W',
     &                                          'change type ref.',IER)
          call EGETWR(OUTSTR,K,DAT(II,1),0.,100.,'W',
     &                                               'change mag.',IER)
          call EGETWR(OUTSTR,K,DAT(II,2),0.,100.,'W',
     &                                               'change mag.',IER)
        elseif (IDTYP(II).eq.5) then

C Convection coefficients.
          call STRIPC(IUALF,OUTSTR,2,ND,0,'Conv coeffs.',IER)
          if (IER.NE.0) return
          K=0
          call EGETWI(OUTSTR,K,IDMAT(II,1),1,2,'W','dummy.',IER)
          call EGETWI(OUTSTR,K,IDMAT(II,2),1,2,'W','dummy.',IER)
          if (IDMAT(II,1).ne.0) then

C Read associated data.
            call STRIPC(IUALF,OUTSTR,3,ND,0,'corelations.',IER)
            if (IER.NE.0) return
            K=0
            call EGETWI(OUTSTR,K,IDMAT(II,3),4,4,'W',
     &                                          'corr ids.',IER)
            call EGETWR(OUTSTR,K,DAT(II,1),1.,8.,'W',
     &                                               'high corr.',IER)
            call EGETWR(OUTSTR,K,DAT(II,2),1.,8.,'W',
     &                                               'low corr.',IER)
          endif
        elseif (IDTYP(II).eq.101) then

C Zone level assumptions.
          call STRIPC(IUALF,OUTSTR,2,ND,0,'Zone air.',IER)
          if (IER.NE.0) return
          K=0
          call EGETWI(OUTSTR,K,IDMAT(II,1),1,1,'W','Air.',IER)
          call EGETWI(OUTSTR,K,IDMAT(II,2),1,1,'W','Property.',IER)
          if (IDMAT(II,1).ne.0) then

C Read associated data.
            call STRIPC(IUALF,OUTSTR,3,ND,0,'data.',IER)
            if (IER.NE.0) return
            K=0
            call EGETWI(OUTSTR,K,IDMAT(II,3),1,4,'W',
     &                                          'change type ref.',IER)
            call EGETWR(OUTSTR,K,DAT(II,1),0.,100.,'W',
     &                                               'change mag.',IER)
            call EGETWR(OUTSTR,K,DAT(II,2),0.,100.,'W',
     &                                               'change mag.',IER)
          endif
        elseif (IDTYP(II).eq.1001) then

C Control uncertainties.
          call STRIPC(IUALF,OUTSTR,4,ND,0,'Control.',IER)
          if (IER.NE.0) return
          K=0
          call EGETWI(OUTSTR,K,IDMAT(II,1),1,mcf,'W',
     &                                        'Function.',IER)
          call EGETWI(OUTSTR,K,IDMAT(II,2),1,MCDT,'W',
     &                                        'Day type.',IER)
          call EGETWI(OUTSTR,K,IMISCD(II,1),1,MCDP,'W',
     &                                        'Period.',IER)
          call EGETWI(OUTSTR,K,IMISCD(II,2),1,7,'W',
     &                                        'Ctl parameter.',IER)

C Read associated data.
          call STRIPC(IUALF,OUTSTR,3,ND,0,'data.',IER)
          if (IER.NE.0) return
          K=0
          call EGETWI(OUTSTR,K,IDMAT(II,3),1,4,'W',
     &                                          'change type ref.',IER)
          call EGETWR(OUTSTR,K,DAT(II,1),0.,100.,'W',
     &                                               'change mag.',IER)
          call EGETWR(OUTSTR,K,DAT(II,2),0.,100.,'W',
     &                                               'change mag.',IER)
        endif
 10   continue

C Read the locations.
      do 20 II=1,NILOC
        call STRIPC(IUALF,OUTSTR,0,ND,0,'location definition',IER)
        if (IER.NE.0) return
        K=0
        call EGETW(OUTSTR,K,WORD,'W','definition type check',IFLAG)
        if (WORD(1:8).ne.'*loc_def') then
          call EDISP (IUOUT,' Not a location definition.')
          call ERPFREE(IUALF,ISTAT)
          return
        endif
        call STRIPC(IUALF,OUTSTR,0,ND,0,'location name',IER)
        if (IER.NE.0) return
        K=0
        call EGETW(OUTSTR,K,LLOC(II),'W','location name',IFLAG)
        call STRIPC(IUALF,OUTSTR,1,ND,0,'no of zones.',IER)
        if (IER.NE.0) return
        K=0
        call EGETWI(OUTSTR,K,NZG(II),0,MCOM,'W','no of zones.',IER)

C If NZG(II)<=1 then time based location.
        if (NZG(II).lt.1) then

C Set NZG to positive value.
          NTLOC(II)=-1
          NZG(II)=NZG(II)*NTLOC(II)

C Uncertainty will have a start and finish time. NTLOC,NTG(MNIL,4)
C          NTG(II,1)=start day
C          NTG(II,2)=start hour
C          NTG(II,3)=finish day
C          NTG(II,4)=finish hour
          call STRIPC(IUALF,OUTSTR,4,ND,0,'start/ finish times',IER)
          K=0
          call EGETWI(OUTSTR,K,NTG(II,1),1,365,'W','start day',IER)
          call EGETWI(OUTSTR,K,NTG(II,2),1,24,'W','start hour',IER)
          call EGETWI(OUTSTR,K,NTG(II,3),
     &                            NTG(II,1),365,'W','finish day',IER)
          if (NTG(II,3).gt.NTG(II,1)) then
            call EGETWI(OUTSTR,K,NTG(II,4),1,24,'W','finish hour',IER)
          else
            call EGETWI(OUTSTR,K,NTG(II,4),
     &                            NTG(II,2),24,'W','finish hour',IER)
          endif
        endif
        if (NZG(II).gt.0) then

C For each zone read the zone id number and then the number of surfaces 
C included in the definition.
          do 201 IJ=1,NZG(II)
            call STRIPC(IUALF,OUTSTR,2,ND,
     &                                  0,'zones no and surfaces.',IER)
            if (IER.NE.0) return
            K=0
            call EGETWI(OUTSTR,K,NZNOG(II,IJ),
     &                                      1,MCOM,'W','zones no.',IER)
            call EGETWI(OUTSTR,K,NSG(II,IJ),
     &                                1,MCOM,'W','no of surfaces.',IER)

C Read surface list.
            call STRIPC(IUALF,OUTSTR,NSG(II,IJ),
     &                                        ND,0,'surface list.',IER)
            if (IER.NE.0) return
            K=0
            do 202 IK=1,NSG(II,IJ)
              call EGETWI(OUTSTR,K,NSNOG(II,IJ,IK),
     &                                    1,MCOM,'W','surface no.',IER)
 202        continue
 201      continue
        endif
 20   continue

C Read the actions.
      do 30 II=1,NIACT
        call STRIPC(IUALF,OUTSTR,0,ND,0,'action definition',IER)
        if (IER.NE.0) return
        K=0
        call EGETW(OUTSTR,K,WORD,'W','action type check',IFLAG)
        if (WORD(1:8).ne.'*act_def') then
          call EDISP (IUOUT,' Not an action definition.')
          call ERPFREE(IUALF,ISTAT)
          return
        endif
        call STRIPC(IUALF,OUTSTR,1,ND,0,'distrib ref.',IER)
        if (IER.NE.0) return
        K=0
        call EGETWI(OUTSTR,K,IACTD(II,1),1,MNCNG,'W','distrib',IER)
        call STRIPC(IUALF,OUTSTR,1,ND,0,'location ref.',IER)
        if (IER.NE.0) return
        K=0
        call EGETWI(OUTSTR,K,IACTD(II,2),1,MNIL,'W','locat.',IER)
 30   continue

      call ERPFREE(IUALF,ISTAT)

      return
      END

C ***************** WRITUAL *********************
C WRITUAL reads the data fron an existing UA library.

      subroutine WRITUAL(IUALF)
#include "building.h"
#include "uncertainty.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)
      COMMON/UA2/NILOC,NZG(MNIL),NZNOG(MNIL,MCOM),NSG(MNIL,MCOM),
     &             NSNOG(MNIL,MCOM,MS)
      COMMON/UA2T/NTLOC(MNIL),NTG(MNIL,4)
      COMMON/UA3/NICNG,IDTYP(MNCNG),IDMAT(MNCNG,3),DAT(MNCNG,2)
      COMMON/UA31/MATNAM(MNCNG,2)
      COMMON/UA4/NIACT,IACTD(MNACT,2)

      character LUALF*72,LCNG*15,LLOC*15,outs*124,MATNAM*12

      IER=0

C Open existing file.
      call EFOPSEQ(IUALF,LUALF,4,IER)
      if (IER.LT.0) then
        IER=1
        return
      endif

      write (IUALF,30,IOSTAT=ISTAT,ERR=1003)
  30  FORMAT('*Uncertainty analysis library')

      write (IUALF,32,IOSTAT=ISTAT,ERR=1003)NICNG,NILOC,NIACT
  32  FORMAT(3i4,' # Changes, Locations, Actions')

C For each change write info.
      write (IUALF,40,IOSTAT=ISTAT,ERR=1003)
  40  FORMAT('#  ')
      write (IUALF,41,IOSTAT=ISTAT,ERR=1003)
  41  FORMAT('# Changes definitions follow...')
      write (IUALF,43,IOSTAT=ISTAT,ERR=1003)
  43  FORMAT('#  ')
      do 1000, II=1,NICNG
        write (IUALF,42,IOSTAT=ISTAT,ERR=1003)
  42    FORMAT('*cng_def')
        write (IUALF,44,IOSTAT=ISTAT,ERR=1003)LCNG(II)
  44    FORMAT(a15,' # Change id string')
        write (outs,'(a)') ' # Change type: unknown '
        if (IDTYP(II).eq.1) then
          write (outs,'(a)') ' # Change type: Materials'
        elseif (IDTYP(II).eq.2) then
          write (outs,'(a)') ' # Change type: Composite construction'
        elseif (IDTYP(II).eq.3) then
          write (outs,'(a)') ' # Change type: Climate parameter'
        elseif (IDTYP(II).eq.4) then
          write (outs,'(a)') ' # Change type: Scheduled operation'
        elseif (IDTYP(II).eq.5) then
          write (outs,'(a)') ' # Change type: Convection coefficients'
        endif
        write (IUALF,46,IOSTAT=ISTAT,ERR=1003)IDTYP(II),outs
  46    FORMAT(i4,a40)
        if (IDTYP(II).eq.1) then
          write(IUALF,45,IOSTAT=ISTAT,ERR=1003)IDMAT(II,1),IDMAT(II,2)
  45      FORMAT(2i4,' # material db ref, property id')
        elseif (IDTYP(II).eq.2) then
          write(IUALF,48,IOSTAT=ISTAT,ERR=1003)IDMAT(II,1),MATNAM(II,1)
  48      FORMAT(i4,' ',a,' # material db ref, composite name')
        elseif (IDTYP(II).eq.3) then
          write (IUALF,47,IOSTAT=ISTAT,ERR=1003)IDMAT(II,1)
  47      FORMAT(i4,' # Climate parameter ref')
        else
          write (IUALF,49,IOSTAT=ISTAT,ERR=1003)IDMAT(II,1),IDMAT(II,2)
  49      FORMAT(2i4,' # ref 1, ref 2')
        endif
        write (IUALF,50,IOSTAT=ISTAT,ERR=1003)IDMAT(II,3),
     &                                              DAT(II,1),DAT(II,2)
  50    FORMAT(i4,2f10.3,' # Data')
 1000 continue

C For each location write info.
      write (IUALF,60,IOSTAT=ISTAT,ERR=1003)
  60  FORMAT('#  ')
      write (IUALF,61,IOSTAT=ISTAT,ERR=1003)
  61  FORMAT('# Locations definitions follow...')
      write (IUALF,63,IOSTAT=ISTAT,ERR=1003)
  63  FORMAT('#  ')
      do 2000, II=1,NILOC
        write (IUALF,62,IOSTAT=ISTAT,ERR=1003)
  62    FORMAT('*loc_def')
        write (IUALF,64,IOSTAT=ISTAT,ERR=1003)LLOC(II)
  64    FORMAT(a15,' # Location id string')
        if (NTLOC(II).eq.0) then
          write (IUALF,66,IOSTAT=ISTAT,ERR=1003)NZG(II)
        else
          write (IUALF,66,IOSTAT=ISTAT,ERR=1003)NZG(II)*NTLOC(II)
        endif
  66    FORMAT(i4,' # Number of zones')

C If NTLOC=-1 then write time information.
        if (NTLOC(II).eq.-1) then
          write (IUALF,67,IOSTAT=ISTAT,ERR=1003)(NTG(II,IJ),IJ=1,4)
  67      FORMAT(4i4,' # Start day, hour, finish day, hour')
        endif

C Data for each zone.
        do 2100, IJ=1,NZG(II)
          write (IUALF,68,IOSTAT=ISTAT,ERR=1003)NZNOG(II,IJ),NSG(II,IJ)
  68      FORMAT(2i4,' # Zone number, Number of surfaces')
          write (IUALF,70,IOSTAT=ISTAT,ERR=1003)
     &                                (NSNOG(II,IJ,IK),IK=1,NSG(II,IJ))
  70      FORMAT(25i3,' # Surfaces')
 2100   continue
 2000 continue

C For each actions write info.
      write (IUALF,80,IOSTAT=ISTAT,ERR=1003)
  80  FORMAT('#  ')
      write (IUALF,81,IOSTAT=ISTAT,ERR=1003)
  81  FORMAT('# Action definitions follow...')
      write (IUALF,83,IOSTAT=ISTAT,ERR=1003)
  83  FORMAT('#  ')
      do 3000, II=1,NIACT
        write (IUALF,82,IOSTAT=ISTAT,ERR=1003)
  82    FORMAT('*act_def')
        write (IUALF,86,IOSTAT=ISTAT,ERR=1003)
     &                                    IACTD(II,1),LCNG(IACTD(II,1))
  86    FORMAT(i4,' # Uncertanty ref: ',a15)
        write (IUALF,88,IOSTAT=ISTAT,ERR=1003)
     &                                    IACTD(II,2),LLOC(IACTD(II,2))
  88    FORMAT(i4,' # Location ref: ',a15)
 3000 continue

   99 CALL ERPFREE(IUALF,ISTAT)
      RETURN

C Error messages.
 1003 CALL USRMSG(' File write error in ',LUALF,'W')
      IER=1
      GOTO 99
      
      END

C ***************** listual *****************
C ISEC defines which section of the uncertainty definitions to list
C IELM defines which element.
C 0=all; 1=distributions; 2=locations; 3=uncertainties.

      subroutine listual(ISEC,IELM)
#include "building.h"
#include "geometry.h"
#include "uncertainty.h"
#include "esprdbfile.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/UA1/LUALF,LCNG(MNCNG),LLOC(MNIL)
      COMMON/UA2/NILOC,NZG(MNIL),NZNOG(MNIL,MCOM),NSG(MNIL,MCOM),
     &             NSNOG(MNIL,MCOM,MS)
      COMMON/UA3/NICNG,IDTYP(MNCNG),IDMAT(MNCNG,3),DAT(MNCNG,2)
      COMMON/UA31/MATNAM(MNCNG,2)
      COMMON/UA4/NIACT,IACTD(MNACT,2)

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
      logical closemat

      integer legmat  ! local variable for legacy index.
      integer matarrayindex ! the indes within matdatarray

      character LUALF*72,LCNG*15,LLOC*15,outs*124
      character NAM*72,TITL*72,MATNAM*12,ITEM(6)*20

      IER=0

      call edisp (iuout,'  ')
      write(outs,'(a,a)')'Uncertainty analysis library: ',LUALF(1:40)
      call edisp (iuout,outs)

      if (ISEC.eq.0.or.ISEC.eq.1) then
C List distributions.

        call edisp (iuout,'  ')
        write(outs,'(a,i4)')'Number of distributions defined:',NICNG
        call edisp (iuout,outs)
        call eclose(matver,1.1,0.01,closemat)
        do 10 I=1,NICNG
          if (IELM.eq.0.or.IELM.eq.I) then
            call edisp (iuout,'  ')
            write(outs,'(a,i4,a,a)')'Distribution :',I,'; ',LCNG(I)
            call edisp (iuout,outs)
            if (IDTYP(I).eq.1) then
              IDB=IDMAT(I,1)
              if(closemat)then
                legmat=IDB   ! which legacy index
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
                CALL ERPCDB(IFMAT,IDB,ITRU,IERR,DBCON,DBDEN,DBSHT,E,A,
     &            DRV,TITL,NAM)
              endif
              write(outs,'(2a)')' Material: ',NAM
              call edisp (iuout,outs)
              if (IDMAT(I,2).eq.1) then
                call edisp(iuout,' Uncertain property: conductivity.')
              elseif (IDMAT(I,2).eq.2) then
                call edisp(iuout,' Uncertain property: density.')
              elseif (IDMAT(I,2).eq.3) then
                call edisp(iuout,' Uncertain property: specific heat.')
              elseif (IDMAT(I,2).eq.4) then
                call edisp(iuout,' Uncertain property: emissivity.')
              elseif (IDMAT(I,2).eq.5) then
                call edisp(iuout,' Uncertain property: absorptivity.')
              elseif (IDMAT(I,2).eq.6) then
                call edisp(iuout,' Uncertain property: diffusion res.')
              endif
            elseif(IDTYP(I).eq.2) then
              write(outs,'(2a)')' Composite construction: ',MATNAM(I,1)
              call edisp (iuout,outs)
              if (IDMAT(I,1).eq.0) then
                call edisp (iuout,'Undefined')
              else
                IDB=IDMAT(I,1)
                if(closemat)then
                  legmat=IDB   ! which legacy index
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
                  CALL ERPCDB(IFMAT,IDB,ITRU,IERR,DBCON,DBDEN,DBSHT,E,A,
     &              DRV,TITL,NAM)
                endif
                write(outs,'(2a)')' Thickness of material: ',NAM
                call edisp (iuout,outs)
              endif
            elseif(IDTYP(I).eq.3) then
              ITEM(1) ='Dry bulb temperature'
              ITEM(2) ='Direct normal solar '
              ITEM(3) ='Diffuse horiz solar '
              ITEM(4) ='Wind speed          '
              ITEM(5) ='Wind direction      '
              ITEM(6) ='Relative humidity   '
              write(outs,'(2a)')' Climate parameter: ',ITEM(IDMAT(I,1))
              call edisp (iuout,outs)
            elseif(IDTYP(I).eq.4) then
              if (IDMAT(I,1).eq.0) then
                if (IDMAT(I,2).eq.1) then
                  write(outs,'(a)')' Scheduled infiltration. '
                elseif (IDMAT(I,2).eq.2) then
                  write(outs,'(a)')' Scheduled ventilation. '
                elseif (IDMAT(I,2).eq.3) then
                  write(outs,'(a)')' Scheduled ventilation source zone.'
                elseif (IDMAT(I,2).eq.4) then
                  write(outs,'(a)')
     &                     ' Scheduled ventilation source temperature.'
                endif
                call edisp (iuout,outs)
              else
                if (IDMAT(I,2).eq.1) then
                  write(outs,'(a,i4)')
     &                       ' Sensible casual gain, type: ',IDMAT(I,1)
                elseif (IDMAT(I,2).eq.2) then
                  write(outs,'(a,i4)')
     &                         ' Latent casual gain, type: ',IDMAT(I,1)
                elseif (IDMAT(I,2).eq.3) then
                  write(outs,'(a,i4)')
     &               ' Radiant fraction casual gain, type: ',IDMAT(I,1)
                elseif (IDMAT(I,2).eq.4) then
                  write(outs,'(a,i4)')
     &            ' Convective fraction casual gain, type: ',IDMAT(I,1)
                elseif (IDMAT(I,2).eq.5) then
                  write(outs,'(a,i4)')
     &                          ' Total casual gain, type: ',IDMAT(I,1)
                endif
                call edisp (iuout,outs)
              endif
            elseif(IDTYP(I).eq.5) then
              write(outs,'(a)')' Convection coefficients '
              call edisp (iuout,outs)

            endif
            call edisp (iuout,' Distribution type:')
            if (IDMAT(I,3).eq.1) then
              call edisp (iuout,' Normal; limits defined as: ')
              write (outs,'(a,f6.2,a)') ' +/- ',DAT(I,1),
     &                                       '% of the database value.'
              call edisp (iuout,outs)
            elseif (IDMAT(I,3).eq.2) then
              call edisp (iuout,' Normal; limits defined as: ')
              write (outs,'(a,f6.2,a)') ' +/- ',DAT(I,1),
     &                                        ' of the database value.'
              call edisp (iuout,outs)
            elseif (IDMAT(I,3).eq.3) then
              call edisp(iuout,' Normal; absolute limits defined as: ')
              write(outs,'(2(a,f6.2))')' From ',DAT(I,1),' to ',DAT(I,2)
              call edisp (iuout,outs)
            elseif (IDMAT(I,3).eq.4) then
              call edisp(iuout,' Alternative correlations: ')
              write(outs,'(2(a,f6.2))')
     &                              ' High ',DAT(I,1),', Low ',DAT(I,2)
              call edisp (iuout,outs)
            endif
          endif
 10     continue
      endif
      if (ISEC.eq.0.or.ISEC.eq.2) then
C List locations.

        call edisp (iuout,'  ')
        write(outs,'(a,i4)')'Number of locations defined:',NILOC
        call edisp (iuout,outs)
        do 20 I=1,NILOC
          if (IELM.eq.0.or.IELM.eq.I) then
            call edisp (iuout,'  ')
            write(outs,'(a,i4,a,a)')'Location :',I,'; ',LLOC(I)
            call edisp (iuout,outs)
            call edisp (iuout,' Zone           | Surface list ')
            do 30 J=1,NZG(I)
              write (outs,'(1x,a,a,25i3)')ZNAME(NZNOG(I,J)),':',
     &                                    (NSNOG(I,J,K),K=1,NSG(I,J))
              call edisp (iuout,outs)
 30         continue
          endif
 20     continue
      endif
      if (ISEC.eq.0.or.ISEC.eq.3) then
C List uncertainties.

        call edisp (iuout,'  ')
        write(outs,'(a,i4)')'Number of uncertainties defined:',NIACT
        call edisp (iuout,outs)
        do 40 I=1,NIACT
          if (IELM.eq.0.or.IELM.eq.I) then
            call edisp (iuout,'  ')
            write(outs,'(a,i4)')'Uncertainty :',I
            call edisp (iuout,outs)
            call edisp (iuout,' Distribution   | Location')
            write(outs,'(1x,3a)')LCNG(IACTD(I,1)),', ',LLOC(IACTD(I,2))
            call edisp (iuout,outs)
          endif
 40     continue
      endif

      return

      END
