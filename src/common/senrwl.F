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
C Common blocks UA1 UA2 UA3 UA4 within uncertainty.h
#include "uncertainty.h"
#include "espriou.h"
C espriou.h provides currentfile.
#include "control.h"
      
      integer lnblnk  ! function definition

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/TRACE/ITCF,ITRACE(MTRACE),IZNTRC(MCOM),ITU

      COMMON/UA2T/NTLOC(MNIL),NTG(MNIL,4)
      COMMON/UA31/MATNAM(MNCNG,2)
      COMMON/UA32/IMISCD(MNCNG,2)
      
      character MATNAM*32
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
        call EGETWI(OUTSTR,K,IDTYPU(II),1,1001,'W','change type',IER)
        if (IDTYPU(II).eq.1) then

C Thermophysical property.
          call STRIPC(IUALF,OUTSTR,2,ND,0,'db ref & prop.',IER)
          if (IER.NE.0) return
          K=0
          call EGETWI(OUTSTR,K,IDMAT(II,1),1,500,'W','material ary',IER)
          call EGETWI(OUTSTR,K,IDMAT(II,2),1,6,'W','property ref.',IER)
          call STRIPC(IUALF,OUTSTR,3,ND,0,'data.',IER)
          if (IER.NE.0) return
          K=0
          call EGETWI(OUTSTR,K,IDMAT(II,3),1,3,'W',
     &      'change type ref.',IER)
          call EGETWR(OUTSTR,K,DATU(II,1),0.,100.,'W','change mag.',IER)
          call EGETWR(OUTSTR,K,DATU(II,2),0.,100.,'W','change mag.',IER)
        elseif (IDTYPU(II).eq.2) then

C material index MLC name & layer position.
          call STRIPC(IUALF,OUTSTR,3,ND,0,'Layer and MLC name.',IER)
          if (IER.NE.0) return
          K=0
          call EGETWI(OUTSTR,K,IDMAT(II,1),1,500,'W',
     &      'Mat index or if zero toggle.',IER)
          if (IDMAT(II,1).eq.0) then

C 2 MLC names.
            call EGETW(OUTSTR,K,MATNAM(II,1),'W','MLC name 1',IFLAG)
            call EGETW(OUTSTR,K,MATNAM(II,2),'W','MLC name 2',IFLAG)
          else

C 1 MLC name and which layer followed by data for thickness change.
            call EGETW(OUTSTR,K,MATNAM(II,1),'W','MLC name 1',IFLAG)
            call EGETWI(OUTSTR,K,IMISCD(II,1),1,12,'W',
     &        'which layer in mlc.',IER)
            call STRIPC(IUALF,OUTSTR,3,ND,0,'data.',IER)
            if (IER.NE.0) return
            K=0
            call EGETWI(OUTSTR,K,IDMAT(II,3),1,3,'W',
     &        'change type ref.',IER)
            call EGETWR(OUTSTR,K,DATU(II,1),0.,100.,'W',
     &        'change mag.',IER)
            call EGETWR(OUTSTR,K,DATU(II,2),0.,100.,'W',
     &        'change mag.',IER)
          endif
        elseif (IDTYPU(II).eq.3) then

C Climate parameters.
          call STRIPC(IUALF,OUTSTR,1,ND,0,'Clm par.',IER)
          if (IER.NE.0) return
          K=0
          call EGETWI(OUTSTR,K,IDMAT(II,1),1,6,'W','Clm var.',IER)
          call STRIPC(IUALF,OUTSTR,3,ND,0,'data.',IER)
          if (IER.NE.0) return
          K=0
          call EGETWI(OUTSTR,K,IDMAT(II,3),1,2,'W',
     &      'climate change type ref.',IER)
          call EGETWR(OUTSTR,K,DATU(II,1),0.,10.,'W','change mag.',IER)
          call EGETWR(OUTSTR,K,DATU(II,2),0.,10.,'W','change mag.',IER)
        elseif (IDTYPU(II).eq.4) then

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
     &      'oper change type ref.',IER)
          call EGETWR(OUTSTR,K,DATU(II,1),0.,100.,'W','change mag.',IER)
          call EGETWR(OUTSTR,K,DATU(II,2),0.,100.,'W','change mag.',IER)
        elseif (IDTYPU(II).eq.5) then

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
            call EGETWI(OUTSTR,K,IDMAT(II,3),4,4,'W','corr ids.',IER)
            call EGETWR(OUTSTR,K,DATU(II,1),1.,8.,'W','high corr.',IER)
            call EGETWR(OUTSTR,K,DATU(II,2),1.,8.,'W','low corr.',IER)
          endif
        elseif (IDTYPU(II).eq.6) then

C Blind setpoints.
          call STRIPC(IUALF,OUTSTR,2,ND,0,'Blind data.',IER)
          if (IER.NE.0) return
          K=0
          call EGETWI(OUTSTR,K,IDMAT(II,1),0,4,'W','Blind.',IER)
          call EGETWI(OUTSTR,K,IDMAT(II,2),1,8,'W','N/A',IER)

C Read associated data.
          call STRIPC(IUALF,OUTSTR,3,ND,0,'blind data.',IER)
          if (IER.NE.0) return
          K=0
          call EGETWI(OUTSTR,K,IDMAT(II,3),1,3,'W',
     &      'blind change type ref.',IER)
          call EGETWR(OUTSTR,K,DATU(II,1),0.,100.,'W','change mag.',IER)
          call EGETWR(OUTSTR,K,DATU(II,2),0.,100.,'W','change mag.',IER)

        elseif (IDTYPU(II).eq.101) then

C Zone level assumptions.
          call STRIPC(IUALF,OUTSTR,2,ND,0,'Zone air or SNA.',IER)
          if (IER.NE.0) return
          K=0
          call EGETWI(OUTSTR,K,IDMAT(II,1),1,2,'W','Air or SNA',IER)
          call EGETWI(OUTSTR,K,IDMAT(II,2),1,1,'W','Property.',IER)
          if (IDMAT(II,1).ne.0) then

C Read associated data.
            call STRIPC(IUALF,OUTSTR,3,ND,0,'data.',IER)
            if (IER.NE.0) return
            K=0
            call EGETWI(OUTSTR,K,IDMAT(II,3),1,4,'W',
     &        'zone air change type ref.',IER)
            call EGETWR(OUTSTR,K,DATU(II,1),0.,100.,'W',
     &        'change mag.',IER)
            call EGETWR(OUTSTR,K,DATU(II,2),0.,100.,'W',
     &        'change mag.',IER)
          endif
        elseif (IDTYPU(II).eq.1001) then

C Control uncertainties.
          call STRIPC(IUALF,OUTSTR,4,ND,0,'Control.',IER)
          if (IER.NE.0) return
          K=0
          call EGETWI(OUTSTR,K,IDMAT(II,1),1,mcf,'W','Function.',IER)
          call EGETWI(OUTSTR,K,IDMAT(II,2),1,MCDT,'W','Day type.',IER)
          call EGETWI(OUTSTR,K,IMISCD(II,1),1,MCDP,'W','Period.',IER)
          call EGETWI(OUTSTR,K,IMISCD(II,2),1,7,'W',
     &      'Ctl parameter.',IER)

C Read associated data.
          call STRIPC(IUALF,OUTSTR,3,ND,0,'data.',IER)
          if (IER.NE.0) return
          K=0
          call EGETWI(OUTSTR,K,IDMAT(II,3),1,4,'W',
     &      'ctl change type ref.',IER)
          call EGETWR(OUTSTR,K,DATU(II,1),0.,100.,'W','change mag.',IER)
          call EGETWR(OUTSTR,K,DATU(II,2),0.,100.,'W','change mag.',IER)
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
        call EGETWI(OUTSTR,K,NZGU(II),0,MCOM,'-','no of zones',IER)

C If NZGU(II)<=1 then time based location.
        if (NZGU(II).lt.1) then

C Set NZGU to positive value.
          NTLOC(II)=-1
          NZGU(II)=NZGU(II)*NTLOC(II)

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
        if (NZGU(II).gt.0) then

C For each zone read the zone id number and then the number of surfaces 
C included in the definition.
          do 201 IJ=1,NZGU(II)
            call STRIPC(IUALF,OUTSTR,2,ND,
     &        0,'zones no and surfaces and tmc.',IER)
            if (IER.NE.0) return
            K=0
            call EGETWI(OUTSTR,K,NZNOGU(II,IJ),
     &        1,MCOM,'W','zones no.',IER)
            call EGETWI(OUTSTR,K,NSGU(II,IJ),
     &        0,MS,'W','no of surfaces.',IER)

C Read surface list.
            call STRIPC(IUALF,OUTSTR,NSGU(II,IJ),
     &        ND,0,'surface list.',IER)
            if (IER.NE.0) return

            if(NSGU(II,IJ).eq.0)then
C If there were no surfaces there might be TMC.
              K=0
              call EGETWI(OUTSTR,K,IV1,1,MS,'W','no of tmc.',IER)
              call EGETWI(OUTSTR,K,IV2,1,MS,'W','tmc. period',IER)
              NTGU(II,IJ,1)=IV1
              NTGU(II,IJ,2)=IV2
C              write(6,*) 'READUAL ntgu',II,IJ,IV1,IV2
            else
              K=0
              do 202 IK=1,NSGU(II,IJ)
                call EGETWI(OUTSTR,K,IV3,1,MCOM,'W','surface no.',IER)
                NSNOG(II,IJ,IK)=IV3
 202          continue
            endif
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
C Common blocks UA1 UA2 UA3 UA4 within uncertainty.h
#include "uncertainty.h"
#include "esprdbfile.h"
#include "material.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/UA2T/NTLOC(MNIL),NTG(MNIL,4)
      COMMON/UA31/MATNAM(MNCNG,2)
      COMMON/UA32/IMISCD(MNCNG,2)

      character outs*124,datmsg*42,datmsg2*42,chgmsg*42,MATNAM*32

      IER=0

C Open existing file.
      call EFOPSEQ(IUALF,LUALF,4,IER)
      if (IER.LT.0) then
        IER=1
        return
      endif

      write (IUALF,'(a)',IOSTAT=ISTAT,ERR=1003)
     &  '*Uncertainty analysis library'

      write (IUALF,'(3i4,a)',IOSTAT=ISTAT,ERR=1003)
     &  NICNG,NILOC,NIACT,' # Changes, Locations, Actions'

C For each change write info.
      write (IUALF,'(a)',IOSTAT=ISTAT,ERR=1003) '#  '
      write (IUALF,'(a)',IOSTAT=ISTAT,ERR=1003)
     &  '# Changes definitions follow...'
      write (IUALF,'(a)',IOSTAT=ISTAT,ERR=1003) '#  '
      do 1000, II=1,NICNG
        write (IUALF,'(a)',IOSTAT=ISTAT,ERR=1003) '*cng_def'
        write (IUALF,'(2a)',IOSTAT=ISTAT,ERR=1003)LCNG(II),
     &    ' # Change id string'

C Initial strings.
        write (outs,'(a)') ' # Change type: unknown '
        write (datmsg,'(a)') ' # Data type: unknown '
        if(IDMAT(II,3).eq.1) chgmsg=' # % change'
        if(IDMAT(II,3).eq.2) chgmsg=' # abs change'
        if(IDMAT(II,3).eq.3) chgmsg=' # explicit change'
        if (IDTYPU(II).eq.1) then
          write (outs,'(a)') ' # Change type: Materials'
          write (IUALF,'(i4,a)',IOSTAT=ISTAT,ERR=1003)IDTYPU(II),
     &      outs(1:lnblnk(outs))
          write(IUALF,'(2i4,a)',IOSTAT=ISTAT,ERR=1003)
     &      IDMAT(II,1),IDMAT(II,2),' # material & property id'
        elseif (IDTYPU(II).eq.2) then

C Layer thickness decode attributes including material name
          write (outs,'(a)') ' # Change type: Constructions'
          write (IUALF,'(i4,a)',IOSTAT=ISTAT,ERR=1003)IDTYPU(II),
     &      outs(1:lnblnk(outs))
          datmsg=' layer thickness'
          write(datmsg2,'(2a)') ' ',matname(IDMAT(II,1))
          write(IUALF,'(i4,2a,i3,a)',IOSTAT=ISTAT,ERR=1003)IDMAT(II,1),
     &      ' ',MATNAM(II,1)(1:lnblnk(MATNAM(II,1))),IMISCD(II,1),
     &      '    # material index & construction name & which layer'
        elseif (IDTYPU(II).eq.3) then
          write (outs,'(a)') ' # Change type: Weather parameter'
          write (IUALF,'(i4,a)',IOSTAT=ISTAT,ERR=1003)IDTYPU(II),
     &      outs(1:lnblnk(outs))
          if(IDMAT(II,1).eq.1) datmsg=' Dry bulb T'
          if(IDMAT(II,1).eq.2) datmsg=' Direct normal sol'
          if(IDMAT(II,1).eq.3) datmsg=' Diffuse horiz sol'
          if(IDMAT(II,1).eq.4) datmsg=' Wind speed'
          if(IDMAT(II,1).eq.5) datmsg=' Wind direction'
          if(IDMAT(II,1).eq.6) datmsg=' Relative humidity'
          datmsg2=' -'
          write (IUALF,'(i4,2a)',IOSTAT=ISTAT,ERR=1003)IDMAT(II,1),
     &      ' # Weather parameter',datmsg
        elseif (IDTYPU(II).eq.4) then
          write (outs,'(a)') ' # Change type: Scheduled operation'
          write (IUALF,'(i4,a)',IOSTAT=ISTAT,ERR=1003)IDTYPU(II),
     &      outs(1:lnblnk(outs))
         if(IDMAT(II,1).eq.0)then
            if(IDMAT(II,2).eq.1) datmsg=' infiltration'
            if(IDMAT(II,2).eq.2) datmsg=' ventilation'
            datmsg2=' -'
          else
            if(IDMAT(II,1).eq.1) datmsg=' occupants'
            if(IDMAT(II,1).eq.2) datmsg=' lights'
            if(IDMAT(II,1).eq.3) datmsg=' equipment'
            if(IDMAT(II,2).eq.1) datmsg2=' sensible'
            if(IDMAT(II,2).eq.2) datmsg2=' latent'
            if(IDMAT(II,2).eq.3) datmsg2=' radiant frac'
            if(IDMAT(II,2).eq.4) datmsg2=' convective frac'
            if(IDMAT(II,2).eq.5) datmsg2=' total Q'
            if(IDMAT(II,2).eq.6) datmsg2=' period start'
            if(IDMAT(II,2).eq.7) datmsg2=' RC split'
          endif
          write (IUALF,'(2i4,3a)',IOSTAT=ISTAT,ERR=1003)IDMAT(II,1),
     &      IDMAT(II,2),' # Operations parameter',
     &      datmsg(1:lnblnk(datmsg)),datmsg2(1:lnblnk(datmsg2))
        elseif (IDTYPU(II).eq.5) then
          write (outs,'(a)') ' # Change type: Convection coefficients'
          write (IUALF,'(i4,a)',IOSTAT=ISTAT,ERR=1003)IDTYPU(II),
     &      outs(1:lnblnk(outs))
          write (IUALF,'(2i4,a)',IOSTAT=ISTAT,ERR=1003)IDMAT(II,1),
     &      IDMAT(II,2),' # Heat transfer ref'
          datmsg=' Halcrow time invarient'
          datmsg2=' -'
        elseif (IDTYPU(II).eq.6) then
          write (outs,'(a)') ' # Change type: optical properties'
          write (IUALF,'(i4,a)',IOSTAT=ISTAT,ERR=1003)IDTYPU(II),
     &      outs(1:lnblnk(outs))
          write (IUALF,'(2i4,a)',IOSTAT=ISTAT,ERR=1003)IDMAT(II,1),
     &      IDMAT(II,2),' # setpoint '
          datmsg=' blind setpoint'
          datmsg2=' -'
        elseif (IDTYPU(II).eq.101) then
          write (outs,'(a)') ' # Change type: Zone volume or area'
          write (IUALF,'(i4,a)',IOSTAT=ISTAT,ERR=1003)IDTYPU(II),
     &      outs(1:lnblnk(outs))
          if(IDMAT(II,1).eq.1) datmsg=' zone air volume'
          if(IDMAT(II,1).eq.2) datmsg=' surface area'
          datmsg2=' -'
          write (IUALF,'(2i4,2a)',IOSTAT=ISTAT,ERR=1003)IDMAT(II,1),
     &      IDMAT(II,2),' # Zone parameter ',datmsg(1:lnblnk(datmsg))
        elseif (IDTYPU(II).eq.1001) then
          write (outs,'(a)') ' # Change type: Zone control'
          write (IUALF,'(i4,a)',IOSTAT=ISTAT,ERR=1003)IDTYPU(II),
     &      outs(1:lnblnk(outs))
          if(IDMAT(II,1).gt.0) datmsg=' control loop'
          if(IMISCD(II,2).eq.1)  datmsg2=' period Start'
          if(IMISCD(II,2).eq.2)  datmsg2=' max heat'
          if(IMISCD(II,2).eq.3)  datmsg2=' min heat'
          if(IMISCD(II,2).eq.4)  datmsg2=' max cool'
          if(IMISCD(II,2).eq.5)  datmsg2=' min cool'
          if(IMISCD(II,2).eq.6)  datmsg2=' heating SP'
          if(IMISCD(II,2).eq.7)  datmsg2=' cooling SP'
          write (IUALF,'(4i4,2a)',IOSTAT=ISTAT,ERR=1003)IDMAT(II,1),
     &      IDMAT(II,2),IMISCD(II,1),IMISCD(II,2),
     &      ' # Control parameters ',datmsg2(1:lnblnk(datmsg2))
        else
          write (outs,'(a)') ' # Change type: unknown'
          write (IUALF,'(i4,a)',IOSTAT=ISTAT,ERR=1003)IDTYPU(II),
     &      outs(1:lnblnk(outs))
          write (IUALF,'(2i4,a)',IOSTAT=ISTAT,ERR=1003)
     &      IDMAT(II,1),IDMAT(II,2),' # ref 1, ref 2'
          datmsg=' other'; datmsg2=' -'
        endif

C << can we be more specific in comment? >>
C        write(6,*) 'datmsg is ',datmsg(1:lnblnk(datmsg))
C        write(6,*) 'datmsg2 is ',datmsg2(1:lnblnk(datmsg2))

        if (IDTYPU(II).eq.1001.or.IDTYPU(II).eq.2) then
          write (IUALF,50,IOSTAT=ISTAT,ERR=1003)IDMAT(II,3),
     &      DATU(II,1),DATU(II,2),chgmsg(1:lnblnk(chgmsg)),
     &      datmsg(1:lnblnk(datmsg)),datmsg2(1:lnblnk(datmsg2))
  50      FORMAT(i4,2f10.3,'  ',3a)
        else
          write (IUALF,51,IOSTAT=ISTAT,ERR=1003)IDMAT(II,3),
     &      DATU(II,1),DATU(II,2),chgmsg(1:lnblnk(chgmsg)),
     &      datmsg(1:lnblnk(datmsg))
  51      FORMAT(i4,2f10.3,'  ',2a)
        endif
 1000 continue

C For each location write info.
      write (IUALF,'(a)',IOSTAT=ISTAT,ERR=1003) '#  '
      write (IUALF,'(a)',IOSTAT=ISTAT,ERR=1003)
     &  '# Locations definitions follow...'
      write (IUALF,'(a)',IOSTAT=ISTAT,ERR=1003) '#  '
      do 2000, II=1,NILOC
        write (IUALF,'(a)',IOSTAT=ISTAT,ERR=1003) '*loc_def'
        write (IUALF,'(2a)',IOSTAT=ISTAT,ERR=1003)LLOC(II),
     &    ' # Location id string'
        if (NTLOC(II).eq.0) then
          write (IUALF,'(i4,a)',IOSTAT=ISTAT,ERR=1003)NZGU(II),
     &      ' # Number of zones'
        else
          write (IUALF,'(i4,a)',IOSTAT=ISTAT,ERR=1003)
     &      NZGU(II)*NTLOC(II),' # Number of zones'
        endif

C If NTLOC=-1 then write time information.
        if (NTLOC(II).eq.-1) then
          write (IUALF,'(4i4,a)',IOSTAT=ISTAT,ERR=1003)
     &      (NTG(II,IJ),IJ=1,4),' # Start day, hour, finish day, hour'
        endif

C Data for each zone.
        do 2100, IJ=1,NZGU(II)
          write (IUALF,'(2i4,a)',IOSTAT=ISTAT,ERR=1003)
     &      NZNOGU(II,IJ),NSGU(II,IJ),
     &      ' # Zone number Number of surfaces'
          if(NSGU(II,IJ).gt.0)then
            write (IUALF,70,IOSTAT=ISTAT,ERR=1003)
     &        (NSNOG(II,IJ,IK),IK=1,NSGU(II,IJ))
  70        FORMAT(25i3,' # Surfaces')
          else
            write (IUALF,'(2i4,a)',IOSTAT=ISTAT,ERR=1003)
     &        NTGU(II,IJ,1),NTGU(II,IJ,2),' # TMC type & period'
          endif
C          write (IUALF,'(25i3,a)',IOSTAT=ISTAT,ERR=1003)
C     &      (NSNOG(II,IJ,IK),IK=1,NSGU(II,IJ)),' # Surfaces'
 2100   continue
 2000 continue

C For each actions write info.
      write (IUALF,'(a)',IOSTAT=ISTAT,ERR=1003) '#  '
      write (IUALF,'(a)',IOSTAT=ISTAT,ERR=1003)
     &  '# Action definitions follow...'
      write (IUALF,'(a)',IOSTAT=ISTAT,ERR=1003) '#  '
      do 3000, II=1,NIACT
        write (IUALF,'(a)',IOSTAT=ISTAT,ERR=1003) '*act_def'
        write (IUALF,'(i4,2a)',IOSTAT=ISTAT,ERR=1003)
     &    IACTD(II,1),' # Uncertanty ref: ',LCNG(IACTD(II,1))
        write (IUALF,'(i4,2a)',IOSTAT=ISTAT,ERR=1003)
     &    IACTD(II,2),' # Location ref: ',LLOC(IACTD(II,2))
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
C Common blocks UA1 UA2 UA3 UA4 within uncertainty.h
#include "uncertainty.h"
#include "esprdbfile.h"
#include "material.h"

      COMMON/OUTIN/IUOUT,IUIN

      COMMON/UA31/MATNAM(MNCNG,2)
      COMMON/UA32/IMISCD(MNCNG,2)

      logical closemat1,closemat2

      integer matarrayindex ! the indes within matdatarray

      character outs*124
      character NAM*72,MATNAM*32,ITEM(6)*20

      IER=0

C Find version of materials database.
      call eclose(matver,1.1,0.01,closemat1)
      call eclose(matver,1.2,0.01,closemat2)
      if(closemat1.or.closemat2)then
        continue
      else
        call usrmsg('The materials arrays are incomplete so un-',
     &    'certainty attributes not possible.','W')
        return
      endif

      call edisp (iuout,'  ')
      write(outs,'(2a)')'Uncertainty analysis library: ',LUALF(1:40)
      call edisp (iuout,outs)

      if (ISEC.eq.0.or.ISEC.eq.1) then
C List distributions.

        call edisp (iuout,'  ')
        write(outs,'(a,i3)')'Number of distributions defined:',NICNG
        call edisp (iuout,outs)
        do 10 I=1,NICNG  ! for each distribution
          if (IELM.eq.0.or.IELM.eq.I) then
            call edisp (iuout,'  ')
            write(outs,'(a,i4,a,a)')'Distribution :',I,'; ',LCNG(I)
            call edisp (iuout,outs)
            if (IDTYPU(I).eq.1) then  ! if materials focus
              IDB=IDMAT(I,1)
              if(IDB.gt.0)then
                matarrayindex=IDB  ! find matching array index
 
C And if matarrayindex is zero then resetn dbcon dbden dbsht
                if(matarrayindex.eq.0)then
                  NAM='AIR'
                else
                  write(NAM,'(a)') matname(matarrayindex)(1:32)
                endif
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
            elseif(IDTYPU(I).eq.2) then  ! if MLC focus
              write(outs,'(2a)')' Composite construction: ',MATNAM(I,1)
              call edisp (iuout,outs)
              if (IDMAT(I,1).eq.0) then
                call edisp (iuout,'Undefined')
              else
                IDB=IDMAT(I,1)
                if(IDB.gt.0)then
                  matarrayindex=IDB  ! find matching array index
 
C And if matarrayindex is zero then resetn dbcon dbden dbsht
                  if(matarrayindex.eq.0)then
                    NAM='AIR'
                  else
                    write(NAM,'(a)') matname(matarrayindex)(1:32)
                  endif
                endif
                write(outs,'(3a,i3)')' Thickness of material: ',NAM,
     &            ' @ layer ',IMISCD(I,1)
                call edisp (iuout,outs)
              endif
            elseif(IDTYPU(I).eq.3) then  ! if weather focus
              ITEM(1) ='Dry bulb temperature'
              ITEM(2) ='Direct normal solar '
              ITEM(3) ='Diffuse horiz solar '
              ITEM(4) ='Wind speed          '
              ITEM(5) ='Wind direction      '
              ITEM(6) ='Relative humidity   '
              write(outs,'(2a)')' Climate parameter: ',ITEM(IDMAT(I,1))
              call edisp (iuout,outs)
            elseif(IDTYPU(I).eq.4) then  ! if operation focus
              if (IDMAT(I,1).eq.0) then
                if (IDMAT(I,2).eq.1) then
                  write(outs,'(a)')' Scheduled infiltration. '
                elseif (IDMAT(I,2).eq.2) then
                  write(outs,'(a)')' Scheduled ventilation. '
                elseif (IDMAT(I,2).eq.3) then
                  write(outs,'(a)')' Scheduled ventilation source zone.'
                elseif (IDMAT(I,2).eq.4) then
                  write(outs,'(a)')
     &              ' Scheduled ventilation source temperature.'
                endif
                call edisp (iuout,outs)
              else
                if (IDMAT(I,2).eq.1) then
                  write(outs,'(a,i4)')
     &              ' Sensible casual gain, type: ',IDMAT(I,1)
                elseif (IDMAT(I,2).eq.2) then
                  write(outs,'(a,i4)')
     &              ' Latent casual gain, type: ',IDMAT(I,1)
                elseif (IDMAT(I,2).eq.3) then
                  write(outs,'(a,i4)')
     &              ' Radiant fraction casual gain, type: ',IDMAT(I,1)
                elseif (IDMAT(I,2).eq.4) then
                  write(outs,'(a,i4)')
     &            ' Convective fraction casual gain, type: ',IDMAT(I,1)
                elseif (IDMAT(I,2).eq.5) then
                  write(outs,'(a,i4)')
     &              ' Total casual gain, type: ',IDMAT(I,1)
                elseif (IDMAT(I,2).eq.6) then
                  write(outs,'(a,i4)')
     &              ' Period start hour, type: ',IDMAT(I,1)
                elseif (IDMAT(I,2).eq.8) then
                  write(outs,'(a,i4)')
     &              ' RC split, type: ',IDMAT(I,1)
                endif
                call edisp (iuout,outs)
              endif
            elseif(IDTYPU(I).eq.5) then  ! if hc coefficients
              write(outs,'(a)')' Convection coefficients '
              call edisp (iuout,outs)
            elseif(IDTYPU(I).eq.6) then  ! if optical properties
              write(outs,'(a)')' Optical properties '
              call edisp (iuout,outs)
            elseif(IDTYPU(I).eq.101) then  ! if geometry focus
              write(outs,'(a)')' Zone air volume '
              call edisp (iuout,outs)
            endif
            call edisp (iuout,' Distribution type:')
            if (IDMAT(I,3).eq.1) then
              write (outs,'(a,f6.2,a)') 
     &         ' Distribution type: Normal; limits +/- ',
     &         DATU(I,1),'% of the database value.'
              call edisp (iuout,outs)
            elseif (IDMAT(I,3).eq.2) then
              write (outs,'(a,f6.2,a)') 
     &         ' Distribution type: Normal; limits +/- ',
     &         DATU(I,1),'of the database value.'
              call edisp (iuout,outs)
            elseif (IDMAT(I,3).eq.3) then
              write (outs,'(2a,f6.2,a,f6.2)') 
     &         ' Distribution type: Normal; absolute limits:',
     &         ' From ',DATU(I,1),' to ',DATU(I,2)
              call edisp (iuout,outs)
            elseif (IDMAT(I,3).eq.4) then
              write (outs,'(2a,f6.2,a,f6.2)') 
     &         ' Distribution type: Alternative correlations: ',
     &          ' High ',DATU(I,1),', Low ',DATU(I,2)
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
            call edisp (iuout,' Zone           | Surface or TMC list ')
            do 30 J=1,NZGU(I)
              if(NSGU(I,J).gt.0)then
                write (outs,'(1x,a,a,25i3)')ZNAME(NZNOGU(I,J)),':',
     &            (NSNOG(I,J,K),K=1,NSGU(I,J))
              else
                write (outs,'(1x,a,a,2i3,a)')ZNAME(NZNOGU(I,J)),':',
     &            NTGU(I,J,1),NTGU(I,J,2),' tmc type & period'
              endif
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
