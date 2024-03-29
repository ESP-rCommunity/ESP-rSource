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


C This file contains the following routines.
C SHDCON: Sets up the parameters relating to shading computation.
C FINDISC: Determines the values in array isc based on information in
C          the zone geometry file and or user interactions.
C SHADC:  Implements the shading computation.
C DECOMP: Generates a modified shadow polygon for the case where the
C         projected surface crosses the plane of the target surface.
C INTERSECT: Determines the intersection point between the YT=0
C            plane and a line.
C SHDTST: Tests each grid centre point of a user-specified grid to
C         establish if it is within the target surface and shadow
C         polygon.

C ******************** SHDCON ********************
C Sets up the parameters relating to shading computation.

      subroutine shdcon(icomp,ier,act)
#include "building.h"
#include "geometry.h"
#include "help.h"
      
      integer lnblnk  ! function definition

      common/filep/ifil
      common/outin/iuout,iuin,ieout

      integer mon,isc,iyd
      common/contr/mon,isc(ms),iyd
      character ltrns*72
      integer multic,mons,monf
      common/mtfile/ltrns,multic,mons,monf
      common/ract/paction
      common/shad0/ISIcalc,icalcD,icalcM

      dimension irecs(ms),iavdec(12)
      character tbase*72,outs*124
      character*3 month(12)
      character act*1,paction*16
      logical ok
      logical newgeo  ! used to test for new/old geometry file
      integer ier

C Day of average solar declination in each month.
      data iavdec/17,15,16,15,15,11,17,16,16,16,15,11/
      data month/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &            'Sep','Oct','Nov','Dec'/

      helpinsub='shdcon'  ! set for subroutine
      newgeo=.false.  ! Assume older format geometry.
      iunit=ifil
   35 call eclose(gversion(icomp),1.1,0.01,newgeo)

C Make up help text for the following interactions.
      helptopic='shdcon_actions'
      call gethelptext(helpinsub,helptopic,nbhelp)
   
C Recover the isc and associated arrays.
      call findisc(icomp,ier,act,icont)

C Check that at least one surface has been chosen or gleaned
C from the zone geometry file.
      if(icont.eq.0)then
         if(newgeo)then
            call edisp(iuout,
     &      'No shading directives in zone geometry file, skipping!')
            return
         else
            if(act.eq.'s')then
              call edisp(iuout,
     &        'No shading directives in zone geometry file, skipping!')
              return
            else
               call easkok(
     &           'No shading directives in zone geometry file!',
     &           'Re-specify?',ok,nbhelp)
               if(ok)goto 35
               call edisp(iuout,'Shading computation abandoned!')
               return
            endif
         endif
      endif

C Calculate shading for specified period.
      multic=2
      if(paction(1:13).ne.'update_silent')then
         call edisp(iuout,' ')
         call edisp(iuout,
     &   'A zone transitional shading file will be opened for each')
         call edisp(iuout,
     &   'computation month to store surface shading data that')
         call edisp(iuout,
     &   'are required by the internal surface insolation algorithm.')
         call edisp(iuout,
     &   'These file are named ?Jan, ?Feb etc., where `?` is the')
         call edisp(iuout,
     &   'zone base name. These files will be automatically deleted')
         call edisp(iuout,
     &   'when the Shading & Insolation program is exited normally')
         call edisp(iuout,
     &   '(otherwise they may be deleted manually).')
      endif
      if(ISIcalc.eq.1)then  ! embedded S/I calculation case
         mons=icalcM
         monf=icalcM
      elseif(ISIcalc.eq.2)then
        if(act.eq.'s')then
          mons=1
          monf=12
          mon=mons
        else
          if(mons.gt.12) mons=1  ! if mons was clobbered reset
          call easki(mons,' ','Start month number?',
     &      1,'F',12,'F',1,'start month',ier,nbhelp)
          if(ier.ne.0)return
          if(monf.gt.12) monf=12  ! if monf was clobbered reset
          call easki(monf,' ','End month number?',
     &      mons,'F',12,'F',1,'end month',ier,nbhelp)
          if(ier.ne.0)return
          if(mons.eq.monf)multic=0
        endif
      endif

      do 601 mon=mons,monf

C For S/I file case, convert day of average solar declination
C in month to a year day number. If ish called from bps, use
C passed day and month.
      iday=iavdec(mon)
      imon=mon
      if(ISIcalc.eq.1)iday=icalcD
      call eday(iday,imon,iyd)

C As a separate file will be created for each month of a 
C simulation, append 'Jan' or 'Feb' to the end of the base name to
C identify them. Close any previous file on iunit and open a new one.
      tbase=ltrns(1:lnblnk(ltrns))//month(mon)
      irecw=ms+5
      ier=0
      call efopran(iunit,tbase,irecw,3,ier)

C Write the first record of the zone transitional shading file.
      irec=1
      do 801 i=1,ms
         irecs(i)=0
  801 continue
      write(iunit,rec=irec,iostat=istat,err=1000)(irecs(i),i=1,ms)
      call tfile1(icomp)
      call shadc(icomp)
      call ssave(icomp,ier)
      if(ier.ne.0)then
         call usrmsg('Error condition detected during file save!',
     &               'Terminating calculation.','W')
         return
      endif
  601 continue
      return

 1000 write(outs,21)irec
   21 format('SHDCON: Zone Transitional Shading file error, record',i6)
      call edisp(iuout,outs)
      return
      end


C ******************** findisc ********************
C findisc determines the values in array isc based on information in
C the zone geometry file and or user interactions.
C   icomp - the zone index.
C   act   - 's' for silent mode, '-' for user interaction mode.
c   icont - >0 if shaded surfaces were found.

      subroutine findisc(icomp,ier,act,icont)
#include "building.h"
#include "geometry.h"

      common/outin/iuout,iuin,ieout
      integer izstocn
      common/c24/izstocn(mcom,ms)

      integer mon,isc,iyd
      common/contr/mon,isc(ms),iyd
      common/ract/paction

C iaplic(1) toggle for shading;
C iaplic(2) toggle for insolation;
C           where 1 means consider all applicable surfaces and 0
C           means manual selection of surfaces.
C nsurfcalc is number of shaded surfaces.
C lstsfcalc list of applicable surfaces.
C nsurfinso number of insolation sources.
C isurfinso list of insolation sources.
      common/ishdirec/iaplic(mcom,2),nsurfcalc(mcom),lstsfcalc(mcom,ms),
     &       nsurfinso(mcom),isurfinso(mcom,ms)

      dimension iva(ms)
      character act*1,paction*16
      logical newgeo  ! Used to test for new/old geometry file.

      newgeo=.false.  ! Assume older format geometry.
      ier=0

      call eclose(gversion(icomp),1.1,0.01,newgeo)

C If new format geometry and iaplic(icomp,1) equals 1 then
C all applicable surfaces were set in the zone geometry file.
      if(newgeo)then
         if(iaplic(icomp,1).eq.1)then
            np=nsurfcalc(icomp)
            do 41 j=1,np
               iva(j)=lstsfcalc(icomp,j)
   41       continue

C Set up isc array (surfaces included in the shading analysis).
            icont=0
            do 42 i=1,nzsur(icomp)
               isc(i)=0
               do 43 j=1,np
                 if(i.eq.iva(j))isc(i)=1
   43          continue
               if(isc(i).eq.1)icont=1
   42       continue

C If iaplic(icomp,1) is 0 and nsurfcalc is non-zero then user
C has manually selected surfaces so fill arrays. If both 0 then
C the user effectively said no shading should be calculated
C so just clear isc() array.
         elseif(iaplic(icomp,1).eq.0)then
            np=nsurfcalc(icomp)
            if(np.eq.0)then
               do 44 i=1,nzsur(icomp)
                 isc(i)=0
   44          continue
            else
               do 45 j=1,np
                  iva(j)=lstsfcalc(icomp,j)
   45          continue

C Set up isc array (surfaces included in the shading analysis).
               icont=0
               do 46 i=1,nzsur(icomp)
                  isc(i)=0
                  do 47 j=1,np
                     if(i.eq.iva(j))isc(i)=1
   47             continue
                  if(isc(i).EQ.1)icont=1
   46          continue
            endif
         endif
      else

C Older geometry file, user is required to set preferences for 
C surfaces to include or, in the case of silent action, just
C do 'all appliable' surfaces.
         if(act.eq.'s'.or.paction(1:11).eq.'recalculate')then

C Clear return array and select all applicable surfaces.
            do 14 i=1,nzsur(icomp)
               iva(i)=0
   14       continue
            np=0
            do 13 i=1,nzsur(icomp)
               icn=izstocn(icomp,i)
               if(sother(icomp,i,1)(1:5).eq.'EXTER')then
                  np=np+1
                  iva(np)=i
               endif
   13       continue
            if(np.eq.0)then
               call usrmsg('No enternal surfaces found!',' ','W')
               return
            endif
            iaplic(icomp,1)=1
         else
            call edisp(iuout,' ')
            call edisp(iuout,'Select external surfaces to include in')
            call edisp(iuout,'the direct/duffuse shading calculation.')
            call pickssur(icomp,np,'s',iva,ier)
            if(ier.ne.0)return
         endif

C Set up isc array (surfaces included in the shading analysis).
         icont=0
         do 110 i=1,nzsur(icomp)
            isc(i)=0
            do 120 j=1,np
               if(i.eq.iva(j))isc(i)=1
  120       continue
            if(isc(i).eq.1)icont=1
  110    continue

C Fill ishdirec array.
         nsurfcalc(icomp)=np
         do 121 j=1,np
            lstsfcalc(icomp,j)=iva(j)
  121    continue
      endif

      return

      end

C ******************** SHADC ********************
C Calculates the direct and diffuse solar shading. Called from shdcon.

      subroutine shadc(icomp)
#include "building.h"
#include "geometry.h"

      common/tracech/icout
      common/prec8/slat,slon
      common/shad1/isunup
      common/shad3/ioshd(mox,moz),foshd(mox,moz),gssa,ipexcl
      common/shad4/oshd(mox,moz),ihit(mox,moz)
      common/grid11/xop(mox,moz),zop(mox,moz)
      integer mon,isc,iyd
      common/contr/mon,isc(ms),iyd
      common/ihtime/ihour
      common/sangl1/sazi1,salt1
      common/sangl2/sazi2,salt2
      common/ract/paction

C ical = 0 indicates that no insolation calculations have been
C performed and is set to 1 when they have.
      common/data1/ical,idifc,init

      dimension month(12),npz(8),alt(8),azi(8)
      character outs*124,paction*16
      character month*3,mode*3
      logical tok
      real xxop,zzop  ! to pass to point1

      data month/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &           'Sep','Oct','Nov','Dec'/

C Sky vault discretisation.
      data npz/30,60,84,108,126,138,144,145/
      data alt/6.,18.,30.,42.,54.,66.,78.,90./
      data azi/12.,12.,15.,15.,20.,30.,60.,0./

C If trace requested enable writing.
      tok=.false.
      if(icout.eq.33)tok=.true.

      if(nbobs(icomp).eq.0)then
         call usrmsg('No obstruction blocks defined so',
     &               'exiting from shading calculation.','W')
         return
      endif

      ical=0
      indcom=0
      kind=0
      knt=0
      ksu=0

C ksu is the number of sun-up hours
      do 10 i=1,24
         stime=float(i)
         call eazalts(stime,iyd,slat,slon,isunup,sazi,salt)
         if(isunup.eq.1)ksu=ksu+1
   10 continue

C knt is the total number of target surfaces in zone.
      do 11 i=1,nzsur(icomp)
         if(isc(i).eq.1)knt=knt+1
   11 continue

C Estimate simulaton extent (surface-hours) to support the
C progress report.         
      kt=ksu*knt

C Commence direct (mode=dir) and diffuse (mode=dif) shading computation
C for each target surface in turn.
      do 20 itsur=1,nzsur(icomp)
         init=0

C Skip if surface is not selected by user.
         if(isc(itsur).eq.0)goto 20
         if(paction(1:13).ne.'update_silent')then
            call usrmsg(' ',' ','-')
         endif

C Trace output.
         if(tok)then
            call edisp(icout,' ')
            write(outs,9998)itsur
 9998       format('*** SHADC: processing target surface ',I2)
            call edisp(icout,outs)
         endif

C Partially derive transformation equations by computing time
C independent part of equation coefficients - equivalent to
C translation plus three-dimensional plane rotation.
         call trnsf1(icomp,itsur)

C Transfer transformed surface coordinates to zone transitional
C shading file.
         call tfile2(icomp,itsur)

C Set up target surface mesh.
         call mesh(icomp,itsur)

C Proceed hour-by-hour.
         do 30 ihr=1,24

C Reset foshd each hour, setting any grid point not within surface
C to -1.0.
            do 401 i=1,nox(icomp)
               do 402 j=1,noz(icomp)
                  xxop=xop(i,j); zzop=zop(i,j)  ! to pass to point1
                  call point1(icomp,itsur,xxop,zzop,i,j,ians)
                  foshd(i,j)=0
                  if(ians.eq.0)foshd(i,j)=-1.0
  402          continue
  401       continue

            ihour=ihr
            stime=float(ihour)

C Determine the solar azimuth and altitude angles relative to the
C original coordinate system.
            call eazalts(stime,iyd,slat,slon,isunup,sazi1,salt1)

C Remember sun position for this hour.
            sazir=sazi1
            saltr=salt1

C Abandon calculation for this hour if before sunrise or after sunset.
            if(isunup.eq.0)then
               mode='abn'
               call tfile3(icomp,itsur,mode)

C Trace output.
               if(tok)then
                  call edisp(icout,' ')
                  write(outs,9997)ihour
 9997             format('*** SHADC: Sun not up at hour ',I2)
                  call edisp(icout,outs)
               endif
               goto 30
            endif

C Initialise ipexcl, the number of sky patched 'behind' the
C target surface.
            ipexcl=0

C Compute direct (ip=1) then diffuse (ip=2,146, i.e. 145 sky
C patches) shading. Where the latter is not time dependent
C (e.g. geometry is time invariant and sky irradiance distribution
C is handled by bps), initiate only for the first sun-up time
C step. gssa, the overall shading on target surface, is initialised
C here for use in shdtst.
            do 40 ip=1,146

C Reset ioshd() and oshd() before each sun or sky patch loop, setting
C ioshd() for any grid point not within target surface to -1.
              do 403 i=1,nox(icomp)
                 do 404 j=1,noz(icomp)
                    xxop=xop(i,j); zzop=zop(i,j)  ! to pass to point1
                    call point1(icomp,itsur,xxop,zzop,i,j,ians)
                    oshd(i,j)=0.0
                    ioshd(i,j)=0
                    if(ians.eq.0)ioshd(i,j)=-1
  404            continue
  403         continue

C Direct case.
               if(ip.eq.1)then
                  mode='dir'
                  gssa=0.0
C Trace output.
                  if(tok)then
                     call edisp(icout,' ')
                     write(outs,'(a,i2)')
     &               '*** SHADC: calculating direct shading at hour ',
     &               ihour
                     call edisp(icout,outs)
                  endif

C Diffuse case (only initilise gssa at start of sky vault processing).
               else
                  mode='dif'
                  if(ip.eq.2)gssa=0.0
C Trace output.
                  if(tok)then
                     call edisp(icout,' ')
                     write(outs,'(a,i2,a,i3)')
     &               '*** SHADC: calculating diffuse shading at hour ',
     &               ihour,' for patch ',ip-1
                     call edisp(icout,outs)
                   endif

C Test whether to calculate diffuse shading again after first sun-up hour.
                  if(idifc.eq.0.and.init.eq.1)then
                     mode='dup'
                     call tfile3(icomp,itsur,mode)
                     goto 1
                  endif
                  iptch=ip-1
                  if(iptch.ge.1.and.iptch.le.30) nzone = 1
                  if(iptch.ge.31.and.iptch.le.60) nzone = 2
                  if(iptch.ge.61.and.iptch.le.84) nzone = 3
                  if(iptch.ge.85.and.iptch.le.108) nzone = 4
                  if(iptch.ge.109.and.iptch.le.126) nzone = 5
                  if(iptch.ge.127.and.iptch.le.138) nzone = 6
                  if(iptch.ge.139.and.iptch.le.144) nzone = 7
                  if(iptch.eq.145) nzone = 8
                  if(nzone.eq.8) then
                     ptazi = 0.0
                     ptalt = 90.0
                  elseif(nzone.eq.1) then
                     ptazi = azi(nzone)*(iptch - 1)
                     ptalt = alt(nzone)
                  else
                     ptazi = azi(nzone)*((iptch - npz(nzone-1)) - 1)
                     ptalt = alt(nzone)
                  endif
C If current sky patch is also sun position then exclude from
C calculation.
                  diff1=abs(sazir-ptazi)
                  diff2=abs(saltr-ptalt)
                  if(diff1.le.6.0.and.diff2.le.6.0)goto 40
                  sazi1=ptazi
                  salt1=ptalt
               endif
               
C Compute pseudo solar angles relative to the local surface
C coordinate system.
               call trnsf2(ihour)

C Do not initiate projections if transformed surface faces away
C from the sun or sky patch.
               if(sazi2.le.90.0.or.sazi2.ge.270.0
     &              .or.abs(salt2).gt.90.0)goto 2

C Complete projection transformation on an obstruction block (ibn)
C and surface-by-surface (ibs) basis.
               do 50 ibn=1,nbobs(icomp)
                 do 501 i=1,nox(icomp)
                   do 502 j=1,noz(icomp)
                    ihit(i,j)=0
  502              continue
  501            continue

C Compute X, Y and Z coordinates of obstruction block corners 
C relative to the original site coordinate system.
                 call cornr(icomp,ibn)

C Project obstruction block surface, form intersection with plane
C of target surface if necessary, then determine if target surface
C is shaded (and summate related opacities). Pass in index of the
C zone, target surface and the current patch to help document the
C trace report.
                 do 60 ibs=1,6
                   call trnsf3(ihour,icomp,ibn,ibs,itsur,ip)
                   call decomp(icomp,ibn,ibs,itsur)
                   call shdtst(icomp,itsur,opob(icomp,ibn))
   60            continue 
   50          continue         ! obstruction block loop

C For direct shading case, transfer intermediate results (ioshd)
C to zone transitional shading file and determine overall surface
C direct shading (pso).
               if(mode.eq.'dir')then
                  call tfile3(icomp,itsur,mode)

C For diffuse shading case, establish sky patch contribution to
C each target surface grid point (foshd) and determine overall
C diffuse surface shading if last sky patch has just been
C considered (psof).
               elseif(mode.eq.'dif')then
                  do 70 i=1,nox(icomp)
                     do 80 j=1,noz(icomp)
                        if(ioshd(i,j).eq.-1)goto 80
                        foshd(i,j)=foshd(i,j)+oshd(i,j)/145.0
   80                continue
   70             continue
                  if(iptch.eq.145)call tfile3(icomp,itsur,mode)
               endif

C Dump intermediate calculations.
               if(tok)then
                 write(icout,*) ' '
                 write(icout,'(a,i3,a,f6.2,a,f6.2,2a)') 
     &             'SHADC patch loop ',ip,' azimuth ',
     &             sazi1,' elevation ',salt1,' mode ',mode
                 write(icout,*) 'obstructions ihit array'
                 call dumpnoxnozi(icomp,ihour,ihit)
                 write(icout,*) ' '
                 write(icout,*) 'obstructions ioshd array'
                 call dumpnoxnozi(icomp,ihour,ioshd)
                 write(icout,*) ' '
                 write(icout,*) 'obstructions foshd array'
                 call dumpnoxnozr(icomp,ihour,foshd)
                 write(icout,*) ' '
                 write(icout,*) 'obstructions oshd array'
                 call dumpnoxnozr(icomp,ihour,oshd)
               endif

               goto 40

C Surface cannot see sun or sky patch.
    2          if(mode.eq.'dir')then
                  do 90 i=1,nox(icomp)
                     do 100 j=1,noz(icomp)
                        if(ioshd(i,j).eq.-1)goto 100
                        ioshd(i,j)=1
  100                continue
   90             continue
               elseif(mode.eq.'dif')then

C Ipexcl counts the number of sky patches that are behind the target
C surface. It is used in tfile3 to ensure that the diffuse shading
C factor relates only to the visible sky portion.
                  ipexcl=ipexcl+1
               endif
               if(tok)write(icout,*) 'Cannot see sky patch @ ',iptch
               if(ip.eq.1.or.ip.eq.146)call tfile3(icomp,itsur,mode)
   40       continue        ! sky patch loop

C Dump intermediate calculations after patch loop completed.
            if(tok)then
              write(icout,*) 'shdcon end of patch loop======= ',mode
              write(icout,*) 'obstructions ihit array'
              call dumpnoxnozi(icomp,ihour,ihit)
              write(icout,*) ' '
              write(icout,*) 'obstructions ioshd array'
              call dumpnoxnozi(icomp,ihour,ioshd)
              write(icout,*) '  '
              write(icout,*) 'obstructions foshd array'
              call dumpnoxnozr(icomp,ihour,foshd)
              write(icout,*) '  '
              write(icout,*) 'obstructions oshd array'
              call dumpnoxnozr(icomp,ihour,oshd)
            endif

C Indicate that sky vault has been processes once in case idifc = 0
C (i.e. time invariant geometry and sky irradiance distribution).
            init=1

C Output calculation progress report.
    1       kind=kind+1
            calcom=float(kind)/float(kt)
            write(outs,'(a,a3,a)')'`',month(mon),
     &                         '` shading calculation commenced:'
            if(calcom.ge.0.95)then
               if(indcom.eq.4)goto 30
               indcom=indcom+1
               if(paction(1:13).ne.'update_silent')then
                  call usrmsg(outs,' complete','-')
               endif
            elseif(calcom.ge.0.75)then
               if(indcom.eq.3)goto 30
               indcom=indcom+1
               if(paction(1:13).ne.'update_silent')then
                  call usrmsg(outs,' 75% complete','-')
               endif
            elseif(calcom.ge.0.50)then
               if(indcom.eq.2)goto 30
               indcom=indcom+1
               if(paction(1:13).ne.'update_silent')then
                  call usrmsg(outs,' 50% complete','-')
               endif
            elseif(calcom.ge.0.25)then
               if(indcom.eq.1)goto 30
               indcom=indcom+1
               if(paction(1:13).ne.'update_silent')then
                  call usrmsg(outs,' 25% complete','-')
               endif
            endif

   30    continue     ! hour loop
   20 continue        ! target surface loop
      ical =1
      return
      end

C ********************  DECOMP ********************
C If a projected surface crosses the target plane the intersection
C points are located and a modified shadow polygon produced.

      subroutine decomp(izone,ibn,ibs,itsur)
#include "building.h"
#include "geometry.h"

      common/tracech/icout
      common/tc/itc,icnt
      common/transform/xt(4),yt(4),zt(4)
      common/proj/xp(4),yp(4),zp(4)
      common/shad2/ishd,npoint,xs(5),zs(5)

      character case*40,outs*124
      character ZSDES*28,ZSDESC*20,ZSDESS*16
      logical tok
      dimension ipc(4)
      integer lnzsn

C If writing fort.33 file and verbose then include this.
      tok=.false.
      if(icout.eq.33.and.itc.eq.2)tok=.true.

C If one or more of the 4 projected point yp values are > 0 then the
C obstruction block surface crosses the x`z` plane. Depending on the
C number of points that satisfy this condition, the surface is either
C partially or fully behind the target surface.

C Initialise array (ipc) that holds points that cross plane.
      do 10 i=1,4
         ipc(i)=0
   10 continue

C Count number of points that cross plane (iscnt) and set
C ishd=1 to indicate that shdtst should process the generated
C shadow polygon.
      ishd=1
      iscnt=0
      do 20 i=1,4
        xs(i)=xp(i)
        zs(i)=zp(i)
        if(yt(i).gt.0.0)then
          iscnt=iscnt+1
          ipc(iscnt)=i
        else
C Remember point not excluded for use in following 3 point
C exclusion case vertex reordering.
          inx3=i
        endif
   20 continue

C For the case of 3 excluded points, possibly rearrange order
C to ensure correct intersection line selection.
      if(iscnt.eq.3)then
         if(inx3.EQ.2)then
            ipc(1)=3
            ipc(2)=4
            ipc(3)=1
         elseif(inx3.EQ.3)then
            ipc(1)=4
            ipc(2)=1
            ipc(3)=2
         endif
      endif

C *** Case A: No points cross => 4 point shadow polygon.
      if(iscnt.eq.0)then
         case='Intersection case: no points cross.'
         npoint=4
         do 30 i=1,4
C Shadow polygon is projected surface.
            xs(i)=xp(i)
            zs(i)=zp(i)
   30    continue
         goto 1

C *** Case B: 1 point crosses => 2 intersection points and a 5 point
C shadow polygon.
      elseif(iscnt.eq.1)then
         case='Intersection case: 1 point crosses.'
         npoint=5
C 1st line intersection.
         ip1=ipc(1)
         ip2=ip1-1
         if(ip2.eq.0)ip2=4
         call intersect(ip1,ip2,xi,zi)
C Point replaces excluded point.
         xs(ip1)=xi
         zs(ip1)=zi
C 2nd line intersection.
         ip1=ipc(1)
         ip2=ip1+1
         if(ip2.EQ.5)ip2=1
         call intersect(ip1,ip2,xi,zi)
C Point replaces next (anticlockwise) ordered point with subsequent
C points reordered.
         xs(ip2)=xi
         zs(ip2)=zi
         if(ip2.eq.1)then
            xs(5)=xs(4)
            xs(4)=xs(3)
            xs(3)=xs(2)
            xs(2)=xs(1)
            zs(5)=zs(4)
            zs(4)=zs(3)
            zs(3)=zs(2)
            zs(2)=zs(1)
         endif 
         if(ip2.eq.2)then
            xs(5)=xs(4)
            xs(4)=xs(3)
            xs(3)=xs(2)
            zs(5)=zs(4)
            zs(4)=zs(3)
            zs(3)=zs(2)
         endif 
         if(ip2.eq.3)then
            xs(5)=xs(4)
            xs(4)=xs(3)
            zs(5)=zs(4)
            zs(4)=zs(3)
         endif 
         if(ip2.eq.4)then
            xs(5)=xs(4)
            zs(5)=zs(4)
         endif 
         goto 1

C *** Case C: 2 points cross => 2 intersection points and a 4 point
C shadow polygon.
      elseif(iscnt.eq.2)then
         Case='Intersection case: 2 points cross.'
         npoint=4
C 1st line intersection.
         ip1=ipc(1)
         ip2=ip1-1
         if(ip2.eq.0)ip2=4
         if(ip2.eq.ipc(2))then
            ip2=ip1+1
            if(ip2.eq.5)ip2=1
         endif
         call intersect(ip1,ip2,xi,zi)
C Point replaces 1st excluded point.
         xs(ip1)=xi
         zs(ip1)=zi
C 2nd line intersection.
         ip1=ipc(2)
         ip2=ip1+1
         if(ip2.eq.5)ip2=1
         if(ip2.eq.ipc(1))then
            ip2=ip1-1
            if(ip2.eq.0)ip2=4
         endif
         call intersect(ip1,ip2,xi,zi)
C Point replaces 2nd excluded point.
         xs(ip1)=xi
         zs(ip1)=zi
         goto 1

C *** Case D: 3 points cross => 2 intersection points and a 3 point
C shadow polygon.
      elseif(iscnt.eq.3)then
         case='Intersection case: 3 points cross.'
         npoint=3
         do 40 i=1,4
            if(ipc(i).eq.0)then
C Non-excluded point becomes 1st shadow point.
               xs(1)=xp(i)
               zs(1)=zp(i)
            endif
   40    continue
C 1st line intersection.
         ip1=ipc(1)
         ip2=ip1-1
         if(ip2.eq.0)ip2=4
         call intersect(ip1,ip2,xi,zi)
C Point becomes 2nd shadow point.
         xs(2)=xi
         zs(2)=zi
C 2nd line intersection.
         ip1=ipc(3)
         ip2=ip1+1
         if(ip2.eq.5)ip2=1
         call intersect(ip1,ip2,xi,zi)
C Point becomes 2nd shadow point.
         xs(3)=xi
         zs(3)=zi
         goto 1

C *** Case E: Entire surface crosses plane => abandon.
      elseif(iscnt.eq.4)then
C Tell shdtst to abandon shadow polygon.
         ishd=0
         case='Intersection case: all points cross.'
         npoint=0
         goto 1
      endif

C Trace output.
   1  if(tok)then
        call edisp(icout,' ')
        call zsid(izone,itsur,ZSDES,ZSDESC,ZSDESS)
        lnzsn=lnblnk(zsdes)
        write(outs,'(3a,i3,3a,i3,a,i1,a)')
     &    '*** DECOMP: Target surface ',zsdes(1:lnzsn),' (',itsur,
     &    ') obstruction ',
     &    blockname(izone,ibn)(1:lnblockname(izone,ibn)),' (',ibn,
     &    ') face ',ibs,'.'
        call edisp(icout,outs)
        call edisp(icout,case)
        if(iscnt.ne.0)then
          write(outs,'(a,4i2)')'Excluded vertices =',(ipc(i),i=1,iscnt)
          call edisp(icout,outs)
        endif
        call edisp(icout,'Shadow polygon (XS,ZS):')
        call edisp(icout,'     X-Coord   Y-Coord   Z-Coord')
        if(npoint.eq.0)then
          call edisp(icout,'None')
          goto 9999
        endif
        do 9997 I=1,npoint
          write(outs,'(i2,f10.2,f20.2)')i,xs(i),zs(i)
          call edisp(icout,outs)
 9997   continue
      endif

 9999 return
      end

C ********************  INTERSECT ********************
C Determines coordinates of intersection point of YT=0 plane and
C a given line. Uses formula:
C
C x = x1 + (y1.x1-y1.x2)
C          -------------  
C             (y2-y1)

      subroutine intersect(ip1,ip2,xi,zi)

      common/tracech/icout
      common/outin/iuout,iuin,ieout
      common/transform/xt(4),yt(4),zt(4)
      common/shad2/ishd,npoint,xs(5),zs(5)

      character outs*124
      logical tok

      tok=.false.
      if(icout.eq.33)tok=.true.

C Determine intersection points in transformed coordinate system. Note
C that the transformed coordinates are also the projected coordinates
C because the intersection points are in the plane of the target surface.
C In all cases yi=0.0.

C Ensure that line crosses yt=0 plane by ensuring that end point
C yt values have different sign.
      if((yt(ip1).gt.0.0.and.yt(ip2).gt.0.0).or.
     &  (yt(ip1).lt.0.0.and.yt(ip2).lt.0.0))then
         call edisp(iuout,
     &      'INTERSECT ERROR: line does not cross yt=0 plane!')

C Tell shdtst that shadow polygon should be abandoned.
         ishd=0
         goto 1
      endif

C Avoid divide by 0.
      diff=yt(ip2)-yt(ip1)
      if(abs(diff).lt.1.E-5)diff=1.E-5

C Intersection point.
       xi=xt(ip1)+(((yt(ip1)*xt(ip1))-(yt(ip1)*xt(ip2)))/diff)
       zi=zt(ip1)+(((yt(ip1)*zt(ip1))-(yt(ip1)*zt(ip2)))/diff)

C Trace output.
   1  if(tok)then
        call edisp(icout,' ')
        if(ishd.eq.1)call edisp(icout,
     &       '*** INTERSECT: line crosses yt=0 plane.')
        if(ishd.eq.0)call edisp(icout,
     &       '*** INTERSECT: line does not cross yt=0 plane.')
        write(outs,'(a,i1,a,i1)')'Line = ',ip1,' - ',ip2
        call edisp(icout,outs)
        write(outs,'(a,3f10.3)')'Point 1 = ',xt(ip1),yt(ip1),zt(ip1)
        call edisp(icout,outs)
        write(outs,'(a,3f10.3)')'Point 2 = ',xt(ip2),yt(ip2),zt(ip2)
        call edisp(icout,outs)
        write(outs,'(a,2f10.3)')'Intersection = ',xi,zi
        call edisp(icout,outs)
      endif

      return
      end

C ******************** SHDTST ********************
C Tests each grid centre point of a user-specified grid to
C establish if it is within the target surface and shadow polygon.

      subroutine shdtst(izone,itsur,op)
#include "building.h"
#include "geometry.h"

      common/shad2/ishd,npoint,xs(5),zs(5)
      common/grid11/xop(mox,moz),zop(mox,moz)
      common/shad3/ioshd(mox,moz),foshd(mox,moz),gssa,ipexcl
      common/shad4/oshd(mox,moz),ihit(mox,moz)
      common/outin/iuout,iuin,ieout

      if(ishd.eq.0)return

C For each opaque surface grid point, test whether it is within 
C the target surface. If it is, then test whether also within
C the shadow polygon.
      do 10 i=1,nox(izone)
         do 20 j=1,noz(izone)
C Do nothing if point is not within surface itsur.
            if(ioshd(i,j).eq.-1)goto 20

C Do nothing if point is not within the shadow polygon as held in common/shad2/.
            call point2(xop(i,j),zop(i,j),ians)
            if(ians.eq.0)goto 20

C Point obscured by current obstruction block face. Point shaded
C if any face obscures so only increment oshd for first hit, ignoring
C other hits.
            ihit(i,j)=ihit(i,j)+1
            ioshd(i,j)=1

C If more than one obstruction block shaded then opacities need to be
C compounded.
            if(ihit(i,j).eq.1)oshd(i,j)=1.0-(1.0-oshd(i,j))*(1.0-op)
            if(oshd(i,j).gt.1.0)oshd(i,j)=1.0
   20    continue
   10 continue

      return
      end
