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

C RADCTL.F contains routines to control the creation of radiance files 
C and the control the visulaisation process depending on the value of 
C SCENEPURP. Contains:
C  RADPAR - check and update render options.

C ********************* RADPAR *********************
C RADPAR - check and update render options.
C If VAL is < 0 then return current setting otherwise set par=VAL.
      SUBROUTINE RADPAR(PAR,VAL)
#include "building.h"
#include "e2r_common.h"

      integer lnblnk  ! function definition
      integer iCountWords

      COMMON/FILEP/IFIL
      common/expath/runpath,pathtype

      character flag*8,PAR*2
      character OUTSTR*124,ltmp*72
      character runpath*72,pathtype*24

      ITA2 = IFIL+7

C Set all values to -1 (i.e. unset).
C Direct values.
      xdp=-1.
      xds=-1.
      xdj=-1.
      xdt=-1.
      xdc=-1.
      xdr=-1.

C Specular values.
      xsj=-1.
      xst=-1.

C Ambient values.
      xab=-1.
      xaa=-1.
      xad=-1.
      xas=-1.
      xar=-1.
      xav=-1.

C Reflections.
      xlr=-1.
      xlw=-1.

C Mist. - not active yet
C      xms=-1.
      

C Check for defined items - if found then decode.
      IW = iCountWords(crenrp)
      K=0
 10   if (IW.gt.1) then
        call EGETW(crenrp,K,flag,'W','radiance parameter',IER)
        if (flag(2:3).eq.'dp') then
          call EGETWR(crenrp,K,xdp,0.0,2048.0,'W','dir pretest den',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'ds') then
          call EGETWR(crenrp,K,xds,0.0,1.0,'W','direct sampling',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'dj') then
          call EGETWR(crenrp,K,xdj,0.0,1.0,'W','direct jitter',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'dt') then
          call EGETWR(crenrp,K,xdt,0.0,1.0,'W','direct threshold',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'dc') then
          call EGETWR(crenrp,K,xdc,0.0,1.0,'W','direct certainty',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'dr') then
          call EGETWR(crenrp,K,xdr,0.0,10.0,'W','direct relays',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'sj') then
          call EGETWR(crenrp,K,xsj,0.0,1.0,'W','specular jitter',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'st') then
          call EGETWR(crenrp,K,xst,0.0,1.0,'W','specular threshold',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'ab') then
          call EGETWR(crenrp,K,xab,0.0,7.0,'W','ambient bounces',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'aa') then
          call EGETWR(crenrp,K,xaa,0.0,0.5,'W','ambient accuracy',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'ad') then
          call EGETWR(crenrp,K,xad,0.0,4096.0,'W','ambient divs',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'as') then
          call EGETWR(crenrp,K,xas,0.0,2048.0,'W','amb super-smpls',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'ar') then
          call EGETWR(crenrp,K,xar,0.0,1024.0,'W','amb resolution',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'av') then
          call EGETWR(crenrp,K,xavr,0.0,30.0,'W','amb value red',IER)
          call EGETWR(crenrp,K,xavg,0.0,30.0,'W','amb value green',IER)
          call EGETWR(crenrp,K,xavb,0.0,30.0,'W','amb value blue',IER)
          xav=(xavr+xavg+xavb)/3.
          IW=IW-4
        elseif (flag(2:3).eq.'lr') then
          call EGETWR(crenrp,K,xlr,0.0,12.0,'W','limit reflection',IER)
          IW=IW-2
        elseif (flag(2:3).eq.'lw') then
          call EGETWR(crenrp,K,xlw,0.0,1.0,'W','limit weight',IER)
          IW=IW-2
        else

C Debug.
C          write(6,*) 'Unknown radiance parameter (1): ',flag
        endif

C Check other parameters.
        goto 10
      endif

      if (VAL.lt.0.0) then

C Return current value of parameter.
        if (PAR.eq.'dp') then
          VAL=xdp
        elseif (PAR.eq.'ds') then
          VAL=xds
        elseif (PAR.eq.'dj') then
          VAL=xdj
        elseif (PAR.eq.'dt') then
          VAL=xdt
        elseif (PAR.eq.'dc') then
          VAL=xdc
        elseif (PAR.eq.'dr') then
          VAL=xdr
        elseif (PAR.eq.'sj') then
          VAL=xsj
        elseif (PAR.eq.'st') then
          VAL=xst
        elseif (PAR.eq.'ab') then
          VAL=xab
        elseif (PAR.eq.'aa') then
          VAL=xaa
        elseif (PAR.eq.'ad') then
          VAL=xad
        elseif (PAR.eq.'as') then
          VAL=xas
        elseif (PAR.eq.'ar') then
          VAL=xar
        elseif (PAR.eq.'av') then
          VAL=xav
        elseif (PAR.eq.'lr') then
          VAL=xlr
        elseif (PAR.eq.'lw') then
          VAL=xlw
        else

C Debug.
C          write(6,*) 'Unknown radiance parameter (2): ',PAR

        endif

C If no value has been found for the parameter then scan options file.
        if (VAL.lt.0.0) then
          write (ltmp,'(2a)') runpath(1:lnblnk(runpath)),
     &                        optnfil(1:lnblnk(optnfil))
          call FPOPEN(ITA2,ISTAT,1,1,ltmp)
 25       call STRIPC(ITA2,OUTSTR,99,ND,0,'rad options',IER)
          if (IER.eq.0) then
            K=0
            call EGETW(OUTSTR,K,FLAG,'W','radiance parameter',IER)
            if (FLAG(2:3).eq.PAR(1:2)) then
              XV=0.
              do 20 IP=1,ND-1
                call EGETWR(OUTSTR,K,XVA,0.0,0.0,'-','rad par val',IER)
                XV=XV+XVA
 20           continue
              VAL=XV/real(ND-1)
            endif
            goto 25
          endif
          CALL ERPFREE(ITA2,ISTAT)
        endif
      else

C Set new/ Update parameter.
        if (PAR.eq.'dp') then
          xdp=VAL
        elseif (PAR.eq.'ds') then
          xds=VAL
        elseif (PAR.eq.'dj') then
          xdj=VAL
        elseif (PAR.eq.'dt') then
          xdt=VAL
        elseif (PAR.eq.'dc') then
          xdc=VAL
        elseif (PAR.eq.'dr') then
          xdr=VAL
        elseif (PAR.eq.'sj') then
          xsj=VAL
        elseif (PAR.eq.'st') then
          xst=VAL
        elseif (PAR.eq.'ab') then
          xab=VAL
        elseif (PAR.eq.'aa') then
          xaa=VAL
        elseif (PAR.eq.'ad') then
          xad=VAL
        elseif (PAR.eq.'as') then
          xas=VAL
        elseif (PAR.eq.'ar') then
          xar=VAL
        elseif (PAR.eq.'av') then
          xav=VAL
        elseif (PAR.eq.'lr') then
          xlr=VAL
        elseif (PAR.eq.'lw') then
          xlw=VAL
        else

C Debug.
C          write(6,*) 'Unknown radiance parameter (3): ',PAR

        endif

C Write out new string.
        crenrp=' '
        IPOSE=0
        IPOS=IPOSE+1
        if (xdp.ge.0.0) then
          IPOSE=IPOS+9
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,i4)') '-dp',int(xdp)
          IPOS=IPOSE+1
        endif
        if (xds.ge.0.0) then
          IPOSE=IPOS+9
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,f4.2)') '-ds',xds
          IPOS=IPOSE+1
        endif
        if (xdj.ge.0.0) then
          IPOSE=IPOS+9
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,f4.2)') '-dj',xdj
          IPOS=IPOSE+1
        endif
        if (xdt.ge.0.0) then
          IPOSE=IPOS+9
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,f4.2)') '-dt',xdt
          IPOS=IPOSE+1
        endif
        if (xdc.ge.0.0) then
          IPOSE=IPOS+9
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,f4.2)') '-dc',xdc
          IPOS=IPOSE+1
        endif
        if (xdr.ge.0.0) then
          IPOSE=IPOS+7
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,i2)') '-dc',int(xdr)
          IPOS=IPOSE+1
        endif
        if (xsj.ge.0.0) then
          IPOSE=IPOS+9
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,f4.2)') '-sj',xsj
          IPOS=IPOSE+1
        endif
        if (xst.ge.0.0) then
          IPOSE=IPOS+9
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,f4.2)') '-st',xst
          IPOS=IPOSE+1
        endif
        if (xab.ge.0.0) then
          IPOSE=IPOS+7
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,i2)') '-ab',int(xab)
          IPOS=IPOSE+1
        endif
        if (xaa.ge.0.0) then
          IPOSE=IPOS+9
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,f4.2)') '-aa',xaa
          IPOS=IPOSE+1
        endif
        if (xad.ge.0.0) then
          IPOSE=IPOS+9
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,i4)') '-ad',int(xad)
          IPOS=IPOSE+1
        endif
        if (xas.ge.0.0) then
          IPOSE=IPOS+9
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,i4)') '-as',int(xas)
          IPOS=IPOSE+1
        endif
        if (xar.ge.0.0) then
          IPOSE=IPOS+9
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,i4)') '-ar',int(xar)
          IPOS=IPOSE+1
        endif
        if (xav.ge.0.0) then
          IPOSE=IPOS+19
         write(crenrp(IPOS:IPOSE),'(1x,a3,3(1x,f4.1))')'-av',xav,xav,xav
          IPOS=IPOSE+1
        endif
        if (xlr.ge.0.0) then
          IPOSE=IPOS+7
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,i2)') '-lr',int(xlr)
          IPOS=IPOSE+1
        endif
        if (xlw.ge.0.0) then
          IPOSE=IPOS+10
          write (crenrp(IPOS:IPOSE),'(1x,a3,1x,f5.3)') '-lw',int(xlw)
          IPOS=IPOSE+1
        endif
      endif

      return
      end
