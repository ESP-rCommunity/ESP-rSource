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

C Set default uncertainties:
C   SDDFLT001:    Thermophysical properties. 

C ***************** SDDFLT001 *********************
C SDDFLT001 set defaults based on material type.
      subroutine SDDFLT001
#include "building.h"
#include "uncertainty.h"
#include "esprdbfile.h"
C esprdbfile.h supplies the following:
C LFMAT,IFMAT,LFMUL,IFMUL (for materials and constructions database)
#include "material.h"

      common/pophelp/h(60)

      real DFLTRANGE     ! topic & attribute increments
      integer IDFLTR     ! 1=% 2=abs
      common/UADFLT/DFLTRANGE(6,6),IDFLTR(6,6)
      CHARACTER ITEM(30)*32

      character H*72,KEY*1,outs*124,sym*2
      character t32*32,d32*32
      logical closemat1,closemat2,ok,dok
      integer NCO,ICO ! max items and current menu item
      
C Set default uncertainties. 5% as %. If user asks.
      dok=.true.
      CALL ASKOK('Set initial standard uncertainty range defaults?  ',
     & '(all to 5%)',OK,DOK,5)
      if(ok)then
        do I=1,6
          do J=1,6
            DFLTRANGE(I,J)=5.0
            IDFLTR(I,J)=1
          enddo
        enddo
      endif
      
C Setup for menu.
C Gather the names of the various classes and present this list.
  40  ICO=-1

C << arrange defaults by the general topic
C << update the DFLTMAT to work with multiple main and sub-topics
C << show % or a in a column plus the value for the topic.
      ITEM(1) =' Weather...                    '
      if(IDFLTR(3,1).eq.1) sym=' %'
      if(IDFLTR(3,1).eq.2) sym=' a'
      write(ITEM(2),'(a,f5.2,a)')'a dry bulb temperature ',
     &  DFLTRANGE(3,1),sym
      if(IDFLTR(3,2).eq.1) sym=' %'
      if(IDFLTR(3,2).eq.2) sym=' a'
      write(ITEM(3),'(a,f5.2,a)')'b direct normal solar ',
     &  DFLTRANGE(3,2),sym
      if(IDFLTR(3,3).eq.1) sym=' %'
      if(IDFLTR(3,3).eq.2) sym=' a'
      write(ITEM(4),'(a,f5.2,a)')'c diffuse horiz solar ',
     &  DFLTRANGE(3,3),sym
      if(IDFLTR(3,4).eq.1) sym=' %'
      if(IDFLTR(3,4).eq.2) sym=' a'
      write(ITEM(5),'(a,f5.2,a)')'d wind speed ',
     &  DFLTRANGE(3,4),sym
      if(IDFLTR(3,5).eq.1) sym=' %'
      if(IDFLTR(3,5).eq.2) sym=' a'
      write(ITEM(6),'(a,f5.2,a)')'e wind direction ',
     &  DFLTRANGE(3,5),sym
      if(IDFLTR(3,6).eq.1) sym=' %'
      if(IDFLTR(3,6).eq.2) sym=' a'
      write(ITEM(7),'(a,f5.2,a)')'f relative humidity ',
     &  DFLTRANGE(3,6),sym
      ITEM(8) =' Thermophysical properties..   '
      if(IDFLTR(1,1).eq.1) sym=' %'
      if(IDFLTR(1,1).eq.2) sym=' a'
      write(ITEM(9), '(a,f5.2,a)')'g conductivity ',
     &  DFLTRANGE(1,1),sym
      if(IDFLTR(1,2).eq.1) sym=' %'
      if(IDFLTR(1,2).eq.2) sym=' a'
      write(ITEM(10),'(a,f5.2,a)')'h density ',
     &  DFLTRANGE(1,2),sym
      if(IDFLTR(1,3).eq.1) sym=' %'
      if(IDFLTR(1,3).eq.2) sym=' a'
      write(ITEM(11),'(a,f5.2,a)')'i specific heat ',
     &  DFLTRANGE(1,3),sym
      if(IDFLTR(1,4).eq.1) sym=' %'
      if(IDFLTR(1,4).eq.2) sym=' a'
      write(ITEM(12),'(a,f5.2,a)')'j emissivity ',
     &  DFLTRANGE(1,4),sym
      if(IDFLTR(1,5).eq.1) sym=' %'
      if(IDFLTR(1,5).eq.2) sym=' a'
      write(ITEM(13),'(a,f5.2,a)')'k absorptivity ',
     &  DFLTRANGE(1,5),sym
      ITEM(14)=' Construction composites       '
      if(IDFLTR(2,1).eq.1) sym=' %'
      if(IDFLTR(2,1).eq.2) sym=' a'
      write(ITEM(15),'(a,f5.2,a)')'l layer thickness ',
     &  DFLTRANGE(2,1),sym
      ITEM(16)=' Ideal controls...             '
      if(IDFLTR(5,1).eq.1) sym=' %'
      if(IDFLTR(5,1).eq.2) sym=' a'
      write(ITEM(17),'(a,f5.2,a)')'m period start time ',
     &  DFLTRANGE(5,1),sym
      if(IDFLTR(5,2).eq.1) sym=' %'
      if(IDFLTR(5,2).eq.2) sym=' a'
      write(ITEM(18),'(a,f5.2,a)')'n heat & Cool cap ',
     &  DFLTRANGE(5,2),sym
      if(IDFLTR(5,4).eq.1) sym=' %'
      if(IDFLTR(5,4).eq.2) sym=' a'
      write(ITEM(19),'(a,f5.2,a)')'o heat & Cool set pt ',
     &  DFLTRANGE(5,4),sym
      ITEM(20)=' Zones composition ...         '
      if(IDFLTR(6,1).eq.1) sym=' %'
      if(IDFLTR(6,1).eq.2) sym=' a'
      write(ITEM(21),'(a,f5.2,a)')'p geometry (air volume)',
     &  DFLTRANGE(6,1),sym
      if(IDFLTR(6,2).eq.1) sym=' %'
      if(IDFLTR(6,2).eq.2) sym=' a'
      write(ITEM(22),'(a,f5.2,a)')'q geometry (surf area)',
     &  DFLTRANGE(6,2),sym
      ITEM(23)=' Zone operations...            '
      if(IDFLTR(4,1).eq.1) sym=' %'
      if(IDFLTR(4,1).eq.2) sym=' a'
      write(ITEM(24),'(a,f5.2,a)')'r period start time ',
     &  DFLTRANGE(4,1),sym
      if(IDFLTR(4,2).eq.1) sym=' %'
      if(IDFLTR(4,2).eq.2) sym=' a'
      write(ITEM(25),'(a,f5.2,a)')'s sensible & Latent W',
     &  DFLTRANGE(4,2),sym
      if(IDFLTR(4,4).eq.1) sym=' %'
      if(IDFLTR(4,4).eq.2) sym=' a'
      write(ITEM(26),'(a,f5.2,a)')'t rad/conv split ',
     &  DFLTRANGE(4,4),sym
      ITEM(27)='* clear all defaults (to 0.01%)'
      ITEM(28)='* initialise defaults          '
      ITEM(29)='? Help                         '
      ITEM(30)='- Exit                         '
      NCO=30

C Help text for this menu.
      H(1)='Each of the uncertainty topics can have defaults for how'
      H(2)='the range of uncertainties are treated. Users can define'
      H(3)='these as either % of the current value of the attribute'
      H(4)='or as a fixed value offset. At run time these definitions'
      H(5)='of the range of uncertainty is multipled by a random '
      H(6)='number (in the range of -2.0 to 2.0). '
      H(7)='If you set new defaults for the topics you can use'
      H(8)='them as you define new distributions (otherwise you'
      H(9)='will have to nominate values for each one manually. '
      H(10)=' '
      H(11)='Intially 5% is assumed for each. If you clear the defaults'
      H(12)='the value is reset to 0.01% (effectively no change). '

      CALL EMENU('Uncertainty ranges',ITEM,NCO,ICO)

      IF(ICO.EQ.NCO)THEN
        RETURN
      ELSEIF(ICO.EQ.(NCO-1))THEN
        CALL PHELPD('uncertainty_range',12,'-',0,0,IER)
      ELSEIF(ICO.EQ.(NCO-2))THEN
      
C Initialise default uncertainties. 5.0% and %.
        do I=1,6
          do J=1,6
            DFLTRANGE(I,J)=5.0
            IDFLTR(I,J)=1
          enddo
        enddo
      ELSEIF(ICO.EQ.(NCO-3))THEN
      
C Clear default uncertainties. 0.01% and %.
        do I=1,6
          do J=1,6
            DFLTRANGE(I,J)=0.01
            IDFLTR(I,J)=1
          enddo
        enddo
      ELSEIF(ICO.EQ.(NCO-3))THEN
        continue
      ELSEIF(ICO.EQ.1.OR.ICO.EQ.8.OR.ICO.EQ.14.OR.ICO.EQ.16)THEN
        continue
      ELSEIF(ICO.EQ.2)THEN

C Deal with weather ambient T.
        VALUE=DFLTRANGE(3,1); DVALUE=DFLTRANGE(3,1)
        call EASKAB('Define ambient T bounds',' via:',
     &    'percentage change','absolute change',ICNG,nbhelp)
        IDFLTR(3,1)=ICNG
        if(ICNG.eq.1)then
          call EASKR(VALUE,' ','Percentage change value?',
     &      0.,'F',100.,'F',DVALUE,'Percentage change',IER,nbhelp)
        else
          call EASKR(VALUE,' ','Absolute change value?',
     &      0.,'F',10.,'W',DVALUE,'absolute change',IER,nbhelp)
        endif
        DFLTRANGE(3,1)=VALUE
      ELSEIF(ICO.EQ.3)THEN

C Deal with weather direct normal solar.
        VALUE=DFLTRANGE(3,2); DVALUE=DFLTRANGE(3,2)
        call EASKAB('Define direct normal solar bounds',' via:',
     &    'percentage change','absolute change',ICNG,nbhelp)
        IDFLTR(3,2)=ICNG
        if(ICNG.eq.1)then
          call EASKR(VALUE,' ','Percentage change value?',
     &      0.,'F',100.,'F',DVALUE,'Percentage change',IER,nbhelp)
        else
          call EASKR(VALUE,' ','Absolute change value?',
     &      0.,'F',20.,'W',DVALUE,'absolute change',IER,nbhelp)
        endif
        DFLTRANGE(3,2)=VALUE
      ELSEIF(ICO.EQ.4)THEN

C Deal with weather diffuse horiz solar.
        VALUE=DFLTRANGE(3,3); DVALUE=DFLTRANGE(3,3)
        call EASKAB('Define diffuse horiz solar bounds',' via:',
     &    'percentage change','absolute change',ICNG,nbhelp)
        IDFLTR(3,3)=ICNG
        if(ICNG.eq.1)then
          call EASKR(VALUE,' ','Percentage change value?',
     &      0.,'F',100.,'F',DVALUE,'Percentage change',IER,nbhelp)
        else
          call EASKR(VALUE,' ','Absolute change value?',
     &      0.,'F',20.,'W',DVALUE,'absolute change',IER,nbhelp)
        endif
        DFLTRANGE(3,3)=VALUE
      ELSEIF(ICO.EQ.5)THEN

C Deal with weather wind speed.
        VALUE=DFLTRANGE(3,4); DVALUE=DFLTRANGE(3,4)
        call EASKAB('Define wind speed bounds',' via:',
     &    'percentage change','absolute change',ICNG,nbhelp)
        IDFLTR(3,4)=ICNG
        if(ICNG.eq.1)then
          call EASKR(VALUE,' ','Percentage change value?',
     &      0.,'F',100.,'F',DVALUE,'Percentage change',IER,nbhelp)
        else
          call EASKR(VALUE,' ','Absolute change value?',
     &      0.,'F',10.,'W',DVALUE,'absolute change',IER,nbhelp)
        endif
        DFLTRANGE(3,4)=VALUE
      ELSEIF(ICO.EQ.6)THEN

C Deal with weather wind direction.
        VALUE=DFLTRANGE(3,5); DVALUE=DFLTRANGE(3,5)
        call EASKAB('Define wind direction bounds',' via:',
     &    'percentage change','absolute change',ICNG,nbhelp)
        IDFLTR(3,5)=ICNG
        if(ICNG.eq.1)then
          call EASKR(VALUE,' ','Percentage change value?',
     &      0.,'F',100.,'F',DVALUE,'Percentage change',IER,nbhelp)
        else
          call EASKR(VALUE,' ','Absolute change value?',
     &      0.,'F',30.,'W',DVALUE,'absolute change',IER,nbhelp)
        endif
        DFLTRANGE(3,5)=VALUE
      ELSEIF(ICO.EQ.7)THEN

C Deal with weather relative humidity.
        VALUE=DFLTRANGE(3,6); DVALUE=DFLTRANGE(3,6)
        call EASKAB('Define relative humidity bounds',' via:',
     &    'percentage change','absolute change',ICNG,nbhelp)
        IDFLTR(3,6)=ICNG
        if(ICNG.eq.1)then
          call EASKR(VALUE,' ','Percentage change value?',
     &      0.,'F',100.,'F',DVALUE,'Percentage change',IER,nbhelp)
        else
          call EASKR(VALUE,' ','Absolute change value?',
     &      0.,'F',20.,'W',DVALUE,'absolute change',IER,nbhelp)
        endif
        DFLTRANGE(3,6)=VALUE
      ELSEIF(ICO.EQ.9)THEN

C Deal with conductivity.
        VALUE=DFLTRANGE(1,1); DVALUE=DFLTRANGE(1,1)
        call EASKAB('Define conductivity bounds',' via:',
     &    'percentage change','absolute change',ICNG,nbhelp)
        IDFLTR(1,1)=ICNG
        if(ICNG.eq.1)then
          call EASKR(VALUE,' ','Percentage change value?',
     &      0.,'F',100.,'F',DVALUE,'Percentage change',IER,nbhelp)
        else
          call EASKR(VALUE,' ','Absolute change value?',
     &      0.,'F',5.0,'W',DVALUE,'absolute change',IER,nbhelp)
        endif
        DFLTRANGE(1,1)=VALUE
      ELSEIF(ICO.EQ.10)THEN

C Deal with density.
        VALUE=DFLTRANGE(1,2); DVALUE=DFLTRANGE(1,2)
        call EASKAB('Define density bounds',' via:',
     &    'percentage change','absolute change',ICNG,nbhelp)
        IDFLTR(1,2)=ICNG
        if(ICNG.eq.1)then
          call EASKR(VALUE,' ','Percentage change value?',
     &      0.,'F',100.,'F',DVALUE,'Percentage change',IER,nbhelp)
        else
          call EASKR(VALUE,' ','Absolute change value?',
     &      0.,'F',5.0,'W',DVALUE,'absolute change',IER,nbhelp)
        endif
        DFLTRANGE(1,2)=VALUE
      ELSEIF(ICO.EQ.11)THEN

C Deal with specific Heat.
        VALUE=DFLTRANGE(1,3); DVALUE=DFLTRANGE(1,3)
        call EASKAB('Define specific heat bounds',' via:',
     &    'percentage change','absolute change',ICNG,nbhelp)
        IDFLTR(1,3)=ICNG
        if(ICNG.eq.1)then
          call EASKR(VALUE,' ','Percentage change value?',
     &      0.,'F',100.,'F',DVALUE,'Percentage change',IER,nbhelp)
        else
          call EASKR(VALUE,' ','Absolute change value?',
     &      0.,'F',10.,'W',DVALUE,'absolute change',IER,nbhelp)
        endif
        DFLTRANGE(1,3)=VALUE
      ELSEIF(ICO.EQ.12)THEN

C Deal with long wave emissivity.
        VALUE=DFLTRANGE(1,4); DVALUE=DFLTRANGE(1,4)
        call EASKAB('Define long wave emissivity bounds',' via:',
     &    'percentage change','absolute change',ICNG,nbhelp)
        IDFLTR(1,4)=ICNG
        if(ICNG.eq.1)then
          call EASKR(VALUE,' ','Percentage change value?',
     &      0.,'F',100.,'F',DVALUE,'Percentage change',IER,nbhelp)
        else
          call EASKR(VALUE,' ','Absolute change value?',
     &      0.,'F',0.3,'W',DVALUE,'absolute change',IER,nbhelp)
        endif
        DFLTRANGE(1,4)=VALUE
      ELSEIF(ICO.EQ.13)THEN

C Deal with short wave absorptivity.
        VALUE=DFLTRANGE(1,5); DVALUE=DFLTRANGE(1,5)
        call EASKAB('Define short wave absorptivity bounds',' via:',
     &    'percentage change','absolute change',ICNG,nbhelp)
        IDFLTR(1,5)=ICNG
        if(ICNG.eq.1)then
          call EASKR(VALUE,' ','Percentage change value?',
     &      0.,'F',100.,'F',DVALUE,'Percentage change',IER,nbhelp)
        else
          call EASKR(VALUE,' ','Absolute change value?',
     &      0.,'F',0.3,'W',DVALUE,'absolute change',IER,nbhelp)
        endif
        DFLTRANGE(1,5)=VALUE
      ELSEIF(ICO.EQ.15)THEN

C Deal with layer thickness in a MLC.
        VALUE=DFLTRANGE(2,1); DVALUE=DFLTRANGE(2,1)
        call EASKAB('Define MLC layer thickness bounds',' via:',
     &    'percentage change','absolute change',ICNG,nbhelp)
        IDFLTR(2,1)=ICNG
        if(ICNG.eq.1)then
          call EASKR(VALUE,' ','Percentage change value?',
     &      0.,'F',100.,'F',DVALUE,'Percentage change',IER,nbhelp)
        else
          call EASKR(VALUE,' ','Absolute change value?',
     &      0.,'F',0.1,'W',DVALUE,'absolute change',IER,nbhelp)
        endif
        DFLTRANGE(2,1)=VALUE
      ELSEIF(ICO.EQ.17)THEN

C Deal with ideal control period start time.
        VALUE=DFLTRANGE(5,1); DVALUE=DFLTRANGE(5,1)
        call EASKAB('Ideal control period start time bounds',' via:',
     &    'percentage change','absolute change',ICNG,nbhelp)
        IDFLTR(5,1)=ICNG
        if(ICNG.eq.1)then
          call EASKR(VALUE,' ','Percentage change value?',
     &      0.,'F',100.,'F',DVALUE,'Percentage change',IER,nbhelp)
        else
          call EASKR(VALUE,' ','Absolute change value?',
     &      0.,'F',1.0,'W',DVALUE,'absolute change',IER,nbhelp)
        endif
        DFLTRANGE(5,1)=VALUE
      ELSEIF(ICO.EQ.18)THEN

C Deal with ideal control heat & Cool capacity.
        VALUE=DFLTRANGE(5,2); DVALUE=DFLTRANGE(5,2)
        call EASKAB('Ideal control heat & cool capacity',' via:',
     &    'percentage change','absolute change',ICNG,nbhelp)
        IDFLTR(5,2)=ICNG
        IDFLTR(5,3)=ICNG
        if(ICNG.eq.1)then
          call EASKR(VALUE,' ','Percentage change value?',
     &      0.,'F',100.,'F',DVALUE,'Percentage change',IER,nbhelp)
        else
          call EASKR(VALUE,' ','Absolute change value?',
     &      0.,'F',200.,'W',DVALUE,'absolute change',IER,nbhelp)
        endif
        DFLTRANGE(5,2)=VALUE
        DFLTRANGE(5,3)=VALUE
      ELSEIF(ICO.EQ.19)THEN

C Deal with ideal control heat & cool set points.
        VALUE=DFLTRANGE(5,4); DVALUE=DFLTRANGE(5,4)
        call EASKAB('Ideal control heat & cool set points',' via:',
     &    'percentage change','absolute change',ICNG,nbhelp)
        IDFLTR(5,4)=ICNG
        IDFLTR(5,5)=ICNG
        if(ICNG.eq.1)then
          call EASKR(VALUE,' ','Percentage change value?',
     &      0.,'F',100.,'F',DVALUE,'Percentage change',IER,nbhelp)
        else
          call EASKR(VALUE,' ','Absolute change value?',
     &      0.,'F',2.0,'W',DVALUE,'absolute change',IER,nbhelp)
        endif
        DFLTRANGE(5,4)=VALUE
        DFLTRANGE(5,5)=VALUE
      ELSEIF(ICO.EQ.21)THEN

C Deal with zone air volume.
        VALUE=DFLTRANGE(6,1); DVALUE=DFLTRANGE(6,1)
        call EASKAB('Define zone air volume bounds',' via:',
     &    'percentage change','absolute change',ICNG,nbhelp)
        IDFLTR(6,1)=ICNG
        if(ICNG.eq.1)then
          call EASKR(VALUE,' ','Percentage change value?',
     &      0.,'F',100.,'F',DVALUE,'Percentage change',IER,nbhelp)
        else
          call EASKR(VALUE,' ','Absolute change value?',
     &      0.,'F',2.0,'W',DVALUE,'absolute change',IER,nbhelp)
        endif
        DFLTRANGE(6,1)=VALUE
      ELSEIF(ICO.EQ.22)THEN

C Deal with surface area.
        VALUE=DFLTRANGE(6,2); DVALUE=DFLTRANGE(6,2)
        call EASKAB('Define surface area bounds',' via:',
     &    'percentage change','absolute change',ICNG,nbhelp)
        IDFLTR(6,2)=ICNG
        if(ICNG.eq.1)then
          call EASKR(VALUE,' ','Percentage change value?',
     &      0.,'F',100.,'F',DVALUE,'Percentage change',IER,nbhelp)
        else
          call EASKR(VALUE,' ','Absolute change value?',
     &      0.,'F',2.0,'W',DVALUE,'absolute change',IER,nbhelp)
        endif
        DFLTRANGE(6,2)=VALUE
      ELSEIF(ICO.EQ.24)THEN

C Deal with zone operations period start time.
        VALUE=DFLTRANGE(4,1); DVALUE=DFLTRANGE(4,1)
        call EASKAB('zone operations period start time bounds',' via:',
     &    'percentage change','absolute change',ICNG,nbhelp)
        IDFLTR(4,1)=ICNG
        if(ICNG.eq.1)then
          call EASKR(VALUE,' ','Percentage change value?',
     &      0.,'F',100.,'F',DVALUE,'Percentage change',IER,nbhelp)
        else
          call EASKR(VALUE,' ','Absolute change value?',
     &      0.,'F',2.0,'W',DVALUE,'absolute change',IER,nbhelp)
        endif
        DFLTRANGE(4,1)=VALUE
      ELSEIF(ICO.EQ.25)THEN

C Deal with casual gain sensible & latent W.
        VALUE=DFLTRANGE(4,2); DVALUE=DFLTRANGE(4,2)
        call EASKAB('Casual gain sensible & latent W bounds',' via:',
     &    'percentage change','absolute change',ICNG,nbhelp)
        IDFLTR(4,2)=ICNG
        IDFLTR(4,3)=ICNG
        if(ICNG.eq.1)then
          call EASKR(VALUE,' ','Percentage change value?',
     &      0.,'F',100.,'F',DVALUE,'Percentage change',IER,nbhelp)
        else
          call EASKR(VALUE,' ','Absolute change value?',
     &      0.,'F',200.,'W',DVALUE,'absolute change',IER,nbhelp)
        endif
        DFLTRANGE(4,2)=VALUE
        DFLTRANGE(4,3)=VALUE
      ELSEIF(ICO.EQ.26)THEN

C Deal with zone casual radiant convective split.
        VALUE=DFLTRANGE(4,4); DVALUE=DFLTRANGE(4,4)
        call EASKAB('zone casual rad/convective split bounds',' via:',
     &    'percentage change','absolute change',ICNG,nbhelp)
        IDFLTR(4,4)=ICNG
        if(ICNG.eq.1)then
          call EASKR(VALUE,' ','Percentage change value?',
     &      0.,'F',100.,'W',DVALUE,'Percentage change',IER,nbhelp)
        else
          call EASKR(VALUE,' ','Absolute change value?',
     &      0.,'F',0.5,'W',DVALUE,'absolute change',IER,nbhelp)
        endif
        DFLTRANGE(4,4)=VALUE
      endif

      goto 40

      end
