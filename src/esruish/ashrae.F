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

C ASHRAE.F contains the following reoutine:
C WINSHD - Establishes dimensions of window and overhang/side-fins
C          if any.
C WSHAD  - Computes the shading for the given time of year and
C          surface azimuth.

C ********* WINSHD
C ASHRAE algorithm to compute the shading on a single, vertical,
C rectangular window caused by overhang and/or side fins.

      subroutine winshd
#include "building.h"

      common/outin/iuout,iuin
      common/filech/ixopen,ixunit,ixpunit
      common/prec8/slat,slon
      common/sangl1/sazi1,salt1
      common/shad1/isunup
      common/wins/isun(mt),shd(mt)

      dimension shdx(20),mdays(12),month(12)
      character outs*124,month*3
      logical ok,dok

C For help messages
      character helpinsub*24 ! subroutine name
      character helptopic*24 ! string (unique) for topic
      integer nbhelp     ! number of help lines found

      data month/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &          'Sep','Oct','Nov','Dec'/
      data mdays/31,28,31,30,31,30,31,31,30,31,30,31/

C If output to file, alter the edisp unit number.
      helpinsub='winshd'  ! set for subroutine
      itru = iuout
      if(ixopen.eq.1)then
        itru = ixunit
        call edisp(iuout,' ')
        call edisp(iuout,'Output directed to file.')
      endif

      helptopic='ashrae_window_calcs'
      call gethelptext(helpinsub,helptopic,nbhelp)
      shdx(1)=1.0
      call easkr(shdx(1),' ','Window width?',
     &   0.0,'F',99.0,'W',2.0,'window width',ier,nbhelp)
 
      shdx(2)=1.0
      call easkr(shdx(2),' ','Window height?',
     &   0.0,'F',99.0,'W',1.5,'window height',ier,nbhelp)

C Overhang data.
      shdx(3)=0.0
      shdx(4)=0.0
      shdx(5)=0.0
      shdx(6)=0.0
      shdx(7)=0.0

      dok=.false.
      call askok(' ','Is there an overhang?',ok,dok,nbhelp)
      if(.not.ok)goto 108

      shdx(3)=0.25
      call easkr(shdx(3),'Overhang:','depth?',
     &   0.0,'F',99.0,'W',0.25,'Overhang: depth',ier,nbhelp)

      shdx(4)=0.1
      call easkr(shdx(4),'Overhang:',
     &   'distance from top of window?',
     &   0.0,'F',99.0,'W',0.1,'Overhang: dist from top',ier,nbhelp)

      shdx(5)=0.1
      call easkr(shdx(5),'Overhang:',
     &   'extension past left window edge?',
     &   0.0,'F',99.0,'W',0.1,'Overhang: exten left',ier,nbhelp)

      shdx(6)=0.1
      call easkr(shdx(6),'Overhang:',
     &   'extension past right window edge?',
     &   0.0,'F',99.0,'W',0.1,'Overhang: exten right',ier,nbhelp)

      shdx(7)=0.1
      call easkr(shdx(7),'Overhang:',
     &   'dimension of end vertical feature?',
     &   0.0,'F',99.0,'W',0.1,'Overhang: vert feature',ier,nbhelp)
  108 shdx(8)=0.0
      shdx(9)=0.0
      shdx(10)=0.0
      shdx(11)=0.0

      dok=.false.
      call askok(' ','Is there a left-side fin?',ok,dok,nbhelp)
      if(.not.ok)goto 103

      shdx(8)=0.1
      call easkr(shdx(8),'Left side fin:','depth?',
     &   0.0,'F',99.0,'W',0.1,'Left fin depth',ier,nbhelp)

      shdx(9)=0.1
      call easkr(shdx(9),'Left side fin:',
     &  'extension above top of window?',
     &   0.0,'F',99.0,'W',0.1,'Left fin ext top',ier,nbhelp)

      shdx(10)=0.1
      call easkr(shdx(10),'Left side fin:',
     &  'distance from window edge?',
     &   0.0,'F',99.0,'W',0.1,'Left fin dis win edge',ier,nbhelp)

      shdx(11)=0.1
      call easkr(shdx(11),'Left side fin:',
     &  'stop-short distance from window bottom?',
     &   0.0,'F',99.0,'W',0.1,'Left fin stop short',ier,nbhelp)
  103 shdx(12)=0.0
      shdx(13)=0.0
      shdx(14)=0.0
      shdx(15)=0.0

      dok=.false.
      call askok(' ','Is there a right-side fin?',ok,dok,nbhelp)
      if(.not.ok)goto 71

      shdx(12)=0.1
      call easkr(shdx(12),'Right side fin:','depth?',
     &   0.0,'F',99.0,'W',0.1,'Right fin depth',ier,nbhelp)

      shdx(13)=0.1
      call easkr(shdx(13),'Right side fin:',
     &  'extension above top of window?',
     &   0.0,'F',99.0,'W',0.1,'R fin ext top',ier,nbhelp)

      shdx(14)=0.1
      call easkr(shdx(14),'Right side fin:',
     &  'distance from window edge?',
     &   0.0,'F',99.0,'W',0.1,'R fin dis win edge',ier,nbhelp)

      shdx(15)=0.1
      call easkr(shdx(15),'Right side fin:',
     &  'stop-short distance from window bottom?',
     &   0.0,'F',99.0,'W',0.1,'R fin stop short',ier,nbhelp)

  71  wazi=180.
      helptopic='ashrae_window_orien'
      call gethelptext(helpinsub,helptopic,nbhelp)
      call easkr(wazi,' ','Window azimuth?',
     &   0.0,'F',360.0,'F',180.0,'azimuth',ier,nbhelp)

C Convert to radians relative to south.
      pi = 4.0 * atan(1.0)
      r=pi/180.
      wazi=(wazi-180.)*r
      shdx(16)=wazi

      im=1
      call easki(im,' ','Computation month number?',
     &    1,'F',12,'F',1,'comp month',ier,nbhelp)
      id=15
      call easki(id,' ','Day of the month?',
     &     1,'F',mdays(im),'F',15,'comp day',ier,nbhelp)

C Commence hour-by-hour computation.
      call eday(id,im,iyd)
      do 10 i=1,24
        ii=i
        stime=float(ii)
        call eazalts(stime,iyd,slat,slon,isunup,sazi,salt)
        sazi1=sazi
        salt1=salt
        isun(i)=isunup
        if(isunup.eq.0)goto 10
        phi=(sazi1-180.)*r
        cosz=cos((90.-salt1)*r)
        call wshad(shdx,phi,cosz,shrat)

C 'shrat' is insolated portion.
        shd(i)=1.-shrat
   10 continue

      call edisp(itru,' ')
      call edisp(itru,'ASHRAE window shading algorithm')
      write(outs,85)id,month(im)
   85 format('results for ',i2,1x,a3)
      call edisp(itru,outs)
      call edisp(itru,' Hour   Direct')
      call edisp(itru,'        Shading')
      iprint=0
      do 20 i=1,24
        if(isun(i).eq.0)then			! Sun not up.
          if(iprint.eq.2)then
             write(outs,'(i2,a)')i,':00   After sun-set'
             call edisp(itru,outs)
C Don't write again.
             iprint=-1
          else					! Sun up.
            iprint=1
          endif
        else
          if(iprint.eq.1)then
             write(outs,'(i2,a)')i-1,':00   Before sun-rise'
             call edisp(itru,outs)
          endif
          iprint=2
          write(outs,'(i2,a,1x,f7.2,a)')i,':00',shd(i)*100.0,'%'
          call edisp(itru,outs)
         endif
   20 continue
      return
      end

C ********* WSHAD
C Calculates shadow cast by overhang and side fins using ASHRAE
C algorithm.

C phi   - solar azimuth angle
C cosz  - cosine of solar zenith angle
C shrat - shade ratio: ratio of the sunlit area to the total window 
C         area
C ht    - window height
C fl    - window width
C fp    - depth of the overhang
C aw    - distance from the top of the window to the overhang
C bwl   - distance of the overhang extended beyond the left edge 
C         of the window
C bwr   - distance of the overhang extended beyond the right edge 
C         of the window
C d     - depth of vertical projection at the end of the overhang
C fp1   - depth of the left fin
C a1    - distance of the left fin extended above the top of the 
C         window
C b1    - distance from the left edge of the window to the left fin
C c1    - distance of the left fin stop short above the bottom of 
C         the window
C fp2   - depth of the right fin
C a2    - distance of the right fin extended above the top of the 
C         window
C b2    - distance from the right edge of the window to the right 
C         fin
C c2    - distance of the right fin stop short above the bottom 
C         of the window
C wazi  - window azimuth angle

      subroutine wshad(shdx,phi,cosz,shrat)

      dimension shdx(20)
      fl=shdx(1)
      ht=shdx(2)
      fp=shdx(3)
      aw=shdx(4)
      bwl=shdx(5)
      bwr=shdx(6)
      d=shdx(7)
      fp1=shdx(8)
      a1=shdx(9)
      b1=shdx(10)
      c1=shdx(11)
      fp2=shdx(12)
      a2=shdx(13)
      b2=shdx(14)
      c2=shdx(15)
      wazi=shdx(16)
      shrat=1.

      a=aw
      h=ht
      gamma=phi-wazi
      cosg=cos(gamma)
      if(cosg)100,100,104
  100 shrat=0.0
      goto 2000
  104 continue
      sbeta=cosz
      if(sbeta)100,100,152
  152 sing=sin(gamma)
      vert=sbeta/sqrt(1.0-sbeta*sbeta)/cosg
      horiz=abs(sing)/cosg
      tceta=vert/horiz
      if(gamma)155,154,154

C Sun on left.
  154 b=bwl
      goto 156

C Sun on right.
  155 b=bwr
  156 arshf=0.0
      areav=0.0
      arsif=0.0
      areao=0.0
      area1=0.0
      arsh1=0.0
      fl3=0.0
      h3=0.0
      h1=H
      fl1=FL
      k=1
      l=1
      t1=fp*vert
      fm1=fp*horiz
      if(fp)37,37,153
  153 t=t1
      fm=fm1
      ab=b*tceta
      ug=(fl+b)*tceta
      de=(h+a)/tceta

C Horizontal overhang 'AREAO'.
      if(t-a)27,27,2
    2 if(ab-a)14,14,3
    3 if(de-b)12,12,4
    4 if(fm-b)11,11,5
    5 if(de-(fl+b))8,8,6
    6 if(fm-(fl+b))9,7,7

C Horizontal 9.
    7 areao=fl*(0.5*(ab+ug)-a)
      goto 37
    8 if(t-(h+a))9,10,10

C Horizontal 7.
    9 areao=(t-a)*fl-((fm-b)**2)*tceta*0.5
      l=2
      goto 21

C Horizontal 8.
   10  areao=h*fl-(de-b)**2*tceta*0.5
      goto 37

C Horizontal 3.
   11 areao=fl*(t-a)
      l=2
      goto 24
   12 if(t-(h+a))11,13,13

C Horizontal 2.
   13 areao=h*fl
      goto 68
   14 if(ug-a)27,27,15
   15 if(de-(fl+b))18,18,16
   16 if(fm-(fl+b))20,17,17

C Horizontal 6.
   17 areao=(ug-a)**2/tceta*0.5
      goto 37
   18 if(t-(h+a))20,19,19

C Horizontal 5.
   19 areao=h*(fl-(a+0.5*h)/tceta+b)
      goto 37

C Horizontal 4.
   20 areao=(t-a)*(fl+b-fm*(1.0+a/t)*0.5)
      l=2

C Vertical projection 'AREAV'.
   21 fl3=fl+b-fm
      if(t+d-(h+a))22,22,23

C Vertical 8.
   22 h3=d
      goto 3700

C Vertical 9.
   23 h3=h+a-t
      goto 3700
   24 fl3=fl
      if(t+d-(h+a))26,26,25

C Vertical 7.
   25 h3=h+a-t
      areav=h3*fl3
      goto 68

C Vertical 6.
   26 h3=d
      goto 3700
   27 if(t+d-a)37,37,28
   28 if(fm-b)34,34,29

C fm not defined!
   29 if(fm-(fl+b))31,37,37
   31 fl3=fl+b-fm
      if(t+d-(h+a))33,33,32

C Vertical 5.
   32 h3=h
      goto 3700

C Vertical 4.
   33 h3=t+d-a
      goto 3700
   34 if(t+d-(h+a))36,35,35

C Vertical 2.
   35 areav=h*fl
      goto 68

C Vertical 3.
   36 h3=t+d-a
      fl3=fl
 3700 areav=fl3*h3

C Side fin and short side fin.
C Side fin 'area1', 'arsif'
   37 if(gamma)66,68,74
   74 fpf=fp1
      af=a1
      bf=b1
      cx=c1
      goto 84
   66 fpf=fp2
      af=a2
      bf=b2
      cx=c2
   84 if(fpf)68,68,67
   67 t=fpf*vert
      fm=fpf*horiz
      af1=af
      if(areao)73,73,88

C Test for overlap of fin and overhang shadow.
   88 at=a+(bf-b)*tceta
      if(at-af)711,73,73

C Overlap exists.
C Set l=2 if overhang shadow has horizontal edge in window.
  711 goto (621,712),l

C Test for type of overlap.
  712 if((fm-bf)-(fm1-b))621,622,622

C Set l=1, shadow intersects on inclined edge of overhang shadow.
C Fin shadow is below the inclined edge of overhang shadow.
  621 af=at
      l=1
      goto 73

C l is 2, horizontal edge of overhang shadow - portion above 
C horizontal edge not in overhang shadow is fin shadow.
  622 area1=fl*(t1-a)-areao

C Reset to calculate fin shadow below horizontal edge 
C of overhang shadow.
      af=t1-a+af1
      h=h+af1-af

C Shadow of fin (K=1 on glass, K=2 on vertical projection shadow).
   73 ab=bf*tceta
      ug=(fl+bf)*tceta
      de=(h+af)/tceta
      dj=cx/tceta
      if(fm-bf)69,69,38
   38 if(ab-af)39,50,50
   39 if(ug-af)48,48,40
   40 if(t-af)47,47,41
   41 if(ug-(h+af))44,44,42
   42 if(t-(h+af))91,80,80

C Fin 9.
   80 area1=h*((af+h*0.5)/tceta-bf)+area1
      goto 58
   44 if(fm-(fl+bf))91,89,89

C Fin 8.
   89 area1=h*fl-(ug-af)**2/tceta*0.5+area1
      goto 58

C Fin 7.
   91 area1=(fm-bf)*h-(t-af)**2/tceta*0.5+area1
      goto 63
   48 if(fm-(fl+bf))47,47,49

C Fin 3.
   47 area1=h*(fm-bf)+area1
      goto 63

C Fin 2.
   49 area1=h*fl+area1
      goto 58
   50 if(de-bf)69,69,51
   51 if(ug-(h+af))55,55,52
   52 if(t-(h+af))93,94,94

C Fin 6.
   94 area1=(de-bf)**2*tceta*0.5+area1
      goto 58

C Fin 4.
   93 area1=(fm-bf)*(h+af-(t+ab)*0.5)+area1
      goto 63
   55 if(fm-(fl+bf))93,99,99

C Fin 5.
   99 area1=fl*(h-(bf+fl*0.5)*tceta+af)+area1

C Short side fin 'arsh1','arshf'
   58 if(dj-bf)69,69,59
   59 if(dj-(fl+bf))61,61,60

C Short 3.
   60 arsh1=-fl*(cx-(bf+fl/2)*tceta)
      goto 69

C Short 4.
   61 arsh1=-(cx-ab)**2/tceta*0.5
      goto 69
   63 if(dj-bf)69,69,64
   64 if(dj-fm)61,61,65

C Short 2.
   65 arsh1=-(fm-bf)*(cx-(t+ab)*0.5)
   69 goto (77,76),k
   76 arsh1=-arsh1
      area1=-area1
   77 arshf=arshf+arsh1
      arsif=arsif+area1
      goto (78,68),k
   78 if(areav)68,68,72

C Reset parameters to deduct fin shadow overlap on 
C vertical projection shadow.
   72 k=2
      area1=0.0
      arsh1=0.0
      bbf=bf
      bf=fm1-b+bf
      if(bf)186,185,185
  186 bf=bbF
  185 if(ht+a-t1-d)87,87,188
  188 cx=cx-(ht+a-t1-d)
      if(cx)85,87,87
   85 cx=0.0
   87 af=t1-a+af
      h=h3
      fl=fl3
      goto 73

C Shaded area 'arsha'
   68 arsha=areao+areav+arshf+arsif
      shrat=(fl1*h1-arsha)/(fl1*h1)
      fl=fl1
 2000 continue
      return
      end
