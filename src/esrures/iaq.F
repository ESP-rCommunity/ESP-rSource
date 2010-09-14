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
C iaqmenu
C mycotox

C Iaqmenu controls indoor environmental quality choices.  All Mycotoxin
C facilities are 'hidden' in the current version.
      SUBROUTINE iaqmenu

      COMMON/OUTIN/IUOUT,IUIN
      COMMON/OUTPCH/ICOUT
      common/pophelp/h(60)
      COMMON/GRTOOL/IHFLAG,IDHFLG,ILFLAG
      common/exporttg/xfile,tg,delim
      COMMON/EXPORTI/ixopen,ixunit,ixpunit
      COMMON/SIMPIK/ISIM,ISTADD,ID1,IM1,ID2,IM2,ISDS,ISDF,NTS,ISAVE
      
      integer ncomp,ncon
      COMMON/C1/NCOMP,NCON

      CHARACTER*23 ITEM(12)
      character doit*124,tmode*8
      character H*72,xfile*144,tg*1,delim*1
      integer NITMS,INO ! max items and current menu item

#ifdef OSI
      integer ICPMOD     ! passed from tchild
#else
      integer*8 ICPMOD     ! passed from tchild
#endif

C Default value for xvgr file = 0 (off).
      IHFLAG=0

C Setup for menu.
    3 NITMS=12
      ITEM(1)   ='2 select result set    '
      ITEM(2)   ='3 define output period '
      ITEM(3)   ='4 select zones         '
      ITEM(4)   ='  ___________________  '
      ITEM(5)   ='a comfort              '
      ITEM(6)   ='b mycotoxin            '
      ITEM(7)   ='d glare                '
      ITEM(8)   ='  ___________________  '
      if(ixopen.eq.1)then
        ITEM(9) ='> output >> file       '
      elseif(ixopen.eq.0)then
        ITEM(9) ='> output >> screen     '
      endif
      if(IHFLAG.eq.0)then
        ITEM(10)='* time >> 10h30      '
      elseif(IHFLAG.eq.1)then
        ITEM(10)='* time >> 0.4375     '
      endif

C << put ILFLAG toggle here >>
      ITEM(11)  ='? help                 '
      ITEM(12)  ='- exit                 '

C Instanciate the h() strings for this menu.
      H(1) ='The output format selection allows you to direct '
      H(2) ='the listings to the standard text feedback area '
      H(3) ='or to a named file.  '
      H(4) ='  '
      H(5) ='The comfort choice allows you to look at changes '
      H(6) ='in comfort over time.  Fangers PMV method is used. '
C      H(7) ='  '
C      H(8) ='The pollution level reporting, when implemented will'
C      H(9) ='track concentrations over time.  '
C      H(10) ='  '
C      H(11)='The Mycotoxin reporting will ...... '
C      H(12)='    '

      INO=-2
      CALL EMENU(' Indoor Quality',ITEM,NITMS,INO)

      IF(INO.EQ.1)then
        CALL MORESS
      elseif(INO.EQ.2)then
        CALL MOOPER
      elseif(INO.EQ.3)then
        CALL MOZDFN
      elseif(INO.EQ.5.AND.ISAVE.GT.1)then
        IF(NCOMP.GT.1)then
          call askzon(IZONE)
        else
          IZONE=NCOMP
        endif
        CALL EDAY(ID1,IM1,IDAY)
        CALL MOCMFT(IDAY,IZONE,'V')
      elseif(INO.EQ.6)then
        call edisp(iuout,' ')
        doit = ' '
        call tchild(ICPMOD)
        call termode(ICPMOD,tmode)
        write(doit,'(a,a,a,a,a)') 'mld -mode ',tmode,' &'
        call runit(doit,tmode)
      elseif(INO.EQ.7)then
        call edisp(iuout,' ')

C If there are associated glare images and the image browser
C has not been invoked do this now.
          call imgdisp(0,'FGLR',ier)

C Section on Bioscience to be 'hidden'.
C        call edisp(iuout,' ')
C        call edisp(iuout,'   Bioscience Data')
C        call edisp(iuout,
C     &  ' ------------------------------------------------------')
C        call edisp(iuout,
C     &  ' Min aw            12 deg C               25 deg C')
C        call edisp(iuout,
C     &  ' <0.80         Aspergillus repens    Aspergillus repens')
C        call edisp(iuout,
C     &  ' (Primary      P. brevicompactum     P. brevicompactum ')
C        call edisp(iuout,
C     &  '  colonizers)  P. chrysogenum        P. chrysogenum')
C        call edisp(iuout,
C     &  '                                     Aspergillus versicolor')
C        call edisp(iuout,' ')
C        call edisp(iuout,
C     &  ' 0.80-0.90     A. versicolor         Aspergillus repens')
C        call edisp(iuout,
C     &  ' (Secondary    A. pullulans      Cladospaoium cladosporioides')
C        call edisp(iuout,
C     &  '  colonizers)  C. cladosporioides    C. sphaerospermum')
C        call edisp(iuout,
C     &  '               C. sphaerospernm      Geomyces pannorum')
C        call edisp(iuout,
C     &  '               Geomyces pannorum     Ulocladium chartarum')
C        call edisp(iuout,
C     &  '               Ulocladium chartarum  U. comsortuale')
C        call edisp(iuout,' ')
C        call edisp(iuout,
C     &  ' >0.90         Phoma herbarum        Phoma herbarum')
C        call edisp(iuout,
C     &  ' (Tertiary     Stachybotrys atra     Stachybotrys atra ')
C        call edisp(iuout,
C     &  '  colonizers)  U. consortiale  ')
C        call edisp(iuout,' ')
C        call edisp(iuout,' ')
C        call edisp(iuout,'     Engineering Data')
C        call edisp(iuout,
C     &    ' ------------------------------------------------------')
C        call edisp(iuout,' ')

C Get air temperature and relative humidity.
C        CALL MOMXMN(3)
C        CALL MOMXMN(14)
      elseif(INO.EQ.9)then
        call ctlexp(xfile,ixopen,ixunit,ixpunit,'T','IEQ',IER)
      elseif(INO.EQ.10)then
        IHFLAG=IHFLAG+1
        if(IHFLAG.GT.1)IHFLAG=0
        if(IHFLAG.eq.0)then
          call edisp(iuout,' ')
          call edisp(iuout,'standard display time = 10h30 set ')
        elseif(IHFLAG.eq.1)then
          call edisp(iuout,' ')
          call edisp(iuout,'3rd party graphing time = day fraction set')
        endif
      elseif(INO.EQ.11)then
        CALL PHELPD('iaq_page',6,'-',0,0,IER)
      elseif(INO.EQ.12)then
        return
      else
        INO=-1
        goto 3
      endif
      goto 3
      end

