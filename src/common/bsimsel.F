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

C ***** select BASESIMP type
C bsimtype is based on data from BASESIMP, A Simplified Foundation  
C Energy-Loss Model Derived from BASECALC (TM) Simultions
C Ian Beausoleil-Morrison, CANMET Energy Technology Centre,
C Natural Resources Canada, 1996, 1998 1999.
C ibst (integer) is the index of the user selection to be returned

      subroutine bsimtype(ibst)
#include "help.h"
      dimension bl(145),IVAL(145)
      character bl*65,outs*72
      logical ok
      integer ibst  ! index of user selection

      helpinsub='bsimsel'  ! set for subroutine
      helptopic='bsimtype_selection'
      call gethelptext(helpinsub,helptopic,nbhelp)

C Build the selection list.
      write(bl(1),'(a)')
     &'BCIN_1 conc Wl&Fl; intrn wall ins full-height; *any constr'
      write(bl(2),'(a)')
     &'BCIN_2 conc Wl&Fl; intrn ins to 0.2 above floor; *any constr'
      write(bl(3),'(a)')
     &'BCIN_3 conc Wl&Fl; intrn ins to 0.6 below grade; *brick veneer'
      write(bl(4),'(a)')
     &'BCIN_4 conc Wl&Fl; intrn to 0.6 below grade; *non-brick veneer'
      write(bl(5),'(a)')
     &'BCEN_1 conc Wl&Fl; extrn wall ins full-height; *brick veneer'
      write(bl(6),'(a)')
     &'BCEN_2 conc Wl&Fl; extrn wall ins full-height; *non-brick veneer'
      write(bl(7),'(a)')
     &'BCEN_3 conc Wl&Fl; extrn wall ins below grade; *brick veneer'
      write(bl(8),'(a)')
     &'BCEN_4 conc Wl&Fl; extrn ins below grade; *non-brick veneer'
      bl(9)='BCNN_1 conc Wl&Fl; no wall or floor ins; *brick veneer'
      write(bl(10),'(a)')
     &'BCNN_2 conc Wl&Fl; no wall or floor ins; *non-brick veneer'
      write(bl(11),'(a)')
     &'BCCN_1 conc Wl&Fl; extrn bel_grd with overlap; *brick on wall'
      write(bl(12),'(a)')
     &'BCCN_2 conc Wl&Fl; extrn bel_grd with overlap; *non-brick'
      bl(13)='BWNN_1 wood Wl&Fl; no wall or floor ins; *any constr'
      write(bl(14),'(a)')
     &'BWIN_1 wood Wl&Fl; intrn wall ins full-height; *any constr'
      write(bl(15),'(a)')
     &'BWIN_2 wood Wl&Fl; intrn ins to 0.2 above floor; *any constr'
      write(bl(16),'(a)')
     &'BWIN_3 wood Wl&Fl; intrn ins to 0.6 below grade; *any constr'
      write(bl(17),'(a)')
     &'BWEN_1 wood Wl&Fl; extrn wall ins full-height; *any constr'
      write(bl(18),'(a)')
     &'BWEN_2 wood Wl&Fl; extrn wall ins below grade; *any constr'
      write(bl(19),'(a)')
     &'BCIB_1 conc Wl&Fl; intrn ins full; ins sub-surface; *any constr'
      write(bl(20),'(a)')
     &'BCIB_2 conc Wl&Fl; intrn full; 0.6m perm floor ins; *any constr'
      write(bl(21),'(a)')
     &'BCIB_3 conc Wl&Fl; intrn full; 1.0m perm floor ins; *any constr'
      write(bl(22),'(a)')
     &'BCIB_4 conc Wl&Fl; intrn ins full; full floor ins; *any constr'
      write(bl(23),'(a)')
     &'BCIB_5 conc Wl&Fl; intrn ins full; 0.6m perm floor ins; TB; *any'
      write(bl(24),'(a)')
     &'BCIB_6 conc Wl&Fl; intrn ins full; 1.0m perm floor ins; TB; *any'
      write(bl(25),'(a)')
     &'BCEB_1 conc Wl&Fl; extrn ins full; full floor ins; *any constr'
      write(bl(26),'(a)')
     &'BCEB_2 conc Wl&Fl; extrn full; 0.6m perm floor ins; *any constr'
      write(bl(27),'(2a)')
     &'BCEB_3 conc Wl&Fl; extrn full; 1.0m perm floor ins; *any constr'
      bl(28)='SCN_1 conc/soil FL; no wall/floor ins; *non-brick veneer'
      bl(29)='SCN_2 conc/soil FL; no wall/floor ins; *brick on slab'
      write(bl(30),'(a)')
     &'SCN_3 conc/soil FL; no wall/floor ins; TB; *non-brick veneer'
      bl(31)='SCN_4 conc/soil FL; no wall/floor ins; TB; *brick veneer'
      write(bl(32),'(a)')
     &'SCN_7 conc/soil FL; no wall/floor ins; vert skirt; *non-brick'
      write(bl(33),'(2a)')
     &'SCN_8 conc/soil FL; no wall/floor ins; vert skirt; TB; ',
     &'*brick vnr'
      write(bl(34),'(a)')
     &'SCB_1 conc/soil FL; 0.6m perm slab ins; *non-brick veneer'
      write(bl(35),'(a)')
     &'SCB_2 conc/soil FL; 0.6m perm slab ins; *brick on slab'
      write(bl(36),'(a)')
     &'SCB_3 conc/soil FL; 0.6m perm slab & footing ins; *non-brick'
      write(bl(37),'(a)')
     &'SCB_4 conc/soil FL; 0.6m perm slab & footing ins; *brick on slab'
      write(bl(38),'(2a)')
     &'SCB_5 conc/soil FL; no slab or footing ins; TB; ',
     &'*non-brick veneer'
      write(bl(39),'(a)')
     &'SCB_6 conc/soil FL; no slab or footing ins; TB; *brick on slab'
      write(bl(40),'(a)')
     &'SCB_9 conc/soil FL; 0.6m perm slab; TB; vert skirt; *non-brick'
      write(bl(41),'(a)')
     &'SCB_10 conc/soil FL; 0.6m perm slab; TB; vert skirt; *brick slab'
      write(bl(42),'(a)')
     &'SCB_11 conc/soil FL; 0.6m perm slab; horiz skirt; *non-brick'
      write(bl(43),'(a)')
     &'SCB_12 conc/soil FL; 0.6m perm slab; horiz skirt; *brick on slab'
      write(bl(44),'(a)')
     &'SCB_13 conc/soil FL; 1.0m perm slab; horiz skirt; *brick on slab'
      write(bl(45),'(a)')
     &'SCB_14 conc/soil FL; 1.0m perm slab ins; *brick on slab'
      write(bl(46),'(a)')
     &'SCB_17 conc/soil FL; 1.0m perm slab ins; TB; *non-brick veneer'
      write(bl(47),'(a)')
     &'SCB_18 conc/soil FL; 1.0m perm slab ins; TB; *brick on slab'
      write(bl(48),'(a)')
     &'SCB_21 conc/soil FL; 1.0m perm slab; TB; vert skirt; *non-brick'
      write(bl(49),'(a)')
     &'SCB_22 conc/soil FL; 1.0m perm slab; TB; vert skirt; *brick slab'
      write(bl(50),'(a)')
     &'SCB_23 conc/soil FL; 1.0m perm slab; TB; horiz skirt; *non-brick'
      write(bl(51),'(a)')
     &'SCB_24 conc/soil FL; 1.0m perm slab; TB; horiz skirt; *brick slb'
      write(bl(52),'(a)')
     &'SCB_25 conc/soil FL; full slab ins; *non-brick veneer'
      write(bl(53),'(a)')
     &'SCB_26 conc/soil FL; full slab ins; *brick on slab'
      write(bl(54),'(a)')
     &'SCB_29 conc/soil FL; full slab ins; TB; *non-brick veneer'
      write(bl(55),'(a)')
     &'SCB_30 conc/soil FL; full slab ins; TB; *brick on slab'
      write(bl(56),'(a)')
     &'SCB_33 conc/soil FL; full slab ins; TB; vert skirt; *non-brick'
      write(bl(57),'(a)')
     &'SCB_34 conc/soil FL; full slab; TB; vert skirt; *brick on slab'
      write(bl(58),'(a)')
     &'SCB_35 conc/soil FL; full slab; horiz skirt; *non-brick veneer'
      write(bl(59),'(a)')
     &'SCB_36 conc/soil FL; full slab ins; horiz skirt; *brick on slab'
      write(bl(60),'(a)')
     &'SCA_17 conc/soil FL; full top slab ins; *non-brick veneer'
      write(bl(61),'(a)')
     &'SCA_18 conc/soil FL; full top slab ins; *brick on slab'
      write(bl(62),'(a)')
     &'SCA_19 conc/soil FL; full top slab ins; TB; *non-brick veneer'
      write(bl(63),'(a)')
     &'SCA_20 conc/soil FL; full top slab ins; TB; *brick on slab'
      write(bl(64),'(a)')
     &'SCA_21 conc/soil FL; full top slab; TB; vert skirt; *non-brick'
      write(bl(65),'(a)')
     &'SCA_22 conc/soil FL; full top slab; TB; vert skirt; *brick slab'
      write(bl(66),'(a)')
     &'SCA_23 conc/soil FL; full top slab; TB; horiz skirt; *non-brick'
      write(bl(67),'(a)')
     &'SCA_24 conc/soil FL; full top slab; TB; horiz skirt; brick slab'
      write(bl(68),'(a)')
     &'BCCN_3 conc Wl&Fl; no slab ins; TB; full ins both sides; *any'
      write(bl(69),'(a)')
     &'BCCN_4 conc Wl&Fl; full slab ins; TB; full ins both sides; *any'
      write(bl(70),'(a)')
     &'BCEA_1 conc Wl&Fl; full top slab; extrn ins full; *brick veneer'
      write(bl(71),'(a)')
     &'BCEA_4 conc Wl&Fl; full top slab ins; extrn ins full; *non-brick'
      write(bl(72),'(a)')
     &'BCIA_1 conc Wl&Fl; full top slab; intern ins full; *brick veneer'
      write(bl(73),'(a)')
     &'BCIA_4 conc Wl&Fl; full top slab; intern ins full; *non-brick'
      write(bl(74),'(a)')
     &'BCEA_7 conc Wl&Fl; full top slab; extrn ins blw_grd; *brick slab'
      write(bl(75),'(a)')
     &'BCEA_8 conc Wl&Fl; full top slab ins; extrn blw_grd; *non-brick'
      write(bl(76),'(a)')
     &'BCEB_8 conc Wl&Fl; full slab ins; extrn ins blw_grd; *brick venr'
      write(bl(77),'(a)')
     &'BCEB_9 conc Wl&Fl; full slab; TB; extrn 0.6 blw_grd; *non-brick'
      write(bl(78),'(a)')
     &'BWEN_3 wood Wl&Fl; extrn wall ins to 0.6 blw_grd; *any constr'
      write(bl(79),'(a)')
     &'BBIB_3 wood Wl conc Fl; full slab; TB; intrn full; *any constr'
      write(bl(80),'(a)')
     &'BBIB_4 wood Wl conc Fl; full slab; TB; intrn 0.6 blw_grd; *brick'
      write(bl(81),'(a)')
     &'SCB_31 conc/soil FL; full slab & footings; TB; *non-brick veneer'
      write(bl(82),'(a)')
     &'SCB_32 conc/soil FL; full slab & footings; TB; *brick on slab'
      write(bl(83),'(a)')
     &'SCB_37 conc/soil FL; 0.35m perm slab & footings; TB; *non-brick'
      write(bl(84),'(a)')
     &'SCB_38 conc/soil FL; 0.35m perm slab & footings; TB; *brick slab'
      write(bl(85),'(a)')
     &'SCB_39 conc/soil FL; 0.75m perm slab & footings; TB; *non-brick'
      write(bl(86),'(a)')
     &'SCB_40 conc/soil FL; 0.75m perm slab & footings; TB; *brick slab'
      write(bl(87),'(a)')
     &'BBEB_1 wood Wl conc Fl; full slab; extrn 0.6 blw_grd; *brick wal'
      write(bl(88),'(a)')
     &'BBEN_1 wood Wl conc Fl; no slab ins; extrn full; *brick on wall'
      write(bl(89),'(a)')
     &'BBEN_2 wood Wl conc Fl; no slab; extrn 0.6 blw_grd; *brick wall'
      write(bl(90),'(a)')
     &'BBIA_2 wood Wl conc Fl; top slab; intrn ins full; *any constr'
      write(bl(91),'(a)')
     &'BBIN_2 wood Wl conc Fl; intrn to 0.6 blw_grd; *non-brick veneer'
      write(bl(92),'(a)')
     &'BCCB_8 conc Wl&Fl; 0.6m perm slab; intrn&extrn ins full; *any'
      write(bl(93),'(a)')
     &'BCCA_7 conc Wl&Fl; top slab; intrn full; ext 0.6 blw_grd; *any'
      write(bl(94),'(a)')
     &'BCCA_8 conc Wl&Fl; top slab; intrn above 0.2; extrn full; *any'
      write(bl(95),'(a)')
     &'BCCN_4 conc Wl&Fl; intrn full; extrn 0.6 blw_grd; *any constr'
      write(bl(96),'(a)')
     &'BCCN_5 conc Wl&Fl; intrn top to 0.2; extrn ins full; *any constr'
      write(bl(97),'(a)')
     &'BCEA_5 conc Wl&Fl; top slab; extrn 0.6 blw_grd; *brick veneer'
      write(bl(98),'(a)')
     &'BCEA_6 conc Wl&Fl; top slab; extrn 0.6 blw_grd; *non-brick vener'
      write(bl(99),'(a)')
     &'BCEB_4 conc Wl&Fl; full slab ins; extrn full; *non-brick veneer'
      write(bl(100),'(a)')
     &'BCEB_5 conc Wl&Fl; 0.6m perm slab; extrn full; *non-brick veneer'
      write(bl(101),'(a)')
     &'BCEB_6 conc Wl&Fl; 1.0m perm slab; extrn full; *non-brick veneer'
      write(bl(102),'(a)')
     &'BWIA_1 wood Wl&Fl; 0.6m perm top floor; intrn full; *any constr'
      write(bl(103),'(a)')
     &'BWIA_2 wood Wl&Fl; top floor ins; intrn wall full; *any constr'
      write(bl(104),'(a)')
     &'SCA_1 conc/soil FL; 0.6m perm top slab ins; *non-brick veneer'
      write(bl(105),'(a)')
     &'SCA_2 conc/soil FL; 0.6m perm top slab ins; *brick on slab'
      write(bl(106),'(a)')
     &'SCA_9 conc/soil FL; 1.0m perm top slab ins; *non-brick veneer'
      write(bl(107),'(a)')
     &'SCA_10 conc/soil FL; 1.0m perm top slab ins; *brick on slab'
      write(bl(108),'(a)')
     &'BBIN_1 wood Wl conc Fl; intrn wall ins full; *non-brick veneer'
      write(bl(109),'(a)')
     &'BCEN_5 conc Wl&Fl; extrn wall ins to 0.6 blw_grd; *brick on wall'
      write(bl(110),'(a)')
     &'BCEN_6 conc Wl&Fl; extrn ins to 0.6 blw_grd; *non-brick veneer'
      write(bl(111),'(a)')
     &'BBIA_1 wood Wl conc Fl; top slab ins; intrn full; *any constr'
      write(bl(112),'(a)')
     &'BBIB_1 wood Wl conc Fl; 0.6m perm slab; intrn full; *any constr'
      write(bl(113),'(a)')
     &'BBIB_2 wood Wl conc Fl; full slab ins; intrn full; *any constr'
      write(bl(114),'(a)')
     &'BCCB_9 conc Wl&Fl; full slab; intrn full; extr 0.6 blw_grd; *any'
      write(bl(115),'(a)')
     &'BCCB_10 conc Wl&Fl; 0.6m perm; intr full; extr 0.6 blw_grd; *any'
      write(bl(116),'(a)')
     &'BCCA_1 conc Wl&Fl; full top; int overlap; extrn blw_grd; *brick'
      write(bl(117),'(a)')
     &'BCCA_4 conc Wl&Fl; full top; int overlap; ext blw_grd; *non-brck'
      write(bl(118),'(a)')
     &'BCIB_7 conc Wl&Fl; full slab; TB; intrn 0.6 blw_grd; *brick wall'
      write(bl(119),'(a)')
     &'BCIB_8 conc Wl&Fl; full slab; TB; intrn 0.6 blw_grd; *non-brick'
      write(bl(120),'(a)')
     &'BBEB_2 wood W conc F; full slab; TB; extr 0.6 blw_grd; *brick wl'
      write(bl(121),'(a)')
     &'BCIA_3 conc Wl&Fl; 1.0m perm top ins; intrn full; *brick on wall'
      write(bl(122),'(a)')
     &'BCIA_5 conc Wl&Fl; 0.6m perm top; intrn 0.6 blw_grd; *brick wall'
      write(bl(123),'(a)')
     &'BCIA_6 conc Wl&Fl; 0.6m perm top; intrn 0.6 blw_grd; *non-brick'
      write(bl(124),'(a)')
     &'BCIB_9 conc Wl&Fl; 0.6m perm; TB; intrn 0.6 blw_grd; *brick wall'
      write(bl(125),'(a)')
     &'BCIB_10 conc Wl&Fl; 0.6m perm; TB; intrn 0.6 blw_grd; *non-brick'
      write(bl(126),'(a)')
     &'BCEB_10 conc Wl&Fl; 0.6m perm; TB; extrn 0.6 blw_grd; *brick wal'
      write(bl(127),'(a)')
     &'BCEB_11 conc Wl&Fl; 0.6m perm; TB; extrn 0.6 blw_grd; *non-brick'
      write(bl(128),'(a)')
     &'BCEA_3 conc Wl&Fl; 1.0m perm top ins; extrn full; *brick on wall'
      write(bl(129),'(a)')
     &'BCEA_9 conc Wl&Fl; 1.0m perm top ins; extrn full; *non-brick'
      write(bl(130),'(a)')
     &'BCEA_10 conc Wl&Fl; 0.6m perm top; extrn 0.6 blw_grd; *brick wal'
      write(bl(131),'(a)')
     &'BCEA_11 conc Wl&Fl; 0.6m perm top; extrn 0.6 blw_grd; *non-brick'
      write(bl(132),'(a)')
     &'BWIB_1 wood Wl&Fl; 1.0m perm floor; intrn full; *brick on wall'
      write(bl(133),'(a)')
     &'BWIB_2 wood Wl&Fl; full floor ins; intrn full; *brick on wall'
      write(bl(134),'(a)')
     &'BWIB_4 wood Wl&Fl; 0.6m perm floor; intrn full; *brick on wall'
      write(bl(135),'(a)')
     &'BWIA_3 wood Wl&Fl; 0.6m perm top; intrn 0.6 blw_grd; *brick wall'
      write(bl(136),'(a)')
     &'BWEB_1 wood Wl&Fl; 1.0m perm floor; extrn full; *brick on wall'
      write(bl(137),'(a)')
     &'BWEB_2 wood Wl&Fl; full floor ins; extrn full; *brick on wall'
      write(bl(138),'(a)')
     &'BWEB_3 wood Wl&Fl; 0.6m perm floor; extrn 0.6 blw_grd; *brick wl'
      write(bl(139),'(a)')
     &'BWEB_4 wood Wl&Fl; 0.6m perm floor; extrn full; * brick on wall'
      write(bl(140),'(a)')
     &'BBIB_5 wood Wl conc Fl; 1.0m perm; intrn ins full; *brick on wal'
      write(bl(141),'(a)')
     &'BBIB_6 wood Wl conc Fl; 1.0m perm; intrn 0.6 blw_grd; *brick wl'
      write(bl(142),'(a)')
     &'BBEB_3 wood Wl conc Fl; 1.0m perm; extrn full;* brick on wall'
      write(bl(143),'(a)')
     &'BBEB_4 wood Wl conc Fl; 0.6m perm; extrn full; *brick on wall'
      write(bl(144),'(a)')
     &'BBEB_5 wood Wl conc Fl; 0.6m perm; extrn 0.6 blw_grd; *brick wl'
      write(bl(145),'(a)')
     &'BBEA_2 wood Wl conc Fl; full top slab; extrn full; *brick on wal'

C Present list to user, note current value.
  42  IX=1
      helptopic='bsimtype_selection'
      call gethelptext(helpinsub,helptopic,nbhelp)
      if(ibst.ne.0)then
        write(outs,'(3a)') '(currently ',bl(ibst)(1:52),'...)'
      else
        write(outs,'(a)') '(currently nothing defined)'
      endif
      CALL EPICKS(IX,IVAL,'The other side of surface faces: ',outs,
     &  65,144,bl,' Available BSIMP configurations',IER,nbhelp)

      IF(IX.EQ.0)return
      if(IVAL(1).eq.0)then
        return
      else
        IBS = IVAL(1)
      endif

C Provide an expanded description for confirmation.
      if(IBS.ge.1.and.IBS.le.10)then
        helptopic='bsimtype_details1-10'
      elseif(IBS.ge.11.and.IBS.le.20)then
        helptopic='bsimtype_details11-20'
      elseif(IBS.ge.21.and.IBS.lt.30)then
        helptopic='bsimtype_details21-30'
      elseif(IBS.ge.31.and.IBS.lt.40)then
        helptopic='bsimtype_details31-40'
      elseif(IBS.ge.41.and.IBS.lt.50)then
        helptopic='bsimtype_details41-50'
      elseif(IBS.ge.51.and.IBS.lt.60)then
        helptopic='bsimtype_details51-60'
      elseif(IBS.ge.61.and.IBS.lt.70)then
        helptopic='bsimtype_details61-70'
      elseif(IBS.ge.71.and.IBS.lt.80)then
        helptopic='bsimtype_details71-80'
      elseif(IBS.ge.81.and.IBS.lt.90)then
        helptopic='bsimtype_details81-90'
      elseif(IBS.ge.91.and.IBS.lt.100)then
        helptopic='bsimtype_details91-100'
      elseif(IBS.ge.101.and.IBS.lt.110)then
        helptopic='bsimtype_details101-110'
      elseif(IBS.ge.111.and.IBS.lt.120)then
        helptopic='bsimtype_details111-120'
      elseif(IBS.ge.121.and.IBS.lt.130)then
        helptopic='bsimtype_details121-130'
      elseif(IBS.ge.131.and.IBS.lt.140)then
        helptopic='bsimtype_details131-140'
      elseif(IBS.ge.141.and.IBS.lt.140)then
        helptopic='bsimtype_details141-146'
      endif
      call gethelptext(helpinsub,helptopic,nbhelp)

      CALL PHELPD('bsimp synopsis',nbhelp,'-',0,0,IER)
      call easkok(' ','Is this selection ok?',ok,nbhelp)
      if(ok)then
        ibst=IBS
      else
        goto 42
      endif

      return
      end
