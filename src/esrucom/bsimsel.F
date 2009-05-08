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
      common/pophelp/h(60)
      dimension bl(145),IVAL(145)
      character bl*65,h*72,outs*72
      logical ok,dok
      integer ibst  ! index of user selection

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
      H(1)='Specify the BASESIMP configuration number (iconfig).'
      H(2)='Refer to src/esp-r/esrubld/bscoeff.F for the'
      H(3)='correspondence between iconfig and standard BASESIMP'
      H(4)='descriptors (e.g. BCIN_1).'
      H(5)='*any constr = and first storey construction.'
      H(6)='*brick on wall = brick veneer directly on basement wall.'
      H(7)='*non-brick = non-brick veneer or bricks thermally broken'
      H(8)='  from basement concrete walls.'
      H(9)='*brick on slab = brick veneer placed directly on floor slab'
      H(10)='TB = thermal-break between walls and floor slab,'
      H(11)='intrn full = internal wall insulation full height,'
      H(12)='extrn full = external wall insulation fill height,'
      H(13)='0.6m perm = slab permiter insulation 0.6m wide,'
      H(14)='wood Wl conc Fl = wood wall and concrete floor,'
      H(15)='conc Wl&Fl = concrete wall and floor,'
      H(16)='wood Wl&Fl = wooden wall and floor.'
      if(ibst.ne.0)then
        write(outs,'(3a)') '(currently ',bl(ibst)(1:52),'...)'
      else
        write(outs,'(a)') '(currently nothing defined)'
      endif
      CALL EPICKS(IX,IVAL,'The other side of surface faces: ',outs,
     &  65,144,bl,' Available BSIMP configurations',IER,16)

      IF(IX.EQ.0)return
      if(IVAL(1).eq.0)then
        return
      else
        IBS = IVAL(1)
      endif

C Provide an expanded description for confirmation.
      H(4)=' '
      if(IBS.eq.1)then
        H(1)='BCIN_1 details: concrete walls and floor,'
        H(2)='interior surface of wall insulated over full-height,'
        H(3)='any first storey construction type.'
      elseif(IBS.eq.2)then
        H(1)='BCIN_2 details: concrete walls and floor,'
        H(2)='interior surface of wall insulated from top of wall to'
        H(3)='0.2 m from floor, any first storey construction type.'
      elseif(IBS.eq.3)then
        H(1)='BCIN_3 details: concrete walls and floor, interior surf'
        H(2)='of wall insulated from top of wall to 0.6 m below grade,'
        H(3)='1st storey brick veneer directly on basement walls.'
      elseif(IBS.eq.4)then
        H(1)='BCIN_4 details: concrete walls and floor, interior surf'
        H(2)='of wall insulated from top of wall to 0.6 m below grade,'
        H(3)='1st storey is non-brick veneer or bricks thermally broken'
        H(4)='from basement concrete walls.'
      elseif(IBS.eq.5)then
        H(1)='BCEN_1 details: concrete walls and floor, exterior surf'
        H(2)='of wall insulated over full-height, first storey brick'
        H(3)='veneer placed directly on basement concrete walls.'
      elseif(IBS.eq.6)then
        H(1)='BCEN_2 details: concrete walls and floor, exterior surf'
        H(2)='of wall insulated over full-height, first storey is'
        H(3)='non-brick veneer or bricks thermally broken from'
        H(4)='basement concrete walls.'
      elseif(IBS.eq.7)then
        H(1)='BCEN_3 details: concrete walls and floor, exterior surf'
        H(2)='of wall insulated below grade, first storey brick'
        H(3)='veneer placed directly on basement concrete walls.'
      elseif(IBS.eq.8)then
        H(1)='BCEN_4 details: concrete walls and floor, exterior surf'
        H(2)='of wall insulated below grade, first storey is non-brick'
        H(3)='veneer or bricks thermally broken from basement'
        H(4)='concrete walls.'
      elseif(IBS.eq.9)then
        H(1)='BCNN_1 details: concrete walls and floor, no wall or'
        H(2)='floor insulation, first storey brick veneer placed'
        H(3)='directly on basement concrete walls.'
      elseif(IBS.eq.10)then
        H(1)='BCNN_2 details: concrete walls and floor, no wall or'
        H(2)='floor insulation, first storey is non-brick veneer or'
        H(3)='bricks thermally broken from basement concrete walls.'
      elseif(IBS.eq.11)then
        H(1)='BCCN_1 details: concrete walls and floor, exterior of'
        H(2)='wall insulated below grade and interior insulation from'
        H(3)='top of wall overlapping with ext insulation. 1st story'
        H(4)='brick veneer placed directly on basement concrete walls.'
      elseif(IBS.eq.12)then
        H(1)='BCCN_2 details: concrete walls and floor, exterior of'
        H(2)='wall insulated below grade and interior insulation from'
        H(3)='top of wall overlapping with ext insulation. 1st story'
        H(4)='is non-brick veneer or thermally broken bricks.'
      elseif(IBS.eq.13)then
        H(1)='BWNN_1 details: wood walls and wood floor, no insulation'
        H(2)='and any first storey construction type.'
        H(3)=' '
      elseif(IBS.eq.14)then
        H(1)='BWIN_1 details: wood walls and wood floor, interior of'
        H(2)='wall insulated over full-height and any first storey'
        H(3)='construction type.'
      elseif(IBS.eq.15)then
        H(1)='BWIN_2 details: wood walls and wood floor, interior of'
        H(2)='wall insulated from top of wall to 0.2 m from floor,'
        H(3)='any first storey construction type.'
      elseif(IBS.eq.16)then
        H(1)='BWIN_3 details: wood walls and wood floor, interior of'
        H(2)='wall insulated from top of wall to 0.6 m below grade,'
        H(3)='any first storey construction type.'
      elseif(IBS.eq.17)then
        H(1)='BWEN_1 details: wood walls and wood floor, exterior of'
        H(2)='wall insulated over full-height, any first storey'
        H(3)='construction type.'
      elseif(IBS.eq.18)then
        H(1)='BWEN_2 details: wood walls and wood floor, exterior of'
        H(2)='wall insulated below grade, any first storey'
        H(3)='construction type.'
      elseif(IBS.eq.19)then
        H(1)='BCIB_1 details: concrete walls and floor, interior wall'
        H(2)='insulated over full-height, sub-surface of slab fully'
        H(3)='insulated but no insulation under footings,any first'
        H(4)='storey construction type.'
      endif
      if(IBS.eq.20)then
        H(1)='BCIB_2 details: concrete walls and floor, interior wall'
        H(2)='insulated over full-height, sub-surface of floor slab'
        H(3)='insulated with a 0.6 m strip around perimeter, any first'
        H(4)='storey construction type.'
      elseif(IBS.eq.21)then
        H(1)='BCIB_3 details: concrete walls and floor, interior wall'
        H(2)='insulated over full-height,sub-surface of floorslab'
        H(3)='insulated with a 1.0 m strip around perimeter, any first'
        H(4)='storey construction type.'
      elseif(IBS.eq.22)then
        H(1)='BCIB_4 details: concrete walls and floor, interior wall'
        H(2)='insulated over full-height, sub-surface of slab fully'
        H(3)='insulated but no insulation under footings, thermal-break'
        H(4)='between walls and slab, any 1st storey construction type.'
      elseif(IBS.eq.23)then
        H(1)='BCIB_5 details: concrete walls and floor, interior wall'
        H(2)='insulated over full-height, sub-surface of slab insulated'
        H(3)='with a 0.6 m strip around perimeter, thermal-break'
        H(4)='between walls and slab, any 1st storey construction type.'
      elseif(IBS.eq.24)then
        H(1)='BCIB_6 details: concrete walls and floor, interior wall'
        H(2)='insulated over full-height, sub-surface of slab insulated'
        H(3)='with a 1.0 m strip around perimeter, thermal-break'
        H(4)='between walls and slab, any 1st storey construction type.'
      elseif(IBS.eq.25)then
        H(1)='BCEB_1 details: concrete walls and floor, exterior wall'
        H(2)='insulated over full-height, sub-surface of slab fully'
        H(3)='insulated but no insulation under footings, first storey'
        H(4)='brick veneer placed directly on basement concrete walls.'
      elseif(IBS.eq.26)then
        H(1)='BCEB_2 details: concrete walls and floor, exterior wall'
        H(2)='insulated over full-height, sub-surface of slab insulated'
        H(3)='with a 0.6 m strip around perimeter, first storey brick'
        H(4)='veneer placed directly on basement concrete walls.'
      elseif(IBS.eq.27)then
        H(1)='BCEB_3 details: concrete walls and floor, exterior wall'
        H(2)='insulated over full-height, sub-surface of slab insulated'
        H(3)='with a 1.0 m strip around perimeter, first storey brick'
        H(4)='veneer placed directly on basement concrete walls.'
      elseif(IBS.eq.28)then
        H(1)='SCN_1 details: concrete or soil (for crawl space) floor,'
        H(2)='no insulation, first storey is non-brick veneer or bricks'
        H(3)='thermally broken from concrete floor.'
      elseif(IBS.eq.29)then
        H(1)='SCN_2 details: concrete or soil (for crawl space) floor,'
        H(2)='no insulation, first storey brick veneer placed directly'
        H(3)='on concrete slab.'
      elseif(IBS.eq.30)then
        H(1)='SCN_3 details: concrete or soil (for crawl space) floor,'
        H(2)='no insulation except for thermal break around slab edge,'
        H(3)='first storey is non-brick veneer or bricks thermally'
        H(4)='broken from concrete floor.'
      elseif(IBS.eq.31)then
        H(1)='SCN_4 details: concrete or soil (for crawl space) floor,'
        H(2)='thermal break around edge of slab, first storey brick'
        H(3)='veneer placed directly on concrete slab.'
      elseif(IBS.eq.32)then
        H(1)='SCN_7 details: concrete or soil (for crawl space) floor,'
        H(2)='thermal break around edge of slab, vertical skirt extends'
        H(3)='from bottom of slab,first storey is non-brick veneer or'
        H(4)='bricks thermally broken from concrete floor.'
      elseif(IBS.eq.33)then
        H(1)='SCN_8 details: concrete or soil (for crawl space) floor,'
        H(2)='thermal break around edge of slab, vertical skirt extends'
        H(3)='from bottom of slab, first storey brick veneer placed'
        H(4)='directly on concrete slab.'
      elseif(IBS.eq.34)then
        H(1)='SCB_1 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab insulated 0.6 m around perimeter, no'
        H(3)='insulation under footing or wall (starts 0.25m from edge)'
        H(4)='first storey is non-brick veneer or thermally broken.'
      elseif(IBS.eq.35)then
        H(1)='SCB_2 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab insulated 0.6 m around perimeter, no'
        H(3)='insulation under footing or wall (starts 0.25m from edge)'
        H(4)='1st storey brick veneer placed directly on concrete slab.'
      elseif(IBS.eq.36)then
        H(1)='SCB_3 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab insulated 0.6 m around perimeter and under'
        H(3)='footing creating a 0.85 m continuous section, 1st storey'
        H(4)='is non-brick veneer or bricks thermally broken.'
      elseif(IBS.eq.37)then
        H(1)='SCB_4 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab insulated 0.6 m around perimeter and under'
        H(3)='footing creating a 0.85 m continuous section, 1st storey'
        H(4)='1st storey brick veneer placed directly on concrete slab.'
      elseif(IBS.eq.38)then
        H(1)='SCB_5 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab insulated 0.6 m around perimeter, no'
        H(3)='insulation under footing or wall (starts 0.25m from edge)'
        H(4)='1st storey non-brick veneer or bricks thermally broken.'
      elseif(IBS.eq.39)then
        H(1)='SCB_6 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab insulated 0.6 m around perimeter, no'
        H(3)='insulation under footing or wall (starts 0.25m from edge)'
        H(4)='1st storey brick veneer placed directly on concrete slab.'
      endif
      if(IBS.eq.40)then
        H(1)='SCB_9 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab insulated 0.6 m around perimeter, no ins'
        H(3)='under footing or wall (starts 0.25m from edge), vertical'
        H(3)='skirt & 1st storey non-brick veneer or thermally broken.'
      elseif(IBS.eq.41)then
        H(1)='SCB_10 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab insulated 0.6 m around perimeter, no ins'
        H(3)='under footing or wall (starts 0.25m from edge), thermal'
        H(4)='break @ edge, vertical skirt, 1st storey brick on slab.'
      elseif(IBS.eq.42)then
        H(1)='SCB_11 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab insulated 0.6 m around perimeter, no ins'
        H(3)='under footing or wall (starts 0.25m from edge), horiz'
        H(4)='skirt from slab, 1st storey non-brick or thermal brake.'
      elseif(IBS.eq.43)then
        H(1)='SCB_12 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab insulated 0.6 m around perimeter, no ins'
        H(3)='under footing or wall (starts 0.25m from edge), horiz'
        H(4)='skirt from slab, 1st storey brick veneer on slab.'
      elseif(IBS.eq.44)then
        H(1)='SCB_13 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab insulated 1.0 m around perimeter, no ins'
        H(3)='under footing or wall (starts 0.25m from edge), first'
        H(4)='storey non-brick veneer or thermally broken brick.'
      elseif(IBS.eq.45)then
        H(1)='SCB_14 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab insulated 1.0 m around perimeter, no ins'
        H(3)='under footing or wall (starts 0.25m from edge), first' 
        H(4)='storey brick veneer on slab.'
      elseif(IBS.eq.46)then
        H(1)='SCB_17 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab insulated 1.0 m around perimeter, no ins'
        H(3)='under footing or wall (starts 0.25m from edge), thermal'
        H(4)='break around edge of slab, 1st storey no-brick veneer.'
      elseif(IBS.eq.47)then
        H(1)='SCB_18 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab insulated 1.0 m around perimeter, no ins'
        H(3)='under footing or wall (starts 0.25m from edge), thermal'
        H(4)='break around slab edge, 1st storey brick veneer on slab.'
      elseif(IBS.eq.48)then
        H(1)='SCB_21 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab insulated 1.0 m around perimeter, no ins'
        H(3)='under footing or wall (starts 0.25m from edge), thermal'
        H(4)='break & slab edge, vertical skirt, 1st storey non-brick.'
      elseif(IBS.eq.49)then
        H(1)='SCB_22 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab insulated 1.0 m around perimeter, no ins'
        H(3)='under footing or wall (starts 0.25m from edge), thermal'
        H(4)='break slab edge, vert. skirt, 1st storey brick on slab.'
      elseif(IBS.eq.50)then
        H(1)='SCB_23 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab insulated 1.0 m around perimeter, no ins'
        H(3)='under footing or wall (starts 0.25m from edge), horiz.'
        H(4)='skirt, 1st storey non-brick veneer or thermally broken.'
      elseif(IBS.eq.51)then
        H(1)='SCB_24 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab insulated 1.0 m around perimeter, no ins'
        H(3)='under footing or wall (starts 0.25m from edge), horiz.'
        H(4)='skirt, 1st storey brick veneer on concrete slab.'
      elseif(IBS.eq.52)then
        H(1)='SCB_25 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab fully insulated, no insulation under'
        H(3)='footing or wall (starts 0.25m from edge), first storey'
        H(4)='non-brick veneer or thermally broken brick.'
      elseif(IBS.eq.53)then
        H(1)='SCB_26 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab fully insulated , no insulation'
        H(3)='under footing or wall (starts 0.25m from edge), first'
        H(4)='storey brick veneer placed directly on concrete slab.'
        H(3)=' '
      elseif(IBS.eq.54)then
        H(1)='SCB_29 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab fully insulated except under footing or'
        H(3)='wall (starts 0.25m from edge), thermal break around'
        H(4)='edge, 1st storey is not-brick veneer or thermally broken.'
      elseif(IBS.eq.55)then
        H(1)='SCB_30 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab fully insulated except under footing or'
        H(3)='footing or wall (starts 0.25m from edge), thermal break'
        H(4)='around slab edge, 1st storey is brick veneer on slab.'
      elseif(IBS.eq.56)then
        H(1)='SCB_33 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab fully insulated except under footing or'
        H(3)='wall (starts 0.25m from edge), thermal break around'
        H(4)='slab edge, vertical skirt, 1st storey non-brick veneer.'
      elseif(IBS.eq.57)then
        H(1)='SCB_34 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab fully insulated except under footing or'
        H(3)='wall (starts 0.25m from edge), thermal break around slab'
        H(4)='edge, vertical skirt, 1st storey brick veneer on slab.'
      elseif(IBS.eq.58)then
        H(1)='SCB_35 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab fully insulated except under footing or'
        H(3)='wall (starts 0.25m from edge), horizontal skirt extends'
        H(4)='from slab, 1st storey is non-brick veneer.'
      elseif(IBS.eq.59)then
        H(1)='SCB_36 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab fully insulated except under footing or'
        H(3)='wall (starts 0.25m from edge), horizontal skirt extends'
        H(4)='from slab, 1st storey is brick veneer on slab.'
      endif
      if(IBS.eq.60)then
        H(1)='SCA_17 details: concrete or soil (for crawl space) floor,'
        H(2)='top of slab fully insulated, first storey is non-brick'
        H(3)='veneer or bricks thermally broken from concrete slab.'
      elseif(IBS.eq.61)then
        H(1)='SCA_18 details: concrete or soil (for crawl space) floor,'
        H(2)='top of slab fully insulated, first storey is brick'
        H(3)='veneer placed directly on slab.'
      elseif(IBS.eq.62)then
        H(1)='SCA_19 details: concrete or soil (for crawl space) floor,'
        H(2)='top of slab fully insulated, thermal break around edge'
        H(3)='of slab, first storey is non-brick veneer or bricks'
        H(4)='thermally broken from concrete floor.'
      elseif(IBS.eq.63)then
        H(1)='SCA_20 details: concrete or soil (for crawl space) floor,'
        H(2)='top of slab fully insulated, thermal break around edge'
        H(3)='of slab, first storey brick veneer on concrete slab.'
      elseif(IBS.eq.64)then
        H(1)='SCA_21 details: concrete or soil (for crawl space) floor,'
        H(2)='top of slab fully insulated, thermal break around edge'
        H(3)='of slab, vertical skirt extend from bottom of slab, 1st'
        H(4)='storey is non-brick veneer or thermally broken bricks.'
      elseif(IBS.eq.65)then
        H(1)='SCA_22 details: concrete or soil (for crawl space) floor,'
        H(2)='top of slab fully insulated, thermal break around edge'
        H(3)='of slab, vertical skirt extend from bottom of slab, 1st'
        H(4)='storey is brick veneer placed directly on concrete slab.'
      elseif(IBS.eq.66)then
        H(1)='SCA_23 details: concrete or soil (for crawl space) floor,'
        H(2)='top of slab fully insulated, horizontal skirt extending'
        H(3)='from slab, first storey is non-brick veneer or thermally'
        H(4)='broken bricks.'
      elseif(IBS.eq.67)then
        H(1)='SCA_24 details: concrete or soil (for crawl space) floor,'
        H(2)='top of slab fully insulated, horizontal skirt extending'
        H(3)='from slab, first storey is brick veneer placed directly'
        H(4)='on concrete slab.'
      elseif(IBS.eq.68)then
        H(1)='BCCN_3 details: concrete walls and floor, interior and'
        H(2)='exterior wall insulated over full-height, thermal-break'
        H(3)='between walls and floor slab, any first storey'
        H(4)='construction type.'
      elseif(IBS.eq.69)then
        H(1)='BCCB_4 details: concrete walls and floor, interior and'
        H(2)='exterior wall insulated over full-height, sub-surface of'
        H(3)='slab fully insulated but no insulation under footings,'
        H(4)='thermal-break W/F, any first storey construction type.'
      elseif(IBS.eq.70)then
        H(1)='BCEA_1 details: concrete walls and floor, exterior of'
        H(2)='wall insulated over full-height, top of slab fully'
        H(3)='insulated, first storey brick veneer placed directly on'
        H(4)='basement concrete walls.'
      elseif(IBS.eq.71)then
        H(1)='BCEA_4 details: concrete walls and floor, exterior of'
        H(2)='wall insulated over full-height, top of slab fully'
        H(3)='insulated, first storey non-brick veneer or thermally'
        H(4)='broken bricks.'
      elseif(IBS.eq.72)then
        H(1)='BCIA_1 details: concrete walls and floor, interior of'
        H(2)='wall insulated over full-height, top of slab fully'
        H(3)='insulated, any first storey construction type.'
      elseif(IBS.eq.73)then
        H(1)='BCIA_4 details: concrete walls and floor, interior of'
        H(2)='wall insulated from top of wall to 0.2 m from floor, top'
        H(3)='of slab fully insulated, any first storey construction.'
      elseif(IBS.eq.74)then
        H(1)='BCEA_7 details: concrete walls and floor, exterior of'
        H(2)='wall insulated below grade, top of slab fully insulated,'
        H(3)='first storey brick veneer placed directly on basement'
        H(4)='concrete walls.'
      elseif(IBS.eq.75)then
        H(1)='BCEA_8 details: concrete walls and floor, exterior of'
        H(2)='wall insulated below grade, top of slab fully insulated,'
        H(3)='first storey non-brick veneer or bricks thermally broken'
        H(4)='from basement concrete walls.'
      elseif(IBS.eq.76)then
        H(1)='BCEB_8 details: concrete walls and floor, exterior of'
        H(2)='wall insulated to 0.6 m below grade, thermal-break at'
        H(3)='walls & floor, sub-surface of floor slab fully insulated'
        H(4)='except footings, 1st storey brick on concrete walls.'
      elseif(IBS.eq.77)then
        H(1)='BCEB_9 details: concrete walls and floor, exterior of'
        H(2)='wall insulated to 0.6 m below grade, thermal-break at'
        H(3)='walls & floor, sub-surface of floor slab fully insulated'
        H(4)='except footings, 1st storey non-brick veneer.'
      elseif(IBS.eq.78)then
        H(1)='BWEN_3 details: wood walls and wood floor, exterior wall'
        H(2)='insulated to 0.6 m below grade, any first storey'
        H(3)='construction type.'
      elseif(IBS.eq.79)then
        H(1)='BBIB_3 details: wood walls and concrete floor, interior'
        H(2)='of wall insulated over full-height, thermal-break at'
        H(3)='walls & floor, sub-surface of floor slab fully insulated'
        H(4)='except footings, any first storey construction type.'
      endif
      if(IBS.eq.80)then
        H(1)='BBIB_4 details: wood walls and concrete floor, interior'
        H(2)='wall insulated from top of wall to 0.6 m below grade,'
        H(3)='thermal-break at walls & floor, sub-surface of slab fully'
        H(4)='insulated except footings, 1st storey brick vnr on walls.'
      elseif(IBS.eq.81)then
        H(1)='SCB_31 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab fully insulated including footings,'
        H(3)='thermal break around edge of slab, first storey non-brick'
        H(4)='veneer or bricks thermally broken from concrete floor.'
      elseif(IBS.eq.82)then
        H(1)='SCB_32 details: concrete or soil (for crawl space) floor,'
        H(2)='bottom of slab fully insulated including footings,'
        H(3)='thermal break around edge of slab, first storey brick'
        H(4)='brick veneer placed directly on concrete slab.'
      elseif(IBS.eq.83)then
        H(1)='SCB_37 details: concrete or soil (for crawl space) floor,'
        H(2)='slab insulated 0.35 m around perimeter plus footings for'
        H(3)='0.6 m continuous section, thermal break around slab edge,'
        H(4)='first storey is non-brick veneer or thermally broken.'
      elseif(IBS.eq.84)then
        H(1)='SCB_38 details: concrete or soil (for crawl space) floor,'
        H(2)='slab insulated 0.35 m around perimeter plus footings for'
        H(3)='0.6 m continuous section, thermal break around slab edge,'
        H(4)='first storey brick veneer placed directly on slab.'
      elseif(IBS.eq.85)then
        H(1)='SCB_39 details: concrete or soil (for crawl space) floor,'
        H(2)='slab insulated 0.75 m around perimeter plus footings for'
        H(3)='1.0 m continuous section, thermal break around slab edge,'
        H(4)='first storey is non-brick veneer or thermally broken.'
      elseif(IBS.eq.86)then
        H(1)='SCB_40 details: concrete or soil (for crawl space) floor,'
        H(2)='slab insulated 0.75 m around perimeter plus footings for'
        H(3)='1.0 m continuous section, thermal break around slab edge,'
        H(4)='first storey brick veneer placed directly on slab.'
      elseif(IBS.eq.87)then
        H(1)='BBEB_1 details: wood walls and concrete floor, exterior'
        H(2)='wall insulated to 0.6 m below grade, sub-surface of slab'
        H(3)='fully insulated except footings, 1st storey brick veneer'
        H(4)='placed directly on slab.'
      elseif(IBS.eq.88)then
        H(1)='BBEN_1 details: wood walls and concrete floor, exterior'
        H(2)='wall insulated over full-height, 1st storey brick veneer'
        H(3)='placed directly on basement concrete walls.'
      elseif(IBS.eq.89)then
        H(1)='BBEN_2 details: wood walls and concrete floor, exterior'
        H(2)='wall insulated to 0.6 m below grade, 1st storey brick'
        H(3)='veneer placed directly on basement concrete walls.'
      elseif(IBS.eq.90)then
        H(1)='BBIA_2 details: wood walls and concrete floor, interior'
        H(2)='wall insulated over full-height, top of slab fully'
        H(3)='insulated, any first storey construction type.'
      elseif(IBS.eq.91)then
        H(1)='BBIN_2 details: wood walls and concrete floor, interior'
        H(2)='wall insulated from top of wall to 0.6 m below grade,'
        H(3)='first storey is non-brick veneer or bricks thermally'
        H(4)='broken from  basement concrete walls.'
      elseif(IBS.eq.92)then
        H(1)='BCCB_8 details: concrete walls and floor, interior and'
        H(2)='exterior wall insulated over full-height, perimeter of'
        H(3)='floor slab insulated to 0.6 m, any first storey'
        H(4)='construction type.'
      elseif(IBS.eq.93)then
        H(1)='BCCA_7 details: concrete walls and floor, interior of'
        H(2)='wall insulated over full-height, exterior insulated to'
        H(3)='0.6 m below grade, top of slab fully insulated, any first'
        H(4)='storey construction type.'
      elseif(IBS.eq.94)then
        H(1)='BCCA_8 details: concrete walls and floor, interior of'
        H(2)='wall insulated from top of wall to 0.2m, exterior wall'
        H(3)='insulated over full-height, top of slab fully insulated,'
        H(4)='any first storey construction type.'
      elseif(IBS.eq.95)then
        H(1)='BCCN_4 details: concrete walls and floor, interior of'
        H(2)='wall insulated over full-height, exterior wall insulated'
        H(3)='to 0.6 m below grade, any first storey construction type.'
      elseif(IBS.eq.96)then
        H(1)='BCCN_5 details: concrete walls and floor, interior of'
        H(2)='wall insulated from top of wall to 0.2 m exterior wall'
        H(3)='insulated over full-height, any first storey construction'
        H(4)='type.'
      elseif(IBS.eq.97)then
        H(1)='BCEA_5 details: concrete walls and floor, exterior of'
        H(2)='wall insulated to 0.6 m below grade, top of slab fully'
        H(3)='insulated, first storey brick veneer placed directly on'
        H(4)='basement concrete walls.'
      elseif(IBS.eq.98)then
        H(1)='BCEA_6 details: concrete walls and floor, exterior of'
        H(2)='wall insulated to 0.6 m below grade, top of slab fully'
        H(3)='insulated, first storey is non-brick veneer or bricks'
        H(4)='thermally broken from basement concrete walls.'
      elseif(IBS.eq.99)then
        H(1)='BCEB_4 details: concrete walls and floor, exterior of'
        H(2)='wall insulated over full-height, sub-surface floor slab'
        H(3)='fully insulated except footings, first storey is non-'
        H(4)='brick veneer or thermally broken from basement walls.'
      endif
      if(IBS.eq.100)then
        H(1)='BCEB_5 details: concrete walls and floor, exterior of'
        H(2)='wall insulated over full-height, sub-surface floor slab'
        H(3)='insulated with a 0.6 m strip around perimeter, 1st storey'
        H(4)='non-brick veneer or thermally broken from basement walls.'
      elseif(IBS.eq.101)then
        H(1)='BCEB_6 details: concrete walls and floor, exterior of'
        H(2)='wall insulated over full-height, slab insulated 1.0m'
        H(3)='around perimeter, 1st storey non-brick veneer or brick'
        H(4)='thermally broken from basement walls.'
      elseif(IBS.eq.102)then
        H(1)='BWIA_1 details: wood walls and wood floor, interior of'
        H(2)='wall insulated over full-height, top of slab insulated'
        H(3)='with a 0.6 m strip around perimeter,any first storey'
        H(4)='construction type.'
      elseif(IBS.eq.103)then
        H(1)='BWIA_2 details: wood walls and wood floor, interior of'
        H(2)='wall insulated over full-height, top of slab fully'
        H(3)='insulated, any first storey construction type.'
      elseif(IBS.eq.104)then
        H(1)='SCA_1 details: concrete or soil (for crawl space) floor,'
        H(2)='top of slab insulated with a 0.6 m perimeter strip,'
        H(3)='1st storey non-brick veneer or brick thermally broken'
        H(4)='from basement walls.'
      elseif(IBS.eq.105)then
        H(1)='SCA_2 details: concrete or soil (for crawl space) floor,'
        H(2)='top of slab insulated with a 0.6 m perimeter strip,'
        H(3)='first storey brick veneer placed directly on slab.'
      elseif(IBS.eq.106)then
        H(1)='SCA_9 details: concrete or soil (for crawl space) floor,'
        H(2)='top of slab insulated with a 1.0 m perimeter strip,'
        H(3)='first storey brick veneer placed directly on slab.'
      elseif(IBS.eq.107)then
        H(1)='SCA_10 details: concrete or soil (for crawl space) floor,'
        H(2)='top of slab insulated with a 1.0 m perimeter strip,'
        H(3)='first storey brick veneer placed directly on slab.'
      elseif(IBS.eq.108)then
        H(1)='BBIN_1 details: wood walls and concrete floor, interior'
        H(2)='wall insulated over full-height, first storey non-brick'
        H(3)='veneer or bricks thermally broken from concrete walls.'
      elseif(IBS.eq.109)then
        H(1)='BCEN_5 details: concrete walls and floor, exterior of'
        H(2)='wall insulated to 0.6 m below grade, first storey brick'
        H(3)='veneer placed directly on basement concrete walls.'
      elseif(IBS.eq.110)then
        H(1)='BCEN_6 details: concrete walls and floor, exterior of'
        H(2)='wall insulated to 0.6 m below grade, 1st storey non-brick'
        H(3)='veneer or bricks thermally broken from concrete walls.'
      elseif(IBS.eq.111)then
        H(1)='BBIA_1 details: wood walls and concrete floor, interior'
        H(2)='wall insulated over full-height, top of slab fully'
        H(3)='insulated, any first storey construction type.'
      elseif(IBS.eq.112)then
        H(1)='BBIB_1 details: wood walls and concrete floor, interior'
        H(2)='wall insulated over full-height, floor slab insulated'
        H(3)='with a 0.6 m perimeter strip, any first storey'
        H(4)='construction type.'
      elseif(IBS.eq.113)then
        H(1)='BBIB_2 details: wood walls and concrete floor, interior'
        H(2)='wall insulated over full-height, floor slab fully'
        H(3)='insulated except footings, any first storey construction'
        H(4)='type.'
      elseif(IBS.eq.114)then
        H(1)='BCCB_9 details: concrete walls and floor, interior of'
        H(2)='wall insulated over full-height, exterior of wall'
        H(2)='insulated to 0.6 m below grade, floor slab fully'
        H(3)='insulated except footings,any first storey construction'
        H(4)='type.'
      elseif(IBS.eq.115)then
        H(1)='BCCB_10 details: concrete walls and floor, interior of'
        H(2)='wall insulated over full-height, exterior of wall'
        H(3)='insulated to 0.6 m below grade, floor slab fully insul'
        H(4)='except for footings, any first storey construction type.'
      elseif(IBS.eq.116)then
        H(1)='BCCA_1 details: concrete walls and floor, exterior of'
        H(2)='wall insulated below grade and interior from top of wall'
        H(3)='to produce an overlap, top of slab fully insulated, 1st'
        H(4)='storey brick veneer placed directly on basement walls.'
      elseif(IBS.eq.117)then
        H(1)='BCCA_4 details: concrete walls and floor, exterior of'
        H(2)='wall insulated below grade and interior from top of wall'
        H(3)='to produce an overlap, top of slab fully insulated, 1st'
        H(4)='story non-brick veneer or thermally broken from walls.'
      elseif(IBS.eq.118)then
        H(1)='BCIB_7 details: concrete walls and floor, interior wall'
        H(2)='insulated from top of wall to 0.6 m below grade, floor'
        H(3)='slab fully insulated except for footings, thermal-break'
        H(4)='walls & slab, 1st storey brick veneer on basement walls.'
      elseif(IBS.eq.119)then
        H(1)='BCIB_8 details: concrete walls and floor, interior wall'
        H(2)='insulated from top of wall to 0.6 m below grade, floor'
        H(3)='slab fully insulated except for footings, thermal-break'
        H(4)='walls & slab, 1st storey is non-brick veneer.'
      endif
      if(IBS.eq.120)then
        H(1)='BBEB_2 details: wood walls and concrete floor, exterior'
        H(2)='wall insulated to 0.6 m below grade, slab fully insulated'
        H(3)='except footings, thermal-break walls & slab, 1st storey'
        H(4)='brick veneer placed directly on basement concrete walls.'
      elseif(IBS.eq.121)then
        H(1)='BCIA_3 details: concrete walls and floor, interior wall'
        H(2)='insulated from top of wall to 0.6 m below grade, top of'
        H(3)='slab insulated with a 1.0 m strip around perimeter, 1st'
        H(4)='storey brick veneer on basement concrete walls.'
      elseif(IBS.eq.122)then
        H(1)='BCIA_5 details: concrete walls and floor, interior wall'
        H(2)='insulated from top of wall to 0.6 m below grade, slab'
        H(3)='insulated with a 0.6 m perimeter strip, 1st storey brick'
        H(4)='veneer directly on basement concrete walls.'
      elseif(IBS.eq.123)then
        H(1)='BCIA_6 details: concrete walls and floor, interior wall'
        H(2)='insulated from top of wall to 0.6 m below grade, slab'
        H(3)='insulated with a 0.6 m perimeter strip, first storey'
        H(4)='non-brick veneer or brick thermally broken from walls.'
      elseif(IBS.eq.124)then
        H(1)='BCIB_9 details: concrete walls and floor, interior wall'
        H(2)='insulated from top of wall to 0.6 m below grade, slab'
        H(3)='insulated with a 0.6 m perimeter strip, thermal break'
        H(4)='walls & slab, 1st storey brick veneer direct on walls.'
      elseif(IBS.eq.125)then
        H(1)='BCIB_10 details: concrete walls and floor, interior wall'
        H(2)='insulated from top of wall to 0.6 m below grade, slab'
        H(3)='insulated with a 0.6 m perimeter strip, thermal break'
        H(4)='walls & slab, 1st storey non-brick veneer.'
      elseif(IBS.eq.126)then
        H(1)='BCEB_10 details: concrete walls and floor, exterior wall'
        H(2)='insulated to 0.6 m below grade, slab insulated with a'
        H(3)='0.6 m perimeter strip, thermal break walls & slab, 1st'
        H(4)='storey brick veneer direct on basement walls.'
      elseif(IBS.eq.127)then
        H(1)='BCEB_11 details: concrete walls and floor, exterior wall'
        H(2)='insulated to 0.6 m below grade, slab insulated with a'
        H(3)='0.6 m perimeter strip, thermal break walls & slab, 1st'
        H(4)='storey non-brick veneer or thermally broken from walls.'
      elseif(IBS.eq.128)then
        H(1)='BCEA_3 details: concrete walls and floor, exterior wall'
        H(2)='insulated over full-height, top slab insulated with a 1.0'
        H(3)='perimeter strip, first storey brick veneer direct on'
        H(4)='basement walls.'
      elseif(IBS.eq.129)then
        H(1)='BCEA_9 details: concrete walls and floor, exterior wall'
        H(2)='insulated over full-height, top slab insulated with a 1.0'
        H(3)='perimeter strip, first storey non-brick veneer or brick'
        H(4)='thermally broken from walls.'
      elseif(IBS.eq.130)then
        H(1)='BCEA_10 details: concrete walls and floor, exterior wall'
        H(2)='insulated to 0.6 m below grade, top slab insulated with'
        H(3)='a 0.6m perimeter strip, first storey brick veneer on'
        H(4)='basement concrete walls.'
      elseif(IBS.eq.131)then
        H(1)='BCEA_11 details: concrete walls and floor, exterior wall'
        H(2)='insulated to 0.6 m below grade, top slab insulated with'
        H(3)='a 0.6m perimeter strip, first storey non-brick veneer or'
        H(4)='brick thermally broken from walls.'
      elseif(IBS.eq.132)then
        H(1)='BWIB_1 details: wood walls and wood floor, interior wall'
        H(2)='insulated over full-height, sub-surface slab insulated'
        H(3)='with a 1.0m perimeter strip, first storey brick veneer on'
        H(4)='basement concrete walls.'
      elseif(IBS.eq.133)then
        H(1)='BWIB_2 details: wood walls and wood floor, interior wall'
        H(2)='insulated over full-height, sub-surface of floor slab'
        H(3)='insulated except for footings, first storey brick veneer'
        H(4)='on basement concrete walls.'
      elseif(IBS.eq.134)then
        H(1)='BWIB_4 details: wood walls and wood floor, interior wall'
        H(2)='insulated over full-height, sub-surface slab insulated'
        H(3)='with a 0.6m perimeter strip, first storey brick veneer on'
        H(4)='basement concrete walls.'
      elseif(IBS.eq.135)then
        H(1)='BWIA_3 details: wood walls and wood floor, interior wall'
        H(2)='insulated from top of wall to 0.6 m below grade, top'
        H(3)='slab insulated with a 0.6 m strip around perimeter, first'
        H(4)='storey brick veneer on basement concrete walls.'
      elseif(IBS.eq.136)then
        H(1)='BWEB_1 details: wood walls and wood floor, exterior wall'
        H(2)='insulated over full-height, sub-surface slab insulated'
        H(3)='with a 1.0m perimeter strip, first storey brick veneer on'
        H(4)='basement concrete walls.'
      elseif(IBS.eq.137)then
        H(1)='BWEB_2 details: wood walls and wood floor, exterior wall'
        H(2)='insulated over full-height, sub-surface of floor slab'
        H(3)='insulated except for footings, first storey brick veneer'
        H(4)='on basement concrete walls.'
      elseif(IBS.eq.138)then
        H(1)='BWEB_3 details: wood walls and wood floor, exterior wall'
        H(2)='insulated to 0.6 m below grade, sub-surface slab'
        H(3)='insulated with a 0.6m perimeter strip, first storey'
        H(4)='brick veneer on basement concrete walls.'
      elseif(IBS.eq.139)then
        H(1)='BWEB_4 details: wood walls and wood floor, exterior wall'
        H(2)='insulated over full-height, sub-surface slab insulated'
        H(3)='with a 0.6m perimeter strip, first storey brick veneer'
        H(4)='on basement concrete walls.'
      elseif(IBS.eq.140)then
        H(1)='BBIB_5 details: wood walls and concrete floor, interior'
        H(2)='wall insulated over full-height, sub-surface slab'
        H(3)='insulated with a 1.0m perimeter strip, first storey brick'
        H(4)='veneer on basement concrete walls.'
      elseif(IBS.eq.141)then
        H(1)='BBIB_6 details: wood walls and concrete floor, interior'
        H(2)='wall insulated from top of wall to 0.6 m below grade,'
        H(3)=' sub-surface slab insulated with a 0.6m perimeter strip,'
        H(4)='first storey brick veneer on basement concrete walls.'
      elseif(IBS.eq.142)then
        H(1)='BBEB_3 details: wood walls and concrete floor, exterior'
        H(2)='wall insulated over full-height, sub-surface slab'
        H(3)='insulated with a 1.0m perimeter strip, first storey brick'
        H(4)='veneer on basement concrete walls.'
      elseif(IBS.eq.143)then
        H(1)='BBEB_4 details: wood walls and concrete floor, exterior'
        H(2)='wall insulated over full-height, sub-surface slab'
        H(3)='insulated with a 0.6m perimeter strip, first storey brick'
        H(4)='veneer on basement concrete walls.'
      elseif(IBS.eq.144)then
        H(1)='BBEB_5 details: wood walls and concrete floor, exterior'
        H(2)='wall insulated to 0.6 m below grade, sub-surface slab'
        H(3)='insulated with a 0.6m perimeter strip, first storey brick'
        H(4)='veneer on basement concrete walls.'
      elseif(IBS.eq.145)then
        H(1)='BBEA_2 details: wood walls and concrete floor, exterior'
        H(2)='wall insulated over full-height, top of slab fully'
        H(3)='insulated,  first storey brick veneer on basement'
        H(4)='concrete walls.'
C      else
C        H(1)='Not a recognised foundation configuration.'
C        H(2)=' '
C        H(3)=' '
      endif
      CALL PHELPD('bsimp synopsis',4,'-',0,0,IER)
      dok=.true.
      h(1)='If you want to use the selection say ok. '
      call askok(' Is this selection ok?',' ',ok,dok,1)
      if(ok)then
        ibst=IBS
      else
        goto 42
      endif

      return
      end
