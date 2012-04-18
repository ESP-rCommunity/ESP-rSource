*Geometry 1.1,GEN,upstair_pa # tag version, format, zone name
*date Wed Apr 18 07:38:39 2012  # latest file modification 
upstair_pa describes upper portion of stairs and passage
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,5.50000,4.90000,2.80000  #   1
*vertex,4.50000,6.10000,2.80000  #   2
*vertex,4.50000,4.90000,2.80000  #   3
*vertex,3.60000,4.90000,2.80000  #   4
*vertex,3.60000,4.10000,2.80000  #   5
*vertex,3.60000,3.90000,2.80000  #   6
*vertex,3.60000,3.70000,2.80000  #   7
*vertex,3.60000,2.80000,2.80000  #   8
*vertex,3.60000,2.50000,2.80000  #   9
*vertex,4.50000,2.50000,2.80000  #  10
*vertex,5.50000,2.50000,2.80000  #  11
*vertex,5.50000,3.60000,2.80000  #  12
*vertex,5.50000,4.40000,2.80000  #  13
*vertex,5.50000,4.90000,5.60000  #  14
*vertex,3.60000,3.70000,5.60000  #  15
*vertex,3.60000,2.80000,5.60000  #  16
*vertex,3.60000,2.50000,5.60000  #  17
*vertex,4.50000,2.50000,5.60000  #  18
*vertex,5.50000,2.50000,5.60000  #  19
*vertex,5.50000,3.60000,5.60000  #  20
*vertex,5.50000,4.40000,5.60000  #  21
*vertex,4.50000,7.00000,2.80000  #  22
*vertex,4.50000,7.00000,3.90000  #  23
*vertex,4.50000,5.57568,5.60000  #  24
*vertex,4.50000,4.90000,5.60000  #  25
*vertex,3.60000,4.90000,5.60000  #  26
*vertex,3.60000,4.10000,5.60000  #  27
*vertex,3.60000,3.90000,5.60000  #  28
*vertex,6.50000,7.00000,2.80000  #  29
*vertex,6.50000,7.00000,3.90000  #  30
*vertex,6.50000,5.57568,5.60000  #  31
*vertex,6.50000,4.90000,5.60000  #  32
*vertex,6.50000,7.00000,2.60000  #  33
*vertex,4.50000,7.00000,2.60000  #  34
*vertex,4.50000,6.10000,2.60000  #  35
*vertex,5.50000,6.10000,2.60000  #  36
*vertex,5.50000,4.90000,2.60000  #  37
*vertex,6.50000,4.90000,2.60000  #  38
*vertex,5.90000,7.00000,2.80000  #  39
*vertex,5.10000,7.00000,2.80000  #  40
*vertex,5.10000,7.00000,3.70000  #  41
*vertex,5.90000,7.00000,3.70000  #  42
*vertex,5.86000,7.00000,2.84000  #  43
*vertex,5.14000,7.00000,2.84000  #  44
*vertex,5.14000,7.00000,3.66000  #  45
*vertex,5.86000,7.00000,3.66000  #  46
*vertex,4.50000,4.90000,2.60000  #  47
*vertex,6.50000,4.90000,2.80000  #  48
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,48,32,14  #  1
*edges,12,33,34,22,23,30,29,33,39,42,41,40,39  #  2
*edges,4,4,5,27,26  #  3
*edges,4,6,7,15,28  #  4
*edges,4,7,8,16,15  #  5
*edges,4,8,9,17,16  #  6
*edges,4,9,10,18,17  #  7
*edges,4,10,11,19,18  #  8
*edges,4,11,12,20,19  #  9
*edges,4,12,13,21,20  # 10
*edges,4,13,1,14,21  # 11
*edges,6,1,13,12,11,10,3  # 12
*edges,6,2,3,25,24,23,22  # 13
*edges,4,3,4,26,25  # 14
*edges,4,5,6,28,27  # 15
*edges,4,30,23,24,31  # 16
*edges,6,34,33,38,37,36,35  # 17
*edges,7,38,33,29,30,31,32,48  # 18
*edges,6,47,37,38,48,1,3  # 19
*edges,15,32,31,24,25,26,27,28,15,16,17,18,19,20,21,14  # 20
*edges,6,34,35,47,3,2,22  # 21
*edges,10,39,40,41,42,39,43,46,45,44,43  # 22
*edges,4,43,44,45,46  # 23
*edges,4,36,37,47,35  # 24
*edges,8,10,9,8,7,6,5,4,3  # 25
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_wc_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,06,05  #   1 ||< ptn_wc_a:upper_wc
*surf,low_facade,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,door_br2,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,04,03  #   3 ||< door_hall:up_br2
*surf,ptn_br3_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,08,05  #   4 ||< ptn_psg_b:up_br_three
*surf,door_br3,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,08,04  #   5 ||< door_psg:up_br_three
*surf,ptn_br3_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,08,03  #   6 ||< ptn_psg_a:up_br_three
*surf,door_br1,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,07,10  #   7 ||< door_br1:up_br_one
*surf,ptn_br1,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,07,11  #   8 ||< ptn_br1a:up_br_one
*surf,ptn_wc_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,06,06  #   9 ||< ptn_wc_b:upper_wc
*surf,door_wc_b,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,06,07  #  10 ||< door_wc_b:upper_wc
*surf,ptn_wc_c,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,06,08  #  11 ||< ptn_wc_c:upper_wc
*surf,psg_floor,FLOR,-,-,-,Floorti_upc,OPAQUE,ANOTHER,02,12  #  12 ||< ceil_entry:entry
*surf,ptn_sloped,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,04,05  #  13 ||< ptn_sloped:up_br2
*surf,ptn_hall_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,04,04  #  14 ||< ptn_hall_b:up_br2
*surf,Wall-3,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,04,02  #  15 ||< ptn_hall:up_br2
*surf,slop_ceil,SLOP,-,-,-,Roof_stairs,OPAQUE,EXTERIOR,0,0  #  16 ||< external
*surf,fict,FLOR,-,-,-,fict,SC_fictit,ANOTHER,02,21  #  17 ||< fict:entry
*surf,fac_sloped,VERT,-,WALL,-,tk_block_rn,OPAQUE,EXTERIOR,0,0  #  18 ||< external
*surf,struc_a,VERT,-,-,-,Parttim,OPAQUE,SIMILAR,00,00  #  19 ||< identical environment
*surf,ceil_staur,CEIL,-,-,-,Ceiltk_up,OPAQUE,ANOTHER,10,02  #  20 ||< ceil_staur:roof_space
*surf,struc_c,VERT,-,-,-,Parttim,OPAQUE,SIMILAR,00,00  #  21 ||< identical environment
*surf,frame,VERT,low_facade,F-FRAME,CLOSED,Window_frme,OPAQUE,EXTERIOR,0,0  #  22 ||< external
*surf,glaz,VERT,frame,C-WINDOW,CLOSED,Window_ext,DC_8074_04nb,EXTERIOR,0,0  #  23 ||< external
*surf,fict_b,FLOR,-,-,-,fict,SC_fictit,ANOTHER,02,22  #  24 ||< fict_b:entry
*surf,psg_fl_liv,FLOR,-,-,-,Floorti_upc,OPAQUE,ANOTHER,03,35  #  25 ||< ceil_hall:living
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,4,14,12,17,24,    11.28 0  # zone base list
