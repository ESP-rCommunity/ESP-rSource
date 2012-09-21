*Geometry 1.1,GEN,upper_wc # tag version, format, zone name
*date Thu Apr 19 10:35:21 2012  # latest file modification 
upper_wc describes WC at top of landing
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,5.50000,2.40000,2.80000  #   1
*vertex,6.50000,2.40000,2.80000  #   2
*vertex,6.50000,4.90000,2.80000  #   3
*vertex,5.50000,4.90000,2.80000  #   4
*vertex,5.50000,4.40000,2.80000  #   5
*vertex,5.50000,3.60000,2.80000  #   6
*vertex,5.50000,2.50000,2.80000  #   7
*vertex,5.50000,2.40000,5.60000  #   8
*vertex,6.50000,2.40000,5.60000  #   9
*vertex,6.50000,4.90000,5.60000  #  10
*vertex,5.50000,4.90000,5.60000  #  11
*vertex,5.50000,3.60000,5.60000  #  12
*vertex,5.50000,2.50000,5.60000  #  13
*vertex,5.50000,4.40000,5.60000  #  14
*vertex,6.50000,3.40000,3.60000  #  15
*vertex,6.50000,4.00000,3.60000  #  16
*vertex,6.50000,4.00000,4.80000  #  17
*vertex,6.50000,3.40000,4.80000  #  18
*vertex,6.50000,3.48000,3.68000  #  19
*vertex,6.50000,3.92000,3.68000  #  20
*vertex,6.50000,3.92000,4.72000  #  21
*vertex,6.50000,3.48000,4.72000  #  22
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,9,8  #  1
*edges,10,2,3,10,9,2,15,18,17,16,15  #  2
*edges,4,7,1,8,13  #  3
*edges,7,1,7,6,5,4,3,2  #  4
*edges,4,3,4,11,10  #  5
*edges,4,6,7,13,12  #  6
*edges,4,5,6,12,14  #  7
*edges,4,4,5,14,11  #  8
*edges,7,10,11,14,12,13,8,9  #  9
*edges,10,15,16,17,18,15,19,22,21,20,19  # 10
*edges,4,19,20,21,22  # 11
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_br1,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,07,07  #   1 ||< ptn_br1:up_br_one
*surf,facade,VERT,-,WALL,-,conc_blck_rn,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,ptn_br1_a,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,07,08  #   3 ||< ptn_br1_a:up_br_one
*surf,wc_floor,FLOR,-,-,-,Floorti_up,OPAQUE,ANOTHER,02,24  #   4 ||< ceil_wc:entry
*surf,ptn_wc_a,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,05,01  #   5 ||< ptn_wc_a:upstair_pa
*surf,ptn_wc_b,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,05,09  #   6 ||< ptn_wc_b:upstair_pa
*surf,door_wc_b,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,05,10  #   7 ||< door_wc_b:upstair_pa
*surf,ptn_wc_c,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,05,11  #   8 ||< ptn_wc_c:upstair_pa
*surf,wc_ceiling,CEIL,-,-,-,Ceiltk_up,OPAQUE,ANOTHER,10,03  #   9 ||< wc_ceiling:roof_space
*surf,frame,VERT,facade,F-FRAME,CLOSED,Window_frme,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,glazing,VERT,frame,C-WINDOW,CLOSED,Window_ext,DC_8074_04nb,EXTERIOR,0,0  #  11 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,2,9,4,     5.00 0  # zone base list
