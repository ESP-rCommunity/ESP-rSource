*Geometry 1.1,GEN,up_br_one # tag version, format, zone name
*date Thu Apr 19 10:35:22 2012  # latest file modification 
up_br_one describes small bedroom or study on an upper floor
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,6.50000,0.20000,2.80000  #   1
*vertex,6.50000,2.40000,2.80000  #   2
*vertex,5.50000,2.40000,2.80000  #   3
*vertex,5.50000,2.50000,2.80000  #   4
*vertex,4.50000,2.50000,2.80000  #   5
*vertex,3.60000,2.50000,2.80000  #   6
*vertex,3.60000,0.20000,2.80000  #   7
*vertex,3.80000,0.20000,2.80000  #   8
*vertex,4.50000,0.20000,2.80000  #   9
*vertex,6.50000,0.20000,3.90000  #  10
*vertex,3.60000,0.20000,3.90000  #  11
*vertex,3.80000,0.20000,3.90000  #  12
*vertex,4.50000,0.20000,3.90000  #  13
*vertex,6.50000,2.40000,5.60000  #  14
*vertex,5.50000,2.40000,5.60000  #  15
*vertex,5.50000,2.50000,5.60000  #  16
*vertex,3.60000,2.50000,5.60000  #  17
*vertex,3.60000,1.90000,5.60000  #  18
*vertex,4.50000,2.50000,5.60000  #  19
*vertex,6.50000,1.90000,5.60000  #  20
*vertex,3.80000,0.55355,4.25355  #  21
*vertex,4.50000,0.55355,4.25355  #  22
*vertex,4.50000,1.75564,5.45563  #  23
*vertex,3.80000,1.75564,5.45563  #  24
*vertex,3.86000,0.59598,4.29598  #  25
*vertex,4.44000,0.59598,4.29598  #  26
*vertex,4.44000,1.71321,5.41321  #  27
*vertex,3.86000,1.71321,5.41321  #  28
# 
# tag, number of vertices followed by list of associated vert
*edges,5,1,2,14,20,10  #  1
*edges,4,7,8,12,11  #  2
*edges,4,8,9,13,12  #  3
*edges,4,9,1,10,13  #  4
*edges,12,13,10,20,18,11,21,24,23,22,21,11,12  #  5
*edges,6,1,9,5,4,3,2  #  6
*edges,4,2,3,15,14  #  7
*edges,4,3,4,16,15  #  8
*edges,5,6,7,11,18,17  #  9
*edges,4,5,6,17,19  # 10
*edges,4,4,5,19,16  # 11
*edges,7,18,20,14,15,16,19,17  # 12
*edges,10,21,22,23,24,21,25,28,27,26,25  # 13
*edges,4,25,26,27,28  # 14
*edges,5,9,8,7,6,5  # 15
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,side_wall,VERT,-,WALL,-,conc_blck_rn,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,low_fac_a,VERT,-,WALL,-,conc_blck_rn,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,low_frame,VERT,-,F-FRAME,CLOSED,Window_frme,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,low_fac_b,VERT,-,WALL,-,conc_blck_rn,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,sloped_roof,SLOP,-,ROOF,PITCHED,Roof_stairs,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,br1_floor,FLOR,-,-,-,Floorti_upc,OPAQUE,ANOTHER,01,09  #   6 ||< lbath_ceil:lower_bath
*surf,ptn_br1,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,06,01  #   7 ||< ptn_br1:upper_wc
*surf,ptn_br1_a,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,06,03  #   8 ||< ptn_br1_a:upper_wc
*surf,ptn_br3,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,08,02  #   9 ||< ptn_br1:up_br_three
*surf,door_br1,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,05,07  #  10 ||< door_br1:upstair_pa
*surf,ptn_br1a,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,05,08  #  11 ||< ptn_br1:upstair_pa
*surf,br1_ceiling,CEIL,-,-,-,Ceiltk_up,OPAQUE,ANOTHER,10,04  #  12 ||< br1_ceiling:roof_space
*surf,sky_frame,SLOP,sloped_roof,F-FRAME,CLOSED,Window_frme,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,sky_glz,SLOP,sky_frame,S-WINDOW,CLOSED,Window_ext,DC_8074_04nb,EXTERIOR,0,0  #  14 ||< external
*surf,br_floor_k,FLOR,-,-,-,Floorti_upc,OPAQUE,ANOTHER,12,16  #  15 ||< liv_ceil_a:kit_dining
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,2,8,6,     6.85 0  # zone base list
