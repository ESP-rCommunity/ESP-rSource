*Geometry 1.1,GEN,living # tag version, format, zone name
*date Sun Apr  8 09:34:56 2012  # latest file modification 
living describes the main downatairs space
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,4.50000,7.00000,0.20000  #   1
*vertex,2.80000,7.00000,0.20000  #   2
*vertex,1.20000,7.00000,0.20000  #   3
*vertex,-1.60000,7.00000,0.20000  #   4
*vertex,-1.60000,5.40000,0.20000  #   5
*vertex,-1.60000,2.80000,0.20000  #   6
*vertex,4.50000,2.40000,0.20000  #   7
*vertex,4.50000,3.20000,0.20000  #   8
*vertex,4.50000,3.40000,0.20000  #   9
*vertex,4.50000,4.20000,0.20000  #  10
*vertex,4.50000,4.90000,0.20000  #  11
*vertex,4.50000,6.10000,0.20000  #  12
*vertex,4.50000,7.00000,2.60000  #  13
*vertex,2.80000,7.00000,2.60000  #  14
*vertex,1.20000,7.00000,2.60000  #  15
*vertex,-1.60000,7.00000,2.60000  #  16
*vertex,-1.60000,5.40000,2.60000  #  17
*vertex,-1.60000,2.80000,2.60000  #  18
*vertex,4.50000,2.40000,2.60000  #  19
*vertex,4.50000,3.20000,2.60000  #  20
*vertex,4.50000,3.40000,2.60000  #  21
*vertex,4.50000,4.20000,2.60000  #  22
*vertex,4.50000,4.90000,2.60000  #  23
*vertex,4.50000,6.10000,2.60000  #  24
*vertex,2.80000,7.00000,0.95000  #  25
*vertex,1.20000,7.00000,0.95000  #  26
*vertex,2.80000,7.00000,2.30000  #  27
*vertex,1.20000,7.00000,2.30000  #  28
*vertex,2.76000,7.00000,0.99000  #  29
*vertex,1.24000,7.00000,0.99000  #  30
*vertex,1.24000,7.00000,2.26000  #  31
*vertex,2.76000,7.00000,2.26000  #  32
*vertex,4.50000,6.10000,1.20000  #  33
*vertex,4.50000,7.00000,1.20000  #  34
*vertex,4.50000,4.90000,1.20000  #  35
*vertex,3.60000,2.40000,2.60000  #  36
*vertex,3.60000,2.80000,2.60000  #  37
*vertex,-1.60000,3.90000,2.60000  #  38
*vertex,3.60000,3.90000,2.60000  #  39
*vertex,3.60000,4.90000,2.60000  #  40
*vertex,3.60000,2.40000,0.20000  #  41
*vertex,3.60000,2.80000,0.20000  #  42
*vertex,1.60000,2.80000,0.20000  #  43
*vertex,1.60000,2.80000,2.60000  #  44
# 
# tag, number of vertices followed by list of associated vert
*edges,7,1,2,25,27,14,13,34  #  1
*edges,4,2,3,26,25  #  2
*edges,6,3,4,16,15,28,26  #  3
*edges,4,4,5,17,16  #  4
*edges,5,5,6,18,38,17  #  5
*edges,4,7,8,20,19  #  6
*edges,4,8,9,21,20  #  7
*edges,4,9,10,22,21  #  8
*edges,5,10,11,35,23,22  #  9
*edges,10,13,14,15,16,17,38,39,40,23,24  # 10
*edges,15,1,12,11,10,9,8,7,41,42,43,6,5,4,3,2  # 11
*edges,4,27,28,15,14  # 12
*edges,10,25,26,28,27,25,29,32,31,30,29  # 13
*edges,4,29,30,31,32  # 14
*edges,4,12,1,34,33  # 15
*edges,5,11,12,33,23,35  # 16
*edges,5,33,34,13,24,23  # 17
*edges,5,18,44,37,39,38  # 18
*edges,9,22,23,40,39,37,36,19,20,21  # 19
*edges,4,41,7,19,36  # 20
*edges,4,42,41,36,37  # 21
*edges,4,43,42,37,44  # 22
*edges,4,6,43,44,18  # 23
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,no_facde_a,VERT,-,-,-,conc_blck_rn,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,nor_sill,VERT,-,-,-,conc_blck_rn,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,nor_facade_b,VERT,-,-,-,conc_blck_rn,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,left_facade,VERT,-,-,-,conc_blck_rn,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,ptn_other_b,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,13,07  #   5 ||< ptn_other_b:garage
*surf,ptn_hall_a,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,02,11  #   6 ||< ptn_liv_a:entry
*surf,ptn_hall_b,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,02,10  #   7 ||< ptn_liv_b:entry
*surf,door_hall,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,02,09  #   8 ||< door_liv:entry
*surf,ptn_hall_c,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,02,08  #   9 ||< ptn_liv_c:entry
*surf,liv_ceiling,CEIL,-,-,-,Ceilti_lowc,OPAQUE,ANOTHER,04,11  #  10 ||< br2_floor:up_br2
*surf,liv_floor,FLOR,-,-,-,Floorti_lowc,OPAQUE,ANOTHER,11,03  #  11 ||< liv_floor:crawl
*surf,head_c,VERT,-,WALL,-,conc_blck_rn,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,frame_c,VERT,-,F-FRAME,CLOSED,Window_frme,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,glaz_c,VERT,frame_c,C-WINDOW,CLOSED,Window_ext,DC_8074_04nb,EXTERIOR,0,0  #  14 ||< external
*surf,lower_ptn,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,09,04  #  15 ||< Wall-6:below_str
*surf,ptn_below,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,09,05  #  16 ||< Wall-7:below_str
*surf,above_str,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,02,18  #  17 ||< side_wall:entry
*surf,ceil_br3_b,CEIL,-,-,-,Floorti_upc,OPAQUE,ANOTHER,08,16  #  18 ||< br3_flr_a:up_br_three
*surf,ceil_hall,CEIL,-,-,-,Ceilti_lowc,OPAQUE,ANOTHER,05,25  #  19 ||< psg_fl_liv:upstair_pa
*surf,ptn_liv_a,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,12,18  #  20 ||< ptn_liv_a:kit_dining
*surf,ptn_liv_b,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,12,19  #  21 ||< ptn_liv_b:kit_dining
*surf,ptn_liv_c,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,12,20  #  22 ||< ptn_liv_c:kit_dining
*surf,fict_liv,VERT,-,-,-,fict,SC_fictit,ANOTHER,12,21  #  23 ||< fict_liv:kit_dining
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,11,    25.98 0  # zone base list
