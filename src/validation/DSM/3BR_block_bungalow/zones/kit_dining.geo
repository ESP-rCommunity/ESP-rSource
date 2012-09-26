*Geometry 1.1,GEN,kit_dining # tag version, format, zone name
*date Sun Apr  8 09:30:46 2012  # latest file modification 
kit_dining describes a combined kitchen and dining room
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,-1.60000,2.80000,0.20000  #   1
*vertex,-1.60000,0.20000,0.20000  #   2
*vertex,1.30000,0.20000,0.20000  #   3
*vertex,2.00000,0.20000,0.20000  #   4
*vertex,2.60000,0.20000,0.20000  #   5
*vertex,3.10000,0.20000,0.20000  #   6
*vertex,3.80000,0.20000,0.20000  #   7
*vertex,4.50000,0.20000,0.20000  #   8
*vertex,4.50000,2.40000,0.20000  #   9
*vertex,-1.60000,2.80000,2.60000  #  10
*vertex,-1.60000,0.20000,2.60000  #  11
*vertex,1.30000,0.20000,2.60000  #  12
*vertex,2.00000,0.20000,2.60000  #  13
*vertex,2.60000,0.20000,2.60000  #  14
*vertex,3.10000,0.20000,2.60000  #  15
*vertex,3.80000,0.20000,2.60000  #  16
*vertex,4.50000,0.20000,2.60000  #  17
*vertex,4.50000,2.40000,2.60000  #  18
*vertex,1.30000,0.20000,0.95000  #  19
*vertex,2.00000,0.20000,0.95000  #  20
*vertex,1.30000,0.20000,2.30000  #  21
*vertex,2.00000,0.20000,2.30000  #  22
*vertex,1.34000,0.20000,0.99000  #  23
*vertex,1.96000,0.20000,0.99000  #  24
*vertex,1.96000,0.20000,2.26000  #  25
*vertex,1.34000,0.20000,2.26000  #  26
*vertex,3.10000,0.20000,1.10000  #  27
*vertex,3.80000,0.20000,1.10000  #  28
*vertex,3.10000,0.20000,2.30000  #  29
*vertex,3.80000,0.20000,2.30000  #  30
*vertex,3.14000,0.20000,1.14000  #  31
*vertex,3.76000,0.20000,1.14000  #  32
*vertex,3.76000,0.20000,2.26000  #  33
*vertex,3.14000,0.20000,2.26000  #  34
*vertex,3.60000,2.40000,2.60000  #  35
*vertex,3.60000,0.20000,2.60000  #  36
*vertex,3.60000,2.80000,2.60000  #  37
*vertex,3.60000,2.40000,0.20000  #  38
*vertex,3.60000,2.80000,0.20000  #  39
*vertex,1.60000,2.80000,0.20000  #  40
*vertex,1.60000,2.80000,2.60000  #  41
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,11,10  #  1
*edges,6,2,3,19,21,12,11  #  2
*edges,4,3,4,20,19  #  3
*edges,6,4,5,14,13,22,20  #  4
*edges,6,5,6,27,29,15,14  #  5
*edges,4,6,7,28,27  #  6
*edges,6,7,8,17,16,30,28  #  7
*edges,4,8,9,18,17  #  8
*edges,12,9,8,7,6,5,4,3,2,1,40,39,38  #  9
*edges,4,21,22,13,12  # 10
*edges,10,19,20,22,21,19,23,26,25,24,23  # 11
*edges,4,23,24,25,26  # 12
*edges,5,29,30,16,36,15  # 13
*edges,10,27,28,30,29,27,31,34,33,32,31  # 14
*edges,4,31,32,33,34  # 15
*edges,5,16,17,18,35,36  # 16
*edges,10,15,36,35,37,41,10,11,12,13,14  # 17
*edges,4,9,38,35,18  # 18
*edges,4,38,39,37,35  # 19
*edges,4,39,40,41,37  # 20
*edges,4,40,1,10,41  # 21
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_other_c,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,13,02  #   1 ||< ptn_kit:garage
*surf,so_facade_a,VERT,-,WALL,-,conc_blck_rn,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,so_sill_a,VERT,-,WALL,-,conc_blck_rn,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,so_facade_b,VERT,-,WALL,-,conc_blck_rn,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,so_facade_c,VERT,-,WALL,-,conc_blck_rn,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,so_sill_b,VERT,-,WALL,-,conc_blck_rn,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,so_facade_d,VERT,-,WALL,-,conc_blck_rn,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,ptn_bath,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,01,08  #   8 ||< ptn_liv:lower_bath
*surf,liv_floor,FLOR,-,-,-,Ceilti_crlc,OPAQUE,ANOTHER,11,10  #   9 ||< kit_floor:crawl
*surf,head_a,VERT,-,WALL,-,conc_blck_rn,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,frame_a,VERT,-,F-FRAME,CLOSED,Window_frme,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,glaz_a,VERT,frame_a,C-WINDOW,CLOSED,Window_ext,DC_8074_04nb,EXTERIOR,0,0  #  12 ||< external
*surf,head_b,VERT,-,WALL,-,conc_blck_rn,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,frame_b,VERT,-,F-FRAME,CLOSED,Window_frme,OPAQUE,EXTERIOR,0,0  #  14 ||< external
*surf,glaz_b,VERT,frame_b,C-WINDOW,CLOSED,Window_ext,DC_8074_04nb,EXTERIOR,0,0  #  15 ||< external
*surf,liv_ceil_a,CEIL,-,-,-,Ceilti_lowc,OPAQUE,ANOTHER,07,15  #  16 ||< br_floor_k:up_br_one
*surf,ceil_liv_br3,CEIL,-,-,-,Ceilti_lowc,OPAQUE,ANOTHER,08,11  #  17 ||< br3_floor:up_br_three
*surf,ptn_liv_a,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,03,20  #  18 ||< ptn_liv_a:living
*surf,ptn_liv_b,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,03,21  #  19 ||< ptn_liv_b:living
*surf,ptn_liv_c,VERT,-,-,-,intcnc_block,OPAQUE,ANOTHER,03,22  #  20 ||< ptn_liv_c:living
*surf,fict_liv,VERT,-,-,-,fict,SC_fictit,ANOTHER,03,23  #  21 ||< fict_liv:living
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,9,    15.50 0  # zone base list
