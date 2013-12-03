*Geometry 1.1,GEN,up_br_three # tag version, format, zone name
*date Mon Mar 19 12:06:23 2012  # latest file modification 
up_br_three describes bedroom three on upper level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,3.10000,0.20000,2.80000  #   1
*vertex,3.60000,0.20000,2.80000  #   2
*vertex,3.60000,2.50000,2.80000  #   3
*vertex,3.60000,2.80000,2.80000  #   4
*vertex,3.60000,3.70000,2.80000  #   5
*vertex,3.60000,3.90000,2.80000  #   6
*vertex,0.10000,3.90000,2.80000  #   7
*vertex,0.10000,2.80000,2.80000  #   8
*vertex,0.10000,0.20000,2.80000  #   9
*vertex,1.30000,0.20000,2.80000  #  10
*vertex,2.00000,0.20000,2.80000  #  11
*vertex,3.10000,0.20000,3.90000  #  12
*vertex,3.60000,0.20000,3.90000  #  13
*vertex,0.10000,3.90000,3.90000  #  14
*vertex,0.10000,0.20000,3.90000  #  15
*vertex,1.30000,0.20000,3.90000  #  16
*vertex,2.00000,0.20000,3.90000  #  17
*vertex,3.60000,3.90000,5.60000  #  18
*vertex,0.10000,3.90000,5.60000  #  19
*vertex,0.10000,1.90000,5.60000  #  20
*vertex,3.60000,1.90000,5.60000  #  21
*vertex,3.60000,2.50000,5.60000  #  22
*vertex,3.60000,2.80000,5.60000  #  23
*vertex,3.60000,3.70000,5.60000  #  24
*vertex,1.36000,0.20000,2.86000  #  25
*vertex,1.94000,0.20000,2.86000  #  26
*vertex,1.94000,0.20000,3.84000  #  27
*vertex,1.36000,0.20000,3.84000  #  28
*vertex,1.30000,0.27071,3.97071  #  29
*vertex,2.00000,0.27071,3.97071  #  30
*vertex,2.00000,1.68492,5.38492  #  31
*vertex,1.30000,1.68492,5.38492  #  32
*vertex,1.36000,0.31314,4.01314  #  33
*vertex,1.94000,0.31314,4.01314  #  34
*vertex,1.94000,1.64250,5.34250  #  35
*vertex,1.36000,1.64250,5.34250  #  36
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,13,12  #  1
*edges,5,2,3,22,21,13  #  2
*edges,4,3,4,23,22  #  3
*edges,4,4,5,24,23  #  4
*edges,4,5,6,18,24  #  5
*edges,7,7,8,9,15,20,19,14  #  6
*edges,4,9,10,16,15  #  7
*edges,10,10,11,17,16,10,25,28,27,26,25  #  8
*edges,4,11,1,12,17  #  9
*edges,13,12,13,21,20,15,29,32,31,30,29,15,16,17  # 10
*edges,8,1,11,10,9,8,4,3,2  # 11
*edges,5,6,7,14,19,18  # 12
*edges,7,20,21,22,23,24,18,19  # 13
*edges,4,25,26,27,28  # 14
*edges,10,29,30,31,32,29,33,36,35,34,33  # 15
*edges,4,33,34,35,36  # 16
*edges,5,4,8,7,6,5  # 17
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,low_fac_a,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,ptn_br1,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,07,09  #   2 ||< ptn_br3:up_br_one
*surf,ptn_psg_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,05,06  #   3 ||< ptn_br3_b:upstair_pa
*surf,door_psg,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,05,05  #   4 ||< door_br3:upstair_pa
*surf,ptn_psg_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,05,04  #   5 ||< ptn_br3_a:upstair_pa
*surf,ptn_other,VERT,-,-,-,Partywall_ti,OPAQUE,SIMILAR,00,00  #   6 ||< identical environment
*surf,low_fac_c,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,low_frame,VERT,-,F-FRAME,CLOSED,Window_frme,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,low_fac_b,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,sloped_roof,SLOP,-,ROOF,PITCHED,Roof_stairs,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,br3_floor,FLOR,-,-,-,Floorti_upc,OPAQUE,ANOTHER,03,33  #  11 ||< ceil_liv_br3:living
*surf,ptn_br,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,04,01  #  12 ||< ptn_br:up_br2
*surf,br3_ceiling,CEIL,-,-,-,Ceilti_up,OPAQUE,ANOTHER,10,05  #  13 ||< br3_ceiling:roof_space
*surf,low_glz,VERT,low_frame,C-WINDOW,CLOSED,Window_ext,DC_8074_04nb,EXTERIOR,0,0  #  14 ||< external
*surf,sky_frame,SLOP,sloped_roof,F-FRAME,CLOSED,Doorint_sol,OPAQUE,EXTERIOR,0,0  #  15 ||< external
*surf,sky_glz,SLOP,sky_frame,S-WINDOW,CLOSED,Window_ext,DC_8074_04nb,EXTERIOR,0,0  #  16 ||< external
*surf,br3_flr_a,FLOR,-,-,-,Ceilti_lowc,OPAQUE,ANOTHER,03,34  #  17 ||< ceil_br3_b:living
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,11,    12.95 0  # zone base list
