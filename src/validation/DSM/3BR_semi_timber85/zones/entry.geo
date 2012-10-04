*Geometry 1.1,GEN,entry # tag version, format, zone name
*date Mon Mar 19 11:03:26 2012  # latest file modification 
entry describes area at base of stairs and front door
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,4.50000,2.40000,0.20000  #   1
*vertex,4.70000,2.40000,0.20000  #   2
*vertex,5.50000,2.40000,0.20000  #   3
*vertex,6.50000,2.40000,0.20000  #   4
*vertex,6.50000,3.60000,0.20000  #   5
*vertex,6.50000,4.40000,0.20000  #   6
*vertex,6.50000,4.90000,0.20000  #   7
*vertex,5.50000,4.90000,0.20000  #   8
*vertex,4.50000,4.90000,0.20000  #   9
*vertex,4.50000,4.20000,0.20000  #  10
*vertex,4.50000,3.40000,0.20000  #  11
*vertex,4.50000,3.20000,0.20000  #  12
*vertex,4.50000,2.40000,2.60000  #  13
*vertex,4.70000,2.40000,2.60000  #  14
*vertex,5.50000,2.40000,2.60000  #  15
*vertex,6.50000,2.40000,2.60000  #  16
*vertex,6.50000,3.60000,2.60000  #  17
*vertex,6.50000,4.40000,2.60000  #  18
*vertex,6.50000,4.90000,2.60000  #  19
*vertex,5.50000,4.90000,2.60000  #  20
*vertex,4.50000,4.90000,2.60000  #  21
*vertex,4.50000,4.20000,2.60000  #  22
*vertex,4.50000,3.40000,2.60000  #  23
*vertex,4.50000,3.20000,2.60000  #  24
*vertex,4.50000,4.90000,1.20000  #  25
*vertex,5.50000,4.90000,1.20000  #  26
*vertex,5.50000,6.10000,1.20000  #  27
*vertex,6.50000,6.10000,1.20000  #  28
*vertex,6.50000,7.00000,1.20000  #  29
*vertex,4.50000,7.00000,1.20000  #  30
*vertex,4.50000,6.10000,1.20000  #  31
*vertex,5.50000,6.10000,2.60000  #  32
*vertex,6.50000,7.00000,2.60000  #  33
*vertex,4.50000,7.00000,2.60000  #  34
*vertex,4.50000,6.10000,2.60000  #  35
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,14,13  #  1
*edges,4,2,3,15,14  #  2
*edges,4,3,4,16,15  #  3
*edges,4,4,5,17,16  #  4
*edges,4,5,6,18,17  #  5
*edges,7,6,7,28,29,33,19,18  #  6
*edges,6,8,9,25,21,20,26  #  7
*edges,5,9,10,22,21,25  #  8
*edges,4,10,11,23,22  #  9
*edges,4,11,12,24,23  # 10
*edges,4,12,1,13,24  # 11
*edges,8,13,14,15,20,21,22,23,24  # 12
*edges,12,1,12,11,10,9,8,7,6,5,4,3,2  # 13
*edges,5,31,30,29,28,27  # 14
*edges,4,7,8,27,28  # 15
*edges,4,31,27,20,21  # 16
*edges,4,29,30,34,33  # 17
*edges,5,30,31,21,35,34  # 18
*edges,3,27,20,32  # 19
*edges,3,20,27,32  # 20
*edges,6,33,34,35,32,20,19  # 21
*edges,4,20,32,35,21  # 22
*edges,4,27,8,26,20  # 23
*edges,6,15,16,17,18,19,20  # 24
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_bath_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,01,07  #   1 ||< ptn_ent_b:lower_bath
*surf,door_bath,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,01,06  #   2 ||< door_ent:lower_bath
*surf,ptn_bath_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,01,05  #   3 ||< ptn_ent_a:lower_bath
*surf,facade_a,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,ent_door,VERT,-,P-DOOR,CLOSED,Door_ext,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,facade_b,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,door_und_st,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,09,01  #   7 ||< door_hall:below_str
*surf,ptn_liv_c,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,03,17  #   8 ||< ptn_hall_c:living
*surf,door_liv,VERT,-,-,-,Doorint_sol,OPAQUE,ANOTHER,03,16  #   9 ||< door_hall:living
*surf,ptn_liv_b,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,03,15  #  10 ||< ptn_hall_b:living
*surf,ptn_liv_a,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,03,14  #  11 ||< ptn_hall_a:living
*surf,ceil_entry,CEIL,-,-,-,Ceilti_lowc,OPAQUE,ANOTHER,05,12  #  12 ||< psg_floor:upstair_pa
*surf,entry_floor,FLOR,-,-,-,Floorti_lowc,OPAQUE,ANOTHER,11,02  #  13 ||< entry_floor:crawl
*surf,landing,FLOR,-,-,-,Floorti_upc,OPAQUE,ANOTHER,09,06  #  14 ||< Top-8:below_str
*surf,steps,SLOP,-,-,-,Floorti_upc,OPAQUE,ANOTHER,09,08  #  15 ||< steps:below_str
*surf,up_steps,SLOP,-,-,-,Floorti_upc,OPAQUE,ANOTHER,09,09  #  16 ||< up_steps:below_str
*surf,land_wall,VERT,-,WALL,-,Extwall_ti,OPAQUE,EXTERIOR,0,0  #  17 ||< external
*surf,side_wall,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,03,31  #  18 ||< above_str:living
*surf,banister,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,02,20  #  19 ||< xbanister:entry
*surf,xbanister,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,02,19  #  20 ||< banister:entry
*surf,fict,CEIL,-,-,-,fict,SC_fictit,ANOTHER,05,17  #  21 ||< fict:upstair_pa
*surf,fict_b,CEIL,-,-,-,fict,SC_fictit,ANOTHER,05,24  #  22 ||< fict_b:upstair_pa
*surf,low_banist,VERT,-,-,-,Parttim,OPAQUE,ANOTHER,09,10  #  23 ||< low_banist:below_str
*surf,ceil_wc,CEIL,-,-,-,Ceilti_low,OPAQUE,ANOTHER,06,04  #  24 ||< wc_floor:upper_wc
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,2,13,14,     6.80 0  # zone base list
