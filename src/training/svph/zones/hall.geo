*Geometry 1.1,GEN,hall # tag version, format, zone name
*date Thu Aug 23 18:13:58 2007  # latest file modification 
hall describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,9.77300,11.33700,0.00000  #   1
*vertex,8.85500,11.09100,0.00000  #   2
*vertex,9.17900,9.88400,0.00000  #   3
*vertex,7.77800,9.50800,0.00000  #   4
*vertex,7.45500,10.71600,0.00000  #   5
*vertex,5.59600,10.21800,0.00000  #   6
*vertex,6.41200,7.17500,0.00000  #   7
*vertex,10.58800,8.29400,0.00000  #   8
*vertex,9.77300,11.33700,2.40000  #   9
*vertex,8.85500,11.09100,2.40000  #  10
*vertex,9.17900,9.88400,2.40000  #  11
*vertex,7.77800,9.50800,2.40000  #  12
*vertex,7.45500,10.71600,2.40000  #  13
*vertex,5.59600,10.21800,2.40000  #  14
*vertex,6.41200,7.17500,2.40000  #  15
*vertex,8.27000,7.67300,2.40000  #  16
*vertex,10.58800,8.29400,2.40000  #  17
*vertex,9.57981,11.28523,1.48000  #  18
*vertex,8.97129,11.12216,1.48000  #  19
*vertex,8.97129,11.12216,2.11000  #  20
*vertex,9.57981,11.28523,2.11000  #  21
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,10,9,1,18,21,20,19,18  #  1
*edges,4,2,3,11,10  #  2
*edges,4,3,4,12,11  #  3
*edges,4,4,5,13,12  #  4
*edges,4,5,6,14,13  #  5
*edges,4,6,7,15,14  #  6
*edges,5,7,8,17,16,15  #  7
*edges,4,8,1,9,17  #  8
*edges,6,9,10,11,12,16,17  #  9
*edges,5,13,14,15,16,12  # 10
*edges,8,8,7,6,5,4,3,2,1  # 11
*edges,4,18,19,20,21  # 12
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,north_b,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,p_lobbya,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,12,04  #   2 ||< inl_halc:int_lobby
*surf,p_lobbyb,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,12,03  #   3 ||< inl_halb:int_lobby
*surf,p_lobbyc,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,12,02  #   4 ||< inl_hal:int_lobby
*surf,north_c,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,p_kitchen,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,04,06  #   6 ||< kit_hall:kitchen
*surf,p_living,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,01,01  #   7 ||< hall_liv:living_room
*surf,east,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,ceil_b3,CEIL,-,-,-,grnd_ceiling,OPAQUE,ANOTHER,08,06  #   9 ||< flr_hall:bedroom_3
*surf,ceil_z10,CEIL,-,-,-,grnd_ceiling,OPAQUE,ANOTHER,10,09  #  10 ||< flr_hall:upper_hall
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,00,01  #  11 ||< user def grnd profile  1
*surf,glass,VERT,-,-,-,double_glaz,DCF7672_06nb,EXTERIOR,0,0  #  12 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,11,    11.81 0  # zone base list
