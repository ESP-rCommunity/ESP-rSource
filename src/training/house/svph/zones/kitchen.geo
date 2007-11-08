*Geometry 1.1,GEN,kitchen # tag version, format, zone name
*date Thu Aug 23 18:13:58 2007  # latest file modification 
kitchen describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,5.59700,10.21800,0.00000  #   1
*vertex,2.45200,9.37500,0.00000  #   2
*vertex,1.37000,9.08500,0.00000  #   3
*vertex,2.18500,6.04300,0.00000  #   4
*vertex,5.27600,6.87100,0.00000  #   5
*vertex,6.41200,7.17500,0.00000  #   6
*vertex,5.59700,10.21800,2.40000  #   7
*vertex,4.75800,9.99300,2.40000  #   8
*vertex,2.45200,9.37500,2.40000  #   9
*vertex,1.37000,9.08500,2.40000  #  10
*vertex,1.92900,7.00000,2.40000  #  11
*vertex,2.18500,6.04300,2.40000  #  12
*vertex,5.27600,6.87100,2.40000  #  13
*vertex,6.41200,7.17500,2.40000  #  14
*vertex,5.31600,7.90800,2.40000  #  15
*vertex,5.02000,7.82800,2.40000  #  16
*vertex,5.04500,10.07300,1.05000  #  17
*vertex,3.30600,9.60700,1.05000  #  18
*vertex,3.30700,9.60500,2.10000  #  19
*vertex,5.04500,10.07100,2.10000  #  20
# 
# tag, number of vertices followed by list of associated vert
*edges,11,1,2,9,8,7,1,17,20,19,18,17  #  1
*edges,4,2,3,10,9  #  2
*edges,5,3,4,12,11,10  #  3
*edges,4,4,5,13,12  #  4
*edges,4,5,6,14,13  #  5
*edges,4,6,1,7,14  #  6
*edges,6,7,8,15,16,13,14  #  7
*edges,6,8,9,10,11,16,15  #  8
*edges,4,16,11,12,13  #  9
*edges,6,6,5,4,3,2,1  # 10
*edges,4,17,18,19,20  # 11
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,n_ext,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,exl_kit,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,11,03  #   2 ||< exl_kit:ext_lobby
*surf,gar_kit,VERT,-,-,-,ext_wall,OPAQUE,ANOTHER,14,05  #   3 ||< gar_kit:garage
*surf,kit_din,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,02,01  #   4 ||< kit_din:dining_room
*surf,kit_liv,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,01,02  #   5 ||< kit_liv:living_room
*surf,kit_hall,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,03,06  #   6 ||< p_kitchen:hall
*surf,kit_uh,CEIL,-,-,-,grnd_ceiling,OPAQUE,ANOTHER,10,10  #   7 ||< flr_kit:upper_hall
*surf,ceil_k_bth,CEIL,-,-,-,grnd_ceiling,OPAQUE,ANOTHER,05,09  #   8 ||< flr_kit:bath
*surf,ceil_k_b1,CEIL,-,-,-,grnd_ceiling,OPAQUE,ANOTHER,06,10  #   9 ||< flr_kit:bedroom_1
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,00,01  #  10 ||< user def grnd profile  1
*surf,window,VERT,-,-,-,double_glaz,DCF7672_06nb,EXTERIOR,0,0  #  11 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,10,    13.78 0  # zone base list
