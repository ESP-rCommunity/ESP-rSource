*Geometry 1.1,GEN,garage # tag version, format, zone name
*date Thu Aug 23 18:13:58 2007  # latest file modification 
garage describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,-0.95000,10.01700,0.00000  #   1
*vertex,0.69300,3.88300,0.00000  #   2
*vertex,2.62500,4.40100,0.00000  #   3
*vertex,2.18500,6.04300,0.00000  #   4
*vertex,1.37000,9.08600,0.00000  #   5
*vertex,0.98200,10.53500,0.00000  #   6
*vertex,-0.95000,10.01700,2.40000  #   7
*vertex,0.69300,3.88300,2.40000  #   8
*vertex,2.62500,4.40100,2.40000  #   9
*vertex,2.18500,6.04300,2.40000  #  10
*vertex,1.92900,7.00000,2.40000  #  11
*vertex,1.37000,9.08600,2.40000  #  12
*vertex,0.98200,10.53500,2.40000  #  13
*vertex,-0.56200,8.56800,3.46600  #  14
*vertex,-0.00300,6.48300,4.26200  #  15
*vertex,1.92900,7.00000,4.26200  #  16
*vertex,1.37000,9.08600,3.46600  #  17
*vertex,-0.13000,6.95500,4.60000  #  18
*vertex,1.80200,7.47300,4.60000  #  19
*vertex,1.24100,9.56900,0.00000  #  20
*vertex,1.00800,10.43800,0.00000  #  21
*vertex,1.00800,10.43800,2.10000  #  22
*vertex,1.24100,9.56900,2.10000  #  23
# 
# tag, number of vertices followed by list of associated vert
*edges,4,6,1,7,13  #  1
*edges,7,1,2,8,15,18,14,7  #  2
*edges,4,2,3,9,8  #  3
*edges,4,3,4,10,9  #  4
*edges,5,4,5,12,11,10  #  5
*edges,9,5,20,23,22,21,6,13,17,12  #  6
*edges,4,9,10,11,16  #  7
*edges,5,11,12,17,19,16  #  8
*edges,4,13,7,14,17  #  9
*edges,4,17,14,18,19  # 10
*edges,4,8,9,16,15  # 11
*edges,4,15,16,19,18  # 12
*edges,8,6,21,20,5,4,3,2,1  # 13
*edges,4,20,21,22,23  # 14
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,n_ext,VERT,-,-,-,int_part_1,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,w_ext,VERT,-,-,-,int_part_1,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,s_ext,VERT,-,-,-,int_part_1,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,gar_din,VERT,-,-,-,ext_wall_rev,OPAQUE,ANOTHER,02,02  #   4 ||< gar_kit:dining_room
*surf,gar_kit,VERT,-,-,-,ext_wall_rev,OPAQUE,ANOTHER,04,03  #   5 ||< gar_kit:kitchen
*surf,gar_exl,VERT,-,-,-,ext_wall_rev,OPAQUE,ANOTHER,11,02  #   6 ||< gar_exl:ext_lobby
*surf,gar_b1,VERT,-,-,-,ext_wall_rev,OPAQUE,ANOTHER,06,02  #   7 ||< p_garage:bedroom_1
*surf,gar_bath,VERT,-,-,-,ext_wall_rev,OPAQUE,ANOTHER,05,03  #   8 ||< gar_bth:bath
*surf,north,SLOP,-,-,-,int_part_1,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,north_up,SLOP,-,-,-,int_part_1,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,south,SLOP,-,-,-,int_part_1,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,south_up,SLOP,-,-,-,int_part_1,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,00,01  #  13 ||< user def grnd profile  1
*surf,door,VERT,-,-,-,door,OPAQUE,ANOTHER,11,08  #  14 ||< door:ext_lobby
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,13,    12.70 0  # zone base list
