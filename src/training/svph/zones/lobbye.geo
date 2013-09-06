*Geometry 1.1,GEN,ext_lobby # tag version, format, zone name
*date Thu Aug 23 18:13:58 2007  # latest file modification 
ext_lobby describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,2.06400,10.82400,0.00000  #   1
*vertex,0.98200,10.53400,0.00000  #   2
*vertex,1.37000,9.08500,0.00000  #   3
*vertex,2.45200,9.37500,0.00000  #   4
*vertex,2.06400,10.82400,2.40000  #   5
*vertex,0.98200,10.53400,2.40000  #   6
*vertex,1.37000,9.08500,2.40000  #   7
*vertex,2.45200,9.37500,2.40000  #   8
*vertex,2.45200,9.37500,3.46600  #   9
*vertex,1.37000,9.08500,3.46600  #  10
*vertex,1.00800,10.43700,0.00000  #  11
*vertex,1.24100,9.56800,0.00000  #  12
*vertex,1.24100,9.56800,2.10000  #  13
*vertex,1.00800,10.43700,2.10000  #  14
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,9,2,11,14,13,12,3,7,10,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,7,8,9,10  #  4
*edges,5,4,1,5,9,8  #  5
*edges,4,5,6,10,9  #  6
*edges,6,4,3,12,11,2,1  #  7
*edges,4,11,12,13,14  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,n_ext,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,gar_exl,VERT,-,-,-,ext_wall,OPAQUE,ANOTHER,14,06  #   2 ||< gar_exl:garage
*surf,exl_kit,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,04,02  #   3 ||< exl_kit:kitchen
*surf,exl_bath,VERT,-,-,-,int_part_1,OPAQUE,ANOTHER,05,02  #   4 ||< u_exl_bth:bath
*surf,e_ext,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,roof,SLOP,-,-,-,grnd_ceiling,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,00,01  #   7 ||< user def grnd profile  1
*surf,door,VERT,-,-,-,door,OPAQUE,ANOTHER,14,14  #   8 ||< door:garage
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,7,     1.68 0  # zone base list
