*Geometry 1.1,GEN,liv_room # tag version, format, zone name
*date Sun Dec  8 16:04:12 2013  # latest file modification 
liv_room describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,4.00000,0.00000  #   1
*vertex,6.00000,4.00000,0.00000  #   2
*vertex,6.00000,7.00000,0.00000  #   3
*vertex,0.00000,7.00000,0.00000  #   4
*vertex,0.00000,4.00000,2.20000  #   5
*vertex,6.00000,4.00000,2.20000  #   6
*vertex,6.00000,7.00000,2.20000  #   7
*vertex,0.00000,7.00000,2.20000  #   8
*vertex,1.50000,4.00000,2.20000  #   9
*vertex,1.50000,4.00000,0.00000  #  10
*vertex,3.00000,4.00000,0.00000  #  11
*vertex,3.00000,4.00000,2.20000  #  12
*vertex,5.00000,7.00000,0.80000  #  13
*vertex,3.00000,7.00000,0.80000  #  14
*vertex,3.00000,7.00000,2.00000  #  15
*vertex,5.00000,7.00000,2.00000  #  16
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,10,9,5  #  1
*edges,4,2,3,7,6  #  2
*edges,10,3,4,8,7,3,13,16,15,14,13  #  3
*edges,4,4,1,5,8  #  4
*edges,6,5,9,12,6,7,8  #  5
*edges,6,4,3,2,11,10,1  #  6
*edges,4,10,11,12,9  #  7
*edges,4,11,2,6,12  #  8
*edges,4,13,14,15,16  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,stairs,VERT,-,-,-,int_part,OPAQUE,ANOTHER,02,03  #   1 ||< livrm:stairs
*surf,east_ext,VERT,-,-,-,ext_part,OPAQUE,SIMILAR,00,00  #   2 ||< identical environment
*surf,north_ext,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,west_part,VERT,-,-,-,ext_part,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,north_beds,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,10,11  #   5 ||< Surf-11:north_beds
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,hall_1,VERT,-,-,-,int_doors,OPAQUE,ANOTHER,03,03  #   7 ||< livroom:hall_1
*surf,kitch,VERT,-,-,-,int_part,OPAQUE,ANOTHER,04,03  #   8 ||< liv_r:kitchen
*surf,window,VERT,-,-,-,single_glaz,SC_8985_04nb,EXTERIOR,0,0  #   9 ||< external
# 
*insol,1,6,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    18.00 0  # zone base list
