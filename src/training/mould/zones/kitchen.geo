*Geometry 1.1,GEN,kitchen # tag version, format, zone name
*date Sun Dec  8 16:04:12 2013  # latest file modification 
kitchen describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,3.00000,0.00000,0.00000  #   1
*vertex,6.00000,0.00000,0.00000  #   2
*vertex,6.00000,4.00000,0.00000  #   3
*vertex,3.00000,4.00000,0.00000  #   4
*vertex,3.00000,0.00000,2.20000  #   5
*vertex,6.00000,0.00000,2.20000  #   6
*vertex,6.00000,4.00000,2.20000  #   7
*vertex,3.00000,4.00000,2.20000  #   8
*vertex,3.00000,3.00000,2.20000  #   9
*vertex,6.00000,3.00000,2.20000  #  10
*vertex,3.00000,1.50000,2.20000  #  11
*vertex,3.00000,1.50000,0.00000  #  12
*vertex,4.00000,0.00000,0.80000  #  13
*vertex,5.20000,0.00000,0.80000  #  14
*vertex,5.20000,0.00000,2.00000  #  15
*vertex,4.00000,0.00000,2.00000  #  16
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,6,5,1,13,16,15,14,13  #  1
*edges,5,2,3,7,10,6  #  2
*edges,4,3,4,8,7  #  3
*edges,5,4,12,11,9,8  #  4
*edges,5,5,6,10,9,11  #  5
*edges,5,4,3,2,1,12  #  6
*edges,4,9,10,7,8  #  7
*edges,4,12,1,5,11  #  8
*edges,4,13,14,15,16  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,south,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,east_part,VERT,-,-,-,ext_part,OPAQUE,SIMILAR,00,00  #   2 ||< identical environment
*surf,liv_r,VERT,-,-,-,int_part,OPAQUE,ANOTHER,05,08  #   3 ||< kitch:liv_room
*surf,hall,VERT,-,-,-,int_part,OPAQUE,ANOTHER,03,02  #   4 ||< kitchen:hall_1
*surf,bed_1,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,07,06  #   5 ||< floor:bed_south
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,north_beds,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,10,12  #   7 ||< kitch:north_beds
*surf,Ext,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,window,VERT,-,-,-,single_glaz,SC_8985_04nb,EXTERIOR,0,0  #   9 ||< external
# 
*insol,1,6,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    12.00 0  # zone base list
