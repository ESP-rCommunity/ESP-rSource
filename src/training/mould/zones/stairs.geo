*Geometry 1.1,GEN,stairs # tag version, format, zone name
*date Sun Dec  8 16:04:12 2013  # latest file modification 
stairs describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,1.50000,0.00000  #   1
*vertex,1.50000,1.50000,0.00000  #   2
*vertex,1.50000,4.00000,0.00000  #   3
*vertex,0.00000,4.00000,0.00000  #   4
*vertex,0.00000,1.50000,2.20000  #   5
*vertex,1.50000,1.50000,2.20000  #   6
*vertex,1.50000,4.00000,2.20000  #   7
*vertex,0.00000,4.00000,2.20000  #   8
*vertex,0.00000,3.00000,2.20000  #   9
*vertex,0.00000,2.00000,2.20000  #  10
*vertex,1.50000,2.00000,2.20000  #  11
*vertex,1.50000,3.00000,2.20000  #  12
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,6,2,3,7,12,11,6  #  2
*edges,4,3,4,8,7  #  3
*edges,6,4,1,5,10,9,8  #  4
*edges,4,5,6,11,10  #  5
*edges,4,4,3,2,1  #  6
*edges,4,10,11,12,9  #  7
*edges,4,9,12,7,8  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,bin,VERT,-,-,-,ext_part,OPAQUE,ANOTHER,01,03  #   1 ||< stairs:bin_c
*surf,hall,VERT,-,-,-,int_part,OPAQUE,ANOTHER,03,04  #   2 ||< stairs:hall_1
*surf,livrm,VERT,-,-,-,int_part,OPAQUE,ANOTHER,05,01  #   3 ||< stairs:liv_room
*surf,west,VERT,-,-,-,ext_part,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,bathrm,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,06,08  #   5 ||< stairs:bathrm
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,whc,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,08,06  #   7 ||< floor:w_heat_c
*surf,north_beds,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,09,10  #   8 ||< stairs:hall_2
# 
*insol,1,6,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     3.75 0  # zone base list
