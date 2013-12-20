*Geometry 1.1,GEN,hall_1 # tag version, format, zone name
*date Sun Dec  8 16:04:12 2013  # latest file modification 
hall_1 describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,1.50000,1.50000,0.00000  #   1
*vertex,3.00000,1.50000,0.00000  #   2
*vertex,3.00000,4.00000,0.00000  #   3
*vertex,1.50000,4.00000,0.00000  #   4
*vertex,1.50000,1.50000,2.20000  #   5
*vertex,3.00000,1.50000,2.20000  #   6
*vertex,3.00000,4.00000,2.20000  #   7
*vertex,1.50000,4.00000,2.20000  #   8
*vertex,1.50000,2.00000,2.20000  #   9
*vertex,3.00000,2.00000,2.20000  #  10
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,5,2,3,7,10,6  #  2
*edges,4,3,4,8,7  #  3
*edges,5,4,1,5,9,8  #  4
*edges,4,9,10,7,8  #  5
*edges,4,4,3,2,1  #  6
*edges,4,5,6,10,9  #  7
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,door,VERT,-,-,-,int_doors,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,kitchen,VERT,-,-,-,int_part,OPAQUE,ANOTHER,04,04  #   2 ||< hall:kitchen
*surf,livroom,VERT,-,-,-,int_doors,OPAQUE,ANOTHER,05,07  #   3 ||< hall_1:liv_room
*surf,stairs,VERT,-,-,-,int_part,OPAQUE,ANOTHER,02,02  #   4 ||< hall:stairs
*surf,hall_2,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,09,08  #   5 ||< hall1:hall_2
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,bathrm,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,06,09  #   7 ||< hall_1:bathrm
# 
*insol,1,6,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     3.75 0  # zone base list
