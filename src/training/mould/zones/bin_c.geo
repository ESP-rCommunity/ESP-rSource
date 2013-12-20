*Geometry 1.1,GEN,bin_c # tag version, format, zone name
*date Sun Dec  8 16:04:12 2013  # latest file modification 
bin_c describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,1.50000,0.00000,0.00000  #   2
*vertex,1.50000,1.50000,0.00000  #   3
*vertex,0.00000,1.50000,0.00000  #   4
*vertex,0.00000,0.00000,2.20000  #   5
*vertex,1.50000,0.00000,2.20000  #   6
*vertex,1.50000,1.50000,2.20000  #   7
*vertex,0.00000,1.50000,2.20000  #   8
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,4,4,3,2,1  #  6
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,south,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,porch,VERT,-,-,-,int_doors,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,stairs,VERT,-,-,-,ext_part,OPAQUE,ANOTHER,02,01  #   3 ||< bin:stairs
*surf,west,VERT,-,-,-,ext_part,OPAQUE,SIMILAR,00,00  #   4 ||< identical environment
*surf,ceiling,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,06,07  #   5 ||< bin_c:bathrm
*surf,floor,FLOR,-,-,-,grnd_floor,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
# 
*insol,1,6,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     2.25 0  # zone base list
