*Geometry 1.1,GEN,collector # tag version, format, zone name
*date Sun Dec  8 13:46:26 2013  # latest file modification 
collector describes a solar collector (air based)
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,1.50000,0.90000,5.51300  #   1
*vertex,4.00000,0.90000,5.51300  #   2
*vertex,4.00000,2.74000,6.29700  #   3
*vertex,1.50000,2.74000,6.29700  #   4
*vertex,1.50000,0.91990,5.46712  #   5
*vertex,4.00000,0.91990,5.46712  #   6
*vertex,4.00000,2.75976,6.25129  #   7
*vertex,1.50000,2.75976,6.25129  #   8
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,3,4  #  1
*edges,4,6,5,8,7  #  2
*edges,4,2,6,7,3  #  3
*edges,4,3,7,8,4  #  4
*edges,4,1,4,8,5  #  5
*edges,4,2,1,5,6  #  6
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,first,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,xxcollector,SLOP,-,-,-,collector,OPAQUE,ANOTHER,11,16  #   2 ||< xcollector:loft
*surf,xcoll-e,VERT,-,-,-,door,OPAQUE,ANOTHER,11,17  #   3 ||< coll-e:loft
*surf,xcoll-up,SLOP,-,-,-,door,OPAQUE,ANOTHER,11,18  #   4 ||< coll-up:loft
*surf,xcoll-w,VERT,-,-,-,door,OPAQUE,ANOTHER,11,19  #   5 ||< coll-w:loft
*surf,xcoll-dn,SLOP,-,-,-,door,OPAQUE,ANOTHER,11,20  #   6 ||< coll-dn:loft
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,0.00,0  # zone base
