*Geometry 1.1,GEN,attic # tag version, format, zone name
*date Sun May  2 07:44:17 2021  # latest file modification 
attic zone
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,2.70000  #   1
*vertex,10.00000,0.00000,2.70000  #   2
*vertex,10.00000,9.00000,2.70000  #   3
*vertex,5.00000,9.00000,2.70000  #   4
*vertex,5.00000,15.00000,2.70000  #   5
*vertex,0.00000,15.00000,2.70000  #   6
*vertex,5.00000,0.00000,5.03000  #   7
*vertex,5.00000,15.00000,5.03000  #   8
*vertex,10.00000,15.00000,2.70000  #   9
# 
# tag, number of vertices followed by list of associated vert
*edges,3,1,2,7  #   1
*edges,5,2,3,9,8,7  #   2
*edges,4,6,8,9,5  #   3
*edges,4,1,7,8,6  #   4
*edges,6,2,1,6,5,4,3  #   5
*edges,4,9,3,4,5  #   6
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Southwall,VERT,-,-,-,roof,OPAQUE,EXTERIOR,000,000  #   1 ||< external
*surf,eastwall,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,000,000  #   2 ||< external
*surf,northwall,VERT,-,-,-,roof,OPAQUE,EXTERIOR,000,000  #   3 ||< external
*surf,westwall,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,000,000  #   4 ||< external
*surf,floor-main,FLOR,-,-,-,ceiling_inv,OPAQUE,ANOTHER,001,007  #   5 ||< ceiling:main
*surf,floor-garge,FLOR,-,-,-,ceiling_inv,OPAQUE,ANOTHER,002,005  #   6 ||< garage-ceil:garage
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,2,5,6,   150.00 0  # zone base list
