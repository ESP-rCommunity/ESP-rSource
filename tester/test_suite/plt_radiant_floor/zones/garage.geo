*Geometry 1.1,GEN,garage # tag version, format, zone name
*date Sun May  2 07:44:17 2021  # latest file modification 
garage
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,5.00000,9.00000,0.00000  #   1
*vertex,10.00000,9.00000,0.00000  #   2
*vertex,10.00000,15.00000,0.00000  #   3
*vertex,5.00000,15.00000,0.00000  #   4
*vertex,5.00000,9.00000,2.70000  #   5
*vertex,10.00000,9.00000,2.70000  #   6
*vertex,10.00000,15.00000,2.70000  #   7
*vertex,5.00000,15.00000,2.70000  #   8
*vertex,9.50000,15.00000,0.00000  #   9
*vertex,5.50000,15.00000,0.00000  #  10
*vertex,5.50000,15.00000,2.40000  #  11
*vertex,9.50000,15.00000,2.40000  #  12
*vertex,5.00000,9.50000,0.00000  #  13
*vertex,5.00000,10.50000,0.00000  #  14
*vertex,5.00000,10.50000,2.00000  #  15
*vertex,5.00000,9.50000,2.00000  #  16
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #   1
*edges,4,2,3,7,6  #   2
*edges,8,3,9,12,11,10,4,8,7  #   3
*edges,8,1,5,8,4,14,15,16,13  #   4
*edges,4,5,6,7,8  #   5
*edges,8,1,13,14,4,10,9,3,2  #   6
*edges,4,9,10,11,12  #   7
*edges,4,14,13,16,15  #   8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,garage-s,VERT,-,-,-,internl_wal,OPAQUE,ANOTHER,001,003  #   1 ||< garge-s:main
*surf,garage-e,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,000,000  #   2 ||< external
*surf,garage-n,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,000,000  #   3 ||< external
*surf,garage-w,VERT,-,-,-,internl_wal,OPAQUE,ANOTHER,001,004  #   4 ||< garge-w:main
*surf,garage-ceil,CEIL,-,-,-,ceiling,OPAQUE,ANOTHER,003,006  #   5 ||< floor-garge:attic
*surf,garage-flor,FLOR,-,-,-,garage-slab,OPAQUE,GROUND,000,001  #   6 ||< user def grnd profile  1
*surf,garage-edor,VERT,-,-,-,ext-door,OPAQUE,EXTERIOR,000,000  #   7 ||< external
*surf,garage-idor,VERT,-,-,-,int_doors,OPAQUE,ANOTHER,001,012  #   8 ||< garage-idor:main
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    30.00 0  # zone base list
