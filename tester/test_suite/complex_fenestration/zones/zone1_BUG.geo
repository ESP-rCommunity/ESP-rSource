*Geometry 1.1,GEN,zone1 # tag version, format, zone name
*date Wed Dec  4 09:03:30 2019  # latest file modification 
zone1 describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,5.00000,0.00000,0.00000  #   2
*vertex,5.00000,3.00000,0.00000  #   3
*vertex,0.00000,3.00000,0.00000  #   4
*vertex,0.00000,0.00000,2.70000  #   5
*vertex,5.00000,0.00000,2.70000  #   6
*vertex,5.00000,3.00000,2.70000  #   7
*vertex,0.00000,3.00000,2.70000  #   8
*vertex,0.25000,0.00000,0.25000  #   9
*vertex,2.25000,0.00000,0.25000  #  10
*vertex,2.25000,0.00000,2.25000  #  11
*vertex,0.25000,0.00000,2.25000  #  12
*vertex,2.75000,0.00000,0.25000  #  13
*vertex,4.75000,0.00000,0.25000  #  14
*vertex,4.75000,0.00000,2.25000  #  15
*vertex,2.75000,0.00000,2.25000  #  16
*vertex,0.00000,2.50000,0.25000  #  17
*vertex,0.00000,0.50000,0.25000  #  18
*vertex,0.00000,0.50000,2.25000  #  19
*vertex,0.00000,2.50000,2.25000  #  20
# 
# tag, number of vertices followed by list of associated vert
*edges,16,1,2,14,13,16,15,14,2,6,5,1,9,12,11,10,9  #   1
*edges,4,2,3,7,6  #   2
*edges,4,3,4,8,7  #   3
*edges,10,4,1,5,8,4,17,20,19,18,17  #   4
*edges,4,5,6,7,8  #   5
*edges,4,1,4,3,2  #   6
*edges,4,9,10,11,12  #   7
*edges,4,13,14,15,16  #   8
*edges,4,17,18,19,20  #   9
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Wall-1,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,00,000  #   1 ||< external
*surf,Wall-2,VERT,-,-,-,partition,OPAQUE,ANOTHER,02,004  #   2 ||< Wall-4:zone2
*surf,Wall-3,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,00,000  #   3 ||< external
*surf,Wall-4,VERT,-,-,-,extern_wall,OPAQUE,EXTERIOR,00,000  #   4 ||< external
*surf,Top-5,CEIL,-,-,-,extern_wall,OPAQUE,EXTERIOR,00,000  #   5 ||< external
*surf,Base-6,FLOR,-,-,-,extern_wall,OPAQUE,EXTERIOR,00,000  #   6 ||< external
*surf,window1,VERT,-,-,-,glz_glz_BUG,CFC2,EXTERIOR,00,000  #   7 ||< external
*surf,window2,VERT,-,-,-,glz_glz_BUG2,CFC2,EXTERIOR,00,000  #   8 ||< external
*surf,window3,VERT,-,-,-,glz_glz_BUG2,CFC2,EXTERIOR,00,000  #   9 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,    15.00 0  # zone base list
