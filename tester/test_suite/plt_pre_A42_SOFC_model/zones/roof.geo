*Geometry 1.1,GEN,roof # tag version, format, zone name
*date Sun May  2 07:55:42 2021  # latest file modification 
roof describes the two-level roof of the house
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,8.08500  #   1
*vertex,12.00000,0.00000,8.08500  #   2
*vertex,12.00000,9.20000,8.08500  #   3
*vertex,0.00000,9.20000,8.08500  #   4
*vertex,5.75000,4.60000,11.92000  #   5
*vertex,6.25000,4.60000,11.92000  #   6
*vertex,12.00000,1.10000,9.08500  #   7
*vertex,12.00000,8.20000,9.08500  #   8
*vertex,12.00000,12.20000,8.08500  #   9
*vertex,0.00000,12.20000,8.08500  #  10
*vertex,6.00000,9.20000,10.58500  #  11
*vertex,6.00000,8.50000,10.58500  #  12
*vertex,6.00000,6.26680,10.58500  #  13
# 
# tag, number of vertices followed by list of associated vert
*edges,3,4,1,5  #   1
*edges,3,6,7,8  #   2
*edges,4,1,7,6,5  #   3
*edges,4,2,3,8,7  #   4
*edges,3,9,10,11  #   5
*edges,5,13,12,11,10,4  #   6
*edges,5,3,9,11,12,13  #   7
*edges,6,5,6,8,3,13,4  #   8
*edges,3,1,2,7  #   9
*edges,6,3,2,1,4,10,9  #  10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,roof1_back,SLOP,-,-,-,asphalt,OPAQUE,EXTERIOR,000,000  #   1 ||< external
*surf,roof1_frnt,SLOP,-,-,-,asphalt,OPAQUE,EXTERIOR,000,000  #   2 ||< external
*surf,roof1_side,SLOP,-,-,-,asphalt,OPAQUE,EXTERIOR,000,000  #   3 ||< external
*surf,roof1_vert,VERT,-,-,-,asphalt,OPAQUE,EXTERIOR,000,000  #   4 ||< external
*surf,roof2_vert,SLOP,-,-,-,asphalt,OPAQUE,EXTERIOR,000,000  #   5 ||< external
*surf,roof2_back,SLOP,-,-,-,asphalt,OPAQUE,EXTERIOR,000,000  #   6 ||< external
*surf,roof2_front,SLOP,-,-,-,asphalt,OPAQUE,EXTERIOR,000,000  #   7 ||< external
*surf,roof_1_2,SLOP,-,-,-,asphalt,OPAQUE,EXTERIOR,000,000  #   8 ||< external
*surf,roof1_tri,SLOP,-,-,-,asphalt,OPAQUE,EXTERIOR,000,000  #   9 ||< external
*surf,ceiling,FLOR,-,-,-,ceiling_r,OPAQUE,ANOTHER,002,005  #  10 ||< ceiling:main_second
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,10,   142.68 0  # zone base list
