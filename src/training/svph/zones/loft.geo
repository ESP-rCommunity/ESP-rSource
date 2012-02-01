*Geometry 1.1,GEN,loft # tag version, format, zone name
*date Tue Dec 13 17:43:59 2011  # latest file modification 
loft describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,9.77300,11.33700,4.80000  #   1
*vertex,7.45502,10.71593,4.79991  #   2
*vertex,4.75700,9.99300,4.80000  #   3
*vertex,1.36900,9.08500,4.80000  #   4
*vertex,1.92800,7.00000,4.80000  #   5
*vertex,3.00000,3.00000,4.80000  #   6
*vertex,5.60800,3.69900,4.80000  #   7
*vertex,8.06900,4.35800,4.80000  #   8
*vertex,11.40400,5.25200,4.80000  #   9
*vertex,10.58800,8.29400,4.80000  #  10
*vertex,8.27000,7.67300,4.80000  #  11
*vertex,7.25400,7.40100,4.80000  #  12
*vertex,5.31600,7.90800,4.80000  #  13
*vertex,5.01900,7.82800,4.80000  #  14
*vertex,5.27600,6.87100,4.80000  #  15
*vertex,5.83200,4.79400,4.80000  #  16
*vertex,5.34900,4.66500,4.80000  #  17
*vertex,10.58800,8.29400,7.26500  #  18
*vertex,2.18500,6.04300,7.26500  #  19
# 
# tag, number of vertices followed by list of associated vert
*edges,6,3,4,19,18,1,2  #  1
*edges,4,4,5,6,19  #  2
*edges,6,6,7,8,9,18,19  #  3
*edges,4,9,10,1,18  #  4
*edges,4,10,11,2,1  #  5
*edges,7,11,12,15,14,13,3,2  #  6
*edges,5,13,14,5,4,3  #  7
*edges,7,14,15,16,17,7,6,5  #  8
*edges,6,8,7,17,16,15,12  #  9
*edges,5,9,8,12,11,10  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,north,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,west,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,south,SLOP,-,-,-,roof,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,east,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,ceil_b3,FLOR,-,-,-,1st_ceil_rev,OPAQUE,ANOTHER,08,05  #   5 ||< ceil:bedroom_3
*surf,ceil_hall,FLOR,-,-,-,1st_ceil_rev,OPAQUE,ANOTHER,10,08  #   6 ||< ceil:upper_hall
*surf,ceil_bath,FLOR,-,-,-,1st_ceil_rev,OPAQUE,ANOTHER,05,08  #   7 ||< ceil:bath
*surf,ceil_b1,FLOR,-,-,-,1st_ceil_rev,OPAQUE,ANOTHER,06,09  #   8 ||< ceil:bedroom_1
*surf,ceil_b4,FLOR,-,-,-,1st_ceil_rev,OPAQUE,ANOTHER,09,07  #   9 ||< ceil:bedroom_4
*surf,ceil_b2,FLOR,-,-,-,1st_ceil_rev,OPAQUE,ANOTHER,07,07  #  10 ||< ceil:bedroom_2
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,54.81,1  # zone base
