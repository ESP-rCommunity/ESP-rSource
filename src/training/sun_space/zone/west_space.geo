*Geometry 1.1,GEN,west_space # tag version, format, zone name
*date Thu Aug 23 18:09:28 2007  # latest file modification 
west_space is the construction void within west wall
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,8.28600,5.58600,0.00000  #   1
*vertex,8.48600,5.93200,0.00000  #   2
*vertex,3.11600,9.03200,0.00000  #   3
*vertex,2.91600,8.68600,0.00000  #   4
*vertex,8.28600,5.58600,2.40000  #   5
*vertex,8.48600,5.93200,2.40000  #   6
*vertex,3.11600,9.03200,2.40000  #   7
*vertex,2.91600,8.68600,2.40000  #   8
*vertex,3.52300,8.33600,0.60000  #   9
*vertex,4.25900,7.91100,0.60000  #  10
*vertex,4.25900,7.91100,2.10000  #  11
*vertex,3.52300,8.33600,2.10000  #  12
*vertex,6.81300,6.43600,0.60000  #  13
*vertex,7.55000,6.01100,0.60000  #  14
*vertex,7.55000,6.01100,2.10000  #  15
*vertex,6.81300,6.43600,2.10000  #  16
*vertex,7.79300,6.33300,0.60000  #  17
*vertex,7.05600,6.75800,0.60000  #  18
*vertex,7.05600,6.75800,2.10000  #  19
*vertex,7.79300,6.33300,2.10000  #  20
*vertex,4.50200,8.23300,0.60000  #  21
*vertex,3.76600,8.65800,0.60000  #  22
*vertex,3.76600,8.65800,2.10000  #  23
*vertex,4.50200,8.23300,2.10000  #  24
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,16,2,3,22,21,24,23,22,3,7,6,2,17,20,19,18,17  #  2
*edges,4,3,4,8,7  #  3
*edges,16,4,1,14,13,16,15,14,1,5,8,4,9,12,11,10,9  #  4
*edges,4,5,6,7,8  #  5
*edges,4,1,4,3,2  #  6
*edges,4,9,22,23,12  #  7
*edges,4,21,10,11,24  #  8
*edges,4,9,10,21,22  #  9
*edges,4,11,12,23,24  # 10
*edges,4,13,18,19,16  # 11
*edges,4,17,14,15,20  # 12
*edges,4,13,14,17,18  # 13
*edges,4,15,16,19,20  # 14
# 
# surf attributes:
#  surf name, surf position VERT/CIIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,s_buf,VERT,-,-,-,int_buf,OPAQUE,ANOTHER,07,07  #   1 ||< s_west:buf_1
*surf,s_liv,VERT,-,-,-,int_westrev,OPAQUE,ANOTHER,01,04  #   2 ||< s_west:kitliv
*surf,s_adjhouse,VERT,-,-,-,ext_west,OPAQUE,SIMILAR,00,00  #   3 ||< identical environment
*surf,ext_west,VERT,-,-,-,ext_west,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,ceil,CEIL,-,-,-,int_roof,OPAQUE,ANOTHER,10,07  #   5 ||< west_sp:roof
*surf,floor,FLOR,-,-,-,house_floor,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,jambl,VERT,-,-,-,ext_west,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,jambr,VERT,-,-,-,ext_west,OPAQUE,EXTERIOR,0,0  #   8 ||< external
*surf,sill,CEIL,-,-,-,ext_west,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,head,FLOR,-,-,-,ext_west,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,jl,VERT,-,-,-,ext_west,OPAQUE,EXTERIOR,0,0  #  11 ||< external
*surf,jr,VERT,-,-,-,ext_west,OPAQUE,EXTERIOR,0,0  #  12 ||< external
*surf,sillr,CEIL,-,-,-,ext_west,OPAQUE,EXTERIOR,0,0  #  13 ||< external
*surf,headr,FLOR,-,-,-,ext_west,OPAQUE,EXTERIOR,0,0  #  14 ||< external
# 
*insol,2,10,9,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     2.48 2  # zone base list
