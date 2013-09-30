*Geometry 1.1,GEN,living # tag version, format, zone name
*date Wed Jul 22 14:18:47 2009  # latest file modification 
living describes a...
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,2.71000,0.00000  #   1
*vertex,2.56000,2.71000,0.00000  #   2
*vertex,5.40000,2.71000,0.00000  #   3
*vertex,5.40000,8.67000,0.00000  #   4
*vertex,0.00000,8.67000,0.00000  #   5
*vertex,0.00000,2.71000,2.70000  #   6
*vertex,2.56000,2.71000,2.70000  #   7
*vertex,5.40000,2.71000,2.70000  #   8
*vertex,5.40000,8.67000,2.70000  #   9
*vertex,0.00000,8.67000,2.70000  #  10
*vertex,4.40000,8.67000,0.38000  #  11
*vertex,3.87000,8.67000,0.38000  #  12
*vertex,3.87000,8.67000,2.00000  #  13
*vertex,4.40000,8.67000,2.00000  #  14
*vertex,2.90000,8.67000,0.55000  #  15
*vertex,0.75000,8.67000,0.55000  #  16
*vertex,0.75000,8.67000,2.00000  #  17
*vertex,2.90000,8.67000,2.00000  #  18
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,7,6  #  1
*edges,4,2,3,8,7  #  2
*edges,4,3,4,9,8  #  3
*edges,16,4,5,16,15,18,17,16,5,10,9,4,11,14,13,12,11  #  4
*edges,4,5,1,6,10  #  5
*edges,5,6,7,8,9,10  #  6
*edges,5,1,5,4,3,2  #  7
*edges,4,11,12,13,14  #  8
*edges,4,15,16,17,18  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_hall,VERT,-,-,-,Inner_wall,OPAQUE,CONSTANT,15,00  #   1 ||< constant @ 15dC &   0W rad
*surf,ptn_kitch,VERT,-,-,-,Inner_wall,OPAQUE,CONSTANT,18,00  #   2 ||< constant @ 18dC &   0W rad
*surf,east_neigh,VERT,-,-,-,Partition,OPAQUE,SIMILAR,00,00  #   3 ||< identical environment
*surf,north,VERT,-,-,-,Facade,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,west_neigh,VERT,-,-,-,Partition,OPAQUE,SIMILAR,00,00  #   5 ||< identical environment
*surf,ceiling,CEIL,-,-,-,Inner_floor,OPAQUE,CONSTANT,18,00  #   6 ||< constant @ 18dC &   0W rad
*surf,floor,FLOR,-,-,-,Ground_floor,OPAQUE,GROUND,00,01  #   7 ||< user def grnd profile  1
*surf,north_gla,VERT,-,-,-,double_glz,DC_8074_04nb,EXTERIOR,0,0  #   8 ||< external
*surf,north_glb,VERT,-,-,-,double_glz,DC_8074_04nb,EXTERIOR,0,0  #   9 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,7,    32.18 0  # zone base list
