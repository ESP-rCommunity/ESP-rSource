*Geometry 1.1,GEN,cold_store # tag version, format, zone name
*date Mon Feb 13 15:06:37 2012  # latest file modification 
cold_store is a storage for cold product at zero degrees.
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,100.00000,69.00000,0.00000  #   1
*vertex,100.00000,78.50000,0.00000  #   2
*vertex,100.00000,69.00000,3.00000  #   3
*vertex,100.00000,78.50000,3.00000  #   4
*vertex,92.00000,69.00000,0.00000  #   5
*vertex,92.00000,69.00000,3.00000  #   6
*vertex,92.00000,78.50000,3.00000  #   7
*vertex,92.00000,78.50000,0.00000  #   8
# 
# tag, number of vertices followed by list of associated vert
*edges,4,5,1,3,6  #  1
*edges,4,1,2,4,3  #  2
*edges,4,2,8,7,4  #  3
*edges,4,6,3,4,7  #  4
*edges,4,8,2,1,5  #  5
*edges,4,8,5,6,7  #  6
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_refr,VERT,-,-,-,insul_cab,OPAQUE,ANOTHER,02,19  #   1 ||< ptn_refr:Refrig
*surf,right_fac,VERT,-,WALL,-,SMexwal2008,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,back_fac,VERT,-,WALL,-,SMexwal2008,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,ceiling,CEIL,-,-,-,insul_cab,OPAQUE,ANOTHER,10,09  #   4 ||< ptn_storage:Store2
*surf,floor,FLOR,-,FLOOR,EXTGRND,SM_grnd_floo,OPAQUE,GROUND,01,00  #   5 ||< ground profile  1
*surf,ptn_storage,VERT,-,-,-,insul_cab,OPAQUE,ANOTHER,10,11  #   6 ||< ptn_cold:Store2
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,    76.00 2  # zone base list
