*Geometry 1.1,GEN,Store1 # tag version, format, zone name
*date Mon Feb 13 19:53:27 2012  # latest file modification 
Store1 describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,69.00000,0.00000  #   1
*vertex,0.00000,78.50000,0.00000  #   2
*vertex,0.00000,69.00000,3.00000  #   3
*vertex,0.00000,78.50000,3.00000  #   4
*vertex,65.81139,78.50000,1.02566  #   5
*vertex,34.18861,78.50000,1.02566  #   6
*vertex,34.18861,78.50000,1.97434  #   7
*vertex,65.81139,78.50000,1.97434  #   8
*vertex,68.00000,69.00000,0.00000  #   9
*vertex,68.00000,69.00000,3.00000  #  10
*vertex,68.00000,78.50000,0.00000  #  11
*vertex,68.00000,78.50000,3.00000  #  12
# 
# tag, number of vertices followed by list of associated vert
*edges,10,11,2,4,12,11,5,8,7,6,5  #  1
*edges,4,2,1,3,4  #  2
*edges,4,3,10,12,4  #  3
*edges,4,1,2,11,9  #  4
*edges,4,5,6,7,8  #  5
*edges,4,1,9,10,3  #  6
*edges,4,9,11,12,10  #  7
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,back_fac,VERT,-,WALL,-,SMexwal2008,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,left_fac,VERT,-,WALL,-,SMexwal2008,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,ceiling,CEIL,-,-,-,susp_floor,OPAQUE,ANOTHER,10,06  #   3 ||< floor:Store2
*surf,floor,FLOR,-,FLOOR,EXTGRND,SM_grnd_floo,OPAQUE,GROUND,01,00  #   4 ||< ground profile  1
*surf,fac_glaz,VERT,back_fac,C-WINDOW,CLOSED,SM_glazing,DCF7671_06nb,EXTERIOR,0,0  #   5 ||< external
*surf,ptn_sales,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,01,10  #   6 ||< ptn_st1:Sales
*surf,ptn_storage,VERT,-,-,-,insul_cab,OPAQUE,ANOTHER,10,10  #   7 ||< ptn_st1:Store2
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,1350.00,1  # zone base
