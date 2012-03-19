*Geometry 1.1,GEN,Office1 # tag version, format, zone name
*date Mon Feb 13 19:47:26 2012  # latest file modification 
Office1 describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,4.00000,0.00000  #   1
*vertex,32.00000,4.00000,0.00000  #   2
*vertex,32.00000,13.50000,0.00000  #   3
*vertex,0.00000,13.50000,0.00000  #   4
*vertex,0.00000,4.00000,3.00000  #   5
*vertex,32.00000,4.00000,3.00000  #   6
*vertex,32.00000,13.50000,3.00000  #   7
*vertex,0.00000,13.50000,3.00000  #   8
*vertex,4.68629,4.00000,0.43934  #   9
*vertex,27.31371,4.00000,0.43934  #  10
*vertex,27.31371,4.00000,2.56066  #  11
*vertex,4.68629,4.00000,2.56066  #  12
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,6,5,1,9,12,11,10,9  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,4,1,4,3,2  #  6
*edges,4,9,10,11,12  #  7
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,front_fac,VERT,-,WALL,-,SMexwal2008,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,ptn_foy,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,03,13  #   2 ||< ptn_off_lft:Foyer
*surf,ptn_shop,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,01,08  #   3 ||< ptn_off1:Sales
*surf,left_fac,VERT,-,WALL,-,SMexwal2008,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,ceiling,CEIL,-,-,-,susp_floor,OPAQUE,ANOTHER,06,06  #   5 ||< floor:Office3
*surf,floor,FLOR,-,FLOOR,-,SM_grnd_floo,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,fac_glz,VERT,front_fac,C-WINDOW,-,SM_glazing,DCF7671_06nb,EXTERIOR,0,0  #   7 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,   304.00 0  # zone base list
