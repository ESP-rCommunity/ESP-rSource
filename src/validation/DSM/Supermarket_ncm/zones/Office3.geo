*Geometry 1.1,GEN,Office3 # tag version, format, zone name
*date Tue Feb 14 10:39:49 2012  # latest file modification 
Office3 describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,32.00000,4.00000,3.00000  #   1
*vertex,0.00000,4.00000,3.00000  #   2
*vertex,0.00000,13.50000,3.00000  #   3
*vertex,32.00000,13.50000,3.00000  #   4
*vertex,32.00000,4.00000,6.00000  #   5
*vertex,0.00000,4.00000,6.00000  #   6
*vertex,0.00000,13.50000,6.00000  #   7
*vertex,32.00000,13.50000,6.00000  #   8
*vertex,27.31371,4.00000,3.43934  #   9
*vertex,4.68629,4.00000,3.43934  #  10
*vertex,4.68629,4.00000,5.56066  #  11
*vertex,27.31371,4.00000,5.56066  #  12
*vertex,32.00000,11.00000,3.00000  #  13
*vertex,32.00000,11.00000,6.00000  #  14
*vertex,32.00000,4.00000,4.09091  #  15
# 
# tag, number of vertices followed by list of associated vert
*edges,11,2,1,15,5,6,2,10,11,12,9,10  #  1
*edges,4,3,2,6,7  #  2
*edges,4,4,3,7,8  #  3
*edges,3,14,5,15  #  4
*edges,5,6,5,14,8,7  #  5
*edges,5,4,13,1,2,3  #  6
*edges,4,10,9,12,11  #  7
*edges,4,13,4,8,14  #  8
*edges,4,1,13,14,15  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,front_fac,VERT,-,WALL,-,SMexwal2008,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,left_fac,VERT,-,WALL,-,SMexwal2008,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,ptn_shop,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,01,09  #   3 ||< ptn_off3:Sales
*surf,ptn_foy,VERT,-,WALL,-,mass_part,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,roof,CEIL,-,ROOF,-,SM_roof2008,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,floor,FLOR,-,-,-,susp_flr_re,OPAQUE,ANOTHER,04,05  #   6 ||< ceiling:Office1
*surf,fac_glaz,VERT,front_fac,C-WINDOW,-,SM_glazing,DCF7671_06nb,EXTERIOR,0,0  #   7 ||< external
*surf,ptn_off,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,07,04  #   8 ||< ptn_off:passage
*surf,tri_left,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,03,11  #   9 ||< tri_left:Foyer
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,   304.00 0  # zone base list
