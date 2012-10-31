*Geometry 1.1,GEN,Foyer # tag version, format, zone name
*date Mon Feb 13 19:42:54 2012  # latest file modification 
Foyer describes a
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,32.00000,0.00000,0.00000  #   1
*vertex,68.00000,0.00000,0.00000  #   2
*vertex,68.00000,13.50000,0.00000  #   3
*vertex,32.00000,13.50000,0.00000  #   4
*vertex,32.00000,0.00000,3.00000  #   5
*vertex,68.00000,0.00000,3.00000  #   6
*vertex,68.00000,13.50000,3.00000  #   7
*vertex,32.00000,13.50000,3.00000  #   8
*vertex,37.27208,0.00000,0.43934  #   9
*vertex,62.72792,0.00000,0.43934  #  10
*vertex,62.72792,0.00000,2.56066  #  11
*vertex,37.27208,0.00000,2.56066  #  12
*vertex,32.00000,11.00000,3.00000  #  13
*vertex,68.00000,11.00000,3.00000  #  14
*vertex,37.27208,11.00000,3.43934  #  15
*vertex,62.72792,11.00000,3.43934  #  16
*vertex,62.72792,11.00000,5.56066  #  17
*vertex,37.27208,11.00000,5.56066  #  18
*vertex,68.00000,11.00000,6.00000  #  19
*vertex,32.00000,11.00000,6.00000  #  20
*vertex,32.00000,4.00000,0.00000  #  21
*vertex,32.00000,4.00000,3.00000  #  22
*vertex,68.00000,4.00000,0.00000  #  23
*vertex,68.00000,4.00000,3.00000  #  24
*vertex,68.00000,4.00000,4.09091  #  25
*vertex,32.00000,4.00000,4.09091  #  26
*vertex,41.95016,3.04032,3.82918  #  27
*vertex,58.04984,3.04032,3.82918  #  28
*vertex,58.04984,7.95967,5.17082  #  29
*vertex,41.95016,7.95967,5.17082  #  30
*vertex,32.00000,3.50000,0.00000  #  31
*vertex,32.00000,0.50000,0.00000  #  32
*vertex,32.00000,0.50000,2.50000  #  33
*vertex,32.00000,3.50000,2.50000  #  34
*vertex,68.00000,0.50000,0.00000  #  35
*vertex,68.00000,3.50000,0.00000  #  36
*vertex,68.00000,3.50000,2.50000  #  37
*vertex,68.00000,0.50000,2.50000  #  38
# 
# tag, number of vertices followed by list of associated vert
*edges,10,1,2,6,5,1,9,12,11,10,9  #  1
*edges,9,2,35,38,37,36,23,24,25,6  #  2
*edges,4,3,4,8,7  #  3
*edges,9,21,31,34,33,32,1,5,26,22  #  4
*edges,12,5,6,25,19,20,26,5,27,30,29,28,27  #  5
*edges,10,1,32,31,21,4,3,23,36,35,2  #  6
*edges,4,9,10,11,12  #  7
*edges,4,8,13,14,7  #  8
*edges,4,16,15,18,17  #  9
*edges,10,14,13,15,16,17,18,15,13,20,19  # 10
*edges,4,13,22,26,20  # 11
*edges,4,24,14,19,25  # 12
*edges,5,4,21,22,13,8  # 13
*edges,5,23,3,7,14,24  # 14
*edges,4,27,28,29,30  # 15
*edges,4,31,32,33,34  # 16
*edges,4,35,36,37,38  # 17
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,entrance,VERT,-,WALL,-,SMexwal2008,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,right_fac,VERT,-,WALL,-,SMexwal2008,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,ptn_shop,VERT,-,-,-,fictitious,TRAN,ANOTHER,01,07  #   3 ||< ptn_foy:Sales
*surf,left-fac,VERT,-,WALL,-,SMexwal2008,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,roof,SLOP,-,ROOF,-,SM_roof2008,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,floor_gr,FLOR,-,FLOOR,-,SM_grnd_floo,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,entry_glz,VERT,entrance,C-WINDOW,-,SM_glazing,TRAN,EXTERIOR,0,0  #   7 ||< external
*surf,floor,CEIL,-,-,-,susp_floor,OPAQUE,ANOTHER,07,06  #   8 ||< floor:passage
*surf,inner_glaz,VERT,ptn_corid,-,-,dbl_glz,TRAN,ANOTHER,07,07  #   9 ||< inner_glaz:passage
*surf,ptn_corid,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,07,01  #  10 ||< ptn_foy:passage
*surf,tri_left,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,06,09  #  11 ||< tri_left:Office3
*surf,tri_right,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,08,09  #  12 ||< tri_right:Staff_traing
*surf,ptn_off_lft,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,04,02  #  13 ||< ptn_foy:Office1
*surf,ptn_cafe,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,05,04  #  14 ||< ptn_foy:Cafe
*surf,skylight,SLOP,roof,S-WINDOW,CLOSED,dbl_glz,TRAN,EXTERIOR,0,0  #  15 ||< external
*surf,left_door,VERT,left-fac,D-WINDOW,CLOSED,dbl_glz,TRAN,EXTERIOR,0,0  #  16 ||< external
*surf,right_door,VERT,right_fac,D-WINDOW,CLOSED,dbl_glz,TRAN,EXTERIOR,0,0  #  17 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,0,486.00,1  # zone base
