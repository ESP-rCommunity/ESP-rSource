*Geometry 1.1,GEN,Staff_traing # tag version, format, zone name
*date Tue Feb 14 10:39:15 2012  # latest file modification 
Staff describes a room for staff changing & meetings.
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,100.00000,4.00000,3.00000  #   1
*vertex,68.00000,4.00000,3.00000  #   2
*vertex,68.00000,13.50000,3.00000  #   3
*vertex,100.00000,13.50000,3.00000  #   4
*vertex,100.00000,4.00000,6.00000  #   5
*vertex,68.00000,4.00000,6.00000  #   6
*vertex,68.00000,13.50000,6.00000  #   7
*vertex,100.00000,13.50000,6.00000  #   8
*vertex,95.31371,4.00000,3.43934  #   9
*vertex,72.68629,4.00000,3.43934  #  10
*vertex,72.68629,4.00000,5.56066  #  11
*vertex,95.31371,4.00000,5.56066  #  12
*vertex,68.00000,11.00000,3.00000  #  13
*vertex,68.00000,11.00000,6.00000  #  14
*vertex,68.00000,4.00000,4.09091  #  15
# 
# tag, number of vertices followed by list of associated vert
*edges,11,2,1,5,6,15,2,10,11,12,9,10  #  1
*edges,3,6,14,15  #  2
*edges,4,4,3,7,8  #  3
*edges,4,1,4,8,5  #  4
*edges,5,6,5,8,7,14  #  5
*edges,5,4,1,2,13,3  #  6
*edges,4,10,9,12,11  #  7
*edges,4,3,13,14,7  #  8
*edges,4,13,2,15,14  #  9
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,front_fac,VERT,-,WALL,-,SMexwal2008,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,left_fac,VERT,-,WALL,-,SMexwal2008,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,ptn_shop,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,02,01  #   3 ||< ptn_staff:Refrig
*surf,right_fac,VERT,-,WALL,-,SMexwal2008,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,roof,CEIL,-,ROOF,FLAT,SM_roof2008,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,floor,FLOR,-,-,-,susp_flr_re,OPAQUE,ANOTHER,05,05  #   6 ||< ceiling:Cafe
*surf,fac_glaz,VERT,front_fac,C-WINDOW,-,SM_glazing,DCF7671_06nb,EXTERIOR,0,0  #   7 ||< external
*surf,ptn_staff,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,07,02  #   8 ||< ptn_staff:passage
*surf,tri_right,VERT,-,-,-,mass_part,OPAQUE,ANOTHER,03,12  #   9 ||< tri_right:Foyer
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,   304.00 0  # zone base list
