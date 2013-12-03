*Geometry 1.1,GEN,Cabinet_lw # tag version, format, zone name
*date Mon Feb 13 15:06:04 2012  # latest file modification 
Cabinet_lw describes a low temperature frozen cabinet
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,77.50000,20.00000,0.00000  #   1
*vertex,93.50000,20.00000,0.00000  #   2
*vertex,93.50000,40.00000,0.00000  #   3
*vertex,77.50000,40.00000,0.00000  #   4
*vertex,77.50000,20.00000,2.50000  #   5
*vertex,93.50000,20.00000,2.50000  #   6
*vertex,93.50000,40.00000,2.50000  #   7
*vertex,77.50000,40.00000,2.50000  #   8
*vertex,77.51000,20.01000,1.50000  #   9
*vertex,93.49000,20.01000,1.50000  #  10
*vertex,93.49000,39.99000,1.50000  #  11
*vertex,77.51000,39.99000,1.50000  #  12
*vertex,77.51000,20.01000,1.00000  #  13
*vertex,93.49000,20.01000,1.00000  #  14
*vertex,93.49000,39.99000,1.00000  #  15
*vertex,77.51000,39.99000,1.00000  #  16
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,4,1,4,3,2  #  6
*edges,4,9,10,11,12  #  7
*edges,4,10,9,12,11  #  8
*edges,4,13,14,15,16  #  9
*edges,4,14,13,16,15  # 10
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,lt_cab_fr,VERT,-,-,-,insul_cab,OPAQUE,ANOTHER,02,11  #   1 ||< cab_fr:Refrig
*surf,lt_cab_ri,VERT,-,-,-,insul_cab,OPAQUE,ANOTHER,02,12  #   2 ||< cab_ri:Refrig
*surf,lt_cab_bk,VERT,-,-,-,insul_cab,OPAQUE,ANOTHER,02,13  #   3 ||< cab_bk:Refrig
*surf,lt_cab_lft,VERT,-,-,-,insul_cab,OPAQUE,ANOTHER,02,14  #   4 ||< cab_lft:Refrig
*surf,lt_cab_top,CEIL,-,-,-,fictitious,SC_fictit,ANOTHER,02,15  #   5 ||< cab_top:Refrig
*surf,lt_cab_base,FLOR,-,FLOOR,EXTGRND,SM_grnd_cab,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
*surf,up_TM_fu,CEIL,-,-,-,mass_part,OPAQUE,ANOTHER,12,08  #   7 ||< up_TM_fd:Cabinet_lw
*surf,up_TM_fd,FLOR,-,-,-,mass_part,OPAQUE,ANOTHER,12,07  #   8 ||< up_TM_fu:Cabinet_lw
*surf,low_TM_fu,CEIL,-,-,-,mass_part,OPAQUE,ANOTHER,12,10  #   9 ||< low_TM_fd:Cabinet_lw
*surf,low_TM_fd,FLOR,-,-,-,mass_part,OPAQUE,ANOTHER,12,09  #  10 ||< low_TM_fu:Cabinet_lw
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,   320.00 2  # zone base list
