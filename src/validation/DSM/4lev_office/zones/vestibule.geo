*Geometry 1.1,GEN,vestibule # tag version, format, zone name
*date Mon Apr  2 15:30:02 2012  # latest file modification 
vestibule is 3m deep zone adjacent to entry_core
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,26.00000,26.00000,0.35000  #   1
*vertex,24.00000,26.00000,0.35000  #   2
*vertex,24.00000,26.00000,3.15000  #   3
*vertex,26.00000,26.00000,3.15000  #   4
*vertex,25.89443,26.00000,0.49780  #   5
*vertex,25.89443,26.00000,3.00220  #   6
*vertex,24.10557,26.00000,3.00220  #   7
*vertex,24.10557,26.00000,0.49780  #   8
*vertex,26.00000,23.00000,0.35000  #   9
*vertex,24.00000,23.00000,0.35000  #  10
*vertex,24.00000,23.00000,3.15000  #  11
*vertex,26.00000,23.00000,3.15000  #  12
*vertex,25.89443,23.00000,0.49780  #  13
*vertex,25.89443,23.00000,3.00220  #  14
*vertex,24.10557,23.00000,3.00220  #  15
*vertex,24.10557,23.00000,0.49780  #  16
# 
# tag, number of vertices followed by list of associated vert
*edges,4,9,10,2,1  #  1
*edges,10,1,2,3,4,1,5,6,7,8,5  #  2
*edges,4,5,8,7,6  #  3
*edges,10,10,9,13,16,15,14,13,9,12,11  #  4
*edges,4,16,13,14,15  #  5
*edges,4,2,10,11,3  #  6
*edges,4,3,11,12,4  #  7
*edges,4,9,1,4,12  #  8
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,floor,FLOR,-,FLOOR,EXTGRND,foundation,OPAQUE,GROUND,0,01  #   1 ||< user def grnd profile  1
*surf,entry_frame,VERT,-,-,-,window_frame,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,entry_door,VERT,entry_frame,-,-,double_glz,DC_8074_04nb,EXTERIOR,0,0  #   3 ||< external
*surf,inner_frame,VERT,-,-,-,window_frame,OPAQUE,ANOTHER,07,20  #   4 ||< inner_frame:entry_core
*surf,inner_door,VERT,inner_frame,-,-,double_glz,DC_8074_04nb,ANOTHER,07,21  #   5 ||< inner_door:entry_core
*surf,west_2,VERT,-,-,-,ext_wall_r,OPAQUE,ANOTHER,02,20  #   6 ||< ptn_vest:office_w_0
*surf,upper,CEIL,-,-,-,ceiling_tl_r,OPAQUE,ANOTHER,16,18  #   7 ||< upper:ceiling_g
*surf,ptn_2,VERT,-,-,-,ext_wall_r,OPAQUE,ANOTHER,01,20  #   8 ||< ptn_2:office_e_0
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,1,     6.00 0  # zone base list
