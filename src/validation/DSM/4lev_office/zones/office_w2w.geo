*Geometry 1.1,GEN,office_w2w # tag version, format, zone name
*date Mon Apr  2 09:25:05 2012  # latest file modification 
office_w2w is the west facing perimeter of 2nd level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,10.00000,8.00000,8.05000  #   1
*vertex,10.00000,26.00000,8.05000  #   2
*vertex,10.00000,8.00000,10.85000  #   3
*vertex,10.00000,26.00000,10.85000  #   4
*vertex,10.00000,12.00000,8.85000  #   5
*vertex,10.00000,15.00000,8.85000  #   6
*vertex,10.00000,15.00000,10.25000  #   7
*vertex,10.00000,12.00000,10.25000  #   8
*vertex,10.00000,20.00000,8.85000  #   9
*vertex,10.00000,20.00000,10.25000  #  10
*vertex,10.00000,17.30000,8.85000  #  11
*vertex,10.00000,17.30000,10.25000  #  12
*vertex,10.00000,16.00000,8.05000  #  13
*vertex,10.00000,16.00000,10.85000  #  14
*vertex,15.00000,13.00000,8.05000  #  15
*vertex,15.00000,13.00000,10.85000  #  16
*vertex,15.00000,21.00000,8.05000  #  17
*vertex,15.00000,21.00000,10.85000  #  18
*vertex,10.10000,16.00000,8.15000  #  19
*vertex,14.90000,16.00000,8.15000  #  20
*vertex,14.90000,16.00000,10.75000  #  21
*vertex,10.10000,16.00000,10.75000  #  22
# 
# tag, number of vertices followed by list of associated vert
*edges,10,13,1,3,14,13,6,7,8,5,6  #  1
*edges,5,3,16,18,4,14  #  2
*edges,5,1,13,2,17,15  #  3
*edges,4,6,5,8,7  #  4
*edges,4,9,11,12,10  #  5
*edges,10,2,13,14,4,2,9,10,12,11,9  #  6
*edges,4,1,15,16,3  #  7
*edges,4,17,2,4,18  #  8
*edges,4,15,17,18,16  #  9
*edges,4,20,19,22,21  # 10
*edges,4,19,20,21,22  # 11
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,east,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   1 ||< external
*surf,top,CEIL,-,-,-,ceiling_dr,OPAQUE,ANOTHER,18,15  #   2 ||< top_w2w:ceiling_3
*surf,bottom,FLOR,-,-,-,inter_flr,OPAQUE,ANOTHER,17,15  #   3 ||< bottom_wo:ceiling_2
*surf,window_est_1,VERT,-,-,-,double_glz,DC_8074_04nb,EXTERIOR,0,0  #   4 ||< external
*surf,window_est_2,VERT,-,-,-,double_glz,DC_8074_04nb,EXTERIOR,0,0  #   5 ||< external
*surf,east_b,VERT,-,-,-,ext_wall,OPAQUE,EXTERIOR,0,0  #   6 ||< external
*surf,sw_ptn,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,12,06  #   7 ||< sw_ptn:office_2_sth
*surf,nw_ptn,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,11,05  #   8 ||< nw_ptn:office_2_nor
*surf,corid_b,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,15,14  #   9 ||< west_p_cor:passage_2
*surf,wf_ptn_e,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,14,11  #  10 ||< wf_ptn_ee:office_w2w
*surf,wf_ptn_ee,VERT,-,-,-,partn_ltw,OPAQUE,ANOTHER,14,10  #  11 ||< wf_ptn_e:office_w2w
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,all_applicable   2 # insolation sources
  4  5
# 
*base_list,0,65.00,1  # zone base
