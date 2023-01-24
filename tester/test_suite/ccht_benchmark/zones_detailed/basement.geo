*Geometry 1.1,GEN,basement # tag version, format, zone name
*date Wed May  5 16:30:33 2021  # latest file modification 
basement describes the basement not including the area below the
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,0.00000,0.00000,0.00000  #   1
*vertex,12.00000,0.00000,0.00000  #   2
*vertex,12.00000,6.79000,0.00000  #   3
*vertex,4.97000,6.79000,0.00000  #   4
*vertex,4.97000,12.20000,0.00000  #   5
*vertex,0.00000,12.20000,0.00000  #   6
*vertex,0.00000,0.00000,2.60000  #   7
*vertex,12.00000,0.00000,2.60000  #   8
*vertex,12.00000,6.79000,2.60000  #   9
*vertex,4.97000,6.79000,2.60000  #  10
*vertex,4.97000,12.20000,2.60000  #  11
*vertex,0.00000,12.20000,2.60000  #  12
*vertex,4.83000,0.00000,2.60000  #  13
*vertex,4.83000,2.27000,0.00000  #  14
*vertex,4.83000,2.27000,2.60000  #  15
*vertex,7.25000,2.27000,2.60000  #  16
*vertex,7.25000,0.00000,2.60000  #  17
*vertex,4.83000,0.00000,0.00000  #  18
*vertex,7.25000,0.00000,0.00000  #  19
*vertex,7.25000,2.27000,0.00000  #  20
# 
# tag, number of vertices followed by list of associated vert
*edges,4,3,4,10,9  #   1
*edges,4,4,5,11,10  #   2
*edges,4,5,6,12,11  #   3
*edges,10,7,13,15,16,17,8,9,10,11,12  #   4
*edges,10,1,6,5,4,3,2,19,20,14,18  #   5
*edges,4,13,18,14,15  #   6
*edges,4,16,15,14,20  #   7
*edges,4,17,16,20,19  #   8
*edges,4,19,2,8,17  #   9
*edges,4,1,18,13,7  #  10
*edges,4,2,3,9,8  #  11
*edges,4,6,1,7,12  #  12
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,found_lft2,VERT,-,-,-,ccht_wall,OPAQUE,GROUND,001,000  #   1 ||< ground profile  1
*surf,found_frnt2,VERT,-,-,-,foundation_,OPAQUE,GROUND,001,000  #   2 ||< ground profile  1
*surf,found_lft1,VERT,-,-,-,foundation_,OPAQUE,GROUND,001,000  #   3 ||< ground profile  1
*surf,bsment_ceiln,CEIL,-,-,-,floors_r,OPAQUE,ANOTHER,003,019  #   4 ||< main_floor:mainfloor
*surf,bsment_floor,FLOR,-,-,-,slab_floor,OPAQUE,GROUND,001,000  #   5 ||< ground profile  1
*surf,stair_north,VERT,-,-,-,int_partitn,OPAQUE,ANOTHER,005,004  #   6 ||< bsm_north:stairwell
*surf,stair_east,VERT,-,-,-,fictitious,SC_fictit,ANOTHER,005,003  #   7 ||< bsm_east:stairwell
*surf,stair_south,VERT,-,-,-,int_partitn,OPAQUE,ANOTHER,005,002  #   8 ||< bsm_south:stairwell
*surf,found_rgt2,VERT,-,-,-,foundation_,OPAQUE,GROUND,001,000  #   9 ||< ground profile  1
*surf,found_rgt1,VERT,-,-,-,foundation_,OPAQUE,GROUND,001,000  #  10 ||< ground profile  1
*surf,found_frnt1,VERT,-,-,-,foundation_,OPAQUE,GROUND,001,000  #  11 ||< ground profile  1
*surf,found_back,VERT,-,-,-,foundation_,OPAQUE,GROUND,001,000  #  12 ||< ground profile  1
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,5,   102.87 0  # zone base list
