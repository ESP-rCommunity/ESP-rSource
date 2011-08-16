*Geometry 1.1,GEN,column # tag version, format, zone name
*date Wed Aug 17 06:47:47 2011  # latest file modification 
column describes a dummy zone for columns
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,4.60483,-1.95512,47.95000  #   1
*vertex,6.55995,-1.98085,47.95000  #   2
*vertex,6.55995,-0.56596,47.95000  #   3
*vertex,4.60483,-0.59168,47.95000  #   4
*vertex,4.60483,-1.95512,49.95000  #   5
*vertex,6.55995,-1.98085,49.95000  #   6
*vertex,6.55995,-0.56596,49.95000  #   7
*vertex,4.60483,-0.59168,49.95000  #   8
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,6,5  #  1
*edges,4,2,3,7,6  #  2
*edges,4,3,4,8,7  #  3
*edges,4,4,1,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,4,1,4,3,2  #  6
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Wall-1,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,00,00  #   1 ||< external
*surf,Wall-2,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,00,00  #   2 ||< external
*surf,Wall-3,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,00,00  #   3 ||< external
*surf,Wall-4,VERT,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,00,00  #   4 ||< external
*surf,Top-5,CEIL,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,00,00  #   5 ||< external
*surf,Base-6,FLOR,-,-,-,UNKNOWN,OPAQUE,EXTERIOR,00,00  #   6 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     2.72 0  # zone base list
# 
# block entities:
#  *obs = obstructions
*block_start, 20 20 # geometric blocks
*obs,-0.200,-0.600,47.950,0.100,0.200,15.440,13.000,1.00,sol_va,door  # block   1
*obs,2.200,0.200,47.950,0.100,0.200,15.440,13.000,1.00,sol_vb,door  # block   2
*obs,4.600,0.800,47.950,0.100,0.200,15.440,13.000,1.00,sol_vc,door  # block   3
*obs,7.000,1.400,47.950,0.100,0.200,15.440,13.000,1.00,sol_vd,door  # block   4
*obs,9.600,2.000,47.950,0.100,0.200,15.440,13.000,1.00,sol_ve,door  # block   5
*obs,12.000,2.800,47.950,0.100,0.200,15.440,13.000,1.00,sol_vf,door  # block   6
*obs,14.400,3.400,47.950,0.100,0.200,15.440,13.000,1.00,sol_vg,door  # block   7
*obs,16.800,4.000,47.950,0.100,0.200,15.440,13.000,1.00,sol_vh,door  # block   8
*obs,19.200,4.600,47.950,0.100,0.200,15.440,13.000,1.00,sol_vi,door  # block   9
*obs,21.600,5.200,47.950,0.100,0.200,15.440,13.000,1.00,sol_vj,door  # block  10
*obs,24.000,5.800,47.950,0.100,0.200,15.440,13.000,1.00,sol_vk,door  # block  11
*obs,26.400,6.400,47.950,0.100,0.200,15.440,13.000,1.00,sol_vl,door  # block  12
*obs,28.800,7.200,47.950,0.100,0.200,15.440,13.000,1.00,sol_vm,door  # block  13
*obs,31.200,7.800,47.950,0.100,0.200,15.440,13.000,1.00,sol_vn,door  # block  14
*obs,33.800,8.400,47.950,0.100,0.200,15.440,13.000,1.00,sol_vo,door  # block  15
*obs,13.250,40.750,51.690,1.000,1.000,13.450,0.000,1.00,vnt_a,door  # block  16
*obs,20.250,40.750,51.690,1.000,1.000,13.450,0.000,1.00,vnt_b,door  # block  17
*obs,26.750,40.750,51.690,1.000,1.000,13.450,0.000,1.00,vnt_c,door  # block  18
*obs,33.750,40.750,51.690,1.000,1.000,13.450,0.000,1.00,vnt_d,door  # block  19
*obs,40.750,40.750,51.690,1.000,1.000,13.450,0.000,1.00,vnt_e,door  # block  20
*obs,47.250,40.750,51.690,1.000,1.000,13.450,0.000,1.00,vnt_f,door  # block  21
*obs,0.750,1.000,47.950,0.400,0.400,15.440,0.000,1.00,col_at_a,door  # block  22
*obs,4.250,2.000,47.950,0.400,0.400,15.440,0.000,1.00,col_at_b,door  # block  23
*obs,9.000,3.250,47.950,0.400,0.400,15.440,0.000,1.00,col_at_c,door  # block  24
*obs,14.000,4.500,47.950,0.400,0.400,15.440,0.000,1.00,col_at_d,door  # block  25
*obs,18.750,5.750,47.950,0.400,0.400,15.440,0.000,1.00,col_at_e,door  # block  26
*obs,0.500,7.750,47.950,0.250,0.250,15.440,0.000,1.00,col_a,door  # block  27
*obs,7.000,9.250,47.950,0.250,0.250,15.440,0.000,1.00,col_b,door  # block  28
*obs,13.750,11.000,47.950,0.250,0.250,15.440,0.000,1.00,col_c,door  # block  29
*obs,17.250,11.750,47.950,0.250,0.250,15.440,0.000,1.00,col_d,door  # block  30
*obs,17.250,14.000,47.950,0.250,0.250,15.440,0.000,1.00,col_e,door  # block  31
*obs,13.750,14.000,47.950,0.250,0.250,15.440,0.000,1.00,col_f,door  # block  32
*obs,7.000,14.000,47.950,0.250,0.250,15.440,0.000,1.00,col_g,door  # block  33
*obs,0.500,14.000,47.950,0.250,0.250,15.440,0.000,1.00,col_h,door  # block  34
*obs,0.500,21.250,47.950,0.250,0.250,15.440,0.000,1.00,col_i,door  # block  35
*obs,7.000,21.250,47.950,0.250,0.250,15.440,0.000,1.00,col_j,door  # block  36
*obs,13.750,21.250,47.950,0.250,0.250,15.440,0.000,1.00,col_k,door  # block  37
*obs,17.250,21.250,47.950,0.250,0.250,15.440,0.000,1.00,col_l,door  # block  38
*obs,13.750,28.500,47.950,0.250,0.250,15.440,0.000,1.00,col_m,door  # block  39
*obs,17.250,28.500,47.950,0.250,0.250,15.440,0.000,1.00,col_n,door  # block  40
*obs,24.000,28.500,47.950,0.250,0.250,15.440,0.000,1.00,col_o,door  # block  41
*obs,27.500,28.500,47.950,0.250,0.250,15.440,0.000,1.00,col_p,door  # block  42
*obs,24.000,21.250,47.950,0.250,0.250,15.440,0.000,1.00,col_q,door  # block  43
*obs,27.500,21.250,47.950,0.250,0.250,15.440,0.000,1.00,col_r,door  # block  44
*obs,34.250,21.250,47.950,0.250,0.250,15.440,0.000,1.00,col_s,door  # block  45
*obs,38.000,21.250,47.950,0.250,0.250,15.440,0.000,1.00,col_t,door  # block  46
*obs,34.250,28.500,47.950,0.250,0.250,15.440,0.000,1.00,col_u,door  # block  47
*obs,38.000,28.500,47.950,0.250,0.250,15.440,0.000,1.00,col_v,door  # block  48
*obs,44.500,28.500,47.950,0.250,0.250,15.440,0.000,1.00,col_w,door  # block  49
*obs,44.500,21.250,47.950,0.250,0.250,15.440,0.000,1.00,col_x,door  # block  50
*obs,44.500,15.750,47.950,0.250,0.250,15.440,0.000,1.00,col_y,door  # block  51
*obs,38.000,14.250,47.950,0.250,0.250,15.440,0.000,1.00,col_z,door  # block  52
*obs,50.500,13.500,47.950,0.400,0.400,15.440,0.000,1.00,col_at_g,door  # block  53
*end_block
