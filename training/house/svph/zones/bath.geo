# geometry of bath defined in: ../zones/bath.geo
GEN  bath  bath describes a...  # type, name, descr
      23      11   0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
      4.75700     9.99300     2.40000  # vert   1
      2.45100     9.37500     2.40000  # vert   2
      1.36900     9.08500     2.40000  # vert   3
      1.92800     7.00000     2.40000  # vert   4
      5.01900     7.82800     2.40000  # vert   5
      5.31600     7.90800     2.40000  # vert   6
      2.45100     9.37500     3.46600  # vert   7
      1.36900     9.08500     3.46600  # vert   8
      1.80200     7.47200     4.60000  # vert   9
      1.92800     7.00000     4.26200  # vert  10
      4.75700     9.99300     4.80000  # vert  11
      1.36900     9.08500     4.80000  # vert  12
      1.92800     7.00000     4.80000  # vert  13
      5.01900     7.82800     4.80000  # vert  14
      5.31600     7.90800     4.80000  # vert  15
      4.69900     9.97700     3.60000  # vert  16
      3.54000     9.66700     3.60000  # vert  17
      3.54000     9.66700     4.50000  # vert  18
      4.69900     9.97700     4.50000  # vert  19
      2.53000     9.39600     3.87000  # vert  20
      1.92200     9.23300     3.87000  # vert  21
      1.92200     9.23300     4.49900  # vert  22
      2.53000     9.39600     4.49900  # vert  23
# no of vertices followed by list of associated vert
  18,  1,  2,  7,  8, 21, 20, 23, 22, 21,  8, 12, 11,  1, 16, 19, 18, 17, 16,
   4,  2,  3,  8,  7,
   5,  3,  4, 10,  9,  8,
   5,  8,  9, 10, 13, 12,
   5,  4,  5, 14, 13, 10,
   4,  5,  6, 15, 14,
   4,  6,  1, 11, 15,
   5, 11, 12, 13, 14, 15,
   6,  6,  5,  4,  3,  2,  1,
   4, 16, 17, 18, 19,
   4, 20, 21, 22, 23,
# unused index
 0,0,0,0,0,0,0,0,0,0,0
# surfaces indentation (m)
 0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00,0.00
    3   0   0   0    # default insolation distribution
# surface attributes follow: 
# id  surface      geom  loc/  construction environment
# no  name         type  posn  name         other side
  1, n_ext         OPAQ  VERT  ext_wall     EXTERIOR       
  2, u_exl_bth     OPAQ  VERT  int_part_1   ext_lobby      
  3, gar_bth       OPAQ  VERT  ext_wall     garage         
  4, w_ext         OPAQ  VERT  ext_wall     EXTERIOR       
  5, bth_b1        OPAQ  VERT  int_part_2   bedroom_1      
  6, bth_uhb       OPAQ  VERT  int_part_2   Upper_hall     
  7, bth_uha       OPAQ  VERT  int_part_2   Upper_hall     
  8, ceil          OPAQ  CEIL  1st_ceiling  loft           
  9, flr_kit       OPAQ  FLOR  grd_ceil_rev kitchen        
 10, winda         TRAN  VERT  double_glaz  EXTERIOR       
 11, windb         TRAN  VERT  double_glaz  EXTERIOR       
# base
  9  0  0  0  0  0     7.57 0
