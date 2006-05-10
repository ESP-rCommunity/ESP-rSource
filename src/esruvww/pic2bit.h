/* pic2bit 
   Creates a picture on an exrep bitmap from one of the following
   picture formats:-
		 viewer
*/

typedef struct _pair {
	int	x;
	int	y;
struct	_pair	*next;
struct	_pair	*prev;
}pair;

typedef struct _poly {
	 int	number;
	 int	flag;		/*    filled or edge polygon */
	 int	colour_index; 	/*    polygon colour */
	 int	brightness;	/*    polygon brightness (monochrome) */
	 int	links;  	/*    number of links in polygon */
	 pair	*points; 	/* -> polygon points list*/
  struct _poly	*next;		/* -> next polygon */
} poly;

typedef struct _colour {
	int r,g,b;
	struct _colour *next;
} Colour;
poly	*poly_data;


char    *cmalloc ();
poly 	*new_poly();
pair    *new_point();
Colour	*new_colour();
int	no_op();

