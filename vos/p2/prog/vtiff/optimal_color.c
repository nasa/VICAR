/*
 *    TABLE: -- Convert 24 bit RGB images into 8 bit pseudocolor
 *
 *         Description:
 *
 *            The TABLE module allows the creation of table structures
 *            which may be used to gather color histogram data for an
 *            RGB image, compute an optimal color table, and convert
 *            the RGB into an index-color table pair.
 *
 *         Color conversion Algorithm:
 *
 *            Currently the TABLE module converts RGB into a 12-bit
 *            packed integer, by taking the first 4 bits from each of
 *            the RGB byte values. As colors are added to the palette of
 *            the table, a histogram is kept of each color. When the
 *            process of adding colors is complete, the colors are sorted
 *            and the most common are picked for the color table. In addition,
 *            the color white and black are added to the table for completeness.
 * 
 *            A lookup table is used to map the 12-bit integers to the correct
 *            color table index value. The table is initialized by computing
 *            the index values for just the colors used to compute the histogram,
 *            and others are added as requests are made to compute indices for
 *            new RGB triplets.
 *
 *            Eventually, the optimization will be performed on a packed integer
 *            based on the YCbCr (luminance-chrominance) model, using seven bits
 *            for the Y component, and 4 each for the chrominance. This will
 *            be transparent to the client program, as the subroutine interface
 *            will be the same.
 *
 *         Compatibility:
 *  
 *            This code should work on either 2-byte or 4-byte int compilers
 *            and does not depend upon byte order, except that the temporary
 *            files derived from table_pack_rgb will have the 2-byte byte-order
 *            of the native machine, and so should not be moved to other hosts.
 *
 *         Revision History:
 *
 *            Original program by	Alan Mazer	fall 1988
 *	      Modularized               Niles Ritter	2 June 1993
 *	      Add new routines:		Niles Ritter	15 June 1993
 *		table_ncolors
 *		table_increment_color
 */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>

#ifndef NULL
#define NULL ((void*)0)
#endif

#define BITS_USED 4

/* useful macros */

#define RGB_INDEX( red,grn,blu )        \
       ( *(shift_left_red   + (red))  | \
         *(shift_left_green + (grn))  | \
         *(shift_left_blue  + (blu)) )
#define BYTE_COPY( src,dst,count) (memcpy((dst),(src),(count)))
#define BYTE_ZERO( src,count) (memset((src),0,(count)))

struct entry_def {
    unsigned char red,green,blue;    /* unpacked color: each ranges from 0 to 16 */
    unsigned int color;              /* packed color:  ranges from 0 to 4096    */
    long count;                      /* # times color found in sampling          */
};

struct table_def {
	struct entry_def *palette;   /* Palette of all colors found in image    */
	int *colormap;               /* Table mapping packed color to LUT index */
	int bits_used;               /* Number of bits used in packing (==4)    */
	int ncolors;                 /* Number of colors desired in LUT         */
	long length;                 /* Dimension of Palette (==4096)          */
};

/* public routines */
static public_routines(){}
int  table_new();
void table_free();
void table_add_colors();
long table_build();
void table_extract_lut();
void table_rgb_to_index();
void table_packed_to_index();
void table_pack_rgb();
void table_increment_color();
int  table_ncolors();



/* private routines */
static private_routines(){}
static void initialize_tables();
static long merge();
static long merge_new_index();
static int compare();
static int ShellSort();
void swap_entries();


/* private data */
static private_data(){}
static  int first_time=1;
static  long square[513];
static  unsigned int shift_left_red[256];
static  unsigned int shift_left_green[256];
static  unsigned int shift_left_blue[256];
static  unsigned int shift_left[256];



/**
 **  *****  PUBLIC IMPLEMENTATION ROUTINES  ******
 **/


/**
 **   table_new:
 **      creates new table structure.
 **      currently the table posesses a palette,
 **      used to gather the RGB color cube histogram,
 **      and a colormap array, which maps the packed
 **      RGB value to a sub-palette of <ncolors> colors.
 **
 **      Returns: 1 if successful, 0 if failure.
 **/



int table_new(  tab, ncolors )
int **tab;
int ncolors;
{
    int bits2;
    int i;
    int *colormap;
    long length;
    register int index;
    struct table_def *table=(struct table_def *)0;
    struct entry_def *palette;
    unsigned char pix_red,pix_green,pix_blue;

    if (first_time) initialize_tables();
    if (ncolors > 256 || ncolors < 0) goto failure;
    if (ncolors == 0) ncolors = 256;
   
             /* useful values */

    bits2 = 1<<BITS_USED;
    length = bits2 * bits2 * bits2;
 
	    /* allocate space for color table */

    table = (struct table_def *)calloc( 1L, sizeof( struct table_def) );
    if (!table) goto failure;
   
    table->length = length;
    table->bits_used = BITS_USED;  /* currently hardwired to 5 */
    table->ncolors = ncolors;
       

	/* initialize palette */

    table->palette = (struct entry_def *)calloc( 1L,
          sizeof(struct entry_def) * length);
    if (!table->palette) goto failure;
    palette = table->palette;

    for (pix_red=0;(int)pix_red < bits2; pix_red++)
      for (pix_green=0;(int)pix_green < bits2; pix_green++)
        for (pix_blue=0; (int)pix_blue < bits2; pix_blue++) {
            index = pix_red << 2*BITS_USED | pix_green << BITS_USED | pix_blue;
            (palette+index)->color = index;
            (palette+index)->red = pix_red;
            (palette+index)->green = pix_green;
            (palette+index)->blue = pix_blue;
        }

           /* init colormap to -1: flags that color is not mapped yet */
           
    colormap = (int *)malloc( sizeof(int) * length);
    table->colormap = colormap;
    if (!table->colormap) goto failure;
    for (i=0;i<length;i++)
       *colormap++ = -1;


           /* set pointer and return success flag */
    
    *tab = (int *)table;    
    return (1);
    
failure:
    table_free( table );
    return (0);
}



/**
 **   table_free: destruction method for table structure.
 **/


void table_free( table )
struct table_def *table;
{
    if (table)
    {
    	if (table->palette) free(table->palette);
    	if (table->colormap) free(table->colormap);
    	free (table);
    }
}


/**
 ** table_add_colors - adds the specified arrays of RGB
 **  pixels to the palette, for eventual sorting and
 **  optimization.
 **/

void table_add_colors(table, red, green, blue, npix)
struct table_def *table;
unsigned char *red,*green,*blue;
long npix;
{
    struct entry_def *palette = table->palette;
    register long pixel;
    register unsigned int color;
    register unsigned char *red_ptr,*green_ptr,*blue_ptr;
 
	/*
         *   augment the color histogram with new colors
	 */

    red_ptr = red;
    green_ptr = green;
    blue_ptr = blue;
    for (pixel=0;pixel<npix;pixel++) {
	color = RGB_INDEX( *red_ptr++, *green_ptr++, *blue_ptr++ );
	(palette+color)->count++;
    }

}



/*
 * table_increment_color:
 *    increment the histogram count of single color specified.
 */

void table_increment_color(table, red, green, blue, number)
    struct table_def *table;
    unsigned char red;
    unsigned char green;
    unsigned char blue;
    long number;
{
    unsigned int color;
    struct entry_def *palette = table->palette;

        color = RGB_INDEX(red, green, blue);
        (palette+color)->count += number;
}



/*
 *  table_ncolors:
 *    return the number of lut colors set in table_new
 */


int table_ncolors( table )
struct table_def *table;
{
	return( table->ncolors );
}




/**
 **   table_build: gather color gamut information collected
 **     from the (red,grn,blue) arrays, and set up an
 **     optimal <ncolors>-color mapping for that set of colors.
 **     returns max error distance in colorspace.
 **/

long table_build( table )
struct table_def *table;
{
    int compare(),max_error;
    register long num_colors_found;
    struct entry_def *palette=table->palette,temp;
    long ncolors=table->ncolors;
    long length=table->length;
    register long source;

	/* compress palette by tossing out all colors with zero counts */

    source = 1;                  /* dont toss out black at 0 */
    palette[length-1].count = 1; /* dont toss out white, either */
    for (num_colors_found = 1;;num_colors_found++) {
	while ((palette+source)->count == 0 && source < length) source++;
	if (source == length) break;
	BYTE_COPY((char *)(palette+source),(char *)(palette+num_colors_found),
	    sizeof(*palette));
	source++;
    }
 
	    /*
	     *   Exclude BLACK and WHITE in the gamut from
	     *   the histogram sorting process. The palette will
	     *   eventually look like this:
	     *
	     *   WHITE, p[1], p[2], ... , BLACK, <everybody else>.
	     *
	     *    XXX-should also include points of extreme color.
	     */

	    /* sort the remainder to determine colors to use */

    ShellSort((char *)(palette+1),num_colors_found-2,
         (long)sizeof(*palette),compare);





	    /* 
	     * Force the Lightest color into the table 
	     * and swap BLACK and WHITE.
	     */


    /* Add WHITE to table */
    swap_entries(palette, ncolors-1, num_colors_found-1);
    
    /* swap WHITE <-> BLACK */
    swap_entries(palette, 0L, ncolors-1);
        
	    /* merge infrequent colors to nearest class */

    max_error = merge(table,num_colors_found);

    return (max_error);
}

/**
 ** table_extract_lut:
 **  return the lookup table used, in a public format
 **/
  
void table_extract_lut( table, red_lut, green_lut, blue_lut)
struct table_def *table;
unsigned char *red_lut,*green_lut,*blue_lut;
{
    int dn,bits_used=table->bits_used;
    register int ncolors = table->ncolors;
    register struct entry_def *palette = table->palette;  
	/* determine lookup table */

    for (dn=0;dn < ncolors;dn++) {
	red_lut[dn]   = shift_left[ (palette+dn)->red   ];
	green_lut[dn] = shift_left[ (palette+dn)->green ];
	blue_lut[dn]  = shift_left[ (palette+dn)->blue  ];
    }
}


/**
 **  table_rgb_to_index
 **   Compress RGB array to indexed value.
 **/


void table_rgb_to_index( table, red, green, blue, npix, index )
struct table_def *table;
unsigned char *red,*green,*blue;
long npix;
unsigned char *index;
{
    int ncolors = table->ncolors;
    unsigned char pix_red,pix_green,pix_blue;
    register long pixel;
    register long color;
    register unsigned char *red_ptr,*green_ptr,*blue_ptr;
    register int *ColorMap = table->colormap;
    

	/* create new red, green, and blue planes */

    red_ptr = red;
    green_ptr = green;
    blue_ptr = blue;
    for (pixel=0;pixel<npix;pixel++) {

        color = RGB_INDEX( *red_ptr++, *green_ptr++, *blue_ptr++ );
	 
                /* If table hasn't seen this; add to ColorMap */
                
	if (*(ColorMap+color) < 0) merge_new_index( table, color );
	    
	*(index+pixel) = *(ColorMap+color);
    }
}


/**
 **  table_packed_to_index
 **   Compress 2-byte packed color to indexed value.
 **/


void table_packed_to_index( table, pack, npix, index )
struct table_def *table;
unsigned short *pack;
long npix;
unsigned char *index;
{
    int ncolors = table->ncolors;
    register long pixel;
    register long color;
    register unsigned short *color_ptr=pack;
    register int *ColorMap = table->colormap;
    

	/* create new red, green, and blue planes */

    for (pixel=0;pixel<npix;pixel++) {

        color = *color_ptr++;
	 
                /* If table hasn't seen this; add to ColorMap */
                
	if (*(ColorMap+color) < 0) merge_new_index( table, color );
	    
	*index++ = *(ColorMap+color);
    }
}


/**
 **  table_pack_rgb
 **   Compress RGB array to 2-byte packed representaton.
 **/


void table_pack_rgb( table, red, green, blue, npix, pack )
struct table_def *table;
unsigned char *red,*green,*blue;
long npix;
unsigned short *pack;
{
    register long pixel;
    register unsigned char *red_ptr,*green_ptr,*blue_ptr;
    
    red_ptr = red;
    green_ptr = green;
    blue_ptr = blue;
    for (pixel=0;pixel<npix;pixel++) {

        *pack++ = RGB_INDEX( *red_ptr++, *green_ptr++, *blue_ptr++ );

    }
}


/**
 **  *****  PRIVATE IMPLEMENTATION ROUTINES  ******
 **/

/**
 **  swap_entries
 **    swaps  two color entries in a palette
 **/


void swap_entries(palette, index1, index2 )
struct entry_def *palette;
long index1;
long index2;
{
   struct entry_def temp;
  
   BYTE_COPY( palette+index1, &temp,          sizeof(temp));
   BYTE_COPY( palette+index2, palette+index1, sizeof(temp));
   BYTE_COPY( &temp,          palette+index2, sizeof(temp));
}



/**
 **  initialize_tables:
 **   sets up data arrays for module.
 **/

static void initialize_tables()
{
    register long int_temp;
 
    if (!first_time) return;
 
	/* make table for squares of differences */

    for (int_temp = -256;int_temp <= 256;int_temp++)
	square[int_temp+256] = int_temp*int_temp;

	/* make tables for right and left shifts */

    for (int_temp = 0;int_temp < 256;int_temp++) {
	shift_left_red[int_temp] =   (int_temp >> (8-BITS_USED)) << (2*BITS_USED);
	shift_left_green[int_temp] = (int_temp >> (8-BITS_USED)) << BITS_USED;
	shift_left_blue[int_temp] =  (int_temp >> (8-BITS_USED));
	
	shift_left[int_temp] =       (int_temp << (8-BITS_USED)) | int_temp;

    }
    
    first_time=0;
}



    /* Merge - takes color table generated by Build_Table and merges */
    /* infrequently used colors in with most popular.                */

static long merge(table,num_colors)
struct table_def *table;
long num_colors;
{
    int best_dist,best_index,dist;
    long unclassed_index;
    int j,max_error,ncolors=table->ncolors;
    register int *ColorMap = table->colormap;
    register struct entry_def *palette=table->palette;

	/* Initialize ColorMap output colors */

    for (j=0;j<ncolors;j++)
	*(ColorMap+ (palette[j].color) ) = j;

	/* do merge */

    max_error = 0;
    for (unclassed_index=ncolors;unclassed_index < num_colors;
	    unclassed_index++) {
	
	/*
	 * XXX for optimal performance, should #define a
	 * common loop for merge_new_index and this routine
	 * to save the stack-frame overhead.
	 */

	best_dist = merge_new_index(table, unclassed_index );
	if (best_dist > max_error) max_error = best_dist;

    }
    return(max_error);
}

/*
 * merge_new_index:
 *  merges a single color into the preset color table
 *  by finding the best matching color and then setting the
 *  new indexes colormap value to the best match.
 */

static long merge_new_index(table, unclassed_index )
struct table_def *table;
long unclassed_index;
{
    int best_index;
    long best_dist,dist;
    int j,ncolors=table->ncolors;
    register int *ColorMap = table->colormap;
    register long *square_off;
    register struct entry_def *palette=table->palette,*pal_ptr;
    register int red_diff,green_diff,blue_diff;
    register unsigned char pix_red,pix_green,pix_blue;

    square_off = &square[256];  /* squares of differences */

    /* do merge */
	
    pix_red = palette[unclassed_index].red;
    pix_green = palette[unclassed_index].green;
    pix_blue = palette[unclassed_index].blue;
    red_diff = pix_red - palette->red;
    green_diff = pix_green - palette->green;
    blue_diff = pix_blue - palette->blue;
    best_dist = *(square_off+red_diff) + *(square_off+green_diff) +
        *(square_off+blue_diff);
    best_index = 0;
    pal_ptr = palette+1;
    for (j=1;j < ncolors;j++) {
        red_diff = pix_red - pal_ptr->red;
        green_diff = pix_green - pal_ptr->green;
        blue_diff = pix_blue - pal_ptr->blue;
        dist = *(square_off+red_diff) + *(square_off+green_diff) +
           *(square_off+blue_diff);
        if (dist < best_dist) {
           best_dist = dist;
           best_index = j;
        }
        pal_ptr++;
    }
    
    *(ColorMap+(palette+unclassed_index)->color) = best_index;
    (palette+best_index)->count += (palette+unclassed_index)->count;
  
    return(best_dist);
}


/*
 * compare:
 *    ordering routine for sorting color entries by 
 * histogram count.
 */

static int compare(entry1,entry2)
struct entry_def *entry1,*entry2;
{
    int return_value;
    return_value = (entry1->count < entry2->count? 1:
	(entry1->count == entry2->count? 0:-1));
    return(return_value);
}

/*
 * ShellSort:
 *  A standard array-sorting algorithm.
 */

static int ShellSort(start, nelem, size, compar)
  char *start;		/* starting addr for sort	*/
  long	nelem;		/* number of elements to sort	*/
  long	size;		/* size of one element in bytes	*/
  int	(*compar)();	/* addr of compare routine	*/
{
  int	i,j,h;
  char  *v;

  v = malloc(size);
  if (v == 0) return 0;

  for (h = 1; h <= nelem; h = 3 * h + 1);
  while (h >= 3)
  {
    h /= 3;
    for (i = h; i < nelem; i++)
    {
      BYTE_COPY(start + size * i, v, size);
      j = i;
      while ((*compar)(start + size * (j - h), v) > 0)
      {
	BYTE_COPY(start + size * (j - h), start + size * j, size);
	j -= h;
	if (j < h) break;
      }
      BYTE_COPY(v, start + size * j, size);
    }
  }
  
  free (v);
  return 1;
}
