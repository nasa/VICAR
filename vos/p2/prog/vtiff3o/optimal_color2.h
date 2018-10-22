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

struct table_def;

int table_new( struct table_def **tab, int ncolors );

/**
 ** table_add_colors - adds the specified arrays of RGB
 **  pixels to the palette, for eventual sorting and
 **  optimization.
 **/

void table_add_colors( struct table_def *table,
		       unsigned char *red,unsigned char *green,unsigned char *blue,
		       long npix );

/**
 **   table_free: destruction method for table structure.
 **/

void table_free( struct table_def *table );

/**
 **   table_build: gather color gamut information collected
 **     from the (red,grn,blue) arrays, and set up an
 **     optimal <ncolors>-color mapping for that set of colors.
 **     returns max error distance in colorspace.
 **/

long table_build( struct table_def *table );

/**
 **  table_rgb_to_index
 **   Compress RGB array to indexed value.
 **/


void table_rgb_to_index( struct table_def *table,
			 unsigned char *red,unsigned char*green,unsigned char*blue,
			 long npix,
			 unsigned char *index );

/**
 ** table_extract_lut:
 **  return the lookup table used, in a public format
 **/
  
void table_extract_lut( struct table_def *table,
			unsigned char *red_lut,unsigned char*green_lut,unsigned char*blue_lut );

