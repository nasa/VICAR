/* X windows Interface

   Written by: Mark Mann

   Calling Sequence:

       STATUS = x_interface(Unit,Function,parameters)
*/

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdalloc.h"
#include "xdfuncs.h"
#include "xderrors.h"
#include "x11_device_main.h"

#ifdef DEBUG
#define DPR(A)  {printf A; fflush(stdout);}
#else
#define DPR(A)
#endif

X_Interface(Unit, Function,
	          par1, par2, par3, par4, par5,
	          par6, par7, par8, par9, par10 )
     int *Unit, Function;
     int *par1, *par2, *par3, *par4, *par5,
	 *par6, *par7, *par8, *par9, *par10;
{
  int status;
  int aw[4];

  switch ( Function )
    {
    /*
     * MUST have these routines
     */
    case INITIALIZE_TABLES:
      DPR(("x_interface: INITIALIZE_TABLES\n"));
      status = x_inittables( Unit );
      return(status);
      break;
    case OPEN_DEVICE:
      DPR(("x_interface: OPEN_DEVICE\n"));
      return( x_opendevice( Unit ) );
      break;
    case READ_LINE:
      DPR(("x_interface: OPEN_DEVICE\n"));
      /*
       * par1 : imp       integer
       * par2 : xpos      integer
       * par3 : ypos      integer
       * par4 : length    integer
       * par5 : array     char[]
       */
      return( x_read_line( Unit, (int)par1, (int)par2, (int)par3, (int)par4,
			(char *)par5) );
      break;
    case READ_LUT:
      /*
       * par1 : lut       integer
       * par2 : section   integer 
       * par3 : array     integer[]
       */
      return( x_read_lut( Unit, (int)par1, (int)par2, (int *)par3) );
      break;
    case READ_PIXEL:
      /*
       * par1 : imp     integer
       * par2 : xpos    integer
       * par3 : ypos    integer
       * par4 : value  *char
       */
      return( x_read_pixel( Unit, (int)par1, (int)par2, (int)par3,
					(char *)par4) );
      break;
    case WRITE_LINE:
      DPR(("x_interface: WRITE_LINE\n"));
      /*
       * par1 : imp	             integer
       * par2 : left boundary        integer
       * par3 : y_pos	             integer
       * par4 : length	             integer
       * par5 : buffer	             unsigned char[]
       * par6 : mask                 unsigned char
       */
      return( x_writeline( Unit, (int)par1, (int)par2, (int)par3, (int)par4,
			(unsigned char *)par5, (unsigned char)(int)par6 ) );
      break;
    case WRITE_LUT:
      DPR(("x_interface: WRITE_LUT\n"));
      /*
       * par1 : lut       integer
       * par2 : section   integer
       * par3 : array     integer[]
       */
      return( x_write_lut( Unit, (int)par1, (int)par2, (int *)par3) );
      break;
    case WRITE_PIXEL:
      DPR(("x_interface: WRITE_PIXEL\n"));
      /*
       * par1 : imp    integer
       * par2 : xpos   integer
       * par3 : ypos   integer
       * par4 : value  *char
       * par5 : mask   unsigned char
       */
      return( x_write_pixel( Unit, (int)par1, (int)par2, (int)par3,
				(char *)par4, (unsigned char)(int)par5 ) );
      break;
    case CLOSE_DEVICE:
      DPR(("x_interface: CLOSE_DEVICE\n"));
      return( x_closedevice( Unit ) );
      break;

    /*
     * SHOULD have these routines
     */

    case AUTO_OFF:
    case AUTO_ON:
      /*
       * par1 : device    integer
       * par2 : cursor    integer
       */
      return( x_autotrack( Unit, Function, (int)par1, (int)par2 ) );
      break;
    case CURSOR_OFF:
      DPR(("x_interface: CURSOR_OFF\n"));
      /*
       * par1: cursor #  integer
       */
      return( x_cursor_off( Unit, (int)par1 ) );
      break;
    case CURSOR_ON:
      DPR(("x_interface: CURSOR_ON\n"));
      /*
       * par1: cursor #     integer
       * par2: cursor type  integer
       */
      return( x_cursor_on( Unit, (int)par1, (int)par2 ) );
      break;
    case FILL_AREA:
      DPR(("x_interface: FILL_AREA\n"));
      /*
       * par1 : imp		integer
       * par2 : value		unsigned char
       * par3 : mask		unsigned char
       * par4 : access window	integer[4]
       */
      return ( x_fill_area( Unit, (int)par1, (unsigned char)(int)par2,
			(unsigned char)(int)par3, (int *)par4 ) );
      break;
    case GRAPHICS_OFF:
      DPR(("x_interface: GRAPHICS_OFF\n"));
      return( x_graphics_off( Unit ) );
      break;
    case GRAPHICS_ON:
      DPR(("x_interface: GRAPHICS_ON\n"));
      return( x_graphics_on( Unit ) );
      break;
    case READ_AREA:
      DPR(("x_interface: READ_AREA\n"));
      /*
       * par1 : imp     integer
       * par2 : size    integer
       * par3 : aw      integer[4]
       * par4 : array   char[]
       *
       */
      return( x_read_area( Unit, (int)par1, (int)par2, (int *)par3,
						(char *)par4) );
      break;
    case READ_CURSOR:
/*
      DPR(("x_interface: READ_CURSOR\n"));
*/
      /*
       * par1: cursor #   integer
       * par2: x pos     *integer
       * par3: y pos     *integer 
       */
      return( x_read_cursor( Unit, (int)par1, (int *)par2, (int *)par3 ) );
      break;
    case READ_SWITCH:
/*
      DPR(("x_interface: READ_SWITCH\n"));
*/
      /*
       * par1 : device    integer
       * par2 : switch    integer
       * par3 : value    *integer
       */
      return( x_read_switch( Unit, (int)par1, (int)par2, (int *)par3) );
      break;
    case READ_2D:
     DPR(("x_interfaceINT: READ_2D\n"));
      /*
       * par1 : device	 integer
       * par2 : x coodinate   *float
       * par3 : y coordinate  *float
       * par4 : proximity     *integer
       * par5 : pen	   *integer
       *
       */
      return( x_read_2d( Unit, (int)par1, (float *)par2, (float *)par3,
				(int *)par4, (int *)par5 ) );
      break;
    case WRITE_AREA:
      DPR(("x_interface: WRITE_AREA\n"));
      /*
       * par1 : imp		integer
       * par2 : size		integer
       * par3 : access window	integer[4]
       * par4 : array		unsigned char[]
       * par5 : mask		unsigned char
       */
      return( x_write_area( Unit, (int)par1, (int)par2, (int *)par3,
			(unsigned char *)par4, (unsigned char)(int)par5 ) );
      break;
    case WRITE_CURSOR:
/*
      DPR(("x_interface: WRITE_CURSOR\n"));
*/
      /*
       * par1: cursor # int
       * par2: x pos    int 
       * par3: y pos    int
       */
      return( x_write_cursor( Unit, (int)par1, (int)par2, (int)par3 ) );
      break;
    case WRITE_OVERLAY_LUT:
      DPR(("x_interfaceINT: WRITE OVERLAY LUT\n"));
      /*
       * par1: red lut  int[]
       * par2: grn lut  int[]
       * par3: blu lut  int[]
       */
      return(x_write_overlay_lut( Unit, (int *)par1, (int *)par2, (int *)par3));
      break;      
    case READ_OVERLAY_LUT:
      DPR(("x_interfaceINT: READ OVERLAY LUT\n"));
      /*
       * par1: red lut  int[] returned
       * par2: grn lut  int[] returned
       * par3: blu lut  int[] returned
       */
      return( x_read_overlay_lut( Unit, (int *)par1, (int *)par2, (int *)par3));
      break;      
    case WRITE_VECTOR:
      DPR(("x_interfaceINT: WRITE_VECTOR\n"));
      /*
       * par1 : imp        integer
       * par2 : npts       integer
       * par3 : x coords   integer[npts]
       * par4 : y coords   integer[npts]
       * par5 : value      unsigned char
       * par6 : mask       unsigned char
       */
      return( x_write_vector( Unit, (int)par1, (int)par2, (int *)par3,
	   (int *)par4, (unsigned char)(int)par5, (unsigned char)(int)par6 ) );
      break;
    case DRAW_CIRCLE:
       DPR(("x_interface: DRAW_CIRCLE\n"));
       /*
        * par1 : imp		integer
        * par2 : xcenter	integer
        * par3 : ycenter	integer
        * par4 : radius		integer
        * par5 : value		unsigned char
        * par6 : mask		unsigned char
        * par7 : area[4]	integer
        */
       return (x_draw_circle(Unit, (int)par1, (int)par2, (int)par3, (int)par4,
		(unsigned char)(int)par5, (unsigned char)(int)par6, (int)par7));
       break;
    case ZOOM_IMP:
      DPR(("x_interface: ZOOM_IMP\n"));
      /*
       * par1: imp   integer
       * par2: zoom  integer
       */
      return( x_zoom(Unit,(int)par1,(int)par2));
      break;
    case SET_DW:
      /*
       * par1: imp   integer
       * par2: x     integer
       * par3: y     integer
       */
      DPR(("x_interface: SET_DW\n"));
      return (x_set_dw(Unit,(int)par1,(int)par2,(int)par3));
      break;
    case COLLECT_HISTOGRAM:
      DPR(("x_interface: COLLECT HIST\n"));
      /*
       * par1 : imp       integer
       * par2 : mask imp  integer
       * par3 : histogram integer[]
       * par4 : aw(imp)   integer[]
       * par5 : aw(mask)  integer[]
       */
      return( x_collect_histogram( Unit, (int)par1, (int)par2, (int *)par3,
			(int *)par4, (int *)par5 ) );
      break;
    case CONFIG_DEVICE:
      DPR(("x_interface: CONFIG DEVICE\n"));
      return (SUCCESS);
      break;
    case CONNECT_IMPS_LUTS:
      DPR(("x_interface: CONNECT_IMPS_LUTS\n"));
      /*
       * par1 : imp      integer
       * par2 : lut     *integer
       * par3 : section *integer
       * par4 : bypass  *integer
       */
      return( x_connect_lut( Unit, (int *)par1, (int *)par2) );
      break;
    case CONNECT_OVERLAY:
      DPR(("x_interface: CONNECT OVERLAY\n"));
      /*
       * par1 : imp      integer
       * par2 : section
       */
	return( x_connect_glut( Unit, (int)par1) );
      break;
    case OPEN_CLOSE_WIN:
      /*
       * par1 : open/close flag
       */
      return( x_open_close_win( Unit, (int)par1 ) );
      break;
    case MOVE_WIN:
      /*
       * par1 : new x location
       * par2 : new y location
       */
      return( x_move_win( Unit, (int)par1, (int)par2 ) );
      break;
    case RESIZE_WIN:
      /*
       * par1 : new width
       * par2 : new height
       */
      return( x_resize_win( Unit, (int)par1, (int)par2 ) );
      break;
    case SET_BATCH_MODE:
       /*
        * par 1 : mode		integer
        */
      return( x_set_batch_mode( Unit, (int)par1 ) );
    default:
      {
	return ( DEVICE_CANNOT_DO_IT );
      }
    }
}
      
