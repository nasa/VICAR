/*	IVAS_Interface - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = IVAS_Interface( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *
 */

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

FUNCTION IVAS_Interface( Unit, Function, par1, par2, par3, par4, par5,
   					 par6, par7, par8, par9, par10 )
int	*Unit, Function;
int	par1, par2, par3, par4, par5,
	par6, par7, par8, par9, par10;
   {
   switch (Function) {
      case INITIALIZE_TABLES: {
	 return IVAS_InitTables( Unit );
	 }

      case OPEN_DEVICE: {
	 return IVAS_OpenDevice( Unit );
	 }

      case CLOSE_DEVICE: {
	 return IVAS_CloseDevice( Unit );
	 }

      case CONFIG_DEVICE: {
	 return IVAS_ConfigDevice( Unit, par1 );
	 }

      case READ_LINE:
      case WRITE_LINE: {
	 return IVAS_Line( Unit, Function, par1, par2, 
			par3, par4, par5, par6 );
	 }

      case READ_LUT:
      case WRITE_LUT: {
	 return IVAS_Lut( Unit, Function, par1, par2, par3 );
	 }

      case READ_PIXEL:
      case WRITE_PIXEL: {
	 return IVAS_Pixel( Unit, Function, par1, par2, par3, par4, par5 );
	 }

      case AUTO_ON:
      case AUTO_OFF: {
         return IVAS_AutoTrack( Unit, Function, par1, par2 );
         }

      case CONNECT_IMPS_LUTS: {
         return IVAS_Connect( Unit );
         }

      case COLOR_CURSOR: {
         return IVAS_Cursor( Unit, Function, par1, CURSOR_FORM(par1),
                             CURSOR_BLINK(par1), CURSOR_XSIZE(par1),
                             CURSOR_YSIZE(par1), par2, par3, par4 );
         }

      case RESIZE_CURSOR: {
         return IVAS_Cursor( Unit, Function, par1, CURSOR_FORM(par1),
                             CURSOR_BLINK(par1), par2, par3,
                             CURSOR_RED(par1), CURSOR_GREEN(par1),
                             CURSOR_BLUE(par1) );
         }

      case CURSOR_ON: {
         return IVAS_Cursor( Unit, Function, par1, par2, par3,
			     CURSOR_XSIZE(par1), CURSOR_YSIZE(par1),
                             CURSOR_RED(par1), CURSOR_GREEN(par1),
                             CURSOR_BLUE(par1) );
         }
      case CURSOR_OFF: {
         return IVAS_Cursor( Unit, Function, par1, CURSOR_FORM(par1), 
                             CURSOR_BLINK(par1), CURSOR_XSIZE(par1),
                             CURSOR_YSIZE(par1), CURSOR_RED(par1),
                             CURSOR_GREEN(par1), CURSOR_BLUE(par1) );
         }

      case GRAPHICS_ON:
      case GRAPHICS_OFF: {
	 return IVAS_Graphics( Unit, Function );
	 }

      case CONNECT_OVERLAY: {
         return IVAS_GConnect( Unit, par1, par2, par3 );
         }

      case READ_OVERLAY_LUT:
      case WRITE_OVERLAY_LUT: {
	 return IVAS_Overlay( Unit, Function, par1, par2, par3 );
	 }

      case READ_CURSOR:
      case WRITE_CURSOR: {
	 return IVAS_CurPosition( Unit, Function, par1, par2, par3 );
	 }

      case WRITE_VECTOR: {
	 return IVAS_Vector( Unit, par1, par2, par3, par4, par5, par6 );
	 }

      case FILL_AREA: {
	 return IVAS_Fill( Unit, par1, par2, par3, par4 );
	 }

      case READ_SWITCH: {
	 return IVAS_Switch( Unit, par1, par2, par3 );
	 }

      case READ_2D: {
	 return IVAS_Read_2d( Unit, par1, par2, par3, par4, par5 );
	 }

      case ZOOM_IMP: {
	 return IVAS_Zoom_IMP( Unit, par1, par2, par3 );
	 }

      case SET_DW: {
	 return IVAS_Scroll_IMP( Unit, par1, par2, par3 );
	 }

      case READ_AREA:
      case WRITE_AREA: {
	 return IVAS_Area( Unit, Function, par1, par2, par3, par4, par5 );
	 }

      case SET_BATCH_MODE: {
         return SUCCESS;
         }

      default: {
	 return (DEVICE_CANNOT_DO_IT);
	 }
      }
   }
