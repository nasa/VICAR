/*	JUP_Interface
 *
 *	Purpose:	VRDI Interface between XD_INTERFACE routine and
 *			display-dependent Jupiter J-Station Display Device
 *			routines
 *
 *	Written by:	Fred Burnette
 *	Date:		October 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = JUP_interface( parameters )
 *
 *	Parameter List:
 *
 *		Unit:		Display device unit number
 *		Function:	Function to be executed on Jupiter
 *		Par1 - Par10:	Integer Parameters used as needed
 *
 *	Possible Error Codes:
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "jupinc.h"

FUNCTION JUP_Interface( Unit, Function, par1, par2, par3, par4, par5,
                                        par6, par7, par8, par9, par10 )
int *Unit, Function;
int par1, par2, par3, par4, par5,
    par6, par7, par8, par9, par10;

{

/* If someone else changed the config, we need to know about it so we can */
/* make the same change ourselves.  j_config() doesn't know when another  */
/* process changes the config.						  */

   if (J12file != NULL &&
		(local_mode[*Unit] != CONFIG_MODE ||
		 local_ovbits[*Unit] != CONFIG_OVBITS))
   {
      j_config(CONFIG_MODE, 1, CONFIG_OVBITS, 0,
		vid_parm(VIDEO_FILE, "hw", JUP_GPUNO, JUP_VIDEO));
      local_mode[*Unit] = CONFIG_MODE;
      local_ovbits[*Unit] = CONFIG_OVBITS;

      if (OVERLAY_AVAILABLE && OVERLAY_ON)
         j_ormask(0x0F);	/* Set the display mask to all on for overlay */
      else {
         j_ormask(0x00);	/* Set the display mask to all off */
         OVERLAY_ON = FALSE;
      }
   }

/* Likewise for whether the cursor is on the image or the graphics plane. */

   if (J12file != NULL && local_cursor[*Unit] != CONFIG_CURSOR)
   {
      j_cursor_sel(CONFIG_CURSOR);
      local_cursor[*Unit] = CONFIG_CURSOR;
   }

   switch (Function)
   {
      case INITIALIZE_TABLES:
      {
         return JUP_InitTables( Unit );
      }

      case OPEN_DEVICE:
      {
         return JUP_OpenDevice( Unit );
      }

      case CLOSE_DEVICE:
      {
         return JUP_CloseDevice( Unit );
      }

      case CONFIG_DEVICE:
      {
         return JUP_ConfigDevice( Unit, par1 );
      }

      case READ_LUT:
      case WRITE_LUT:
      {
         return JUP_Lut( Unit , Function, par1, par2, par3 );
      }

      case AUTO_OFF:
      case AUTO_ON:
      {
         return JUP_AutoTrack( Unit, Function, par1, par2 );
      }

      case CURSOR_OFF:
      case CURSOR_ON:
      {
         return JUP_Cursor( Unit, Function, par1, par2, par3 );
      }

      case COLOR_CURSOR:
      {
         return JUP_CursorColor(Unit, par1, par2, par3, par4);
      }

      case DRAW_CIRCLE:
      {
         return JUP_Circle( Unit, par1, par2, par3,
                                  par4, par5, par6 );
      }

      case FILL_AREA:
      {
         return JUP_Fill( Unit, par1, par2, par3, par4 );
      }

      case READ_CURSOR:
      {
         return JUP_ReadCursor( Unit, par1, par2, par3 );
      }

      case WRITE_CURSOR:
      {
         return JUP_WriteCursor( Unit, par1, par2, par3 );
      }

      case READ_OVERLAY_LUT:
      case WRITE_OVERLAY_LUT:
      {
         return JUP_Overlay( Unit, Function, par1, par2, par3 );
      }

      case GRAPHICS_ON:
      case GRAPHICS_OFF:
      {
         return JUP_Graphics( Unit, Function );
      }

      case READ_SWITCH:
      {
         return JUP_Switch( Unit, par1, par2, par3 );
      }

      case WRITE_VECTOR:
      {
         return JUP_Vector( Unit, par1, par2, par3,
                                  par4, par5, par6 );
      }

      case ZOOM_IMP:
      {
         return JUP_ZoomDW( Unit, Function, par1, par2,
                            DW_LEFT(par1), DW_TOP(par1) );
      }

      case SET_DW:
      {
         return JUP_ZoomDW( Unit, Function, par1, ZOOM(par1), par2, par3 );
      }

      /* All three write commands use the same routine, with the parameters: */
      /* (imp, size, top, left, bottom, right, buffer, mask)		     */

      case WRITE_AREA:
      {			/* (imp, size, aw, buffer, mask) */
         return JUP_Write( Unit, par1, par2,
			((int *)par3)[TOP], ((int *)par3)[LEFT],
			((int *)par3)[BOTTOM], ((int *)par3)[RIGHT],
			par4, par5 );
      }

      case WRITE_LINE:
      {			/* (imp, left, top, size, buffer, mask) */
         return JUP_Write( Unit, par1, par4,
			par3, par2, par3, par2+par4-1,
			par5, par6 );
      }

      case WRITE_PIXEL:
      {			/* (imp, x, y, value, mask) */
         return JUP_Write( Unit, par1, 1,
			par3, par2, par3, par2,
			par4, par5 );
      }

      /* All three read commands use the same routine, with the parameters: */
      /* (imp, size, top, left, bottom, right, buffer)			    */

      case READ_AREA:
      {			/* (imp, size, aw, buffer) */
         return JUP_Read( Unit, par1, par2,
			((int *)par3)[TOP], ((int *)par3)[LEFT],
			((int *)par3)[BOTTOM], ((int *)par3)[RIGHT], par4 );
      }

      case READ_LINE:
      {			/* (imp, left, top, size, buffer) */
         return JUP_Read( Unit, par1, par4,
			par3, par2, par3, par2+par4-1, par5 );
      }

      case READ_PIXEL:
      {			/* (imp, x, y, value) */
         return JUP_Read( Unit, par1, 1,
			par3, par2, par3, par2, par4 );
      }

      case SET_BATCH_MODE:
      {
         return SUCCESS;
      }

      default:
      {
         return (DEVICE_CANNOT_DO_IT);
      }
   }
}
