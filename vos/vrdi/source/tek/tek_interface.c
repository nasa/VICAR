/*	TEK_Interface - description
 *
 *	Purpose: Provide an interface between the device-independent and the
 *		 Tektronix 4237 dependent part of the VRDI.
 *
 *	Written by: Paul Bartholomew
 *	Date:	    September 8, 1989
 *
 *	Calling Sequence:
 *		STATUS = TEK2D_Interface(Unit, function, par1...par10)
 *
 *	Parameter List:
 *		Unit:	  Display device unit number
 *		function: Function code
 *		par1...par10: Parameters for the various functions
 *
 *	Possible Error Codes:
 *		DEVICE_CANNOT_DO_IT: Function not implemented yet or not
 *			available in the hardware
 *		Any return code from one of the functions is passed on
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdsupport.h"
#include "tek.h"

FUNCTION TEK_Interface (Unit, function, par1, par2, par3, par4, par5,
   					  par6, par7, par8, par9, par10)
int *Unit, function;
int par1, par2, par3, par4, par5;
int par6, par7, par8, par9, par10;
{
   int code;

   switch (function) {
      case INITIALIZE_TABLES:
         code = TEK_InitTables(Unit);
         break;

      case OPEN_DEVICE:
         code = TEK_OpenDevice(Unit);
         break;

      case CLOSE_DEVICE:
         code = TEK_CloseDevice(Unit);
         break;

      case CONFIG_DEVICE:
         code = TEK_ConfigDevice(Unit, par1);
         break;

      case READ_LINE:
      case WRITE_LINE:
         code = TEK_Line(Unit, function, par1, par2, par3, par4, par5, par6);
         break;

      case READ_LUT:
      case WRITE_LUT:
         code = TEK_Lut(Unit, function, par1, par2, par3);
         break;

      case READ_PIXEL:
      case WRITE_PIXEL:
         code = TEK_Pixel(Unit, function, par1, par2, par3, par4, par5);
         break;

      case AUTO_ON:
      case AUTO_OFF:
         code = TEK_AutoTrack(Unit, function, par1, par2);
         break;

      case CONNECT_IMPS_LUTS:
         code = TEK_Connect(Unit, *(int *)par1, *(int *)par2, *(int *)par3,
                            *(int *)par4);
         break;

      case CURSOR_ON:
         code = TEK_Cursor(Unit, function, par1, par2, par3, CURSOR_RED(par1),
                           CURSOR_GREEN(par1), CURSOR_BLUE(par1));
         break;

      case CURSOR_OFF:
         code = TEK_Cursor(Unit, function, par1, CURSOR_FORM(par1),
                           CURSOR_BLINK(par1), CURSOR_RED(par1),
                           CURSOR_GREEN(par1), CURSOR_BLUE(par1));
         break;

      case COLOR_CURSOR:
         code = TEK_Cursor(Unit, function, par1, CURSOR_FORM(par1),
                           CURSOR_BLINK(par1), par2, par3, par4);
         break;

      case GRAPHICS_ON:
      case GRAPHICS_OFF:
         code = TEK_Graphics(Unit, function);
         break;

      case READ_OVERLAY_LUT:
         code = TEK_Overlay(Unit, function, par1, par2, par3);
         break;

      case WRITE_OVERLAY_LUT:
         code = TEK_Overlay(Unit, function, par1, par2, par3);
         break;

      case READ_CURSOR:
         code = TEK_ReadCursor(Unit, par1, par2, par3);
         break;

      case WRITE_CURSOR:
         code = TEK_WriteCursor(Unit, par1, par2, par3);
         break;

      case WRITE_VECTOR:
         code = TEK_Vector(Unit, par1, par2, par3, par4, par5, par6);
         break;

      case DRAW_CIRCLE:
         code = TEK_Circle(Unit, par1, par2, par3, par4, par5, par6, par7);
         break;

      case FILL_AREA:
         code = TEK_Fill(Unit, par1, par2, par3, par4);
         break;

      case READ_SWITCH:
         code = DEVICE_CANNOT_DO_IT;
         break;

      case READ_2D:
         code = DEVICE_CANNOT_DO_IT;
         break;

      case ZOOM_IMP:
         code = TEK_Zoom(Unit, par1, par2);
         break;

      case ROTATE_IMP:
         code = TEK_Rotate(Unit, par1, par2, par3);
         break;

      case SET_DW:
         code = TEK_SetDW(Unit, par1, par2, par3);
         break;

      case READ_AREA:
      case WRITE_AREA:
         code = TEK_Area(Unit, function, par1, par2, par3, par4, par5,
                         TEK_WRITE_AREA);
         break;

      case SET_BATCH_MODE:
         return SUCCESS;

      default:
         code = DEVICE_CANNOT_DO_IT;
         break;
   }

   return(code);
}
