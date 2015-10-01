/*	Dummy_Interface - description
 *
 *	Purpose:  Provide an interface between the dummy device and the VRDI
 *
 *	Written by: Paul Bartholomew
 *	Date:	    May 30, 1990
 *
 *	Calling Sequence:
 *		STATUS = Dummy_Interface(Unit, function, par1...par10)
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

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

FUNCTION Dummy_Interface (Unit, function, par1, par2, par3, par4, par5,
   					  par6, par7, par8, par9, par10)
int *Unit, function;
int *par1, *par2, *par3, *par4, *par5;
int *par6, *par7, *par8, *par9, *par10;
{
   int code;

   switch (function) {
      case INITIALIZE_TABLES:
         code = Dummy_InitTables(Unit);
         break;

      case OPEN_DEVICE:
         code = Dummy_OpenDevice(Unit);
         break;

      case CLOSE_DEVICE:
         code = Dummy_CloseDevice(Unit);
         break;

      case CONFIG_DEVICE:
         code = Dummy_ConfigDevice(Unit, par1);
         break;

      case READ_LINE:
      case WRITE_LINE:
         code = Dummy_Line(Unit, function, par1, par2, par3, par4, par5, par6);
         break;

      case READ_LUT:
      case WRITE_LUT:
         code = Dummy_Lut(Unit, function, par1, par2, par3);
         break;

      case READ_PIXEL:
      case WRITE_PIXEL:
         code = Dummy_Pixel(Unit, function, par1, par2, par3, par4, par5);
         break;

      case AUTO_ON:
      case AUTO_OFF:
         code = SUCCESS;
         break;

      case CONNECT_IMPS_LUTS:
         code = SUCCESS;
         break;

      case CURSOR_ON:
      case CURSOR_OFF:
         code = SUCCESS;
         break;

      case COLOR_CURSOR:
         code = SUCCESS;
         break;

      case GRAPHICS_ON:
      case GRAPHICS_OFF:
         code = SUCCESS;
         break;

      case READ_OVERLAY_LUT:
      case WRITE_OVERLAY_LUT:
         code = Dummy_Overlay(Unit, function, par1, par2, par3);
         break;

      case READ_CURSOR:
         code = Dummy_ReadCursor(Unit, par1, par2, par3);
         break;

      case WRITE_CURSOR:
         code = Dummy_WriteCursor(Unit, par1, par2, par3);
         break;

      case WRITE_VECTOR:
         code = Dummy_Vector(Unit, par1, par2, par3, par4, par5, par6);
         break;

      case DRAW_CIRCLE:
         code = Dummy_Circle(Unit, par1, par2, par3, par4, par5, par6, par7);
         break;

      case FILL_AREA:
         code = Dummy_Fill(Unit, par1, par2, par3, par4);
         break;

      case READ_SWITCH:
         code = Dummy_Switch(Unit, par1, par2, par3);
         break;

      case READ_2D:
         code = Dummy_Read2d(Unit, par1, par2, par3, par4, par5);
         break;

      case ZOOM_IMP:
         code = SUCCESS;
         break;

      case ROTATE_IMP:
         code = Dummy_Rotate(Unit, par1, par2, par3);
         break;

      case SET_DW:
         code = SUCCESS;
         break;

      case READ_AREA:
      case WRITE_AREA:
         code = Dummy_Area(Unit, function, par1, par2, par3, par4, par5);
         break;

      case SET_BATCH_MODE:
         return SUCCESS;

      default:
         code = SUCCESS;
         break;
   }

   return(code);
}
