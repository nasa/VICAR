/*	IP85LO_Interface - description
 *
 *	Purpose: Provide an interface between the device-independent and the
 *		 DeAnza dependent part of the VRDI.
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *		STATUS = IP85LO_Interface(Unit, function, par1...par10)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
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

/* Macros to arbitrate multiple subprocess access to the device */

globalvalue ATT, DET;

#define ATTACH	{ ip8qw(ATT, &lun); }
#define DETACH  { ip8qw(DET, &lun); }

FUNCTION IP85LO_Interface (Unit, function, par1, par2, par3, par4, par5,
   					  par6, par7, par8, par9, par10)
int *Unit, function;
int par1, par2, par3, par4, par5;
int par6, par7, par8, par9, par10;

{
short lun;
int code;

lun = DEV_UNIT_NO;

switch (function) {
   case INITIALIZE_TABLES:
      code = IP85LO_InitTables(Unit, lun);
      break;

   case OPEN_DEVICE:
      code = IP85LO_OpenDevice(Unit, lun);
      lun = DEV_UNIT_NO;		/* DEV_UNIT_NO is set by OpenDevice */
      DETACH;
      break;

   case CLOSE_DEVICE:
      ATTACH;
      code = IP85LO_CloseDevice(Unit, lun);
      break;

   case CONFIG_DEVICE:
      ATTACH;
      code = IP85LO_ConfigDevice(Unit, lun, par1);
      DETACH;
      break;

   case READ_LINE:
   case WRITE_LINE:
      ATTACH;
      code = IP85LO_Line(Unit, lun, function,
			 par1, par2, par3, par4, par5, par6);
      DETACH;
      break;

   case WRITE_LUT:
      ATTACH;
      code = IP85LO_WriteLut(Unit, lun, par1, par2, par3);
      DETACH;
      break;

   case READ_LUT:
      ATTACH;
      code = IP85LO_ReadLut(Unit, lun, par1, par2, par3);
      DETACH;
      break;

   case READ_PIXEL:
   case WRITE_PIXEL:
      ATTACH;
      code = IP85LO_Pixel(Unit, lun, function, par1, par2, par3, par4, par5);
      DETACH;
      break;

   case AUTO_ON:
   case AUTO_OFF:
      ATTACH;
      code = IP85LO_AutoTrack(Unit, lun, function, par1, par2);
      DETACH;
      break;

   case CONNECT_IMPS_LUTS:
      ATTACH;
      code = IP85LO_Connect(Unit, lun, *(int *)par1, *(int *)par2,
					*(int *)par3, *(int *)par4);
      DETACH;
      break;

   case CONNECT_OVERLAY:	/* overlay is on lut 4 */
      ATTACH;
      code = IP85LO_Connect(Unit, lun, par1, 4, par2, par3);
      DETACH;
      break;

   case CURSOR_ON:
      ATTACH;
      code = IP85LO_CursorOn(Unit, lun, par1, par2, par3);
      DETACH;
      break;

   case CURSOR_OFF:
      ATTACH;
      code = IP85LO_CursorOff(Unit, lun, par1);
      DETACH;
      break;

   case COLOR_CURSOR:
      ATTACH;
      code = IP85LO_CursorColor(Unit, lun, par1, par2, par3, par4);
      DETACH;
      break;

   case GRAPHICS_ON:
   case GRAPHICS_OFF:
      ATTACH;
      code = IP85LO_Graphics(Unit, lun, function);
      DETACH;
      break;

   case READ_OVERLAY_LUT:
      ATTACH;
      code = IP85LO_ReadOverlay(Unit, lun, par1, par2, par3);
      DETACH;
      break;

   case WRITE_OVERLAY_LUT:
      ATTACH;
      code = IP85LO_WriteOverlay(Unit, lun, par1, par2, par3);
      DETACH;
      break;

   case READ_CURSOR:
      ATTACH;
      code = IP85LO_ReadCursor(Unit, lun, par1, par2, par3);
      DETACH;
      break;

   case WRITE_CURSOR:
      ATTACH;
      code = IP85LO_WriteCursor(Unit, lun, par1, par2, par3);
      DETACH;
      break;

   case WRITE_VECTOR:
      ATTACH;
      code = IP85LO_Vector(Unit, lun, par1, par2, par3, par4, par5, par6);
      DETACH;
      break;

   case FILL_AREA:
      ATTACH;
      code = IP85LO_Fill(Unit, lun, par1, par2, par3, par4);
      DETACH;
      break;

   case READ_SWITCH:
      ATTACH;
      code = IP85LO_Switch(Unit, lun, par1, par2, par3);
      DETACH;
      break;

   case READ_2D:
      ATTACH;
      code = IP85LO_Read2d(Unit, lun, par1, par2, par3, par4, par5);
      DETACH;
      break;

   case ZOOM_IMP:
      ATTACH;
      code = IP85LO_ZoomDW(Unit, lun, par1, par2, DW_LEFT(par1), DW_TOP(par1));
      DETACH;
      break;

   case SET_DW:
      ATTACH;
      code = IP85LO_ZoomDW(Unit, lun, par1, ZOOM(par1), par2, par3);
      DETACH;
      break;

   case READ_AREA:
   case WRITE_AREA:
      ATTACH;
      code = IP85LO_Area(Unit, lun, function, par1, par2, par3, par4, par5);
      DETACH;
      break;

   case SET_BATCH_MODE:
      return SUCCESS;

   default:
      code = (DEVICE_CANNOT_DO_IT);
      break;
}
return code;
}
