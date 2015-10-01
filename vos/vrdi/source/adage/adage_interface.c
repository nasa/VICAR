/*	ADAGE_Interface - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = ADAGE_Interface( parameters )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *
 *	Possible Error Codes:
 *
 */
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"


FUNCTION ADAGE_Interface( Unit, Function, par1, par2, par3, par4, par5,
   					 par6, par7, par8, par9, par10 )
int	*Unit, Function;
int	par1, par2, par3, par4, par5,
	par6, par7, par8, par9, par10;

   {
   switch (Function) {
      case INITIALIZE_TABLES: {
	 return ADAGE_InitTables( Unit );
	 }

      case OPEN_DEVICE: {
	 return ADAGE_OpenDevice( Unit );
	 }

      case CONFIG_DEVICE: {
	 return ADAGE_Configure( Unit, par1 );
	 }

      case READ_LINE:
      case WRITE_LINE: {
	 return ADAGE_Line( Unit, Function, par1, par2, 
			par3, par4, par5, par6 );
	 }

      case READ_LUT:
      case WRITE_LUT: {
	 return ADAGE_Lut( Unit, Function, par1, par2, par3 );
	 }

      case READ_PIXEL:
      case WRITE_PIXEL: {
	 return ADAGE_Pixel( Unit, Function, par1, par2, par3, par4, par5 );
	 }

      case AUTO_ON:
      case AUTO_OFF: {
         return ADAGE_AutoTrack( Unit, Function, par1, par2 );
         }

      case CONNECT_IMPS_LUTS: {
         return ADAGE_Connect( Unit, *(int *)par1 );
         }

      case CONNECT_OVERLAY: {
         return ADAGE_Connect( Unit, par1 );
         }

      case CURSOR_ON:
      case CURSOR_OFF: {
         return ADAGE_Cursor( Unit, Function, par1, par2, par3 );
         }

      case GRAPHICS_ON:
      case GRAPHICS_OFF: {
	 return ADAGE_Graphics( Unit, Function );
	 }

      case READ_OVERLAY_LUT:
      case WRITE_OVERLAY_LUT: {
	 return ADAGE_Overlay( Unit, Function, par1, par2, par3 );
	 }

      case READ_CURSOR:
      case WRITE_CURSOR: {
	 return ADAGE_CurPosition( Unit, Function, par1, par2, par3 );
	 }

/*      case WRITE_VECTOR: {
	 return ADAGE_Vector( Unit, par1, par2, par3, par4, par5, par6 );
	 }

      case FILL_AREA: {
	 return ADAGE_Fill( Unit, par1, par2, par3, par4 );
	 }
*/
      case READ_SWITCH: {
	 return ADAGE_Switch( Unit, par1, par2, par3 );
	 }

      case READ_2D: {
	 return ADAGE_Read2d( Unit, par1, par2, par3, par4, par5 );
	 }

      case ZOOM_IMP: {
	 return ADAGE_Zoom( Unit, par1, par2, par3 );
	 }

      case SET_DW: {
	 return ADAGE_SetWindow( Unit, par1, par2, par3 );
	 }

      case SET_BATCH_MODE: {
         return SUCCESS;
         }

      default: {
	 return (DEVICE_CANNOT_DO_IT);
	 }
      }
   }
