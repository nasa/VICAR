/*	RAMTEK_Interface - description
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = RAMTEK_Interface( parameters )
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

#include "rtekinc.h"

FUNCTION RAMTEK_Interface( Unit, Function, par1, par2, par3, par4, par5,
   					 par6, par7, par8, par9, par10 )
int	*Unit, Function;
int	par1, par2, par3, par4, par5,
	par6, par7, par8, par9, par10;

{

  RM_Channel_No = DEV_UNIT_NO;  

  switch (Function) {
      case INITIALIZE_TABLES: {
	 return RAMTEK_InitTables( Unit );
	 }

      case OPEN_DEVICE: {
	 return RAMTEK_OpenDevice( Unit );
	 }

      case CLOSE_DEVICE: {
	 return RAMTEK_CloseDevice( Unit );
	 }

      case CONFIG_DEVICE: {
	 return RAMTEK_ConfigDevice( Unit, par1 );
	 }

      case READ_LINE:
      case WRITE_LINE: {
	 return RAMTEK_Line( Unit, Function, par1, par2, 
			par3, par4, par5, par6 );
	 }

      case READ_LUT:
      case WRITE_LUT: {
	 return RAMTEK_Lut( Unit, Function, par1, par2, par3 );
	 }

      case READ_PIXEL:
      case WRITE_PIXEL: {
	 return RAMTEK_Pixel( Unit, Function, par1, par2, par3, par4, par5 );
	 }

      case CONNECT_IMPS_LUTS: {
         return RAMTEK_Connect( Unit, par1, par2, par3 );
         }

      case WRITE_VECTOR: {
	 return RAMTEK_Vector( Unit, par1, par2, par3, par4, par5, par6 );
	 }

      case WRITE_TEXT: {
	 return RAMTEK_Text( Unit, par1, par2, par3, par4, par5, par6 );
	 }

      case FILL_AREA: {
	 return RAMTEK_Fill( Unit, par1, par2, par3, par4 );
	 }

      case ZOOM_IMP: {
	 return RAMTEK_Zoom_IMP( Unit, par1, par2, par3 );
	 }

      case SET_DW: {
	 return RAMTEK_SetWindow( Unit, par1, par2, par3 );
	 }

      case DRAW_CIRCLE: {
	 return RAMTEK_Circle( Unit, par1, par2, par3, par4, par5, par6 );
	 }

      case READ_AREA:
      case WRITE_AREA: {
	 return RAMTEK_Area( Unit, Function, par1, par2, par3, par4, par5 );
	 }

      case CURSOR_OFF:
      case CURSOR_ON: {
	 return RAMTEK_Cursor( Unit, Function, par1, par2, par3 );
	 }

      case READ_CURSOR:
      case WRITE_CURSOR: {
	 return RAMTEK_CurPosition( Unit, Function, par1, par2, par3 );
	 }

      case AUTO_ON:
      case AUTO_OFF: {
         return RAMTEK_AutoTrack( Unit, Function, par1, par2 );
         }

      case AFG_ON:
      case AFG_OFF: {
	 return RAMTEK_AFG( Unit, Function );
	 }

      case AFG_TEXT: {
	 return RAMTEK_AFGText( Unit, Function, par1, par2, par3, par4, par5, par6 );
	 }

      case AFG_CLEAR: {
	 return RAMTEK_AFGClear( Unit, Function, par1, par2, par3 );
	 }

      case SET_BATCH_MODE: {
         return SUCCESS;
         }

      default: {
	 return (DEVICE_CANNOT_DO_IT);
	 }
  }
}
