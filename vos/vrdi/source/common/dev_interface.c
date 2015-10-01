/*	XD_Interface - provides the calling interface between the device
 *			independent and device dependent layers.
 *
 *	Purpose:
 *
 *	Written by:  R. A. Mortensen
 *	Date:	Sep 11, 1986
 *
 *	Calling Sequence:
 *
 *		XD_Device_Interface( Unit, Function, par1, par2, ... par10 )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *
 */

#include "xvmaininc.h"
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdsupport.h"
#include "xdfuncs.h"


int XD_Device_Interface( Unit, Function, 
		par1, par2, par3, par4, par5,
		par6, par7, par8, par9, par10 )
int	*Unit, Function;
int	*par1, *par2, *par3, *par4, *par5,
	*par6, *par7, *par8, *par9, *par10;

{
   switch (DEV_TYPE) {

      case DEANZA_IP85LO:
      case DEANZA_IP85LX:
      case DEANZA_IP9000:
	 return ( IP85LO_Interface( Unit, Function,
			  par1, par2, par3, par4, par5,
			  par6, par7, par8, par9, par10 ) );

      case DEANZA_IP85HI:
	 return ( IP85HI_Interface( Unit, Function,
			  par1, par2, par3, par4, par5,
			  par6, par7, par8, par9, par10 ) );

      case RAMTEK_9460:
	 return ( RAMTEK_Interface( Unit, Function, 
			  par1, par2, par3, par4, par5,
			  par6, par7, par8, par9, par10 ) );

      case IIS_IVAS_IV:
	 return ( IVAS_Interface( Unit, Function, 
			par1, par2, par3, par4, par5,
			par6, par7, par8, par9, par10 ) );

      case ADAGE_3000:
	 return ( ADAGE_Interface( Unit, Function, 
			par1, par2, par3, par4, par5,
			par6, par7, par8, par9, par10 ) );

      case JUP_JSTATION:
	 return ( JUP_Interface( Unit, Function,
			par1, par2, par3, par4, par5,
			par6, par7, par8, par9, par10 ) );

      case TEK_4237:
      case TEK_3D_LEFT:
      case TEK_3D_RIGHT:
         return ( TEK_Interface( Unit, Function,
			par1, par2, par3, par4, par5,
			par6, par7, par8, par9, par10 ) );

      case DUMMY:
         return ( Dummy_Interface( Unit, Function,
			par1, par2, par3, par4, par5,
			par6, par7, par8, par9, par10 ) );

      case X_WINDOW:
         return ( X_Interface( Unit, Function,
			par1, par2, par3, par4, par5,
			par6, par7, par8, par9, par10 ) );

      default:
	 return INVALID_DEVICE_TYPE;

   }
}

