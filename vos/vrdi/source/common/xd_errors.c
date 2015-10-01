/*	XD_ERRORS - error handling routines
 *
 *	Purpose:
 *
 *	Written by:
 *	Date:
 *
 *	Calling Sequence:
 *
 *		STATUS = xd_error_handler( Unit, Code )
 *
 *	Parameter List:
 *
 *		Unit:	Display device unit number
 *		Code:	Status code
 *
 *	Possible Error Codes:
 *
 */
#include "xvmaininc.h"
#include "xdexterns.h"
#include "xderrors.h"
#include "xdroutines.h"
#include "xdfuncs.h"

void xd_error_handler( Unit, code )
int	*Unit, code;

{
  if (code == SUCCESS) return;		/* Check for SUCCESS */

  /* Turn batch mode off to make sure that the screen is completely updated */
  if (xd_current_call != DBATCH)
    batch_off(Unit, FALSE);

  if (IS_XD_CODE(code) && IS_KNOWN_CODE(code)) {

    if ( (IS_WARNING(code)   && SYS_MSG(xd_warn_action)   ) ||
	   (IS_ERROR(code)   && SYS_MSG(xd_error_action)  ) ||
	   (IS_FATAL(code)   && SYS_MSG(xd_fatal_action)  ) ) {

      xd_sys_msg( Unit, code );
    }

    if ( (IS_WARNING(code)   && ABORT_FLAG(xd_warn_action)   ) ||
	   (IS_ERROR(code)   && ABORT_FLAG(xd_error_action)  ) ||
	   (IS_FATAL(code)   && ABORT_FLAG(xd_fatal_action)  ) ) {

      exit(-1);
    }
  }
  else {	/* If not XD code or unknown XD code, it's fatal by default */
    if (SYS_MSG(xd_fatal_action))
      xd_sys_msg( Unit, code );
    if (ABORT_FLAG(xd_fatal_action))
      exit(-1);
  }
}

xd_sys_msg( Unit, incode )
int	*Unit, incode;

{
  int code;
  static char	msg[132];
   
  code = incode;
  if (code == SUCCESS) return;			/*  Just in case  */

  strcpy( msg, "Exception in " );
  strcat( msg, xd_routines[xd_current_call] );
  if (*Unit != -1) {
    strncat( msg, " processing device:  ", 132-strlen(msg) );
    strncat( msg, DIB[*Unit]->DeviceName, 132-strlen(msg) ); 
/** strncat( msg, DEV_NAME, 132-strlen(msg) ); **/
  }
  msg[131] = 0;					/*  Just in case  */

  zvmessage( msg, "VRDI-GENERR" );
  zdesignal ( code );
}
