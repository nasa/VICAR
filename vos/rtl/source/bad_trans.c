#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "rtlintproto.h"


/* Handle an invalid translation by returning a status code	*/

int v2_bad_trans(void *UNUSED(from), void *UNUSED(to) , int UNUSED(len), 
		 struct trans *UNUSED(trans))
{
   return INVALID_FORMAT_TRANSLATION;
}
