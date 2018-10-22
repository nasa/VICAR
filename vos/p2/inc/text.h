/************************************************************************/
/* Text generating routine.  See text.com for documentation.		*/
/************************************************************************/

#ifndef _ZTEXT_H
#define _ZTEXT_H

#include "xvmaininc.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Constants for the size of the text */

#define ZTEXT_VERTICAL_SIZE	7
#define ZTEXT_HORIZONTAL_SIZE	6

#ifdef _NO_PROTO
void ztext();
#else
void ztext(char *inbuf, int insize, int line, unsigned char *outbuf,
		int size, int dn);
#endif

#ifdef __cplusplus
}
#endif

#endif	/* _AMOEBA_H */

