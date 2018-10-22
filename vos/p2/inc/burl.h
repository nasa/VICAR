#ifndef __BURL_H
#define __BURL_H

#include <math.h>

#ifndef MAX
#define MAX(A, B) ((A) > (B) ? (A) : (B))
#endif

#ifndef MIN
#define MIN(A, B) ((A) < (B) ? (A) : (B))
#endif

#ifndef ABS
#define ABS(A)    ((A) > 0.0 ? (A) : (-(A)))
#endif

#ifndef ROUND
#define ROUND(A)  (floor((A) + 0.5))
#endif

#ifndef CLIP
#define CLIP(V, A, B) ( MIN((MAX((V),(A))), (B)) )
#endif

#ifndef MAXABS
#define MAXABS(A, B) (MAX(ABS(A),ABS(B)))
#endif

#ifndef SIGNUM
#define SIGNUM(A) (((A) == 0) ? 0 : (((A) > 0) ? 1 : -1))
#endif

#ifndef TWOPI
#define TWOPI (((double) 2.0) * M_PI)
#endif

#ifndef DEG2RAD
#define  DEG2RAD ((M_PI)/((double) 180.0))
#endif

#ifndef ARCSEC2RAD
#define  ARCSEC2RAD ((M_PI)/((double) 180.0 * 3600.0))
#endif

#ifndef RAD2DEG
#define  RAD2DEG   (((double) 180.0)/(M_PI))
#endif

#ifndef D_ZERO
#define D_ZERO ((double) 0.0)
#endif

#ifndef D_ONE
#define D_ONE ((double) 1.0)
#endif

#ifndef D_TWO
#define D_TWO ((double) 2.0)
#endif

#ifndef D_THREE
#define D_THREE ((double) 3.0)
#endif

#ifndef D_FOUR
#define D_FOUR ((double) 4.0)
#endif

#ifndef D_TEN
#define D_TEN ((double) 10.0)
#endif

#ifndef ERR
#define ERR -1
#endif

#ifndef OK
#define OK 0
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef TRUE
#define TRUE 1
#endif

#ifndef MILLION
#define MILLION 1000000
#endif

#ifndef BILLION
#define BILLION 1000000000
#endif

#ifndef MAXSTRING
#define MAXSTRING 4096
#endif

#ifndef MAXLINE
#define MAXLINE 4096
#endif

#endif
