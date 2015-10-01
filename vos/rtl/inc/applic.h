#ifndef APPLIC_H
#define APPLIC_H

/* Defines:                                                           */

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif
#define STRING_LENGTH 132
#ifndef SUCCESS
#define SUCCESS 1
#endif
#define FAILURE 0
#ifndef NULL
#ifdef __cplusplus
#define NULL 0
#else
#define NULL (void*) 0
#endif
#endif
#define YES 1
#define NO 0
#ifndef MAX
#define MAX(x,y) (((x)<(y)) ? (y) : (x))
#endif
#ifndef MIN
#define MIN(x,y) (((x)>(y)) ? (y) : (x))
#endif
#define EQUAL(x,y) (strcmp(x,y)==0)
#define ODD(y) (y & 0x00000001)
#define V2_ACTIVE 1
#define V2_INACTIVE 0
/* #define LARGEST_FLOAT 1.0e+38		not needed? */
/* #define LARGEST_INTEGER 2147483647		not needed? */

/* Type definitions:                                                  */

typedef char BYTE;
typedef int (*ROUTINE)(void);
typedef int FLAG;

#endif /* APPLIC_H */
