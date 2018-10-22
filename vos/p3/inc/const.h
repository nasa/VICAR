
#ifndef EQUAL
#define EQUAL(x,y) (strcmp(x,y)==0)
#endif
#ifndef EQUALN
#define EQUALN(x,y) (strncmp(x,y,strlen(y))==0)
#endif
#ifndef FAILURE
#define FAILURE 0
#endif
#ifndef FALSE 
#define FALSE 0 
#endif
#ifndef LineLength
#define LineLength 80
#endif
#ifndef LLength
#define LLength 192
#endif
#ifndef MAX
#define MAX(x,y) ((x)<(y) ? (y) : (x))
#endif
#ifndef MIN
#define MIN(x,y) ((x)>(y) ? (y) : (x))
#endif
#ifndef PI
#define PI 3.14159265458979
#endif
#ifndef SUCCESS
#define SUCCESS 1
#endif
#ifndef TRUE
#define TRUE 1
#endif

