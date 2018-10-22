#ifndef __QMALLOC_H
#define __QMALLOC_H

#include <stdlib.h>

#define QMALLOC_TRUE  1
#define QMALLOC_FALSE 0

#ifdef __cplusplus
extern "C" {
#endif

void *qmalloc(size_t nelem, size_t elsize, int reset, char *infunc,
	      char *vname);

#ifdef __cplusplus
}
#endif
#endif
