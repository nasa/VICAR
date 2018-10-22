#include "gems.h"

typedef struct {		/* window: a discrete 2-D rectangle */
    int x0, y0;			/* xmin and ymin */
    int x1, y1;			/* xmax and ymax (inclusive) */
} Window;

void concave(int nvert,			/* number of vertices */
	     Point2 *point,		/* vertices of polygon */
	     Window *win,		/* screen clipping window */
	     void (*spanproc)());	/* called for each span of pixels */
