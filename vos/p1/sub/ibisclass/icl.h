/*
 *  Common stuff for ICL utilities (private interface)
 */

#ifndef _H_ICL
#define _H_ICL


/*
 *  List of LOOKUP members that we recognize.
 */

#define LOOK_STRETCH "HISTOGRAM"
#define LOOK_PSEUDOCOLOR "PSEUDOCOLOR"

static char *LookMemberList[]={
	LOOK_STRETCH,
	LOOK_PSEUDOCOLOR,
	(char *)0
};
typedef enum {
	LK_STRETCH=1,
	LK_PSEUDOCOLOR
} look_type;


/*
 *  List of STATISTICS members that we recognize.
 */

#define STAT_HISTOGRAM "HISTOGRAM"
#define STAT_CORRELATION "CORRELATION"
#define STAT_COVARIANCE "COVARIANCE"

static char *StatMemberList[]={
	STAT_HISTOGRAM,
	STAT_CORRELATION,
	STAT_COVARIANCE,
	(char *)0
};
typedef enum {
	STAT_HIST=1,
	STAT_CORR,
	STAT_COVAR
} stat_type;

/*
 *  List of POINT members that we recognize.
 */

#define POINT_FRAME "FRAME"
#define POINT_QUALITY "QUALITY"
#define POINT_PIXEL "PIXEL"

static char *PointMemberList[]={
	POINT_FRAME,
	POINT_QUALITY,
	POINT_PIXEL,
	(char *)0
};
typedef enum {
	PT_FRAME=1,
	PT_QUALITY,
	PT_PIXEL
} point_type;


int icl_keymatch(char *keystr, char **keys);

#endif /* ICL */

