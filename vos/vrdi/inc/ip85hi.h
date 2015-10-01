/*
  	DeAnza IP8500 HI-res specific
 */

#ifdef PRIVATE
#undef PRIVATE
#endif

#if VMS_OS
#ifdef 	IP85HI_INITIALIZE
#define 	PRIVATE	globaldef noshare
#else
#define 	PRIVATE	globalref
#endif
#endif

#if UNIX_OS
#define PRIVATE
#endif

#define IP85HI_X_DEV(x)	((x)-1)
#define IP85HI_DEV_X(x)	((x)+1)
#define IP85HI_Y_DEV(y) (N_LINES-(y))
#define IP85HI_DEV_Y(y) (N_LINES-(y))

#define IP85HI_X_VDEV(x)	((x)-1)
#define IP85HI_VDEV_X(x)	((x)+1)
#define IP85HI_Y_VDEV(y) (VIDEO_LINES-(y))
#define IP85HI_VDEV_Y(y) (VIDEO_LINES-(y))

/* DeAnza driver level 1 calls */

/* The Level 1 calls are mostly unusable for hi-res, since they were	*/
/* written for lo-res.  There are many places where they assume you're	*/
/* working with only one memory plane, instead of sets of four as you	*/
/* are in hi-res.  In addition, the VOC and cursor boards are		*/
/* different.  A few of the calls would work, but in order to avoid	*/
/* confusion only level 0 routines are used for hi-res.			*/

