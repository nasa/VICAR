/* This same code is used in both the VRDI and in the USEDISP/FREEDISP	*/
/* commands in TAE.  The code must be repeated since TAE cannot link to	*/
/* the VRDI, and USEDISP/FREEDISP must be TAE intrinsics under VMS so	*/
/* the device will be allocated in the parent process.			*/

#ifndef TAE
#include "xvmaininc.h"
#endif

#include "xdintproto.h"

#ifndef TAE
#ifndef VRDI
#define VRDI            /* default */
#endif
#endif

/*
 *
 *	XDFUNCS.H - XDLIB Device independent function codes
 *
 */

/*
 *	The following functions MUST be supported by ALL devices.
 */

#ifdef VRDI

#define	INITIALIZE_TABLES	1
#define	OPEN_DEVICE		3
#define	READ_LINE		4
#define	READ_LUT		5
#define	READ_PIXEL		6
#define	WRITE_LINE		7
#define	WRITE_LUT		8
#define	WRITE_PIXEL		9
#define	CLOSE_DEVICE		10

#endif /* VRDI */

#define ALLOCATE_DEVICE		11
#define GET_DEVICE		12
#define FREE_DEVICE		13

/*
 *	The following functions SHOULD be supported by MOST devices.
 */

#ifdef VRDI

#define	AUTO_OFF		100
#define	AUTO_ON			101
#define	CURSOR_OFF		102
#define	CURSOR_ON		103
#define	DRAW_CIRCLE		104
#define	FILL_AREA		105
#define	GRAPHICS_OFF		106
#define	GRAPHICS_ON		107
#define	READ_AREA		108
#define	READ_CURSOR		109
#define	READ_OVERLAY_LUT	110
#define	READ_SWITCH		111
#define	READ_1D			112
#define	READ_2D			113
#define	READ_3D			114
#define	WRITE_AREA		115
#define	WRITE_CURSOR		116
#define	WRITE_OVERLAY_LUT	117
#define	WRITE_VECTOR		118
#define	ZOOM_IMP		119
#define GET_NAMED_DEVICE	120
#define GET_DEVICES_ALLOC	121
#define RESIZE_CURSOR		122
#define COLOR_CURSOR		123

#endif /* VRDI */

#define ALLOC_SHMEM		122
#define REMOVE_SHMEM		123
#define ATTACH_SHMEM		124
#define DETACH_SHMEM		125

/*
 *	The following functions will be supported by only a FEW devices
 */

#ifdef VRDI

#define	AFG_CLEAR		1000
#define	AFG_OFF			1001
#define	AFG_ON			1002
#define	AFG_TEXT		1003
#define	ARITHMETIC_OPERATION	1004
#define	COLLECT_HISTOGRAM	1005
#define	CONFIG_DEVICE		1006
#define	CONNECT_IMPS_LUTS	1007
#define	CONNECT_OVERLAY		1008
#define	COPY_IMP		1009
#define	FILL_REGION		1010
#define	LOGICAL_OPERATION	1011
#define	ROTATE_IMP		1012
#define	SET_DW			1013
#define	SHIFT_IMP		1014
#define WRITE_TEXT		1015
#define OPEN_CLOSE_WIN		1016
#define MOVE_WIN		1017
#define RESIZE_WIN		1018
#define SET_BATCH_MODE		1019

#endif /* VRDI */
