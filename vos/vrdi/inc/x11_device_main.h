/* SCCS %W% %G%  %Q% */
/*
  File: x11_device_main.h

  Routines: none

  Purpose: This file contains the main header files, structure descriptions,
           and globally defined values

  Written by: Mark Mann
*/

#include <X11/Xlib.h>
#include <stdio.h>			/* for DPR */
#include <errno.h>

#ifdef PRIVATE
#undef PRIVATE
#endif

#if VMS_OS
#if ALPHA_ARCH
#ifdef X_INITIALIZE
#define PRIVATE
#else
#define PRIVATE extern
#endif
#else /* VAX */
#ifdef X_INITIALIZE
#define PRIVATE globaldef noshare
#else
#define PRIVATE globalref
#endif
#endif
#endif

#if UNIX_OS
#ifdef X_INITIALIZE
#define PRIVATE
#else
#define PRIVATE extern
#endif
#endif

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifndef SUCCESS
#define SUCCESS 1
#endif

#ifndef FAILURE
#define FAILURE 0
#endif

/* define refresh modes */
#define REFRESH_MAIN  0001
#define REFRESH_TB    0002
#define REFRESH_ALL   0004

typedef struct icon_window {
  Window
    main,        /* main icon                         */
    head;        /* icon header                       */
  int
    offset;      /* icon image offset                 */
} ICON_WINDOW;

#define X_LUT_SIZE 		256

typedef struct hist_window {
  Window
    main,			/* main histogram window */
    quit,			/* quit button */
    linear,			/* linear button */
    low_mark,			/* low end linear mark */
    high_mark,			/* hi end linear mark */
    spike_win,			/* spike control window */
    spike_mark;			/* spike level button */
  GC
    hist_gc;			/* GC for histogram window */
  int
    histogram_on,		/* status flag */
    histogram[X_LUT_SIZE],	/* histogram values */
    str_histogram[X_LUT_SIZE],  /* stretched histogram values */
    spike,
    str_spike,
    hist_win_x,
    hist_win_y,
    max_hist,
    max_str_hist;
} HIST_WINDOW;

typedef struct main_window {
  Window
    root,        /* root window                          */
    main,        /* main window                          */
    image,       /* image window                         */
    mag,	 /* magnify/pointer switch               */

    /* control mode buttons */
    lock,	 /* colormap lock/unlock switch          */
    s1,          /* vertical scrollbar                   */
    ve,          /* vertical elevator                    */
    up,          /* up scroll button                     */
    dn,          /* down scroll button                   */
    s2,          /* horizontal scrollbar                 */
    he,          /* horizontal elevator                  */
    lt,          /* left scroll button                   */
    rt,          /* right scroll button                  */
    cursor,      /* cursor mode toggle button            */
    hist_but,	 /* histogram/cmap manip. switch         */
    bit_mode,    /* toggles between 8-bit and 7-bit mode */
                 /* (only used on 8-bit displays)        */

    /* message mode windows */
    right_fill,	 /* gray out right side controls         */
    message;	 /* message window, on bottom            */    
  
  ICON_WINDOW
    icon;        /* icon window -- not a subwindow       */

  HIST_WINDOW
    hist[3];     /* histogram windows -- one per image plane */

  char
    dev[8];      /* device name of window                */

    /* control mode button pixmaps */
  Pixmap
    mag_pix,         /* magnifier button                     */
    lock_pix,	     /* colormap lock/unlock switch          */
    elev_pix,        /* elevators (both)                     */
    up_pix,          /* up scroll button                     */
    dn_pix,          /* down scroll button                   */
    lt_pix,          /* left scroll button                   */
    rt_pix,          /* right scroll button                  */
    cursor_pix,      /* cursor mode toggle button            */
    hist_but_pix,    /* histogram/cmap manip. switch         */
    bit_mode_pix;    /* toggles between 8-bit and 7-bit mode */
                     /* (only used on 8-bit displays)        */
} MAIN_WINDOW ;


typedef struct str_window {
  Window
    main,	 /* main window                       */
    head,	 /* header window                     */
    image_sel,   /* select image window to operate on */
    load,	 /* load colormap                     */
    store,	 /* store colormap                    */
    left,	 /* left mouse button                 */
    middle,	 /* middle mouse button               */
    right,	 /* right mouse button                */
    input,	 /* input histogram window            */
    output;	 /* output histogram window           */
} STR_WINDOW;

typedef struct vrdi_cursor {
  int  on;                      /* is this cursor on ?     */
  int  auto_track;              /* autoracking flag        */
  int  x;			/* cursor origin, horiz    */
  int  y;			/* cursor origin, vert     */
  int  x0,y0;			/* top left coordinate     */
  int  x1,y1;			/* bottom right coordinate */
  int  type;			/* cursor pattern spec.    */
  char pattern[32][32];		/* cursor design           */
} VRDIcursor;

#define N_VCURSORS 2

#define HARD_MODE 0
#define SOFT_MODE 1

#define X_NO_ANGLE		  0
#define X_180			  1
#define X_MINUS90		  2
#define X_90			  3

#define X_CURSOR_REFRESH	 -2
#define X_ALL_IMPS		 -1
#define X_RAMP_LUTS		  0
#define X_USE_SW_LUTS		  1

#define X_FREE_FLOATING		100
#define X_PLANTED		101

#define MIN_X 80		/* minimum total x dimension        */
#define MIN_Y 80		/* minimum total y dimension        */
#define BDR_WW 16		/* border window width              */

#define HIST_WIN_W 300
#define HIST_WIN_H 300

#define ICON_WD 50
#define ICON_HT 50

#define RIGHT_BUTTON 0
#define MIDDLE_BUTTON 1
#define LEFT_BUTTON 2

#define LINEAR_MODE 0
#define RAMP_MODE 1
#define AUTO_MODE 2
#define SCALE_MODE 3

#define NORMAL_SCALE 0
#define LOG_SCALE 1
#define POW_SCALE 2

#define MAGNIFY_DONE 0
#define MAGNIFY_REDO 1

#define ODD_NUM(x)		((x) % 2)
#define POSITIVE_SIGN(x)	((x) & 0x10)

#ifndef MIN
#define MIN(x,y)	((x) < (y) ? (x) : (y))
#endif  /* MIN */
#ifndef MAX
#define MAX(x,y)	((x) > (y) ? (x) : (y))
#endif  /* MAX */

#define X_RED		 1
#define X_GREEN		 2
#define X_BLUE		 3

#define RED_HIST	 0
#define GREEN_HIST	 1
#define BLUE_HIST	 2

#define X_24BIT_MODE	24
#define X_7BIT_MODE	 1
#define X_8BIT_MODE	 2

PRIVATE int   color_order[4];
/******************************************************************************/

/*  Software X Window image plane look-up tables  */
PRIVATE unsigned char *X_LUTS;
#define X_LUT(lut, color) \
	(*(X_LUTS + (((lut)-1)*(X_LUT_SIZE)) + (color)))
#define X_LUTP(lut, color) \
	(X_LUTS + (((lut)-1)*(X_LUT_SIZE)) + (color))

/******************************************************************************/

/*  Software VRDI image plane look-up tables  */
PRIVATE unsigned char *VRDI_LUTS;
#define VRDI_LUT(lut, color) \
	(*(VRDI_LUTS + (((lut)-1)*(X_LUT_SIZE)) + (color)))
#define VRDI_LUTP(lut, color) \
	(VRDI_LUTS + (((lut)-1)*(X_LUT_SIZE)) + (color))

/******************************************************************************/

/*  Software X Window overlay plane look-up tables  */
PRIVATE unsigned char *X_OVLY_LUTS;
#define X_OVLY_LUT(lut, color) \
	(*(X_OVLY_LUTS + (((lut)-1)*(X_LUT_SIZE)) + (color)))
#define X_OVLY_LUTP(lut, color) \
	(X_OVLY_LUTS + (((lut)-1)*(X_LUT_SIZE)) + (color))

/******************************************************************************/

/*  Software VRDI overlay plane look-up tables  */
PRIVATE unsigned char *VRDI_OVLY_LUTS;
#define VRDI_OVLY_LUT(lut, color) \
	(*(VRDI_OVLY_LUTS + (((lut)-1)*(X_LUT_SIZE)) + (color)))
#define VRDI_OVLY_LUTP(lut, color) \
	(VRDI_OVLY_LUTS + (((lut)-1)*(X_LUT_SIZE)) + (color))

/******************************************************************************/

/*  Software VRDI image memory planes  */
PRIVATE unsigned char *VRDI_IMPS;

/*  Macros to access the local image memory planes  */
#define VRDI_IMP(imp, x, y) \
(*(VRDI_IMPS + (((imp)-1)*ZN_SAMPS*ZN_LINES) + (((y)-1)*ZN_SAMPS) + ((x)-1)))

#define VRDI_IMPP(imp, x, y) \
(VRDI_IMPS + (((imp)-1)*ZN_SAMPS*ZN_LINES) + (((y)-1)*ZN_SAMPS) + ((x)-1))

/******************************************************************************/

/*  Image memory planes needed by XCreateImage  */
PRIVATE unsigned char *X_IMPS;

/*  Macros to access the X image memory planes  */
#define X_IMP(imp, x, y) \
(*(X_IMPS + (X_Bytes_Per_Pixel) * (((y)-1)*ZN_SAMPS + ((x)-1)) + color_order[imp]))

#define X_IMPP(imp, x, y) \
(X_IMPS + ((X_Bytes_Per_Pixel) * (((y)-1)*ZN_SAMPS + ((x)-1))))

/******************************************************************************/

/*  Software icon image memory planes  */
PRIVATE unsigned char *ICON_IMAGE;

/*  Macro to access the icon image memory planes  */
#define ICON_IMP(imp, x, y) \
(*(ICON_IMAGE + (X_Bytes_Per_Pixel) * (((y)-1)*ICON_WD + ((x)-1)) + color_order[imp]))

/******************************************************************************/
/*  Since refreshing the screen takes so long, we would like to avoid doing   */
/*  it whenever possible.  Therefore, we keep track of the following informa- */
/*  tion--if these things don't change, we don't need to refresh the screen.  */
/*  We keep private copies of many things that are already available in the   */
/*  VRDI because of race conditions and because sometimes these variables are */
/*  not set until after the X routines are called.                            */
/******************************************************************************/

PRIVATE int which_imp[3];
PRIVATE int imp_zoom[4];
PRIVATE int imp_dw_left[4];
PRIVATE int imp_dw_top[4];
PRIVATE int imp_angle[4];
PRIVATE int imp_written[4];

#define X_WHICH_IMP(lut)	which_imp[(lut)-1]
#define X_ZOOM(imp)		imp_zoom[(imp)-1]
#define X_DW_LEFT(imp)		imp_dw_left[(imp)-1]
#define X_DW_TOP(imp)		imp_dw_top[(imp)-1]
#define X_ANGLE(imp)		imp_angle[(imp)-1]
#define X_WRITTEN(imp)		imp_written[(imp)-1]

/******************************************************************************/
/* The following flags help us to keep track of the various possible con-     */
/* figurations of the X Window system.                                        */
/******************************************************************************/

PRIVATE int   X_Lut_RW;
PRIVATE int   X_Bytes_Per_Line;
PRIVATE int   X_Bytes_Per_Pixel;
PRIVATE int   X_Batch_Mode;
PRIVATE int   X_Cursor_Mode;
PRIVATE int   X_Bit_Mode;
PRIVATE int   X_Max_Height;
PRIVATE int   X_Max_Width;
PRIVATE int   X_Color_Locked;
PRIVATE int   X_Use_System_Cmap;
PRIVATE int   X_Use_System_Overlay_Cmap;
PRIVATE int   X_Display_Orig_X;
PRIVATE int   X_Display_Orig_Y;
PRIVATE int   X_Icon_Image_Exists;
PRIVATE float X_Percent_Scroll_X;
PRIVATE float X_Percent_Scroll_Y;

/******************************************************************************/
/*  The following flags have to be kept in the DCB because they are used in   */
/*  the X device routines as well as the x_* routines.                        */
/******************************************************************************/

#define X_Overlay_On		DCB[unit]->DeviceDependent[0]
#define X_Overlay_Imp		DCB[unit]->DeviceDependent[1]
#define X_Overlay_Size		DCB[unit]->DeviceDependent[2]
#define X_Image_Depth		DCB[unit]->DeviceDependent[3]
