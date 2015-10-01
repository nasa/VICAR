/* SCCS %W% %G%  %Q% */
/******************************************************************************/
/*                                                                            */
/*  File: x11_device.c                                                        */
/*                                                                            */
/*  Routines:                                                                 */
/*                                                                            */
/*	main(argc,argv)                                                       */
/*	int argc;                                                             */
/*	char *argv[];                                                         */
/*                                                                            */
/*	Cursor CreateCursor(dpy, win, bits, mask_bits, width, height, x, y)   */
/*		Display       *dpy;                                           */
/*		Window         win;                                           */
/*		unsigned char *bits, *mask_bits;                              */
/*		int            width, height, x, y;                           */
/*                                                                            */
/*  Written by:			Mark Mann                                     */
/*  Date:			Sep 28 1988                                   */
/*                                                                            */
/*  Cognizant Programmer:	Paul Bartholomew                              */
/*  Revision History:                                                         */
/*    Date    FR #   Description                                              */
/*  --------  -----  -------------------------------------------------------  */
/*  08-01-92   ---   PDB-Complete rewrite.                                    */
/*                                                                            */
/******************************************************************************/

#include "xvmaininc.h"

#include <stdio.h>
#include <errno.h>

#if VMS_OS
#include <file.h>
#define caddr_t caddr_t2	/* avoid multiple def between socket.h and Xt */
#include <socket.h>
#undef caddr_t
#include "sock_emulate.h"
#else
#include <fcntl.h>
#include <unistd.h>
#include <sys/socket.h>
#endif

#if !LOCKF_AVAIL_OS
#include <sys/file.h>
#endif

#define XD_INITIALIZE
#define X_INITIALIZE
#include "xdexterns.h"
#include "xderrors.h"
#include "xdsupport.h"		/* for XD_Read_DIBs */
#include "xdroutines.h"
#include "x11_device_main.h"
#include "x11_bitmaps.h"
#if VMS_OS && ALPHA_ARCH
#undef MIN              /* redefined by Intrinsic.h */
#undef MAX
#endif
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#if VMS_OS
#include "xdalloc.h"		/* for shmem_vms */
#endif

/* #define DEBUG */
#ifdef DEBUG
#define DPR(A)	{printf A; fflush(stdout);}
#else
#define DPR(A)
#endif

/* global variables */
int unit;
int window_fd;				/* main window socket */
XtAppContext app_context;
XImage *x_image_str, *icon_ximage;	/* Image structure of main&icon images*/
Pixmap image_pm;			/* Image pixmap... sent to X server */
XColor default_colors[X_LUT_SIZE];
XColor fg_rgb;

VRDIcursor vcursor[N_VCURSORS];		/* Virtual cursors */

Cursor					/* X window cursors */
     main_cursor,
     close_cursor,
     jump_cursor,
     ud_cursor,
     ub_cursor,
     db_cursor,
     lr_cursor,
     lb_cursor,
     rb_cursor,
     up_cursor,
     left_cursor,
     v1_cursor,
     v2_cursor,
     v3_cursor,
     v4_cursor,
     v5_cursor,
     v6_cursor,
     watch_cursor,
     empty_cursor;

XFontStruct
     *font_struct,			/* Main window font */
     *icon_font_struct;			/* Icon window font */
int
     fontx, fonty,			/* Main font offsets */
     iconfontx, iconfonty;		/* Icon font offsets */

unsigned long Plane_Mask[8], Base_Pixel, Overlay_Pixels[16];
int default_depth;
Visual *default_visual;
XVisualInfo vip;			/* Visual info pointers */
unsigned long fg, bg;			/* Foreground and Background */
Colormap colormap, default_cmap;	/* Local colormap */
Atom wm_proto, wm_del_win, wm_cmap;
Window parent_window;			/* Parent window's id */
XWindowAttributes parent_attributes;	/* Parent window's attributes */

char MESSAGE[80];			/* Default message */
int MESSAGE_LEN = 0;			/* Default message length in chars */

Display *dpy;
MAIN_WINDOW win;

GC
     message_gc,			/* Message window Graphics Context */
     pm_gc,				/* Pixmap GC...immediate to image ops */
     im_gc,				/* Image GC */
     icon_gc,				/* Icon GC */
     button_gc;				/* Button GC (for redrawing buttons) */

Pixmap					/* Pixmaps needed externally */
     HardPixmap,			/* Hardware lookup table used */
     SoftPixmap,			/* Software LUT used */
     SevenBitPixmap,
     EightBitPixmap,
     GrayPixmap;

unsigned long MAIN_MASK =		/* Main windows event mask */
     StructureNotifyMask |
     ButtonPressMask |
     ButtonReleaseMask | 
     EnterWindowMask;

unsigned long ICON_EVENT_MASK =		/* Icon window event mask */
     ExposureMask |
     StructureNotifyMask |
     SubstructureNotifyMask;

unsigned long NORM_EVENT_MASK =		/* Normal event mask */
     ExposureMask |
     ColormapChangeMask |
     LeaveWindowMask |
     ButtonPressMask |
     KeyPressMask |
     KeyReleaseMask;

/* local to main() */

#define DEFAULT_BORDER_WIDTH 1

/*
  Main program

    argv[1]	device name
    argv[2]	image plane size y
    argv[3]	image plane size x
    argv[4]     Calling window ID
    argv[5]     VRDI display unit number
*/


main(argc,argv)
int argc;
char *argv[];
{
   int i, imp, status;
     
   /* LOCK FILE */
   int lock_fd;				/* Lock file descriptor */
   char lock_file[100];			/* Lock file name */
   int lock_stat;			/* Lock file status from lockf() */
   char *getenv();

   /* SOCKET */
   char socket_name[80];		/* name of socket file */
   struct sockaddr mstr_header;		/* info holder for sockets */

   /* X11 */
   int depth;
   int class;

   /* window parameters */
   Window win_array[2];
   int border_width = DEFAULT_BORDER_WIDTH;	/* border width */

   XSetWindowAttributes attributes;	/* Window attributes */
   XWindowAttributes win_info;		/* main window info */
   XWMHints wm_hints;			/* Window Manager hints */
   XSizeHints size_hints;		/* WM size hints */
   XGCValues xgcvalues;			/* GC values */
   int screen;				/* Default screen */
   int nvi;				/* Number of VisualInfos returned */

   unsigned long int valuemask;		/* Generic mask */
   int x_loc, y_loc, width, height;	/* Window location & size info */
   int
	icon_x_loc, icon_y_loc,		/* Icon location & size info */
	icon_width, icon_height;
   XIconSize *icon_sizes;
   int num_sizes;
   unsigned int size;

   char
	dev_name[80],			/* Device name */
	prog_name[80];			/* Program name */

   Cursor CreateCursor();		/* Local create cursor routine */
   Window CreateWindow();		/* Local create window routine */

   int nlines, nsamps;			/* temporary for cmd-line arguments */
   char *ptr;				/* for setting up shared memory */

   /* Get program options */     
   strcpy(prog_name,((argc < 1) ? "" : argv[0]));
   strcpy(dev_name,((argc < 2) ? "Dev" : argv[1]));
   nlines = (argc < 3) ? 512 : atoi(argv[2]);
   nsamps = (argc < 4) ? 512 : atoi(argv[3]);
   parent_window = (Window) ((argc < 5) ? 0 : atoi(argv[4]));
   parent_window = 0;   /* !!!! */
   unit = (argc < 6) ? 0 : atoi(argv[5]);
   DPR(("x11_device: args='%s %s %s %s %s'\n", argv[1],argv[2],argv[3],argv[4],argv[5]));

   /* Set up the Xt stuff */
   XtToolkitInitialize();
   app_context = XtCreateApplicationContext();
   dpy = XtOpenDisplay(app_context, NULL, dev_name, "XDisplay", NULL, 0,
#if XtSpecificationRelease > 4
        &argc, argv);
#else
        (Cardinal *) &argc, argv);
#endif

   if (dpy == NULL) {
      printf("Could not open display '%s'\n", XDisplayName(NULL));
      exit(-1);
   }
   DPR(("x11_device: X display opened\n"));

   /* Set the root and default screen */
   screen = DefaultScreen(dpy);
   win.root = RootWindow(dpy, screen);
   default_depth = DefaultDepth(dpy, screen);
   default_cmap = DefaultColormap(dpy, screen);
   default_visual = DefaultVisual(dpy, screen);

   DPR(("x11_device: default depth=%d, visual=%d, screen=%d\n",
        default_depth, default_visual->class, screen));

   /* Initialize the DIB table */

   XD_Read_DIBs();

   /* Attach to the shared memory area so we can access the DCB */

   /* This size calculation is also used in XDDALLOCATE and twice */
   /* in XDDOPEN and again below when setting up the pointers.    */
   /* If you change the size calc, don't forget to change all	  */
   /* five places.						  */

   size = sizeof(struct DCB_STRUCTURE) +		  /* DCB */
             4 * sizeof(int) * DIB[unit]->nImps +         /* access window */
             2 * sizeof(int) * DIB[unit]->nImps +         /* display window */
             1 * sizeof(int) * DIB[unit]->nImps +         /* zoom factor */
             1 * sizeof(int) * DIB[unit]->nLuts +         /* imp_to_lut */
             1 * sizeof(int) * DIB[unit]->nLuts +         /* lut section */
             2 * sizeof(int) * DIB[unit]->nCursors +      /* cursor position */
             1 * sizeof(int) * DIB[unit]->nCursors +      /* cursor form */
             1 * sizeof(int) * DIB[unit]->nCursors +      /* cursor blink */
             2 * sizeof(int) * DIB[unit]->nCursors +      /* cursor size */
             3 * sizeof(int) * DIB[unit]->nCursors +      /* cursor color */
             1 * sizeof(int) * DIB[unit]->nCursors +      /* cursor active */
             1 * sizeof(int) * DIB[unit]->nLuts +         /* lut_bypass */
             1 * sizeof(int) *(DIB[unit]->nLuts + 1) +    /* lut_flag */
             1 * sizeof(int) *(DIB[unit]->nImps + 1);     /* image_plane_flag */

   DPR(("x11_device: call ATTACH_SHMEM\n"));
   status = Attach_Shmem(unit, size, &DCB[unit], SHMEM_DCB);
   DPR(("x11_device: called ATTACH_SHMEM %d\n",status));

   if (status != SUCCESS && status != SHMEM_ALREADY_ACTIVE)
      error(dpy, &win, "Cannot attach to shared memory.");

   /* Now set up the pointers for all the DCB-related arrays.	*/
   /* These arrays are all one right after the other in the	*/
   /* shared memory, so add the length of the previous one	*/
   /* to its address to get the address of the new one.		*/

   ptr = (char *)DCB[unit] + sizeof(struct DCB_STRUCTURE);

   access_window[unit] = (int (*)[]) ptr;
   ptr += 4 * sizeof(int) * DIB[unit]->nImps;

   display_window[unit] = (int (*)[]) ptr;
   ptr += 2 * sizeof(int) * DIB[unit]->nImps;

   zoom_factor[unit] = (int *) ptr;
   ptr += 1 * sizeof(int) * DIB[unit]->nImps;

   imp_to_lut[unit] = (int *) ptr;
   ptr += 1 * sizeof(int) * DIB[unit]->nLuts;

   lut_section[unit] = (int *) ptr;
   ptr += 1 * sizeof(int) * DIB[unit]->nLuts;

   cursor_position[unit] = (int (*)[]) ptr;
   ptr += 2 * sizeof(int) * DIB[unit]->nCursors;

   cursor_form[unit] = (int *) ptr;
   ptr += 1 * sizeof(int) * DIB[unit]->nCursors;

   cursor_blink[unit] = (int *) ptr;
   ptr += 1 * sizeof(int) * DIB[unit]->nCursors;

   cursor_size[unit] = (int (*)[]) ptr;
   ptr += 2 * sizeof(int) * DIB[unit]->nCursors;

   cursor_color[unit] = (int (*)[]) ptr;
   ptr += 3 * sizeof(int) * DIB[unit]->nCursors;

   cursor_active[unit] = (int *) ptr;
   ptr += 1 * sizeof(int) * DIB[unit]->nCursors;

   lut_bypass[unit] = (int *) ptr;
   ptr += 1 * sizeof(int) * DIB[unit]->nLuts;

   lut_flag[unit] = (int *) ptr;
   ptr += 1 * sizeof(int) * (DIB[unit]->nLuts + 1);

   image_plane_flag[unit] = (int *) ptr;

#if UNIX_OS			/* File lock only needed under Unix */
   /* Set up the file lock */
   if (getenv("VRDITMP") == NULL)
      sprintf(lock_file, "%s%s.%d", X_SCRATCH_DIR, dev_name, getuid());
   else
      sprintf(lock_file, "%s%s.%d", getenv("VRDITMP"), dev_name, getuid());
   unlink(lock_file);
   lock_fd = open(lock_file, O_CREAT | O_WRONLY, 0777);
   if (lock_fd == -1)
      error(dpy, &win, "Cannot create lock file");
   errno = 0;
#if LOCKF_AVAIL_OS
   lock_stat = lockf(lock_fd, F_LOCK, 0);
#else
   lock_stat = flock(lock_fd, LOCK_EX | LOCK_NB);
#endif
   if (lock_stat == -1)
      error(dpy, &win, "Error locking device lock file");
   DPR(("x11_device: lock file created\n"));
#endif	/* UNIX_OS */

   /* Copy name of device into window struct */
   strcpy(win.dev, dev_name);

   /* Set up the socket communications */
#if VMS_OS		/* VMS names are unique per process tree anyway */
   sprintf(socket_name,"%s%s", X_SOCKET_HDR, dev_name);
#else
   sprintf(socket_name,"%s%s%04.4x", X_SOCKET_HDR, dev_name, getuid());
#endif
   window_fd = socket_open(10, socket_name, &mstr_header, 0);
   if (window_fd == -1)
      error(dpy, &win, "Communication channel connect error");
   DPR(("x11_device: socket set up\n"));

   /* Initialize global variables */
   X_Batch_Mode = FALSE;
   X_Cursor_Mode = X_FREE_FLOATING;
   X_Color_Locked = TRUE;
   X_Use_System_Cmap = FALSE;
   X_Use_System_Overlay_Cmap = FALSE;
   X_Display_Orig_X = 0;
   X_Display_Orig_Y = 0;
   X_Icon_Image_Exists = FALSE;
   X_Percent_Scroll_X = 0.0;
   X_Percent_Scroll_Y = 0.0;
   X_Overlay_On = FALSE;

   /* Create geometry for the user window */
   x_loc = 630;
   y_loc = 20;
   width = 512 + BDR_WW;
   height = 512 + BDR_WW;

#ifdef DEBUG
   XSynchronize(dpy, 1);	/*!!!! DEBUG ONLY !!!!*/
#endif

   /* Get the size of the root window */
   status = XGetWindowAttributes(dpy, win.root, &win_info);
   if (status) {
      DPR(("x11_device: root window size x=%d, y=%d, border_width=%d, depth=%d\n",
           win_info.width, win_info.height, win_info.border_width, win_info.depth));
      X_Max_Height = win_info.height;
      X_Max_Width = win_info.width;
   }
   else {
      DPR(("x11_device: unable to get root window attributes\n"));
   }

   /* Get the visual information best suited to the device */
   status = get_visual_model(dpy, screen, &depth, &class, &vip);
   if (status != SUCCESS)
      error(dpy, &win, "Unable to match XVisualInfo structure.\n");

   DPR(("x11_device: class=%d, depth=%d, ID=%x, map_entries=%d\n",
        class, depth, vip.visual->visualid, vip.visual->map_entries));
   DPR(("x11_device: red mask=%d, green mask=%d, blue mask=%d\n",
        vip.red_mask, vip.green_mask, vip.blue_mask));

   X_Image_Depth = depth;
   switch (X_Image_Depth) {
      case 24:
         ZN_IMPS = 4;
         X_Bytes_Per_Pixel = 4;
         X_Bit_Mode = X_24BIT_MODE;
         X_Overlay_Size = 256;
         X_Overlay_Imp = 4;
         ZOUTPUT_MODE = FULL_COLOR;
         break;
      case 8:
         ZN_IMPS = 2;
         X_Bytes_Per_Pixel = 1;
         X_Bit_Mode = X_7BIT_MODE;
         X_Overlay_Size = 16;
         X_Overlay_Imp = 2;
         for (i = 0; i < X_LUT_SIZE; i++)
            default_colors[i].pixel = i;
         XQueryColors(dpy, default_cmap, default_colors, X_LUT_SIZE);
         ZOUTPUT_MODE = BLACK_AND_WHITE;
         break;
      default:
         error(dpy, &win, "Invalid image depth.  Only 24-bit and 8-bit are supported.");
         break;
   }

   if (class == DirectColor || class == PseudoColor || class == GrayScale) {
      X_Lut_RW = TRUE;
      colormap = XCreateColormap(dpy, win.root, vip.visual, AllocAll);
   }
   else {
      X_Lut_RW = FALSE;
      colormap = XCreateColormap(dpy, win.root, vip.visual, AllocNone);
   }

   if (colormap == 0)
      error(dpy, &win, "Bad get on colormap\n");
   DPR(("x11_device: created colormap, id=%d\n", colormap));

   ZN_LINES = nlines;
   ZN_SAMPS = nsamps;
   ZVIDEO_LINES = nlines;
   ZVIDEO_SAMPLES = nsamps;
   ZN_LUTS = DIB[unit]->nLuts;

   /* Allocate memory for the image planes and look-up tables. */
   DPR(("x11_device: ZN_IMPS=%d, ZN_LINES=%d, ZN_SAMPS=%d, ZN_LUTS=%d, X_LUT_SIZE=%d\n",
        ZN_IMPS, ZN_LINES, ZN_SAMPS, ZN_LUTS, X_LUT_SIZE));

   size = X_Bytes_Per_Pixel * ZN_LINES * ZN_SAMPS;	/* X image structure */
   DPR(("x11_device: allocating %d bytes for the X image structure\n", size));
   X_IMPS = (unsigned char *) malloc(size);
   if (X_IMPS == NULL)
      error(dpy, &win, "Unable to allocate X image plane memory.");
   memset(X_IMPS, (int) 0, size);

   size = ZN_IMPS * ZN_LINES * ZN_SAMPS;		/* VRDI image planes */
   DPR(("x11_device: allocating %d bytes for the VRDI image planes\n", size));
   VRDI_IMPS = (unsigned char *) malloc(size);
   if (VRDI_IMPS == NULL)
      error(dpy, &win, "Unable to allocate VRDI image plane memory.");
   memset(VRDI_IMPS, (int) 0, size);

   size = ZN_LUTS * X_LUT_SIZE;				/* X LUTs */
   DPR(("x11_device: allocating %d bytes for the LUTs\n", size));
   X_LUTS = (unsigned char *) malloc(size);
   if (X_LUTS == NULL)
      error(dpy, &win, "Unable to allocate X LUT memory.");
   memset(X_LUTS, (int) 0, size);

   VRDI_LUTS = (unsigned char *) malloc(size);		/* VRDI LUTs */
   if (VRDI_LUTS == NULL)
      error(dpy, &win, "Unable to allocate VRDI LUT memory.");
   memset(VRDI_LUTS, (int) 0, size);

   X_OVLY_LUTS = (unsigned char *) malloc(size);	/* X overlay LUTs */
   if (X_OVLY_LUTS == NULL)
      error(dpy, &win, "Unable to allocate X overlay LUT memory.");
   memset(X_OVLY_LUTS, (int) 0, size);

   VRDI_OVLY_LUTS = (unsigned char *) malloc(size);	/* VRDI overlay LUTs */
   if (VRDI_OVLY_LUTS == NULL)
      error(dpy, &win, "Unable to allocate VRDI overlay LUT memory");
   memset(VRDI_OVLY_LUTS, (int) 0, size);

   /* Initialize global shared memory values. */
   for (imp = 1; imp <= ZN_IMPS; imp++) {
      ZAW_LEFT(imp) = 1;
      ZAW_TOP(imp) = 1;
      ZAW_RIGHT(imp) = ZN_SAMPS;
      ZAW_BOTTOM(imp) = ZN_LINES;
      X_ZOOM(imp) = 1;
      X_DW_LEFT(imp) = 1;
      X_DW_TOP(imp) = 1;
      X_ANGLE(imp) = X_NO_ANGLE;
      X_WRITTEN(imp) = FALSE;
   }

   /* open the font X11.4 style */
   font_struct = XLoadQueryFont(dpy, "*-courier-medium-r-normal--*-90-*");
   if (font_struct == NULL) {
      /* attempt to open font X11.2 style */
      font_struct = XLoadQueryFont(dpy, "9x15");
      if (font_struct == NULL)
         error(dpy, &win, "Cannot open main font");
   }
   DPR(("x11_device: fontid=%d, ascent=%d\n", (int) font_struct->fid,
        font_struct->ascent));
   icon_font_struct = XLoadQueryFont(dpy, "-*-courier-medium-r-*-*-11-*-*-*-*-*-*-*");
   if (icon_font_struct == NULL) {
      icon_font_struct = XLoadQueryFont(dpy, "-Adobe-Courier-Medium-R-Normal--10-100-75-75-M-60-ISO8859-1");
      if (icon_font_struct == NULL) {
         icon_font_struct = XLoadQueryFont(dpy, "9x15");
         if (icon_font_struct == NULL)
            error(dpy, &win, "Cannot open icon font");
      }
   }

   fonty = (BDR_WW - font_struct->ascent)/2 + font_struct->ascent;
   iconfonty  = (BDR_WW-icon_font_struct->ascent)/2 + icon_font_struct->ascent;

   if (X_Image_Depth == 8) {
      status = XAllocColorCells(dpy, default_cmap, True, Plane_Mask, 7,
           &Base_Pixel, 1);
      if (status) {
         X_Use_System_Cmap = TRUE;
         DPR(("x11_device: allocated colors successfully, Base_Pixel=%d\n",
              Base_Pixel));
         for (i = 0; i < 7; i++) {
            DPR(("x11_device: Plane_Mask[%d]=%d\n", i, Plane_Mask[i]));
         }
         status = XAllocColorCells(dpy, default_cmap, False, NULL, 0,
              Overlay_Pixels, X_Overlay_Size);
         if (status) {
            X_Use_System_Overlay_Cmap = TRUE;
            DPR(("x11_device: allocated overlay colors successfully\n"));
            for (i = 0; i < X_Overlay_Size; i++) {
               DPR(("x11_device: pixel[%d] = %d\n", i, Overlay_Pixels[i]));
            }
         }
      }
   }

   init_luts(dpy);
   DPR(("x11_device: allocating various window colors\n"));
   if (X_Image_Depth == 8) {
      fg = (1 << X_Image_Depth) - 1;
      bg = 0;
      for (i = 0; i < X_LUT_SIZE; i++) {
         if (default_colors[i].red == 0 && default_colors[i].green == 0 &&
              default_colors[i].blue == 0) {
            bg = i;
            break;
         }
      }
   }
   else {
      fg = WhitePixel(dpy, screen);
      bg = BlackPixel(dpy, screen);
   }
   fg_rgb.pixel = (1 << X_Image_Depth) - 1;
   XQueryColor(dpy, colormap, &fg_rgb);

   /* Set up pixmaps to be used as buttons */
   DPR(("x11_device: creating pixmaps\n"));
   win.up_pix = XCreatePixmapFromBitmapData(dpy, win.root,
	(char *)up_arrow_button_bits,
        up_arrow_button_width, up_arrow_button_height, fg, bg, default_depth);
   win.dn_pix=XCreatePixmapFromBitmapData(dpy, win.root,
	(char *)down_arrow_button_bits,
        down_arrow_button_width, down_arrow_button_height, fg, bg,
        default_depth);
   win.lt_pix=XCreatePixmapFromBitmapData(dpy, win.root,
	(char *)left_arrow_button_bits,
        left_arrow_button_width, left_arrow_button_height, fg, bg,
        default_depth);
   win.rt_pix=XCreatePixmapFromBitmapData(dpy, win.root,
	(char *)right_arrow_button_bits,
        right_arrow_button_width, right_arrow_button_height, fg, bg,
        default_depth);
   win.elev_pix = XCreatePixmapFromBitmapData(dpy, win.root,
	(char *)elevator_bits,
        elevator_width, elevator_height, fg, bg, default_depth);
   win.hist_but_pix = XCreatePixmapFromBitmapData(dpy, win.root,
	(char *)histogram_bits,
        histogram_width, histogram_height, fg, bg, default_depth);
   win.mag_pix = XCreatePixmapFromBitmapData(dpy, win.root, (char *)mag_bits,
        mag_width, mag_height, fg, bg, default_depth);
   win.cursor_pix = XCreatePixmapFromBitmapData(dpy, win.root,
	(char *)point_bits,
        point_width, point_height, fg, bg, default_depth);
     
   /* Now pixmaps needed as globals.  win.lock_pix and win.bit_mode_pix */
   /* are set up elsewhere from Hard, Soft, EightBit, or SevenBitPixmap */
   GrayPixmap = XCreatePixmapFromBitmapData(dpy, win.root, (char *)gray_bits,
        gray_width, gray_height, fg, bg, default_depth);
   HardPixmap = XCreatePixmapFromBitmapData(dpy, win.root,(char *)hard_lut_bits,
        hard_lut_width, hard_lut_height, fg, bg, default_depth);
   SoftPixmap = XCreatePixmapFromBitmapData(dpy, win.root,(char *)soft_lut_bits,
        soft_lut_width, soft_lut_height, fg, bg, default_depth);
   EightBitPixmap = XCreatePixmapFromBitmapData(dpy, win.root,
	(char *)eight_bit_bits,
        eight_bit_width, eight_bit_height, fg, bg, default_depth);
   SevenBitPixmap = XCreatePixmapFromBitmapData(dpy, win.root,
	(char *)seven_bit_bits,
        seven_bit_width, seven_bit_height, fg, bg, default_depth);
   win.lock_pix = SoftPixmap;
   win.bit_mode_pix = SevenBitPixmap;

   /* Create cursors */
   DPR(("x11_device: creating X Window cursors\n"));
   main_cursor     = XCreateFontCursor(dpy, XC_left_ptr);
   jump_cursor     = XCreateFontCursor(dpy, XC_exchange);
   close_cursor    = XCreateFontCursor(dpy, XC_icon);
   ud_cursor       = XCreateFontCursor(dpy, XC_sb_v_double_arrow);
   db_cursor       = XCreateFontCursor(dpy, XC_sb_down_arrow);
   ub_cursor       = XCreateFontCursor(dpy, XC_sb_up_arrow);
   lr_cursor       = XCreateFontCursor(dpy, XC_sb_h_double_arrow);
   rb_cursor       = XCreateFontCursor(dpy, XC_sb_right_arrow);
   lb_cursor       = XCreateFontCursor(dpy, XC_sb_left_arrow);
   up_cursor       = XCreateFontCursor(dpy, XC_top_side);
   left_cursor     = XCreateFontCursor(dpy, XC_left_side);
   watch_cursor    = XCreateFontCursor(dpy, XC_watch);

   DPR(("x11_device: creating virtual VRDI cursors\n"));
   v1_cursor = CreateCursor(dpy, win.root, (char *)v1_bits, (char *)v1_bits,
        v1_width, v1_height, v1_x_hot,v1_y_hot);
   v2_cursor = CreateCursor(dpy, win.root, (char *)v2_bits, (char *)v2_bits,
        v2_width, v2_height, v2_x_hot,v2_y_hot);
   v3_cursor = CreateCursor(dpy, win.root, (char *)v3_bits, (char *)v3_bits,
        v3_width, v3_height, v3_x_hot,v3_y_hot);
   v4_cursor = CreateCursor(dpy, win.root, (char *)v4_bits, (char *)v4_bits,
        v4_width, v4_height, v4_x_hot,v4_y_hot);
   v5_cursor = CreateCursor(dpy, win.root, (char *)v5_bits, (char *)v5_bits,
        v5_width, v5_height, v5_x_hot,v5_y_hot);
   v6_cursor = CreateCursor(dpy, win.root, (char *)v6_bits, (char *)v6_bits,
        v6_width, v6_height, v6_x_hot,v6_y_hot);
   watch_cursor = CreateCursor(dpy, win.root, (char *)watch_bits,
	(char *)watch_mask_bits,
        watch_width, watch_height, watch_x_hot, watch_y_hot);
   empty_cursor = CreateCursor(dpy, win.root, (char *)empty_bits,
	(char *)empty_bits,
        empty_width, empty_height, empty_x_hot, empty_y_hot);

   /* Create main user window */

   if (X_Image_Depth == 8) {
      DPR(("x11_device: creating 8-bit main window\n"));
      valuemask = CWColormap | CWBorderPixel;
      attributes.colormap = colormap;
      attributes.border_pixel = bg;
      win.main = XCreateWindow(dpy, win.root, x_loc, y_loc, width, height, 0,
         X_Image_Depth, InputOutput, vip.visual, valuemask, &attributes);
   }
   else {
      DPR(("x11_device: creating 24-bit main window\n"));
      win.main = XCreateSimpleWindow(dpy, win.root, x_loc, y_loc, width,
           height, 0, bg, bg);
   }

   valuemask = CWColormap | CWBorderPixel;
   attributes.colormap = colormap;
   attributes.border_pixel = bg;

   if (win.main == 0)
      error(dpy, &win, "Can't create main window");
   DPR(("x11_device: main window created\n"));

   /* Create image window */
   win.image = XCreateWindow(dpy, win.main, 0, 0, ZVIDEO_SAMPLES, ZVIDEO_LINES,
        0, X_Image_Depth, InputOutput, vip.visual, valuemask, &attributes);

   if (win.image == 0)
      error(dpy, &win, "Can't create image window");    
   DPR(("x11_device: image window created\n"));

/*
   status = XGetIconSizes(dpy, win.main, &icon_sizes, &num_sizes);
   if (!status) {
      DPR(("Window manager has not set default icon sizes."));
   }
   else {
      for (i = 0; i < num_sizes; i++) {
         DPR(("x11_device: min_width=%d, min_height=%d, max_width=%d, max_height=%d\n",
              icon_sizes->min_width, icon_sizes->min_height,
              icon_sizes->max_width, icon_sizes->max_height));
      }
   }
   XFree(icon_sizes);
*/
   icon_x_loc = 990;
   icon_y_loc = 0;
   icon_width = ICON_WD;
   icon_height = ICON_HT;

   /* Create icon window */
   win.icon.main = XCreateWindow(dpy, win.root, icon_x_loc, icon_y_loc,
        icon_width, icon_height, border_width, X_Image_Depth, InputOutput,
        vip.visual, valuemask, &attributes);

   if (win.icon.main == 0)
      error(dpy, &win, "Can't create icon window");
   DPR(("x11_device: icon window created\n"));

   /* Initialize VRDI cursors */
   for (i = 0; i < N_VCURSORS; i++) {
      vcursor[i].on = FALSE;
      set_vcursor_loc(dpy, &win, i, 1, 1);
   }

   /**** Tell the window manager about our windows ****/

   /* Set up main window for window manager */
   wm_hints.flags = StateHint | IconWindowHint | InputHint;
   wm_hints.initial_state = NormalState;
   wm_hints.icon_window   = win.icon.main;
   wm_hints.input = True;

   size_hints.flags = USPosition | USSize | PMinSize | PMaxSize | PResizeInc;
   size_hints.x = x_loc;
   size_hints.y = y_loc;
   size_hints.width = width;
   size_hints.height = height;
   size_hints.min_width = 128 + BDR_WW;
   size_hints.min_height = 128 + BDR_WW;
   size_hints.max_width = ZVIDEO_SAMPLES + BDR_WW;
   size_hints.max_height = ZVIDEO_LINES + BDR_WW;
   size_hints.width_inc = 1;
   size_hints.height_inc = 1;

   XSetWMHints(dpy, win.main, &wm_hints);
   XSetNormalHints(dpy, win.main, &size_hints);
   XStoreName(dpy, win.main, dev_name);
   XSetIconName(dpy, win.main, dev_name);

   wm_proto = XInternAtom(dpy, "WM_PROTOCOLS", True);
   wm_del_win = XInternAtom(dpy, "WM_DELETE_WINDOW", True);
   wm_cmap = XInternAtom(dpy, "WM_COLORMAP_WINDOWS", True);
   XChangeProperty(dpy, win.main, wm_proto, XA_ATOM, 32, PropModeReplace,
        (unsigned char *)&wm_del_win, 1);

   /* wm_cmap is apparently 0 (unavailable?) on some systems.  The effect */
   /* of not making this call is somewhat unclear, but it's better than   */
   /* simply crashing...  (rgd 2/2005)                                    */
   if (X_Image_Depth == 24 && wm_cmap != 0) {
      win_array[0] = win.image;
      win_array[1] = win.main;
      XChangeProperty(dpy, win.main, wm_cmap, XA_WINDOW, 32, PropModeReplace,
           (unsigned char *)win_array, 2);
   }

   initialize_histogram(dpy, &win);

   /* Set up icon window for window manager */
   wm_hints.flags = StateHint;
   wm_hints.initial_state = NormalState;

   size_hints.flags = PPosition | PSize | PMinSize | PMaxSize | PResizeInc;
   size_hints.x = icon_x_loc;
   size_hints.y = icon_y_loc;
   size_hints.width = icon_width;
   size_hints.height = icon_height;
   size_hints.min_width = icon_width;
   size_hints.min_height = icon_height;
   size_hints.max_width = icon_width;
   size_hints.max_height = icon_height;
   size_hints.width_inc = 0;
   size_hints.height_inc = 0;

   XSetWMHints(dpy, win.icon.main, &wm_hints);
   XSetNormalHints(dpy, win.icon.main, &size_hints);
   XStoreName(dpy, win.icon.main, dev_name);

   /* Customize the icon window */
   attributes.background_pixel = bg;
   attributes.border_pixel = bg;
   attributes.cursor = close_cursor;
   attributes.colormap = colormap;
   valuemask = (CWBackPixel | CWBorderPixel | CWCursor | CWColormap);

   XChangeWindowAttributes(dpy, win.icon.main, valuemask, &attributes);

   if (parent_window > 0) {
      /* Customize the parent window, saving its original attribute list */
      XGetWindowAttributes(dpy, parent_window, &parent_attributes);
      if (parent_attributes.visual->visualid == vip.visual->visualid)
         XSetWindowColormap(dpy, parent_window, colormap);
      else
         parent_window = 0;	/* Different visuals, so don't change cmap! */
   }

   /* Select the events for each high-level window */
   XSelectInput(dpy, win.image, NORM_EVENT_MASK);
   XSelectInput(dpy, win.main, MAIN_MASK);
   XSelectInput(dpy, win.icon.main, ICON_EVENT_MASK);

   CreateSubWindows(dpy, &win);

   DPR(("x11_device:  creating icon head window\n"));
   valuemask = CWBorderPixel | CWBackPixel | CWColormap;
   attributes.colormap = colormap;
   win.icon.head = XCreateWindow(dpy, win.icon.main, 0, 0, ICON_WD, BDR_WW, 0,
        CopyFromParent, InputOutput, CopyFromParent, valuemask, &attributes);

   /* Free the pixmaps that won't be used anymore */
   XFreePixmap(dpy, GrayPixmap);  

   /* Set up the XImage structures */
   x_image_str = XCreateImage(dpy, vip.visual, X_Image_Depth, ZPixmap, 0,
        (char *)X_IMPS, ZN_SAMPS, ZN_LINES, 8, 0);
   X_Bytes_Per_Line = x_image_str->bytes_per_line;
   X_Bytes_Per_Pixel = x_image_str->bits_per_pixel / 8;

   DPR(("x11_device: bitmap_unit=%d, bitmap_pad=%d, depth=%d, bytes_per_line=%d\n",
        x_image_str->bitmap_unit, x_image_str->bitmap_pad,
        x_image_str->depth, x_image_str->bytes_per_line));
   DPR(("x11_device: bits_per_pixel=%d, red=%d, green=%d, blue=%d\n",
        x_image_str->bits_per_pixel, x_image_str->red_mask,
        x_image_str->green_mask, x_image_str->blue_mask));
   DPR(("x11_device: byte_order=%d, bitmap_bit_order=%d\n",
        x_image_str->byte_order, x_image_str->bitmap_bit_order));
   DPR(("x11_device: nformats=%d\n", dpy->nformats));
#ifdef DEBUG
   for (i = 0; i < dpy->nformats; i++) {
      DPR(("     %d: depth=%d, bits_per_pixel=%d, pad=%d\n",
           i, dpy->pixmap_format[i].depth,
           dpy->pixmap_format[i].bits_per_pixel,
           dpy->pixmap_format[i].scanline_pad));
   }
#endif

   if (X_Image_Depth == 24) {
      if (x_image_str->byte_order == MSBFirst) {
         if (vip.blue_mask == 0x0000ff) {
            color_order[X_RED] = 1;
            color_order[X_GREEN] = 2;
            color_order[X_BLUE] = 3;
         }
         else {
            color_order[X_RED] = 3;
            color_order[X_GREEN] = 2;
            color_order[X_BLUE] = 1;
         }
      }
      else {
         if (vip.blue_mask == 0x0000ff) {
            color_order[X_RED] = 2;
            color_order[X_GREEN] = 1;
            color_order[X_BLUE] = 0;
         }
         else {
            color_order[X_RED] = 0;
            color_order[X_GREEN] = 1;
            color_order[X_BLUE] = 2;
         }
      }
   }
   else {
      color_order[X_RED] = 0;
      color_order[X_GREEN] = 0;
      color_order[X_BLUE] = 0;
   }

   /* allocate memory for the icon's image */
   ICON_IMAGE = (unsigned char *) malloc(ICON_HT*ICON_WD*X_Bytes_Per_Pixel);
   if (ICON_IMAGE == 0)
      error(dpy, &win, "Insufficient memory for icon image plane");
   memset(ICON_IMAGE, (int) 0, ICON_WD * ICON_HT * X_Bytes_Per_Pixel);

   icon_ximage = XCreateImage(dpy, vip.visual, X_Image_Depth, ZPixmap, 0,
        (char *)ICON_IMAGE, ICON_WD, ICON_HT, 8, 0);

   DPR(("x11_device: bitmap_unit=%d, bitmap_pad=%d, depth=%d, bytes_per_line=%d\n",
        icon_ximage->bitmap_unit, icon_ximage->bitmap_pad,
        icon_ximage->depth, icon_ximage->bytes_per_line));
   DPR(("x11_device: bits_per_pixel=%d, red=%d, green=%d, blue=%d\n",
        icon_ximage->bits_per_pixel, icon_ximage->red_mask,
        icon_ximage->green_mask, icon_ximage->blue_mask));
   DPR(("x11_device: byte_order=%d, bitmap_bit_order=%d\n",
        icon_ximage->byte_order, icon_ximage->bitmap_bit_order));

#if UNIX_OS
   /* Set up the image pixmap structure */
   image_pm = XCreatePixmap(dpy, win.root, ZN_SAMPS, ZN_LINES, X_Image_Depth);

   if (image_pm == None)
        error(dpy, &win, "Can't create pixmap\n");
#endif

   /* Set up the graphics contexts */

   valuemask = GCFont | GCForeground | GCBackground;
   xgcvalues.font = font_struct->fid;
   xgcvalues.foreground = bg;
   xgcvalues.background = fg;

   /* create the message window GC */
   message_gc = XCreateGC(dpy, win.message, valuemask, &xgcvalues);
   if (message_gc == 0)
      error(dpy, &win, "Cannot create message graphics context");

   xgcvalues.foreground = bg;
   xgcvalues.background = fg_rgb.pixel;
   /* create the icon window GC */
   icon_gc = XCreateGC(dpy, win.icon.head, valuemask, &xgcvalues);
   if (icon_gc == 0)
      error(dpy, &win, "Cannot create icon graphics context");

   valuemask = GCForeground | GCBackground;
   xgcvalues.foreground = fg_rgb.pixel;
   xgcvalues.background = bg;

   /* create the image GC */
   im_gc = XCreateGC(dpy, win.image, valuemask, &xgcvalues);
   if (im_gc == 0)
      error(dpy, &win, "Cannot create image graphics context");

   /* create the button GC */
   valuemask = GCForeground | GCBackground;
   xgcvalues.foreground = fg;
   xgcvalues.background = bg;
   button_gc = XCreateGC(dpy, win.root, valuemask, &xgcvalues);
   if (button_gc == 0)
      error(dpy, &win, "Cannot create button graphics context");

#if UNIX_OS     
   valuemask = GCForeground | GCBackground;
   xgcvalues.foreground = fg_rgb.pixel;
   xgcvalues.background = bg;

   /* create the pixmap GC */
   pm_gc = XCreateGC(dpy, image_pm, valuemask, &xgcvalues);
   if (pm_gc == 0)
      error(dpy, &win, "Cannot create pixmap graphics context");
#endif

   /* Map the subwindows */
   XMapWindow(dpy, win.image);
   XMapWindow(dpy, win.mag);
   map_controls(dpy, &win);

   /* Map the main window */
   XMapWindow(dpy, win.main);

   set_vcursor_pattern(0, 1);
   vcursor[0].on = TRUE;
   set_vcursor_autotrack(dpy, &win, 0, TRUE);
   set_floating_cursor(dpy, &win, 0);

   /* Enter the window/socket event loop */
   x11_handler();

   /* Free the communications socket */
   DPR(("x11_device: freeing the socket\n"));
   socket_free(window_fd, &mstr_header);

#if UNIX_OS
   /* Close and remove the lock file */
   DPR(("x11_device: closing and removing the lock file\n"));
   close(lock_fd);
   unlink(lock_file);
#endif

   /* Reset the parent window's original attributes */
   if (parent_window > 0)
      XSetWindowColormap(dpy, parent_window, parent_attributes.colormap);

   DPR(("x11_device: destroying the image structures\n"));
   XDestroyImage(x_image_str);
   XDestroyImage(icon_ximage);

   DPR(("x11_device: freeing the image plane memory\n"));
   free(X_IMPS);
   free(VRDI_IMPS);
   free(X_LUTS);
   free(VRDI_LUTS);
   free(X_OVLY_LUTS);
   free(VRDI_OVLY_LUTS);
   free(ICON_IMAGE);

   /* Close the display */
   DPR(("x11_device: closing the X display\n"));
   XCloseDisplay(dpy);
}


Cursor CreateCursor(dpy, win, bits, mask_bits, width, height, x, y)
Display       *dpy;
Window         win;
unsigned char *bits, *mask_bits;
int            width, height, x, y;
{
   Cursor cursor;
   XColor fgcolor, bgcolor;
   int screen;

   screen = DefaultScreen(dpy);
   fgcolor.pixel = WhitePixel(dpy, screen);
   bgcolor.pixel = BlackPixel(dpy, screen);
   XQueryColor(dpy,XDefaultColormap(dpy,screen),&fgcolor);
   XQueryColor(dpy,XDefaultColormap(dpy,screen),&bgcolor);

   cursor = XCreatePixmapCursor(dpy,
        XCreateBitmapFromData(dpy, win, (char *)bits, width, height),
        XCreateBitmapFromData(dpy, win, (char *)mask_bits, width, height),
        &fgcolor, &bgcolor, x, y);

   return (cursor);
}


int get_visual_model(dpy, screen, depth, class, vip)
Display *dpy;
int screen, *depth, *class;
XVisualInfo *vip;
{
   int i, status, nvi;
   XVisualInfo *tvip;

   *depth = 24;
   *class = DirectColor;
   status = XMatchVisualInfo(dpy, screen, *depth, *class, vip);
   if (status) {
      DPR(("x11_device: setting visual mode to 24-bit DirectColor\n"));
      return (SUCCESS);
   }

   *class = TrueColor;
   status = XMatchVisualInfo(dpy, screen, *depth, *class, vip);
   if (status) {
      DPR(("x11_device: setting visual mode to 24-bit TrueColor\n"));
      return (SUCCESS);
   }

   *depth = 8;
   *class = PseudoColor;
   status = XMatchVisualInfo(dpy, screen, *depth, *class, vip);
   if (status) {
      DPR(("x11_device: setting visual mode to 8-bit PseudoColor\n"));
      return (SUCCESS);
   }

   *class = StaticColor;
   status = XMatchVisualInfo(dpy, screen, *depth, *class, vip);
   if (status) {
      DPR(("x11_device: setting visual mode to 8-bit StaticColor\n"));
      return (SUCCESS);
   }

   *class = GrayScale;
   status = XMatchVisualInfo(dpy, screen, *depth, *class, vip);
   if (status) {
      DPR(("x11_device: setting visual mode to 8-bit GrayScale\n"));
      return (SUCCESS);
   }

   *class = StaticGray;
   status = XMatchVisualInfo(dpy, screen, *depth, *class, vip);
   if (status) {
      DPR(("x11_device: setting visual mode to 8-bit StaticGray\n"));
      return (SUCCESS);
   }

   DPR(("x11_device: No valid visual modes available.\n"));

#ifdef DEBUG
   if ((tvip = XGetVisualInfo(dpy, VisualNoMask, NULL, &nvi)) != NULL) {
      DPR(("x11_device: Checking %d available visual modes\n", nvi));
      for (i = 0; i < nvi; i++) {
         switch (tvip[i].class) {
            case DirectColor:
               DPR(("     Mode %d:  %d-bit, DirectColor, screen=%d, cmapsize=%d, rgb_bits=%d\n",
                    i, tvip[i].depth, tvip[i].screen, tvip[i].colormap_size,
                    tvip[i].bits_per_rgb));
               break;
            case TrueColor:
               DPR(("     Mode %d:  %d-bit, TrueColor, screen=%d, cmapsize=%d, rgb_bits=%d\n",
                    i, tvip[i].depth, tvip[i].screen, tvip[i].colormap_size,
                    tvip[i].bits_per_rgb));
               break;
            case PseudoColor:
               DPR(("     Mode %d:  %d-bit, PseudoColor, screen=%d, cmapsize=%d, rgb_bits=%d\n",
                    i, tvip[i].depth, tvip[i].screen, tvip[i].colormap_size,
                    tvip[i].bits_per_rgb));
               break;
            case StaticColor:
               DPR(("     Mode %d:  %d-bit, StaticColor, screen=%d, cmapsize=%d, rgb_bits=%d\n",
                    i, tvip[i].depth, tvip[i].screen, tvip[i].colormap_size,
                    tvip[i].bits_per_rgb));
               break;
            case GrayScale:
               DPR(("     Mode %d:  %d-bit, GrayScale, screen=%d, cmapsize=%d, rgb_bits=%d\n",
                    i, tvip[i].depth, tvip[i].screen, tvip[i].colormap_size,
                    tvip[i].bits_per_rgb));
               break;
            case StaticGray:
               DPR(("     Mode %d:  %d-bit, StaticColor, screen=%d, cmapsize=%d, rgb_bits=%d\n",
                    i, tvip[i].depth, tvip[i].screen, tvip[i].colormap_size,
                    tvip[i].bits_per_rgb));
               break;
         }
      }
   }
#endif
   return (FAILURE);
}


CreateSubWindows(dpy, win)
Display *dpy;
MAIN_WINDOW *win;
{
   XSetWindowAttributes attributes;
   unsigned long int valuemask;
   XWindowAttributes win_info;
   int wd, ht;

   XGetWindowAttributes(dpy, win->main, &win_info);

   wd = win_info.width;
   ht = win_info.height;

   attributes.background_pixel = bg;
   attributes.border_pixel = bg;
   attributes.colormap = default_cmap;
   attributes.event_mask = ExposureMask;

   DPR(("x11_device:  creating magnify button window\n"));
   valuemask = CWBorderPixel | CWBackPixel | CWEventMask;
   win->mag = XCreateWindow(dpy, win->main, 0, ht-BDR_WW, BDR_WW, BDR_WW, 0,
        CopyFromParent, InputOutput, CopyFromParent, valuemask, &attributes);

   DPR(("x11_device:  creating LUT button window\n"));
   valuemask = CWBorderPixel | CWBackPixel | CWEventMask;
   win->lock = XCreateWindow(dpy, win->main, BDR_WW, ht-BDR_WW, BDR_WW, BDR_WW,
        0, CopyFromParent, InputOutput, CopyFromParent, valuemask, &attributes);

   DPR(("x11_device:  creating cursor button window\n"));
   valuemask = CWBorderPixel | CWBackPixel | CWEventMask;
   win->cursor = XCreateWindow(dpy, win->main, wd-2*BDR_WW, ht-BDR_WW, BDR_WW,
        BDR_WW, 0, CopyFromParent, InputOutput, CopyFromParent, valuemask,
        &attributes);

   DPR(("x11_device:  creating histogram button window\n"));
   valuemask = CWBorderPixel | CWBackPixel | CWEventMask;
   win->hist_but = XCreateWindow(dpy, win->main, wd-3*BDR_WW, ht-BDR_WW, BDR_WW,
        BDR_WW, 0, CopyFromParent, InputOutput, CopyFromParent, valuemask,
        &attributes);

   DPR(("x11_device:  creating right fill window\n"));
   valuemask = CWBorderPixel | CWBackPixmap;
   attributes.background_pixmap = GrayPixmap;
   win->right_fill = XCreateWindow(dpy, win->main, wd-BDR_WW, 0, BDR_WW,
        ht-BDR_WW, 0, CopyFromParent, InputOutput, CopyFromParent, valuemask,
        &attributes);

   DPR(("x11_device:  creating message window\n"));
   valuemask = CWBorderPixel | CWBackPixel;
   attributes.background_pixel = fg;
   win->message = XCreateWindow(dpy, win->main, BDR_WW, ht-BDR_WW, wd-BDR_WW,
        BDR_WW, 0, CopyFromParent, InputOutput, CopyFromParent, valuemask,
        &attributes);
   attributes.background_pixel = bg;

   DPR(("x11_device:  creating vertical scrollbar window\n"));
   valuemask = CWBorderPixel | CWCursor | CWBackPixmap;
   attributes.background_pixmap = GrayPixmap;
   attributes.cursor = ud_cursor;
   win->s1 = XCreateWindow(dpy, win->main, wd-BDR_WW, BDR_WW, BDR_WW,
        ht-BDR_WW*2, 0, CopyFromParent, InputOutput, CopyFromParent, valuemask,
        &attributes);

   DPR(("x11_device:  creating vertical scrollbar control window\n"));
   valuemask = CWBorderPixel | CWCursor | CWBackPixel | CWEventMask;
   attributes.cursor = ud_cursor;
   win->ve = XCreateWindow(dpy, win->main, wd-BDR_WW, BDR_WW, BDR_WW, BDR_WW,
        0, CopyFromParent, InputOutput, CopyFromParent, valuemask, &attributes);

   DPR(("x11_device:  creating vertical scrollbar up window\n"));
   valuemask = CWBorderPixel | CWCursor | CWBackPixel | CWEventMask;
   attributes.cursor = ub_cursor;
   win->up = XCreateWindow(dpy, win->main, wd-BDR_WW, 0, BDR_WW, BDR_WW, 0,
        CopyFromParent, InputOutput, CopyFromParent, valuemask, &attributes);

   DPR(("x11_device:  creating vertical scrollbar down window\n"));
   valuemask = CWBorderPixel | CWCursor | CWBackPixel | CWEventMask;
   attributes.cursor = db_cursor;
   win->dn = XCreateWindow(dpy, win->main, wd-BDR_WW, ht-BDR_WW*2, BDR_WW,
        BDR_WW, 0, CopyFromParent, InputOutput, CopyFromParent, valuemask,
        &attributes);

   switch (X_Image_Depth) {
      case 24:
         win->bit_mode = None;
         DPR(("x11_device:  creating horizontal scrollbar window\n"));
         valuemask = CWBorderPixel | CWCursor | CWBackPixmap;
         attributes.background_pixmap = GrayPixmap;
         attributes.cursor = lr_cursor;
         win->s2 = XCreateWindow(dpy, win->main, 3*BDR_WW, ht-BDR_WW,
               wd-7*BDR_WW, BDR_WW, 0, CopyFromParent, InputOutput,
               CopyFromParent, valuemask, &attributes);

         DPR(("x11_device:  creating horizontal scrollbar control window\n"));
         valuemask = CWBorderPixel | CWCursor | CWBackPixel | CWEventMask;
         attributes.cursor = lr_cursor;
         win->he = XCreateWindow(dpy, win->main, 3*BDR_WW, ht-BDR_WW, BDR_WW,
              BDR_WW, 0, CopyFromParent, InputOutput, CopyFromParent, valuemask,
              &attributes);

         DPR(("x11_device:  creating horizontal scrollbar left window\n"));
         valuemask = CWBorderPixel | CWCursor | CWBackPixel | CWEventMask;
         attributes.cursor = lb_cursor;
         win->lt = XCreateWindow(dpy, win->main, 2*BDR_WW, ht-BDR_WW, BDR_WW,
              BDR_WW, 0, CopyFromParent, InputOutput, CopyFromParent,
              valuemask, &attributes);

         DPR(("x11_device:  creating horizontal scrollbar right window\n"));
         valuemask = CWBorderPixel | CWCursor | CWBackPixel | CWEventMask;
         attributes.cursor = rb_cursor;
         win->rt = XCreateWindow(dpy, win->main, wd-4*BDR_WW, ht-BDR_WW,
              BDR_WW, BDR_WW, 0, CopyFromParent, InputOutput, CopyFromParent,
              valuemask, &attributes);
         break;
      case 8:
         DPR(("x11_device: creating bit-mode toggle window\n"));
         valuemask = CWBorderPixel | CWCursor | CWBackPixel | CWEventMask;
         attributes.cursor = main_cursor;
         win->bit_mode = XCreateWindow(dpy, win->main, 2*BDR_WW, ht-BDR_WW,
              BDR_WW, BDR_WW, 0, CopyFromParent, InputOutput, CopyFromParent,
              valuemask, &attributes);

         DPR(("x11_device:  creating horizontal scrollbar window\n"));
         valuemask = CWBorderPixel | CWCursor | CWBackPixmap;
         attributes.background_pixmap = GrayPixmap;
         attributes.cursor = lr_cursor;
         win->s2 = XCreateWindow(dpy, win->main, 4*BDR_WW, ht-BDR_WW,
            wd-8*BDR_WW, BDR_WW, 0, CopyFromParent, InputOutput, CopyFromParent,
            valuemask, &attributes);

         DPR(("x11_device:  creating horizontal scrollbar control window\n"));
         valuemask = CWBorderPixel | CWCursor | CWBackPixel | CWEventMask;
         attributes.cursor = lr_cursor;
         win->he = XCreateWindow(dpy, win->main, 4*BDR_WW, ht-BDR_WW, BDR_WW,
              BDR_WW, 0, CopyFromParent, InputOutput, CopyFromParent,
              valuemask, &attributes);

         DPR(("x11_device:  creating horizontal scrollbar left window\n"));
         valuemask = CWBorderPixel | CWCursor | CWBackPixel | CWEventMask;
         attributes.cursor = lb_cursor;
         win->lt = XCreateWindow(dpy, win->main, 3*BDR_WW, ht-BDR_WW, BDR_WW,
              BDR_WW, 0, CopyFromParent, InputOutput, CopyFromParent,
              valuemask, &attributes);

         DPR(("x11_device:  creating horizontal scrollbar right window\n"));
         valuemask = CWBorderPixel | CWCursor | CWBackPixel | CWEventMask;
         attributes.cursor = rb_cursor;
         win->rt = XCreateWindow(dpy, win->main, wd-4*BDR_WW, ht-BDR_WW,
              BDR_WW, BDR_WW, 0, CopyFromParent, InputOutput, CopyFromParent,
              valuemask, &attributes);
         break;
   }
}
