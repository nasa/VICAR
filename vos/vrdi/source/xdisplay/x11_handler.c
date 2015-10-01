/* SCCS %W% %G%  %Q% */
/******************************************************************************/
/*                                                                            */
/*  File: x11_handler.c                                                       */
/*                                                                            */
/*  Routines:                                                                 */
/*                                                                            */
/*	x11_handler()                                                         */
/*                                                                            */
/*	x11_err_handler(sig, code, scp)                                       */
/*		int sig, code;                                                */
/*		struct sigcontext *scp;                                       */
/*                                                                            */
/*	x11_xerr_handler(dpy, xerror)                                         */
/*		XErrorEvent *xerror;                                          */
/*                                                                            */
/*	x11_xioerr_handler(dpy)                                               */
/*		Display *dpy;                                                 */
/*                                                                            */
/*	handle_socket_request(phone_fd, s_active, s_intact)                   */
/*		int phone_fd, int *s_active, int *s_intact;                   */
/*                                                                            */
/*	update_batch_area(ss, sl, es, el, imp)                                */
/*		int ss, sl, es, el, imp;                                      */
/*                                                                            */
/*	process_batch()                                                       */
/*                                                                            */
/*	update_display(ss, sl, ns, nl, imp)                                   */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		int ss, sl, ns, nl, imp;                                      */
/*                                                                            */
/*	display_vector(imp, npts, xvec, yvec, value, mask, ss, sl, es, el)    */
/*		int imp, npts, xvec[], yvec[];                                */
/*		unsigned char value, mask;                                    */
/*		int *ss, *sl, *es, *el;                                       */
/*                                                                            */
/*	display_circle(imp, xcenter, ycenter, radius, color, mask, area,      */
/*	               ss, sl, es, el)                                        */
/*		int imp, xcenter, ycenter, radius;                            */
/*		unsigned char color, mask;                                    */
/*		int area[4], *ss, *sl, *es, *el;                              */
/*                                                                            */
/*	check_min_max(xpos, ypos, xmin, xmax, ymin, ymax)                     */
/*		int xpos, ypos, *xmin, *xmax, *ymin, *ymax;                   */
/*                                                                            */
/*	process_keyboard_input(key, event)                                    */
/*		KeySym key;                                                   */
/*		XEvent *event;                                                */
/*                                                                            */
/*	void process_socket_connect()                                         */
/*                                                                            */
/*	void process_socket_request(sock_index)                               */
/*		int sock_index;                                               */
/*                                                                            */
/*  Written by:			???                                           */
/*  Date:			???                                           */
/*                                                                            */
/*  Cognizant Programmer:	Paul Bartholomew                              */
/*  Revision History:                                                         */
/*    Date    FR #   Description                                              */
/*  --------  -----  -------------------------------------------------------  */
/*  08-01-92   ---   PDB-Complete rewrite.                                    */
/*  07-26-95   ---   VXP-Implemented Move, Resize routines 		      */
/*                                                                            */
/******************************************************************************/

#define _BSD_SIGNALS		/* Needed for SGI compiler (see man page */
				/* for sigvec).                          */
#include "xvmaininc.h"
#include <signal.h>
#include <time.h>
#include "xdexterns.h"
#include "xdfuncs.h"
#include "x11_device_main.h"
#if VMS_OS && ALPHA_ARCH
#undef MIN              /* redefined by Intrinsic.h */
#undef MAX
#endif
#include <X11/keysym.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#if VMS_OS
#if VAX_ARCH
#define caddr_t caddr_t2	/* avoid multiple def between socket.h and Xt */
#include <socket.h>
#undef caddr_t
#else				/* ALPHA_ARCH */
#include <socket.h>
#endif
#else				/* UNIX_OS */
#include <sys/socket.h>
#endif

/* #define DEBUG */
#ifdef DEBUG
#define DPR(A)  {printf A; fflush(stdout);}
#else
#define DPR(A)
#endif

#define SGO if (*s_active && *s_intact) *s_intact =
#define MAX_SCONN	50	/* Max # of socket connections */
/* The following are global only for the error handlers */
int sock_active[MAX_SCONN+1];	/* True if socket connection exists */
int sock_intact[MAX_SCONN+1];	/* False if error occurs but conn still there */
int sock_fd[MAX_SCONN+1];
XtInputId sock_input[MAX_SCONN+1];
int sock_num;			/* Number of connections */
int sock_index;			/* Index number of current socket */

#define X_LEFT		0
#define X_TOP		1
#define X_RIGHT		2
#define X_BOTTOM	3

extern int unit;
extern Display *dpy;
extern MAIN_WINDOW win;
extern int window_fd;
extern XtAppContext app_context;
extern Window parent_window;
extern Colormap colormap, default_cmap;
extern XColor default_colors[];
extern Cursor main_cursor;
extern unsigned long NORM_EVENT_MASK;
extern VRDIcursor vcursor[];

int lbutton_down = FALSE;
int mbutton_down = FALSE;
int rbutton_down = FALSE;
int key1_down = FALSE;
int key2_down = FALSE;
int key3_down = FALSE;
int key4_down = FALSE;
int key5_down = FALSE;
int key6_down = FALSE;

int contin = TRUE;
int err_cnt = 0;
unsigned long last_err_time = 0;
int batch_area[6];
Window main_win_for_error;

#ifdef DEBUG
char *event_array[] = { "Null",
			"Null",
			"KeyPress",
			"KeyRelease",
			"ButtonPress",
			"ButtonRelease",
			"MotionNotify",
			"EnterNotify",
			"LeaveNotify",
			"FocusIn",
			"FocusOut",
			"KeymapNotify",
			"Expose",
			"GraphicsExpose",
			"NoExpose",
			"VisibilityNotify",
			"CreateNotify",
			"DestroyNotify",
			"UnmapNotify",
			"MapNotify",
			"MapRequest",
			"ReparentNotify",
			"ConfigureNotify",
			"ConfigureRequest",
			"GravityNotify",
			"ResizeRequest",
			"CirculateNotify",
			"CirculateRequest",
			"PropertyNotify",
			"SelectionClear",
			"SelectionRequest",
			"SelectionNotify",
			"ColormapNotify",
			"ClientMessage",
			"MappingNotify",
			"LASTEvent" };
#endif


x11_handler()
{
   void process_socket_connect();

   int i, j, incr, ns, nl, imp, x, y, found;
   int cnum, save_cursor;
   KeySym key;
   char keybuffer[20], *client_msg;
   int charcount, bufsize=20;
   XEvent event;
   int status;
   static int first_time = TRUE;
   int temp_fd;

   /* X Windows error handlers */
   void x11_err_handler();
   int x11_xerr_handler();
   int x11_xioerr_handler();


#if SIGVEC_OS
   /* signal handler variables */
   struct sigvec vec, ovec;

   sock_index = -1;

   main_win_for_error = win.main;

   vec.sv_handler = x11_err_handler;
   vec.sv_mask = 0;
   vec.sv_onstack = 0;
   sigvec(SIGINT, &vec, &ovec);
   sigvec(SIGPIPE, &vec, &ovec);

#else
#if SIGACT_OS
   /* signal handler variables */
   struct sigaction vec, ovec;

   sock_index = -1;

   main_win_for_error = win.main;

   vec.sa_handler = x11_err_handler;
   vec.sa_flags = 0;
   sigaction(SIGINT, &vec, &ovec);
   sigaction(SIGPIPE, &vec, &ovec);

#else
   /* signal handler variables */
   struct sigvec vec, ovec;

   sock_index = -1;

   main_win_for_error = win.main;

   vec.sv_handler = x11_err_handler;
   vec.sv_mask = 0;
   vec.sv_onstack = 0;
   sigvector(SIGINT, &vec, &ovec);
   sigvector(SIGPIPE, &vec, &ovec);
#endif
#endif

   XSetIOErrorHandler(x11_xioerr_handler);
   XSetErrorHandler(x11_xerr_handler);

   sock_num = 0;
   for (i = 0; i < MAX_SCONN; i++) {
      sock_active[i] = FALSE;
      sock_intact[i] = FALSE;
      sock_fd[i] = -1;
   }

   if (X_Lut_RW)
      unlock_colormap(dpy, &win);

   batch_area[0] = 0;
   batch_area[1] = ZN_SAMPS+1;
   batch_area[2] = ZN_LINES+1;
   batch_area[3] = 0;
   batch_area[4] = 0;
   batch_area[5] = 0;

#if VMS_OS
   temp_fd = sock_efn(window_fd);
   XtAppAddInput(app_context, temp_fd, NULL,
	process_socket_connect, NULL);
#else
   temp_fd = window_fd;
   XtAppAddInput(app_context, temp_fd, (XtPointer)XtInputReadMask,
	process_socket_connect, NULL);
#endif

   while (contin) {

      XtAppNextEvent(app_context, &event);

      DPR(("x11_handler: event = %s\n", event_array[event.type]));
      if (event.type == Expose) {
         DPR(("x11_handler: Expose: count=%d, x=%d, y=%d, w=%d, h=%d, window=%d\n",
              event.xexpose.count, event.xexpose.x, event.xexpose.y,
              event.xexpose.width, event.xexpose.height, event.xexpose.window));
         if (event.xexpose.count == 0) {
            for (found = FALSE, imp = 1; imp < ZN_IMPS; imp++) {
               if (event.xexpose.window == win.hist[imp-1].main) {
                  found = TRUE;
                  refresh_histogram(dpy, &win, imp);
               }
            }
            if (!found) {		/* Check for button exposures */
               if (event.xexpose.window == win.mag)		/* magnify */
                  redraw_button(dpy, win.mag, win.mag_pix);
               else if (event.xexpose.window == win.lock)	/* colormap */
                  redraw_button(dpy, win.lock, win.lock_pix);
               else if (event.xexpose.window == win.ve)		/* vert elev */
                  redraw_button(dpy, win.ve, win.elev_pix);
               else if (event.xexpose.window == win.up)		/* up scroll */
                  redraw_button(dpy, win.up, win.up_pix);
               else if (event.xexpose.window == win.dn)		/* down scrl */
                  redraw_button(dpy, win.dn, win.dn_pix);
               else if (event.xexpose.window == win.he)		/* horiz elev */
                  redraw_button(dpy, win.he, win.elev_pix);
               else if (event.xexpose.window == win.lt)		/* left scrl */
                  redraw_button(dpy, win.lt, win.lt_pix);
               else if (event.xexpose.window == win.rt)		/* right scrl */
                  redraw_button(dpy, win.rt, win.rt_pix);
               else if (event.xexpose.window == win.cursor)	/* cursor mode*/
                  redraw_button(dpy, win.cursor, win.cursor_pix);
               else if (event.xexpose.window == win.hist_but)	/* histogram */
                  redraw_button(dpy, win.hist_but, win.hist_but_pix);
               else if ((X_Image_Depth == 8) &&
                        (event.xexpose.window == win.bit_mode)) /* 7/8 bit */
                  redraw_button(dpy, win.bit_mode, win.bit_mode_pix);

               else if (event.xexpose.window == win.icon.main)	/* Icon */
                  refresh_icon(dpy, &win);

               else if (event.xexpose.window == win.image) {	/* image win */
                  if (first_time) {
                     compile_image(1, 1, ZN_SAMPS, ZN_LINES, X_ALL_IMPS);
                     first_time = FALSE;
                  }
                  refresh_windows(dpy, &win);
		  /* Discard any other Expose events for this window */
		  while (XCheckTypedWindowEvent(dpy, win.image, Expose, &event))
		     ;
               }
#ifdef DEBUG
               else
                  DPR(("Unknown window on expose\n"));
#endif
            }
         }
      }

      else if (event.type == ColormapNotify) {
         if (event.xcolormap.colormap == default_cmap) {
            if (X_Bit_Mode == X_7BIT_MODE) {
               DPR(("x11_handler: the default colormap changed\n"));
               XQueryColors(dpy, default_cmap, default_colors, X_LUT_SIZE);
               if (X_Color_Locked)
                  set_colormap(dpy, X_RAMP_LUTS);
               else
                  set_colormap(dpy, X_USE_SW_LUTS);
            }
         }
      }

      else if (event.type == EnterNotify) {
         if (parent_window > 0)
            XSetWindowColormap(dpy, parent_window, colormap);
      }

      else if (event.type == ClientMessage) {
         DPR(("x11_handler: ClientMessage event received\n"));
         client_msg = XGetAtomName(dpy, event.xclient.message_type);

         if (client_msg != NULL) {
            DPR(("x11_handler: client message is '%s'\n", client_msg));
            if (strcmp(client_msg, "WM_PROTOCOLS") == 0) {
               if (event.xclient.window == win.main) {
                  DPR(("x11_handler: shutting down socket connections\n"));
                  if (sock_active[sock_index]) {
                     socket_close(sock_fd[sock_index], 0);
                     sock_active[sock_index] = FALSE;
                     sock_intact[sock_index] = FALSE;
                     XtRemoveInput(sock_input[sock_index]);
                  }
                  DPR(("x11_handler: closing the display window\n"));
                  contin = FALSE;
               }

               for (imp = 1; imp < ZN_IMPS; imp++) {
                  if (event.xclient.window == win.hist[imp-1].main) {
                     win.hist[imp-1].histogram_on = FALSE;
                     XUnmapWindow(dpy, win.hist[imp-1].main);
                  }
               }
            }
            XFree(client_msg);
         }
      }

      else if (event.xany.window == win.icon.main) {
         switch (event.type) {		/* Expose is caught above */
            case MapNotify:
            case ConfigureNotify:
               refresh_icon(dpy, &win);
               break;
            default:
               break;
         }
      }

      else if (event.xany.window == win.main) {
         switch (event.type) {
            case ConfigureNotify:
               DPR(("x11_handler: ConfigureNotify on win.main\n"));
               reconfigure(dpy, &win);
               break;
            case MapNotify:
               break;
            case ButtonPress:
               if (event.xbutton.subwindow == win.cursor)
                  break;
               else if (event.xbutton.subwindow == win.lock)
                  break;
               else if (event.xbutton.subwindow == win.mag)
                  break;
               else if (event.xbutton.subwindow == win.hist_but)
                  break;
               else if (event.xbutton.subwindow == win.ve)
                  vert_elev(dpy, &win);
               else if (event.xbutton.subwindow == win.s1)
                  vert_scroll(dpy, &win, event);
               else if (event.xbutton.subwindow == win.up)
                  up_scroll(dpy, &win);
               else if (event.xbutton.subwindow == win.dn)
                  down_scroll(dpy, &win);
               else if (event.xbutton.subwindow == win.s2)
                  horiz_scroll(dpy, &win, event);
               else if (event.xbutton.subwindow == win.he)
                  horiz_elev(dpy, &win);
               else if (event.xbutton.subwindow == win.lt)
                  left_scroll(dpy, &win);
               else if (event.xbutton.subwindow == win.rt)
                  right_scroll(dpy, &win);
               else {
                  if (X_Image_Depth == 8) {
                     if (event.xbutton.subwindow == win.bit_mode)
                        break;
                  }
               }
               break;

            case ButtonRelease:
               if (event.xbutton.subwindow == win.cursor) {
                  if (X_Cursor_Mode == X_FREE_FLOATING) {
                     X_Cursor_Mode = X_PLANTED;
                     XDefineCursor(dpy, win.image, main_cursor);
                     if ((cnum = find_active_cursor()) != -1)
                        turn_vcursor_on(dpy, &win, cnum);
                  }
                  else {
                     if ((cnum = find_active_cursor()) != -1) {
                        set_floating_cursor(dpy, &win, cnum);
                        turn_vcursor_off(dpy, &win, cnum);
                        vcursor[cnum].on = TRUE;
                        X_Cursor_Mode = X_FREE_FLOATING;
                     }
                     else {
                        soft_error(dpy, &win, "No active cursor.");
                     }
                  }
                  DPR(("x11_handler: cursor mode=%d\n", X_Cursor_Mode));
               }
               else if (event.xbutton.subwindow == win.lock) {
                  if (X_Color_Locked)
                     unlock_colormap(dpy, &win, 0);
                  else
                     lock_colormap(dpy, &win, 0);
               }
               else if (event.xbutton.subwindow == win.mag) {
                  for (cnum=0, save_cursor=0; cnum < N_VCURSORS; cnum++) {
                     if (vcursor[cnum].on) {
                        turn_vcursor_off(dpy, &win, cnum);
                        save_cursor |= 1 << cnum;
                        DPR(("x11_handler: turning cursor %d off\n", cnum));
                     }
                  }

                  if (sock_num != 0) {
                     sock_fd[sock_num] = window_fd;	/* for connect reqs */
                     go_magnify(dpy, &win, sock_num+1, sock_fd);
                  }
                  else
                     go_magnify(dpy, &win, 1, &window_fd);

                  DPR(("x11_handler: save_cursor=%d\n", save_cursor));
                  for (cnum = 0; cnum < N_VCURSORS; cnum++) {
                     if (save_cursor & (1 << cnum)) {
                        DPR(("x11_handler: turning cursor %d on\n", cnum));
                        turn_vcursor_on(dpy, &win, cnum);
                     }
                  }
               }
               else if (event.xbutton.subwindow == win.hist_but)
                  histogram_toggle(dpy, &win);
               else {
                  if (X_Image_Depth == 8) {
                     if (event.xbutton.subwindow == win.bit_mode) {
                        if (X_Bit_Mode == X_8BIT_MODE)
                           set_7bit_mode(dpy, &win);
                        else
                           set_8bit_mode(dpy, &win);
                     }
                  }
               }
               break;

            default:
               break;
         }
      }

      else if (event.xany.window == win.image) {
         switch (event.type) {
            case LeaveNotify:
               DPR(("x11_handler: LeaveNotify on win.image\n"));
               key1_down = FALSE;
               key2_down = FALSE;
               key3_down = FALSE;
               key4_down = FALSE;
               key5_down = FALSE;
               key6_down = FALSE;
               break;
            case KeyPress:
               DPR(("x11_handler: KeyPress in win.image\n"));
               charcount = XLookupString((XKeyEvent *)&event, keybuffer,
							bufsize, &key, NULL);
               DPR(("x11_handler: keysym is 0x%x, count=%d\n", key, charcount));
               process_keyboard_input(key, &event);
               break;
            case KeyRelease:
               DPR(("x11_handler: KeyRelease in win.image\n"));
               charcount = XLookupString((XKeyEvent *)&event, keybuffer,
							bufsize, &key, NULL);
               DPR(("x11_handler: keysym is 0x%x, count=%d\n", key, charcount));
               process_keyboard_input(key, &event);
               break;
            case ButtonPress:
               if (X_Cursor_Mode == X_PLANTED) {
                  if (sock_num != 0) {
                     sock_fd[sock_num] = window_fd;
                     cursor_select(dpy, &win, sock_num+1, sock_fd);
                  }
                  else
                     cursor_select(dpy, &win, 1, &window_fd);
               }
               else {
                  if ((cnum = find_active_cursor()) != -1)
                     read_vcursor(dpy, &win, cnum, &x, &y);
               }
               break;
            case UnmapNotify:
               break;
            default:
               break;
         }
      }

      for (i = 1; i <= ZN_LUTS; i++) {
         if (event.xany.window == win.hist[i-1].main) {
            switch (event.type) {
               case ConfigureNotify:	/* Expose is done above */
                  refresh_histogram(dpy, &win, i);
		  break;
               case ButtonPress:
                  switch (event.xbutton.button) {
                     case Button1:
                        incr = -1;
                        break;
                     case Button2:
                        incr = 0;
                        break;
                     case Button3:
                        incr = 1;
                        break;
                     default:
                        incr = 0;
                        soft_error(dpy, &win, "Unknown button on histogram");
                  }
                  adjust_spike(dpy, &win, incr, i);
                  break;
               default:
                  break;
            }
         }
      }
   } /* matches while(contin) */
} /* end of x11_handler */


void x11_err_handler(sig, code, scp)
int sig, code;
void *scp;	/* was struct sigcontext * */
{
   int i;
   char msg[81];

   if (sig != SIGINT && sig != SIGPIPE) {
      sprintf(msg, "VRDI X11 Device Error - Unknown signal %d", sig);
      soft_error(dpy, &win, msg);
   }

   if (sock_index != -1)
      sock_intact[sock_index] = FALSE;
   else {				/* unknown index, close 'em all */
      for (i=0; i<sock_num; i++) {
         if (sock_active[i])
            socket_close(sock_fd[i]);
         sock_active[i] = FALSE;
         sock_intact[i] = FALSE;
         XtRemoveInput(sock_input[i]);
      }
      sock_num = 0;
   }
}


x11_xerr_handler(dpy, xerror)
Display *dpy;
XErrorEvent *xerror;
{
   unsigned long time_val, cur_time;
   char buffer[240];
   int i;

   XGetErrorText(dpy, xerror->error_code, buffer, 240);
   printf("VRDI X11 Device X Error - %s (%d)\n", buffer, xerror->request_code);

   if (sock_index != -1)
      sock_intact[sock_index] = FALSE;
   else {
      for (i=0; i<sock_num; i++) {
         if (sock_active[i])
            socket_close(sock_fd[i]);
         sock_active[i] = FALSE;
         sock_intact[i] = FALSE;
         XtRemoveInput(sock_input[i]);
      }
   }
   sock_num = 0;
   XUngrabPointer(dpy, CurrentTime);
   cur_time = (unsigned long)time(0);
   time_val = cur_time - last_err_time;
   last_err_time = cur_time;
   if ((time_val / 60) > 1)
      err_cnt = 1;
   else
      err_cnt++ ;

   if (err_cnt > 10)
      exit(-1);
}


x11_xioerr_handler(dpy)
Display *dpy;
{
   printf("VRDI X11 Device Error - X IO Fatal Error\n");
   exit(-1);
}


handle_socket_request(phone_fd, s_active, s_intact)
int phone_fd;
int *s_active;
int *s_intact;
{
   int i, j, ctr;			/* loop */
   int request;				/* request value */
   int radius, area[4];
   int int_size = sizeof(int);		/* size of int */
   int size;
   int ss, sl;				/* starting sample, line */
   int imp;				/* image plane */
   int nsamps, ns, nl;			/* number samples, lines */
   int es, el;				/* ending sample, line */
   int x, y;				/* tmp x, y loc values */
   int rootx, rooty;			/* root x, y loc values */
   int block_size;			/* size of transfer */
   int lut;				/* LUT number */
   int lut_array[X_LUT_SIZE];		/* temp LUT */
   int
      red[X_LUT_SIZE],			/* red LUT */
      grn[X_LUT_SIZE],			/* green LUT */
      blu[X_LUT_SIZE];			/* blue LUT */
   int val;				/* temp value used in calcs */
   int previous;
   int cnum;				/* cursor number */
   int ctype;				/* cursor type */
   int flag;				/* flag */
   int graphics_exist;			/* Is there graphics ?  */
   int switch_no;			/* switch number */
   int prox;				/* proximal flag */
   int pen;				/* is the pen down (i.e. button down) */
   int delx, dely;			/* delta values */
   int *xvec, *yvec;			/* dynamic x,y vectors */
   int lxvec[2], lyvec[2];		/* transformed x,y lines */
   int npts;				/* number of pts in vectors */
   int mask;				/* mask imp */
   int imp_aw[4], mask_aw[4];		/* access windows */
   unsigned int state;			/* state flag */
   float fx, fy;			/* x, y loc temp */
   unsigned char *bptr, *temp;		/* pointer to temp image */
   unsigned char char_value, dn_mask;
   char msg[81];
   XWindowAttributes win_info;		/* window information */
   Window root_ret, child_ret;		/* tmp windows */

   SGO socket_recv(phone_fd, &request, int_size);
   switch(request) {

      case OPEN_DEVICE:

         /* We need the following code to override the defaults set in the    */
         /* xddopen() routine.  That routine bases its choices on the DIB     */
         /* file and isn't aware of the type of display the user is using.    */

         switch (X_Image_Depth) {
            case 24:
               ZN_IMPS = 4;
               ZOUTPUT_MODE = FULL_COLOR;
               break;
            case 8:
               ZN_IMPS = 2;
               ZOUTPUT_MODE = BLACK_AND_WHITE;
               break;
         }
         for (lut = 1, imp = 1; lut <= ZN_LUTS; lut++) {
            ZWHICH_IMP(lut) = imp;
            if (X_Image_Depth == 24)
               imp++;
         }
         break;

      case READ_AREA:
         SGO socket_recv(phone_fd, &ss, int_size);
         SGO socket_recv(phone_fd, &sl, int_size);
         SGO socket_recv(phone_fd, &ns, int_size);
         SGO socket_recv(phone_fd, &nl, int_size);
         SGO socket_recv(phone_fd, &imp, int_size);
         SGO socket_recv(phone_fd, &size, int_size);

         DPR(("x11_handler: READ_AREA, ss=%d, sl=%d, ns=%d, nl=%d, imp=%d, size=%d\n",
              ss, sl, ns, nl, imp, size));

         for (i = 0, ctr = 0; ctr < size; i++, ctr += ns) {
            i = i % nl;
            temp = VRDI_IMPP(imp, ss, i+sl);
            if (ctr + ns <= size) {
               SGO socket_send(phone_fd, temp, ns);
            } else {
               SGO socket_send(phone_fd, temp, (size-ctr));
	    }
         }
         break;

      case READ_LUT:
         SGO socket_recv(phone_fd, &lut, int_size);

         DPR(("x11_handler: READ_LUT, lut=%d\n", lut));
         for (i = 0; i < X_LUT_SIZE; i++)
            lut_array[i] = (int) VRDI_LUT(lut, i);
         SGO socket_send(phone_fd, lut_array, X_LUT_SIZE*int_size);
         break;

      case WRITE_AREA:
         SGO socket_recv(phone_fd, &sl, int_size);
         SGO socket_recv(phone_fd, &ss, int_size);
         SGO socket_recv(phone_fd, &nl, int_size);
         SGO socket_recv(phone_fd, &ns, int_size);
         SGO socket_recv(phone_fd, &imp, int_size);
         SGO socket_recv(phone_fd, &size, int_size);
         SGO socket_recv(phone_fd, &dn_mask, sizeof(unsigned char));

         DPR(("x11_handler: WRITE_AREA, ss=%d, sl=%d, ns=%d, nl=%d, imp=%d, size=%d, mask=%d\n",
               ss, sl, ns, nl, imp, size, dn_mask));

         X_WRITTEN(imp) = TRUE;
         X_Icon_Image_Exists = FALSE;

         if (dn_mask == ALL_BITS) {
            for (i = 0, ctr = 0; ctr < size; i++, ctr += ns) {
               i = i % nl;
               temp = VRDI_IMPP(imp, ss, i+sl);
               if (ctr + ns <= size) {
                  SGO socket_recv(phone_fd, temp, ns);
               } else {
                  SGO socket_recv(phone_fd, temp, (size-ctr));
	       }
            }
         }
         else {
            temp = (unsigned char *) malloc(ns);
            if (temp == 0) {
               soft_error(dpy, &win, "write_area - insufficient memory for buffer");
               *s_intact = FALSE;
               break;
            }
            for (i = 0, ctr = 0; ctr < size; i++, ctr += ns) {
               i = i % nl;
               if (ctr + ns <= size) {
                  SGO socket_recv(phone_fd, temp, ns);
                  nsamps = ns;
               }
               else {
                  SGO socket_recv(phone_fd, temp, (size-ctr));
                  nsamps = size - ctr;
               }
               bptr = VRDI_IMPP(imp, ss, i+sl);
               for (j = 0; j < nsamps; j++)
                  bptr[j] = (temp[j] & dn_mask) | (bptr[j] & ~dn_mask);
            }
            free(temp);
         }

         if (X_Batch_Mode)
            update_batch_area(ss, sl, ns+(ss-1), nl+(sl-1), imp, 0);
         else {
            compile_image(ss, sl, ns+(ss-1), nl+(sl-1), imp);
            update_display(ss, sl, ns, nl, imp);
            if (win.hist[imp-1].histogram_on && imp != X_Overlay_Imp) {
               collect_histogram(&win, imp);
               collect_str_histogram(&win, imp);
               refresh_histogram(dpy, &win, imp);
            }
         }
         break;

      case WRITE_LUT:
         SGO socket_recv(phone_fd, &lut, int_size);
         SGO socket_recv(phone_fd, lut_array, X_LUT_SIZE*int_size);

         DPR(("x11_handler: WRITE_LUT, lut=%d\n", lut));

         for (i = 0; i < X_LUT_SIZE; i++) {
            VRDI_LUT(lut, i) = (unsigned char) lut_array[i];
            if (X_Color_Locked) {
               if (X_Bit_Mode == X_7BIT_MODE)
                  char_value = (unsigned char) ((lut_array[i] >> 1) | 0x80);
               else
                  char_value = (unsigned char) lut_array[i];
               X_LUT(lut, i) = char_value;
            }
         }

         imp = X_WHICH_IMP(lut);

         if (X_Image_Depth == 8 && !X_Batch_Mode) {
            if (X_Color_Locked && check_display_mode() == PSEUDO_COLOR)
               unlock_colormap(dpy, &win);
         }

         if (!X_Color_Locked && X_Lut_RW) {
            set_colormap(dpy, X_USE_SW_LUTS);
            if (win.hist[imp-1].histogram_on) {
               collect_histogram(&win, imp);
               collect_str_histogram(&win, imp);
               refresh_histogram(dpy, &win, imp);
            }
         }
         else {
            if (X_WRITTEN(imp)) {
               if (X_Batch_Mode) {
                  update_batch_area(1, 1, ZN_SAMPS, ZN_LINES, imp, lut);
               }
               else {
                  compile_image(1, 1, ZN_SAMPS, ZN_LINES, imp);
                  update_display(1, 1, ZN_SAMPS, ZN_LINES, imp);

                  if (win.hist[imp-1].histogram_on) {
                     collect_str_histogram(&win, imp);
                     refresh_histogram(dpy, &win, imp);
                  }
               }
            }
         }
         break;

      case WRITE_OVERLAY_LUT:
         SGO socket_recv(phone_fd, red, X_Overlay_Size*int_size);
         SGO socket_recv(phone_fd, grn, X_Overlay_Size*int_size);
         SGO socket_recv(phone_fd, blu, X_Overlay_Size*int_size);

         DPR(("x11_handler: WRITE_OVERLAY_LUT\n"));
         for (i = 0; i < X_Overlay_Size; i++) {
            VRDI_OVLY_LUT(X_RED, i) = (unsigned char) red[i];
            VRDI_OVLY_LUT(X_GREEN, i) = (unsigned char) grn[i];
            VRDI_OVLY_LUT(X_BLUE, i) = (unsigned char) blu[i];
            X_OVLY_LUT(X_RED, i) = (unsigned char) red[i];
            X_OVLY_LUT(X_GREEN, i) = (unsigned char) grn[i];
            X_OVLY_LUT(X_BLUE, i) = (unsigned char) blu[i];
         }

         if (X_Image_Depth == 8)
            set_overlay_colormap(dpy);
         break;

      case READ_OVERLAY_LUT:
         DPR(("x11_handler: READ_OVERLAY_LUT\n"));
         for (lut = 1; lut <= ZN_LUTS; lut++) {
            DPR(("x11_handler: sending lut %d of size %d\n", lut, X_Overlay_Size));
            for (i = 0; i < X_Overlay_Size; i++)
               lut_array[i] = (int) VRDI_OVLY_LUT(lut, i);
            SGO socket_send(phone_fd, lut_array, X_Overlay_Size*int_size);
         }
         DPR(("x11_handler: exiting READ_OVERLAY_LUT\n"));
         break;

      case CLOSE_DEVICE:
         DPR(("x11_handler: CLOSE_DEVICE\n"));
         if (*s_active)
            socket_close(phone_fd);
         *s_active = FALSE;
         *s_intact = FALSE;
         break;

      case FREE_DEVICE:
         DPR(("x11_handler: FREE_DEVICE\n"));
         if (*s_active) {
            socket_free(phone_fd, 0);
            *s_active = FALSE;
            *s_intact = FALSE;
         }
         contin = FALSE;
         create_clientmessage_event(dpy, win.main);
         break;

      case AUTO_OFF:
      case AUTO_ON:
         SGO socket_recv(phone_fd, &cnum, int_size);
         SGO socket_recv(phone_fd, &flag, int_size);

         DPR(("x11_handler: AUTO_ON/OFF, cnum=%d, flag=%d\n", cnum, flag));
         cnum--;
         set_vcursor_autotrack(dpy, &win, cnum, flag);
         break;

      case CURSOR_OFF:
          SGO socket_recv(phone_fd, &cnum, int_size);
          DPR(("x11_handler: CURSOR_OFF, cnum=%d\n", cnum));
          cnum--;
          if (vcursor[cnum].on == TRUE)
             turn_vcursor_off(dpy, &win, cnum);
          break;

      case CURSOR_ON:
         SGO socket_recv(phone_fd, &cnum, int_size);
         SGO socket_recv(phone_fd, &ctype, int_size);
         DPR(("x11_handler: CURSOR_ON, cnum=%d, ctype=%d\n", cnum, ctype));
         cnum--;
         set_vcursor_pattern(cnum, ctype);
         turn_vcursor_on(dpy, &win, cnum);
         break;

      case FILL_AREA:
         SGO socket_recv(phone_fd, &ss, int_size);
         SGO socket_recv(phone_fd, &sl, int_size);
         SGO socket_recv(phone_fd, &ns, int_size);
         SGO socket_recv(phone_fd, &nl, int_size);
         SGO socket_recv(phone_fd, &imp, int_size);
         SGO socket_recv(phone_fd, &char_value, sizeof(char));
         SGO socket_recv(phone_fd, &dn_mask, sizeof(char));
         DPR(("x11_handler: FILL_AREA, ss=%d, sl=%d, ns=%d, nl=%d, imp=%d, value=%d, mask=%d\n",
              ss, sl, ns, nl, imp, char_value, dn_mask));

         el = sl + nl - 1;
         es = ss + ns - 1;

         X_WRITTEN(imp) = TRUE;

         if (dn_mask == ALL_BITS) {
            for (i = sl; i <= el; i++) {
               temp = VRDI_IMPP(imp, ss, i);
               for (j = 0; j < ns; j++)
                  temp[j] = char_value;
            }
         }
         else {
            for (i = sl; i <= el; i++) {
               temp = VRDI_IMPP(imp, ss, i);
               for (j = 0; j < ns; j++)
                  temp[j] = (char_value & dn_mask) | (temp[j] & ~dn_mask);
            }
         }

         if (X_Batch_Mode)
            update_batch_area(ss, sl, es, el, imp, 0);
         else {
            compile_image(ss, sl, es, el, imp);
            update_display(ss, sl, ns, nl, imp);

            if (ss == 1 && sl == 1 && es == ZN_SAMPS && el == ZN_LINES) {
               X_WRITTEN(imp) = FALSE;
               for (lut = 1; lut <= ZN_LUTS; lut++) {
                  if (VRDI_LUT(lut, char_value) != 0)
                     X_WRITTEN(imp) = TRUE;
               }
            }
         }
         break;

      case GRAPHICS_OFF:
         DPR(("x11_handler: GRAPHICS_OFF\n"));
         if (X_Overlay_On) {
            X_Overlay_On = FALSE;
            if (X_WRITTEN(X_Overlay_Imp)) {
               if (X_Batch_Mode)
                  update_batch_area(1, 1, ZN_SAMPS, ZN_LINES, X_Overlay_Imp, 0);
               else {
                  compile_image(1, 1, ZN_SAMPS, ZN_LINES, X_Overlay_Imp);
                  update_display(1, 1, ZN_SAMPS, ZN_LINES, X_Overlay_Imp);
               }
            }
         }
         break;

      case GRAPHICS_ON:
         DPR(("x11_handler: GRAPHICS_ON\n"));
         if (X_Overlay_On == FALSE) {
            X_Overlay_On = TRUE;
            if (X_WRITTEN(X_Overlay_Imp) == TRUE) {
               if (X_Batch_Mode)
                  update_batch_area(1, 1, ZN_SAMPS, ZN_LINES, X_Overlay_Imp, 0);
               else {
                  compile_image(1, 1, ZN_SAMPS, ZN_LINES, X_Overlay_Imp);
                  update_display(1, 1, ZN_SAMPS, ZN_LINES, X_Overlay_Imp);
               }
            }
         }
         break;

      case READ_CURSOR:
         SGO socket_recv(phone_fd, &cnum, int_size);
         DPR(("x11_handler: READ_CURSOR, cnum=%d\n", cnum));
         cnum--;
         read_vcursor(dpy, &win, cnum, &x, &y);
         SGO socket_send(phone_fd, &x, int_size);
         SGO socket_send(phone_fd, &y, int_size);
         break;

      case READ_SWITCH:
         SGO socket_recv(phone_fd, &switch_no, int_size);
         DPR(("x11_handler: READ_SWITCH, switch_no=%d\n", switch_no));
         state = button_status(dpy, win.main);
         switch(switch_no) {
            case 1:
               val = (state & Button1Mask) ? TRUE : FALSE;
               val |= key1_down;
               lbutton_down = TRUE;
               break;
            case 2:
               val = (state & Button2Mask) ? TRUE : FALSE;
               val |= key2_down;
               mbutton_down = TRUE;
               break;
            case 3:
               val = (state & Button3Mask) ? TRUE : FALSE;
               val |= key3_down;
               rbutton_down = TRUE;
               break;
            case 4:
               val = key4_down;
               break;
            case 5:
               val = key5_down;
               break;
            case 6:
               val = key6_down;
               break;
         }
         SGO socket_send(phone_fd, &val, int_size);
         break;

      case READ_2D:
         XQueryPointer(dpy, win.main, &root_ret, &child_ret, &rootx, &rooty,
              &ss, &sl, (unsigned int *)&mask);

         for (i = 0; i < N_VCURSORS; i++) {
            if (vcursor[i].auto_track) {
               if (X_Cursor_Mode == X_FREE_FLOATING) {
                  fx = (float) ss + 1;
                  fy = (float) sl + 1;
               }
               else {
                  fx = (float) (vcursor[i].x);
                  fy = (float) (vcursor[i].y);
               }
               break;
            }
         }
         if (ss >= 0 && ss <= win_info.width-BDR_WW)
            fx = (fx - ZVIDEO_SAMPLES/2) / ((float) ZVIDEO_SAMPLES/2);
         else if (ss < 0)
            fx = -1;
         else
            fx = 1;

         if (sl >= 0 && sl <= win_info.height-BDR_WW)
            fy = (fy - ZVIDEO_LINES/2) / ((float) ZVIDEO_LINES/2);
         else if (sl < 0)
            fy = -1;
         else
            fy = 1;

         XGetWindowAttributes(dpy, win.main, &win_info);
         prox = FALSE;
         pen  = FALSE;
         if ((ss >= 0 && ss <= win_info.width-BDR_WW) &&
             (sl >= 0 && sl <= win_info.height-BDR_WW))
            prox = TRUE;
         lbutton_down = (mask & Button1Mask) ? TRUE : FALSE;
         lbutton_down |= key1_down;
         mbutton_down = (mask & Button2Mask) ? TRUE : FALSE;
         mbutton_down |= key2_down;
         rbutton_down = (mask & Button3Mask) ? TRUE : FALSE;
         rbutton_down |= key3_down;
         pen = lbutton_down;

         SGO socket_send(phone_fd, &fx, sizeof(float));
         SGO socket_send(phone_fd, &fy, sizeof(float));
         SGO socket_send(phone_fd, &prox, int_size);
         SGO socket_send(phone_fd, &pen, int_size);
         break;

      case WRITE_CURSOR:
         SGO socket_recv(phone_fd, &cnum, int_size);
         SGO socket_recv(phone_fd, &x, int_size);
         SGO socket_recv(phone_fd, &y, int_size);
         DPR(("x11_handler: WRITE_CURSOR, cnum=%d, x=%d, y=%d\n", cnum, x, y));
         cnum--;
         move_vcursor(dpy, &win, cnum, x, y);
         break;

      case WRITE_VECTOR:
         SGO socket_recv(phone_fd, &imp, int_size);
         SGO socket_recv(phone_fd, &npts, int_size);
         SGO socket_recv(phone_fd, &char_value, sizeof(unsigned char));
         SGO socket_recv(phone_fd, &dn_mask, sizeof(unsigned char));

         DPR(("x11_handler: WRITE_VECTOR, imp=%d, npts=%d, dn=%d, mask=%d\n",
              imp, npts, char_value, dn_mask));
         xvec = (int *) malloc(npts*int_size);
         if (xvec == 0) {
            soft_error(dpy, &win, "write vector - allocation for xvec");
            *s_intact = FALSE;
            break;
         }
         SGO socket_recv(phone_fd, xvec, int_size*npts);

         yvec = (int *) malloc(npts*int_size);
         if (yvec == 0) {
            soft_error(dpy, &win, "write vector - allocation for yvec");
            *s_intact = FALSE;
            break;
         }
         SGO socket_recv(phone_fd, yvec, int_size*npts);

         display_vector(imp, npts, xvec, yvec, char_value, dn_mask,
              &ss, &sl, &es, &el);

         if (ss > es && sl > el)
            break;

         X_WRITTEN(imp) = TRUE;
         if (imp != X_Overlay_Imp || (imp == X_Overlay_Imp && X_Overlay_On)) {
            if (X_Batch_Mode)
               update_batch_area(ss, sl, es, el, imp, 0);
            else {
               compile_image(ss, sl, es, el, imp);
               ns = es - ss + 1;
               if (ns < 0) ns = 0;
               nl = el - sl + 1;
               if (nl < 0) nl = 0;
               update_display(ss, sl, ns, nl, imp);
            }
         }
         free(xvec);
         free(yvec);
         break;

      case DRAW_CIRCLE:
         DPR(("x11_handler: DRAW_CIRCLE\n"));

         SGO socket_recv(phone_fd, &imp, int_size);
         SGO socket_recv(phone_fd, &x, int_size);
         SGO socket_recv(phone_fd, &y, int_size);
         SGO socket_recv(phone_fd, &radius, int_size);
         SGO socket_recv(phone_fd, &char_value, sizeof(unsigned char));
         SGO socket_recv(phone_fd, &dn_mask, sizeof(unsigned char));
         SGO socket_recv(phone_fd, area, 4*int_size);

         DPR(("x11_handler: imp=%d, x=%d, y=%d, radius=%d, value=%d, mask=%d\n",
              imp, x, y, radius, char_value, dn_mask));
         DPR(("x11_handler: access window=%d, %d, %d, %d\n", area[0], area[1],
              area[2], area[3]));

         display_circle(imp, x, y, radius, char_value, dn_mask, area,
              &ss, &sl, &es, &el);
         if (ss > es && sl > el)
            break;

         X_WRITTEN(imp) = TRUE;
         if (imp != X_Overlay_Imp || (imp == X_Overlay_Imp && X_Overlay_On)) {
            if (X_Batch_Mode)
               update_batch_area(ss, sl, es, el, imp, 0);
            else {
               compile_image(ss, sl, es, el, imp);
               ns = es - ss + 1;
               nl = el - sl + 1;
               update_display(ss, sl, ns, nl, imp);
            }
         }
         break;

      case ZOOM_IMP:
         SGO socket_recv(phone_fd, &val, int_size);
         SGO socket_recv(phone_fd, &imp, int_size);

         DPR(("x11_handler: ZOOM_IMP, zoom=%d, imp=%d\n",val, imp));
         if (val != X_ZOOM(imp)) {
            if (val < 1)
               X_ZOOM(imp) = 1;
            else if (val > ZMAX_ZOOM_FACTOR)
               X_ZOOM(imp) = ZMAX_ZOOM_FACTOR;
            else
               X_ZOOM(imp) = val;

            if (X_WRITTEN(imp)) {
               if (X_Batch_Mode)
                  update_batch_area(1, 1, ZN_SAMPS, ZN_LINES, imp, 0);
               else {
                  compile_image(1, 1, ZN_SAMPS, ZN_LINES, imp);
                  update_display(1, 1, ZN_SAMPS, ZN_LINES, imp);
               }
            }
         }
         break;

      case SET_DW:
         SGO socket_recv(phone_fd, &imp, int_size);
         SGO socket_recv(phone_fd, &x, int_size);
         SGO socket_recv(phone_fd, &y, int_size);
         DPR(("x11_handler: SET_DW, imp=%d, x=%d, y=%d\n", imp, x, y));

         if (x != X_DW_LEFT(imp) || y != X_DW_TOP(imp)) {
            X_DW_LEFT(imp) = x;
            X_DW_TOP(imp) = y;

            if (X_WRITTEN(imp)) {
               if (X_Batch_Mode)
                  update_batch_area(1, 1, ZN_SAMPS, ZN_LINES, imp, 0);
               else {
                  compile_image(1, 1, ZN_SAMPS, ZN_LINES, imp);
                  update_display(1, 1, ZN_SAMPS, ZN_LINES, imp);
               }
            }
         }
         break;

      case CONNECT_IMPS_LUTS:
         SGO socket_recv(phone_fd, &imp, int_size);
         SGO socket_recv(phone_fd, &lut, int_size);
         DPR(("x11_handler: CONNECT_IMPS_LUTS, imp=%d, lut=%d\n", imp, lut));

         if (X_WHICH_IMP(lut) != imp) {
            previous = X_WHICH_IMP(lut);
            DPR(("previous = %d\n", previous));
            X_WHICH_IMP(lut) = imp;

            if (X_WRITTEN(previous) || X_WRITTEN(imp)) {
               if (X_Batch_Mode)
                  update_batch_area(1, 1, ZN_SAMPS, ZN_LINES, imp, lut);
               else {
                  compile_image(1, 1, ZN_SAMPS, ZN_LINES, imp);
                  update_display(1, 1, ZN_SAMPS, ZN_LINES, imp);
               }
            }
         }
         break;

      case CONNECT_OVERLAY:
         SGO socket_recv(phone_fd, &imp, int_size);
         break;

      case COLLECT_HISTOGRAM:
         SGO socket_recv(phone_fd, &imp, int_size);
         SGO socket_recv(phone_fd, &mask, int_size);
         SGO socket_recv(phone_fd, imp_aw, 4*int_size);
         SGO socket_recv(phone_fd, mask_aw, 4*int_size);
         collect_histogram(&win, imp);
         SGO socket_send(phone_fd, win.hist[imp-1].histogram,
              int_size*X_LUT_SIZE);
         break;

      case OPEN_CLOSE_WIN:
         SGO socket_recv(phone_fd, &flag, int_size);
         /* Do nothing */
         break;

      case MOVE_WIN:
         SGO socket_recv(phone_fd, &x, int_size);
         SGO socket_recv(phone_fd, &y, int_size);
	 DPR(("x11_handler: MOVE_WIN, x=%d, y=%d\n", x, y));
	 XMoveWindow(dpy, win.main, x, y);
         break;

      case RESIZE_WIN:
         SGO socket_recv(phone_fd, &x, int_size);
         SGO socket_recv(phone_fd, &y, int_size);
	 DPR(("x11_handler: RESIZE_WIN, x=%d, y=%d\n", x, y));
	 XResizeWindow(dpy, win.main, x, y);
         break;

      case SET_BATCH_MODE:
         SGO socket_recv(phone_fd, &flag, int_size);

         if (!flag)
            process_batch();
         else
            DPR(("x11_handler: batch mode is now on\n"));
         X_Batch_Mode = flag;
         break;

      default:
         *s_intact = FALSE;		/* closed down below */
   } /* matches switch(request) */

   if (*s_active && !(*s_intact)) {
      /* socket is no longer intact -- close it */

      sprintf(msg, "SOCKET FAILED: no longer intact, request %d err %d",
           request, errno);
      soft_error(dpy, &win, msg);
      *s_active = FALSE;
      *s_intact = FALSE;
      socket_close(phone_fd);
   }
}


update_batch_area(ss, sl, es, el, imp, lut)
int ss, sl, es, el, imp, lut;
{
   int i;

   DPR(("x11_handler: updating batch area--ss=%d, sl=%d, es=%d, el=%d, imp=%d, lut=%d\n",
        ss, sl, es, el, imp, lut));

   if (imp == X_Overlay_Imp || imp == X_ALL_IMPS) {
      for (i = 1; i < ZN_IMPS; i++)
         batch_area[0] |= (1 << i);
   }
   else
      batch_area[0] |= (1 << imp);

   if (ss < batch_area[1])
      batch_area[1] = ss;
   if (sl < batch_area[2])
      batch_area[2] = sl;
   if (es > batch_area[3])
      batch_area[3] = es;
   if (el > batch_area[4])
      batch_area[4] = el;

   if (batch_area[1] == 1 && batch_area[2] == 1 &&
        (batch_area[3] > ZN_SAMPS || batch_area[4] > ZN_LINES)) {
      batch_area[3] = ZN_SAMPS;
      batch_area[4] = ZN_LINES;
   }
   batch_area[5] = lut;
}


process_batch()
{
   int imp, nl, ns;

   if (X_Image_Depth == 8) {
      if (X_Color_Locked && batch_area[5] != 0) {
         if (check_display_mode() == PSEUDO_COLOR)
            unlock_colormap(dpy, &win);
      }
   }

   DPR(("x11_handler: batch mode is now off--updating...\n"));
   if (batch_area[0] != 0) {
      ns = batch_area[3] - batch_area[1] + 1;
      nl = batch_area[4] - batch_area[2] + 1;
      for (imp = 1; imp < ZN_IMPS; imp++) {
         if (batch_area[0] & 1 << imp) {
            compile_image(batch_area[1], batch_area[2], batch_area[3], batch_area[4], imp);
            update_display(batch_area[1], batch_area[2], ns, nl, imp);
            if (win.hist[imp-1].histogram_on) {
               collect_histogram(&win, imp);
               collect_str_histogram(&win, imp);
               refresh_histogram(dpy, &win, imp);
            }
         }
      }
      batch_area[0] = 0;
      batch_area[1] = ZN_SAMPS+1;
      batch_area[2] = ZN_LINES+1;
      batch_area[3] = 0;
      batch_area[4] = 0;
      batch_area[5] = 0;
   }
}


update_display(ss, sl, ns, nl, imp)
int ss, sl, ns, nl, imp;
{
   int impss, impsl, impel, impes, vidss, vidsl, vidnl, vidns, adj_ns, adj_nl;

   if (ss <= 1 && sl <= 1 && ns >= ZN_SAMPS && nl >= ZN_LINES) {
      DPR(("x11_handler: updating entire display window\n"));
      reconf_imp(dpy, &win, ss, sl, ns, nl);
      return;
   }

   impss = ss;
   impsl = sl;
   impes = ns + ss - 1;
   impel = nl + sl - 1;

   DPR(("x11_handler: impss=%d, impsl=%d, impes=%d, impel=%d, ns=%d, nl=%d\n",
        impss, impsl, impes, impel, ns, nl));
   imp2raw(imp, &impss, &impsl, impes, impel, &vidss, &vidsl, &vidns, &vidnl);

   DPR(("x11_handler: vidss=%d, vidsl=%d, vidns=%d, vidnl=%d\n",
        vidss, vidsl, vidns, vidnl));

   if (((vidss + vidns) > ZN_SAMPS) && ((vidsl + vidnl) > ZN_LINES)) {
      adj_nl = ZN_LINES - vidsl + 1;
      adj_ns = ZN_SAMPS - vidss + 1;
      DPR(("x11_handler: writing to all four corners, adj_nl=%d, adj_ns=%d\n",
           adj_nl, adj_ns));
      reconf_imp(dpy, &win, vidss, vidsl, adj_ns, adj_nl);
      reconf_imp(dpy, &win, 1, 1, (ns-adj_ns), (nl-adj_nl));
      reconf_imp(dpy, &win, 1, vidsl, (ns-adj_ns), adj_nl);
      reconf_imp(dpy, &win, vidss, 1, adj_ns, (nl-adj_nl));
   }
   else if ((vidss + vidns) > ZN_SAMPS) {
      adj_ns = ZN_SAMPS - vidss + 1;
      DPR(("x11_handler: crossing the x axis, adj_ns=%d\n", adj_ns));
      reconf_imp(dpy, &win, vidss, vidsl, adj_ns, nl);
      reconf_imp(dpy, &win, 1, vidsl, (ns-adj_ns), nl);
   }
   else if ((vidsl + vidnl) > ZN_LINES) {
      adj_nl = ZN_LINES - vidsl + 1;
      DPR(("x11_handler: crossing the y axis, adj_nl=%d\n", adj_nl));
      reconf_imp(dpy, &win, vidss, vidsl, ns, adj_nl);
      reconf_imp(dpy, &win, vidss, 1, ns, (nl-adj_nl));
   }
   else {
      DPR(("x11_handler: entire write area inside boundaries\n"));
      reconf_imp(dpy, &win, vidss, vidsl, vidns, vidnl);
   }
}


display_vector(imp, npts, xvec, yvec, value, mask, ss, sl, es, el)
int imp, npts, xvec[], yvec[];
unsigned char value, mask;
int *ss, *sl, *es, *el;
{
   int i, j, delx, dely, x, y, x1, y1;
   float slope;
   unsigned char *temp;

   DPR(("x11_handler: DISPLAY_VECTOR, imp=%d, npts=%d, dn=%d, mask=%d\n",
        imp, npts, value, mask));
   *ss = ZN_SAMPS + 1;
   *sl = ZN_LINES + 1;
   *es = -1;
   *el = -1;

   for (i = 0; i < npts; i++) {
      xvec[i] = MIN(MAX(xvec[i], 1), ZN_SAMPS);
      yvec[i] = MIN(MAX(yvec[i], 1), ZN_LINES);
   }

   for (i = 0; i < (npts-1); i++) {
      delx = xvec[i+1] - xvec[i];
      dely = yvec[i+1] - yvec[i];

      if (abs(delx) > abs(dely)) {
         if (delx < 0) {
            x1 = xvec[i+1];
            y1 = yvec[i+1];
            delx = -delx;
            dely = -dely;
         }
         else {
            x1 = xvec[i];
            y1 = yvec[i];
         }
      }
      else {
         if (dely < 0) {
            x1 = xvec[i+1];
            y1 = yvec[i+1];
            delx = -delx;
            dely = -dely;
         }
         else {
            x1 = xvec[i];
            y1 = yvec[i];
         }
      }

      if (delx == 0 && dely == 0) {
         temp = VRDI_IMPP(imp, xvec[i], yvec[i]);
         *temp = (value & mask) | (*temp & ~mask);
      }
      else {
         if (abs(delx) > abs(dely)) {
            slope = (float) (dely) / (float) (delx);
            for (j = 0; j <= delx; j++) {
               y = y1 + j*slope;
               x = x1 + j;
               temp = VRDI_IMPP(imp, x, y);
               *temp = (value & mask) | (*temp & ~mask);
            }
         }
         else {
            slope = (float) (delx) / (float) (dely);
            for (j = 0; j <= dely; j++) {
               x = x1 + j*slope;
               y = y1 + j;
               temp = VRDI_IMPP(imp, x, y);
               *temp = (value & mask) | (*temp & ~mask);
            }
         }
      }
      *ss = MIN(MIN(xvec[i], xvec[i+1]), *ss);
      *es = MAX(MAX(xvec[i], xvec[i+1]), *es);
      *sl = MIN(MIN(yvec[i], yvec[i+1]), *sl);
      *el = MAX(MAX(yvec[i], yvec[i+1]), *el);
   }

   *ss = MAX(1, (*ss) - 1);
   *es = MIN(ZN_SAMPS, (*es) + 1);
   *sl = MAX(1, (*sl) - 1);
   *el = MIN(ZN_LINES, (*el) + 1);
}


display_circle(imp, xcenter, ycenter, radius, color, mask, area, ss, sl, es, el)
int imp, xcenter, ycenter, radius;
unsigned char color, mask;
int area[4], *ss, *sl, *es, *el;
{
   int  status, x, y, xpos, ypos, d;
   int  xmin, xmax, ymin, ymax;
   unsigned char *temp;

   xmin = ZN_SAMPS + 1;
   xmax = 0;
   ymin = ZN_LINES + 1;
   ymax = 0;
   x = 0;
   y = radius;
   d = 3 - (2 * radius);

   while (x <= y) {
      xpos = xcenter + x;
      ypos = ycenter + y;
      if ((xpos >= area[X_LEFT]) && (xpos <= area[X_RIGHT]) &&
           (ypos >= area[X_TOP])  && (ypos <= area[X_BOTTOM])) {
         temp = VRDI_IMPP(imp, xpos, ypos);
         *temp = (color & mask) | (*temp & ~mask);
         check_min_max(xpos, ypos, &xmin, &xmax, &ymin, &ymax);
      }

      xpos = xcenter + x;
      ypos = ycenter - y;
      if ((xpos >= area[X_LEFT]) && (xpos <= area[X_RIGHT]) &&
           (ypos >= area[X_TOP])  && (ypos <= area[X_BOTTOM])) {
         temp = VRDI_IMPP(imp, xpos, ypos);
         *temp = (color & mask) | (*temp & ~mask);
         check_min_max(xpos, ypos, &xmin, &xmax, &ymin, &ymax);
      }

      xpos = xcenter - x;
      ypos = ycenter + y;
      if ((xpos >= area[X_LEFT]) && (xpos <= area[X_RIGHT]) &&
           (ypos >= area[X_TOP])  && (ypos <= area[X_BOTTOM])) {
         temp = VRDI_IMPP(imp, xpos, ypos);
         *temp = (color & mask) | (*temp & ~mask);
         check_min_max(xpos, ypos, &xmin, &xmax, &ymin, &ymax);
      }

      xpos = xcenter - x;
      ypos = ycenter - y;
      if ((xpos >= area[X_LEFT]) && (xpos <= area[X_RIGHT]) &&
           (ypos >= area[X_TOP])  && (ypos <= area[X_BOTTOM])) {
         temp = VRDI_IMPP(imp, xpos, ypos);
         *temp = (color & mask) | (*temp & ~mask);
         check_min_max(xpos, ypos, &xmin, &xmax, &ymin, &ymax);
      }

      xpos = xcenter + y;
      ypos = ycenter + x;
      if ((xpos >= area[X_LEFT]) && (xpos <= area[X_RIGHT]) &&
           (ypos >= area[X_TOP])  && (ypos <= area[X_BOTTOM])) {
         temp = VRDI_IMPP(imp, xpos, ypos);
         *temp = (color & mask) | (*temp & ~mask);
         check_min_max(xpos, ypos, &xmin, &xmax, &ymin, &ymax);
      }

      xpos = xcenter + y;
      ypos = ycenter - x;
      if ((xpos >= area[X_LEFT]) && (xpos <= area[X_RIGHT]) &&
           (ypos >= area[X_TOP])  && (ypos <= area[X_BOTTOM])) {
         temp = VRDI_IMPP(imp, xpos, ypos);
         *temp = (color & mask) | (*temp & ~mask);
         check_min_max(xpos, ypos, &xmin, &xmax, &ymin, &ymax);
      }

      xpos = xcenter - y;
      ypos = ycenter + x;
      if ((xpos >= area[X_LEFT]) && (xpos <= area[X_RIGHT]) &&
           (ypos >= area[X_TOP])  && (ypos <= area[X_BOTTOM])) {
         temp = VRDI_IMPP(imp, xpos, ypos);
         *temp = (color & mask) | (*temp & ~mask);
         check_min_max(xpos, ypos, &xmin, &xmax, &ymin, &ymax);
      }

      xpos = xcenter - y;
      ypos = ycenter - x;
      if ((xpos >= area[X_LEFT]) && (xpos <= area[X_RIGHT]) &&
           (ypos >= area[X_TOP])  && (ypos <= area[X_BOTTOM])) {
         temp = VRDI_IMPP(imp, xpos, ypos);
         *temp = (color & mask) | (*temp & ~mask);
         check_min_max(xpos, ypos, &xmin, &xmax, &ymin, &ymax);
      }

      if (d < 0)
         d = d + (4 * x) + 6;
      else {
         d = d + (4 * (x - y)) + 10;
         y -= 1;
      }
      x += 1;
   }
   *ss = xmin;
   *sl = ymin;
   *es = xmax;
   *el = ymax;
}


check_min_max(xpos, ypos, xmin, xmax, ymin, ymax)
int xpos, ypos, *xmin, *xmax, *ymin, *ymax;
{
   if (xpos < *xmin)  *xmin = xpos;
   if (xpos > *xmax)  *xmax = xpos;
   if (ypos < *ymin)  *ymin = ypos;
   if (ypos > *ymax)  *ymax = ypos;
}


process_keyboard_input(key, event)
KeySym key;
XEvent *event;
{
   int cnum;

   if (event->type == KeyPress) {
      switch (key) {
         case XK_a:
         case XK_A:
         case XK_Return:
         case XK_1:
            key1_down = TRUE;
            DPR(("x11_handler: key 1 down set TRUE\n"));
            break;
         case XK_c:
         case XK_C:
         case XK_2:
            key2_down = TRUE;
            DPR(("x11_handler: key 2 down set TRUE\n"));
            break;
         case XK_t:
         case XK_T:
         case XK_r:
         case XK_R:
         case XK_3:
            key3_down = TRUE;
            DPR(("x11_handler: key 3 down set TRUE\n"));
            break;
         case XK_4:
            key4_down = TRUE;
            DPR(("x11_handler: key 4 down set TRUE\n"));
            break;
         case XK_5:
            key5_down = TRUE;
            DPR(("x11_handler: key 5 down set TRUE\n"));
            break;
         case XK_6:
            key6_down = TRUE;
            DPR(("x11_handler: key 6 down set TRUE\n"));
            break;
         case XK_Escape:
         case XK_quoteleft:
            if (X_Cursor_Mode == X_FREE_FLOATING) {
               X_Cursor_Mode = X_PLANTED;
               XDefineCursor(dpy, win.image, main_cursor);
               if ((cnum = find_active_cursor()) != -1) {
                  set_vcursor_loc(dpy, &win, cnum, event->xkey.x+1, event->xkey.y+1);
                  turn_vcursor_on(dpy, &win, cnum);
               }
            }
            else {
               if ((cnum = find_active_cursor()) != -1) {
                  set_floating_cursor(dpy, &win, cnum);
                  turn_vcursor_off(dpy, &win, cnum);
                  vcursor[cnum].on = TRUE;
                  X_Cursor_Mode = X_FREE_FLOATING;
               }
               else {
                  soft_error(dpy, &win, "No active cursor.");
               }
            }
            DPR(("x11_handler: cursor mode=%d\n", X_Cursor_Mode));
            break;
      }
   }
   else {
      switch (key) {
         case XK_a:
         case XK_A:
         case XK_Return:
         case XK_1:
            key1_down = FALSE;
            DPR(("x11_handler: key 1 down set FALSE\n"));
            break;
         case XK_c:
         case XK_C:
         case XK_2:
            key2_down = FALSE;
            DPR(("x11_handler: key 2 down set FALSE\n"));
            break;
         case XK_t:
         case XK_T:
         case XK_r:
         case XK_R:
         case XK_3:
            key3_down = FALSE;
            DPR(("x11_handler: key 3 down set FALSE\n"));
            break;
         case XK_4:
            key4_down = FALSE;
            DPR(("x11_handler: key 4 down set FALSE\n"));
            break;
         case XK_5:
            key5_down = FALSE;
            DPR(("x11_handler: key 5 down set FALSE\n"));
            break;
         case XK_6:
            key6_down = FALSE;
            DPR(("x11_handler: key 6 down set FALSE\n"));
            break;
      }
   }
}


void process_socket_connect()
{
   void process_socket_request();
   struct sockaddr head;
   int temp_fd;

   DPR(("x11_handler: processing new socket request\n"));
   sock_fd[sock_num] = socket_accept(window_fd, &head, 0, FALSE);
   if (sock_fd[sock_num] >= 0) {
      sock_active[sock_num] = TRUE;
      sock_intact[sock_num] = TRUE;
      DPR(("x11_handler: adding socket number %d to event loop\n", sock_num));
#if VMS_OS
      temp_fd = sock_efn(sock_fd[sock_num]);
      sock_input[sock_num] = XtAppAddInput(app_context, temp_fd,
           NULL, process_socket_request, (XtPointer) sock_num);
#else
      temp_fd = sock_fd[sock_num];
      sock_input[sock_num] = XtAppAddInput(app_context, temp_fd,
           (XtPointer)XtInputReadMask, process_socket_request,
	   (XtPointer) sock_num);
#endif
      sock_num++;
   }
#if VMS_OS
   temp_fd = sock_efn(window_fd);		/* reset event flag delivery */
#endif
}


void process_socket_request(sock_index)
int sock_index;
{
   int i;
   int temp_fd;

   DPR(("x11_handler: processing socket request from socket %d\n", sock_index));
   handle_socket_request(sock_fd[sock_index], &sock_active[sock_index],
        &sock_intact[sock_index]);
   if (!sock_active[sock_index] || !sock_intact[sock_index]) {	/* it died */
      XtRemoveInput(sock_input[sock_index]);
      for (i=sock_index; i<sock_num-1; i++) {	/* shift others down */
         sock_fd[i] = sock_fd[i+1];
         sock_active[i] = sock_active[i+1];
         sock_intact[i] = sock_intact[i+1];
         sock_input[i] = sock_input[i+1];
      }
      sock_num--;
   }
#if VMS_OS
   temp_fd = sock_efn(sock_fd[sock_index]);	/* reset event flag delivery */
#endif
}
