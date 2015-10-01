/* SCCS %W% %G%  %Q% */
/******************************************************************************/
/*                                                                            */
/*  File: x11_cursor.c                                                        */
/*                                                                            */
/*  Routines:                                                                 */
/*                                                                            */
/*	set_vcursor_autotrack(dpy, win, cnum, flag)                           */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		int cnum, flag;                                               */
/*                                                                            */
/*	set_vcursor_loc(dpy, win, cnum, x, y)                                 */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		int cnum, x, y;                                               */
/*                                                                            */
/*	move_vcursor(dpy, win, cnum, x, y)                                    */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		int cnum, x, y;                                               */
/*                                                                            */
/*	set_vcursor_pattern(cnum, ctype)                                      */
/*		int cnum, ctype;                                              */
/*                                                                            */
/*	turn_vcursor_on(dpy, win, cnum)                                       */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		int cnum;                                                     */
/*                                                                            */
/*	turn_vcursor_off(dpy, win, cnum)                                      */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		int cnum;                                                     */
/*                                                                            */
/*	read_vcursor(dpy, win, cnum, x, y)                                    */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		int cnum, *x, *y;                                             */
/*                                                                            */
/*	cursor_select(dpy, win, sock_num, sock_fd)                            */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		int sock_num, *sock_fd;                                       */
/*                                                                            */
/*	init_floating_cursor(dpy, win)                                        */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*                                                                            */
/*	find_active_cursor()                                                  */
/*                                                                            */
/*	set_floating_cursor(dpy, win, cnum)                                   */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		int cnum;                                                     */
/*                                                                            */
/*  Written by:			Mark Mann                                     */
/*  Date:			???                                           */
/*                                                                            */
/*  Cognizant Programmer:	Paul Bartholomew                              */
/*  Revision History:                                                         */
/*    Date    FR #   Description                                              */
/*  --------  -----  -------------------------------------------------------  */
/*  08-01-92   ---   PDB-Complete rewrite.                                    */
/*                                                                            */
/******************************************************************************/

#include "xvmaininc.h"
#include "xdexterns.h"
#include "x11_device_main.h"
#include "x11_bitmaps.h"
#if VMS_OS && ALPHA_ARCH
#undef MIN              /* redefined by Intrinsic.h */
#undef MAX
#endif
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

/* #define DEBUG */
#ifdef DEBUG
#define DPR(A)  {printf A; fflush(stdout);}
#else
#define DPR(A)
#endif

#define HCSZ 15
#define MCSZ 31
#define CSZ  32

extern int unit;
extern XtAppContext app_context;
extern VRDIcursor vcursor[];
extern Cursor main_cursor;
extern Cursor ud_cursor;
extern Cursor lr_cursor;
extern Cursor up_cursor;
extern Cursor left_cursor;
extern Cursor ub_cursor;
extern Cursor db_cursor;
extern Cursor lb_cursor;
extern Cursor rb_cursor;
extern Cursor v1_cursor;
extern Cursor v2_cursor;
extern Cursor v3_cursor;
extern Cursor v4_cursor;
extern Cursor v5_cursor;
extern Cursor v6_cursor;
extern Cursor watch_cursor;
extern Cursor empty_cursor;


set_vcursor_autotrack(dpy, win, cnum, flag)
Display *dpy;
MAIN_WINDOW *win;
int cnum, flag;
{
   Window root_ret, child_ret;
   int root_x, root_y, x, y, i;
   unsigned int state;

   DPR(("x11_cursor: turn autotracking for %d to %d\n", cnum, flag));

   if (flag == vcursor[cnum].auto_track)
      return;

   /* If we're turning autotracking on for a cursor, we turn it off to the */
   /* others because we can only track one at a time.                      */

   if (flag == TRUE) {
      for (i = 0; i < N_VCURSORS; i++) {
         if (vcursor[i].auto_track && i != cnum) {
            vcursor[i].auto_track = FALSE;
            if (X_Cursor_Mode == X_FREE_FLOATING) {
               XQueryPointer(dpy, win->image, &root_ret, &child_ret, &root_x,
                    &root_y, &x, &y, &state);
               x++;
               y++;
               if (x > ZN_SAMPS) x = ZN_SAMPS;
               if (x < 1) x = 1;
               if (y > ZN_LINES) y = ZN_LINES;
               if (y < 1) y = 1;
               set_vcursor_loc(dpy, win, i, x, y);
            }
            turn_vcursor_on(dpy, win, i);
         }
      }
   }
   else {
      if (X_Cursor_Mode == X_FREE_FLOATING && vcursor[cnum].on &&
           vcursor[cnum].auto_track) {
         X_Cursor_Mode = X_PLANTED;
         XDefineCursor(dpy, win->image, main_cursor);
         turn_vcursor_on(dpy, win, cnum);
      }
   }

   if (X_Cursor_Mode == X_FREE_FLOATING && flag && vcursor[cnum].on) {
      if (!vcursor[cnum].auto_track) {
         set_floating_cursor(dpy, win, cnum);
         turn_vcursor_off(dpy, win, cnum);
         vcursor[cnum].on = TRUE;
      }
      XWarpPointer(dpy, None, win->image, 0, 0, 0, 0, vcursor[cnum].x-1,
           vcursor[cnum].y-1);
   }
   vcursor[cnum].auto_track = flag;
}


set_vcursor_loc(dpy, win, cnum, x, y)
Display *dpy;
MAIN_WINDOW *win;
int cnum, x, y;
{
   int oldx, oldy;

   DPR(("x11_cursor: set vcursor %d  loc %3dx%-3d\n", cnum, x, y));

   oldx = vcursor[cnum].x;
   oldy = vcursor[cnum].y;

   vcursor[cnum].x = x;
   vcursor[cnum].y = y;

   if (x-HCSZ >= 1)
      vcursor[cnum].x0 = x - HCSZ;
   else
      vcursor[cnum].x0 = ZN_SAMPS + (x-HCSZ);

   if (x+HCSZ <= ZN_SAMPS)
      vcursor[cnum].x1 = x + HCSZ;
   else
      vcursor[cnum].x1 = (x + HCSZ) - ZN_SAMPS;

   if (y-HCSZ >= 1)
      vcursor[cnum].y0 = y - HCSZ;
   else
      vcursor[cnum].y0 = ZN_LINES + (y - HCSZ);

   if (y+HCSZ <= ZN_LINES)
      vcursor[cnum].y1 = y+HCSZ;
   else
      vcursor[cnum].y1 = (y+HCSZ) - ZN_LINES;

   if (X_Cursor_Mode == X_FREE_FLOATING && vcursor[cnum].auto_track)
      XWarpPointer(dpy, None, win->image, 0, 0, 0, 0, x-1, y-1);

   DPR(("x11_cursor: leave set vcursor loc %3dx%-3d %3dx%-3d %3dx%-3d\n",
        vcursor[cnum].x, vcursor[cnum].y, vcursor[cnum].x0, vcursor[cnum].y0,
        vcursor[cnum].x1, vcursor[cnum].y1));
}


move_vcursor(dpy, win, cnum, x, y)
Display *dpy;
MAIN_WINDOW *win;
int cnum, x, y;
{
   DPR(("x11_cursor: move vcursor to %d %d\n", x, y));

   if (vcursor[cnum].on) {
      if (X_Cursor_Mode == X_PLANTED || !vcursor[cnum].auto_track) {
         if (vcursor[cnum].x != x || vcursor[cnum].y != y) {
            turn_vcursor_off(dpy, win, cnum);
            set_vcursor_loc(dpy, win, cnum, x, y);
            turn_vcursor_on(dpy, win, cnum);
         }
      }
      else
         set_vcursor_loc(dpy, win, cnum, x, y);
   }
   else {
      if (vcursor[cnum].x != x || vcursor[cnum].y != y)
         set_vcursor_loc(dpy, win, cnum, x, y);
   }

   DPR(("x11_cursor: leave move cursor\n"));
}


set_vcursor_pattern(cnum, ctype)
int cnum, ctype;
{
   int i, j;
   short int bits[64];

   DPR(("x11_cursor: set cursor %d pattern to %d\n", cnum, ctype));

   if (ctype < 1)
      ctype = 1;
   else if (ctype > 6)
      ctype = 6;

   vcursor[cnum].type = ctype;

   for (i = 0; i < 64; i++) {
      switch (vcursor[cnum].type) {
         case 1:
            bits[i] = (short int) soft_v1_bits[i];
            break;
         case 2:
            bits[i] = (short int) soft_v2_bits[i];
            break;
         case 3:
            bits[i] = (short int) soft_v3_bits[i];
            break;
         case 4:
            bits[i] = (short int) soft_v4_bits[i];
            break;
         case 5:
            bits[i] = (short int) soft_v5_bits[i];
            break;
         case 6:
            bits[i] = (short int) soft_v6_bits[i];
            break;
      }
   }

   DPR(("x11_cursor: in load pattern\n"));
   for (i = 0; i < CSZ; i++) {
      for(j = 0; j < CSZ; j++) {
         vcursor[cnum].pattern[i][j] = (unsigned char)
             ((bits[(i*2)+(int)(j/16)] & (1 << ((j+16)%16))) ? 0 : 255);
      }
   }
   DPR(("x11_cursor: leave set cursor pattern\n"));
}


turn_vcursor_on(dpy, win, cnum)
Display *dpy;
MAIN_WINDOW *win;
int cnum;
{
   XWindowAttributes info;
   short origin_changed = FALSE;

   DPR(("x11_cursor: turn cursor on at %3dx%-3d %3dx%-3d %3dx%-3d\n",
        vcursor[cnum].x, vcursor[cnum].y, vcursor[cnum].x0, vcursor[cnum].y0,
        vcursor[cnum].x1, vcursor[cnum].y1));

   if (X_Cursor_Mode == X_FREE_FLOATING && vcursor[cnum].auto_track) {
      set_floating_cursor(dpy, win, cnum);
      vcursor[cnum].on = TRUE;
      return;
   }
   XGetWindowAttributes(dpy, win->image, &info);

   if (vcursor[cnum].on)
      turn_vcursor_off(dpy, win, cnum);

   if (vcursor[cnum].x > X_Display_Orig_X + info.width) {
      DPR(("x11_cursor: change1 x orig from %d ", X_Display_Orig_X));
      X_Display_Orig_X = MIN((vcursor[cnum].x - (info.width/2)),
           (ZN_SAMPS - info.width));
      DPR(("to %d\n", X_Display_Orig_X));
      origin_changed = TRUE;
   }
   else if (vcursor[cnum].x < X_Display_Orig_X) {
      DPR(("x11_cursor: change2 x orig from %d ", X_Display_Orig_X));
      X_Display_Orig_X = MAX((vcursor[cnum].x - (info.width/2)), 0);
      DPR(("to %d\n", X_Display_Orig_X));
      origin_changed = TRUE;
   }

   if (vcursor[cnum].y > X_Display_Orig_Y + info.height) {
      DPR(("x11_cursor: change1 y orig from %d ", X_Display_Orig_Y));
      X_Display_Orig_Y = MIN((vcursor[cnum].y - (info.height/2)),
           (ZN_LINES - info.height));
      DPR(("to %d\n", X_Display_Orig_Y));
      origin_changed = TRUE;
   }
   else if (vcursor[cnum].y < X_Display_Orig_Y) {
      DPR(("x11_cursor: change2 y orig from %d ", X_Display_Orig_Y));
      X_Display_Orig_Y = MAX((vcursor[cnum].y - (info.height/2)), 0);
      DPR(("to %d\n", X_Display_Orig_Y));
      origin_changed = TRUE;
   }

   if (origin_changed) {
      DPR(("x11_cursor: origin changed\n"));
      refresh_windows(dpy, win);
      DPR(("x11_cursor: done with refresh\n"));
      turn_vcursor_on(dpy, win, cnum);
      DPR(("x11_cursor: ...turn vcursor on\n"));
   }
   else {
      vcursor[cnum].on = TRUE;
      if ((vcursor[cnum].x0+MCSZ > ZN_SAMPS) &&
          (vcursor[cnum].y0+MCSZ > ZN_LINES)) {
         DPR(("x11_cursor: turn cursor on to all four corners\n"));
         reconf_imp(dpy, win, vcursor[cnum].x0, vcursor[cnum].y0, MCSZ, MCSZ);
         reconf_imp(dpy, win, 1, vcursor[cnum].y0, MCSZ, MCSZ);
         reconf_imp(dpy, win, vcursor[cnum].x0, 1, MCSZ, MCSZ);
         reconf_imp(dpy, win, 1, 1, MCSZ, MCSZ);
      }
      else if (vcursor[cnum].x0+MCSZ > ZN_SAMPS) {
         DPR(("x11_cursor: turn cursor on, cross y axis\n"));
         reconf_imp(dpy, win, vcursor[cnum].x0, vcursor[cnum].y0, MCSZ, MCSZ);
         reconf_imp(dpy, win, 1, vcursor[cnum].y0, MCSZ, MCSZ);
      }
      else if (vcursor[cnum].y0+MCSZ > ZN_LINES) {
         DPR(("x11_cursor: turn cursor on, cross x axis\n"));
         reconf_imp(dpy, win, vcursor[cnum].x0, vcursor[cnum].y0, MCSZ, MCSZ);
         reconf_imp(dpy, win, vcursor[cnum].x0, 1, MCSZ, MCSZ);
      }
      else {
         DPR(("x11_cursor: turn cursor on, whole on image\n"));
         reconf_imp(dpy, win, vcursor[cnum].x0, vcursor[cnum].y0, MCSZ, MCSZ);
      }
   }
   DPR(("x11_cursor: leave turn cursor on\n"));
}


turn_vcursor_off(dpy, win, cnum)
Display *dpy;
MAIN_WINDOW *win;
int cnum;
{
   int i, xlo, xhi, ylo, yhi;

   DPR(("x11_cursor: turn cursor %d off\n", cnum));

   if (X_Cursor_Mode == X_FREE_FLOATING && vcursor[cnum].on &&
        vcursor[cnum].auto_track) {
      XDefineCursor(dpy, win->image, main_cursor);
      vcursor[cnum].on = FALSE;
      return;
   }

   vcursor[cnum].on = FALSE;

   xlo = vcursor[cnum].x0;
   ylo = vcursor[cnum].y0;

   if (vcursor[cnum].x0 <= vcursor[cnum].x1)
      xhi = vcursor[cnum].x1;
   else
      xhi = ZN_SAMPS + vcursor[cnum].x1;

   if (vcursor[cnum].y0 <= vcursor[cnum].y1)
      yhi = vcursor[cnum].y1;
   else
      yhi = ZN_LINES + vcursor[cnum].y1;

   compile_image(xlo, ylo, xhi, yhi, X_CURSOR_REFRESH);

   if ((vcursor[cnum].x0+MCSZ > ZN_SAMPS) &&
       (vcursor[cnum].y0+MCSZ > ZN_LINES)) {
      DPR(("x11_cursor: turn cursor off, erase all four quads\n"));
      reconf_imp(dpy, win, vcursor[cnum].x0, vcursor[cnum].y0, MCSZ, MCSZ);
      reconf_imp(dpy, win, 1, vcursor[cnum].y0, MCSZ, MCSZ);
      reconf_imp(dpy, win, vcursor[cnum].x0, 1, MCSZ, MCSZ);
      reconf_imp(dpy, win, 1, 1, MCSZ, MCSZ);
   }
   else if (vcursor[cnum].x0+MCSZ > ZN_SAMPS) {
      DPR(("x11_cursor: turn cursor off, erase over y axis\n"));
      reconf_imp(dpy, win, vcursor[cnum].x0, vcursor[cnum].y0, MCSZ, MCSZ);
      reconf_imp(dpy, win, 1, vcursor[cnum].y0, MCSZ, MCSZ);
   }
   else if (vcursor[cnum].y0+MCSZ > ZN_LINES) {
      DPR(("x11_cursor: turn cursor off, erase over x axis\n"));
      reconf_imp(dpy, win, vcursor[cnum].x0, vcursor[cnum].y0, MCSZ, MCSZ);
      reconf_imp(dpy, win, vcursor[cnum].x0, 1, MCSZ, MCSZ);
   }
   else {
      DPR(("x11_cursor: turn cursor off, erase whole\n"));
      reconf_imp(dpy, win, vcursor[cnum].x0, vcursor[cnum].y0, MCSZ, MCSZ);
   }
   DPR(("x11_cursor: leave turn cursor off\n"));
}


read_vcursor(dpy, win, cnum, x, y)
Display *dpy;
MAIN_WINDOW *win;
int cnum, *x, *y;
{
   unsigned int state;
   Window root_ret, child_ret;
   int x_ret, y_ret, rootx, rooty;

   DPR(("x11_cursor: in read cursor\n"));
   if (X_Cursor_Mode == X_PLANTED || !vcursor[cnum].auto_track) {
      *x = vcursor[cnum].x;
      *y = vcursor[cnum].y;
   }
   else {
      XQueryPointer(dpy, win->image, &root_ret, &child_ret, &rootx, &rooty,
         &x_ret, &y_ret, &state);
      DPR(("x11_cursor: x_ret=%d, y_ret=%d\n", x_ret, y_ret));
      x_ret++;
      y_ret++;
      if (x_ret > ZN_SAMPS) x_ret = ZN_SAMPS;
      if (x_ret < 1) x_ret = 1;
      if (y_ret > ZN_LINES) y_ret = ZN_LINES;
      if (y_ret < 1) y_ret = 1;
      *x = x_ret;
      *y = y_ret;
      set_vcursor_loc(dpy, win, cnum, x_ret, y_ret);
   }

   DPR(("x11_cursor: read cursor--return %d %d\n", *x, *y));
}


cursor_select(dpy, win, sock_num, sock_fd)
Display *dpy;
MAIN_WINDOW *win;
int sock_num, *sock_fd;
{
   int i, x, y, rootx, rooty, impx, impy, cnum, plane, lut, index;
   int charcount, bufsize=20;
   char keybuffer[20];
   KeySym key;
   unsigned int state;
   short cont_proc, active, textwd;
   char buffer[80], tempbuf[40];
   XEvent event;
   Window root_ret, child_ret;
   XWindowAttributes info;

   DPR(("x11_cursor: in select cursor\n"));

   if ((cnum = init_floating_cursor(dpy, win)) == -1)
      return FAILURE;

   XGetWindowAttributes(dpy, win->main, &info);
   message_on(dpy, win);
   XQueryPointer(dpy, win->image, &root_ret, &child_ret, &rootx, &rooty,
        &x, &y, &state);
   x++; y++;
   x = (x > info.width-BDR_WW)  ? info.width-BDR_WW  : x;
   y = (y > info.height-BDR_WW) ? info.height-BDR_WW : y;
   x = MAX(MIN(x+X_Display_Orig_X, ZN_SAMPS), 1);
   y = MAX(MIN(y+X_Display_Orig_Y, ZN_LINES), 1);
   buffer[0] = 0;
   for (plane = 1; plane < ZN_IMPS; plane++) {
      impx = (x - 1) / X_ZOOM(plane) + X_DW_LEFT(plane);
      impy = (y - 1) / X_ZOOM(plane) + X_DW_TOP(plane);
      if (impx > ZN_SAMPS) impx -= ZN_SAMPS;
      if (impy > ZN_LINES) impy -= ZN_LINES;
      sprintf(tempbuf, "DN%1d:%3d ", plane, VRDI_IMP(plane, impx, impy));
      strcat(buffer, tempbuf);
   }
   sprintf(tempbuf, "l:%3d s:%3d", y, x);
   strcat(buffer, tempbuf);
   display_message(dpy, win, buffer, 0);
   cont_proc = TRUE;
   DPR(("x11_cursor: enter event loop\n"));
   while (cont_proc) {
      XtAppNextEvent(app_context, &event);
      switch (event.type) {
         case ButtonRelease:
            DPR(("x11_cursor: button released\n"));
            XQueryPointer(dpy, win->image, &root_ret, &child_ret,
                 &rootx, &rooty, &x, &y, &state);
            x++; y++;
            x = (x > info.width-BDR_WW) ? info.width-BDR_WW : x;
            y = (y > info.height-BDR_WW) ? info.height-BDR_WW : y;
            x = MAX(MIN(x+X_Display_Orig_X, ZN_SAMPS), 1);
            y = MAX(MIN(y+X_Display_Orig_Y, ZN_LINES), 1);
            set_vcursor_loc(dpy, win, cnum, x, y);
            XUngrabPointer(dpy, CurrentTime);
            turn_vcursor_on(dpy, win, cnum);
            cont_proc = FALSE;
            break;
         case MotionNotify:
            buffer[0] = 0;
            XQueryPointer(dpy, win->image, &root_ret, &child_ret,
                 &rootx, &rooty, &x, &y, &state);
            x++; y++;
            x = (x > info.width-BDR_WW) ? info.width-BDR_WW : x;
            y = (y > info.height-BDR_WW) ? info.height-BDR_WW : y;
            x = MAX(MIN(x+X_Display_Orig_X, ZN_SAMPS), 1);
            y = MAX(MIN(y+X_Display_Orig_Y, ZN_LINES), 1);
            for (plane = 1; plane < ZN_IMPS; plane++) {
               impx = (x - 1) / X_ZOOM(plane) + X_DW_LEFT(plane);
               impy = (y - 1) / X_ZOOM(plane) + X_DW_TOP(plane);
               if (impx > ZN_SAMPS) impx -= ZN_SAMPS;
               if (impy > ZN_LINES) impy -= ZN_LINES;
               sprintf(tempbuf, "DN%1d:%3d ", plane, VRDI_IMP(plane, impx, impy));
               strcat(buffer, tempbuf);
            }
            sprintf(tempbuf, "l:%3d s:%3d", y, x);
            strcat(buffer, tempbuf);
            display_message(dpy, win, buffer, 0);
            break;
         case KeyPress:
            DPR(("x11_cursor: KeyPress in win.image\n"));
            charcount = XLookupString((XKeyEvent *)&event, keybuffer, bufsize,
								&key, NULL);
            DPR(("x11_cursor: keysym is 0x%x, count=%d\n", key, charcount));
            process_keyboard_input(dpy, win, key, &event);
            break;
         case KeyRelease:
            DPR(("x11_cursor: KeyRelease in win.image\n"));
            charcount = XLookupString((XKeyEvent *)&event, keybuffer, bufsize,
								&key, NULL);
            DPR(("x11_cursor: keysym is 0x%x, count=%d\n", key, charcount));
            process_keyboard_input(dpy, win, key, &event);
            break;
      }
   }
   DPR(("x11_cursor: leave while\n"));
   XUngrabPointer(dpy, CurrentTime);
   message_off(dpy, win);
   DPR(("x11_cursor: leave cursor select\n"));
} 


int init_floating_cursor(dpy, win)
Display *dpy;
MAIN_WINDOW *win;
{
   int i, active, cnum;
   unsigned int event_mask;

   DPR(("x11_cursor: in init_floating_cursor\n"));

   for (i = 0, active = -1, cnum = -1; i < N_VCURSORS; i++) {
      if (vcursor[i].on) {
         active = i;
         if (vcursor[i].auto_track)
            cnum = i;
      }
   }

   if (active == -1) {
      soft_error(dpy, win, "No active cursor.");
      return (active);
   }

   if (cnum == -1) {
      soft_error(dpy, win, "Cursor autotracking has not been enabled.");
      return (cnum);
   }

   event_mask = ButtonReleaseMask | PointerMotionMask | ButtonMotionMask;
   turn_vcursor_off(dpy, win, cnum);

   switch (vcursor[cnum].type) {
      case 0:
      case 1:
         XGrabPointer(dpy, win->image, True, event_mask, GrabModeAsync,
              GrabModeAsync, False, v1_cursor, CurrentTime);
         break;
      case 2:
         XGrabPointer(dpy, win->image, True, event_mask, GrabModeAsync,
              GrabModeAsync, False, v2_cursor, CurrentTime);
         break;
      case 3:
         XGrabPointer(dpy, win->image, True, event_mask, GrabModeAsync,
              GrabModeAsync, False, v3_cursor, CurrentTime);
         break;
      case 4:
         XGrabPointer(dpy, win->image, True, event_mask, GrabModeAsync,
              GrabModeAsync, False, v4_cursor, CurrentTime);
         break;
      case 5:
         XGrabPointer(dpy, win->image, True, event_mask, GrabModeAsync,
              GrabModeAsync, False, v5_cursor, CurrentTime);
         break;
      case 6:
         XGrabPointer(dpy, win->image, True, event_mask, GrabModeAsync,
              GrabModeAsync, False, v6_cursor, CurrentTime);
         break;
   }
   XAllowEvents(dpy, SyncPointer, CurrentTime);
   DPR(("x11_cursor: control mouse\n"));
   return (cnum);
}


int find_active_cursor()
{
   int i, cnum;

   DPR(("x11_cursor: in find_active_cursor\n"));

   cnum = -1;
   for(i = 0; i < N_VCURSORS; i++) {
      if (vcursor[i].on && vcursor[i].auto_track) {
         cnum = i;
      }
   }
   DPR(("x11_cursor: active cursor = %d\n", cnum));
   return (cnum);
}


set_floating_cursor(dpy, win, cnum)
Display *dpy;
MAIN_WINDOW *win;
int cnum;
{
   DPR(("x11_cursor: setting floating cursor %d, type=%d\n", cnum,
        vcursor[cnum].type));
   switch (vcursor[cnum].type) {
      case 0:
      case 1:
         XDefineCursor(dpy, win->image, v1_cursor);
         break;
      case 2:
         XDefineCursor(dpy, win->image, v2_cursor);
         break;
      case 3:
         XDefineCursor(dpy, win->image, v3_cursor);
         break;
      case 4:
         XDefineCursor(dpy, win->image, v4_cursor);
         break;
      case 5:
         XDefineCursor(dpy, win->image, v5_cursor);
         break;
      case 6:
         XDefineCursor(dpy, win->image, v6_cursor);
         break;
   }
}
