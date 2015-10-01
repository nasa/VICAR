/* SCCS %W% %G%  %Q% */
/******************************************************************************/
/*                                                                            */
/*  File: x11_scroll.c                                                        */
/*                                                                            */
/*  Routines:                                                                 */
/*                                                                            */
/*	up_scroll(dpy, win)                                                   */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*                                                                            */
/*	down_scroll(dpy, win)                                                 */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*                                                                            */
/*	left_scroll(dpy, win)                                                 */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*                                                                            */
/*	right_scroll(dpy, win)                                                */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*                                                                            */
/*	vert_scroll(dpy, win, event)                                          */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		XEvent event;                                                 */
/*                                                                            */
/*	horiz_scroll(dpy, win, event)                                         */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		XEvent event;                                                 */
/*                                                                            */
/*	vert_elev(dpy, win)                                                   */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*                                                                            */
/*	horiz_elev(dpy, win)                                                  */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*                                                                            */
/*  Written by:			Mark Mann                                     */
/*  Date:			???                                           */
/*                                                                            */
/*  Cognizant Programmer:	Paul Bartholomew                              */
/*  Revision History:                                                         */
/*    Date    FR #   Description                                              */
/*  --------  -----  -------------------------------------------------------  */
/*  08-01-82   ---   PDB-Complete rewrite.                                    */
/*                                                                            */
/******************************************************************************/

#include "xvmaininc.h"
#include "xdexterns.h"
#include "x11_device_main.h"

#ifdef DEBUG
#define DPR(A)  {printf A; fflush(stdout);}
#else
#define DPR(A)
#endif

extern int unit;
extern Cursor left_cursor, up_cursor;


up_scroll(dpy, win)
Display *dpy;
MAIN_WINDOW *win;
{
   XEvent xevent;

   if (X_Percent_Scroll_Y > 0.0) {
      X_Percent_Scroll_Y = MAX(0, X_Percent_Scroll_Y - 0.01);
      reconfigure(dpy, win);
   }

   if (XCheckTypedEvent(dpy, ButtonRelease, &xevent) == False &&
       X_Percent_Scroll_Y > 0.0) {
      sleep(1);
      while (XCheckTypedEvent(dpy, ButtonRelease, &xevent) == False &&
           X_Percent_Scroll_Y > 0.0) {
         X_Percent_Scroll_Y = MAX(0, X_Percent_Scroll_Y - 0.01);
         reconfigure(dpy, win);
      }
   }
}


down_scroll(dpy, win)
Display *dpy;
MAIN_WINDOW *win;
{
   XEvent xevent;

   if (X_Percent_Scroll_Y < 1.0) {
      X_Percent_Scroll_Y = MIN(1.0, X_Percent_Scroll_Y + 0.01);
      reconfigure(dpy, win);
   }

   if (XCheckTypedEvent(dpy, ButtonRelease, &xevent) == False &&
        X_Percent_Scroll_Y > 0.0) {
      sleep(1);

      while (XCheckTypedEvent(dpy, ButtonRelease, &xevent) == False &&
           X_Percent_Scroll_Y < 1.0) {
         X_Percent_Scroll_Y = MIN(1.0, X_Percent_Scroll_Y + 0.01);
         reconfigure(dpy, win);
      }
   }
}


left_scroll(dpy, win)
Display *dpy;
MAIN_WINDOW *win;
{
   XEvent xevent;

   if (X_Percent_Scroll_X > 0.0) {
      X_Percent_Scroll_X = MAX(0.0, X_Percent_Scroll_X - 0.01);
      reconfigure(dpy, win);
   }

   if (XCheckTypedEvent(dpy, ButtonRelease, &xevent) == False &&
        X_Percent_Scroll_X > 0.0) {
      sleep(1);

      while (XCheckTypedEvent(dpy, ButtonRelease, &xevent) == False &&
           X_Percent_Scroll_X > 0.0) {
         X_Percent_Scroll_X = MAX(0.0, X_Percent_Scroll_X - 0.01);
         reconfigure(dpy, win);
      }
   }
}


right_scroll(dpy, win)
Display *dpy;
MAIN_WINDOW *win;
{
   XEvent xevent;

   if (X_Percent_Scroll_X < 1.0) {
      X_Percent_Scroll_X = MIN(1.0, X_Percent_Scroll_X + 0.01);
      reconfigure(dpy, win);
   }

   if (XCheckTypedEvent(dpy, ButtonRelease, &xevent) == False &&
        X_Percent_Scroll_X > 0.0) {
      sleep(1);

      while (XCheckTypedEvent(dpy, ButtonRelease, &xevent) == False &&
           X_Percent_Scroll_X < 1.0) {
         X_Percent_Scroll_X = MIN(1.0, X_Percent_Scroll_X + 0.01);
         reconfigure(dpy, win);
      }
   }
}


vert_scroll(dpy, win, event)
Display *dpy;
MAIN_WINDOW *win;
XButtonEvent event;
{
   XEvent xevent;
   XWindowAttributes info;
   float change;

   XGetWindowAttributes(dpy, win->ve, &info);

   if (event.y < info.y)
      change = -0.10;
   else
      change = 0.10;

   X_Percent_Scroll_Y = MAX(0.0, MIN(1.0, X_Percent_Scroll_Y + change));
   reconfigure(dpy, win);

   if (XCheckTypedEvent(dpy, ButtonRelease, &xevent) == False &&
        X_Percent_Scroll_Y > 0.0 && X_Percent_Scroll_Y < 1.0) {
      sleep(1);

      while (XCheckTypedEvent(dpy, ButtonRelease, &xevent) == False &&
           X_Percent_Scroll_Y > 0.0 && X_Percent_Scroll_Y < 1.0) {
         X_Percent_Scroll_Y = MAX(0.0, MIN(1.0, X_Percent_Scroll_Y + change));
         reconfigure(dpy, win);
      }
   }
}


horiz_scroll(dpy, win, event)
Display *dpy;
MAIN_WINDOW *win;
XButtonEvent event;
{
   XEvent xevent;
   XWindowAttributes info;
   float change;

   XGetWindowAttributes(dpy, win->he, &info);

   if (event.x < info.x)
      change = -0.10;
   else
      change = 0.10;

   X_Percent_Scroll_X = MAX(0.0, MIN(1.0, X_Percent_Scroll_X + change));
   reconfigure(dpy, win);

   if (XCheckTypedEvent(dpy, ButtonRelease, &xevent) == False &&
        X_Percent_Scroll_X > 0.0 && X_Percent_Scroll_X < 1.0) {
      sleep(1);

      while (XCheckTypedEvent(dpy, ButtonRelease, &xevent) == False &&
           X_Percent_Scroll_X > 0.0 && X_Percent_Scroll_X < 1.0) {
         X_Percent_Scroll_X = MAX(0.0, MIN(1.0, X_Percent_Scroll_X + change));
         reconfigure(dpy, win);
      }
   }
}


vert_elev(dpy, win)
Display *dpy;
MAIN_WINDOW *win;
{
   int cont_proc = 1, middle_line, screen_offset;
   int x,y;
   Window root_ret, child_ret;
   int rootx, rooty, state;
   XEvent event;
   XWindowAttributes info;
   char buffer[100];
   int percent;

   message_on(dpy, win);

   XGetWindowAttributes(dpy, win->main, &info);

   XGrabPointer(dpy, RootWindow(dpy, DefaultScreen(dpy)), True,
        (ButtonReleaseMask | PointerMotionMask | ButtonMotionMask),
        GrabModeSync, GrabModeAsync, win->main, left_cursor, CurrentTime);
   XAllowEvents(dpy, SyncPointer, CurrentTime);

   screen_offset = (info.height-BDR_WW)/2;
   middle_line = X_Percent_Scroll_Y * (ZN_LINES-(info.height-BDR_WW))+screen_offset;
   percent = X_Percent_Scroll_Y * 100;
   sprintf(buffer,"line %3d(%3d%%)",middle_line,percent);
   display_message(dpy, win, buffer, 0);
   while (cont_proc) {
      XNextEvent(dpy,&event);
      switch (event.type) {
         case MotionNotify:
            XQueryPointer(dpy, win->main, &root_ret, &child_ret,
                 &rootx, &rooty, &x, &y, (unsigned int *)&state);
            if (y<0)
               X_Percent_Scroll_Y = 0.0;
            else if (y >= info.height-1)
               X_Percent_Scroll_Y = 1.0;
            else
               X_Percent_Scroll_Y = (float)((float)(y)/(float)(info.height));
            percent = X_Percent_Scroll_Y * 100;
            middle_line = X_Percent_Scroll_Y * (ZN_LINES-(info.height-BDR_WW)) +
            screen_offset;
            sprintf(buffer,"line %3d(%3d%%)", middle_line, percent);
            display_message(dpy, win, buffer, 0);
            break;
         case ButtonRelease:
            XQueryPointer(dpy, win->main, &root_ret, &child_ret,
                 &rootx, &rooty, &x, &y, (unsigned int *)&state);
            x = event.xmotion.x;
            y = event.xmotion.y;
            if (y<0)
               X_Percent_Scroll_Y = 0.0;
            else if (y >= info.height-1)
               X_Percent_Scroll_Y = 1.0;
            else
               X_Percent_Scroll_Y = (float)((float)(y)/(float)(info.height));
            cont_proc = 0;
            break;
         default:
            break;
      }
   }
   message_off(dpy, win);
   XUngrabPointer(dpy, CurrentTime);
   reconfigure(dpy, win);
}


horiz_elev(dpy, win)
Display *dpy;
MAIN_WINDOW *win;
{
   int cont_proc = 1, middle_samp, screen_offset;
   int x,y;
   Window root_ret, child_ret;
    int rootx, rooty, state;
   XEvent event;
   XWindowAttributes info;
   char buffer[100];
   int percent;

   message_on(dpy, win);
   XGetWindowAttributes(dpy, win->main, &info);

   XGrabPointer(dpy, RootWindow(dpy, DefaultScreen(dpy)), True,
        (ButtonReleaseMask | PointerMotionMask | ButtonMotionMask),
        GrabModeSync, GrabModeAsync, win->main, up_cursor, CurrentTime);
   XAllowEvents(dpy, SyncPointer, CurrentTime);
   screen_offset = (info.width-BDR_WW) / 2;
   middle_samp = X_Percent_Scroll_X * (ZN_SAMPS-(info.width-BDR_WW)) + screen_offset;
   percent = X_Percent_Scroll_X * 100;
   sprintf(buffer,"sample %3d(%3d%%)", middle_samp, percent);
   display_message(dpy, win, buffer, 0);
   while (cont_proc) {
      XNextEvent(dpy,&event);
      switch (event.type) {
         case MotionNotify:
            XQueryPointer(dpy, win->main, &root_ret, &child_ret,
                 &rootx, &rooty, &x, &y, (unsigned int *)&state);
            if (x < 0)
               X_Percent_Scroll_X = 0.0;
            else if (x >= info.width-1)
               X_Percent_Scroll_X = 1.0;
            else
               X_Percent_Scroll_X =(float)((float)(x)/(float)(info.width));
            middle_samp = X_Percent_Scroll_X * (ZN_SAMPS-(info.width-BDR_WW)) +
                 screen_offset;
            percent = X_Percent_Scroll_X*100;
            sprintf(buffer,"sample %3d(%3d%%)", middle_samp, percent);
            display_message(dpy, win, buffer, 0);
            break;
         case ButtonRelease:
            XQueryPointer(dpy, win->main, &root_ret, &child_ret,
                &rootx, &rooty, &x, &y, (unsigned int *)&state);
            if (x < 0)
               X_Percent_Scroll_X = 0.0;
            else if (x >= info.width-1)
               X_Percent_Scroll_X = 1.0;
            else
               X_Percent_Scroll_X = (float)((float)(x)/(float)(info.width));
            cont_proc = 0;
            break;
         default:
            break;
      }
   }
   message_off(dpy, win);
   XUngrabPointer(dpy, CurrentTime);
   reconfigure(dpy, win);
}
