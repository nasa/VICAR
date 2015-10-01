/* SCCS %W% %G%	 %Q% */
/******************************************************************************/
/*                                                                            */
/*  File: x11_magnify.c                                                       */
/*                                                                            */
/*  Routines:                                                                 */
/*                                                                            */
/*	go_magnify(dpy, win, fd)                                              */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		int fd;                                                       */
/*                                                                            */
/*	magnify_mode(dpy, win, fd)                                            */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		int fd;                                                       */
/*                                                                            */
/*	init_mag_image(dpy)                                                   */
/*		Display *dpy;                                                 */
/*                                                                            */
/*	display_static(dpy, win, mag, x, y, w, dispx, dispy, dispw, disph)    */
/*		Display *dpy;                                                 */
/*		Window win;                                                   */
/*		int mag, x, y, w, dispx, dispy, dispw, disph;                 */
/*                                                                            */
/*  Written by:			???                                           */
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
#if VMS_OS && ALPHA_ARCH
#undef MIN              /* redefined by Intrinsic.h */
#undef MAX
#endif
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

/* #define DEBUG */
#ifdef DEBUG
#define DPR(A)  {printf A; fflush(stdout);}
#else
#define DPR(A)
#endif

#define MAGSIZE  128
#define HMAGSIZE  64
#define EPS       16

extern int unit;
extern Cursor watch_cursor, empty_cursor;
extern GC im_gc;
extern XVisualInfo vip;
extern XImage *x_image_str;
extern unsigned long NORM_EVENT_MASK;
extern unsigned long fg, bg;
extern XColor fg_rgb;
extern XtAppContext app_context;

XImage *imagexx;


go_magnify(dpy, win, sock_num, sock_fd)
Display *dpy;
MAIN_WINDOW *win;
int sock_num, *sock_fd;
{
   while (magnify_mode(dpy, win, sock_num, sock_fd) == MAGNIFY_REDO)
      ;
}


magnify_mode(dpy, win, sock_num, sock_fd)
Display *dpy;
MAIN_WINDOW *win;
int sock_num, *sock_fd;
{
   int x, y, lut, imp;
   int x_box, y_box, width, height;
   int oldx, oldy, oldwidth, oldheight;
   int lastx, lasty;
   int rx, ry, rw, rh;
   int rootx, rooty;
   unsigned long time, oldtime = 0;
   long mask;
   unsigned int state;
   short draw_new;
   short init = FALSE;
   short cont = TRUE;
   short cur_mag = 2;
   char buf[80], temp[80];
   unsigned char dn, sdn;
   short rect_drawn = FALSE;
   short rect_bright = TRUE;
   long rect_mask;
   int rect_x, rect_y, dark_rect;
   unsigned int rect_width, rect_height;
   int index;

   int return_status = MAGNIFY_DONE;

   GC bright_rect_gc, dark_rect_gc;
   XGCValues bright_rect_values, dark_rect_values;
   XEvent event;
   Window root_win, child_win;

   message_on(dpy, win);
   display_message(dpy, win, "Initializing...", 0);

   mask = (EnterWindowMask | ButtonPressMask | LeaveWindowMask |
        StructureNotifyMask | SubstructureNotifyMask | ButtonReleaseMask);

   XSelectInput(dpy, win->image, mask);

   rect_mask = (GCFunction | GCLineWidth | GCForeground);

   bright_rect_values.function   = GXcopy;
   bright_rect_values.line_width = 1;
   bright_rect_values.foreground = fg_rgb.pixel;
   bright_rect_gc = XCreateGC(dpy, win->image, rect_mask, &bright_rect_values);

   dark_rect_values.function   = GXcopy;
   dark_rect_values.line_width = 1;
   dark_rect_values.foreground = bg;
   dark_rect_gc = XCreateGC(dpy, win->image, rect_mask, &dark_rect_values);

   DPR(("x11_magnify: GCs set up.\n"));

   if (init_mag_image(dpy, win) < 0) {
      soft_error(dpy, win, "unable to initialize magnifying glass");
      message_off(dpy, win);
      return(return_status);
   }

   sprintf(buf, "MAG:%2dx", cur_mag);
   display_message(dpy, win, buf, 0);

   DPR(("Entering the loop....\n"));

   while (cont) {
      XtAppNextEvent(app_context, &event);
      switch (event.type) {

         case MapNotify:
            DPR(("x11_magnify: MapNotify event received--"));
            if (event.xmap.window == win->main) {
               DPR(("refreshing windows.\n"));
               refresh_windows(dpy, win);
            }
            else
               DPR(("not the main window.\n"));
            break;

         case ConfigureNotify:
            DPR(("x11_magnify: ConfigureNotify event received.\n"));
            cont = FALSE;
            return_status = MAGNIFY_REDO;
            break;

         case ButtonRelease:
            DPR(("x11_magnify: ButtonRelease event received--"));
            if (event.xbutton.subwindow == win->mag) {
               DPR(("in win.mag.  Terminating....\n"));
               cont = FALSE;
            }
            break;

         case ButtonPress:
            DPR(("x11_magnify: ButtonPress event received--"));
            if (event.xbutton.subwindow == win->mag) {
               DPR(("in magnify window\n"));
               break;
            }
            else if (event.xbutton.subwindow == 0  &&  init) {
               DPR(("in main window.\n"));
               switch (event.xbutton.button) {
                  case Button2:
                     rect_drawn = !rect_drawn;
                     break;
                  case Button3:
                     break;
                  case Button1:
                     DPR(("Incrementing the current mag from %d ", cur_mag));
                     cur_mag = ((cur_mag*2) > 16) ? 2 : cur_mag*2;
                     DPR(("to %d\n", cur_mag));
                     sprintf(buf, "MAG:%2dx", cur_mag);
                     display_message(dpy, win, buf, 0);
               }

               if (rect_drawn) {
                  DPR(("x11_magnify: rect_drawn=TRUE\n"));
                  display_static(dpy, win->image, cur_mag, x, y,
                       ZN_SAMPS, x_box, y_box, width, height);
                  x = event.xbutton.x + 1;
                  y = event.xbutton.y + 1;
                  for (imp = 1, dark_rect=FALSE; imp < ZN_IMPS; imp++) {
                     dn = VRDI_IMP(imp, x-X_Display_Orig_X, y-X_Display_Orig_Y);
                     sdn = VRDI_LUT(imp, dn);
                     if (imp == 1)
                        sprintf(buf, "DN1=%3d", dn);
                     else {
                        sprintf(temp, " DN%1d=%3d", imp, dn);
                        strcat(buf, temp);
                     }
                     if (sdn > 196)
                        dark_rect = TRUE;
                  }
                  sprintf(temp, " l=%3d s=%3d MAG=%2dx",
                       (y - X_Display_Orig_Y), (x - X_Display_Orig_X), cur_mag);
                  strcat(buf, temp);
                  display_message(dpy, win, buf, 0);

                  rect_x = x - 1;
                  rect_y = y - 1;
                  rect_width = cur_mag + 1;
                  rect_height = cur_mag + 1;
                  if (dark_rect)
                     XDrawRectangle(dpy, win->image, dark_rect_gc,
                          rect_x, rect_y, rect_width, rect_height);
                  else
                     XDrawRectangle(dpy, win->image, bright_rect_gc,
                          rect_x, rect_y, rect_width, rect_height);
               }

               else { /* if !rect_drawn */
                  DPR(("x11_magnify: rect_drawn = FALSE\n"));
                  sprintf(buf, "MAG:%2dx", cur_mag);
                  display_message(dpy, win, buf, 0);
                  display_static(dpy, win->image, cur_mag, x, y,
                       ZN_SAMPS, x_box, y_box, width, height);
               }
            }
            else
               DPR(("Who knows where?\n"));
            break;

         case MotionNotify:
            if (event.xmotion.subwindow != 0)
               break;
            x = event.xmotion.x + 1;
            y = event.xmotion.y + 1;
            lastx = x;
            lasty = y;

            time = event.xmotion.time;
            if (((time - oldtime) < 50) && (time > oldtime))
               break;
            else if ((time - oldtime) < 150)
               break;

            oldtime = time;
            draw_new = ((x > 0) && (x < (ZN_SAMPS)) &&
                        (y > 0) && (y < (ZN_LINES)));

            x_box = x - HMAGSIZE;
            y_box = y - HMAGSIZE;
            width = MAGSIZE;
            height = MAGSIZE;

            if (x_box < 0) {
               width = MAGSIZE + x_box;
               x_box = 0;
            }

            if (y_box < 0) {
               height = MAGSIZE + y_box;
               y_box = 0;
            }

            if (y_box > oldy) {
               rx = oldx;
               ry = oldy;
               rw = oldwidth;
               rh = MIN(oldheight, y_box - oldy);
               XPutImage(dpy, win->image, im_gc, x_image_str,
                    rx+X_Display_Orig_X, ry+X_Display_Orig_Y, rx, ry, rw, rh);
            }
            else if ((y_box+height) < (oldy+oldheight)) {
               rx = oldx;
               ry = MAX(oldy, (y_box+height));
               rw = oldwidth;
               rh = oldy + oldheight - ry;
               XPutImage(dpy, win->image, im_gc, x_image_str,
                    rx+X_Display_Orig_X, ry+X_Display_Orig_Y, rx, ry, rw, rh);
            }

            if (x_box > oldx) {
               rx = oldx;
               ry = oldy;
               rw = MIN(oldwidth, x_box - oldx);
               rh = oldheight;
               XPutImage(dpy, win->image, im_gc, x_image_str,
                    rx+X_Display_Orig_X, ry+X_Display_Orig_Y, rx, ry, rw, rh);
            }
            else if ((x_box+width) < (oldx+oldwidth)) {
               rx = MAX(oldx, (x_box+width));
               ry = oldy;
               rw = oldx + oldwidth - rx ;
               rh = oldheight;
               XPutImage(dpy, win->image, im_gc, x_image_str,
                    rx+X_Display_Orig_X, ry+X_Display_Orig_Y, rx, ry, rw, rh);
            }

            if (draw_new) {
               display_static(dpy, win->image, cur_mag, x, y,
                    ZN_SAMPS, x_box, y_box, width, height);

               if (rect_drawn) {
                  for (imp = 1, dark_rect=FALSE; imp < ZN_IMPS; imp++) {
                     dn = VRDI_IMP(imp, x-X_Display_Orig_X, y-X_Display_Orig_Y);
                     sdn = VRDI_LUT(imp, dn);
                     if (imp == 1)
                        sprintf(buf, "DN1=%3d", dn);
                     else {
                        sprintf(temp, " DN%1d=%3d", imp, dn);
                        strcat(buf, temp);
                     }
                     if (sdn > 196)
                        dark_rect = TRUE;
                  }
                  sprintf(temp, " l=%3d s=%3d MAG=%2dx",
                       (y-X_Display_Orig_Y), (x-X_Display_Orig_X), cur_mag);
                  strcat(buf, temp);
                  display_message(dpy, win, buf, 0);

                  rect_x = x - 1;
                  rect_y = y - 1;
                  rect_width = cur_mag + 1;
                  rect_height = cur_mag + 1;

                  if (dark_rect)
                     XDrawRectangle(dpy, win->image, dark_rect_gc,
                          rect_x, rect_y, rect_width, rect_height);
                  else
                     XDrawRectangle(dpy, win->image, bright_rect_gc,
                          rect_x, rect_y, rect_width, rect_height);
               }

               oldx = x_box;
               oldy = y_box;
               oldwidth = width;
               oldheight = height;
            }
            XFlush(dpy);
            break;

         case EnterNotify:
            DPR(("x11_magnify: EnterNotify event received.\n"));
            if (event.xmotion.subwindow != 0)
               break;

            if (XQueryPointer(dpy, win->image, &root_win, &child_win,
                 &rootx, &rooty, &x, &y, &state) == False)
               break;
            x++;		/* Make it 1-based */
            y++;
            XGrabPointer(dpy, win->image, True, mask, GrabModeAsync,
                 GrabModeAsync, False, empty_cursor, CurrentTime);

            /* start accepting motion events */
            mask |= (PointerMotionMask | ButtonMotionMask);
            XSelectInput(dpy, win->image, mask);

            x_box = x - HMAGSIZE;
            y_box = y - HMAGSIZE;
            width = MAGSIZE;
            height = MAGSIZE;

            if (x_box < 0) {
               width = MAGSIZE + x_box;
               x_box = 0;
            }

            if (y_box < 0) {
               height = MAGSIZE + y_box;
               y_box = 0;
            }

            display_static(dpy, win->image, cur_mag, x, y, ZN_SAMPS, x_box,
                 y_box, width, height);

            oldx = x_box;
            oldy = y_box;
            oldwidth = width;
            oldheight = height;
            init = True;
            break;

         case LeaveNotify:
            DPR(("x11_magnify: LeaveNotify event received\n"));
            XUngrabPointer(dpy, CurrentTime);
            mask &= ~(PointerMotionMask | ButtonMotionMask);
            XSelectInput(dpy, win->image, mask);

            if (init) {
               XPutImage(dpy, win->image, im_gc, x_image_str, oldx+X_Display_Orig_X,
                    oldy+X_Display_Orig_Y, oldx, oldy, oldwidth, oldheight);
               init = False;
            }
            break;

         default:
            break;
      }
   }

   free(imagexx->data);
   XDestroyImage(imagexx);
   XFreeGC(dpy, bright_rect_gc);
   XFreeGC(dpy, dark_rect_gc);

   XUngrabPointer(dpy, CurrentTime);
   XSelectInput(dpy, win->image, NORM_EVENT_MASK);
   message_off(dpy, win);
   refresh_windows(dpy, win);
   return(return_status);
}


init_mag_image(dpy, win)
Display *dpy;
MAIN_WINDOW *win;
{
   unsigned char *ptr;

   ptr = (unsigned char *) malloc(X_Bytes_Per_Pixel*MAGSIZE*MAGSIZE);
   if (ptr == 0) {
      soft_error(dpy, win, "unable to allocate space for magnification window");
      return(FAILURE);
   }

   imagexx = XCreateImage(dpy, vip.visual, X_Image_Depth, ZPixmap, 0,
      (char *)ptr, MAGSIZE, MAGSIZE, 8, 0);

   DPR(("x11_magnify: image width=%d, image height=%d\n",
        imagexx->width, imagexx->height));
   DPR(("x11_magnify: bitmap_unit=%d, bitmap_pad=%d, depth=%d, bytes_per_line=%d\n",
        imagexx->bitmap_unit, imagexx->bitmap_pad,
        imagexx->depth, imagexx->bytes_per_line));
   DPR(("x11_magnify: bits_per_pixel=%d, rmask=%d, gmask=%d, bmask=%d\n",
        imagexx->bits_per_pixel, imagexx->red_mask, imagexx->green_mask,
        imagexx->blue_mask));

   if (imagexx == 0) {
      soft_error(dpy, win, "unable to create image structures");
      return(FAILURE);
   }
   return(SUCCESS);
}


display_static(dpy, win, mag, x, y, w, dispx, dispy, dispw, disph)
Display *dpy;
Window win;
int mag, x, y, w, dispx, dispy, dispw, disph;
{
   register int i, j, ctr, line_width, mag_line_width;
   register int scale;
   register int yindex, xindex, xxindex;
   unsigned char *image, *xx;

   image = (unsigned char *) x_image_str->data;
   xx = (unsigned char *) imagexx->data;
   line_width = w * X_Bytes_Per_Pixel;
   mag_line_width = MAGSIZE * X_Bytes_Per_Pixel;

   switch (mag) {
      case 2:
         scale  = 1;
         break;
      case 4:
         scale  = 2;
         break;
      case 8:
         scale  = 3;
         break;
      case 16:
         scale  = 4;
         break;
      default:
         scale  = 0;
   }

   for (i = 0; i < disph; i++) {
      for (j = 0; j < dispw; j++) {
         yindex = ((y - 1) + ((i - (y-dispy)) >> scale)) * line_width;
         xindex = ((x - 1) + ((j - (x-dispx)) >> scale)) * X_Bytes_Per_Pixel;
         xxindex = (i * mag_line_width) + (j * X_Bytes_Per_Pixel);
         for (ctr = 0; ctr < X_Bytes_Per_Pixel; ctr++)
            xx[xxindex+ctr] = image[yindex + xindex + ctr];
      }
   }
   XPutImage(dpy, win, im_gc, imagexx, 0, 0, dispx, dispy, dispw, disph);
   XFlush(dpy);
}
