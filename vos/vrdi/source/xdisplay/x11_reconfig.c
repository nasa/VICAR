/* SCCS %W% %G%  %Q% */
/******************************************************************************/
/*                                                                            */
/*  File: x11_reconfig.c                                                      */
/*                                                                            */
/*  Routines:                                                                 */
/*                                                                            */
/*	reconfigure(dpy, win)                                                 */
/*              Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		-- refresh the main window                                    */
/*                                                                            */
/*	reconf_image(dpy, win, x, y, width, height)                           */
/*              Display *dpy;                                                 */
/*		Window win;                                                   */
/*		int x, y, width, height;                                      */
/*		-- refresh a specified area within the image window           */
/*                                                                            */
/*	grab_window_event(win)                                                */
/*		Window win;                                                   */
/*		-- remove all events off the event queue relating to win      */
/*                                                                            */
/*	reconf_insect_cursor(dpy, nc, x, y, ns)                               */
/*              Display *dpy;                                                 */
/*		int nc, x, y, ns;                                             */
/*		-- fix the shown image memory plane if a cursor is within a   */
/*		   changed portion of it                                      */
/*                                                                            */
/*	reconf_imp(dpy, win, ss, sl, ns, nl)                                  */
/*              Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		int ss, sl, ns, nl;                                           */
/*		-- translate image plane coordinates into window coordinates  */
/*                                                                            */
/*	compile_image(ss, sl, es, el, imp)                                    */
/*		int ss, sl, es, el;                                           */
/*                                                                            */
/*	refresh_icon(dpy, win)                                                */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*                                                                            */
/*	reconf_icon_imp(dpy, win, ss, sl, wd, ht)                             */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		int ss, sl, wd, ht;                                           */
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

/* #define DEBUG */
#ifdef DEBUG
#define DPR(A)  {printf A; fflush(stdout);}
#else
#define DPR(A)
#endif

extern int unit;
extern unsigned long NORM_EVENT_MASK;
extern VRDIcursor vcursor[];
extern Pixmap image_pm;
extern GC pm_gc, im_gc, icon_gc;
extern XImage *x_image_str;
extern XFontStruct *icon_font_struct;
extern int iconfontx, iconfonty;
extern XImage *icon_ximage;
extern Colormap colormap;
extern XColor fg_rgb;


reconfigure(dpy, win)
Display *dpy;
MAIN_WINDOW *win;
{
   XWindowAttributes info;
   int width, height;
   int vert_scr, horiz_scr;
   int textwd;

   XGetWindowAttributes(dpy, win->main, &info);
   width  = info.width;
   height = info.height;

   DPR(("x11_reconfig: reconfigure()--width=%d, height=%d\n", width, height));

   X_Display_Orig_X = X_Percent_Scroll_X * (ZN_SAMPS - (width-BDR_WW));
   X_Display_Orig_Y = X_Percent_Scroll_Y * (ZN_LINES - (height-BDR_WW));

   vert_scr  = X_Percent_Scroll_Y * (height - BDR_WW*4) + BDR_WW;

   XMoveResizeWindow(dpy, win->s1, width-BDR_WW, BDR_WW, BDR_WW,
        height-(3*BDR_WW));
   switch (X_Image_Depth) {
      case 24:
         horiz_scr = X_Percent_Scroll_X * (width  - BDR_WW*8) + 3*BDR_WW;
         XMoveResizeWindow(dpy, win->s2, 3*BDR_WW, height-BDR_WW,
              width-(7*BDR_WW), BDR_WW);
         XMoveWindow(dpy, win->lt, 2*BDR_WW, height-BDR_WW);
         XMoveWindow(dpy, win->rt, width-4*BDR_WW, height-BDR_WW);
         XMoveWindow(dpy, win->he, horiz_scr, height-BDR_WW);
         break;
      case 8:
         horiz_scr = X_Percent_Scroll_X * (width  - BDR_WW*8) + 4*BDR_WW;
         XMoveWindow(dpy, win->bit_mode, 2*BDR_WW, height-BDR_WW);
         XMoveResizeWindow(dpy, win->s2, 4*BDR_WW, height-BDR_WW,
              width-(8*BDR_WW), BDR_WW);
         XMoveWindow(dpy, win->lt, 3*BDR_WW, height-BDR_WW);
         XMoveWindow(dpy, win->rt, width-4*BDR_WW, height-BDR_WW);
         XMoveWindow(dpy, win->he, horiz_scr, height-BDR_WW);
         break;
   }
   XMoveResizeWindow(dpy, win->right_fill, width-BDR_WW, 0,
        BDR_WW, height-BDR_WW);
   XMoveResizeWindow(dpy, win->message, BDR_WW, height-BDR_WW,
        width-BDR_WW, BDR_WW);

   XMoveWindow(dpy, win->mag, 0, height-BDR_WW);
   XMoveWindow(dpy, win->lock, BDR_WW, height-BDR_WW);
   XMoveWindow(dpy, win->ve, width-BDR_WW, vert_scr);
   XMoveWindow(dpy, win->hist_but, width-3*BDR_WW, height-BDR_WW);
   XMoveWindow(dpy, win->cursor, width-2*BDR_WW, height-BDR_WW);
   XMoveWindow(dpy, win->up, width-BDR_WW, 0);
   XMoveWindow(dpy, win->dn, width-BDR_WW, height-2*BDR_WW);

   display_message(dpy, win, 0, 1);

   reconf_image(dpy, win->image, 1, 1, width-BDR_WW, height-BDR_WW);

}


reconf_image(dpy, win, ix, iy, iw, ih)
Display *dpy;
Window win;
int ix, iy, iw, ih;
{
   int loop;
   int x, y, width, height;
   int srcx, srcy, dstx, dsty;

   if (iw > 0 && ih > 0) {
      x = ix;
      y = iy;
      width = iw;
      height = ih;

      for (loop = 0; loop < N_VCURSORS; loop++) {
         if (vcursor[loop].on &&
              (X_Cursor_Mode == X_PLANTED || vcursor[loop].auto_track == FALSE))
            reconf_insect_cursor(dpy, loop, x, y, width);
      }
      srcx = X_Display_Orig_X + x - 1;
      srcy = X_Display_Orig_Y + y - 1;
      dstx = x - 1;
      dsty = y - 1;

      DPR(("x11_reconfig: XPutImage srcx=%d, srcy=%d, dstx=%d, dsty=%d, width=%d, height=%d\n",
         srcx, srcy, dstx, dsty, width, height));
#if VMS_OS
      XPutImage(dpy, win, im_gc, x_image_str, srcx, srcy, dstx, dsty,
           width, height);
#else
      XPutImage(dpy, image_pm, pm_gc, x_image_str, srcx, srcy, srcx, srcy,
           width, height);

      XCopyArea(dpy, image_pm, win, im_gc, srcx, srcy, width, height,
           dstx, dsty);
#endif
   }
}


insect(line_c1, line_c2, object_c1, object_c2)
int line_c1, line_c2, object_c1, object_c2;
{
   int l1, l2, o1, o2;
   int status = FALSE;

   if (line_c1 > line_c2) {
      l1 = line_c2;
      l2 = line_c1;
   }
   else {
      l1 = line_c1;
      l2 = line_c2;
   }

   if (object_c1 > object_c2) {
      o1 = object_c2;
      o2 = object_c1;
   }
   else {
      o1 = object_c1;
      o2 = object_c2;
   }

   if ((l1 <= o2) && (l2 >= o1))
      status = TRUE;

   return(status);
}


reconf_insect_cursor(dpy, nc, x, y, ns)
Display *dpy;
int nc, x, y, ns;
{
   int i, j, acti, actj;
   int act_x1, act_x2, act_y1, act_y2;
   int act_cur_x0, act_cur_x1, act_cur_y0, act_cur_y1;
   int xlo, ylo, xhi, yhi;
   VRDIcursor *tmp;

   DPR(("x11_reconfig: cursor %d: x=%d, y=%d, ns=%d\n", nc, x, y, ns));
   tmp = &vcursor[nc];

   if (tmp->on) {
      act_x1 = x;
      act_x2 = act_x1 + ns - 1;
      act_y1 = y;
      act_y2 = act_y1 + ns - 1;

      act_cur_x0 = tmp->x0 - X_Display_Orig_X;
      act_cur_x1 = tmp->x1 - X_Display_Orig_X;
      act_cur_y0 = tmp->y0 - X_Display_Orig_Y;
      act_cur_y1 = tmp->y1 - X_Display_Orig_Y;

      if (insect(act_x1, act_x2, act_cur_x0, act_cur_x1) &&
           insect(act_y1, act_y2, act_cur_y0, act_cur_y1)) {
         if (tmp->x0 <= tmp->x1) {
            xlo = tmp->x0;
            xhi = tmp->x1;
         }
         else {
            xlo = tmp->x0;
            xhi = ZN_SAMPS + tmp->x1;
         }
         if (tmp->y0 <= tmp->y1) {
            ylo = tmp->y0;
            yhi = tmp->y1;
         }
         else {
            ylo = tmp->y0;
            yhi = ZN_LINES + tmp->y1;
         }

         DPR(("x11_reconfig: drawing cursor from (%d,%d) to (%d,%d)\n",
              xlo, ylo, xhi, yhi));

         for (i = ylo; i <= yhi; i++) {
            if (i <= ZN_LINES)
               acti = i;
            else
               acti = i - ZN_LINES ;
            for (j = xlo; j <= xhi; j++) {
               if (j <= ZN_SAMPS)
                  actj = j;
               else
                  actj = j - ZN_SAMPS;
               if (tmp->pattern[i-ylo][j-xlo]) {
                  switch (X_Image_Depth) {
                     case 24:
                        X_IMP(X_RED, actj, acti) = (unsigned char) fg_rgb.red;
                        X_IMP(X_GREEN, actj, acti) = (unsigned char) fg_rgb.green;
                        X_IMP(X_BLUE, actj, acti) = (unsigned char) fg_rgb.blue;
                        break;
                     case 8:
                        X_IMP(X_RED, actj, acti) = (unsigned char) fg_rgb.pixel;
                        break;
                  }
               }
            }
         }
      }
   }
}


reconf_imp(dpy, win, ss, sl, ns, nl)
Display *dpy;
MAIN_WINDOW *win;
int ss, sl, ns, nl;
{
   XWindowAttributes info, icon_info;
   int adj_ss, adj_sl, adj_ns, adj_nl;
   float x_scale, y_scale, tmp_ss, tmp_sl, tmp_ns, tmp_nl;

   XGetWindowAttributes(dpy, win->main, &info);
   XGetWindowAttributes(dpy, win->icon.main, &icon_info);
   info.width -= BDR_WW;		/* compensate for borders */
   info.height -= BDR_WW;

   if (icon_info.map_state == IsViewable)
      reconf_icon_imp(dpy, win, ss, sl, ns, nl);
   else {
      if (ss - X_Display_Orig_X < 0) {
         adj_ss = 0;
         adj_ns = ns + (ss - X_Display_Orig_X);
         adj_ns = MIN(adj_ns, info.width);
      }
      else {
         adj_ss = ss - X_Display_Orig_X;
         adj_ns = ns;
         adj_ns = MIN(adj_ns, info.width);
      }

      if (sl - X_Display_Orig_Y < 0) {
         adj_sl = 0;
         adj_nl = nl + (sl - X_Display_Orig_Y);
         adj_nl = MIN(adj_nl, info.height);
      }
      else {
         adj_sl = sl - X_Display_Orig_Y;
         adj_nl = nl;
         adj_nl = MIN(adj_nl, info.height);
      }
      DPR(("x11_reconfig: orig_x=%d, orig_y=%d, adj_sl=%d, adj_nl=%d, adj_ss=%d, adj_ns=%d\n",
           X_Display_Orig_X, X_Display_Orig_Y, adj_sl, adj_nl, adj_ss, adj_ns));
      reconf_image(dpy, win->image, adj_ss, adj_sl, adj_ns, adj_nl);
   }
}


compile_image(ss, sl, es, el, imp)
int ss, sl, es, el, imp;
{
   int line, samp, sampctr, linectr, lut, tempimp, refsamp, nl, ns;
   int X_line, X_samp, X_index, refX_samp, X_ctr, refX_ctr;
   int ovline, ovsamp, ovrefsamp, xzoomctr, yzoomctr;
   unsigned char dn, *local_imp_ptr, *X_imp_ptr, *new_ptr, *ov_imp_ptr;
   unsigned char *ovly_lut_ptr, *lut_ptr;

   for (lut = 1; lut < ZN_IMPS; lut++) {
      if (imp == X_Overlay_Imp || imp == X_ALL_IMPS || imp == X_CURSOR_REFRESH)
         tempimp = X_WHICH_IMP(lut);
      else
         tempimp = imp;

      if (X_WHICH_IMP(lut) != tempimp)
         continue;

      DPR(("x11_reconfig: compiling image--imp=%d, tempimp=%d, lut=%d\n",
           imp, tempimp, lut));
      DPR(("x11_reconfig: ss=%d, sl=%d, es=%d, el=%d\n", ss, sl, es, el));

      refsamp = ss;
      line = sl;
      if (imp == X_CURSOR_REFRESH)
         cursor_coords(tempimp, &refsamp, &line, es, el, &refX_samp, &X_line, &ns, &nl);
      else
         imp2raw(tempimp, &refsamp, &line, es, el, &refX_samp, &X_line, &ns, &nl);

      refX_ctr = refX_samp;
      refX_samp = (refX_samp - 1) * X_Bytes_Per_Pixel + color_order[lut];
      DPR(("x11_reconfig: samp=%d, line=%d, ns=%d, nl=%d, X_line=%d, X_samp=%d\n",
           refsamp, line, ns, nl, X_line, refX_ctr));
      lut_ptr = X_LUTP(lut, 0);
      ovly_lut_ptr = X_OVLY_LUTP(lut, 0);

      if (X_Overlay_On && X_WRITTEN(X_Overlay_Imp)) {

         raw2imp(X_Overlay_Imp, refX_ctr, X_line, &ovrefsamp, &ovline);
         DPR(("x11_reconfig: ovsamp=%d, ovline=%d\n", ovrefsamp, ovline));

         for (linectr = 0; linectr < nl; linectr++) {
            local_imp_ptr = VRDI_IMPP(tempimp, 1, line);
            ov_imp_ptr = VRDI_IMPP(X_Overlay_Imp, 1, ovline);
            X_imp_ptr = X_IMPP(lut, 1, X_line);
            X_line++;

            if ((X_line-1) % X_ZOOM(tempimp) == 0) {
               line++;
               line = (line <= ZN_LINES) ? line : 1;
            }
            if ((X_line-1) % X_ZOOM(X_Overlay_Imp) == 0) {
               ovline++;
               ovline = (ovline <= ZN_LINES) ? ovline : 1;
            }
            if (X_line > ZN_LINES) {
               X_line = 1;
               line = X_DW_TOP(tempimp);
               ovline = X_DW_TOP(X_Overlay_Imp);
            }

            X_ctr = refX_ctr;
            X_samp = refX_samp;
            samp = refsamp;
            ovsamp = ovrefsamp;

            for (sampctr = 0; sampctr < ns; sampctr++) {
               if ((dn = ov_imp_ptr[ovsamp-1]) == 0)
                  dn = lut_ptr[local_imp_ptr[samp-1]];
               else
                  dn = ovly_lut_ptr[dn];
               X_imp_ptr[X_samp] = dn;
               X_samp += X_Bytes_Per_Pixel;
               X_ctr++;

               if ((X_ctr - 1) % X_ZOOM(tempimp) == 0) {
                  samp++;
                  samp = (samp <= ZN_SAMPS) ? samp : 1;
               }
               if ((X_ctr - 1) % X_ZOOM(X_Overlay_Imp) == 0) {
                  ovsamp++;
                  ovsamp = (ovsamp <= ZN_SAMPS) ? ovsamp : 1;
               }

               if (X_ctr > ZN_SAMPS) {
                  X_ctr = 1;
                  X_samp = color_order[lut];
                  samp = X_DW_LEFT(tempimp);
                  ovsamp = X_DW_LEFT(X_Overlay_Imp);
               }
            }
         }
      }
      else {

         /*  Either the overlay plane isn't on or we haven't written  */
         /*  anything to it yet.                                      */

         for (linectr = 0; linectr < nl; linectr++) {
            local_imp_ptr = VRDI_IMPP(tempimp, 1, line);
            X_imp_ptr = X_IMPP(lut, 1, X_line);
            line++;
            line = (line <= ZN_LINES) ? line : 1;
            X_line++;
            if (X_line > ZN_LINES) {
               X_line = 1;
               line = X_DW_TOP(tempimp);
            }

            X_samp = refX_samp;
            samp = refsamp;
            for (sampctr = 0; sampctr < ns; ) {
               dn = local_imp_ptr[samp-1];
               samp++;
               samp = (samp <= ZN_SAMPS) ? samp : 1;

               for (xzoomctr = 0; xzoomctr < X_ZOOM(tempimp); xzoomctr++, sampctr++) {
                  X_imp_ptr[X_samp] = lut_ptr[dn];
                  X_samp += X_Bytes_Per_Pixel;
                  if (X_samp >= X_Bytes_Per_Line) {
                     X_samp = color_order[lut];
                     samp = X_DW_LEFT(tempimp);
                  }
               }
            }

            /* If we're zooming, duplicate the line we just wrote */

            for (yzoomctr = 1; yzoomctr < X_ZOOM(tempimp) && linectr < nl-1; yzoomctr++) {
               new_ptr = X_imp_ptr + (yzoomctr * X_Bytes_Per_Line);
               memcpy(new_ptr, X_imp_ptr, X_Bytes_Per_Line);
               X_line++;
               if (X_line > ZN_LINES) {
                  X_line = 1;
                  line = X_DW_TOP(tempimp);
               }
               linectr++;
            }
         }
      }
   }
}


refresh_icon(dpy, win)
Display *dpy;
MAIN_WINDOW *win;
{
   char buffer[80];
   int textwd;
   XWindowAttributes xwa;

   DPR(("x11_reconfig: entering refresh_icon routine\n"));

   XGetWindowAttributes(dpy, win->icon.main, &xwa);

   if (xwa.map_state != IsViewable)
      return;

   XGetWindowAttributes(dpy, win->main, &xwa);
   reconf_icon_imp(dpy, win, X_Display_Orig_X, X_Display_Orig_Y,
        xwa.width-BDR_WW, xwa.height-BDR_WW);

   DPR(("x11_reconfig: leaving refresh_icon routine\n"));

}


reconf_icon_imp(dpy, win, ss, sl, wd, ht)
Display *dpy;
MAIN_WINDOW *win;
int ss, sl, wd, ht;
{
   register int x, y, xim, yim;
   int imp, lut, es, el, bx, by, ex, ey, new_wd, new_ht;
   float scale, x_scale, y_scale;
   unsigned char dn;

   DPR(("x11_reconfig: reconfiguring the icon, ss=%d, sl=%d, wd=%d, ht=%d\n",
        ss, sl, wd, ht));

   es = ss + wd - 1;
   el = sl + ht - 1;

   /* use scale to translate ICON --> IMAGE coordinates */
   x_scale = (int) ((float) wd / ICON_WD + 0.5);
   y_scale = (int) ((float) ht / ICON_HT + 0.5);
   scale = MAX(x_scale, y_scale);

   new_wd = (int) ((float) ICON_WD * ((float) wd / (float) ZN_SAMPS) + 0.5);
   new_ht = (int) ((float) ICON_HT * ((float) ht / (float) ZN_LINES) + 0.5);

   DPR(("x11_reconfig: scale=%f, new_wd=%d, new_ht=%d\n", scale, new_wd, new_ht));

   if (ht == 0 || wd == 0)
      printf("Cannot reconfigure icon--null ht %d wd %d\n", ht, wd);

   DPR(("x11_reconfig: copying data from image to icon\n"));
   for (lut = 1; lut < ZN_IMPS; lut++) {
      imp = X_WHICH_IMP(lut);
      DPR(("x11_reconfig: lut=%d, imp=%d\n", lut, imp));
      for (y = 1; y <= ICON_HT; y++) {
         yim = (int) ((float) y * scale + 0.5) + sl;
         for (x = 1; x <= ICON_WD; x++) {
            xim = (int) ((float) x * scale + 0.5) + ss;
            if (xim > es || yim > el) {
               ICON_IMP(lut, x, y) = 0;
            }
            else {
               dn = X_IMP(imp, xim, yim);
               ICON_IMP(lut, x, y) = X_LUT(lut, dn);
            }
         }
      }
   }

   DPR(("x11_reconfig: putting the data out to the icon\n"));
   XPutImage(dpy, win->icon.main, icon_gc, icon_ximage, 0, 0, 0, 0,
        ICON_WD, ICON_HT);
   XFlush(dpy);
}
