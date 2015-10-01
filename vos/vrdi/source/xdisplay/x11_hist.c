/* SCCS %W% %G%  %Q% */
/******************************************************************************/
/*                                                                            */
/*  File:  x11_hist                                                           */
/*                                                                            */
/*  Routines:                                                                 */
/*                                                                            */
/*	initialize_histogram(dpy, win, visual, bw, vmask, attr)               */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		Visual *visual;                                               */
/*		int bw;                                                       */
/*		unsigned long int vmask;				      */
/*		XSetWindowAttributes *attr;				      */
/*                                                                            */
/*	histogram_toggle(dpy, win)                                            */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*                                                                            */
/*	refresh_histogram(dpy, win, imp)                                      */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		int imp;                                                      */
/*                                                                            */
/*	collect_histogram(win, imp)                                           */
/*		MAIN_WINDOW *win;                                             */
/*		int imp;                                                      */
/*                                                                            */
/*	collect_str_histogram(win, imp)                                       */
/*		MAIN_WINDOW *win;                                             */
/*		int imp;                                                      */
/*                                                                            */
/*	int find_bin_value_for_spike(histogram, spike)                        */
/*		int histogram[];                                              */
/*		int spike;                                                    */
/*                                                                            */
/*	adjust_spike(dpy, win, incr, plane)                                   */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		int incr, plane;                                              */
/*                                                                            */
/*	draw_hist(dpy, win, gc, hist, max_hist, sx, sy, ht, wd)               */
/*		Display *dpy;                                                 */
/*		Window win;                                                   */
/*		GC gc;                                                        */
/*		int hist[], max_hist;                                         */
/*		int sx, sy, ht, wd;                                           */
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
#include <X11/Xutil.h>
#include <X11/Xatom.h>

/* #define DEBUG */
#ifdef DEBUG
#define DPR(A)  {printf A; fflush(stdout);}
#else
#define DPR(A)
#endif

#define BDR   20

extern int unit;
extern Atom wm_proto, wm_del_win;
extern XColor fg_rgb;
extern long bg;
extern Colormap default_cmap;

int fonty;


initialize_histogram(dpy, win)
Display *dpy;
MAIN_WINDOW *win;
{
   XColor red_fg, green_fg, blue_fg, white_bg, dummy_fg;
   int status, bw = 1, imp;
   unsigned long gc_mask;
   XGCValues gc_values;
   XSizeHints size_hints;
   XWMHints wm_hints;
   XFontStruct *hist_font;
   char hist_name[40];

   win->hist[RED_HIST].main = None;
   win->hist[GREEN_HIST].main = None;
   win->hist[BLUE_HIST].main = None;

   hist_font = XLoadQueryFont(dpy, "*-courier-medium-r-normal--*-60-*");
   if (hist_font == NULL) {
      hist_font = XLoadQueryFont(dpy, "5x8");
      if (hist_font == NULL)
         error(dpy, win, "Cannot open histogram font");
   }
   fonty = hist_font->ascent;

   status = XAllocNamedColor(dpy, default_cmap, "red", &red_fg, &dummy_fg);
   if (!status)
      red_fg = fg_rgb;
   if (X_Image_Depth == 24) {
      status = XAllocNamedColor(dpy, default_cmap, "forest green", &green_fg, &dummy_fg);
      if (!status)
         green_fg = fg_rgb;
      status = XAllocNamedColor(dpy, default_cmap, "blue", &blue_fg, &dummy_fg);
      if (!status)
         blue_fg = fg_rgb;
   }
   status = XAllocNamedColor(dpy, default_cmap, "white", &white_bg, &dummy_fg);
   if (!status)
      white_bg.pixel = bg;

   DPR(("x11_hist: hist_fontid = %d, fonty=%d\n", (int) hist_font->fid, fonty));
   gc_mask = GCFont | GCForeground | GCBackground;
   gc_values.font = hist_font->fid;
   gc_values.background = white_bg.pixel;

   size_hints.flags = USPosition | USSize | PMinSize | PMaxSize | PResizeInc;
   size_hints.width = HIST_WIN_W;
   size_hints.height = HIST_WIN_H;
   size_hints.min_width = HIST_WIN_W;
   size_hints.max_width = X_Max_Width;
   size_hints.min_height = HIST_WIN_H;
   size_hints.max_height = X_Max_Height;
   size_hints.width_inc = 1;
   size_hints.height_inc = 1;

   wm_hints.flags = StateHint | InputHint;
   wm_hints.initial_state = NormalState;
   wm_hints.input = True;

   for (imp = 1; imp < ZN_IMPS; imp++) {
      win->hist[imp-1].main = XCreateSimpleWindow(dpy, win->root, imp*30, imp*30,
           HIST_WIN_W, HIST_WIN_H, bw, white_bg.pixel, white_bg.pixel);
      if (win->hist[imp-1].main == 0)
         error(dpy, win, "cannot create histogram window");
      win->hist[imp-1].spike_win = XCreateSimpleWindow(dpy,
           win->hist[imp-1].main, BDR_WW, HIST_WIN_H-2*BDR_WW,
           HIST_WIN_W-2*BDR_WW, BDR_WW, bw, white_bg.pixel, white_bg.pixel);
      win->hist[imp-1].spike = 1;
      win->hist[imp-1].str_spike = 1;
      XChangeProperty(dpy, win->hist[imp-1].main, wm_proto, XA_ATOM, 32,
           PropModeReplace, (unsigned char *)&wm_del_win, 1);
      switch (imp) {
         case X_RED:
            gc_values.foreground = red_fg.pixel;
            sprintf(hist_name, "%s: Red Plane Histogram", win->dev);
            break;
         case X_GREEN:
            gc_values.foreground = green_fg.pixel;
            sprintf(hist_name, "%s: Green Plane Histogram", win->dev);
            break;
         case X_BLUE:
            gc_values.foreground = blue_fg.pixel;
            sprintf(hist_name, "%s: Blue Plane Histogram", win->dev);
            break;
      }
      win->hist[imp-1].hist_gc = XCreateGC(dpy, win->hist[imp-1].main, gc_mask, 
           &gc_values);
      if (win->hist[imp-1].hist_gc == 0)
         error(dpy, win, "cannot create histogram graphics context");

      XSetWMHints(dpy, win->hist[imp-1].main, &wm_hints);
      size_hints.x = imp*30;
      size_hints.y = imp*30;
      XSetNormalHints(dpy, win->hist[imp-1].main, &size_hints);
      XStoreName(dpy, win->hist[imp-1].main, hist_name);
      XSelectInput(dpy, win->hist[imp-1].main,
           ExposureMask | StructureNotifyMask | SubstructureNotifyMask | ButtonPressMask);

   }
   win->hist[RED_HIST].histogram_on = FALSE;
   win->hist[GREEN_HIST].histogram_on = FALSE;
   win->hist[BLUE_HIST].histogram_on = FALSE;
}


histogram_toggle(dpy, win)
Display *dpy;
MAIN_WINDOW *win;
{
   int imp;

   DPR(("x11_hist: toggling histogram--"));
   if (win->hist[RED_HIST].histogram_on == TRUE ||
        win->hist[GREEN_HIST].histogram_on == TRUE ||
        win->hist[BLUE_HIST].histogram_on == TRUE) {
      DPR(("setting it FALSE\n"));
      for (imp = 1; imp < ZN_IMPS; imp++) {
         if (win->hist[imp-1].histogram_on) {
            XUnmapWindow(dpy, win->hist[imp-1].main);
            win->hist[imp-1].histogram_on = FALSE;
         }
      }
   }
   else {
      DPR(("setting it TRUE\n"));
      for (imp = 1; imp < ZN_IMPS; imp++) {
         win->hist[imp-1].histogram_on = TRUE;
         XMapWindow(dpy, win->hist[imp-1].main);
         collect_histogram(win, imp);
         collect_str_histogram(win, imp);
      }
   }
}


refresh_histogram(dpy, win, imp)
Display *dpy;
MAIN_WINDOW *win;
int imp;
{
   int i;
   int border;		/* space on sides of histogram */
   int infoht;		/* height of info area */
   int histht;		/* height of each histogram */
   int histwd;		/* width of each histogram */
   int infoy;		/* starting y pos of info area */
   int hist1y;		/* starting y pos of main hist */
   int hist2y;		/* starting y pos of str. hist */
   XWindowAttributes att;
   char buffer[80];

   if (win->hist[imp-1].histogram_on == FALSE)
      return;

   DPR(("x11_hist: refreshing histogram %d\n", imp));

   XGetWindowAttributes(dpy, win->hist[imp-1].main, &att);
   border = 20;
   infoht = fonty + 10;
   histht = ((att.height - border*3) - infoht) / 2;
   histwd = att.width - border*2;
   infoy  = border + 10;
   hist1y = infoht + border;
   hist2y = infoht + border + histht + border;

   XClearWindow(dpy, win->hist[imp-1].main);
   sprintf(buffer, "Spike = %3d", win->hist[imp-1].spike);
   DPR(("x11_hist: spike=%d\n", win->hist[imp-1].spike));

   XDrawImageString(dpy, win->hist[imp-1].main, win->hist[imp-1].hist_gc,
        border, infoy, buffer, strlen(buffer));
   draw_hist(dpy, win->hist[imp-1].main, win->hist[imp-1].hist_gc,
        win->hist[imp-1].histogram, win->hist[imp-1].max_hist, border, hist1y,
        histht, histwd);
   draw_hist(dpy, win->hist[imp-1].main, win->hist[imp-1].hist_gc,
        win->hist[imp-1].str_histogram, win->hist[imp-1].max_str_hist, border,
        hist2y, histht, histwd);
   XFlush(dpy);
}


collect_histogram(win, imp)
MAIN_WINDOW *win;
int imp;
{
   int x, y;
   unsigned char *imp_ptr;

   if (win->hist[imp-1].histogram_on == FALSE)
      return;

   DPR(("x11_hist: collecting histogram %d\n", imp));
   memset(win->hist[imp-1].histogram, 0, X_LUT_SIZE*sizeof(int));

   for (y = 1; y <= ZN_LINES; y++) {
      imp_ptr = VRDI_IMPP(imp, 1, y);
      for (x = 1; x <= ZN_SAMPS; x++)
         win->hist[imp-1].histogram[imp_ptr[x-1]]++;
   }
   win->hist[imp-1].max_hist = find_bin_value_for_spike(win->hist[imp-1].histogram,
        win->hist[imp-1].spike);

   DPR(("x11_hist: max=%d\n", win->hist[imp-1].max_hist));
}


collect_str_histogram(win, imp)
MAIN_WINDOW *win;
int imp;
{
   int i;

   if (win->hist[imp-1].histogram_on == FALSE)
      return;

   DPR(("x11_hist: collecting stretch histogram %d\n", imp));
   memset(win->hist[imp-1].str_histogram, 0, X_LUT_SIZE*sizeof(int));

   for (i = 0; i < X_LUT_SIZE; i++)
      win->hist[imp-1].str_histogram[VRDI_LUT(imp, i)] += win->hist[imp-1].histogram[i];

   win->hist[imp-1].max_str_hist = find_bin_value_for_spike(win->hist[imp-1].str_histogram,
        win->hist[imp-1].str_spike);
}


int find_bin_value_for_spike(histogram, spike)
int histogram[];
int spike;
{
   register int i, j;
   register int ceiling, floor;
   register int tmp, bin_val;

   DPR(("x11_hist: entering find_bin_value_for_spike routine\n"));
   if (spike < X_LUT_SIZE/2) {
      ceiling = ZN_SAMPS * ZN_LINES + 1;
      for (i = 0; i < spike; i++) {
         bin_val = 0;
         for (j = 0; j < X_LUT_SIZE; j++) {
            tmp = histogram[j];
            if (tmp < ceiling  &&  tmp > bin_val)
               bin_val = tmp;
         }
         ceiling = bin_val;
      }
   }
   else {
      floor = 0;
      for (i = X_LUT_SIZE-1; i >= spike; i--) {
         bin_val = ZN_SAMPS * ZN_LINES + 1;
         for (j = 0; j < X_LUT_SIZE; j++) {
            if ((tmp = histogram[j]) > floor && tmp < bin_val)
               bin_val = tmp;
         }
         floor = bin_val;
      }
   }
   DPR(("x11_hist: finding bin value %d for spike %d\n", bin_val, spike));
   return bin_val;
}


adjust_spike(dpy, win, incr, plane)
Display *dpy;
MAIN_WINDOW *win;
int incr, plane;
{
   int i, spike, str_spike;

   spike = win->hist[plane-1].spike;
   str_spike = win->hist[plane-1].str_spike;

   DPR(("x11_hist: adjusting spike from %d to ", spike));
   if (incr == 0) {
      spike = 1;
      str_spike = 1;
   }
   else {
      spike += incr;
      str_spike += incr;
   }

   spike = MAX(MIN(spike, X_LUT_SIZE), 1);
   str_spike = MAX(MIN(str_spike, X_LUT_SIZE), 1);

   DPR(("%d using increment %d\n", spike, incr));
   if (win->hist[plane-1].spike == spike)
      return;

   win->hist[plane-1].spike = spike;
   win->hist[plane-1].str_spike = str_spike;

   win->hist[plane-1].max_hist = find_bin_value_for_spike(win->hist[plane-1].histogram, spike);
   win->hist[plane-1].max_str_hist = find_bin_value_for_spike(win->hist[plane-1].str_histogram, str_spike);
   refresh_histogram(dpy, win, plane);
}
 

draw_hist(dpy, win, gc, hist, max_hist, sx, sy, ht, wd)
Display *dpy;			/* display */
Window win;			/* window */
GC gc;				/* graphics context */
int hist[], max_hist;		/* histogram and maximal_value of the hist */
int sx, sy, ht, wd;		/* starting x, y  and height and width */
{
   register int i;	                  /* loop */
   register float bin_size;		  /* width of each hist. bin */
   register float scale;		  /* linear slope st x(i)=a+ix */
   int loc;				  /* location of hist ticks */
   int label_y;				  /* bottom of the labels */
   int hist_y;				  /* bottom of the hist */
   int hist_ht;				  /* height of the hist */
   int descent;				  /* length of the tick */
   XRectangle rect[X_LUT_SIZE];	  	  /* hist. bin rectangles */
   char buffer[8];			  /* used for hist label */

   DPR(("x11_hist: entering draw histogram routine, sx=%d, sy=%d, ht=%d, wd=%d\n",
        sx, sy, ht, wd));

   label_y = sy + ht;
   hist_y  = (sy + ht) - (fonty + 4);
   hist_ht = ht - (fonty + 4);
 
   bin_size = wd / (float) X_LUT_SIZE;

   XDrawLine(dpy, win, gc, sx, hist_y, sx + wd - 1, hist_y);

   for (i = 0; i <= 32; i++) {
      if (i != 32)
         loc = bin_size*i*(X_LUT_SIZE/32) + sx;
      else
         loc = bin_size*(X_LUT_SIZE-1) + sx;

      if ((i & 3) != 0)
         descent = 4;
      else
         descent = 8;

      XDrawLine(dpy, win, gc, loc, hist_y, loc, hist_y+descent);
      if ((i & 3) == 0) {
         if (i != 32)
            sprintf(buffer, "%d", i*(X_LUT_SIZE/32));
         else
            sprintf(buffer, "%d", (X_LUT_SIZE-1));
         XDrawImageString(dpy, win, gc, loc+2, label_y, buffer, strlen(buffer));
      }
   }

   if (max_hist > 0) {
      scale = (float) hist_ht / max_hist;
      for (i = 0; i < X_LUT_SIZE; i++) { 
         rect[i].width = (short)((i+1)*bin_size) - (short)(i*bin_size);
         rect[i].x = i*bin_size + sx; 
         rect[i].height = MIN(scale * hist[i], hist_ht);
         rect[i].y = hist_y - rect[i].height; 
      } 
      XFillRectangles(dpy, win, gc, rect, X_LUT_SIZE);
   }
}
