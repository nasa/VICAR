/* SCCS %W% %G%  %Q% */
/******************************************************************************/
/*                                                                            */
/*  File: x11_color.c                                                         */
/*                                                                            */
/*  Routines:                                                                 */
/*                                                                            */
/*	init_luts(dpy)                                                        */
/*		Display *dpy;                                                 */
/*                                                                            */
/*	set_colormap(dpy, mode)                                               */
/*		Display *dpy;                                                 */
/*              int mode;                                                     */
/*                                                                            */
/*	set_overlay_colormap(dpy)                                             */
/*		Display *dpy;                                                 */
/*                                                                            */
/*	lock_colormap(dpy, win, mandate)                                      */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		int mandate;                                                  */
/*                                                                            */
/*	unlock_colormap(dpy, win, mandate)                                    */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*		int mandate;                                                  */
/*                                                                            */
/*	set_7bit_mode(dpy, win)                                               */
/*		Display *dpy;                                                 */
/*		MAIN_WINDOW *win;                                             */
/*                                                                            */
/*	set_8bit_mode(dpy, win)                                               */
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
#include <stdio.h>
#if UNIX_OS
#include <unistd.h>
#endif
#include "xdexterns.h"
#include "x11_device_main.h"

#if VMS_OS
#define R_OK	4		/* why doesn't VMS do this?? */
#endif

/* #define DEBUG */
#ifdef DEBUG
#define DPR(A)  {printf A; fflush(stdout);}
#else
#define DPR(A)
#endif

#define NO_CMAP_FILE	-1

extern int unit;
extern Colormap colormap, default_cmap;
extern Pixmap HardPixmap, SoftPixmap, EightBitPixmap, SevenBitPixmap;
extern long fg, bg;
extern XColor fg_rgb, default_colors[];
extern unsigned long Base_Pixel, Plane_Mask[], Overlay_Pixels[];
XColor x_colors[X_LUT_SIZE];


init_luts(dpy)
Display *dpy;
{
   int imp, lut, i, j, maxval, red_dn, green_dn, blue_dn;
   unsigned char dn;

   DPR(("x11_color: initializing LUTs\n"));
   for (lut = 1, imp = 1; lut <= ZN_LUTS; lut++) {
      for (i = 0; i < X_LUT_SIZE; i++) {
         if (X_Bit_Mode == X_7BIT_MODE)
            dn = (unsigned char) ((i >> 1) | 0x80);
         else
            dn = (unsigned char) i;
         VRDI_LUT(lut, i) = (unsigned char) i;
         X_LUT(lut, i) = dn;
      }
      X_WHICH_IMP(lut) = imp;
      if (X_Image_Depth == 24)
         imp++;
   }

   DPR(("x11_color: LUTs initialized.  Initializing overlay LUTs\n"));
   if (X_Image_Depth == 24) {
      for (i = 0; i < X_Overlay_Size; i++) {
         red_dn = 0;
         green_dn = 0;
         blue_dn = 0;
         if (i & 0x01)					/* red */
            red_dn += 255;
         if (i & 0x02)					/* green */
            green_dn += 255;
         if (i & 0x04)					/* blue */
            blue_dn += 255;
         if (i & 0x08) {				/* white */
            red_dn += 255;
            green_dn += 255;
            blue_dn += 255;
         }
         if (i & 0x10) {				/* magenta */
            red_dn += 255;
            blue_dn += 255;
         }
         if (i & 0x20) {				/* yellow */
            red_dn += 255;
            green_dn += 255;
         }
         if (i & 0x40) {				/* cyan */
            green_dn += 255;
            blue_dn += 255;
         }
         maxval = MAX(red_dn, MAX(green_dn, blue_dn));
         if (maxval > 255) {
            red_dn *= ((double) 255 / (double) maxval);
            green_dn *= ((double) 255 / (double) maxval);
            blue_dn *= ((double) 255 / (double) maxval);
         }
         if (i & 0x80) {				/* black */
            red_dn /= 2;
            green_dn /= 2;
            blue_dn /= 2;
         }
         VRDI_OVLY_LUT(X_RED, i) = red_dn;
         VRDI_OVLY_LUT(X_GREEN, i) = green_dn;
         VRDI_OVLY_LUT(X_BLUE, i) = blue_dn;
         X_OVLY_LUT(X_RED, i) = red_dn;
         X_OVLY_LUT(X_GREEN, i) = green_dn;
         X_OVLY_LUT(X_BLUE, i) = blue_dn;
      }
   }
   else {
      for (i = 0; i < X_Overlay_Size; i++) {
         red_dn = 0;
         green_dn = 0;
         blue_dn = 0;
         if (i & 0x01)					/* red */
            red_dn += 255;
          if (i & 0x02)					/* green */
            green_dn += 255;
         if (i & 0x04)					/* blue */
            blue_dn += 255;
         if (i & 0x08) {
            red_dn /= 2;
            green_dn /= 2;
            blue_dn /= 2;
         }
         VRDI_OVLY_LUT(X_RED, i) = red_dn;
         VRDI_OVLY_LUT(X_GREEN, i) = green_dn;
         VRDI_OVLY_LUT(X_BLUE, i) = blue_dn;
         X_OVLY_LUT(X_RED, i) = red_dn;
         X_OVLY_LUT(X_GREEN, i) = green_dn;
         X_OVLY_LUT(X_BLUE, i) = blue_dn;
      }
   }
   DPR(("x11_color: Overlay LUTs initialized.\n"));
   if (X_Lut_RW)
      set_colormap(dpy, X_RAMP_LUTS);
}


set_colormap(dpy, mode)
Display *dpy;
int mode;
{
   int i, j, lut;
   unsigned char dn;

   if (!X_Lut_RW)
      return;

   DPR(("x11_color: Initializing hardware LUTs\n"));
   if (X_Image_Depth == 24) {
      if (mode == X_USE_SW_LUTS) {
         DPR(("x11_color: depth=24, setting hw luts from sw luts\n"));
         for (i = 0; i < X_LUT_SIZE; i++) {
            dn = VRDI_LUT(X_RED, i);
            x_colors[i].red = ((unsigned short) dn << 8);
            dn = VRDI_LUT(X_GREEN, i);
            x_colors[i].green = ((unsigned short) dn << 8);
            dn = VRDI_LUT(X_BLUE, i);
            x_colors[i].blue = ((unsigned short) dn << 8);
            x_colors[i].flags = DoRed | DoGreen | DoBlue;
            x_colors[i].pixel = (i<<16) | (i<<8) | i;
         }
      }
      else {
         DPR(("x11_color: depth=24, setting hw luts to a ramp\n"));
         for (i = 0; i < X_LUT_SIZE; i++) {
            x_colors[i].red = ((unsigned short) i << 8);
            x_colors[i].green = ((unsigned short) i << 8);
            x_colors[i].blue = ((unsigned short) i << 8);
            x_colors[i].flags = DoRed | DoGreen | DoBlue;
            x_colors[i].pixel = (i<<16) | (i<<8) | i;
         }
      }
   }
   else {
      if (mode == X_USE_SW_LUTS) {
         if (X_Bit_Mode == X_7BIT_MODE) {
            DPR(("x11_color: depth=7, setting hw luts from sw luts\n"));
            for (i = 0; i < X_LUT_SIZE/2; i++) {
               x_colors[i].red = default_colors[i].red;
               x_colors[i].green = default_colors[i].green;
               x_colors[i].blue = default_colors[i].blue;
               x_colors[i].flags = default_colors[i].flags;
               x_colors[i].pixel = default_colors[i].pixel;
            }
            for (i = 0, j = X_LUT_SIZE/2; j < X_LUT_SIZE; j++, i += 2) {
               dn = VRDI_LUT(X_RED, i);
               x_colors[j].red = ((unsigned short) dn << 8);
               dn = VRDI_LUT(X_GREEN, i);
               x_colors[j].green = ((unsigned short) dn << 8);
               dn = VRDI_LUT(X_BLUE, i);
               x_colors[j].blue = ((unsigned short) dn << 8);
               x_colors[j].flags = DoRed | DoGreen | DoBlue;
               x_colors[j].pixel = j;
            }
         }
         else {
            DPR(("x11_color: depth=8, setting hw luts from sw luts\n"));
            for (i = 0; i < X_LUT_SIZE; i++) {
               dn = VRDI_LUT(X_RED, i);
               x_colors[i].red = ((unsigned short) dn << 8);
               dn = VRDI_LUT(X_GREEN, i);
               x_colors[i].green = ((unsigned short) dn << 8);
               dn = VRDI_LUT(X_BLUE, i);
               x_colors[i].blue = ((unsigned short) dn << 8);
               x_colors[i].flags = DoRed | DoGreen | DoBlue;
               x_colors[i].pixel = i;
            }
         }
      }
      else {
         if (X_Bit_Mode == X_7BIT_MODE) {
            DPR(("x11_color: depth=7, setting hw luts to a ramp\n"));
            for (i = 0; i < X_LUT_SIZE/2; i++) {
               x_colors[i].red = default_colors[i].red;
               x_colors[i].green = default_colors[i].green;
               x_colors[i].blue = default_colors[i].blue;
               x_colors[i].flags = default_colors[i].flags;
               x_colors[i].pixel = default_colors[i].pixel;
            }
            for (i = 0, j = X_LUT_SIZE/2; j < X_LUT_SIZE; j++, i += 2) {
               x_colors[j].red = ((unsigned short) i << 8);
               x_colors[j].green = ((unsigned short) i << 8);
               x_colors[j].blue = ((unsigned short) i << 8);
               x_colors[j].flags = DoRed | DoGreen | DoBlue;
               x_colors[j].pixel = j;
            }
         }
         else {
            DPR(("x11_color: depth=8, setting hw luts to a ramp\n"));
            for (i = 0; i < X_LUT_SIZE; i++) {
               x_colors[i].red = ((unsigned short) i << 8);
               x_colors[i].green = ((unsigned short) i << 8);
               x_colors[i].blue = ((unsigned short) i << 8);
               x_colors[i].flags = DoRed | DoGreen | DoBlue;
               x_colors[i].pixel = i;
            }
         }
      }
   }

   DPR(("Setting hardware LUTs\n"));
   if (X_Use_System_Cmap)
      XStoreColors(dpy, default_cmap, &x_colors[X_LUT_SIZE/2], X_LUT_SIZE/2);
   XStoreColors(dpy, colormap, x_colors, X_LUT_SIZE);

   if (X_Image_Depth == 8)
      set_overlay_colormap(dpy);
}


set_overlay_colormap(dpy)
Display *dpy;
{
   int i, index;
   unsigned char dn;

   if (X_Image_Depth == 24)
      return (SUCCESS);

   DPR(("x11_color: entering set_overlay_colormap routine\n"));

   if (X_Bit_Mode == X_7BIT_MODE) {
      if (X_Use_System_Overlay_Cmap) {
         DPR(("x11_color: initializing overlay lut in system colormap\n"));
         for (i = 0; i < X_Overlay_Size; i++) {
            X_OVLY_LUT(X_RED, i) = Overlay_Pixels[i];
            X_OVLY_LUT(X_GREEN, i) = Overlay_Pixels[i];
            X_OVLY_LUT(X_BLUE, i) = Overlay_Pixels[i];
            x_colors[i].red = ((unsigned short) VRDI_OVLY_LUT(X_RED, i) << 8);
            x_colors[i].green = ((unsigned short) VRDI_OVLY_LUT(X_GREEN, i) << 8);
            x_colors[i].blue = ((unsigned short) VRDI_OVLY_LUT(X_BLUE, i) << 8);
            x_colors[i].flags = DoRed | DoGreen | DoBlue;
            x_colors[i].pixel = Overlay_Pixels[i];
         }
         XStoreColors(dpy, default_cmap, x_colors, X_Overlay_Size);
         XStoreColors(dpy, colormap, x_colors, X_Overlay_Size);
      }
      else {
         DPR(("x11_color: initializing overlay lut in image colormap\n"));
         index = X_LUT_SIZE/2 - X_Overlay_Size;
         for (i = 0; i < X_Overlay_Size; i++) {
            X_OVLY_LUT(X_RED, i) = index + i;
            X_OVLY_LUT(X_GREEN, i) = index + i;
            X_OVLY_LUT(X_BLUE, i) = index + i;
            x_colors[i].red = ((unsigned short) VRDI_OVLY_LUT(X_RED, i) << 8);
            x_colors[i].green = ((unsigned short) VRDI_OVLY_LUT(X_GREEN, i) << 8);
            x_colors[i].blue = ((unsigned short) VRDI_OVLY_LUT(X_BLUE, i) << 8);
            x_colors[i].flags = DoRed | DoGreen | DoBlue;
            x_colors[i].pixel = index + i;
         }
         XStoreColors(dpy, colormap, x_colors, X_Overlay_Size);
      }
   }
   else {
      for (i = 0; i < X_Overlay_Size; i++) {
         dn = MAX((int)VRDI_OVLY_LUT(X_RED,i),
              MAX((int)VRDI_OVLY_LUT(X_GREEN,i), (int)VRDI_OVLY_LUT(X_BLUE,i)));
         X_OVLY_LUT(X_RED, i) = dn;
         X_OVLY_LUT(X_GREEN, i) = dn;
         X_OVLY_LUT(X_BLUE, i) = dn;
      }
   }
   return (SUCCESS);
}


lock_colormap(dpy, win)
Display *dpy;
MAIN_WINDOW *win;
{
   int i, lut, luts_ramped=TRUE, something_there=FALSE;
   unsigned char *temp, dn;

   if (X_Image_Depth != 24) {
      if (check_display_mode() != BLACK_AND_WHITE) {
         soft_error(dpy, win, "May not use software LUTs in pseudo-color mode.");
         return;
      }
   }

   X_Color_Locked = TRUE;
   win->lock_pix = SoftPixmap;
   redraw_button(dpy, win->lock, win->lock_pix);

   /* check current lut status--if the luts are ramped, we're done */
   for (lut = 1; lut <= ZN_LUTS && luts_ramped; lut++) {
      temp = VRDI_LUTP(lut, 0);
      for (i = 0; i < X_LUT_SIZE && luts_ramped; i++) {
         if (temp[i] != (unsigned char) i)
            luts_ramped = FALSE;
       }
   }

   if (!luts_ramped) {
      /* ramp hardware luts */
      DPR(("x11_color: using SW luts--luts were not ramped\n"));
      if (X_Bit_Mode == X_7BIT_MODE) {
         for (i = 0; i < X_LUT_SIZE; i++) {
            X_LUT(X_RED, i) = (VRDI_LUT(X_RED, i) >> 1) | 0x80;
            X_LUT(X_GREEN, i) = (VRDI_LUT(X_GREEN, i) >> 1) | 0x80;
            X_LUT(X_BLUE, i) = (VRDI_LUT(X_BLUE, i) >> 1) | 0x80;
         }
      }
      else {
         for (i = 0; i < X_LUT_SIZE; i++) {
            X_LUT(X_RED, i) = VRDI_LUT(X_RED, i);
            X_LUT(X_GREEN, i) = VRDI_LUT(X_GREEN, i);
            X_LUT(X_BLUE, i) = VRDI_LUT(X_BLUE, i);
         }
      }
      set_colormap(dpy, X_RAMP_LUTS);

      for (i = 1; i <= ZN_IMPS; i++) {
         if (X_WRITTEN(i))
            something_there = TRUE;
      }

      if (something_there) {
         DPR(("x11_color: image planes being redrawn\n"));
         compile_image(1, 1, ZN_SAMPS, ZN_LINES, X_ALL_IMPS);
         reconf_imp(dpy, win, 1, 1, ZN_SAMPS, ZN_LINES);
      }
   }
   else
      DPR(("x11_color: software luts were ramped--no need to redraw\n"));
}


unlock_colormap(dpy, win)
Display *dpy;
MAIN_WINDOW *win;
{
   int i, lut, luts_ramped=TRUE, something_there=FALSE;
   unsigned char *temp, dn;

   DPR(("x11_color: unlocking color map\n"));

   if (!X_Lut_RW) {
      soft_error(dpy, win, "Hardware lookup tables may not be modified on this display\n");
      return;
   }

   X_Color_Locked = FALSE;
   win->lock_pix = HardPixmap;
   redraw_button(dpy, win->lock, win->lock_pix);

   /* check current lut status--if the luts are ramped, we're done */
   for (lut = 1; lut <= ZN_LUTS && luts_ramped; lut++) {
      temp = VRDI_LUTP(lut, 0);
      for (i = 0; i < X_LUT_SIZE && luts_ramped; i++) {
         if (temp[i] != (unsigned char) i)
            luts_ramped = FALSE;
      }
   }

   if (!luts_ramped) {
      DPR(("x11_color: using HW luts--luts were not ramped\n"));
      for (i = 0; i < X_LUT_SIZE; i++) {
         if (X_Bit_Mode == X_7BIT_MODE)
            dn = (unsigned char) ((i >> 1) | 0x80);
         else
            dn = (unsigned char) i;
         X_LUT(X_RED, i) = dn;
         X_LUT(X_GREEN, i) = dn;
         X_LUT(X_BLUE, i) = dn;
      }

      /* Set hardware luts from software luts */
      set_colormap(dpy, X_USE_SW_LUTS);

      for (i = 1; i <= ZN_IMPS; i++) {
         if (X_WRITTEN(i))
            something_there = TRUE;
      }

      if (something_there) {
         DPR(("x11_color: image planes being redrawn\n"));
         compile_image(1, 1, ZN_SAMPS, ZN_LINES, X_ALL_IMPS);
         reconf_imp(dpy, win, 1, 1, ZN_SAMPS, ZN_LINES);
      }
   }
   else
      DPR(("x11_color: software luts were ramped--no need to redraw\n"));

   /* need to do something here about the overlay luts? */
}


set_7bit_mode(dpy, win)
Display *dpy;
MAIN_WINDOW *win;
{
   int i;
   unsigned char dn;

   DPR(("x11_color: setting 7-bit mode\n"));
   X_Bit_Mode = X_7BIT_MODE;
   win->bit_mode_pix = SevenBitPixmap;
   redraw_button(dpy, win->bit_mode, win->bit_mode_pix);
   if (X_Color_Locked) {
      for (i = 0; i < X_LUT_SIZE; i++) {
         X_LUT(X_RED, i) = (VRDI_LUT(X_RED, i) >> 1) | 0x80;
         X_LUT(X_GREEN, i) = (VRDI_LUT(X_GREEN, i) >> 1) | 0x80;
         X_LUT(X_BLUE, i) = (VRDI_LUT(X_BLUE, i) >> 1) | 0x80;
      }
   }
   else {
      for (i = 0; i < X_LUT_SIZE; i++) {
         dn = (unsigned char) ((i >> 1) | 0x80);
         X_LUT(X_RED, i) = dn;
         X_LUT(X_GREEN, i) = dn;
         X_LUT(X_BLUE, i) = dn;
      }
   }
   if (X_Color_Locked)
      set_colormap(dpy, X_RAMP_LUTS);
   else
      set_colormap(dpy, X_USE_SW_LUTS);
   compile_image(1, 1, ZN_SAMPS, ZN_LINES, X_ALL_IMPS);
   reconf_imp(dpy, win, 1, 1, ZN_SAMPS, ZN_LINES);
}


set_8bit_mode(dpy, win)
Display *dpy;
MAIN_WINDOW *win;
{
   int i;

   DPR(("x11_color: setting 8-bit mode\n"));
   X_Bit_Mode = X_8BIT_MODE;
   win->bit_mode_pix = EightBitPixmap;
   redraw_button(dpy, win->bit_mode, win->bit_mode_pix);
   if (X_Color_Locked) {
      for (i = 0; i < X_LUT_SIZE; i++) {
         X_LUT(X_RED, i) = VRDI_LUT(X_RED, i);
         X_LUT(X_GREEN, i) = VRDI_LUT(X_GREEN, i);
         X_LUT(X_BLUE, i) = VRDI_LUT(X_BLUE, i);
      }
   }
   else {
      for (i = 0; i < X_LUT_SIZE; i++) {
         X_LUT(X_RED, i) = (unsigned char) i;
         X_LUT(X_GREEN, i) = (unsigned char) i;
         X_LUT(X_BLUE, i) = (unsigned char) i;
      }
   }
   if (X_Color_Locked)
      set_colormap(dpy, X_RAMP_LUTS);
   else
      set_colormap(dpy, X_USE_SW_LUTS);
   compile_image(1, 1, ZN_SAMPS, ZN_LINES, X_ALL_IMPS);
   reconf_imp(dpy, win, 1, 1, ZN_SAMPS, ZN_LINES);
}
