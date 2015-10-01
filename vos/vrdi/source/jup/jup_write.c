/*	JUP_Write
 *
 *	Purpose:	To write an area of, line of, or single pixel values
 *			to the Jupiter J-Station Display Device
 *
 *	Written by:	Fred Burnette
 *	Date:		December 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = JUP_Write( parameters )
 *
 *	Parameter List:
 *
 *		Unit:		Display device unit number
 *		Imp:		Image Memory Plane
 *		Size:		Number of bytes in buffer
 *		Top:		Coordinates of area to read/write
 *		Left:
 *		Bottom:
 *		Right:
 *		Buffer:		Data buffer
 *		Mask:		Write mask
 *
 *	Possible Error Codes:
 *
 */

/************************************************************************/
/* Some games are played here for efficiency.  The j_wrt_image routine	*/
/* can deal with any pixel size, but the buffer has to be shortword	*/
/* aligned and you can only transfer multiples of two (or four) pixels.	*/
/* The j_cwrt_image routine can transfer any number of bytes at any	*/
/* alignment, but it can only handle 8-bit pixels.  To get around this,	*/
/* some tricks are played.  In general, write 8-bit pixels always,	*/
/* using write masks to mask out the parts we don't want to modify.	*/
/* (When "mask" is mentioned below, the given pattern is actually	*/
/* combined with the "Mask" parameter, of course).			*/
/* Note that for overlays, we want the low-order 4 bits of the buffer to*/
/* go, while for images, we want the upper 4 bits (if only 4 allowed).	*/
/*									*/
/* Overlay:  Write 8 bits, the write mask is set to 0x0F, so only the	*/
/*    low order 4 bits of the buffer are written.			*/
/* 8-bit planes:  This is easy.  No weird stuff.			*/
/* 4-bit planes 2 or 3:  We actually write planes {1 and 2} for 2, and	*/
/*    {2/3} for 3, with the write mask set to 0xF0.  So, for plane 3,	*/
/*    the high order nibble of the buffer goes to plane 3; the low	*/
/*    order would go to plane 2 except it's inhibited by the write mask.*/
/* 4-bit plane 1:  This is trickier since there's no plane 0 for the	*/
/*    low-order nibble to go to.  So, we copy the buffer, shifting left	*/
/*    by 4, and write planes {1/2} with a mask of 0x0F, thereby only	*/
/*    modifying plane 1.						*/
/************************************************************************/

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "jupinc.h"

FUNCTION JUP_Write(Unit,Imp,Size,Top,Left,Bottom,Right,Buffer,Mask)

int *Unit, Imp, Size, Top, Left, Bottom, Right;
BYTE Buffer;
unsigned char Mask;

{
   int i;
   int JUP_x, JUP_y, JUP_width, JUP_height, JUP_size;
   int vmask;
   int startbit;
   int allocated;
   unsigned char *buf;
   struct jggxattr ja;

   allocated = FALSE;

   JUP_x = JUP_X_IMG(Left);
   JUP_y = JUP_Y_IMG(Top);

   JUP_width = Right - Left + 1;
   JUP_height = Bottom - Top + 1;
   JUP_size = MIN(Size, JUP_width * JUP_height);

   if (Imp == OVERLAY_IMP && OVERLAY_AVAILABLE)
   {

      j_32wmask(0x000F);
      j_bdepth(ID_OV,0,8);
      j_c_area(JUP_x,JUP_y,JUP_x+JUP_width-1,JUP_y-JUP_height+1);
      j_cwrt_image(Buffer,JUP_size);
      jfflush(J12file);

      j_32wmask(0xFFFFFFFF);
      jfflush(J12file);

   }
   else			/* Image plane */
   {

      i = j_get_attr(&ja);

      if (ja.ja_fb == CLT4 || ja.ja_fb == RGB12)
      {
         if (Imp == 1)		/* can't play tricks with imp 1, so copy it */
         {
            buf = malloc(JUP_size);
            allocated = TRUE;
            if (buf == NULL)
            {
               if (Top != Bottom)
                  return DEVICE_CANNOT_DO_IT;	/* let linewrite take over */
               else
                  return MEMORY_ERROR;	/* this is linewrite, give true error */
            }
            for (i=0; i<JUP_size; i++)
               buf[i] = Buffer[i] >> 4;
            vmask = 0x000F & (Mask >> 4);
            startbit = 0;
         }
         else			/* Not plane 1, play tricks */
         {
            startbit = (Imp-2) * 4;
            vmask = ((Mask & 0xF0) << startbit);
            buf = Buffer;
         }
      }
      else				/* We have 8-bit planes */
      {
         startbit = (Imp-1) * 8;
         vmask = Mask << ((Imp-1)*8);
         buf = Buffer;
      }

      j_32wmask(vmask);
      j_bdepth(ID_BUF1, startbit, 8);
      j_c_area(JUP_x,JUP_y,JUP_x+JUP_width-1,JUP_y-JUP_height+1);
      j_cwrt_image(buf, JUP_size);
      jfflush(J12file);

      j_32wmask(0xFFFFFFFF);
      jfflush(J12file);
   }

   if (allocated)
      free(buf);

   return SUCCESS;
}

