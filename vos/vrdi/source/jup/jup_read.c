/*	JUP_Read
 *
 *	Purpose:	To read an area of, line of, or single pixel values
 *			from the Jupiter J-Station Display Device
 *
 *	Written by:	Fred Burnette
 *	Date:		December 1989
 *
 *	Calling Sequence:
 *
 *		STATUS = JUP_Read( parameters )
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
 *
 *	Possible Error Codes:
 *
 */

/************************************************************************/
/* Some games are played here for efficiency.  The j_rd_image routine	*/
/* can deal with any pixel size, but the buffer has to be shortword	*/
/* aligned and you can only transfer multiples of two (or four) pixels.	*/
/* The j_crd_image routine can transfer any number of bytes at any	*/
/* alignment, but it can only handle 8-bit pixels.  To get around this,	*/
/* some tricks are played.  We always read in 8 bits (possibly from	*/
/* more than one image plane), then the extra bits are masked out.	*/
/* Note that for overlays, we want the low-order 4 bits of the buffer	*/
/* to be filled up, while for images, we want the upper 4 bits (if it's	*/
/* only a 4-bit image plane).						*/
/************************************************************************/

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "jupinc.h"

FUNCTION JUP_Read(Unit,Imp,Size,Top,Left,Bottom,Right,Buffer)

int *Unit, Imp, Size, Top, Left, Bottom, Right;
BYTE Buffer;

{
   int i;
   int JUP_x, JUP_y, JUP_width, JUP_height, JUP_size;
   int startbit;
   struct jggxattr ja;

   JUP_x = JUP_X_IMG(Left);
   JUP_y = JUP_Y_IMG(Top);

   JUP_width = Right - Left + 1;
   JUP_height = Bottom - Top + 1;
   JUP_size = MIN(Size, JUP_width * JUP_height);

   if (Imp == OVERLAY_IMP && OVERLAY_AVAILABLE)
   {

      j_bdepth(ID_OV,0,8);
      j_c_area(JUP_x,JUP_y,JUP_x+JUP_width-1,JUP_y-JUP_height+1);
      j_crd_image(Buffer,JUP_size);
      jfflush(J12file);

      for (i=0; i<JUP_size; i++)
         Buffer[i] &= 0x0F;		/* Return low 4 bits only */
   }
   else			/* Image plane */
   {

      i = j_get_attr(&ja);

      if (ja.ja_fb == CLT4 || ja.ja_fb == RGB12)
      {
         if (Imp == 1)		/* Gotta play different tricks with imp 1 */
            startbit = 0;
         else
            startbit = (Imp-2) * 4;
      }
      else
         startbit = (Imp-1) * 8;	/* 8-bit image planes */

      j_bdepth(ID_BUF1, startbit, 8);
      j_c_area(JUP_x,JUP_y,JUP_x+JUP_width-1,JUP_y-JUP_height+1);
      j_crd_image(Buffer, JUP_size);
      jfflush(J12file);

      if (ja.ja_fb == CLT4 || ja.ja_fb == RGB12)	/* 4-bit planes */
      {
         if (Imp == 1)
            for (i=0; i<JUP_size; i++)
               Buffer[i] = (Buffer[i] & 0x0F) << 4;
         else
            for (i=0; i<JUP_size; i++)
               Buffer[i] &= 0xF0;		/* Return high 4 bits only */
      }
   }

   return SUCCESS;
}
