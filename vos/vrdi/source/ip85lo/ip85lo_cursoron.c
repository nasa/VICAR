/*	IP85LO_CursorOn - description
 *
 *	Purpose: Turn the display of a cursor on for the DeAnza display device
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *		STATUS = IP85LO_CursorOn(Unit, lun, cursor, form, rate)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		cursor:	Cursor number to turn on
 *		form:	Cursor form
 *			0 = default (same as 1)
 *			1 = 5 dots in a + shape
 *			2 = + shape
 *			3 = box with dot in center
 *			4 = + with three pixels at center missing (blinks to 5)
 *			5 = + formed with double lines and center missing
 *				(blink alternates with 4)
 *			6 = X shape
 *			7 = Very large box with large + inside
 *			8 = Full-screen two-pixel-wide crosshairs
 *		rate:	Cursor blink rate
 *			0 = no blink
 *			1-16 : fastest=1 (.26 sec), slowest=16 (4.2 sec)
 *
 *	Possible Error Codes:
 *		none
 *
 */
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "ip85lo.h"

globalvalue RCR, LCR, RR, LR, LPA;

FUNCTION IP85LO_CursorOn(Unit, lun, cursor, form, rate)
int *Unit;
short lun;
int cursor, form, rate;
{
int ratem1;
short vocdat = { 0x4300 };		/* VOC control register */
short reg;
short iosb[4];
int bit;

ip8qw(RCR, &lun, iosb, 0,0, &reg, &2, &0, &4);  /* read cursor control reg */

if (form == 0)
   form = 1;		/* default */

/* Set the cursor shape.  Forms 1-7 are programmable, 8 is hw full-screen */

if (cursor == 2)
   bit = 0x8000;		/* programmable enable bit */
else
   bit = 0x4000;

if (form <= 7) {
   ip85lo_write_cursor_array(cursor, lun, 1024, cursorshape[form-1]);
   reg |= bit;			/* enable programmable cursor */
}
else {
   if (cursor == 2)			/* enable X/Y for fixed form cursor */
      reg |= 0x0300;
   else
      reg |= 0x0030;
   reg &= 0xFFF0;			/* use form 0 */
   reg &= ~ bit;			/* disable programmable cursor */
}

ip8qw(LCR, &lun, iosb, 0,0, &reg, &2, &0, &4);	/* write cursor ctrl reg back */

/* Set blink rate.  Note that cursors 1 and 2 have the SAME blink rate, */
/* although the blink can be turned on and off independently.		*/

ip8qw(RCR, &lun, iosb, 0,0, &reg, &2, &0, &6);  /* read blink rate reg */

if (cursor == 2)	/* blink enable bit */
   bit = 0x2000;
else
   bit = 0x1000;

if (rate == 0) {
   reg &= ~ bit;		/* no blink */
   ratem1 = 0;
}
else {
   reg |= bit;			/* turn on blink */
   ratem1 = rate - 1;
}

reg = (reg & 0xFFF0) | (ratem1 & 0x000F);	/* set blink rate */

ip8qw(LCR, &lun, iosb, 0,0, &reg, &2, &0, &6);  /* write blink rate reg */

/*	Re-enable master graphics just to be sure	*/

VOC_LOAD_REG(&lun, &VOCBUF, &2, &1, &vocdat);

return (SUCCESS);

}

/************************************************************************/
/* Internal routine to write the cursor shape array.  Replaces the	*/
/* level 1 routine CURWR().						*/
/************************************************************************/

ip85lo_write_cursor_array(cursor, lun, size, array)
int cursor;		/* cursor number */
short lun;
short size;
int *array;
{
short saveres, resreg;
short curm1;
short iosb[4];

/* Read in resolution register */
ip8qw(RR, &lun, iosb, 0,0, &saveres, &2, &0, &11);

/* Set word transfers in resolution register */
resreg = saveres;
resreg &= ~ 0x0040;		/* Set word transfers */
ip8qw(LR, &lun, iosb, 0,0, &resreg, &2, &0, &11);

/* Load new lut address register value (always 0) */
ip8qw(LCR, &lun, iosb, 0,0, &0, &2, &0, &5);

/* Now send the array */
curm1 = cursor-1;
ip8qw(LPA, &lun, iosb, 0,0, array, &size, &curm1, &3);

/* Restore resolution register */
ip8qw(LR, &lun, iosb, 0,0, &saveres, &2, &0, &11);

return(SUCCESS);
}
