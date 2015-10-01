/*	IP85HI_CursorOn - description
 *
 *	Purpose: Turn the display of a cursor on for the DeAnza display device
 *
 *	Written by: Bob Deen
 *	Date:	    June 17, 1988
 *
 *	Calling Sequence:
 *		STATUS = IP85HI_CursorOn(Unit, lun, cursor, form, rate)
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
 *			1-16 : fastest=1 (.26 sec), slowest=16 (3.94 sec)
 *
 *	Possible Error Codes:
 *		none
 *
 */
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "ip85hi.h"

globalvalue RCR, LCR;

static short formdat[6] = {2, 0, 3, 2, 0, 3};
static short dashdat[6] = {0, 0, 0, -1, -1, -1};

FUNCTION IP85HI_CursorOn(Unit, lun, cursor, form, rate)
int *Unit;
short lun;
int cursor, form, rate;
{
short reg[3];
short iosb[4];
int bit;

if (form == 0)
   form = 1;		/* default */

ip8qw(RCR, &lun, iosb, 0,0, reg, &6, &0, &4);	/* read cur ctrl & rate regs */

/* Set the cursor form.  Note that cursors 1 and 2 share the form bits.	*/
/* However, dashed forms can be turned on/off independently.		*/

reg[0] &= ~0x0003;		/* clear the cursor form */
reg[0] |= formdat[form-1];	/* set the form */

if (cursor == 2)		/* set/clear dashed bit */
    bit = 0x0040;
else
    bit = 0x0020;
reg[0] &= ~ bit;
reg[0] |= (bit & dashdat[form-1]);

if (cursor == 2)
    reg[0] |= 0x0010;		/* enable cursor 2 */
else
    reg[0] |= 0x0008;		/* enable cursor 1 */

/* Set blink rate.  Note that cursors 1 and 2 have the SAME blink rate, */
/* although the blink can be turned on and off independently.		*/

if (cursor == 2)
    bit = 0x0020;		/* blink enable bit */
else
    bit = 0x0010;

if (rate == 0)
    reg[2] &= ~ bit;		/* turn off blink */
else {
    reg[2] |= bit;		/* turn on blink */
    reg[2] &= ~ 0x000F;		/* clear rate register */
    reg[2] |= (rate-1);		/* set blink rate */
}
ip8qw(LCR, &lun, iosb, 0,0, reg, &6, &0, &4);	/* write cur ctrl & rate regs */

return (SUCCESS);

}
