/*	IP85HI_ConfigDevice - description
 *
 *	Purpose: Configure the DeAnza display device
 *
 *	Written by: Bob Deen
 *	Date:	    June 17, 1988
 *
 *	Calling Sequence:
 *		STATUS = IP85HI_ConfigDevice(Unit, lun, config)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		config:	Four integer array of configuration information...
 *			only the first element is currently used
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

static short memdat[] = {
	0,		/* X scroll and blank wraparound (filled in) */
	0,		/* Y scroll and blank wraparound (filled in) */
	0xFFFF,		/* Write mask */
	0x1000		/* ITT LUT address and control (partially filled in) */
};

FUNCTION IP85HI_ConfigDevice(Unit, lun, config)
int *Unit;
short lun;
int config[4];
{
int lut, imp, impm1, oimpm1, ulx, uly;
short reg;
short iosb[4];

globalvalue BMC, RVR, LVR;

ulx = IP85HI_X_DEV(1);			/* upper left hand corner of IMP */
uly = IP85HI_Y_DEV(1);

memdat[0] = ulx>>2;	/* upper left corner, shifted for hi-res */
memdat[1] = uly;
if (memdat[1] & 0x0400) {	/* top bit of Y scroll goes in X scroll reg! */
    memdat[1] &= ~ 0x0400;
    memdat[0] |= 0x0200;
}
memdat[3] &= ~0x0400;		/* Define as Image plane */
ip8qw(BMC, &lun, iosb, 0, 0,
      memdat, &8, &1, &0);	/* load mem ctrl regs */
ip8qw(BMC, &lun, iosb, 0, 0,
      memdat, &8, &2, &0);	/* load mem ctrl regs */
ip8qw(BMC, &lun, iosb, 0, 0,
      memdat, &8, &3, &0);	/* load mem ctrl regs */
memdat[3] &= ~0x0400;		/* Define as Graphics plane */
ip8qw(BMC, &lun, iosb, 0, 0,
      memdat, &8, &4, &0);	/* load mem ctrl regs */

/* Set BW, pseudo-color, or full-color mode */

ip8qw(RVR, &lun, iosb, 0,0, &reg, &2, &0, &4);
reg &= ~0xC000;				/* clear mode bits (FULL_COLOR) */
if (config[0] == PSEUDO_COLOR)
    reg |= 0x4000;			/* R=bits 5-7, G=bits 2-4, B=bits 0-1 */
else if (config[0] == BLACK_AND_WHITE)
    reg |= 0x8000;			/* channel 1 b&w */
ip8qw(LVR, &lun, iosb, 0,0, &reg, &2, &0, &4);

return (SUCCESS);

}
