/*	IP85HI_OpenDevice - description
 *
 *	Purpose: Open an I/O channel to the DeAnza display device
 *
 *	Written by: Bob Deen
 *	Date:	    June 17, 1988
 *
 *	Calling Sequence:
 *
 *		STATUS = IP85HI_OpenDevice(Unit, dummylun)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		dummylun: DeAnza logical unit number... not used
 *
 *	Possible Error Codes:
 *		CANNOT_ALLOC_DEVICE : I/O channel could not be opened.
 *
 */
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"

#include "ip85hi.h"

globalvalue ATT, BMC, BPA, LCR, LPR, LR, LVR, RR;

FUNCTION IP85HI_OpenDevice(Unit, dummylun)
int *Unit;
short dummylun;

{
short iosb[4];
short lun;
unsigned char lut[256];
short i, istat;
short addr;
short ulx, uly;
short reg;

short cmrdat[] = {
	0x000F,0, 0x000F,0, 0x00F0,0, 0x0F00,0, 0xF000,0	/* cmr 0-4 */
};

short memdat[] = {
	0,		/* X scroll and blank wraparound (filled in) */
	0,		/* Y scroll and blank wraparound (filled in) */
	0xFFFF,		/* Write mask */
	0x1000		/* ITT LUT address and control (partially filled in) */
};

short perreset[] = { 0, 0, 0, 0 };	/* reset trackball */
short perdat[] = {
	0,		/* trackball status register */
	0x20,		/* control reg */
	0,		/* x position (upper left of IMP) (filled in) */
	0		/* y position (upper left of IMP) (filled in) */
};

short curdat[] = {
	0,		/* Cursor 1 X position (filled in) */
	0,		/* Cursor 1 Y position (filled in) */
	0,		/* Cursor 2 X position (filled in) */
	0,		/* Cursor 2 Y position (filled in) */
	0,		/* Cursor control */
	0,		/* LUT address register (?) */
	0		/* Cursor rate */
};

short vocdat[] = {
	0,		/* Red Zoom/Scroll */
	0,		/* Green Zoom/Scroll */
	0,		/* Blue Zoom/Scroll */
	0,		/* Graphics Zoom/Scroll */
	0x1038		/* Enable Control register */
};

short xdat[] = {	/* Initialize system registers 17-22 (X axis stuff) */
	0,		/* X-Translate */
	0,		/* X-Minimum */
	0,		/* X-Maximum (filled in) */
	0,		/* X-Access window minimum */
	0,		/* X-Access window maximum (filled in) */
	0		/* Delta-X */
};
short ydat[] = {	/* Initialize system registers 25-30 (Y axis stuff) */
	0,		/* Y-Translate */
	0,		/* Y-Minimum */
	0,		/* Y-Maximum (filled in) */
	0,		/* Y-Access window minimum */
	0,		/* Y-Access window maximum (filled in) */
	0		/* Delta-Y */
};

/*	Extract logical unit number from logical name 'IPn'	*/

DEV_UNIT_NO = DIB[*Unit]->LogicalName[2] - '0';
lun = DEV_UNIT_NO;

istat = ip8qw(ATT, &lun, iosb);
if (istat != 1)
   return CANNOT_ALLOC_DEVICE;

ulx = IP85HI_X_VDEV(1);		/* upper left corner of display (not IMP) */
uly = IP85HI_Y_VDEV(1);

/*	Initialize peripheral input device	*/

if (N_IO_DEVICES != 0) {
   ip8qw(LPR, &lun, iosb, 0, 0,
	 perreset, &8, &0, &0);		/* reset and halt trackball */
   perdat[2] = ulx;			/* upper left corner */
   perdat[3] = uly;
   ip8qw(LPR, &lun, iosb, 0, 0,
	 perdat, &8, &0, &0);		/* start it up again */
}

/*	Set up system X and Y registers */
xdat[2] = N_SAMPS-1;
xdat[4] = N_SAMPS-1;
ip8qw(LR, &lun, iosb, 0,0, xdat, &12, &1, &17);

ydat[2] = N_LINES-1;
ydat[4] = N_LINES-1;
ip8qw(LR, &lun, iosb, 0,0, ydat, &12, &1, &25);

/*	Set up non-matrix addressing mode */
ip8qw(RR, &lun, iosb, 0,0, &reg, &2, &1, &10);
reg &= ~ 0x000F;			/* clear addr mode bits */
reg |= 0x02;	      /* matrix off, X-axis primary, decrement Y, increment X */
ip8qw(LR, &lun, iosb, 0,0, &reg, &2, &1, &10);

/*	Set up the resolution register */
ip8qw(RR, &lun, iosb, 0,0, &reg, &2, &1, &11);
reg &= ~ 0x002F;			/* clear relevant bits */
reg |= 0x0010;				/* hi-res mode */
if (N_LINES == 2048 && N_SAMPS == 2048) {
    reg |= 0x0020;			/* extended memory */
    reg |= 0x0005;			/* 2048 resolution in both X and Y */
}
ip8qw(LR, &lun, iosb, 0,0, &reg, &2, &1, &11);

/*	set up CMR registers, and all memory control registers	*/
ip8qw(LR, &lun, iosb, 0, 0,
      cmrdat, &20, &1, &32);	/* set CMR regs (0 is scratch) */
memdat[0] = IP85HI_X_DEV(1)>>2;	/* upper left corner, shifted for hi-res */
memdat[1] = IP85HI_Y_DEV(1);
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
memdat[3] |= 0x0400;		/* Define as Graphics plane */
ip8qw(BMC, &lun, iosb, 0, 0,
      memdat, &8, &4, &0);	/* load mem ctrl regs */

/*	initialize the cursor	*/

curdat[0] = ulx;		/* corner of video display (not IMP) */
curdat[1] = uly;
curdat[2] = ulx;
curdat[3] = uly;
ip8qw(LCR, &lun, iosb, 0, 0, curdat, &14, &0, &0);

/* Ramp the overlay ITT (graphics overlay must always be inverted!) */

for (i=0; i<256; i++)
   lut[i] = 255-i;
ip8qw(BPA, &lun, iosb, 0, 0, lut, &256, &4, &0);

/*	Initialize the hi-res VOC	*/

ip8qw(LVR, &lun, iosb, 0, 0, vocdat, &10, &0, &0);

return (SUCCESS);
}
