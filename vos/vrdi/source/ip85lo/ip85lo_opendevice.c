/*	IP85LO_OpenDevice - description
 *
 *	Purpose: Open an I/O channel to the DeAnza display device
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *
 *		STATUS = IP85LO_OpenDevice(Unit, dummylun)
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

#include "ip85lo.h"

globalvalue ATT, LPR, LR, LMC, LCR;
globalvalue RR;

FUNCTION IP85LO_OpenDevice(Unit, dummylun)
int *Unit;
short dummylun;

{
short iosb[4];
short lun;
short lut[256];
short i, istat;
short addr;
short xxx;
short nlinem1, nsampm1, vlinem1;
short regs[3];

short cmrdat[] = {
	0x0001,0, 0x0002,0, 0x0004,0, 0x0008,0	/* channel mask reg 0-3 */
};

short memdat[] = {
	0,		/* X scroll and blank wraparound */
	0x1FF,		/* Y scroll and blank wraparound (filled in) */
	0xFFFF,		/* Write mask */
	0		/* ITT LUT address and control */
};

short perreset[] = { 0, 0, 0, 0 };	/* reset trackball */
short perdat[] = {
	0,		/* trackball status register */
	0x20,		/* control reg */
	0,		/* x position (upper right of screen) */
	511		/* y position (upper right of screen) (filled in) */
};

short curdat[] = {
	0,		/* Cursor 1 X position (filled in) */
        0,              /* Cursor 1 Y position (filled in) */
        0,              /* Cursor 2 X position (filled in) */
        0,              /* Cursor 2 Y position (filled in) */
        0,              /* Cursor control */
        0,              /* LUT address register */
        0               /* Cursor rate */
};

short vocdat[] = {
	0,		/* X split position */
	0,		/* Y split position */
	0x4300,		/* VOC control register */
	0		/* VOC LUT address */
};

/*	Extract logical unit number from logical name 'IPn'	*/

DEV_UNIT_NO = DIB[*Unit]->LogicalName[2] - '0';
lun = DEV_UNIT_NO;

istat = ip8qw(ATT, &lun, iosb);
if (istat != 1)
   return CANNOT_ALLOC_DEVICE;

/*	Initialize VOC	*/

VOC_LOAD_REG(&lun, &VOCBUF, &0, &4, vocdat);

/*	Define access window and set up memory */

IP85LO_ConfigDevice(Unit, lun, DIB[*Unit]->DefaultConfig);

nlinem1 = N_LINES-1;
nsampm1 = N_SAMPS-1;

/*	Initialize peripheral input device	*/

if (N_IO_DEVICES != 0) {
   ip8qw(LPR, &lun, iosb, 0, 0,
	 perreset, &8, &0, &0);		/* reset and halt trackball */
   perdat[3] = nlinem1;
   ip8qw(LPR, &lun, iosb, 0, 0,
	 perdat, &8, &0, &0);		/* start it up again */
}

/*	define logical address space coords	*/
regs[0] = 0;		/* X translate reg */
regs[1] = 0;		/* X minimum reg */
regs[2] = nsampm1;	/* X maximum reg */
ip8qw(LR, &lun, iosb, 0,0, regs, &6, &1, &17);

regs[0] = 0;		/* Y translate reg */
regs[1] = 0;		/* Y minimum reg */
regs[2] = nlinem1;	/* Y maximum reg */
ip8qw(LR, &lun, iosb, 0,0, regs, &6, &1, &25);

/*	set up non-matrix addressing, define axes	*/
xxx = 0;		/* x/y delta register */
ip8qw(LR, &lun, iosb, 0,0, &xxx, &2, &1, &22);
ip8qw(LR, &lun, iosb, 0,0, &xxx, &2, &1, &30);

ip8qw(RR, &lun, iosb, 0,0, &xxx, &2, &1, &10);
xxx &= 0xFFF0;
xxx |= 0x0002;		/* PA+1, SA-1, primary=X, nonmatrix */
ip8qw(LR, &lun, iosb, 0,0, &xxx, &2, &1, &10);

/*	set up CMR registers, and all memory control registers	*/
ip8qw(LR, &lun, iosb, 0, 0,
      cmrdat, &16, &1, &32);	/* set CMR regs */
memdat[1] = nlinem1;
ip8qw(LMC, &lun, iosb, 0, 0,
      memdat, &8, &0, &0);	/* load mem ctrl regs */
ip8qw(LMC, &lun, iosb, 0, 0,
      memdat, &8, &1, &0);	/* load mem ctrl regs */
ip8qw(LMC, &lun, iosb, 0, 0,
      memdat, &8, &2, &0);	/* load mem ctrl regs */
ip8qw(LMC, &lun, iosb, 0, 0,
      memdat, &8, &3, &0);	/* load mem ctrl regs */

/*	initialize the cursor	*/
vlinem1 = VIDEO_LINES - 1;
curdat[0] = 0;		/* cursor 1 X */
curdat[1] = vlinem1;	/* cursor 1 Y */
curdat[2] = 0;		/* cursor 2 X */
curdat[3] = vlinem1;	/* cursor 3 X */
ip8qw(LCR, &lun, iosb, 0,0, curdat, &14, &0, &0);

/* Set overlay LUT 0 (the cursor LUT) to solid white */
IP85LO_CursorColor(Unit, lun, 1, 255, 255, 255);

/* Set overlay LUT 2 to transparent (cursor black for blink) */
for (i=0; i<256; i++)
   lut[i] = 0;
addr = (2<<9);		/* section 2 */
VOCWR_OVERLAY_LUT(&lun, &VOCBUF, &addr, &256, lut);

return (SUCCESS);
}
