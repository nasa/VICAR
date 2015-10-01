/*	IP85LO_ConfigDevice - description
 *
 *	Purpose: Configure the DeAnza display device
 *
 *	Written by: Bob Deen
 *	Date:	    August 21, 1987
 *
 *	Calling Sequence:
 *		STATUS = IP85LO_ConfigDevice(Unit, lun, config)
 *
 *	Parameter List:
 *		Unit:	Display device unit number
 *		lun:	DeAnza logical unit number
 *		config:	Four integer array of configuration information...
 *			the last element is currently unused
 *
 *	Possible Error Codes:
 *		none
 *
 */
#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdsupport.h"

#include "ip85lo.h"

globalvalue LR, RR, LMC, LCR, ATT, DET, PAT;

FUNCTION IP85LO_ConfigDevice(Unit, lun, config)
int *Unit;
short lun;
int config[4];
{
int lut, imp, impm1, oimpm1, nlinem1, nsampm1, vlinem1;
short xxx, yyy;
short iosb[4];
short reg[3];

if (config[1] == IMP_512) {
   N_LINES = 512;
   N_SAMPS = 512;
}
else if (config[1] == IMP_1024) {
   if (DEV_TYPE == DEANZA_IP85LO)	/* N/A on this device */
      return INVALID_DEVICE_REQ;

   N_LINES = 1024;
   N_SAMPS = 1024;
}
else if (config[1] == IMP_2048) {
   if (DEV_TYPE != DEANZA_IP9000)
      return INVALID_DEVICE_REQ;

   N_LINES = 2048;
   N_SAMPS = 2048;
}
else
   return INVALID_DEVICE_REQ;		/* whoops! */

if (config[2] == VIDEO_512) {
   VIDEO_LINES = 512;
   VIDEO_SAMPLES = 512;
}
else if (config[2] == VIDEO_1024) {
   if (DEV_TYPE != DEANZA_IP9000)
      return INVALID_DEVICE_REQ;
   if (config[1] == IMP_512)
      return INVALID_DEVICE_REQ;

   VIDEO_LINES = 1024;
   VIDEO_SAMPLES = 1024;
}
else
   return INVALID_DEVICE_REQ;		/* whoops! */

reg[0] = 0;				/* X access window min */
reg[1] = N_SAMPS - 1;			/* X access window max */
ip8qw(LR, &lun, iosb, 0,0, &reg, &4, &1, &20);

reg[0] = 0;				/* Y access window min */
reg[1] = N_LINES - 1;			/* Y access window max */
ip8qw(LR, &lun, iosb, 0,0, &reg, &4, &1, &28);

/*	define logical address space coords	*/
reg[0] = 0;		/* X translate reg */
reg[1] = 0;		/* X minimum reg */
reg[2] = N_SAMPS - 1;	/* X maximum reg */
ip8qw(LR, &lun, iosb, 0,0, reg, &6, &1, &17);

reg[0] = 0;		/* Y translate reg */
reg[1] = 0;		/* Y minimum reg */
reg[2] = N_LINES - 1;	/* Y maximum reg */
ip8qw(LR, &lun, iosb, 0,0, reg, &6, &1, &25);

if (DEV_TYPE == DEANZA_IP85LO || DEV_TYPE == DEANZA_IP85LX) {
   ip8qw(RR, &lun, iosb, 0,0, &xxx, &2, &1, &11); /* resolution register */
   xxx &= ~0x0F;			/* clear resolution fields */
   if (config[1] == IMP_1024)
      xxx |= 0x20;			/* set extended memory */
   else
      xxx &= ~0x20;			/* clear extended memory */
   ip8qw(LR, &lun, iosb, 0,0, &xxx, &2, &1, &11);
}

if (DEV_TYPE == DEANZA_IP9000) {
   ip8qw(RR, &lun, iosb, 0,0, &xxx, &2, &1, &11); /* resolution register */
   xxx &= 0x0040;			/* clear all but SG (single byte) bit */
   if (config[2] == VIDEO_1024 || config[1] == IMP_2048) {
      xxx |= 0x0080;			/* extended mode */
      xxx |= 0x1000;			/* set extended cursors */
   }
   if (config[1] == IMP_1024) {
      xxx |= 0x0020;			/* HIF = 1 */
      if (config[2] == VIDEO_1024)
        xxx |= 0x0010;			/* MIF = 1 */
   }
   if (config[1] == IMP_2048) {
      xxx |= 0x0200;			/* HIF = 2 */
      if (config[2] == VIDEO_1024)
         xxx |= 0x0100;			/* MIF = 2 */
   }
   ip8qw(LR, &lun, iosb, 0,0, &xxx, &2, &1, &11); /* write out the register */
   ip8qw(RR, &lun, iosb, 0,0, &yyy, &2, &1, &11); /* read it back */
   if (xxx != yyy) {			/* The extended bit needs to change. */
      ip8qw(DET, &lun);			/* It's protected, so detach and */
      ip8qw(PAT, &lun);			/* reattach in privileged mode */
      ip8qw(LR, &lun, iosb, 0,0, &xxx, &2, &1, &11); /* write the register */
      ip8qw(LR, &lun, iosb, 0,0, &xxx, &2, &1, &11); /* (need to do it twice) */
      ip8qw(DET, &lun);			/* detach and reattach in normal mode */
      ip8qw(ATT, &lun);
   }
}

for (lut=1,imp=1; lut <= N_LUTS; lut++) {
   WHICH_IMP(lut) = imp;
   IP85LO_Connect(Unit, lun, imp, lut, 1, 0);   /* section 1, nobypass */
   if (config[0] == FULL_COLOR)
      imp++;
}

oimpm1 = OVERLAY_IMP-1;
IP85LO_Connect(Unit, lun, OVERLAY_IMP, 4, 1, 0); /* Connect graphics */

for (imp=0; imp<N_IMPS; imp++) {
   AW_LEFT(imp+1) = 1;
   AW_TOP(imp+1) = 1;
   AW_RIGHT(imp+1) = N_SAMPS;
   AW_BOTTOM(imp+1) = N_LINES;
   IP85LO_ZoomDW(Unit, lun, imp+1, 1, 1, 1);
   /* ITT is disabled by Connect, IRS is disabled by ZoomDW */
   MEM_SOFT_REG(&lun, &imp, &1);	/* load soft regs */
   MEM_BIT_PLANE_MASK(&lun, &imp, &-1);
}

vlinem1 = VIDEO_LINES - 1;		/* put the cursor within bounds */
reg[0] = 0;			/* x for cursor 1 */
reg[1] = vlinem1;		/* y for cursor 1 */
reg[0] = 0;			/* x for cursor 2 */
reg[1] = vlinem1;		/* y for cursor 2 */
ip8qw(LCR, &lun, iosb, 0,0, reg, &8, &0, &0);

if (DEV_TYPE == DEANZA_IP9000) {	/* set up cursor bounds & all other */
   reg[0] = 0;				/* extended cusor functions */
   if (config[2] == VIDEO_512 && config[1] == IMP_2048)
      reg[0] = 1;	/* zoom cursors by 2 for 512 video & 2048 memory */
   reg[1] = VIDEO_SAMPLES - 1;
   reg[2] = VIDEO_LINES - 1;
   ip8qw(LCR, &lun, iosb, 0,0, reg, &6, &0, &7);
}

return (SUCCESS);

}
