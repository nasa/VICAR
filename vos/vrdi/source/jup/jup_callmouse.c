/*	JUP_CallMouse
 *
 *	Purpose:	Subroutines used to talk to the mouse subprocess
 *
 *	Written by:	Bob Deen
 *	Date:		April 3, 1990
 *
 *	Calling Sequences:
 *
 *		STATUS = JUP_StartMouse( Unit )
 *
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdalloc.h"

#include "jupinc.h"
#include "jupmouse.h"

#include <ssdef>
#include <iodef>
#include <descrip>
#include <lnmdef>

/************************************************************************/
/* Create (or attach to if it's already running) the mouse subprocess.	*/
/************************************************************************/

JUP_StartMouse(Unit, mouse, video, mode, ovbits, gpuno)
int *Unit;
int mouse, video, mode, ovbits, gpuno;
{
int status, i, j;
static int length;
static char out_name[20];
static char image_name[80], image_tmp[80];
char mbx_name[20], prc_name[20];
JUP_MESSAGE message;

static struct {
   short length, code;
   int *addr, *retaddr, end;
} trnlogitmlst = {20, LNM$_STRING, out_name, &length, 0};

static struct {
   short length, code;
   int *addr, *retaddr, end;
} trnlogitmlst2 = {70, LNM$_STRING, image_tmp, &length, 0};

$DESCRIPTOR(imaged, image_name);		/* image name of subprocess */
$DESCRIPTOR(outfiled, "SYS$OUTPUT");		/* stdout for subprocess */
$DESCRIPTOR(logtabd, "LNM$FILE_DEV");		/* name table for sys$output */
$DESCRIPTOR(outnamed, out_name);		/* Translated name of stdout */
$DESCRIPTOR(mbxd, mbx_name);			/* Mailbox name */
$DESCRIPTOR(prcnamed, prc_name);		/* Process name */

/* Get strings for all descriptors */

sprintf(mbx_name, "%s_MBX_%d", DEV_NAME, mouse);
mbxd.dsc$w_length = strlen(mbx_name);

sprintf(prc_name, "%s_MOUSE_%d", DEV_NAME, mouse%10);
prcnamed.dsc$w_length = strlen(prc_name);

/* Get translation for SYS$OUTPUT */

status = sys$trnlnm(0, &logtabd, &outfiled, 0, &trnlogitmlst);
if (status != SS$_NORMAL)
   return status;
outnamed.dsc$w_length = length;

/* Get image filename: translate VRDI$LIB and append "JUP_MOUSE" to it */
/* We have to repeatedly translate VRDI$LIB since it's usually V2$LIB  */

strcpy(image_name, "VRDI$LIB");
imaged.dsc$w_length = strlen(image_name);

do {
   status = sys$trnlnm(0, &logtabd, &imaged, 0, &trnlogitmlst2);
   if (status == SS$_NORMAL) {
      if (image_tmp[length-1] == ':') length--;   /* strip trailing ':' */
      image_tmp[length] = '\0';
      strcpy(image_name, image_tmp);
      imaged.dsc$w_length = strlen(image_name);
   }
} while (status == SS$_NORMAL);		/* loop till no more translations */
strcat(image_name, "JUP_MOUSE");		/* Image name */
imaged.dsc$w_length = strlen(image_name);

/* Create the mailbox used to send messages to the subprocess */

status = sys$crembx(0, &MBX_CHAN, 0,0,0,0, &mbxd);
if (status != SS$_NORMAL)
   return status;

/* Attach to shared memory.  If it doesn't already exist, then start	*/
/* up the subprocess.  If it does, set the shmem pointer and return.	*/

status = Attach_Shmem(*Unit, sizeof(struct shmem_jup), &SHMEM, SHMEM_JUP);
if (status != SUCCESS && status != SHMEM_ALREADY_ACTIVE)
   return status;

if (status == SHMEM_ALREADY_ACTIVE)
   return SUCCESS;			/* all done */

/* Create the subprocess */

status = sys$creprc(0, &imaged, &mbxd, &outnamed, &outnamed,
		0, 0, &prcnamed, 4, 0, 0, 0);
if (status != SS$_NORMAL)
   return status;

/* Send the initialization message */

message.type = MESS_INIT;
message.init.mouse = mouse;
message.init.video = video;
message.init.gpuno = gpuno;
message.init.mode = mode;
message.init.ovbits = ovbits;
strcpy(message.init.secname, SectionName);  /* Section name from Attach_Shmem */
message.init.curs_dn = zdgrgb(*Unit, CURSOR_RED(1), CURSOR_GREEN(1),
				     CURSOR_BLUE(1));
message.init.half_dn = zdgrgb(*Unit, CURSOR_RED(1)/2, CURSOR_GREEN(1)/2,
				     CURSOR_BLUE(1)/2);
message.init.black_dn = zdgrgb(*Unit, 0, 0, 0);
message.init.mem_x = N_SAMPS;
message.init.mem_y = N_LINES;
message.init.vid_x = VIDEO_SAMPLES;
message.init.vid_y = VIDEO_LINES;

status = JUP_SendMessage(Unit, &message);
if (status != SUCCESS)
   return status;

return SUCCESS;
}

/************************************************************************/
/* Actually send a message to the mouse process via the mailbox.	*/
/************************************************************************/

JUP_SendMessage(Unit, m)
int *Unit;
JUP_MESSAGE *m;
{
int status;
struct iosb iosb;

status = sys$qiow(0, MBX_CHAN, IO$_WRITEVBLK | IO$M_NOW, &iosb, 0,0,
		m, sizeof(*m), 0,0,0,0);

if (status == SS$_NORMAL)
   return SUCCESS;
return status;

}

/****************************************************************************/
/* When we change the config, we need to notify all the other subprocesses. */
/* j_config() doesn't know when another process changed the config, so it   */
/* must be done in all processes (handled in xd_interface.c).  We also	    */
/* notify the mouse process.						    */
/****************************************************************************/

JUP_NewConfig(Unit, mode)
int *Unit;
int mode;
{
JUP_MESSAGE mess;

   CONFIG_MODE = mode;
   local_mode[*Unit] = mode;

   if (OVERLAY_AVAILABLE)
   {
      CONFIG_OVBITS = 4;
      local_ovbits[*Unit] = 4;
   }
   else
   {
      CONFIG_OVBITS = 0;
      local_ovbits[*Unit] = 0;
   }

   mess.type = MESS_NEWCONFIG;		/* For the mouse process */
   mess.newconfig.mode = mode;
   mess.newconfig.ovbits = local_ovbits[*Unit];
   mess.newconfig.ovon = OVERLAY_AVAILABLE && OVERLAY_ON;
   mess.newconfig.mem_x = N_SAMPS;
   mess.newconfig.mem_y = N_LINES;
   mess.newconfig.vid_x = VIDEO_SAMPLES;
   mess.newconfig.vid_y = VIDEO_LINES;
   strncpy(mess.newconfig.hw, vid_parm(VIDEO_FILE,"hw",JUP_GPUNO,JUP_VIDEO),
		HW_SIZE);
   mess.newconfig.hw[HW_SIZE-1] = '\0';
   JUP_SendMessage(Unit, &mess);

   jfflush(J12file);			/* probably not needed */
}

/************************************************************************/
/* When we change the overlay status (on/off or available/not avail),	*/
/* we need to notify all the other subprocesses.  j_cursor_sel()	*/
/* doesn't know when another process changed the cursor image/overlay	*/
/* location, so it must be done in all processes (handled in		*/
/* xd_interface.c).  We also notify the mouse process.			*/
/************************************************************************/

JUP_CursorOverlay(Unit)
int *Unit;
{
JUP_MESSAGE mess;

   mess.overlay.type = MESS_OVERLAY;
   mess.overlay.curs_dn = zdgrgb(*Unit, CURSOR_RED(1), CURSOR_GREEN(1),
				CURSOR_BLUE(1));
   mess.overlay.half_dn = zdgrgb(*Unit, CURSOR_RED(1)/2, CURSOR_GREEN(1)/2,
				CURSOR_BLUE(1)/2);
   mess.overlay.black_dn = zdgrgb(*Unit, 0, 0, 0);

   if (OVERLAY_AVAILABLE && OVERLAY_ON)
   {
      mess.overlay.on = TRUE;
      JUP_SendMessage(Unit, &mess);	/* tell mouse process */
      j_cursor_sel(OI_OVERLAYS);	/* do it ourselves */
      CONFIG_CURSOR = OI_OVERLAYS;	/* tell other subprocs */
      local_cursor[*Unit] = OI_OVERLAYS;
   }
   else
   {
      mess.overlay.on = FALSE;
      JUP_SendMessage(Unit, &mess);	/* tell mouse process */
      j_cursor_sel(OI_IMAGE);		/* do it ourselves */
      CONFIG_CURSOR = OI_IMAGE;		/* tell other subprocs */
      local_cursor[*Unit] = OI_IMAGE;
   }
   jfflush(J12file);

   JUP_SendPanZoom(Unit);
}

/************************************************************************/
/* When we change the pan or zoom of the plane the cursor is on (or of	*/
/* course just change the plane the cursor is on) we must notify the	*/
/* mouse process of the new pan/zoom factors.  This routine does that.	*/
/************************************************************************/

JUP_SendPanZoom(Unit)
int *Unit;
{
JUP_MESSAGE mess;

   mess.overlay.type = MESS_PANZOOM;
   if (OVERLAY_AVAILABLE && OVERLAY_ON) {
      mess.panzoom.dw_left = DW_LEFT(OVERLAY_IMP);
      mess.panzoom.dw_top = DW_TOP(OVERLAY_IMP);
      mess.panzoom.zoom = ZOOM(OVERLAY_IMP);
   }
   else {			/* image plane, use plane 1 */
      mess.panzoom.dw_left = DW_LEFT(1);
      mess.panzoom.dw_top = DW_TOP(1);
      mess.panzoom.zoom = ZOOM(1);
   }

   if (mess.panzoom.zoom == 0)
      mess.panzoom.zoom = 1;

   JUP_SendMessage(Unit, &mess);	/* tell mouse process */

   jfflush(J12file);

}
