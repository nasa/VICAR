#include <stdio.h>
/*	JUP_Mouse
 *
 *	Purpose:	An independent subprocess that reads the Jupiter
 *			mouse and updates a shared memory area with the
 *			current mouse status.
 *
 *	Written by:	Bob Deen
 *	Date:		March, 1990
 *
 */
#include "jupmouse.h"

#include <jgio.h>
#include <jconfig.h>

#include <ssdef>
#include <iodef>
#include <dvidef>
#include <ttdef>
#include <descrip>
#include <secdef>

#define SUCCESS 1

#define MAX_TICKS	100		/* # of ticks before next speed level */
#define SPEED_LEVELS	3		/* # of timer speed levels */
#define EVENT_LIMIT	50		/* # of events to process before */
					/* checking messages again       */

#define WRAP_AMT	100		/* amt of off-screen space for cursor */

#define TIMER_ID	54321		/* arbitrary */

/* This #define is are needed because there's only one graphics cursor	*/
/* for ALL video on the device, but the graphics cursor is better than	*/
/* the multiple cursor which is available on all screens.  Currently 	*/
/* the test is only that the mouse number == 0 to use GCSR.  This check	*/
/* is also made in jup_opendevice.c to set the DCB tables correctly, so	*/
/* if you change the test, change it there too.				*/

#define CK_GCSR(mouse)	((mouse) == 0)
#define CSR_T(mouse)	((CK_GCSR(mouse)) ? GCSR : MCSR)      /* cursor type */
#define MCSR_ENABLE(shape)   (((shape)%2)+1)	/* j_mccsr enable value */

JFILE *J12file;

int J_MX, J_MY;			/* Max X and Y dimensions of memory */
int J_VX, J_VY;			/* Max sizes of video */

char string[255];
int out_chan;

int bintime[SPEED_LEVELS][2];

globalref int jup_cursorshape[7][20][20];

/************************************************************************/
/* Main mouse program... set things up, and wait for an event, which is	*/
/* either a timer (in which case we go read the mouse state and update	*/
/* the location in shared memory), or a message from the parent process	*/
/* (in which case we respond to the command).				*/
/************************************************************************/

main()
{
   int status, i, flag;
   int in_channel;
   int mouse_number, video_number, gpu_number;
   struct iosb iosb;
   JUP_MESSAGE mess;
   struct mouse_state state;
   struct shmem_jup *shmem;
   $DESCRIPTOR(inputd, "SYS$INPUT");
   $DESCRIPTOR(outputd, "SYS$OUTPUT");
   $DESCRIPTOR(time0d, "0000 00:00:00.10");	/* 1/10 second */
   $DESCRIPTOR(time1d, "0000 00:00:00.50");	/* 1/2 second */
   $DESCRIPTOR(time2d, "0000 00:00:02.00");	/* two seconds */
   int mbox_efn, timer_efn;
   int flag_state;
   int x, y, buttons;
   int oldx, oldy, oldb;
   int set;
   int limit;
   int event_count;
   char *map_memory();

   status = sys$assign(&outputd, &out_chan, 0,0);

   status = sys$assign(&inputd, &in_channel, 0,0);	/* get input channel */
   if (status != SS$_NORMAL) {
      sprintf(string, "Error assigning input channel = %d\r\n", status);
      print_me(string);
      exit(status);
   }

   status = lib$get_ef(&mbox_efn);
   if (status == SS$_NORMAL)
      status = lib$get_ef(&timer_efn);
   if (status != SS$_NORMAL) {
      sprintf(string, "Error getting event flag =%d\r\n", status);
      print_me(string);
      exit(status);
   }

   status = sys$bintim(&time0d, bintime[0]);
   if (status == SS$_NORMAL)
      status = sys$bintim(&time1d, bintime[1]);
   if (status == SS$_NORMAL)
      status = sys$bintim(&time2d, bintime[2]);
   if (status != SS$_NORMAL) {
      sprintf(string, "Error getting bintime=%d\r\n", status);
      print_me(string);
      exit(status);
   }

   /* Get the initialization message */

   do {
      status = sys$qiow(0, in_channel, IO$_READVBLK, &iosb, 0,0,
			&mess, sizeof(mess), 0,0,0,0);
      if (status != SS$_NORMAL) {
         sprintf(string, "Error getting initialization message=%d\r\n", status);
         print_me(string);
         exit(status);
      }
      if (iosb.status != SS$_NORMAL) {
         sprintf(string, "Error getting init message=%d\r\n", iosb.status);
         print_me(string);
         exit(iosb.status);
      }
   } while (mess.type != MESS_INIT);	/* whoops! wrong message! */

   mouse_number = mess.init.mouse;
   video_number = mess.init.video;
   gpu_number = mess.init.gpuno;
   state.disp_mode = mess.init.mode;
   state.curs_dn = mess.init.curs_dn;
   state.half_dn = mess.init.half_dn;
   state.black_dn = mess.init.black_dn;

   J_MX = mess.init.mem_x;
   J_MY = mess.init.mem_y;
   J_VX = mess.init.vid_x;
   J_VY = mess.init.vid_y;

   state.mouse = mouse_number;
   state.on = TRUE;
   state.shape = 0;
   state.autot = TRUE;
   state.red = 255;
   state.green = 255;
   state.blue = 255;
   state.curs_overlay = (mess.init.ovbits == 0) ? FALSE : TRUE;
   state.dw_left = 1;
   state.dw_top = 1;
   state.zoom = 1;

   shmem = map_memory(sizeof(*shmem), mess.init.secname);

   shmem->x = 1;
   shmem->y = 2;
   shmem->buttons = 3;

   /* Set up the mouse */

   J12file=jginit(gpu_number,video_number,mess.init.mode,1,0,mess.init.ovbits);

   status = j_prepserial(SDMOUSE, mouse_number);	/* Initialize mouse */
   if (status != 0) {
      sprintf(string, "Error in j_prepserial = %d\r\n", status);
      print_me(string);
      exit(status);
   }
   status = j_blocks(mouse_number, 0);		/* don't block */

   make_cursor(&state);
   status = j_attachs(mouse_number, 1, CSR_T(mouse_number), state.mcsr_color,
							state.shape_array);
   jfflush(J12file);

   if (CK_GCSR(mouse_number)) {
      j_csrset(state.shape_array);	/* For graphics cursors */
      j_dca(1, 1, 1);
   }
   else
      j_mccsr(1, 1, MCSR_ENABLE(0));	/* For multiple cursors */
   j_setxy(mouse_number, 1, 1);
   if (mess.init.ovbits == 0)
      j_cursor_sel(OI_IMAGE);    /* No overlay, put cursor on image planes */
   else
      j_cursor_sel(OI_OVERLAYS);    /* Overlay available, put cursor there */
   jfflush(J12file);

   /* WRAP_AMT is an arbitrary wrap factor here... allows us enough room to */
   /* notice that the cursor has wrapped and to move it to the other side. */

   j_clampse(mouse_number, JUPM_X_IMG(-WRAP_AMT), JUPM_Y_IMG(J_MY+WRAP_AMT),
			   JUPM_X_IMG(J_MX+WRAP_AMT), JUPM_Y_IMG(-WRAP_AMT));

   /* Set up the two events that will interrupt us */

   set_timer(timer_efn, TRUE);

   status = sys$qio(mbox_efn, in_channel, IO$_READVBLK, &iosb, 0,0,
			&mess, sizeof(mess), 0,0,0,0);
   if (status != SS$_NORMAL) {
      sprintf(string, "qio read failed =%d\r\n", status);
      print_me(string);
      exit(status);
   }

   /* Main loop.  Wait until we get a message, or the timer expires	*/
   /* which means it's time to check the mouse again.			*/

   while (1) {
      status = sys$wflor(mbox_efn, (1<<(mbox_efn&31)) | (1<<(timer_efn&31)));
      if (status != SS$_NORMAL) {
         sprintf(string, "wflor failed =%d\r\n", status);
         print_me(string);
         exit(status);
      }

      /* A timer event */

      if (sys$readef(timer_efn, &flag_state) == SS$_WASSET) {
         sys$clref(timer_efn);
         flag = FALSE;
         x = shmem->x;		/* needed for correct loop termination */
         y = shmem->y;
         buttons = shmem->buttons;
         event_count = 0;

         do {

/* Only do EVENT_LIMIT events in a row before checking messages again	*/
/* so we don't get too far behind the main process.			*/

            event_count++;

            oldx = x;
            oldy = y;
            oldb = buttons;

            j_getserial(mouse_number, &x, &y, &buttons);

            x = JUPM_IMG_X(x);
            y = JUPM_IMG_Y(y);

            set = FALSE;

/* Check for display boundaries.  Two cases:  A) Image plane boundary	*/
/* is on screen (i.e. DW is too close to edge so it wraps) and B) Image	*/
/* plane is contiguous on screen (no wrap).  The boundary checks are	*/
/* different for the two cases.  If the coordinate is off the boundary,	*/
/* then we need to move the cursor.  The new coordinate is determined	*/
/* by the while loops below.  In either case we need to check for IMP	*/
/* wrapping.  If the cursor coord is on screen, adjust it by the memory	*/
/* size.  If it's off screen, adjust it by the video size.		*/

            limit = state.dw_left + (J_VX+state.zoom-1) / state.zoom;  /*right*/
            if (limit > J_MX) {			/* Case A */
               if (x < state.dw_left && x >= (limit-J_MX)) {
                  set = TRUE;
                  if (x < 1)     x += J_VX;		/* off left of IMP */
                  if (x > J_MX)  x -= J_VX;		/* off right edge  */
               }
               else {
                  if (x < 1)    {  x += J_MX;  set = TRUE;  }	/* off left  */
                  if (x > J_MX) {  x -= J_MX;  set = TRUE;  }	/* off right */
               }
            }
            else {				/* Case B */
               if (x < state.dw_left || x >= limit) {
                  set = TRUE;
                  if (x < 1)     x += J_VX;		/* off left of IMP */
                  if (x > J_MX)  x -= J_VX;		/* off right edge  */
               }
               else {
                  if (x < 1)    {  x += J_MX;  set = TRUE;  }	/* off left  */
                  if (x > J_MX) {  x -= J_MX;  set = TRUE;  }	/* off right */
               }
            }
            x = (x - state.dw_left) * state.zoom + 1;	/* imp->raw coords */
            if (x < -WRAP_AMT)
               x += J_MX * state.zoom;

            limit = state.dw_top + (J_VY+state.zoom-1) / state.zoom;  /*bottom*/
            if (limit > J_MY) {			/* Case A */
               if (y < state.dw_top && y >= (limit-J_MY)) {
                  set = TRUE;
                  if (y < 1)     y += J_VY;		/* off bottom of IMP */
                  if (y > J_MY)  y -= J_VY;		/* off top edge      */
               }
               else {
                  if (y < 1)    {  y += J_MY;  set = TRUE;  }	/* off bottom */
                  if (y > J_MY) {  y -= J_MY;  set = TRUE;  }	/* off top    */
               }
            }
            else {				/* Case B */
               if (y < state.dw_top || y >= limit) {
                  set = TRUE;
                  if (y < 1)     y += J_VY;		/* off bottom of IMP */
                  if (y > J_MY)  y -= J_VY;		/* off top edge      */
               }
               else {
                  if (y < 1)    {  y += J_MY;  set = TRUE;  }	/* off bottom */
                  if (y > J_MY) {  y -= J_MY;  set = TRUE;  }	/* off top    */
               }
            }
            y = (y - state.dw_top) * state.zoom + 1;	/* imp->raw coords */
            if (y < -WRAP_AMT)
               y += J_MY * state.zoom;

/* Check for display wrap.  Can go more than one screen away if zoomed */

            while (x < 1)    x += J_VX;		/* left */
            while (x > J_VX) x -= J_VX;		/* right */
            while (y < 1)    y += J_VY;		/* bottom */
            while (y > J_VY) y -= J_VY;		/* top */

            if (x != oldx || y != oldy || buttons != oldb || set) {
               flag = TRUE;		/* something changed */

               if (set && state.autot)	/* reset position only if autot is on */
                  SetXY(&state, x, y);

               if (state.autot) {		/* only update if autotracking is on */
                  shmem->x = x;
                  shmem->y = y;
                  shmem->buttons = buttons;
               }
            }
         } while ((x != oldx || y != oldy || buttons != oldb || set) &&
			(event_count < EVENT_LIMIT));

         set_timer(timer_efn, flag);
      }

      /* Message */

      if (sys$readef(mbox_efn, &flag_state) == SS$_WASSET) {
         sys$clref(mbox_efn);

         process_message(&mess, &state, shmem);

         status = sys$qio(mbox_efn, in_channel, IO$_READVBLK, &iosb, 0,0,
				&mess, sizeof(mess), 0,0,0,0);
         if (status != SS$_NORMAL) {
            sprintf(string, "qio read failed =%d\r\n", status);
            print_me(string);
            exit(status);
         }
      }
   }

   exit(0);			/* Never reached */
}

/************************************************************************/
/* Print a message to the parent process's terminal.  Should only be	*/
/* used in case of fatal error!						*/
/************************************************************************/

print_me(s)
char *s;
{
   sys$qiow(0, out_chan, IO$_WRITEVBLK, 0,0,0,
	"\n\rFatal error from mouse subprocess!!\n\r", 39, 0,0,0,0);
   sys$qiow(0, out_chan, IO$_WRITEVBLK, 0,0,0, s, strlen(s), 0,0,0,0);
}

/************************************************************************/
/* Map to a global section of memory that has been created by the	*/
/* parent process.  If it doesn't exist, it is created, but that should	*/
/* not happen.								*/
/************************************************************************/

char *map_memory(shmem_size, section_name)
int shmem_size;
char *section_name;
{

  int status, size, flags;
  struct dsc$descriptor_s section;
  char *r[2], *s[2];

  /*  Create a name for the shared memory that will be unique for different*/
  /*  processes, devices, and types of shared memory.  The process id is   */
  /*  the id of the parent of the process--so that all processes in the    */
  /*  same process tree have access to the same section of shared memory.  */

  section.dsc$w_length = strlen(section_name);
  section.dsc$b_class = DSC$K_CLASS_S;
  section.dsc$b_dtype = DSC$K_DTYPE_T;
  section.dsc$a_pointer = section_name;

  r[0] = 10;				/* any arbitrary address in P0 space */
  r[1] = 10;
  size = (shmem_size + 511) / 512;	/* compute size in pages */

  flags = SEC$M_WRT | SEC$M_EXPREG | SEC$M_GBL;
  status = sys$mgblsc(r, s, 0, flags, &section, 0, 0);

  /*  If the shared memory has been created by another process in the same */
  /*  process tree, then map to it and return SUCCESS.                     */

  if (status == SS$_NORMAL)
    return s[0];

  /*  If the shared memory has not been created by this or any other pro-  */
  /*  cess in the same process tree, then create it.                       */

  flags = SEC$M_WRT | SEC$M_EXPREG | SEC$M_GBL | SEC$M_PAGFIL | SEC$M_DZRO;

  status = sys$crmpsc(r,s,0,flags,&section,0,0,0,size,0,0,0);

  if ((status == SS$_NORMAL) || (status == SS$_CREATED))
    return s[0];
  else					/* an error occurred */
    return NULL;

}

/************************************************************************/
/* Set the timer for a length of time that depends on how long it's     */
/* been since we last got an event.  Once we get an event, we set the   */
/* timer back up to it's fastest speed (1/10 sec).  After several at    */
/* that speed with no changes, we drop down to 1/2 sec for a while,     */
/* then eventually down to 2 seconds, which is the slowest we go.       */
/************************************************************************/

set_timer(timer_efn, flag)
int timer_efn;                  /* event flag for timer */
int flag;                       /* TRUE if an event happened */
{
   static int speed_level = 0;
   static int ticks = 0;           /* # of ticks at this level */
   int status;

   if (flag) {
      speed_level = 0;
      ticks = 0;
   }
   else {
      ticks++;
      if (ticks > MAX_TICKS && speed_level < SPEED_LEVELS-1) {
         speed_level++;
         ticks = 0;
      }
   }

   status = sys$setimr(timer_efn, bintime[speed_level], 0, TIMER_ID);
   if (status != SS$_NORMAL) {
      sprintf(string, "setimr failed =%d\r\n", status);
      print_me(string);
      exit(status);
   }
}

/************************************************************************/
/* Given a cursor shape, color, and whether it's on the image or	*/
/* overlay plane, create a shape to send to the device (for a GCSR	*/
/* cursor).  The codes in the jup_cursorshape array are translated into	*/
/* actual colors based on the mode and requested cursor color.		*/
/************************************************************************/

make_cursor(state)
struct mouse_state *state;
{
   int colors[4];
   int r, g, b, r2, g2, b2;
   int i, j;

   r  = state->red;      g  = state->green;     b  = state->blue;
   r2 = state->red / 2;  g2 = state->green / 2; b2 = state->blue / 2;

/* Figure out the colors to use for the various modes */
/* 0==transparent, 1==white (or cursor color), 2==half intensity, 3==black */

   colors[0] = 0;

   if (state->curs_overlay) {		/* overlay on */
      colors[1] = state->curs_dn;
      colors[2] = state->half_dn;
      colors[3] = state->black_dn;

      state->mcsr_color = state->curs_dn;
   }

   else {				/* overlay off, use image */
      if (state->disp_mode == RGB24) {
         colors[1] = (b  << 16) | (g  << 8) | r;
         colors[2] = (b2 << 16) | (g2 << 8) | r2;
         colors[3] = 0x00010101;
      }
      else if (state->disp_mode == RGB12) {
         colors[1] = ((b  << 4) & 0x0F00) | (g  & 0x00F0) | (r  >> 4);
         colors[2] = ((b2 << 4) & 0x0F00) | (g2 & 0x00F0) | (r2 >> 4);
         colors[3] = 0x00000111;
      }
      else if (state->disp_mode == CLT8) {
         colors[1] = MAX(r, MAX(g, b));
         colors[2] = colors[1] / 2;
         colors[3] = 1;
      }
      else {					/* CLT4 */
         colors[1] = (MAX(r, MAX(g, b))) >> 4;
         colors[2] = colors[1] / 2;
         colors[3] = 1;
      }
      state->mcsr_color = colors[1];
   }

/* Now create the new cursor shape by running the master through colors[] */

   for (i=0; i<20; i++)
      for (j=0; j<20; j++)
         state->shape_array[i][j] = colors[jup_cursorshape[state->shape][i][j]];

}

/************************************************************************/
/* Given a message from the parent process, do the requested command.	*/
/************************************************************************/

process_message(mess, state, shmem)
JUP_MESSAGE *mess;
struct mouse_state *state;
struct shmem_jup *shmem;
{

   switch (mess->type) {

   case MESS_NEWPOS:
      shmem->x = mess->newpos.x;
      shmem->y = mess->newpos.y;
      SetXY(state, shmem->x, shmem->y);
      break;

   case MESS_AUTO:
      state->autot = mess->autot.onoff;
      if (mess->autot.onoff)			/* on */
         j_attachs(state->mouse, 1, CSR_T(state->mouse), state->mcsr_color,
							state->shape_array);
      else
         j_attachs(state->mouse, 0, CSR_T(state->mouse), state->mcsr_color,
							state->shape_array);
      jfflush(J12file);
      SetXY(state, shmem->x, shmem->y);
      break;

   case MESS_ONOFF:
      state->on = mess->onoff.onflag;
      if (mess->onoff.onflag) {		/* cursor on */
         state->shape = mess->onoff.shape;
         make_cursor(state);
         j_attachs(state->mouse, 1, CSR_T(state->mouse), state->mcsr_color,
							state->shape_array);
         if (CK_GCSR(state->mouse)) {
            j_csrset(state->shape_array);
         }
      }

      SetXY(state, shmem->x, shmem->y);
      break;

   case MESS_NEWCONFIG:
      j_config(mess->newconfig.mode, 1, mess->newconfig.ovbits, 0,
					mess->newconfig.hw);
      state->disp_mode = mess->newconfig.mode;
      if (mess->newconfig.ovon)
         j_ormask(0x0F);	/* Set the display mask to all on for overlay */
      else
         j_ormask(0x00);	/* Set the display mask to all off */
      J_MX = mess->newconfig.mem_x;
      J_MY = mess->newconfig.mem_y;
      J_VX = mess->newconfig.vid_x;
      J_VY = mess->newconfig.vid_y;

      jfflush(J12file);

      SetXY(state, shmem->x, shmem->y);
      break;

   case MESS_OVERLAY:
      state->curs_overlay = mess->overlay.on;
      state->curs_dn = mess->overlay.curs_dn;
      state->half_dn = mess->overlay.half_dn;
      state->black_dn = mess->overlay.black_dn;
      make_cursor(state);
      if (mess->overlay.on)
         j_cursor_sel(OI_OVERLAYS);    /* Overlay available, put cursor there */
      else
         j_cursor_sel(OI_IMAGE);    /* No overlay, put cursor on image planes */
      if (CK_GCSR(state->mouse))
         j_csrset(state->shape_array);
      else
         j_mcpar(state->mouse, state->mcsr_color);
      jfflush(J12file);
      break;

   case MESS_COLOR:
      state->red = mess->color.red;
      state->green = mess->color.green;
      state->blue = mess->color.blue;
      state->curs_dn = mess->color.curs_dn;
      state->half_dn = mess->color.half_dn;
      state->black_dn = mess->color.black_dn;
      make_cursor(state);
      if (CK_GCSR(state->mouse))
         j_csrset(state->shape_array);
      else
         j_mcpar(state->mouse, state->mcsr_color);
      jfflush(J12file);
      break;

   case MESS_PANZOOM:
      state->dw_left = mess->panzoom.dw_left;
      state->dw_top = mess->panzoom.dw_top;
      state->zoom = mess->panzoom.zoom;
      SetXY(state, shmem->x, shmem->y);
      break;

   }

   return;

}


/************************************************************************/
/* Set the mouse to a particular X/Y location, using the appropriate	*/
/* zoom/pan factors.  Called many times because it also sets cursor	*/
/* shape and on/off status.						*/
/************************************************************************/

SetXY(state, x, y)
struct mouse_state *state;
int x, y;
{

   x = (x - 1) / state->zoom + state->dw_left;	   /* Convert to IMP coords */
   y = (y - 1) / state->zoom + state->dw_top;

   while (x > J_MX) x -= J_MX;		/* compensate for wrap */
   while (x < 1) x += J_MX;
   while (y > J_MY) y -= J_MY;
   while (y < 1) y += J_MY;

   x = JUPM_X_IMG(x);
   y = JUPM_Y_IMG(y);

   if (state->on) {
      if (CK_GCSR(state->mouse))		/* Set no-autotrack pos */
         j_dca(x, y, 1);				/* GCSR */
      else
         j_mccsr(x, y, MCSR_ENABLE(state->shape));
   }
   else {
      if (CK_GCSR(state->mouse))
         j_dca(x, y, 0);
      else
         j_mccsr(x, y, 0);
   }
   j_setxy(state->mouse, x, y);		/* Set autotrack (mouse) pos */
   jfflush(J12file);

}

