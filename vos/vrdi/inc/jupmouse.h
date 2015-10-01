#ifndef SHMEM_JUP
#define SHMEM_JUP	13		/* Duplicated in xdexterns.h */
#endif

#define MAX(a,b) (((a) > (b)) ? (a) : (b))

#define	JUPM_X_IMG(x)	((x)-1)
#define	JUPM_Y_IMG(y)	(J_MY-(y))
#define	JUPM_IMG_X(x)	((x)+1)
#define	JUPM_IMG_Y(y)	(J_MY-(y))

#define MESS_INIT	1
#define MESS_QUIT	2
#define MESS_NEWPOS	3
#define MESS_AUTO	4
#define MESS_ONOFF	5
#define MESS_NEWCONFIG	6
#define MESS_OVERLAY	7
#define MESS_COLOR	8
#define MESS_PANZOOM	9

#define HW_SIZE		40	/* max size for "hw" string from video file */

struct iosb {
   unsigned short status;
   unsigned short transfer_count;
   long device_info;
};

/* Messages through mailbox for mouse process */

typedef union {
   int type;
   struct {		/* Initialization message */
      int type;
      int mouse;		/* Mouse number 0,1,etc. */
      int video;		/* Video number 0,1,etc. */
      int gpuno;		/* GPU number 0,1,etc. */
      int mode;			/* RGB24, RGB12, CLT8, CLT4, etc. */
      int ovbits;
      char secname[40];		/* Shared memory section name */
      int curs_dn;		/* used if cursor is on graphics plane */
      int half_dn;
      int black_dn;
      int mem_x;		/* Max X and Y sizes of memory */
      int mem_y;
      int vid_x;		/* X and Y sizes of video */
      int vid_y;
   } init;
   struct {		/* New position message */
      int type;
      int x;
      int y;
   } newpos;
   struct {		/* Autotracking on/off message */
      int type;
      int onoff;
   } autot;
   struct {		/* Cursor on/off/shape/blink message */
      int type;
      int onflag;
      int cursor;
      int shape;
      int rate;
   } onoff;
   struct {		/* New configuration of memory planes message */
      int type;
      int mode;			/* RGB24, RGB12, CLT8, CLT4, etc. */
      int ovbits;		/* # of bits in overlay */
      int ovon;			/* True if overlay on now */
      int mem_x;		/* Max X and Y sizes of memory */
      int mem_y;
      int vid_x;		/* X and Y sizes of video */
      int vid_y;
      char hw[HW_SIZE];		/* "hw" string from video file */
   } newconfig;
   struct {		/* Overlay on/off message */
      int type;
      int on;			/* Ovly stat, select image or graph for curs */
      int curs_dn;		/* used if cursor is on graphics plane */
      int half_dn;
      int black_dn;
   } overlay;			/* NOTE: Caller must also do j_cursor_sel()! */
   struct {		/* Change cursor color */
      int type;
      int red;			/* cursor color */
      int green;
      int blue;
      int curs_dn;		/* used if cursor is on graphics plane */
      int half_dn;
      int black_dn;
   } color;
   struct {		/* New pan or zoom factor for active cursor plane */
      int type;
      int dw_left;
      int dw_top;
      int zoom;
   } panzoom;
} JUP_MESSAGE;

/* Shared memory structure */

struct shmem_jup {
   int x;
   int y;
   int buttons;
};

/* Current state of everything related to the cursor */

struct mouse_state {
   int mouse;
   int on;
   int shape;
   int autot;
   int red;
   int green;
   int blue;
   int curs_dn;
   int half_dn;
   int black_dn;
   int mcsr_color;
   int disp_mode;
   int curs_overlay;
   int dw_left;
   int dw_top;
   int zoom;
   int shape_array[20][20];
};
