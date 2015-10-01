/*	VIDSdefs.h
 *
 *	Standard definitions for VIDS subroutines
 *
 *	Abbreviations/acronyms:
 *		LUT	LookUp Table
 *		IMP	Image Memory Plane
 *		BSQ	Band SeQuential
 *		BIL	Band Interleaved by Line
 *		BIP	Band Interleaved by Pixel
 */
#include	"taeconf.inp"		/* TAE standard defs      */
#include	"comminc.inp"
#include	"parblk.inc"
#include	"errdefs.h"
#include <string.h>


/********************** Functions **************************/

void *malloc();

/********************** Macros **************************/
#ifndef MIN
#define MIN(x,y)	(((x) < (y)) ? (x) : (y))
#endif
#ifndef MAX
#define MAX(x,y)	(((x) > (y)) ? (x) : (y))
#endif
#define ABS(x)		(((x) > 0) ? (x) : -(x))
#define EQUAL(a,b) (strcmp((a),(b)) == 0)
#define SetRect(x,t,l,b,r) {(x)->left=(l);(x)->top=(t);(x)->right=(r);(x)->bottom=(b);}
#define ABORT(S,M,K) {s_copy(M,env->message); s_copy(K,env->key); return S;}
#define GC(c)		(env->graphColors[c])
#define TAEVariable struct VARIABLE
#define VideoLines(env)	(zdsvnl(env->devUnit))	/* # lines in video display   */
#define VideoSamps(env) (zdsvns(env->devUnit))	/* # samples in video display */

/********************** Constants **************************/
#ifdef FAIL
#undef FAIL
#endif
#define FAIL		-2
#define CLIPPED		 2		/* Indicates coordinates out of range */
#ifndef NULL
#define NULL		 ((void *)0)
#endif
#define noUnit		-1		/* no unit number for file (not open) */
#define noLUT		 0		/* Standard lookup table numbers      */
#define redLUT		 1
#define greenLUT	 2
#define blueLUT		 3
#define MAXPLANES	100		/* Maximum no. of planes available    */
#define MAXDEVICES	 1		/* max no. of display devices avail.  */
#define MAXREGIONS	100		/* MAX no. of regions available	      */
#define HSIZE		256		/* size of a histogram		      */
#define MAXDEVNAMESIZE	7		/* max size of a device name	      */

/********************** Enumerated types **************************/
typedef enum {Rectangle=1,Oval,Polygon,Circle,Square,Line,Segments,Arrow}
			ObjectType;
typedef enum {NoColor, Red, Green, Blue, White, Magenta, Yellow, Cyan,
		Black, System, Rubber, Special, LastColor} GraphColor;
typedef enum {NoOrientation, Horizontal, Vertical, Limit} Orientation;
		/* line orientation: Limit restricts the line with a rect     */
typedef enum {False=0,True} Boolean;	/* True=1, False=0		      */
typedef enum {ACCEPT=1, CANCEL, REJECT} ButtonAction;
typedef enum				/* Message types for notifying the    */
{					/* user.			      */
  Silent, Inform, Verbose 
} MessageType;
typedef enum {BSQ,BIL,BIP} FileOrg;	/* file organizations		      */
typedef enum				/* VICAR data formats		      */
{
  ByteFormat=1,HalfFormat,FullFormat,RealFormat,DoubFormat,CompFormat
} FileFormat;

/********************** Structure templates **************************/
typedef struct FunctionDef
{
  float		args[20];	/* list of arguments to the function	*/
  char		buf[980];	/* buffer to hold compiled function	*/
} FunctionDef;

typedef struct Rect
{
  int		top;		/* top of rectangle			*/
  int		left;		/* left side of rectangle		*/
  int		bottom;		/* bottom of rectangle			*/
  int		right;		/* right side of rectangle		*/
} Rect;

typedef struct Point
{
  int		v;		/* vertical coordinate			*/
  int		h;		/* horizontal coordinate		*/
} Point;

typedef struct Region
{
  struct Region	*next;		/* next region in list			*/
  struct Region	*prev;		/* previous region in list		*/
  char	     name[STRINGSIZ+1];	/* Name of the region			*/
  int		seed;		/* integer uniquely identifying region	*/
  Rect		bounds;		/* bounding rectangle of the region	*/
  GraphColor	color;		/* color to draw frame			*/
  ObjectType	type;		/* rectangle,polygon, etc. (see above)	*/
  int		nPoints;	/* number of points in polygon		*/
  Point		*pointList;	/* pointer to a list of points defining	*/
  				/* the vectors in the polygon		*/
  unsigned	temporary : 1;	/* True if region is temporary		*/
} Region;

typedef struct PolyEdge
{
  int ymin;			/* Minimum y-coord of edge		*/
  int ymax;			/* Maximum y-coord of edge		*/
  float x;			/* Current x-coord (starts at ymin)	*/
  float deltax;			/* Increment to x for next scan line	*/
				/* Also ending x-coord for horiz. lines	*/
  Boolean horiz;		/* True iff edge is horizontal		*/
  int xmin, xmax;		/* smallest and largest x coord on line	*/
  struct PolyEdge *next;	/* pointer to next in linked list	*/
} PolyEdge;

typedef union RegionState
{
  struct {			/* Rectangle */
    int		y;		/* current y-coord			*/
  } r;
  struct {			/* Oval */
    int		nPoints;	/* num of points (lines) to calculate	*/
    int		index;		/* current line number			*/
    int		pass;		/* two passes for oval			*/
    float	a, b, b2;	/* dimensions of the oval		*/
  } o;
  struct {			/* Polygon */
    PolyEdge *ET;		/* Pointer to Edge Table		*/
    PolyEdge *AET;		/* Pointer to Active Edge Table		*/
    PolyEdge *nextpair;		/* Pointer to next pair of active edges	*/
    int y;			/* Current y coordinate			*/
    int maxx;			/* Max x-coord so far on current line	*/
				/* (used to avoid duplicates)		*/
  } p;
} RegionState;

typedef struct SizeField
{
  int		sl;		/* starting line			*/
  int		ss;		/* starting sample			*/
  int		nl;		/* number of lines			*/
  int		ns;		/* number of samples			*/
} SizeField;

typedef struct CursorInfo
  {
    int		number;		/* cursor number	    	  */
    int		form;		/* cursor form (shape)	    	  */
    int		blink;		/* cursor blink rate	    	  */
  } CursorInfo;

typedef struct CLut
{
  int		red[256];	/* red lookup table		*/
  int		green[256];	/* green lookup table		*/
  int		blue[256];	/* blue lookup table		*/
} CLut;

typedef struct ColorTriplet
{
  int		red;		/* red value			*/
  int		green;		/* green value			*/
  int		blue;		/* blue value			*/
} ColorTriplet;

typedef struct PSTable
{
  int		red[256];	/* red lookup table		*/
  int		green[256];	/* green lookup table		*/
  int		blue[256];	/* blue lookup table		*/
} PSTable;

typedef struct FileInfo
{
  struct FileInfo	*next;	/* ptr to next file on list		      */
  struct FileInfo	*last;	/* ptr to last file on list		      */
  char	filename[STRINGSIZ+1];	/* Full host file name			      */
  int		fileUnit;	/* Vicar unit number for this file	      */
  unsigned	isOpen : 1;	/* True if unit is currently open	      */
  unsigned char	*addr;		/* pointer to data if array i/o; if addr ==   */
				/*     NIL and isOpen, we must do line i/o    */
  int		nl;		/* No. of lines in file			      */
  int		ns;		/* No. of samples per line in file	      */
  int		nb;		/* No. of bands in file			      */
  FileOrg	org;		/* file org: BSQ, BIL, or BIP		      */
  FileFormat	format;		/* data format: ByteFormat, HalfFormat, etc   */
  struct
  {				/* this struct defines a scale to be applied  */
    float	slope;		/* to this file when loading it.	      */
    float	offset;		/* if both are 0, then the default must be    */
  } scale;			/* used instead (GetDefDataRange()).	      */
} FileInfo;

typedef struct PlaneInfo
{
  int		imp;		/* image memory plane number for this plane   */
  FileInfo	*file;		/* ptr to information about this file	      */
  int		band;		/* Band in file associated with this plane    */
  SizeField	imageWindow;	/* window into the image to be displayed      */
  struct			/* subPixel contains the number of imp pixels */
  {				/* of the first (partial) pixel displayed.    */
    int		left;		/* If = 0 or = softZoom, no subpixeling.      */
    int		top;
  } subPixel;
  SizeField	accessWindow;	/* access window (active area) of image plane */
  int		softZoom;	/* software zoom factor (image to IMP factor) */
  struct
  {
    long	*array;		/* ptr to histogram (NULL if not available)   */
    int		rgnseed;	/* ID number for region used to collect hist  */
				/* (0 means no region, so entire screen used) */
  } hist;
  int		*lut;		/* ptr to lut (NULL if none/ramp)	      */
  PSTable	*pstable;	/* ptr to pseudocolor table (NULL if none)    */
} PlaneInfo;

typedef struct PlaneName		/* Used only in impnames.c	*/
{
  int		imp;			/* image memory plane number	*/
  char		name[STRINGSIZ+1];	/* Logical name for this plane	*/
  struct PlaneName	*last;		/* link to last member of list	*/
  struct PlaneName	*next;		/* link to next member of list	*/
} PlaneName;				/* first name only		*/

typedef struct TextAttr			/* Used only in jtext.c		*/
{
  int		font;			/* font number */
  int		size;			/* height in pixels */
  float		aspect;			/* width/height ratio */
  float		angle;			/* rotation angle */
} TextAttr;

/* There is one environment block per active device.  They are stored in a    */
/* circular double linked list (i.e., no null pointers... the tail points     */
/* back to the head).  There is always exactly one env block that is not in   */
/* use, marked by env->devUnit==noUnit.  When a new device is added (JGRAB),  */
/* the free slot gets used for the new device, and a new free slot is created.*/
/* Likewise, when a device is deleted (JDROP), the free slot is deleted and   */
/* the env for the deleted device becomes the new free slot.  This is done to */
/* simplify list management in vids.c.					      */

typedef struct VIDSEnvironment
{
  struct VIDSEnvironment	*next;	/* Pointer to environ of next device  */
  struct VIDSEnvironment	*prev;	/* Pointer to previous device on list */
  Region		*regionList;	/* List of regions for this device    */
  int			devUnit;	/* unit number of device for this env.*/
  int			nimps;		/* number of IMPs available	      */
  int			nlMax;		/* maximum number of lines per IMP    */
  int			nsMax;		/* max number of pixels per IMP line  */
  int			nButtons;	/* no. of trackball or mouse buttons  */
  int			bwIMP;		/* Image Memory Plane to use for BW   */
  int			redIMP;		/* IMP attached to red plane if color */
  int			greenIMP;	/* IMP attach. to green plane if color*/
  int			blueIMP;	/* IMP attached to blue plane if color*/
  int			grafIMP;	/* IMP to use for graphics plane      */
  float			default_slope;	/* defaults for JSET-RANGE */
  float			default_offset;
  long			bufSize;	/* Size of env.buffer		      */
  unsigned char		*buffer;	/* IMP sized buffer for temp moves    */
					/* NOTE: buffer must be allocated     */
					/* early, and freed/re-allocated each */
					/* time the IMPs are reconfigured.    */
  struct
  {					/* Each member is True if that plane  */
    Boolean		bw,red,green;	/* is turned on.		      */
    Boolean		blue,graf;
  } isOn;
  PlaneInfo	   planes[MAXPLANES+1];	/* Info image planes		      */
  CursorInfo		cursor;		/* Current cursor attritbutes	      */
  PlaneName		planeNames;	/* first in linked list of plane names*/
  TextAttr		UserAttr;	/* attributes for user text	      */
  TextAttr		SystemAttr;	/* attributes for system text	      */
  struct
  {					/* JLIST draws gr box that gets erased*/
					/* on next cmd.  These store box info.*/
    Rect		oldarea;	/* location of box currently on screen*/
    Boolean		oldvalid;	/* True iff there's an old box        */
    unsigned char	oldmask;	/* Mask to use to erase old box	      */
  } listData;
  struct
  {					/* state info for JMOVIE	      */
    int			nlFrame,nsFrame;/* size of a single frame	      */
    int			nFrames;	/* current number of frames	      */
    int			bwFrames;	/* total bw frames available	      */
    int			colorFrames;	/* total number of color frames	      */
    int			movieZoom;	/* zoom for movie		      */
    int			prevFrame;	/* previous frame number for load     */
    Boolean		indPan;		/* True if independent pan	      */
					/* Keys for interactive control:      */
    char	reverseKey;		/* reverse direction		      */
    char	fastKey,slowKey;	/* faster, slower		      */
    char	pauseKey,stepKey;	/* pause, step single frame	      */
  } movie;
  struct
  {					/* save JPROFILE values for 'NOSCALE  */
    int			savednmin;	/* min DN of last profile	      */
    int			savednmax;	/* max DN of last profile	      */
    int			savexmax;	/* max X coordinate for last profile  */
  } profile;
  unsigned char	graphColors[LastColor];	/* Look-up table for graphics colors  */
  unsigned char		drawColor;	/* Current graphics drawing color     */
  unsigned char		drawMask;	/* Current graphics drawing mask      */
  char	     devName[MAXDEVNAMESIZE+1]; /* Device name for this device	      */
  char		  message[STRINGSIZ+1];	/* Error message if required	      */
  char		      key[STRINGSIZ+1];	/* Key for error message if required  */
  unsigned		isColor : 1;	/* TRUE if color mode		      */
  unsigned		isPseudo : 1;	/* TRUE if pseudocolor mode	      */
  unsigned		isBW : 1;	/* TRUE if bw mode		      */
} VIDSEnvironment;
