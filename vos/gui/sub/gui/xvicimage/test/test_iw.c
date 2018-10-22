#include <math.h>
#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
#include <Xm/Separator.h>
#include <Xm/Label.h>
#include "XvicImage.h"

#ifndef PI
#define PI 3.14159265358979323846
#endif

/* #define DPR(x)  printf x */
#define DPR(x)

#ifndef MIN
#define MIN(x,y) ((x)<(y) ? (x) : (y))
#endif

#ifndef ABS
#define ABS(x) ((x)>=0 ? (x) : (-(x)))
#endif

typedef enum {MoveExisting, EraseExisting,
	FillArc, FillArcs, FillPolygon, FillRectangle, FillRectangles,
	Arc, Arcs, Bitmap, ImageString, Line, Lines, Point, Points,
	Rectangle, Rectangles, Segments, DrawString} Shape;
#define MAX_POINTS 100

struct GraphicsStuff {
   XvicColor color, bgcolor;
   XvicGC gc, rubber_gc, new_gc;
   Widget id_widget, last_id_widget, shape_widget, text_widget;
   Widget gc_widget, last_gc_widget;
   XvicID id, rubber_id;
   Shape shape;
   int shape_int;
   XGCValues gc_values;
   unsigned long gc_mask;
   Boolean rubber_mode;
   int justify;
   XtIntervalId timeout_id;
   double x, y;				/* "start" locations */
   double width, height;
   int angle1, angle2;
   int npoints;
   XvicArc arcs[MAX_POINTS+1];
   XvicPoint points[MAX_POINTS+1];
   XvicRectangle rectangles[MAX_POINTS+1];
   XvicSegment segments[MAX_POINTS+1];
   int state;		/* which part of the object is being tracked now */
   unsigned int bitmap_width, bitmap_height;
   Pixmap bitmap;
   int bitmap_hot_x, bitmap_hot_y;
} gbl_gr;

typedef struct _TimerData {
   XEvent event;
   int x, y;
} TimerData;


Widget gbl_iw;
int memory_control;
unsigned char *shared_buf = NULL;
unsigned char *shared_buf2 = NULL;
unsigned char *shared_buf3 = NULL;

double data_min, data_max, data_inc;

Widget w_imageMode;
Widget w_dataType;
Widget w_ditherMode;
Widget w_lutType;
Widget w_lut16Type;
Widget w_stretchPolicy;
Widget w_colormapPolicy;
Widget w_visualType;
Widget w_enableDirectColor;
Widget w_workProcPolicy;
Widget w_dataSavePolicy;
Widget w_constrainPan;
Widget w_bwDither;
Widget w_bwStretchPolicy;
Widget w_bwColormapPolicy;
Widget w_colorDither;
Widget w_colorStretchPolicy;
Widget w_colorColormapPolicy;
Widget w_pseudoDither;
Widget w_pseudoStretchPolicy;
Widget w_pseudoColormapPolicy;
Widget w_xPan;
Widget w_yPan;
Widget w_xSubpixelPan;
Widget w_ySubpixelPan;
Widget w_xZoomIn;
Widget w_xZoomOut;
Widget w_yZoomIn;
Widget w_yZoomOut;
Widget w_maximumMemory;
Widget w_grayLevels;
Widget w_redLevels;
Widget w_greenLevels;
Widget w_blueLevels;
Widget w_rawDataMin;
Widget w_rawDataMax;
Widget w_scaledDataMax;
Widget w_outputDataMax;
Widget w_imageWidth;
Widget w_imageHeight;
Widget w_tileWidth;
Widget w_tileHeight;
Widget w_viewWidth;
Widget w_viewHeight;
Widget w_width;
Widget w_height;
Widget w_xPreSubpixelPan;
Widget w_yPreSubpixelPan;
Widget w_xPreZoomIn;
Widget w_xPreZoomOut;
Widget w_yPreZoomIn;
Widget w_yPreZoomOut;
Widget w_bwVisualType;
Widget w_colorVisualType;
Widget w_pseudoVisualType;
Widget w_cursorMode;
Widget w_scrollBarDisplayPolicy;
Widget w_cursorX;
Widget w_cursorY;
Widget w_cursorXfp;
Widget w_cursorYfp;
Widget w_cursorForeground;
Widget w_cursorBackground;
Widget w_cursor;
Widget w_trackFloatingCursor;
Widget w_input;
Widget w_dataMin;
Widget w_dataMax;
Widget w_dataInc;


Widget gw_color;
Widget gw_bgColor;
Widget gw_text;
Widget gw_textJustify;
Widget gw_bitmap;
Widget gw_erase;
Widget gw_shape;
Widget gw_id;
Widget gw_lastId;
Widget gw_gc;
Widget gw_lastGc;
Widget gw_arcMode;
Widget gw_capStyle;
Widget gw_dashes;
Widget gw_dashOffset;
Widget gw_fillRule;
Widget gw_font;
Widget gw_joinStyle;
Widget gw_lineStyle;
Widget gw_lineWidth;
Widget gw_rubberMode;

/*--------------------------------------------------------------*/

/*!!!! VVVV !!!!*/
void DestroyCallback(w, cp, cb)
Widget w;
XtPointer cp;                       /* Client data */
XtPointer cb;    /* Call data */
{
   XtDestroyWidget(gbl_iw);
}
/*!!!! ^^^^ !!!!*/

void UpdateCallback(w, cp, cb)
Widget w;
XtPointer cp;                       /* Client data */
XtPointer cb;    /* Call data */
{
   reset_defaults(gbl_iw);
}

void EraseOverlayCallback(w, cp, cb)
Widget w;
XtPointer cp;                       /* Client data */
XtPointer cb;    /* Call data */
{
   XvicImageEraseOverlay(gbl_iw);
}

void expose(iw, client_data, call_data)
Widget iw;
XtPointer client_data;
XtPointer call_data;
{
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *)call_data;
   XvicImageData img;
   int x_prezoom_in, x_prezoom_out, y_prezoom_in, y_prezoom_out;
   int x_presub, y_presub;
   int i, j;
   int width, height;
   unsigned char image_mode, data_type;
   int x, y, offs;
   int pixel_size;
   int data_range;

   img.x = cb->x;
   img.y = cb->y;
   img.width = cb->width;
   img.height = cb->height;
   img.memory_control = memory_control;
   img.line_width = cb->prezoom_width;	/* adjust for pixel size later */
   img.start_offset = 0;

   XtVaGetValues(iw,
	XvicNxPreZoomIn, &x_prezoom_in, XvicNxPreZoomOut, &x_prezoom_out,
	XvicNyPreZoomIn, &y_prezoom_in, XvicNyPreZoomOut, &y_prezoom_out,
	XvicNxPreSubpixelPan, &x_presub, XvicNyPreSubpixelPan, &y_presub,
	XvicNimageWidth, &width, XvicNimageHeight, &height,
	XvicNimageMode, &image_mode, XvicNdataType, &data_type,
	NULL);

   if (data_type == XvicREAL || data_type == XvicDOUBLE)
      data_range = (data_max - data_min) / data_inc;
   else
      data_range = (data_max + 1 - data_min) / data_inc;
   if (data_range == 0)
      data_range = 1;		/* avoid divide by 0 */

   switch (data_type) {
      case XvicBYTE: pixel_size = sizeof(XvicByte); break;
      case XvicHALF: pixel_size = sizeof(XvicHalf); break;
      case XvicUHALF: pixel_size = sizeof(XvicUHalf); break;
      case XvicFULL: pixel_size = sizeof(XvicFull); break;
      case XvicUFULL: pixel_size = sizeof(XvicUFull); break;
      case XvicREAL: pixel_size = sizeof(XvicReal); break;
      case XvicDOUBLE: pixel_size = sizeof(XvicDouble); break;
      default: printf("Bad data type in test program!\n"); exit(1);
   }
   img.line_width = cb->prezoom_width * pixel_size;

   if (memory_control == XvicMEMORY_SHARED) {
      width = ((width+1)*x_prezoom_in-1)/x_prezoom_out;
      height = ((height+1)*y_prezoom_in-1)/y_prezoom_out;
      img.line_width = width * pixel_size;;
      if (!shared_buf) {
         shared_buf = (unsigned char *)malloc(width*height*pixel_size);
         shared_buf2 = (unsigned char *)malloc(width*height*pixel_size);
         shared_buf3 = (unsigned char *)malloc(width*height*pixel_size);
         if (shared_buf==NULL || shared_buf2==NULL || shared_buf3==NULL) {
            printf("Out of memory in test program!\n");
            exit(1);
         }
      }
   }

   if (image_mode == XvicBW) {
      if (memory_control==XvicMEMORY_SHARED) {
         img.bw_pixels=shared_buf + cb->prezoom_y*img.line_width;
         img.start_offset = cb->prezoom_x * pixel_size;
      }
      else {
         img.bw_pixels=(unsigned char *)malloc(cb->prezoom_width*cb->prezoom_height * pixel_size);
         if (img.bw_pixels == NULL) {
            printf("Out of memory in test program!\n");
            exit(1);
         }
      }

#define BWLOOP(DATATYPE)						   \
      for (i=0; i<cb->prezoom_height; i++) {				   \
         for (j=0; j<cb->prezoom_width; j++) {				   \
            *(DATATYPE *)(img.bw_pixels + i*img.line_width +		   \
				j*pixel_size+img.start_offset) =	   \
	      ((((i+cb->prezoom_y)*y_prezoom_out-y_presub)/y_prezoom_in +  \
		((j+cb->prezoom_x)*x_prezoom_out-x_presub)/x_prezoom_in) % \
                data_range) * data_inc + data_min;			   \
            }								   \
         }

      switch (data_type) {
         case XvicBYTE:
            BWLOOP(XvicByte)
            break;
         case XvicHALF:
            BWLOOP(XvicHalf)
            break;
         case XvicUHALF:
            BWLOOP(XvicUHalf)
            break;
         case XvicFULL:
            BWLOOP(XvicFull)
            break;
         case XvicUFULL:
            BWLOOP(XvicUFull)
            break;
         case XvicREAL:
            BWLOOP(XvicReal)
            break;
         case XvicDOUBLE:
            BWLOOP(XvicDouble)
            break;
      }
   }
   else {			/* Color */
      if (memory_control==XvicMEMORY_SHARED) {
         img.red_pixels=shared_buf + cb->prezoom_y * img.line_width;
         img.grn_pixels=shared_buf2 + cb->prezoom_y * img.line_width;
         img.blu_pixels=shared_buf3 + cb->prezoom_y * img.line_width;
         img.start_offset = cb->prezoom_x;
      }
      else {
         img.red_pixels=(unsigned char *)malloc(cb->prezoom_width*cb->prezoom_height * pixel_size);
         img.grn_pixels=(unsigned char *)malloc(cb->prezoom_width*cb->prezoom_height * pixel_size);
         img.blu_pixels=(unsigned char *)malloc(cb->prezoom_width*cb->prezoom_height * pixel_size);
         if (img.red_pixels==NULL || img.grn_pixels==NULL || img.blu_pixels==NULL) {
            printf("Out of memory in test program!\n");
            exit(1);
         }
      }

#define COLORLOOP(DATATYPE)						     \
      for (i=0; i<cb->prezoom_height; i++) {				     \
         for (j=0; j<cb->prezoom_width; j++) {				     \
            y=((i+cb->prezoom_y)*y_prezoom_out-y_presub)/y_prezoom_in;	     \
            x=((j+cb->prezoom_x)*x_prezoom_out-x_presub)/x_prezoom_in;	     \
            offs = i*img.line_width+j*pixel_size+img.start_offset;	     \
            *(DATATYPE *)(img.red_pixels+offs) = ((x + y) % data_range) *    \
						data_inc + data_min;	     \
            *(DATATYPE *)(img.grn_pixels+offs) = (abs(y - x) % data_range) * \
						data_inc + data_min;	     \
            *(DATATYPE *)(img.blu_pixels+offs) =			     \
		(((x+y*2)%data_range) * data_inc + data_min +		     \
		 ((x*2+y)%data_range) * data_inc + data_min) / 2;	     \
            /* *(DATATYPE *)(img.blu_pixels+offs) = ((((x+y*2) + (x*2+y)) / 2)*/  \
			/*		% data_range) * data_inc + data_min; */ \
         }								     \
      }

      switch (data_type) {
         case XvicBYTE:
            COLORLOOP(XvicByte)
            break;
         case XvicHALF:
            COLORLOOP(XvicHalf)
            break;
         case XvicUHALF:
            COLORLOOP(XvicUHalf)
            break;
         case XvicFULL:
            COLORLOOP(XvicFull)
            break;
         case XvicUFULL:
            COLORLOOP(XvicUFull)
            break;
         case XvicREAL:
            COLORLOOP(XvicReal)
            break;
         case XvicDOUBLE:
            COLORLOOP(XvicDouble)
            break;
      }

   }

   XvicImageWrite(iw, &img, FALSE);

   if (memory_control==XvicMEMORY_APPLIC) {
      if (image_mode == XvicBW)
         free(img.bw_pixels);
      else {
         free(img.red_pixels);
         free(img.grn_pixels);
         free(img.blu_pixels);
      }
   }
}

void resize(iw, client_data, call_data)
Widget iw;
XtPointer client_data;
XtPointer call_data;
{
   reset_defaults(gbl_iw);
}

void pan(iw, client_data, call_data)
Widget iw;
XtPointer client_data;
XtPointer call_data;
{
   set_int_default(w_xPan, XvicNxPan);
   set_int_default(w_yPan, XvicNyPan);
}

void work_proc_active(iw, client_data, call_data)
Widget iw;
XtPointer client_data;
XtPointer call_data;
{
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *)call_data;
   Pixel fg, bg;

   if (cb->reason != XvicCR_WORK_PROC_ACTIVE)
      return;
   /* Invert colors on WorkProcPolicy widget to indicate state */
   XtVaGetValues(w_workProcPolicy, XmNbackground, &bg, XmNforeground, &fg, NULL);
   XtVaSetValues(w_workProcPolicy, XmNbackground, fg, XmNforeground, bg, NULL);
}

/************************************************************************/
/* Graphics drawing stuff.  Implements a simple editor.			*/
/************************************************************************/

/* If the timer expires while still dragging, pan a little bit and	*/
/* invoke the Input callback again.					*/

void timer_proc(client_data, id)
XtPointer client_data;
XtIntervalId *id;
{
   TimerData *td = (TimerData *)client_data;
   String params[2] = {"Draw", "drag"};
   int x_pan, y_pan;
   int x1, y1, x2, y2;

   XvicImageDisplayBounds(gbl_iw, &x1, &y1, &x2, &y2);

   XtVaGetValues(gbl_iw, XvicNxPan, &x_pan, XvicNyPan, &y_pan, NULL);

   if (td->x < x1)
      x_pan -= (x1 - td->x);
   else if (td->x > x2)
      x_pan += (td->x - x2);
   if (td->y < y1)
      y_pan -= (y1 - td->y);
   else if (td->y > y2)
      y_pan += (td->y - y2);

   XtVaSetValues(gbl_iw, XvicNxPan, x_pan, XvicNyPan, y_pan, NULL);

   XtCallActionProc(gbl_iw, "Input", (XEvent *)client_data, params, 2);
}

/* Get the angle from the position given the bounding box */

int get_angle(x_left, y_top, width, height, x, y)
double x_left, y_top;
double width, height;
double x, y;
{
   double xc, yc;		/* center */
   double angle;

   xc = x_left + (width/2.0);
   yc = y_top + (height/2.0);

   angle = atan2(yc-y, x-xc);

   return (int) ((angle * 180.0 / PI) * 64.0);
}

void start_object(x, y)
double x, y;
{
   gbl_gr.x = x;
   gbl_gr.y = y;

   switch (gbl_gr.shape) {
      case FillArc:
      case Arc:
         gbl_gr.width = 0;
         gbl_gr.height = 0;
         gbl_gr.angle1 = 0;
         gbl_gr.angle2 = 360*64;
         gbl_gr.state = 1;
         break;

      case FillArcs:
      case Arcs:
         gbl_gr.npoints = 0;
         gbl_gr.arcs[0].x = x;
         gbl_gr.arcs[0].y = y;
         gbl_gr.arcs[0].width = 0;
         gbl_gr.arcs[0].height = 0;
         gbl_gr.arcs[0].angle1 = 0;
         gbl_gr.arcs[0].angle2 = 360*64;
         gbl_gr.state = 1;
         break;

      case FillPolygon:
      case Lines:
      case Points:
         gbl_gr.npoints = 1;
         gbl_gr.points[0].x = x;
         gbl_gr.points[0].y = y;
         break;

      case FillRectangles:
      case Rectangles:
         gbl_gr.npoints = 0;
         gbl_gr.rectangles[0].x = x;
         gbl_gr.rectangles[0].y = y;
         gbl_gr.rectangles[0].width = 0;
         gbl_gr.rectangles[0].height = 0;
         gbl_gr.state = 1;
         break;

      case Segments:
         gbl_gr.npoints = 0;
         gbl_gr.segments[0].x1 = x;
         gbl_gr.segments[0].x2 = x;
         gbl_gr.segments[0].y1 = y;
         gbl_gr.segments[0].y2 = y;
         gbl_gr.state = 1;
         break;

      default:
         break;
   }

}

/* Add a point to an existing object */

void point_object(x, y, use_rubber)
double x, y;
int use_rubber;
{
   switch (gbl_gr.shape) {

      case FillArc:
      case Arc:
         switch (gbl_gr.state) {
            case 0:			/* starting arc */
               gbl_gr.x = x;
               gbl_gr.y = y;
               gbl_gr.width = gbl_gr.height = gbl_gr.angle1 = 0;
               gbl_gr.angle2 = 360*64;
               gbl_gr.state = 1;
               break;
            case 1:			/* sizing rectangle */
               gbl_gr.width = x - gbl_gr.x + 1.0;
               gbl_gr.height = y - gbl_gr.y + 1.0;
               gbl_gr.state = 2;
               break;
            case 2:			/* first angle */
               gbl_gr.angle1 = get_angle(gbl_gr.x, gbl_gr.y,
			gbl_gr.width, gbl_gr.height, x, y);
               gbl_gr.angle2 = 360*64;	/* make it easy to get a circle */
               gbl_gr.state = 3;
               break;
            case 3:			/* second angle */
               gbl_gr.angle2 = get_angle(gbl_gr.x, gbl_gr.y,
			gbl_gr.width, gbl_gr.height, x, y) - gbl_gr.angle1;
               if (gbl_gr.angle2 <= 0)
                  gbl_gr.angle2 += 360*64;
               break;
         }
         break;
 
      case FillArcs:
      case Arcs:
         switch (gbl_gr.state) {
            case 0:			/* starting arc */
               gbl_gr.arcs[gbl_gr.npoints].x = x;
               gbl_gr.arcs[gbl_gr.npoints].y = y;
               gbl_gr.arcs[gbl_gr.npoints].width = 0;
               gbl_gr.arcs[gbl_gr.npoints].height = 0;
               gbl_gr.arcs[gbl_gr.npoints].angle1 = 0;
               gbl_gr.arcs[gbl_gr.npoints].angle2 = 360*64;
               gbl_gr.state = 1;
               break;
            case 1:			/* sizing rectangle */
               gbl_gr.arcs[gbl_gr.npoints].width =
					x - gbl_gr.arcs[gbl_gr.npoints].x + 1.0;
               gbl_gr.arcs[gbl_gr.npoints].height =
					y - gbl_gr.arcs[gbl_gr.npoints].y + 1.0;
               gbl_gr.state = 2;
               break;
            case 2:			/* first angle */
               gbl_gr.arcs[gbl_gr.npoints].angle1 =
			get_angle(gbl_gr.arcs[gbl_gr.npoints].x,
				gbl_gr.arcs[gbl_gr.npoints].y,
				gbl_gr.arcs[gbl_gr.npoints].width,
				gbl_gr.arcs[gbl_gr.npoints].height, x, y);
               /* make it easy to get a circle */
               gbl_gr.arcs[gbl_gr.npoints].angle2 = 360*64;
               gbl_gr.state = 3;
               break;
            case 3:			/* second angle */
               gbl_gr.arcs[gbl_gr.npoints].angle2 =
			get_angle(gbl_gr.arcs[gbl_gr.npoints].x,
			gbl_gr.arcs[gbl_gr.npoints].y,
			gbl_gr.arcs[gbl_gr.npoints].width,
			gbl_gr.arcs[gbl_gr.npoints].height, x, y) -
				gbl_gr.arcs[gbl_gr.npoints].angle1;
               if (gbl_gr.arcs[gbl_gr.npoints].angle2 <= 0)
                  gbl_gr.arcs[gbl_gr.npoints].angle2 += 360*64;
               if (gbl_gr.npoints < MAX_POINTS)
                  gbl_gr.npoints++;
               gbl_gr.state = 0;
               break;
         }
         break;
 
      case FillPolygon:
      case Lines:
      case Points:
         if (gbl_gr.npoints < MAX_POINTS-1) {
            gbl_gr.points[gbl_gr.npoints].x = x;
            gbl_gr.points[gbl_gr.npoints].y = y;
            gbl_gr.npoints++;
         }
         break;

      case FillRectangles:
      case Rectangles:
         if (gbl_gr.state) {	/* state==1, finish this rect, prep for next */
            if (x < gbl_gr.rectangles[gbl_gr.npoints].x ||
                y < gbl_gr.rectangles[gbl_gr.npoints].y)
               printf("Warning: Rectangles must be pulled down-right\n");
            gbl_gr.rectangles[gbl_gr.npoints].width =
			x - gbl_gr.rectangles[gbl_gr.npoints].x + 1.0;
            gbl_gr.rectangles[gbl_gr.npoints].height =
			y - gbl_gr.rectangles[gbl_gr.npoints].y + 1.0;
            if (gbl_gr.npoints < MAX_POINTS)
               gbl_gr.npoints++;
            gbl_gr.state = 0;
         }
         else {		/* state==0, starting new rect */
            gbl_gr.rectangles[gbl_gr.npoints].x = x;
            gbl_gr.rectangles[gbl_gr.npoints].y = y;
            gbl_gr.rectangles[gbl_gr.npoints].width = 0;
            gbl_gr.rectangles[gbl_gr.npoints].height = 0;
            gbl_gr.state = 1;
         }
         break;

      case Segments:
         if (gbl_gr.state) {	/* state==1, finish this seg, prep for next */
            gbl_gr.segments[gbl_gr.npoints].x2 = x;
            gbl_gr.segments[gbl_gr.npoints].y2 = y;
            if (gbl_gr.npoints < MAX_POINTS)
               gbl_gr.npoints++;
            gbl_gr.state = 0;
         }
         else {		/* state==0, starting new segment */
            gbl_gr.segments[gbl_gr.npoints].x1 = x;
            gbl_gr.segments[gbl_gr.npoints].y1 = y;
            gbl_gr.segments[gbl_gr.npoints].x2 = x;
            gbl_gr.segments[gbl_gr.npoints].y2 = y;
            gbl_gr.state = 1;
         }

      default:
         break;
   }
}

void draw_object(x, y, use_rubber)
double x, y;
int use_rubber;
{
   XvicGC gc;
   XvicID id;
   double h, w;
   double xx, yy;
   char buf[20];
   char *string;

   if (use_rubber) {
      gc = gbl_gr.rubber_gc;
      id = gbl_gr.rubber_id;
   }
   else {
      gc = gbl_gr.gc;
      id = gbl_gr.id;
   }

   switch (gbl_gr.shape) {
      case FillArc:
      case Arc:
         switch (gbl_gr.state) {
            case 0:			/* starting arc */
               gbl_gr.x = x;
               gbl_gr.y = y;
               gbl_gr.width = gbl_gr.height = gbl_gr.angle1 = 0;
               gbl_gr.angle2 = 360*64;
               break;
            case 1:			/* sizing rectangle */
               gbl_gr.width = x - gbl_gr.x + 1.0;
               gbl_gr.height = y - gbl_gr.y + 1.0;
               break;
            case 2:			/* first angle */
               gbl_gr.angle1 = get_angle(gbl_gr.x, gbl_gr.y,
			gbl_gr.width, gbl_gr.height, x, y);
               gbl_gr.angle2 = 315*64;	/* let user see the angle */
               break;
            case 3:			/* second angle */
               gbl_gr.angle2 = get_angle(gbl_gr.x, gbl_gr.y,
			gbl_gr.width, gbl_gr.height, x, y) - gbl_gr.angle1;
               if (gbl_gr.angle2 <= 0)
                  gbl_gr.angle2 += 360*64;
               break;
         }
         if (gbl_gr.shape == Arc)
            id = XvicImageDrawArc(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.x, gbl_gr.y, gbl_gr.width, gbl_gr.height,
		gbl_gr.angle1, gbl_gr.angle2);
         else
            id = XvicImageFillArc(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.x, gbl_gr.y, gbl_gr.width, gbl_gr.height,
		gbl_gr.angle1, gbl_gr.angle2);
         break;

      case FillArcs:
      case Arcs:
         switch (gbl_gr.state) {
            case 0:			/* starting arc */
               gbl_gr.arcs[gbl_gr.npoints].x = x;
               gbl_gr.arcs[gbl_gr.npoints].y = y;
               gbl_gr.arcs[gbl_gr.npoints].width = 0;
               gbl_gr.arcs[gbl_gr.npoints].height = 0;
               gbl_gr.arcs[gbl_gr.npoints].angle1 = 0;
               gbl_gr.arcs[gbl_gr.npoints].angle2 = 360*64;
               break;
            case 1:			/* sizing rectangle */
               gbl_gr.arcs[gbl_gr.npoints].width =
					x - gbl_gr.arcs[gbl_gr.npoints].x + 1.0;
               gbl_gr.arcs[gbl_gr.npoints].height =
					y - gbl_gr.arcs[gbl_gr.npoints].y + 1.0;
               break;
            case 2:			/* first angle */
               gbl_gr.arcs[gbl_gr.npoints].angle1 =
			get_angle(gbl_gr.arcs[gbl_gr.npoints].x,
				gbl_gr.arcs[gbl_gr.npoints].y,
				gbl_gr.arcs[gbl_gr.npoints].width,
				gbl_gr.arcs[gbl_gr.npoints].height, x, y);
               /* let user see the angle */
               gbl_gr.arcs[gbl_gr.npoints].angle2 = 315*64;
               break;
            case 3:			/* second angle */
               gbl_gr.arcs[gbl_gr.npoints].angle2 =
			get_angle(gbl_gr.arcs[gbl_gr.npoints].x,
			gbl_gr.arcs[gbl_gr.npoints].y,
			gbl_gr.arcs[gbl_gr.npoints].width,
			gbl_gr.arcs[gbl_gr.npoints].height, x, y) -
				gbl_gr.arcs[gbl_gr.npoints].angle1;
               if (gbl_gr.arcs[gbl_gr.npoints].angle2 <= 0)
                  gbl_gr.arcs[gbl_gr.npoints].angle2 += 360*64;
               break;
         }
         if (gbl_gr.shape == Arcs)
            id = XvicImageDrawArcs(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.arcs, gbl_gr.npoints+1);
         else
            id = XvicImageFillArcs(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.arcs, gbl_gr.npoints+1);
         break;
 
      case Bitmap:
         id = XvicImageDrawBitmap(gbl_iw, id, gc, gbl_gr.color,
		x, y, gbl_gr.bitmap, gbl_gr.bitmap_width, gbl_gr.bitmap_height,
		gbl_gr.bitmap_hot_x, gbl_gr.bitmap_hot_y);
         break;

      case Line:
         id = XvicImageDrawLine(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.x, gbl_gr.y, x, y);
         break;

      case FillPolygon:
      case Lines:
      case Points:
         gbl_gr.points[gbl_gr.npoints].x = x;
         gbl_gr.points[gbl_gr.npoints].y = y;
         if (gbl_gr.shape == Lines)
            id = XvicImageDrawLines(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.points, gbl_gr.npoints+1, CoordModeOrigin);
         else if (gbl_gr.shape == FillPolygon)
            id = XvicImageFillPolygon(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.points, gbl_gr.npoints+1, Complex, CoordModeOrigin);
         else		/* Points */
            id = XvicImageDrawPoints(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.points, gbl_gr.npoints+1, CoordModeOrigin);
         break;

      case Point:
         id = XvicImageDrawPoint(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.x, gbl_gr.y);
         break;

      case FillRectangle:
      case Rectangle:
         xx = MIN(gbl_gr.x, x);
         yy = MIN(gbl_gr.y, y);
         h = ABS(x - gbl_gr.x) + 1.0;
         w = ABS(y - gbl_gr.y) + 1.0;
         if (gbl_gr.shape == Rectangle)
            id = XvicImageDrawRectangle(gbl_iw, id, gc, gbl_gr.color,
		xx, yy, h, w);
         else
            id = XvicImageFillRectangle(gbl_iw, id, gc, gbl_gr.color,
		xx, yy, h, w);
         break;

      case FillRectangles:
      case Rectangles:
         if (gbl_gr.state) {			/* second part of rect */
            if (x < gbl_gr.rectangles[gbl_gr.npoints].x ||
                y < gbl_gr.rectangles[gbl_gr.npoints].y)
               printf("Warning: Rectangles must be pulled down-right\n");
            gbl_gr.rectangles[gbl_gr.npoints].width =
			x - gbl_gr.rectangles[gbl_gr.npoints].x + 1.0;
            gbl_gr.rectangles[gbl_gr.npoints].height =
			y - gbl_gr.rectangles[gbl_gr.npoints].y + 1.0;
         }
         else {				/* first part of rect */
            gbl_gr.rectangles[gbl_gr.npoints].x = x;
            gbl_gr.rectangles[gbl_gr.npoints].y = y;
            gbl_gr.rectangles[gbl_gr.npoints].width = 0;
            gbl_gr.rectangles[gbl_gr.npoints].height = 0;
         }
         if (gbl_gr.shape == Rectangles)
            id = XvicImageDrawRectangles(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.rectangles, gbl_gr.npoints+1);
         else
            id = XvicImageFillRectangles(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.rectangles, gbl_gr.npoints+1);
         break;

      case Segments:
         if (gbl_gr.state) {			/* second part of line */
            gbl_gr.segments[gbl_gr.npoints].x2 = x;
            gbl_gr.segments[gbl_gr.npoints].y2 = y;
         }
         else {				/* first part of line */
            gbl_gr.segments[gbl_gr.npoints].x1 = x;
            gbl_gr.segments[gbl_gr.npoints].y1 = y;
            gbl_gr.segments[gbl_gr.npoints].x2 = x;
            gbl_gr.segments[gbl_gr.npoints].y2 = y;
         }
         id = XvicImageDrawSegments(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.segments, gbl_gr.npoints+1);
         break;

      case DrawString:
         XtVaGetValues(gbl_gr.text_widget, XmNvalue, &string, NULL);
         id = XvicImageDrawString(gbl_iw, id, gc, gbl_gr.color,
		x, y, string, strlen(string), gbl_gr.justify);
         XtFree(string);
         break;

      case ImageString:
         XtVaGetValues(gbl_gr.text_widget, XmNvalue, &string, NULL);
         id = XvicImageDrawImageString(gbl_iw, id, gc, gbl_gr.color,
		gbl_gr.bgcolor, x, y, string, strlen(string), gbl_gr.justify);
         XtFree(string);
         break;
   }

   if (use_rubber) {
      gbl_gr.rubber_id = id;
   }
   else {
      sprintf(buf, "%d", id);
      XtVaSetValues(gbl_gr.last_id_widget, XmNvalue, buf, NULL);
   }
}

void input(iw, client_data, call_data)
Widget iw;
XtPointer client_data;
XtPointer call_data;
{
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *)call_data;
   static TimerData td;

   if (cb->input_num_params > 0)
      XtVaSetValues(w_input, XmNvalue, cb->input_params[0], NULL);
   else
      XtVaSetValues(w_input, XmNvalue, "", NULL);

   if (cb->input_num_params > 0) {
      if (strcmp(cb->input_params[0], "Draw") == 0) {
         if (gbl_gr.timeout_id) {
            XtRemoveTimeOut(gbl_gr.timeout_id);
            gbl_gr.timeout_id = 0;
         }
         if (cb->input_num_params > 1) {
            if (strcmp(cb->input_params[1], "start") == 0) {
               if (gbl_gr.shape == MoveExisting) {
                  gbl_gr.x = cb->x_fp;
                  gbl_gr.y = cb->y_fp;
               }
               else
                  start_object(cb->x_fp, cb->y_fp);
               gbl_gr.rubber_id = 0;
            }
            else if (strcmp(cb->input_params[1], "drag") == 0) {
               if (gbl_gr.shape == MoveExisting) {
                  XvicImageMoveObject(iw, gbl_gr.id, (cb->x_fp-gbl_gr.x),
						     (cb->y_fp-gbl_gr.y));
                  gbl_gr.x = cb->x_fp;
                  gbl_gr.y = cb->y_fp;
               }
               else {
                  if (gbl_gr.rubber_id) {
                     XvicImageEraseObject(iw, gbl_gr.rubber_id);
                     gbl_gr.rubber_id = 0;
                  }
                  draw_object(cb->x_fp, cb->y_fp, True);
               }
               if (!cb->on_screen) {		/* Set up an auto-pan */
                  memcpy((void *)&td.event, (void *)cb->event, sizeof(XEvent));
                  td.x = cb->x;
                  td.y = cb->y;
                  gbl_gr.timeout_id = XtAppAddTimeOut(XtWidgetToApplicationContext(iw),
			500, timer_proc, &td);
               }
            }
            else if (strcmp(cb->input_params[1], "mark") == 0) {
               if (gbl_gr.shape != MoveExisting) {
                  if (gbl_gr.rubber_id) {
                     XvicImageEraseObject(iw, gbl_gr.rubber_id);
                     gbl_gr.rubber_id = 0;
                  }
                  draw_object(cb->x_fp, cb->y_fp, True);
                  point_object(cb->x_fp, cb->y_fp, True);
               }
            }
            else if (strcmp(cb->input_params[1], "end") == 0) {
               if (gbl_gr.shape == MoveExisting)
                  XvicImageMoveObject(iw, gbl_gr.id, (cb->x_fp-gbl_gr.x),
						     (cb->y_fp-gbl_gr.y));
               else {
                  if (gbl_gr.rubber_id) {
                     XvicImageEraseObject(iw, gbl_gr.rubber_id);
                     gbl_gr.rubber_id = 0;
                  }
                  draw_object(cb->x_fp, cb->y_fp, False);
               }
            }
         }
      }
   }
}

void cursor(iw, client_data, call_data)
Widget iw;
XtPointer client_data;
XtPointer call_data;
{
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *)call_data;
   char buf[20];
   XmHighlightMode mode;

   sprintf(buf, "%0d", cb->x);
   XtVaSetValues(w_cursorX, XmNvalue, buf, NULL);
   sprintf(buf, "%0d", cb->y);
   XtVaSetValues(w_cursorY, XmNvalue, buf, NULL);

   sprintf(buf, "%g", cb->x_fp);
   XtVaSetValues(w_cursorXfp, XmNvalue, buf, NULL);
   sprintf(buf, "%g", cb->y_fp);
   XtVaSetValues(w_cursorYfp, XmNvalue, buf, NULL);

   if (cb->on_screen)
      mode = XmHIGHLIGHT_NORMAL;
   else
      mode = XmHIGHLIGHT_SELECTED;

   XmTextSetHighlight(w_cursorX, 0, 20, mode);
   XmTextSetHighlight(w_cursorY, 0, 20, mode);
}

/*--------------------------------------------------------------*/

void CB_imageMode(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicCOLOR, XvicBW};
   XtVaSetValues(gbl_iw, XvicNimageMode, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void set_data_range(type)
unsigned char type;
{
   char buf[20];

   /* Change data ranges to default */
   switch(type) {
      case XvicBYTE:
         data_min = 0; data_max = 255; data_inc = 1; break;
      case XvicHALF:
         data_min = -32768; data_max = 32767; data_inc = 200; break;
      case XvicUHALF:
         data_min = 0; data_max = 65535; data_inc = 100; break;
      case XvicFULL:
         data_min = -50000; data_max = 50000; data_inc = 100; break;
      case XvicUFULL:
         data_min = 0; data_max = 100000; data_inc = 200; break;
      case XvicREAL:
         data_min = 0.0; data_max = 1.0; data_inc = .001; break;
      case XvicDOUBLE:
         data_min = -100.0; data_max = 100.0; data_inc = .2; break;
   }
   sprintf(buf, "%0.10lf", data_min);
   XtVaSetValues(w_dataMin, XmNvalue, buf, NULL);
   sprintf(buf, "%0.10lf", data_max);
   XtVaSetValues(w_dataMax, XmNvalue, buf, NULL);
   sprintf(buf, "%0.10lf", data_inc);
   XtVaSetValues(w_dataInc, XmNvalue, buf, NULL);
}

void CB_dataType(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicBYTE, XvicHALF, XvicUHALF, XvicFULL,
				    XvicUFULL, XvicREAL, XvicDOUBLE};
   set_data_range(values[(int)btn]);
   XtVaSetValues(gbl_iw, XvicNdataType, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_ditherMode(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicNONE, XvicORDERED, XvicKAGELS};
   XtVaSetValues(gbl_iw, XvicNditherMode, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_lutType(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicSTRETCH, XvicRAW, XvicPSEUDO, XvicPSEUDO_ONLY};
   XtVaSetValues(gbl_iw, XvicNlutType, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_lut16Type(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicSTRETCH, XvicRAW, XvicPSEUDO, XvicPSEUDO_ONLY};
   XtVaSetValues(gbl_iw, XvicNlut16Type, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_stretchPolicy(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicUSE_HW, XvicUSE_SW};
   XtVaSetValues(gbl_iw, XvicNstretchPolicy, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_colormapPolicy(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicFULL, XvicHALF, XvicDITHER, XvicALLOC, XvicFULL_COLOR};
   XtVaSetValues(gbl_iw, XvicNcolormapPolicy, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_visualType(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicUSE_DEFAULT, XvicUSE_8BIT, XvicUSE_24BIT};
   XtVaSetValues(gbl_iw, XvicNvisualType, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_workProcPolicy(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicNONE, XvicREAD, XvicALL};
   XtVaSetValues(gbl_iw, XvicNworkProcPolicy, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_dataSavePolicy(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicNONE, XvicRAW, XvicXIMAGE, XvicPIXMAP};
   XtVaSetValues(gbl_iw, XvicNdataSavePolicy, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_constrainPan(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicNONE, XvicX_ONLY, XvicY_ONLY, XvicBOTH};
   XtVaSetValues(gbl_iw, XvicNconstrainPan, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

/*--------------------------------------------------------------*/

void CB_bwDither(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicNONE, XvicORDERED, XvicKAGELS};
   XtVaSetValues(gbl_iw, XvicNbwDither, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_colorDither(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicNONE, XvicORDERED, XvicKAGELS};
   XtVaSetValues(gbl_iw, XvicNcolorDither, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_pseudoDither(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicNONE, XvicORDERED, XvicKAGELS};
   XtVaSetValues(gbl_iw, XvicNpseudoDither, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_bwStretchPolicy(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicUSE_HW, XvicUSE_SW};
   XtVaSetValues(gbl_iw, XvicNbwStretchPolicy, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_colorStretchPolicy(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicUSE_HW, XvicUSE_SW};
   XtVaSetValues(gbl_iw, XvicNcolorStretchPolicy, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_pseudoStretchPolicy(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicUSE_HW, XvicUSE_SW};
   XtVaSetValues(gbl_iw, XvicNpseudoStretchPolicy, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_bwColormapPolicy(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicFULL, XvicHALF, XvicDITHER, XvicALLOC, XvicFULL_COLOR};
   XtVaSetValues(gbl_iw, XvicNbwColormapPolicy, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_colorColormapPolicy(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicFULL, XvicHALF, XvicDITHER, XvicALLOC, XvicFULL_COLOR};
   XtVaSetValues(gbl_iw, XvicNcolorColormapPolicy, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_pseudoColormapPolicy(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicFULL, XvicHALF, XvicDITHER, XvicALLOC, XvicFULL_COLOR};
   XtVaSetValues(gbl_iw, XvicNpseudoColormapPolicy, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_bwVisualType(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicUSE_DEFAULT, XvicUSE_8BIT, XvicUSE_24BIT};
   XtVaSetValues(gbl_iw, XvicNbwVisualType, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_colorVisualType(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicUSE_DEFAULT, XvicUSE_8BIT, XvicUSE_24BIT};
   XtVaSetValues(gbl_iw, XvicNcolorVisualType, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

void CB_pseudoVisualType(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicUSE_DEFAULT, XvicUSE_8BIT, XvicUSE_24BIT};
   XtVaSetValues(gbl_iw, XvicNpseudoVisualType, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

/*--------------------------------------------------------------*/

void CB_cursorMode(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicFLOATING, XvicPLANTED};
   XtVaSetValues(gbl_iw, XvicNcursorMode, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

/*--------------------------------------------------------------*/

void CB_scrollBarDisplayPolicy(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicSTATIC, XvicAS_NEEDED, XvicNEVER};
   XtVaSetValues(gbl_iw, XvicNscrollBarDisplayPolicy, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

/*--------------------------------------------------------------*/

void CB_trackFloatingCursor(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {True, False};
   XtVaSetValues(gbl_iw, XvicNtrackFloatingCursor, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

/*--------------------------------------------------------------*/

void CB_enableDirectColor(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {True, False};
   XtVaSetValues(gbl_iw, XvicNenableDirectColor, values[(int)btn], NULL);
   reset_defaults(gbl_iw);
}

/*--------------------------------------------------------------*/

void CB_int(w, res, call_data)
Widget w;
XtPointer res;
XtPointer call_data;
{
   char *buf;
   int value, old_value;
   XtVaGetValues(w, XmNvalue, &buf, NULL);
   value = atoi(buf);
   XtVaGetValues(gbl_iw, (char *)res, &old_value, NULL);
   if (value != old_value) {
      XtVaSetValues(gbl_iw, (char *)res, value, NULL);
      reset_defaults(gbl_iw);
   }
   XtFree(buf);
}

void CB_int_sh(w, res, call_data)
Widget w;
XtPointer res;
XtPointer call_data;
{
   char *buf;
   int value, old_value;
   XtVaGetValues(w, XmNvalue, &buf, NULL);
   value = atoi(buf);
   XtVaGetValues(gbl_iw, (char *)res, &old_value, NULL);
   if (value != old_value) {
      XtVaSetValues(gbl_iw, (char *)res, value, NULL);
      reset_defaults(gbl_iw);
   }
   if (memory_control == XvicMEMORY_SHARED && shared_buf) {
      free(shared_buf);
      free(shared_buf2);
      free(shared_buf3);
      shared_buf = NULL;
   }
   XtFree(buf);
}

void CB_Dim(w, res, call_data)
Widget w;
XtPointer res;
XtPointer call_data;
{
   char *buf;
   Dimension value, old_value;
   XtVaGetValues(w, XmNvalue, &buf, NULL);
   value = atoi(buf);
   XtVaGetValues(gbl_iw, (char *)res, &old_value, NULL);
   if (value != old_value) {
      XtVaSetValues(gbl_iw, (char *)res, value, NULL);
      reset_defaults(gbl_iw);
   }
   XtFree(buf);
}

void CB_double(w, res, call_data)
Widget w;
XtPointer res;
XtPointer call_data;
{
   char *buf;
   double value, old_value;
   XtVaGetValues(w, XmNvalue, &buf, NULL);
   value = atof(buf);
   XtVaGetValues(gbl_iw, (char *)res, &old_value, NULL);
   if (value != old_value) {
      XtVaSetValues(gbl_iw, (char *)res, XvicDOUBLE_ARG(value), NULL);
      reset_defaults(gbl_iw);
   }
   XtFree(buf);
}

/*--------------------------------------------------------------*/

void CB_string(w, res, call_data)
Widget w;
XtPointer res;
XtPointer call_data;
{
   char *value;
   char *old_value;
   XtVaGetValues(w, XmNvalue, &value, NULL);
   XtVaGetValues(gbl_iw, (char *)res, &old_value, NULL);
   if (old_value == NULL || strcmp(value, old_value) != 0) {
      XtVaSetValues(gbl_iw, (char *)res, value, NULL);
      reset_defaults(gbl_iw);
   }
   XtFree(value);
}

/*--------------------------------------------------------------*/

void CB_data_min(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
   char *buf;
   XtVaGetValues(w, XmNvalue, &buf, NULL);
   data_min = atof(buf);
   XtFree(buf);
   XvicImageClear(gbl_iw);
}

void CB_data_max(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
   char *buf;
   XtVaGetValues(w, XmNvalue, &buf, NULL);
   data_max = atof(buf);
   XtFree(buf);
   XvicImageClear(gbl_iw);
}

void CB_data_inc(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
   char *buf;
   XtVaGetValues(w, XmNvalue, &buf, NULL);
   data_inc = atof(buf);
   XtFree(buf);
   XvicImageClear(gbl_iw);
}

/*--------------------------------------------------------------*/

void CB_mem_ctl(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static unsigned char values[] = {XvicMEMORY_APPLIC, XvicMEMORY_WIDGET, XvicMEMORY_SHARED};
   if (memory_control == XvicMEMORY_SHARED && shared_buf) {
      free(shared_buf);
      free(shared_buf2);
      free(shared_buf3);
      shared_buf = NULL;
   }
   memory_control = values[(int)btn];
}

/*--------------------------------------------------------------*/

void CB_stretchLUT(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   int lut[256];

   SetLut(lut, (int)btn);
   XvicImageSetMonoLUT(gbl_iw, lut);
}

void CB_redLUT(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   int lut[256];

   SetLut(lut, (int)btn);
   XvicImageSetColorLUT(gbl_iw, lut, NULL, NULL);
}

void CB_greenLUT(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   int lut[256];

   SetLut(lut, (int)btn);
   XvicImageSetColorLUT(gbl_iw, NULL, lut, NULL);
}

void CB_blueLUT(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   int lut[256];

   SetLut(lut, (int)btn);
   XvicImageSetColorLUT(gbl_iw, NULL, NULL, lut);
}

SetLut(lut, n)
int lut[256];
int n;
{
   int i;

   for (i=0; i<256; i++) {
      switch (n) {
         case 0:
            lut[i] = 255-i;
            break;
         case 1:
            lut[i] = (i/2+64) % 256;
            break;
         case 2:
            lut[i] = (i+128) % 256;
            break;
         case 3:
            lut[i] = (i+64) % 256;
            break;
         case 4:
            lut[i] = i;
            break;
         case 5:
            lut[i] = 0;
            break;
      }
   }
}

void CB_stretchLUT16(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   int lut[65536];

   SetLut16(lut, (int)btn);
   XvicImageSetMonoLUT16(gbl_iw, lut, 65536);
}

void CB_redLUT16(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   int lut[65536];

   SetLut16(lut, (int)btn);
   XvicImageSetColorLUT16(gbl_iw, lut, NULL, NULL, 65536);
}

void CB_greenLUT16(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   int lut[65536];

   SetLut16(lut, (int)btn);
   XvicImageSetColorLUT16(gbl_iw, NULL, lut, NULL, 65536);
}

void CB_blueLUT16(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   int lut[65536];

   SetLut16(lut, (int)btn);
   XvicImageSetColorLUT16(gbl_iw, NULL, NULL, lut, 65536);
}

SetLut16(lut, n)
int lut[65536];
int n;
{
   int i;

   for (i=0; i<65536; i++) {
      switch (n) {
         case 0:
            lut[i] = i;
            break;
         case 1:
            lut[i] = (i/2+16384) % 65536;
            break;
         case 2:
            lut[i] = (i%1024) * 64;
            break;
         case 3:
            lut[i] = (i+16384) % 65536;
            break;
         case 4:
            lut[i] = 65535-i;
            break;
         case 5:
            lut[i] = (i<4096)?i:0;
            break;
         case 6:
            lut[i] = 0;
            break;
      }
   }
}

/************************************************************************/
/* Graphics callbacks							*/
/************************************************************************/

void new_gc()
{
   char buf[20];

   if (gbl_gr.rubber_mode) {
      gbl_gr.rubber_gc = XvicImageCreateRubberGC(gbl_iw, gbl_gr.gc_mask,
				&gbl_gr.gc_values);
      if (gbl_gr.new_gc) {
         XvicImageChangeGC(gbl_iw, gbl_gr.new_gc,
			gbl_gr.gc_mask, &gbl_gr.gc_values);
         gbl_gr.gc = gbl_gr.new_gc;
      }
      else
         gbl_gr.gc = gbl_gr.rubber_gc;
   }
   else {
      if (gbl_gr.new_gc) {
         XvicImageChangeGC(gbl_iw, gbl_gr.new_gc,
			gbl_gr.gc_mask, &gbl_gr.gc_values);
         gbl_gr.gc = gbl_gr.new_gc;
      }
      else {
         gbl_gr.gc = XvicImageCreateGC(gbl_iw,
			gbl_gr.gc_mask, &gbl_gr.gc_values);
      }
      gbl_gr.rubber_gc = XvicImageCreateRubberGC(gbl_iw, gbl_gr.gc_mask,
				&gbl_gr.gc_values);
   }
   sprintf(buf, "%d", gbl_gr.gc);
   XtVaSetValues(gbl_gr.last_gc_widget, XmNvalue, buf, NULL);
}

void CBgr_color(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
   char *string;
   XColor xcolor;

   XtVaGetValues(w, XmNvalue, &string, NULL);
   XParseColor(XtDisplay(w), DefaultColormapOfScreen(XtScreen(w)),
		string, &xcolor);
   gbl_gr.color = XvicImageGetGrColor(gbl_iw, &xcolor);
   XtFree(string);
}

void CBgr_bgcolor(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
   char *string;
   XColor xcolor;

   XtVaGetValues(w, XmNvalue, &string, NULL);
   XParseColor(XtDisplay(w), DefaultColormapOfScreen(XtScreen(w)),
		string, &xcolor);
   gbl_gr.bgcolor = XvicImageGetGrColor(gbl_iw, &xcolor);
   XtFree(string);
}

void CBgr_text(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
   /* Does nothing.  Function simply needed so create_text() will make	*/
   /* it editable.							*/
}

void CBgr_textjustify(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static int values[] = {XvicJUST_LEFT, XvicJUST_CENTER, XvicJUST_RIGHT};
   gbl_gr.justify = values[(int)btn];
}

void CBgr_bitmap(w, client_data, call_data)
Widget w;
XtPointer client_data;
XtPointer call_data;
{
   char *string;
   XtVaGetValues(w, XmNvalue, &string, NULL);
   if (XReadBitmapFile(XtDisplay(w), RootWindowOfScreen(XtScreen(w)),
		string, &gbl_gr.bitmap_width, &gbl_gr.bitmap_height,
		&gbl_gr.bitmap, &gbl_gr.bitmap_hot_x, &gbl_gr.bitmap_hot_y)
	!= BitmapSuccess) {
      gbl_gr.bitmap = NULL;
   }
   else {
      if (gbl_gr.bitmap_hot_x == -1)
         gbl_gr.bitmap_hot_x = 0;
      if (gbl_gr.bitmap_hot_y == -1)
         gbl_gr.bitmap_hot_y = 0;
   }
   XtFree(string);
}

void CBgr_shape(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static Shape values[] = {MoveExisting, EraseExisting,
	FillArc, FillArcs, FillPolygon, FillRectangle, FillRectangles,
	Arc, Arcs, Bitmap, ImageString, Line, Lines, Point, Points,
	Rectangle, Rectangles, Segments, DrawString};

   if (values[(int)btn] == EraseExisting) {
      XvicImageEraseObject(gbl_iw, gbl_gr.id);
      set_option_default(gbl_gr.shape_widget, (int)gbl_gr.shape_int);
   }
   else {
      gbl_gr.shape = values[(int)btn];
      gbl_gr.shape_int = (int)btn;
   }
}

void CBgr_id(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   char *string;
   XtVaGetValues(w, XmNvalue, &string, NULL);
   gbl_gr.id = atoi(string);
   XtFree(string);
}

void CBgr_gc(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   char *string;
   XtVaGetValues(w, XmNvalue, &string, NULL);
   gbl_gr.new_gc = atoi(string);
   XtFree(string);
}

void CBgr_arcmode(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static int values[] = {ArcPieSlice, ArcChord};
   gbl_gr.gc_values.arc_mode = values[(int)btn];
   gbl_gr.gc_mask |= GCArcMode;
   new_gc();
}

void CBgr_capstyle(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static int values[] = {CapNotLast, CapButt, CapRound, CapProjecting};
   gbl_gr.gc_values.cap_style = values[(int)btn];
   gbl_gr.gc_mask |= GCCapStyle;
   new_gc();
}

void CBgr_dashes(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   char *string;
   XtVaGetValues(w, XmNvalue, &string, NULL);
   gbl_gr.gc_values.dashes = (char) atoi(string);
   gbl_gr.gc_mask |= GCDashList;
   new_gc();
   XtFree(string);
}

void CBgr_dashoffset(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   char *string;
   XtVaGetValues(w, XmNvalue, &string, NULL);
   gbl_gr.gc_values.dash_offset = atoi(string);
   gbl_gr.gc_mask |= GCDashOffset;
   new_gc();
   XtFree(string);
}

void CBgr_fillrule(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static int values[] = {EvenOddRule, WindingRule};
   gbl_gr.gc_values.fill_rule = values[(int)btn];
   gbl_gr.gc_mask |= GCFillRule;
   new_gc();
}

void CBgr_font(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   char *string;
   char **names;
   int count;
   XtVaGetValues(w, XmNvalue, &string, NULL);
   names = XListFonts(XtDisplay(w), string, 1, &count);
   XFreeFontNames(names);
   if (count > 0) {
      gbl_gr.gc_values.font = XLoadFont(XtDisplay(w),  string);
      gbl_gr.gc_mask |= GCFont;
      new_gc();
   }
   XtFree(string);
}

void CBgr_joinstyle(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static int values[] = {JoinMiter, JoinRound, JoinBevel};
   gbl_gr.gc_values.join_style = values[(int)btn];
   gbl_gr.gc_mask |= GCJoinStyle;
   new_gc();
}

void CBgr_linestyle(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static int values[] = {LineSolid, LineOnOffDash, LineDoubleDash};
   gbl_gr.gc_values.line_style = values[(int)btn];
   gbl_gr.gc_mask |= GCLineStyle;
   new_gc();
}

void CBgr_linewidth(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   char *string;
   XtVaGetValues(w, XmNvalue, &string, NULL);
   gbl_gr.gc_values.line_width = atoi(string);
   gbl_gr.gc_mask |= GCLineWidth;
   new_gc();
   XtFree(string);
}

void CBgr_rubbermode(w, btn, call_data)
Widget w;
XtPointer btn;
XtPointer call_data;
{
   static int values[] = {False, True};
   gbl_gr.rubber_mode = values[(int)btn];
   new_gc();
}

/************************************************************************/

main(argc,argv)
int argc;
char **argv;
{
   XtAppContext app_context;
   Widget topLevel, form, iw, rc, shell;
   Dimension width, height;
   Arg args[20];
   int n;
   unsigned char data_type;

   topLevel = XtVaAppInitialize(
	&app_context,
	"Test_IW",
	NULL, 0,
	&argc, argv,
	NULL, NULL);

   XSynchronize(XtDisplay(topLevel), True);	/*!!!!*/

   form = XtVaCreateManagedWidget("form", xmFormWidgetClass, topLevel, NULL);

   n = 0;
   XtSetArg(args[n], XmNtopAttachment, (XtArgVal)XmATTACH_FORM); n++;
   XtSetArg(args[n], XmNbottomAttachment, (XtArgVal)XmATTACH_FORM); n++;
   XtSetArg(args[n], XmNleftAttachment, (XtArgVal)XmATTACH_FORM); n++;
   XtSetArg(args[n], XmNrightAttachment, (XtArgVal)XmATTACH_FORM); n++;
   XtSetArg(args[n], XvicNimageWidth, (XtArgVal)1500); n++;
   XtSetArg(args[n], XvicNimageHeight, (XtArgVal)1500); n++;
   XtSetArg(args[n], XvicNtrackFloatingCursor, (XtArgVal)False); n++;

   iw = XvicCreateImage(form, "image", args, n);
   XtManageChild(iw);

   gbl_iw = iw;
   XtAddCallback(iw, XvicNresizeCallback, resize, NULL);
   XtAddCallback(iw, XvicNexposeCallback, expose, NULL);
   XtAddCallback(iw, XvicNpanCallback, pan, NULL);
   XtAddCallback(iw, XvicNinputCallback, input, NULL);
   XtAddCallback(iw, XvicNcursorCallback, cursor, NULL);
   XtAddCallback(iw, XvicNworkProcActiveCallback, work_proc_active, NULL);

   shell = XtVaCreatePopupShell("control", topLevelShellWidgetClass, topLevel,
	XmNwidth, 720, NULL);
   rc = XtVaCreateManagedWidget("rc", xmRowColumnWidgetClass, shell,
	XmNwidth, 720,
	XmNorientation, XmVERTICAL,
	XmNpacking, XmPACK_TIGHT,
	XmNadjustLast, False,
	XmNleftAttachment, XmATTACH_FORM,
	XmNtopAttachment, XmATTACH_FORM,
	XmNbottomAttachment, XmATTACH_FORM,
	NULL);

   create_menus(rc, iw);
   XtPopup(shell, XtGrabNone);

   shell = XtVaCreatePopupShell("graphics", topLevelShellWidgetClass, topLevel,
	XmNwidth, 225, NULL);
   rc = XtVaCreateManagedWidget("rc", xmRowColumnWidgetClass, shell,
	XmNwidth, 225,
	XmNorientation, XmVERTICAL,
	XmNpacking, XmPACK_TIGHT,
	NULL);

   create_graphics_menus(rc, iw);
   XtPopup(shell, XtGrabNone);

   reset_defaults(iw);

   XtVaGetValues(iw, XvicNdataType, &data_type, NULL);
   set_data_range(data_type);

   XtRealizeWidget(topLevel);

   XtVaGetValues(iw, XvicNviewWidth, &width, XvicNviewHeight, &height, NULL);
   DPR(("view width=%d, height=%d\n", width, height));
   XtVaGetValues(iw, XmNwidth, &width, XmNheight, &height, NULL);
   DPR(("core width=%d, height=%d\n", width, height));

   XtAppMainLoop(app_context);
}


/****************************************************************/

Widget create_text(parent, label, cb, data)
Widget parent;
char *label;
XtCallbackProc cb;
XtPointer data;
{
   XmString str;
   Widget rc, lbl, txt;

   str = XmStringCreateSimple(label);
   rc = XtVaCreateWidget("rc", xmRowColumnWidgetClass, parent,
	XmNorientation, XmHORIZONTAL,
	NULL);
   lbl = XtVaCreateManagedWidget("label", xmLabelWidgetClass, rc,
	XmNlabelString, str,
	NULL);
   txt = XtVaCreateManagedWidget("text", xmTextFieldWidgetClass, rc,
	XmNcolumns, 6, XmNeditable, True, XmNmaxLength, 7,
	NULL);

   if (cb == CB_string || cb == NULL || cb == CBgr_color || cb == CBgr_bgcolor
		|| cb == CBgr_font || cb == CBgr_text || cb == CBgr_bitmap)
      XtVaSetValues(txt, XmNcolumns, 20, XmNmaxLength, 150, NULL);
   if (cb == CB_data_min || cb == CB_data_max || cb == CB_data_inc ||
		cb == CB_double)
      XtVaSetValues(txt, XmNcolumns, 12, XmNmaxLength, 20, NULL);

   if (cb == NULL)
      XtVaSetValues(txt, XmNeditable, False, NULL);
   else
      XtAddCallback(txt, XmNactivateCallback, cb, data);

   XtManageChild(rc);
   XmStringFree(str);
   return txt;
}

/****************************************************************/

Widget create_bool(parent, label, cb)
Widget parent;
char *label;
XtCallbackProc cb;
{
   XmString s1, s2, s3;
   Widget w;

   s1 = XmStringCreateSimple(label);
   s2 = XmStringCreateSimple("True");
   s3 = XmStringCreateSimple("False");
   w = XmVaCreateSimpleOptionMenu(parent, "bool", s1, 0, 0, cb,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XtManageChild(w);

   return w;
}

/****************************************************************/

create_menus(rc, iw)
Widget rc, iw;
{
   XmString s1, s2, s3, s4, s5, s6, s7, s8, s9;
   Widget w;

/*--------------------------------------------------------------*/
/* BasicImage resources						*/
/*--------------------------------------------------------------*/

   s1 = XmStringCreateSimple("Image Mode");
   s2 = XmStringCreateSimple("Color");
   s3 = XmStringCreateSimple("BW");
   w_imageMode = XmVaCreateSimpleOptionMenu(rc, "imageMode", s1, 0, 0, CB_imageMode,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XtManageChild(w_imageMode);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Data Type");
   s2 = XmStringCreateSimple("Byte");
   s3 = XmStringCreateSimple("Half");
   s4 = XmStringCreateSimple("Unsigned Half");
   s5 = XmStringCreateSimple("Full");
   s6 = XmStringCreateSimple("Unsigned Full");
   s7 = XmStringCreateSimple("Real");
   s8 = XmStringCreateSimple("Double");
   w_dataType = XmVaCreateSimpleOptionMenu(rc, "dataType", s1, 0, 0, CB_dataType,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		XmVaPUSHBUTTON, s7, 0, 0, 0,
		XmVaPUSHBUTTON, s8, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XmStringFree(s6);
   XmStringFree(s7);
   XmStringFree(s8);
   XtManageChild(w_dataType);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Dither Mode");
   s2 = XmStringCreateSimple("None");
   s3 = XmStringCreateSimple("Ordered");
   s4 = XmStringCreateSimple("Kagels");
   w_ditherMode = XmVaCreateSimpleOptionMenu(rc, "ditherMode", s1, 0, 0, CB_ditherMode,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(w_ditherMode);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Lut Type");
   s2 = XmStringCreateSimple("Stretch");
   s3 = XmStringCreateSimple("Raw");
   s4 = XmStringCreateSimple("Pseudo");
   s5 = XmStringCreateSimple("Pseudo Only");
   w_lutType = XmVaCreateSimpleOptionMenu(rc, "lutType", s1, 0, 0, CB_lutType,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XtManageChild(w_lutType);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Lut16 Type");
   s2 = XmStringCreateSimple("Stretch");
   s3 = XmStringCreateSimple("Raw");
   s4 = XmStringCreateSimple("Pseudo");
   s5 = XmStringCreateSimple("Pseudo Only");
   w_lut16Type = XmVaCreateSimpleOptionMenu(rc, "lut16Type", s1, 0, 0, CB_lut16Type,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XtManageChild(w_lut16Type);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Stretch Policy");
   s2 = XmStringCreateSimple("Use HW");
   s3 = XmStringCreateSimple("Use SW");
   w_stretchPolicy = XmVaCreateSimpleOptionMenu(rc, "stretchPolicy", s1, 0, 0, CB_stretchPolicy,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XtManageChild(w_stretchPolicy);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Colormap Policy");
   s2 = XmStringCreateSimple("Full");
   s3 = XmStringCreateSimple("Half");
   s4 = XmStringCreateSimple("Dither");
   s5 = XmStringCreateSimple("Alloc");
   s6 = XmStringCreateSimple("Full Color");
   w_colormapPolicy = XmVaCreateSimpleOptionMenu(rc, "colormapPolicy", s1, 0, 0, CB_colormapPolicy,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XmStringFree(s6);
   XtManageChild(w_colormapPolicy);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Visual Type");
   s2 = XmStringCreateSimple("Use Default");
   s3 = XmStringCreateSimple("Use 8 Bit");
   s4 = XmStringCreateSimple("Use 24 Bit");
   w_visualType = XmVaCreateSimpleOptionMenu(rc, "visualType", s1, 0, 0, CB_visualType,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(w_visualType);

/*--------------------------------------------------------------*/
   w_enableDirectColor = create_bool(rc, "Enable Direct Color", CB_enableDirectColor);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Work Proc Policy");
   s2 = XmStringCreateSimple("None");
   s3 = XmStringCreateSimple("Read");
   s4 = XmStringCreateSimple("All");
   w_workProcPolicy = XmVaCreateSimpleOptionMenu(rc, "workProcPolicy", s1, 0, 0, CB_workProcPolicy,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(w_workProcPolicy);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Data Save Policy");
   s2 = XmStringCreateSimple("None");
   s3 = XmStringCreateSimple("Raw");
   s4 = XmStringCreateSimple("Ximage");
   s5 = XmStringCreateSimple("Pixmap");
   w_dataSavePolicy = XmVaCreateSimpleOptionMenu(rc, "dataSavePolicy", s1, 0, 0, CB_dataSavePolicy,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XtManageChild(w_dataSavePolicy);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Constrain Pan");
   s2 = XmStringCreateSimple("None");
   s3 = XmStringCreateSimple("X Only");
   s4 = XmStringCreateSimple("Y Only");
   s5 = XmStringCreateSimple("Both");
   w_constrainPan = XmVaCreateSimpleOptionMenu(rc, "constrainPan", s1, 0, 0, CB_constrainPan,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XtManageChild(w_constrainPan);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("BW Dither Mode");
   s2 = XmStringCreateSimple("None");
   s3 = XmStringCreateSimple("Ordered");
   s4 = XmStringCreateSimple("Kagels");
   w_bwDither = XmVaCreateSimpleOptionMenu(rc, "bwDither", s1, 0, 0, CB_bwDither,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(w_bwDither);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("BW Stretch Policy");
   s2 = XmStringCreateSimple("Use HW");
   s3 = XmStringCreateSimple("Use SW");
   w_bwStretchPolicy = XmVaCreateSimpleOptionMenu(rc, "bwStretchPolicy", s1, 0, 0, CB_bwStretchPolicy,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XtManageChild(w_bwStretchPolicy);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("BW Colormap Policy");
   s2 = XmStringCreateSimple("Full");
   s3 = XmStringCreateSimple("Half");
   s4 = XmStringCreateSimple("Dither");
   s5 = XmStringCreateSimple("Alloc");
   s6 = XmStringCreateSimple("Full Color");
   w_bwColormapPolicy = XmVaCreateSimpleOptionMenu(rc, "bwColormapPolicy", s1, 0, 0, CB_bwColormapPolicy,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XmStringFree(s6);
   XtManageChild(w_bwColormapPolicy);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("BW Visual Type");
   s2 = XmStringCreateSimple("Use Default");
   s3 = XmStringCreateSimple("Use 8 Bit");
   s4 = XmStringCreateSimple("Use 24 Bit");
   w_bwVisualType = XmVaCreateSimpleOptionMenu(rc, "bwVisualType", s1, 0, 0, CB_bwVisualType,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(w_bwVisualType);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Color Dither Mode");
   s2 = XmStringCreateSimple("None");
   s3 = XmStringCreateSimple("Ordered");
   s4 = XmStringCreateSimple("Kagels");
   w_colorDither = XmVaCreateSimpleOptionMenu(rc, "colorDither", s1, 0, 0, CB_colorDither,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(w_colorDither);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Color Stretch Policy");
   s2 = XmStringCreateSimple("Use HW");
   s3 = XmStringCreateSimple("Use SW");
   w_colorStretchPolicy = XmVaCreateSimpleOptionMenu(rc, "colorStretchPolicy", s1, 0, 0, CB_colorStretchPolicy,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XtManageChild(w_colorStretchPolicy);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Color Colormap Policy");
   s2 = XmStringCreateSimple("Full");
   s3 = XmStringCreateSimple("Half");
   s4 = XmStringCreateSimple("Dither");
   s5 = XmStringCreateSimple("Alloc");
   s6 = XmStringCreateSimple("Full Color");
   w_colorColormapPolicy = XmVaCreateSimpleOptionMenu(rc, "colorColormapPolicy", s1, 0, 0, CB_colorColormapPolicy,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XmStringFree(s6);
   XtManageChild(w_colorColormapPolicy);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Color Visual Type");
   s2 = XmStringCreateSimple("Use Default");
   s3 = XmStringCreateSimple("Use 8 Bit");
   s4 = XmStringCreateSimple("Use 24 Bit");
   w_colorVisualType = XmVaCreateSimpleOptionMenu(rc, "colorVisualType", s1, 0, 0, CB_colorVisualType,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(w_colorVisualType);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Pseudo Dither Mode");
   s2 = XmStringCreateSimple("None");
   s3 = XmStringCreateSimple("Ordered");
   s4 = XmStringCreateSimple("Kagels");
   w_pseudoDither = XmVaCreateSimpleOptionMenu(rc, "pseudoDither", s1, 0, 0, CB_pseudoDither,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(w_pseudoDither);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Pseudo Stretch Policy");
   s2 = XmStringCreateSimple("Use HW");
   s3 = XmStringCreateSimple("Use SW");
   w_pseudoStretchPolicy = XmVaCreateSimpleOptionMenu(rc, "pseudoStretchPolicy", s1, 0, 0, CB_pseudoStretchPolicy,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XtManageChild(w_pseudoStretchPolicy);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Pseudo Colormap Policy");
   s2 = XmStringCreateSimple("Full");
   s3 = XmStringCreateSimple("Half");
   s4 = XmStringCreateSimple("Dither");
   s5 = XmStringCreateSimple("Alloc");
   s6 = XmStringCreateSimple("Full Color");
   w_pseudoColormapPolicy = XmVaCreateSimpleOptionMenu(rc, "pseudoColormapPolicy", s1, 0, 0, CB_pseudoColormapPolicy,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XmStringFree(s6);
   XtManageChild(w_pseudoColormapPolicy);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Pseudo Visual Type");
   s2 = XmStringCreateSimple("Use Default");
   s3 = XmStringCreateSimple("Use 8 Bit");
   s4 = XmStringCreateSimple("Use 24 Bit");
   w_pseudoVisualType = XmVaCreateSimpleOptionMenu(rc, "pseudoVisualType", s1, 0, 0, CB_pseudoVisualType,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(w_pseudoVisualType);

/*--------------------------------------------------------------*/
   w_xPan = create_text(rc, "X Pan", CB_int, XvicNxPan);
   w_yPan = create_text(rc, "Y Pan", CB_int, XvicNyPan);
   w_xSubpixelPan = create_text(rc, "X Subpixel Pan", CB_int, XvicNxSubpixelPan);
   w_ySubpixelPan = create_text(rc, "Y Subpixel Pan", CB_int, XvicNySubpixelPan);
   w_xZoomIn = create_text(rc, "X Zoom In", CB_int, XvicNxZoomIn);
   w_xZoomOut = create_text(rc, "X Zoom Out", CB_int, XvicNxZoomOut);
   w_yZoomIn = create_text(rc, "Y Zoom In", CB_int, XvicNyZoomIn);
   w_yZoomOut = create_text(rc, "Y Zoom Out", CB_int, XvicNyZoomOut);
   w_maximumMemory = create_text(rc, "Maximum Memory", CB_int, XvicNmaximumMemory);
   w_grayLevels = create_text(rc, "Gray Levels", CB_int, XvicNgrayLevels);
   w_redLevels = create_text(rc, "Red Levels", CB_int, XvicNredLevels);
   w_greenLevels = create_text(rc, "Green Levels", CB_int, XvicNgreenLevels);
   w_blueLevels = create_text(rc, "Blue Levels", CB_int, XvicNblueLevels);
   w_rawDataMin = create_text(rc, "Raw Data Min", CB_double, XvicNrawDataMin);
   w_rawDataMax = create_text(rc, "Raw Data Max", CB_double, XvicNrawDataMax);
   w_scaledDataMax = create_text(rc, "Scaled Data Max", CB_int, XvicNscaledDataMax);
   w_outputDataMax = create_text(rc, "Output Data Max", CB_int, XvicNoutputDataMax);
   w_imageWidth = create_text(rc, "Image Width", CB_int_sh, XvicNimageWidth);
   w_imageHeight = create_text(rc, "Image Height", CB_int_sh, XvicNimageHeight);
   w_tileWidth = create_text(rc, "Tile Width", CB_int, XvicNtileWidth);
   w_tileHeight = create_text(rc, "Tile Height", CB_int, XvicNtileHeight);
   w_viewWidth = create_text(rc, "View Width", CB_Dim, XvicNviewWidth);
   w_viewHeight = create_text(rc, "View Height", CB_Dim, XvicNviewHeight);
   w_width = create_text(rc, "Core Width", CB_Dim, XmNwidth);
   w_height = create_text(rc, "Core Height", CB_Dim, XmNheight);
   w_xPreSubpixelPan = create_text(rc,"X Pre Subpixel Pan",CB_int,XvicNxPreSubpixelPan);
   w_yPreSubpixelPan = create_text(rc,"Y Pre Subpixel Pan",CB_int,XvicNyPreSubpixelPan);
   w_xPreZoomIn = create_text(rc, "X Pre Zoom In", CB_int_sh, XvicNxPreZoomIn);
   w_xPreZoomOut = create_text(rc, "X Pre Zoom Out", CB_int_sh, XvicNxPreZoomOut);
   w_yPreZoomIn = create_text(rc, "Y Pre Zoom In", CB_int_sh, XvicNyPreZoomIn);
   w_yPreZoomOut = create_text(rc, "Y Pre Zoom Out", CB_int_sh, XvicNyPreZoomOut);

/*--------------------------------------------------------------*/
/* Image resources						*/
/*--------------------------------------------------------------*/

   XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass, rc,
	XmNwidth, 100, NULL);

   s1 = XmStringCreateSimple("Cursor Mode");
   s2 = XmStringCreateSimple("Floating");
   s3 = XmStringCreateSimple("Planted");
   w_cursorMode = XmVaCreateSimpleOptionMenu(rc, "cursorMode", s1, 0, 0, CB_cursorMode,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XtManageChild(w_cursorMode);

/*--------------------------------------------------------------*/

   s1 = XmStringCreateSimple("Scrollbar Disp Pol");
   s2 = XmStringCreateSimple("Static");
   s3 = XmStringCreateSimple("As Needed");
   s4 = XmStringCreateSimple("Never");
   w_scrollBarDisplayPolicy = XmVaCreateSimpleOptionMenu(rc, "scrollBarDisplayPolicy", s1, 0, 0
, CB_scrollBarDisplayPolicy,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(w_scrollBarDisplayPolicy);

   w_cursorX = create_text(rc, "Cursor X", CB_int, XvicNcursorX);
   w_cursorY = create_text(rc, "Cursor Y", CB_int, XvicNcursorY);
   w_cursorXfp = create_text(rc, "Cursor X fp", CB_double, XvicNcursorXfp);
   w_cursorYfp = create_text(rc, "Cursor Y fp", CB_double, XvicNcursorYfp);
   w_cursorForeground = create_text(rc, "Cursor Foreground", CB_string, XvicNcursorForeground);
   w_cursorBackground = create_text(rc, "Cursor Background", CB_string, XvicNcursorBackground);
   w_cursor = create_text(rc, "Cursor Shape", CB_string, XvicNcursor);
   w_trackFloatingCursor = create_bool(rc, "Track Floating Cursor", CB_trackFloatingCursor);

/*--------------------------------------------------------------*/

   w_input = create_text(rc, "Input Arg", NULL, NULL);

/*--------------------------------------------------------------*/
/* Test program controls					*/
/*--------------------------------------------------------------*/

   XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass, rc,
	XmNwidth, 100, NULL);

   w_dataMin = create_text(rc, "Data Minimum", CB_data_min, NULL);
   w_dataMax = create_text(rc, "Data Maximum", CB_data_max, NULL);
   w_dataInc = create_text(rc, "Data Increment", CB_data_inc, NULL);

   s1 = XmStringCreateSimple("Memory Control");
   s2 = XmStringCreateSimple("Applic");
   s3 = XmStringCreateSimple("Widget");
   s4 = XmStringCreateSimple("Shared");
   w = XmVaCreateSimpleOptionMenu(rc, "memory_control", s1, 0, 0, CB_mem_ctl,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   memory_control = XvicMEMORY_APPLIC;
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(w);

/*--------------------------------------------------------------*/

   s1 = XmStringCreateSimple("BW Stretch");
   s2 = XmStringCreateSimple("Inverse 255-0");
   s3 = XmStringCreateSimple("Linear 64-196");
   s4 = XmStringCreateSimple("Wrap 128-127");
   s5 = XmStringCreateSimple("Wrap 64-63");
   s6 = XmStringCreateSimple("Linear 0-255");
   s7 = XmStringCreateSimple("Black");
   w = XmVaCreateSimpleOptionMenu(rc, "bw_stretch", s1, 0, 4, CB_stretchLUT,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		XmVaPUSHBUTTON, s7, 0, 0, 0,
		0);
   XtManageChild(w);
   CB_stretchLUT(w, 4, NULL);
   XmStringFree(s1);

   s1 = XmStringCreateSimple("Red Stretch");
   w = XmVaCreateSimpleOptionMenu(rc, "red_stretch", s1, 0, 4, CB_redLUT,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		XmVaPUSHBUTTON, s7, 0, 0, 0,
		0);
   XtManageChild(w);
   CB_redLUT(w, 4, NULL);
   XmStringFree(s1);

   s1 = XmStringCreateSimple("Green Stretch");
   w = XmVaCreateSimpleOptionMenu(rc, "green_stretch", s1, 0, 4, CB_greenLUT,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		XmVaPUSHBUTTON, s7, 0, 0, 0,
		0);
   XtManageChild(w);
   CB_greenLUT(w, 4, NULL);
   XmStringFree(s1);

   s1 = XmStringCreateSimple("Blue Stretch");
   w = XmVaCreateSimpleOptionMenu(rc, "blue_stretch", s1, 0, 4, CB_blueLUT,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		XmVaPUSHBUTTON, s7, 0, 0, 0,
		0);
   XtManageChild(w);
   CB_blueLUT(w, 4, NULL);
   XmStringFree(s1);

   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XmStringFree(s6);
   XmStringFree(s7);

/*--------------------------------------------------------------*/

   s1 = XmStringCreateSimple("BW Stretch 16");
   s2 = XmStringCreateSimple("Linear 0-64K");
   s3 = XmStringCreateSimple("Linear 16K-48K");
   s4 = XmStringCreateSimple("Ramp 0-1024 rpt");
   s5 = XmStringCreateSimple("Wrap 16K");
   s6 = XmStringCreateSimple("Inverse 64K-0");
   s7 = XmStringCreateSimple("12-bit 0-4K");
   s8 = XmStringCreateSimple("Black");
   w = XmVaCreateSimpleOptionMenu(rc, "bw_stretch16", s1, 0, 0, CB_stretchLUT16,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		XmVaPUSHBUTTON, s7, 0, 0, 0,
		XmVaPUSHBUTTON, s8, 0, 0, 0,
		0);
   XtManageChild(w);
   XmStringFree(s1);

   s1 = XmStringCreateSimple("Red Stretch 16");
   w = XmVaCreateSimpleOptionMenu(rc, "red_stretch16", s1, 0, 0, CB_redLUT16,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		XmVaPUSHBUTTON, s7, 0, 0, 0,
		XmVaPUSHBUTTON, s8, 0, 0, 0,
		0);
   XtManageChild(w);
   XmStringFree(s1);

   s1 = XmStringCreateSimple("Green Stretch 16");
   w = XmVaCreateSimpleOptionMenu(rc, "green_stretch16", s1, 0, 0,CB_greenLUT16,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		XmVaPUSHBUTTON, s7, 0, 0, 0,
		XmVaPUSHBUTTON, s8, 0, 0, 0,
		0);
   XtManageChild(w);
   XmStringFree(s1);

   s1 = XmStringCreateSimple("Blue Stretch 16");
   w = XmVaCreateSimpleOptionMenu(rc, "blue_stretch16", s1, 0, 0, CB_blueLUT16,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		XmVaPUSHBUTTON, s7, 0, 0, 0,
		XmVaPUSHBUTTON, s8, 0, 0, 0,
		0);
   XtManageChild(w);
   XmStringFree(s1);

   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XmStringFree(s6);
   XmStringFree(s7);
   XmStringFree(s7);

/*!!!! VVVV !!!!*/
   w=XtVaCreateManagedWidget("Destroy Widget",xmPushButtonWidgetClass,rc,NULL);
   XtAddCallback(w, XmNactivateCallback, DestroyCallback, (XtPointer)NULL);
/*!!!! ^^^^ !!!!*/

   w=XtVaCreateManagedWidget("Update Values",xmPushButtonWidgetClass,rc,NULL);
   XtAddCallback(w, XmNactivateCallback, UpdateCallback, (XtPointer)NULL);

}

/****************************************************************/

create_graphics_menus(rc, iw)
Widget rc, iw;
{
   XmString s1, s2, s3, s4, s5, s6, s7, s8, s9;
   XmString s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20;
   Widget w;

/* Init items in the global gr structure */

   gbl_gr.gc = 0;
   gbl_gr.rubber_gc = 0;
   gbl_gr.id = 0;
   gbl_gr.rubber_id = 0;
   gbl_gr.shape = Line;
   gbl_gr.shape_int = 11;
   gbl_gr.gc_mask = 0;
   gbl_gr.bitmap = NULL;

/*--------------------------------------------------------------*/
/* Colors for graphics						*/
/*--------------------------------------------------------------*/

   gw_color = create_text(rc, "Color", CBgr_color, NULL);
   XtVaSetValues(gw_color, XmNvalue, "white", NULL);
   CBgr_color(gw_color, NULL, NULL);

   gw_bgColor = create_text(rc, "BG Color", CBgr_bgcolor, NULL);
   XtVaSetValues(gw_bgColor, XmNvalue, "black", NULL);
   CBgr_bgcolor(gw_bgColor, NULL, NULL);

   XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass, rc,
	XmNwidth, 100, NULL);

/*--------------------------------------------------------------*/
/* String for text						*/
/*--------------------------------------------------------------*/

   gw_text = create_text(rc, "Text", CBgr_text, NULL);
   gbl_gr.text_widget = gw_text;

   s1 = XmStringCreateSimple("Text Justify");
   s2 = XmStringCreateSimple("Left");
   s3 = XmStringCreateSimple("Center");
   s4 = XmStringCreateSimple("Right");
   gw_textJustify = XmVaCreateSimpleOptionMenu(rc, "textJustify", s1, 0, 0, CBgr_textjustify,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(gw_textJustify);

   XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass, rc,
	XmNwidth, 100, NULL);

/*--------------------------------------------------------------*/
/* Bitmap file							*/
/*--------------------------------------------------------------*/

   gw_bitmap = create_text(rc, "Bitmap File", CBgr_bitmap, NULL);

/*--------------------------------------------------------------*/
/* Erase overlay						*/
/*--------------------------------------------------------------*/

   gw_erase=XtVaCreateManagedWidget("Erase Overlay",xmPushButtonWidgetClass,rc,NULL);
   XtAddCallback(gw_erase, XmNactivateCallback, EraseOverlayCallback, (XtPointer)NULL);

   XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass, rc,
	XmNwidth, 100, NULL);

/*--------------------------------------------------------------*/
/* Shape for graphics						*/
/*--------------------------------------------------------------*/

   s1 = XmStringCreateSimple("Shape");
   s2 = XmStringCreateSimple("Move Existing");
   s3 = XmStringCreateSimple("Erase Existing");
   s4 = XmStringCreateSimple("Filled Arc");
   s5 = XmStringCreateSimple("Filled Arcs");
   s6 = XmStringCreateSimple("Filled Polygon");
   s7 = XmStringCreateSimple("Filled Rectangle");
   s8 = XmStringCreateSimple("Filled Rectangles");
   s9 = XmStringCreateSimple("Arc");
   s10 = XmStringCreateSimple("Arcs");
   s11 = XmStringCreateSimple("Bitmap");
   s12 = XmStringCreateSimple("Image String");
   s13 = XmStringCreateSimple("Line");
   s14 = XmStringCreateSimple("Lines");
   s15 = XmStringCreateSimple("Point");
   s16 = XmStringCreateSimple("Points");
   s17 = XmStringCreateSimple("Rectangle");
   s18 = XmStringCreateSimple("Rectangles");
   s19 = XmStringCreateSimple("Segments");
   s20 = XmStringCreateSimple("String");
   gw_shape = XmVaCreateSimpleOptionMenu(rc, "grShape", s1, 0, 11, CBgr_shape,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		XmVaPUSHBUTTON, s6, 0, 0, 0,
		XmVaPUSHBUTTON, s7, 0, 0, 0,
		XmVaPUSHBUTTON, s8, 0, 0, 0,
		XmVaPUSHBUTTON, s9, 0, 0, 0,
		XmVaPUSHBUTTON, s10, 0, 0, 0,
		XmVaPUSHBUTTON, s11, 0, 0, 0,
		XmVaPUSHBUTTON, s12, 0, 0, 0,
		XmVaPUSHBUTTON, s13, 0, 0, 0,
		XmVaPUSHBUTTON, s14, 0, 0, 0,
		XmVaPUSHBUTTON, s15, 0, 0, 0,
		XmVaPUSHBUTTON, s16, 0, 0, 0,
		XmVaPUSHBUTTON, s17, 0, 0, 0,
		XmVaPUSHBUTTON, s18, 0, 0, 0,
		XmVaPUSHBUTTON, s19, 0, 0, 0,
		XmVaPUSHBUTTON, s20, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XmStringFree(s6);
   XmStringFree(s7);
   XmStringFree(s8);
   XmStringFree(s9);
   XmStringFree(s10);
   XmStringFree(s11);
   XmStringFree(s12);
   XmStringFree(s13);
   XmStringFree(s14);
   XmStringFree(s15);
   XmStringFree(s16);
   XmStringFree(s17);
   XmStringFree(s18);
   XmStringFree(s19);
   XmStringFree(s20);
   XtManageChild(gw_shape);
   gbl_gr.shape_widget = gw_shape;

   gw_id = create_text(rc, "Object ID", CBgr_id, NULL);
   gbl_gr.id_widget = gw_id;
   XtVaSetValues(gw_id, XmNvalue, "0", NULL);
   gw_lastId = create_text(rc, "Last ID", NULL, NULL);
   gbl_gr.last_id_widget = gw_lastId;

   gw_gc = create_text(rc, "GC ID", CBgr_gc, NULL);
   gbl_gr.gc_widget = gw_gc;
   XtVaSetValues(gw_gc, XmNvalue, "0", NULL);
   gw_lastGc = create_text(rc, "Last GC", NULL, NULL);
   gbl_gr.last_gc_widget = gw_lastGc;

   XtVaCreateManagedWidget("sep", xmSeparatorWidgetClass, rc,
	XmNwidth, 100, NULL);

/*--------------------------------------------------------------*/
/* GC stuff							*/
/*--------------------------------------------------------------*/

   s1 = XmStringCreateSimple("Arc Mode");
   s2 = XmStringCreateSimple("ArcPieSlice");
   s3 = XmStringCreateSimple("ArcChord");
   gw_arcMode = XmVaCreateSimpleOptionMenu(rc, "arcMode", s1, 0, 0, CBgr_arcmode,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XtManageChild(gw_arcMode);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Cap Style");
   s2 = XmStringCreateSimple("CapNotLast");
   s3 = XmStringCreateSimple("CapButt");
   s4 = XmStringCreateSimple("CapRound");
   s5 = XmStringCreateSimple("CapProjecting");
   gw_capStyle = XmVaCreateSimpleOptionMenu(rc, "capStyle", s1, 0, 1, CBgr_capstyle,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		XmVaPUSHBUTTON, s5, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XmStringFree(s5);
   XtManageChild(gw_capStyle);

/*--------------------------------------------------------------*/
   gw_dashes = create_text(rc, "Dashes", CBgr_dashes, NULL);
   gw_dashOffset = create_text(rc, "Dash Offset", CBgr_dashoffset, NULL);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Fill Rule");
   s2 = XmStringCreateSimple("EvenOddRule");
   s3 = XmStringCreateSimple("WindingRule");
   gw_fillRule = XmVaCreateSimpleOptionMenu(rc, "fillRule", s1, 0, 0, CBgr_fillrule,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XtManageChild(gw_fillRule);

/*--------------------------------------------------------------*/
   gw_font = create_text(rc, "Font", CBgr_font, NULL);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Join Style");
   s2 = XmStringCreateSimple("JoinMiter");
   s3 = XmStringCreateSimple("JoinRound");
   s4 = XmStringCreateSimple("JoinBevel");
   gw_joinStyle = XmVaCreateSimpleOptionMenu(rc, "joinStyle", s1, 0, 0, CBgr_joinstyle,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(gw_joinStyle);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Line Style");
   s2 = XmStringCreateSimple("LineSolid");
   s3 = XmStringCreateSimple("LineOnOffDash");
   s4 = XmStringCreateSimple("LineDoubleDash");
   gw_lineStyle = XmVaCreateSimpleOptionMenu(rc, "lineStyle", s1, 0, 0, CBgr_linestyle,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		XmVaPUSHBUTTON, s4, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XmStringFree(s4);
   XtManageChild(gw_lineStyle);

/*--------------------------------------------------------------*/
   gw_lineWidth = create_text(rc, "Line Width", CBgr_linewidth, NULL);
   XtVaSetValues(gw_lineWidth, XmNvalue, "1", NULL);
   CBgr_linewidth(gw_lineWidth, NULL, NULL);

/*--------------------------------------------------------------*/
   s1 = XmStringCreateSimple("Draw Mode");
   s2 = XmStringCreateSimple("Normal");
   s3 = XmStringCreateSimple("RubberBand");
   gw_rubberMode = XmVaCreateSimpleOptionMenu(rc, "rubberMode", s1, 0, 0, CBgr_rubbermode,
		XmVaPUSHBUTTON, s2, 0, 0, 0,
		XmVaPUSHBUTTON, s3, 0, 0, 0,
		0);
   XmStringFree(s1);
   XmStringFree(s2);
   XmStringFree(s3);
   XtManageChild(gw_rubberMode);

}

/****************************************************************/

set_int_default(w, res)
Widget w;
char *res;
{
   char buf[20];
   char *old_buf;
   int value;

   XtVaGetValues(gbl_iw, res, &value, NULL);
   sprintf(buf, "%0d", value);
   XtVaGetValues(w, XmNvalue, &old_buf, NULL);
   if (strcmp(buf, old_buf) != 0)
      XtVaSetValues(w, XmNvalue, buf, NULL);
   XtFree(old_buf);
}

/****************************************************************/

set_dim_default(w, res)
Widget w;
char *res;
{
   char buf[20];
   char *old_buf;
   Dimension value;

   XtVaGetValues(gbl_iw, res, &value, NULL);
   sprintf(buf, "%0d", value);
   XtVaGetValues(w, XmNvalue, &old_buf, NULL);
   if (strcmp(buf, old_buf) != 0)
      XtVaSetValues(w, XmNvalue, buf, NULL);
   XtFree(old_buf);
}

/****************************************************************/

set_option_default(w, n)
Widget w;
int n;
{
   WidgetList buttons;
   Cardinal num_buttons;
   int count, i;
   Widget menu;

   XtVaGetValues(XmOptionButtonGadget(w), XmNsubMenuId, &menu, NULL);
   XtVaGetValues(menu, XmNchildren, &buttons, XmNnumChildren, &num_buttons, NULL);
   count = 0;
   for (i=0; i<num_buttons; i++) {
      if (XmIsPushButtonGadget(buttons[i]) || XmIsPushButton(buttons[i])) {
         if (count == n)
            break;
         count++;
      }
   }
   if (i < num_buttons)
      XtVaSetValues(w, XmNmenuHistory, buttons[i], NULL);
}

/****************************************************************/

set_bool_default(w, res)
Widget w;
char *res;
{
   Boolean bvalue;
   int def;

   XtVaGetValues(gbl_iw, res, &bvalue, NULL);
   if (bvalue)
      def = 0;
   else
      def = 1;
   set_option_default(w, def);
}

/****************************************************************/

set_string_default(w, res)
Widget w;
char *res;
{
   char *buf;
   char *old_buf;

   XtVaGetValues(gbl_iw, res, &buf, NULL);
   XtVaGetValues(w, XmNvalue, &old_buf, NULL);
   if (buf == NULL)
      if (strlen(old_buf) != 0)
         XtVaSetValues(w, XmNvalue, "", NULL);
   else if (strcmp(buf, old_buf) != 0)
      XtVaSetValues(w, XmNvalue, buf, NULL);
   XtFree(old_buf);
}

/****************************************************************/

set_dbl_default(w, res)
Widget w;
char *res;
{
   char buf[20];
   char *old_buf;
   double value;

   XtVaGetValues(gbl_iw, res, &value, NULL);
   sprintf(buf, "%0.10lf", value);
   XtVaGetValues(w, XmNvalue, &old_buf, NULL);
   if (strcmp(buf, old_buf) != 0)
      XtVaSetValues(w, XmNvalue, buf, NULL);
   XtFree(old_buf);
}

/****************************************************************/

reset_defaults(iw)
Widget iw;
{
   unsigned char value, def;
   Boolean bvalue;

   XtVaGetValues(iw, XvicNimageMode, &value, NULL);
   switch (value) {
      case XvicCOLOR:		def = 0; break;
      case XvicBW:		def = 1; break;
   }
   set_option_default(w_imageMode, def);

   XtVaGetValues(iw, XvicNdataType, &value, NULL);
   switch (value) {
      case XvicBYTE:		def = 0; break;
      case XvicHALF:		def = 1; break;
      case XvicUHALF:		def = 2; break;
      case XvicFULL:		def = 3; break;
      case XvicUFULL:		def = 4; break;
      case XvicREAL:		def = 5; break;
      case XvicDOUBLE:		def = 6; break;
   }
   set_option_default(w_dataType, def);

   XtVaGetValues(iw, XvicNditherMode, &value, NULL);
   XtVaGetValues(iw, XvicNditherMode, &value, NULL);
   switch (value) {
      case XvicNONE:		def = 0; break;
      case XvicORDERED:		def = 1; break;
      case XvicKAGELS:		def = 2; break;
   }
   set_option_default(w_ditherMode, def);

   XtVaGetValues(iw, XvicNlutType, &value, NULL);
   switch (value) {
      case XvicSTRETCH:		def = 0; break;
      case XvicRAW:		def = 1; break;
      case XvicPSEUDO:		def = 2; break;
      case XvicPSEUDO_ONLY:	def = 3; break;
   }
   set_option_default(w_lutType, def);

   XtVaGetValues(iw, XvicNlut16Type, &value, NULL);
   switch (value) {
      case XvicSTRETCH:		def = 0; break;
      case XvicRAW:		def = 1; break;
      case XvicPSEUDO:		def = 2; break;
      case XvicPSEUDO_ONLY:	def = 3; break;
   }
   set_option_default(w_lut16Type, def);

   XtVaGetValues(iw, XvicNstretchPolicy, &value, NULL);
   switch (value) {
      case XvicUSE_HW:		def = 0; break;
      case XvicUSE_SW:		def = 1; break;
   }
   set_option_default(w_stretchPolicy, def);

   XtVaGetValues(iw, XvicNcolormapPolicy, &value, NULL);
   switch (value) {
      case XvicFULL:		def = 0; break;
      case XvicHALF:		def = 1; break;
      case XvicDITHER:		def = 2; break;
      case XvicALLOC:		def = 3; break;
      case XvicFULL_COLOR:	def = 4; break;
   }
   set_option_default(w_colormapPolicy, def);

   XtVaGetValues(iw, XvicNvisualType, &value, NULL);
   switch (value) {
      case XvicUSE_DEFAULT:	def = 0; break;
      case XvicUSE_8BIT:	def = 1; break;
      case XvicUSE_24BIT:	def = 2; break;
   }
   set_option_default(w_visualType, def);

   set_bool_default(w_enableDirectColor, XvicNenableDirectColor);

   XtVaGetValues(iw, XvicNworkProcPolicy, &value, NULL);
   switch (value) {
      case XvicNONE:		def = 0; break;
      case XvicREAD:		def = 1; break;
      case XvicALL:		def = 2; break;
   }
   set_option_default(w_workProcPolicy, def);

   XtVaGetValues(iw, XvicNdataSavePolicy, &value, NULL);
   switch (value) {
      case XvicNONE:		def = 0; break;
      case XvicRAW:		def = 1; break;
      case XvicXIMAGE:		def = 2; break;
      case XvicPIXMAP:		def = 3; break;
   }
   set_option_default(w_dataSavePolicy, def);

   XtVaGetValues(iw, XvicNconstrainPan, &value, NULL);
   switch (value) {
      case XvicNONE:		def = 0; break;
      case XvicX_ONLY:		def = 1; break;
      case XvicY_ONLY:		def = 2; break;
      case XvicBOTH:		def = 3; break;
   }
   set_option_default(w_constrainPan, def);

   XtVaGetValues(iw, XvicNbwDither, &value, NULL);
   switch (value) {
      case XvicNONE:		def = 0; break;
      case XvicORDERED:		def = 1; break;
      case XvicKAGELS:		def = 2; break;
   }
   set_option_default(w_bwDither, def);

   XtVaGetValues(iw, XvicNbwStretchPolicy, &value, NULL);
   switch (value) {
      case XvicUSE_HW:		def = 0; break;
      case XvicUSE_SW:		def = 1; break;
   }
   set_option_default(w_bwStretchPolicy, def);

   XtVaGetValues(iw, XvicNbwColormapPolicy, &value, NULL);
   switch (value) {
      case XvicFULL:		def = 0; break;
      case XvicHALF:		def = 1; break;
      case XvicDITHER:		def = 2; break;
      case XvicALLOC:		def = 3; break;
      case XvicFULL_COLOR:	def = 4; break;
   }
   set_option_default(w_bwColormapPolicy, def);

   XtVaGetValues(iw, XvicNbwVisualType, &value, NULL);
   switch (value) {
      case XvicUSE_DEFAULT:	def = 0; break;
      case XvicUSE_8BIT:	def = 1; break;
      case XvicUSE_24BIT:	def = 2; break;
   }
   set_option_default(w_bwVisualType, def);

   XtVaGetValues(iw, XvicNcolorDither, &value, NULL);
   switch (value) {
      case XvicNONE:		def = 0; break;
      case XvicORDERED:		def = 1; break;
      case XvicKAGELS:		def = 2; break;
   }
   set_option_default(w_colorDither, def);

   XtVaGetValues(iw, XvicNcolorStretchPolicy, &value, NULL);
   switch (value) {
      case XvicUSE_HW:		def = 0; break;
      case XvicUSE_SW:		def = 1; break;
   }
   set_option_default(w_colorStretchPolicy, def);

   XtVaGetValues(iw, XvicNcolorColormapPolicy, &value, NULL);
   switch (value) {
      case XvicFULL:		def = 0; break;
      case XvicHALF:		def = 1; break;
      case XvicDITHER:		def = 2; break;
      case XvicALLOC:		def = 3; break;
      case XvicFULL_COLOR:	def = 4; break;
   }
   set_option_default(w_colorColormapPolicy, def);

   XtVaGetValues(iw, XvicNcolorVisualType, &value, NULL);
   switch (value) {
      case XvicUSE_DEFAULT:	def = 0; break;
      case XvicUSE_8BIT:	def = 1; break;
      case XvicUSE_24BIT:	def = 2; break;
   }
   set_option_default(w_colorVisualType, def);

   XtVaGetValues(iw, XvicNpseudoDither, &value, NULL);
   switch (value) {
      case XvicNONE:		def = 0; break;
      case XvicORDERED:		def = 1; break;
      case XvicKAGELS:		def = 2; break;
   }
   set_option_default(w_pseudoDither, def);

   XtVaGetValues(iw, XvicNpseudoStretchPolicy, &value, NULL);
   switch (value) {
      case XvicUSE_HW:		def = 0; break;
      case XvicUSE_SW:		def = 1; break;
   }
   set_option_default(w_pseudoStretchPolicy, def);

   XtVaGetValues(iw, XvicNpseudoColormapPolicy, &value, NULL);
   switch (value) {
      case XvicFULL:		def = 0; break;
      case XvicHALF:		def = 1; break;
      case XvicDITHER:		def = 2; break;
      case XvicALLOC:		def = 3; break;
      case XvicFULL_COLOR:	def = 4; break;
   }
   set_option_default(w_pseudoColormapPolicy, def);

   XtVaGetValues(iw, XvicNpseudoVisualType, &value, NULL);
   switch (value) {
      case XvicUSE_DEFAULT:	def = 0; break;
      case XvicUSE_8BIT:	def = 1; break;
      case XvicUSE_24BIT:	def = 2; break;
   }
   set_option_default(w_pseudoVisualType, def);

   set_int_default(w_xPan, XvicNxPan);
   set_int_default(w_yPan, XvicNyPan);
   set_int_default(w_xSubpixelPan, XvicNxSubpixelPan);
   set_int_default(w_ySubpixelPan, XvicNySubpixelPan);
   set_int_default(w_xZoomIn, XvicNxZoomIn);
   set_int_default(w_xZoomOut, XvicNxZoomOut);
   set_int_default(w_yZoomIn, XvicNyZoomIn);
   set_int_default(w_yZoomOut, XvicNyZoomOut);
   set_int_default(w_maximumMemory, XvicNmaximumMemory);
   set_int_default(w_grayLevels, XvicNgrayLevels);
   set_int_default(w_redLevels, XvicNredLevels);
   set_int_default(w_greenLevels, XvicNgreenLevels);
   set_int_default(w_blueLevels, XvicNblueLevels);
   set_dbl_default(w_rawDataMin, XvicNrawDataMin);
   set_dbl_default(w_rawDataMax, XvicNrawDataMax);
   set_int_default(w_scaledDataMax, XvicNscaledDataMax);
   set_int_default(w_outputDataMax, XvicNoutputDataMax);
   set_int_default(w_imageWidth, XvicNimageWidth);
   set_int_default(w_imageHeight, XvicNimageHeight);
   set_int_default(w_tileWidth, XvicNtileWidth);
   set_int_default(w_tileHeight, XvicNtileHeight);
   set_dim_default(w_viewWidth, XvicNviewWidth);
   set_dim_default(w_viewHeight, XvicNviewHeight);
   set_dim_default(w_width, XmNwidth);
   set_dim_default(w_height, XmNheight);
   set_int_default(w_xPreSubpixelPan, XvicNxPreSubpixelPan);
   set_int_default(w_yPreSubpixelPan, XvicNyPreSubpixelPan);
   set_int_default(w_xPreZoomIn, XvicNxPreZoomIn);
   set_int_default(w_xPreZoomOut, XvicNxPreZoomOut);
   set_int_default(w_yPreZoomIn, XvicNyPreZoomIn);
   set_int_default(w_yPreZoomOut, XvicNyPreZoomOut);

   XtVaGetValues(iw, XvicNcursorMode, &value, NULL);
   switch (value) {
      case XvicFLOATING:	def = 0; break;
      case XvicPLANTED:		def = 1; break;
   }
   set_option_default(w_cursorMode, def);

   XtVaGetValues(iw, XvicNscrollBarDisplayPolicy, &value, NULL);
   switch (value) {
      case XvicSTATIC:		def = 0; break;
      case XvicAS_NEEDED:	def = 1; break;
      case XvicNEVER:		def = 2; break;
   }
   set_option_default(w_scrollBarDisplayPolicy, def);

   set_int_default(w_cursorX, XvicNcursorX);
   set_int_default(w_cursorY, XvicNcursorY);
   set_dbl_default(w_cursorXfp, XvicNcursorXfp);
   set_dbl_default(w_cursorYfp, XvicNcursorYfp);
   set_string_default(w_cursorForeground, XvicNcursorForeground);
   set_string_default(w_cursorBackground, XvicNcursorBackground);

   set_string_default(w_cursor, XvicNcursor);

   set_bool_default(w_trackFloatingCursor, XvicNtrackFloatingCursor);

}

