#include "xvmaininc.h"

#include <stdio.h>
#if VMS_OS
#include <types.h>
#include <socket.h>
#else
#include <sys/types.h>
#include <sys/socket.h>
#endif

#if VMS_OS && ALPHA_ARCH
#define CADDR_T		/* prevent multiple define with socket.h and xlib.h */
#endif
#include "xdexterns.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "x11_device_main.h"

#ifdef DEBUG
#define DPR(A)  {printf A; fflush(stdout);}
#else
#define DPR(A)
#endif

static int fd[MAXIMUM_UNITS];

#define FD fd[*Unit]

static int int_size = sizeof(int);


x_opendevice(Unit)
int *Unit;
{
   char device[8], sockname[20];
   struct sockaddr slave;
   int status, request;

   /* get the current device name, DIB is initialized from xd_initialize() */
   strcpy(device, DIB[*Unit]->DeviceName);

   /* create the socket name */
#if VMS_OS		/* VMS names are unique per process tree anyway */
   sprintf(sockname, "%s%s", X_SOCKET_HDR, device);
#else
   sprintf(sockname, "%s%s%04.4x", X_SOCKET_HDR, device, getuid());
#endif

   DPR(("x_utilities: sockname='%s'\n", sockname));
   /* open the socket and connect to it */
   FD = socket_open(0, sockname, &slave, 0);
   if (FD == -1)
      status = CANNOT_ALLOC_DEVICE;
   else
      status = SUCCESS;

   request = OPEN_DEVICE;
   socket_send (FD, &request, int_size);

   return(status);
}


x_closedevice(Unit)
int *Unit;
{
   int request, status;

   request = CLOSE_DEVICE;
   socket_send(FD, &request, int_size);
   socket_free(FD, NULL);
  
   return(SUCCESS);
}


x_write_area(Unit, imp, size, aw, image, mask)
int *Unit, imp, size, aw[4];
unsigned char image[], mask;
{
   register int i;
   int request, sl, ss, nl, ns, ctr;

   sl = aw[TOP];
   ss = aw[LEFT];
   nl = aw[BOTTOM] - aw[TOP]  + 1;
   ns = aw[RIGHT]  - aw[LEFT] + 1;

   DPR(("x_utilities: in write_area\n"));
   request = WRITE_AREA;
   socket_send(FD, &request, int_size);
   socket_send(FD, &sl, int_size);
   socket_send(FD, &ss, int_size);
   socket_send(FD, &nl, int_size);
   socket_send(FD, &ns, int_size);
   socket_send(FD, &imp, int_size);
   socket_send(FD, &size, int_size);
   socket_send(FD, &mask, sizeof(unsigned char));

   DPR(("x_utilities: sending image\n"));
   for (i = 0, ctr = 0; ctr < size; i++, ctr += ns) {
      i = i % nl;
      if (ctr + ns <= size)
         socket_send(FD, &image[i*ns], ns);
      else {
         DPR(("x_utilities: not enough data in last line, only %d bytes\n",
              (size-ctr)));
         socket_send(FD, &image[i*ns], (size-ctr));
      }
   }
   DPR(("x_utilities: leave write_area\n"));
   return (SUCCESS);
}


x_read_area(Unit, imp, size, aw, array)
int *Unit, imp, size, aw[4];
char array[];
{
   int i, request, ss, sl, ns, nl, ctr;

   ss = aw[LEFT];
   sl = aw[TOP];
   ns = aw[RIGHT]  - ss + 1;
   nl = aw[BOTTOM] - sl + 1;

   DPR(("x_utilities: in x_read_area--ss=%d sl=%d ns=%d nl%d\n", ss, sl, ns, nl));

   request = READ_AREA;
   socket_send(FD, &request, int_size);
   socket_send(FD, &ss, int_size);
   socket_send(FD, &sl, int_size);
   socket_send(FD, &ns, int_size);
   socket_send(FD, &nl, int_size);
   socket_send(FD, &imp, int_size);
   socket_send(FD, &size, int_size);

   for (i = 0, ctr = 0; ctr < size; i++, ctr += ns) {
      i = i % nl;
      if (ctr + ns <= size)
         socket_recv(FD, &array[i*ns], ns);
      else
         socket_recv(FD, &array[i*ns], (size-ctr));
   }
   return(SUCCESS);
}


x_read_line(Unit, imp, xpos, ypos, len, array)
int *Unit, imp, xpos, ypos, len;
char array[];
{
   int aw[4];

   aw[LEFT] = xpos;
   aw[RIGHT] = xpos+len-1;
   aw[TOP] = ypos;
   aw[BOTTOM] = ypos;

   x_read_area(Unit, imp, len, aw, array);

   return(SUCCESS);  
}


x_read_pixel(Unit, imp, xpos, ypos, value)
int *Unit, imp, xpos, ypos;
char *value;
{
   int aw[4], size=1;
   char buffer[10];

   aw[LEFT] = xpos;
   aw[RIGHT] = xpos;
   aw[TOP] = ypos;
   aw[BOTTOM] = ypos;

   x_read_area(Unit, imp, size, aw, buffer);
   *value = buffer[0];
   return(SUCCESS);
}


x_writeline(Unit, imp, left, ypos, length, buffer, mask)
int *Unit, imp, left, ypos, length;
unsigned char buffer[], mask;
{
   int aw[4];

   aw[LEFT]   = left;
   aw[TOP]    = ypos;
   aw[RIGHT]  = left+length-1;
   aw[BOTTOM] = ypos;

   x_write_area(Unit, imp, length, aw, buffer, mask);
   return(SUCCESS);
}


x_write_pixel(Unit, imp, xpos, ypos, value, mask)
int *Unit, imp, ypos, xpos;
unsigned char *value, mask;
{
  int  aw[4];

   DPR(("x_utilities: in x_write_pixel, imp=%d, value=%d, x=%d, y=%d\n",
        imp, (int) *value, xpos, ypos));
   aw[LEFT]   = xpos;
   aw[RIGHT]  = xpos;
   aw[TOP]    = ypos;
   aw[BOTTOM] = ypos;

   x_fill_area(Unit, imp, *value, mask, aw);
   DPR(("x_utilities: leave x_write_pixel\n"));

   return(SUCCESS);
}


x_fill_area(Unit, imp, value, mask, aw)
int *Unit, imp;
unsigned char value, mask;
int aw[4];
{
   int request;
   int ss, sl, nl, ns;

   ss = aw[LEFT];
   sl = aw[TOP];
   ns = aw[RIGHT] - aw[LEFT] + 1;
   nl = aw[BOTTOM] - aw[TOP] + 1;

   DPR(("x_utilities: in x_fill_area\n"));
   request = FILL_AREA;
   socket_send(FD, &request, int_size);
   socket_send(FD, &ss, int_size);
   socket_send(FD, &sl, int_size);
   socket_send(FD, &ns, int_size);
   socket_send(FD, &nl, int_size);
   socket_send(FD, &imp, int_size);
   socket_send(FD, &value, sizeof(char));
   socket_send(FD, &mask, sizeof(char));

   return(SUCCESS);
}


x_write_vector(Unit, imp, npts, x, y, value, mask)
int *Unit, imp, npts, x[], y[];
unsigned char value, mask;
{
   int request;

   DPR(("x_utilities: in write_vector\n"));
   request = WRITE_VECTOR;
   socket_send(FD, &request, int_size);
   socket_send(FD, &imp, int_size);
   socket_send(FD, &npts, int_size);
   socket_send(FD, &value, sizeof(unsigned char));
   socket_send(FD, &mask, sizeof(unsigned char));
   socket_send(FD, x, int_size*npts);
   socket_send(FD, y, int_size*npts);
   DPR(("x_utilities: leave write_vector\n"));
   return (SUCCESS);
}  


x_draw_circle(Unit, imp, xcenter, ycenter, radius, value, mask, area)
int *Unit, imp, xcenter, ycenter, radius;
unsigned char value, mask;
int area[4];
{
   int request;

   DPR(("x_utilities: in x_draw_circle\n"));
   request = DRAW_CIRCLE;
   socket_send(FD, &request, int_size);
   socket_send(FD, &imp, int_size);
   socket_send(FD, &xcenter, int_size);
   socket_send(FD, &ycenter, int_size);
   socket_send(FD, &radius, int_size);
   socket_send(FD, &value, sizeof(unsigned char));
   socket_send(FD, &mask, sizeof(unsigned char));
   socket_send(FD, area, 4*int_size);
   DPR(("x_utilites: leave x_draw_circle"));
   return (SUCCESS);
}


x_graphics_on(Unit)
int *Unit;
{
   int request;

   DPR(("x_utilities: in x_graphics_on\n"));
   request = GRAPHICS_ON;
   socket_send(FD, &request, int_size);
   DPR(("x_utilities: return from x_graphics_on\n"));
   return(SUCCESS);
}


x_graphics_off(Unit)
int *Unit;
{
   int request;

   DPR(("x_utilities: in x_graphics_off\n"));
   request = GRAPHICS_OFF;
   socket_send(FD, &request, int_size);
   DPR(("x_utilities: return from x_graphics_off\n"));
   return(SUCCESS);
}


x_autotrack(Unit, function, device, cursor)
int *Unit, function, cursor, device;
{
   int flag;

   DPR(("x_utilities: in x_autotrack\n"));

   if (function == AUTO_ON)
      flag = TRUE;
   else
      flag = FALSE;

   socket_send(FD, &function, int_size);
   socket_send(FD, &cursor, int_size);
   socket_send(FD, &flag, int_size);
   return (SUCCESS);
}    


x_cursor_on(Unit, cursor, ctype)
int *Unit, cursor, ctype;
{
   int request;

   DPR(("x_utilities: in x_cursor_on\n"));
   request = CURSOR_ON;
   socket_send(FD, &request, int_size);
   socket_send(FD, &cursor, int_size);
   socket_send(FD, &ctype, int_size);
   return(SUCCESS);
}


x_cursor_off(Unit, cursor)
int *Unit, cursor;
{
   int request;

   DPR(("x_utilities: in x_cursor_off\n"));
   request = CURSOR_OFF;
   socket_send(FD, &request, int_size);
   socket_send(FD, &cursor, int_size);
   return(SUCCESS);
}


x_write_cursor(Unit, cursor, x, y)
int *Unit, cursor, x, y;
{
   int request;

   DPR(("x_utilities: in x_write_cursor\n"));
   request = WRITE_CURSOR;
   socket_send(FD, &request, int_size);
   socket_send(FD, &cursor, int_size);
   socket_send(FD, &x, int_size);
   socket_send(FD, &y, int_size);
   DPR(("x_utilities: leave x_write_cursor\n"));
   return(SUCCESS);
}


x_read_cursor(Unit, cursor, x, y)
int *Unit, cursor, *x, *y;
{
   int request;

   DPR(("x_utilities: in x_read_cursor\n"));
   request = READ_CURSOR;
   socket_send(FD, &request, int_size);
   socket_send(FD, &cursor, int_size);
   socket_recv(FD, x, int_size);
   socket_recv(FD, y, int_size);
   DPR(("x_utilities: leave x_read cursor x=%d, y=%d\n", *x, *y));
   return(SUCCESS);
}


x_read_switch(Unit, device, switch_no, value)
int *Unit, device, switch_no, *value;
{
   int request;

   request = READ_SWITCH;
   socket_send(FD, &request, int_size);
   socket_send(FD, &switch_no, int_size);
   socket_recv(FD, value, int_size);
   return(SUCCESS);
}
  

x_read_2d(Unit, device, xvalue, yvalue, prox, pen)
int *Unit, device;
float *xvalue, *yvalue;
int *prox, *pen;
{
   int request;
   int fsize = sizeof (float);

   DPR(("x_utilities: in x_x2d\n"));
   request = READ_2D;
   socket_send(FD, &request, int_size);
   socket_recv(FD, xvalue, fsize);
   socket_recv(FD, yvalue, fsize);
   socket_recv(FD, prox, int_size);
   socket_recv(FD, pen, int_size);
   DPR(("x_utilities: return from x_x2d\n"));
   return(SUCCESS);
}


x_zoom(Unit, imp, zoom)
int *Unit, imp, zoom;
{
   int request;

   request = ZOOM_IMP;
   socket_send(FD, &request, int_size);
   socket_send(FD, &zoom, int_size);
   socket_send(FD, &imp, int_size);
   return(SUCCESS);
}


x_set_dw(Unit, imp, x, y)
int *Unit, imp, x, y;
{
   int request;

   DPR(("x_utilities: x_set_dw, imp=%d, x=%d, y=%d\n", imp, x, y));
   request = SET_DW;
   socket_send(FD, &request, int_size);
   socket_send(FD, &imp, int_size);
   socket_send(FD, &x, int_size);
   socket_send(FD, &y, int_size);
   return(SUCCESS);
}
  

x_collect_histogram(Unit, imp, mask, hist, imp_aw, mask_aw)
int *Unit, imp, mask, hist[], imp_aw[], mask_aw[];
{
   int request;

   request = COLLECT_HISTOGRAM;
   socket_send(FD, &request, int_size);
   socket_send(FD, &imp, int_size);
   socket_send(FD, &mask, int_size);
   socket_send(FD, imp_aw, 4*int_size);
   socket_send(FD, mask_aw, 4*int_size);
   socket_recv(FD, hist, X_LUT_SIZE*int_size);
   return(SUCCESS);
}


x_connect_lut(Unit, imp, lut)
int *Unit, *imp, *lut;
{
   int request;

   request = CONNECT_IMPS_LUTS;
   socket_send(FD, &request, int_size);
   socket_send(FD, imp, int_size);
   socket_send(FD, lut, int_size);
   return (SUCCESS);
}


x_connect_glut(Unit, imp)
int *Unit, imp;
{
   int request;

   request = CONNECT_OVERLAY;
   socket_send(FD, &request, int_size);
   socket_send(FD, &imp, int_size);
   return(SUCCESS);
}


x_write_lut(Unit, lut, section, array)
int *Unit, lut, section, array[];
{
   int request;

   request = WRITE_LUT;
   socket_send(FD, &request, int_size);
   socket_send(FD, &lut, int_size);
   socket_send(FD, array, X_LUT_SIZE*int_size);
   return (SUCCESS);
}


x_read_lut(Unit, lut, section, array)
int *Unit, lut, section, array[];
{
   int request;

   request = READ_LUT;
   socket_send(FD, &request, int_size);
   socket_send(FD, &lut, int_size);
   socket_recv(FD, array, X_LUT_SIZE*int_size);
   DPR(("x_utilities: lut %d received\n", lut));
   return (SUCCESS);
}


x_write_overlay_lut(Unit, red, green, blue)
int *Unit, red[], green[], blue[];
{
   int request, unit;

   unit = *Unit;
   request = WRITE_OVERLAY_LUT;
   socket_send(FD, &request, int_size);
   socket_send(FD, red,   X_Overlay_Size*int_size);
   socket_send(FD, green, X_Overlay_Size*int_size);
   socket_send(FD, blue,  X_Overlay_Size*int_size);
   return (SUCCESS);
}


x_read_overlay_lut(Unit, red, green, blue)
int *Unit, red[], green[], blue[];
{
   int request, unit;

   unit = *Unit;
   DPR(("x_utilities: entering x_read_overlay_lut\n"));
   request = READ_OVERLAY_LUT;
   socket_send(FD, &request, int_size);
   socket_recv(FD, red, X_Overlay_Size*int_size);
   socket_recv(FD, green, X_Overlay_Size*int_size);
   socket_recv(FD, blue, X_Overlay_Size*int_size);
   return (SUCCESS);
}


x_open_close_win(Unit, open_flag)
int *Unit, open_flag;
{
   int request;

   request = OPEN_CLOSE_WIN;
   socket_send(FD, &request, int_size);
   socket_send(FD, &open_flag, int_size);
   return(SUCCESS);
}


x_move_win(Unit, x_loc, y_loc)
int *Unit, x_loc, y_loc;
{
   int request;

   request = MOVE_WIN;
   socket_send(FD, &request, int_size);
   socket_send(FD, &x_loc, int_size);
   socket_send(FD, &y_loc, int_size);
   return(SUCCESS);
}


x_resize_win(Unit, width, height)
int *Unit, width, height;
{
   int request, status;

   request = RESIZE_WIN;
   socket_send(FD, &request, int_size);
   socket_send(FD, &width, int_size);
   socket_send(FD, &height, int_size);
   return(SUCCESS);
}


x_set_batch_mode(Unit, flag)
int *Unit, flag;
{
   int request;

   request = SET_BATCH_MODE;
   socket_send(FD, &request, int_size );
   socket_send(FD, &flag, int_size );
   return (SUCCESS);
}


error(buf)
char *buf;
{
   printf("\n%s\n", buf);
}
