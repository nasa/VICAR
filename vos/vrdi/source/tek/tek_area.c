/*	TEK_Area - description
 *
 *	Purpose: Read or write an area to the Tektronix display terminal.
 *
 *	Written by: Paul Bartholomew
 *	Date:	    October 18, 1989
 *
 *	Calling Sequence:
 *		STATUS = TEK_Area(Unit, function, imp, size, in_area, buffer,
 *				  mask, tek_function);
 *
 *	Parameter List:
 *		Unit:	  Display device unit number
 *		function: Function code, either READ_AREA or WRITE_AREA
 *		imp:	  Image plane number
 *		size:	  Size of buffer to transfer in bytes
 *		in_area:  Access Window array
 *		buffer:	  Image to display, or buffer to hold image read
 *		mask:	  Bit plane mask
 *		tek_function:  Determines which routine called this function
 *			  so that options can be set accordingly.  Currently,
 *			  this function is called by:
 *
 *			  TEK_Circle	-- TEK_REFRESH_AREA
 *			  TEK_Connect	-- TEK_LUT_AREA
 *			  TEK_Fill	-- TEK_REFRESH_AREA
 *			  TEK_Interface	-- TEK_WRITE_AREA
 *			  TEK_Lut	-- TEK_LUT_AREA
 *			  TEK_Overlay	-- TEK_REFRESH_SCREEN
 *			  TEK_Rotate	-- TEK_REFRESH_AREA
 *			  TEK_SetDW	-- TEK_REFRESH_SCREEN
 *			  TEK_Vector	-- TEK_REFRESH_AREA
 *			  TEK_Zoom	-- TEK_REFRESH_SCREEN
 *
 *	Possible Error Codes:
 *		none
 */

#include "xdexterns.h"
#include "xdroutines.h"
#include "xderrors.h"
#include "xdfuncs.h"
#include "xdsupport.h"
#include "tek.h"
#include <stdio.h>

FUNCTION TEK_Area(Unit, function, imp, size, in_area, buffer, mask,
                  tek_function)
int *Unit, function, imp, size, in_area[4];
unsigned char *buffer, mask;
int tek_function;
{
  long  jterm;
  int   ctr, xmin, xmax, ymin, ymax, nlines, nsamps, lut, linectr, ovlinectr;
  int	nwords, npix, pxlines, tempsamp, impline, ovline, tempimp, limit;
  int	vidline, vidsamp, impsamp, ovsamp, ovzoom, zoom, done=FALSE, ycoord;
  int	imp_or_mode=FALSE, out_of_range, area[4], sampctr, ovsampctr;
  unsigned char *tempbuf;

  /*  If we are filling a specified access window or we are writing data  */
  /*  to the image plane, then we have a specific area to use.  Otherwise */
  /*  use the entire image plane.                                         */

  if ((tek_function != TEK_WRITE_AREA) && (tek_function != TEK_REFRESH_AREA))
  {
    area[LEFT] = 1;
    area[RIGHT] = N_SAMPS;
    area[TOP] = 1;
    area[BOTTOM] = N_LINES;
    size = N_SAMPS * N_LINES;
  }
  else
  {
    area[LEFT] = in_area[LEFT];
    area[RIGHT] = in_area[RIGHT];
    area[TOP] = in_area[TOP];
    area[BOTTOM] = in_area[BOTTOM];
  }

  nsamps = area[RIGHT] - area[LEFT] + 1;
  nlines = area[BOTTOM] - area[TOP] + 1;

  if (function == WRITE_AREA)
  {
    /*  If we are re-drawing an image plane because the look-up table has    */
    /*  been modified, then the "imp" parameter is actually the look-up      */
    /*  that has been modified.  We find the image plane that the look-up    */
    /*  is connected to and use that in subsequent calculations.             */

    if (tek_function == TEK_LUT_AREA)
      tempimp = TEK_WHICH_IMP(imp);
    else
      tempimp = imp;

    /*  If the terminal is being used in stereo mode and is set for 512x512  */
    /*  image planes, then double the number of samples output.              */

    if ((DEV_TYPE != TEK_4237) && (IMP_SIZE == IMP_512))
    {
      zoom = 2 * TEK_ZOOM(tempimp);
      ovzoom = 2 * TEK_ZOOM(TEK_OVERLAY_IMP);
    }
    else
    {
      zoom = TEK_ZOOM(tempimp);
      ovzoom = TEK_ZOOM(TEK_OVERLAY_IMP);
    }

    /*  If not performing a screen refresh, then put the pixel values into */
    /*  the stored image memory plane.                                     */

    /*  There are three possibilities to consider when putting the image   */
    /*  into image memory:  1) the image is the same size as the access    */
    /*  window, 2) the image is smaller than the access window, and 3) the */
    /*  image is larger than the access window.                            */

    /*  If the image is smaller than the access window, we'll run out of   */
    /*  data before filling the access window.  When this happens, we      */
    /*  simply break out of the loop.                                      */

    if (tek_function == TEK_WRITE_AREA)
    {
      TEK_WRITTEN(imp) = TRUE;
      for (ctr=0, vidline=0; (ctr * nsamps) < size; ctr++)
      {
        impline = area[TOP] + vidline;
        for (vidsamp = 0; vidsamp < nsamps; vidsamp++)
        {
          impsamp = area[LEFT] + vidsamp;
          if ((vidsamp+(ctr*nsamps)) >= size)
            break;
          switch (DEV_TYPE)
          {
            case TEK_4237:
              if (!imp_or_mode)
                TEK_IMP(imp,impsamp,impline) = (TEK_IMP(imp,impsamp,impline) &
                             ~mask) | (buffer[vidsamp + (ctr * nsamps)] & mask);
              else
                TEK_IMP(imp,impsamp,impline) |= (TEK_IMP(imp,impsamp,impline) &
                             ~mask) | (buffer[vidsamp + (ctr * nsamps)] & mask);
              break;
            case TEK_3D_LEFT:
              if (!imp_or_mode)
                TEK_IMP_3DL(imp,impsamp,impline) = 
                               (TEK_IMP_3DL(imp,impsamp,impline) & ~mask) | 
                               (buffer[vidsamp + (ctr * nsamps)] & mask);
              else
                TEK_IMP_3DL(imp,impsamp,impline) |= 
                               (TEK_IMP_3DL(imp,impsamp,impline) & ~mask) | 
                               (buffer[vidsamp + (ctr * nsamps)] & mask);
              break;
            case TEK_3D_RIGHT:
              if (!imp_or_mode)
                TEK_IMP_3DR(imp,impsamp,impline) = 
                               (TEK_IMP_3DR(imp,impsamp,impline) & ~mask) | 
                               (buffer[vidsamp + (ctr * nsamps)] & mask);
              else
                TEK_IMP_3DR(imp,impsamp,impline) |= 
                               (TEK_IMP_3DR(imp,impsamp,impline) & ~mask) | 
                               (buffer[vidsamp + (ctr * nsamps)] & mask);
              break;
          }
        }

        /*  If the access window is too small, we'll have data left over   */
        /*  after we've filled the access window.  When this happens, we   */
        /*  simply start at the top again--continuing to output data until */
        /*  we've exhausted the input buffer.                              */

        vidline++;
        if (vidline >= nlines)
        {
          vidline = 0;
          imp_or_mode = TRUE;
        }
      }
    }

    if ((tempimp == TEK_OVERLAY_IMP) && (TEK_OVERLAY_ON == FALSE))
      return (SUCCESS);
 
    if ((tek_function == TEK_WRITE_AREA) || (tek_function == TEK_REFRESH_AREA))
    {
      area[RIGHT] += TEK_DW_LEFT(tempimp) - 1;
      area[BOTTOM] += TEK_DW_TOP(tempimp) - 1;
      size = (area[RIGHT]-area[LEFT]+1) * (area[BOTTOM]-area[TOP]+1);
    }

    nsamps = area[RIGHT] - area[LEFT] + 1;
    nlines = area[BOTTOM] - area[TOP] + 1;

    xmin = (area[LEFT] * zoom) - (zoom-1);
    xmin = X_VRDI2TEK(xmin);
    xmax = area[RIGHT] * zoom;
    if (((DEV_TYPE == TEK_4237) || (IMP_SIZE != IMP_512)) && (xmax > N_SAMPS))
      xmax = N_SAMPS;
    if ((DEV_TYPE != 4237) && (IMP_SIZE == IMP_512) && (xmax > 2*N_SAMPS))
      xmax = 2 * N_SAMPS;
    xmax = X_VRDI2TEK(xmax);

    ymin = area[BOTTOM] * TEK_ZOOM(tempimp);
    if (ymin > N_LINES)
      ymin = N_LINES;
    ymin = Y_VRDI2TEK(ymin);
    ymax = (area[TOP] * TEK_ZOOM(tempimp)) - (TEK_ZOOM(tempimp)-1);
    ymax = Y_VRDI2TEK(ymax);

    /*  The actual number of pixels to display on screen will depend on  */
    /*  the zoom factor and the screen size.                             */

    npix = MIN((nsamps * zoom), (xmax - xmin + 1));
    pxlines = MIN((nlines * TEK_ZOOM(tempimp)), (ymax - ymin + 1));

    /*  Since the Tektronix terminal accepts data in 32-bit words,  */
    /*  determine the number of words needed for a line of data.    */

    if ((npix % PIXELS_PER_WORD) == 0)
      nwords = npix / PIXELS_PER_WORD;
    else
      nwords = (npix / PIXELS_PER_WORD) + 1;

    /*  Allocate enough storage to handle a line of pixels.  */

    if ((tempbuf = (unsigned char *) malloc(nwords * PIXELS_PER_WORD)) == NULL)
      return (MEMORY_ERROR);

    llinit(&jterm);			/* Start STI */
    llkblk(TEK_ON);			/* Lock keyboard while working */
    llcode(TEK_BINARY);			/* Set display mode to binary */

    llmove(0, 0);
    lpzbas();
    lpm2a(0, 0);
    lpups(0x7f);

    lppipe(PIPE_MASK, PIPE_MODE);

    /*  Since more than one look-up table can be connected to an image  */
    /*  plane, we check each look-up table.  If, however, we are per-   */
    /*  forming an XDLCONNECT or XDLWRITE, then only one look-up table  */
    /*  is affected--we do not need to refresh the screen for the other */
    /*  look-up tables.                                                 */

    for (lut = 1; ((lut <= N_LUTS) && (!done)); lut++)
    {
      out_of_range = FALSE;
      if (tek_function == TEK_LUT_AREA)
      {
        lut = imp;
        imp = TEK_WHICH_IMP(lut);
        done = TRUE;
      }

      if (imp == TEK_OVERLAY_IMP)
      {
        tempimp = TEK_WHICH_IMP(lut);
        if ((DEV_TYPE != TEK_4237) && (IMP_SIZE == IMP_512))
          zoom = 2 * TEK_ZOOM(tempimp);
        else
          zoom = TEK_ZOOM(tempimp);
      }
      else
        tempimp = imp;

      if (TEK_WHICH_IMP(lut) == tempimp)
      {
        /*  Set the terminal such that only the appropriate eight bits  */
        /*  of the 24-bit pixels values will be affected.               */

        lpfbw(PXSHIFT[lut], PXMASK[lut]);

        /*  Output the data, one line at a time, until all the data has */
        /*  been used.                                                  */

        if (imp != TEK_OVERLAY_IMP)
        {
          impline = ((area[TOP] + N_LINES - TEK_DW_TOP(tempimp)) % N_LINES) + 1;
          linectr = 0;
          ovline = Y_TEK2VRDI(ymax);
          ovlinectr = (ovline + TEK_ZOOM(TEK_OVERLAY_IMP) - 1) %
                                                     TEK_ZOOM(TEK_OVERLAY_IMP);
          ovline = (ovline + TEK_ZOOM(TEK_OVERLAY_IMP) - 1) /
                                                     TEK_ZOOM(TEK_OVERLAY_IMP);
          ovline = ((ovline + N_LINES - TEK_DW_TOP(TEK_OVERLAY_IMP)) %
                                                                   N_LINES) + 1;
        }
        else
        {
          impline = Y_TEK2VRDI(ymax);
          linectr = (impline + TEK_ZOOM(tempimp) - 1) % TEK_ZOOM(tempimp);
          impline = (impline + TEK_ZOOM(tempimp) - 1) / TEK_ZOOM(tempimp);
          impline = ((impline + N_LINES - TEK_DW_TOP(tempimp)) % N_LINES) + 1;

          ovline = ((area[TOP] + N_LINES - TEK_DW_TOP(TEK_OVERLAY_IMP)) %
                                                                   N_LINES) + 1;
          ovlinectr = 0;
        }

        for (ctr=0, vidline=0; ((ctr * nsamps) < size) && (!out_of_range); )
        {
          if (imp != TEK_OVERLAY_IMP)
          {
            impsamp = ((area[LEFT]+N_SAMPS-TEK_DW_LEFT(tempimp)) % N_SAMPS) + 1;
            limit = nsamps;
            sampctr = 0;
          }
          else
          {
            impsamp = X_TEK2VRDI(xmin);
            sampctr = (impsamp + zoom - 1) % zoom;
            impsamp = (impsamp + zoom - 1) / zoom;
            impsamp = ((impsamp + N_SAMPS-TEK_DW_LEFT(tempimp)) % N_SAMPS) + 1;
            limit = nsamps*ovzoom;
          }

          for (vidsamp = 0; vidsamp < limit; vidsamp++)
          {
            if ((vidsamp+(ctr*nsamps)) >= size)
              break;

            /*  Put a pixel value into the line of pixels.  We duplicate  */
            /*  the pixel value according to the zoom factor.             */

            for (; sampctr < zoom; sampctr++)
            {
              /*  If the access window is too small or the zoom factor  */
              /*  is too large, we may be trying to write off the edge  */
              /*  of the screen.  If this happens, we stop filling the  */
              /*  buffer.                                               */

              if (((vidsamp*zoom)+sampctr) > npix)
                break;

              /*  Place a value from image plane memory into the buffer */

              switch (DEV_TYPE)
              {
                case TEK_4237:
                  if (BYPASS(lut))
                    tempbuf[(vidsamp*zoom)+sampctr] = TEK_IMP(tempimp, impsamp,
                                                              impline);
                  else
                    tempbuf[(vidsamp*zoom)+sampctr] = TEK_LUT(lut,
                                           TEK_IMP(tempimp, impsamp, impline));
                  break;
                case TEK_3D_LEFT:
                  if (BYPASS(lut))
                    tempbuf[(vidsamp*zoom)+sampctr] =
                                 TEK_IMP_3DL(tempimp, impsamp, impline);
                  else
                    tempbuf[(vidsamp*zoom)+sampctr] = TEK_LUT_3DL(lut,
                                TEK_IMP_3DL(tempimp, impsamp, impline));
                  break;
                case TEK_3D_RIGHT:
                  if (BYPASS(lut))
                    tempbuf[(vidsamp*zoom)+sampctr] =
                                 TEK_IMP_3DR(tempimp, impsamp, impline);
                  else
                    tempbuf[(vidsamp*zoom)+sampctr] = TEK_LUT_3DR(lut,
                                TEK_IMP_3DR(tempimp, impsamp, impline));
                  break;
              }
            }
            sampctr = 0;
            impsamp++;
            if (impsamp > N_SAMPS)
              impsamp = 1;
          }

          /*  Since we must have an integer number of words to send to the  */
          /*  Tektronix routines, we fill any remaining space with zeros.   */

          for (impsamp=zoom*vidsamp; impsamp<nwords*PIXELS_PER_WORD; impsamp++)
            tempbuf[impsamp] = 0;

          /*  If the overlay plane is active, we replace the pixel values   */
          /*  in the image plane with any non-zero values in the overlay    */
          /*  plane.                                                        */

          if (TEK_OVERLAY_ON)
          {
            if (imp == TEK_OVERLAY_IMP)
            {
              ovsamp = ((area[LEFT]+N_SAMPS-TEK_DW_LEFT(TEK_OVERLAY_IMP)) %
                                                                  N_SAMPS) + 1;
              limit = nsamps;
              ovsampctr = 0;
            }
            else
            {
              ovsamp = X_TEK2VRDI(xmin);
              ovsampctr = (ovsamp + ovzoom - 1) % ovzoom;
              ovsamp = (ovsamp + ovzoom - 1) / ovzoom;
              ovsamp = ((ovsamp+N_SAMPS-TEK_DW_LEFT(TEK_OVERLAY_IMP)) %
                                                                  N_SAMPS) + 1;
              limit = nsamps * zoom;
            }

            for (vidsamp = 0; vidsamp < limit; vidsamp++)
            {
              if ((vidsamp+(ctr*nsamps)) >= size)
                break;

              /*  Put a pixel value into the line of pixels.  We duplicate  */
              /*  the pixel value according to the zoom factor.             */

              for (; ovsampctr < ovzoom; ovsampctr++)
              {

                /*  If the access window is too small or the zoom factor  */
                /*  is too large, we may be trying to write off the edge  */
                /*  of the screen.  If this happens, we stop filling the  */
                /*  buffer.                                               */

                if (((vidsamp*ovzoom)+ovsampctr) > npix)
                  break;

                /*  Place value from overlay plane memory into the buffer */
                /*  (only if value is non-zero)                           */

                switch (DEV_TYPE)
                {
                  case TEK_4237:
                    if (TEK_IMP(TEK_OVERLAY_IMP, ovsamp, ovline) != 0)
                      tempbuf[(vidsamp*ovzoom)+ovsampctr] = TEK_OVLY_LUT(lut,
                                     TEK_IMP(TEK_OVERLAY_IMP, ovsamp, ovline));
                    break;
                  case TEK_3D_LEFT:
                    if (TEK_IMP_3DL(TEK_OVERLAY_IMP, ovsamp, ovline) != 0)
                      tempbuf[(vidsamp*ovzoom)+ovsampctr] = TEK_OVLY_LUT_L(lut,
                                 TEK_IMP_3DL(TEK_OVERLAY_IMP, ovsamp, ovline));
                    break;
                  case TEK_3D_RIGHT:
                    if (TEK_IMP_3DR(TEK_OVERLAY_IMP, ovsamp, ovline) != 0)
                      tempbuf[(vidsamp*ovzoom)+ovsampctr] = TEK_OVLY_LUT_R(lut,
                                 TEK_IMP_3DR(TEK_OVERLAY_IMP, ovsamp, ovline));
                    break;
                }
              }

              ovsampctr=0;
              ovsamp++;
              if (ovsamp > N_SAMPS)
                ovsamp = 1;
            }
          }  /* end if (TEK_OVERLAY_ON) */

          /*  Reverse the string of pixels within word boundaries, since    */
          /*  the Tektronix terminal reads each word in reverse order.      */

          reverse(tempbuf, (nwords*PIXELS_PER_WORD), FALSE);

          /*  Output the line of pixels  */

          if (DEV_TYPE == TEK_3D_RIGHT)
          {
            ycoord = ymax + STEREO_OFFSET - vidline;
            lpm2a(xmin, ycoord);
            if (ycoord <= 0)
              out_of_range = TRUE;
          }
          else
          {
            ycoord = ymax - vidline;
            lpm2a(xmin, ycoord);
            if (ycoord <= 0)
              out_of_range = TRUE;
          }

          lprwrt(PIXELS_PER_WORD, npix, nwords, tempbuf);
          lldump();

          /*  Increment the video screen and image plane line numbers.  If  */
          /*  we have gone below the access window, we reset the line number*/
          /*  to zero and continue to output pixels back at the top of the  */
          /*  window.                                                       */

          vidline++;
          if (vidline >= (nlines*TEK_ZOOM(imp)))
            vidline = 0;

          linectr++;
          if (linectr >= TEK_ZOOM(tempimp))
          {
            if (imp != TEK_OVERLAY_IMP)
              ctr++;
            linectr = 0;
            impline++;
            if (impline > N_LINES)
              impline = 1;
          }

          ovlinectr++;
          if (ovlinectr >= TEK_ZOOM(TEK_OVERLAY_IMP))
          {
            if (imp == TEK_OVERLAY_IMP)
              ctr++;
            ovlinectr = 0;
            ovline++;
            if (ovline > N_LINES)
              ovline = 1;
          }
        }
      }
    }
    lpfbw(PXSHIFT[TEK_RESET], PXMASK[TEK_RESET]);

    lpops();
    llmove(0, 0);
    lpzbas();
    lpm2a(0, 0);

    llkblk(TEK_OFF);			/* Unlock keyboard when finished */
    llcode(TEK_ANSI);			/* Reset terminal mode to ANSI */
    llstop();
  }
  else					/* Function = READ_AREA */
  {
    for (impline = 0; impline < nlines; impline++)
    {
      for (impsamp = 0; impsamp < nsamps; impsamp++)
      {
        switch (DEV_TYPE)
        {
          case TEK_4237:
            buffer[impsamp+(nsamps*impline)] = TEK_IMP(imp, area[LEFT]+impsamp,
                                                            area[TOP]+impline);
            break;
          case TEK_3D_LEFT:
            buffer[impsamp+(nsamps*impline)] = TEK_IMP_3DL(imp,
                                        area[LEFT]+impsamp, area[TOP]+impline);
            break;
          case TEK_3D_RIGHT:
            buffer[impsamp+(nsamps*impline)] = TEK_IMP_3DR(imp,
                                        area[LEFT]+impsamp, area[TOP]+impline);
            break;
        }
      }
    }
  }
  return (SUCCESS);
}
