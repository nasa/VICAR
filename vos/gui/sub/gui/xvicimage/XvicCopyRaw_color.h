/************************************************************************/
/* XvicCopyRaw_color.h							*/
/* This code, called by XvicCopyRawFn_{1|3}band.h, creates the actual	*/
/* processing loops based on how the data is being displayed for color	*/
/* data (pseudocolor is color by this point).  This is the first time	*/
/* the type of X display comes into play.				*/
/************************************************************************/

   switch (biw->bim.DN_transform) {

      case TRANS_FULLCOLOR:

         if (ximage->bits_per_pixel == 24) {
            /* Strange 3-byte format on some PC servers */
            if (ximage->blue_mask == 0x0000ff) {		/* 123 */
               Y_LOOP_START
                  X_PRELOOP
                  X_LOOP_START
                     GET_PIXELS

                     /* Byte order is RED GREEN BLUE */

                     *dest_ptr++ = dn_red;
                     *dest_ptr++ = dn_grn;
                     *dest_ptr++ = dn_blu;
                  X_LOOP_END
               Y_LOOP_END
            }
            else {						/* 321 */
               Y_LOOP_START
                  X_PRELOOP
                  X_LOOP_START
                     GET_PIXELS

                     /* Byte order is BLUE GREEN RED */

                     *dest_ptr++ = dn_blu;
                     *dest_ptr++ = dn_grn;
                     *dest_ptr++ = dn_red;
                  X_LOOP_END
               Y_LOOP_END
            }
         }
         else if (ximage->byte_order == MSBFirst) {
            if (ximage->blue_mask == 0x0000ff) {		/* 123 */
               Y_LOOP_START
                  X_PRELOOP
                  X_LOOP_START
                     GET_PIXELS

                     /* Byte order is NULL RED GREEN BLUE */

                     *dest_ptr++ = 0;
                     *dest_ptr++ = dn_red;
                     *dest_ptr++ = dn_grn;
                     *dest_ptr++ = dn_blu;
                  X_LOOP_END
               Y_LOOP_END
            }
            else {						/* 321 */
               Y_LOOP_START
                  X_PRELOOP
                  X_LOOP_START
                     GET_PIXELS

                     /* Byte order is NULL BLUE GREEN RED */

                     *dest_ptr++ = 0;
                     *dest_ptr++ = dn_blu;
                     *dest_ptr++ = dn_grn;
                     *dest_ptr++ = dn_red;
                  X_LOOP_END
               Y_LOOP_END
            }
         }
         else {				/* LSBfirst */
            if (ximage->blue_mask == 0x0000ff) {		/* 210 */
               Y_LOOP_START
                  X_PRELOOP
                  X_LOOP_START
                     GET_PIXELS

                     /* Byte order is BLUE GREEN RED NULL */

                     *dest_ptr++ = dn_blu;
                     *dest_ptr++ = dn_grn;
                     *dest_ptr++ = dn_red; 
                     *dest_ptr++ = 0;
                  X_LOOP_END
               Y_LOOP_END
            }
            else {					/* 012 */
               Y_LOOP_START
                  X_PRELOOP
                  X_LOOP_START
                     GET_PIXELS

                     /* Byte order is RED GREEN BLUE NULL */

                     *dest_ptr++ = dn_red;
                     *dest_ptr++ = dn_grn;
                     *dest_ptr++ = dn_blu;
                     *dest_ptr++ = 0;
                  X_LOOP_END
               Y_LOOP_END
            }
         }
         break;

      case TRANS_332:

         if (biw->bim.dither_mode == XvicORDERED) {
            Y_LOOP_START
               X_PRELOOP
               X_LOOP_START
                  GET_PIXELS

                  ORDERED_DITHER(dn_red, dn_red, 8, x_dpy, y_dpy);
                  ORDERED_DITHER(dn_grn, dn_grn, 8, x_dpy, y_dpy);
                  ORDERED_DITHER(dn_blu, dn_blu, 4, x_dpy, y_dpy);
                  *dest_ptr++ = (dn_red << 5) | (dn_grn<<2) | dn_blu;
               X_LOOP_END
            Y_LOOP_END
         }
         else {
            Y_LOOP_START
               X_PRELOOP
               X_LOOP_START
                  GET_PIXELS

                  *dest_ptr++ =(dn_red&0xe0) | ((dn_grn>>3)&0x1c) | (dn_blu>>6);
               X_LOOP_END
            Y_LOOP_END
         }
         break;

      case TRANS_232:

         if (biw->bim.dither_mode == XvicORDERED) {
            Y_LOOP_START
               X_PRELOOP
               X_LOOP_START
                  GET_PIXELS

                  ORDERED_DITHER(dn_red, dn_red, 4, x_dpy, y_dpy);
                  ORDERED_DITHER(dn_grn, dn_grn, 8, x_dpy, y_dpy);
                  ORDERED_DITHER(dn_blu, dn_blu, 4, x_dpy, y_dpy);
                  *dest_ptr++ = (dn_red << 5) | (dn_grn << 2) | dn_blu | 0x80;
               X_LOOP_END
            Y_LOOP_END
         }
         else {
            Y_LOOP_START
               X_PRELOOP
               X_LOOP_START
                  GET_PIXELS

                  *dest_ptr++ = (((dn_red>>1) & 0x60) | ((dn_grn>>3) & 0x1c) |
				(dn_blu>>6)) | 0x80;
               X_LOOP_END
            Y_LOOP_END
         }
         break;

      case TRANS_CMAP:

         Y_LOOP_START
            X_PRELOOP
            X_LOOP_START
               GET_PIXELS

               KAGELS_DITHER(*dest_ptr++, dn_red,dn_grn,dn_blu,x_dpy,y_dpy);
            X_LOOP_END
         Y_LOOP_END

         break;

      default:			/* shouldn't happen */
         XtAppErrorMsg(XtWidgetToApplicationContext((Widget)biw),
		"BadDNTrans", "XvicBasicImage", "XvicBasicImageWidgetError",
		"Internal error: Unknown DN_transform in _XvicCopyRawXimage()",
		(String *)NULL, (Cardinal *)NULL);
   }

