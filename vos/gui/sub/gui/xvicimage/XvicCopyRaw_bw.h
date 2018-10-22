/************************************************************************/
/* XvicCopyRaw_bw.h							*/
/* This code, called by XvicCopyRawFn_1band.h, creates the actual	*/
/* processing loops based on how the data is being displayed for bw	*/
/* data.  This is the first time the type of X display comes into play.	*/
/************************************************************************/

   switch (biw->bim.DN_transform) {

      case TRANS_FULLCOLOR:

         if (ximage->bits_per_pixel == 24) {
            /* Strange 3-byte format on some PC servers */
            Y_LOOP_START
               X_PRELOOP
               X_LOOP_START
                  GET_PIXELS

                  /* Byte order is RED GREEN BLUE or BLUE GREEN RED */

                  *dest_ptr++ = dn;
                  *dest_ptr++ = dn;
                  *dest_ptr++ = dn;
               X_LOOP_END
            Y_LOOP_END
         }
         else if (ximage->byte_order == MSBFirst) {	/* 123 or 321 */
            Y_LOOP_START
               X_PRELOOP
               X_LOOP_START
                  GET_PIXELS

                  /* Byte order is NULL RED GREEN BLUE or NULL BLUE GREEN RED */

                  *dest_ptr++ = 0;
                  *dest_ptr++ = dn;
                  *dest_ptr++ = dn;
                  *dest_ptr++ = dn;
               X_LOOP_END
            Y_LOOP_END
         }
         else {					/* LSBfirst - 210 or 012 */
            Y_LOOP_START
               X_PRELOOP
               X_LOOP_START
                  GET_PIXELS

                  /* Byte order is RED GREEN BLUE NULL or BLUE GREEN RED NULL */

                  *dest_ptr++ = dn;
                  *dest_ptr++ = dn;
                  *dest_ptr++ = dn;
                  *dest_ptr++ = 0;
               X_LOOP_END
            Y_LOOP_END
         }
         break;

      case TRANS_DIRECT:

         Y_LOOP_START
            X_PRELOOP
            X_LOOP_START
               GET_PIXELS

               *dest_ptr++ = dn;
            X_LOOP_END
         Y_LOOP_END

         break;

      case TRANS_HALF:

         Y_LOOP_START
            X_PRELOOP
            X_LOOP_START
               GET_PIXELS

               *dest_ptr++ = (dn>>1) | 0x80;
            X_LOOP_END
         Y_LOOP_END

         break;

      case TRANS_CMAP:

         if (biw->bim.dither_mode == XvicKAGELS) {
            Y_LOOP_START
               X_PRELOOP
               X_LOOP_START
                  GET_PIXELS

                  KAGELS_DITHER(*dest_ptr++, dn, dn, dn, x_dpy, y_dpy);
               X_LOOP_END
            Y_LOOP_END
         }
         else if (biw->bim.dither_mode == XvicORDERED) {
            Y_LOOP_START
               X_PRELOOP
               X_LOOP_START
                  GET_PIXELS

                  ORDERED_DITHER(dn,dn,biw->bim.gray_levels,x_dpy,y_dpy);
                  *dest_ptr++=biw->bim.cmap_gray[dn];
               X_LOOP_END
            Y_LOOP_END
         }
         else {				/* XvicNONE */
            Y_LOOP_START
               X_PRELOOP
               X_LOOP_START
                  GET_PIXELS

                  *dest_ptr++ =
			biw->bim.cmap_gray[((int)dn*biw->bim.gray_levels)/256];
               X_LOOP_END
            Y_LOOP_END
         }
         break;

      default:			/* shouldn't happen */
         XtAppErrorMsg(XtWidgetToApplicationContext((Widget)biw),
		"BadDNTrans", "XvicBasicImage", "XvicBasicImageWidgetError",
		"Internal error: Unknown DN_transform in _XvicCopyRawXimage()",
		(String *)NULL, (Cardinal *)NULL);
   }

