#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* This routine copies part of a record from the source to the destination  */
/* buffer, translating data formats as it goes.  The source and dest both   */
/* define a partial record, starting at byte 'pos' with 'len' bytes.  For   */
/* the user's buffer, this is normally the slice1 and nslice1 converted to  */
/* bytes.  For the system buffer, it is the portion of the record contained */
/* in the buffer.  This routine copies the portion of the record that is in */
/* both the source and destination buffer.  It basically works as follows:  */
/* First, the passed-in coordinates (bytes) are converted to pixels.  The   */
/* overlapping area is determined, then the start of the overlapping area   */
/* is converted back into bytes and the area is translated and copied from  */
/* the source to the destination.					    */
/* Complications arise because pixels can cross buffer boundaries.  This is */
/* handled by partpix and pplen.  First, it checks to see if any partial    */
/* pixels are needed for the transfer.  If *pplen == 0, then there have     */
/* been no previous partial pixels, so the buffer 'partpix' is filled and   */
/* parts of it used.  If *pplen != 0, then part of the pixel has already    */
/* been used, so the rest of it is retrieved from partpix.  Partpix can be  */
/* used in two different ways.  If the source contains the partial pixel,   */
/* partpix holds the partial pixel the first time, then gets the rest of    */
/* the pixel the second time and translation is performed.  If the dest has */
/* the partial pixel, partpix holds the entire translated pixel, and part   */
/* of it is used each time.  *pplen also shows which part has been used and */
/* which has not.							    */
/* Trans->notransamt is the number of bytes at the beginning of the record  */
/* that are not to be translated.  These are copied before translation is   */
/* started.								    */

int v2_movetrans(
   char *sbuf,		/* source */
   int spos,		/* position in record of start of buffer in bytes*/
   int slen,		/* length of part of record that is in buffer in bytes*/
   char *dbuf,		/* destination */
   int dpos,
   int dlen,
   struct trans *trans,	/* translation control structure */
   int *pplen,		/* # of bytes in partpix */
   char *partpix	/* holds a partial pixel */
)

{
   int sppos, splen;	/* spos, slen in pixels */
   int dppos, dplen;	/* dpos, dlen in pixels */
   int start, end, len;	/* of whole-pixel transfer, in pixels */
   int sstart, dstart;	/* of whole-pixel transfer, in bytes */
   int slength, dlength; /* of whole-pixel transfer, in bytes */
   int extrasst, extrasend, extradst, extradend; /* partial pixel byte counts */
   int nextra;		/* # of partial pixel problems... cannot be > 1 */
   int status;

/* If no translation, take the easy way out... do it in terms of bytes */

   if (trans->transfn == NULL) {

      start = MAX(spos, dpos);		/* starting byte of overlap*/
      end = MIN(spos+slen, dpos+dlen);	/* ending byte+1 of overlap */
      len = end - start;

      if (len <= 0)
	 return SUCCESS;

      sstart = start - spos;
      dstart = start - dpos;
      v2_move(dbuf+dstart, sbuf+sstart, len);

      return SUCCESS;
   }

/* Translation required, so do it the hard way */

/* First check for untranslated portions */

   if (spos < trans->notransamt && dpos < trans->notransamt) {	/* got some */

      start = MAX(spos, dpos);			/* starting byte of overlap*/
      end = MIN(trans->notransamt, MIN(spos+slen, dpos+dlen)); /* end byte+1 */
      len = end - start;

      if (len > 0) {
	 sstart = start - spos;
	 dstart = start - dpos;
	 v2_move(dbuf+dstart, sbuf+sstart, len);
      }
   }

/* Convert from byte to pixel offsets and lengths, ignoring partials for now */

   sppos = CEIL(MAX((spos-trans->notransamt),0), trans->spixsize);
   splen = (slen - MAX(trans->notransamt-spos,0)) / trans->spixsize;
   dppos = CEIL(MAX((dpos-trans->notransamt),0), trans->dpixsize);
   dplen = (dlen - MAX(trans->notransamt-dpos,0)) / trans->dpixsize;

/* Compute overlap */

   start = MAX(sppos, dppos);		/* starting pixel of overlap*/
   end = MIN(sppos+splen, dppos+dplen);	/* ending pixel+1 of overlap */
   len = end - start;

   sstart = start * trans->spixsize + trans->notransamt - spos;
   dstart = start * trans->dpixsize + trans->notransamt - dpos;
   slength = len * trans->spixsize;
   dlength = len * trans->dpixsize;

   if (len < 0)		/* no overlap */
      return SUCCESS;	/* len == 0 means only (maybe) part pixel overlap */

   if (len > 0) {				/* Do full-pixel translate */
      status = (*trans->transfn)(sbuf+sstart, dbuf+dstart, len, trans);
      if (status != SUCCESS)
	 return status;
   }

/* Check for partial pixels */

   nextra = 0;
   extrasst = MAX(spos-trans->notransamt,0) % trans->spixsize;
   if (extrasst != 0) {
      extrasst = trans->spixsize - extrasst;
      nextra++;
   }
   extradst = MAX(dpos-trans->notransamt,0) % trans->dpixsize;
   if (extradst != 0) {
      extradst = trans->dpixsize - extradst;
      nextra++;
   }
   extrasend=(slen - MAX(trans->notransamt-spos,0) -extrasst) % trans->spixsize;
   if (extrasend != 0)
      nextra++;
   extradend=(dlen - MAX(trans->notransamt-dpos,0) -extradst) % trans->dpixsize;
   if (extradend != 0)
       nextra++;

   if (nextra == 0)			/* No partial pixels */
      return SUCCESS;
   if (nextra > 1)
      return INTERNAL_ERROR;		/* Shouldn't happen! */

/* Handle partial pixel translations */

/* Partial at beginning of source (end of the pixel) and it fits in dest */

   if (extrasst != 0 && dstart - trans->dpixsize >= 0) {
      v2_move(partpix+trans->spixsize-extrasst, sbuf+sstart-extrasst, extrasst);
      if (*pplen == 0) {			/* first part */
	 *pplen = extrasst;
      }
      else {
	 if (*pplen + extrasst != trans->spixsize)
	    return INTERNAL_ERROR;		/* whoops! */
	 status=(*trans->transfn)(partpix, dbuf+dstart-trans->dpixsize,1,trans);
	 if (status != SUCCESS)
	    return status;
      }
   }

/* Partial at end of source (first part of the pixel) and it fits in dest */

   if (extrasend != 0 && dstart + dlength + trans->dpixsize < dlen) {
      v2_move(partpix, sbuf+sstart+slength, extrasend);
      if (*pplen == 0) {			/* first part */
	 *pplen = extrasend;
      }
      else {
	 if (*pplen + extrasend != trans->spixsize)
	    return INTERNAL_ERROR;		/* whoops! */
	 status = (*trans->transfn)(partpix, dbuf+dstart+dlength, 1, trans);
	 if (status != SUCCESS)
	    return status;
      }
   }

/* Partial at beginning of dest (end of the pixel) and source is valid */

   if (extradst != 0 && sstart - trans->spixsize >= 0) {
      if (*pplen == 0) {			/* first part */
	 *pplen = extradst;
	 status=(*trans->transfn)(sbuf+sstart-trans->spixsize, partpix, 1,trans);
	 if (status != SUCCESS)
	    return status;
      }
      else {				/* second part */
	 if (*pplen + extradst != trans->dpixsize)
	    return INTERNAL_ERROR;		/* whoops! */
      }
      v2_move(dbuf+dstart-extradst, partpix+trans->dpixsize-extradst, extradst);
   }

/* Partial at end of dest (first part of the pixel) and source is valid */

   if (extradend != 0) {
      if (*pplen == 0) {			/* first part */
	 *pplen = extradend;
	 status = (*trans->transfn)(sbuf+sstart+slength, partpix, 1, trans);
	 if (status != SUCCESS)
	    return status;
      }
      else {
	 if (*pplen + extradend != trans->dpixsize)
	    return INTERNAL_ERROR;		/* whoops! */
      }
      v2_move(dbuf+dstart+dlength, partpix, extradend);
   }

/* Whew! */

   return SUCCESS;
}
