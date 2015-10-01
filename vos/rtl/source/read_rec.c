#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Read a record from a file */

int v2_read_rec(
   struct bufstate *state,	/* buffer state for the file */
   char *buffer,		/* The buffer to read into */
   int rec,			/* Record number to read */
   int startbyte,		/* Starting byte within the record */
   int nbytes,			/* # of bytes to read, or 0 if whole rec */
   struct trans *trans		/* Format translation control struct */
)

{
   int status;
   int bufloc;
   int first_part_read, last_part_read;
   V2_OFFSET location;
   int pplen;
   char partpix[MAX_PIXEL_SIZE];
#if VMS_OS
   int len;
#endif
   
   status = v2_io_complete_check(&state->devstate);
   if (status != SUCCESS)
      return status;

   if (nbytes == 0)
      nbytes = state->recsize;

   pplen = 0;			/* no partial pixels read yet */

   if (state->flags & BUF_VARREC) {	/* read a variable length record */
      status = v2_io_complete_check(&state->devstate); /* wait for prev writes*/
      if (status != SUCCESS)
	 return status;
#if VMS_OS
      state->devstate.dev.tape.blocksize=state->bufsize; /*varrec on tape only*/
#endif
      status = v2_read_blocks(&state->devstate, state->buffer, (V2_OFFSET)rec,
						(V2_OFFSET)1, FALSE);
      if (status != SUCCESS)
	 return status;
#if VMS_OS
      len = state->devstate.transfer_count;
      status = v2_movetrans(state->buffer, 0, len,
			   buffer, startbyte, nbytes,
			   trans, &pplen, partpix);
#endif
      return status;
   }

/* We can't check for EOF here since old VICAR image files had the EOF mark */
/* set improperly (after the image, before the EOL labels).  :-(	    */
/* if (state->eof_record >= 0 && rec >= state->eof_record)		    */
/*    return END_OF_FILE;			check for EOF if known      */

   first_part_read = FALSE;
   last_part_read = FALSE;

   if (state->flags & BUF_VALID) {	/* the buffer contains some data */
      if (rec >= state->first_complete_rec &&
         rec <= state->last_complete_rec) {	/* entire record is in buf */

         bufloc = state->rec_offset +
                  (rec - state->first_complete_rec)*state->recsize;
         status = v2_movetrans(state->buffer+bufloc, 0, state->recsize,
			    buffer, startbyte, nbytes,
			    trans, &pplen, partpix);
	 if (status != SUCCESS)
	    return status;

         if (state->flags & BUF_CACHE) {
            status = v2_read_cache(state, rec);
	    if (status != SUCCESS)
	       return status;
	 }
         return SUCCESS;
      }

 /* check for partial records in buffer... only if sequential & non-aligned */

      if ((state->flags&BUF_SEQ) && !(state->flags&BUF_ALIGN)) {

         if (rec == state->first_rec) {		/* read last part of record */
	    status = v2_movetrans(
		      state->buffer, state->first_rec_pos, state->rec_offset,
		      buffer, startbyte, nbytes,
		      trans, &pplen, partpix);
	    if (status != SUCCESS)
	       return status;
            last_part_read = TRUE;
         }
         else if (rec == state->last_rec) {	/* read first part of record */
	    status = v2_movetrans(
		      state->buffer+state->last_rec_pos, 0, state->last_rec_len,
		      buffer, startbyte, nbytes,
		      trans, &pplen, partpix);
	    if (status != SUCCESS)
	       return status;
            first_part_read = TRUE;
	 }
      }
   }			/* end valid */

   if (state->flags & BUF_DIRTY) {
      status = v2_flush_buffer(state);
      if (status != SUCCESS)
	 return status;
   }

   if (first_part_read)
      location = state->blockno + state->nblocksinbuf;
   else if (last_part_read)
      location = state->blockno - state->nblocksinbuf;
   else						/* no partial reads done */
      location = (rec * (V2_OFFSET)state->recsize) / state->blocksize;

/* This is to handle old tapes, where blocksize is not necessarily a multiple */
/* of recsize (there's wasted space at the end of a block).  (bs/rs) below    */
/* determines the # of complete records that fit in a buffer.		      */

   if (state->flags & BUF_ALIGN) {	 /* Align buffer to buffer boundaries */
      location = (rec / (state->bufsize / state->recsize))
		  * (V2_OFFSET)state->nblocksinbuf;
   }
   if (location < 0)
      location = 0;	/* could happen for last_part_read case above */

   status = v2_io_complete_check(&state->devstate); /*wait for flush to finish*/
   if (status != SUCCESS)
      return status;

   status = v2_read_blocks(&state->devstate, state->buffer, location,
		        (V2_OFFSET)state->nblocksinbuf, FALSE);	/* sync read */
   v2_update_bufstate(state, location);
   state->flags |= BUF_VALID;

   if (status != SUCCESS)
      return status;

/* Copy the rest of the record (possibly the whole record) to the user's buf */

   if (first_part_read)		/* rest of record now at front of buf */
      status = v2_movetrans(state->buffer, state->first_rec_pos,
			 state->rec_offset,
		         buffer, startbyte, nbytes,
		         trans, &pplen, partpix);

/* When we figure out the location to read, for this case there's a good     */
/* possibility that the desired location for reading went negative, in which */
/* case it gets set back to 0.  However, this causes the partial record to   */
/* not be at the back of the buffer any more, but somewhere in the middle.   */

   else if (last_part_read)	/* rest of record now at back of buf */
      if (rec >= state->first_complete_rec &&
         rec <= state->last_complete_rec) {	/* entire record is in buf */

         bufloc = state->rec_offset +
                  (rec - state->first_complete_rec)*state->recsize;
         status = v2_movetrans(state->buffer+bufloc, 0, state->recsize,
			    buffer, startbyte, nbytes,
			    trans, &pplen, partpix);
      }
      else {
	 status = v2_movetrans(
		      state->buffer+state->last_rec_pos, 0, state->last_rec_len,
		      buffer, startbyte, nbytes,
		      trans, &pplen, partpix);
      }
   else {				/* entire record now in buf */
      bufloc = state->rec_offset +
	       (rec - state->first_complete_rec)*state->recsize;
      status = v2_movetrans(state->buffer+bufloc, 0, state->recsize,
		         buffer, startbyte, nbytes,
		         trans, &pplen, partpix);
   }
   if (status != SUCCESS)
      return status;

   if (state->flags & BUF_CACHE) {
      status = v2_read_cache(state, rec);
      if (status != SUCCESS)
	 return status;
   }

   return SUCCESS;
}
