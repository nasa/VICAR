#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Write a record out to a file */

int v2_write_rec(
   struct bufstate *state,	/* buffer state for the file */
   char *buffer,		/* The buffer to write */
   int rec,			/* Record number to write */
   int startbyte,		/* Starting byte within the record */
   int nbytes,			/* # of bytes to write, or 0 if whole rec */
   struct trans *trans		/* Format translation control struct */
)

{
   int status;
   int bufloc;
   int first_part_written, last_part_written;
   V2_OFFSET location;
   int pplen;
   char partpix[MAX_PIXEL_SIZE];

   status = v2_io_complete_check(&state->devstate);
   if (status != SUCCESS)
      return status;

   if (nbytes == 0)
      nbytes = state->recsize;

   if (state->flags & BUF_VARREC) {	/* write a variable length record */
      status = v2_movetrans(buffer, startbyte, nbytes,
			 state->buffer, 0, (int)state->bufsize,
			 trans, &pplen, partpix);
      if (status != SUCCESS)
	 return status;
#if VMS_OS
      state->devstate.dev.tape.blocksize=nbytes; /* varrec only legal on tapes*/
#endif
      status = v2_write_blocks(&state->devstate, state->buffer,
			(V2_OFFSET)rec, (V2_OFFSET)1, TRUE);
      if (status != SUCCESS)
	 return status;
      return status;
   }

   if (rec >= state->eof_record)
      state->eof_record = rec+1;

   first_part_written = FALSE;
   last_part_written = FALSE;

   pplen = 0;			/* no partial pixels written yet */

   if (state->flags & BUF_VALID) {	/* the buffer contains some data */
      if (rec >= state->first_complete_rec &&
         rec <= state->last_complete_rec) {	/* entire record is in buf */

         bufloc = state->rec_offset +
                  (rec - state->first_complete_rec)*state->recsize;
         status = v2_movetrans(buffer, startbyte, nbytes,
			    state->buffer+bufloc, 0, state->recsize,
			    trans, &pplen, partpix);
	 if (status != SUCCESS)
	    return status;

	 state->flags |= BUF_DIRTY;

	 if (rec == state->last_rec) {	/* we're exactly at end of buffer */
	    status = v2_flush_buffer(state);	/* so flush it */
	    if (status != SUCCESS)
	       return status;
	 }
	 return SUCCESS;
      }

 /* check for partial records in buffer... only if sequential & non-aligned */

      if ((state->flags&BUF_SEQ) && !(state->flags&BUF_ALIGN)) {

         if (rec == state->first_rec) {		/* write last part of record */
	    status = v2_movetrans(
		      buffer, startbyte, nbytes,
		      state->buffer, state->first_rec_pos, state->rec_offset,
		      trans, &pplen, partpix);
	    if (status != SUCCESS)
	       return status;
            last_part_written = TRUE;
	    state->flags |= BUF_DIRTY;
         }
         else if (rec == state->last_rec) {	/* write first part of record */
	    status = v2_movetrans(
		      buffer, startbyte, nbytes,
		      state->buffer+state->last_rec_pos, 0, state->last_rec_len,
		      trans, &pplen, partpix);
	    if (status != SUCCESS)
	       return status;
            first_part_written = TRUE;
	    state->flags |= BUF_DIRTY;
	 }
      }
   }			/* end valid */

   if (state->flags & BUF_DIRTY) {
      status = v2_flush_buffer(state);
      if (status != SUCCESS)
	 return status;
   }

   if (first_part_written)
      location = state->blockno + state->nblocksinbuf;
   else if (last_part_written)
      location = state->blockno - state->nblocksinbuf;
   else						/* no partial writes done */
      location = (rec * (V2_OFFSET)state->recsize) / state->blocksize;

/* This is to handle old tapes, where blocksize is not necessarily a multiple */
/* of recsize (there's wasted space at the end of a block).  (bs/rs) below    */
/* determines the # of complete records that fit in a buffer.		      */

   if (state->flags & BUF_ALIGN) {	 /* Align buffer to buffer boundaries */
      location = (rec / (state->bufsize / state->recsize))
		 * (V2_OFFSET)state->nblocksinbuf;
   }

   if (location < 0)
      location = 0;	/* could happen for last_part_written case above */

   v2_update_bufstate(state, location);

   status = v2_io_complete_check(&state->devstate); /*wait for flush to finish*/
   if (status != SUCCESS)
      return status;

/* Check to see if a preread of the buffer is necessary */

   if (!(state->flags & BUF_NOPREREAD) &&	/* device can't preread */
      ((startbyte != 0 || nbytes != state->recsize) ||	/* partial record */
       (rec < state->eof_record-1) ||			/* we backed up */
       (rec != state->first_rec))) {		/* rec not at start of buf */

      status = v2_read_blocks(&state->devstate, state->buffer, location,
		   (V2_OFFSET)state->nblocksinbuf, FALSE);	/* sync read */
      if (status != SUCCESS && status != END_OF_FILE)
	 return status;
   }

   state->flags |= BUF_VALID | BUF_DIRTY;

/* Copy the rest of the record (possibly the whole record) to the system buf */

   if (first_part_written)		/* rest of record now at front of buf */
      status = v2_movetrans(buffer, startbyte, nbytes,
		         state->buffer, state->first_rec_pos, state->rec_offset,
		         trans, &pplen, partpix);

/* When we figure out the location to write, for this case there's a good    */
/* possibility that the desired location for reading went negative, in which */
/* case it gets set back to 0.  However, this causes the partial record to   */
/* not be at the back of the buffer any more, but somewhere in the middle.   */

   else if (last_part_written)	/* rest of record now at back of buf */
      if (rec >= state->first_complete_rec &&
         rec <= state->last_complete_rec) {	/* entire record is in buf */

         bufloc = state->rec_offset +
                  (rec - state->first_complete_rec)*state->recsize;
         status = v2_movetrans(buffer, startbyte, nbytes,
			    state->buffer+bufloc, 0, state->recsize,
			    trans, &pplen, partpix);
      }
      else {
	 status = v2_movetrans(buffer, startbyte, nbytes,
		       state->buffer+state->last_rec_pos, 0,state->last_rec_len,
		       trans, &pplen, partpix);
      }
   else {				/* entire record now in buf */
      bufloc = state->rec_offset +
	       (rec - state->first_complete_rec)*state->recsize;
      status = v2_movetrans(buffer, startbyte, nbytes,
		         state->buffer+bufloc, 0, state->recsize,
		         trans, &pplen, partpix);
   }
   if (status != SUCCESS)
      return status;

   if (rec == state->last_rec) {	/* we're exactly at end of buffer */
      status = v2_flush_buffer(state);	/* so flush it */
      if (status != SUCCESS)
	 return status;
   }

   return SUCCESS;
}
