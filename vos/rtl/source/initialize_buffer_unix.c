#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/* Initialize bufstate */

int v2_initialize_buffer(int unit)
{
   struct bufstate *bufstate;
   int nblocks;
   int big_tape;		/* TRUE iff recsize > max tape blocksize */

   big_tape = FALSE;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);

   bufstate->recsize = CURRENT_I_VALUE(RECSIZE);

   if (bufstate->devstate.device_type == DEV_TAPE) {
      if (bufstate->bufsize == 0) {
	 bufstate->bufsize = CURRENT_I_VALUE(BUFSIZE);

/* If we're reading, and the blocksize is known, use the max of the	*/
/* blocksize and the record size (made into a multiple of the blocksize)*/
/* as the buffer size instead of BUFSIZ from the label, since BUFSIZ	*/
/* from the label may not be a multiple of blocksize, which is required.*/
/* If VICAR did not write the tape, BUFSIZ is still a multiple of the	*/
/* disk block size, not the tape block size!				*/

	 if (bufstate->blocksize != 0 && !EQUAL(CURRENT_S_VALUE(OP), "WRITE")) {
	    bufstate->bufsize = MAX(bufstate->blocksize,bufstate->recsize);
	    nblocks = CEIL(bufstate->bufsize, bufstate->blocksize);
	    bufstate->bufsize = nblocks * bufstate->blocksize;
	 }
      }

/* Large tape records (>65534) need to be spanned because we can't have a */
/* single block that big.  So, we pick a smaller block size and then span */
/* the records just like we do on a normal disk file.  Since we are	  */
/* picking a block size to use, this won't work if the user specifies the */
/* blocksize on the mount, or if the caller has NOBLOCK selected.  It	  */
/* It turns out that state->blocksize is set for output files only if	  */
/* either of those conditions are true, so that's what we check here.	  */

      if (bufstate->recsize > MAX_TAPE_BLOCKSIZE) {
	 big_tape = TRUE;	/* big record, so we need to span them */
	 if ((bufstate->blocksize != 0) && (EQUAL(CURRENT_S_VALUE(OP),"WRITE")))
	    return BUF_NOT_MULT_OF_REC;
	 bufstate->blocksize = AVG_TAPE_BLOCKSIZE;
	 bufstate->devstate.dev.tape.blocksize = bufstate->blocksize;  /* yuck*/

	 bufstate->bufsize = bufstate->recsize;
	 if (bufstate->blocksize == 0)	/* out tapes STILL don't know blksize */
	    bufstate->blocksize = bufstate->bufsize;
	 nblocks = CEIL(bufstate->bufsize, bufstate->blocksize);
	 if (CEIL(bufstate->recsize, bufstate->blocksize) == nblocks)
	    nblocks++;		/* nblocks must be >= ceil(rec,block) + 1 */
	 bufstate->bufsize = nblocks * bufstate->blocksize;
      }

      if (!big_tape) {
	 /* Don't allow spanning! */
	 if (bufstate->bufsize <= 0)
	    bufstate->bufsize = CEIL(APPROX_BUFFER_SIZE,bufstate->recsize) *
				bufstate->recsize;

	 if (bufstate->blocksize == 0) {
	    bufstate->blocksize = bufstate->bufsize;
	    bufstate->devstate.dev.tape.blocksize = bufstate->bufsize; /* yuck*/
	 }

/* We don't check if bufsize is a multiple of recsize if we're reading	*/
/* so that we have a better chance of reading a strange format if it	*/
/* comes along.								*/

	 if (EQUAL(CURRENT_S_VALUE(OP), "WRITE") &&
		bufstate->bufsize % bufstate->recsize != 0)
	    return BUF_NOT_MULT_OF_REC;
      }
   }

/* For tapes, this is only executed if we have >64K recsize on output tapes */

   if (bufstate->bufsize <= 0) {
      if (EQUAL(CURRENT_S_VALUE(METHOD), "RANDOM"))
	 bufstate->bufsize = MAX(RANDOM_BUFFER_SIZE, bufstate->recsize);
      else
	 bufstate->bufsize = MAX(APPROX_BUFFER_SIZE, bufstate->recsize);
      if (bufstate->blocksize == 0) /* output tapes STILL don't know blocksize*/
	 bufstate->blocksize = bufstate->bufsize;
      nblocks = CEIL(bufstate->bufsize, bufstate->blocksize);
      if (CEIL(bufstate->recsize, bufstate->blocksize) == nblocks)
	 nblocks++;		/* nblocks must be >= ceil(rec,block) + 1 */
      bufstate->bufsize = nblocks * bufstate->blocksize;
   }

   CURRENT_I_VALUE(BUFSIZE) = bufstate->bufsize;

   /* Array I/O has already set nblocksinbuf */
   if (bufstate->nblocksinbuf == 0)
      bufstate->nblocksinbuf = bufstate->bufsize / bufstate->blocksize;

   if (EQUAL(CURRENT_S_VALUE(METHOD), "SEQ"))
      bufstate->flags |= BUF_SEQ;

   if (SEQ_DEVICE)
      bufstate->flags |= BUF_SEQ;

   if (bufstate->devstate.device_type == DEV_TAPE) {	/* yuck */
      if (!big_tape)
	 bufstate->flags |= BUF_ALIGN;
   }

   if ((bufstate->recsize % bufstate->bufsize == 0) ||
       (bufstate->bufsize % bufstate->recsize == 0))
      bufstate->flags |= BUF_ALIGN;

   if (CURRENT_I_VALUE(FLAGS) & VARREC) {
      bufstate->flags |= BUF_VARREC;
      bufstate->bufsize = 65535;	/* big enough to hold any size record */
      if (bufstate->devstate.device_type != DEV_TAPE)
	 return VARREC_ERROR;		/* only allowed on tapes */
   }

/* If you want caching, you should get an event flag for read-only */
/* files (see the open routines for each device type).		   */
/*!!!!
   if (EQUAL(CURRENT_S_VALUE(METHOD), "SEQ"))
      bufstate->flags |= BUF_CACHE;
!!!!*/

   if (SEQ_DEVICE && bufstate->recsize != bufstate->bufsize) /* since seq devs*/
      bufstate->flags &= ~BUF_CACHE;	       /* only read 1 block at a time */

   if (bufstate->eof_record > 0)	     /* for disk files eof_rec is in terms of */
      bufstate->eof_record /= bufstate->recsize;	   /* bytes until now */

   if (bufstate->buffer == NULL) { /* mem or array file already set up buffer */

      bufstate->buffer = malloc((int)(bufstate->bufsize + bufstate->buf_extra));
      if (bufstate->buffer == NULL)
	 return INSUFFICIENT_MEMORY;
   }
   else {			/* mem or array file... set up eof_record */
      bufstate->eof_record = bufstate->bufsize / bufstate->recsize;
   }

   v2_update_bufstate(bufstate, (V2_OFFSET)0);

   return SUCCESS;
}
