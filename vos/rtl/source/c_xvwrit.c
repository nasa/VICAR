#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/************************************************************************/
/* Common Code								*/
/************************************************************************/

int c_xvwrit(int unit, char *buffer)
{
   int status;
   int rec;
   int binary_prefix, binary_header;
   int nbb;
   int user_pixsize;
   int start, len;
   int ss, ssbin, ssnobin, ns, nsbin, nsnobin;
   struct bufstate *bufstate;
   struct trans *trans_buf;

   /* If it's a seq device and labels have been modified, write out the	*/
   /* labels before the first data write.				*/
   if (SEQ_DEVICE) {
      if (CURRENT_I_VALUE(FLAGS) & LABELS_MODIFIED) {
         status = v2_write_seq_labels(unit);
         if (status != SUCCESS)
            return status;
         CURRENT_I_VALUE(FLAGS) &= ~LABELS_MODIFIED;
      }
   }

#if RTL_USE_PROTO
   /* If compressed, call the compression write program. */
   if (COMPRESSED) {
      bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
      trans_buf = &bufstate->write_trans;
      status = v2_compress_writ_rec(unit, bufstate, buffer, trans_buf);

      return status;
   }
#endif

   /* Check for the existence of SLICE2 and SLICE3 to figure out the record # */

   if ((*CURRENT_IP_VALUE(SLICE2) == 0) &&	/* neither, so incr image rec */
       (*CURRENT_IP_VALUE(SLICE3) == 0)) {
      if (EQUAL(CURRENT_S_VALUE(OP), "UPDATE"))	/* unless it's UPDATE mode */
         rec = CURRENT_I_VALUE(IMG_REC);	/* Rewrite last record     */
      else
         rec = CURRENT_I_VALUE(IMG_REC) + 1;
   }
   else if (*CURRENT_IP_VALUE(SLICE3)==0) {	/* SLICE2 given, not SLICE3 */
      if (CURRENT_I_VALUE(N3) == 1)	/* If N3==1, SLICE3 can be defaulted. */
         rec = *CURRENT_IP_VALUE(SLICE2);
      else
         return START_REC_ERROR;
   }
   else if (*CURRENT_IP_VALUE(SLICE2)==0) {	/* SLICE3 given, not SLICE2 */
      return START_REC_ERROR;
   }
   else {					/* Both given */
      rec = CURRENT_I_VALUE(N2) * (*CURRENT_IP_VALUE(SLICE3) - 1) +
	    *CURRENT_IP_VALUE(SLICE2);
   }

   /* Check for eof on labeled files (so we don't clobber EOL labels) */

   binary_header = 0;	/* usage is reversed below for convenience */
   if (BINARY_ACCESS)
      binary_header = CURRENT_I_VALUE(NLB);
   if (EQUAL(CURRENT_S_VALUE(OP), "UPDATE") &&
	!(CURRENT_I_VALUE(FLAGS) & NO_LABELS) &&
	(rec > CURRENT_I_VALUE(N2) * CURRENT_I_VALUE(N3) + binary_header))
      return END_OF_FILE;

   nbb = CURRENT_I_VALUE(NBB);
   binary_prefix = 0;
   binary_header = 0;
   if (!BINARY_ACCESS) {
      binary_prefix = CURRENT_I_VALUE(NBB);
      binary_header = CURRENT_I_VALUE(NLB);
   }

   if (SEQ_DEVICE) {			/* can't back up on seq dev */
      if (rec != CURRENT_I_VALUE(IMG_REC)+1)	/* tried to back up */
         return NON_SEQUENTIAL_WRITE;
   }

   CURRENT_I_VALUE(IMG_REC) = rec;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
   rec += (CURRENT_I_VALUE(LBLSIZE) / CURRENT_I_VALUE(RECSIZE))
		+ binary_header;
   user_pixsize = bufstate->write_trans.spixsize;

/* Calculate starting sample position as a byte offset into the record.	*/
/* Pixels in binary header are always one byte long, while pixels in	*/
/* the file are user_pixsize bytes long.				*/

   ss = (*CURRENT_IP_VALUE(SLICE1))-1 + binary_prefix;	/* sample number */
   ssbin = MIN(nbb,ss);			/* part that's in binary header */
   ssnobin = MAX((ss-ssbin), 0);	/* part that's in normal samples */
   start = ssbin + ssnobin*user_pixsize;
   if (*CURRENT_IP_VALUE(SLICE1) == 0) {
	start = binary_prefix;
	ss = binary_prefix;
   }

/* Calculate length in bytes of transfer.  Same pixel sizes as above.	*/

   ns = *CURRENT_IP_VALUE(NSLICE1);

   nsbin = MAX((MIN(nbb, ss+ns) - ss), 0);	/* length in binary header */
   nsnobin = MAX((ns-nsbin), 0);		/* length in normal samples */
   len = nsbin + nsnobin*user_pixsize;

   if (ns == 0) {
      len = CURRENT_I_VALUE(N1) * user_pixsize;
      if (BINARY_ACCESS)
	 len += nbb;
   }

/* can't write partial recs on seq dev (unless it's var len records) */

   if (SEQ_DEVICE && !(CURRENT_I_VALUE(FLAGS) & VARREC)) {
      if (len != CURRENT_I_VALUE(RECSIZE))	/* tried to write partial rec */
         return NON_SEQUENTIAL_WRITE;
   }

   trans_buf = &bufstate->write_trans;
   if (rec <= CURRENT_I_VALUE(NLB) +
			(CURRENT_I_VALUE(LBLSIZE) / CURRENT_I_VALUE(RECSIZE)))
      trans_buf = &no_trans;	/* Don't translate anything in binary header */

   status = v2_write_rec(bufstate, buffer, rec-1, /* rec-1 makes it 0 based */
		      start, len, trans_buf);

   CURRENT_I_VALUE(FLAGS) |= DATA_WRITTEN;

   return status;
}
