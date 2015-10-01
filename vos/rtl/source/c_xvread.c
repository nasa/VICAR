#include "xvmaininc.h"
#include "defines.h"
#include "rtlintproto.h"
#include "declares.h"
#include "externs.h"

/************************************************************************/
/* Common Code								*/
/************************************************************************/

int c_xvread(int unit, char *buffer)
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

#if RTL_USE_COMPRESSION
   /* If compressed, call the compression read program */
   if (COMPRESSED) {
      bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
      trans_buf = &bufstate->read_trans;
      status = v2_compress_read_rec(unit, bufstate, buffer, trans_buf);
      return status;
   }
#endif

   /* Check for the existence of SLICE2 and SLICE3 to figure out the record # */

   if ((*CURRENT_IP_VALUE(SLICE2) == 0) &&	/* neither, so incr image rec */
       (*CURRENT_IP_VALUE(SLICE3) == 0)) {
      rec = CURRENT_I_VALUE(IMG_REC) + 1;
   }
   else if (*CURRENT_IP_VALUE(SLICE3)==0) {	/* SLICE2 given, not SLICE3 */
      if (CURRENT_I_VALUE(N3) == 1)	/* If N3==1, SLICE3 can be defaulted */
         rec = *CURRENT_IP_VALUE(SLICE2);
      else
         return START_REC_ERROR;
   }
   else if (*CURRENT_IP_VALUE(SLICE2)==0) {	/* SLICE3 given, not SLICE2 */
      return START_REC_ERROR;
   }
   else {						/* Both given */
      rec = CURRENT_I_VALUE(N2) * (*CURRENT_IP_VALUE(SLICE3) - 1) +
	    *CURRENT_IP_VALUE(SLICE2);
   }

   /* Check for eof on labeled files (so we don't read EOL labels by mistake) */

   binary_header = 0;	/* usage is reversed below for convenience */
   if (BINARY_ACCESS)
      binary_header = CURRENT_I_VALUE(NLB);
   if (!(CURRENT_I_VALUE(FLAGS) & NO_LABELS) &&
	  rec > CURRENT_I_VALUE(N2) * CURRENT_I_VALUE(N3) + binary_header)
      return END_OF_FILE;

   nbb = CURRENT_I_VALUE(NBB);
   binary_prefix = 0;
   binary_header = 0;
   if (!BINARY_ACCESS) {
      binary_prefix = CURRENT_I_VALUE(NBB);
      binary_header = CURRENT_I_VALUE(NLB);
   }

   CURRENT_I_VALUE(IMG_REC) = rec;

   bufstate = (struct bufstate *)CURRENT_IP_VALUE(BUFSTATE);
   rec += (CURRENT_I_VALUE(LBLSIZE) / CURRENT_I_VALUE(RECSIZE))
		+ binary_header;
   user_pixsize = bufstate->read_trans.dpixsize;

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

   trans_buf = &bufstate->read_trans;
   if (rec <= CURRENT_I_VALUE(NLB) +
			(CURRENT_I_VALUE(LBLSIZE) / CURRENT_I_VALUE(RECSIZE)))
      trans_buf = &no_trans;	/* Don't translate anything in binary header */

   status = v2_read_rec(bufstate, buffer, rec-1, /* rec-1 makes it 0 based */
		        start, len, trans_buf);

   if (status != SUCCESS)
      return status;
   
#if VMS_OS
   /* get the variable length record size */
   CURRENT_I_VALUE(VARSIZE) = bufstate->devstate.transfer_count;
#endif

   return SUCCESS;
}
