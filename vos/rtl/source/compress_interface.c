#include "xvmaininc.h"
#include "defines.h"
#include "declares.h"
#include "externs.h"
#include "ftnbridge.h"
#include "rtlintproto.h"

#if RTL_USE_COMPRESSION

/* This function calls the relevent preprocessing
   routine according to the compression type */
int v2_compress_preprocess(int unit)
{
   int status; 
   
   //   printf("opening unit: %d\n", unit);
   if(EQUAL(CURRENT_S_VALUE(COMPRESS), "BASIC") || EQUAL(CURRENT_S_VALUE(COMPRESS), "BASIC2"))
   {
      status = v2_basic_precheck(unit);
      if(status != SUCCESS) return status;

      status = v2_basic_preprocess(unit);
      if(status != SUCCESS) return status;
   }

   return SUCCESS;
}

/* This function calls the relevent closing
   routine according to the compression type */
int v2_compress_close(int unit)
{
   int status; 
   
   //   printf("closing unit: %d\n", unit);
   if(EQUAL(CURRENT_S_VALUE(COMPRESS), "BASIC") || EQUAL(CURRENT_S_VALUE(COMPRESS), "BASIC2"))
   {
      status = v2_basic_close(unit);
      if(status != SUCCESS) return status;
   }

   return SUCCESS;
}

/* This function calls the relevent reading
   routine according to the compression type
   and then performs translation on decoded data*/
int v2_compress_read_rec(
   int unit,
   struct bufstate *state,		/* buffer state for the file */
   char *buffer,			/* The buffer to read into */
   struct trans *trans			/* Format translation control struct */
)

{
   int status; 
   int spos, slen, dpos, dlen, pplen, partpix;
   int ns, nsamps, samp, spixsize, dpixsize;
   
   if(EQUAL(CURRENT_S_VALUE(COMPRESS), "BASIC"))
      status = v2_basic_read_rec(unit, state);
   else if(EQUAL(CURRENT_S_VALUE(COMPRESS), "BASIC2"))
      status = v2_basic2_read_rec(unit, state);
   else
      return INVALID_COMPRESSION_TYPE;

   samp = CURRENT_I_VALUE(SAMP);
   nsamps = CURRENT_I_VALUE(NSAMPS);
   ns = CURRENT_I_VALUE(NS);
   spixsize = trans->spixsize;
   dpixsize = trans->dpixsize;

   spos = 0;
   slen = ns*spixsize;

   /* calculate destination start position */
   if(samp == 0)
      dpos = 0;
   else 
      dpos = (samp-1)*dpixsize;

   /* calculate destination length */
   if(nsamps == 0)
      dlen = ns * dpixsize;
   else
      dlen = nsamps * dpixsize;

   v2_movetrans( (char*)(CURRENT_IP_VALUE(DECODED_BUF)), spos, slen, buffer,
                dpos, dlen, trans, &pplen, (char*) &partpix);

   return status;
}

/* This function performs translation on decoded data
   and then calls the relevent writing 
   routine according to the compression type*/
int v2_compress_writ_rec(int unit, struct bufstate *state, char *buffer,
			struct trans *trans)
{
   int status;
   int spos, slen, dpos, dlen, pplen, partpix;
   int samp, ns, nsamps, spixsize, dpixsize;
   pplen = 0;

   samp = CURRENT_I_VALUE(SAMP);
   nsamps = CURRENT_I_VALUE(NSAMPS);
   ns = CURRENT_I_VALUE(NS);
   spixsize = trans->spixsize;
   dpixsize = trans->dpixsize;

   spos = samp*spixsize;
   dpos = samp*dpixsize;

   if(nsamps == 0)
   {
      slen = ns * spixsize;
      dlen = ns * dpixsize;
   }
   else
   {
      slen = nsamps * spixsize;
      dlen = nsamps * dpixsize;
   }

   v2_movetrans(buffer, spos, slen, (char*)(CURRENT_IP_VALUE(DECODED_BUF)),
                dpos, dlen, trans, &pplen, (char *) &partpix);

   if(EQUAL(CURRENT_S_VALUE(COMPRESS), "BASIC"))
   {
      status = v2_basic_writ_rec(unit, state);
      return status;
   }
   else if(EQUAL(CURRENT_S_VALUE(COMPRESS), "BASIC2"))
   {
      status = v2_basic2_writ_rec(unit, state);
      return status;
   }

   return INVALID_COMPRESSION_TYPE;
}

/* This function calls the relevent get_eol_size
   routine according to the compression type */
int v2_compress_get_eol_size(int unit)
{
   int status;

   if(EQUAL(CURRENT_S_VALUE(COMPRESS), "BASIC") || EQUAL(CURRENT_S_VALUE(COMPRESS), "BASIC2"))
   {
      status = v2_basic_get_eol_size(unit);
      return status;
   }

   return INVALID_COMPRESSION_TYPE;
}

/* This function calls the relevent routine
   to acquire the eol label according to the
   compression type                           */
int v2_compress_read_in_eol(int unit, char* q, int eol_size)
{
   int status;

   if(EQUAL(CURRENT_S_VALUE(COMPRESS), "BASIC") || EQUAL(CURRENT_S_VALUE(COMPRESS), "BASIC2"))
   {
      status = v2_basic_read_in_eol(unit, q, eol_size);
      return status;
   }

   return INVALID_COMPRESSION_TYPE;
}

#else

int v2_compress_preprocess(int UNUSED(unit))
{
   return INVALID_COMPRESSION_TYPE;
}

int v2_compress_close(int UNUSED(unit))
{
   return INVALID_COMPRESSION_TYPE;
}

int v2_compress_read_rec(int UNUSED(unit), struct bufstate * UNUSED(state),
		char * UNUSED(buffer), struct trans * UNUSED(trans))
{
   return INVALID_COMPRESSION_TYPE;
}

int v2_compress_writ_rec(int UNUSED(unit), struct bufstate * UNUSED(state),
		char * UNUSED(buffer), struct trans * UNUSED(trans))
{
   return INVALID_COMPRESSION_TYPE;
}

int v2_compress_get_eol_size(int UNUSED(unit))
{
   return INVALID_COMPRESSION_TYPE;
}

int v2_compress_read_in_eol(int UNUSED(unit), char* UNUSED(q),
			int UNUSED(eol_size))
{
   return INVALID_COMPRESSION_TYPE;
}

#endif

