/*****************************************************************************/
/* Subroutines which read/write the GLL EDR/UDR binary bad data value header */
/* M. O'Shaughnessy, R. Deen (telemetry header code )                        */
/*                                                                           */
/* Contents:                                                                 */
/*                                                                           */
/* Routines to read and write the binary bad-data value header:              */
/*       get_ssi_bdv_hdr_rec (unit,line,dest)                                */
/*       write_ssi_bdv_hdr_rec (ounit,line,source)                           */
/*     The "line" parameter is absolute line; line=1 is the first record in  */
/*     the file. See "UDR_THDR_RECS" and "EDR_THDR_RECS" in BDVH.H for more  */
/*     information.                                                          */
/*                                                                           */
/* Routines to read and write an EDR image line:                             */
/*       get_gll_edr_ln (unit,line,dest)                                     */
/*       write_gll_edr_ln (ounit,line,source)                                */
/*                                                                           */
/* Routines to read and write a UDR image line:                              */
/*       get_gll_udr_ln (unit,line,dest)                                     */
/*       write_gll_udr_ln (ounit,line,source)                                */
/*                                                                           */
/* Routines to read and write the binary telemetry header:                   */
/*       get_ssi_telem_hdr (unit,dest)                                       */
/*       write_ssi_telem_hdr (ounit,source)                                  */
/*                                                                           */
/* Routine to write a binary image line prefix:                              */
/*       write_ssi_prefix(ounit,line,source)                                 */
/*                                                                           */
/* In *ALL* the above subroutines, the unit or ounit most be that of a file  */
/* which has already been opened with "COND","BINARY".  Host information     */
/* should also be set in the zvopen() call; see gllfillin.com for examples.  */
/*                                                                           */
/* Internal routines:                                                        */
/*    The trin* and trout* routines translate specific gll datatypes.        */
/*       trin_scet_typ(from,to)                                              */
/*       trin_ert_typ(from,to)                                               */
/*       trin_sclk_typ(from,to)                                              */
/*       trin_ssi_lrs_p_typ(from,to)                                         */
/*       trin_ssi_flg_typ(from,to)                                           */
/*       trin_j2000_ascii_typ(from,to)                                       */
/*                                                                           */
/*       trout_scet_typ(from,to)                                             */
/*       trout_ert_typ(from,to)                                              */
/*       trout_sclk_typ(from,to)                                             */
/*       trout_ssi_lrs_p_typ(from,to)                                        */
/*       trout_ssi_flg_typ(from,to)                                          */
/*       trout_j2000_ascii_typ(from,to)                                      */
/*                                                                           */
/*       get_ssi_data_format(unit,format)                                    */
/*                                                                           */
/*       init_trans_out(byte_trans,half_trans,full_trans)                    */
/*       init_trans_inb(unit,byte_trans,half_trans,full_trans)               */
/*       init_pixsizeb(unit,byte_size,full_size,half_size)                   */
/*****************************************************************************/
#include "xvmaininc.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "gll_main.h"
#include "gll_lrs.h"
#include "gll_ssi_edr.h"
#include "gll_ssi_bin.h"
#include "zvproto.h"

int gll_ssi_bin_debug = FALSE; /* Flag for turning debugging on.             */

/*****************************************************************************/
/* Get and translate one SSI EDR/UDR binary bad data value header record.    */
/* Translation is from any format, into native format.                       */
/*****************************************************************************/
ROUTINE int get_ssi_bdv_hdr_rec(unit,line,dest)
int            unit,            /* must be an open file with COND BINARY set */
               line;            /* file line! This number is never less than
                                   2 if there is a telemetry header on the 
                                   image.                                    */
ssi_bdvh_typ  *dest;
{
   unsigned char *buf, 
                 *p;
   int recsize, 
       status = SUCCESS,
       i,
       data_exists;

   init_trans_inb(unit,byte_trans,half_trans,full_trans);
   init_pixsizeb(unit,&byte_size,&half_size,&full_size);
   status = zvget(unit, "RECSIZE", &recsize, NULL);
   if (status != SUCCESS) {
     sprintf(aline,
     "get_ssi_bdv_hdr_rec> failed zvget at line %d, status %d",line,status);
     zvmessage(aline,0);
     zvsignal(unit,status,TRUE);
   }
   buf = (unsigned char *) malloc(recsize);
   if (buf == NULL) return BAD_MALLOC;
   status = zvread(unit,buf,"LINE",line,NULL); 
   if (status != SUCCESS) {
     sprintf(aline,
     "get_ssi_bdv_hdr_rec> failed zvread at line %d, status %d",line,status);
     zvmessage(aline,0);
     zvsignal(unit,status,TRUE);
   }
   p = buf;

   zvtrans(half_trans, p, &dest->record_id, 1); 
   /* Check to see if this is good data */
   if ((dest->record_id != BDV_DATA_DROPOUT)    && 
       (dest->record_id != BDV_SATURATED_PX)    &&
       (dest->record_id != BDV_LFW_PX)          &&
       (dest->record_id != BDV_SINGLE_PX_SPIKE) &&
       (dest->record_id != BDV_RS_OVERFLOW)) { 
          /* Image data structure uses rec_id as first byte */
     if ((dest->record_id & 0xFF) == BDV_IMAGE_DATA) {
         /* we got image data! */       
/*       sprintf(aline,
       "get_ssi_bdv_hdr_rec> first image line found at file line %d",line);
       zvmessage(aline,0);
*/
       return (END_BDVH);
     } 
     else if (dest->record_id == BDV_EMPTY_REC) { /*empty record found*/
       sprintf(aline,
      "get_ssi_bdv_hdr_rec> empty bdv data record found at line %d",line);
       zvmessage(aline,0);
       /* keep going, and return the empty line to the calling routine. */
     }
     else { /* we got garbage */
       sprintf(aline, 
       "get_ssi_bdv_hdr_rec> found unrecognizable record ID = %d at line %d",
         dest->record_id,line);
       zvmessage(aline,0);
       zabend();
     }
   }
   p += half_size;

   THALF(p, &dest->code); 
   if ((dest->code != BDV_SPX)      &&
       (dest->code != BDV_LINE_SEG) &&
       (dest->code != BDV_SAMP_SEG)) { /* bad data code was found */
      sprintf(aline,
      "get_ssi_bdv_hdr_rec> found unrecognizable BDV object code = %d at line %d",
      dest->code,line);
      zvmessage(aline,0);
      zabend();
   }

   THALF(p, &dest->nobjs); 
   if ((dest->code == BDV_SPX)      && (dest->nobjs > MAX_FFE_SPX_OBJ)) {
     sprintf(aline,
     "get_ssi_bdv_hdr_rec> bad number of bdv objects in line %d",line);
     zvmessage(aline,0);
     sprintf(aline,
       "get_ssi_bdv_hdr_rec> %d objects indicated, max allowed is %d for single pixel",
       dest->nobjs, MAX_FFE_SPX_OBJ);
     zvmessage(aline,0);
     zabend();
   } 
   if ((dest->code == BDV_LINE_SEG) && (dest->nobjs > MAX_FFE_LINE_OBJ)) {
     sprintf(aline,
     "get_ssi_bdv_hdr_rec> bad number of bdv objects in line %d",line);
     zvmessage(aline,0);
     sprintf(aline,
       "get_ssi_bdv_hdr_rec> %d objects indicated, max allowed is %d for line triplet",
       dest->nobjs, MAX_FFE_LINE_OBJ);
     zvmessage(aline,0);
     zabend();
   }
   if ((dest->code == BDV_SAMP_SEG) && (dest->nobjs > MAX_FFE_COL_OBJ)) {
     sprintf(aline,
     "get_ssi_bdv_hdr_rec> bad number of bdv objects in line %d",line);
     zvmessage(aline,0);
     sprintf(aline,
      "get_ssi_bdv_hdr_rec> %d objects indicated, max allowed is %d for sample triplet",
      dest->nobjs, MAX_FFE_COL_OBJ);
     zvmessage(aline,0);
     zabend();
   }

   if (dest->code == BDV_SPX) { 
     for (i=0; i < dest->nobjs; i++) {
       THALF(p, &dest->coords.pixel_data[i].line);   
       THALF(p, &dest->coords.pixel_data[i].sample);   
     }
   } 
   else if (dest->code == BDV_LINE_SEG) { 
     for (i=0; i < dest->nobjs; i++) {
       THALF(p, &dest->coords.line_data[i].line);   
       THALF(p, &dest->coords.line_data[i].ss);   
       THALF(p, &dest->coords.line_data[i].ns);   
     }
   }
   else if (dest->code == BDV_SAMP_SEG) { 
     for (i=0; i < dest->nobjs; i++) {
       THALF(p, &dest->coords.column_data[i].sample);   
       THALF(p, &dest->coords.column_data[i].sl);   
       THALF(p, &dest->coords.column_data[i].nl);   
     }
   }
   if (dest->record_id == BDV_EMPTY_REC)
      return (EMPTY_BDVH_REC);
   else {
      return (SUCCESS);
   }
} /* end ssi_get_bdv_hdr() */


/*****************************************************************************/
/* Write one SSI EDR/UDR binary bad data value header record.                */
/* Translation is from native format into VAX format.                        */
/* This routine does not return any values; the contents of "dest" get       */
/* written directly to the output file "ounit".                              */
/*                                                                           */
/* NOTE: Unlike get_ssi_bdv_hdr_rec(), this routine does not check against   */
/* the record_id or other data to make sure that the data is valid!!         */
/*****************************************************************************/
ROUTINE int write_ssi_bdv_hdr_rec(ounit,line,source)
int            ounit,           /* must be an open file with COND BINARY set */
               line;            
ssi_bdvh_typ  *source;          /* data to be written to ounit */
{
   unsigned char *buf, 
                 *p;            /* destination buffer */
   int recsize,
       i,
       data_exists,
       status = SUCCESS;

   init_trans_out(byte_trans,half_trans,full_trans);
   init_pixsizeb(ounit,&byte_size,&half_size,&full_size);
   status = zvget(ounit, "RECSIZE", &recsize, NULL);
   zvsignal(ounit,status,TRUE);
   buf = (unsigned char *) malloc(recsize);
   if (buf == NULL) return BAD_MALLOC;
   p = buf;

/* Start filling up the buffer with pieces from the *source structure,       */
/* until one line is packed. Then write it out.                              */
   TOHALF(&source->record_id, p);
   TOHALF(&source->code, p);
   TOHALF(&source->nobjs, p); 

   /* check nobjs to be sure we have a rational number for looping */
   if ((source->nobjs < 1) || (source->nobjs > MAX_FFE_SPX_OBJ)) {
     sprintf(aline,"SSI_WRITE_BDV_HDR> invalid nobjs = %d",source->nobjs);
     zvmessage(aline,0);
     zabend();
   }

   if (source->code == BDV_SPX) { 
     for (i=0; i < source->nobjs; i++) {
       TOHALF(&source->coords.pixel_data[i].line, p); 
       TOHALF(&source->coords.pixel_data[i].sample, p);   
     }
   } 
   else if (source->code == BDV_LINE_SEG) { 
     for (i=0; i < source->nobjs; i++) {
       TOHALF(&source->coords.line_data[i].line, p);   
       TOHALF(&source->coords.line_data[i].ss, p);   
       TOHALF(&source->coords.line_data[i].ns, p);   
     }
   }
   else if (source->code == BDV_SAMP_SEG) { 
     for (i=0; i < source->nobjs; i++) {
       TOHALF(&source->coords.column_data[i].sample, p);   
       TOHALF(&source->coords.column_data[i].sl, p);   
       TOHALF(&source->coords.column_data[i].nl, p);   
     }
   }
   else {
     sprintf(aline,"SSI_WRITE_BDV_HDR> invalid code = %d",source->code);
     zvmessage(aline,0);
     zabend();
   }

   status = zvwrit(ounit,buf,"LINE",line,NULL); 
   zvsignal(ounit,status,TRUE);

   free(buf);

   return SUCCESS;
} /* end ssi_write_bdv_hdr() */

/*****************************************************************************/
/* Get one image line from an SSI EDR, translating both the binary prefix    */
/* and image data.                                                           */
/*****************************************************************************/
ROUTINE int get_gll_edr_ln(unit, line, dest)
int             unit,           /* must be an open file with COND BINARY set */
                line;           /* image line number (starting at zero)      */
ssi_edr_ln_typ *dest;
{  
   int i, ns, nlb,
       status = SUCCESS;

   /* Grab the binary prefix. get_ssi_prefix adds nlb to "line" internally. */

   get_ssi_prefix(unit, line, &(dest->hdr));  

   /* Fill dest structure, bypassing the binary prefix (which is already    */
   /* flled.)  The pixel array is of type UWORD.                           */

   /* The call to zvget for NS determines how many samples there are in the */
   /* image line, which is particularly important for EDRs as this number   */
   /* is different for full-frame and sum-mode EDRs.                        */

   status = zvget(unit, "NS", &ns, "NLB", &nlb, NULL);
   zvsignal(unit,status,TRUE);
   status = zvread(unit, &dest->pixel[0], 
                "LINE", line+nlb, 
                "SAMP", BYTE_PREFIX_SZ+1, 
                "NSAMPS", ns,
                NULL);	
   if (status != SUCCESS) {
     sprintf(aline,"get_gll_edr_ln> bad zvread, failing line = %d\n",line);
     zvmessage(aline,0);
   }
   zvsignal(unit,status,TRUE);

   return SUCCESS;
} /*end get_gll_edr_ln */

/*****************************************************************************/
/* Write one image line to an SSI EDR, translating both the binary prefix    */
/* and image data.                                                           */
/*****************************************************************************/
ROUTINE int write_gll_edr_ln(unit, line, source)
int            unit,             /* must be an open file with COND BINARY set */
               line;             /* image line number */

ssi_edr_ln_typ *source;
{  
   int i, ns, nlb, status = SUCCESS;

   /* Write the binary prefix. write_ssi_prefix adds nlb to "line"          */
   /* internally.                                                           */

    write_ssi_prefix(unit, line, &source->hdr);

   /* Fill the dest structure, bypassing the binary prefix.                 */

   /* The call to zvget for NS determines how many samples there are in the */
   /* image line, which is particularly important for EDRs as this number   */
   /* is different for full-frame and sum-mode EDRs.                        */

   status = zvget(unit, "NS", &ns, "NLB", &nlb, NULL);
   zvsignal(unit,status,TRUE);
   status = zvwrit(unit, &source->pixel[0], 
                "LINE", line+nlb, 
                "SAMP", BYTE_PREFIX_SZ+1, 
                "NSAMPS", ns,
                NULL);	
   zvsignal(unit,status,TRUE);

   return SUCCESS;
} /*end write_gll_edr_ln */

/*****************************************************************************/
/* Get one image line from an SSI UDR, translating both the binary prefix    */
/* and image data.                                                           */
/*****************************************************************************/
ROUTINE int get_gll_udr_ln(unit, line, dest)
int            unit,               /* must be an open file with COND BINARY set */
               line;               /* image line number */
ssi_udr_ln_typ *dest;
{  
   int i, ns, nlb,
       status = SUCCESS;
 
   /* Grab the binary prefix. get_ssi_prefix adds nlb to "line" internally. */

   get_ssi_prefix(unit, line, &dest->hdr);

   /* Fill dest structure, bypassing the binary prefix (which is already    */
   /* filled. The pixel array is of type UBYTE. */

   status = zvget(unit, "NS", &ns, "NLB", &nlb, NULL);
   zvsignal(unit,status,TRUE);
   status = zvread(unit, &dest->pixel[0], 
                "LINE", line+nlb, 
                "SAMP", BYTE_PREFIX_SZ+1, 
                "NSAMPS", ns,
                NULL);	
   zvsignal(unit,status,TRUE);

   return SUCCESS;
} /*end get_gll_udr_ln */

/*****************************************************************************/
/* Write one image line to an SSI UDR, translating both the binary prefix    */
/* and image data.                                                           */
/*****************************************************************************/
ROUTINE int write_gll_udr_ln(unit, line, source)
int            unit,            /* must be an open file with COND BINARY set */
               line;            /* image line number */
ssi_udr_ln_typ *source;
{  
   int i, ns, nlb, status = SUCCESS;

   /* Write the binary prefix. write_ssi_prefix adds nlb to "line"          */
   /* internally.                                                           */
    write_ssi_prefix(unit, line, &source->hdr);

   /* Fill the dest structure, bypassing the binary prefix. */

   status = zvget(unit, "NS", &ns, "NLB", &nlb, NULL);
   zvsignal(unit,status,TRUE);
   status = zvwrit(unit, &source->pixel[0], 
                "LINE", line+nlb,
                "SAMP", BYTE_PREFIX_SZ+1,  
                "NSAMPS", ns,
                NULL);	
   zvsignal(unit,status,TRUE);

   return SUCCESS;
} /*end write_gll_udr_ln */


/****************************************************************************/
/* Read in a GLL SSI EDR binary telemetry header, and translate it to native	*/
/* format.  The structure of the file is defined by how the VAX/VMS	*/
/* compiler interprets the structures in "gll_ssi_edr.h".  The		*/
/* structure cannot be used to overlay the data read from the file,	*/
/* since different compilers will pack the structure differently.  So,	*/
/* the data elements are in the order they are defined in the structure,*/
/* packed together as tightly as possible.				*/

/* It is assumed that the entire binary header fits in the first line	*/
/* of the file.  If this is not the case, modify the malloc and read	*/
/* statusements appropriately.						*/

/* The caller is responsible for setting an error action to deal with	*/
/* any errors that occur during the read.				*/

/* This is not technically correct because a signed conversion (HALF or	*/
/* FULL) is used for numbers defined to be unsigned... but this is ok	*/
/* for all currently defined integer types.				*/

/* The strings could be defined to be longer in the destination		*/
/* structure, and a NULL terminator appended.  The dest structure does	*/
/* NOT have to have any resemblance to the file structure if it's not	*/
/* convenient.  Constants are used for string lengths; it is assumed	*/
/* these are defined in the file format and will not change.  A sizeof	*/
/* operator should not be used on the destination string, as that may	*/
/* not be the same size as the file.  Symbolic constants would be	*/
/* better, but since this and the associated write routine are the only	*/
/* routines that should touch the file structure, literal constants	*/
/* are acceptable here.							*/

/* An assumption is made that fields that are declared "char x[n]",	*/
/* such as trunc_pixel and i_f, really *are* characters.  If this is	*/
/* not the case, the conversions will have to be modified for those	*/
/* fields.								*/

ROUTINE int get_ssi_telem_hdr(unit, dest)
int unit;		/* must be an open file with COND BINARY set */
ssi_edr_hdr_typ *dest;
{
   unsigned char *buf,
                 *p;
   int            recsize,i,status = SUCCESS,
                  format,  /* data format */
                  nrecs;   /* number of records to get */

   init_trans_inb(unit,byte_trans,half_trans,full_trans); 
   init_pixsizeb(unit,&byte_size,&half_size,&full_size);

   get_ssi_data_format(unit,&format);
   if (format == BYTE_DATA) 
     nrecs = UDR_THDR_RECS;
   else if (format == FF_HALF_DATA)
     nrecs = FF_EDR_THDR_RECS;
   else if (format == SM_HALF_DATA)
     nrecs = SM_EDR_THDR_RECS;

   status = zvget(unit,"RECSIZE",&recsize,NULL);
   zvsignal(unit,status,TRUE);
   buf = (unsigned char *) malloc(recsize*nrecs);
   if (buf == NULL)
      return BAD_MALLOC;

   /* Fill up buf with the entire telemetry header, reading the number of    */
   /* records indicated by the data format.                                  */
   for (i=0; i<nrecs; i++) {     
     status = zvread(unit,buf+(i*recsize),"LINE",1+i,NULL);
     zvsignal(unit,status,TRUE);
   }
   p = buf;

   /* Fill in the structure.  Bytes are translated (although that's	*/
   /* probably unnecessary, it's a good idea), while characters are	*/
   /* just moved.							*/

   TBYTE(p, &dest->record_number);
   TBYTE(p, &dest->file_number);
   TSTRN(p, dest->project, 10);
   TSTRN(p, dest->instrument, 6);
   THALF(p, &dest->phy_seq);
   THALF(p, &dest->log_seq);
   trin_ert_typ(&p, &dest->fert);	/* increments p */
   trin_ert_typ(&p, &dest->lert);
   trin_sclk_typ(&p, &dest->fsclk);
   trin_sclk_typ(&p, &dest->lsclk);
   trin_scet_typ(&p, &dest->scet);
   TSTRN(p, dest->mipl_prd, 59);
   THALF(p, &dest->format_id);
   TFULL(p, &dest->sync_err);
   TBYTE(p, &dest->boom);
   THALF(p, &dest->missings);
   THALF(p, &dest->partials);
   THALF(p, &dest->unreadables);
   THALF(p, &dest->seq_brk);
   THALF(p, &dest->src_inp);
   THALF(p, &dest->wbdl);
   THALF(p, &dest->sdrs);
   THALF(p, &dest->sfdus);
   TSTRN(p, dest->pic_no, 7);
   trin_ssi_lrs_p_typ(&p, &dest->ssi_lrs);
   trin_ssi_flg_typ(&p, &dest->flags);
   TSTRN(p, dest->dn, 6);
   TSTRN(p, dest->trunc_bits, 6);
   TSTRN(p, dest->trunc_pixel, 6);
   TSTRN(p, dest->i_f, 12);
   TSTRN(p, dest->entropy, 7);
   TSTRN(p, dest->entropies, 15 * 7);
   trin_j2000_ascii_typ(&p, &dest->point_dir);
   TSTRN(p, dest->scale, 2 * 8);
   TSTRN(p, dest->slope, 32);
   TSTRN(p, dest->offset, 32);
   TSTRN(p, dest->activity, 20);
   p += byte_size;                                 /* skip filler_1 */
   TBYTE(p, &dest->filter);
   TBYTE(p, &dest->exposure);
   TBYTE(p, &dest->image_mode);
   TBYTE(p, &dest->gain);
   TFULL(p, &dest->range);
   TBYTE(p, &dest->tlm_frmt);
   THALF(p, &dest->version);
   trin_sclk_typ(&p, &dest->ssclk);
   trin_sclk_typ(&p, &dest->esclk);
   p += 318;                                    /* skip reserved */
   zvtrans(full_trans, p, dest->hist, 256);
   p += 256 * full_size;

   return SUCCESS;	
} /* end get_ssi_telem_hdr */

/****************************************************************************/
/* write the telemetry header out                                           */
/****************************************************************************/
ROUTINE write_ssi_telem_hdr(unit, source)
int              unit;		/* must be an open file with COND BINARY set */
ssi_edr_hdr_typ *source;
{
   unsigned char *buf, 
                 *p;
   int            recsize,i,status = SUCCESS,
                  format,  /* data format */
                  nrecs;   /* number of records to get */

   init_trans_out(byte_trans,half_trans,full_trans);
   init_pixsizeb(unit,&byte_size,&half_size,&full_size);

   get_ssi_data_format(unit,&format);
   if (format == BYTE_DATA) 
     nrecs = UDR_THDR_RECS;
   else if (format == FF_HALF_DATA)
     nrecs = FF_EDR_THDR_RECS;
   else if (format == SM_HALF_DATA)
     nrecs = SM_EDR_THDR_RECS;

   status = zvget(unit, "RECSIZE", &recsize, NULL);
   zvsignal(unit,status,TRUE);
   buf = (unsigned char *) malloc(recsize*nrecs);
   if (buf == NULL)
      return BAD_MALLOC;
   p = buf;

   /* Fill in the buffer with elements from the structure.  Bytes are       */
   /* translated (although that's probably unnecessary, it's a good idea),  */
   /* while characters are just moved.		         	      	    */

   TOBYTE(&source->record_number, p);
   TOBYTE(&source->file_number, p);
   TOSTRN(source->project, p, 10);
   TOSTRN(source->instrument, p, 6);
   TOHALF(&source->phy_seq, p);
   TOHALF(&source->log_seq, p);
   trout_ert_typ(&source->fert, &p);	/* increments p */
   trout_ert_typ(&source->lert, &p);
   trout_sclk_typ(&source->fsclk, &p);
   trout_sclk_typ(&source->lsclk, &p);
   trout_scet_typ(&source->scet, &p);
   TOSTRN(source->mipl_prd, p, 59);
   TOHALF(&source->format_id, p);
   TOFULL(&source->sync_err, p);
   TOBYTE(&source->boom, p);
   TOHALF(&source->missings, p);
   TOHALF(&source->partials, p);
   TOHALF(&source->unreadables, p);
   TOHALF(&source->seq_brk, p);
   TOHALF(&source->src_inp, p);
   TOHALF(&source->wbdl, p);
   TOHALF(&source->sdrs, p);
   TOHALF(&source->sfdus, p);
   TOSTRN(source->pic_no, p, 7);
   trout_ssi_lrs_p_typ(&source->ssi_lrs, &p);
   trout_ssi_flg_typ(&source->flags, &p);
   TOSTRN(source->dn, p, 6);
   TOSTRN(source->trunc_bits, p, 6);
   TOSTRN(source->trunc_pixel, p, 6);
   TOSTRN(source->i_f, p, 12);
   TOSTRN(source->entropy, p, 7);
   TOSTRN(source->entropies, p, 15 * 7);
   trout_j2000_ascii_typ(&source->point_dir, &p);
   TOSTRN(source->scale, p, 2 * 8);
   TOSTRN(source->slope, p, 32);
   TOSTRN(source->offset, p, 32);
   TOSTRN(source->activity, p, 20);   
   p += byte_size;                           /* skip filler_1 */
   TOBYTE(&source->filter, p);
   TOBYTE(&source->exposure, p);
   TOBYTE(&source->image_mode, p);
   TOBYTE(&source->gain, p);
   TOFULL(&source->range, p);
   TOBYTE(&source->tlm_frmt, p);
   TOHALF(&source->version, p);
   trout_sclk_typ(&source->ssclk, &p);
   trout_sclk_typ(&source->esclk, &p);   
   p += 318;                                /* skip reserved */
   zvtrans(full_trans, source->hist, p, 256);
   p += 256 * full_size;

   /* Write out the entire telemetry header, by writing the number of        */
   /* records indicated by the data format.                                  */
   for (i=0; i<nrecs; i++) {     
     status = zvwrit(unit,buf+(i*recsize),"LINE",1+i,NULL);
     zvsignal(unit,status,TRUE);
   }

   free(buf);

   return SUCCESS;
} /* end write_ssi_telem_hdr */

/*****************************************************************************/
/* This routine translates one line of binary image line prefix into native  */
/* format (DEFAULT_HOST) and writes it out to the file specified by ounit.   */
/*****************************************************************************/
ROUTINE int write_ssi_prefix(ounit,line,source)
int           ounit,
              line;
ssi_lhdr_typ *source;
{
  unsigned char *p, 
                *buf;
  UBYTE          bits_byte = 0;      
  UWORD          bits_half = 0;  
  UINT           bits_full = 0;
  int            status = SUCCESS,
                 nlb,          /* # lines of binary header */
                 nbb;          /* # bytes of binary prefix data */

 status =  zvget(ounit, "NLB", &nlb, "NBB", &nbb, NULL);
 zvsignal(ounit,status,TRUE);
 buf = (unsigned char *) malloc (nbb);         
 if (buf == NULL) 
   return BAD_MALLOC;
 p = buf;

 init_trans_out(byte_trans,half_trans,full_trans);
 init_pixsizeb(ounit,&byte_size,&half_size,&full_size);
   
 TOBYTE(&source->rec_id, p);
 TOBYTE(&source->file_no, p);
 TOHALF(&source->phy_seq, p);

 TOHALF(&source->log_seq, p);

 TOHALF(&source->ert.year, p);
 TOHALF(&source->ert.day, p);
 TOBYTE(&source->ert.hour, p);
 TOBYTE(&source->ert.minute, p);
 TOBYTE(&source->ert.second, p);
 TOHALF(&source->ert.msecond, p);

 TOFULL(&source->sclk.rim, p);
 TOBYTE(&source->sclk.mod91, p);
 TOBYTE(&source->sclk.mod10, p);
 TOBYTE(&source->sclk.mod8, p);

 TOSTRN(source->mpr, p, 59);
 TOHALF(&source->fid, p) ;
 TOBYTE(&source->input.type, p);

 /* input source */
 if (source->input.SFDU)  bits_byte  |= 0x01;
 if (source->input.WBDL)  bits_byte  |= 0x02;
 if (source->input.SDR)   bits_byte  |= 0x04;
 if (source->input.IDR)   bits_byte  |= 0x08;
 if (source->input.EDR)   bits_byte  |= 0x10;
 if (source->input.RT)    bits_byte  |= 0x20;
 if (source->input.APB)   bits_byte  |= 0x40;
 TOBYTE(&bits_byte, p);

 TOBYTE(&source->allowed_sync_err, p);
 TOBYTE(&source->sync_err, p);
 zvtrans(byte_trans, &source->ssi_lrs[0], p, 12);
 p += 12 * byte_size;
 
 TOHALF(&source->last_pix, p);

 bits_half |= (source->sync_stat.error & 0x3FFF);
 bits_half |= (source->sync_stat.status & 0x03) << 14;
 TOHALF(&bits_half, p);

 bits_full |= (source->truncation.blk0 & 0x03);
 bits_full |= (source->truncation.blk1 & 0x03) << 2;
 bits_full |= (source->truncation.blk2 & 0x03) << 4;
 bits_full |= (source->truncation.blk3 & 0x03) << 6;
 bits_full |= (source->truncation.blk4 & 0x03) << 8;
 bits_full |= (source->truncation.blk5 & 0x03) << 10;
 bits_full |= (source->truncation.blk6 & 0x03) << 12;
 bits_full |= (source->truncation.blk7 & 0x03) << 14;
 bits_full |= (source->truncation.blk8 & 0x03) << 16;
 bits_full |= (source->truncation.blk9 & 0x03) << 18;
 bits_full |= (source->truncation.blk10 & 0x03) << 20;
 bits_full |= (source->truncation.blk11 & 0x03) << 22;
 bits_full |= (source->truncation.blk12 & 0x03) << 24;
 TOFULL(&bits_full, p);

 TOHALF(&source->truncated, p);
 TOHALF(&source->version, p);
 TOHALF(&source->ssnr, p);
 TOBYTE(&source->dsn_id, p);
 TOHALF(&source->image_line, p);
 TOBYTE(&source->rs_overflow, p);

 status = zvwrit(ounit, buf, "LINE", line+nlb, "NSAMPS", nbb, NULL);
 zvsignal(ounit,status,TRUE);

 free(buf);
  
 return SUCCESS;
} /* end write_ssi_prefix */

/*****************************************************************************/
/* Translation routines for specific datatypes: translate any input host fmt */
/*****************************************************************************/
ROUTINE static void trin_scet_typ(from, to)
unsigned char **from;
scet_typ *to;
{
   trin_ert_typ(from, (ert_typ *)to);		/* scet_typ same as ert_typ */
}


ROUTINE static void trin_ert_typ(from, to)
unsigned char **from;
ert_typ *to;
{
   THALF(*from, &to->year);
   THALF(*from, &to->day);
   TBYTE(*from, &to->hour);
   TBYTE(*from, &to->minute);
   TBYTE(*from, &to->second);
   THALF(*from, &to->msecond);
}

ROUTINE static void trin_sclk_typ(from, to)
unsigned char **from;
sclk_typ *to;
{
   TFULL(*from, &to->rim);
   TBYTE(*from, &to->mod91);
   TBYTE(*from, &to->mod10);
   TBYTE(*from, &to->mod8);
}

ROUTINE static void trin_ssi_lrs_p_typ(from, to)
unsigned char **from;
ssi_lrs_p_typ *to;
{
   int i;

   for (i=0; i<3; i++) {
      zvtrans(byte_trans, *from, to->packet[i].stnd_hk, 2);
      *from += 2*byte_size;
      zvtrans(byte_trans, *from, to->packet[i].img_hk, 2);
      *from += 2*byte_size;
   }
}

ROUTINE static void trin_ssi_flg_typ(from, to)
unsigned char **from;
ssi_flg_typ *to;
{
   unsigned short int bits;

   zvtrans(half_trans, *from, &bits, 1);    /* byte-swap so bit masks work */
   *from += half_size;
   to->comp = bits & 0x01;
   to->cmp_mode = (bits >> 1) & 0x01;
   to->expo = (bits >> 2) & 0x01;
   to->flood = (bits >> 3) & 0x01;
   to->blem = (bits >> 4) & 0x01;
   to->clock = (bits >> 5) & 0x01;
   /* to->filler is not set */
}

ROUTINE static void trin_j2000_ascii_typ(from, to)
unsigned char **from;
j2000_ascii_typ *to;
{
   TSTRN(*from, to->ra, 8);
   TSTRN(*from, to->dcl, 8);
   TSTRN(*from, to->twst, 8);
}

/*****************************************************************************/
/* Translation routines for specific datatypes: write NATIVE output          */
/*****************************************************************************/
ROUTINE static void trout_scet_typ(from, to)
scet_typ *from;
unsigned char **to;
{
   trout_ert_typ((ert_typ *)from, to);		/* scet_typ same as ert_typ */
}

ROUTINE static void trout_ert_typ(from, to)
ert_typ *from;
unsigned char **to;
{
   TOHALF(&from->year, *to);
   TOHALF(&from->day, *to);
   TOBYTE(&from->hour, *to);
   TOBYTE(&from->minute, *to);
   TOBYTE(&from->second, *to);
   TOHALF(&from->msecond, *to);
}

ROUTINE static void trout_sclk_typ(from, to)
sclk_typ *from;
unsigned char **to;
{
   TOFULL(&from->rim, *to);
   TOBYTE(&from->mod91, *to);
   TOBYTE(&from->mod10, *to);
   TOBYTE(&from->mod8, *to);
}

ROUTINE static void trout_ssi_lrs_p_typ(from, to)
ssi_lrs_p_typ *from;
unsigned char **to;
{
   int i;

   for (i=0; i<3; i++) {
      zvtrans(byte_trans, from->packet[i].stnd_hk, *to, 2);
      *to += 2*byte_size;
      zvtrans(byte_trans, from->packet[i].img_hk, *to, 2);
      *to += 2*byte_size;
   }
}

ROUTINE static void trout_ssi_flg_typ(from, to)
ssi_flg_typ *from;
unsigned char **to;
{
   unsigned short int bits = 0;
   if (from->comp)     bits |= 0x01;
   if (from->cmp_mode) bits |= 0x02;
   if (from->expo)     bits |= 0x04;
   if (from->flood)    bits |= 0x08;
   if (from->blem)     bits |= 0x10;
   if (from->clock)    bits |= 0x20;
   zvtrans(half_trans, &bits, *to, 1);    /* byte-swap so bit masks work */
   *to += half_size;
   /* from->filler is not set */
}

ROUTINE static void trout_j2000_ascii_typ(from, to)
j2000_ascii_typ *from;
unsigned char **to;
{
   TOSTRN(from->ra, *to, 8);
   TOSTRN(from->dcl, *to, 8);
   TOSTRN(from->twst, *to, 8);
}

/****************************************************************************/
/* This routine determines whether an SSI image is one of the following:   */
/*     full-frame EDR   (FF_HALF_DATA)                                     */
/*     sum-mode   EDR   (SM_HALF_DATA)                                     */
/*                UDR   (BYTE_DATA)                                        */
/* Full-frame and sum-mode UDRs are identical as far as binary information */
/* is concerned, so they are not distinguished.                            */
/*                                                                         */
/* This routine is not static void since it's used in gllfillin.c.         */
/****************************************************************************/
ROUTINE void get_ssi_data_format(unit,format)
int  unit,   /* unit number of the file */
    *format; /* output */
{
  char fmt[32];
  int nl, status = SUCCESS;

  status = zvget(unit,"format",fmt,NULL);
  zvsignal(unit,status,TRUE);

  if (strcmp(fmt,"BYTE") == 0) *format = BYTE_DATA;
  else if (strcmp(fmt,"HALF") == 0) {
    status = zvget(unit, "NL", &nl, NULL); 
    zvsignal(unit,status,TRUE);
    if (nl == NLINES)         *format = FF_HALF_DATA; /* Full-Frame EDR */ 
    else if (nl == NLINES_SE) *format = SM_HALF_DATA; /* Sum-mode EDR */
  }
  else {
    zvmessage("get_data_format> Error! Image format is not byte or halfword",0);
    zabend();
  }
}
/****************************************************************************/
/* This routine sets up translation buffers for output, converting from the */
/* machine's native representation into the DEFAULT_HOST (VAX)              */
/* representation.                                                          */
/****************************************************************************/
ROUTINE static void init_trans_out(byte_tr,half_tr,full_tr)
int *byte_tr,
    *half_tr,
    *full_tr;
{
   int status;
   char intfmt[12],realfmt[12];

   status = zvhost(DEFAULT_HOST,intfmt,realfmt);
   status = zvtrans_out(byte_tr, "BYTE", "BYTE", intfmt, realfmt);
   status = zvtrans_out(half_tr, "HALF", "HALF", intfmt, realfmt);
   status = zvtrans_out(full_tr, "FULL", "FULL", intfmt, realfmt);
 } /* end init_trans_out */

/****************************************************************************/
/* This routine sets up translation buffers for input, converting from the  */
/* host representation into the machine's native representation.            */
/****************************************************************************/
ROUTINE static void init_trans_inb(unit,byte_tr,half_tr,full_tr)
int unit,
    *byte_tr,
    *half_tr,
    *full_tr;
{
   int status = SUCCESS;

   status = zvtrans_inb(byte_tr, "BYTE", "BYTE", unit);
   zvsignal(unit,status,TRUE);
   status = zvtrans_inb(half_tr, "HALF", "HALF", unit);
   zvsignal(unit,status,TRUE);
   status = zvtrans_inb(full_tr, "FULL", "FULL", unit);
   zvsignal(unit,status,TRUE);
 } /* end init_trans_inb */

/****************************************************************************/
/* This routine returns the size of a binary label value (in bytes) from a  */
/* file.
/****************************************************************************/
ROUTINE static void init_pixsizeb(unit,byte_sz,half_sz,full_sz)
int unit,
    *byte_sz,
    *half_sz,
    *full_sz;
{
   int status;
   char aline[80];
 
   status = zvpixsizeb(byte_sz, "BYTE", unit);
   zvsignal(unit,status,TRUE);
   if (byte_sz == 0) {
     sprintf(aline,
     "init_pixsizeb> error in byte pixel size determination, status %d",byte_sz);
     zvmessage(aline,0);
     zabend();
   }

   status = zvpixsizeb(half_sz, "HALF", unit);
   zvsignal(unit,status,TRUE);
   if (half_sz == 0) {
     sprintf(aline,
    "init_pixsizeb> error in halfword pixel size determination, status %d",half_sz);
     zvmessage(aline,0);
     zabend();
   }

   status = zvpixsizeb(full_sz, "FULL", unit);
   zvsignal(unit,status,TRUE);
   if (full_sz == 0) {
     sprintf(aline,
    "init_pixsizeb> error in fullword pixel size determination, status %d",full_sz);
     zvmessage(aline,0);
     zabend();
   }
} /* end init_pixsizeb */
/* end module */
