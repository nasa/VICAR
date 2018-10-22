/*****************************************************************************/
/* Subroutines which read/write the GLL EDR/UDR Phase II binary  header      */
/* F, Moss, R. Deen (telemetry header code )                                 */
/*                                                                           */
/* Contents:                                                                 */
/*                                                                           */
/* Routines to read and write an EDR image line:                             */
/*       get_gll_ph2_edr_ln (unit,line,dest)                                 */
/*       write_gll_ph2_edr_ln (ounit,line,source)                            */
/*                                                                           */
/* Routines to read and write a UDR image line:                              */
/*       get_gll_ph2_udr_ln (unit,line,dest)                                 */
/*       write_gll_ph2_udr_ln (ounit,line,source)                            */
/*                                                                           */
/* Routines to write the binary telemetry header:                            */
/*       write_ssi_ph2_telem_hdr (ounit,source)                              */
/*                                                                           */
/* Routine to write a binary image line prefix:                              */
/*       write_ssi_ph2_prefix(ounit,line,source)                             */
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
/*       trin_flg_comp_typ(from,to)                                          */
/*       trin_word23_typ(from, to)                                           */
/*       trin_word24_typ(from, to)                                           */
/*       trin_word25_typ(from, to)                                           */
/*       trin_word26_typ(from, to)                                           */
/*                                                                           */
/*       trout_scet_typ(from,to)                                             */
/*       trout_ert_typ(from,to)                                              */
/*       trout_sclk_typ(from,to)                                             */
/*       trout_word23_typ(from, to)                                          */
/*       trout_word24_typ(from, to)                                          */
/*       trout_word25_typ(from, to)                                          */
/*       trout_word26_typ(from, to)                                          */
/*                                                                           */
/*       init_trans_out(byte_trans,half_trans,full_trans)        */
/*       init_trans_inb(unit,byte_trans,half_trans,full_trans)   */
/*       init_pixsizeb(unit,byte_size,full_size,half_size)       */
/*****************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include "xvmaininc.h"   
#include "rts_typedefs.h" 
#include "gll_rts_main.h"
#include "gll_rts_main_diff.h"
#include "gll_ph2_ssi_edr.h"
#include "gll_ssi_bin_diff.h" 
#include "gll_ph2_ssi_bin.h" 
#include "zvproto.h" 

/*    routine is declared as void, routine is called first(default returned   */
/*    value is int) before it is declared,so it needs to put prototype first  */ 
static void trin_word23_typ();  
static void trin_word24_typ();
static void trin_word25_typ();
static void trin_word26_typ();
static void trout_word23_typ();
static void trout_word24_typ();
static void trout_word25_typ();
static void trout_word26_typ();

int gll_ssi_bin_ph2_debug = FALSE; /* Flag for turning debugging on.         */

/*****************************************************************************/
/* Get one image line from an SSI EDR, translating both the binary prefix    */
/* and image data.                                                           */
/*****************************************************************************/
ROUTINE int get_gll_ph2_edr_ln(unit, line, dest)
int             unit,           /* must be an open file with COND BINARY set */
                line;           /* image line number (starting at zero)      */
ssi_edr_line_typ *dest;
{  
   int ns, nlb,
       status = SUCCESS;

   /* Grab the binary prefix. get_ssi_ph2_prefix adds nlb to "line" internally. */

   get_ssi_ph2_prefix(unit, line, &(dest->prefix));  

   /* Fill dest structure, bypassing the binary prefix (which is already    */
   /* flled.)  The pixel array is of type UWORD.                           */

   /* The call to zvget for NS determines how many samples there are in the */
   /* image line, which is particularly important for EDRs as this number   */
   /* is different for full-frame and sum-mode EDRs.                        */

   status = zvget(unit, "NS", &ns, "NLB", &nlb, NULL);
   zvsignal(unit,status,TRUE);
   status = zvread(unit, &dest->pixels[0], 
                "LINE", line+nlb, 
                "SAMP", BYTE_PREFIX_SZ+1, 
                "NSAMPS", ns,
                NULL);	
   if (status != SUCCESS) {
     sprintf(ph2_aline,"get_gll_ph2_edr_ln> bad zvread, failing line = %d\n",line);
     zvmessage(ph2_aline,0);
   }
   zvsignal(unit,status,TRUE);

   return SUCCESS;
} /*end get_gll_ph2_edr_ln */

/*****************************************************************************/
/* Write one image line to an SSI EDR, translating both the binary prefix    */
/* and image data.                                                           */
/*****************************************************************************/
ROUTINE int write_gll_ph2_edr_ln(unit, line, source)
int            unit,             /* must be an open file with COND BINARY set */
               line;             /* image line number */

ssi_edr_line_typ *source;
{  
   int ns, nlb, status = SUCCESS;

   /* Write the binary prefix. write_ssi_ph2_prefix adds nlb to "line"          */
   /* internally.                                                           */

    write_ssi_ph2_prefix(unit, line, &source->prefix);

   /* Fill the dest structure, bypassing the binary prefix.                 */

   /* The call to zvget for NS determines how many samples there are in the */
   /* image line, which is particularly important for EDRs as this number   */
   /* is different for full-frame and sum-mode EDRs.                        */

   status = zvget(unit, "NS", &ns, "NLB", &nlb, NULL);
   zvsignal(unit,status,TRUE);
   status = zvwrit(unit, &source->pixels[0], 
                "LINE", line+nlb, 
                "SAMP", BYTE_PREFIX_SZ+1, 
                "NSAMPS", ns,
                NULL);	
   zvsignal(unit,status,TRUE);

   return SUCCESS;
} /* end write_gll_ph2_edr_ln */

/*****************************************************************************/
/* Get one image line from an SSI UDR, translating both the binary prefix    */
/* and image data.                                                           */
/*****************************************************************************/
ROUTINE int get_gll_ph2_udr_ln(unit, line, dest)
int            unit,               /* must be an open file with COND BINARY set */
               line;               /* image line number */
ssi_line_typ *dest;
{  
   int ns, nlb,
       status = SUCCESS;
 
   /* Grab the binary prefix. get_ssi_ph2_prefix adds nlb to "line" internally. */

   get_ssi_ph2_prefix(unit, line, &dest->prefix);

   /* Fill dest structure, bypassing the binary prefix (which is already    */
   /* filled. The pixel array is of type UBYTE. */

   status = zvget(unit, "NS", &ns, "NLB", &nlb, NULL);
   if (status != SUCCESS) return status;

   status = zvread(unit, &dest->pixels[0], 
                "LINE", line+nlb, 
                "SAMP", BYTE_PREFIX_SZ+1, 
                "NSAMPS", ns,
                NULL);	

   return status;
} /*end get_gll_ph2_udr_ln */

/*****************************************************************************/
/* Write one image line to an SSI UDR, translating both the binary prefix    */
/* and image data.                                                           */
/*****************************************************************************/
ROUTINE int write_gll_ph2_udr_ln(unit, line, source)
int            unit,            /* must be an open file with COND BINARY set */
               line;            /* image line number */
ssi_line_typ *source;
{  
   int ns, nlb, status = SUCCESS;

   /* Write the binary prefix. write_ssi_ph2_prefix adds nlb to "line"          */
   /* internally.                                                           */
    write_ssi_ph2_prefix(unit, line, &source->prefix);

   /* Fill the dest structure, bypassing the binary prefix. */

   status = zvget(unit, "NS", &ns, "NLB", &nlb, NULL);
   if (status != SUCCESS) return status;

   status = zvwrit(unit, &source->pixels[0], 
                "LINE", line+nlb,
                "SAMP", BYTE_PREFIX_SZ+1,  
                "NSAMPS", ns,
                NULL);	

   return status;
} /*end write_gll_ph2_udr_ln */


/****************************************************************************/
/* Read in a GLL SSI EDR binary telemetry header, and translate it to native	*/
/* format.  The structure of the file is defined by how the VAX/VMS	*/
/* compiler interprets the structures in "gll_ph2_ssi_edr.h".  The		*/
/* structure cannot be used to overlay the data read from the file,	*/
/* since different compilers will pack the structure differently.  So,	*/
/* the data elements are in the order they are defined in the structure,*/
/* packed together as tightly as possible.				*/

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

ROUTINE int get_ssi_ph2_telem_hdr(unit, dest)
int unit;		/* must be an open file with COND BINARY set */
ssi_hdr_typ *dest;
{
   unsigned char *buf,
                 *p;
   int            recsize,i,status = SUCCESS,
                  nrecs;   /* number of records to get */

   init_trans_inb(unit,byte_trans,half_trans,full_trans); 
   init_pixsizeb(unit,&byte_size,&half_size,&full_size);

   status = zvget(unit, "RECSIZE", &recsize, NULL);
   zvsignal(unit,status,TRUE);

   if (recsize == 1800) nrecs =1;
   else if (recsize == 1000) nrecs = 2;
   else return BAD_MALLOC;

   buf = (unsigned char *) malloc (recsize*nrecs);
   if (buf == NULL) 
     return BAD_MALLOC;
   p = buf;

    /* Fill up buf with the entire telemetry header, reading the number of    */
   /* records indicated by the record size.                                  */
   for (i=0; i<nrecs; i++) {     
     status = zvread(unit,&buf[i*recsize],"LINE",1+i,NULL);
     if (status != SUCCESS) return status;
   }

   /* Fill in the structure.  Bytes are translated (although that's	*/
   /* probably unnecessary, it's a good idea), while characters are	*/
   /* just moved.							*/

   TBYTE(p, &dest->record_id);
   TBYTE(p, &dest->reserved1);
   TSTRN(p, dest->project, 10);
   TSTRN(p, dest->instrument, 6);
   THALF(p, &dest->reserved2);
   THALF(p, &dest->log_seq);
   trin_ert_typ(&p, &dest->first_ert);	/* increments p */
   trin_ert_typ(&p, &dest->last_ert);
   trin_sclk_typ(&p, &dest->first_sclk_rec);
   trin_sclk_typ(&p, &dest->last_sclk_rec);
   trin_scet_typ(&p, &dest->scet);
   TSTRN(p, dest->mips_prd.os, 8);
   TSTRN(p, dest->mips_prd.reserved1, 26);
   TSTRN(p, dest->mips_prd.cpu, 8);
   TSTRN(p, dest->mips_prd.date, 11);
   TSTRN(p, dest->mips_prd.filler, 6);
   THALF(p, &dest->rec_format);
   TFULL(p, &dest->reserved3A);
   TBYTE(p, &dest->boom);
   THALF(p, &dest->miss_lines);
   THALF(p, &dest->part_lines);
   THALF(p, &dest->reserved4);
   THALF(p, &dest->seq_break);
   THALF(p, &dest->reserved5[0]);  
   THALF(p, &dest->reserved5[1]);  
   THALF(p, &dest->reserved5[2]);   /*  or p += 3 * half_size  */  
   THALF(p, &dest->sfdus);
   TSTRN(p, dest->pic_no, 7);
/*   trin_ssi_lrs_p_typ(&p, &dest->ssi_lrs);   */
   p += 12 * byte_size;              /*  reserved6[12]    */
   trin_flg_comp_typ(&p, &dest->flags);
   TSTRN(p, dest->mean_dn, 6);
   TSTRN(p, dest->trun_bits, 6);
   TSTRN(p, dest->trun_pixel, 6);
   TSTRN(p, dest->mean_if, 12);
   TSTRN(p, dest->entrop_avg, 7);
   TSTRN(p, dest->entropies, 15 * 7);
   TSTRN(p, dest->pointing, 3*8);
   TSTRN(p, dest->scale_fact, 2 * 8);
   TSTRN(p, dest->slope_file, 32);
   TSTRN(p, dest->offset_file, 32);
   TSTRN(p, dest->activity, 20);
   p += byte_size;                                 /* skip filler_1 */
   TBYTE(p, &dest->filter);
   TBYTE(p, &dest->exposure);
   TBYTE(p, &dest->image_mode);
   TBYTE(p, &dest->gain);
   TFULL(p, &dest->range);
   TBYTE(p, &dest->reserved7);
   THALF(p, &dest->version);
   trin_sclk_typ(&p, &dest->start_sclk_img);
   trin_sclk_typ(&p, &dest->end_sclk_img);

   TSTRN(p, dest->ssi3_pkt.ra, 8);
   TSTRN(p, dest->ssi3_pkt.dec, 8);
   TSTRN(p, dest->ssi3_pkt.twist, 8);
   TSTRN(p, dest->ssi3_pkt.clock_angle, 8);
   TBYTE(p, &dest->ssi3_pkt.ccd_temp_fine);
   TBYTE(p, &dest->ssi3_pkt.ccd_temp_coarse);
   TBYTE(p, &dest->ssi3_pkt.pic_no);
   trin_word23_typ(&p, &dest->ssi3_pkt.word23);
   trin_word24_typ(&p, &dest->ssi3_pkt.word24);
   trin_word25_typ(&p, &dest->ssi3_pkt.word25);
   trin_word26_typ(&p, &dest->ssi3_pkt.word26);

/*
   TBYTE(p, &dest->ssi3_pkt.word23);
   TBYTE(p, &dest->ssi3_pkt.word24);
   TBYTE(p, &dest->ssi3_pkt.word25);
   TBYTE(p, &dest->ssi3_pkt.word26);

   THALF(p, &dest->ssi3_pkt.stan_hskping[0]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[1]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[2]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[3]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[4]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[5]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[6]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[7]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[8]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[9]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[10]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[11]);
   THALF(p, &dest->ssi3_pkt.stan_hskping[12]);

   zvtrans(half_trans, p, dest->ssi3_pkt.stan_hskping, 13);
   p += 13 * half_size;                                      skip reserved */

   p += 279 * byte_size;                                    /* skip reserved */
   zvtrans(full_trans, p, dest->histogram, 256);

   return SUCCESS;	
} /* end get_ssi_ph2_telem_hdr */

/****************************************************************************/
/* write the telemetry header out                                           */
/****************************************************************************/
int write_ssi_ph2_telem_hdr(unit, source)
int              unit;		/* must be an open file with COND BINARY set */
ssi_hdr_typ *source;
{
   unsigned char *buf, 
                 *p;
   int            recsize,i,status = SUCCESS,
                  nrecs;   /* number of records to get */

   init_trans_out(byte_trans,half_trans,full_trans);
   init_pixsizeb(unit,&byte_size,&half_size,&full_size);

   status = zvget(unit, "RECSIZE", &recsize, NULL);
   zvsignal(unit,status,TRUE);

   if (recsize == 1800) nrecs =1;
   else if (recsize == 1000) nrecs = 2;
   else return BAD_MALLOC;

   buf = (unsigned char *) malloc (recsize*nrecs);
   if (buf == NULL) 
     return BAD_MALLOC;
   p = buf;

   /* Fill in the buffer with elements from the structure.  Bytes are       */
   /* translated (although that's probably unnecessary, it's a good idea),  */
   /* while characters are just moved.		         	      	    */

   TOBYTE(&source->record_id, p);
   TOBYTE(&source->reserved1, p);
   TOSTRN(source->project, p, 10);
   TOSTRN(source->instrument, p, 6);
   TOHALF(&source->reserved2, p);
   TOHALF(&source->log_seq, p);
   trout_ert_typ(&source->first_ert, &p);	/* increments p */
   trout_ert_typ(&source->last_ert, &p);
   trout_sclk_typ(&source->first_sclk_rec, &p);
   trout_sclk_typ(&source->last_sclk_rec, &p);
   trout_scet_typ(&source->scet, &p);
   TOSTRN(source->mips_prd.os, p, 8);
   TOSTRN(source->mips_prd.reserved1, p, 26);
   TOSTRN(source->mips_prd.cpu, p, 8);
   TOSTRN(source->mips_prd.date, p, 11);
   TOSTRN(source->mips_prd.filler, p, 6);
   TOHALF(&source->rec_format, p);
   TOFULL(&source->reserved3A, p);
   TOBYTE(&source->boom, p);
   TOHALF(&source->miss_lines, p);
   TOHALF(&source->part_lines, p);
   TOHALF(&source->reserved4, p);
   TOHALF(&source->seq_break, p);
   TOHALF(&source->reserved5[0], p);
   TOHALF(&source->reserved5[1], p);
   TOHALF(&source->reserved5[2], p);
   TOHALF(&source->sfdus, p);
   TOSTRN(source->pic_no, p, 7);
/*   trout_ssi_lrs_p_typ(&source->ssi_lrs, &p);   */
   p += 12 * byte_size;
   trout_flg_comp_typ(&source->flags, &p);
   TOSTRN(source->mean_dn, p, 6);
   TOSTRN(source->trun_bits, p, 6);
   TOSTRN(source->trun_pixel, p, 6);
   TOSTRN(source->mean_if, p, 12);
   TOSTRN(source->entrop_avg, p, 7);
   TOSTRN(source->entropies, p, 15 * 7);
/*   trout_j2000_ascii_typ(&source->point_dir, &p); */
   TOSTRN(source->pointing, p, 3 * 8);
   TOSTRN(source->scale_fact, p, 2 * 8);
   TOSTRN(source->slope_file, p, 32);
   TOSTRN(source->offset_file, p, 32);
   TOSTRN(source->activity, p, 20);   
   p += byte_size;                           /* skip filler_1 */
   TOBYTE(&source->filter, p);
   TOBYTE(&source->exposure, p);
   TOBYTE(&source->image_mode, p);
   TOBYTE(&source->gain, p);
   TOFULL(&source->range, p);
   TOBYTE(&source->reserved7, p);
   TOHALF(&source->version, p);
   trout_sclk_typ(&source->start_sclk_img, &p);
   trout_sclk_typ(&source->end_sclk_img, &p);   

   TOSTRN(source->ssi3_pkt.ra, p, 8);
   TOSTRN(source->ssi3_pkt.dec, p, 8);
   TOSTRN(source->ssi3_pkt.twist, p, 8);
   TOSTRN(source->ssi3_pkt.clock_angle, p, 8);
   TOBYTE(&source->ssi3_pkt.ccd_temp_fine, p);
   TOBYTE(&source->ssi3_pkt.ccd_temp_coarse, p);
   TOBYTE(&source->ssi3_pkt.pic_no, p);
   trout_word23_typ(&source->ssi3_pkt.word23,&p);
   trout_word24_typ(&source->ssi3_pkt.word24,&p);
   trout_word25_typ(&source->ssi3_pkt.word25,&p);
   trout_word26_typ(&source->ssi3_pkt.word26,&p);

/* commented out -----
   TOBYTE(&source->ssi3_pkt.word23, p);
   TOBYTE(&source->ssi3_pkt.word24,P);
   TOBYTE(&source->ssi3_pkt.word25,P);
   TOBYTE(&source->ssi3_pkt.word26,P);

   zvtrans(half_trans,source->ssi3_pkt.stan_hskping, p , 13);
   p += 13 * half_size;                                  -----skip reserved */

   p += 279 * byte_size;                                    /* skip reserved */
   zvtrans(full_trans, source->histogram, p, 256);

   /* Write out the entire telemetry header, by writing the number of        */
   /* records indicated by the data format.                                  */
   for (i=0; i<nrecs; i++) {     
     status = zvwrit(unit,&buf[i*recsize],"LINE",1+i,NULL);
     if (status != SUCCESS) return status;
   }

   return SUCCESS;
} /* end write_ssi_ph2_telem_hdr */

/*****************************************************************************/
/* This routine translates one line of binary image line prefix into native  */
/* format (DEFAULT_HOST) and writes it out to the file specified by ounit.   */
/*****************************************************************************/
ROUTINE int write_ssi_ph2_prefix(ounit,line,source)
int           ounit,
              line;
ssi_prefix_typ *source;
{
  unsigned char  buf[SSI_PRFX_LEN],
		*p = buf;
  UBYTE          bits_byte = 0;      
  UINT           bits_full = 0;
  int            status = SUCCESS,
                 nlb,          /* # lines of binary header */
                 nbb;          /* # bytes of binary prefix data */

 status =  zvget(ounit, "NLB", &nlb, "NBB", &nbb, NULL);
 if (status != SUCCESS) return status;
 if (nbb > SSI_PRFX_LEN) return BAD_MALLOC;

 init_trans_out(byte_trans,half_trans,full_trans);
 init_pixsizeb(ounit,&byte_size,&half_size,&full_size);
   
 TOBYTE(&source->record_id, p);
 TOBYTE(&source->reserved1A, p);
 TOHALF(&source->reserved1B, p);

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

 TOSTRN(source->mips_prd.os, p, 8);
 TOSTRN(source->mips_prd.reserved1, p, 26);
 TOSTRN(source->mips_prd.cpu, p, 8);
 TOSTRN(source->mips_prd.date, p, 11);
 TOSTRN(source->mips_prd.filler, p, 6);

 TOHALF(&source->record_format, p) ;
 TOBYTE(&source->input_type, p);

 /* input source */
 if (source->source.SFDU)  bits_byte  |= 0x01;
 if (source->source.WBDL)  bits_byte  |= 0x02;
 if (source->source.SDR)   bits_byte  |= 0x04;
 if (source->source.IDR)   bits_byte  |= 0x08;
 if (source->source.EDR)   bits_byte  |= 0x10;
 if (source->source.MIPS)  bits_byte  |= 0x20;
 if (source->source.APB)   bits_byte  |= 0x40;
 TOBYTE(&bits_byte, p);

 TOBYTE(&source->reserved3A, p);
 TOBYTE(&source->reserved3B, p);
 zvtrans(byte_trans, &source->reserved3C[0], p, 12);
 p += 12 * byte_size;
 
 TOHALF(&source->reserved3D, p);
 TOHALF(&source->reserved3E, p);

 bits_full |= (source->trun_bits.block0 & 0x03);
 bits_full |= (source->trun_bits.block1 & 0x03) << 2;
 bits_full |= (source->trun_bits.block2 & 0x03) << 4;
 bits_full |= (source->trun_bits.block3 & 0x03) << 6;
 bits_full |= (source->trun_bits.block4 & 0x03) << 8;
 bits_full |= (source->trun_bits.block5 & 0x03) << 10;
 bits_full |= (source->trun_bits.block6 & 0x03) << 12;
 bits_full |= (source->trun_bits.block7 & 0x03) << 14;
 bits_full |= (source->trun_bits.block8 & 0x03) << 16;
 bits_full |= (source->trun_bits.block9 & 0x03) << 18;
 bits_full |= (source->trun_bits.block10 & 0x03) << 20;
 bits_full |= (source->trun_bits.block11 & 0x03) << 22;
 bits_full |= (source->trun_bits.block12 & 0x03) << 24;
 TOFULL(&bits_full, p);

 TOHALF(&source->trun_pix, p);
 TOHALF(&source->version, p);
 TOHALF(&source->reserved4, p);
 TOBYTE(&source->dsn_id, p);
 TOHALF(&source->line_no, p);
 TOBYTE(&source->reserved5, p);
 TOHALF(&source->ss1, p);
 TOHALF(&source->es1, p);
 TOHALF(&source->ss2, p);
 TOHALF(&source->es2, p);
 bits_byte = 0;
 bits_byte |= (source->line_con.part_pkts & 0x0F);
 bits_byte |= (source->line_con.full_pkts & 0x0F) << 4;
 
 TOBYTE(&bits_byte, p);

 TOBYTE(&source->apid, p);
 TOFULL(&source->pkt_seq, p);
 TOHALF(&source->pkt_pix_srt, p);
 TOHALF(&source->strt_pix, p);
 TOHALF(&source->stop_pix, p);
 TOHALF(&source->rct.year, p);
 TOHALF(&source->rct.day, p);
 TOBYTE(&source->rct.hour, p);
 TOBYTE(&source->rct.minute, p);
 TOBYTE(&source->rct.second, p);
 TOHALF(&source->rct.msecond, p);
 TOBYTE(&source->decomp_stat, p);

 TOSTRN(source->comp_ratio, p, 6);

 zvtrans(byte_trans, &source->reserved6[0], p, 47);
 p += 47 * byte_size;

 status = zvwrit(ounit, buf, "LINE", line+nlb, "NSAMPS", nbb, NULL);

 return status;
} /* end write_ssi_ph2_prefix */

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

ROUTINE static void trin_flg_comp_typ(from, to)
unsigned char **from;
flag_comp_typ *to;
{
   unsigned short int bits;

   zvtrans(half_trans, *from, &bits, 1);    /* byte-swap so bit masks work */
   *from += half_size;
   to->barc_comp = bits & 0x01;
   to->barc_mode = (bits >> 1) & 0x01;
   to->expo = (bits >> 2) & 0x01;
   to->flood = (bits >> 3) & 0x01;
   to->blem = (bits >> 4) & 0x01;
   to->clock = (bits >> 5) & 0x01;
   to->ict_comp = (bits >> 6) & 0x01;
   to->huff_comp = (bits >> 7) & 0x01;
   /* to->filler is not set */
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

ROUTINE static void trout_flg_comp_typ(from, to)
flag_comp_typ *from;
unsigned char **to;
{
   unsigned short int bits = 0;
   if (from->barc_comp)     bits |= 0x01;
   if (from->barc_mode) bits |= 0x02;
   if (from->expo)     bits |= 0x04;
   if (from->flood)    bits |= 0x08;
   if (from->blem)     bits |= 0x10;
   if (from->clock)    bits |= 0x20;
   if (from->ict_comp)    bits |= 0x40;
   if (from->huff_comp)    bits |= 0x80;
   zvtrans(half_trans, &bits, *to, 1);    /* byte-swap so bit masks work */
   *to += half_size;
   /* from->filler is not set */
}
/****************************************************************************/
/* This routine sets up translation buffers for output, converting from the */
/* machine's native representation into the host representation.            */
/****************************************************************************/
ROUTINE static void init_trans_out(byte_tr,half_tr,full_tr)
int *byte_tr,
    *half_tr,
    *full_tr;
{
   char intfmt[12],realfmt[12];
   int status;

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
/* file.								    */
/****************************************************************************/
ROUTINE static void init_pixsizeb(unit,byte_sz,half_sz,full_sz)
int unit,
    *byte_sz,
    *half_sz,
    *full_sz;
{
   int status;
   char ph2_aline[80];
 
   status = zvpixsizeb(byte_sz, "BYTE", unit);
   zvsignal(unit,status,TRUE);
   if (byte_sz == 0) {
     sprintf(ph2_aline,
     "init_pixsizeb> error in byte pixel size determination, status %d",byte_sz);
     zvmessage(ph2_aline,0);
     zabend();
   }

   status = zvpixsizeb(half_sz, "HALF", unit);
   zvsignal(unit,status,TRUE);
   if (half_sz == 0) {
     sprintf(ph2_aline,
    "init_pixsizeb> error in halfword pixel size determination, status %d",half_sz);
     zvmessage(ph2_aline,0);
     zabend();
   }

   status = zvpixsizeb(full_sz, "FULL", unit);
   zvsignal(unit,status,TRUE);
   if (full_sz == 0) {
     sprintf(ph2_aline,
    "init_pixsizeb> error in fullword pixel size determination, status %d",full_sz);
     zvmessage(ph2_aline,0);
     zabend();
   }
} /* end init_pixsizeb */

ROUTINE static void trin_word23_typ(from, to)
unsigned char **from;
word23_typ *to;
{
   unsigned char bits;

   zvtrans(byte_trans, *from, &bits, 1);    /* byte-swap so bit masks work */
   *from += byte_size;
   to->exposure_no = bits & 0x1F;  /* bits 0-4, bits 0 is the rightmost(lsb) */
   to->cmnd_gain = (bits >> 5) & 0x03;   /*   bits 5-6   */
   to->flood = (bits >> 7) & 0x01;   /*  bit 7  */
}

ROUTINE static void trin_word24_typ(from, to)
unsigned char **from;
word24_typ *to;
{
   unsigned char bits;

   zvtrans(byte_trans, *from, &bits, 1);    /* byte-swap so bit masks work */
   *from += byte_size;
   to->cmnd_filter_pos = bits & 0x07;         /*  bits 0-2    */
   to->cmnd_filter_step = (bits >> 3) & 0x01; /*  bits 3      */
   to->cmnd_blemish = (bits >> 4) & 0x01;     /*  bits 4      */
   to->cmnd_expo_mode = (bits >>5) & 0x01;    /*  bits 5      */
   to->cmnd_expo_cycle = (bits>>6) & 0x01;    /*  bits 6      */
   to->skip = bits >> 7; /*  bit 7  */
}

ROUTINE static void trin_word25_typ(from, to)
unsigned char **from;
word25_typ *to;
{
   unsigned char bits;

   zvtrans(byte_trans, *from, &bits, 1);    /* byte-swap so bit masks work */
   *from += byte_size;
   to->state_used = bits & 0x03;        /*   bits 0-1   */
   to->comp_stat = (bits >>2) & 0x03;    /*   bits  2-3  */
   to->long_expo = (bits >> 4) & 0x01;      /*   bits  4  */
   to->img_mode = (bits >> 5) & 0x07;      /*   bits  5-7  */
}

ROUTINE static void trin_word26_typ(from, to)
unsigned char **from;
word26_typ *to;
{
   unsigned char bits;

   zvtrans(byte_trans, *from, &bits, 1);    /* byte-swap so bit masks work */
   *from += byte_size;
   to->odd_parity = bits & 0x01;         /*   bit 0        */
   to->filter = (bits >> 1) & 0x07;      /*   bits   1-3   */
   to->blemish = (bits >> 4) & 0x01;     /*   bits   4     */
   to->watchdog = (bits >> 5) & 0x01;    /*   bits   5     */
   to->par_clock = (bits >> 6) & 0x01;   /*   bits   6     */
   to->mem_write = (bits >> 7) & 0x01;   /*   bits   7     */
}

ROUTINE static void trout_word23_typ(from, to)
word23_typ *from;
unsigned char **to;
{
   unsigned char bits = 0;
   bits |= from->flood;                      /*   bits 7     */
   bits <<= 2;
   bits |= from->cmnd_gain;                  /*   bits 5-6   */
   bits <<= 5;
   bits |= from->exposure_no;                /*   bits 0-4   */
   zvtrans(byte_trans, &bits, *to, 1);    /* byte-swap so bit masks work */
   *to += byte_size;
   /* from->filler is not set */
}

ROUTINE static void trout_word24_typ(from, to)
word24_typ *from;
unsigned char **to;
{
   unsigned char bits = 0;
   bits |= from->cmnd_expo_cycle;                       /*  bits 6    */
   bits <<= 1;
   bits |= from->cmnd_expo_mode;                        /*  bits 5    */
   bits <<= 1;
   bits |= from->cmnd_blemish;                          /*  bits 4    */
   bits <<= 1;
   bits |= from->cmnd_filter_step;                      /*  bits 3    */
   bits <<= 3;
   bits |= from->cmnd_filter_pos;                       /*  bits 0-2  */
   zvtrans(byte_trans, &bits, *to, 1);    /* byte-swap so bit masks work */
   *to += byte_size;
   /* from->filler is not set */
}

ROUTINE static void trout_word25_typ(from, to)
word25_typ *from;
unsigned char **to;
{
   unsigned char bits = 0;
   bits |= from->img_mode;                     /*   bits 5-6   */
   bits <<= 1;
   bits |= from->long_expo;                    /*   bits 4     */
   bits <<= 2;
   bits |= from->comp_stat;                    /*   bits 2-3   */
   bits <<= 2;
   bits |= from->state_used;                   /*   bits 0-1   */

   zvtrans(byte_trans, &bits, *to, 1);    /* byte-swap so bit masks work */
   *to += byte_size;
   /* from->filler is not set */
}

ROUTINE static void trout_word26_typ(from, to)
word26_typ *from;
unsigned char **to;
{
   unsigned char bits = 0;
   bits |= from->mem_write;                     /*  bits 7     */
   bits <<= 1;
   bits |= from->par_clock;                     /*  bits 6     */
   bits <<= 1;
   bits |= from->watchdog;                      /*  bits 5     */
   bits <<= 1;
   bits |= from->blemish;                       /*  bits 4     */
   bits <<= 3;
   bits |= from->filter;                        /*  bits 1-3   */
   bits <<= 1;
   bits |= from->odd_parity;                    /*  bits 0     */

   zvtrans(byte_trans, &bits, *to, 1);     /*  byte-swap so bit masks work */
   *to += byte_size;
   /* from->filler is not set */
}
/* end module */

