#include <stdio.h>
#include <stdlib.h>
#include "xvmaininc.h"   
#include "rts_typedefs.h" 
#include "gll_rts_main.h"
/* #include "gll_lrs.h" */
#include "gll_ph2_ssi_edr.h"
#include "zvproto.h"
/*******************************************************************************

Purpose:	This subroutine gets the GLL Phase II SSI Binary Line PREFIX 
                information 
	
Written:	August 10,  1994 

Language:	C  (MSTP Portable)

Written by:	F. Moss

*******************************************************************************/


/*  RGD's macros to reduce typing   */

#define TBYTE(from, to)  zvtrans(byte_conv,(from),(to),1); (from) += ph2_byte_size;
#define THALF(from, to)  zvtrans(half_conv,(from),(to),1); (from) += ph2_half_size;
#define TFULL(from, to)  zvtrans(full_conv,(from),(to),1); (from) += ph2_full_size;
#define TSTRN(from, to, n) zmove((from), (to), (n));  (from) += (n) ;


get_ssi_ph2_prefix (iu, lin, dest)  
int            iu ;
int            lin ;  
ssi_prefix_typ *dest ;

{
 int     ind ;               /*  Status indicator  */
 unsigned  char  *sorc ;
 unsigned  char  *pt ;
 int  byte_conv[12], ph2_byte_size ;
 int  half_conv[12], ph2_half_size ;
 int  full_conv[12], ph2_full_size ;
 UBYTE   bits_byte,      
         bits_byte1,      
         bits_byte2,      
         bits_byte3;      
 UWORD   bits_half ;  
 UINT    bits_full ;
 static  int   nlb ; 
 static  int   nbb ;

 zvget(iu, "NLB", &nlb, "NBB", &nbb , NULL) ;
 pt = (unsigned char *) malloc (nbb) ;         
 zvread(iu, pt, "LINE", lin+nlb, "NSAMPS", nbb, NULL) ;
 
 sorc = pt ; 

      /*  set up translation buffers  */

 ind = zvtrans_inb(byte_conv, "BYTE", "BYTE", iu) ;
 ind = zvtrans_inb(half_conv, "HALF", "HALF", iu) ;
 ind = zvtrans_inb(full_conv, "FULL", "FULL", iu) ;

 ind = zvpixsizeb( &ph2_byte_size, "BYTE", iu) ;
 ind = zvpixsizeb( &ph2_half_size, "HALF", iu) ;
 ind = zvpixsizeb( &ph2_full_size, "FULL", iu) ;
   
 TBYTE(sorc, &dest->record_id) ;
 TBYTE(sorc, &dest->reserved1A) ;
 THALF(sorc, &dest->reserved1B) ;

 THALF(sorc, &dest->log_seq) ;

 THALF(sorc, &dest->ert.year) ;
 THALF(sorc, &dest->ert.day) ;
 TBYTE(sorc, &dest->ert.hour) ;
 TBYTE(sorc, &dest->ert.minute) ;
 TBYTE(sorc, &dest->ert.second) ;
 THALF(sorc, &dest->ert.msecond) ;

 TFULL(sorc, &dest->sclk.rim) ;
 TBYTE(sorc, &dest->sclk.mod91) ;
 TBYTE(sorc, &dest->sclk.mod10) ;
 TBYTE(sorc, &dest->sclk.mod8) ;

 TSTRN(sorc, dest->mips_prd.os, 8);
 TSTRN(sorc, dest->mips_prd.reserved1, 26);
 TSTRN(sorc, dest->mips_prd.cpu, 8);
 TSTRN(sorc, dest->mips_prd.date, 11);
 TSTRN(sorc, dest->mips_prd.filler, 6);

 THALF(sorc, &dest->record_format)  ;

 TBYTE(sorc, &dest->input_type) ;
 TBYTE(sorc, &bits_byte ) ;
 dest->source.SFDU =  bits_byte           &  01 ;
 dest->source.WBDL = (bits_byte >> 1)     &  01 ;
 dest->source.SDR =  (bits_byte >> 2)     &  01 ;
 dest->source.IDR =  (bits_byte >> 3)     &  01 ;
 dest->source.EDR =  (bits_byte >> 4)     &  01 ;
 dest->source.MIPS = (bits_byte >> 5)     &  01 ;
 dest->source.APB =  (bits_byte >> 6)     &  01 ;

 TBYTE(sorc, &dest->reserved3A) ;
 TBYTE(sorc, &dest->reserved3B) ;
 zvtrans(byte_conv, sorc, &dest->reserved3C[0], 12) ;
 sorc += 12 * ph2_byte_size ;
 
 THALF(sorc, &dest->reserved3D) ;
 THALF(sorc, &dest->reserved3E) ;

 TFULL(sorc, &bits_full) ;
 dest->trun_bits.block0   =    bits_full          &   0x03 ;
 dest->trun_bits.block1   =   (bits_full >>  2)   &   0x03 ;
 dest->trun_bits.block2   =   (bits_full >>  4)   &   0x03 ;
 dest->trun_bits.block3   =   (bits_full >>  6)   &   0x03 ;
 dest->trun_bits.block4   =   (bits_full >>  8)   &   0x03 ;
 dest->trun_bits.block5   =   (bits_full >> 10)   &   0x03 ;
 dest->trun_bits.block6   =   (bits_full >> 12)   &   0x03 ;
 dest->trun_bits.block7   =   (bits_full >> 14)   &   0x03 ;
 dest->trun_bits.block8   =   (bits_full >> 16)   &   0x03 ;
 dest->trun_bits.block9   =   (bits_full >> 18)   &   0x03 ;
 dest->trun_bits.block10  =   (bits_full >> 20)   &   0x03 ;
 dest->trun_bits.block11  =   (bits_full >> 22)   &   0x03 ;
 dest->trun_bits.block12  =   (bits_full >> 24)   &   0x03 ;
   
 THALF(sorc, &dest->trun_pix) ;
 THALF(sorc, &dest->version) ;
 THALF(sorc, &dest->reserved4) ;
 TBYTE(sorc, &dest->dsn_id) ;
 THALF(sorc, &dest->line_no) ;
 TBYTE(sorc, &dest->reserved5) ;
 THALF(sorc, &dest->ss1) ;
 THALF(sorc, &dest->es1) ;
 THALF(sorc, &dest->ss2) ;
 THALF(sorc, &dest->es2) ;

 TBYTE(sorc, &bits_byte) ;
 dest->line_con.part_pkts =    bits_byte          &   0x0F ; 
 dest->line_con.full_pkts =   (bits_byte >>  4)   &   0x0F ;
 

 TBYTE(sorc, &dest->apid) ;
 TFULL(sorc, &dest->pkt_seq) ;
 THALF(sorc, &dest->pkt_pix_srt) ;
 THALF(sorc, &dest->strt_pix) ;
 THALF(sorc, &dest->stop_pix) ;
 THALF(sorc, &dest->rct.year) ;
 THALF(sorc, &dest->rct.day) ;
 TBYTE(sorc, &dest->rct.hour) ;
 TBYTE(sorc, &dest->rct.minute) ;
 TBYTE(sorc, &dest->rct.second) ;
 THALF(sorc, &dest->rct.msecond) ;
 TBYTE(sorc, &dest->decomp_stat) ;

 zvtrans(byte_conv, sorc, &dest->comp_ratio, 6) ;

 free (pt) ;  

}        /*    End of Subroutine       */
