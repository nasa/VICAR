#include <stdio.h>
#include <stdlib.h>
#include "xvmaininc.h"   
#include "gll_main.h"
#include "gll_lrs.h"
#include "gll_ssi_edr.h"
#include "zvproto.h"
/*******************************************************************************

Purpose:	This subroutine gets the GLL SSI Binary PREFIX information 
	
Written:	Sept 16,  1992 

Language:	C  (MSTP Portable)

Written by:	Wen-Piao  Lee  

*******************************************************************************/


/*  RGD's macros to reduce typing   */

#define TBYTE(from, to)  zvtrans(byte_conv,(from),(to),1); (from) += byte_size;
#define THALF(from, to)  zvtrans(half_conv,(from),(to),1); (from) += half_size;
#define TFULL(from, to)  zvtrans(full_conv,(from),(to),1); (from) += full_size;
#define TSTRN(from, to, n) zmove((from), (to), (n));  (from) += (n) ;


get_ssi_prefix (iu, lin, dest)  
int            iu ;
int            lin ;  
ssi_lhdr_typ   *dest ;

{
 int     ind ;               /*  Status indicator  */
 unsigned  char  *sorc ;
 unsigned  char  *pt ;
 int  byte_conv[12], byte_size ;
 int  half_conv[12], half_size ;
 int  full_conv[12], full_size ;
 UBYTE   bits_byte ;      
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

 ind = zvpixsizeb( &byte_size, "BYTE", iu) ;
 ind = zvpixsizeb( &half_size, "HALF", iu) ;
 ind = zvpixsizeb( &full_size, "FULL", iu) ;
   
 TBYTE(sorc, &dest->rec_id) ;
 TBYTE(sorc, &dest->file_no) ;
 THALF(sorc, &dest->phy_seq) ;

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

 TSTRN(sorc, dest->mpr, 59) ;
 THALF(sorc, &dest->fid)  ;

 TBYTE(sorc, &dest->input.type) ;
 TBYTE(sorc, &bits_byte ) ;
 dest->input.SFDU =  bits_byte           &  01 ;
 dest->input.WBDL = (bits_byte >> 1)     &  01 ;
 dest->input.SDR =  (bits_byte >> 2)     &  01 ;
 dest->input.IDR =  (bits_byte >> 3)     &  01 ;
 dest->input.EDR =  (bits_byte >> 4)     &  01 ;
 dest->input.RT =   (bits_byte >> 5)     &  01 ;
 dest->input.APB =  (bits_byte >> 6)     &  01 ;

 TBYTE(sorc, &dest->allowed_sync_err) ;
 TBYTE(sorc, &dest->sync_err) ;
 zvtrans(byte_conv, sorc, &dest->ssi_lrs[0], 12) ;
 sorc += 12 * byte_size ;
 
 THALF(sorc, &dest->last_pix) ;
 THALF(sorc, &bits_half) ;
 dest->sync_stat.error =   bits_half & 0x3FFF ;
 dest->sync_stat.status = (bits_half >> 14)  &  0x03  ;

 TFULL(sorc, &bits_full) ;
 dest->truncation.blk0   =    bits_full          &   0x03 ;
 dest->truncation.blk1   =   (bits_full >>  2)   &   0x03 ;
 dest->truncation.blk2   =   (bits_full >>  4)   &   0x03 ;
 dest->truncation.blk3   =   (bits_full >>  6)   &   0x03 ;
 dest->truncation.blk4   =   (bits_full >>  8)   &   0x03 ;
 dest->truncation.blk5   =   (bits_full >> 10)   &   0x03 ;
 dest->truncation.blk6   =   (bits_full >> 12)   &   0x03 ;
 dest->truncation.blk7   =   (bits_full >> 14)   &   0x03 ;
 dest->truncation.blk8   =   (bits_full >> 16)   &   0x03 ;
 dest->truncation.blk9   =   (bits_full >> 18)   &   0x03 ;
 dest->truncation.blk10  =   (bits_full >> 20)   &   0x03 ;
 dest->truncation.blk11  =   (bits_full >> 22)   &   0x03 ;
 dest->truncation.blk12  =   (bits_full >> 24)   &   0x03 ;
   
 THALF(sorc, &dest->truncated) ;
 THALF(sorc, &dest->version) ;
 THALF(sorc, &dest->ssnr) ;
 TBYTE(sorc, &dest->dsn_id) ;
 THALF(sorc, &dest->image_line) ;
 TBYTE(sorc, &dest->rs_overflow) ;

 free (pt) ;  

}        /*    End of Subroutine       */
