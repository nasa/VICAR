
#ifndef MIPS_GLL_DECOMPRESSION_INCLUDED
#define MIPS_GLL_DECOMPRESSION_INCLUDED 1

/*****************************************************************************
 *  The purpose of this file is for defining structures the structure that   *
 *  will be used to stored decompression results.                            *
 *                                                                           *
 *  Delivery Date     Programmer    Comment                                  *
 *  -------------     ----------    ---------------------------------------- *
 *    07-05-94           DDK        Original Delivery                        *
 *                                                                           *
 ****************************************************************************/


typedef struct
    {
         int ndcmp;
         int tndcmp;
         int tntb;
         int tndcmp50;
         int tntb50;
	 int ncalls;
         unsigned char ntbpp[13];
         unsigned char mode_count[2];
         float avgntb;
         float avgntp;
         short int barc_complete;
         int bits_read;
         short int ict_complete;
         short int good_blocks;
         short int sync_mark;
         short int slice_no;
         short int full_pkt;
         short int partial_pkt;
         float max_bytes_read;
         float min_bytes_read;
         float max_bytes_lossless;
         float min_bytes_lossless;
         float tot_bytes_read;
         float tot_bytes_lossless;
   }
    comp_stat_typ;

#endif

