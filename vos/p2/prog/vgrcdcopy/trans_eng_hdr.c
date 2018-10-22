/*  This routine will translate the Engr. Data Binary Hdr (found in the	*/
/*  Voyager PDS CD-ROMs) and convert it to s structure that is in the 	*/
/*  native machine format.  This is necessary so that vgrcdlabgen()	*/
/*  will correctly interpret the information to create the VGR VICAR 	*/
/*  label.  								*/
/*  Myche McAuley  6/95							*/
/*									*/
#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "zvproto.h"

/*#ifdef __unix		wrong flag! */
#if SUN_SOLARIS_ARCH
#include "uengrhdr.h"
#else
#include "engrhdr.h"
#endif


int trans_eng_hdr ( struct edrhdr* ptr, struct edrhdr* transed,
		    int byte_size, int half_size, int byte_trans[12], int half_trans[12])  {

  unsigned char *curloc;

  curloc = (unsigned char *) ptr;

  zvtrans ( byte_trans, curloc, &(transed->recid), 1); curloc += byte_size;
  zvtrans ( byte_trans, curloc, &(transed->fileno), 1); curloc += byte_size;
  zvtrans ( half_trans, curloc, &(transed->phys_seq_no), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->log_seq_no), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->ert), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->ert_min), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->ert_msec), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->lert), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->lert_min), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->lert_msec), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->fds_mod16), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->fds_mod60), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->fds_line), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->lfds_mod16), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->lfds_mod60), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->lfds_line), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->scet), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->scet_min), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->scet_msec), 1); curloc += half_size;
  zvtrans ( byte_trans, curloc, &(transed->system_version), 32); curloc += byte_size * 32;
  zvtrans ( half_trans, curloc, &(transed->gcf[1].sync_code_msb), 1); curloc += half_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[1].source_station), 1); curloc += byte_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[1].sync_code_lsb), 1); curloc += byte_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[1].block_format), 1); curloc += byte_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[1].destination_code), 1); curloc += byte_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[1].gddudt.word), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[1].s1), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[1].time_lsb), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[1].s2), 1); curloc += half_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[1].msec_clock), 1); curloc += byte_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[1].serial_number), 1); curloc += byte_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[1].dsn.word), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[1].esc), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[2].sync_code_msb), 1); curloc += half_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[2].source_station), 1); curloc += byte_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[2].sync_code_lsb), 1); curloc += byte_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[2].block_format), 1); curloc += byte_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[2].destination_code), 1); curloc += byte_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[2].gddudt.word), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[2].s1), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[2].time_lsb), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[2].s2), 1); curloc += half_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[2].msec_clock), 1); curloc += byte_size;
  zvtrans ( byte_trans, curloc, &(transed->gcf[2].serial_number), 1); curloc += byte_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[2].dsn.word), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->gcf[2].esc), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->irt), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->irt_min), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->irt_msec), 1); curloc += half_size;
  zvtrans ( byte_trans, curloc, &(transed->tlm_mode), 1); curloc += byte_size;
  zvtrans ( byte_trans, curloc, &(transed->unused), 1); curloc += byte_size;
  zvtrans ( half_trans, curloc, &(transed->sds.unused1), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.fid), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.system_noise_temp_min), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.system_noise_temp_max), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.symbol_snr_min), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.symbol_snr_max), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.agc_min), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.agc_max), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.pn_errs), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.fds_count_errs), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.sync_pars), 3); curloc += half_size * 3;
  zvtrans ( half_trans, curloc, &(transed->sds.nlines), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.nfull), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.npartial), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.nbadrec), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.nlog_seq_breaks), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.sort_par), 4); curloc += half_size * 4;
  zvtrans ( half_trans, curloc, &(transed->sds.nmf_from_idr), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.nmf_from_wbdl), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.nmf_from_sdr), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.nmf_missing), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->sds.unused2), 1); curloc += half_size;
  zvtrans ( byte_trans, curloc, &(transed->sds.picno), 10); curloc += byte_size * 10;
  zvtrans ( byte_trans, curloc, &(transed->sds.target_body), 10); curloc += byte_size * 10;
  zvtrans ( half_trans, curloc, &(transed->sds.input.source), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.subword1), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.subword2), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.subword3), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.picture_count), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.parword_a), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.parword_a_ind), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.parword_a_ptr), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.parword_b), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.parword_b_ptr), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.parword_c), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.parword_c_ptr), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.parword_d), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.parword_d_ind), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->subcom.parword_d_ptr), 1); curloc += half_size;
  zvtrans ( byte_trans, curloc, &(transed->subcom.na_sample1), 10); curloc += byte_size * 10;
  zvtrans ( half_trans, curloc, &(transed->subcom.word20.word), 1); curloc += half_size;
  zvtrans ( half_trans, curloc, &(transed->iss_engineering), 5); curloc += half_size * 5;
  zvtrans ( byte_trans, curloc, &(transed->nept_byte), 1); curloc += byte_size;
  zvtrans ( byte_trans, curloc, &(transed->unused5), 5); curloc += byte_size * 5;

  return 1;

}
