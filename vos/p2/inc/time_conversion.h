#ifndef __TIME_CONVERSION_H
#define __TIME_CONVERSION_H

/* From UTC_ISO_TIME_STRING */
int utc_iso_time_string_to_si(char *UTC_string, double *SI_adr);
int utc_iso_time_string_to_tt_and_ut1(char *UTC_string, double Delta_UT1, double *TT_adr, double *UT1_adr);
int utc_iso_time_string_to_tt_iso_time_string(char *UTC_string, char **TT_string_adr);
int utc_iso_time_string_to_ut1_iso_time_string(char *UTC_string, double Delta_UT1, char **UT1_string_adr);
int utc_iso_time_string_to_leapsec_count(char *UTC_string, double *leapsec_count_adr);
int utc_iso_time_string_to_dut1tbl_time_string(char *UTC_string, char **DUT1_table_string_adr);

/* From UTC time components */
int utc_time_components_to_leapsec_count(int y0, int t0, int d0, int h0, int m0, double s0, double *leapsec_count_adr);
int utc_time_components_to_leap_table_index(int y0, int t0, int d0, int h0, int m0, double s0, int *k_adr);

/* From ACS */
int acs_to_tt_and_ut1(double ACS, double Delta_UT1, double *TT_adr, double *UT1_adr);
int acs_to_tt(double ACS_time, double *TT_adr);
int acs_to_ut1(double ACS_time, double Delta_UT1, double *UT1_adr);
int acs_to_utc_iso_time_string(double ACS, char **UTC_string_adr);

/* From SI */
int si_to_tt_and_ut1(double SI, double Delta_UT1, double *TT_adr, double *UT1_adr);
int si_to_tt(double SI, double *TT_adr);
int si_to_ut1(double SI, double Delta_UT1, double *UT1_adr);
int si_to_utc_iso_time_string(double SI, char **UTC_string_adr);

/* From TT_ISO_TIME_STRING */
int tt_iso_time_string_to_tt(char *TT_string, double *TT_adr);
int tt_iso_time_string_to_tdb(char *TT_string, double *TDB_adr);

/* From TT */
int tt_to_utc_iso_time_string(double UTC, char **UTC_string_adr);
int tt_to_si(double TT, double *SI_adr);
int tt_to_ut1(double TT, double Delta_UT1, double *UT1_adr);
int tt_to_tdb(double TT, double *TDB_adr);

/* Helper functions */
int lookup_delta_ut1(char *fname, int yyyymmdd, double *Delta_UT1_adr);
int initialize_leap_second_table(char *filename);

#endif
