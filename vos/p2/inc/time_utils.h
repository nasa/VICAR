#ifndef __TIME_UTILS_H
#define __TIME_UTILS_H

int julian_date(int year, int month, int day, int hour, int minute, double second, double *jd_adr);
int parse_iso_time_string(char *T, int *year_adr, int *month_adr, int *day_adr, int *hour_adr, int *minute_adr, double *second_adr);
int compose_iso_time_string(int year, int month, int day, int hour, int minute, double second, char **T_adr);
int compose_dut1_table_time_string(int year, int month, int day, int hour, int minute, int second, char **T_adr);
int is_leap_year(int year);
int parse_yyyymmdd(int yyyymmdd, int *year_adr, int *month_adr, int *day_adr);
int compose_yyyymmdd(int year, int month, int day, int *yyyymmdd_adr);
int standardize_time_components(int y0, int t0, int d0, int h0, int m0, double s0, int *y1_adr, int *t1_adr, int *d1_adr, int *h1_adr, int *m1_adr, double *s1_adr);
int calendar_increment(int yyyymmdd_old, int n_days, int *yyyymmdd_new_adr);
int month_string_to_month(char *month_str, int *month_adr);

#endif
