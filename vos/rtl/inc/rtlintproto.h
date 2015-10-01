/* These are prototypes that are used internally by the RTL library.	*/
/* These shouldn't be called externally from the RTL library.		*/
#ifndef RTLINTPROTO_H
#define RTLINTPROTO_H

#include "defines.h"
#include "declares.h"
#include <strings.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include "zvproto.h"
#if RTL_USE_TAE
/* This gets defined again in taeconf.inp */
#undef MAX
#include "taeconf.inp"
#include "parblk.inc"
#include "taeextproto.h"
#else
struct PARBLK;
struct LARGE_PARBLK;
struct VARIABLE;
#endif
void *v2_array_io_location(int unit);
void v2_deassign_value_block(int unit);
void v2_full_init_value_tbl(int unit);
int v2_evaluate_label_item(char *value, int vallen, char *format,int *length,
			int *nelements,int *level,int *strlength);
int v2_obtain_label_values(char *value, int vallen, char *valarray, 
			   int nelements, int maxlength, char *format);
void v2_move(char *to, char *from, int len);
int v2_create_label_item(char *key, char *valarray, int maxlen, 
			 int nelements, char *format, int level, char **text);
int v2_cut_label_string(char *start, char *stop, char *label_item);
int v2_insert_label_string(int unit, char *loc, char *newstr);
int v2_add_complex_label_item(int unit, char *newvalue, char *value, 
			      int vallen, char *start, char *stop,
			      int element, int nelements_to_add, int newlength,
			      int level, char *format, char *key);
int v2_add_str_current_table(char *a,int kopt,VALUE_TABLE *currval_table,
			  VALUE_TABLE *def_table);
char *v2_find_entry(char *s, char *key, char **value, int *vallen,char **place);
char *v2_string(char *s, int len);
void v2_add_label_size_item(char *ps,int lblsize); 
void v2_set_eol(char *p);
void v2_clear_eol(char *p);
int v2_close_os_file(int unit);
int v2_set32BitInt(unsigned int len, unsigned char *ptr);
int v2_set_basic_label_params(int unit);
int v2_substr(char *c_string,char *sub);
int v2_delete_file(int unit);
void v2_close_unit(int unit);
void v2_final_cleanup(int unit);
int v2_deactivate_a_unit(int unit);
int v2_read_in_labels(int unit,char **p, int *size);
int v2_lazy_search(int unit);
int v2_move_to_output_label(int unit,char *buf, int size, char *recbuf);
char *v2_place_cut(char *p, int size);
int v2_write_rec(struct bufstate *state, char *buffer, int rec, int startbyte, 
	      int nbytes, struct trans *trans);
int v2_activate_a_unit(int *unit, int instance, char *name);
int v2_est_primary_environ(int unit);
int v2_io_complete_check(struct devstate *devstate);
int v2_flush_buffer(struct bufstate *state);
int v2_finish_file(int unit);
int v2_free_mapped_sect(struct arraystate *arraystate);
int v2_get_one_string_parm(char *name, int number, char **value);
int v2_basic_precheck(int unit);
int v2_basic_preprocess(int unit);
int v2_basic_close(int unit);
int v2_basic_read_rec(int unit, struct bufstate *state);
int v2_basic2_read_rec(int unit, struct bufstate *state);
int v2_movetrans(char *sbuf, int spos, int slen, char *dbuf, int dpos, 
		 int dlen, struct trans* trans, int *pplen, char *partpix);
int v2_basic_writ_rec(int unit, struct bufstate *state);
int v2_basic2_writ_rec(int unit, struct bufstate *state);
int v2_basic_get_eol_size(int unit);
int v2_basic_read_in_eol(int unit, char *p, int eol_size);
int v2_double_byte_swap(unsigned char from[8], unsigned char to[8]);
int v2_real_byte_swap(unsigned char from[4], unsigned char to[4]);
int v2_copy_value_block_item(int from_unit, int to_unit, int item);
int v2_add_msg_current_table(char *a, int kopt, VALUE_TABLE* currval_table,
			  VALUE_TABLE *def_table);
int v2_convert_to_text(char **text, char *value, char *format);
int v2_build_system_label(int unit,char *recbuf,int *lblsize);
int v2_trans_prim_prop_labels(int unit,int *lblsize,char *recbuf);
int v2_trans_prim_hist_labels(int unit,int *lblsize,char *recbuf);
int v2_trans_curr_hist_item(int unit,int *lblsize,char *recbuf);
int v2_update_label_size_item(int unit,int lblsize);
int v2_find_task(int unit, char *task, int instance, char **start, char **stop);
int v2_find_system(int unit,char **start,char **stop);
int v2_find_property(int unit, char *prop, int instance, char **start, 
		     char **stop, int *inst_out);
int v2_find_first_hist_item(char* buf, char **loc);
int v2_add_label_item(int unit, char *newvalue, char *start, char *stop, 
		      char *key, char *value, int vallen,
		      int element, int nelements, int level, char *format, 
		      int maxlen);
int v2_find_key(char **start, char **stop, char *key, char **value,
		int *vallen);
int v2_delete_label_item(int unit, char *start, char *stop, char *key, 
		      char *value, int vallen,
		      int start_element, int *nelements, char **newstop);
void v2_add_lbl_item_value_tbl(int unit,int kopt, char *value);
int v2_compress_close(int unit);
int v2_check_out_lbl_on_close(int unit);
int v2_close_file(int unit);
int v2_process_output_file(int unit);
int v2_process_input_file(int unit);
int v2_determine_translation(int unit);
int v2_compress_preprocess(int unit);
void v2_error_handler(int unit, int code);
int v2_compress_read_rec(int unit, struct bufstate* state, char *buffer, 
		      struct trans *trans);
int v2_read_rec(struct bufstate* state, char *buffer, int rec, int startbyte, 
	     int nbytes, struct trans *trans);
int v2_write_seq_labels(int unit);
int v2_compress_writ_rec(int unit, struct bufstate* state, char *buffer, 
		      struct trans *trans);
int v2_del_complex_label_item(char *value, int vallen, char *start, 
			      char *stop, int start_element,
			      int* nelements_to_delete, int nelements_present,
			      int length, int level, char *format, 
			      char *key, char **newstop);
int v2_translate_input(char *stype, char *dtype, char *sihost, 
		       char *srhost, struct trans *trans);
int v2_translate_output(char *stype, char *dtype, char *dihost, 
			char *drhost, struct trans *trans);
void v2_sys_msg(int unit, int code);
int v2_check_primary_input(int unit);
int v2_get_label_item(char **value, int *vallen, char **element, int *level);
int v2_parse_label(char *label, int len, char **sk, char **ek, char **sv, 
		char **ev);
char *v2_dequoted(char *s,int len);
int v2_write_blocks(struct devstate *devstate, char *buf, V2_OFFSET block, 
		    V2_OFFSET nblocks, int async_flag);
void v2_get_def_filename(void);
int v2_compress_get_eol_size(int unit);
int v2_get_one_int_parm(char *name, int number, int *value);
void v2_update_bufstate(struct bufstate* bufstate, V2_OFFSET location);
int v2_read_blocks(struct devstate *devstate, char *buf, V2_OFFSET block, 
		   V2_OFFSET nblocks, int async_flag);
int v2_set_file_offset(int unit, V2_OFFSET new_offset);
int v2_extend_disk_file(struct diskstate *diskstate, V2_OFFSET amount);
int v2_write_disk_eof(int unit, struct diskstate *diskstate, 
		      V2_OFFSET last_byte, int recsize);
int v2_convert_from_text(char **converted_item, char *text, int length, 
			 char *format);
int v2_open_disk_input(int unit, struct bufstate* bufstate);
int v2_map_disk_file(int unit);
int v2_open_disk_output(int unit, struct bufstate* bufstate);
int v2_open_input_file(int unit);
void v2_close_down(int unit);
int v2_initialize_from_label(int unit);
int v2_bytes_per_pixel(char *ctype, char *cihost, char *crhost, int *status);
int v2_initialize_buffer(int unit);
void v2_collect_history_info(void);
int v2_get_eol_size(int unit);
int v2_add_hist_task(int unit);
void v2_get_out_size_from_parm(int unit);
int v2_est_primary_input(void);
void v2_copy_primary_input_val(int unit);
int v2_open_output_file(int unit);
int v2_create_output_label(int unit);
void v2_general_initialize(void);
int v2_valid_unit(int unit);
void v2_initialize_value_table(struct UNIT_TABLE* unit_table,
			    int n_unit_table_entries,
			    VALUE_TABLE* current_table,
			    VALUE_TABLE* default_table);
int v2_read_disk(struct diskstate* disk, char *buf, V2_OFFSET block, 
		 V2_OFFSET nblocks, int async_flag, 
		 int* transfer_count, V2_OFFSET file_offset);
int v2_read_nop(void);
int v2_read_cache(struct bufstate* bufstate, int rec);
int v2_compress_read_in_eol(int unit, char* q, int eol_size);
int v2_vax_ieee_r(unsigned char* from, unsigned char *ieee);
int v2_vax_ieee_d(unsigned char* from, unsigned char *ieee);
int v2_ieee_vax_r(unsigned char* ieee, unsigned char *from);
int v2_ieee_vax_d(unsigned char* ieee, unsigned char *from);
void v2_hostmsg(int code, char* msg, int maxlen);
void v2_build_history_label(char* ps);
int v2_write_disk(struct diskstate* disk, char *buf, V2_OFFSET block, 
		  V2_OFFSET nblocks, int async_flag, int *transfer_count,
		  V2_OFFSET file_offset);
int v2_write_nop(void);
int p_xladd(int unit);
int p_xldel(int unit);
int c_xldel(int unit, char *type, char *key);
int v2_get_nopts(int *nopts, int *nargs, int nconst, va_list *params);
int v2_process_optionals_c(int unit, struct UNIT_TABLE* opts_table, 
			int n_entries, VALUE_TABLE* currval_table, 
			VALUE_TABLE *def_table,
			int nopts, va_list *params);
int c_xladd(int unit, char *type, char *key, void *newvalue, int len);
int v2_format_preset_for(int unit, int *nopts, int *nargs, int nconst, 
		      va_list *params);
int v2_process_optionals_for(int unit, struct UNIT_TABLE* opts_table, 
			  int n_entries, VALUE_TABLE* currval_table, 
			  VALUE_TABLE* def_table,
			  int nopts, va_list* params, 
			  char *argptr, int nargs, int argno, int strno,
			  va_list* str_params, int *str_which);
int p_xlget(int unit);
int c_xlget(int unit, char *type, char *key, char **newvalue,
	    int *internal_length, int *start_element, 
	    int *u_nelements, char **outformat);
int p_xlhinfo(int unit);
int c_xlhinfo(int unit, char *tasks, int *instances, int *nhist, int len);
int p_xlinfo(int unit);
int c_xlinfo(int unit, char *type, char *key, char *format, int *maxlength, 
	     int *nelement);
int p_xlninfo(int unit);
int c_xlninfo(int unit, char *key, char *format, int *maxlength, int *nelement);
int p_xlpinfo(int unit);
int c_xlpinfo(int unit, char *props, int *nprop, int len);
int p_xvadd(int unit);
int c_xvadd(int unit);
int p_xvclose(int unit);
int c_xvclose(int unit);
int p_xvget(int unit);
int c_xvget(int unit);
int v2_open_act_preset_c(int unit, int nopts, va_list *params);
int v2_open_act_preset_for(int unit, int *nopts, int *nargs, int nconst, 
			va_list *params);
int p_xvopen(int unit);
int c_xvopen(int unit);
int v2_parm_write(char *addr, int len);
int v2_parm_close(void);
int v2_parm_init(int inunit);
int p_xvread(int unit);
int c_xvread(int unit, char *buffer);
int p_xvunit(int *unit, char *name, int instance);
int c_xvunit(int unit, char *name, int instance);
int p_xvwrit(int unit);
int c_xvwrit(int unit, char *buffer);
void v2_get_parm_for(struct PARBLK* parblock,
		  char *name,int *count, int *def, int maxcnt,
		  char *value, char **argptr, int nargs, int argno,
		  int strno, va_list *params, int *which, int double_flag);
int v2_get_parm_c(struct PARBLK* parblock, char *name, char *value, int *count, 
	       int *def, int maxcnt, int length, int double_flag);
int v2_test_keyword(struct PARBLK* parblock, char *value);
int v2_get_pstat(struct PARBLK* parblock, char *key, int *count, int *def, 
	      int *maxlen, char *type);
void v2_pack_xvsptr(char* out, struct VARIABLE *v, int count);
void *v2_array_io_location(int unit);
int v2_byte2half(void *from, void *to, int len, struct trans *trans);
int v2_byte2full(void *from, void *to, int len, struct trans *trans);
int v2_byte2real(void *from, void *to, int len, struct trans *trans);
int v2_byte2doub(void *from, void *to, int len, struct trans *trans);
int v2_byte2comp(void *from, void *to, int len, struct trans *trans);
int v2_half2byte(void *from, void *to, int len, struct trans *trans);
int v2_half2full(void *from, void *to, int len, struct trans *trans);
int v2_half2real(void *from, void *to, int len, struct trans *trans);
int v2_half2doub(void *from, void *to, int len, struct trans *trans);
int v2_half2comp(void *from, void *to, int len, struct trans *trans);
int v2_full2byte(void *from, void *to, int len, struct trans *trans);
int v2_full2half(void *from, void *to, int len, struct trans *trans);
int v2_full2real(void *from, void *to, int len, struct trans *trans);
int v2_full2doub(void *from, void *to, int len, struct trans *trans);
int v2_full2comp(void *from, void *to, int len, struct trans *trans);
int v2_real2byte(void *from, void *to, int len, struct trans *trans);
int v2_real2half(void *from, void *to, int len, struct trans *trans);
int v2_real2full(void *from, void *to, int len, struct trans *trans);
int v2_real2doub(void *from, void *to, int len, struct trans *trans);
int v2_real2comp(void *from, void *to, int len, struct trans *trans);
int v2_doub2byte(void *from, void *to, int len, struct trans *trans);
int v2_doub2half(void *from, void *to, int len, struct trans *trans);
int v2_doub2full(void *from, void *to, int len, struct trans *trans);
int v2_doub2real(void *from, void *to, int len, struct trans *trans);
int v2_doub2comp(void *from, void *to, int len, struct trans *trans);
int v2_comp2byte(void *from, void *to, int len, struct trans *trans);
int v2_comp2half(void *from, void *to, int len, struct trans *trans);
int v2_comp2full(void *from, void *to, int len, struct trans *trans);
int v2_comp2real(void *from, void *to, int len, struct trans *trans);
int v2_comp2doub(void *from, void *to, int len, struct trans *trans);
int v2_trans_swap2(void *from, void *to, int len, struct trans *trans);
int v2_trans_swap4(void *from, void *to, int len, struct trans *trans);
int v2_trans_swap8(void *from, void *to, int len, struct trans *trans);
int v2_r_vax2ieee(void *from, void *to, int len, struct trans *trans);
int v2_r_vax2rieee(void *from, void *to, int len, struct trans *trans);
int v2_r_ieee2vax(void *from, void *to, int len, struct trans *trans);
int v2_r_rieee2vax(void *from, void *to, int len, struct trans *trans);
int v2_d_vax2ieee(void *from, void *to, int len, struct trans *trans);
int v2_d_vax2rieee(void *from, void *to, int len, struct trans *trans);
int v2_d_ieee2vax(void *from, void *to, int len, struct trans *trans);
int v2_d_rieee2vax(void *from, void *to, int len, struct trans *trans);
int v2_c_vax2ieee(void *from, void *to, int len, struct trans *trans);
int v2_c_vax2rieee(void *from, void *to, int len, struct trans *trans);
int v2_c_ieee2vax(void *from, void *to, int len, struct trans *trans);
int v2_c_rieee2vax(void *from, void *to, int len, struct trans *trans);
int v2_c_ieee2rieee(void *from, void *to, int len, struct trans *trans);
int v2_c_rieee2ieee(void *from, void *to, int len, struct trans *trans);
char *v2_determine_format(char *value);
int v2_dual_translation(void *from, void *to, int len, struct trans *trans);
int v2_align_in_translation(void *from, void *to, int len, 
			    struct trans *trans);
int v2_bad_trans(void *from, void *to, int len, struct trans *trans);
int v2_align_out_translation(void *from, void *to, int len, 
			     struct trans *trans);
char *v2_find_pds_keyword(char *label, char *key);
int v2_line_size(VALUE_TYPE value);
int v2_band_size(VALUE_TYPE value);
int v2_error_action(VALUE_TYPE value);
int v2_error_mess(VALUE_TYPE value);
int v2_image_size(VALUE_TYPE value);
int v2_label_format(VALUE_TYPE value);
int v2_samp_size(VALUE_TYPE value);
int v2_nsamp_size(VALUE_TYPE value);
int v2_binary_size(VALUE_TYPE value);
int v2_instance(VALUE_TYPE value);
int v2_element(VALUE_TYPE value);
int v2_image_org(VALUE_TYPE value);
int v2_method(VALUE_TYPE value);
int v2_op(VALUE_TYPE value);
int v2_format(VALUE_TYPE value);
int v2_type(VALUE_TYPE value);
int v2_dim(VALUE_TYPE value);
int v2_hist_name(VALUE_TYPE value);
int v2_property_name(VALUE_TYPE value);
int v2_str_item(VALUE_TYPE value);
int v2_cond(VALUE_TYPE value);
int v2_closa(VALUE_TYPE value);
int v2_u_file(VALUE_TYPE value);
int v2_ladd_mode(VALUE_TYPE value);
int v2_unavailable(VALUE_TYPE value);
int v2_convert_chk(VALUE_TYPE value);
int v2_intfmt_chk(VALUE_TYPE value);
int v2_realfmt_chk(VALUE_TYPE value);
int v2_host_chk(VALUE_TYPE value);
int v2_bltype_chk(VALUE_TYPE value);
int v2_upd_hist_chk(VALUE_TYPE value);
int v2_compress_chk(VALUE_TYPE value);
V2_OFFSET v2_find_pds_offset(char *label);
char *v2_expand_filename(char *inpath, int makedir, int *status);
void v2_i_crack (char *entry, char *name, char *device);
int v2_parm_read(char addr[], int len);

void v2_make_upper_case(char *out, char *in);
void v2_make_upper_case_max(char* out, char *in, int max);

int v2_det_tape_blksize(int unit, int read_ok, int index, int file);
int v2_det_tape_recsize(int unit);
int v2_double_eof(struct tapestate *);
int v2_open_tape(int unit, struct bufstate *bufstate, int in_index, int filenr);
int v2_read_tape(struct tapestate *state, char *buf, V2_OFFSET block,
	V2_OFFSET nblocks, int async_flag, int *transfer_count);
int v2_space_record(struct tapestate *state, int records);
void v2_i_init(struct PARBLK *parblk);
void v2_i_exit(int, void *);
int v2_i_search_name(char *tape[], int count, char *name);
int v2_i_search_device(char *tape[], int count, char *device);
int v2_i_analyze(char *filespec, char *tape_table[], int count, int *index,
					int *filenr);
int v2_write_tape(struct tapestate *state, char *buf, V2_OFFSET block,
	V2_OFFSET nblocks, int async_flag, int *transfer_count);
int v2_i_position_tape(int channel, int index, int filenr);
int v2_i_rewind(int channel, int index);
int v2_i_space_file(int channel, int index, int files);
int v2_i_back_space(int channel, int index);
int v2_i_space_record(int channel, int index, int records);
int v2_exit_handler(void (*func)(int,void*));

#if RTL_USE_SHELL_VIC
int zzq_out(struct PARBLK* parblk);
int zzinit(struct PARBLK *parblk,int argc,char *argv[]);
#endif
#endif
