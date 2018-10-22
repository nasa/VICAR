/************************************************************************/
/* This include generates function names for the split-out functions	*/
/* (based on zoom type, data type, and color/ps/bw mode).  It is	*/
/* extraordinarily messy because the stupid preprocessor won't allow	*/
/* this kind of construct:						*/
/* #define NAME(x,y) xxx_##x##_##y					*/
/* #define TYPE qqq							*/
/* NAME(TYPE,abc)							*/
/* This should generate "xxx_qqq_abc" but it doesn't work.  Token paste	*/
/* won't substitute the values it's pasting!!  Stupid.			*/
/************************************************************************/

#ifdef FN_NAME
#undef FN_NAME
#endif

#ifdef EASYZOOM

#if DTYPE == DTYPE_BYTE

#if MODE == MODE_COLOR
#define FN_NAME(x) XvicCopyRaw_ez_byte_col_##x
#else
#if MODE == MODE_PSEUDO
#define FN_NAME(x) XvicCopyRaw_ez_byte_ps_##x
#else
#define FN_NAME(x) XvicCopyRaw_ez_byte_bw_##x
#endif
#endif /* MODE == MODE_COLOR */

#else /* DTYPE == DTYPE_BYTE */
#if DTYPE == DTYPE_HALF

#if MODE == MODE_COLOR
#define FN_NAME(x) XvicCopyRaw_ez_half_col_##x
#else
#if MODE == MODE_PSEUDO
#define FN_NAME(x) XvicCopyRaw_ez_half_ps_##x
#else
#define FN_NAME(x) XvicCopyRaw_ez_half_bw_##x
#endif
#endif /* MODE == MODE_COLOR */

#else /* DTYPE == DTYPE_HALF */

#if MODE == MODE_COLOR
#define FN_NAME(x) XvicCopyRaw_ez_othr_col_##x
#else
#if MODE == MODE_PSEUDO
#define FN_NAME(x) XvicCopyRaw_ez_othr_ps_##x
#else
#define FN_NAME(x) XvicCopyRaw_ez_othr_bw_##x
#endif
#endif /* MODE == MODE_COLOR */

#endif /* DTYPE == DTYPE_HALF */
#endif /* DTYPE == DTYPE_BYTE */



#else /* EASYZOOM */



#if DTYPE == DTYPE_BYTE

#if MODE == MODE_COLOR
#define FN_NAME(x) XvicCopyRaw_hz_byte_col_##x
#else
#if MODE == MODE_PSEUDO
#define FN_NAME(x) XvicCopyRaw_hz_byte_ps_##x
#else
#define FN_NAME(x) XvicCopyRaw_hz_byte_bw_##x
#endif
#endif /* MODE == MODE_COLOR */

#else /* DTYPE == DTYPE_BYTE */
#if DTYPE == DTYPE_HALF

#if MODE == MODE_COLOR
#define FN_NAME(x) XvicCopyRaw_hz_half_col_##x
#else
#if MODE == MODE_PSEUDO
#define FN_NAME(x) XvicCopyRaw_hz_half_ps_##x
#else
#define FN_NAME(x) XvicCopyRaw_hz_half_bw_##x
#endif
#endif /* MODE == MODE_COLOR */

#else /* DTYPE == DTYPE_HALF */

#if MODE == MODE_COLOR
#define FN_NAME(x) XvicCopyRaw_hz_othr_col_##x
#else
#if MODE == MODE_PSEUDO
#define FN_NAME(x) XvicCopyRaw_hz_othr_ps_##x
#else
#define FN_NAME(x) XvicCopyRaw_hz_othr_bw_##x
#endif
#endif /* MODE == MODE_COLOR */

#endif /* DTYPE == DTYPE_HALF */
#endif /* DTYPE == DTYPE_BYTE */



#endif /* EASYZOOM */

