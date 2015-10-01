#if VAX_ARCH + MAC_MPW_ARCH + HP700_ARCH
#define FTN_NAME(a) a
#endif

#if SUN3_ARCH+SUN4_ARCH+ALLIANT_ARCH+DECSTATION_ARCH+MAC_AUX_ARCH+SGI_ARCH
#define FTN_NAME(a) a/**/_
#endif

#if CRAY_ARCH
/* Needs to be upper case! */
#define FTN_NAME(a) a
#endif
