#ifdef PRIVATE
#undef PRIVATE
#endif

#if VMS_OS
#if ALPHA_ARCH
#define PRIVATE
#else	/* VAX */
#ifdef DUMMY_INITIALIZE
#define PRIVATE globaldef noshare
#else
#define PRIVATE globalref
#endif
#endif
#endif

#if UNIX_OS
#define PRIVATE
#endif

#define DUMMY_RED		1
#define DUMMY_GREEN		2
#define DUMMY_BLUE		3

#define DUMMY_CURSOR_X(cursor)		DCB[*Unit]->DeviceDependent[(cursor)-1]
#define DUMMY_CURSOR_Y(cursor)		DCB[*Unit]->DeviceDependent[(cursor)+1]

/*  Software image plane look-up tables  */
PRIVATE unsigned char (*DUMMY_LUTS);

#define DUMMY_LUT(lut, color) \
	(*(DUMMY_LUTS + (((lut)-1)*(MAX_LUT_VALUE+1)) + (color)))

/*  Software overlay plane look-up tables  */
PRIVATE unsigned char (*DUMMY_OVLY_LUTS);

#define DUMMY_OVLY_LUT(lut, color) \
	(*(DUMMY_OVLY_LUTS + (((lut)-1)*(MAX_LUT_VALUE+1)) + (color)))

/*  Software image memory planes  */
PRIVATE unsigned char (*DUMMY_IMPS);

#define DUMMY_IMP(imp, x, y) \
(*(DUMMY_IMPS + (((imp)-1)*N_SAMPS*N_LINES) + (((y)-1)*N_SAMPS) + ((x)-1)))
