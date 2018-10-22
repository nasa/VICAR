/*				GLL_LRS.H
 ******************************************************************************
 *
 * Low Rate Science Frame, definitions and macros.
 *
 * NOTES:
 *	This file uses some symbols & structures defined in MAIN_GLL.H
 *	All lengths are given in bytes unless stated otherwise.
 *
 * History:
 * 
 * Date		Reference	Description
 * -----------  --------------	------------------------------------------------
 *  7- 7-1989	N/A		Payam Zamani - Original Delivery
 ******************************************************************************
 */

/* AACS_LRS_TYP
 *=============================================================================
 * AACS Low Rate Science packet
 *=============================================================================
 */
typedef	struct
	{
	UWORD		rotor_ra;	/* Rotor Attitude Right Ascension    */
	UWORD		rotor_dec;	/* Rotor Attitude Declination	     */
	UWORD		rotor_twist;	/* Rotor Attitude Twist		     */
	UWORD		pltfrm_ra;	/* Platform Att.  Right Ascension    */
	UWORD		pltfrm_dec;	/* Platform Att.  Declination	     */
	UWORD		pltfrm_twist;	/* Platform Att.  Twist		     */
	UWORD		pltfrm_cone_rate;	/* Platform rate, cone	     */
	UWORD		pltfrm_x_cone_rate;	/* Platform rate, cross cone */
	UWORD		rotor_spin_delta;	/* Rotor Spin Motion Delta   */
	UWORD		rotor_spin_pos;		/* Rotor Spin Position angel */
	UWORD		cone_pos;		/* Cone Position, 1/2**16th  */
	UWORD		clock_pos;		/* Clock Position	     */
	}
	aacs_lrs_p_typ;

/* NIMS_LRS_TYP
 *=============================================================================
 * Near Infrared Mapping Spectrometer subsystem, Low Rate Science packet
 *=============================================================================
 */
typedef	struct
	{
	UBYTE		dsae[3];	/* Digital Status & Analog Engnrng   */
	}
	nims_lrs_p_typ;


/* ENG_LRS_TYP
 *=============================================================================
 * Engineering Low Rate Science packet
 *=============================================================================
 */
typedef	struct
	{
	UBYTE		hlm_1a[5];
	UBYTE		llm_1a[6];
	UBYTE		llm_2a[2];
	UBYTE		hlm_1b[5];
	UBYTE		llm_1b[6];
	UBYTE		llm_2b[2];
	UBYTE		aacs[16];
	UBYTE		filler;		
	UBYTE		p1[5];
	UBYTE		p2[5];
	UBYTE		p3[5];
	UBYTE		p4[5];
	UBYTE		p5[5];
	UBYTE		p6[5];
	UBYTE		p7[5];
	UBYTE		p8[5];
	UBYTE		p9[5];
	}
	eng_lrs_p_typ;

/* PWS_LRS_TYP
 *=============================================================================
 * Plasma Wave Subsytem, Low Rate Science packet
 *=============================================================================
 */
typedef	struct
	{
	UBYTE		digital_stat;	/* Digital Status	*/
	UBYTE		analog_eng;	/* Analog Engineering	*/
	UBYTE		filter[7];	/* Filter Channels	*/
	UBYTE		data_qlty;	/* Data Quality		*/
	UBYTE		wave[10];	/* Waveform Survey	*/
	}
	pws_lrs_p_typ;

/* SSI_LRS_TYP
 *=============================================================================
 * Solid State Imaging, Low Rate Science packet
 *=============================================================================
 */
typedef	struct
	{   struct
	    {
	    UBYTE		stnd_hk[2];	/* Standard Housekeeping     */
	    UBYTE		img_hk[2];	/* Imaging Housekeeping Data */
	    }	packet[3];
	}
	ssi_lrs_p_typ;

/* LRS_PACKET_TYP
 *=============================================================================
 * Compressed Low Rate Science packet lengths, NO GOLAY symbols.
 *=============================================================================
 */
#define		LRS_DDS_LEN		 2
#define		LRS_EPD1_LEN		50
#define		LRS_EPD2_LEN		26
#define		LRS_GOLAY_LEN		54	/* 4 packetes, same length   */
#define		LRS_HIC_LEN		12
#define		LRS_MAG_LEN		10	/* 2 packetes, same length   */
#define		LRS_PLS_LEN		51
#define		LRS_PPR_LEN		18
#define		LRS_UVS_LEN		84

typedef	struct
	{
	tlm_hdr_typ	header;			/* TLM header		    */
	eng_lrs_p_typ	eng;			/* Engineering packet	    */
	UBYTE		uvs[LRS_UVS_LEN];	/* Ultra Violet packet	    */
	UBYTE		hic[LRS_HIC_LEN];
	ssi_lrs_p_typ	ssi;
	UBYTE		pls[LRS_PLS_LEN];
	nims_lrs_p_typ	nims;
	UBYTE		dds[LRS_DDS_LEN];
	UBYTE		reserve[2];
	UBYTE		epd_1[LRS_EPD1_LEN];
	UBYTE		epd_2[LRS_EPD2_LEN];
	UBYTE		ppr[LRS_PPR_LEN];
	UBYTE		mag_1[LRS_MAG_LEN];
	UBYTE		mag_2[LRS_MAG_LEN];
	pws_lrs_p_typ	pws;
	aacs_lrs_p_typ	aacs;
	}
	lrs_typ;

/*
 *==============================================================================
 *	Same as "lrs_typ" but the Golay symbols ARE included
 *==============================================================================
 */
typedef	struct
	{
	tlm_hdr_typ	header;			/* TLM header		    */
	eng_lrs_p_typ	eng;			/* Engineering packet	    */
	UBYTE		uvs[LRS_UVS_LEN];	/* Ultra Violet packet	    */
	UBYTE		hic[LRS_HIC_LEN];
	ssi_lrs_p_typ	ssi;
	UBYTE		pls[LRS_PLS_LEN];
	nims_lrs_p_typ	nims;
	UBYTE		golay1[LRS_GOLAY_LEN];
	UBYTE		dds[LRS_DDS_LEN];
	UBYTE		reserve[2];
	UBYTE		epd_1[LRS_EPD1_LEN];
	UBYTE		golay2[LRS_GOLAY_LEN];
	UBYTE		epd_2[LRS_EPD2_LEN];
	UBYTE		ppr[LRS_PPR_LEN];
	UBYTE		mag_1[LRS_MAG_LEN];
	UBYTE		golay3[LRS_GOLAY_LEN];
	UBYTE		mag_2[LRS_MAG_LEN];
	pws_lrs_p_typ	pws;
	aacs_lrs_p_typ	aacs;
	UBYTE		golay4[LRS_GOLAY_LEN];
	}
	lrs_golay_typ;
