/* This same code is used in both the VRDI and in the USEDISP/FREEDISP	*/
/* commands in TAE.  The code must be repeated since TAE cannot link to	*/
/* the VRDI, and USEDISP/FREEDISP must be TAE intrinsics under VMS so	*/
/* the device will be allocated in the parent process.			*/

/*
 *
 *	XDSUPPORT.H - Supported Devices and Systems
 *
 */

#define	DEANZA_IP85LO	1
#define	ADAGE_3000	2
#define	RAMTEK_9460	3
#define	RASTER_TECH	4
#define	IIS_IVAS_IV	5
#define X_WINDOW	6
#define JUP_JSTATION	7
#define TEK_4237	8
#define TEK_3D_LEFT	9
#define TEK_3D_RIGHT	10
#define	DEANZA_IP85HI	11
#define DEANZA_IP85LX	12
#define DEANZA_IP9000	13
#define DUMMY		14

#define	TOTAL_SUPPORTED	14	/* total in the table below */

#ifndef XD_INITIALIZE
PUBLIC	struct	{
	char	*Prefix;
	int	Type;
	char	SubType;
	} Supported_Devices[];
#else /* XD_INITIALIZE */
PUBLIC	struct	{
	char	*Prefix;
	int	Type;
	char	SubType;
	} Supported_Devices[] = {
	{ "EP",		DEANZA_IP85LO,		'A'	},
	{ "IK",		ADAGE_3000,		'A'	},
	{ "RM",		RAMTEK_9460,		'*'	},
	{ "GD",		RASTER_TECH,		'*'	},
	{ "IV",		IIS_IVAS_IV,		'A'	},
	{ "XW",		X_WINDOW,		'*'	},
	{ "JU",		JUP_JSTATION,		'*'	},
	{ "TK",		TEK_4237,		'A'	},
	{ "TK",		TEK_3D_LEFT,		'L'	},
	{ "TK",		TEK_3D_RIGHT,		'R'	},
	{ "EP",		DEANZA_IP85HI,		'B'	},
	{ "EP",		DEANZA_IP85LX,		'C'	},
	{ "EP",		DEANZA_IP9000,		'D'	},
	{ "DU",		DUMMY,			'*'	},
	{ 0,		0,			0	}
	};
#endif /* XD_INITIALIZE */
