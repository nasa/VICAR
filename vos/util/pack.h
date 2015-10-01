#define SUCCESS		1
#define FAILURE		0
#define TRUE		1
#define FALSE		0
#define NAME_OFFSET	9

#ifndef SEEK_SET
#define SEEK_SET	0
#endif

#define REPACK_LABEL		0
#define REPACK_BIT	0x00000001
#define SOURCE_LABEL		1
#define SOURCE_BIT	0x00000002
#define IMAKE_LABEL		2
#define IMAKE_BIT	0x00000004
#define MAKE_LABEL		3
#define MAKE_BIT	0x00000008
#define BUILD_LABEL		4
#define BUILD_BIT	0x00000010
#define PDF_LABEL		5
#define PDF_BIT		0x00000020
#define TEST_LABEL		6
#define TEST_BIT	0x00000040
#define DOC_LABEL		7
#define DOC_BIT		0x00000080
#define OTHER_LABEL		8
#define OTHER_BIT	0x00000100
#define ALL_BITS	0x000001FF
#define SYS_BITS	SOURCE_BIT | PDF_BIT | IMAKE_BIT | DOC_BIT
#define STD_BITS	SOURCE_BIT | PDF_BIT | IMAKE_BIT

#define SPECIFIC_FILES	0
#define ALL_FILES	1
#define SYS_FILES	2
#define STD_FILES	3

#define NO_ACTION	-1

#define SET_BIT(val, bit)	((val) | (bit))
#define CLEAR_BIT(val, bit)	((val) & ~(bit))
#define BIT_TEST(val, bit)	(((val) & (bit)) == (bit))

char *sections[][2] = {{"-r", "$Repack_File:"},
                       {"-s", "$Source_File:"},
                       {"-i", "$Imake_File:"},
                       {"-m", "$Make_File:"},
                       {"-b", "$Build_File:"},
                       {"-p", "$PDF_File:"},
                       {"-t", "$Test_File:"},
                       {"-d", "$Doc_File:"},
                       {"-o", "$Other_File:"},
                       {0,    0}};

char *EOFSTRING = "$ VOKAGLEVE";
