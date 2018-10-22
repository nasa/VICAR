/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <math.h>

#include "return_status.h"
#include "lbl_gen_api.h"
#include <zvproto.h>

typedef struct
	{
	LblApiStringItem_typ	label1;
	LblApiIntItem_typ	label2[4];
	LblApiRealItem_typ	label3;
	LblApiDoubleItem_typ	label4;
	char			KeywordItem[255];
	int			DummyValue;
	} LblExample_1_typ;

typedef struct
	{
	char	label1[256];
	int	label2[4];
	float	label3;
	double	label4;
        int	label1Valid;
        int	label2Valid;
        int	label3Valid;
        int	label4Valid;
        int	label1Element;
        int	label2Element;
        int	label3Element;
        int	label4Element;
	char			KeywordItem[255];
	} LblExample_2_typ;


/***  Function Prototypes  ***/
	void	read_labels( int );
	void	read_labels1( int );
	void	read_labels2( int );
	void	write_labels(int );
	void	write_labels1(int );
	void	write_labels2(int );

/*******************************************************************************
/*				MAIN
/*
/******************************************************************************/
int	main(
  int	argc,
  char	*argv[])
{ int	cnt,
	Line,
	OutUnit,
	status;
  char	Pixels[100];

  zveaction("SA","");		// make sure error overrides work

  if (argc > 1) strcpy(Pixels,argv[1]);
  else strcpy(Pixels,"label_test.img");

  status = zvunit(&OutUnit,"NA",1,"U_NAME",Pixels,NULL);
  if (status != 1)
  { zvmessage("Could not open output file"," ");
    zabend();
  }

  status = zvopen(OutUnit, "OP","WRITE", "O_FORMAT","BYTE", "U_FORMAT","BYTE",
                  "OPEN_ACT","AS", "U_NL",10, "U_NS",10, "U_NB",1,  NULL);
  zvsignal(OutUnit, status, 1);

  printf("***\n***\t Writing Labels\n***\n");

  write_labels(OutUnit);

  for (Line=1; Line<=10; Line++)
      status =  zvwrit(OutUnit, Pixels, NULL);
  zvclose(OutUnit, NULL);

  status = zvopen(OutUnit, "OP","READ", "OPEN_ACT","AS",  NULL);
  zvsignal(OutUnit, status, 1);

  printf("***\n***\t Reading Labels\n***\n");
  read_labels(OutUnit);
  zvclose(OutUnit, NULL);

  return 0;
}

/******************************************************************************
/*				LBL_EXAMPLE_1
/*
/*****************************************************************************/
int	LblExample_1(
  int	Unit,
  int	Obtain,
  LblExample_1_typ	*LabelItems)
{ int	RtnStatus;
  LblApiCntrl_typ	Cntrl;
  static LblApiElement_typ	LabelTbl[] = {

	{"KEYWORD1",	"STRING",	1,	0,	1,	1,	NULL,
	 LBL_OFFSET(LblExample_1_typ,label1.Value),
	 LBL_OFFSET(LblExample_1_typ,label1.Valid),
/***
	 LBL_NO_RETURN},
**/
	 LBL_OFFSET(LblExample_1_typ,DummyValue)},
	 
	{"KEYWORD2",	"INT",		0,	0,	1,	1,	NULL,
	 LBL_OFFSET(LblExample_1_typ,label2[0].Value),
	 LBL_OFFSET(LblExample_1_typ,label2[0].Valid),
	 LBL_OFFSET(LblExample_1_typ,DummyValue)},
	 
	{"KEYWORD2",	"INT",		0,	1,	1,	2,	NULL,
	 LBL_OFFSET(LblExample_1_typ,label2[1].Value),
	 LBL_OFFSET(LblExample_1_typ,label2[1].Valid),
	 LBL_OFFSET(LblExample_1_typ,DummyValue)},
	 
	{"KEYWORD2",	"INT",		0,	1,	1,	3,	NULL,
	 LBL_OFFSET(LblExample_1_typ,label2[2].Value),
	 LBL_OFFSET(LblExample_1_typ,label2[2].Valid),
	 LBL_OFFSET(LblExample_1_typ,DummyValue)},
	 
	{"KEYWORD2",	"INT",		0,	1,	1,	4,	NULL,
	 LBL_OFFSET(LblExample_1_typ,label2[3].Value),
	 LBL_OFFSET(LblExample_1_typ,label2[3].Valid),
	 LBL_OFFSET(LblExample_1_typ,DummyValue)},
	 
	{"KEYWORD3,KEYWORD3A",	"REAL",		0,	0,	1,	1,	NULL,
	 LBL_OFFSET(LblExample_1_typ,label3.Value),
	 LBL_OFFSET(LblExample_1_typ,label3.Valid),
	 LBL_OFFSET(LblExample_1_typ,DummyValue)},
	 
	{"KEYWORD4",	"DOUB",		0,	0,	1,	1,	NULL,
	 LBL_OFFSET(LblExample_1_typ,label4.Value),
	 LBL_OFFSET(LblExample_1_typ,label4.Valid),
	 LBL_OFFSET(LblExample_1_typ,DummyValue)},
	 
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} };

  LblApiProcess_typ	Label = {
	LabelTbl,	"PROPERTY",	"PROPERTY",	"EXAMPLE_1_PROPERTY",
	NULL };

  LabelTbl[2].KeywordUsed = LabelItems->KeywordItem,
  Label.Buffer = (void *)LabelItems;

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;
  
  RtnStatus = LblProcessor(&Cntrl, &Label);
  if (RTN_FAILURE(RtnStatus))
  { printf("\n\nLabel Type: %s\n",Label.Type);
    printf("%s Name: %s\n",Label.NameKeyword,Label.NameValue);
    printf("Label Processing failed: %s\n\t'%s'\n",
           RTN_DFLT_MSG(RtnStatus),Cntrl.ErrMsgBuf);
  }
  PrintLabelElements(&Label);

  return (RtnStatus);
}

/******************************************************************************
/*				LBL_EXAMPLE_2
/*
/*****************************************************************************/
int	LblExample_2(
  int	Unit,
  int	Obtain,
  LblExample_2_typ	*LabelItems)
{ int	RtnStatus;
  LblApiCntrl_typ	Cntrl;
  static LblApiElement_typ	LabelTbl[] = {

	{"KEYWORD1",	"STRING",	0,	0,	1,	1,	NULL,
	 LBL_OFFSET(LblExample_2_typ,label1),
	 LBL_OFFSET(LblExample_2_typ,label1Valid),
	 LBL_OFFSET(LblExample_2_typ,label1Element)},
	 
	{"KEYWORD2",	"INT",		0,	0,	4,	1,	NULL,
	 LBL_OFFSET(LblExample_2_typ,label2),
	 LBL_OFFSET(LblExample_2_typ,label2Valid),
	 LBL_OFFSET(LblExample_2_typ,label2Element)},
	 
	{"KEYWORD3",	"REAL",		0,	0,	1,	1,	NULL,
	 LBL_OFFSET(LblExample_2_typ,label3),
	 LBL_OFFSET(LblExample_2_typ,label3Valid),
	 LBL_OFFSET(LblExample_2_typ,label3Element)},
	 
	{"KEYWORD4,KEYWORD4A",	"DOUB",		0,	0,	1,	1,	NULL,
	 LBL_OFFSET(LblExample_2_typ,label4),
	 LBL_OFFSET(LblExample_2_typ,label4Valid),
	 LBL_OFFSET(LblExample_2_typ,label4Element)},
	 
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0} };

  LblApiProcess_typ	Label = {
	LabelTbl,	"HISTORY",	"HIST",		"TASK",
	NULL };

  LabelTbl[3].KeywordUsed = LabelItems->KeywordItem;	/* 4th Item */
  Label.Buffer = (void *)LabelItems;

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;
  
  RtnStatus = LblProcessor(&Cntrl, &Label);
  if (RTN_FAILURE(RtnStatus))
  { printf("\n\nLabel Type: %s\n",Label.Type);
    printf("%s Name: %s\n",Label.NameKeyword,Label.NameValue);
    printf("Label Processing failed: %d '%s'\n",
           RTN_NUMBER(RtnStatus),Cntrl.ErrMsgBuf);
  } else PrintLabelElements(&Label);

  return (RtnStatus);
}

/*******************************************************************************
/*				write_labels
/*
/******************************************************************************/
void	write_labels(
  int	Unit)
{
  LblSetVersion("V2");
  write_labels1(Unit);
  LblSetVersion("V1");
  write_labels2(Unit);
  return;
}

/*******************************************************************************
/*				read_labels
/*
/******************************************************************************/
void	read_labels(
  int	Unit)
{
  read_labels1(Unit);
  read_labels2(Unit);
  return;
}

/*******************************************************************************
/*				write_labels1
/*
/******************************************************************************/
void	write_labels1(
  int	Unit)
{ int	lc;
  LblExample_1_typ	Test;

  memset(&Test,0,sizeof(LblExample_1_typ));

  strcpy(Test.label1.Value,"This is a test String");
  for (lc=1234; lc<1238; lc++) Test.label2[lc-1234].Value = lc*2;
  Test.label3.Value = 1234.56;
  Test.label4.Value = 1.2e34;

  Test.label1.Valid = LBL_VALID;
  Test.label2[0].Valid = LBL_VALID;
  Test.label2[1].Valid = LBL_VALID;
  Test.label2[2].Valid = LBL_VALID;
  Test.label2[3].Valid = LBL_VALID;
  Test.label3.Valid = LBL_VALID;
  Test.label4.Valid = LBL_VALID;

  Test.label2[1].Valid = LBL_PDS_NULL;
  Test.label2[3].Valid = LBL_PDS_UNK;
/***
  Test.label2[3].Valid = LBL_PDS_NA;
***/

  LblExample_1(Unit,LBL_FALSE,&Test);
  return;
}

/*******************************************************************************
/*				write_labels2
/*
/******************************************************************************/
void	write_labels2(
  int	Unit)
{ int	lc;
  LblExample_2_typ	Test;

  memset(&Test,0,sizeof(LblExample_2_typ));

  strcpy(Test.label1,"This is a test String");
  for (lc=0; lc<4; lc++) Test.label2[lc] = lc*2;
  Test.label3 = 1234.56;
  Test.label4 = 1.2e34;

  Test.label1Valid = LBL_VALID;
  Test.label2Valid = LBL_VALID;
  Test.label3Valid = LBL_VALID;
  Test.label4Valid = LBL_VALID;

/***
***/
  Test.label2Valid = LBL_PDS_NA;

  LblExample_2(Unit,LBL_FALSE,&Test);
  return;
}

/*******************************************************************************
/*				read_labels1
/*
/******************************************************************************/
void	read_labels1(
  int	Unit)
{ int	lc,
	status;
  LblExample_1_typ	Test;

  memset(&Test,0,sizeof(LblExample_2_typ));

  status = LblExample_1(Unit,LBL_TRUE,&Test);

  return;
}

/*******************************************************************************
/*				read_labels2
/*
/******************************************************************************/
void	read_labels2(
  int	Unit)
{ int	lc;
  LblExample_2_typ	Test;

  memset(&Test,0,sizeof(LblExample_2_typ));

  LblExample_2(Unit,LBL_TRUE,&Test);
}
