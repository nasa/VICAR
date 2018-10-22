/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "return_status.h"
#include "lbl_gen_api.h"
#include "zvproto.h"

#define USE_NEW_VERSIONS	1

static char	ErrorBuffer[256];
static int	SelectedVersion = LBL_KEYWORD_VERSION;

/******************************************************************************
 *				LBL_PROCESSOR
 *
 *	This routine uses the structures in the lbl_gen_api.h that define a
 *  label and how to process it.
 *
 *	The control structure (LblApiCntrl_typ) defines:
 *		VICAR file unit number
 *		Instance of the property or history label
 *		The return status
 *		The error count and error message, if any
 *		Processing flags: Proceed-On-Error and Obtain/Write
 *
 *	The label structure (LblApiProcess_typ) defines :
 *		The list of label elements in the label
 *		The type of label (property, history, etc.)
 *		The keyword of the label type (PROPERTY, HISTORY, SYSTEM)
 *		The value of the label type
 *		The address of the buffer to receive/send the label values
 *
 *	The label structure LblApiProcess_typ points to a table that
 *  defines all of the label elements.  This structure (LblApiElement_typ)
 *  defines:
 *		The keyword list (versioning)
 *		The format of the label item
 *		A Required flag
 *		A Continuation flag (part of previous item)
 *		Maximum values for this label item
 *		The value element number
 *		The keyword version used to define this value (debugging only)
 *		The byte offset to the address of the value
 *		The byte offset to the address of the valid flag
 *		The byte offset to the address of the num of elements returned
 *		The storage size available to the value (development/debugging)
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *****************************************************************************/
int	LblProcessor(
  LblApiCntrl_typ	*Cntrl,
  LblApiProcess_typ	*Label)
{ int	lc,
	status = 0,
        delItems,
	BadContinueLink = LBL_TRUE,	/* Continued label flagged as invalid */
					/* First label can not be continued   */
	Element,
	Idx,
	Items,
	Instance,
	MaxLength,
	MaxElements,
	RtnStatus = RTN_NORMAL,
	StringWriteFlag,
	VersionIdx,
	*ValidId;			/* Validity indicator of label value */
  char	*Keyword,			/* Keyword of label item             */
	ContinuationKeyword[LBL_KEYWORD_LTH],
	PrimaryKeyword[LBL_KEYWORD_LTH],
	LabelFormat[24],
	*PdsCheck,
	StringValue[LBL_MAX_STRING],
	*Buffer = Label->Buffer,
  	KeywordList[LBL_KEYWORDLIST_LTH];
  char	*KeywordVersions[LBL_KEYWORD_VERSION],
	KeywordNames[LBL_KEYWORD_VERSION][LBL_KEYWORD_LTH];
  LblApiElement_typ	*Fields = Label->Table;

  Cntrl->ErrorCount = 0;
  ErrorBuffer[0] = 0;

  Instance = (Cntrl->Instance < 1) ? 1 : Cntrl->Instance;

  if (Cntrl->Obtain==LBL_READ)
     memset(Label->Buffer,0,Label->BufferSize);
  else			/* Check for writing required labels */
  { for (Items=0; Fields[Items].KeywordList; Items++)
        if (Fields[Items].Required && !(int*)(Buffer+Fields[Items].ValidOffset))
    { RtnStatus = RTN_LBL_MISSING_REQ;

      /* Display error message only for LBL_WRITE */
      if (Cntrl->Obtain==LBL_WRITE) {
         sprintf(Cntrl->ErrMsgBuf,"Error missing label required: %-.200s",
                 Fields[Items].KeywordList);
         strcpy(ErrorBuffer,Cntrl->ErrMsgBuf);
      }
      if (!Cntrl->ProceedOnError) return RtnStatus;
    }
  }

  for (Items=0; Fields[Items].KeywordList; Items++)
  { /***  Check for label element continuation.  If a label item has
     ***  multiple values, it can be referenced one element at a time.
     ***  Continuation elements must use the same keyword, and must not
     ***  have gaps in the valid list (i.e., 1, 2, 4, 5).
     ***/
    ValidId = (int*)(Buffer+Fields[Items].ValidOffset);
    if (Fields[Items].Continuation)
    { if (Cntrl->Obtain==LBL_READ)
      { if (BadContinueLink)
        { /* Previous label 'obtain' must be successful for a continuation */
          *ValidId = LBL_INVALID;
          continue;
        }
      } else
      { /* Previous label 'put' and/or current item must be valid */
        if (!(*ValidId))
           BadContinueLink = LBL_TRUE;
        if (BadContinueLink) continue;
      }
    } else /* load up keyword(s) for a new item */
    { strncpy(KeywordList,Fields[Items].KeywordList,(LBL_KEYWORDLIST_LTH-1));
      KeywordList[LBL_KEYWORDLIST_LTH-1] = 0;
      Keyword = strtok(KeywordList,LBL_KEYWORD_DELIMITERS);

      /* Build a Keyword Generations table that points */
      /* to the valid keyword for each version.        */
      if (!Keyword)		/* There has to be atleast one */
      { *ValidId = LBL_INVALID;
        continue;
      } else Idx = -1;
      for (lc=0; lc<LBL_KEYWORD_VERSION; lc++)
      { if (Keyword && strcmp(Keyword,"-"))	/* Found a 'new' keyword */
           strcpy(KeywordNames[++Idx],Keyword);
        KeywordVersions[lc] = KeywordNames[Idx];
        Keyword = strtok(NULL,LBL_KEYWORD_DELIMITERS);
      }
      Keyword = KeywordVersions[VersionIdx=(SelectedVersion-1)];
      strcpy(PrimaryKeyword,Keyword);	 /* for error messages */
      strcpy(ContinuationKeyword,Keyword);
      if (Fields[Items].KeywordUsed)
         Fields[Items].KeywordUsed[0] = 0;  /* in case one is not found */
      StringWriteFlag = 0;		/* reset flag */
      BadContinueLink = LBL_FALSE;
    }

    status = 0;				/* clear status value */
    do	/* for all keywords in list */
      if (Cntrl->Obtain==LBL_READ)
    /********************************************************************
     ********************************************************************
     ***
     ***  Read the label items from the file
     ***
     ********************************************************************
     *******************************************************************/
    { Keyword = KeywordVersions[VersionIdx];

      if (!Fields[Items].Continuation)		/* 1st value for keyword */
      { if (VersionIdx+1 == SelectedVersion)	/* 1st time thru loop */
        { /*** Verify compatibility of label first  ***/
          status = zlinfo(Cntrl->FileUnit, Label->Type, Keyword,
			LabelFormat, &MaxLength, &MaxElements,
			"INSTANCE",	Instance,
			"ERR_ACT", "",
			Label->NameKeyword,	Label->NameValue, NULL);
          *ValidId = LBL_INVALID;

          if (status != 1)	/* It's okay for the item to be missing */
          { BadContinueLink = LBL_TRUE;
            continue;
          }

          if (MaxLength > LBL_MAX_STRING)  /* String larger than buffer */
          { BadContinueLink = LBL_TRUE;
            continue;
          }
        } else
        { /* Check to see if the last keyword is same as current .... */
          /* ... if last one failed, this one should too, so skip it. */
          if (strcmp(Keyword,KeywordVersions[VersionIdx+1]) == 0)
              continue;
        }
        strcpy(ContinuationKeyword,Keyword);
      } else	/* Insure multi-value items use the same keyword version */
      { VersionIdx = 0;
        Keyword = ContinuationKeyword;
      }

      /*** Verify existance of elements being read ***/
      if ((Fields[Items].Element+Fields[Items].MaxElements-1) > MaxElements)
      { BadContinueLink = LBL_TRUE;
        continue;
      }

      /***  Check all string labels, element by element, to see  ***/
      /***  if they could be a PDS: NULL, UNKN, or N/A           ***/
      if (strcmp(LabelFormat,"STRING") == 0)
         for (Element=0; Element<Fields[Items].MaxElements; Element++)
      { status = zlget(Cntrl->FileUnit, Label->Type,
                      Keyword,        StringValue,
                      "FORMAT",       LabelFormat,
                      "INSTANCE",     Instance,
                      "ELEMENT",      Fields[Items].Element+Element,
                      "NELEMENT",     1,
                      "ULEN",         (MaxLength+1),
		      "ERR_ACT",      "",
                      Label->NameKeyword,     Label->NameValue, NULL);
        if (status != 1)		/* Can there be any error recorvery ? */
        { BadContinueLink = LBL_TRUE;
          continue;
        }

        /***  An expected numeric label element has a simpler check  ***/
        /***  than a string, so branch for efficency (?)             ***/
        if (strcmp(Fields[Items].Format,LabelFormat) != 0)	/** Numeric **/
           switch (StringValue[1])
        { case '/': *ValidId = LBL_PDS_NA;
               break;
          case 'N': *ValidId = LBL_PDS_UNK;
               break;
          case 'U': *ValidId = LBL_PDS_NULL;
               break;
          default: *ValidId = LBL_VALID;
                   switch(Fields[Items].Format[0])
                   { case 'I': *((int*)(Buffer+Fields[Items].ValueOffset)) =
                               atoi(StringValue);
                          break;

                     case 'R': *((float*)(Buffer+Fields[Items].ValueOffset)) =
                               atof(StringValue);
                          break;

                     case 'D': scanf(StringValue,"%e",
                                     (double*)(Buffer+Fields[Items].ValueOffset)); 
                          break;

                     default: *ValidId = LBL_INVALID;
                              BadContinueLink = LBL_TRUE;
                          break;
                    }
               break;
        } else							/** String **/
        { strcpy((Buffer+Fields[Items].ValueOffset),StringValue);
          if (strlen(StringValue) != 3 && strlen(StringValue) != 4)
             *ValidId = LBL_VALID;
          else if (strcmp(StringValue,LBL_PDS_STRING_NULL) == 0)
             *ValidId = LBL_PDS_NULL;
          else if (strcmp(StringValue,LBL_PDS_STRING_NA) == 0)
             *ValidId = LBL_PDS_NA;
          else if (strcmp(StringValue,LBL_PDS_STRING_UNK) == 0)
             *ValidId = LBL_PDS_UNK;
          else *ValidId = LBL_VALID;
        }
      } else				/*  Label is only numeric  */
      { /***  Return the number of values associated with a label item if  ***/
        /***  the label element has defined a location to place the value  ***/
        if (Fields[Items].RtnElementOffset == LBL_NO_RETURN)
           status = zlget(Cntrl->FileUnit, Label->Type,
			Keyword,	(Buffer+Fields[Items].ValueOffset),
			"FORMAT",	Fields[Items].Format,
			"INSTANCE",	Instance,
			"ELEMENT",	Fields[Items].Element,
			"NELEMENT",	Fields[Items].MaxElements,
			"ERR_ACT",	"",
			Label->NameKeyword,	Label->NameValue, NULL);
        else status = zlget(Cntrl->FileUnit, Label->Type,
			Keyword,	(Buffer+Fields[Items].ValueOffset),
			"FORMAT",	Fields[Items].Format,
			"INSTANCE",	Instance,
			"ELEMENT",	Fields[Items].Element,
			"NELEMENT",	Fields[Items].MaxElements,
			"NRET",		(Buffer+Fields[Items].RtnElementOffset),
			"ERR_ACT",	"",
			Label->NameKeyword,	Label->NameValue, NULL);

        if (status == 1)
        { *(int*)(Buffer+Fields[Items].ValidOffset) = LBL_VALID;
          BadContinueLink = LBL_FALSE;
        } else BadContinueLink = LBL_TRUE;
      }
    } else if (LBL_CHK_PRESENT(*ValidId))
    /********************************************************************
     ********************************************************************
     ***
     ***  Write the label items to the file
     ***
     ********************************************************************
     *******************************************************************/
    { if (LBL_CHK_VALID(*ValidId))
      { if (StringWriteFlag)		/* Previous element is NULL/NA/UNK */
        { switch(Fields[Items].Format[0])
          { case 'S': strcpy(StringValue,(char*)(Buffer+Fields[Items].ValueOffset));
                 break;
            case 'I': sprintf(StringValue,"%d",
                              *((int*)(Buffer+Fields[Items].ValueOffset)));
                 break;
            case 'R': sprintf(StringValue,"%g",
                              *((float*)(Buffer+Fields[Items].ValueOffset)));
                 break;
            case 'D': sprintf(StringValue,"%.16g",
                              *((double*)(Buffer+Fields[Items].ValueOffset)));
                 break;
            default: strcpy(StringValue,LBL_PDS_STRING_UNK);
                 break;
          }
          /** may need to handle deleting some elements here as well as below **/
          status = zladd(Cntrl->FileUnit, Label->Type,
                         Keyword,        StringValue,
                         "FORMAT",       "STRING",
                         "INSTANCE",     Instance,
                         "NELEMENT",     1,
                         "ELEMENT",      Fields[Items].Element,
                         "MODE",         "REPLACE",
			 "ERR_ACT",	 "",
                         Label->NameKeyword,     Label->NameValue, NULL);
        } else 
        {
          status = zlinfo(Cntrl->FileUnit, Label->Type, Keyword,
                          LabelFormat, &MaxLength, &MaxElements,
                          "INSTANCE",    Instance,
			  "ERR_ACT", "",
                          Label->NameKeyword,    Label->NameValue, NULL);

          /** delete the existing keyword first before adding **/
          if ((Fields[Items].MaxElements<MaxElements) && (Fields[Items].Element==1)) 
          { status = zldel(Cntrl->FileUnit, Label->Type, Keyword,
                           "INSTANCE",  Instance,
			   "ERR_ACT", "",
                           Label->NameKeyword,  Label->NameValue, NULL);
          }

          status = zladd(Cntrl->FileUnit, Label->Type,
                        Keyword,        (Buffer+Fields[Items].ValueOffset),
                        "FORMAT",       Fields[Items].Format,
			"INSTANCE",	Instance,
			"NELEMENT",	Fields[Items].MaxElements,
			"ELEMENT",	Fields[Items].Element,
			"MODE",		"REPLACE",
			"ERR_ACT",	"",
			Label->NameKeyword,	Label->NameValue, NULL);
        }
      } else if (*ValidId == LBL_DELETE) 
      {
        status = zlinfo(Cntrl->FileUnit, Label->Type, Keyword,
                        LabelFormat, &MaxLength, &MaxElements,
                        "INSTANCE",    Instance,
			"ERR_ACT", "",
                        Label->NameKeyword,    Label->NameValue, NULL);

        status = zldel(Cntrl->FileUnit, Label->Type, Keyword,
                       "INSTANCE",  Instance,
                       "ELEMENT", Fields[Items].Element,
		       "ERR_ACT", "",
                       Label->NameKeyword,  Label->NameValue, NULL);

        /*** If a label item has multiple values and the deleted element above is in the 
         *** middle of the list, delete rest of the elements since the label item must   
         *** not have gaps.  
         ***/
        if (MaxElements > Fields[Items].Element) 
        { for (delItems=Fields[Items].Element+1; delItems<=MaxElements; delItems++) 
            status = zldel(Cntrl->FileUnit, Label->Type, Keyword,
                           "INSTANCE",       Instance,
                           "ELEMENT", delItems,
			   "ERR_ACT", "",
                           Label->NameKeyword,       Label->NameValue, NULL);
        }          
      } else
      { StringWriteFlag = 1;	/* Indicates remainging elements are strings */ 
        for (Element=0; Element<Fields[Items].MaxElements; Element++)
            switch (*ValidId)
        { case LBL_PDS_NULL:
               status = zladd(Cntrl->FileUnit, Label->Type,
                              Keyword,        LBL_PDS_STRING_NULL,
                              "FORMAT",       "STRING",
                              "INSTANCE",     Instance,
                              "NELEMENT",     1,
                              "ELEMENT",      Fields[Items].Element+Element,
                              "MODE",         "REPLACE",
			      "ERR_ACT",      "",
                              Label->NameKeyword,     Label->NameValue, NULL);
               break;

          case LBL_PDS_UNK:
               status = zladd(Cntrl->FileUnit, Label->Type,
                              Keyword,        LBL_PDS_STRING_UNK,
                              "FORMAT",       "STRING",
                              "INSTANCE",     Instance,
                              "NELEMENT",     1,
                              "ELEMENT",      Fields[Items].Element+Element,
                              "MODE",         "REPLACE",
			      "ERR_ACT",      "",
                              Label->NameKeyword,     Label->NameValue, NULL);
               break;

          case LBL_PDS_NA:
               status = zladd(Cntrl->FileUnit, Label->Type,
                              Keyword,        LBL_PDS_STRING_NA,
                              "FORMAT",       "STRING",
                              "INSTANCE",     Instance,
                              "NELEMENT",     1,
                              "ELEMENT",      Fields[Items].Element+Element,
                              "MODE",         "REPLACE",
			      "ERR_ACT",      "",
                              Label->NameKeyword,     Label->NameValue, NULL);
               break;

          default:
               sprintf(Cntrl->ErrMsgBuf,"Errorenous validity option (%d): %s",
                       (*ValidId),Keyword);
               status = RTN_LBL_UNSUPPORTED;
               break;
        }
      }

      if (status != 1) BadContinueLink = LBL_TRUE;
      break;				/* Only 1st keyword used for writes */
    } else	/***  Label value not supplied for writing this keyword  ***/
    { if (Fields[Items].Continuation)
         BadContinueLink = LBL_TRUE;	/* Subsequent Continues won't work */
      break;				/* Only 1st keyword used for writes */
    } while (status != 1 && --VersionIdx >= 0);	/* continue with keyword list */

    /****  probably should rethink error processing response  ***/

    if (status != 1)			 /* Label item error processing */
    { if ((Cntrl->Obtain == LBL_READ) || /* ANY read operation  */
          (Cntrl->Obtain == LBL_WRITE && /* Only if Required or error on write */
           (status != 0 || Fields[Items].Required)))
      { Cntrl->ErrorCount++;
        if (RtnStatus != RTN_LBL_MISSING_REQ)	/* Check for previous error */
        { if (Fields[Items].Required)
          { RtnStatus = RTN_LBL_MISSING_REQ;
            sprintf(Cntrl->ErrMsgBuf,"Error (%d) %s required label: %s",
                    status,(Cntrl->Obtain ? "reading" : "writing"),
                    PrimaryKeyword);
          } else if (RtnStatus == RTN_NORMAL)
          { RtnStatus = RTN_LBL_MISSING_OPT;
            sprintf(Cntrl->ErrMsgBuf,"Error (%d) processing label (%d): %s",
                    status,Items,PrimaryKeyword);
          }
          strcpy(ErrorBuffer,Cntrl->ErrMsgBuf);
          if (!Cntrl->ProceedOnError) return RtnStatus;
        }
      }
    } else
    { if (Fields[Items].KeywordUsed)
         strcpy(Fields[Items].KeywordUsed,Keyword);
    }
  }

  return RtnStatus;
}

/*******************************************************************************
 *				LBL_SET_VERSION
 *
 *	Sets the version of the keyword to be used
 ******************************************************************************/
int	LblSetVersion(
  const char	*Version )
{
  int	lc;
  char	*Name,
	NameList[1024];
  const char	*VersionNames[LBL_KEYWORD_VERSION] =
		{ LBL_VERSION_ONE, LBL_VERSION_TWO };

  for (lc=(LBL_KEYWORD_VERSION-1); lc>=0; lc--)
  { strcpy(NameList,VersionNames[lc]);
    Name = strtok(NameList,LBL_KEYWORD_DELIMITERS);
    do
    { if (strcmp(Name,Version) == 0)
      { SelectedVersion = lc + 1;
        return RTN_NORMAL;
      }
      Name = strtok(NULL,LBL_KEYWORD_DELIMITERS);
    } while (Name);
  }

  return RTN_INVLD_ARG;
}

/*******************************************************************************
 *				LBL_ERROR_MESSAGE
 *
 *	Returns an error message from the generic label API
 ******************************************************************************/
const char	*LblErrorMessage( void )
{
  return (ErrorBuffer);
}

/*******************************************************************************
 *				PRINT_LABEL_ELEMENTS
 *
 *	Routine is basically used for testing and debugging the implementation
 * of a label API and not for use by indivifual applications.
 ******************************************************************************/
void	PrintLabelElements(
  LblApiProcess_typ	*Label)
{ int	lc,
	lc_lf,
	ValueIdx,
	*ValidId,
	status,
	MaxLength,
	MaxElements;
  char	*Keyword,
	*Buffer = Label->Buffer,
  	KeywordList[LBL_KEYWORDLIST_LTH];
  LblApiElement_typ	*Fields = Label->Table;

  char	*LabelFormat[] = { "INT", "REAL", "DOUB", "STRING", "" };

  printf("\n\nLabel Type: %s\n",Label->Type);
  printf("%s Name: %s\n",Label->NameKeyword,Label->NameValue);


  for (lc=0; Fields[lc].KeywordList; lc++)
  {
    /***  Print out General info about the label item  ***/
    /***  Keyword, Format, max elements, etc  ***/

    if (Fields[lc].Continuation)
    { printf("  + (%02d at %02d): ",
             Fields[lc].MaxElements,Fields[lc].Element);
    } else
    { if (Fields[lc].Required)
         printf("\nKeywords (Req): ");
      else printf("\nKeywords (Opt): ");
      printf("%s\n",Fields[lc].KeywordList);
      if (Fields[lc].KeywordUsed)
         printf("         Found: %s\n",Fields[lc].KeywordUsed);
/***  Extra print statement that just adds clutter
      else printf("         Found: *** Not Requested ***\n");
 ***/
      printf("        Format: %s [%d] (%d)\n",
             Fields[lc].Format,Fields[lc].MaxElements,
             Fields[lc].MemoryAllocated);
      printf("        Values: ");
    }

    /***  Print out the value(s) of the label item  ***/
    ValidId = (int*)(Buffer+Fields[lc].ValidOffset);

    if (!LBL_CHK_PRESENT(*ValidId))
    { printf("<NULL>\n");
      continue;
    }


    /* Determine Label value format: int, real, doub, string */
    for (lc_lf=0; strlen(LabelFormat[lc_lf]); lc_lf++)
        if (strcmp(Fields[lc].Format,LabelFormat[lc_lf]) == 0) break;

    /* Determine number of values associated with keyword */
    if (Fields[lc].RtnElementOffset == LBL_NO_RETURN)
       MaxElements = Fields[lc].MaxElements;
    else
    { MaxElements = *(int*)(Buffer+Fields[lc].RtnElementOffset);
      if (MaxElements == 0) MaxElements = Fields[lc].MaxElements;
    }

    for (ValueIdx=0; ValueIdx<MaxElements; ValueIdx++)
    { if (ValueIdx > 0) printf("                ");
      if (LBL_CHK_VALID(*ValidId))
         switch (lc_lf)
      { case 0: /* INT */
                printf("%d\n",*((int*)(Buffer+Fields[lc].ValueOffset)+ValueIdx));
          break;

        case 1: /* REAL */
                printf("%f\n",*((float*)(Buffer+Fields[lc].ValueOffset)+ValueIdx));
          break;

        case 2: /* DOUB */
                printf("%e\n",*((double*)(Buffer+Fields[lc].ValueOffset)+ValueIdx));
          break;

        case 3: /* STRING */
                printf("%s\n",((char*)(Buffer+Fields[lc].ValueOffset)+ValueIdx));
          break;

        default: printf(" ***  Undeterminable  ***\n");
          break;
      } else
         switch (*ValidId)
      { case LBL_PDS_NULL:
                printf("'%s'\n",LBL_PDS_STRING_NULL);
          break;

        case LBL_PDS_UNK:
                printf("'%s'\n",LBL_PDS_STRING_UNK);
          break;

        case LBL_PDS_NA:
                printf("'%s'\n",LBL_PDS_STRING_NA);
          break;

        default: printf("Undefined valid flag: %d\n",*ValidId);
          break;
      }
    }
  }
  printf("\n");

  return;
}

/*******************************************************************************
 *				TEST_LOAD_LABEL_ELEMENTS
 *
 *	Routine is basically used for testing and debugging the implementation
 * of a label API and not for use by individual applications.
 ******************************************************************************/
void	TestLoadLabelElements(
  LblApiProcess_typ	*Label)
{ int	BaseI = 0,
	lc,
	lc_lf,
	ValueIdx,
	status,
	MaxLength,
	MaxElements;
  char	BaseC[8] = "AAAAAA",
	*Buffer = Label->Buffer;
  float	BaseR = 100.0;
  double	BaseD = 10000.0;
  LblApiElement_typ	*Fields = Label->Table;

  char	*LabelFormat[] = { "INT", "REAL", "DOUB", "STRING", "" };

  for (lc=0; Fields[lc].KeywordList; lc++)
  {
    /* Determine Label value format: int, real, doub, string */
    for (lc_lf=0; strlen(LabelFormat[lc_lf]); lc_lf++)
        if (strcmp(Fields[lc].Format,LabelFormat[lc_lf]) == 0) break;

    /* Determine number of values associated with keyword */
    if (Fields[lc].RtnElementOffset == LBL_NO_RETURN)
       MaxElements = Fields[lc].MaxElements;
    else
    { MaxElements = *(int*)(Buffer+Fields[lc].RtnElementOffset);
      if (MaxElements == 0) MaxElements = Fields[lc].MaxElements;
    }

    for (ValueIdx=0; ValueIdx<MaxElements; ValueIdx++)
    { *(int*)(Buffer+Fields[lc].ValidOffset) = LBL_VALID;
      switch (lc_lf)
      { case 0: /* INT */
                *((int*)(Buffer+Fields[lc].ValueOffset)+ValueIdx) = BaseI;
                BaseI++;
          break;

        case 1: /* REAL */
                *((float*)(Buffer+Fields[lc].ValueOffset)+ValueIdx) = BaseR;
		BaseR += 10.0;
          break;

        case 2: /* DOUB */
                *((double*)(Buffer+Fields[lc].ValueOffset)+ValueIdx) = BaseD;
		BaseD += 1000.0;
          break;

        case 3: /* STRING */
                strcpy(((char*)(Buffer+Fields[lc].ValueOffset)+ValueIdx),
                       BaseC);
                if (BaseC[5] == 'Z') BaseC[5] = 'a' - 1;
                if (BaseC[5] == 'z') BaseC[5] = '0' - 1;
                if (BaseC[5] == '9') BaseC[5] = 'A' - 1;
                BaseC[0] = BaseC[1] = BaseC[2] = BaseC[3] =
                           BaseC[4] = BaseC[5] += 1; 
          break;

        default: *(int*)(Buffer+Fields[lc].ValidOffset) = LBL_INVALID;
          break;
      }
    }
  }

  return;
}
