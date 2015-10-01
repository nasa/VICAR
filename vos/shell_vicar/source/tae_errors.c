#include "tae_lib.h"
#include "zvproto.h"
#include <string.h>
#include "taeextproto.h"
/*
tae_abort takes the error code together with an optional 
string value. prints out the associated message and aborts.
*/


void tae_abort(int code, char *val)
{
   static struct
   {
      char *key;
      char *text;
   } tae_err_msg[] =
   {

	
     {"AMBIGPAR",	"Ambiguous parameter abbreviation \'%s\'."},
     {"AMBIGSUB",	"Ambiguous subcommand abbreviation \'%s\'."},
     {"AMBIGVAL",	"Ambiguous value for \'%s\'."},
     {"BADFILE",	"File not found or incorrect file specification for \'%s\'."},
     {"BADPAR",	"\'%s\' is an undefined parameter or unknown qualifier."},
     {"BADPFILE",	"Bad parameter file specification, \'%s\'."},
     {"BADREF",	"Undefined DEFAULT variable reference \'%s\'."},
     {"BADVALCT",	"Too many or too few values for variable \'%s\'."},
     {"CNTERR",	"Invalid COUNT specification for \'%s\'."},
     {"DFTERR",	"Invalid DEFAULT or INITIAL specification for \'%s\'."},
     {"EXPR",		"Unexpected characters in expression"},
     {"FMTRESTR",	"Parameter file not properly formatted."},
     {"INVINT",	"Invalid integer value for \'%s\'."},
     {"INVNAME",	"Invalid variable name \'%s\'."},
     {"INVPNAME",	"Invalid parameter name, \'%s\'."},
     {"INVPVAL",	"Invalid parameter value for variable \'%s\'."},
     {"INVREAL",	"Invalid realvalue for \'%s\'."},
     {"INVSTR",	"Value not one of the acceptable strings defined for \'%s\'."},
     {"INVSUBNAME","Error in command field format & Invalid variable name in substitution"},
     {"INVVAL",	"Value not one of the valid values defined for \'%s\'."},
     {"KEYSYNTAX","Invalid keyword syntax"},
     {"KEYWORD",	"Ambiguous or unknown keyword \'%s\'."},
     {"MISINTRO",	"Introductory command (PROCEDURE, PROCESS, ...) missing."},
     {"MISPAR",	"Missing parameter:  \'%s\'."},
     {"MIXTYPE",	"Type of value does not match type of variable on left."},
     {"MIXVAR",	"Mixed data types."},
     {"MULVAR",	"Numeric or string operation using multiple values."},
     {"NAMERR",	"VALID, ACCESS, and COUNT not allowed with TYPE = NAME"},
     {"NESTMUL",	"Multivalued expression within another multivalued expression."},
     {"NOGLB",	"Global variable \'%s\' does not exist."},
     {"NOPARM",	"The \'%s\' command has no parameters."},
     {"NOTINPDF",	"\'%s\' not allowed in proc definition file."},
     {"NOTNULL",	"\'%s\' does not accept a null value."},
     {"NOTUSED",	"\'%s\' not Implemented."},
     {"NOVAL",	"Variable \'%s\' has no current value."},
     {"NOVALUE",	"Variable \'%s\' has no value."},
     {"NULLARG",	"Null value not permitted as an argument to \'%s\'."},
     {"NULLELEM",	"Null value not permittedas an element in a list."},
     {"NUMNULL",	"Nullvalues not permitted in numeric expressions."},
     {"PARCREATE","Unable to create parameter file. \'%s\'."},
     {"PARS",		"Too many parameter values for this command or too many qualifiers."},
     {"POSERR",	"Positional values may not follow values specified by name."},
     {"PSETOVER",	"Parameter values overflow process message capacity."},
     {"RANGE",	"Value out of the defined range for \'%s\'."},
     {"REFEX",	"Variable \'%s\' previously defined or referenced."},
     {"SCRCNT",	"COUNT parameter must be positive or zero."},
     {"STRSIZ",	"Invalid string size specification for \'%s\'."},
     {"SUBREQ",	"Subcommand is required for \'%s\'."},
     {"SUBTWC",	"Subcommand \'%s\' specified twice."},
     {"TOOFEWVAL","Too few values for variable \'%s\'."},
     {"TOOMANYVAL","Too many values for variable \'%s\' ."},
     {"TYPERR",	"Invalid TYPE specification for \'%s\'."},
     {"UNDEFSUB",	"Undefined or ambiguous subcommand \'%s\'."},
     {"UNDEFVAR",	"Reference to undefined variable \'%s\'."},
     {"UNEXEOL",	"Unexpected end of line."},
     {"VALIDREQ",	"VALID specification required for KEYWORD parameter \'%s\'."},
     {"VARCNFLCT","\'%s\' name conflicts with existing variable."}
   };

   static char msgbuf[200];

   if (code<0)
      	if (code >= LAST_TAE_ERR)
	  	{
			 char err_key[18];
			
			 strcpy(err_key,"TAE-");
			 strcat(err_key,tae_err_msg[TAE_ERR_BASE-code].key);
			 sprintf(msgbuf,
				tae_err_msg[TAE_ERR_BASE-code].text,val );
			
			 zvmessage(msgbuf, err_key);
			 
			 z_exit(code,"");
   		}

   return;
}
