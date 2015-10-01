$!****************************************************************************
$!
$! Build proc for MIPL module difpic
$! VPACK Version 1.9, Monday, November 07, 2011, 10:50:59
$!
$! Execute by entering:		$ @difpic
$!
$! The primary option controls how much is to be built.  It must be in
$! the first parameter.  Only the capitalized letters below are necessary.
$!
$! Primary options are:
$!   COMPile     Compile the program modules
$!   ALL         Build a private version, and unpack the PDF and DOC files.
$!   STD         Build a private version, and unpack the PDF file(s).
$!   SYStem      Build the system version with the CLEAN option, and
$!               unpack the PDF and DOC files.
$!   CLEAN       Clean (delete/purge) parts of the code, see secondary options
$!   UNPACK      All files are created.
$!   REPACK      Only the repack file is created.
$!   SOURCE      Only the source files are created.
$!   SORC        Only the source files are created.
$!               (This parameter is left in for backward compatibility).
$!   PDF         Only the PDF file is created.
$!   TEST        Only the test files are created.
$!   IMAKE       Only the IMAKE file (used with the VIMAKE program) is created.
$!
$!   The default is to use the STD parameter if none is provided.
$!
$!****************************************************************************
$!
$! The secondary options modify how the primary option is performed.
$! Note that secondary options apply to particular primary options,
$! listed below.  If more than one secondary is desired, separate them by
$! commas so the entire list is in a single parameter.
$!
$! Secondary options are:
$! COMPile,ALL:
$!   DEBug      Compile for debug               (/debug/noopt)
$!   PROfile    Compile for PCA                 (/debug)
$!   LISt       Generate a list file            (/list)
$!   LISTALL    Generate a full list            (/show=all)   (implies LIST)
$! CLEAN:
$!   OBJ        Delete object and list files, and purge executable (default)
$!   SRC        Delete source and make files
$!
$!****************************************************************************
$!
$ write sys$output "*** module difpic ***"
$!
$ Create_Source = ""
$ Create_Repack =""
$ Create_PDF = ""
$ Create_Test = ""
$ Create_Imake = ""
$ Do_Make = ""
$!
$! Parse the primary option, which must be in p1.
$ primary = f$edit(p1,"UPCASE,TRIM")
$ if (primary.eqs."") then primary = " "
$ secondary = f$edit(p2,"UPCASE,TRIM")
$!
$ if primary .eqs. "UNPACK" then gosub Set_Unpack_Options
$ if (f$locate("COMP", primary) .eqs. 0) then gosub Set_Exe_Options
$ if (f$locate("ALL", primary) .eqs. 0) then gosub Set_All_Options
$ if (f$locate("STD", primary) .eqs. 0) then gosub Set_Default_Options
$ if (f$locate("SYS", primary) .eqs. 0) then gosub Set_Sys_Options
$ if primary .eqs. " " then gosub Set_Default_Options
$ if primary .eqs. "REPACK" then Create_Repack = "Y"
$ if primary .eqs. "SORC" .or. primary .eqs. "SOURCE" then Create_Source = "Y"
$ if primary .eqs. "PDF" then Create_PDF = "Y"
$ if primary .eqs. "TEST" then Create_Test = "Y"
$ if primary .eqs. "IMAKE" then Create_Imake = "Y"
$ if (f$locate("CLEAN", primary) .eqs. 0) then Do_Make = "Y"
$!
$ if (Create_Source .or. Create_Repack .or. Create_PDF .or. Create_Test .or -
        Create_Imake .or. Do_Make) -
        then goto Parameter_Okay
$ write sys$output "Invalid argument given to difpic.com file -- ", primary
$ write sys$output "For a list of valid arguments, please see the header of"
$ write sys$output "of this .com file."
$ exit
$!
$Parameter_Okay:
$ if Create_Repack then gosub Repack_File
$ if Create_Source then gosub Source_File
$ if Create_PDF then gosub PDF_File
$ if Create_Test then gosub Test_File
$ if Create_Imake then gosub Imake_File
$ if Do_Make then gosub Run_Make_File
$ exit
$!
$ Set_Unpack_Options:
$   Create_Repack = "Y"
$   Create_Source = "Y"
$   Create_PDF = "Y"
$   Create_Test = "Y"
$   Create_Imake = "Y"
$ Return
$!
$ Set_EXE_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$ Return
$!
$ Set_Default_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_All_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Do_Make = "Y"
$   Create_PDF = "Y"
$ Return
$!
$ Set_Sys_Options:
$   Create_Source = "Y"
$   Create_Imake = "Y"
$   Create_PDF = "Y"
$   Do_Make = "Y"
$ Return
$!
$Run_Make_File:
$   if F$SEARCH("difpic.imake") .nes. ""
$   then
$      vimake difpic
$      purge difpic.bld
$   else
$      if F$SEARCH("difpic.bld") .eqs. ""
$      then
$         gosub Imake_File
$         vimake difpic
$      else
$      endif
$   endif
$   if (primary .eqs. " ")
$   then
$      @difpic.bld "STD"
$   else
$      @difpic.bld "''primary'" "''secondary'"
$   endif
$ Return
$!#############################################################################
$Repack_File:
$ create difpic.repack
$ DECK/DOLLARS="$ VOKAGLEVE"
$ vpack difpic.com -mixed -
	-s difpic.cc VicarImageUtil.cc VicarImageLabel.cc VicarHistoryLabel.cc -
	   DifpicParameters.cc difpic_bridges.cc difpic_pixel.f -
	   difpic_bridges.h DifpicParameters.h VicarImageUtil.h -
	   VicarImageLabel.h VicarHistoryLabel.h -
	-p difpic.pdf -
	-i difpic.imake -
	-t tstdifpic.pdf
$ Exit
$ VOKAGLEVE
$ Return
$!#############################################################################
$Source_File:
$ create difpic.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "DifpicParameters.h"
#include "difpic_bridges.h"
#include "VicarImageUtil.h"
#include <iostream>

extern "C"
{
#include "xvmaininc.h"
#include "applic.h"
#include "ftnbridge.h"
#include "taeextproto.h"
#include "parblk.inc"
#include "vicmain_c"
}

using namespace std;

void main44()
{
	static const int LABEL_DIFFERS  = 1;
	static const int BINHDR_DIFFERS = 2;
	static const int HIST_DIFFERS   = 4;
	static const int LPFX_DIFFERS   = 8;
	static const int PIX_DIFFERS    = 16;

	int status=0;
        struct PARBLK par_block;

	//zvmessage(const_cast<char*>(jpl::mipl::p2::difpic::VicarImageUtil::getVersion().c_str()),"");
	zvmessage(const_cast<char*>(jpl::mipl::p2::difpic::VicarImageUtil::getVersion().c_str()),NULL);

	if (!labeldifC())
	{
		//cerr<<"Will call label diff"<<endl;
		status = LABEL_DIFFERS;
	}

	if (!binaryheaderdifC())
	{
		//cerr<<"Will call binhdr diff"<<endl;
		status |= BINHDR_DIFFERS;
	}
	if (!lineprefixdifC())
	{
		//cerr<<"Will call lp diff"<<endl;
		status |= LPFX_DIFFERS;
	}

	if (!histdifC())
	{
		//cerr<<"Will call hist diff"<<endl;
		status |= HIST_DIFFERS;
	}

	int tstatus=0;
	if (jpl::mipl::p2::difpic::DifpicParameters::instance()->pixDiffEnabled())
	{
		//cerr<<"Will call pix diff"<<endl;
		FTN_NAME2(main44_ftn,MAIN44_FTN)(&tstatus);
		if (tstatus != 0)
			status |= PIX_DIFFERS;
	}


        // Send return value
        q_init (&par_block, P_BYTES, P_ABORT);
        q_intg(&par_block, "RETVAL", 1, &status, P_ADD);
        zvq_out(&par_block);

        return;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create VicarImageUtil.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
#include "VicarImageUtil.h"
using namespace std;

//====================================================================================
//====================================================================================
//====================================================================================
string
jpl::mipl::p2::difpic::
VicarImageUtil::getVersion()
{
  return "DIFPIC version 06Oct11";
}
//====================================================================================
//====================================================================================
//====================================================================================
bool
jpl::mipl::p2::difpic::
VicarImageUtil::contains(const vector<string> &v, const string &element)
{
  vector<string>::const_iterator vit;
  for (vit=v.begin(); vit!=v.end(); ++vit)
    if ((*vit).compare(element)==0) return true;
  return false;
}

//====================================================================================
//====================================================================================
//====================================================================================
void
jpl::mipl::p2::difpic::VicarImageUtil::
clone(const map < string, map < string, vector < string >* >* > &from,
            map < string, map < string, vector < string >* >* > &to)
{
  map < string, map < string, vector < string >* >* >::const_iterator fit;
  for (fit=from.begin(); fit!=from.end(); ++fit)
  {
    map < string, vector < string >* >* tv = new map < string, vector < string >* >();
    jpl::mipl::p2::difpic::VicarImageUtil::clone(*fit->second,*tv);
    to.insert(pair<string, map < string, vector < string >* >* >(fit->first,tv));
  }
}

//====================================================================================
//====================================================================================
//====================================================================================
void
jpl::mipl::p2::difpic::VicarImageUtil::
clone(const map < string, vector < string >* > &from,
            map < string, vector < string >* > &to)
{
  map < string, vector < string >* >::const_iterator fit;
  for (fit=from.begin(); fit!=from.end(); ++fit)
  {
    vector < string >* nv = new vector < string >();
    nv->assign(fit->second->begin(), fit->second->end());
    to.insert(pair<string, vector < string >* >(fit->first,nv)); 
  }
}


//====================================================================================
//====================================================================================
//====================================================================================
void
jpl::mipl::p2::difpic::VicarImageUtil::
formatDMsg(const string &label, const string &rval, const string &lval, string &outMsg)
{
  string fmsg(label);
  fmsg.append(" : ").append(rval).append(" <> ").append(lval).append("\n");
  outMsg.append(fmsg);
}
#if 0
{
  int rhl = rval.size();
  int lhl = lval.size();

  int rhp = 0;
  int lhp = 0;

  //string fmsg(80,' ');
  string fmsg;
  size_t ll = MAX_LABEL_KEY_SIZE-label.size();
  fmsg.append(ll,' ').append(label).append(" : ");
  //cerr<<"fmsg.size="<<fmsg.size()<<endl;
  //fmsg.insert(ll,label).insert(ll+label.size()," : ");
  bool firsttime=true;
  while (rhp < rhl || lhp < lhl)
  {
    if (rhp < rhl)
    {
      int rsize = rhl-rhp;
      int psize = rsize > 20 ? 0 : 20-rsize;
      fmsg.append(psize,' ').append(rval,rhp,20);
      //if (label.compare("IMAGE_MID_TIME")==0)
      //cerr<<"psize="<<psize<<" rsize="<<rsize<<" fmsg="<<fmsg<<endl;
      //fmsg.insert(35+psize,rval,rhp,20);
      rhp+=20;
    }
    else
      fmsg.append(20,' ');
    //fmsg.insert(55," <> ");
    if (firsttime)
    {
      fmsg.append(" <> ");
      firsttime=false;
    }
    else
      fmsg.append("    ");
    //if (label.compare("IMAGE_MID_TIME")==0)
    //cerr<<"size="<<fmsg.size()<<" fmsg="<<fmsg<<endl;

    if (lhp < lhl)
    {
      int rsize = lhl-lhp;
      //size_t psize = rsize > 20 ? 0 : 20-rsize;
      //fmsg.insert(59,lval,lhp,20);
      fmsg.append(lval,lhp,20);
      lhp+=20;
    }
    //if (label.compare("IMAGE_MID_TIME")==0)
    //cerr<<"size="<<fmsg.size()<<" fmsg="<<fmsg<<endl;
    outMsg.append(fmsg).append("\n");
    fmsg.erase();
    fmsg.append(35,' ');
  }
}
#endif
//====================================================================================
//====================================================================================
//====================================================================================
void
jpl::mipl::p2::difpic::VicarImageUtil::
formatHdrMsg(const string &hdr, string &outMsg1)
{
  string outMsg;
  int psize=((80-16-hdr.size())/2);
  //cerr<<"hdr="<<hdr.size()<<" psize="<<psize<<endl;
  int i;
  outMsg.append("\n");
  for (i=1; i<psize-1; ++i)
    outMsg.append(" ");
  outMsg.append("------- ").append(hdr).append(" -------");
  outMsg.append("\n");
  outMsg1.append(outMsg);
}
//====================================================================================
//====================================================================================
//====================================================================================
bool
jpl::mipl::p2::difpic::VicarImageUtil::
processExtraRHLabels(map < string, vector < string >* >& labels,
		   const vector < string >& labelsToIgnore,
		   string & differences)
{
  bool sameLabels=true;
  string emptyString("<LABEL MISSING>");
  map < string, vector < string >* >::iterator lit = labels.begin();
  while (lit!=labels.end())
  {
    if (!jpl::mipl::p2::difpic::VicarImageUtil::contains(labelsToIgnore,lit->first))
    {
      string lval = jpl::mipl::p2::difpic::VicarImageUtil::getValue(*lit->second);
      jpl::mipl::p2::difpic::VicarImageUtil::formatDMsg(lit->first,emptyString,lval,differences);
      sameLabels=false;
    }
    labels.erase(lit->first);
    lit=labels.begin();
  }
  return sameLabels;
}

//====================================================================================
//====================================================================================
//====================================================================================
bool
jpl::mipl::p2::difpic::VicarImageUtil::
processExtraRHProperties(map < string, map < string, vector < string >* >* > &labels,
			  const vector < string >& propertiesToIgnore,
			  const vector < string >& labelsToIgnore,
			  string & differences)
{
  bool sameLabels=true;
  map < string, map < string, vector < string >* >* >::iterator lit = labels.begin();
  while (lit!=labels.end())
  {
    if (!jpl::mipl::p2::difpic::VicarImageUtil::contains(propertiesToIgnore,lit->first))
    {
      jpl::mipl::p2::difpic::VicarImageUtil::formatHdrMsg(lit->first,differences);
      if (!jpl::mipl::p2::difpic::VicarImageUtil::processExtraRHLabels(*lit->second,labelsToIgnore,differences))
	sameLabels=false;
    }
    labels.erase(lit->first);
    lit=labels.begin();
  }
  return sameLabels;
}
//====================================================================================
//====================================================================================
//====================================================================================
string
jpl::mipl::p2::difpic::VicarImageUtil::
getValue(const vector <string >& v)
{
  string value;
  if (v.size()>1) value.append("(");
  value.append(v[0]);
  for (int i=1; i<v.size(); ++i)
    value.append(",").append(v[i]);
  if (v.size()>1) value.append(")");
  return value;
}


$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create VicarImageLabel.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
       Copyright 2011-Present
       California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledged. 09-08-2011.

       Auther : Rajesh R. Patel

History :
---------

Date              Who                        What
-------------------------------------------------------------------------------
09/08/2011        Raj       Initial Release

 */

#include "VicarImageLabel.h"
#include "VicarImageUtil.h"
#include <cstring>
#include <cstdio>
#include <iostream>

//defines constants for VICAR label key and value size, max properties, etc.
// defines.h has some horrible macros such as "U", and other single
// letters. "U" happens to be a template class in C++ standard library,
// so putting this include first may cause the compilation to break.
extern "C"
{
#include "defines.h"
}
using namespace jpl::mipl::p2;
using namespace std;

//====================================================================================
//====================================================================================
//====================================================================================
jpl::mipl::p2::VicarImageLabel::VicarImageLabel(int unitNo)
throw (VicarException)
{
	this->unitNo_=unitNo;
	try
	{
		this->init_();
	}
	catch (const VicarException & e)
	{
		this->cleanUp();
		throw e;
	}
}
//====================================================================================
//====================================================================================
//====================================================================================
jpl::mipl::p2::VicarImageLabel::
~VicarImageLabel()
{
	this->cleanUp();
}

//====================================================================================
//====================================================================================
//====================================================================================
void
jpl::mipl::p2::VicarImageLabel::
cleanUp()
{
	map < string, map < string, vector < string >* >* >::iterator pit = this->labels_.begin();

	//for (pit=this->labels_.begin(); pit != this->labels_.end(); ++pit)
	while (pit!=this->labels_.end())
	{
		//cerr<<"Will clean up "<<pit->first<<endl;
		map < string, vector < string >* >* v = pit->second;

		map < string, vector < string >* >::iterator kpit = v->begin();
		//for (; kpit!=v->end(); ++kpit)
		while (kpit!=v->end())
		{
			//cerr<<"\tDeleting "<<kpit->first<<endl;
			kpit->second->clear();
			delete kpit->second;
			//kpit->second=NULL;
			v->erase(kpit);
			kpit=v->begin();
		}
		//v->clear();
		delete v;
		this->labels_.erase(pit);
		pit=this->labels_.begin();
	}
}

//====================================================================================
//====================================================================================
//====================================================================================
void
jpl::mipl::p2::VicarImageLabel::listLabels(string &inThisString) const
{
	map < string, map < string, vector < string >* >* >::const_iterator pit;

	for (pit=this->labels_.begin(); pit != this->labels_.end(); ++pit)
	{
		inThisString.append("---------- ").append(pit->first).append(" ----------\n");
		map < string, vector < string >* >* v = pit->second;

		map < string, vector < string >* >::const_iterator kpit = v->begin();
		for (; kpit!=v->end(); ++kpit)
			inThisString.append("\t").append(kpit->first).append("=").append(jpl::mipl::p2::difpic::VicarImageUtil::getValue(*kpit->second));
	}
}

//====================================================================================
//====================================================================================
//====================================================================================
void
jpl::mipl::p2::VicarImageLabel::init_()
throw (VicarException)
{
	char props[MAX_PROPS][MAX_LABEL_KEY_SIZE+1];
	int  inst_num[MAX_PROPS];

	int noProps=-1;
	int max_sets=MAX_PROPS;

	int status = zlpinfo(this->unitNo_,(char*)props,&max_sets,
			"NRET",&noProps,
			"ULEN",MAX_LABEL_KEY_SIZE+1,
			"INST_NUM",inst_num,
			NULL);
	if (status != SUCCESS)
		throw VicarException("Error obtaining number of property items from image 1");

	if (noProps>MAX_PROPS)
	{
		char msg[100];
		sprintf(msg,"Image has %d property sets; max allowed is %d",noProps, MAX_PROPS);
		string emsg("Property labels in first image exceeds max allowed property sets. ");
		emsg.append(msg).append(".");
		throw VicarException(emsg.c_str());
	}

	//cerr<<"Total number of property items="<<noProps<<endl;

	char format[8+1];
	int maxLen, noElements, dummy;

	for (int pno=0; pno<noProps; ++pno)
	{
		//ignore TASK and HISTORY properties....
		//if (strcmp(props[pno],"TASK")==0 || strcmp(props[pno],"HISTORY")) continue;
		//cerr<<"Working on property label="<<props[pno]<<endl;
		status = zlinfo(this->unitNo_,const_cast<char*>("PROPERTY"),
				const_cast<char*>("PROPERTY"),format,&dummy,
				&noElements,"PROPERTY",props[pno], "INSTANCE",inst_num[pno],
				"STRLEN", &maxLen, NULL);
		//cerr<<"Property="<<props[pno]<<" instance="<<inst_num[pno]<<" noElem="<<noElements<<endl;
		map < string, map < string, vector < string >* >* >::iterator pit = this->labels_.find(props[pno]);
		map < string, vector < string >* >* val;
		if (pit == this->labels_.end())
		{
			val = new map < string, vector < string >* >();
			this->labels_.insert(pair< string, map < string, vector < string >* >* >(props[pno], val) );
		}
		else
			val = pit->second;

		char key[MAX_LABEL_KEY_SIZE+1];
		char value[MAX_LABEL_VALUE_SIZE+1];

		while (TRUE)
		{
			status = zlninfo(this->unitNo_,key,format,&dummy,&noElements,
					"STRLEN",&maxLen,NULL);
			//cerr<<"\tkey="<<key<<" maxlen="<<maxLen<<" noElements="<<noElements<<endl;
			if (status != SUCCESS)
				throw VicarException("Error getting information about next key");
			if ((status == END_OF_LABEL) || (strcmp(key,"TASK") == 0) ||
					(strcmp(key,"PROPERTY") == 0)) {
				//cerr<<"Will break."<<endl;
				break; }
			vector < string >* values;
			map < string, vector < string >* >::iterator vit = val->find(key);
			if (vit == val->end())
			{
				values = new vector < string > ();
				val->insert(pair< string, vector < string >* >(key,values));
			}
			else
				values = vit->second;

			//cerr<<"\tWorking on key="<<key<<endl;
			//char *value = new char[maxLen+1];
			for (int ino=1; ino<=noElements; ++ino)
			{
				status = zlget(this->unitNo_,const_cast<char*>("PROPERTY"),key,value,
						const_cast<char*>("PROPERTY"),props[pno],
						"INSTANCE",inst_num[pno],
						"FORMAT","STRING",
						"ULEN", MAX_LABEL_VALUE_SIZE,
						"ELEMENT",
						ino, NULL);
				values->push_back(value);
			}
			//delete [] value;
		}
	}
	//cerr<<"End of labels."<<endl;
}


//====================================================================================
//====================================================================================
//====================================================================================
bool
jpl::mipl::p2::VicarImageLabel::compare(const VicarImageLabel &toThisLabels,
		const vector <string> &propertiesToIgnore,
		const vector <string> &labelsToIgnore,
		string& differences) const
{
	map < string, map < string, vector < string >* >* > MLabels;
	map < string, map < string, vector < string >* >* > OLabels;

	jpl::mipl::p2::difpic::VicarImageUtil::clone(this->labels_,MLabels);
	jpl::mipl::p2::difpic::VicarImageUtil::clone(toThisLabels.labels_,OLabels);

	bool try1 = this->compare(MLabels,OLabels,propertiesToIgnore,labelsToIgnore,differences);

	//cerr<<"try1="<<try1<<endl;
	map < string, map < string, vector < string >* >* > MLabels2;
	map < string, map < string, vector < string >* >* > OLabels2;

	jpl::mipl::p2::difpic::VicarImageUtil::clone(OLabels,MLabels2);
	jpl::mipl::p2::difpic::VicarImageUtil::clone(MLabels,OLabels2);

	bool try2 = this->compare(OLabels2,MLabels2,propertiesToIgnore,labelsToIgnore,differences);
	//cerr<<"try2="<<try2<<endl;

	return try1 && try2;
}


//====================================================================================
//====================================================================================
//====================================================================================
bool
jpl::mipl::p2::VicarImageLabel::
compare(map < string, map < string, vector < string >* >* > &lh,
		map < string, map < string, vector < string >* >* > &rh,
		const vector <string> &propertiesToIgnore,
		const vector <string> &labelsToIgnore,
		string& differences) const
{
	//map <string, vector < string > > diffs;
	bool sameLabels = true;
	string emptyString("<LABEL MISSING>");
	map < string, map < string, vector < string >* >* >::const_iterator lhit = lh.begin();
	while (lhit!=lh.end())
	{//loop over all properties
		if (jpl::mipl::p2::difpic::VicarImageUtil::contains(propertiesToIgnore,lhit->first))
		{
			lh.erase(lhit->first);
			if (rh.count(lhit->first)!=0)
				rh.erase(lhit->first);
			lhit=lh.begin();
			continue;
		}
		map < string, vector < string >* >* lhkv = lhit->second;
		map < string, vector < string >* >::const_iterator lhkvit = lhkv->begin();
		map < string, map < string, vector < string >* >* >::const_iterator rhit = rh.find(lhit->first);
		if (rhit==rh.end())
		{// property missing in right hand image
			jpl::mipl::p2::difpic::VicarImageUtil::formatHdrMsg(lhit->first,differences);
			for (; lhkvit!=lhkv->end(); ++lhkvit)
			{
				string lhval = jpl::mipl::p2::difpic::VicarImageUtil::getValue(*lhkvit->second);
				jpl::mipl::p2::difpic::VicarImageUtil::formatDMsg(lhkvit->first,lhval,emptyString,differences);
			}
			sameLabels=false;

			//differences.append(
			//cerr<<"Missing property "<<lhit->first<<" from right hand image"<<endl;
		}
		else
		{// property found on right hand image
			bool doneProp = false;

			map < string, vector < string >* >* rhkv =rhit->second;

			while (lhkvit!=lhkv->end())
			{// while loop over all labels of a property
				map < string, vector < string >* >::const_iterator rhkvit = rhkv->find(lhkvit->first);
				if (jpl::mipl::p2::difpic::VicarImageUtil::contains(labelsToIgnore,lhkvit->first))
				{ // key in set of keys to ignore
					lhkv->erase(lhkvit->first);
					if (rhkvit!=rhkv->end())
						rhkv->erase(rhkvit->first);
					lhkvit=lhkv->begin();
					continue;
				}
				string lhval = jpl::mipl::p2::difpic::VicarImageUtil::getValue(*lhkvit->second);
				if (rhkvit==rhkv->end())
				{ // label not in right hand image
					if (!doneProp)
					{
						doneProp = true;
						jpl::mipl::p2::difpic::VicarImageUtil::formatHdrMsg(lhit->first,differences);
					}
					jpl::mipl::p2::difpic::VicarImageUtil::formatDMsg(lhkvit->first,lhval,emptyString,differences);
					sameLabels=false;
				}
				else
				{ // label in right hand image
					string rhval = jpl::mipl::p2::difpic::VicarImageUtil::getValue(*rhkvit->second);
					if(lhval.compare(rhval)!=0)
					{
						if (!doneProp)
						{
							doneProp = true;
							jpl::mipl::p2::difpic::VicarImageUtil::formatHdrMsg(lhit->first,differences);
						}
						jpl::mipl::p2::difpic::VicarImageUtil::formatDMsg(lhkvit->first,lhval,rhval,differences);
						sameLabels=false;
					}
					rhkv->erase(rhkvit->first);
				} // end of label in right hand image
				lhkv->erase(lhkvit->first);
				lhkvit=lhkv->begin();
			} // end of while over all labels in property
			//if (rhkv->size()>0)
			//cerr<<"Before size="<<rhkv->size()<<endl;
			if (!doneProp && rhkv->size()>0)
			{
				jpl::mipl::p2::difpic::VicarImageUtil::formatHdrMsg(lhit->first,differences);
				if (!jpl::mipl::p2::difpic::VicarImageUtil::processExtraRHLabels(*rhkv,labelsToIgnore,differences))
					sameLabels=false;
			}
			//cerr<<"After size="<<rhkv->size()<<" differences="<<differences<<endl;
		} // end of if
		lh.erase(lhit->first);
		rh.erase(lhit->first);
		lhit=lh.begin();
	}// end of loop over all property labels
	if (rh.size()>0)
		if (!jpl::mipl::p2::difpic::VicarImageUtil::processExtraRHProperties(rh,propertiesToIgnore,labelsToIgnore,differences))
			sameLabels=false;
	return sameLabels;
}


//====================================================================================
//====================================================================================
//====================================================================================
bool jpl::mipl::p2::VicarImageLabel::
getKeyword(const string & property, const string & keyword, vector<string> & value)
{
	map < string, map < string, vector < string >* >* >::iterator pit = this->labels_.find(property);

	if (pit == this->labels_.end())
		return false;

	map < string, vector < string >* >* val = pit->second;
	map < string, vector < string >* >::iterator vit = val->find(keyword);
	if (vit == val->end()) return false;

	vector < string >* v = vit->second;

	for (size_t i = 0; i < v->size(); i++)
		value.push_back(v->at(i));

	return true;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create VicarHistoryLabel.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
       Copyright 2011-Present
       California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledged. 09-08-2011.

       Auther : Rajesh R. Patel

History :
---------

Date              Who                        What
-------------------------------------------------------------------------------
09/08/2011        Raj       Initial Release

 */

#include "VicarHistoryLabel.h"
#include "VicarImageUtil.h"
#include <cstring>
#include <cstdio>
#include <iostream>
//defines constants for VICAR label key and value size, max properties, etc.
// defines.h has some horrible macros such as "U", and other single
// letters. "U" happens to be a template class in C++ standard library,
// so putting this include first may cause the compilation to break.
extern "C"
{
#include "defines.h"
}

using namespace jpl::mipl::p2;
using namespace std;

//====================================================================================
//====================================================================================
//====================================================================================
jpl::mipl::p2::VicarHistoryLabel::VicarHistoryLabel(int unitNo)
throw (VicarException)
{
	this->unitNo_=unitNo;
	try
	{
		this->init_();
	}
	catch (const VicarException & e)
	{
		this->cleanUp();
		throw e;
	}
}
//====================================================================================
//====================================================================================
//====================================================================================
jpl::mipl::p2::VicarHistoryLabel::~VicarHistoryLabel()
{
	this->cleanUp();
}

//====================================================================================
//====================================================================================
//====================================================================================
void jpl::mipl::p2::VicarHistoryLabel::cleanUp()
{
	map < string, map < string, vector < string >* >* >::iterator pit = this->labels_.begin();

	//for (pit=this->labels_.begin(); pit != this->labels_.end(); ++pit)
	while (pit!=this->labels_.end())
	{
		//cerr<<"Will clean up "<<pit->first<<endl;
		map < string, vector < string >* >* v = pit->second;

		map < string, vector < string >* >::iterator kpit = v->begin();
		//for (; kpit!=v->end(); ++kpit)
		while (kpit!=v->end())
		{
			//cerr<<"\tDeleting "<<kpit->first<<endl;
			kpit->second->clear();
			delete kpit->second;
			//kpit->second=NULL;
			v->erase(kpit);
			kpit=v->begin();
		}
		//v->clear();
		delete v;
		this->labels_.erase(pit);
		pit=this->labels_.begin();
	}
}

//====================================================================================
//====================================================================================
//====================================================================================
void jpl::mipl::p2::VicarHistoryLabel::listLabels(string &inThisString) const
{
	map < string, map < string, vector < string >* >* >::const_iterator pit;

	for (pit=this->labels_.begin(); pit != this->labels_.end(); ++pit)
	{
		inThisString.append("---------- ").append(pit->first).append(" ----------\n");
		map < string, vector < string >* >* v = pit->second;

		map < string, vector < string >* >::const_iterator kpit = v->begin();
		for (; kpit!=v->end(); ++kpit)
			inThisString.append("\t").append(kpit->first).append("=").append(jpl::mipl::p2::difpic::VicarImageUtil::getValue(*kpit->second));
	}
}

//====================================================================================
//====================================================================================
//====================================================================================
void jpl::mipl::p2::VicarHistoryLabel::init_()
throw (VicarException)
{
	char tasks[MAX_TASKS][MAX_LABEL_KEY_SIZE+1];
	int  inst_num[MAX_TASKS];

	int noTasks=-1;
	int max_sets=MAX_TASKS;

	int status = zlhinfo(this->unitNo_,
			(char*)tasks,
			inst_num,
			&max_sets,
			"NRET",&noTasks,
			"ULEN",MAX_LABEL_KEY_SIZE+1,
			NULL);
	if (status != SUCCESS)
		throw VicarException("Error obtaining number of property items from history 1");

	if (noTasks>MAX_TASKS)
	{
		char msg[100];
		sprintf(msg,"History has %d property sets; max allowed is %d",noTasks, MAX_TASKS);
		string emsg("Property labels in first history exceeds max allowed property sets. ");
		emsg.append(msg).append(".");
		throw VicarException(emsg.c_str());
	}

	cerr<<"Total number of history items="<<noTasks<<endl;

	char format[8+1];
	int maxLen, noElements, dummy;

	for (int tno=0; tno<noTasks; ++tno)
	{
		cerr<<"Working on history label="<<tasks[tno]<<endl;
		status = zlinfo(this->unitNo_,const_cast<char*>("HISTORY"),const_cast<char*>("TASK"),format,&dummy,
				&noElements,"HIST",tasks[tno],
				"INSTANCE",inst_num[tno],
				"STRLEN", &maxLen, NULL);
		cerr<<"ITEM="<<tasks[tno]<<" instance="<<inst_num[tno]<<" noElem="<<noElements<<endl;
		map < string, map < string, vector < string >* >* >::iterator pit = this->labels_.find(tasks[tno]);
		map < string, vector < string >* >* val;
		if (pit == this->labels_.end())
		{
			val = new map < string, vector < string >* >();
			this->labels_.insert(pair< string, map < string, vector < string >* >* >(tasks[tno], val) );
		}
		else
			val = pit->second;

		char key[MAX_LABEL_KEY_SIZE+1];
		char value[MAX_LABEL_VALUE_SIZE+1];

		while (TRUE)
		{
			status = zlninfo(this->unitNo_,key,format,&dummy,&noElements,
					"STRLEN",&maxLen,NULL);
			if (status == END_OF_LABEL)
			{
				cerr<<"Will break."<<endl;
				break;
			}
			cerr<<"\tkey="<<key<<" maxlen="<<maxLen<<" noElements="<<noElements<<endl;
			if (status != SUCCESS)
				throw VicarException("Error getting information about next key");
			vector < string >* values;
			map < string, vector < string >* >::iterator vit = val->find(key);
			if (vit == val->end())
			{
				values = new vector < string > ();
				val->insert(pair< string, vector < string >* >(key,values));
			}
			else
				values = vit->second;

			//cerr<<"\tWorking on key="<<key<<endl;
			//char *value = new char[maxLen+1];
			for (int ino=1; ino<=noElements; ++ino)
			{
				status = zlget(this->unitNo_,const_cast<char*>("PROPERTY"),key,value,
						"PROPERTY",tasks[tno],
						"INSTANCE",inst_num[tno],
						"FORMAT","STRING",
						"ULEN", MAX_LABEL_VALUE_SIZE,
						"ELEMENT",
						ino, NULL);
				values->push_back(value);
			}
			//delete [] value;
		}
	}
	//cerr<<"End of labels."<<endl;
}


//====================================================================================
//====================================================================================
//====================================================================================
bool jpl::mipl::p2::VicarHistoryLabel::
compare(const VicarHistoryLabel &toThisLabels,
		const vector <string> &propertiesToIgnore,
		const vector <string> &labelsToIgnore,
		string& differences) const
{
	map < string, map < string, vector < string >* >* > MLabels;
	map < string, map < string, vector < string >* >* > OLabels;

	jpl::mipl::p2::difpic::VicarImageUtil::clone(this->labels_,MLabels);
	jpl::mipl::p2::difpic::VicarImageUtil::clone(toThisLabels.labels_,OLabels);

	bool try1 = this->compare(MLabels,OLabels,propertiesToIgnore,labelsToIgnore,differences);

	//cerr<<"try1="<<try1<<endl;
	map < string, map < string, vector < string >* >* > MLabels2;
	map < string, map < string, vector < string >* >* > OLabels2;

	jpl::mipl::p2::difpic::VicarImageUtil::clone(OLabels,MLabels2);
	jpl::mipl::p2::difpic::VicarImageUtil::clone(MLabels,OLabels2);

	bool try2 = this->compare(OLabels2,MLabels2,propertiesToIgnore,labelsToIgnore,differences);
	//cerr<<"try2="<<try2<<endl;

	return try1 && try2;
}


//====================================================================================
//====================================================================================
//====================================================================================
bool
jpl::mipl::p2::VicarHistoryLabel::
compare(map < string, map < string, vector < string >* >* > &lh,
		map < string, map < string, vector < string >* >* > &rh,
		const vector <string> &propertiesToIgnore,
		const vector <string> &labelsToIgnore,
		string& differences) const
{
	//map <string, vector < string > > diffs;
	bool sameLabels = true;
	string emptyString("<LABEL MISSING>");
	map < string, map < string, vector < string >* >* >::const_iterator lhit = lh.begin();
	while (lhit!=lh.end())
	{//loop over all properties
		if (jpl::mipl::p2::difpic::VicarImageUtil::contains(propertiesToIgnore,lhit->first))
		{
			lh.erase(lhit->first);
			if (rh.count(lhit->first)!=0)
				rh.erase(lhit->first);
			lhit=lh.begin();
			continue;
		}
		map < string, vector < string >* >* lhkv = lhit->second;
		map < string, vector < string >* >::const_iterator lhkvit = lhkv->begin();
		map < string, map < string, vector < string >* >* >::const_iterator rhit = rh.find(lhit->first);
		if (rhit==rh.end())
		{// property missing in right hand history
			jpl::mipl::p2::difpic::VicarImageUtil::formatHdrMsg(lhit->first,differences);
			for (; lhkvit!=lhkv->end(); ++lhkvit)
			{
				string lhval = jpl::mipl::p2::difpic::VicarImageUtil::getValue(*lhkvit->second);
				jpl::mipl::p2::difpic::VicarImageUtil::formatDMsg(lhkvit->first,lhval,emptyString,differences);
			}
			sameLabels=false;

			//differences.append(
			//cerr<<"Missing property "<<lhit->first<<" from right hand history"<<endl;
		}
		else
		{// property found on right hand history
			bool doneProp = false;

			map < string, vector < string >* >* rhkv =rhit->second;

			while (lhkvit!=lhkv->end())
			{// while loop over all labels of a property
				map < string, vector < string >* >::const_iterator rhkvit = rhkv->find(lhkvit->first);
				if (jpl::mipl::p2::difpic::VicarImageUtil::contains(labelsToIgnore,lhkvit->first))
				{ // key in set of keys to ignore
					lhkv->erase(lhkvit->first);
					if (rhkvit!=rhkv->end())
						rhkv->erase(rhkvit->first);
					lhkvit=lhkv->begin();
					continue;
				}
				string lhval = jpl::mipl::p2::difpic::VicarImageUtil::getValue(*lhkvit->second);
				if (rhkvit==rhkv->end())
				{ // label not in right hand history
					if (!doneProp)
					{
						doneProp = true;
						jpl::mipl::p2::difpic::VicarImageUtil::formatHdrMsg(lhit->first,differences);
					}
					jpl::mipl::p2::difpic::VicarImageUtil::formatDMsg(lhkvit->first,lhval,emptyString,differences);
					sameLabels=false;
				}
				else
				{ // label in right hand history
					string rhval = jpl::mipl::p2::difpic::VicarImageUtil::getValue(*rhkvit->second);
					if(lhval.compare(rhval)!=0)
					{
						if (!doneProp)
						{
							doneProp = true;
							jpl::mipl::p2::difpic::VicarImageUtil::formatHdrMsg(lhit->first,differences);
						}
						jpl::mipl::p2::difpic::VicarImageUtil::formatDMsg(lhkvit->first,lhval,rhval,differences);
						sameLabels=false;
					}
					rhkv->erase(rhkvit->first);
				} // end of label in right hand history
				lhkv->erase(lhkvit->first);
				lhkvit=lhkv->begin();
			} // end of while over all labels in property
			//if (rhkv->size()>0)
			//cerr<<"Before size="<<rhkv->size()<<endl;
			if (!doneProp && rhkv->size()>0)
			{
				jpl::mipl::p2::difpic::VicarImageUtil::formatHdrMsg(lhit->first,differences);
				if (!jpl::mipl::p2::difpic::VicarImageUtil::processExtraRHLabels(*rhkv,labelsToIgnore,differences))
					sameLabels=false;
			}
			//cerr<<"After size="<<rhkv->size()<<" differences="<<differences<<endl;
		} // end of if
		lh.erase(lhit->first);
		rh.erase(lhit->first);
		lhit=lh.begin();
	}// end of loop over all property labels
	if (rh.size()>0)
		if (!jpl::mipl::p2::difpic::VicarImageUtil::processExtraRHProperties(rh,propertiesToIgnore,labelsToIgnore,differences))
			sameLabels=false;
	return sameLabels;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DifpicParameters.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
       Copyright 2011-Present
       California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledged. 09-08-2011.

       Auther : Rajesh R. Patel

History :
---------

Date              Who                        What
-------------------------------------------------------------------------------
09/13/2011        Raj       Initial Release

 */

#include "DifpicParameters.h"
#include "VicarException.h"
#include <cstdio>
#include <iostream>

// defines.h has some really horrible macros such as U
// which incidentally is a template item in C++ standard
// library. Thus the code won't compile if these C 
// includes are put before the C++ includes.
extern "C"
{
#include "defines.h"
#include "zvproto.h"
}

using namespace std;
/*
string jpl::mipl::p2::VicarImageLabel::INP_VAR_NAME="INP";
string jpl::mipl::p2::VicarImageLabel::LBLCMP_VAR_NAME="DIFF_LABELS";
string jpl::mipl::p2::VicarImageLabel::IGNORE_LABELS_VAR_NAME="IGNORE_LABELS";
string jpl::mipl::p2::VicarImageLabel::IGNORE_PROPERTIES_VAR_NAME="IGNORE_PROPERTIES";
 */
jpl::mipl::p2::difpic::DifpicParameters* jpl::mipl::p2::difpic::DifpicParameters::instance_=(jpl::mipl::p2::difpic::DifpicParameters*)NULL;
//====================================================================================
//====================================================================================
//====================================================================================
/**/
jpl::mipl::p2::difpic::DifpicParameters* 
jpl::mipl::p2::difpic::DifpicParameters::instance() throw(exception)
{
	if (jpl::mipl::p2::difpic::DifpicParameters::instance_==NULL)
	{
		jpl::mipl::p2::difpic::DifpicParameters::instance_=new DifpicParameters();
	}
	return jpl::mipl::p2::difpic::DifpicParameters::instance_;
}

//====================================================================================
//====================================================================================
//====================================================================================
jpl::mipl::p2::difpic::DifpicParameters::
DifpicParameters()
{
	this->getParameters();
}

//====================================================================================
//====================================================================================
//====================================================================================
void
jpl::mipl::p2::difpic::DifpicParameters::
getMultiStringValueParam(const std::string& name,
		std::vector<std::string>& values,
		int maxLength,
		int minCount,
		int maxCount,
		bool fillTillMax)
{
	int count=0;
	int status = zvpcnt((char*)name.c_str(),&count);
	if (status != SUCCESS)
	{
		string emsg="Error obtaining count for ";
		emsg.append(name).append(" keyword.");
		zvmessage((char*)emsg.c_str(),NULL);
	}
	else
	{
		char *val = new char[maxLength];
		if (count<minCount || count>maxCount)
		{
			sprintf(val,"%s count=%d outside the range of [%d,%d]",name.c_str(),count,minCount,maxCount);
			zvmessage(val,const_cast<char*>("ERROR"));
		}
		else
		{
			int i=1;
			for (; i<=count; ++i)
			{
				status = zvpone((char*)name.c_str(),val,i,maxLength);
				if (status != SUCCESS)
				{
					sprintf(val,"Error obtaining value number %d for %s keyword. Return status=%d",i,name.c_str(),status);
					zvmessage(val,const_cast<char*>("ERROR"));
					values.clear();
					break;
				}
				else
				{
					values.push_back(string(val));
				}
			}
			if (fillTillMax)
				for (; i<=maxCount; ++i)
					values.push_back(string(val));
		}

		delete [] val;
	}
}
//====================================================================================
//====================================================================================
//====================================================================================
void jpl::mipl::p2::difpic::DifpicParameters::getParameters()
{

	int  status;
	const int max_val_size = 512;
	char val[max_val_size];
	char expanded[max_val_size];

	status = zvpone(const_cast<char*>("BHFMTMAP"), val, 1, max_val_size);
	if (status == SUCCESS)
	{
		status = zvfilename(val, expanded, max_val_size);
		if (status == SUCCESS)
			this->bhFmtMappingFile_ = string(expanded);
	}
	// if status fail, we ought to throw exception or something

	status = zvpone(const_cast<char*>("LPFMTMAP"), val, 1, max_val_size);
	if (status == SUCCESS)
	{
		status = zvfilename(val, expanded, max_val_size);
		if (status == SUCCESS)
			this->lpFmtMappingFile_ = string(expanded);
	}
	// if status fail, we ought to throw exception or something

        this->getMultiStringValueParam("BLTYPES", this->bltypes_, 32, 0, 2);

	if (zvptst(const_cast<char*>("ALL")))
		this->histDiffEnabled_=this->binDiffEnabled_=
				this->linePreDiffEnabled_=this->labelDiffEnabled_=pixDiffEnabled_=true;
	else
	{
		this->histDiffEnabled_=zvptst(const_cast<char*>("HSTCMP"));
		this->binDiffEnabled_=zvptst(const_cast<char*>("BINCMP"));
		this->linePreDiffEnabled_=zvptst(const_cast<char*>("LPRCMP"));
		this->pixDiffEnabled_=zvptst(const_cast<char*>("PIXCMP"));
		this->labelDiffEnabled_ = zvptst(const_cast<char*>("LBLCMP"));
	}

	this->silent_=zvptst(const_cast<char*>("SILENT"));

	this->getMultiStringValueParam("INP", this->inputFileNames_, 1024, 2,2, true);

	if (this->binaryHeaderDiffEnabled())
	{
		this->getMultiStringValueParam("BHFMTFILES",this->bhFMTFileNames_,1024,0,2);
		this->getMultiStringValueParam("BHK_IGNORE", this->bhFieldsToIgnore_, 128, 0, 600);
	}

	if (this->linePrefixDiffEnabled())
	{
		this->getMultiStringValueParam("LPFMTFILES",this->lpFMTFileNames_,1024,0,2);
		this->getMultiStringValueParam("LPK_IGNORE", this->lpFieldsToIgnore_, 128, 0, 600);
	}

	if (this->labelDiffEnabled())
	{
		this->getMultiStringValueParam("LBL_IGNORE",this->labelsToIgnore_,32,0,600);
		this->getMultiStringValueParam("PROP_IGNORE",this->propertiesToIgnore_,32,0,600);
	}
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create difpic_bridges.cc
$ DECK/DOLLARS="$ VOKAGLEVE"
#include <iostream>
#include <cstring>
#include <string>
#include "VicarImageLabel.h"
#include "VicarHistoryLabel.h"
#include "difpic_bridges.h"
#include "DifpicParameters.h"
#include "VicarFmtMap.h"
#include "VicarBinaryHeader.h"
#include "balm.h"

extern "C"
{
#include "xvmaininc.h"
#include "applic.h"
#include "ftnbridge.h"
#include "defines.h"
}

using namespace std;
using namespace jpl::mipl::p2::difpic;
using namespace jpl::mipl::p2;

bool found (const vector<string>& source, const string target);
void print (const string& msg);

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
void print (const string& msg)
{
	const size_t max = 250;
	char buffer[max];
	size_t len = msg.size();
	size_t i = 0, j = 0;
	char c;

	do
	{
		c = msg.at(i);
		i++;
		buffer[j] = c;
		j++;
		if ( j == (max - 1) || c == '\n' || i == len)
		{
			buffer[j] = '\0';
			zvmessage(buffer, NULL);
			j = 0;
		}
	} while (i < len);
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
extern "C" bool labeldifC()
{
	bool same=true;
	int iunit1=-1, iunit2=-2, status=0;
	try
	{
		const DifpicParameters * params = DifpicParameters::instance();
		string differences;
		if (params->labelDiffEnabled())
		{
			status = zvunit(&iunit1, const_cast< char *>("INP"), 1,NULL);
			status = zvopen(iunit1,"OP", "READ", "OPEN_ACT", "SA","IO_ACT", "SA", NULL);
			status = zvunit(&iunit2, const_cast<char*>("INP"), 2,NULL);
			status = zvopen(iunit2,"OP", "READ", "OPEN_ACT", "SA","IO_ACT", "SA", NULL);
			VicarImageLabel ld1(iunit1);
			VicarImageLabel ld2(iunit2);

			same=ld1.compare(ld2,
					params->propertiesToIgnore(),
					params->labelsToIgnore(),
					differences);
			if (!params->silent())
			{
				if (!same)
				{
					zvmessage("Found following label differences between given images:\n", NULL);
					print(differences);
					zvmessage("\n------------ Vicar label differs. ------------\n", NULL);
				}
				else
					zvmessage("------- Vicar label comparison passed. -------\n", NULL);
				zvmessage("\n", NULL);
			}
		}
	}
	catch (const exception& e)
	{
		zvmessage(const_cast<char*>(e.what()),const_cast<char*>("Exception::labeldifC"));
	}
	if (iunit1!=-1)
		status = zvclose(iunit1,NULL);
	if (iunit2!=-2)
		status = zvclose(iunit2,NULL);

	return same;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

extern "C" void FTN_NAME2(labeldif, LABELDIF) (int *status)
{
	*status = labeldifC() ? 0 : 1;
}


/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

extern "C" bool histdifC()
{
	bool same=true;
	int iunit1=-1, iunit2=-2, status=0;
	try
	{
		const DifpicParameters * params = DifpicParameters::instance();
		string differences;

		if (params->historyDiffEnabled())
		{
			status = zvunit(&iunit1, const_cast<char*>("INP"), 1,NULL);
			status = zvopen(iunit1,"OP", "READ", "OPEN_ACT", "SA","IO_ACT", "SA", NULL);
			status = zvunit(&iunit2, const_cast<char*>("INP"), 2,NULL);
			status = zvopen(iunit2,"OP", "READ", "OPEN_ACT", "SA","IO_ACT", "SA", NULL);
			/*
	VicarHistoryLabel ld1(iunit1);
	VicarHistoryLabel ld2(iunit2);

	same=ld1.compare(ld2,
	params->propertiesToIgnore(),
	params->labelsToIgnore(),
	differences);
			 */
			same=false;
			if (!params->silent())
				zvmessage("***** History label comparision not implemented yet. *****\n", "ERROR");
		}
	}
	catch (const exception &e)
	{
		zvmessage(const_cast<char*>(e.what()),const_cast<char*>("Exception::histdifC"));
	}
	if (iunit1!=-1)
		status = zvclose(iunit1,NULL);
	if (iunit2!=-2)
		status = zvclose(iunit2,NULL);
	return same;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

extern "C" void FTN_NAME2(histdif, HISTDIF) (int *status)
{
	*status = histdifC() ? 0 : 1;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
bool getBHFMTs (string & fmt1, string & fmt2, int iunit1, int iunit2, string & errmsg)
{
	const DifpicParameters * params = DifpicParameters::instance();
	const vector<string>& fmtnames = params->getBinaryHeaderFMTFileNames();

	// Check if BHFMTFILES are specified
	if (fmtnames.size() != 2 && fmtnames.size() != 0)
	{
		errmsg.append("Need exactly two file names for BHFMTFILES (Binary Header FMT File Names)");
		return false;
	}

	// ------- Begin: BHFMTFILES are not specified, use FMT mapping -----
	if (fmtnames.size() == 0)
	{
		// Check if BHFMTMAP exists,
		if (params->getBinaryHeaderFMTMappingFile().empty())
		{
			// BHFMTMAP does not exist, report error and quit
			errmsg.append("Need BHFMTMAP");
			return false;
		}

		// BHFMTMAP exist, read and parse map
		VicarFmtMap fmtMap(params->getBinaryHeaderFMTMappingFile());
		string bltype1, bltype2;

		// check if BLTYPES are specified
		if (params->getBlTypes().size() != 0 && params->getBlTypes().size() != 2)
		{
			errmsg.append("Need exactly two strings for BLTYPES.");
			return false;
		}

		// ----- Begin: BLTYPES are not specified, read Vicar system label -----
		if (params->getBlTypes().size() != 2)
		{
			char value[MAX_LABEL_VALUE_SIZE+1];
			zlget(iunit1, const_cast<char*>("SYSTEM"), const_cast<char*>("BLTYPE"), value, "ERR_ACT", "SA", NULL);
			bltype1 = string(value);
			zlget(iunit2, const_cast<char*>("SYSTEM"), const_cast<char*>("BLTYPE"), value, "ERR_ACT", "SA", NULL);
			bltype2 = string(value);
		}
		// ----- End: BLTYPES are not specified, read Vicar system label -------
		else
		{
			bltype1 = params->getBlTypes().at(0);
			bltype2 = params->getBlTypes().at(1);
		}

		// Using BLTYPES to read FMT file names from FMT map
		if (!fmtMap.getFmt(bltype1, fmt1))
		{
			errmsg.append("Cannot find FMT for ");
			errmsg.append(bltype1);
			return false;
		}
		if (!fmtMap.getFmt(bltype2, fmt2))
		{
			errmsg.append("Cannot find FMT for ");
			errmsg.append(bltype2);
			return false;
		}
	}
	// ------- End: BHFMTFILES are not specified, use FMT mapping -------
	// BHFMTFILES are specified, then obtain the FMT's file names from parameters
	else
	{
		fmt1 = fmtnames.at(0);
		fmt2 = fmtnames.at(1);
	}

	return true;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
bool getUnionOfFields (BalmFormat format1, BalmFormat format2,
		const string& input1, const string& input2,
		const vector< string >& ignored, vector< BalmField >& allFields,
		string& differences, string& errmsg, bool* same)
{
	vector<BalmField> allFields1;
	vector<BalmField> allFields2;

	if (!getAllFields(format1, allFields1, errmsg))
		return false;

	if (!getAllFields(format2, allFields2, errmsg))
		return false;

	size_t numField = allFields1.size();

	// make a intersection of fields to check
	for (size_t i = 0; i < numField; i++)
	{
		const BalmField* field1 = &allFields1.at(i);
		const BalmField* field2 = getField(allFields2, field1->getName(), field1->getStart());

		if (!found(ignored, field1->getName()))
		{
			if (field2 == NULL)
			{
				differences += "Field ";
				differences += field1->getName();
				differences += " doesn't exist in file ";
				differences += input2;
				differences += ".\n";
				*same = false;
			}
			else if (field2 != NULL)
			{
				allFields.push_back(*field1);
				allFields.push_back(*field2);
			}
		}
		// if field is ignored, then we don't if it's missing
	}

	numField = allFields2.size();
	for (size_t i = 0; i < numField; i++)
	{
		const BalmField* field2 = &allFields2.at(i);
		const BalmField* field1 = getField(allFields1, field2->getName(), field2->getStart());
		if (field1 == NULL && !found(ignored, field2->getName()))	// if field is ignored, then we don't if it's missing
		{
			differences += "Field ";
			differences += field2->getName();
			differences += " doesn't exist in file ";
			differences += input1;
			differences += ".\n";
			*same = false;
		}
	}

	return true;
}


/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
// helper functions
bool found (const vector<string>& source, const string target)
{
	// this silly function is here because I can't figured out
	// out to use the algorithm library in 5 minutes.
	size_t len = source.size();

	for (size_t i = 0; i < len; i++)
	{
		if (iequals(source.at(i), target))
		{
			return true;
		}
	}

	return false;
}
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
void printGetFieldError (int result, const string& field)
{
	string error("Error while comparing field ");
	error += field;
	error += ": ";

	switch (result)
	{
	case BalmField::fld_wrong_type:
		error += "Wrong type.";
		break;
	case BalmField::fld_output_size_too_small:
		error += "Output size too small.";
		break;
	case BalmField::fld_input_size_too_small:
		error += "Input buffer too small.";
		break;
	case BalmField::fld_bitstr_too_long:
		error += "Bit string too long.";
		break;
		// fld_incorrect_output_size won't be return by getValue* functions
	default:
		error += "Unknown.";
	}

	zvmessage(const_cast<char*>(error.c_str()), NULL);
}
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
bool bitStrEquals (BalmField& field1, const unsigned char* buf1, size_t size1,
		BalmField& field2, const unsigned char* buf2, size_t size2)
{
	// this assumes fields are bit string
	unsigned long bs1, bs2;
	int result;

	result = field1.getBitString(buf1, size1, &bs1);
	if (result != BalmField::fld_success)
	{
		printGetFieldError(result, field1.getName());
		return false;
	}
	result = field2.getBitString(buf2, size2, &bs2);
	if (result != BalmField::fld_success)
	{
		printGetFieldError(result, field2.getName());
		return false;
	}

	return bs1 == bs2;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
bool unsignedIntEquals (BalmField& field1, const unsigned char* buf1, size_t size1,
		BalmField& field2, const unsigned char* buf2, size_t size2)
{
	unsigned long l1, l2;
	int result;

	result = field1.getIntegerValue(buf1, size1, &l1);
	if (result != BalmField::fld_success)
	{
		printGetFieldError(result, field1.getName());
		return false;
	}
	result = field2.getIntegerValue(buf2, size2, &l2);
	if (result != BalmField::fld_success)
	{
		printGetFieldError(result, field1.getName());
		return false;
	}
	return l1 == l2;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
bool integerEquals (BalmField& field1, const unsigned char* buf1, size_t size1,
		BalmField& field2, const unsigned char* buf2, size_t size2)
{
	long l1, l2;
	int result;
	result = field1.getIntegerValue(buf1, size1, &l1);
	if (result != BalmField::fld_success)
	{
		printGetFieldError(result, field1.getName());
		return false;
	}
	result = field2.getIntegerValue(buf2, size2, &l2);
	if (result != BalmField::fld_success)
	{
		printGetFieldError(result, field1.getName());
		return false;
	}
	return l1 == l2;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
bool realEquals (BalmField& field1, const unsigned char* buf1, size_t size1,
		BalmField& field2, const unsigned char* buf2, size_t size2)
{
	double d1, d2;
	int result;

	// this assumes fields are floating point number
	result = field1.getRealValue(buf1, size1, &d1);
	if (result != BalmField::fld_success)
	{
		printGetFieldError(result, field1.getName());
		return false;
	}

	result = field2.getRealValue(buf2, size2, &d2);
	if (result != BalmField::fld_success)
	{
		printGetFieldError(result, field2.getName());
		return false;
	}

	return d1 == d2;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
bool boolEquals (BalmField & field1, const unsigned char* buf1, size_t size1,
		BalmField & field2, const unsigned char* buf2, size_t size2)
{
	// this assumes fields are boolean
	bool b1, b2;
	int result;
	result = field1.getBoolean(buf1, size1, &b1);
	if (result != BalmField::fld_success)
	{
		printGetFieldError(result, field1.getName());
		return false;
	}
	result = field2.getBoolean(buf2, size2, &b2);
	if (result != BalmField::fld_success)
	{
		printGetFieldError(result, field2.getName());
		return false;
	}
	return b1 == b2;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
bool byteEquals (BalmField& field1, const unsigned char* buf1, size_t size1,
		BalmField& field2, const unsigned char* buf2, size_t size2)
{
	const size_t len = field1.getLength();
	// this assumes fields are byte arrays
	if (len != field2.getLength())
		return false;

	unsigned char* a1 = new unsigned char [len];
	unsigned char* a2 = new unsigned char [len];
	int result;

	bool success = (result = field1.getByteStream(buf1, size1, a1, len)) == BalmField::fld_success;
	if (!success)
	{
		printGetFieldError(result, field1.getName());
	}
	if (success)
	{
		success = (result = field2.getByteStream(buf2, size2, a2, len)) == BalmField::fld_success;
		if (!success)
		{
			printGetFieldError(result, field2.getName());
		}
	}
	success = success && (memcmp(a1,a2,len) == 0);

	delete [] a1;
	delete [] a2;

	return success;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
bool fieldEquals (BalmField& field1, const unsigned char* buf1, size_t size1,
		BalmField& field2, const unsigned char* buf2, size_t size2)
{
	if (field1.isBitField() != field2.isBitField())
	{
		zvmessage((char*)"Type mismatch", (char*)"");
		return false;
	}

	if (field1.getType() != field2.getType())
	{
		zvmessage((char*)"Type mismatch", (char*)"");
		return false;
	}

	if (field1.isBitField())
		return bitStrEquals(field1, buf1, size1, field2, buf2, size2);
	else
	{
		switch (field1.getType())
		{
		case BalmField::BOOLEAN: return boolEquals(field1, buf1, size1, field2, buf2, size2);
		case BalmField::BYTE: return byteEquals(field1, buf1, size1, field2, buf2, size2);
		case BalmField::FLOAT: return realEquals(field1, buf1, size1, field2, buf2, size2);
		case BalmField::SIGNED_INTEGER: return integerEquals(field1, buf1, size1, field2, buf2, size2);
		case BalmField::UNSIGNED_INTEGER: return unsignedIntEquals(field1, buf1, size1, field2, buf2, size2);
		default:
			zvmessage((char*)"Fields are UNSET.", (char*)"");
		}
	}
	return false;	// so that the caller will print error messages with field's name.

}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
extern "C" bool binaryheaderdifC()
{
	bool same = true;
	try
	{
		const DifpicParameters * params = DifpicParameters::instance();
		string differences;
		if (params->binaryHeaderDiffEnabled())
		{
			bool got_error = false;
			string errmsg;
			int iunit1, iunit2, status;
			string fmt1, fmt2;
			BalmFormat format1 = NULL;
			BalmFormat format2 = NULL;
			vector<BalmField> allFields;
			VicarBinaryHeader bh1, bh2;

			status = zvunit(&iunit1,(char*) "INP", 1,NULL);
			zvsignal(iunit1, status, 1);
			zvopen(iunit1,"OP", "READ", "OPEN_ACT", "SA","IO_ACT", "SA", "COND", "BINARY", NULL);
			status = zvunit(&iunit2, (char*)"INP", 2, NULL);
			zvsignal(iunit2, status, 1);
			zvopen(iunit2, "OP", "READ", "OPEN_ACT", "SA", "IO_ACT", "SA", "COND", "BINARY", NULL);

			got_error = !(getBHFMTs(fmt1, fmt2, iunit1, iunit2, errmsg));

			// -------------------- Begin: Parse format files ----------------------
			if (!got_error)
			{
				format1 = parseFormat(fmt1.c_str());
				if (format1 == NULL)
				{
					errmsg = string ("Unknown error in format file ");
					errmsg += fmt1;
					got_error = true;
				}
			}

			if (!got_error)
			{
				format2 = parseFormat(fmt2.c_str());
				if (format2 == NULL)
				{
					errmsg = string ("Unknown error in format file ");
					errmsg += fmt2;
					got_error = true;
				}
			}
			// -------------------- End: Parse format files ------------------------

			// Read ignore fields
			const vector<string>& ignored = params->bhFieldsToIgnore();

			// Set up fields' union list
			if (!got_error)
			{
				string input1 = params->getInputFileNames().at(0);
				string input2 = params->getInputFileNames().at(1);
				got_error = !getUnionOfFields (format1, format2, input1, input2,
						ignored, allFields, differences, errmsg, &same);
			}

			// Get binary header
			if (!got_error)
			{
				try
				{
					// Read binary headers
					bh1 = VicarBinaryHeader(iunit1);
					bh2 = VicarBinaryHeader(iunit2);
				}
				catch (const exception& e1)
				{
					errmsg.append(e1.what());
					got_error = true;
				}

			}

			// --------------- Begin: Traverse union list and diff ----------------
			if (!got_error)
			{
				size_t numField = allFields.size();
				BalmField* field1;
				BalmField* field2;
				const unsigned char* buf1 = bh1.getHeader();
				const unsigned char* buf2 = bh2.getHeader();
				const size_t bin_hdr_size1 = bh1.getSize();
				const size_t bin_hdr_size2 = bh2.getSize();

				for (size_t i = 0; i < numField; i += 2)
				{
					field1 = &allFields.at(i);
					field2 = &allFields.at(i+1);
					if (!fieldEquals(*field1, buf1, bin_hdr_size1, *field2, buf2, bin_hdr_size2))
					{
						// for niceties, the values from respective files ought to be printed.
						// however, because FMT file is still messed up at this time,
						// it's better not to print them yet because the value maybe wrong.
						differences += "Field ";
						differences += field1->getName();
						differences += " differs.\n";
						same = false;

						// don't break here because I want to see all fields that are different
					}
				}
			}
			// --------------- End: Traverse union list and diff ------------------

			if (format1 != NULL) freeFormat(format1);
			if (format2 != NULL) freeFormat(format2);
			status = zvclose(iunit1,NULL);
			status = zvclose(iunit2,NULL);
			if (got_error)
			{
				zvmessage((char*)errmsg.c_str(), (char*)"ERROR");
				return false;
			}

			if (!params->silent())
			{
				if (!same)
				{
					zvmessage("Found following binary header differences between given images:\n", NULL);
					print(differences);
					zvmessage("\n--------------- Binary header differs. ----------------\n", NULL);
				}
				else
					zvmessage("----------- Binary header comparison passed. ----------\n", NULL);
				zvmessage("\n", NULL);
			}
		}
	}
	catch (const exception& e)
	{
		zvmessage(const_cast<char*>(e.what()), (char*)"Exception::binaryheaderdifC");
	}

	return same;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

extern "C" void FTN_NAME2(binaryheaderdif, BINARYHEADERDIF) (int *unit1, int *unit2, int *status)
{
	*status = binaryheaderdifC() ? 0 : 1;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
bool getLPFMTs (string & fmt1, string & fmt2, int iunit1, int iunit2, string & errmsg)
{
	const DifpicParameters * params = DifpicParameters::instance();
	const vector<string>& fmtnames = params->getLinePrefixFMTFileNames();

	// Check if LPFMTFILES are specified
	if (fmtnames.size() != 2 && fmtnames.size() != 0)
	{
		errmsg.append("Need exactly two file names for LPFMTFILES (Line Prefix FMT File Names)");
		return false;
	}

	// ------- Begin: LPFMTFILES are not specified, use FMT mapping -----
	if (fmtnames.size() == 0)
	{
		// Check if LPFMTMAP exists,
		if (params->getLinePrefixFMTMappingFile().empty())
		{
			// LPFMTMAP does not exist, report error and quit
			errmsg.append("Need LPFMTMAP");
			return false;
		}

		// LPFMTMAP exist, read and parse map
		VicarFmtMap fmtMap(params->getLinePrefixFMTMappingFile());
		string bltype1, bltype2;

		// check if BLTYPES are specified
		if (params->getBlTypes().size() != 0 && params->getBlTypes().size() != 2)
		{
			errmsg.append("Need exactly two strings for BLTYPES.");
			return false;
		}

		// ----- Begin: BLTYPES are not specified, read Vicar system label -----
		if (params->getBlTypes().size() != 2)
		{
			char value[MAX_LABEL_VALUE_SIZE+1];
			zlget(iunit1, const_cast<char*>("SYSTEM"), const_cast<char*>("BLTYPE"), value, "ERR_ACT", "SA", NULL);
			bltype1 = string(value);
			zlget(iunit2, const_cast<char*>("SYSTEM"), const_cast<char*>("BLTYPE"), value, "ERR_ACT", "SA", NULL);
			bltype2 = string(value);
		}
		// ----- End: BLTYPES are not specified, read Vicar system label -------
		else
		{
			bltype1 = params->getBlTypes().at(0);
			bltype2 = params->getBlTypes().at(1);
		}

		// Using BLTYPES to read FMT file names from FMT map
		if (!fmtMap.getFmt(bltype1, fmt1))
		{
			errmsg.append("Cannot find FMT for ");
			errmsg.append(bltype1);
			return false;
		}
		if (!fmtMap.getFmt(bltype2, fmt2))
		{
			errmsg.append("Cannot find FMT for ");
			errmsg.append(bltype2);
			return false;
		}
	}
	// ------- End: LPFMTFILES are not specified, use FMT mapping -------
	// LPFMTFILES are specified, then obtain the FMT's file names from parameters
	else
	{
		fmt1 = fmtnames.at(0);
		fmt2 = fmtnames.at(1);
	}

	return true;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
// helper functions
bool compareOneLine (const unsigned char* buf1, size_t size1,
		const unsigned char* buf2, size_t size2,
		vector<BalmField> & allFields, string & differences)
{
	size_t numField = allFields.size();
	BalmField* field1;
	BalmField* field2;
	bool same = true;

	// go through the fields of the first file
	for (size_t i = 0; i < numField; i += 2)
	{
		field1 = &allFields.at(i);
		field2 = &allFields.at(i+1);
		if (!fieldEquals(*field1, buf1, size1, *field2, buf2, size2))
		{
			// for niceties, the values from respective files ought to be printed.
			// however, because FMT file is still messed up at this time,
			// it's better not to print them yet because the value maybe wrong.
			differences += " ";
			differences += field1->getName();
			same = false;

			// don't break here because I want to see all fields that are different
		}
	}

	return same;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
extern "C" bool lineprefixdifC()
{
	bool same=true;
	try
	{
		const DifpicParameters * params = DifpicParameters::instance();
		string differences;
		bool got_error = false;
		string errmsg;

		if (params->linePrefixDiffEnabled())
		{
			string fmt1, fmt2;
			BalmFormat format1 = NULL;
			BalmFormat format2 = NULL;
			vector<BalmField> allFields;
			int nl1, ns1, nlb1, nbb1;
			int nl2, ns2, nlb2, nbb2;
			int pixSize1=0, pixSize2=0;
			int iunit1, iunit2, status;

			status = zvunit(&iunit1,(char*) "INP", 1,NULL);
			zvsignal(iunit1, status, 1);
			zvopen(iunit1,"OP", "READ", "OPEN_ACT", "SA","IO_ACT", "SA", "COND", "BINARY", NULL);
			status = zvunit(&iunit2, (char*)"INP", 2,NULL);
			zvsignal(iunit2, status, 1);
			zvopen(iunit2, "OP", "READ", "OPEN_ACT", "SA", "IO_ACT", "SA", "COND", "BINARY", NULL);

			// Get FMT file
			got_error = !(getLPFMTs(fmt1, fmt2, iunit1, iunit2, errmsg));

			// ----------------- Begin: Parse FMT file --------------------
			if (!got_error)
			{
				format1 = parseFormat(fmt1.c_str());
				if (format1 == NULL)
				{
					errmsg = string ("Unknown error in format file ");
					errmsg += fmt1;
					got_error = true;
				}
			}

			if (!got_error)
			{
				format2 = parseFormat(fmt2.c_str());
				if (format2 == NULL)
				{
					errmsg = string ("Unknown error in format file ");
					errmsg += fmt2;
					got_error = true;
				}
			}

			// ----------------- End: Parse FMT file ----------------------

			// Get ignore fields
			const vector<string>& ignored = params->lpFieldsToIgnore();

			// get union of unignored fields
			if (!got_error)
			{
				string input1 = params->getInputFileNames().at(0);
				string input2 = params->getInputFileNames().at(1);
				got_error = !getUnionOfFields (format1, format2, input1, input2,
						ignored, allFields, differences, errmsg, &same);
			}


			// --------------- Begin: Diff line prefix for all lines --------------
			if (!got_error)
			{
				// get dimensions of data
				zvget(iunit1,"NL",&nl1,"NS",&ns1,
						"NLB",&nlb1,"NBB",&nbb1,
						"PIX_SIZE",&pixSize1,NULL);
				zvget(iunit2,"NL",&nl2,"NS",
						&ns2,"NLB",&nlb2,"NBB",&nbb2,
						"PIX_SIZE",&pixSize2,NULL);
				if (nl1 != nl2)
				{
					same = false;
					differences += "Files have different number of lines.\n";
				}
				else
				{
					size_t size1 = (nbb1 + ns1) * pixSize1;
					size_t size2 = (nbb2 + ns2) * pixSize2;
					unsigned char* buf1 = new unsigned char[size1];
					unsigned char* buf2 = new unsigned char[size2];
					size_t el1 = nl1 + nlb1;
					char tempErrMsg[64];
					bool tempSame;

					// ----------------- Begin: Loop through every lines of image ------------------
					// the two files have the same number of lines
					// so we just need to check the maximum line number of one file.
					for (size_t j = nlb1 + 1, k = nlb2 + 1; j <= el1; j++, k++)
					{
						string tempDifference;
						zvread(iunit1, buf1, (char*)"LINE", j, NULL);
						zvread(iunit2, buf2, (char*)"LINE", k, NULL);
						// Loop through every fields and diff
						tempSame = compareOneLine (buf1, size1, buf2, size2, allFields, tempDifference);
						same = same && tempSame;
						if (!tempSame)
						{
							sprintf(tempErrMsg, "Line %ld differs at fields:", j-nlb1);
							differences += tempErrMsg;
							differences += tempDifference;
							differences += ".\n";
						}
					}

					// ----------------- End: Loop through every lines of image --------------------

					delete [] buf1;
					delete [] buf2;
				}
			}
			// --------------- End: Diff line prefix for all lines ----------------

			status = zvclose(iunit1,NULL);
			status = zvclose(iunit2,NULL);

			if (format1 != NULL) freeFormat(format1);
			if (format2 != NULL) freeFormat(format2);

			if (got_error)
			{
				zvmessage((char*)errmsg.c_str(), (char*)"ERROR");
				return false;
			}
			if (!params->silent())
			{
				if (!same)
				{
					zvmessage("Found following line prefix differences between given images:\n", NULL);
					print(differences);
					zvmessage("\n--------------- Line prefix differs. ----------------\n", NULL);
				}
				else
					zvmessage("----------- Line prefix comparison passed. ----------\n", NULL);
				zvmessage("\n", NULL);
			}
		}
	}
	catch (const exception &e)
	{
		zvmessage(const_cast<char*>(e.what()), (char*)"Exception::lineprefixdifC");
	}
	return same;
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
extern "C" void FTN_NAME2(lineprefixdif, LINEPREFIXDIF) (int *status)
{
	*status = lineprefixdifC() ? 0 : 1;
}
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create difpic_pixel.f
$ DECK/DOLLARS="$ VOKAGLEVE"
C
C    REVISION HISTORY
C       1-85  SP   EXPANDED BUFFER SIZES TO 60000 AND REMOVED MESSAGE ABOUT
C                  PROCESSING FIRST 10000 BYTES IF IMAGE LINE TOO BIG. NOW
C                  USES 2 BUFFERS INSTEAD OF 3.
C       1-85  SP   MOVED COUNTING OF DIFFERENCES BEFORE COMPARING AGAINST
C                  MINDN BECAUSE MINDN IS 0 FOR BYTE DATA.
C       1-85  SP   ADDED MOD PARAMETER FOR BYTE DATA.
C       1-85  SP   ADDED WCHECK CALL AFTER WRITES.
C       1-85  SP   ADDED CODE TO AVOID INTEGER OVERFLOW ON EXTREME HALFWORD
C                  VALUES.
C       1-85  SP   CONVERTED TO VICAR2 SUBROUTINE CALLS.  ( U_FORMAT and 
C                  optional parameters in XVREAD and XVWRIT avoided because
C                  of apparent speed problems.)
C       1-85  SP   CHANGED MESSAGE 'NUMBER OF NONZERO PIXELS' TO 'NUMBER OF
C                  DIFFERENT PIXELS'.
C       1-85  SP   CHANGED TO IGNORE FORMAT PARAMETER BECAUSE VICAR2 USES
C                  ONLY THE FORMAT IN LABEL.
C       1-85  SP   MADE OUTPUT FILE OPTIONAL TO ALLOW GREATER SPEED.
C      12-91  SP   REPLACED PRNT CALLS WITH CALLS TO PRNINT AND PRNTREAL
C                  FOR SIMPLICITY.
C      12-91  SP   PORTED TO RUN ON BOTH UNIX AND VMS.
C       9-92  SP   Made buffer size 200000 bytes. Modified to handle 
C                  all data formats.  CHANGED AVE VALS TO DISPLAY AS FLOAT.
C                  CORRECTED "AVE DN OF PIX" TO "AVE VAL OF DIFFS"
C       3-93  SP   Modified to not use -2147483648 to work around Sun compiler.
C                  Added ability to handle 3d files if SIZE field defaulted
C                  and no output file specified.
C       7-94  SP   Allowed format="WORD" as alternative to HALF.
C       8-03  lwk  removed restrictions on NB, added SB parameter;  use of
C		   optionals in XVREAD/WRIT is no longer a speed issue.
C      12-03  lwk  added checks on size/format of input files
C       6-04  lwk  allow for deviant Format types WORD & COMPLEX; removed 
c		mabend at BIP check because SIT objected to it(!)  (I don't
c		agree, and have retained all the other mabend calls, but
c		there's no need to make an issue of it)
C      07-10  lwk  fixed checks on sizes of input files; replaced abends
c		with reasonable default sizes
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      SUBROUTINE MAIN44_FTN(STATUS)

C============================================================================
C
      integer   MAXBYTESPAR, STATUS
      parameter (MAXBYTESPAR=200000)
      BYTE      BUF1(MAXBYTESPAR),BUF2(MAXBYTESPAR)

C  COMMON BLOCK - written only by MAIN44, read by subroutines.
      INTEGER      INFILE1,INFILE2,NO,OUTFILE,SL,SS,EL,ES,SB,EB
      COMMON /C1/  INFILE1,INFILE2,NO,OUTFILE,SL,SS,EL,ES,SB,EB

      CHARACTER*12 FORM1, FORM2
      CHARACTER*8 ORG1, ORG2

      INTEGER  COUNT,DEFAULTED,TSTATUS
      LOGICAL  DFPXFG, XVPTST

C============================================================================
C
C  OPEN DATA SETS

C      call xvmessage('DIFPIC version 17Sep11', ' ')
      STATUS=0
      TSTATUS=0
      CALL XVUNIT( INFILE1, 'INP', 1, IND, ' ' )
      CALL XVOPEN( INFILE1,IND, 'OP', 'READ', 'OPEN_ACT', 'SA',
     . 'IO_ACT', 'SA', ' ')
      CALL XVGET( INFILE1, IND, 'FORMAT', FORM1, 'PIX_SIZE', IPIXSIZE,
     . 'ORG', ORG1, 'NL', NL1, 'NS', NS1, 'NB', NB1, ' ')
      IF (NS1*IPIXSIZE .GT. MAXBYTESPAR) call mabend(
     . 'Record too big for buffer, notify cognizant programmer.',
     . ' ')
      if (nb1.gt.1 .and. org1.eq.'BIP') then
	call xvmessage(
     .  ' BIP files not supported, use program TRAN to convert to BSQ',
     .  ' ')
        CALL XVCLOSE(INFILE1,TSTATUS,' ')
	return
      endif
      if (form1.eq.'WORD') form1 = 'HALF'

      CALL XVUNIT( INFILE2, 'INP', 2, IND, ' ' )
      CALL XVOPEN( INFILE2, IND, 'OP', 'READ', 'OPEN_ACT', 'SA',
     . 'IO_ACT', 'SA', ' ')
      CALL XVGET( INFILE2, IND, 'FORMAT', FORM2, 'ORG', ORG2,
     . 'NL', NL2, 'NS', NS2, 'NB', NB2, ' ')
      if (nb2.gt.1 .and. org2.eq.'BIP') then
	call xvmessage(
     .  ' BIP files not supported, use program TRAN to convert to BSQ',
     .  ' ')
        CALL XVCLOSE(INFILE1,TSTATUS,' ')
        CALL XVCLOSE(INFILE2,TSTATUS,' ')
	return
      endif
      if (form2.eq.'WORD') form2 = 'HALF'

      ! just look at 4 bytes to make COMP same as COMPLEX
      if (form1(1:4).ne.form2(1:4)) then
        CALL XVCLOSE(INFILE1,TSTATUS,' ')
        CALL XVCLOSE(INFILE2,TSTATUS,' ')
	call mabend('Files must have same formats')
      endif

      if (org1.eq.'BIL' .or. org2.eq.'BIL') call xvmessage(
     . ' Warning: BIL format may cause performance degradation',' ')

      CALL XVSIZE( SL, SS, NLO, NSO, NLI, NSI )   ! GET SIZE PARAMETER.
      CALL XVBANDS( SB, NBO, NBI)
      ! n?i is from 1st input label ... replace with smallest value
      ! n?o is from param N? or SIZE/BANDS, if specified;  else nli
      ! make n?o smallest value to fit both inputs
      nsi = min(ns1,ns2)
      if ((ss+nso-1).gt.nsi) then
        call xvmessage(' NS too large, reduced to fit input',' ')
        nso = nsi-ss+1
      endif
      nli = min(nl1,nl2)
      if ((sl+nlo-1).gt.nli) then
        call xvmessage(' NL too large, reduced to fit input',' ')
        nlo = nli-sl+1
      endif
      nbi = min(nb1,nb2)
      if ((sb+nbo-1).gt.nbi) then
        call xvmessage(' NB too large, reduced to fit input',' ')
        nbo = nbi-sb+1
      endif

c  these don't work!
c     if ( (nl1.ne.nl2.and.sl.eq.1.and.nlo.eq.nli) .or.
c    .     (ns1.ne.ns2.and.ss.eq.1.and.nso.eq.nsi) .or.
c    .     (nb1.ne.nb2.and.sb.eq.1.and.nbo.eq.nbi) ) call mabend(
c    .' Files have different dimensions, specify SIZE/BANDS parameter!')

      EL=SL+NLO-1
      ES=SS+NSO-1
      EB=SB+NBO-1

      CALL XVPCNT( 'OUT', NO )     ! NUMBER OF OUTPUT FILES.
      IF ( NO .NE. 0)  THEN
        CALL XVUNIT( OUTFILE, 'OUT', 1, IND, ' ' )
        CALL XVOPEN( OUTFILE, IND, 'OP', 'WRITE', 'OPEN_ACT', 'SA',
     .      'IO_ACT', 'SA', 'U_NL', NLO, 'U_NS', NSO, 'U_NB', NBO, ' ' )
      END IF


      IF (FORM1(1:4) .EQ. 'BYTE')  THEN
         CALL DIFPICB(BUF1, BUF2, STATUS)
      ELSEIF (FORM1(1:4) .EQ. 'HALF' .OR. FORM1(1:4) .EQ. 'WORD') THEN
         CALL DIFPICH(BUF1, BUF2, STATUS)
      ELSEIF (FORM1(1:4) .EQ. 'FULL') THEN
         CALL DIFPICF(BUF1, BUF2, STATUS)
      ELSEIF (FORM1(1:4) .EQ. 'REAL') THEN
         CALL DIFPICR(BUF1, BUF2, STATUS)
      ELSEIF (FORM1(1:4) .EQ. 'DOUB') THEN
         CALL DIFPICD(BUF1, BUF2, STATUS)
      ELSEIF (FORM1(1:4) .EQ. 'COMP') THEN
         CALL DIFPICC(BUF1, BUF2, STATUS)
      ELSE
         CALL XVMESSAGE('ERROR: INVALID DATA FORMAT FROM XVGET', ' ')
      END IF

      CALL XVCLOSE(INFILE1,TSTATUS,' ')
      CALL XVCLOSE(INFILE2,TSTATUS,' ')
      RETURN
      END

C##################################################################
      SUBROUTINE DIFPICB(BUF1, BUF2, STATUS)

      include 'fortport'

      BYTE        BUF1(*),BUF2(*), STATUS

C  COMMON BLOCK - written only by MAIN44, read by subroutines.
      INTEGER      INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB
      COMMON /C1/  INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB

      INTEGER     IPTOT, INTOT, NPIX, NPOS, NNEG, MAXDN, MINDN
      LOGICAL MODFLAG
      LOGICAL XVPTST

C==================================================================
      MODFLAG = XVPTST( 'MOD' )     ! MOD ONLY FOR BYTE DATA.

      STATUS=0
      IPTOT=0
      INTOT=0
      NPIX=0
      NPOS=0
      NNEG=0
      MAXDN=255
      MINDN=0
      IREC=0
C      CALL XVMESSAGE('IN BYTE DIF PROGRAM',' ')
      IF (NO .EQ. 0)   THEN
                             ! DIFPIC - NO OUTPUT FILE
        DO IB=SB,EB
          DO IL=SL,EL
            CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            DO J= SS, ES
              IF ( BUF1(J) .NE. BUF2(J) )    NPIX = NPIX + 1
            ENDDO 
          ENDDO
        ENDDO
        IF (NPIX .GT. 0) STATUS=1
        CALL PRNTINT(NPIX,' NUMBER OF DIFFERENT PIXELS =')
        RETURN
      ENDIF

      ! DIFPIC WITH OUTPUT FILE

      IBO = 1
      DO IB=SB,EB
        ILO = 1
        DO IL=SL,EL
          CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB, ' ' )
          CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB, ' ' )

          IREC=IREC+1    !THIS IS THE OUTPUT RECORD NUMBER

          II = 1
          DO J= SS, ES
             K= BYTE2INT(BUF1(J)) - BYTE2INT(BUF2(J)) 
                           ! BYTE2INT CONVERTS UNSIGNED BYTE TO INTEGER.

             IF(K.NE.0)THEN
               IF(K.GT.0)THEN  !COUNT UP THE NUMBER OF POSITIVE 
                                                        !DIFFERENCES
                NPOS=NPOS+1
                IPTOT=IPTOT+K
               ENDIF
               IF(K.LT.0)THEN !COUNT UP THE NUMBER OF NEGATIVE 
                                                    !DIFFERENCES
                NNEG=NNEG+1
                INTOT=INTOT+K
               ENDIF	
             ENDIF

             IF (MODFLAG)    THEN
                 IF (K .LT. MINDN)  K = K + 256
             ELSE
               IF(K.GT.MAXDN) K=MAXDN
               IF(K.LT.MINDN) K=MINDN
             END IF

             BUF1(II) =INT2BYTE(K)               !LOW ORDER BYTE.
             II = II + 1
          END DO 
          IF ( MOD(IREC,1000) .EQ. 0 ) CALL PRNTINT(IREC,' REC # =')
          CALL XVWRIT( OUTFILE, BUF1, IND, 'LINE', ILO, 'BAND', IBO,
     1 ' ')

          ILO = ILO+1
        ENDDO
        IBO = IBO+1
      ENDDO

      IF (NPOS.NE.0 .OR. NNEG.NE.O) STATUS=1
      IF (NPOS.NE.0)
     .  CALL PRNTREAL(FLOAT(IPTOT)/NPOS,' AVE VAL OF POS DIFFS=')
      CALL PRNTINT(NPOS,' NUMBER OF POS DIFF=')
      IF (NNEG.NE.0)
     .  CALL PRNTREAL(FLOAT(INTOT)/NNEG,' AVE VAL OF NEG DIFFS=')
      CALL PRNTINT(NNEG,' NUMBER OF NEG DIFFS=')
      CALL PRNTINT(NNEG+NPOS,' TOTAL NUMBER OF DIFFERENT PIXELS=')
      NB = EB-SB+1
      NL = EL-SL+1
      NS = ES-SS+1
      CALL PRNTREAL(FLOAT(IPTOT+INTOT)/(NB*NL*NS),
     . ' AVE VAL OF DIFFS=')
      CALL PRNTREAL(100.0*(NPOS+NNEG)/(NB*NL*NS),' % DIFF PIXELS=')

      RETURN
      END

C##################################################################
      SUBROUTINE DIFPICH(BUF1, BUF2, STATUS)

      INTEGER*2     BUF1(*),BUF2(*), STATUS

C  COMMON BLOCK - written only by MAIN44, read by subroutines.
      INTEGER      INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB
      COMMON /C1/  INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB

      INTEGER     NPIX, NPOS, NNEG, MAXDN, MINDN
      REAL        IPTOT, INTOT
C==================================================================
      STATUS=0
      IPTOT=0.
      INTOT=0.
      NPIX=0
      NPOS=0
      NNEG=0
      MAXDN=32767
      MINDN=-32768
      IREC=0

      IF (NO .EQ. 0)   THEN
                             ! DIFPIC - NO OUTPUT FILE
        DO IB=SB,EB
          DO IL=SL,EL
            CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            DO J= SS, ES
              IF ( BUF1(J) .NE. BUF2(J) )    NPIX = NPIX + 1
            ENDDO 
          ENDDO
        ENDDO
        IF (NPIX .GT. 0) STATUS=1
        CALL PRNTINT(NPIX,' NUMBER OF DIFFERENCES =')
        RETURN
      ENDIF

      ! DIFPIC WITH OUTPUT FILE

      IBO = 1
      DO IB=SB,EB
        ILO = 1
        DO IL=SL,EL
          CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB, ' ' )
          CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB, ' ' )

          IREC=IREC+1    !THIS IS THE OUTPUT RECORD NUMBER

          II = 1
          DO J= SS, ES
            K=BUF1(J)
            K = K - BUF2(J)       ! AVOID INTEGER OVERFLOW ON HALFWORD DATA.

            IF(K.NE.0)THEN
              IF(K.GT.0)THEN  !COUNT UP THE NUMBER OF POSITIVE DIFFERENCES
                NPOS=NPOS+1
                IPTOT=IPTOT+K
              ENDIF
              IF(K.LT.0)THEN !COUNT UP THE NUMBER OF NEGATIVE DIFFERENCES
                NNEG=NNEG+1
                INTOT=INTOT+K
              ENDIF	
            ENDIF

            IF(K.GT.MAXDN)THEN
              BUF1(II)=MAXDN
            ELSE IF(K.LT.MINDN)THEN
              BUF1(II)=MINDN
            ELSE
              BUF1(II)=K
            ENDIF
 
            II = II + 1
          END DO 
          IF ( MOD(IREC,1000) .EQ. 0 ) CALL PRNTINT(IREC,' REC # =')
          CALL XVWRIT( OUTFILE, BUF1, IND, 'LINE', ILO, 'BAND', IBO,
     1 ' ')

          ILO = ILO+1
        ENDDO
        IBO = IBO+1
      ENDDO

      IF (NPOS.NE.0 .OR. NNEG.NE.O) STATUS=1

      IF(NPOS.NE.0)
     .  CALL PRNTREAL(IPTOT/NPOS,' AVE VAL OF POS DIFFS=')
      CALL PRNTINT(NPOS,' NUMBER OF POS DIFF=')
      IF(NNEG.NE.0)
     .  CALL PRNTREAL(INTOT/NNEG,' AVE VAL OF NEG DIFFS=')
      CALL PRNTINT(NNEG,' NUMBER OF NEG DIFFS=')
      CALL PRNTINT(NNEG+NPOS,' TOTAL NUMBER OF DIFFERENT PIXELS=')
      NB = EB-SB+1
      NL = EL-SL+1
      NS = ES-SS+1
      CALL PRNTREAL((IPTOT+INTOT)/(NB*NL*NS),
     . ' AVE VAL OF DIFFS=')
      CALL PRNTREAL(100.0*(NPOS+NNEG)/(NB*NL*NS),' % DIFF PIXELS=')

      RETURN
      END

C##################################################################
      SUBROUTINE DIFPICF(BUF1, BUF2, STATUS)

      INTEGER*4     BUF1(*),BUF2(*),STATUS

C  COMMON BLOCK - written only by MAIN44, read by subroutines.
      INTEGER      INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB
      COMMON /C1/  INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB

      INTEGER     NPIX, NPOS, NNEG, MAXDN, MINDN
      REAL        IPTOT, INTOT, RMAX, RMIN
C==================================================================
      STATUS=0
      IPTOT=0.
      INTOT=0.
      NPIX=0
      NPOS=0
      NNEG=0
      MAXDN=2147483647
      MINDN=-MAXDN-1
      RMAX = MAXDN
      RMIN = MINDN
      IREC=0

      IF (NO .EQ. 0)   THEN
                             ! DIFPIC - NO OUTPUT FILE
        DO IB=SB,EB
          DO IL=SL,EL
            CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            DO J= SS, ES
              IF ( BUF1(J) .NE. BUF2(J) )    NPIX = NPIX + 1
            ENDDO 
          ENDDO
        ENDDO
        IF (NPIX .GT. 0) STATUS=1
        CALL PRNTINT(NPIX,' NUMBER OF DIFFERENCES =')
        RETURN
      ENDIF

      ! DIFPIC WITH OUTPUT FILE

      IBO = 1
      DO IB=SB,EB
        ILO = 1
        DO IL=SL,EL
          CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB, ' ' )
          CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB, ' ' )

          IREC=IREC+1    !THIS IS THE OUTPUT RECORD NUMBER

          II = 1
          DO J= SS, ES
            K=BUF1(J)
            L=BUF2(J)
            IF (K .GE. 0 .AND. L .GE. 0) THEN   ! MOST COMMON CASE
              K = K - L
              R = FLOAT(K)
            ELSE 
              R = FLOAT(K) - FLOAT(L)    ! CHECK IF K-L IS OUT OF RANGE.
              IF ( R .LT. RMIN ) THEN
                K = MINDN
              ELSE IF ( R .GT. RMAX ) THEN
                K = MAXDN
              ELSE 
                K = K-L
              END IF
            END IF

            IF(K.NE.0)THEN
              IF(K.GT.0)THEN  !COUNT UP THE NUMBER OF POSITIVE DIFFERENCES
                NPOS=NPOS+1
                IPTOT=IPTOT+R
              ENDIF
              IF(K.LT.0)THEN !COUNT UP THE NUMBER OF NEGATIVE DIFFERENCES
                NNEG=NNEG+1
                INTOT=INTOT+R
              ENDIF	
            ENDIF

            BUF1(II)=K
            II = II + 1
          END DO 
          IF ( MOD(IREC,1000) .EQ. 0 ) CALL PRNTINT(IREC,' REC # =')
          CALL XVWRIT( OUTFILE, BUF1, IND, 'LINE', ILO, 'BAND', IBO,
     1 ' ')

          ILO = ILO+1
        ENDDO
        IBO = IBO+1
      ENDDO

      IF (NPOS.NE.0 .OR. NNEG.NE.O) STATUS=1

      IF(NPOS.NE.0)
     .  CALL PRNTREAL(IPTOT/NPOS,' AVE VAL OF POS DIFFS=')
      CALL PRNTINT(NPOS,' NUMBER OF POS DIFF=')
      IF(NNEG.NE.0)
     .  CALL PRNTREAL(INTOT/NNEG,' AVE VAL OF NEG DIFFS=')
      CALL PRNTINT(NNEG,' NUMBER OF NEG DIFFS=')
      CALL PRNTINT(NNEG+NPOS,' TOTAL NUMBER OF DIFFERENT PIXELS=')
      NB = EB-SB+1
      NL = EL-SL+1
      NS = ES-SS+1
      CALL PRNTREAL((IPTOT+INTOT)/(NB*NL*NS),
     . ' AVE VAL OF DIFFS=')
      CALL PRNTREAL(100.0*(NPOS+NNEG)/(NB*NL*NS),' % DIFF PIXELS=')

      RETURN
      END

C##################################################################
      SUBROUTINE DIFPICR(BUF1, BUF2, STATUS)

      REAL*4     BUF1(*),BUF2(*),STATUS

C  COMMON BLOCK - written only by MAIN44, read by subroutines.
      INTEGER      INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB
      COMMON /C1/  INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB

      INTEGER     NPIX, NPOS, NNEG
      REAL        IPTOT, INTOT
C==================================================================
      STATUS=0
      IPTOT=0.
      INTOT=0.
      NPIX=0
      NPOS=0
      NNEG=0
      IREC=0

      IF (NO .EQ. 0)   THEN
                             ! DIFPIC - NO OUTPUT FILE
        DO IB=SB,EB
          DO IL=SL,EL
            CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            DO J= SS, ES
              IF ( BUF1(J) .NE. BUF2(J) )    NPIX = NPIX + 1
            ENDDO 
          ENDDO
        ENDDO
        IF (NPIX .GT. 0) STATUS=1
        CALL PRNTINT(NPIX,' NUMBER OF DIFFERENCES =')
        RETURN
      ENDIF

      ! DIFPIC WITH OUTPUT FILE

      IBO = 1
      DO IB=SB,EB
        ILO = 1
        DO IL=SL,EL
          CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB, ' ' )
          CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB, ' ' )

          IREC=IREC+1    !THIS IS THE OUTPUT RECORD NUMBER

          II = 1
          DO J= SS, ES
            R=BUF1(J) - BUF2(J)

            IF(R.NE.0)THEN
              IF(R.GT.0.)THEN  !COUNT UP THE NUMBER OF POSITIVE DIFFERENCES
                NPOS=NPOS+1
                IPTOT=IPTOT+R
              ENDIF
              IF(R.LT.0.)THEN !COUNT UP THE NUMBER OF NEGATIVE DIFFERENCES
                NNEG=NNEG+1
                INTOT=INTOT+R
              ENDIF	
            ENDIF
            BUF1(II) = R
            II = II + 1
          END DO 
          IF ( MOD(IREC,1000) .EQ. 0 ) CALL PRNTINT(IREC,' REC # =')
          CALL XVWRIT( OUTFILE, BUF1, IND, 'LINE', ILO, 'BAND', IBO,
     1 ' ')

          ILO = ILO+1
        ENDDO
        IBO = IBO+1
      ENDDO

      IF (NPOS.NE.0 .OR. NNEG.NE.O) STATUS=1

      IF(NPOS.NE.0) CALL PRNTREAL(IPTOT/NPOS,' AVE VAL OF POS DIFFS=')
      CALL PRNTINT(NPOS,' NUMBER OF POS DIFF=')
      IF(NNEG.NE.0) CALL PRNTREAL(INTOT/NNEG,' AVE VAL OF NEG DIFFS=')
      CALL PRNTINT(NNEG,' NUMBER OF NEG DIFFS=')
      CALL PRNTINT(NNEG+NPOS,' TOTAL NUMBER OF DIFFERENT PIXELS=')
      NB = EB-SB+1
      NL = EL-SL+1
      NS = ES-SS+1
      CALL PRNTREAL((IPTOT+INTOT)/(NB*NL*NS),
     . ' AVE VAL OF DIFFS=')
      CALL PRNTREAL(100.0*(NPOS+NNEG)/(NB*NL*NS),' % DIFF PIXELS=')

      RETURN
      END

C##################################################################
      SUBROUTINE DIFPICD(BUF1, BUF2, STATUS)

      REAL*8     BUF1(*),BUF2(*), STATUS

C  COMMON BLOCK - written only by MAIN44, read by subroutines.
      INTEGER      INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB
      COMMON /C1/  INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB

      INTEGER     NPIX, NPOS, NNEG
      REAL*8      R, IPTOT, INTOT
C==================================================================
      STATUS=0
      IPTOT=0.
      INTOT=0.
      NPIX=0
      NPOS=0
      NNEG=0
      IREC=0

      IF (NO .EQ. 0)   THEN
                             ! DIFPIC - NO OUTPUT FILE
        DO IB=SB,EB
          DO IL=SL,EL
            CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            DO J= SS, ES
              IF ( BUF1(J) .NE. BUF2(J) )    NPIX = NPIX + 1
            ENDDO 
          ENDDO
        ENDDO
        IF (NPIX .GT. 0) STATUS=1
        CALL PRNTINT(NPIX,' NUMBER OF DIFFERENCES =')
        RETURN
      ENDIF

      ! DIFPIC WITH OUTPUT FILE

      IBO = 1
      DO IB=SB,EB
        ILO = 1
        DO IL=SL,EL
          CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB, ' ' )
          CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB, ' ' )

          IREC=IREC+1    !THIS IS THE OUTPUT RECORD NUMBER

          II = 1
          DO J= SS, ES
            R=BUF1(J) - BUF2(J)

            IF(R.NE.0)THEN
              IF(R.GT.0.)THEN  !COUNT UP THE NUMBER OF POSITIVE DIFFERENCES
                NPOS=NPOS+1
                IPTOT=IPTOT+R
              ENDIF
              IF(R.LT.0.)THEN !COUNT UP THE NUMBER OF NEGATIVE DIFFERENCES
                NNEG=NNEG+1
                INTOT=INTOT+R
              ENDIF	
            ENDIF
            BUF1(II) = R
            II = II + 1
          END DO 
          IF ( MOD(IREC,1000) .EQ. 0 ) CALL PRNTINT(IREC,' REC # =')
          CALL XVWRIT( OUTFILE, BUF1, IND, 'LINE', ILO, 'BAND', IBO,
     1 ' ')

          ILO = ILO+1
        ENDDO
        IBO = IBO+1
      ENDDO

      IF (NPOS.NE.0 .OR. NNEG.NE.O) STATUS=1

      IF(NPOS.NE.0)
     .  CALL PRNTREAL(SNGL(IPTOT)/NPOS,' AVE VAL OF POS DIFFS=')
      CALL PRNTINT(NPOS,' NUMBER OF POS DIFF=')
      IF(NNEG.NE.0)
     .  CALL PRNTREAL(SNGL(INTOT)/NNEG,' AVE VAL OF NEG DIFFS=')
      CALL PRNTINT(NNEG,' NUMBER OF NEG DIFFS=')
      CALL PRNTINT(NNEG+NPOS,' TOTAL NUMBER OF DIFFERENT PIXELS=')
      NB = EB-SB+1
      NL = EL-SL+1
      NS = ES-SS+1
      CALL PRNTREAL(SNGL(IPTOT+INTOT)/(NB*NL*NS),
     . ' AVE VAL OF DIFFS=')
      CALL PRNTREAL(100.0*(NPOS+NNEG)/(NB*NL*NS),' % DIFF PIXELS=')

      RETURN
      END

C##################################################################
      SUBROUTINE DIFPICC(BUF1, BUF2, STATUS)

      COMPLEX*8   BUF1(*),BUF2(*),STATUS

C  COMMON BLOCK - written only by MAIN44, read by subroutines.
      INTEGER      INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB
      COMMON /C1/  INFILE1,INFILE2, NO,OUTFILE,SL,SS,EL,ES,SB,EB

      INTEGER     NPIX, NPOS, NNEG
      COMPLEX*8   R, IPTOT, INTOT
C==================================================================
      STATUS=0
      IPTOT=(0., 0.)
      INTOT=(0., 0.)
      NPIX=0
      NPOS=0
      NNEG=0
      IREC=0

      IF (NO .EQ. 0)   THEN
                             ! DIFPIC - NO OUTPUT FILE
        DO IB=SB,EB
          DO IL=SL,EL
            CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB,
     1    ' ' )
            DO J= SS, ES
              IF ( BUF1(J) .NE. BUF2(J) )    NPIX = NPIX + 1
            ENDDO 
          ENDDO
        ENDDO
        IF (NPIX .GT. 0) STATUS=1
        CALL PRNTINT(NPIX,' NUMBER OF DIFFERENCES =')
        RETURN
      ENDIF

      ! DIFPIC WITH OUTPUT FILE

      IBO = 1
      DO IB=SB,EB
        ILO = 1
        DO IL=SL,EL
          CALL XVREAD( INFILE1, BUF1, IND, 'LINE', IL, 'BAND', IB, ' ' )
          CALL XVREAD( INFILE2, BUF2, IND, 'LINE', IL, 'BAND', IB, ' ' )

          IREC=IREC+1    !THIS IS THE OUTPUT RECORD NUMBER

          II = 1
          DO J= SS, ES
            R=BUF1(J) - BUF2(J)

            IF(BUF1(J) .NE. BUF2(J) )THEN
              IF(REAL(R).GE.0.)THEN  !COUNT UP THE NUMBER OF POSITIVE DIFFERENCES
                NPOS=NPOS+1
                IPTOT=IPTOT+R
              ENDIF
              IF(REAL(R).LT.0.)THEN !COUNT UP THE NUMBER OF NEGATIVE DIFFERENCES
                NNEG=NNEG+1
                INTOT=INTOT+R
              ENDIF	
            ENDIF
            BUF1(II) = R
            II = II + 1
          END DO 
          IF ( MOD(IREC,1000) .EQ. 0 ) CALL PRNTINT(IREC,' REC # =')
          CALL XVWRIT( OUTFILE, BUF1, IND, 'LINE', ILO, 'BAND', IBO,
     1 ' ')

          ILO = ILO+1
        ENDDO
        IBO = IBO+1
      ENDDO

      IF (NPOS.NE.0 .OR. NNEG.NE.O) STATUS=1
      IF(NPOS.NE.0) CALL PRNT(10,1,IPTOT/NPOS,' AVE VAL OF POS DIFFS=')
      CALL PRNTINT(NPOS,' NUMBER OF POS DIFF=')
      IF(NNEG.NE.0) CALL PRNT(10,1,INTOT/NNEG,' AVE VAL OF NEG DIFFS=')
      CALL PRNTINT(NNEG,' NUMBER OF NEG DIFFS=')
      CALL PRNTINT(NNEG+NPOS,' TOTAL NUMBER OF DIFFERENT PIXELS=')
      NB = EB-SB+1
      NL = EL-SL+1
      NS = ES-SS+1
      CALL PRNT(10,1,(IPTOT+INTOT)/(NB*NL*NS),' AVE VAL OF DIFFS=')
      CALL PRNTREAL(100.0*(NPOS+NNEG)/(NB*NL*NS),' % DIFF PIXELS=')

      RETURN
      END

C##################################################################

      SUBROUTINE PRNTINT( IVAL, TITLE )
C
C     PURPOSE: PRNTINT prints the INTEGER value IVAL on the same line
C              and to the right of the description string TITLE.
C
C     REVISION HISTORY
C       12-91   SP  ORIGINAL VERSION PATTERNED AFTER PRNT FOR A SINGLE VALUE.
C
      INTEGER*4     IVAL
      CHARACTER*(*) TITLE
      CHARACTER*132 BUF
      INTEGER       L, N

C==============START OF EXECUTABLE CODE================================

      L = LEN( TITLE)
      L = MIN( 100, L)              ! NO SPACE FOR MORE THAN ABOUT 100 CHARS.
      BUF(1:L) = TITLE

C  IF 4 DIGITS ARE ENOUGH USE 4 DIGITS; ELSE USE 11.

      IF (-999 .LE. IVAL .AND. IVAL .LE. 9999)  THEN
         WRITE( BUF(L+1:L+4), 9040) IVAL
         N = L+4
      ELSE
         WRITE( BUF(L+1:L+11), 9110) IVAL
         N = L+11
      END IF

      CALL XVMESSAGE( BUF(1:N), ' ')
      RETURN

9040  FORMAT( I4 )
9110  FORMAT( I11)              ! MAXIMUM LENGTH FOR 32-BIT INTEGER
      END
      SUBROUTINE PRNTREAL( RVAL, TITLE )
C
C     PURPOSE: PRNTREAL prints the REAL value RVAL on the same line
C              and to the right of the description string TITLE.
C
C     REVISION HISTORY
C       12-91   SP  ORIGINAL VERSION PATTERNED AFTER PRNT FOR A SINGLE VALUE.
C
      REAL*4        RVAL
      CHARACTER*(*) TITLE
      CHARACTER*132 BUF
      INTEGER       L, N

C==============START OF EXECUTABLE CODE================================

      L = LEN( TITLE)
      L = MIN( 100, L)              ! NO SPACE FOR MORE THAN ABOUT 100 CHARS.
      BUF(1:L) = TITLE

      WRITE( BUF(L+1:L+13), 9130) RVAL
      N = L+13

      CALL XVMESSAGE( BUF(1:N), ' ')
      RETURN

9130  FORMAT( G13.6)              ! 6 SIGNIFICANT DIGITS IN FIXED POINT
                                  ! FOR MAGNITUDES FROM .1 TO 999999.
                                  ! OTHERWISE IN EXPONENTIAL FORMAT.
      END
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create difpic_bridges.h
$ DECK/DOLLARS="$ VOKAGLEVE"
#ifndef __DIFPIC_BRIDGES_H__
#define __DIFPIC_BRIDGES_H__

extern "C"
{
  #include "xvmaininc.h"
  #include "applic.h"
  #include "ftnbridge.h"
  void FTN_NAME2(main44_ftn,MAIN44_FTN)(int *STATUS);
  bool labeldifC();
  bool histdifC();
  bool binaryheaderdifC();
  bool lineprefixdifC();
}

#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create DifpicParameters.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
       Copyright 2011-Present
       California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledged. 09-08-2011.

       Auther : Rajesh R. Patel

History :
---------

Date              Who                        What
-------------------------------------------------------------------------------
09/13/2011        Raj       Initial Release

 */
#ifndef __DIFF2_PARAMETERS_H__
#define __DIFF2_PARAMETERS_H__

#include <cstdarg>
#include <string>
#include <vector>
//using namespace std;

namespace jpl
{
namespace mipl
{
namespace p2
{
namespace difpic
{
class DifpicParameters
{

public:
	//DifpicParameters() throw (std::exception);
	virtual ~DifpicParameters() {}
	static DifpicParameters* instance() throw (std::exception);
	/*
	    {
	      if (instance_==NULL)
		instance_=new DifpicParameters();
	      return instance_;
	    }
	 */
	bool  silent() const { return this->silent_; }
	bool  pixDiffEnabled() const { return this->pixDiffEnabled_; }
	bool  labelDiffEnabled() const { return this->labelDiffEnabled_; }
	bool  historyDiffEnabled() const { return this->histDiffEnabled_; }
	bool  binaryHeaderDiffEnabled() const { return this->binDiffEnabled_; }
	bool  linePrefixDiffEnabled() const { return this->linePreDiffEnabled_; }
	const std::string& getBinaryHeaderFMTMappingFile() const { return this->bhFmtMappingFile_; }
	const std::string& getLinePrefixFMTMappingFile() const { return this->lpFmtMappingFile_; }
	const std::vector<std::string>& getBlTypes() const { return this->bltypes_; }
	const std::vector<std::string>& getBinaryHeaderFMTFileNames() const { return this->bhFMTFileNames_; }
	const std::vector<std::string>& getLinePrefixFMTFileNames() const { return this->lpFMTFileNames_; }
	const std::vector<std::string>& labelsToIgnore() const { return this->labelsToIgnore_; }
	const std::vector<std::string>& propertiesToIgnore() const { return this->propertiesToIgnore_; }

	const std::vector<std::string>& getInputFileNames() const { return this->inputFileNames_; }
	const std::vector<std::string>& bhFieldsToIgnore() const { return this->bhFieldsToIgnore_; }
	const std::vector<std::string>& lpFieldsToIgnore() const { return this->lpFieldsToIgnore_; }
protected:

	DifpicParameters();
	static DifpicParameters* instance_;
	virtual void getParameters();

	std::vector <std::string> labelsToIgnore_;
	std::vector <std::string> propertiesToIgnore_;
	std::vector<std::string> bhFMTFileNames_;
	std::vector<std::string> lpFMTFileNames_;
	std::vector<std::string> bhFieldsToIgnore_;
	std::vector<std::string> lpFieldsToIgnore_;
	std::vector<std::string> inputFileNames_;
	std::vector<std::string> bltypes_;
	std::string bhFmtMappingFile_;
	std::string lpFmtMappingFile_;

	void getMultiStringValueParam(const std::string& name,
			std::vector<std::string>& values,
			int maxLength,
			int minCount,
			int maxCount,
			bool fillTillMax=false);

	bool silent_, labelDiffEnabled_,
	histDiffEnabled_,pixDiffEnabled_,
	binDiffEnabled_, linePreDiffEnabled_;
};
};
};
};
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create VicarImageUtil.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
  Copyright 2011-Present
  California Institute of Technology. 
  ALL RIGHTS RESERVED.
  U.S. Government Sponsorship acknowledged. 09-08-2011.

  Auther : Rajesh R. Patel

  History :
  ---------

  Date              Who                        What
  -------------------------------------------------------------------------------
  09/08/2011        Raj       Initial Release

 */
#ifndef __VICAR_IMAGE_UTIL_H__
#define __VICAR_IMAGE_UTIL_H__

#include "applic.h"
#include "zvproto.h"
#include <stdarg.h>
#include <string>
#include <vector>
#include <map>
//using namespace std;

namespace jpl
{
namespace mipl
{
namespace p2
{
namespace difpic
{
class VicarImageUtil
{
public:
	static std::string getVersion();

	static bool contains(const std::vector<std::string> &v,
			const std::string &element);
	static void clone(const std::map < std::string, std::map < std::string, std::vector < std::string >* >* > &from,
			std::map < std::string, std::map < std::string, std::vector < std::string >* >* > &to);
	static void clone(const std::map < std::string, std::vector < std::string >* > &from,
			std::map < std::string, std::vector < std::string >* > &to);
	static void formatDMsg(const std::string &label,
			const std::string &rval,
			const std::string &lval,
			std::string &outMsg);
	static void formatHdrMsg(const std::string &hdr,
			std::string &outMsg);
	static bool processExtraRHLabels(std::map < std::string, std::vector < std::string >* >& labels,
			const std::vector <std::string> &labelsToIgnore,
			std::string & differences);
	static bool processExtraRHProperties(std::map < std::string,
			std::map < std::string, std::vector < std::string >* >* > &labels,
			const std::vector <std::string> &propertiesToIgnore,
			const std::vector <std::string> &labelsToIgnore,
			std::string & differences);
	static std::string getValue(const std::vector <std::string >& v);
private:
};
};
};
};
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create VicarImageLabel.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
       Copyright 2011-Present
       California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledged. 09-08-2011.

       Auther : Rajesh R. Patel

History :
---------

Date              Who                        What
-------------------------------------------------------------------------------
09/08/2011        Raj       Initial Release

 */
#ifndef __VICAR_IMAGE_LABEL_H__
#define __VICAR_IMAGE_LABEL_H__

extern "C"
{
#include "applic.h"
#include "zvproto.h"
}
#include "VicarException.h"
#include <stdarg.h>
#include <string>
#include <vector>
#include <map>

namespace jpl
{
namespace mipl
{
namespace p2
{

/**
 * This class reads the entier VICAR label in memory. The label is stored as map of map of lables and its values
 * (map < string map <string, vector<string> * >* >), where the vector<string*> are all the values associated
 * with the label and the outter map is map with key is properties.
 */
class VicarImageLabel
{

public:

	VicarImageLabel(int unitNo) throw (VicarException);

	virtual ~VicarImageLabel();

	void listLabels(std::string &inThisString) const;

	bool compare(const VicarImageLabel &toThisLabels,
			const std::vector <std::string> &propertiesToIgnore,
			const std::vector <std::string> &labelsToIgnore,
			std::string& differences) const;

	bool getKeyword (const std::string & property, const std::string & keyword, std::vector<std::string> & value);

protected:

	int unitNo_;
	// std::map of properties that contains a key and it's values (more then one if it is an array).
	std::map < std::string, std::map < std::string, std::vector < std::string >* >* > labels_;
	//std::vector<std::string> properties_;

	void cleanUp();
	void init_() throw(VicarException);

	bool compare(std::map < std::string, std::map < std::string, std::vector < std::string >* >* > &lh,
			std::map < std::string, std::map < std::string, std::vector < std::string >* >* > &rh,
			const std::vector <std::string> &propertiesToIgnore,
			const std::vector <std::string> &labelsToIgnore,
			std::string& differences) const;

};
};
};
};
#endif
$ VOKAGLEVE
$!-----------------------------------------------------------------------------
$ create VicarHistoryLabel.h
$ DECK/DOLLARS="$ VOKAGLEVE"
/*
       Copyright 2011-Present
       California Institute of Technology. 
       ALL RIGHTS RESERVED.
       U.S. Government Sponsorship acknowledged. 09-08-2011.

       Auther : Rajesh R. Patel

History :
---------

Date              Who                        What
-------------------------------------------------------------------------------
09/08/2011        Raj       Initial Release

 */
#ifndef __VICAR_HISTORY_LABEL_H__
#define __VICAR_HISTORY_LABEL_H__

#include "applic.h"
#include "zvproto.h"
#include "VicarException.h"
#include <stdarg.h>
#include <string>
#include <vector>
#include <map>

namespace jpl
{
namespace mipl
{
namespace p2
{

/**
 * This class reads the entier VICAR label in memory. The label is stored as map of map of lables and its values
 * (map < string map <string, vector<string> * >* >), where the vector<string*> are all the values associated
 * with the label and the outter map is map with key is properties.
 */
class VicarHistoryLabel
{

public:

	VicarHistoryLabel(int unitNo) throw (VicarException);

	virtual ~VicarHistoryLabel();

	void listLabels(std::string &inThisString) const;

	bool compare(const VicarHistoryLabel &toThisLabels,
			const std::vector <std::string> &propertiesToIgnore,
			const std::vector <std::string> &labelsToIgnore,
			std::string& differences) const;

protected:

	int unitNo_;
	// std::map of properties that contains a key and it's values (more then one if it is an array).
	std::map < std::string, std::map < std::string, std::vector < std::string >* >* > labels_;
	//std::vector<std::string> properties_;

	void cleanUp();
	void init_() throw(VicarException);

	bool compare(std::map < std::string, std::map < std::string, std::vector < std::string >* >* > &lh,
			std::map < std::string, std::map < std::string, std::vector < std::string >* >* > &rh,
			const std::vector <std::string> &propertiesToIgnore,
			const std::vector <std::string> &labelsToIgnore,
			std::string& differences) const;
};
};
};
};
#endif
$ VOKAGLEVE
$ Return
$!#############################################################################
$PDF_File:
$ create difpic.pdf
process help=*

local dummy INTEGER

PARM INP TYPE=STRING COUNT=2
PARM OUT TYPE=STRING COUNT=(0:1)  DEFAULT=--
PARM SIZE TYPE=INTEGER COUNT=4 DEFAULT=(1,1,0,0)
PARM SL TYPE=INTEGER DEFAULT=1
PARM SS TYPE=INTEGER DEFAULT=1
PARM NL TYPE=INTEGER DEFAULT=0
PARM NS TYPE=INTEGER DEFAULT=0
PARM BANDS TYPE=INTEGER COUNT=2 DEFAULT=(1,0)
PARM SB TYPE=INTEGER DEFAULT=1
PARM NB TYPE=INTEGER DEFAULT=0
PARM MOD  TYPE=KEYWORD COUNT=0:1 VALID=MOD  DEFAULT=--

PARM PIXCMP TYPE=KEYWORD COUNT=0:1 VALID=(PIXCMP,NOPIXCMP) DEFAULT=PIXCMP
PARM ALL TYPE=KEYWORD COUNT=0:1 VALID=ALL DEFAULT=--
PARM VERBOSE TYPE=KEYWORD COUNT=0:1 VALID=(VERBOSE,SILENT) DEFAULT=VERBOSE
PARM LBLCMP TYPE=KEYWORD COUNT=0:1 VALID=(LBLCMP) DEFAULT=--
PARM HSTCMP TYPE=KEYWORD COUNT=0:1 VALID=(HSTCMP) DEFAULT=--
PARM LBL_IGNORE TYPE=(STRING,32) COUNT=(0:600) DEFAULT=--
PARM PROP_IGNORE TYPE=(STRING,32) COUNT=(0:600) DEFAULT=--
PARM BLTYPES TYPE=(STRING,32) COUNT=0:2 DEFAULT=--
PARM LPFMTMAP TYPE=(STRING,250) COUNT=1 DEFAULT=$V2DATA/balm/lpfmt_mapping.txt
PARM BHFMTMAP TYPE=(STRING,250) COUNT=1 DEFAULT=$V2DATA/balm/bhfmt_mapping.txt
PARM LPK_IGNORE TYPE=(STRING, 128) COUNT=(0:600) DEFAULT=--
PARM BHK_IGNORE TYPE=(STRING, 128) COUNT=(0:600) DEFAULT=--
PARM BINCMP TYPE=KEYWORD COUNT=0:1 VALID=(BINCMP) DEFAULT=--
PARM LPRCMP TYPE=KEYWORD COUNT=0:1 VALID=(LPRCMP) DEFAULT=--
PARM BHFMTFILES TYPE=STRING COUNT=0:2 DEFAULT=--
PARM LPFMTFILES TYPE=STRING COUNT=0:2 DEFAULT=--

PARM RETVAL NAME DEFAULT=dummy

END-PROC
!# annot icon = difpic
.TITLE
DIFPIC version 06Oct11
.HELP
 PURPOSE:

Program DIFPIC is used to find the difference between two images.  By default
DIFPIC only diff the pixels. There are optional arguments to diff VICAR
property labels, binary headers, and line prefix as well.

 EXECUTION:

The input images may have any VICAR data format: BYTE, HALF, FULL, REAL, DOUB,
or COMP.  Both input images must have the same format.  The data format is taken
from the VICAR label of the first input file.  The optional output 
image has the same data format as the input images.  

Both input images must have the same dimensions, or, if they are of different 
sizes, the SIZE parameter must be specified.  If the images are three-dimensional,
they must be of BSQ or BIL file organization;  BIP files are not supported.
.PAGE
When an output file is produced, the output DN (data number) for a given line and 
sample is found by subtracting the DN from the second image from the DN from the 
first image. The resulting data numbers are then checked for being valid for the 
data type (byte, halfword, or fullword) of the image and are adjusted if invalid.  
For byte data, data numbers less than 0 are set to 0 (or are added to 256 if MOD 
is specified), and data numbers greater than 255 are set to 255. 
For halfword data, data numbers less than -32768 are set to -32768, and data numbers 
greater than 32767 are set to 32767. 
For fullword data, data numbers less than -2147483648 are set to -2147483648, and 
data numbers greater than 2147483647 are set to 2147483647. 
For formats REAL, DOUB, and COMP, no checking for floating point overflow or 
underflow is performed by the program.  Thus it is possible for DIFPIC to terminate 
with such a floating point error.  If this happens, the user can either run DIFPIC 
without on output file or divide both input images through by 1.0E5 and run DIFPIC 
on the results.
NOTE that program F2 can also be used to compare images, with somewhat greater
generality and robustness, but that program is not as simple to use and does not
give as much supplementary information as does DIFPIC.
.PAGE
When compare VICAR property labels, the program can optionally ignore specified
properties.
To compare binary headers and line prefixes, the program looks for FMT mapping
file to map a BLTYPE to an FMT file for line prefix or binary header. Using
the FMT file, the program finds a list of fields, which are OBJECTs in the FMT 
file, to be diff-ed for.
.PAGE
TAE COMMAND LINE FORMAT
      The following command line formats show the major allowable forms:
      difpic INP=(a1,a2) OUT=b SIZE=(sl,ss,nl,ns) optional parameters
      difpic INP=(a1,a2) OUT=b  SL=sl SS=ss NL=nl NS=ns optional parameters
      difpic (a1,a2) b (sl,ss,nl,ns) optional parameters
      difpic (a1,a2) b optional parameters
      difpic INP=(a1,a2) SIZE=(sl,ss,nl,ns) optional parameters
      difpic INP=(a1,a2) SL=sl SS=ss NL=nl NS=ns optional parameters
      difpic (a1,a2) (sl,ss,nl,ns) optional parameters
      difpic (a1,a2) optional parameters
      difpic INP=(a1,a2) bincmp=bincmp lprcmp=lprcmp verbose=verbose
      difpic INP=(a1,a2) lblcmp=lblcmp ignore_lbls="PRODUCT_CREATION_TIME" verbose=verbose

       Here 'a1' and 'a2' represent the input image file names, and
       'b' represents the output image file name.
.PAGE
EXAMPLES

1.    difpic INP=(A1,A2) OUT=B 'MOD 

      In this example the difference between the byte images A1 and A2 is
      written to the output file B.  Where the DN for image A1 is greater
      than the DN for image A2, the arithmetic difference is less than 0.
      The output DN is determined by taking the arithmetic difference modulo
      256.

2.    difpic INP=(A1,A2) 

      In this example the number of differences (pixels) between the 
      images A1 and A2 is printed and no output file is produced.
.PAGE
STATISTICS
Statistics on the number of differences are displayed by default.  Fuller 
statistics are produced if an output file is specified.  

Some of the statistics are average values for the differences found by DIFPIC.
These are computed in floating point and may involve numerous additions and 
thus may result in slightly different values on different computers.

The value displayed for AVE VAL OF DIFFS includes all pixels: those with 
positive, negative or zero differences.  This is a floating point value based 
on the real differences between the pixels and not on the value of the output 
pixel, which might be coerced to the numeric limits for the data type of the 
pixel.
.PAGE
 PRECISION: 
  When an output file is specified, DIFPIC computes the average difference
as well as the average positive and negative differences.  These are computed
in a straightforward way, adding up the differences and then dividing by the
number of differences.  (These computations are intended to be fast but not
necessarily highly precise.)  To accommodate the large differences that are
possible in the worst cases, the adding up takes place in single precision
floating point for all data formats except for BYTE and DOUBLE.  If there are a
large number of differences, this can result in a large number of floating
point operations and in some cases can result in a noticeable amount of
round-off error.  The test procedure contains such a case.  Thus the precision
of the average differences is not guaranteed.  In normal use the variation in 
these values that can be expected on different MIPS-supported machines should
not differ by more than 1 in six significant digits.  In contrast, the NUMBER
OF DIFFERENCES is a precise integer value.
.PAGE
RESTRICTIONS
1. The input files must both have the same data format. 
2. The maximum number of bytes per line is 200000.
3. BIP file organization is not supported.

 Ported to UNIX by:      Steve Pohorsky               4 Apr 1992

 COGNIZANT PROGRAMMER:   Lucas Kamp

 REVISIONS:
    12-91  SP   PORTED TO RUN ON BOTH UNIX AND VMS.
     9-92  SP   Made buffer size 200000 bytes. Modified to handle 
                all data formats.  CHANGED AVE VALS TO DISPLAY AS FLOAT.
                CORRECTED "AVE DN OF PIX" TO "AVE VAL OF DIFFS"
     3-93  SP   Modified to not use -2147483648 to work around Sun compiler.
                Added ability to handle 3D files if SIZE field defaulted
                and no output file specified.
     7-94  SP   Allowed format="WORD" as alternative to HALF.
     8-03  lwk  removed restrictions on NB, added SB parameter
    11-03  lwk  added checks on size/format of input files
    09-11  rrp  converted difpic to be a regular subroutine and not
                a VICAR main44 subroutine. Modified the subroutine
		to accept open input file unit numbers (instead of
		opening the input files in the subroutine) and a return
		status to indicate 0 if no differences are found or
		1 if there are differences. Modiffed DPFPIC* subroutines
		to return a status value as indicated above. Added new
		parameters to this pdf for controlling the comparison of
		labels, binary header, line prefix, for ignoring
		label items and binary header fields, and the name of the
		binary header fmt file and line prefix fmt files.
.LEVEL1
.VARIABLE INP
Input file names
.VARIABLE OUT
Output file name (optional)
.VARIABLE RETVAL
The result of difpic as a number
.VARIABLE SIZE
Standard Vicar size field:
  (SL,SS,NL,NS)
.VARIABLE SL
Starting line number
.VARIABLE SS
Starting sample number
.VARIABLE NL
Number of lines
.VARIABLE NS
Number of samples
.VARIABLE BANDS
Standard Vicar Bands field:
  (SB,NB)
.VARIABLE SB
Starting band number
.VARIABLE NB
Number of bands
.VARIABLE MOD
Specifies for byte images that
the difference will be taken
modulo 256.
.VARIABLE VERBOSE
Whether or not output messages 
should be printed.
.VARIABLE ALL
Turn on all comparisons.
.VARIABLE  PIXCMP
Indicates if the image content 
should be compared.
.VARIABLE  LBLCMP
Indicates if the labels are 
to be compared.
.VARIABLE  HSTCMP
Indicates if the history 
labels should be compared.
.VARIABLE LBL_IGNORE
List labels that should be 
ignored when comparing images.
.VARIABLE PROP_IGNORE
List properties that shoule 
be ignored when comparing 
images.
.VARIABLE BHFMTMAP
Input file to specify FMT for 
a given BLTYPE for binary 
header.
.VARIABLE LPFMTMAP
Input file to specify FMT for 
a given BLTYPE for line prefix.
.VARIABLE BLTYPES
Overriding BLTYPES values for 
VICAR label.
.VARIABLE LPK_IGNORE
List of line prefix fields to 
ignore.
.VARIABLE BHK_IGNORE
List of binary header fields 
to ignore.
.VARIABLE BINCMP
Indicates if the binary 
header(s) in the input images 
are to be compared.
.VARIABLE BMFMTFILES
Respective name of FMT files for 
the binary header for the given 
inputs. 
.VARIABLE LPRFCMP
Indicates if the line prefixes 
in the input images are to be 
compared.
.VARIABLE LPFMTFILES
Respective name of FMT files 
for the line prefix for the 
given inputs. 

.LEVEL2
.VARIABLE OUT
If no output file is specified, DIFPIC operates in a faster mode and only
prints the number of different pixels.
.VARIABLE RETVAL
The value of RETVAL will be the sum of all comparisons (label, history
label, binary header, line prefix, and pixel). 
Individually, if a comparison finds no difference, its return value is
0. Otherwise, it's 1 for property label, 2 for binary header, 4 for
history label, 8 for line prefix, and 16 for pixel.
.VARIABLE SIZE
Standard Vicar size field:
  (SL,SS,NL,NS)
You can enter SL,SS,NL,
and NS together as SIZE, OR
enter the SL,SS,NL, and NS
parameters separately.
By default, the entire input
image is used if these
parameters are not entered.
.VARIABLE BANDS
Standard Vicar Bands field:
  (SB,NB)
You can enter SB and NB together
as BANDS, OR enter the SB and NB
parameters separately.
By default, the entire input
image is used if these
.VARIABLE NB
Number of bands: This may be used for multi-band (3D) images.  The default is
the number of bands in the input image label.
.VARIABLE MOD
MOD is an option for byte images that is useful when one wants to see in the 
output file the location of all pixels for which the input DNs are not the 
same.  When MOD is specified for byte images, all pixels with the same input
DNs will have an output DN of 0, and all pixels that do not have the same input
DNs will have an output DN that is not 0.  MOD has no effect except for BYTE 
images.  MOD is recommended for byte images when an output file is produced.

For byte data if the difference computed is less than 0, the output DN is set
to 0 if MOD is not specified, but is set to 256 plus the difference if MOD is
specified.  This amounts to taking the difference modulo 256.
.VARIABLE VERBOSE
Specify if the label diff, binary header diff and line prefix diff should print
the differences found to console (VERBOSE) or not (SILENT).
.VARIABLE LBL_IGNORE
List labels that should be ignored when comparing images. Ignored labels are
treated as if they are don't exists in the image and thus ignored labels with
different values or missing in one image are not considered when determining
if the labels between the given images differ.
.VARIABLE  HSTCMP
Currently, this option is unimplemented, and always return false (different).
.VARIABLE PROP_IGNORE
List properties that shoule be ignored when comparing images. All labels from
these properties are ignored and the images are considered same if all the
labels under the remaining properties are same.
.VARIABLE BHFMTMAP
Mapping of BLTYPE keyword to update-to-date version of binary header FMT
file for every project.
.VARIABLE LPFMTMAP
Mapping of BLTYPE keyword to update-to-date version of line prefix FMT file
for every project.
.VARIABLE LPK_IGNORE
List of line prefix fields to ignore. The name of a field appears in dot
notation. For example, if you have column named 'A', and another column
named 'B' inside 'A', to refer to 'B', you use 'A.B'. If there are more
than one column that can be referred to by the same name, then all of
those columns are ignored.
.VARIABLE BHK_IGNORE
List of binary header fields to ignore. The name of a field appears in dot
notation. For example, if you have column named 'A', and another column
named 'B' inside 'A', to refer to 'B', you use 'A.B'. If there are more
than one column that can be referred to by the same name, then all of
those columns are ignored.

.END
$ Return
$!#############################################################################
$Imake_File:
$ create difpic.imake
#define PROGRAM difpic

#define MODULE_LIST difpic.cc VicarImageUtil.cc VicarImageLabel.cc VicarHistoryLabel.cc DifpicParameters.cc difpic_bridges.cc difpic_pixel.f

#define MAIN_LANG_C_PLUS_PLUS

#define USES_C_PLUS_PLUS
#define CCC_TEMPLATES

#define USES_FORTRAN
#define FTNINC_LIST fortport

#define USES_ANSI_C
#define LIB_RTL
#define LIB_TAE

#define LIB_P2SUB
#define LIB_P1SUB
#define R2LIB
#define LIB_FORTRAN
#define LIB_PDS

/*#define DEBUG     for local build */
/*#define LIB_LOCAL   for local build */
$ Return
$!#############################################################################
$Test_File:
$ create tstdifpic.pdf
procedure
refgbl $echo
body

local cas_iss3_1 string init="/project/test_work/testdata/cassini/casIss/N1488210398_2.IMG"
local cas_iss3_2 string init="/project/test_work/testdata/cassini/casIss/N1629509454_1.IMG"
local cas_iss4 string init="/project/test_work/testdata/cassini/casIss/N1638304759_2.IMG"
local binfmt string init="/project/test_work/testdata/cassini/fmt/cas_tlmtab.fmt"
local lpfmt string init="/project/test_work/testdata/cassini/fmt/cas_prefix2.fmt"
local bhfmtmap string init="/project/test_work/testdata/p2/balm/bhfmt_mapping.txt"
local lpfmtmap_ string init="/project/test_work/testdata/p2/balm/lpfmt_mapping.txt"
local out1 string init=""
local retval int

let _onfail="continue"
let $echo="yes"
!

gen difpica 100 100 'byte
gen difpicb 100 100 ival=5 'byte
difpic (difpica,difpicb) difpicc 'mod
list difpicc (1,1,10,20)
difpic (difpicb,difpica) difpicc 
list difpicc (1,1,10,20)
difpic (difpicb,difpica) size=(2,2,9,9)
!
gen difpica 100 100 'half
gen difpicb 100 100 ival=5 'half
difpic (difpica,difpicb) difpicc
list difpicc (1,1,10,20)
difpic (difpicb,difpica) difpicc 
list difpicc (1,1,10,20)
copy difpica difpicd
difpic (difpica,difpicd) size=(2,2,9,9)
!
gen difpica 10 10 linc=2 sinc=2
gen difpicb 10 10 ival=6
difpic (difpica,difpicb) difpicc size=(2,3,8,6) 'mod
list difpicc
difpic (difpica,difpicb) size=(2,3,8,6) 
!
gen difpica 100 100 'full
gen difpicb 100 100 ival=5 'full
difpic (difpica,difpicb) difpicc
list difpicc (1,1,10,5)
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
list difpicc (1,1,9,5)
copy difpica difpicd
difpic (difpica,difpicd) 
!
gen difpica 100 100 'real4
gen difpicb 100 100 ival=5 'real4
difpic (difpica,difpicb) difpicc
list difpicc (1,1,10,5)
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
list difpicc (1,1,9,5)
copy difpica difpicd
difpic (difpica,difpicd) 
!
!
gen difpica 100 100 'real8
gen difpicb 100 100 ival=5 'real8
difpic (difpica,difpicb) difpicc
list difpicc (1,1,10,5)
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
list difpicc (1,1,9,5)
copy difpica difpicd
difpic (difpica,difpicd) 
!
!
gen difpica 100 100 'comp
gen difpicb 100 100 ival=5 'comp
difpic (difpica,difpicb) difpicc
list difpicc (1,1,10,5)
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
list difpicc (1,1,9,5)
copy difpica difpicd
difpic (difpica,difpicd) 
!
!check for handling of max and min DNs
!
gen difpica 100 100 ival=0 linc=0 sinc=0 'byte
gen difpicb 100 100 ival= 255 linc=0 sinc=0 'byte
difpic (difpica,difpicb) difpicc
list difpicc (1,1,10,5) 'zero
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
list difpicc (1,1,9,5)
!
gen difpica 100 100 ival=-32768 'half
gen difpicb 100 100 ival= 32500 'half
difpic (difpica,difpicb) difpicc
list difpicc (1,1,10,5)
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
list difpicc (1,1,9,5)
!
gen difpica 100 100 ival=-2147483648 'full
gen difpicb 100 100 ival= 2147483000 'full
difpic (difpica,difpicb) difpicc
list difpicc (1,1,10,5)
difpic (difpicb,difpica) difpicc size=(2,2,9,9)
list difpicc (1,1,9,5)
!
write "Test difpic on multiband images"
gen a 10 20 30 ORG="BSQ"
gen b 10 20 30 ORG="BSQ" ival=1
write "Should get 6000 differences."
difpic (a b)
!
gen a 10 20 30 ORG="BIL" 'HALF
gen b 10 20 30 ORG="BIL" 'HALF ival=1
write "Should get 3000 differences."
difpic (a b) NB=15
!
gen a 10 20 30 ORG="BIP" 'REAL
gen b 10 20 30 ORG="BIP" 'REAL ival=1
write "Should get error message about BIP files"
difpic (a b) NB=15
!
write "Test difpic on multiband images with output file"
!
gen difpica 40 40 10 'byte
gen difpicb 40 40 10 ival=5 'byte
difpic (difpica,difpicb) difpicc 'mod
list difpicc (1,1,10,20) nb=2
!
gen difpica 40 40 10 'half
gen difpicb 40 40 10 ival=5 'half
difpic (difpicb,difpica) difpicc 
list difpicc (1,1,10,20) nb=2
!
gen difpica 40 40 10 'full
gen difpicb 40 40 10 ival=5 'full
difpic (difpica,difpicb) difpicc
list difpicc (1,1,10,5) nb=2
!
gen difpica 40 40 10 'real4
gen difpicb 40 40 10 ival=5 'real4
difpic (difpica,difpicb) difpicc nb=4 sb=3
list difpicc (1,1,10,5) nb=2
!
gen difpica 40 40 10 'real8
gen difpicb 40 40 10 ival=5 'real8
difpic (difpica,difpicb) difpicc
list difpicc (1,1,10,5) nb=2
!
gen difpica 40 40 10 'comp
gen difpicb 40 40 10 ival=5 'comp
difpic (difpica,difpicb) difpicc
list difpicc (1,1,10,5) nb=2
!
! test that COMPLEX format treated properly:
gen difpica 40 40 'comp
gen difpicb 40 40 ival=5 'comp
ccomp difpica (cr ci) 'rect 'forw
ccomp (cr ci) difpicb 'rect 'inv   
label-l difpica
label-l difpicb
difpic (difpica,difpicb) difpicc
!
! test for inputs of different sizes (DAR-CORE-P2-difpic Jul.2010):
gen difpica 100 100 
gen difpicb 10 10 
difpic (difpica,difpicb) difpicc 
! should be all zeroes:
list difpicc 
!

!write "Should not run beyond this point."
!return

let $echo="no"
!!!!!
!!!!! Test case for difpic changes as of 09/2011
!!!!!

! diff label only

write "******************************************************"
write "************** Test label diffing ********************"
write "******************************************************"

write
write "***************** Compare same labels ****************"
let $echo="yes"
difpic inp=(&cas_iss3_1,&cas_iss3_1) pixcmp=nopixcmp lblcmp=lblcmp retval=retval
disp retval

!'retval' should equal '0'
let $echo="no"

write
write "***************** Compare only labels **************"
let $echo="yes"
difpic inp=(&cas_iss3_1,&cas_iss3_2) pixcmp=nopixcmp lblcmp=lblcmp retval=retval
disp retval

!'retval' should equal '1'
let $echo="no"

write
write "***************** Compare only labels but don't print any differences **************"
let $echo="yes"
difpic inp=(&cas_iss3_1,&cas_iss3_2) pixcmp=nopixcmp lblcmp=lblcmp verbose=silent retval=retval
disp retval

!'retval' should equal '1'
let $echo="no"

write
write "***************** Let's ignore some labels **************"
let $echo="yes"
difpic inp=(&cas_iss3_1,&cas_iss3_2) pixcmp=nopixcmp lblcmp=lblcmp lbl_ignore=(IMAGE_MID_TIME,IMAGE_TIME) retval=retval
disp retval
!'retval' should equal '1'

let $echo="no"

write
write "***************** Let's ignore some properties **************"
let $echo="yes"
difpic inp=(&cas_iss3_1,&cas_iss3_2) pixcmp=nopixcmp lblcmp=lblcmp prop_ignore=(COMMAND,TELEMETRY) retval=retval
disp retval
!'retval' should equal '1'
let $echo="no"

! diff binary header only

write "******************************************************"
write "********** Test binary header diffing ****************"
write "******************************************************"

write
write "************* Compare same binary header *************"
write "********** while using specified FMT files ***********"
let $echo="yes"
difpic inp=(&cas_iss3_1,&cas_iss3_1) pixcmp=nopixcmp bincmp=bincmp bhfmtfiles=(&binfmt,&binfmt) retval=retval
disp retval
!'retval' should equal '0'
let $echo="no"

write
write "************* Compare only binary header *************"
write "***** while using default BHFMTMAP and BLTYPE ********"
let $echo="yes"
difpic inp=(&cas_iss3_1,&cas_iss3_2) pixcmp=nopixcmp bincmp=bincmp retval=retval
disp retval
!'retval' should equal '2'
let $echo="no"

write
write "************** Let's ignore some fields **************"
write "************* and overwrite BHFMTMAP file ************"
write "All fields named 'EXTENDED_ISS_SCIENCE_HEADER.SPARE' will be ignored; you won't see them listed as differences."
write
let $echo="yes"
difpic inp=(&cas_iss3_1,&cas_iss3_2) pixcmp=nopixcmp bincmp=bincmp bhfmtmap=&bhfmtmap bhk_ignore=(EXTENDED_ISS_SCIENCE_HEADER.SPARE) retval=retval
disp retval
!'retval' should equal '2'
let $echo="no"

write
write "***** Compare binary headers of different BLTYPE *****"
let $echo="yes"
difpic inp=(&cas_iss3_1,&cas_iss4) pixcmp=nopixcmp bincmp=bincmp retval=retval
disp retval
!'retval' should equal '2'
let $echo="no"

write
write "***** Compare binary headers while overriding BLTYPE *****"
let $echo="yes"
difpic inp=(&cas_iss3_1,&cas_iss4) pixcmp=nopixcmp bincmp=bincmp BLTYPES=(CAS-ISS3,CAS-ISS3) retval=retval
disp retval
!'retval' should equal '2'

let $echo="no"

write
write "***** Compare binary headers in SILENT mode *****"
let $echo="yes"
difpic inp=(&cas_iss3_1,&cas_iss3_2) pixcmp=nopixcmp bincmp=bincmp verbose=silent retval=retval
disp retval
!'retval' should equal '2'
let $echo="no"

! diff line prefix only

write "******************************************************"
write "************ Test line prefix diffing ****************"
write "******************************************************"

write
write "************* Compare same line prefix ***************"
let $echo="yes"
difpic inp=(&cas_iss3_1,&cas_iss3_1) pixcmp=nopixcmp lprcmp=lprcmp lpfmtfiles=(&lpfmt,&lpfmt) retval=retval
disp retval
!'retval' should equal '0'
let $echo="no"

write
write "************* Compare only line prefix ***************"
write "******** while using default LPFMTMAP and BLTYPE *****"
let $echo="yes"
difpic inp=(&cas_iss3_1,&cas_iss3_2) pixcmp=nopixcmp lprcmp=lprcmp retval=retval
disp retval
!'retval' should equal '8'
let $echo="no"

write
write "************** Let's ignore some fields **************"
write "************* while overriding LPFMTMAP **************"
write "Field 'EXTENDED_PIXEL' will be ignored; you won't see the field listed as a difference."
write
let $echo="yes"
difpic inp=(&cas_iss3_1,&cas_iss4) pixcmp=nopixcmp lprcmp=lprcmp lpfmtmap=(&lpfmtmap_) lpk_ignore=(EXTENDED_PIXEL) BLTYPES=(CAS-ISS3,CAS-ISS3) retval=retval
disp retval
!'retval' should equal '8'
let $echo="no"

write
write "****** Compare line prefix of different BLTYPE ******"
let $echo="yes"
difpic inp=(&cas_iss3_1,&cas_iss4) pixcmp=nopixcmp lprcmp=lprcmp retval=retval
disp retval
!'retval' should equal '8'
let $echo="no"

write
write "***** Compare line prefix while overriding BLTYPE *****"
let $echo="yes"
difpic inp=(&cas_iss3_1,&cas_iss4) pixcmp=nopixcmp lprcmp=lprcmp BLTYPES=(CAS-ISS3,CAS-ISS3) retval=retval
disp retval
!'retval' should equal '8'
let $echo="no"

write
write "******** Compare line prefix in SILENT mode **********"
let $echo="yes"
difpic inp=(&cas_iss3_1,&cas_iss3_2) pixcmp=nopixcmp lprcmp=lprcmp verbose=silent retval=retval
disp retval
!'retval' should equal '8'
let $echo="no"

write
write "*************** Compare everything *******************"
let $echo="yes"
difpic inp=(&cas_iss3_1,&cas_iss3_2) 'ALL retval=retval
disp retval

!'retval' should equal '31'
let $echo="no"

end-proc
$ Return
$!#############################################################################
