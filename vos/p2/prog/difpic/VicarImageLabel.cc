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
