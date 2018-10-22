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


