#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <map>
#include <assert.h>

extern "C"
{
#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "zvproto.h"
#include "zifmessage.h"
#include "zmabend.h"
}

using namespace std;

struct point {
  double x;
  double y;
  double lon;
  double lat;
  int clust;
  point() : x(0), y(0), lon(0), lat(0), clust(0) {}
  point(double _x, double _y) {
    x = _x;
    y = _y;
    lon = 0;
    lat = 0;
    clust = 0;
  }
  point(double _x, double _y, double _lon, double _lat) {
    x = _x;
    y = _y;
    lon = _lon;
    lat = _lat;
    clust = 0;
  }
};

bool operator<(const point &a, const point &b) {
  return a.x < b.x || (a.x == b.x && a.y < b.y);
}

void main44(void) {
  int status, i_unit, clen, ibis, row, cnt, def;
  char msgBuf[1000], fmtstring[10];
  int dline, dsamp;
  int sampCol, lineCol, clustCol;
  int sampCtrCol, lineCtrCol;
  int lonCol, latCol, lonCtrCol, latCtrCol;
  int nextClust = 1;

  zvmessage("ibisclst3 version 2016-09-14"," ");
  
  if ((zvparm("dline", &dline, &cnt, &def, 1, 0)) != 1)
    zmabend("error parsing dline");
  sprintf(msgBuf, "dline %d", dline);
  zifmessage(msgBuf);

  if ((zvparm("dsamp", &dsamp, &cnt, &def, 1, 0)) != 1)
    zmabend("error parsing dsamp");
  sprintf(msgBuf, "dsamp %d", dsamp);
  zifmessage(msgBuf);

  if (dline <= 0 && dsamp <= 0)
    zmabend("either dline or dsamp must be > 0");

  if ((zvparmd("sampcol", &sampCol, &cnt, &def, 1, 0)) != 1)
    zmabend("error parsing sampcol");
  sprintf(msgBuf, "sampcol %d", sampCol);
  zifmessage(msgBuf);

  if ((zvparmd("linecol", &lineCol, &cnt, &def, 1, 0)) != 1)
    zmabend("error parsing linecol");
  sprintf(msgBuf, "linecol %d", lineCol);
  zifmessage(msgBuf);

  if ((zvparmd("clustcol", &clustCol, &cnt, &def, 1, 0)) != 1)
    zmabend("error parsing clstcol");
  sprintf(msgBuf, "clustcol %d", clustCol);
  zifmessage(msgBuf);

  if ((zvparmd("sampctr", &sampCtrCol, &cnt, &def, 1, 0)) != 1)
    zmabend("error parsing sampctr");
  sprintf(msgBuf, "sampctr %d", sampCtrCol);
  zifmessage(msgBuf);

  if ((zvparmd("linectr", &lineCtrCol, &cnt, &def, 1, 0)) != 1)
    zmabend("error parsing linectr");
  sprintf(msgBuf, "linectr %d", lineCtrCol);
  zifmessage(msgBuf);

  if ((zvparmd("loncol", &lonCol, &cnt, &def, 1, 0)) != 1)
    zmabend("error parsing loncol");
  sprintf(msgBuf, "loncol %d", lonCol);
  zifmessage(msgBuf);

  if ((zvparmd("latcol", &latCol, &cnt, &def, 1, 0)) != 1)
    zmabend("error parsing latcol");
  sprintf(msgBuf, "latcol %d", latCol);
  zifmessage(msgBuf);

  if ((zvparmd("lonctr", &lonCtrCol, &cnt, &def, 1, 0)) != 1)
    zmabend("error parsing lonctr");
  sprintf(msgBuf, "lonctr %d", lonCtrCol);
  zifmessage(msgBuf);

  if ((zvparmd("latctr", &latCtrCol, &cnt, &def, 1, 0)) != 1)
    zmabend("error parsing latctr");
  sprintf(msgBuf, "latctr %d", latCtrCol);
  zifmessage(msgBuf);

  if ((status = zvunit(&i_unit,"inp",1, NULL)) != 1)
    zmabend("error calling zvunit");
  if ((status = IBISFileOpen(i_unit, &ibis, "UPDATE", 0, 0, 0, 0)) != 1)
    IBISSignalU(i_unit, status, 1);
  IBISFileGet(ibis, "nr", &clen, 1, 1, 0);

  sprintf(msgBuf, "%d points input", clen);
  zifmessage(msgBuf);

  if ((status = IBISColumnGet(ibis, "FORMAT", fmtstring, sampCol)) != 1)
    zmabend("error getting sampcol column format");

  if (fmtstring[0] != 'D') {
    sprintf(msgBuf, "sampcol fmt %s should be DOUB", fmtstring);
    zmabend(msgBuf);
  }

  if ((status = IBISColumnGet(ibis, "FORMAT", fmtstring, lineCol)) != 1)
    zmabend("error getting linecol column format");

  if (fmtstring[0] != 'D') {
    sprintf(msgBuf, "linecol fmt %s should be DOUB", fmtstring);
    zmabend(msgBuf);
  }

  if ((status = IBISColumnGet(ibis, "FORMAT", fmtstring, clustCol)) != 1)
    zmabend("error getting clustCol column format");

  if (fmtstring[0] != 'D') {
    sprintf(msgBuf, "clustCol fmt %s should be DOUB", fmtstring);
    zmabend(msgBuf);
  }

  // check samp/line centroid column format, if provided
  if (sampCtrCol > 0 && lineCtrCol > 0) {
    if ((status = IBISColumnGet(ibis, "FORMAT", fmtstring, sampCtrCol)) != 1)
      zmabend("error getting sampCtrCol column format");

    if (fmtstring[0] != 'D') {
      sprintf(msgBuf, "sampCtrCol fmt %s should be DOUB", fmtstring);
      zmabend(msgBuf);
    }

    if ((status = IBISColumnGet(ibis, "FORMAT", fmtstring, lineCtrCol)) != 1)
      zmabend("error getting lineCtrCol column format");

    if (fmtstring[0] != 'D') {
      sprintf(msgBuf, "lineCtrCol fmt %s should be DOUB", fmtstring);
      zmabend(msgBuf);
    }
  }

  // check lat/lon centroid column format, if provided
  if (lonCol > 0 && latCol > 0 && lonCtrCol > 0 && latCtrCol > 0) {
    if ((status = IBISColumnGet(ibis, "FORMAT", fmtstring, lonCol)) != 1)
      zmabend("error getting lonCol column format");

    if (fmtstring[0] != 'D') {
      sprintf(msgBuf, "lonCol fmt %s should be DOUB", fmtstring);
      zmabend(msgBuf);
    }

    if ((status = IBISColumnGet(ibis, "FORMAT", fmtstring, latCol)) != 1)
      zmabend("error getting latCol column format");

    if (fmtstring[0] != 'D') {
      sprintf(msgBuf, "latCol fmt %s should be DOUB", fmtstring);
      zmabend(msgBuf);
    }

    if ((status = IBISColumnGet(ibis, "FORMAT", fmtstring, lonCtrCol)) != 1)
      zmabend("error getting lonCtrCol column format");

    if (fmtstring[0] != 'D') {
      sprintf(msgBuf, "lonCtrCol fmt %s should be DOUB", fmtstring);
      zmabend(msgBuf);
    }

    if ((status = IBISColumnGet(ibis, "FORMAT", fmtstring, latCtrCol)) != 1)
      zmabend("error getting latCtrCol column format");

    if (fmtstring[0] != 'D') {
      sprintf(msgBuf, "latCtrCol fmt %s should be DOUB", fmtstring);
      zmabend(msgBuf);
    }
  }

  vector<point> points(clen);

  double x, y, lon, lat;

  if (lonCol > 0 && latCol > 0 && lonCtrCol > 0 && latCtrCol > 0) {
    for (row = 0; row < clen; ++row) {
      if ((status = IBISColumnRead(ibis, (char*) &x, sampCol, row + 1, 1)) != 1)
	IBISSignal(ibis, status, 1);
      if ((status = IBISColumnRead(ibis, (char*) &y, lineCol, row + 1, 1)) != 1)
	IBISSignal(ibis, status, 1);
      if ((status = IBISColumnRead(ibis, (char*) &lon, lonCol, row + 1, 1)) != 1)
	IBISSignal(ibis, status, 1);
      if ((status = IBISColumnRead(ibis, (char*) &lat, latCol, row + 1, 1)) != 1)
	IBISSignal(ibis, status, 1);
      points[row] = point(x, y, lon, lat);
    }
  } else {
    for (row = 0; row < clen; ++row) {
      if ((status = IBISColumnRead(ibis, (char*) &x, sampCol, row + 1, 1)) != 1)
	IBISSignal(ibis, status, 1);
      if ((status = IBISColumnRead(ibis, (char*) &y, lineCol, row + 1, 1)) != 1)
	IBISSignal(ibis, status, 1);
      points[row] = point(x, y);
    }
  }

  map<point,vector<point>*> clusterMap;
  for (int i = 0; i < clen; ++i) {
    map<point,vector<point>*>::iterator iCluster;
    iCluster = clusterMap.find(points[i]);
    if (iCluster == clusterMap.end()) { // i is not in a cluster yet, so make one for it
      vector<point> *clust = new vector<point>;
      clust->push_back(points[i]);
      clusterMap.insert(pair<point,vector<point>*>(points[i], clust));
      points[i].clust = nextClust;
      nextClust++;

      // check for close neighbors
      for (int j = i + 1; j < clen; ++j) {
	if (fabs(points[i].x - points[j].x) <= dsamp &&
	    fabs(points[i].y - points[j].y) <= dline) {
	  map<point,vector<point>*>::iterator jCluster;
	  jCluster = clusterMap.find(points[j]);
	  if (jCluster == clusterMap.end()) { // j not in a cluster yet, so add it
	    clust->push_back(points[j]);
	    clusterMap.insert(pair<point,vector<point>*>(points[j], clust));
	    points[j].clust = points[i].clust;
	  }
	}
      }
    }
  }

  int clustCount = nextClust - 1;

#define MAXCLUSTCNT 10000
  if (clustCount > MAXCLUSTCNT)
    zmabend("clustCount > MAXCLUSTCNT");

  // save cluster ids
  double *clusterDat = new double[clen];

  for (int i = 0; i < clen; ++i)
    clusterDat[i] = points[i].clust;

  if ((status = IBISColumnWrite(ibis, (char*) clusterDat, clustCol, 1, clen)) != 1)
    IBISSignal(ibis,status,1);

  delete[] clusterDat;

  // generate/save cluster line/samp centroids if requested
  if (sampCtrCol > 0 && lineCtrCol > 0) {
    int pointCount[MAXCLUSTCNT]; // dynamic allocation here fails on delete[]
    double lineSum[MAXCLUSTCNT];
    double sampSum[MAXCLUSTCNT];

    for (int i = 0; i < clustCount; ++i) {
      pointCount[i] = 0;
      lineSum[i] = 0.0;
      sampSum[i] = 0.0;
    }
    
    for (int i = 0; i < clen; ++i) {
      assert(points[i].clust - 1 < clustCount);
      pointCount[points[i].clust - 1] ++;
      lineSum[points[i].clust - 1] += points[i].y;
      sampSum[points[i].clust - 1] += points[i].x;
    }

    for (int i = 0; i < clustCount; ++i) {
      lineSum[i] /= pointCount[i];
      sampSum[i] /= pointCount[i];
    }

    double *lsAvg = new double[clen];

    for (int i = 0; i < clen; ++i)
      lsAvg[i] = lineSum[points[i].clust - 1];

    if ((status = IBISColumnWrite(ibis, (char*) lsAvg, lineCtrCol, 1, clen)) != 1)
      IBISSignal(ibis,status,1);

    for (int i = 0; i < clen; ++i)
      lsAvg[i] = sampSum[points[i].clust - 1];

    if ((status = IBISColumnWrite(ibis, (char*) lsAvg, sampCtrCol, 1, clen)) != 1)
      IBISSignal(ibis,status,1);

    delete[] lsAvg;
  }

  if (lonCol > 0 && latCol > 0 && lonCtrCol > 0 && latCtrCol > 0) {
    int pointCount[MAXCLUSTCNT];
    double latSum[MAXCLUSTCNT];
    double lonSum[MAXCLUSTCNT];

    for (int i = 0; i < clustCount; ++i) {
      pointCount[i] = 0;
      latSum[i] = 0.0;
      lonSum[i] = 0.0;
    }
    
    for (int i = 0; i < clen; ++i) {
      pointCount[points[i].clust - 1] ++;
      latSum[points[i].clust - 1] += points[i].lat;
      lonSum[points[i].clust - 1] += points[i].lon;
    }

    for (int i = 0; i < clustCount; ++i) {
      latSum[i] /= pointCount[i];
      lonSum[i] /= pointCount[i];
    }

    double *doubleDat = new double[clen];

    for (int i = 0; i < clen; ++i)
      doubleDat[i] = latSum[points[i].clust - 1];

    if ((status = IBISColumnWrite(ibis, (char*) doubleDat, latCtrCol, 1, clen)) != 1)
      IBISSignal(ibis,status,1);

    for (int i = 0; i < clen; ++i)
      doubleDat[i] = lonSum[points[i].clust - 1];

    if ((status = IBISColumnWrite(ibis, (char*) doubleDat, lonCtrCol, 1, clen)) != 1)
      IBISSignal(ibis,status,1);

    delete[] doubleDat;
  }

  if ((status = IBISFileClose(ibis, ICLOSE_UKEEP) != 1))
    IBISSignal(ibis,status,1);

  sprintf(msgBuf, "%d clusters found", clustCount);
  zifmessage(msgBuf);
}
