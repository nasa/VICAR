#include <math.h>
#include <stdio.h>
#include <vector>
#include <map>

extern "C"
{
#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "zvproto.h"
#include "zmabend.h"
#include "zifmessage.h"
}

using namespace std;

struct point {
  double x;
  double y;
  int clust;
  point() : x(0), y(0), clust(0) {}
  point(double _x, double _y) {
    x = _x;
    y = _y;
    clust = 0;
  }
};

bool operator<(const point &a, const point &b) {
  return a.x < b.x || (a.x == b.x && a.y < b.y);
}

static float rss(float x, float y) {
  return sqrt(x * x + y * y);
}

void main44(void) {
  int status, i_unit, clen, ibis, row, cnt, def;
  char msgBuf[1000], fmtstring[10];
  double x, y;
  double dist;
  int xCol, yCol, clustCol;
  int nextClust = 1;

  zvmessage("ibisclst2 version 2016-06-16"," ");
  
  if ((zvparmd("distance", &dist, &cnt, &def, 1, 0)) != 1)
    zmabend("error parsing dist");

  if ((zvparmd("xcol", &xCol, &cnt, &def, 1, 0)) != 1)
    zmabend("error parsing xcol");

  if ((zvparmd("ycol", &yCol, &cnt, &def, 1, 0)) != 1)
    zmabend("error parsing ycol");

  if ((zvparmd("clustcol", &clustCol, &cnt, &def, 1, 0)) != 1)
    zmabend("error parsing clstcol");

  if (dist <= 0.0)
    zmabend("dist must be > 0.0");

  if ((status = zvunit(&i_unit,"inp",1, NULL)) != 1)
    zmabend("error calling zvunit");
  if ((status = IBISFileOpen(i_unit, &ibis, "UPDATE", 0, 0, 0, 0)) != 1)
    IBISSignalU(i_unit, status, 1);
  IBISFileGet(ibis, "nr", &clen, 1, 1, 0);

  sprintf(msgBuf, "%d rows input", clen);
  zifmessage(msgBuf);

  if ((status = IBISColumnGet(ibis, "FORMAT", fmtstring, xCol)) != 1)
    zmabend("error getting xcol column format");

  if (fmtstring[0] != 'D') {
    sprintf(msgBuf, "xCol fmt %s should be DOUB", fmtstring);
    zmabend(msgBuf);
  }

  if ((status = IBISColumnGet(ibis, "FORMAT", fmtstring, yCol)) != 1)
    zmabend("error getting yCol column format");

  if (fmtstring[0] != 'D') {
    sprintf(msgBuf, "yCol fmt %s should be DOUB", fmtstring);
    zmabend(msgBuf);
  }

  if ((status = IBISColumnGet(ibis, "FORMAT", fmtstring, clustCol)) != 1)
    zmabend("error getting clustCol column format");

  if (fmtstring[0] != 'D') {
    sprintf(msgBuf, "clustCol fmt %s should be DOUB", fmtstring);
    zmabend(msgBuf);
  }

  vector<point> points(clen);

  for (row = 0; row < clen; ++row) {
    if ((status = IBISColumnRead(ibis, (char*) &x, xCol, row + 1, 1)) != 1)
      IBISSignal(ibis, status, 1);
    if ((status = IBISColumnRead(ibis, (char*) &y, yCol, row + 1, 1)) != 1)
      IBISSignal(ibis, status, 1);
    points[row] = point(x, y);
  }

  map<point,vector<point>*> clusterMap;
  for (int i = 0; i < clen - 1; ++i) {
    map<point,vector<point>*>::iterator iCluster;
    iCluster = clusterMap.find(points[i]);
    for (int j = i + 1; j < clen; ++j) {
      if (rss(points[i].x - points[j].x, points[i].y - points[j].y) < dist) {
	if (iCluster == clusterMap.end()) {
	  // i is not in a cluster yet
	  map<point,vector<point>*>::iterator jCluster;
	  jCluster = clusterMap.find(points[j]);
	  if (jCluster == clusterMap.end()) {
	    // j is not in a cluster, either; make a new cluster for them
	    vector<point> *clust = new vector<point>;
	    clust->push_back(points[i]);
	    clust->push_back(points[j]);
	    clusterMap.insert(pair<point,vector<point>*>(points[i], clust));
	    clusterMap.insert(pair<point,vector<point>*>(points[j], clust));
	    points[i].clust = nextClust;
	    points[j].clust = nextClust;
	    nextClust++;
	  } else {
	    // j already in a cluster, add i to it
	    jCluster->second->push_back(points[i]);
	    clusterMap.insert(pair<point,vector<point>*>(points[i], jCluster->second));
	    points[i].clust = points[j].clust;
	  }
	  iCluster = clusterMap.find(points[i]);
	} else {
	  // i already in a cluster, check for j
	  map<point,vector<point>*>::iterator jCluster;
	  jCluster = clusterMap.find(points[j]);
	  if (jCluster == clusterMap.end()) {
	    iCluster->second->push_back(points[j]);
	    clusterMap.insert(pair<point,vector<point>*>(points[j], iCluster->second));
	    points[j].clust = points[i].clust;
	  }
	}
      }
    }
    // i is not near any other, so give it its own cluster
    if (! points[i].clust) {
      points[i].clust = nextClust;
      nextClust++;
    }
  }

  double *clusterDat = new double[clen];

  for (int i = 0; i < clen; ++i)
    clusterDat[i] = points[i].clust;

  if ((status = IBISColumnWrite(ibis, (char*) clusterDat, clustCol, 1, clen)) != 1)
    IBISSignal(ibis,status,1);

  delete[] clusterDat;

  if ((status = IBISFileClose(ibis, ICLOSE_UKEEP) != 1))
    IBISSignal(ibis,status,1);

  sprintf(msgBuf, "%d clusters found", nextClust - 1);
  zifmessage(msgBuf);
}
