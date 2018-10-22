// ads_MissionDataFinder.h
//
//  April 9, 2002
//  Michael Brady

#if !defined ADS_DATA_FINDER_H_INCLUDED
#define ADS_DATA_FINDER_H_INCLUDED

#include <memory>

#include "jpl_mipl_spice_corba.C.h"

/**
 *  This class encapsulates the mission-specific data finding process.
 */
class ads_MissionDataFinder
{
public:
   
   virtual ~ads_MissionDataFinder() = 0;
   
   /** Gets geometry without looking at updates. */
   virtual void getGeometryNoUpdates (
          int instNaifId,
          int targetNaifId,
          const char* utcTime,
          const char* referenceFrame,
          double tolerance,
	       jpl::mipl::spice::corba::Geometry_out theGeometry,
	       jpl::mipl::spice::corba::GeometryMetaData_out metaData
          )
      throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException,
              jpl::mipl::spice::corba::MiplSpiceLib::PointingNotFound ) = 0;
   
   /** Gets a geometry which matches the specified criteria. */
   virtual void getGeometry (
         const jpl::mipl::spice::corba::ImageData & image,
         const jpl::mipl::spice::corba::GeometryMetaData & criteria,
         jpl::mipl::spice::corba::Geometry_out theGeometry,
         jpl::mipl::spice::corba::GeometryMetaData_out metaData
         )
      throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException,
              jpl::mipl::spice::corba::MiplSpiceLib::PointingNotFound ) = 0;

   virtual void storeGeometry (
      const jpl::mipl::spice::corba::ImageData & image,
      const jpl::mipl::spice::corba::Geometry & theGeometry,
      const jpl::mipl::spice::corba::GeometryMetaData & theMetaData
      )
      throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) = 0;
          
    virtual void getInstrument (
              const jpl::mipl::spice::corba::ImageData & image,
              jpl::mipl::spice::corba::InstrumentData_out theInstrument
            )
            throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) = 0;
          
    virtual void getTarget (
              const jpl::mipl::spice::corba::ImageData & image,
              jpl::mipl::spice::corba::Target_out theTarget
            )
            throw ( jpl::mipl::spice::corba::SpiceLib::ToolkitException ) = 0;


   static
   ads_MissionDataFinder* 
   createDataFinder(jpl::mipl::spice::corba::Instrument inst);


};


#endif // !defined ADS_DATA_FINDER_H_INCLUDED
