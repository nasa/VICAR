************************************************************************
*
* Define dependencies in the VICAR system
*
* The ultimate intent is to create a minimum vicset1.csh for a given build,
* containing just those symbols needed for that build.
*
* There are three levels of symbols here:
*
* LEVEL 1:  Projects  PROJ_*
*
* These are Projects, defining which system to build at the highest of
* levels.  Only one Project should be defined.  These are used in
* vicset1_projects.h (just to keep things cleaner).  Based on the project,
* that file  defines the products, as well as individual subsystems
* and externals, that are needed.  Note that it really "should" define
* just projects, letting dependencies pick the necessary externals, but
* this is not always practical.
*
* LEVEL 2:  Products  PROD_*
*
* These are Products, which are things that an end use might want to
* actually use.  They are not the same as directories or packages (although
* there should be some relationship), nor does every scrap of code need to
* be covered by a Product.  A product can be a library if that library is
* independently usable, or is used in multiple products, but mostly "product"
* means an application or family of applications.
*
* One or more Products can be specified independently, without a Project
* being defined, although that is an unusual usage.
*
* LEVEL 3:  Subsystems and Externals  SS_*  EXT_*  JEXT_*
*
* These are the lowest-level symbols, and are the ONLY symbols that
* vicset1.source uses.  Ultimately, all Projects and Products must resolve
* to the subsystems and externals actually in use.
*
************************************************************************
************************************************************************
*
* Non-Java Dependencies
*
* These are all derived manually... no specific dependency tool.
*
* Pieces of P2 depend on most of whats in external.  However, these are
* not listed.  Until such point as P2 is split into more manageable pieces,
* projects will need to define most of the non-Java externals needed by the
* p2 programs they need, themselves.
*
* Most of the PROD_* items here are either potentially separable pieces of
* a larger subsystem, or aggregations of several subsystems, intended to
* make specifying projects easier.
************************************************************************

#ifdef PROD_MARS_SURFACE
#define SS_CRUMBS
#define SS_MARS
#define PROD_PIG
#define PROD_VICAR_EXEC
#define SS_P1SUB
#define SS_P2SUB
#define EXT_MPI
#define EXT_XERCES_CXX
#define EXT_INVENTOR
#endif

#ifdef PROD_PIG
#define PROD_VICAR_EXEC
#define SS_P2SUB
#define EXT_XERCES_CXX
#define EXT_MERIDD
#endif

#ifdef PROD_XVD
#define PROD_VICAR_EXEC
#define SS_GUI
#define SS_V2DATA
#define SS_MOTIFAPP
#define SS_P1SUB
#define EXT_PDS
#endif

#ifdef SS_CRUMBS
#define EXT_INVENTOR
#define EXT_OPENSCENEGRAPH
#endif

* Not really sure about the MDMS ones...

#ifdef PROD_FEI3
#define SS_MDMS
#define EXT_SYBASE
#define SS_V2CONFIG
#define SS_FEI
#define EXT_KERBEROS5
#define EXT_ROGUEWAVE
#define EXT_ACE_TAO
#endif

#ifdef PROD_FEI5
#define SS_MDMS
#define SS_FEI5
#define PROD_FEI5_Client
#define PROD_FEI5_Server
#endif

#if defined(PROD_DBQ) || defined(PROD_DBVIEW) || defined(PROD_DBI)
#define SS_MDMS
#define EXT_SYBASE
#define EXT_ROGUEWAVE	/* assumption... */
#define EXT_ACE_TAO
#define EXT_KERBEROS4
#define EXT_KERBEROS5
#endif

#if defined(PROD_PWDSERVER) || defined(PROD_PWDSERVER_CLIENT)
#define SS_PWDSERVER
#define SS_MDMS
#define EXT_SYBASE
#define EXT_ROGUEWAVE	/* assumption... */
#endif

****

#ifdef SS_P1PROG
#define SS_P1SUB
#endif

#ifdef SS_P2PROG
#define SS_P2SUB
#define EXT_GNUPLOT
#define EXT_TIFF
#define EXT_FFTW
#endif

#ifdef SS_P3PROG
#define SS_P3SUB
#define EXT_TIFF
#endif

#ifdef PROD_VICAR_EXEC
#define SS_RTL
#define SS_SHVIC
#define SS_STAE
#endif

#ifdef SS_VIDS
#define SS_VRDI
#endif

* This is backwards, SS should not define PROD, but it works...
#ifdef SS_TLM
#define PROD_DBI
#define EXT_CALLABLE_JDK
#endif

************************************************************************
*
* Java Dependencies
*
* Developed using the dependency tool "jarjar", then converted by hand
* to this format.
*
* java -jar /home/rgd/work/cm/vicset1/jarjar/jarjar-1.0rc7.jar find jar $V2TOP/html/jars/\* > depend.txt
*
************************************************************************

********************
* Products first
********************

***
* GUI
***

#ifdef PROD_MICA
#define SS_JAVA
#define PROD_JadeDisplay
#define PROD_JavaVicarIO
#define JEXT_XERCES
#define EXT_JNI
#endif

#ifdef PROD_JEDI
#define SS_JAVA
#define PROD_TLM_JavaCore
#define PROD_JavaVicarIO
#define JEXT_EJB
#endif

#ifdef PROD_MIS
#define PROD_Marsviewer
#define PROD_JUNEBERRY
#define JEXT_WEBSOCKET
#endif

#ifdef PROD_FFW
#define SS_JAVA
#define PROD_Marsviewer
#define PROD_JUNEBERRY
#endif



#ifdef PROD_Marsviewer
#define SS_JAVA
#define PROD_JavaVicarIO
#define PROD_JadeDisplay
#define PROD_JADIS
#define JEXT_COMMON_LOGGING	/* PHX only */
#define JEXT_LOG4J		/* PHX only */
#define JEXT_HTTPCORE		/* w10n stuff here and below */
#define JEXT_HTTPCLIENT
#define JEXT_COMMON_CODEC
#define JEXT_JSON_SIMPLE
#define JEXT_JMONKEYENGINE
#endif


#ifdef PROD_PLACES
#define SS_JAVA
#define PROD_JavaVicarIO
#define JEXT_JAXB
#define JEXT_LOG4J
#define JEXT_COMMON_CODEC
#define JEXT_XERCES
#define JEXT_COMMON_LOGGING
#define JEXT_COMMON_COLLECTIONS
#define JEXT_HIBERNATE
#define JEXT_C3P0
#define JEXT_DOM4J
#define JEXT_MYSQL
#define JEXT_RESTLET
#define JEXT_FREEMARKER
#define JEXT_RSA
#define JEXT_JAVAMAIL
#define JEXT_JAI
#define JEXT_JAI_IIO
#define JEXT_JSON_SIMPLE
#endif

#ifdef PROD_PLACES_CORE
#define SS_JAVA
#define PROD_JavaVicarIO
#define JEXT_JAXB
#define JEXT_LOG4J
#define JEXT_COMMON_CODEC
#define JEXT_XERCES
#define JEXT_COMMON_LOGGING
#define JEXT_COMMON_COLLECTIONS
#define JEXT_HIBERNATE
#define JEXT_C3P0
#define JEXT_DOM4J
#define JEXT_MYSQL
#define JEXT_RESTLET
#define JEXT_FREEMARKER
#define JEXT_RSA
#define JEXT_JAVAMAIL
#define JEXT_JAI
#define JEXT_JAI_IIO
#endif

#ifdef PROD_PLACES_MER
#define PROD_PLACES_CORE
#define SS_JAVA
#define JEXT_JAXB
#define JEXT_LOG4J
#define JEXT_COMMON_CODEC
#define JEXT_XERCES
#define JEXT_COMMON_LOGGING
#define JEXT_COMMON_COLLECTIONS
#define JEXT_HIBERNATE
#define JEXT_C3P0
#define JEXT_DOM4J
#define JEXT_MYSQL
#define JEXT_RESTLET
#define JEXT_FREEMARKER
#define JEXT_RSA
#define JEXT_JAVAMAIL
#endif


#ifdef PROD_POSSUM
#define SS_JAVA
#define PROD_JavaVicarIO
#define PROD_JadeDisplay
#define JEXT_COMMON_LOGGING	/* PHX only */
#define JEXT_LOG4J		/* PHX only */
#endif

#ifdef PROD_SITH
#define SS_JAVA
#define PROD_JadeDisplay
#define PROD_JADIS
#define PROD_JavaVicarIO
#endif

#ifdef PROD_JADIS
#define SS_JAVA
#define JEXT_JOGL
#endif

#ifdef PROD_WEBIFICATION
#define PROD_JUNEBERRY
#define PROD_COTINGA
#define PROD_PASSEPARTOUT
#define PROD_JEEVES
#define EXT_JS	/* placeholder until javascript product is named */
#define SS_JS	/* placeholder until javascript product is named */
#endif

#ifdef PROD_JUNEBERRY
#define SS_JAVA
#define PROD_JavaVicarIO
#define JEXT_JSON_SIMPLE
#define JEXT_TREEVOTEE
#define	JEXT_JUNAMI
#define JEXT_LOG4J
#define JEXT_SLF4J
#define JEXT_EHCACHE
#define JEXT_IVOAFITS
#define JEXT_COMMON_CODEC
#define JEXT_COMMONS_EXEC
#define JEXT_URL_REWRITE
#define JEXT_RESTLET
#endif

#ifdef PROD_COTINGA
#define SS_JAVA
#define JEXT_COMMON_CODEC
#define JEXT_COMMON_LOGGING
#define JEXT_HTTPCLIENT
#define JEXT_HTTPCORE
#define JEXT_SON_SIMPLE
#define JEXT_LUCENE
#define JEXT_TREEVOTEE
#define JEXT_RESTLET
#endif

#ifdef PROD_JEEVES
#define JEXT_SXN
#endif

#ifdef PROD_PASSEPARTOUT
#define PYEXT_VIRTUALENV
#define SS_PY
#endif


***
* Telemetry and MATIS
***

#ifdef PROD_QVIEWER
#define SS_JAVA
#define PROD_CDRS
#endif

#ifdef PROD_CDRS
#define SS_JAVA
#define PROD_MATIS_CORE
#define PROD_FEI5_Client
#define PROD_JavaVicarIO
#endif

#ifdef PROD_MATIS_PHX
#define SS_JAVA
#define PROD_MATIS_CORE
#endif

* The MPCS consumer ones should be removed from MER and NSYT once the
* jpl.mipl.msl package is removed from the build.

#ifdef PROD_MATIS_MER
#define SS_JAVA
#define PROD_MATIS_CORE
#endif

#ifdef PROD_MATIS_NSYT
#define SS_JAVA
#define PROD_MATIS_CORE
#endif

#ifdef PROD_MATIS_MSL
#define SS_JAVA
#define PROD_MATIS_CORE
#define JEXT_ACTIVEMQ			/* mpcs consumer only */
#define	JEXT_COMMON_CONFIGURATION	/* mpcs consumer only */
#endif

#ifdef PROD_MATIS_Diviner
#define SS_JAVA
#define PROD_MATIS_CORE
#define PROD_Diviner
#define JEXT_ACTIVEMQ
#define EXT_JNI
#endif

#ifdef PROD_MATIS_CORE
#define SS_JAVA
#define PROD_FEI5_Client
#define PROD_TLM_JavaCore
#define JEXT_LOG4J
#define JEXT_HIBERNATE
#define JEXT_JBPM
#define JEXT_COMMON_LOGGING
#define JEXT_XERCES
#define JEXT_JSCH
#define PROD_JavaVicarIO
#define JEXT_XAMPLE
#define JEXT_COMMONS_IO
#define JEXT_ACTIVITI
#define JEXT_GANYMED_SSH2 
#define JEXT_JOPT_SIMPLE
#endif

#ifdef PROD_MATIS_LOGGING
#define SS_JAVA
#define JEXT_LOG4J
#define JEXT_COMMON_LOGGING
#endif

#ifdef PROD_TLM_MRO
#define SS_JAVA
#define JEXT_XERCES
#define JEXT_AMMOSDOM
#define PROD_FEI5_Client
#define PROD_JavaVicarIO
#define PROD_MATIS_LOGGING
#define PROD_TLM_JavaCore
#endif

#ifdef PROD_TLM_SIRTF
#define SS_JAVA
#define PROD_TLM_JavaCore
#define JEXT_AMMOSDOM
#define JEXT_XERCES
#define PROD_TLM_PEWRAPPER
#define EXT_JNI
#endif

#ifdef PROD_TLM_PEWRAPPER
#define SS_JAVA
#define SS_TLM
#define EXT_SIRTF_PHE
#endif

#ifdef PROD_TLM_JavaCore
#define SS_JAVA
#define JEXT_ORBACUS
#define JEXT_OODT
#define JEXT_COSNOTIFY
#endif

***
* MDMS
***

* JConnect is the connection to Sybase.  Turn it on if Sybase is used, but
* only if we also have some Java.
#if defined(EXT_SYBASE) && defined(SS_JAVA)
#define JEXT_JCONNECT
#endif

#ifdef PROD_PWDSERVER_JCLIENT
#define SS_JAVA
#define PROD_FEI5_Client	/* ???? */
#endif

#ifdef PROD_FEI5_Savannah
#define SS_JAVA
#define PROD_FEI5_Client
#endif

#ifdef PROD_FEI5_Client_MSL    /* MSL plug-ins for FEI5 framework */
#define SS_JAVA
#define PROD_FEI5_Client
#define JEXT_DERBY
#endif

#ifdef PROD_FEI5_Spider
#define SS_JAVA
#define JEXT_XERCES
#define PROD_FEI5_Client
#define JEXT_JAXB
#define JEXT_HIBERNATE2
#define JEXT_LOG4J
#define JEXT_DOM4J
#define JEXT_ACTIVEMQ
#endif

#ifdef PROD_FEI5_Client
#define SS_JAVA
#define JEXT_XERCES
#define JEXT_ATLAS_SERVICE
#define JEXT_DOM4J
#define JEXT_LOG4J
#define JEXT_JAXWS
#define JEXT_JAXB
#define JEXT_FEI5_Server	/* ???? */
#endif

#ifdef PROD_FEI5_Server
#define SS_JAVA
#define JEXT_JAXB
#define JEXT_XERCES
#define JEXT_JGROUPS
#define JEXT_DOM4J
#define JEXT_C3P0
#define JEXT_EJB 
#define JEXT_ACTIVEMQ
#define JEXT_RSA
#define JEXT_MYSQL
#define JEXT_FSCONTEXT
#define JEXT_PROVIDERUTIL
#endif

***
* Other
***

#ifdef PROD_MER_RMC
#define SS_JAVA
#define JEXT_XERCES
#endif

#ifdef PROD_SPICE_CORBA
#define SS_JAVA
#define JEXT_ORBACUS
#endif

#ifdef PROD_SPICE_JNI
#define SS_JAVA
#define EXT_JNI
#endif

#ifdef PROD_JavaVicarIO
#define SS_JAVA
#define JEXT_XERCES
#define JEXT_XALAN
#define JEXT_NOM_TAM_FITS
#define JEXT_JAKARTA_ORO
#define JEXT_JAI
#define JEXT_JAI_IIO
#define JEXT_JAI_EXT
*Added for Vicar Open Source
#define JEXT_COMMONS_VFS
#define JEXT_COMMON_LOGGING
*Added for Vicar Open Source 3/21/16
*#define JEXT_PDS4_TOOLS
*Added for Vicar Open Source 3/22/16
*#define JEXT_PDS_GENERATE_TOOL
*Added for Vicar Open Source 3/23/16
*#define JEXT_PROD_TOOLS
#endif

#ifdef PROD_JadeDisplay
#define SS_JAVA
#define JEXT_JAI
#define JEXT_JAI_IIO
#endif

#ifdef EXT_CALLABLE_JDK
#define EXT_JNI
#endif

#ifdef EXT_JNI
#define SS_JAVA
#endif

#ifdef EXT_SPICE
#define SS_V2CONFIG	/* used in spices vicset1 paragraph */
#endif

#ifdef PROD_APPS
#define SS_JAVA
#define JEXT_PROD_TOOLS
#define JEXT_JSON
#define JEXT_COMMONS_VFS
#define JEXT_HTTPCOMP_CLIENT
#define JEXT_JACKRABBIT_WEBDAV
#define JEXT_VELOCITY
#define JEXT_VELOCITY_TOOLS
#define JEXT_OODT_TARGET
#define JEXT_OODT_WORKFLOW
#define JEXT_OODT_PGE
#define JEXT_RABBITMQ
#define JEXT_COMMONS_DIGESTER
#define JEXT_COMMONS_BEANUTILS
#define JEXT_COMMONS_LANG
#define JEXT_PDS_GENERATE_TOOL
#define JEXT_PDS4_TOOLS
#endif

********************
* JPL Externals
********************

#ifdef JEXT_JSCH
#define JEXT_AMMOSDOM
#endif

#ifdef JEXT_AMMOSDOM
#define JEXT_JAVAMAIL
#define JEXT_JDOM
#define JEXT_JAF
#endif

#ifdef JEXT_ATLAS_SERVICE
#define JEXT_JAXWS
#define JEXT_JAXB
#endif

#ifdef JEXT_OODT
#define JEXT_ORBACUS
#define JEXT_XERCES
#define JEXT_XALAN
#endif

********************
* True Externals
********************

#ifdef JEXT_MYSQL
#define JEXT_C3P0
#define JEXT_LOG4J
#endif

#ifdef JEXT_DOM4J
#define JEXT_XERCES
#define JEXT_JAXB
#define JEXT_JAXEN
#endif

#ifdef JEXT_COSNOTIFY
#define JEXT_ORBACUS
#endif

#ifdef JEXT_JGROUPS
#define JEXT_LOG4J
#define JEXT_XERCES
#define JEXT_JBOSS
#define JEXT_UTIL_CONCURRENT
#endif

#ifdef JEXT_JCONNECT
#define JEXT_HIBERNATE
#endif

#ifdef JEXT_JBPM
#define JEXT_COMMON_LOGGING
#define JEXT_HIBERNATE
#define JEXT_JBOSS
#define JEXT_DOM4J
#define JEXT_XERCES
#endif

#ifdef JEXT_JBOSS
#define JEXT_XERCES
#define JEXT_LOG4J
#define JEXT_UTIL_CONCURRENT
#define JEXT_DOM4J
#define JEXT_HIBERNATE
#endif

#ifdef JEXT_RESTLET
#endif

#ifdef JEXT_COMMON_CODEC
#endif

#ifdef JEXT_HIBERNATE2
#define JEXT_XERCES
#define JEXT_DOM4J
#define JEXT_C3P0
#define JEXT_COMMON_POOL
#define JEXT_COMMON_DBCP
#define JEXT_COMMON_COLLECTIONS
#define JEXT_COMMON_LOGGING
#endif

#ifdef JEXT_HIBERNATE
#define JEXT_COMMON_LOGGING
#define JEXT_COMMON_COLLECTIONS
#define JEXT_XERCES
#define JEXT_DOM4J
#define JEXT_C3P0
#define JEXT_JBOSS
#define JEXT_COMMONS_LANG
#endif

#ifdef JEXT_C3P0
#define JEXT_XERCES
#endif

#ifdef JEXT_COMMON_DBCP
#define JEXT_COMMON_POOL
#define JEXT_COMMON_COLLECTIONS
#define JEXT_XERCES
#endif

#ifdef JEXT_COMMON_LOGGING
#define JEXT_LOG4J
#endif

#ifdef JEXT_COMMON_POOL
#define JEXT_COMMON_COLLECTIONS
#endif

#ifdef JEXT_COMMON_CONFIGURATION
#define JEXT_COMMON_COLLECTIONS
#define JEXT_COMMONS_LANG
#endif

#if defined(JEXT_JAXB) || defined(JEXT_JAXWS)
#define JEXT_XERCES
#define JEXT_ACTIVATION
#define JEXT_JAXWS
#define JEXT_DOM4J
#define JEXT_JAXEN
#endif

#ifdef JEXT_JAXEN
#define JEXT_XERCES
#define JEXT_DOM4J
#define JEXT_JDOM
#endif

#ifdef JEXT_JDOM
#define JEXT_XERCES
#endif

#ifdef JEXT_LOG4J
#define JEXT_JAVAMAIL
#endif

#ifdef JEXT_JAVAMAIL
#define JEXT_ACTIVATION
#endif

#ifdef JEXT_XALAN
#define JEXT_XERCES
#endif

#ifdef JEXT_IVOAFITS
#define JEXT_NOM_TAM_FITS
#endif
