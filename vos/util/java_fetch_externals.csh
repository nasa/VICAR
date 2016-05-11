#!/bin/tcsh
# Note: tcsh is used instead of csh due to restrictions on the length of an
# env var in Solaris 2.7.  $CLASSPATH needs to be more than 1024 characters.
# The above line may need to be changed to the correct path for tcsh on your
# system.  If csh is ever fixed (as it is on Linux), change this back to csh.
#
# Fetch all jars and associated files from the Java external products and
# put them in the appropriate places in $V2HTML for use at runtime.
# Note that $CLASSPATH may need to be reset after this, in order to include
# these new jars.  Rerun vicset1 (aka select) if necessary.
#

echo "##########"
echo " Copying external jars"
echo "##########"

if ( ! -e $V2HTML ) then
   mkdir $V2HTML
endif
if ( ! -e $V2HTML/jars ) then
   mkdir $V2HTML/jars
endif

if ($?J2_XAMPLE != 0) then
  cp $J2_XAMPLE/lib/mdate.jar $V2HTML/jars
  cp $J2_XAMPLE/lib/xml-apis.jar $V2HTML/jars
  cp $J2_XAMPLE/lib/xmleditor.jar $V2HTML/jars
  cp $J2_XAMPLE/xample.jar $V2HTML/jars
endif

# ActivMQ - high performance Apache 2.0 licensed Message Broker and JMS 1.1 implementation

if ($?J2_ACTIVEMQ != 0) then
  cp $J2_ACTIVEMQ/activemq-all-5.1.0.jar $V2HTML/jars
endif

# Orbacus

if ($?J2_ORBACUS != 0) then
  cp $J2_ORBACUS/lib/OB.jar $V2HTML/jars
endif

# nom_tam_fits

if ($?J2_NOM_TAM_FITS != 0) then
  cp $J2_NOM_TAM_FITS/fits.jar $V2HTML/jars
endif

# CosNotify

if ($?J2_COSNOTIFY != 0) then
  cp $J2_COSNOTIFY/lib/CosNotify.jar $V2HTML/jars
endif

# Xerces

if ($?J2_XERCES != 0) then
#cp $J2_XERCES/xml-apis.jar $V2HTML/jars
  cp $J2_XERCES/xmlParserAPIs.jar $V2HTML/jars
  cp $J2_XERCES/xercesImpl.jar $V2HTML/jars
endif

# Xalan
# Hopefully xalanj1compat.jar is temporary until tlm.log can get cleaned up!

if ($?J2_XALAN != 0) then
  cp $J2_XALAN/xalan.jar $V2HTML/jars
#  cp $J2_XALAN/bin/xalanj1compat.jar $V2HTML/jars
  cp $J2_XALAN/serializer.jar $V2HTML/jars
  cp $J2_XALAN/xercesImpl.jar $V2HTML/jars
  cp $J2_XALAN/xml-apis.jar $V2HTML/jars
endif

# EJB, Servlet, and JSP Development Kit
#
if ($?J2_J2EESDK != 0) then
  cp $J2_J2EESDK/j2ee.jar $V2HTML/jars
endif

# Object Oriented Data Technology framework

if ($?J2_OODT != 0) then
  cp $J2_OODT/lib/oodt.jar $V2HTML/jars
endif

# Sybase JDBC driver

if ($?J2_JCONNECT != 0) then
  cp $J2_JCONNECT/classes/jconn2.jar $V2HTML/jars
endif

# Jakarta-oro regular expression parsing tools

if ($?J2_JAKARTA_ORO != 0) then
  cp $J2_JAKARTA_ORO/jakarta-oro-2.0.4.jar $V2HTML/jars
endif

# util.concurrent concurrency toolkit

if ($?J2_UTIL_CONCURRENT != 0) then
  cp $J2_UTIL_CONCURRENT/dist/lib/util.concurrent.jar $V2HTML/jars
endif

# JDOM XML data manipulation and output

if ($?J2_JDOM != 0) then
  cp $J2_JDOM/build/jdom.jar $V2HTML/jars
endif

# AmmosDOM (Distributed Object Manager) code needed for SIRTF (possibly others)

if ($?J2_AMMOSDOM != 0) then
  cp $J2_AMMOSDOM/ammosdom.jar $V2HTML/jars
endif

# Java Web Services Development Package.  Note that we only care about JAXB
# (and its dependencies) at this time.
#
# NOTE: These are superceded by the JAXWS jars below
#
#if ($?J2_JWSDP != 0) then
#cp $J2_JWSDP/jaxb/lib/jaxb-api.jar $V2HTML/jars
#cp $J2_JWSDP/jaxb/lib/jaxb-impl.jar $V2HTML/jars
#cp $J2_JWSDP/jaxb/lib/jaxb-libs.jar $V2HTML/jars
#cp $J2_JWSDP/jwsdp-shared/lib/namespace.jar $V2HTML/jars
#cp $J2_JWSDP/jwsdp-shared/lib/relaxngDatatype.jar $V2HTML/jars
#cp $J2_JWSDP/jwsdp-shared/lib/jax-qname.jar $V2HTML/jars
#cp $J2_JWSDP/jwsdp-shared/lib/xsdlib.jar $V2HTML/jars
#endif
#

#
# Java API for XML binding
#
if ($?J2_JAXB != 0) then
  cp $J2_JAXB/lib/jaxb-api.jar $V2HTML/jars
  cp $J2_JAXB/lib/jaxb-impl.jar $V2HTML/jars
  cp $J2_JAXB/lib/jsr173_1.0_api.jar $V2HTML/jars
  cp $J2_JAXB/lib/jaxb-xjc.jar $V2HTML/jars
endif

#
# Java API for XML-based Web Services.
#
#  NOTE: activation.jar is also needed by
#   JAX-WS.  Not fetching it here because
#   it is already fetched from $J2_JAF
#   Similarly, not fetching jaxb-api, jaxb-impl
#   jaxb-xjc, and jsr173_api jar files 
#   because these are already fetched from
#   $J2_JAXB
if ($?J2_JAXWS != 0) then
  cp $J2_JAXWS/lib/FastInfoset.jar $V2HTML/jars
  cp $J2_JAXWS/lib/http.jar $V2HTML/jars
  cp $J2_JAXWS/lib/jaxws-api.jar $V2HTML/jars
  cp $J2_JAXWS/lib/jaxws-rt.jar $V2HTML/jars
  cp $J2_JAXWS/lib/jaxws-tools.jar $V2HTML/jars
  cp $J2_JAXWS/lib/jsr181-api.jar $V2HTML/jars
  cp $J2_JAXWS/lib/jsr250-api.jar $V2HTML/jars
  cp $J2_JAXWS/lib/resolver.jar $V2HTML/jars
  cp $J2_JAXWS/lib/saaj-api.jar $V2HTML/jars
  cp $J2_JAXWS/lib/saaj-impl.jar $V2HTML/jars
  cp $J2_JAXWS/lib/sjsxp.jar $V2HTML/jars
  cp $J2_JAXWS/lib/stax-ex.jar $V2HTML/jars
  cp $J2_JAXWS/lib/streambuffer.jar $V2HTML/jars
endif

# Log4J library
if ($?J2_LOG4J != 0) then
  cp $J2_LOG4J/dist/lib/log4j-1.2.14.jar $V2HTML/jars
endif

# JNDI fscontext library
if ($?J2_FSCONTEXT != 0) then
  cp $J2_FSCONTEXT/lib/fscontext.jar $V2HTML/jars
endif

# JNDI providerutil library
if ($?J2_PROVIDERUTIL != 0) then
  cp $J2_PROVIDERUTIL/lib/providerutil.jar $V2HTML/jars
endif

# Jaxen library
if ($?J2_JAXEN != 0) then
  cp $J2_JAXEN/jaxen-1.1.jar $V2HTML/jars 
endif

# JavaMail library
if ($?J2_JAVAMAIL != 0) then
  cp $J2_JAVAMAIL/mail.jar $V2HTML/jars
endif

# JAF library
if ($?J2_JAF != 0) then
  cp $J2_JAF/activation.jar $V2HTML/jars
endif

# Jakarta Common Logging Library
if ($?J2_COMMON_LOGGING != 0) then
  cp $J2_COMMON_LOGGING/commons-logging.jar $V2HTML/jars
  cp $J2_COMMON_LOGGING/commons-logging-api.jar $V2HTML/jars
endif

# JGroups Library
if ($?J2_JGROUPS != 0) then
  cp $J2_JGROUPS/jgroups-all.jar $V2HTML/jars
endif

# Hibernate Libraries
if ($?J2_HIBERNATE2 != 0) then
  cp $J2_HIBERNATE2/hibernate2.jar $V2HTML/jars
endif

# Hibernate Libraries
if ($?J2_HIBERNATE != 0) then
  cp $J2_HIBERNATE/hibernate3.jar $V2HTML/jars
endif
# 3rd party JARS required by Hibernate
if ($?J2_HIBERNATE != 0) then
  cp $J2_HIBERNATE/lib/bytecode/cglib/cglib-2.2.jar $V2HTML/jars
  cp $J2_HIBERNATE/lib/jpa/hibernate-jpa-2.0-api-1.0.0.Final.jar $V2HTML/jars
#  cp $J2_HIBERNATE/lib/required/antlr-2.7.6.jar $V2HTML/jars
#  cp $J2_HIBERNATE/lib/required/dom4j-1.6.1.jar $V2HTML/jars
  cp $J2_HIBERNATE/lib/required/jta-1.1.jar $V2HTML/jars
  cp $J2_HIBERNATE/lib/required/commons-collections-3.1.jar $V2HTML/jars
  cp $J2_HIBERNATE/lib/required/javassist-3.12.0.GA.jar $V2HTML/jars
  cp $J2_HIBERNATE/lib/required/slf4j-api-1.6.1.jar $V2HTML/jars
endif

# Commons-lang
if ($?J2_COMMONS_LANG != 0) then
   cp $J2_COMMONS_LANG/commons-lang-2.6.jar $V2HTML/jars
endif

# Jakarta Common Connection Pooling Libraries
# I see some duplicates with the $J2_HIBERNATE above, but some versions
# are different. Maybe we can combine and only keep one version? (ems)
if ($?J2_COMMON_COLLECTIONS != 0) then
  cp $J2_COMMON_COLLECTIONS/commons-collections-3.1.jar $V2HTML/jars
endif
if ($?J2_COMMON_POOL != 0) then
  cp $J2_COMMON_POOL/commons-pool-1.2.jar $V2HTML/jars
endif
if ($?J2_COMMON_DBCP != 0) then
  cp $J2_COMMON_DBCP/commons-dbcp-1.2.1.jar $V2HTML/jars
endif

# Commons-configuration
if ($?J2_COMMON_CONFIGURATION != 0) then
   cp $J2_COMMON_CONFIGURATION/commons-configuration-1.6.jar $V2HTML/jars
endif

# C3P0 connection pooling toolkit
if ($?J2_C3P0 != 0) then
  cp $J2_C3P0/lib/c3p0-0.8.5.2.jar $V2HTML/jars
endif

# DOM4J XML toolkit
if ($?J2_DOM4J != 0) then
  cp $J2_DOM4J/dom4j-1.6.jar $V2HTML/jars
endif

# JBOSS client runtime libraries
if ($?J2_JBOSS != 0) then
  cp $J2_JBOSS/client/jboss-common-client.jar $V2HTML/jars
  cp $J2_JBOSS/client/jbossmq-client.jar $V2HTML/jars
  cp $J2_JBOSS/client/jnp-client.jar $V2HTML/jars
endif

# Added for JBPM
if ($?J2_JBOSS != 0) then
  cp $J2_JBOSS/server/all/lib/hsqldb.jar $V2HTML/jars
  cp $J2_JBOSS/server/all/lib/bsh-1.3.0.jar $V2HTML/jars
  cp $J2_JBOSS/server/all/deploy/jboss-hibernate.deployer/antlr-2.7.5H3.jar $V2HTML/jars
endif

# JBPM stuff
if ($?J2_JBPM != 0) then
  cp $J2_JBPM/build/jbpm-3.0.4.jar $V2HTML/jars
  cp $J2_JBPM/build/jbpm-identity-3.0.4.jar $V2HTML/jars
endif

# PDS Atlas Web Service jars
if ($?J2_ATLAS_SERVICE != 0) then
  cp $J2_ATLAS_SERVICE/lib/atlas-ingest-service.jar $V2HTML/jars
  cp $J2_ATLAS_SERVICE/lib/atlas-query-service.jar $V2HTML/jars
endif

# MySQL stuff
if ($?J2_MYSQL != 0) then
  cp $J2_MYSQL/mysql-connector-java-3.1.12/mysql-connector-java-3.1.12-bin.jar $V2HTML/jars
endif

# JSch
if ($?J2_JSCH != 0) then
  cp $J2_JSCH/dist/lib/jsch-0.1.38.jar $V2HTML/jars
endif

# JOGL Java Bindings for OpenGL
if ($?J2_JOGL != 0) then
  cp $J2_JOGL/lib/jogl.jar $V2HTML/jars/
  cp $J2_JOGL/lib/gluegen-rt.jar $V2HTML/jars/
# we need to copy native libs to all platforms' dirs at once, regardless what machine we run the process on.
# So make sure that necessary dirs exist, if not create them.
  mkdir -m go-w -p $V2HTML/lib/sun-solr/
  mkdir -m go-w -p $V2HTML/lib/x86-linux/
  cp $J2_JOGL/x86-linux/lib/libjogl.so $V2HTML/lib/x86-linux/
  cp $J2_JOGL/x86-linux/lib/libjogl_awt.so $V2HTML/lib/x86-linux/
  cp $J2_JOGL/x86-linux/lib/libjogl_cg.so $V2HTML/lib/x86-linux/
  cp $J2_JOGL/x86-linux/lib/libgluegen-rt.so $V2HTML/lib/x86-linux/
  cp $J2_JOGL/sun-solr/lib/libjogl.so $V2HTML/lib/sun-solr/
  cp $J2_JOGL/sun-solr/lib/libjogl_awt.so $V2HTML/lib/sun-solr/
  cp $J2_JOGL/sun-solr/lib/libgluegen-rt.so $V2HTML/lib/sun-solr/
endif
# JMonkeyEngine3
if ($?J2_JMONKEYENGINE != 0) then
  cp $J2_JMONKEYENGINE/lib/jME3-core.jar $V2HTML/jars/
  cp $J2_JMONKEYENGINE/lib/jME3-desktop.jar  $V2HTML/jars/
  cp $J2_JMONKEYENGINE/lib/jME3-lwjgl-natives.jar  $V2HTML/jars/
  cp $J2_JMONKEYENGINE/lib/jME3-lwjgl.jar  $V2HTML/jars/
  cp $J2_JMONKEYENGINE/lib/lwjgl.jar $V2HTML/jars/  
endif
# Apache Derby stuff
# This jar is built for java 1.6.  
 if ($?J2_DERBY != 0) then
   cp $J2_DERBY/lib/derby.jar $V2HTML/jars
 endif

# RESTLET jars
if ($?J2_RESTLET != 0) then
  cp $J2_RESTLET/lib/org.restlet.jar $V2HTML/jars
  # Updating servlet jars to v3.0 - awt
  cp $V2EXT/javax-servlet/v3.0/javax.servlet.jar $V2HTML/jars
  cp $J2_RESTLET/lib/com.noelios.restlet.jar $V2HTML/jars
  cp $J2_RESTLET/lib/com.noelios.restlet.ext.servlet_2.4.jar $V2HTML/jars
  cp $J2_RESTLET/lib/org.restlet.ext.freemarker_2.3.jar $V2HTML/jars
endif

#RSA/TFA Libraries
if ($?J2_RSA != 0) then
  cp $J2_RSA/lib/authapi.jar $V2HTML/jars
  cp $J2_RSA/lib/cryptoj.jar $V2HTML/jars
# Commented out 10/15/13
#  cp $J2_RSA/lib/log4j-1.2.8.jar $V2HTML/jars
endif

#COMMON_CODEC
if ($?J2_CODEC != 0) then
  cp $J2_CODEC/commons-codec-1.4.jar $V2HTML/jars
endif

# JAI and JAI-IIO Tools
if ($?J2_JAI != 0) then
   cp $J2_JAI/jars/jai_codec.jar $V2HTML/jars/
   cp $J2_JAI/jars/jai_core.jar $V2HTML/jars/
   cp $J2_JAI/jars/mlibwrapper_jai.jar $V2HTML/jars/
#   cp $J2_JAI/sun-solr/lib/jai_codec.jar $V2HTML/jars/
#   cp $J2_JAI/sun-solr/lib/jai_core.jar $V2HTML/jars/
#   cp $J2_JAI/sun-solr/lib/mlibwrapper_jai.jar $V2HTML/jars/
# we need to copy native libs to all platforms' dirs at once, regardless what machine we run the process
# on.
# So make sure that necessary dirs exist, if not create them.
  mkdir -m go-w -p $V2HTML/lib/sun-solr/
  mkdir -m go-w -p $V2HTML/lib/x86-linux/
  mkdir -m go-w -p $V2HTML/lib/x86-64-linx/
   cp $J2_JAI/sun-solr/lib/libmlib_jai.so $V2HTML/lib/sun-solr/
   cp $J2_JAI/sun-solr/lib/libmlib_jai_vis.so $V2HTML/lib/sun-solr/
   cp $J2_JAI/sun-solr/lib/libmlib_jai_vis2.so $V2HTML/lib/sun-solr/
   cp $J2_JAI/x86-linux/lib/libmlib_jai.so $V2HTML/lib/x86-linux/
   cp $J2_JAI/x86-64-linx/lib/libmlib_jai.so $V2HTML/lib/x86-64-linx/
endif

if ($?J2_JAI_IIO != 0) then
   cp $J2_JAI_IIO/jars/clibwrapper_jiio.jar $V2HTML/jars/
   cp $J2_JAI_IIO/jars/jai_imageio.jar $V2HTML/jars/

#   cp $J2_JAI_IIO/sun-solr/lib/clibwrapper_jiio.jar $V2HTML/jars/
#   cp $J2_JAI_IIO/sun-solr/lib/jai_imageio.jar $V2HTML/jars/
# we need to copy native libs to all platforms' dirs at once, regardless what machine we run the process
# on.
# So make sure that necessary dirs exist, if not create them.
  mkdir -m go-w -p $V2HTML/lib/sun-solr/
  mkdir -m go-w -p $V2HTML/lib/x86-linux/
  mkdir -m go-w -p $V2HTML/lib/x86-64-linx/
   cp $J2_JAI_IIO/sun-solr/lib/libclib_jiio.so $V2HTML/lib/sun-solr/
   cp $J2_JAI_IIO/sun-solr/lib/libclib_jiio_vis.so $V2HTML/lib/sun-solr/
   cp $J2_JAI_IIO/sun-solr/lib/libclib_jiio_vis2.so $V2HTML/lib/sun-solr/
   cp $J2_JAI_IIO/x86-linux/lib/libclib_jiio.so $V2HTML/lib/x86-linux/
   cp $J2_JAI_IIO/x86-64-linx/lib/libclib_jiio.so $V2HTML/lib/x86-64-linx/
endif

# TREEVOTEE
if ($?J2_TREEVOTEE != 0) then
  cp $J2_TREEVOTEE/treevotee-0.9.9p1ee.jar $V2HTML/jars
endif

# JUNAMI
if ($?J2_JUNAMI != 0) then
  cp $J2_JUNAMI/junami-0.7.0p1.jar $V2HTML/jars
endif

# JSON_SIMPLE
if ($?J2_JSON_SIMPLE != 0) then
  cp $J2_JSON_SIMPLE/json_simple-1.1.jar $V2HTML/jars
endif

# SLF4J
if ($?J2_SLF4J != 0) then
  cp $J2_SLF4J/slf4j-api-1.6.1.jar $V2HTML/jars
  cp $J2_SLF4J/slf4j-jdk14-1.6.1.jar $V2HTML/jars
# Commented out 10/15/13
  cp $J2_SLF4J/slf4j-log4j12-1.6.1.jar $V2HTML/jars
endif

# EHCACHE 2.4.2
if ($?J2_EHCACHE != 0) then 
  cp $J2_EHCACHE/ehcache-core-2.4.2.jar $V2HTML/jars
endif

# COMMONS-IO V2.0.1
if ($?J2_COMMONS_IO != 0) then
  cp $J2_COMMONS_IO/commons-io-2.0.1-javadoc.jar $V2HTML/jars
  cp $J2_COMMONS_IO/commons-io-2.0.1-sources.jar $V2HTML/jars
  cp $J2_COMMONS_IO/commons-io-2.0.1.jar $V2HTML/jars
endif

# IVOAFITS
if ($?J2_IVOAFITS != 0) then
  cp $J2_IVOAFITS/ivoafits-0.3.jar $V2HTML/jars
endif

# ACTIVITI
if ($?J2_ACTIVITI != 0) then
  cp $J2_ACTIVITI/setup/files/dependencies/libs/activiti-cxf-5.7.jar $V2HTML/jars
  cp $J2_ACTIVITI/setup/files/dependencies/libs/activiti-cxf-5.7-sources.jar $V2HTML/jars
  cp $J2_ACTIVITI/setup/files/dependencies/libs/activiti-engine-5.7.jar $V2HTML/jars
  cp $J2_ACTIVITI/setup/files/dependencies/libs/activiti-engine-5.7-sources.jar $V2HTML/jars
  cp $J2_ACTIVITI/setup/files/dependencies/libs/activiti-spring-5.7.jar $V2HTML/jars
  cp $J2_ACTIVITI/setup/files/dependencies/libs/activiti-spring-5.7-sources.jar $V2HTML/jars
endif

# PDS_IMG_ATLAS
#if ($?J2_PDS_IMG_ATLAS != 0) then
#cp $J2_PDS_IMG_ATLAS/mysql-connector-java-3.0.17-ga-bin.jar $V2HTML/jars
#endif
 
# LUCENE
if ($?J2_LUCENE != 0) then
  cp $J2_LUCENE/lucene-core-3.5.0.jar $V2HTML/jars
endif

# HTTPCOMPONENTS-CORE
if ($?J2_HTTPCORE != 0) then
  cp $J2_HTTPCORE/httpcore-4.1.4.jar $V2HTML/jars      
endif

# HTTPCLIENT
if ($?J2_HTTPCLIENT != 0) then
  cp $J2_HTTPCLIENT/httpclient-4.1.3.jar $V2HTML/jars 
endif

# GANYMED_SSH2
if ($?J2_GANYMED_SSH2 != 0) then
  cp $J2_GANYMED_SSH2/ganymed-ssh2-build250.jar $V2HTML/jars
endif

# JOPT_SIMPLE 
if ($?J2_JOPT_SIMPLE != 0) then
  cp $J2_JOPT_SIMPLE/jopt-simple-4.3.jar $V2HTML/jars
endif

# COMMONS_EXEC
if ($?J2_COMMONS_EXEC != 0) then
  cp $J2_COMMONS_EXEC/commons-exec-1.1.jar $V2HTML/jars
  cp $J2_COMMONS_EXEC/commons-exec-1.1-javadoc.jar $V2HTML/jars
  cp $J2_COMMONS_EXEC/commons-exec-1.1-sources.jar $V2HTML/jars
endif

# URL Rewrite
if ($?J2_URL_REWRITE != 0) then
  cp $J2_URL_REWRITE/urlrewritefilter-4.0.2.jar $V2HTML/jars
endif

# EJB Enterprise JavaBeans
if ($?J2_EJB != 0) then
  cp $J2_EJB/javax.ejb.jar $V2HTML/jars
endif

# FREEMARKER
if ($?J2_FREEMARKER != 0) then
  cp $J2_FREEMARKER/freemarker.jar $V2HTML/jars
endif

# PRODUCT_TOOLS
if ($?J2_PROD_TOOLS != 0) then
# Commented out 10/15/13
#   cp $J2_PROD_TOOLS/antlr-2.7.7.jar $V2HTML/jars
#   cp $J2_PROD_TOOLS/antlr-3.2.jar $V2HTML/jars
   cp $J2_PROD_TOOLS/antlr-runtime-3.2.jar $V2HTML/jars
   cp $J2_PROD_TOOLS/commons-cli-1.2.jar $V2HTML/jars
#   cp $J2_PROD_TOOLS/commons-io-1.4.jar  $V2HTML/jars
#   cp $J2_PROD_TOOLS/commons-lang-2.4.jar $V2HTML/jars
   cp $J2_PROD_TOOLS/java-diff-1.1.0.jar $V2HTML/jars
#   cp $J2_PROD_TOOLS/log4j-1.2.12.jar $V2HTML/jars
   cp $J2_PROD_TOOLS/pds-utils-1.0.1.jar $V2HTML/jars
   cp $J2_PROD_TOOLS/product-tools-3.2.0.jar $V2HTML/jars
   cp $J2_PROD_TOOLS/stringtemplate-3.2.jar $V2HTML/jars
endif

# JSON
if ($?J2_JSON != 0) then
    cp $J2_JSON/json_simple-1.1.jar $V2HTML/jars
endif

# COMMONS_VFS
if ($?J2_COMMONS_VFS != 0) then
    cp $J2_COMMONS_VFS/commons-vfs2-2.0.jar $V2HTML/jars
endif

# HTTPCOMP_CLIENT
if ($?J2_HTTPCOMP_CLIENT != 0) then
    cp $J2_HTTPCOMP_CLIENT/httpclient-cache-4.2.5.jar $V2HTML/jars
    cp $J2_HTTPCOMP_CLIENT/httpmime-4.2.5.jar $V2HTML/jars
endif

# JACKRABBIT WEBDAV
if ($?J2_JACKRABBIT_WEBDAV != 0) then
   cp $J2_JACKRABBIT_WEBDAV/jackrabbit-webdav-2.6.5.jar $V2HTML/jars
endif

# VELOCITY 
if ($?J2_VELOCITY != 0) then
   cp $J2_VELOCITY/velocity-1.7.jar $V2HTML/jars
endif

# VELOCITY-TOOLS 
if ($?J2_VELOCITY_TOOLS != 0) then
   cp $J2_VELOCITY_TOOLS/velocity-tools-2.0.jar $V2HTML/jars
endif

# OODT TARGET 
if ($?J2_OODT_TARGET != 0) then
   cp $J2_OODT_TARGET/cas-metadata-0.3.jar $V2HTML/jars
endif

# OODT WORKFLOW
if ($?J2_OODT_WORKFLOW != 0) then
   cp $J2_OODT_WORKFLOW/aopalliance-1.0.jar $V2HTML/jars
   cp $J2_OODT_WORKFLOW/asm-1.5.3.jar $V2HTML/jars
   cp $J2_OODT_WORKFLOW/asm-attrs-1.5.3.jar $V2HTML/jars
   cp $J2_OODT_WORKFLOW/cas-metadata-0.3.jar $V2HTML/jars
   cp $J2_OODT_WORKFLOW/cas-resource-0.3.jar $V2HTML/jars
   cp $J2_OODT_WORKFLOW/cas-workflow-0.3.jar $V2HTML/jars
# Commented out 10/15/13
#   cp $J2_OODT_WORKFLOW/cglib-2.1_3.jar $V2HTML/jars
   cp $J2_OODT_WORKFLOW/commons-httpclient-3.0.jar $V2HTML/jars
   cp $J2_OODT_WORKFLOW/geronimo-activation_1.1_spec-1.1.jar $V2HTML/jars
   cp $J2_OODT_WORKFLOW/geronimo-javamail_1.4_mail-1.8.1.jar $V2HTML/jars
   cp $J2_OODT_WORKFLOW/hibernate-3.2.5.ga.jar $V2HTML/jars
#   cp $J2_OODT_WORKFLOW/hsqldb-1.8.0.7.jar $V2HTML/jars
#   cp $J2_OODT_WORKFLOW/jta-1.0.1B.jar $V2HTML/jars
   cp $J2_OODT_WORKFLOW/jug-2.0.0-asl.jar $V2HTML/jars
#   cp $J2_OODT_WORKFLOW/lucene-core-2.0.0.jar $V2HTML/jars
   cp $J2_OODT_WORKFLOW/oodt-commons-0.3.jar $V2HTML/jars
   cp $J2_OODT_WORKFLOW/pcs-input-0.3.jar $V2HTML/jars
   cp $J2_OODT_WORKFLOW/spring-beans-2.0.8.jar $V2HTML/jars
   cp $J2_OODT_WORKFLOW/spring-context-2.0.8.jar $V2HTML/jars
   cp $J2_OODT_WORKFLOW/spring-core-2.5.4.jar $V2HTML/jars
   cp $J2_OODT_WORKFLOW/spring-dao-2.0.8.jar $V2HTML/jars
   cp $J2_OODT_WORKFLOW/spring-hibernate3-2.0.8.jar $V2HTML/jars
   cp $J2_OODT_WORKFLOW/spring-jdbc-2.0.8.jar $V2HTML/jars
   cp $J2_OODT_WORKFLOW/tika-core-0.8.jar $V2HTML/jars
   cp $J2_OODT_WORKFLOW/xmlrpc-2.0.1.jar $V2HTML/jars
   cp $J2_OODT_WORKFLOW/xpp3_min-1.1.4c.jar $V2HTML/jars
   cp $J2_OODT_WORKFLOW/xstream-1.3.1.jar $V2HTML/jars
endif

# OODT_PGE
if ($?J2_OODT_PGE != 0) then
   cp $J2_OODT_PGE/cas-crawler-0.3.jar $V2HTML/jars
   cp $J2_OODT_PGE/cas-filemgr-0.3.jar $V2HTML/jars
   cp $J2_OODT_PGE/cas-pge-0.3.jar $V2HTML/jars
   cp $J2_OODT_PGE/jython-2.2-beta1.jar $V2HTML/jars
   cp $J2_OODT_PGE/saxon-8.7.jar $V2HTML/jars
   cp $J2_OODT_PGE/saxon-dom-8.7.jar $V2HTML/jars
endif

# RABBITMQ
if ($?J2_RABBITMQ != 0) then
   cp $J2_RABBITMQ/rabbitmq-client.jar $V2HTML/jars
endif
#
# SXN
#
if ($?J2_SXN != 0) then
  cp $J2_SXN/sxn-0.9.2.jar $V2HTML/jars
endif

# COMMONS_BEANUTILS
if ($?J2_COMMONS_BEANUTILS != 0) then
   cp $J2_COMMONS_BEANUTILS/commons-beanutils.jar $V2HTML/jars
endif

# COMMONS_DIGESTER
if ($?J2_COMMONS_DIGESTER != 0) then
  cp $J2_COMMONS_DIGESTER/commons-digester-1.8.jar $V2HTML/jars 
endif

# PDS_GENERATE_TOOL
if ($?J2_PDS_GENERATE_TOOL != 0) then
  cp $J2_PDS_GENERATE_TOOL/generate-0.9.1.jar $V2HTML/jars
endif

# PDS4_TOOLS
if ($?J2_PDS4_TOOLS != 0) then
  cp $J2_PDS4_TOOLS/objectAccess-0.5.0.jar $V2HTML/jars
  cp $J2_PDS4_TOOLS/opencsv-2.3.jar $V2HTML/jars
endif

# WEBSOCKET
if ($?J2_WEBSOCKET != 0) then
  cp $J2_WEBSOCKET/javax.websocket-api-1.1.jar $V2HTML/jars
endif

# JAI_EXT
if ($?J2_JAI_EXT != 0) then
  cp $J2_JAI_EXT/guava-17.0.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-affine-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-algebra-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-artifact-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-bandcombine-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-bandmerge-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-bandselect-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-binarize-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-border-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-buffer-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-classifier-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-colorconvert-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-colorindexer-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-concurrent-tile-cache-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-crop-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-errordiffusion-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-format-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-imagefunction-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-iterators-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-lookup-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-mosaic-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-nullop-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-orderdither-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-piecewise-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-rescale-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-rlookup-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jts-1.12.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-scale-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-scheduler-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-square-root-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-stats-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-translate-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-utilities-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-utils-1.3.1.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-vectorbin-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-warp-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/jt-zonal-1.0.3.jar $V2HTML/jars
  cp $J2_JAI_EXT/junit-4.8.1.jar $V2HTML/jars
endif

chmod u+w $V2HTML/jars/*
chmod a+r $V2HTML/jars/*

# Done!
