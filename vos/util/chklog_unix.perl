#! /usr/bin/perl
#
##
#  This program evaluates the Vicar build logs for 
#  errors.
#
#  Input vicar build log name
#
$log = @ARGV[0];
$flag = @ARGV[1];
#
# Open log file
# 
open(LOG,$log) || die "The vicar build log file could not be opened.\n";
if ($flag eq "")
{  print "************Platform designation missing for build log.***********\n";
   print "Please enter solr or linux.\n"
}
$name = " ";
$errmsg = " ";
#
#####################################################################################
# SUN-SOLR Build Exception Reasons:
# 
# 1) pgm_icons,rts_tcl_scripts,tp_pdf and xvd_pdf, rtcsetupvms 
#    actually have no build.  The files only need to get unpacked.
#    The errors generated are spurious.
#
# 2) jpegcomp: Per Allan Runkle, "Since this code is currently not 
#              required for any active mission, I do not
#              think it is worth correcting."
#
# 3) Linux only: comp3_pdf     Per Barbara McGuffie, 10/11/01 
#                ads_server    Per MPB, 5/3/02
#
# 4) P3 ROUTINES: p3/sub/zlow7.com, p3/prog/l7cal,sebasscal,tcal2,timscal2,timscal2re  
#                  These routines were delivered by a summer hire for Barbara McGuffie.
#                  zlow7.com is missing a header file.  The p3 programs are missing a 
#                  module mod3files.  9/9/05 
#         
# 5) Routines removed from list due to the code being obsoleted:  
#                 craf, gedread, gll_brws_dsp, gllimbuildnims, gllimbuildpws, gllimbuildssi,
#                 glltelemproc, nimsmerge2, pgm_icons                
#
# 6) P3 routine: temis added 3/8/06 Part of the summer hire delivery.
# 7) Phoenix code delivered to linux only.  The following code does not build on sun-solr:
#       phx_cam_decompress, phx_ra_decompress, phxtelemlib, phxtelemproc    5/10/2006
# 8) tlm_base, tlm_event, tlm_Test_Test_stream: Not used on sun-solr 3/22/07 
# 9) Diviner code delivered to linux only - libjpl_mipl_diviner_spice, diviner_ckern,diviner_l1b
# 10) Diviner code delivered to linux only - div/sub/sim_subs, div/prog/simscan
# 11) Diviner code delivered to linux only - div/sub/libdivl1b_so,libdivl1b_a, div/prog/divl1b 12/2/08
# 12) Removed tlm_event.  Builds on sun-solr.  6/19/2009 sxc
# 13) Added galsos.  1/28/2010 sxc 
# 14) Removed comp3_pdf,jpegcomp,libjpl_mipl_diviner_spice,nimsmerge2,ricecomp,sebasscal,
#     timscal2, timscal2re, zlow7, phxtelemproc, phxtelemlib, phx_ra_decompress,tlm_base,
#     from list.  It builds on solr. 6/1/10
# 15) Removed ads_server,reseaulog,rtcsetupvms,rts_tcl_scripts,tp_pdf,xvd_pdf from list. 6/1/10  
#     They have been obsoleted. 
# 16) Added mslreach.  Program only builds on the Linux platform. 6/21/11
# 17) Added mslfilter.  Program only build on the 32-bit Linux platform. 3/29/12
# 18) Added the XRT routines: xrtps,ccdnoise,ccdrecip,ccdslope,mosplot,otf1,plot3d,
#     plotint, pltgraf, power,qplot2, statplt and tieplot. 5/8/13
# 19) Removed from exception list:galsos,ccdnoise,ccdrecip,ccdslope,mosplot,otf1,plot3d,plotint,pltgraf,qplot2,tieplot
#     and statplt. 1/28/15
#####################################################################################
#
# 
@exceptsolr=("diviner_ckern","diviner_l1b","l7cal","tcal2",
"temis","tlm_Test_Test_stream",
"sim_subs","simscan","libdivl1b_so","libdivl1b_a","divl1b","mslreach","mslfilter",
"xrtps");
#
#######################################################################################
#
# Linux Build Exception Reasons:
#
#
# 1) NO XRT LIB: ccdnoise,ccdrecip,ccdslope,mosplot,otf1,plot3d,plotint, pltgraf, power,
#             qplot2, statplt,tieplot and xrtps require the XRT library which is not available
#             on the sgi or linux.
# 2) NO PVM3 LIB: gllcntrl, gll_brws_dsp, isslab, mpfcntrl,mpfcordsquat, mpfimbuildimp,
#                 mpfimbuildapx, mpfimbuildrvr, mpfpredict, mpfrvrclr, mpfrvrpredict,
#                 mpftelemproc,rtdsplot,rtlogger,m98_ssi_pktmaker,m98predict 
#
# 3) NO SIMBAD LIB: simbad, simbadcat, sky, skycat, 
#       
# 4) SPURIOUS ERRORS: pgm_icons,rts_tcl_scripts,tp_pdf, xvd_pdf,rtcsetupvms
#    actually have no build.  The files only need to get unpacked.
#    The errors generated are spurious.
#
# 5) SOLR ONLY: ds1comm,ds1sfdu,ds1telemproc,ds1fileobj,ds1sirtfbase,isis2vic
#               casisstlmproc,casvimstlmproc,sirtftlmproc,cascommonsource,
#               casisslosslessdecompsource,casisssource,casvimssource,cas_tlm_cat,
#               cas_lplist,cas_pixlist,cas_ul_cck_subs_1,cas_ul_cck_subs_2,
#               cas_ul_globals_1,cas_ul_globals_2,
#               ul_cck_tool,
#               sirtfcomm, sirtfdatamodel, sirtffileobj, sirtftlmproc, suffix2core,
#               rovernav (Per Jean Lorre 10/26/00 will deliver for linux
#                         at later date.) 
#               ics2sasf, vioi2ics added 02/12/01
#               sclk2scet added 03/05/01, solr is the only platform noted on pkg form 
#               sirtf_rt,sirtf_h,sirtflib added 6/7/01: solr only per cxr's pkg form.
#               casworker_h,casworkersource add 6/7/01: solr only per rrp's pkge form.
#               tlm/prog/tlm_Cassini_ISS_stream,tlm_Cassini_VIMS_steam: solr only per pkg
#                                                                    tlm_collocated_streams-mpb
#               
#               cas_common_cat cas_swat_cat casswatsource castpspluginsource,
#               casissstdtlmproc casswat casvimsstdtlmproc added 11/27/01: 
#               solr only per ays.
#               tlm_SIRTF_SIRTF_stream added 11/27/01: solr only per mpb
#               tlm/prog/mertelemlib & mertelemproc added 9/6/02 per 
#               MerGds6.5 pkg pxz,cxr
#               cdrsgrg added 4/9/03 per rrp
#               casstdpluginsource added 8/8/03 per rrp
#               p2/prog/ads_server.com added 9/29/2004 per sxc
#
# 6) SOLR/ALPHA ONLY: vimslab (per Alice Stanboli, 10/2/00)
#               casvimscruisesource and casvimscruisestdtlmproc added 11/13/02
#               per sxc.
#
# 7) All other modules listed have not been ported to linux.
# 8) p2/prog/featherv.com delivered for alpha only by BAM, 1/3/01.
# 9) now supporting geoma per Barbara and RGD 6/18/02.
# 10) Modules now supported on LINUX: p2/prog/ibisupdate, overlay, sargonb  
#     Delivered by Gary Yagi in August 2002 for D29.0.1. 
# 11) Removed gll/prog/galsos.com from exception list per CDE Amy Chen, 1/10/03
# 12) Removed mars/src/sub/mertelemlib and mertelemproc per pkg MER_Upd_050603_GDS8.5
#     by Hyun Lee. 
# 13) XRT Lib: Removed ccdnoise,ccdrecip,ccdslope,mosplot,otf1,plot3d,plotint, pltgraf, power,
#             qplot2, statplt,tieplot and xrtps from the build exception list.
#             The XRT Library is now available on the LINUX platform (mipldevlinux4).
#             1/28/05 sxc 
# 14) P3 Routines: These routines were delivered by a summer hire for Barbara McGuffie. They do not build on linux.
#	           p3/sub/get_sebass_wavlen,timssubs,uniflt,zlow7, p3/prog/alphar,amer,append,astergeo,astertir,avhrrlog,
#                  bmss,boxflt2,c130rect,c,convim,demloga,destretch,eigen,envi2vic,fastclass,filter,fit,flot,fromascii,
#                  gainoff,garslog,genthis,gradrem,hist,insect,l7cal,lave,lookup,maskv,masterhot,mastertir,mastertoa,
#                  mastir,median,minmap,mivistir,mss,navlog,pars,pixgrad,pixstat,qsar,ratio0,reallist,repair,sargonb,
#                  sebasscal,sebasste,shady2,shady,simplify,size,specfil,spectrum,stats,stretch3d,stretch,stretchf,strippit,
#                  swapper,tcal2,temis,textad,tgeom2,tgeominv,timscal2,timscal2old,timscal2re,timscal,timsconv,
#                  timsemis,timslog,timsnav,timsresp,tmscal,tmscalit,to3d,tran,transect,trigrid,vic2srf,watermap,xform
#                  zfill.
#                   9/9/05
#
# 15) Removed from exception list because modules now build on linux:
#                 automatch,cas_lplist,colort,data_transfer,getpc,ibisgcp,insert3d,isslab,jpegcomp,manmatch,
#                 mpfpredict,mpfrvrclr,mpfrvrpredict,maplabprog,marsjplstereo,nav2,nimscmm2,nutinp
#                  mpf_compression_labels,mpf_rcw_daemon,mpfcordsquat,obs_cat,
#                  obs_list,phot_func,phot_list,ptp,ressar77,rice_comp,ricecomp,rice_decomp,
#                 rtdsplog,rtlogger,rts_tcl_scripts,sclk2scet,scet2sclk,simbad,simbadcat,sky,skycat,suffix2core
#                 time_value,vioi2ics,rtcsetupvms    3/8/06
# 16) Removed from exception list because modules have been obsoleted:
#                 cas_pixlist,casworkersource,cas_ul_cck_subs_1,cas_ul_cck_subs_2,cas_ul_globals_1,cas_ul_globals_2,
#                 catsup, craf, ds1*, gedread, m98*, nimscmm, nimsfloat,nimsobs,nimstsi2,
#                 pgm_icons,pwssnip,sirtf_rt, sirtfcomm,sirtfdatamodel,sirtf_h,sirtffileobj
#                 sirtftelemproc,sirtflib,sirtftlmproc,ssisnip,
#                 tlm_SIRTF_SIRTF_stream,ul_cck_tool,vimslab   3/8/06
#
# 17) P3 Routine:  temis 3/8/06  Part of summer hire delivery
#
# 18) tlm_base, tlm_event, tlm_Test_Test_stream: No longer build with RHE4, appears not to be needed by SIRTF. (3/22/07)
# 19) Removed tlm_event from exception list.  Module builds with no errors.  (6/19/2009) 
# 20) Added galsos.  (1/28/2010) 
# 21) Removed the following modules from list because they now build on Linux platform:
#     get_sebass_wavlen,timssubs,uniflt,zlow7,alphar,amer,append,astergeo,astertir,avhrrlog, 
#     bmss,boxflt2,c130rect,c,convim,demloga,destretch,eigen,envi2vic,fastclas,filter,fit,flot,fromascii,
#     gainoff,garslog,genthis,gradrem,hist,insect,l7cal,lave,lookup,
#     maskv,masterhot,mastertir,mastertoa,mastir,median,minmap,mivistir,mss,navlog  
#     pixgrad,pixstat,qsar,ratio0,reallist,repair,sargonb,sebasscal,sebasste,shady,simplify,size,
#     specfil,spectrum,stats,stretch3d,stretch,stretchf,strippit,swapper,textad,tgeom2, 
#     tgeominv,timscal2,timscal2old, timscal2re, timscal,timsconv,
#     timsemis,timslog,timsnav,timsresp,tmscal,tmscalit,to3d, tran,transect,trigrid    
#     watermap,xform,zfill,arc2graf,cascommonsource,casvimssource,casisslosslessdecompsource,casisssource,
#     casswat,casstdpluginsource,casswatsource,casvimsstdtlmproc,cas_common_cat,cas_swat_cat,cas_tlm_cat,cdrsgrg,clusan,
#     clustest,featherv,fcnpolar,graf2arc,ibisnav,ics2sasf2,interloc,jpegfix, 
#     oldgeoma2ibis,oospice_sub,paint,polypmap,vtiff,tlm_base
# 22) Removed the following modules becasue they no longer exist on the system:
#     pars,ads_server,castpspluginsource,casvimscruisesource,casvimscruisestdtlmproc,casvimstlmproc,
#     casisstlmproc,isis2vic,mpf_brws_dsp,mpf_rts_dsp,mpf_spice_time,mpfcatlbl,mpfcntrl,
#     mpfimbuildapx,mpfimbuildimp,mpfimbuildrvr,mpftelemproc,reseaulog, 
#     tlm_Cassini_ISS_stream,tlm_Cassini_VIMS_stream,xvd_pdf,tp_pdf 
#
# 23) Added the XRT routines: xrtps,ccdnoise,ccdrecip,ccdslope,mosplot,otf1,plot3d,
#     plotint, pltgraf, power,qplot2, statplt and tieplot. No XRT license available.  5/8/13
# 24) Removed from exception list:galsos,ccdnoise,ccdrecip,ccdslope,mosplot,otf1,plot3d,plotint,pltgraf,qplot2,tieplot
#     1/28/15
#
#######################################################################################
#
@exceptlinux=("tcal2","temis","vic2srf","casissstdtlmproc","rovernav","tlm_Test_Test_stream","plotit","xyznet",
"xrtps");
#
#######################################################################################
#
# Linux 64-bit Build Exception Reasons:
#
#
# 1) NO XRT LIB: ccdnoise,ccdrecip,ccdslope,mosplot,otf1,plot3d,plotint, pltgraf, power,
#             qplot2, statplt,tieplot and xrtps require the XRT library which is not available
#             on the sgi or linux.
# 2) NO PVM3 LIB: gllcntrl, gll_brws_dsp, isslab, mpfcntrl,mpfcordsquat, mpfimbuildimp,
#                 mpfimbuildapx, mpfimbuildrvr, mpfpredict, mpfrvrclr, mpfrvrpredict,
#                 mpftelemproc,rtdsplot,rtlogger,m98_ssi_pktmaker,m98predict 
#
# 3) NO SIMBAD LIB: simbad, simbadcat, sky, skycat, 
#       
# 4) SPURIOUS ERRORS: pgm_icons,rts_tcl_scripts,tp_pdf, xvd_pdf,rtcsetupvms
#    actually have no build.  The files only need to get unpacked.
#    The errors generated are spurious.
#
# 5) SOLR ONLY: ds1comm,ds1sfdu,ds1telemproc,ds1fileobj,ds1sirtfbase,isis2vic
#               casisstlmproc,casvimstlmproc,sirtftlmproc,cascommonsource,
#               casisslosslessdecompsource,casisssource,casvimssource,cas_tlm_cat,
#               cas_lplist,cas_pixlist,cas_ul_cck_subs_1,cas_ul_cck_subs_2,
#               cas_ul_globals_1,cas_ul_globals_2,
#               ul_cck_tool,
#               sirtfcomm, sirtfdatamodel, sirtffileobj, sirtftlmproc, suffix2core,
#               rovernav (Per Jean Lorre 10/26/00 will deliver for linux
#                         at later date.) 
#               ics2sasf, vioi2ics added 02/12/01
#               sclk2scet added 03/05/01, solr is the only platform noted on pkg form 
#               sirtf_rt,sirtf_h,sirtflib added 6/7/01: solr only per cxr's pkg form.
#               casworker_h,casworkersource add 6/7/01: solr only per rrp's pkge form.
#               tlm/prog/tlm_Cassini_ISS_stream,tlm_Cassini_VIMS_steam: solr only per pkg
#                                                                    tlm_collocated_streams-mpb
#               
#               cas_common_cat cas_swat_cat casswatsource castpspluginsource,
#               casissstdtlmproc casswat casvimsstdtlmproc added 11/27/01: 
#               solr only per ays.
#               tlm_SIRTF_SIRTF_stream added 11/27/01: solr only per mpb
#               tlm/prog/mertelemlib & mertelemproc added 9/6/02 per 
#               MerGds6.5 pkg pxz,cxr
#               cdrsgrg added 4/9/03 per rrp
#               casstdpluginsource added 8/8/03 per rrp
#               p2/prog/ads_server.com added 9/29/2004 per sxc
#
# 6) SOLR/ALPHA ONLY: vimslab (per Alice Stanboli, 10/2/00)
#               casvimscruisesource and casvimscruisestdtlmproc added 11/13/02
#               per sxc.
#
# 7) All other modules listed have not been ported to linux.
# 8) p2/prog/featherv.com delivered for alpha only by BAM, 1/3/01.
# 9) now supporting geoma per Barbara and RGD 6/18/02.
# 10) Modules now supported on LINUX: p2/prog/ibisupdate, overlay, sargonb  
#     Delivered by Gary Yagi in August 2002 for D29.0.1. 
# 11) Removed gll/prog/galsos.com from exception list per CDE Amy Chen, 1/10/03
# 12) Removed mars/src/sub/mertelemlib and mertelemproc per pkg MER_Upd_050603_GDS8.5
#     by Hyun Lee. 
# 13) XRT Lib: Removed ccdnoise,ccdrecip,ccdslope,mosplot,otf1,plot3d,plotint, pltgraf, power,
#             qplot2, statplt,tieplot and xrtps from the build exception list.
#             The XRT Library is now available on the LINUX platform (mipldevlinux4).
#             1/28/05 sxc 
# 14) P3 Routines: These routines were delivered by a summer hire for Barbara McGuffie. They do not build on linux.
#                  p3/sub/get_sebass_wavlen,timssubs,uniflt,zlow7, p3/prog/alphar,amer,append,astergeo,astertir,avhrrlog,
#                  bmss,boxflt2,c130rect,c,convim,demloga,destretch,eigen,envi2vic,fastclass,filter,fit,flot,fromascii,
#                  gainoff,garslog,genthis,gradrem,hist,insect,l7cal,lave,lookup,maskv,masterhot,mastertir,mastertoa,
#                  mastir,median,minmap,mivistir,mss,navlog,pars,pixgrad,pixstat,qsar,ratio0,reallist,repair,sargonb,
#                  sebasscal,sebasste,shady2,shady,simplify,size,specfil,spectrum,stats,stretch3d,stretch,stretchf,strippit,
#                  swapper,tcal2,temis,textad,tgeom2,tgeominv,timscal2,timscal2old,timscal2re,timscal,timsconv,
#                  timsemis,timslog,timsnav,timsresp,tmscal,tmscalit,to3d,tran,transect,trigrid,vic2srf,watermap,xform
#                  zfill.
#                   9/9/05
#
# 15) Removed from exception list because modules now build on linux:
#                 automatch,cas_lplist,colort,data_transfer,getpc,ibisgcp,insert3d,isslab,jpegcomp,manmatch,
#                 mpfpredict,mpfrvrclr,mpfrvrpredict,maplabprog,marsjplstereo,nav2,nimscmm2,nutinp
#                  mpf_compression_labels,mpf_rcw_daemon,mpfcordsquat,obs_cat,
#                  obs_list,phot_func,phot_list,ptp,ressar77,rice_comp,ricecomp,rice_decomp,
#                 rtdsplog,rtlogger,rts_tcl_scripts,sclk2scet,scet2sclk,simbad,simbadcat,sky,skycat,suffix2core
#                 time_value,vioi2ics,rtcsetupvms    3/8/06
# 16) Removed from exception list because modules have been obsoleted:
#                 cas_pixlist,casworkersource,cas_ul_cck_subs_1,cas_ul_cck_subs_2,cas_ul_globals_1,cas_ul_globals_2,
#                 catsup, craf, ds1*, gedread, m98*, nimscmm, nimsfloat,nimsobs,nimstsi2,
#                 pgm_icons,pwssnip,sirtf_rt, sirtfcomm,sirtfdatamodel,sirtf_h,sirtffileobj
#                 sirtftelemproc,sirtflib,sirtftlmproc,ssisnip,
#                 tlm_SIRTF_SIRTF_stream,ul_cck_tool,vimslab   3/8/06
#
# 17) P3 Routine:  temis 3/8/06  Part of summer hire delivery
#
# 18) tlm_base, tlm_event, tlm_Test_Test_stream: No longer build with RHE4, appears not to be needed by SIRTF. (3/22/07)
# 19) Removed tlm_event from exception list.  Module builds with no errors.  (6/19/2009) 
# 20) Added galsos.  (1/28/2010) 
# 21) Removed the following modules from list because they now build on Linux platform:
#     get_sebass_wavlen,timssubs,uniflt,zlow7,alphar,amer,append,astergeo,astertir,avhrrlog, 
#     bmss,boxflt2,c130rect,c,convim,demloga,destretch,eigen,envi2vic,fastclas,filter,fit,flot,fromascii,
#     gainoff,garslog,genthis,gradrem,hist,insect,l7cal,lave,lookup,
#     maskv,masterhot,mastertir,mastertoa,mastir,median,minmap,mivistir,mss,navlog  
#     pixgrad,pixstat,qsar,ratio0,reallist,repair,sargonb,sebasscal,sebasste,shady,simplify,size,
#     specfil,spectrum,stats,stretch3d,stretch,stretchf,strippit,swapper,textad,tgeom2, 
#     tgeominv,timscal2,timscal2old, timscal2re, timscal,timsconv,
#     timsemis,timslog,timsnav,timsresp,tmscal,tmscalit,to3d, tran,transect,trigrid    
#     watermap,xform,zfill,arc2graf,cascommonsource,casvimssource,casisslosslessdecompsource,casisssource,
#     casswat,casstdpluginsource,casswatsource,casvimsstdtlmproc,cas_common_cat,cas_swat_cat,cas_tlm_cat,cdrsgrg,clusan,
#     clustest,featherv,fcnpolar,graf2arc,ibisnav,ics2sasf2,interloc,jpegfix, 
#     oldgeoma2ibis,oospice_sub,paint,polypmap,vtiff,tlm_base
# 22) Removed the following modules because they no longer exist on the system:
#     pars,ads_server,castpspluginsource,casvimscruisesource,casvimscruisestdtlmproc,casvimstlmproc,
#     casisstlmproc,isis2vic,mpf_brws_dsp,mpf_rts_dsp,mpf_spice_time,mpfcatlbl,mpfcntrl,
#     mpfimbuildapx,mpfimbuildimp,mpfimbuildrvr,mpftelemproc,reseaulog, 
#     tlm_Cassini_ISS_stream,tlm_Cassini_VIMS_stream,xvd_pdf,tp_pdf 
# 23) Added mslfilter.  It only builds on Linux 32-bit.  
#
# 24) Removed from exception list:galsos,ccdnoise,ccdrecip,ccdslope,mosplot,otf1,plot3d,plotint,pltgraf,qplot2,tieplot
#     statplt, plotit. 1/28/15
#######################################################################################
#
@exceptlinux64=("tcal2","temis","vic2srf","casissstdtlmproc","casisslosslessdecompsource","casisssource","rovernav",
"tlm_Test_Test_stream","xyznet",
"mslreach","libpig_native","PEWrapper","xrtps","libdivl1b_a","libdivl1b_so",
"power","isslab","suffix2core","vimslab","casswat","casvimsstdtlmproc","divl1b",
"usedisp","mslfilter");

ERROR: 

while (<LOG>) {
       chomp;
       if (/^\#\s*module\s*(\w+)\s*$/) {
         $name = $1;
#       print "* module $name\n";
 
}


if ($flag eq "solr") {                                     # SOLR
  if (/Error: / || /error: /) {                            
          $errmsg = $_;
          foreach $pattern (@exceptsolr) {
            if ($name =~ $pattern) {
            next ERROR;}
         }
      print "\n"; 
      print "* module $name\n"; 
      print "$errmsg\n";
    }

}

elsif ($flag eq "linux") {
 if (/Error 1/||/No rule/) {
         $errmsg = $_;
         foreach $pattern (@exceptlinux) {
           if ($name =~ $pattern) {
           next ERROR;}
        }
      print "\n";
      print "* module $name\n";
      print "$errmsg\n";
    } 
}

elsif ($flag eq "linux64") {
 if (/Error 1/||/No rule/) {
         $errmsg = $_;
         foreach $pattern (@exceptlinux64) {
           if ($name =~ $pattern) {
           next ERROR;}
        }
      print "\n";
      print "* module $name\n";
      print "$errmsg\n";
    }
}
}
close(LOG);
