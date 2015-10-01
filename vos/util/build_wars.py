#!/usr/bin/env python

# This is a standalone program to build all webapps
# under $V2TOP/webapp/src/, and package them into wars
# under $V2HTML/wars/.
#
# This uses $V2UTIL/canonical_version.py. Thus, $V2UTIL
# must be in the $PYTHONPATH, or users should "cd"
# to $V2UTIL to use this program.
#
# Usage: $V2UTIL/build_wars.py
#
# Author: Haley Nguyen
# Email: Haley.Nguyen@jpl.nasa.gov
#
# Change Log:
#  A. Tinio       v1.1   3/23/2015
#    - Added support for jar_file_optional parameter
#    - Modified to continue building other webapps even after 
#      the current webapp fails to build.
      
import os
import os.path
import shutil
import sys
from subprocess import call
from subprocess import Popen
from subprocess import STDOUT
from subprocess import PIPE

import canonical_version

__version__ = '1.1'

# This method returns True only if v2param
# can successfully test for the param, and
# successfully find the param.
def test_param (param, env):
    print "v2param -test", param
    p = Popen(['v2param', '-test', param], env=env,
    stderr=STDOUT, stdout=PIPE)
    stdout = (p.communicate()[0]).strip()

    if p.returncode != 0:
        print "WARNING: v2param fails to test for", param
        return False

    if stdout == '0':
        return False

    return True

# Return None if there's error getting the param.
# Return an empty list if there's no error getting
# the param, but there's nothing returned.
# Return a list of strings otherwise.
def get_list_param (param, env):
    print "v2param", param
    p = Popen(['v2param', param], env=env,
    stderr=STDOUT, stdout=PIPE)
    # Without the call to communicate()
    # There will be no return code, as the 
    # subprocess wouldn't be processed yet,
    # since this call is asynchronous.
    stdout = p.communicate()[0]
    if p.returncode != 0:
        print "WARNING: v2param fails to find", param
        return None
    l = stdout.splitlines()
    return l

# Return None if there's error getting the param.
# Return an empty string if there's no error getting
# the param, but there's nothing returned.
# Return a string otherwise.
def get_single_param (param, env):
    # since every param is treated as a list
    # a single param simply a list of one element.
    # If a longer list is supplied, get the first
    # element. If an empty list is supplied, print
    # a warning and return None.
    l = get_list_param(param, env)
    if len(l) == 0:
        print "WARNING: v2param fails to find a single value for", param
        return None
    if len(l) > 1:
        print "WARNING:", param, "has more than 1 elements.",
        print "All but the first are ignored."
    return l[0]

# This function will only copy jars
# if 'jar_file' parameter exists. 
# At this point, $PWD points to WEB-INF
def copy_jars (env):
    V2HTML = env['V2HTML']
    lib = os.path.join(env['PWD'], 'lib')
    # The lib directory is required to be present
    # even if there is no jars to be copied. That's
    # how the way it's always been.
    print "mkdir -p", lib
    call(['mkdir', '-p', lib], env=env)
    # if jar_file parameter exists, we need to copy jars to this webapp
    if test_param('jar_file', env):
        jars = get_list_param('jar_file', env)
        if jars == None:
            raise IOError("Can't get 'jar_file'.")

        jar_dir = os.path.join(V2HTML, 'jars')
        for j in jars:
            j = os.path.join(jar_dir, j.strip())
            print "cp", j, lib
            shutil.copy(j, lib)

# This function will copy jars if exist
# if 'jar_file_optional' parameter exists. 
# At this point, $PWD points to WEB-INF
def copy_optional_jars (env):
    V2HTML = env['V2HTML']
    lib = os.path.join(env['PWD'], 'lib')
    # The lib directory is required to be present
    # even if there is no jars to be copied. That's
    # how the way it's always been.
    print "mkdir -p", lib
    call(['mkdir', '-p', lib], env=env)
    # if jar_file parameter exists, we need to copy jars to this webapp
    if test_param('jar_file_optional', env):
        jars = get_list_param('jar_file_optional', env)
        if jars == None:
            raise IOError("Can't get 'jar_file'.")

        jar_dir = os.path.join(V2HTML, 'jars')
        for j in jars:
            j = os.path.join(jar_dir, j.strip())
            print "cp", j, lib
	    try:
                shutil.copy(j, lib)
            except IOError:
                print "WARN: Failed to copy ",j

def process_includes (env):
    # if include_list parameter exists, we need to copy other directories
    # to this webapp
    if not test_param('include_list', env):
        return

    # Get list of parameters to check on
    include_list = get_list_param('include_list', env)
    if include_list == None:
        raise IOError("Can't get 'include_list'")

    for d in include_list:
        d = d.strip()
            
        # Find source for this include.
        # The source attribute is a list.
        src = d + '.src'
        if not test_param(src, env):
            print "No", src, "attribute found. Moving on..."
            continue
        vsrc = get_list_param(src, env)
        if vsrc == None:
            msg = "ERROR: Can't get '%s'. Quitting..." % (src)
            raise IOError(msg)
        src_loc = d + '.src_loc'
        if not test_param(src_loc, env):
            print "No", src_loc, "attribute found. Moving on..."
            continue
        vsrc_loc = get_single_param(src_loc, env)
        if vsrc_loc == None:
            msg = "ERROR: Can't get '%s'. Quitting..." % (vsrc_loc)
            raise IOError(msg)
            
        dst = d + '.dst'
        if not test_param(dst, env):
            print "No", dst, "attribute found. Moving on..."
            continue
        vdst = get_single_param(dst, env)
        if vdst == None:
            msg = "ERROR: Can't get '%s'. Quitting..." % (vdst)
            raise IOError(msg)

        # Create destination directory if not exist
        vdst = os.path.abspath(vdst.strip())
        # Check if this path is inside webapp's own root
        # Note that $PWD is currently this webapp's WEB-INF
        this_webapp = os.path.join(env['PWD'], '..')
        this_webapp = os.path.abspath(this_webapp)
        if not vdst.startswith(this_webapp):
            raise IOError("ERROR:", dst, "falls outside of this webapp directory.")

        print "mkdir -p", vdst
        call(['mkdir', '-p', vdst], env=env)
            
        # Find source locations
        vsrc_loc = vsrc_loc.strip()
        if not env.has_key (vsrc_loc):
            raise IOError("ERROR: There's no such env variable named", vsrc_loc)
            
        src_root = env[vsrc_loc]

        for s in vsrc:
            s = s.strip()
            # First, establish version
            ver = "%s.%s.version" % (d, s)
            if test_param (ver, env):
                vver = get_single_param(ver, env)
                if vver == None:
                    msg = "ERROR: Can't get '%s'. Quitting..." % (vver)
                    raise IOError(msg)
            else:
                default_versions = None
                if vsrc_loc == 'V2EXT_JS':
                    default_versions = canonical_version.JS_DEFAULT
                
                # default_versions can be done when the source
                # is internal JPL code
                if vsrc_loc == 'JSLIB':
                    vver = '.' # '.' signify current directory
                elif default_versions == None:
                    raise IOError("ERROR: No canonical versions database found for ",vsrc_log)
                else:
                    try:
                        vver = default_versions[s]
                    except KeyError:
                        print "ERROR: No version supplied for", s,
                        print ", and no canonical version found either.",
                        print "Quitting..."
                        raise 
                    vver = 'v' + vver

            # Next, establish files to copy
            src_files = os.path.join(src_root, s, vver, '*')

            cp_cmd = "cp -r %s %s" % (src_files, vdst)
            print cp_cmd
            # I can use shell=True here without worrying about
            # security, because I construct the directory, and
            # add the wildcard character (*) myself.
            call(cp_cmd, env=env, shell=True)

def build_wars_dir(webapp):
    print "Building", webapp

    env = os.environ
    V2TOP = env['V2TOP']
    V2HTML = env['V2HTML']

    web_inf = os.path.join(V2TOP, 'webapp', 'build', webapp, 'WEB-INF')

    # Clean up existing war
    war = os.path.join(V2HTML, 'wars', webapp + '.war')
    call(['rm', '-rf', war])

    # Added a check for existence of a WEB-INF directory.
    # WEB-INF is only applicable to Java based web apps.
    # In the event that the webapp is not Java based,
    # skip all this logic. 
    if os.path.exists(web_inf):
        print "cd", web_inf
        os.chdir(web_inf)
        # Since os.environ is not modified after os module is imported,
        # and we need to use the env object while calling a number of
        # subprocesses, we better have this variable reflects the 
        # current directory correctly.
        env['PWD'] = os.getcwd()

        # Not sure why we have to set up classes,
        # but that's the way it's been done so far.
        classes = os.path.join(web_inf, 'classes')
        print "mkdir -p", classes
        call(['mkdir', '-p', classes]) 

        war_build = os.path.join(web_inf, 'war.build')
        if os.path.isfile(war_build):
            try: 
               print "Set V2PARAM_FILE to", war_build
               env['V2PARAM_FILE'] = war_build

               copy_jars(env)
               copy_optional_jars(env)
               process_includes(env)

               # cd to the webapp's root directory
               os.chdir('..')
               env['PWD'] = os.getcwd()
               call(['jar', 'cvf', war, '.'])
            except Exception, e:
               print 'ERROR building webapp',webapp,':',str(e)
    
    os.chdir(V2TOP)
    env['PWD'] = os.getcwd()

def build_wars():
    env = os.environ
    V2HTML = env['V2HTML']
    V2TOP = env['V2TOP']

    wars = os.path.join(V2HTML, 'wars')
    if (not os.path.exists(wars)):
        os.mkdir(wars)

    webapp_root = os.path.join(V2TOP, 'webapp')
    print "cd", webapp_root
    os.chdir(webapp_root)
    env['PWD'] = os.getcwd()
    # Clear out the build directory
    print "rm -rf build"
    call(['rm', '-rf', 'build'], env=env)
    print "mkdir build"
    call(['mkdir', 'build'], env=env)
   
    # Copy the source to the build area
    webapp_src = os.path.join('src', '*')
    cp_cmd = "cp -r %s %s" % (webapp_src, 'build')
    print cp_cmd
    call(cp_cmd, env=env, shell=True)

    # Iteratively create lib and classes dir for each web_app_name
    print "cd build"
    os.chdir('build')
    env['PWD'] = os.getcwd()
    for webapp in os.listdir(os.getcwd()):
        build_wars_dir(webapp)

    os.chdir(V2TOP)
    # env is just a short name, we actually modify os.environ
    # directly, so we better have it reflects $PWD correctly.
    env['PWD'] = os.getcwd()

def build_js ():
    print "Building JavaScript"

    env = os.environ
    V2JS = env['V2JS']
    JSLIB = env['JSLIB']
    src = os.path.join(V2JS, 'src')
    if not os.path.exists(src):
        print "ERROR", src, "does not exist. Quitting..."
        sys.exit(1)

    # Blow away old build
    print "rm -rf", JSLIB
    call (['rm', '-rf', JSLIB])
    # The build process is simply a copy
    print "cp -R", src, JSLIB
    call (['cp', '-R', src, JSLIB])

if __name__ == '__main__':
    print sys.argv[0], "version", __version__

    try:
        build_js()
        build_wars()
    except KeyError:
        print "Please make sure that you have choose a select first."


