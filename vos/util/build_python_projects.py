#!/usr/bin/env python

# This is a standalone program to build all python
# projects under $V2PY/src. It accomplishes by
# create a virtual environment that is $V2PY/build;
# activates the environment and run pip install for
# all the Python projects.
#
# This script uses $PY_VIRTUALENV/virtualenv.py 
# script.
#
# Usage: $V2UTIL/build_python_projects.py
#
# Author: Haley Nguyen
# Email: Haley.Nguyen@jpl.nasa.gov

import os
import os.path
import sys
import stat
from subprocess import call
from subprocess import Popen
from subprocess import STDOUT
from subprocess import PIPE

__version__ = '1.0'

def build_python_projects():
    env = os.environ
    V2TOP = env['V2TOP']
    V2PY = env['V2PY']
    PY_VIRTUALENV = env['PY_VIRTUALENV']

    pysrc = os.path.join(V2PY, 'src')
    pybuild = os.path.join(V2PY, 'build')

    if not os.path.exists(pysrc):
        print "ERROR:", pysrc, "does not exist. Quitting..."
        sys.exit(1)

    # Clear out the build directory
    print "rm -rf", pybuild
    call(['rm', '-rf', pybuild], env=env)

    # The build of python project is a virtual
    # environment created by virtualenv tool.
    os.chdir(V2PY)
    env['PWD'] = V2PY
    virtualenv = os.path.join(PY_VIRTUALENV, 'virtualenv.py')
    cmd = "python %s --distribute --system-site-packages build" % (virtualenv)
    print cmd
    call(['python', virtualenv, '--distribute', '--system-site-packages', 'build'], env=env)
   
    # Activate this virtualenv, and install the projects.
    # Since activating this virtual environment means
    # sourcing of variable, it's best to execute the installation
    # as a script, so I'm going to generate a script file, and
    # execute it.
    script_file = os.path.join(env['PWD'], 'pip_install.sh')
    print "Generating temporary install script", script_file

    bash_script = open(script_file, 'w')
    bash_script.write('#!/bin/bash\n\n')
    bash_script.write('. ')
    bash_script.write(os.path.join(pybuild, 'bin', 'activate'))
    bash_script.write('\n\n')

    for project in os.listdir(pysrc):
        project_path = os.path.join(pysrc, project)
        bash_script.write('pip install ')
        bash_script.write(project_path)
        bash_script.write('\n')

    bash_script.close()

    print "cat", script_file
    call(['cat', script_file], env=env)
    print "chmod +x", script_file
    os.chmod(script_file, stat.S_IXUSR | stat.S_IRUSR | stat.S_IWUSR)

    print "Execute script"
    call([script_file], env=env)

    print "rm", script_file
    call(['rm', script_file], env=env)

    os.chdir(V2TOP)
    # env is just a short name, we actually modify os.environ
    # directly, so we better have it reflects $PWD correctly.
    env['PWD'] = os.getcwd()

if __name__ == '__main__':
    print sys.argv[0], "version", __version__

    try:
        build_python_projects()
    except KeyError:
        print "Please make sure that you have choose a select first."


