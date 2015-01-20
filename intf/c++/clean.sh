#!/bin/bash
scons -c -f SConstruct.main
scons -c -f SConstruct.lib
rm -rf .sconsign.dblite
