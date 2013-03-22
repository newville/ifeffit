#!/bin/sh
# set environment for using Ifeffit / iXAFS.app codes

get_abspath() {
    me=${BASH_SOURCE[0]%/}
    if [[ ! $me == */* ]]; then
	me="./"$me
    fi
    abspath=$(cd ${me%/*} && echo $PWD/${0##*/})
    echo 'ABSPATH ' $abspath
    abspath=`dirname $abspath`
    return 0
}

append_path() {
  if ! eval test -z "\"\${$1##*:$2:*}\"" -o -z "\"\${$1%%*:$2}\"" -o -z "\"\${$1##$2:*}\"" -o -z "\"\${$1##$2}\"" ; then
    eval "$1=\$$1:$2"
  fi
}

prepend_path()
{
  if ! eval test -z "\"\${$1##*:$2:*}\"" -o -z "\"\${$1%%*:$2}\"" -o -z "\"\${$1##$2:*}\"" -o -z "\"\${$1##$2}\"" ; then
    eval "$1=$2:\$$1"
  fi
}

get_abspath

local=`dirname $abspath`
echo 'Current Directory = ' $abspath
echo 'Parent Directory = ' $local
