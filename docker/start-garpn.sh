#!/bin/bash
#
# Start script for the PlWeb docker
#
# This script is started in /srv/cliopatria.

start=--no-fork
udaemon=daemon
cd /garpn

date "+%s" > /var/run/epoch

if [ -t 0 ] ; then
  start=--interactive
  exec swipl run.pl --user=$udaemon --port=3510 $start
fi

case $1 in
    --bash)
	bash
	exit 0
	;;
esac

## Make the server stop on signals sent from the health.sh.  Process
## 1 only accepts signals for which there is an installed signal
## handler.  We cannot install a signal handler for SIGKILL and
## therefore forcefully killing that even works in the case of
## deadlocks does not work.   We run the server in another pid
## to work around this issue.

stop()
{ echo "signal = $1; child = $child_pid"

  kill -s $1 $child_pid
  timeout 10 tail --pid=$child_pid -f /dev/null
  if [ $? == 124 ]; then
      echo "Gracefull termination failed.  Killing"
      kill -s KILL $child_pid
  fi

  exit 1
}

hangup()
{ echo "child = $child_pid"
  kill -s HUP $child_pid
}

trap "stop TERM" SIGTERM
trap "stop QUIT" SIGQUIT
trap "hangup" SIGHUP

cd /garpn
echo swipl run.pl --user=$udaemon --port=3510 $start &
swipl run.pl --user=$udaemon --port=3510 $start &
child_pid=$!

stat=129
while [ $stat = 129 ]; do
  wait -f $child_pid
  stat=$?
done
