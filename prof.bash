#!/bin/bash

# -K100M -H100M

bindir="$(dirname "$0")/../cabal-dev/bin"

case $1 in
   "thread")
      shift
      "$bindir"/unfav-threadscope -f "my.token" +RTS -N -ls -sstderr $@ \
      && exec "$bindir"/threadscope unfav-threadscope.eventlog
      ;;

#   "heap")
#      # after cabal-dev with flag --enable-executable-profiling or --enable-library-profiling
#      shift
#      echo "just echoing..."
#      echo ./dist/build/unfav/unfav -f "my.token" +RTS -N -sstderr -p $@ \
#      && echo hp2ps -e8in -d -c unfav.hp \
#      && echo evince unfav.ps &
#      ;;

   *)
      echo "Usage: $(basename "$0") thread [...]"
#      echo "   or: $(basename "$0") heap [-hy | -hc | -hd | ... ]"
      ;;
esac

