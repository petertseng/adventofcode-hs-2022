yes=

if [ "$1" = "--yes" ]; then
  yes=1
  shift
fi

day=$1

if [ $# -eq 0 ]; then
  day=$(TZ=UTC date +%-d)
  echo "OK, we have to guess the day, let's guess $day???"
else
  shift
  if [ "$1" = "--yes" ]; then
    yes=1
    shift
  fi
fi

daypad=$(seq -f %02g $day $day)

if [ "$yes" = "1" ]; then
  for f in $(ls secrets/*); do
    target=alt-input/$daypad$(basename $f)
    if [ -f $target ]; then
      echo "THE INPUT ALREADY EXISTS!!!"
    else
      curl --cookie session=$(cat $f) --user-agent 'https://github.com/petertseng/adventofcode-hs-2022/blob/master/fetch.sh' -o $target https://adventofcode.com/2022/day/$day/input
      sleep 1
    fi
  done
else
  curl -o input$day http://example.com
fi
