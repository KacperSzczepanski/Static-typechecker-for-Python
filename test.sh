SUCCESSES_NUM=0
FAILURES_NUM=0

for test in `find tests -name '*.py'` ; do
    echo ""
    echo "Testing:" $test
    start=$SECONDS
    program=`./main $test`
    len=${#test}
    output="${test:0:(len-2)}out"
    answer="correct_outputs${test:5:(len-7)}out"

    if cmp -s $output $answer ; then
        echo -e "\e[92mSuccess.\e[39m"
        SUCCESSES_NUM=$(($SUCCESSES_NUM+1))
    else
        echo -e "\e[91mFailure!\e[39m\n"
        FAILURES_NUM=$(($FAILURES_NUM+1))
    fi
    echo time: $(( SECONDS - start )) seconds
done

TOTAL_NUM=$(($FAILURES_NUM+$SUCCESSES_NUM))
echo -e "\n$TOTAL_NUM tests in total, from which"
if (($FAILURES_NUM == 0)); then
   echo -e "\e[92m$SUCCESSES_NUM succeeded\e[39m and"
   echo -e "$FAILURES_NUM failed."
else
    echo -e "$SUCCESSES_NUM succeeded and"
    echo -e "\e[91m$FAILURES_NUM failed\e[39m."
fi
