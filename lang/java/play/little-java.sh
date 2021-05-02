#!/bin/bash

function run_java_for_this_book {
     
    # javac -verbose  -classpath .:$CLASSPATH $1   && \
    javac -classpath .:$CLASSPATH $1   && \
    java run_${1/.java/}
    
    # 执行的命令的例子是:
    # javac -classpath .:$CLASSPATH 2_methods_to_our_madness.java
    # java run_2_methods_to_our_madness
}

run_java_for_this_book $1

exit
