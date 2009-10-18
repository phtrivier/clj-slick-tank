CP=.:./bin:./lib/lwgl.jar:./lib/slick.jar:./lib/clojure.jar:./lib/native/linux/*:./lib/*
LIB_PATH=./lib/native/linux/

CLASS=clojure.lang.Script
ARG="./src/main/clj/tank.clj"

#echo java -cp ${CP} -Djava.library.path=${LIB_PATH} ${CLASS} ${ARG}
java -cp ${CP} -Djava.library.path=${LIB_PATH} ${CLASS} ${ARG}

