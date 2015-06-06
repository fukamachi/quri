#!/bin/sh

if [ "$COVERALLS" ]; then
  cl -l prove -l cl-coveralls
     -e '(or (coveralls:with-coveralls (:exclude "t")
               (prove:run :quri-test))
             (uiop:quit -1))'
else
  cl -l prove -e '(or (prove:run :quri-test) (uiop:quit -1))'
fi
