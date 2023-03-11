#/bin/sh
export RLWRAP_HOME=${PWD}/.repl
export C_INCLUDE_PATH=/usr/include
export LIBRARY_PATH=/usr/lib64:/usr/lib
export CL_REGISTRY


#export CL_SOURCE_REGISTRY="C:/Users/bhw/quicklisp/dists/quicklisp/software/qlot-20220331/git/;C:/Users/bhw/Code/lisp/cl-i;C:/Users/bhw/Code/lisp;C:/Users/bhw/Code/lisp/asdf"

#set QUICKLISP_HOME="C:/Users/bhw/Code/lisp/cl-i/.qlot"

clpm bundle exec -- "C:\Program Files (x86)\ecl\ecl.exe" -load tools/preload.lisp

#rlwrap \
#    --ansi-colour-aware \
#    --no-children \
#    --forget-matching '[^)] *$' \
#    --multi-line='  ' \
#    --break-chars "(){}[],'#\";|\\" \
#    --case-insensitive \
#    --file tools/edi-weitz.txt \
#    --history-filename ~/.cl-history \
#    --forget-matching '^[^(]' \
#    -M '.lisp' \
#    --only-cook '^[]*>0-9]+ ' \
#    --prompt-colour=red \
#    --quote-characters "\"" \
#    --remember \
#qlot exec ros run -S . -- --load tools/preload.lisp
#sbcl --load tools/preload.lisp