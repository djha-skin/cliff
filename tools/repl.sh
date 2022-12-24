#/bin/sh
#export RLWRAP_HOME=${PWD}/.repl
export C_INCLUDE_PATH=/usr/include
export LIBRARY_PATH=/usr/lib64:/usr/lib
rlwrap \
    --forget-matching '[^)] *$' \
    --multi-line='  ' \
    --break-chars "(){}[],'#\";|\\" \
    --case-insensitive \
    --file tools/edi-weitz.txt \
    --history-filename ~/.cl-history \
    --forget-matching '^[^(]' \
    -M '.lisp' \
    --only-cook '^[]*>0-9]+ ' \
    --prompt-colour=red \
    --quote-characters "\"" \
    --remember \
    qlot exec ros run -S . -- --load tools/preload.lisp
#sbcl --load tools/preload.lisp