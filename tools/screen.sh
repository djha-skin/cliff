#!/bin/sh

name="cl-i"

usage() {
    echo "Usage: tools/screen.sh <options>" >&2
    echo "  Options:" >&2
    echo "    [--vertical|--horizontal] (vertical by default)" >&2
    echo "    [--enable-split|--disable-split] (enabled by default)" >&2
    echo "    [--session-name <sn>] (\`cl-i\` by default)" >&2
    echo "    [--tab-name <nn>] (\`USERNAME\` by default)" >&2
    echo "  This must be run from the root of the project." >&2
    exit 1
}

if [ ! -e "${PWD}/${name}.asd" ]
then
    echo "This must be run from the root of the project." >&2
    usage
fi

root_path="${PWD}"

vertical=1
split=1
session_name="${name}"
tab_name="$(whoami)"

while [ -n "${1}" ]
do
    case "${1}" in
        --vertical)
            vertical=1
            shift
            ;;
        --horizontal)
            vertical=0
            shift
            ;;
        --enable-split)
            split=1
            shift
            ;;
        --disable-split)
            split=0
            shift
            ;;
        --session-name)
            shift
            session_name="${1}"
            shift
            ;;
        --tab-name)
            shift
            tab_name="${1}"
            shift
            ;;
        *)
            usage
            ;;
    esac
done

mkdir -p "${root_path}/.dev-cache"
setup_screen="${root_path}/.dev-cache/setup.screen"
cat > "${setup_screen}" <<SCREEN
hardstatus alwayslastline "%{=b}%{G} Screen(s): %{b}%w %=%{G}%C%A  "
caption always "%{= kC}%n (%t)"
#rendition so "= dd"
startup_message off
msgwait 1
bindkey -k k7 detach
bindkey -k k8 kill
bindkey -k k; screen
bindkey -k k9 title
bindkey -k F1 prev
bindkey -k F2 next
bind \" select
bind ( focus up
bind ) focus down
bind ; resize -4
bind \' resize +4
bind = resize =
bind [ copy
bind ] paste .
bind / eval "clear" "scrollback 0" "scrollback 5000"
bind \\ focus next
defscrollback 5000
bell off
vbell off
bell_msg ""
term screen-256color
shell -$SHELL
termcapinfo screen* ti@:te@
at 0 stuff "tools/repl.sh"
at 0 title "repl"
SCREEN

if [ "${split}" -ne 0 ]
then
    if [ "${vertical}" -ne 0 ]
    then
cat >> "${setup_screen}" <<SCREEN
split -v
focus
SCREEN
    else
cat >> "${setup_screen}" <<SCREEN
split
focus
SCREEN
    fi
fi

cat >> "${setup_screen}" <<SCREEN
screen -t "lighttpd" 1
at 1 stuff "./lighttpd-environment/lighttpd.exp"
screen -t "editor" 2

bindkey ^K at 0 stuff "(rove:run-test *)"
SCREEN

screen -dRR -S "${session_name}" -t "${tab_name}"