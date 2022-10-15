#!/bin/sh
# https://www.cyberciti.biz/tips/howto-lighttpd-create-self-signed-ssl-certificates.html
# https://redmine.lighttpd.net/projects/lighttpd/wiki/Docs_SSL
# https://redmine.lighttpd.net/projects/1/wiki/HowToBasicAuth

set -ex
root_path="${PWD}"


if [ ! -e "${root_path}/cl-i.asd" ]
then
    echo "This script must be run from the root of the project." >&2
    exit 1
fi

testing_path="${root_path}/lighttpd-environment"

cd "${testing_path}"

rm -rf lighttpd
mkdir -p lighttpd

# Make self-signed certificate
#openssl req -x509 -newkey rsa:4096 -keyout server.pem -out server.pem -days 365
cp server.pem lighttpd
config_file="${testing_path}/lighttpd/lighttpd.conf"
auth_file="${testing_path}/lighttpd/auth.conf"
cat > "${auth_file}" << AUTH
mode:code
AUTH

cat > "${config_file}" << CONF

var.confdir = "${testing_path}/lighttpd"
server.modules += ("mod_access", "mod_auth", "mod_authn_file", "mod_openssl")
server.username = "$(id -un)"
server.groupname = "$(id -gn)"
server.port = 8443
server.dir-listing = "enable"
ssl.engine = "enable"
ssl.pemfile = var.confdir + "/server.pem"
server.name = "djha-skin.me"
server.document-root = "${testing_path}/wwwroot"
auth.backend = "plain"
auth.backend.plain.userfile = "${auth_file}"
# insecure location; temporary; FIX to something better
auth.require = ( "/basic" => ("method" => "basic", "realm" => "djha-skin.me", "require" => "valid-user") )

\$HTTP["url"] =~ "^/token/" {
    \$REQUEST_HEADER["Authorization"] != "Bearer 600dc0de6077a10ada600ddea10fda7a" {
        url.access-deny = ( "" )
    }
}
CONF

rm -rf wwwroot
mkdir wwwroot
mkdir wwwroot/basic
echo "basic complete" > "wwwroot/basic/complete.txt"
mkdir wwwroot/token
echo "token complete" > "wwwroot/token/complete.txt"
mkdir wwwroot/noauth
echo "noauth complete" > "wwwroot/noauth/complete.txt"

lighttpd -f "${config_file}" -D
