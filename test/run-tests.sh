#!/bin/sh

set -exv

stack build
ELMI_PATH="$(stack path --local-install-root)/bin/elmi-to-json"
rm -Rf tmp_test_workspace
mkdir -p tmp_test_workspace
cd tmp_test_workspace

yes | elm init
mkdir -p src
cat <<EOF > src/Main.elm
import Html
type alias X = String
main = Html.text ""
EOF
elm make src/Main.elm
"$ELMI_PATH"
