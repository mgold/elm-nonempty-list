#!/usr/bin/env bash

# based on elm-io.sh
read -d '' handler <<- EOF
(function(){
    if (typeof Elm === "undefined") { throw "elm-io config error: Elm is not defined. Make sure you call elm-io with a real Elm output file"}
    if (typeof Elm.Main === "undefined" ) { throw "Elm.Main is not defined, make sure your module is named Main." };
    var worker = Elm.worker(Elm.Main);
})();
EOF

elm make Test.elm --yes 2>/dev/null # This will error
elm make && # And this fixes it. Somehow.
elm make Test.elm --output=test.js &&
echo "$handler" >> test.js &&
node test.js
