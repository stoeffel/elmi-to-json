var binwrap = require("binwrap");
var path = require("path");

var packageInfo = require(path.join(__dirname, "package.json"));
var version = packageInfo.version;

var root =
"https://github.com/stoeffel/elmi-to-json/releases/download/" +
  version +
  "/elmi-to-json-" +
  version;

module.exports = binwrap({
  dirname: __dirname,
  binaries: ["elmi-to-json"],
  urls: {
    "darwin-x64": root + "-osx.tar.gz",
    "linux-x64": root + "-linux.tar.gz",
    "win32-x64": root + "-win32-x64.zip",
    "win32-ia32": root + "-win32-ia32.zip"
  }
});
