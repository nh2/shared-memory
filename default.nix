{ mkDerivation, base, bytestring, stdenv, unix }:
mkDerivation {
  pname = "shared-memory";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base unix ];
  testHaskellDepends = [ base bytestring unix ];
  homepage = "https://github.com/nh2/shared-memory";
  description = "POSIX shared memory";
  license = stdenv.lib.licenses.mit;
}
