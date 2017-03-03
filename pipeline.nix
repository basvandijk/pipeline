{ mkDerivation, base, stdenv, stm, stm-chans }:
mkDerivation {
  pname = "pipeline";
  version = "HEAD";
  src = ./.;
  libraryHaskellDepends = [ base stm stm-chans ];
  homepage = "https://github.com/basvandijk/pipeline";
  description = "A pipeline service";
  license = stdenv.lib.licenses.bsd3;
}
