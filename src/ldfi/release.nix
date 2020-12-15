{ lib, pythonPackages, gitignoreSource }:

let
  z3-solver = pythonPackages.buildPythonPackage rec {
    pname = "z3-solver";
    version = "4.8.9.0";
    src = pythonPackages.fetchPypi {
      inherit pname version;
      sha256 = "0w2b917n5a3d7wj15xgb9h25f9s8g14f182i2zy371fkxda42kah";
    };
    doCheck = false;
  };
in

pythonPackages.buildPythonApplication rec {
  pname = "detsys-ldfi";
  version = "latest";
  src = gitignoreSource ./.;

  preBuild = ''
    export SETUPTOOLS_SCM_PRETEND_VERSION="${lib.commitIdFromGitRepo ./../../.git}"
  '';

  checkInputs = with pythonPackages; [ pytest pytestrunner ];
  propagatedBuildInputs = with pythonPackages; [ z3-solver setuptools setuptools_scm ];
}
