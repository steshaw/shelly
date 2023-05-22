self: super: {
  world = with super.python3Packages; buildPythonApplication rec {
    name = "world-5.0.1";
    src = builtins.fetchGit {
      url = "https://gitlab.com/warsaw/world";
      ref = "refs/tags/5.0.1";
    };
    format = "pyproject";
    nativeBuildInputs = [
      pdm-pep517
    ];
    propagatedBuildInputs = [
      atpublic
    ];
  };
}
