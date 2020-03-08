{ nixpkgs  ? import <nixpkgs> { config.allowBroken = true; }
}:
with nixpkgs;
let
  reflexPlatformSrc = fetchGit {
    url = https://github.com/reflex-frp/reflex-platform;
    rev = "1419505ec714ff5aa449917f2cf764a38e78a503";
  };

  reflexWidgetsSrc = fetchGit {
    url = https://github.com/atidot/reflex-widgets;
    rev = "ea488c0cc7dbcde6bc34d17280a01efeb5bf7c1b";
  };

  stratosphereSrc = fetchGit {
    url = https://github.com/Atidot/stratosphere;
    rev = "8981ed145a0e582981403fb7ee7e10d5be5de508";
  };

  reflex-platform = import reflexPlatformSrc {};
in
reflex-platform.project({ pkgs, ... }: {
  overrides = self: super: rec {
  };

  packages = {
    stratosphere      = stratosphereSrc;

    atidot-deployment        = ../../atidot-deployment;
    atidot-deployment-editor = ../../atidot-deployment-editor;


    reflex-utils      = reflexWidgetsSrc + "/reflex-utils";
    reflex-mdl        = reflexWidgetsSrc + "/reflex-mdl";
    reflex-chartjs    = reflexWidgetsSrc + "/reflex-chartjs";
    reflex-jsoneditor = reflexWidgetsSrc + "/reflex-jsoneditor";
    reflex-codemirror = reflexWidgetsSrc + "/reflex-codemirror";
    reflex-select2    = reflexWidgetsSrc + "/reflex-select2";
    reflex-jexcel     = reflexWidgetsSrc + "/reflex-jexcel";
    reflex-fileapi    = reflexWidgetsSrc + "/reflex-fileapi";
  };

  shells = {
    ghcjs = [ "atidot-deployment-editor"
            ];
  };
})

