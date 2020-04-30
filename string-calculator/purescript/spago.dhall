{ name = "my-project"
, dependencies = [ "console", "effect", "psci-support", "spec", "debug" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
