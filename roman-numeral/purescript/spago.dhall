{ name = "my-project"
, dependencies = [ "console", "effect", "psci-support", "spec" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
