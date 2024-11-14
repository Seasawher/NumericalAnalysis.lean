import Lake
open Lake DSL

package "NumericalAnalysis" where
  -- add package configuration options here
  leanOptions := #[
    ⟨`autoImplicit, false⟩,
    ⟨`relaxedAutoImplicit, false⟩,
    ⟨`linter.missingDocs, true⟩
  ]

require proofwidgets from git "https://github.com/leanprover-community/ProofWidgets4" @ "v0.0.46"

@[default_target]
lean_lib «NumericalAnalysis» where
  -- add library configuration options here
  globs := #[.submodules `NumericalAnalysis]

lean_exe power_test where
  root := `NumericalAnalysis.Ch1.PowerAlgorithm
