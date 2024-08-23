import Lake
open Lake DSL

package "NumericalAnalysis" where
  -- add package configuration options here
  leanOptions := #[
    ⟨`autoImplicit, false⟩,
    ⟨`relaxedAutoImplicit, false⟩,
    ⟨`linter.missingDocs, true⟩
  ]

@[default_target]
lean_lib «NumericalAnalysis» where
  -- add library configuration options here
  globs := #[.submodules `NumericalAnalysis]
