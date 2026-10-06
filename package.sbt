addCommandAlias("makeZip", "show cli / Universal / packageBin")
addCommandAlias("dockerPublish", "cli / Docker / publish")

lazy val localInstallHome =
  settingKey[File]("Home directory for local installation")
lazy val installLocal =
  taskKey[File]("Build and install the CLI for the current user")

localInstallHome := file(sys.props("user.home"))
installLocal / aggregate := false
installLocal := LocalInstall.install(
  (LocalProject("cli") / Universal / stage).value,
  localInstallHome.value,
  (LocalProject("cli") / executableScriptName).value,
  streams.value.log
)
