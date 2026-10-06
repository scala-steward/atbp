import java.nio.file.Files
import java.nio.file.LinkOption.NOFOLLOW_LINKS
import sbt.*

object LocalInstall {
  def install(
      stage: File,
      home: File,
      scriptName: String,
      log: Logger
  ): File = {
    val installation =
      home.toPath.toAbsolutePath.normalize.resolve(".local/share/atbp")
    val commandLink =
      home.toPath.toAbsolutePath.normalize.resolve(".local/bin/atbp")
    val stagedLauncher = stage.toPath.resolve("bin").resolve(scriptName)
    val installedLauncher = installation.resolve("bin").resolve(scriptName)

    if (!Files.isRegularFile(stagedLauncher)) {
      sys.error(s"Staged launcher is missing: $stagedLauncher")
    }
    if (
      Files.exists(installation, NOFOLLOW_LINKS) &&
      (Files.isSymbolicLink(installation) || !Files.isDirectory(installation))
    ) {
      sys.error(
        s"Installation path must be a directory, not a symlink or file: $installation"
      )
    }
    if (
      Files.exists(commandLink, NOFOLLOW_LINKS) && !Files.isSymbolicLink(
        commandLink
      )
    ) {
      sys.error(s"Command path must be absent or a symlink: $commandLink")
    }

    IO.createDirectory(installation.getParent.toFile)
    IO.createDirectory(commandLink.getParent.toFile)
    IO.delete(installation.toFile)
    IO.copyDirectory(stage, installation.toFile, preserveExecutable = true)
    if (!installedLauncher.toFile.setExecutable(true, false)) {
      sys.error(
        s"Could not make installed launcher executable: $installedLauncher"
      )
    }
    Files.deleteIfExists(commandLink)
    Files.createSymbolicLink(commandLink, installedLauncher)
    log.info(s"Installed distribution: $installation")
    log.info(s"Installed command: $commandLink")
    installedLauncher.toFile
  }
}
