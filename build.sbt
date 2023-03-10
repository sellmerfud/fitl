import Path.FileMap
import java.nio.file.{ Files, Paths }
import java.nio.file.attribute.PosixFilePermissions

lazy val commonSettings = Seq(
  organization := "org.sellmerfud",
  version      := "1.0",
  scalaVersion := "2.13.10"
)

lazy val stage       = taskKey[Unit]("Create distribution zip file")
lazy val sourceOther = settingKey[File]("Other source file included in the package")


lazy val fitl = (project in file("."))
  .settings(
    commonSettings,
    name        := "Fire-in-the-Lake",
    description := "A scala implementation of the solo Tru'ng Bots for Fire in the Lake",
    scalacOptions       ++= Seq( "-deprecation", "-unchecked", "-feature" ),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1"
    ),
    sourceOther := sourceDirectory.value / "other",
    
    // Task to create the distribution zip file
    Compile / stage := {
      val log = streams.value.log
      (Compile / packageBin).value  // Depends on the package being built
      def rebaseTo(directory: File)(origfile: File): Option[File] = {
        val mapper: FileMap = Path.flat(directory)
        mapper(origfile)
      }
      //  Full classpath including our own built jar file
      val pkgDir  = target.value / s"fitl-${version.value}"
      val lib     = pkgDir / "lib"
      val zipfile = file(s"${pkgDir.getAbsolutePath}.zip")
      val jars    = (Compile / fullClasspathAsJars).value.files
      val others  = (sourceOther.value * "*").get
      val assets  = (others pair rebaseTo(pkgDir)) ++ (jars pair rebaseTo(lib))
      
      log.info(s"Staging to $pkgDir ...")
      IO.delete(pkgDir)
      IO.createDirectory(lib)
      IO.copy(assets, CopyOptions().withOverwrite(true))
      IO.setPermissions(pkgDir / "fitl", "rwxr-xr-x") // Make bash script executable
      // Create zip file
      (pkgDir ** ".DS_Store").get foreach IO.delete
      val zipEntries = (pkgDir ** "*").get map (f => (f, IO.relativize(target.value, f).get) )
      IO.zip(zipEntries, zipfile, None)
    }
  )
  







