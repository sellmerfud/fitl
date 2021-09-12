### Building a release


1. Run: just setvers <vers>
   - Updates the version number in the build.sbt file
   - Updates the version number in the files: *source/other/fitl*, *src/other/fitl.cmd*
2. Reload the sbt configuration.
3. In sbt, use the `stage` command to build and stage the files to *target/fitl-<ver>*
4. Run: just package <vers>
   - Removes and .DS_Store files in the staged directories.
   - Creates zip file of *target/fitl-<vers>* folder
   - Copies zip file to *~/Dropbox/fitl*
5. Test that new package works
6. Update the version number and download link in the *README.md* file
