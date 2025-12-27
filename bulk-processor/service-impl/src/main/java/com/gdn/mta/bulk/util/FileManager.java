package com.gdn.mta.bulk.util;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Comparator;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class FileManager {
  private static final Logger LOGGER = LoggerFactory.getLogger(FileManager.class);

  private FileManager() {
  }

  /**
   * Delete a directory and files within
   * 
   * @param dirPath
   */
  public static void deleteDirectory(String dirPath) {
    try {
      File parentDir = new File(dirPath);
      Path parentDirPath = Paths.get(parentDir.getAbsolutePath());

      Files.walk(parentDirPath).sorted(Comparator.reverseOrder()).map(Path::toFile)
          .forEach(File::delete);
    } catch (IOException e) {
      LOGGER.error("Error delete dir : {}", dirPath, e);
    }
  }

  public static void createFile(byte[] file, String path) throws IOException {
    File image = new File(path);
    if (!image.getParentFile().exists()) {
      image.getParentFile().mkdir();
    }
    try (FileOutputStream fileOutputStream = new FileOutputStream(image)) {
      fileOutputStream.write(file);
    }
  }
}
