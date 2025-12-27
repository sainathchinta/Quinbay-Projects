package com.gdn.mta.bulk.util;

import java.io.File;

import org.junit.jupiter.api.Test;

public class FileManagerTest {

  private static final String TEST = "testString";
  private static final String PARENT_PATH = "parent";
  private static final String IMG_PATH = "CreateProductV2";
  private static final String TXT_PATH = "testString.txt";

  @Test
  public void createFileParentDidntExistCreateParentFolder() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    byte[] bytes = TEST.getBytes();
    String parentPath = classLoader.getResource(IMG_PATH).getPath() + File.separator + PARENT_PATH;
    String filePath = parentPath + File.separator + TXT_PATH;
    FileManager.createFile(bytes, filePath);
    FileManager.deleteDirectory(parentPath);
  }
}
