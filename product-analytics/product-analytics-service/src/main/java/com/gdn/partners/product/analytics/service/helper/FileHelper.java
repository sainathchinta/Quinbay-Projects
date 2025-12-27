package com.gdn.partners.product.analytics.service.helper;

import java.io.IOException;
import java.util.List;

public interface FileHelper {

  /**
   * Split the given file every noOfRecordsPerFile lines
   *
   * @param fileName
   * @param extension
   * @param noOfRecordsPerFile
   * @param localFilePathList
   * @return
   */
  List<String> splitFileIntoSmallFiles(String fileName, String extension, int noOfRecordsPerFile,
    List<String> localFilePathList);


  /**
   * Unzip file
   *
   * @param fileName
   * @param extension
   * @param storageBlobToLocalByBlobNamePrefix
   * @return
   */
  List<String> unZipFile(String fileName, String extension, List<String> storageBlobToLocalByBlobNamePrefix);

  /**
   * Delete directory
   *
   */
  void deleteFilesFromAutoQCDirectory() throws IOException;

  /**
   * delete all files of folder
   * @param bucketName
   * @param folderPath
   */
  int deleteDirectoryFromGcs(String bucketName, String folderPath);
}
