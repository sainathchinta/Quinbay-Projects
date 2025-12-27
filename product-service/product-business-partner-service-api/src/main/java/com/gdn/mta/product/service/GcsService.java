package com.gdn.mta.product.service;

import com.google.cloud.storage.Blob;
import com.google.cloud.storage.Bucket;

public interface GcsService {

  /**
   * upload source image file to a location
   * @param bucket
   * @param destinationByteFile
   * @param filePath
   * @return
   * @throws Exception
   */
  Blob uploadFile(Bucket bucket, String filePath, byte[] destinationByteFile) throws Exception;

  /**
   * Accept a file path and return download blob content in bytes
   *
   * @param filePath
   * @param bucketName
   * @return
   */
  byte[] downloadFile(String bucketName, String filePath);

  /**
   * Check if the file is existing in the file path of not
   *
   * @param filePath
   * @param bucketName
   * @return
   */
  boolean isFileExists(String bucketName, String filePath);

  /**
   * delete source image blob
   * @param filePath
   * @param bucketName
   * @return
   */
  boolean deleteFile(String bucketName, String filePath);
}
