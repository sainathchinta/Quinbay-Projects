package com.gdn.partners.pcu.internal.service;


import com.google.cloud.storage.Bucket;

public interface GCSService {
  /**
   * @param bucket
   * @param filePath
   * @param destinationByteFile
   * @return
   * @throws Exception
   */
  String uploadCreatedFile(Bucket bucket, String filePath, byte[] destinationByteFile) throws Exception;


  /**
   *
   * @param bucketName
   * @param fileDownloadPath
   * @return
   */
  byte[] downloadFile(String bucketName, String fileDownloadPath);

  /**
   * delete source image blob
   * @param filePath
   * @param bucketName
   * @return
   */
  boolean deleteFile(String bucketName, String filePath);
}
