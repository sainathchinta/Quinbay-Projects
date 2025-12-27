package com.gdn.partners.pcu.external.service;

import com.google.cloud.storage.Blob;
import com.google.cloud.storage.Bucket;

import java.time.Duration;

public interface GCSService {

  /**
   *
   * @param bucketName
   * @param fileDownloadPath
   * @return
   * @throws Exception
   */
  Blob downloadFile(String bucketName, String fileDownloadPath) throws Exception;

  /**
   *
   * @param destinationByteFile
   * @param filePath
   * @return
   * @throws Exception
   */
  Blob uploadCreatedFile(Bucket bucket, String filePath, byte[] destinationByteFile) throws Exception;

    /**
     * Generates a signed URL for uploading a file to Google Cloud Storage.
     *
     * @param bucket The GCS bucket where the file will be uploaded.
     * @param filePath The path in the bucket where the file will be stored.
     * @param durationMinutes The duration for which the signed URL will be valid.
     * @return A SignedUrlResponse containing the signed URL and other metadata.
     */
    String generateSignedUrl(Bucket bucket, String filePath,
      Duration durationMinutes);
}