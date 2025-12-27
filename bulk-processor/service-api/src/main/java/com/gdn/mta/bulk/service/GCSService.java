package com.gdn.mta.bulk.service;

import com.google.api.gax.paging.Page;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.BlobId;
import com.google.cloud.storage.Bucket;

import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;

public interface GCSService {

  /**
   *
   * @param destinationByteFile
   * @param filePath
   * @return
   * @throws Exception
   */
  Blob uploadCreatedFile(Bucket bucket, String filePath, byte[] destinationByteFile) throws Exception;

  /**
   *
   * @param destinationByteFile destinationByteFile
   * @param bucket bucket
   * @param filePath filePath
   * @return mimeType mimeType
   *
   * @throws Exception
   */

  Blob uploadCreatedFileWithMimeType(Bucket bucket, String filePath, byte[] destinationByteFile, String mimeType)
      throws Exception;

  /**
   * @param bucket
   * @param filePath
   * @param contentType
   * @param destinationByteFileStream
   * @return
   * @throws Exception
   */
  Blob uploadCreatedFileStream(Bucket bucket, String filePath, String contentType,
      InputStream destinationByteFileStream) throws Exception;

  /**
   * Accept a file path and return download blob content in bytes
   *
   * @param filePath
   * @return
   */
  byte[] downloadFile(String bucketName, String filePath);

  /**
   * Check if the file is existing in the file path of not
   *
   * @param filePath
   * @return
   */
  boolean isFileExists(String bucketName, String filePath);

  /**
   * Copy File from Source to Destination and return the blob
   *
   * @param sourcePath
   * @param destinationPath
   * @return
   */
  Blob copyBlobInChunks(String bucketName, String sourcePath, String destinationPath) throws IOException;

  /**
   * Deletes files by BlobId
   * @param blobsToDelete
   */
  void deleteFilesByBlobIds(Collection<BlobId> blobsToDelete);

  /**
   * @param bucketName
   * @param directory
   * @return Internally paginated list of files present in a given GCS bucket and directory.
   */
  Page<Blob> listFilesAtDirectory(String bucketName, String directory);

  /**
   * @param bucketName String
   * @param filePath   String
   * @return InputStream
   */
  InputStream openFileStream(String bucketName, String filePath);
}
