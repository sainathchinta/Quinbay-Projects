package com.gdn.mta.bulk.service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.models.GenericErrorMessages;
import com.google.api.gax.paging.Page;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.BlobId;
import com.google.cloud.storage.Bucket;
import com.google.cloud.storage.Storage;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URLConnection;
import java.nio.channels.Channels;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Collection;
import java.util.Objects;

@Service
@Slf4j
public class GCSServiceBean implements GCSService {

  @Autowired
  private Storage googleCloudStorage;

  @Override
  public Blob uploadCreatedFile(Bucket bucket, String filePath, byte[] destinationByteFile) throws Exception {
    log.info("File: {} being uploaded to GCS!!!", filePath);
    Blob blob = bucket.create(filePath, destinationByteFile,
        getMimeType(new ByteArrayInputStream(destinationByteFile)));
    if (Objects.nonNull(blob)) {
      log.info("File: {} successfully uploaded to GCS!!!", filePath);
    }
    return blob;
  }

  @Override
  public Blob uploadCreatedFileWithMimeType(Bucket bucket, String filePath, byte[] destinationByteFile, String mimeType)
      throws Exception {
    log.info("File: {} being uploaded to GCS!!!", filePath);
    Blob blob = bucket.create(filePath, destinationByteFile,
        StringUtils.isNotBlank(mimeType) ? mimeType : getMimeType(new ByteArrayInputStream(destinationByteFile)));
    if (Objects.nonNull(blob)) {
      log.info("File: {} successfully uploaded to GCS!!!", filePath);
    }
    return blob;
  }

  @Override
  public Blob uploadCreatedFileStream(Bucket bucket, String filePath, String mimeType,
      InputStream destinationByteFileStream) throws Exception {
    log.info("File: {} being uploaded to GCS!!!", filePath);
    Blob blob = bucket.create(filePath, destinationByteFileStream, mimeType);
    if (Objects.nonNull(blob)) {
      log.info("File: {} successfully uploaded to GCS!!!", filePath);
    }
    return blob;
  }

  @Override
  public byte[] downloadFile(String bucketName, String filePath) {
    log.info("Downloading file from GCS : {} ", filePath);
    Blob blob = googleCloudStorage.get(bucketName, filePath);
    if (Objects.nonNull(blob)) {
      return blob.getContent();
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
        GenericErrorMessages.GCS_FILE_NOT_FOUND + " file path {} : " + filePath);
    }
  }

  @Override
  public boolean isFileExists(String bucketName, String filePath) {
    Blob blob = googleCloudStorage.get(bucketName, filePath);
    return Objects.nonNull(blob);
  }

  @Override
  public Blob copyBlobInChunks(String bucketName, String sourcePath, String destinationPath) throws IOException {
    Blob blob = googleCloudStorage.get(bucketName, sourcePath);
    if (Objects.nonNull(blob)) {
      byte[] bytes = blob.getContent();
      String contentType = Files.probeContentType(Paths.get(Arrays.toString(bytes)));
      Bucket bucket = googleCloudStorage.get(bucketName, Storage.BucketGetOption.fields());
      return bucket.create(destinationPath, bytes, contentType);
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
        GenericErrorMessages.GCS_FILE_NOT_FOUND + " file path {} : " + sourcePath);
    }
  }

  private String getMimeType(InputStream destinationByteFile) throws IOException {
    String mimeType = null;
    try(InputStream is = new BufferedInputStream(destinationByteFile)) {
      mimeType = URLConnection.guessContentTypeFromStream(is);
    }
    return mimeType;
  }

  @Override
  public void deleteFilesByBlobIds(Collection<BlobId> blobsToDelete) {
    if (CollectionUtils.isNotEmpty(blobsToDelete)) {
      googleCloudStorage.delete(blobsToDelete);
    }
  }

  @Override
  public Page<Blob> listFilesAtDirectory(String bucketName, String directory) {
    return googleCloudStorage.list(bucketName, Storage.BlobListOption.prefix(directory));
  }

  @Override
  public InputStream openFileStream(String bucketName, String filePath) {
    log.info("Opening file stream from GCS : {}", filePath);
    Blob blob = googleCloudStorage.get(bucketName, filePath);
    if (Objects.isNull(blob)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
        GenericErrorMessages.GCS_FILE_NOT_FOUND + " file path : " + filePath);
    }
    return Channels.newInputStream(blob.reader());
  }
}
