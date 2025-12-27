package com.gdn.micro.graphics.service;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URLConnection;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Objects;

import org.apache.tika.Tika;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.Bucket;
import com.google.cloud.storage.Storage;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class GcsServiceImpl implements GcsService {
  private static final String GCS_SOURCE_IMAGE_FILE_NOT_FOUND = "Gcs source image file not found. filePath : %s ";
  private static final Tika TIKA = new Tika();

  @Autowired
  @Qualifier("googleCloudStorage")
  private Storage googleCloudStorage;

  @Value("${webp.conversion.enabled}")
  private boolean webpConversionEnabled;

  @Override
  public Blob uploadFile(Bucket bucket, String filePath, byte[] destinationByteFile) throws Exception {
    String mimeType = getMimeType(destinationByteFile);
    log.info("Uploading source image file : {} to GCS with mimeType {} ", filePath, mimeType);
    Blob blob = bucket.create(filePath, destinationByteFile, mimeType);
    if (Objects.nonNull(blob)) {
      log.info("File: {} successfully uploaded to GCS!!! ", filePath);
    }
    return blob;
  }

  @Override
  public byte[] downloadFile(String bucketName, String filePath) {
    log.info("Downloading source image file from GCS : {} ", filePath);
    Blob blob = googleCloudStorage.get(bucketName, filePath);
    if (Objects.nonNull(blob)) {
      return blob.getContent();
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          String.format(GCS_SOURCE_IMAGE_FILE_NOT_FOUND, filePath));
    }
  }

  @Override
  public void downloadFileTo(String bucketName, String gcsFilePath, String fileStorePath) {
    log.info("Downloading source image file from GCS : {} and store to filestore : {} ", gcsFilePath, fileStorePath);
    Blob blob = googleCloudStorage.get(bucketName, gcsFilePath);
    if (Objects.nonNull(blob)) {
      blob.downloadTo(Paths.get(fileStorePath));
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          String.format(GCS_SOURCE_IMAGE_FILE_NOT_FOUND, gcsFilePath));
    }
  }

  @Override
  public boolean isFileExists(String bucketName, String filePath) {
    Blob blob = googleCloudStorage.get(bucketName, filePath);
    return Objects.nonNull(blob);
  }

  @Override
  public boolean deleteFile(String bucketName, String filePath) {
    Blob blob = googleCloudStorage.get(bucketName, filePath);
    if (Objects.nonNull(blob)) {
      return blob.delete(Blob.BlobSourceOption.generationMatch());
    }
    log.error("File not found in filePath : {} ", filePath);
    return false;
  }

  private String getMimeType(byte[] destinationByteFile) throws IOException {
    String mimeType = null;
    try(InputStream is = new BufferedInputStream(new ByteArrayInputStream(destinationByteFile))){
      if (webpConversionEnabled) {
        mimeType = TIKA.detect(destinationByteFile);
      } else {
        mimeType = URLConnection.guessContentTypeFromStream(is);
      }
    }
    return mimeType;
  }
}

