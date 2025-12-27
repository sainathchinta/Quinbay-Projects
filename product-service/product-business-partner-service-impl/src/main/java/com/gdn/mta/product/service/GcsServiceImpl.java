package com.gdn.mta.product.service;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URLConnection;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
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

  @Autowired
  @Qualifier("googleCloudStorage")
  private Storage googleCloudStorage;

  @Override
  public Blob uploadFile(Bucket bucket, String filePath, byte[] destinationByteFile) throws Exception {
    log.info("Uploading source image file : {} to GCS ", filePath);
    Blob blob = bucket.create(filePath, destinationByteFile, getMimeType(destinationByteFile));
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
          String.format(ErrorMessages.GCS_SOURCE_IMAGE_FILE_NOT_FOUND, filePath));
    }
  }

  @Override
  public boolean isFileExists(String bucketName, String filePath) {
    log.info("Check file exists, path : {} ", filePath);
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
      mimeType = URLConnection.guessContentTypeFromStream(is);
    }
    return mimeType;
  }
}

