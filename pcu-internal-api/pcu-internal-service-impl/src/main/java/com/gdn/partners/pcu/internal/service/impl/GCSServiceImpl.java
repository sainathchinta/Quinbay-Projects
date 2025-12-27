package com.gdn.partners.pcu.internal.service.impl;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URLConnection;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Objects;

import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.properties.ProductImageProperties;
import org.apache.tika.Tika;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.service.GCSService;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.Bucket;
import com.google.cloud.storage.Storage;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class GCSServiceImpl implements GCSService {

  @Autowired
  private Storage googleCloudStorage;

  @Autowired
  private ProductImageProperties productImageProperties;

  private static final Tika TIKA = new Tika();

  @Override
  public String uploadCreatedFile(Bucket bucket, String filePath, byte[] destinationByteFile) throws Exception {
    Blob blob = bucket.create(filePath, destinationByteFile, getMimeType(destinationByteFile));
    if (Objects.nonNull(blob)) {
      log.info("File: {} successfully uploaded to GCS!!!", filePath);
    }
    return filePath;
  }

  @Override
  public byte[] downloadFile(String bucketName, String fileDownloadPath) {
    log.info("Downloading source image file from GCS : {} ", fileDownloadPath);
    Blob blob = googleCloudStorage.get(bucketName, fileDownloadPath);
    if (Objects.nonNull(blob)) {
      return blob.getContent();
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          String.format(ErrorMessages.ERROR_GETTING_DATA_FROM_GCS, fileDownloadPath));
    }
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
    try (InputStream is = new BufferedInputStream(new ByteArrayInputStream(destinationByteFile))) {
      if (productImageProperties.getValidExtensionsToMagicNumbers().keySet()
          .contains(Constants.WEBP_FORMAT)) {
        mimeType = TIKA.detect(destinationByteFile);
      } else {
        mimeType = URLConnection.guessContentTypeFromStream(is);
      }
    }
    return mimeType;
  }

}
