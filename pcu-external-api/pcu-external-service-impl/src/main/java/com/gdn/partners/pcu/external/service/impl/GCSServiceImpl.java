package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.service.GCSService;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.BlobInfo;
import com.google.cloud.storage.Bucket;
import com.google.cloud.storage.HttpMethod;
import com.google.cloud.storage.Storage;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.tika.Tika;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URLConnection;
import java.time.Duration;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

@Slf4j
@Component
public class GCSServiceImpl implements GCSService {

  private static final Tika TIKA = new Tika();

  @Autowired
  private Storage googleCloudStorage;

  @Value("${image.formats.supported}")
  private List<String> imageFormatsSupported;

  @Override
  public Blob downloadFile(String bucketName, String fileDownloadPath) {
    if (StringUtils.isNotBlank(fileDownloadPath)) {
      log.info("Downloading file from path : {}",fileDownloadPath);
      return googleCloudStorage.get(bucketName, fileDownloadPath);
    } else {
      log.error("Empty path to download file! ");
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_FORMAT,
        ErrorMessages.ERROR_GETTING_DATA_FROM_GCS);
    }
  }

  @Override
  public Blob uploadCreatedFile(Bucket bucket, String filePath, byte[] destinationByteFile) throws Exception {
    log.info("Uploading file : {} to GCS!!!", filePath);
    Blob blob =
        bucket.create(filePath, destinationByteFile, getMimeType(destinationByteFile, filePath));
    if (Objects.nonNull(blob)) {
      log.info("File: {} successfully uploaded to GCS!!!", filePath);
    }
    return blob;
  }

  private String getMimeType(byte[] destinationByteFile, String filepath) throws IOException {
    String mimeType = null;
    try (InputStream is = new BufferedInputStream(new ByteArrayInputStream(destinationByteFile))) {
      if (imageFormatsSupported.contains(Constants.WEBP_FORMAT)) {
        mimeType = TIKA.detect(destinationByteFile);
      } else {
        mimeType = URLConnection.guessContentTypeFromStream(is);
      }
    }
    log.info("Detected mime type: {} for file: {} ", mimeType, filepath);
    return mimeType;
  }

  @Override
  public String generateSignedUrl(Bucket bucket, String filePath, Duration durationMinutes) {
    BlobInfo blobInfo = BlobInfo.newBuilder(bucket, filePath).build();
    return googleCloudStorage.signUrl(blobInfo, durationMinutes.toMinutes(), TimeUnit.MINUTES,
        Storage.SignUrlOption.httpMethod(HttpMethod.PUT)).toString();
  }
}