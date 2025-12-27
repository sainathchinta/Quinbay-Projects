package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.pcu.external.properties.GCSProperties;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.BlobInfo;
import com.google.cloud.storage.Bucket;
import com.google.cloud.storage.Storage;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.util.IOUtils;
import org.apache.tika.Tika;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Duration;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

public class GCSConfigImplTest {

  public static final String WEBP_CONTENT_TYPE = "image/webp";
  @InjectMocks
  private GCSServiceImpl gcsConfig;

  @Mock
  private Storage googleCloudStorage;

  @Mock
  private Blob blob;

  @Mock
  private Tika tika;

  @Mock
  private GCSProperties gcsProperties;

  @Mock
  private Bucket bucket;

  private static String gcsUnifiedTemplateDirectory = "/temp";
  private static String BUCKET_NAME = "BUCKET-NAME";

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(gcsConfig, "imageFormatsSupported", List.of("jpg", "jpeg", "png", "webp"));
  }

  @Test
  public void downloadSourceFileTest() {
    Mockito.when(googleCloudStorage.get(BUCKET_NAME, gcsUnifiedTemplateDirectory)).thenReturn(blob);
    gcsConfig.downloadFile(BUCKET_NAME, gcsUnifiedTemplateDirectory);
    Mockito.verify(googleCloudStorage).get(BUCKET_NAME, gcsUnifiedTemplateDirectory);
  }

  @Test
  public void downloadSourceFileEmptyFilePathTest() {
    Mockito.when(googleCloudStorage.get(BUCKET_NAME, StringUtils.EMPTY)).thenReturn(blob);
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> gcsConfig.downloadFile(BUCKET_NAME, StringUtils.EMPTY));
  }

  @Test
  public void uploadCreatedFileTest() throws Exception {
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    ReflectionTestUtils.setField(gcsConfig, "imageFormatsSupported", List.of("jpg", "jpeg", "png"));
    String contentType = Files.probeContentType(Paths.get(Arrays.toString(excelFile)));
    Mockito.when(bucket.create("/temp", excelFile, contentType)).thenReturn(blob);
    Blob blob = gcsConfig.uploadCreatedFile(bucket, gcsUnifiedTemplateDirectory, excelFile);
    assertNotNull(blob);
  }

  @Test
  public void uploadCreatedFileWebpTest() throws Exception {
    Path filePath = Paths.get("src/test/resources/images/input/pixel.webp");
    byte[] image = Files.readAllBytes(filePath);
    Mockito.when(tika.detect(image)).thenReturn(WEBP_CONTENT_TYPE);
    Mockito.when(bucket.create("/temp", image, WEBP_CONTENT_TYPE)).thenReturn(blob);
    Blob blob = gcsConfig.uploadCreatedFile(bucket, gcsUnifiedTemplateDirectory, image);
    assertNotNull(blob);
  }

  @Test
  public void uploadCreatedFileFailedTest() throws Exception {
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String contentType = Files.probeContentType(Paths.get(Arrays.toString(excelFile)));
    Mockito.when(bucket.create("/temp/BP-10001/general_template.xlsm", excelFile, contentType))
      .thenReturn(null);
    Blob blob = gcsConfig.uploadCreatedFile(bucket, gcsUnifiedTemplateDirectory, excelFile);
    assertNull(blob);
  }

  @Test
  public void generateSignedUrlTest() throws MalformedURLException {
    String filePath = "/test/path/file.txt";
    Duration duration = Duration.ofMinutes(15);
    String expectedSignedUrl =
      "https://storage.googleapis.com/test-bucket/test/path/file.txt?signature=xyz";

    Mockito.when(bucket.getName()).thenReturn("test-bucket");

    URL mockUrl = new URL(expectedSignedUrl);

    Mockito.when(
        googleCloudStorage.signUrl(Mockito.any(BlobInfo.class), Mockito.eq(duration.toMinutes()),
          Mockito.eq(TimeUnit.MINUTES), Mockito.any(Storage.SignUrlOption.class)))
      .thenReturn(mockUrl);

    String actualSignedUrl = gcsConfig.generateSignedUrl(bucket, filePath, duration);

    assertNotNull(actualSignedUrl);
    assertEquals(expectedSignedUrl, actualSignedUrl);
    Mockito.verify(googleCloudStorage)
      .signUrl(Mockito.any(BlobInfo.class), Mockito.eq(duration.toMinutes()),
        Mockito.eq(TimeUnit.MINUTES), Mockito.any(Storage.SignUrlOption.class));
  }

  @Test
  public void generateSignedUrlWithDifferentDurationTest() throws MalformedURLException {
    String filePath = "/test/path/file.txt";
    Duration duration = Duration.ofHours(1);
    String expectedSignedUrl =
      "https://storage.googleapis.com/test-bucket/test/path/file.txt?signature=xyz";

    Mockito.when(bucket.getName()).thenReturn("test-bucket");

    URL mockUrl = new URL(expectedSignedUrl);

    Mockito.when(
        googleCloudStorage.signUrl(Mockito.any(BlobInfo.class), Mockito.eq(duration.toMinutes()),
          Mockito.eq(TimeUnit.MINUTES), Mockito.any(Storage.SignUrlOption.class)))
      .thenReturn(mockUrl);

    String actualSignedUrl = gcsConfig.generateSignedUrl(bucket, filePath, duration);

    assertNotNull(actualSignedUrl);
    assertEquals(expectedSignedUrl, actualSignedUrl);
    Mockito.verify(googleCloudStorage)
      .signUrl(Mockito.any(BlobInfo.class), Mockito.eq(duration.toMinutes()),
        Mockito.eq(TimeUnit.MINUTES), Mockito.any(Storage.SignUrlOption.class));
  }

  private Map<String, String> getFiles() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(
      Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("Excel-template" + File.separator + "products-template-English.xls"))),
      "UTF-8");
    files.put("xlsx", excelData);
    return files;
  }

}