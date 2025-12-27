package com.gdn.partners.pcu.internal.service.impl;

import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import com.gdn.partners.pcu.internal.properties.ProductImageProperties;
import org.apache.commons.codec.binary.Base64;
import org.apache.poi.util.IOUtils;
import org.apache.tika.Tika;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.Bucket;
import com.google.cloud.storage.Storage;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class GCSServiceImplTest {
  private static final String BUCKET_NAME = "BUCKET-NAME";
  private static final String PATH = "/temp";

  @InjectMocks
  private GCSServiceImpl gcsService;

  @Mock
  private Storage googleCloudStorage;

  @Mock
  private Bucket bucket;

  @Mock
  private Blob blob;

  @Mock
  private ProductImageProperties productImageProperties;

  @Mock
  private Tika tika;

  private Map<String, String> getFiles() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("ExcelTemplate" + File.separator + "Blibli-internal-bulk-template.xlsm"))), "UTF-8");
    files.put("xlsx", excelData);
    return files;
  }

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(googleCloudStorage);
    verifyNoMoreInteractions(bucket);
    verifyNoMoreInteractions(blob);
  }

  @Test
  public void uploadCreatedFileTest() throws Exception {
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String contentType = Files.probeContentType(Paths.get(Arrays.toString(excelFile)));
    Mockito.when(bucket.create("file-Path", excelFile, contentType)).thenReturn(blob);
    String filePath = gcsService.uploadCreatedFile(bucket,"file-Path", excelFile);
    Mockito.verify(bucket).create("file-Path", excelFile, contentType);
    Assertions.assertNotNull(filePath);
  }

  @Test
  public void uploadCreatedFileFailedTest() throws Exception {
    Map<String, String> files = this.getFiles();
    byte[] excelFile = Base64.decodeBase64(files.get("xlsx"));
    String contentType = Files.probeContentType(Paths.get(Arrays.toString(excelFile)));
    Mockito.when(bucket.create("file-Path", excelFile, contentType))
        .thenReturn(null);
    String filePath = gcsService.uploadCreatedFile(bucket, "file-Path", excelFile);
    Mockito.verify(bucket).create("file-Path", excelFile, contentType);
  }

  @Test
  public void uploadCreatedFileWebpMimeTypeTest() throws Exception {
    // Arrange
    byte[] webpBytes = Files.readAllBytes(Paths.get("src/test/resources/imageValidator/webp_test.webp"));

    // Mock productImageProperties to return a map containing "webp"
    Map<String, String> validExtensions = new HashMap<>();
    validExtensions.put(".webp", "52 49 46 46"); // mock magic number or any string
    Mockito.when(productImageProperties.getValidExtensionsToMagicNumbers())
        .thenReturn(validExtensions);

    // Mock TIKA detection
    Mockito.when(tika.detect(webpBytes)).thenReturn("image/webp");

    // Mock bucket create with detected content type
    Mockito.when(bucket.create("webp-file-path", webpBytes, "image/webp"))
        .thenReturn(blob);

    // Act
    String filePath = gcsService.uploadCreatedFile(bucket, "webp-file-path", webpBytes);

    // Assert
    Assertions.assertNotNull(filePath);
    Mockito.verify(bucket).create("webp-file-path", webpBytes, "image/webp");
  }


  @Test
  public void downloadSourceFileTest() {
    Mockito.when(googleCloudStorage.get(BUCKET_NAME, PATH)).thenReturn(blob);
    gcsService.downloadFile(BUCKET_NAME, PATH);
    Mockito.verify(googleCloudStorage).get(BUCKET_NAME, PATH);
    Mockito.verify(blob).getContent();
  }

  @Test
  public void downloadSourceFileNullTest() {
    try {
      gcsService.downloadFile(BUCKET_NAME, PATH);
    }
    catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    }
    finally {
      Mockito.verify(googleCloudStorage).get(BUCKET_NAME, PATH);
    }
  }

  @Test
  public void deleteFileFromGcsTest() {
    Mockito.when(googleCloudStorage.get(BUCKET_NAME, PATH)).thenReturn(blob);
    Mockito.when(blob.delete(Blob.BlobSourceOption.generationMatch())).thenReturn(true);
    boolean success = gcsService.deleteFile(BUCKET_NAME, PATH);
    Mockito.verify(googleCloudStorage).get(BUCKET_NAME, PATH);
    Mockito.verify(blob).delete(Blob.BlobSourceOption.generationMatch());
    Assertions.assertTrue(success);
  }

  @Test
  public void deleteFileFromGcsErrorTest() {
    Mockito.when(googleCloudStorage.get(BUCKET_NAME, PATH)).thenReturn(null);
    boolean success = gcsService.deleteFile(BUCKET_NAME, PATH);
    Mockito.verify(googleCloudStorage).get(BUCKET_NAME, PATH);
    Assertions.assertFalse(success);
  }
}
