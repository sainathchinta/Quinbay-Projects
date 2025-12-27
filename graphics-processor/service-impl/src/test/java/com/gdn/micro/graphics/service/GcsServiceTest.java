package com.gdn.micro.graphics.service;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.apache.poi.util.IOUtils;
import org.apache.tika.Tika;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.Bucket;
import com.google.cloud.storage.Storage;

public class GcsServiceTest {

  private static final String PATH = "/temp";
  private static final String BUCKET_NAME = "bucket-name";
  private static final String CONTENT_TYPE = "image/jpeg";

  @InjectMocks
  private GcsServiceImpl gcsService;

  @Mock
  private Storage googleCloudStorage;

  @Mock
  private Bucket sourceImageBucket;

  @Mock
  private Blob blob;

  @Mock
  private Tika tika;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(sourceImageBucket, blob, sourceImageBucket);
  }

  @Test
  void uploadCreatedFileTest() throws Exception {
    byte[] imageFile = getFile();
    Mockito.when(sourceImageBucket.create(PATH, imageFile, CONTENT_TYPE)).thenReturn(blob);
    Blob blob = gcsService.uploadFile(sourceImageBucket, PATH, imageFile);
    Mockito.verify(sourceImageBucket).create(PATH, imageFile, CONTENT_TYPE);
    Assertions.assertNotNull(blob);
  }

  @Test
  void uploadCreatedWebFileTest() throws Exception {
    String WEBP_CONTENT_TYPE = "image/webp";
    ReflectionTestUtils.setField(gcsService, "webpConversionEnabled", true);
    Path filePath = Paths.get("src/test/resources/Images/pixel.webp");
    byte[] imageFile = Files.readAllBytes(filePath);
    Mockito.when(tika.detect(imageFile)).thenReturn(WEBP_CONTENT_TYPE);
    Mockito.when(sourceImageBucket.create(PATH, imageFile, WEBP_CONTENT_TYPE)).thenReturn(blob);
    Blob blob = gcsService.uploadFile(sourceImageBucket, PATH, imageFile);
    Mockito.verify(sourceImageBucket).create(PATH, imageFile, WEBP_CONTENT_TYPE);
    Assertions.assertNotNull(blob);
  }

  @Test
  void uploadCreatedFileFailedTest() throws Exception {
    byte[] imageFile = getFile();
    Mockito.when(sourceImageBucket.create(PATH, imageFile, CONTENT_TYPE)).thenReturn(null);
    Blob result = gcsService.uploadFile(sourceImageBucket, PATH, imageFile);
    Mockito.verify(sourceImageBucket).create(PATH, imageFile, CONTENT_TYPE);
    Assertions.assertNull(result);
  }

  @Test
  void downloadSourceFileTest() {
    Mockito.when(googleCloudStorage.get(BUCKET_NAME, PATH)).thenReturn(blob);
    gcsService.downloadFile(BUCKET_NAME, PATH);
    Mockito.verify(googleCloudStorage).get(BUCKET_NAME, PATH);
    Mockito.verify(blob).getContent();
  }

  @Test
  void downloadSourceFileNullTest() {
    Mockito.when(googleCloudStorage.get(BUCKET_NAME, PATH)).thenReturn(null);
    try {
      gcsService.downloadFile(BUCKET_NAME, PATH);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(googleCloudStorage).get(BUCKET_NAME, PATH);
    }
  }

  @Test
  void downloadSourceFileToTest() {
    Mockito.when(googleCloudStorage.get(BUCKET_NAME, PATH)).thenReturn(blob);
    Mockito.doNothing().when(blob).downloadTo(Paths.get(PATH));
    gcsService.downloadFileTo(BUCKET_NAME, PATH, PATH);
    Mockito.verify(blob).downloadTo(Paths.get(PATH));
  }

  @Test
  void downloadSourceFileToNullTest() {
    Mockito.when(googleCloudStorage.get(BUCKET_NAME, PATH)).thenReturn(null);
    try {
      gcsService.downloadFileTo(BUCKET_NAME, PATH, PATH);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(googleCloudStorage).get(BUCKET_NAME, PATH);
    }
  }

  @Test
  void checkIfBlobExitsTest() {
    Mockito.when(googleCloudStorage.get(BUCKET_NAME, PATH)).thenReturn(blob);
    gcsService.isFileExists(BUCKET_NAME, PATH);
    Mockito.verify(googleCloudStorage).get(BUCKET_NAME, PATH);
  }

  @Test
  void checkIfBlobExitsFalseTest() {
    Mockito.when(googleCloudStorage.get(BUCKET_NAME, PATH)).thenReturn(null);
    gcsService.isFileExists(BUCKET_NAME, PATH);
    Mockito.verify(googleCloudStorage).get(BUCKET_NAME, PATH);
  }

  @Test
  void deleteSourceImageFileTest() {
    Mockito.when(googleCloudStorage.get(BUCKET_NAME, PATH)).thenReturn(blob);
    Mockito.when(blob.delete(Blob.BlobSourceOption.generationMatch())).thenReturn(true);
    boolean success = gcsService.deleteFile(BUCKET_NAME, PATH);
    Mockito.verify(googleCloudStorage).get(BUCKET_NAME, PATH);
    Mockito.verify(blob).delete(Blob.BlobSourceOption.generationMatch());
    Assertions.assertTrue(success);
  }

  @Test
  void deleteSourceImageFileErrorTest() {
    Mockito.when(googleCloudStorage.get(BUCKET_NAME, PATH)).thenReturn(null);
    boolean success = gcsService.deleteFile(BUCKET_NAME, PATH);
    Mockito.verify(googleCloudStorage).get(BUCKET_NAME, PATH);
    Assertions.assertFalse(success);
  }

  private byte[] getFile() throws Exception {
    return IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("Images/TestImage.jpg"));
  }

}
