package com.gdn.mta.product.service;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;

import org.apache.poi.util.IOUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.Bucket;
import com.google.cloud.storage.Storage;

public class GcsServiceTest {

  private static final String PATH = "/temp";
  private static final String BUCKET_NAME = "bucket-name";

  @InjectMocks
  private GcsServiceImpl gcsService;

  @Mock
  private Storage googleCloudStorage;

  @Mock
  private Bucket sourceImageBucket;

  @Mock
  private Blob blob;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(sourceImageBucket, blob, sourceImageBucket);
  }

  @Test
  public void uploadCreatedFileTest() throws Exception {
    byte[] imageFile = getFile();
    String contentType = Files.probeContentType(Paths.get(Arrays.toString(imageFile)));
    Mockito.when(sourceImageBucket.create(PATH, imageFile, contentType)).thenReturn(blob);
    Blob blob = gcsService.uploadFile(sourceImageBucket, PATH, imageFile);
    Mockito.verify(sourceImageBucket).create(PATH, imageFile, contentType);
  }

  @Test
  public void uploadCreatedFileFailedTest() throws Exception {
    byte[] imageFile = getFile();
    String contentType = Files.probeContentType(Paths.get(Arrays.toString(imageFile)));
    Mockito.when(sourceImageBucket.create(PATH, imageFile, contentType)).thenReturn(null);
    Blob result = gcsService.uploadFile(sourceImageBucket, PATH, imageFile);
    Mockito.verify(sourceImageBucket).create(PATH, imageFile, contentType);
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
    Mockito.when(googleCloudStorage.get(BUCKET_NAME, PATH)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        gcsService.downloadFile(BUCKET_NAME, PATH);
      });
    } finally {
      Mockito.verify(googleCloudStorage).get(BUCKET_NAME, PATH);
    }
  }

  @Test
  public void checkIfBlobExitsTest() {
    Mockito.when(googleCloudStorage.get(BUCKET_NAME, PATH)).thenReturn(blob);
    gcsService.isFileExists(BUCKET_NAME, PATH);
    Mockito.verify(googleCloudStorage).get(BUCKET_NAME, PATH);
  }

  @Test
  public void checkIfBlobExitsFalseTest() {
    Mockito.when(googleCloudStorage.get(BUCKET_NAME, PATH)).thenReturn(null);
    gcsService.isFileExists(BUCKET_NAME, PATH);
    Mockito.verify(googleCloudStorage).get(BUCKET_NAME, PATH);
  }

  @Test
  public void deleteSourceImageFileTest() {
    Mockito.when(googleCloudStorage.get(BUCKET_NAME, PATH)).thenReturn(blob);
    Mockito.when(blob.delete(Blob.BlobSourceOption.generationMatch())).thenReturn(true);
    boolean success = gcsService.deleteFile(BUCKET_NAME, PATH);
    Mockito.verify(googleCloudStorage).get(BUCKET_NAME, PATH);
    Mockito.verify(blob).delete(Blob.BlobSourceOption.generationMatch());
    Assertions.assertTrue(success);
  }

  @Test
  public void deleteSourceImageFileErrorTest() {
    Mockito.when(googleCloudStorage.get(BUCKET_NAME, PATH)).thenReturn(null);
    boolean success = gcsService.deleteFile(BUCKET_NAME, PATH);
    Mockito.verify(googleCloudStorage).get(BUCKET_NAME, PATH);
    Assertions.assertFalse(success);
  }

  private byte[] getFile() throws Exception {
    return IOUtils.toByteArray(Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("resize/nike_test_product_and_item_images_full01_hhp7330d.jpg"));
  }

}
