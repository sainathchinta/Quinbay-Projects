package com.gdn.x.mta.distributiontask.service.impl;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;

import org.apache.commons.io.IOUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.google.cloud.storage.Blob;
import com.google.cloud.storage.Bucket;
import com.google.cloud.storage.Storage;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
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

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(sourceImageBucket, blob, sourceImageBucket);
  }

  @Test
   void uploadCreatedFileTest() throws Exception {
    byte[] imageFile = getFile();
    String contentType = Files.probeContentType(Paths.get(Arrays.toString(imageFile)));
    Mockito.when(sourceImageBucket.create(PATH, imageFile, contentType)).thenReturn(blob);
    Blob blob = gcsService.uploadFile(sourceImageBucket, PATH, imageFile);
    Mockito.verify(sourceImageBucket).create(PATH, imageFile, contentType);
    Assertions.assertNotNull(blob);
  }

  @Test
   void uploadCreatedFileFailedTest() throws Exception {
    byte[] imageFile = getFile();
    String contentType = Files.probeContentType(Paths.get(Arrays.toString(imageFile)));
    Mockito.when(sourceImageBucket.create(PATH, imageFile, contentType)).thenReturn(null);
    Blob result = gcsService.uploadFile(sourceImageBucket, PATH, imageFile);
    Mockito.verify(sourceImageBucket).create(PATH, imageFile, contentType);
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
      Assertions.assertThrows(Exception.class,
        () -> gcsService.downloadFile(BUCKET_NAME, PATH));
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
        .getResourceAsStream("resize/nike_test_product_and_item_images_full01_hhp7330d.jpg"));
  }

}

