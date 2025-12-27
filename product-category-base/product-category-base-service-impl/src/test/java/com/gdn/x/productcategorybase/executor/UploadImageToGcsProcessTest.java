package com.gdn.x.productcategorybase.executor;


import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.x.productcategorybase.service.FileStorageService;

public class UploadImageToGcsProcessTest {
  private static final String FILE_STORE_PATH = "fileStorePath";
  private static final String GCS_PATH = "gcsPath";
  private UploadImageToGcsProcess processor;

  @Mock
  private FileStorageService fileStorageService;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    processor = new UploadImageToGcsProcess(FILE_STORE_PATH, GCS_PATH, fileStorageService);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(fileStorageService);
  }

  @Test
  public void call() throws Exception {
    Mockito.when(fileStorageService.uploadFileFromGfsToGcs(FILE_STORE_PATH, GCS_PATH))
        .thenReturn(true);
    Boolean response = processor.call();
    Mockito.verify(fileStorageService)
        .uploadFileFromGfsToGcs(FILE_STORE_PATH, GCS_PATH);
    Assertions.assertTrue(response.booleanValue());
  }

}
