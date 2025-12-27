package com.gdn.x.productcategorybase.service.impl;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.executor.UploadFinalImageToGcsProcess;
import com.gdn.x.productcategorybase.properties.GcsProperties;
import com.gdn.x.productcategorybase.service.GcsService;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.Bucket;
import com.google.common.collect.ImmutableSet;

public class FileStorageServiceImplTest {

  private static final String PRODUCT_CODE = "MTA-1100587";
  private static final String PATH_PREFIX = "catalog-image";
  private static final String SOURCE_IMAGE_DIRECTORY = "source-image";
  private static final String SOURCE_IMAGE_DIRECTORY_RESIZE = "source-image/resize";
  private static final String TEST_TEMP_FILE1 = "src/test/resources/source-image/catalog-image/MTA-1100587/download.jpeg";
  private static final String TEST_TEMP_FILE2 = "src/test/resources/filestore1/mta/images/source/MTA-1100587/download.jpeg";

  private static final String IMAGE_SOURCE_DIRECTORY_FILE_STORE = "src/test/resources/filestore/mta/images/source";
  private static final String FULL_IMAGE_SOURCE_DIRECTORY_FILE_STORE =
      "src/test/resources/filestore/wcsstore/Indraprastha/images/catalog/full";
  private static final String LOCATION_PATH_FILE_STORE = "/83/MTA-1100457/download.jpeg";
  private static final String LOCATION_PATH_FILE_STORE_2 = "MTA-1100587/download.jpeg";
  private static final String LOCATION_PATH_GCS = "/catalog-image/83/MTA-1100457/download.jpeg";

  private static final String INVALID_LOCATION_PATH_GCS = "/catalog-image/83/MTA-1100458/download"
    + ".jpeg";

  private static final String FILE_NAME = "fileName";
  private static final String FILE_NAME1 = "download.jpeg";
  private static final String SOURCE_FILE_1 =
      "src/test/resources/filestore1/mta/images/source/MTA-1100586/download.jpeg";

  private static Image image = new Image();
  private static final String SOURCE_DIRECTORY = "imageSourceDirectory";
  private static final String FULL_DIRECTORY = "fullImageDirectory";
  private static final String MEDIUM_DIRECTORY = "mediumImageDirectory";
  private static final String THUMBNAIL_DIRECTORY = "thumbnailImageDirectory";
  private static final String LOCATION_PATH = "/nike_test_product_and_item_images_full01_hhp7330d.jpg";
  private static final String CATALOG_NAME = "/catalog-image";
  private static final String FILE = "/sample.txt";
  private static final String FILE_PATH = "src/test/resources/filestr/sample.txt";
  private static final String BUCKET_NAME = "bucket-name";
  private static final String LOCATION_PATH_FULL = "/53/MTA-1100587/testFinal_full.jpg";
  private static final String LOCATION_PATH_MEDIUM = "/53/MTA-1100587/testFinal_medium.jpg";
  private static final String LOCATION_PATH_THUMBNAIL = "/53/MTA-1100587/testFinal_thumbnail.jpg";
  private static final String FULL_IMAGE_FINAL_DIRECTORY =
      "src/test/resources/filestore/wcsstore/Indraprastha/images/catalog/full";
  private static final String MEDIUM_IMAGE_FINAL_DIRECTORY =
      "src/test/resources/filestore/wcsstore/Indraprastha/images/catalog/medium";
  private static final String THUMBNAIL_IMAGE_FINAL_DIRECTORY =
      "src/test/resources/filestore/wcsstore/Indraprastha/images/catalog/thumbnail";
  private static final String TEST_FINAL_FULL_IMAGE =
      "src/test/resources/filestore/wcsstore/Indraprastha/images/catalog/full/53/MTA-1100587/testFinal_full.jpg";
  private static final String TEST_FINAL_MEDIUM_IMAGE =
      "src/test/resources/filestore/wcsstore/Indraprastha/images/catalog/medium/53/MTA-1100587/testFinal_full.jpg";
  private static final String TEST_FINAL_THUMBNAIL_IMAGE =
      "src/test/resources/filestore/wcsstore/Indraprastha/images/catalog/thumbnail/53/MTA-1100587/testFinal_full.jpg";
  private static final String FILE_STORE_RESIZE_PATH = "/MTA-10142142/resize/casingkpop_casing_hp_xiaomi_nct_dream_reload_case_full01_gb4h73p5.jpg";
  private static final String GCS_RESIZE_PATH = "/resize/catalog-image/MTA-10142142/casingkpop_casing_hp_xiaomi_nct_dream_reload_case_full01_gb4h73p5.jpg";
  private static final String FILE_STORE_PATH = "/MTA-10142142/casingkpop_casing_hp_xiaomi_nct_dream_reload_case_full01_gb4h73p5.jpg";
  private static final String GCS_PATH = "catalog-image/MTA-10142142/casingkpop_casing_hp_xiaomi_nct_dream_reload_case_full01_gb4h73p5.jpg";
  private static final String FILE_STORE_SOURCE_IMAGE_PATH = "src/test/resources/filestore/mta/images/source/MTA-1100587/download.jpeg";
  private static final String FILE_STORE_SOURCE_IMAGE_COPY_PATH = "copy/mta/images/source/MTA-1100587/download.jpeg";
  private static final String GCS_SOURCE_IMAGE_PATH = "src/test/resources/filestore/mta/images/source/MTA-1100587/download.jpeg";
  private static File file;

  @InjectMocks
  private FileStorageServiceImpl fileStorageService;

  @Mock
  private GcsProperties gcsProperties;

  @Mock
  private GcsService gcsService;

  @Mock
  private Bucket sourceImageBucket;

  @Mock
  private ExecutorService imageUploadExecutorService;

  @Mock
  private Blob blob;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(fileStorageService, "imageSourceDirectory","src/test/resources/filestr");
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn("final-image");
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn("full");
    Mockito.when(gcsProperties.getFinalMediumImageDirectory()).thenReturn("medium");
    Mockito.when(gcsProperties.getFinalThumbnailImageDirectory()).thenReturn("thumbnail");
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn("catalog-image");
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn("source-image");
    Mockito.when(gcsProperties.getFinalMediumImageDirectory()).thenReturn("medium");
    Mockito.when(gcsProperties.getFinalThumbnailImageDirectory()).thenReturn("thumbnail");
    Mockito.when(gcsProperties.getFinalImageBucketName()).thenReturn("finalImage");
    ReflectionTestUtils.setField(fileStorageService,"avoidDuplicateCallsToGcsForImageUploads",true);

  }


  @Test
  public void updateImageLocationGcsTest() throws Exception {
    image.setLocationPath(LOCATION_PATH_GCS);
    image.setActive(false);
    Set<String> uploadedImagePath = new HashSet<>();
    uploadedImagePath.add(LOCATION_PATH_GCS.concat("-1234"));
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(PATH_PREFIX);
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(SOURCE_IMAGE_DIRECTORY);
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn("merchant-qa1-static");
    Mockito.when(gcsProperties.getSourceResizeImageDirectory()).thenReturn(SOURCE_IMAGE_DIRECTORY_RESIZE);
    Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString())).thenReturn(FileUtils.readFileToByteArray(
        new File(SOURCE_FILE_1)));

    fileStorageService.updateLocationForImages(image, PRODUCT_CODE, IMAGE_SOURCE_DIRECTORY_FILE_STORE,
        FULL_IMAGE_SOURCE_DIRECTORY_FILE_STORE, uploadedImagePath, new AtomicInteger(0));

    Mockito.verify(gcsProperties, Mockito.times(4)).getPathPrefix();
    Mockito.verify(gcsProperties, Mockito.times(2)).isSourceImageEnabled();
    Mockito.verify(gcsProperties).getSourceImageBucketName();
    Mockito.verify(gcsService).downloadFile(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(gcsProperties, Mockito.times(2)).isSourceImageEnabled();
    Mockito.verify(gcsProperties, Mockito.times(2)).getSourceImageDirectory();
    Mockito.verify(gcsService)
        .uploadFile(sourceImageBucket, gcsProperties.getSourceImageDirectory() + File.separator + gcsProperties.getPathPrefix() + File.separator
            + PRODUCT_CODE + File.separator + FILE_NAME1, FileUtils.readFileToByteArray(
            new File(SOURCE_FILE_1)));
    deleteTestTempFile("MTA-1100587", TEST_TEMP_FILE2);
  }

  @Test
  public void updateImageLocationGcsSwitchOffTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageService,"avoidDuplicateCallsToGcsForImageUploads",
      false);
      image.setLocationPath(LOCATION_PATH_GCS);
    image.setActive(false);
    Set<String> uploadedImagePath = new HashSet<>();
    uploadedImagePath.add(LOCATION_PATH_GCS.concat("-1234"));
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(PATH_PREFIX);
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(SOURCE_IMAGE_DIRECTORY);
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn("merchant-qa1-static");
    Mockito.when(gcsProperties.getSourceResizeImageDirectory()).thenReturn(SOURCE_IMAGE_DIRECTORY_RESIZE);
    Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString())).thenReturn(FileUtils.readFileToByteArray(
      new File(SOURCE_FILE_1)));

    fileStorageService.updateLocationForImages(image, PRODUCT_CODE, IMAGE_SOURCE_DIRECTORY_FILE_STORE,
      FULL_IMAGE_SOURCE_DIRECTORY_FILE_STORE, uploadedImagePath, new AtomicInteger(0));

    Mockito.verify(gcsProperties, Mockito.times(4)).getPathPrefix();
    Mockito.verify(gcsProperties, Mockito.times(2)).isSourceImageEnabled();
    Mockito.verify(gcsProperties).getSourceImageBucketName();
    Mockito.verify(gcsService).downloadFile(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(gcsProperties, Mockito.times(2)).isSourceImageEnabled();
    Mockito.verify(gcsProperties, Mockito.times(2)).getSourceImageDirectory();
    Mockito.verify(gcsService)
      .uploadFile(Mockito.any(), Mockito.anyString(), Mockito.eq(FileUtils.readFileToByteArray(
        new File(SOURCE_FILE_1))));
    deleteTestTempFile("MTA-1100587", TEST_TEMP_FILE2);
  }

  @Test
  public void updateImageLocationGcsForDuplicateLocationTest() throws Exception {
    image.setLocationPath(LOCATION_PATH_FILE_STORE_2);
    image.setActive(false);
    Set<String> uploadedImagePath = new HashSet<>();
    uploadedImagePath.add("download.jpeg");
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(PATH_PREFIX);
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(SOURCE_IMAGE_DIRECTORY);
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn("merchant-qa1-static");
    Mockito.when(gcsProperties.getSourceResizeImageDirectory()).thenReturn(SOURCE_IMAGE_DIRECTORY_RESIZE);
    Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString())).thenReturn(FileUtils.readFileToByteArray(
      new File(SOURCE_FILE_1)));

    fileStorageService.updateLocationForImages(image, PRODUCT_CODE, IMAGE_SOURCE_DIRECTORY_FILE_STORE,
      FULL_IMAGE_SOURCE_DIRECTORY_FILE_STORE, uploadedImagePath, new AtomicInteger(0));
    deleteTestTempFile("MTA-1100587", TEST_TEMP_FILE2);
  }

  @Test
  public void updateImageLocationGcsForDuplicateLocationSwitchOffTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageService,"avoidDuplicateCallsToGcsForImageUploads",
      false);
    image.setLocationPath(LOCATION_PATH_FILE_STORE_2);
    image.setActive(false);
    Set<String> uploadedImagePath = new HashSet<>();
    uploadedImagePath.add("download.jpeg");
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(PATH_PREFIX);
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(SOURCE_IMAGE_DIRECTORY);
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn("merchant-qa1-static");
    Mockito.when(gcsProperties.getSourceResizeImageDirectory()).thenReturn(SOURCE_IMAGE_DIRECTORY_RESIZE);
    Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString())).thenReturn(FileUtils.readFileToByteArray(
      new File(SOURCE_FILE_1)));

    fileStorageService.updateLocationForImages(image, PRODUCT_CODE, IMAGE_SOURCE_DIRECTORY_FILE_STORE,
      FULL_IMAGE_SOURCE_DIRECTORY_FILE_STORE, uploadedImagePath, new AtomicInteger(0));
    deleteTestTempFile("MTA-1100587", TEST_TEMP_FILE2);
  }

  @Test
  public void updateImageLocationGcsForDuplicateLocationTest2() throws Exception {
    image.setLocationPath(INVALID_LOCATION_PATH_GCS);
    image.setActive(false);
    Set<String> uploadedImagePath = new HashSet<>();
    uploadedImagePath.add("download.jpeg");
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(PATH_PREFIX);
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(SOURCE_IMAGE_DIRECTORY);
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn("merchant-qa1-static");
    Mockito.when(gcsProperties.getSourceResizeImageDirectory()).thenReturn(SOURCE_IMAGE_DIRECTORY_RESIZE);
    Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString())).thenReturn(FileUtils.readFileToByteArray(
      new File(SOURCE_FILE_1)));

    fileStorageService.updateLocationForImages(image, PRODUCT_CODE, IMAGE_SOURCE_DIRECTORY_FILE_STORE,
      FULL_IMAGE_SOURCE_DIRECTORY_FILE_STORE, uploadedImagePath, new AtomicInteger(0));
    deleteTestTempFile("MTA-1100587", TEST_TEMP_FILE2);
  }

  @Test
  public void updateImageLocationGcsForDuplicateLocationWithNonGcsLocationTest() throws Exception {
    image.setLocationPath(LOCATION_PATH_FILE_STORE);
    image.setActive(false);
    Set<String> uploadedImagePath = new HashSet<>();
    uploadedImagePath.add("download.jpeg");
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(PATH_PREFIX);
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(SOURCE_IMAGE_DIRECTORY);
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn("merchant-qa1-static");
    Mockito.when(gcsProperties.getSourceResizeImageDirectory()).thenReturn(SOURCE_IMAGE_DIRECTORY_RESIZE);
    Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString())).thenReturn(FileUtils.readFileToByteArray(
      new File(SOURCE_FILE_1)));

    fileStorageService.updateLocationForImages(image, PRODUCT_CODE, IMAGE_SOURCE_DIRECTORY_FILE_STORE,
      FULL_IMAGE_SOURCE_DIRECTORY_FILE_STORE, uploadedImagePath, new AtomicInteger(0));
    deleteTestTempFile("MTA-1100587", TEST_TEMP_FILE2);
  }

  @Test
  public void updateImageLocationGcsForDuplicateLocationWithNonGcsActiveImageLocationTest() throws Exception {
    image.setLocationPath(LOCATION_PATH_FILE_STORE);
    image.setActive(true);
    Set<String> uploadedImagePath = new HashSet<>();
    uploadedImagePath.add("download.jpeg");
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(PATH_PREFIX);
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(SOURCE_IMAGE_DIRECTORY);
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn("merchant-qa1-static");
    Mockito.when(gcsProperties.getSourceResizeImageDirectory()).thenReturn(SOURCE_IMAGE_DIRECTORY_RESIZE);
    Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString())).thenReturn(FileUtils.readFileToByteArray(
      new File(SOURCE_FILE_1)));

    fileStorageService.updateLocationForImages(image, PRODUCT_CODE, IMAGE_SOURCE_DIRECTORY_FILE_STORE,
      FULL_IMAGE_SOURCE_DIRECTORY_FILE_STORE, uploadedImagePath, new AtomicInteger(0));
    deleteTestTempFile("MTA-1100587", TEST_TEMP_FILE2);
  }

  @Test
  public void updateImageLocationGcsForDuplicateLocationActiveFalseImageTest() throws Exception {
    image.setLocationPath(GCS_PATH);
    image.setActive(false);
    Set<String> uploadedImagePath = new HashSet<>();
    uploadedImagePath.add("download.jpeg");
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(PATH_PREFIX);
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(SOURCE_IMAGE_DIRECTORY);
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn("merchant-qa1-static");
    Mockito.when(gcsProperties.getSourceResizeImageDirectory()).thenReturn(SOURCE_IMAGE_DIRECTORY_RESIZE);
    Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString())).thenReturn(FileUtils.readFileToByteArray(
      new File(SOURCE_FILE_1)));

    fileStorageService.updateLocationForImages(image, PRODUCT_CODE, IMAGE_SOURCE_DIRECTORY_FILE_STORE,
      FULL_IMAGE_SOURCE_DIRECTORY_FILE_STORE, uploadedImagePath, new AtomicInteger(0));
    deleteTestTempFile("MTA-1100587", TEST_TEMP_FILE2);
  }


  private static void deleteTestTempFile(String folder, String tempFilePath) {
    while (tempFilePath.contains(folder)) {
      File file1 = new File(tempFilePath);
      if (file1.exists()) {
        file1.delete();
        tempFilePath = tempFilePath.substring(0, tempFilePath.lastIndexOf(File.separator));
      } else {
        break;
      }
    }
  }

  @Test
  public void updateImageLocationFileStoreTest() throws Exception {
    image.setLocationPath(LOCATION_PATH_FILE_STORE);
    image.setActive(true);
    Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString())).thenReturn(
        FileUtils.readFileToByteArray(
            new File(SOURCE_FILE_1)));
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    fileStorageService.updateLocationForImages(image, PRODUCT_CODE, IMAGE_SOURCE_DIRECTORY_FILE_STORE,
        StringUtils.EMPTY, new HashSet<>(), new AtomicInteger(0));
    Mockito.verify(gcsProperties, Mockito.times(2)).isSourceImageEnabled();
  }

  @Test
  public void updateImageLocationFileStoreGcsPathTest() throws Exception {
    image.setLocationPath(LOCATION_PATH_GCS);
    image.setActive(true);
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    try{
      Assertions.assertThrows(Exception.class, () -> fileStorageService.updateLocationForImages(image, PRODUCT_CODE, IMAGE_SOURCE_DIRECTORY_FILE_STORE, StringUtils.EMPTY,
        new HashSet<>(), new AtomicInteger(0)));
    } finally {
      Mockito.verify(gcsProperties).isSourceImageEnabled();
    }
  }

  @Test
  public void updateImageLocationFileStoreToGcsTest() throws Exception {
    image.setLocationPath(LOCATION_PATH_FILE_STORE);
    image.setActive(true);
    Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString())).thenReturn(
        FileUtils.readFileToByteArray(
            new File(SOURCE_FILE_1)));
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    fileStorageService.updateLocationForImages(image, PRODUCT_CODE, IMAGE_SOURCE_DIRECTORY_FILE_STORE, StringUtils.EMPTY,
      new HashSet<>(), new AtomicInteger(0));
    Mockito.verify(gcsProperties, Mockito.times(2)).isSourceImageEnabled();
    Mockito.verify(gcsService)
        .uploadFile(Mockito.any(), Mockito.anyString(),Mockito.any());
  }

  @Test
  public void updateImageLocationFileStoreToGcsMigrationDoneTest() throws Exception {
    image.setLocationPath(LOCATION_PATH_FILE_STORE);
    image.setActive(true);
    Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString())).thenReturn(
        FileUtils.readFileToByteArray(
            new File(SOURCE_FILE_1)));
    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(true);
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    fileStorageService.updateLocationForImages(image, PRODUCT_CODE, IMAGE_SOURCE_DIRECTORY_FILE_STORE, StringUtils.EMPTY,
      new HashSet<>(), new AtomicInteger(0));
    Mockito.verify(gcsProperties, Mockito.times(2)).isSourceImageEnabled();
    Mockito.verify(gcsService)
        .uploadFile(Mockito.any(), Mockito.anyString(),Mockito.any());
  }

  @Test
  public void updateImageLocationFileStoreToGcsMigrationNotDoneExceptionTest() throws Exception {
    image.setLocationPath(LOCATION_PATH_FILE_STORE);
    image.setActive(true);
    String pathForDownloadImageFromGcs =
        (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalFullImageDirectory()
            + File.separator + gcsProperties.getPathPrefix() + File.separator + PRODUCT_CODE + File.separator
            + FILE_NAME1);
    Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString()))
        .thenThrow(new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
            String.format(ErrorMessage.GCS_SOURCE_IMAGE_FILE_NOT_FOUND.getMessage(), pathForDownloadImageFromGcs)));
    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(false);
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    fileStorageService.updateLocationForImages(image, PRODUCT_CODE, IMAGE_SOURCE_DIRECTORY_FILE_STORE,
        StringUtils.EMPTY, new HashSet<>(), new AtomicInteger(0));
    Mockito.verify(gcsProperties, Mockito.times(2)).isSourceImageEnabled();
    Mockito.verify(gcsService).uploadFile(Mockito.any(), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void updateImageLocationFileStoreToGcsMigrationNotDoneUnHandledExceptionTest() throws Exception {
    image.setLocationPath(LOCATION_PATH_FILE_STORE);
    image.setActive(true);
    String pathForDownloadImageFromGcs =
        (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalFullImageDirectory()
            + File.separator + gcsProperties.getPathPrefix() + File.separator + PRODUCT_CODE + File.separator
            + FILE_NAME1);
    Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString()))
        .thenThrow(new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
            String.format(ErrorMessage.GCS_SOURCE_IMAGE_FILE_NOT_FOUND.getMessage(), pathForDownloadImageFromGcs)));
    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(false);
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    Assertions.assertThrows(Exception.class, () -> fileStorageService.updateLocationForImages(image, PRODUCT_CODE, IMAGE_SOURCE_DIRECTORY_FILE_STORE,
        StringUtils.EMPTY, new HashSet<>(), new AtomicInteger(0)));
  }

  @Test
  public void updateImageLocationFileStoreToFileStoreMigrationNotDoneExceptionTest() throws Exception {
    image.setLocationPath(LOCATION_PATH_FILE_STORE);
    image.setActive(true);
    String pathForDownloadImageFromGcs =
        (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalFullImageDirectory()
            + File.separator + gcsProperties.getPathPrefix() + File.separator + PRODUCT_CODE + File.separator
            + FILE_NAME1);
    Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString()))
        .thenThrow(new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
            String.format(ErrorMessage.GCS_SOURCE_IMAGE_FILE_NOT_FOUND.getMessage(), pathForDownloadImageFromGcs)));
    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(false);
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    fileStorageService.updateLocationForImages(image, PRODUCT_CODE, IMAGE_SOURCE_DIRECTORY_FILE_STORE,
        StringUtils.EMPTY, new HashSet<>(), new AtomicInteger(0));
    Mockito.verify(gcsProperties, Mockito.times(2)).isSourceImageEnabled();
  }

  @Test
  public void updateImageLocationGcsToFileStoreTest() throws Exception {
    image.setLocationPath(LOCATION_PATH_GCS);
    image.setActive(false);

    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(PATH_PREFIX);
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(SOURCE_IMAGE_DIRECTORY);
    Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString())).thenReturn(new byte[]{});

    fileStorageService.updateLocationForImages(image, PRODUCT_CODE, IMAGE_SOURCE_DIRECTORY_FILE_STORE, FULL_IMAGE_SOURCE_DIRECTORY_FILE_STORE,
      new HashSet<>(), new AtomicInteger(0));

    Mockito.verify(gcsProperties, Mockito.times(2)).getPathPrefix();
    Mockito.verify(gcsProperties, Mockito.times(2)).isSourceImageEnabled();
    Mockito.verify(gcsProperties, Mockito.times(1)).getSourceImageBucketName();
    Mockito.verify(gcsService).downloadFile(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(gcsProperties, Mockito.times(2)).isSourceImageEnabled();
    Mockito.verify(gcsProperties).getSourceImageDirectory();

    deleteTestTempFile("source-image", TEST_TEMP_FILE1);
  }

  @Test
  public void updateImageLocationGcsToFileStoreMigratedToGcsTrueTest() throws Exception {
    image.setLocationPath(LOCATION_PATH_GCS);
    image.setActive(false);

    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(PATH_PREFIX);
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(SOURCE_IMAGE_DIRECTORY);
    Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString())).thenReturn(new byte[]{});

    fileStorageService.updateLocationForImages(image, PRODUCT_CODE, IMAGE_SOURCE_DIRECTORY_FILE_STORE,
        FULL_IMAGE_SOURCE_DIRECTORY_FILE_STORE, new HashSet<>(), new AtomicInteger(0));

    Mockito.verify(gcsProperties, Mockito.times(2)).getPathPrefix();
    Mockito.verify(gcsProperties, Mockito.times(2)).isSourceImageEnabled();
    Mockito.verify(gcsProperties, Mockito.times(1)).getSourceImageBucketName();
    Mockito.verify(gcsService).downloadFile(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(gcsProperties, Mockito.times(2)).isSourceImageEnabled();
    Mockito.verify(gcsProperties).getSourceImageDirectory();

    deleteTestTempFile("source-image", TEST_TEMP_FILE1);
  }

  @Test
  public void updateImageLocationGcsToFileStoreFileStorePathTest() throws Exception {
    image.setLocationPath(LOCATION_PATH_FILE_STORE);
    image.setActive(false);

    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(true);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(PATH_PREFIX);
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(SOURCE_IMAGE_DIRECTORY);
    Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString())).thenReturn(new byte[]{});

    fileStorageService.updateLocationForImages(image, PRODUCT_CODE, IMAGE_SOURCE_DIRECTORY_FILE_STORE, FULL_IMAGE_SOURCE_DIRECTORY_FILE_STORE,
      new HashSet<>(), new AtomicInteger(0));

    Mockito.verify(gcsProperties, Mockito.times(3)).getPathPrefix();
    Mockito.verify(gcsProperties, Mockito.times(2)).isSourceImageEnabled();

    deleteTestTempFile("source-image", TEST_TEMP_FILE1);
  }

  @Test
  public void uploadFileToFileStoreExceptionTest() throws Exception {
    image.setLocationPath(LOCATION_PATH_GCS);
    image.setActive(false);
    Assertions.assertThrows(Exception.class, () -> fileStorageService.uploadFileToFileStore(image, PRODUCT_CODE, IMAGE_SOURCE_DIRECTORY_FILE_STORE, null, FILE_NAME));
  }

  @Test
  public void updateLocationForImagesSameProductCodeTest() throws Exception {
    image.setLocationPath(PRODUCT_CODE);
    fileStorageService.updateLocationForImages(image, PRODUCT_CODE, IMAGE_SOURCE_DIRECTORY_FILE_STORE,
        FULL_IMAGE_SOURCE_DIRECTORY_FILE_STORE, new HashSet<>(), new AtomicInteger(0));
  }

  @Test
  public void updateImageLocationFileStoreImageTest() throws Exception {
    image.setLocationPath(LOCATION_PATH_FILE_STORE);
    image.setActive(true);
    Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString())).thenReturn(
        FileUtils.readFileToByteArray(
            new File(SOURCE_FILE_1)));
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    fileStorageService.updateLocationForImages(image, PRODUCT_CODE, IMAGE_SOURCE_DIRECTORY_FILE_STORE,
        IMAGE_SOURCE_DIRECTORY_FILE_STORE, new HashSet<>(), new AtomicInteger(0));
    Mockito.verify(gcsProperties, Mockito.times(2)).isSourceImageEnabled();
  }

  @Test
  public void deleteImagesTrueTest() {
    String locationPath = LOCATION_PATH;
    File image = Mockito.mock(File.class);
    Mockito.when(image.exists()).thenReturn(true);
    fileStorageService.deleteImages(locationPath);
  }

  @Test
  public void deleteImagesFalseTest() {
    String locationPath = SOURCE_DIRECTORY + LOCATION_PATH + CATALOG_NAME;
    File image = Mockito.mock(File.class);
    Mockito.when(gcsService.deleteFile(BUCKET_NAME,(gcsProperties.getSourceImageDirectory() + File.separator + locationPath).replaceAll("//", "/"))).thenReturn(true);
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn(BUCKET_NAME);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn("source/");
    Mockito.when(image.exists()).thenReturn(false);
    fileStorageService.deleteImages(locationPath);
    Mockito.verify(gcsService,Mockito.times(1)).deleteFile(BUCKET_NAME,(gcsProperties.getSourceImageDirectory() + File.separator + locationPath).replaceAll("//", "/"));
    Mockito.verify(gcsProperties).getSourceImageBucketName();
    Mockito.verify(gcsProperties, Mockito.times(3)).getSourceImageDirectory();
  }

  @Test
  public void deleteImagesTest()  throws Exception {
    mockImageFile(FILE_PATH);
    fileStorageService.deleteImages(FILE);
  }

  private void mockImageFile(String filePath) throws IOException {
    mockFile(filePath);
  }

  private void mockFile(String filePath) throws IOException {
    file = new File(filePath);
    file.mkdirs();
  }

  @Test
  public void deleteFromGcsTest() {
    String locationPath = SOURCE_DIRECTORY + LOCATION_PATH;
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn(BUCKET_NAME);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn("source/");
    Mockito.when(gcsService.deleteFile(BUCKET_NAME, (gcsProperties.getSourceImageDirectory() + File.separator + locationPath).replaceAll("//", "/"))).thenReturn(false);
    try {
      fileStorageService.deleteFromGcs(locationPath);
    } finally {
      Mockito.verify(gcsService,Mockito.times(1)).deleteFile(BUCKET_NAME,(gcsProperties.getSourceImageDirectory() + File.separator + locationPath).replaceAll("//", "/"));
      Mockito.verify(gcsProperties).getSourceImageBucketName();
      Mockito.verify(gcsProperties, Mockito.times(3)).getSourceImageDirectory();
    }
  }

  @Test
  public void deleteFileTest() {
    String locationPath = SOURCE_DIRECTORY + LOCATION_PATH;
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn("source/");
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn(BUCKET_NAME);
    Mockito.when(gcsService.deleteFile(BUCKET_NAME,(gcsProperties.getSourceImageDirectory() + File.separator + locationPath).replaceAll("//", "/"))).thenReturn(true);
    fileStorageService.deleteFromGcs(locationPath);
    Mockito.verify(gcsService,Mockito.times(1)).deleteFile(BUCKET_NAME,(gcsProperties.getSourceImageDirectory() + File.separator + locationPath).replaceAll("//", "/"));
    Mockito.verify(gcsProperties).getSourceImageBucketName();
    Mockito.verify(gcsProperties, Mockito.times(3)).getSourceImageDirectory();
  }

  @Test
  public void isFinalImageFileExistTest() {
    Mockito.when(gcsService.isFileExists("bucket-name", "image-directory/full-image-directory/file.jpg")).thenReturn(false);
    boolean result = fileStorageService.isFinalImageFileExist("file.jpg");
    Assertions.assertTrue(result);
  }

  @Test
  public void isFinalImageFileExistFalseTest() {
    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(false);
    Mockito.when(gcsService.isFileExists("bucket-name", "image-directory/full-image-directory/file.jpg")).thenReturn(false);
    boolean result = fileStorageService.isFinalImageFileExist("prefix/file.jpg");
    Assertions.assertTrue(result);
  }

  @Test
  public void isFinalImageFileExistTrueTest() {
    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(false);
    boolean result = fileStorageService.isFinalImageFileExist("file.jpg");
    Assertions.assertTrue(result);
  }

  @Test
  public void isFinalImageFileExistEmptyTest() {
    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(false);
    Mockito.when(gcsService.isFileExists("bucket-name", "image-directory/full-image-directory/file.jpg"))
        .thenReturn(true);
    boolean result = fileStorageService.isFinalImageFileExist("prefix/file.jpg");
    Assertions.assertTrue(result);
  }

  @Test
  public void isFinalImageFileExistEmptyImageTest() {
    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(true);
    Mockito.when(gcsService.isFileExists("bucket-name", "image-directory/full-image-directory/file.jpg"))
        .thenReturn(true);
    boolean result = fileStorageService.isFinalImageFileExist("file.jpg");
    Assertions.assertTrue(result);
  }

  @Test
  public void isFinalImageFileExist() {
    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(true);
    Mockito.when(gcsService.isFileExists(Mockito.any(),Mockito.anyString())).thenReturn(true);
    boolean result = fileStorageService.isFinalImageFileExist("file.jpg");
    Assertions.assertFalse(result);
  }

  @Test
  public void isFinalImageFileExistFalse() {
    ReflectionTestUtils.setField(fileStorageService,"fullImageDirectory","src/test/resources/");
    Mockito.when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(false);
    Mockito.when(gcsService.isFileExists(Mockito.any(), Mockito.anyString())).thenReturn(false);
    boolean result = fileStorageService.isFinalImageFileExist("MTA-0615254/resize/nike_test_product_and_item_images_full01_hhp7330d.jpg");
    Assertions.assertFalse(result);
  }

  @Test
  public void migrateFinalImageFromGfsToGcsParallelProcessTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageService, "fullImageDirectory", FULL_IMAGE_FINAL_DIRECTORY);
    ReflectionTestUtils.setField(fileStorageService, "mediumImageDirectory", MEDIUM_IMAGE_FINAL_DIRECTORY);
    ReflectionTestUtils.setField(fileStorageService, "thumbnailImageDirectory", THUMBNAIL_IMAGE_FINAL_DIRECTORY);
    ReflectionTestUtils.setField(fileStorageService, "migratedFinalImageDeletionEnabled", true);
    Set<String> locationPathSet = new HashSet<>();
    locationPathSet.add(LOCATION_PATH_FULL);
    createTestFile(TEST_FINAL_FULL_IMAGE);
    createTestFile(TEST_FINAL_MEDIUM_IMAGE);
    createTestFile(TEST_FINAL_THUMBNAIL_IMAGE);
    Mockito.when(gcsProperties.isFinalImageParallelUploadEnabled()).thenReturn(true);
    fileStorageService.migrateFinalImageFromGfsToGcs(locationPathSet);
    Mockito.verify(gcsProperties, Mockito.times(2)).isFinalImageParallelUploadEnabled();
    Mockito.verify(imageUploadExecutorService).invokeAll(Mockito.anyList());
  }

  @Test
  public void migrateFinalImageFromGfsToGcsParallelProcessFileNotFoundTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageService, "fullImageDirectory", FULL_IMAGE_FINAL_DIRECTORY);
    ReflectionTestUtils.setField(fileStorageService, "mediumImageDirectory", MEDIUM_IMAGE_FINAL_DIRECTORY);
    ReflectionTestUtils.setField(fileStorageService, "thumbnailImageDirectory", THUMBNAIL_IMAGE_FINAL_DIRECTORY);
    ReflectionTestUtils.setField(fileStorageService, "migratedFinalImageDeletionEnabled", false);
    Set<String> locationPathSet = new HashSet<>();
    locationPathSet.add(LOCATION_PATH_MEDIUM);
    Mockito.when(gcsProperties.isFinalImageParallelUploadEnabled()).thenReturn(true);
    fileStorageService.migrateFinalImageFromGfsToGcs(locationPathSet);
    Mockito.verify(gcsProperties, Mockito.times(2)).isFinalImageParallelUploadEnabled();
    Mockito.verify(imageUploadExecutorService).invokeAll(Mockito.anyList());
  }

  @Test
  public void migrateFinalImageFromGfsToGcsSequentiallyProcessTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageService, "fullImageDirectory", FULL_IMAGE_FINAL_DIRECTORY);
    ReflectionTestUtils.setField(fileStorageService, "mediumImageDirectory", MEDIUM_IMAGE_FINAL_DIRECTORY);
    ReflectionTestUtils.setField(fileStorageService, "thumbnailImageDirectory", THUMBNAIL_IMAGE_FINAL_DIRECTORY);
    ReflectionTestUtils.setField(fileStorageService, "migratedFinalImageDeletionEnabled", true);
    Set<String> locationPathSet = new HashSet<>();
    locationPathSet.add(LOCATION_PATH_FULL);
    createTestFile(TEST_FINAL_FULL_IMAGE);
    createTestFile(TEST_FINAL_MEDIUM_IMAGE);
    createTestFile(TEST_FINAL_THUMBNAIL_IMAGE);
    Mockito.when(gcsProperties.isFinalImageParallelUploadEnabled()).thenReturn(false);
    fileStorageService.migrateFinalImageFromGfsToGcs(locationPathSet);
    Mockito.verify(gcsProperties, Mockito.times(2)).isFinalImageParallelUploadEnabled();
    Mockito.verify(gcsProperties, Mockito.times(3)).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties).getFinalMediumImageDirectory();
    Mockito.verify(gcsProperties).getFinalThumbnailImageDirectory();
    Mockito.verify(gcsService, Mockito.times(3)).uploadFile(Mockito.any(), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void uploadFinalImageFromGfsToGcsTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageService, "fullImageDirectory", FULL_IMAGE_FINAL_DIRECTORY);
    ReflectionTestUtils.setField(fileStorageService, "mediumImageDirectory", MEDIUM_IMAGE_FINAL_DIRECTORY);
    ReflectionTestUtils.setField(fileStorageService, "thumbnailImageDirectory", THUMBNAIL_IMAGE_FINAL_DIRECTORY);
    createTestFile(TEST_FINAL_FULL_IMAGE);
    createTestFile(TEST_FINAL_MEDIUM_IMAGE);
    createTestFile(TEST_FINAL_THUMBNAIL_IMAGE);
    fileStorageService.uploadFinalImageFromGfsToGcs(LOCATION_PATH_FULL);
    Mockito.verify(gcsProperties, Mockito.times(3)).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties).getFinalMediumImageDirectory();
    Mockito.verify(gcsProperties).getFinalThumbnailImageDirectory();
    Mockito.verify(gcsService, Mockito.times(3)).uploadFile(Mockito.any(), Mockito.anyString(), Mockito.any());
  }

  @Test
  public void uploadFinalImageFromGfsToGcsFileNotFoundTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageService, "fullImageDirectory", FULL_IMAGE_FINAL_DIRECTORY);
    ReflectionTestUtils.setField(fileStorageService, "mediumImageDirectory", MEDIUM_IMAGE_FINAL_DIRECTORY);
    ReflectionTestUtils.setField(fileStorageService, "thumbnailImageDirectory", THUMBNAIL_IMAGE_FINAL_DIRECTORY);
    fileStorageService.uploadFinalImageFromGfsToGcs(LOCATION_PATH_MEDIUM);
  }

  private void createTestFile(String path) throws IOException {
    File fileNew = new File(path);
    File soureFile = new File("src/test/resources/filestore/mta/images/sources/MTA-1100587/download.jpeg");
    byte[] sourceFile = FileUtils.readFileToByteArray(soureFile);
    FileUtils.writeByteArrayToFile(fileNew, sourceFile);
  }

  @Test
  public void isAlreadyGcsImageTest () {
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(PATH_PREFIX);

    Assertions.assertTrue(fileStorageService.isAlreadyGcsImage(TEST_TEMP_FILE1));
    Assertions.assertFalse(fileStorageService.isAlreadyGcsImage(TEST_TEMP_FILE2));

    Mockito.verify(gcsProperties, Mockito.times(2)).getPathPrefix();
  }

  @Test
  public void getGcsPathWithPrefixTest() {
    Mockito.when(gcsProperties.getResizePrefix()).thenReturn("/resize");
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(PATH_PREFIX);
    Assertions.assertEquals(GCS_RESIZE_PATH, fileStorageService.getGcsPathWithPrefix(FILE_STORE_RESIZE_PATH));
    Assertions.assertEquals(GCS_PATH, fileStorageService.getGcsPathWithPrefix(FILE_STORE_PATH));
    Mockito.verify(gcsProperties, Mockito.times(4)).getResizePrefix();
    Mockito.verify(gcsProperties, Mockito.times(2)).getPathPrefix();
  }

  @Test
  public void migrateImagesFromGfsToGcsTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageService, "imageSourceDirectory","src/test/resources/filestore");
    try {
      File file = new File(FILE_STORE_SOURCE_IMAGE_PATH);
      File copyFile = new File("src/test/resources/filestore/" + FILE_STORE_SOURCE_IMAGE_COPY_PATH);
      copyFile.getParentFile().mkdirs();
      Files.copy(file.toPath(), copyFile.toPath());

      Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn("source-image");
      Mockito.when(gcsProperties.isParallelUploadEnabled()).thenReturn(false);
      Mockito.when(
          gcsService.uploadFile(Mockito.eq(sourceImageBucket), Mockito.eq("source-image/" + GCS_SOURCE_IMAGE_PATH),
              Mockito.any())).thenReturn(blob);

      fileStorageService.migrateImagesFromGfsToGcs(PRODUCT_CODE,
          ImmutableSet.of(Pair.of(FILE_STORE_SOURCE_IMAGE_COPY_PATH, GCS_SOURCE_IMAGE_PATH)));

      Mockito.verify(gcsProperties).isParallelUploadEnabled();
      Mockito.verify(gcsService)
          .uploadFile(Mockito.any(), Mockito.eq("source-image/" + GCS_SOURCE_IMAGE_PATH),
              Mockito.any());
    } catch (Exception e){
      FileUtils.cleanDirectory(new File("src/test/resources/filestore/copy"));
    }
  }

  @Test
  public void migrateImagesFromGfsToGcsParallelUploadTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageService, "imageSourceDirectory","src/test/resources/filestore");
    try {
      File file = new File(FILE_STORE_SOURCE_IMAGE_PATH);
      File copyFile = new File("src/test/resources/filestore/" + FILE_STORE_SOURCE_IMAGE_COPY_PATH);
      copyFile.getParentFile().mkdirs();
      Files.copy(file.toPath(), copyFile.toPath());

      Mockito.when(gcsProperties.isParallelUploadEnabled()).thenReturn(true);

      fileStorageService.migrateImagesFromGfsToGcs(PRODUCT_CODE,
          ImmutableSet.of(Pair.of(FILE_STORE_SOURCE_IMAGE_COPY_PATH, GCS_SOURCE_IMAGE_PATH)));

      Mockito.verify(gcsProperties).isParallelUploadEnabled();
      Mockito.verify(imageUploadExecutorService).invokeAll(Mockito.anyList());
    } catch (Exception e){
      FileUtils.cleanDirectory(new File("src/test/resources/filestore/copy"));
    }
  }

  private Future<Boolean> getImageResponseFuture() {
    return new Future<Boolean>() {
      @Override
      public boolean cancel(boolean mayInterruptIfRunning) {
        return false;
      }

      @Override
      public boolean isCancelled() {
        return false;
      }

      @Override
      public boolean isDone() {
        return false;
      }

      @Override
      public Boolean get() throws InterruptedException, ExecutionException {
        return true;
      }

      @Override
      public Boolean get(long timeout, TimeUnit unit)
          throws InterruptedException, ExecutionException, TimeoutException {
        return true;
      }
    };
  }

  @Test
  public void deleteFullImagesTrueTest() {
    String locationPath = LOCATION_PATH;
    File image = Mockito.mock(File.class);
    Mockito.when(image.exists()).thenReturn(true);
    fileStorageService.deleteFullFinalImagesFromGcs(locationPath);
  }

  @Test
  public void deleteMediumImagesTrueTest() {
    String locationPath = LOCATION_PATH;
    File image = Mockito.mock(File.class);
    Mockito.when(image.exists()).thenReturn(true);
    fileStorageService.deleteFinalMediumImagesFromGcs(locationPath);
  }

  @Test
  public void deleteThumbnailImagesTrueTest() {
    String locationPath = LOCATION_PATH;
    File image = Mockito.mock(File.class);
    Mockito.when(image.exists()).thenReturn(true);
    fileStorageService.deleteFinalThumbnailImagesFromGcs(locationPath);
  }

  @Test
  public void deleteFullImagesFalseTest() {
    String locationPath = FULL_DIRECTORY + LOCATION_PATH + CATALOG_NAME;
    File image = Mockito.mock(File.class);
    Mockito.when(gcsService.deleteFile(BUCKET_NAME,
        (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalFullImageDirectory()
            + File.separator + locationPath).replaceAll("//", "/"))).thenReturn(true);
    Mockito.when(gcsProperties.getFinalImageBucketName()).thenReturn(BUCKET_NAME);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn("full/");
    Mockito.when(image.exists()).thenReturn(false);
    fileStorageService.deleteFullFinalImagesFromGcs(locationPath);
    Mockito.verify(gcsService, Mockito.times(1)).deleteFile(BUCKET_NAME,
        (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalFullImageDirectory()
            + File.separator + locationPath).replaceAll("//", "/"));
    Mockito.verify(gcsProperties).getFinalImageBucketName();
    Mockito.verify(gcsProperties, Mockito.times(3)).getFinalFullImageDirectory();
  }

  @Test
  public void deleteMediumImagesFalseTest() {
    String locationPath = MEDIUM_DIRECTORY + LOCATION_PATH + CATALOG_NAME;
    File image = Mockito.mock(File.class);
    Mockito.when(gcsService.deleteFile(BUCKET_NAME,
        (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalMediumImageDirectory()
            + File.separator + locationPath).replaceAll("//", "/"))).thenReturn(true);
    Mockito.when(gcsProperties.getFinalImageBucketName()).thenReturn(BUCKET_NAME);
    Mockito.when(gcsProperties.getFinalMediumImageDirectory()).thenReturn("medium/");
    Mockito.when(image.exists()).thenReturn(false);
    fileStorageService.deleteFinalMediumImagesFromGcs(locationPath);
    Mockito.verify(gcsService, Mockito.times(1)).deleteFile(BUCKET_NAME,
        (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalMediumImageDirectory()
            + File.separator + locationPath).replaceAll("//", "/"));
    Mockito.verify(gcsProperties).getFinalImageBucketName();
    Mockito.verify(gcsProperties, Mockito.times(3)).getFinalMediumImageDirectory();
  }

  @Test
  public void deleteThumbnailImagesFalseTest() {
    String locationPath = THUMBNAIL_DIRECTORY + LOCATION_PATH + CATALOG_NAME;
    File image = Mockito.mock(File.class);
    Mockito.when(gcsService.deleteFile(BUCKET_NAME,
        (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalThumbnailImageDirectory()
            + File.separator + locationPath).replaceAll("//", "/"))).thenReturn(true);
    Mockito.when(gcsProperties.getFinalImageBucketName()).thenReturn(BUCKET_NAME);
    Mockito.when(gcsProperties.getFinalThumbnailImageDirectory()).thenReturn("thumbnail/");
    Mockito.when(image.exists()).thenReturn(false);
    fileStorageService.deleteFinalThumbnailImagesFromGcs(locationPath);
    Mockito.verify(gcsService, Mockito.times(1)).deleteFile(BUCKET_NAME,
        (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalThumbnailImageDirectory()
            + File.separator + locationPath).replaceAll("//", "/"));
    Mockito.verify(gcsProperties).getFinalImageBucketName();
    Mockito.verify(gcsProperties, Mockito.times(3)).getFinalThumbnailImageDirectory();
  }

  @Test
  public void deleteFullImagesTest() {
    String locationPath = FULL_DIRECTORY + LOCATION_PATH + CATALOG_NAME;
    File image = Mockito.mock(File.class);
    Mockito.when(gcsService.deleteFile(BUCKET_NAME,
        (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalFullImageDirectory()
            + File.separator + locationPath).replaceAll("//", "/"))).thenReturn(false);
    Mockito.when(gcsProperties.getFinalImageBucketName()).thenReturn(BUCKET_NAME);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn("full/");
    Mockito.when(image.exists()).thenReturn(false);
    fileStorageService.deleteFullFinalImagesFromGcs(locationPath);
    Mockito.verify(gcsService, Mockito.times(1)).deleteFile(BUCKET_NAME,
        (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalFullImageDirectory()
            + File.separator + locationPath).replaceAll("//", "/"));
    Mockito.verify(gcsProperties).getFinalImageBucketName();
    Mockito.verify(gcsProperties, Mockito.times(3)).getFinalFullImageDirectory();
  }

  @Test
  public void deleteMediumImagesTest() {
    String locationPath = MEDIUM_DIRECTORY + LOCATION_PATH + CATALOG_NAME;
    File image = Mockito.mock(File.class);
    Mockito.when(gcsService.deleteFile(BUCKET_NAME,
        (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalMediumImageDirectory()
            + File.separator + locationPath).replaceAll("//", "/"))).thenReturn(false);
    Mockito.when(gcsProperties.getFinalImageBucketName()).thenReturn(BUCKET_NAME);
    Mockito.when(gcsProperties.getFinalMediumImageDirectory()).thenReturn("medium/");
    Mockito.when(image.exists()).thenReturn(false);
    fileStorageService.deleteFinalMediumImagesFromGcs(locationPath);
    Mockito.verify(gcsService, Mockito.times(1)).deleteFile(BUCKET_NAME,
        (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalMediumImageDirectory()
            + File.separator + locationPath).replaceAll("//", "/"));
    Mockito.verify(gcsProperties).getFinalImageBucketName();
    Mockito.verify(gcsProperties, Mockito.times(3)).getFinalMediumImageDirectory();
  }

  @Test
  public void deleteThumbnailImagesTest() {
    String locationPath = THUMBNAIL_DIRECTORY + LOCATION_PATH + CATALOG_NAME;
    File image = Mockito.mock(File.class);
    Mockito.when(gcsService.deleteFile(BUCKET_NAME,
        (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalThumbnailImageDirectory()
            + File.separator + locationPath).replaceAll("//", "/"))).thenReturn(false);
    Mockito.when(gcsProperties.getFinalImageBucketName()).thenReturn(BUCKET_NAME);
    Mockito.when(gcsProperties.getFinalThumbnailImageDirectory()).thenReturn("thumbnail/");
    Mockito.when(image.exists()).thenReturn(false);
    fileStorageService.deleteFinalThumbnailImagesFromGcs(locationPath);
    Mockito.verify(gcsService, Mockito.times(1)).deleteFile(BUCKET_NAME,
        (gcsProperties.getFinalImageDirectory() + File.separator + gcsProperties.getFinalThumbnailImageDirectory()
            + File.separator + locationPath).replaceAll("//", "/"));
    Mockito.verify(gcsProperties).getFinalImageBucketName();
    Mockito.verify(gcsProperties, Mockito.times(3)).getFinalThumbnailImageDirectory();
  }

  @Test
  public void deleteFullImageFileTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageService,"fullImageDirectory","src/test/resources/filestr");
    mockImageFile(FILE_PATH);
    fileStorageService.deleteFullFinalImagesFromGcs(FILE);
  }

  @Test
  public void deleteMediumImageFileTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageService,"mediumImageDirectory","src/test/resources/filestr");
    mockImageFile(FILE_PATH);
    fileStorageService.deleteFinalMediumImagesFromGcs(FILE);
  }

  @Test
  public void deleteThumbnailImageFileTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageService,"thumbnailImageDirectory","src/test/resources/filestr");
    mockImageFile(FILE_PATH);
    fileStorageService.deleteFinalThumbnailImagesFromGcs(FILE);
  }
}
