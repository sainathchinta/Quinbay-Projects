package com.gdn.micro.graphics.service;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.UUID;

import org.apache.commons.io.FileUtils;
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
import com.gdn.micro.graphics.domain.event.model.ImageResponse;
import com.gdn.micro.graphics.domain.event.model.ImageResultDetail;
import com.gdn.micro.graphics.model.GraphicDetailCommand;
import com.gdn.micro.graphics.model.GraphicImageDetail;
import com.gdn.micro.graphics.service.config.GcsProperties;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.Bucket;
import com.google.cloud.storage.Storage;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;

public class FileStorageServiceImplTest {

  @InjectMocks
  private FileStorageServiceImpl fileStorageService;

  @Mock
  private GcsService gcsService;

  @Mock
  private GcsProperties gcsProperties;

  @Mock(name = "sourceImageBucket")
  private Bucket sourceImageBucket;

  @Mock
  private ImagePathConfiguration imagePathConfiguration;

  @Mock(name = "rmaImageBucket")
  private Bucket rmaImageBucket;

  @Mock(name = "finalImageBucket")
  private Bucket finalImageBucket;

  @Mock(name = "oxfordImageBucket")
  private Bucket oxfordImageBucket;

  @Mock(name = "orderImageBucket")
  private Bucket orderImageBucket;

  private static final String PATH_PREFIX = "catalog-image";
  private static final String SOURCE_IMAGE_BUCKET_NAME = "merchant-qa1-static";
  private static final String TEMP_DIR_FOR_RESIZE_IMAGE = "tempDirForResizeImage";
  private static final String TEMP_DIR_FOR_SCALE_IMAGE = "tempDirForScaleImage";
  private static final String SOURCE_IMAGE_RESIZE_DIRECTORY = "source-image/catalog-image/";
  private static final String DESTINATION_PATH =
      "src/test/resources/source-image-7/catalog-image/resize/MTA-48566158/download.jpeg";
  private static final String SOURCE_PATH = "source-image/catalog-image/resize/MTA-48566158/download.jpeg";

  private static final String PATH = "source-image/catalog-image/MTA-48566158/download.jpeg";
  private static final String SOURCE_PATH_FILE_STORE = "/filestore/mta/images/source/MTA-48566158/download.jpeg";
  private static final String PREFIX_PATH_FILE_STORE = "src/test/resources/filestore/mta/images/source";
  private static final String PRODUCT_CODE_FILE_NAME = "MTA-48566158/download.jpeg";
  private static final String SOURCE_RESIZE_IMAGE_DIR = "source-image/catalog-image/resize";
  private static final String SOURCE_PATH_AFTER_RESIZE =
      "src/test/resources/filestore/mta/images/source/temp_directory_resize_image_MTA-00000000/download.jpeg";
  private static final String SOURCE_PATH_AFTER_SCALE =
      "src/test/resources/filestore/mta/images/source/temp_directory_scale_image_MTA-00000000/download.jpeg";
  private static final String SOURCE_FILE_LOCATION =
      "src/test/resources/source-image1/catalog-image/resize/MTA-48566158/download.jpeg";
  private static final String NEW_FILE_LOCATION =
      "src/test/resources/filestore/mta/images/source/temp_directory_resize_image_MTA-48566158/download.jpeg";
  private static final String NEW_FILE_RESIZE_LOCATION =
      "src/test/resources/filestore/mta/images/source/resize/MTA-48566158/download.jpeg";
  private static final String NEW_FILE_SCALE_LOCATION =
      "src/test/resources/filestore/mta/images/source/scale/MTA-48566158/download.jpeg";
  private static final String NEW_FILE_SEOUL_LOCATION =
      "src/test/resources/filestore/mta/images/source/seoul/MTA-48566158/download.jpeg";
  private static final String NEW_FILE_NONE_LOCATION =
      "src/test/resources/filestore/mta/images/none/MTA-48566158/download.jpeg";
  private static final String RESIZE = "resize";
  private static final String SCALE = "scale";
  private static final String SEOUL = "seoul";
  private static final String XRMA = "xrma";
  private static final String OXFORD = "oxford";
  private static final String ORDER = "xorder";

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(PATH_PREFIX);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(SOURCE_IMAGE_RESIZE_DIRECTORY);
    Mockito.when(gcsProperties.getSourceImageBucketName()).thenReturn(SOURCE_IMAGE_BUCKET_NAME);
    ReflectionTestUtils.setField(fileStorageService, TEMP_DIR_FOR_RESIZE_IMAGE, "/temp_directory_resize_image_");
    ReflectionTestUtils.setField(fileStorageService, TEMP_DIR_FOR_SCALE_IMAGE, "/temp_directory_scale_image_");
    ReflectionTestUtils.setField(fileStorageService, "sourcePrefixPath",
        "src/test/resources/filestore/mta/images/source/");
    Mockito.when(gcsService.downloadFile(anyString(), anyString())).thenReturn(new byte[] {1});
    Mockito.when(gcsProperties.getSourceResizeImageDirectory()).thenReturn(SOURCE_RESIZE_IMAGE_DIR);
    Mockito.when(gcsProperties.getResizeImagePrefix()).thenReturn("resize/");
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn("final-image");
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn("full");
    Mockito.when(gcsProperties.getFinalImageBucketName()).thenReturn("finalImageBucket");
    Mockito.when(gcsProperties.getFinalMediumImageDirectory()).thenReturn("medium");
    Mockito.when(gcsProperties.getFinalThumbnailImageDirectory()).thenReturn("thumbnail");
    Mockito.when(gcsProperties.getFinalSeoulImageDirectory()).thenReturn("seoul");
  }

  @AfterEach
  public void tearDown() throws IOException {
    Mockito.verifyNoMoreInteractions(gcsService, gcsProperties, sourceImageBucket);
    File file = new File(DESTINATION_PATH);
    file.delete();
  }

  @Test
  void getLocalSourcePathOfFileTest() throws IOException {
    File file = new File(NEW_FILE_LOCATION);
    file.getParentFile().mkdirs();
    File sourceFile = new File(SOURCE_FILE_LOCATION);
    FileUtils.copyFile(sourceFile, file);
    String sourcePath =
        fileStorageService.getLocalSourcePathOfFile(SOURCE_PATH, PREFIX_PATH_FILE_STORE, true, PRODUCT_CODE_FILE_NAME,
            false);
    Mockito.verify(gcsProperties, Mockito.times(2)).getPathPrefix();
    Mockito.verify(gcsProperties).getSourceImageDirectory();
    Mockito.verify(gcsProperties).getSourceImageBucketName();
    Mockito.verify(gcsService).downloadFileTo(anyString(), anyString(), anyString());
    FileUtils.cleanDirectory(new File("src/test/resources/filestore"));
  }

  @Test
  void getLocalSourcePathOfFileEditedTrueTest() throws IOException {
    File file = new File(NEW_FILE_LOCATION);
    file.getParentFile().mkdirs();
    File sourceFile = new File(SOURCE_FILE_LOCATION);
    FileUtils.copyFile(sourceFile, file);
    String sourcePath =
        fileStorageService.getLocalSourcePathOfFile(SOURCE_PATH, PREFIX_PATH_FILE_STORE, true, PRODUCT_CODE_FILE_NAME,
            true);
    Mockito.verify(gcsProperties, Mockito.times(2)).getPathPrefix();
    Mockito.verify(gcsProperties).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties).getFinalImageBucketName();
    Mockito.verify(gcsService).downloadFileTo(anyString(), anyString(), anyString());
    FileUtils.cleanDirectory(new File("src/test/resources/filestore"));
  }

  @Test
  void getLocalSourcePathOfFileFileStoreTest() throws IOException {
    String sourcePath =
        fileStorageService.getLocalSourcePathOfFile(SOURCE_PATH_FILE_STORE, PREFIX_PATH_FILE_STORE, true,
            PRODUCT_CODE_FILE_NAME, false);
    Mockito.verify(gcsProperties).getPathPrefix();
    Assertions.assertTrue(sourcePath.equals(SOURCE_PATH_FILE_STORE));
    FileUtils.cleanDirectory(new File("src/test/resources/filestore"));
  }

  @Test
  void getLocalSourcePathOfFileFileStoreResizeFalseTest() throws IOException {
    String sourcePath =
        fileStorageService.getLocalSourcePathOfFile(SOURCE_PATH_FILE_STORE, PREFIX_PATH_FILE_STORE, false,
            PRODUCT_CODE_FILE_NAME, false);
    Mockito.verify(gcsProperties).getPathPrefix();
    Assertions.assertTrue(sourcePath.equals(SOURCE_PATH_FILE_STORE));
    FileUtils.cleanDirectory(new File("src/test/resources/filestore"));
  }

  @Test
  void getLocalSourcePathOfFileResizeFalseTest() throws IOException {
    createFileForTest(NEW_FILE_LOCATION);
    String sourcePath =
        fileStorageService.getLocalSourcePathOfFile(SOURCE_PATH, PREFIX_PATH_FILE_STORE, false, PRODUCT_CODE_FILE_NAME,
            false);
    Mockito.verify(gcsProperties, Mockito.times(2)).getPathPrefix();
    Mockito.verify(gcsProperties).getSourceResizeImageDirectory();
    Mockito.verify(gcsProperties).getSourceImageBucketName();
    Mockito.verify(gcsService).downloadFileTo(anyString(), anyString(), anyString());
    Mockito.verify(gcsProperties, Mockito.times(1)).getResizeImagePrefix();
    FileUtils.cleanDirectory(new File("src/test/resources/filestore"));
  }

  @Test
  void getLocalSourcePathOfFileFalseResizeTest() throws IOException {
    File file = new File(NEW_FILE_LOCATION);
    file.getParentFile().mkdirs();
    File sourceFile = new File(SOURCE_FILE_LOCATION);
    FileUtils.copyFile(sourceFile, file);
    String sourcePath =
        fileStorageService.getLocalSourcePathOfFile(PATH, PREFIX_PATH_FILE_STORE, false, PRODUCT_CODE_FILE_NAME, false);
    Mockito.verify(gcsProperties, Mockito.times(2)).getPathPrefix();
    Mockito.verify(gcsProperties).getSourceImageDirectory();
    Mockito.verify(gcsProperties).getSourceImageBucketName();
    Mockito.verify(gcsService).downloadFileTo(anyString(), anyString(), anyString());
    Mockito.verify(gcsProperties, Mockito.times(1)).getResizeImagePrefix();
    FileUtils.cleanDirectory(new File("src/test/resources/filestore"));
  }

  @Test
  void getLocalSourcePathOfFileExceptionTest() throws IOException {
    Mockito.when(gcsService.downloadFile(anyString(), anyString())).thenReturn(new byte[0]);
    File file = new File(NEW_FILE_LOCATION);
    file.getParentFile().mkdirs();
    File sourceFile = new File(SOURCE_FILE_LOCATION);
    FileUtils.copyFile(sourceFile, file);
    String sourcePath =
        fileStorageService.getLocalSourcePathOfFile(SOURCE_PATH, PREFIX_PATH_FILE_STORE, true, PRODUCT_CODE_FILE_NAME,
            false);
    Mockito.verify(gcsProperties, Mockito.times(2)).getPathPrefix();
    Mockito.verify(gcsProperties).getSourceImageDirectory();
    Mockito.verify(gcsProperties).getSourceImageBucketName();
    Mockito.verify(gcsService).downloadFileTo(anyString(), anyString(), anyString());
    FileUtils.cleanDirectory(new File("src/test/resources/filestore"));
  }

  @Test
  void saveResizedImagesTest() throws Exception {
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    createFileForTest(DESTINATION_PATH);
    fileStorageService.saveImages(DESTINATION_PATH, new ImageResponse(), true, PRODUCT_CODE_FILE_NAME, false);
    Mockito.verify(gcsProperties).isSourceImageEnabled();
    Mockito.verify(gcsProperties).getSourceResizeImageDirectory();
    Mockito.verify(gcsProperties).getPathPrefix();
    Mockito.verify(gcsService).uploadFile(any(), anyString(), any());
    Mockito.verify(gcsProperties).getResizeImagePrefix();
  }

  @Test
  void saveResizedImagesTempLocationTest() throws Exception {
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    createFileForTest(DESTINATION_PATH);
    fileStorageService.saveImages(DESTINATION_PATH, new ImageResponse(), true,
        "/temp_directory_resize_image_/" + (UUID.randomUUID().toString() + File.separator
            + PRODUCT_CODE_FILE_NAME).replaceAll("/", "_"), false);
    Mockito.verify(gcsProperties).isSourceImageEnabled();
    Mockito.verify(gcsProperties).getSourceResizeImageDirectory();
    Mockito.verify(gcsProperties).getPathPrefix();
    Mockito.verify(gcsService).uploadFile(any(), anyString(), any());
    Mockito.verify(gcsProperties).getResizeImagePrefix();
  }

  @Test
  void saveResizedImagesWrongTempLocationTest() throws Exception {
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    createFileForTest(DESTINATION_PATH);
    fileStorageService.saveImages(DESTINATION_PATH, new ImageResponse(), true, PRODUCT_CODE_FILE_NAME, false);
    Mockito.verify(gcsProperties).isSourceImageEnabled();
    Mockito.verify(gcsProperties).getSourceResizeImageDirectory();
    Mockito.verify(gcsProperties).getPathPrefix();
    Mockito.verify(gcsService).uploadFile(any(), anyString(), any());
    Mockito.verify(gcsProperties).getResizeImagePrefix();
  }

  private static void createFileForTest(String destinationFileName) throws IOException {
    File file = new File(destinationFileName);
    file.getParentFile().mkdirs();
    File sourceFile = new File(SOURCE_FILE_LOCATION);
    FileUtils.copyFile(sourceFile, file);
  }

  @Test
  void saveResizedImagesResizeFalseTest() throws Exception {
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    createFileForTest(DESTINATION_PATH);
    fileStorageService.saveImages(DESTINATION_PATH, new ImageResponse(), false,
        "/temp_directory_resize_image/" + File.separator + UUID.randomUUID().toString() + PRODUCT_CODE_FILE_NAME,
        false);
    Mockito.verify(gcsProperties).isSourceImageEnabled();
    Mockito.verify(gcsProperties).isFinalImageEnabled();
    FileUtils.cleanDirectory(new File("src/test/resources/filestore"));
  }

  @Test
  void saveResizedImagesResizeFalseScaleTempFileTest() throws Exception {
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    createFileForTest(DESTINATION_PATH);
    fileStorageService.saveImages(DESTINATION_PATH, new ImageResponse(), false,
        "/temp_directory_scale_image/" + File.separator + UUID.randomUUID().toString() + PRODUCT_CODE_FILE_NAME, false);
    Mockito.verify(gcsProperties).isSourceImageEnabled();
    Mockito.verify(gcsProperties).isFinalImageEnabled();
    FileUtils.cleanDirectory(new File("src/test/resources/filestore"));
  }

  @Test
  void saveResizedImagesGcsFlagFalseTest() throws Exception {
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    createFileForTest(DESTINATION_PATH);
    fileStorageService.saveImages(DESTINATION_PATH, new ImageResponse(), true, PRODUCT_CODE_FILE_NAME, false);
    Mockito.verify(gcsProperties).isSourceImageEnabled();
    Mockito.verify(gcsProperties).isFinalImageEnabled();
    FileUtils.cleanDirectory(new File("src/test/resources/filestore"));
  }

  @Test
  void saveResizedImagesGcsFlagResizeFalseTest() throws Exception {
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    createFileForTest(DESTINATION_PATH);
    fileStorageService.saveImages(DESTINATION_PATH, new ImageResponse(), false, PRODUCT_CODE_FILE_NAME, false);
    Mockito.verify(gcsProperties).isSourceImageEnabled();
    Mockito.verify(gcsProperties).isFinalImageEnabled();
    FileUtils.cleanDirectory(new File("src/test/resources/filestore"));
  }

  @Test
  void saveImagesFinalImageScaleFullTest() throws Exception {
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(true);
    String destinationPath =
        "src/test/resources/filestore5/wcsstore/Indraprastha/images/catalog/full/101/MTA-48580336/download.jpeg";
    createFileForTest(destinationPath);
    String productCodeFileName = "MTA-48580336/download.jpeg";
    ImageResponse imageResultDetail = new ImageResponse();
    fileStorageService.saveImages(destinationPath, imageResultDetail, false, productCodeFileName, false);
    Mockito.verify(gcsProperties).isFinalImageEnabled();
    Mockito.verify(gcsService).uploadFile(any(), anyString(), any());
    Mockito.verify(gcsProperties).isSourceImageEnabled();
    Mockito.verify(gcsProperties, Mockito.times(3)).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties).getPathPrefix();
    Assertions.assertTrue(imageResultDetail.getImagePathLocation().equals("catalog-image/101/MTA-48580336/download.jpeg"));
    FileUtils.cleanDirectory(new File("src/test/resources/filestore5"));
  }

  @Test
  void saveImagesFinalImageScaleMediumTest() throws Exception {
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(true);
    String destinationPath =
        "src/test/resources/filestore5/wcsstore/Indraprastha/images/catalog/medium/101/MTA-48580336/download.jpeg";
    createFileForTest(destinationPath);
    String productCodeFileName = "MTA-48580336/download.jpeg";
    ImageResponse imageResultDetail = new ImageResponse();
    fileStorageService.saveImages(destinationPath, imageResultDetail, false, productCodeFileName, false);
    Mockito.verify(gcsProperties).isFinalImageEnabled();
    Mockito.verify(gcsService).uploadFile(any(), anyString(), any());
    Mockito.verify(gcsProperties).isSourceImageEnabled();
    Mockito.verify(gcsProperties, Mockito.times(3)).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalMediumImageDirectory();
    Mockito.verify(gcsProperties).getPathPrefix();
    Assertions.assertTrue(imageResultDetail.getImagePathLocation().equals("catalog-image/101/MTA-48580336/download.jpeg"));
    FileUtils.cleanDirectory(new File("src/test/resources/filestore5"));
  }

  @Test
  void saveImagesFinalImageScaleThumbnailTest() throws Exception {
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(true);
    String destinationPath =
        "src/test/resources/filestore5/wcsstore/Indraprastha/images/catalog/thumbnail/101/MTA-48580336/download.jpeg";
    createFileForTest(destinationPath);
    String productCodeFileName = "MTA-48580336/download.jpeg";
    ImageResponse imageResultDetail = new ImageResponse();
    fileStorageService.saveImages(destinationPath, imageResultDetail, false, productCodeFileName, false);
    Mockito.verify(gcsProperties).isFinalImageEnabled();
    Mockito.verify(gcsService).uploadFile(any(), anyString(), any());
    Mockito.verify(gcsProperties).isSourceImageEnabled();
    Mockito.verify(gcsProperties, Mockito.times(3)).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalThumbnailImageDirectory();
    Mockito.verify(gcsProperties).getPathPrefix();
    Assertions.assertTrue(imageResultDetail.getImagePathLocation().equals("catalog-image/101/MTA-48580336/download.jpeg"));
    FileUtils.cleanDirectory(new File("src/test/resources/filestore5"));
  }

  @Test
  void saveImagesFinalImageScaleSeoulTest() throws Exception {
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(true);
    String destinationPath =
        "src/test/resources/filestore5/wcsstore/Indraprastha/images/catalog/101/MTA-48580336/download.jpeg";
    createFileForTest(destinationPath);
    String productCodeFileName = "MTA-48580336/download.jpeg";
    ImageResponse imageResultDetail = new ImageResponse();
    fileStorageService.saveImages(destinationPath, imageResultDetail, false, productCodeFileName, false);
    Mockito.verify(gcsProperties).isFinalImageEnabled();
    Mockito.verify(gcsService).uploadFile(any(), anyString(), any());
    Mockito.verify(gcsProperties).isSourceImageEnabled();
    Mockito.verify(gcsProperties, Mockito.times(3)).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalSeoulImageDirectory();
    Mockito.verify(gcsProperties).getPathPrefix();
    Assertions.assertTrue(imageResultDetail.getImagePathLocation().equals("catalog-image/101/MTA-48580336/download.jpeg"));
    FileUtils.cleanDirectory(new File("src/test/resources/filestore5"));
  }

  @Test
  void saveImagesFinalImageEditedFullTest() throws Exception {
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(true);
    String destinationPath =
        "src/test/resources/filestore6/wcsstore/Indraprastha/images/catalog/full/MTA-48580336/download.jpeg";
    createFileForTest(destinationPath);
    String productCodeFileName = "MTA-48580336/download.jpeg";
    ImageResponse imageResultDetail = new ImageResponse();
    fileStorageService.saveImages(destinationPath, imageResultDetail, false, productCodeFileName, true);
    Mockito.verify(gcsProperties).isFinalImageEnabled();
    Mockito.verify(gcsService).uploadFile(any(), anyString(), any());
    Mockito.verify(gcsProperties).isSourceImageEnabled();
    Mockito.verify(gcsProperties, Mockito.times(3)).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties).getPathPrefix();
    Assertions.assertTrue(imageResultDetail.getImagePathLocation().equals("catalog-image/MTA-48580336/download.jpeg"));
    FileUtils.cleanDirectory(new File("src/test/resources/filestore6"));
  }

  @Test
  void saveImagesFinalImageEditedMediumTest() throws Exception {
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(true);
    String destinationPath =
        "src/test/resources/filestore6/wcsstore/Indraprastha/images/catalog/medium/MTA-48580336/download.jpeg";
    createFileForTest(destinationPath);
    String productCodeFileName = "MTA-48580336/download.jpeg";
    ImageResponse imageResultDetail = new ImageResponse();
    fileStorageService.saveImages(destinationPath, imageResultDetail, false, productCodeFileName, true);
    Mockito.verify(gcsProperties).isFinalImageEnabled();
    Mockito.verify(gcsService).uploadFile(any(), anyString(), any());
    Mockito.verify(gcsProperties).isSourceImageEnabled();
    Mockito.verify(gcsProperties, Mockito.times(3)).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalMediumImageDirectory();
    Mockito.verify(gcsProperties).getPathPrefix();
    Assertions.assertTrue(imageResultDetail.getImagePathLocation().equals("catalog-image/MTA-48580336/download.jpeg"));
    FileUtils.cleanDirectory(new File("src/test/resources/filestore6"));
  }

  @Test
  void saveImagesFinalImageEditedThumbnailTest() throws Exception {
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(true);
    String destinationPath =
        "src/test/resources/filestore6/wcsstore/Indraprastha/images/catalog/thumbnail/MTA-48580336/download.jpeg";
    createFileForTest(destinationPath);
    String productCodeFileName = "MTA-48580336/download.jpeg";
    ImageResponse imageResultDetail = new ImageResponse();
    fileStorageService.saveImages(destinationPath, imageResultDetail, false, productCodeFileName, true);
    Mockito.verify(gcsProperties).isFinalImageEnabled();
    Mockito.verify(gcsService).uploadFile(any(), anyString(), any());
    Mockito.verify(gcsProperties).isSourceImageEnabled();
    Mockito.verify(gcsProperties, Mockito.times(3)).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalThumbnailImageDirectory();
    Mockito.verify(gcsProperties).getPathPrefix();
    Assertions.assertTrue(imageResultDetail.getImagePathLocation().equals("catalog-image/MTA-48580336/download.jpeg"));
    FileUtils.cleanDirectory(new File("src/test/resources/filestore6"));
  }

  @Test
  void saveImagesFinalImageEditedSeoulTest() throws Exception {
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(true);
    String destinationPath =
        "src/test/resources/filestore6/wcsstore/Indraprastha/images/catalog/MTA-48580336/download.jpeg";
    createFileForTest(destinationPath);
    String productCodeFileName = "MTA-48580336/download.jpeg";
    ImageResponse imageResultDetail = new ImageResponse();
    fileStorageService.saveImages(destinationPath, imageResultDetail, false, productCodeFileName, true);
    Mockito.verify(gcsProperties).isFinalImageEnabled();
    Mockito.verify(gcsService).uploadFile(any(), anyString(), any());
    Mockito.verify(gcsProperties).isSourceImageEnabled();
    Mockito.verify(gcsProperties, Mockito.times(3)).getFinalImageDirectory();
    Mockito.verify(gcsProperties).getFinalSeoulImageDirectory();
    Mockito.verify(gcsProperties).getPathPrefix();
    Assertions.assertTrue(imageResultDetail.getImagePathLocation().equals("catalog-image/MTA-48580336/download.jpeg"));
    FileUtils.cleanDirectory(new File("src/test/resources/filestore6"));
  }

  @Test
  void saveImagesFinalImageEditedFullResizeTrueTest() throws Exception {
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(true);
    String destinationPath =
        "src/test/resources/filestore6/wcsstore/Indraprastha/images/catalog/full/MTA-48580336/download.jpeg";
    createFileForTest(destinationPath);
    String productCodeFileName = "MTA-48580336/download.jpeg";
    ImageResponse imageResultDetail = new ImageResponse();
    fileStorageService.saveImages(destinationPath, imageResultDetail, true, productCodeFileName, true);
    Mockito.verify(gcsProperties).isFinalImageEnabled();
    Mockito.verify(gcsProperties).isSourceImageEnabled();
    FileUtils.cleanDirectory(new File("src/test/resources/filestore6"));
  }

  @Test
  void saveImagesFinalImageScaleGroupCodeAsFullTest() throws Exception {
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(true);
    String destinationPath =
        "src/test/resources/filestore5/wcsstore/Indraprastha/images/catalog/full/MTA-48580336/download.jpeg";
    createFileForTest(destinationPath);
    String productCodeFileName = "MTA-48580336/download.jpeg";
    ImageResponse imageResultDetail = new ImageResponse();
    try {
      fileStorageService.saveImages(destinationPath, imageResultDetail, false, productCodeFileName, false);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(gcsProperties).isFinalImageEnabled();
      Mockito.verify(this.gcsProperties).isFinalImageEnabled();
      Mockito.verify(gcsProperties).isSourceImageEnabled();
      FileUtils.cleanDirectory(new File("src/test/resources/filestore5"));
    }
  }

  @Test
  void saveImagesFinalImageScaleGroupCodeAsMediumTest() throws Exception {
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(true);
    String destinationPath =
        "src/test/resources/filestore5/wcsstore/Indraprastha/images/catalog/medium/MTA-48580336/download.jpeg";
    createFileForTest(destinationPath);
    String productCodeFileName = "MTA-48580336/download.jpeg";
    ImageResponse imageResultDetail = new ImageResponse();
    try {
      fileStorageService.saveImages(destinationPath, imageResultDetail, false, productCodeFileName, false);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(gcsProperties).isFinalImageEnabled();
      Mockito.verify(this.gcsProperties).isFinalImageEnabled();
      Mockito.verify(gcsProperties).isSourceImageEnabled();
      FileUtils.cleanDirectory(new File("src/test/resources/filestore5"));
    }
  }

  @Test
  void saveImagesFinalImageScaleGroupCodeAsThumbnailTest() throws Exception {
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(true);
    String destinationPath =
        "src/test/resources/filestore5/wcsstore/Indraprastha/images/catalog/thumbnail/MTA-48580336/download.jpeg";
    createFileForTest(destinationPath);
    String productCodeFileName = "MTA-48580336/download.jpeg";
    ImageResponse imageResultDetail = new ImageResponse();
    try {
      fileStorageService.saveImages(destinationPath, imageResultDetail, false, productCodeFileName, false);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(gcsProperties).isFinalImageEnabled();
      Mockito.verify(this.gcsProperties).isFinalImageEnabled();
      Mockito.verify(gcsProperties).isSourceImageEnabled();
      FileUtils.cleanDirectory(new File("src/test/resources/filestore5"));
    }
  }

  @Test
  void saveImagesFinalImageScaleGroupCodeAsSeoulTest() throws Exception {
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(true);
    String destinationPath =
        "src/test/resources/filestore5/wcsstore/Indraprastha/images/catalog/MTA-48580336/download.jpeg";
    createFileForTest(destinationPath);
    String productCodeFileName = "MTA-48580336/download.jpeg";
    ImageResponse imageResultDetail = new ImageResponse();
    try {
      fileStorageService.saveImages(destinationPath, imageResultDetail, false, productCodeFileName, false);
    } catch (ApplicationRuntimeException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(gcsProperties).isFinalImageEnabled();
      Mockito.verify(this.gcsProperties).isFinalImageEnabled();
      Mockito.verify(gcsProperties).isSourceImageEnabled();
      FileUtils.cleanDirectory(new File("src/test/resources/filestore5"));
    }
  }

  @Test
  void deletingTempFileUsedForResizeImageGcsTest() throws Exception {
    createFileForTest(SOURCE_PATH_AFTER_RESIZE);
    fileStorageService.deletingTempFileUsedForResizeImageGcs(SOURCE_PATH_AFTER_RESIZE);
    FileUtils.cleanDirectory(new File("src/test/resources/filestore"));
  }

  @Test
  void deletingTempFileUsedForResizeImageGcsForScalingTest() throws Exception {
    createFileForTest(SOURCE_PATH_AFTER_SCALE);
    fileStorageService.deletingTempFileUsedForResizeImageGcs(SOURCE_PATH_AFTER_SCALE);
    FileUtils.cleanDirectory(new File("src/test/resources/filestore"));
  }

  @Test
  void deletingTempFileUsedForResizeImageGcsForSwitchOffTest() throws Exception {
    File file = new File(SOURCE_FILE_LOCATION);
    file.getParentFile().mkdirs();
    fileStorageService.deletingTempFileUsedForResizeImageGcs(SOURCE_FILE_LOCATION);
    FileUtils.cleanDirectory(new File("src/test/resources/filestore"));
  }

  @Test
  void deletingTempFileUsedForResizeImageGcsFileNotExistsTest() throws Exception {
    fileStorageService.deletingTempFileUsedForResizeImageGcs(SOURCE_PATH_AFTER_RESIZE + SOURCE_PATH_AFTER_RESIZE);
  }

  @Test
  void deleteTempCreatedImagesTest() throws IOException {
    createFileForTest(NEW_FILE_LOCATION);
    fileStorageService.deleteTempCreatedImages(Arrays.asList(NEW_FILE_LOCATION));
    fileStorageService.deleteTempCreatedImages(new ArrayList<>());
    FileUtils.cleanDirectory(new File("src/test/resources/filestore"));
  }

  @Test
  void downloadImagesAndSaveToTempLocationTest() throws IOException {
    createFileForTest(NEW_FILE_LOCATION);
    GraphicImageDetail graphicImageDetail =
        new GraphicImageDetail("", SOURCE_PATH, "", null, PREFIX_PATH_FILE_STORE, "");
    Collection<String> tempFiles =
        fileStorageService.downloadImagesAndSaveToTempLocation(Arrays.asList(graphicImageDetail), true, false);
    Mockito.verify(gcsProperties, Mockito.times(2)).getPathPrefix();
    Mockito.verify(gcsProperties).getSourceImageDirectory();
    Mockito.verify(gcsProperties).getSourceImageBucketName();
    Mockito.verify(gcsService).downloadFileTo(anyString(), anyString(), anyString());
    Assertions.assertEquals(1, tempFiles.size());
    Assertions.assertTrue(tempFiles.iterator().next().contains("temp_directory_resize_image"));
    FileUtils.cleanDirectory(new File("src/test/resources/filestore"));
  }

  @Test
  void deletingFileAndDirectoryPathInvalidTest() {
    fileStorageService.deletingFileAndDirectory(PATH_PREFIX, PATH_PREFIX);
  }

  @Test
  void deleteDestinationPathTest() throws IOException {
    createFileForTest(NEW_FILE_RESIZE_LOCATION);
    createFileForTest(NEW_FILE_SCALE_LOCATION);
    createFileForTest(NEW_FILE_SEOUL_LOCATION);
    createFileForTest(NEW_FILE_NONE_LOCATION);

    Mockito.when(imagePathConfiguration.getLocationPrefix(RESIZE)).thenReturn(RESIZE);
    Mockito.when(imagePathConfiguration.getLocationPrefix(SCALE)).thenReturn(SCALE);
    Mockito.when(imagePathConfiguration.getLocationPrefix(SEOUL)).thenReturn(SEOUL);
    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(true);

    fileStorageService.deleteDestinationPath(
        Arrays.asList(NEW_FILE_RESIZE_LOCATION, NEW_FILE_SCALE_LOCATION, NEW_FILE_SEOUL_LOCATION, NEW_FILE_NONE_LOCATION),
        true);

    Mockito.when(gcsProperties.isSourceImageEnabled()).thenReturn(false);
    fileStorageService.deleteDestinationPath(
        Arrays.asList(NEW_FILE_RESIZE_LOCATION, NEW_FILE_SCALE_LOCATION, NEW_FILE_SEOUL_LOCATION, NEW_FILE_LOCATION),
        true);

    fileStorageService.deleteDestinationPath(
        Arrays.asList(NEW_FILE_RESIZE_LOCATION, NEW_FILE_SCALE_LOCATION, NEW_FILE_SEOUL_LOCATION, NEW_FILE_LOCATION),
        false);

    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(false);
    fileStorageService.deleteDestinationPath(
        Arrays.asList(NEW_FILE_RESIZE_LOCATION, NEW_FILE_SCALE_LOCATION, NEW_FILE_SEOUL_LOCATION, NEW_FILE_LOCATION),
        false);

    fileStorageService.deleteDestinationPath(new ArrayList<>(), true);

    Mockito.verify(imagePathConfiguration, Mockito.times(30)).getLocationPrefix(anyString());
    Mockito.verify(gcsProperties, Mockito.times(7)).isSourceImageEnabled();
    Mockito.verify(gcsProperties, Mockito.times(8)).isFinalImageEnabled();
    FileUtils.cleanDirectory(new File("src/test/resources/filestore"));
  }

  @Test
  void uploadToGcsRMATest() {
    Mockito.when(gcsProperties.isScaleGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getRmaClientId()).thenReturn(XRMA);
    Assertions.assertTrue(fileStorageService.uploadToGcsRMA(XRMA));

    Mockito.when(gcsProperties.getRmaClientId()).thenReturn(OXFORD);
    Assertions.assertFalse(fileStorageService.uploadToGcsRMA(XRMA));

    Mockito.when(gcsProperties.isScaleGcsEnabled()).thenReturn(false);
    Assertions.assertFalse(fileStorageService.uploadToGcsRMA(XRMA));

    Mockito.verify(gcsProperties, Mockito.times(3)).isScaleGcsEnabled();
    Mockito.verify(gcsProperties, Mockito.times(2)).getRmaClientId();
  }

  @Test
  void uploadToGcsOxfordTest() {
    Mockito.when(gcsProperties.isStoreGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getOxfordClientId()).thenReturn(OXFORD);
    Assertions.assertTrue(fileStorageService.uploadToGcsOxford(OXFORD));

    Mockito.when(gcsProperties.getOxfordClientId()).thenReturn(XRMA);
    Assertions.assertFalse(fileStorageService.uploadToGcsOxford(OXFORD));

    Mockito.when(gcsProperties.isStoreGcsEnabled()).thenReturn(false);
    Assertions.assertFalse(fileStorageService.uploadToGcsOxford(OXFORD));

    Mockito.verify(gcsProperties, Mockito.times(3)).isStoreGcsEnabled();
    Mockito.verify(gcsProperties, Mockito.times(2)).getOxfordClientId();
  }

  @Test
  void createTemporaryFileRMATest() throws Exception {
    InputStream inputStream = new FileInputStream(SOURCE_FILE_LOCATION);
    Mockito.when(gcsProperties.isScaleGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getRmaClientId()).thenReturn(XRMA);
    Mockito.when(gcsProperties.getRmaTemporaryImageSourcePath()).thenReturn("target/rma");
    fileStorageService.createTemporaryFile(inputStream, ".tmp", XRMA);
    Mockito.verify(gcsProperties).getRmaTemporaryImageSourcePath();
    Mockito.verify(gcsProperties).isScaleGcsEnabled();
    Mockito.verify(gcsProperties).getRmaClientId();
  }

  @Test
  void createTemporaryFileOxfordTest() throws Exception {
    InputStream inputStream = new FileInputStream(SOURCE_FILE_LOCATION);
    Mockito.when(gcsProperties.isScaleGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getRmaClientId()).thenReturn(XRMA);
    Mockito.when(gcsProperties.isStoreGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getOxfordClientId()).thenReturn(OXFORD);
    Mockito.when(gcsProperties.getOxfordTemporaryImageSourcePath()).thenReturn("target/oxford");
    fileStorageService.createTemporaryFile(inputStream, ".tmp", OXFORD);
    Mockito.verify(gcsProperties).getOxfordTemporaryImageSourcePath();
    Mockito.verify(gcsProperties).isStoreGcsEnabled();
    Mockito.verify(gcsProperties).getOxfordClientId();
    Mockito.verify(gcsProperties).isScaleGcsEnabled();
    Mockito.verify(gcsProperties).getRmaClientId();
  }

  @Test
  void createTemporaryFileTest() throws Exception {
    InputStream inputStream = new FileInputStream(SOURCE_FILE_LOCATION);
    Mockito.when(gcsProperties.isScaleGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getRmaClientId()).thenReturn(XRMA);
    Mockito.when(gcsProperties.isStoreGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getOxfordClientId()).thenReturn(OXFORD);
    fileStorageService.createTemporaryFile(inputStream, ".tmp", RESIZE);
    Mockito.verify(gcsProperties).isStoreGcsEnabled();
    Mockito.verify(gcsProperties).getOxfordClientId();
    Mockito.verify(gcsProperties).isScaleGcsEnabled();
    Mockito.verify(gcsProperties).getRmaClientId();
  }

  @Test
  void getImageLocationPathPrefixTest() {
    Mockito.when(gcsProperties.isScaleGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getRmaClientId()).thenReturn(XRMA);
    Mockito.when(gcsProperties.getRmaTemporaryImageDestinationPath()).thenReturn("tmp/rma");
    fileStorageService.getImageLocationPathPrefix(XRMA);

    Mockito.when(gcsProperties.isStoreGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getOxfordClientId()).thenReturn(OXFORD);
    Mockito.when(gcsProperties.getOxfordTemporaryImageDestinationPath()).thenReturn("tmp/oxford");
    fileStorageService.getImageLocationPathPrefix(OXFORD);


    Mockito.when(imagePathConfiguration.getLocationPrefix(RESIZE)).thenReturn("tmp/resize");
    fileStorageService.getImageLocationPathPrefix(RESIZE);

    Mockito.verify(gcsProperties, Mockito.times(3)).isScaleGcsEnabled();
    Mockito.verify(gcsProperties, Mockito.times(3)).getRmaClientId();
    Mockito.verify(gcsProperties, Mockito.times(2)).isStoreGcsEnabled();
    Mockito.verify(gcsProperties, Mockito.times(2)).getOxfordClientId();
    Mockito.verify(gcsProperties).getRmaTemporaryImageDestinationPath();
    Mockito.verify(gcsProperties).getOxfordTemporaryImageDestinationPath();
  }

  @Test
  void uploadToGcsTest() throws Exception {
    ImageResultDetail imageResultDetail = new ImageResultDetail();
    imageResultDetail.setImagePathLocation(SOURCE_FILE_LOCATION.replace("src/",""));

    Mockito.when(gcsProperties.getRmaClientId()).thenReturn(XRMA);
    Mockito.when(gcsProperties.getRmaGcsPath()).thenReturn("rma");
    Mockito.when(gcsProperties.getRmaTemporaryImageDestinationPath()).thenReturn("src");
    fileStorageService.uploadToGcs(imageResultDetail, XRMA);

    Mockito.when(gcsProperties.getOxfordClientId()).thenReturn(OXFORD);
    Mockito.when(gcsProperties.getOxfordGcsPath()).thenReturn("oxford");
    Mockito.when(gcsProperties.getOxfordTemporaryImageDestinationPath()).thenReturn("src");
    Mockito.when(gcsProperties.getOrderClientId()).thenReturn(ORDER);
    Mockito.when(gcsProperties.getActiveProductNewImageClientId()).thenReturn(SCALE);
    fileStorageService.uploadToGcs(imageResultDetail, OXFORD);

    fileStorageService.uploadToGcs(imageResultDetail, RESIZE);

    Mockito.verify(gcsProperties, Mockito.times(3)).getRmaClientId();
    Mockito.verify(gcsProperties).getRmaGcsPath();
    Mockito.verify(gcsProperties, Mockito.times(2)).getRmaTemporaryImageDestinationPath();
    Mockito.verify(gcsProperties, Mockito.times(2)).getOxfordClientId();
    Mockito.verify(gcsProperties).getOxfordGcsPath();
    Mockito.verify(gcsProperties).getOrderClientId();
    Mockito.verify(gcsProperties, Mockito.times(2)).getActiveProductNewImageClientId();
    Mockito.verify(gcsProperties, Mockito.times(2)).getOxfordTemporaryImageDestinationPath();
    Mockito.verify(gcsService, Mockito.times(2)).uploadFile(any(Bucket.class), anyString(), any());
  }

  @Test
  void deleteFromTemporaryLocation() throws Exception {
    createFileForTest("target/rma/sample1.jpg");
    createFileForTest("target/oxford/sample2.jpg");

    Mockito.when(gcsProperties.getRmaClientId()).thenReturn(XRMA);
    Mockito.when(gcsProperties.getRmaTemporaryImageDestinationPath()).thenReturn("src");
    fileStorageService.deleteFromTemporaryLocation("target/rma/sample1.jpg", XRMA);
    fileStorageService.deleteFromTemporaryLocation("target/rma/sample.jpg", XRMA);

    Mockito.when(gcsProperties.getOxfordClientId()).thenReturn(OXFORD);
    Mockito.when(gcsProperties.getOxfordTemporaryImageDestinationPath()).thenReturn("src");
    fileStorageService.deleteFromTemporaryLocation("target/oxford/sample2.jpg", OXFORD);
    fileStorageService.deleteFromTemporaryLocation("target/oxford/sample.jpg", OXFORD);

    fileStorageService.deleteFromTemporaryLocation("target/oxford/sample.jpg", RESIZE);

    Mockito.verify(gcsProperties, Mockito.times(5)).getRmaClientId();
    Mockito.verify(gcsProperties, Mockito.times(4)).getRmaTemporaryImageDestinationPath();
    Mockito.verify(gcsProperties, Mockito.times(3)).getOxfordClientId();
    Mockito.verify(gcsProperties, Mockito.times(4)).getOxfordTemporaryImageDestinationPath();
  }

  @Test
  void uploadToAndDeleteFromTempLocationGcsTest() throws Exception {
    ImageResultDetail imageResultDetail = new ImageResultDetail();
    imageResultDetail.setImagePathLocation("rma/sample1.jpg");
    GraphicDetailCommand graphicImageDetail =
        new GraphicDetailCommand(null, "target/rma/sample2.jpg", null, null, null, true);
    createFileForTest("target/rma/sample1.jpg");
    createFileForTest("target/rma/sample2.jpg");
    Mockito.when(gcsProperties.getRmaClientId()).thenReturn(XRMA);
    Mockito.when(gcsProperties.getRmaGcsPath()).thenReturn("rma");
    Mockito.when(gcsProperties.getRmaTemporaryImageDestinationPath()).thenReturn("target");
    fileStorageService.uploadToAndDeleteFromTempLocationGcs(true, XRMA, Arrays.asList(graphicImageDetail),
        Arrays.asList(imageResultDetail));

    fileStorageService.uploadToAndDeleteFromTempLocationGcs(true, XRMA, new ArrayList<>(), new ArrayList<>());

    fileStorageService.uploadToAndDeleteFromTempLocationGcs(false, XRMA, new ArrayList<>(), new ArrayList<>());

    Mockito.verify(gcsProperties, Mockito.times(2)).getRmaClientId();
    Mockito.verify(gcsProperties, Mockito.times(4)).getRmaTemporaryImageDestinationPath();
    Mockito.verify(gcsProperties).getRmaGcsPath();
    Mockito.verify(gcsService).uploadFile(any(Bucket.class), anyString(), any());
  }

  @Test
  void uploadToAndDeleteFromTempLocationGcsTest_tempFileNotPresent() throws Exception {
    ImageResultDetail imageResultDetail = new ImageResultDetail();
    imageResultDetail.setImagePathLocation("rma/sample1.jpg");
    GraphicDetailCommand graphicImageDetail =
        new GraphicDetailCommand(null, "target/rma/sample2.jpg", null, null, null, true);
    createFileForTest("target/rma/sample1.jpg");
    Mockito.when(gcsProperties.getRmaClientId()).thenReturn(XRMA);
    Mockito.when(gcsProperties.getRmaGcsPath()).thenReturn("rma");
    Mockito.when(gcsProperties.getRmaTemporaryImageDestinationPath()).thenReturn("target");
    fileStorageService.uploadToAndDeleteFromTempLocationGcs(true, XRMA, Arrays.asList(graphicImageDetail),
        Arrays.asList(imageResultDetail));

    fileStorageService.uploadToAndDeleteFromTempLocationGcs(true, XRMA, new ArrayList<>(), new ArrayList<>());

    fileStorageService.uploadToAndDeleteFromTempLocationGcs(false, XRMA, new ArrayList<>(), new ArrayList<>());

    Mockito.verify(gcsProperties, Mockito.times(2)).getRmaClientId();
    Mockito.verify(gcsProperties, Mockito.times(4)).getRmaTemporaryImageDestinationPath();
    Mockito.verify(gcsProperties).getRmaGcsPath();
    Mockito.verify(gcsService).uploadFile(any(Bucket.class), anyString(), any());
  }

  @Test
  void gcsToFileForXRMA() throws IOException {
    Mockito.when(gcsProperties.getRmaBucketName()).thenReturn("BUCKET");
    Mockito.when(gcsProperties.isDisplayGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getRmaClientId()).thenReturn("xrma");
    fileStorageService.gcsToFileForXRMA("imagePath","xrma");
    Mockito.verify(gcsProperties).getRmaBucketName();
    Mockito.verify(gcsProperties).isDisplayGcsEnabled();
    Mockito.verify(gcsProperties).getRmaClientId();
    Mockito.verify(gcsProperties).getRmaGcsPath();
    Mockito.verify(gcsService).downloadFile(any(), any());
  }

  @Test
  void gcsToFileForXRMAIsDisplayFalse() throws IOException {
    Mockito.when(gcsProperties.isDisplayGcsEnabled()).thenReturn(false);
    fileStorageService.gcsToFileForXRMA("imagePath","xrma");
    Mockito.verify(gcsProperties).isDisplayGcsEnabled();
  }

  @Test
  void gcsToFileForXRMAFalse() throws IOException {
    Mockito.when(gcsProperties.isDisplayGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getRmaClientId()).thenReturn("otherClient");
    fileStorageService.gcsToFileForXRMA("imagePath","xrma");
    Mockito.verify(gcsProperties).isDisplayGcsEnabled();
    Mockito.verify(gcsProperties).getRmaClientId();
  }

  @Test
  void gcsToFileForXRMAContentEmpty() throws IOException {
    Mockito.when(gcsProperties.getRmaBucketName()).thenReturn("BUCKET");
    Mockito.when(gcsProperties.isDisplayGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getRmaClientId()).thenReturn("xrma");
    Mockito.when(gcsProperties.getRmaGcsPath()).thenReturn("xrma");
    Mockito.when(gcsService.downloadFile(any(), any())).thenReturn(null);
    fileStorageService.gcsToFileForXRMA("imagePath","xrma");
    Mockito.verify(gcsProperties).getRmaBucketName();
    Mockito.verify(gcsProperties).isDisplayGcsEnabled();
    Mockito.verify(gcsProperties).getRmaClientId();
    Mockito.verify(gcsProperties).getRmaGcsPath();
    Mockito.verify(gcsProperties).isRMAImageMigrationDone();
    Mockito.verify(gcsService).downloadFile(any(), any());
  }

  @Test
  void gcsToFileForXRMAContentIsRmaMigrationIsTrue() throws IOException {
    Mockito.when(gcsProperties.getRmaBucketName()).thenReturn("BUCKET");
    Mockito.when(gcsProperties.isDisplayGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getRmaClientId()).thenReturn("xrma");
    Mockito.when(gcsProperties.getRmaGcsPath()).thenReturn("xrma");
    Mockito.when(gcsProperties.isRMAImageMigrationDone()).thenReturn(true);
    Mockito.when(gcsService.downloadFile(any(), any())).thenReturn(null);
    fileStorageService.gcsToFileForXRMA("imagePath","xrma");
    Mockito.verify(gcsProperties).getRmaBucketName();
    Mockito.verify(gcsProperties).isDisplayGcsEnabled();
    Mockito.verify(gcsProperties).getRmaClientId();
    Mockito.verify(gcsProperties).getRmaGcsPath();
    Mockito.verify(gcsProperties).isRMAImageMigrationDone();
    Mockito.verify(gcsService).downloadFile(any(), any());
  }

  @Test
  void gcsToFileForXRMAContentIsRmaMigrationIsFileExists() {
    Mockito.when(gcsProperties.isDisplayGcsEnabled()).thenReturn(false);
    Mockito.when(imagePathConfiguration.getLocationPrefix(any())).thenReturn("src/test");
    try {
      fileStorageService.gcsToFileForXRMA("/resources", "xrma");
    } catch (IOException ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(gcsProperties).isDisplayGcsEnabled();
      Mockito.verify(imagePathConfiguration).getLocationPrefix(any());
    }
  }

  @Test
  void gcsRemoveForOxforeTest() throws Exception {
    Mockito.when(gcsProperties.getOxfordBucketName()).thenReturn("BUCKET");
    Mockito.when(gcsProperties.isRemoveGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getOxfordClientId()).thenReturn("oxford");
    Mockito.when(gcsProperties.getOxfordGcsPath()).thenReturn(null);
    fileStorageService.gcsRemoveForOxford("imagePath","oxford");
    Mockito.verify(gcsProperties,Mockito.times(2)).getOxfordBucketName();
    Mockito.verify(gcsProperties).isRemoveGcsEnabled();
    Mockito.verify(gcsProperties).getOxfordClientId();
    Mockito.verify(gcsProperties).getRemoveGcsPathForOxford();
    Mockito.verify(gcsProperties,Mockito.times(3)).getOxfordGcsPath();
    Mockito.verify(gcsService).downloadFile(any(), any());
    Mockito.verify(gcsService).uploadFile(any(),any(),any());
    Mockito.verify(gcsService).deleteFile(any(),any());
  }
  @Test
  void gcsRemoveForOxforeTestForMigrationDone() throws Exception {
    Mockito.when(gcsProperties.getOxfordBucketName()).thenReturn("BUCKET");
    Mockito.when(gcsProperties.isRemoveGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getOxfordClientId()).thenReturn("oxford");
    Mockito.when(gcsProperties.getOxfordGcsPath()).thenReturn(null);
    Mockito.when(gcsService.downloadFile(any(), any())).thenReturn(null);
    Mockito.when(gcsProperties.isOxfordImageMigrationDone()).thenReturn(true);
    Mockito.when(gcsService.downloadFile(any(), any())).thenReturn(null);
    fileStorageService.gcsRemoveForOxford("imagePath","oxford");
    Mockito.verify(gcsProperties, Mockito.times(1)).getOxfordBucketName();
    Mockito.verify(gcsProperties).isRemoveGcsEnabled();
    Mockito.verify(gcsProperties).isOxfordImageMigrationDone();
    Mockito.verify(gcsProperties, Mockito.times(1)).getOxfordClientId();
    Mockito.verify(gcsProperties, Mockito.times(1)).getOxfordGcsPath();
    Mockito.verify(gcsService).downloadFile(any(), any());
  }

  @Test
  void gcsRemoveForOxforeisRemoveGcsFalseTest() throws Exception {
    Mockito.when(gcsProperties.isRemoveGcsEnabled()).thenReturn(false);
    fileStorageService.gcsRemoveForOxford("imagePath","oxford");
    Mockito.verify(gcsProperties).isRemoveGcsEnabled();
  }

  @Test
  void gcsRemoveForOxforeIsRemoveGcsTrueTest() throws Exception {
    Mockito.when(gcsProperties.isRemoveGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getOxfordClientId()).thenReturn("SomeClient");
    fileStorageService.gcsRemoveForOxford("imagePath","oxford");
    Mockito.verify(gcsProperties).isRemoveGcsEnabled();
    Mockito.verify(gcsProperties).getOxfordClientId();
  }

  @Test
  void gcsRemoveForOxfordContentEmptyForFileisExistAndNotDirectory() throws Exception {
    try {
      createFileForTest("target/rma/sample1.jpg");
      Mockito.when(gcsProperties.getOxfordBucketName()).thenReturn("BUCKET");
      Mockito.when(gcsProperties.isRemoveGcsEnabled()).thenReturn(true);
      Mockito.when(gcsProperties.getOxfordClientId()).thenReturn("oxford");
      Mockito.when(gcsProperties.getOxfordGcsPath()).thenReturn(null);
      Mockito.when(gcsService.downloadFile(any(), any())).thenReturn(null);
      Mockito.when(gcsProperties.isOxfordImageMigrationDone()).thenReturn(false);
      Mockito.when(gcsService.downloadFile(any(), any())).thenReturn(null);
      Mockito.when(imagePathConfiguration.getLocationPrefix(any())).thenReturn("target/");
      Mockito.when(imagePathConfiguration.getLocationPrefix("oxford.remove")).thenReturn("src/");
      fileStorageService.gcsRemoveForOxford("rma/sample1.jpg", "oxford");
    } finally {
      Mockito.verify(gcsProperties, Mockito.times(1)).getOxfordBucketName();
      Mockito.verify(gcsProperties).isRemoveGcsEnabled();
      Mockito.verify(gcsProperties).isOxfordImageMigrationDone();
      Mockito.verify(gcsProperties, Mockito.times(1)).getOxfordClientId();
      Mockito.verify(gcsProperties, Mockito.times(2)).getOxfordGcsPath();
      Mockito.verify(gcsService).uploadFile(any(),any(),any());
      Mockito.verify(gcsProperties).getRemoveGcsPathForOxford();
      Mockito.verify(gcsService).downloadFile(any(), any());
      Mockito.verify(imagePathConfiguration, Mockito.times(2)).getLocationPrefix(any());
      Mockito.verify(gcsService, Mockito.times(1)).downloadFile(any(), any());
    }
  }

  @Test
  void gcsRemoveForOxfordContentEmptyForFileisExistAndDirectory1() throws Exception {
    try {
      createFileForTest("target/rma/sample1.jpg");
      Mockito.when(gcsProperties.getOxfordBucketName()).thenReturn("BUCKET");
      Mockito.when(gcsProperties.isRemoveGcsEnabled()).thenReturn(true);
      Mockito.when(gcsProperties.getOxfordClientId()).thenReturn("oxford");
      Mockito.when(gcsProperties.getOxfordGcsPath()).thenReturn(null);
      Mockito.when(gcsService.downloadFile(any(), any())).thenReturn(null);
      Mockito.when(gcsProperties.isOxfordImageMigrationDone()).thenReturn(false);
      Mockito.when(gcsService.downloadFile(any(), any())).thenReturn(null);
      Mockito.when(imagePathConfiguration.getLocationPrefix(any())).thenReturn("target/");
      fileStorageService.gcsRemoveForOxford("rma/", "oxford");
    } catch (Exception ex){
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(gcsProperties, Mockito.times(1)).getOxfordBucketName();
      Mockito.verify(gcsProperties).isRemoveGcsEnabled();
      Mockito.verify(gcsProperties).isOxfordImageMigrationDone();
      Mockito.verify(gcsProperties, Mockito.times(1)).getOxfordClientId();
      Mockito.verify(gcsProperties, Mockito.times(1)).getOxfordGcsPath();
      Mockito.verify(gcsService).downloadFile(any(), any());
      Mockito.verify(imagePathConfiguration, Mockito.times(2)).getLocationPrefix(any());
      Mockito.verify(gcsService, Mockito.times(1)).downloadFile(any(), any());
    }
  }

  @Test
  void createTemporaryFileInGCSRmaScaling_test() throws Exception {
    byte[] bytes = "Test Data".getBytes();
    String path = "testPath";
    String gcsPath = "gcsPath";

    Mockito.when(gcsProperties.getRmaGcsPath()).thenReturn(gcsPath);
    Mockito.when(gcsService.uploadFile(eq(rmaImageBucket), anyString(), eq(bytes))).thenReturn(
        Mockito.mock(Blob.class));

    fileStorageService.createTemporaryFileInGCSRmaScaling(bytes, path);

    Mockito.verify(gcsService).uploadFile(eq(rmaImageBucket), eq(gcsPath + "/" + path), eq(bytes));
    Mockito.verify(gcsProperties).getRmaGcsPath();
  }

  @Test
  void createTemporaryFileInGCSRmaScaling_failureTest() throws Exception {
    byte[] bytes = "Test Data".getBytes();
    String path = "testPath";
    String gcsPath = "gcsPath";

    Mockito.when(gcsProperties.getRmaGcsPath()).thenReturn(gcsPath);
    Mockito.when(gcsService.uploadFile(eq(rmaImageBucket), anyString(), eq(bytes))).thenThrow(
        ApplicationRuntimeException.class);

    fileStorageService.createTemporaryFileInGCSRmaScaling(bytes, path);

    Mockito.verify(gcsService).uploadFile(eq(rmaImageBucket), eq(gcsPath + "/" + path), eq(bytes));
    Mockito.verify(gcsProperties).getRmaGcsPath();
  }

  @Test
  void uploadToGcsXOrderTest() throws Exception {
    ImageResultDetail imageResultDetail = new ImageResultDetail();
    imageResultDetail.setImagePathLocation(SOURCE_FILE_LOCATION);
    Mockito.when(gcsProperties.getRmaClientId()).thenReturn(XRMA);
    Mockito.when(gcsProperties.getOxfordClientId()).thenReturn(OXFORD);
    Mockito.when(gcsProperties.getOrderClientId()).thenReturn(ORDER);
    fileStorageService.uploadToGcs(imageResultDetail, ORDER);
    Mockito.verify(gcsProperties).getOrderClientId();
    Mockito.verify(gcsProperties).getOxfordClientId();
    Mockito.verify(gcsProperties).getRmaClientId();
    Mockito.verify(gcsService, Mockito.times(1))
      .uploadFile(any(Bucket.class), anyString(), any());
  }

  @Test
  void uploadToGcsActiveProductTest() throws Exception {
    ImageResultDetail imageResultDetail = new ImageResultDetail();
    imageResultDetail.setImagePathLocation(SOURCE_FILE_LOCATION);
    imageResultDetail.setDestinationPath(SOURCE_FILE_LOCATION);
    Mockito.when(gcsProperties.getRmaClientId()).thenReturn(XRMA);
    Mockito.when(gcsProperties.getOxfordClientId()).thenReturn(OXFORD);
    Mockito.when(gcsProperties.getOrderClientId()).thenReturn(ORDER);
    Mockito.when(gcsProperties.getActiveProductNewImageClientId()).thenReturn(SCALE);
    fileStorageService.uploadToGcs(imageResultDetail, SCALE);
    Mockito.verify(gcsProperties).getOrderClientId();
    Mockito.verify(gcsProperties).getOxfordClientId();
    Mockito.verify(gcsProperties).getRmaClientId();
    Mockito.verify(gcsProperties, Mockito.times(2)).getActiveProductNewImageClientId();
    Mockito.verify(gcsService, Mockito.times(1))
      .uploadFile(any(Bucket.class), anyString(), any());
  }

  @Test
  void uploadToGcsActiveImageProductTest() throws Exception {
    ImageResultDetail imageResultDetail = new ImageResultDetail();
    imageResultDetail.setImagePathLocation(SOURCE_FILE_LOCATION);
    imageResultDetail.setDestinationPath(SOURCE_FILE_LOCATION);
    imageResultDetail.setActive(true);
    Mockito.when(gcsProperties.getRmaClientId()).thenReturn(XRMA);
    Mockito.when(gcsProperties.getOxfordClientId()).thenReturn(OXFORD);
    Mockito.when(gcsProperties.getOrderClientId()).thenReturn(ORDER);
    Mockito.when(gcsProperties.getActiveProductNewImageClientId()).thenReturn(SCALE);
    fileStorageService.uploadToGcs(imageResultDetail, SCALE);
    Mockito.verify(gcsProperties).getOrderClientId();
    Mockito.verify(gcsProperties).getOxfordClientId();
    Mockito.verify(gcsProperties).getRmaClientId();
    Mockito.verify(gcsProperties, Mockito.times(1)).getActiveProductNewImageClientId();
    Mockito.verify(gcsService, Mockito.times(1)).uploadFile(any(Bucket.class), anyString(), any());
  }
}