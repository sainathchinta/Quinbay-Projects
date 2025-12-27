package com.gdn.partners.pcu.internal.service.impl;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Map;

import javax.imageio.ImageIO;

import com.gdn.partners.pcu.internal.properties.ImageProperties;
import com.gdn.partners.pcu.internal.service.model.UploadAttributeImageRequest;
import com.gdn.partners.pcu.internal.web.model.request.UploadImageRequest;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.google.cloud.storage.Blob;
import com.google.common.collect.ImmutableMap;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.internal.client.feign.XGPFeign;
import com.gdn.partners.pcu.internal.properties.GCSProperties;
import com.gdn.partners.pcu.internal.properties.SystemParameterProperties;
import com.gdn.partners.pcu.internal.service.GCSService;
import com.gdn.partners.pcu.internal.service.impl.exception.ImageValidationException;
import com.gdn.x.productcategorybase.dto.brand.BrandApproveRequest;
import com.gdn.x.productcategorybase.dto.brand.UpdateBrandRequest;
import com.google.cloud.storage.Bucket;
import org.springframework.test.util.ReflectionTestUtils;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
class FileStorageServiceImplTest {

  @InjectMocks
  private FileStorageServiceImpl fileStorageService;

  @Mock
  private GCSService gcsService;

  @Mock
  private GCSProperties gcsProperties;

  @Mock
  private ImageProperties imageProperties;

  @Mock
  private XGPFeign xgpFeign;

  @Mock
  private SystemParameterProperties systemParameterProperties;

  @Mock
  private Bucket bulkBucket;

  @Mock
  private Bucket brandBucket;

  @Mock
  private Bucket attributeBucket;

  @Mock
  private MultipartFile multipartFileData;

  @Captor
  private ArgumentCaptor<String> stringArgumentCaptor;

  @Mock
  private Blob blob;

  @Mock
  private Bucket sourceImageBucket;

  private static final String baseDirPath = "src/test/resources/ExcelTemplate";
  private static final String filePath = "src/test/resources/ExcelTemplate";
  private static final String PATH = "src/test/resources/ExcelTemplate";
  private static final String FILE = "/originalFilename.xls";
  private static final String RESOURCE_PATH = "src/test/resources/";
  private MockMultipartFile multipartFile;
  private static ClassLoader classLoader;
  private static File file;
  private static final String ORIGINAL_FILENAME = "originalFilename.xls";
  private static final String REQUEST_ID = "requestId";
  private static final String BRAND_AUTH_ADD = "BRAND_AUTH_ADD";
  private static final String BRAND_AUTH_DELETE = "BRAND_AUTH_DELETE";

  private static final String BULK_APPROVAL = "BULK_APPROVAL";
  private static final String BULK_REJECTION = "BULK_REJECTION";
  private static final String INTERNAL_BULK_UPLOAD = "internal";
  private static final String INTERNAL_SUSPENSION = "suspension";
  private static final String STORE_COPY = "store-copy";
  private static final String SALES_CATEORY = "sales-category";
  private static final String RECAT = "recat";
  private static final String BULK_CONFIG = "config";
  private static final String VENDOR = "vendor";
  private static final String IMAGE_FILE_NAME = "/MTA-10000.jpg";
  private static final String INVALID_IMAGE_FILE_NAME = "invalid";
  private String[] splitImageFilenameByDash;
  private static final String DEFAULT = "default";
  private static final String FULL_IMAGE_TYPE = "full-MTA.jpg";
  private static final String IMAGE_PATH = RESOURCE_PATH + "image";
  private static final String SLASH_SEPARATOR = "/";
  private static final String GCS_FILE_PREFIX = "catalog-image";
  private static final String GCS_BUCKET_NAME = "bucket-name";
  private static final String GCS_SOURCE_IMAGE_DIR = "image-source-dir";
  private static final String FULL = "full";
  private static final String FILE_NAME = "/MTA-10000.jpg";
  private static final String BRAND_CODE ="BRAND-CODE";
  private static final String SELLER_CODE ="SELLER-CODE";
  private static final String BRAND_AUTH_DOC = "document.pdf";
  private static final String BRAND_SOURCE_DIRECTORY = "/brandSourceDirectory";
  private static final String BRAND_FINAL_DIRECTORY = "/brandFinalDirectory";
  private static final String LOCATION_PATH = "dummy-excel copy.jpeg";
  private static final String DEFAULT_BRAND_LOGO_PATH = "blibli-com-logo.";
  private static final String BRAND_LOCATION_PATH = "/brandSourceDirectory/brandLogo/BRAND-CODE/blibli-com-logo.";
  private static final String FINAL_BRAND_LOCATION = "/brandFinalDirectory/brandLogo/BRAND-CODE/blibli-com-logo.";
  private static final String BRAND_LOGO = "brandLogo";
  private static final String BRAND_PROFILE = "brandProfile";
  private static final String FILE_STORE_PATH = "src/test/resources/brandSourceDirectory/brandLogo";
  private static final String MASTER_SKU_BULK_ASSIGNEE = "MASTER_SKU_BULK_ASSIGNEE";
  private static final String MASTER_SKU_BULK_REVIEW = "MASTER_SKU_BULK_REVIEW";
  private static final String IPR_PORTAL_BULK_ADD_REVIEW = "IPR_PORTAL_BULK_ADD_REVIEW";
  private static final String BULK_PRICE_UPDATE = "BULK_PRICE_UPDATE";
  private static final String ACTIVE_IMAGE_FILE_NAME_1 = "/apple_mouse_full01.jpg";
  private static final String ACTIVE_IMAGE_FILE_NAME_2 = "/apple_mouse_full02.jpeg";
  private static final String ACTIVE_IMAGE_FILE_NAME_3 = "/apple_mouse_full03.png";
  private static final String PATH_INPUT = "src/test/resources/images/input";
  private static final String PATH_OUTPUT = "src/test/resources/images/output";
  private static final String PATH_PREFIX = "catalog-image";
  private static final String PATH_FULL= "/full";
  private static final String PATH_MEDIUM= "/medium";
  private static final String PATH_THUMBNAIL= "/thumbnail";
  private static final String PRODUCT_CODE = "MTA-0315060";
  private static final String USERNAME = "username";
  private static final String FILE_FOLDER = "Product";
  private static final String PATH_1 = "path";
  private static final String FILE_2 = "/MTA-0315060/apple_mouse_full02.jpg";
  private static  String JPG = "jpg";
  private static  String JPEG = "jpeg";

  private byte[] fileContent;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    fileContent = new byte[] {-1, -40, -20, -10};
    splitImageFilenameByDash = new String[1];
    splitImageFilenameByDash[0] = FULL_IMAGE_TYPE;
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(gcsService);
    verifyNoMoreInteractions(gcsProperties);
    verifyNoMoreInteractions(systemParameterProperties);
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    file.getParentFile().mkdirs();
    FileUtils.writeByteArrayToFile(file, fileContent);
  }

  private void deleteFolder(String folderPath) throws IOException {
    FileUtils.deleteDirectory(new File(folderPath));
  }

  @Test
  void getFilePathGCSEnabledTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.TRUE);
    mockFile(baseDirPath + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(baseDirPath + FILE)));
    when(systemParameterProperties.getGcsUploadPath()).thenReturn(filePath);
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any()))
        .thenReturn(baseDirPath);
    String path = fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, INTERNAL_BULK_UPLOAD);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
    Mockito.verify(gcsProperties, times(2)).isEnabled();
    Mockito.verify(systemParameterProperties).getGcsInternalUploadPath();
  }

  @Test
  void getSuspensionFilePathGCSEnabledTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.TRUE);
    mockFile(baseDirPath + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(baseDirPath + FILE)));
    when(systemParameterProperties.getGcsUploadPath()).thenReturn(filePath);
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any()))
        .thenReturn(baseDirPath);
    String path = fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, INTERNAL_SUSPENSION);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
    Mockito.verify(gcsProperties, times(2)).isEnabled();
    Mockito.verify(systemParameterProperties).getGcsSuspensionUploadPath();
  }

  @Test
  void getBulkPriceUpdateFilePathGCSEnabledTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.TRUE);
    mockFile(baseDirPath + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(baseDirPath + FILE)));
    when(systemParameterProperties.getGcsUploadPath()).thenReturn(filePath);
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any()))
        .thenReturn(baseDirPath);
    String path = fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, BULK_PRICE_UPDATE);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
    Mockito.verify(gcsProperties).isEnabled();
    Mockito.verify(systemParameterProperties).getBulkPriceUpdateUploadPath();
  }

  @Test
  void getStoreCopyFilePathGCSEnabledTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.TRUE);
    mockFile(baseDirPath + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(baseDirPath + FILE)));
    when(systemParameterProperties.getGcsUploadPath()).thenReturn(filePath);
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any()))
        .thenReturn(baseDirPath);
    String path = fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, STORE_COPY);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
    Mockito.verify(gcsProperties, times(2)).isEnabled();
    Mockito.verify(systemParameterProperties).getGcsStoreCopyUploadPath();
  }

  @Test
  void getSalesCategoryFilePathGCSEnabledTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.TRUE);
    mockFile(baseDirPath + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(baseDirPath + FILE)));
    when(systemParameterProperties.getGcsUploadPath()).thenReturn(filePath);
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any()))
        .thenReturn(baseDirPath);
    String path = fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, SALES_CATEORY);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
    Mockito.verify(gcsProperties, times(2)).isEnabled();
    Mockito.verify(systemParameterProperties).getGcsSalesCategoryUploadPath();
  }

  @Test
  void getRecatFilePathGCSEnabledTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.TRUE);
    mockFile(baseDirPath + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(baseDirPath + FILE)));
    when(systemParameterProperties.getGcsUploadPath()).thenReturn(filePath);
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any()))
        .thenReturn(baseDirPath);
    String path = fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, RECAT);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
    Mockito.verify(gcsProperties, times(2)).isEnabled();
    Mockito.verify(systemParameterProperties).getGcsRecatUploadPath();
  }

  @Test
  void getVendorFilePathGCSEnabledTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.TRUE);
    mockFile(baseDirPath + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(baseDirPath + FILE)));
    when(systemParameterProperties.getGcsUploadPath()).thenReturn(filePath);
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any()))
        .thenReturn(baseDirPath);
    String path = fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, VENDOR);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
    Mockito.verify(gcsProperties, times(2)).isEnabled();
    Mockito.verify(systemParameterProperties).getGcsVendorUploadPath();
  }

  @Test
  void getBrandAuthAddFilePathGCSEnabledTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.TRUE);
    mockFile(baseDirPath + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(baseDirPath + FILE)));
    when(systemParameterProperties.getGcsUploadPath()).thenReturn(filePath);
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any()))
        .thenReturn(baseDirPath);
    String path = fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, BRAND_AUTH_ADD);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
    Mockito.verify(gcsProperties, times(1)).isEnabled();
    Mockito.verify(systemParameterProperties).getBulkBrandAuthAddUploadPath();
  }

  @Test
  void getBrandAuthDeleteFilePathGCSEnabledTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.TRUE);
    mockFile(baseDirPath + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(baseDirPath + FILE)));
    when(systemParameterProperties.getGcsUploadPath()).thenReturn(filePath);
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any()))
        .thenReturn(baseDirPath);
    String path = fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, BRAND_AUTH_DELETE);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
    Mockito.verify(gcsProperties, times(1)).isEnabled();
    Mockito.verify(systemParameterProperties).getBulkBrandAuthDeleteUploadPath();
  }

  @Test
  void getBulkReviewApprovalFilePathGCSEnabledTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.TRUE);
    mockFile(baseDirPath + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
      FileUtils.readFileToByteArray(new File(baseDirPath + FILE)));
    when(systemParameterProperties.getGcsUploadPath()).thenReturn(filePath);
    Mockito.when(
        gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any()))
      .thenReturn(baseDirPath);
    String path = fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, BULK_APPROVAL);
    Mockito.verify(gcsService)
      .uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
    Mockito.verify(gcsProperties, times(1)).isEnabled();
    Mockito.verify(systemParameterProperties).getBulkReviewApprovalUploadPath();
  }

  @Test
  void getBulkReviewRejectionFilePathGCSEnabledTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.TRUE);
    mockFile(baseDirPath + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
      FileUtils.readFileToByteArray(new File(baseDirPath + FILE)));
    when(systemParameterProperties.getGcsUploadPath()).thenReturn(filePath);
    Mockito.when(
        gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any()))
      .thenReturn(baseDirPath);
    String path = fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, BULK_REJECTION);
    Mockito.verify(gcsService)
      .uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
    Mockito.verify(gcsProperties, times(1)).isEnabled();
    Mockito.verify(systemParameterProperties).getBulkReviewRejectionUploadPath();
  }

  @Test
  void getConfigFilePathGCSEnabledTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.TRUE);
    mockFile(baseDirPath + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(baseDirPath + FILE)));
    when(systemParameterProperties.getGcsUploadPath()).thenReturn(filePath);
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any()))
        .thenReturn(baseDirPath);
    String path = fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, BULK_CONFIG);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
    Mockito.verify(gcsProperties, times(2)).isEnabled();
    Mockito.verify(systemParameterProperties).getGcsBulkConfigUploadPath();
  }


  @Test
  void getFilePathDefaultTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.TRUE);
    mockFile(baseDirPath + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(baseDirPath + FILE)));
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any()))
        .thenReturn(baseDirPath);
    String path = fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, DEFAULT);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
    Mockito.verify(gcsProperties, times(1)).isEnabled();
  }

  @Test
  void getFilePathTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.FALSE);
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(systemParameterProperties.getBulkUploadFilePath()).thenReturn(PATH);
    String path = fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, INTERNAL_BULK_UPLOAD);
    Mockito.verify(gcsProperties, times(2)).isEnabled();
    Mockito.verify(systemParameterProperties).getBulkUploadFilePath();
  }

  @Test
  void getSuspensionFilePathTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.FALSE);
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(systemParameterProperties.getBulkProductSuspensionFilePath()).thenReturn(PATH);
    String path = fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, INTERNAL_SUSPENSION);
    Mockito.verify(gcsProperties, times(2)).isEnabled();
    Mockito.verify(systemParameterProperties).getBulkProductSuspensionFilePath();
  }

  @Test
  void getStoreCopyFilePathTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.FALSE);
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(systemParameterProperties.getStoreCopyUploadFilePath()).thenReturn(PATH);
    String path = fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, STORE_COPY);
    Mockito.verify(gcsProperties, times(2)).isEnabled();
    Mockito.verify(systemParameterProperties).getStoreCopyUploadFilePath();
  }

  @Test
  void getSalesCategoryFilePathTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.FALSE);
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(systemParameterProperties.getSalesCategoryUploadFilePath()).thenReturn(PATH);
    String path = fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, SALES_CATEORY);
    Mockito.verify(gcsProperties, times(2)).isEnabled();
    Mockito.verify(systemParameterProperties).getSalesCategoryUploadFilePath();
  }

  @Test
  void getRecatFilePathTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.FALSE);
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(systemParameterProperties.getRecatUploadFilePath()).thenReturn(PATH);
    String path = fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, RECAT);
    Mockito.verify(gcsProperties, times(2)).isEnabled();
    Mockito.verify(systemParameterProperties).getRecatUploadFilePath();
  }

  @Test
  void getConfigFilePathTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.FALSE);
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(systemParameterProperties.getBulkConfigUploadFilePath()).thenReturn(PATH);
    String path = fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, BULK_CONFIG);
    Mockito.verify(gcsProperties, times(2)).isEnabled();
    Mockito.verify(systemParameterProperties).getBulkConfigUploadFilePath();
  }

  @Test
  void getVendorFilePathTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.FALSE);
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(systemParameterProperties.getVendorBulkAssignFilePath()).thenReturn(PATH);
    String path = fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, VENDOR);
    Mockito.verify(gcsProperties, times(2)).isEnabled();
    Mockito.verify(systemParameterProperties).getVendorBulkAssignFilePath();
  }

  @Test
  void getInternalDownloadTemplateFilePathsGCSTest() {
    ReflectionTestUtils.setField(fileStorageService, "internalTemplatesGcsPath", ImmutableMap.of(
            "CategoryConfigUploadTemplate", "/xbulk-assets/templates/CategoryConfigUpload" + ".xlsx'"));
    Map<String, String> result = fileStorageService.getInternalDownloadTemplateFilePaths();
    Assertions.assertEquals(1, result.size());
  }

  @Test
  void checkImage1SizeTest() throws IOException {
    mockImageFile(IMAGE_PATH + SLASH_SEPARATOR + FULL_IMAGE_TYPE);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_FILE_PREFIX);
    fileStorageService.checkImageSize(IMAGE_FILE_NAME, splitImageFilenameByDash,
        new StringBuilder(IMAGE_PATH));
    Mockito.verify(gcsProperties).getPathPrefix();
  }

  @Test
  void getImageSizeTest() throws IOException {
    byte[] imageContent = new byte[10];
    when(gcsProperties.getFinalImageDirectory()).thenReturn("final");
    when(gcsProperties.getFinalFullImageDirectory()).thenReturn("full");
    when(gcsProperties.getFinalImageBucketName()).thenReturn("final-bucket");
    when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString())).thenReturn(imageContent);
    fileStorageService.getImageSize(IMAGE_FILE_NAME);
    verify(gcsService).downloadFile(Mockito.anyString(), Mockito.anyString());
    verify(gcsProperties).getFinalImageDirectory();
    verify(gcsProperties).getFinalFullImageDirectory();
    verify(gcsProperties).getFinalImageBucketName();
  }

  @Test
  void getImageSizeExceptionTest() throws IOException {
    byte[] imageContent = new byte[0];
    when(gcsProperties.getFinalImageDirectory()).thenReturn("final");
    when(gcsProperties.getFinalFullImageDirectory()).thenReturn("full");
    when(gcsProperties.getFinalImageBucketName()).thenReturn("final-bucket");
    when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString())).thenThrow(
        ApplicationRuntimeException.class);
    Mockito.when(xgpFeign.checkImageSizeByImageFilename(SLASH_SEPARATOR + FULL + FILE_NAME))
        .thenReturn(new GdnRestSimpleResponse<>(null, null, true, null, StringUtils.EMPTY));
    fileStorageService.getImageSize(IMAGE_FILE_NAME);
    verify(gcsProperties).getFinalImageDirectory();
    verify(gcsProperties).getFinalFullImageDirectory();
    verify(gcsService).downloadFile(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(xgpFeign).checkImageSizeByImageFilename(SLASH_SEPARATOR + FULL + FILE_NAME);
    verify(gcsProperties).getFinalImageBucketName();
  }

  @Test
  void checkImage2SizeTest() throws IOException {
    splitImageFilenameByDash = new String[2];
    splitImageFilenameByDash[0] = FULL_IMAGE_TYPE;
    splitImageFilenameByDash[1] = StringUtils.EMPTY;
    mockImageFile(IMAGE_PATH + SLASH_SEPARATOR + FULL_IMAGE_TYPE);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_FILE_PREFIX);
    fileStorageService.checkImageSize(IMAGE_FILE_NAME, splitImageFilenameByDash, new StringBuilder(IMAGE_PATH));
    Mockito.verify(gcsProperties).getPathPrefix();
  }

  @Test
  void checkImage3SizeTest() throws IOException {
    splitImageFilenameByDash = new String[3];
    splitImageFilenameByDash[0] = FULL_IMAGE_TYPE;
    splitImageFilenameByDash[1] = StringUtils.EMPTY;
    splitImageFilenameByDash[2] = StringUtils.EMPTY;
    mockImageFile(IMAGE_PATH + SLASH_SEPARATOR + FULL_IMAGE_TYPE);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_FILE_PREFIX);
    int size =
        fileStorageService.checkImageSize(IMAGE_FILE_NAME, splitImageFilenameByDash, new StringBuilder(IMAGE_PATH));
    Mockito.verify(gcsProperties).getPathPrefix();
  }

  @Test
  void checkImageExceptionSizeTest() throws IOException {
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_FILE_PREFIX);
    try {
      fileStorageService.checkImageSize(INVALID_IMAGE_FILE_NAME, splitImageFilenameByDash,
          new StringBuilder(INVALID_IMAGE_FILE_NAME));
    }
    catch (ImageValidationException ex){
      Assertions.assertNotNull(ex);
    }
    finally {
      Mockito.verify(gcsProperties).getPathPrefix();
    }
  }

  @Test
  void checkImageSizeGCSTest() throws IOException {
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(GCS_FILE_PREFIX);
    Mockito.when(gcsProperties.getBucketName()).thenReturn(GCS_BUCKET_NAME);
    Mockito.when(gcsProperties.getSourceImageDirectory()).thenReturn(GCS_SOURCE_IMAGE_DIR);
    Mockito.when(gcsService.downloadFile(Mockito.any(), Mockito.any())).thenReturn(fileContent);
    fileStorageService.checkImageSize(GCS_FILE_PREFIX, splitImageFilenameByDash,
        new StringBuilder(INVALID_IMAGE_FILE_NAME));
    Mockito.verify(gcsProperties).getPathPrefix();
    Mockito.verify(gcsProperties).getSourceImageBucketName();
    Mockito.verify(gcsProperties).getSourceImageDirectory();
    Mockito.verify(gcsService).downloadFile(Mockito.any(), Mockito.any());
  }

  @Test
  void uploadBrandAuthDocTest() throws Exception {
    byte[] imageContent = new byte[10];
    when(systemParameterProperties.getGcsBulkBasePath()).thenReturn(PATH);
    when(gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any()))
      .thenReturn(baseDirPath);
    fileStorageService.uploadBrandAuthDoc(IMAGE_FILE_NAME,BRAND_CODE,SELLER_CODE, imageContent);
    verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
    verify(systemParameterProperties).getGcsBulkBasePath();
  }

  @Test
  void checkIfFileExistingTest() throws Exception {
    String path = "/BRAND-CODE/SELLER-CODE/document.pdf";
    when(systemParameterProperties.getGcsBulkBasePath()).thenReturn(StringUtils.EMPTY);
    when(gcsProperties.getBucketName()).thenReturn(GCS_BUCKET_NAME);
    when(gcsService.downloadFile(GCS_BUCKET_NAME, path)).thenReturn(new byte[1]);
    fileStorageService.checkIfFileExisting(BRAND_AUTH_DOC, BRAND_CODE, SELLER_CODE);
    verify(gcsService).downloadFile(GCS_BUCKET_NAME, path);
    verify(gcsProperties).getBucketName();
    verify(systemParameterProperties).getGcsBulkBasePath();
  }

  private MultipartFile generateMultipartFile() throws Exception {
    classLoader = this.getClass().getClassLoader();
    mockFile("target/test-classes/Brand/" + DEFAULT_BRAND_LOGO_PATH);
    file = new File(classLoader.getResource("Brand/" + DEFAULT_BRAND_LOGO_PATH).getFile());
    multipartFile = new MockMultipartFile(DEFAULT_BRAND_LOGO_PATH, DEFAULT_BRAND_LOGO_PATH, "image/jpg",
        IOUtils.toByteArray(new FileInputStream(file)));
    return multipartFile;
  }

  @Test
  void uploadBrandFileToGcsTest() throws Exception {
    String fileName =
        fileStorageService.uploadBrandFileToGcs(multipartFileData, LOCATION_PATH);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(brandBucket), Mockito.any(), Mockito.any());
    Assertions.assertEquals(fileName, "dummy-excel copy.jpeg");
  }

  @Test
  void uploadBrandFileToGcsEmptyLocationPathTest() throws Exception {
    fileStorageService.uploadBrandFileToGcs(multipartFileData, "");
  }

  @Test
  void uploadBrandFileToGcsNullFileTest() throws Exception {
    fileStorageService.uploadBrandFileToGcs(null, LOCATION_PATH);
  }

  @Test
  void uploadBrandFileToGcsEmptyFileTest() throws Exception {
    MultipartFile multipartFile1 = new MockMultipartFile("request", new byte[0]);
    String filename = fileStorageService.uploadBrandFileToGcs(multipartFile1, LOCATION_PATH);
  }

  @Test
  void createBrandLogoFileTest() throws Exception {
    BrandApproveRequest brandApproveRequest = new BrandApproveRequest();
    brandApproveRequest.setBrandLogoPath(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(true);
    Mockito.when(systemParameterProperties.getDirectoryBrandLogoSource()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(systemParameterProperties.getDirectoryBrandLogoFinal()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(gcsProperties.getBrandSourceDirectory()).thenReturn(BRAND_SOURCE_DIRECTORY);
    Mockito.when(gcsProperties.getBrandLogoDirectory()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(brandBucket), Mockito.any(), Mockito.any())).thenReturn(new String(""));
    fileStorageService.createBrandLogoFile(brandApproveRequest, multipartFileData, BRAND_CODE);
    Mockito.verify(gcsService, times(1)).uploadCreatedFile(Mockito.eq(brandBucket), Mockito.any(), Mockito.any());
    verify(gcsProperties).isBrandGcsEnabled();
    verify(gcsProperties).getBrandLogoDirectory();
    verify(gcsProperties).getBrandSourceDirectory();
  }

  @Test
  void createBrandLogoFileFalseTest() throws Exception {
    MultipartFile multipart = generateMultipartFile();
    BrandApproveRequest brandApproveRequest = new BrandApproveRequest();
    brandApproveRequest.setBrandLogoPath(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(false);
    Mockito.when(systemParameterProperties.getDirectoryBrandLogoSource()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    fileStorageService.createBrandLogoFile(brandApproveRequest, multipart, BRAND_CODE);
    verify(systemParameterProperties, times(1)).getDirectoryBrandLogoSource();
    verify(gcsProperties).isBrandGcsEnabled();
  }

  @Test
  void createProfileBannerFileTest() throws Exception {
    BrandApproveRequest brandApproveRequest = new BrandApproveRequest();
    brandApproveRequest.setProfileBannerPath(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getBrandSourceDirectory()).thenReturn(BRAND_SOURCE_DIRECTORY);
    Mockito.when(gcsProperties.getBrandProfileBannerDirectory()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(brandBucket), Mockito.any(), Mockito.any())).thenReturn(new String(""));
    fileStorageService.createBrandProfileBannerFile(brandApproveRequest, multipartFileData, BRAND_CODE);
    Mockito.verify(gcsService, times(1)).uploadCreatedFile(Mockito.eq(brandBucket), Mockito.any(), Mockito.any());
    verify(gcsProperties).isBrandGcsEnabled();
    verify(gcsProperties).getBrandSourceDirectory();
    verify(gcsProperties).getBrandProfileBannerDirectory();
  }

  @Test
  void createProfileBannerFileFalseTest() throws Exception {
    MultipartFile multipart = generateMultipartFile();
    BrandApproveRequest brandApproveRequest = new BrandApproveRequest();
    brandApproveRequest.setProfileBannerPath(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(false);
    Mockito.when(systemParameterProperties.getDirectoryProfileBannerSource()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    fileStorageService.createBrandProfileBannerFile(brandApproveRequest, multipart, BRAND_CODE);
    verify(systemParameterProperties, times(1)).getDirectoryProfileBannerSource();
    verify(gcsProperties).isBrandGcsEnabled();
  }

  @Test
  void approveBrandLogoTest() throws Exception {
    BrandApproveRequest brandApproveRequest = new BrandApproveRequest();
    brandApproveRequest.setBrandLogoPath(DEFAULT_BRAND_LOGO_PATH);
    brandApproveRequest.setBrandRequestCode(BRAND_CODE);
    MultipartFile multipart = generateMultipartFile();
    Mockito.when(gcsProperties.getBrandSourceDirectory()).thenReturn(BRAND_SOURCE_DIRECTORY);
    Mockito.when(gcsProperties.getBrandFinalDirectory()).thenReturn(BRAND_SOURCE_DIRECTORY);
    Mockito.when(gcsProperties.getBrandLogoDirectory()).thenReturn("brandLogo");
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getBrandBucketName()).thenReturn(GCS_BUCKET_NAME);
    Mockito.when(gcsService.downloadFile(GCS_BUCKET_NAME, BRAND_LOCATION_PATH)).thenReturn(multipart.getBytes());
    Mockito.when(gcsService.uploadCreatedFile(brandBucket, BRAND_LOCATION_PATH, multipartFile.getBytes()))
        .thenReturn(new String(""));
    Mockito.when(gcsService.deleteFile(GCS_BUCKET_NAME, BRAND_LOCATION_PATH)).thenReturn(true);
    fileStorageService.approveBrandLogo(brandApproveRequest, BRAND_CODE, multipart, BRAND_CODE);
    verify(gcsProperties).isBrandGcsEnabled();
    verify(gcsProperties, times(2)).getBrandBucketName();
    verify(gcsService).downloadFile(GCS_BUCKET_NAME, BRAND_LOCATION_PATH);
    verify(gcsService).uploadCreatedFile(brandBucket, BRAND_LOCATION_PATH, multipart.getBytes());
    verify(gcsService).deleteFile(GCS_BUCKET_NAME, BRAND_LOCATION_PATH);
    verify(gcsProperties).getBrandFinalDirectory();
    verify(gcsProperties, times(2)).getBrandSourceDirectory();
    verify(gcsProperties, times(3)).getBrandLogoDirectory();
  }

  @Test
  void approveBrandLogoFileNotExistsTest() throws Exception {
    BrandApproveRequest brandApproveRequest = new BrandApproveRequest();
    brandApproveRequest.setBrandLogoPath(DEFAULT_BRAND_LOGO_PATH);
    brandApproveRequest.setBrandRequestCode(BRAND_CODE);
    MultipartFile multipart = generateMultipartFile();
    Mockito.when(systemParameterProperties.getDirectoryBrandLogoSource()).thenReturn("brand");
    Mockito.when(gcsProperties.getBrandSourceDirectory()).thenReturn(BRAND_SOURCE_DIRECTORY);
    Mockito.when(gcsProperties.getBrandFinalDirectory()).thenReturn(BRAND_SOURCE_DIRECTORY);
    Mockito.when(gcsProperties.getBrandLogoDirectory()).thenReturn("brandLogo1111");
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getBrandBucketName()).thenReturn(GCS_BUCKET_NAME);
    Mockito.when(gcsService.downloadFile(Mockito.anyString(), Mockito.anyString())).thenThrow(ApplicationRuntimeException.class);
    Mockito.when(gcsService.uploadCreatedFile(brandBucket, BRAND_LOCATION_PATH, multipartFile.getBytes()))
        .thenReturn(new String(""));
    Mockito.when(gcsService.deleteFile(GCS_BUCKET_NAME, BRAND_LOCATION_PATH)).thenReturn(true);
    fileStorageService.approveBrandLogo(brandApproveRequest, BRAND_CODE, multipart, BRAND_CODE);
    verify(gcsProperties).isBrandGcsEnabled();
    verify(gcsProperties, times(1)).getBrandBucketName();
    verify(gcsService).downloadFile(Mockito.anyString(), Mockito.anyString());
    verify(gcsProperties, times(2)).getBrandSourceDirectory();
    verify(gcsProperties, times(2)).getBrandLogoDirectory();
    verify(systemParameterProperties).getDirectoryBrandLogoSource();
  }

  @Test
  void approveBrandLogoFalseTest() throws Exception {
    BrandApproveRequest brandApproveRequest = new BrandApproveRequest();
    brandApproveRequest.setBrandLogoPath(DEFAULT_BRAND_LOGO_PATH);
    brandApproveRequest.setBrandRequestCode(BRAND_CODE);
    MultipartFile multipart = generateMultipartFile();
    Mockito.when(systemParameterProperties.getDirectoryBrandLogoSource()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(systemParameterProperties.getDirectoryBrandLogoFinal()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(false);
    fileStorageService.approveBrandLogo(brandApproveRequest, BRAND_CODE, multipart, BRAND_CODE);
    verify(gcsProperties).isBrandGcsEnabled();
    verify(systemParameterProperties).getDirectoryBrandLogoFinal();
    verify(systemParameterProperties).getDirectoryBrandLogoSource();
  }

  @Test
  void approveProfileBannerTest() throws Exception {
    BrandApproveRequest brandApproveRequest = new BrandApproveRequest();
    brandApproveRequest.setProfileBannerPath(DEFAULT_BRAND_LOGO_PATH);
    brandApproveRequest.setBrandRequestCode(BRAND_CODE);
    MultipartFile multipart = generateMultipartFile();
    Mockito.when(gcsProperties.getBrandSourceDirectory()).thenReturn(BRAND_SOURCE_DIRECTORY);
    Mockito.when(gcsProperties.getBrandFinalDirectory()).thenReturn(BRAND_SOURCE_DIRECTORY);
    Mockito.when(gcsProperties.getBrandProfileBannerDirectory()).thenReturn("brandLogo");
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getBrandBucketName()).thenReturn(GCS_BUCKET_NAME);
    Mockito.when(gcsService.downloadFile(GCS_BUCKET_NAME, BRAND_LOCATION_PATH)).thenReturn(multipart.getBytes());
    Mockito.when(gcsService.uploadCreatedFile(brandBucket, BRAND_LOCATION_PATH, multipartFile.getBytes()))
        .thenReturn(new String(""));
    Mockito.when(gcsService.deleteFile(GCS_BUCKET_NAME, BRAND_LOCATION_PATH)).thenReturn(true);
    fileStorageService.approveProfileBanner(brandApproveRequest, BRAND_CODE, multipart, BRAND_CODE);
    verify(gcsProperties).isBrandGcsEnabled();
    verify(gcsProperties, times(2)).getBrandBucketName();
    verify(gcsService).downloadFile(GCS_BUCKET_NAME, BRAND_LOCATION_PATH);
    verify(gcsService).uploadCreatedFile(brandBucket, BRAND_LOCATION_PATH, multipart.getBytes());
    verify(gcsService).deleteFile(GCS_BUCKET_NAME, BRAND_LOCATION_PATH);
    verify(gcsProperties).getBrandFinalDirectory();
    verify(gcsProperties, times(2)).getBrandSourceDirectory();
    verify(gcsProperties, times(3)).getBrandProfileBannerDirectory();
  }

  @Test
  void approveProfileBannerFalseTest() throws Exception {
    BrandApproveRequest brandApproveRequest = new BrandApproveRequest();
    brandApproveRequest.setProfileBannerPath(DEFAULT_BRAND_LOGO_PATH);
    brandApproveRequest.setBrandRequestCode(BRAND_CODE);
    MultipartFile multipart = generateMultipartFile();
    Mockito.when(systemParameterProperties.getDirectoryProfileBannerFinal()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(systemParameterProperties.getDirectoryProfileBannerSource()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(false);
    fileStorageService.approveProfileBanner(brandApproveRequest, BRAND_CODE, multipart, BRAND_CODE);
    verify(gcsProperties).isBrandGcsEnabled();
    verify(systemParameterProperties).getDirectoryProfileBannerFinal();
    verify(systemParameterProperties).getDirectoryProfileBannerSource();
  }


  @Test
  void updateBrandFilesTest() throws Exception {
    UpdateBrandRequest updateBrandRequest = new UpdateBrandRequest();
    updateBrandRequest.setProfileBannerPath(DEFAULT_BRAND_LOGO_PATH);
    updateBrandRequest.setBrandLogoPath(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getBrandFinalDirectory()).thenReturn(BRAND_SOURCE_DIRECTORY);
    Mockito.when(gcsProperties.getBrandLogoDirectory()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(gcsProperties.getBrandProfileBannerDirectory()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(brandBucket), Mockito.any(), Mockito.any())).thenReturn(new String(""));
    fileStorageService.updateBrandFiles(updateBrandRequest, multipartFileData, multipartFileData, BRAND_CODE);
    Mockito.verify(gcsService, times(2)).uploadCreatedFile(Mockito.eq(brandBucket), Mockito.any(), Mockito.any());
    verify(gcsProperties).isBrandGcsEnabled();
    verify(gcsProperties, times(2)).getBrandFinalDirectory();
    verify(gcsProperties, times(2)).getBrandLogoDirectory();
  }

  @Test
  void updateBrandFilesFalseTest() throws Exception {
    MultipartFile multipart = generateMultipartFile();
    UpdateBrandRequest updateBrandRequest = new UpdateBrandRequest();
    updateBrandRequest.setProfileBannerPath(DEFAULT_BRAND_LOGO_PATH);
    updateBrandRequest.setBrandLogoPath(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(false);
    Mockito.when(systemParameterProperties.getDirectoryProfileBannerFinal()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(systemParameterProperties.getDirectoryBrandLogoFinal()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    fileStorageService.updateBrandFiles(updateBrandRequest, multipart, multipart, BRAND_CODE);
    verify(systemParameterProperties, times(1)).getDirectoryProfileBannerFinal();
    verify(systemParameterProperties).getDirectoryBrandLogoFinal();
    verify(gcsProperties).isBrandGcsEnabled();
  }

  @Test
  void deleteBrandLogoTest() throws Exception {
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getBrandBucketName()).thenReturn(GCS_BUCKET_NAME);
    Mockito.when(gcsProperties.getBrandFinalDirectory()).thenReturn(BRAND_FINAL_DIRECTORY);
    Mockito.when(gcsProperties.getBrandLogoDirectory()).thenReturn(BRAND_LOGO);
    Mockito.when(gcsService.deleteFile(GCS_BUCKET_NAME, FINAL_BRAND_LOCATION)).thenReturn(true);
    fileStorageService.deleteUpdatedBrandLogo(DEFAULT_BRAND_LOGO_PATH, BRAND_CODE);
    verify(gcsProperties).isBrandGcsEnabled();
    verify(gcsService).deleteFile(GCS_BUCKET_NAME, FINAL_BRAND_LOCATION);
    verify(gcsProperties).getBrandBucketName();
    verify(gcsProperties).getBrandFinalDirectory();
    verify(gcsProperties).getBrandLogoDirectory();
  }

  @Test
  void deleteBrandLogoFalseTest() throws Exception {
    Mockito.when(systemParameterProperties.getDirectoryBrandLogoFinal()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(false);
    fileStorageService.deleteUpdatedBrandLogo(DEFAULT_BRAND_LOGO_PATH, BRAND_CODE);
    verify(gcsProperties).isBrandGcsEnabled();
    verify(systemParameterProperties).getDirectoryBrandLogoFinal();
  }

  @Test
  void gcsAndFileStorePathTest() throws Exception {
    BrandApproveRequest brandApproveRequest = new BrandApproveRequest();
    brandApproveRequest.setProfileBannerPath(DEFAULT_BRAND_LOGO_PATH);
    brandApproveRequest.setBrandLogoPath(DEFAULT_BRAND_LOGO_PATH);
    brandApproveRequest.setBrandRequestCode(BRAND_CODE);
    MultipartFile multipart = generateMultipartFile();
    mockFile("src/test/resources/brandSourceDirectory/brandLogo/BRAND-CODE/blibli-com-logo.");
    Mockito.when(gcsProperties.getBrandSourceDirectory()).thenReturn(BRAND_SOURCE_DIRECTORY);
    Mockito.when(gcsProperties.getBrandProfileBannerDirectory()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getBrandBucketName()).thenReturn(GCS_BUCKET_NAME);
    Mockito.when(systemParameterProperties.getDirectoryProfileBannerSource()).thenReturn(FILE_STORE_PATH);
    Mockito.when(gcsService.downloadFile(Mockito.any(), Mockito.any()))
        .thenThrow(ApplicationRuntimeException.class);
    Mockito.when(gcsService.deleteFile(GCS_BUCKET_NAME, BRAND_LOCATION_PATH)).thenReturn(true);
    fileStorageService.approveProfileBanner(brandApproveRequest, BRAND_CODE, multipart, BRAND_CODE);
    verify(gcsProperties).isBrandGcsEnabled();
    verify(gcsProperties, times(2)).getBrandBucketName();
    verify(gcsService, times(1)).downloadFile(Mockito.any(), Mockito.any());
    verify(gcsService, times(1)).uploadCreatedFile(Mockito.any(), Mockito.any(), Mockito.any());
    verify(gcsService, times(1)).deleteFile(Mockito.any(), Mockito.any());
    verify(gcsProperties, times(1)).getBrandFinalDirectory();
    verify(systemParameterProperties, times(1)).getDirectoryProfileBannerSource();
    verify(gcsProperties, times(2)).getBrandSourceDirectory();
    verify(gcsProperties, times(3)).getBrandProfileBannerDirectory();
    verify(gcsProperties, times(3)).getBrandProfileBannerDirectory();
  }

  @Test
  void gcsAndFileStoreLogoPathTest() throws Exception {
    BrandApproveRequest brandApproveRequest = new BrandApproveRequest();
    brandApproveRequest.setBrandLogoPath(DEFAULT_BRAND_LOGO_PATH);
    brandApproveRequest.setProfileBannerPath(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(gcsProperties.getBrandLogoDirectory()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getBrandBucketName()).thenReturn(GCS_BUCKET_NAME);
    Mockito.when(systemParameterProperties.getDirectoryBrandLogoSource()).thenReturn(FILE_STORE_PATH);
    Mockito.when(gcsService.downloadFile(Mockito.any(), Mockito.any())).thenThrow(ApplicationRuntimeException.class);
    Mockito.when(gcsService.deleteFile(GCS_BUCKET_NAME, BRAND_LOCATION_PATH)).thenReturn(true);
    MultipartFile multipart = generateMultipartFile();
    mockFile("src/test/resources/brandSourceDirectory/brandLogo/BRAND-CODE/blibli-com-logo.");
    fileStorageService.approveBrandLogo(brandApproveRequest, BRAND_CODE, multipart, BRAND_CODE);
    verify(gcsProperties).isBrandGcsEnabled();
    verify(gcsProperties, times(2)).getBrandBucketName();
    verify(gcsService, times(1)).downloadFile(Mockito.any(), Mockito.any());
    verify(gcsService, times(1)).uploadCreatedFile(Mockito.any(), Mockito.any(), Mockito.any());
    verify(gcsService, times(1)).deleteFile(Mockito.any(), Mockito.any());
    verify(gcsProperties, times(1)).getBrandFinalDirectory();
    verify(systemParameterProperties, times(1)).getDirectoryBrandLogoSource();
    verify(gcsProperties, times(2)).getBrandSourceDirectory();
    verify(gcsProperties, times(3)).getBrandLogoDirectory();
    verify(gcsProperties, times(3)).getBrandLogoDirectory();
  }

  @Test
  void gcsAndFileStorePathNotFoundTest() throws Exception {
    BrandApproveRequest brandApproveRequest = new BrandApproveRequest();
    brandApproveRequest.setProfileBannerPath(DEFAULT_BRAND_LOGO_PATH + "1");
    brandApproveRequest.setBrandLogoPath(DEFAULT_BRAND_LOGO_PATH);
    brandApproveRequest.setBrandRequestCode(BRAND_CODE);
    MultipartFile multipart = generateMultipartFile();
    mockFile("src/test/resources/brandSourceDirectory/brandLogo/BRAND-CODE/blibli-com-logo.");
    Mockito.when(gcsProperties.getBrandSourceDirectory()).thenReturn(BRAND_SOURCE_DIRECTORY);
      Mockito.when(gcsProperties.getBrandProfileBannerDirectory()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
      Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(true);
      Mockito.when(gcsProperties.getBrandBucketName()).thenReturn(GCS_BUCKET_NAME);
      Mockito.when(systemParameterProperties.getDirectoryProfileBannerSource()).thenReturn(FILE_STORE_PATH);
      Mockito.when(gcsService.downloadFile(Mockito.any(), Mockito.any()))
          .thenThrow(ApplicationRuntimeException.class);
      fileStorageService.approveProfileBanner(brandApproveRequest, BRAND_CODE, multipart, BRAND_CODE);
      verify(gcsProperties).isBrandGcsEnabled();
      verify(gcsProperties, times(1)).getBrandBucketName();
      verify(gcsService, times(1)).downloadFile(Mockito.any(), Mockito.any());
      verify(systemParameterProperties, times(1)).getDirectoryProfileBannerSource();
      verify(gcsProperties, times(2)).getBrandSourceDirectory();
      verify(gcsProperties, times(2)).getBrandProfileBannerDirectory();
  }

private void mockImageFile(String filePath) throws IOException {
    File file = new File(filePath);
    file.mkdirs();
    int width = 640;
    int height = 320;
    BufferedImage img = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR);
    ImageIO.write(img, "jpg", file);
  }

  @Test
  void getBrandImageTest() {
    Mockito.when(gcsProperties.getBrandFinalDirectory()).thenReturn(BRAND_FINAL_DIRECTORY);
    Mockito.when(gcsProperties.getBrandSourceDirectory()).thenReturn(BRAND_SOURCE_DIRECTORY);
    Mockito.when(gcsProperties.getBrandLogoDirectory()).thenReturn(BRAND_LOGO);
    Mockito.when(gcsProperties.getBrandProfileBannerDirectory()).thenReturn(BRAND_PROFILE);
    Mockito.when(gcsProperties.getBrandBucketName()).thenReturn(GCS_BUCKET_NAME);
    Mockito.when(gcsService.downloadFile(Mockito.eq(GCS_BUCKET_NAME), Mockito.anyString()))
        .thenReturn(new byte[] {});

    fileStorageService.getBrandImage(BRAND_CODE, DEFAULT_BRAND_LOGO_PATH, true, true);
    fileStorageService.getBrandImage(BRAND_CODE, DEFAULT_BRAND_LOGO_PATH, true, false);
    fileStorageService.getBrandImage(BRAND_CODE, DEFAULT_BRAND_LOGO_PATH, false, true);
    fileStorageService.getBrandImage(BRAND_CODE, DEFAULT_BRAND_LOGO_PATH, false, false);

    Mockito.verify(gcsProperties, times(2)).getBrandFinalDirectory();
    Mockito.verify(gcsProperties, times(2)).getBrandSourceDirectory();
    Mockito.verify(gcsProperties, times(2)).getBrandLogoDirectory();
    Mockito.verify(gcsProperties, times(2)).getBrandProfileBannerDirectory();
    Mockito.verify(gcsProperties, times(4)).getBrandBucketName();
    Mockito.verify(gcsService, times(4)).downloadFile(Mockito.eq(GCS_BUCKET_NAME), Mockito.anyString());
  }

  @Test
  void getMasterSkuBulkAssigneeTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.TRUE);
    mockFile(baseDirPath + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
      FileUtils.readFileToByteArray(new File(baseDirPath + FILE)));
    when(systemParameterProperties.getMasterSkuBulkAssigneeUploadPath()).thenReturn(filePath);
    Mockito.when(
        gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any()))
      .thenReturn(baseDirPath);
    String path =
      fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, MASTER_SKU_BULK_ASSIGNEE);
    Mockito.verify(gcsService)
      .uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
    Mockito.verify(gcsProperties, times(1)).isEnabled();
    Mockito.verify(systemParameterProperties).getMasterSkuBulkAssigneeUploadPath();
  }

  @Test
  void getMasterSkuBulkReviewTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.TRUE);
    mockFile(baseDirPath + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
      FileUtils.readFileToByteArray(new File(baseDirPath + FILE)));
    when(systemParameterProperties.getMasterSkuBulkReviewUploadPath()).thenReturn(filePath);
    Mockito.when(
        gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any()))
      .thenReturn(baseDirPath);
    String path =
      fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, MASTER_SKU_BULK_REVIEW);
    Mockito.verify(gcsService)
      .uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
    Mockito.verify(gcsProperties, times(1)).isEnabled();
    Mockito.verify(systemParameterProperties).getMasterSkuBulkReviewUploadPath();
  }

  @Test
  void getIprBulkAddReviewTest() throws Exception {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.TRUE);
    mockFile(baseDirPath + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(baseDirPath + FILE)));
    when(systemParameterProperties.getIprBulkAddReviewUploadPath()).thenReturn(filePath);
    Mockito.when(
            gcsService.uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(),
                Mockito.any()))
        .thenReturn(baseDirPath);
    fileStorageService.uploadFilePath(multipartFile, REQUEST_ID, IPR_PORTAL_BULK_ADD_REVIEW);
    Mockito.verify(gcsService)
        .uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.anyString(), Mockito.any());
    Mockito.verify(gcsProperties).isEnabled();
    Mockito.verify(systemParameterProperties).getIprBulkAddReviewUploadPath();
  }

  @Test
  void uploadAttributeImagesTest() throws Exception {
    byte[] image1 = Files.readAllBytes(Paths.get(PATH_INPUT + ACTIVE_IMAGE_FILE_NAME_1));
    byte[] image2 = Files.readAllBytes(Paths.get(PATH_INPUT + ACTIVE_IMAGE_FILE_NAME_2));
    byte[] image3 = Files.readAllBytes(Paths.get(PATH_INPUT + ACTIVE_IMAGE_FILE_NAME_3));
    UploadAttributeImageRequest uploadImageRequest1 =
        UploadAttributeImageRequest.builder().imageFileName(ACTIVE_IMAGE_FILE_NAME_1).bytes(image1)
            .originalFileType("jpg").build();
    UploadAttributeImageRequest uploadImageRequest2 =
        UploadAttributeImageRequest.builder().imageFileName(ACTIVE_IMAGE_FILE_NAME_2).bytes(image2)
            .originalFileType("jpeg").build();
    UploadAttributeImageRequest uploadImageRequest3 =
        UploadAttributeImageRequest.builder().imageFileName(ACTIVE_IMAGE_FILE_NAME_3).bytes(image3)
            .originalFileType("png").build();
    Mockito.when(gcsProperties.getAttributeImageDirectory()).thenReturn(PATH_OUTPUT);
    Mockito.when(gcsProperties.getAttributePathPrefix()).thenReturn(PATH_PREFIX);
    fileStorageService.uploadAttributeImages(uploadImageRequest1);
    fileStorageService.uploadAttributeImages(uploadImageRequest2);
    fileStorageService.uploadAttributeImages(uploadImageRequest3);

    Mockito.verify(gcsProperties, Mockito.times(3)).getAttributeImageDirectory();
    Mockito.verify(gcsProperties, Mockito.times(3)).getAttributePathPrefix();
    Mockito.verify(gcsService, Mockito.times(3))
        .uploadCreatedFile(Mockito.any(), stringArgumentCaptor.capture(), Mockito.any());

    Assertions.assertNotNull(stringArgumentCaptor.getAllValues().get(0));
    Assertions.assertNotNull(stringArgumentCaptor.getAllValues().get(1));
    Assertions.assertNotNull(stringArgumentCaptor.getAllValues().get(2));
  }

  @Test
  void uploadAttributeImagesExceptionTest() throws Exception {
    byte[] image1 = Files.readAllBytes(Paths.get(PATH_INPUT + ACTIVE_IMAGE_FILE_NAME_1));
    UploadAttributeImageRequest uploadImageRequest1 =
        UploadAttributeImageRequest.builder().imageFileName(ACTIVE_IMAGE_FILE_NAME_1).bytes(image1)
            .originalFileType("jpg").build();
    Mockito.when(gcsProperties.getAttributeImageDirectory()).thenReturn(PATH_OUTPUT);
    Mockito.when(gcsProperties.getAttributePathPrefix()).thenReturn(PATH_PREFIX);
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(attributeBucket), Mockito.any(), Mockito.any()))
        .thenThrow(Exception.class);
    try {
      fileStorageService.uploadAttributeImages(uploadImageRequest1);
    } catch (ApiIncorrectInputDataException ex) {
      Assertions.assertNotNull(ex);
    } finally {
      Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(attributeBucket), Mockito.any(), Mockito.any());
      Mockito.verify(gcsProperties).getAttributeImageDirectory();
      Mockito.verify(gcsProperties).getAttributePathPrefix();
    }
  }

  @Test
  public void uploadActiveImagesGcsEnabledTest() throws Exception {
    byte[] image1 = Files.readAllBytes(Paths.get(PATH_INPUT + ACTIVE_IMAGE_FILE_NAME_1));
    byte[] image2 = Files.readAllBytes(Paths.get(PATH_INPUT + ACTIVE_IMAGE_FILE_NAME_2));
    byte[] image3 = Files.readAllBytes(Paths.get(PATH_INPUT + ACTIVE_IMAGE_FILE_NAME_3));
    UploadImageRequest uploadImageRequest1 =
        UploadImageRequest.builder().imageFileName(ACTIVE_IMAGE_FILE_NAME_1).productCode(PRODUCT_CODE).bytes(image1)
            .active(true).retryRequest(false).originalFileType("jpg").build();
    UploadImageRequest uploadImageRequest2 =
        UploadImageRequest.builder().imageFileName(ACTIVE_IMAGE_FILE_NAME_2).productCode(PRODUCT_CODE).bytes(image2)
            .active(true).retryRequest(false).originalFileType("jpeg").build();
    UploadImageRequest uploadImageRequest3 =
        UploadImageRequest.builder().imageFileName(ACTIVE_IMAGE_FILE_NAME_3).productCode(PRODUCT_CODE).bytes(image3)
            .active(true).retryRequest(false).originalFileType("png").build();

    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(PATH_OUTPUT);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(PATH_FULL);
    Mockito.when(gcsProperties.getFinalMediumImageDirectory()).thenReturn(PATH_MEDIUM);
    Mockito.when(gcsProperties.getFinalThumbnailImageDirectory()).thenReturn(PATH_THUMBNAIL);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(PATH_PREFIX);
    Mockito.when(gcsService.uploadCreatedFile(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(FILE_NAME);

    fileStorageService.uploadActiveImages(uploadImageRequest1);
    fileStorageService.uploadActiveImages(uploadImageRequest2);
    fileStorageService.uploadActiveImages(uploadImageRequest3);

    Mockito.verify(gcsProperties, Mockito.times(9)).isFinalImageEnabled();
    Mockito.verify(gcsProperties, Mockito.times(9)).getFinalImageDirectory();
    Mockito.verify(gcsProperties, Mockito.times(3)).getFinalFullImageDirectory();
    Mockito.verify(gcsProperties, Mockito.times(3)).getFinalMediumImageDirectory();
    Mockito.verify(gcsProperties, Mockito.times(3)).getFinalThumbnailImageDirectory();
    Mockito.verify(gcsProperties, Mockito.times(9)).getPathPrefix();
    Mockito.verify(gcsService, Mockito.times(9))
        .uploadCreatedFile(Mockito.any(), stringArgumentCaptor.capture(), Mockito.any());

    Assertions.assertEquals(PATH_OUTPUT + PATH_FULL + File.separator + PATH_PREFIX + ACTIVE_IMAGE_FILE_NAME_1,
        stringArgumentCaptor.getAllValues().get(0));
    Assertions.assertEquals(PATH_OUTPUT + PATH_MEDIUM + File.separator + PATH_PREFIX + ACTIVE_IMAGE_FILE_NAME_1,
        stringArgumentCaptor.getAllValues().get(1));
    Assertions.assertEquals(PATH_OUTPUT + PATH_THUMBNAIL + File.separator + PATH_PREFIX + ACTIVE_IMAGE_FILE_NAME_1,
        stringArgumentCaptor.getAllValues().get(2));

    Assertions.assertEquals(PATH_OUTPUT + PATH_FULL + File.separator + PATH_PREFIX + ACTIVE_IMAGE_FILE_NAME_2,
        stringArgumentCaptor.getAllValues().get(3));
    Assertions.assertEquals(PATH_OUTPUT + PATH_MEDIUM + File.separator + PATH_PREFIX + ACTIVE_IMAGE_FILE_NAME_2,
        stringArgumentCaptor.getAllValues().get(4));
    Assertions.assertEquals(PATH_OUTPUT + PATH_THUMBNAIL + File.separator + PATH_PREFIX + ACTIVE_IMAGE_FILE_NAME_2,
        stringArgumentCaptor.getAllValues().get(5));

    Assertions.assertEquals(PATH_OUTPUT + PATH_FULL + File.separator + PATH_PREFIX + ACTIVE_IMAGE_FILE_NAME_3,
        stringArgumentCaptor.getAllValues().get(6));
    Assertions.assertEquals(PATH_OUTPUT + PATH_MEDIUM + File.separator + PATH_PREFIX + ACTIVE_IMAGE_FILE_NAME_3,
        stringArgumentCaptor.getAllValues().get(7));
    Assertions.assertEquals(PATH_OUTPUT + PATH_THUMBNAIL + File.separator + PATH_PREFIX + ACTIVE_IMAGE_FILE_NAME_3,
        stringArgumentCaptor.getAllValues().get(8));
  }

  @Test
  public void uploadActiveImagesGcsDisbaledErrorTest() throws Exception {
    byte[] image1 = Files.readAllBytes(Paths.get(PATH_INPUT + ACTIVE_IMAGE_FILE_NAME_1));
    byte[] image2 = Files.readAllBytes(Paths.get(PATH_INPUT + ACTIVE_IMAGE_FILE_NAME_2));
    byte[] image3 = Files.readAllBytes(Paths.get(PATH_INPUT + ACTIVE_IMAGE_FILE_NAME_3));
    UploadImageRequest uploadImageRequest1 =
        UploadImageRequest.builder().imageFileName(ACTIVE_IMAGE_FILE_NAME_1).productCode(PRODUCT_CODE).bytes(image1)
            .active(true).retryRequest(false).originalFileType("jpg").build();
    UploadImageRequest uploadImageRequest2 =
        UploadImageRequest.builder().imageFileName(ACTIVE_IMAGE_FILE_NAME_2).productCode(PRODUCT_CODE).bytes(image2)
            .active(true).retryRequest(false).originalFileType("jpeg").build();
    UploadImageRequest uploadImageRequest3 =
        UploadImageRequest.builder().imageFileName(ACTIVE_IMAGE_FILE_NAME_3).productCode(PRODUCT_CODE).bytes(image3)
            .active(true).retryRequest(false).originalFileType("png").build();

    Mockito.when(gcsProperties.isFinalImageEnabled()).thenThrow(ApplicationRuntimeException.class);

    try {
      fileStorageService.uploadActiveImages(uploadImageRequest1);
    }
    catch (ApiIncorrectInputDataException ex){
      Assertions.assertNotNull(ex);
    }
    finally {
      Mockito.verify(gcsProperties).isFinalImageEnabled();
    }
  }

  @Test
  public void uploadActiveImagesGcsDisbaledTest() throws Exception {
    byte[] image1 = Files.readAllBytes(Paths.get(PATH_INPUT + ACTIVE_IMAGE_FILE_NAME_1));
    byte[] image2 = Files.readAllBytes(Paths.get(PATH_INPUT + ACTIVE_IMAGE_FILE_NAME_2));
    byte[] image3 = Files.readAllBytes(Paths.get(PATH_INPUT + ACTIVE_IMAGE_FILE_NAME_3));
    UploadImageRequest uploadImageRequest1 =
        UploadImageRequest.builder().imageFileName(ACTIVE_IMAGE_FILE_NAME_1).productCode(PRODUCT_CODE).bytes(image1)
            .active(true).retryRequest(false).originalFileType("jpg").build();
    UploadImageRequest uploadImageRequest2 =
        UploadImageRequest.builder().imageFileName(ACTIVE_IMAGE_FILE_NAME_2).productCode(PRODUCT_CODE).bytes(image2)
            .active(true).retryRequest(false).originalFileType("jpeg").build();
    UploadImageRequest uploadImageRequest3 =
        UploadImageRequest.builder().imageFileName(ACTIVE_IMAGE_FILE_NAME_3).productCode(PRODUCT_CODE).bytes(image3)
            .active(true).retryRequest(false).originalFileType("png").build();

    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(false);
    Mockito.when(imageProperties.getFullPath()).thenReturn(PATH_OUTPUT + PATH_FULL);
    Mockito.when(imageProperties.getMediumPath()).thenReturn(PATH_OUTPUT + PATH_MEDIUM);
    Mockito.when(imageProperties.getThumbnailPath()).thenReturn(PATH_OUTPUT + PATH_THUMBNAIL);

    fileStorageService.uploadActiveImages(uploadImageRequest1);
    fileStorageService.uploadActiveImages(uploadImageRequest2);
    fileStorageService.uploadActiveImages(uploadImageRequest3);

    Mockito.verify(gcsProperties, Mockito.times(9)).isFinalImageEnabled();
    Mockito.verify(imageProperties, Mockito.times(3)).getFullPath();
    Mockito.verify(imageProperties, Mockito.times(3)).getMediumPath();
    Mockito.verify(imageProperties, Mockito.times(3)).getThumbnailPath();

    FileUtils.cleanDirectory(new File(PATH_OUTPUT));
  }

  @Test
  public void uploadImageTest() throws Exception
  {
    UploadImageRequest uploadImageRequest=new UploadImageRequest();
    uploadImageRequest.setActive(false);
    uploadImageRequest.setOriginalFileType(JPEG);
    uploadImageRequest.setImageFileName(USERNAME);
    File file = new File(PATH_1 + FILE_2);
    file.mkdirs();
    int width = 700;
    int height = 700;
    BufferedImage img = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR);
    ImageIO.write(img, JPG, file);
    uploadImageRequest.setBytes(FileUtils.readFileToByteArray(file));
    when(gcsProperties.isEnabled()).thenReturn(Boolean.FALSE);
    when(imageProperties.getBasePath()).thenReturn(FILE_FOLDER);
    fileStorageService.uploadImage(uploadImageRequest);
    verify(gcsProperties).isSourceImageEnabled();
    verify(imageProperties).getBasePath();
    deleteFolder(PATH_1);
  }

  @Test
  public void uploadImageTest1() throws Exception
  {
    UploadImageRequest uploadImageRequest=new UploadImageRequest();
    uploadImageRequest.setActive(false);
    uploadImageRequest.setOriginalFileType(JPG);
    uploadImageRequest.setImageFileName(USERNAME);
    File file = new File(PATH_1 + FILE_2);
    file.mkdirs();
    int width = 700;
    int height = 700;
    BufferedImage img = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR);
    ImageIO.write(img, JPG, file);
    uploadImageRequest.setBytes(FileUtils.readFileToByteArray(file));
    when(gcsProperties.isEnabled()).thenReturn(Boolean.FALSE);
    when(imageProperties.getBasePath()).thenReturn(FILE_FOLDER);
    fileStorageService.uploadImage(uploadImageRequest);
    verify(gcsProperties).isSourceImageEnabled();
    verify(imageProperties).getBasePath();
    deleteFolder(PATH_1);
  }

  @Test
  public void uploadImageTest2() throws Exception
  {
    UploadImageRequest uploadImageRequest=new UploadImageRequest();
    uploadImageRequest.setActive(false);
    uploadImageRequest.setOriginalFileType(USERNAME);
    uploadImageRequest.setImageFileName(USERNAME);
    File file = new File(PATH_1 + FILE_2);
    file.mkdirs();
    int width = 700;
    int height = 700;
    BufferedImage img = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR);
    ImageIO.write(img, JPG, file);
    uploadImageRequest.setBytes(FileUtils.readFileToByteArray(file));
    when(gcsProperties.isEnabled()).thenReturn(Boolean.FALSE);
    when(imageProperties.getBasePath()).thenReturn(FILE_FOLDER);
    fileStorageService.uploadImage(uploadImageRequest);
    verify(gcsProperties).isSourceImageEnabled();
    verify(imageProperties).getBasePath();
    deleteFolder(PATH_1);
  }

  @Test
  public void uploadImageTest3() throws Exception
  {
    UploadImageRequest uploadImageRequest=new UploadImageRequest();
    uploadImageRequest.setActive(false);
    uploadImageRequest.setOriginalFileType(USERNAME);
    uploadImageRequest.setImageFileName(USERNAME);
    File file = new File(PATH_1 + FILE_2);
    file.mkdirs();
    int width = 700;
    int height = 700;
    BufferedImage img = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR);
    ImageIO.write(img, JPG, file);
    uploadImageRequest.setBytes(FileUtils.readFileToByteArray(file));
    when(gcsProperties.isSourceImageEnabled()).thenReturn(Boolean.TRUE);
    when(gcsProperties.getSourceImageDirectory()).thenReturn(FILE_FOLDER);
    when(gcsProperties.getPathPrefix()).thenReturn(FILE_FOLDER);
    Mockito.when(
        gcsService.uploadCreatedFile(Mockito.eq(sourceImageBucket), Mockito.any(), Mockito.any()))
        .thenReturn(GCS_BUCKET_NAME);
    fileStorageService.uploadImage(uploadImageRequest);
    verify(gcsProperties).isSourceImageEnabled();
    verify(gcsProperties).getSourceImageDirectory();
    verify(gcsProperties).getPathPrefix();
    verify(gcsService).uploadCreatedFile(Mockito.any(), Mockito.any(), Mockito.any());
    deleteFolder(PATH_1);
  }
}
