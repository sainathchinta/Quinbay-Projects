package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.pcu.external.model.Accessibilty;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.properties.GCSProperties;
import com.gdn.partners.pcu.external.properties.ImageProperties;
import com.gdn.partners.pcu.external.properties.SystemParameterProperties;
import com.gdn.partners.pcu.external.service.GCSService;
import com.gdn.partners.pcu.external.service.impl.helper.ExcelTemplateUtil;
import com.gdn.partners.pcu.external.service.model.request.UploadAttributeImageRequest;
import com.gdn.partners.pcu.external.service.model.request.UploadImageRequest;
import com.gdn.partners.pcu.external.service.model.request.XgpImageScaleRequest;
import com.gdn.partners.pcu.external.streaming.model.bulk.BulkProcessEntity;
import com.gdn.partners.pcu.external.web.model.request.ProcessFileType;
import com.gdn.partners.pcu.external.web.model.request.SignedUrlRequest;
import com.gdn.partners.pcu.external.web.model.response.SignedUrlResponse;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipRequest;
import com.google.cloud.storage.Blob;
import com.google.cloud.storage.Bucket;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.multipart.MultipartFile;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Base64;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

public class FileStorageServiceImplTest {

  private List<String> fileNames = new ArrayList<>();
  private MockMultipartFile multipartFile;
  private static ClassLoader classLoader;
  private static File file;
  private static final String DUMMY_FILE_NAME = "dummy-excel.xls";
  private static final String USERNAME = "username";
  private static final String FILE_FOLDER = "Product";
  private static final String EXCEL_FILE = "dummy.xlsx";
  private static final String ZIP_FILE = "dummy.zip";
  private static final String INVALID_EXCEL_FILE = "dummy.csv";
  private static final String EXCEL_FILE_XLSM = "dummy-excel copy.xlsm";
  private static final String PDF_FILE = "file.pdf";
  private static final String DUMMY_JPEG = "dummy-excel copy.jpeg";
  private UploadImageRequest uploadImageRequest;
  private static final String FINAL_PATH = "final/";
  private static final String NEW_PATH = "new";
  private static String BUCKET_NAME = "BUCKET-NAME";
  private static  String JPEG = "jpeg";
  private static  String JPG = "jpg";
  private static final String PATH_1 = "path";
  private static final String FILE_2 = "/MTA-0315060/apple_mouse_full02.jpg";
  private static final String PATH_INPUT = "src/test/resources/images/input";
  private static final String PATH_OUTPUT = "src/test/resources/images/output";
  private static final String PATH_PREFIX = "catalog-image";
  private static final String PATH_FULL= "/full";
  private static final String PATH_MEDIUM= "/medium";
  private static final String PATH_THUMBNAIL= "/thumbnail";
  private static final String PRODUCT_CODE = "MTA-0315060";
  private static final String ACTIVE_IMAGE_FILE_NAME_1 = "/apple_mouse_full01.jpg";
  private static final String ACTIVE_IMAGE_FILE_NAME_2 = "/apple_mouse_full02.jpeg";
  private static final String ACTIVE_IMAGE_FILE_NAME_3 = "/apple_mouse_full03.png";
  private static final String ACTIVE_IMAGE_FILE_NAME_4 = "/apple_mouse_full01_jpg.jpg";
  private static final String LOCATION_PATH = "dummy-excel copy.jpeg";
  private static final String INTERNAL = "INTERNAL";
  private static final String BRAND_CODE = "/brandRequestCode";
  private static final String BRAND_SOURCE_DIRECTORY = "/brandSourceDirectory";
  private static final String PROFILE_BANNER_PATH = "/profileBannerPath";
  private static final String PROFILE_BANNER_DIRECTORY = "/profileBannerDirectory";
  private static final String DEFAULT_BRAND_LOGO_PATH = "blibli-com-logo.";
  private static final String SELLER_CODE = "sellerCode";
  private byte[] fileContent;


  @InjectMocks
  private FileStorageServiceImpl fileStorageServiceBean;

  @Mock
  private GCSProperties gcsProperties;

  @Mock
  private GCSService gcsService;

  @Mock
  private ImageProperties imageProperties;

  @Mock
  private Bucket bulkBucket;

  @Mock
  private Bucket iprBucket;

  @Mock
  private Bucket brandBucket;

  @Mock
  private Bucket sourceImageBucket;

  @Mock
  private Bucket finalImageBucket;

  @Mock
  private Bucket attributeBucket;

  @Mock
  private SystemParameterProperties systemParameterProperties;

  @Mock
  private Blob blob;

  @Captor
  private ArgumentCaptor<String> stringArgumentCaptor;

  @BeforeEach
  public void init() throws Exception {
    MockitoAnnotations.initMocks(this);
    //generateDirectoryValue();
    fileContent = new byte[] {-1, -40, -20, -10};
    ReflectionTestUtils.setField(fileStorageServiceBean, "fileUploadAllowedType", ".pdf,.zip");
  }

  @AfterEach
  public void teardown() {
    verifyNoMoreInteractions(this.gcsService);
  }

  @Test
  public void downloadMultiPartFileTest() throws Exception {
    List<MultipartFile> mockFiles = Collections.singletonList(
        new MockMultipartFile("dummy.xlsx", "dummy.xlsx", "application/vnd.ms-excel",
            new byte[10]));
    try (MockedStatic<ExcelTemplateUtil> mocked = Mockito.mockStatic(ExcelTemplateUtil.class)) {
      mocked.when(() -> ExcelTemplateUtil.getMultipartFiles(USERNAME, fileNames))
          .thenReturn(mockFiles);
      fileNames.clear();
      fileNames.add("dummy.xlsx");
      List<MultipartFile> fileList =
          fileStorageServiceBean.downloadMultiPartFile(USERNAME, fileNames);
      Assertions.assertEquals(1, fileList.size());
    }
  }


  @Test
  public void downloadMultiPartFileGcsEnabledTest() throws Exception {
    fileNames.clear();
    fileNames.add(EXCEL_FILE);
    Blob blob = mock(Blob.class);
    Map<String, String> files = this.getFiles("products-upload-template");
    byte[] excelFile = Base64.getDecoder().decode(files.get("xls"));
    when(blob.getContent()).thenReturn(excelFile);
    Mockito.when(gcsProperties.isEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getBucketName()).thenReturn(BUCKET_NAME);
    when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.any())).thenReturn(blob);
    List<MultipartFile> fileList =
      fileStorageServiceBean.downloadMultiPartFile(USERNAME, fileNames);
    Assertions.assertEquals(fileList.size(), 1);
    Mockito.verify(gcsService).downloadFile(Mockito.eq(BUCKET_NAME), Mockito.any());
  }

  @Test
  public void downloadMultiPartFileGcsEnabledWithNoBlobTest() throws Exception {
    fileNames.clear();
    fileNames.add(EXCEL_FILE);
    Blob blob = null;
    Map<String, String> files = this.getFiles("products-upload-template");
    byte[] excelFile = Base64.getDecoder().decode(files.get("xls"));
    Mockito.when(gcsProperties.isEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getBucketName()).thenReturn(BUCKET_NAME);
    when(gcsService.downloadFile(Mockito.eq(BUCKET_NAME), Mockito.any())).thenReturn(blob);
    List<MultipartFile> fileList =
      fileStorageServiceBean.downloadMultiPartFile(USERNAME, fileNames);
    Assertions.assertEquals(fileList.size(), 0);
    Mockito.verify(gcsService).downloadFile(Mockito.eq(BUCKET_NAME), Mockito.any());
  }

  @Test
  public void uploadFileToGcsTest() throws Exception {
    String fileName =
      fileStorageServiceBean.uploadFileToGcs(generateDummyExcelMultipartFileXlsm(), USERNAME);
    Assertions.assertEquals(fileName, "dummy-excel copy.xlsm");
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.any(), Mockito.any());
  }

  @Test
  public void uploadEvidenceFileToGcsTest() throws Exception {
    fileStorageServiceBean.uploadEvidenceFileToGcs(generateDummyPdfMultipartFile(), USERNAME, SELLER_CODE);
    Mockito.verify(gcsService)
        .uploadCreatedFile(Mockito.eq(iprBucket), Mockito.any(), Mockito.any());
  }

  @Test
  public void uploadEvidenceFileToGcsZipTest() throws Exception {
    fileStorageServiceBean.uploadEvidenceFileToGcs(generateDummyExcelMultipartFileZip(), USERNAME,
        SELLER_CODE);
    Mockito.verify(gcsService)
        .uploadCreatedFile(Mockito.eq(iprBucket), Mockito.any(), Mockito.any());
  }

  @Test
  public void processFileUploadTest() throws Exception {
    fileStorageServiceBean.processFileUpload(generateDummyPdfMultipartFile(), PRODUCT_CODE,
        ProcessFileType.EVIDENCE_FILE.getValue(), SELLER_CODE);
    Mockito.verify(gcsService)
        .uploadCreatedFile(Mockito.eq(iprBucket), Mockito.any(), Mockito.any());
  }

  @Test
  public void processFileUploadOtherFileTest() throws Exception {
    fileStorageServiceBean.processFileUpload(generateDummyPdfMultipartFile(), PRODUCT_CODE,
        StringUtils.EMPTY, SELLER_CODE);
  }

  @Test
  public void uploadEvidenceFileToGcsMultipartFileNullTest() throws Exception {
    byte[] content = new byte[0];
    MultipartFile request = new MockMultipartFile(Constants.FILENAME, content);
    fileStorageServiceBean.uploadEvidenceFileToGcs(request, USERNAME, SELLER_CODE);
  }

  @Test
  void uploadEvidenceFileToGcsErrorTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> fileStorageServiceBean.uploadEvidenceFileToGcs(generateDummyExcelMultipartFileXlsm(),
            USERNAME, SELLER_CODE));
  }

  @Test
  public void generateXgpImageScaleRequestTest() throws IOException {
    ReflectionTestUtils.setField(fileStorageServiceBean, "imageFormatsSupported", List.of("webp"));
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(PATH_OUTPUT);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(PATH_FULL);
    Mockito.when(gcsProperties.getFinalMediumImageDirectory()).thenReturn(PATH_MEDIUM);
    Mockito.when(gcsProperties.getFinalThumbnailImageDirectory()).thenReturn(PATH_THUMBNAIL);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(PATH_PREFIX);
    String ACTIVE_IMAGE_GCS_FILE_NAME_1 =
        "src/test/resources/images/output/full/catalog-image/apple_mouse_full01.webp";
    String MEDIUM_IMAGE_GCS_FILE_NAME_1 =
        "src/test/resources/images/output/medium/catalog-image/apple_mouse_full01.webp";
    String THUMBNAIL_IMAGE_GCS_FILE_NAME_1 =
        "src/test/resources/images/output/thumbnail/catalog-image/apple_mouse_full01.webp";
    XgpImageScaleRequest xgpImageScaleRequest = fileStorageServiceBean.generateXgpImageScaleRequest(
        new UploadImageRequest(ACTIVE_IMAGE_FILE_NAME_1, PRODUCT_CODE,
            Files.readAllBytes(Paths.get(PATH_INPUT + ACTIVE_IMAGE_FILE_NAME_1)), true, false,
            Constants.WEBP_FORMAT));
    Assertions.assertEquals(xgpImageScaleRequest.getFullImageUploadRequest().getImagePath(),
        ACTIVE_IMAGE_GCS_FILE_NAME_1);
    Assertions.assertEquals(xgpImageScaleRequest.getMediumImageUploadRequest().getImagePath(),
        MEDIUM_IMAGE_GCS_FILE_NAME_1);
    Assertions.assertEquals(xgpImageScaleRequest.getThumbNailImageUploadRequest().getImagePath(),
        THUMBNAIL_IMAGE_GCS_FILE_NAME_1);
  }

  @Test
  public void generateXgpImageScaleRequest_JPG_Test() throws IOException {
    ReflectionTestUtils.setField(fileStorageServiceBean, "imageFormatsSupported",
        List.of(Constants.JPEG, "png"));
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(PATH_OUTPUT);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(PATH_FULL);
    Mockito.when(gcsProperties.getFinalMediumImageDirectory()).thenReturn(PATH_MEDIUM);
    Mockito.when(gcsProperties.getFinalThumbnailImageDirectory()).thenReturn(PATH_THUMBNAIL);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(PATH_PREFIX);
    String ACTIVE_IMAGE_GCS_FILE_NAME_1 =
        "src/test/resources/images/output/full/catalog-image/apple_mouse_full01.jpg";
    String MEDIUM_IMAGE_GCS_FILE_NAME_1 =
        "src/test/resources/images/output/medium/catalog-image/apple_mouse_full01.jpg";
    String THUMBNAIL_IMAGE_GCS_FILE_NAME_1 =
        "src/test/resources/images/output/thumbnail/catalog-image/apple_mouse_full01.jpg";
    XgpImageScaleRequest xgpImageScaleRequest = fileStorageServiceBean.generateXgpImageScaleRequest(
        new UploadImageRequest(ACTIVE_IMAGE_FILE_NAME_1, PRODUCT_CODE,
            Files.readAllBytes(Paths.get(PATH_INPUT + ACTIVE_IMAGE_FILE_NAME_1)), true, false,
            Constants.WEBP_FORMAT));
    Assertions.assertEquals(xgpImageScaleRequest.getFullImageUploadRequest().getImagePath(),
        ACTIVE_IMAGE_GCS_FILE_NAME_1);
    Assertions.assertEquals(xgpImageScaleRequest.getMediumImageUploadRequest().getImagePath(),
        MEDIUM_IMAGE_GCS_FILE_NAME_1);
    Assertions.assertEquals(xgpImageScaleRequest.getThumbNailImageUploadRequest().getImagePath(),
        THUMBNAIL_IMAGE_GCS_FILE_NAME_1);
  }

  @Test
  public void generateXgpImageScaleRequestImagePathHashCodeContainsJPGTest() throws IOException {
    ReflectionTestUtils.setField(fileStorageServiceBean, "imageFormatsSupported",
        List.of("jpg", "png","webp"));
    Mockito.when(gcsProperties.getFinalImageDirectory()).thenReturn(PATH_OUTPUT);
    Mockito.when(gcsProperties.getFinalFullImageDirectory()).thenReturn(PATH_FULL);
    Mockito.when(gcsProperties.getFinalMediumImageDirectory()).thenReturn(PATH_MEDIUM);
    Mockito.when(gcsProperties.getFinalThumbnailImageDirectory()).thenReturn(PATH_THUMBNAIL);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(PATH_PREFIX);
    String ACTIVE_IMAGE_GCS_FILE_NAME_1 =
        "src/test/resources/images/output/full/catalog-image/apple_mouse_full01_jpg.webp";
    String MEDIUM_IMAGE_GCS_FILE_NAME_1 =
        "src/test/resources/images/output/medium/catalog-image/apple_mouse_full01_jpg.webp";
    String THUMBNAIL_IMAGE_GCS_FILE_NAME_1 =
        "src/test/resources/images/output/thumbnail/catalog-image/apple_mouse_full01_jpg.webp";
    XgpImageScaleRequest xgpImageScaleRequest = fileStorageServiceBean.generateXgpImageScaleRequest(
        new UploadImageRequest(ACTIVE_IMAGE_FILE_NAME_4, PRODUCT_CODE,
            Files.readAllBytes(Paths.get(PATH_INPUT + ACTIVE_IMAGE_FILE_NAME_4)), true, false,
            Constants.WEBP_FORMAT));
    Assertions.assertEquals(ACTIVE_IMAGE_GCS_FILE_NAME_1,
        xgpImageScaleRequest.getFullImageUploadRequest().getImagePath());

  }

  @Test
  public void uploadFileToGcsXlsxTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "excelTemplateUploadSizeThresholdInMB",
      1L);
    String fileName =
        fileStorageServiceBean.uploadFileToGcs(generateDummyExcelMultipartFileXlsx(), USERNAME);
    Assertions.assertEquals(fileName, "dummy.xlsx");
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.any(), Mockito.any());
  }

  @Test
  public void uploadFileToGcsZipTest() throws Exception {
    ReflectionTestUtils.setField(fileStorageServiceBean, "excelTemplateUploadSizeThresholdInMB",
      8L);
    String fileName =
        fileStorageServiceBean.uploadFileToGcs(generateDummyExcelMultipartFileZip(), USERNAME);
    Assertions.assertEquals(fileName, "dummy.zip");
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.any(), Mockito.any());
  }

  @Test
  void uploadFileToGcsWrongFileTest() throws Exception {
    Assertions.assertThrows(ApiIncorrectInputDataException.class,
        () -> fileStorageServiceBean.uploadFileToGcs(generateDummyExcelMultipartFileJpeg(),
            USERNAME));
  }

  @Test
  public void uploadSubjectToVatFileTest() throws Exception {
    when(systemParameterProperties.getDirectoryVatFilepath()).thenReturn(Constants.DATA_BASE_DIR);
    String bulkProcessCode = UUID.randomUUID().toString();
    String fileName =
      fileStorageServiceBean.uploadSubjectToVatFile(generateDummyExcelMultipartFileXlsx(),
      bulkProcessCode);
    Mockito.verify(systemParameterProperties).getDirectoryVatFilepath();
    Assertions.assertNotNull(fileName);
  }

  @Test
  public void uploadSubjectToVatFileGCSTest() throws Exception {
    Mockito.when(gcsProperties.isEnabled()).thenReturn(true);
    String bulkProcessCode = UUID.randomUUID().toString();
    String fileName =
      fileStorageServiceBean.uploadSubjectToVatFile(generateDummyExcelMultipartFileXlsx(),
      bulkProcessCode);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(bulkBucket), Mockito.any(), Mockito.any());
    Assertions.assertNotNull(fileName);
  }

  @Test
  void uploadSubjectToVatInvalidFileGCSTest() throws Exception {
    Mockito.when(gcsProperties.isEnabled()).thenReturn(true);
    String bulkProcessCode = UUID.randomUUID().toString();
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> fileStorageServiceBean.uploadSubjectToVatFile(generateInvalidExcelMultipartFile(),
            bulkProcessCode));
  }

  @Test
  public void getImagePathPrefixTest()
  {
    fileStorageServiceBean.getImagePathPrefix();
    verify(gcsProperties).getPathPrefix();
  }

  @Test
  public void downloadFileTest() throws Exception {
    Blob blob = mock(Blob.class);
    when(gcsProperties.getSourceImageDirectory()).thenReturn(USERNAME);
    when(gcsService.downloadFile(Mockito.any(),Mockito.any())).thenReturn(blob);
    when(gcsProperties.getSourceImageBucketName()).thenReturn(BUCKET_NAME);
    fileStorageServiceBean.downloadFile(DUMMY_FILE_NAME);
    verify(gcsProperties).getSourceImageDirectory();
    verify(gcsService).downloadFile(BUCKET_NAME, USERNAME.concat(Constants.ROOT).concat(DUMMY_FILE_NAME));
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
    fileStorageServiceBean.uploadImage(uploadImageRequest);
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
    fileStorageServiceBean.uploadImage(uploadImageRequest);
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
    fileStorageServiceBean.uploadImage(uploadImageRequest);
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
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(sourceImageBucket), Mockito.any(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean.uploadImage(uploadImageRequest);
    verify(gcsProperties).isSourceImageEnabled();
    verify(gcsProperties).getSourceImageDirectory();
    verify(gcsProperties).getPathPrefix();
    verify(gcsService).uploadCreatedFile(Mockito.any(), Mockito.any(), Mockito.any());
    deleteFolder(PATH_1);
  }

  @Test
  void uploadSubjectToVatInvalidFileTest() throws Exception {
    when(systemParameterProperties.getDirectoryVatFilepath()).thenReturn(Constants.DATA_BASE_DIR);
    String bulkProcessCode = UUID.randomUUID().toString();
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> fileStorageServiceBean.uploadSubjectToVatFile(generateInvalidExcelMultipartFile(),
            bulkProcessCode));
  }

  private File generateDummyExcelFile() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File file =
      new File(classLoader.getResource(FILE_FOLDER + File.separator + DUMMY_FILE_NAME).getFile());
    return file;
  }

  private File generateDummyPdfFile() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File file =
        new File(classLoader.getResource(FILE_FOLDER + File.separator + PDF_FILE).getFile());
    return file;
  }

  private MultipartFile generateDummyExcelMultipartFileJpeg() throws Exception {
    File file = generateDummyExcelFile();
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    MultipartFile multipartFile =
        new MockMultipartFile("dummy.jpeg", DUMMY_JPEG, null, fileData);
    return multipartFile;
  }

  private MultipartFile generateDummyExcelMultipartFileXlsm() throws Exception {
    File file = generateDummyExcelFile();
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    MultipartFile multipartFile =
      new MockMultipartFile("dummy.xlsm", EXCEL_FILE_XLSM, null, fileData);
    return multipartFile;
  }

  private MultipartFile generateDummyPdfMultipartFile() throws Exception {
    File file = generateDummyPdfFile();
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    MultipartFile multipartFile =
        new MockMultipartFile("file.pdf", PDF_FILE, null, fileData);
    return multipartFile;
  }

  private MultipartFile generateDummyExcelMultipartFileZip() throws Exception {
    File file = generateDummyExcelFile();
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    MultipartFile multipartFile =
        new MockMultipartFile("dummy.zip", ZIP_FILE, null, fileData);
    return multipartFile;
  }


  private MultipartFile generateDummyExcelMultipartFileXlsx() throws Exception {
    File file = generateDummyExcelFile();
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    MultipartFile multipartFile =
      new MockMultipartFile("dummy.xlsx", EXCEL_FILE, null, fileData);
    return multipartFile;
  }

  private MultipartFile generateInvalidExcelMultipartFile() throws Exception {
    File file = generateDummyExcelFile();
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    MultipartFile multipartFile =
      new MockMultipartFile("dummy.jpg", INVALID_EXCEL_FILE, null, fileData);
    return multipartFile;
  }

  private MultipartFile generateMultipartFile() throws Exception {
    classLoader = this.getClass().getClassLoader();
    mockFiles("target/test-classes/Brand/" + DEFAULT_BRAND_LOGO_PATH);
    file = new File(classLoader.getResource("Brand/" + DEFAULT_BRAND_LOGO_PATH).getFile());
    multipartFile = new MockMultipartFile(DEFAULT_BRAND_LOGO_PATH, DEFAULT_BRAND_LOGO_PATH, "image/jpg",
        IOUtils.toByteArray(new FileInputStream(file)));
    return multipartFile;
  }
  private void mockFiles(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, fileContent);
  }

  private Map<String, String> getFiles(String fileName) throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    String excelData = new String(Base64.getEncoder().encode(IOUtils.toByteArray(
      Thread.currentThread().getContextClassLoader()
        .getResourceAsStream("Excel-template" + File.separator + fileName + ".xlsx"))), "UTF-8");
    files.put("xls", excelData);
    return files;
  }

  private void generateDirectoryValue() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File file = new File(classLoader
      .getResource(new StringBuilder().append(FILE_FOLDER).append("/data.txt").toString())
      .getFile());
    Field field = Constants.class.getField("DATA_BASE_DIR");
    Field modifiersField = Field.class.getDeclaredField("modifiers");
    modifiersField.setAccessible(true);
    modifiersField.setInt(field, field.getModifiers() & ~Modifier.FINAL);
    field.setAccessible(true);
    field.set(null, file.getAbsolutePath().replace("/data.txt", ""));
  }

  @Test
  public void getExternalDownloadTemplateFilePathsGCSTest() {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.TRUE);
    when(systemParameterProperties.getGcsBulkBasePath()).thenReturn(PATH_1);
    when(systemParameterProperties.getArchieveTemplate()).thenReturn(PATH_1);
    when(systemParameterProperties.getInstoreTemplate()).thenReturn(PATH_1);
    when(systemParameterProperties.getL5DeleteTemplate()).thenReturn(PATH_1);
    when(systemParameterProperties.getVatTemplate()).thenReturn(PATH_1);
    when(systemParameterProperties.getLogisticsTemplate()).thenReturn(PATH_1);
    Map<String, String> result = fileStorageServiceBean.getExternalDownloadTemplateFilePaths();
    Mockito.verify(systemParameterProperties).getArchieveTemplate();
    Mockito.verify(systemParameterProperties).getGcsBulkBasePath();
    Mockito.verify(systemParameterProperties).getInstoreTemplate();
    Mockito.verify(systemParameterProperties).getL5DeleteTemplate();
    Mockito.verify(systemParameterProperties).getVatTemplate();
    Mockito.verify(gcsProperties).isEnabled();
  }

  @Test
  public void getExternalDownloadTemplateFilePathsTest() {
    when(gcsProperties.isEnabled()).thenReturn(Boolean.FALSE);
    when(systemParameterProperties.getFileStoreBulkBasePath()).thenReturn(PATH_1);
    when(systemParameterProperties.getArchieveTemplate()).thenReturn(PATH_1);
    when(systemParameterProperties.getInstoreTemplate()).thenReturn(PATH_1);
    when(systemParameterProperties.getL5DeleteTemplate()).thenReturn(PATH_1);
    when(systemParameterProperties.getVatTemplate()).thenReturn(PATH_1);
    when(systemParameterProperties.getLogisticsTemplate()).thenReturn(PATH_1);
    Map<String, String> result = fileStorageServiceBean.getExternalDownloadTemplateFilePaths();
    Mockito.verify(systemParameterProperties).getArchieveTemplate();
    Mockito.verify(systemParameterProperties).getFileStoreBulkBasePath();
    Mockito.verify(systemParameterProperties).getInstoreTemplate();
    Mockito.verify(systemParameterProperties).getL5DeleteTemplate();
    Mockito.verify(systemParameterProperties).getVatTemplate();
    Mockito.verify(gcsProperties).isEnabled();
    Mockito.verify(gcsProperties).isEnabled();
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
        .thenReturn(blob);

    fileStorageServiceBean.uploadActiveImages(uploadImageRequest1);
    fileStorageServiceBean.uploadActiveImages(uploadImageRequest2);
    fileStorageServiceBean.uploadActiveImages(uploadImageRequest3);

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
  public void uploadAttributeImagesTest() throws Exception {
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
    Mockito.when(gcsService.uploadCreatedFile(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenReturn(blob);
    fileStorageServiceBean.uploadAttributeImages(uploadImageRequest1);
    fileStorageServiceBean.uploadAttributeImages(uploadImageRequest2);
    fileStorageServiceBean.uploadAttributeImages(uploadImageRequest3);

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
    Mockito.when(gcsService.uploadCreatedFile(Mockito.any(), Mockito.any(), Mockito.any()))
        .thenThrow(Exception.class);
    try {
      Assertions.assertThrows(ApiIncorrectInputDataException.class,
          () -> fileStorageServiceBean.uploadAttributeImages(uploadImageRequest1));
    } finally {
      Mockito.verify(gcsService).uploadCreatedFile(Mockito.any(), Mockito.any(), Mockito.any());
    }
  }

  @Test
  void uploadActiveImagesGcsDisbaledErrorTest() throws Exception {
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
      Assertions.assertThrows(ApiIncorrectInputDataException.class,
          () -> fileStorageServiceBean.uploadActiveImages(uploadImageRequest1));
    } finally {
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

    fileStorageServiceBean.uploadActiveImages(uploadImageRequest1);
    fileStorageServiceBean.uploadActiveImages(uploadImageRequest2);
    fileStorageServiceBean.uploadActiveImages(uploadImageRequest3);

    Mockito.verify(gcsProperties, Mockito.times(9)).isFinalImageEnabled();
    Mockito.verify(imageProperties, Mockito.times(3)).getFullPath();
    Mockito.verify(imageProperties, Mockito.times(3)).getMediumPath();
    Mockito.verify(imageProperties, Mockito.times(3)).getThumbnailPath();

    FileUtils.cleanDirectory(new File(PATH_OUTPUT));
  }

  private void deleteFolder(String folderPath) throws IOException {
    FileUtils.deleteDirectory(new File(folderPath));
  }

  @Test
  public void getImageContentTest() throws Exception {
    gcsProperties.setPathPrefix("catalog-image");
    when(gcsProperties.getPathPrefix()).thenReturn("catalog-image/");
    when(gcsProperties.getFinalImageDirectory()).thenReturn("src/test/resources/final-image/");
    when(gcsProperties.getFinalSeoulImageDirectory()).thenReturn("seoul/");
    when(gcsService.downloadFile(Mockito.any(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean.getImageContent(DUMMY_FILE_NAME);
    verify(gcsProperties, times(2)).getFinalImageDirectory();
    verify(gcsProperties).getFinalSeoulImageDirectory();
    verify(gcsProperties, Mockito.times(1)).getPathPrefix();
    verify(gcsService).downloadFile(Mockito.any(), Mockito.any());
  }
  @Test
  public void getImageFromImagePathTest() throws Exception {
    Blob blob = mock(Blob.class);
    gcsProperties.setFinalImageEnabled(true);
    when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(true);
    when(gcsService.downloadFile(Mockito.any(),Mockito.any())).thenReturn(blob);
    fileStorageServiceBean.getImageFromImagePath(DUMMY_FILE_NAME);
    verify(gcsProperties).isFileStoreToGcsMigrationCompleted();
    verify(gcsService).downloadFile(Mockito.any(),Mockito.any());
  }
  @Test
  public void getImageFromImagePathTest2() throws Exception {
    Blob blob = null;
    gcsProperties.setFinalImageEnabled(true);
    when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(false);
    when(gcsProperties.getFinalImageBucketName()).thenReturn("final-image");
    when(gcsService.downloadFile(Mockito.any(),Mockito.any())).thenReturn(blob);
    fileStorageServiceBean.getImageFromImagePath(DUMMY_FILE_NAME);
    verify(gcsProperties).isFileStoreToGcsMigrationCompleted();
    verify(gcsProperties).getFinalImageBucketName();
    verify(gcsService).downloadFile(Mockito.any(),Mockito.any());
  }
  @Test
  public void getImageFromImagePathTest3() throws Exception {
    gcsProperties.setFinalImageEnabled(false);
    when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(true);
    when(gcsService.downloadFile(Mockito.any(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean.getImageFromImagePath(DUMMY_FILE_NAME);
    verify(gcsProperties).isFileStoreToGcsMigrationCompleted();
    verify(gcsService).downloadFile(Mockito.any(), Mockito.any());
  }
  @Test
  public void getImageFromImagePathTest4() throws Exception {
    uploadImageRequest = new UploadImageRequest();
    uploadImageRequest.setImageFileName("dummy-excel.xls");
    mockFile(FINAL_PATH + NEW_PATH);
    gcsProperties.setFinalImageEnabled(false);
    when(gcsProperties.isFileStoreToGcsMigrationCompleted()).thenReturn(true);
    when(gcsService.downloadFile(Mockito.any(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean.getImageFromImagePath(NEW_PATH);
    verify(gcsProperties).isFileStoreToGcsMigrationCompleted();
    verify(gcsService).downloadFile(Mockito.any(), Mockito.any());
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    file.mkdirs();
    int width = 700;
    int height = 700;
    BufferedImage img = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR);
    ImageIO.write(img, JPG, file);
    uploadImageRequest.setBytes(FileUtils.readFileToByteArray(file));
  }

  @Test
  public void generatePathTest() {
    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getPathPrefix()).thenReturn(PATH_PREFIX);
    Assertions.assertEquals(PATH_PREFIX + File.separator + PRODUCT_CODE + File.separator + FILE_FOLDER + ".jpeg",
        fileStorageServiceBean.generatePath(PRODUCT_CODE, FILE_FOLDER));

    Mockito.when(gcsProperties.isFinalImageEnabled()).thenReturn(false);
    Assertions.assertEquals(PRODUCT_CODE + File.separator + FILE_FOLDER + ".jpeg",
        fileStorageServiceBean.generatePath(PRODUCT_CODE, FILE_FOLDER));

    Mockito.verify(gcsProperties, times(2)).isFinalImageEnabled();
    Mockito.verify(gcsProperties).getPathPrefix();
  }

  @Test
  public void uploadBrandFileToGcsTest() throws Exception {
    String fileName =
        fileStorageServiceBean.uploadBrandFileToGcs(generateDummyExcelMultipartFileJpeg(), LOCATION_PATH);
    Mockito.verify(gcsService).uploadCreatedFile(Mockito.eq(brandBucket), Mockito.any(), Mockito.any());
    Assertions.assertEquals(fileName, "dummy-excel copy.jpeg");
  }

  @Test
  void uploadBrandFileToGcsEmptyLocationPathTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> fileStorageServiceBean.uploadBrandFileToGcs(generateDummyExcelMultipartFileJpeg(),
            ""));
  }

  @Test
  public void createBrandProfileBannerFileTest() throws Exception {
    CreateBrandWipRequest createBrandWipRequest = new CreateBrandWipRequest();
    createBrandWipRequest.setProfileBannerPath(PROFILE_BANNER_PATH);
    createBrandWipRequest.setBusinessPartnerCode("1234");
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getBrandSourceDirectory()).thenReturn(BRAND_SOURCE_DIRECTORY);
    Mockito.when(gcsProperties.getBrandProfileBannerDirectory()).thenReturn(PROFILE_BANNER_DIRECTORY);
    MultipartFile multipartFile = generateDummyExcelMultipartFileJpeg();
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(brandBucket), Mockito.any(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean.createBrandProfileBannerFile(createBrandWipRequest, multipartFile, BRAND_CODE);
    Mockito.verify(gcsService, times(2)).uploadCreatedFile(Mockito.eq(brandBucket), Mockito.any(), Mockito.any());
    verify(gcsProperties, times(3)).isBrandGcsEnabled();
  }

  @Test
  public void createBrandProfileBannerFileInternalTest() throws Exception {
    CreateBrandWipRequest createBrandWipRequest = new CreateBrandWipRequest();
    createBrandWipRequest.setProfileBannerPath(PROFILE_BANNER_PATH);
    createBrandWipRequest.setBusinessPartnerCode(INTERNAL);
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getBrandSourceDirectory()).thenReturn(BRAND_SOURCE_DIRECTORY);
    Mockito.when(gcsProperties.getBrandProfileBannerDirectory()).thenReturn(PROFILE_BANNER_DIRECTORY);
    MultipartFile multipartFile = generateDummyExcelMultipartFileJpeg();
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(brandBucket), Mockito.any(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean.createBrandProfileBannerFile(createBrandWipRequest, multipartFile, BRAND_CODE);
    Mockito.verify(gcsService, times(1)).uploadCreatedFile(Mockito.eq(brandBucket), Mockito.any(), Mockito.any());
    Mockito.verify(gcsProperties, times(2)).isBrandGcsEnabled();
  }

  @Test
  public void createProfileBannerFileFromFileStoreTest() throws Exception {
    CreateBrandWipRequest createBrandWipRequest = new CreateBrandWipRequest();
    createBrandWipRequest.setProfileBannerPath(DEFAULT_BRAND_LOGO_PATH);
    createBrandWipRequest.setBusinessPartnerCode("1234");
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(false);
    Mockito.when(systemParameterProperties.getDirectoryProfilebannerSource()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(systemParameterProperties.getDirectoryProfileBannerFinal()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    MultipartFile multipartFile = generateMultipartFile();
    fileStorageServiceBean.createBrandProfileBannerFile(createBrandWipRequest, multipartFile, BRAND_CODE);
    verify(systemParameterProperties).getDirectoryProfilebannerSource();
    verify(systemParameterProperties).getDirectoryProfileBannerFinal();
    verify(gcsProperties, times(3)).isBrandGcsEnabled();
  }
  @Test
  public void createProfileBannerFileFromFileStoreInternalTest() throws Exception {
    CreateBrandWipRequest createBrandWipRequest = new CreateBrandWipRequest();
    createBrandWipRequest.setProfileBannerPath(DEFAULT_BRAND_LOGO_PATH);
    createBrandWipRequest.setBusinessPartnerCode(INTERNAL);
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(false);
    Mockito.when(systemParameterProperties.getDirectoryProfilebannerSource()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    MultipartFile multipartFile = generateMultipartFile();
    fileStorageServiceBean.createBrandProfileBannerFile(createBrandWipRequest, multipartFile, BRAND_CODE);
    verify(systemParameterProperties).getDirectoryProfilebannerSource();
    verify(gcsProperties, times(2)).isBrandGcsEnabled();
  }

  @Test
  public void createBrandLogoFileTest() throws Exception {
    CreateBrandWipRequest createBrandWipRequest = new CreateBrandWipRequest();
    createBrandWipRequest.setBrandLogoPath(PROFILE_BANNER_PATH);
    createBrandWipRequest.setBusinessPartnerCode("1234");
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getBrandSourceDirectory()).thenReturn(BRAND_SOURCE_DIRECTORY);
    MultipartFile multipartFile = generateDummyExcelMultipartFileJpeg();
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(brandBucket), Mockito.any(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean.createBrandLogoFile(createBrandWipRequest, multipartFile, BRAND_CODE);
    Mockito.verify(gcsService, times(2)).uploadCreatedFile(Mockito.eq(brandBucket), Mockito.any(), Mockito.any());
    verify(gcsProperties, times(3)).isBrandGcsEnabled();
  }

  @Test
  public void createBrandLogoFileInternalTest() throws Exception {
    CreateBrandWipRequest createBrandWipRequest = new CreateBrandWipRequest();
    createBrandWipRequest.setProfileBannerPath(PROFILE_BANNER_PATH);
    createBrandWipRequest.setBusinessPartnerCode(INTERNAL);
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(true);
    Mockito.when(gcsProperties.getBrandSourceDirectory()).thenReturn(BRAND_SOURCE_DIRECTORY);
    MultipartFile multipartFile = generateDummyExcelMultipartFileJpeg();
    Mockito.when(gcsService.uploadCreatedFile(Mockito.eq(brandBucket), Mockito.any(), Mockito.any())).thenReturn(blob);
    fileStorageServiceBean.createBrandLogoFile(createBrandWipRequest, multipartFile, BRAND_CODE);
    Mockito.verify(gcsService, times(1)).uploadCreatedFile(Mockito.eq(brandBucket), Mockito.any(), Mockito.any());
    Mockito.verify(gcsProperties, times(2)).isBrandGcsEnabled();
  }

  @Test
  public void createBrandLogoFileFromFileStoreTest() throws Exception {
    CreateBrandWipRequest createBrandWipRequest = new CreateBrandWipRequest();
    createBrandWipRequest.setBrandLogoPath(DEFAULT_BRAND_LOGO_PATH);
    createBrandWipRequest.setBusinessPartnerCode("1234");
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(false);
    Mockito.when(systemParameterProperties.getDirectoryBrandlogoSource()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    Mockito.when(systemParameterProperties.getDirectoryBrandLogoFinal()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    MultipartFile multipartFile = generateMultipartFile();
    fileStorageServiceBean.createBrandLogoFile(createBrandWipRequest, multipartFile, BRAND_CODE);
    verify(systemParameterProperties).getDirectoryBrandlogoSource();
    verify(systemParameterProperties).getDirectoryBrandLogoFinal();
    verify(gcsProperties, times(3)).isBrandGcsEnabled();
  }
  @Test
  public void createBrandLogoFileFromFileStoreInternalTest() throws Exception {
    CreateBrandWipRequest createBrandWipRequest = new CreateBrandWipRequest();
    createBrandWipRequest.setBrandLogoPath(DEFAULT_BRAND_LOGO_PATH);
    createBrandWipRequest.setBusinessPartnerCode(INTERNAL);
    Mockito.when(gcsProperties.isBrandGcsEnabled()).thenReturn(false);
    Mockito.when(systemParameterProperties.getDirectoryBrandlogoSource()).thenReturn(DEFAULT_BRAND_LOGO_PATH);
    MultipartFile multipartFile = generateMultipartFile();
    fileStorageServiceBean.createBrandLogoFile(createBrandWipRequest, multipartFile, BRAND_CODE);
    verify(systemParameterProperties).getDirectoryBrandlogoSource();
    verify(gcsProperties, times(2)).isBrandGcsEnabled();
  }

  @Test
  public void generateSignedUrl_Success() throws Exception {
    SignedUrlRequest request = new SignedUrlRequest();
    request.setFileName("test-file.xlsx");
    request.setProcessType(BulkProcessEntity.PRODUCT_BASIC_INFO.name());

    String expectedSignedUrl =
      "https://storage.googleapis.com/test-bucket/test/path/file.txt?signature=xyz";

    ReflectionTestUtils.setField(fileStorageServiceBean, "bulkMasterDataUploadLocation",
      "test/path");
    ReflectionTestUtils.setField(fileStorageServiceBean, "signedUrlDuration", 15L);
    when(
      gcsService.generateSignedUrl(any(Bucket.class), anyString(), any(Duration.class))).thenReturn(
      expectedSignedUrl);

    SignedUrlResponse response = fileStorageServiceBean.generateSignedUrl(request);

    Assertions.assertNotNull(response);
    Assertions.assertEquals(expectedSignedUrl, response.getSignedUrl());
    Assertions.assertNotNull(response.getBulkProcessCode());
    Assertions.assertTrue(response.getUploadedPath().contains("test/path"));
    Assertions.assertTrue(response.getUploadedPath().contains("test-file.xlsx"));
    Assertions.assertTrue(response.getExpiresAt() > System.currentTimeMillis());
    verify(gcsService).generateSignedUrl(any(Bucket.class), anyString(), any(Duration.class));
  }

  @Test
  public void generateSignedUrlNullFileNameTestThrowsException() {
    // Given
    SignedUrlRequest request = new SignedUrlRequest();
    request.setFileName(null);
    request.setProcessType(BulkProcessEntity.PRODUCT_BASIC_INFO.name());

    // When
    try {
      fileStorageServiceBean.generateSignedUrl(request);
      Assertions.fail("Expected ApplicationRuntimeException to be thrown");
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals(ErrorCategory.VALIDATION, e.getErrorCodes());
      Assertions.assertTrue(e.getMessage().contains(ErrorMessages.FILE_NAME_CANNOT_BE_EMPTY));
    }
  }

  @Test
  public void generateSignedUrlEmptyProcessTypeTestThrowsException() {
    SignedUrlRequest request = new SignedUrlRequest();
    request.setFileName("test-file.xlsx");
    request.setProcessType("");

    try {
      fileStorageServiceBean.generateSignedUrl(request);
      Assertions.fail("Expected ApplicationRuntimeException to be thrown");
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals(ErrorCategory.VALIDATION, e.getErrorCodes());
      Assertions.assertTrue(e.getMessage().contains(ErrorMessages.PROCESS_TYPE_CANNOT_BE_EMPTY));
    }
  }

  @Test
  public void generateSignedUrl_NullProcessType_ThrowsException() {
    SignedUrlRequest request = new SignedUrlRequest();
    request.setFileName("test-file.xlsx");
    request.setProcessType(null);
    try {
      fileStorageServiceBean.generateSignedUrl(request);
      Assertions.fail("Expected ApplicationRuntimeException to be thrown");
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals(ErrorCategory.VALIDATION, e.getErrorCodes());
      Assertions.assertTrue(e.getMessage().contains(ErrorMessages.PROCESS_TYPE_CANNOT_BE_EMPTY));
    }
  }

  @Test
  public void generateSignedUrl_InvalidProcessType_ThrowsException() {
    SignedUrlRequest request = new SignedUrlRequest();
    request.setFileName("test-file.xlsx");
    request.setProcessType("Invalid Type");
    ReflectionTestUtils.setField(fileStorageServiceBean, "signedUrlUploadFileTypes",
      "CATEGORY_UPLOAD");
    try {
      fileStorageServiceBean.generateSignedUrl(request);
      Assertions.fail("Expected ApplicationRuntimeException to be thrown");
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals(ErrorCategory.UNSPECIFIED, e.getErrorCodes());
    }
  }

  @Test
  public void generateSignedUrlForCreationSuccess() {
    Credential.setAccessibilities(
      new String[] {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION_BULK_CREATION});
    ReflectionTestUtils.setField(fileStorageServiceBean, "signedUrlUploadFileTypes",
      "CATEGORY_UPLOAD");
    SignedUrlRequest request = new SignedUrlRequest();
    request.setFileName("test-file.xlsx");
    request.setProcessType("CATEGORY_UPLOAD");
    request.setUsername(USERNAME);

    String expectedSignedUrl =
      "https://storage.googleapis.com/test-bucket/test/path/file.txt?signature=xyz";

    ReflectionTestUtils.setField(fileStorageServiceBean, "bulkMasterDataUploadLocation",
      "test/path");
    ReflectionTestUtils.setField(fileStorageServiceBean, "signedUrlDuration", 15L);
    when(gcsProperties.getDataUploadPath()).thenReturn("file");
    when(
      gcsService.generateSignedUrl(any(Bucket.class), anyString(), any(Duration.class))).thenReturn(
      expectedSignedUrl);

    SignedUrlResponse response = fileStorageServiceBean.generateSignedUrl(request);

    Assertions.assertNotNull(response);
    Assertions.assertEquals(expectedSignedUrl, response.getSignedUrl());
    Assertions.assertNotNull(response.getBulkProcessCode());
    Assertions.assertTrue(response.getUploadedPath().contains("test-file.xlsx"));
    Assertions.assertTrue(response.getExpiresAt() > System.currentTimeMillis());
    verify(gcsService).generateSignedUrl(any(Bucket.class), anyString(), any(Duration.class));
  }

    @Test
    public void generateSignedUrlForExternalCreationSuccess() {
        Credential.setAccessibilities(
            new String[] {Accessibilty.STORE_PRODUCT_PRODUCT_CREATION_BULK_CREATION});
        ReflectionTestUtils.setField(fileStorageServiceBean, "signedUrlUploadFileTypes",
            "CATEGORY_UPLOAD");
        SignedUrlRequest request = new SignedUrlRequest();
        request.setFileName("test-file.zip");
        request.setProcessType("EXTERNAL_UPLOAD");
        request.setUsername(USERNAME);

        String expectedSignedUrl =
            "https://storage.googleapis.com/test-bucket/test/path/file.txt?signature=xyz";

        ReflectionTestUtils.setField(fileStorageServiceBean, "externalCreationDataUploadLocation",
            "test/path");
        ReflectionTestUtils.setField(fileStorageServiceBean, "signedUrlDuration", 15L);
        when(gcsProperties.getDataUploadPath()).thenReturn("file");
        when(gcsService.generateSignedUrl(any(Bucket.class), anyString(),
            any(Duration.class))).thenReturn(expectedSignedUrl);

        SignedUrlResponse response = fileStorageServiceBean.generateSignedUrl(request);

        Assertions.assertNotNull(response);
        Assertions.assertEquals(expectedSignedUrl, response.getSignedUrl());
        Assertions.assertNotNull(response.getBulkProcessCode());
        Assertions.assertTrue(response.getUploadedPath().contains(".zip"));
        Assertions.assertTrue(response.getExpiresAt() > System.currentTimeMillis());
        verify(gcsService).generateSignedUrl(any(Bucket.class), anyString(), any(Duration.class));
        Credential.setAccessibilities(new String[] {});
    }

  @Test
  public void generateSignedUrlForCreationNoAccessTest() {
    ReflectionTestUtils.setField(fileStorageServiceBean, "signedUrlUploadFileTypes",
      "CATEGORY_UPLOAD");
    SignedUrlRequest request = new SignedUrlRequest();
    request.setFileName("test-file.xlsx");
    request.setProcessType("CATEGORY_UPLOAD");
    request.setUsername(USERNAME);

    String expectedSignedUrl =
      "https://storage.googleapis.com/test-bucket/test/path/file.txt?signature=xyz";

    ReflectionTestUtils.setField(fileStorageServiceBean, "bulkMasterDataUploadLocation",
      "test/path");
    ReflectionTestUtils.setField(fileStorageServiceBean, "signedUrlDuration", 15L);
    when(gcsProperties.getDataUploadPath()).thenReturn("file");
    when(
      gcsService.generateSignedUrl(any(Bucket.class), anyString(), any(Duration.class))).thenReturn(
      expectedSignedUrl);
    Assertions.assertThrows(ApplicationRuntimeException.class,
      () -> fileStorageServiceBean.generateSignedUrl(request));
  }
}
