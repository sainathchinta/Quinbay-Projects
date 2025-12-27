package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.exception.ApplicationException;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.properties.ImageProperties;
import com.gdn.partners.pcu.external.service.FileStorageService;
import com.gdn.partners.pcu.external.service.XgpService;
import com.gdn.partners.pcu.external.service.impl.exception.ImageNotFoundException;
import com.gdn.partners.pcu.external.service.impl.exception.ImageValidationException;
import com.gdn.partners.pcu.external.service.impl.helper.ImageValidator;
import com.gdn.partners.pcu.external.service.model.request.FullImageUploadRequest;
import com.gdn.partners.pcu.external.service.model.request.MediumImageUploadRequest;
import com.gdn.partners.pcu.external.service.model.request.ThumbNailImageUploadRequest;
import com.gdn.partners.pcu.external.service.model.request.UploadAttributeImageRequest;
import com.gdn.partners.pcu.external.service.model.request.UploadImageRequest;
import com.gdn.partners.pcu.external.service.model.request.XgpImageScaleRequest;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.multipart.MultipartFile;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class ImageServiceImplTest {

  private static final String FILE = "/full//81/MTA-0315060/apple_mouse_full02.jpg";
  private static final String FILE_2 = "/MTA-0315060/apple_mouse_full02.jpg";
  private static final String FILE_1 = "81/MTA-0315060/apple_mouse_full02.jpg";
  private static final String FILE_3 = "apple-mouse-full02";
  private static final String PATH = "path";
  private static final String JPEG = "jpeg";
  private static final String PNG = "png";
  private static final String WEBP = "webp";
  private static final String IMAGE_URL1 = "http://www.netflix.com";
  private static final String IMAGE_URL2 = "https://imgur.com/gallery/bFDWhkK";
  public static final String JPG = "jpg";
  private static final String ORIGINAL_FILENAME = "originalFilename.jpg";

  @InjectMocks
  private ImageServiceImpl imageService;

  @Mock
  private ImageProperties imageProperties;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private XgpService xgpService;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  private UploadImageRequest uploadImageRequest;
  private UploadAttributeImageRequest uploadAttributeImageRequest;
  private byte[] imageContent;
  private ImageValidator imageValidator;
  private MultipartFile multipartFile;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    uploadImageRequest = new UploadImageRequest();
    uploadImageRequest.setImageFileName(FILE_2);
    uploadAttributeImageRequest = new UploadAttributeImageRequest();
    uploadAttributeImageRequest.setImageFileName(FILE_2);

    ReflectionTestUtils.setField(imageService, "imageMaxSize", 4000000);
    ReflectionTestUtils.setField(imageService, "imageUploadSizeThresholdInMB", 10);
    ReflectionTestUtils.setField(imageService, "attributeImageAllowedFormats",
        List.of(JPG, JPEG, PNG));
  }


  @Test
  void getImageDetailActiveFalseTest() throws Exception {
    when(fileStorageService.getImagePathPrefix()).thenReturn(PATH);
    when(imageProperties.getBasePath()).thenReturn(PATH);
    try {
      Assertions.assertThrows(ImageNotFoundException.class,
          () -> this.imageService.getImageDetail(FILE, Boolean.FALSE));
    } finally {
      verify(fileStorageService).getImagePathPrefix();
    }
  }

  @Test
  public void getImageDetailActiveFalseWithBasePathTest() throws Exception {
    try {
      when(fileStorageService.getImagePathPrefix()).thenReturn(PATH);
      when(imageProperties.getBasePath()).thenReturn(PATH);
      this.imageService.getImageDetail(FILE_1, Boolean.FALSE);
      verify(fileStorageService).getImagePathPrefix();
      verify(fileStorageService).downloadFile(FILE_1);
    } catch (Exception e) {
      assertEquals(e.getClass(), ImageNotFoundException.class);
    }
  }

  @Test
  public void getImageDetailActiveTrueSuccessTrue() throws Exception {
    byte[] bytearr = mockFile2(PATH + FILE);
    when(fileStorageService.getImageContent(FILE)).thenReturn(bytearr);
    this.imageService.getImageDetail(FILE , Boolean.TRUE);
    verify(fileStorageService).getImageContent(FILE);
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

  private void mockFile1(String filePath) throws IOException {
    File file = new File(filePath);
    file.mkdirs();
    int width = 100;
    int height = 100;
    BufferedImage img = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR);
    ImageIO.write(img, JPG, file);
    uploadImageRequest.setBytes(FileUtils.readFileToByteArray(file));
  }

  private void deleteFolder(String folderPath) throws IOException {
    FileUtils.deleteDirectory(new File(folderPath));
  }

  @Test
  public void getImageDetailActiveFalseSuccessTrue() throws Exception {
    mockFile(PATH + FILE_2);
    when(imageProperties.getBasePath()).thenReturn(PATH);
    when(fileStorageService.getImagePathPrefix()).thenReturn(PATH);
    this.imageService.getImageDetail(FILE , Boolean.FALSE);
    verify(this.imageProperties).getBasePath();
  }

  @Test
  public void getImageDetailActiveFalseSuccessTrue1() throws Exception {
    mockFile(PATH + FILE_2);
    when(imageProperties.getBasePath()).thenReturn(PATH);
    when(fileStorageService.getImagePathPrefix()).thenReturn(PATH);
    this.imageService.getImageDetail(PATH.concat(FILE) , Boolean.FALSE);
    verify(fileStorageService).getImagePathPrefix();
    verify(fileStorageService).downloadFile(PATH.concat(FILE));
  }

  @Test
  public void uploadImageTest() throws Exception {
    ReflectionTestUtils.setField(imageService, "imageFormatsSupported", List.of(JPG, JPEG, PNG));
    mockFile(PATH + FILE_2);
    uploadImageRequest.setActive(Boolean.TRUE);
    uploadImageRequest.setOriginalFileType(JPEG);
    this.imageService.uploadImage(uploadImageRequest);
    Mockito.verify(mandatoryParameterHelper).isExternal();
    Mockito.verify(mandatoryParameterHelper).getClientType();
  }

  @Test
  public void uploadImageTestActiveFalse() throws Exception {
    mockFile(PATH + FILE_2);
    uploadImageRequest.setActive(Boolean.FALSE);
    uploadImageRequest.setOriginalFileType(JPEG);
    when(this.imageProperties.getBasePath()).thenReturn(PATH);
    when(this.imageProperties.getFullPath()).thenReturn(PATH);
    when(this.imageProperties.getMediumPath()).thenReturn(PATH);
    when(this.imageProperties.getThumbnailPath()).thenReturn(PATH);
    this.imageService.uploadImage(uploadImageRequest);
    verify(fileStorageService).uploadImage(uploadImageRequest);
    Mockito.verify(mandatoryParameterHelper).isExternal();
    Mockito.verify(mandatoryParameterHelper).getClientType();
  }

  @Test
  void uploadImageTestActiveFalseException() throws Exception {
    mockFile(PATH + FILE_2);
    uploadImageRequest.setActive(Boolean.FALSE);
    uploadImageRequest.setOriginalFileType(JPEG);
    when(fileStorageService.uploadImage(uploadImageRequest)).thenThrow(Exception.class);
    when(this.imageProperties.getBasePath()).thenReturn(PATH);
    when(this.imageProperties.getFullPath()).thenReturn(PATH);
    when(this.imageProperties.getMediumPath()).thenReturn(PATH);
    when(this.imageProperties.getThumbnailPath()).thenReturn(PATH);
    try {
      Assertions.assertThrows(ApiIncorrectInputDataException.class,
          () -> this.imageService.uploadImage(uploadImageRequest));
    } finally {
      verify(fileStorageService).uploadImage(uploadImageRequest);
      Mockito.verify(mandatoryParameterHelper).isExternal();
      Mockito.verify(mandatoryParameterHelper).getClientType();
    }
  }

  @Test
  public void uploadAttributeImageTest() throws Exception {
    mockFile(PATH + FILE_3);
    multipartFile = new MockMultipartFile("image", ORIGINAL_FILENAME, "image/jpeg",
        FileUtils.readFileToByteArray(new File(PATH + FILE_3)));
    uploadAttributeImageRequest.setOriginalFileType(JPG);
    uploadAttributeImageRequest.setBytes(multipartFile.getBytes());
    uploadAttributeImageRequest.setImageFileName(FILE_3);
    this.imageService.uploadAttributeImage(multipartFile, FILE_3);
    verify(fileStorageService).uploadAttributeImages(uploadAttributeImageRequest);
  }

  @Test
  void uploadAttributeImageValidationTest() throws Exception {
    mockFile(PATH + FILE_2);
    multipartFile = new MockMultipartFile("image", null, "image/jpeg",
        FileUtils.readFileToByteArray(new File(PATH + FILE_2)));
    uploadAttributeImageRequest.setOriginalFileType(JPG);
    uploadAttributeImageRequest.setBytes(multipartFile.getBytes());
    Assertions.assertThrows(ApiIncorrectInputDataException.class,
        () -> this.imageService.uploadAttributeImage(multipartFile, FILE_2));
  }

  @Test
  void uploadAttributeImageExceptionTest() throws Exception {
    mockFile(PATH + FILE_3);
    multipartFile = new MockMultipartFile("image", ORIGINAL_FILENAME, "image/jpeg",
        FileUtils.readFileToByteArray(new File(PATH + FILE_3)));
    uploadAttributeImageRequest.setImageFileName(FILE_3);
    uploadAttributeImageRequest.setOriginalFileType(JPG);
    uploadAttributeImageRequest.setBytes(multipartFile.getBytes());
    when(fileStorageService.uploadAttributeImages(uploadAttributeImageRequest)).thenThrow(
        ApiIncorrectInputDataException.class);
    try {
      Assertions.assertThrows(ApiIncorrectInputDataException.class,
          () -> this.imageService.uploadAttributeImage(multipartFile, FILE_3));
    } finally {
      verify(fileStorageService).uploadAttributeImages(uploadAttributeImageRequest);
    }
  }

  @Test
  public void uploadImageJpgTest() throws Exception {
    ReflectionTestUtils.setField(imageService, "imageFormatsSupported", List.of(JPG, JPEG, PNG));
    mockFile(PATH + FILE_2);
    uploadImageRequest.setActive(Boolean.TRUE);
    uploadImageRequest.setOriginalFileType(JPG);
    this.imageService.uploadImage(uploadImageRequest);
    Mockito.verify(mandatoryParameterHelper).isExternal();
    Mockito.verify(mandatoryParameterHelper).getClientType();
  }

  @Test
  public void uploadImageWebpTest() throws Exception {
    mockFile(PATH + FILE_3);
    String ACTIVE_IMAGE_GCS_FILE_NAME_1 =
        "src/test/resources/images/output/full/catalog-image/apple_mouse_full01.jpg";
    String MEDIUM_IMAGE_GCS_FILE_NAME_1 =
        "src/test/resources/images/output/medium/catalog-image/apple_mouse_full01.jpg";
    String THUMBNAIL_IMAGE_GCS_FILE_NAME_1 =
        "src/test/resources/images/output/thumbnail/catalog-image/apple_mouse_full01.jpg";
    XgpImageScaleRequest xgpImageScaleRequest =
        new XgpImageScaleRequest(new FullImageUploadRequest(ACTIVE_IMAGE_GCS_FILE_NAME_1),
            new MediumImageUploadRequest(MEDIUM_IMAGE_GCS_FILE_NAME_1),
            new ThumbNailImageUploadRequest(THUMBNAIL_IMAGE_GCS_FILE_NAME_1),
            FileUtils.readFileToByteArray(new File(PATH + FILE_3)));

    Mockito.when(
            fileStorageService.generateXgpImageScaleRequest(Mockito.any(UploadImageRequest.class)))
        .thenReturn(xgpImageScaleRequest);
    Mockito.doNothing().when(xgpService).scaleActiveProductNewImages(xgpImageScaleRequest);
    ReflectionTestUtils.setField(imageService, "imageFormatsSupported",
        List.of(JPG, JPEG, PNG, WEBP));
    uploadImageRequest.setActive(Boolean.TRUE);
    uploadImageRequest.setOriginalFileType(WEBP);
    this.imageService.uploadImage(uploadImageRequest);
    Mockito.verify(mandatoryParameterHelper).isExternal();
    Mockito.verify(mandatoryParameterHelper).getClientType();
    Mockito.verify(fileStorageService)
        .generateXgpImageScaleRequest(Mockito.any(UploadImageRequest.class));
    Mockito.verify(xgpService).scaleActiveProductNewImages(Mockito.any(XgpImageScaleRequest.class));
  }

  @Test
  public void uploadImage_Scale_Using_XGP_Test() throws Exception {
    mockFile(PATH + FILE_3);
    String ACTIVE_IMAGE_GCS_FILE_NAME_1 =
        "src/test/resources/images/output/full/catalog-image/apple_mouse_full01.jpg";
    String MEDIUM_IMAGE_GCS_FILE_NAME_1 =
        "src/test/resources/images/output/medium/catalog-image/apple_mouse_full01.jpg";
    String THUMBNAIL_IMAGE_GCS_FILE_NAME_1 =
        "src/test/resources/images/output/thumbnail/catalog-image/apple_mouse_full01.jpg";
    XgpImageScaleRequest xgpImageScaleRequest =
        new XgpImageScaleRequest(new FullImageUploadRequest(ACTIVE_IMAGE_GCS_FILE_NAME_1),
            new MediumImageUploadRequest(MEDIUM_IMAGE_GCS_FILE_NAME_1),
            new ThumbNailImageUploadRequest(THUMBNAIL_IMAGE_GCS_FILE_NAME_1),
            FileUtils.readFileToByteArray(new File(PATH + FILE_3)));

    Mockito.when(
            fileStorageService.generateXgpImageScaleRequest(Mockito.any(UploadImageRequest.class)))
        .thenReturn(xgpImageScaleRequest);
    Mockito.doNothing().when(xgpService).scaleActiveProductNewImages(xgpImageScaleRequest);
    ReflectionTestUtils.setField(imageService, "callXgpForImageScaling", true);
    ReflectionTestUtils.setField(imageService, "imageFormatsSupported", List.of(PNG));
    uploadImageRequest.setActive(Boolean.TRUE);
    uploadImageRequest.setOriginalFileType(WEBP);
    this.imageService.uploadImage(uploadImageRequest);
    Mockito.verify(mandatoryParameterHelper).isExternal();
    Mockito.verify(mandatoryParameterHelper).getClientType();
    Mockito.verify(fileStorageService)
        .generateXgpImageScaleRequest(Mockito.any(UploadImageRequest.class));
    Mockito.verify(xgpService).scaleActiveProductNewImages(Mockito.any(XgpImageScaleRequest.class));
  }

  @Test
  public void uploadImageWebp_ExceptionTest() throws Exception {
    mockFile(PATH + FILE_3);
    String ACTIVE_IMAGE_GCS_FILE_NAME_1 =
        "src/test/resources/images/output/full/catalog-image/apple_mouse_full01.jpg";
    String MEDIUM_IMAGE_GCS_FILE_NAME_1 =
        "src/test/resources/images/output/medium/catalog-image/apple_mouse_full01.jpg";
    String THUMBNAIL_IMAGE_GCS_FILE_NAME_1 =
        "src/test/resources/images/output/thumbnail/catalog-image/apple_mouse_full01.jpg";
    XgpImageScaleRequest xgpImageScaleRequest =
        new XgpImageScaleRequest(new FullImageUploadRequest(ACTIVE_IMAGE_GCS_FILE_NAME_1),
            new MediumImageUploadRequest(MEDIUM_IMAGE_GCS_FILE_NAME_1),
            new ThumbNailImageUploadRequest(THUMBNAIL_IMAGE_GCS_FILE_NAME_1),
            FileUtils.readFileToByteArray(new File(PATH + FILE_3)));

    Mockito.doThrow(new ImageValidationException("")).when(fileStorageService)
        .generateXgpImageScaleRequest(Mockito.any(UploadImageRequest.class));
    Mockito.doNothing().when(xgpService).scaleActiveProductNewImages(xgpImageScaleRequest);
    ReflectionTestUtils.setField(imageService, "imageFormatsSupported",
        List.of(JPG, JPEG, PNG, WEBP));
    uploadImageRequest.setActive(Boolean.TRUE);
    uploadImageRequest.setOriginalFileType(WEBP);
    Assertions.assertThrows(ApiIncorrectInputDataException.class,
        () -> this.imageService.uploadImage(uploadImageRequest));
    Mockito.verify(mandatoryParameterHelper).isExternal();
    Mockito.verify(mandatoryParameterHelper).getClientType();
    Mockito.verify(fileStorageService)
        .generateXgpImageScaleRequest(Mockito.any(UploadImageRequest.class));
  }

  @Test
  public void uploadImageNotJpegTest() throws Exception {
    ReflectionTestUtils.setField(imageService, "imageFormatsSupported", List.of(JPG, JPEG, PNG));
    mockFile(PATH + FILE_2);
    uploadImageRequest.setActive(Boolean.TRUE);
    uploadImageRequest.setOriginalFileType(Constants.JPEG);
    this.imageService.uploadImage(uploadImageRequest);
    Mockito.verify(mandatoryParameterHelper).isExternal();
    Mockito.verify(mandatoryParameterHelper).getClientType();
  }

  @Test
  void uploadImageExceptionTest() throws Exception {
    ReflectionTestUtils.setField(imageService, "imageFormatsSupported", List.of(JPG, JPEG, PNG));
    mockFile(PATH + FILE_2);
    uploadImageRequest.setActive(Boolean.TRUE);
    uploadImageRequest.setOriginalFileType(JPEG);
    Mockito.doThrow(ImageValidationException.class).when(fileStorageService)
        .uploadActiveImages(Mockito.any(UploadImageRequest.class));
    try {
      Assertions.assertThrows(ApiIncorrectInputDataException.class,
          () -> this.imageService.uploadImage(uploadImageRequest));
    } finally {
      verify(mandatoryParameterHelper).isExternal();
      verify(mandatoryParameterHelper).getClientType();
    }
  }

  @Test
  void getImageInputStreamHttpPathTest() throws Exception {
    Assertions.assertThrows(ApplicationException.class,
        () -> this.imageService.getImageInputStream(IMAGE_URL1));
    try {
      this.imageService.uploadImage(uploadImageRequest);
    } finally {
      Mockito.verify(mandatoryParameterHelper).isExternal();
      Mockito.verify(mandatoryParameterHelper).getClientType();
    }
  }

  @Test
  public void getImageInputStreamHttpsPathTest() throws Exception {
    ReflectionTestUtils.setField(imageService, "allowedImageMimeTypes",
        List.of("image/jpeg", "image/png", "image/jpg"));
    this.imageService.getImageInputStream(IMAGE_URL2);
  }

  @Test
  public void getImageInputStreamHttpsPath_Webp_Test() throws Exception {
    ReflectionTestUtils.setField(imageService, "allowedImageMimeTypes", List.of("image/webp", "text/html"));
    this.imageService.getImageInputStream(IMAGE_URL2);
  }

  @Test
  public void getImageInputStreambase64Test() throws Exception {
    this.imageService.getImageInputStream(PATH);
  }

  @Test
  public void imageExistsAndValidTest() throws Exception {
    byte[] bytearr = mockFile2("images/input/apple_mouse_full03.png");
    when(fileStorageService.getImageFromImagePath(Mockito.anyString())).thenReturn(bytearr);
    boolean success = this.imageService.imageExistsAndValid(FILE_2);
    verify(fileStorageService).getImageFromImagePath(Mockito.anyString());
  }

  @Test
  public void imageExistsAndValidTest1() throws Exception {
    byte[] bytearr = null;
    when(fileStorageService.getImageFromImagePath(Mockito.anyString())).thenReturn(bytearr);
    boolean success = this.imageService.imageExistsAndValid(FILE_2);
    verify(fileStorageService).getImageFromImagePath(Mockito.anyString());
  }


  @Test
  public void imageExistsAndValidFalseTest() throws Exception {
    byte[] bytearr = mockFile2(PATH + FILE_1);
    when(fileStorageService.getImageFromImagePath(Mockito.anyString())).thenReturn(bytearr);
    boolean success = this.imageService.imageExistsAndValid(FILE_1);
    verify(fileStorageService).getImageFromImagePath(Mockito.anyString());
    Assertions.assertFalse(success);
  }

  @Test
  public void imageExistsAndValidLessResolutionTest() throws Exception {
    ReflectionTestUtils.setField(imageService, "decreaseImageResolution", true);
    byte[] bytearr = mockFile3(PATH + FILE_2);
    when(fileStorageService.getImageFromImagePath(Mockito.anyString())).thenReturn(bytearr);
    boolean success = this.imageService.imageExistsAndValid(FILE_2);
    verify(fileStorageService).getImageFromImagePath(Mockito.anyString());
  }

  private byte[] mockFile2(String filePath) throws IOException {
    File file = new File(filePath);
    file.mkdirs();
    int width = 100;
    int height = 100;
    BufferedImage img = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR);
    ImageIO.write(img, JPG, file);
    byte[] bytearr = FileUtils.readFileToByteArray(file);
    return bytearr;
  }

  private byte[] mockFile3(String filePath) throws IOException {
    File file = new File(filePath);
    file.mkdirs();
    int width = 300;
    int height = 300;
    BufferedImage img = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR);
    ImageIO.write(img, JPG, file);
    byte[] bytearr = FileUtils.readFileToByteArray(file);
    return bytearr;
  }


  @AfterEach
  public void tearDown() throws Exception {
    deleteFolder(PATH);
    Mockito.verifyNoMoreInteractions(mandatoryParameterHelper, xgpService);
  }
}
