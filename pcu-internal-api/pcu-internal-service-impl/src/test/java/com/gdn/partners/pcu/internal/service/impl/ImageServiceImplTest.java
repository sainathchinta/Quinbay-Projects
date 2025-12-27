package com.gdn.partners.pcu.internal.service.impl;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.service.impl.exception.ImageValidationException;
import com.gdn.partners.pcu.internal.service.model.UploadAttributeImageRequest;
import com.gdn.partners.pcu.internal.web.model.request.UploadImageRequest;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.partners.pcu.internal.client.feign.XGPFeign;
import com.gdn.partners.pcu.internal.properties.ImageProperties;
import com.gdn.partners.pcu.internal.properties.SystemParameterProperties;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.multipart.MultipartFile;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ImageServiceImplTest {
  private static final String FILE_NAME = "/MTA-10000.jpg";
  private static final int IMAGE_SIZE = 10;
  private static final String PATH = "image";
  private static final String FILE_2 = "/MTA-0315060/apple_mouse_full02.jpg";
  public static final String JPG = "jpg";
  private static final String ORIGINAL_FILENAME = "originalFilename.jpg";
  private static final String FILE_3 = "apple-mouse-full02";
  private UploadAttributeImageRequest uploadAttributeImageRequest;
  private MultipartFile multipartFile;
  private static final String JPEG = "jpeg";

  @Mock
  private XGPFeign xgpFeign;

  @InjectMocks
  private ImageServiceImpl imageService;

  @Mock
  ImageProperties imageProperties;

  @Mock
  SystemParameterProperties systemParameterProperties;

  @Mock
  FileStorageService fileStorageService;

  private UploadImageRequest uploadImageRequest;

  @BeforeEach
  public void setUp() throws Exception {
    uploadImageRequest = new UploadImageRequest();
    uploadImageRequest.setImageFileName(FILE_2);
    uploadAttributeImageRequest = new UploadAttributeImageRequest();
    uploadAttributeImageRequest.setImageFileName(FILE_2);

    ReflectionTestUtils.setField(imageService, "imageUploadSizeThresholdInMB", 10);
  }

  @Test
  void checkImageSizeTest() throws IOException {
    mockFile(PATH + FILE_NAME);
    Mockito.when(imageProperties.getSourceDirectory()).thenReturn(PATH);
    Mockito.when(imageProperties.getMaxReviewSize()).thenReturn(81920);
    imageService.checkImageSize(FILE_NAME, Boolean.FALSE);
    Mockito.verify(imageProperties).getSourceDirectory();
    Mockito.verify(imageProperties).getMaxReviewSize();
  }

  @Test
  void uploadDocumentTest() throws Exception {
    mockFile(PATH + FILE_NAME + "/sellerCode-brandCode");
    Mockito.when(systemParameterProperties.getDirectoryBrandAuthDocPath()).thenReturn(PATH);
    imageService.uploadDocument(FILE_NAME, new byte[]{}, "sellerCode", "brandCode");
    Mockito.verify(systemParameterProperties).getDirectoryBrandAuthDocPath();
  }

  @Test
  void uploadDocumentExceptionTest() throws Exception {
    mockFile(PATH + FILE_NAME + "/sellerCode-brandCode");
    Mockito.when(systemParameterProperties.getDirectoryBrandAuthDocPath()).thenReturn(PATH);
    imageService.uploadDocument(FILE_NAME, new byte[]{}, "sellerCode", "brandCode");
    Mockito.verify(systemParameterProperties).getDirectoryBrandAuthDocPath();
  }

  @Test
  void uploadDocumentExceptionDocExistsTest() throws Exception {
    mockFile(PATH + "/brandCode-sellerCode" + FILE_NAME);
    Mockito.when(systemParameterProperties.getDirectoryBrandAuthDocPath()).thenReturn(PATH);
    imageService.uploadDocument(FILE_NAME, new byte[]{}, "sellerCode", "brandCode");
    Mockito.verify(systemParameterProperties).getDirectoryBrandAuthDocPath();
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    file.mkdirs();
    int width = 640;
    int height = 320;
    BufferedImage img = new BufferedImage(width, height, BufferedImage.TYPE_3BYTE_BGR);
    ImageIO.write(img, "jpg", file);
  }

  private void deleteFolder(String folderPath) throws IOException {
    FileUtils.deleteDirectory(new File(folderPath));
  }

  @Test
  void checkImageSizeActiveImageTest() throws IOException {
    Mockito.when(imageProperties.getMaxReviewSize()).thenReturn(10);
    Mockito.when(fileStorageService.getImageSize(FILE_NAME)).thenReturn(IMAGE_SIZE);
    boolean response = imageService.checkImageSize(FILE_NAME, Boolean.TRUE);
    Mockito.verify(imageProperties).getMaxReviewSize();
    Mockito.verify(fileStorageService).getImageSize(FILE_NAME);
    Assertions.assertEquals(true, response);
  }

  @Test
  void checkImageSizeActiveImageResponseFalseTest() throws IOException {
    Mockito.when(imageProperties.getMaxReviewSize()).thenReturn(1);
    Mockito.when(fileStorageService.getImageSize(FILE_NAME)).thenReturn(IMAGE_SIZE);
    boolean response = imageService.checkImageSize(FILE_NAME, Boolean.TRUE);
    Mockito.verify(imageProperties).getMaxReviewSize();
    Mockito.verify(fileStorageService).getImageSize(FILE_NAME);
    Assertions.assertEquals(false, response);
  }

  @Test
  void uploadAttributeImageTest() throws Exception {
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
    try {
      this.imageService.uploadAttributeImage(multipartFile, FILE_2);
    } catch (ApiIncorrectInputDataException ex) {
      Assertions.assertNotNull(ex);
    }
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
      this.imageService.uploadAttributeImage(multipartFile, FILE_3);
    } catch (ApiIncorrectInputDataException ex) {
      Assertions.assertNotNull(ex);
    } finally {
      verify(fileStorageService).uploadAttributeImages(uploadAttributeImageRequest);
    }
  }

  @Test
  public void uploadImageTest() throws Exception {
    mockFile(PATH + FILE_2);
    uploadImageRequest.setActive(Boolean.TRUE);
    uploadImageRequest.setOriginalFileType(JPEG);
    this.imageService.uploadImage(uploadImageRequest);
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
  }

  @Test
  public void uploadImageTestActiveFalseException() throws Exception {
    mockFile(PATH + FILE_2);
    uploadImageRequest.setActive(Boolean.FALSE);
    uploadImageRequest.setOriginalFileType(JPEG);
    when(fileStorageService.uploadImage(uploadImageRequest)).thenThrow(Exception.class);
    when(this.imageProperties.getBasePath()).thenReturn(PATH);
    when(this.imageProperties.getFullPath()).thenReturn(PATH);
    when(this.imageProperties.getMediumPath()).thenReturn(PATH);
    when(this.imageProperties.getThumbnailPath()).thenReturn(PATH);
    try {
      this.imageService.uploadImage(uploadImageRequest);
    }
    catch (ApiIncorrectInputDataException e) {
      Assertions.assertNotNull(e);
    }
    finally {
      verify(fileStorageService).uploadImage(uploadImageRequest);
    }
  }

  @Test
  public void uploadImageJpgTest() throws Exception {
    mockFile(PATH + FILE_2);
    uploadImageRequest.setActive(Boolean.TRUE);
    uploadImageRequest.setOriginalFileType(JPG);
    this.imageService.uploadImage(uploadImageRequest);
  }

  @Test
  public void uploadImageNotJpegTest() throws Exception {
    mockFile(PATH + FILE_2);
    uploadImageRequest.setActive(Boolean.TRUE);
    uploadImageRequest.setOriginalFileType(JPEG);
    this.imageService.uploadImage(uploadImageRequest);
  }

  @Test
  public void uploadImageExceptionTest() throws Exception {
    mockFile(PATH + FILE_2);
    uploadImageRequest.setActive(Boolean.TRUE);
    uploadImageRequest.setOriginalFileType(JPEG);
    Mockito.doThrow(ImageValidationException.class).when(fileStorageService)
        .uploadActiveImages(Mockito.any(UploadImageRequest.class));
    try {
      this.imageService.uploadImage(uploadImageRequest);
    } catch (ApiIncorrectInputDataException ex) {
      Assertions.assertNotNull(ex);
    }
  }

  @AfterEach
  public void tearDown() throws Exception {
    deleteFolder(PATH);
    Mockito.verifyNoMoreInteractions(xgpFeign);
    Mockito.verifyNoMoreInteractions(imageProperties);
  }
}