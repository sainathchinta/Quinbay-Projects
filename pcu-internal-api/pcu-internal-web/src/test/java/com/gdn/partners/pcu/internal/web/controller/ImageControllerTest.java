package com.gdn.partners.pcu.internal.web.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Map;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.partners.pcu.internal.properties.ProductImageProperties;
import com.gdn.partners.pcu.internal.web.model.request.UploadImageRequest;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.ImageApiPath;
import com.gdn.partners.pcu.internal.service.ImageService;
import com.gdn.partners.pcu.internal.web.TestApplication;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;

@SpringBootTest(classes = TestApplication.class)
@AutoConfigureMockMvc
@ExtendWith(MockitoExtension.class)
@TestPropertySource(properties = {"image.upload.size.threshold.mb=4",
    "replace.undefined.with.product.code.switch=true"})
public class ImageControllerTest extends TestHelper {

  private static final String FILE_NAME = "MTA-10000/";
  public static final String SELLER_CODE = "sellerCode";
  public static final String BRAND_CODE = "brandCode";

  @MockBean
  private ClientParameterHelper clientParameterHelper;

  @MockBean
  private ImageService imageService;

  @MockBean
  private ProductController productController;

  @MockBean
  private ProductImageProperties productImageProperties;

  @InjectMocks
  private ImageController imageController;

  @Captor
  private ArgumentCaptor<UploadImageRequest> uploadImageRequestArgumentCaptor;

  private byte[] imageContent;
  private byte[] imageContentInvalid;
  private byte[] webpImageBytes;
  private static final String FILE = "/full//81/MTA-0315060/apple_mouse_full02.jpg";
  private static final String FILE_JPEG = "/full//81/MTA-0315060/apple_mouse_full02.jpeg";
  private static final String FILE_PNG = "/full//81/MTA-0315060/apple_mouse_full02.png";
  private static final String FILE_PDF = "/full//81/MTA-0315060/apple_mouse_full02.pdf";
  private static final String FILE_WEBP = "/full//81/MTA-0315060/apple_mouse_full02.webp";
  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final String DOCUMENT_FILE_NAME = "documentFileName";
  private static final String ACTIVE = "active";
  private static final String PATH = "path";
  private MockMultipartFile multipartFile;
  private static final String ORIGINAL_FILENAME = "originalFilename.jpg";
  private static final String ORIGINAL_FILENAME_PDF = "originalFilename.pdf";
  private static final String ORIGINAL_FILENAME_JPEG = "originalFilename.jpeg";
  private static final String ORIGINAL_FILENAME_PNG = "originalFilename.png";
  private static final String ORIGINAL_FILENAME_WEBP = "originalFilename.webp";
  private static final String IMAGE_FILE_NAME = "imageFileName";

  @BeforeEach
  public void setUp() throws Exception {
    mockMvc = MockMvcBuilders.standaloneSetup(imageController).build();
    imageContent = new byte[]{-1, -40, -20, -10};
    imageContentInvalid = new byte[]{1, 2, 3, 4};
    ReflectionTestUtils.setField(productController, "productNameTrim", true);
    ReflectionTestUtils.setField(imageController, "imageUploadSizeThresholdInMB", 4);
    when(productImageProperties.getValidExtensionsToMagicNumbers()).thenReturn(
        Map.of(".jpeg", "ffd8", ".jpg", "ffd8", ".png", "8950", ".webp", "52494646"));
  }

  @Test
  void checkImageSizeTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(imageService.checkImageSize(FILE_NAME, Boolean.FALSE)).thenReturn(true);
    MockHttpServletRequestBuilder requestBuilder =
        get(ImageApiPath.BASE_PATH + ImageApiPath.CHECK_SIZE)
            .param("imageFileName", FILE_NAME)
            .param("active", String.valueOf(Boolean.FALSE))
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(imageService).checkImageSize(FILE_NAME, Boolean.FALSE);
  }

  @Test
  void checkImageSizeExceptionTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    doThrow(IOException.class).when(imageService).checkImageSize(FILE_NAME, Boolean.FALSE);
    MockHttpServletRequestBuilder requestBuilder =
        get(ImageApiPath.BASE_PATH + ImageApiPath.CHECK_SIZE)
            .param("imageFileName", FILE_NAME)
            .param("active", String.valueOf(Boolean.FALSE))
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(clientParameterHelper).getRequestId();
    verify(imageService).checkImageSize(FILE_NAME, Boolean.FALSE);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(clientParameterHelper);
    verifyNoMoreInteractions(imageService);
    FileUtils.deleteDirectory(new File(PATH));
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, imageContent);
  }

  private void mockFileInvalid(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, imageContentInvalid);
  }

  private void mockWebPImage(String filePath) throws IOException {
    File file = new File(filePath);
    webpImageBytes =
        Files.readAllBytes(Paths.get("src/test/resources/imageValidator/webp_test.webp"));
    FileUtils.writeByteArrayToFile(file, webpImageBytes);
  }

  @Test
  void uploadDocumentSuccessTrueTest() throws Exception {
    ReflectionTestUtils.setField(imageController, "imageUploadSizeThresholdInMB", 4);
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("document", ORIGINAL_FILENAME, "image/jpeg",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    doNothing().when(imageService).uploadDocument(Mockito.anyString(), Mockito.any(), Mockito.anyString(),
        Mockito.anyString());
    doReturn(Constants.REQUEST_ID).when(clientParameterHelper).getRequestId();
    this.mockMvc.perform(
            MockMvcRequestBuilders.multipart(ImageApiPath.BASE_PATH + ImageApiPath.UPLOAD_DOCUMENT)
                .file(multipartFile)
                .param(DOCUMENT_FILE_NAME, FILE).param(ACTIVE, String.valueOf(Boolean.TRUE))
                .param(SELLER_CODE, SELLER_CODE)
                .param(BRAND_CODE, SELLER_CODE)
                .accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
                .sessionAttr(Constants.SESSION, getDefaultSession())).andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(this.imageService).uploadDocument(Mockito.anyString(), Mockito.any(), Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  void uploadDocumentSuccessExceptionTest() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("document", ORIGINAL_FILENAME, "image/jpeg",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    doThrow(Exception.class).when(imageService).uploadDocument(Mockito.anyString(), Mockito.any(), Mockito.anyString(),
        Mockito.anyString());
    doReturn(Constants.REQUEST_ID).when(clientParameterHelper).getRequestId();
    this.mockMvc.perform(
            MockMvcRequestBuilders.multipart(ImageApiPath.BASE_PATH + ImageApiPath.UPLOAD_DOCUMENT)
                .file(multipartFile)
                .param(DOCUMENT_FILE_NAME, FILE).param(ACTIVE, String.valueOf(Boolean.TRUE))
                .param(SELLER_CODE, SELLER_CODE)
                .param(BRAND_CODE, SELLER_CODE)
                .accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
                .sessionAttr(Constants.SESSION, getDefaultSession())).andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    verify(clientParameterHelper).getRequestId();
    verify(this.imageService).uploadDocument(Mockito.anyString(), Mockito.any(), Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  void uploadDocumentSuccessFalseTest() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("document", this.getClass().getResourceAsStream("/sample1.jpg"));
    doReturn(Constants.REQUEST_ID).when(clientParameterHelper).getRequestId();
    this.mockMvc.perform(
            MockMvcRequestBuilders.multipart(ImageApiPath.BASE_PATH + ImageApiPath.UPLOAD_DOCUMENT)
                .file(multipartFile)
                .param(DOCUMENT_FILE_NAME, FILE).param(ACTIVE, String.valueOf(Boolean.TRUE))
                .param(SELLER_CODE, SELLER_CODE)
                .param(BRAND_CODE, SELLER_CODE)
                .accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
                .sessionAttr(Constants.SESSION, getDefaultSession())).andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  void uploadAttributeImageSuccessTrueTest() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("image", FILE, "image/jpeg",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(this.imageService.uploadAttributeImage(multipartFile, FILE)).thenReturn(StringUtils.EMPTY);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    this.mockMvc.perform(MockMvcRequestBuilders.multipart(
                ImageApiPath.BASE_PATH + ImageApiPath.UPLOAD_ATTRIBUTE_IMAGE).file(multipartFile)
            .param(IMAGE_FILE_NAME, FILE).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession()))
        .andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    verify(this.imageService).uploadAttributeImage(multipartFile, FILE);
    verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  void uploadImageSuccessTrueTest() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("image", ORIGINAL_FILENAME, "image/jpeg",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(this.imageService.uploadImage(Mockito.any(UploadImageRequest.class))).thenReturn(
        Boolean.TRUE);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    this.mockMvc.perform(
            MockMvcRequestBuilders.multipart(ImageApiPath.BASE_PATH + ImageApiPath.UPLOAD_IMAGE)
                .file(multipartFile).param(IMAGE_FILE_NAME, FILE)
                .param(ACTIVE, String.valueOf(Boolean.TRUE)).accept(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION,
                    getDefaultSession()))
        .andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(this.imageService).uploadImage(uploadImageRequestArgumentCaptor.capture());
  }

  @Test
  void uploadImageSuccessTrueTest_webPImage() throws Exception {
    mockWebPImage(PATH + FILE);
    multipartFile = new MockMultipartFile("image", ORIGINAL_FILENAME_WEBP, "image/webp",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(this.imageService.uploadImage(Mockito.any(UploadImageRequest.class))).thenReturn(
        Boolean.TRUE);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    this.mockMvc.perform(
            MockMvcRequestBuilders.multipart(ImageApiPath.BASE_PATH + ImageApiPath.UPLOAD_IMAGE)
                .file(multipartFile).param(IMAGE_FILE_NAME, FILE)
                .param(ACTIVE, String.valueOf(Boolean.TRUE)).accept(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION,
                    getDefaultSession()))
        .andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(this.imageService).uploadImage(uploadImageRequestArgumentCaptor.capture());
  }

  @Test
  void uploadImageSuccessTrueTest_replaceUndefinedPath() throws Exception {
    ReflectionTestUtils.setField(imageController, "replaceUndefinedWithProductCode", true);
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("image", ORIGINAL_FILENAME, "image/jpeg",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(this.imageService.uploadImage(Mockito.any(UploadImageRequest.class))).thenReturn(
        Boolean.TRUE);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    this.mockMvc.perform(
            MockMvcRequestBuilders.multipart(ImageApiPath.BASE_PATH + ImageApiPath.UPLOAD_IMAGE)
                .file(multipartFile).param(IMAGE_FILE_NAME, "undefined/"+FILE)
                .param(ACTIVE, String.valueOf(Boolean.TRUE)).accept(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION,
                    getDefaultSession()))
        .andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(this.imageService).uploadImage(uploadImageRequestArgumentCaptor.capture());
  }

  @Test
  void uploadImageSuccessTrueTest_replaceUndefinedPathTrueFileNameEmpty() throws Exception {
    ReflectionTestUtils.setField(imageController, "replaceUndefinedWithProductCode", true);
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("image", ORIGINAL_FILENAME, "image/jpeg",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(this.imageService.uploadImage(Mockito.any(UploadImageRequest.class))).thenReturn(
        Boolean.TRUE);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    this.mockMvc.perform(
            MockMvcRequestBuilders.multipart(ImageApiPath.BASE_PATH + ImageApiPath.UPLOAD_IMAGE)
                .file(multipartFile).param(IMAGE_FILE_NAME, "")
                .param(ACTIVE, String.valueOf(Boolean.TRUE)).accept(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION,
                    getDefaultSession()))
        .andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(this.imageService).uploadImage(uploadImageRequestArgumentCaptor.capture());
  }

  @Test
  void uploadImageSuccessTrueTest_jpeg() throws Exception {
    mockFile(PATH + FILE_JPEG);
    multipartFile = new MockMultipartFile("image", ORIGINAL_FILENAME_JPEG, "image/jpeg",
        FileUtils.readFileToByteArray(new File(PATH + FILE_JPEG)));
    when(this.imageService.uploadImage(Mockito.any(UploadImageRequest.class))).thenReturn(
        Boolean.TRUE);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    this.mockMvc.perform(
            MockMvcRequestBuilders.multipart(ImageApiPath.BASE_PATH + ImageApiPath.UPLOAD_IMAGE)
                .file(multipartFile).param(IMAGE_FILE_NAME, FILE)
                .param(ACTIVE, String.valueOf(Boolean.TRUE)).accept(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION,
                    getDefaultSession()))
        .andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(this.imageService).uploadImage(uploadImageRequestArgumentCaptor.capture());
  }

  @Test
  void uploadImageSuccessTrueTest_png() throws Exception {
    mockFile(PATH + FILE_PNG);
    multipartFile = new MockMultipartFile("image", ORIGINAL_FILENAME_PNG, "image/png",
        FileUtils.readFileToByteArray(new File(PATH + FILE_PNG)));
    when(this.imageService.uploadImage(Mockito.any(UploadImageRequest.class))).thenReturn(
        Boolean.TRUE);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    this.mockMvc.perform(
            MockMvcRequestBuilders.multipart(ImageApiPath.BASE_PATH + ImageApiPath.UPLOAD_IMAGE)
                .file(multipartFile).param(IMAGE_FILE_NAME, FILE)
                .param(ACTIVE, String.valueOf(Boolean.TRUE)).accept(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION,
                    getDefaultSession()))
        .andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    verify(this.imageService).uploadImage(uploadImageRequestArgumentCaptor.capture());
  }

  @Test
  void uploadImageSuccessFalseTest_pdfFile() throws Exception {
    Exception exception = null;
    mockFile(PATH + FILE_PDF);
    multipartFile = new MockMultipartFile("image", ORIGINAL_FILENAME_PDF, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE_PDF)));
    when(this.imageService.uploadImage(Mockito.any(UploadImageRequest.class))).thenReturn(
        Boolean.TRUE);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    try {
      this.mockMvc.perform(
              MockMvcRequestBuilders.multipart(ImageApiPath.BASE_PATH + ImageApiPath.UPLOAD_IMAGE)
                  .file(multipartFile).param(IMAGE_FILE_NAME, FILE_PDF)
                  .param(ACTIVE, String.valueOf(Boolean.TRUE)).accept(MediaType.APPLICATION_JSON)
                  .accept(MediaType.APPLICATION_JSON)
                  .sessionAttr(Constants.SESSION, getDefaultSession()))
          .andExpect(status().is5xxServerError())
          .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    } catch (Exception e) {
      exception = e;
    } finally {
      Assertions.assertNotNull(exception);
      verify(clientParameterHelper).getRequestId();
    }
  }

  @Test
  void uploadImageSuccessFalseTest_fileNameJPGButContentPDF() throws Exception {
    Exception exception = null;
    mockFileInvalid(PATH + FILE_PDF);
    multipartFile = new MockMultipartFile("image", ORIGINAL_FILENAME_JPEG, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE_PDF)));
    when(this.imageService.uploadImage(Mockito.any(UploadImageRequest.class))).thenReturn(
        Boolean.TRUE);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    try {
      this.mockMvc.perform(
              MockMvcRequestBuilders.multipart(ImageApiPath.BASE_PATH + ImageApiPath.UPLOAD_IMAGE)
                  .file(multipartFile).param(IMAGE_FILE_NAME, FILE_PDF)
                  .param(ACTIVE, String.valueOf(Boolean.TRUE)).accept(MediaType.APPLICATION_JSON)
                  .accept(MediaType.APPLICATION_JSON)
                  .sessionAttr(Constants.SESSION, getDefaultSession()))
          .andExpect(status().is5xxServerError())
          .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    } catch (Exception e) {
      exception = e;
    } finally {
      Assertions.assertNotNull(exception);
      verify(clientParameterHelper).getRequestId();
    }
  }

  @Test
  public void uploadImageTest_withImageSizeExceedingThreshold() throws Exception {
    Exception exception = null;
    ReflectionTestUtils.setField(imageController, "imageUploadSizeThresholdInMB", 0);
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("image", ORIGINAL_FILENAME, "image/jpeg",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(this.imageService.uploadImage(Mockito.any(UploadImageRequest.class))).thenReturn(
        Boolean.TRUE);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    try {
      this.mockMvc.perform(
              MockMvcRequestBuilders.multipart(ImageApiPath.BASE_PATH + ImageApiPath.UPLOAD_IMAGE)
                  .file(multipartFile).param(IMAGE_FILE_NAME, FILE)
                  .param(ACTIVE, String.valueOf(Boolean.TRUE)).accept(MediaType.APPLICATION_JSON)
                  .accept(MediaType.APPLICATION_JSON)
                  .sessionAttr(Constants.SESSION, getDefaultSession()))
          .andExpect(status().is5xxServerError())
          .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    } catch (Exception e) {
      exception = e;
    } finally {
      Assertions.assertNotNull(exception);
      Assertions.assertEquals(String.format(ErrorMessages.INVALID_IMAGE_SIZE, 0),
          exception.getCause().getMessage());
      verify(clientParameterHelper).getRequestId();
    }
  }

  @Test
  public void uploadImageSuccessFalseTest() throws Exception {
    mockFile(PATH + FILE);
    try {
      multipartFile =
          new MockMultipartFile("image", this.getClass().getResourceAsStream("/sample1.jpg"));
      when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
      this.mockMvc.perform(
              MockMvcRequestBuilders.multipart(ImageApiPath.BASE_PATH + ImageApiPath.UPLOAD_IMAGE)
                  .file(multipartFile).param(IMAGE_FILE_NAME, FILE)
                  .param(ACTIVE, String.valueOf(Boolean.TRUE)).accept(MediaType.APPLICATION_JSON)
                  .accept(MediaType.APPLICATION_JSON)
                  .sessionAttr(Constants.SESSION, getDefaultSession())).andExpect(status().isOk())
          .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    } catch (Exception e) {
      Assertions.assertEquals(ErrorMessages.FILE_EMPTY_ERROR_MESSAGE, e.getCause().getMessage());
      verify(clientParameterHelper).getRequestId();
    }
  }

  @Test
  public void uploadImageOriginalFileNameNullTest() throws Exception {
    Exception exception = null;
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("image", null, "image/jpeg",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(this.imageService.uploadImage(Mockito.any(UploadImageRequest.class))).thenReturn(
        Boolean.TRUE);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    try {
      this.mockMvc.perform(
              MockMvcRequestBuilders.multipart(ImageApiPath.BASE_PATH + ImageApiPath.UPLOAD_IMAGE)
                  .file(multipartFile).param(IMAGE_FILE_NAME, FILE)
                  .param(ACTIVE, String.valueOf(Boolean.TRUE)).accept(MediaType.APPLICATION_JSON)
                  .accept(MediaType.APPLICATION_JSON)
                  .sessionAttr(Constants.SESSION, getDefaultSession()))
          .andExpect(status().is5xxServerError())
          .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    } catch (Exception e) {
      exception = e;
    } finally {
      Assertions.assertNotNull(exception);
      Assertions.assertEquals(ErrorCategory.VALIDATION.getMessage(),
          exception.getCause().getMessage());
      verify(clientParameterHelper).getRequestId();
    }
  }
}