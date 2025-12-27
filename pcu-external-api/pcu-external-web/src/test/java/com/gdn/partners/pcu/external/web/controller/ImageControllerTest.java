package com.gdn.partners.pcu.external.web.controller;

import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.model.ImageApiPath;
import com.gdn.partners.pcu.external.service.ImageService;
import com.gdn.partners.pcu.external.service.model.request.UploadAttributeImageRequest;
import com.gdn.partners.pcu.external.service.model.request.UploadImageRequest;
import com.gdn.partners.pcu.external.web.helper.TestHelper;
import com.gdn.partners.pcu.external.web.model.response.ApiErrorCode;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import java.io.File;
import java.io.IOException;
import java.util.List;

import static com.gdn.partners.pcu.external.model.Constants.REQUEST_ID;
import static com.gdn.partners.pcu.external.model.Constants.STORE_ID;
import static com.gdn.partners.pcu.external.model.Constants.USER_NAME;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

public class ImageControllerTest extends TestHelper {

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private ImageService imageService;

  @InjectMocks
  private ImageController imageController;


  private MockMultipartFile multipartFile;
  private UploadImageRequest uploadImageRequest;
  private UploadAttributeImageRequest uploadAttributeImageRequest;

  @Captor
  private ArgumentCaptor<UploadImageRequest> uploadImageRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<UploadAttributeImageRequest> uploadAttributeImageRequestArgumentCaptor;

  private byte[] imageContent;
  private static final String FILE = "/full//81/MTA-0315060/apple_mouse_full02.jpg";
  private static final String FILE_INVALID = "/full//81/MTA-0315060/apple_mouse_full02.gif";
  private static final String FILE_NAME = "fileName";
  private static final String IMAGE_FILE_NAME = "imageFileName";
  private static final String ACTIVE = "active";
  private static final String PATH = "path";
  private static final String ORIGINAL_FILENAME = "originalFilename.jpg";
  private static final String ORIGINAL_FILENAME_WEBP = "originalFilename.gif";
  private static final String JPG = "jpg";
  private static final String JPEG = "jpeg";
  private static final String PNG = "png";
  private static final String WEBP = "webp";

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = standaloneSetup(this.imageController).build();
    imageContent = new byte[]{-1, -40, -20, -10};
    uploadImageRequest = new UploadImageRequest();
    uploadImageRequest.setImageFileName(FILE);
    uploadImageRequest.setBytes(imageContent);
    uploadImageRequest.setRetryRequest(Boolean.FALSE);
    uploadImageRequest.setActive(Boolean.TRUE);
    uploadAttributeImageRequest = new UploadAttributeImageRequest();
    uploadAttributeImageRequest.setImageFileName(FILE);
    uploadAttributeImageRequest.setBytes(imageContent);
    ReflectionTestUtils.setField(imageController, "imageFormatsSupported",
        List.of(JPEG, JPG, PNG, WEBP));
  }

  @Test
  public void getImageDetailSuccessTrueTest() throws Exception {
    when(this.imageService.getImageDetail(FILE, Boolean.TRUE)).thenReturn(imageContent);
    MockHttpServletRequestBuilder requestBuilder =
          get(ImageApiPath.BASE_PATH + ImageApiPath.IMAGE_DETAIL)
              .param(FILE_NAME, FILE)
              .param(ACTIVE, String.valueOf(Boolean.TRUE))
              .accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
              .sessionAttr(Constants.SESSION, getDefaultSession());
    MvcResult result = mockMvc.perform(requestBuilder).andExpect(status().isOk()).andReturn();
    verify(this.imageService).getImageDetail(FILE, Boolean.TRUE);
    assertEquals("image/jpg", result.getResponse().getContentType());
    assertEquals(4, result.getResponse().getContentAsByteArray().length);
  }

  @Test
  public void uploadImageSuccessTrueTest() throws Exception {
    ReflectionTestUtils.setField(imageController, "imageUploadSizeThresholdInMB", 4);
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("image", ORIGINAL_FILENAME, "image/jpeg",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(this.imageService.uploadImage(Mockito.any(UploadImageRequest.class))).thenReturn(Boolean.TRUE);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(mandatoryParameterHelper.getUsername()).thenReturn(USER_NAME);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    this.mockMvc
        .perform(MockMvcRequestBuilders.multipart(ImageApiPath.BASE_PATH + ImageApiPath.UPLOAD_IMAGE)
                .file(multipartFile).param(IMAGE_FILE_NAME, FILE)
            .param(ACTIVE, String.valueOf(Boolean.TRUE))
            .accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .sessionAttr(Constants.SESSION, getDefaultSession())).andExpect(status().isOk())
            .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    verify(this.imageService).uploadImage(uploadImageRequestArgumentCaptor.capture());
  }

  @Test
  public void uploadImageExceptionTest() throws Exception {
    ReflectionTestUtils.setField(imageController, "imageUploadSizeThresholdInMB", 4);
    mockFile(PATH + FILE_INVALID);
    multipartFile = new MockMultipartFile("image", ORIGINAL_FILENAME_WEBP, "image/jpeg",
        FileUtils.readFileToByteArray(new File(PATH + FILE_INVALID)));
    when(this.imageService.uploadImage(Mockito.any(UploadImageRequest.class))).thenReturn(Boolean.TRUE);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(mandatoryParameterHelper.getUsername()).thenReturn(USER_NAME);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    Assertions.assertThrows(Exception.class, () -> this.mockMvc.perform(
            MockMvcRequestBuilders.multipart(ImageApiPath.BASE_PATH + ImageApiPath.UPLOAD_IMAGE)
                .file(multipartFile).param(IMAGE_FILE_NAME, FILE)
                .param(ACTIVE, String.valueOf(Boolean.TRUE)).accept(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION,
                    getDefaultSession()))
        .andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true))));
  }

  @Test
  public void uploadImageTest_withImageSizeExceedingThreshold() throws Exception {
    Exception exception = null;
    ReflectionTestUtils.setField(imageController, "imageUploadSizeThresholdInMB", 0);
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("image", ORIGINAL_FILENAME, "image/jpeg",
      FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(this.imageService.uploadImage(Mockito.any(UploadImageRequest.class))).thenReturn(Boolean.TRUE);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(mandatoryParameterHelper.getUsername()).thenReturn(USER_NAME);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    try {
      this.mockMvc.perform(
          MockMvcRequestBuilders.multipart(ImageApiPath.BASE_PATH + ImageApiPath.UPLOAD_IMAGE).file(multipartFile).param(IMAGE_FILE_NAME, FILE)
            .param(ACTIVE, String.valueOf(Boolean.TRUE)).accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession()))
        .andExpect(status().is5xxServerError())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    }
    catch (Exception e){
      exception = e;
    }
    finally {
      Assertions.assertNotNull(exception);
      Assertions.assertEquals(String.format(ApiErrorCode.INVALID_IMAGE_SIZE.getDesc(), 0),
        exception.getCause().getMessage());
    }
  }

  @Test
  public void uploadImageSuccessFalseTest() throws Exception {
    mockFile(PATH + FILE);
    try {
      multipartFile = new MockMultipartFile("image", this.getClass().getResourceAsStream("/sample1.jpg"));
      when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
      when(mandatoryParameterHelper.getUsername()).thenReturn(USER_NAME);
      when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
      this.mockMvc.perform(
              MockMvcRequestBuilders.multipart(ImageApiPath.BASE_PATH + ImageApiPath.UPLOAD_IMAGE).file(multipartFile)
                  .param(IMAGE_FILE_NAME, FILE).param(ACTIVE, String.valueOf(Boolean.TRUE))
                  .accept(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
                  .sessionAttr(Constants.SESSION, getDefaultSession())).andExpect(status().isOk())
          .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    } catch (Exception e) {
      Assertions.assertEquals(ErrorMessages.FILE_EMPTY_ERROR_MESSAGE, e.getCause().getMessage());
    }
  }

  @Test
  public void uploadAttributeImageSuccessTrueTest() throws Exception {
    ReflectionTestUtils.setField(imageController, "imageUploadSizeThresholdInMB", 4);
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("image", FILE, "image/jpeg",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(this.imageService.uploadAttributeImage(multipartFile, FILE)).thenReturn(StringUtils.EMPTY);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    when(mandatoryParameterHelper.getUsername()).thenReturn(USER_NAME);
    when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    this.mockMvc.perform(MockMvcRequestBuilders.multipart(
                ImageApiPath.BASE_PATH + ImageApiPath.UPLOAD_ATTRIBUTE_IMAGE).file(multipartFile)
            .param(IMAGE_FILE_NAME, FILE).accept(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).sessionAttr(Constants.SESSION, getDefaultSession()))
        .andExpect(status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    verify(this.imageService).uploadAttributeImage(multipartFile, FILE);
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, imageContent);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(imageService);
    FileUtils.deleteDirectory(new File(PATH));
  }
}