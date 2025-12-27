package com.gdn.partners.pcu.external.web.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.UploadFileApiPath;
import com.gdn.partners.pcu.external.service.FileStorageService;
import com.gdn.partners.pcu.external.streaming.model.bulk.BulkProcessEntity;
import com.gdn.partners.pcu.external.web.helper.TestHelper;
import com.gdn.partners.pcu.external.web.model.request.ProcessFileType;
import com.gdn.partners.pcu.external.web.model.request.SignedUrlRequest;
import com.gdn.partners.pcu.external.web.model.response.SignedUrlResponse;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.FileInputStream;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;


public class UploadFileControllerTest extends TestHelper {

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;


  private MockMultipartFile multipartFile;

  private static final String FILE_NAME = "file.pdf";
  private static final String FILE_FOLDER = "Product";
  private static final String PRODUCT_SKU = "PRODUCT_SKU";


  @InjectMocks
  private UploadFileController uploadFileController;

  @Mock
  private FileStorageService fileStorageService;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(uploadFileController).build();
  }

  @Test
  public void uploadEvidenceFileGcsEnabledTrueTest() throws Exception {
    multipartFile = new MockMultipartFile("request", FILE_NAME, null, generateDummyPDFMultipartFile().getBytes());
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);

    this.mockMvc.perform(
            MockMvcRequestBuilders.multipart(UploadFileApiPath.BASE_PATH + UploadFileApiPath.FILE)
                .file(multipartFile)
                .param("keyword", PRODUCT_SKU)
                .param("processFileType", ProcessFileType.EVIDENCE_FILE.getValue())
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

    verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void uploadEvidenceFileProcessFileDifferentTest() throws Exception {
    multipartFile = new MockMultipartFile("request", FILE_NAME, null, generateDummyPDFMultipartFile().getBytes());
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);

    this.mockMvc.perform(
            MockMvcRequestBuilders.multipart(UploadFileApiPath.BASE_PATH + UploadFileApiPath.FILE)
                .file(multipartFile)
                .param("keyword", PRODUCT_SKU)
                .param("processFileType", StringUtils.EMPTY)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());

  }

  @Test
  public void uploadEvidenceFileGcsEnabledFalseTest() throws Exception {
    multipartFile = new MockMultipartFile("request", FILE_NAME, null, generateDummyPDFMultipartFile().getBytes());
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);

    this.mockMvc.perform(
            MockMvcRequestBuilders.multipart(UploadFileApiPath.BASE_PATH + UploadFileApiPath.FILE)
                .file(multipartFile)
                .param("keyword", PRODUCT_SKU)
                .param("processFileType", ProcessFileType.EVIDENCE_FILE.getValue())
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
  }

  @Test
  public void uploadEvidenceFileEmptyRequestTest() throws Exception {
    byte[] arr = new byte[0];
    multipartFile = new MockMultipartFile("request", arr);
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Assertions.assertThrows(Exception.class, () -> this.mockMvc.perform(
            MockMvcRequestBuilders.multipart(UploadFileApiPath.BASE_PATH + UploadFileApiPath.FILE)
                .file(multipartFile).param("keyword", PRODUCT_SKU)
                .param("processFileType", ProcessFileType.EVIDENCE_FILE.getValue())
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk()));

  }

  @Test
  public void generateSignedUrlTest() throws Exception {
    SignedUrlRequest request = new SignedUrlRequest();
    request.setFileName("sample-video.mp4");
    request.setProcessType(BulkProcessEntity.PRODUCT_BASIC_INFO.name());

    SignedUrlResponse response = SignedUrlResponse.builder()
        .signedUrl("https://signed-url.com/upload")
        .uploadedPath("/test/path/sample-video.mp4")
        .bulkProcessCode("test-process-code")
        .expiresAt(System.currentTimeMillis() + 900000) // 15 minutes from now
        .build();

    when(fileStorageService.generateSignedUrl(Mockito.any(SignedUrlRequest.class))).thenReturn(response);
    when(mandatoryParameterHelper.getRequestId()).thenReturn("req-123");

    this.mockMvc.perform(MockMvcRequestBuilders.post(UploadFileApiPath.BASE_PATH + UploadFileApiPath.GENERATE_SIGNED_URL)
        .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
        .content(new ObjectMapper().writeValueAsString(request))).andExpect(status().isOk())
      .andExpect(jsonPath("$.value.uploadedPath").value("/test/path/sample-video.mp4"))
      .andExpect(jsonPath("$.value.bulkProcessCode").value("test-process-code"))
      .andExpect(jsonPath("$.value.expiresAt").exists())
      .andExpect(jsonPath("$.requestId").value("req-123"));

    verify(fileStorageService).generateSignedUrl(Mockito.any(SignedUrlRequest.class));
    verify(mandatoryParameterHelper).getRequestId();
  }


  private MultipartFile generateDummyPDFMultipartFile() throws Exception {
    File file = generateDummyPDFFile();
    byte[] fileData = IOUtils.toByteArray(new FileInputStream(file));
    MultipartFile multipartFile = new MockMultipartFile("file", FILE_NAME, null, fileData);
    return multipartFile;
  }

  private File generateDummyPDFFile() throws Exception {
    ClassLoader classLoader = getClass().getClassLoader();
    File file = new File(classLoader.getResource(FILE_FOLDER + File.separator + FILE_NAME).getFile());
    return file;
  }
}

