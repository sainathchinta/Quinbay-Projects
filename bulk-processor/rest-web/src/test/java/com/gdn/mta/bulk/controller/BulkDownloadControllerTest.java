package com.gdn.mta.bulk.controller;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Collections;
import java.util.Map;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.dto.BulkInternalPendingRequestResponse;
import com.gdn.mta.bulk.dto.CampaignBulkDownloadRequest;
import com.gdn.mta.bulk.dto.PickupPointCodesRequestDTO;
import com.gdn.mta.bulk.dto.TaggedProductFilterRequest;
import com.gdn.mta.bulk.service.BulkDownloadServiceWrapper;
import com.gdn.mta.bulk.service.download.BulkProcessDownloadService;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;
import org.apache.commons.lang3.StringUtils;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.BulkDownloadErrorCode;
import com.gdn.mta.bulk.BulkDownloadException;
import com.gdn.mta.bulk.UnifiedBulkDownloadException;
import com.gdn.mta.bulk.dto.BulkDownloadFileContentDTO;
import com.gdn.mta.bulk.dto.BulkDownloadFileContentResponse;
import com.gdn.mta.bulk.dto.BulkDownloadMailRecipient;
import com.gdn.mta.bulk.dto.BulkDownloadProductDTO;
import com.gdn.mta.bulk.dto.BulkDownloadProductRequest;
import com.gdn.mta.bulk.dto.BulkDownloadProductResponse;
import com.gdn.mta.bulk.dto.UnifiedBulkDownloadDTO;
import com.gdn.mta.bulk.dto.UnifiedBulkDownloadResponse;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.FileType;
import com.gdn.mta.bulk.service.BulkDownloadService;
import com.google.common.collect.ImmutableMap;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

/**
 * Created by virajjasani on 09/09/16.
 */
public class BulkDownloadControllerTest {

  private static final Integer PRODUCT_SIZE = 123;
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "channel";
  private static final String DEFAULT_CLIENT_ID = "client";
  private static final String DEFAULT_USERNAME = "developer";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BPCODE";
  private static final String DEFAULT_REQUEST_ID = "REQUEST_ID";
  private static final String DEFAULT_FILE_ID = "FILE_ID";
  private static final String DEFAULT_GDN_SKU = "SKU-0001";
  private static final String MAIL_TO = "mail-to";
  private static final String MAIL_CC = "mail-cc";
  private static final String MAIL_BCC = "mail-bcc";
  private static final String BULK_PROCESS_CODE = "BULK_PROCESS_CODE";
  private static final String STATE = "STATE";
  private static final Map<String, Boolean> PRIVILEGE_MAP =
      ImmutableMap.<String, Boolean>builder().put("isPrivilegedToReadProductType", true)
          .put("isPrivilegedToReadPickupPoint", true).put("isPrivilegedToReadPrice", true)
          .put("isPrivilegedToReadAvailableStock", true)
          .put("isPrivilegedToReadDisplayBuyable", true).put("isPrivilegedToReadO2O", true).build();
  private static final String DOWNLOAD_VENDOR_PRODUCT_EXCEL = "/download-vendor-product";

  private BulkDownloadProductRequest bulkDownloadProductRequest;
  private BulkDownloadFileContentDTO bulkDownloadFileContentDTO;
  private BulkDownloadProductDTO bulkDownloadProductDTO;
  private BulkDownloadMailRecipient bulkDownloadMailRecipient;
  private ProductLevel3SummaryRequest request = null;
  private UnifiedBulkDownloadDTO unifiedBulkDownloadDTO;
  private ObjectMapper objectMapper = new ObjectMapper();
  private MockMvc mockMvc;

  @InjectMocks
  private BulkDownloadController bulkDownloadController;

  @Mock
  private BulkDownloadService bulkDownloadService;

  @Mock
  private BulkProcessDownloadService bulkProcessDownloadService;

  @Mock
  private BulkDownloadServiceWrapper bulkDownloadServiceWrapper;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(this.bulkDownloadController).build();
    bulkDownloadProductRequest = new BulkDownloadProductRequest();
    bulkDownloadProductRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkDownloadProductRequest.setPrivilegedMap(PRIVILEGE_MAP);
    bulkDownloadProductRequest.setProductSize(PRODUCT_SIZE);
    bulkDownloadProductRequest.setEmailTo(MAIL_TO);
    bulkDownloadProductRequest.setEmailCc(MAIL_CC);
    bulkDownloadFileContentDTO = new BulkDownloadFileContentDTO();
    bulkDownloadFileContentDTO.setFileAvailable(true);
    bulkDownloadProductDTO = new BulkDownloadProductDTO();
    bulkDownloadProductDTO.setCreatedBy(DEFAULT_USERNAME);
    bulkDownloadMailRecipient = new BulkDownloadMailRecipient();
    bulkDownloadMailRecipient.setEmailTo(MAIL_TO);
    bulkDownloadMailRecipient.setEmailCc(MAIL_CC);
    
    bulkDownloadProductRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    request = new ProductLevel3SummaryRequest();
    request.setGdnSku(DEFAULT_GDN_SKU);
    bulkDownloadProductRequest.setRequest(request);

    unifiedBulkDownloadDTO = new UnifiedBulkDownloadDTO();
    unifiedBulkDownloadDTO.setFilePath("filePath");
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(bulkDownloadService);
    Mockito.verifyNoMoreInteractions(bulkDownloadServiceWrapper);
  }

  @Test
  public void downloadBulkProductsTest() throws Exception {
    GdnBaseRestResponse response = bulkDownloadController
        .downloadBulkProducts(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID,
            DEFAULT_REQUEST_ID, DEFAULT_USERNAME, bulkDownloadProductRequest);
    Mockito.verify(bulkDownloadService)
        .preProcess(DEFAULT_REQUEST_ID, PRIVILEGE_MAP, DEFAULT_BUSINESS_PARTNER_CODE, PRODUCT_SIZE, request,
            DEFAULT_USERNAME, bulkDownloadMailRecipient);
    Assertions.assertTrue(response.isSuccess());
  }

  @Test
  public void downloadBulkProductsTest_failedResponse() throws Exception {
    Mockito.doThrow(Exception.class).when(bulkDownloadService).preProcess(Mockito.anyString(), Mockito.anyMap(), Mockito.anyString(), 
        Mockito.anyInt(), Mockito.any(), Mockito.anyString(), Mockito.any());
        
    GdnBaseRestResponse response = bulkDownloadController
        .downloadBulkProducts(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID,
            DEFAULT_REQUEST_ID, DEFAULT_USERNAME, bulkDownloadProductRequest);
    
    Mockito.verify(bulkDownloadService)
    .preProcess(DEFAULT_REQUEST_ID, PRIVILEGE_MAP, DEFAULT_BUSINESS_PARTNER_CODE, PRODUCT_SIZE, request,
        DEFAULT_USERNAME, bulkDownloadMailRecipient);
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getFileContentsTest() throws Exception {
    when(bulkDownloadService.getFileContents(DEFAULT_FILE_ID))
        .thenReturn(bulkDownloadFileContentDTO);
    GdnRestSingleResponse<BulkDownloadFileContentResponse> response = bulkDownloadController
        .getFileContents(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID,
            DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_FILE_ID);
    Mockito.verify(bulkDownloadService, Mockito.times(1)).getFileContents(DEFAULT_FILE_ID);
    Assertions.assertTrue(response.isSuccess());
  }

  @Test
  public void getFileContentsTest_failedResponse() throws Exception {
    GdnRestSingleResponse<BulkDownloadFileContentResponse> response = bulkDownloadController
        .getFileContents(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID,
            DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_FILE_ID);
    Mockito.verify(bulkDownloadService, Mockito.times(1)).getFileContents(DEFAULT_FILE_ID);
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getBulkDownloadProductInfoTest() throws Exception {
    when(bulkDownloadService.getBulkDownloadProductByRequestId(DEFAULT_FILE_ID))
        .thenReturn(bulkDownloadProductDTO);
    GdnRestSingleResponse<BulkDownloadProductResponse> response = bulkDownloadController
        .getBulkDownloadProductInfo(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID,
            DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_FILE_ID);
    Mockito.verify(bulkDownloadService).getBulkDownloadProductByRequestId(DEFAULT_FILE_ID);
    Assertions.assertTrue(response.isSuccess());
  }
  
  @Test
  public void getBulkDownloadProductInfoWhenError() throws Exception {
    when(bulkDownloadService.getBulkDownloadProductByRequestId(DEFAULT_FILE_ID))
        .thenThrow(Exception.class);
    GdnRestSingleResponse<BulkDownloadProductResponse> response = bulkDownloadController
        .getBulkDownloadProductInfo(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID,
            DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_FILE_ID);
    Mockito.verify(bulkDownloadService).getBulkDownloadProductByRequestId(DEFAULT_FILE_ID);
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getBulkDownloadFile_noRecordFound_ResponseFailure() throws Exception {
    when(bulkDownloadService.getBulkDownloadProduct(anyString(), anyString())).thenThrow(
        new BulkDownloadException(BulkDownloadErrorCode.RECORD_NOT_FOUND.toString(),
            BulkDownloadErrorCode.RECORD_NOT_FOUND.getErrorMessage()));
    GdnRestSingleResponse<BulkDownloadFileContentResponse> response = bulkDownloadController
        .getBulkDownloadFile(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID,
            DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_FILE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertEquals(BulkDownloadErrorCode.RECORD_NOT_FOUND.getErrorMessage(),
        response.getErrorMessage());
    verify(bulkDownloadService).getBulkDownloadProduct(anyString(), anyString());
  }

  @Test
  public void getBulkDownloadFile_diffMerchant_ResponseFailure() throws Exception {
    BulkDownloadProductDTO testDto = new BulkDownloadProductDTO();
    testDto.setBusinessPartnerCode("123");
    when(bulkDownloadService.getBulkDownloadProduct(anyString(), anyString())).thenReturn(testDto);
    GdnRestSingleResponse<BulkDownloadFileContentResponse> response = bulkDownloadController
        .getBulkDownloadFile(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID,
            DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_FILE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Assertions.assertFalse(response.isSuccess());
    verify(bulkDownloadService).getBulkDownloadProduct(anyString(), anyString());
  }

  @Test
  public void getBulkDownloadFile_recordExist_fileNotFound_ResponseFailure() throws Exception {
    BulkDownloadProductDTO testDto = new BulkDownloadProductDTO();
    testDto.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    when(bulkDownloadService.getBulkDownloadProduct(anyString(), anyString())).thenReturn(testDto);
    BulkDownloadFileContentDTO fileContentDTO = new BulkDownloadFileContentDTO();
    fileContentDTO.setFileAvailable(false);
    when(bulkDownloadService.getFileContents(any(BulkDownloadProductDTO.class))).thenReturn(fileContentDTO);
    GdnRestSingleResponse<BulkDownloadFileContentResponse> response = bulkDownloadController
        .getBulkDownloadFile(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID,
            DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_FILE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Assertions.assertFalse(response.isSuccess());
    verify(bulkDownloadService).getFileContents(any(BulkDownloadProductDTO.class));
    verify(bulkDownloadService).getBulkDownloadProduct(anyString(), anyString());
  }

  @Test
  public void getBulkDownloadFile_fileExist_ResponseTrue() throws Exception {
    BulkDownloadProductDTO testDto = new BulkDownloadProductDTO();
    testDto.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    testDto.setEntityType(BulkProcessEntity.ORDER.toString());
    testDto.setFileName("test.csv");
    when(bulkDownloadService.getBulkDownloadProduct(anyString(), anyString())).thenReturn(testDto);
    BulkDownloadFileContentDTO fileContentDTO = new BulkDownloadFileContentDTO();
    fileContentDTO.setFileAvailable(true);
    fileContentDTO.setFileContent("test".getBytes());
    when(bulkDownloadService.getFileContents(any(BulkDownloadProductDTO.class)))
        .thenReturn(fileContentDTO);
    GdnRestSingleResponse<BulkDownloadFileContentResponse> response = bulkDownloadController
        .getBulkDownloadFile(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID,
            DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_FILE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertTrue(StringUtils.isEmpty(response.getErrorMessage()));
    Assertions.assertEquals(FileType.CSV.name(), response.getValue().getFileType());
    Assertions.assertEquals(BulkProcessEntity.ORDER.name(), response.getValue().getEntityType());
    verify(bulkDownloadService).getBulkDownloadProduct(anyString(), anyString());
    verify(bulkDownloadService).getFileContents(any(BulkDownloadProductDTO.class));
  }

  @Test
  public void getBulkDownloadFile_fileExist_ResponseTrue_EmptyBpCode() throws Exception {
    BulkDownloadProductDTO testDto = new BulkDownloadProductDTO();
    testDto.setEntityType(BulkProcessEntity.ORDER.toString());
    testDto.setFileName("test.csv");
    when(bulkDownloadService.getBulkDownloadProduct(anyString(), anyString())).thenReturn(testDto);
    BulkDownloadFileContentDTO fileContentDTO = new BulkDownloadFileContentDTO();
    fileContentDTO.setFileAvailable(true);
    fileContentDTO.setFileContent("test".getBytes());
    when(bulkDownloadService.getFileContents(any(BulkDownloadProductDTO.class)))
        .thenReturn(fileContentDTO);
    GdnRestSingleResponse<BulkDownloadFileContentResponse> response = bulkDownloadController
        .getBulkDownloadFile(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID,
            DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_FILE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertTrue(StringUtils.isEmpty(response.getErrorMessage()));
    Assertions.assertEquals(FileType.CSV.name(), response.getValue().getFileType());
    Assertions.assertEquals(BulkProcessEntity.ORDER.name(), response.getValue().getEntityType());
    verify(bulkDownloadService).getBulkDownloadProduct(anyString(), anyString());
    verify(bulkDownloadService).getFileContents(any(BulkDownloadProductDTO.class));
  }

  @Test
  public void getBulkProcessProductFileTest() throws Exception {

    BulkDownloadFileContentDTO fileContentDTO = new BulkDownloadFileContentDTO();
    fileContentDTO.setFileAvailable(true);
    fileContentDTO.setFileContent("test".getBytes());
    fileContentDTO.setRequestId(BULK_PROCESS_CODE);
    when(bulkDownloadService.getBulkProcessProductFile(DEFAULT_STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(fileContentDTO);
    GdnRestSingleResponse<BulkDownloadFileContentResponse> response = bulkDownloadController
        .getBulkProcessProductFile(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID,
            DEFAULT_REQUEST_ID, DEFAULT_USERNAME, BULK_PROCESS_CODE);
    verify(bulkDownloadService).getBulkProcessProductFile(DEFAULT_STORE_ID, BULK_PROCESS_CODE);
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertTrue(StringUtils.isEmpty(response.getErrorMessage()));
  }

  @Test
  public void getBulkProcessProductFile_ResponseFalse() throws Exception {

    BulkDownloadFileContentDTO fileContentDTO = new BulkDownloadFileContentDTO();
    fileContentDTO.setFileAvailable(false);
    fileContentDTO.setFileContent(null);
    fileContentDTO.setRequestId(null);
    when(bulkDownloadService.getBulkProcessProductFile(DEFAULT_STORE_ID, null))
        .thenThrow(new Exception());
    GdnRestSingleResponse<BulkDownloadFileContentResponse> response = bulkDownloadController
        .getBulkProcessProductFile(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID,
            DEFAULT_REQUEST_ID, DEFAULT_USERNAME, null);
    verify(bulkDownloadService).getBulkProcessProductFile(DEFAULT_STORE_ID, null);
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void downloadProductUnifiedTemplateTest() throws Exception {
    when(bulkDownloadService.downloadProductUnifiedTemplate(DEFAULT_STORE_ID,
        DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE, Collections.emptySet())).thenReturn(unifiedBulkDownloadDTO);
    GdnRestSimpleResponse<UnifiedBulkDownloadResponse> response = bulkDownloadController
        .downloadProductUnifiedTemplate(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID,
            DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE);
    verify(bulkDownloadService).downloadProductUnifiedTemplate(DEFAULT_STORE_ID,
        DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE, Collections.emptySet());
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertTrue(StringUtils.isEmpty(response.getErrorMessage()));
  }

  @Test
  public void downloadProductUnifiedTemplateExceptionTest() throws Exception {
    when(bulkDownloadService.downloadProductUnifiedTemplate(DEFAULT_STORE_ID,
        DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE, Collections.emptySet())).thenThrow(new Exception());
    GdnRestSimpleResponse<UnifiedBulkDownloadResponse> response = bulkDownloadController
        .downloadProductUnifiedTemplate(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID,
            DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE);
    verify(bulkDownloadService).downloadProductUnifiedTemplate(DEFAULT_STORE_ID,
        DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE, Collections.emptySet());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void downloadProductUnifiedTemplateUnifiedBulkDownloadExceptionTest() throws Exception {
    when(bulkDownloadService
        .downloadProductUnifiedTemplate(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            Collections.emptySet())).thenThrow(
        new UnifiedBulkDownloadException(BulkDownloadErrorCode.DOWNLOAD_IN_PROGRESS.toString(),
            BulkDownloadErrorCode.DOWNLOAD_IN_PROGRESS.getErrorMessage()));
    GdnRestSimpleResponse<UnifiedBulkDownloadResponse> response = bulkDownloadController
        .downloadProductUnifiedTemplate(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID, DEFAULT_REQUEST_ID,
            DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE);
    verify(bulkDownloadService)
        .downloadProductUnifiedTemplate(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE,
            Collections.emptySet());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void downloadProductUnifiedTemplateV2Test() throws Exception {
    when(bulkDownloadService.downloadProductUnifiedTemplate(DEFAULT_STORE_ID,
        DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE, null)).thenReturn(unifiedBulkDownloadDTO);
    GdnRestSimpleResponse<UnifiedBulkDownloadResponse> response = bulkDownloadController
        .downloadProductUnifiedTemplateV2(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID,
            DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE,
            new PickupPointCodesRequestDTO());
    verify(bulkDownloadService).downloadProductUnifiedTemplate(DEFAULT_STORE_ID,
        DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertTrue(StringUtils.isEmpty(response.getErrorMessage()));
  }

  @Test
  public void downloadProductUnifiedTemplateV2UnifiedBulkDownloadExceptionTest() throws Exception {
    when(bulkDownloadService.downloadProductUnifiedTemplate(DEFAULT_STORE_ID,
        DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE, null))
        .thenThrow(new UnifiedBulkDownloadException(StringUtils.EMPTY, StringUtils.EMPTY));
    GdnRestSimpleResponse<UnifiedBulkDownloadResponse> response = bulkDownloadController
        .downloadProductUnifiedTemplateV2(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID,
            DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE,
            new PickupPointCodesRequestDTO());
    verify(bulkDownloadService).downloadProductUnifiedTemplate(DEFAULT_STORE_ID,
        DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void downloadProductUnifiedTemplateV2ExceptionTest() throws Exception {
    when(bulkDownloadService.downloadProductUnifiedTemplate(DEFAULT_STORE_ID,
        DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE, Collections.emptySet())).thenThrow(new Exception());
    GdnRestSimpleResponse<UnifiedBulkDownloadResponse> response = bulkDownloadController
        .downloadProductUnifiedTemplateV2(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID,
            DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE,
            new PickupPointCodesRequestDTO());
    verify(bulkDownloadService).downloadProductUnifiedTemplate(DEFAULT_STORE_ID,
        DEFAULT_REQUEST_ID, DEFAULT_BUSINESS_PARTNER_CODE, null);
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void bulkVendorProductActionsTest() throws Exception {
    Mockito.doNothing().when(bulkDownloadServiceWrapper)
        .downloadAndSendVendorProductMail(eq(DEFAULT_STORE_ID),
            Mockito.any(FilterSummaryRequest.class), Mockito.anyString(), eq(DEFAULT_USERNAME));
    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders
        .post(BulkProcessController.BASE_PATH + DOWNLOAD_VENDOR_PRODUCT_EXCEL)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID)
        .param(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, DEFAULT_CHANNEL_ID)
        .param(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, DEFAULT_CLIENT_ID)
        .param(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID)
        .param(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME)
        .content(objectMapper.writeValueAsString(new FilterSummaryRequest()));
    this.mockMvc.perform(request).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(bulkDownloadServiceWrapper)
        .downloadAndSendVendorProductMail(eq(DEFAULT_STORE_ID),
            Mockito.any(FilterSummaryRequest.class), Mockito.anyString(), eq(DEFAULT_USERNAME));
  }

  @Test
  public void bulkVendorProductActions_exceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(bulkDownloadServiceWrapper)
        .downloadAndSendVendorProductMail(eq(DEFAULT_STORE_ID),
            Mockito.any(FilterSummaryRequest.class), Mockito.anyString(), eq(DEFAULT_USERNAME));
    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders
        .post(BulkProcessController.BASE_PATH + DOWNLOAD_VENDOR_PRODUCT_EXCEL)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID)
        .param(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, DEFAULT_CHANNEL_ID)
        .param(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, DEFAULT_CLIENT_ID)
        .param(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID)
        .param(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME)
        .content(objectMapper.writeValueAsString(new FilterSummaryRequest()));
    this.mockMvc.perform(request).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(bulkDownloadServiceWrapper)
        .downloadAndSendVendorProductMail(eq(DEFAULT_STORE_ID),
            Mockito.any(FilterSummaryRequest.class), Mockito.anyString(), eq(DEFAULT_USERNAME));
  }

  @Test
  public void checkCampaignBulkDownloadRequestTest() throws Exception {
    Mockito.when(bulkDownloadService
        .countNumberOfCampaignDownloads(eq(DEFAULT_STORE_ID), eq(DEFAULT_BUSINESS_PARTNER_CODE),
            any(CampaignBulkDownloadRequest.class))).thenReturn((long) 100);
    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders.post(
        BulkProcessController.BASE_PATH + "/" + BulkDownloadController.CHECK_CAMPAIGN_PENDING_BULK_DOWNLOAD_REQUESTS)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID)
        .param(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, DEFAULT_CHANNEL_ID)
        .param(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, DEFAULT_CLIENT_ID)
        .param(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID)
        .param(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME)
        .param("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .content(objectMapper.writeValueAsString(new CampaignBulkDownloadRequest(DEFAULT_GDN_SKU, 0)));
    this.mockMvc.perform(request).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(bulkDownloadService)
        .countNumberOfCampaignDownloads(eq(DEFAULT_STORE_ID), eq(DEFAULT_BUSINESS_PARTNER_CODE),
            any(CampaignBulkDownloadRequest.class));
  }

  @Test
  public void checkPendingBulkDownloadRequestTest() throws Exception {
    Mockito.when(bulkDownloadService.countPendingRequestsByUsernameAndDownloadType(DEFAULT_BUSINESS_PARTNER_CODE,
        DEFAULT_USERNAME, BulkProcessEntity.STORE_COPY_PRODUCTS.name())).thenReturn(new BulkInternalPendingRequestResponse());
    GdnRestSingleResponse<BulkInternalPendingRequestResponse> response =
        bulkDownloadController.getPendingBulkRequests(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID,
            DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE,
            BulkProcessEntity.STORE_COPY_PRODUCTS.name());
    Mockito.verify(bulkDownloadService)
        .countPendingRequestsByUsernameAndDownloadType(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_USERNAME,
            BulkProcessEntity.STORE_COPY_PRODUCTS.name());
    Assertions.assertNotNull(response);
    Assertions.assertTrue(response.isSuccess());
  }

  @Test
  public void checkPendingBulkDownloadRequestSuccessFalseTest() throws Exception {
    Mockito.when(bulkDownloadService.countPendingRequestsByUsernameAndDownloadType(DEFAULT_BUSINESS_PARTNER_CODE,
        DEFAULT_USERNAME, BulkProcessEntity.STORE_COPY_PRODUCTS.name())).thenThrow(Exception.class);
    GdnRestSingleResponse<BulkInternalPendingRequestResponse> response = null;
    try {
       response =
          bulkDownloadController.getPendingBulkRequests(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID,
              DEFAULT_REQUEST_ID, DEFAULT_USERNAME, DEFAULT_BUSINESS_PARTNER_CODE, BulkProcessEntity.STORE_COPY_PRODUCTS.name());
    } catch (Exception e) {
    } finally {
      Mockito.verify(bulkDownloadService)
          .countPendingRequestsByUsernameAndDownloadType(DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_USERNAME,
              BulkProcessEntity.STORE_COPY_PRODUCTS.name());
      Assertions.assertNull(response);
    }
  }

  @Test
  public void checkCampaignBulkDownloadRequestExceptionTest() throws Exception {
    final MockHttpServletRequestBuilder request = MockMvcRequestBuilders.post(
        BulkProcessController.BASE_PATH + "/" + BulkDownloadController.CHECK_CAMPAIGN_PENDING_BULK_DOWNLOAD_REQUESTS)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, DEFAULT_STORE_ID)
        .param(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, DEFAULT_CHANNEL_ID)
        .param(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, DEFAULT_CLIENT_ID)
        .param(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, DEFAULT_REQUEST_ID)
        .param(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME)
        .param("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .content(objectMapper.writeValueAsString(new CampaignBulkDownloadRequest()));
    this.mockMvc.perform(request).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", Matchers.equalTo(false)));
  }

  @Test
  public void clearInProgressDownloadsTest() throws Exception {
    this.mockMvc.perform(MockMvcRequestBuilders.put(BulkProcessController.BASE_PATH +
        BulkDownloadController.CLEAR_IN_PROGRESS_DOWNLOADS).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", DEFAULT_STORE_ID)
        .param("requestId", DEFAULT_REQUEST_ID)
        .param("channelId", DEFAULT_CHANNEL_ID)
        .param("clientId", DEFAULT_CHANNEL_ID)
        .param("username", DEFAULT_USERNAME)
        .param("status", STATE)
        .param("entityType", BulkProcessEntity.STORE_COPY_PRODUCTS.name()))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.bulkDownloadService).clearInProgressDownloads(DEFAULT_STORE_ID,
        BulkProcessEntity.STORE_COPY_PRODUCTS.name(), STATE);
  }

  @Test
  public void clearInProgressDownloadsExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(bulkDownloadService)
        .clearInProgressDownloads(DEFAULT_STORE_ID, BulkProcessEntity.STORE_COPY_PRODUCTS.name(), STATE);
    this.mockMvc.perform(MockMvcRequestBuilders.put(BulkProcessController.BASE_PATH +
        BulkDownloadController.CLEAR_IN_PROGRESS_DOWNLOADS).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", DEFAULT_STORE_ID)
        .param("requestId", DEFAULT_REQUEST_ID)
        .param("channelId", DEFAULT_CHANNEL_ID)
        .param("clientId", DEFAULT_CHANNEL_ID)
        .param("username", DEFAULT_USERNAME).param("status", STATE)
            .param("entityType", BulkProcessEntity.STORE_COPY_PRODUCTS.name()))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(this.bulkDownloadService).clearInProgressDownloads(DEFAULT_STORE_ID, BulkProcessEntity.STORE_COPY_PRODUCTS.name(), STATE );
  }

  @Test
  public void processBulkDownloadTest() throws Exception {
    this.mockMvc.perform(
        MockMvcRequestBuilders.get(BulkProcessController.BASE_PATH + BulkDownloadController.PROCESS_ALL_DOWNLOAD)
            .accept(MediaType.APPLICATION_JSON).param("storeId", DEFAULT_STORE_ID)
            .param("requestId", DEFAULT_REQUEST_ID).param("channelId", DEFAULT_CHANNEL_ID)
            .param("clientId", DEFAULT_CHANNEL_ID).param("username", DEFAULT_USERNAME).param("status", STATE)
            .param("entityType", BulkProcessEntity.STORE_COPY_PRODUCTS.name()))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.bulkProcessDownloadService).processDownload(DEFAULT_STORE_ID);
  }

  @Test
  public void processBulkDownloadTaggedProductsTest() throws Exception {
    TaggedProductFilterRequest taggedProductFilterRequest = new TaggedProductFilterRequest();
    taggedProductFilterRequest.setEmailAddress("abc@gmail.com");
    this.mockMvc.perform(
            MockMvcRequestBuilders.post(BulkProcessController.BASE_PATH + BulkDownloadController.DOWNLOAD_TAGGED_PRODUCTS)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", DEFAULT_STORE_ID)
                .param("requestId", DEFAULT_REQUEST_ID)
                .param("channelId", DEFAULT_CHANNEL_ID)
                .param("clientId", DEFAULT_CHANNEL_ID)
                .param("username", DEFAULT_USERNAME)
                .content(objectMapper.writeValueAsString(taggedProductFilterRequest)))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(this.bulkProcessDownloadService).processDownloadTaggedProducts(taggedProductFilterRequest);
  }
  @Test
  public void processBulkDownloadTaggedProductsExceptionTest() throws Exception {
    TaggedProductFilterRequest taggedProductFilterRequest = new TaggedProductFilterRequest();
    taggedProductFilterRequest.setEmailAddress("");
    this.mockMvc.perform(
            MockMvcRequestBuilders.post(BulkProcessController.BASE_PATH + BulkDownloadController.DOWNLOAD_TAGGED_PRODUCTS)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", DEFAULT_STORE_ID)
                .param("requestId", DEFAULT_REQUEST_ID)
                .param("channelId", DEFAULT_CHANNEL_ID)
                .param("clientId", DEFAULT_CHANNEL_ID)
                .param("username", DEFAULT_USERNAME)
                .content(objectMapper.writeValueAsString(taggedProductFilterRequest)))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
  }
}
