package com.gdn.mta.bulk.controller;

import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.HYPHEN;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.INSTANT_PICKUP_DEL;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.INSTANT_PICKUP_DEL_TYPE;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.INSTANT_PICKUP_UPDATE;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.INSTANT_PICKUP_UPDATE_TYPE;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.PRODUCT_UPDATE_EVENT;
import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

import com.gdn.mta.bulk.dto.BulkProcessExternalUploadRequest;
import com.gdn.mta.bulk.service.ExternalProductCreationService;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.utils.URIBuilder;
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
import org.mockito.verification.VerificationMode;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.http.converter.ResourceHttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.dto.BulkAddCampaignProductDTO;
import com.gdn.mta.bulk.dto.BulkBasicInfoRequest;
import com.gdn.mta.bulk.dto.BulkPendingRequestsResponse;
import com.gdn.mta.bulk.dto.BulkProcessAddCampaignProductRequest;
import com.gdn.mta.bulk.dto.BulkProcessExternalUploadRequest;
import com.gdn.mta.bulk.dto.BulkProcessNotesRequest;
import com.gdn.mta.bulk.dto.BulkProcessNotesResponse;
import com.gdn.mta.bulk.dto.BulkProcessPublishRequest;
import com.gdn.mta.bulk.dto.BulkProcessRequest;
import com.gdn.mta.bulk.dto.BulkProcessResponse;
import com.gdn.mta.bulk.dto.BulkProcessStatusListingResponse;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkProcessUpdateRequest;
import com.gdn.mta.bulk.dto.BulkProcessUploadRequest;
import com.gdn.mta.bulk.dto.BulkProcessV2Request;
import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.dto.GenericTemplateFileType;
import com.gdn.mta.bulk.dto.QrExcelUploadRequest;
import com.gdn.mta.bulk.dto.WholeSaleCountResponse;
import com.gdn.mta.bulk.dto.product.BulkProcessSubjectToVatRequest;
import com.gdn.mta.bulk.dto.product.constant.TrackerConstants;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.models.GenericErrorMessages;
import com.gdn.mta.bulk.service.BulkBasicInfoUpdateService;
import com.gdn.mta.bulk.service.BulkDeleteService;
import com.gdn.mta.bulk.service.BulkProcessDataService;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.mta.bulk.service.BulkProcessServiceWrapper;
import com.gdn.mta.bulk.service.BulkUpdateService;
import com.gdn.mta.bulk.service.BulkUpsertService;
import com.gdn.mta.bulk.service.EANProductLevel4BulkUpdateService;
import com.gdn.mta.bulk.service.ExternalProductCreationService;
import com.gdn.mta.bulk.service.ProcessorService;
import com.gdn.mta.bulk.service.TrackerService;
import com.gdn.mta.bulk.service.UnifiedBulkDownloadService;
import com.gdn.mta.bulk.service.util.BeanUtils;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.google.common.collect.ImmutableMap;

public class BulkProcessControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "channel";
  private static final String DEFAULT_CLIENT_ID = "client";
  private static final String DEFAULT_USERNAME = "developer";
  private static final String DEFAULT_BULK_PROCESS_CODE = UUID.randomUUID().toString();
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLI-00106";
  private static final Date DEFAULT_DATE = Calendar.getInstance().getTime();
  private static final Integer DEFAULT_PAGE = 0;
  private static final Integer DEFAULT_SIZE = 10;
  private static final Pageable DEFAULT_PAGEABLE = PageRequest.of(DEFAULT_PAGE, DEFAULT_SIZE);
  private static final VerificationMode AT_LEAST_ONE = Mockito.times(1);
  private static final VerificationMode AT_LEAST_NONE = Mockito.times(0);
  private static final String FILE_NAME = "bulk-update-template.xlsx";
  private static final String DEFAULT_BULK_PROCESS_TYPE = "ProductLevel3";
  private static final String INSTORE_BULK_PROCESS_TYPE = "InStore";
  private static final String DEFAULT_BEAN_BULK_UPDATE = "ProductLevel3BulkUpdateService";
  private static final int DEFAULT_PENDING_REQUEST_COUNT = 1;
  private static final String PRIVILEGED_MAP_ERROR =
      "privileged map for business partner should have size: 7";
  private static final Map<String, Boolean> PRIVILEGED_MAP =
      ImmutableMap.<String, Boolean>builder().put("isPrivilegedToEditAvailableStock", true)
          .put("isPrivilegedToEditPrice", true).put("isPrivilegedToEditDisplayBuyable", true)
          .put("isPrivilegedToEditPickupPoint", true).put("isPrivilegedToEditO2O", true)
          .put("isOnlyExternalUser", false).put("isPrivilegedToEditCncStatus", true).build();
  private static final Map<String, Boolean> PRIVILEGED_MAP_INVALID =
      ImmutableMap.<String, Boolean>builder().put("isPrivilegedToEditAvailableStock", true)
          .put("isPrivilegedToEditPrice", true)
          .put("isPrivilegedToEditDisplayBuyable", true)
          .put("isPrivilegedToEditO2O", true).build();
  private static final int DEFAULT_ABORT_TIME = 3600;
  private static final String DEFAULT_PATH = "path";
  private static final String FILE_TYPE = "FILE_TYPE";

  private MockMvc mockMvc;
  private ObjectMapper objectMapper;
  private String requestId;
  private QrExcelUploadRequest qrExcelUploadRequest = new QrExcelUploadRequest();

  @InjectMocks
  private BulkProcessController bulkProcessController;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private BulkProcessServiceWrapper bulkProcessServiceWrapper;

  @Mock
  private TrackerService trackerService;

  @Mock
  private AutowireCapableBeanFactory autowireCapableBeanFactory;

  @Mock
  private BulkUpdateService bulkUpdateService;

  @Mock
  private BulkUpsertService bulkUpsertService;

  @Mock
  private UnifiedBulkDownloadService unifiedBulkDownloadService;

  @Mock
  private BulkProcessDataService bulkProcessDataService;

  @Captor
  private ArgumentCaptor<BulkUpdateProcessDTO> bulkUpdateProcessDTOArgumentCaptor;

  @Mock
  private BulkBasicInfoUpdateService bulkBasicInfoUpdateService;

  @Mock
  private ExternalProductCreationService externalProductCreationService;

  @Mock
  private EANProductLevel4BulkUpdateService eanProductLevel4BulkUpdateService;

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.bulkProcessService, this.trackerService, this.autowireCapableBeanFactory,
        this.bulkUpdateService, this.bulkUpsertService, this.unifiedBulkDownloadService,
        this.bulkBasicInfoUpdateService, this.externalProductCreationService, this.eanProductLevel4BulkUpdateService);
  }

  private List<BulkProcess> getBulkProcess() throws Exception {
    List<BulkProcess> bulkProcess = new ArrayList<BulkProcess>();
    bulkProcess.add(new BulkProcess(DEFAULT_BULK_PROCESS_CODE, DEFAULT_BULK_PROCESS_TYPE,
        DEFAULT_BUSINESS_PARTNER_CODE, null,
        null, BulkProcess.STATUS_PENDING, null, new ArrayList<BulkProcessNotes>()));
    bulkProcess.add(new BulkProcess(DEFAULT_BULK_PROCESS_CODE + 1, DEFAULT_BULK_PROCESS_TYPE,
        DEFAULT_BUSINESS_PARTNER_CODE,
        DEFAULT_DATE, null, BulkProcess.STATUS_IN_PROGRESS, null,
        new ArrayList<BulkProcessNotes>()));
    for (BulkProcess bulkProcessItem : bulkProcess) {
      bulkProcessItem.setStoreId(DEFAULT_STORE_ID);
      bulkProcessItem.setCreatedBy(DEFAULT_USERNAME);
      bulkProcessItem.setCreatedDate(DEFAULT_DATE);
      bulkProcessItem.setUpdatedBy(DEFAULT_USERNAME);
      bulkProcessItem.setUpdatedDate(DEFAULT_DATE);
    }
    return bulkProcess;
  }


  private BulkPendingRequestsResponse getBulkPendingRequestsResponse() throws Exception {
    BulkPendingRequestsResponse bulkPendingRequestsResponse = new BulkPendingRequestsResponse();
    bulkPendingRequestsResponse.setPendingRequestsCount(DEFAULT_PENDING_REQUEST_COUNT);
    return bulkPendingRequestsResponse;
  }

  private BulkProcessRequest getBulkProcessRequest() throws Exception {
    BulkProcessRequest bulkProcessRequest =
        new BulkProcessRequest(null, null, null, null, null, null, DEFAULT_BULK_PROCESS_CODE,
            DEFAULT_BULK_PROCESS_TYPE,
            DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_DATE, DEFAULT_DATE, BulkProcess.STATUS_FINISHED,
            null,
            new ArrayList<BulkProcessNotesRequest>());
    bulkProcessRequest.setStoreId(DEFAULT_STORE_ID);
    bulkProcessRequest.setCreatedBy(DEFAULT_USERNAME);
    bulkProcessRequest.setCreatedDate(DEFAULT_DATE);
    bulkProcessRequest.setUpdatedBy(DEFAULT_USERNAME);
    bulkProcessRequest.setUpdatedDate(DEFAULT_DATE);
    return bulkProcessRequest;
  }

  private List<BulkProcessResponse> getBulkProcessResponses() throws Exception {
    List<BulkProcessResponse> bulkProcessResponses = new ArrayList<BulkProcessResponse>();
    bulkProcessResponses
        .add(new BulkProcessResponse(null, null, null, null, null, null, DEFAULT_BULK_PROCESS_CODE,
            DEFAULT_BULK_PROCESS_TYPE, DEFAULT_BUSINESS_PARTNER_CODE, null, null,
            BulkProcess.STATUS_PENDING, null,
            new ArrayList<BulkProcessNotesResponse>()));
    bulkProcessResponses.add(
        new BulkProcessResponse(null, null, null, null, null, null, DEFAULT_BULK_PROCESS_CODE + 1,
            DEFAULT_BULK_PROCESS_TYPE, DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_DATE, null,
            BulkProcess.STATUS_IN_PROGRESS, null,
            new ArrayList<BulkProcessNotesResponse>()));
    for (BulkProcessResponse bulkProcessResponse : bulkProcessResponses) {
      bulkProcessResponse.setStoreId(DEFAULT_STORE_ID);
      bulkProcessResponse.setCreatedBy(DEFAULT_USERNAME);
      bulkProcessResponse.setCreatedDate(DEFAULT_DATE);
      bulkProcessResponse.setUpdatedBy(DEFAULT_USERNAME);
      bulkProcessResponse.setUpdatedDate(DEFAULT_DATE);
    }
    return bulkProcessResponses;
  }

  public MockMvc getMockMvc() {
    return mockMvc;
  }

  public ObjectMapper getObjectMapper() {
    return objectMapper;
  }

  public String getRequestId() {
    return requestId;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    setMockMvc(MockMvcBuilders
        .standaloneSetup(bulkProcessController)
        .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
            new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
            new MappingJackson2HttpMessageConverter()).build());
    setRequestId(UUID.randomUUID().toString());
    setObjectMapper(new ObjectMapper(new JsonFactory()));
  }

  public void setMockMvc(MockMvc mockMvc) {
    this.mockMvc = mockMvc;
  }

  public void setObjectMapper(ObjectMapper objectMapper) {
    this.objectMapper = objectMapper;
  }

  public void setRequestId(String requestId) {
    this.requestId = requestId;
  }

  @Test
  public void testDeleteByBulkProcessCode() throws Exception {
    BulkProcessRequest request = getBulkProcessRequest();
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, getRequestId());
    doNothing().when(bulkProcessService)
        .deleteByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    URI uri =
        new URIBuilder().setPath(
            BulkProcessController.BASE_PATH + BulkProcessController.DELETE_BULK_PROCESS_CODE)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
            .addParameter("username", DEFAULT_USERNAME).build();
    MvcResult result =
        getMockMvc()
            .perform(
                post(uri).accept(MediaType.APPLICATION_JSON)
                    .contentType(MediaType.APPLICATION_JSON)
                    .content(getObjectMapper().writeValueAsString(request)))
            .andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
        result.getResponse().getContentAsString());
    verify(bulkProcessService, AT_LEAST_ONE).deleteByBulkProcessCode(DEFAULT_STORE_ID,
        DEFAULT_BULK_PROCESS_CODE);
  }

  @Test
  public void testDeleteByBulkProcessCodeWithInvalidBulkProcessCode() throws Exception {
    BulkProcessRequest request = getBulkProcessRequest();
    request.setBulkProcessCode(null);
    doNothing().when(bulkProcessService)
        .deleteByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    URI uri =
        new URIBuilder().setPath(
            BulkProcessController.BASE_PATH + BulkProcessController.DELETE_BULK_PROCESS_CODE)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
            .addParameter("username", DEFAULT_USERNAME).build();
    try {
      getMockMvc().perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(getObjectMapper().writeValueAsString(request)))
          .andExpect(status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(
          e.getMessage().contains(BulkProcessController.BULK_PROCESS_CODE_MUST_NOT_BE_BLANK));
      verify(bulkProcessService, AT_LEAST_NONE)
          .deleteByBulkProcessCode(DEFAULT_STORE_ID,
              DEFAULT_BULK_PROCESS_CODE);
    }
  }

  @Test
  public void testcheckForPendingBulkProcessByMerchantCode() throws Exception {
    BulkPendingRequestsResponse bulkPendingRequestsResponse = getBulkPendingRequestsResponse();
    GdnRestSingleResponse<BulkPendingRequestsResponse> response =
        new GdnRestSingleResponse<BulkPendingRequestsResponse>(null, null, true,
            bulkPendingRequestsResponse, getRequestId());
    when(bulkProcessService.checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME, Constant.BULK_UPLOAD_TYPE,
        DEFAULT_BUSINESS_PARTNER_CODE, SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE,
        DEFAULT_BULK_PROCESS_TYPE)).thenReturn(bulkPendingRequestsResponse);
    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH
            + BulkProcessController.CHECK_PENDING_BULK_REQUESTS_BY_BP_CODE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .addParameter("bulkProcessType", DEFAULT_BULK_PROCESS_TYPE).build();
    MvcResult result =
        getMockMvc().perform(MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
        result.getResponse().getContentAsString());
    verify(bulkProcessService).checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME, Constant.BULK_UPLOAD_TYPE,
        DEFAULT_BUSINESS_PARTNER_CODE, SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE,
        DEFAULT_BULK_PROCESS_TYPE);
  }

  @Test
  public void testcheckForPendingBulkProcessByMerchantCodeWithInvalidBusinessPartnerCode()
      throws Exception {
    BulkPendingRequestsResponse bulkPendingRequestsResponse = getBulkPendingRequestsResponse();
    GdnRestSingleResponse<BulkPendingRequestsResponse> response =
        new GdnRestSingleResponse<BulkPendingRequestsResponse>(null, null, true,
            bulkPendingRequestsResponse, getRequestId());
    when(bulkProcessService.checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME, Constant.BULK_UPLOAD_TYPE,
        DEFAULT_BUSINESS_PARTNER_CODE, SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE,
        DEFAULT_BULK_PROCESS_TYPE)).thenReturn(bulkPendingRequestsResponse);
    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH
            + BulkProcessController.CHECK_PENDING_BULK_REQUESTS_BY_BP_CODE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("businessPartnerCode", StringUtils.EMPTY)
        .addParameter("bulkProcessType", DEFAULT_BULK_PROCESS_TYPE).build();
    try {
      MvcResult result =
          getMockMvc().perform(MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
    } catch (Exception e) {
      Assertions.assertTrue(
          e.getMessage().contains(BulkProcessController.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK));
      verify(bulkProcessService, AT_LEAST_NONE).checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME,
          Constant.BULK_UPLOAD_TYPE, DEFAULT_BUSINESS_PARTNER_CODE,
          SystemParameterConfigNames.DISABLE_BULK_UPDATE_VARIABLE, DEFAULT_BULK_PROCESS_TYPE);
    }
  }

  @Test
  public void testFilterByBulkProcessCode() throws Exception {
    List<BulkProcess> bulkProcess = getBulkProcess();
    List<BulkProcessResponse> bulkProcessResponses = getBulkProcessResponses();
    bulkProcessResponses.get(0).setCreatedDate(bulkProcess.get(0).getCreatedDate());
    bulkProcessResponses.get(0).setUpdatedDate(bulkProcess.get(0).getUpdatedDate());
    GdnRestSingleResponse<BulkProcessResponse> response =
        new GdnRestSingleResponse<BulkProcessResponse>(null, null, true,
            bulkProcessResponses.get(0), getRequestId());
    when(
        bulkProcessService.findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenReturn(bulkProcess.get(0));
    URI uri =
        new URIBuilder()
            .setPath(
                BulkProcessController.BASE_PATH + BulkProcessController.FILTER_BULK_PROCESS_CODE
                    + DEFAULT_BULK_PROCESS_CODE).addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .build();
    MvcResult result =
        getMockMvc()
            .perform(
                MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk())
            .andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
        result.getResponse().getContentAsString());
    verify(bulkProcessService, AT_LEAST_ONE).findByBulkProcessCode(DEFAULT_STORE_ID,
        DEFAULT_BULK_PROCESS_CODE);
  }
  
  @Test
  public void testFilterByBulkProcessCodeWhenError() throws Exception {
    List<BulkProcess> bulkProcess = getBulkProcess();
    List<BulkProcessResponse> bulkProcessResponses = getBulkProcessResponses();
    bulkProcessResponses.get(0).setCreatedDate(bulkProcess.get(0).getCreatedDate());
    bulkProcessResponses.get(0).setUpdatedDate(bulkProcess.get(0).getUpdatedDate());
    GdnRestSingleResponse<BulkProcessResponse> response =
        new GdnRestSingleResponse<BulkProcessResponse>(null, null, true,
            bulkProcessResponses.get(0), getRequestId());
    when(bulkProcessService.findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenThrow(RuntimeException.class);
    URI uri =
        new URIBuilder()
            .setPath(
                BulkProcessController.BASE_PATH + BulkProcessController.FILTER_BULK_PROCESS_CODE
                    + DEFAULT_BULK_PROCESS_CODE).addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .build();

    MvcResult result = null;
    try{
      result =
          getMockMvc()
              .perform(
                  MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
                      .contentType(MediaType.APPLICATION_JSON))
              .andExpect(status().isOk())
              .andReturn();
    } catch(Exception e){
      verify(bulkProcessService, AT_LEAST_ONE).findByBulkProcessCode(DEFAULT_STORE_ID,
          DEFAULT_BULK_PROCESS_CODE);
    }
  }

  @Test
  public void testFilterByBulkProcessCodeWithBulkProcessNotes() throws Exception {
    List<BulkProcess> bulkProcess = getBulkProcess();
    bulkProcess.get(0).getBulkProcessNotes().add(new BulkProcessNotes());
    List<BulkProcessResponse> bulkProcessResponses = getBulkProcessResponses();
    bulkProcessResponses.get(0).setCreatedDate(bulkProcess.get(0).getCreatedDate());
    bulkProcessResponses.get(0).setUpdatedDate(bulkProcess.get(0).getUpdatedDate());
    bulkProcessResponses.get(0).getBulkProcessNotes().add(new BulkProcessNotesResponse());
    GdnRestSingleResponse<BulkProcessResponse> response =
        new GdnRestSingleResponse<BulkProcessResponse>(null, null, true,
            bulkProcessResponses.get(0), getRequestId());
    when(bulkProcessService.findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenReturn(bulkProcess.get(0));
    URI uri =
        new URIBuilder()
            .setPath(
                BulkProcessController.BASE_PATH + BulkProcessController.FILTER_BULK_PROCESS_CODE
                    + DEFAULT_BULK_PROCESS_CODE).addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .build();
    MvcResult result =
        getMockMvc()
            .perform(
                MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk())
            .andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
        result.getResponse().getContentAsString());
    verify(bulkProcessService, AT_LEAST_ONE).findByBulkProcessCode(DEFAULT_STORE_ID,
        DEFAULT_BULK_PROCESS_CODE);
  }

  @Test
  public void testFilterByBulkProcessCodeWithNullBulkProcess() throws Exception {
    GdnRestSingleResponse<BulkProcessResponse> response =
        new GdnRestSingleResponse<BulkProcessResponse>(null, null, true, null, getRequestId());
    when(bulkProcessService.findByBulkProcessCode(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenReturn(null);
    URI uri =
        new URIBuilder()
            .setPath(
                BulkProcessController.BASE_PATH + BulkProcessController.FILTER_BULK_PROCESS_CODE
                    + DEFAULT_BULK_PROCESS_CODE).addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .build();
    MvcResult result =
        getMockMvc()
            .perform(
                MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk())
            .andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
        result.getResponse().getContentAsString());
    verify(bulkProcessService, AT_LEAST_ONE).findByBulkProcessCode(DEFAULT_STORE_ID,
        DEFAULT_BULK_PROCESS_CODE);
  }

  @Test
  public void testFilterByBusinessPartnerCodeAndBulkProcessTypeAndCreatedBy() throws Exception {
    List<BulkProcess> bulkProcess = getBulkProcess();
    List<BulkProcessResponse> bulkProcessResponses = getBulkProcessResponses();
    Page<BulkProcess> page = new PageImpl<BulkProcess>(bulkProcess);
    GdnRestListResponse<BulkProcessResponse> response =
        new GdnRestListResponse<BulkProcessResponse>(null, null, true, bulkProcessResponses,
            new PageMetaData(
                DEFAULT_SIZE, DEFAULT_PAGE, page.getTotalElements()), getRequestId());
    when(bulkProcessService
        .findByBusinessPartnerCodeAndBulkProcessTypeAndCreatedBy(DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BULK_PROCESS_TYPE, DEFAULT_PAGEABLE))
        .thenReturn(page);
    URI uri =
        new URIBuilder()
            .setPath(
                BulkProcessController.BASE_PATH + BulkProcessController.FILTER_BUSINESS_PARTNER_CODE
                    + DEFAULT_BUSINESS_PARTNER_CODE + "/bulk-process-type/ProductLevel3")
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
            .addParameter("username", DEFAULT_USERNAME).build();
    MvcResult result =
        getMockMvc()
            .perform(
                MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk())
            .andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
        result.getResponse().getContentAsString());
    verify(bulkProcessService, AT_LEAST_ONE)
        .findByBusinessPartnerCodeAndBulkProcessTypeAndCreatedBy(DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BULK_PROCESS_TYPE, DEFAULT_PAGEABLE);
  }
  
  @Test
  public void filterByBusinessPartnerCodeAndBulkProcessTypeAndCreatedByWhenError() throws Exception {
    List<BulkProcess> bulkProcess = getBulkProcess();
    List<BulkProcessResponse> bulkProcessResponses = getBulkProcessResponses();
    Page<BulkProcess> page = new PageImpl<BulkProcess>(bulkProcess);
    GdnRestListResponse<BulkProcessResponse> response =
        new GdnRestListResponse<BulkProcessResponse>(null, null, true, bulkProcessResponses,
            new PageMetaData(
                DEFAULT_SIZE, DEFAULT_PAGE, page.getTotalElements()), getRequestId());
    when(bulkProcessService
        .findByBusinessPartnerCodeAndBulkProcessTypeAndCreatedBy(DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BULK_PROCESS_TYPE, DEFAULT_PAGEABLE))
        .thenThrow(RuntimeException.class);
    URI uri =
        new URIBuilder()
            .setPath(
                BulkProcessController.BASE_PATH + BulkProcessController.FILTER_BUSINESS_PARTNER_CODE
                    + DEFAULT_BUSINESS_PARTNER_CODE + "/bulk-process-type/ProductLevel3")
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
            .addParameter("username", DEFAULT_USERNAME).build();
    try{
      MvcResult result =
          getMockMvc()
              .perform(
                  MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
                      .contentType(MediaType.APPLICATION_JSON))
              .andExpect(status().isOk())
              .andReturn();
    } catch(Exception e){
      verify(bulkProcessService, AT_LEAST_ONE)
          .findByBusinessPartnerCodeAndBulkProcessTypeAndCreatedBy(DEFAULT_STORE_ID,
              DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BULK_PROCESS_TYPE, DEFAULT_PAGEABLE);
    }
  }

  @Test
  public void testFilterByBusinessPartnerCodeAndCreatedBy() throws Exception {
    List<BulkProcess> bulkProcess = getBulkProcess();
    List<BulkProcessResponse> bulkProcessResponses = getBulkProcessResponses();
    Page<BulkProcess> page = new PageImpl<BulkProcess>(bulkProcess);
    GdnRestListResponse<BulkProcessResponse> response =
        new GdnRestListResponse<BulkProcessResponse>(null, null, true, bulkProcessResponses,
            new PageMetaData(
                DEFAULT_SIZE, DEFAULT_PAGE, page.getTotalElements()), getRequestId());
    when(bulkProcessService
            .findByBusinessPartnerCodeAndCreatedBy(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE,
                DEFAULT_USERNAME, DEFAULT_PAGEABLE)).thenReturn(page);
    URI uri =
        new URIBuilder()
            .setPath(
                BulkProcessController.BASE_PATH + BulkProcessController.FILTER_BUSINESS_PARTNER_CODE
                    + DEFAULT_BUSINESS_PARTNER_CODE).addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .build();
    MvcResult result =
        getMockMvc()
            .perform(
                MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
                    .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk())
            .andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
        result.getResponse().getContentAsString());
    verify(bulkProcessService, AT_LEAST_ONE)
        .findByBusinessPartnerCodeAndCreatedBy(DEFAULT_STORE_ID,
            DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_USERNAME, DEFAULT_PAGEABLE);
  }
  
  @Test
  public void testFilterByBusinessPartnerCodeAndCreatedByWhenError() throws Exception {
    List<BulkProcess> bulkProcess = getBulkProcess();
    List<BulkProcessResponse> bulkProcessResponses = getBulkProcessResponses();
    Page<BulkProcess> page = new PageImpl<BulkProcess>(bulkProcess);
    GdnRestListResponse<BulkProcessResponse> response =
        new GdnRestListResponse<BulkProcessResponse>(null, null, true, bulkProcessResponses,
            new PageMetaData(
                DEFAULT_SIZE, DEFAULT_PAGE, page.getTotalElements()), getRequestId());
    when(bulkProcessService
            .findByBusinessPartnerCodeAndCreatedBy(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE,
                DEFAULT_USERNAME, DEFAULT_PAGEABLE)).thenThrow(RuntimeException.class);
    URI uri =
        new URIBuilder()
            .setPath(
                BulkProcessController.BASE_PATH + BulkProcessController.FILTER_BUSINESS_PARTNER_CODE
                    + DEFAULT_BUSINESS_PARTNER_CODE).addParameter("storeId", DEFAULT_STORE_ID)
            .addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID)
            .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME)
            .build();
    try{
      MvcResult result =
          getMockMvc()
              .perform(
                  MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
                      .contentType(MediaType.APPLICATION_JSON))
              .andExpect(status().isOk())
              .andReturn();
    } catch(Exception e){
      verify(bulkProcessService, AT_LEAST_ONE)
          .findByBusinessPartnerCodeAndCreatedBy(DEFAULT_STORE_ID,
              DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_USERNAME, DEFAULT_PAGEABLE);
    }
  }

  @Test
  public void testUpload() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    files.put("xls", Base64.encodeBase64String(new byte[256]));
    BulkProcessUploadRequest request =
        new BulkProcessUploadRequest(DEFAULT_BULK_PROCESS_TYPE, DEFAULT_BUSINESS_PARTNER_CODE,
            files,
            new HashMap<String, String>());
    ProcessorService processorService = Mockito.mock(ProcessorService.class);
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, getRequestId());
    when(autowireCapableBeanFactory.getBean(request.getBulkProcessType() + "ProcessorService"))
        .thenReturn(processorService);
    URI uri =
        new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
            .addParameter("username", DEFAULT_USERNAME).build();
    MvcResult result =
        getMockMvc()
            .perform(
                post(uri).accept(MediaType.APPLICATION_JSON)
                    .contentType(MediaType.APPLICATION_JSON)
                    .content(getObjectMapper().writeValueAsString(request)))
            .andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
        result.getResponse().getContentAsString());
    verify(autowireCapableBeanFactory, AT_LEAST_ONE).getBean(
        request.getBulkProcessType() + "ProcessorService");
  }

  @Test
  public void testUploadAvoidSwitchOn() throws Exception {
    ReflectionTestUtils.setField(bulkProcessController, "avoidRedundantDownloadInBulkCreation",
        true);
    Map<String, String> files = new HashMap<String, String>();
    files.put("xls", Base64.encodeBase64String(new byte[256]));
    BulkProcessUploadRequest request =
        new BulkProcessUploadRequest(DEFAULT_BULK_PROCESS_TYPE, DEFAULT_BUSINESS_PARTNER_CODE,
            files,
            new HashMap<String, String>());
    ProcessorService processorService = Mockito.mock(ProcessorService.class);
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, getRequestId());
    when(autowireCapableBeanFactory.getBean(request.getBulkProcessType() + "ProcessorService"))
        .thenReturn(processorService);
    URI uri =
        new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
            .addParameter("username", DEFAULT_USERNAME).build();
    MvcResult result =
        getMockMvc()
            .perform(
                post(uri).accept(MediaType.APPLICATION_JSON)
                    .contentType(MediaType.APPLICATION_JSON)
                    .content(getObjectMapper().writeValueAsString(request)))
            .andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
        result.getResponse().getContentAsString());
    verify(autowireCapableBeanFactory, AT_LEAST_ONE).getBean(
        request.getBulkProcessType() + "ProcessorService");
  }

  @Test
  public void testUploadForApiIncorrectExceptionTest() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    files.put("xls", Base64.encodeBase64String(new byte[256]));
    BulkProcessUploadRequest request =
        new BulkProcessUploadRequest(DEFAULT_BULK_PROCESS_TYPE, DEFAULT_BUSINESS_PARTNER_CODE, files,
            new HashMap<String, String>());
    ProcessorService processorService = Mockito.mock(ProcessorService.class);
    GdnBaseRestResponse response = new GdnBaseRestResponse("", "", true, getRequestId());
    Mockito.doThrow(new ApiIncorrectInputDataException()).when(autowireCapableBeanFactory)
        .getBean(request.getBulkProcessType() + "ProcessorService");
    URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();
    try {
      MvcResult result = getMockMvc().perform(
          post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(getObjectMapper().writeValueAsString(request))).andExpect(status().isOk()).andReturn();

    } finally {
      verify(autowireCapableBeanFactory, AT_LEAST_ONE).getBean(request.getBulkProcessType() + "ProcessorService");
    }
  }

  @Test
  public void testUploadForExceptionTest() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    files.put("xls", Base64.encodeBase64String(new byte[256]));
    BulkProcessUploadRequest request =
        new BulkProcessUploadRequest(DEFAULT_BULK_PROCESS_TYPE, DEFAULT_BUSINESS_PARTNER_CODE, files,
            new HashMap<String, String>());
    ProcessorService processorService = Mockito.mock(ProcessorService.class);
    GdnBaseRestResponse response = new GdnBaseRestResponse("", "", true, getRequestId());
    Mockito.doThrow(new RuntimeException()).when(autowireCapableBeanFactory)
        .getBean(request.getBulkProcessType() + "ProcessorService");
    URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();
    try {
      MvcResult result = getMockMvc().perform(
          post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
              .content(getObjectMapper().writeValueAsString(request))).andExpect(status().isOk()).andReturn();

    } finally {
      verify(autowireCapableBeanFactory, AT_LEAST_ONE).getBean(request.getBulkProcessType() + "ProcessorService");
    }
  }

  @Test
  public void testUploadWithInvalidBulkProcessType() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    files.put("xls", Base64.encodeBase64String(new byte[256]));
    BulkProcessUploadRequest request =
        new BulkProcessUploadRequest(null, DEFAULT_BUSINESS_PARTNER_CODE, files,
            new HashMap<String, String>());
    ProcessorService processorService = Mockito.mock(ProcessorService.class);
    when(autowireCapableBeanFactory.getBean(request.getBulkProcessType() + "ProcessorService"))
        .thenReturn(processorService);
    URI uri =
        new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
            .addParameter("username", DEFAULT_USERNAME).build();
    try {
      getMockMvc().perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(getObjectMapper().writeValueAsString(request)))
          .andExpect(status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(
          e.getMessage().contains(BulkProcessController.BULK_PROCESS_TYPE_MUST_NOT_BE_BLANK));
      verify(autowireCapableBeanFactory, AT_LEAST_NONE).getBean(
          request.getBulkProcessType() + "ProcessorService");
    }
  }

  @Test
  public void testUploadWithInvalidFiles() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    BulkProcessUploadRequest request =
        new BulkProcessUploadRequest(DEFAULT_BULK_PROCESS_TYPE, DEFAULT_BUSINESS_PARTNER_CODE,
            files,
            new HashMap<String, String>());
    ProcessorService processorService = Mockito.mock(ProcessorService.class);
    when(autowireCapableBeanFactory.getBean(request.getBulkProcessType() + "ProcessorService"))
        .thenReturn(processorService);
    URI uri =
        new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
            .addParameter("username", DEFAULT_USERNAME).build();
    try {
      getMockMvc().perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(getObjectMapper().writeValueAsString(request)))
          .andExpect(status().isOk());
    } catch (Exception e) {
      Assertions.assertTrue(e.getMessage().contains(BulkProcessController.FILES_MUST_NOT_BE_BLANK));
      verify(autowireCapableBeanFactory, AT_LEAST_NONE).getBean(
          request.getBulkProcessType() + "ProcessorService");
    }
  }

  @Test
  public void testUploadForBulkUpdate() throws Exception {
    BulkProcessUpdateRequest bulkProcessUpdateRequest =
        new BulkProcessUpdateRequest(DEFAULT_BULK_PROCESS_TYPE, DEFAULT_BUSINESS_PARTNER_CODE,
            new byte[256], FILE_NAME, PRIVILEGED_MAP, DEFAULT_USERNAME, DEFAULT_CLIENT_ID);
    BulkUpdateService bulkUpdateService = Mockito.mock(BulkUpdateService.class);
    GdnBaseRestResponse response = new GdnBaseRestResponse("", "", true, getRequestId());
    when((BulkUpdateService) autowireCapableBeanFactory.getBean(DEFAULT_BEAN_BULK_UPDATE))
        .thenReturn(bulkUpdateService);
    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD_BULK_UPDATE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();
    MvcResult result = getMockMvc().perform(
        post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(bulkProcessUpdateRequest)))
        .andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
        result.getResponse().getContentAsString());
    verify(autowireCapableBeanFactory, AT_LEAST_ONE)
        .getBean(bulkProcessUpdateRequest.getBulkProcessType() + "BulkUpdateService");
  }

  @Test
  public void testUploadForBulkBasicUpdate() throws Exception {
    BulkBasicInfoRequest bulkProcessUpdateRequest =
        new BulkBasicInfoRequest(DEFAULT_BULK_PROCESS_TYPE, DEFAULT_BUSINESS_PARTNER_CODE,
            DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_USERNAME, false, false, DEFAULT_USERNAME, DEFAULT_STORE_ID,
            FILE_NAME, false, false);
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, getRequestId());
    URI uri =
        new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD_BASIC_INFO_BULK_UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
            .addParameter("username", DEFAULT_USERNAME).build();
    MvcResult result = getMockMvc().perform(
            post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(getObjectMapper().writeValueAsString(bulkProcessUpdateRequest))).andExpect(status().isOk())
        .andReturn();
    verify(bulkBasicInfoUpdateService).preProcessBulkBasicInfoUpdate(DEFAULT_STORE_ID, getRequestId(),
        bulkProcessUpdateRequest);
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response), result.getResponse().getContentAsString());
  }

  @Test
  public void testuploadForBulkAddCampaignProduct() throws Exception {
    BulkProcessAddCampaignProductRequest bulkProcessUpdateRequest =
        new BulkProcessAddCampaignProductRequest();
    BulkAddCampaignProductDTO bulkAddCampaignProductDTO = new BulkAddCampaignProductDTO();
    bulkProcessUpdateRequest.setBulkProcessType(DEFAULT_BULK_PROCESS_TYPE);
    bulkProcessUpdateRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkProcessUpdateRequest.setFileContent(new byte[256]);
    bulkProcessUpdateRequest.setFileName(FILE_NAME);
    bulkProcessUpdateRequest.setPrivilegedMap(PRIVILEGED_MAP);
    bulkProcessUpdateRequest.setUpdatedBy(DEFAULT_USERNAME);
    bulkProcessUpdateRequest.setClientHost(DEFAULT_CLIENT_ID);
    bulkProcessUpdateRequest.setMinQuota(5);
    BeanUtils.copyProperties(bulkProcessUpdateRequest, bulkAddCampaignProductDTO);
    BulkUpdateService bulkUpdateService = Mockito.mock(BulkUpdateService.class);
    GdnBaseRestResponse response = new GdnBaseRestResponse("", "", true, getRequestId());
    when((BulkUpdateService) autowireCapableBeanFactory.getBean(DEFAULT_BEAN_BULK_UPDATE))
        .thenReturn(bulkUpdateService);
    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD_BULK_ADD_CAMPAIGN_PRODUCT)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();
    MvcResult result = getMockMvc().perform(
        post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(bulkProcessUpdateRequest)))
        .andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
        result.getResponse().getContentAsString());
    verify(autowireCapableBeanFactory, AT_LEAST_ONE)
        .getBean(bulkProcessUpdateRequest.getBulkProcessType() + "BulkUpdateService");
    verify(bulkUpdateService)
        .preProcessCampaignProductBulkUpdate(DEFAULT_STORE_ID, getRequestId(), bulkAddCampaignProductDTO);
  }


  @Test
  public void testSubjectToVat() throws Exception {
    BulkProcessSubjectToVatRequest bulkProcessSubjectToVatRequest =
        new BulkProcessSubjectToVatRequest();
    bulkProcessSubjectToVatRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkProcessSubjectToVatRequest.setFileName(FILE_NAME);
    bulkProcessSubjectToVatRequest.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    GdnBaseRestResponse response = new GdnBaseRestResponse("", "", true, getRequestId());
    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD_SUBJECT_TO_VAT)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();
    MvcResult result = getMockMvc().perform(
        post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(bulkProcessSubjectToVatRequest)))
        .andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
        result.getResponse().getContentAsString());
    verify(bulkProcessService).preProcessSubjectToVatUploadEvent(DEFAULT_STORE_ID, getRequestId(), DEFAULT_USERNAME,
        DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BULK_PROCESS_CODE, FILE_NAME);
  }

  @Test
  public void testSubjectToVatARETest() throws Exception {
    BulkProcessSubjectToVatRequest bulkProcessSubjectToVatRequest =
        new BulkProcessSubjectToVatRequest();
    bulkProcessSubjectToVatRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkProcessSubjectToVatRequest.setFileName(null);
    bulkProcessSubjectToVatRequest.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD_SUBJECT_TO_VAT)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(
        post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(bulkProcessSubjectToVatRequest)))
        .andExpect(status().isOk()).andReturn();
  }

  @Test
  public void testSubjectToVatAETest() throws Exception {
    BulkProcessSubjectToVatRequest bulkProcessSubjectToVatRequest =
        new BulkProcessSubjectToVatRequest();
    bulkProcessSubjectToVatRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkProcessSubjectToVatRequest.setFileName(FILE_NAME);
    bulkProcessSubjectToVatRequest.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    GdnBaseRestResponse response = new GdnBaseRestResponse("", "", true, getRequestId());
    doThrow(ArithmeticException.class).when(bulkProcessService)
        .preProcessSubjectToVatUploadEvent(DEFAULT_STORE_ID, getRequestId(), DEFAULT_USERNAME,
            DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BULK_PROCESS_CODE, FILE_NAME);

    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD_SUBJECT_TO_VAT)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(
        post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(bulkProcessSubjectToVatRequest)))
        .andExpect(status().isOk()).andReturn();
    verify(bulkProcessService).preProcessSubjectToVatUploadEvent(DEFAULT_STORE_ID, getRequestId(), DEFAULT_USERNAME,
        DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_BULK_PROCESS_CODE, FILE_NAME);
  }
  
  @Test
  public void testUploadForBulkUpdateWhenError() throws Exception {
    BulkProcessUpdateRequest bulkProcessUpdateRequest =
        new BulkProcessUpdateRequest(DEFAULT_BULK_PROCESS_TYPE, DEFAULT_BUSINESS_PARTNER_CODE,
            new byte[256], FILE_NAME, PRIVILEGED_MAP, DEFAULT_USERNAME, DEFAULT_CLIENT_ID);

    when((BulkUpdateService) autowireCapableBeanFactory.getBean(DEFAULT_BEAN_BULK_UPDATE))
        .thenThrow(RuntimeException.class);
    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD_BULK_UPDATE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();
    
    MvcResult result = getMockMvc().perform(
        post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(bulkProcessUpdateRequest)))
        .andExpect(status().isOk()).andReturn();
    verify(autowireCapableBeanFactory, AT_LEAST_ONE)
        .getBean(bulkProcessUpdateRequest.getBulkProcessType() + "BulkUpdateService");
    verify(trackerService)
        .sendTracker(eq(PRODUCT_UPDATE_EVENT), eq(PRODUCT_UPDATE_ATTRI_TYPE),
            eq(HYPHEN), eq(TrackerConstants.FAILED), anyString());
  }

  @Test
  public void testuploadForBulkAddCampaignProductWhenError() throws Exception {
    BulkProcessAddCampaignProductRequest bulkProcessUpdateRequest =
        new BulkProcessAddCampaignProductRequest();
    bulkProcessUpdateRequest.setBulkProcessType(DEFAULT_BULK_PROCESS_TYPE);
    bulkProcessUpdateRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkProcessUpdateRequest.setFileContent(new byte[256]);
    bulkProcessUpdateRequest.setFileName(FILE_NAME);
    bulkProcessUpdateRequest.setPrivilegedMap(PRIVILEGED_MAP);
    bulkProcessUpdateRequest.setUpdatedBy(DEFAULT_USERNAME);
    bulkProcessUpdateRequest.setClientHost(DEFAULT_CLIENT_ID);
    when((BulkUpdateService) autowireCapableBeanFactory.getBean(DEFAULT_BEAN_BULK_UPDATE))
        .thenThrow(RuntimeException.class);
    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD_BULK_ADD_CAMPAIGN_PRODUCT)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();

    MvcResult result = getMockMvc().perform(
        post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(bulkProcessUpdateRequest)))
        .andExpect(status().isOk()).andReturn();
    verify(autowireCapableBeanFactory, AT_LEAST_ONE)
        .getBean(bulkProcessUpdateRequest.getBulkProcessType() + "BulkUpdateService");
  }

  @Test
  public void testUploadForBulkUpdate_BulkProcessTypeBlank() throws Exception {
    BulkProcessUpdateRequest bulkProcessUpdateRequest =
        new BulkProcessUpdateRequest(StringUtils.EMPTY, DEFAULT_BUSINESS_PARTNER_CODE,
            new byte[256], FILE_NAME, PRIVILEGED_MAP, DEFAULT_USERNAME, DEFAULT_CLIENT_ID);
    BulkUpdateService bulkUpdateService = Mockito.mock(BulkUpdateService.class);
    GdnBaseRestResponse response = new GdnBaseRestResponse(ErrorCategory.VALIDATION.getMessage()
        + GenericErrorMessages.BULK_PROCESS_TYPE_MUST_NOT_BE_BLANK,
        ErrorCategory.VALIDATION.toString(), false, getRequestId());
    when((BulkUpdateService) autowireCapableBeanFactory.getBean(DEFAULT_BEAN_BULK_UPDATE))
        .thenReturn(bulkUpdateService);
    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD_BULK_UPDATE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();
    MvcResult result = getMockMvc().perform(
        post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(bulkProcessUpdateRequest)))
        .andExpect(status().isOk()).andReturn();
    verify(trackerService)
        .sendTracker(eq(PRODUCT_UPDATE_EVENT), eq(PRODUCT_UPDATE_ATTRI_TYPE),
            eq(HYPHEN), eq(TrackerConstants.FAILED), Mockito.anyString());
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
        result.getResponse().getContentAsString());
  }

  @Test
  public void testuploadForBulkAddCampaignProductProcessTypeBlank() throws Exception {
    BulkProcessAddCampaignProductRequest bulkProcessUpdateRequest =
        new BulkProcessAddCampaignProductRequest();
    bulkProcessUpdateRequest.setBulkProcessType(StringUtils.EMPTY);
    bulkProcessUpdateRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkProcessUpdateRequest.setFileContent(new byte[256]);
    bulkProcessUpdateRequest.setFileName(FILE_NAME);
    bulkProcessUpdateRequest.setPrivilegedMap(PRIVILEGED_MAP);
    bulkProcessUpdateRequest.setUpdatedBy(DEFAULT_USERNAME);
    bulkProcessUpdateRequest.setClientHost(DEFAULT_CLIENT_ID);
    BulkUpdateService bulkUpdateService = Mockito.mock(BulkUpdateService.class);
    GdnBaseRestResponse response = new GdnBaseRestResponse(ErrorCategory.VALIDATION.getMessage()
        + GenericErrorMessages.BULK_PROCESS_TYPE_MUST_NOT_BE_BLANK,
        ErrorCategory.VALIDATION.toString(), false, getRequestId());
    when((BulkUpdateService) autowireCapableBeanFactory.getBean(DEFAULT_BEAN_BULK_UPDATE))
        .thenReturn(bulkUpdateService);
    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD_BULK_ADD_CAMPAIGN_PRODUCT)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();
    MvcResult result = getMockMvc().perform(
        post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(bulkProcessUpdateRequest)))
        .andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
        result.getResponse().getContentAsString());
  }

  @Test
  public void testUploadForBulkUpdate_FileNameEmpty() throws Exception {
    BulkProcessUpdateRequest bulkProcessUpdateRequest =
        new BulkProcessUpdateRequest(DEFAULT_BULK_PROCESS_TYPE, DEFAULT_BUSINESS_PARTNER_CODE,
            new byte[256], StringUtils.EMPTY, PRIVILEGED_MAP, DEFAULT_USERNAME, DEFAULT_CLIENT_ID);
    BulkUpdateService bulkUpdateService = Mockito.mock(BulkUpdateService.class);
    GdnBaseRestResponse response = new GdnBaseRestResponse(
        ErrorCategory.VALIDATION.getMessage() + GenericErrorMessages.FILE_NAME_MUST_NOT_BE_BLANK,
        ErrorCategory.VALIDATION.toString(), false, getRequestId());
    when((BulkUpdateService) autowireCapableBeanFactory.getBean(DEFAULT_BEAN_BULK_UPDATE))
        .thenReturn(bulkUpdateService);
    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD_BULK_UPDATE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();
    MvcResult result = getMockMvc().perform(
        post(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(bulkProcessUpdateRequest)))
        .andExpect(status().isOk()).andReturn();
    verify(trackerService)
        .sendTracker(eq(PRODUCT_UPDATE_EVENT), eq(PRODUCT_UPDATE_ATTRI_TYPE),
            eq(HYPHEN), eq(TrackerConstants.FAILED), Mockito.anyString());
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
        result.getResponse().getContentAsString());
  }

  @Test
  public void testUploadForBulkBasicUpdate_Exception() throws Exception {
    BulkBasicInfoRequest bulkProcessUpdateRequest =
        new BulkBasicInfoRequest(DEFAULT_BULK_PROCESS_TYPE, DEFAULT_BUSINESS_PARTNER_CODE,
            DEFAULT_BUSINESS_PARTNER_CODE, DEFAULT_USERNAME, false, false, DEFAULT_USERNAME, DEFAULT_STORE_ID,
            FILE_NAME, false, false);
    GdnBaseRestResponse response = new GdnBaseRestResponse(
        ErrorCategory.VALIDATION.getMessage() + GenericErrorMessages.FILE_NAME_MUST_NOT_BE_BLANK,
        ErrorCategory.VALIDATION.toString(), false, getRequestId());
    doThrow(ApplicationRuntimeException.class).when(bulkBasicInfoUpdateService)
        .preProcessBulkBasicInfoUpdate(DEFAULT_STORE_ID, getRequestId(), bulkProcessUpdateRequest);
    URI uri =
        new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD_BASIC_INFO_BULK_UPDATE)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
            .addParameter("username", DEFAULT_USERNAME).build();
    MvcResult result = getMockMvc().perform(
            post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(getObjectMapper().writeValueAsString(bulkProcessUpdateRequest))).andExpect(status().isOk())
        .andReturn();
    verify(bulkBasicInfoUpdateService).preProcessBulkBasicInfoUpdate(DEFAULT_STORE_ID, getRequestId(),
        bulkProcessUpdateRequest);
  }
  
  @Test
  public void deleteOldBulkProcessRecordsTest() throws Exception {
    doNothing().when(bulkProcessService).updateBulkProcessRecordAsMarkForDeleteTrue("YYYY/MM/DD");
    
    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH + BulkProcessController.DELETE_BULK_PROCESS_RECORDS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).addParameter("date", "YYYY/MM/DD").build();
    MvcResult result = getMockMvc().perform(
        MockMvcRequestBuilders.delete(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk()).andReturn();
    
    verify(bulkProcessService).updateBulkProcessRecordAsMarkForDeleteTrue("YYYY/MM/DD");
  }
  
  @Test
  public void deleteOldBulkProcessRecordsWhenError() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(bulkProcessService).updateBulkProcessRecordAsMarkForDeleteTrue("YYYY/MM/DD");

    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH + BulkProcessController.DELETE_BULK_PROCESS_RECORDS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).addParameter("date", "YYYY/MM/DD").build();

    try{
      MvcResult result = getMockMvc().perform(
          MockMvcRequestBuilders.delete(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk()).andReturn();
    } catch(Exception e){
      verify(bulkProcessService).updateBulkProcessRecordAsMarkForDeleteTrue("YYYY/MM/DD");
    }
  }

  @Test
  public void uploadBulkRecatTest() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    files.put("xls", Base64.encodeBase64String(new byte[256]));
    BulkProcessUploadRequest request =
        new BulkProcessUploadRequest(BulkProcessType.RECATEGORIZATION.getValue(), DEFAULT_BUSINESS_PARTNER_CODE,
            files, new HashMap<String, String>());
    ProcessorService processorService = Mockito.mock(ProcessorService.class);
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, getRequestId());
    when(autowireCapableBeanFactory.getBean(request.getBulkProcessType() + "ProcessorService"))
        .thenReturn(processorService);
    URI uri =
        new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD_BULK_RECAT)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
            .addParameter("username", DEFAULT_USERNAME).build();
    MvcResult result =
        getMockMvc()
            .perform(
                post(uri).accept(MediaType.APPLICATION_JSON)
                    .contentType(MediaType.APPLICATION_JSON)
                    .content(getObjectMapper().writeValueAsString(request)))
            .andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
        result.getResponse().getContentAsString());
    verify(autowireCapableBeanFactory, AT_LEAST_ONE).getBean(
        request.getBulkProcessType() + "ProcessorService");
  }

  @Test
  public void uploadBulkRecatTest_WhenRequiredParamsEmpty() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    BulkProcessUploadRequest request =
        new BulkProcessUploadRequest(BulkProcessType.RECATEGORIZATION.getValue(), DEFAULT_BUSINESS_PARTNER_CODE,
            files, new HashMap<String, String>());
    ProcessorService processorService = Mockito.mock(ProcessorService.class);
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, getRequestId());
    URI uri =
        new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD_BULK_RECAT)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
            .addParameter("username", DEFAULT_USERNAME).build();
    Assertions.assertThrows(Exception.class, () ->
        getMockMvc()
            .perform(
                post(uri).accept(MediaType.APPLICATION_JSON)
                    .contentType(MediaType.APPLICATION_JSON)
                    .content(getObjectMapper().writeValueAsString(request)))
            .andExpect(status().isOk()).andReturn());
  }

  @Test
  public void uploadBulkRecatTest_WhenInvalidBulkProcessType() throws Exception {
    Map<String, String> files = new HashMap<String, String>();
    files.put("xls", Base64.encodeBase64String(new byte[256]));
    BulkProcessUploadRequest request =
        new BulkProcessUploadRequest(null, DEFAULT_BUSINESS_PARTNER_CODE, files,
            new HashMap<String, String>());
    request.setBulkProcessType("invalid");
    when((ProcessorService) autowireCapableBeanFactory.getBean(DEFAULT_BEAN_BULK_UPDATE))
        .thenThrow(ApplicationRuntimeException.class);
    URI uri =
        new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD_BULK_RECAT)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
            .addParameter("username", DEFAULT_USERNAME).build();
    try {
      getMockMvc().perform(
          post(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)
              .content(getObjectMapper().writeValueAsString(request)))
          .andExpect(status().isOk());
    } catch (Exception e) {
      verify(autowireCapableBeanFactory).getBean(
          request.getBulkProcessType() + "ProcessorService");
    }
  }

  @Test
  public void scheduledDeletionBulkUploadFileTest() throws Exception {
    doNothing().when(this.bulkProcessService).findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    URI uri =
        new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.SCHEDULE_EXCEL_FILE_DELETE)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
            .addParameter("username", DEFAULT_USERNAME).build();
      getMockMvc().perform(
          MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    verify(this.bulkProcessService).findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
  }

  @Test
  public void scheduledDeletionBulkUploadFile_expectException() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(this.bulkProcessService)
        .findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    try {
      URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.SCHEDULE_EXCEL_FILE_DELETE)
          .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
          .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
          .addParameter("username", DEFAULT_USERNAME).build();
      getMockMvc().perform(MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    } catch (ApplicationRuntimeException e) {
    }
    finally {
      verify(this.bulkProcessService).findAndDeleteBulkProcessCodesBeforeXDays(DEFAULT_STORE_ID);
    }
  }

  @Test
  public void regenerateBrandValuesInGenericBulkTemplateTest() throws Exception {
    doNothing().when(this.bulkProcessService).regenerateBrandValuesInGenericBulkTemplate(FILE_TYPE);
    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH + BulkProcessController.REGENERATE_BRAND_VALUES_IN_GENERIC_TEMPLATE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
            .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
    verify(this.bulkProcessService)
        .regenerateBrandValuesInGenericBulkTemplate(GenericTemplateFileType.DEFAULT_FILE.name());
  }

  @Test
  public void bulkArchiveItemTest() throws Exception {
    BulkProcessUpdateRequest request =
        new BulkProcessUpdateRequest(DEFAULT_BULK_PROCESS_TYPE, DEFAULT_BUSINESS_PARTNER_CODE,
            new byte[256], FILE_NAME, PRIVILEGED_MAP, DEFAULT_USERNAME, DEFAULT_CLIENT_ID);
    when(autowireCapableBeanFactory.getBean(anyString())).thenReturn(this.bulkUpdateService);
    doNothing().when(this.bulkUpdateService).preProcessBulkArchiveItems(anyString(), anyString(), any());
    String requestId = getRequestId();
    URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.BULK_ARCHIVE_ITEM)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", requestId)
        .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(request))).andExpect(status().isOk());
    Mockito.verify(bulkUpdateService).preProcessBulkArchiveItems(eq(DEFAULT_STORE_ID), eq(requestId),
        bulkUpdateProcessDTOArgumentCaptor.capture());
    Mockito.verify(autowireCapableBeanFactory).getBean(eq(DEFAULT_BULK_PROCESS_TYPE + BulkProcessController.SERVICE_BEAN_NAME));
    BulkUpdateProcessDTO bulkUpdateProcessDTO = bulkUpdateProcessDTOArgumentCaptor.getValue();
    Assertions.assertEquals(DEFAULT_BULK_PROCESS_TYPE, bulkUpdateProcessDTO.getBulkProcessType());
  }

  @Test
  public void bulkArchiveItemsTest_ExpectException() throws Exception {
    BulkProcessUpdateRequest request = new BulkProcessUpdateRequest(DEFAULT_BULK_PROCESS_TYPE,
        DEFAULT_BUSINESS_PARTNER_CODE, new byte[256], FILE_NAME, PRIVILEGED_MAP,
        DEFAULT_USERNAME, DEFAULT_CLIENT_ID);
    when(autowireCapableBeanFactory.getBean(anyString())).thenReturn(this.bulkUpdateService);
    doThrow(ApplicationRuntimeException.class).when(this.bulkUpdateService)
        .preProcessBulkArchiveItems(anyString(), anyString(), any());
    String requestId = getRequestId();
    URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.BULK_ARCHIVE_ITEM)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", requestId)
        .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(request))).andExpect(status().isOk()).andExpect(jsonPath("$.success").value(false));
    verify(bulkUpdateService)
        .preProcessBulkArchiveItems(eq(DEFAULT_STORE_ID),
            eq(requestId),
            bulkUpdateProcessDTOArgumentCaptor.capture());
    verify(autowireCapableBeanFactory).getBean(eq(DEFAULT_BULK_PROCESS_TYPE
        + BulkProcessController.SERVICE_BEAN_NAME));
    BulkUpdateProcessDTO bulkUpdateProcessDTO = bulkUpdateProcessDTOArgumentCaptor.getValue();
    Assertions.assertEquals(DEFAULT_BULK_PROCESS_TYPE, bulkUpdateProcessDTO.getBulkProcessType());
  }

  @Test
  public void bulkArchiveProductTest() throws Exception {
    BulkProcessUpdateRequest request =
        new BulkProcessUpdateRequest(DEFAULT_BULK_PROCESS_TYPE, DEFAULT_BUSINESS_PARTNER_CODE,
            new byte[256], FILE_NAME, PRIVILEGED_MAP, DEFAULT_USERNAME, DEFAULT_CLIENT_ID);
    when(autowireCapableBeanFactory.getBean(anyString())).thenReturn(this.bulkUpdateService);
    doNothing().when(this.bulkUpdateService).preProcessBulkArchiveProducts(anyString(), anyString(), any());
    String requestId = getRequestId();
    URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.BULK_ARCHIVE_PRODUCTS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", requestId)
        .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(request))).andExpect(status().isOk());
    Mockito.verify(bulkUpdateService).preProcessBulkArchiveProducts(eq(DEFAULT_STORE_ID), eq(requestId),
        bulkUpdateProcessDTOArgumentCaptor.capture());
    Mockito.verify(autowireCapableBeanFactory).getBean(eq(DEFAULT_BULK_PROCESS_TYPE + BulkProcessController.SERVICE_BEAN_NAME));
    BulkUpdateProcessDTO bulkUpdateProcessDTO = bulkUpdateProcessDTOArgumentCaptor.getValue();
    Assertions.assertEquals(DEFAULT_BULK_PROCESS_TYPE, bulkUpdateProcessDTO.getBulkProcessType());
  }

  @Test
  public void bulkArchiveProcuctExceptionTest() throws Exception {
    BulkProcessUpdateRequest request = new BulkProcessUpdateRequest(DEFAULT_BULK_PROCESS_TYPE,
        DEFAULT_BUSINESS_PARTNER_CODE, new byte[256], FILE_NAME, PRIVILEGED_MAP,
        DEFAULT_USERNAME, DEFAULT_CLIENT_ID);
    when(autowireCapableBeanFactory.getBean(anyString())).thenReturn(this.bulkUpdateService);
    doThrow(ApplicationRuntimeException.class).when(this.bulkUpdateService)
        .preProcessBulkArchiveProducts(anyString(), anyString(), any());
    String requestId = getRequestId();
    URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.BULK_ARCHIVE_PRODUCTS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", requestId)
        .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(request))).andExpect(status().isOk()).andExpect(jsonPath("$.success").value(false));
    verify(bulkUpdateService)
        .preProcessBulkArchiveProducts(eq(DEFAULT_STORE_ID),
            eq(requestId),
            bulkUpdateProcessDTOArgumentCaptor.capture());
    verify(autowireCapableBeanFactory).getBean(eq(DEFAULT_BULK_PROCESS_TYPE
        + BulkProcessController.SERVICE_BEAN_NAME));
    BulkUpdateProcessDTO bulkUpdateProcessDTO = bulkUpdateProcessDTOArgumentCaptor.getValue();
    Assertions.assertEquals(DEFAULT_BULK_PROCESS_TYPE, bulkUpdateProcessDTO.getBulkProcessType());
  }

  @Test
  public void BulkArchiveProductsException1Test() throws Exception {
    BulkProcessUpdateRequest request =
        new BulkProcessUpdateRequest(null, DEFAULT_BUSINESS_PARTNER_CODE, new byte[256], FILE_NAME,
            PRIVILEGED_MAP, DEFAULT_USERNAME, DEFAULT_CLIENT_ID);
    String requestId = getRequestId();
    URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.BULK_ARCHIVE_PRODUCTS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", requestId)
        .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(request))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(false));
  }

  @Test
  public void bulkUpdateOff2OnTest() throws Exception {
    BulkProcessUpdateRequest request =
        new BulkProcessUpdateRequest(INSTORE_BULK_PROCESS_TYPE, DEFAULT_BUSINESS_PARTNER_CODE, new byte[256], FILE_NAME,
            PRIVILEGED_MAP, DEFAULT_USERNAME, DEFAULT_CLIENT_ID);
    when(autowireCapableBeanFactory.getBean(anyString())).thenReturn(this.bulkUpdateService);
    doNothing().when(this.bulkUpdateService).preProcessBulkUpdateOff2On(anyString(), anyString(), any());
    String requestId = getRequestId();
    URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.BULK_UPDATE_OFF2ON)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", requestId)
        .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(request))).andExpect(status().isOk());
    Mockito.verify(bulkUpdateService)
        .preProcessBulkUpdateOff2On(eq(DEFAULT_STORE_ID), eq(requestId), bulkUpdateProcessDTOArgumentCaptor.capture());
    Mockito.verify(autowireCapableBeanFactory)
        .getBean(eq(DEFAULT_BULK_PROCESS_TYPE + BulkProcessController.SERVICE_BEAN_NAME));
    BulkUpdateProcessDTO bulkUpdateProcessDTO = bulkUpdateProcessDTOArgumentCaptor.getValue();
    Assertions.assertEquals(INSTORE_BULK_PROCESS_TYPE, bulkUpdateProcessDTO.getBulkProcessType());
  }

  @Test
  public void bulkUpdateOff2OnExceptionTest() throws Exception {
    BulkProcessUpdateRequest request =
        new BulkProcessUpdateRequest(INSTORE_BULK_PROCESS_TYPE, DEFAULT_BUSINESS_PARTNER_CODE, new byte[256], FILE_NAME,
            PRIVILEGED_MAP, DEFAULT_USERNAME, DEFAULT_CLIENT_ID);
    when(autowireCapableBeanFactory.getBean(anyString())).thenReturn(this.bulkUpdateService);
    doThrow(ApplicationRuntimeException.class).when(this.bulkUpdateService).preProcessBulkUpdateOff2On(anyString(), anyString(), any());
    String requestId = getRequestId();
    URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.BULK_UPDATE_OFF2ON)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", requestId)
        .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(request))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(false));
    verify(bulkUpdateService)
        .preProcessBulkUpdateOff2On(eq(DEFAULT_STORE_ID), eq(requestId), bulkUpdateProcessDTOArgumentCaptor.capture());
    verify(autowireCapableBeanFactory).getBean(eq(DEFAULT_BULK_PROCESS_TYPE + BulkProcessController.SERVICE_BEAN_NAME));
    BulkUpdateProcessDTO bulkUpdateProcessDTO = bulkUpdateProcessDTOArgumentCaptor.getValue();
    Assertions.assertEquals(INSTORE_BULK_PROCESS_TYPE, bulkUpdateProcessDTO.getBulkProcessType());
  }

  @Test
  public void bulkUpdateOff2OnException1Test() throws Exception {
    BulkProcessUpdateRequest request =
        new BulkProcessUpdateRequest(null, DEFAULT_BUSINESS_PARTNER_CODE, new byte[256], FILE_NAME,
            PRIVILEGED_MAP, DEFAULT_USERNAME, DEFAULT_CLIENT_ID);
    String requestId = getRequestId();
    URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.BULK_UPDATE_OFF2ON)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", requestId)
        .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(request))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(false));
  }

  @Test
  public void abortPendingBulkProcessTest() throws Exception {
    ArgumentCaptor<Date> dateArgumentCaptor = ArgumentCaptor.forClass(Date.class);
    doNothing().when(bulkProcessService)
      .abortPendingBulkProcessBefore(eq(DEFAULT_STORE_ID));
    doNothing().when(bulkProcessDataService)
        .abortPendingBulkProcessDataBeforeOrById(eq(DEFAULT_STORE_ID), eq(StringUtils.EMPTY));
    URI uri = new URIBuilder().setPath(
        BulkProcessController.BASE_PATH + BulkProcessController.ABORT_PENDING_TASK)
        .addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("requestId", requestId)
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("id", StringUtils.EMPTY)
        .build();
    Calendar instance1 = Calendar.getInstance();
    instance1.add(Calendar.SECOND, -DEFAULT_ABORT_TIME);
    getMockMvc().perform(put(uri).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(true));
    verify(bulkProcessDataService).abortPendingBulkProcessDataBeforeOrById(eq(DEFAULT_STORE_ID),
      eq(StringUtils.EMPTY));
    verify(bulkProcessService).abortPendingBulkProcessBefore(eq(DEFAULT_STORE_ID));
  }

  @Test
  public void abortPendingBulkProcessTest_Exception() throws Exception {
    doThrow(ApplicationRuntimeException.class).when(bulkProcessDataService)
        .abortPendingBulkProcessDataBeforeOrById(eq(DEFAULT_STORE_ID), eq(null));
    URI uri = new URIBuilder().setPath(
        BulkProcessController.BASE_PATH + BulkProcessController.ABORT_PENDING_TASK)
        .addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("requestId", requestId)
        .addParameter("username", DEFAULT_USERNAME)
        .build();
    getMockMvc().perform(put(uri).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(false));
    verify(bulkProcessService).abortPendingBulkProcessBefore(eq(DEFAULT_STORE_ID));
    verify(bulkProcessDataService).abortPendingBulkProcessDataBeforeOrById(eq(DEFAULT_STORE_ID),
      eq(null));
  }

  @Test
  public void testUploadForBulkUpsertInstantPickupProduct() throws Exception {
    BulkProcessUpdateRequest bulkProcessUpdateRequest =
      new BulkProcessUpdateRequest("InstantPickupProduct", DEFAULT_BUSINESS_PARTNER_CODE,
        new byte[256], FILE_NAME, PRIVILEGED_MAP, DEFAULT_USERNAME, DEFAULT_CLIENT_ID);

    BulkUpsertService bulkUpsertService = Mockito.mock(BulkUpsertService.class);

    GdnBaseRestResponse response = new GdnBaseRestResponse("", "", true, getRequestId());

    when((BulkUpsertService) autowireCapableBeanFactory
        .getBean("InstantPickupProductBulkUpsertService")).thenReturn(bulkUpsertService);

    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH
            + BulkProcessController.UPLOAD_BULK_UPSERT_OFFLINE_ITEMS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();

    MvcResult result = getMockMvc()
        .perform(
            post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(getObjectMapper().writeValueAsString(bulkProcessUpdateRequest)))
        .andExpect(status().isOk()).andReturn();

    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
        result.getResponse().getContentAsString());

    verify(autowireCapableBeanFactory, AT_LEAST_ONE)
        .getBean(bulkProcessUpdateRequest.getBulkProcessType() + "BulkUpsertService");
  }

  @Test
  public void testUploadForBulkUpsertInstantPickupProduct_throwsException() throws Exception {
    BulkProcessUpdateRequest bulkProcessUpdateRequest =
        new BulkProcessUpdateRequest("InstantPickupProduct", DEFAULT_BUSINESS_PARTNER_CODE,
            new byte[256], FILE_NAME, PRIVILEGED_MAP, DEFAULT_USERNAME, DEFAULT_CLIENT_ID);

    when((BulkUpdateService) autowireCapableBeanFactory
        .getBean("InstantPickupProductBulkUpsertService")).thenThrow(RuntimeException.class);

    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH
            + BulkProcessController.UPLOAD_BULK_UPSERT_OFFLINE_ITEMS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();

    getMockMvc()
        .perform(
            post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(getObjectMapper().writeValueAsString(bulkProcessUpdateRequest)))
        .andExpect(status().isOk()).andReturn();

    verify(autowireCapableBeanFactory, AT_LEAST_ONE)
        .getBean(bulkProcessUpdateRequest.getBulkProcessType() + "BulkUpsertService");
    verify(trackerService).sendTracker(eq(INSTANT_PICKUP_UPDATE),
        eq(INSTANT_PICKUP_UPDATE_TYPE), eq(HYPHEN), eq(TrackerConstants.FAILED),
        anyString());
  }

  @Test
  public void testUploadForBulkUpsertInstantPickupProduct_bulkProcessTypeBlank_throwsApplicationRuntimeException()
      throws Exception {
    BulkProcessUpdateRequest bulkProcessUpdateRequest =
        new BulkProcessUpdateRequest("", DEFAULT_BUSINESS_PARTNER_CODE, new byte[256], FILE_NAME,
            PRIVILEGED_MAP, DEFAULT_USERNAME, DEFAULT_CLIENT_ID);

    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH
            + BulkProcessController.UPLOAD_BULK_UPSERT_OFFLINE_ITEMS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();

    getMockMvc()
        .perform(
            post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(getObjectMapper().writeValueAsString(bulkProcessUpdateRequest)))
        .andExpect(status().isOk()).andReturn();

    verify(trackerService).sendTracker(eq(INSTANT_PICKUP_UPDATE),
        eq(INSTANT_PICKUP_UPDATE_TYPE), eq(HYPHEN), eq(TrackerConstants.FAILED),
        anyString());
  }

  @Test
  public void testUploadForBulkDeleteOfflineItems() throws Exception {
    BulkProcessUpdateRequest bulkProcessUpdateRequest =
        new BulkProcessUpdateRequest("InstantPickupProduct", DEFAULT_BUSINESS_PARTNER_CODE,
            new byte[256], FILE_NAME, PRIVILEGED_MAP, DEFAULT_USERNAME, DEFAULT_CLIENT_ID);

    BulkDeleteService bulkDeleteService = Mockito.mock(BulkDeleteService.class);

    GdnBaseRestResponse response = new GdnBaseRestResponse("", "", true, getRequestId());

    when((BulkDeleteService) autowireCapableBeanFactory
        .getBean("InstantPickupProductBulkDeleteService")).thenReturn(bulkDeleteService);

    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH
            + BulkProcessController.UPLOAD_BULK_DELETE_OFFLINE_ITEMS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();

    MvcResult result = getMockMvc()
        .perform(
            post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(getObjectMapper().writeValueAsString(bulkProcessUpdateRequest)))
        .andExpect(status().isOk()).andReturn();

    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
        result.getResponse().getContentAsString());

    verify(autowireCapableBeanFactory, AT_LEAST_ONE)
        .getBean(bulkProcessUpdateRequest.getBulkProcessType() + "BulkDeleteService");
  }

  @Test
  public void testUploadForBulkDeleteOfflineItems_throwsException() throws Exception {
    BulkProcessUpdateRequest bulkProcessUpdateRequest =
        new BulkProcessUpdateRequest("InstantPickupProduct", DEFAULT_BUSINESS_PARTNER_CODE,
            new byte[256], FILE_NAME, PRIVILEGED_MAP, DEFAULT_USERNAME, DEFAULT_CLIENT_ID);

    when((BulkDeleteService) autowireCapableBeanFactory
        .getBean("InstantPickupProductBulkDeleteService")).thenThrow(RuntimeException.class);

    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH
            + BulkProcessController.UPLOAD_BULK_DELETE_OFFLINE_ITEMS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();

    getMockMvc()
        .perform(
            post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(getObjectMapper().writeValueAsString(bulkProcessUpdateRequest)))
        .andExpect(status().isOk()).andReturn();

    verify(autowireCapableBeanFactory, AT_LEAST_ONE)
        .getBean(bulkProcessUpdateRequest.getBulkProcessType() + "BulkDeleteService");
    verify(trackerService).sendTracker(eq(INSTANT_PICKUP_DEL),
        eq(INSTANT_PICKUP_DEL_TYPE), eq(HYPHEN), eq(TrackerConstants.FAILED),
        anyString());
  }

  @Test
  public void testUploadForBulkDeleteOfflineItems_bulkProcessTypeBlank_throwsApplicationRuntimeException()
      throws Exception {
    BulkProcessUpdateRequest bulkProcessUpdateRequest =
        new BulkProcessUpdateRequest("", DEFAULT_BUSINESS_PARTNER_CODE, new byte[256], FILE_NAME,
            PRIVILEGED_MAP, DEFAULT_USERNAME, DEFAULT_CLIENT_ID);

    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH
            + BulkProcessController.UPLOAD_BULK_DELETE_OFFLINE_ITEMS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();

    getMockMvc()
        .perform(
            post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .content(getObjectMapper().writeValueAsString(bulkProcessUpdateRequest)))
        .andExpect(status().isOk()).andReturn();

    verify(trackerService).sendTracker(eq(INSTANT_PICKUP_DEL),
        eq(INSTANT_PICKUP_DEL_TYPE), eq(HYPHEN), eq(TrackerConstants.FAILED),
        anyString());
  }

  @Test
  public void testCheckForPendingBulkCncProcessByMerchantCode() throws Exception {
    BulkPendingRequestsResponse bulkPendingRequestsResponse = getBulkPendingRequestsResponse();
    GdnRestSingleResponse<BulkPendingRequestsResponse> response =
        new GdnRestSingleResponse<BulkPendingRequestsResponse>(null, null, true,
            bulkPendingRequestsResponse, getRequestId());
    when(bulkProcessService.checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME, Constant.BULK_UPLOAD_TYPE,
        DEFAULT_BUSINESS_PARTNER_CODE, SystemParameterConfigNames.DISABLE_BULK_CNC_UPDATES_VARIABLE,
        Constant.INSTANT_PICKUP_PRODUCT)).thenReturn(bulkPendingRequestsResponse);
    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH
            + BulkProcessController.CHECK_PENDING_BULK_CNC_REQUESTS_BY_BP_CODE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .build();
    MvcResult result =
        getMockMvc().perform(MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
        result.getResponse().getContentAsString());
    verify(bulkProcessService).checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME, Constant.BULK_UPLOAD_TYPE,
        DEFAULT_BUSINESS_PARTNER_CODE, SystemParameterConfigNames.DISABLE_BULK_CNC_UPDATES_VARIABLE,
        Constant.INSTANT_PICKUP_PRODUCT);
  }

  @Test
  public void testCheckForPendingBulkCncProcessByMerchantCode_Failed() throws Exception {
    GdnRestSingleResponse<BulkPendingRequestsResponse> response =
        new GdnRestSingleResponse<>(null, null, false, null, getRequestId());
    when(bulkProcessService.checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME, Constant.BULK_UPLOAD_TYPE,
        DEFAULT_BUSINESS_PARTNER_CODE, SystemParameterConfigNames.DISABLE_BULK_CNC_UPDATES_VARIABLE,
        Constant.INSTANT_PICKUP_PRODUCT)).thenThrow(ApplicationRuntimeException.class);
    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH
            + BulkProcessController.CHECK_PENDING_BULK_CNC_REQUESTS_BY_BP_CODE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE).build();
    MvcResult result =
          getMockMvc().perform(MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
          result.getResponse().getContentAsString());
    verify(bulkProcessService).checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME, Constant.BULK_UPLOAD_TYPE,
        DEFAULT_BUSINESS_PARTNER_CODE, SystemParameterConfigNames.DISABLE_BULK_CNC_UPDATES_VARIABLE,
        Constant.INSTANT_PICKUP_PRODUCT);
  }

  @Test
  public void testCheckForPendingBulkCncProcessByMerchantCode_withInvalidBusinessPartnerCode()
      throws Exception {
    BulkPendingRequestsResponse bulkPendingRequestsResponse = getBulkPendingRequestsResponse();
    GdnRestSingleResponse<BulkPendingRequestsResponse> response =
        new GdnRestSingleResponse<BulkPendingRequestsResponse>(null, null, true,
            bulkPendingRequestsResponse, getRequestId());
    when(bulkProcessService.checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME, Constant.BULK_UPLOAD_TYPE,
        DEFAULT_BUSINESS_PARTNER_CODE, SystemParameterConfigNames.DISABLE_BULK_CNC_UPDATES_VARIABLE,
        Constant.INSTANT_PICKUP_PRODUCT)).thenReturn(bulkPendingRequestsResponse);
    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH
            + BulkProcessController.CHECK_PENDING_BULK_REQUESTS_BY_BP_CODE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("businessPartnerCode", StringUtils.EMPTY)
        .addParameter("bulkProcessType", Constant.INSTANT_PICKUP_PRODUCT).build();
    try {
      MvcResult result =
          getMockMvc().perform(MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
    } catch (Exception e) {
      Assertions.assertTrue(
          e.getMessage().contains(BulkProcessController.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK));
      verify(bulkProcessService, AT_LEAST_NONE).checkForPendingBulkProcess(DEFAULT_STORE_ID, DEFAULT_USERNAME,
          Constant.BULK_UPLOAD_TYPE, DEFAULT_BUSINESS_PARTNER_CODE,
          SystemParameterConfigNames.DISABLE_BULK_CNC_UPDATES_VARIABLE, Constant.INSTANT_PICKUP_PRODUCT);
    }
  }

  @Test
  public void filterPromoBulkProcessNotesByBulkProcessCodeTest() throws Exception {
    when(bulkProcessService.filterPromoBulkProcessNotes(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenReturn(new ArrayList<>());
    URI uri = new URIBuilder().setPath(
        BulkProcessController.BASE_PATH + BulkProcessController.FILTER_BULK_PROCESS_CODE + DEFAULT_BULK_PROCESS_CODE
            + "/promoNotes").addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();
    MvcResult result = getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true))).andReturn();
    verify(bulkProcessService).filterPromoBulkProcessNotes(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
  }

  @Test
  public void filterPromoBulkProcessNotesByBulkProcessCode_expectTest() throws Exception {
    when(bulkProcessService.filterPromoBulkProcessNotes(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenThrow(Exception.class);
    URI uri = new URIBuilder().setPath(
        BulkProcessController.BASE_PATH + BulkProcessController.FILTER_BULK_PROCESS_CODE + DEFAULT_BULK_PROCESS_CODE
            + "/promoNotes").addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();
    MvcResult result = getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", is(false))).andReturn();
    verify(bulkProcessService).filterPromoBulkProcessNotes(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
  }

  @Test
  public void regenerateCategoryAttributeMappingInGenericBulkTemplateTest() throws Exception {
    doNothing().when(this.bulkProcessService).regenerateCategoryAttributeMappingInGenericBulkTemplate(
    );
    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH + BulkProcessController.REGENERATE_CATEGORY_ATTRIBUTE_MAPPING_GENERIC_TEMPLATE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON))
            .andExpect(status().isOk());
    verify(this.bulkProcessService)
        .regenerateCategoryAttributeMappingInGenericBulkTemplate();
  }

  @Test
  public void regenerateCategoryAttributeMappingInGenericBulkTemplateExceptionTest() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(this.bulkProcessService)
        .regenerateCategoryAttributeMappingInGenericBulkTemplate();
    try {
      URI uri = new URIBuilder()
          .setPath(BulkProcessController.BASE_PATH + BulkProcessController.REGENERATE_CATEGORY_ATTRIBUTE_MAPPING_GENERIC_TEMPLATE)
          .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
          .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
          .addParameter("username", DEFAULT_USERNAME).build();
      getMockMvc().perform(
          MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON)
              .contentType(MediaType.APPLICATION_JSON))
              .andExpect(status().isOk());
    } catch (ApplicationRuntimeException e) {}
    finally {
      verify(this.bulkProcessService)
          .regenerateCategoryAttributeMappingInGenericBulkTemplate();
    }
  }

  @Test
  public void regenerateCategoryAttributeInGenericBulkTemplateByFileType() throws Exception {
    doNothing().when(this.bulkProcessService)
        .regenerateTemplateByFileType(DEFAULT_STORE_ID, GenericTemplateFileType.PURE_DELIVERY_FILE, getRequestId());
    URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH
            + BulkProcessController.REGENERATE_CATEGORY_ATTRIBUTE_MAPPING_GENERIC_TEMPLATE_BY_FILE_TYPE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("fileType", GenericTemplateFileType.PURE_DELIVERY_FILE.name()).build();
    getMockMvc().perform(
            MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
    verify(this.bulkProcessService).regenerateTemplateByFileType(DEFAULT_STORE_ID,
        GenericTemplateFileType.PURE_DELIVERY_FILE, getRequestId());
  }

  @Test
  public void regenerateCategoryAttributeInGenericBulkTemplateByFileGenericExceptionType() throws Exception {
    Mockito.doThrow(new IllegalArgumentException()).when(this.bulkProcessService)
        .regenerateTemplateByFileType(DEFAULT_STORE_ID, GenericTemplateFileType.PURE_DELIVERY_FILE, getRequestId());
    try {
      URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH
              + BulkProcessController.REGENERATE_CATEGORY_ATTRIBUTE_MAPPING_GENERIC_TEMPLATE_BY_FILE_TYPE)
          .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
          .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
          .addParameter("username", DEFAULT_USERNAME)
          .addParameter("fileType", GenericTemplateFileType.PURE_DELIVERY_FILE.name()).build();
      getMockMvc().perform(
              MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    } catch (ApplicationRuntimeException e) {
    } finally {
      verify(this.bulkProcessService).regenerateTemplateByFileType(DEFAULT_STORE_ID,
          GenericTemplateFileType.PURE_DELIVERY_FILE, getRequestId());
    }
  }

  @Test
  public void regenerateCategoryAttributeInGenericBulkTemplateByFileApplicationExceptionType() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(this.bulkProcessService)
        .regenerateTemplateByFileType(DEFAULT_STORE_ID, GenericTemplateFileType.PURE_DELIVERY_FILE, getRequestId());
    try {
      URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH
              + BulkProcessController.REGENERATE_CATEGORY_ATTRIBUTE_MAPPING_GENERIC_TEMPLATE_BY_FILE_TYPE)
          .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
          .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
          .addParameter("username", DEFAULT_USERNAME)
          .addParameter("fileType", GenericTemplateFileType.PURE_DELIVERY_FILE.name()).build();
      getMockMvc().perform(
              MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
          .andExpect(status().isOk());
    } catch (ApplicationRuntimeException e) {
    } finally {
      verify(this.bulkProcessService).regenerateTemplateByFileType(DEFAULT_STORE_ID,
          GenericTemplateFileType.PURE_DELIVERY_FILE, getRequestId());
    }
  }

  @Test
  public void abortPendingDownloadTasksTest() throws Exception {
    doNothing().when(unifiedBulkDownloadService).abortPendingDownloadProcesses(eq(DEFAULT_STORE_ID));
    URI uri =
        new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.ABORT_PENDING_DOWNLOAD_TASK)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", requestId)
            .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(put(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true));
    verify(unifiedBulkDownloadService).abortPendingDownloadProcesses(eq(DEFAULT_STORE_ID));
  }

  @Test
  public void abortPendingDownloadTasksTest_Exception() throws Exception {
    doThrow(ApplicationRuntimeException.class).when(unifiedBulkDownloadService).abortPendingDownloadProcesses(eq(DEFAULT_STORE_ID));
    URI uri =
        new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.ABORT_PENDING_DOWNLOAD_TASK)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", requestId)
            .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(put(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(false));
    verify(unifiedBulkDownloadService).abortPendingDownloadProcesses(eq(DEFAULT_STORE_ID));
  }

  @Test
  public void filterWholeSaleConfigBulkProcessNotesByBulkProcessCodeTest() throws Exception {
    when(bulkProcessService.filterWholeSaleConfigBulkProcessNotes(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE))
        .thenReturn(new WholeSaleCountResponse(10, 10, 10, DEFAULT_PATH));
    URI uri = new URIBuilder().setPath(
        BulkProcessController.BASE_PATH + BulkProcessController.FILTER_BULK_PROCESS_CODE + DEFAULT_BULK_PROCESS_CODE
            + "/wholesaleConfig").addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();
    MvcResult result = getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true))).andReturn();
    verify(bulkProcessService).filterWholeSaleConfigBulkProcessNotes(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
  }

  @Test
  public void regenerateCategoryAttributeInGenericBulkTemplateTest() throws Exception {
    doNothing().when(this.bulkProcessService).regenerateMasterBrandValuesInGenericBulkTemplate(FILE_TYPE);
    URI uri = new URIBuilder().setPath(
        BulkProcessController.BASE_PATH + BulkProcessController.REGENERATE_MASTER_BRAND_VALUES_IN_GENERIC_TEMPLATE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
    verify(this.bulkProcessService)
        .regenerateMasterBrandValuesInGenericBulkTemplate(GenericTemplateFileType.DEFAULT_FILE.name());
  }

  @Test
  public void regenerateBrandValuesInCategoryTemplateTest() throws Exception {
    doNothing().when(this.bulkProcessService).regenerateBrandValuesInCategoryTemplate();
    URI uri = new URIBuilder()
        .setPath(BulkProcessController.BASE_PATH + BulkProcessController.REGENERATE_BRAND_VALUES_IN_CATEGORY_TEMPLATE)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
    verify(this.bulkProcessService).regenerateBrandValuesInCategoryTemplate();
  }

  @Test
  public void countNumberOfUploadsTest() throws Exception {
    when(this.bulkProcessService
        .countNumberOfUploadsByUser(DEFAULT_STORE_ID, BulkProcessType.VENDOR_BULK_ASSIGN.getValue(), DEFAULT_USERNAME,
            BulkProcess.STATUS_PENDING)).thenReturn(1L);
    URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.COUNT_NUMBER_OF_UPLOADS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("bulkProcessType", BulkProcessType.VENDOR_BULK_ASSIGN.getValue())
        .addParameter("status", BulkProcess.STATUS_PENDING).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk());
    verify(this.bulkProcessService)
        .countNumberOfUploadsByUser(DEFAULT_STORE_ID, BulkProcessType.VENDOR_BULK_ASSIGN.getValue(), DEFAULT_USERNAME,
            BulkProcess.STATUS_PENDING);
  }

  @Test
  public void getPendingBulkRequestMailTest() throws Exception {
    Mockito.doNothing().when(this.bulkProcessService).sendMailIfPendingRequestMoreThanThreshold(DEFAULT_STORE_ID);
    URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.GET_PENDING_BULK_REQUESTS_MAIL)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(this.bulkProcessService).sendMailIfPendingRequestMoreThanThreshold(DEFAULT_STORE_ID);
  }

  @Test
  public void getPendingBulkRequestMailFailedTest() throws Exception {
    Mockito.doThrow(Exception.class)
        .when(this.bulkProcessService).sendMailIfPendingRequestMoreThanThreshold(DEFAULT_STORE_ID);
    URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.GET_PENDING_BULK_REQUESTS_MAIL)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", is(false)));
      verify(this.bulkProcessService).sendMailIfPendingRequestMoreThanThreshold(DEFAULT_STORE_ID);
  }

  @Test
  public void checkBulkProcessStatusTest() throws Exception {
    URI uri = new URIBuilder().setPath(
            BulkProcessController.BASE_PATH + BulkProcessController.ROOT + INSTORE_BULK_PROCESS_TYPE
                + "/check-bulk-process-status").addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID).addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("requestId", getRequestId()).addParameter("username", DEFAULT_USERNAME).build();

    getMockMvc().perform(
            MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(this.bulkProcessService).checkBulkProcessStatus(DEFAULT_STORE_ID, INSTORE_BULK_PROCESS_TYPE);
  }

  @Test
  public void deleteFromDbTest() throws Exception {
    Mockito.doNothing().when(this.bulkProcessService).deleteFromDb(DEFAULT_STORE_ID);
    URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.DELETE_DATA_FROM_DB)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.delete(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(this.bulkProcessService).deleteFromDb(DEFAULT_STORE_ID);
  }

  @Test
  public void publishEventsByProcessTypeTest() throws Exception {
    URI uri = new URIBuilder().setPath(
            BulkProcessController.BASE_PATH + BulkProcessController.ROOT + INSTORE_BULK_PROCESS_TYPE + "/publishEvents")
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(
            MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(this.bulkProcessServiceWrapper).processReadyToProcessData(DEFAULT_STORE_ID, INSTORE_BULK_PROCESS_TYPE);
  }

  @Test
  public void sendNotificationByProcessTypeTest() throws Exception {
    URI uri = new URIBuilder().setPath(
            BulkProcessController.BASE_PATH + BulkProcessController.ROOT + INSTORE_BULK_PROCESS_TYPE + "/sendNotification")
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(
            MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    verify(this.bulkProcessServiceWrapper).processProcessedData(DEFAULT_STORE_ID, INSTORE_BULK_PROCESS_TYPE);
  }

  @Test
  public void checkStuckProcessStatusTest() throws Exception {
    doNothing().when(bulkProcessService).checkStuckProcessStatus(Mockito.anyString());
    URI uri =
        new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.CHECK_STUCK_PROCESS_STATUS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", requestId)
            .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(put(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true));
    verify(bulkProcessService).checkStuckProcessStatus(Mockito.any());
  }

  @Test
  public void publishCreateEventTest() throws Exception {
    BulkProcessPublishRequest bulkProcessPublishRequest =
        BulkProcessPublishRequest.builder().bulkProcessCode("blpCode").businessPartnerCode("business")
            .categoryCode("Categetory").build();
    doNothing().when(bulkProcessService).publishBulkProductCreationEvent(Mockito.anyString(), any(BulkProcess.class),
      anyString());
    URI uri =
        new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.PUBLISH_BULK_CREATE_EVENT)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", requestId)
            .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(getObjectMapper().writeValueAsString(bulkProcessPublishRequest))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(true));
    verify(bulkProcessService).publishBulkProductCreationEvent(Mockito.anyString(), any(BulkProcess.class),
      any());
  }


  @Test
  public void abortBulkProcessTest() throws Exception {
    doNothing().when(bulkProcessService).abortPendingInprogressBulkProcess(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    URI uri =
        new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.ABORT_BULK_PROCESS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", requestId)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("bulkProcessCode", DEFAULT_BULK_PROCESS_CODE).build();
    getMockMvc().perform(put(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true));
    verify(bulkProcessService).abortPendingInprogressBulkProcess(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
  }


  @Test
  public void abortBulkProcessExceptionTest() throws Exception {
    doThrow(Exception.class).when(bulkProcessService).abortPendingInprogressBulkProcess(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    URI uri =
        new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.ABORT_BULK_PROCESS)
            .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
            .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", requestId)
            .addParameter("username", DEFAULT_USERNAME)
            .addParameter("bulkProcessCode", DEFAULT_BULK_PROCESS_CODE).build();
    getMockMvc().perform(put(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(false));
    verify(bulkProcessService).abortPendingInprogressBulkProcess(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
  }

  @Test
  public void fetchBulkProcessStatusListingTest() throws Exception {
    int page = 0;
    int size = 50;
    BulkProcessStatusListingResponse response = new BulkProcessStatusListingResponse();
    response.setUser(DEFAULT_USERNAME);
    response.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    Pageable pageable = PageRequest.of(page, size);

    Page<BulkProcessStatusListingResponse> pageResponse = new PageImpl<>(List.of(response), pageable, 1L);
    when(bulkProcessService.fetchProcessListingResponse(eq(DEFAULT_STORE_ID),
      eq(DEFAULT_BULK_PROCESS_TYPE), eq(DEFAULT_USERNAME), eq(DEFAULT_BUSINESS_PARTNER_CODE),
      eq(Optional.empty()), eq(Optional.empty()), eq(false), eq(pageable))).thenReturn(pageResponse);
    mockMvc.perform(MockMvcRequestBuilders.get(BulkProcessController.BASE_PATH+BulkProcessController.BULK_PROCESS_STATUS_LISTING)
        .param("storeId", DEFAULT_STORE_ID)
        .param("bulkProcessType", DEFAULT_BULK_PROCESS_TYPE)
        .param("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .param("requestId", DEFAULT_USERNAME)
        .contentType(MediaType.APPLICATION_JSON))
      .andExpect(status().isOk())
      .andExpect(jsonPath("$.success").value(true))
      .andExpect(jsonPath("$.content[0]").exists())
      .andExpect(jsonPath("$.requestId").value(DEFAULT_USERNAME));

    verify(bulkProcessService).fetchProcessListingResponse(eq(DEFAULT_STORE_ID),
      eq(DEFAULT_BULK_PROCESS_TYPE), eq(DEFAULT_USERNAME), eq(DEFAULT_BUSINESS_PARTNER_CODE),
      eq(Optional.empty()), eq(Optional.empty()), eq(false), eq(pageable));
  }

  @Test
  public void testFetchBulkProcessStatusListing_Exception() throws Exception {
    Pageable pageable = PageRequest.of(0, 50);
    when(bulkProcessService.fetchProcessListingResponse(eq(DEFAULT_STORE_ID),
      eq(DEFAULT_BULK_PROCESS_TYPE), eq(DEFAULT_USERNAME), eq(DEFAULT_BUSINESS_PARTNER_CODE),
      eq(Optional.empty()), eq(Optional.empty()), eq(false), eq(pageable))).thenThrow(new ApplicationRuntimeException(ErrorCategory.VALIDATION,
      ErrorCategory.VALIDATION.getCode()));

    mockMvc.perform(MockMvcRequestBuilders.get(
          BulkProcessController.BASE_PATH + BulkProcessController.BULK_PROCESS_STATUS_LISTING)
        .param("storeId", DEFAULT_STORE_ID).param("bulkProcessType", DEFAULT_BULK_PROCESS_TYPE)
        .param("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE).param("requestId",
          DEFAULT_USERNAME).param("estimationNeeded", String.valueOf(false))
        .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success").value(false)).andExpect(jsonPath("$.data").doesNotExist())
      .andExpect(jsonPath("$.metadata").doesNotExist())
      .andExpect(jsonPath("$.requestId").value(DEFAULT_USERNAME));
    verify(bulkProcessService).fetchProcessListingResponse(eq(DEFAULT_STORE_ID),
      eq(DEFAULT_BULK_PROCESS_TYPE), eq(DEFAULT_USERNAME), eq(DEFAULT_BUSINESS_PARTNER_CODE),
      eq(Optional.empty()), eq(Optional.empty()), eq(false), eq(pageable));
  }

  @Test
  public void evaluateBulkProcessEstimationTest() throws Exception{
    doNothing().when(bulkProcessService).evaluateBulkProcessEstimation(DEFAULT_STORE_ID,
      Constant.USER_NAME);

    mockMvc.perform(MockMvcRequestBuilders.post(
          BulkProcessController.BASE_PATH + BulkProcessController.EVALUATE_BULK_PROCESS_ESTIMATION)
        .param("storeId", DEFAULT_STORE_ID).param("username", Constant.USER_NAME))
      .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true));

    verify(bulkProcessService).evaluateBulkProcessEstimation(DEFAULT_STORE_ID,Constant.USER_NAME);
  }

  @Test
  public void testCheckForQrProcessAllowedSuccess() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse(true);
    when(bulkProcessService.checkForQrProcessAllowed(DEFAULT_STORE_ID,
        DEFAULT_BUSINESS_PARTNER_CODE)).thenReturn(true);
    URI uri = new URIBuilder().setPath(
            BulkProcessController.BASE_PATH + BulkProcessController.CHECK_QR_PROCESS_ALLOWED)
        .addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .build();
    MvcResult result = getMockMvc().perform(MockMvcRequestBuilders.get(uri)
        .accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
        result.getResponse().getContentAsString());
    verify(bulkProcessService).checkForQrProcessAllowed(DEFAULT_STORE_ID,
        DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void testCheckForQrProcessAllowedFail() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse(false);
    when(bulkProcessService.checkForQrProcessAllowed(DEFAULT_STORE_ID,
        DEFAULT_BUSINESS_PARTNER_CODE)).thenThrow(new RuntimeException());
    URI uri = new URIBuilder().setPath(
            BulkProcessController.BASE_PATH + BulkProcessController.CHECK_QR_PROCESS_ALLOWED)
        .addParameter("storeId", DEFAULT_STORE_ID)
        .addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID)
        .addParameter("requestId", getRequestId())
        .addParameter("username", DEFAULT_USERNAME)
        .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
        .build();
    MvcResult result = getMockMvc().perform(MockMvcRequestBuilders.get(uri)
        .accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
        result.getResponse().getContentAsString());
    verify(bulkProcessService).checkForQrProcessAllowed(DEFAULT_STORE_ID,
        DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void uploadQrGenerationExcelTest() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse(true);
    String requestId = getRequestId();
    URI uri = new URIBuilder().setPath(
        BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD_EXCEL_FOR_QR)
      .addParameter("storeId", DEFAULT_STORE_ID)
      .addParameter("channelId", DEFAULT_CHANNEL_ID)
      .addParameter("clientId", DEFAULT_CLIENT_ID)
      .addParameter("requestId", requestId)
      .addParameter("username", DEFAULT_USERNAME)
      .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
      .build();
    MvcResult result = getMockMvc().perform(MockMvcRequestBuilders.post(uri)
        .content(this.objectMapper.writeValueAsString(qrExcelUploadRequest))
      .accept(MediaType.APPLICATION_JSON)
      .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
      result.getResponse().getContentAsString());
    verify(bulkProcessService).insertQrExcelRequest(DEFAULT_STORE_ID,
      qrExcelUploadRequest, requestId, DEFAULT_USERNAME);
  }

  @Test
  public void uploadQrGenerationExcel_exceptionTest() throws Exception {
    GdnBaseRestResponse response = new GdnBaseRestResponse(false);
    String requestId = getRequestId();
    Mockito.doThrow(Exception.class).when(bulkProcessService).insertQrExcelRequest(DEFAULT_STORE_ID,
      qrExcelUploadRequest, requestId, DEFAULT_USERNAME);
    URI uri = new URIBuilder().setPath(
        BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD_EXCEL_FOR_QR)
      .addParameter("storeId", DEFAULT_STORE_ID)
      .addParameter("channelId", DEFAULT_CHANNEL_ID)
      .addParameter("clientId", DEFAULT_CLIENT_ID)
      .addParameter("requestId", requestId)
      .addParameter("username", DEFAULT_USERNAME)
      .addParameter("businessPartnerCode", DEFAULT_BUSINESS_PARTNER_CODE)
      .build();
    MvcResult result = getMockMvc().perform(MockMvcRequestBuilders.post(uri)
            .content(this.objectMapper.writeValueAsString(qrExcelUploadRequest))
      .accept(MediaType.APPLICATION_JSON)
      .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk()).andReturn();
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
      result.getResponse().getContentAsString());
    verify(bulkProcessService).insertQrExcelRequest(DEFAULT_STORE_ID,
      qrExcelUploadRequest, requestId, DEFAULT_USERNAME);
  }

  @Test
  public void createWorkOrderTest() throws Exception {
    BulkProcessUpdateRequest bulkWorkOrderRequest = new BulkProcessUpdateRequest();
    bulkWorkOrderRequest.setBulkProcessType(DEFAULT_BULK_PROCESS_TYPE);
    bulkWorkOrderRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkWorkOrderRequest.setPrivilegedMap(new HashMap<>());
    bulkWorkOrderRequest.setClientHost(Constant.DEFAULT_CLIENT_HOST);
    bulkWorkOrderRequest.setFileName(FILE_NAME);
    bulkWorkOrderRequest.setFileContent(new byte[1]);
    bulkWorkOrderRequest.setUpdatedBy("");
    URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.BULK_WORK_ORDER)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", requestId)
        .addParameter("username", DEFAULT_USERNAME).build();
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(bulkWorkOrderRequest))).andExpect(status().isOk());
    Assertions.assertEquals(DEFAULT_BULK_PROCESS_TYPE, bulkWorkOrderRequest.getBulkProcessType());
    Mockito.verify(bulkProcessService).preProcessWorkOrder(any(), any(), any());
  }

  @Test
  public void createWorkOrder_Exception_Test() throws Exception {
    BulkProcessUpdateRequest bulkWorkOrderRequest = new BulkProcessUpdateRequest();
    bulkWorkOrderRequest.setBulkProcessType(DEFAULT_BULK_PROCESS_TYPE);
    bulkWorkOrderRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkWorkOrderRequest.setPrivilegedMap(new HashMap<>());
    bulkWorkOrderRequest.setClientHost(Constant.DEFAULT_CLIENT_HOST);
    bulkWorkOrderRequest.setFileName(FILE_NAME);
    bulkWorkOrderRequest.setFileContent(new byte[1]);
    bulkWorkOrderRequest.setUpdatedBy("");
    URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.BULK_WORK_ORDER)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", requestId)
        .addParameter("username", DEFAULT_USERNAME).build();
    Mockito.doThrow(new ApplicationRuntimeException()).when(bulkProcessService)
        .preProcessWorkOrder(any(), any(), any());
    getMockMvc().perform(post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(bulkWorkOrderRequest))).andExpect(status().isOk());
    Assertions.assertEquals(DEFAULT_BULK_PROCESS_TYPE, bulkWorkOrderRequest.getBulkProcessType());
    Mockito.verify(bulkProcessService).preProcessWorkOrder(any(), any(), any());
  }

  @Test
  public void abortStruckProcessesByProcessTypeTest() throws Exception {
    List<String> bulkProcessTypes = Arrays.asList(BulkProcessType.RECATEGORIZATION.getValue(),
      BulkProcessType.DELETE_PICKUP_POINT.getValue());
    Mockito.doNothing().when(bulkProcessServiceWrapper)
      .abortStruckProcessesByProcessTypes(DEFAULT_STORE_ID, bulkProcessTypes);
    String bulkProcessTypesJson = new ObjectMapper().writeValueAsString(bulkProcessTypes);
    URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH
        + BulkProcessController.ABORT_STRUCK_PROCESSES_BY_PROCESS_TYPE)
      .addParameter("storeId", DEFAULT_STORE_ID).build();
    getMockMvc().perform(put(uri).content(bulkProcessTypesJson).accept(MediaType.APPLICATION_JSON)
      .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
    Mockito.verify(bulkProcessServiceWrapper)
      .abortStruckProcessesByProcessTypes(DEFAULT_STORE_ID, bulkProcessTypes);
  }

  @Test
  public void abortStruckProcessesByProcessTypeExceptionTest() throws Exception {
    List<String> bulkProcessTypes = Arrays.asList(BulkProcessType.RECATEGORIZATION.getValue(),
      BulkProcessType.DELETE_PICKUP_POINT.getValue());
    Mockito.doThrow(new RuntimeException()).when(bulkProcessServiceWrapper)
      .abortStruckProcessesByProcessTypes(DEFAULT_STORE_ID, bulkProcessTypes);
    String bulkProcessTypesJson = new ObjectMapper().writeValueAsString(bulkProcessTypes);
    URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH
        + BulkProcessController.ABORT_STRUCK_PROCESSES_BY_PROCESS_TYPE)
      .addParameter("storeId", DEFAULT_STORE_ID).build();
    getMockMvc().perform(put(uri).content(bulkProcessTypesJson).accept(MediaType.APPLICATION_JSON)
      .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk());
    Mockito.verify(bulkProcessServiceWrapper)
      .abortStruckProcessesByProcessTypes(DEFAULT_STORE_ID, bulkProcessTypes);
  }

  @Test
  public void testExternalUpload() throws Exception {
    BulkProcessExternalUploadRequest request = new BulkProcessExternalUploadRequest();
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, getRequestId());
    URI uri = new URIBuilder().setPath(
        BulkProcessController.BASE_PATH + BulkProcessController.EXTERNAL_UPLOAD)
      .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
      .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", getRequestId())
      .addParameter("username", DEFAULT_USERNAME).build();
    MvcResult result = getMockMvc().perform(
        post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(request))).andExpect(status().isOk())
      .andReturn();
    Mockito.verify(externalProductCreationService).preProcess(anyString(), anyString(), anyString(),
      any(BulkProcessExternalUploadRequest.class));
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response),
      result.getResponse().getContentAsString());
  }

  @Test
  public void testExternalUploadApplicationException() throws Exception {
    // given
    BulkProcessExternalUploadRequest request = new BulkProcessExternalUploadRequest();
    String requestId = getRequestId();
    String errorMessage = "Can not process invalid input data :ErrorMessage";
    String errorCode = "VALIDATION";
    ApplicationRuntimeException exception =
      new ApplicationRuntimeException(ErrorCategory.VALIDATION, "ErrorMessage");
    // mock preProcess to throw
    Mockito.doThrow(exception).when(externalProductCreationService)
      .preProcess(anyString(), anyString(), anyString(),
        any(BulkProcessExternalUploadRequest.class));
    URI uri = new URIBuilder().setPath(
        BulkProcessController.BASE_PATH + BulkProcessController.EXTERNAL_UPLOAD)
      .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
      .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", requestId)
      .addParameter("username", DEFAULT_USERNAME).build();
    // expected response (from controller catch block)
    GdnBaseRestResponse expectedResponse =
      new GdnBaseRestResponse(errorMessage, errorCode, false, requestId);
    // when
    MvcResult result = getMockMvc().perform(
        post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(request))).andExpect(status().isOk())
      .andReturn();
    // then
    Mockito.verify(externalProductCreationService).preProcess(anyString(), anyString(), anyString(),
      any(BulkProcessExternalUploadRequest.class));
    Assertions.assertEquals(getObjectMapper().writeValueAsString(expectedResponse),
      result.getResponse().getContentAsString());
  }

  @Test
  public void testExternalUploadGenericException() throws Exception {
    // given
    BulkProcessExternalUploadRequest request = new BulkProcessExternalUploadRequest();
    String requestId = getRequestId();
    String errorMessage = Constant.SYSTEM_ERROR;
    String errorCode = "";
    ApiIncorrectInputDataException exception =
      new ApiIncorrectInputDataException(Constant.SYSTEM_ERROR, "ErrorMessage");
    // mock preProcess to throw
    Mockito.doThrow(exception).when(externalProductCreationService)
      .preProcess(anyString(), anyString(), anyString(),
        any(BulkProcessExternalUploadRequest.class));
    URI uri = new URIBuilder().setPath(
        BulkProcessController.BASE_PATH + BulkProcessController.EXTERNAL_UPLOAD)
      .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
      .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", requestId)
      .addParameter("username", DEFAULT_USERNAME).build();
    // expected response (from controller catch block)
    GdnBaseRestResponse expectedResponse =
      new GdnBaseRestResponse(errorMessage, errorCode, false, requestId);
    // when
    MvcResult result = getMockMvc().perform(
        post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
          .content(getObjectMapper().writeValueAsString(request))).andExpect(status().isOk())
      .andReturn();
    // then
    Mockito.verify(externalProductCreationService).preProcess(anyString(), anyString(), anyString(),
      any(BulkProcessExternalUploadRequest.class));
    Assertions.assertEquals(getObjectMapper().writeValueAsString(expectedResponse),
      result.getResponse().getContentAsString());
  }

  @Test
  public void testUploadForBulkEANUpdate() throws Exception {
    BulkProcessV2Request bulkRequest = new BulkProcessV2Request();
    bulkRequest.setBulkProcessType(DEFAULT_BULK_PROCESS_TYPE);
    bulkRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkRequest.setFileName(FILE_NAME);
    bulkRequest.setFileContent(new byte[256]);
    bulkRequest.setPrivilegedMap(PRIVILEGED_MAP);
    bulkRequest.setUpdatedBy(DEFAULT_USERNAME);
    bulkRequest.setClientHost(DEFAULT_CLIENT_ID);

    String requestId = getRequestId();
    GdnBaseRestResponse response = new GdnBaseRestResponse("", "", true, requestId);

    doNothing().when(eanProductLevel4BulkUpdateService)
        .preProcessBulkUpdateEAN(anyString(), anyString(), any(BulkUpdateProcessDTO.class));

    URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD_BULK_UPDATE_EAN)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", requestId)
        .addParameter("username", DEFAULT_USERNAME).build();

    MvcResult result = getMockMvc().perform(
        post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(bulkRequest))).andExpect(status().isOk()).andReturn();

    verify(eanProductLevel4BulkUpdateService).preProcessBulkUpdateEAN(eq(DEFAULT_STORE_ID), eq(requestId),
        any(BulkUpdateProcessDTO.class));
    Assertions.assertEquals(getObjectMapper().writeValueAsString(response), result.getResponse().getContentAsString());
  }

  @Test
  public void testUploadForBulkEANUpdate_ApplicationRuntimeException() throws Exception {
    // Test case for ApplicationRuntimeException catch block (lines 560-568)
    BulkProcessV2Request bulkRequest = new BulkProcessV2Request();
    bulkRequest.setBulkProcessType(DEFAULT_BULK_PROCESS_TYPE);
    bulkRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkRequest.setFileName(FILE_NAME);
    bulkRequest.setFileContent(new byte[256]);
    bulkRequest.setPrivilegedMap(PRIVILEGED_MAP);
    bulkRequest.setUpdatedBy(DEFAULT_USERNAME);
    bulkRequest.setClientHost(DEFAULT_CLIENT_ID);

    String requestId = getRequestId();
    String errorMessage = "Validation error";
    String errorCode = "VALIDATION";
    ApplicationRuntimeException exception = new ApplicationRuntimeException(ErrorCategory.VALIDATION, errorMessage);

    doThrow(exception).when(eanProductLevel4BulkUpdateService)
        .preProcessBulkUpdateEAN(anyString(), anyString(), any(BulkUpdateProcessDTO.class));

    // ApplicationRuntimeException with ErrorCategory.VALIDATION prefixes message with "Can not process invalid input data :"
    String expectedErrorMessage = ErrorCategory.VALIDATION.getMessage() + errorMessage;
    GdnBaseRestResponse expectedResponse = new GdnBaseRestResponse(expectedErrorMessage, errorCode, false, requestId);

    URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD_BULK_UPDATE_EAN)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", requestId)
        .addParameter("username", DEFAULT_USERNAME).build();

    MvcResult result = getMockMvc().perform(
        post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(bulkRequest))).andExpect(status().isOk()).andReturn();

    verify(eanProductLevel4BulkUpdateService).preProcessBulkUpdateEAN(eq(DEFAULT_STORE_ID), eq(requestId),
        any(BulkUpdateProcessDTO.class));
    verify(trackerService).sendTracker(PRODUCT_UPDATE_EVENT, PRODUCT_UPDATE_ATTRI_TYPE, HYPHEN, TrackerConstants.FAILED,
        DEFAULT_USERNAME);
    Assertions.assertEquals(getObjectMapper().writeValueAsString(expectedResponse),
        result.getResponse().getContentAsString());
  }

  @Test
  public void testUploadForBulkEANUpdate_GenericException() throws Exception {
    // Test case for generic Exception catch block (lines 569-576)
    BulkProcessV2Request bulkRequest = new BulkProcessV2Request();
    bulkRequest.setBulkProcessType(DEFAULT_BULK_PROCESS_TYPE);
    bulkRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkRequest.setFileName(FILE_NAME);
    bulkRequest.setFileContent(new byte[256]);
    bulkRequest.setPrivilegedMap(PRIVILEGED_MAP);
    bulkRequest.setUpdatedBy(DEFAULT_USERNAME);
    bulkRequest.setClientHost(DEFAULT_CLIENT_ID);

    String requestId = getRequestId();
    String errorMessage = "System error occurred";
    RuntimeException exception = new RuntimeException(errorMessage);

    doThrow(exception).when(eanProductLevel4BulkUpdateService)
        .preProcessBulkUpdateEAN(anyString(), anyString(), any(BulkUpdateProcessDTO.class));

    GdnBaseRestResponse expectedResponse = new GdnBaseRestResponse(errorMessage, "", false, requestId);

    URI uri = new URIBuilder().setPath(BulkProcessController.BASE_PATH + BulkProcessController.UPLOAD_BULK_UPDATE_EAN)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("requestId", requestId)
        .addParameter("username", DEFAULT_USERNAME).build();

    MvcResult result = getMockMvc().perform(
        post(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(getObjectMapper().writeValueAsString(bulkRequest))).andExpect(status().isOk()).andReturn();

    verify(eanProductLevel4BulkUpdateService).preProcessBulkUpdateEAN(eq(DEFAULT_STORE_ID), eq(requestId),
        any(BulkUpdateProcessDTO.class));
    verify(trackerService).sendTracker(PRODUCT_UPDATE_EVENT, PRODUCT_UPDATE_ATTRI_TYPE, HYPHEN, TrackerConstants.FAILED,
        DEFAULT_USERNAME);
    Assertions.assertEquals(getObjectMapper().writeValueAsString(expectedResponse),
        result.getResponse().getContentAsString());
  }
}

