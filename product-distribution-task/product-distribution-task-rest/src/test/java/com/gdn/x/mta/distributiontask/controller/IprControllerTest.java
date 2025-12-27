package com.gdn.x.mta.distributiontask.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.model.ErrorCategory;
import com.gdn.x.mta.distributiontask.model.IprApiPath;
import com.gdn.x.mta.distributiontask.model.dto.BrandReport;
import com.gdn.x.mta.distributiontask.model.dto.IPRProductListRequest;
import com.gdn.x.mta.distributiontask.model.dto.IPRUpdateAssigneeRequest;
import com.gdn.x.mta.distributiontask.model.dto.IprActionRequest;
import com.gdn.x.mta.distributiontask.model.dto.SubmitEvidenceRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.IprProductDetailsResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.IprProductListResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.IprSuspensionInProgressResponse;
import com.gdn.x.mta.distributiontask.service.api.IprService;
import com.gdn.x.mta.distributiontask.service.api.IprWrapperService;

import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

@ExtendWith(MockitoExtension.class)
class IprControllerTest {

  @InjectMocks
  private IprController iprController;

  @Mock
  private IprService iprService;

  @Mock
  private IprWrapperService iprWrapperService;

  private MockMvc mockMvc;
  private IprSuspensionInProgressResponse iprSuspensionInProgressResponse;
  private IPRProductListRequest iprProductListRequest;
  private SubmitEvidenceRequest submitEvidenceRequest;
  private Pageable pageable;
  private ObjectMapper objectMapper;
  private Page<IprProductListResponse> iprProductListResponsePage;
  private IPRUpdateAssigneeRequest iprUpdateAssigneeRequest;
  private BrandReport brandReport;

  private static final String STORE_ID = "store-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String CLIENT_ID = "client-id";
  private static final String USER_NAME = "user-name";
  private static final String REQUEST_ID = "request-id";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String ASSIGNEE = "assignee";
  private static final String ERROR_MESSAGE = "error message";
  private static final String BUSINESS_PARTNER_CODE = "HUA-60027";
  private static final String SOURCE = "CUSTOMER_REPORT";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String SORT_ORDER = "ASC";


  @BeforeEach
  public void setUp() {
    iprSuspensionInProgressResponse = new IprSuspensionInProgressResponse();
    iprProductListRequest = new IPRProductListRequest();
    submitEvidenceRequest = new SubmitEvidenceRequest();
    pageable = PageRequest.of(PAGE, SIZE);
    objectMapper = new ObjectMapper();
    IprProductListResponse iprProductListResponse=new IprProductListResponse();
    iprProductListResponsePage = new PageImpl<>(List.of(iprProductListResponse), pageable, 1L);
    iprUpdateAssigneeRequest =
        IPRUpdateAssigneeRequest.builder().productSku(Collections.singletonList(PRODUCT_SKU))
            .assignedTo(ASSIGNEE).build();
    brandReport = new BrandReport();
    this.mockMvc = MockMvcBuilders.standaloneSetup(this.iprController).build();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(iprService, iprWrapperService);
    Mockito.verifyNoMoreInteractions(iprWrapperService);
  }

  @Test
  void findSuspensionInProgressProductTest() throws Exception {
    iprSuspensionInProgressResponse.setProductCode(PRODUCT_CODE);
    Mockito.when(
            this.iprService.findSuspensionInProgressProducts(STORE_ID, BUSINESS_PARTNER_CODE, 0,
                25, "asc"))
        .thenReturn(createIprSuspensionProduct(iprSuspensionInProgressResponse));
    this.mockMvc.perform(
        MockMvcRequestBuilders.get(IprApiPath.BASE_PATH + IprApiPath.SUSPENSION_IN_PROGRESS)
          .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
          .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
          .param("username", USER_NAME).param("businessPartnerCode", BUSINESS_PARTNER_CODE)
          .param("size", "25").param("page", "0").param("sortOrder", "asc"))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.iprService, Mockito.times(1))
        .findSuspensionInProgressProducts(STORE_ID, BUSINESS_PARTNER_CODE, 0, 25, "asc");
  }

  @Test
  void findSuspensionInProgressProductFailedTest() throws Exception {
    Mockito.when(
      this.iprService.findSuspensionInProgressProducts(STORE_ID, BUSINESS_PARTNER_CODE, 0, 25,
        SORT_ORDER)).thenThrow(new Exception());
    this.mockMvc.perform(
        MockMvcRequestBuilders.get(IprApiPath.BASE_PATH + IprApiPath.SUSPENSION_IN_PROGRESS)
          .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
          .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
          .param("username", USER_NAME).param("businessPartnerCode", BUSINESS_PARTNER_CODE)
          .param("size", "25").param("page", "0").param("sortOder", SORT_ORDER))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.iprService, Mockito.times(1))
      .findSuspensionInProgressProducts(STORE_ID, BUSINESS_PARTNER_CODE, 0, 25, SORT_ORDER);
  }

  @Test
  void fetchProductIprListingTest() throws Exception {
    Mockito.when(
        this.iprService.getIprProductListResponse(STORE_ID, iprProductListRequest, pageable))
      .thenReturn(iprProductListResponsePage);
    this.mockMvc.perform(MockMvcRequestBuilders.post(IprApiPath.BASE_PATH + IprApiPath.FILTER)
        .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USER_NAME)
        .param("size", "10").param("page", "0")
        .content(objectMapper.writeValueAsString(iprProductListRequest))
        .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.iprService)
      .getIprProductListResponse(STORE_ID, iprProductListRequest, pageable);
  }

  @Test
  void fetchProductIprListingTestExceptionTest() throws Exception {
    Mockito.when(
        this.iprService.getIprProductListResponse(STORE_ID, iprProductListRequest, pageable))
      .thenThrow(new Exception());
    this.mockMvc.perform(MockMvcRequestBuilders.post(IprApiPath.BASE_PATH + IprApiPath.FILTER)
        .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USER_NAME)
        .param("size", "10").param("page", "0")
        .content(objectMapper.writeValueAsString(iprProductListRequest))
        .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.iprService)
      .getIprProductListResponse(STORE_ID, iprProductListRequest, pageable);
  }

  private Page<IprSuspensionInProgressResponse> createIprSuspensionProduct(
      IprSuspensionInProgressResponse iprSuspensionInProgressResponse) {
    return new PageImpl<>(Collections.singletonList(iprSuspensionInProgressResponse),
        PageRequest.of(0, 25), 25);
  }

  @Test
  void submitEvidenceTest() throws Exception {
    this.mockMvc.perform(MockMvcRequestBuilders.post(IprApiPath.BASE_PATH + IprApiPath.SUBMIT_EVIDENCE)
            .accept(MediaType.APPLICATION_JSON).param("requestId", REQUEST_ID)
            .content(objectMapper.writeValueAsString(submitEvidenceRequest)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.iprWrapperService).submitEvidenceForProduct(submitEvidenceRequest);
  }

  @Test
  void createIprProductTest() throws Exception {
    Mockito.when(
            this.iprWrapperService.addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null,
                brandReport))
        .thenReturn(ERROR_MESSAGE);
    this.mockMvc.perform(
            MockMvcRequestBuilders.post(IprApiPath.BASE_PATH + IprApiPath.ADD_PRODUCT_IPR,
                    PRODUCT_SKU)
                .accept(MediaType.APPLICATION_JSON).param("requestId", REQUEST_ID)
                .param("storeId", STORE_ID).param("source", SOURCE)
                .content(objectMapper.writeValueAsString(brandReport))
                .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.iprWrapperService)
        .addProductToIPR(PRODUCT_SKU, STORE_ID, SOURCE, null, brandReport);
  }

  @Test
  void getProductDetailsForAllProductTypesTest() throws Exception {
    IprProductDetailsResponse response = new IprProductDetailsResponse();
    response.setProductSku(PRODUCT_SKU);
    Mockito.when(this.iprService.fetchIprProductDetails(PRODUCT_SKU)).thenReturn(response);
    this.mockMvc.perform(
            MockMvcRequestBuilders.get(IprApiPath.BASE_PATH + IprApiPath.DETAILS, PRODUCT_SKU)
                .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USER_NAME))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.iprService, Mockito.times(1)).fetchIprProductDetails(PRODUCT_SKU);
  }

  @Test
  void updateAssigneeIprTest() throws Exception {
    Mockito.doNothing().when(
            this.iprWrapperService).updateAssignee(iprUpdateAssigneeRequest);
    this.mockMvc.perform(MockMvcRequestBuilders.post(IprApiPath.BASE_PATH + IprApiPath.UPDATE_ASSIGNEE)
            .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USER_NAME)
            .content(objectMapper.writeValueAsString(iprUpdateAssigneeRequest))
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.iprWrapperService)
        .updateAssignee(iprUpdateAssigneeRequest);
  }

  @Test
  void performIprActionTest() throws Exception {
    IprActionRequest iprActionRequest = new IprActionRequest();
    Mockito.when(this.iprWrapperService.performIprActionForProduct(iprActionRequest, STORE_ID))
        .thenReturn(ErrorCategory.EMPTY_ERROR_MESSAGE);
    this.mockMvc.perform(
            MockMvcRequestBuilders.post(IprApiPath.BASE_PATH + IprApiPath.PERFORM_IPR_ACTION)
                .accept(MediaType.APPLICATION_JSON).param("requestId", REQUEST_ID)
                .param("storeId", STORE_ID).content(objectMapper.writeValueAsString(iprActionRequest))
                .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.iprWrapperService).performIprActionForProduct(iprActionRequest, STORE_ID);
  }

  @Test
  void getPrimaryFilterCountsTest() throws Exception {
    Map<String, Object> response = new HashMap<>();
    Mockito.when(this.iprService.getPrimaryFilterCounts(STORE_ID)).thenReturn(response);
    this.mockMvc.perform(
        MockMvcRequestBuilders.get(IprApiPath.BASE_PATH + IprApiPath.PRIMARY_FILTER_COUNTS)
          .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
          .param("requestId", REQUEST_ID)).andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.iprService, Mockito.times(1)).getPrimaryFilterCounts(STORE_ID);
  }

  @Test
  void suspendEvidenceRequestedProductTest() throws Exception {
    pageable = PageRequest.of(PAGE, SIZE, Sort.by(Sort.Direction.DESC, Constants.UPDATED_DATE));
    this.mockMvc.perform(MockMvcRequestBuilders.post(
          IprApiPath.BASE_PATH + IprApiPath.SUSPEND_EVIDENCE_REQUESTED_PRODUCT)
        .accept(MediaType.APPLICATION_JSON).param("requestId", REQUEST_ID).param("storeId", STORE_ID)
        .param("daysThreshold", "3").param("page", "0").param("size", "10")
        .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.iprService).fetchAndSuspendEvidenceRequestedProduct(STORE_ID, 3, pageable);
  }

  @Test
  void iprHistoryTest() throws Exception {
    pageable = PageRequest.of(PAGE, SIZE);
    Mockito.when(this.iprService.fetchIprHistoryByProductSku(STORE_ID, "PRODUCT_SKU", pageable))
        .thenReturn(new PageImpl<>(Collections.EMPTY_LIST));
    this.mockMvc.perform(MockMvcRequestBuilders.get(IprApiPath.BASE_PATH + IprApiPath.HISTORY)
            .param("requestId", REQUEST_ID).param("storeId", STORE_ID)
            .param("page", "0").param("size", "10")
            .param("productSku", "PRODUCT_SKU")).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.iprService).fetchIprHistoryByProductSku(STORE_ID, "PRODUCT_SKU", pageable);
  }
}
