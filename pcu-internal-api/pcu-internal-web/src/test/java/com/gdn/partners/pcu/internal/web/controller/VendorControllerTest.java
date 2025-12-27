package com.gdn.partners.pcu.internal.web.controller;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.VendorApiPath;
import com.gdn.partners.pcu.internal.service.VendorService;
import com.gdn.partners.pcu.internal.service.impl.exception.ClientException;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;
import com.gdn.partners.pcu.internal.web.model.request.BulkDeleteProductRequest;
import com.gdn.partners.pcu.internal.web.model.request.PrimaryFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductImageQcWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.RejectProductRequest;
import com.gdn.partners.pcu.internal.web.model.request.RejectReason;
import com.gdn.partners.pcu.internal.web.model.request.ScreeningProductBulkActionsWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.VendorAutoAssignmentFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.VendorAutoConsignmentWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.VendorSummaryFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.AssigneeWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.BulkUpdatePendingWebResposne;
import com.gdn.partners.pcu.internal.web.model.response.DistributionProductWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.NeedRevisionWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductBusinessPartnerMapperWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductDetailWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.ProductHistoryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.RejectProductResponse;
import com.gdn.partners.pcu.internal.web.model.response.VendorDetailWebResponse;
import com.gdn.x.mta.distributiontask.response.VendorDefaultFilterResponse;
import com.gdn.x.mta.distributiontask.response.VendorDetailResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductDetailRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorQuickApprovalRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.MapResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;

import jakarta.servlet.ServletException;
import org.apache.commons.io.FileUtils;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static com.gdn.partners.pcu.internal.model.Constants.VENDOR_TYPE_CONTENT;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@AutoConfigureMockMvc
public class VendorControllerTest extends TestHelper {

  private static final int PAGE = 0;
  private static final int SIZE = 25;
  private static final String PRODUCT_CODE = "productCode";
  private static final String STORE_ID = "storeId";
  private static final String NOTES = "notes";
  private static final String USER_NAME = "username";
  private static final String REQUEST_ID = "requestId";
  private static final String VENDOR_CODE = "vendorCode";
  private static final String TYPE = "content";
  private static final String PATH = "path";
  private static final String ORIGINAL_FILENAME = "originalFilename.xls";
  private static final String FILE = "/filestore/originalFilename.xls";
  private static final String VERSION = "1";
  private static final String PRODUCT = "PRODUCT";
  private static final String VENDOR_EMAIL = "vendorEmail";
  private static final String PROCESS_TYPE = "processType";

  private byte[] fileContent;
  private String productWebRequestAsString;
  private MockMultipartFile multipartFile;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @Mock
  private VendorService vendorService;

  @InjectMocks
  private VendorController vendorController;

  private static final String DEFAULT_BUSINESS_PARTNER_NAME = "blibli";
  private static final String DEFAULT_ASSIGNEE_NAME= "blibli";
  private static final String DEFAULT_REQUEST_ID = "REQUEST-ID";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BP-0001";
  private static final String CATEGORY_CODE = "category_code";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String TIME_FILTER_TYPE = "timeFilterType";
  private static final String KEYWORD = "keyword";
  private static final String ASSIGNEE_EMAIL_ID = "keyword";
  private static final String SORT_ORDER_BY_CREATED_DATE = "SOBCD";
  private static final String ACTION = "assign";
  private static final String STATE = "REJECTED";
  private static final String ASSIGNED_BY= "assigned_by";

  private static final String BULK_APPROVAL = "BULK_APPROVAL";
  private static final String BULK_REJECTION = "BULK_REJECTION";

  private ScreeningProductBulkActionsWebRequest screeningProductBulkActionsWebRequest = new ScreeningProductBulkActionsWebRequest();
  private PrimaryFilterWebRequest primaryFilterWebRequest = new PrimaryFilterWebRequest();
  private ProductBusinessPartnerMapperWebResponse productBusinessPartnerMapperResponse =
      new ProductBusinessPartnerMapperWebResponse();
  private List<ProductBusinessPartnerMapperWebResponse> productBusinessPartnerMapperResponseList = new ArrayList();
  private ListBaseResponse<ProductBusinessPartnerMapperWebResponse> responseGdnRestListResponse;
  private AssigneeWebResponse assigneeWebResponse = new AssigneeWebResponse();
  private List<AssigneeWebResponse> assigneeWebResponseList = new ArrayList();
  private VendorSummaryFilterWebRequest vendorSummaryFilterWebRequest = new VendorSummaryFilterWebRequest();
  private DistributionProductWebResponse distributionProductWebResponse = new DistributionProductWebResponse();
  private List<DistributionProductWebResponse> distributionProductWebResponseList = new ArrayList<>();
  private Page<DistributionProductWebResponse> productPage;
  private VendorSummaryFilterWebRequest vendorSummaryFilterWebRequestBuilder;
  private List<String> productCodes = new ArrayList<>();
  private RejectProductVendorRequest rejectProductVendorRequest;
  private BulkDeleteProductRequest bulkDeleteProductRequest;
  private VendorQuickApprovalRequest vendorQuickApprovalRequest;
  private VendorAutoConsignmentWebRequest vendorAutoConsignmentWebRequest;

  private ArgumentCaptor<RejectProductVendorRequest> argumentCaptor = ArgumentCaptor.forClass(RejectProductVendorRequest.class);

  @BeforeEach
  public void init() throws IOException {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(vendorController).build();

    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(clientParameterHelper.getVendorCode()).thenReturn(Constants.VENDOR_CODE);

    productWebRequestAsString = FileUtils.readFileToString(new File(Thread.currentThread()
        .getContextClassLoader()
        .getResource("VendorUpdateTestRequest.json")
        .getFile()), "UTF-8");

    primaryFilterWebRequest = PrimaryFilterWebRequest.builder().assignment(Boolean.TRUE).imagePending(Boolean.TRUE)
        .contentPending(Boolean.TRUE).vendorCode(Constants.VENDOR_CODE).build();
    productBusinessPartnerMapperResponse.builder().businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)
        .businessPartnerName(DEFAULT_BUSINESS_PARTNER_NAME).build();
    assigneeWebResponse.setAssigneeEmailId(DEFAULT_ASSIGNEE_NAME);
    assigneeWebResponseList.add(assigneeWebResponse);
    productBusinessPartnerMapperResponseList.add(productBusinessPartnerMapperResponse);
    responseGdnRestListResponse =
        new ListBaseResponse<ProductBusinessPartnerMapperWebResponse>(null, null, Boolean.TRUE,
            DEFAULT_REQUEST_ID, productBusinessPartnerMapperResponseList, null);

    vendorSummaryFilterWebRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    vendorSummaryFilterWebRequest.setAssigneeEmailId(DEFAULT_ASSIGNEE_NAME);
    distributionProductWebResponseList.add(distributionProductWebResponse);
    productPage = new PageImpl<>(distributionProductWebResponseList, PageRequest.of(PAGE, SIZE), SIZE);
    productCodes.add(PRODUCT_CODE);
    screeningProductBulkActionsWebRequest.setProductCodes(productCodes);
    screeningProductBulkActionsWebRequest.setAssignedBy(ASSIGNED_BY);
    screeningProductBulkActionsWebRequest.setAssignTo(DEFAULT_ASSIGNEE_NAME);

    com.gdn.x.mta.distributiontask.util.RejectReasonRequest rejectReasonNew =
        new com.gdn.x.mta.distributiontask.util.RejectReasonRequest();
    List<String> productNew = new ArrayList<>();
    productNew.add(PRODUCT);
    rejectReasonNew.setProduct(productNew);
    rejectProductVendorRequest = new RejectProductVendorRequest();
    rejectProductVendorRequest.setProductCode(PRODUCT_CODE);
    rejectProductVendorRequest.setRejectReasonRequest(rejectReasonNew);
    rejectProductVendorRequest.setNotes(NOTES);

    RejectReason rejectReason = new RejectReason();
    List<String> product = new ArrayList<>();
    product.add(PRODUCT);
    rejectReason.setProduct(product);

    RejectProductRequest rejectProductRequest = new RejectProductRequest();
    rejectProductRequest.setProductCode(PRODUCT_CODE);
    rejectProductRequest.setMerchantCommissionType(TYPE);

    bulkDeleteProductRequest = new BulkDeleteProductRequest();
    bulkDeleteProductRequest.setCodes(List.of(rejectProductRequest));
    bulkDeleteProductRequest.setNotes(NOTES);
    bulkDeleteProductRequest.setRejectReason(rejectReason);

    vendorSummaryFilterWebRequestBuilder = VendorSummaryFilterWebRequest.builder().keyword(KEYWORD)
        .timeFilterWebType(TIME_FILTER_TYPE).assignment(Boolean.TRUE)
        .imagePending(Boolean.TRUE).contentPending(Boolean.TRUE)
        .assigneeEmailId(ASSIGNEE_EMAIL_ID).categoryCode(CATEGORY_CODE)
        .businessPartnerCode(BUSINESS_PARTNER_CODE).isCnCategory(Boolean.TRUE)
        .sortOrder(SORT_ORDER_BY_CREATED_DATE).build();

    fileContent = new byte[]{-1, -40, -20, -10};

    vendorQuickApprovalRequest =
        VendorQuickApprovalRequest.builder().vendorCode(VENDOR_CODE).productCode(PRODUCT_CODE).Notes(NOTES).build();
    vendorAutoConsignmentWebRequest =
        vendorAutoConsignmentWebRequest.builder().defaultSettingsEnabled(true).vendorEmail(VENDOR_CODE)
            .vendorAutoAssignmentFilterWebRequest(new VendorAutoAssignmentFilterWebRequest())
            .assigneeList(new ArrayList<>()).build();
  }

  @Test
  public void getBusinessPartnerListTest() throws Exception{
    Mockito.when(this.vendorService.getBusinessPartnerList(PAGE, SIZE, primaryFilterWebRequest))
        .thenReturn(productBusinessPartnerMapperResponseList);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(clientParameterHelper.getVendorCode()).thenReturn(Constants.VENDOR_CODE);
    MockHttpServletRequestBuilder requestBuilder =
        post(VendorApiPath.BASE_PATH + VendorApiPath.BUSINESS_PARTNERS_LIST)
            .content(toJson(primaryFilterWebRequest))
            .param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE))
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.vendorService).getBusinessPartnerList(PAGE, SIZE, primaryFilterWebRequest);
    Mockito.verify(this.clientParameterHelper).getRequestId();
    Mockito.verify(this.clientParameterHelper).getVendorCode();
  }

  @Test
  public void getBusinessPartnerList_expectException() throws Exception {
    Mockito.when(this.vendorService.getBusinessPartnerList(PAGE, SIZE, primaryFilterWebRequest))
        .thenThrow(Exception.class);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(clientParameterHelper.getVendorCode()).thenReturn(Constants.VENDOR_CODE);
    try {
      MockHttpServletRequestBuilder requestBuilder =
          post(VendorApiPath.BASE_PATH + VendorApiPath.BUSINESS_PARTNERS_LIST).content(toJson(primaryFilterWebRequest))
              .param("page", String.valueOf(PAGE)).param("size", String.valueOf(SIZE))
              .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
      mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
    } finally {
      Mockito.verify(this.vendorService).getBusinessPartnerList(PAGE, SIZE, primaryFilterWebRequest);
      Mockito.verify(this.clientParameterHelper).getRequestId();
      Mockito.verify(this.clientParameterHelper).getVendorCode();

    }
  }

  @Test
  public void getAssigneeListTest() throws Exception{
    Mockito.when(this.vendorService.getAssigneeList(PAGE, SIZE, primaryFilterWebRequest))
        .thenReturn(assigneeWebResponseList);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(clientParameterHelper.getVendorCode()).thenReturn(Constants.VENDOR_CODE);
    MockHttpServletRequestBuilder requestBuilder =
        post(VendorApiPath.BASE_PATH + VendorApiPath.ASSIGNEE_LIST)
            .content(toJson(primaryFilterWebRequest))
            .param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE))
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.vendorService).getAssigneeList(PAGE, SIZE, primaryFilterWebRequest);
    Mockito.verify(this.clientParameterHelper).getRequestId();
    Mockito.verify(this.clientParameterHelper).getVendorCode();
  }

  @Test
  public void getAssigneeList_expectException() throws Exception {
    Mockito.when(this.vendorService.getAssigneeList(PAGE, SIZE, primaryFilterWebRequest)).thenThrow(Exception.class);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(clientParameterHelper.getVendorCode()).thenReturn(Constants.VENDOR_CODE);
    try {
      MockHttpServletRequestBuilder requestBuilder =
          post(VendorApiPath.BASE_PATH + VendorApiPath.ASSIGNEE_LIST).content(toJson(primaryFilterWebRequest))
              .param("page", String.valueOf(PAGE)).param("size", String.valueOf(SIZE))
              .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
      mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
    } finally {
      Mockito.verify(this.vendorService).getAssigneeList(PAGE, SIZE, primaryFilterWebRequest);
      Mockito.verify(this.clientParameterHelper).getRequestId();
      Mockito.verify(this.clientParameterHelper, times(1)).getVendorCode();

    }
  }

  @Test
  public void getProductHistoryTest() throws Exception {
    Mockito.when(vendorService.getProductHistory(PRODUCT_CODE, PAGE, SIZE))
        .thenReturn(new PageImpl<>(Arrays.asList(new ProductHistoryWebResponse())));
    MockHttpServletRequestBuilder requestBuilder =
        get(VendorApiPath.BASE_PATH + VendorApiPath.PRODUCT_HISTORY_FOR_VENDOR, PRODUCT_CODE)
            .param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE))
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(vendorService).getProductHistory(PRODUCT_CODE, PAGE, SIZE);
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void getproductDetailsByProductCodeTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    Mockito.when(vendorService.getProductDetailsByProductCode(REQUEST_ID, USER_NAME, PRODUCT_CODE))
        .thenReturn(new SingleBaseResponse<>("", null, true, REQUEST_ID, new ProductDetailWebResponse()));
    MockHttpServletRequestBuilder requestBuilder =
        get(VendorApiPath.BASE_PATH + VendorApiPath.GET_PRODUCT_DETAILS, PRODUCT_CODE)
            .param("requestId", REQUEST_ID)
            .param("userName", USER_NAME)
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(vendorService).getProductDetailsByProductCode(REQUEST_ID, USER_NAME, PRODUCT_CODE);
    Mockito.verify(this.clientParameterHelper).getRequestId();
    Mockito.verify(this.clientParameterHelper).getUsername();
  }

  @Test
  public void republishEditedProductTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    Mockito.doNothing().when(vendorService).republishEditedProduct(PRODUCT_CODE);
    MockHttpServletRequestBuilder requestBuilder =
        get(VendorApiPath.BASE_PATH + VendorApiPath.REPUBLISH_EDITED_PRODUCT, PRODUCT_CODE)
            .param("requestId", REQUEST_ID)
            .param("userName", USER_NAME)
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(vendorService).republishEditedProduct(PRODUCT_CODE);
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void updateVendorProductTest_Success() throws Exception {
    doNothing().when(vendorService).updateProduct(anyString(), anyString(),
        any(DistributionProductDetailRequest.class));
    MockHttpServletRequestBuilder requestBuilder =
        post(VendorApiPath.BASE_PATH + VendorApiPath.VENDOR_PRODUCT_UPDATE
            , VENDOR_TYPE_CONTENT)
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON)
            .content(productWebRequestAsString);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(vendorService).updateProduct(eq(VENDOR_TYPE_CONTENT), eq(VENDOR_CODE),
        any(DistributionProductDetailRequest.class));
    Mockito.verify(this.clientParameterHelper).getRequestId();
    Mockito.verify(this.clientParameterHelper).getStoreId();
    Mockito.verify(this.clientParameterHelper).getUsername();
    Mockito.verify(this.clientParameterHelper).getVendorCode();

  }

  @Test
  public void updateVendorProductTest_InvalidUserType() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(clientParameterHelper.getVendorCode()).thenReturn("");
    MockHttpServletRequestBuilder requestBuilder =
        post(VendorApiPath.BASE_PATH + VendorApiPath.VENDOR_PRODUCT_UPDATE
            , VENDOR_TYPE_CONTENT)
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON)
            .content(productWebRequestAsString);
    try {
      mockMvc.perform(requestBuilder);
    } catch (ServletException ex) {
      Assertions.assertEquals(ErrorCategory.VALIDATION, ((ApplicationRuntimeException)ex.getCause()).getErrorCodes());
      Mockito.verify(this.clientParameterHelper).getVendorCode();

    }
  }

  @Test
  public void productListTest() throws Exception {
    when(vendorService
        .getVendorProductList(eq(PAGE), eq(SIZE), any(VendorSummaryFilterWebRequest.class), eq(VENDOR_CODE)))
        .thenReturn(productPage);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(clientParameterHelper.getVendorCode()).thenReturn(Constants.VENDOR_CODE);
    MockHttpServletRequestBuilder requestBuilder =
        post(VendorApiPath.BASE_PATH + VendorApiPath.VENDOR_PRODUCT_LIST)
            .content(toJson(vendorSummaryFilterWebRequest))
            .param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE))
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(vendorService)
        .getVendorProductList(eq(PAGE), eq(SIZE), any(VendorSummaryFilterWebRequest.class), eq(VENDOR_CODE));
    Mockito.verify(this.clientParameterHelper).getRequestId();
    Mockito.verify(this.clientParameterHelper).getVendorCode();
  }

  @Test
  public void getProductFilterCountsTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(vendorService.getFilterCounts(VENDOR_CODE, null, null,
        null))
        .thenReturn(new MapResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(VendorApiPath.BASE_PATH + VendorApiPath.PRIMARY_FILTER).param("vendorCode", VENDOR_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(vendorService).getFilterCounts(VENDOR_CODE, null, null, null);
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void detectEditByMerchantEditedTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(vendorService.getEditedByMerchant(PRODUCT_CODE, Integer.parseInt(VERSION)))
        .thenReturn(true);
    MockHttpServletRequestBuilder requestBuilder =
        get(VendorApiPath.BASE_PATH + VendorApiPath.DETECT_EDIT_BY_MERCHANT, PRODUCT_CODE).param(
            "version",
            VERSION).contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder)
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", equalTo(true)));
    Mockito.verify(vendorService).getEditedByMerchant(PRODUCT_CODE, Integer.parseInt(VERSION));
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void detectEditByMerchantNotEditedTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(vendorService.getEditedByMerchant(PRODUCT_CODE, Integer.parseInt(VERSION)))
        .thenReturn(false);
    MockHttpServletRequestBuilder requestBuilder =
        get(VendorApiPath.BASE_PATH + VendorApiPath.DETECT_EDIT_BY_MERCHANT, PRODUCT_CODE).param(
            "version",
            VERSION).contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder)
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", equalTo(false)));
    Mockito.verify(vendorService).getEditedByMerchant(PRODUCT_CODE, Integer.parseInt(VERSION));
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void getProductReviewConfigCountsTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(vendorService.getReviewConfigProductCounts(VENDOR_CODE)).thenReturn(new MapResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(VendorApiPath.BASE_PATH + VendorApiPath.PRODUCT_REVIEW_CONFIG_COUNTS)
            .param("vendorCode", VENDOR_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(vendorService).getReviewConfigProductCounts(VENDOR_CODE);
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void vendorProductActionsTest() throws Exception{
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.doNothing().when(vendorService).vendorProductActions(ACTION, screeningProductBulkActionsWebRequest);
    MockHttpServletRequestBuilder requestBuilder =
        post(VendorApiPath.BASE_PATH + VendorApiPath.VENDOR_PRODUCT_ASSIGN_UNASSIGN, ACTION, TYPE)
            .content(toJson(screeningProductBulkActionsWebRequest))
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(vendorService).vendorProductActions(ACTION, screeningProductBulkActionsWebRequest);
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void approveVendorProductTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(clientParameterHelper.getVendorCode()).thenReturn(Constants.VENDOR_CODE);
    MockHttpServletRequestBuilder requestBuilder =
        post(VendorApiPath.BASE_PATH + VendorApiPath.VENDOR_PRODUCT_APPROVE).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON)
            .content(productWebRequestAsString);
    try {
      mockMvc.perform(requestBuilder);
    } catch (ServletException ex) {
      Assertions.assertEquals(ErrorCategory.VALIDATION, ((ApplicationRuntimeException) ex.getCause()).getErrorCodes());
      Mockito.verify(this.clientParameterHelper).getVendorCode();
    }
  }

  @Test
  public void vendorRejectProductsTest() throws Exception {
    RejectProductResponse response = new RejectProductResponse();
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(vendorService.rejectProductByVendor(VENDOR_CODE, bulkDeleteProductRequest))
        .thenReturn(Collections.singletonList(response));
    MockHttpServletRequestBuilder requestBuilder =
        post(VendorApiPath.BASE_PATH + VendorApiPath.VENDOR_BULK_REJECT).content(
                toJson(bulkDeleteProductRequest)).param("vendorCode", VENDOR_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(vendorService).rejectProductByVendor(VENDOR_CODE, bulkDeleteProductRequest);
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void vendorRejectProductsNullTest() throws Exception {
    bulkDeleteProductRequest.setCodes(new ArrayList<>());
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(VendorApiPath.BASE_PATH + VendorApiPath.VENDOR_BULK_REJECT).content(
                toJson(bulkDeleteProductRequest)).param("vendorCode", VENDOR_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    try {
      mockMvc.perform(requestBuilder);
    } catch (ServletException ex) {
      Assertions.assertEquals(ErrorCategory.VALIDATION, ((ApplicationRuntimeException) ex.getCause()).getErrorCodes());
    }
  }

  @Test
  public void productCorrectionActionTest() throws Exception {
    ScreeningProductBulkActionsWebRequest screeningProductBulkActionsWebRequest =
        ScreeningProductBulkActionsWebRequest.builder().build();
    MockHttpServletRequestBuilder requestBuilder =
        post(VendorApiPath.BASE_PATH + VendorApiPath.PRODUCT_CORRECTION_ACTION).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).content(toJson(screeningProductBulkActionsWebRequest));
    when(vendorService.doProductNeedCorrection(VENDOR_CODE, screeningProductBulkActionsWebRequest))
        .thenReturn(new NeedRevisionWebResponse(true, true, true));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.clientParameterHelper).getRequestId();
    Mockito.verify(this.clientParameterHelper).getVendorCode();
    verify(vendorService).doProductNeedCorrection(VENDOR_CODE, screeningProductBulkActionsWebRequest);
  }

  @Test
  public void bulkDownloadFilteredVendorProductsTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(clientParameterHelper.getVendorCode()).thenReturn(Constants.VENDOR_CODE);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    Mockito.doNothing().when(vendorService)
        .bulkDownloadFilteredVendorProducts(USER_NAME, VENDOR_CODE, vendorSummaryFilterWebRequestBuilder);
    MockHttpServletRequestBuilder requestBuilder =
        post(VendorApiPath.BASE_PATH + VendorApiPath.DOWNLOAD_FILTERED_PRODUCT_VENDOR)
            .content(toJson(vendorSummaryFilterWebRequestBuilder)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.vendorService)
        .bulkDownloadFilteredVendorProducts(USER_NAME, VENDOR_CODE, vendorSummaryFilterWebRequestBuilder);
    Mockito.verify(this.clientParameterHelper).getRequestId();
    Mockito.verify(this.clientParameterHelper).getVendorCode();
    Mockito.verify(this.clientParameterHelper).getUsername();
  }

  @Test
  public void getScreeningNotesTest() throws Exception {
    Mockito.when(vendorService.getProductScreeningNotes(PRODUCT_CODE))
        .thenReturn(NOTES);
    MockHttpServletRequestBuilder requestBuilder =
        get(VendorApiPath.BASE_PATH + VendorApiPath.PRODUCT_SCREENING_NOTES, PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(vendorService).getProductScreeningNotes(PRODUCT_CODE);
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void sendProductBackToVendorTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(VendorApiPath.BASE_PATH + VendorApiPath.SEND_PRODUCT_BACK_TO_VENDOR, PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(vendorService).sendProductBackToVendor(PRODUCT_CODE);
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void vendorProductsBulkAssignTest() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    Mockito.when(clientParameterHelper.getVendorCode()).thenReturn(Constants.VENDOR_CODE);
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    Mockito.doNothing().when(vendorService)
        .saveBulkAssignFile(STORE_ID, Constants.VENDOR_CODE, USER_NAME, REQUEST_ID, multipartFile);
    this.mockMvc.perform(MockMvcRequestBuilders.multipart(VendorApiPath.BASE_PATH + VendorApiPath.BULK_PRODUCT_ASSIGN)
            .file(multipartFile).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    Mockito.verify(vendorService)
        .saveBulkAssignFile(STORE_ID, Constants.VENDOR_CODE, USER_NAME, REQUEST_ID, multipartFile);
    Mockito.verify(this.clientParameterHelper).getRequestId();
    Mockito.verify(this.clientParameterHelper).getUsername();
    Mockito.verify(this.clientParameterHelper).getVendorCode();
    Mockito.verify(this.clientParameterHelper).getStoreId();
  }

  @Test
  public void vendorProductsBulkAssignExceptionTest() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(PATH + FILE)));
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    Mockito.when(clientParameterHelper.getVendorCode()).thenReturn(Constants.VENDOR_CODE);
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    Mockito.doThrow(IOException.class).when(vendorService)
        .saveBulkAssignFile(STORE_ID, Constants.VENDOR_CODE, USER_NAME, REQUEST_ID, multipartFile);
    this.mockMvc.perform(MockMvcRequestBuilders.multipart(VendorApiPath.BASE_PATH + VendorApiPath.BULK_PRODUCT_ASSIGN)
            .file(multipartFile).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    Mockito.verify(vendorService)
        .saveBulkAssignFile(STORE_ID, Constants.VENDOR_CODE, USER_NAME, REQUEST_ID, multipartFile);
    Mockito.verify(this.clientParameterHelper).getRequestId();
    Mockito.verify(this.clientParameterHelper).getVendorCode();
    Mockito.verify(this.clientParameterHelper).getUsername();
    Mockito.verify(this.clientParameterHelper).getStoreId();

  }

  @Test
  public void getVendorListTest() throws Exception {
    Mockito.when(vendorService.getVendorList()).thenReturn(Arrays.asList(new VendorDetailWebResponse()));
    MockHttpServletRequestBuilder requestBuilder =
        get(VendorApiPath.BASE_PATH + VendorApiPath.VENDORS_LIST).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(vendorService).getVendorList();
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void getProductImageFeedbackTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(VendorApiPath.BASE_PATH + VendorApiPath.GET_IMAGE_FEEDBACK, PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(vendorService).getProductImageQcFeedback(PRODUCT_CODE);
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void updateProductImageFeedbackTest() throws Exception {
    ProductImageQcWebRequest productImageQcWebRequest = new ProductImageQcWebRequest();
    MockHttpServletRequestBuilder requestBuilder =
        put(VendorApiPath.BASE_PATH + VendorApiPath.UPDATE_IMAGE_FEEDBACK)
            .content(toJson(productImageQcWebRequest))
            .param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE))
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(vendorService).updateProductImageQcFeedback(productImageQcWebRequest);
    verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void updateProductImageFeedbackExceptionTest() throws Exception {
    ProductImageQcWebRequest productImageQcWebRequest = new ProductImageQcWebRequest();
    Mockito.doThrow(new ApplicationRuntimeException()).when(vendorService)
        .updateProductImageQcFeedback(productImageQcWebRequest);
    MockHttpServletRequestBuilder requestBuilder =
        put(VendorApiPath.BASE_PATH + VendorApiPath.UPDATE_IMAGE_FEEDBACK)
            .content(toJson(productImageQcWebRequest))
            .param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE))
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(vendorService).updateProductImageQcFeedback(productImageQcWebRequest);
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void getDifferentFaultyTypeTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(VendorApiPath.BASE_PATH + VendorApiPath.GET_IMAGE_FAULTY_TYPE).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(vendorService).getDifferentFaultyType();
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void reindexProductToSolrTest() throws Exception {
    Mockito.when(this.vendorService.reindexProductToSolr(PRODUCT_CODE)).thenReturn(true);
    MockHttpServletRequestBuilder requestBuilder =
        get(VendorApiPath.BASE_PATH + VendorApiPath.REINDEX_PRODUCT, PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(vendorService).reindexProductToSolr(PRODUCT_CODE);
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void checkPendingUploadTest() throws Exception {
    Mockito.when(
        this.vendorService.checkCountOfUploads(Constants.VENDOR_BULK_ASSIGN, Constants.BULK_PROCESS_STATE_PENDING))
        .thenReturn(new BulkUpdatePendingWebResposne(false, 1));
    MockHttpServletRequestBuilder requestBuilder = get(VendorApiPath.BASE_PATH + VendorApiPath.PENDING_UPLOAD)
        .param("bulkProcessType", Constants.VENDOR_BULK_ASSIGN).param("status", Constants.BULK_PROCESS_STATE_PENDING)
        .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(vendorService).checkCountOfUploads(Constants.VENDOR_BULK_ASSIGN, Constants.BULK_PROCESS_STATE_PENDING);
    verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void getReviewConfigCountTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(vendorService.getReviewProductCountsForConfig(VENDOR_CODE, Boolean.FALSE))
        .thenReturn(new MapResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(VendorApiPath.BASE_PATH + VendorApiPath.GET_REVIEW_CONFIG_COUNT).param("vendorCode", VENDOR_CODE)
            .param("postLive", "false").contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(vendorService).getReviewProductCountsForConfig(VENDOR_CODE, Boolean.FALSE);
    verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void getVendorDetailTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(vendorService.getVendorDetail(VENDOR_CODE)).thenReturn(new VendorDetailResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(VendorApiPath.BASE_PATH + VendorApiPath.VENDOR_DETAIL, VENDOR_CODE).contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(vendorService).getVendorDetail(VENDOR_CODE);
    verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void vendorProductQuickApprovalTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(vendorService.vendorProductQuickApproval(any(VendorQuickApprovalRequest.class)))
        .thenReturn(new VendorQuickApprovalResponse());
    MockHttpServletRequestBuilder requestBuilder =
        post(VendorApiPath.BASE_PATH + VendorApiPath.QUICK_PRODUCT_APPROVAL).content(toJson(vendorQuickApprovalRequest))
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(vendorService).vendorProductQuickApproval(any(VendorQuickApprovalRequest.class));
    verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void vendorProductQuickApprovalFalseTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(vendorService.vendorProductQuickApproval(any(VendorQuickApprovalRequest.class)))
        .thenReturn(VendorQuickApprovalResponse.builder().errorCodes(Arrays.asList("error")).build());
    MockHttpServletRequestBuilder requestBuilder =
        post(VendorApiPath.BASE_PATH + VendorApiPath.QUICK_PRODUCT_APPROVAL).content(toJson(vendorQuickApprovalRequest))
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(vendorService).vendorProductQuickApproval(any(VendorQuickApprovalRequest.class));
    verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void getProductReviewersTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(vendorService.getProductReviewers()).thenReturn(new ArrayList<>());
    MockHttpServletRequestBuilder requestBuilder =
        get(VendorApiPath.BASE_PATH + VendorApiPath.PRODUCT_REVIEWERS).contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(vendorService).getProductReviewers();
    verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void autoAssignProductsTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    Mockito.when(clientParameterHelper.getVendorCode()).thenReturn(Constants.VENDOR_CODE);
    MockHttpServletRequestBuilder requestBuilder =
        post(VendorApiPath.BASE_PATH + VendorApiPath.AUTO_ASSIGN_PRODUCTS).content(
                toJson(vendorAutoConsignmentWebRequest))
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(vendorService).autoAssignProducts(any(VendorAutoConsignmentWebRequest.class), eq(STORE_ID), eq(VENDOR_CODE));
    verify(this.clientParameterHelper).getRequestId();
    verify(this.clientParameterHelper).getUsername();
    verify(this.clientParameterHelper).getStoreId();
    verify(this.clientParameterHelper).getVendorCode();
  }

  @Test
  public void autoAssignProductsExceptionTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    Mockito.when(clientParameterHelper.getVendorCode()).thenReturn(Constants.VENDOR_CODE);
    Mockito.doThrow(ClientException.class).when(vendorService)
        .autoAssignProducts(any(VendorAutoConsignmentWebRequest.class), anyString(),
          eq(VENDOR_CODE));
    try {
      MockHttpServletRequestBuilder requestBuilder =
          post(VendorApiPath.BASE_PATH + VendorApiPath.AUTO_ASSIGN_PRODUCTS).content(
                  toJson(vendorAutoConsignmentWebRequest)).contentType(MediaType.APPLICATION_JSON)
              .accept(MediaType.APPLICATION_JSON);
      mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    } catch (Exception e) {
    } finally {
      Mockito.verify(vendorService).autoAssignProducts(any(VendorAutoConsignmentWebRequest.class), anyString(),
          anyString());
      verify(this.clientParameterHelper).getRequestId();
      verify(this.clientParameterHelper).getStoreId();
      verify(this.clientParameterHelper).getUsername();
      verify(this.clientParameterHelper).getVendorCode();
    }
  }

  @Test
  public void getDefaultSettingTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(vendorService.getDefaultSetting(anyString()))
            .thenReturn(new VendorDefaultFilterResponse());
    MockHttpServletRequestBuilder requestBuilder =
            get(VendorApiPath.BASE_PATH + VendorApiPath.GET_DEFAULT_SETTING).content(toJson(vendorQuickApprovalRequest))
                    .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON).param("vendorEmail", VENDOR_EMAIL);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(vendorService).getDefaultSetting(anyString());
    verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void getDefaultSettingExceptionTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.doThrow(ClientException.class).when(vendorService).getDefaultSetting(anyString());
    MockHttpServletRequestBuilder requestBuilder =
            get(VendorApiPath.BASE_PATH + VendorApiPath.GET_DEFAULT_SETTING).content(toJson(vendorQuickApprovalRequest))
                    .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON).param("vendorEmail", VENDOR_EMAIL);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(vendorService).getDefaultSetting(anyString());
    verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  public void checkPendingAssignmentsTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    Mockito.doThrow(ClientException.class).when(vendorService)
            .checkPendingAssignments(anyString(), anyString(), anyString());
    MockHttpServletRequestBuilder requestBuilder =
            get(VendorApiPath.BASE_PATH + VendorApiPath.CHECK_PENDING_ASSIGNMENTS).content(toJson(vendorQuickApprovalRequest))
                    .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON).param("processType", PROCESS_TYPE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(vendorService).checkPendingAssignments(anyString(), anyString(), anyString());
    verify(this.clientParameterHelper).getRequestId();
    verify(this.clientParameterHelper).getUsername();
    verify(this.clientParameterHelper).getStoreId();
  }

  @Test
  public void checkPendingAssignmentsExceptionTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    MockHttpServletRequestBuilder requestBuilder =
            get(VendorApiPath.BASE_PATH + VendorApiPath.CHECK_PENDING_ASSIGNMENTS).content(toJson(vendorQuickApprovalRequest))
                    .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON).param("processType", PROCESS_TYPE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(vendorService).checkPendingAssignments(anyString(), anyString(), anyString());
    verify(this.clientParameterHelper).getRequestId();
    verify(this.clientParameterHelper).getUsername();
    verify(this.clientParameterHelper).getStoreId();
  }

  @Test
  public void bulkReviewUploadTets() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
      FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    this.mockMvc.perform(
        MockMvcRequestBuilders.multipart(VendorApiPath.BASE_PATH + VendorApiPath.BULK_REVIEW_UPLOAD,
          BULK_APPROVAL).file(multipartFile).accept(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    Mockito.verify(vendorService)
      .saveBulkReviewFile(multipartFile, BULK_APPROVAL, Constants.REQUEST_ID, Constants.STORE_ID,
        Constants.USER_NAME);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getStoreId();
    verify(clientParameterHelper).getUsername();
  }

  @Test
  public void bulkReviewUploadTetsInvalidType() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
      FileUtils.readFileToByteArray(new File(PATH + FILE)));
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    try {
      this.mockMvc.perform(MockMvcRequestBuilders.multipart(
            VendorApiPath.BASE_PATH + VendorApiPath.BULK_REVIEW_UPLOAD, REQUEST_ID).file(multipartFile)
          .accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)));
    } catch (Exception ex){
      Assertions.assertNotNull(ex);
    } finally {
      verify(clientParameterHelper).getRequestId();
    }
  }

  @Test
  public void bulkReviewUpload_ExceptionTest() throws Exception {
    mockFile(PATH + FILE);
    multipartFile = new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
      FileUtils.readFileToByteArray(new File(PATH + FILE)));
    Mockito.doThrow(IOException.class).when(vendorService)
      .saveBulkReviewFile(multipartFile, BULK_REJECTION, Constants.REQUEST_ID, Constants.STORE_ID,
        Constants.USER_NAME);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    when(clientParameterHelper.getStoreId()).thenReturn(Constants.STORE_ID);
    this.mockMvc.perform(
        MockMvcRequestBuilders.multipart(VendorApiPath.BASE_PATH + VendorApiPath.BULK_REVIEW_UPLOAD,
          BULK_REJECTION).file(multipartFile).accept(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)));
    Mockito.verify(vendorService)
      .saveBulkReviewFile(multipartFile, BULK_REJECTION, Constants.REQUEST_ID, Constants.STORE_ID,
        Constants.USER_NAME);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();
    verify(clientParameterHelper).getStoreId();
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, fileContent);
  }

  @AfterEach
  public void tearDown() throws IOException {
    Mockito.verifyNoMoreInteractions(vendorService);
    Mockito.verifyNoMoreInteractions(clientParameterHelper);
    FileUtils.deleteDirectory(new File(PATH));
  }
}
