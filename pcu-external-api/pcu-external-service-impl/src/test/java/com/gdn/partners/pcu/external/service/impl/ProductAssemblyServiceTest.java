package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.pcu.external.client.feign.PCBFeign;
import com.gdn.partners.pcu.external.client.feign.ProductAssemblyFeign;
import com.gdn.partners.pcu.external.client.feign.XProductFeign;
import com.gdn.partners.pcu.external.model.Accessibilty;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.service.BusinessPartnerService;
import com.gdn.partners.pcu.external.web.model.request.AssemblyDisAssemblyListingRequest;
import com.gdn.partners.pcu.external.web.model.request.AssemblyDisassemblyRequest;
import com.gdn.partners.pcu.external.web.model.request.SimpleListAssemblyDisassemblyRequest;
import com.gdn.partners.pcu.external.web.model.request.TransferRequest;
import com.gdn.partners.pcu.external.web.model.response.HistoryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.MasterWarehouseListWebResponse;
import com.gdn.partners.pcu.external.web.model.response.RequestFormResponse;
import com.gdn.partners.pcu.external.web.model.response.RequestFormWebResponse;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.response.BundleRecipeV2Response;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Arrays;

import static org.mockito.ArgumentMatchers.any;

public class ProductAssemblyServiceTest {

  private static final String PAGE = "1";
  private static final String LIMIT = "500";
  private static final int PAGE_NUMBER = 0;
  private static final int PAGE_SIZE = 50;
  private static final String REQUEST_FORM_NUMBER = "1";
  private static final String ITEM_SKU = "1";
  private static final String ITEM_NAME = "1";
  private static final String CATEGORY_CODE = "1";
  private static final String WAREHOUSE_CODE = "1";
  private static final String STATUS = "1";
  private static final String SOURCE_ITEM_SKU = "1";
  private static final String SOURCE_ITEM_NAME = "1";
  private static final String CATEGORY_NAME = "1";
  private static final String ACTIVITY = "ACTIVITY";
  private static final String NOTES = "notes";
  private static final String REQUEST_ID = "requestId";
  private static final String ASSEMBLY_REQUEST = "assemblyReq";
  private static final String MERCHANT_TYPE_TD = "TD";
  private static final String MERCHANT_TYPE_CM = "CM";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String RETRY_TYPE = "RETRY";
  private static final String USERNAME = "USERNAME";
  private static final String SORT_ORDER = "ASC";


  @InjectMocks
  private ProductAssemblyServiceImpl productAssemblyService;

  @Mock
  private ProductAssemblyFeign productAssemblyFeign;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @Mock
  private PCBFeign pcbFeign;

  @Mock
  private XProductFeign xProductFeign;

  private SimpleListAssemblyDisassemblyRequest simpleListAssemblyDisassemblyRequest;
  private AssemblyDisassemblyRequest assemblyDisassemblyRequest;
  private TransferRequest transferRequest;
  private ProfileResponse profileResponse;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(productAssemblyService, "page", PAGE);
    ReflectionTestUtils.setField(productAssemblyService, "limit", LIMIT);
    simpleListAssemblyDisassemblyRequest = new SimpleListAssemblyDisassemblyRequest();
    assemblyDisassemblyRequest = new AssemblyDisassemblyRequest();
    transferRequest = new TransferRequest();
    simpleListAssemblyDisassemblyRequest.getValue().add(assemblyDisassemblyRequest);
    profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    profileResponse.getCompany().setMerchantType(MERCHANT_TYPE_TD);
  }

  @AfterEach
  public void after() throws Exception {
    Mockito.verifyNoMoreInteractions(productAssemblyFeign, pcbFeign, businessPartnerService);
  }

  @Test
  public void getMasterWarehouseListResponseTest() {
    Mockito.when(productAssemblyFeign.getWarehouseCodeAndFulfillmentCenter(PAGE, LIMIT))
        .thenReturn(new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(1, 0, 1), null));
    GdnRestListResponse<MasterWarehouseListWebResponse> response =
        productAssemblyService.getMasterWarehouseListResponse();
    Mockito.verify(productAssemblyFeign).getWarehouseCodeAndFulfillmentCenter(PAGE, LIMIT);
    Assertions.assertNotNull(response.getContent());
  }

  @Test
  public void getRequestFormsListingResponseWithEmptyFeignResponseTest() {
    Mockito.when(productAssemblyFeign.getListingResponse(PAGE_NUMBER, PAGE_SIZE, new AssemblyDisAssemblyListingRequest()))
        .thenReturn(new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(1, 0, 1), null));
    GdnRestListResponse<RequestFormWebResponse> response =
        productAssemblyService.getRequestFormsListingResponse(PAGE_NUMBER, PAGE_SIZE, null, new AssemblyDisAssemblyListingRequest());
    Mockito.verify(productAssemblyFeign)
        .getListingResponse(Mockito.anyInt(), Mockito.anyInt(), any());
  }

  @Test
  public void getRequestFormsListingResponseTest() {
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setName(CATEGORY_NAME);
    GdnRestListResponse<CategoryResponse> categoryResponseGdnRestListResponse = new GdnRestListResponse<>();
    categoryResponseGdnRestListResponse.setSuccess(true);
    categoryResponseGdnRestListResponse.setContent(Arrays.asList(categoryResponse));
    Mockito.when(productAssemblyFeign.getListingResponse(PAGE_NUMBER, PAGE_SIZE, new AssemblyDisAssemblyListingRequest())).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(
            new RequestFormResponse(REQUEST_FORM_NUMBER, ITEM_SKU, ITEM_NAME, CATEGORY_CODE, WAREHOUSE_CODE, null, null,
                STATUS, SOURCE_ITEM_SKU, SOURCE_ITEM_NAME, ITEM_SKU, ITEM_SKU, 10, true)), new PageMetaData(1, 0, 1), null));
    Mockito.when(pcbFeign.filterCategoryHierarchyByCategoryCode(CATEGORY_CODE))
        .thenReturn(categoryResponseGdnRestListResponse);
    Mockito.when(xProductFeign.getItemBasicDetails(Mockito.anyBoolean(), Mockito.any())).thenReturn(
        new GdnRestListResponse<>(null, null, true, Arrays.asList(ItemBasicDetailV2Response.builder().itemSku(ITEM_SKU)
                .bundleRecipeList(Arrays.asList(new BundleRecipeV2Response())).build(),
            ItemBasicDetailV2Response.builder().itemSku(ITEM_SKU)
                .bundleRecipeList(Arrays.asList(new BundleRecipeV2Response())).build(),
            ItemBasicDetailV2Response.builder().itemSku(ITEM_SKU).bundleRecipeList(new ArrayList<>()).build()),
            new PageMetaData(0, 0, 0), REQUEST_ID));
    GdnRestListResponse<RequestFormWebResponse> response =
        productAssemblyService.getRequestFormsListingResponse(PAGE_NUMBER, PAGE_SIZE, null, new AssemblyDisAssemblyListingRequest());
    Mockito.verify(productAssemblyFeign).getListingResponse(Mockito.anyInt(), Mockito.anyInt(), any());
    Mockito.verify(pcbFeign).filterCategoryHierarchyByCategoryCode(CATEGORY_CODE);
    Mockito.verify(xProductFeign).getItemBasicDetails(Mockito.anyBoolean(), Mockito.any());
  }

  @Test
  public void getRequestFormsListingResponseEmptyCategoryNameTest() {
    CategoryResponse categoryResponse = new CategoryResponse();
    GdnRestListResponse<CategoryResponse> categoryResponseGdnRestListResponse = new GdnRestListResponse<>();
    categoryResponseGdnRestListResponse.setSuccess(true);
    categoryResponseGdnRestListResponse.setContent(Arrays.asList(categoryResponse));
    Mockito.when(productAssemblyFeign.getListingResponse(PAGE_NUMBER, PAGE_SIZE, new AssemblyDisAssemblyListingRequest())).thenReturn(
        new GdnRestListResponse<>(Arrays.asList(
            new RequestFormResponse(REQUEST_FORM_NUMBER, ITEM_SKU, ITEM_NAME, CATEGORY_CODE, WAREHOUSE_CODE, null, null,
                STATUS, SOURCE_ITEM_SKU, SOURCE_ITEM_NAME, ITEM_SKU, ITEM_SKU, 0, true)), new PageMetaData(1, 0, 1), null));
    Mockito.when(pcbFeign.filterCategoryHierarchyByCategoryCode(CATEGORY_CODE))
        .thenReturn(categoryResponseGdnRestListResponse);
    GdnRestListResponse<RequestFormWebResponse> response =
        productAssemblyService.getRequestFormsListingResponse(PAGE_NUMBER, PAGE_SIZE, null, new AssemblyDisAssemblyListingRequest());
    Mockito.verify(productAssemblyFeign).getListingResponse(Mockito.anyInt(), Mockito.anyInt(), any());
    Mockito.verify(pcbFeign).filterCategoryHierarchyByCategoryCode(CATEGORY_CODE);
    Assertions.assertTrue(response.getContent().get(0).isPhysicalBundle());
  }

  @Test
  public void getRequestFormsBusinessPartnerAutorizationCheckTest() {
    try {
      Credential.setAccessibilities(new String[] {Accessibilty.ASSEMBLY_REQUEST_LISTING});
      ReflectionTestUtils.setField(productAssemblyService, "validateBusinessPartnerCodeForSecurityEnabled", true);
      GdnRestListResponse<RequestFormWebResponse> response =
          productAssemblyService.getRequestFormsListingResponse(PAGE_NUMBER, PAGE_SIZE, null,
              new AssemblyDisAssemblyListingRequest());
    } catch (ApplicationRuntimeException applicationRuntimeException) {
      Assertions.assertEquals("Unauthorize access :You are not authorized", applicationRuntimeException.getErrorMessage());
    }
  }

  @Test
  public void getRequestFormsBusinessPartnerAutorizationCheck1Test() {
    try {
      Credential.setAccessibilities(new String[] {Accessibilty.ASSEMBLY_REQUEST_LISTING});
      AssemblyDisAssemblyListingRequest assemblyDisAssemblyListingRequest = new AssemblyDisAssemblyListingRequest();
      assemblyDisAssemblyListingRequest.setType("ASSEMBLY-REQUEST");
      ReflectionTestUtils.setField(productAssemblyService, "validateBusinessPartnerCodeForSecurityEnabled", true);
      GdnRestListResponse<RequestFormWebResponse> response =
          productAssemblyService.getRequestFormsListingResponse(PAGE_NUMBER, PAGE_SIZE, null,
              assemblyDisAssemblyListingRequest);
    } catch (ApplicationRuntimeException applicationRuntimeException) {
      Assertions.assertEquals("Unauthorize access :You are not authorized", applicationRuntimeException.getErrorMessage());
    }
  }

  @Test
  public void getRequestHistoryTest(){
    HistoryWebResponse webResponse = new HistoryWebResponse(ACTIVITY, NOTES);
    GdnRestListResponse<HistoryWebResponse> response =
        new GdnRestListResponse(Arrays.asList(webResponse), new PageMetaData(PAGE_SIZE, PAGE_NUMBER, 1), REQUEST_ID);
    Mockito.when(productAssemblyFeign.getHistory(PAGE_NUMBER, PAGE_SIZE, SORT_ORDER, REQUEST_FORM_NUMBER, "")).thenReturn(response);
    productAssemblyService.getRequestHistory(REQUEST_FORM_NUMBER, PAGE_NUMBER, PAGE_SIZE, SORT_ORDER, REQUEST_ID, "");
    Mockito.verify(productAssemblyFeign).getHistory(PAGE_NUMBER, PAGE_SIZE, SORT_ORDER, REQUEST_FORM_NUMBER, "");
  }

  @Test
  public void createAssemblyDisAssemblyAndTransferRequestsTest(){
    ReflectionTestUtils.setField(productAssemblyService, "bundlingAllowedSellerType", MERCHANT_TYPE_TD);
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(productAssemblyFeign.transferRequest(any()))
        .thenReturn(new GdnBaseRestResponse(null, null, true, null));
    productAssemblyService.createAssemblyDisAssemblyAndTransferRequests(BUSINESS_PARTNER_CODE,
        Constants.TRANSFER_REQUEST, simpleListAssemblyDisassemblyRequest);
    Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(productAssemblyFeign).transferRequest(any());
  }

  @Test
  public void createAssemblyDisAssemblyAndTransferRequestsAssemblyDisassemblyRequestTest(){
    ReflectionTestUtils.setField(productAssemblyService, "bundlingAllowedSellerType", MERCHANT_TYPE_TD);
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    Mockito.when(productAssemblyFeign.assemblyDisassemblyRequest(ASSEMBLY_REQUEST,
        simpleListAssemblyDisassemblyRequest)).thenReturn(new GdnBaseRestResponse(null, null, true, null));
    productAssemblyService.createAssemblyDisAssemblyAndTransferRequests(BUSINESS_PARTNER_CODE,
        ASSEMBLY_REQUEST, simpleListAssemblyDisassemblyRequest);
    Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    Mockito.verify(productAssemblyFeign).assemblyDisassemblyRequest(ASSEMBLY_REQUEST, simpleListAssemblyDisassemblyRequest);
  }

  @Test
  public void createAssemblyDisAssemblyAndTransferRequestsEmptyRequestTest() {
    ReflectionTestUtils.setField(productAssemblyService, "bundlingAllowedSellerType", MERCHANT_TYPE_TD);
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE)).thenReturn(profileResponse);
    productAssemblyService.createAssemblyDisAssemblyAndTransferRequests(BUSINESS_PARTNER_CODE,
        ASSEMBLY_REQUEST, new SimpleListAssemblyDisassemblyRequest());
    Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
  }

  @Test
  void createAssemblyDisAssemblyAndTransferRequestsInCorrectSellerRequestTest() {
    ReflectionTestUtils.setField(productAssemblyService, "bundlingAllowedSellerType",
        MERCHANT_TYPE_TD);
    profileResponse.getCompany().setMerchantType(MERCHANT_TYPE_CM);
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(profileResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> productAssemblyService.createAssemblyDisAssemblyAndTransferRequests(
              BUSINESS_PARTNER_CODE, ASSEMBLY_REQUEST, new SimpleListAssemblyDisassemblyRequest()));
    } finally {
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  void createAssemblyDisAssemblyAndTransferRequestsInCorrectProfileResponseTest() {
    ReflectionTestUtils.setField(productAssemblyService, "bundlingAllowedSellerType",
        MERCHANT_TYPE_TD);
    Mockito.when(businessPartnerService.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> productAssemblyService.createAssemblyDisAssemblyAndTransferRequests(
              BUSINESS_PARTNER_CODE, ASSEMBLY_REQUEST, new SimpleListAssemblyDisassemblyRequest()));
    } finally {
      Mockito.verify(businessPartnerService).filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  public void cancelOrRetryTest() {
    GdnBaseRestResponse response = new GdnBaseRestResponse(null, null, true, REQUEST_ID);
    Mockito.when(productAssemblyFeign.cancelOrRetryRequestForm(REQUEST_FORM_NUMBER, RETRY_TYPE, "", REQUEST_ID))
        .thenReturn(response);
    productAssemblyService.cancelOrRetry(REQUEST_FORM_NUMBER, RETRY_TYPE, "", REQUEST_ID);
    Mockito.verify(productAssemblyFeign).cancelOrRetryRequestForm(REQUEST_FORM_NUMBER, RETRY_TYPE, "", REQUEST_ID);
  }

}
