package com.gdn.mta.bulk.repository.download;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.gdn.mta.bulk.models.download.responsedata.DistributionProductResponse;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.bulk.feignConfig.PDTFeign;
import com.gdn.mta.bulk.models.BrandReport;
import com.gdn.mta.bulk.models.IprActionRequest;
import com.gdn.mta.bulk.models.download.IPRProductListRequest;
import com.gdn.mta.bulk.models.download.responsedata.IprProductsResponse;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.mta.distributiontask.model.enums.ProductStateIPR;
import com.gdn.x.mta.distributiontask.response.ProductCodeResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.BoostedProductFilterRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorQuickApprovalRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorQuickApprovalResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.mta.distributiontask.rest.model.request.BulkVendorProductActionsRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductListRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.BulkVendorProductActionsResponse;

import static org.mockito.ArgumentMatchers.eq;

public class ProductDistributionTaskRepositoryImplTest {

  private static final String REQUEST_ID = "request-id";
  private static final String USERNAME = "username";
  private static final String CONTENT_ASSIGN = "content";
  private static final Integer PAGE = 0;
  private static final Integer SIZE = 1;

  private static final String PRODUCT_CODE = "MTA-1234";

  private static final String STORE_ID = "store-id";
  private static final String STORE_ID_VAL = "10001";
  private static final String API = "api";
  private static final String XBULK = "x-bulk";

  private static final String VENDOR_CODE = "vendor-code";
  private static final String PRODUCT_SKU = "productSku";
  private static final String  SOURCE = "SOURCE";
  private static final String  ASSIGNEE = "ASSIGNEE";
  private static final String CHANNEL_ID = "CHANNEL_ID";
  private static final String CLIENT_ID = "CLIENT_ID";

  private Pageable pageable = PageRequest.of(PAGE, SIZE);

  private ProductListRequest productListRequest;

  private FilterSummaryRequest filterSummaryRequest;
  private BulkVendorProductActionsRequest bulkVendorProductActionsRequest;

  private GdnRestListResponse<DistributionProductResponse> productResponseGdnRestListResponse;

  BoostedProductFilterRequest boostedProductFilterRequest;
  private GdnRestListResponse<ProductCodeResponse> productCodeResponseGdnRestListResponse;
  private RejectProductVendorRequest rejectProductVendorRequest;
  private VendorQuickApprovalRequest vendorQuickApprovalRequest;
  private IprActionRequest iprActionRequest;

  @InjectMocks
  private ProductDistributionTaskRepositoryImpl productDistributionTaskRepository;

  @Mock
  private PDTFeign pdtFeign;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.productListRequest = new ProductListRequest();
    DistributionProductResponse distributionProduct = new DistributionProductResponse();
    List<DistributionProductResponse> distributionProductResponseList =
        Collections.singletonList(distributionProduct);

    productResponseGdnRestListResponse =
        new GdnRestListResponse<DistributionProductResponse>(null, null, true, distributionProductResponseList, null,
            REQUEST_ID);
    boostedProductFilterRequest = new BoostedProductFilterRequest();
    boostedProductFilterRequest.setEdited(Boolean.TRUE);
    bulkVendorProductActionsRequest = new BulkVendorProductActionsRequest();
    bulkVendorProductActionsRequest.setRequestId(REQUEST_ID);
    bulkVendorProductActionsRequest.setUserName(USERNAME);
    bulkVendorProductActionsRequest.setAssignmentType(CONTENT_ASSIGN);
    productCodeResponseGdnRestListResponse = getProductCodeSuccessResponse();
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, "CHANNEL");
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, "CLIENT-ID");
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, "REQUEST-ID");
    rejectProductVendorRequest = new RejectProductVendorRequest();

    vendorQuickApprovalRequest = new VendorQuickApprovalRequest();
    iprActionRequest = new IprActionRequest();
  }

  private GdnRestListResponse<DistributionProductResponse> generateSuccessResponse()
      throws Exception {
    List<DistributionProductResponse> distributionProductResponses = new ArrayList<>();
    DistributionProductResponse distributionProductResponse = new DistributionProductResponse();
    distributionProductResponses.add(distributionProductResponse);
    return new GdnRestListResponse<>(distributionProductResponses,
        new PageMetaData(PAGE, SIZE, distributionProductResponses.size()), REQUEST_ID);
  }

  private GdnRestListResponse<ProductCodeResponse> getProductCodeSuccessResponse(){
    List<ProductCodeResponse> productCodeResponses = new ArrayList<>();
    ProductCodeResponse productCodeResponse = new ProductCodeResponse();
    productCodeResponse.setProductCodes(PRODUCT_CODE);
    productCodeResponses.add(productCodeResponse);
    return new GdnRestListResponse<>(null, null, true,productCodeResponses,
      new PageMetaData(PAGE, SIZE, productCodeResponses.size()), REQUEST_ID);
  }

  @Test
  public void getProductsForVendorTest() throws Exception {
    GdnRestListResponse<DistributionProductResponse> successResponse = generateSuccessResponse();
    Mockito.when(
            pdtFeign.getProductListingForVendorCode(eq(STORE_ID_VAL), eq(REQUEST_ID), eq(API),
                eq(XBULK), eq(USERNAME), eq(PAGE), eq(SIZE), eq(productListRequest)))
        .thenReturn(successResponse);
    productDistributionTaskRepository
        .getProductsForVendor(REQUEST_ID, USERNAME, pageable, productListRequest);
    Mockito.verify(pdtFeign)
        .getProductListingForVendorCode(eq(STORE_ID_VAL), eq(REQUEST_ID), eq(API), eq(XBULK),
            eq(USERNAME), eq(PAGE), eq(SIZE), eq(productListRequest));
  }

  @Test
  public void getProductsForVendorTest_Failed() throws Exception {
    GdnRestListResponse<DistributionProductResponse> response =
        new GdnRestListResponse<>(null, null, false, REQUEST_ID);
    Mockito.when(pdtFeign.getProductListingForVendorCode(eq(STORE_ID_VAL), eq(REQUEST_ID), eq(API),
            eq(XBULK), eq(USERNAME), eq(PAGE), eq(SIZE), eq(productListRequest)))
        .thenReturn(response);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> productDistributionTaskRepository.getProductsForVendor(REQUEST_ID, USERNAME,
              pageable, productListRequest));
    } catch (Exception e) {} finally {
      Mockito.verify(pdtFeign)
          .getProductListingForVendorCode(eq(STORE_ID_VAL), eq(REQUEST_ID), eq(API),
              eq(XBULK), eq(USERNAME), eq(PAGE), eq(SIZE), eq(productListRequest));
    }
  }

  @Test
  public void getVendorFilteredProductsTest() throws Exception{
    filterSummaryRequest = new FilterSummaryRequest();
    GdnRestListRequest pageRequest = new GdnRestListRequest(PAGE, PAGE);
    Mockito.when(pdtFeign.getProductList(eq(STORE_ID_VAL), eq(REQUEST_ID), eq(API),
            eq(XBULK), eq(USERNAME), eq(PAGE), eq(SIZE), eq(filterSummaryRequest)))
        .thenReturn(productResponseGdnRestListResponse);
    productDistributionTaskRepository
        .getVendorFilteredProducts(USERNAME, REQUEST_ID, pageable, filterSummaryRequest);
    Mockito.verify(pdtFeign)
        .getProductList(eq(STORE_ID_VAL), eq(REQUEST_ID), eq(API),
            eq(XBULK), eq(USERNAME), eq(PAGE), eq(SIZE), eq(filterSummaryRequest));
  }

  @Test
  public void getVendorFilteredProductFailedTest() throws Exception {
    GdnRestListResponse<DistributionProductResponse> response =
        new GdnRestListResponse<>(null, null, false, REQUEST_ID);
    Mockito.when(
            pdtFeign.getProductList(eq(STORE_ID_VAL), eq(REQUEST_ID), eq(API),
                eq(XBULK), eq(USERNAME), eq(PAGE), eq(SIZE), eq(filterSummaryRequest)))
        .thenReturn(response);
    try {
      productDistributionTaskRepository
          .getVendorFilteredProducts(USERNAME, REQUEST_ID, pageable, filterSummaryRequest);
    } catch (Exception e) {
      Mockito.verify(pdtFeign)
          .getProductList(eq(STORE_ID_VAL), eq(REQUEST_ID), eq(API),
              eq(XBULK), eq(USERNAME), eq(PAGE), eq(SIZE), eq(filterSummaryRequest));
    }
  }

  @Test
  public void bulkVendorProductActionsTest() throws Exception {
    Mockito.when(pdtFeign.bulkVendorProductActions(eq(STORE_ID_VAL), eq(REQUEST_ID), eq(API),
            eq(XBULK), eq(USERNAME), eq(bulkVendorProductActionsRequest)))
        .thenReturn(new GdnRestSingleResponse<>(new BulkVendorProductActionsResponse(new ArrayList<>()), REQUEST_ID));
    BulkVendorProductActionsResponse response =
        productDistributionTaskRepository.bulkVendorProductActions(bulkVendorProductActionsRequest);
    Mockito.verify(pdtFeign)
        .bulkVendorProductActions(eq(STORE_ID_VAL), eq(REQUEST_ID), eq(API),
            eq(XBULK), eq(USERNAME), eq(bulkVendorProductActionsRequest));
    Assertions.assertNotNull(response);
  }

  @Test
  public void bulkVendorProductActionsExceptionTest() throws Exception {
    Mockito.when(pdtFeign.bulkVendorProductActions(eq(STORE_ID_VAL), eq(REQUEST_ID), eq(API),
            eq(XBULK), eq(USERNAME), eq(bulkVendorProductActionsRequest)))
        .thenReturn(
            new GdnRestSingleResponse<>(null, null, false, new BulkVendorProductActionsResponse(new ArrayList<>()),
                REQUEST_ID));
    try {
      productDistributionTaskRepository.bulkVendorProductActions(bulkVendorProductActionsRequest);
    } catch (ApplicationException e) {
    } finally {
      Mockito.verify(pdtFeign)
          .bulkVendorProductActions(eq(STORE_ID_VAL), eq(REQUEST_ID), eq(API),
              eq(XBULK), eq(USERNAME), eq(bulkVendorProductActionsRequest));
    }
  }

  @Test
  public void bulkVendorProductActionsConnectionExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationException.class).when(pdtFeign)
        .bulkVendorProductActions(eq(STORE_ID_VAL), eq(REQUEST_ID), eq(API),
            eq(XBULK), eq(USERNAME), eq(bulkVendorProductActionsRequest));
    try {
      productDistributionTaskRepository.bulkVendorProductActions(bulkVendorProductActionsRequest);
    } catch (ApplicationException e) {
    } finally {
      Mockito.verify(pdtFeign)
          .bulkVendorProductActions(eq(STORE_ID_VAL), eq(REQUEST_ID), eq(API),
              eq(XBULK), eq(USERNAME), eq(bulkVendorProductActionsRequest));
    }
  }

  @Test
  public void fetchProductsForAutoAssignmentTest() throws Exception{
    Mockito.when(pdtFeign.fetchProductsForAutoAssignment(eq(STORE_ID_VAL), eq(REQUEST_ID), eq(API),
            eq(XBULK), eq(USERNAME),
        eq(PAGE),eq(SIZE),eq(boostedProductFilterRequest)))
      .thenReturn(productCodeResponseGdnRestListResponse);
    GdnRestListResponse<ProductCodeResponse> response =
      productDistributionTaskRepository.fetchProductsForAutoAssignment(REQUEST_ID, USERNAME, PAGE,
        SIZE, boostedProductFilterRequest);
    Mockito.verify(pdtFeign)
      .fetchProductsForAutoAssignment(eq(STORE_ID_VAL), eq(REQUEST_ID), eq(API),
          eq(XBULK), eq(USERNAME),
          eq(PAGE),eq(SIZE),eq(boostedProductFilterRequest));
    Assertions.assertNotNull(response);
  }

  @Test
  public void fetchProductsForAutoAssignmentExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationException.class).when(
      pdtFeign).fetchProductsForAutoAssignment(eq(STORE_ID_VAL), eq(REQUEST_ID), eq(API),
        eq(XBULK), eq(USERNAME),
        eq(PAGE),eq(SIZE), eq(new BoostedProductFilterRequest()));
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> productDistributionTaskRepository.fetchProductsForAutoAssignment(REQUEST_ID,
              USERNAME, PAGE, SIZE, new BoostedProductFilterRequest()));
    }  finally {
      Mockito.verify(pdtFeign)
        .fetchProductsForAutoAssignment(eq(STORE_ID_VAL), eq(REQUEST_ID), eq(API),
            eq(XBULK), eq(USERNAME),
            eq(PAGE),eq(SIZE), eq(new BoostedProductFilterRequest()));
    }
  }

  @Test
  public void fetchProductsForAutoAssignmentNullTest() throws Exception {
    GdnRestListResponse<ProductCodeResponse> response = new GdnRestListResponse<>();
    response.setSuccess(false);
    Mockito.when(pdtFeign.fetchProductsForAutoAssignment(eq(STORE_ID_VAL), eq(REQUEST_ID), eq(API),
            eq(XBULK), eq(USERNAME),
            eq(PAGE),eq(SIZE),eq(boostedProductFilterRequest)))
      .thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> productDistributionTaskRepository.fetchProductsForAutoAssignment(REQUEST_ID,
              USERNAME, PAGE, SIZE, boostedProductFilterRequest));
    }  finally {
      Mockito.verify(pdtFeign)
        .fetchProductsForAutoAssignment(eq(STORE_ID_VAL), eq(REQUEST_ID), eq(API),
            eq(XBULK), eq(USERNAME),
            eq(PAGE),eq(SIZE),eq(boostedProductFilterRequest));
    }
  }

  @Test
  public void fetchProductsForAutoAssignmentWithNoContentTest() throws Exception {
    GdnRestListResponse<ProductCodeResponse> response = new GdnRestListResponse<>();
    response.setSuccess(true);
    response.setContent(Collections.EMPTY_LIST);
    Mockito.when(pdtFeign.fetchProductsForAutoAssignment(eq(STORE_ID_VAL), eq(REQUEST_ID), eq(API),
            eq(XBULK), eq(USERNAME),
            eq(PAGE),eq(SIZE),eq(boostedProductFilterRequest)))
      .thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> productDistributionTaskRepository.fetchProductsForAutoAssignment(REQUEST_ID,
              USERNAME, PAGE, SIZE, boostedProductFilterRequest));
    }  finally {
      Mockito.verify(pdtFeign)
        .fetchProductsForAutoAssignment(eq(STORE_ID_VAL), eq(REQUEST_ID), eq(API),
            eq(XBULK), eq(USERNAME),
            eq(PAGE),eq(SIZE),eq(boostedProductFilterRequest));
    }
  }

  @Test
  public void vendorRejectionTest() {
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(true);
    rejectProductVendorRequest.setProductCode(PRODUCT_CODE);
    Mockito.when(pdtFeign.vendorRejection(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any(RejectProductVendorRequest.class))).thenReturn(response);
    productDistributionTaskRepository.vendorRejection(STORE_ID, USERNAME, VENDOR_CODE, rejectProductVendorRequest);
    Mockito.verify(pdtFeign).vendorRejection(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any(RejectProductVendorRequest.class));
  }

  @Test
  public void vendorRejectionNullTest() {
    rejectProductVendorRequest.setProductCode(PRODUCT_CODE);
    Mockito.when(pdtFeign.vendorRejection(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.any(RejectProductVendorRequest.class))).thenReturn(null);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> productDistributionTaskRepository.vendorRejection(STORE_ID, USERNAME, null,
              rejectProductVendorRequest));
    } finally {
      Mockito.verify(pdtFeign)
          .vendorRejection(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.anyString(), Mockito.any(),
              Mockito.any(RejectProductVendorRequest.class));
    }
  }

  @Test
  public void vendorRejectionSuccessFalseTest() {
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(false);
    response.setErrorMessage("Error in rejection");
    rejectProductVendorRequest.setProductCode(PRODUCT_CODE);
    Mockito.when(pdtFeign.vendorRejection(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any(RejectProductVendorRequest.class))).thenReturn(response);
    productDistributionTaskRepository.vendorRejection(STORE_ID, USERNAME, VENDOR_CODE, rejectProductVendorRequest);
    Mockito.verify(pdtFeign).vendorRejection(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any(RejectProductVendorRequest.class));
  }

  @Test
  public void vendorQuickApprovalTest() {
    GdnRestSingleResponse<VendorQuickApprovalResponse> response = new GdnRestSingleResponse<>();
    VendorQuickApprovalResponse vendorQuickApprovalResponse = new VendorQuickApprovalResponse();
    response.setValue(vendorQuickApprovalResponse);
    response.setSuccess(true);
    rejectProductVendorRequest.setProductCode(PRODUCT_CODE);
    Mockito.when(pdtFeign.vendorQuickApproval(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any(VendorQuickApprovalRequest.class))).thenReturn(response);
    productDistributionTaskRepository.vendorQuickApproval(STORE_ID, USERNAME, vendorQuickApprovalRequest);
    Mockito.verify(pdtFeign).vendorQuickApproval(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any(VendorQuickApprovalRequest.class));
  }

  @Test
  public void vendorQuickApprovalFalseResponseTest() {
    GdnRestSingleResponse<VendorQuickApprovalResponse> response = new GdnRestSingleResponse<>();
    VendorQuickApprovalResponse vendorQuickApprovalResponse = new VendorQuickApprovalResponse();
    response.setValue(vendorQuickApprovalResponse);
    vendorQuickApprovalResponse.setErrorCodes(Collections.singletonList("Error in approval"));
    response.setSuccess(false);
    rejectProductVendorRequest.setProductCode(PRODUCT_CODE);
    Mockito.when(pdtFeign.vendorQuickApproval(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any(VendorQuickApprovalRequest.class))).thenReturn(response);
    productDistributionTaskRepository.vendorQuickApproval(STORE_ID, USERNAME, vendorQuickApprovalRequest);
    Mockito.verify(pdtFeign).vendorQuickApproval(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any(VendorQuickApprovalRequest.class));
  }

  @Test
  public void vendorRejectionNullResponseTest() {
    rejectProductVendorRequest.setProductCode(PRODUCT_CODE);
    Mockito.when(pdtFeign.vendorQuickApproval(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any(VendorQuickApprovalRequest.class))).thenReturn(null);

    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> productDistributionTaskRepository.vendorQuickApproval(STORE_ID, USERNAME,
              vendorQuickApprovalRequest));
    } finally {
      Mockito.verify(pdtFeign)
          .vendorQuickApproval(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.anyString(),
              Mockito.any(VendorQuickApprovalRequest.class));
    }
  }

  @Test
  public void performIprActionTest() {
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(true);
    Mockito.when(pdtFeign.performIprAction(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any(IprActionRequest.class))).thenReturn(response);
    productDistributionTaskRepository.performIprAction(STORE_ID, USERNAME, iprActionRequest);
    Mockito.verify(pdtFeign).performIprAction(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any(IprActionRequest.class));
  }

  @Test
  public void performIprActionAddBrandReportTest() {
    iprActionRequest.setAction(ProductStateIPR.IN_REVIEW.name());
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setSource(SOURCE);
    iprActionRequest.setAssignee(ASSIGNEE);
    iprActionRequest.setBrandReport(new BrandReport());
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(true);
    Mockito.when(pdtFeign.addProductToIPR(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), eq(PRODUCT_SKU), eq(SOURCE), eq(ASSIGNEE), Mockito.any())).thenReturn(response);
    productDistributionTaskRepository.performIprAction(STORE_ID, USERNAME, iprActionRequest);
    Mockito.verify(pdtFeign).addProductToIPR(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), eq(PRODUCT_SKU), eq(SOURCE), eq(ASSIGNEE), Mockito.any());
  }

  @Test
  public void performIprActionAddNewTest() {
    iprActionRequest.setAction(ProductStateIPR.IN_REVIEW.name());
    iprActionRequest.setProductSku(PRODUCT_SKU);
    iprActionRequest.setSource(SOURCE);
    iprActionRequest.setAssignee(ASSIGNEE);
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(true);
    Mockito.when(pdtFeign.addProductToIPR(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), eq(PRODUCT_SKU), eq(SOURCE), eq(ASSIGNEE), Mockito.any())).thenReturn(response);
    productDistributionTaskRepository.performIprAction(STORE_ID, USERNAME, iprActionRequest);
    Mockito.verify(pdtFeign).addProductToIPR(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), eq(PRODUCT_SKU), eq(SOURCE), eq(ASSIGNEE), Mockito.any());
  }

  @Test
  public void performIprActionNullTest() {
    Mockito.when(pdtFeign.performIprAction(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any(IprActionRequest.class)))
        .thenReturn(null);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> productDistributionTaskRepository.performIprAction(STORE_ID, USERNAME,
              iprActionRequest));
    } finally {
      Mockito.verify(pdtFeign)
          .performIprAction(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.anyString(), Mockito.any(IprActionRequest.class));
    }
  }

  @Test
  public void performIprActionSuccessFalseTest() {
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(false);
    Mockito.when(pdtFeign.performIprAction(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any(IprActionRequest.class))).thenReturn(response);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> productDistributionTaskRepository.performIprAction(STORE_ID, USERNAME,
              iprActionRequest));
    } finally {
      Mockito.verify(pdtFeign)
          .performIprAction(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.anyString(), Mockito.any(IprActionRequest.class));
    }
  }

  @Test
  public void getIprProductsListTest() {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    GdnRestListResponse<IprProductsResponse> response = new GdnRestListResponse<>();
    response.setContent(Collections.singletonList(IprProductsResponse.builder().build()));
    response.setSuccess(true);
    Mockito.when(
      pdtFeign.getIprProductsList(eq(Constant.STORE_ID), eq(REQUEST_ID), eq(Constant.CHANNEL_ID),
        eq(Constant.CLIENT_ID), eq(Constant.USER_NAME), Mockito.any(IPRProductListRequest.class),
        eq(0), eq(10))).thenReturn(response);
    productDistributionTaskRepository.getIprProductsList(REQUEST_ID,
      IPRProductListRequest.builder().build(), 0, 10);
    Mockito.verify(pdtFeign)
      .getIprProductsList(Mockito.any(), Mockito.anyString(), Mockito.any(), Mockito.any(),
        Mockito.any(), Mockito.any(), Mockito.anyInt(), Mockito.anyInt());
  }

  @Test
  public void getIprProductsListTest_nullResponse() {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    Mockito.when(
            pdtFeign.getIprProductsList(eq(STORE_ID), eq(REQUEST_ID), eq(CHANNEL_ID), eq(CLIENT_ID),
                eq(USERNAME), Mockito.any(IPRProductListRequest.class), eq(0), eq(10)))
        .thenReturn(null);
    try {
      productDistributionTaskRepository.getIprProductsList(REQUEST_ID,
          IPRProductListRequest.builder().build(), 0, 10);
    } catch (ApplicationRuntimeException exception) {
      Assertions.assertNotNull(exception);
    } finally {
      Mockito.verify(pdtFeign)
        .getIprProductsList(eq(Constant.STORE_ID), eq(REQUEST_ID), eq(Constant.CHANNEL_ID),
          eq(Constant.CLIENT_ID), eq(Constant.USER_NAME), Mockito.any(IPRProductListRequest.class),
          eq(0), eq(10));
    }
  }

  @Test
  public void getIprProductsListTest_successFalse() {
    GdnRestListResponse<IprProductsResponse> response = new GdnRestListResponse<>();
    response.setSuccess(false);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, CHANNEL_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, USERNAME);
    Mockito.when(
            pdtFeign.getIprProductsList(eq(STORE_ID), eq(REQUEST_ID), eq(CHANNEL_ID), eq(CLIENT_ID),
                eq(USERNAME), Mockito.any(IPRProductListRequest.class), eq(0), eq(10)))
        .thenReturn(response);
    try {
      productDistributionTaskRepository.getIprProductsList(REQUEST_ID,
          IPRProductListRequest.builder().build(), 0, 10);
    } catch (ApplicationRuntimeException exception) {
      Assertions.assertNotNull(exception);
    } finally {
      Mockito.verify(pdtFeign)
          .getIprProductsList(eq(Constant.STORE_ID), eq(REQUEST_ID), eq(Constant.CHANNEL_ID),
            eq(Constant.CLIENT_ID),
              eq(Constant.USER_NAME), Mockito.any(IPRProductListRequest.class), eq(0), eq(10));
    }
  }

  @AfterEach
  public void finalizeTest() throws Exception {
  }
}
