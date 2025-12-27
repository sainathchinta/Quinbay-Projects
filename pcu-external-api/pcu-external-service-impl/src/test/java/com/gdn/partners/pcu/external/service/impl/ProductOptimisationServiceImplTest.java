package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.external.client.feign.ProductAnalyticsFeign;
import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationFeedbackRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationListRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationListWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationUpdateStatusRequest;
import com.gdn.partners.pcu.external.web.model.response.ProductCountsWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductOptimisationListResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductOptimisationSuggestionResponse;
import com.gdn.x.product.enums.Constants;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;

import java.util.List;


@ExtendWith(MockitoExtension.class)
class ProductOptimisationServiceImplTest {

  private static final String STORE_ID = "10001";
  private static final String SELLER_CODE = "sellerCode";
  private static final String REQUEST_ID = "requestId";
  private static final int PRODUCT_COUNT = 1;
  private static final String PRODUCT_SKU = "productSku";
  private static final String IMAGE = "image";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private final GdnRestSingleResponse<ProductCountsWebResponse> response = new GdnRestSingleResponse<>();
  private final GdnBaseRestResponse result = new GdnBaseRestResponse();
  private final ProductOptimisationFeedbackRequest request = new ProductOptimisationFeedbackRequest();
  private final ProductOptimisationSuggestionResponse productOptimisationSuggestionResponse =
      new ProductOptimisationSuggestionResponse();
  private final ProductOptimisationUpdateStatusRequest productOptimisationUpdateStatusRequest =
      new ProductOptimisationUpdateStatusRequest();

  @InjectMocks
  private ProductOptimisationServiceImpl productOptimisationService;

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(productAnalyticsFeign);
  }

  @BeforeEach
  void setUp() {
    ProductCountsWebResponse productCountsWebResponse = new ProductCountsWebResponse();
    productCountsWebResponse.setProductCount(PRODUCT_COUNT);
    response.setValue(productCountsWebResponse);
    response.setSuccess(true);
    result.setSuccess(true);
    request.setProductSku(PRODUCT_SKU);
    productOptimisationUpdateStatusRequest.setViewed(Boolean.TRUE);
    productOptimisationUpdateStatusRequest.setProductSku(PRODUCT_SKU);
  }

  @Mock
  private ProductAnalyticsFeign productAnalyticsFeign;

  @Test
  void getProductCount() {
    Mockito.when(productAnalyticsFeign.getProductCount(STORE_ID, REQUEST_ID,SELLER_CODE)).thenReturn(response);
    productOptimisationService.getProductCount(STORE_ID, REQUEST_ID, SELLER_CODE);
    Mockito.verify(productAnalyticsFeign).getProductCount(STORE_ID, REQUEST_ID, SELLER_CODE);
  }

  @Test
  void fetchProductOptimisationListTest() {
    ProductOptimisationListWebRequest productOptimisationListWebRequest =
        new ProductOptimisationListWebRequest();
    productOptimisationListWebRequest.setKeyword(PRODUCT_SKU);
    ProductOptimisationListRequest productOptimisationListRequest = new ProductOptimisationListRequest();
    productOptimisationListRequest.setSellerCode(SELLER_CODE);
    productOptimisationListRequest.setKeyword(PRODUCT_SKU);
    ProductOptimisationListResponse productOptimisationListResponse =
        new ProductOptimisationListResponse();
    productOptimisationListResponse.setProductSku(PRODUCT_SKU);
    productOptimisationListResponse.setImage(IMAGE);
    GdnRestListResponse<ProductOptimisationListResponse> response = new GdnRestListResponse();
    response.setSuccess(true);
    response.setContent(List.of(productOptimisationListResponse));
    response.setPageMetaData(new PageMetaData(SIZE, PAGE, Constants.ONE));
    Mockito.when(productAnalyticsFeign.filter(PAGE, SIZE, productOptimisationListRequest))
        .thenReturn(response);
    Page<ProductOptimisationListResponse> result =
        productOptimisationService.fetchProductOptimisationList(SELLER_CODE,
            productOptimisationListWebRequest, PAGE, SIZE);
    Assertions.assertEquals(Constants.ONE, result.getTotalElements());
    Assertions.assertEquals(PRODUCT_SKU, result.getContent().get(0).getProductSku());
    Assertions.assertEquals(IMAGE, result.getContent().get(0).getImage());
    Mockito.verify(productAnalyticsFeign).filter(PAGE, SIZE, productOptimisationListRequest);
  }

  @Test
  void provideSuggestionFeedbackTest() {
    Mockito.when(productAnalyticsFeign.provideSuggestionFeedback(request)).thenReturn(result);
    productOptimisationService.submitSuggestionFeedback(request);
    Mockito.verify(productAnalyticsFeign).provideSuggestionFeedback(request);
  }

  @Test
  void getSuggestionDetailsTest() {
    GdnRestListResponse<ProductOptimisationSuggestionResponse> response =
        new GdnRestListResponse<>(List.of(productOptimisationSuggestionResponse), null, null);
    Mockito.when(productAnalyticsFeign.getSuggestionDetail(PRODUCT_SKU)).thenReturn(response);
    productOptimisationService.getSuggestionDetails(PRODUCT_SKU);
    Mockito.verify(productAnalyticsFeign).getSuggestionDetail(PRODUCT_SKU);
  }

  @Test
  void updateStatusTest() {
    GdnBaseRestResponse response = new GdnBaseRestResponse(true);
    Mockito.when(productAnalyticsFeign.updateStatus(REQUEST_ID, productOptimisationUpdateStatusRequest))
        .thenReturn(response);
    productOptimisationService.updateStatusForProductOptimisation(REQUEST_ID, productOptimisationUpdateStatusRequest);
    Mockito.verify(productAnalyticsFeign).updateStatus(REQUEST_ID, productOptimisationUpdateStatusRequest);
  }
}
