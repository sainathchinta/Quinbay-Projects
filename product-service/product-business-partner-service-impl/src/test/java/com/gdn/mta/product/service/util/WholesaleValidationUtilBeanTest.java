package com.gdn.mta.product.service.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import org.springframework.test.util.ReflectionTestUtils;

import com.gda.mta.product.dto.PickupPointCreateRequest;
import com.gda.mta.product.dto.ProductItemBusinessPartnerRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductItemWholesalePriceRequest;
import com.gda.mta.product.dto.ProductPriceAndWholesaleRequest;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.request.PriceRequest;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.response.MinWholesaleDiscountResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleConfigResponse;
import com.gdn.x.productcategorybase.dto.response.WholesaleMappingResponse;

public class WholesaleValidationUtilBeanTest {

  private static final String CATEGORY_CODE = "category_code";
  private static final String PRICE_PERCENTAGE = "PRICE_PERCENTAGE";
  private static final String STORE_ID = "store_id";
  private static final String ITEM_SKU = "itemSku";
  private static final int MINIMUM_PRICE = 1;
  private static final double PRICE = 2000.0;
  private static final int QUANTITY = 3;
  private static final int QUANTITY_1 = 5;
  private List<ProductItemCreationRequest> productItemCreationRequestList;
  private List<ProductItemBusinessPartnerRequest> productItemBusinessPartnerRequests;
  private WholesaleMappingResponse wholesalePricePercentageMappingResponse;
  private ProductAndItemsResponse productAndItemsResponse =
      new ProductAndItemsResponse(new ProductResponse(), new ArrayList<>());
  private ProductPriceAndWholesaleRequest productPriceAndWholesaleRequest = new ProductPriceAndWholesaleRequest();
  private ProductItemWholesalePriceRequest productItemWholesalePriceRequest;
  private ItemRequest itemRequest;

  @InjectMocks
  private WholesaleValidationUtilBean wholesaleValidationUtilBean;

  @Mock
  private PCBFeign pcbFeign;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(wholesaleValidationUtilBean, "maxWholesalePriceRequests", 5);
    ProductItemCreationRequest productItemCreationRequest = new ProductItemCreationRequest();
    productItemCreationRequest.setPrice(18000.0);
    productItemCreationRequest.setSalePrice(18000.0);
    productItemCreationRequest.setWholesalePriceActivated(true);
    productItemWholesalePriceRequest =  new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest.setQuantity(3);
    productItemWholesalePriceRequest.setWholesaleDiscount(10);
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest1 = new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest1.setQuantity(7);
    productItemWholesalePriceRequest1.setWholesaleDiscount(20);
    productItemCreationRequest.setProductItemWholesalePriceRequests(
        Arrays.asList(productItemWholesalePriceRequest, productItemWholesalePriceRequest1));
    productItemCreationRequestList = new ArrayList<>();
    productItemCreationRequestList.add(productItemCreationRequest);
    wholesalePricePercentageMappingResponse = new WholesaleMappingResponse();
    wholesalePricePercentageMappingResponse.setConfigurationType(PRICE_PERCENTAGE);
    wholesalePricePercentageMappingResponse.setWholesaleConfig(new ArrayList<>());
    WholesaleConfigResponse wholesaleConfigResponse2 = new WholesaleConfigResponse();
    wholesaleConfigResponse2.setQuantity(QUANTITY);
    wholesaleConfigResponse2.setMinWholesaleDiscount(new ArrayList<>());
    wholesaleConfigResponse2.getMinWholesaleDiscount().add(new MinWholesaleDiscountResponse(10000.0, 6.0));
    wholesaleConfigResponse2.getMinWholesaleDiscount().add(new MinWholesaleDiscountResponse(20000.0, 8.0));
    wholesaleConfigResponse2.getMinWholesaleDiscount().add(new MinWholesaleDiscountResponse(30000.0, 10.0));
    wholesaleConfigResponse2.getMinWholesaleDiscount().add(new MinWholesaleDiscountResponse(40000.0, 12.0));
    WholesaleConfigResponse wholesaleConfigResponse3 = new WholesaleConfigResponse();
    wholesaleConfigResponse3.setQuantity(QUANTITY_1);
    wholesaleConfigResponse3.setMinWholesaleDiscount(new ArrayList<>());
    wholesaleConfigResponse3.getMinWholesaleDiscount().add(new MinWholesaleDiscountResponse(10000.0, 7.0));
    wholesaleConfigResponse3.getMinWholesaleDiscount().add(new MinWholesaleDiscountResponse(20000.0, 8.0));
    wholesaleConfigResponse3.getMinWholesaleDiscount().add(new MinWholesaleDiscountResponse(30000.0, 10.0));
    wholesaleConfigResponse3.getMinWholesaleDiscount().add(new MinWholesaleDiscountResponse(40000.0, 12.0));
    wholesalePricePercentageMappingResponse.setWholesalePriceConfigEnabled(true);
    wholesalePricePercentageMappingResponse
        .setWholesaleConfig(Arrays.asList(wholesaleConfigResponse2, wholesaleConfigResponse3));
    productItemBusinessPartnerRequests = new ArrayList<>();
    ProductItemBusinessPartnerRequest productItemBusinessPartnerRequest = new ProductItemBusinessPartnerRequest();
    productItemBusinessPartnerRequest.setPrice(18000.0);
    productItemBusinessPartnerRequest.setSalePrice(18000.0);
    productItemBusinessPartnerRequest.setWholesalePriceActivated(true);
    productItemBusinessPartnerRequest.setProductItemWholesalePriceRequests(
        Arrays.asList(productItemWholesalePriceRequest, productItemWholesalePriceRequest1));
    productItemBusinessPartnerRequests.add(productItemBusinessPartnerRequest);
    productAndItemsResponse.getProduct().setMasterCatalog(new MasterCatalogDTO());
    productAndItemsResponse.getProduct().getMasterCatalog()
        .setCategory(new CategoryDTO(CATEGORY_CODE, StringUtils.EMPTY));
    productPriceAndWholesaleRequest.setWholesalePriceActivated(Boolean.TRUE);
    productPriceAndWholesaleRequest.setOfferPrice(PRICE);
    productPriceAndWholesaleRequest.setListPrice(PRICE);
    productPriceAndWholesaleRequest.setProductItemWholesalePriceRequests(new ArrayList<>());
    productPriceAndWholesaleRequest.getProductItemWholesalePriceRequests().add(new ProductItemWholesalePriceRequest(2, 10.0));
    productPriceAndWholesaleRequest.getProductItemWholesalePriceRequests().add(new ProductItemWholesalePriceRequest(3, 12.0));
    productPriceAndWholesaleRequest.getProductItemWholesalePriceRequests().add(new ProductItemWholesalePriceRequest(4, 14.0));

    itemRequest = new ItemRequest();
    itemRequest.setWholesalePriceActivated(true);
    PriceRequest priceRequest = new PriceRequest();
    priceRequest.setListPrice(10);
    priceRequest.setOfferPrice(8);
    Set<PriceRequest> prices = new HashSet<>();
    prices.add(priceRequest);
    itemRequest.setPrice(prices);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(pcbFeign);
  }

  @Test
  public void validateWholesaleConfigOnFlow1SameThresholdTest() {
    setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePricePercentageMappingResponse, Constants.DEFAULT_REQUEST_ID));
    wholesaleValidationUtilBean.validateWholesaleConfigOnFlow1(CATEGORY_CODE, productItemCreationRequestList, true);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
  }

  @Test
  public void validateWholesaleConfigOnFlow1L5Test() {
    setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePricePercentageMappingResponse, Constants.DEFAULT_REQUEST_ID));
    productItemCreationRequestList.get(0).setPickupPoints(new ArrayList<>());
    wholesaleValidationUtilBean.validateWholesaleConfigOnFlow1(CATEGORY_CODE, productItemCreationRequestList, false);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
  }

  @Test
  public void validateWholesaleConfigOnFlow1L5NotNullTest() {
    setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePricePercentageMappingResponse, Constants.DEFAULT_REQUEST_ID));
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest = new ProductItemWholesalePriceRequest();
    pickupPointCreateRequest
        .setProductItemWholesalePriceRequests(Collections.singletonList(productItemWholesalePriceRequest));
    productItemCreationRequestList.get(0).setPickupPoints(Collections.singletonList(pickupPointCreateRequest));
    wholesaleValidationUtilBean.validateWholesaleConfigOnFlow1(CATEGORY_CODE, productItemCreationRequestList, false);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
  }

  @Test
  public void validateWholesaleConfigOnFlow1L5NotNull1Test() {
    setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePricePercentageMappingResponse, Constants.DEFAULT_REQUEST_ID));
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    productItemCreationRequestList.get(0).setPickupPoints(Collections.singletonList(pickupPointCreateRequest));
    wholesaleValidationUtilBean.validateWholesaleConfigOnFlow1(CATEGORY_CODE, productItemCreationRequestList, false);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
  }

  @Test
  public void validateWholesaleConfigOnFlow1L5ExceptionTest() {
    setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePricePercentageMappingResponse, Constants.DEFAULT_REQUEST_ID));
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPrice(18000.0);
    pickupPointCreateRequest.setSalePrice(18000.0);
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest = new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest.setWholesaleDiscount(1);
    productItemWholesalePriceRequest.setQuantity(3);
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest1 = new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest1.setQuantity(7);
    productItemWholesalePriceRequest1.setWholesaleDiscount(20);
    pickupPointCreateRequest
        .setProductItemWholesalePriceRequests(Arrays.asList(productItemWholesalePriceRequest, productItemWholesalePriceRequest1));
    pickupPointCreateRequest.setWholesalePriceActivated(true);
    productItemCreationRequestList.get(0).setPickupPoints(Collections.singletonList(pickupPointCreateRequest));
    wholesaleValidationUtilBean.validateWholesaleConfigOnFlow1(CATEGORY_CODE, productItemCreationRequestList, false);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
  }

  @Test
  public void validateWholesaleConfigOnFlow1L5Test1() {
    setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePricePercentageMappingResponse, Constants.DEFAULT_REQUEST_ID));
    PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
    pickupPointCreateRequest.setPrice(18000.0);
    pickupPointCreateRequest.setSalePrice(18000.0);
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest = new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest.setQuantity(3);
    productItemWholesalePriceRequest.setWholesaleDiscount(10);
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest1 = new ProductItemWholesalePriceRequest();
    productItemWholesalePriceRequest1.setQuantity(7);
    productItemWholesalePriceRequest1.setWholesaleDiscount(20);
    pickupPointCreateRequest
        .setProductItemWholesalePriceRequests(Arrays.asList(productItemWholesalePriceRequest, productItemWholesalePriceRequest1));
    pickupPointCreateRequest.setWholesalePriceActivated(true);
    productItemCreationRequestList.get(0).setPickupPoints(Collections.singletonList(pickupPointCreateRequest));
    wholesaleValidationUtilBean.validateWholesaleConfigOnFlow1(CATEGORY_CODE, productItemCreationRequestList, false);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
  }

  @Test
  public void validateWholesaleConfigOnFlow1DifferentThresholdTestWHATrue() {
    productItemCreationRequestList.get(0).getProductItemWholesalePriceRequests().get(0).setWholesaleDiscount(1);
    setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePricePercentageMappingResponse, Constants.DEFAULT_REQUEST_ID));
    wholesaleValidationUtilBean.validateWholesaleConfigOnFlow1(CATEGORY_CODE, productItemCreationRequestList, true);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Assertions.assertFalse(productItemCreationRequestList.get(0).getWholesalePriceActivated());

  }

  @Test
  public void validateWholesaleConfigOnFlow1DifferentThresholdTestWHATrueAndConfigEnabledFalse() {
    wholesalePricePercentageMappingResponse.setWholesalePriceConfigEnabled(false);
    productItemCreationRequestList.get(0).getProductItemWholesalePriceRequests().get(0).setWholesaleDiscount(1);
    setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePricePercentageMappingResponse, Constants.DEFAULT_REQUEST_ID));
    wholesaleValidationUtilBean.validateWholesaleConfigOnFlow1(CATEGORY_CODE, productItemCreationRequestList, true);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Assertions.assertTrue(productItemCreationRequestList.get(0).getWholesalePriceActivated());
  }

  @Test
  public void validateWholesaleConfigOnFlow1DifferentThresholdTestWHAFalse() {
    productItemCreationRequestList.get(0).getProductItemWholesalePriceRequests().get(0).setWholesaleDiscount(1);
    productItemCreationRequestList.get(0).setWholesalePriceActivated(false);
    setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePricePercentageMappingResponse, Constants.DEFAULT_REQUEST_ID));
    wholesaleValidationUtilBean.validateWholesaleConfigOnFlow1(CATEGORY_CODE, productItemCreationRequestList, true);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Assertions.assertFalse(productItemCreationRequestList.get(0).getWholesalePriceActivated());
  }

  @Test
  public void validateWholesaleConfigOnFlow1DifferentThresholdTestWHANull() {
    productItemCreationRequestList.get(0).getProductItemWholesalePriceRequests().get(0).setWholesaleDiscount(1);
    productItemCreationRequestList.get(0).setWholesalePriceActivated(null);
    setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePricePercentageMappingResponse, Constants.DEFAULT_REQUEST_ID));
    wholesaleValidationUtilBean.validateWholesaleConfigOnFlow1(CATEGORY_CODE, productItemCreationRequestList, true);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Assertions.assertNull(productItemCreationRequestList.get(0).getWholesalePriceActivated());
  }


  @Test
  public void validateWholesaleConfigOnFlow2SameThresholdTest() {
    setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePricePercentageMappingResponse, Constants.DEFAULT_REQUEST_ID));
    wholesaleValidationUtilBean.validateWholesaleConfigOnFlow2(CATEGORY_CODE, productItemBusinessPartnerRequests);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
  }

  @Test
  public void validateWholesaleConfigOnFlow2DifferentThresholdTestWHATrue() {
    productItemBusinessPartnerRequests.get(0).getProductItemWholesalePriceRequests().get(0).setWholesaleDiscount(1);
    setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePricePercentageMappingResponse, Constants.DEFAULT_REQUEST_ID));
    wholesaleValidationUtilBean.validateWholesaleConfigOnFlow2(CATEGORY_CODE, productItemBusinessPartnerRequests);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Assertions.assertFalse(productItemBusinessPartnerRequests.get(0).getWholesalePriceActivated());

  }

  @Test
  public void validateWholesaleConfigOnFlow2DifferentThresholdTestWHATrueAndConfigEnabledFalse() {
    wholesalePricePercentageMappingResponse.setWholesalePriceConfigEnabled(false);
    productItemBusinessPartnerRequests.get(0).getProductItemWholesalePriceRequests().get(0).setWholesaleDiscount(1);
    setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePricePercentageMappingResponse, Constants.DEFAULT_REQUEST_ID));
    wholesaleValidationUtilBean.validateWholesaleConfigOnFlow2(CATEGORY_CODE, productItemBusinessPartnerRequests);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Assertions.assertTrue(productItemBusinessPartnerRequests.get(0).getWholesalePriceActivated());
  }

  @Test
  public void validateWholesaleConfigOnFlow2DifferentThresholdTestWHAFalse() {
    productItemBusinessPartnerRequests.get(0).getProductItemWholesalePriceRequests().get(0).setWholesaleDiscount(1);
    productItemBusinessPartnerRequests.get(0).setWholesalePriceActivated(false);
    setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePricePercentageMappingResponse, Constants.DEFAULT_REQUEST_ID));
    wholesaleValidationUtilBean.validateWholesaleConfigOnFlow2(CATEGORY_CODE, productItemBusinessPartnerRequests);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Assertions.assertFalse(productItemBusinessPartnerRequests.get(0).getWholesalePriceActivated());
  }

  @Test
  public void validateWholesaleConfigOnFlow2DifferentThresholdTestWHANull() {
    productItemBusinessPartnerRequests.get(0).getProductItemWholesalePriceRequests().get(0).setWholesaleDiscount(1);
    productItemBusinessPartnerRequests.get(0).setWholesalePriceActivated(null);
    setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
    Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePricePercentageMappingResponse, Constants.DEFAULT_REQUEST_ID));
    wholesaleValidationUtilBean.validateWholesaleConfigOnFlow2(CATEGORY_CODE, productItemBusinessPartnerRequests);
    Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Assertions.assertNull(productItemBusinessPartnerRequests.get(0).getWholesalePriceActivated());
  }

  @Test
  public void validateWholesaleConfigOnFlow2ExceptionTest() {
    try {
      setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
          Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
      Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
          .thenReturn(new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, false, null,
              Constants.DEFAULT_REQUEST_ID));
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        wholesaleValidationUtilBean.validateWholesaleConfigOnFlow2(CATEGORY_CODE, productItemBusinessPartnerRequests);
      });
    } catch (ApplicationRuntimeException e) {
      throw new ApplicationRuntimeException();
    } finally {
      Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    }
  }

  @Test
  public void validateWholesaleConfigOnFlow1ExceptionTest() {
    try {
      setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
          Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
      Mockito.when(pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
          .thenReturn(new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, false, null,
              Constants.DEFAULT_REQUEST_ID));
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        wholesaleValidationUtilBean.validateWholesaleConfigOnFlow1(CATEGORY_CODE, productItemCreationRequestList, true);
      });
    } catch (ApplicationRuntimeException e) {
      throw new ApplicationRuntimeException();
    } finally {
      Mockito.verify(pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    }
  }

  @Test
  public void validateWholesalePriceRequestForUpdateTest() {
    Mockito.when(this.pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePricePercentageMappingResponse, Constants.DEFAULT_REQUEST_ID));
    this.wholesaleValidationUtilBean
        .validateWholesalePriceRequestForUpdate(STORE_ID, productPriceAndWholesaleRequest, ITEM_SKU, productAndItemsResponse, MINIMUM_PRICE);
    Mockito.verify(this.pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
  }

  @Test
  public void validateWholesalePriceRequestForUpdate_loadApiErrorCodeTest() {
    productPriceAndWholesaleRequest.getProductItemWholesalePriceRequests().get(0).setWholesaleDiscount(5.0);
    productPriceAndWholesaleRequest.getProductItemWholesalePriceRequests().get(1).setWholesaleDiscount(6.0);
    productPriceAndWholesaleRequest.getProductItemWholesalePriceRequests().get(2).setWholesaleDiscount(7.0);
    productPriceAndWholesaleRequest.setOfferPrice(100000);
    Mockito.when(this.pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePricePercentageMappingResponse, Constants.DEFAULT_REQUEST_ID));
    this.wholesaleValidationUtilBean
        .validateWholesalePriceRequestForUpdate(STORE_ID, productPriceAndWholesaleRequest, ITEM_SKU, productAndItemsResponse, MINIMUM_PRICE);
    Mockito.verify(this.pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
  }

  @Test
  public void validateWholesalePriceRequestForUpdate_exceptionTest() {
    Mockito.when(this.pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.wholesaleValidationUtilBean
            .validateWholesalePriceRequestForUpdate(STORE_ID, productPriceAndWholesaleRequest, ITEM_SKU,
                productAndItemsResponse, MINIMUM_PRICE);
      });
    } finally {
      Mockito.verify(this.pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    }
  }

  @Test
  public void validateWholesalePriceRequestForUpdateExceptionTest() {
    productPriceAndWholesaleRequest.setOfferPrice(1);
    ApiErrorCode apiErrorCode = null;
    try {
      Mockito.when(this.pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
          .thenReturn(new GdnRestSingleResponse<>(wholesalePricePercentageMappingResponse, Constants.DEFAULT_REQUEST_ID));
      apiErrorCode = this.wholesaleValidationUtilBean
          .validateWholesalePriceRequestForUpdate(STORE_ID, productPriceAndWholesaleRequest, ITEM_SKU,
              productAndItemsResponse, MINIMUM_PRICE);
    } finally {
      Mockito.verify(this.pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
      Assertions.assertNotNull(apiErrorCode);
      Assertions.assertEquals(apiErrorCode, ApiErrorCode.WHOLESALE_DISCOUNT_LTE_MINIMUM_DISCOUNT);
    }
  }


  @Test
  public void validateWholesaleConfigOnUpdateTest() {
    Mockito.when(this.pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePricePercentageMappingResponse, Constants.DEFAULT_REQUEST_ID));
    this.wholesaleValidationUtilBean
        .validateWholesaleConfigOnUpdate(CATEGORY_CODE, Arrays.asList(productItemWholesalePriceRequest), itemRequest,
            MINIMUM_PRICE, ITEM_SKU, Boolean.TRUE, null);
    Mockito.verify(this.pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
  }

  @Test
  public void validateWholesaleConfigOnUpdatePriceConfigEnabledFalseTest() {
    setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
    wholesalePricePercentageMappingResponse.setWholesalePriceConfigEnabled(false);
    Mockito.when(
            this.pcbFeign.getWholesaleConfigToCategory(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePricePercentageMappingResponse, Constants.DEFAULT_REQUEST_ID));
    this.wholesaleValidationUtilBean.validateWholesaleConfigOnUpdate(CATEGORY_CODE,
        Arrays.asList(productItemWholesalePriceRequest), itemRequest, MINIMUM_PRICE, ITEM_SKU, Boolean.TRUE, null);
    Mockito.verify(this.pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
  }

  @Test
  public void validateWholesaleConfigOnUpdateNullResponseTest() {
    Mockito.when(this.pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, Constants.DEFAULT_REQUEST_ID));
    this.wholesaleValidationUtilBean
        .validateWholesaleConfigOnUpdate(CATEGORY_CODE, Arrays.asList(productItemWholesalePriceRequest), itemRequest,
            MINIMUM_PRICE, ITEM_SKU, Boolean.TRUE, null);
    Mockito.verify(this.pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
  }

  @Test
  public void validateWholesaleConfigOnUpdateNotNullMappingTest() {
    Mockito.when(this.pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePricePercentageMappingResponse, Constants.DEFAULT_REQUEST_ID));
    this.wholesaleValidationUtilBean
        .validateWholesaleConfigOnUpdate(CATEGORY_CODE, Arrays.asList(productItemWholesalePriceRequest), itemRequest,
            MINIMUM_PRICE, ITEM_SKU, Boolean.TRUE, wholesalePricePercentageMappingResponse);
  }

  @Test
  public void validateWholesaleConfigOnUpdateWithEmptyRequestTest() {
    Assertions.assertNull(this.wholesaleValidationUtilBean
        .validateWholesaleConfigOnUpdate(CATEGORY_CODE, Arrays.asList(), itemRequest, MINIMUM_PRICE, ITEM_SKU,
            Boolean.TRUE, null));
  }

  @Test
  public void validateWholesaleConfigOnUpdateWithwholesalePriceActivatedFalseTest() {
    Assertions.assertNull(this.wholesaleValidationUtilBean
        .validateWholesaleConfigOnUpdate(CATEGORY_CODE, Arrays.asList(), itemRequest, MINIMUM_PRICE, ITEM_SKU,
            Boolean.FALSE, null));
  }

  @Test
  public void validateWholesaleConfigOnUpdateActivatedFalseTest() {
    ApiErrorCode response = this.wholesaleValidationUtilBean
        .validateWholesaleConfigOnUpdate(CATEGORY_CODE, Arrays.asList(productItemWholesalePriceRequest), itemRequest,
            MINIMUM_PRICE, ITEM_SKU, Boolean.FALSE, null);
    Assertions.assertNull(response);
  }

  @Test
  public void validateWholesaleConfigOnUpdateTestExceptionTest() {
    setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
    Mockito.when(this.pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        this.wholesaleValidationUtilBean
            .validateWholesaleConfigOnUpdate(CATEGORY_CODE, Arrays.asList(productItemWholesalePriceRequest), itemRequest,
                MINIMUM_PRICE, ITEM_SKU, Boolean.TRUE, null);
      });
    } finally {
      Mockito.verify(this.pcbFeign)
          .getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    }
  }

  @Test
  public void validateWholesaleConfigOnUpdateCatchExceptionTest() {
    setMdcParameters(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME);
    Mockito.when(this.pcbFeign.getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(wholesalePricePercentageMappingResponse, Constants.DEFAULT_REQUEST_ID));
    ApiErrorCode apiErrorCode = this.wholesaleValidationUtilBean
        .validateWholesaleConfigOnUpdate(CATEGORY_CODE, Arrays.asList(productItemWholesalePriceRequest), itemRequest,
            (int) (itemRequest.getPrice().stream().findFirst().get().getOfferPrice() + 1), ITEM_SKU, Boolean.TRUE, null);
    Mockito.verify(this.pcbFeign).getWholesaleConfigToCategory(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Assertions.assertNotNull(apiErrorCode);
    Assertions.assertEquals(apiErrorCode , ApiErrorCode.WHOLESALE_DISCOUNT_LTE_MINIMUM_DISCOUNT);
  }

  private void setMdcParameters(String storeId, String channelId, String clientId, String requestId, String username) {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, clientId);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, channelId);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
  }
}