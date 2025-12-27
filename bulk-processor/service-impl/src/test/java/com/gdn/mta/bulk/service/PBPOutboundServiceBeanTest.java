package com.gdn.mta.bulk.service;


import static org.mockito.ArgumentMatchers.eq;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import com.gda.mta.product.dto.BrandUpdateRequest;
import com.gda.mta.product.dto.FbbCreatePickupPointRequest;
import com.gda.mta.product.dto.FbbCreatePickupPointResponse;
import com.gda.mta.product.dto.ItemErrorListResponse;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.ItemPriceStockQuickUpdateResponse;
import com.gda.mta.product.dto.ItemSkuPickupPointRequest;
import com.gda.mta.product.dto.ProductLevel3QuickEditV2Request;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductLevel3ViewConfigStockRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.response.InProgressProductResponse;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.ProductCodeAndNameDetails;
import com.gda.mta.product.dto.response.ProductCodeAndNameResponseList;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.models.InternalBrandUpdateEventModel;
import com.gdn.mta.bulk.models.ProductBrandUpdateRequest;
import com.gdn.mta.bulk.models.ProductMasterDataEditRequest;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.feignConfig.PBPFeign;
import com.gdn.mta.bulk.dto.product.ProductAndBrandResponse;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

public class PBPOutboundServiceBeanTest {

  private static final String CATEGORY_ID = "CatgeoryId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final int PAGE = 0;
  private static final int SIZE = 25;
  private static final String ITEM_SKU = "item-sku";
  private static final String PRODUCT_SKU = "productSku";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String OLD_BRAND_CODE = "oldBrandCode";
  private static final String NEW_BRAND_CODE = "newBrandCode";
  private static final String STORE_ID = "storeId";
  private static final String DESTINATION_BRAND_NAME = "destinationBrand";
  private FbbCreatePickupPointRequest fbbCreatePickupPointRequest;

  private Pageable pageable = PageRequest.of(PAGE, SIZE);

  @Mock
  private PBPFeign pbpFeign;

  @InjectMocks
  private PBPOutboundServiceBean pbpOutboundService;

  @BeforeEach
  public void init() throws Exception {
    fbbCreatePickupPointRequest = new FbbCreatePickupPointRequest();
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(pbpFeign);
  }

  @Test
  public void getProductAndBrandResponseGdnRestListResponse_Success() {
    setMdcParameters();
    int fetchSize = 50;
    ReflectionTestUtils.setField(pbpOutboundService, "bulkBrandUpdateProductsFetchSize", fetchSize);

    List<ProductAndBrandResponse> content = Collections.singletonList(new ProductAndBrandResponse());
    Mockito.when(pbpFeign.getProductsByBrandName(
        eq(STORE_ID),
        eq(MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER)),
        eq(MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER)),
        eq(MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER)),
        eq(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER)),
        eq(DESTINATION_BRAND_NAME),
        eq(PAGE),
        eq(fetchSize)))
      .thenReturn(new GdnRestListResponse<>(null, null, true, content, new PageMetaData(PAGE, fetchSize, content.size()), REQUEST_ID));

    GdnRestListResponse<ProductAndBrandResponse> response =
      pbpOutboundService.getProductAndBrandResponseGdnRestListResponse(PAGE, DESTINATION_BRAND_NAME, STORE_ID);

    Assertions.assertTrue(response.isSuccess());
    Mockito.verify(pbpFeign).getProductsByBrandName(
      eq(STORE_ID),
      eq(MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER)),
      eq(MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER)),
      eq(MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER)),
      eq(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER)),
      eq(DESTINATION_BRAND_NAME),
      eq(PAGE),
      eq(fetchSize));
  }

  @Test
  public void getProductAndBrandResponseGdnRestListResponse_CommunicationFailure() {
    setMdcParameters();
    int fetchSize = 25;
    ReflectionTestUtils.setField(pbpOutboundService, "bulkBrandUpdateProductsFetchSize", fetchSize);

    Mockito.when(pbpFeign.getProductsByBrandName(
        eq(STORE_ID),
        eq(MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER)),
        eq(MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER)),
        eq(MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER)),
        eq(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER)),
        eq(DESTINATION_BRAND_NAME),
        eq(PAGE),
        eq(fetchSize)))
      .thenReturn(new GdnRestListResponse<>(null, null, false, null, null, REQUEST_ID));

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> pbpOutboundService.getProductAndBrandResponseGdnRestListResponse(PAGE, DESTINATION_BRAND_NAME, STORE_ID));
    } finally {
      Mockito.verify(pbpFeign).getProductsByBrandName(
        eq(STORE_ID),
        eq(MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER)),
        eq(MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER)),
        eq(MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER)),
        eq(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER)),
        eq(DESTINATION_BRAND_NAME),
        eq(PAGE),
        eq(fetchSize));
    }
  }

  @Test
  public void getProductAndBrandResponseGdnRestListResponse_DataNotFound() {
    setMdcParameters();
    int fetchSize = 30;
    ReflectionTestUtils.setField(pbpOutboundService, "bulkBrandUpdateProductsFetchSize", fetchSize);

    Mockito.when(pbpFeign.getProductsByBrandName(
        eq(STORE_ID),
        eq(MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER)),
        eq(MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER)),
        eq(MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER)),
        eq(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER)),
        eq(DESTINATION_BRAND_NAME),
        eq(PAGE),
        eq(fetchSize)))
      .thenReturn(new GdnRestListResponse<>(null, null, true, Collections.emptyList(), new PageMetaData(), REQUEST_ID));

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> pbpOutboundService.getProductAndBrandResponseGdnRestListResponse(PAGE, DESTINATION_BRAND_NAME, STORE_ID));
    } finally {
      Mockito.verify(pbpFeign).getProductsByBrandName(
        eq(STORE_ID),
        eq(MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER)),
        eq(MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER)),
        eq(MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER)),
        eq(MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER)),
        eq(DESTINATION_BRAND_NAME),
        eq(PAGE),
        eq(fetchSize));
    }
  }

  @Test
  public void getActiveBrandsByCategoryId() throws Exception {
    setMdcParameters();
    Mockito.when(pbpFeign.getActiveBrandsByCategoryId(MDC.get(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER),
        MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
        MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER),
        MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER),
        MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER), CATEGORY_ID, true))
        .thenReturn(new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(), REQUEST_ID));
    pbpOutboundService.getActiveBrandsByCategoryId(CATEGORY_ID);
    Mockito.verify(pbpFeign)
        .getActiveBrandsByCategoryId(MDC.get(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER),
            MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER), CATEGORY_ID, true);
  }

  @Test
  public void getActiveBrandsByCategoryIdExceptionTest() throws Exception {
    try {
      setMdcParameters();
      Mockito.when(pbpFeign
          .getActiveBrandsByCategoryId(MDC.get(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER),
              MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
              MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER),
              MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER),
              MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER), CATEGORY_ID, true))
          .thenReturn(new GdnRestListResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, false, REQUEST_ID));
      Assertions.assertThrows(RuntimeException.class,
          () -> pbpOutboundService.getActiveBrandsByCategoryId(CATEGORY_ID));
    } catch (ApplicationRuntimeException e) {
      throw new ApplicationRuntimeException();
    } finally {
      Mockito.verify(pbpFeign)
          .getActiveBrandsByCategoryId(MDC.get(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER),
              MDC.get(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER),
              MDC.get(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER),
              MDC.get(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER),
              MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER), CATEGORY_ID, true);
    }
  }

  private void setMdcParameters() {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER,
        GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER);
  }

  @Test
  public void fetchInProgressProductsByMerchantCodeTest() {
    Mockito.when(
      this.pbpFeign.fetchInProgressProductsByMerchantCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE, PAGE, SIZE)).thenReturn(
      new GdnRestListResponse<>(null, null, true, Collections.EMPTY_LIST, new PageMetaData(PAGE, SIZE, PAGE), REQUEST_ID));
    Page<InProgressProductResponse> response =
      this.pbpOutboundService.fetchInProgressProductsByMerchantCode(REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE, pageable);
    Mockito.verify(this.pbpFeign)
      .fetchInProgressProductsByMerchantCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, REQUEST_ID,
        USERNAME, BUSINESS_PARTNER_CODE, PAGE, SIZE);
  }

  @Test
  public void fetchInProgressProductsByMerchantCode_successFalseTest() {
    Mockito.when(
        this.pbpFeign.fetchInProgressProductsByMerchantCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
          REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE, PAGE, SIZE))
      .thenReturn(new GdnRestListResponse<>(null, null, false, null, null, REQUEST_ID));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> this.pbpOutboundService.fetchInProgressProductsByMerchantCode(REQUEST_ID, USERNAME,
              BUSINESS_PARTNER_CODE, pageable));
    } finally {
      Mockito.verify(this.pbpFeign)
        .fetchInProgressProductsByMerchantCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, REQUEST_ID,
          USERNAME, BUSINESS_PARTNER_CODE, PAGE, SIZE);
    }
  }

  @Test
  public void findProductDetailsFromL5Test() throws Exception {
    ProductLevel3SummaryRequest productLevel3SummaryRequest = new ProductLevel3SummaryRequest();
    productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));

    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    Mockito.when(this.pbpFeign.getItemPickupPointL3Listing(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
        eq(Constant.CLIENT_ID), eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(pageable.getPageNumber()),
        eq(pageable.getPageSize()), Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(
        new GdnRestListResponse<>(null, null, true, Arrays.asList(itemPickupPointListingL3Response),
            new PageMetaData(PAGE, SIZE, SIZE), REQUEST_ID));
    this.pbpOutboundService.findProductDetailsFromL5(BUSINESS_PARTNER_CODE, productLevel3SummaryRequest, pageable, true);
    Mockito.verify(this.pbpFeign)
        .getItemPickupPointL3Listing(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
            eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(pageable.getPageNumber()), eq(pageable.getPageSize()),
            Mockito.any(ItemPickupPointListingL3Request.class));
  }

  @Test
  public void findProductDetailsFromL5SuccessTrueWithEmptyResponseTest() throws Exception {
    ProductLevel3SummaryRequest productLevel3SummaryRequest = new ProductLevel3SummaryRequest();
    productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));

    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    Mockito.when(this.pbpFeign.getItemPickupPointL3Listing(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
        eq(Constant.CLIENT_ID), eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(pageable.getPageNumber()),
        eq(pageable.getPageSize()), Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(
        new GdnRestListResponse<>(null, null, true, new ArrayList<>(),
            new PageMetaData(PAGE, SIZE, SIZE), REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class,
          () -> this.pbpOutboundService.findProductDetailsFromL5(BUSINESS_PARTNER_CODE,
              productLevel3SummaryRequest, pageable, true));
    } finally {
      Mockito.verify(this.pbpFeign)
          .getItemPickupPointL3Listing(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID), eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(pageable.getPageNumber()), eq(pageable.getPageSize()),
              Mockito.any(ItemPickupPointListingL3Request.class));
    }
  }

  @Test
  public void findProductDetailsFromL5ExceptionTest() throws Exception {
    ProductLevel3SummaryRequest productLevel3SummaryRequest = new ProductLevel3SummaryRequest();
    productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));

    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    Mockito.when(this.pbpFeign.getItemPickupPointL3Listing(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
        eq(Constant.CLIENT_ID), eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(pageable.getPageNumber()),
        eq(pageable.getPageSize()), Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(
        new GdnRestListResponse<>(null, null, false, Arrays.asList(itemPickupPointListingL3Response),
            new PageMetaData(PAGE, SIZE, SIZE), REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class,
          () -> this.pbpOutboundService.findProductDetailsFromL5(BUSINESS_PARTNER_CODE,
              productLevel3SummaryRequest, pageable, true));
    } finally {
      Mockito.verify(this.pbpFeign)
          .getItemPickupPointL3Listing(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
              eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(pageable.getPageNumber()), eq(pageable.getPageSize()),
              Mockito.any(ItemPickupPointListingL3Request.class));
    }
  }

  @Test
  public void findProductDetailsFromL5SuccessFalseWithEmptyResponseExceptionTest() throws Exception {
    ProductLevel3SummaryRequest productLevel3SummaryRequest = new ProductLevel3SummaryRequest();
    productLevel3SummaryRequest.setProductSkuList(Arrays.asList(PRODUCT_SKU));

    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    itemPickupPointListingL3Response.setItemSku(ITEM_SKU);
    Mockito.when(this.pbpFeign.getItemPickupPointL3Listing(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
        eq(Constant.CLIENT_ID), eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(pageable.getPageNumber()),
        eq(pageable.getPageSize()), Mockito.any(ItemPickupPointListingL3Request.class))).thenReturn(
        new GdnRestListResponse<>(null, null, false, new ArrayList<>(),
            new PageMetaData(PAGE, SIZE, SIZE), REQUEST_ID));
    try {
      Assertions.assertThrows(Exception.class,
          () -> this.pbpOutboundService.findProductDetailsFromL5(BUSINESS_PARTNER_CODE,
              productLevel3SummaryRequest, pageable, true));
    } finally {
      Mockito.verify(this.pbpFeign)
          .getItemPickupPointL3Listing(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
              eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(pageable.getPageNumber()), eq(pageable.getPageSize()),
              Mockito.any(ItemPickupPointListingL3Request.class));
    }
  }

  @Test
  public void listingUpdateTest() throws Exception {
    GdnRestSingleResponse<ItemPriceStockQuickUpdateResponse> gdnRestSingleResponse = new GdnRestSingleResponse<>();
    ItemPriceStockQuickUpdateResponse itemPriceStockQuickUpdateResponse = new ItemPriceStockQuickUpdateResponse();
    itemPriceStockQuickUpdateResponse.setApiErrorCode(null);
    itemPriceStockQuickUpdateResponse.setVariantsErrorList(Arrays.asList());
    gdnRestSingleResponse.setValue(itemPriceStockQuickUpdateResponse);
    gdnRestSingleResponse.setSuccess(true);
    Mockito.when(
            this.pbpFeign.listingPartialUpdate(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
                Constant.USER_NAME, PRODUCT_SKU, new ProductLevel3QuickEditV2Request()))
        .thenReturn(gdnRestSingleResponse);
    this.pbpOutboundService.listingUpdate(new ProductLevel3QuickEditV2Request(), PRODUCT_SKU);
    Mockito.verify(this.pbpFeign).listingPartialUpdate(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
        Constant.USER_NAME, PRODUCT_SKU, new ProductLevel3QuickEditV2Request());
  }

  @Test
  public void listingUpdateSuccessTrueWithErrorCodeTest() throws Exception {
    GdnRestSingleResponse<ItemPriceStockQuickUpdateResponse> gdnRestSingleResponse = new GdnRestSingleResponse<>();
    ItemPriceStockQuickUpdateResponse itemPriceStockQuickUpdateResponse = new ItemPriceStockQuickUpdateResponse();
    itemPriceStockQuickUpdateResponse.setApiErrorCode((ApiErrorCode.WARNA_AND_FAMILY_COLOR_ERROR));
    ItemErrorListResponse itemErrorListResponse = new ItemErrorListResponse();
    List<ItemErrorListResponse> itemErrorListResponseList = new ArrayList<>();
    itemErrorListResponseList.add(itemErrorListResponse);
    itemPriceStockQuickUpdateResponse.setVariantsErrorList(itemErrorListResponseList);
    gdnRestSingleResponse.setValue(itemPriceStockQuickUpdateResponse);
    gdnRestSingleResponse.setSuccess(true);
    Mockito.when(
        this.pbpFeign.listingPartialUpdate(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, PRODUCT_SKU, new ProductLevel3QuickEditV2Request())).thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(Exception.class,
          () -> this.pbpOutboundService.listingUpdate(new ProductLevel3QuickEditV2Request(),
              PRODUCT_SKU));
    } finally {
      Mockito.verify(this.pbpFeign)
          .listingPartialUpdate(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, PRODUCT_SKU, new ProductLevel3QuickEditV2Request());
    }
  }

  @Test
  public void listingUpdateSuccessFalseWithValueNull() throws Exception {
    GdnRestSingleResponse<ItemPriceStockQuickUpdateResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>();
    ItemPriceStockQuickUpdateResponse itemPriceStockQuickUpdateResponse =
        new ItemPriceStockQuickUpdateResponse();
    itemPriceStockQuickUpdateResponse.setApiErrorCode((ApiErrorCode.WARNA_AND_FAMILY_COLOR_ERROR));
    ItemErrorListResponse itemErrorListResponse = new ItemErrorListResponse();
    List<ItemErrorListResponse> itemErrorListResponseList = new ArrayList<>();
    itemErrorListResponseList.add(itemErrorListResponse);
    itemPriceStockQuickUpdateResponse.setVariantsErrorList(itemErrorListResponseList);
    gdnRestSingleResponse.setValue(null);
    gdnRestSingleResponse.setSuccess(false);
    Mockito.when(this.pbpFeign.listingPartialUpdate(Constant.STORE_ID, Constant.CHANNEL_ID,
        Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, PRODUCT_SKU,
        new ProductLevel3QuickEditV2Request())).thenReturn(gdnRestSingleResponse);
    try {
      this.pbpOutboundService.listingUpdate(new ProductLevel3QuickEditV2Request(), PRODUCT_SKU);
    } catch (Exception e) {
      Assertions.assertEquals(
          "Error while updating using listingUpdate, ApiErrorCode : null, error - null",
          e.getMessage());
    } finally {
      Mockito.verify(this.pbpFeign)
          .listingPartialUpdate(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
              Constant.REQUEST_ID, Constant.USER_NAME, PRODUCT_SKU,
              new ProductLevel3QuickEditV2Request());
    }
  }

  @Test
  public void listingUpdateResponseNullTest() throws Exception {
    GdnRestSingleResponse<ItemPriceStockQuickUpdateResponse> gdnRestSingleResponse =
        new GdnRestSingleResponse<>();
    gdnRestSingleResponse.setValue(null);
    gdnRestSingleResponse.setSuccess(false);
    Mockito.when(this.pbpFeign.listingPartialUpdate(Constant.STORE_ID, Constant.CHANNEL_ID,
        Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, PRODUCT_SKU,
        new ProductLevel3QuickEditV2Request())).thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(Exception.class,
          () -> this.pbpOutboundService.listingUpdate(new ProductLevel3QuickEditV2Request(),
              PRODUCT_SKU));
    } finally {
      Mockito.verify(this.pbpFeign)
          .listingPartialUpdate(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
              Constant.REQUEST_ID, Constant.USER_NAME, PRODUCT_SKU,
              new ProductLevel3QuickEditV2Request());
    }
  }

  @Test
  public void listingUpdateErrorListTest() throws Exception {
    GdnRestSingleResponse<ItemPriceStockQuickUpdateResponse> gdnRestSingleResponse = new GdnRestSingleResponse<>();
    ItemPriceStockQuickUpdateResponse itemPriceStockQuickUpdateResponse = new ItemPriceStockQuickUpdateResponse();
    itemPriceStockQuickUpdateResponse.setApiErrorCode(ApiErrorCode.WARNA_AND_FAMILY_COLOR_ERROR);
    ItemErrorListResponse itemErrorListResponse = new ItemErrorListResponse();
    itemErrorListResponse.setErrorMessage(Constant.ERROR_IN_ITEM_SKU);
    List<ItemErrorListResponse> itemErrorListResponseList = new ArrayList<>();
    itemErrorListResponseList.add(itemErrorListResponse);
    itemPriceStockQuickUpdateResponse.setVariantsErrorList(itemErrorListResponseList);
    gdnRestSingleResponse.setValue(itemPriceStockQuickUpdateResponse);
    gdnRestSingleResponse.setSuccess(false);
    Mockito.when(
            this.pbpFeign.listingPartialUpdate(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
                Constant.USER_NAME, PRODUCT_SKU, new ProductLevel3QuickEditV2Request()))
        .thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(Exception.class,
          () -> this.pbpOutboundService.listingUpdate(new ProductLevel3QuickEditV2Request(),
              PRODUCT_SKU));
    } finally {
      Mockito.verify(this.pbpFeign).listingPartialUpdate(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
          Constant.USER_NAME, PRODUCT_SKU, new ProductLevel3QuickEditV2Request());
    }
  }

  @Test
  public void listingUpdateSuccessFalseTest() throws Exception {
    GdnRestSingleResponse<ItemPriceStockQuickUpdateResponse> gdnRestSingleResponse = new GdnRestSingleResponse<>();
    ItemPriceStockQuickUpdateResponse itemPriceStockQuickUpdateResponse = new ItemPriceStockQuickUpdateResponse();
    itemPriceStockQuickUpdateResponse.setApiErrorCode(ApiErrorCode.WARNA_AND_FAMILY_COLOR_ERROR);
    itemPriceStockQuickUpdateResponse.setVariantsErrorList(new ArrayList<>());
    gdnRestSingleResponse.setValue(itemPriceStockQuickUpdateResponse);
    gdnRestSingleResponse.setSuccess(false);
    Mockito.when(
            this.pbpFeign.listingPartialUpdate(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
                Constant.USER_NAME, PRODUCT_SKU, new ProductLevel3QuickEditV2Request()))
        .thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(Exception.class,
          () -> this.pbpOutboundService.listingUpdate(new ProductLevel3QuickEditV2Request(),
              PRODUCT_SKU));
    } finally {
      Mockito.verify(this.pbpFeign).listingPartialUpdate(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
          Constant.USER_NAME, PRODUCT_SKU, new ProductLevel3QuickEditV2Request());
    }
  }

  @Test
  public void listingUpdateVariantsErrorListTest() throws Exception {
    GdnRestSingleResponse<ItemPriceStockQuickUpdateResponse> gdnRestSingleResponse = new GdnRestSingleResponse<>();
    ItemPriceStockQuickUpdateResponse itemPriceStockQuickUpdateResponse = new ItemPriceStockQuickUpdateResponse();
    itemPriceStockQuickUpdateResponse.setApiErrorCode(null);
    ItemErrorListResponse itemErrorListResponse = new ItemErrorListResponse();
    itemErrorListResponse.setErrorMessage(Constant.ERROR_IN_ITEM_SKU);
    List<ItemErrorListResponse> itemErrorListResponseList = new ArrayList<>();
    itemErrorListResponseList.add(itemErrorListResponse);
    itemPriceStockQuickUpdateResponse.setVariantsErrorList(itemErrorListResponseList);
    gdnRestSingleResponse.setValue(itemPriceStockQuickUpdateResponse);
    gdnRestSingleResponse.setSuccess(true);
    Mockito.when(
            this.pbpFeign.listingPartialUpdate(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
                Constant.USER_NAME, PRODUCT_SKU, new ProductLevel3QuickEditV2Request()))
        .thenReturn(gdnRestSingleResponse);
    try {
      Assertions.assertThrows(Exception.class,
          () -> this.pbpOutboundService.listingUpdate(new ProductLevel3QuickEditV2Request(),
              PRODUCT_SKU));
    } finally {
      Mockito.verify(this.pbpFeign).listingPartialUpdate(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
          Constant.USER_NAME, PRODUCT_SKU, new ProductLevel3QuickEditV2Request());
    }
  }

  @Test
  public void updateProductItemViewConfigTest() throws Exception {
    ProductLevel3ViewConfigStockRequest productLevel3ViewConfigStockRequest = new ProductLevel3ViewConfigStockRequest();
    Mockito.when(this.pbpFeign.updateProductItemViewConfig(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, ITEM_SKU, productLevel3ViewConfigStockRequest))
        .thenReturn(new GdnRestListResponse<>(null, null, true, null, null, REQUEST_ID));
    this.pbpOutboundService.updateProductItemViewConfig(ITEM_SKU, productLevel3ViewConfigStockRequest);
    Mockito.verify(this.pbpFeign)
        .updateProductItemViewConfig(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, ITEM_SKU, productLevel3ViewConfigStockRequest);
  }

  @Test
  public void updateProductItemViewConfigExceptionTest() throws Exception {
    ProductLevel3ViewConfigStockRequest productLevel3ViewConfigStockRequest = new ProductLevel3ViewConfigStockRequest();
    Mockito.when(this.pbpFeign.updateProductItemViewConfig(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, ITEM_SKU, productLevel3ViewConfigStockRequest))
        .thenReturn(new GdnRestListResponse<>(null, null, false, null, null, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.pbpOutboundService.updateProductItemViewConfig(ITEM_SKU,
              productLevel3ViewConfigStockRequest));
    } finally {
      Mockito.verify(this.pbpFeign)
          .updateProductItemViewConfig(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
              Constant.USER_NAME, ITEM_SKU, productLevel3ViewConfigStockRequest);
    }
  }

  @Test
  public void fetchProductLevel3SummaryByProductSkuListTest() throws Exception {
    Mockito.when(this.pbpFeign.getL5SummaryByProductSkuList(Constant.STORE_ID, Constant.CHANNEL_ID,
      Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, PAGE, SIZE,
      BUSINESS_PARTNER_CODE, true,
      new SimpleListStringRequest(Collections.singletonList(ITEM_SKU)))).thenReturn(
      new GdnRestListResponse<>(null, null, true, Collections.emptyList(),
        new PageMetaData(PAGE, SIZE, SIZE), REQUEST_ID));
    this.pbpOutboundService.fetchProductLevel3SummaryByProductSkuList(PAGE, SIZE,
      BUSINESS_PARTNER_CODE, true, Collections.singletonList(ITEM_SKU));
    Mockito.verify(this.pbpFeign)
      .getL5SummaryByProductSkuList(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, PAGE, SIZE, BUSINESS_PARTNER_CODE, true,
        new SimpleListStringRequest(Collections.singletonList(ITEM_SKU)));
  }

  @Test
  public void fetchProductLevel3SummaryByProductSkuListExceptionTest() throws Exception {
    Mockito.when(this.pbpFeign.getL5SummaryByProductSkuList(Constant.STORE_ID, Constant.CHANNEL_ID,
        Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, PAGE, SIZE,
        BUSINESS_PARTNER_CODE, true,
        new SimpleListStringRequest(Collections.singletonList(ITEM_SKU))))
      .thenReturn(new GdnRestListResponse<>(null, null, false, null, null, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.pbpOutboundService.fetchProductLevel3SummaryByProductSkuList(PAGE, SIZE,
              BUSINESS_PARTNER_CODE, true, Collections.singletonList(ITEM_SKU)));
    } finally {
      Mockito.verify(this.pbpFeign)
        .getL5SummaryByProductSkuList(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
          Constant.REQUEST_ID, Constant.USER_NAME, PAGE, SIZE, BUSINESS_PARTNER_CODE, true,
          new SimpleListStringRequest(Collections.singletonList(ITEM_SKU)));
    }
  }

  @Test
  public void createNewProductTest() throws Exception {
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    Mockito.when(
        this.pbpFeign.createNewProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, productCreationRequest)).thenReturn(new GdnBaseRestResponse(true));
    this.pbpOutboundService.createNewProduct(Constant.USER_NAME, Constant.REQUEST_ID, productCreationRequest);
    Mockito.verify(this.pbpFeign)
        .createNewProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, productCreationRequest);
  }

  @Test
  public void createNewProductExceptionTest() throws Exception {
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    Mockito.when(
        this.pbpFeign.createNewProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, productCreationRequest)).thenReturn(new GdnBaseRestResponse(false));
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.pbpOutboundService.createNewProduct(Constant.USER_NAME, Constant.REQUEST_ID,
              productCreationRequest));
    } finally {
      Mockito.verify(this.pbpFeign)
          .createNewProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
              Constant.USER_NAME, productCreationRequest);
    }
  }

  @Test
  public void getItemPickupPointListingL3ResponseTest() throws Exception {
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    Mockito.when(
            this.pbpFeign.getItemPickupPointListingL3Response(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), Mockito.anyBoolean(),
                Mockito.any(ItemPickupPointListingL3Request.class)))
        .thenReturn(new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(), REQUEST_ID));
    this.pbpOutboundService.getItemPickupPointListingL3Response(0, 1, itemPickupPointListingL3Request);
    Mockito.verify(this.pbpFeign)
        .getItemPickupPointListingL3Response(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, 0, 1, false, itemPickupPointListingL3Request);
  }

  @Test
  public void getItemPickupPointListingL3ResponseExceptionTest() throws Exception {
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    Mockito.when(
            this.pbpFeign.getItemPickupPointListingL3Response(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt(), Mockito.anyBoolean(),
                Mockito.any(ItemPickupPointListingL3Request.class)))
        .thenReturn(new GdnRestListResponse<>(null, null, false, null, null, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.pbpOutboundService.getItemPickupPointListingL3Response(0, 1,
              itemPickupPointListingL3Request));
    } finally {
      Mockito.verify(this.pbpFeign)
          .getItemPickupPointListingL3Response(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
              Constant.REQUEST_ID, Constant.USER_NAME, 0, 1, false, itemPickupPointListingL3Request);
    }
  }

  @Test
  public void updateSummaryL5Test() throws Exception {
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Mockito.when(this.pbpFeign.updateSummaryL5(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any(ProductVariantUpdateRequest.class)))
        .thenReturn(new GdnRestSingleResponse<>(new ItemsPriceStockImagesUpdateResponse(), REQUEST_ID));
    this.pbpOutboundService.updateSummaryL5(Constant.USER_NAME, productVariantUpdateRequest, Constant.CLIENT_ID);
    Mockito.verify(this.pbpFeign)
        .updateSummaryL5(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, productVariantUpdateRequest);
  }

  @Test
  public void updateSummaryL5ExceptionTest() throws Exception {
    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    Mockito.when(this.pbpFeign.updateSummaryL5(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.any(ProductVariantUpdateRequest.class))).thenReturn(
        new GdnRestSingleResponse<>(null, null, false, new ItemsPriceStockImagesUpdateResponse(), REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.pbpOutboundService.updateSummaryL5(Constant.USER_NAME,
              productVariantUpdateRequest, Constant.CLIENT_ID));
    } finally {
      Mockito.verify(this.pbpFeign)
          .updateSummaryL5(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
              Constant.USER_NAME, productVariantUpdateRequest);
    }
  }

  @Test
  public void deleteTerminatedSellerProductsTest() throws Exception {
    Mockito.when(pbpFeign.deleteTerminatedSellerProducts(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, PRODUCT_SKU))
        .thenReturn(new GdnBaseRestResponse(null, null, true, REQUEST_ID));
    pbpOutboundService.deleteTerminatedSellerProducts(PRODUCT_SKU);
    Mockito.verify(pbpFeign)
        .deleteTerminatedSellerProducts(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, PRODUCT_SKU);
  }


  @Test
  public void deleteTerminatedSellerProductsErrorTest() throws Exception {
    Mockito.when(pbpFeign.deleteTerminatedSellerProducts(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, PRODUCT_SKU))
        .thenReturn(new GdnBaseRestResponse(null, null, false, REQUEST_ID));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> pbpOutboundService.deleteTerminatedSellerProducts(PRODUCT_SKU));
    } finally {
      Mockito.verify(pbpFeign)
          .deleteTerminatedSellerProducts(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
              Constant.REQUEST_ID, Constant.USER_NAME, PRODUCT_SKU);
    }
  }

  @Test
  public void deleteL5ByPickupPointCodeTest() throws Exception {
    Mockito.when(pbpFeign.deleteL5ByPickupPointCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, new ItemSkuPickupPointRequest()))
        .thenReturn(new GdnRestListResponse<>(null, null, true, REQUEST_ID));
    pbpOutboundService.deleteL5ByPickupPointCode(new ItemSkuPickupPointRequest());
    Mockito.verify(pbpFeign)
        .deleteL5ByPickupPointCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, new ItemSkuPickupPointRequest());
  }


  @Test
  public void deleteL5ByPickupPointCodeExceptionTest() throws Exception {
    Mockito.when(pbpFeign.deleteL5ByPickupPointCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, new ItemSkuPickupPointRequest()))
        .thenReturn(new GdnRestListResponse<>(null, null, false, REQUEST_ID));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> pbpOutboundService.deleteL5ByPickupPointCode(new ItemSkuPickupPointRequest()));
    } finally {
      Mockito.verify(pbpFeign)
          .deleteL5ByPickupPointCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
              Constant.REQUEST_ID, Constant.USER_NAME, new ItemSkuPickupPointRequest());
    }
  }

  @Test
  public void getInProgressProductsByPickupPointCodeTest() {
    Mockito.when(pbpFeign.getInProgressProductsByPickupPointCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE, 0, 10))
        .thenReturn(new GdnRestListResponse<>(null, null, true, REQUEST_ID));
    pbpOutboundService.getInProgressProductsByPickupPointCode(BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE, 0, 10);
    Mockito.verify(pbpFeign)
        .getInProgressProductsByPickupPointCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE, 0, 10);
  }


  @Test
  public void getInProgressProductsByPickupPointCodeExceptionTest() {
    Mockito.when(pbpFeign.getInProgressProductsByPickupPointCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE, 0, 10))
        .thenReturn(new GdnRestListResponse<>(null, null, false, REQUEST_ID));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> pbpOutboundService.getInProgressProductsByPickupPointCode(BUSINESS_PARTNER_CODE,
              PICKUP_POINT_CODE, 0, 10));
    } finally {
      Mockito.verify(pbpFeign)
          .getInProgressProductsByPickupPointCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
              Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE, 0, 10);
    }
  }

  @Test
  public void createDefaultL5FbbTest() throws Exception {
    Mockito.when(pbpFeign
      .createDefaultL5Fbb(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, fbbCreatePickupPointRequest)).thenReturn(
      new GdnRestSingleResponse<>(null, null, true, new FbbCreatePickupPointResponse(),
        REQUEST_ID));
    pbpOutboundService.createDefaultL5Fbb(fbbCreatePickupPointRequest);
    Mockito.verify(pbpFeign)
      .createDefaultL5Fbb(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, fbbCreatePickupPointRequest);
  }

  @Test
  public void createDefaultL5FbbExceptionTest() throws Exception {
    Mockito.when(pbpFeign
      .createDefaultL5Fbb(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, fbbCreatePickupPointRequest)).thenReturn(
      new GdnRestSingleResponse<>(null, null, false, new FbbCreatePickupPointResponse(),
        REQUEST_ID));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> pbpOutboundService.createDefaultL5Fbb(fbbCreatePickupPointRequest));
    }
    finally {
      Mockito.verify(pbpFeign)
        .createDefaultL5Fbb(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, fbbCreatePickupPointRequest);
    }
  }

  @Test
  public void updateBrandOfProductTest() {
    Mockito.when(pbpFeign.updateBrandOfProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, PRODUCT_SKU, new BrandUpdateRequest(OLD_BRAND_CODE, NEW_BRAND_CODE)))
        .thenReturn(new GdnRestListResponse<>(null, null, true, REQUEST_ID));
    pbpOutboundService.updateBrandOfProduct(Constant.STORE_ID, new ProductBrandUpdateRequest(PRODUCT_SKU, OLD_BRAND_CODE, NEW_BRAND_CODE));
    Mockito.verify(pbpFeign)
        .updateBrandOfProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, PRODUCT_SKU, new BrandUpdateRequest(OLD_BRAND_CODE, NEW_BRAND_CODE));
  }


  @Test
  public void updateBrandOfProductExceptionTest() {
    Mockito.when(pbpFeign.updateBrandOfProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, PRODUCT_SKU, new BrandUpdateRequest(OLD_BRAND_CODE, NEW_BRAND_CODE)))
        .thenReturn(new GdnRestListResponse<>(null, null, false, REQUEST_ID));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> pbpOutboundService.updateBrandOfProduct(Constant.STORE_ID,
              new ProductBrandUpdateRequest(PRODUCT_SKU, OLD_BRAND_CODE, NEW_BRAND_CODE)));
    } finally {
      Mockito.verify(pbpFeign)
          .updateBrandOfProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
              Constant.REQUEST_ID, Constant.USER_NAME, PRODUCT_SKU, new BrandUpdateRequest(OLD_BRAND_CODE, NEW_BRAND_CODE));
    }
  }

  @Test
  public void getProductDetailsOfBusinessPartnerFromPBP_SuccessTest() {
    ReflectionTestUtils.setField(pbpOutboundService, "fetchBatchSizeForNRProductDetails", 10);
    GdnRestListResponse<ProductCodeAndNameDetails> response = new GdnRestListResponse<>();
    List<ProductCodeAndNameDetails> products = new ArrayList<>();
    ProductCodeAndNameDetails productCodeAndNameDetails = new ProductCodeAndNameDetails();
    productCodeAndNameDetails.setProductCode(PRODUCT_SKU);
    productCodeAndNameDetails.setProductName("productName");
    products.add(productCodeAndNameDetails);
    response.setContent(products);
    response.setSuccess(true);
    PageMetaData pageMetaData = new PageMetaData();
    pageMetaData.setTotalRecords(20);
    response.setPageMetaData(pageMetaData);
    Mockito.when(
            pbpFeign.getProductsByBusinessPartnerCodeFromLastXDays(Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), Mockito.anyInt() ,Mockito.anyInt()))
        .thenReturn(response);

    ProductCodeAndNameResponseList result =
        pbpOutboundService.getProductDetailsOfBusinessPartnerFromPBP(Constant.STORE_ID, REQUEST_ID,
            BUSINESS_PARTNER_CODE);
    
    Mockito.verify(pbpFeign, Mockito.times(2))
        .getProductsByBusinessPartnerCodeFromLastXDays(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt());
  }

  @Test
    public void getProductDetailsOfBusinessPartnerFromPBP_FailureTest() {
    ReflectionTestUtils.setField(pbpOutboundService, "fetchBatchSizeForNRProductDetails", 10);
    GdnRestListResponse<ProductCodeAndNameDetails> response = new GdnRestListResponse<>();
    response.setSuccess(false);
    response.setContent(null);
    response.setErrorMessage("Error while fetching products");

    Mockito.when(
            pbpFeign.getProductsByBusinessPartnerCodeFromLastXDays(Mockito.anyString(), Mockito.anyString(),
                Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt()))
        .thenReturn(response);

    ApplicationRuntimeException exception = Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> pbpOutboundService.getProductDetailsOfBusinessPartnerFromPBP(Constant.STORE_ID, REQUEST_ID,
            BUSINESS_PARTNER_CODE));

    Mockito.verify(pbpFeign, Mockito.times(1))
        .getProductsByBusinessPartnerCodeFromLastXDays(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt());
  }

  @Test
  public void getProductDetailsOfBusinessPartnerFromPBP_EmptyContentTest() {
    try {
      ReflectionTestUtils.setField(pbpOutboundService, "fetchBatchSizeForNRProductDetails", 10);
      GdnRestListResponse<ProductCodeAndNameDetails> response = new GdnRestListResponse<>();
      response.setSuccess(true);
      response.setContent(new ArrayList<>());
      PageMetaData pageMetaData = new PageMetaData();
      pageMetaData.setTotalRecords(0);
      response.setPageMetaData(pageMetaData);
      Mockito.when(pbpFeign.getProductsByBusinessPartnerCodeFromLastXDays(Mockito.anyString(), Mockito.anyString(),
          Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt())).thenReturn(response);

      ProductCodeAndNameResponseList result =
          pbpOutboundService.getProductDetailsOfBusinessPartnerFromPBP(Constant.STORE_ID, REQUEST_ID,
              BUSINESS_PARTNER_CODE);
    }
    catch(ApplicationRuntimeException e) {
      Assertions.assertEquals(e.getErrorCodes().getCode(), "UNSPECIFIED");
    }
    finally {
      Mockito.verify(pbpFeign, Mockito.times(1))
          .getProductsByBusinessPartnerCodeFromLastXDays(Mockito.anyString(), Mockito.anyString(),
              Mockito.anyString(), Mockito.anyInt(), Mockito.anyInt());
    }

  }

  @Test
  void testUpdatedProductMaterData_success() {
    String productSku = "SKU123";
    ProductMasterDataEditRequest request = ProductMasterDataEditRequest.builder()
        .productSku(productSku)
        .productName("Test Product")
        .build();
    GdnBaseRestResponse successResponse = new GdnBaseRestResponse();
    successResponse.setSuccess(true);
    Mockito.when(pbpFeign.updatedProductMaterData(
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.eq(productSku), Mockito.eq(request)))
        .thenReturn(successResponse);
    Assertions.assertDoesNotThrow(() ->
        pbpOutboundService.updatedProductMaterData(productSku, request, ""));
    Mockito.verify(pbpFeign).updatedProductMaterData(
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.eq(productSku), Mockito.eq(request));
  }

  @Test
  void testUpdatedProductMaterData_unsuccessfulResponse_throwsException() {
    String productSku = "SKU123";
    ProductMasterDataEditRequest request = ProductMasterDataEditRequest.builder().build();

    GdnBaseRestResponse errorResponse = new GdnBaseRestResponse();
    errorResponse.setSuccess(false);
    errorResponse.setErrorMessage("Update failed");

    Mockito.when(pbpFeign.updatedProductMaterData(
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.eq(productSku), Mockito.eq(request)))
        .thenReturn(errorResponse);

    ApplicationRuntimeException ex = Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> pbpOutboundService.updatedProductMaterData(productSku, request, ""));

    Mockito.verify(pbpFeign).updatedProductMaterData(
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.eq(productSku), Mockito.eq(request));

    Assertions.assertEquals("Unspecified error :System error", ex.getMessage());
  }

  @Test
  void testUpdatedProductMaterData_success_but_error_msg_preset_test() {
    String productSku = "SKU123";
    ProductMasterDataEditRequest request = ProductMasterDataEditRequest.builder()
        .productSku(productSku)
        .productName("Test Product")
        .build();
    GdnBaseRestResponse successResponse = new GdnBaseRestResponse();
    successResponse.setSuccess(true);
    successResponse.setErrorMessage("Update successful with warnings");
    Mockito.when(pbpFeign.updatedProductMaterData(
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.eq(productSku), Mockito.eq(request)))
        .thenReturn(successResponse);
    ApplicationRuntimeException ex = Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> pbpOutboundService.updatedProductMaterData(productSku, request, ""));
    Mockito.verify(pbpFeign).updatedProductMaterData(
        Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
        Mockito.anyString(), Mockito.anyString(), Mockito.eq(productSku), Mockito.eq(request));
  }

  @Test
  void testUpdateProductBrandName_Success() {
    InternalBrandUpdateEventModel eventModel =
        InternalBrandUpdateEventModel.builder().storeId("10001").oldBrandCode("BRAND123")
            .processType("INTERNAL_BRAND_UPDATE").updatedBy("testuser").build();

    GdnBaseRestResponse successResponse = new GdnBaseRestResponse();
    successResponse.setSuccess(true);
    successResponse.setErrorCode("SUCCESS");
    successResponse.setErrorCode(null);// ensure it does not match UNSPECIFIED

    Mockito.when(pbpFeign.updateProductBrandName(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any()))
        .thenReturn(successResponse);

    GdnBaseRestResponse result =
        pbpOutboundService.updateProductBrandName(eventModel, new BulkInternalProcessData());

    Assertions.assertNotNull(result);
    Assertions.assertTrue(result.isSuccess());
    Mockito.verify(pbpFeign)
        .updateProductBrandName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }

  @Test
  void testUpdateProductBrandName_Failure_UnspecifiedError() {
    InternalBrandUpdateEventModel eventModel =
        InternalBrandUpdateEventModel.builder().storeId("10001").oldBrandCode("BRAND123")
            .processType("INTERNAL_BRAND_UPDATE").updatedBy("testuser").build();

    GdnBaseRestResponse failureResponse = new GdnBaseRestResponse();
    failureResponse.setSuccess(false);
    failureResponse.setErrorCode(ErrorCategory.UNSPECIFIED.getCode()); // triggers exception
    failureResponse.setErrorMessage("Brand update failed");

    Mockito.when(pbpFeign.updateProductBrandName(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any()))
        .thenReturn(failureResponse);

    ApplicationRuntimeException ex = Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> pbpOutboundService.updateProductBrandName(eventModel, new BulkInternalProcessData()));

    Assertions.assertEquals("Unspecified error :Failed to update the brand due to a PBP service error", ex.getErrorMessage());

    Mockito.verify(pbpFeign)
        .updateProductBrandName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }

  @Test
  void testUpdateProductBrandName_Failure_NullResponse() {
    InternalBrandUpdateEventModel eventModel =
        InternalBrandUpdateEventModel.builder().storeId("10001").oldBrandCode("BRAND123")
            .processType("INTERNAL_BRAND_UPDATE").updatedBy("testuser").build();

    Mockito.when(pbpFeign.updateProductBrandName(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.anyString(), Mockito.any()))
        .thenReturn(null); // triggers exception path

    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> pbpOutboundService.updateProductBrandName(eventModel, new BulkInternalProcessData()));

    Mockito.verify(pbpFeign)
        .updateProductBrandName(Mockito.anyString(), Mockito.anyString(), Mockito.anyString(),
            Mockito.anyString(), Mockito.anyString(), Mockito.any());
  }

}