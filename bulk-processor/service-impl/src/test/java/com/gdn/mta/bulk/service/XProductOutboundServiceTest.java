package com.gdn.mta.bulk.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.dto.ItemBasicL4Response;
import com.gdn.mta.bulk.models.ProductL3Response;
import com.gdn.mta.bulk.models.download.responsedata.BulkDownloadProductBasicInfoResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductBasicResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductSkuResponse;
import com.gdn.x.product.rest.web.model.L3VersionResponse;
import com.gdn.x.product.rest.web.model.request.DeleteItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.GetProductInfoRequestV2;
import com.gdn.x.product.rest.web.model.request.ItemLevel4ListingWebRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuSummaryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.bulk.feignConfig.XProductFeign;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.x.product.model.vo.SimpleStringBooleanMapRequest;
import com.gdn.x.product.model.vo.UnmappedSkuResponse;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointViewConfigBaseRequest;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigBaseRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.SimpleSetStringRequest;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemCodeDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemInfoResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.product.rest.web.model.response.SimpleListStringResponse;

public class XProductOutboundServiceTest {

  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATALOG_CODE = "catalogCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String ITEM_SKU = "item-sku";
  private static final String PICKUP_POINT = "pickupPoint";
  private Map<String, Boolean> productSkuBooleanMap = new HashMap<>();
  private ProductL3SummaryResponse productL3SummaryResponse = new ProductL3SummaryResponse();
  private ItemSummaryRequest itemSummaryRequest = new ItemSummaryRequest();
  private Pageable pageable = PageRequest.of(PAGE, SIZE);
  private ProductSkuSummaryRequest productSkuSummaryRequest = new ProductSkuSummaryRequest();
  private ItemPickupPointListingRequest itemPickupPointListingRequest =
    new ItemPickupPointListingRequest();
  private ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();

  @Mock
  private XProductFeign xProductFeign;

  @InjectMocks
  private XProductOutboundServiceImpl xProductOutboundService;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    productSkuBooleanMap.put(PRODUCT_SKU, true);
    productL3SummaryResponse.setProductSku(PRODUCT_SKU);
    ReflectionTestUtils.setField(xProductOutboundService, "dormantSellerErrorSuppressErrorCodes",
        "ERR-XP-400002,ERR-XP-400007");
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(xProductFeign);
  }

  @Test
  public void getUnmappedSkusTest() throws Exception {
    UnmappedSkuResponse unmappedSkuResponse = new UnmappedSkuResponse();
    GdnRestListResponse gdnRestListResponse =
        new GdnRestListResponse(Arrays.asList(unmappedSkuResponse), new PageMetaData(0, 0, 0), Constant.REQUEST_ID);
    when(xProductFeign
        .getUnmappedProductSkus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, Arrays.asList(CATEGORY_CODE))).thenReturn(gdnRestListResponse);
    List<UnmappedSkuResponse> response = xProductOutboundService.getUnmappedSkus(Arrays.asList(CATEGORY_CODE));
    verify(xProductFeign)
        .getUnmappedProductSkus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, Arrays.asList(CATEGORY_CODE));
  }

  @Test
  public void getUnmappedSkusSuccessFalseTest() throws Exception {
    GdnRestListResponse gdnRestListResponse = new GdnRestListResponse();
    when(xProductFeign
        .getUnmappedProductSkus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, Arrays.asList(CATEGORY_CODE))).thenReturn(gdnRestListResponse);
    try {
      Assertions.assertThrows(Exception.class,
          () -> xProductOutboundService.getUnmappedSkus(Arrays.asList(CATEGORY_CODE)));
    } finally {
      verify(xProductFeign)
          .getUnmappedProductSkus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
              Constant.USER_NAME, Arrays.asList(CATEGORY_CODE));
    }
  }

  @Test
  public void bulkUpdateOff2OnByProductSkusTest() {
    when(this.xProductFeign
        .bulkUpdateOff2OnActiveFlagByProductSkus(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
            eq(Constant.CLIENT_ID), eq(Constant.REQUEST_ID), eq(Constant.USER_NAME),
            eq(Boolean.TRUE), any(SimpleStringBooleanMapRequest.class))).thenReturn(
        new GdnRestSingleResponse<>(null, null, true,
            new SimpleListStringResponse(Arrays.asList(PRODUCT_SKU)), Constant.REQUEST_ID));
    SimpleListStringResponse response = this.xProductOutboundService
        .bulkUpdateOff2OnByProductSkus(new HashMap<>(), Constant.REQUEST_ID, Constant.USER_NAME, Boolean.TRUE);
    verify(this.xProductFeign)
        .bulkUpdateOff2OnActiveFlagByProductSkus(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
            eq(Constant.CLIENT_ID), eq(Constant.REQUEST_ID), eq(Constant.USER_NAME),
            eq(Boolean.TRUE), any(SimpleStringBooleanMapRequest.class));
    Assertions.assertEquals(PRODUCT_SKU, response.getValue().get(0));
  }

  @Test
  public void bulkUpdateOff2OnByProductSkus_successFalseTest() {
    when(this.xProductFeign
        .bulkUpdateOff2OnActiveFlagByProductSkus(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
            eq(Constant.CLIENT_ID), eq(Constant.REQUEST_ID), eq(Constant.USER_NAME),
            eq(Boolean.TRUE), any(SimpleStringBooleanMapRequest.class)))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, Constant.REQUEST_ID));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> this.xProductOutboundService.bulkUpdateOff2OnByProductSkus(new HashMap<>(),
              Constant.REQUEST_ID, Constant.USER_NAME, Boolean.TRUE));
    } finally {
      verify(this.xProductFeign)
          .bulkUpdateOff2OnActiveFlagByProductSkus(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
              eq(Constant.CLIENT_ID), eq(Constant.REQUEST_ID), eq(Constant.USER_NAME),
              eq(Boolean.TRUE), any(SimpleStringBooleanMapRequest.class));
    }
  }

  @Test
  public void getProductL3SummaryResponseTest() {
    Mockito.when(this.xProductFeign
        .getFilterSummaryL3(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
            eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(PAGE), eq(SIZE),
            any(ProductSummaryRequest.class))).thenReturn(
        new GdnRestListResponse<>(null, null, true, Arrays.asList(productL3SummaryResponse),
            new PageMetaData(PAGE, SIZE, 1), Constant.REQUEST_ID));
    Page<ProductL3SummaryResponse> response = xProductOutboundService
        .getProductL3SummaryResponse(new ProductSummaryRequest(), PAGE, SIZE, Constant.REQUEST_ID,
            Constant.USER_NAME);
    verify(this.xProductFeign).getFilterSummaryL3(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
        eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(PAGE), eq(SIZE),
        any(ProductSummaryRequest.class));
    Assertions.assertEquals(PRODUCT_SKU, response.getContent().get(0).getProductSku());
    Assertions.assertEquals(1, response.getTotalElements());
  }

  @Test
  public void getProductL3SummaryResponse_successFalseTest() {
    Mockito.when(this.xProductFeign
        .getFilterSummaryL3(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
            eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(PAGE), eq(SIZE),
            any(ProductSummaryRequest.class))).thenReturn(
        (GdnRestListResponse<ProductL3SummaryResponse>) new GdnRestListResponse<>(null, null, false,
            Collections.EMPTY_LIST, null, null));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> xProductOutboundService.getProductL3SummaryResponse(new ProductSummaryRequest(),
              PAGE, SIZE, Constant.REQUEST_ID, Constant.USER_NAME));
    } finally {
      verify(this.xProductFeign)
          .getFilterSummaryL3(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
              eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(PAGE), eq(SIZE),
              any(ProductSummaryRequest.class));
    }
  }


  @Test
  public void getItemDetailsByItemCodesTest() {
    Mockito.when(this.xProductFeign
        .getItemDetailsByItemCodes(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
            eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), any(SimpleSetStringRequest.class))).thenReturn(
        (GdnRestListResponse<ItemCodeDetailResponse>) new GdnRestListResponse<>(null, null, true,
            Collections.EMPTY_LIST, null, null));
    List<ItemCodeDetailResponse> response = xProductOutboundService
        .getItemDetailsByItemCodes(Constant.REQUEST_ID, Constant.USER_NAME, new SimpleSetStringRequest());
    verify(xProductFeign)
        .getItemDetailsByItemCodes(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
            eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), any(SimpleSetStringRequest.class));
  }

  @Test
  public void getItemDetailsByItemCodesExceptionTest() {
    Mockito.when(this.xProductFeign
        .getItemDetailsByItemCodes(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
            eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), any(SimpleSetStringRequest.class))).thenReturn(
        (GdnRestListResponse<ItemCodeDetailResponse>) new GdnRestListResponse<>(null, null, false,
            Collections.EMPTY_LIST, null, null));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> xProductOutboundService.getItemDetailsByItemCodes(Constant.REQUEST_ID,
              Constant.USER_NAME, new SimpleSetStringRequest()));
    } finally {
      verify(xProductFeign)
          .getItemDetailsByItemCodes(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
              eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), any(SimpleSetStringRequest.class));
    }
  }

  @Test
  public void getItemSummaryByFilterTest() {
    Mockito.when(
      xProductFeign.getItemSummaryByFilter(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, REQUEST_ID, USERNAME,
        PAGE, SIZE, itemSummaryRequest)).thenReturn(
      new GdnRestListResponse<>(null, null, true, Collections.EMPTY_LIST, new PageMetaData(SIZE, PAGE, SIZE), REQUEST_ID));
    Page<ItemSummaryResponse> response =
      this.xProductOutboundService.getItemSummaryByFilter(REQUEST_ID, USERNAME, pageable, itemSummaryRequest);
    Mockito.verify(this.xProductFeign)
      .getItemSummaryByFilter(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, REQUEST_ID, USERNAME, PAGE, SIZE,
        itemSummaryRequest);
  }

  @Test
  public void getItemSummaryByFilter_successFalseTest() {
    Mockito.when(
      xProductFeign.getItemSummaryByFilter(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, REQUEST_ID, USERNAME,
        PAGE, SIZE, itemSummaryRequest)).thenReturn(new GdnRestListResponse<>(null, null, false, null, null, REQUEST_ID));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> this.xProductOutboundService.getItemSummaryByFilter(REQUEST_ID, USERNAME, pageable,
              itemSummaryRequest));
    } finally {
      Mockito.verify(this.xProductFeign)
        .getItemSummaryByFilter(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, REQUEST_ID, USERNAME, PAGE, SIZE,
          itemSummaryRequest);
    }
  }

  @Test
  public void updateProductItemViewConfigTest() throws Exception {
    ItemViewConfigBaseRequest itemViewConfigBaseRequest = ItemViewConfigBaseRequest.builder().build();
    Mockito.when(
            xProductFeign.updateItemViewConfigWithItemStatus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
                Constant.REQUEST_ID, Constant.USER_NAME, ITEM_SKU, itemViewConfigBaseRequest))
        .thenReturn(new GdnBaseRestResponse(null, null, true, Constant.REQUEST_ID));
    this.xProductOutboundService.updateProductItemViewConfig(ITEM_SKU, itemViewConfigBaseRequest);
    Mockito.verify(this.xProductFeign)
        .updateItemViewConfigWithItemStatus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, ITEM_SKU, itemViewConfigBaseRequest);
  }

  @Test
  public void updateProductItemViewConfigFailedExceptionTest() throws Exception {
    ItemViewConfigBaseRequest itemViewConfigBaseRequest = ItemViewConfigBaseRequest.builder().build();
    Mockito.when(
            xProductFeign.updateItemViewConfigWithItemStatus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
                Constant.REQUEST_ID, Constant.USER_NAME, ITEM_SKU, itemViewConfigBaseRequest))
        .thenReturn(new GdnBaseRestResponse(null, null, false, Constant.REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.xProductOutboundService.updateProductItemViewConfig(ITEM_SKU,
              itemViewConfigBaseRequest));
    } finally {
      Mockito.verify(this.xProductFeign)
          .updateItemViewConfigWithItemStatus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
              Constant.REQUEST_ID, Constant.USER_NAME, ITEM_SKU, itemViewConfigBaseRequest);
    }
  }

  @Test
  public void updateItemPickupPointViewConfigWithProductStatusTest() throws Exception{
    ItemPickupPointViewConfigBaseRequest itemPickupPointViewConfigBaseRequest =
        new ItemPickupPointViewConfigBaseRequest();
    itemPickupPointViewConfigBaseRequest.setBuyable(false);
    itemPickupPointViewConfigBaseRequest.setDiscoverable(false);
    itemPickupPointViewConfigBaseRequest.setCncActivated(false);
    Mockito.when(xProductFeign.updateItemPickupPointViewConfigWithProductStatus(Constant.STORE_ID, Constant.CHANNEL_ID,
            Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, PRODUCT_SKU, itemPickupPointViewConfigBaseRequest))
        .thenReturn(new GdnBaseRestResponse(null, null, true, Constant.REQUEST_ID));
    xProductOutboundService.updateItemPickupPointViewConfigWithProductStatus(PRODUCT_SKU, itemPickupPointViewConfigBaseRequest);
    Mockito.verify(this.xProductFeign)
        .updateItemPickupPointViewConfigWithProductStatus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, PRODUCT_SKU, itemPickupPointViewConfigBaseRequest);
  }

  @Test
  public void updateItemPickupPointViewConfigWithProductStatusExceptionTest() throws Exception{
    ItemPickupPointViewConfigBaseRequest itemPickupPointViewConfigBaseRequest =
        new ItemPickupPointViewConfigBaseRequest();
    itemPickupPointViewConfigBaseRequest.setBuyable(false);
    itemPickupPointViewConfigBaseRequest.setDiscoverable(false);
    itemPickupPointViewConfigBaseRequest.setCncActivated(false);
    Mockito.when(xProductFeign.updateItemPickupPointViewConfigWithProductStatus(Constant.STORE_ID, Constant.CHANNEL_ID,
            Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, PRODUCT_SKU, itemPickupPointViewConfigBaseRequest))
        .thenReturn(new GdnBaseRestResponse(null, null, false, Constant.REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.xProductOutboundService.updateItemPickupPointViewConfigWithProductStatus(PRODUCT_SKU,
          itemPickupPointViewConfigBaseRequest));
    } finally {
      Mockito.verify(this.xProductFeign)
          .updateItemPickupPointViewConfigWithProductStatus(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
              Constant.REQUEST_ID, Constant.USER_NAME, PRODUCT_SKU, itemPickupPointViewConfigBaseRequest);
    }
  }

  @Test
  public void getProductAndItemsWithProductDataTest() {
    Mockito.when(xProductFeign
        .getProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, PRODUCT_SKU, true, true)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new ProductAndItemsResponse(),
            GdnMandatoryRequestParameterUtil.getRequestId()));
    GdnRestSingleResponse response = xProductOutboundService.getProductAndItemsWithProductData(false, PRODUCT_SKU, true);
    Mockito.verify(xProductFeign)
        .getProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), false, PRODUCT_SKU, true, true);
  }

  @Test
  public void getProductAndItemsWithProductDataExceptionTest() {
    Mockito.when(xProductFeign
            .getProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
                Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
                GdnMandatoryRequestParameterUtil.getUsername(), false, PRODUCT_SKU, true, true))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, null));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> xProductOutboundService.getProductAndItemsWithProductData(false, PRODUCT_SKU,
              true));
    } finally {
      Mockito.verify(xProductFeign)
          .getProductAndItems(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), false, PRODUCT_SKU, true, true);
    }
  }

  @Test
  public void getProductInfoByItemSkuTest() {
    GetProductInfoRequestV2 requestV2 = new GetProductInfoRequestV2();
    requestV2.setItemSkus(Set.of(ITEM_SKU));
    ProductAndItemInfoResponseV2 productAndItemInfoResponseV2 = new ProductAndItemInfoResponseV2();
    Mockito.when(xProductFeign.getProductInfoByItemSku(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        requestV2)).thenReturn(
        new GdnRestListResponse<>(null, null, true, Arrays.asList(productAndItemInfoResponseV2),
            new PageMetaData(0L, 0L, 1L), REQUEST_ID));
    GdnRestListResponse<ProductAndItemInfoResponseV2> response =
        xProductOutboundService.getProductInfoByItemSku(requestV2);
    Mockito.verify(xProductFeign)
        .getProductInfoByItemSku(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            requestV2);
  }

  @Test
  public void getProductInfoByItemSkuExceptionTest() {
    GetProductInfoRequestV2 requestV2 = new GetProductInfoRequestV2();
    requestV2.setItemSkus(Set.of(ITEM_SKU));
    ProductAndItemInfoResponseV2 productAndItemInfoResponseV2 = new ProductAndItemInfoResponseV2();
    Mockito.when(xProductFeign.getProductInfoByItemSku(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        requestV2)).thenReturn(
        new GdnRestListResponse<>(null, null, false, Arrays.asList(productAndItemInfoResponseV2),
            new PageMetaData(0L, 0L, 1L), REQUEST_ID));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> xProductOutboundService.getProductInfoByItemSku(requestV2));
    } finally {
      Mockito.verify(xProductFeign).getProductInfoByItemSku(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), requestV2);
    }
  }

  @Test
  public void addSalesCategoryTest() {
    Mockito.when(xProductFeign
        .addProductSalesCatalog(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, CATALOG_CODE, CATEGORY_CODE, new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU))))
        .thenReturn(new GdnBaseRestResponse(null, null, true, Constant.REQUEST_ID));
    this.xProductOutboundService
        .addSalesCategory(Constant.STORE_ID, CATALOG_CODE, CATEGORY_CODE, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(this.xProductFeign)
        .addProductSalesCatalog(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, CATALOG_CODE, CATEGORY_CODE, new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU)));
  }

  @Test
  public void addSalesCategoryExceptionTest() {
    Mockito.when(xProductFeign
        .addProductSalesCatalog(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, CATALOG_CODE, CATEGORY_CODE, new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU))))
        .thenReturn(new GdnBaseRestResponse(null, null, false, Constant.REQUEST_ID));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> this.xProductOutboundService.addSalesCategory(Constant.STORE_ID, CATALOG_CODE,
              CATEGORY_CODE, Arrays.asList(PRODUCT_SKU)));
    } finally {
      Mockito.verify(this.xProductFeign)
          .addProductSalesCatalog(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
              Constant.USER_NAME, CATALOG_CODE, CATEGORY_CODE, new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU)));
    }
  }

  @Test
  public void deleteSalesCategoryTest() {
    Mockito.when(xProductFeign
        .deleteSalesCatalog(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, CATALOG_CODE, CATEGORY_CODE, new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU))))
        .thenReturn(new GdnBaseRestResponse(null, null, true, Constant.REQUEST_ID));
    this.xProductOutboundService
        .deleteSalesCategory(Constant.STORE_ID, CATALOG_CODE, CATEGORY_CODE, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(this.xProductFeign)
        .deleteSalesCatalog(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, CATALOG_CODE, CATEGORY_CODE, new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU)));
  }

  @Test
  public void deleteSalesCategoryExceptionTest() {
    Mockito.when(xProductFeign
        .deleteSalesCatalog(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, CATALOG_CODE, CATEGORY_CODE, new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU))))
        .thenReturn(new GdnBaseRestResponse(null, null, false, Constant.REQUEST_ID));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> this.xProductOutboundService.deleteSalesCategory(Constant.STORE_ID, CATALOG_CODE,
              CATEGORY_CODE, Arrays.asList(PRODUCT_SKU)));
    } finally {
      Mockito.verify(this.xProductFeign)
          .deleteSalesCatalog(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
              Constant.USER_NAME, CATALOG_CODE, CATEGORY_CODE, new SimpleListStringRequest(Arrays.asList(PRODUCT_SKU)));
    }
  }

  @Test
  public void archiveByItemSkuTest() {
    Mockito.when(
        xProductFeign.toggleArchiveProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
          Constant.REQUEST_ID, Constant.USER_NAME, true, ITEM_SKU))
      .thenReturn(new GdnBaseRestResponse(null, null, true, Constant.REQUEST_ID));
    this.xProductOutboundService.archiveByProductSku(ITEM_SKU, true, false);
    Mockito.verify(this.xProductFeign)
      .toggleArchiveProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, true, ITEM_SKU);
  }

  @Test
  public void archiveByItemSkuErrorCodeTest() {
    Mockito.when(
            xProductFeign.toggleArchiveProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
                Constant.REQUEST_ID, Constant.USER_NAME, true, ITEM_SKU))
        .thenReturn(new GdnBaseRestResponse(null, "ERR-XP-400002", false, Constant.REQUEST_ID));
    this.xProductOutboundService.archiveByProductSku(ITEM_SKU, true, true);
    Mockito.verify(this.xProductFeign)
        .toggleArchiveProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, true, ITEM_SKU);
  }

  @Test
  public void archiveByItemSkuNullResponseTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> this.xProductOutboundService.archiveByProductSku(ITEM_SKU, true, true));
    Mockito.verify(this.xProductFeign)
        .toggleArchiveProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, true, ITEM_SKU);
  }

  @Test
  public void archiveByItemSku_successFalseTest() {
    Mockito.when(
        xProductFeign.toggleArchiveProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
          Constant.REQUEST_ID, Constant.USER_NAME, true, ITEM_SKU))
      .thenReturn(new GdnBaseRestResponse(null, PRODUCT_SKU, false, Constant.REQUEST_ID));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> this.xProductOutboundService.archiveByProductSku(ITEM_SKU, true, true));
    } finally {
      Mockito.verify(this.xProductFeign)
        .toggleArchiveProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
          Constant.REQUEST_ID, Constant.USER_NAME, true, ITEM_SKU);
    }
  }

  @Test
  public void archiveByItemSkuErrorMessageEmptyTest() {
    Mockito.when(
            xProductFeign.toggleArchiveProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
                Constant.REQUEST_ID, Constant.USER_NAME, true, ITEM_SKU))
        .thenReturn(new GdnBaseRestResponse(null, null, false, Constant.REQUEST_ID));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> this.xProductOutboundService.archiveByProductSku(ITEM_SKU, true, true));
    } finally {
      Mockito.verify(this.xProductFeign)
          .toggleArchiveProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
              Constant.REQUEST_ID, Constant.USER_NAME, true, ITEM_SKU);
    }
  }

  @Test
  public void getProductSkuSummaryResponseTest() {
    Mockito.when(
      xProductFeign.getProductSkuSummary(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, PAGE, SIZE,
        productSkuSummaryRequest)).thenReturn(
      new GdnRestListResponse<>(null, null, true, Collections.emptyList(),
        new PageMetaData(PAGE, SIZE, SIZE), REQUEST_ID));
    this.xProductOutboundService.getProductSkuSummaryResponse(BUSINESS_PARTNER_CODE, PAGE, SIZE,
      productSkuSummaryRequest);
    Mockito.verify(this.xProductFeign)
      .getProductSkuSummary(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, PAGE, SIZE,
        productSkuSummaryRequest);
  }

  @Test
  public void getProductSkuSummaryResponse_successFalseTest() {
    Mockito.when(
      xProductFeign.getProductSkuSummary(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, PAGE, SIZE,
        productSkuSummaryRequest)).thenReturn(
      new GdnRestListResponse<>(null, null, false, Collections.emptyList(),
        new PageMetaData(PAGE, SIZE, SIZE), REQUEST_ID));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> this.xProductOutboundService.getProductSkuSummaryResponse(BUSINESS_PARTNER_CODE,
              PAGE, SIZE, productSkuSummaryRequest));
    } finally {
      Mockito.verify(this.xProductFeign)
        .getProductSkuSummary(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
          Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, PAGE, SIZE,
          productSkuSummaryRequest);
    }
  }

  @Test
  public void getItemPickupPointCodeByItemSkusTest() {
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(ITEM_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT);
    Mockito.when(
        xProductFeign.getItemPickupPointCodeByItemSkus(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID), eq(Constants.DEFAULT_USERNAME),
            any())).thenReturn(
        new GdnRestListResponse<>(null, null, true, Arrays.asList(itemSkuPickupPointCodeResponse),
            new PageMetaData(PAGE, SIZE, SIZE), REQUEST_ID));
    SimpleListStringRequest itemSkuList = new SimpleListStringRequest();
    itemSkuList.setValue(Arrays.asList(ITEM_SKU));
    this.xProductOutboundService.getItemPickupPointCodeByItemSkus(itemSkuList);
    Mockito.verify(xProductFeign)
        .getItemPickupPointCodeByItemSkus(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID), eq(Constants.DEFAULT_USERNAME),
            any());
  }

  @Test
  public void getItemPickupPointCodeByItemSkusListEmptyTest() {
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(ITEM_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT);
    Mockito.when(
        xProductFeign.getItemPickupPointCodeByItemSkus(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID), eq(Constants.DEFAULT_USERNAME),
            any())).thenReturn(
        new GdnRestListResponse<>(null, null, true, Arrays.asList(itemSkuPickupPointCodeResponse),
            new PageMetaData(PAGE, SIZE, SIZE), REQUEST_ID));
    SimpleListStringRequest itemSkuList = new SimpleListStringRequest();
    itemSkuList.setValue(Arrays.asList());
    Assertions.assertThrows(RuntimeException.class,
        () -> this.xProductOutboundService.getItemPickupPointCodeByItemSkus(itemSkuList));
  }

  @Test
  public void getItemPickupPointCodeByItemSkusExceptionTest() {
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(ITEM_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT);
    Mockito.when(
        xProductFeign.getItemPickupPointCodeByItemSkus(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID), eq(Constants.DEFAULT_USERNAME),
            any())).thenReturn(
        new GdnRestListResponse<>(null, null, false, Collections.emptyList(), new PageMetaData(PAGE, SIZE, SIZE),
            REQUEST_ID));
    SimpleListStringRequest itemSkuList = new SimpleListStringRequest();
    itemSkuList.setValue(Arrays.asList(ITEM_SKU));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> this.xProductOutboundService.getItemPickupPointCodeByItemSkus(itemSkuList));
    } finally {
      Mockito.verify(xProductFeign)
          .getItemPickupPointCodeByItemSkus(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
              eq(Constants.DEFAULT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID), eq(Constants.DEFAULT_USERNAME),
              any());
    }
  }

  @Test
  public void getItemPickupPointCodeByItemSkusExceptionNullTest() {
    ItemSkuPickupPointCodeResponse itemSkuPickupPointCodeResponse = new ItemSkuPickupPointCodeResponse();
    itemSkuPickupPointCodeResponse.setItemSku(ITEM_SKU);
    itemSkuPickupPointCodeResponse.setPickupPointCode(PICKUP_POINT);
    Mockito.when(
        xProductFeign.getItemPickupPointCodeByItemSkus(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
            eq(Constants.DEFAULT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID), eq(Constants.DEFAULT_USERNAME),
            any())).thenReturn(null);
    SimpleListStringRequest itemSkuList = new SimpleListStringRequest();
    itemSkuList.setValue(Arrays.asList(ITEM_SKU));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> this.xProductOutboundService.getItemPickupPointCodeByItemSkus(itemSkuList));
    } finally {
      Mockito.verify(xProductFeign)
          .getItemPickupPointCodeByItemSkus(eq(Constants.DEFAULT_STORE_ID), eq(Constants.DEFAULT_CHANNEL_ID),
              eq(Constants.DEFAULT_CLIENT_ID), eq(Constants.DEFAULT_REQUEST_ID), eq(Constants.DEFAULT_USERNAME),
              any());
    }
  }

  @Test
  public void getItemPickupPointListTest() {
    Mockito.when(this.xProductFeign.getItemPickupPointList(Constant.STORE_ID, Constant.CHANNEL_ID,
      Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, pageable.getPageNumber(),
      pageable.getPageSize(), itemPickupPointListingRequest)).thenReturn(
      new GdnRestListResponse<>(null, null, true, Collections.emptyList(),
        new PageMetaData(PAGE, SIZE, SIZE), null));
    this.xProductOutboundService.getItemPickupPointList(pageable, itemPickupPointListingRequest);
    Mockito.verify(this.xProductFeign)
      .getItemPickupPointList(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, pageable.getPageNumber(), pageable.getPageSize(),
        itemPickupPointListingRequest);
  }

  @Test
  public void getItemPickupPointList_successFalseTest() {
    Mockito.when(this.xProductFeign.getItemPickupPointList(Constant.STORE_ID, Constant.CHANNEL_ID,
        Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME, pageable.getPageNumber(),
        pageable.getPageSize(), itemPickupPointListingRequest))
      .thenReturn(new GdnRestListResponse<>(null, null, false, null, null, null));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> this.xProductOutboundService.getItemPickupPointList(pageable,
              itemPickupPointListingRequest));
    } finally {
      Mockito.verify(this.xProductFeign)
        .getItemPickupPointList(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
          Constant.REQUEST_ID, Constant.USER_NAME, pageable.getPageNumber(), pageable.getPageSize(),
          itemPickupPointListingRequest);
    }
  }

  @Test
  public void getItemSummaryByItemSkuAndPPCodeTest() {
    Mockito.when(this.xProductFeign
      .getItemSummaryByItemSkuAndPPCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, Collections.singletonList(itemPickupPointRequest)))
      .thenReturn(new GdnRestListResponse<>(null, null, true, Collections.emptyList(),
        new PageMetaData(PAGE, SIZE, SIZE), null));
    this.xProductOutboundService
      .getItemSummaryByItemSkuAndPPCode(Collections.singletonList(itemPickupPointRequest));
    Mockito.verify(this.xProductFeign)
      .getItemSummaryByItemSkuAndPPCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, Collections.singletonList(itemPickupPointRequest));
  }

  @Test
  public void getItemSummaryByItemSkuAndPPCodeTest_SuccessFalseTest() {
    Mockito.when(this.xProductFeign
      .getItemSummaryByItemSkuAndPPCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, Collections.singletonList(itemPickupPointRequest)))
      .thenReturn(new GdnRestListResponse<>(null, null, false, null, null, null));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> this.xProductOutboundService.getItemSummaryByItemSkuAndPPCode(
              Collections.singletonList(itemPickupPointRequest)));
    } finally {
      Mockito.verify(this.xProductFeign)
        .getItemSummaryByItemSkuAndPPCode(Constant.STORE_ID, Constant.CHANNEL_ID,
          Constant.CLIENT_ID, Constant.REQUEST_ID, Constant.USER_NAME,
          Collections.singletonList(itemPickupPointRequest));
    }
  }

  @Test
  public void deleteItemPickupPointByPickupPointCodeTest() {
    Mockito.when(this.xProductFeign
        .deleteItemPickupPointByPickupPointCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, new DeleteItemPickupPointRequest())).thenReturn(
        new GdnRestListResponse<>(null, null, true, Collections.emptyList(), new PageMetaData(PAGE, SIZE, SIZE), null));
    this.xProductOutboundService.deleteItemPickupPointByPickupPointCode(new DeleteItemPickupPointRequest());
    Mockito.verify(this.xProductFeign)
        .deleteItemPickupPointByPickupPointCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, new DeleteItemPickupPointRequest());
  }

  @Test
  public void deleteItemPickupPointByPickupPointCodeExceptionTest() {
    Mockito.when(this.xProductFeign
        .deleteItemPickupPointByPickupPointCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, new DeleteItemPickupPointRequest()))
        .thenReturn(new GdnRestListResponse<>(null, null, false, null, null, null));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> this.xProductOutboundService.deleteItemPickupPointByPickupPointCode(
              new DeleteItemPickupPointRequest()));
    } finally {
      Mockito.verify(this.xProductFeign)
          .deleteItemPickupPointByPickupPointCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
              Constant.REQUEST_ID, Constant.USER_NAME, new DeleteItemPickupPointRequest());
    }
  }

  @Test
  public void getActiveProductsByPickupPointCodeTest() {
    Mockito.when(this.xProductFeign
        .getActiveProductsByPickupPointCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, 0, 10, BUSINESS_PARTNER_CODE, PICKUP_POINT)).thenReturn(
        new GdnRestListResponse<>(null, null, true, Collections.emptyList(), new PageMetaData(PAGE, SIZE, SIZE), null));
    this.xProductOutboundService.getActiveProductsByPickupPointCode(BUSINESS_PARTNER_CODE, PICKUP_POINT, 0, 10);
    Mockito.verify(this.xProductFeign)
        .getActiveProductsByPickupPointCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, 0, 10, BUSINESS_PARTNER_CODE, PICKUP_POINT);
  }

  @Test
  public void getActiveProductsByPickupPointCodeExceptionTest() {
    Mockito.when(this.xProductFeign
        .getActiveProductsByPickupPointCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, 0, 10, BUSINESS_PARTNER_CODE, PICKUP_POINT))
        .thenReturn(new GdnRestListResponse<>(null, null, false, null, null, null));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> this.xProductOutboundService.getActiveProductsByPickupPointCode(
              BUSINESS_PARTNER_CODE, PICKUP_POINT, 0, 10));
    } finally {
      Mockito.verify(this.xProductFeign)
          .getActiveProductsByPickupPointCode(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
              Constant.REQUEST_ID, Constant.USER_NAME, 0, 10, BUSINESS_PARTNER_CODE, PICKUP_POINT);
    }
  }

  @Test
  public void getItemBasicDetailsTest() {
    Mockito.when(this.xProductFeign.getItemBasicDetails(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, PRODUCT_SKU)).thenReturn(
        new GdnRestListResponse<>(null, null, true, Collections.emptyList(), new PageMetaData(PAGE, SIZE, SIZE), null));
    this.xProductOutboundService.getItemBasicDetails(PRODUCT_SKU);
    Mockito.verify(this.xProductFeign)
        .getItemBasicDetails(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, PRODUCT_SKU);
  }

  @Test
  public void getItemBasicDetailseExceptionTest() {
    Mockito.when(this.xProductFeign.getItemBasicDetails(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, PRODUCT_SKU))
        .thenReturn(new GdnRestListResponse<>(null, null, false, null, null, null));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> this.xProductOutboundService.getItemBasicDetails(PRODUCT_SKU));
    } finally {
      Mockito.verify(this.xProductFeign)
          .getItemBasicDetails(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
              Constant.USER_NAME, PRODUCT_SKU);
    }
  }

  @Test
  public void getItemL5DetailsTest() {
    Mockito.when(
        this.xProductFeign.getItemL5Details(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
            eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(PRODUCT_SKU), eq(0), eq(10), eq(true),
            eq(new SimpleListStringRequest()))).thenReturn(
        new GdnRestListResponse<>(null, null, true, Collections.emptyList(), new PageMetaData(PAGE, SIZE, SIZE), null));
    this.xProductOutboundService.getItemL5Details(PRODUCT_SKU, true, 0, 10);
    Mockito.verify(this.xProductFeign)
        .getItemL5Details(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
            eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(PRODUCT_SKU), eq(0), eq(10), eq(true),
            eq(new SimpleListStringRequest()));
  }

  @Test
  public void getItemL5DetailsExceptionTest() {
    Mockito.when(
            this.xProductFeign.getItemL5Details(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
                eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(PRODUCT_SKU), eq(0), eq(10), eq(false),
                eq(new SimpleListStringRequest())))
        .thenReturn(new GdnRestListResponse<>(null, null, false, null, null, null));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> this.xProductOutboundService.getItemL5Details(PRODUCT_SKU, false, 0, 10));
    } finally {
      Mockito.verify(this.xProductFeign)
          .getItemL5Details(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
              eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(PRODUCT_SKU), eq(0), eq(10), eq(false),
              eq(new SimpleListStringRequest()));
    }
  }

  @Test
  public void getItemL5DetailsWithL5IdsTest() {
    Mockito.when(this.xProductFeign.getItemL5DetailsByL5Ids(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
        eq(Constant.CLIENT_ID), eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(0), eq(10), eq(true),
        eq(new SimpleListStringRequest(Collections.singletonList(PRODUCT_SKU))))).thenReturn(
        new GdnRestListResponse<>(null, null, true, Collections.emptyList(), new PageMetaData(PAGE, SIZE, SIZE), null));
    this.xProductOutboundService.getItemL5DetailsByL5Ids(Collections.singletonList(PRODUCT_SKU), true, 0, 10);
    Mockito.verify(this.xProductFeign)
        .getItemL5DetailsByL5Ids(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
            eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(0), eq(10), eq(true),
            eq(new SimpleListStringRequest(Collections.singletonList(PRODUCT_SKU))));
  }

  @Test
  public void getItemL5DetailsWithL5Ids_exceptionTest() {
    Mockito.when(this.xProductFeign.getItemL5DetailsByL5Ids(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
            eq(Constant.CLIENT_ID), eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(0), eq(10), eq(false),
            eq(new SimpleListStringRequest(Collections.singletonList(PRODUCT_SKU)))))
        .thenReturn(new GdnRestListResponse<>(null, null, false, null, null, null));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> this.xProductOutboundService.getItemL5DetailsByL5Ids(
              Collections.singletonList(PRODUCT_SKU), false, 0, 10));
    } finally {
      Mockito.verify(this.xProductFeign)
          .getItemL5DetailsByL5Ids(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
              eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(0), eq(10), eq(false),
              eq(new SimpleListStringRequest(Collections.singletonList(PRODUCT_SKU))));
    }
  }

  @Test
  public void fetchPriceRangeSuccess() {
    Set<String> skus = Collections.singleton(BUSINESS_PARTNER_CODE);
    when(xProductFeign.getMinAndMaxPriceRange(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, skus))
        .thenReturn(new GdnRestListResponse<>(new ArrayList<>(), null, Constant.REQUEST_ID));
    this.xProductOutboundService.fetchPriceRange(BUSINESS_PARTNER_CODE, skus);
    verify(xProductFeign).getMinAndMaxPriceRange(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, skus);
  }

  @Test
  public void fetchPriceRangeFail() {
    Set<String> skus = Collections.singleton(BUSINESS_PARTNER_CODE);
    when(xProductFeign.getMinAndMaxPriceRange(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, skus))
        .thenReturn(new GdnRestListResponse<>(null, null, false, null, null, null));
    try {
      this.xProductOutboundService.fetchPriceRange(BUSINESS_PARTNER_CODE, skus);
    } catch (Exception exception) {
      verify(xProductFeign).getMinAndMaxPriceRange(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
          Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, skus);
    }
  }

  @Test
  public void getBasicProductInfoTest() {
    Mockito.when(this.xProductFeign.getBasicProductInfo(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, new BasicProductResponse(), Constant.REQUEST_ID));
    BasicProductResponse response = xProductOutboundService.getBasicProductInfo(Constant.STORE_ID, PRODUCT_SKU);
    Mockito.verify(this.xProductFeign)
        .getBasicProductInfo(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, PRODUCT_SKU);
    Assertions.assertNotNull(response);
  }

  @Test
  public void getBasicProductInfoExceptionTest() {
    Mockito.when(this.xProductFeign.getBasicProductInfo(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, PRODUCT_SKU))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, new BasicProductResponse(), Constant.REQUEST_ID));
    BasicProductResponse response = null;
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> xProductOutboundService.getBasicProductInfo(Constant.STORE_ID, PRODUCT_SKU));
    } finally {

      Mockito.verify(this.xProductFeign)
          .getBasicProductInfo(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
              Constant.USER_NAME, PRODUCT_SKU);
    }
  }

  @Test
  public void getItemBasicDetailByItemSkuTest() {
    Mockito.when(xProductFeign.getItemBasicDetailByItemSku(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, true, new SimpleListStringRequest())).thenReturn(
        new GdnRestListResponse<>(null, null, true,
            Arrays.asList(ItemBasicDetailV2Response.builder().itemSku(ITEM_SKU).build()), new PageMetaData(),
            Constant.REQUEST_ID));
    List<ItemBasicDetailV2Response> response =
        xProductOutboundService.getItemBasicDetailByItemSku(Constant.STORE_ID, true, new SimpleListStringRequest());
    Mockito.verify(xProductFeign)
        .getItemBasicDetailByItemSku(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, true, new SimpleListStringRequest());
    Assertions.assertNotNull(response);
  }

  @Test
  public void getItemBasicDetailByItemSkuSuccessFalseTest() {
    Mockito.when(xProductFeign.getItemBasicDetailByItemSku(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, true, new SimpleListStringRequest())).thenReturn(
        new GdnRestListResponse<>(null, null, false,
            Arrays.asList(ItemBasicDetailV2Response.builder().itemSku(ITEM_SKU).build()), new PageMetaData(),
            Constant.REQUEST_ID));
    List<ItemBasicDetailV2Response> response = null;
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> xProductOutboundService.getItemBasicDetailByItemSku(Constant.STORE_ID, true,
              new SimpleListStringRequest()));
    } finally {
      Mockito.verify(xProductFeign)
          .getItemBasicDetailByItemSku(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
              Constant.USER_NAME, true, new SimpleListStringRequest());
      Assertions.assertNull(response);
    }
  }

  @Test
  public void getItemBasicDetailByItemSkuResponseEmptyTest() {
    Mockito.when(xProductFeign.getItemBasicDetailByItemSku(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, true, new SimpleListStringRequest())).thenReturn(
        new GdnRestListResponse<>(null, null, true,
            new ArrayList<>(), new PageMetaData(),
            Constant.REQUEST_ID));
    List<ItemBasicDetailV2Response> response = null;
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> xProductOutboundService.getItemBasicDetailByItemSku(Constant.STORE_ID, true,
              new SimpleListStringRequest()));
    } finally {
      Mockito.verify(xProductFeign)
          .getItemBasicDetailByItemSku(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
              Constant.USER_NAME, true, new SimpleListStringRequest());
      Assertions.assertNull(response);
    }
  }

  @Test
  public void getItemBasicDetailsByItemSkuTest() {
    Mockito.when(xProductFeign
        .getItemBasicDetailsByItemSku(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, false, false, false,
            new SimpleListStringRequest(Collections.singletonList(ITEM_SKU), false))).thenReturn(
        new GdnRestListResponse(null, null, true, new ArrayList(), new PageMetaData(), Constant.REQUEST_ID));
    List<ItemBasicDetailV2Response> response = null;
    response = xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU);
    Mockito.verify(xProductFeign)
        .getItemBasicDetailsByItemSku(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, false, false, false,
            new SimpleListStringRequest(Collections.singletonList(ITEM_SKU), false));

  }

  @Test
  public void getItemBasicDetailsByItemSkuErrorTest() {
    Mockito.when(xProductFeign
        .getItemBasicDetailsByItemSku(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, false, false, false,
            new SimpleListStringRequest(Collections.singletonList(ITEM_SKU), false))).thenReturn(
        new GdnRestListResponse(null, null, false, new ArrayList(), new PageMetaData(), Constant.REQUEST_ID));
    List<ItemBasicDetailV2Response> response = null;
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> xProductOutboundService.getItemBasicDetailsByItemSku(ITEM_SKU));
    } finally {
      Mockito.verify(xProductFeign)
          .getItemBasicDetailsByItemSku(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
              Constant.USER_NAME, false, false, false,
              new SimpleListStringRequest(Collections.singletonList(ITEM_SKU), false));
    }
  }

  @Test
  public void isSharedProductSuccessFalseTest() {
    Mockito.when(
        xProductFeign.isSharedProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
          Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, PRODUCT_SKU, true))
      .thenReturn(new GdnRestSingleResponse(null, null, false, null, null));
    try {
      Assertions.assertThrows(RuntimeException.class,
        () -> xProductOutboundService.isSharedProduct(Constant.STORE_ID, PRODUCT_SKU,
          BUSINESS_PARTNER_CODE));
    } finally {
      Mockito.verify(xProductFeign)
        .isSharedProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
          Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, PRODUCT_SKU, true);
    }
  }

  @Test
  public void isSharedProductResponseNullTest() {
    Mockito.when(
        xProductFeign.isSharedProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
          Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, PRODUCT_SKU, true))
      .thenReturn(new GdnRestSingleResponse(null, null, true, null, null));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> xProductOutboundService.isSharedProduct(Constant.STORE_ID, PRODUCT_SKU, BUSINESS_PARTNER_CODE));
    } finally {
      Mockito.verify(xProductFeign)
        .isSharedProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
          Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, PRODUCT_SKU, true);
    }
  }

  @Test
  public void isSharedProductTest() {
    Mockito.when(
        xProductFeign.isSharedProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
          Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, PRODUCT_SKU, true))
      .thenReturn(
        new GdnRestSingleResponse(null, null, true, new SimpleBooleanResponse(false), null));
    Assertions.assertFalse(xProductOutboundService.isSharedProduct(Constant.STORE_ID, PRODUCT_SKU, BUSINESS_PARTNER_CODE));
    Mockito.verify(xProductFeign)
      .isSharedProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, PRODUCT_SKU, true);
  }

  @Test
  public void isSharedProductTrueTest() {
    Mockito.when(
        xProductFeign.isSharedProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
          Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, PRODUCT_SKU, true))
      .thenReturn(
        new GdnRestSingleResponse(null, null, true, new SimpleBooleanResponse(true), null));
    Assertions.assertTrue(xProductOutboundService.isSharedProduct(Constant.STORE_ID, PRODUCT_SKU,
      BUSINESS_PARTNER_CODE));
    Mockito.verify(xProductFeign)
      .isSharedProduct(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
        Constant.REQUEST_ID, Constant.USER_NAME, BUSINESS_PARTNER_CODE, PRODUCT_SKU, true);
  }

  @Test
  public void getProductBasicInfoDetails_Success() throws ApplicationException {
    ProductLevel3SummaryRequest request = new ProductLevel3SummaryRequest();
    BulkDownloadProductBasicInfoResponse expectedResponse = new BulkDownloadProductBasicInfoResponse();
    GdnRestSingleResponse<BulkDownloadProductBasicInfoResponse> mockResponse =
        new GdnRestSingleResponse<>(expectedResponse, Constant.REQUEST_ID);
    when(
        xProductFeign.getProductBasicInfoDetails(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
            eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(request))).thenReturn(mockResponse);
    BulkDownloadProductBasicInfoResponse actualResponse = xProductOutboundService.getProductBasicInfoDetails(request);
    assertNotNull(actualResponse);
    assertEquals(expectedResponse, actualResponse);
    verify(xProductFeign).getProductBasicInfoDetails(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
        eq(Constant.CLIENT_ID), eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(request));
  }

  @Test
  public void getProductBasicInfoDetails_NullResponse() {
    ProductLevel3SummaryRequest request = new ProductLevel3SummaryRequest();
    when(
        xProductFeign.getProductBasicInfoDetails(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
            eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(request))).thenReturn(null);
    ApplicationException exception =
        assertThrows(ApplicationException.class, () -> xProductOutboundService.getProductBasicInfoDetails(request));
    verify(xProductFeign).getProductBasicInfoDetails(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
        eq(Constant.CLIENT_ID), eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(request));
  }

  @Test
  public void getProductBasicInfoDetails_UnsuccessfulResponse() {
    ProductLevel3SummaryRequest request = new ProductLevel3SummaryRequest();
    GdnRestSingleResponse<BulkDownloadProductBasicInfoResponse> mockResponse =
        new GdnRestSingleResponse<>("Error occurred", "ERR001", false, null, Constant.REQUEST_ID);
    when(
        xProductFeign.getProductBasicInfoDetails(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
            eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(request))).thenReturn(mockResponse);
    ApplicationException exception =
        assertThrows(ApplicationException.class, () -> xProductOutboundService.getProductBasicInfoDetails(request));
    verify(xProductFeign).getProductBasicInfoDetails(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
        eq(Constant.CLIENT_ID), eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(request));
  }

  @Test
  public void getProductBasicInfoDetails_UnsuccessfulResponseTest() {
    ProductLevel3SummaryRequest request = new ProductLevel3SummaryRequest();
    GdnRestSingleResponse<BulkDownloadProductBasicInfoResponse> mockResponse =
        new GdnRestSingleResponse<>("Error occurred", "ERR001", true, null, Constant.REQUEST_ID);
    when(
        xProductFeign.getProductBasicInfoDetails(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
            eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(request))).thenReturn(mockResponse);
    ApplicationException exception =
        assertThrows(ApplicationException.class, () -> xProductOutboundService.getProductBasicInfoDetails(request));
    verify(xProductFeign).getProductBasicInfoDetails(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID),
        eq(Constant.CLIENT_ID), eq(Constant.REQUEST_ID), eq(Constant.USER_NAME), eq(request));
  }

  @Test
  public void getProductSkuResponse_Success() {
    ProductSummaryRequest productSummaryRequest = new ProductSummaryRequest();
    ProductSkuResponse productSkuResponse1 = new ProductSkuResponse();
    ProductSkuResponse productSkuResponse2 = new ProductSkuResponse();
    List<ProductSkuResponse> mockResponses = Arrays.asList(productSkuResponse1, productSkuResponse2);
    PageMetaData pageMetaData = new PageMetaData();
    GdnRestListResponse<ProductSkuResponse> mockResponse =
        new GdnRestListResponse<>(mockResponses, pageMetaData, REQUEST_ID);
    when(xProductFeign.getProductSkuResponse(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
        eq(REQUEST_ID), eq(USERNAME), eq(0), eq(10), eq(productSummaryRequest))).thenReturn(mockResponse);
    Page<ProductSkuResponse> result =
        xProductOutboundService.getProductSkuResponse(productSummaryRequest, 0, 10, REQUEST_ID, USERNAME);
    assertNotNull(result);
    assertEquals(2, result.getContent().size());
    verify(xProductFeign).getProductSkuResponse(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
        eq(REQUEST_ID), eq(USERNAME), eq(0), eq(10), eq(productSummaryRequest));
  }

  @Test
  public void getProductSkuResponse_UnsuccessfulResponse() {
    ProductSummaryRequest productSummaryRequest = new ProductSummaryRequest();
    GdnRestListResponse<ProductSkuResponse> mockResponse =
        new GdnRestListResponse<>("Error occurred", "ERR001", false, REQUEST_ID);
    when(xProductFeign.getProductSkuResponse(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
        eq(REQUEST_ID), eq(USERNAME), eq(0), eq(10), eq(productSummaryRequest))).thenReturn(mockResponse);
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class,
        () -> xProductOutboundService.getProductSkuResponse(productSummaryRequest, 0, 10, REQUEST_ID, USERNAME));
    verify(xProductFeign).getProductSkuResponse(eq(Constant.STORE_ID), eq(Constant.CHANNEL_ID), eq(Constant.CLIENT_ID),
        eq(REQUEST_ID), eq(USERNAME), eq(0), eq(10), eq(productSummaryRequest));
  }

  @Test
  void testGetProductL3DetailsByProductSku_success() throws ApplicationException {
    String sku = "SKU123";
    ProductL3Response expectedResponse = new ProductL3Response();
    GdnRestSingleResponse<ProductL3Response> mockResponse = new GdnRestSingleResponse<>();
    mockResponse.setSuccess(true);
    mockResponse.setValue(expectedResponse);
    Mockito.when(
            xProductFeign.getProductDetailsByProductSku(anyString(), anyString(),
                anyString(), anyString(), anyString(), Mockito.eq(sku)))
        .thenReturn(mockResponse);
    ProductL3Response actual = xProductOutboundService.getProductL3DetailsByProductSku(sku);
    Mockito.verify(xProductFeign).getProductDetailsByProductSku(anyString(), anyString(),
        anyString(), anyString(), anyString(), Mockito.eq(sku));
    Assertions.assertEquals(expectedResponse, actual);
  }

  @Test
  void testGetProductL3DetailsByProductSku_responseNull() {
    String sku = "SKU123";
    Mockito.when(
            xProductFeign.getProductDetailsByProductSku(anyString(), anyString(),
                anyString(), anyString(), anyString(), Mockito.eq(sku)))
        .thenReturn(null);

    ApplicationException ex = Assertions.assertThrows(ApplicationException.class,
        () -> xProductOutboundService.getProductL3DetailsByProductSku(sku));
    Mockito.verify(xProductFeign).getProductDetailsByProductSku(anyString(), anyString(),
        anyString(), anyString(), anyString(), Mockito.eq(sku));    Assertions.assertEquals("Can not communicate :xproduct error", ex.getMessage());
  }

  @Test
  void testGetProductL3DetailsByProductSku_unsuccessfulResponse() {
    String sku = "SKU123";
    GdnRestSingleResponse<ProductL3Response> response = new GdnRestSingleResponse<>();
    response.setSuccess(false);
    response.setErrorMessage("Failed to fetch");
    Mockito.when(
            xProductFeign.getProductDetailsByProductSku(anyString(), anyString(),
                anyString(), anyString(), anyString(), Mockito.eq(sku)))
        .thenReturn(response);
    ApplicationException ex = Assertions.assertThrows(ApplicationException.class,
        () -> xProductOutboundService.getProductL3DetailsByProductSku(sku));
    Mockito.verify(xProductFeign)
        .getProductDetailsByProductSku(anyString(), anyString(),
            anyString(), anyString(), anyString(), Mockito.eq(sku));
    Assertions.assertEquals("Unspecified error :Failed to fetch", ex.getMessage());
  }

  @Test
  void testGetProductL3DetailsByProductSku_nullValue() {
    String sku = "SKU123";
    GdnRestSingleResponse<ProductL3Response> response = new GdnRestSingleResponse<>();
    response.setSuccess(true);
    response.setValue(null);
    response.setErrorMessage("Null value from xproduct");

    Mockito.when(xProductFeign.getProductDetailsByProductSku(
            anyString(), anyString(), anyString(),
            anyString(), anyString(), Mockito.eq(sku)))
        .thenReturn(response);

    ApplicationException ex = Assertions.assertThrows(ApplicationException.class, () ->
        xProductOutboundService.getProductL3DetailsByProductSku(sku)
    );
    Mockito.verify(xProductFeign)
        .getProductDetailsByProductSku(anyString(), anyString(),
            anyString(), anyString(), anyString(), Mockito.eq(sku));
  }

  @Test
  public void getProductBasicInfoTest() {
    Mockito.when(this.xProductFeign.getProductBasicInfo(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, PRODUCT_SKU, false))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, new ProductBasicResponse(), Constant.REQUEST_ID));
    ProductBasicResponse response = xProductOutboundService.getProductBasicInfo(Constant.STORE_ID, PRODUCT_SKU, false);
    Mockito.verify(this.xProductFeign)
        .getProductBasicInfo(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, PRODUCT_SKU, false);
    Assertions.assertNotNull(response);
  }

  @Test
  public void getBasicProductInfo_ExceptionTest() {
    Mockito.when(this.xProductFeign.getProductBasicInfo(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID,
            Constant.REQUEST_ID, Constant.USER_NAME, PRODUCT_SKU, false))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, new ProductBasicResponse(), Constant.REQUEST_ID));
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> xProductOutboundService.getProductBasicInfo(Constant.STORE_ID, PRODUCT_SKU, false));
    } finally {

      Mockito.verify(this.xProductFeign)
          .getProductBasicInfo(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
              Constant.USER_NAME, PRODUCT_SKU, false);
    }
  }

  @Test
  public void testMapProductToItem() {
    String productCode = "Product code";
    List<ProductItemUpcCodeUpdateRequest> productItemUpcCodeUpdateRequests = new ArrayList<>();
    productItemUpcCodeUpdateRequests.add(
        ProductItemUpcCodeUpdateRequest.builder().upcCode("upc").skuCode("sku").build());
    Mockito.when(xProductFeign.mapProductToItem(anyString(), anyString(), anyString(),
            anyString(), anyString(), anyString()))
        .thenReturn(new GdnRestSingleResponse<>(L3VersionResponse.builder().l3Version(1L).build(), "123"));
    xProductOutboundService.mapProductToItem(productCode);
    Mockito.verify(xProductFeign)
        .mapProductToItem(anyString(), anyString(), anyString(), anyString(),
            anyString(), anyString());
  }

  @Test
  public void testMapProductToItemWithVersionFalse() {
    String productCode = "Product code";
    Mockito.when(xProductFeign.mapProductToItem(anyString(), anyString(), anyString(),
            anyString(), anyString(), anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, "123"));
    xProductOutboundService.mapProductToItem(productCode);
    Mockito.verify(xProductFeign)
        .mapProductToItem(anyString(), anyString(), anyString(), anyString(),
            anyString(), anyString());
  }

  @Test
  public void testMapProductToItemErrorTest() {
    String productCode = "Product code";
    Mockito.when(xProductFeign.mapProductToItem(anyString(), anyString(), anyString(),
            anyString(), anyString(), anyString()))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, null));
    try {
      Assertions.assertThrows(RuntimeException.class, () -> xProductOutboundService.mapProductToItem(productCode));
    } finally {
      Mockito.verify(xProductFeign)
          .mapProductToItem(anyString(), anyString(), anyString(), anyString(),
              anyString(), anyString());
    }
  }

  @Test
  void testGetL4ItemListByProductSku_success() {

    int page = 0;
    int size = 10;

    ItemLevel4ListingWebRequest request = new ItemLevel4ListingWebRequest();
    ItemBasicL4Response item = new ItemBasicL4Response();

    GdnRestListResponse<ItemBasicL4Response> feignResponse = new GdnRestListResponse<>();
    feignResponse.setSuccess(true);
    feignResponse.setContent(List.of(item));
    feignResponse.setPageMetaData(new PageMetaData(1,1,1)); // assuming constructor: totalRecords = 1

    when(xProductFeign.getL4ItemListByProductSku(
        anyString(), anyString(), anyString(),
        anyString(), anyString(),
        eq(page), eq(size),
        eq(request)
    )).thenReturn(feignResponse);

    // When
    Page<ItemBasicL4Response> result = xProductOutboundService.getL4ItemListByProductSku(
        page, size, "req123", "johnDoe", request
    );

    // Then
    assertNotNull(result);
    assertEquals(1, result.getTotalElements());
    assertEquals(item, result.getContent().get(0));
    verify(xProductFeign).getL4ItemListByProductSku(anyString(), anyString(), anyString(),
        anyString(), anyString(),
        eq(page), eq(size),
        eq(request));
  }

  @Test
  void testGetL4ItemListByProductSku_failure_throwsException() {

    int page = 0;
    int size = 10;

    ItemLevel4ListingWebRequest request = new ItemLevel4ListingWebRequest();

    GdnRestListResponse<ItemBasicL4Response> feignResponse = new GdnRestListResponse<>();
    feignResponse.setSuccess(false);
    feignResponse.setContent(Collections.emptyList());
    feignResponse.setErrorMessage("Error occurred");

    when(xProductFeign.getL4ItemListByProductSku(
        anyString(), anyString(), anyString(),
        anyString(), anyString(),
        eq(page), eq(size),
        eq(request)
    )).thenReturn(feignResponse);

    // When & Then
    ApplicationRuntimeException ex = assertThrows(
        ApplicationRuntimeException.class,
        () -> xProductOutboundService.getL4ItemListByProductSku(page, size, "req123", "johnDoe", request)
    );

    assertEquals("Can not communicate :Error occurred", ex.getMessage());
    verify(xProductFeign).getL4ItemListByProductSku(anyString(), anyString(), anyString(),
        anyString(), anyString(),
        eq(page), eq(size),
        eq(request));
  }
}
